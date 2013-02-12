;;; Structured access to bytevector contents.  We try to imitate C's arrays,
;;; structs, and unions here.  You use a "descriptor" together with "accessors"
;;; to access parts of a bytevector whose contents conform to what is described
;;; in your descriptor.  Like you had a void* in C and casted some
;;; array/struct/union on it, then accessed members.

(define-module (taylan bytestructures)
  #:export (define-bytestructure-descriptor-type
            bytestructure-descriptor
            bytestructure-descriptor?
            bytestructure-descriptor-size
            bytestructure-access
            bytestructure-access*))

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-11)
             (rnrs bytevectors))

(define-syntax assert
  (syntax-rules ()
    ((assert expression)
     (unless expression
       (error "Assertion not met." 'expression)))))

;;; Types

(define-record-type :bytestructure-descriptor-type
  (bytestructure-descriptor-type compound? constructor predicate size
                                 bytevector-constructor-helper
                                 bytevector-accessor bytevector-mutator)
  bytestructure-descriptor-type?
  (compound? bytestructure-descriptor-type-compound?)
  (constructor bytestructure-descriptor-constructor)
  (predicate bytestructure-descriptor-type-predicate)
  (size bytestructure-descriptor-type-size)
  (bytevector-constructor-helper bytevector-constructor-helper)
  (bytevector-accessor bytevector-accessor)
  (bytevector-mutator bytevector-mutator))

(define bytestructure-descriptor-types (make-parameter '()))

(define (define-bytestructure-descriptor-type name constructor predicate
          size-or-size-accessor bytevector-accessor bytevector-mutator)
  (assert (symbol? name))
  (assert (every procedure? (list constructor predicate bytevector-accessor
                                  bytevector-mutator)))
  (assert (or (procedure? size-or-size-accessor)
              (and (integer? size-or-size-accessor)
                   (exact? size-or-size-accessor)
                   (< 0 size-or-size-accessor))))
  (bytestructure-descriptor-types
   (alist-cons
    name (bytestructure-descriptor-type #f constructor predicate
                                        size-or-size-accessor #f
                                        bytevector-accessor bytevector-mutator)
    (bytestructure-descriptor-types)))
  *unspecified*)

(define (define-bytestructure-descriptor-compound-type name constructor
          predicate size-accessor bytevector-accessor
          bytevector-constructor-helper)
  (assert (symbol? name))
  (assert (every procedure? (list constructor predicate size-accessor
                                  bytevector-accessor
                                  bytevector-constructor-helper)))
  (bytestructure-descriptor-types
   (alist-cons
    name (bytestructure-descriptor-type #t constructor predicate size-accessor
                                        bytevector-constructor-helper
                                        bytevector-accessor #f)
    (bytestructure-descriptor-types)))
  *unspecified*)

(define (bytestructure-descriptor-type-with-name name)
  (cdr (or (assoc name (bytestructure-descriptor-types))
           (error "Not a bytestructure-descriptor-type name." name))))

(define (bytestructure-descriptor-find-type descriptor)
  (or (find (lambda (type)
              ((bytestructure-descriptor-type-predicate type) descriptor))
            (bytestructure-descriptor-types))
      (error "Not a bytestructure-descriptor." descriptor)))

;;; Generals

(define (bytestructure-descriptor description)
  (cond
   ((symbol? description)
    ((bytestructure-descriptor-constructor
      (bytestructure-descriptor-type-with-name description))))
   ((list? description)
    (let ((name (car description))
          (contents (cdr description)))
      (apply (bytestructure-descriptor-constructor
              (bytestructure-descriptor-type-with-name name))
             contents)))
   (else (error "Invalid bytestructure-descriptor description." description))))

(define (bytestructure-descriptor? obj)
  (not (not (bytestructure-descriptor-find-type obj))))

(define (bytestructure-descriptor-size descriptor)
  (let* ((type (bytestructure-descriptor-find-type descriptor))
         (size (bytestructure-descriptor-type-size type)))
    (if (procedure? size)
        (size descriptor)
        size)))

(define-syntax bytestructure
  (syntax-rules ()
    ((_ descriptor)
     (make-bytevector (bytestructure-descriptor-size descriptor)))
    ((_ descriptor (value ...))
     (let ((bytevector (bytectructure descriptor)))
       (bytestructure* bytevector 0 (value ...))
       bytevector))))

(define-syntax bytectructure*
  (syntax-rules ()
    ((_ descriptor bytevector offset (value ...))
     (let ((type (bytestructure-descriptor-find-type descriptor))
           (index 0))
       (begin 
         (let-values (((descriptor* offset*)
                       ((bytevector-constructor-helper type) descriptor index)))
           (bytestructure* bytevector (+ offset offset*) value))
         (set! index (+ 1 index)))
       ...))
    ((_ descriptor bytevector offset value)
     ((bytevector-mutator (bytestructure-descriptor-find-type descriptor))
      bytevector offset value))))

(define-syntax bytestructure-access
  (syntax-rules ()
    ((_ bytevector descriptor accessor ...)
     (bytestructure-access* bytevector descriptor 0 accessor ...))))

(define-syntax bytestructure-access*
  (syntax-rules ()
    ((_ bytevector descriptor offset)
     (let ((type (bytestructure-descriptor-find-type descriptor)))
       (if (bytestructure-descriptor-type-compound? type)
           (values bytevector descriptor offset)
           ((bytevector-accessor type) bytevector descriptor offset))))
    ((_ bytevector descriptor offset accessor accessors ...)
     (let ((type (bytestructure-descriptor-find-type descriptor)))
       (let-values (((offset* descriptor*)
                     ((bytevector-accessor type) descriptor accessor)))
         (bytestructure-access* bytevector descriptor* (+ offset offset*)
                                accessors ...))))))

(define-syntax bytestructure-set!
  (syntax-rules ()
    ((_ bytevector descriptor accessor ... value)
     (bytestructure-set!* bytevector descriptor 0 accessor ... value))))

(define-syntax bytestructure-set!*
  (syntax-rules ()
    ((_ bytevector descriptor offset value)
     (let ((type (bytestructure-descriptor-find-type descriptor)))
       (if (bytestructure-descriptor-type-compound? type)
           (bytevector-copy! value 0 bytevector offset
                             (bytevector-length value))
           ((bytevector-mutator type) bytevector descriptor offset value))))
    ((_ bytevector descriptor offset accessor accessors ... value)
     (let ((type (bytestructure-descriptor-find-type descriptor)))
       (let-values (((offset* descriptor*)
                     ((bytevector-accessor type) descriptor accessor)))
         (bytestructure-set!* bytevector descriptor* (+ offset offset*)
                              accessors ...))))))

;;; Vector

(define-record-type :vector-descriptor
  (vector-descriptor* length content-descriptor size)
  vector-descriptor?
  (length vector-descriptor-length)
  (content-descriptor vector-descriptor-content-descriptor)
  (size vector-descriptor-size))

(define (vector-descriptor length content-description)
  (assert (and (integer? length) (<= 0 length)))
  (let ((content-descriptor (bytestructure-descriptor content-description)))
    (vector-descriptor*
     length content-descriptor
     (* length (bytestructure-descriptor-size content-descriptor)))))

(define (vector-access descriptor index)
  (let ((content-descriptor (vector-descriptor-content-descriptor descriptor)))
    (values (* index (bytestructure-descriptor-size content-descriptor))
            content-descriptor)))

(define-bytestructure-descriptor-compound-type
  'vector
  vector-descriptor
  vector-descriptor?
  vector-descriptor-size
  (lambda () #f)
  vector-access)

;;; Helpers for Structures and Unions

(define field-name car)
(define field-content-descriptor cdr)

(define (construct-fields fields)
  (map (lambda (field)
         (assert (and (list? field)
                      (= 2 (length field))
                      (symbol? (car field))))
         (cons (car field)
               (bytestructure-descriptor (cadr field))))
       fields))

;;; Structs

(define-record-type :struct-descriptor
  (struct-descriptor* fields size)
  struct-descriptor?
  (fields struct-descriptor-fields)
  (size struct-descriptor-size))

(define (struct-descriptor . fields)
  (let ((fields (construct-fields fields)))
    (struct-descriptor*
     fields (apply + (map (lambda (field)
                            (bytestructure-descriptor-size
                             (field-content-descriptor field)))
                          fields)))))

(define (struct-access descriptor key)
  (let ((fields (struct-descriptor-fields descriptor)))
    (let try-next ((field (car fields))
                   (fields (cdr fields))
                   (offset 0))
      (if (eq? (field-name field) key)
          (values offset (field-content-descriptor field))
          (try-next (car fields)
                    (cdr fields)
                    (+ offset (bytestructure-descriptor-size
                               (field-content-descriptor field))))))))

(define-bytestructure-descriptor-compound-type
  'struct
  struct-descriptor
  struct-descriptor?
  struct-descriptor-size
  (lambda () #f)
  struct-access)

;;; Unions

(define-record-type :union-descriptor
  (union-descriptor* fields size)
  union-descriptor?
  (fields union-descriptor-fields)
  (size union-descriptor-size))

(define (union-descriptor . fields)
  (assert (list? fields))
  (let ((fields (construct-fields fields)))
    (union-descriptor*
     fields (apply max (map (lambda (field)
                              (bytestructure-descriptor-size
                               (field-content-descriptor field)))
                            fields)))))

(define (union-access descriptor key)
  (values 0 (field-content-descriptor
             (assq key (union-descriptor-fields descriptor)))))

(define-bytestructure-descriptor-type
  'union
  union-descriptor
  union-descriptor?
  union-descriptor-size
  (lambda () #f)
  union-access)
