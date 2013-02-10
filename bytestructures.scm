;;; Structured access to bytevector contents.  We try to imitate C's arrays,
;;; structs, and unions here.  You use a "descriptor" together with "accessors"
;;; to access parts of a bytevector whose contents conform to what is described
;;; in your descriptor.  Like you had a void* in C and casted some
;;; array/struct/union on it, then accessed members.

(define-module (taylan bytestructures)
  #:export (bytestructure-descriptor
            bytestructure-descriptor?
            bytestructure-descriptor-size
            bytestructure-access
            bytestructure-access*
            define-bytestructure-descriptor-type))

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-11))

(define-syntax assert
  (syntax-rules ()
    ((assert expression)
     (unless expression
       (error "Assertion not met." 'expression)))))

;;; Types

(define-record-type :bytestructure-descriptor-type
  (bytestructure-descriptor-type name constructor predicate size-accessor
                                 accessor compound?)
  bytestructure-descriptor-type?
  (name bytestructure-descriptor-type-name)
  (constructor bytestructure-descriptor-type-constructor)
  (predicate bytestructure-descriptor-type-predicate)
  (size-accessor bytestructure-descriptor-type-size-accessor)
  (accessor bytestructure-descriptor-type-accessor)
  (compound? bytestructure-descriptor-type-compound?))

(define bytestructure-descriptor-types (make-parameter '()))

(define (define-bytestructure-descriptor-type
          name constructor predicate size-accessor accessor compound?)
  ;; TODO assertions
  (bytestructure-descriptor-types
   (cons (bytestructure-descriptor-type name constructor predicate size-accessor
                                        accessor compound?)
         (bytestructure-descriptor-types)))
  *unspecified*)

;;; Generals

(define (bytestructure-descriptor description)
  (let ((name (car description))
        (contents (cdr description)))
    (apply (bytestructure-descriptor-type-constructor
            (cond
             ((find (lambda (type)
                      (eq? name (bytestructure-descriptor-type-name type)))
                    (bytestructure-descriptor-types))
              => (lambda (obj) obj))
             (else (error "Not a bytestructure-descriptor-type name." name))))
           contents)))

(define (bytestructure-descriptor-find-type descriptor)
  (find (lambda (type)
          ((bytestructure-descriptor-type-predicate type) descriptor))
        (bytestructure-descriptor-types)))

(define (bytestructure-descriptor? obj)
  (bytestructure-descriptor-find-type obj))

(define (bytestructure-descriptor-size descriptor)
  ((bytestructure-descriptor-type-size-accessor
    (cond
     ((bytestructure-descriptor-find-type descriptor) => (lambda (obj) obj))
     (else (error "Not a bytestructure-descriptor." descriptor))))
   descriptor))

(define-syntax bytestructure-access
  (syntax-rules ()
    ((bytestructure-access bytevector descriptor accessor ...)
     (bytestructure-access* bytevector descriptor 0 accessor ...))))

(define-syntax bytestructure-access*
  (syntax-rules ()
    ((bytestructure-access* bytevector descriptor offset)
     (bytestructure-terminal-access bytevector descriptor offset))
    ((bytestructure-access* bytevector descriptor offset accessor accessors ...)
     (let ((type (bytestructure-descriptor-find-type descriptor)))
       (let-values (((offset* descriptor*)
                     ((bytestructure-descriptor-type-accessor type)
                      bytevector descriptor accessor)))
         (bytestructure-access* bytevector descriptor* (+ offset offset*)
                                accessors ...))))))

(define (bytestructure-terminal-access bytevector descriptor offset)
  (let ((type (bytestructure-descriptor-find-type descriptor)))
    (if (bytestructure-descriptor-type-compound? type)
        (let* ((size ((bytestructure-descriptor-type-size-accessor type)
                      descriptor))
               (copy (make-bytevector size)))
          (bytevector-copy! bytevector offset copy 0 size)
          copy)
        ((bytestructure-descriptor-type-accessor type)
         bytevector descriptor offset))))

(define (bytestructure-compound-access bytevector descriptor accessor)
  (bytestructure-compound-access* bytevector descriptor 0 accessor))

(define (bytestructure-compound-access* bytevector descriptor offset accessor)
  (let ((type (bytestructure-descriptor-find-type descriptor)))
    ((bytestructure-descriptor-type-accessor type)
     bytevector descriptor offset accessor)))

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

(define (vector-access bytevector descriptor index)
  (let ((content-descriptor (vector-descriptor-content-descriptor descriptor)))
    (values (* index (bytestructure-descriptor-size content-descriptor))
            content-descriptor)))

(define-bytestructure-descriptor-type
  'vector
  vector-descriptor
  vector-descriptor?
  vector-descriptor-size
  vector-access
  #t)

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

;;; Structures

(define-record-type :structure-descriptor
  (structure-descriptor* fields size)
  structure-descriptor?
  (fields structure-descriptor-fields)
  (size structure-descriptor-size))

(define (structure-descriptor fields)
  (let ((fields (construct-fields fields)))
    (structure-descriptor*
     fields (apply + (map (lambda (field)
                            (bytestructure-descriptor-size
                             (field-content-descriptor field)))
                          fields)))))

(define (structure-access bytevector descriptor key)
  (let ((fields (structure-descriptor-fields descriptor)))
    (let try-next ((field (car fields))
                   (fields (cdr fields))
                   (offset 0))
      (if (eq? (field-name field) key)
          (values offset (field-content-descriptor field))
          (try-next (car fields)
                    (cdr fields)
                    (+ offset (bytestructure-descriptor-size
                               (field-content-descriptor field))))))))

(define-bytestructure-descriptor-type
  'struct
  structure-descriptor
  structure-descriptor?
  structure-descriptor-size
  structure-access
  #t)

;;; Unions

(define-record-type :union-descriptor
  (union-descriptor* fields size)
  union-descriptor?
  (fields union-descriptor-fields)
  (size union-descriptor-size))

(define (union-descriptor fields)
  (assert (list? fields))
  (let ((fields (construct-fields fields)))
    (union-descriptor*
     fields (apply max (map (lambda (field)
                              (bytestructure-descriptor-size
                               (field-content-descriptor field)))
                            fields)))))

(define (union-access bytevector descriptor key)
  (values 0 (field-content-descriptor
             (assq key (union-descriptor-fields descriptor)))))

(define-bytestructure-descriptor-type
  'union
  union-descriptor
  union-descriptor?
  union-descriptor-size
  union-access
  #t)
