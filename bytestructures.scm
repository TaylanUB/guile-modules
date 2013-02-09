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
            define-bytestructure-descriptor-type))

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (rnrs bytevector))

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
         (bytestructure-descriptor-types))))

;;; Generals

(define (bytestructure-descriptor description)
  (let ((name (car description))
        (contents (cdr description)))
    (apply (bytestructure-descriptor-type-constructor
            (cond
             ((find (lambda (type)
                      (string= name (bytestructure-descriptor-type-name type)))
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

(define (bytestructure-access bytevector descriptor accessors offset)
  (let ((type (bytestructure-descriptor-find-type descriptor)))
    (if (null? accessors)
        (if (bytestructure-descriptor-type-compound? type)
            (let* ((size ((bytestructure-descriptor-type-size-accessor type)
                          descriptor))
                   (copy (make-bytevector size)))
              (bytevector-copy! bytevector offset copy 0 size)
              copy)
            ((bytestructure-descriptor-type-accessor type)
             bytevector descriptor offset))
        (if (not (bytestructure-descriptor-type-compound? type))
            (error "Superfluous accessors." accessors)
            ((bytestructure-descriptor-type-accessor type)
             bytevector descriptor accessors offset)))))

;;; Vector

(define-record-type :vector-descriptor
  (vector-descriptor* content-descriptor length size)
  vector-descriptor?
  (content-descriptor vector-descriptor-content-descriptor)
  (length vector-descriptor-length)
  (size vector-descriptor-size))

(define (vector-descriptor content-description length)
  (assert (and (integer? length) (<= 0 length)))
  (let ((content-descriptor (bytestructure-descriptor content-description)))
    (vector-descriptor*
     content-descriptor length
     (* length (bytestructure-descriptor-size content-descriptor)))))

(define (vector-access bytevector descriptor accessors offset)
  (let ((content-descriptor (vector-descriptor-content-descriptor descriptor))
        (index (car accessors))
        (rest-accessors (cdr accessors)))
    (bytestructure-access bytevector content-descriptor rest-accessors
                          (+ offset (* index (bytestructure-descriptor-size
                                              content-descriptor))))))

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

(define (structure-access bytevector descriptor accessors offset)
  (let ((fields (structure-descriptor-fields descriptor))
        (target-field-name (car accessors)))
    (let try-next ((field (car fields))
                   (fields (cdr fields))
                   (offset offset))
      (if (eq? (field-name field) target-field-name)
          (bytestructure-access bytevector
                                (field-content-descriptor field)
                                (cdr accessors)
                                offset)
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

(define (union-access bytevector descriptor accessors offset)
  (bytestructure-access bytevector
                        (field-content-descriptor
                         (assq (car accessors)
                               (union-descriptor-fields type)))
                        (cdr accessors)
                        offset))

(define-bytestructure-descriptor-type
  'union
  union-descriptor
  union-descriptor?
  union-descriptor-size
  union-access
  #t)
