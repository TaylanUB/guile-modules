;;; Structured access to bytevector contents.  We try to imitate C's numeric
;;; types, arrays, structs, and unions here.  You use a "descriptor" together
;;; with "accessors" to access parts of a bytevector whose contents conform to
;;; what is described in your descriptor.  Like you had a void* in C and casted
;;; some array/struct/union on it, then accessed members.

(define-module (taylan bytestructures)
  #:export (bytestructure-descriptor
            bytestructure-descriptor?
            bytestructure-descriptor-size
            bytestructure-access
            numeric-descriptor
            numeric-descriptor?
            numeric-descriptor-signed?
            numeric-descriptor-size
            numeric-descriptor-type
            numeric-descriptor-byte-order
            numeric-access
            vector-descriptor
            vector-descriptor?
            vector-descriptor-content-descriptor
            vector-descriptor-length
            vector-descriptor-size
            vector-access
            structure-descriptor
            structure-descriptor?
            structure-descriptor-fields
            structure-descriptor-size
            structure-access
            union-descriptor
            union-descriptor?
            union-descriptor-fields
            union-descriptor-size
            union-access))

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (rnrs bytevector))

(define-syntax assert
  (syntax-rules ()
    ((assert expression)
     (unless expression
       (error "Assertion not met." 'expression)))))

;;; Generals

(define (bytestructure-descriptor description)
  ;; TODO
  )

(define (bytestructure-descriptor? obj)
  (any (lambda (pred) (pred obj))
       (list numeric-descriptor?
             vector-descriptor?
             structure-descriptor?
             union-descriptor?)))

(define (bytestructure-descriptor-size descriptor)
  (apply (cond
          ((numeric-descriptor? descriptor)
           numeric-descriptor-size)
          ((vector-descriptor? descriptor)
           vector-descriptor-size)
          ((structure-descriptor? descriptor)
           structure-descriptor-size)
          (else
           union-descriptor-size))
         (list descriptor)))

(define (bytestructure-access bytevector descriptor accessors offset)
  (if (null? accessors)
      (if (numeric-descriptor? descriptor)
          (numeric-access bytevector descriptor offset)
          (let* ((size (bytestructure-descriptor-size descriptor))
                 (copy (make-bytevector size)))
            (bytevector-copy! bytevector offset copy 0 size)
            copy))
      (if (numeric-descriptor? descriptor)
          (error "Superfluous accessors." accessors)
          (apply (cond
                  ((vector-descriptor? descriptor) vector-access)
                  ((structure-descriptor? descriptor) structure-access)
                  (else union-access))
                 (list bytevector descriptor accessors offset)))))

;;; Numeric descriptors

(define-record-type :numeric-descriptor
  (numeric-descriptor* signed? size set byte-order)
  numeric-descriptor?
  (signed? numeric-descriptor-signed?)
  (size numeric-descriptor-size)
  (set numeric-descriptor-type)
  (byte-order numeric-descriptor-byte-order))

(define (numeric-descriptor signed? size set . maybe-byte-order)
  (assert (boolean? signed?))
  (assert (and (integer? size) (< 0 size)))
  (assert (memq set '(bool char integer float complex)))
  (assert (or (null? maybe-byte-order)
              (and (memq (car maybe-byte-order) (list (endianness little)
                                                      (endianness big)))
                   (null? (cdr maybe-byte-order)))))
  (assert (if (= size 1) (null? maybe-byte-order) #t))
  (let ((byte-order (if (null? maybe-byte-order)
                        (native-endianness)
                        (car maybe-byte-order))))
    (numeric-descriptor* signed? size set byte-order)))

(define (numeric-access bytevector descriptor offset)
  ;; TODO
  (bytevector-u8-ref bytevector offset))

;;; Vector descriptors

(define-record-type :vector-descriptor
  (vector-descriptor* content-descriptor length size)
  vector-descriptor?
  (content-descriptor vector-descriptor-content-descriptor)
  (length vector-descriptor-length)
  (size vector-descriptor-size))

(define (vector-descriptor content-descriptor length)
  (assert (bytestructure-descriptor? content-descriptor))
  (assert (and (integer? length) (<= 0 length)))
  (vector-descriptor*
   content-descriptor length
   (* length (bytestructure-descriptor-size content-descriptor))))

(define (vector-access bytevector descriptor accessors offset)
  (let ((content-descriptor (vector-descriptor-content-descriptor descriptor))
        (index (car accessors))
        (rest-accessors (cdr accessors)))
    (bytestructure-access bytevector content-descriptor rest-accessors
                          (+ offset (* index (bytestructure-descriptor-size
                                              content-descriptor))))))

;;; Field descriptors

(define (field? obj)
  (and (list? obj)
       (= (length obj 2))
       (symbol? (car obj))
       (bytestructure-descriptor? (cadr obj))))

(define (field-name field)
  (car field))

(define (field-content-descriptor field)
  (cadr field))

;;; Structure descriptors

(define-record-type :structure-descriptor
  (structure-descriptor* fields size)
  structure-descriptor?
  (fields structure-descriptor-fields)
  (size structure-descriptor-size))

(define (structure-descriptor fields)
  (assert (list? fields))
  (assert (every field? fields))
  (structure-descriptor*
   fields (apply + (map (lambda (field)
                          (bytestructure-descriptor-size
                           (field-content-descriptor field)))
                        fields))))

(define (structure-access bytevector descriptor accessors offset)
  (let ((fields (structure-descriptor-fields descriptor))
        (target-field-name (car accessors))
        (rest-accessors (cdr accessors)))
    (let try-next ((field (car fields))
                   (fields (cdr fields))
                   (offset offset))
      (if (eq? (field-name field) target-field-name)
          (bytestructure-access bytevector
                                (field-content-descriptor field)
                                rest-accessors
                                offset)
          (try-next (car fields)
                    (cdr fields)
                    (+ offset (bytestructure-descriptor-size
                               (field-content-descriptor field))))))))

;;; Union descriptors

(define-record-type :union-descriptor
  (union-descriptor* fields size)
  union-descriptor?
  (fields union-descriptor-fields)
  (size union-descriptor-size))

(define (union-descriptor fields)
  (assert (list? fields))
  (assert (every field? fields))
  (union-descriptor*
   fields (apply max (map (lambda (field)
                            (bytestructure-descriptor-size
                             (field-content-descriptor field)))
                          fields))))

(define (union-access bytevector descriptor accessors offset)
  (bytestructure-access bytevector
                        (field-type (assq (car accessors)
                                          (union-descriptor-fields type)))
                        (cdr accessors)
                        offset))
