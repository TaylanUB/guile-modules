;;; Structured access to bytevector contents.  We try to imitate C's numeric
;;; types, arrays, structs, and unions here.  You use a "descriptor" together
;;; with "accessors" to access parts of a bytevector whose contents conform to
;;; what is described in your descriptor.  Like you had a void* in C and casted
;;; some array/struct/union on it, then accessed members.

(define-module (taylan bytestructures)
  #:export (bytestructure-descriptor?
            bytestructure-access
            vector-descriptor
            vector-descriptor?
            vector-descriptor-type
            vector-descriptor-length
            vector-descriptor-size
            structure-descriptor
            structure-descriptor?
            structure-descriptor-fields
            structure-descriptor-size
            union-descriptor
            union-descriptor?
            union-descriptor-fields
            union-descriptor-size))

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (rnrs bytevector))

(define-syntax assert
  (syntax-rules ()
    ((assert expression)
     (unless expression
       (error "Assertion not met." 'expression)))))

;;; Numeric types

(define-record-type :numeric-type
  (numeric-type* signed? size set byte-order)
  numeric-type?
  (signed? numeric-type-signed?)
  (size numeric-type-size)
  (set numeric-type-set)
  (byte-order numeric-type-byte-order))

(define (numeric-type signed? size set . maybe-byte-order)
  (assert (boolean? signed?))
  (assert (and (integer? size) (< 0 size)))
  (assert (memq set '(integer float complex)))
  (assert (or (null? maybe-byte-order)
              (and (memq (car maybe-byte-order) (list (endianness little)
                                                      (endianness big)))
                   (null? (cdr maybe-byte-order)))))
  (assert (if (= size 1) (null? maybe-byte-order) #t))
  (let ((byte-order (if (null? maybe-byte-order)
                        (native-endianness)
                        (car maybe-byte-order))))
    (numeric-type* signed? size set byte-order)))

(define (numeric-type-access bytevector type offset)
  ;; TODO
  (bytevector-u8-ref bytevector offset))

;;; Vector descriptors

(define-record-type :vector-descriptor
  (vector-descriptor* type length size)
  vector-descriptor?
  (type vector-descriptor-type)
  (length vector-descriptor-length)
  (size vector-descriptor-size))

(define (vector-descriptor type length)
  (assert (bytestructure-descriptor? type))
  (assert (and (integer? length) (<= 0 length)))
  (vector-descriptor*
   type length (* length (bytestructure-descriptor-size type))))

;;; Field descriptors

(define (field? obj)
  (and (list? obj)
       (= (length obj 2))
       (symbol? (car obj))
       (bytestructure-descriptor? (cadr obj))))

(define (field-name field)
  (car field))

(define (field-type field)
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
                           (field-type field)))
                        fields))))

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
                             (field-type field)))
                          fields))))

;;; Generalization

(define (bytestructure-descriptor? obj)
  (any (lambda (pred) (pred obj))
       (list numeric-type?
             vector-descriptor?
             structure-descriptor?
             union-descriptor?)))

(define (bytestructure-descriptor-size type)
  (apply (cond
          ((numeric-type? type)
           numeric-type-size)
          ((vector-descriptor? type)
           vector-descriptor-size)
          ((structure-descriptor? type)
           structure-descriptor-size)
          (else
           union-descriptor-size))
         (list type)))

(define (bytestructure-access bytevector
                              type
                              offset
                              accessors)
  (if (null? accessors)
      (if (numeric-type? type)
          (numeric-type-access bytevector type offset)
          (let* ((size (bytestructure-descriptor-size type))
                 (copy (make-bytevector size)))
            (bytevector-copy! bytevector offset copy 0 size)
            copy))
      (cond
       ((vector-descriptor? type)
        (let ((sub-type (vector-descriptor-type type)))
          (bytestructure-access bytevector
                                sub-type
                                (+ offset (vector-offset type (car accessors)))
                                (cdr accessors))))
       ((structure-descriptor? type)
        (let-values (((sub-type sub-offset)
                      (structure-access-helper type (car accessors))))
          (bytestructure-access bytevector
                                sub-type
                                (+ offset sub-offset)
                                (cdr accessors))))
       ((union-descriptor? type)
        (bytestructure-access bytevector
                              (field-type (assq (car accessors)
                                                (union-descriptor-fields type)))
                              offset
                              (cdr accessors)))
       (else
        (error "Superfluous accessors." accessors)))))

(define (vector-offset type index)
  (* index (bytestructure-descriptor-size type)))

(define (structure-access-helper type target-field-name)
  (let ((fields (structure-descriptor-fields type)))
    (let try-next ((field (car fields))
                   (fields (cdr fields))
                   (offset 0))
      (if (eq? target-field-name (field-name field))
          (values (field-type field) offset)
          (try-next (car fields)
                    (cdr fields)
                    (+ offset (bytestructure-descriptor-size
                               (field-type field))))))))
