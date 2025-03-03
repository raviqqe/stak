(import (scheme base) (scheme write) (stak base))

(define constant-instruction 0)
(define get-instruction 1)
(define set-instruction 2)
(define if-instruction 3)
(define nop-instruction 4)
(define call-instruction 5)

(define integer-base 128)
(define number-base 16)
(define tag-base 16)
(define share-base 31)

(define-record-type stack
  (make-stack values)
  stack?
  (values stack-values stack-set-values!))

(define (stack-top stack)
  (car (stack-values stack)))

(define (stack-push! stack value)
  (stack-set-values! stack (cons value (stack-values stack))))

(define (stack-pop! stack)
  (let ((value (car (stack-values stack))))
    (stack-set-values! stack (cdr (stack-values stack)))
    value))

(define (stack-swap! stack index)
  (let* ((values (stack-values stack))
         (pair (list-tail values (- index 1)))
         (head (cdr pair))
         (tail (cdr head)))
    (set-cdr! head values)
    (set-cdr! pair tail)
    (stack-set-values! stack head)))

(define (decode-integer-tail x base)
  (let loop ((x x)
             (y (quotient x 2))
             (base base))
    (if (even? x)
      y
      (let ((x (read-u8)))
        (loop x (+ y (* base (quotient x 2))) (* base integer-base))))))

(define (decode-number integer)
  (cond
    ((even? integer)
      (quotient integer 2))

    ((even? (quotient integer 2))
      (- (quotient integer 4)))

    (else
      (let* ((x (quotient integer 4))
             (m (* (if (even? x) 1 -1) (quotient x 4096)))
             (e (- (modulo (quotient integer 2) 2048) 1023)))
        (* m (expt 2 e))))))

(define (decode-ribs)
  (define dictionary (make-stack '()))
  (define stack (make-stack '()))

  (do ((byte (read-u8) (read-u8)))
    ((eof-object? byte))
    (cond
      ((even? byte)
        (stack-push! stack (cons (quotient byte 2) (stack-pop! stack))))

      ((even? (quotient byte 2))
        (let ((head (quotient byte 4)))
          (if (zero? head)
            (stack-push! dictionary (stack-top stack))
            (let* ((head (quotient byte 4))
                   (integer (decode-integer-tail (- head 1) share-base))
                   (index (quotient integer 2)))
              (when (> index 0)
                (stack-swap! dictionary index))
              (let ((value (stack-top dictionary)))
                (when (even? integer)
                  (stack-pop! dictionary))
                (stack-push! stack value))))))

      ((even? (quotient byte 4))
        (let* ((d (stack-pop! stack))
               (a (stack-pop! stack))
               (tag (decode-integer-tail (quotient byte 8) tag-base)))
          (stack-push! stack (rib a d tag))))

      (else
        (stack-push!
          stack
          (decode-number (decode-integer-tail (quotient byte 8) number-base))))))

  (stack-pop! stack))

(define (display-deep-ribs codes depth)
  ; TODO Handle the triforce.
  (do ((codes (rib-cdr codes) (rib-cdr codes)))
    ((null? codes))
    (write-string (make-string depth #\space))
    (display "- ")
    (let ((a (rib-car codes)))
      (display (rib-tag codes))
      (write-char #\space)
      (if (number? a)
        (begin
          (display a)
          (newline))
        (begin
          (newline)
          (display-deep-ribs a (+ depth 2)))))))

(define (display-ribs codes)
  (display-deep-ribs codes 0))

(display-ribs (decode-ribs))
