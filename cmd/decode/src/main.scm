(import (scheme base) (scheme write))

(define constant-instruction 0)
(define get-instruction 1)
(define set-instruction 2)
(define if-instruction 3)
(define nop-instruction 4)
(define call-instruction 5)

(define integer-base 128)
(define number-base 64)
(define tag-base 32)
(define share-base 31)

(define stack '())

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

(define (decode-ribs data)
  (do ((byte (read-u8) (read-u8)))
    ((eof-object? byte))
    (cond
      ((even? byte)
        (set!
          stack
          (cons
            (decode-number (decode-integer-tail byte number-base))
            stack)))

      ; TODO
      (else
        (write-string "- ")
        (write byte)
        (newline)))))

(decode-ribs #f)
