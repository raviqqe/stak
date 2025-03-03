(import (scheme base) (scheme write) (stak base))

(define constant-instruction 0)
(define get-instruction 1)
(define set-instruction 2)
(define if-instruction 3)
(define nop-instruction 4)
(define call-instruction 5)

(define procedure-type 3)

(define integer-base 128)
(define number-base 16)
(define tag-base 16)
(define share-base 31)

(define (display-instruction instruction)
  (write-string
    (case instruction
      ((0)
        "constant")
      ((1)
        "get")
      ((2)
        "set")
      ((3)
        "if")
      ((4)
        "nop")
      (else
        (string-append
          "call-"
          (number->string (- instruction call-instruction)))))))

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

(define-record-type display-context
  (make-display-context false)
  display-context?
  (false display-context-false))

(define (display-context-true context)
  (cdr (display-context-false context)))

(define (display-context-null context)
  (car (display-context-false context)))

(define (display-indent depth)
  (write-string (make-string (* 2 depth) #\space)))

(define (display-procedure context procedure depth)
  (let ((code (car procedure)))
    (if (number? code)
      (begin
        (display-indent depth)
        (display "- primitive: ")
        (write code)
        (newline))
      (let ((arity (car code)))
        (display-indent depth)
        (display "- arity: ")
        (write (quotient arity 2))
        (newline)
        (let ((depth (+ depth 1)))
          (display-indent depth)
          (display "variadic: ")
          (write (odd? arity))
          (newline)
          (display-indent depth)
          (display "code:")
          (newline)
          (display-code context (cdr code) depth))))))

(define (display-data context data depth)
  (if (number? data)
    (begin
      (write data)
      (newline))
    (cond
      ((eq? data (display-context-null context))
        (write-string "()")
        (newline))
      ((eq? data (display-context-false context))
        (write-string "#f")
        (newline))
      ((eq? data (display-context-true context))
        (write-string "#t")
        (newline))
      ((eq? (rib-tag data) procedure-type)
        (newline)
        (display-procedure context data depth))
      (else
        (write data)
        (newline)))))

(define (display-code context code depth)
  (do ((code code (rib-cdr code)))
    ((eq? code (display-context-null context)))
    (display-indent depth)
    (display "- ")
    (let ((a (rib-car code)))
      (display-instruction (rib-tag code))
      (if (= (rib-tag code) if-instruction)
        (begin
          (newline)
          (display-code context a (+ depth 1)))
        (begin
          (write-char #\space)
          (display-data context a (+ depth 1)))))))

(define (display-ribs code)
  (display-code (make-display-context (car code)) (cdr code) 0))

(display-ribs (decode-ribs))
