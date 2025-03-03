(import (scheme base) (scheme write) (stak base))

(define if-instruction 3)
(define call-instruction 5)

(define procedure-type 3)

(define integer-base 128)
(define number-base 16)
(define tag-base 16)
(define share-base 31)

; Decoding

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

(define (decode)
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

; Marshalling

(define-record-type marshal-context
  (make-marshal-context false true null)
  marshal-context?
  (false marshal-context-false)
  (true marshal-context-true)
  (null marshal-context-null))

(define (marshal-value context value)
  (cond
    ((eq? value (marshal-context-false context))
      #f)
    ((eq? value (marshal-context-true context))
      #t)
    ((eq? value (marshal-context-null context))
      '())
    (else
      ; 0 as a false value
      0)))

(define (marshal-value! context value)
  (when (rib? value)
    (let ((a (marshal-value context (car value)))
          (d (marshal-value context (cdr value))))
      (if (eq? a 0)
        (marshal-value! context (car value))
        (set-car! value a))
      (if (eq? d 0)
        (marshal-value! context (cdr value))
        (set-cdr! value d)))))

(define (marshal! rib)
  (marshal-value!
    (make-marshal-context (car rib) (cdar rib) (caar rib))
    rib))

; Display

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
        (let ((arity (- instruction call-instruction)))
          (string-append
            "call "
            (number->string (quotient arity 2))
            " #"
            (if (even? arity) "f" "t")))))))

(define (display-procedure procedure depth)
  (let ((code (car procedure)))
    (if (number? code)
      (begin
        (display "primitive ")
        (write code)
        (newline))
      (let ((arity (car code)))
        (display "procedure ")
        (write (quotient arity 2))
        (write-char #\space)
        (write (odd? arity))
        (newline)
        (display-code (cdr code) depth)))))

(define (display-data data depth)
  (if (number? data)
    (begin
      (write data)
      (newline))
    (cond
      ((eq? (rib-tag data) procedure-type)
        (display-procedure data depth))
      (else
        (write data)
        (newline)))))

(define (display-code code depth)
  (do ((code code (rib-cdr code)))
    ((null? code))
    (write-string (make-string (* 2 depth) #\space))
    (display "- ")
    (let ((a (rib-car code)))
      (display-instruction (rib-tag code))
      (if (= (rib-tag code) if-instruction)
        (begin
          (newline)
          (display-code a (+ depth 1)))
        (begin
          (write-char #\space)
          (display-data a (+ depth 1)))))))

(define (display-ribs code)
  (display-code (cdr code) 0))

(define ribs (decode))
(marshal! ribs)
(display-ribs ribs)
