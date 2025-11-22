(import
  (scheme base)
  (scheme process-context)
  (scheme write)
  (stak base))

(define if-instruction 3)
(define call-instruction 4)

(define integer-base 64)
(define number-base 8)
(define tag-base 8)
(define share-base 15)

(define window-size 127)

; Window

(define-record-type window
  (make-window values offset)
  window?
  (values window-values window-set-values!)
  (length window-length window-set-length!))

(define (window-ref window index)
  (list-ref (window-values window) index))

(define (window-push! window x)
  (let ((xs (window-values window))
        (n (window-length window)))
    (window-set-values! window (cons x xs))
    (if (< n (* 2 window-size))
      (window-set-length! window (+ 1 n))
      (begin
        (set-cdr! (list-tail xs window-size) '())
        (window-set-length! window (+ 1 window-size))))))

; Decompressor

(define-record-type decompressor
  (make-decompressor window offset length)
  decompressor?
  (window decompressor-window)
  (offset decompressor-offset decompressor-set-offset!)
  (length decompressor-length decompressor-set-length!))

(define (decompressor-ref decompressor index)
  (window-ref (decompressor-window decompressor) index))

(define (decompressor-push! decompressor x)
  (window-push! (decompressor-window decompressor) x))

(define (read-code decompressor)
  (cond
    ((eof-object? (peek-u8))
      (eof-object))
    ((zero? (decompressor-length decompressor))
      (let* ((x (read-u8))
             (y (quotient x 2)))
        (if (zero? (remainder x 2))
          (begin
            (decompressor-push! decompressor y)
            y)
          (begin
            (decompressor-set-offset! decompressor y)
            (decompressor-set-length! decompressor (read-u8))
            (read-code decompressor)))))
    (else
      (let ((x
              (decompressor-ref
                decompressor
                (decompressor-offset decompressor))))
        (decompressor-push! decompressor x)
        (decompressor-set-length! decompressor (- (decompressor-length decompressor) 1))
        x))))

; Stack

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

; Decoding

(define (decode-integer-tail decompressor x base)
  (let loop ((x x)
             (y (quotient x 2))
             (base base))
    (if (even? x)
      y
      (let ((x (read-code decompressor)))
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
  (define decompressor
    (make-decompressor
      (make-window (make-list window-size 0) 0)
      0
      0))

  (do ((byte (read-code decompressor) (read-code decompressor)))
    ((eof-object? byte))
    (cond
      ((even? byte)
        (stack-push! stack (cons (quotient byte 2) (stack-pop! stack))))
      ((even? (quotient byte 2))
        (let ((head (quotient byte 4)))
          (if (zero? head)
            (stack-push! dictionary (stack-top stack))
            (let* ((head (quotient byte 4))
                   (integer (decode-integer-tail decompressor (- head 1) share-base))
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
               (tag (decode-integer-tail decompressor (quotient byte 8) tag-base)))
          (stack-push! stack (rib a d tag))))
      (else
        (stack-push!
          stack
          (decode-number (decode-integer-tail decompressor (quotient byte 8) number-base))))))

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
  (when (and (rib? value) (not (memq value '(#f #t ()))))
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

(define maximum-continuation-distance 64)

(define (find-continuation branch code)
  (let branch-loop ((branch branch))
    (if (null? branch)
      '()
      (let code-loop ((code code) (index 0))
        (cond
          ((or (null? code) (= index maximum-continuation-distance))
            (branch-loop (cdr branch)))
          ((eq? branch code)
            branch)
          (else
            (code-loop (cdr code) (+ index 1))))))))

(define (display-indent depth)
  (write-string (make-string (* 2 depth) #\space)))

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
        (write-string "procedure ")
        (write (quotient arity 2))
        (write-char #\space)
        (write (odd? arity))
        (newline)
        (display-code (cdr code) '() depth)))))

(define (display-data data depth)
  (cond
    ((number? data)
      (write data)
      (newline))
    ((procedure? data)
      (display-procedure data depth))
    (else
      (write data)
      (newline))))

(define (display-list xs depth)
  (do ((xs xs (cdr xs)))
    ((null? xs))
    (display-indent depth)
    (display "- ")
    (display-top-data (car xs) (+ depth 1))))

(define (display-top-data data depth)
  (if (and (pair? data) (list? data))
    (begin
      (write-string "list")
      (newline)
      (display-list data depth))
    (display-data data depth)))

(define (display-code code continuation depth)
  (let loop ((code code))
    (unless (null? code)
      (display-indent depth)
      (display "- ")
      (if (eq? code continuation)
        (write-string "continue\n")
        (begin
          (let ((operand (car code)))
            (display-instruction (rib-tag code))
            (if (= (rib-tag code) if-instruction)
              (begin
                (newline)
                (display-code operand (find-continuation operand (cdr code)) (+ depth 1)))
              (begin
                (write-char #\space)
                (display-top-data operand (+ depth 1))))
            (loop (cdr code))))))))

(define (display-ribs code)
  (display-code (cdr code) '() 0))

(define (main)
  (let ((ribs (decode)))
    (marshal! ribs)
    (display-ribs ribs)))

(let ((arguments (command-line)))
  (when (or
         (member "-h" arguments)
         (member "--help" arguments))
    (write-string "The Stak Scheme bytecode decoder.\n\n")
    (write-string "Usage: stak-decode < BYTECODE_FILE\n")
    (exit)))

(main)
