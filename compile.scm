(define integer-base 128)

; Constants

(define false-index 0)
(define true-index 1)
(define null-index 2)
(define rib-index 3)

; Instructions

(define call-instruction 0)
(define set-instruction 1)
(define get-instruction 2)
(define constant-instruction 3)
(define if-instruction 4)

;; Codes

(define return-call-code 0)
(define call-code 0)
(define set-code 1)
(define get-code 2)
(define constant-code 3)
(define if-code 4)

; Utility

(define (todo value) (error "not implemented" value))

(cond-expand
  (gambit
    (define (rib tag car cdr)
      (cons (cons tag car) cdr))

    (define (rib-tag rib)
      (caar rib))

    (define (rib-car rib)
      (cdar rib))

    (define (rib-cdr rib)
      (cdr rib))))

; Source code reading

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(define (read-source)
  (cons 'begin (read-all)))

; Target code writing

(define (write-target codes)
  (map write-u8 codes))

; Non-primitive expansion

(define (expand expression)
  (cond
    ((symbol? expression)
      expression)

    ((pair? expression)
      (let ((first (car expression)))
        (cond
          ((eqv? first 'define)
            (let ((pattern (cadr expression)))
              (cons 'set!
                (if (pair? pattern)
                  (list
                    (car pattern)
                    (expand (cons 'lambda (cons (cdr pattern) (cddr expression)))))
                  (list pattern (expand (caddr expression)))))))

          ((eqv? first 'begin)
            (cons 'begin (map expand (cdr expression))))

          (else
            expression))))

    (else
      expression)))

; Context

; (block . symbols)
(define (make-context)
  (cons '() '()))

(define (resolve-variable* variables variable index)
  (cond
    ((null? variables)
      variable)

    ((eqv? (car variables) variable)
      index)

    (else
      (resolve-variable (cdr variables) variable (+ index 1)))))

(define (resolve-variable context variable index)
  (resolve-variable* (car context) variable index))

; Compilation

(define (compile-set variable continuation)
  (rib set-instruction variable continuation))

(define (compile-begin context expressions continuation)
  (compile-expression context
    (car expressions)
    (if (pair? (cdr expressions))
      (compile-begin context (cdr expressions) continuation)
      continuation)))

(define (compile-expression context expression continuation)
  (cond
    ((symbol? expression)
      (todo variable))

    ((pair? expression)
      (let ((first (car expression)))
        (cond
          ((eqv? first 'set!)
            (compile-expression context (caddr expression)
              (compile-set
                (resolve-variable context (cadr expression) 1)
                continuation)))

          ((eqv? first 'begin)
            (compile-begin context (cdr expression) continuation))

          (else
            (todo first)))))

    (else
      (rib constant-instruction expression continuation))))

(define (compile expression)
  (compile-expression (make-context) expression '()))

; Encoding

(define (encode-integer integer target)
  (encode-integer-rest integer #t target))

(define (encode-integer-rest integer first target)
  (append
    (encode-integer-rest (quotient integer integer-base) #f target)
    (cons (* (if first 1 -1) (modulo integer integer-base)) target)))

(define (encode-operand operand)
  (cond
    ((symbol? operand)
      (encode-integer (* operand 2)))

    ((number? operand)
      (encode-integer (+ (* operand 2) 1)))

    (else (error "invalid operand"))))

(define (encode-instruction instruction)
  (let ((tag (rib-tag instruction)))
    (cond
      ((eqv? tag call-instruction)
        (cons
          (if (null? (rib-cdr instruction))
            return-call-code
            call-code)
          (encode-operand (rib-car instruction))))

      ((eqv? tag set-instruction)
        (cons
          set-code
          (encode-operand (rib-car instruction))))

      ((eqv? tag get-instruction)
        (cons
          get-code
          (encode-operand (rib-car instruction))))

      ((eqv? tag constant-instruction)
        (cons
          constant-code
          (encode-operand (rib-car instruction))))

      ((eqv? tag if-instruction)
        (todo instruction))

      (else (error "invalid instruction")))))

(define (encode-codes codes target)
  (if (null? codes)
    target
    (encode-codes
      (rib-cdr codes)
      (append
        (encode-instruction codes)
        target))))

(define (encode codes)
  (encode-codes codes '()))

; Main

(write-target (encode (compile (expand (read-source)))))
