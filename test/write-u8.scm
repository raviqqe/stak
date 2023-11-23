($$define-syntax define-syntax
  (syntax-rules ()
    ((_ name value)
      ($$define-syntax name value))))

(define-syntax define
  (syntax-rules ()
    ((_ (name argument ...) body1 body2 ...)
      (define name (lambda (argument ...) body1 body2 ...)))

    ((_ (name argument ... . rest) body1 body2 ...)
      (define name (lambda (argument ... . rest) body1 body2 ...)))

    ((_ name value)
      ($$define name value))))

(define-syntax lambda
  (syntax-rules (define define-syntax)
    ((_ arguments (define content ...) body1 body2 ...)
      (lambda "value" arguments () (define content ...) body1 body2 ...))

    ((_ "value" arguments ((name value) ...)
        (define (new-name argument ...) body1 body2 ...)
        body3
        body4
        ...)
      (lambda "value" arguments ((name value) ...)
        (define new-name (lambda (argument ...) body1 body2 ...))
        body3
        body4
        ...))

    ((_ "value" arguments ((name value) ...)
        (define (new-name argument ... . rest) body1 body2 ...)
        body3
        body4
        ...)
      (lambda "value" arguments ((name value) ...)
        (define new-name (lambda (argument ... . rest) body1 body2 ...))
        body3
        body4
        ...))

    ((_ "value" arguments ((name value) ...) (define new-name new-value) body1 body2 ...)
      (lambda "value" arguments ((name value) ... (new-name new-value)) body1 body2 ...))

    ((_ "value" arguments ((name value) ...) body1 body2 ...)
      (lambda arguments (letrec* ((name value) ...) body1 body2 ...)))

    ((_ arguments (define-syntax name value) body1 body2 ...)
      (lambda "syntax" arguments ((name value)) body1 body2 ...))

    ((_ "syntax" arguments ((name value) ...) (define-syntax new-name new-value) body1 body2 ...)
      (lambda "syntax" arguments ((name value) ... (new-name new-value)) body1 body2 ...))

    ((_ "syntax" arguments ((name value) ...) body1 body2 ...)
      (lambda arguments (letrec-syntax ((name value) ...) body1 body2 ...)))

    ((_ arguments body1 body2 ...)
      ($$lambda arguments (begin body1 body2 ...)))))

(define procedure-type 1)
(define rib-id 17))
(define write-u8 ($$rib rib-id '() procedure-type))

(write-u8 65)
