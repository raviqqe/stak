(import
  (scheme base)
  (scheme file)
  (scheme read)
  (scheme write)
  (srfi 1))

(define basenames
  '(base
    case-lambda
    char
    complex
    cxr
    eval
    file
    inexact
    lazy
    load
    process-context
    read
    repl
    time
    write))

(define (read-libraries)
  (filter
    (lambda (value)
      (eq? (car value) 'define-library))
    (with-input-from-file "prelude.scm"
      (lambda ()
        (let loop ((value (read)))
          (if (eof-object? value)
            '()
            (cons value (loop (read)))))))))

; (for-each
;   (lambda (basename)
;     (with-input-from-file (string-append
;                            "tools/r7rs/"
;                            (symbol->string basename)
;                            ".scm")
;       (lambda ()
;         (do ((value (read) (read)))
;           ((eof-object? value))
;           (write value)
;           (newline)))))
;   basenames)

(write (read-libraries))
