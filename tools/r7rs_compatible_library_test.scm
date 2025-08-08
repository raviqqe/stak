(import
  (scheme base)
  (scheme file)
  (scheme read)
  (scheme write))

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
  (with-input-from-file (string-append "tools/r7rs/" basename ".scm")
    (lambda ()
      (let loop ((value (read)))
        (if (eof-object? value)
          '()
          (cons (cadr value) (loop (read))))))))

(for-each
  (lambda (basename)
    (with-input-from-file (string-append
                           "tools/r7rs/"
                           (symbol->string basename)
                           ".scm")
      (lambda ()
        (do ((value (read) (read)))
          ((eof-object? value))
          (write value)))))
  basenames)

(write (read-libraries))
