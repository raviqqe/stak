(import
  (scheme base)
  (scheme cxr)
  (scheme file)
  (scheme process-context)
  (scheme read)
  (scheme write)
  (srfi 1))

(define (read-libraries prelude-path)
  (map
    (lambda (library)
      (cons
        (cadr library)
        (append-map
          (lambda (clause)
            (if (and
                 (pair? clause)
                 (eq? (car clause) 'export))
              (cdr clause)
              '()))
          library)))
    (filter
      (lambda (value)
        (and
          (eq? (car value) 'define-library)
          (eq? (caadr value) 'scheme)))
      (with-input-from-file prelude-path
        (lambda ()
          (let loop ((value (read)))
            (if (eof-object? value)
              '()
              (cons value (loop (read))))))))))

(define arguments (command-line))
(define prelude-path (list-ref arguments 2))
(define library-directory (list-ref arguments 3))

(for-each
  (lambda (library)
    (with-input-from-file (string-append
                           library-directory
                           "/"
                           (symbol->string (cadar library))
                           ".scm")
      (lambda ()
        (write (car library))
        (newline)
        (do ((value (read) (read)))
          ((eof-object? value))
          (unless (memq value (cdr library))
            (display "  ")
            (write value)
            (newline))))))
  (read-libraries prelude-path))
