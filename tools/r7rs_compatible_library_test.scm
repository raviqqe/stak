(import
  (scheme base)
  (scheme cxr)
  (scheme file)
  (scheme read)
  (scheme write)
  (srfi 1))

(define (read-libraries)
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
      (with-input-from-file "prelude.scm"
        (lambda ()
          (let loop ((value (read)))
            (if (eof-object? value)
              '()
              (cons value (loop (read))))))))))

(for-each
  (lambda (library)
    (with-input-from-file (string-append
                           "tools/r7rs/"
                           (symbol->string (cadar library))
                           ".scm")
      (lambda ()
        (do ((value (read) (read)))
          ((eof-object? value))
          (write value)
          (newline)))))
  (read-libraries))
