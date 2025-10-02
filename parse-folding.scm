(import
  (scheme base)
  (scheme read)
  (scheme write))

(define (read-all)
  (map
    (lambda (line) line)
    (let loop ()
      (let ((line (read-line)))
        (if (eof-object? line)
          '()
          (cons line (loop)))))))

(write (read-all))
