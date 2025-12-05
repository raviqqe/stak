(import
  (scheme base)
  (scheme write)
  (srfi 13)
  (srfi 180))

(define (read-metrics)
  (let ((line (read-line)))
    (if (eof-object? line)
      '()
      (cons
        (let ((tokens (string-tokenize line)))
          (cons (car tokens) (cadr tokens)))
        (read-metrics)))))

(json-write (read-metrics))
