(import
  (scheme base)
  (scheme cxr)
  (scheme write)
  (scheme process-context)
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

(define arguments (command-line))

(json-write
  `((key . ,(cadr arguments))
    (name . ,(caddr arguments))
    (metrics . ,(list->vector
                 (map
                   (lambda (pair)
                     `((key . ,(string-map
                                (lambda (char)
                                  (case char
                                    ((#\. #\/)
                                      #\_)
                                    (else
                                      char)))
                                (car pair)))
                       (name . ,(car pair))
                       (value . ,(string->number (cdr pair)))
                       (unit . "bytes")))
                   (read-metrics))))
    (acceptables . #())))
