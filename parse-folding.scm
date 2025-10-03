(import
  (scheme base)
  (scheme char)
  (scheme read)
  (scheme write)
  (srfi 1))

(define (parse-token)
  (list->string
    (let loop ()
      (if (or
           (char-alphabetic? (peek-char))
           (char-numeric? (peek-char)))
        (let ((character (read-char)))
          (cons character (loop)))
        '()))))

(define (parse-tokens)
  (let loop ()
    (do ((character (peek-char) (peek-char)))
      ((not (memv character '(#\; #\space))))
      (read-char))

    (if (eqv? (peek-char) #\#)
      '()
      (let ((token (parse-token)))
        (cons token (loop))))))

(define (read-records)
  (map
    (lambda (line)
      (parameterize ((current-input-port (open-input-string line)))
        (parse-tokens)))
    (let loop ()
      (let ((line (read-line)))
        (cond
          ((eof-object? line)
            '())
          ((or (equal? line "") (eqv? (string-ref line 0) #\#))
            (loop))
          (else
            (cons line (loop))))))))

(define (parse-character-code code)
  (string->number code 16))

(define (parse-records records)
  (map
    (lambda (record)
      (cons
        (parse-character-code (car record))
        (cons
          (cadr record)
          (map parse-character-code (cddr record)))))
    records))

(define (group-records records)
  (let ((records
          (map
            (lambda (record)
              (cons
                (car record)
                (if (null? (cdddr record))
                  (caddr record)
                  (cddr record))))
            (filter
              (lambda (record)
                (member (cadr record) '("C" "F")))
              records))))
    (reverse
      (let loop ((records records) (last #f) (groups '()))
        (if (null? records)
          groups
          (let ((record (car records)))
            (if (not last)
              (loop
                (cdr records)
                record
                (cons record groups))
              (loop
                (cdr records)
                record
                (cons record groups)))))))))

(write
  (group-records
    (parse-records
      (read-records))))
