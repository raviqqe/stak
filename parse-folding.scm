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
      (map
        (lambda (group)
          (let ((step (car group)))
            (if (not step)
              (cadr group)
              (let ((records (cdr group)))
                (cons step
                  (cons
                    (last records)
                    (car records)))))))
        (let loop ((records records) (groups '()))
          (if (null? records)
            groups
            (let ((record (car records)))
              (loop
                (cdr records)
                (if (and
                     (pair? groups)
                     (number? (cdr record)))
                  (let* ((group (car groups))
                         (step (car group)))
                    (cond
                      ((and
                          (or (not step) (= step 1))
                          (= (car record) (+ (caadr group) 1)))
                        (cons
                          (cons 1 (cons record (cdr group)))
                          (cdr groups)))
                      ((and
                          (or (not step) (= step 2))
                          (= (car record) (+ (caadr group) 2)))
                        (cons
                          (cons 2 (cons record (cdr group)))
                          (cdr groups)))
                      (else
                        (cons (list #f record) groups))))
                  (cons
                    (list #f record)
                    groups))))))))))

(write
  (group-records
    (parse-records
      (read-records))))
