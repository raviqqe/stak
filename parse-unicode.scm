(import
  (scheme base)
  (scheme char)
  (scheme cxr)
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

(define (list->pair record)
  (cons (car record) (cadr record)))

(define (filter-records records)
  (map
    (lambda (record)
      (cons (car record) (cddr record)))
    (filter
      (lambda (record)
        (member (cadr record) '("C" "F")))
      records)))

(define (group-records records)
  (let ((records
          (map
            (lambda (record)
              (cons (car record) (cddr record)))
            (filter
              (lambda (record)
                (member (cadr record) '("C" "F")))
              records))))
    (reverse
      (map
        (lambda (group)
          (let ((step (car group)))
            (if (not step)
              (list->pair (cadr group))
              (let* ((records (cdr group))
                     (start (list->pair (last records)))
                     (end (list->pair (car records))))
                (append
                  (list
                    'step
                    step
                    (+ (/ (- (car end) (car start)) step) 1)
                    start)
                  (cddar records))))))
        (let loop ((records records) (groups '()))
          (if (null? records)
            groups
            (let ((record (car records)))
              (loop
                (cdr records)
                (if (and
                     (pair? groups)
                     (number? (cadr record))
                     (number? (car (cdadar groups))))
                  (let* ((group (car groups))
                         (step (car group))
                         (step? (lambda (count)
                                 (and
                                   (or (not step) (= step count))
                                   (= (car record) (+ (caadr group) count))
                                   (= (cadr record) (+ (cadadr group) count))
                                   (equal? (cddr record) (cddadr group))))))
                    (cond
                      ((step? 1)
                        (cons
                          (cons 1 (cons record (cdr group)))
                          (cdr groups)))
                      ((step? 2)
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
