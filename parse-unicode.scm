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

(define (differentiate-records records)
  (cons
    (car records)
    (let loop ((previous (car records))
               (current (cadr records))
               (records (cddr records)))
      (if (null? records)
        '()
        (cons
          (let ((record
                  (cons
                    (- (car current) (car previous))
                    (cons
                      (- (cadr current) (cadr previous))
                      (cddr current)))))
            (if (and
                 (= (car record) (cadr record))
                 (equal? (cddr previous) (cddr current)))
              (car record)
              record))
          (loop current (car records) (cdr records)))))))

(define (group-records records)
  (let loop ((record #f)
             (count 0)
             (records records))
    (if (null? records)
      '()
      (let ((record (car records))
            (records (cdr records)))
        (cond
          ((pair? record)
            (cons
              record
              (loop record 0 records)))
          ((and
              (number? record)
              (equal? record (car records)))
            (loop record (+ count 1) records))
          (else
            (cons
              (cons count record)
              (loop record 0 records))))))))

(write
  (group-records
    (differentiate-records
      (filter-records
        (parse-records
          (read-records))))))
