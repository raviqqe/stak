(import
  (scheme base)
  (scheme char)
  (scheme cxr)
  (scheme read)
  (scheme write)
  (scheme process-context)
  (srfi 1))

(define (parse-token)
  (list->string
    (let loop ()
      (if (or
           (eof-object? (peek-char))
           (eqv? (peek-char) #\;))
        '()
        (let ((character (read-char)))
          (cons character (loop)))))))

(define (parse-tokens)
  (do ((character (peek-char) (peek-char)))
    ((not (eqv? character #\space)))
    (read-char))

  (let ((token (parse-token)))
    (cons token
      (if (let ((character (read-char)))
           (or
             (eof-object? character)
             (eqv? character #\#)))
        '()
        (parse-tokens)))))

(define (read-records . rest)
  (define filter
    (if (null? rest)
      (lambda (x) x)
      (car rest)))

  (let ((line (read-line)))
    (cond
      ((eof-object? line)
        '())
      ((or (equal? line "") (eqv? (string-ref line 0) #\#))
        (read-records filter))
      (else
        (let ((record
                (parameterize ((current-input-port (open-input-string line)))
                  (filter (parse-tokens)))))
          (if record
            (cons
              record
              (read-records filter))
            (read-records filter)))))))

(define (parse-character-code code)
  (string->number code 16))

(define (parse-case-records records)
  (map
    (lambda (record)
      (map parse-character-code record))
    records))

(define (parse-fold-records records)
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

(define (filter-fold-records records)
  (map
    (lambda (record)
      (cons (car record) (cddr record)))
    (filter
      (lambda (record)
        (member (cadr record) '("C" "S")))
      records)))

(define (differentiate-records records)
  (let loop ((previous '(0 0)) (records records))
    (if (null? records)
      '()
      (let ((current (car records)))
        (cons
          (let ((step
                  (list
                    (- (car current) (car previous))
                    (- (cadr current) (cadr previous)))))
            (if (= (car step) (cadr step))
              (car step)
              step))
          (loop current (cdr records)))))))

(define (group-records records)
  (let loop ((count 0) (records records))
    (if (null? records)
      '()
      (let ((record (car records))
            (records (cdr records)))
        (if (and
             (number? record)
             (eq? record (car records)))
          (loop (+ count 1) records)
          (cons
            (if (pair? record)
              record
              (cons count record))
            (loop 0 records)))))))

(define type (caddr (command-line)))

(write
  (group-records
    (differentiate-records
      (cond
        ((equal? type "downcase")
          (parse-case-records
            (read-records
              (lambda (record)
                (let ((lower (list-ref record 13)))
                  (and
                    (not (equal? lower ""))
                    (list (first record) lower)))))))
        ((equal? type "fold")
          (filter-fold-records
            (parse-fold-records
              (read-records))))
        ((equal? type "upcase")
          (parse-case-records
            (read-records
              (lambda (record)
                (let ((upper (list-ref record 14)))
                  (and
                    (not (equal? upper ""))
                    (list (first record) upper)))))))
        (else
          (error "unknown unicode data type"))))))
