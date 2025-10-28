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

(define (read-case-records column)
  (read-records
    (lambda (record)
      (let ((to (list-ref record column)))
        (and
          (not (equal? to ""))
          (list (first record) to))))))

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

; Space

(define (read-space-records)
  (filter
    (lambda (x)
      (> x 255))
    (map
      (lambda (record)
        (string->number (car record) 16))
      (read-records
        (lambda (record)
          (and (eqv? (string-ref (list-ref record 2) 0) #\Z) record))))))

; Main

(define type (caddr (command-line)))

(write
  (cond
    ((equal? type "downcase")
      (group-records
        (differentiate-records
          (parse-case-records
            (read-case-records 13)))))
    ((equal? type "fold")
      (group-records
        (differentiate-records
          (filter-fold-records
            (parse-fold-records
              (read-records))))))
    ((equal? type "upcase")
      (group-records
        (differentiate-records
          (parse-case-records
            (read-case-records 14)))))
    ((equal? type "space")
      (read-space-records))
    (else
      (error "unknown unicode data type"))))
