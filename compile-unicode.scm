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

(define (differentiate-codes codes)
  (let loop ((previous 0) (codes codes))
    (if (null? codes)
      '()
      (cons
        (- (car codes) previous)
        (loop (car codes) (cdr codes))))))

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

(define (group-codes codes)
  (let loop ((count 0) (codes codes))
    (if (null? codes)
      '()
      (let ((code (car codes))
            (codes (cdr codes)))
        (if (and
             (pair? codes)
             (= code (car codes)))
          (loop (+ count 1) codes)
          (cons
            (if (zero? count)
              code
              (cons count code))
            (loop 0 codes)))))))

(define (group-records records)
  (let loop ((count 0) (records records))
    (if (null? records)
      '()
      (let ((record (car records))
            (records (cdr records)))
        (if (and
             (number? record)
             (pair? records)
             (eq? record (car records)))
          (loop (+ count 1) records)
          (cons
            (if (pair? record)
              record
              (cons count record))
            (loop 0 records)))))))

; Case

(define (read-case-records column)
  (read-records
    (lambda (record)
      (let ((to (list-ref record column)))
        (and
          (not (equal? to ""))
          (list (first record) to))))))

(define (compile-case-table column)
  (group-records
    (differentiate-records
      (parse-case-records
        (read-case-records column)))))

(define (compile-lone-case-table category column)
  (group-codes
    (differentiate-codes
      (read-records
        (lambda (record)
          (and
            (equal? (caddr record) category)
            (equal? (list-ref record column) "")
            (parse-character-code (first record))))))))

; Fold

(define (parse-fold-records records)
  (map
    (lambda (record)
      (cons
        (parse-character-code (car record))
        (cons
          (cadr record)
          (map parse-character-code (cddr record)))))
    records))

(define (filter-fold-records records)
  (map
    (lambda (record)
      (cons (car record) (cddr record)))
    (filter
      (lambda (record)
        (member (cadr record) '("C" "S")))
      records)))

(define (compile-fold-table)
  (group-records
    (differentiate-records
      (filter-fold-records
        (parse-fold-records
          (read-records))))))

; Space

(define (compile-space-table)
  (filter
    (lambda (x) (> x 255))
    (map
      (lambda (record) (string->number (car record) 16))
      (read-records
        (lambda (record)
          (and (eqv? (string-ref (list-ref record 2) 0) #\Z) record))))))

; Main

(define type (last (command-line)))

(write
  (cond
    ((equal? type "downcase")
      (compile-case-table 13))
    ((equal? type "fold")
      (compile-fold-table))
    ((equal? type "lone-lower")
      (compile-lone-case-table "Ll" 12))
    ((equal? type "lone-upper")
      (compile-lone-case-table "Lu" 13))
    ((equal? type "upcase")
      (compile-case-table 12))
    ((equal? type "space")
      (compile-space-table))
    (else
      (error "unknown subcommand"))))
