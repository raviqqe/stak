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
    (cons
      token
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

  (define root (cons #f '()))
  (define pair root)

  (do ((line (read-line) (read-line)))
    ((eof-object? line) (cdr root))
    (unless (or (equal? line "") (eqv? (string-ref line 0) #\#))
      (let ((record
              (parameterize ((current-input-port (open-input-string line)))
                (filter (parse-tokens)))))
        (when record
          (set-cdr! pair (list record))
          (set! pair (cdr pair)))))))

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

; Alphabetic

(define (compile-alphabetic-table)
  (group-codes
    (differentiate-codes
      (read-records
        (lambda (record)
          (and
            (member (caddr record) '("Lm" "Lo" "Lt" "Nl"))
            (parse-character-code (car record))))))))

; Case

(define (read-case-records column)
  (read-records
    (lambda (record)
      (let ((to (list-ref record column)))
        (and
          (not (equal? to ""))
          (list (car record) to))))))

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
            (parse-character-code (car record))))))))

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

(define (compile-fold-table downcase-file)
  (group-records
    (differentiate-records
      (filter-fold-records
        (parse-fold-records
          (read-records))))))

; Numeric

(define (compile-numeric-table)
  (group-codes
    (differentiate-codes
      (read-records
        (lambda (record)
          (and
            (equal? (caddr record) "Nd")
            (parse-character-code (car record))))))))

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

(define-values (subcommand argument)
  (let ((arguments (command-line)))
    (values
      (cadr arguments)
      (and
        (pair? (cddr arguments))
        (caddr arguments)))))

(write
  (cond
    ((equal? subcommand "alphabetic")
      (compile-alphabetic-table))
    ((equal? subcommand "downcase")
      (compile-case-table 13))
    ((equal? subcommand "fold")
      (compile-fold-table argument))
    ((equal? subcommand "lone-lower")
      (compile-lone-case-table "Ll" 12))
    ((equal? subcommand "lone-upper")
      (compile-lone-case-table "Lu" 13))
    ((equal? subcommand "numeric")
      (compile-numeric-table))
    ((equal? subcommand "upcase")
      (compile-case-table 12))
    ((equal? subcommand "space")
      (compile-space-table))
    (else
      (error "unknown subcommand"))))
