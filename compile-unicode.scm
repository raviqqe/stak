(import
  (scheme base)
  (scheme char)
  (scheme cxr)
  (scheme file)
  (scheme process-context)
  (scheme read)
  (scheme write)
  (srfi 1))

(define (read-blank)
  (do ((character (peek-char) (peek-char)))
    ((not (eqv? character #\space)))
    (read-char)))

(define (trim-end-blank xs)
  (if (null? xs)
    '()
    (let ((ys (trim-end-blank (cdr xs))))
      (cons
        (car xs)
        (if (equal? ys '(#\space))
          '()
          ys)))))

(define (read-raw-token)
  (list->string
    (trim-end-blank
      (let loop ()
        (if (or
             (eof-object? (peek-char))
             (memv (peek-char) '(#\; #\#)))
          '()
          (let ((character (read-char)))
            (cons character (loop))))))))

(define (read-token)
  (read-blank)
  (read-raw-token))

(define (read-tokens)
  (let ((token (read-token)))
    (cons
      token
      (if (let ((character (read-char)))
           (or
             (eof-object? character)
             (eqv? character #\#)))
        '()
        (read-tokens)))))

(define (read-code-point)
  (list->string
    (let loop ()
      (let ((x (peek-char)))
        (if (and
             (char? x)
             (or
               (char-alphabetic? x)
               (char-numeric? x)))
          (let ((x (read-char)))
            (cons x (loop)))
          '())))))

(define (read-code-points)
  (if (eof-object? (peek-char))
    '()
    (let ((x (read-code-point)))
      (read-char)
      (read-char)
      (cons x (read-code-points)))))

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
                (filter (read-tokens)))))
        (when record
          (set-cdr! pair (list record))
          (set! pair (cdr pair)))))))

(define (parse-code-point code)
  (string->number code 16))

(define (parse-case-records records)
  (map
    (lambda (record)
      (map parse-code-point record))
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
            (parse-code-point (car record))))))))

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
            (parse-code-point (car record))))))))

; Fold

(define (parse-fold-records records)
  (map
    (lambda (record)
      (cons
        (parse-code-point (car record))
        (cons
          (cadr record)
          (map parse-code-point (cddr record)))))
    records))

(define (filter-fold-records downcase-chars records)
  (filter
    (lambda (record)
      (let ((pair (assq (car record) downcase-chars)))
        (not
          (and
            pair
            (= (cadr record) (cdr pair))))))
    (map
      (lambda (record)
        (cons (car record) (cddr record)))
      (filter
        (lambda (record)
          (member (cadr record) '("C" "S")))
        records))))

(define (compile-fold-table data-file)
  (let ((downcase-chars
          (map
            (lambda (xs)
              (cons (car xs) (cadr xs)))
            (parse-case-records
              (with-input-from-file data-file
                (lambda ()
                  (read-case-records 13)))))))
    (group-records
      (differentiate-records
        (filter-fold-records
          downcase-chars
          (parse-fold-records
            (read-records)))))))

; Numeric

(define (compile-numeric-table)
  (group-codes
    (differentiate-codes
      (read-records
        (lambda (record)
          (and
            (equal? (caddr record) "Nd")
            (parse-code-point (car record))))))))

; Property

(define (differentiate-prop-records records)
  (let loop ((code 0) (records records))
    (if (null? records)
      '()
      (let* ((record (car records)))
        (cons
          (cons
            (- (car record) code)
            (if (null? (cdr record))
              '()
              (- (cadr record) (car record))))
          (loop
            (last record)
            (cdr records)))))))

(define (compile-prop-table name)
  (append-map
    (lambda (record)
      (if (null? (cdr record))
        (list (car record))
        (list record)))
    (differentiate-prop-records
      (read-records
        (lambda (record)
          (and
            (equal? (cadr record) name)
            (parameterize ((current-input-port (open-input-string (car record))))
              (map
                parse-code-point
                (read-code-points)))))))))

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
    ((equal? subcommand "other-alphabetic")
      (compile-prop-table "Other_Alphabetic"))
    (else
      (error "unknown subcommand"))))
