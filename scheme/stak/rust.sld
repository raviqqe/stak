(define-library (stak rust)
  (import
    (stak base)
    (stak string)
    (stak symbol))

  (begin
    (do ((names ((primitive 1000)) (cdr names))
         (index 1 (+ index 1)))
      ((null? names))
      (let ((name (car names)))
        (set-car!
          (car
            (member
              (code-points->string name)
              ($$dynamic-symbols)
              (lambda (x y) (equal? x (symbol->string y)))))
          (primitive (+ 1000 index)))))))
