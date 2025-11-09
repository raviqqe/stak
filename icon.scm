(import (scheme base) (scheme write))

(define lambda-chars '(#\λ #\𝛌 #\𝜆 #\𝝀 #\𝝺 #\𝞴))
(define max-lambda-index (- (length lambda-chars) 1))

(define (render-css class)
  (string-append
    (symbol->string (car class))
    "{"
    (apply
      string-append
      (map
        (lambda (pair)
          (string-append (symbol->string (car pair)) ":" (cadr pair) ";"))
        (cdr class)))
    "}"))

(define (render-attribute pair)
  (string-append
    (case (car pair)
      ((view-box)
        "viewBox")
      (else =>
        symbol->string))
    "=\""
    (cadr pair)
    "\""))

(define (render element)
  (if (string? element)
    element
    (case (car element)
      ((style)
        (string-append
          "<style>"
          (apply
            string-append
            (map render-css (cdr element)))
          "</style>"))
      (else =>
        (lambda (name)
          (string-append
            "<"
            (symbol->string name)
            (if (null? (cadr element))
              ""
              (apply
                string-append
                (map
                  (lambda (attribute)
                    (string-append " "
                      (render-attribute attribute)))
                  (cadr element))))
            ">"
            (apply
              string-append
              (map render (cddr element)))
            "</"
            (symbol->string name)
            ">"))))))

(display
  (render
    `(svg
      ((view-box "0 0 20 20")
       (xmlns "http://www.w3.org/2000/svg"))
      (style
       (text
        (dominant-baseline "central")
        (font-size "22px")
        (font-weight "bold")
        (text-anchor "middle")
        (text-shadow "4px 4px 16px rgb(0 0 0 / 0.2)")
        (transform-box "fill-box")
        (transform-origin "center")))
      (text
       ((x "50%")
        (y "50%")
        (fill "white")
        (transform "translate(0 2)rotate(180)")
        (style "font-size: 0.8em"))
       "⟁")
      ,@(let loop ((chars lambda-chars) (index 0))
         (if (null? chars)
           '()
           (cons
             `(text
               ((x "50%")
                (y ,(string-append
                     (number->string (- 65 (* 40 (/ index max-lambda-index))))
                     "%"))
                (fill
                 ,(let ((ratio (* 100 (/ index max-lambda-index))))
                   (string-append
                     "color-mix(in oklab, royalblue "
                     (number->string (- 100 ratio))
                     "%, crimson "
                     (number->string ratio)
                     "%)")))
                (transform
                 ,(string-append
                   "scale(1 0.5)"
                   "rotate("
                   (number->string (+ -45 (* 90 (/ index max-lambda-index))))
                   ")"
                   "scale("
                   (number->string
                     (+
                       (/ 3 5)
                       (*
                         (/ 2 5)
                         (/ (- max-lambda-index index) max-lambda-index))))
                   ")")))
               ,(string (car chars)))
             (loop (cdr chars) (+ index 1))))))))
