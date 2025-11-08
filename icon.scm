(import (scheme base) (scheme write))

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
        (font-size "18px")
        (fill "red")
        (text-anchor "middle")
        (padding-top "2px")))
      ,@(let loop ((chars '(#\λ #\𝛌 #\𝜆 #\𝝀 #\𝝺 #\𝞴)))
         (if (null? chars)
           '()
           (cons
             (text ((x "50%") (y "50%")) char)
             (loop))))
      (text ((x "50%") (y "50%")))
      (text ((x "50%") (y "50%")))
      (text ((x "50%") (y "50%")))
      (text ((x "50%") (y "50%")))
      (text ((x "50%") (y "50%")))
      (text ((x "50%") (y "50%"))))))
