(import (scheme base) (scheme write))

(define (render-css-class class)
  (string-append
    (symbol->string (car class))
    "{"
    "}"
    foo))

(define (render element)
  (case (car element)
    ((svg text)
      foo)
    ((style)
      (string-append
        "<style>"
        (apply
          string-append
          (map render-css-class (cdr element)))
        "</style>"))
    (else
      "foo")))

(display
  (render
    '(svg ((view-box "0 0 20 20") (xmlns "http://www.w3.org/2000/svg"))
      (style
       (text
        (dominant-baseline "central")
        (font-size: "18px")
        (text-anchor "middle")
        (padding-top "2px")))
      (text ((x "50%") (y "50%")) "λ")
      (text ((x "50%") (y "50%")) "𝛌")
      (text ((x "50%") (y "50%")) "𝜆")
      (text ((x "50%") (y "50%")) "𝝀")
      (text ((x "50%") (y "50%")) "𝝺")
      (text ((x "50%") (y "50%")) "𝞴"))))
