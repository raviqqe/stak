(lambda () 42)

(define (f) 65)

(write-u8 (f))

(define (f x) x)

(write-u8 (f 65))

(define (g x y) y)

(write-u8 (g 66 65))
