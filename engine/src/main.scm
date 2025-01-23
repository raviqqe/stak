(import (stak base))
(import (scheme base))

(define debug (primitive 1000))
(define make-foo (primitive 1001))

(debug (make-foo))
