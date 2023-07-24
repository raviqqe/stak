(define procedure-tag 1)

; Primitives

(define cons (rib 1 '() procedure-tag))
(define id (rib 2 '() procedure-tag))
(define pop (rib 3 '() procedure-tag))
(define skip (rib 4 '() procedure-tag))
(define close (rib 5 '() procedure-tag))
(define is-cons (rib 6 '() procedure-tag))
(define rib-car (rib 7 '() procedure-tag))
(define rib-cdr (rib 8 '() procedure-tag))
(define rib-set-car! (rib 9 '() procedure-tag))
(define rib-set-cdr! (rib 10 '() procedure-tag))
(define eq? (rib 11 '() procedure-tag))
(define rib-set-cdr! (rib 10 '() procedure-tag))
(define rib-set-cdr! (rib 10 '() procedure-tag))
