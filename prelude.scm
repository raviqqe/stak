(define stak #t)

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
(define lt? (rib 12 '() procedure-tag))
(define + (rib 13 '() procedure-tag))
(define - (rib 14 '() procedure-tag))
(define * (rib 15 '() procedure-tag))
(define / (rib 16 '() procedure-tag))
(define read-u8 (rib 17 '() procedure-tag))
(define write-u8 (rib 18 '() procedure-tag))
