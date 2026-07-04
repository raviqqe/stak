(import
  (scheme base)
  (scheme write))

; Floating-point number literals are encoded in bytecode files as mantissas of
; up to 38 bits and exponents. The literals below exercise the encoder's edge
; cases: an exactly representable fraction, a decimal fraction rounded to the
; mantissa limit, mantissas around the limit of 2^38, and a positive exponent.
(for-each
  (lambda (x)
    (write x)
    (newline))
  '(0.5 0.1 137438953471.5 274877906943.5 549755813888.5))
