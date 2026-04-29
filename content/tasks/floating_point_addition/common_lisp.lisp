;; Rationals are the default for decimal literals when written as fractions.
(format t "~a~%" (+ 1/10 2/10))            ; 3/10
(format t "~a~%" (= (+ 1/10 2/10) 3/10))   ; T

;; Decimal literals like 0.1 are read as single-floats by default.
(format t "~a~%" (+ 0.1 0.2))              ; 0.3 (rounded for display)
(format t "~a~%" (= (+ 0.1 0.2) 0.3))      ; NIL

;; Double-floats expose the IEEE 754 representation.
(format t "~a~%" (+ 0.1d0 0.2d0))          ; 0.30000000000000004d0
