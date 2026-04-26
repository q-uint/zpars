; Tree-sitter-style queries over examples/calc.peg parse trees.
;
; Run with:  zpars query examples/calc.peg examples/calc.scm "1+2*3"

; Capture every Factor (the digit leaves of an arithmetic expression).
(Factor) @factor

; Capture only odd-digit Factors via a regex predicate.
((Factor) @odd
 (#match? @odd "[13579]"))

; Capture Terms that contain more than one Factor (i.e. a multiplication).
(Term (Factor) (Factor)) @mul

; Capture the very first Term inside an Expr.
(Expr . (Term) @first-term)
