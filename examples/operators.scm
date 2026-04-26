; Captures every "+" and "*" anonymous-token node.
;
; Run with:
;   zpars query examples/calc-tokens.peg examples/operators.scm "1+2*3"
"+" @plus
"*" @star
