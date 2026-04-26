; Captures the function's name and body via field selectors.
;
;   zpars query examples/func.peg examples/func.scm "function foo{}"
(Func
  name: (Ident) @fname
  body: (Body) @fbody)
