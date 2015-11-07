@(progn
   (format t "~LPrinting something from inlined Lisp expression.~%")
   (asm "jmp $1234"))

@(progn
   (defmacro test-asm-macro ()
     '(asm "lda #0
           tax
           tay"
           "txs"))
   nil)

@(test-asm-macro)
