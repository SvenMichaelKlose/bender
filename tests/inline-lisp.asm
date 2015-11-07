@(progn
   (format t "~LPrinting something from assembly code.~%")
   (asm "jmp $1234"))

@(progn
   (defmacro test-asm-macro ()
     '(asm "lda #0
           tax
           tay"
           "txs"))
   nil)

@(test-asm-macro)
