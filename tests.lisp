;;;;; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(assemble-files "out.prg" "tests/all_instructions.asm")
(? (string== (fetch-file "out.prg") (fetch-file "tests/all_instructions.bin"))
   (format t "; All instructions assembled correctly.~%")
   (error "Instructions assembled incorrectly.~%"))

(disassemble-file "tests/all_instructions.bin" "all_instructions_diassembled.asm")

#|
(assemble-files "test.prg"
                "../vic-20/viclib/vic.bender.asm"
                "../vic-20/viclib/basic-loader.bender.asm"
                "test.asm")
(make-vice-commands "test.txt")
|#
