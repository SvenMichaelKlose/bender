; bender – Copyright (c) 2015 Sven Michael Klose <pixel@copei.de>

(format t "; Assembling reference 'tests/all_instructions.asm' to 'obj/all_instructions.bin'…~%")
(assemble-files "obj/all_instructions.bin" "tests/all_instructions.asm")

(format t "; Diassembling 'obj/all_instructions.bin' to 'obj/all_instructions_disassembled.lst'…")
(disassemble-file "tests/all_instructions.bin" "obj/all_instructions_diassembled.lst")

(= *model* :vic-20)
(? (string== (fetch-file "obj/all_instructions.bin")
             (fetch-file "tests/all_instructions.bin"))
   (format t "; All instructions assembled correctly.~%")
   (error "Instructions assembled incorrectly.~%"))

(assemble-files "obj/cbm-basic-init.prg"
                "vic-20/basic-loader.asm"
                "growroom/cbm-basic-init.asm")

(assemble-files "obj/inline-lisp.bin"
                "tests/inline-lisp.asm")
