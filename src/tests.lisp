; bender – Copyright (c) 2015 Sven Michael Klose <pixel@copei.de>

(format t "; Assembling reference 'tests/all_instructions.asm' to 'out.prg'…~%")
(assemble-files "out.prg" "tests/all_instructions.asm")
(? (string== (fetch-file "out.prg")
    (fetch-file "tests/all_instructions.bin"))
   (format t "; All instructions assembled correctly.~%")
   (error "Instructions assembled incorrectly.~%"))

(format t "; Diassembling 'out.prg' to match against 'tests/all_instructions_disassembled.asm'…")
(disassemble-file "tests/all_instructions.bin" "all_instructions_diassembled.asm")
(= *model* :vic-20)
(assemble-files "cbm-basic-init.prg"
                "vic-20/basic-loader.asm"
                "growroom/cbm-basic-init.asm")
