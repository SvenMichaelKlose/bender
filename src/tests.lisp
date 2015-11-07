; bender – Copyright (c) 2015 Sven Michael Klose <pixel@copei.de>

(defun test-compare-files (a b)
  (| (string== (fetch-file a) (fetch-file b))
     (error "Test failed! Files '~A' and '~A' differ." a b)))

(assemble-files "obj/all_instructions.bin" "tests/all_instructions.asm")
(test-compare-files "obj/all_instructions.bin"
                    "tests/all_instructions.bin")
(test-compare-files "obj/all_instructions.bin.lst"
                    "tests/all_instructions.bin.lst")

(disassemble-file "tests/all_instructions.bin" "obj/all_instructions_diassembled.lst")

(assemble-files "obj/inline-lisp.bin"
                "tests/inline-lisp.asm")

(with-temporary *model* :vic-20
  (assemble-files "obj/cbm-basic-init.prg"
                  "vic-20/basic-loader.asm"
                  "growroom/cbm-basic-init.asm"))
