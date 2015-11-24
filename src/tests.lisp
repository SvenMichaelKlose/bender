; bender – Copyright (c) 2015 Sven Michael Klose <pixel@copei.de>

(defun test-compare-files (a b)
  (| (string== (fetch-file a) (fetch-file b))
     (error "Test failed! Files '~A' and '~A' differ." a b)))

(defun do-bender-test (name)
  (with (source    (+ "tests/" name ".asm")
         result    (+ "obj/" name ".bin")
         reference (+ "tests/" name ".bin"))
    (assemble-files result source)
    (test-compare-files result reference)))

(disassemble-file "tests/all_instructions.bin" "obj/all_instructions_diassembled.lst")

(do-bender-test "all_instructions")
(do-bender-test "inline-lisp")
(do-bender-test "if")

(with-output-file o "obj/test.tap"
  (write-tap o
    (bin2cbmtap (string-list (fetch-file "obj/all_instructions.bin"))
                "ALL INSTRUCTIONS"
                :start #x1001)))
(test-compare-files "obj/test.tap"
                    "tests/test.tap")

(with-temporary *model* :vic-20
  (assemble-files "obj/cbm-basic-init.prg"
                  "vic-20/basic-loader.asm"
                  "growroom/cbm-basic-init.asm"))

(assemble-files "obj/segment.bin" "tests/segment.asm")
