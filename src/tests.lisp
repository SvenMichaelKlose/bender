; bender – Copyright (c) 2015,2024 Sven Michael Klose <pixel@copei.de>

(fn test-opcode-instruction-symmetry ()
  (format t "Testing symmetry of INSTRUCTION-OPCODE and OPCODE-INSTRUCTION: ")
  (adotimes 256
    (format t " ~A~F" !)
    (let i (opcode-instruction !)
      (unless (eq 'ill (instruction-mnemonic i))
        (unless (== ! (instruction-opcode i))
          (print !)
          (error "Instruction generates opcode ~A instead of ~A."
                 (print-hexword (instruction-opcode i) nil)
                 (print-hexword ! nil))))))
  (format t " – O.K.~%"))

(fn test-compare-files (a b)
  (| (string== (fetch-file a) (fetch-file b))
     (error "Test failed! Files '~A' and '~A' differ." a b)))

(fn do-bender-test (name)
  (with (source    (+ "tests/" name ".asm")
         result    (+ "obj/" name ".bin")
         reference (+ "tests/" name ".bin"))
    (assemble-files result source)
    (test-compare-files result reference)))

(test-opcode-instruction-symmetry)

(disassemble-file "tests/all_instructions.bin" "obj/all_instructions_diassembled.lst")

(do-bender-test "all_instructions")
;(do-bender-test "inline-lisp")
(do-bender-test "if")

(fn test-tap ()
  (with-output-file o "obj/test.tap"
    (write-tap o
      (bin2cbmtap (string-list (fetch-file "obj/all_instructions.bin"))
                  "ALL INSTRUCTIONS"
                  :start #x1001)))
  (test-compare-files "obj/test.tap"
                      "tests/test.tap"))

(test-tap)

(with-io i "obj/test.tap" o "obj/test.wav"
  (tap2wav i o 441000 (cpu-cycles :pal)))

(with-temporary *model* :vic-20
  (assemble-files "obj/cbm-basic-init.prg"
                  "vic-20/basic-loader.asm"
                  "growroom/cbm-basic-init.asm"))

;(assemble-files "obj/segment.bin" "tests/segment.asm")
(do-bender-test "segment")
