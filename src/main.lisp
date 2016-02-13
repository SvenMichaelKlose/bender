; bender – Copyright (c) 2014–2016 Sven Michael Klose <pixel@copei.de>

;(cl:proclaim '(cl:optimize (cl:speed 0) (cl:space 0) (cl:safety 3) (cl:debug 3)))
(cl:proclaim '(cl:optimize (cl:speed 3) (cl:space 0) (cl:safety 0) (cl:debug 0)))

(tre-load "platforms/shared/read-binary.lisp")
(tre-load "platforms/shared/xml-entities.lisp")
(tre-load "platforms/shared/xml2lml.lisp")
(tre-load "platforms/shared/wavinfo.lisp")

(load "src/6502/cpu.lisp")
(load "src/6502/instruction.lisp")
(load "src/6502/opcode.lisp")
(load "src/6502/cycles.lisp")
(load "src/6502/instruction-map.lisp")

(load "src/disassembler/print.lisp")
(load "src/disassembler/toplevel.lisp")

(load "src/assembler/global.lisp")
(load "src/assembler/message.lisp")
(load "src/assembler/labels.lisp")
(load "src/assembler/tokenizer.lisp")
(load "src/assembler/parser.lisp")
(load "src/assembler/labels-to-exprs.lisp")
(load "src/assembler/assemble.lisp")
(load "src/assembler/helpers.lisp")
(load "src/assembler/vice.lisp")
(load "src/assembler/segment.lisp")
(load "src/assembler/directives.lisp")
(load "src/assembler/dump.lisp")
(load "src/assembler/write.lisp")
(load "src/assembler/toplevel.lisp")

(load "src/tap/bin2cbmtap.lisp")
(load "src/tap/tap.lisp")
(load "src/tap/tap2wav.lisp")
(load "src/tap/wav2tap.lisp")

(load "src/cbm/ascii2petscii.lisp")
(load "src/cbm/exomize.lisp")

(load "src/cbm-basic/keywords.lisp")
(load "src/cbm-basic/start-addresses.lisp")

(load "src/vic-20/cpu-cycles.lisp")
(load "src/vic-20/palettes.lisp")
(load "src/vic-20/minigrafik.lisp")

(load "src/c/gen-c-cycle-map.lisp")
(load "src/c/gen-c-emulator.lisp")
(load "src/c/gen-c-opcode-map.lisp")

(load "src/tests.lisp")

(gen-c-cycle-map "c/cycle-map.c")
(gen-c-emulator "c/6502-instructions.c")
(gen-c-opcode-map "c/opcode-map.c")
(dump-system "bender")
(quit)
