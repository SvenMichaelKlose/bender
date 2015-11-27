; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(load "environment/platforms/shared/read-binary.lisp")
(load "environment/platforms/shared/xml-entities.lisp")
(load "environment/platforms/shared/xml2lml.lisp")

(load "src/6502/cpu.lisp")
(load "src/6502/instruction.lisp")
(load "src/6502/opcode.lisp")
(load "src/6502/cycles.lisp")
(load "src/6502/instruction-map.lisp")

(load "src/disassembler/print.lisp")
(load "src/disassembler/toplevel.lisp")

(load "src/assembler/labels.lisp")
(load "src/assembler/tokenizer.lisp")
(load "src/assembler/parser.lisp")
(load "src/assembler/labels-to-exprs.lisp")
(load "src/assembler/helpers.lisp")
(load "src/assembler/vice.lisp")
(load "src/assembler/toplevel.lisp")

(load "src/tap/bin2cbmtap.lisp")
(load "src/tap/tap.lisp")
(load "src/tap/tap2wav.lisp")

(load "src/cbm/ascii2petscii.lisp")

(load "src/cbm-basic/keywords.lisp")
(load "src/cbm-basic/start-addresses.lisp")

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
