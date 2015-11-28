; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(def-instruction instruction-write-operand (inst out)
  (dotimes (i (instruction-operand-size inst))
    (write-byte (mod operand 256) out)
    (= operand (>> operand 8))))

(def-instruction instruction-write (instruction out)
  (write-byte (instruction-opcode instruction) out)
  (instruction-write-operand instruction out))
