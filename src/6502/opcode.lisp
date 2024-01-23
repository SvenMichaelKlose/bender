; bender – Copyright (c) 2014,2024 Sven Michael Klose <pixel@copei.de>

(fn opcode-mnemonic-aa-cc (aa cc)
  (elt (elt *mnemonics* cc) aa))

(fn opcode-mnemonic (opcode)
  (opcode-mnemonic-aa-cc (opcode-aa opcode) (opcode-cc opcode)))

(fn branch-mnemonic (opcode)
  (opcode-mnemonic-aa-cc (opcode-aa opcode) 3))

(fn single-mnemonic (opcode)
  (| (?
       (== #x08 (bit-and opcode #x0f))
         (opcode-mnemonic-aa-cc (>> opcode 4) 4)
       (== #x8a (bit-and opcode #x8f))
         (opcode-mnemonic-aa-cc (bit-and (>> opcode 4) 7) 5))
     'ill))

(fn addrmode (opcode)
  (elt (elt *addrmodes* (opcode-cc opcode)) (opcode-bb opcode)))

(fn legal-addrmode? (opcode)
  (& (not (eq 'ill (addrmode opcode)))
     (elt (elt (elt *legal-addrmodes* (opcode-cc opcode))
               (opcode-bb opcode))
          (opcode-aa opcode))))

(fn opcode-branch? (opcode)
  (eq 'branch (addrmode opcode)))

(fn zero-aacc? (opcode)
  (== 0 (opcode-aa opcode) (opcode-cc opcode)))

(fn zero-9f? (opcode)
  (== 0 (bit-and opcode #x9f)))

(fn opcode-mnemonic-addrmode (opcode)
  (?
    (== #x20 opcode)
      (values 'jsr 'abs)
    (== #x6c opcode)
      (values 'jmp 'indi)
    (zero-9f? opcode)
      (values (opcode-mnemonic-aa-cc (>> opcode 5) 6)
              nil)
    (opcode-branch? opcode)
      (values (branch-mnemonic opcode)
              (addrmode opcode))
    (| (zero-aacc? opcode)
       (not (legal-addrmode? opcode)))
      (values (single-mnemonic opcode) nil)
    (values (opcode-mnemonic opcode)
            (addrmode opcode))))

(fn opcode-instruction (opcode)
  (with ((mnem m) (opcode-mnemonic-addrmode opcode))
    (make-instruction :mnemonic  mnem
                      :addrmode  (?
                                   (not m)            'accu
                                   (& (in? mnem 'ldx 'stx)
                                      (eq 'absx m))   'absy
                                   m))))

(fn stream-instruction (in)
  (awhen (read-byte in)
    (aprog1 (opcode-instruction !)
      (dotimes (i (instruction-operand-size !))
        (push (read-byte in) (instruction-operand !))))))
