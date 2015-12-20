; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defun opcode-mnemonic-aa-cc (aa cc)
  (elt (elt *mnemonics* cc) aa))

(defun opcode-mnemonic (opcode)
  (opcode-mnemonic-aa-cc (opcode-aa opcode) (opcode-cc opcode)))

(defun branch-mnemonic (opcode)
  (opcode-mnemonic-aa-cc (opcode-aa opcode) 3))

(defun single-mnemonic (opcode)
  (| (?
       (== #x08 (bit-and opcode #x0f))  (opcode-mnemonic-aa-cc (>> opcode 4) 4)
       (== #x8a (bit-and opcode #x8f))  (opcode-mnemonic-aa-cc (bit-and (>> opcode 4) 7) 5))
     'ill))

(defun addrmode (opcode)
  (elt (elt *addrmodes* (opcode-cc opcode)) (opcode-bb opcode)))

(defun legal-addrmode? (opcode)
  (& (not (eq 'ill (addrmode opcode)))
     (elt (elt (elt *legal-addrmodes* (opcode-cc opcode)) (opcode-bb opcode)) (opcode-aa opcode))))

(defun opcode-branch? (opcode)
  (eq 'branch (addrmode opcode)))

(defun zero-aacc? (opcode)
  (& (zero? (opcode-aa opcode))
     (zero? (opcode-cc opcode))))

(defun zero-9f? (opcode)
  (zero? (bit-and opcode #x9f)))

(defun opcode-mnemonic-addrmode (opcode)
  (?
    (== #x20 opcode)        (values 'jsr 'abs)
    (== #x6c opcode)        (values 'jmp 'indi)
    (zero-9f? opcode)       (values (opcode-mnemonic-aa-cc (>> opcode 5) 6)
                                    nil)
    (opcode-branch? opcode) (values (branch-mnemonic opcode)
                                    (addrmode opcode))
    (| (zero-aacc? opcode)
       (not (legal-addrmode? opcode)))
                            (values (single-mnemonic opcode) nil)
    (values (opcode-mnemonic opcode)
            (addrmode opcode))))

(defun opcode-instruction (opcode)
  (with ((mnem m) (opcode-mnemonic-addrmode opcode))
    (make-instruction :mnemonic  mnem
                      :addrmode  (?
                                   (not m)            'accu
                                   (& (in? mnem 'ldx 'stx)
                                      (eq 'absx m))   'absy
                                   m))))

(defun stream-instruction (in)
  (awhen (read-byte in)
    (aprog1 (opcode-instruction !)
      (dotimes (i (instruction-operand-size !))
        (push (read-byte in) (instruction-operand !))))))
