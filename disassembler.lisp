;;;;; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

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

(defun opcode-instruction (opcode)
  (with (mnem  (opcode-mnemonic opcode)
         m     (addrmode opcode))
    (?
      (== #x20 opcode)
        (= m     'abs
           mnem  'jsr)
      (== #x6c opcode)
        (= m     'indi
           mnem  'jmp)
      (zero? (bit-and opcode #x9f))
        (= m     nil
           mnem  (opcode-mnemonic-aa-cc (>> opcode 5) 6))
      (eq 'branch m)
        (= mnem (branch-mnemonic opcode))
      (| (& (zero? (opcode-aa opcode))
            (zero? (opcode-cc opcode)))
         (not (legal-addrmode? opcode)))
        (= m     nil
           mnem  (single-mnemonic opcode)))
    (make-instruction :opcode    opcode
                      :mnemonic  mnem
                      :addrmode  (?
                                   (not m)            'accu
                                   (& (in? mnem 'ldx 'stx)
                                      (eq 'absx m))   'absy
                                   m))))

(defun stream-instruction (in)
  (unless (end-of-file? in)
    (aprog1 (opcode-instruction (read-char in))
      (dotimes (i (instruction-operand-size !))
        (push (read-char in) (instruction-operand !))))))

(defun disassemble-file (in-name out-name)
  (with-input-file i in-name
    (with-output-file o out-name
      (awhile (stream-instruction i)
              nil
        (print-instruction ! o)))))
