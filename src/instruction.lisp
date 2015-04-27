;;;;; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defstruct instruction
  mnemonic
  addrmode
  opcode
  (operand nil))

(defun opcode-aa (x)
  (bit-and (>> x 5) 7))

(defun opcode-bb (x)
  (bit-and (>> x 2) 7))

(defun opcode-cc (x)
  (bit-and x 3))

(defun addrmode-size (addrmode)
  (?
    (in? addrmode 'accu 'single 'ill)                 0
    (in? addrmode 'branch 'imm 'zp 'zpx 'izpx 'izpy)  1
    (in? addrmode 'indi 'abs 'absx 'absy)             2
    (error "Cannot determine size of addressing mode ~A.~%" addrmode)))

(defun instruction-operand-size (inst)
  (addrmode-size (instruction-addrmode inst)))

(def-instruction instruction-optimize-addrmode (instruction)
  (unless (in? mnemonic 'jmp 'jsr)
    (= (instruction-addrmode instruction)
       (case addrmode
         'zp   'abs
         'zpx  'absx
         'zpy  'absy
         addrmode))
    (= addrmode (instruction-addrmode instruction))
    (when (& operand (< operand 256))
      (= (instruction-addrmode instruction)
         (case addrmode
           'abs   'zp
           'absx  'zpx
           'absy  (?
                    (in? mnemonic 'lda 'sta 'sbc)  'absy
                    (in? mnemonic 'ldx 'stx)       'zpx
                    addrmode)
           addrmode))
      (= (instruction-opcode instruction) (generate-opcode mnemonic (instruction-addrmode instruction))))))
