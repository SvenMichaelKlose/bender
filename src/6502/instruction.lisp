; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defstruct instruction
  address
  mnemonic
  addrmode
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

(defun instruction-size (inst)
  (++ (instruction-operand-size inst)))

(def-instruction instruction-branch-address (instruction pc)
  (| (eq addrmode 'branch)
     (error "Cannot determine a destination address for non–branch ~A." mnemonic))
  (alet (car (instruction-operand instruction))
    (+ pc 2 ! (? (< 127 !)
                 -256
                 0))))

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
           addrmode)))))

(defun generate-opcode (mnemonic addrmode)
  (alet (href *instructions* mnemonic)
    (| (href ! addrmode)
       (? (in? addrmode 'abs 'zp)
          (href ! 'branch))
          (error "Incorrect addressing mode ~A for ~A." addrmode mnemonic))))

(def-instruction instruction-opcode (instruction)
  (generate-opcode mnemonic addrmode))
