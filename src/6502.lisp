;;;;; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defconstant *mnemonics*
    '((nil bit jmp jmp      ; CC == 0
       sty ldy cpy cpx)
      (ora and eor adc      ; CC == 1
       sta lda cmp sbc)
      (asl rol lsr ror      ; CC == 2
       stx ldx dec inc)
      (bpl bmi bvc bvs      ; branching
       bcc bcs bne beq)
      (php clc plp sec      ; implied (== #x08 (bit-and opcode #x0f))
       pha cli pla sei
       dey tya tay clv
       iny cld inx sed)
      (txa txs tax tsx      ; implied (== #x8a (bit-and opcode #x8f))
       dex nil nop nil)
      (brk jsr rti rts)))   ; (zero? (bit-and opcode #x9f))

(defconstant *mnemonic-list* (remove-if #'not (apply #'+ *mnemonics*)))

; Addressing modes by CC.
(defconstant *addrmodes*
    '((imm  zp single abs branch  zpx ill     absx)
      (izpx zp imm    abs izpy    zpx absy    absx)
      (imm  zp accu   abs ill     zpx single  absx)))

; Legal addressing modes by CC. Each CC is mnemonic (AA) by addressing mode (BB).
(defconstant *legal-addrmodes*
    '((; BIT JMP JMP() STY LDY CPY CPX
        (nil nil nil   nil nil  t   t   t ) ; immediate
        (nil  t  nil   nil  t   t   t   t ) ; zero page
        (nil nil nil   nil nil nil nil nil)
        (nil  t   t     t   t   t   t   t ) ; absolute
        (nil nil nil   nil nil nil nil nil)
        (nil nil nil   nil  t   t  nil nil) ; zero page X
        (nil nil nil   nil nil nil nil nil)
        (nil nil nil   nil  t   t  nil nil)); absolute X
      (; ORA AND EOR ADC STA LDA CMP SBC
        ( t   t   t   t   t   t   t   t )   ; indirect X
        ( t   t   t   t   t   t   t   t )   ; zero page
        ( t   t   t   t  nil  t   t   t )   ; immediate
        ( t   t   t   t   t   t   t   t )   ; absolute
        ( t   t   t   t   t   t   t   t )   ; indirect Y
        ( t   t   t   t   t   t   t   t )   ; zero page X
        ( t   t   t   t   t   t   t   t )   ; absolute Y
        ( t   t   t   t   t   t   t   t ))  ; absolute X
      (; ASL ROL LSR ROR STX LDX DEC INC
        (nil nil nil nil nil  t  nil nil)   ; immediate
        ( t   t   t   t   t   t   t   t )   ; zero page
        ( t   t   t   t  nil nil nil nil)   ; accu
        ( t   t   t   t   t   t   t   t )   ; absolute
        (nil nil nil nil nil nil nil nil)
        ( t   t   t   t   t   t   t   t )   ; zero page X (Y for LDX and STX)
        (nil nil nil nil nil nil nil nil)
        ( t   t   t   t   t   t  t   t )))) ; absolute X (Y for LDX and STX)

(defun mnemonic? (x)                                                            
  (some [member ! x :test #'eq] *mnemonics*))