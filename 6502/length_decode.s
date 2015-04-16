; 6502 operand length decoder
;
; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

operand_lengths:
    1   1   1
    1   1   1
    0   1   0
    2   2   2
    1   1   255
    1   1   1
    255 2   0
    2   2   2

char legal_addrmodes:
    %00000111 %11111111 %00000100
    %01001111 %11111111 %11111111
    %00000000 %11111011 %11110000
    %01111111 %11111111 %11111111
    %00000000 %11111111 %00000000
    %00001100 %11111111 %11111111
    %00000000 %11111111 %00000000
    %00001100 %11111111 %11111111

length_decode:
    cmp #$20                ; JSR?
    bne no_jsr
    lda #2
    rts
no_jsr:
    cmp #$6c                ; JMP(indirect)?
    bne no_jmp_indirect
    lda #2
    rts
no_jmp_indirect
    tay
    and #$9f                ; Implied?
    bne not_implied
    rts
not_implied:
    tya
    and #%11100011          ; Implied?
    beq no_operand
    tya
    jsr get_operand_length  ; Illegal BB?
    bpl no_illegal_bb
    tya
    and #%00011111
    tax
    tya
    pha
    lsr
    lsr
    lsr
    lsr
    lsr
    tay
    lda legal_addrmodes,x
l:  dey
    bmi +d
    asl
    jmp -l
d:  asl
    pla
    bcc no_illegal_bb
no_operand:
    lda #0
    rts
no_illegal_bb:
    jsr get_operand_length:
    bmi no_operand
    rts

get_operand_length:
    and #%00011111
    tax
    lda operand_lengths,x
    rts
