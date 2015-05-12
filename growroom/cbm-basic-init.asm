strings_free = 0 ; Some unused zero page word.

; PRINT null–terminated ASCII string.
print_asciiz:
l:  ldy #0
    lda (ptr),y
    beq +n
    jsr $ffd2
    inc ptr
    bne -l
    inc @(++ ptr)
    jmp -l
n:  rts

; Garbage–collect strings.
gc: rts

main:
    ; Save stack pointer for END.
    tsx
    stx end_sp

    lda #<strings
    sta strings_free
    lda #>strings
    sta @(++ strings_free)

first_line:
