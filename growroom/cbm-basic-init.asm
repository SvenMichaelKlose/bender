; bender – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

;; ZERO PAGE

code = @*pc*

    org 0
    data
accu:           0 0 ; 16-bit accu
s:              0 0 ; source pointer
d:              0 0 ; destination pointer
p:              0 0 ; temporary pointer
strings_end:    0 0 ; pointer to free string space
strings_left:   0 0 ; number of string bytes left
end_sp:         0   ; BASIC stack pointer

;;
memory_end = $1e00
string_vars = $1c00
strings = $1d00
    end

    org code

;; ZERO PAGE POINTER MANIPULATION

inc_zp:
    inc 0,x
    beq +r
    inc 1,x
r:  rts

sub_zp:
    sec
    sbc 0,x
    sta 0,x
    bcc +r
    dec 1,x
r:  rts

add_zp:
    clc
    adc 0,x
    sta 0,x
    bcc +r
    inc 1,x
r:  rts


;; STRINGS

; Garbage–collect strings.
gc: ldx #128
    jsr mark_strings

    ; Initialize pointers to pool start.
    lda #<strings
    sta s
    sta strings_end
    lda #>strings
    sta @(++ s)
    sta @(++ strings_end)

    ; Get mark.
    ldy #0
l:  lda (s),y
    beq +done   ; End of pool…
    bpl +u      ; Unreferenced string…

    ; Correct pointers to string.
    lda #<string_vars
    sta p
    lda #>string_vars
    sta @(++ p)
v:  lda (p),y
    beq +c
    cmp s
    bne +n
    iny
    lda (p),y
    dey
    cmp @(++ s)
    bne +n
    lda @(++ strings_end)
    sta (p),y
    lda strings_end
    iny
    sta (p),y
    dey
    jmp -v
n:  ldx #p
    jsr inc_zp
    jmp -v

    ; Copy string to new position.
c:  lda (s),y
    sta (strings_end),y
    beq +ok
    ldx #s
    jsr inc_zp
    ldx #d
    jsr inc_zp
    jmp -c
n:

    ; Jump over string.
u:  ldx #s
l:  jsr inc_zp
    lda (s),y
    bne -l

    ; Move pointer past terminating 0.
ok: ldx #s
    jsr inc_zp
    jmp -l

done:
    lda #<memory_end
    sec
    sbc strings_end
    sta strings_left
    lda #>memory_end
    sbc @(++ strings_end)
    sta @(++ strings_left)

    ; Mark end of pool.
    lda #0
    tax
    iny
    sta (strings_end),y

    ; Mark strings
mark_strings:
    lda #<string_vars
    sta p
    lda #>string_vars
    sta @(++ p)
l:  lda (p),y
    beq +r
    sta accu
    iny
    lda (p),y
    dey
    sta @(++ accu)
    txa
    sta (accu),y
    ldx #p
    lda #2
    jsr add_zp
    tax
    jmp -l

assign_string:
    ; Check if there's enough memory.
    lda @(++ strings_left)
    bne +n
    lda (s),y
    cmp strings_left
    beq +n
    jsr gc
    jmp assign_string
n:
    ; Subtract from free space counter.
    ldx #strings_left
    jsr sub_zp

    ; Step to first character.
    ldx #s
    jsr inc_zp

    ; Save string address to variable.
    lda strings_end
    sta d
    lda @(++ strings_end)
    sta @(++ d)

    ; Save GC marker.
    lda #0
    sta (strings_end),y

    ; Copy string to pool.
l:  lda (s),y
    sta (strings_end),y
    iny
    bne -l

    ; Mark end of pool.
    sta (strings_end),y

    ; Step to rest of free space and return.
    tya
    ldx #strings_end
    jmp add_zp


;; INPUT/OUTPUT

; PRINT null–terminated ASCII string.
print_asciiz:
l:  ldy #0
    lda (p),y
    beq +r
    jsr $ffd2
    ldx #p
    jsr inc_zp
    jmp -l
r:  rts


;; PROGRAM START

main:
    ; Save stack pointer for END.
    tsx
    stx end_sp

    ; Initialize string pool.
    lda #<strings
    sta strings_end
    lda #>strings
    sta @(++ strings_end)
    lda #0
    tay
    sta (strings_end),y

first_line:
