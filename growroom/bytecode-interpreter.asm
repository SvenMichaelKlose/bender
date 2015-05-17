; Combined direct and token threading.
; See also: https://en.wikipedia.org/wiki/Threaded_code

interpret:
.(
    ldy #0
    lda (bytecode_ptr),y
    beq done
    inc bytecode_ptr
    asl
    bcs local_fun
    sta jump+1
bytecode_funs:
    lda bytecode_flags
    ldy bytecode_y
    pha
    lda bytecode_a
    plp
jump:
    jsr $1234
    sta bytecode_a
    sty bytecode_y
    php
    pla
    sta bytecode_flags
    jmp interpret
local_fun:
    sta jump2+1
jump2:
    jsr $1234
    jmp interpret
done:
    rts
.)

local_bytecode_funs:
bc_lda:
    iny
    lda (bytecode_ptr),y
    sta bytecode_a
    inc bytecode_ptr
    rts

bc_bcs:
.(
    bcc r
    lda (bytecode_ptr),y
    sta bytecode_ptr
r:  inc bytecode_ptr
    rts
.)
