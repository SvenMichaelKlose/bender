; Calculate line address in screen memory.
scraddr:
    ldy scry
    lda $edfd,y         ; Get low line address.
    sta scr
    cpy #12             ; Set carry flag if above line 11.
    lda #@(half (high screen)) ; Take screen page shifted 1 to the right...
    rol                 ; ... and roll in carry flag to add it.
    sta @(++ scr)
    ldy scrx
    rts

; Calculate line address in screen and colour memory.
scrcoladdr:
    ldy scry
    lda $edfd,y
    sta scr
    sta col
    cpy #12
    lda #@(half (high screen))
    rol
    sta @(++ scr)
    and #1
    ora #@(high colors)
    sta @(++ col)
    ldy scrx
    rts
