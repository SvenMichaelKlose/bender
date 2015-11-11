; Viewer for MINIGRAPHIK files without BASIC header.

mg_screen      = $1000
mg_charset     = $1100
mg_colors      = $9400
mg_bitmap_size = 3840

; Epects MINIGRAFIK-WITHOUT-CODE at pointer 'mg_s'.
; Also define words 'mg_d' and 'mg_c'.
mg_display:
    ; Set up VIC colors.
    ldy #0
    lda (mg_s),y
    inc mg_s
    sta $900e
    lda (mg_s),y
    inc mg_s
    sta $900f

    ; Copy bitmap to charset.
    lda #<mg_charset
    sta mg_d
    lda #>mg_charset
    sta @(++ mg_d)
    lda #@(low (- mg_bitmap_size))
    sta mg_c
    lda #@(high (- mg_bitmap_size))
    sta @(++ mg_c)
    jsr mg_copy

    ; Unpack colors.
    lda #<mg_colors
    sta mg_d
    lda #>mg_colors
    sta @(++ mg_d)
    ldx #120
l:  lda (mg_s),y
    and #$0f
    sta (mg_d),y
    jsr mg_inc_d
    lda (mg_s),y
    lsr
    lsr
    lsr
    lsr
    sta (mg_d),y
    jsr mg_inc_sd
    dex
    bne -l

    ; Make screen columns.
    lda #<mg_screen
    sta mg_d
    lda #>mg_screen
    sta @(++ mg_d)
    ldx #0
l:  ldy #0
    txa
    clc
    adc #$10
m:  sta (mg_d),y
    clc
    adc #12
    iny
    cpy #20
    bne -m
    lda mg_d
    clc
    adc #20
    sta mg_d
    bcc +n
    inc @(++ mg_d)
n:  inx
    cpx #12
    bne -l

    ; Set up VIC.
    ldx #5
l:  clc
    lda $ede4,x
    adc mg_vicregs,x
    sta $9000,x
    dex
    bpl -l

    rts

mg_copy:
l:  lda (mg_s),y
    sta (mg_d),y
    jsr mg_inc_sd
    inc mg_c
    bne -l
    inc @(++ mg_c)
    bne -l
    rts

mg_inc_sd:
    inc mg_s
    bne mg_inc_d
    inc @(++ mg_s)
mg_inc_d:
    inc mg_d
    bne +n
    inc @(++ mg_d)
n:  rts

mg_vicregs:
    $02 $fe $fe $eb $00 $0c
