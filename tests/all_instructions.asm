; Bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

l1:bcc l1
bcs l1
beq l1
bmi l1
bne l1
bpl l1
bvc l1
bvs l1

adc #@(- 1 1)
adc $12
adc $12,x
adc $1234
adc $1234,x
adc $1234,y
adc ($12,x)
adc ($12),y

and #0
and $12
and $12,x
and $1234
and $1234,x
and $1234,y
and ($12,x)
and ($12),y

asl
asl $12
asl $12,x
asl $1234
asl $1234,x

brk

bit $12
bit $1234

clc
cld
cli
clv

cmp #0
cmp $12
cmp $12,x
cmp $1234
cmp $1234,x
cmp $1234,y
cmp ($12,x)
cmp ($12),y

cpx #0
cpx $12
cpx $1234

cpy #0
cpy $12
cpy $1234

dec $12
dec $12,x
dec $1234
dec $1234,x

dex
dey

eor #0
eor $12
eor $12,x
eor $1234
eor $1234,x
eor $1234,y
eor ($12,x)
eor ($12),y

inc $12
inc $12,x
inc $1234
inc $1234,x

inx
iny

jmp $1234
jmp ($1234)

jsr $1234

lda #0
lda $12
lda $12,x
lda $1234
lda $1234,x
lda $1234,y
lda ($12,x)
lda ($12),y

ldx #0
ldx $12
ldx $12,y
ldx $1234
ldx $1234,y

ldy #0
ldy $12
ldy $12,x
ldy $1234
ldy $1234,x

lsr
lsr $12
lsr $12,x
lsr $1234
lsr $1234,x

nop

ora #0
ora $12
ora $12,x
ora $1234
ora $1234,x
ora $1234,y
ora ($12,x)
ora ($12),y

pha
php
pla
plp

rol
rol $12
rol $12,x
rol $1234
rol $1234,x

ror
ror $12
ror $12,x
ror $1234
ror $1234,x

rti
rts

sbc #0
sbc $12
sbc $12,x
sbc $1234
sbc $1234,x
sbc $1234,y
sbc ($12,x)
sbc ($12),y

sec
sei

sta $12
sta $12,x
sta $1234
sta $1234,x
sta $1234,y
sta ($12,x)
sta ($12),y

stx $12
stx $12,y
stx $1234

sty $12
sty $12,x
sty $1234

tax
tay
tsx
txa
txs
tya
