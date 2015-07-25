; Based on http://problemkaputt.de/2k6specs.htm

; Write addresses

VSYNC = $00     ; ......1.  vertical sync set-clear
VBLANK = $01    ; 11....1.  vertical blank set-clear
WSYNC = $02     ; <strobe>  wait for leading edge of horizontal blank
RSYNC = $03     ; <strobe>  reset horizontal sync counter
NUSIZ0 = $04    ; ..111111  number-size player-missile 0
NUSIZ1 = $05    ; ..111111  number-size player-missile 1
COLUP0 = $06    ; 1111111.  color-lum player 0 and missile 0
COLUP1 = $07    ; 1111111.  color-lum player 1 and missile 1
COLUPF = $08    ; 1111111.  color-lum playfield and ball
COLUBK = $09    ; 1111111.  color-lum background
CTRLPF = $0A    ; ..11.111  control playfield ball size & collisions
REFP0 = $0B     ; ....1...  reflect player 0
REFP1 = $0C     ; ....1...  reflect player 1
PF0 = $0D       ; 1111....  playfield register byte 0
PF1 = $0E       ; 11111111  playfield register byte 1
PF2 = $0F       ; 11111111  playfield register byte 2
RESP0 = $10     ; <strobe>  reset player 0
RESP1 = $11     ; <strobe>  reset player 1
RESM0 = $12     ; <strobe>  reset missile 0
RESM1 = $13     ; <strobe>  reset missile 1
RESBL = $14     ; <strobe>  reset ball
AUDC0 = $15     ; ....1111  audio control 0
AUDC1 = $16     ; ....1111  audio control 1
AUDF0 = $17     ; ...11111  audio frequency 0
AUDF1 = $18     ; ...11111  audio frequency 1
AUDV0 = $19     ; ....1111  audio volume 0
AUDV1 = $1A     ; ....1111  audio volume 1
GRP0 = $1B      ; 11111111  graphics player 0
GRP1 = $1C      ; 11111111  graphics player 1
ENAM0 = $1D     ; ......1.  graphics (enable) missile 0
ENAM1 = $1E     ; ......1.  graphics (enable) missile 1
ENABL = $1F     ; ......1.  graphics (enable) ball
HMP0 = $20      ; 1111....  horizontal motion player 0
HMP1 = $21      ; 1111....  horizontal motion player 1
HMM0 = $22      ; 1111....  horizontal motion missile 0
HMM1 = $23      ; 1111....  horizontal motion missile 1
HMBL = $24      ; 1111....  horizontal motion ball
VDELP0 = $25    ; .......1  vertical delay player 0
VDELP1 = $26    ; .......1  vertical delay player 1
VDELBL = $27    ; .......1  vertical delay ball
RESMP0 = $28    ; ......1.  reset missile 0 to player 0
RESMP1 = $29    ; ......1.  reset missile 1 to player 1
HMOVE = $2A     ; <strobe>  apply horizontal motion
HMCLR = $2B     ; <strobe>  clear horizontal motion registers
CXCLR = $2C     ; <strobe>  clear collision latches

; Read addresses

CXM0P = $30     ; 11......  read collision M0-P1, M0-P0 (Bit 7,6)
CXM1P = $31     ; 11......  read collision M1-P0, M1-P1
CXP0FB = $32    ; 11......  read collision P0-PF, P0-BL
CXP1FB = $33    ; 11......  read collision P1-PF, P1-BL
CXM0FB = $34    ; 11......  read collision M0-PF, M0-BL
CXM1FB = $35    ; 11......  read collision M1-PF, M1-BL
CXBLPF = $36    ; 1.......  read collision BL-PF, unused
CXPPMM = $37    ; 11......  read collision P0-P1, M0-M1
INPT0 = $38     ; 1.......  read pot port
INPT1 = $39     ; 1.......  read pot port
INPT2 = $3A     ; 1.......  read pot port
INPT3 = $3B     ; 1.......  read pot port
INPT4 = $3C     ; 1.......  read input
INPT5 = $3D     ; 1.......  read input
