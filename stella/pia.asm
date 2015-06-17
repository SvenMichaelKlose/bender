; http://problemkaputt.de/2k6specs.htm

; Read addresses

CXM0P = $30 ; 11......  read collision M0-P1, M0-P0 (Bit 7,6)
CXM1P = $31 ; 11......  read collision M1-P0, M1-P1
CXP0FB = $32 ; 11......  read collision P0-PF, P0-BL
CXP1FB = $33 ; 11......  read collision P1-PF, P1-BL
CXM0FB = $34 ; 11......  read collision M0-PF, M0-BL
CXM1FB = $35 ; 11......  read collision M1-PF, M1-BL
CXBLPF = $36 ; 1.......  read collision BL-PF, unused
CXPPMM = $37 ; 11......  read collision P0-P1, M0-M1
INPT0 = $38 ; 1.......  read pot port
INPT1 = $39 ; 1.......  read pot port
INPT2 = $3A ; 1.......  read pot port
INPT3 = $3B ; 1.......  read pot port
INPT4 = $3C ; 1.......  read input
INPT5 = $3D ; 1.......  read input

; Switches

SWCHA = $0280 ; 11111111  Port A; input or output  (read or write)
SWACNT = $0281 ; 11111111  Port A DDR, 0= input, 1=output
SWCHB = $0282 ; 11111111  Port B; console switches (read only)
SWBCNT = $0283 ; 11111111  Port B DDR (hardwired as input)

; Timers

INTIM = $0284 ; 11111111  Timer output (read only)
INSTAT = $0285 ; 11......  Timer Status (read only, undocumented)
TIM1T = $0294 ; 11111111  set 1 clock interval (838 nsec/interval)
TIM8T = $0295 ; 11111111  set 8 clock interval (6.7 usec/interval)
TIM64T = $0296 ; 11111111  set 64 clock interval (53.6 usec/interval)
T1024T = $0297 ; 11111111  set 1024 clock interval (858.2 usec/interval)
