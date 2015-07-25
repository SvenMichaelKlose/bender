; Based on http://problemkaputt.de/2k6specs.htm

; Switches

SWCHA = $0280   ; 11111111  Port A; input or output  (read or write)
SWACNT = $0281  ; 11111111  Port A DDR, 0= input, 1=output
SWCHB = $0282   ; 11111111  Port B; console switches (read only)
SWBCNT = $0283  ; 11111111  Port B DDR (hardwired as input)

; Timers

INTIM = $0284   ; 11111111  Timer output (read only)
INSTAT = $0285  ; 11......  Timer Status (read only, undocumented)
TIM1T = $0294   ; 11111111  set 1 clock interval (838 nsec/interval)
TIM8T = $0295   ; 11111111  set 8 clock interval (6.7 usec/interval)
TIM64T = $0296  ; 11111111  set 64 clock interval (53.6 usec/interval)
T1024T = $0297  ; 11111111  set 1024 clock interval (858.2 usec/interval)
