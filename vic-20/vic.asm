screensize = @(* 22 23)
charset_upcase          = $8000
charset_upcase_reversed = $8400
charset_locase          = $8800
charset_locase_reversed = $8c00

;#ifdef M16K
;screen  = $1000
;#else
screen  = $1e00
;#endif

colors  = $9600

vic_screen_1000      = %11000000
vic_screen_1200      = %11000000
vic_screen_1400      = %11010000
vic_screen_1600      = %11010000
vic_screen_1800      = %11100000
vic_screen_1a00      = %11100000
vic_screen_1c00      = %11110000
vic_screen_1e00      = %11110000

vic_charset_upnormal = %0000
vic_charset_uprev    = %0001
vic_charset_lonormal = %0010
vic_charset_lorev    = %0011
vic_charset_1000     = %1100
vic_charset_1400     = %1101
vic_charset_1800     = %1110
vic_charset_1c00     = %1111

vicreg_interlace_horigin        = $9000
vicreg_vorigin                  = $9001
vicreg_screenlo_columns         = $9002
vicreg_rasterlo_rows_charsize   = $9003
vicreg_rasterhi                 = $9004
vicreg_screenhi_charset         = $9005
vicreg_hpen                     = $9006
vicreg_vpen                     = $9007
vicreg_paddle1                  = $9008
vicreg_paddle2                  = $9009
vicreg_bass                     = $900a
vicreg_alto                     = $900b
vicreg_soprano                  = $900c
vicreg_noise                    = $900d
vicreg_auxcol_volume            = $900e
vicreg_screencol_reverse_border = $900f

multicolor = 8
black   = 0
white   = 1
red     = 2
cyan    = 3
purple  = 4
green   = 5
blue    = 6
yellow  = 7
orange  = 8
lorange = 9
pink    = 10
lcyan   = 11
lpurple = 12
lgreen  = 13
lblue   = 14
lyellow = 15

joy_fire    = %00100000
joy_up      = %00000100
joy_down    = %00001000
joy_left    = %00010000
