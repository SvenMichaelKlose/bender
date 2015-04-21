; Bender – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

screensize = @(* 22 23)

screen      = $1e00
colors      = $9600
colors_low  = $9400

vic_screen_1000      = %11000000
vic_screen_1200      = %11000000
vic_screen_1400      = %11010000
vic_screen_1600      = %11010000
vic_screen_1800      = %11100000
vic_screen_1a00      = %11100000
vic_screen_1c00      = %11110000
vic_screen_1e00      = %11110000

charset_upcase          = $8000
charset_upcase_reversed = $8400
charset_locase          = $8800
charset_locase_reversed = $8c00

vic_charset_upcase   = %0000
vic_charset_8000     = %0000
vic_charset_upcase_reversed = %0001
vic_charset_8400     = %0001
vic_charset_locase   = %0010
vic_charset_8800     = %0010
vic_charset_locase_reversed = %0011
vic_charset_8c00     = %0011
vic_charset_1000     = %1100
vic_charset_1400     = %1101
vic_charset_1800     = %1110
vic_charset_1c00_up  = %1111
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

; Character/border colors.
black        = 0
white        = 1
red          = 2
cyan         = 3
purple       = 4
green        = 5
blue         = 6
yellow       = 7

; Additional screen and auxiliary colors.
orange       = 8
light_orange = 9
pink         = 10
light_cyan   = 11
light_purple = 12
light_green  = 13
light_blue   = 14
light_yellow = 15

; Sound channel clocks
; f = clk / (127 - x)
; x = 127 - (clk / f)
clk_bass_pal    = 4329
clk_alto_pal    = 8659
clk_soprano_pal = 17320
clk_noise_pal   = 34640
clk_bass_ntsc    = 3995
clk_alto_ntsc    = 7990
clk_soprano_ntsc = 15980
clk_noise_ntsc   = 31960

joy_fire    = %00100000
joy_up      = %00000100
joy_down    = %00001000
joy_left    = %00010000
