; Bender – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

via_port_B              = $9110
via_port_A              = $9111
via_data_direction_A    = $9112
via_data_direction_B    = $9113
via_T1_lo               = $9114
via_T1_hi               = $9115
via_T1_lo_latch         = $9116
via_T1_hi_latch         = $9117
via_T2_lo               = $9118
via_T2_hi               = $9119
via_shift               = $911a
via_auxiliary_ctrl      = $911b
    via_T1_PB7_enable        = %10000000
    via_T1_free_running      = %01000000
    via_T2_PB6_count         = %00100000
    via_shift_disabled       = %00000000
    via_shift_in_T2          = %00000100
    via_shift_in_sysclk      = %00001000
    via_shift_in_extpin      = %00001100
    via_shift_out_T2         = %00010000
    via_shift_out_T2CB2      = %00010100
    via_shift_out_sysclk     = %00011000
    via_shift_out_extpin     = %00011100
    ; Shift register is missing.
    via_port_B_latch_enable  = %00000010
    via_port_A_latch_enable  = %00000001

via_peripheral_ctrl     = $911c
    ; CA2/CB2 modes
    via_interrupt_input             = 0
    via_independent_interrupt_input = 1
    via_input                       = 2
    via_independent_input           = 3
    via_handshake_output            = 4
    via_pulse_output                = 5
    via_manual_low                  = 6
    via_manual_high                 = 7

    via_CA1_ctrl  = 1
    via_CA2_ctrl  = 2
    via_CB1_ctrl  = 16
    via_CB2_ctrl  = 32

via_interrupt_flags     = $911d
    via_irq_status  = %10000000
    via_T1_timeout  = %01000000
    via_T2_timeout  = %00100000
    via_CB1_int     = %00010000
    via_CB2_int     = %00001000
    via_shift_int   = %00000100
    via_CA1_int     = %00000010
    via_CB1_int     = %00000001

via_interrupt_enable    = $911e
    ; Bits 0-6 have the same layout as via_interrupt_flags.
    via_int_enable_ctrl = %10000000

via_port_A_no_handshaking = $911f
