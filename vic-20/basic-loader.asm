    org $0fff

load_address:
    $01 $10 @(low basic_end) @(high basic_end) $01 $00 $9e @(princ main nil) 0
basic_end:
    $00 $00
