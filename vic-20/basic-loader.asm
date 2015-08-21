    org @(- (basic-start-address *model*) 2)

load_address:
    @(low (basic-start-address *model*))
    @(high (basic-start-address *model*))
    @(low basic_end)
    @(high basic_end)
    $01 $00 $9e @(princ main nil) 0
basic_end:
    $00 $00
