    org @(- (basic-start-address *model*) 2)

load_address:
    @(low (basic-start-address *model*))
    @(high (basic-start-address *model*))
    @(unless (first-pass?) (low basic_end))
    @(unless (first-pass?) (high basic_end))
    $01 $00 $9e @(unless (first-pass?) (princ main nil)) 0
basic_end:
    $00 $00
