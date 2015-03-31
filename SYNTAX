# Bender syntax

The assembler is line-oriented.  A line may contain a label and an instruction
or a directive. Also literal values are possible.


## Instructions

Instructions are notated as usual.  The A register must not be used as an
operand.  Comments start with a semicolon.

```
    lda #0
    ror a   ; Syntax error. 'a' is implied.
    ror     ; The right way to notate it.
```

## Literals


Bytes, words and strings can be written without directives. Words must have
enough leading zeroes:

```
    $1      ; A hexadecimal byte.
    $01     ; A hexadecimal byte as well.
    $001    ; A hexadecimal word.
    $0001   ; Also a hexadecimal word.
    %101    ; A binary byte.
    "This is an ASCIIZ string." 0
```


## Labels

Label definitions must end with a colon.

```
    label:
```


## Local labels

Labels may be defined more than once.  But that'll give you
errors…

```
l1: lda f0
    bne n1
    jsr s0
n1: lda f1
    bne n1      ; ERROR: Bender can't tell which "n1" you desire.
    jsr s1
n1: jsr s2
    jmp l1      ; ERROR: Bender can't tell which "l1" you desire.
l1:
```

…until you tell Bender the direction to look for:

```
l1: lda f0
    bne n1
    jsr s0
n1: lda f1
    bne +n1     ; Use next "n1".
    jsr s1
n1: jsr s2
    jmp -l1     ; Use previous "l1".
l1:
```


## Applying Lisp expressions

Lisp expressions can be inserted with the @ character.

```
    ldx #@(- table_end table_start) ; Load X with size of table.
```
