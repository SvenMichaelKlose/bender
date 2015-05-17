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


Bytes, words and strings don't need red tape . Words must have
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

Lisp expressions can be inserted with the @ character.  Line breaks
are not allowed inside them.

```
    ldx #@(- table_end table_start) ; Load X with size of table.
```

Expressions can be used at toplevel as well.

```
; Make a PETSCIIZ string at compile-time.
text: @(@ #'ascii2petscii (string-list "Hello world!"))
      0
```

Toplevel expressions are expected to return numbers, strings or a
lists of parsed expressions.  A parsed expression can be a number,
a string or an expression of the following form:

```
(LABEL . name)
(IDENTIFIER . name)
(DIRECTIVE name parameters)
(INSTRUCTION mnemonic addressing-mode operand)
(EXPRESSION . tré-expression)
```

IDENTIFIER gets the value of a label.  An operand can be a number,
IDENTIFIER or EXPRESSION.


## Directives

The assembler could be controlled with Lisp expressions but bender
also comes with a couple of directives for additional comfort.

### org <addr>

Set the program counter to the specified address. Would do the
same like:
```
@(& (= *pc* <addr>) nil)
```

### fill <num_bytes>

Fills in num_bytes zeroes.  With Lisp expressions it would be:
```
@(maptimes [0] <num_bytes>)
```

### if <Lisp boolean>

Assembles the following lines if the argument is not NIL.
0 is not NIL.  if it is NIL all following lines up to an "end"
directive are ignored.

### data

Causes the assemble to mute output until an "end" directive is
found.  Great for layouting zero pages for example.

### end

Ends an "if" or "data" directive.
