# Overview

This is Bender, a development suite for the MOS Technology
6502 CPU, written in the Lisp dialect tré.  Bender can:

* assemble programs with inlined Lisp code
* disassemble binaries (very incomplete)
* generate VICE monitor command files
* generate TAP and WAV files
* generate C code for 6502 emulation and disassembly

For tré see https://github.com/SvenMichaelKlose/tre/


## Programs built with Bender

https://github.com/SvenMichaelKlose/arukanoido/

https://github.com/SvenMichaelKlose/nipkow/

https://github.com/SvenMichaelKlose/pulse/

https://github.com/SvenMichaelKlose/shadowVIC/


# Installation

Run 'make.sh' to generate a tré SBCL image called 'bender'.


# Syntax

The assembler is line-oriented.  A line may contain a label
and an instruction or a directive.  Also literal values are
possible.


## Instructions

Instructions are notated as usual.  The A register must not
be used as an operand.  Comments start with a semicolon.

```
    lda #0
    ror a   ; Syntax error. 'a' is implied.
    ror     ; The right way to notate it.
```

## Literals


Bytes, words and strings do not need red tape.

```
    1       ; A decimal byte.
    $1      ; A hexadecimal byte.
    $01     ; A hexadecimal byte as well.
    %101    ; A binary byte.
    "This is an ASCIIZ string." 0
```


## Labels

Label definitions must end with a colon.

```
    label:
```


## Local labels

Labels may be defined more than once.  But that will give
you errors unless you tell Bender the direction in which
to look for:

```
l1: lda f0
    bne n1
    jsr s0
n1: lda f1
    bne n1      ; ERROR: Bender cannot tell which "n1" you desire.
    jsr s1
n1: jsr s2
    jmp l1      ; ERROR: Bender cannot tell which "l1" you desire.
l1:
```

A leading '+' makes Bender search for later labels,
a leading '-' makes it search for earlier code:

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

w:  jmp -w      ; Wait…
```


## Applying Lisp expressions

Lisp expressions can be inserted starting with the '@'
character.  They replace the more familiar inline
arithmetic expressions other assemblers use and may span
multiple lines.

```
    ; NOT used in Bender;
    ldx #table_end-tablestart

    ; Instead use a Lisp expression:
    ldx #@(- table_end table_start)
```

Expressions can be used at toplevel as well.

```
; Make a null–terminated PETSCIIZ string at compile-time.
text: @(ascii2petscii "Hello world!") 0
```

Toplevel expressions are expected to return

* numbers or lists of numbers,
* strings,
* lists of parsed expressions (also see ASM epressions) or
* NIL.


### ASM expressions

Function ASM generates parsed expressions for you.  They
are explained in the next session.  It expects one or more
assembly source strings.

```
@(progn
   (defmacro inc16 (x)
     `(asm ,(format nil "
                      inc ~A
                      bne +n
                      inc @(++ ~A)
                    n:"
                    x x))
      nil) ; No parsed expression returned!

some_code:
    @(inc16 "ptr")
```

Admittedly, this looks ugly but it also is a good basis for
Lispy extensions, and thus, work in progress.


### Parsed expressions

A parsed expression can be a number, a string or an
expression of the following forms:

```
(LABEL . name)
(IDENTIFIER . name)
(DIRECTIVE name parameters)
(INSTRUCTION mnemonic addressing-mode operand)
(EXPRESSION . Lisp expression)
```

IDENTIFIER gets the value of a label.  An operand can be
a number, IDENTIFIER or EXPRESSION.


## Directives

The assembler could be controlled with Lisp expressions but
Bender also comes with a couple of directives for additional
comfort.

### ORG <addr>

Set the program counter to the specified address.

```
    org $1001
```

Would do the same like:
```
@(& (= *pc* #x1001) nil)
```

### FILL <num_bytes>

Fills in num_bytes zeroes.
```
    fill 256    ; Fill page with zeroes.
```

With Lisp expressions this would be:

```
@(maptimes [identity 0] 256)
```

### IF <Lisp boolean>

Assembles the following lines unless its arguments is NIL
until an 'end' directive is reached.  The lines are always
being parsed.  'if' can be nested.


```
if @*with-feature-x?*
    …some code…
end if
```

You must be aware that zero is not NIL, so this example
might not do what you would expect:

```
if 0
    jmp oh_no   ; Still assembled!
end if
```

If you want to disable code, do it like this:

```
if @nil
    jmp oh_no   ; Not assembled.
end if
```

### DATA

Causes the assembler to mute output until an "end" directive
is found.  Great for layouting zero pages for example:

```
    ; Some example zero page.
    org 0
    data
s:      0 0
d:      0 0
scr:    0
count:  0
    end data
```

### BLOCK

This directive collects code to put it into SEGMENTs in the
last pass.  See also function SEGMENT.

```
block
    "This will go into any SEGMENT later."
end

block
    "This as well."
end
```

### END

Ends an "if", "data" or "block" directive.  The name of the
directive that should be ended should be passed as an argument.


```
    ; The four possible forms of `end`.
    end if
    end data
    end block
    end ; To be deprecated, soon.
```

## Assembly source lisp functions

### Function (SEGMENT &KEY size (may-be-shorter? NIL))

This is a function called from assembly sources.  It fills
the specified number of bytes with recently unassigned
BLOCKs, starting with the biggest BLOCK that fits in.

```
    @(segment :size #x318)           ; Before NMI vector.
    0 0                              ; NMI vector.
    @(segment :size (- #x400 $31a))  ; After NMI vector.
```

If the optional last argument is not NIL, the segment is not
filled up with zeroes to match its desired size.

```
    ; e.g. last segment in code:
    @(segment :size #xffff :may-be-shorter? t)  ; Eat up all unassigned BLOCKs.
```


# Assembling programs

## ASSEMBLE-FILES output-file &REST input-files

Takes a list of input files and generates a binary output file.

The following example generates file 'example.prg' as well as
an assembler dump of all passes to 'example.prg.lst':

```
(assemble-files "example.prg"
                "zeropage.asm"
                "example.asm")
```

## GET-LABELS – Importing/exporting labels across assemblies

GET-LABELS returns an associative list of the labels found
in the last call to ASSEMBLE-FILES.  If assigned to global
variable *IMPORTED-LABELS*, they are used as a fallback in
the next call to ASSEMBLE-FILES:

```
(assemble-files "core.prg" "core.asm")

(with-temporary *imported-labels* (get-labels)
  (assemble-files "loader.prg" "loader.asm")
```

## MAKE-VICE-COMMANDS output-file &OPTIONAL additional-commands

Generate a VICE monitor file and appends optional additional
commands with the data collected from the last call of
ASSEMBLE-FILES.

```
(make-vice-commands "example.txt" "break .stop")
```
