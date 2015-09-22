# Bender

## Overview

This is a development suite for the MOS Technology 6502 CPU,
written in the Lisp dialect tré.  Bender can:

* assemble programs
* disassemble binaries (very incomplete)
* generate TAP and WAV files

For tré see https://github.com/SvenMichaelKlose/tre/


## Why?

Lisp is a very powerful programming language for which no 6502-CPU
development framework exists as far as I know of.  I hope to utilize
the power of Lisp to ease manual work like layouting memory, cycle
counting, reusing code for multiple special cases and automatic
optimizations.


## Installation

Run 'make.sh'.
