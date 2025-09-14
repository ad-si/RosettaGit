+++
title = "Brainfuck"
description = ""
date = 2014-11-21T14:16:22Z
aliases = []
[extra]
id = 2022
[taxonomies]
categories = []
tags = []
+++

Created by Urban Müller in 1993 in an attempt to create the world's smallest Turing-complete compiler.
It is noted as an [esoteric programming language](https://rosettacode.org/wiki/:Category:Esoteric_Languages),
as it is not ordinarily used for applications development,
but it also noted as being a minimalist language.

The construction of the language is similar to a
[Turing Machine](https://en.wikipedia.org/wiki/Turing_Machine).

As with the Turing Machine, Brainfuck is built from a finite state machine and an infinite tape of cells.
Each cell can be any size, including unbounded, but is frequently an eight bit byte.
The finite state machine is the program code with the program counter pointing at the current state.

The strong similarity is one reason that a Brainfuck equivalent named ''Ρʺ''
was suitable for use by Corrado Böhm in 1964
to prove that structured programming using only ''while loops''
was just a powerful as ''goto spagetti'' programming.

The complete specification for the language
(the available state transitions of the Turing machine)
can be summed up with the following eight symbols:

| Character | Meaning |
|:---------:|---------|
| `>` | increment the pointer (to point to the next cell to the right). |
| `<` | decrement the pointer (to point to the next cell to the left). |
| `+` | increment (increase by one) the cell at the pointer. |
| `-` | decrement (decrease by one) the cell at the pointer. |
| `[` | jump forward to the command after the corresponding `]` if the cell at the pointer is zero. |
| `]` | jump back to the command after the corresponding `[` if the cell at the pointer is nonzero. |
| `.` | output the value of the cell at the pointer as a character. |
| `,` | accept one character of input, storing its value in the cell at the pointer. |

Alternatively, the `]` command may instead be translated as an unconditional jump '''to''' the corresponding `[` command, or vice versa; programs will behave the same but may run more slowly.

All other symbols, including traditional whitespace characters, are interpreted as comments.

The definition of the `.` and `,` in the above table still has some ambiguities due to the many ways of converting 'numbers' to 'characters'.
Urban Müller's ''smallest compiler'' converted between characters and numbers using the ASCII character set.
The newline character is number ''10'' and a end of file on input is signalled by the cell value being unchanged when the `,` command completes.
The `,` command uses line editing and waits for for the return key to be pressed.

Due to this minimal instruction set, Brainfuck is used as an introduction to compilers and has even been successfully implemented as a microprocessor core and the foundation to an operating system using a slightly extended syntax for output.
BUT due to vehement opposition to the name [http://esolangs.org/wiki/Cupid various] [http://esolangs.org/wiki/Category:Brainfuck_equivalents equivalents] are frequently used.


## Links

- [Wikipedia - Brainfuck](https://en.wikipedia.org/wiki/Brainfuck)
- [RosettaCode - Brainfuck](https://rosettacode.org/wiki/Rosetta_Code:Brainfuck)
- [DMOZ Brainfuck category](http://dmoz.org/Computers/Programming/Languages/Brainfuck/)
- [Brainfuck tutorial](http://www.iwriteiam.nl/Ha_BF.html)
