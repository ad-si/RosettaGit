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

The construction of the language is similar to a [Turing Machine](https://en.wikipedia.org/wiki/Turing_Machine).

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

{| class="wikitable"
!align="center"|Character
!align="left" |Meaning
|-
|style="text-align:center"|<tt>></tt>
||increment the pointer (to point to the next cell to the right).
|-
|style="text-align:center"|<tt><</tt>
||decrement the pointer (to point to the next cell to the left).
|-
|style="text-align:center"|<tt>+</tt>
||increment (increase by one) the cell at the pointer.
|-
|style="text-align:center"|<tt>-</tt>
||decrement (decrease by one) the cell at the pointer.
|-
|style="text-align:center"|<tt>[</tt>
||jump forward to the command after the corresponding <tt>]</tt> if the cell at the pointer is zero.
|-
|style="text-align:center"|<tt>]</tt>
||jump back to the command after the corresponding <tt>[</tt> if the cell at the pointer is nonzero.
|-
|style="text-align:center"|<tt>.</tt>
||output the value of the cell at the pointer as a character.
|-
|style="text-align:center"|<tt>,</tt>
||accept one character of input, storing its value in the cell at the pointer.
|}

Alternatively, the <tt>]</tt> command may instead be translated as an unconditional jump '''to''' the corresponding <tt>[</tt> command, or vice versa; programs will behave the same but may run more slowly.

All other symbols, including traditional whitespace characters, are interpreted as comments.

The definition of the <tt>.</tt> and <tt>,</tt> in the above table still has some ambiguities due to the many ways of converting 'numbers' to 'characters'.
Urban Müller's ''smallest compiler'' converted between characters and numbers using the ASCII character set.
The newline character is number ''10'' and a end of file on input is signalled by the cell value being unchanged when the <tt>,</tt> command completes.
The <tt>,</tt> command uses line editing and waits for for the return key to be pressed.

Due to this minimal instruction set, Brainfuck is used as an introduction to compilers and has even been successfully implemented as a microprocessor core and the foundation to an operating system using a slightly extended syntax for output.
BUT due to vehement opposition to the name [http://esolangs.org/wiki/Cupid various] [http://esolangs.org/wiki/Category:Brainfuck_equivalents equivalents] are frequently used.


## See also
* [Rosetta Code:Brainfuck](https://rosettacode.org/wiki/Rosetta_Code:Brainfuck)  ( why some people call it BF :)
* [RCBF](https://rosettacode.org/wiki/RCBF) - BF interpreters as a Rosetta Code task

## Citations
* [Esoteric languages wiki entry](https://rosettacode.org/wiki/eso:Brainfuck)
*[Wikipedia entry on Brainfuck](https://en.wikipedia.org/wiki/Brainfuck)
* [http://dmoz.org/Computers/Programming/Languages/Brainfuck/ DMOZ Brainfuck category]
* [http://www.iwriteiam.nl/Ha_BF.html Brainfuck tutorial]


[Category:Esoteric_Languages](https://rosettacode.org/wiki/Category:Esoteric_Languages)


## Merged content
'''Dbfi''' is a [brainfuck](https://rosettacode.org/wiki/brainfuck) self-interpreter written by [Daniel B. Cristofani](https://rosettacode.org/wiki/Daniel_B._Cristofani) [http://www.hevanet.com/cristofd/dbfi.b] and described in detail in [http://arxiv.org/abs/cs/0311032v1]. This is the shortest known brainfuck self-interpreter and possibly the shortest self-interpreter amongst all imperative languages. Note that a big part of dbfi code is devoted to ASCII decoding, comment handling, and avoiding undefined behaviour. Removing this would reduce the size to nearly half.
 >>>+[[-]>>[-]++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[
 ->>]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<
 ]<]<[[<]>[[>]>>[>>]+[<<]<[<]<+>>-]>[>]+[->>]<<<<[[<<]<[<]+<<[+>+<<-[>-->+<<-[>
 +<[>>+<<-]]]>[<+>-]<]++>>-->[>]>>[>>]]<<[>>+<[[<]<]>[[<<]<[<]+[-<+>>-[<<+>++>-
 [<->[<<+>>-]]]<[>+<-]>]>[>]>]>[>>]>>]<<[>>+>>+>>]<<[->>>>>>>>]<<[>.>>>>>>>]<<[
 >->>>>>]<<[>,>>>]<<[>+>]<<[+<<]<]

Dbfi inspired Clive Gifford to write the fastest Brainfuck self-interpreter (since dbfi is the shortest, but not the fastest) [http://eigenratios.blogspot.com/2006/12/very-fast-brainfck-self-interpreter.html]. Its compressed version is
 >>>>>+[->>++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[->>
 ]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<]>[
 -]+<<[--[[-]>>->+<<<]>>[-<<<<[>+<-]>>>>>>+<]<<]>>[-]<<>>>[<<<+>>>-]<<<]>>>+<<<
 <[<<]>>[[<+>>+<-]+<-[-[-[-[-[-[-[->->>[>>]>>[>>]<+<[<<]<<[<<]<]>[->>[>>]>>[>>]
 <,<[<<]<<[<<]]<]>[->>[>>]>>[>>]<-<[<<]<<[<<]]<]>[->>[>>]>>[>>]<.<[<<]<<[<<]]<]
 >[->>[>>]>>[>>]<<-<<[<<]<<[<<]]<]>[->>[>>]>>[>>]+[<<]<<[<<]]<]>[->>[>>]>>[>>]<
 [>+>>+<<<-]>[<+>-]>>[<<+>>[-]]+<<[>>-<<-]>>[<<+>>>>+<<-]>>[<<+>>-]<<[>>+<<-]+>
 >[<<->>-]<<<<[-<<[<<]<<[<<]<<<<<++>>]>>[-<<<<<<[<<]<<[<<]<]>]<]>[->>[>>]>>[>>]
 <[>+>>+<<<-]>[[<+>-]>>[-]+<<]>>[<<+>>>>+<<-]>>[<<+>>-]<<[>>+<<-]+>>[<<->>-]<<<
 <[-<<[<<]<<[<<]<<<<<+>>]>>[-<<<<<<[<<]<<[<<]<]>]>[<+>-]<<<<<<[>>+<<-[->>->>+[>
 >>[-<+>>+<]+<-[-[[-]>[-]<]>[-1<<<+>>>]<]>[-<<<->>>]>[-<+>]<<<<[>>+<<-]>>]<<<<<
 <]>>[-<<+[>>>[-<+>>+<]+<-[-[[-]>[-]<]>[-1<<<->>>]<]>[-<<<+>>>]>[-<+>]<<<<[<<+>
 >-]<<]]]>>>>>>>]
Below is relative comparison of time to run a simple program with a stack of two self-interpreters.
{| class="wikitable"
|-
| '''Lower SI''' || '''Upper SI''' || '''Time'''
|-
| cgbfi || cgbfi || 9.1
|-
| cgbfi || dbfi || 7.0
|-
| dbfi || cgbfi || 35.0
|-
| dbfi || dbfi || 21.3
|-
|}
Here cgbfi stands for Clive Gifford Brainfuck Interpreter. Time is given in seconds, which does not matter, because only relative values are important.

The second line shows faster result because DBFI is shorter than CGBSI, and this outweighs the speed benefit of the faster self-interpreter used in the second level in the stack of interpreters. The short program used for the comparison is
 >+>+>+>+>++<[>[<+++>-]<<]> [>+>+<<-]>[-]>.!
and the interpreter [http://mazonka.com/brainf/bff4.c bff4.c].

Content is available under [http://creativecommons.org/publicdomain/zero/1.0/ CC0 public domain dedication] via [http://esolangs.org/wiki/Dbfi the esolang wiki].
