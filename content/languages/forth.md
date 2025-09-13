+++
title = "Forth"
description = ""
date = 2015-05-06T08:44:02Z
aliases = []
[extra]
id = 1712
[taxonomies]
categories = []
tags = []
+++
'''Forth''', a [procedural](https://rosettacode.org/wiki/procedural_programming), [stack](https://rosettacode.org/wiki/stack)-oriented and [reflective programming](https://rosettacode.org/wiki/reflective_programming) language without type checking, Forth features both interactive execution of commands (making it suitable as a shell for systems that lack a more formal [operating system](https://rosettacode.org/wiki/:Category:Operating_Systems)) and the ability to compile sequences of commands for later execution. Some Forth versions (especially early ones) compile threaded code, but many implementations today generate optimized machine code like other language compilers.

Where not otherwise specified, examples conform to the 1994 [ANSI](https://rosettacode.org/wiki/ANSI) Standard, also known as '''ANS Forth'''. Most Forth implementations now conform to this standard, often with system-specific extensions and convenience libraries. Some examples use words that are not in the standard, but which have become accepted as [common practice](https://rosettacode.org/wiki/Forth_common_practice) since 1994. Standard words should be uppercase, but most Forth systems are case-insensitive.

## Citations
* [Wikipedia:Forth (programming language)](https://en.wikipedia.org/wiki/Forth_%28programming_language%29)
* [http://lars.nocrew.org/dpans/dpansf.htm Index to the ANS Forth words]


## Merged content



A Brainfuck compiler written by [Ian Osgood](https://rosettacode.org/wiki/User:IanOsgood) in [Forth](https://rosettacode.org/wiki/Forth).

## Usage
Load the code your preferred Forth environment.  The code will define the words ''':bf''' and ''':bf-file''',
### Compiling Brainfuck code
Let's say you have this sequence that prints a capital A:
 ++++++[>+++++++++++<-]>-.
You can compile it by using the ''':bf''' word:
 :bf printA " ++++++[>+++++++++++<-]>-."
Your Brainfuck code is now compiled in your Forth environment as the word '''printA'''.  If you want to use a different name, replace printA with your preferred name.

Larger BF programs may be loaded from a file instead of parsed from the interpreter:
 :bf-file bottles bottles.bf

### Running the compiled Brainfuck code
Assuming you used the word '''printA''' from the above example, you can execute your code by running the word:
 printA

## Source
Tested in: [GNU Forth](https://rosettacode.org/wiki/GNU_Forth)

```txt

\ brainfuck compiler

1024 constant size
: init  ( -- p *p ) here size erase  here 0 ;
: right ( p *p -- p+1 *p ) over c!  1+  dup c@ ;
: left  ( p *p -- p-1 *p ) over c!  1-  dup c@ ;		\ range check?

: compile-bf-char ( c -- )
  case
  [char] [ of postpone begin
              postpone dup
              postpone while  endof
  [char] ] of postpone repeat endof
  [char] + of postpone 1+     endof
  [char] - of postpone 1-     endof
  [char] > of postpone right  endof
  [char] < of postpone left   endof
  [char] , of postpone drop
              postpone key    endof
  [char] . of postpone dup
              postpone emit   endof
    \ ignore all other characters
  endcase ;

: compile-bf-string ( addr len -- )
  postpone init
  bounds do i c@ compile-bf-char loop
  postpone swap
  postpone c!
  postpone ;
;

: :bf ( name " bfcode" -- )
  :
  char parse    \ get string delimiter
  compile-bf-string ;

: :bf-file ( name file -- )
  :
  bl parse slurp-file
  compile-bf-string ;
```

