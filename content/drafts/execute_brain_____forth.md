+++
title = "Execute Brain****/Forth"
description = ""
date = 2010-02-06T14:20:38Z
aliases = []
[extra]
id = 2326
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}
A Brainf*** compiler written by [[User:IanOsgood|Ian Osgood]] in [[Forth]].

==Usage==

Load the code your preferred Forth environment.  The code will define the words ''':bf''' and ''':bf-file''',
===Compiling Brainf*** code===
Let's say you have this sequence that prints a capital A:
 ++++++[>+++++++++++<-]>-.
You can compile it by using the ''':bf''' word:
 :bf printA " ++++++[>+++++++++++<-]>-."
Your Brainf*** code is now compiled in your Forth environment as the word '''printA'''.  If you want to use a different name, replace printA with your preferred name.

Larger BF programs may be loaded from a file instead of parsed from the interpreter:
 :bf-file bottles bottles.bf

===Running the compiled Brainf*** code===
Assuming you used the word '''printA''' from the above example, you can execute your code by running the word:
 printA

==Source==
Tested in: [[GNU Forth]]

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

