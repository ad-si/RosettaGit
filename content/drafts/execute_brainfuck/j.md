+++
title = "Execute Brainfuck/J"
description = ""
date = 2015-07-06T11:00:07Z
aliases = []
[extra]
id = 6668
[taxonomies]
categories = []
tags = []
+++


```J
require'general/misc/prompt' NB. was require'misc' in j602

NB. operations
inc=: 1 0 +/@,: ]
dec=: _1 0 +/@,: ]
INC=: ] +/@,: -@>:@ptr {. 1:
DEC=: ] +/@,: -@>:@ptr {. _1:
OUT=:  ][ ptr output@{ extend
ACC=: ] input`ptr`]} extend
LJ=: JMP^:zero
RJ=: JMP^:(0=zero)

NB. utilities
ptr=: 2 + 0 { ]
pc=: 1 { ]
extend=: ] +/@,: 0,ptr {. 0: NB. tape must be long enough for i/o

NB. buffered io
OBUF=:IBUF=:''
flush=:] [ 3 :0
  1!:2&2 OBUF
  OBUF=:''
)
output=:3 :0@]
  OBUF=:OBUF,7 u:4 u:y
)
input=:3 :0@]
  if.0=#IBUF do.
    IBUF=:prompt flush ''
  end.
  3 u: t [ IBUF=:}.IBUF [ t=.{.IBUF
)


NB. program sequencing
JMP=: [`1:`]}
fixL=:3 :0"0
  {. y&LJ`''
)
fixR=:3 :0"0
  {. y&RJ`''
)
zero=: (0 = ptr { ]) ::1:
left=: I.@:=&'['
nesting=: [: (~. </ ]) 0,1 _1 0 +/\@:{~ '[]' i. ]
right=: [:/:~/ [:;"1 (#:1 2) <@I.@E."1/ nesting
next=: (1 + 1 { ])`1:`]}

NB. interpreter
OPCODES=: '><+-.,[]'
OPS=: inc`dec`INC`DEC`OUT`ACC`FIXME`FIXME`NOP
compile=:3 :0
  src=. ([-.-.)&OPCODES y
  obj=. OPS {~ OPCODES i. src
  L=. left src
  R=. right src
  (fixR L) R} (fixL R) L} obj
)

evoke=:4 :'x`:6 y'
step=: [ next ] evoke~ pc { [

run=: [: flush step^:(pc < #@[)^:_

execute=: 2 }. compile@] run 0 0,0: :[
```


Here is hello world:


```J
hello=: 0 :0
+++++ +++++             initialize counter (cell #0) to 10
[                       use loop to set the next four cells to 70/100/30/10
    > +++++ ++              add  7 to cell #1
    > +++++ +++++           add 10 to cell #2
    > +++                   add  3 to cell #3
    > +                     add  1 to cell #4
    <<<< -                  decrement counter (cell #0)
]
> ++ .                  print 'H'
> + .                   print 'e'
+++++ ++ .              print 'l'
.                       print 'l'
+++ .                   print 'o'
>++ .                   print ' '
<< +++++ +++++ +++++ .  print 'W'
> .                     print 'o'
+++ .                   print 'r'
----- - .               print 'l'
----- --- .             print 'd'
> + .                   print '!'
> .                     print '\n'
)
```


Example use:

```J
   execute hello
Hello World!

0 87 100 33 10
```


execute compiles the program, generates an initial state of a blank tape with the data and instruction pointers both being zero, runs the program and then returns as its result the final state of the tape.

The first two numbers in that result are the data and code pointers.  The following numbers represent the tape itself.  In the above example, 0 is the data pointer, and the data cell selected by this data pointer has the value 100.

Or, here is the addDigit program from wikipedia, with explicit compilation and run as separate operations:


```J
   (compile ',>++++++[<-------->-],[<+>-]<.') run 0 0

23
5
0 30 53 0
```


Here, 2 and 3 were provided as input, and 5 was displayed as the result.

The buffered I/O implementation here prompts the user for a line of code whenever a character is needed and none has been provided.  The prompt will be a blank line.  Output is flushed before prompting the user and when completing a program.  If a program terminates prematurely (because of an interrupt or a bug), unflushed output will appear in the variable OBUF.

Note this implementation encodes branch targets into the compiled code, and that <code>compile</code> assumes that <code>[</code> and <code>]</code> are balanced.  Unbalanced brackets are not supported (and will cause the first instruction of the program to be replaced with an unspecified branch instruction).

You can also use <code>trace=: [: flush step^:(pc < #@[)^:a:</code>, instead of <code>run</code>.  <code>trace</code> takes the same arguments as run, but will return every state the tape went through, instead of the final state.  This is mostly interested for understanding the behavior of small programs.

When investigating a bug, you can use:


```J
next=: (1 + 1 { ])`1:`]} ([ smoutput)
```


This means that programs when run (or traced) will display each tape state after it has been generated, which means that when the program crashes you will have displayed a tape state shortly before the crash.

All operations and utility functions take tape state as their argument.  Operations produce the new tape state as their result.  The <code>next</code>(and <code>traceNext</code>) function advances the code pointer to the next instruction.  The <code>step</code> function expects compiled code for its left argument and tape state for its right argument and executes one instruction, advancing the code pointer to the next instruction.
