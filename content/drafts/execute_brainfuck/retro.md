+++
title = "Execute Brainfuck/Retro"
description = ""
date = 2011-06-16T04:19:24Z
aliases = []
[extra]
id = 9925
[taxonomies]
categories = []
tags = []
+++

Executing Brainfuck in Retro is currently a two part task. First, there is a compiler, and secondly you need to run the compiled code.


```Retro
( Ngaro Assembler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ )
( Copyright [c] 2008 - 2011, Charles Childers                                 )
( Copyright [c] 2009 - 2010, Luke Parrish                                     )
( Copyright [c] 2010,        Marc Simpson                                     )
( Copyright [c] 2010,        Jay Skeer                                        )
( ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ )

8000 constant MAX-APP-SIZE

( Assembler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ )
3 elements target origin fid
: pad ( - ) @origin 32 + !target ;
: m,  ( n-  ) @target !+ !target ;
: vm: ( n"- ) ` : .data ` m, ` ; ;
   0 vm: nop,          1 vm: lit,          2 vm: dup,
   3 vm: drop,         4 vm: swap,         5 vm: push,
   6 vm: pop,          7 vm: loop,         8 vm: jump,
   9 vm: ret,         10 vm: >jump,       11 vm: <jump,
  12 vm: !jump,       13 vm: =jump,       14 vm: @,
  15 vm: !,           16 vm: +,           17 vm: -,
  18 vm: *,           19 vm: /mod,        20 vm: and,
  21 vm: or,          22 vm: xor,         23 vm: <<,
  24 vm: >>,          25 vm: 0;           26 vm: 1+,
  27 vm: 1-,          28 vm: in,          29 vm: out,
  30 vm: wait,

: t-here      (  -n ) @target @origin - ;

{{
  : writeByte ( n- )
    @fid ^files'write drop ;

  : applyMask ( n- )
    %00000000000000000000000011111111 and ;

  : writeCell ( n- )
         dup applyMask writeByte
    8 >> dup applyMask writeByte
    8 >> dup applyMask writeByte
    8 >>     applyMask writeByte ;
---reveal---
  : saveImage (  - )
    "appImage" ^files':W ^files'open !fid
    @origin t-here [ @+ writeCell ] times drop
    @fid ^files'close drop bye ;
}}

: endApp      (  -  )
  t-here "\nApp ends @ %d\n" puts
  MAX-APP-SIZE t-here - "%d cells free" puts
  depth 1 >= [ "\nError in stack depth!: " puts .s ] ifTrue ;
: :main       (  -  ) t-here [ "\nMAIN @ %d" puts ] [ @origin 1+ ! ] bi ;
: #           ( n-  ) lit, m, ;
: __#         ( $-  ) lit, toNumber m, ; parsing
: $,          ( $-  ) withLength [ @+ m, ] times 0 m, drop ;

: __: ( $-  ) header t-here @last !d->xt ; parsing
: call ( "- ) ' m, ;
: jump ( "- ) 8 m, ' m, ;

( Setup target memory for new image ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ )
here [ !target ] [ !origin ] bi MAX-APP-SIZE allot
jump, 0 m, pad
reset


( Support functions: basic input, output, and data pointer support )
:wait
  #0 #0 out,
  wait,
  ret,

:bye
  #-9 #5 out,
  ret,

:dp 32768 m,

:bf_>
  dp # @,
  1+,
  dp # !,
  ret,

:bf_<
  dp # @,
  1-,
  dp # !,
  ret,

:bf_+
  dp # @, @,
  1+,
  dp # @, !,
  ret,

:bf_-
  dp # @, @,
  1-,
  dp # @, !,
  ret,

:bf_.
  dp # @, @,
  #1 #2 out,
  call wait
  #0 #3 out,
  ret,

:bf_,
  #1 #1 out,
  call wait
  #1 in,
  dp # @, !,
  ret,

( Actual Brainfuck compiler )
variable ip

: run
  t-here putn space @ip @ putc cr
  @ip @ ip ++
  [ '> = ] [ drop bf_> m, ] when
  [ '< = ] [ drop bf_< m, ] when
  [ '+ = ] [ drop bf_+ m, ] when
  [ '- = ] [ drop bf_- m, ] when
  [ '. = ] [ drop bf_. m, ] when
  [ ', = ] [ drop bf_, m, ] when
  [ '[ = ] [ drop t-here dp # @, @, lit, 0 m, =jump, @target 0 m, ] when
  [ '] = ] [ drop swap jump, m, t-here swap ! ] when
  drop ;

: do
  [ run @ip @ ] while ;

: bf: ( "- )
  '~ accept tib keepString !ip cr do ;

( Start Compilation of Brainfuck code after this )
:main
```


Apart from support code, the actual compiler is implemented in the '''run''' function.

This accepts sources like:


```Retro
bf: >+++++++++[<++++++++>
-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]~
bf: <.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[~
bf: <++++>-]<+.[-]++++++++++.~

endApp
saveImage
```


And upon completion a new ''appImage'' file is created. This can be run from the command line, using the '''--image''' command line argument:


```Retro
./retro --image appImage
```

