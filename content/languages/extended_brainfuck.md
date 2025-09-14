+++
title = "Extended Brainfuck"
description = ""
date = 2019-03-29T23:40:32Z
aliases = []
[extra]
id = 18278
[taxonomies]
categories = []
tags = []
+++

'''Extended Brainfuck''' is an extension of [Brainfuck](https://rosettacode.org/wiki/Brainfuck),
that aims to make it easier to use.

There are several more opcodes, e.g.:
* <tt>@</tt> : Ends the program, can be used as a separator between code and data.


With data already in place, most BF-programs get much shorter,

e.g. a "Hello World"-program in standard BF needs to do a lot of work

to setup the ASCII-codes of the text:

```brainfuck
bf>++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++
++
++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>
>+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.
<+++++++.--------.<<<<<+.<+++.---.
```


Extended BF just needs a short loop for output:

```brainfuck
bf>[.
]@Hello World!
```


## Links

- [Esolangs - Brainfuck](https://esolangs.org/wiki/Extended_Brainfuck)
