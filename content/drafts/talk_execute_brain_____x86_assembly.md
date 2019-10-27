+++
title = "Talk:Execute Brain****/x86 Assembly"
description = ""
date = 2016-02-14T18:48:21Z
aliases = []
[extra]
id = 20042
[taxonomies]
categories = []
tags = []
+++

== Something weird ==



```txt

$ cat helloworld.b
>+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]
<.#>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[
<++++>-]<+.[-]++++++++++.

$ ./x86 helloworld.b
Hello World!
$ cat hell-test.b
>++++++++[-<+++++++++>]<.>[][<-]>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.
>->+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+.
$ bf hell-test.b
Hello World!
$ ./x86 hell-test.b
Hekkn World!
$ wget -q -O x86-test.b https://github.com/rdebath/Brainfuck/raw/master/bitwidth.b
$ bf x86-test.b
Hello World! 255
$ ./x86 x86-test.b
Hello$Zombie!
$


```

It's buggy, but not in any way I've seen before [[User:Rdebath|Rdebath]] ([[User talk:Rdebath|talk]]) 09:21, 13 February 2016 (UTC)


Odd. I'll get on this. [[User:Calculuswhiz|Calculuswhiz]] ([[User talk:Calculuswhiz|talk]]) 02:30, 14 February 2016 (UTC)

Ok. That should do it! I messed up my suffixes. Oops. Does that fix things? [[User:Calculuswhiz|Calculuswhiz]] ([[User talk:Calculuswhiz|talk]]) 04:03, 14 February 2016 (UTC)

: It's looking all right, a bit sluggish so I can't run every test, but the important ones run perfectly.
: The only thing I might add would be a <code>.section .bss</code> before all your zero init variables so the exe isn't a megabyte of nuls. [[User:Rdebath|Rdebath]] ([[User talk:Rdebath|talk]]) 08:42, 14 February 2016 (UTC)

:: Ah, so that's what .bss does. Thanks. I added it. [[User:Calculuswhiz|Calculuswhiz]] ([[User talk:Calculuswhiz|talk]]) 18:47, 14 February 2016 (UTC)
