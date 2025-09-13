+++
title = "Execute HQ9+/Ursala"
description = ""
date = 2010-02-06T14:31:53Z
aliases = []
[extra]
id = 4725
[taxonomies]
categories = []
tags = []
+++


In this [Ursala](https://rosettacode.org/wiki/Ursala) program, there are the following constraints:
* Unrecognized operators or a missing input file cause diagnostic messages.
* Since the accumulator is unreadable and has no operational consequences, its value isn't stored.
* A line break at the end of the file, if any, is ignored.


```Ursala
#import std                     
#import nat                     
                                
quantity = ~&iNC+ --' of beer'+ ~&?(
   1?=/'1 bottle'! --' bottles'+ ~&h+ %nP,
   'no more bottles'!)

verse =

^(successor,~&); ("s","n"). -[
   -[quantity "s"]- on the wall, -[quantity "s"]-,
   Take one down and pass it around, -[quantity "n"]- on the wall.]-

refrain "n" =

-[
   No more bottles of beer on the wall, -[quantity 0]-.
   Go to the store and buy some more, -[quantity "n"]- on the wall.]-

whole_song "n" = ~&ittt2BSSL (verse*x iota "n")--<refrain "n">

#executable ('parameterized','')

rchq =

<.file$[contents: --<''>]>+ -+
   *= case~&r (
      {`h: -[Hello, World!]-!,`9: whole_song99!,`+: <>!,`q: ~&l},
      % <.'unrecognized operator: '--+ ~&rNC>),
   ~command.files; ~&itZB?/~&h.contents.&iziyQBLD <'usage: rchq [file]'>!%+-
```

