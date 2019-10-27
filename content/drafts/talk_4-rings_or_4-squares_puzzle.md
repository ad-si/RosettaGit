+++
title = "Talk:4-rings or 4-squares puzzle"
description = ""
date = 2017-06-10T13:47:28Z
aliases = []
[extra]
id = 21287
[taxonomies]
categories = []
tags = []
+++

=Why 2860?=

```txt

The equations are:
 a+b=X
 b+c+d=X
 d+e+f=X
 f+g=X

which imply that d = a-c
             and d = g-e

when d=9 the only values are a=9, c=0
when d=8 the values are a=9, c=1; a=8, c=0.
which for d=0..9 sums to 55.

So there are 55*55 cases to consider for a, c, d, e, and g.
Fixing b fixes f so it should not be necessary to consider more than 55*55*10 (which is 25250) cases, which
 is rather less than the permutations that some solutions are testing!

The task is to determine the number of solutions that there are, for which I need to go a little further.

X must have a minimum value of d when b,c,e,f=0 and a maximum value of 18 when a,b=9.
For d=0..9 I generate something Pascal's Triangle like for the count Z of solutions:

d=9 Z9 = 10 ->10*1
d=8 Z8 = 38 -> 2*1 + 9*2*2
d=7 Z7 = 82 -> 2*1 + 2*2*2 + 8*3*3
d=6 Z6 =140 -> 2*1 + 2*2*2 + 2*3*3 + 7*4*4
d=5 Z5 =210 -> 2*1 + 2*2*2 + 2*3*3 + 2*4*4 + 6*5*5
d=4 Z4 =290 -> 2*1 + 2*2*2 + 2*3*3 + 2*4*4 + 2*5*5 + 5*6*6
d=3 Z3 =378 -> 2*1 + 2*2*2 + 2*3*3 + 2*4*4 + 2*5*5 + 2*6*6 + 4*7*7
d=2 Z2 =472 -> 2*1 + 2*2*2 + 2*3*3 + 2*4*4 + 2*5*5 + 2*6*6 + 2*7*7 + 3*8*8
d=1 Z1 =570 -> 2*1 + 2*2*2 + 2*3*3 + 2*4*4 + 2*5*5 + 2*6*6 + 2*7*7 + 2*8*8 + 2*9*9
d=0 Z0 =670 -> 2*1 + 2*2*2 + 2*3*3 + 2*4*4 + 2*5*5 + 2*6*6 + 2*7*7 + 2*8*8 + 2*9*9 + 1*10*10

Sum of Z0 through Z9 is 2860

```

--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 17:49, 25 January 2017 (UTC)
=Why nearly all write "non-unique solutions" ? =
I think the unique solutions are part of the 2860 solutions.So I think calling them solutions is the right term
--[[User:Horsth|Horsth]]

```txt


There is a better algorithm for the solutions when they are unique:

Take 1 from 7 as d and leave set of 6 remaining
Take 1 from 6 as 'a' fixing c if part of the remaining set
Take 1 from 4 as g fixing e if part of the remaining set
which leaves 2 values for b.

So maximum of 336 combinations (7*6*4*2) to test, rather than 5040 permutations of all 7.

```

--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 18:29, 25 January 2017 (UTC)

== <del>Some missing solutions</del> ==

<del>It looks like some of the current implementations are missing some of the puzzle solutions.</del> --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:55, 10 June 2017 (UTC)

    4 5 3 1 6 2 7
    7 2 6 1 3 5 4
    3 7 2 1 5 4 6
    6 4 1 5 2 3 7
    6 4 5 1 2 7 3
    7 3 2 5 1 4 6
    3 8 1 2 4 5 6 <-
    3 8 1 2 5 4 7 <-
    3 8 2 1 4 6 5 <-
    3 8 2 1 6 4 7 <- 
    4 7 1 3 2 6 5
    5 6 2 3 1 7 4

:Re the J solution (reproduced above) Ummm. I think you have a bug in your list generation. Where does 8 fall in 1 through 7? --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 13:34, 10 June 2017 (UTC)

::Ah, good point. Careless of me. Thanks. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:46, 10 June 2017 (UTC)
