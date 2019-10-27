+++
title = "Talk:Topswops"
description = ""
date = 2017-06-16T01:13:29Z
aliases = []
[extra]
id = 12628
[taxonomies]
categories = []
tags = []
+++

==Speed?==
Should I change the limits and ask for an output table for n in 1..8 instead? The idea is not to solicit heavily speed optimised solutions but to show an accurate solution in idiomatic code. --[[User:Paddy3118|Paddy3118]] 22:48, 22 November 2012 (UTC)
:I would be surprised if there was not a better algorithm than the straightforward translation of the rounding process.  So I think it's fine to let n from 1 to 10 in order to give an incentive to find a good algorithm.--[[User:Grondilu|Grondilu]] 23:16, 22 November 2012 (UTC)

::With a limit that is greater than nine, (at least for the approach that I used in REXX), I needed to use a different algorithm   (with a two digit decimal number).   So, I think that a limit of ten is warranted, although speed was an issue for me for testing.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:12, 16 June 2017 (UTC)

== Repitition? ==

I assume you're not allowed to repeat configurations?  Otherwise you could swap infinitely for any initial setup that didn't start with 1. --[[User:Mwn3d|Mwn3d]] 20:26, 23 November 2012 (UTC)

: I think I read that for all starting permutations you either start at or end up with one on top. There was no mention of it not terminating.  There is [http://oeis.org/A000376 a variant] where you only count a perm if it ends up sorted when the one is on top. (It doesn't always). --[[User:Paddy3118|Paddy3118]] 23:28, 23 November 2012 (UTC)

: There's no need to worry about that.  It's easy to prove that the swapping process always terminates with 1 at the front, which is the same as saying the configurations will never repeat. --[[User:Ledrug|Ledrug]] 00:22, 24 November 2012 (UTC)
::Maybe I'm missing something then. Why can't you have a sequence of reversals like this?:
 2, 5, 1, 4, 3
 4, 1, 5, 2, 3
 2, 5, 1, 4, 3
 And on and on forever...
::Any starting sequence that doesn't have 1 at the start or end (since I assume you would want to have the 1 be part of every reversal step) could go on forever like that. If you're looking for the max then infitity would be it for those starting configurations unless there are additional rules for the reversals. --[[User:Mwn3d|Mwn3d]] 17:35, 24 November 2012 (UTC)
:::Why are you reversing 4 elements when the first element is 2? --[[User:TimToady|TimToady]] 21:08, 24 November 2012 (UTC)
::::I found out what I missed! "Where m is the value of the topmost card". Sorry for the confusion. --[[User:Mwn3d|Mwn3d]] 04:17, 25 November 2012 (UTC)

== Alternate J solution ==

I moved this off the [[Topswops#J|main page]] because it is not idiomatic J; however, its structured-programming style may be more accessible to users coming from non-J languages, so I've reprised it here:

'''Solution''' (''interpretation of faster C version''):
```j
   trySwop=: 4 :0
'n d f s'=. y
best=: n (d>.{)`[`]} best
whilst. s=.s-1 do.
  if. s (= +. _1 = ]) s{x do. break. end.
  if. (d+s{best) <: n{best do. return. end.
end.
d=.d+1
t=._1=tx=.s{. }. x
for_i. 1+I.0= ((-.t)*.tx~:1+i.s) +. t *. 0~:f 17 b. 2^1+i.s do.
 ((|.i{.x) (1+i.i) } i 0 } x) trySwop n,d,s,~f 23 b. 2^i
end.
d=.d-1
) 

topSwops=: 3 :0
best=: 0$~y+1
for_n. i=.1+i.y do. (0, _1$~y) trySwop n, 0, 1, n-1 end.
i,.}. best
)

   topSwops 10
 1  0
 2  1
 3  2
 4  4
 5  7
 6 10
 7 16
 8 22
 9 30
10 38
```


I don't want this to be taken as an indictment of the code, and we can certainly discuss restoring it to the main page, but I'd like to see it whittled down and/or made more idiomatic first.  --[[User:DanBron|DanBron]] 01:30, 18 December 2012 (UTC)
