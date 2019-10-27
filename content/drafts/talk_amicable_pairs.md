+++
title = "Talk:Amicable pairs"
description = ""
date = 2016-10-22T15:25:05Z
aliases = []
[extra]
id = 19623
[taxonomies]
categories = []
tags = []
+++

==amicable pairs, out of order==
The following is the output from the REXX program (3rd entry) when specifying   '''2333444'''   (two million +)   as the argument:

```txt

    220  and      284  are an amicable pair.
   1184  and     1210  are an amicable pair.
   2620  and     2924  are an amicable pair.
   5020  and     5564  are an amicable pair.
   6232  and     6368  are an amicable pair.
  10744  and    10856  are an amicable pair.
  12285  and    14595  are an amicable pair.
  17296  and    18416  are an amicable pair.
  66928  and    66992  are an amicable pair.
  67095  and    71145  are an amicable pair.
  63020  and    76084  are an amicable pair.
  69615  and    87633  are an amicable pair.
  79750  and    88730  are an amicable pair.
 122368  and   123152  are an amicable pair.
 100485  and   124155  are an amicable pair.
 122265  and   139815  are an amicable pair.
 141664  and   153176  are an amicable pair.
 142310  and   168730  are an amicable pair.
 171856  and   176336  are an amicable pair.
 176272  and   180848  are an amicable pair.
 196724  and   202444  are an amicable pair.
 185368  and   203432  are an amicable pair.
 280540  and   365084  are an amicable pair.
 308620  and   389924  are an amicable pair.
 356408  and   399592  are an amicable pair.
 319550  and   430402  are an amicable pair.
 437456  and   455344  are an amicable pair.
 469028  and   486178  are an amicable pair.
 503056  and   514736  are an amicable pair.
 522405  and   525915  are an amicable pair.
 643336  and   652664  are an amicable pair.
 600392  and   669688  are an amicable pair.
 609928  and   686072  are an amicable pair.
 624184  and   691256  are an amicable pair.
 635624  and   712216  are an amicable pair.
 667964  and   783556  are an amicable pair.
 726104  and   796696  are an amicable pair.
 802725  and   863835  are an amicable pair.
 879712  and   901424  are an amicable pair.
 898216  and   980984  are an amicable pair.
 998104  and  1043096  are an amicable pair.
1077890  and  1099390  are an amicable pair.
 947835  and  1125765  are an amicable pair.
1154450  and  1189150  are an amicable pair.
1185376  and  1286744  are an amicable pair.
1156870  and  1292570  are an amicable pair.
1280565  and  1340235  are an amicable pair.
1175265  and  1438983  are an amicable pair.
1392368  and  1464592  are an amicable pair.
1328470  and  1483850  are an amicable pair.
1358595  and  1486845  are an amicable pair.
1511930  and  1598470  are an amicable pair.
1466150  and  1747930  are an amicable pair.
1468324  and  1749212  are an amicable pair.
1798875  and  1870245  are an amicable pair.
1669910  and  2062570  are an amicable pair.
2082464  and  2090656  are an amicable pair.

57 amicable pairs found up to 2333444

```

It clearly shows that some of the amicable pairs are "out of order".   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:34, 5 October 2015 (UTC)

: It looks to me like the REXX output is sorted on the larger value of the pair and that your point is that this means that the smaller values are not in sorted order? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:44, 5 October 2015 (UTC)

:: Actually, the output isn't sorted at all, except in the sense that, when looking for the amicable pairs that were found, it finds the lowest number for the 2nd number in the pair.   This is just an artifact of how the search was performed.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:01, 5 October 2015 (UTC)

::: I think you mean that there was no post-process sorting algorithm used here. In other words, I think are talking about the structure of the algorithm rather than the structure of the data. Nevertheless, a statement such as <code>Do x=1 To 20000</code> generates values for x in a sorted order... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:03, 7 October 2015 (UTC)

:::: The   '''do'''   loop mentioned above (as used in the REXX program), like you said, generates values of   '''x'''   in a sorted order, but does not do any sorting of data (amicable numbers).   However, what the   '''do'''   loop does, in reality, is generating values for   '''x'''   in numerical order, where   '''y'''   is coupled to the value of   '''x'''   (where   '''x''' is the first part of the amicable pair, and   '''y'''   is the second part).   However, the   '''sigma'''   of   '''x'''   most likely isn't known at this time, so the value of   '''x'''   isn't displayed until the   '''sigma'''   of   '''y'''   is computed, thus, the values of   '''x'''   are shown out of order, even though one would think that the values of   '''x'''   should appear in numerical order.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:58, 7 October 2015 (UTC)

<!-- Like baseball, the runner on 1st can't run to 2nd until the runner on 2nd advances to 3rd.  Apologies for the similarity to the  Abbot and Costello  routine. -->

:::: Ok. Are you suggesting that one or more of the rexx implementations would discover the   '''y'''   values "out of order"? --[[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:38, 7 October 2015 (UTC)

:::: In fact, all of the REXX versions that I entered show the   '''y'''   values in order.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:38, 7 October 2015 (UTC)


==All formulae up to Example rendered invisible to many browsers by white-space tidying==

Under-tested cosmetic edits at 18:44, 11 September 2016, including the injection of redundant spaces into &lt;math&gt; tags rendered all formulae before the word '''example''' completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of this cosmetic edit may have further compounded the problem [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 16:27, 20 September 2016 (UTC)

: Visibility of task description formulae now restored. This has entailed reverting the task description to its state before the under-tested cosmetic edits of 18:44, 11 September 2016, which left formulae invisible to most browsers on all platforms. The author of these  cosmetic interventions is welcome to fine-tune, but will need to test the real effects of any edits in the main class of browsers (which display the server-side formula graphic) as well as in the minority class (e.g. FireFox), which (installed fonts permitting) process MathML locally. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 15:25, 22 October 2016 (UTC)
