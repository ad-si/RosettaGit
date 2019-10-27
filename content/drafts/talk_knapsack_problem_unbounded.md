+++
title = "Talk:Knapsack problem/Unbounded"
description = ""
date = 2012-08-29T04:21:18Z
aliases = []
[extra]
id = 3197
[taxonomies]
categories = []
tags = []
+++

==Dynamic Python solution==
Is the underlying algorithm so different from what I called an exhaustive search solution? I don't think so.
P.S: should the line  ''w = sack.volume; v = sack.volume'' have the first volume replaced by weight?

It's great that you thought the problem worthy of your time though.  I spent ages trying to find something beginning with K and then on formulating the problem :-)

--[[User:Paddy3118|Paddy3118]] 05:40, 3 December 2008 (UTC)

:Yes, in theory. Dynamic programming runs in time polynomial in the size of the sack weight, sack volume, and number of types of items; whereas the exhaustive search runs in time exponential in the number of types of items. But since the number of types of items here is so small (3), you won't see much of a difference. --[[User:Spoon!|Spoon!]] 07:04, 3 December 2008 (UTC)

:: I'm new to this big-O notation stuff, but I think my algorithm would run in time proportional to: 
```txt
the product of ( the minimum of ( max_restriction(r)/restriction(r of i) for all restrictions r ) ) for each item i 
```
 I can't see the exponential? --[[User:Paddy3118|Paddy3118]] 07:44, 3 December 2008 (UTC)

::: Let n = the number of types of items. So what I am saying is that you are taking the product of n things. So as n grows, you can think of it as something like (something)^n in the worst case. Or in other words, every time you add a new type of item, you always have to multiply it by some (potentially big) number. With the dynamic programming solution, it runs in time of (something)*n; so when n is already very big, adding a new type of item does not increase it by very much. Perhaps you can experiment with this and see what happens if you add a whole bunch of new items. --[[User:Spoon!|Spoon!]] 10:01, 3 December 2008 (UTC)

The decimal points need shifting in the printout of the Python "Dynamic Programming solution". --[[User:Paddy3118|Paddy3118]] 13:17, 2 January 2009 (UTC)

:The problem is that in order to use the dynamic programming solution I had to re-word the problem to only use integer weights and volumes. So the printout is correct for that problem statement. I guess I could manually divide the weights and volumes by the corresponding amounts when I print them, but it doesn't seem like a very nice solution. --[[User:Spoon!|Spoon!]] 22:59, 2 January 2009 (UTC)

::I found [[http://razvi.wordpress.com/2008/10/09/dynamic-programming-integer-knapsack/ this]] to help work out what the dynamic solution looks to do. --[[User:Paddy3118|Paddy3118]] 18:06, 5 January 2009 (UTC)

:::Note however that that page addresses a slightly different problem; the "0-1" problem, where you can take at most 1 of each item. Whereas in the problem here we can take as many of each item as we want. --[[User:Spoon!|Spoon!]] 02:48, 6 January 2009 (UTC)

==The problem with the PARI/GP solution==

Is that it output includes non-optimal solutions, something like:

```txt
$ 30000 :  0  panecea,  0  ichor,  12  gold
$ 31800 :  0  panecea,  1  ichor,  12  gold
$ 33600 :  0  panecea,  2  ichor,  12  gold
$ 35400 :  0  panecea,  3  ichor,  12  gold
$ 37200 :  0  panecea,  4  ichor,  12  gold
$ 39000 :  0  panecea,  5  ichor,  12  gold
$ 40100 :  0  panecea,  7  ichor,  11  gold
$ 41900 :  0  panecea,  8  ichor,  11  gold
$ 43700 :  0  panecea,  9  ichor,  11  gold
$ 45500 :  0  panecea,  10  ichor,  11  gold
$ 47300 :  0  panecea,  11  ichor,  11  gold
$ 49100 :  0  panecea,  12  ichor,  11  gold
$ 50900 :  0  panecea,  13  ichor,  11  gold
$ 52700 :  0  panecea,  14  ichor,  11  gold
$ 54500 :  0  panecea,  15  ichor,  11  gold
$ 54500 :  3  panecea,  10  ichor,  11  gold
$ 54500 :  6  panecea,  5  ichor,  11  gold
$ 54500 :  9  panecea,  0  ichor,  11  gold
```


You need to go the extra mile and reduce the output to the ''very'' "best", as the task description requires. --[[User:Paddy3118|Paddy3118]] 06:59, 30 October 2010 (UTC)

== Terrible numbers ==

With the numbers given, dynamic programming didn't produce a single cache hit.  Basically the given data is perfect for brute force, and a very easy one at that.  Probably not worth all the discussions about big-O as is. --[[User:Ledrug|Ledrug]] 02:01, 11 June 2011 (UTC)
: "restructured. Still brute force (data makes caching not worthwhile), but at least it doesn't hard code three nested loops.)". ''At least'' what? Recursive and iterative methods are both ok (in this case). Maybe you meant to say "recursive brute force instead of iterative"? I even wonder  how would you do three nested loops without hard-coding them. About discussions on big-O, '''actual''' examples do not matter, change the "terrible numbers" and make worth it. --[[User:ShinTakezou|ShinTakezou]] 19:35, 21 May 2012 (UTC)
