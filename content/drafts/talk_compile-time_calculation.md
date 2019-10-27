+++
title = "Talk:Compile-time calculation"
description = ""
date = 2010-07-13T07:42:55Z
aliases = []
[extra]
id = 6087
[taxonomies]
categories = []
tags = []
+++

==D Example==
Should be correct. I queried a D developer, and got this:

```txt
22:24 <@shortcircuit> feep: Could you check on this? 
                      http://rosettacode.org/mw/index.php?title=Compile-time_calculation&diff=75121&oldid=prev
22:25  * shortcircuit wishes he knew of a channel full of D programmers that would find such queries interesting.
22:26 <+feep> hah
22:27 <+feep> shortcircuit: no, it's run at compile time
22:27 <+feep> (you can verify this by disassembling main)
22:28 <+feep> shortcircuit: enum is D2's word for "constant symbol"
```
 --[[User:Short Circuit|Michael Mol]] 03:31, 19 February 2010 (UTC)

:Thanks for checking this! I added a comment to the example. So the next D-illiterate like me will not be confused ;-) - [[User:Wmeyer|Wmeyer]] 16:44, 19 February 2010 (UTC)

== Ada ==

Tried with gnat and analysing the produced asm, it seems that the computation is not done at compile-time. --[[User:ShinTakezou|ShinTakezou]] 07:42, 13 July 2010 (UTC)
