+++
title = "Talk:Factorial"
description = ""
date = 2016-03-06T03:20:55Z
aliases = []
[extra]
id = 3323
[taxonomies]
categories = []
tags = []
+++

==range limits for the factorial function==
Just a thought...it would be interesting to programmatically identify the range limits of the factorial function for the unknown implementation. (The C and c++ implementations, for example, will overflow at different places depending on the range of int.) --[[User:Short Circuit|Short Circuit]] 18:33, 30 January 2009 (UTC)

: Yes, for non-native speakers of any language, it would be nice to know how big the '''thingys''' are: ''int, short, long, long long'', etc.  For experienced programmers, I imagine this is old hat, but to programmers who can barely spell '''C''', ... not so obvious. --- [[User:Gerard Schildberger|Gerard Schildberger]] 21:35, 30 May 2012 (UTC)

```txt

   ┌────────────────────────────────────────────────────────────────┐
   │               ───── Some factorial lengths ─────               │
   │                                                                │
   │                   10 !  =           7  digits                  │
   │                   20 !  =          19  digits                  │
   │                   52 !  =          68  digits                  │
   │                  104 !  =         167  digits                  │
   │                  208 !  =         394  digits                  │
   │                  416 !  =         911  digits   (8 deck shoe)  │
   │                                                                │
   │                   1k !  =       2,568  digits                  │
   │                  10k !  =      35,660  digits                  │
   │                 100k !  =     456,574  digits                  │
   │                                                                │
   │                   1m !  =   5,565,709  digits                  │
   │                  10m !  =  65,657,060  digits                  │
   │                 100m !  = 756,570,556  digits                  │
   └────────────────────────────────────────────────────────────────┘  

```
 

<strike>
In the table above, the factorial length for <b>416!</b> should be <b>911</b> rather than <b>394</b>. Used [[www.javascripter.net/math/calculators/100digitbigintcalculator.htm]] to verify. 

01:27, 12 January 2015 (UTC)</strike>

: Yes, it was inadvertently copied from the previous line. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:36, 12 January 2015 (UTC)

== The moving of 80386 to x86 Assembly ==

I think it is better to use generic x86, and at most to specify a "works with", if needed, rather than let proliferate 80286, 80386, 80486, ... &mdash;[[User:ShinTakezou|ShinTakezou]] 14:56, 27 April 2010 (UTC)
: I agree to a point; there's a definite progression and tendency toward backwards-compatibility. However, there are incompatible revisions. It's possible that different modes (real, protected, long and legacy) warrant some degree of recognition as their own languages--these modes represent forward and reverse compatibility constraints. --[[User:Short Circuit|Michael Mol]] 16:03, 27 April 2010 (UTC)

==D output==

<del>
Is the   '''D'''   output supposed to show (eight times) the value of   ('''15!''')   ? 
</del> 
-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:54, 26 May 2013 (UTC)

(This has been addressed and fixed.) -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:44, 26 May 2013 (UTC)
