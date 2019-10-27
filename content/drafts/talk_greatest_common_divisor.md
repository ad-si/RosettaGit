+++
title = "Talk:Greatest common divisor"
description = ""
date = 2015-07-19T14:35:44Z
aliases = []
[extra]
id = 11701
[taxonomies]
categories = []
tags = []
+++

==errors in programs==

A few programs would attempt to divide by zero if the 2nd argument is 0 (zero).   

 In that special case, the absolute value of the first argument should be returned. -- [[User:Gerard Schildberger|Gerard Schildberger]] 15:39, 17 August 2012 (UTC)

::unless it is 0 --[[User:Walterpachl|Walterpachl]] 12:40, 17 August 2012 (UTC)

::: This gets into the definition of GCD.  What you are proposing here -- that 0 GCD 0 be an exceptional case -- would mean that GCD is not associative.  Meanwhile, Boolean algebra/rings uses an associative definition for GCD.  So I guess I do not feel motivated to adopt your definition.  --[[User:Rdm|Rdm]] 17:29, 17 August 2012 (UTC)

:::: I hope I made it clear that it wasn't my definition, but a convention that some people use.  I chose the definition ['''gcd(0,0=0'''] that didn't cause a '''SYNTAX''' condition to be raised (in REXX) and cause the program to raise an error condition, or cause it to go into error recovery. The '''GCD''' function is normally only defined for non-zero integers (some define it for only positive integers), it's the case(s) where there're arguments which are zero that are contested. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:25, 17 August 2012 (UTC)

:::: It hurts to read that a divisor (how great or common it may be) should be zero when dividing by zero is among the worst things you can do. As for me, I rather go for  undefined! --[[User:Walterpachl|Walterpachl]] 20:47, 17 August 2012 (UTC)

::::: And, yet, this definition -- where 0 == gcd(0,0) -- is the basis of the original formulation of boolean algebra.  It hurts that this bit of history, of math, of logic, of the terminology we use -- is actively being lost to redefinition (and perhaps an unwillingness to think things through).

::::: The problem with dividing a number which is not zero, by zero is that you get a zero result when you multiply zero by any number.  So, obviously, the issue of dividing zero by zero is different -- here, the problem is not that there's no valid answer.  When you divide zero by zero, the problem is that all answers are valid.  So, 0 as the answer for 0 == GCD(0,0) has to be valid.  It's not the only valid answer, but it's algorithmically simple (we do not need any exceptions in the euclidean algorithm to deal with this case) and it's syntactically simple (it lets GCD be associative).  

::::: And, this definition corresponds to the definition that Claude Shannon used for for his subset of George Boole's algebra.  There are other ways of using algebraic notation with logical values, but I am reminded of this approach every time I use a "boolean value".  --[[User:Rdm|Rdm]] 13:12, 18 August 2012 (UTC)

-----

The special case of '''gcd(0,0)''' is usually defined to be '''0''', but some authors consider it to be '''undefined'''. When implementing the REXX version 1 example, the first definition (equal to zero) was chosen.  So, for that case, '''|0| = 0'''. 

From the Wikipedia page: http://en.wikipedia.org/wiki/Greatest_common_divisor

It is useful to define gcd(0,0)=0 and lcm(0,0)=0 because then the natural numbers become a complete distributive lattice with gcd as meet and lcm as join operation. 

-- [[User:Gerard Schildberger|Gerard Schildberger]] 16:10, 17 August 2012 (UTC)



A number of examples don't show the results if either argument is negative. -- [[User:Gerard Schildberger|Gerard Schildberger]] 15:39, 17 August 2012 (UTC)

== REXX Version 1 ==

These results seem to be wrong:
:the GCD of  14  and  0  and 7               is 14 should be 7
:the GCD of   0  and  7                      is 0  should be 7
:the GCD of   0  and  0                      is 0 should be ???
correct:
:the GCD of   7  and  0                      is 7  
--[[User:Walterpachl|Walterpachl]] 12:06, 17 August 2012 (UTC)

Cases of zero have been corrected (the GCD subroutine was exiting instead of processing more arguments). The special case of gcd(0,0) can be defined to be 0, or ''undefined''.  Zero was choosen. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:08, 17 August 2012 (UTC)

: And,yes, wikipedia [[wp:Greatest_common_divisor#Properties|claims]] that gcd is associative.  --[[User:Rdm|Rdm]] 17:31, 17 August 2012 (UTC)


::I don't understand why this is relevant! It's allso communicative.
::I'll surrender and change indef(inite) to 0 as much as I dislike to read DIVISOR=0 --[[User:Walterpachl|Walterpachl]] 15:17, 18 August 2012 (UTC)

== PL/I ==

should return a positive integer
--[[User:Walterpachl|Walterpachl]] 12:39, 17 August 2012 (UTC)
