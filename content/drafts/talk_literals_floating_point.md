+++
title = "Talk:Literals/Floating point"
description = ""
date = 2012-08-28T17:49:25Z
aliases = []
[extra]
id = 9328
[taxonomies]
categories = []
tags = []
+++

== Fortran ==
I'm amazed that there is not a Fortran example for this yet. Since the ISO 1990 (1991?) standard (commonly called Fortran 90), Fortran has had some of the richest language-based (not library-dependent) floating point support around. ISO 2003 adds specific optional support for IEEE numbers too IIRC. I'll try to put together an example if I can find the time, but I no longer have access to a current Fortran compiler. --[[User:Balrog|Balrog]] 09:19, 5 March 2011 (UTC)

== REXX ==

not quite?

something=3;
format(something,,,,0) yields '3' (on ooRexx)
--[[User:Walterpachl|Walterpachl]] 18:27, 27 August 2012 (UTC)

Yes, I adjusted the explanation for the "rule" for the '''FORMAT''' bif for the fifth argument being zero.  

Note that if

```rexx
something = 3.0
say format(something,,,,0)
```
should return 

```txt

3.0

```

not

```txt

3

```

-- [[User:Gerard Schildberger|Gerard Schildberger]] 19:27, 27 August 2012 (UTC)

:I didn't say that, did I? ooRexx returns 3.0 for this!--[[User:Walterpachl|Walterpachl]] 20:33, 27 August 2012 (UTC)

:: No, I wasn't referring to ooRexx, but one of the common REXX interpreter used does, and I didn't want to name it specifically, but I thought it was worth the mention as there'll most likely be some finger pointing about which REXX does what, and what is the one and true answer.  When confronted with problems like this, I like to go back to King Soloman, er I mean, VM, and ask, what would CMS REXX do?  By the way, Personal REXX, PC/REXX, R4, ROO, and now ooRexx all agree.  That leaves the odd man, er, REXX, out. It would be nice if a CMS and TSO REXX whould chime in. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:41, 27 August 2012 (UTC)

::: So we found another difference between classic (as you call it) and ooRexx. The following is specified in ooRexx' manual:

```txt

If expt is 0, exponential notation is always used unless the exponent would
be 0.

```

what does Regina say for format(3,,,,0) ?
--[[User:Walterpachl|Walterpachl]] 06:04, 28 August 2012 (UTC)

No, we didn't find a difference between classic REXX and ooRexx. We just found a difference between a specific REXX (Regina) and ooRexx, and for that matter, almost all the REXXes agree (classic or not) with '''3.0''' being the correct answer. 

Regina results for '''format(3,,,,0)''' is the same as all the others: '''3'''

Regina results for '''format(3.0,,,,0)''' is: '''3''' instead of '''3.0'''. 

Note that Personal REXX, PC/REXX, R4, ROO, ooRexx, TSO REXX all return '''3.0''' (the correct value). 

Presumably CMS REXX will return the same result as TSO REXX. I don't have access to REXX/imc and other REXXes not mentioned, but it would be nice to see what those REXXes return. -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:34, 28 August 2012 (UTC)

:::: Test on REXX/TSO shows

```txt
 
say format(3,,,,0)    
say format(3.0,,,,0)  
3         
3.0    

```
  
--[[User:Walterpachl|Walterpachl]] 11:35, 28 August 2012 (UTC)
::::: But the description in the REXX solution does not mention the special case... "unless the exponent would be 0. --[[User:Walterpachl|Walterpachl]] 17:39, 28 August 2012 (UTC)

::::: I thought I had added that exception, but I must not have saved the update. I re-did the addition.  A better excuse was that the dog ate my homework. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:49, 28 August 2012 (UTC)
