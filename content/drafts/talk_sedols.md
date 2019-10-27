+++
title = "Talk:SEDOLs"
description = ""
date = 2017-03-05T23:53:22Z
aliases = []
[extra]
id = 2966
[taxonomies]
categories = []
tags = []
+++

== J ==

Does the J solution reject strings with vowels in them? It says on [[wp:SEDOL#Description|the wikipedia]] that vowels are never used in the 6-character code, so the function should reject strings that have them. --[[User:Mwn3d|Mwn3d]] 04:58, 5 August 2008 (UTC)

:The J code isn't a validator, it merely calculates and appends the check digit.  

:You could modify it to whine about vowels by removing them:

     sn   =.  '0123456789 BCD FGH JKLMN PQRST VWXYZ'  

:Not terribly exciting.

:[[User:68.175.31.239|68.175.31.239]] 12:44, 5 August 2008 (UTC) (aka [[User:DanBron|DanBron]])

== R ==
Does the R solution reject strings with vowels in them? It says on [[wp:SEDOL#Description|the wikipedia]] that vowels are never used in the 6-character code, so the function should reject strings that have them. (Is this a common trait of languages with single letter names ;-)

--[[User:Paddy3118|Paddy3118]] 19:18, 29 September 2009 (UTC)
:Now it looks like Modula-3 doesn't do the check. Maybe we need to move the algorithm here. --[[User:Mwn3d|Mwn3d]] 22:10, 29 September 2009 (UTC)
::Well, it seems most of the languages do not check for vowels. Only 8 out of the 26 examples check for vowels. --[[User:Mbishop|Mbishop]] 01:47, 30 September 2009 (UTC)
:::The specification doesn't say anything about checking for vowels, or validation in general.  It says calculate and append the check digit.
::::This was recently added: "Your program should also check each input is correctly formed, especially with respect to valid characters allowed in a SEDOL string." --[[User:Mwn3d|Mwn3d]] 17:04, 1 October 2009 (UTC)
:::::I disagree with this addition and may remove it.  I created the initial task and this wasn't in scope -- some people may've implemented it for extra credit, but that doesn't mean everyone has to.  The addition requirement breaks most of the existing solutions.  [[User:DanBron|DanBron]] 17:12, 1 October 2009 (UTC)

::::Hi Danbron; why the change? It seems a good extension to the task, to make it reject badly formed SEDOLs, as a lot of money could ride on its correctness. How about adding the checking as a "stretch goal" to the task? --[[User:Paddy3118|Paddy3118]] 18:50, 1 October 2009 (UTC)
:::::The reversion was due to the fact that the change broke most of the code.  I'm fine adding it as "extra credit" that doesn't make most examples "wrong".  The examples that provide extra validation could easily break it out as a separate function or call it out comments in the code.  (I see calculation and validation as separate concerns that shouldn't be mixed, and would personally separate them into two functions.  And I don't feel sorry for anyone that loses money running code he copped from a website, unscrutinized :)  --[[User:DanBron|DanBron]] 19:43, 1 October 2009 (UTC)
::::::Thanks. Edit made. --[[User:Paddy3118|Paddy3118]] 05:48, 2 October 2009 (UTC)

== '0' checksum ==

Among the test strings ought to be one that requires a '0' to verify that the checksum is in the range 0..9 instead of 1..10. --[[User:IanOsgood|IanOsgood]] 00:34, 7 August 2008 (UTC)

== TCL and _ ==
Hi, I generally like the use of '_' in the TCL solution, but on checking, I find that it fails to reject the use of '_' in a SEDOL:

```tcl
(bin) 11 % set code B0YBKT
B0YBKT
(bin) 12 % set sedol "${code}[sedol::checksum $code]"
B0YBKT7
(bin) 13 % set code B0YBAT
B0YBAT
(bin) 14 % set sedol "${code}[sedol::checksum $code]"
invalid character: A
(bin) 15 % set code B0YB_T
B0YB_T
(bin) 16 % set sedol "${code}[sedol::checksum $code]"
B0YB_T7
(bin) 17 % 
```

--[[User:Paddy3118|Paddy3118]] 05:01, 28 April 2009 (UTC)

True.  I'll fix that.
--[[User:Glennj|Glennj]] 11:19, 28 April 2009 (UTC)

== Extra modulo? ==

Does anyone else think the last modulo isn't needed? I asked on the WP talk page for SEDOL, but no one answered. The formula is given there as: (10 − (weighted sum modulo 10)) modulo 10. (10 − (weighted sum modulo 10)) should already be less than 10 and positive so another mod 10 wouldn't do anything right? --[[User:Mwn3d|Mwn3d]] 13:28, 11 November 2009 (UTC)
:I've tried it both ways and got the same output for several inputs. Does anyone else have an opinion? --[[User:Mwn3d|Mwn3d]] 20:29, 3 February 2010 (UTC)
:See my '0' checksum comment above.  x mod 10 is in the range 0..9, so 10-(x mod 10) is in the range 1..10.  The extra mod brings it back into the 0..9 range. We should have add a test case which exercises this.  --[[User:IanOsgood|IanOsgood]] 21:09, 3 February 2010 (UTC)
::I knew there had to be a reason which is why I didn't just change it right away. I agree we should have a test case for that. Do you know of one? You could just have the computer try random sequences until it gets one. --[[User:Mwn3d|Mwn3d]] 21:27, 3 February 2010 (UTC)
::B00030 gives a 0 check digit and I think it might be in the real range for SEDOLs. I think it gets that case. The weighted sum is 10. (10 - (10 mod 10)) = 10. (10 - (10 mod 10)) mod 10 = 0. We should add it to the examples. --[[User:Mwn3d|Mwn3d]] 21:37, 3 February 2010 (UTC)

== changed the Wiki page reference to here ==
I changed the Wiki page on SODOLs which mentioned (in the link)  that there are 34 Rosetta Code computer programming languages to compute SODOL.  

I changed it to 36 (the current count at this time). [[User:Gerard Schildberger|Gerard Schildberger]]
:Ah, that was you :-)
:I changed it to read "...more than 35 ..." so we don't have to continuously keep the wp page in sync. --[[User:Paddy3118|Paddy3118]] 10:18, 10 December 2010 (UTC)

:: That number has since doubled.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:52, 5 March 2017 (UTC)

== Excel VBA ==

Shouldn't the language be just "VBA"? Admittedly I know almost nothing about VBA and Excel but nothing in the code looks Excel-specific to me. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 22:27, 26 April 2014 (UTC)
