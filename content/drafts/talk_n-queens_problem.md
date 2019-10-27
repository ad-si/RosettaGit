+++
title = "Talk:N-queens problem"
description = ""
date = 2017-10-14T12:42:50Z
aliases = []
[extra]
id = 4839
[taxonomies]
categories = []
tags = []
+++

== Tcl: All Solutions ==
Is the goal to find one solution, or all solutions? --[[Special:Contributions/164.67.235.98|164.67.235.98]] 19:59, 12 September 2009 (UTC)

: I've put up code to find all solutions; finding just one is a trivial adaptation of that (stop iterating after the first solution is found...) —[[User:Dkf|Donal Fellows]] 20:37, 12 September 2009 (UTC)

:: I read the task's requirements differently.   It said to   <big> ''Solve the eight queens puzzle'', </big>   and once solved, stopped looking for more solutions.   I didn't see any point in providing multiple answers.   Of the   '''92'''   solutions on an   '''8x8'''   board,   '''12'''   are fundamental solutions --- and just sifting (choosing which ones to display) those out (I thought) isn't part of the task's requirement.   ··· And if the answers are an ASCII art depiction of the chessboard, the amount of output would be greatly compounded.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:57, 6 May 2016 (UTC)

== Python permission ==
I thought I might copy the email trail where I got permission to post a Python solution from its original author:

<small>
```txt
Subject:
Re: 8-Queens
From:
Steve Howell <showell30@YY.com>
Date:
Mon, 14 Sep 2009 23:07:09 -0700 (PDT)
To:
Paddy McCarthy <paddy3118@XX.net>

Thanks!

--- On Mon, 9/14/09, Paddy McCarthy <paddy3118@XX.net> wrote:


    From: Paddy McCarthy <paddy3118@XX.net>
    Subject: Re: 8-Queens
    To: "Steve Howell" <showell30@YY.com>
    Date: Monday, September 14, 2009, 11:54 AM

    Ta very much. It's in place.

    Steve Howell wrote:
>     Please do, thank you.
>
>     Sent from my iPhone
>
>     On Sep 13, 2009, at 10:43 AM, Paddy McCarthy <paddy3118@XX.net> wrote:
>
>>     Hi,
>>     I wondered if you would allow me to submit your 8-Queens solution to  Rosetta  Code ?
>>
>>
>>     -- Donald 'Paddy' McCarthy
>>     http://paddy3118.blogspot.com/
```
</small>
--[[User:Paddy3118|Paddy3118]] 04:28, 6 September 2010 (UTC)

== Removing existing C code: webbug? ==

I added a C solution, then realized the existing C example is sending results to some http server.  This amounts to a web bug, I'm removing it, until the intent of the webbug can be clarified and justified --[[User:Ledrug|Ledrug]] 03:34, 6 July 2011 (UTC)

:Apparently the ip address is somewhere in germany, on a machine owned by 23media.eu.  Also, a google search finds http://forum.chip.de/c-net/c-code-socket-laeuft-1443464.html which suggests that this is a machine that at one point was owned by Leonie Schöller.  Then again, http://178.77.72.203/wbb2/thread.php?postid=3648536 suggests a machine owned by someone named Manuela. --[[User:Rdm|Rdm]] 04:44, 6 July 2011 (UTC)
:: Whoever it was, that was a fairly odd thing to do: it's not like code here gets compiled and run very often--  &mdash;[[User:Ledrug|Ledrug]] 05:25, 6 July 2011 (UTC)

== REXX mistake == 
I'm not familiar with REXX, nor do I have any idea how to fix it; however, the two outputs produced (for board sizes 8 and 20) are both unsuccessful attempts at nqueens. In the 8x8 board, rows 4 and 6 (1-up) have queens in diagonal contradiction; in the 20x20 board, (at least, didn't bother to check more) rows 1 and 6 have queens in diagonal contradiction. 

Anyone familiar enough to know whether this is a printing error or a solving error, and either way, how to fix it?

: It was my mistake, I had cut and pasted the solutions from an earlier version, corrected the REXX program, but neglected to updated the outputs.   I re-ran the REXX program and pasted the chessboard to the OUTPUT sections. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:39, 22 July 2013 (UTC)

== What about profiling? == 
When doing the Erlang version I thought it would be much faster to calculate diagonals in advance. It did not help at all. I should have profiled before changing the code.

Is there a task on Rosettacode that looks at profiling? Similar to [[Time_a_function]], but for several functions in a task. [[User:bengt]] Sat Dec  7 13:23:33 CET 2013

== Pari/GP ==

I am not known for my compact code, but I have a script in Pari/GP.  Not optimized to any extent.  Just contact me.--[[User:Billymacc|Billymacc]] ([[User talk:Billymacc|talk]]) 12:42, 14 October 2017 (UTC)
