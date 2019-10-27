+++
title = "Talk:Dutch national flag problem"
description = ""
date = 2016-07-12T15:48:32Z
aliases = []
[extra]
id = 11940
[taxonomies]
categories = []
tags = []
+++

==Flag request==
This task needs a picture of the Dutch national flag. --[[User:Paddy3118|Paddy3118]] 09:34, 1 July 2012 (UTC)
:http://en.wikipedia.org/wiki/Netherlands [[User:Fwend|Fwend]] 06:04, 2 July 2012 (UTC)

:: I've added an HTML rendition of the Dutch flag (immediately before the '''Cf.''').   I tried to use   ''style=float''   with right justification, but couldn't make that work without HTML forcing some (unwanted) inter-line spacing.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:19, 9 June 2015 (UTC)

::: Thanks Gerard, I think the task was missing it. I took the liberty of moving the flag text to a sub-heading so the text didn't dominate the task description area. What do you think? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:57, 9 June 2015 (UTC)

:::: Looks great.   I was thinking along the lines of what you did, but I didn't want to utilize too much vertical space (in the Rosetta Code task preamble).   I spent quite a bit of time in doing something so simple;   I first tried to use a multicolored table (one color for each row, and with no text), but that wasn't good enough --- I couldn't get that square peg into a round hole.   So I ended up just rendering a "full" glyph character   █   with a color(ed) font.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:15, 9 June 2015 (UTC)

==Algorithms==
I kinda ducked the issue of optimized algorithms in favour of giving an answer in keeping with how things would be done idiomatically in a language. 
This shouldn't stop those who want to give famous algorithms as well though. --[[User:Paddy3118|Paddy3118]] 09:34, 1 July 2012 (UTC)

: That's completely backwards. Making a Dutch flag isn't an idiomatic thing to do no matter what language you use, and the Python and J examples' using built-in sort completely missed the point.  What's interesting about the problem is that, if you know sorting the array only involves comparing a samll set of values (3 in this case), you can have an O(n) method instead of O(n log(n)) as with a standard sort.  Using an "idiomatic", non-specific method here makes no sense: there are plenty other sorting tasks on RC you can go idiomaticize with, why make yet another one?
: BTW, the J comment about bin sort (counting sort) isn't entirely correct: two elements may compare equal for this sorting purpose, but it doesn't mean they are necessarily identical.  That is, balls with the same color may still be distinguishable from each other, and the sort routine shouldn't automatically assume otherwise.  --[[User:Ledrug|Ledrug]] 02:57, 2 July 2012 (UTC)

::The wording does not stop you from using other algorithms and listing their relative strengths. I've just added a second Python entry myself. --[[User:Paddy3118|Paddy3118]] 06:39, 2 July 2012 (UTC)

::The task, as currently written, does not support the idea of balls containing information which is ignored for the purpose of the sort.  (For example, imagine a task which asks us to sort numbers in the range 1000..3999 by their most significant digit, preserving the relative order of values with the same leading digit.  Or, imagine a task which asks us to generate a variety of random objects and then sort a list of them based on one of their properties.)

::That said, if it did, we could still use the concept of a counting sort.  For example, we could replace the counters with stacks (or queues) -- that would still give us an O(n) algorithm.  (Or we could use some mathematically equivalent construct.)

::That said, note that O(n) algorithm does not guarantee O(n) behavior.  The cache architecture of modern machines will add a Heaviside step function (of n, where n is the amount of memory used) to the time needed for the algorithm to complete.  And, typical OS (and memory management) infrastructure will add random noise to this time.  --[[User:Rdm|Rdm]] 13:22, 2 July 2012 (UTC)

::: With three colors, the sorting can be done in-place, so cache miss is not relevant--the O(n) method requires absolutely minimal swaps, so if the entire array can't fit into cache, there isn't anything better you can do anyway.  Plus, the same cache issue is in your default sort routine, too.  But what's really wrong with initial Python and J examples is that they completely disregarded the essential constraint the problem posed, and provided something uninstructive.  In a sense, they are more like counter-examples than examples: what ''not'' to do to solve the original problem. --[[User:Ledrug|Ledrug]] 15:36, 2 July 2012 (UTC)

:::: When you say "cache miss is not relevant" you are making assumptions about the language implementation, about the OS, and about the hardware.  (You have assumed, for example, that intermediate results will not have to be garbage collected by an arbitrary gc implementation.)  Note also that in a typical machine architecture you have fixed costs associated with the hardware -- if you do not use those resources you're wasting them. --[[User:Rdm|Rdm]] 15:59, 2 July 2012 (UTC)
::::: Cache miss is irrelevant because it's not worse in the O(n) method than in whatever other method you can think of.  It's funny how cache suddenly becomes an issue now, but not when you were using a generic sort routine, hmm?  In any event, let it be known that ''I'' wasn't the one who dragged cache into this discussion, someone else did. --[[User:Ledrug|Ledrug]] 17:01, 2 July 2012 (UTC)
:::::: Correct, I raised that issue.  I raised it because I tried to devise a test that would let me compare the efficiency of the builtin with the efficiency of an O(n) userspace sort.  That said, I failed to point out that cache miss is a measurable issue even for in-place modify: data which is smaller than a cache element will have one timing and data which is larger than a cache element will have a different timing (a different constant multiplier), even for in-place modify.  In a test rig, this can look similar to the kind of effect you would see in a low exponent polynomial time implementation, for the kinds of numbers usually reported in benchmarks.  Anyways, when builtin sort is faster than userspace sort, and efficiency is the grounds for using userspace sort instead of builtin sort, what does that mean for this task?  --[[User:Rdm|Rdm]] 17:08, 2 July 2012 (UTC)
:::::Ignoring those sort of technical details (because they're not important to this task)...it seems that the original intent of the problem is to find a way to do it that in fact ''does'' minimize comparisons and reads on the data. O(n) would do that. O(n log(n)) would not do that. It seems like a big requirement of the original problem. If that isn't going to be part of the task here, then it might not be very valuable. --[[User:Mwn3d|Mwn3d]] 16:14, 2 July 2012 (UTC)
:::::: Actually, O(n log(n)) would do that for the original data if it's being applied to an extracted result rather than the original data.  --[[User:Rdm|Rdm]] 16:21, 2 July 2012 (UTC)

== "ensuring that they are not in the order of the Dutch national flag" ==
Please ensure that entries check for this too. (J language?). --[[User:Paddy3118|Paddy3118]] 22:01, 1 July 2012 (UTC)

:That's a low probability event for any reasonably sized collection of balls, so I did it manually in the example.  If you want it automatic, replacing ? with (3||.@/:)^:(-:/:~)@:? will work unless the user tries to generate a selection of only one random ball.  [I fussed with that one a bit -- the issue behind my fussing was that I wanted a single correction step for all the improbable things that could go wrong in a random sort.]  --[[User:Rdm|Rdm]] 00:58, 2 July 2012 (UTC)

::Hi, The Python is set up to generate a randomized, random sample of from one to five balls of each colour so it only has to check that the balls returned are not ordered. I don't read J code but I guess if you have the possibility of generating zero balls of a colour then  the check would have to be for more? --[[User:Paddy3118|Paddy3118]] 06:29, 2 July 2012 (UTC)

:::Yes.  (3||.@/:)^:(-:/:~)@:? first tries to generate values independently of each other, then it tests to see if they are sorted.  If they are sorted, it generates a red,white,blue,red,white... sequence of the same length as the original sequence, and then reverses it.  This "eliminate random values which are sorted" thing seems to be a dubious requirement though, for the test data -- as near as I can tell the main thing it accomplishes is allowing sort implementations which are broken when the data is already sorted. --[[User:Rdm|Rdm]] 13:28, 2 July 2012 (UTC)

== Haskell entry opening paragraph - better without it? ==
It starts:
:''This problem is very easy to solve in Haskell. No algorithms necessary, because Haskell provides everything we need.''
I think that how easy it is to solve in Haskell might depend on the Haskell programmer. It doesn't seem to be the easiest or smallest Haskell solution to any task which don't come with that statement.

The Haskell program ''does'' describe an algorithm for solution.
The third part of the sentence shows a love of the language, but adds no real 'meat'. 
I propose the deletion of the first paragraph as adding little to the example as a whole. --[[User:Paddy3118|Paddy3118]] 07:30, 30 August 2012 (UTC)

:Thanks for the deletion  Avillen. --[[User:Paddy3118|Paddy3118]] 09:12, 30 August 2012 (UTC)

:: Perhaps the Haskell solution should also implement part 1 of the requirements?  Or is acknowledging that a random list was already sorted sufficient?  --[[User:Rdm|Rdm]] 09:17, 30 August 2012 (UTC)

:::Perhaps. Some effort has been made in a unique way and I did not want to discourage someone new to the site. I remain reluctant and wouldn't mind others chipping in (such as yourself). --[[User:Paddy3118|Paddy3118]] 10:23, 30 August 2012 (UTC)

== solved in BATCH/.BAT ==

i just solved the problem in batch or .bat, can someone post it in the section for me, i didnt understand all those html things yet.
i also am not sure if this is exactly what it is because in batch you dont have variables witch can contain more than one variable (arrays.
this is the script:
if you copy it and save it as "title".bat and click run it will start. you have to save it as all files and not as ".txt".
everything with REM is explaining, it wont bother in the script.
i hope that this solves the problem correctly and that my English isn't too bad


REM this part generates variables with the numbers 1-3
REM 1 means red
REM 2 means white
REM 3 means blue
REM variable ball a is the first in row and c is the last in row.
@echo off
cls
goto aset
:aset
cls
set /a R=%random%%% 3 +1
if %r% EQU 1 set a=1
if %r% EQU 2 set a=2
if %r% EQU 3 set a=3
goto bset
:bset
set /a R=%random%%% 3 +1
if %r% EQU 1 set b=1
if %r% EQU 2 set b=2
if %r% EQU 3 set b=3
goto cset
:cset
set /a R=%random%%% 3 +1
if %r% EQU 1 set c=1
if %r% EQU 2 set c=2
if %r% EQU 3 set c=3
goto acheck
REM this part checks if ball 1 is red, ball 2 is white and ball 3 is blue
REM and if they are in row of the dutch flag it will go to the begin again
:acheck
if %a% EQU 1 goto aset
goto bcheck
:bcheck
if %b% EQU 2 goto aset
goto ccheck
:ccheck
if %c% EQU 3 goto aset
goto color1
REM this part gives the numbers colors.
:color1
if %a% EQU 1 set a=red
if %b% EQU 1 set b=red
if %c% EQU 1 set c=red
goto color2
:color2
if %a% EQU 2 set a=white
if %b% EQU 2 set b=white
if %c% EQU 2 set c=white
goto color3
:color3
if %a% EQU 3 set a=blue
if %b% EQU 3 set b=blue
if %c% EQU 3 set c=blue
goto echoert
REM this part shows/echo/prints it, it shows the new dutch flag
REM that is the random generated flag
:echoert
echo the new dutch flag is...
echo %a%
echo %b%
echo %c%
pause
echo and the new dutch flag organised is...
goto posa
REM this part makes sure that variables will be setted when a equals 1/red and when it is red it will,
REM be standing in front of all the white/2 and blue/3 variables/balls/
:posa
if %a% EQU red set posa=1
if %a% EQU white set posa=2
if %a% EQU blue set posa=3
goto posb
:posb
if %b% EQU red set posb=1
if %b% EQU white set posb=2
if %b% EQU blue set posb=3
goto posc
:posc
if %c% EQU red set posc=1
if %c% EQU white set posc=2
if %c% EQU blue set posc=3
goto echopos1
REM this prints/echo/shows all the balls/colors/variables sorted
:echopos1
if %posa% EQU 1 echo %a%
if %posb% EQU 1 echo %b%
if %posc% EQU 1 echo %c%
goto echopos2
:echopos2
if %posa% EQU 2 echo %a%
if %posb% EQU 2 echo %b%
if %posc% EQU 2 echo %c%
goto echopos3
:echopos3
if %posa% EQU 3 echo %a%
if %posb% EQU 3 echo %b%
if %posc% EQU 3 echo %c%
pause
goto aset
REM this resets the script(it will go to top)
