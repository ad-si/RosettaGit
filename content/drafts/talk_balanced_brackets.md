+++
title = "Talk:Balanced brackets"
description = ""
date = 2016-11-04T11:19:19Z
aliases = []
[extra]
id = 9280
[taxonomies]
categories = []
tags = []
+++

==Name change?==
I propose this be changed to "Balanced brackets" to make it more descriptive. --[[User:Paddy3118|Paddy3118]] 18:13, 20 February 2011 (UTC)
:I'm on board with that. --[[User:Mwn3d|Mwn3d]] 20:22, 20 February 2011 (UTC)
:Me too.--[[User:Tikkanz|Tikkanz]] 22:37, 20 February 2011 (UTC)
:: Seems sensible to me. Done. –[[User:Dkf|Donal Fellows]] 23:10, 20 February 2011 (UTC)

==Extension?==
Currently the task specifies that the strings to be tested will all have the same number of opening as closing brackets. Would it be worthwhile to generalize the problem to also include strings with uneven numbers of opening and closing brackets?--[[User:Tikkanz|Tikkanz]] 22:37, 20 February 2011 (UTC)

:The requirement for an even number of opening and closing brackets seems to be an attempt to make the random generator more likely to produce a string of balanced brackets. The algorithms used tend to end with a check that their are no un-closed brackets when you reach the end of the string so a change would not affect them. --[[User:Paddy3118|Paddy3118]] 06:35, 21 February 2011 (UTC)

::Except the J implementation does not include the second test, except as a footnote, since it was not asked for.  --[[User:Rdm|Rdm]] 13:41, 21 February 2011 (UTC)

::The Ada solution returns True rather than Open = 0, so would fail if the string ended without an opening bracket being closed. But that cannot happen from the way the strings are to be generated. --[[User:Paddy3118|Paddy3118]] 13:50, 21 February 2011 (UTC)

:What about characters that are not brackets? I added a Ruby solution that ''rejects'' "[letters]" because the task wants a string that "consists entirely of pairs of opening/closing brackets". But most of the other solutions on the page ''accept'' "[letters]" because they ignore other characters. The generated strings never have other characters. --[[User:Kernigh|Kernigh]] 21:05, 21 February 2011 (UTC)

::According to the task description, as you've stated, you would never get other letters to accept. --[[User:Paddy3118|Paddy3118]] 21:26, 21 February 2011 (UTC)

== Java incorrect ==

What do you want the Java example to do? There's no requirement for using the checker function from the generator function. --[[User:Mwn3d|Mwn3d]] 13:23, 22 February 2011 (UTC)
:Actually there is. I didn't read it that way at first though. I thought the task wanted two separate functions. I'll fix it later today. --[[User:Mwn3d|Mwn3d]] 13:38, 22 February 2011 (UTC)
::I've added clarification. --[[User:Paddy3118|Paddy3118]] 13:45, 22 February 2011 (UTC)

==On "Constructing correctly balanced strings"==
I read the task as being primarily about checking for balanced strings and the generator being part of a check. I was hoping to find comments on how to generate longer strings with equal probability of being balanced or not.
The Tcl interpretation fits too though. --[[User:Paddy3118|Paddy3118]] 09:57, 24 February 2011 (UTC)

: I created the 3<sup>rd</sup> REXX programming example to generate   ''all''   20-character (or less) random possibilities (over 125,000 permutations).   Of course, it didn't list each of the 125,477 unique strings, but it shows a count of balanced ''vs''. unbalanced strings.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:54, 10 September 2015 (UTC)

== usefulness of task ==

I have a use for something like this, but to make it useful, any character not a grouping symbol is essentially ignored, but parsing for quoted strings is done with multiple deliminters allowed, namely the double quote ["] and the apostrophe ['].  

The input strings are essentially statements for assignment to variables.

The allowable grouping symbols being <tt> ( )  [ ]  { }  « » </tt> (and could be expanded). 

Equations of the sort: <tt> xxx=]yyy+zz[ </tt> are, of course, illegal (even though they "pair up").

The equation: <tt> zz='(' </tt> is legal.   

I had a difficult time programming to catch errors like: <tt> xxx = ((a+ [b/c)] ) </tt>.

Such a generalized task, I guess, would better be served with its own task. -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:29, 31 March 2012 (UTC)

==Undiscussed deletions (JavaScript) on June 5 2016==

I notice that a couple of JavaScript contributions were deleted without discussion on June 5 2016, on the grounds (suggested in the edit comment) that they were felt to be 'over-engineered'.

Deletion will sometimes be constructive, but does needs to be discussed. In this case, for example, functional composition of JavaScript code was left unrepresented (the new example was imperative, and the deleted functional example was not replaced).

I have restored, for the moment, an ES6 version of a functional approach, which also provides visual indication of where the brackets become unbalanced. If you feel that it would really be better to delete it again, you are very welcome to make that argument here on the discussion page. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 11:19, 4 November 2016 (UTC)
