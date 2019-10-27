+++
title = "Talk:24 game/Solve"
description = ""
date = 2019-01-01T09:23:35Z
aliases = []
[extra]
id = 4942
[taxonomies]
categories = []
tags = []
+++

==Python Solution incomplete?==
One problem with this solution is that it can only attempt one set of brackets. What if more than one set of brackets is necessary? --[[Special:Contributions/164.67.235.128|164.67.235.128]] 17:19, 1 November 2009 (UTC)
: I had thought of that, but that was after I had a solution for none or any one set of brackets, which I thought was crucial. I couldn't at the time, come up with a set of numbers where the solution needed two brackets, (I didn't try very hard - I was enjoying what I had wrote already); so just left it out. Do you know of a set of numbers that can only be solved with the use of two sets of brackets? --[[User:Paddy3118|Paddy3118]] 18:57, 1 November 2009 (UTC)
: P.S. Gosh you're sharp. No flies on you :-)

:My son just worked out the following result: <code>(9-5)*(9-3)</code>, but putting 9,5,9,3 into the solver made it produce: <code>5 / 3 * 9 + 9</code>. I don't know if a less than two bracket solution exists for every two bracket solution though? --[[User:Paddy3118|Paddy3118]] 21:40, 1 November 2009 (UTC)
:: Paddy, Python supports declarative logic, doesn't it?  Just thought I'd mention it. (It'd be nice to see some more declarative logic solutions and/or tasks, too.) --[[User:Short Circuit|Michael Mol]] 00:23, 2 November 2009 (UTC)

::: Unfortunately declarative logic is only supported by external libraries in Python. --[[User:Paddy3118|Paddy3118]] 06:42, 2 November 2009 (UTC)
:::: If external libraries are un-Pythonic, I suppose solutions [http://xkcd.com/353/ involving antigravity] are out of the question... --[[User:Short Circuit|Michael Mol]] 11:16, 2 November 2009 (UTC)
:::: I think it's perfectly OK to invoke external libraries – you'd have to do that in many other languages anyway – but if that's done, they should be properly declared with the right <nowiki>{{template}}</nowiki> so that it's easy to see what's going on. (Internal libs, by which I mean ones that are included with standard distributions of the language, don't need such extra declarations.) —[[User:Dkf|Donal Fellows]] 11:35, 2 November 2009 (UTC)

::: Sorry I meant: "Unfortunately declarative logic is only supported by external libraries in Python ''and I don't know them well enough''". --[[User:Paddy3118|Paddy3118]] 22:52, 2 November 2009 (UTC)
:::: Hehe.  I didn't mean anything personal by it; I've just got a bit of an undersatisfied fascination with declarative logic. No worries. :) --[[User:Short Circuit|Michael Mol]] 01:31, 3 November 2009 (UTC)


I asked on [http://www.reddit.com/r/math/comments/a0fmi/can_all_twoormore_sets_of_bracket_solutions_to/ math reddit] and found that their are indeed digits such as 1127 that need two sets of brackets for their solution: <code>( 1 + 2 ) * ( 1 + 7 )</code>, so I updated the Python solution to handle this case. --[[User:Paddy3118|Paddy3118]] 05:25, 4 November 2009 (UTC)

== Should we enumerate all solutions? ==

It turns out to take almost no time at all to enumerate all the possible solutions, as  there's only 7680 (5 fundamental parse structures, 24 mappings of digits to the leaves, and 64 mappings of operators to the branches). Hence, is it acceptable to produce all the solutions? Or only the first one found? –[[User:Dkf|Donal Fellows]] 13:53, 2 November 2009 (UTC)

:I actually play the game, where only one solution suffices. You could find all and show how to take the first, or otherwise generate all-or-one solution, but one is all that is ''needed'' I would think? --[[User:Paddy3118|Paddy3118]] 16:02, 2 November 2009 (UTC)

::Fair enough. –[[User:Dkf|Donal Fellows]] 16:29, 2 November 2009 (UTC)

==Ruby and checking for 0?==
Hi, will the Ruby code allow zero as a digit in its checking routine? --[[User:Paddy3118|Paddy3118]] 04:36, 25 November 2009 (UTC)
:Yes. It will also allow numbers greater than 9. --[[User:Glennj|glennj]] 14:20, 26 November 2009 (UTC)

==Clojure, generality, and new to Wiki==
Hello! This is my first Wiki edit, firstly, so I'd appreciate any format-related advice.

I implemented the Clojure version this very late night - what it currently lacks for complete generality are backwards-div and backwards-sub in its operation list and formatting function to make the two tree patterns completely generic to the problem. If there was an elegant, terse way to prevent the application of these on the first pattern, it'd be much nicer.. otherwise much duplicity will result. I wonder if anyone has better symmetry-elimination? Suggestions?
--[[User:Geva|Geva]] 04:04, 5 December 2009 (UTC)
: I don't know Clojure, but you might try posting the code to the page anyway, and add [[Template:In progress]]. --[[User:Short Circuit|Michael Mol]] 07:59, 5 December 2009 (UTC)

: Hi Geva and welcome :-)
: On the formatting - The aim is to have something produce an expression that a player of 24 could understand how to make 24 out of the given digits. You give more than one way, and the solutions look good enough to convince a player to me. --[[User:Paddy3118|Paddy3118]] 10:18, 5 December 2009 (UTC)

== Use 1 1 2 7 in example runs? ==
Hi, After finding out that the digits 1 1 2 and 7 ''need'' the use of two sets of brackets for its (infix) solution (see above), maybe all examples might like to show how they handle this case? --[[User:Paddy3118|Paddy3118]] 06:22, 6 January 2010 (UTC)

: The OCaml solution fails on this example as well as 2 2 5 7. --[[User:Andrew Gacek|Andrew Gacek]] 05:32, 27 July 2010 (UTC)

== Negating the first number? ==
I was thinking about making a solution for this in ABAP, and I was wondering if a negated first number is valid?

E.g.
-6 + 6 * 6 - 6

I realise 6 + 6 + 6 + 6 is an alternative solution for this, but I'm curious nonetheless.
--[[User:Rjf89|Rjf89]] 00:28, 6 January 2011 (UTC)

: Sounds fine to me, although thats leads to the question: "Is there a set of digits where a leading minus is necessary"? I don't think it is necessary - but do think it is allowed. --[[User:Paddy3118|Paddy3118]] 06:04, 26 January 2011 (UTC)

:: This leads to another question: what operations may be considered part of an [http://www.webopedia.com/TERM/A/arithmetic_expression.html arithmetic expression]?
::: wp mentions ''"Addition, subtraction, multiplication, or division, and sometimes other operations"'' so there are probably variants that use a different mix of operators, but our game specifically mentions just those first four. --[[User:Paddy3118|Paddy3118]] 21:23, 27 January 2011 (UTC)

== Use 3 3 8 8 in example runs? ==
I have just updated the Python example to use precise rational arithmatic whenever a division is involved as the solution with the digits 3 3 8 and 8 of 8 / (3 - 8 / 3) does not evaluate to 24 when using floating point. You might have to switch too (although the task description does not mandate this)! --[[User:Paddy3118|Paddy3118]] 06:00, 26 January 2011 (UTC)

: I just noticed that my REXX program also suffered the same weakness.   I have corrected the comparison logic and added support to find solutions for such numbers   (that needed rational arithmetic or something similar).    I also agree that '''3388''' should be one of   ''those''   numbers to be tested for program validation.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:22, 1 January 2019 (UTC)
