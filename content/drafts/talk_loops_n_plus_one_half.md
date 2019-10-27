+++
title = "Talk:Loops/N plus one half"
description = ""
date = 2015-09-19T18:17:06Z
aliases = []
[extra]
id = 4951
[taxonomies]
categories = []
tags = []
+++

I noticed that many implementations don't run the loop 9.5 times. Instead a check is made whether the comma has to be printed. This way the loop still runs exactly 10 times, with a different action taken depending on the loop variable.
:This is pointed out in the description:
 in the last iteration one executes only part of the loop body
:Notice the quotes around n+1/2 showing that it's not literally n+1/2 times, but rather the last time through only part of the work is done. Running the loop 9.5 times isn't a requirement, but if you can get your language to do it then go for it. --[[User:Mwn3d|Mwn3d]] 21:09, 4 November 2009 (UTC)

==Task name doesn't make sense?==
You are asked to print the numbers one to ten with separating commas. Surely n == 10. And so you want to go through a loop doing the same thing n minus one, i.e 9 times; then do something else the n'th time through the loop, as many of the examples do.

A more correct name might be '''"Loop/n minus one half"''', but even better might be '''"Looping with a different action on the last iteration"''' - its not as twee, but more accurate? --[[User:Paddy3118|Paddy3118]] 06:10, 5 November 2009 (UTC)

: Actually it turns out I mis-remembered the name of the loop pattern. The correct name seems to be: "Loop and a half".
: "Looping with a different action on the last iteration" would be wrong (and actually pointless, because then the natural thing to do would be to just do the different thing after the loop). After all, the action to do is the same as the action on previous loops, except that on the last iteration not ''all'' of the loop body is executed. --[[User:Ce|Ce]] 17:25, 5 November 2009 (UTC)

:Hi Ce, if what you are after, is to get a conditional within a loop that causes only one of two actions to be done for the last iteration of the loop then the task descriptions use of the term "n+1/2" and even a revised name "Loop and a half" suggest looping (n times), then doing something else, which is counter to the rest of the description. The pattern name is counter to the actions required. --[[User:Paddy3118|Paddy3118]] 05:47, 6 November 2009 (UTC)

:: While "n+1/2" was a mis-remember from my side, "Loop and a half" seems to be the common name of the pattern. You may not like it, but being the commonly used name, it's the one to use.
:: Also note that in most languages (namely all having a "break" or similar) arguably the best solution is not to make the second part a conditional, but to make a conditional break in the loop (because that more clearly communicates that you leave the loop afterwards). Of course, SESE purists may differ. --[[User:Ce|Ce]] 13:53, 6 November 2009 (UTC)

::: A little googling found me [http://academicearth.org/lectures/the-loop-and-half-problem this lecture] on the pattern. Is a while true loop necessary? Is the break necessary? If so, then we still need to work on the task description to make this clear. --[[User:Paddy3118|Paddy3118]] 18:11, 6 November 2009 (UTC)

The other problem is defining "best". For example, the third COBOL posting uses GO TO. It's really fast, but is it the "best" way of doing it? [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 15:45, 29 May 2013 (UTC)

With the (obviously unfair) benefit of hindsight, it's clear (not least from the various sighs and qualifications attached to a number of the solutions that were submitted here) that this is one of those defectively formulated tasks from which something needs to be learned. What went wrong ?

:'''The assumptions were (inadvertently of course) too parochial'''
:: The solutions in '''ACL2''', '''Haskell''', '''IDL''', '''J''' and '''R''' all came in with comments to the effect that using 'loops' would constitute a suboptimal, irrelevant or even perverse approach to solving the problem of special cases at the end of series or patterns. "''No one would ever use a loop in IDL to output a vector of numbers''" comments one submission. Other submissions, like those in '''Closure''' and '''Scala''', don't bother with explicit comment. but either quietly submit a more appropriate non-iterative solution to the problem (sometimes in addition to the artificially 'loopy' one), or just ignore the inadvertently parochial formulation, and submit an appropriate (loopless) solution.
:'''The nature of the task was framed too superficially for a Rosetta context'''
::Real Rosetta tasks solve problems. What was needed was a clearer formulation of what the problem actually was. The prominent position (at the start of the task name) of an unreflecting assumption about what the '''solution''' would look like was symptomatic of insufficient clarity about the nature of the '''problem'''. 
:As a result the framing of the task slipped away from the core Rosetta goal:
::''"The idea is to present solutions to the same task in '''as many different languages as possible"''''' (see the landing page). This is the heart and value of the Rosetta project, and the "loop" formulation of the problem was an immediate failure in terms of inclusivity, depth, and relevance.
:::#It may well have discouraged users of some languages from submitting
:::#It risked encouraging literal-minded and over-zealous editorial deletions and admonitions.
:And it was inconsistent with a core Rosetta principle:
:[[Rosetta_Code:Add_a_Task#Task_focus|Task focus]]
:'Using a hammer' is not a task. 'Felling a tree' is a task.  The formulation here is really in the genre of "''What is the best way of using your hammer to fell a tree ?''" As if the Rosetta audience was specifically imagined as a community of hammer-users rather than as a community of foresters (or forest dwellers) in general.

So what do we learn ? Identify the underlying '''problem''' (not the superficial syntactic glitch), and frame a task as the '''solution''' of the deeper problem.  If you unreflectingly assume that all languages should naturally be written in hieroglyphics (what else, for heaven's sake ?), you will not get much use from the Rosetta stone that results . If you assume that a special case at the end of a series or pattern must naturally involve a loop, then you will both reduce the submission count, and also risk fruitless bickering and contested deletions at the editorial gates. Users will not benefit. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:17, 19 September 2015 (UTC)

== Number of items may not be known ==

Most (all?) solutions shown so far rely on the number of items (10) being known. Rexx version 3 does not! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:43, 24 January 2015 (UTC)

: Indeed, and most languages allow that sequence of characters ('10') to be replaced by another sequence of characters (perhaps 'x') and also provide a substitution mechanism that allow you to specify (in another context) what value to use for your new symbol.

:Some people would even claim that this concept of substitution is the basis of computer programming. (Others, more practical, would get on with whatever they feel they need to do.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:17, 24 January 2015 (UTC)

:: I don't get your point!--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 13:36, 24 January 2015 (UTC)
