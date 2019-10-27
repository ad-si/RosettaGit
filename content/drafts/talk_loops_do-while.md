+++
title = "Talk:Loops/Do-while"
description = ""
date = 2019-10-20T17:28:00Z
aliases = []
[extra]
id = 4841
[taxonomies]
categories = []
tags = []
+++

== self-contradictory task? ==

If value is supposed to start at "zero" and looping is to be performed "while value mod 6 is not equal to zero", then it follows that the loops should be executed zero times since 0 mod 6 == 0. Requiring that the loop should execute at least once is in conflict with the other requirements.[[User:Sgeier|Sgeier]] 04:54, 15 September 2009 (UTC)
: I believe the goal is to demonstrate a post-execute condition check, but I don't have a good idea of precise-yet-clear ways to describe it, and that interpretation of the behavior is specific to imperative programming.  "Process at least once, or while condition is true" might be closer. --[[User:Short Circuit|Michael Mol]] 06:31, 15 September 2009 (UTC)

: <cite>Start with a value at 0. Loop while value mod 6 is not equal to 0. Each time through the loop, add 1 to the value then print it.</cite> It is not clear (maybe the title do-while should suggest that, at least to C-or-similar coders), but the condition is tested at the end of the loop, so that the value has get incremented already (<cite>each time through the loop ad 1 to the value</cite>). Anyway the final <cite>The loop must execute at least once</cite> can be read as: (if needed) fix it so that the loop is executed at least once. --[[User:ShinTakezou|ShinTakezou]] 09:18, 15 September 2009 (UTC)

== is this a task at all ? Certainly not a Rosetta task ... ==

Fortunately the Rosetta scribes were given a task, which they solved across a range of very different types of syntax and writing system – they weren't asked to footle around in the superficial syntax or hieroglyphics of a particular type of language or orthography.

I see no sign of a task here at all – no statement of a problem (just a slightly parochial focus on some surface syntax of a kind which many languages don't even use), and no specification of inputs or target outputs.

Something drifted completely loose from the Rosetta project here ...

Helpful, I think to check off the basic Rosetta goals before signing off on a task – this one needed much more work before it could even qualify as a task.

Look at the landing page and remember the goals:

:#The idea is to present solutions to the same task in '''as many different languages as possible'''
:#demonstrate '''how languages are similar and different'''
:#and to '''aid a person with a grounding in one approach to a problem in learning another'''

'Loops/Do-while' fails to qualify as a Rosetta task in all three respects. (That is, in fact, its main computational achievement :-)

:#Many languages make no use of 'Loops' or 'Do-While'. That is not how they approach problems.
:#Far from showing how languages are similar and different, it appears to assume that all languages will obviously be the same. Short on insight and depth.
:#Too parochial and superficial to help learners build any insight. More of an obstacle to insight than a resource for learning.

What is to be learned ?

:- Use the 3 Rosetta goals as a basic editorial checklist
:- Raise the game on depth and insight
:- Actually state a problem and a task


: I disagree - the task requirements seem well enough defined and the solutions do demonstrate how the languages differ in syntax. There are also a number of solutions in languages that don't have while loops. These are interesting too. [[User:Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]): ??:??, 21 September 2015


:: Absolutely – there is certainly a potential task here, and posters have responded creatively despite its defective formulation. The main problem in this (and in a set of related (aspirant) tasks) radiates out from defects in the way they are framed and named. (A secondary problem here is the lack of a required output, which is a necessary stimulus to richer and more transparent comparability. These tasks should have been (and could still be):
:::#Gathered around a less superficial and more revealing concept and word. Perhaps, for example ''repetition''.
:::#Presented in terms of the general problems that arise with repetitive processes, rather than in terms of "loops". 

::Instead of offering deeper insight and a clear framework for comparability and contrast, the use of a narrowly imperative idiom was procrustean and distorting.

::It should have stated an actual problem (in more general and insightful terms than loops), and an invitation to solve the problem in the manner best suited to each language.

::Instead, it forced several languages into artificial and potentially misleading exercises. The comment following the J example is symptomatic:

::: "'''Though it's rare to see J code like this'''."

::Editorial suggestions:
:::#Raise the game in terms of breadth (relevance to more languages), depth (insight and generality in lieu of superficial syntax fetishism) and usefulness to users (clear problems, good comparability, and encouragement of undistorted code) in the framing of future tasks.
:::#Rename the 'Loop' tagged tasks in terms of 'Repetition', and clearly frame the general problem in each case, without limiting or distorting presumption about how languages should tackle that problem.

:: Perhaps, for example: 
:::Conditional repetition
:::Halting repetition
:::Repetition - special terminating cases
:::Repetition - handling exceptions
:::etc etc. 

:Even more eloquent than the name, however, is a clear example of input and output. (Missing, in this case). [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:21, 21 September 2015 (UTC)


### It is an RC task from 2008 with 123 language examples. 

Hi Hout, I suggest you try writing a few tasks and nursing them through those first eight or so language examples to better understand the issues in writing tasks and fostering community inherent in trying to keep Rosetta Code going. Criticism (both good and bad), doesn't come for free and such experience should help you understand better the tasks and members you judge.

I find a good way of helping task writers is to engage them when they first start a task if I can think of any way to help but with the thought that I am writing to an audience I cannot see who might take offence and my goal is to improve and expand the RC community. This page, for example, is far too old, with far too many existing language examples to expect changes to the task description to lead to coherent examples any time soon. (I remember debating this about some other task in the past). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:21, 21 September 2015 (UTC)

===But a defectively formulated task – from which we need to learn===
:::Paddy3118 The cost of task formulations which fail on the 3 axes of Rosetta's goals (inclusivity, comparative insight, and value to learners – see the landing page) is well illustrated by your own claims, made during your (fortunately unsuccessful :-) attempts to delete a JavaScript ES5 illustration of functional set definition.  Your argument for deletion was that it did not use the 'list comprehension' notation named in the (equally defective, as several others commented) formulation of the task (ES5 JavaScript provides no such notation, but the example code shows how the same problem can be solved with a different but isomorphic notation, and translated the Mathematica example directly).
:::Your claim was specifically that '''"The task is written to show those languages that have the construct"''',  and you had earlier, on the talk page, openly expressed an eagerness to embark on a program of deletions where this narrowly syntactic criterion was not met. Others restrained you, but apparently your eagerness proved hard to contain.
:::Good formulations, consistent with Rosetta's goals, would protect innocent editors such as yourself from the honest feeling that this kind of inadvertent vandalism was somehow "correct" in spite of its deliberate destruction of content that filled a gap, provided insight, and was useful to learners. It would also protect contributors from having their time wasted by (ultimately abortive) interventions of this kind, and would protect readers from losing access to material that is useful on all 3 of Rosetta's axes of value.
:::As for your various implications that the validity of an argument is a function of its authorship: "original members", "criticism not for free" etc etc... Well ... probably more constructive to think about the issues :-) The specious character of the ''ad hominem'' fallacy has been only too well understood for thousands of years :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:00, 21 September 2015 (UTC)

== BASIC dialects – section grouping? ==

Should all the BASIC dialects be grouped in the BASIC section or should those currently grouped instead be made top-level entries too? As of this writing, the following BASIC dialects are not grouped: FreeBASIC, FutureBasic, GW-BASIC, Liberty BASIC, Microsoft Small Basic, PureBasic, and Visual Basic .NET. [[User:Matt El|Matt El]] ([[User talk:Matt El|talk]]) 17:28, 20 October 2019 (UTC)
