+++
title = "Talk:Dinesman's multiple-dwelling problem"
description = ""
date = 2016-11-20T13:20:54Z
aliases = []
[extra]
id = 9966
[taxonomies]
categories = []
tags = []
+++

==Weak signal-to-noise ratio in the task description. Please improve and remove this message==

: The quality of a formulation is the amount it reveals, divided by the processing effort which it imposes on readers. In short – the ratio of signal to noise. 
: The ratio in this task description, on the issue labelled 'flexibility', is demonstrably just a bit too weak and dysfunctional (in its present form) to actually work.
: The significant number of "please XYZ and remove this message" notices on this page are are expressions of:

:'''1.''' Deficiencies in the task formulation 
:'''2.''' Limited awareness of those deficiencies on the part of the author(s) of that formulation 
:'''3.''' Projection of those deficiencies, and that lack of awareness, onto the hapless contributors of solutions.

: I think it might be constructive to summarily remove all the current 'compliance' notices. (The effort that would be required to work out what is really being asked for beneath that weak formulation seems rather unlikely to prove even remotely commensurate with the rewards). 
: If there is felt to be any real light under the 'flexibility?' bushel, then perhaps the bushel needs to be removed by a fresh rewording, (with a better ratio of signal-to-noise) and a couple of examples. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:20, 20 November 2016 (UTC) 

==Why stated like that?==
Remember those maths essay problems from school? Remember your teacher saying "It's not just getting the right answer that is important - it's how you got to it that gets the full marks"? Well this, I hope, is that kind of problem.
The idea is for examples to be able to vary names, numbers, constraints; and for the example's problem statement and answers to be easy to recognise from the task's problem description (or variants of). 
If someone wants to implement natural language processing, be my guest. If someone wants to use a syntax more familiar to constraint programmers then that can be done too, (but think of the likely audience). --[[User:Paddy3118|Paddy3118]] 08:37, 25 June 2011 (UTC)

This is one of the few tasks that I have set where I don't have a canned answer in Python before setting the task. Well, not really; Pythons solution in [[Amb#Python]] is easilly adapted. --[[User:Paddy3118|Paddy3118]] 08:42, 25 June 2011 (UTC)

: Heh there's no way to reliably parse English text, unless the text is restricted to be following some syntax rules.  The question is rather, are you more interested in seeing how people provide methods for flexible input, or how people solve the dwelling problem? Which is the emphasis of this task? --[[User:Ledrug|Ledrug]] 08:46, 25 June 2011 (UTC)

::I thought finding a solution is no problem - brute force it if you must. The emphasis should be on stating the problem and presenting the result w.r.t. the problem as stated. I tried to give hints that parsing the text of the problem statement allowing for variability, as well as stating what that variability is, is an avenue one might want to follow. The right answer is <u>expected</u>. How you get to it is very important. --[[User:Paddy3118|Paddy3118]] 17:20, 25 June 2011 (UTC)


:About the python solution: parsing natural language is FUTILE unless you specify a clear subset of English language you are going to use.  Is your program able to parse "A lives somewhere below B"?  Or "A's floor is no higher than 4"?  Or if one of the characters involved is in fact named "Mr. Floor?"  I don't think one should go on writing a parser without a clear spec of what text might be involved. --[[User:Ledrug|Ledrug]] 00:47, 26 June 2011 (UTC)
::Hmm [http://oxforddictionaries.com/definition/futile futile]? I think not. But I did recognise a need to state the extent of the language recognised but chose, out of expediency, to instead state some of the variations allowed and give a single extra example. The type of variation allowed and showed in the Python example, such as not relying on fixed names is more than that shown in some other examples. --[[User:Paddy3118|Paddy3118]] 01:13, 26 June 2011 (UTC)
:::A proper task needs to specify what's allowed and what's not in the input text.  The "variation" the python script can cope with are really not much variation at all, consider "Fletcher lives on a floor not adjacent to Miller's, but rather Smith's".  Parse <i>that</i>, just the name part, then consider how many other ways that can be expressed in.  Writing a parser for a known string isn't really all that useful --
:::And we haven't gotten to the part of optimizing the code itself yet: what happens when there are twenty tenents? J code runs out of memory, every other one takes eaons to complete, if at all.  But that's another can of worms. --[[User:Ledrug|Ledrug]] 01:26, 26 June 2011 (UTC)
::::Re-read my first submission on this talk page, written before your C example. It is the very nature of this task to permit "artistic license" and to encourage, but not limit the solutions to examples that parse the problem statement - that is why it is mentioned both here and in the task statement. The task asks for allowing some variation and to show how "easy" it is to vary. 
I too found it difficult, at first, to give much more than the right answer to maths essay problems. --[[User:Paddy3118|Paddy3118]] 06:10, 26 June 2011 (UTC)

:::Maybe it's just me failing to understand why anyone would want to write a program that can only deal with some text already known, and can't be reasonably expected to work on much of anything else with a similar nature--kind of defeating the purpose of programming.  But if you find it fun, good for you. --[[User:Ledrug|Ledrug]] 06:26, 26 June 2011 (UTC)

==Flexibility==
What's wrong with the flexibility in the PicoLisp solution? It says

```PicoLisp
(not (topFloor Baker @Tenants))
(not (bottomFloor Cooper @Tenants))
(not (or ((topFloor Fletcher @Tenants)) ((bottomFloor Fletcher @Tenants))))
(higherFloor Miller Cooper @Tenants)
(not (adjacentFloor Smith Fletcher @Tenants))
(not (adjacentFloor Fletcher Cooper @Tenants))
```

In which way is this less flexible than, say, the C version, where the parameters are kept in macros and constants?--[[User:Abu|Abu]] 07:38, 9 August 2011 (UTC)

: I think the tag is there because there's no discussion about the solution's flexibility, not because the solution lacks flexibility. --[[User:Ledrug|Ledrug]] 18:41, 9 August 2011 (UTC)
: Ledrug has it right. You need to comment on the flexilility of the solution. It was not flagged because it was not flexible just because its flexibility was not explained at all. --[[User:Paddy3118|Paddy3118]] 19:27, 9 August 2011 (UTC)
:: I still don't get it. Where is the comment on flexibility in the C solution? --[[User:Abu|Abu]] 06:25, 10 August 2011 (UTC)
:::That would be the paragraph after the example output.  It's just something explaining how to modify the code to deal with similar problems in case the code is not clear enough.  To be fair, it's pretty obvious how to adapt the PicoLisp code to different input conditions, but it wouldn't hurt to write a sentence or two to explain it to readers who are unfamiliar with Lisp. --[[User:Ledrug|Ledrug]] 07:12, 10 August 2011 (UTC)
:::: Ah, sorry, my fault. I didn't look down far enough, too much focused on the code. --[[User:Abu|Abu]] 09:08, 10 August 2011 (UTC)

==Second D entry==
After adding the second D entry, would someone, (Bearophile?), add comments on the examples comparative flexibility? --[[User:Paddy3118|Paddy3118]] 06:44, 24 January 2013 (UTC)
