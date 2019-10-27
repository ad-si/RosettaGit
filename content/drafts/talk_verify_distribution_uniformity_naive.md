+++
title = "Talk:Verify distribution uniformity/Naive"
description = ""
date = 2010-02-06T14:52:34Z
aliases = []
[extra]
id = 4681
[taxonomies]
categories = []
tags = []
+++

We really ought to use a [[wp:Pearson's chi-square test|chi-squared test]] for this, as that can be made self-calibrating. After all, we've got the tools for calculating the [[Gamma function]], needed for generating the related distribution for a single random variable. Too early in the morning for heavy math for me though… —[[User:Dkf|Donal Fellows]] 06:16, 8 August 2009 (UTC)
:After reading your link, and being up early creating the task in the first place (I'm in Bristol), I also would not want to tackle the maths ;-)
::Please, feel free to add another task to run a chi-square test on the results of [[Seven-dice from Five-dice]], but write the task in such a way that enough languages would be able to compute it if possible. (But then, if mathematica or R have a built-in function, shouldn't they be able to shine)? --[[User:Paddy3118|Paddy3118]] 07:27, 8 August 2009 (UTC)
::: And why shouldn't they shine at something they're good at? —[[User:Dkf|Donal Fellows]] 11:56, 8 August 2009 (UTC)
::: Now over at [[Verify Distribution Uniformity with Chi-Squared Test‎]]. Go knock yourselves out. ;-) —[[User:Dkf|Donal Fellows]] 12:15, 9 August 2009 (UTC)
==What is Delta?==
It would be nice if the interpretation of the delta parameter were more clearly specified.
I don't feel comfortable improvising. —[[User:sluggo|Dennis Furey]] 21:54, 8 August 2009 (UTC)

:"...check bin counts are within +/- delta % of repeats/bincount" (From the Python example). 
:I kinda knew that people with more experience probably wouldn't do it that way, (See the Chi-square comment above); but thought that if you took a fixed sample of a million, any fitness metric should be able to be translated into this form, so went with it. I have no idea of what is good-enough, and also didn't want to parrot some figure of fitness that I did not understand. --[[User:Paddy3118|Paddy3118]] 05:54, 9 August 2009 (UTC)

:: Of course a random number checker like this will report a perfect random distribution if your "RNG" returns the sequence "1234567123456712345671234567...". BTW, shouldn't the function also be given info about what values are to expected? Because if a random number generator intended to emulate a normal six-sided dice actually returns a flat distribution of numbers from 1 to 7 or from 1 to 5, it's certainly not very good :-) --[[User:Ce|Ce]] 21:26, 24 September 2009 (UTC)

:I guess I knew that greater minds would tear the statistical foundations of this task apart. But that is why I put simple in the task name. The really clever alternative is to write something both more accurate ''and'', easier to understand ;-)

:--[[User:Paddy3118|Paddy3118]] 01:46, 25 September 2009 (UTC)

== Renamed ==

I renamed this task so that I can put in a task (or tasks) that does a more sophisticated job and which will give various languages' statistics support a better workout. —[[User:Dkf|Donal Fellows]] 11:47, 9 August 2009 (UTC)
