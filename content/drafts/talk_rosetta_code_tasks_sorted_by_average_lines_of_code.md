+++
title = "Talk:Rosetta Code/Tasks sorted by average lines of code"
description = ""
date = 2014-09-10T17:45:47Z
aliases = []
[extra]
id = 12390
[taxonomies]
categories = []
tags = []
+++

==Delete time!==
Unless there are further convincing arguments in the next hour, I am thinking of deleting the draft task and this talk page. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:45, 10 September 2014 (UTC)

==The Rest...==

I've no 1st demo, sorry. I hope RC may provide this list from the site itself in the future, thank you. [[User:Dingowolf|dingowolf]] 15:09, 15 November 2010 (UTC)
: Might I suggest bringing this up over in the [[Rosetta Code:Village Pump|Village Pump]]? (That page is currently under reconstruction, but it's still mostly serviceable; someone (probably me) will fix anything that needs tidying if you add a post.)--[[User:Short Circuit|Michael Mol]] 15:33, 15 November 2010 (UTC)
::Oh.. I don't aware that Village Pump. I often go to Recent Change as my 1st page of RC, then jump to task/talk. IIRC, last time I visited VP, VP was a seperated blog from RC. It seems it changed a lot.
::As of bringing this task to Village Pump, I would not do so for now, but welcome anyone take over this as a project in VP. 
::I recently has an idea to make a greasemonkey [http://userscripts.org/ userscript] to create a side-by-side panel for comparing different language example codes, Village Pump would be a good place to discuss if further optional extension/function would be possible :) [[User:Dingowolf|dingowolf]] 00:18, 16 November 2010 (UTC)
::: Check out [[Rosetta Code talk:Village Pump/Javascript Functionality Add]] and [[Rosetta Code:Village Pump/Javascript Functionality Add]]. I think you're going to enjoy the site. :) Also, I just redesigned the main page, so you might find that of interest, too. --[[User:Short Circuit|Michael Mol]] 01:56, 16 November 2010 (UTC)

==Modification?==
How about modifying the task to count something like, either:
# The number of chars in the wiki text divided by the number of cases of '{{header|' strings found
# Or: the number of characters in the (X)HTML divided by the number of entries in the contents table (when present).

P.S. As currently written, the page reads like a discussion for a task rather than a draft task.

--[[User:Paddy3118|Paddy3118]] 18:01, 15 November 2010 (UTC)
:I perfer #2 as it is much simpler. Then, by modifying the task so will the title change. Does it need to create another task? [[User:Dingowolf|dingowolf]] 00:18, 16 November 2010 (UTC)

== is this a good idea? ==

I thought we wanted to de-emphasize mechanical issues and emphasize comprehensibility issues here.

Is it fair to be making official wiki tasks which emphasize mechanical metrics?  --[[User:Rdm|Rdm]] 19:31, 16 November 2010 (UTC)

:I can alter these metrics quite a bit. The verbs in MUMPS can be shortened to 1 or 2 characters, I can take out all comments, and I'll use single letters for variables, and naked references for globals (stored data). I can even take out carriage returns in a lot of places (254 character lines are just fine). But then, the programs aren't anywhere near as legible, and thus we lose some of the reason for, in effect, re-inventing the wheel time and time again. These metrics might be amusing, but I think that they would be a disincentive to providing a tool for understanding. [[User:Stormneedle|Stormneedle]] 21:33, 16 November 2010 (UTC)
:Generally, I don't think it's a good idea. I don't want to incentivize contributors to golf. If I had to find a way to rationalize it, I'd suggest creating it as a separate type of solution. On Rosetta Code, the default intended solution is one that it comprehensible and/or idiomatic, at least to the nature of the language. (Some languages' accepted practice is write with the assumption that it won't be modified.) Again, if I had to rationalize accepting golfing, it would be by setting up golfing as a part of a solution type system, and treating each type differently in the "unimplemented in X" pages. I ''have'' been thinking about golfing lately, but not as a line or character count, but along lines of how golfing can be used to show language expressiveness when applied to a problem domain. That would require a more symbolic analysis of code, rather than simply line or character counting.
:
:Also, I don't know how they're going to calculate the ratings of [[APL]], [[Piet]], [[SNUSP]] or [[Brainf***]] examples...
:
:If the question is calculating the relative difficulty of tasks, then I think a much better approach would be to apply semantic properties to all task descriptions. In that way, unimplemented tasks could be sorted by some component of the task's domain, which would allow a perusing coder to find tasks closer to their level of understanding. --[[User:Short Circuit|Michael Mol]] 02:18, 17 November 2010 (UTC)

::I had not previously recognised that the suggested task could degenerate into promoting code golfing. Now I know, then I too must vote against it as I would rather not promote examples being modified for reasons of adversarial brevity. --[[User:Paddy3118|Paddy3118]] 05:00, 17 November 2010 (UTC)
::: It's not to say I'd be against the creation of this task; I find mechanized processing of RC's public APIs and data interesting in itself. I'd simply have to caution against using the metric as anything meaningful, and point out that we have to be on-the-spot with ENA templates and ensuring that comprehensible solutions exist whenever applicable to the task. --[[User:Short Circuit|Michael Mol]] 13:37, 17 November 2010 (UTC)
:::: Actually, on reflection this task seems to be designed to minimize golfing effects:  The average lines of code for a task do not say anything about any specific language and instead are a reflection of the task itself.  On tasks with little support, one language would contribute a large part of the average, but on more reasonable tasks the contribution of any one language would be fairly minor.  That said, I do not think we currently have markup which would allow code to distinguish between implementations and examples -- a reasonable approximation might be to treat the first {{nwtag|lang}} block as the implementation and ignore the others?  --Rdm
::::: I think you're correct in that it attempts to use code length as a measure of task complexity. That's why I mentioned applying semantic description to the tasks themselves as a basis for better browsing and measurement.  As far as parsing the code...I think code sample executed output is typically done using {{nwtag|pre}}. Also, this would be a good time to point out that [[mw:Extension:Page Object Model]] has been installed on RC for a few weeks, now. --[[User:Short Circuit|Michael Mol]] 00:48, 18 November 2010 (UTC)

: I'm going to disagree with the others on this.  If the task was to compare ''languages'' there would be real danger of descending into code-golf, but as long as it's just ''tasks'' I'm not worried.  (If you're trying to promote language X, it's not clear how it helps to make the average codelength for a task longer or shorter.)  Now there may be other ways of comparing difficulty of tasks but I think that this has value even with others implemented. [[User:CRGreathouse|CRGreathouse]] 14:03, 28 December 2011 (UTC)

== Alternatives ==
Since the purpose of this section is to discuss how to gauge the complexity of a task, let's start a discussion on alternatives. Why not just allow a vote or something similar in the discussion area of a task to gauge the complexity on some predefined scale: say 1 to 10? --[[User:Rob.s.brit|Rob Britton]] 14:17 18 November 2010 (UTC)

: Ok...
::  Instead averaging just lines of code, we could also average character counts and token counts (with a comment being 0 tokens).  Or we could count the average number of lang blocks.
::  Since token formation rules are language specific we could ask that each language only measure task implementations for that language, and/or we could focus on a specific language.
::  Instead of averaging over all tasks, we could average over a sampling of tasks, either randomly selected or preselected.  We might also consider independent sampling groups (multiple averages based on different selections).
::  Instead of averaging totals, we could use average other aggregate measures.  Perhaps unique token counts or averaging the number of occurrences of each character.
::  Instead of using arithmetic mean as our averaging mechanism, we could use some other kinds of average (median or mode, or geometric mean, or ...)
::  Instead of finding average we could find variance.
::  Since we are talking about tasks here and not implementations -- some implementations are incomplete while other tasks may have multiple implementations for the same language label -- perhaps we should instead compare lang blocks to surrounding text?
: That's all I can think of at the moment, but I am sure that there's other possibilities. --[[User:Rdm|Rdm]] 13:13, 15 October 2012 (UTC)
:: I think rating the task manually is the best: beginner, intermediate, advanced, or 1 star, 2 stars, 3 stars. If you do want to automate it, then the correlation between the number of contributions and the amount of time (perhaps months) a task has been published is probably the best measure. Put up a simple task and you'll get lots of contributions, put up a more difficult one and you'll get fewer. [[User:Fwend|Fwend]] 11:01, 15 February 2013 (UTC)
::: There's some justice to that, especially in the context of inadequately described and/or ambiguous tasks. That said, tasks often undergo revision, and there can be other influences on task implementation (such as publicity). --[[User:Rdm|Rdm]] 13:33, 15 February 2013 (UTC)
