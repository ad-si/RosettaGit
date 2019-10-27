+++
title = "Talk:Search a list of records"
description = ""
date = 2016-08-27T12:23:30Z
aliases = []
[extra]
id = 19637
[taxonomies]
categories = []
tags = []
+++

== Junk task assault? ==

The BugMeNot junk "task" assault (from behind a login control evasion service) continues :-) 

Someone's desk job to establish beachhead positions ?

:Maybe, but the credentials should not be as important as the content. Deleting due to the use of BugMeNot isn't a very good reason for deletion, although in these cases the content isn't so good either (but may be improvable; it is wiki so anyone can and should make such improvement please). --[[User:Zzo38|Zzo38]] ([[User talk:Zzo38|talk]]) 19:53, 9 October 2015 (UTC)

:: [[User:Zzo38|Zzo38]], thank you for the logical separation of account types and 'task assaults'. I've added a title to this thread, since it was missing. The unsigned poster seems to be [[User:Hout|Hout]] for the record.

:: I'm not sure what is meant by a junk task assault. I use this wiki as a rosetta stone for looking up how to do various programming related tasks in various languages. E.g. is it called a sub, a function or a method in the language i'm using right now. I switch between more languages than what's good for me and while i do remember most things, sometimes it's just easier to look them up, than to 'spend brain cycles' remembering every corner of every language. That's my use case and i hope it's a good one for this wiki. I'm just trying to help and in turn also helping myself in the future, when i wanna look it up again. Am i doing something wrong in that regard? If so, please elaborate, so i can improve. If you improve my edits, i'll hopefully notice and make sure to improve future edits. --[[User:Bugmenot2|Bugmenot2]] ([[User talk:Bugmenot2|talk]]) 07:50, 21 October 2015 (UTC)

== draft task ==

Please see [[Rosetta_Code:Add_a_Task|Add a Task]] and if you are more generally interested, check out the "Ways to Contribute" column on the [http://rosettacode.org home page]. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:49, 10 October 2015 (UTC)

==perhaps 2/3 slight expansions of the task description and requirements ?==

The references to:
:# homogenous vs heterogenous lists, and
:# looking for elements of two different types

both allude to the implicit presence of specific '''matching/equality functions'''.

To get a good Rosetta task, in which comparison across languages makes the hidden convergences and divergences a bit more visible, and intelligible, perhaps ask submitters to show:

:* The behaviour of the implicit equality function used by any built-in search function. (Case sensitive ? Int values = real values ? Expressions evaluated before the match ?)
:* Show a search function  which takes a coder-supplied equality function as an argument. (Finding by a particular property, given a list/array of objects (or otherwise compound elements), for example)
:* A search which returns an index, and a search which return a value.

(As a footnote on apparent divergence and hidden convergence, Haskell, at first sight, doesn't use heterogenous lists, but it does use homogenous lists of compound data types. JavaScript appears to have heterogenous list of elements, which might include a mixture of reals, strings, functions, and objects, but in fact these are all JS objects, some atomic on the surface but all, in fact, compound below the water-line, each with a type/Constructor as well as various properties/values/sub-expressions.  Searching for an item will always involve a function which derives a type-dependent value and applies a particular type of equality check, often involving some kind of reduction, normalisation or coercion.

Search also varies with evaluation strategy – if one element of a list is a function call, in some languages that call will be eagerly reduced/evaluated before the search equality function is applied. In others, such as JavaScript, an .indexOf search will '''not''' match a string like "epsilon" with a potentially equivalent function call like (function () {return "epsilon"})()
It might be helpful to specify requirements which drew these things out and made them visible. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:13, 10 October 2015 (UTC)

: A couple of concrete examples – I think the requirements should at least prompt, in the case of JavaScript ES6,  the supply of a specific equality function by the coder, returning a value in the case of '''Array.prototype.find'''(''callback''), and an index in the case of '''Array.prototype.findIndex'''(''callback''). 
:For JS ES5, it should prompt the use/demonstration of coder-specified equality functions through equivalent 'polyfills'
:In the case of Haskell, the requirements should be such as to require the supply of a predicate function to '''find :: (a -> Bool) -> [a] -> Maybe a'''
:and also to '''findIndex :: (a -> Bool) -> [a] -> Maybe Int'''
: [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:33, 10 October 2015 (UTC)

:: That sounds plausible. Please feel free to update the task description - it's not like you have to worry about messing that up... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:40, 10 October 2015 (UTC)

::: But on second thoughts, do we need this task anyway, given the existing http://rosettacode.org/wiki/Search_a_list ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:43, 10 October 2015 (UTC)

:::: That's good question. One relevant issue might be that [[Search a list]] specifies an index result, while this task allows non-index results. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 05:27, 11 October 2015 (UTC)

::::: A name change here, perhaps ? -> sth like 'Find the first match in a vector' ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 06:44, 11 October 2015 (UTC)

:::::: Population exceeding or population less than ? this is confusing --[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 10:10, 11 October 2015 (UTC)

::::::: First item with population below a certain limit. (less than X) (assumes that this list is sorted (as shown) in descending size of population, and that we are searching from the largest city at the beginning towards the smallest city at the end). Is there an alternative wording that you might prefer ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:21, 11 October 2015 (UTC)

:::::: Maybe add find all city's with pop < 5 million or some such to get multiple results --[[User:Craigd|CraigD]] ([[User talk:Craigd|talk]]) 19:58, 11 October 2015 (UTC)

::::::: I wonder if that would take us from the simplest 'first match' of functions like JS's   '''.indexOf(), .find()''', and '''.findIndex()''', into a broader database query ? Perhaps there's an argument for restricting the scope a little ? Not sure …  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:21, 11 October 2015 (UTC)

:::::::: Normally, in all my professional travels in computer-land, I found that most users want to know   ''all''   the cities whose (say) population is under five million, not just the first city in the list --- which depends on, in most cases for this Rosetta Code task, when each entry (city) was built/constructed    (first come, first served, er, ··· first found).   That's something most people don't care about or even know in which order the cities (entries) were "entered" (constructed).   But, the task requirements are what they are.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:40, 22 August 2016 (UTC)

::::::::: Both finding the first match in a list, and finding all matches, are sometimes useful in practice. You're right that the data set in this task is not the best example of a useful scenario for finding the first match only, but I thought that replacing it with a different data set would be too invasive a change, so I left it as-is.
::::::::: I don't know if there's already a task for finding all matches in a list... If not, I think it should be created as a separate task under the name "'''Filter a list'''". --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 04:58, 23 August 2016 (UTC)

==Long Line==
The long line of json in the task description should be better formatted, or just split I think. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:56, 3 December 2015 (UTC)
: Thanks :-)
--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:40, 4 December 2015 (UTC)

==Grep redirect==
Grep is used for many things in programming - it is a major posix Unix command for example. I don't think it is wise to forward Grep to this task. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:56, 3 December 2015 (UTC)

==Overhauled task==

I've completely rewritten the task description. It should be of much higher quality now, without invalidating existing solutions much (and I've added notice templates where solutions do need updating). I've also renamed the page from "'''Array search'''" to "'''Search a list of records'''", to better represent what it is about and to make it consistent with the existing [[Search a list]] task.


:: Make sure that other Rosetta Code tasks that refer to this one   (via '''Related tasks''' links --- or the new   ''' See also'''   being implemented via some overhauling).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:26, 22 August 2016 (UTC)

::: Yes, I took care of that when moving the page. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 04:52, 23 August 2016 (UTC)

I think in this form it's ready to be taken out of draft status, but I'll wait a while to see if people have any objections, or issues while updating to the new task description.

--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 10:48, 22 August 2016 (UTC)

: It shouldn't be taken out of draft task until most of the programming entries conform to the new and/or changed task requirements and give those programmers a chance to comply and/or comment on the changes and new requirement(s).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:21, 22 August 2016 (UTC)

:: Taking it out of draft status now, as two thirds of the entries already fulfill the updated task description (and the rest basically just need the 3rd test-case added, which should be simple enough). --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 12:22, 27 August 2016 (UTC)

: Also, I much prefer   (for this task and other tasks being changed)   the old form of the task's preamble where all the section headers are in '''bold'''   (which makes it much easier to find the relevant sections).   Also, I think all those   <small> [edit] </small>   thingys just obscure the readability of the task's preamble.   If somebody wants to edit the task's requirement(s), description, related tasks, examples, etc.,   they can use the   <big> ''Edit'' </big>   tab entry   (at the top, just like always).   There's no need to provide an individual   <small> [edit] </small>   button (an HTML field thingy)   for each section individually.   Having five   <small> [edit] </small>   thingys (for this task) interferes with the perusing of the task's preamble.   Also, most     '''Related task(s)'''     section headers have been changed to     '''See also'''     section headers when using the new format.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:21, 22 August 2016 (UTC)

:: Let's take this discussion to [[Template_talk:Task_heading]]. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 04:52, 23 August 2016 (UTC)
