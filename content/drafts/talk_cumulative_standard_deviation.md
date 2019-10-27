+++
title = "Talk:Cumulative standard deviation"
description = ""
date = 2016-08-21T14:13:49Z
aliases = []
[extra]
id = 4396
[taxonomies]
categories = []
tags = []
+++

== n is always >0 ==
But several examples check for <code>n == 0</code>. I don't think it is necessary in this task (but won't hurt). --[[User:Paddy3118|Paddy3118]] 22:06, 16 June 2009 (UTC)
: It won't hurt, and n is not always > 0. All the code I've done, and even the rest for what I can understand, provide a way of calculating standard deviation and mean, even if the first datum was not injected yet. A smart user won't call the method/function to get the mean if s/he's not put at least one value, but a dummy user could. In the OO cases, some methods shouldn't be public, and standard deviation could not be requested without adding a new value (very odd!), i.e. the only way to get stddev should be adding a new value... so that it would be a "know once and then forget" function/object. So to me it was "natural" to check for n>0, letting the user call mean, variance or whatever also if s/he's not added any value at all, without causing a division by zero problem. Of course for the task purpose it is not strictly necessary (real usability is not a requirement), nonetheless, it won't hurt and it assures that the behaviour is "good" for any input or misusage... --[[User:ShinTakezou|ShinTakezou]] 23:23, 16 June 2009 (UTC)
:: I'm assured by my statistically-versed friends that the Standard Deviation isn't defined for <math>N<2</math> though it's convenient to define it to be <math>0</math> when <math>N=1</math> (i.e., it's trivial to get that from the mathematics if you're asking for the sample deviation, which is what this task is about). —[[User:Dkf|Donal Fellows]] 12:35, 17 June 2009 (UTC)
::: Oh yes, it is so, but I was talking about values num/count can assume, not if the result is meaningful or not when they get that value. --[[User:ShinTakezou|ShinTakezou]] 12:54, 17 June 2009 (UTC)

==Function takes vector/array or single value?==
I took the task description to mean that the function should be called multiple times with a succession of values - hence the need of state between calls. Some think that the function should take an array of values. What to do? --[[User:Paddy3118|Paddy3118]] 11:45, 17 June 2009 (UTC)
:The function should hold state between calls and take a single value at a time. Some people can't seem to resist implementing it wrongly. —[[User:Dkf|Donal Fellows]] 12:31, 17 June 2009 (UTC)
:: That should probably have been called a ''method'' rather than a ''function''. Some people can't seem to resist naming it wrongly? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:31, 15 June 2015 (UTC)
::: I would perhaps call it a static function. Shrug. -- [[User:Eriksiers|Erik Siers]] ([[User talk:Eriksiers|talk]]) 03:57, 16 June 2015 (UTC)

Many examples (too many), have not paid attention to the task description. I've started to mark those but it is a long job...  Please remember that if your language cannot match some aspect of the task description then state this clearly, up front and describe what aspects of the task you can follow. (Note, that is if your language cannot do something - not merely if your example does not follow the task). The J language example goes about it in this way, I guess if you had a "pure" functional language that did not allow functions to save state between calls then you might need to explain this at the beginning of your example as well as what compromises this lead to. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 22:16, 16 June 2015 (UTC)

: Rather than "cannotdo something" I would prefer the more accurate "should not do something". Presumably the focus on the detail of how the incremental process works has something to do with ideas about efficiency? But, depending on the language, this level of micromanagement might make things considerably more inefficient than they would be otherwise - I have seen this kind of issue lead to timing ratios where one implementation would be over 9000 times faster than the other for some test data sets (though smaller differences are also possible and I try not to care when the difference is less than a factor of 2 -- modern machines introduce variations which can easily reverse a "less than factor of 2" performance issue). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 10:33, 17 June 2015 (UTC)

:: '''Unfortunate case of an unfairly restrictive task description'''
:: I too think that mandating a stateful function makes this task needlessly restrictive. If the task were ''called'' "Stateful function" this would be appropriate, but for a generic math-problem task called "Standard deviation", I think it's bad to force solutions into a particular idiom that may be appropriate in some languages but not others - because this effectively bars those other languages from showing off an idiomatic solutions to that math problem on RosettaCode.
:: If there weren't such a huge number of existing solution making changes unfeasible now, I would suggest to:
::* Rename the task to "Running standard deviation" (so that a separate "Standard deviation" task can deal with the simpler case of fixed inputs).
::* Change the task description to something like "''For a stream of values that may come in over time (e.g. from standard input or from another algorithm), calculate and print the running standard deviation immediately after each incoming value''". Leave it open to implementors how to accomplish this. Some may use a stateful function that is called repeatedly with single values, while others may use a function that takes a stream or lazy list as argument, etc.
::It may be too late for this task, but lets try to make sure that in the future, such needlessly restrictive tasks get fixed during draft stage (and before they become big). Tasks asking for specific programming idioms or language features should be clearly named as such - generic math-problem tasks should not be hijacked for that.
::--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 12:03, 17 June 2015 (UTC)

:::The task description ''is'' complete. If one reads the title and skims the task description then of course one is likely to come a cropper. This task is not only about the calculation but also how it is to be done - numbers given to the function singly, intermediate results. 
:::Looking at the failing examples so far they mostly stem from ignoring the task description. I see it as equivalent to asking for a merge sort and someone sticking in a bubble sort and saying "it sorts doesn't it"? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 13:38, 17 June 2015 (UTC)

:::: I think there has been some similar discussion in the context of sorting algorithms. The talk page on [[Talk:Quicksort|quicksort]] has some of that, though not the conversation I thought I remembered. Meanwhile, [[User:Short_Circuit|Short Circuit's]] user page has some of the relevant policy. And of course the [[Help:Adding_a_new_programming_task|Add a Task]] page has some good advice. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:18, 17 June 2015 (UTC)

:::: '''Clarification'''
:::: "<span style="color:green">This task is not only about the calculation but also how it is to be done</span>" - And my point is that a task which is named simply "Standard deviation", ''shouldn't'' be about that. To go with your example: A task called "Merge sort" should demand using the merge sort algorithm, but a task called "Sort an integer array" shouldn't. And neither of them should demand that the sort algorithm be implemented with tail recursion.
:::: IMO there are three types of RosettaCode tasks:
::::# ''Tasks asking for a (mathematical or practical) problem to be solved.'' <small>(Any solution that produces the right output for every input, is accepted.)</small>
::::# ''Tasks asking for a known CS algorithm to be implemented.'' <small>(There is still a decent amount of leeway, e.g. many algorithms can be implemented either recursively or iteratively; state can be stored and propagated through different means; etc.)</small>
::::# ''Tasks asking for a specific programming language "idiom" or feature to be demonstrated.''
:::: The third type of task description is the hardest to do well, because it's easy to fall into the trap of thinking that what makes sense in the few languages one knows, makes sense everywhere - and in my experience such tasks are often unsatisfying to solve. This doesn't mean that creating such tasks shouldn't be attempted at all. However, they should not take away page names from (1) and (2). In particular, pages named after a generic mathematical or practical problem should be reserved for type 1 tasks, and pages named after known algorithms, for type 2 tasks.
:::: In case it wasn't clear from my previous comment, I think that in the example at hand, it would have been better if instead of the type3-ish "Standard deviation" task we have now, we had instead one or more of the following:
::::* "Standard deviation" - Type 1 task - for calculating the SD for a list of numbers, using any means.
::::* "Rolling standard deviation" - Type 1 task - for calculating the ''rolling'' SD's for a sequence of numbers which don't come in all at once, using any means.
::::* "Stateful function" - Type 3 task - for demonstrating that particular idiom.
::::--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 16:26, 17 June 2015 (UTC)

:::: Hi Smls, I read the task as being about doing a standard deviation in a particular way. The title
is expanded in the task description and they do not seem at odds with one another to me. From comparing the wrong answers to the task description it seems that implementers have ignored two phrases ''"a series of floating point numbers, one at a time"'' and ''"returns the running standard deviation of the series."'' by taking all the example numbers at once as a list and by omitting intermediate output and just give one final value. These changes are made without any comment, just ignored. 
:::: With implementors ignoring the task description I think flagging the deviations might prompt updates.
:::: It may be that initial wrong implementations got copied but the page would be much better if people checked their languages and updated their language examples now that they are being flagged. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:36, 17 June 2015 (UTC)
