+++
title = "Talk:Test a function"
description = ""
date = 2010-02-06T15:04:50Z
aliases = []
[extra]
id = 4392
[taxonomies]
categories = []
tags = []
+++

How about just write a test for the palindrome task, if your language doesn't have large testing frameworks?  

Shouldn't tasks lead toward implementation rather than omitting? --[[User:Tinku99|Tinku99]] 22:52, 15 June 2009 (UTC)

:I'm tempted to disagree with such a change. Having a testing framework makes it easier to test the language implementation itself; they're often among the biggest consumers of such frameworks. So, implementing a framework (which doesn't actually need to be ''that'' complex) that lets you satisfy this task means that you can have a higher-quality language implementation. View it as a hint for how to make the jump to the next level of software engineering quality. (For example, [[Tcl]]'s test suite exercises virtually the entire language implementation, including a very large fraction of the failure paths, and it makes it much easier to know early that maintenance hasn't screwed things up.)

:On the other hand, a test framework really doesn't need to be more than a few functions; one to run tests, one to compare results for equality, one to print a summary at the end. —[[User:Dkf|Dkf]] 23:36, 15 June 2009 (UTC)

:Hi Tinku, languages don't have to have ''large'' testing frameworks. The task is worded to point out languages that don't have ''any'' well known testing frameworks, and give those that do, some small chance to show what is extra needs to be done in testing a small function. --[[User:Paddy3118|Paddy3118]] 04:03, 16 June 2009 (UTC)

:: Since testing frameworks don't have to be large, Why not simply prefer a well known example and just allow a quick implementation if there isn't already one?  I will post a little example in autohotkey.  --[[User:Tinku99|Tinku99]] 16:58, 16 June 2009 (UTC)
:::Hi again Tinku, the Autohotkey fits the task description nicely. You state that there is no well known tool before creating your own. I put in the bit about a well known testing library so people can judge whether they will have to 'roll their own'. --[[User:Paddy3118|Paddy3118]] 21:59, 16 June 2009 (UTC)
