+++
title = "Talk:One-time pad"
description = ""
date = 2016-05-02T21:42:18Z
aliases = []
[extra]
id = 18246
[taxonomies]
categories = []
tags = []
+++

==Clarity==
Hi, apart from the fact that a one-time pad is to be used, this task description does not give enough direction to produce examples that would be comparable, as too much is left unsaid.

You could implement something in a language, check it for the amount of time needed to produce your implementation, (too long and it may take time to gain interest), then the task description could be honed to produce similar and comparable implementations in other languages. (But try and refrain from stating "just follow language X" in the task description.  --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:45, 18 November 2014 (UTC)

:It didn't seem that urgent, to have a very detailed task-description up front, as well a solution.
:After all, the task is not much more complicated as rot13/caesar/vignere etc.
:With a broader scope, somebody might already have some solution ready, or suggestions / requirements to add.
:But I haven't seen much discussion going on about tasks...
:BTW, [[Rosetta Code:Village Pump/Suggest a programming task]] needs cleanup... --[[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 20:35, 19 November 2014 (UTC)

Hi Hajo, the task description needs to be enough to allow people that don't know the subject to follow it and create comparable solutions. Sometimes it goes well, sometimes people with the best of intentions interpret the task description in a different way to produce non-comparable language examples - that is to be avoided and leads to discussion on talk pages and probably modification of the task description.

If the draft task starts with both a "tight" description and a first implementation then this helps it progress to full task status as it aids others in generating their examples. We need to set some standard for draft tasks to stop people from using them instead of the [[Rosetta Code:Village Pump/Suggest a programming task]] page which is where people with the idea for a task, but without the wherewithal to start a draft task with enough umph to propel it to full task status should add their ideas.

Oh, and yep - that task suggestion page probably does need a cleanup. I would not be surprised if some suggestions are available as tasks and so probably should have their (approximate) task match appended. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 01:24, 25 November 2014 (UTC)

== Some cautions ==

/dev/random is operating system specific and often has security constraints on its use (to prevent denial of service attacks).

That means that this task is implicitly operating system specific (don't expect /dev/random to work on Windows, or an iPhone, for example) and even if the operating system is specified, you should expect this code to fail on some machines - perhaps all available examples of machines of the specified operating system.

And, of course, there's no way to guarantee that there are no eavesdroppers watching the one-time-pad being built.

Because of these constraints, either the task or the implementations should specify the operational requirements for the code. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:13, 13 December 2014 (UTC)

:Sure, /dev/random on Unix is easy to use, and accessible from all languages.
:On Windows, e.g. there is the [http://msdn.microsoft.com/en-us/library/windows/desktop/aa379942%28v=vs.85%29.aspx CryptGenRandom_function], which MS claims "fills a buffer with cryptographically random bytes". --[[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 12:45, 13 December 2014 (UTC)

::Looks like you are right - /dev/random is available on iPhone and android. I'm still a bit concerned about the underlying kernel implementation on some of those systems - the whole "entropy management" thing could be better in some cases. Even Solaris has /dev/random nowadays. So it's mostly stuff like Windows, forth machines, specialty hardware, and stuff like that which don't have /dev/random --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:35, 13 December 2014 (UTC)

==So little on the Page ==
We need to get solutions actually on Rosetta Code. If the solution isn't on RC then it is not a solution. RC should not degenerate into a list of off-site links. This is different to cases where people link to needed libraries and /or documentation that may be off site. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:10, 31 March 2015 (UTC)
: Agreed. Rosetta is not a link directory. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 21:42, 2 May 2016 (UTC)
