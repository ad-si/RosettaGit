+++
title = "Rosetta Code:Village Pump/Grouping tasks"
description = ""
date = 2011-05-19T12:01:29Z
aliases = []
[extra]
id = 9450
[taxonomies]
categories = ["task", "something goes here"]
tags = []
+++

## Task

{{Vptopic
|topic=Grouping tasks
|summary=A request to categorize some programming tasks
}}
I would like to group these tasks in <nowiki>[[Category:Higher-order functions]]</nowiki>.

* [[First-class functions]]
* [[Function composition]]
* [[Higher-order functions]]
* [[Partial function application]]
* [[Y combinator]]

However, the system of categories under [[:Category:Solutions by Programming Task]] is to difficult for me to understand, so I will not yet group these tasks. I just learned that [[Template:Task]] takes a parameter (as <nowiki>{{task|something goes here}}</nowiki>). --[[User:Kernigh|Kernigh]] 17:31, 20 April 2011 (UTC)

:Yeah I had tried to organize the tasks long ago, but it didn't work as well as I had hoped. It would be a little easier to understand with the category tree extension, but it still wouldn't be that good. The argument for the task template is for a category to put it in. I say go ahead and make the category. I think people use [[:Category:Programming Tasks]] in general anyway, so it won't confuse people. If you can think of any more groups go ahead and suggest them here. Maybe we can give the organization another shot. --[[User:Mwn3d|Mwn3d]] 17:54, 20 April 2011 (UTC)

:BTW. I like the classifications of function-functions above.  --[[User:Dgamey|Dgamey]] 22:50, 18 May 2011 (UTC)

:I did a quick analysis of classifications on RC [[Icon%2BUnicon/Analysis_of_UnimplementedTasks#Analysis_of_Programming_Categories_on_Rosetta|Analysis of Rosetta Programming Categories]] as part trying to understand how the classifications evolved (for another purpose).  This may be of use. Feel free to copy it for this purpose.  
::It seems to me that there are opportunities where consolidation is in order.  Do we really need Prime, Primes, and Prime Numbers?  Would we want to fix and have redirects?
::It's also clear that things have been misfiled over time. 
::And there a quite a few with one member in the category as well.
::I started to look at Crypto classes at [[Talk:RSA_code#New.2FModified_Tasks_and_Categories]] as well.
::I know that SML may change the way this gets approached.  I haven't made any progress on that myself, anyone else?
:Thoughts?  --[[User:Dgamey|Dgamey]] 21:38, 17 May 2011 (UTC)

==Phantom Categories==
:I was trying to test cleaning up some categories of tasks and thought I'd start with Primes, Prime, and Prime Numbers.  So I added the Category:Prime Numbers to the page but low and behold I can't find where [[Truncatable_primes]] references ''Primes''.  In the html source there is a "wgCategories=[" inside a script and I can see at the bottom where a "Category:Primes (page does not exist)" is generated but I can't find where to fix this.  This also appears this is the case with the single member of the ''Prime'' category.  Help?  --[[User:Dgamey|Dgamey]] 10:31, 18 May 2011 (UTC)

: Bingo!  These appear to be caused by the incorrect use of Library templates.  Rather than reference the library as a whole, people are referencing the module within it creating an entire category for a single non-unique member.  Not sure if it's just careless use or if there isn't enough supporting documentation in the template.
:: So far I've seen this in D, Haskell, and Ruby code.
:: I haven't yet started to go around marking these examples incorrect yet.  Likely I will after its been looked into a bit more.
:: Question, is there an easy way to get a list of all pages using the Library template?  Or is this an easy bot (if the problem is large enough - mark Library references to non-created pages in error)?
: --[[User:Dgamey|Dgamey]] 11:45, 18 May 2011 (UTC)
::Go to the template page and click on the "what links here" link on the bottom left. That will show you all the pages that use a template (as well as ones that link to it directly). --[[User:Mwn3d|Mwn3d]] 12:25, 18 May 2011 (UTC)

==Library/Libheader appears to be being used incorrectly==
:: I really think more is going on here and somethings aren't working as expected.  Cleaning these up will need to be part of any regrouping effort.

::A comment on one of the pages in question suggested the library may be correct and that disambiguation may be need like Library: prime (language).  I'm not convinced if there is a library or package called primes that it would not have a broader name like the well known hosting site or such.  The RC template [[:Template:Library]] has no documentation about what is expected/required so there is little surprise that people use the template differently.  At least one attempt to get more information was made in 2009 but it didn't seem to go anywhere.  Even if we conclude that a certain use is wrong, we'd need to go around an mark the pages needs improvement or similar.  Examples:

::: <nowiki>Using {{libheader|Primes}} from [http://hackage.haskell.org/packages/hackage.html HackageDB]</nowiki> - creates Primes and formats in an ugly fashion. The code refers to HackageDB (which I would take to be the library and [http://hackage.haskell.org/package/primes primes] the package.

::: <nowiki>{{libheader|Icon Programming Library}} [http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings.icn provides deletec and sortc]</wiki>  This allows for the category page to represent the entire library rather than a single member.

::: vs. something else.  What is intended?    I see an example that includes: <nowiki>{{libheader|GTK}}{{libheader|GtkAda}}{{uses from|library|GtkAda|component1=Window|component2=Widget|component3=Handlers|component4=Main}}</nowiki> which seems much along the lines of what I would have expected. Except that the libheader for GtkAda and the uses from point at different pages (GtkAda and Library/GtkAda).

::: The example in Ruby provides no useful information on the source of the library <nowiki>{{libheader|optparse}}{{libheader|prime}}</nowiki>

::: The Alternate D version in counting by primes <nowiki>{{libheader|uiprimes}} Library ''uiprimes'' is a homebrew library to generate prime numbers</nowiki> provides little more and doesn't say where the code is located or if the code is even available.

::: The special case of TCLlib, seems to generate nicely formatted and and well organized content (<nowiki>{{tcllib|struct::list}}</nnowiki> generates '''Library:''' Tcllib ('''Package:''' struct::list).  If we had a general form of <nowiki>{{LibraryTemplate|Library Name|member1, member2, ...}}</nowiki>.  That would work nicely.

:: --[[User:Dgamey|Dgamey]] 22:50, 18 May 2011 (UTC) Updated: --[[User:Dgamey|Dgamey]] 02:20, 19 May 2011 (UTC)

==Categories/Keywords, what they might look like==
Articles in journals are often tagged with keywords for searching.  I'd like to be able to do this for Tasks.  Using categories is one way but may not be the best.

How the keywords/categories get organized is a challenge.  It would be nice if they could somehow self organize but I'm not sure there is a way to do that.  Maybe SMW may allow that, I don't know yet. Any other organization would be manual.  
*  Having much more than a couple of levels could get nasty. 
*  Having major and minor categories would be easier to organize.  
*  At some level minor categories/keywords should be able to participate in multiple categories/keywords at a higher level.  Things might be in math and cryptography, or graphics and math, etc.  Checksums might be in content integrity (if such a category existed) and crypography.  Some checksums like SEDOLS and Luhn checks are simple integrity anti-garble methods, while things like MD5 is (was) intended to be much more robust.

--[[User:Dgamey|Dgamey]] 02:40, 19 May 2011 (UTC)

: Hi Dgamey, do we want to impose a hierarchy of categories? An alternative is to use multiple tags and be able to filter tasks on multiple tags i.e find all tasks with tags CRPTO and CHECKERS. The Stack overflow site has a good system where it tries to make you use tags that are already in use by giving usage feedback. As you are encouraged to apply tagsm the system tells you how many times the tag has been applied etc. --[[User:Paddy3118|Paddy3118]] 02:52, 19 May 2011 (UTC)

:: Paddy3118, I was just giving examples. I'm not yet sure if I'd want to impose hierarchies or not.  And if we were to do it, my thoughts on this are only in localized topic areas.  I'll have a look at stack overflow's system over the weekend.  It's always better to have examples to look at. 
::: I had a quick look.  It's interesting and similar to the keyword idea.  The two search fields (tags single lookup), search (search) were a bit confusing at first.  It alos doesn't preclude topic categories or pages that link tags if we wanted to provide some organization.  It also looks like it would provide the self organization I was thinking of.  I like the fact that the tags are in visible buttons in the articles.  How could we do something like that?  --[[User:Dgamey|Dgamey]] 12:01, 19 May 2011 (UTC)
