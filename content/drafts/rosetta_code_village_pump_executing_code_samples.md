+++
title = "Rosetta Code:Village Pump/Executing code samples"
description = ""
date = 2014-01-30T20:47:45Z
aliases = []
[extra]
id = 10793
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Executing samples
|summary=What about providing the informations about how to run/compile/execute all the pieces of code available on RC?
}}
The informations about how to execute the pieces of code are for most of them not available on RC. Most often it is not difficult to get these informations, but IMHO it would be fine and easier to get these right in RC, and it would save some time when willing to try different languages.

=Proposal=

I propose that on the pages of each programming language (or on the pages of its implementations), we add a new paragraph about what is the standard command line to execute a simple/basic piece of code, whether it is an interpreter or a compiler (and sometimes both are available), and telling what is the command line to run an interactive interpreter when one is provided.

I propose that we also provide the information about what is the name of the package to install on a standard GNU/Linux distro, along with the equivalent on other main OS's.

I propose that we do the same on each library page (<code>{{libheader</code>), telling what additional command line arguments to use, and what package(s) to install to use it (also telling if we have to install the <code>-devel</code> package too or not).

I would also suggest to create a new template (for example <code>{{howto_run_info_missing<nowiki>}}</nowiki></code>) for samples or language/lib pages where all these informations are not available on RC yet.
[[User:Blue Prawn|Blue Prawn]] 00:35, 6 November 2011 (UTC)
: sounds like a good idea, but somehow it strikes me as something that would also be nice for comparison, thus on a page with all languages together like any other task. i am not sure which is better though. with the task though we could use the existing mechanisms to see which language has the instructions and which one is still missing. at least in the beginning that may be easier to track. later they could be moved to the appropriate language page. or copied.--[[User:EMBee|eMBee]] 05:00, 6 November 2011 (UTC)
:: Putting these informations on the implementation page is much more rational IMHO, because then we also put the equivalent on the library pages, and so then anyone is able to find the informations easily to run any code sample, because on any task page on RC the code sample for a language appears after a title with the name of the language which is a link, it becomes then quite natural to click on it to find howto run the code, and the same for a library given with the template libheader. With a task page a casual reader would have to search more to find the information about how to run a code sample, it is not very natural IMHO. Create a task for this may appear a good idea at first, but I think it is not because imagine what would be the result, a long page with only "install this package, run this command line" there is nothing really subject to compare of a valueable interest.
::: i disagree with this particular point. if i am researching languages that i don't know yet, comparing the effort to get something running is very much of interest.--[[User:EMBee|eMBee]] 05:56, 7 November 2011 (UTC)
:: Moreover if it is a task page, where to put the informations about libraries? On this task page too, or on the library page? In the first case it becomes very difficult to track every lib used, in the second case it is not very homogeneous. [[User:Blue Prawn|Blue Prawn]] 18:16, 6 November 2011 (UTC)
:: About creating first a task page, and migrate later the content to the implementations is not a good idea in my opinion, because first it is more rational to go straight in the final direction, and second because it is a very bad wiki practice. I think that Rosetta Code should respect the ownership of every submission, and if you copy-paste content from one page to another and the delete the original page, the ownership of the author is lost, and even worse the content is attributed to the person that has made the copy-past. This has been done (well at least one time) in the past on RC, and I think that it is not respectful to the author. If we tolerate losing the ownership of an author, then is this FDL licensing very serious? [[User:Blue Prawn|Blue Prawn]] 18:16, 6 November 2011 (UTC)
:: About how about tracking what has been done, and what is still to do, it can be done easily with the usual mechanism on this wiki which is the templates like <code><nowiki>{{improve|C|some reason}}</nowiki></code> or <code><nowiki>{{output?|OCaml}}</nowiki></code>. [[User:Blue Prawn|Blue Prawn]] 18:16, 6 November 2011 (UTC)
::: you are bringing up a lot of good points. especially the authorship issue. the remaining concern i have is, that while we can track which instructions need improvements, how do we track which languages do not have any instructions yet? i guess that would take a new bot to walk all the language and library pages to see if a certain tag is there?--[[User:EMBee|eMBee]] 05:56, 7 November 2011 (UTC)
:::: no, it would not need a bot, just the same template system used yet like <code><nowiki>{{improve|C}}</nowiki></code> or <code><nowiki>{{output?|C}}</nowiki></code>. We could create <code><nowiki>{{howto_run?|Language}}</nowiki></code>. [[User:Blue Prawn|Blue Prawn]] 20:27, 12 November 2011 (UTC)
: It seems to me that how to run a code sample would be more a property of the implementations of the language? After all, a language is really an abstract specification of syntax and semantics. (OTOH, there's nothing wrong with giving an example with a common implementation.) Who's going to go through and review/add all the text? –[[User:Dkf|Donal Fellows]] 08:43, 6 November 2011 (UTC)
::just like every other task. set one up and let people fill it in. sure it is a question of the implementation, and not of the language itself, but that doesn't mean it's not useful for a language comparison. how to solve a problem in any language includes building the program and making it run.--[[User:EMBee|eMBee]] 09:09, 6 November 2011 (UTC)


=Not the same, but worth a look?=
Inspired by, but not really a solution to the discussions above, I have created the new draft task: [[Hello world/Newbie]]. --[[User:Paddy3118|Paddy3118]] 09:15, 6 November 2011 (UTC)

="Unobtainium" languages?=

What about languages for which there is no available implementation, so that nobody but the author (supposedly) can execute the code samples at all? Example: I was browsing around and came across [[ProDOS]] (thinking, who the heck would name a language after a historic disk operating system for the Apple 2 family? Is there a connection? I had to take a look) Evidently, this is not publicly released (but was supposed to be sometime in 2012). There is no link to any page.

I don't think stuff like this is valuable Rosetta content. There is no way to verify the examples; for all anyone knows, it could all be made up.

There should be a minimum standard: implementations of the language should be available (not necessarily for free), running on hardware and operating systems that are in reasonably wide use (not something you can only run if you have keys to the basement of the Smithsonian, and bring replacement capacitors and vacuum tubes). [[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 20:47, 30 January 2014 (UTC)
