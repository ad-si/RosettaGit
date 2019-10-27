+++
title = "Rosetta Code:Village Pump/Replace MediaWiki"
description = ""
date = 2010-11-28T17:44:56Z
aliases = []
[extra]
id = 4488
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Replace MediaWiki
|summary=On the need for different serverside software.
}}
==Example engine==

I did something in PHP. http://gugarcode.awardspace.us/

Ignore my horrible code. Nothing W3C, almost no CSS. It's just an example. There is missing a lot of important features, like edit history, language info, accounts, and a "visual". This example will not replace MediaWiki. In this engine, there is no "lang tags" or "headers" , you choose a language, write description and code in different fields and submit, and it's all done. All automatic. If you want more than 1 example for a language, just add again. It will be grouped by Language.

'''I changed it to Awardspace, gugarcode.atbhost.net was apparently "hacked".'''
'''If someone really want to contribute, post in my User Talk.'''

'''GugaRC had more than 70% of RC examples. (I've made a transition bot.)'''
'''But ALL examples were completly erased by someone.'''

==Replace MediaWiki==

MediaWiki has a lot of limitations. What your think of replacing it?
Most problems from [[Rosetta_Code:Wiki_Wishlist|Wishlist]] will be solved.
It will be a lot easier to adminstrate and contribute to Rosetta Code.
If we use bots, a transition can be very fast.

It's not really easy to build from scratch something like this, but we can try.--[[User:Guga360|Guga360]] 20:31, 9 July 2009 (UTC)
:Replace it with what? --[[User:Mwn3d|Mwn3d]] 20:40, 9 July 2009 (UTC)
:: Read again.
:: --
:: If we use bots, a transition can be very fast.
::
::It's not really easy to '''build from scratch''' something like this, but we can try.'''
:: --

I did some tests in PHP. It looks good.
A "main page" with "Add new Task", "Add new Language", "Recent added/changed tasks", "Most popular tasks/languages" .
With a Search feature, in you can search a task or a language. And view all tasks from a language, like RC Category:Language. --[[User:Guga360|Guga360]] 22:26, 9 July 2009 (UTC)

:I participated in a group that migrated its wiki '''twice''' (once from Twiki to MoinMoin due to security exploits, then again from MoinMoin to ikiwiki because they wanted a git-backed wiki), I don't recommend changing wikis unless you absolutely have to.  Our group lost literally years of productivity managing the content transfers (and we still aren't quite done), losing a large chunk of our peripheral community in the carnage. I don't think you appreciate the number of features MediaWiki + Geshi is already giving us.
:That said, it might be an interesting exercise to list the desired features Rosetta Code currently uses and would like in the future. --[[User:IanOsgood|IanOsgood]] 00:00, 10 July 2009 (UTC)
::Migration is really, the only problem. But like i said, we will not use any wikis, but an "engine" built from scratch, this can be really interesting if RC members contribute. I see no problems if we use bots to migrate contents, while we keep MediaWiki with edits "closed." --[[User:Guga360|Guga360]] 02:49, 10 July 2009 (UTC)
:::What problems do you wish to solve, with what, and how? --[[User:Paddy3118|Paddy3118]] 03:11, 10 July 2009 (UTC)
::::[[Village_Pump:Home/Features_Wanted]], [[Rosetta_Code:Wiki_Wishlist]]. --[[User:Guga360|Guga360]] 04:08, 10 July 2009 (UTC)
::::: If you're going to do this, remember that the #1 requirement is ''guaranteed'' to be a zero-hassle transfer of the contents of RC to the new platform. I've been through a number of CMS/Wiki changes on various sites, and I've observed that when the content can be transferred directly, they've been a success, and when the content had to be recreated, they've been a failure (or the old site was such a failure of its own that throwing away the content was an acceptable solution, but even then it was months of pain and work). Given this, I'll only support a transfer to a new platform if it is sufficiently painless and the new platform is provably better. After all, what we have now works for almost everything that we try. —[[User:Dkf|Donal Fellows]] 20:34, 11 July 2009 (UTC)
:::::: Obviously, I will not do it alone. (not because it's hard, it's because i'm very poor at design.) Most people will help, will be completely open-source. The transfer will be very easy with bots. But there is a problem. For the bot do its job correctly, the article must be properly formatted. (Lang tags, headers and etc)
:::::: Incorrectly formatted articles should be transferred manually. In less than 2 weeks, we can transfer more than 90% of Rosetta Code. --[[User:Guga360|Guga360]] 20:58, 11 July 2009 (UTC)
::::::: It's your money and effort. Nobody other than yourself is going to put in effort until you've proved that you have a platform that's ready to receive us. (If there pages that you think are buggy, we should fix those pages or demonstrate to you why they are not actually buggy. Can't tell ''a priori'' which.) —[[User:Dkf|Donal Fellows]] 16:14, 12 July 2009 (UTC)
:::::::: This is just a example, a concept. It's not "my platform". It's just showing how examples will be added (with description, code and languages in different forms), adding a language two times to group and etc. New RC should be completly different from this example. --[[User:Guga360|Guga360]] 16:53, 12 July 2009 (UTC)

I made a small transition bot (it's running now) it already added a lot of tasks and examples, but some incorrect things ("<nowiki>{{trans C++}}</nowiki>" (missing |), <nowiki>
```txt
</nowiki> tags, or spaces to start a "code box" and etc.), just fails silently, incorrect examples are not added. But some wiki tags like "<nowiki>'''</nowiki>" are not compatible, so, we need to adjust other things to make a transition perfect. PS: There is no way to move incorrectly formated examples. If we are really thinking of moving the platform, we should fix all examples first. --[[User:Guga360|Guga360]] 23:17, 13 July 2009 (UTC)

==Out of nowhere?==
Where did the need come from? Where's the pages and pages of agonised debate about lost abilities over numerous months? 

This seems to have popped-up out of nowhere and gives no reason to undergo what could be a a damaging migration proccess. --[[User:Paddy3118|Paddy3118]] 02:51, 10 July 2009 (UTC)
: There is exact reason. Like i said, MediaWiki has a lot of limitations, if we use a "engine", it will be really better. [[User:Short Circuit|Short Circuit]] already wanted to switch away from MediaWiki. [http://www.reddit.com/r/programming/comments/8bvbl/dear_proggit_heres_what_ive_been_wasting_my_life/]. But he never had time. We can just continue that project. --[[User:Guga360|Guga360]] 03:44, 10 July 2009 (UTC)
:: Sounds like a heck of a lot of work. Don't you have something better to do, like having a life or something? (OK, a little uncharitable, but really there's no need to add another damn wiki implementation to the big pile we already have. Also, migrating the existing content to something other than MediaWiki would be a major PITA; it uses MW features quite heavily.) —[[User:Dkf|Donal Fellows]] 10:55, 10 July 2009 (UTC)

Take a look at [[Village_Pump:Home/Features_Wanted]]. Most features are almost impossible to implement using MediaWiki. --[[User:Guga360|Guga360]] 03:51, 10 July 2009 (UTC)
: Actually, the vast majority (all?) of them are possible.  I know how each of them could be done; My understanding of MediaWiki has grown tremendously over the last several months. --[[User:Short Circuit|Short Circuit]] 08:03, 10 July 2009 (UTC)

==Stick with MediaWiki==

I think we should continue to use MediaWiki.
* We don't have to maintain MW ourselves.
* Many people know MW wiki syntax.
* MW is very powerful in the matter of ''user-created functionality'', using categories, templates, and sometimes bots. All of the structures we've built -- <nowiki>{{header}}</nowiki>, categorized and flat indexes of examples, "Tasks not implemented in", etc. -- all of this does not need attention from RC administrators/programmers. Consider MediaWiki like a general-purpose programming language: if we create something that is adapted to what we want now, it will lack generality and make it difficult for people to come up with ideas for improvements and ''just do them''. (Of course, this is moot if we invent a ''better wiki'' rather than Rosetta Code-specific software.
--[[User:Kevin Reid|Kevin Reid]] 21:41, 11 July 2009 (UTC)
::Maintaining a wiki software is not something easy. But MW is really powerful. But sometimes lacks features that we need. Like some insoluble problems, like "C sharp bug". With the new software, we can stop using bots, because we can do a "Tasks not implemented in" in real time. We don't need to maintain a PHP script (like "viewunimpl.php"), but we need to maintain a bot. Bots are very unstable. (See [[Tasks_not_implemented_in_LaTeX]]). With a specific software, we don't need to adapt MW. It will be more stable, and we can do everything, without limitations. A example: RC changed "lang tags" a time ago. What happened? Today, we are still using deprecated tags in most Tasks. A transition has "pros and cons", We should really think and discuss this before doing anything. --[[User:Guga360|Guga360]] 15:44, 12 July 2009 (UTC)
