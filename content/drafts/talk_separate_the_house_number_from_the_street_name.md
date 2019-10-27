+++
title = "Talk:Separate the house number from the street name"
description = ""
date = 2018-08-10T09:40:41Z
aliases = []
[extra]
id = 17697
[taxonomies]
categories = []
tags = []
+++

== Localization ==

These addresses look...Dutch? It looks like the house numbers comes after the street name there. Other countries might do it differently--in the US we put the house number first. I'm not sure how to account for this. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 17:39, 9 June 2014 (UTC)
:Right, in the Netherlands -given in the task, Germany also mentioned- we use Dutch and in the other case we use German. Sorry, I have a long backlog of Rosetta issues, besides doing this a have also a job, women and kids.
:<p>At Royal Philips we say: Think globally, act locally.</p>--[[User:Siskus|Siskus]] ([[User talk:Siskus|talk]]) 07:32, 28 July 2014 (UTC)

==The Rules?==
Hi, you need a link to the rules for extracting the numbers (in English); or add the rules to the task (preferred). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:42, 9 June 2014 (UTC)

Unfortunately a statement of the type  "the right answers are here.." just means that someone could print the given text. You describe no algorithm to follow so any algorithm can be used to generate the output. '''You really need a description of ''how'' to do the separation'''. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:03, 10 June 2014 (UTC)

:In addition, there seems to be no justification in the referenced article for hardwiring 1940 and 1945. It's certainly possible to blindly translate Scala's regex to other languages, but it would be nice if it also made some sense to people who are from other parts of the world.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 23:36, 10 June 2014 (UTC)
:1940 and 1945 also noted as '45 are important historic years, the years of the second world war of course.--[[User:Siskus|Siskus]] ([[User talk:Siskus|talk]]) 21:43, 13 June 2014 (UTC)
:: The problem is that there are many such commemorative street and square names, (there's also for instance: [http://nl.wikipedia.org/wiki/Straatnaam 1813, 1944, 1953, 1960, 1992]), so you will at least have to specify in the task description which years we should look for. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 01:22, 14 June 2014 (UTC)


== Vote for deletion? ==
Despite being asked for some algorithm to complete the task, none has been given after nearly two months and the later questions of this task page left unanswered. The author has also linked a new draft task [[Starting a web browser]] to this one, which compounds the issue. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:52, 27 July 2014 (UTC)
: I vote for deletion, there are already too many of these half-baked tasks on the wiki. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 21:48, 27 July 2014 (UTC)
:: Yeah, let's improve the tasks. In this process I am. But I am limited with time. This is a draft task, those are half-baked, it's in development. It's so cheap to delete, why don't we delete to whole wiki as well as the half-bake solutions. (I saw crap where a java.util.Calendar is '''more''' thread-safe. The author must be kidding. There are only qualities; thread-safe or thread-unsave. This Calendar is the worst API ever and women can't be more, less or maybe half pregnant. Avoid this depreciated crap, it translated from C in the 1990s by Taligent, do you remember?)
::Deletion because the rest of the wiki is half-bake is a poor argument because this draft task will be scarified for the rest of the wiki. It's better to level-up the rest of the wiki then making destructive noises. (If Fwend is also familiar with the task problem, and I think he is, he is able to give his contribution.) Which task has to be deleted, anyway?--[[User:Siskus|Siskus]] ([[User talk:Siskus|talk]]) 10:25, 28 July 2014 (UTC)?
::: Well, here's your chance: stop babbling and fix the task description, save it from deletion. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 13:33, 28 July 2014 (UTC)

::: Hi Siskus. I had asked quite nicely I thought, for you to tell us a bit more of ''how'' to achieve the goals. You ignored this. You then created another task based on this one too. You seemed to be making things worse from my standpoint - not just ignoring a request. 
::: We do have some tasks that have been in draft for some time and other members have made it easier to find them and assess them. Some might have been abandoned and just needed others to see they were there, to improve them. This task I think needs someone to write the algorithm of what needs to be done, in English, with the data. Can you do that? Can anyone else? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:04, 28 July 2014 (UTC)

'''Several years and still no explanation. I vote for deletion'''. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:44, 30 June 2018 (UTC)
I vote for remaining (the author is blocked and silenced by Paddy)--[[User:Cloudius|Cloudius]] ([[User talk:Cloudius|talk]]) 20:14, 30 June 2018 (UTC)

:User Siskus is not blocked by me Cloudius. What made you think So? 
:Even so blocked or not; are you willing to improve this task? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:57, 1 July 2018 (UTC)
:Deletion fine by me. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 04:37, 4 August 2018 (UTC)
::Rather than delete the task, I've amended the task description to suggest how one might tackle it i.e. either using the regular expression in the Scala solution or an equivalent algorithm. This is how most of the other existing solutions appear to have approached it in any case.

::The bonus (i.e. dealing with addresses in other countries) is inappropriate in the circumstances and so I've deleted it.

::However, if you still don't like it, I've no objections to it being removed altogether.

::Incidentally, Pete, have you tried using Euphoria syntax highlighting for your Phix entries? As an experiment, I tried it here and it looked pretty close.--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 11:10, 8 August 2018 (UTC)
:::I have, and I have rejected that proposal. It would reduce any incentive to get phix syntax supported, which is of course the exact opposite of what I want. Any changed tags would all need changing back if phix ever does get added. There has been a phix.php geshi file available since october 2015, and it is in geshi 1.0.9.0 which was released over a year ago and has still not been applied. I would, of course, greatly favour a method of being allowed to update geshi files directly on rosettacode - new languages are created and existing ones evolve all the time, and this site is not doing a very good job of supporting that. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 23:45, 9 August 2018 (UTC)

::::Yeah, I'm in the same boat as you with my Kotlin entries where I use Scala syntax highlighting - this is what Kotlin posters before me did so I just carried on the tradition. As you say, it would be a nuisance having to change the lang tags if the site were updated to use the latest version of GeSHi but it's a problem I wouldn't mind having :)

::::Incidentally, I did ask Short Circuit last year about updating GeSHi (see [[User_talk:Short_Circuit#Current_version_of_GeSHi]]) but didn't get a response. Wonder if Paddy has any influence here? --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 09:40, 10 August 2018 (UTC)

==Fuzzy==
Yes a litte bit fuzzy. How to parse "Peace Street 1940 1945" . Where is the street name ? --[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 16:33, 12 October 2015 (UTC)

==zw==
The address of The Accor Hotel at Schipol Airport is Stationsplein Zuid-West 981, 1117 CE Schiphol, Netherlands. Is the street "Stationsplein Zuid-West" and the house number 981, or is the street "Stationsplein" and the house number "zw 981"? --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:25, 8 August 2018 (UTC)
