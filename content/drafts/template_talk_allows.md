+++
title = "Template talk:Allows"
description = ""
date = 2010-08-19T18:48:13Z
aliases = []
[extra]
id = 8010
[taxonomies]
categories = []
tags = []
+++

Since everything in "allows" should start with "Capability:", maybe one should include that in the template (i.e. instead of "<nowiki>={{1}}</nowiki>" use "<nowiki>=Capability:{{1}}</nowiki>", and then instead of "<nowiki>{{allows|Capability:Graphics}}</nowiki>" use "<nowiki>{{allows|Graphics}}</nowiki>"). --[[User:Ce|Ce]] 06:35, 18 August 2010 (UTC)
: If everything is going to be a capability, does the "Capability" need to be explicitly stated? I left the Capability part out since I wasn't sure that everything would be a capability. --[[User:Coderjoe|Coderjoe]] 16:26, 18 August 2010 (UTC)
:: Of course not everything is a capability. Only everything referenced by "provides", "allows" and "requires". The idea of the "Capability:" prefix was to separate them on the page level, since you may very well e.g. have both a task named "Graphics" and a capability "Graphics". That is, the Capability "namespace" is to avoid unintentional naming conflicts (not unlike the User namespace prevents conflicts between user names and page names, except that the Capability namespace isn't directly supported by MediaWiki, but only based on convention).
:: I don't have strong feelings about that prefix; it's certainly not needed to ''identify'' capabilities. I just thought it would be a good idea to prevent naming conflict from the beginning by using the prefix. But I wouldn't care too much if the prefix were removed (it would increase the risk of naming conflicts, of course).
:: If I'm the only one considering that prefix a good idea, it may well go. In that case, the change should probably be done early, so that the work required isn't too large. In any case, it would be interesting to know what others think about the prefix. --[[User:Ce|Ce]] 16:45, 18 August 2010 (UTC)
::: I think Coderjoe was trying to say that the template could be defined to use "Capability:{{{1}}}", instead of as "{{{1}}}". That way, the "Capability:" could be left out of the template argument. (Just clarifying; don't mind me.) --[[User:Short Circuit|Michael Mol]] 19:36, 18 August 2010 (UTC)
:::: Given that this is exactly what I have suggested in the initial comment, I doubt that it's what Coderjoe meant. --[[User:Ce|Ce]] 19:46, 18 August 2010 (UTC)
::::: Right. My question was: if everything (in these three properties) was the name of a capability, does the "Capability" part need to be in the value? Ce brings up using it to disambiguate pages for the capability from another page by the same name, using a pseudo-namespace. Do these capabilities need pages? SMW has types other than "Page". --[[User:Coderjoe|Coderjoe]] 19:52, 18 August 2010 (UTC)
:::::: Where would the description of those properties go? And would that constrain the flexibility (e.g. with pages we can also describe relations between capabilities with semantic markup)? My ultimate goal is still to find a way to implement an automatic omit. Since semantic wiki query has some serious constraints, I'm in fear of every additional flexibility removed, because that removed flexibility might turn out to be essential for that goal. --[[User:Ce|Ce]] 20:42, 18 August 2010 (UTC)
:::::: Just a note: I've just added reverse lookups for requires, provides and allows (required by, provided by, allowed by). This is completely automated by inserting some code into the Capability template, which should be included into each capability anyway (this is the only thing one has to remember). I don't know how to do this without pages for capabilities. --[[User:Ce|Ce]] 13:46, 19 August 2010 (UTC)
::::::: That is a pretty compelling reason to have the page. It is kinda sad that SMW doesn't have the ability to connect the two endpoints without a page in the middle like this, since this method means someone has to make sure to create a page with the proper contents every time a new capability is added. --[[User:Coderjoe|Coderjoe]] 18:48, 19 August 2010 (UTC)
