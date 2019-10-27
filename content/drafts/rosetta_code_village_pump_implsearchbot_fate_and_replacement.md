+++
title = "Rosetta Code:Village Pump/ImplSearchBot Fate and Replacement"
description = ""
date = 2010-11-28T17:41:11Z
aliases = []
[extra]
id = 4840
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=ImplSearchBot Fate and Replacement
|summary=ISB disabled, replacements
}}
Taken from ISB's [[User:ImplSearchBot|user page]]:

Due to a lack of available time resulting from the growth of the site, the growth of ISB's mission, and other issues unrelated to Rosetta Code, [[User:Short Circuit|Short Circuit]] does not have time to maintain and operate the bot in addition to other site maintenance tasks.  As a result, ISB has been disabled since Labor Day 2009 due to a lack of sufficient time to get the bot up and running properly, and likely won't be resumed in its normal role.  It needs to be replaced by another bot maintained and operated by someone who has more time available to respond to bugs and feature requests.  To this end, ISB will be repurposed to provide fast access to the raw category data of the wiki, avoiding some of the overhead that bots that depend on category data currently face.  --[[User:Short Circuit|Michael Mol]] 15:06, 10 September 2009 (UTC)
: I should add that the "unimplemented in X" pages are still ''needed'', but I don't have the time to maintain the software that explicitly creates and maintains them.  I am very, very open to helping anyone interested in writing a substitute bot get started, and I would suggest that a discussion take place over at the [[Rosetta Code:Village Pump|Village Pump]] as to what features the substitute bot provides. --[[User:Short Circuit|Michael Mol]] 17:33, 10 September 2009 (UTC)
:: Until I have time to set up the new Report namespace, take a look at [http://rosettacode.org/json/isb/ these JSON] files.  There is one JSON file there for every category on the wiki.  Each JSON file contains the contents of the relevant category, with the exception of [http://rosettacode.org/json/isb/_categories.json this one], which contains the names of all the categories.  There is a running service on the server that updates the JSON files within a few minutes of a page being added to the category. The update is tied to the server's five-second load average, and should almost never take longer than four minutes.  The timestamps on the file, for the most part, reflect the last time the category was updated; The files are only written to if the category contents change between checks, or if the file was deleted by manual means.
:: Wander over to {{vp|ImplSearchBot Fate and Replacement}} and discuss a replacement for the ImplSearchBot.  Some time in the next couple weeks, I'll be ready to grant Bot privileges to whatever account is to replace ImplSearchBot.  Don't limit your ideas to MediaWiki bots, though; There are a variety of other ways the data could be used, from RSS feeds to in-browser widgets.  --[[User:Short Circuit|Michael Mol]] 09:07, 13 September 2009 (UTC)
::: I have hacked together a very crude version (mostly for my own use now) which can be found [http://svn.freebsd.lando.cc/joey/Public/Rosetta%20Code/_ImplSearchBot in my SVN]. This probably won't be of much use to most people here as it only runs locally and it's in PowerShell so it will only run on Windows. Still, maybe someone finds it useful. I think I'll include omit checking tomorrow too. —[[User:Hypftier|Johannes Rössel]] 21:48, 13 September 2009 (UTC)

==Essentials completed==

[[User:Opticron|Opticron]] has done a wonderful job hacking up the MultiCategorySearch extension to better suit Rosetta Code's needs. Important differences from the MCS's trunk include the ability to transclude the search results, which made the new Reports pages plausible, and adding support for memcached, which made the new Reports pages practical.

MCS works by including in its result set a listing of the pages in specified categories, and culling pages in other specified categories. Members of subcategories are not considered, for a variety of existing and forseen technical reasons. Due to difficult-to-fix limitations in the MediaWiki core, result set counts are not transcludable. It's a very simple approach, and it's my opinion that most quirks that may result (such as the current implementations of RCBF and friends as missing from ''all'' languages) should be addressed by changing the wiki content and structure where possible, rather than of the underlying site code. --[[User:Short Circuit|Michael Mol]] 18:52, 24 October 2009 (UTC)


###  Consequences and Resolutions 

* Tasks where the implementations are large or complex enough to warrant their own pages, such as [[RCBF]], [[RCSNUSP]], [[RCRPG]] and [[RCHQ9+]] current show as omitted for all tasks, because while the task page is in [[:Category:Programming Tasks]], the implementation page is not.  I don't know of an elegant solution. --[[User:Short Circuit|Michael Mol]] 18:52, 24 October 2009 (UTC)
* Where ISB created the "Unimplemented in X" pages, there is nothing actually creating those pages now.  Consequentially, that's currently a manual process.  There are also a fair number of other pages whose content are categorically similar yet are manually created, and I'd like to see this problem tied in with that one. [[Rosetta Code:Village Pump/Trivial task automation]] should probably hold the discussion for that. --[[User:Short Circuit|Michael Mol]] 18:52, 24 October 2009 (UTC)
* There are no ''edit'' links associated with the unimplemented listings. The only way to change that would be to further modify the MCS code, but I would like to focus efforts on other areas first, and not push Opticron to donate any more of his time than he has. --[[User:Short Circuit|Michael Mol]] 18:52, 24 October 2009 (UTC)
* The MCS extensions' transclusion capability allows the recreation of much of the Category namespace functionality built-in to MediaWiki, with the added benefit of showing more entries per page, as well as better control of the page's layout. I would like some investigation on the possibility of moving content out of Category: pages, and into pages which include code one or more listings of code samples. --[[User:Short Circuit|Michael Mol]] 18:52, 24 October 2009 (UTC)
