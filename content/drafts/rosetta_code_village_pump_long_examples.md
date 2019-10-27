+++
title = "Rosetta Code:Village Pump/Long examples"
description = ""
date = 2010-11-27T00:13:38Z
aliases = []
[extra]
id = 5328
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Long examples
|summary=What to do about particularly long examples.
}}Recently, examples have been moved to subpages of tasks (ex: [[Closest pair problem‎]] and [[Closest pair problem‎/SmallTalk]]). The reasons for this are so that pages don't get too long for some browsers to open when editing and so that the server won't get overloaded if one of these tasks becomes popular (bandwidth may also be a concern). It has been pointed out that this solution may be counter-productive to the goals of the site. This idea and possible alternate solutions should be discussed here. --[[User:Mwn3d|Mwn3d]] 21:44, 8 January 2010 (UTC)

* The “requirement” I have in mind is: that no effort should be needed for one to be exposed to examples in ''all'' languages while browsing a task. As far as I know, the most efficient way to do this is to have all the examples on one page.
: I agree that there are practical limits, but I disagree that we've reached them, and e.g. [[Playing Cards/E]] (which is one of the examples [[User:Glennj]] split off) is surely not too long. Regarding “too long for ... editing”, are you referring to the automatic 32K notice Wikipedia puts up? As far as I know, we have lots of pages that long and no one's complained. I agree that length can be a technical and/or usability problem eventually, but I feel that we have not reached that point. --[[User:Kevin Reid|Kevin Reid]] 22:25, 8 January 2010 (UTC)
::Some examples that have been separated are pretty short. Some others (like [[Arithmetic Evaluator/Ada]]) are obviously contributing a lot to a future problem. I hesitate to put a size requirement in bytes on examples that are too long since some take up a lot of space without being very large in bytes (like lots of [[Assembly]] examples). I'm not sure what the criteria could be except "it takes up too much space on the page". --[[User:Mwn3d|Mwn3d]] 03:01, 9 January 2010 (UTC)

::: I too don't like splitting up long pages unless absolutely necessary. What is too long might depend on a lot of factors, but I would think that a page that is still usable on, say, a netbook should be OK - it is new, but low-powered hardware. --[[User:Paddy3118|Paddy3118]] 05:49, 9 January 2010 (UTC)

:: I think the 32k limit was for IE6 and earlier. I don't know how many contributors use that ancient POS, but surely not too many? Programmers tend to use more recent browsers that aren't so pathetic. –[[User:Dkf|Donal Fellows]] 08:20, 9 January 2010 (UTC)

::: I don't know whether the 32k limit is browser-specific, but I just posted CSVs from the wiki's analytics data. [[Rosetta Code:Analytics/20091209-20100108/Browsers|Browser break-down]] and [[Rosetta Code:Analytics/20091209-20100108/IE Versions|IE versions breakdown]]--[[User:Short Circuit|Michael Mol]] 19:17, 9 January 2010 (UTC)

: (Warning: Pointless general observations ahead) Some of those notices are driven by resource availability, and when pages exceed the final size limits, things get ''nasty''. My first attempt at building the system that's now satisfied by the "tasks unimplemented in language X" pages was to build a full table cross-referencing languages and tasks. MediaWiki OOM'd when I tried uploading the resulting table wikicode, which weighed in at over 1MB.
:
: Yes, 1MB is a ''huge'' amount of data to ask to be rendered as HTML, but the example shows that resource limitations are very real. The problem with the task pages as they currently exist is that while some pages, flattened, consist of a small number of large examples, some pages, flattened, consist of a very large number of small examples. Both of those scenarios lead to pages that choke MediaWiki.  Without pruning out languages, it becomes difficult to chose which of the latter examples to break out into subpages. Some of it ''may'' be able to be worked around with page transclusion, though. There will still be upper limits, but it's something to try. At some point, it may be possible to paginate overly-long transclusions, but that's a bit of a dream, at this point. --[[User:Short Circuit|Michael Mol]] 06:28, 9 January 2010 (UTC)

:: Does WP have this problem? What do they do? --[[User:Paddy3118|Paddy3118]] 23:04, 9 January 2010 (UTC)

::: I don't know. I'd have to ask in #MediaWiki. I'm sure I'll get a "MediaWiki is the wrong tool for the job" response when I do. I've had that on past occasions. At least I'll have a [http://rosettacode.org/blog/2009/12/why-and-why-not-mediawiki.html reply to that]... --[[User:Short Circuit|Michael Mol]] 03:07, 10 January 2010 (UTC)
