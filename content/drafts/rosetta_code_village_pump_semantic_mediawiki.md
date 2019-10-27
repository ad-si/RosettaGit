+++
title = "Rosetta Code:Village Pump/Semantic MediaWiki"
description = ""
date = 2010-11-16T00:20:32Z
aliases = []
[extra]
id = 7831
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Semantic MediaWiki
|summary=Advanced classification features of the MediaWiki software, and ways to leverage it to understand the content in this site.
}}
I've installed the [http://semantic-mediawiki.org/wiki/Help:Editing Semantic MediaWiki] extension. Play around with it. Figure out what can be accomplished. Get stuff better-organized around here. Things I expect will be possible
* Turn Task pages into reports using SMW
* SMW should be able to handle the kind of queries that power the "Unimplemented in X" pages
* Be able to create pages for "examples of X in language Y"
* Be able to treat things like PureBasic and GAMBAS as supersets of BASIC, etc. --[[User:Short Circuit|Michael Mol]] 21:34, 26 July 2010 (UTC)

See [[Hough transform]] and [[Talk:Hough transform]]; the content on that page does a decent job of demonstrating a lot of the existing semantic structure on the wiki, so it's our guinea pig.

I also strongly recommend reading these pages:
* [[smw:Help:Editing]]
* [[smw:Help:Properties and types]]
* [[smw:Help:Semantic templates]]
* [[smw:Help:Inline queries]]
* [[smw:Help:RDF export]]
--[[User:Short Circuit|Michael Mol]] 11:20, 27 July 2010 (UTC)
:What's up with this page?: [[Special:SemanticStatistics]]. Also, it looks like it's getting some things confused with library names that contain "::". --[[User:Mwn3d|Mwn3d]] 19:22, 28 July 2010 (UTC)
:: Ah, yes. It'll pick up an any link that looks like <tt><nowiki>[[blah1::blah2]]</nowiki></tt>; the double-colon is the separator between property and value. Prefix a : to clear it up, same way you have to prefix a colon to make a category link. (i.e. <tt><nowiki>[[:Category:Programming Tasks]]</nowiki></tt>) --[[User:Short Circuit|Michael Mol]] 19:28, 28 July 2010 (UTC)


Ok, here's the general scoop. As far as Rosetta Code is concerned, Semantic MediaWiki doesn't do a ''whole'' lot for site structure that we weren't already massively applying categories for already. The biggest thing it ''will'' allow us to do is break code examples into individual pages, and be able to build page listings (like Task pages) based on any category intersection we choose. That means we can get a listing of all examples using a particular library/language cross-section, or a listing of all examples from languages A and B from tasks where solutions from languages A and B are both present. --[[User:Short Circuit|Michael Mol]] 14:17, 29 July 2010 (UTC)

So far, SMW has already done one useful thing: It's let us count how many language/task intersections we have.  See [[Special:Properties]], "Implemented in language", and the number between the parentheses. That's how many times [[Template:Header]] is being used, which is currently 13571. That's a lot of examples. --[[User:Short Circuit|Michael Mol]] 14:17, 29 July 2010 (UTC)

One other thing that would be really handy is if you could query for all unimplemented examples in all languages in the mylang section of your user page.  This would allow you to quickly locate tasks that you could implement.  If it's possible, it would be fun to be able to sort by the number of unimplemented examples a particular language has, so the unimplemented tasks in an infrequently used language are sorted to the top.  --[[User:Tyrok1|Tyrok1]] 14:23, 31 July 2010 (UTC)

==Issues==

### Remaining

{{alertbox|lightgreen|These issues are new}}

### Resolved

{{alertbox|#fdc|These issues have been resolved.}}
Look at [[:Category:AutoIt]]. I think some SMW error messages are leaking through. I think it's got something to do with [[Template:stub]]. --[[User:Mwn3d|Mwn3d]] 17:30, 30 July 2010 (UTC)
:Looks like you got it. I'll be on the lookout for more problems like that. I can try to fix them when I see them. I must have added the extra colon myself when I tried to fix it. --[[User:Mwn3d|Mwn3d]] 18:22, 30 July 2010 (UTC)
:: Fixed.  The normal SMW syntax for adding properties to things is in the form of <nowiki>[[property::value]]</nowiki>, but "value" shows up as an intrawiki hyperlink (to the 'property' page, I believe). Since that's not usually what I've wanted when I'm adding properties without changing the actual structure of the wiki (categories and properties are somewhat analogous, but SMW supports a few more features when dealing with properties), I try to remember to use this syntax, instead: <nowiki>[[property::value| ]]</nowiki>.  I can hide a regular MW link the same way: [[User:Short Circuit| ]] ... --[[User:Short Circuit|Michael Mol]] 18:25, 30 July 2010 (UTC)

::: A similar fix should be done to Libheader template, since libheader with "arg" like DBD::Mysql or alike is again interpreted by SMW. --[[User:ShinTakezou|ShinTakezou]] 08:35, 6 August 2010 (UTC)
:::: Yeah, I know. I need (anyone could) to talk to the folks in #semantic-mediawiki on Freenode to find out exactly how, but I haven't had the time. --[[User:Short Circuit|Michael Mol]] 13:24, 6 August 2010 (UTC)

I think SMW ate the "Example" namespace because it added a "Property" namespace, and was not expecting there to be any other namespaces. It seems to be treating the "Example" namespace as it should be treating the "Property" namespace. It may be calling out for the namespace by the index value it expected it to have. --[[User:Coderjoe|Coderjoe]] 05:59, 11 August 2010 (UTC)
: it appears that the following setting may be responsible: [[smw:Help:Configuration#smwgNamespaceIndex]].
:: Yeah, I set that correctly (At least, I think I did), but I think a bug in SMW didn't account for it properly. I haven't had time to poke the SMW folks for proper extraction and recovery. (I know a little bit about how the DB schema in MW is set up, but I don't know what happens if I remove a namespace. The "Reports" namespace would need to get munged around, too.) --[[User:Short Circuit|Michael Mol]] 10:32, 11 August 2010 (UTC)
==Goals==
In no particular order:
* Turn Task pages into reports using SMW
:Would this mean a return to the example pages of old? Is that namespace still around? If we did it that way we might end up with complications when a task is renamed. --[[User:Mwn3d|Mwn3d]] 19:08, 29 August 2010 (UTC)
* SMW should be able to handle the kind of queries that power the "Unimplemented in X" pages
* Be able to create pages for "examples of X in language Y"
* Be able to treat things like PureBasic and GAMBAS as supersets of BASIC, etc.
