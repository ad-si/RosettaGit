+++
title = "Template talk:Libheader"
description = ""
date = 2014-12-28T04:40:50Z
aliases = []
[extra]
id = 3046
[taxonomies]
categories = []
tags = []
+++

What if we changed this template so it made a subheading for the library? That way people browsing Solutions by Library could see their library in the TOC. --[[User:Mwn3d|Mwn3d]] 19:19, 15 September 2008 (UTC)

I put a little demo of what I want it to do in the [[Sandbox]]. --[[User:Mwn3d|Mwn3d]] 16:17, 16 September 2008 (UTC)
:I understand the motivation for this idea, but I don't think it makes much sense in general, because the template isn't always used to begin an alternative implementation. Often it begins the only implementation, in which case the language's section would have all its content in a single subsection, which would be silly. And when it does begin an alternative, one of the other alternatives is likely to not use any library, so we'd end up with something like this:
:*Foo Programming Language
:**Implementation without any libraries.
:**Bar Library
:***Implementation with the library.
:Since the implementation without any libraries and the one with a library are logically on the same footing, it's counterintuitive for them to be in different levels of the section hierarchy. I suppose we could do something like this:
:*Foo Programming Language
:**Core language
:***Implementation without any libraries.
:**Bar Library
:***Implementation with the library.
:but that's pretty ugly, I think. —[[User:Underscore|Underscore]] 11:17, 22 September 2008 (UTC)
::You know what? You're right. And when you're right you're right. And you? You're right.  I'm glad I waited for someone to say something before I went and did it. With those cases you talked about I don't there's any good way to do it. Even if we added #link code to the template, some libraries are used in a couple of languages so we'd have to code in unique labels.--[[User:Mwn3d|Mwn3d]] 16:56, 22 September 2008 (UTC)

==Formatting==
* I've seen examples of this tag that assume you can write with 2 parameters.  Parameter 1 being the wiki reference and parameter 2 being the content that gets written into the page.  How hard is it to make this happen?  And who do I bounce it off first?  Currently if the Library page has underscores in the name they format and it looks like hell. --[[User:Dgamey|Dgamey]] 11:23, 22 April 2010 (UTC)
:When a wiki page URL has spaces in it, they will show up in the URL as underscores. When you make a link to that page the underscores are optional. For instance, [[Compound data type]] and [[Compound_data_type]] link to the same place, but one has underscores in the final formatting because I put them there in the wiki formatting. So if you don't want the underscores to show up just use spaces. Is that the problem you're talking about? --[[User:Mwn3d|Mwn3d]] 15:49, 22 April 2010 (UTC)
::Yes thanks!

==Problems with double semicolons==
This is especially a problem with Perl, whose modules use double semicolons in their names.

The following shows "bar" twice:
{{libheader|foo::bar}}

The following shows "Complex" twice:
{{libheader|Math::Complex}}

And on some pages, (e.g. [[Plot coordinate pairs#Perl]]), it shows some weird "warning.png" text afterwards. (I can't reproduce it here.) --[[Special:Contributions/71.141.128.235|71.141.128.235]] 05:30, 18 August 2010 (UTC)

: This is caused by the Semantic MediaWiki extension, which parses links with double colons as semantic annotations. It is not only polluting the pages with libheader, but also the semantic properties. The <nowiki>[[SMW::off]](somelink)[[SMW::on]]</nowiki> disables warnings and storage for the link between the off/on pair, but unfortunately it still modifies the </nowiki>[[Category:]]</nowiki> style links, which add the current page to the named category. This last problem only seems to apply to links which do not start with a single colon. Short of modifying the SMW parser extensions to also not modify links when off, or changing the wiki to handle the double-colon separator similarly to the # character in languages like C#, I don't know what to do. We might need to modify SMW anyway to fix another problem. --[[User:Coderjoe|Coderjoe]] 14:32, 21 August 2010 (UTC)

:: Disguising the colons by <code>&amp;#58;</code> like in the following seems to help.  Is there a better way? -- [[User:Kevin Ryde|Kevin Ryde]] ([[User talk:Kevin Ryde|talk]]) 04:40, 28 December 2014 (UTC)

{{libheader|Math&#58;&#58;Complex}}
