+++
title = "Rosetta Code:Village Pump/Trivial task automation"
description = ""
date = 2009-10-24T23:34:30Z
aliases = []
[extra]
id = 4926
[taxonomies]
categories = []
tags = []
+++

There are a number of pages, categories and others that get linked to automatically, have consistent content, but require manual creation.  Examples include: "X examples needing attention" found as subcategories of [[:Category:Examples needing attention]], "Tasks not Implemented in X" found as subcategories of [[:Category:Unimplemented tasks by language]], LCT pages such as [[:Category:Garbage collection]] and [[:Category:Garbage collection/Allowed]] etc. It would be very helpful if these pages could be automatically populated.

The first problem is identifying which which pages need to be populated.

The second problem in doing this is identifying that the pages need to be populated in the first place.  [[:Category:C++ examples needing attention]] won't appear as a subcategory of [[:Category:Examples needing attention]] until after the page has been created, and <nowiki>[[Category:Examples needing attention]]</nowiki> has been included in that page.  ''However'', if one actually navigates to the page, one will see any members that exist within it.  If one programmatically asks for a listing of the category members via the MediaWiki API, one will get a listing of those members (or a null set in the case of there not being any members.). Grabbing the "static" JSON file from [http://rosettacode.org/json/isb/ http://rosettacode.org/json/isb/] will give you the current set of members as well. (In fact, I prefer that people use the JSON files, rather than the MediaWiki API, as that API is expensive on the server side.)

The third problem is identifying what to put in the pages. Using a template seems the obvious choice.

What I'd <em>like</em> to see is a bot that would be able to pull simple instructions off of a page on the wiki, perhaps something on the order of a list of "{category name},{template source}" pages.  To deal with server load concerns, I can provide a file with a numeric value indicating rough server load; The larger the value, the slower the bot (or any other bot) should run.  The JSON file generation already spins at a rate limited by a similar number.

Thoughts, critiques or implementations? --[[User:Short Circuit|Michael Mol]] 23:34, 24 October 2009 (UTC)
