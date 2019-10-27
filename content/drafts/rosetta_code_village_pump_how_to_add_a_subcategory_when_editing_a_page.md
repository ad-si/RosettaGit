+++
title = "Rosetta Code:Village Pump/How to add a subcategory when editing a page"
description = ""
date = 2013-05-20T13:42:29Z
aliases = []
[extra]
id = 12420
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=How to Add a Subcategory in Page Edit?
|summary=How do I add the Subcategory "Assembler (360-z/OS)" under the already existing Category "Assembly"?
}}
Recently created the Category "Assembler (360-z/OS)" for IBM Mainframe Assembly Language, from System/360 to z/OS.  Then I learned there was already an "Assembly" category, with subcategories for various Assembly Languages.  It seems like my new Category should also be a subcategory of "Assembly" but when I edit the "Assembly" category, I see no subcategories to add mine to.  I would like to know how to add my new category as a subcategory of "Assembly".

I also now realize it might have been better to have separate categories for the various IBM Assembly languages than a single category for all.  Being "upward compatible", they are largely the same, but there are subtle differences, the biggest being those between VSE and MVS regarding the operating system interface.  Would it be better to create those categories now (with considerable redundancy!) or wait until the best structure becomes more apparent?
:Subcategories actually have pretty much nothing to do with their parent categories except their names. I'll use an example. Say we have a category called [[:Category:Parent category|Category:Parent category]]. To make a "subcategory" you just have to add a slash to the name and then add the subcategory's name like this: [[:Category:Parent category/Subcategory|Category:Parent category/Subcategory]]. The wiki recognizes that these two pages are related because their names start with the same text (the text up to the slash) and it should put a link to the parent category on the subcategory's page (I think we have that option enabled). Other than that these are treated as totally separate categories until we link them somehow through other wiki-means. --[[User:Mwn3d|Mwn3d]] 20:18, 21 October 2012 (UTC)
:: Formally, a subcategory is a category that is a member of the parent category, i.e., that has a link to the parent category done with <tt><nowiki>[[Category:Parent]]</nowiki></tt> and not <tt><nowiki>[[:Category:Parent]]</nowiki></tt> (the colon is significant). –[[User:Dkf|Donal Fellows]] 21:44, 21 October 2012 (UTC)
