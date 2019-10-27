+++
title = "Rosetta Code:Village Pump/Language Page Links"
description = ""
date = 2010-11-17T22:38:27Z
aliases = []
[extra]
id = 5370
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Language page links
|summary=The best way of handling (automatically-generated) links from languages to the implementations of tasks in those languages.
}}
This is [[User:BR|BR]] on a library computer (soon to be at home, and I'll verify it's me then), recommending that the links on the main language pages should point to the languages example.
Example: On the [[4D]] page, there are links to all the tasks that have been completed in 4D. The top one is "[[Change string case]]". It links to [[Change string case]]. If this passes, it would instead link to "[[Change string case#4D]]" --[[Special:Contributions/66.27.48.50|66.27.48.50]] 22:48, 23 January 2010 (UTC) [[User:BR|BR]] 05:20, 24 January 2010 (UTC) (I'm at home)

:The language pages are MediaWiki categories, which just don't support that. Unfortunately. —[[User:Kevin Reid|Kevin Reid]] 22:54, 23 January 2010 (UTC)

::That's too bad... [[User:BR|BR]] 05:20, 24 January 2010 (UTC)

With the transclusion functionality added to the MCS extension, it becomes possible to create category listings that aren't in the Category namespace.  I wouldn't mind seeing that, to be honest; The Category namespace has a lot of quirky restrictions that have made some things difficult. (Such as their lack of supporting "<nowiki>REDIRECT [[some other category]]</nowiki>") --[[User:Short Circuit|Michael Mol]] 07:21, 24 January 2010 (UTC)

:I don't see how that's relevant. It's not the page namespace that's the problem; it's that there's no way of expressing “Some_page#Some_section is in Some_category”, as opposed to “Some_page is in Some_category”, whether you use standard MW categories or MCS. Am I missing something? —[[User:Kevin Reid|Kevin Reid]] 12:42, 24 January 2010 (UTC)

::Ah, yes, you're right. I can think of three solutions, though:
::* Do some JavaScript foo taking advantage of the referring page URL, and inject it into [[MediaWiki:Common.js]]. I won't write it, though; Cross-browser, production-grade JavaScript isn't among my skill set.
::* Write a MW extension to inject that JavaScript code into the relevant pages. (To be included in [[Template:Task]], for example.
::* Write a MW extension to detect a forward from another page, and then ''re-''forward to the present page+#lang. (That, at least, doesn't require JS support, but hits the server twice for the same page, ''and'' is a potential disaster if any part of the MW-to-user chain caches the wrong page.)
::--[[User:Short Circuit|Michael Mol]] 15:45, 24 January 2010 (UTC)

:::Another solution I can see is if there was some server-side language (well, I guess JS would work too..) that redirects ?lang=4D to #4D (a MW extension might work there, appending "?lang=CategoryName" to the links to things within Categories). Another thing I just thought of is adding some JavaScript to the Category template that checks for all links in the first table under the heading "Pages in Category |lang|" and append #|lang| to the href attribute. --[[User:BR|BR]]

:::I like the first option. The second is unnecessarily complex, and the third fails if, for example, a category page has a link to a specific example or other section of another page, because the server can't tell if there was ''already'' a #fragment, but JS can. —[[User:Kevin Reid|Kevin Reid]] 17:02, 24 January 2010 (UTC)

:::I'm going to try to write the JS (for shortcircuit's first option). This is by no means a discouragement for you to try as well - I'm not a JS wizard, so it'll probably take a while. [[User:BR|BR]] 03:07, 17 November 2010 (UTC)

:::Actually, it took no time at all. I'm done now. I'll post after dinner. [[User:BR|BR]] 04:12, 17 November 2010 (UTC)

::::To the problem template add:
```JavaScript
window.onload = function()
{
	if(document.referrer == '' || window.location.hash != '') return;
	category = document.referrer.match(/\/Category:([^\/]+)$/)[1];
	if(category == null) return;
	if(!document.body.innerHTML.indexOf("\"/wiki/" + category + "\"")) return; //EDIT: just added this line to try to prevent it from redirecting pages that aren't in the category (and thus don't have a solution or anchor) to the categories section. Now it isn't tested, but it should still work.
	window.location.hash = category;
}
```
 Tested with IE, FireFox, Opera, Chrome, Safari (from multiple categories to multiple problems, including "dud" categories without the code, and from non-categories). Should work with others too, nothing special was done. In IE theres a little bug - go to category, click link, it'll take you to the right section. Press the Back button and it'll take you to the top of the page, hit Back again to get back to the category. In all of them, links from any page that starts with "Category:" to any page with the problem template will attempt to redirect to #category-name (I think it's fixed now).
::::Alternatively, add the following code to the category template:
```JavaScript
window.onload = function()
{
	var links = document.links;
	var i = links.length;
	var category = document.location.href.match(/\/Category:([^\/]+)$/)[1];
	while (i-- > 0)
		links[i].href = links[i].href + "#" + category;
}
```
 I haven't tested this very well - just once in Opera from Category:1 to Problem1 (it's almost time to go to bed). It will also have the unwanted side effect of changing ALL the links on the page.
::::Beware that these might be used to inject a malicious anchor in somehow (haven't thought it through that far, but I'm convinced it could be done somehow). Also, the second one modifies  ''all'' links on a page. ˙ʇuǝɯǝʌoɹdɯı ǝɯos spǝǝu ʇı os [[User:BR|BR]] 06:34, 17 November 2010 (UTC)
