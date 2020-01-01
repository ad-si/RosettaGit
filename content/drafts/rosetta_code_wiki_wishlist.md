+++
title = "Rosetta Code:Wiki Wishlist"
description = ""
date = 2009-07-30T11:48:37Z
aliases = []
[extra]
id = 2657
[taxonomies]
categories = []
tags = []
+++

What do you wish Rosetta Code's wiki did?

==Granted wishes==
* Language expertise registry, like the userboxes on Literate Programs. --[[User:IanOsgood|IanOsgood]] 20:55, 5 March 2008 (MST)
:I tried to make something like that based on things I saw on wikipedia. You can check it out and change it: [[Template:User]]. You can see it in use on my userpage. --[[User:Mwn3d|Mwn3d]] 19:27, 5 March 2008 (MST)
::The new [[Template:Mylang|mylang templates]] look better. These should be used instead. I tried. --[[User:Mwn3d|Mwn3d]] 08:09, 26 March 2008 (MDT)
*I wish div boxes didn't mess up the edit links on a page.
:Fixed for userboxes only. You need to put your userboxes in between <nowiki>{{usertop}} and {{userbottom}}</nowiki> now.
* I wish [[C++]] worked
: This is was formerly an Apache problem, now it's just an Apache configuration problem.  I might be able to spend a few minutes this weekend to get it working. --[[User:Short Circuit|Short Circuit]] 09:28, 11 July 2009 (UTC)
* I wish the forum (village pump) and the wiki were integrated (especially, single login for both, but also the forum style sheet might be adapted to fit better to the wiki look)
*I wish the links in the Java syntax highlighting went to Java 1.5 (or higher) JavaDocs (use java5)
* [[:Category:Unimplemented tasks by language]]
** Definitely something like "list of all tasks for which there is no solution in language-X yet".[[User:Sgeier|Sgeier]] 18:20, 5 March 2008 (MST)
** Way to see which tasks are ''not'' performed by a language (sort of an inverse category: (Solutions by Language) - (language category) ).

==[[User:Short Circuit|Short Circuit]]==
* I wish there was a quick way to pump out pages in LaTeX
* I wish there was a way to go to a language page, click on a task link, and jump to that language for that task
: Should be fairly with Javascript by peeking at the referring URL.  Probably trivial.  If someone provides me with a .JS, I'd likely add it. --[[User:Short Circuit|Short Circuit]] 09:38, 11 July 2009 (UTC)
:: Try this:
```javascript
document.location = document.location + "#" + document.referrer.split("/").slice(-1);
```
 --[[User:Guga360|Guga360]] 22:03, 29 July 2009 (UTC)
* I wish there was a clean (i.e. easily editable) way to share code between pages
** Something like WP infobox template transclusion? --[[User:IanOsgood|IanOsgood]] 19:29, 9 July 2009 (UTC)
*** I don't know; I haven't played with that feature. --[[User:Short Circuit|Short Circuit]] 09:38, 11 July 2009 (UTC)
* I wish there was a way to display a cross-section of the Solutions by Language category and the Solutions by Task category
** Sorta done with ImplSearchBot.  Not likely to be done in a complete fashion, as the matrix is too large, though a paginated version is plausible.
* I wish pages could include a list of "what links here", rather than have it as a link on the side
** Should be doable with a theme that uses JS to query api.php. --[[User:Short Circuit|Short Circuit]] 09:38, 11 July 2009 (UTC)
* I wish [[C#]] worked
** Not likely to happen at all with pretty URLs, as the # gets interpreted as the basis for an anchor target.  However, it may be possible to pre-hook Apache's processing of <nowiki>[[link]]</nowiki>, and have it use a non-pretty link instead of a pretty one. --[[User:Short Circuit|Short Circuit]] 09:38, 11 July 2009 (UTC)
* I wish it were possible to give libraries aliases. (i.e. [[GTK]], [[Gtk]], [[GTK+]], [[Gtk+]], [[Gtk2]] and [[PyGTK]] are all pretty much the same.)
* I wish there was a program to convert Vim syntax highlighting scripts into PHP files, so that we might have more robust syntax highlighting.
** This isn't so necessary any more; I've got a much better understanding of GeSHi, a langfile generator wizard is in the works, and I'm getting langfile submissions every week. --[[User:Short Circuit|Short Circuit]] 09:38, 11 July 2009 (UTC)

==[[User:Mwn3d|Mwn3d]]==
* I wish the Google search were built into the page and didn't log me out
** Is the Google search still logging you out?  It hasn't done that to me.  However, I could probably add an embedded Google search widget below the MediaWiki search box. --[[User:Short Circuit|Short Circuit]] 09:43, 11 July 2009 (UTC)
* I wish links weren't case-sensitive
* I wish a category's links on pages would disappear when the category was deleted
* I wish I could hit the tab key while adding code without the cursor moving to the summary box
* I wish there were an easier way (or that people participated in existing easier ways) to debate about content on pages--talk pages get crowded quickly
:*The Wikipedia solution is to archive old conversations to talk archive pages. And there is always the Village Pump.
::* The Wikipedia's Village pump is simply a set of pages dedicated to talk, and so archived.  I had to go with forum software because I didn't know how to set up a bot to do the archiving.  It could be worth looking into again. --[[User:Short Circuit|Short Circuit]] 21:46, 3 March 2008 (MST)
* I wish there were more stats available for use in the content of pages
:*Number of members in a category
::*<nowiki>{{PAGESINCATEGORY}}</nowiki> magic word
:* Number of uses for a template
::*Like [[Special:Mostlinkedtemplates]]?
:::Not like that. Short Circuit had asked a while ago on IRC for a way to count the number of times <nowiki>{{header}}</nowiki> was used so he could tell how many examples there were on the site. That page only shows how many pages use a template, instead of how many times a template is used. --[[User:Mwn3d|Mwn3d]] 17:28, 4 March 2008 (MST)
:* User-specific stats like number of edits (with options to filter out talk edits and minor edits)
::*I've seen such things on Wikipedia. Also, total edits are listed on your preferences page.
:::This wish is low priority for me. I just thought it'd be cool to put the number of edits I had on my userpage. --[[User:Mwn3d|Mwn3d]] 17:28, 4 March 2008 (MST)
* I wish you had to preview an edit to make it
** This can be done in your preferences.  I wouldn't want to force it site-wide. --[[User:Short Circuit|Short Circuit]] 09:43, 11 July 2009 (UTC)
*I wish div boxes didn't mess up the edit links on a page.
** Is this still happening? --[[User:Short Circuit|Short Circuit]] 09:43, 11 July 2009 (UTC)
::Yep. Look in the [[sandbox]] at the fourth and fifth task divs that I put on there. There are a bunch edit links next to them that came from other headings. --[[User:Mwn3d|Mwn3d]] 17:58, 11 July 2009 (UTC)
::Now that I look at it I don't think that shows it. I forget what did it before. I'll have to experiment more later. --[[User:Mwn3d|Mwn3d]] 18:51, 11 July 2009 (UTC)
*I wish that if a category were redirected to another category, its members would be added to the new category. Ex: Page1 is a member of Category:Cat1, then Category:Cat1 gets blanked to say <nowiki>"REDIRECT [[:Category:Cat2]]"</nowiki>, Page1 should now be a member of Category:Cat2.
*I wish that sorting of category members ignored case (ex: GUI comes before Games in Solutions by Task).
:Add a <nowiki>{{DEFAULTSORT:key}}</nowiki> to the page.--[[User:Mwn3d|Mwn3d]] 20:52, 29 July 2009 (UTC)
*I wish MW could automatically change the edit comment link from <nowiki>"{{header|Lang}}"</nowiki> to "Lang".
::When editing, so that the "comments" in the changelog are right and the redirect link at the end goes to where you were editing? That would be nice. —[[User:Dkf|Donal Fellows]] 21:43, 15 June 2009 (UTC)

==[[User:Ce|Ce]]==
* I wish there were syntax highlighting for C++ (the C highlighting doesn't know C++ keywords)

==[[User:IanOsgood|Ian Osgood]]==
* More outreach to other programming language communities. Most languages haven't seen new examples for a year. Maybe via comp.lang.* postings, IRC, and programming language portals.
* Category for starter tasks which should be easy for new contributors.
* Merge or delete similar tasks.
* Delete empty and singleton language implementation categories.
* [[Help:Style Guide]], demonstrating the preferred formatting for each Rosetta Code language. (Or would that be a declaration of Edit War?)
:I think we would get a lot of unnecessary nerd fights about curly braces and whitespace. Maybe if RC invented a language we could make a standard. --[[User:Mwn3d|Mwn3d]] 13:11, 8 March 2008 (MST)

==[[User:Sgeier|SGeier]]==

* As somewhat more fine-grained tagging system would be cool. Say, tag all tasks that only make sense for OO languages so as to be able to look at only (or all but) those tasks. Or everything that has to do with GUI programming. Or file I/O. Ideally coupled with all the other functions, so I could have a "list of everything that Rosetta Code knows about doing file I/O in Ada" or such.

==[[User:Rahul|Rahul]]==
* I wish there was a way to see the entries in each of the programming tasks for a particular language in a single page (not as links as is currently)
* I wish when clicking on a task from a language page, it was linked to the anchor of that particular implementation in the task page rather than to just the task page.
* I wish there was a way to see which all languages and to which all tasks a user has contributed, linked from the users page. [[User:Rahul|Rahul]] 09:40, 25 September 2008 (UTC)

==[[User:ShinTakezou|ShinTakezou]]==
* I would like Search form would work like a search is supposed to work; e.g. if I type in "sort", it must not say me simply that there's no such a page...
* It is me who can't see it, or there's no a ''add a section'' button that would avoid the editing of a whole page just to add a section? (Already existing section can be edited apart, but how to create a new one?)
::Talk pages have one. Standard pages don't, since they're not normally appended to. —[[User:Dkf|Donal Fellows]] 21:46, 15 June 2009 (UTC)
::: It was an ancient doubt of mine. I understood lately that in "common" pages the absence of + help keeping the programming examples sorted; anyway it would be nice if a mediawiki setup would allow to autosort == sections... or maybe it's just extra cpu load that can be avoided. --[[User:ShinTakezou|ShinTakezou]] 14:09, 16 June 2009 (UTC)
:::: Yeah. Manual sorting is easy enough (especially given current update rates) that there's really no need to put in machinery to do it; not enough gain to justify the pain. —[[User:Dkf|Donal Fellows]] 15:18, 16 June 2009 (UTC)
::: Automatically sorting language examples would be an appropriate task for a bot. --[[User:Kevin Reid|Kevin Reid]] 22:07, 11 July 2009 (UTC)

==[[User:Kevin Reid|Kevin Reid]]==
* Recent Changes feed with more entries than just 25, so that it's easier to keep up to date with ''all'' edits (since any one of them could e.g. be a change to task reqs I need to know about, or a discussion point I want to contribute to, or ...); as it is, new edits often fall off the end of the feed when I check it in the morning. Watching isn't helpful because I would have to watch many pages, and the watch notification mail doesn't include diffs like the feed does. The obvious URL to use is http://rosettacode.org/mw/index.php?title=Special:RecentChanges&feed=atom&limit=200 , but that still returns only 25. --[[User:Kevin Reid|Kevin Reid]] 11:48, 30 July 2009 (UTC)
