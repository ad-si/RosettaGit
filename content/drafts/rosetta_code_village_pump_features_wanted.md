+++
title = "Rosetta Code:Village Pump/Features Wanted"
description = ""
date = 2011-06-26T12:55:59Z
aliases = []
[extra]
id = 4489
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Features Wanted
|summary=A place to put the features we want.  Please keep each feature in its own section.  Some of these may be doable by bot.  Others may be doable by writing MediaWiki extensions to fill additional namespaces.
}}
==Logically Separate Code Samples==

The absolute biggest thing I would like would be able to do something akin to "SELECT code FROM table WHERE (some expression tying language, library, paradigm, family, or a version of which.)".  The best way I can think of to do this is to make each code sample a separate piece of information, associated with a set of tags. --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)
: I'm not convinced that that's a good idea. Sometimes a language solves a problem in a more discursive style (i.e., interleaved text and code) that makes doing that sort of thing much more awkward. Cut-n-paste works... —[[User:Dkf|Donal Fellows]] 10:05, 4 August 2009 (UTC)

==On-server, on-domain language documentation==

I'd like to be able to link to documentation on-domain where such documentation is retrievable progrommatically and legally.  With, e.g. the output of perldoc -f, or kernel syscalls, etc formatted for display on-site and associated with related content on the site. --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)

==Keyword indexing==

"See other code samples that use this keyword" --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)

==Per-language RSS or twitter feeds==

Whenever a task gets added, twitter it in the "new tasks" feed.  Whenever a language gets added, twitter it in the "new languages" feed.  Whever a new task is solved in a language, twitter it in ''that language's'' feed. --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)
: This is ''possible'' now, due to the JSON category data export.  I'm considering Atom+PubSubHubbub. --[[User:Short Circuit|Michael Mol]] 17:52, 13 September 2009 (UTC)

==Rosetta Planet==

I'd like to be able to pull in blogs from members of the Rosetta Code community that relate to Rosetta Code and to programming. --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)
: This is implemented.  I'm looking for volunteers who'd like (or, at least, wouldn't mind) their blogs aggregated.  Preferably using a specific Atom/RSS feed for articles that would like to show up in the planet. --[[User:Short Circuit|Short Circuit]] 04:09, 4 August 2009 (UTC)
:Will you add it to the xfeeds on the main page eventually? --[[User:Mwn3d|Mwn3d]] 14:48, 4 August 2009 (UTC)
::Once I've got some more blogs in there.  I'd rather my gripes about the C++ standard license be a little farther down the list. :) --[[User:Short Circuit|Short Circuit]] 01:29, 5 August 2009 (UTC)

==Per-language news feed aggregation==

I'd like each language page to be able to display RSS feeds related to that language. --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)

==Better commenting system==

I hate to say it, but the MediaWiki page editing format is horrible for maintaining threaded conversations.  I'd ''love'' to have something that could integrate IRC, talk/VP pages, email and even NNTP. --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)

==Better data export==

I'd love to have cached copies of MediaWiki data available for quick export, even if the cache is only updated daily or weekly. --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)
: Category data is [http://rosettacode.org/json/isb/ currently available]. --[[User:Short Circuit|Michael Mol]] 17:49, 13 September 2009 (UTC)

==Semantic Pages==

I'd love for pages served up by the wiki side of things to be semantically arranged, to simplify programmatic access and client-side acceleration of their use. --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)

==More powerful theme==

I would like a wiki theme that is more aware of Rosetta Code's structure, such as prominent display of the categories a page is in, and in-page exploration of related categories without changing the current page. --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)
:How about custom "Add an example" and "Add a language" GUIs? They would make some of these other features easier (for example, tagging examples with libraries, languages, and versions), and they would allow for more uniformity across pages (alphabetization and standard lang tags, works with, and libheaders). I don't know how hard that would be to make though. Watch out for SQL injection ;). --[[User:Mwn3d|Mwn3d]] 20:39, 13 July 2009 (UTC)

:Also, how about an "up one level" button/tab for subpages (like Loop/* and Average/*)? --[[User:Mwn3d|Mwn3d]] 12:55, 17 August 2009 (UTC)

::That's a MediaWiki feature that needs to be turned on, see [http://www.mediawiki.org/wiki/Help:Subpages MW Help:Subpages]. The link doesn't show if the 'parent' page doesn't exist. --[[User:Kevin Reid|Kevin Reid]] 13:44, 17 August 2009 (UTC)
::: Just noting that it was turned on for the main namespace last month (at Kevin Reid's request).. --[[User:Short Circuit|Michael Mol]] 17:51, 13 September 2009 (UTC)

==Mobile theme==

I would like a stripped down version of the wiki interface for mobile environments. --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)

==Non-Web Interface==

I would like for there to be an interface for interacting with Rosetta Code that doesn't depend on having a web browser.  A console-based interface using ncurses would be slick, or even a standalone XUL-based app. --[[User:Short Circuit|Short Circuit]] 02:52, 10 July 2009 (UTC)
::This may be a stupid question, since I don't understand the goal you're trying to achieve with that; but what part of this request is NOT served by Lynx running in a console? [[User:Sgeier|Sgeier]] 18:15, 8 October 2009 (UTC)
::::  An interesting question.  Probably the biggest reason is due to some of the limitations of HTML web design; There are three sets of important links on each page, and each set of links winds up amassed prior to the page body.  Browsers like w3m work slightly better, but are still clunky for navigating pages. Having function keys or other keyboard shortcuts corresponding to those menus and common page editing behaviors would improve my workflow on MW sites.
::::
:::: There are other features that would come in handy as an editor, such as an abbreviated list of of recent changes in-page, quick assess to a listing of category membership and intrawiki, interwiki and outbound links.  Those aren't part of the default data that gets served up with a wiki page.
::::
:::: I'm loathe to implement such things via JavaScript as part of the theme.  I've been down the theme modification path with the Rosetta theme, and theme modifications make MW upgrades ''very'' painful.  I've even brought the subject up in ##mediawiki, and was told that you're not supposed to modify those themes.  Then there are the reactions one gets from using a client-side language as part of a web page.  --[[User:Short Circuit|Michael Mol]] 21:24, 8 October 2009 (UTC)
::I rather agree with the idea of <s>extending</s> using some already-existing chunk of software. One possibility that comes to mind would be using the Midnight Commander sources to, say, give the links in the left pane, and the content on the right (or something). mc already has all the code needed for a project like this; all that's required is some fat-trimming, and some server-side code to access the db. -- [[User:Eriksiers|Eriksiers]] 22:49, 8 October 2009 (UTC)

==Powerful Search==
It would be nice to have a search that could search only examples in a specific language and/or only pages in a language's category. This may be related to [[{{FULLPAGENAME}}#Logically Separate Code Samples|Logically Separate Code Samples]]. --[[User:Mwn3d|Mwn3d]] 17:37, 15 September 2009 (UTC)

==Simple features==
- I'd like a page that shows all tasks done with a language.
- I'd like a language page like:
http://rosettacode.org/wiki/Category:Lisp
to contain links as:
http://rosettacode.org/wiki/Empty_program#Lisp
Instead of:
http://rosettacode.org/wiki/Empty_program

: Something like that is in the works, with the help of Semantic Mediawiki. We've got a few bugs to work out first, however. --[[User:Coderjoe|Coderjoe]] 02:51, 22 August 2010 (UTC)

:: The first query should already be possible: {{#ask: [[Category:Programming Tasks]] [[Category:Lisp]] | limit=3}}
::: This page http://rosettacode.org/wiki/100_doors shows all languages for a task, but I'd like to see the opposite, a page that shows all tasks for a language (not jut a list, with all the inlined source code too).
:: Of course you would put that into a better format, and would make a nice page out of it. --[[User:Ce|Ce]] 07:55, 22 August 2010 (UTC)
:::Those links don't go right to the example though. You need to add the anchor on to the ends of the links. --[[User:Mwn3d|Mwn3d]] 15:37, 22 August 2010 (UTC)
:Check http://rosettacode.org/wiki/Rosetta_Code:Village_Pump/Language_Page_Links - it's been recommended and two possible solutions (in code) and a few others (in theory) have been made. [[User:BR|BR]] 20:39, 15 December 2010 (UTC)

- I'd like to do side-by-side comparisons between pairs of languages.  For example,  I know Python very well but am still learning Scheme.  Being able to easily view side by side examples would be a tremendous time saver compared to having to scroll between the Python and Scheme entries for each example.  BTW, this site is a terrific idea.  My thanks to all the implementors!
:If you sign up for an account and then copy [[User:Tyrok1/monobook.js]] to the same subpage of your userpage (if your username was "username" then it would go to User:Username/monobook.js, and mine is at [[User:Mwn3d/monobook.js]]), you get a neat little "my scripts" option at the top of every page. If you check "language comparison" then checkboxes will appear next to language names in the tables of contents on task pages. You can then check a couple of those and only those languages will show on the page. It's not side-by-side, but there's a lot less scrolling. Plus it's nice to have an account and a user page to decorate. --[[User:Mwn3d|Mwn3d]] 13:35, 25 March 2011 (UTC)

== Webchat fix ==
In file /extensions/WebChat/WebChat_body.php, line 40, change the width from '600' to '100%'.  Also, in function webChatExpand on line 52, comment out the two lines that automatically resize the webchat applet to the size of the screen.  Doing this helps prevent an excessivly wide window, especially on those 2048x1152 displays that are popular right now.  For those with Mediawiki bugzilla access, you can also see a patch at [https://bugzilla.wikimedia.org/show_bug.cgi?id=29592 bug 29592]. --[[User:Sigma 7|Sigma 7]] 12:28, 26 June 2011 (UTC)
: Done --[[User:Short Circuit|Michael Mol]] 12:55, 26 June 2011 (UTC)
