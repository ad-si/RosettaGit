+++
title = "Help talk:Adding a new programming language"
description = ""
date = 2008-10-27T12:55:00Z
aliases = []
[extra]
id = 3090
[taxonomies]
categories = []
tags = []
+++

== Redirect? ==
What is this thing with redirect? I tried to insert redirect command as described, but it would cause a redirect to the page itself.
--[[User:PauliKL|PauliKL]] 15:02, 23 October 2008 (UTC)
:The "My Language" should be replaced with the category title that you want to redirect to. "Add the following text, replacing My Language with the name of your programming language"

:The link is actually the page that you want to redirect to. For instance, if I wanted a page to redirect to my user page, I would add <nowiki>REDIRECT [[User:Mwn3d]]</nowiki>. If I wanted to redirect to [[IEEE]], I would add <nowiki>REDIRECT [[IEEE]]</nowiki>. Look at [http://rosettacode.org/w/index.php?title=Vedit_macro_language&oldid=18520 this page] for another example. --[[User:Mwn3d|Mwn3d]] 15:55, 23 October 2008 (UTC)

::Yes, I know that redirecting to some page redirects to that page. The question is: '''why''' would I want to redirect? And from where?
::According to this help page, clicking a red link in a task page would lead to some page. From there you are supposed to redirect to ''another'' page. But the problem is that the first page is already the page where you are supposed to redirect to. That is, you are redirecting to the page itself.
::I am guessing that you are supposed to redirect from page "some language" to page "category:some language". But it looks like the links in the language headings in the task pages already point to "category:some language".
:--[[User:PauliKL|PauliKL]] 10:15, 27 October 2008 (UTC)
:::The redirects from "some language" to "category:some language" in the example headers are not user generated. They are part of the [[Template:Header|header template]]. The purpose of a redirect from the main namespace to the category namespace is for convenience so that when I want to link to the Java page all I need to type is <nowiki>[[Java]] rather than [[Category:Java]]</nowiki>. Also, the main content of the language pages is on the category page. That's where all of the tasks are listed, which is the main purpose of having a language page. With regard to this help page, it seems that about a year ago the header template was changed to create links to the category page rather than the main namespace page, but this page was never changed to reflect that. Everything seemed to be going pretty smoothly since then so I guess we never noticed. I'll change it. --[[User:Mwn3d|Mwn3d]] 12:55, 27 October 2008 (UTC)
