+++
title = "Rosetta Code:Village Pump/More convenient links to tasks"
description = ""
date = 2015-07-07T22:25:35Z
aliases = []
[extra]
id = 17158
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=More convenient links
|summary=How about better task links on each programming language page?
}}

Currently, if you're browsing the page of a programming language, and click on the tasks which are already implemented in the language, an unexpected behavior occurs: the link takes you to the entire page for the task as a whole, rather than to the task solution for that language. You then have to scroll through the page to search for the solution for that language.

It would be better if these links included the #lang part in the URL to jump to the example in that language.
[[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 07:21, 2 February 2014 (UTC)

:But sometimes you need to know the detail of the task. If you go to the top then their are links just below to all the languages; If you go to the language then there is no link to the top? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:29, 2 February 2014 (UTC)

::The name-based link (# syntax in the URL) to the solution in the given language should put you somewhere in the middle of the page for that task such that the solution for the given language is in view. If you want the task description from there, you can just scroll to the top. This is usually fairly convenient: e.g. use the Home key in some browsers, or hold down PgUp, or in some other way scroll rapidly upward until you hit the top. You don't have to search for anything. --[[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 01:04, 3 February 2014 (UTC)

:::What if an additional option, like a small icon, is added at the end of the link. The text link would stay the same but the icon link would allow users to jump straight to the referenced location (# syntax in the URL). Ultimately, this gives the user the choice of how to approach the linked page. --[[User:Relish|Relish]] ([[User talk:Relish|talk]]) 21:41, 10 September 2014 (UTC)

:::: That would be better than the current behavior, but still suboptimal. I think the best behavior would be to link to the section, with an icon to go to the general page; next best would be to link straight to the anchor; next would be linking to the general page with an icon for the anchor; worst is the current situation. -- [[User:CRGreathouse|CRGreathouse]] ([[User talk:CRGreathouse|talk]]) 19:28, 14 September 2014 (UTC)

::::: What if we changed the template for the solution header so that it included a link back to the task description/top of the page?  Then the implementation links on the language page could link directly to the language solution, and there'd be a link right there to go up top to read the task description. [[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 04:40, 11 February 2015 (UTC)

:: My take on how to solve this issue is to '''introduce collapsible language sections'''. If entering a task without the language tag, all the language sections could be collapsed with options to open them. If entering using a tag, then the general description of the task and the tagged language would be open. --[[User:Holroy|Holroy]] ([[User talk:Holroy|talk]]) 19:15, 5 March 2015 (UTC)

::: I wrote a blog entry, [http://oddsbyeven.blogspot.com/2015/03/bookmarklet-idea-collapse-all.html Boomarklet Idea - Collapse all subsections], to illustrate my concept of collapsing language sections. --[[User:Holroy|Holroy]] ([[User talk:Holroy|talk]]) 21:16, 5 March 2015 (UTC)

:::: Ideally, if this were implemented, it would be implemented in the wiki software we use here (which I believe is [[mw:Gerrit|mediawiki]]). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:25, 7 July 2015 (UTC)
