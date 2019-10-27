+++
title = "Rosetta Code:Village Pump/Link to task-section"
description = ""
date = 2014-10-28T18:16:30Z
aliases = []
[extra]
id = 18158
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Link to task-section
|summary=Change task-template, to create a link to the first section of page
}}
Currently, the very first section of task-pages have no edit-link. 

So, to make edits there (e.g. to add a new category), 
the whole page has to be edited. 

This is unfortunate, because most task-pages are quite big.

I made an attempt to do this, see Template:Task2.

For testing, I'm using one of the shorter and less popular tasks, 
[[Create a file on magnetic tape]]

This gets a link and header "(Task)" at the top of the contents,
but the edit-link points to the template.

Also, it would be nice to give that link the number 0,
so the link to the first solution gets the number 1.
:That looks pretty ugly and it moves the task box (on the right) to a weird place. What was wrong with my suggestion from before that seems to have been erased? Just click the edit link on the first section and then add your desired categories above the <nowiki>==Header==</nowiki>. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 01:11, 27 October 2014 (UTC)
:: FYI, You can edit text ''before'' the first section by editing the first section, then changing the <code>&section=1</code> part of the URL to <code>&section=0</code>. (from [[wp:Wikipedia:Page_size#If_you_have_problems_editing_a_long_article|Wikipedia's help]]). Edit: actually you could also use the whole page edit link and then add <code>&section=0</code> to the end of the URL as well, although that will start loading the edit window with the whole document (which for long documents was what the WP help page was trying to avoid).
:: Also, you can reposition the contents with <code><nowiki>__TOC__</nowiki></code>. But that's a lot of mucking just to get an edit link IMO. &mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 06:31, 27 October 2014 (UTC)
