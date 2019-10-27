+++
title = "Talk:Rosetta Code/Find unimplemented tasks"
description = ""
date = 2010-02-06T13:22:58Z
aliases = []
[extra]
id = 3362
[taxonomies]
categories = []
tags = []
+++

About: "Since most of these other implementations seem to be fetching tasks already implemented, I did the same..."

All examples are working. Except AutoHotkey, that is getting all implemented tasks from a language. --[[User:Guga360|Guga360]] 01:37, 28 May 2009 (UTC)

Oops, I didn't see the part about getting all tasks, in my translation... 

Its fixed now.  Thanks.  [[User:tinku99|tinku99]]

== Server load ==

<s>Unfortunately, a large number of server requests lately appear to be from this task.  I'd like this task to be suspended until I provide an alternate resource for the category data.  (ImplSearchBot already generates JSON files for each category, but I don't have them in a location the httpd can see them from.) --[[User:Short Circuit|Michael Mol]] 10:02, 3 September 2009 (UTC)</s>
: Task rewritten to take advantage of "static" files. --[[User:Short Circuit|Michael Mol]] 04:35, 14 September 2009 (UTC)
:: The "static" (JSON) files mentioned in the task description are no longer available. The README file in the linked folder suggests that "calling MediaWiki's API for JSON data shouldn't be as painful as it once was". Should the task description be reverted to use the MediaWiki API again? --[[User:Tikkanz|Tikkanz]] 00:35, 29 January 2010 (UTC)
::: That's what I would recommend.  Sorry about not leaving a note here before. (The script that was updating the JSON files was consuming 18M of RAM, and I needed that to deal with StumbleUpon while trying to get other parts of the server configuration tuned.  One consequence of the tuning is that dynamic JSON generation should be much faster, if MediaWiki takes advantage of caching there.) --[[User:Short Circuit|Michael Mol]] 04:54, 29 January 2010 (UTC)
:::: OK, have restored task description to pre-JSON-source-file state. --[[User:Tikkanz|Tikkanz]] 22:28, 29 January 2010 (UTC)
