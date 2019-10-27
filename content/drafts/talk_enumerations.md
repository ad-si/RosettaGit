+++
title = "Talk:Enumerations"
description = ""
date = 2011-03-18T00:43:39Z
aliases = []
[extra]
id = 4908
[taxonomies]
categories = []
tags = []
+++

== J concerns ==

I'm not sure how having a header template and an omit template for the same language on one page will behave. If we find a way to do counts of omitted tasks and completed tasks later, this may screw things up, but for now I guess it's OK (especially since the explanation is a bit more than the usual HTML comment). --[[User:Mwn3d|Mwn3d]] 13:25, 20 October 2009 (UTC)
:Oh, I just assumed this was the "proper" way to omit a task.  Use the omit template, but include a brief explanation of why the task isn't suited to the language -- I think this gives nearly as much useful information about the language as a "solved" task.  Not that I have any say in the matter, but I recommend that the ImplSearchBot gives "omit" priority over "header", such that if both are present, the language is counted as omitted. --[[User:DanBron|DanBron]] 14:06, 20 October 2009 (UTC)
::Yeah I don't think we really have solid directions on how to use the omit from template yet. The whole ImplSearchBot idea is kind of in flux right now. Maybe once we decide how (or if) we want ImplSearchBot to work we can nail down how the templates should work. --[[User:Mwn3d|Mwn3d]] 14:13, 20 October 2009 (UTC)
::: The MultiCategorySearch extension is The Way To Go right now. There are still a bunch of mundane tasks that a bot could automate, though.  Like creating Reports: pages for each language, or automatically creating category pages when languages show up.  I had the idea Sunday night of having one bot do ''all'' of those things, and have its workload tuned by way of reading its instructions from a protected wiki content page.
::: The Reports: pages are already tunable in behavior by modifying the arguments for the MCS transclusions. Fix it however it needs to be fixed.  The RCBF, RCSNUSP et al pages look like they're going to need a reorganization, though; subcategories don't count as being ''in'' a category.  That's a limitation of the MW database schema, would incur extra DB processing cost to work around, and opens a can of worms regarding recursive diving into categories. --[[User:Short Circuit|Michael Mol]] 16:24, 20 October 2009 (UTC)

== Javascript/JSON ==

I'm getting a syntax error for "<tt>{ apple, banana, cherry }</tt>", both Javascript and JSON variants. Is there some flavor that supports this syntax? --[[User:MizardX|MizardX]] 12:39, 27 May 2010 (UTC)

== Bad task description? ==

The task description says: ''"Create an enumeration of types with and without values."'' But none of the solutions seem to do that. Some of the solutions seem to create an enumeration of integers.

If I wanted to create an enumeration of types, I would do something like: 
```ruby
# Ruby
[String, Integer, Float, File, Thread].each { |type| puts type }

{String: "a sequence of bytes or characters",
 Integer: "a number like 33",
 Float: "a number like 33.0",
 File: "an open file",
 Thread: "a concurrent task"}.each { |type, value| puts type, value }
```


--[[User:Kernigh|Kernigh]] 00:44, 15 March 2011 (UTC)
:I think the word "types" is getting you. Maybe it should just be "an enumeration of fruits" since most of the examples seem to use apples, bananas, and cherries. --[[User:Mwn3d|Mwn3d]] 00:47, 15 March 2011 (UTC)

:Or maybe it should say ''"an enumeration of constants with and without '''explicit''' values"''. If you look at early revisions, you see that the task is inspired by typical enums of C-like languages. -- [[User:Wmeyer|Wmeyer]] 01:25, 15 March 2011 (UTC)

If you tell me that an apple is a type of fruit, then I understand the task. But saying "an enumeration of constants" might be better. --[[User:Kernigh|Kernigh]] 04:12, 15 March 2011 (UTC)
:Hi Kernigh. You might try looking at older versions of the page using the history tab. Gauge the total of what the current entries seem to be interpreting and then maybe coming up with a better wording? If it is your first edit to a task description then you could try it out here on the talk page and ask for comments - I find that helps me sometimes. :-)
: --[[User:Paddy3118|Paddy3118]] 06:04, 15 March 2011 (UTC)

: I changed the task according to my suggestion. I hope that's okay. -- [[User:Wmeyer|Wmeyer]] 00:43, 18 March 2011 (UTC)
