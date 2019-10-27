+++
title = "Template talk:Collection"
description = ""
date = 2009-11-04T03:51:33Z
aliases = []
[extra]
id = 4945
[taxonomies]
categories = []
tags = []
+++

It would be really nice if we were to tag collection members as being part of a category (of collection members!) so that scripts that work out what tasks have been implemented as parts of collections can do so systematically rather than by guessing on the basis of naming patterns. –[[User:Dkf|Donal Fellows]] 16:06, 3 November 2009 (UTC)
:I'm not sure what that means. Can you give an example using one of the actual collection members? --[[User:Mwn3d|Mwn3d]] 16:16, 3 November 2009 (UTC)
:: Putting, e.g. [[:Category:RCBF]] in [[:Category:Collection Members]]. --[[User:Short Circuit|Michael Mol]] 21:53, 3 November 2009 (UTC)

:: [[:Category:RCBF]] is a good example, as it is a task that nobody satisfies directly. There is just no nice way to determine that it is a collection and that, say, [[RCBF/Perl]] is a page that results in that task being satisfied by the [[Perl]] language. What is needed is a way to mark the masters as specially magic (i.e., needing this treatment) and/or the sub-pages as being special. Right now, the simplest way seems to spot that the task name begins with “Category:RC”, but that's inelegant. Adding categories would do the marking quite handily, and make it possible to do things like [[Find unimplemented tasks]] just by fetching suitable category memberships and doing set operations, or at least without only minimal magic over that foundation. In short, I'm seeking explicit codification of the magical relationship between the collection members and the collection, rather than the current rather ''ad hoc'' method we are using now. (I could claim that it is more Semantic-Web-ish, and so invoke the magical god powers of Berners-Lee, but that's probably over-reaching...) –[[User:Dkf|Donal Fellows]] 22:52, 3 November 2009 (UTC)
::: Done.  Incidentally, this is the kind of thing that could probably have just been moved forward on. --[[User:Short Circuit|Michael Mol]] 03:51, 4 November 2009 (UTC)
