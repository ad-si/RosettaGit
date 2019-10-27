+++
title = "Talk:HTTP"
description = ""
date = 2011-09-20T21:48:43Z
aliases = []
[extra]
id = 3063
[taxonomies]
categories = []
tags = []
+++

== Clarify ==
Again, the task specification is really vague. It only says "Print a URL's content."
This rises questions:
*What do you mean by "print"? Print on paper or display on screen? There seems to be both interpretations in current implementations.
*What do you mean by "content"? The source code or rendered page?

Why is it so difficult to write even couple of sentences to specify the task? Specifying the task is the most important part of any software project.

--[[User:PauliKL|PauliKL]] 09:30, 3 October 2008 (UTC)
:Relax, it's fixed. --[[User:Mwn3d|Mwn3d]] 12:32, 3 October 2008 (UTC)
::I'm not sure I like the approach.  Wouldn't "Display the request content using the most convenient method" be more appropriate?  Some languages may not have easy access to the console. --[[User:Short Circuit|Short Circuit]] 03:54, 4 October 2008 (UTC)
:::That's OK too. --[[User:Mwn3d|Mwn3d]] 16:10, 4 October 2008 (UTC)

:Fixed? I don't understand this spec. The content of a URL is the interior of the string. What is "source code"? Where in the HTTP specification do they use the term "source code" to denote any portion of the document named by a URI?

::(Please sign your posts). Comments on the change in wording? --[[User:Paddy3118|Paddy3118]] 00:54, 20 September 2011 (UTC)
:::Please confine your attention to what is said, not who. I see the text "Paddy3118, ...", but that means nothing to me and carries no authenticity (not that any is required: you're not writing me a check).
:::: This is one example why you need to sign your posts.  You inserted a paragraph in front of someone else's comment without a sig, at the same indent level, now it's difficult to tell if the above utterance was from Kernigh or some impolite person.  If you are too good to register a user name, at least have the courtsey to sign it with your IP. --[[User:Ledrug|Ledrug]] 21:48, 20 September 2011 (UTC)
:::If you fetch an HTML file, you get HTML source code. This is not true in the general case; http://rosettacode.org/mw/title.png is not the source code of anything. --[[User:Kernigh|Kernigh]] 01:08, 20 September 2011 (UTC)
::::Yea, it locates a resource, the nature of the resource is carefully ''not'' defined. --[[User:Paddy3118|Paddy3118]] 01:25, 20 September 2011 (UTC)
::::If you fetch an HTML file, you get an HTTP response with headers, and a body which contains HTML code. Some of the progrmas in this page look like they  just dump the raw response, whereas others show the body.

== Not Tested ==

Well..someone can test C# and Ruby examples?
They are working?

ruby is working [[User:Rahul|Rahul]] 18:47, 5 October 2008 (UTC)

C# (on Mono) is working --[[User:ShinTakezou|ShinTakezou]] 18:34, 27 January 2009 (UTC)
