+++
title = "Rosetta Code talk:Add a Task"
description = ""
date = 2019-06-01T01:34:20Z
aliases = []
[extra]
id = 8338
[taxonomies]
categories = []
tags = []
+++

I've been asked numerous times about how to create a task. I've never had a good answer. This page represents a draft of my best idea of it.
*Please examine, edit, refine, review and debate it.
*Go through our existing set of tasks and use this page as a benchmark for gauging task quality. If the task seems of good quality (i.e. matches the prerequisites), despite not matching this page well, then the page likely requires modification. 
*Build a list (somewhere, anywhere, wikicode or semantic query, be as practical or creative as necessary) where we can see our existing task set and see how well the task description matches this page. --[[User:Short Circuit|Michael Mol]] 17:41, 20 September 2010 (UTC)

==First implementation==
I certainly like the requirement for an implementation. It almost always makes things much easier for other implementers if they can study a working example. Perhaps the requirement for graduating a task from draft status should be taken to be multiple implementations of it and a consensus that the task is clear enough (typically formed by having multiple implementations that people can agree are all correct). –[[User:Dkf|Donal Fellows]] 21:06, 21 September 2010 (UTC)

: Hi Donal, I would think that you don't actually have a task, draft or not, without that first implementation. They go together for me.
 I don't often use the draft task status, and its usually when I find something that interests me, but I am unsure of wider interest, or might shift the focus of a task, as in [[Talk:Simple Quaternion type and operations]], and [[Talk:Short-circuit evaluation]], and [[Talk:Extreme floating point values]], and [[Horner's rule for polynomial evaluation]] - started as a draft I think because I couldn't get the formatting of the equations in the description right. --[[User:Paddy3118|Paddy3118]] 05:47, 22 September 2010 (UTC)

==Other Algorithms==
There needs to be a mention of the use of 'other' algorithms when the task allows - for example using regexps instead of an example using string search; or some other technique when brute-force search is the current solution method. We usually welcome them -sometimes on their own - other times in addition to other methods of solution. (Various [[Knapsack problem]] solutions have better methods than brute-force search for example). --[[User:Paddy3118|Paddy3118]] 15:59, 8 October 2010 (UTC)
: Suggest creating a "what to expect" section, noting common variations and the like, noting that the task author is likely to be surprised by creative (but not necessarilly inappropriate) solutions. "other algorithms" would fall under that, pretty much as a single bullet point. --[[User:Short Circuit|Michael Mol]] 16:24, 8 October 2010 (UTC)

== Semantic annotations? ==

Could we get a bit more on semantic annotations?  I am not sure if I have ever seen them before here.  And the illustrative example does not help me since, when I currently go to [[Delegates|the referenced page]] it does not seem to have any examples of semantic annotations (but it does have a lot of <code>::</code> which makes checking for what I think I am looking for frustrating).  --[[User:Rdm|Rdm]] 21:08, 9 November 2010 (UTC)
: Hm. I'll try to explain it better here, so the page body can be updated in a more clear fashion after it's been vetted and I'm sure I really know what I'm talking about. (I'm not entirely clear on it myself, I just know a couple ways the feature can be used/abused to RC's benefit.) Semantic properties take the form of ''''<nowiki>[[property name::this page's relationship to that property.]]</nowiki>'''. I don't know what happened to the semantic notations on the delegate page. It's possible I never committed my changes to that page while I was writing this one.  For a list of current properties and their usage, check out [[Special:Properties]]. It might also help to look at [[smw:Help:Properties and types]]. --[[User:Short Circuit|Michael Mol]] 21:16, 9 November 2010 (UTC)
:: Ok, if I understand right: properties are like regular wikilinks but with an (invisible) prefix for consumption by computer programs.  They are typically used in templates.  --[[User:Rdm|Rdm]] 21:25, 9 November 2010 (UTC)
::: At a first pass, yes. Note that the prefix and suffix become related, as well as the page the link is created on.  Additionally, it's possible (and desirable in a number of places on RC) to create the prefix/suffix/page association without a visisble component. Finally, the wiki software itself consumes those associations, allowing us to use [[smw:Help:Semantic search|semantic searches]] to do interesting things. (Such as dynamically creating task page and example listings based on task properties, or (eventually) replacing MultiCategorySearch as the mechanism we use for the 'unimplemented in X' pages, and, even farther, 'unimplemented using Y' pages where Y is some concept (language, paradigm, library or other tool) as desired.) --[[User:Short Circuit|Michael Mol]] 21:35, 9 November 2010 (UTC)
:::The way I've seen properties is sort of like ternary categories. With regular categories, you're in or you're out. With properties, not only are you in or out, but you also have a value within it. So a task is in the "Implemented in language" proerty once it has examples, but it's also "implemented in" different languages. So you can go to the [[Property:Implemented in language|Implemented in language property page]] and see all the pages on the left which are "implemented in" something (just like a category). On the right you see the values given for each of those pages, which are the languages that they are implemented in.

:::Just for another way to think about it, maybe we can say that Cateories are DB tables where the keys are the page names. Then Properties would be tables where the keys are the page name and an additional value. --[[User:Mwn3d|Mwn3d]] 22:27, 9 November 2010 (UTC)
