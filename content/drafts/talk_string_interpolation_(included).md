+++
title = "Talk:String interpolation (included)"
description = ""
date = 2013-07-03T14:48:37Z
aliases = []
[extra]
id = 7459
[taxonomies]
categories = []
tags = []
+++

== Task clarification ==

I would think the intent would be to show that strings can be manipulated within the language.  At least one example shows what appears to be print formatting only.  I would think that that wouldn't meet the intent.  So a printf would not pass but an sprintf would.

:Yes, but, has it gone too far to correct? --[[User:Paddy3118|Paddy3118]] 07:03, 1 June 2010 (UTC)
:: It isn't going to get any better.  Another (less widespread) problem is documentation. Fixing it, will just have to look ugly for a while with all the attention tags. The problem is the Wikipedia article shows printf for variable interpolation which is referenced. Choices:
::# Let it ride (and maybe make it clear it's okay)
::# Clarify and make keeping the result in a string a requirement.  Then flag.
::# Clarify and make it optional to show that the interpolated string can be retained in the program for further manipulation.  Then (optional or different) flag? Or not?
:: Thoughts?
--[[User:Dgamey|Dgamey]] 11:26, 1 June 2010 (UTC)
::: I'm in favor of option 3, combined with consideration of another; correct the code example on Wikipedia's talk page in a way that won't raise the ire of the page's protector. --[[User:Short Circuit|Michael Mol]] 15:48, 1 June 2010 (UTC)
:::: I like that approach. -[[User:Dgamey|Dgamey]]

== Return to draft ==

:There seems to be another problem with this task, as I see it:

In traditional interpolation characters and variables are represented by a notation within the string, but I notice that some solutions are calling a substitution routine from within the language. This is really "substitution", rather than "interpolation". Iterpolation is implicit, whereas substitution is explicit (blimey that probably sounds more confusing - huh). Maybe the following examples clarify.

* "Mary had a $type lamb." # This is interpolation (not substitution)
* replace "type" with "little" in the string "Mary had a type lamb" # This is substitution (not interpolation)

Also what does the (included) bit mean in the task title? 

I suggest we knock this task back to draft and sort out the description.

[[User:Markhobley|Markhobley]] 15:35, 22 February 2013 (UTC)

:It seems to me that ''most'' solutions perform substitution instead of interpolation. Maybe this is because the task description is quite unclear. Also, the term "interpolation" is quite misleading. I did not understand the task until I read the Wikipedia article. --[[User:PauliKL|PauliKL]] ([[User talk:PauliKL|talk]]) 14:48, 3 July 2013 (UTC)


###  Wikipedia 

There is already some discussion on WP 
* [http://en.wikipedia.org/wiki/String_literal#Variable_interpolation]  reference page is not the main article
* [http://en.wikipedia.org/wiki/Variable_interpolation]  main article
The talk pages already cover some of this.
* [http://en.wikipedia.org/wiki/Talk:String_literal#Variable_interpolation] 
* [http://en.wikipedia.org/wiki/Talk:Variable#interpolation:_rv_example] 
* [http://en.wikipedia.org/wiki/Talk:Variable_%28programming%29]  (the first jump with little on it of relevance) 

As far back as 2006, the fact the printf wasn't needed.  Yet the examples seem a bit of a mix mash. -[[User:Dgamey|Dgamey]]
