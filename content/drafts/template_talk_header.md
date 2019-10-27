+++
title = "Template talk:Header"
description = ""
date = 2016-03-14T07:08:37Z
aliases = []
[extra]
id = 20506
[taxonomies]
categories = []
tags = []
+++

==New version==
Although there are already several edits to this template it still doesn't work properly for the special cases C# and F#. For example the current version of the C# solution for [http://rosettacode.org/mw/index.php?title=Call_a_function&oldid=221098 Call a function] displays the language name correctly but puts the solution into [[:Category:C Sharp]] which is wrong because it should be put into [[:Category:C sharp]] (s instead of S). 

I therefore have written a new version which gets rid off the second parameter and allows users to use either '''C#''', '''C sharp''' or '''C Sharp''' as the language name (similarly for F#): [[User:AndiPersti/Sandbox]].

You can test this version on [[Special:ExpandTemplates]]. Just put <code><nowiki>{{User:AndiPersti/Sandbox|language}}</nowiki></code> into the '''Input text''' field and press OK.

Any comments? --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 15:23, 13 March 2016 (UTC)
: Seems to work for the tests I ran, though I never noticed the original problem (and that surprises me -- category hast not been case sensitive for the example I have tried, though granted, I have not tried many examples...). I did notice an extra space in your new version. I don't know if that matters --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:51, 13 March 2016 (UTC)
:: Category names (and page names in general) are only case insensitive for the first letter. All other letters in the name are case sensitive. The extra space doesn't matter AFAIK but it makes the template code a little bit easier to read IMHO. Thanks for testing. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 07:08, 14 March 2016 (UTC)
