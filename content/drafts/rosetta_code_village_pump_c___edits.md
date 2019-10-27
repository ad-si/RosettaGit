+++
title = "Rosetta Code:Village Pump/C++ edits"
description = ""
date = 2010-11-16T11:23:54Z
aliases = []
[extra]
id = 4670
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=C++ edits
|summary=Relating to some edits of [[C++]] examples.
}}
Would someone who knows C++ better than I do please review the statements made today (2009-08-07) by [[Special:Contributions/86.199.73.222|86.199.73.222]], particularly [http://rosettacode.org/mw/index.php?title=Creating_an_Array&diff=57404&oldid=prev Creating an Array] and [http://rosettacode.org/mw/index.php?title=User_Input_-_text&diff=57403&oldid=prev User Input - text]? (Those I couldn't figure out what to do with. I've made some followup changes to the other pages; please review those as well.) --[[User:Kevin Reid|Kevin Reid]] 12:08, 7 August 2009 (UTC)

: I'd be tempted to throw the non-code parts of those contributions out. Instead of picking holes in what was previously given, they should be contributing the “right” way. (No, I don't follow the ins and outs of the C++ taliban…) —[[User:Dkf|Donal Fellows]] 13:08, 7 August 2009 (UTC)

:: I know C++ pretty well, and I don't think his statements are right. I'll correct it as I see fit. --[[Special:Contributions/76.91.63.71|76.91.63.71]] 19:32, 7 August 2009 (UTC)
: If the '''new[]''' operator was deprecated, I haven't heard about it.  For arrays, the generally accepted way to go about it is to use std::vector.  boost::array is another option, if you're willing to bring in a third-party library (And that's not at all inappropriate, as far as Rosetta Code is concerned).  There are and always will be scenarios in C++ where '''new[]''' is more appropriate.  I can think of three different, yet equally valid, approaches for the '''cin''' issue, though.  Loop through the cin>>string input sequence until a newline is hit, for example.  The breakage that was described comes from non-integer values being left in the input buffer after a cin>>int, and there's a fix for that. (I don't remember what it is, though.)  Buffer flush fix taken into account, the readline approach and cin>>string+convert to int approaches are just as equally valid, from a correctness standpoint. --[[User:Short Circuit|Short Circuit]] 20:09, 7 August 2009 (UTC)

::: Of course there are no clues about a deprecation of the new operator to (m)allocate, around here and there, or I am not able to seek at all. --[[User:ShinTakezou|ShinTakezou]] 22:00, 7 August 2009 (UTC)
:::: ''''new'''' is preferred over malloc in all cases I'm familiar with.  When used on a type with a constructor, it runs the constructor.  When used on a type where the '''new''' operator is overloaded, it runs the overload.  malloc() isn't so easy to hook into. --[[User:Short Circuit|Short Circuit]] 05:19, 8 August 2009 (UTC)
::::: In fact, <code>malloc()</code> is almost certainly the library call that sits ''underneath'' all forms of <code>new</code>, though the latter is preferred because it is type aware (and <code>std::vector</code> is often a better choice in practice AIUI). Still, all these things have their place. –[[User:Dkf|Donal Fellows]] 11:23, 16 November 2010 (UTC)
