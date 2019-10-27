+++
title = "Talk:Currency"
description = ""
date = 2014-01-05T06:50:48Z
aliases = []
[extra]
id = 17039
[taxonomies]
categories = []
tags = []
+++

==Motivation==
The topic came up on the [https://groups.google.com/forum/?hl=en#!topic/golang-nuts/FAKE3fBY6vc| Go mailing list].  I know it's a common topic and was surprised to see it missing from RC.  My task example comes almost directly from the original post.  I clarified what I thought were points of valid concern, exact representation and rounding, and bumped one number a little so that truncating the tax result would give the wrong answer and rounding was required.  I expect the task to be especially simple for languages with a native or library decimal type.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 23:45, 2 January 2014 (UTC)

==Problem with tasks examples?==
Hi, I just saw the J example use floating point and get the expected result?! (Before 'corrected' to not use floating point). Shouldn't the task be amended so that normal double precision floating point calculations would give the wrong result forcing most languages to handle the calculation using something better than this?

Maybe calculations on a string of values that include millions (or billions), of pounds together with single pence/cents calculations contrived to give wrong results when working in double-precision binary floating point. 

Could we possibly '''delay taking this task out of draft status''' for a while whilst this is discussed here? Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:22, 3 January 2014 (UTC)
:Possibly we could introduce a need for an exact comparison that would fail under floating-point?  I agree about keeping it draft for now. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 16:07, 3 January 2014 (UTC)
::Good points.  I had thought of this but didn't have a good idea for how to do it.  I really didn't want to make arbitrary precision a requirement as I think that's overkill in almost all cases.  I also didn't want to explicitly ban floating point, but how's this for a compromise?.  It's kind of contrived to land numbers in the range where 64 bit floats don't quite work but 64 bit ints still do.  I added 15 zeros to the 4 (and changed the shake price so improper rounding would still show up.)  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 21:25, 4 January 2014 (UTC)
::: The numbers are (necessarily) more foolish now, but the test is a good one anyway. In particular, there's now plenty of places to go wrong if you're not careful… –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 22:48, 4 January 2014 (UTC)

:: I fail to see what this semtence means: 7.65%. (That's 4 with 15 zeros after it. The number is contrived to exclude naïve task solutions using 64 bit floating point types.) " Where is the 4??? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 00:57, 5 January 2014 (UTC)
:::Not the best grammar perhaps, but the sentence begins "Use the values 4000000000000000...."  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 04:34, 5 January 2014 (UTC)
::::Ah, thanks. It refers to the number of hamburgers. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 06:50, 5 January 2014 (UTC)
