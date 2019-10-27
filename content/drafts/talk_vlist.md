+++
title = "Talk:VList"
description = ""
date = 2011-09-13T19:50:24Z
aliases = []
[extra]
id = 9176
[taxonomies]
categories = []
tags = []
+++

==What is the task?==
What are we supposed to do for this task? (Presumably implement a VList, but that's not really enough for a complete task.) What is an adequate demonstration that an implementation of a VList datastructure is at least not grossly incorrect? –[[User:Dkf|Donal Fellows]] 09:21, 20 January 2011 (UTC)
: Marked as a draft. There's no implementation in the page ''and'' there's no task definition either. Until there's a proper task definition and evidence that it can be done in several languages (preferably of different natures), it doesn't match quality requirements for being a task. –[[User:Dkf|Donal Fellows]] 09:33, 24 January 2011 (UTC)
::It doesn't have to be a task. Maybe the intent was for it to be a page like [[Associative array]]. --[[User:Mwn3d|Mwn3d]] 20:59, 16 May 2011 (UTC)

==Draft and not draft?==
I noticed that this is appearing as both a draft and non-draft page on "tasks not implemented pages".  Odd.  --[[User:Dgamey|Dgamey]] 20:32, 16 May 2011 (UTC)
:It's in the non-draft task category because it's labelled as a data structure. {{tmpl|data structure}} puts it directly in the programming tasks category. I'm not sure that that is necessary. I think all of the data structures are already marked as tasks. --[[User:Mwn3d|Mwn3d]] 20:35, 16 May 2011 (UTC)
:I fixed that template. It's just as well. There were some encyclopedic pages that got placed in the programming tasks category. Oops. --[[User:Mwn3d|Mwn3d]] 20:55, 16 May 2011 (UTC)

==Rescuing the page==

Do you think there's any chance of rescuing that page? It doesn't have an implementation at all, just an interface definition (of the highly “so what?” flavour, too) in a single language. If we can't rescue it, killing it off would be better I suppose. –Donal Fellows 14:58, 21 July 2011 (UTC)

:It can probably be rescued. [[User:Markhobley|Markhobley]] 15:18, 21 July 2011 (UTC)

== Some problems ==

# I'm not sure why length can only be found in O(log n) time.  The data structure surely can have bookkeeping to have O(1).
# The second operation, "new array begining at second element" blah blah, is too limited.  It's equally easy to take any valid array slice.
# However, if a slice is taken, some clarification is needed as to what can be done with it.  Should the content be copied? Should the content be copied only when it's modified, or only when the slice is extended? --[[User:Ledrug|Ledrug]] 01:53, 10 September 2011 (UTC)

::In my opinion, the "Obtain a new array beginning at the second element of an old array (O(1))" is a bogus requirement.  Either:
:::(a) The array is not a new array but a reference into the old array, or
:::(b) The time to complete the operation is O(n) rather than O(1).
::And... without this issue, these performance characteristics can be achieved with regular arrays (or, for a language like C: a structure which consists of a pointer to an array, the currently used array length and the currently allocated array length -- when you need more space, use realloc to double the allocated space -- and subtract the index you are using from the length of the array to achieve the "add to front" semantics of the VList).  This flat array approach results in a simpler algorithm and significantly lower constant factors when reading the data. --[[User:Rdm|Rdm]] 15:01, 13 September 2011 (UTC)
::: Sure, but VList is meant to give persistent pointers.  Realloc may move buffer and invalidate all pointers into its elements, while VList would not.  --[[User:Ledrug|Ledrug]] 15:31, 13 September 2011 (UTC)
:::: But none of the task requirements mention pointers.  And "persistent pointers" are a language-specific construct, as are "pointers".  Also the advantages of pointers into an array over array indices are obscure, or negative, especially in the context of languages which implement some form of memory management.  --[[User:Rdm|Rdm]] 15:40, 13 September 2011 (UTC)
::::: If your language provides higher level arrays and such, then all you need is a list, of course a data structure like VList would not be something you care -- but then you don't really worry about realloc either, do you?  VList is a specifically low level construct, it ''is'' a way of doing memory management yourself.  The sublist mentioned in the task is a use of the persistent pointer property: after you take an array slice, pushing more objects into the original list would not affect the validity of the slice, even though there's no further bookkeeping than just a pointer and an offset. --[[User:Ledrug|Ledrug]] 15:53, 13 September 2011 (UTC
:::::: Ok... perhaps this "languages with high level arrays do not need this" concept should be included in the task description?  Meanwhile, to clone the array you need to copy the data from the first (biggest) block of data, which on average will contain c*n elements, where c is a constant (perhaps 0.25) and n is the length of the array.  This means that that operation needs O(n) time. --[[User:Rdm|Rdm]] 16:25, 13 September 2011 (UTC)
::::::: Hmm? If you want to make a reference to a slice of the array, just copy the pointer and set a new offset, which is O(1).  If you need to copy the content, then every block that's not empty should be copied, which involves all n elements that's currently filled.  The point of taking a slice reference is to avoid this copying.  --[[User:Ledrug|Ledrug]] 18:26, 13 September 2011 (UTC)
:::::::: Hmm.. ok...  But you need to be careful about the set of allowed operations on a slice.  For example, think about what happens to a slice that omits the first character of the original and then what happens when you add another character.  Note that if you add to the slice, the behavior of the original is different if it was length 2 vs. if it was length 6.  Something analogous can happen with cloning: with a suitably restricted set of operations (if adding is allowed but not dropping) you do not have to clone the entire set of data and can clone only the first block.  --[[User:Rdm|Rdm]] 19:50, 13 September 2011 (UTC)
