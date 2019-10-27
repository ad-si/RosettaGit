+++
title = "Talk:Count occurrences of a substring"
description = ""
date = 2019-10-15T21:12:44Z
aliases = []
[extra]
id = 9935
[taxonomies]
categories = []
tags = []
+++

== More specific? ==
What happens when there are more than one way to match? Say you have string "aBaBaBa" and want to find <i>non-overlapping</i> "aBa"s,  you could say both (aBa)B(aBa) and aB(aBa)Ba, where brackets denote the found matches, are valid. --[[User:Ledrug|Ledrug]] 17:07, 16 June 2011 (UTC)
:My guess would be to specify to match left-to-right, but that might be too restrictive of possible methods. Maybe say "give the highest possible number of non-overlapping matches"? Can you think of any situations where that isn't the same as the count when matching from left-to-right (or even right-to-left)? --[[User:Mwn3d|Mwn3d]] 17:20, 16 June 2011 (UTC)
::Earliest match always produces most matches.  Proof: suppose a string is matched in two ways:
::<lang>...(m1)...(more matches)...
....(m2)...(more)...
```

:: where m1 and m2 overlap, and suppose both are giving highest possible number of matches at their starting location; further suppose second way matches more times: but this can't be, because we can then give up m2 and use m1 while keeping the rest, and it would have more matches than first way, which is a contradiction.  Hence, highest num of matches can be obtained by starting earliest.

:: Since reversing both pattern and string makes left-to-right problem into a right-to-left one, this also proves highest matches can be obtained from either end.

:: That exercise aside, I just thought the task should be worded so that any given input would produce an unambiguous result. --[[User:Ledrug|Ledrug]] 17:45, 16 June 2011 (UTC)
:::For completeness' sake (and because it wouldn't affect the existing examples), I added that clarification and referenced this proof. --[[User:Mwn3d|Mwn3d]] 17:52, 16 June 2011 (UTC)

Yes, the problem is stated ambiguously and was probably inspired by some particular language's implementation of a built in function.  It would be more appropriate to count all occurrences of a substring.  Counting the maximum number of non-overlapping substrings can turn into a very complicated problem requiring backtracking.

: Yes, some languages' BIF may treat it as caseless, still others may treat all whitespace equally (as blank or blanks).  It would've been interesting to make a case-sensative and a caseless version. The whitespace issue can be more complicated. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:33, 12 June 2012 (UTC)
