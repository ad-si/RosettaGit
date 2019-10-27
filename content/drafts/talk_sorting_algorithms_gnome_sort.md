+++
title = "Talk:Sorting algorithms/Gnome sort"
description = ""
date = 2010-09-12T14:34:02Z
aliases = []
[extra]
id = 4090
[taxonomies]
categories = []
tags = []
+++

== C problem? ==
The second <code>j := j + 1</code> from the example seems to be missing? --[[User:Paddy3118|Paddy3118]] 17:04, 27 April 2009 (UTC)
: The code works for sure (I've used it in Huffman codes... anyway I've written also a test code and did some testing and it seems to work...); I can't remember, maybe I've "reinterpreted" it or optimized it... I will study if it is still Gnome, or if I've changed it so that it can be called Gnome sort no more. --[[User:ShinTakezou|ShinTakezou]] 18:28, 27 April 2009 (UTC)
: How I can understand it now: the previous C version was simply the ''original'' Gnome sort as explained in the Dick Grune citation on Wikipedia (the garden gnome moves always one step forward or backward); the pseudocode is an optimized version, since we don't need to pass through every pot to reach the same position we left when started to go backward; so we can restart from j instead of 1. --[[User:ShinTakezou|ShinTakezou]] 20:37, 27 April 2009 (UTC)
:: Third thought (too many distraction sorry): it's true it's an optimization, but not the rest. When the gnome hits the beginning of the list (i==0), elements 0 and 1 are in the proper order, so that if we put i=1, the next iteration will execute the "true part" of the if, i.e. i=j and j++; ... doing it directly in the "then part" of the if, will avoid an extra comparing we know will be true. This optimization apart, it was really the same. --[[User:ShinTakezou|ShinTakezou]] 20:54, 27 April 2009 (UTC)
== Wikipedia markup reference problem ==
The Wikipedia markup produces a link to new page when it should link to [[wp:Gnome_sort]].  Not sure how to fix the markup. --[[User:Dgamey|Dgamey]] 14:34, 12 September 2010 (UTC)
