+++
title = "Talk:Sorting Algorithms/Circle Sort"
description = ""
date = 2016-01-14T02:50:33Z
aliases = []
[extra]
id = 18476
[taxonomies]
categories = []
tags = []
+++

== Complexity claim is wrong ==

The complexity is said to be O(n log(n)) on the linked sourceforge page, which does not make sense.  Each call to the <code>CircleSort</code> is O(n log(n)), but one call does not sort the array.  It seems the sort routine needs to be called O(log(n)) times on average for a large random array.  Maybe the number of swaps drops with each successive call due to the array becoming more ordered, but the number of comparisons is still the same, so the overall complexity is more like O(n log(n) log(n)).  The runtime difference between this sorting method and quicksort becomes more significant as the array size increases. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 11:28, 7 January 2015 (UTC)

=== Isn't this a "Sorting Algorithm"? ===

Should this task not be called "Sorting Algorithms/Circle Sort" - even as a draft task? --[[User:Tim-brown|Tim-brown]] ([[User talk:Tim-brown|talk]]) 10:01, 14 January 2015 (UTC)


###  Wrong category 


The category should be "Sorting_algorithms" (with lowercase "a").
