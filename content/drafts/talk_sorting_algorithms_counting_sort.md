+++
title = "Talk:Sorting algorithms/Counting sort"
description = ""
date = 2018-09-20T21:41:56Z
aliases = []
[extra]
id = 4883
[taxonomies]
categories = []
tags = []
+++

== number of elements? ==

There's a slight ambiguity in the task description (I think). The last sentence reads: "<tt>sparse arrays may limit the impact of the memory usage</tt>" which appears to allow for the use of sparse arrays, i.e. arrays that have <i>indices</i> in the range [min..max] but do NOT actually have (max-min+1) elements. The pseudocode, however, expressly requires "<tt>count: array of (max - min + 1) elements</tt>" which appears to contradict the prose.

:::::::::: Note that the last paragraph that you are referencing has been substantially re-worded (by me).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:40, 20 September 2018 (UTC)

I'm asking because the TCL implementation strikes me as a shade kludgy - I would write the whole thing using associative arrays, which have only the elements that are actually set. I.e. more like a "sparse array". They also have things like [incr arr(i)] so that the 'helper function' wouldn't be needed at all. They do, however require a bunch more time in element access (since there's a hash function under the hood somewhere) and in a sense dilute the purity of the implemented algorithm. On the other hand, they wouldn't obscure the algorithm all that much, as the user can <i>think of</i> the array as having all the element arr[min] to arr[max] even though most of them don't actually exist (i.e. no memory is allocated for them. They come into existence when they're incremented.)

Just putting this out for discussion ... [[User:Sgeier|Sgeier]] 19:19, 8 October 2009 (UTC)

: Don't moan about it. Write an alternative implementation (and put it in its own subsection of the “Tcl” entry). —[[User:Dkf|Donal Fellows]] 21:00, 8 October 2009 (UTC)
