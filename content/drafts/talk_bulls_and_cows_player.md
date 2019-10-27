+++
title = "Talk:Bulls and cows/Player"
description = ""
date = 2010-06-17T14:04:03Z
aliases = []
[extra]
id = 7509
[taxonomies]
categories = []
tags = []
+++

== Python ramblings ==
When I wrote the original scorer task [[Bulls and cows]] I vaguely remembered that I had written a player before, in a dialect of basic, probably in the late, late eighties. Well, I was ignoring what I was supposed to be doing this weekend and just started doodling a solution and it seemed to gel.

With possible answers being restricted to a selection of four digits from the nine,
# I generate the 3024 possible initial choices (and randomly shuffle them). 
# Select the first choice as my next answer,
# Then get its score. 
# Next, whittle down the possible choices by removing from the choices any choice where ''if'' it where the true answer, it ''would not'' give the score I had just received.
# Repeat from 2.
If I get all bulls then I win. If there are no more choices then the scores must be inconsistent.
 --[[User:Paddy3118|Paddy3118]] 02:46, 14 June 2010 (UTC)

: My conversion of that code (for the Tcl version) does one key thing differently; instead of shuffling the values it picks a random one. Simpler to implement. –[[User:Dkf|Donal Fellows]] 08:43, 14 June 2010 (UTC)
