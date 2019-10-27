+++
title = "Talk:Knapsack problem/Bounded"
description = ""
date = 2017-03-19T06:15:20Z
aliases = []
[extra]
id = 9948
[taxonomies]
categories = []
tags = []
+++

==Some technical details==
Some solutions, including my perl one, use the 0-1 problem solution, but counting items with multiples as multiple different items.  This is almost a good solution, but not quite: if some items have a huge count, the correct solution will not much care, but the hacked 0-1 solution generally can hang.  Try for this example set the beer count to say 1000, and see how well your code copes. --[[User:Ledrug|Ledrug]] 09:11, 22 June 2011 (UTC)
:Hi, wp has the problem marked as taking [[wp:Pseudo-polynomial time]]. Does that mean that there is no better exact algo? --[[User:Paddy3118|Paddy3118]] 16:34, 22 June 2011 (UTC)
::Using 0-1 solution is still Pseudo-polynomial time, but not all Pseudo-poly algorithms are equal.  Suppose you have 1000 beer, each weighing 1, and your algorithm comes down to check on beers with 10 space to spare. The correct algorithm's decision tree branches 11 times, picking from 0 to 10 (weight capped), and see which gives best value.  The 0-1 algorithm under the same sitution sees 1000 different items, each may be taken or not; because of the weight limit, it ends up trying taking any 10 of them, any 9 of them, etc, that is: Binomial(1000, 10) + Binomail(1000, 9) + ... the decision tree branches wildly, even though beer is beer, most of the different choices are really the same.

::The Go solution is correct if you are interested.  I haven't checked if any other example is using the right method. --[[User:Ledrug|Ledrug]] 20:49, 22 June 2011 (UTC)
:::Thanks. --[[User:Paddy3118|Paddy3118]] 03:20, 23 June 2011 (UTC)

== J approach (dynamic variant) ==

Here's how I implemented the dynamic programming variant, in case it helps someone else:

In the 0-1 version, inclusion is binary -- an item is included or it's not.  For this version, however, an item may be included up to n times (where n varies from item to item).  To represent this, I gave items which may be included n times n item numbers.  In other words: map is item 1, compass item 2, water is items 3 and 4.  (1 water would be item 3 and 2 waters would be item 4).

When computing maximum values, I in essence performed a small inner loop where I would (in the case of water) compute the maximum value for both 1 water and 2 waters.  This was a trivial extension of the wikipedia algorithm (2 waters weighs twice what 1 water would weigh).  And, basically, once the weight of a multiple item choice exceeds the permissible weight I just repeated the biggest previous value for fewer items.

A related problem is keeping track of the "best choice so far".  There might be some clever way of extracting it from the values cache, but I used a best choice cache.  For item <math>j</math> which has maximum possible count <math>p_j</math> and current best count <math>c_j</math>, I represent the best combination so far as <math>\sum c_j + P_j</math> where <math>P_j = \prod 1+p_k</math> where <math>k<j</math>.  (This means that when I am considering various options I can ignore an option by multiplying it by 0 and then selecting the largest remaining <math>c_j</math>.)

In other words 0 means "no items", 1 means "1 map", 2 means "1 compass", 3 means "1 map and 1 compass", 4 means "1 water", 8 means "2 waters", 9 means "2 waters and 1 map", 12 means "1 sandwich", and 16 means "1 sandwich and 1 water".  And, since I am taking tiny steps at each step of computation, I cache <math>P_j</math> rather than recomputing it every time I need it.  (You might think of this as a generalization of base n arithmetic where each digit position can be in an independent base (except that these "digits" are probably best thought of as being arranged from left-to-right instead of the usual right-to-left).) --[[User:Rdm|Rdm]] 21:20-ish, 27 June 2011 (UTC)

:Some thoughts about caching the best choice: if you don't and just calculate the best value, and after which you want the list of chosen items, you'd have to solve <math>\sum v_j c_j = v_{\rm total}</math> with the constraint <math>c_j \leq n_j</math> -- that's a reduced bounded knapsack problem in itself, a somewhat amusing situation. --[[User:Ledrug|Ledrug]] 21:22, 27 June 2011 (UTC)

== OOCalc? ==

Currently the OOCalc implementation claims that it is trying to minimize the total value of items contained in the pack.  Shouldn't it be trying to maximize that instead?  --[[User:Rdm|Rdm]] 15:54, 28 June 2011 (UTC)
:Yep, the image was correct, I just wrote the wrong thing in the accompanying text. Fixed now, thanks. --[[User:Paddy3118|Paddy3118]] 18:28, 28 June 2011 (UTC)
