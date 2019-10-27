+++
title = "Talk:Knapsack problem/0-1"
description = ""
date = 2016-02-22T17:25:41Z
aliases = []
[extra]
id = 6041
[taxonomies]
categories = []
tags = []
+++

== dkg? ==

“Decikilogram”? Yowch! Round here we use SI prefixes and call that a “hectogram”. (Well, we don't use it very often at all, but that's another matter.) –[[User:Dkf|Donal Fellows]] 13:10, 14 February 2010 (UTC)
:Maybe it is a hungarian version of "dag". See the english [http://en.wikipedia.org/wiki/Kg#SI_multiples wikipedia] and the [http://hu.wikipedia.org/wiki/Kilogramm#R.C3.A9szei_.C3.A9s_t.C3.B6bbsz.C3.B6r.C3.B6sei hungarian] also. So I did not think on hectogram. In my program you can see this line with comment: "ZeroOneKnapsack zok = new ZeroOneKnapsack(400); // 400 dkg = 4kg" From this comment you could calculate what I did think. --[[User:Pelci]] 21:01, 14 February 2010 (Budapest)

==Change to task description==
I was considering making the following change to the task description as I had seen one slight modification that i wanted to make, but then got carried away:
:''"A tourist wants to make a good trip at the weekend with his friends. They will go to the mountains to see the wonders of nature, so he needs to pack well for the trip. He has a good knapsack for carrying the things, but he knows that he can carry a maximum of only 4kg in it and it will have to last the whole day. He creates a list of what he wants to bring for the trip but the total weight of all items is too much. He then decides to add columns to his initial list detailing their weights and a numerical "value" representing how important the item is for the tour."'' 
The changes are a little more than I had started to write, and would rather the original author take what he likes, if anything, for the task description.  --[[User:Paddy3118|Paddy3118]] 17:01, 14 February 2010 (UTC)
:I can not speak very well the english language. So I could not feel the different between your suggestion and my original text. So if you have the English as mother language, I think, you can decide what is the better text. Your suggestion is for me also good. No problem. --[[User:Pelci]] 20:35, 14 February 2010 (Budapest)

==Delete in favor of Knapsack problem/Bounded==

This task is a subset of [[Knapsack problem/Bounded]]. That is, any program that's a solution to Knapsack problem/Bounded is also a solution to Knapsack problem/0-1. So I recommend we delete this task as entirely redundant. If desired, we can add the sample problem given here to Knapsack problem/Bounded. Thoughts? —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 12:10, 16 February 2010 (UTC)
:Algorithms for the 0-1 problem might not apply to the bounded problem (Such as the excellent [[Knapsack problem/0-1#Dynamic programming solution]] that I am trying to figure out at the moment). --[[User:Paddy3118|Paddy3118]] 14:33, 16 February 2010 (UTC)
:Generaly you are perfectly right. My Java solution for bounded problem contains also the 0-1 problem. I traced back the bounded solution to the 0-1 solution. I separeted the items as they would be independent units. E.g. if we have two books I dissolved them two independent books, and I thought on them as two different items. After this dissolving I solved the problem as a 0-1 problem. So from this aspect I think it is very important to know how you can solve the 0-1 problem. So I do not accomodate with you that we must delete the 0-1 problem. --[[User:Pelci|Pelci]] 17:17, 16 February 2010 (UTC)

== Problem setup doesn't highlight the use of the exponential algorithm ==

with the way the problem is set up, if you choose the highest valued item less than what the knapsack can carry you get the correct answer. normally, if you do this you will miss the right answer, but here it works. I think a small change to the list of items will make it work better for portraying the idea behind the problem.
