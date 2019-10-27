+++
title = "Talk:Tokenize a string with escaping"
description = ""
date = 2017-01-12T20:12:44Z
aliases = []
[extra]
id = 21270
[taxonomies]
categories = []
tags = []
+++

== The alternative Haskell solution needs attention ==

Unfortunately, the alternative Haskell solution, given by @Hout doesn't show the strength of Haskell. The alternative solution is ''very'' inefficient in case of big lists, due to a pattern <code>list ++ [element]</code>. It ruins the effect of <code>foldl</code> or <code>foldl'</code>.

It is possible to make this solution better by using ''difference lists'' with O(1) <code>snoc</code> operator, but then it would require <code>toList . map toList</code> instead of <code>reverse . map reverse</code> as postprocessing. Another workaround would be to replace <code>++</code> by <code>:</code> and perform reversion, but in this case the alternative solution will become just a paraphrase of a DFT solution with less modular logic.

After all, it is a sort of parsing problem, so the most natural solution would use some parsing technique (DFT, for example). I would be happy to see an alternative solution using any of parsing libraries, or conduits/pipes so that it could handle real-world text-based cases and demonstrate everyday practice.

I recommend to remove the solution.

: If you are not replacing the solution, please do not remove it. It's ok to document issues, but if it's really that much of a problem to come up with a better solution, an existing solution which meets the task requirements should almost always be considered adequate. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:40, 12 January 2017 (UTC)

: I have itemized solutions and added conduit-based solution. However, I don't see the reason to leave the inefficient and non-idiomatic variant. --[[User:Samsergey|Samsergey]] ([[User talk:Samsergey|talk]]) 03:39, 12 January 2017 (UTC)

: Thanks, your point about efficiency at scale is interesting and well taken. This task doesn't specify performance at scale, and I think it can be useful to also show the 'simplest' (or most naive :-) single fold solution, but I appreciate your having documented the issue. Very happy to remove for the moment and experiment. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 09:47, 12 January 2017 (UTC)
:: Have now experimented, and agree with [[User:Samsergey|Samsergey]] :-)  Let's remove it – once you make it more efficient at scale, it converges on the first solution, and needs the double reverse. Useful exercise for me – many thanks for your response ! [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 11:29, 12 January 2017 (UTC)
::: On second thoughts, I notice that it appears, (tested with a 180k string), to be fractionally/trivially faster than the original DFA draft, so perhaps worth keeping, at least for the moment, for comparison/discussion [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:22, 12 January 2017 (UTC)

:::: Let a hundred flowers blossom :) We like Haskell for expressiveness, flexibility and freedom to be either experimentators or nudniks which it offers.
::::: Thank you for your helpful thoughts, invariably excellent code, and even forbearance with nudniks :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:03, 12 January 2017 (UTC)
::::: (Of course, thinking back to an earlier life only 1hr behind Kamchatka, I do remember that Mao's patience with the blooming of 100 flowers was not to prove inexhaustible... :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:12, 12 January 2017 (UTC)
