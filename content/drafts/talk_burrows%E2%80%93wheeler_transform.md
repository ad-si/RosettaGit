+++
title = "Talk:Burrows–Wheeler transform"
description = ""
date = 2019-03-25T13:24:22Z
aliases = []
[extra]
id = 22243
[taxonomies]
categories = []
tags = []
+++

===Haskell foldr -> iterate edit worthwhile ?===

I have often learned from [[User:Spoon!|Spoon!]]'s useful Haskell edits, not least to always consider using '''iterate'''.

On this occasion, I suspect that I may be missing something ?

The 2019 March 25 (''Globules -> Spoon!'') edit rewrites '''foldr f''' to '''iterate g !! length''' (where the '''f''' to '''g''' rewrite is a type-change – the pruning out of an accumulator argument).

Perhaps under the influence of Graham Hutton's tutorial on the ''universality and expressiveness of fold'' I tend to think of '''foldr''' as something fairly clean and fundamental, and this particular use of '''iterate''' appears, at first glance, to risk entailing some cost, with '''(!!)''' and '''length''', which might not be offset by dispensing with an accumulator argument, but perhaps I am missing a point about fusion and laziness here ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:11, 25 March 2019 (UTC)
