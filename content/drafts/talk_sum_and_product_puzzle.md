+++
title = "Talk:Sum and Product Puzzle"
description = ""
date = 2017-07-01T05:14:18Z
aliases = []
[extra]
id = 21043
[taxonomies]
categories = []
tags = []
+++

==Remove draft status?==

Now that I improved the task description, is this task ready for prime time? --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 14:47, 5 August 2016 (UTC)
: I went ahead and took it out of draft status. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 12:35, 20 August 2016 (UTC)

== Scala  ==

Could someone in the know please explain these two lines in plain English?

val step2 = step0 filter { sumEq(_) forall { prodEq(_).size != 1 }}

   step2 contains the pairs whose product is unique and ??
  
val step3 = step2 filter { prodEq(_).intersect(step2).size == 1 }

: step2 filters the step0 integer pairs for pairs where "For every possible sum decomposition of the number X+Y, the product has in turn more than one product decomposition" 
: step3 filters the set defined by step2 for pairs where "The number X*Y has only one product decomposition for which fact 1 is true"
: Perhaps the Haskell or JavaScript versions might seem more legible ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:35, 21 October 2016 (UTC)
:: Still not explicit enough :-( Sorry  Meanwhile I added 2 translations where I could understand the source (AWK and GO/ --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 18:54, 26 October 2016 (UTC)
::: Do higher order functions feature in the architecture or traditions of REXX ? If not, the patterns of functional composition used in the Haskell and Scala etc examples may be a little hard to translate all that directly. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:18, 3 November 2016 (UTC)

== A question on GO ==

I translated GO to Rexx and the "final" piece missing for understanding is this:

Why does this justify the removal of the pair p??

Shouldn't the pair a/b be discarded???


```txt

for a := 2; a < s/2+s&1; a++ {
   b := s - a
   if products[a*b] == 1 {
   // Excluded because P would have a unique product
   continue pairs
```
 
--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:59, 05 November 2016 (UTC)
: I think I found the answer myself:-) If ANY of the decompositions of the given pair's sum had a unique product I couldn't be sure that P does NOT know. So all pairs resulting in the given sum could be eliminated not just the one at hand. (They will be later on or have already been...) --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:59, 10 November 2016 (UTC)

== Java program inconsistent with others ==
It allows the sum to be equal to maximum value. The threshold for a second solution should be X+Y<1866, not 1865.
