+++
title = "Talk:Operator precedence"
description = ""
date = 2012-09-27T07:55:57Z
aliases = []
[extra]
id = 12342
[taxonomies]
categories = []
tags = []
+++

== maybe a little more specific? ==

What does "associate from left to right" mean?

When I look at

: ((a + b) + c)

or

: (a + (b + c))

I could implement either of them by associating from left to right.  In the first case, I would be doing bottom-up association, in the second case I would be doing top-down association. --[[User:Rdm|Rdm]] 18:20, 24 September 2012 (UTC)
: From [http://en.wikipedia.org/wiki/Operator_associativity here], given the expression without parenthesis of a + b + c then if '+' is left associative it should be evaluated as (a + b) + c, which I think is what was meant by "associate from left to right". You can add brackets to make the evaluation order explicit, the associativity is used when you don't. --[[User:Paddy3118|Paddy3118]] 20:12, 24 September 2012 (UTC)

:: In the specific case of <tt>+</tt> as applied to integers (either of unlimited precision or in the finite 2s-complement groups as usually implemented by modern hardware) the associativity doesn't matter too much as the operation is also properly commutative. Where it ''does'' matter is with operators like division, exponentiation or list-prepending. (It also matters with addition of floating point numbers, but for more subtle reasons.) –[[User:Dkf|Donal Fellows]] 07:55, 27 September 2012 (UTC)
