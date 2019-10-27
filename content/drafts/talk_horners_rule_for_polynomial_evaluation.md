+++
title = "Talk:Horner's rule for polynomial evaluation"
description = ""
date = 2010-08-12T15:06:00Z
aliases = []
[extra]
id = 6681
[taxonomies]
categories = []
tags = []
+++

==Task format?==
I don't particularly like the layout of the equations in the task description just yet. Although i have tried to cut down on the amount of equations, I needed some, but they don't seem to flow with the text around them very well. --[[User:Paddy3118|Paddy3118]] 06:28, 31 March 2010 (UTC)
: I had a go at tidying things up. The trick is to use indentation and avoid <nowiki>
</nowiki>. I also made the pseudocode look more pseudocode-y; it might be better to rewrite it to use a '''for…downto''' loop though. –[[User:Dkf|Donal Fellows]] 09:13, 31 March 2010 (UTC)
:: Thanks Donal. --[[User:Paddy3118|Paddy3118]] 14:29, 31 March 2010 (UTC)

==J language and p.==
Just wondered, does p. use Horner's rule? --[[User:Paddy3118|Paddy3118]] 03:11, 1 April 2010 (UTC)

: Actually, #. (which represents the core of Horner's approach and is incredibly useful) existed in the language spec long before p. and p. was added for convenience and to support a few other related polynomial mechanisms.  (So, it is almost certain that p. uses Horner's rule.)  --[[User:Rdm|Rdm]] 15:06, 12 August 2010 (UTC)

== C++ solution: Error ==

I think there's a bug in the C++ solution (the original one, not the more idiomatic one I just added): It seems that it decrements an iterator to the first element of the vector, which is undefined behaviour (and may trigger a run time error on certain implementations). --[[User:Ce|Ce]] 12:42, 12 August 2010 (UTC)
