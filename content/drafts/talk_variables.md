+++
title = "Talk:Variables"
description = ""
date = 2009-10-22T13:56:12Z
aliases = []
[extra]
id = 4497
[taxonomies]
categories = []
tags = []
+++

== Similarity to [[Scope modifiers]] ==

Is it just me or is this task really similar to [[Scope modifiers]]?  Maybe they needs to be merged? I may be crazy. --[[User:Mwn3d|Mwn3d]] 20:55, 16 July 2009 (UTC)
:I see what you mean. --[[User:Paddy3118|Paddy3118]] 02:49, 17 July 2009 (UTC)

So what's the plan? --[[User:Mwn3d|Mwn3d]] 01:24, 27 July 2009 (UTC)

== Constants ==

I'm wondering if it would be worth it to either add constants to this task, or make another, related task for declaring constants. Something like:
 Show how to declare a constant (i.e. a variable that can't be changed after being declared).
(I'm thinking of constants as opposed to literals, like <code>const int x = 1</code> or even <code>#define x 1</code> rather than just <code>1</code>.) Y'know, the things that you use instead of magic numbers. -- [[User:Eriksiers|Eriksiers]] 17:41, 20 October 2009 (UTC)

(This, of course, assumes that there isn't already a constants page on here somewhere.) -- [[User:Eriksiers|Eriksiers]] 18:34, 20 October 2009 (UTC)
:If the task is here, it's not where I'd put it ([[:Category:Basic language learning]]). I think it might be good to add it. Though if this task stays intact (see dispute above) it might be better to add it here. --[[User:Mwn3d|Mwn3d]] 18:38, 20 October 2009 (UTC)
::The above dispute is why I asked first, rather than just doing it. -- [[User:Eriksiers|Eriksiers]] 18:42, 20 October 2009 (UTC)

:"The things that you use instead of magic numbers" might be a naming convention on otherwise standard variables? --[[User:Paddy3118|Paddy3118]] 18:49, 20 October 2009 (UTC)
::Perhaps I phrased it poorly here. I'm specifically referring to "variables" that aren't variable, i.e. things declared <code>const</code> in C or BASIC, like in my example above. Not just a naming convention, but part of the language. -- [[User:Eriksiers|Eriksiers]] 01:04, 21 October 2009 (UTC)

There are two things in it:

* a value, denoted in some way, e.g. by a literal, by an identifier (named constant), by a static expression (evaluated at compile time);
* an object, maybe of an immutable subtype.

The term "constant" may refer to either. In [[C]] ''const'' is a type modifier that produces an immutable subtype of the type it precedes. When used in declarations it does an immutable object, e.g. the second thing. But the compiler has right to convert it into the first thing, e.g. not to allocate any objects, but use the corresponding value instead, of course when the value is static. This conversion is semantically valid, when object construction and finalization have no semantically "visible" side effects (memory allocation, CPU cycles are semantically "invisible"). The bottom line is that IMO ''const'' belongs to the tasks dealing with types rather than to variables having those type. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 13:56, 22 October 2009 (UTC)
