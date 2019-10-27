+++
title = "Category talk:C"
description = ""
date = 2009-05-31T22:11:41Z
aliases = []
[extra]
id = 1896
[taxonomies]
categories = []
tags = []
+++

"C++" and "C plus plus" should be merged. [[User:Sgeier|Sgeier]] 18:51, 1 February 2007 (EST)
:Yes, they should, but I can't get to the C++ articles right now--I don't even know how someone managed to create them!  This version of MediaWiki (1.8.2) doesn't support the creation of pages with + in their name.  I'm going to upgrade this weekend to 1.9.1, which does support it.  Merging C++ and C plus plus will be the ''first'' thing I do.

:It's been bugging me since I started the projecct, but when I found out how to fix the + problem, traffic was still too high for me to take the site down long enough to do the upgrade.  --[[User:Short Circuit|Short Circuit]] 19:10, 1 February 2007 (EST)

== C++ problem ==

I can't get to the [[C++]] page. You can try it on that link there. It just takes you to C. RCBF(C++) has a problem too: http://rosettacode.org/wiki/RCBF_(C++) --[[User:Mwn3d|Mwn3d]] 10:25, 7 February 2008 (MST)
:Ugh. This was fixed with a Mediawiki upgrade in November. I don't know what broke it again. --[[User:IanOsgood|IanOsgood]] 10:46, 7 February 2008 (MST)
::It may have been broken again in the maintenance on Sunday. --[[User:Mwn3d|Mwn3d]] 10:49, 7 February 2008 (MST)

== Language Features ==

I was taught that C ''always'' passes parameters by value, and that it simulates pass-by-reference by passing (by value) the address of the entity being referred to. I suppose it is the difference between passing a thing and passing the name of the thing… —[[User:Dkf|Dkf]] 10:07, 31 May 2009 (UTC)
: The only way to do a reference to a "thing" is a pointer indeed. In C this is explicit. In other languages it is not... But pointers are the way C calls "by reference", so function(&c) is a passing of c by reference... then the reference can be dereferenced... not simulation! --[[User:ShinTakezou|ShinTakezou]] 13:46, 31 May 2009 (UTC)
:: That's not call by reference. That's passing the address of the thing by value... ;-) —[[User:Dkf|Dkf]] 13:55, 31 May 2009 (UTC)
::: And what do you think that it works in other languages the "pass by reference"? In C it is explicit the fact that there are pointers at work. In other languages it is not explicit, it's hidden in layers of abstraction... but it is what it happens at the end of the games. No way to get out.
:::
```c
int x;
int *rx;
x = 5;
rx = &x;
```

::: Now, rx can be used as a reference to x. Similarly, a lang like Perl has the "\" "operator", and the "semantic" (maybe the wrong word, I don't know) is the same of the pointers...
:::
```perl
my @arr = ( 1, 2, 3 );
my $ref = \@arr;
a_function($ref); # the same as C, $ref is a reference, ... but it is passed "by value", i.e.
                  # there's no difference with the following
my $scalar = 12;
a_function($scalar);
```

:::: Of course they're all done as about the same thing when the bytes hit the processor. That's not the point. The point is that it is about the model(s) provided by the language itself; C (unlike C++) is strictly value-passing. (BTW, the Perl example is invalid here, since the parameter-passing model is more about how formal parameters are declared rather than how you write the calls; Perl supports multiple parameter modes.)
:::: More to the point is that you can't tell in C whether a formal argument that is of type <code>char *</code> is taking a string or a simulated "reference" to a char variable. This has actually been the source of bugs I've come across in peoples' code when doing maintenance coding, e.g., where someone had misused <code>scanf("%1s", &charVar)</code> and gotten away with it on big-endian platforms… —[[User:Dkf|Dkf]] 22:11, 31 May 2009 (UTC)
