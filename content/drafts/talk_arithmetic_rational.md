+++
title = "Talk:Arithmetic/Rational"
description = ""
date = 2013-01-08T08:10:00Z
aliases = []
[extra]
id = 3371
[taxonomies]
categories = []
tags = []
+++

== Too long ==
Gosh, this task seems far too long. I would suggest the task change to be something like the definition of a rational together with the demonstration of its use in computing the addition and subtraction of two rational numbers.

Leave out the class based, and operator overloading to include more languages; and cut down on the definition of operators.

Another, separate task could focus on operator overloading.  --[[User:Paddy3118|Paddy3118]] 12:58, 13 February 2009 (UTC)
:Agreed. This task should simply be about the definition of a rational type and maybe a couple of helper functions (like reduction). There should be no operator definition in this task. That is covered [[Matrix_exponentiation_operator|elsewhere]]. If you still want the operator functionality, then named functions would be preferred so the task isn't restricted to languages with operator definition. --[[User:Mwn3d|Mwn3d]] 13:47, 13 February 2009 (UTC)
::Actually, if you want the operator functionality, you should simply say "provide a means to do ''blah''," so that languages can use their idiomatic approach, be it operator overloading, function calls, etc. --[[User:Short Circuit|Short Circuit]] 16:06, 13 February 2009 (UTC)

I just changed the task definition, and pruned the code and examples 
back to the minimum required.  I suspect that the original code in
Python would be really short as python can leverage off duck typing.
However ALGOL 68 requires the programmer to MANUALLY define ALL associated
assignment operators (e.g. +:=, -:= etc) together with ALL the UPPERCASE
equivalent operator, e.g. PLUSAB, MINUSAB etc (to provide portability
to wrist watches and main frames).  The net effect is an explosion of
operator definitions. [[User:NevilleDNZ|NevilleDNZ]] 21:53, 13 February 2009 (UTC)

:Maybe still too long?... I've finished a Objective-C implementation... and it is 276 line long (and lacks pow); maybe not too much... I post it after few testing. --[[User:ShinTakezou|ShinTakezou]] 21:13, 14 February 2009 (UTC)
:And it's going worse with Fortran; I've implemented only = and + (and not all possible interfaces), and it takes already 120 lines of code... --[[User:ShinTakezou|ShinTakezou]] 23:27, 14 February 2009 (UTC)

:I had the same problem with my REXX example, I had to resort to making some subroutines into ''one-liners'' (subroutines that detracted from the task's requirements), and remove quite a bit of whitespace to keep the program brief (as possible). -- [[User:Gerard Schildberger|Gerard Schildberger]] 08:10, 8 January 2013 (UTC) 


###  languages that already implement rational numbers? 

What about for languages that already implement rational numbers? Should we re-implement them? --[[User:Spoon!|Spoon!]] 05:00, 14 February 2009 (UTC)

:how about them just noting that fact and maybe giving an example of their use? --[[User:Paddy3118|Paddy3118]] 07:04, 14 February 2009 (UTC)

::Huh, technically, numbers in all computer languages I know are rational. Integer, fixed- and floating-point numbers of any natural radix are rational. Well, ideals NaN and +/-Inf of IEEE 754 aren't rational, though this does not count, because the rest is rational. I remotely remember some implementations of numbers based on Pi or e radix. Those are not rational. Complex numbers are not rational, of course. IMO the task name is quite misleading. It is looks more like a certain representation of rational numbers to be used in order to implement a certain subset of [http://en.wikipedia.org/wiki/Rational_number Q]. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 14:10, 14 February 2009 (UTC)
:::Of course. Looking at the implementation, what the task-writer means is a way of storing rational numbers explicitly as fractions of integers (numerators and denominators), and defining operators acting on these rather than on the whole rational number stored directly (as floating point). The languages that ''already implement rational numbers'' are those languages that treat rational numbers expressed explicitly as fraction of integers in a "natural" way, allowing to grab numerators and denominators, and doing automatic simplification (from 314/100 to 157/50 e.g.), and other operations avoiding computer floating point arithmetic --[[User:ShinTakezou|ShinTakezou]] 21:05, 14 February 2009 (UTC)

IMHO It certainly should be noted if a language already has Rational 
Arithmetic.  For example IPython (in the form of Sage) certainly has
Rational Numbers.

Sage even has these operators for arithmetic on algebraic expressions, 
modulo arithmetic, and differential forms and more.

Maybe we could make the task as "demonstrate how to implement a new 
field of arithmetic in the particular language"?  With the use example
being Rational Arithmetic. [[User:NevilleDNZ|NevilleDNZ]] 13:08, 14 February 2009 (UTC)

: I see what you are saying, but, Python isn't Ipython - we should explicitly explain when functionality is added by libraries that are not part of the standard distribution. --[[User:Paddy3118|Paddy3118]] 16:26, 14 February 2009 (UTC)

==Too long language examples==
After just considering adding an incomplete tag to the C entry and then finding on the talk page a reference about the Algol example being long too I am thinking about what should be done in these cases of bits being missed out for brevity's sake ''on this particular task''.

For comparison reasons it might be best to encourage people, maybe other people, to complete short examples and link to their entries when their completed entries are moved to a sub-page. In that case marking them as incomplete would help those searching for examples to finish-off. --[[User:Paddy3118|Paddy3118]] 00:58, 30 July 2011 (UTC)

: You can either write an elaborate, robust example, or a short, clear example, but rarely both.  And it doesn't help that this task is asking for too much.  Why integer division operator?  Why abs aperator?  What's the "etc" in "cast int to frac, etc"?  For a fraction implemetation to be reasonably useful, arbitrary length integer like GMP is almost a must, but GMP already implements rationals, making an implemetation pointless.  I think the task could be changed so that if an example implementation made the basic idea of a fraction class clear enough, and good enough to do the given test, it should be considered done.  After all, code here intend to demonstrate, not to serve production needs. --[[User:Ledrug|Ledrug]] 02:10, 30 July 2011 (UTC)
:: +1 on that. --[[User:Paddy3118|Paddy3118]] 03:03, 30 July 2011 (UTC)
