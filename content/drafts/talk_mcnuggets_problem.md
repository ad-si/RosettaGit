+++
title = "Talk:McNuggets Problem"
description = ""
date = 2019-02-04T14:41:19Z
aliases = []
[extra]
id = 22043
[taxonomies]
categories = []
tags = []
+++

== draft tasks seem to be born fully-grown lately ==
Perhaps somebody could comment or add some musings on how old a draft should be before it grows up?     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:57, 25 October 2018 (UTC)

== task name ==
You beat me to the punch.   I was going to enter a Rosetta Code problem next week which would've been called the '''Frobenius''' problem or some such   (I'm currently working on the wording of an unrelated Rosetta Code task).

  The function  <big>'''Frobenius'''</big>(a list of some numbers)   returns the largest number for the Frobenius equation:
                        I1*x1  +  ...  +  In*Xn   =   B.   
  At least two integers should be supplied.  If the integers aren't relatively prime,
  the result is infinity and is indicated by a negative one (-1) which is returned.  
  If any of the integers is equal to  '''1'''  (unity),  then  '''0'''  (zero) is returned.     
  Another way of approaching the description of the Frobenius number is:   given a set of integer-demoniation
  stamps (say,  '''4¢'''  and  '''9¢'''),  what is the largest value that those stamps can't represent?

This is why this problem is also known as the '''postage-stamp''' problem and was a real problem when buying stamps for mailing a package at the post-office which may have a restricted set of stamps, and people wanted/collected the different stamps, not wanting '''41''' one-cent stamps put on a package or envelope.   (Now-a-days, of course, the post office just produces a digital imprint of the exact decimal postage amount.)   The U.S. Post Office has (or used to) print a unique stamp for every cent denomination up to and including $1.   I don't know how many stamps are still being issued for over a (U.S.) dollar anymore.

I would have added some sets of numbers that have no highest value,   as well as "stamps" that are multiples of another.

I would not have restricted the high limit to   '''100''',   but left that open-ended   (in other words, infinity).

It would also get around the use of a trade-marked (TM or &trade;) term(s) and also a registered (R or &reg;) trade-mark term, but it seems that Wikipedia skipped around those problems.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:35, 25 October 2018 (UTC)

==Python: "composing generic abstractions"==
What use is the Haskell-like example? Especially when compared to the other Python example. RC is not about showing how to write Haskell in other languages it's about comparing idiomatic solutions in each language (idelly).  

I am considering deletng that example. RC is not the place to pursue private vendettas againt Python's percieved lack of support for a pet programming style in its guidelines. ([http://rosettacode.org/wiki/Talk:Cheryl%27s_Birthday Ref]).

:: The point is to demonstrate functional uses of Python, on which there is an established literature, and which meets the needs of particular contexts. If it resembles Haskell to you, that is because all functional programming, with any interpreter or compiler, is mathematically constrained by the same necessities of functional composition, and this leads to common use, across many languages, of shared generic abstractions. 
::By default, names for these abstractions tend to be taken, in many languages, including Python, from the Haskell/ML tradition, which simply happens to be the main idiom of the academic research in functional programming. The name of Python's '''dropwhile''' function is an example, as explained at the top of the Python itertools module documentation.
::You can safely leave it alone. No need for deletionary zeal. Like it or not, functional programming is just one of the ways in which Python interpreters and compilers are used. Live and let live. I have absolutely no objection whatsoever to other styles of programming. What is good depends entirely on what we are  optimising for, and in what context, and what is meant by 'idiomatic' is best left to linter tools. This code is fully linted by AutoPep8 and Auto-flake8. That seems enough to me. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 08:15, 27 October 2018 (UTC)
:: Perhaps the helpful part of your question is about what that variant adds technically. The shortest answer might be (1.) The use of currying in Python (2.) The application of the itertools.dropwhile function (3.) the relationship between set comprehension and the (concat . map) composition of two atomic and universal functions. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 08:42, 27 October 2018 (UTC)
: paddy Well, I suppose one 'use' is that it does have a style. The examples now under REPL import functional idioms from itertools and abuse them in some sort of style chimera. Somewhere in this discussion you complained about the use of functional idioms rather than the pythonic Comprehensions. These were introduced in Python2 and the What's New in Python2 documentation explains in length how to map one to the other. Why then when I write a solution importing nothing and using only Comprehensions do you denigrate it as 'From F#'. Surley this is the Pythonic Solution--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:08, 29 October 2018 (UTC)

:: Hi Nigel; according to the page history [http://rosettacode.org/mw/index.php?title=McNuggets_Problem&type=revision&diff=271712&oldid=271667 You have "denigrated" your own example]! Please check. [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:48, 29 October 2018 (UTC)
