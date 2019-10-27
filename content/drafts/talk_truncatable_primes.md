+++
title = "Talk:Truncatable primes"
description = ""
date = 2012-09-14T17:36:08Z
aliases = []
[extra]
id = 8276
[taxonomies]
categories = []
tags = []
+++

== Is Zero allowed? ==
I think the task should explicitly state whether zero is allowed in the numbers or not.
If allowed then 999907 is the largest left-truncatable prime less than 1000000.
--[[User:Tikkanz|Tikkanz]] 00:58, 9 September 2010 (UTC)

:Thanks Tikkanz. I've disallowed zero as this is done in part of the Mathworld article. It means disallowing '07' as being a prime for example, and seems reasonable. --[[User:Paddy3118|Paddy3118]] 03:27, 9 September 2010 (UTC)

== OEIS ==
For reference, a few of the related OEIS sequences are [http://oeis.org/A024770 A024770] (right) and [http://oeis.org/A024785 A024785] (left).

==Phantom Category==
I was trying to test cleaning up some categories of tasks (see [[Rosetta_Code:Village_Pump/Grouping_tasks]]) and thought I'd start with Primes, Prime, and Prime Numbers.  So I added Category:Prime Numbers but low and behold I can't find where [[Truncatable_primes]] references ''Primes''.  In the html source there is a "wgCategories=[" inside a script and I can see at the bottom where a "Category:Primes (page does not exist)" is generated but I can't find where to fix this.   Help?  --[[User:Dgamey|Dgamey]] 10:29, 18 May 2011 (UTC)
:Perhaps in the Clojure entry? --[[User:Rdm|Rdm]] 11:11, 18 May 2011 (UTC)
:: Ah ha.  Not Clojure, Haskell references a library called Primes.  That has to be the wrong way to do it! It would create dozens of phantom categories all over RC. It has to be a primes member or package in some Haskell library. Is there a haskell user that can fix this out there?  --[[User:Dgamey|Dgamey]] 11:26, 18 May 2011 (UTC)
:::If that's the real name for the library then that is the right way to do it. It may need to change to something like <nowiki>{{libheader|Primes (Haskell)}}</nowiki>. --[[User:Mwn3d|Mwn3d]] 12:22, 18 May 2011 (UTC)

==redefinition of truncatable primes==

I think this task's definition of ''truncatable primes'' needs to be redefined to include the phrase '''base ten'''.  It could be something like:

: A truncatable prime (expressed in base ten) is a prime that when successive digits are removed from one end of the prime, all numbers thus found are prime.

or some such wording.  

As an alternative, could MathWorld™'s definition be used? -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:17, 12 September 2012 (UTC)

:Hi Gerard, there is nothing in the task description implying a base at all and so usually we mean base ten. People talk about "the prime numbers" meaning the prime numbers in base ten without having to explicitly mention it and without confusion. You're not incorrect, (sorry about the double negative); but is it necessary. --[[User:Paddy3118|Paddy3118]] 18:38, 12 September 2012 (UTC)

:: It's not the base of the prime number(s) ''per se'', but when the description mentions taking the right- (or left-most) digits, those digits (in the case of truncatable primes) are specific to base ten.  Prime numbers are prime in any (positive integer) base.  Taking '''a''' digit from that expression of a prime requires specifying a base in this context. Indeed, there are only 83 right-truncatable primes in base ten, and 4,260 left-truncatable primes in base ten. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:59, 12 September 2012 (UTC)

:::Hi Gerard, I still can't see how talking about taking a digit would mean anything but a decimal digit from a base-10 representation of a prime? The task description on RC leaves out any mention of numerical bases. You introduce bases above when you talk of the number of right-truncatable primes but if you had just said that "There are only 83 right-truncatable primes" then wouldn't a base of 10 be automatically inferred? --[[User:Paddy3118|Paddy3118]] 21:48, 12 September 2012 (UTC)

:::: No, as others have discussed this (outside of Rosetta Code) and are careful in mentioning what base is being meant, is being used, or is to be used.  The number 787 in octal is 1423. 

 If you take the digit '''4''' (from the 1423 number) ... 


this statement doesn't infer that 1423 is base ten. -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:05, 12 September 2012 (UTC) 

-----

There are a few number sequences that are in (or implied) base ten (but some sequences could be in other bases):

* look-and-say (look, then say how many digits there are, in order, of a positive integer)
* apocalypse primes (a prime that is exactly 666 digits)
* apocalyptic (a positive integer power of two that contain a '''666''' in the number)
* Armstrong (N-digit non-negative integers which are equal to their sum of the Nth powers of their digits)
* dear (integers whose digits are 7, 8, or 9, but not two of any of those)
* dearer (integers whose digits are any of two digits of 7, 8, or 9)
* dearest (integers which have at least one each of the digits 7, 8, or 9)
* cheap/cheaper/cheapest (similar to above, for digits 1, 2, or 3)
* middling/middlinger/middlingest (similar to above, for digits 4, 5, or 6)
* curvaceous (non-negative integers whose digits are written with curves (digits 3, 6, 8, 9, and 0)
* curviliner (positive integers whose digits are written with curves and straight lines (digits 2 or 5)
* deBruijn (a sequences of digits for non-negative integer N such that every possible combination of digits is in the sequence)
* digCount, digitCount (the number of each of the digits in a non-negative integer) 
* digitSequence (number of unique numbers that can be found in the base ten expression of the non-negative integer N)
* hateful (positive integers which contain the numerals '''666''')
* prime digit primes (which all digits of P are also prime)
* rare (positive integers in which N+r and N-r are equal, and N is non-palindromic, r is the reverse of N)
* rep unit primes (primes which only contain the digit one)

(Pardon me if some of those sequences should be capitalized.)  Also pardon me if I didn't get the exact definition stated correctly, I was trying to be succinct so that the definition would fit on one line. I was switching between digits and numerals, numbers and integers, and whatnot.  I took a couple of handfuls of sequences that can be described easily (well, I tried to keep the descriptions short and simple).

A few sequences are shown in base ten, but use another base, such as xenodrome numbers (base 12), xenodrome numbers are integers, which expressed in base 12, has every numeral (digit) different from each other, that is, each digit is unique). -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:46, 12 September 2012 (UTC)



I've written a handy-dandy, slicer-dicer, one-size-fits-all, all-in-one general purpose calculator (in REXX) that has all those functions and more, around 1,500 functions). A lot of the Rosetta Code tasks that have REXX solutions were ripped out of that program, dumbed down, much (if not all) error checking removed, and generally, put on a severe diet before posting to RC. I wish Rosetta Code had the space for it. -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:05, 12 September 2012 (UTC)

:Hi again Gerard, my point wasn't that truncatable primes could not be done in other bases, just that, in the lack of any base mentioned in the task description then a base of ten is meant and has been understood by all the example writers for this task (including yourself). Mentioning base 10 as I have now done might not help someone new to the concept and might distract them from what is needed to understand the task. Do you think mentioning base 10 is ''necessary''?
:Maybe because you have or are considering a solution in other bases, this is driving your request? I am questioning it as I would like to understand if I have dumbed the task down too much and truly missed a salient point or if not mentioning the base was enough and maybe more clear  for the purposes of the task (not involving bases other than ten). I wish others would add their opinions too :-)
--[[User:Paddy3118|Paddy3118]] 06:54, 13 September 2012 (UTC)
::Paddy, be careful what you wish for. The largest truncatable prime in a given base can be estimated by probabalistic methods from the Zeta Function. Computing the largest truncatable prime in several bases and comparing them with the estimated values can be used to gain insight into the Zeta Function and maybe the Reimann Hypothosis.--[[User:Nigel Galloway|Nigel Galloway]] 12:01, 14 September 2012 (UTC)
::While claryfing this task you may wish to be clear if you consider 1 to be a prime number, you should not. From which it follows that 13 is a left reducable prime, but not a right reducable prime.--[[User:Nigel Galloway|Nigel Galloway]] 12:10, 14 September 2012 (UTC)

:::Surely treating one as prime is not the ''[http://oeis.org/A000040 normal]'' thing to do. Both Sloane and Mathworld don't consider it so. If someone ios mentioning Truncatable primes in bases other than ten then what base you are considering is important. You may well be having conversations off-site that are clarified if the bases involved are stated, but the task description is right in itself. --[[User:Paddy3118|Paddy3118]] 17:36, 14 September 2012 (UTC)
