+++
title = "Talk:Casting out nines"
description = ""
date = 2016-09-22T09:55:18Z
aliases = []
[extra]
id = 11917
[taxonomies]
categories = []
tags = []
+++

==Clarification of Task==

I am trying to understand this task.

Would it be fair to change the final sentence to read:  

:The task is to write code that given a numeric base and two numbers that mark the begining and end of a range use only this test to eliminate numbers from the range which cannot be valid Kaprekar numbers -- the result is the rest of the numbers from that range.

Or have I completely misunderstood what you are trying to convey?

--[[User:Rdm|Rdm]] 13:59, 25 June 2012 (UTC)

:OK, you have not misunderstood what I am trying to convey. I wanted to accept any test based on the congruence B^n=1(mod B-1). The Kaprekar numbers are a good objective because they have the property k%(B-1) == (k*k)%(B-1) and are explained elswhere on this site. I would accept any other objective.

:Ok!  I think the current task description is a bit too coy about the filtering mechanism.  It's almost equivalent to "filter numbers" since the description of the selection function is so general.  I am currently thinking it should be:

:: Given two numbers which mark the beginning and end of a range of integers [LO,HI], and an integer base (BASE), return the integers which fall in that range where the remainder after dividing the number by BASE-1 is the same as the remainder after dividing the square of the number by BASE-1.

::: That is the test, but I also want to keep the explanation of why we are doing this. Note that Ledrug has implemented the test as ((k*k) - k)%(Base - 1) at http://rosettacode.org/wiki/Kaprekar_numbers#C which is another variation. --[[User:Nigel Galloway|Nigel Galloway]] 13:03, 27 June 2012 (UTC)

: --[[User:Rdm|Rdm]] 14:02, 26 June 2012 (UTC)

:::I'd like to see a different title for the task.  The current description states right up front that the task is not casting out nines.  &mdash;[[User:Sonia|Sonia]] 19:46, 26 June 2012 (UTC)

::::OK, maybe you have misunderstood what I am trying to convey. Following Dr. Math at http://mathforum.org/library/drmath/view/55926.html describing casting out nines to the phrase "(You wouldn't normally get the same check digit for the result of the sum and the products; I just picked a weird example.)" I see that every Kaprekar is a "weird example". Using Dr. Maths "quick explanation of how to do it, without the big words" would be as slow as the String C++ Kaprekar solution. So we turn to http://mathworld.wolfram.com/CastingOutNines.html. What is said there is true for bases other than 10, therefore it is possible to develop a fast test. --[[User:Nigel Galloway|Nigel Galloway]] 12:47, 27 June 2012 (UTC)

:::(Comments moved from task page following Go solution.) I'm not seeing the connection. &mdash;[[User:Sonia]]

::::If you replace the line <code>if k%(base-1) == (k*k)%(base-1) {</code> with something like <code>if co9Peterson(k) == co9Peterson(k*k) {</code> in the C++ translation would it not solve the task? Obviously as written you would have to change k and k*k to strings and it would only work base10, but I think you have demonstrated the connection.--[[User:Nigel Galloway|Nigel Galloway]] 15:15, 28 June 2012 (UTC)

::: Done.  Posted now are my two Go solutions combined, generalized to other bases, and then modified to demonstrate what the task seems to be asking for.  I do think that people will protest that my casting out nines code is superfluous, and I will then repeat that the task should not be called casting out nines if nothing in the task is casting out nines. &mdash;[[User:Sonia|Sonia]] 19:20, 28 June 2012 (UTC)

:::: Agreed. I've changed the task description, do you prefer it? People should not now protest that your casting out of nine is superfluous. It has made it longer, so they may not thank you either. --[[User:Nigel Galloway|Nigel Galloway]] 12:32, 29 June 2012 (UTC)

::::: The new task description seems overly ambigous.  For example, in J, I could define the check digit as 9&| or I could define it as +/&(10&#.inv)(^:_) and I see nothing in the current task description to prefer one over the other (except that my existing implementation already uses the shorter version, and in general useless extra code is probably a bad thing -- if nothing else it's usually slower and harder to read).  Meanwhile, the application of this mechanism seems completely ambiguous.  ... from your above paragraph I was expecting new requirements, but I can't really figure out what they are.  I suppose I could replace 9&| with (co9=: 9&|) but that name would be misleading in the general case... anyways, I'll do the naming thing on one of my examples. --[[User:Rdm|Rdm]] 13:13, 29 June 2012 (UTC)

::: Casting out nines is always presented as a magic trick you can do with pencil and paper, but instructions for the trick vary from site, to site.  Wikipedia says to look over the whole number and cross out pairs of numbers that add to 9, Dr. Math just works with adjacent digits, Wolfram doesn't even bother to reduce numbers to a single digit.  Then there's the magic trick of adding or multiplying the reduced numbers as check for correct arithmetic with the full numbers.  An explanation of this really needs two parts, one to show that casting out nines produces a residue (or at least a smaller number) congruent mod 9, and another part to explain the congruence relation of modular arithmetic to the ring of integers.  The references touch on various parts of this but none of them paint the complete picture.

::: Part 3 of the current task makes these unexplained leaps.  "co9(x) is the residual of x mod 9."  Reference?  It's hard to find!  Dr. Math does say toward the end that "the check digit is essentially the remainder after you divide by 9", but Wikipedia has no mention of it, and Wolfram makes you conclude it from their typically terse explanation.

::: "the procedure can be extended to bases other than 9."  First, which procedure?  The procedure of digit sums and casting out nines to reduce a number to a smaller one?  Extending that to other bases is not even mentioned anywhere, much less shown.  Or the produre of checking arithmetic results?  This is where I think the congruence relation of modular arithmetic to the ring of integers needs to be pointed out, but it hasn't been done.  Or the procedure of optimizing Kaprekar number search with <tt>k%(Base-1) == (k*k)%(Base-1)</tt>?  It follows as a check of arithmetic results, but I'm not sure how obvious it is.

::: The task is kind of going in different directions.  I would simplify it by picking one.  For a task about the magic trick of casting out nines, I would require some digit sum and casting out of nines, I would point out the magic trick for sums and products, and sure, co9(k) == co9(k*k) for Kaprekar numbers is a fine application.  I would not try to explain how it is equivalent to modular arithmetic beyond disallowing %9 as an implementation of co9.  I also would not generalize anything to other bases.  That's hard to do by hand, and so pointless, I think.  Check the base 10 Kaprekar numbers and let people have their minds stimulated.  Actually, for the purposes of chrestomathy, I would put rules for implementing co9 right in the task description and specify to folow those rules, rather than refer them to the Dr. Math page, for example.

::: Another possible direction is to make the leap that co9 implments a modular reduction, and make the task about showing this equivalence.  I think this would be enough for a task.  No checking of arithmetic, no Kaprekar numbers.

::: Another possible direction would demonstrate optimization of Kaprekar number search in arbitrary bases.  Point out that modular arithmetic gives congruent results to arithmetic with larger numbers.  Reference http://en.wikipedia.org/wiki/Modular_arithmetic.  If you like, mention that this is the math behind the techniques of casting out nines.  That one little mention is all that is appropriate in this task.  No details, no applications, no history, just drop it.  Title the task "Modular optimization".  Make the observation that for all Kaprekar numbers, k%(base-1) == (k*k)%(base-1), and show some results in bases 10 and 17 that can be compared to results on the Kaprekar number task.

::: &mdash;[[User:Sonia|Sonia]] 19:37, 29 June 2012 (UTC)

::::Banning things and making up my mind, that would be 2 major lifestyle changes in one task, probably impossible.

::::Not too keen on just stating that k%(base-1) == (k*k)%(base-1) for Kaprekar numbers. I did that on the Kaprekar page the purpose here is to explain why.

::::I do like your implementation of co9 both the origional and base10 and the now extended to bases 2 to 32.

::::I have added another task http://rosettacode.org/wiki/Sum_digits_of_an_integer#C.2B.2B and shown how this can be used in http://rosettacode.org/wiki/Casting_out_nines#C.2B.2B11_For_Each_Generator.

::::I have added task http://rosettacode.org/wiki/Digital_root which I think your implementaions of co9 would fit well. From the ref1 in digital roots see http://en.wikipedia.org/wiki/Digital_root#Congruence_formula which may clarify some of your points

::::--[[User:Nigel Galloway|Nigel Galloway]] 12:32, 20 July 2012 (UTC)  
----
REXX  #=0; dead code? # not used
--[[User:Walterpachl|Walterpachl]] 20:34, 13 July 2012 (UTC)

: dead code removed. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:02, 13 July 2012 (UTC)

I think the task description could be simply:

Write a procedure (say co9(x)) which implements Casting Out Nines by returning the checksum for x. Demonstrate that for a given range of values, the procedure can be used to generate a subset of values with the property co9(k) = co9(k2), containing all the Kaprekar in the range.

[[User:Markhobley|Markhobley]] 16:33, 30 January 2013 (UTC)


==Formulae hidden to most browsers by under-tested cosmetic edits at 06:51, 15 August 2016 ==

Under-tested cosmetic edits made to the task page at 06:51, 15 August 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left some or all of the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 09:55, 22 September 2016 (UTC)
