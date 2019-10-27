+++
title = "Talk:Generalised floating point addition"
description = ""
date = 2013-12-23T19:17:36Z
aliases = []
[extra]
id = 10735
[taxonomies]
categories = []
tags = []
+++

== Clarify? ==

I have a few questions:
# Why?
# A floating point number involves three integers: the significand, the base and the exponent.  Wouldn't it be more of a hoot to allow them to be in different bases?
# Suppose I want to compute a + b.  Is it acceptible to first convert them into machine native format, add up, then convert back to whatever base they were in?  Could a and b be in different bases to begin with? --[[User:Ledrug|Ledrug]] 16:52, 28 October 2011 (UTC)

1. re: Why?

This pattern (or subsets of this pattern) happen often enough that it is worth implementing it in general.  Hence the emphasis on a "Mixin" or Template" solution.  More generally the "DIGITS" are not specifically numerical, they only require a "+" operation, a digit index, MSD, LSD and the concept of ZERO.

2. re: ... allow them to be in different bases?

That would depend on a natural example of the significand, the base and the exponent in different bases.  [[wp:IEEE_754-2008#Basic_formats|IEEE_754 Basic_formats]] has [[wp:Decimal128_floating-point_format|Decimal128 floating-point format]] appears to be an example BCD significand and "modified" Binary exponent.   /* I'm guessing that this is because the input and output data are decimal */. But your suggestion is reasonably close and if it can be implemented reasonably cleanly then it would probably fit the Rosettacode's spirit of "codifying your suggestion".  The one thing I love about Rosettacode is the surprises that emanate from other languages and other interpretations.

3. re: ... Is it acceptible to first convert them into machine native format.

I saw this more as a Templating and significand "list processing" "algorithm" definition task. /* Esp as the "DIGITS" may not even be numerical. */  So my preference is to avoid normalising to a canonical form based on a specific hardware implementation.  [ If you know what I mean... I can almost say that last paragraph in one breath... :-) ]

[[User:NevilleDNZ|NevilleDNZ]] 11:10, 29 October 2011 (UTC)

=== Repeating the "why"? ===

I have posted an implementation of J, based on the answer given here for "why" (ignoring concepts that seem unnecessary and focusing on the desired result).  I believe that this implementation is convenient to use, fast to implement, and that it performs reasonably well.  Is this an acceptable implementation?  If so, could the task description be changed to allow it?  If not, perhaps you could expand on the "why" to explain why I need to do something different?  Thanks!  --[[User:Rdm|Rdm]] 08:22, 30 October 2011 (UTC)

The task requires "Calculate the terms for -8 to 20 in this sequence".  I notice J only got as far as -4.  Most programming languages are constrained by their hardware floating point.  Maybe J isn't, but most are.  Hence the need for a Template/Library in these less advanced languages.

> More generally the "DIGITS" are not specifically numerical, they only require a "+" operation, a digit index, MSD, LSD and the concept of ZERO.

IEEE has [[wp:Decimal128_floating-point_format|Decimal128 floating-point format]] in BCD for just this reason.  In this case BCD is used where performance and/or accuracy is required.

[[User:NevilleDNZ|NevilleDNZ]] 08:45, 30 October 2011 (UTC)

: Yes?  I am being lazy here, and asking for a clarification of the task.  And, since no sequence was defined, I am being too lazy to derive the sequence for myself.  I am also ignoring most of the details of the task -- I am, in fact, not using "floating point" at all here, except as a notation (and that only loosely).  That said, I am confident that my implementation will have significantly better performance than BCD for these calculations -- the BCD implementation multiplies by 81 using 81 successive additions where I am just multiplying.  That said, performance is generally something we try and ignore on this site (except where it gets in the way).  --[[User:Rdm|Rdm]] 09:07, 30 October 2011 (UTC)

I have clarfied the definition of the sequence for avoid confusion.  I elected to demonstrate the sequence by example as seems to be the easiest way to describe it.

I have removed the reference to "Kudos" as I suspect that this word may be too loaded, and may have resulted in the emotive responses.  I will remove other such reference to "Kudos" from my drafts over the next day or so.

I am thinking that the reference to "self" in tasks should be removed: eg
* "I am not currently able to implement the task exactly because I do not quite understand what is being asked for (nor why it would be useful)."

Mostly because "I am" and "I do" look "unencyclopaedic", but also the "I" carries no ".sig" for the general reader to relate to.  For me it looks OK in a draft task, but past draft it looks a little strange.

Good luck with the task.  Bear in mind a ''test case'' is '''only''' the ''test case''.  The real "meat" is the task.

Thanks for the feed back. ;-)

[[User:NevilleDNZ|NevilleDNZ]] 10:14, 30 October 2011 (UTC)

: I can remove the "I" from my task implementation when that ceases to be relevant.  But you asked for the reason that the task cannot be implemented in a certain fashion and my lack of understanding is the reason.  So, for now, "I" stays...  Note that I am not using BCD at all here -- the closest I come to that is the initial and final representations of the numbers, but that's ASCII, not BCD.

: Also, analyzing your sequence: the exponents 63, 54, 45, 36, ... each differ by 9, and correspond to 9 * (7, 6, 5, 4).  So if 63 corresponds to the -8 value in your sequence, then the exponent of 9*0 corresponds to -1 value in your sequence, and the exponent of 9*-1 corresponds to the 0 value in your sequence.  I suspect this is an off-by-one error?  Wouldn't it make more sense for 63 to correspond to -7?  Or, better yet, make 63 correspond to 7, and ask for the values from 7 through -21 (or whatever -- since these values are bulky perhaps it would be better for the task page to ask for fewer, well chosen values)?  --[[User:Rdm|Rdm]] 12:22, 30 October 2011 (UTC)

Good idea. Changed sequence to be from -7 to 21.  It makes it easier to mentally digest. I also changed the multiplication to be from -15 to 13 for the same reason. [[User:NevilleDNZ|NevilleDNZ]] 13:46, 30 October 2011 (UTC)

: Ok, I have updated the J implementation to generate the full sequence.  But that leaves me with my previous question, so I will repeat it for this updated implementation: ''Is this an acceptable implementation?  If so, could the task description be changed to allow it?  If not, perhaps you could expand on the "why" to explain why I need to do something different?'' --[[User:Rdm|Rdm]] 14:54, 30 October 2011 (UTC)

To be honest I don't know J, hence cannot read it, so at the end of the day you are the judge of your own code.  The task  ''test case'' requires "Perform the multiplication of 81 by repeated additions". The repeated pattern "12345679" was picked to maximise the varieties of "carries" performed during addition, hence the requirement "Perform the multiplication of 81 by repeated additions" is a significant part of the ''test case''.  At the end of the day you are the judge of your own code, and visa-versa. [[User:NevilleDNZ|NevilleDNZ]] 21:18, 30 October 2011 (UTC)

I've had a thought.  This method of adding (and multiplying) two numbers together is taught to kids when counting their pocket money, then at grade school in basic decimal arithmetic (about the same time they learn fractions). Then it is in reapplied in highschool maths classes when dealing with numbers bases. And again the technique is reapplied as the student learns about binary computers where they also learn about hex & octal etc. So... I will change the '''task''' emphasis to be on emulating the general technique that is taught in junior classrooms.  [[User:NevilleDNZ|NevilleDNZ]] 22:48, 30 October 2011 (UTC)

: I am currently not doing BCD at all -- I do not see any point in using BCD -- I have explained this, and I am asking why because I do not see the point.  I could change from multiplication to repeated addition, and it still would not be testing BCD carry.  I also do not know what "template" means -- I might already be doing that, or it might be trivial.  Anyways, if the task is going to require BCD shouldn't it include something that demonstrates that BCD is in use?  (Though, honestly, I cannot imagine anything that actually requires the use of BCD.)  Maybe if you ask for intermediate results on the addition though? --[[User:Rdm|Rdm]] 00:53, 31 October 2011 (UTC)

I confess that I cannot think of much used for BCD, hence it is on the '''test case'''.  I was even surprised to find that [[wp:IEEE_754-2008#Basic_formats|IEEE_754 Basic_formats]] has [[wp:Decimal128_floating-point_format|Decimal128 floating-point format]]. 
From wikipedia: http://en.wikipedia.org/wiki/Binary-coded_decimal 
: BCD's main virtue is ease of conversion between machine- and human-readable formats, as well as a more precise machine-format representation of decimal quantities. As compared to typical binary formats, BCD's principal drawbacks are a small increase in the complexity of the circuits needed to implement basic mathematical operations and less efficient usage of storage facilities.

: BCD was used in many early decimal computers. Although BCD is not as widely used as in the past, decimal fixed-point and floating-point formats are still important and continue to be used in financial, commercial, and industrial computing, where subtle conversion and rounding errors that are inherent to floating point binary representations cannot be tolerated.

¢ On bit of a tangent, here is an interesting link: [http://articles.cnn.com/1999-09-30/tech/9909_30_mars.metric_1_mars-orbiter-climate-orbiter-spacecraft-team?_s=PM:TECH NASAs metric confusion caused Mars orbiter loss].  The problem wasn't because they used BCD (which I very much doubt they did).  More it is an example of where ''"subtle conversion and rounding errors"'' can do more then rob bank customers of a cent of two at the end of each month. ¢

My interest lies more in "Binary Coded Billions", also manipulation of polynomials, vectors etc.  All of these include patterns for addition and manipulation of arrays of overlapping terms.  But the same general code should also work for the more mundane BCD.  This makes BCD a good test case.

Your point about "shouldn't it include something that demonstrates that BCD is in use?" is well taken.  I have found an example in wikipedia's [[wp:Long division|Long division]] , eg [[wp:Image:LongDivisionAnimated.gif | LongDivisionAnimated.gif]].  Alas no such animated gif for long addition.  Maybe you could create an appropriate animated gif?

[[User:NevilleDNZ|NevilleDNZ]] 02:44, 31 October 2011 (UTC)

:Personally, if I wanted to work with polynomial multiplication, I would not bother with BCD.  To fill the niches that BCD fills, I would use one of three techniques:

:#Floating point integer (on a 32 bit machine with 64 bit floating points, I get 53 bits of integer using floats).
:#Arbitrary precision integer.
:#Arbitrary precision rational.

:I used the latter two here.

:BCD was a great hack when you had to fit all of your code into a few kilobytes of memory.  But (for example) representing dollars as cents, to avoid the binary fraction issues, is also a great hack.  Nowadays, BCD is mostly useful for backwards compatability and in systems that have included BCD support without supporting other options.

:So, anyways, if I wanted to illustrate polynomial multiplication, I would not bother with BCD.  Instead, I would focus on something like the [[wp:Chain rule (probability)|chain rule]].  For example:  ''If you flip a coin ten times, with 50% odds each for "Heads" and "Tails" on one coin flip, what are the odds that the total number of "Heads" is a prime number?''.  Or something like that that involves summing cumulative probabilities -- perhaps involving dice or perhaps even something with uneven base chances.  If you like, I could draft up a task on that subject.  (Or feel free to do so yourself.)  --[[User:Rdm|Rdm]] 10:33, 31 October 2011 (UTC)

:: That task already exists (morally) at [[Count the coins]]. [[User:CRGreathouse|CRGreathouse]] 17:40, 8 May 2012 (UTC)

:: FWIW, a fair number of languages have built-in arbitrary precision integers; they can do generalised floating point math without needing to fuss around with BCD. ''Requiring'' the use of non-idiomatic techniques when a high-quality idiomatic technique lies close by is a little odd. I'll look into rewriting the task a bit to make this possible. (BCD ''can'' be a possible implementation strategy, of course; I've no problem with that.) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 18:41, 23 December 2013 (UTC)

::: After some thought I think I'd like to draw several distinctions here. One distinction has to do with the number base (BCD vs Binary, for example). Another distinction has to do with precision (53 bits of mantissa? arbitrary precision integers?). Another distinction has to do with the represented range of numbers (integers are different from rational numbers). And yet another distinction has to do with "floating point".

::: Floating point, as I understand them, give us numbers of the form x*y^z where y is typically a constant and where x and z typically have constrained ranges. So one way of representing floating point numbers is using the pair (x,z) along with a specification of the remaining constraints. One might imagine this specification as being a part of an external standard, or a given for users of either certain computing hardware or a specific programming language, library or environment, or we might want a floating point number where each floating point number is an object and a representation of these constraints is embedded in each such object. But, of course, there will always be limits. If you have a floating point number that takes 16 gigabytes to represent you will not be able to have as many of those as you would a floating point number that takes 4 bytes to represent.

::: Anyways, to meaningfully choose tradeoffs, it's usually a good idea to express the purpose of the representation. As I understand it, floating point numbers are based off "scientific notation" where numbers are typically the result of measurements (and, thus, limited in precision while often needing to treat a wide variety of magnitudes). Another aspect of floating point numbers is that they can efficiently represent approximate results of transcendental functions (sine, cosine, logarithm, ...). For many practical purposes approximations are more than adequate - excessive precision can be thought of as distracting attention from the important issues.

::: Hopefully some of these musings will help you in your rewrite? And, thank you. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:17, 23 December 2013 (UTC)
