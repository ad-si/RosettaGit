+++
title = "Talk:Convert decimal number to rational"
description = ""
date = 2016-08-21T15:56:15Z
aliases = []
[extra]
id = 9915
[taxonomies]
categories = []
tags = []
+++

== Name change?==
To possibly: "Convert decimal number to rational". To make it more descriptive. --[[User:Paddy3118|Paddy3118]] 05:59, 12 June 2011 (UTC)
: Or "Convert from decimal into a fraction" --[[User:Markhobley|Markhobley]] 08:34, 12 June 2011 (UTC)
:: What are the plans for results greater than one? Do we want a "top heavy" fraction or whole number and fractional component? What should 3.5 and 7 look like? Presumably we want "7/2" and "7/1" or do we want "3 1/2" and "7"?
:::Well, since it is not specified, I would think that it is left to the individual, but giving a fraction that is still reducible, such as 5/10 would not feel right. --[[User:Paddy3118|Paddy3118]] 11:42, 12 June 2011 (UTC)
::::Yeah. We definitely need lowest terms.--[[User:Markhobley|Markhobley]] 12:14, 12 June 2011 (UTC)
::::: Yes - please rename this, something like "Convert Decimal to Fractional type" or even "Convert to Fraction" works.  My first reaction to the name was that this was some kind of duplicate task.  --[[User:Dgamey|Dgamey]] 11:05, 17 June 2011 (UTC)

== Decimal? ==
Are we talking about only rationals? What about 3.14159265...? --[[User:Ledrug|Ledrug]] 06:36, 12 June 2011 (UTC)
:Well if you can express it as a finite decimal expansion ... :-)
--[[User:Paddy3118|Paddy3118]] 11:44, 12 June 2011 (UTC)
:Presumably we are only working to the precision (or double precision) of the system registers, (or to the number of digits originally provided in numerical string based implementations), so the values would all be rational. --[[User:Markhobley|Markhobley]] 12:28, 12 June 2011 (UTC)

:: System registers? Double precision? I ain't knowing nuttin' 'bout registers or doubles nohow, noway. The REXX language (for instance) doesn't use floating point, nor registers.  It uses whatever precision is in effect, and that may be increased to almost anything pratical (eight million digits is about the useful limit). -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:23, 13 August 2012 (UTC) 

:Definitions: Rationals - the real numbers constructible as ratio of A/B, where A and B are both integers. Irrationals - the real numbers that cannot be expressed as such a ratio.
:The set of rationals exactly matches the set of numbers with decimal expansions that either terminate (i.e. fixed length), or become periodic (i.e. written it with a bar over the last N digits).
:The set of irrationals exactly matches the set of numbers with decimal expansions that neither terminate nor become periodic.
:Therefore, there is no way to put an irrational number into your code as a decimal number, even if you can also indicate number of repeating digits. You *can* say PI or sqrt(2) in most languages, but what you get from those is just the largest (truncated) decimal version that fits into your floating point representation.
:To summarize, IMHO, *yes*, only rationals make sense for this task. --[[User:Util|Util]] 14:49, 12 June 2011 (UTC)
:: Good, I just wanted to clarify whether the task was talking about machine precision floats or any conceptual real numbers, because specifying a "best approximation" for irrationals would make it a lot more complicated. --[[User:Ledrug|Ledrug]] 22:01, 12 June 2011 (UTC)

:: Bah, I implemented the best approximation method anyway, 10 digit integer fractions are just so useless.  The task author really needs to clarify what he had in mind. --[[User:Ledrug|Ledrug]] 01:22, 13 June 2011 (UTC)

== Repetitive digit chains and rounding ==

What do we want to do with chains of repeating sequences such as 0.333333333333? Do we want 111111111111/333333333333 or 333333333333/1000000000000 or do we need to round out the repetitions before evaluation (producing 3/10? or possibly even 1/3). --[[User:Markhobley|Markhobley]] 12:52, 12 June 2011 (UTC)

:I recommend allowing a second optional parameter to indicate number of repeating digits. dec2frac(0.1234,2) would mean 0.123434343434... (written as 0.1234 with a bar over the "34), and should convert to 611/4950, while dec2frac(0.1234) or dec2frac(0.1234, 0) would mean *exactly* 0.1234 (1234/10000), which should convert to 617/5000. --[[User:Util|Util]] 13:48, 12 June 2011 (UTC)

:For an algorithm to handle such indicated repeating digits, see [http://en.wikipedia.org/wiki/Repeating_decimal#Converting_repeating_decimals_to_fractions] --[[User:Util|Util]] 15:15, 12 June 2011 (UTC)

::It is certainly interesting, but I think it goes well beyond what is in the task description and given in the first, (canonical?), example in BASIC. I read it as needing a routine that can take the ''given'' decimal numbers and generate their rational approximations with no need to introduce repeating digit handling. --[[User:Paddy3118|Paddy3118]] 15:25, 12 June 2011 (UTC)

:::Yeah, you are probably right Paddy. The repeating sequence scenario only applies if it occurs across the full width of input. Handling of repetitive digits could be dealt with by a separate handler before this routine is called. A separate task could be to create such a handler. For this task (not the handler task) we could deal just with the conversion, and the output is taken as it is. --[[User:Markhobley|Markhobley]] 16:09, 12 June 2011 (UTC)

::::Whoops! I posted my Perl 6 solution (that handles rep-digits) before checking here. If the task comes out of Draft status with this decision (no rep-digits) intact, then I will reduce the Perl 6 solution to fit the simpler problem. I think that the Python solution will need similar attention. --[[User:Util|Util]] 16:39, 12 June 2011 (UTC)
::::: It probably won't need trimming. We can probably just add a note that the repetitive sequences are sanitized by this version of the program. --[[User:Markhobley|Markhobley]] 17:25, 12 June 2011 (UTC)

== J and best fit ==

This is a curiosity rather than critism, because I know nothing about J to begin with.  My question: how is a floating point stored in J, and how does it determine what a "best fit" is?  In the first example 0.518518 turned into a "866492568306r1671094481399" ("r" means "/" I assume?). Why? What about 518518/1000000? --[[User:Ledrug|Ledrug]] 17:57, 14 June 2011 (UTC)
:I believe J floats are bog standard ieee floats which means just under 16 decimal digits of precision.  And, yes, r means ratio -- it's the first integer divided by the second integer.  As for 866492568306 divided by 1671094481399, when you divide that out and compare 866492568306 with 866492568305 and 866492568307, you have to drop down to 11 digits of precision to before the +1 and -1 versions look identical.  That means that 866492568306 is roughly accurate to 12 digits of precision.  In other words, we might be ignoring up to 16 trailing binary digits with that answer (and apparently the algorithm being used in that version of J apparently requires that we keep over 10 digits of precision).
:I have not studied this matter very closely, but this seems like it is related to the way J checks for equality on floating point numbers:
:J uses an epsilon which is scaled by the value with the largest magnitude when checking for equality .  The idea is that floating point numbers that are meant to be equal are usually quite close.  In other words, when the difference between the two floating point values is less than (in the default case) 2<sup>-44</sup> times the larger magnitude value they are "equal".  This does not always do the right thing (floating point subtraction can get awful), and can be turned off when the programmer is prepared to deal with proper numerical analysis, but it's usually the right thing to do when using an algorithm that cares about equality in the context of floating point numbers.  --[[User:Rdm|Rdm]] 20:30, 14 June 2011 (UTC)
:: I see. 2<sup>-44</sup> epsilon makes it sound like J's doulbe is the typical 8 byte IEEE float, so if J were just taking the bits out of the float, the denominator would be a power of 2 or power of 10.  It probably did something similar to the continued fraction, but with a rather large denominator limit.  For the record, for .518518 my perl code also gives goofy numbers like 30117673588/58084142861 from 10<sup>11</sup> on, so it's just a matter of different rounding then. --[[User:Ledrug|Ledrug]] 21:15, 14 June 2011 (UTC)

==inaccuracies==

The first two examples (and fifth) aren't correctly presented.  The left side doesn't (exactly) equal the right side --- close ... but no cigar.  Unless, however, the equal sign is replaced with an approximately equal ('''≈''') or similar symbol, or the right side number is represented/indicated with some sort of a repeating fraction symbol(s), or an appended plus sign (or somesuch symbol) is used, or some statement to the effect,  ''n/m = .dddddd'' to '''x''' places.  Could it be assumed that the task is asking the program examples to use approximation methods?  Then, what precision could/would be used?  When using high precision arithmetic, crossing that "line" is really a far, far way down the pike. I was referring the '''high''' precision. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:07, 13 August 2012 (UTC)

: Given that floating point representation can only exactly represent fractions whose denominators are powers of 2, I think that we have to assume that an approximation is demanded here (and, frankly, this should hold -- that we are dealing with approximations -- in any context where floating point is used to represent fractions [unless all denominators are explicitly declared to be powers of 2], and also where floating point is used to represent integers which are larger than the floating point mantissa [unless, of course, the mantissa times power-of-two multiplier is explicitly declared to have sufficient precision for complete accuracy]). --[[User:Rdm|Rdm]] 20:26, 13 August 2012 (UTC)

:: No, not all floating point is represented in powers of two;  Intel (and others) have floating point decimal instructions (but hardly anybody uses it as far as I know, PL/I may be the exception as far as languages, and I haven't looked at what PL/I uses on non-IBM hardware). I believe '''C''' has a mention of it in the language definition, but I don't believe any '''C''' language implemented it (I'm not a '''C''' expert, --- hell's bells, I'm not even a novice, but I can spell it).  The REXX language uses base ten floating-point numbers, no matter what the underlying hardware can or does use.  The mantissa is only limited (in REXX) by how large one wishes to make it.  There is no hardware constrained limits of 16, 32, 64, 128, 256, ... 4096 ...  The REXX language for the most part in regards to expressing  numbers and performing arithmetic, endeavors to be human-centric, not machine-centric. It does arithmatic very much like a sentient CBLF would (that has ten fingers).  (er, ... carbon-based life form.) -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:51, 13 August 2012 (UTC)

::: Sorry, yes -- I assumed IEEE-754 floating point, as opposed to some other variation.  And, yes, the term "floating point" is general enough to cover all sorts of implementations.  Still, that doesn't change the fundamental issue here -- which is that floating point is an approximation unless explicitly declared otherwise.  If anything, it reinforces that conclusion (since it requires that the details of the floating point implementation be fixed before we can have a case where floating point is not an approximation).  --[[User:Rdm|Rdm]] 21:08, 13 August 2012 (UTC)

:::: When talking about floating point numbers to a REXXer  (er, I think that's what I am ...), floating point is exact.  10.77 is precise (no approximations) and that's the way REXX stores it: the way you see it here.  What you see is what you get.  I realized that for most programmers, this isn't the case when it comes to the storage or expressing of floating-point numbers.  I'd hate to have REXX excluded from this party because it uses an exact (base ten) floating point number repesentation with an (more-or-less) unbounded/unlimited mantissa (base ten), no guard digit(s), no binary representation of numbers.  REXX knows nothing of IEEE-anything floating point. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:25, 13 August 2012 (UTC)

::::: No.  That REXX floating point is exact for fixed length decimal fractions.  However, you cannot represent the result of dividing 1 by 3 exactly.  For example.  Well, you can, but not using that numeric representation.   --[[User:Rdm|Rdm]] 21:35, 13 August 2012 (UTC)

:::::: Yes, I wasn't talking about the ''result'' of an arithmetic operation (notably division), just a floating-point number. Sorry 'bout any confustion. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:41, 13 August 2012 (UTC)

::::::: That was sort of my point -- that floating point numbers (regardless of the base used) are necessarily approximations in a wide variety of contexts.  When dealing with measurements of distance, for example, this is assumed (since the measurements themselves are going to be approximations).  That said, the task description just says "decimal number" and not "floating point".  Still... the same holds there: finite length decimal numbers can only approximate fractions whose denominators are not representable exactly as powers of 2 and 5.  --[[User:Rdm|Rdm]] 22:08, 13 August 2012 (UTC)

== What do you mean by ''decimal number''? ==

This task confuses me.  I was told at school that a decimal number is a rational number which, once written in decimal notation, has a finite number of decimals.

So a decimal number has an easy rational way of writing it.  For instance:

<math>1.1007 = {11007\over 1000}</math>

If you consider repeating decimals, then you're talking about rationals, and the task then consists in finding the numerator and the denominator of a rational, given its integer part and its possibly infinite, but repetitive list of decimals.  This should be clarified imho.--[[User:Grondilu|Grondilu]] 00:21, 19 November 2012 (UTC)

== Task needs clarification / splitting up ==

Years after the above discussions, the task is still not clear. The solutions I checked have interpreted it in one of the three following ways:

# Take an '''exact''' rational number with a '''terminating decimal''' representation, and convert this representation to a fraction.
#: <small>This is what we've probably all had to do in math class in middle school. Multiply numerator and denominator by the appropriate power of ten to get an integer fraction, then reduce to lowest terms.</small>
# Take an '''exact''' rational number with a '''[[wp:Repeating_decimal|repeating decimal]]''' representation, and convert this representation to a fraction.
#: <small>Also middle-school level stuff, but slightly more interesting.</small>
# Take an '''inexact (floating-point) approximation''' of a rational number, and try to find the fraction that best matches it under certain parameters (e.g. limited digit size of the numerator/denominator).
#: <small>This one is more involved.</small>

This means that many of the solutions are currently not comparable with each other. I flagged the task with [[Template:Clarify task]] for now, but how should it be resolved? One idea would be to make this task about (1), with (2) as extra credit, and split off (3) as a separate task called "Approximate floating-point numbers as fractions" or similar. Thoughts?

--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 15:55, 21 August 2016 (UTC)
