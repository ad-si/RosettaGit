+++
title = "Talk:Babbage problem"
description = ""
date = 2018-12-04T09:02:51Z
aliases = []
[extra]
id = 20778
[taxonomies]
categories = []
tags = []
+++

==task clarification==

I can only assume that a   ''positive integer''   is meant to be found,   otherwise finding the   ''smallest negative integer''   would be pointless.


How about:

-99,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,025,264


(Of course, there are smaller numbers!)



And, in the hinterlands of the Rosetta Code coders, it was heard: 

<big>Oh yeah?   ''my''   googolplex thingy is bigger than   ''your''   googolplex thingy.   So there!</big> 

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:17, 13 April 2016 (UTC)

: Good point. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:52, 13 April 2016 (UTC)

:: I've clarified the wording so it now asks for the smallest positive integer. The reference in the Hollingdale and Tootill book only says 'smallest number': but the fact Babbage thought 99736 was the answer makes it clear it was a positive integer he was after. (Hope I'm doing this right—I'm quite new to Rosetta Code.) --[[User:Edmund|Edmund]] ([[User talk:Edmund|talk]]) 05:52, 13 April 2016 (UTC)

== computer program comments ==

It's a good thing that Charles Babbage, being English, understands ..., er, ...   ''English''   ---   otherwise all of our computer programming languages' comments would be for naught.   Ay, what?   Jolly good show!   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:56, 13 April 2016 (UTC)

==Pictures==
Can we not limit ourselves to a picture of the Analytical Engine, two pictures is a bit much. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 10:35, 13 April 2016 (UTC)
: Three pictures may be a bit much, two is just right.   It's hard to get 1.5 pictures for an average.   Just ignore the 2nd picture and not look at it.   It's not hurting anything (with the right-justified image).   This Rosetta Code task is more about Charles Babbage understanding the computer programs than his analytical engine.   I only included the image of the engine because I thought it looked interesting.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 11:21, 13 April 2016 (UTC)
:: It's crowding the BBC BASIC entry, at least on my screen. Also in other tasks we don't include pictures of the people who invented algorithms either. What is special about Babbage is not his mug but the Analytical Engine. Anyway, we could also move the BBC BASIC entry down, but it would leave a rather large gap. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 11:27, 13 April 2016 (UTC)
::: On my screen it looks good as is, but if we only have room for one picture my vote would go for Babbage. True, it's the Analytical Engine he's famous for—but this task is more about Babbage the person. "Write a program for <i>him</i> to read." [[User:Edmund|Edmund]] ([[User talk:Edmund|talk]]) 20:12, 13 April 2016 (UTC)

:::: As for (user) Fwend's screen crowding, that concern/issue will go away as the   TOC   (table-of-contents)   grows larger.   This is a pretty simple task as far as Rosetta Code tasks (problems) go.   As for who invented what, the picture of Charles Babbage is there as he   (or rather, his  comprehensibility/understandability)   is the main focus of the Rosetta Code task   (as we are writing/creating computer code so that   ''he''   can comprehend and understand the code)   ---   as far as I can tell, that requirement is a first for Rosetta Code.   His picture   (or as it was said, his mug)   wasn't included because of what he invented.   I never assumed or thought that Charles Babbage invented this (or these) particular algorithm(s), we (the programmers ''et al'') are creating the algorithms ourselves, hoping that the clarity and/or simplicity of the computer programming code will be understandable by Babbage (who has never seen a computer or computer program, except possibly for a Jacquard loom).   However, I'm sure that Charles Babbage was intelligent enough to only try integers that ended in the decimal digits   ''four''   or   ''six''.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:59, 13 April 2016 (UTC)

::::: Ahem yes—unlike most of us, so far... [[User:Edmund|Edmund]] ([[User talk:Edmund|talk]]) 21:23, 13 April 2016 (UTC)

==Upgrading from draft task?==
Would anybody object if I upgraded this from a draft to a "proper" task? It now has solutions in 20+ programming languages, and there seems to be a reasonable degree of clarity about what it requires.
[[User:Edmund|Edmund]] ([[User talk:Edmund|talk]]) 14:18, 20 August 2016 (UTC)

: Yeah, take it out of draft status. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 15:07, 20 August 2016 (UTC)

== 64-bit integer arithmetic ==

Hopefully Mister Babbage you guessed wrong! The solution of your problem is 25264 and not 99736. Hopefully because half of the languages examples would have been wrong. Because the square of 25264 (638,269,696) needs only the 32-bit integer type but the square of 99736 (9,947,269,696) needs the 64-bit integer type! And a lot of languages have problems with it. --[[User: PatGarrett|PatGarrett]] ([[User talk: PatGarrett|talk]]) 19:47, 11 February 2017 (UTC)

:There is no need to use 64 bit, as we are only interested in the residues modulo m=10^6, and in modular arithmetic we can just do every addition, subtraction and multiplication modulo m. Thus we can write x = 99,736 = 99 * 1000 + 736; it follows that x² = 99² *1 000² + 2*99*736*1000 + 736². As the first term is 0 mod 10^6, only the last two remain. For the middle term, 99*2*736*1000 = 99*1,472,000 = 99*472,000 = 46,728,000 = 728,000 mod 10^6, thus x² = 728,000  + 541,696 = 1,269,696 = 269,696 mod 10^6. None of the numbers exceeds 31 bits. Of course, as many programming languages do silently calculate mod 2^31, the problem will not be seen. --[[User:Rainglasz|Rainglasz]] ([[User talk:Rainglasz|talk]]) 21:08, 1 December 2018 (UTC)

:As Babbage did use paper and pencil (this takes less than an hour for the calculations once you have found the method), the results come not necessarily in ascending order, and he probably stopped at the first number that solved his problem, not suspecting that there was a second (smaller) solution with the same number of digits, although he -- as a trained mathematician -- had naturally specified ''smallest integer'' in his problem statement. --[[User:Rainglasz|Rainglasz]] ([[User talk:Rainglasz|talk]]) 09:02, 4 December 2018 (UTC)

== Thoughts on Babbage's point of view ==

To wish that example programs might be easily understood by Babbage himself
is a good idea. As he was a trained and capable mathematician, who tend to 
write as terse as possible, verbosity is not necessary at all.

Furthermore, as he has planned his whole life to build a programmable
computer, the Analytical Engine (AE), its concepts should be the
starting point.

The AE could only combine two variables by addition, subtraction, 
multiplication and division (including delivery of the remainder),
and send the result to a third variable, not necessarily different from one
of the inputs, i.e. allow statements of the form

    V1 + V2 -> V3

If the contents of a variable had to be copied, zero had to be added,
and the result sent to the new variable.

A large number of variables was planned to be available,
and variables were seldomly overwritten, except in loops.
Although variables were numbered, this was just a name, not an index;
so using single-letter variable names like in mathematics should be fine.
But identifieres, i.e. words of letters, were not a concept obvious to 
him; remember that mathematicians often do not use a multiplication symbol.

As a variable could only be overwritten if it was zero before -- a complication
we should leave out here --, all not-initialised variables
can be assumed to be reset to zero at start time.

Loops were mentioned in his talk in Milan and the report extended
by Ada Lovelace, but never detailled. Nevertheless, simple loops
are fine.
Note however, that Jaquard cards never used loops, and repeating
pattern were created by repeating cards, so loops should only be used
where indispensible, not when elegant.

In contrast to Alan Turing, subroutines or functions were not present in the AE, 
so the examples should refrain from using functions.

As in all early machines, multiplication, although provided by hardware
in the AE, was time consuming, even if simple multiplications with 2, 3,
4 or 10 etc were rather quick.
Thus, Babbage would never had enumerated square numbers by multiplication,
but by using a binominal formula as in my example below.

No text output was availble; only tabular output of rows and columns
of numbers. So an example should just print numbers.
And of course, to check the lower digits, a division by a power of 10
would be used, no tricky string manipulations. 

This leads to my proposal in AWK (without header comments):

    #   Use x² = (x-1)² + 2x - 1 to enumerate the squares
    #   The variable x contains 2x in the loop
    #   Because of 500² = 250000, start at 500
    BEGIN {
        x = 500 
        y = x * x
        x = 2 * x
        do {
            x = x + 2
            y = y + x
            y = y - 1
            z = y % 1000000
            z = z - 269696
        } while (z != 0)
        x = x / 2
        print x, y
    }

More in-depth information on the AE can be found on my website [http://rclab.de/rclab/analyticalengine/]. Other examples are in the document on the AE Game [https://rclab.de/rclab/analyticalengine/visualae], but I currently do not think it would be useful to add the AE as a programming language.
--[[User:Rainglasz|Rainglasz]] ([[User talk:Rainglasz|talk]]) 16:57, 1 December 2018 (UTC)
