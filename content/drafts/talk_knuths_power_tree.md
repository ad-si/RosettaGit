+++
title = "Talk:Knuth's power tree"
description = ""
date = 2016-03-19T22:32:23Z
aliases = []
[extra]
id = 19212
[taxonomies]
categories = []
tags = []
+++

==duplicate of addition-chain exponentiation?==

This looks like a duplicate of [[Addition-chain exponentiation]]. Perhaps some of the content here belongs on that page? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:46, 2 June 2015 (UTC)

-----

''Knuth's power tree''   isn't a duplicate of   ''addition-chain exponentiation'',   Knuth's power tree is much simpler to compute, and in almost all of the exponentiation process, is different than addition-chain exponentiation (but yields the same result, of course, albeit in a different way).

In the Rosetta Code task   ''Addition-chain exponentiation'',   there's a lot of reference to the   ''binary''   method (also known elsewhere as   ''the factor method'')   which, as noted in the preamble text of this Rosetta Code task:

:: For   ''n''   ≤ 100,000,   the power tree method:
::::*   bests the factor method   88,803   times,
::::*   ties   11,191   times,
::::*   loses   6   times.

From this, it can be seen that Knuth's power tree isn't always the best algorithm for exponentiation (as compared to   ''the factor method''),   but it's better over 88% of the time.

Knuth's power tree probably resembles addition-chain exponentiation as much as calculation of primes via   ''trial division''   versus   ''Sieve of Eratosthenes'',   they have the same result, but with different strategies and complexity. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:49, 2 June 2015 (UTC)

: Ah, my mistake.

: Triggered, on re-reading, by an utter lack of description of the algorithm on the page itself, combined with [for me] unfamiliar (and unlinked) terms for familiar concepts (like "factor method" - technically all of these approaches are "factor methods" so without some definition the term winds up being ambiguous). And I don't have a copy of Knuth's book handy. I guess I am supposed to reverse engineer one of the linked implementations? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:13, 2 June 2015 (UTC)

:: I have Knuth's Vol. 2 book, but the algorithm is way beyond my ability to explain it or paraphrase it, or even parrot it.   (My utter is bigger than your utter, and I have a book for help).   Even if I could explain it, Dr. Knuth's explanation of pretty detailed and extensive.   I've read (and re-read) the particular chapter and still don't know it well enough to explain it.   I was thinking about quoting the text wholesale, but I didn't want to push that particular envelope.   Perhaps someone with a much better mathematics background could dive in that pool and illuminate the process   (well, Dr. Knuth did, but I'm still a bit mystified and unilluminated).   I was hoping that the <strike> two </strike>   several computer programming examples (as referenced by the links) would provide enough insight to code other computer programming language examples (entries). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:33, 2 June 2015 (UTC)

-----

The   ''factor method''   or   ''binary factor method''   or more simply   ''binary method'',   is as follows (as mentioned in Knuth's book):

(Note that this   isn't   a description of Knuth's power tree method)


For a positive integer   ''n''   (for an exponent):
::*   express   ''n''   in binary form   (ignore any leading zeroes)
::*   substitute each   <big>1</big>   by the (two) letters   <big>SX</big>
::*   substitute each   <big>0</big>   by the (single) letter   <big>S</big>
::*   remove the (two) leading (on the left) letters   <big>SX</big>

We now have a method (or "rule") for computing   <big>x<sup>n</sup></big>. 
 

(From left to right): 

::*   <big>S</big>   means to   ''square''   the number, 
::*   <big>X</big>   means to   ''multiply''   the number by   <big>x</big>. 




''' ══════════   An illustrative example of the binary factor method   ══════════ '''

for the power   <big>23</big>,

it's binary representation is   <big>10111</big>,

so the following sequence is:   <big>SX S SX SX SX</big>.

Now, remove the leading (left)   <big>SX</big>   two letters,

resulting in   <big>S SX SX SX</big>. 


So the rule (sequence) is:
:::*   square
:::*   square
:::*   multiple by   <big>x</big>
:::*   square
:::*   multiple by   <big>x</big>
:::*   square
:::*   multiple by   <big>x</big>

Or, in wording it in another way, we have computed:
::::*   <big>x<sup>2</sup></big>
::::*   <big>x<sup>4</sup></big>
::::*   <big>x<sup>5</sup></big>
::::*   <big>x<sup>10</sup></big>
::::*   <big>x<sup>11</sup></big>
::::*   <big>x<sup>22</sup></big>
::::*   <big>x<sup>23</sup></big>



 -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:33, 2 June 2015 (UTC)

:: Yes, but it's the power tree algorithm which we need here. Actually, I found https://comeoncodeon.wordpress.com/2009/03/02/evaluation-of-powers/ which indicates that the factor method is different from the binary method. I'm studying its description of the power tree algorithm now. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:59, 2 June 2015 (UTC)
