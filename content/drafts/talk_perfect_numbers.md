+++
title = "Talk:Perfect numbers"
description = ""
date = 2016-04-06T07:02:51Z
aliases = []
[extra]
id = 8025
[taxonomies]
categories = []
tags = []
+++

= Definition Error =

The alternate definition is awkward/wrong and doesn't make sense.  How can a number have a factor larger than itself?
: An equivalent condition is that <tt>n</tt> is perfect if the sum of <tt>n</tt>'s factors that are less than <tt>n</tt> is equal to <tt>n</tt>.
Replaced definition with wp ref and text 
:: perhaps this was referring to the method used in the rational arithmetic task?
--[[User:Dgamey|Dgamey]] 14:57, 19 August 2010 (UTC)
:The difinition above is correct. It doesn't necessarily allow for factors larger than n, but it does not include n in the sum of the factors. For instance, 6 is perfect. Its factors are 1, 2, 3, and 6. The "factors that are less than" 6 are 1, 2, and 3 which add up to 6. I don't see a problem with that definition. --[[User:Mwn3d|Mwn3d]] 15:52, 19 August 2010 (UTC)
:: Hmmm ...  It was the word factor rather than divisor that made it seem wrong. --[[User:Dgamey|Dgamey]] 01:06, 20 August 2010 (UTC)
:::A brief search on the wikipedia oracle seems to show that they are the same unless you're talking about graphs. It doesn't make any difference to me or math books which word you use in this case, so whatever it is now I say leave it. FFR they are the same, though. --[[User:Mwn3d|Mwn3d]] 03:08, 20 August 2010 (UTC)

:::: The   ''factors that are less than 6 are''   ...       --- Another term for that is   ''proper divisors''.   Proper divisors of   '''X'''   are all (positive integer) divisors of   '''X'''   except   '''X'''.   Unity is treated as a special case.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:32, 23 March 2012 (UTC)

=Haskell example may need some attention=

Perhaps the Haskell example needs a little attention ?

It shows a list of results in which 8128 ( a perfectly perfect number ) is mysteriously followed by 8129, which looks like an accident … 

(if for example, we define divisors as:

> divisors x = [d | d <- [1..x-1], x `mod` d == 0]

and perfect as

> perfect n = (n == sum (divisors n))

then 

> perfect 8128

evaluates to True, but 

> perfect 8129 

is False  ( sum (divisors 8129) == 751  )

[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:27, 13 October 2015 (UTC)

Scratch that - I misread a test as a result (mea culpa)   (and the code is wonderfully fast, incidentally … ) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:01, 13 October 2015 (UTC)
