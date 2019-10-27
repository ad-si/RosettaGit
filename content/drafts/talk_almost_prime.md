+++
title = "Talk:Almost prime"
description = ""
date = 2018-12-07T09:20:37Z
aliases = []
[extra]
id = 17260
[taxonomies]
categories = []
tags = []
+++

==Perl 6: Re: solution with identical output based on the <tt>factors</tt> routine==
Hi I was wondering if there is any import or using type statement together with saving factors in a specific named file needed to grant access to factors for the extra code shown or whether the shown code is to be added to the code for factors to get the output. If there is more then could you state it in the intro to the code? Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 22:42, 23 February 2014 (UTC)
:Fair enough. Currently just hand inclusion, but expected to change. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 01:09, 24 February 2014 (UTC)

==K-almost prime entries that are doubled from previous '''K''' entries==

I was tinkering on how to improve the speed of the computation of
larger K-almost primes  (both in number of each K-almost prime and
the range of '''K'''s of K-almost primes.

I'm sure most people noticed that each successive list of K-primes
for a specific K-almost prime,   a certain number of (initial) K-almost
primes are merely duplicates of the (first) previous '''K''' K-almost
primes.

For some K-almost primes:
:::   K-almost prime    1,        0   "doubles"
:::   K-almost prime    2,        2
:::   K-almost prime    3,        4
:::   K-almost prime    4,        7
:::   K-almost prime    5,            13
:::   K-almost prime    6,            22
:::   K-almost prime    7,            38
:::   K-almost prime    8,            63
:::   K-almost prime    9,                102
:::   K-almost prime        10,                168
:::   K-almost prime        11,                268
:::   K-almost prime        12,                426
:::   K-almost prime        13,                675
:::   K-almost prime        14,                    1064


The question then becomes, how to <strike>know</strike> determine how many previous entries
can be just doubled?   I don't know if there is a
formulaic way to compute that number.

Using a known number of previous doubled entries can be a
very fast way of computing K-almost primes, considering
that for the first 1,064 entries of 14-almost primes, they
can be computed directly by just doubling the first 1,064
entries for the 13-almost primes.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:37, 11 January 2016 (UTC)


I have   (since the above posting)   added optimization into the 2<sup>nd</sup> REXX entry, making it about a hundred times faster.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:22, 12 September 2017 (UTC)


-----


