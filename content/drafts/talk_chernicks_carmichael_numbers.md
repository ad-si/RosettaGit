+++
title = "Talk:Chernick's Carmichael numbers"
description = ""
date = 2019-07-20T12:09:53Z
aliases = []
[extra]
id = 22350
[taxonomies]
categories = []
tags = []
+++

== does a(10) exist? ==
Does anyone know whether a(10) actually exists?

I've checked all values of 'm' up to 16 billion and found nothing. This is in contrast to a(9) which only required 'm' equal to 950,560. 

So, if a(10) does exist, it must be very large and, given the nature of the constraints, the probability of finding 10 primes which satisfy them is beginning to look low to me. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 18:48, 3 June 2019 (UTC)

: a(10) was discovered today by Amiram Eldar (the author of the [https://oeis.org/A318646 A318646] sequence) for m = 3208386195840. -- [[User:Trizen|Trizen]] ([[User talk:Trizen|talk]]) 12:10, 4 June 2019 (UTC)

Yep, knowing that, I've now found a(10) to be:

24616075028246330441656912428380582403261346369700917629170235674289719437963233744091978433592331048416482649086961226304033068172880278517841921

So my 16 billion wasn't even in the right ballpark and I estimate it would have taken my Go program about 8.5 days to find it, albeit on slow hardware. On a fast machine, using a faster compiler and GMP for the big integer stuff, you might be able to get this down to a few hours but it's probably best to remove it as an optional requirement as I see you've now done. Interesting task nonetheless so thanks for creating it. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 15:03, 4 June 2019 (UTC)
::Or we could keep it. The task only gets interesting at 10. See below.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 09:34, 6 June 2019 (UTC)

:: In C++, using 64-bit integers, it's possible to compute '''a(10)''' in just '''~3.5 minutes''', with GMP for primality testing, '''preceded''' by trial division over odd primes up to p=23. It's very likely that one of the 10 factors ('''(6*m+1)''', '''(12*m+1)''', '''(18m+1)''', ...) is composite, having a small prime divisor '''<= p''', therefore we can simply skip it, without running a primality test on it. Another big optimization for '''n > 5''', noticed by Thundergnat (see the Perl 6 entry), and explained bellow by Nigel Galloway, is that '''m''' is also a multiple of '''5''', which makes the search 5 times faster. At the moment, the largest known term is '''a(11)''' (with '''m = 31023586121600''') and was discovered yesterday by Amiram Eldar. -- [[User:Trizen|Trizen]] ([[User talk:Trizen|talk]]) 14:27, 6 June 2019 (UTC)

:::Amazing speed-up as a result of these optimizations which, of course, are always obvious after some-one else has pointed them out :)

:::I've now added a second Go version (keeping the first version as the zkl entry is a translation of it). Even on my modest machine, this is now reaching a(10) in about 22 minutes so I'm very pleased. Think I'll leave a(11) to others though :) --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 10:07, 7 June 2019 (UTC)

==Optimizations==
The good news is no [[Casting out nines]]. The coefficients cₙ of the polynomials for 10 are:

```txt

 n   cₙ
 1    6
 2   12
 3   18
 4   36
 5   72
 6  144
 7  288
 8  576
 9 1152
10 2304

```
  
For 10 m is divisible by 64, so the sequence begins  64 128 192 256 320 384 ... it is easy to see that this continues with 4 8 2 6 0 as the last digit. Which are the same last digits as cₙ. Consider the following table which shows the final digit of the number produced by the polynomial cₙ(m)+1:

```txt

  m   2  4  6  8
  64  9  7  5  3
 128  7  3  9  5
 192  5  9  3  7
 256  3  5  7  9
 320  1  1  1  1
 384

```

Remembering that prime numbers cannot end in 5 from the above I deduce that if m is not divisible by 320 then at least 1 of the polynomials must have a non prime result. This reduces the work to be done by 80%. Using this I obtained an answer in 6h, 31min, 16,387 ms. This could easily be halved using pseudo-primes and validating the final result. There is an obvious multi-threading strategy which could cut the time to 1/number of threads. More interestingly programming wise is start prime testing for a given m from the 10th polynomial. Observe that if this is not prime then for m*2 the 9th polynomial can not be prime (it is the same number). Similarly for m*4 the 8th polynomial can not be prime ... down to m*128 for the 3rd polynomial and then m*384 for the first polynomial. If the 10th polynomial is prime but the Nth (down to the 3rd) is not then the same applies just shifted. Keeping track of the m that do not need to be tested should reduce the time significantly.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 09:34, 6 June 2019 (UTC)
