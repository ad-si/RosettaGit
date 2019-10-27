+++
title = "Talk:Sequence: smallest number with exactly n divisors"
description = ""
date = 2019-04-13T12:54:46Z
aliases = []
[extra]
id = 22275
[taxonomies]
categories = []
tags = []
+++

==The Combinatronics==
A loop factorizing integers to find the number of divisors is a lame way to implement this task!
If I use only 1 prime then it should be 2 and S(n) = 2**(n-1). I shall conject that if n is prime then S(n) is 2**(n-1) no matter how many primes I use, we shall see why later.
Let me consider the case where I use 2 primes. In general they form integers of the form (P1**A1)*(P2**A2), for a minimal P1 should be 2 and P2 should be 3 with A1 greater than or equal A2. 2*3*5=30 so any values I can find less than or equal 30 with 2 primes is minimal. The number of factors of such an integer will be 1+A1+A2+A1*A2. Anything larger than 2**5 is greater than 30 so I construct a table:

```txt

A1 A2 factors value
 1  1    4      6
 2  1    6     12
 2  2    9     36
 3  1    8     24
 3  2   12     72
 3  3   16    216
 4  1   10     48
 4  2   15    144
 4  3   20    432
 4  4   25   1296

```

By examination I can now produce S(1)..S(8). S(9) may be 36, S(10) may be 48 and(S12) may be 72, but being greater than 30 there may be a smaller value with 3 primes.
For 3 primes the number of factors is 1+A1+A2+A3+A1*A2+A1*A3+A2*A3+A1*A2*A3 and are good upto 2*3*5*7 (210). 2*3*5 has 8 factors but can not be better than 24. 2**2*3*5=60 and has 12 factors, so it replaces the existing guess (72). This also validates my current guess for 9 and 10. So I can now generate S(1)..S(13).
Realizing that the formula for the number of factors for n primes is (1+A1)*(1+A2)...(1+An) I can see why there is no value better than S(n)=2**(n-1) for prime n. I can also calculate S(14):
14=(1+6)*(1+1)->2**6*3->192.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:37, 13 April 2019 (UTC)
