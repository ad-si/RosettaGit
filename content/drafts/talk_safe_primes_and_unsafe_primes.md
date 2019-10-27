+++
title = "Talk:Safe primes and unsafe primes"
description = ""
date = 2018-10-12T14:19:19Z
aliases = []
[extra]
id = 22026
[taxonomies]
categories = []
tags = []
+++

Although speeding up checking sophie germain primes isn't that significant, there is a small improvement.

Think of wheel-sieving.
All numbers in the form 30*n+[1,7,11,13,17,19,23,29] are numbers with no dividers of 2,3,5

A sophie prime is a prime where p and 2*p+1 are prime

p must be 30*n+ { } therefore 2*p+1 must be in ( 2*30*n+ {} or 2*30*(n+1)+{} ) 

n= 0 -> 1,7,11,13,17,19,23,29 =>  3,15,23!,27,35 = 30+5 ,39 = 30+ 9 ,47!= 30+17 ,59!= 30+29

This shows, after 7, one needs only to test 30*n+ (11,17,23) and there counterparts 2*30*n+23,2*30*n+47,2*30*n+59
Using a bigger wheel like 2*3*5*7*11*13= 30030 -> 5760 possible primes and only 1485 possible sophie germain primes left over.
