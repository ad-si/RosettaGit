+++
title = "Talk:Sequence of primes by trial division"
description = ""
date = 2018-02-17T17:57:57Z
aliases = []
[extra]
id = 17920
[taxonomies]
categories = []
tags = []
+++

For the context to this, see [[Sieve_of_Eratosthenes#Trial_division_sieves_should_be_moved_from_here|Sieve of Eratosthenes talk page]]. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 14:09, 13 September 2014 (UTC)

== Fortran implementation ==

The comments on the fortran implementation currently state: 

:"Likewise, although the last possible factor to try for n up to the integer limit of 32767 is 16381 because the square of the next prime (16411) exceeds 32767, in order for the method to be able to know this, the PRIME array must have space for this surplus prime."

And, indeed, both 16381 and 16411 are prime numbers.

But the first prime whose square exceeds 32767 is 191.

What is really going on here? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:09, 15 April 2015 (UTC)

:Whoops. I suffered a slipped two, forgetting the difference between 16381*2 and 16381**2. I'll revise the blather, this time before midnight. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 10:29, 16 April 2015 (UTC)
