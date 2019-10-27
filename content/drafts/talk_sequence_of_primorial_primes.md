+++
title = "Talk:Sequence of primorial primes"
description = ""
date = 2019-06-10T11:27:27Z
aliases = []
[extra]
id = 19261
[taxonomies]
categories = []
tags = []
+++

== Python library ==
It looks like the isprime function from the library used in the Python example is no longer present in the new version.  <tt>is_probable_prime</tt> is the new function. [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 03:28, 15 June 2015 (UTC)

:Can you give a link? --[[User:Paddy3118|Paddy3118]] 
:Ah, [https://pypi.python.org/pypi/pyprimes/0.2.2a found it]. It seems that isprime becomes is_prime in the later version. The link is to the earlier version so I won't hurry to update to the new library. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 04:51, 15 June 2015 (UTC)

== Ring's Code Produced Error Message ==

I tried to run Ring code suggested in this page, copy-paste to file named primo.ring

 <nowiki># Project : Sequence of primorial primes

max = 9999
primes = []
for n = 1 to max
     if isprime(n) = 1
        add(primes, n)
     ok
next
for n = 1 to len(primes)
     sum = 1
     for m = 1 to n
          sum = sum * primes[m]
     next
     if (isprime(sum+1) or isprime(sum-1)) = 1
        see "" + n + " "
     ok
next
 
func isprime(num)
       if (num <= 1) return 0 ok
       if (num % 2 = 0) and num != 2 return 0 ok
       for i = 3 to floor(num / 2) -1 step 2
            if (num % i = 0) return 0 ok
       next
       return 1</nowiki>

The code produced error message:
 <nowiki>1 2 3 4 5 6
Line 22 Error (R18) : Numeric Overflow!
In function isprime() in file primo.ring
called from line 15  in file primo.ring</nowiki>

I used Ring version 1.10 to run the code.
