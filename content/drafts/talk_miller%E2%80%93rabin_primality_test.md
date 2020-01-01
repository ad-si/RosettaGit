+++
title = "Talk:Miller–Rabin primality test"
description = ""
date = 2019-08-20T17:26:42Z
aliases = []
[extra]
id = 9078
[taxonomies]
categories = []
tags = []
+++

== C# ==

The code for method RabinMiller.IsPrime(int n, int k) returns incorrect results.  For example, RabinMiller.IsPrime(181, 10) returns false even though 181 is prime.  I believe the incorrect value is returned because the line


```c#
int mod = (int)Math.Pow(a, (double)temp) % n;
```

overflows.

==Erlang ==
The task asks for a function with two arguments. One is an odd integer, the other is a parameter (also integer?). It seems to me that there is no such function in the Erlang module. I could be wrong since the module exports all its functions making it more difficult to be sure. [[User:bengt]]

:I read ''this'' task as just stating that this particular ''algorithm'' is to be used, not necessarily the same variable and function names. (But that's just my guess). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:17, 1 September 2013 (UTC)

::It is not the absence on the task page of a function name or that the only function with the mentioned parameter names (n and k) is not doing the ''algorithm'' that is my problem, although both these things makes it more difficult. My problem is that I can not see any of the exported functions doing the task. Therefore I think that the Erlang solution might not be correct. [[User:bengt]]

== Tcl ==
Is Tcl correct? It has 1 as a prime.

== Testing Composite Probability ==
In addition to finding primes it is interesting to determine how often the algorithm returns composites as prime, see [http://rosettacode.org/wiki/Talk:Carmichael_3_strong_pseudoprimes,_or_Miller_Rabin%27s_nemesis#Analysis here]. Using [http://rosettacode.org/wiki/Miller-Rabin_primality_test#Ruby Ruby's miller_rabin_prime?] as follows:


```ruby

v = (1..times).find_all { |i| miller_rabin_prime?(n,g)}
puts v.length

```


with n=703 (19 * 37) and g=1 for times=10,000 returned 2242 false trues, which compares well with the 2286 which may be expected [http://rosettacode.org/wiki/Talk:Carmichael_3_strong_pseudoprimes,_or_Miller_Rabin%27s_nemesis#Analysis from]. Increasing g to 5 reduced the number of false trues to 7. Increasing g to 10 reduced the number of false trues to 0 even when times was increased to 100000. Increasing times to 1000000 only returned 1 false true. Obviously these results will vary for each trial.--[[User:Nigel Galloway|Nigel Galloway]] 13:47, 30 December 2012 (UTC)

:There is also some mention of this in the Python entry. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:33, 17 January 2014 (UTC)

== python code errors ==

The python code has multiple portions that are incorrect or not clear:

- a is chosen from 2 to n, not 2 to n-2

- modular squaring should be done s-1 times, not s times

- why is it written as 2 functions when it can be simplified?




This has caused errors in the testing of some numbers. '''Do not use this code''' until these errors can be resolved.



== Run Basic and PureBasic problems with output ==

Tested in multiple languages all return 31 as a composite.

To show this go to runbasic.com then click Write Your Own, and enter the source code and run. Then enter 31 as the number to test and any number of witness loops, it will return composite.

: The pseudocode is not broken.  The Run Basic code is broken.  It does not choose the bases correctly, and inverts the x=1 or x=n-1 test. [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 17:17, 11 March 2016 (UTC)

:: Thanks that seems to have fixed Run Basic, I've tried quite a few different numbers and it seems to be correct. I noticed PureBasic was doing almost exactly the same thing and from looking at the pseudocode I thought it was wrong. I wonder how these two Basic languages produced similar errors, maybe someone copied and didn't check the output. [[User:Bearded badger|Bearded badger]] ([[User talk:Bearded badger|talk]])

::: I haven't run the PureBasic code, but it looks like it does the base selection correctly (2 to n-2, albeit the pseudocode is 2 to n-1, so we should test input = 3) and has a continue to skip the test if x=1 or x=n-1.  These were the two issues that weren't right with RunBasic.  The Liberty Basic example has 200 lines of test cruft but the actual loop is broken in that it runs 11 iterations of Miller-Rabin all with the same base (7).  It also looks broken in that it doesn't properly skip the loop if r = n-1.  If we remove the optimization of checking small factors at the beginning, I believe it will fail if given 31 and a base of 3.  IMO it should be written without this optimization and with far less code.  [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 05:41, 12 March 2016 (UTC)

:::: The PureBasic code returns these primes from 4-100: 5, 7, 11, 13, 17, 19, 23, 29, 37, 41, 73, 97. I'm not sure where it is going wrong. EDIT: it appears PureBasic is overflowing when doing the Pow(). PS- sorry I'm new here I hope you don't mind me editing the heading [[User:Bearded badger|Bearded badger]] ([[User talk:Bearded badger|talk]])

::::: Given n = 31, if we select a base of 22 then it does x = (22^15) % 31 and gets -8 as the result (instead of 30 = n-1).  Similar with base 27.  The Pow operation overflows very easily, so the program isn't very useful as written.  It needs a powmod using a ladder (see the C deterministic code or the Go code, for examples). [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 07:22, 12 March 2016 (UTC)
==Pseudocode in task description==
The pseudocode includes the line "for r = 1 .. s − 1". Where is r used?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 17:26, 20 August 2019 (UTC)
