+++
title = "Talk:Proper divisors"
description = ""
date = 2014-12-23T15:38:19Z
aliases = []
[extra]
id = 18391
[taxonomies]
categories = []
tags = []
+++

== Dupe? ==

The explanation on the J implementation here makes a good point: is this task just a trivial change of [[Factors of an integer]]? --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 17:09, 16 December 2014 (UTC)
:Yep, it is allied, but deficient, perfect, abundant number classifications as well as Amicable pairs are based on them. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:34, 16 December 2014 (UTC)
::It seems like it'd be easier to just use the "proper divisors" definition in other tasks where appropriate rather than having a separate task with essentially the same code (except for maybe esoteric languages). Especially since any definition with "factors of an integer" could be made using "proper divisors" with one or two extra additions or subtractions. It just doesn't seem worth it. It looks to me like having a task to "print the number 3" and another task to "print the number before 4". --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 19:11, 16 December 2014 (UTC)
::::From my point of view 'factors' is simpler than 'Proper divisors'. Specifically: 'Proper divisors' are the factors of a number except the number itself except if the number itself is 1. I don't have an opinion on the redundancy, but if people decide that the redundancy is bad, I'd hate to sacrifice the simple concept of 'factors' (all of them, for all cases, instead of just a special case for 1). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:48, 16 December 2014 (UTC)

:::OK. I understand your point. I have just added the third of the triplet of tasks [[Abundant, deficient and perfect number classifications]], they do form a tight triplet, but I am probably pushing it cos I started it. Could it stay guys? (I am definitely not impartial - would be nice to have one champion though in what I hope will be a debate) --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:26, 16 December 2014 (UTC)
::::Yes I see those, but like I said, those could easily be made using the sum of the old factors task definition and "2n" on the right side of the formulas. Seems like "2 pi" and "tau" to me, but let's see if anyone else has an opinion on whether this task should stay. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 19:37, 16 December 2014 (UTC)
::::I like the classification task, but this one seems like a very small change vs. divisors ([[Factors of an integer]]).  If we have this, then why not restricted divisors (remove 1 and n) as well?  It seems like the obvious implementation here is "get divisors using the other task, then remove n" which isn't very interesting (in my opinion). [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 14:33, 17 December 2014 (UTC)
:::::Actually, it's "get factors using the other task then remove n unless n is 1", which is... at least as interesting as fizzbuzz? But I think I see what you are getting at -- I would not want to see too many minor-variation tasks. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:30, 17 December 2014 (UTC)
::::::The "unless n is 1" has been corrected -- 1 is not an exception.  Some sources include the negatives of the proper divisors as well, leading some to the definition including "...is a positive divisor...".  The input 0 is another oddball case -- Wolfram/Alpha and Pari/GP don't agree on its divisors, for example.  [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 18:43, 17 December 2014 (UTC)
::::::: Should probably use "task has changed" warning for older solutions, when that's what happened. Not always a big deal, but sometimes this can help someone editing (or even just reading) the code understand what's going on. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:52, 17 December 2014 (UTC)

Below I think we show that there is a difference between finding factors for a single number and for a lot of numbers. While 20,000 isn't that many, I think if we mandate that this tasks starts from an efficient algorithm for factoring many integers then it continues from where Factors of an integer finished and adds to it rather than failing to use the lessons already learnt there--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:04, 20 December 2014 (UTC)

== Definition ==

I don't believe the definition used is correct.  In particular "always includes 1" doesn't follow from the definition given at the linked site, or [http://mathworld.wolfram.com/ProperDivisor.html MathWorld] or [http://oeis.org/A032741 OEIS].

A simple definition from Mathworld:  "A positive proper divisor is a positive divisor of a number n, excluding n itself." [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 21:09, 16 December 2014 (UTC)
: From which we see that 1 will always divide an integer without remainder. I make a point about mentioning 1 as a reference stated that sometimes the same term "proper divisors" is used when 1 is excluded. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:25, 16 December 2014 (UTC)
:: I guess my point was about the input 1.  The proper divisors of 1 should be the empty set, while all the current implementations are using the "always includes 1" to mean it should be {1}.  Another interesting case is the input 0, but I think we can ignore that. [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 21:09, 16 December 2014 (UTC)
::: Well... on the one hand, the definition of of what proper divisors are for 1 has changed during this discussion, which makes talking about that a bit odd. On the other hand, the list of factors of zero is probably infinite: 0 = 0*1*2*3*4... Or, put differently: the remainder of dividing zero by 100 is zero. So that can't be adequately represented on a computer, so we probably always treat it as an error case (either that or by never returning from a request for that list of divisors). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:05, 18 December 2014 (UTC)
:::: The task has been changed, so the point of this topic is now settled (for me at least).  Re 0, I see the task says "of a positive integer N" so sidesteps the 0 input entirely.  Even if we did care, it would for this task be "whatever you've justified divisors(0) returning, without 0 if it was present."  That covers divisors returning {}, {0}, {0,1}, error, a lazy list, etc.  As an aside, I see sometime between Pari 2.5.3 and 2.6.2 they changed from returning {0,1} to giving a domain error.  Sage returns a value error.  Wolfram/Alpha indicates "(all non-zero integers are divisors of 0)" [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 01:16, 18 December 2014 (UTC)
== The two Python Solutions ==
Hi Paddy3118

I believe you when you say the second version is faster than the first, but note (n + 1) // 2 + 1 can be replaced by floor(sqrt(n)) in the first version. 141 rather than 10,000 in the case of 20,000. This would be a fairer comparison!--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:32, 19 December 2014 (UTC)


### Python: comparisons

You may wish to look at the fourth example [[Factors_of_an_integer#Python]] for an efficient method of finding the factors of a large number of contiguous integers--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 15:57, 19 December 2014 (UTC)

Thanks Nigel,I should really turn all factor examples into proper divisor functions then time them for implementing, say, the [[Aliquot sequence classifications]] task which would probably thrash them the most. Hmmm. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:10, 19 December 2014 (UTC)

:I hacked together timings and cut-n-pasted stuff into Ipython's command line...

```python
#... marshall the factorials (ommitted copy of code from tasks) ...


def fact2pd(factorise):
    'Change factoriser to proper divisor function'
    def proper_divs(n):
        if n == 1: 
            return set()
        if n == 0:
            return {1}
        f = set(factorise(n))
        try:
            f.remove(n)
        except KeyError:
            pass
        return f
    proper_divs.__doc__ = 'From %s: %s' % (factorise.__name__, factorise.__doc__)
    return proper_divs

pd = [fact2pd(fact) for fact in (factors1, factors2, factors3, factors4, factors5)] + [proper_divs22, proper_divs21]

nmax = 50000
ans = [proper_divs21(n) for n in range(1, nmax)]
_divs.cache_clear()

for proper_divs in pd:
    print(proper_divs.__doc__)
    %time print('  OK' if ans == [proper_divs(n) for n in range(1, nmax)] else '  Whoops!')
```



```txt
From factors1: Naive and slow but simplest (check all numbers from 1 to n):
  OK
Wall time: 2min 38s
From factors2: Slightly better (realize that there are no factors between n/2 and n):
  OK
Wall time: 1min 22s
From factors3: Much better (realize that factors come in pairs, the smaller of which is no bigger than sqrt(n)):
  OK
Wall time: 1.44 s
From factors4: More efficient when factoring many numbers:
  OK
Wall time: 1.06 s
From factors5: NEW 2014/12/20: More efficient when factoring many numbers:
  OK
Wall time: 772 ms
A very literal interpretation
  OK
Wall time: 1min 38s
Return the set of proper divisors of n.
  OK
Wall time: 2.69 s
```


:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 00:44, 20 December 2014 (UTC), --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:11, 20 December 2014 (UTC)


:If I time from 500,000 to 550,000 instead of from 1 to 50,000 we can see the effect of larger N:

```python
pd = [fact2pd(fact) for fact in (factors3, factors4, factors5)] + [ proper_divs21]
nrange = range(500000, 550001)
ans = [proper_divs21(n) for n in nrange]
_divs.cache_clear()

for proper_divs in pd:
    print(proper_divs.__doc__)
    %time print('  OK' if ans == [proper_divs(n) for n in nrange] else '  Whoops!')
```



```txt
From factors3: Much better (realize that factors come in pairs, the smaller of which is no bigger than sqrt(n)):
  OK
Wall time: 4.58 s
From factors4: More efficient when factoring many numbers:
  OK
Wall time: 2.17 s
From factors5: NEW 2014/12/20: More efficient when factoring many numbers:
  OK
Wall time: 1.22 s
Return the set of proper divisors of n.
  OK
Wall time: 4.51 s
```

:(I dropped a few obviously slow functions).
:All timings where done on the same lappy doing roughly the same things i.e. not much apart from the run. 
: Python 3.4.2 |Continuum Analytics, Inc.| (default, Oct 22 2014, 11:51:45) [MSC v.1600 64 bit (AMD64) for what it is worth, but it is the broad relative timings that might be of use.
:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:35, 20 December 2014 (UTC)

:: We ought to be careful when trying to decide what's a good way to do things here, because it depends on how the proper divisor routine is going to be used in the end.  If we know we'll factorize a sizable chunk of contiguous integers, sieving is significantly faster due to lower overhead:

```python
from math import sqrt

def factors(bot, top):
        max_p = int(sqrt(top - 1))
        sieve = [True] * (max_p - 1)
        for p in range(2, int(sqrt(max_p)) + 1):
                if not sieve[p - 2]: continue
                for x in range(p*p, max_p + 1, p):
                        sieve[x - 2] = False
        primes = [b[0] for b in enumerate(sieve, 2) if b[1]]

        divs = [() for _ in range(bot, top)]
        for p in primes:
                for x in range(max(p*p, (bot + p - 1)//p*p), top, p):
                        divs[x - bot] += (p,)
        return divs

def prop_divs(bot, top):
        for (v,f) in enumerate(factors(bot, top), bot):
                pd = [1]
                for p in f:
                        inc, s = [],1
                        while not v%p:
                                s,v = s*p, v//p
                                inc += [a*s for a in pd]
                        pd += inc
                if v > 1:
                        pd += [a*v for a in pd]
                yield(pd[:-1])

print(list(enumerate(prop_divs(1, 11), 1)))
print(max([(len(pd), v) for (v,pd) in enumerate(prop_divs(500000, 550001), 500000)]))
```

:: at least until you run out of memory.  But the above code fares horribly if you try to factor a single large number, say <code>12345678901234567</code> (I'm not saying it'd do better when factorizing ''many'' huge numbers, it's just that other methods listed here would then be equally bad, if not worse).  I think we don't need to look too hard for a "best" method, just something that's easy to understand and uses a sound algorithm (that being said, my own code tends to be quite unreadable -- doing the right thing is harder than advocating it).  After all, if running time is the absolute priority, I wouldn't use Python to begin with. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 08:35, 23 December 2014 (UTC)

:::I too don't think that run time alone should dictate what python solution should be preferred for this ''set'' of tasks. If a good looking solution is one of the fastest then it would make an easy choice. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:38, 23 December 2014 (UTC)
