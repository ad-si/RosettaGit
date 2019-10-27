+++
title = "Talk:Heronian triangles"
description = ""
date = 2016-10-28T18:04:06Z
aliases = []
[extra]
id = 18907
[taxonomies]
categories = []
tags = []
+++

The Python part is badly formatted and does not show up in the index, someone who knows wiki-formatting should fix it --[[User:Zorro1024|Zorro1024]] ([[User talk:Zorro1024|talk]]) 14:21, 22 March 2015 (UTC)
:Fixed. The problem was [http://rosettacode.org/mw/index.php?title=Heronian_triangles&oldid=200549 Smoe's R entry] which was not terminated properly (and which should have gone after the python entry rather than before it). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:27, 22 March 2015 (UTC)

::I wonder if a spark of static or noise entered the Python version at some point in its editing history ? On my system the current draft overgenerates triangles, giving  a different output from that shown. (It seems to find 1383 rather than 517 triangles). If not an editing glitch then possibly an artefact of changing Python versions ? I am running Python 2.7.10 on OS X 10.11. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 00:12, 25 October 2015 (UTC)

:::The solution originally just worked for Python 3. I've added the necessary <code>__future__</code> import. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 07:00, 25 October 2015 (UTC)

::::Thanks – that was fast.
::::On the topic of imports, I wonder if it might make good pedagogic (and perhaps engineering) sense to drop the import of ''product'' from ''itertools'', and let the list comprehension do the generation of cartesian products ?
::::The fact that list monads and list comprehensions yield cartesian products unassisted is one of their most interesting (and arguably central) properties, and perhaps we can demonstrate that more clearly by rewriting the first (generating) half of that comprehension as '''h = [(a, b, c) for a in range(1, last) for b in range(a, last) for c in range(b, last)'''
:::: (where ''last'' is maxside+1)
::::'''Advantages''':
::::# The filtering happens earlier. Rather than first generating 8 million potential tuples and only then starting to filter, we immediately begin to filter in the inner ''for'' loop (or inner call to ''concat map'') of the process which is generating the cartesian product, and we never create the full oversized set in the first place.
::::# By defining b and c in terms of a and b, we immediately eliminate the 6646600 out of 8000000 cases which otherwise have to be filtered out by the ''if (a <= b <= c)'' condition, and that condition can now be dropped.
::::# Apart from the probable space improvement, there seems (as it happens) to be a time improvement in the range of 50% (at least on this system with Python 2.7).
::::[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 10:11, 25 October 2015 (UTC)
:::::Well, I personally don't care about the pedagogical value of using a list comprehension unassisted instead of <code>itertools.product</code>. But I agree that generating the full cartesian product isn't necessary. There is also <code>itertools.combinations_with_replacement</code> which also generates the filtered sequence. Using the following timing tests (where <code>heronian</code> contains the code for the task)
:::::
```python
import timeit

setup = "import heronian, itertools; last = 201"
prod = """[(a, b, c) for a,b,c in itertools.product(range(1, last), repeat=3) 
           if a <= b <= c and a + b > c and heronian.gcd3(a, b, c) == 1 and 
           heronian.is_heronian(a, b, c)]"""
list_comp = """[(a, b, c) for a in range(1, last) for b in range(a, last) for c in range(b, last)
                if a + b > c and heronian.gcd3(a, b, c) == 1 and 
                heronian.is_heronian(a, b, c)]"""
comb = """[(a, b, c) for a,b,c in itertools.combinations_with_replacement(range(1, last), 3)
           if a + b > c and heronian.gcd3(a, b, c) == 1 and 
           heronian.is_heronian(a, b, c)]"""

print(timeit.timeit(stmt=prod, setup=setup, number=3))
print(timeit.timeit(stmt=list_comp, setup=setup, number=3))
print(timeit.timeit(stmt=comb, setup=setup, number=3))
```

:::::I get the following results:
:::::
```bash
$ uname -a
Linux arch 4.2.3-1-ARCH #1 SMP PREEMPT Sat Oct 3 18:52:50 CEST 2015 x86_64 GNU/Linux
$ python2 -V
Python 2.7.10
$ python2 timetest.py
9.67713499069
6.35034918785
6.6238899231
$ python3 -V
Python 3.5.0
$ python3 timetest.py
26.007190777992946
22.86392442500801
22.943329881993122
```

:::::IMHO <code>itertools.combinations_with_replacement</code> is on a par with your solution. (And the functions in the <code>itertools</code> module won't waste any space because the return the items of the return sequence successively.)
:::::PS: If you wonder, why running the timing code with Python 3 is so much slower: It looks like <code>fractions.gcd</code> performs really bad on Python 3. Using <code>math.gcd</code> instead (new in Python 3.5) I get the following results:
:::::
```bash
$ head -n 4 heronian.py
from __future__ import division, print_function
from math import sqrt, gcd
from itertools import product

$ python3 timetest.py
7.764596431006794
4.7238479950028704
4.8884705100063
```
--[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 12:39, 26 October 2015 (UTC)

::::: Good work – thanks !
::::: On pedagogy vs optimisation / engineering – I guess there's just some variation in where each of us locates the value of the wiki. I'm personally interested by the two values of insight and value to learners which are articulated in the Rosetta Code landing page formulations – but of course, perspectives are bound to diverge, and in particular it's probably inevitable that the focus of interest for coders will tend to be slightly different from the focus of interest for learners.
::::: Anyway, in this particular case – as you say, the key is probably just to show some intelligible way of not generating the full cartesian product when it isn't actually needed.
::::: [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:30, 26 October 2015 (UTC)


==Formulae hidden to most browsers by under-tested cosmetic edits at 20:13, 9 May 2016 ==

Under-tested cosmetic edits made to the task page at 20:13, 9 May 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left some of the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:47, 22 September 2016 (UTC)

: Visibility was restored on 26 September 2016 [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:04, 28 October 2016 (UTC)
