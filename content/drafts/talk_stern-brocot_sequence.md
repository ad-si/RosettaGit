+++
title = "Talk:Stern-Brocot sequence"
description = ""
date = 2018-10-10T13:53:23Z
aliases = []
[extra]
id = 18360
[taxonomies]
categories = []
tags = []
+++

==Finding first occurrences in Python: procedural version==
Thanks ‎Ledrug for pointing out a much better way to find first occurrence in the procedural Python example. Once you pointed it out, it became clear why your method worked.

I agree with you keeping it as a comment as well, as it does (at least for me), take another step to see that it works and now we have both methods. (either one of which could be commented). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:34, 8 December 2014 (UTC)

==Algorithm used in the C example is different==
The task states a method of calculation that is not used in the current C example. I suggest another example closer to the given algorithm from the task description be used then this C example can be given as an alternative if the algorithm is explained. (I am thinking of starting another task that uses the tree construction and a comparison between the two, but time has not allowed ...). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:47, 8 December 2014 (UTC)
: There's no real difference here: if you look at the n-th element, you obtain the 2n-1 and 2n-th elements, so you can always find each value backwards.  The C program doesn't have a problem printing the first members in sequence, so what's wrong with that? --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 22:22, 8 December 2014 (UTC)
::Hi Ledrug. It was just my perception when doing my background reading - there was the array shuffling of the video, the binary tree, and the function from OEIS... --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 03:36, 9 December 2014 (UTC)

==More functional Python entry==
In such entry the "occurs" variable is not defined.

> (its sb variable stores less compared to the procedural version aboveby popping the last element every time around the while loop.

In CPython lists are implemented as arrays dynamic on the right. I don't remember how exactly they (and the append/pop(0) methods) are implemented, but it's not easy to give to such data structure those operations efficient in both space and time. A collections.deque could be better. --[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])
::Sounds right. I will update it when I truly wake. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 03:28, 9 December 2014 (UTC)


### deque over list?

Well deque's are better for pushing and popping whilst Python lists are better for general indexing (deques loose the ability to be sliced, for example). After Bearophiles comment I decided on doing some timings:


```python
from itertools import takewhile, tee, islice

from collections import deque

from fractions import gcd

def stern_brocot1():
    ...:     sb = [1, 1]
    ...:     while True:
    ...:         sb += [sum(sb[:2]), sb[1]]
    ...:         yield sb.pop(0)
    ...:         

def stern_brocot2():
    ...:     sb = deque([1, 1])
    ...:     while True:
    ...:         sb += [sb[0] + sb[1], sb[1]]
    ...:         yield sb.popleft()
    ...:         

%time [1 + sum(1 for i in takewhile(lambda x: x != occur, stern_brocot1())) for occur in (list(range(1, 11)) + [2500])]
Wall time: 11.7 s
Out[30]: [1, 3, 5, 9, 11, 33, 19, 21, 35, 39, 152931]

%time [1 + sum(1 for i in takewhile(lambda x: x != occur, stern_brocot1())) for occur in (list(range(1, 11)) + [2500])]
Wall time: 11.7 s
Out[31]: [1, 3, 5, 9, 11, 33, 19, 21, 35, 39, 152931]

%time [1 + sum(1 for i in takewhile(lambda x: x != occur, stern_brocot1())) for occur in (list(range(1, 11)) + [2500])]
Wall time: 11.8 s
Out[32]: [1, 3, 5, 9, 11, 33, 19, 21, 35, 39, 152931]



%time [1 + sum(1 for i in takewhile(lambda x: x != occur, stern_brocot2())) for occur in (list(range(1, 11)) + [2500])]
Wall time: 215 ms
Out[37]: [1, 3, 5, 9, 11, 33, 19, 21, 35, 39, 152931]

%time [1 + sum(1 for i in takewhile(lambda x: x != occur, stern_brocot2())) for occur in (list(range(1, 11)) + [2500])]
Wall time: 225 ms
Out[38]: [1, 3, 5, 9, 11, 33, 19, 21, 35, 39, 152931]

%time [1 + sum(1 for i in takewhile(lambda x: x != occur, stern_brocot2())) for occur in (list(range(1, 11)) + [2500])]
Wall time: 223 ms
Out[39]: [1, 3, 5, 9, 11, 33, 19, 21, 35, 39, 152931]
```


The deque is faster, (and the margin increases for later members of the series).

That use of sum() over a slice of the list in the first version does turn out to be slow, but it does ''not'' tip the scales in favour of using a list:

```python
def stern_brocot3():
    ...:     sb = [1, 1]
    ...:     while True:
    ...:         sb += [sb[0] + sb[1], sb[1]]
    ...:         yield sb.pop(0)
    ...:         

%time [1 + sum(1 for i in takewhile(lambda x: x != occur, stern_brocot3())) for occur in (list(range(1, 11)) + [2500])]
Wall time: 7.83 s
Out[43]: [1, 3, 5, 9, 11, 33, 19, 21, 35, 39, 152931]

%time [1 + sum(1 for i in takewhile(lambda x: x != occur, stern_brocot3())) for occur in (list(range(1, 11)) + [2500])]
Wall time: 7.94 s
Out[44]: [1, 3, 5, 9, 11, 33, 19, 21, 35, 39, 152931]

%time [1 + sum(1 for i in takewhile(lambda x: x != occur, stern_brocot3())) for occur in (list(range(1, 11)) + [2500])]
Wall time: 8.12 s
Out[45]: [1, 3, 5, 9, 11, 33, 19, 21, 35, 39, 152931]
```

: If you worry about speed, simply don't <code>pop()</code> the cache.  You have to keep at least half of the array anyway:

```python
def stern_brocot0():
    sb = [1, 1]
    for i in count():
        sb += [sb[i] + sb[i+1], sb[i+1]]
        yield(sb[i])
```

: Or use a <code>tee</code> to do caching for you, counting on Python implementation being good:

```python
def stern_brocot4():
	def gen():
		a = next(tail)
		for x in tail:
			yield x + a
			yield x
			a = x

	tail, out = tee(chain([1, 1], gen()))
	for x in out: yield x
```

: Finding elements is better done by <code>len()</code> than by <code>sum()</code>, probably a small gain:

```python
print [1 + len(list(takewhile(lambda x: x != occur, stern_brocot2()))) for occur in range(1, 1000)]
```

: But it's better to skip the unnecessary lists altogether:

```python
def find(x, seq):
	for i,v in enumerate(seq):
		if v == x: return i + 1
# use it so:
# print [find(x, stern_brocot1()) for x in range(1, 1000)]
```

: In my tests, for larger numbers, using <code>find</code> along with <code>stern_brocot4()</code> is noticeably faster than other combinations. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 19:39, 9 December 2014 (UTC)


==Python using "generic and reusable functional abstractions"==
The answers style seems to be for some ulterior motive as Python functions such as `all` are not used and the Python type annotations are not given. It reads as if the author has written another languages functions in Python then converted another languages solution using it rather than using a more Pythonic solution from the standard Python libraries and built-ins; (and type annotations).

I wonder where the style originates? (Or if a comment on its derivation needs to be attached). [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 13:30, 10 October 2018 (UTC)
: It does use use '''all''', but nests it inside a more general version which takes a predicate as its first argument. The motive is rapid code composition and a high level of code reuse. Nothing 'ulterior' that I am aware of :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:33, 10 October 2018 (UTC)
: More generally, on the multi-paradigm character of Python, and the benefits of functional composition, I find this is a useful reference: https://docs.python.org/3.7/howto/functional.html [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:43, 10 October 2018 (UTC)
: On currying in Python, if that's what feels unheimlich to you, see, for example, https://www.python-course.eu/currying_in_python.php [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:52, 10 October 2018 (UTC)
