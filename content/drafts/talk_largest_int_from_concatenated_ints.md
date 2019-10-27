+++
title = "Talk:Largest int from concatenated ints"
description = ""
date = 2016-04-11T19:00:43Z
aliases = []
[extra]
id = 13246
[taxonomies]
categories = []
tags = []
+++

== Negative integers ==

These programs look like they can't handle negative integers. I don't know how to concatenate two integers if the second one is negative; "4" "-5" => "4-5" is not an integer. Perhaps the task should specify positive integers? Or non-negative integers, if "0" is allowed? --[[User:Kernigh|Kernigh]] ([[User talk:Kernigh|talk]]) 00:38, 4 April 2013 (UTC)

:Yep. I'm sloppy. I will fix this.[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:51, 4 April 2013 (UTC)

:: Why can't zero be included?   That is, allow non-negative integers.   The programs (I think) won't barf if zero would be included in the list.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:28, 7 April 2016 (UTC)

:::Well there is no need to wonder what a number with leading zeros is. Some conventions say ignore leading zeros on an int; others might treat it as signifying that the int should be considered as written in anther base, ... It's peripheral to the task, I guess you could state it as an extension to the task and how these leading zeroes are interpreted by your program, but you should have a first version that fits the task I would think. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:00, 11 April 2016 (UTC)

==Duplicates?==

Paddy3118, please clarify whether the list may or may not include duplicates. --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:02, 4 April 2013 (UTC) PS where have the edit buttons gone?
:Hi Nigel. Duplicates are not excluded. P.S. I assumed that the edit buttons were a (short term) casualty of the MW upgrade. [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:09, 4 April 2013 (UTC)
::The Haskell entry that uses cycle goes in infinite loop if you feed it with numbers like 10 and 1010.-[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])

==On "Python: Compare repeated string method" Second entry for Python 2.6==
Hi Spoon, Is it the case that the first entry does not work on Python 2.6:

:
```python
def maxnum(x):
    maxlen = len(str(max(x)))
    return ''.join(sorted((str(n) for n in x), reverse=True,
                          key=lambda i: i*(maxlen // len(i) + 1)))
```


The second version seems to be quite complex - using Fractions and logs, and although I do not have a version of Python 2.6 to hand, I cannot think of what of the above would break 2.6?

Here's your second version:
:
```python
from fractions import Fraction
from math import log10

def maxnum(x):
    return ''.join(str(n) for n in sorted(x, reverse=True,
                          key=lambda i: Fraction(i, 10**(int(log10(i))+1)-1)))
```


(P.S. thanks for catching my <code>maxnum'''x'''</code> errors)! --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:19, 6 April 2013 (UTC)
: The first one under "repeated string" is infact incorrect.  Try the array <code>[212, 21221]</code> and compare the result with other methods.  You need to repeat each i to a length above <code>maxlen + len(i)</code>. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 08:17, 6 April 2013 (UTC)

::Thanks Ledrug. I'll fix it. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:37, 6 April 2013 (UTC)

: Not at all. I was just expression that the second one requires Python 2.6+, since that's when the <code>fractions</code> module was added. I was not making any statement about the first version. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 02:29, 7 April 2013 (UTC)
