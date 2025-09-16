+++
title = "Talk:Combinations with repetitions"
description = ""
date = 2010-11-19T20:11:00Z
aliases = []
[extra]
id = 8750
[taxonomies]
categories = []
tags = []
+++

Original implementations needed; the linked site does not indicate any license or copyright notice, which leads it to default (at least where Rosetta Code is based out of) to a default of "all rights reserved." The task can likely go to non-draft once it's slightly cleaned up, and original implementations are provided. --[[User:Short Circuit|Michael Mol]] 19:39, 16 November 2010 (UTC)

Suggestion:
It is OK. We can remove the Java solution. The other solutions are original implementation. 
Do I must remove it, or do you do it? [[User:Pelci|Pelci]] 19:18, 18 November 2010 (UTC)
: I pulled it myself. However, I don't mind if others want to pull them first, particularly in cases of clear copyright issues. --[[User:Short Circuit|Michael Mol]] 19:20, 18 November 2010 (UTC)

: So we can put back the task into the tasks... [[User:Pelci|Pelci]] 19:40, 18 November 2010 (UTC)
:: I'd still want to see some cleanup of the task description. Mostly for en-us (or en-anything) grammar and spelling corrections. It might make more sense to wait until we have a few more implementations; as people attempt to implement it, they often find non-obvious problems we need to fix. --[[User:Short Circuit|Michael Mol]] 19:43, 18 November 2010 (UTC)
== Task definition ==

At this stage, it's very hard for me to work out exactly what is to be implemented; the task gives very little guidance over what a “k-combination with repetition” is exactly (it's not the clearest of wikipedia pages that is linked to). At the very least, I'd expect it to use a simple small example (with as few elements as possible) to show exactly what is meant and how things differ from a standard combination-enumerator; it could then ask for the generation of the combinations for a larger input set. –[[User:Dkf|Donal Fellows]] 23:31, 16 November 2010 (UTC)

:Ditto on the lack of clarity.
:What would be the result of:
    n=(1,2,3), k=2; 
    n=(1,1,2,3), k=2
    n=(1,1,1,2,3), k=2
:And how do you compute the result in the general case? --[[User:Paddy3118|Paddy3118]] 00:01, 17 November 2010 (UTC)

::n=(1,2,3), k=1 would have the same result as n=(1,1,2,3), k=1.  Similarly, (1,1,2,3), k=2 would be the same result as (1,1,1,2,3), k=2.  One way of expressing the result would be: calculate all the possibilities and then eliminate the duplicate results.  --[[User:Rdm|Rdm]] 15:18, 17 November 2010 (UTC)

I wrote an example about the task! [[User:Pelci|Pelci]] 19:44, 18 November 2010 (UTC)

:The written example in the task description seems to describe sampling with replacement rather than sampling with repetitions. Which does the task attempt to describe? Given the set n= ('a,a,b,c,d'), k=3; (a,a,a) would be a valid answer if sampling with replacement, however would not be valid if sampling with repetition - which from the wikipedia page I understand to mean that some items may occur more than once in the population to be sampled.--[[User:Tikkanz|Tikkanz]] 21:52, 18 November 2010 (UTC)
:: The original implementation (now deleted for copyright reasons even though the example results were not copyrighted) had:
::: Combination of a repetitions list. list=(1, 2, 2, 3) k=2
:::: [2, 2]
:::: [1, 3]
:::: [2, 3]
:::: [1, 2]
::If it were "sampling with replacement" I imagine that that result would have also included [1,1] and [3,3].  Since it did not, I do not think we can have (a,a,a)  as a result for set n= ('a,a,b,c,d'), k=3.  But perhaps the results from the deleted example should be reposted as a part of the task description?  --[[User:Rdm|Rdm]] 21:58, 18 November 2010 (UTC)

==How about this definition?==
From the [http://docs.python.org/py3k/library/itertools.html#itertools.combinations_with_replacement Python documentation]:

'''itertools.combinations_with_replacement(iterable, r)'''

:Return r length subsequences of elements from the input iterable allowing individual elements to be repeated more than once.

:Combinations are emitted in lexicographic sort order. So, if the input iterable is sorted, the combination tuples will be produced in sorted order.

:Elements are treated as unique based on their position, not on their value. So if the input elements are unique, the generated combinations will also be unique.

:Equivalent to:

:
```python
def combinations_with_replacement(iterable, r):
    # combinations_with_replacement('ABC', 2) --> AA AB AC BB BC CC
    pool = tuple(iterable)
    n = len(pool)
    if not n and r:
        return
    indices = [0] * r
    yield tuple(pool[i] for i in indices)
    while True:
        for i in reversed(range(r)):
            if indices[i] != n - 1:
                break
        else:
            return
        indices[i:] = [indices[i] + 1] * (r - i)
        yield tuple(pool[i] for i in indices)
```


:The code for combinations_with_replacement() can be also expressed as a subsequence of [http://docs.python.org/py3k/library/itertools.html#itertools.product product()] after filtering entries where the elements are not in sorted order (according to their position in the input pool):

:
```python
def combinations_with_replacement(iterable, r):
    pool = tuple(iterable)
    n = len(pool)
    for indices in product(range(n), repeat=r):
        if sorted(indices) == list(indices):
            yield tuple(pool[i] for i in indices)
```


:The number of items returned is <code>(n+r-1)! / r! / (n-1)! when n > 0</code>.


We would need to '''change the page name''' to ''Combinations with replacement'', but I think the above Python does what the [[wp:Combination#Example of counting multicombinations|wp entry]] is trying to describe as:
:
```python
>>>
 from itertools import combinations_with_replacement
>>> len(list(combinations_with_replacement('1234567890', 3)))
220
```


--[[User:Paddy3118|Paddy3118]] 02:37, 19 November 2010 (UTC)

:Maybe keep the name as [http://googlefight.com/index.php?lang=en_GB&word1=%22combinations+with+replacement%22&word2=%22combinations+with+repetition%22 googlefight] prefers it 465 to 91. --[[User:Paddy3118|Paddy3118]] 02:45, 19 November 2010 (UTC)

:I think we have two separate tasks here.  We have the original task (where individual items could be repeated with counts independent of other items) and the new task (where the repetition count on all items is assumed to be at least as large as the number of elements in the desired results).  --[[User:Rdm|Rdm]] 15:58, 19 November 2010 (UTC)

:: And yet I get the n=10, k=3 result of 220 which is mentioned in the Wikipedia article?

:: Looking again at the [http://rosettacode.org/mw/index.php?title=Combinations_with_repetitions&oldid=95620 previous description] of:
:::Write a program which generates the all [[wp:Combination|k-combination with repetitions]] of '''n''' different objects. (Practically numerals!) 

:::An example about the task:
:::You have a hat, and you have '''n''' balls with numbers (1...n). Put the balls into the hat, and cover it. Take '''k'''-times one ball from the hat, but you must put back always immediately the ball after registration of the number into the hat. How many different combination can we have? (Of course the following two combination are not different: [1, 2, 2] and [2, 1, 2]).

::And comparing it to the new doughnut flavoured task of:
::::Q: How many ways can a person choose two doughnuts from a store selling three types of doughnut: iced, jam, and plain?

::::A: 6:  iced and iced; iced and jam; iced and plain; jam and jam; jam and plain; plain and plain.

::I think the task descriptions are equivalent but the old J solution confuses me as it shows fixed repetitions of items, where I read the WP article and the description given as needing only the n types of allowable items - any kind of repetition (or omission), is allowed just so long as only k are chosen at once. Yep, it doesn't help that the wp article is hard to read :-)
:: --[[User:Paddy3118|Paddy3118]] 17:00, 19 November 2010 (UTC)
::: If you go back and look at the original entry, you will see that the original java code worked the same way as the old J example.  But that code was deleted for copyright reasons and when it was deleted, the associated examples were also deleted, which makes talking about that issue a bit confusing.  But, anyways, we have two tasks here: one based on a common interpretation of the wording of the original task description and another based on the original examples.  Without those examples, the original task description does not distinguish between these two cases.  --[[User:Rdm|Rdm]] 19:15, 19 November 2010 (UTC)

::::Hi Rdm. I went to the original blog with the Java code, and I think that it is probably a case of a loosely used term: "Combinations with repetitions" that is not what is mentioned in the wp article.  Wolfram mathworld calls what is mentioned in the wp article [http://mathworld.wolfram.com/Multichoose.html multichoose], but it is not what the Java does - as you have been saying. If someone can come up with some good references for what the Java code is doing then I'd be grateful, Ta. --[[User:Paddy3118|Paddy3118]] 20:11, 19 November 2010 (UTC) 

:: [https://docs.google.com/viewer?url=http://www.nitte.ac.in/userfiles/file/Combinations%2520with%2520Repetitions.pdf&embedded=true&chrome=true Another link] that might be a better explanation? --[[User:Paddy3118|Paddy3118]] 17:07, 19 November 2010 (UTC)
::: The WP article references [https://compprog.wordpress.com/2007/10/17/generating-combinations-1/ this], which seems fairly readable. --[[User:Short Circuit|Michael Mol]] 17:12, 19 November 2010 (UTC)
::::Hi Michael. Unfortunately that article seems to be about plain old combinations. No help with repetitions. --[[User:Paddy3118|Paddy3118]] 17:32, 19 November 2010 (UTC)
::::: Hm. Looking at it, it reminded me of nCr from a business stats class I paid too little attention to, too long ago. Googling for that turns up [[wp:Binomial Coefficient]]. There's a short debate on their relationship in [[wp:Talk:Combination#REDIRECT_to_Binomial_coefficient]], but there's a rather pointed note to [[wp:Multiset#Polynomial_notation]] to demonstrate their relationship. Of course, if I haven't been any help in shedding light on this, then I've probably completely enveloped it in a dense fog... --[[User:Short Circuit|Michael Mol]] 17:43, 19 November 2010 (UTC)
