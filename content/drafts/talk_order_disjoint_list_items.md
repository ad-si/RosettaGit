+++
title = "Talk:Order disjoint list items"
description = ""
date = 2014-10-13T11:45:17Z
aliases = []
[extra]
id = 17600
[taxonomies]
categories = []
tags = []
+++

==Inspiration==
I thought ''"what if you ordered part of a list, not by indices as in [[Sort disjoint sublist]], but by giving a list of items ..."'' --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:17, 4 May 2014 (UTC)

== Perl 6 entry ==

I don't have the 2014 version of Rakudo, but can someone who does test how the Perl 6 code reacts to the input <tt>M = <X X Y></tt> and <tt>N = <Y></tt>?

Edit: and <tt>M = <A X></tt> and <tt>N = <Y A></tt> for that matter. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 02:20, 13 May 2014 (UTC)

:I've added the requested cases; you'll note that the solution to A X ==> Y A assumes (reasonably, I think) that the N list elements are a subset of the M elements, as the task postulates without commentary on violations.  The reason I think this assumption is okay is that it scales well to arbitrarily long M lists (indeed, Perl 6 could apply this function lazily to an infinitely long M list, though of course it can't work with an infinitely long N list due to the necessity of testing set/bag membership).  We tend to avoid solutions that require an unnecessary prescan of a list for this reason, and it would take such an "unnecessary" prescan of M to enforce set membership in the other direction.  In any case, whether or not your language supports infinite lists, it's extra work, and prevents the function from being used to substitute a list of words that weren't in the original, which seem like a kind of arbitrary restriction from a practical point of view.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 04:40, 13 May 2014 (UTC)

:: I agree with you, and I somehow missed the "N items chosen from M" part completely.  I don't much care about the second test case, it's the first one that's important, but then I wrote it wrong: it's M = <X X Y> and N = <X>.  Basically your test should read <tt>bag{$_}-- > 0</tt> instead of just a non-zero test, if Perl 6 hash works similarly to 5. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 05:04, 13 May 2014 (UTC)

::: It still works as expected; we don't need to test for > 0 because bags are smart about going lower than 0, since you can't have a negative number of marbles in your bag. (The advantages of having a type system built in...) In fact, the hashes behind sets and bags automatically delete entries that go to False or 0.  So the key lookup just returns an undefined value rather than 0, and the attempt to decrement that is likewise undefined, hence false in the boolean context.  (Was throwing a bogus exception earlier, which we fixed earlier today, and that's why it requires a newer version.  The first version I checked in would probably have run with an older rakudo, since it did the membership test separately from the decrement to get around the bug.  Well, maybe it wouldn't, since we renamed BagHash a couple of months ago too.) Anyway, if sets and bags were just hashes in disguise with no additional behaviors, we probably wouldn't have bothered adding them.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 05:28, 13 May 2014 (UTC)

I like the algorithm used, but I hate the introduction of a value not in the original list. I am trying to think of some way of allowing both but with the difference highlighted.

How about a "Part 2" something along the lines of:

: '''Action when an item of N is not a member of M.''' 
:Show what happens when  <tt>M = <A X></tt> and <tt>N = <Y A></tt>

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:22, 13 May 2014 (UTC)


There seems to be something wrong with the Perl output, probably a copy and paste error. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 11:22, 4 June 2014 (UTC)

== Task clarification: correct behaviour with duplicates in M and/or N? ==

Hi, the Ruby and Scala implementations have been annotated as wrong by [[User:Bart|Bart]] but I currently believe my Scala implementation is correct according to my own interpretation of the task statement, but it’s not entirely clear. Here’s a comparison of Scala with Python and Ruby. I’ve bolded one significant difference. Note, the Python and Ruby versions fail to complete these tests:

'''Scala output:'''
 Data M: cat cat mat              Order N: mat cat   -> Result M': mat cat mat
 Data M: cat cat mat              Order N: '''cat mat'''   -> Result M': '''cat mat''' mat
 Data M: cat mat                  Order N: mat mat   -> Result M': cat mat
 Data M: cat mat                  Order N: cat mat   -> Result M': cat mat
 Data M: cat mat                  Order N: cat mat cat -> Result M': cat mat
 Data M: cat mat                  Order N: mat cat cat -> Result M': mat cat
 Data M: cat mat                  Order N: cat cat mat -> Result M': cat cat

'''Python output:'''
 Data M: 'cat cat mat'            Order N: 'mat cat' -> M' 'mat cat cat'
 Data M: 'cat cat mat'            Order N: ''''cat mat'''' -> M' ''''cat cat''' mat'
 Data M: 'cat mat'                Order N: 'mat cat' -> M' 'mat cat'
 Data M: 'cat mat'                Order N: 'cat mat' -> M' 'cat mat'
 Data M: 'cat mat'                Order N: 'cat mat cat' Traceback (most recent call last):
   File "<stdin>", line 42, in <module>
   File "<stdin>", line 11, in order_disjoint_list_items
 ValueError: 'cat' is not in list

'''Ruby output:'''
 Data M: cat cat mat              Order N: mat cat   -> M' mat cat mat
 Data M: cat cat mat              Order N: '''cat mat'''   -> M' '''cat mat''' mat
 -:21:in `[]=': no implicit conversion from nil to integer (TypeError)
         from -:21
         from -:21:in `reverse_each'
         from -:21
         from -:23:in `each_slice'
         from -:17:in `each'
         from -:17:in `each_slice'
         from -:17

If the Scala implementation is incorrect, can the task please be clarified to avoid this misinterpretation. Alternatively, if all the other impementations are wrong, can the task please be clarified to avoid this misinterpretation :) --[[User:Jnd|Jnd]] ([[User talk:Jnd|talk]]) 10:20, 13 October 2014 (UTC)

:Hi Jnd. We have:
::''"Given M as a list of items and another list N of items chosen from M, create M' as a list with the first occurrences of items from N sorted to be in one of the set of indices of their original occurrence in M but in the order given by their order in N."''

:For:
:: <code>Data M: cat cat mat   Order N: cat mat</code>
:The second cat in M is never indexed by an item in N. 

:There is only one cat in N which matches the first cat in M. mat matches the last item in M so only the first and last items in M ''could'' be rearranged by their order in N. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:08, 13 October 2014 (UTC)

::Thanks Paddy3118, I still can’t get my head around the language, but you’re saying that items in N are taken from M without replacement, then the corresponding positions in M' are taken by successive items from N? --[[User:Jnd|Jnd]] ([[User talk:Jnd|talk]]) 11:45, 13 October 2014 (UTC)
