+++
title = "Talk:Sorting algorithms/Merge sort"
description = ""
date = 2017-12-14T02:34:55Z
aliases = []
[extra]
id = 7804
[taxonomies]
categories = []
tags = []
+++

== Merge Sort can be an iterative sort ==

The merge sort is NOT a recursive sort through necessity (unlike the quick sort) but through implementation. An alternative algorithm for the merge sort looks like this:
  n=1
  Repeat
    Merge lists of size n
    Double n
  Until n >= Size

This is a purely iterative process.
[[User:Starfiend|Starfiend]] 14:20, 24 July 2010 (UTC)

== Java implementation ==
I don't know if you want functions tuned towards efficiency (rather than code readability), but the java implementation accesses a linked list through an index, which a big performance hit in any language. A much faster implementation (50x faster on a list of 100,000 elements) would be to use an iterator:

```java
 int i = 0;
 for (E d : m)
     if (i++ < middle)
         left.add(d);
     else
         right.add(d);
```

--[[User:Zastrowm|Zastrowm]] 17:34, 6 January 2011 (UTC)
:I made the change. It looks just as readable each way I think and since it's such a big performance boost it seems like a good idea. Thanks. --[[User:Mwn3d|Mwn3d]] 19:12, 23 January 2012 (UTC)
: FWIW, we want ''idiomatic'' implementations. Good practice. Stuff that people with some smarts can look at and learn from. A small decrease in readability for a big gain in performance can be reasonable, though in (almost) all languages it is possible to be both fast and nice to read. (I have my doubts about a few, but maybe I just never grokked the æsthetic there…) That said, the above code looks quite readable to me too; iteration-looping is a wonderful thing. –[[User:Dkf|Donal Fellows]] 22:28, 23 January 2012 (UTC)

== Haskell "simple" implementation is Quicksort ==

Except I'm mistaken the given Haskell simple implementation is actually a Quicksort algorithm : there is no concept of "merging" lists, and the original list isn't split but instead filtered between values inferior and values greater than the head of the original list (i.e. something akin to the quicksort pivot) ...


Sorry, I totally missed the point when I wrote that. Reverted. --[[User:Vektorweg|Vektorweg]] ([[User talk:Vektorweg|talk]]) 02:34, 14 December 2017 (UTC)
