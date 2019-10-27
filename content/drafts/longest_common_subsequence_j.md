+++
title = "Longest common subsequence/J"
description = ""
date = 2013-02-01T22:14:53Z
aliases = []
[extra]
id = 12883
[taxonomies]
categories = []
tags = []
+++

Here's an explanation of how this J implementation works:


```J
mergeSq=: ;@}:  ~.@, {.@;@{. , &.> 3 {:: 4&{.
common=: 2 2 <@mergeSq@,;.3^:_ [: (<@#&.> i.@$) =/
lcs=: [ {~ 0 {"1 ,&$ #: 0 ({:: (#~ [: (= >./) #@>)) 0 ({:: ,) common
```


for the example string pair: 'thisisatest' and 'testing123testing'

First, note that any longest common subsequence implementation can ignore characters which are not common between the two strings.  So, for compactness, let's reduce our example to: 'tisistest' and 'testitesti'

We can inspect our two strings for commonalities by comparing all possible combinations of characters:


```J
   'tisistest' =/ 'testitesti'
1 0 0 1 0 1 0 0 1 0
0 0 0 0 1 0 0 0 0 1
0 0 1 0 0 0 0 1 0 0
0 0 0 0 1 0 0 0 0 1
0 0 1 0 0 0 0 1 0 0
1 0 0 1 0 1 0 0 1 0
0 1 0 0 0 0 1 0 0 0
0 0 1 0 0 0 0 1 0 0
1 0 0 1 0 1 0 0 1 0
```


Here, adjacent characters are represented by 1s which are arranged diagonally from each other.  And we can see in the lower right corner a sequence of 1s which corresponds to the word 'test'.  But we also need to recognize characters which not adjacent in one or both words, which means we need to string together characters and sequences when the diagonal relationship is "stretched apart".  And, furthermore, until we have considered all the possibilities, we do not have any definitive answers as to which of the possibilities is best.

And, in fact, although the title of this task is "Longest" common subsequence we should expect that there can be several "longest" answers.  In this case, we have only one, but if we shorten the left string to be 'tistest' we would have two candidates for "longest possible":


```J
tstest
titest
```


So... how can we do this?

In the approach, here, we translate the board representing all possibilities into a container of lists, with "longest known sequence" being represented for each location.  Before we have the locations start to compare notes, it looks like this:



```J
   (<@#&.> i.@$) 'tisistest' =/ 'testitesti'
+----+----+----+----+----+----+----+----+----+----+
|+-+ |++  |++  |+-+ |++  |+-+ |++  |++  |+-+ |++  |
||0| |||  |||  ||3| |||  ||5| |||  |||  ||8| |||  |
|+-+ |++  |++  |+-+ |++  |+-+ |++  |++  |+-+ |++  |
+----+----+----+----+----+----+----+----+----+----+
|++  |++  |++  |++  |+--+|++  |++  |++  |++  |+--+|
|||  |||  |||  |||  ||14||||  |||  |||  |||  ||19||
|++  |++  |++  |++  |+--+|++  |++  |++  |++  |+--+|
+----+----+----+----+----+----+----+----+----+----+
|++  |++  |+--+|++  |++  |++  |++  |+--+|++  |++  |
|||  |||  ||22||||  |||  |||  |||  ||27||||  |||  |
|++  |++  |+--+|++  |++  |++  |++  |+--+|++  |++  |
+----+----+----+----+----+----+----+----+----+----+
|++  |++  |++  |++  |+--+|++  |++  |++  |++  |+--+|
|||  |||  |||  |||  ||34||||  |||  |||  |||  ||39||
|++  |++  |++  |++  |+--+|++  |++  |++  |++  |+--+|
+----+----+----+----+----+----+----+----+----+----+
|++  |++  |+--+|++  |++  |++  |++  |+--+|++  |++  |
|||  |||  ||42||||  |||  |||  |||  ||47||||  |||  |
|++  |++  |+--+|++  |++  |++  |++  |+--+|++  |++  |
+----+----+----+----+----+----+----+----+----+----+
|+--+|++  |++  |+--+|++  |+--+|++  |++  |+--+|++  |
||50||||  |||  ||53||||  ||55||||  |||  ||58||||  |
|+--+|++  |++  |+--+|++  |+--+|++  |++  |+--+|++  |
+----+----+----+----+----+----+----+----+----+----+
|++  |+--+|++  |++  |++  |++  |+--+|++  |++  |++  |
|||  ||61||||  |||  |||  |||  ||66||||  |||  |||  |
|++  |+--+|++  |++  |++  |++  |+--+|++  |++  |++  |
+----+----+----+----+----+----+----+----+----+----+
|++  |++  |+--+|++  |++  |++  |++  |+--+|++  |++  |
|||  |||  ||72||||  |||  |||  |||  ||77||||  |||  |
|++  |++  |+--+|++  |++  |++  |++  |+--+|++  |++  |
+----+----+----+----+----+----+----+----+----+----+
|+--+|++  |++  |+--+|++  |+--+|++  |++  |+--+|++  |
||80||||  |||  ||83||||  ||85||||  |||  ||88||||  |
|+--+|++  |++  |+--+|++  |+--+|++  |++  |+--+|++  |
+----+----+----+----+----+----+----+----+----+----+
```


In other words: we assign a number to each location and initially either have an empty list for our best match (for locations which do not correspond to matching characters) or a single list of a single number (for locations which correspond to a character match).

Next, we can consider 2 by 2 squares:  For each such grouping we can merge include verbatim, in the upper left square all paths from the upper left, upper right and lower left squares.  We should merge in the lower right square's paths but here we also need to append whatever the initial list was, for this square (those are the values we see above).  If we discard duplicates, we can just keep doing this until we don't have anything new to include.

Here's the first step in that process for our example:


```J
   2 2 <@mergeSq@,;.3 (<@#&.> i.@$) 'tisistest' =/ 'testitesti'
+-----------+-----------+-----------+---------+-------+-----------+-----------+-----------+---------+----+
|+-++       |++         |++-+       |+-++----+|++-+--+|+-++       |++         |++-+       |+-++----+|++  |
||0||       |||         |||3|       ||3||3 14||||5|14|||5||       |||         |||8|       ||8||8 19||||  |
|+-++       |++         |++-+       |+-++----+|++-+--+|+-++       |++         |++-+       |+-++----+|++  |
+-----------+-----------+-----------+---------+-------+-----------+-----------+-----------+---------+----+
|++         |++--+      |++--+      |++--+    |+--++  |++         |++--+      |++--+      |++--+    |+--+|
|||         |||22|      |||22|      |||14|    ||14||  |||         |||27|      |||27|      |||19|    ||19||
|++         |++--+      |++--+      |++--+    |+--++  |++         |++--+      |++--+      |++--+    |+--+|
+-----------+-----------+-----------+---------+-------+-----------+-----------+-----------+---------+----+
|++         |++--+      |+--++      |++--+    |++--+  |++         |++--+      |+--++      |++--+    |++  |
|||         |||22|      ||22||      |||34|    |||34|  |||         |||27|      ||27||      |||39|    |||  |
|++         |++--+      |+--++      |++--+    |++--+  |++         |++--+      |+--++      |++--+    |++  |
+-----------+-----------+-----------+---------+-------+-----------+-----------+-----------+---------+----+
|++         |++--+      |++--+      |++--+    |+--++  |++         |++--+      |++--+      |++--+    |+--+|
|||         |||42|      |||42|      |||34|    ||34||  |||         |||47|      |||47|      |||39|    ||39||
|++         |++--+      |++--+      |++--+    |+--++  |++         |++--+      |++--+      |++--+    |+--+|
+-----------+-----------+-----------+---------+-------+-----------+-----------+-----------+---------+----+
|++--+      |++--+      |+--++-----+|++--+    |++--+  |++--+      |++--+      |+--++-----+|++--+    |++  |
|||50|      |||42|      ||42||42 53||||53|    |||55|  |||55|      |||47|      ||47||47 58||||58|    |||  |
|++--+      |++--+      |+--++-----+|++--+    |++--+  |++--+      |++--+      |+--++-----+|++--+    |++  |
+-----------+-----------+-----------+---------+-------+-----------+-----------+-----------+---------+----+
|+--++-----+|++--+      |++--+      |+--++    |++--+  |+--++-----+|++--+      |++--+      |+--++    |++  |
||50||50 61||||61|      |||53|      ||53||    |||55|  ||55||55 66||||66|      |||58|      ||58||    |||  |
|+--++-----+|++--+      |++--+      |+--++    |++--+  |+--++-----+|++--+      |++--+      |+--++    |++  |
+-----------+-----------+-----------+---------+-------+-----------+-----------+-----------+---------+----+
|++--+      |+--++-----+|++--+      |++       |++     |++--+      |+--++-----+|++--+      |++       |++  |
|||61|      ||61||61 72||||72|      |||       |||     |||66|      ||66||66 77||||77|      |||       |||  |
|++--+      |+--++-----+|++--+      |++       |++     |++--+      |+--++-----+|++--+      |++       |++  |
+-----------+-----------+-----------+---------+-------+-----------+-----------+-----------+---------+----+
|++--+      |++--+      |+--++-----+|++--+    |++--+  |++--+      |++--+      |+--++-----+|++--+    |++  |
|||80|      |||72|      ||72||72 83||||83|    |||85|  |||85|      |||77|      ||77||77 88||||88|    |||  |
|++--+      |++--+      |+--++-----+|++--+    |++--+  |++--+      |++--+      |+--++-----+|++--+    |++  |
+-----------+-----------+-----------+---------+-------+-----------+-----------+-----------+---------+----+
|+--+       |++         |++         |+--+     |++     |+--+       |++         |++         |+--+     |    |
||80|       |||         |||         ||83|     |||     ||85|       |||         |||         ||88|     |    |
|+--+       |++         |++         |+--+     |++     |+--+       |++         |++         |+--+     |    |
+-----------+-----------+-----------+---------+-------+-----------+-----------+-----------+---------+----+
```


Notice that the first list at each location (structurally) is the first list at each location (temporally) - so that's a convenient invariant.  Note also that for the bottom edge and right edge of the board we are working as if there are empty squares to complete those shards.

Anyways, once we've finished this process, we can extract the list of lists from the upper left hand corner of the board:


```J
   0 {::, 2 2 <@mergeSq@,;.3^:_ (<@#&.> i.@$) 'tisistest' =/ 'testitesti'
+-++----+-+--+----+--+--+--+----+----+----+-------+-+-----+--+-----+--+-------+----+-------+-------+----+-----+-----+--+-----+-----+--+----+-------+----+----------+----------+----+--+----+-------+-------+-----+-----+--+--------+--------+-----+-----+-------...
|0||0 22|3|22|3 14|14|34|42|0 14|0 34|0 42|0 22 34|5|22 34|50|42 53|53|0 42 53|0 53|0 22 53|0 22 55|3 34|22 53|22 55|55|34 55|50 61|61|0 55|0 34 55|0 61|0 22 34 55|0 22 55 66|5 27|27|3 27|3 14 27|3 14 47|14 27|14 47|47|22 34 55|22 55 66|55 66|34 47|34 55 6...
+-++----+-+--+----+--+--+--+----+----+----+-------+-+-----+--+-----+--+-------+----+-------+-------+----+-----+-----+--+-----+-----+--+----+-------+----+----------+----------+----+--+----+-------+-------+-----+-----+--+--------+--------+-----+-----+-------...
```


This is a long list so I've only shown the beginning part of it.  And, we only want the longest element(s) so let's extract those:

 
```J
  (#~ (= >./)@:(#@>)) 0 {::, 2 2 <@mergeSq@,;.3^:_ (<@#&.> i.@$) 'tisistest' =/ 'testitesti'
+-------------------+
|0 22 34 55 66 77 88|
+-------------------+
```


So if only we knew how to interpret those numbers, we'd be done.  The were just generated sequentially, 0 1 2... so that means that this is like a two digit number where the first digit is base 9 (the number of letters in the left argument) and the second digit is base 10.  (Actually, we don't really care what base the leftmost digit is.)  In other words, the first digit is the character index into the left argument and the second digit is the character index into the right argument.


```J
    'tisistest' ,&$ 'testitesti'
9 10
   9 10 #: 0 {:: (#~ (= >./)@:(#@>)) 0 {::, 2 2 <@mergeSq@,;.3^:_ (<@#&.> i.@$) 'tisistest' =/ 'testitesti'
0 0
2 2
3 4
5 5
6 6
7 7
8 8
```


All that remains, now, is selecting picking one set of indices and one argument and then extracting the characters.
