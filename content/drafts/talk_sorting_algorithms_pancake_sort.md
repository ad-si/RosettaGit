+++
title = "Talk:Sorting algorithms/Pancake sort"
description = ""
date = 2012-11-15T19:39:17Z
aliases = []
[extra]
id = 6764
[taxonomies]
categories = []
tags = []
+++

== Problem Instructions ==
Isn't the sequence 9 8 7 6 5 4 3 2 1 in descending order?
:Easily fixed with a single flip. -- [[User:Eriksiers|Erik Siers]] 11:17, 26 November 2010 (UTC)

== Burnt Pancake Problem ==

Why not include the "Burnt Pancake Problem" as a extra task? --[[User:Jofur|&lt;KJ&gt;]] 08:53, 5 April 2010 (UTC)
: Mostly because I'm too lazy. ;-) I think it should be a separate task, or a subtask -- something like '''Sorting algorithms/Pancake sort/Burnt Pancake Problem'''. -- [[User:Eriksiers|Eriksiers]] 13:38, 5 April 2010 (UTC)

== Psychic RC'ers ==

Once again, I think to myself, "I should really do '''''X'''''," and someone else goes and does it almost the instant I think of it. This time it's the C implementation here. -- [[User:Eriksiers|Erik Siers]] 20:46, 10 May 2010 (UTC)

== Consecutive integers ==
Some of the solutions seem only to sort a set of ''consecutive'' integers (and indeed generate their test data by shuffling the integers 1..n).  The task description requires the sorting of an 'array of integers' so shouldn't those restricted solutions be considered incorrect? [[User:RichardRussell|RichardRussell]] 18:59, 15 November 2012 (UTC)

:: Yes, I would like to see if the example programs will handle duplicate numbers as well.  Also, some non-positive integers would be nice. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:39, 15 November 2012 (UTC)

== J implementation ==


```J
flip=: C.~ C.@i.@-
unsorted=: #~ 1 , [: >./\. 2 >/\ ]
FlDown=: flip 1 + (i. >./)@unsorted
FlipUp=: flip [:+/<./\@(<: {.)

pancake=: FlipUp@FlDown^:_
```


<code>flip</code> reverses the first N items in a list


```J
   2 3 4 5 6 7 flip 3
4 3 2 5 6 7

```


<code>unsorted</code> finds the largest prefix where a number is less than a number to its left.


```J
   unsorted 1 2 3 2 1 2 3
1 2 3 2 1
```


<code>FlDown</code> finds the largest element in that unsorted prefix and uses flip to bring it to the front of the original list.


```J
   FlDown 2 5 3 6 4 7 1 0 8
7 4 6 3 5 2 1 0 8
```


<code>FlipUp</code> flips the first element of the list as far right as possible


```J
   FlipUp 7 4 6 3 5 2 1 0 8
0 1 2 5 3 6 4 7 8
```


<code>pancake</code> repeatedly uses FlipDown and then FlipUp until the result stops changing.

== Java algorithm ==

Since we want to minimise the number of flips, I did a little cost analysis to see where we can win.

The main idea is that instead of always sorting in order, we can sort in reverse order and do a flip to put back in correct order. Doing this costs one more flip than sorting in order.

Here are some cost values to place the nth number, expressed in flips:

<math>x_0 = maximum_{order} \rightarrow 1 flip</math>, same order

<math>x_{i, 0 < i < n-1} = maximum_{order} \rightarrow 2 flips</math>, same order

<math>x_{n-1} = maximum_{order} \rightarrow 0 flip</math>, same order

<math>x_{n-1} = maximum_{reverse\, order} \rightarrow 0+1 flip</math>, reverse order

<math>x_0 = maximum_{reverse\, order} \rightarrow 1+1 flips</math>, reverse order

A good algorithm would analyse the cost for the few next steps of computation but the algorithm introduced here takes only one step of computation into account. A simple extension would be to note how many numbers will be sorted (cost 0) after next step.
