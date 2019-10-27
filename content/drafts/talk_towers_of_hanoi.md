+++
title = "Talk:Towers of Hanoi"
description = ""
date = 2019-02-03T15:34:01Z
aliases = []
[extra]
id = 2207
[taxonomies]
categories = []
tags = []
+++

== recursion redirect? ==
Why does recursion redirect here?  This isn't the only task that can be solved with recursion.  I'd almost prefer to see a category dedicated to recursion, with pages with recursive examples added to that category. --[[User:Short Circuit|Short Circuit]] 23:43, 7 November 2007 (MST)
:I could try my hand at making it for you and finding articles that fall under the category. I may need a small amount of instruction to get me going though. --[[User:mwn3d|mwn3d]] 15:22, 8 November 2007 (EST)
:Done. --[[User:Mwn3d|Mwn3d]] 17:41, 27 January 2008 (MST)

== Who or what is "Towers of Hanoi"? ==
There seems to be some assumption here that everyone
and their aunt must be intimately familiar with the "Towers of Hanoi"

now I needed to do a Google search to get a clue
as I can find no clue on this Wiki....

["http://www.google.com/search?hl=en&q=%22Towers+of+Hanoi%22&btnG=Google+Search"]

== Verbose explanation for J ==
Here is one of the J implementations:


```j
H =: i.@,&2 ` (({&0 2 1,0 2,{&1 0 2)@$:@<:) @. *
```


This could be rephrased as:


```j
H =: elsePart ` thenPart @. *
elsePart=: i.@,&2
thenPart=: ({&0 2 1,0 2,{&1 0 2)@H@<:
```


First off, note that this rephrasing has introduced two new names and has also replace anonymous recursion (<code>$:</code>) with non-anonymous recursion (<code>H</code>) -- it would be bad to have the recursion happen purely within thenPart without the conditional which was in the original definition of H.

But, wait, why does ''else'' come before ''then''?  (For the same reason that 0 comes before 1:)  The primitive <code>@.</code> uses its right argument to come up with an index which is used to select from the code fragments on the left.

In this case, the right argument to <code>@.</code> is <code>*</code> which returns 0 for zero arguments and 1 for positive arguments.  So the test, in essence is: is the right argument greater than zero?

This means that elsePart only runs with a right argument of zero.  And it looks like this:


```j
   i.@,&2(0)
```


In other words, it returns nothing.  (But it is a carefully crafted nothing: it is a numeric array with two columns and no rows.)

Now let's consider thenPart with an argument of 1:


```j
({&0 2 1,0 2,{&1 0 2)@H@<: 1
```


This is equivalent to:


```j
({&0 2 1,0 2,{&1 0 2)@H 0
```


Which, in turn, is equivalent to


```j
({&0 2 1,0 2,{&1 0 2) i.0 2
```


Which, in turn, is equivalent to:


```j
((i.0 2){0 2 1),0 2,((i.0 2){1 0 2)
```


And { is J's indexing function, but since no actual indices are being used, this is equivalent to:


```j
(i.0 2),0 2,(i.0 2)
```


In other words, we have these two column arrays with no rows and between them we put a row that has the values 0 2.  So the result looks like this:


```j
  H 1
0 2
```


Simple, no?  (Or, it's simple once you get used to the idea of manipulating empty arrays which have structure but no elements.  Until then this whole idea of working with nothing can seem a bit strange.  But, if that bothers you, then maybe you can have some sympathy for how romans must have felt when they first encountered arabic numerals.)

Now, for the <code>H 2</code> case, we know from the above that we are going to be evaluating:


```j
((H 1){0 2 1),0 2,((H 1){1 0 2)
```


And, <code>0 2 { 0 2 1</code> yields <code>0 1</code>  Likewise <code>0 2 { 1 0 2</code> yields <code>1 2</code>.  Thus the result of H 2 is this array:


```j
   H 2
0 1
0 2
1 2
```


In other words, for H n, we find H n-1 and use that result twice:

First, we use it (the previous hanoi solution) keeping our starting point the same, and swapping the use of the other two pegs.  Then (after we have moved our disk from peg 0 to peg 2), we use it again but holding the end point the same and swapping the use of the first two pegs.

(Some people might feel they should object to this remapping of results as inefficient, but this remapping does not make a difference to the [[wp:Big O notation|big O]] efficiency, and in practice should be a [[wiki:PrematureOptimization|minor concern]].)


== A bunch of sample codes are buggy ==
The bug is that they print ndisk, which is the number of remaining disks to be moved, as if it was the index of the disk to be moved.
Can someone correct these bugs?
