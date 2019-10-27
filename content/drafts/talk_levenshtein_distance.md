+++
title = "Talk:Levenshtein distance"
description = ""
date = 2015-09-15T13:56:56Z
aliases = []
[extra]
id = 9145
[taxonomies]
categories = []
tags = []
+++

==J Implementation==

This is some documentation and elaboration for:


```j
levD=: i.@-@>:@#@] ,~ >:@i.@-@#@[ ,.~ ~:/
lev=: [: {. {."1@((1&{ ,~ (1 1,{.) <./@:+ }.)@,/\.)@,./@levD
```


''First, we setup up an matrix of costs, with 0 or 1 for unexplored cells (1 being where the character pair corresponding to that cell position has two different characters).  Note that the "cost to reach the empty string" cells go on the bottom and the right, instead of the top and the left, because this works better with J's "reduce" operator.''

''Then we reduce the rows of that matrix using an operation that treats those two rows as columns and reduces the rows of this derived matrix with an operation that gives the (unexplored cell + the minumum of the other cells) followed by (the cell adjacent to the previously unexplored cell.''

In other words, <code>levD</code> does this:


```j
   'kitten' levD 'sitting'
1 1 1 1 1 1 1 6
1 0 1 1 0 1 1 5
1 1 0 0 1 1 1 4
1 1 0 0 1 1 1 3
1 1 1 1 1 1 1 2
1 1 1 1 1 0 1 1
7 6 5 4 3 2 1 0
```


The rightmost column and bottom row indicate the edit distance from those substrings to the empty string.  In other words, 6 is the edit distance from 'kitten' to the empty string, while 3 is the edit distance from 'ten' to the empty string.

The J operator / inserts its verb between each item in an array, so <code>'kitten' lev 'sitting'</code> is equivalent to:


```j
{. 1 1 1 1 1 1 1 6 {."1@((1&{ ,~ (1 1,{.) <./@:+ }.)@,/\.)@,.  ...  1 1 1 1 1 0 1 1 {."1@((1&{ ,~ (1 1,{.) <./@:+ }.)@,/\.)@,. 7 6 5 4 3 2 1 0
```


Or, breaking it down and avoiding problems with long lines:


```j
d=. 7 6 5 4 3 2 1 0
d=. 1 1 1 1 1 0 1 1 {."1@((1&{ ,~ (1 1,{.) <./@:+ }.)@,/\.)@,. t
d=. 1 1 1 1 1 1 1 2 {."1@((1&{ ,~ (1 1,{.) <./@:+ }.)@,/\.)@,. t
d=. 1 1 0 0 1 1 1 3 {."1@((1&{ ,~ (1 1,{.) <./@:+ }.)@,/\.)@,. t
d=. 1 1 0 0 1 1 1 4 {."1@((1&{ ,~ (1 1,{.) <./@:+ }.)@,/\.)@,. t
d=. 1 0 1 1 0 1 1 5 {."1@((1&{ ,~ (1 1,{.) <./@:+ }.)@,/\.)@,. t
d=. 1 1 1 1 1 1 1 6 {."1@((1&{ ,~ (1 1,{.) <./@:+ }.)@,/\.)@,. t
   {.d  NB. the first number in the final instance of d is our result

```


And, here, each instance of <code>d</code> corresponds to a fully computed row of <code>D</code> in the [[wp:Levenshtein_distance#Computing_Levenshtein_distance|wikipedia implementation]].

So, let's take a specific example:

```j
1 1 1 1 1 0 1 1 {."1@((1&{ ,~ (1 1,{.) <./@:+ }.)@,/\.)@,. 7 6 5 4 3 2 1 0
```


Examining this sentence, bottom up, the first thing we do is join together these two lists, as columns:


```j
   1 1 1 1 1 0 1 1 ,. 7 6 5 4 3 2 1 0
1 7
1 6
1 5
1 4
1 3
0 2
1 1
1 0
```


And then we use insert again, between each row, to compute our final result.  However, we also use the \. operator so we keep a running list of each of these intermediate results (and the last row will be kept as-is, in this accumulated result).  Finally, after this is all done, we will be pulling out the first column of this accumulated result, using <code>{."1</code>

That leaves us with a sentence like:


```j
1 7 (1&{ ,~ (1 1,{.) <./@:+ }.)@,  ...  1 1 (1&{ ,~ (1 1,{.) <./@:+ }.)@, 1 0
```


Or, breaking it down, again:


```j
dd7=.1 0
dd6=.1 1 (1&{ ,~ (1 1,{.) <./@:+ }.)@, dd7
dd5=.0 2 (1&{ ,~ (1 1,{.) <./@:+ }.)@, dd6
dd4=.1 3 (1&{ ,~ (1 1,{.) <./@:+ }.)@, dd5
dd3=.1 4 (1&{ ,~ (1 1,{.) <./@:+ }.)@, dd4
dd2=.1 5 (1&{ ,~ (1 1,{.) <./@:+ }.)@, dd3
dd1=.1 6 (1&{ ,~ (1 1,{.) <./@:+ }.)@, dd2
dd0=.1 7 (1&{ ,~ (1 1,{.) <./@:+ }.)@, dd1
```


So, for example, finding the value for dd4 is 2 3 given that we know that dd5 is 1 2


```j
   1 3, 1 2  NB. the four numbers of interest
1 3 1 2
   }. 1 3 1 2  NB. find previous delete,insert and substitute costs
3 1 2
   {. 1 3 1 2  NB. find if current character matches
1
   1 1, 1      NB. find edit cost delta from previous edit costs
1
   1 1 1+3 1 2 NB. find edit costs from adjacent states to this state
4 2 3
   <./4 2 3  NB. find cheapest of previous delete, insert or substitute
2
   1&{1 3 1 2  NB. retain current delete cost to be used as next substitute cost
3
   3,~ 2  NB. new values
2 3
```


In other words, each instance of this operation is computing the new insert and substitute costs based on the previous values.  (That said, note that "insert" and "delete" are arbitrary designations -- an "insert" on the string 'kitten' will be a delete on the string 'sitting', and vice versa.)

The downside, of these kinds of dynamic algorithms is that while the individual operations can be easy or trivial to picture, the cascading dependencies make comprehending the data as a whole rather slippery.  But perhaps viewing the data as a whole, with significant intermediate results, can help:


```j
   'kitten' {."1@((1&{ ,~ (1 1,{.) <./@:+ }.)@,/\.)@,./\.@levD 'sitting'
3 3 4 5 5 6 6 6
3 2 3 4 4 5 5 5
4 3 2 3 4 4 4 4
5 4 3 2 3 3 3 3
6 5 4 3 2 2 2 2
6 5 4 3 2 1 1 1
7 6 5 4 3 2 1 0
```


Here, the rows correspond to suffixes of the string 'kitten' and the columns correspond to suffixes of the string 'sitting', and the numbers represent the edit cost of matching the two corresponding suffixes.

== Changed definition ==

The [http://rosettacode.org/mw/index.php?title=Levenshtein_distance&oldid=211835 2015-09-15] edits to the task description change the task. Prior to those edits, substitutions cost the same as an insert or a delete. Those edits instead declare that a substitution should cost the same as an insert plus a delete. And, those edits include a reference to a paper which describes this same system.

But that's a different task from what this was. A [[https://en.wikipedia.org/wiki/Edit_distance#Formal_definition_and_properties|wikipedia entry]] currently seems to suggest that this would make the task equivalent to the [[http://rosettacode.org/wiki/Longest_Common_Subsequence|longest common subsequence]] task. I don't think that's precise, but there is a pretty strong relationship there.

Anyways, I am reverting these edits, but did not want to lose track of them.

Perhaps this is worth doing a new task for? I'm not sure... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:56, 15 September 2015 (UTC)
