+++
title = "Talk:Sort stability"
description = ""
date = 2011-06-13T14:29:43Z
aliases = []
[extra]
id = 4340
[taxonomies]
categories = []
tags = []
+++

==Why not ask for a stable sort?==

Why not just ask for an implementation of a stable sort? 

What purpose does it serve to exclude languages that can implement the task as a user routine? --[[User:Tinku99|Tinku99]] 05:08, 7 June 2009 (UTC)
:Hi Tinku, Some people, especially those new to a language, might not know too much about the facilities that the language comes with. This task is a comparison of what languages provide rather than what they can be programmed to provide and might be just as useful.
 [[Integer literals]], [[Mutual Recursion]], and [[Common number base formatting]] are some other tasks that similarly try to contrast the 'native' capabilities of each language. --[[User:Paddy3118|Paddy3118]] 09:08, 7 June 2009 (UTC)

==J and stability?==
Please excuse me, I don't intend to bang on about J, but sorting in ascending order might not be the same thing as a stable sort. The example given in the task description I would say does ''not'' sort other columns in ascending order. If the column being sorted on (the second column), has the same values then the relative order of those rows must stay the same. The table has the US and UK Birmingham rows in ''reverse'' alphabetical order w.r.t. the first column. A stable sort would leave the US above the UK Birmingham. If J would swap them then it does not do a stable sort. --[[User:Paddy3118|Paddy3118]] 20:25, 28 August 2009 (UTC)
:Hi Paddy, No problem. It doesn't talk about <i>sorting</i> in ascending order. It says "Elements of /:y that select equal elements of y are in ascending order" - essentially that equal elements will appear in order of appearance.

```j
   /: 'adfafb'
0 3 5 1 2 4
```

This is the grade and tells us that to sort the list in ascending order we'd arrange the list by its indices as shown.  In this case the equal elements of y are the two a's and the two f's. The a's have indices 0 & 3, the f's have indices 2 & 4. The elements of /:y that select equal elements of y appear in ascending order so the 0 appears before the 3 in the grade and the 2 appears before the 4. Hope that helps! --[[User:tikkanz|Tikkanz]]

```j
   NB. sort txt by txt without the first 4 columns
   txt /: 4&}."1 txt
US  Birmingham
UK  Birmingham
UK  London    
US  New York  

```


:Thanks Tikkanz for the excellent explanation. --[[User:Paddy3118|Paddy3118]] 08:57, 29 August 2009 (UTC)

==GAP and stability?==
Does GAP seem stable with the example given in the task description? I find it hard to follow what is being described in the GAP example. (Maybe some of the notes would be better off here in the talk page and maybe expanded apon)? --[[User:Paddy3118|Paddy3118]] 21:41, 12 June 2011 (UTC)
:I thought it was easy, you can see the R implementation (which I also wrote), mayber it's easier to read. The idea: sort an array of random values A/B, and get the sorting permutation. Then apply this permutation on an array of already sorted integers 1..n. In the result, there are two blocks, one for A and the other for B. If integers are sorted in both blocks, then the sort is stable in this case (I assume you understand why !). Of course I didn't prove it's stable in all cases, but I was not able to find a counterexample. Usually, an unstable sort fails even on such trivial examples (again, see R).
:Funnily, R uses also shell sort (or optionally a quick sort), and the manuals says that when shell sort is called on an array ''with names'', it uses a stable sort. Now, is there a stable variant of shell sort ? Or have I tried a very special case where shell sort is stable ? I'll have to look at this...
:[[User:Toucan|Toucan]] 08:30, 13 June 2011 (UTC)
::Ok, I've found the answer. In file lib/list.gi of GAP source distribution, we can see that SortingPerm doesn't sort exactly its argument (let's say it's L), but a list of pairs [ L[i], i ]. Since comparison of lists is done with lexicographic order, even with an unstable sort, the pairs will be sorted in a stable way (the integers indexes are here to distinguish two equal elements of L). The code I use to check merely retrieves these indexes. Conclusion : Sort is not stable, but we don't care since we can't check for it. And SortingPerm is stable.
::[[User:Toucan|Toucan]] 14:29, 13 June 2011 (UTC)
