+++
title = "Talk:Permutations with some identical elements"
description = ""
date = 2019-07-27T18:34:08Z
aliases = []
[extra]
id = 22436
[taxonomies]
categories = []
tags = []
+++

==Duplicate of Permutations with repetitions==

Seems to me this exact operation is covered by the existing (for 6 years!) [[Permutations_with_repetitions]] task. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 19:32, 25 July 2019 (UTC)
:I don't think so.He wants to create the permutation of different elements in which each element has its own constant multiplicity in this permutation. His example: [2,1] ( element 1 two times und element 2 1 time ) should give results (1,1,2), (1,2,1) and (2,1,1) 
[[User:Horst.h|Horst.h]]

I don't think the permutations_with_repetitions task is about permutations at all, but rather generating all possible sequences (words) of a certain length from an alphabet, where each symbol may be repeated an infinite amount of times as needed.

This task is instead about creating all sequences of a finite set of elements where some elements are indistinguishable from each other.

:Yes, you're right; it is different. I've added a Go solution --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 21:27, 25 July 2019 (UTC)


== task wording ==
Where it says:

    Given an input of the form [a1, a2, ..., ak] where ai denotes how many duplicates of element i you should have, 
    each ai > 0 and the sum of all ai is n.

would it be better with:

    Given an input of the form   <big>[a1, a2, ..., ak]</big>   where   <big>a<sub>k</sub></big>   denotes how many duplicates of element   <big>k</big>   you should have, 
    each   <big>a<sub>k</sub></big> &gt; 0   and the sum of all   <big>a<sub>k</sub>   is   <big>'''n'''</big>.


(I've added whitespace and used a bigger font to make the subscripts easier to read.)     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:51, 27 July 2019 (UTC)
: That is excellent! --[[User:Tobega|Tobega]]
