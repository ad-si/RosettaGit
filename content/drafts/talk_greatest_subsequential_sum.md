+++
title = "Talk:Greatest subsequential sum"
description = ""
date = 2017-01-04T20:05:39Z
aliases = []
[extra]
id = 2086
[taxonomies]
categories = []
tags = []
+++

== what is the task?==
Currently this contains some ruby code, and no clear description of exactly what the task is. Please clarify the exact goal so others can provide implementations for the languages they use. --- [[User:crc|crc]] 2007-06-21

: I think the task is to find the subsequence in the array which has the largest sum of elements, or an empty sequence if all numbers are negative (is arr[0..nil] a representation of the empty sequence in Ruby?). If I understand the code correctly, it never includes leading or trailing zeroes (e.g. for the array [0, 1, 5, 3, 0] it returns [1, 5, 3]), but otherwise if there are several subsequences with the same maximal sum, the result is the first one (i.e. it doesn't generally search the shortest one; e.g. for [1, 2, 3, -100, 1, 5] it gives [1, 2, 3], not [1, 5]). However, I don't know Ruby, so I'm not completely sure I'm interpreting it correctly. Also, it's not clear to me how much of that behaviour is part of the task, and how much just happens to be a property of the specific implementation (e.g. would code which includes leading or trailing zeroes in the sequence or code which finds e.g. the last or the shortest sequence with maximal sum also solve the task?) --[[User:Ce|Ce]] 03:46, 24 June 2007 (EDT)

: Ok, I now have changed the text to my interpretation (I've taken the most liberal interpretation, i.e. not fixing at all which subsequence to take in case more than one has the same value; I also allow empty sequences, which have sum 0, because I think that's what the Ruby code does). I've also added a C++ implementation (which I believe behaves exactly like the ruby one, thus I hope that even if my interpretation of the task turns out not to be what was meant, the code should still be correct for the task to be solved).

==<i>subarray</i> is a somewhat unclear term==
The task does not specify anything about the presumed topology of the "subarray": In some languages, the "shape" of an array can be a rather fuzzy notion, and even if your language has precise rectangular, evenly-spaced arrays, it is not clear from the spec whether the supposed "subarray" has to have a rectangular shape. For example in 2 dimensions, an array could be a grid and this task might be asking for a L-shaped area in that grid. Or maybe only convex shapes are allowed. Or, indeed, only rectangles. What if an "L" can be turned into a rectangle by adding an element that contains zero? I think there needs a much clearer statement of purpose here somewhere... [[User:Sgeier|Sgeier]] 18:13, 3 August 2007 (EDT)

: From the original Ruby example code, I'd expect it to be restricted to one-dimensional arrays. That's also what I implemented in C++ (actually, my function works not only on arrays, but on any sequence accessible through forward iterators, but sequences are one-dimensional by definition, too). An obvious restriction is, of course, that the array has a finite number of elements (some languages may be able to describe infinite arrays).
: Possibly renaming the article from "Maximum subarray" to "Maximum subsequence" would be a good idea (after all, the interesting part here is the algorithm, not the actual data structure used to store it; e.g. in Lisp, one might prefer to use lists rather than arrays). Or even better, rename it to something like "Subsequence with maximal element sum" (surely a better new title can be found along this line). --[[User:Ce|Ce]] 20:45, 4 August 2007 (EDT)

:: I agree that "Maximum subsequence" would make a better title, and that specification in terms of subarrays suggests that summing across portions of higher-dimensional arrays should be included. [[User:TBH|TBH]] 04:24, 24 December 2007

== Erroneous Examples ==
Many of the examples initialize the maximum subarray sum to 0. This produces erroneous results if the maximum subarray total is negative. The maximum subarray should be initialized to the most negative integer value available for each language.--[[User:Waldorf|Waldorf]] 16:37, 29 December 2007 (MST)

The statement that a zero length array will produce a sum of 0 is nonsense. 0 is a valid sum. A zero length array cannot have a sum. All solutions should provide a valid representation for a zero length array as an input to the maximum subarray calculation. The zero length subarray should be reported as an exceptional (or erroneous) condition.--[[User:Waldorf|Waldorf]] 21:20, 29 December 2007 (MST)
:This brings up an interesting philosophical discussion. What is the sum of no numbers (and is it related to the sound of one hand clapping)? --[[User:Mwn3d|Mwn3d]] 14:24, 30 December 2007 (MST)

:It is wholly sensible to specify summation such that zero is the result of summation of a list of no elements. While this is not derivable from the qualities of addition, it may be established by definition. The value of avoiding exceptional conditions is very high. To have summation defined for all nonnegative integers allows many things to be specified simply that would otherwise involve messy special-case handling. One important result is notational clarity. For the unusual situations where summation should be rejected for lists shorter than two elements, input qualification testing will suffice. --[[User:TBH|TBH]] 06:15, 4 January 2008 (PST)

==Some bugs==
The second Python version (and probably other versions, I have fixed the D version) have bugs, try:
[-1, 1, 2, -5, -6]

:I've fixed the bug in Python version [functions maxsumseq() and maxsumit()] - [[User:A|a__]] 17:17, 28 March 2011 (UTC)

==Open question==
Take n random sequences, each of 1000 random numbers in [-1000 .. 1000]. 
Take the mean M<sub>n</sub> of their best score ( sum of values of max sub-sequence / n). Is there a limit for M<sub>n</sub> when n → ∞ ? Experiments show something around 22000, but a formal proof will be great !
--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 17:55, 20 September 2015 (UTC)
