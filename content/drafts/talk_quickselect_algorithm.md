+++
title = "Talk:Quickselect algorithm"
description = ""
date = 2016-10-22T19:58:13Z
aliases = []
[extra]
id = 16405
[taxonomies]
categories = []
tags = []
+++

== How is pivot chosen? ==

Should the task specify how the pivot is to be chosen? Or should it leave it up to the implementer (and perhaps specify explicitly that the implementer can choose it how they want)? For example, the Python implementation chooses a random pivot, so it is randomized quickselect. There are other ones, e.g. choose the first element, or median of three, etc. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 01:23, 1 October 2013 (UTC)

:Yep I saw that too, but thought I would just leave it open. It would allow some one to write an additional (or extend the current) Python solution using other pivot choices. I guess if it means a lot to someone then this is the time to create a separate task if they think it is necessary then restrict this task to be for random pivots, but I didn't think it was needed (which sounds better than I am lazy, however I can't truly remember if laziness applied in this case :-)
--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 22:06, 2 October 2013 (UTC)

:: I saw one program that used a random number, which I used initially in the REXX solution.   But I found that using a midpoint was slightly faster, and the method bypasses using the   '''random'''   BIF   (when generating a random number). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 10:49, 19 November 2013 (UTC)

==C# and Quicksort==
The C# solution also includes quicksort for which we already have a [[Sorting algorithms/Quicksort|task]]. I think it would be better if C# concentrated on just what was needed for Quickselect in its codem to aid in language comparisons. -[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 04:20, 6 October 2013 (UTC)

:C# solution sorting is done by LINQ OrderBy method. It really doesn't talk about how sorting is implemented (it is hidden by OrderBy method - and actually I don't know what kind method OrderBy is using). Goal was to show important feature of QuickSelect - it sorts elements such way that elements before pivot index are smaller or equal than value stored to pivot index and vice versa. However, those elementes are not sorteed. So for e.g. if you have stored employee information to somekind array structure and want to get list of 100 smallest paid employees - without actually sorting employees based on their salary - then you could use QuickSelect algorithm. --[[User:Mpuonti|Mpuonti]] ([[User talk:Mpuonti|talk]]) 06:23, 6 October 2013 (UTC)

::Thanks Mpuonti for pointing out the obvious - in a good way! Of course what is to the 'left' of the pivot are smaller, I just dismissed that in getting the algorithm working, which was done before I set the task goal where the idea of the test was to provide an easy check that the algorithm worked. 
::Unwittingly, the test does allow for optimisations, but I was hoping for examples that concentrated on getting the basic algorithm working correctly. 

::Task writing has its quirks. Thanks for the learning experience :-)
 -[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:39, 6 October 2013 (UTC)

==Task Testing Values==
In future tasks I suggest you to propose common task data values like [90, 80, 70, 60, 50, 0, 10, 20, 30, 40] instead of [9, 8, 7, 6, 5, 0, 1, 2, 3, 4] because with the first it's a little less easy to produce a correct output in presence of code bugs. Also to reduce the probability of having buggy entries that give a correct answer on just the common task data values, I suggest to offer more test cases.
:It is a compromise. I had to give some values from when the task was created and chose the ones I did to at least have a common set of values that were easy to check by eye. At the time of writing, I wanted something, rather than nothing at all. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 13:14, 11 October 2013 (UTC)

:: I also would suggest adding another part to the task requirement:   ''Ensure (but do not show) that the program works with a single value.''     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 10:56, 19 November 2013 (UTC)

== Scala implementation has a bug ==

Consider the input 

   quickSelect(Array(1, 1), 1)

The algorithm will loop infinitely. The reason is the pivot remains in the right partition, so if the pivot is the smallest element in the sequence, the recursive call will be on the same input. This is not too much of a problem if the elements in the right partition are different, because the pivot is chosen randomly so the next pivot might not be the smallest. But if the elements in the right partition are all the same the next pivot will be the same. Here is an implementation that does not use a random pivot, but fixes the bug: https://gist.github.com/jrraymond/602a12d4d648faf592473c80e8a6027b
