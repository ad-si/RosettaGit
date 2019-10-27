+++
title = "Rosetta Code:Village Pump/Arrays"
description = ""
date = 2013-08-01T12:58:25Z
aliases = []
[extra]
id = 4610
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Arrays
|summary=Discussion of just what exactly we mean by an array.
}}
In RC, Array tasks are not very organized. Let's see: [[Array]], [[Creating an Array]], [[Array Initialization]], [[Collections]], and a lot of examples in [[Creating an Array]] (mostly dynamic languages like Python) aren't really using Arrays, they are using just Lists, Vectors and etc. Arrays are not dynamic. A solution is create a [[Creating a List]] or etc. But other languages call it in a different way. This is not really a solution.

:I don't think you can force a distinction between Dynamic and Static arrays. In many languages, it is common practice to use a particular construct in place of another common one. This makes logical sense to appear as the "correct" translation. For example, perl implements many higher-level concepts based on it's standard dynamic data structures, sometimes with special syntatic support. These clearly fit here even if underlying implementation is visible in the language. ----[[Special:Contributions/66.245.151.131|66.245.151.131]] 04:39, 13 August 2009 (UTC)

A real idea is merging [[Assigning Values to an Array]], [[Creating an Array]] and all Array related tasks in an single task. ([[Arrays]]), and creating another task for any "dynamic array" (Lists/Vectors/Sets) called [[Dynamic Arrays]], showing how to create and append values to it. There is a lot of confusion on array tasks in RC. This really need to be changed. --[[User:Guga360|Guga360]] 02:35, 30 July 2009 (UTC)
:I don't like the single task idea. That single task could become very bloated when people try to show all the possible things you can do with an array (and all the ways you can do them). Creating an Array and Array Initialization already have efforts in the works to combine them (not really combine them...more like get rid of Array Initialization completely after all of its information has been reviewed by people who understand the examples that are left). There might be another way to simplify all of the array tasks without making them into one gigantic, memory-hogging, uneditable task. --[[User:Mwn3d|Mwn3d]] 02:53, 30 July 2009 (UTC)
::I understand. But, about "dynamic arrays", what do you think? Python example is using Lists, but Python has an "array" module. C++ example is using both: arrays and vectors, C# example is showing only arrays, but C# has a List class. There is no OCaml example to create and append values to "dynamic arrays" in RC. I think splitting arrays and dynamic arrays it's a good idea. Arrays are not dynamic. But a lot of examples are using dynamic arrays (Lists/Vectors/Sets). This is not exactly right. --[[User:Guga360|Guga360]] 03:05, 30 July 2009 (UTC)

:::I prefer that we set tasks that allow some flexibility in how they are accomplished w.r.t. language datatypes. What if a languages array type were dynamic in that it could automatically grow in length to accommodate any index, but always used a computed offset into contiguous memory for indexing? What if an array could use two or more regions of memory and the array indexing take account of the fact that, for example, different ranges of indices might be in separate memory blocks? It shouldn't matter too much, unless the task is, (like in the number of sort tasks), ''how'' memory is used in the array, rather than a data-structure that is accessed roughly in a certain way. Conversely. If a task were to interface to a C function that took a pointer to a C array as an argument, then the language would have to make sure to send a pointer to a contiguous region of memory set with appropriate values - but the language in normal use may not use such 'arrays' except for interfacing to C. Why punish the language in this case? --[[User:Paddy3118|Paddy3118]] 04:43, 30 July 2009 (UTC)

I advise being very careful. The problem is that language terminology varies wildly and permitted operations is, if anything, even more differentiated across the languages. What is an obvious merger for one might be a loss of an important subtlety for another. (It was the “aren't really using Arrays” that triggered this message, as it indicated that the author of it might be projecting how one language does things onto others, which is usually a mistake.) —[[User:Dkf|Donal Fellows]] 09:56, 30 July 2009 (UTC)
:I agree with this entirely. (Also, if we're going to be specific about the kind of data structure, I would suggest (as opposed to "contiguous memory"): consecutive numeric indexing, arbitrary size, and no greater than O(log n) element access time.) --[[User:Kevin Reid|Kevin Reid]] 11:35, 30 July 2009 (UTC)

:: As for element access time, that would be a sticky wicket.  Some REXX interpreters have serial lookup for all variables (including array elements), some (if not most) have hashing techniques.  Most REXX programmers don't know nor care.  REXX may be one of the few languages that doesn't keep it's array elements in sequential order.  The term then, consecutive numeric indexing, falls by the wayside in REXX.  One can assign array elements consecutively (numerically), but that would be just an artifact.  You'd have to add another one or two or three categories, and I struggle to think what they would be called: a.2=222; a.02=333; a.2.=444; a..2='omg'; g=.7; a.g=555; h='±'; a.h=666 --- all unique. I think grouping arrays by how the language supports them would get argumentative fast, depending on how one interprets the definitions and understanding on ''how'' a particular version of a language worked.  Technically, REXX wouldn't even be invited to his party, as it doesn't have "true" arrays, but that doesn't stop anyone from using REXX's version of arrays: ''stemmed arrays''. They look, taste, feel, and smell like arrays, so, what the heck. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:37, 20 May 2012 (UTC)

:I think it would be fine to merge [[Creating an Array]], [[Assigning Values to an Array]], and [[Retrieving an Element of an Array]], mostly as a way to show off a language's built-in syntax for native arrays.  I think some of the higher order array tasks ([[Select from Array]], [[Apply a callback to an Array]], [[Sorting an Array of Integers]], etc.) should instead operate on the most convenient collection available to the language, no matter whether it is an array, list or some other collection object. --[[User:IanOsgood|IanOsgood]] 17:16, 30 July 2009 (UTC)
::This is OK, with this we can show array syntax in a language and separate "fixed length" and "dyanamic arrays" in a single task. --[[User:Guga360|Guga360]] 17:47, 30 July 2009 (UTC)

I created a small [[Arrays]] page. It's something like this. This sounds OK. We should merge [[Creating an Array]], [[Assigning Values to an Array]], and [[Retrieving an Element of an Array]] to [[Arrays]] showing dynamic arrays, or we should delete [[Arrays]] and don't change anything? --[[User:Guga360|Guga360]] 18:13, 30 July 2009 (UTC)
:Don't delete anything yet. In my experience the merge process here is slow (and probably will be until we get interns). It will be difficult to find someone who knows enough about each language to incorporate all the existing info into a new task. --[[User:Mwn3d|Mwn3d]] 18:25, 30 July 2009 (UTC)

:Could you add to the task so that entries also state what the minimum index of an array is and whether or not this minimum index is changeable (and by what means). Some languages (Verilog and VHDL certainly), allow you to state the minimum and maximum index range in the variable definition and both or either may be zero negative or positive! --[[User:Paddy3118|Paddy3118]] 19:26, 30 July 2009 (UTC)

:: If the purpose is to combine the three tasks mentioned, then that could be done simply by copying the contents of [[Creating an Array]] into [[Arrays]]. Then just add two lines of code into each language example to assign and to retrieve a value. --[[User:PauliKL|PauliKL]] 11:45, 13 August 2009 (UTC)

I wonder though whether this site is suitable for providing generic translations or patterns that might be applied in any language to create or emulate a missing construct. For example, implementing static arrays using hashes or strings. ----[[Special:Contributions/66.245.151.131|66.245.151.131]] 04:39, 13 August 2009 (UTC)


The confusion arises from using the term "array" for different things. This causes ambiguity in the task description. As far as I see it, we can distinguish the following categories:

# linear array, i.e. lists (including list of lists)
# associative arrays, (i.e. hash arrays).  
# multidimensional arrays (i.e. static array), the number of elements along a certain dimension is fixed. This type of array allows for random access. A matrix is usually a two-dimensional array.

If some languages uses the term "array" for some other data structure, it should be added here.  

Any task description should consider this distinction and make clear which kind of array should be used. Only then we can avoid ambigous task descriptions. [[User:Aschloegl|Aschloegl]] ([[User talk:Aschloegl|talk]]) 15:21, 21 July 2013 (UTC)

:For many tasks, this could end up being a distinction without true meaning. Unless the task is specifically about the distinctions you make above then how a task is implemented should be done in the idiomatic way of a language so we can better compare the languages rather than artificially restricting all languages to use the same meaning for "array". If some languages uses a space separated string of the numeric representation of integers as its array on integers then if the task is to store the first ten integers in an array then print the 7'th member of the array, then I think it would be pretty dumb for that languages example to jump through hoops to store ints in any way other than how it does naturally. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:45, 22 July 2013 (UTC)

::I think the term "array" should only be used when talking about arrays. A list is not an array. A hash is not an array. An array is something that allows random access, i.e. you can get a specific item directly using index(es). Note that an array does not need to be multidimensional. A single dimensional array is an array, too.
::Indeed, there is no point to include requirement of some specific data structure in the task description, unless the task is specifically about that data structure. Not all languages have all possible data structures built-in. The task description should describe what is the goal of the task, not how to implement it. --[[User:PauliKL|PauliKL]] ([[User talk:PauliKL|talk]]) 12:58, 1 August 2013 (UTC)
