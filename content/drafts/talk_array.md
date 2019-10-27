+++
title = "Talk:Array"
description = ""
date = 2009-04-04T08:27:52Z
aliases = []
[extra]
id = 2444
[taxonomies]
categories = []
tags = []
+++

What is this task actually asking to do? The explanation is reasonably straightforward (though it omits associative arrays entirely) but it doesn't say what is actually to be done. The single example (in C) computes a histogram over a file - is that what all (different-language) examples in this task should do? [[User:Sgeier|Sgeier]] 14:19, 2 January 2008 (MST)
:This isn't a task, but an explanation of a data structure (check the backlinks). The code is just for illustration, and it probably shouldn't use the {header} template.
:An associative array is an array in name only. It is a map of some sort underneath (e.g. alist, tree, hash). --[[User:IanOsgood|IanOsgood]] 15:00, 2 January 2008 (MST)
::It seems to me that code can provide, at best, an illustration of how the data structure can be implemented. It may be worth working to avoid equating the data structure, per se, with specifics of implementation. --[[User:TBH|TBH]] 15:40, 2 January 2008 (PST)

:: Oh, I guess I'm just kinda confused. I clicked on "solutions by task", there I clicked on "data structures" and there on "array". Maybe this organization is just a little awkward. So this means we don't need/want any further code examples here? Upon reading the description I immediately thought about FORTRAN (where the lower array boundary can be set to any number, even a negative one -- and that since 1977 ...) and IDL (where arrays are elevated to a kind of art form). Is it useful to show a couple languages worth of examples with maybe explanations how/where they deviate from what one would expect in other languages? 

:: While I'm at it: the description here is somewhat informal; I've always thought of arrays as sets of elements in a mathematical sense -- numerically indexed they become ordered sets or as associative arrays they're simply unordered. In that sense, the line between "regular arrays" and associative arrays is actually not so wide. That's why I see no problem with, say, arrays of structures which contain scalars, strings and yet-other-arrays. For example. There's a subtle difference between an array of structures and a structure that holds arrayed data and there's a lot of beautiful (and, I fear, way too language-specific) code one could throw around here. Should we expand on that kind of thing here? How in C a[i] is the same as i[a] since arrays are just pointers, for example? How an n-dimensional array can be thought of as a k-dimensional array of (n-k)-dimensional arrays? That kind of thing? 

::(don't mind me, just throwing out ideas...) [[User:Sgeier|Sgeier]] 17:09, 3 January 2008 (MST)
::: Since its inception, I've wanted Rosetta Code to have encyclopedic content.  Both our current browsing indexes focus on tasks, but there is a growing numper of non-task categories and pages that probably deserve a third, encyclopedic index.  I'll get on that.

::: Oh...And I've only been as far as calc 2; Formal descriptions of computer science topics annoy the hell out of me, and are often beyond me.  I have no problem with formal descriptions being on RC, but I definitely want to see informal descriptions accompanying them. --[[User:Short Circuit|Short Circuit]] 23:26, 5 January 2008 (MST)

:The example shown does not clearly demonstrate all the qualities of an array mentioned in the description. For instance, the description states that it is illegal to exceed the upper bound of an array. The example does not demonstrate any check for bounds overflow. The discussion does not indicate what happens to an array when the lower bound is violated. I think it would be good to provide an example more completely demonstrating the behaviors of an array.--[[User:Waldorf|Waldorf]] 16:39, 5 January 2008 (MST)

::I share Waldorf's dissatisfaction with the example code. To reiterate my comment from last week, I doubt that this topic can be properly approached through code examples. Mind you, I love seeing assertions made in code; that's part of why I rely on J. This article should be accessible to everybody; it should not require knowledge of any computer language. So, the contents should be highly abstract and applicable to as broad a range of languages as possible. --[[User:TBH|TBH]] 12:38, 8 January 2008 (MST)

== Rework ==

Does anybody want to take on the task of rewriting this...er...whatever it is?  I don't care if it becomes an encyclopedia article or a task, but it *needs* to be better organized; The description section of what an array isn't organized at all, so the TOC appears after it.  And the code examples aren't all really related to each other, and most have more to do with I/O than with arrays (associative or linear). --[[User:Short Circuit|Short Circuit]] 05:53, 12 February 2009 (UTC)

: Should this become an umbrella (encyclopedic) page for several kind of data structures that can be defined in some general way ''array''? Or by the word ''array'' alone one should think always of a sequence of (etherogeneous or not, depending on the language) objects "labelled" with a integer index? (Not real labelling, otherwise we fall in the associative array hug which is already on [[Associative array|another page]]). Maybe suggestions about organization can be taken from [[wp:Array|wikipedia]], where array is just one of several possible linear data structures... --[[User:ShinTakezou|ShinTakezou]] 23:38, 2 April 2009 (UTC)

:: I think that Wikipedia represents rather poor example, by taking only one meaning of the word array. "Associative array" is a well established term in CS. But already this narrow definition is wider than what you refers as an array. Wikipedia means kind of container:
<blockquote>
* built-in/primitive,
* dense/contiguous in memory,
* dense/contiguous index,
* elements of same type,
* elements of same size.
</blockquote>
:: For example index is certainly not integer, when arrays are indexed by an enumeration type or are multi-dimensional. I suppose that what most of people associate with an array of this kind is in fact an ordered container indexed as [[O]](1), rather than a data structure of certain type. (:-)) --[[User:Dmitry-kazakov|Dmitry-kazakov]] 10:30, 3 April 2009 (UTC)

::: "Array" seems to be one of those words meaning all and nothing in the same time. The fact that we need an extra adjective to specify it better (multidimensional, associative) seems to confirm it. As concept it has a broad sense. As "thing", it seems it depends a lot on the language: PHP and AWK e.g. call array what it's clearer to call hash table or associative array; C has only arrays indexable by an integer (or two, or three etc. ints for multidimensional arrays), and so it is for e.g. Fortran, Pascal (as far as I can remember) and more langs. As suggested, maybe this page should become an "umbrella" for more specific kind of data structures that can be grouped as "(adjective) arrays". (Maybe ''array'' is used as synonym for ''collection''?) --[[User:ShinTakezou|ShinTakezou]] 12:53, 3 April 2009 (UTC)

:::: I agree that array should become a category with more than one articles in it. To me too, ''Collection'' feels quite close to ''array''. Though it is less oriented on implementation issues, and more on the interface. I believe it was introduced as an [[object-oriented|OO]]-ish substitute for more data/representation-oriented ''array''. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 13:04, 3 April 2009 (UTC)

:::[[wp:Array]] and [[wp:associative array]] are both well defined and ''distinct'' terms. Wikipedia has it right. A lowest-common-denominator, generic "array" is presumed to be a contiguous chunk of same-sized objects, accessed via base, index, and stride, as is supported all the way down to the instruction set in most processors. Such arrays have special syntax in most early, low-level languages. Arrays are the building-block of higher-level collections.  "Associative arrays" are a class of collections which obey an array-like protocol and which could have a variety of implementations, each with different benefits. Their only similarities are name, protocol, and sometimes shared language syntax. In my opinion, they should be kept very distinct on Rosetta Code, following the example of Wikipedia. --[[User:IanOsgood|IanOsgood]] 13:15, 3 April 2009 (UTC)

:::: Fundamentally I agree. (Except in some wording, maybe often computer scientists fail using human language? Array can't be too much distinct from "associative array", or we should call "array" ''abraca'' and "associative array" ''salakur''; otherwise the idea is that an associative array is just an array, one of the possible kind of; forget it if you did not mean that an "associative array" is not an "array") --[[User:ShinTakezou|ShinTakezou]] 15:09, 3 April 2009 (UTC)

:::::Associative array is not an array, just like butterfly is not a fly. Of course it is possible that the data of an associative array is internally stored in an array. After all, a real associative array can not exist in normal computer memory, you would need special hardware. Array is quite a clear concept, as specified in Wikipedia. For example, it is always indexed with integer. (Note: enumeration type ''is'' integer).
:::::I think this page should be an encyclopedia article. Then, some tasks could be created for performing some specific array operations. There is already at least [[Array Initialization]] and [[Creating an Array]]. Maybe there could be a category for grouping these. --[[User:PauliKL|PauliKL]] 16:25, 3 April 2009 (UTC)

:::::: <nowiki>[</nowiki>Please do not feed my interest in word-playing or filology by writing such a nonsense thing like "associative array" &ne; "array" deduced by "butterfly" &ne; "fly"; it's a completely different thing! I couldn't help answering this with at least this ''bit'' :D<nowiki>]</nowiki> --[[User:ShinTakezou|ShinTakezou]] 22:42, 3 April 2009 (UTC)

::::::Enumeration is not an integer, except than in C, which, honestly, does not have enumerations. Let us not confuse ''named integer constants'' with enumeration type. Integer is a datatype that provides a certain set of operations ([http://en.wikipedia.org/wiki/Peano_arithmetic Peano arithmetic]). Enumeration is a set of values without such operations except for comparison and copy.

::::::The key logical difference between associative and "plain" arrays is that the index of former is not necessarily dense, which does not allow iteration. The commonality is that both are maps of index to element. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 08:27, 4 April 2009 (UTC)
