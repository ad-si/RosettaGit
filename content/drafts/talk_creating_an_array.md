+++
title = "Talk:Creating an Array"
description = ""
date = 2009-08-13T02:16:11Z
aliases = []
[extra]
id = 1633
[taxonomies]
categories = []
tags = []
+++

== You're forgetting array base and associative arrays ==

One critical thing missing from this is the concept of zero-based, 1-based or other types of arrays.  Some languages start at 0, others at 1.   

There is also the concept of associative arrays that have both numeric and text based indexes.  They are not supported in all languages either.

:I added a link for people interested in associative arrays.  Since not all languages support array bases, a separate page should be created for changing the base of arrays. --[[User:Short Circuit|Short Circuit]] 08:52, 23 January 2007 (EST)
::On second thought, it would be better to just mention whether arrays begin at 0 or 1. --[[User:Short Circuit|Short Circuit]] 09:08, 1 February 2007 (EST)

== VB coders... ==

I'm sure it's not representative of all VB programmers but the VB entry uses an array list instead of an array. Or is it, perhaps, intentional. One ponders..

== x86 Assembly ==

And what of this ancient language?

:x86 assembly doesn't inherently support an array construct, unless one considers SIMD instructions to be indicative of such.  However, in most of those cases, one is not dealing with multiple values in one variable, but with multiple variables. (Each register representing one variable.)  I suppose one could consider register sets specific to SIMD extensions to be implied arrays.) --[[User:Short Circuit|Short Circuit]] 18:58, 22 January 2007 (EST)

:: I believe for most cpus "creating an array" could mean simply "allocating" memory for it (and "allocating" is rather vague or system specific, but reserving space on the user stack &mdash;if it exists, like in the x86 case&mdash; could be a possible meaning) --[[User:ShinTakezou|ShinTakezou]] 16:51, 9 April 2009 (UTC)

== I think the definition of array needs more elaboration before examples are shown. ==

I think the definition of array needs further discussion over and above what is provide in the existing definition, as the example for Python is actually a list and its capabilities and behaviour differ considerable from what I consider to be a traditional array. For example a Python list is not sparse and you cannot predefine it's size. Its size is determined by its contents.  So I think the python example and the other array related examples are really incorrect and misleading. If you want traditional array characteristics the example should use numarray or the included array module. Without these points of clarification (and I assume similiar comments could also be made about some of the other languages (ie Perl)) it would seem the examples in fact do not fulfill what this project sets out to achieve.

== PHP ==
::<quote>
::You would call the array by this code. This will call the 3rd 1 on the second list
::echo $array[1][3];
::</quote>
:This is in fact wrong, 'echo $array[1][3] would print the 4th element within the second element, not the 3rd one. I'll edit tonight or something... --[[User:CrashandDie|CrashandDie]] 07:18, 2 July 2007 (EDT)

== Python ==
:''See also [[Two-dimensional array (runtime)]]''
Is there any easy way to do a Dim (like from Basic) on Python, as a multidimentional array?
'a=[0]*8' seems to be similar to 'dim a[8]', but i have no idea about how to do on an at least bidimentional array, since neither 'a=[0][0]*8' nor 'a=[0]*8[0]*8' works...

:Here are two ways to do two dim arrays in python:

    >>> m,n = 3,4
    >>> from pprint import pprint as pp
    >>> a = dict(((x,y), 0) for x in range(m) for y in range(n))
    >>> pp(a)
    {(0, 0): 0,
    (0, 1): 0,
    (0, 2): 0,
    (0, 3): 0,
    (1, 0): 0,
    (1, 1): 0,
    (1, 2): 0,
    (1, 3): 0,
    (2, 0): 0,
    (2, 1): 0,
    (2, 2): 0,
    (2, 3): 0}
    >>> a[(0,0)] = 1
    >>> a[(3,2)] = 1
    >>> pp(a)
    {(0, 0): 1,
    (0, 1): 0,
    (0, 2): 0,
    (0, 3): 0,
    (1, 0): 0,
    (1, 1): 0,
    (1, 2): 0,
    (1, 3): 0,
    (2, 0): 0,
    (2, 1): 0,
    (2, 2): 0,
    (2, 3): 0,
    (3, 2): 1}
    >>> b = [[0]*m for _ in range(n)]
    >>> pp(b)
    [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]]
    >>> b[0][0] = 1
    >>> b[3][2] = 1
    >>> pp(b)
    [[1, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 1]]
    >>>

== Merge Create, Assign, and Retreive from Array ==

I think it would be clearer to merge the three tasks [[Creating an Array]], [[Assigning Values to an Array]], and [[Retrieving an Element of an Array]] into a single task, [[Array access]] perhaps. --[[User:IanOsgood|IanOsgood]] 11:13, 20 October 2007 (MDT)
:Normally, I don't like merges, but I think you're right in this case. --[[User:Short Circuit|Short Circuit]] 11:41, 20 October 2007 (MDT)
:Also, perhaps we could address the array definition issue mentioned above, as well, perhaps by specifying a "numerically-indexed array". --[[User:Short Circuit|Short Circuit]] 11:44, 20 October 2007 (MDT)
::Well, in my opinion if you stop indexing by cardinals, then it is no longer an array. :) Associative arrays are poorly named. --[[User:IanOsgood|IanOsgood]] 12:47, 20 October 2007 (MDT)
:::Perhaps a rebranding as "tuple" is in order? --[[User:Short Circuit|Short Circuit]] 13:37, 20 October 2007 (MDT)
:Be sure to remove languages from this task once they're adequately represented in the others. --[[User:Short Circuit|Short Circuit]] 02:16, 13 August 2009 (UTC)
