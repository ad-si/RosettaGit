+++
title = "Talk:Create a two-dimensional array at runtime"
description = ""
date = 2013-09-10T13:43:02Z
aliases = []
[extra]
id = 2224
[taxonomies]
categories = []
tags = []
+++

The title of this page suggests it should really be two tasks: [[Create a multidimensional array]] and [[Read command-line arguments]] --[[User:Short Circuit|Short Circuit]] 20:56, 12 November 2007 (MST)
::Whether or not this is divided into multiple tasks, a more concise name must be possible, I'd think. --[[User:TBH|TBH]] 14:53, 10 January 2008 (MST)
: The point of this task is not ''how'' exactly the numbers are obtained, but that they are ''not known'' at compile time. That is, you cannot use any features of the language which depend on compile-time knowledge of the array size. As a concrete example, in C++ you cannot simply write
 double** array = new double[dim1][dim2];
: because for that dim2 would have to be a compile time constant. It ''would'' be a solution if you dropped the requirement that ''both'' dimensions are given at run time. --[[User:Ce|Ce]] 12:41, 11 January 2008 (MST)
:: This sort of task suggests a limit to the chrestomathy format for language comparison. Perhaps programming languages with dynamic orientation should be excluded from this sort of task, as such languages cannot demonstrate anything other than their normal array-creation features? How might we identify language features so that statuses like "impossible" or "not applicable" could be clear? --[[User:TBH|TBH]] 15:12, 11 January 2008 (MST)
::: Demonstrating a languages normal array-creation features is fine for dynamic language examples. --[[User:Paddy3118|Paddy3118]] 05:17, 16 September 2011 (UTC)

== AutoIT Incorrect? ==

The AutoIT doesn't appear to get values from the user. They are embedded within the program source. Should this be marked as incorrect?
: Probably.  I'm normally fuzzy about this kind of requirements, but the AutoIt example really doesn't make it clear if the array sizes are flexible.  --[[User:Ledrug|Ledrug]] 23:22, 15 September 2011 (UTC)
:: How to mark it as incorrect? Is there an input template?
::: <nowiki>{{incorrect|AutoIt|reasoning for mark it so}}</nowiki> --[[User:Ledrug|Ledrug]] 00:11, 16 September 2011 (UTC)

The shown example was wrong! 
I've now created a working example. Here you can see, that both dimensions can set by user at runtime.

== OoRexx ==

"up to 999,999,999 dimensions"

shouldn't this be up to 999,999,999 elements??

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:23, 14 July 2013 (UTC)
: nobody cares? and the snippet does not show how the array is deleted (Drop ??) --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 13:43, 10 September 2013 (UTC)
