+++
title = "Talk:Assigning Values to an Array"
description = ""
date = 2008-10-12T23:11:34Z
aliases = []
[extra]
id = 1627
[taxonomies]
categories = []
tags = []
+++

Why disturb something as simple as assigning a value in an array with a method/function
declaration?

: Seconded: am I supposed to assign a value to an array or write a function/routine/procedure to assign a value to an array? [[User:Sgeier|Sgeier]] 01:09, 20 February 2007 (EST)
::I think the task is more complicated than need be.  In fact, it should probably be broken into an "Assigning values" and a "Checking to see if a value exists" task. --[[User:Short Circuit|Short Circuit]] 08:04, 20 February 2007 (EST)
::I'm not sure what I was thinking. (^)  "Assigning values to an array" should be a separate task from "create a function to do X". --[[User:Short Circuit|Short Circuit]] 23:11, 12 October 2008 (UTC)

== Type of error to return?  Type of index? ==

This task leaves a lot up in the air.   Some languages can only do numerical indexes, some can do anything.   Also, the requirements say to return an error if the key doesn't exist.  None of the languages at this time fulfill this requirement, except Ada, which will throw an exception if the key is out of bounds.

:I flagged the task for clarification. --[[User:Short Circuit|Short Circuit]] 22:42, 22 January 2007 (EST)

:I think it is that most of the languages (e.g. Python, Java) automatically raise exceptions when an index is out of range. And most languages have in place a way to capture those exceptions and deal with it. Therefore, you do not need to explicitly raise an error unless you deem it necessary. Maybe change the specification to raise an error where applicable for languages that do not have mechanisms in place to handle such exceptions? Such as C for example? --[[User:Adonis|Adonis]] 17:59, 24 January 2007 (EST)

::A more fair task formulation would be to instruct the programmer to catch the errors before the OS or the PL, so that neither OS nor the inbuilt, should need to detect any error. [[User:Rursus|Rursus]] 07:19, 31 December 2007 (MST)
:::I think it might be appropriate, despite the amount of code already in the task, to split the task between Assigning Values to an Array, and one for catching errors.  The problem is, what kinds of errors need to be caught?  Not all languages recognize array bounds, for example. As it is, error checking distracts from what would typically be a syntactical problem, not a procedural problem. Thoughts? --[[User:Short Circuit|Short Circuit]] 22:43, 5 January 2008 (MST)

::BTW, i rewrote the [[array]] article, because it only described the compmetrics, and the numeric access, not the lower and upper bounds, nor the everything-inbetween-can-be-accessed rule. [[User:Rursus|Rursus]] 07:25, 31 December 2007 (MST)
