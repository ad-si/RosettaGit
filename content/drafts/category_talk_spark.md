+++
title = "Category talk:SPARK"
description = ""
date = 2010-09-14T10:59:06Z
aliases = []
[extra]
id = 8314
[taxonomies]
categories = []
tags = []
+++

==Type Safety and run-time errors==
Although I am not doubting that type safety checks can check for some types of errors that might occur at runtime, I am not sure that this is the same as saying that it assures there are ''no'' runtime errors. Errors that occur at run time might include for example:
* Timing errors.
* Randomly induced errors from hardware glitches.
* Resource allocation errors.
* ...
But assuming instant, exact, and infinite resources, then type checking might suffice (also assuming that you can work with the expressiveness of the language). --[[User:Paddy3118|Paddy3118]] 01:31, 14 September 2010 (UTC) 
(P.S. I am embarking on a journey down the path of safety critical systems where I'm currently thinking everything fails, you just have to work out to what degree we should guard against).

:Hi Paddy, if you're contemplating the possible failure of any part of a system then you're in the world of '''system''' safety engineering, which is a large and mature area, but not one that I know much about.

:I only really know about software and any references to freedom from run-time errors that I make refer to properties of the software only.

:With SPARK it's even more restricted than that - claims for freedom from run-time error refer only to properties of the '''source code''', since the SPARK tools only analyze the source text, so there's all the problems of compiler/linker errors to worry about as well. (But Rosetta Code is a source code wiki, so I guess I was taking that as understood.)

:In Ada the language defines 16 possible causes of run-time errors and 11 Of these are eliminated immediately in SPARK by the language design. One of the remaining errors is storage error - this is dealt with by:
:*No programmer access to the dynamic heap
:*Bounded requirements for stack space (the upper bound can be calculated once the stack requirements for each subprogram are known)
:The remaining four errors (index check, range check, division check, overflow check) are what the run-time checks are all about and when I claim that a code example is free of run-time errors I mean that analysis of that source code has shown that none of these errors can occur in that source code.

:Some thoughts on your examples of other possible errors:
:*Timing errors - The main problem here (with modern pipelining processors and multiple cache levels) is calculating realistic values of the worst-case execution times for each program thread (ie values that aren't pessimistic by a few orders of magnitude). However, if you can get sensible wcet values then there are program analysis techniques that check whether program threads will always meet their deadlines.
:*Hardware glitches - Not something that language design can do much about, but all safety-critical systems have multiple levels of built-in test, both at initialization and continuously during program execution - with multiple parallel processors where appropriate and voting on outputs to detect failures by any one processor.  Of course all safety-critical programs should assume that any input from an external device could be incorrect and do whatever is possible to detect failures in sensor inputs.
:*Resource allocation errors - Again this comes back to the overall system design, and the SPARK world is essentially that of embedded systems, where the system designer has full control over the resources that are available to any program.

:On your final point of expressiveness of the language - that is always a problem in this area and people have been designing 'safe subsets' ever since they stopped using Assembler for safety-critical software. There are many areas where SPARK can be criticized as lacking features but we now have 20 years of experience with the language (which has increased in expressiveness and will continue to do so I hope) and we know that we can build major real-world systems with it.--[[User:PhilThornley|PhilThornley]] 10:59, 14 September 2010 (UTC)
