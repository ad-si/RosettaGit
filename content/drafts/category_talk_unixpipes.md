+++
title = "Category talk:UnixPipes"
description = ""
date = 2011-08-10T00:56:10Z
aliases = []
[extra]
id = 2805
[taxonomies]
categories = []
tags = []
+++

==[[UNIX Shell]]?==
What is the difference between this "language" and [[UNIX Shell]]? Could we consider it an implementation or dialect instead? --[[User:IanOsgood|IanOsgood]] 08:14, 8 April 2008 (MDT)

It is based on the observation that a typical Unix Shell contains two very different languages with in it. One with an imperative, algol like syntax and semantics. The second is dataflow oriented, and semantics is based on immutable (possibly infinite) sequences.

While shells provide both in the same language, I would like to posit that the embedding of Pipes in unix is similar to embedding of SQL with in PLSQL - i.e. both are essentially different paradigms, and provide very different mechanisms for computation.

It is even more clear if you consider a similar system : CMS pipelines in Rexx. While Rexx is used to define stages, and even utilize the result of CMS pipelines, the pipelines have tottally different syntax and semantics from Rexx and exist indipendentaly of Rexx.
[[User:Rahul|Rahul]] 09:21, 8 April 2008 (MDT)
:It occurred to me...'''sed''' is a stream-operating tool that uses regular expressions, which are themselves turing-complete, and available on every non-embedded Unix platform I've encoutnered.  Is '''sed''' omitted from the tool set? --[[User:Short Circuit|Short Circuit]] 19:59, 15 February 2009 (UTC)
:: Regex are finite automata, by themselves they are not turing complete, however I am not very sure if sed provides enough programming constructs to make it turing complete. It could very well be, so I would need to work on the general description again.

==language==
I question whether this can be defined as a language in the same sense that other languages on Rosetta Code are, but it's certainly a unique and worthwhile approach.  I'm looking forward to seeing how it can be used. --[[User:Short Circuit|Short Circuit]] 20:55, 10 April 2008 (MDT)

== Deleted solutions ==

I deleted the UnixPipes solutions for [[Sockets]] and for [[Hello world/Standard error]], because they were too similar to the UNIX Shell solutions. The number of UnixPipes solutions decreases from 40 to 38. --[[User:Kernigh|Kernigh]] 00:56, 10 August 2011 (UTC)
