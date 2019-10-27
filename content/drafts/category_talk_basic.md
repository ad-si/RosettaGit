+++
title = "Category talk:BASIC"
description = ""
date = 2011-12-02T05:02:12Z
aliases = []
[extra]
id = 10986
[taxonomies]
categories = []
tags = []
+++

==Major reorganization needed?==

I've given some thought, and it seems to me that the way BASIC is handled needs some reorganization here. For starters, read [http://irclog.perlgeek.de/rosettacode/2011-12-01#i_4778078 this IRC log].

A big part of the problem is that BASIC isn't ''really'' standardized. Sure, there's an ANSI/ISO/IEC/etc. standard (or if you don't feel like paying for it, the withdrawn standards from [[wp:ECMA|ECMA]] are close enough: [http://www.ecma-international.org/publications/files/ECMA-ST-WITHDRAWN/ECMA-55,%201st%20Edition,%20January%201978.pdf ECMA-55, Minimal BASIC] and [http://www.ecma-international.org/publications/files/ECMA-ST-WITHDRAWN/ECMA-116,%201st%20edition,%20June%201986.pdf ECMA-116, BASIC]), but those are ignored by the vast majority of BASICs out there today.

This has led to some vastly different languages being called "BASIC". (How different? Different enough that some BASICs don't even recognize the same comment characters (see [[Comments#BASIC]] for that).) This has led to some BASICs being given their own sections in task pages, while others are grouped under BASIC, like this:
 ==BASIC==
 <nowiki>{{works with|BASICA}}</nowiki>
 ...
 ==PowerBASIC==
 ...
 ==PureBasic==
 ...

In the above IRC discussion, it was suggested that '''all''' BASIC examples should be moved from top-level sections to subsections of BASIC, like so:
 ==BASIC==
 
### BASICA

 ...
 
### PowerBASIC

 ...
 
### PureBasic

 ...

On the one hand, this makes sense to me. On the other hand, this will require editing ''every single BASIC solution on RC''.
:Edit: including all solutions from [[BBC BASIC]], [[PowerBASIC]], [[PureBasic]], [[VB]], etc. -- [[User:Eriksiers|Erik Siers]] 22:03, 1 December 2011 (UTC)

So, I'm looking for something resembling a consensus: should this reorganization be carried out? Or no? Something else? -- [[User:Eriksiers|Erik Siers]] 22:00, 1 December 2011 (UTC)

: Decide this language by language. Frog BASIC might want to be a subsection of BASIC, but Toad BASIC might want to be a separate language. For example, [[TI-83 BASIC]] should remain separate because it is not BASIC, it only has a few similar statements (If, Then, For, Goto, Input). Some other languages, like [[Visual Basic .NET]] are weird but might fit as BASIC subsections.

: I now have [[C Shell]] and [[es]] as subsections of [[UNIX Shell]]; this works because I am the only author of csh/es solutions, so I always put the sections in the correct place. --[[User:Kernigh|Kernigh]] 05:02, 2 December 2011 (UTC)
