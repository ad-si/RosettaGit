+++
title = "Talk:Deepcopy"
description = ""
date = 2011-08-23T11:32:44Z
aliases = []
[extra]
id = 10111
[taxonomies]
categories = []
tags = []
+++

== Time to remove draft ==
It looks like it's stable.  I'll leave it a few more days for comments and then promote it.  --[[User:Dgamey|Dgamey]] 11:32, 23 August 2011 (UTC)

==Provide reference and source==
I don't mind the 'provide reference', but please note that Rosetta Code has limitations on what it can host, for copyright reasons. Link to where to get the library, and the source where applicable... --[[User:Short Circuit|Michael Mol]] 02:56, 18 July 2011 (UTC)
: Not sure what to say here.  There was a debate like this on MD5 and MD5 implementation - I was trying to avoid a situation like that. Although this isn't quite the same situation.  Basically, if it's built in jobs done.  If it isn't and it's written in the language itself, I'd like to see how it works.  If it's copyright then a reference would be fine.  --[[User:Dgamey|Dgamey]] 13:21, 18 July 2011 (UTC)
:: Reasonable. If the library is written in the language, it might be appropriate to demonstrate the mechanisms and principles on which it operates, rather than providing a fully-functional example. --[[User:Short Circuit|Michael Mol]] 14:10, 18 July 2011 (UTC)
: As a general rule, I think it is better not to specify in such detail, and let the example authors document their examples as they see fit. (Or perhaps for this case, “Explain how the deep copy works, such as by showing its source.” — make it into another point-that-must-be-covered.) —[[User:Kevin Reid|Kevin Reid]] 14:10, 18 July 2011 (UTC)

Mod Candidate#1
: Original text:  "This can be via a built-in procedure or operator, a function from a common library (provide reference and source if it is written in the language), or a procedure (show source)."
: Updated text: This can be via a built-in procedure or operator, a function from a common library (provide reference/link), or a procedure written in the language itself.  
:* If this is written in the language, describe or show how this works (possibly by including source if copyrights allow or by other means).
: --[[User:Dgamey|Dgamey]] 01:28, 19 July 2011 (UTC)

Mod Candidate#2 (per Kevin Reed's comments below):
: Original text: Demonstrate how your language handles complete copying, often referred to as deep copying, of complex data structures. 
: Updated text (append to above):  Deepcopying is normally required where structures are mutable and to ensure that independent copies can be manipulated without side-effect.  
: --[[User:Dgamey|Dgamey]] 01:28, 19 July 2011 (UTC)

==Tcl==

# The point of deep copying, as I see it, is to make independent mutable (or identity-bearing) objects (can we work this into the task description?), so I think the first part of the Tcl example should be just removed as there is no point.
:: Your text is consistent with the intent as I saw it and could be added to the description.  (I won't comment on the TCL part). --[[User:Dgamey|Dgamey]] 01:21, 19 July 2011 (UTC)
# It isn't made clear that oo::copy is (or isn't) a ''deep'' copy of object graphs; I request an explanation of what oo::copy copies, per the task requirements.
—[[User:Kevin Reid|Kevin Reid]] 22:50, 18 July 2011 (UTC)
