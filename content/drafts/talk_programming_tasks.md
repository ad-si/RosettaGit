+++
title = "Talk:Programming tasks"
description = ""
date = 2007-01-11T00:28:36Z
aliases = []
[extra]
id = 1506
[taxonomies]
categories = []
tags = []
+++

"I'm around.  For the record, operating system should be an 'optional' addition.  In most languages, there is a compiler available in every popular operating system, and since we won't be dealing with already compiled binaries, OS shouldn't be a necessity.  Especially since most modern languages (.Net and Java) use VMs.

You should probably add an additional parameter for GUIs.  A 'normal' GUI and a 'web' GUI.

I'm around to help out if needed."

-FK
:I can see the argument for removing the '''Operating System''' requirement.  When I included it, I had in mind behavior changes revolving around filesystem differences between DOS/Windows platforms and UNIX platforms.  It might make sense to replace '''Operating System''' with '''Platform'''; one is a more generic representation of the other, and allows for virtual machines.
:As for GUIs, "normal" GUIs and "web" GUIs are one and the same; they just use different widgets, and enforce a different style of GUI behavior.  Normal GUIs can perform operations on the same form, while web GUIs may be limited to dialog-style behavior, depending on whether a server-side or client-side language is used. --[[User:Short Circuit|Short Circuit]] 09:54, 8 January 2007 (PST)
::On second thought, you're probably right.  Web-based GUIs can be created just using HTML, no programming required.  In those cases, it makes more sense to demonstrate accessing GUI elements than the creation of the elements themselves. --[[User:Short Circuit|Short Circuit]] 12:04, 8 January 2007 (PST)

Code should be as simple as possible, unless the language is purposely obfuscated - i.e. obfuscated C.  I agree with FK's recommendation that we go ahead and drop the OS requirement as coding simple structures in most languages are independent of the Operating System.  In the instances where we are talking about a particular OS that can be noted.

[[User:SiliconJesus|SiliconJesus]]

== Event vs Error handling ==

Should event handling and error handling be merged into the same article? --[[User:Short Circuit|Short Circuit]] 08:51, 9 January 2007 (PST)
 Stderr and Stdout are two different interfaces with two different reasons for being --[[User:SiliconJesus|SiliconJesus]] 10:57, 9 January 2007 (PST)
::Hm.  When I was considering "Error handling", I was thinking of the code required to trap runtime errors, rather than methods of reporting them. --[[User:Short Circuit|Short Circuit]] 11:02, 9 January 2007 (PST)

Event and Error handling are two different things in my mind --[[User:FortKnox|FortKnox]] 16:10, 10 January 2007 (PST)

:The question to me seems to be one of definition.  I can define a programming task where the programmer needs to catch an event.  However, how would one define an "error handling" programming task?  What kind of error is one handling?

:If someone wants to define a programming task for event handling, feel free.  I'm restructuring the Programming Tasks page somewhat.  It might even be deprecated at some point.  The [[:Category:Programming Tasks|programming tasks category]] will probably become the crossroads for finding programming tasks. --[[User:Short Circuit|Short Circuit]] 16:28, 10 January 2007 (PST)
