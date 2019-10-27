+++
title = "Talk:Terminal control/Cursor movement"
description = ""
date = 2012-08-17T21:11:43Z
aliases = []
[extra]
id = 12233
[taxonomies]
categories = []
tags = []
+++

==task clarification==

It's not stated, but I assume if the curson is at a terminal screen edge, and we want to move it past the edge, that it is assumed that we are to move to the cursor to the other side of the screen (that is, wrap it)? -- [[User:Gerard Schildberger|Gerard Schildberger]] 09:30, 16 August 2012 (UTC)
:As you say, it's not stated in the task. Perhaps some clarification is in order.
:Personally, I would do whatever is typical for your language of choice. (For example, this isn't a problem for BASIC, normally -- in MS BASIC, an attempt to place the text cursor off the screen using <code>LOCATE</code> results in an error ("Illegal function call"). The graphical cursor (which serves a completely different purpose) continues on to (effectively) infinity.)

:If there's no "standard" behavior for your chosen language... shrug. Do as you see fit. -- [[User:Eriksiers|Erik Siers]] 11:53, 16 August 2012 (UTC)

:: Well, actually, the '''CURSOR''' bif in PC/REXX (and Personal REXX) interpreter doesn't ''move'' the cursor, it ''places'' it.  Any movement on the REXX program's part is actually performed by arithmetic and then '''CURSOR''' just places (or moves, it you prefer) the cursor to the location specified.  I saw no reason to incourage a program failure by ensuring a '''SYNTAX''' error (illegal function call) in the REXX program, so I took it upon myself to do what I would expect a smart terminal to do, that is: wrap. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:21, 16 August 2012 (UTC)

: This isn't the only ambiguity in this task.  Consider J, for example, where "terminal" might mean:  a unix tty, a windows cmd instance, an emacs shell buffer, a browser session (html with javascript), a gtk application specifically designed to provide a development environment for j, or an older java application specifically designed to provide a development environment for j.  As all of these are represented in roughly equal measure, I personally have been mostly ignoring the "terminal" tasks -- they mostly do not make sense to me.  Sometimes, for amusement value, I'll arbitrarily pick a context and provide an implementation for it, but I'm pretty much guaranteed that whatever I pick will represent a small minority of J instances.  Maybe, here, I should illustrate how to implement an emacs hook in a shell buffer, for terminal positioning... --[[User:Rdm|Rdm]] 21:11, 17 August 2012 (UTC)
