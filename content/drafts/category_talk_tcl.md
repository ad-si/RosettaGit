+++
title = "Category talk:Tcl"
description = ""
date = 2010-08-27T10:11:37Z
aliases = []
[extra]
id = 4228
[taxonomies]
categories = []
tags = []
+++

'''Temporary:''' Just so this is easy to find for now. [[Reports:Tasks not implemented in Tcl]]
==Tasks Unlikely to get Implemented==
This is a short discussion of the tasks that are marked with the omit template. —[[User:Dkf|Dkf]] 09:14, 21 May 2009 (UTC)

; [[Parametric Polymorphism]]
: Tcl doesn't have static typing, making this task ridiculously trivial/non-applicable. Note also that the task itself states that it only applies to languages with static typing.
; [[Constrained Genericity]]
: This is a specialization of the Parametric Polymorphism task, so all comments there apply here too.

== Language features ==

Tcl uses a mixture of pass-by-value and pass-by-reference, and is very good at simulating pass-by-name too. The base language semantics are strictly pass-by-value; this was how everything was done up to Tcl 7.6, and when combined with the fact that it was also string based, it gave the language a (deserved) reputation for being slow. In Tcl 8.0 the language implementation was switched to pass-by-reference, with the entities being semantically immutable objects (the actual code is more nuanced than that, of course); that was a major part of why Tcl sped up with that version. The pass-by-name support is through the <code>[http://www.tcl.tk/man/tcl8.6/TclCmd/upvar.htm upvar]</code> command, which allows the looking-up of a variable in one scope and aliasing it to another variable in the current scope. —[[User:Dkf|Dkf]] 10:16, 31 May 2009 (UTC)

I've selected these features:

;<tt>exec=bytecode</tt>
:Tcl's used a bytecode engine (with occasional interpretation) since 8.0, i.e., 1996.
:: OK, we've been discussing compiling to native code for a while now, but we've not got the effort to make that viable across lots of platforms, and going to a common bytecode format like JVM, CIL or LLVM is awkward as they are much more low-level than Tcl; for example, Tcl's variables aren't just simple bits of memory but also have a lot of other things that can be attached off them. —[[User:Dkf|Donal Fellows]] 08:23, 6 August 2009 (UTC)
;<tt>expression=dynamic</tt>
:We use dynamic typing in <code>expr</code>.
;<tt>compat=duck</tt>
:We say it's a <s>duck</s>list if it supports the operations of a <s>duck</s>list.
;<tt>checking=dynamic</tt>
:Our type checks are applied at runtime only. That's when they are enforced strictly.
;<tt>parampass=value</tt>
:We always pass parameters by value. We simulate pass-by-reference by passing handles/names and pass-by-name with the help of <code>upvar</code>.
;<tt>safety=safe</tt>
:The language, especially in a safe interpreter, has no unsafe operations at all.
;<tt>paradigms=Imperative, Object-oriented, Event-driven, Reflective, Concurrent</tt>
:Tcl supports all of these handily enough. With more detail/justification:
:;<tt>Concurrent</tt>
::The Thread extension is long-established.
:;<tt>Event-driven</tt>
::While [[Tk]] has always been event driven, Tcl has been since 7.5 or 7.6 when it gained the event loop from Tk.
:;<tt>Imperative</tt>
::Tcl is definitely an imperative language.
:;<tt>OO</tt>
::Tcl supports this through many extensions, and natively from 8.6.
:;<tt>Reflective</tt>
::Tcl's had introspection for ages; it's vital for the language's self-tests.

We need to check whether these features are enough; if not, we should update the Language template... —[[User:Dkf|Donal Fellows]] 12:26, 1 June 2009 (UTC)


###  How to use? 

What is the correct command to start the examples? I tried 'tclsh' or 'wish', like
   $ tclsh <fileWithTheCode>'
but didn't succeed for many of the examples. I use version 8.4 under Debian Squeeze, and I'm aware that all those examples that need a higher version of Tcl won't run. But if I try, for example, the "GUI component interaction" task, I get
   invalid command name "ttk::frame"
      while executing
   "ttk::frame .bg"
If I try "Extend your language", I get
   missing close-bracket
      while compiling
   "set c1 [uplevel 1 [list expr $cond1]
What am I doing wrong? Is 'tclsh' the right command? --[[User:Abu|Abu]] 10:11, 27 August 2010 (UTC)
