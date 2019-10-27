+++
title = "Category:Bordeaux Threads"
description = ""
date = 2009-08-11T14:14:21Z
aliases = []
[extra]
id = 4638
[taxonomies]
categories = []
tags = []
+++

{{library}}

Bordeaux-Threads is a Common Lisp library providing threading functionality across currently existing libraries. Threading isn't specified in the ANSI CL standard, therefore it's necessary to access implementation-specific functionality.

One common thing left unspecified as well is how special variables behave in relation to threading. Despite no mention in the standard, all implementations implementing threading treat special variables as thread-local variables [1] with some of them providing also truly global variables (see SBCL's <code>SB-EXT:DEFGLOBAL</code> for instance).

More information can be found at the [http://common-lisp.net/project/bordeaux-threads/ project page].

Other library implementing threading is Portable-Threads, but it has omissions, e.g. there's no way to manually acquire or release locks without a <code>WITH-LOCK</code> macro or acquire them non-blockingly.

[1] But this gets tricky sometimes, e.g. there can only be 8192 thread-local variables ever created in SBCL, so using gensyms with <code>PROGV</code> is out of question. Also there's no way to garbage-collect thread-local bindings, so it doesn't matter if you only use few gensyms in <code>PROGV</code> at a time.
