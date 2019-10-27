+++
title = "Template talk:Uses from"
description = ""
date = 2010-11-20T04:34:51Z
aliases = []
[extra]
id = 8776
[taxonomies]
categories = []
tags = []
+++

== Broken things  ==

* Doesn't handle component names like "Gtk::Main".--[[User:Short Circuit|Michael Mol]] 04:34, 20 November 2010 (UTC)
* Ideal use for, e.g. gcc would be <nowiki>{{uses from|GCC|component1=gcc}}</nowiki>, as GCC is the Gnu Compiler Collection, and gcc is the C compiler in that collection. Problem is, it would put that under [[Library/GCC/gcc]], and I don't know if the term "Library" is appropriately generic in this context. (At some linguistic level, you can have a "library" of tools, just as you have a "library" of books, but that's probably too misleading in this context.) I think the likely solution will be to allow a named parameter "implementation" whose value can take the place of {{{1}}}, and which switches the behavior of the template from placing things at [[Library/{{{1}}}]] to [[Implementation/{{{1}}}]]. Either that, or change the semantics of the template so that it places things at [[{{{1}}}|{{{2}}}|{{{componentN}}}]], and {{{2}}} would be the name of the library/collection/whatever. --[[User:Short Circuit|Michael Mol]] 04:34, 20 November 2010 (UTC)
