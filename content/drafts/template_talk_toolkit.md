+++
title = "Template talk:Toolkit"
description = ""
date = 2010-02-04T05:40:50Z
aliases = []
[extra]
id = 5404
[taxonomies]
categories = []
tags = []
+++

I've created this template for use with things like [[Gtk+]], [[GLUT]] and [[Qt]], under the assumption that "toolkit" refers to a library which aids in graphical user interface development.  It's been my experience that that's the case, but I want to check myself; Does the term "toolkit" reasonably and/or commonly apply to support libraries outside of GUI development? While it's been my experience that those libraries adopt the term SDK, again, I want to check myself. --[[User:Short Circuit|Michael Mol]] 04:28, 3 February 2010 (UTC)

:Toolkits are particular kinds of libraries that focus on turning the low-level interactions with a GUI drawing system into higher level concepts (widgets/components). The earliest toolkit I know of is Xt/Xaw (Xt provided the glue – plus a bunch of stuff to make things like an object system in plain [[C]] – and Xaw defined widgets on top of it; Xaw was superseded by Motif, thank goodness…) but there may be earlier ones; my knowledge of the state of GUI system development prior to the late '80s is very shaky. –[[User:Dkf|Donal Fellows]] 17:14, 3 February 2010 (UTC)

:I agree about the meaning of 'toolkit', but I, for one, would prefer that we not use this term; I just find it unaesthetic and gratuitously confusing.
:Also, a SDK is something entirely different: a SDK has these two key attributes: 1. It is that which you need to write software ''which interacts with something else in particular'' -- some hardware, some host application, some platform, whatever (e.g. "iPhone SDK", "Java ME SDK", "Source SDK", etc etc). 2. It is ''not just a library'' but also one or more of an IDE (or plugin for an IDE), documentation, header files if applicable, test suites, compilers (especially cross-compilers, for mobile devices, microcontrollers, etc), other toolchain components, etc. An SDK is a big glob of stuff useful to developers, ''almost none of which goes into your program''; it's ''the stuff you need to write/compile/build the program''.  —[[User:Kevin Reid|Kevin Reid]] 00:13, 4 February 2010 (UTC)
:: Since my impression of the usage of the word "toolkit" was apparently not limited to just me, I would strongly prefer to use it in that capacity. As an established term, it stands to have the greatest amount of common familiarity with other documentation and discussion indexed and contained elsewhere, and so it serves well both from the perspective if arriving after perusing other information sources, as well as seeking other information sources after arriving here.

:: While I'd find a debate on the meaning of "SDK" interesting, I don't think this particular talk page is the best place for that. --[[User:Short Circuit|Michael Mol]] 05:40, 4 February 2010 (UTC)
