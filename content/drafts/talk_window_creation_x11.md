+++
title = "Talk:Window creation/X11"
description = ""
date = 2010-03-10T10:52:39Z
aliases = []
[extra]
id = 3121
[taxonomies]
categories = []
tags = []
+++

==Classification==
These examples should be included under [[User Output - graphical]]. --[[User:Short Circuit|Short Circuit]] 01:59, 18 November 2008 (UTC)
:They should also (obviously) be changed to display "Goodbye, World!"--[[User:Mwn3d|Mwn3d]] 02:27, 18 November 2008 (UTC)
:: Currently there is a problem with the page [[User Output - graphical]], for most languages you can fill the task with GTK or Qt or Tk or Xlib or still other things so if each lib was filled for each language the page would become too large.
:: Also as the task does not tell to give one example by available GUI lib, the contributors have chose one or two, but don't provide something exhaustive.
:: So IMO, the page [[User Output - graphical]] should be split by GUI libs, and if you wish to keep this page around, for each language give a link until the available sub-examples, for example for OCaml I would give a link to this current page [[Xlib_simple_window#OCaml]] and also [[Tk_simple_window#OCaml]] and [[GTK_simple_window#OCaml]]...
:: [[User:Blue Prawn|Blue Prawn]] 18:42, 18 November 2008 (UTC)
:: Pages growing too large is actually not a bad thing.  Once it happens, we can look at a page with a large amount of content and decide on an appropriate way to split up that content.  We don't have a large enough sample of examples to make that decision yet for [[User Output - graphical]].  Incidentally, and for historical perspective, it was already split once.  The original task was [[User Output]].  That task was split between GUI and console forms of output. --[[User:Short Circuit|Short Circuit]] 01:41, 21 November 2008 (UTC)
:: I'm not sure what you mean by your second point.  The task gives a simple requirement, and people who provide examples can use as many or few libraries per given language as they need.  In my opinion, library comparison is valuable; Just as one can use RC to leverage their knowledge of one language to learn another, they can, in some cases, use RC to leverage their knowledge of one library to learn another.  The task is deliberately vague so as not to discourage the use of a variety of languages. --[[User:Short Circuit|Short Circuit]] 01:41, 21 November 2008 (UTC)
:::That page is too simple a task to split up by library I think. If I just want to know how to make a window in a language, I don't care what library I use. Splitting it by library might also make the pages too small if one language has a library available to it that another doesn't. I think it works fine to have the examples marked with the libraries they use just so that all of the examples for one language are in one place. --[[User:Mwn3d|Mwn3d]] 20:08, 18 November 2008 (UTC)
:::: OK, but you should admit that creating a window with GTK or the Xlib are 2 things very different. The area of use are even at the opposite! [[User:Blue Prawn|Blue Prawn]] 20:48, 18 November 2008 (UTC)
:::::They should probably be in different examples if they're so different (I've never used either...Java programmers have Swing and AWT), but if they accomplish the same task they should be in the same task.--[[User:Mwn3d|Mwn3d]] 20:59, 18 November 2008 (UTC)
:::::: The problem is caused by the fact that for C and C++, unlike for Java, there's no standard library for GUI, anf there's a large set of different GUI libraries, which all differ in supported operating systems, supported desktop environments, license, ...
:::::: To just give a "short", non-comprehensive list of GUI toolkits usable from C++: raw Windows API, Xlib, Carbon, MFC, Xt, Motif, Gtk+, Qt, wxWidgets, Ultimate++, ...
:::::: Of course, one could restrict to the basic GUI facility of the corresponding platform, i.e. raw Windows API for Windows, Xlib for Linux/Unix (AFAIK X is the only relevant windowing system on Unix-type systems) etc. But then, most non-trivial programs are not written using those low-level APIs (well, for X, in principle one could even go one level lower and explicitly use the X protocol to communicate with the server, but that's even more unlikely to be used in any serious program). --[[User:Ce|Ce]] 13:46, 19 November 2008 (UTC)
::::::: The other relevant low-level API for Unix is the native one for OSX. Mind you, that's one that Apple strongly discourages developers from using. Instead, they go with using Cocoa which is much higher level. –[[User:Dkf|Donal Fellows]] 10:52, 10 March 2010 (UTC)

Just seen that there is [https://jna.dev.java.net/javadoc/com/sun/jna/examples/unix/package-tree.html X for Java] too. [[User:Blue Prawn|Blue Prawn]] 20:23, 29 November 2008 (UTC)

==Tcl example==

I think the Tcl example is a bit far from the spirit of this task as it isn't operating on X11 entities but rather using Tk. Your thoughts? --[[User:Kevin Reid|Kevin Reid]] 00:54, 3 August 2009 (UTC)
:Who in their right mind wants to use anything but a nice wrapper around X? --[[User:Paddy3118|Paddy3118]] 17:22, 3 August 2009 (UTC)
::I'd say you're disputing the premise of the task...:-)
::Perhaps the examples in this task should be folded into [[Creating a Window]] as variants? The question for the fate of this task is, I think: will anyone be informed by seeing a comparison of creating ''X11'', specifically, windows? Comparison of the different approaches [[:Category:Xlib|Xlib]] and [[:Category:CLX|CLX]] take as X11 protocol bindings in the [[X11 simple window#C|#C]] and [[X11 simple window#Common Lisp|Common Lisp]] might be informative, for example.
::I propose that we leave this task as it is for now, and see what happens.
::--[[User:Kevin Reid|Kevin Reid]] 18:52, 3 August 2009 (UTC)
: "such as xlib" is the exact wording of the task at the moment, which seems to allow wrapper libraries. --[[User:Short Circuit|Short Circuit]] 03:17, 15 August 2009 (UTC)
::I made that change. My intent for it was neutral with regard to the Tk issue, but rather to make not-incorrect those examples which use an X11 ''protocol library'' which is ''not'' Xlib. Tk is not an X11 protocol library, and it is also not Xlib, so the change is irrelevant. --[[User:Kevin Reid|Kevin Reid]] 03:47, 15 August 2009 (UTC)
::: So is the concern that over-abstraction of native Xlib types hides the internals of the protocol? --[[User:Short Circuit|Short Circuit]] 08:36, 15 August 2009 (UTC)
:::: '''NOT XLIB, X11!''' My view is that the distinct feature of this task as opposed to [[Creating a Window]] and all the other GUI tasks is that it is about working in the concepts defined by ''the X11 protocol''. If you're using Tk, you're using Tk's concepts rather than X11's (as far as I know). Unfortunately, as I don't ''really'' know X11 myself, I can't give good specific examples. --[[User:Kevin Reid|Kevin Reid]] 11:54, 15 August 2009 (UTC)
::::: The documentation for the native X11 protocol (which yes, I have read) states clearly that this is not a level intended for use in normal user programs. The documentation for Xlib ''also'' states that it is a layer that should be wrapped up by higher-level toolkits. Admittedly, when that was written they were thinking in terms of Xaw or perhaps Motif (or any other toolkit based on top of Xt; google it if you want the gruesome details) but Tk, GTK, Qt, wxWidgets, etc. all absolutely fit into that space envisaged. If you're writing raw X (Xlib, Xcb or the protocol itself) and you're not doing so because you're writing a toolkit, [http://xkcd.com/463/ you're doing something horribly wrong]. I guess this means I'm disputing the premise of this task on one level, yes, but it also means that I'm disputing the refusal to let people use toolkits to address it; that's how X11 was ''designed to work from the beginning''. I don't talk raw X11 for the same reason I don't write raw machine code, and I avoid raw Xlib as much as I can just like I try to not get down to the level of writing in assembly language. I have higher-level tools, and damnit I'm going to use them! --[[User:Dkf|Donal Fellows]] <small>(on a machine not his own, so not logging in)</small>
::::: To be exact, '''is it the intention of this task to prohibit the use of toolkits?''' If so, it ''must'' be stated clearly in the task definition. —[[User:Dkf|Donal Fellows]] 15:27, 17 August 2009 (UTC)
:::::: Given how people have been solving this task, I've specialized it to say that toolkits shouldn't be used. This invalidates none of the existing solutions. (I've written one in Tcl which lets me show off a bunch of cool things, given that we don't have a standard lib for this low a level of access.) I still think that a toolkit is the ''right'' way to do this thing in production code, but this task is about doing it the hard way. —[[User:Dkf|Donal Fellows]] 23:23, 21 August 2009 (UTC)

== Back on the subject of task specificity ==

I don't care to argue against the task existing as it is, but as specific as its intent is (and looking back on past discussion) I'd like to see one of two things: Either rename the task to [[Window creation/X11 (native)]], and specify clearly that the task is intended to describe working with X11 system objects directly, or fold it back into a more generic task. This task is specific in intent, but not sufficiently clear on what that intent is; if a task creator has to disproportionately step in to moderate examples, then the task isn't clear enough. --[[User:Short Circuit|Michael Mol]] 04:41, 10 March 2010 (UTC)
