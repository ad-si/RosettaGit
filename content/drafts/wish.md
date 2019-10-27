+++
title = "Wish"
description = ""
date = 2010-10-04T09:01:42Z
aliases = []
[extra]
id = 1583
[taxonomies]
categories = []
tags = []
+++

{{implementation|Tcl}}'''wish''' is an interface  for the [[Tcl]] programming language interpreter that provides graphical user interface support that can be scripted with [[libtcl]][[Category:libtcl]].  Wish loads the [[Tk]] library by default; it is virtually identical to [[tclsh]] after the execution of the command:

```tcl>package require Tk</lang

The minor differences have to do with how the program interacts with the user, especially on [[Windows]].

Wish takes a number of arguments itself (in addition to the <code>-encoding</code> option that tclsh supports) which it uses as options when creating the initial <code>toplevel</code> window (<code>.</code>), which are described on the [http://www.tcl.tk/man/tcl8.5/UserCmd/wish.htm#M4 wish](1) manual page. The most useful ones on modern [[:Category:Xlib|X11]]-based desktops are:

;-display ''display''
:Display (and screen) on which to display window.
;-geometry ''geometry''
:Initial geometry to use for window. If this option is specified, its value is stored in the <code>geometry</code> global variable of the application's Tcl interpreter.
;-name ''name''
:Use ''name'' as the title to be displayed in the window, and as the name of the interpreter for <code>send</code> commands.
;-use ''id''
:Specifies that the main window for the application is to be embedded in the window whose identifier is ''id'', instead of being created as an independent toplevel window.
;--
:Pass all remaining arguments through to the script's <code>argv</code> variable without interpreting them. This provides a mechanism for passing arguments such as <code>-name</code> to a script instead of having wish interpret them.

The '''-display''' option is not useful when Tk is not built for X11; other display systems (Windows, MacOS X native) always work locally. (This also applies to all of the wish-specific options not listed above, but they're also largely irrelevant on X11 systems too, even if they are supported there.)

Wish ceases running when the event loop becomes idle and there are no windows to display (including withdrawn windows).
