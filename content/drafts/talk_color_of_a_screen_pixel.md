+++
title = "Talk:Color of a screen pixel"
description = ""
date = 2010-07-02T05:54:25Z
aliases = []
[extra]
id = 4274
[taxonomies]
categories = []
tags = []
+++

This task was originally contributed by the [[AutoHotkey]] community. [[Category:AutoHotkey_Originated]]
----
I propose to clarify the task. In an operating system there can be many screens and many mice and other user output/input raster devices etc. Further under a properly designing operating system you just cannot access an arbitrary pixel on the screen for obvious security reasons. I suppose this task has rather something to do with a certain GUI framework, or, maybe, graphical system? How to get a pixel in X11, Windows API, .NET, OpenGL, GDK, GRAFOR ... infinite list? makes no sense to me. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:38, 28 May 2009 (UTC)
:It seems that the language originating a number of tasks recently is not very respective of security. Something of a worry...
:OTOH, we could just redefine the task to something that more languages can do with common utilities/libraries and ignore the fact that this will cause the originator pain. —[[User:Dkf|Dkf]] 10:53, 28 May 2009 (UTC)

:just like any other task, this is based on the typical running environment for your program.  However, I have added the line:  "The mouse cursor may or may not have to be active in a gui created by your program."  —[[User:tinku99|tinku99]]
:: So what happens when the typical running environment for a program is an emacs comint buffer?  Or a web browser session?  (I am thinking that this, like some other tasks here, has very heavy dependencies on the hosting environment.  Perhaps such tasks should get a special category, and perhaps such tasks warrant multiple implementations based on different host environments for languages which do not abstract this kind of issue.)  --[[User:Rdm|Rdm]] 18:38, 10 June 2010 (UTC)
::: If code has dependencies beyond what's guaranteed by the core language, [[Template:works with]] and [[Template:library]] are used to identify them. As far as it helps illuminate comparative relevant use of platform, implementations, libraries, etc., I very much ''like'' to see multiple code examples per language/task pair. The site software isn't very well constructed to make, e.g. comparison-by-library easy, but that is one of my ultimate aims. --[[User:Short Circuit|Michael Mol]] 22:52, 10 June 2010 (UTC)

Getting pixel information is a basic task related to "accessibility", "testing", and just "getting things done".  Thats what programming is for in the first place.  Its not a security problem if your are the owner of the computer, and that is what you want to do.  If a particular language can not do the task, it should be put in its ignore list.  The task should not be modified to fit that language.  —[[User:tinku99|tinku99]]

: I believe every lang able to use OS API should be able; this creates anyway a "strong" system dependency, unless one can achieve the task using portable toolkit; e.g. GTK (GDK exactly?) should make it possible, and the solution should work on every system with GTK...? (Maybe!) If a system allows the existence of a "snapshot" software (to take picture of the whole screen or of a particular window), then this task is not unsecure. --[[User:ShinTakezou|ShinTakezou]] 10:49, 30 May 2009 (UTC)
:: Whoops seen now the Tcl solution using xwd to take a snapshot! I think it's a good solution.= --[[User:ShinTakezou|ShinTakezou]] 10:51, 30 May 2009 (UTC)

=MUMPS=
I elected to omit MUMPS from the task, even though somebody else somewhere may be able to write code that performs this task. The implementations I am used to work on character cell screens (yes it is an old language). I do have the $X and $Y variables that tell me (mostly) where the cursor is on the screen, but I'm not even sure I can read the device to get what character is there - and I'm fairly certain I can't get color information. [[User:Stormneedle|Stormneedle]] 05:54, 2 July 2010 (UTC)
