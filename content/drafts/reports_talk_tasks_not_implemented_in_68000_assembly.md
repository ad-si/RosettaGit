+++
title = "Reports talk:Tasks not implemented in 68000 Assembly"
description = ""
date = 2014-01-21T11:27:07Z
aliases = []
[extra]
id = 17111
[taxonomies]
categories = []
tags = []
+++

What about tasks that are too ''simple'' to be worth considering in a language?  In almost any machine/assembly language, "address of a variable" is just an addressing mode, only a part of a single instruction.  Further, it depends on where you chose to put the variable, which may be arbitrary.  For instance, for the 68000, addressing a register variable is just e.g. '''D1'''. Or the variable could be in random memory, on a heap or stack, etc.  Basically, any memory address or register that the program can read and write could hold a variable.

"... To mark a task as such, add {{omit from|68000 Assembly}}, ..."

Ok, add it ''where?''

What prompts this is task "Add a variable to a class instance at runtime", which is meaningless in languages without built-in classes, as the solution would depend on the actual implementation of classes, which is unspecified.
