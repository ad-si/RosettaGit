+++
title = "Korn Shell"
description = ""
date = 2014-10-16T16:29:18Z
aliases = []
[extra]
id = 4794
[taxonomies]
categories = []
tags = []
+++

{{implementation|UNIX Shell}}
'''Korn Shell''', or ''ksh'', is the creation of David Korn at AT&T. This shell combines [[derived from::compatible with::Bourne Shell]] syntax with a command-line editor, command history, tilde expansion, arithmetic expressions, arrays, coprocesses and several more features. Korn Shell has influenced many later shells; [[Public Domain Korn Shell]] and [[Z Shell]] clone several features, and the X/Open and POSIX standards take a few features from Korn Shell. David Korn continues to maintain [[ksh93]], the original implementation.

AT&T freed ksh93 during 2000, using an open-source license. For many years before that, the original Korn Shell was not free; it was only part of AT&T System V and some commercial Unix variants. Therefore, ''ksh'' in some systems is not David Korn's shell, but is some other shell, perhaps ''pdksh'' or [[MirBSD Korn Shell|''mksh'']].

== Which Korn Shell do I have? ==
Start ''ksh'' and run 


```bash
$ echo $KSH_VERSION
```


* If the output looks like <code style="background: yellow;">Version JM 93u 2011-02-08</code>, then you have ''[[ksh93]]''.
** Version AJM 93u+ 2012-08-01
* If the output looks like <code style="background: yellow;">@(#)PD KSH v5.2.14 99/07/13.2</code>, then you have ''[[pdksh]]''.
* If the output looks like <code style="background: yellow;">@(#)MIRBSD KSH R49 2014/01/11</code>, then you have ''[[mksh]]''.
* A ''[[zsh]]'' invoked as ''ksh'' sets ZSH_VERSION, not KSH_VERSION.

== Links ==
* [http://www.kornshell.com/ Home Page For The KornShell Command And Programming Language]
* [[wp:korn shell]]
