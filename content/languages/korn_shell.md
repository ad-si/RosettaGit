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
'''Korn Shell''', or ''ksh'', is the creation of David Korn at AT&T. This shell combines [derived from::compatible with::Bourne Shell](https://rosettacode.org/wiki/derived_from::compatible_with::Bourne_Shell) syntax with a command-line editor, command history, tilde expansion, arithmetic expressions, arrays, coprocesses and several more features. Korn Shell has influenced many later shells; [Public Domain Korn Shell](https://rosettacode.org/wiki/Public_Domain_Korn_Shell) and [Z Shell](https://rosettacode.org/wiki/Z_Shell) clone several features, and the X/Open and POSIX standards take a few features from Korn Shell. David Korn continues to maintain [ksh93](https://rosettacode.org/wiki/ksh93), the original implementation.

AT&T freed ksh93 during 2000, using an open-source license. For many years before that, the original Korn Shell was not free; it was only part of AT&T System V and some commercial Unix variants. Therefore, ''ksh'' in some systems is not David Korn's shell, but is some other shell, perhaps ''pdksh'' or [''mksh''](https://rosettacode.org/wiki/MirBSD_Korn_Shell).

## Which Korn Shell do I have?
Start ''ksh'' and run 


```bash
$ echo $KSH_VERSION
```


* If the output looks like <code style="background: yellow;">Version JM 93u 2011-02-08</code>, then you have ''[ksh93](https://rosettacode.org/wiki/ksh93)''.
** Version AJM 93u+ 2012-08-01
* If the output looks like <code style="background: yellow;">@(#)PD KSH v5.2.14 99/07/13.2</code>, then you have ''[pdksh](https://rosettacode.org/wiki/pdksh)''.
* If the output looks like <code style="background: yellow;">@(#)MIRBSD KSH R49 2014/01/11</code>, then you have ''[mksh](https://rosettacode.org/wiki/mksh)''.
* A ''[zsh](https://rosettacode.org/wiki/zsh)'' invoked as ''ksh'' sets ZSH_VERSION, not KSH_VERSION.

## Links
* [http://www.kornshell.com/ Home Page For The KornShell Command And Programming Language]
* [wp:korn shell](https://en.wikipedia.org/wiki/korn_shell)
