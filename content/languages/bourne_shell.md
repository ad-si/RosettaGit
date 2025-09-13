+++
title = "Bourne Shell"
description = ""
date = 2012-03-02T18:24:42Z
aliases = []
[extra]
id = 1666
[taxonomies]
categories = []
tags = []
+++



The '''[Bourne Shell](https://en.wikipedia.org/wiki/Bourne_Shell)''' is a [Unix](https://rosettacode.org/wiki/Unix) shell upon which many shells are based; notably the [Korn shell](https://en.wikipedia.org/wiki/Korn_shell) and [Bourne Again SHell](https://rosettacode.org/wiki/Bourne_Again_SHell). (The other major tree of Unix shells descend from [csh](https://rosettacode.org/wiki/csh).)

'''Portable Shell Syntax''' is the scripting language syntax used by the [System V](https://en.wikipedia.org/wiki/UNIX_System_V) Bourne shell. This syntax is compatible with the heirloom shell and is the syntax documented in most Unix books. Examples marked "Works with: Bourne Shell" should work in any of the Bourne-compatible shells.

A Bourne Shell script begins with a [shebang](https://en.wikipedia.org/wiki/shebang_(Unix)) (also known as a ''hashbang'') like this, which tells the operating system to use the Bourne compatible shell interpreter:

 #!/bin/sh

In 2009, [Computerworld](https://en.wikipedia.org/wiki/Computerworld) published an in-depth interview with Steve Bourne, ''[http://www.computerworld.com.au/article/279011/a-z_programming_languages_bourne_shell_sh/ The A-Z of Programming Languages: Bourne shell, or sh]'', which details the Bourne shell origins and design decisions.

## Bugs
Bourne Shell and Heirloom Shell have problems with here documents. Here is one such problem. A substitution, inside a here document, inside backquotes, inside double quotes, does insert too many backslashes.


```bash
f() {
	cat <<!
here $1
!
}

expr "`f string`"
# Output from Bourne Shell:     here \s\t\r\i\n\g
# Correct output:               here string
```


The workaround is to move the backquotes to an assignment.


```bash
f() {
	cat <<!
here $1
!
}

var=`f string`
expr "$var"
# Output:     here string
```

