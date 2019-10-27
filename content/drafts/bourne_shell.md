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

{{stub}}{{implementation|UNIX Shell}}

The '''[[wp:Bourne Shell|Bourne Shell]]''' is a [[Unix]] shell upon which many shells are based; notably the [[wp:Korn shell|Korn shell]] and [[Bourne Again SHell]]. (The other major tree of Unix shells descend from [[csh]].)

'''Portable Shell Syntax''' is the scripting language syntax used by the [[wp:UNIX System V|System V]] Bourne shell. This syntax is compatible with the heirloom shell and is the syntax documented in most Unix books. Examples marked "Works with: Bourne Shell" should work in any of the Bourne-compatible shells.

A Bourne Shell script begins with a [[wp:shebang (Unix)|shebang]] (also known as a ''hashbang'') like this, which tells the operating system to use the Bourne compatible shell interpreter:

 #!/bin/sh

In 2009, [[wp:Computerworld|Computerworld]] published an in-depth interview with Steve Bourne, ''[http://www.computerworld.com.au/article/279011/a-z_programming_languages_bourne_shell_sh/ The A-Z of Programming Languages: Bourne shell, or sh]'', which details the Bourne shell origins and design decisions.

== Bugs ==
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

