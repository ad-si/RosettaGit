+++
title = "Gawk"
description = ""
date = 2011-08-14T23:06:43Z
aliases = []
[extra]
id = 2287
[taxonomies]
categories = []
tags = []
+++

{{implementation|AWK}}
''gawk'' is [[GNU]] awk, the first free AWK implementation. ''gawk'' is compatible with [[nawk]]. ''gawk'' also adds several extensions to its language.

Many systems, and most GNU/[[Linux]] distros, use ''gawk'' as their default ''awk''. (Older [[BSD]] systems also used ''gawk'' as their default ''awk'', until newer BSD systems switched to ''[[nawk]]''.) ''gawk'' was the only free ''awk'', until the 1991 release of ''[[mawk]]''.

== Gawkisms ==
A "gawkism" is an extension that works with ''gawk'', but not with other AWK variants. Gawk's own manual identifies GNU extensions, so programmers know which features require ''gawk''.

Some gawkisms are

* GNU-style regular expressions.
* extra built-in functions, like <code>asort()</code> and <code>gensub()</code>.
* [[Korn Shell|Korn]]-style coprocesses.
* TCP and UDP networking with "/inet/".
* ''(since gawk 4.0.0)'' arrays of arrays, like <code>ary[2][3] = 4</code>.
* ''(since gawk 4.0.0)'' indirect function calls, like <code>str = "foo"; @str() # calls foo()</code>.
* other GNU extensions in the manual.

== Links ==
* Home page: [http://www.gnu.org/software/gawk/ gawk]
* Manual: [http://www.gnu.org/software/gawk/manual/ ''Gawk: Effective AWK Programming'']
