+++
title = "Category:Jot"
description = ""
date = 2011-07-28T02:11:40Z
aliases = []
[extra]
id = 9340
[taxonomies]
categories = []
tags = []
+++

{{library}}
<code>jot(1)</code> is a shell command from [[BSD]] that can print a list of numbers. <code>jot(1)</code> is convenient when a [[UNIX Shell]] program needs to iterate a range of numbers. Examples that use <code>jot(1)</code> will not work with other Unix systems that are missing <code>jot(1)</code>.


```bash
# Example: this loop echoes Got 1, Got 2, Got 3.
for i in `jot 3`; do
	echo Got $i
done
```


The syntax is <code>jot count begin end step</code>

All four arguments are optional. A hyphen <code>-</code> skips an argument. Here are some examples.

* <code>jot 5</code> prints 1 2 3 4 5.
* <code>jot 5 10 20</code> prints 10 12 15 18 20.
* <code>jot -p 2 5 10 20</code> prints 10.00 12.50 15.00 17.50 20.00. 
* <code>jot - 3 7 1</code> prints 3 4 5 6 7.
* <code>jot - 7 3 -1</code> prints 7 6 5 4 3.

It has a few other features, like random numbers. For a manual page, see [http://www.openbsd.org/cgi-bin/man.cgi?query=jot&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html jot(1)].
