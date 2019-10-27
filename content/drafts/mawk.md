+++
title = "Mawk"
description = ""
date = 2011-07-26T23:05:13Z
aliases = []
[extra]
id = 10158
[taxonomies]
categories = []
tags = []
+++

{{implementation|AWK}}
'''mawk''' is Mike Brennan's awk. It is compatible with [[nawk]] and POSIX awk. It has at least one more feature: RS can be a regular expression.

mawk's interpreter tries to be fast. It translates each program to an internal [[bytecode]]. This program, to sum the numbers on each input line, becomes ten instructions of bytecode:


```txt
$ mawk -W dump '{ sum += $0 } END { print sum }' 
END
000 pushi       sum
002 pushint     1
004 print
006 exit0
MAIN
000 omain
001 pusha       sum
003 pushi       $0
005 add_asg
006 pop
007 ol_gl
```


The last version from Mike Brennan was mawk 1.3.3. Thomas E Dickey revived the project and made mawk 1.3.4.

* [http://invisible-island.net/mawk/mawk.html mawk 1.3.4]
* [http://openports.se/lang/mawk OpenBSD port of mawk 1.3.3]
