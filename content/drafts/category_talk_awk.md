+++
title = "Category talk:AWK"
description = ""
date = 2018-07-01T01:01:25Z
aliases = []
[extra]
id = 5276
[taxonomies]
categories = []
tags = []
+++

There is a problem with the enumeration entry for AWK, as there is with several other languages, in that it does not show examples of the USE of the enumeration type, only its definition.  By giving an example of the use of the scheme suggested for AWK, it will be apparent that it is quite weak.

==It's a programming language==
Can't we state up front that AWK is a programming language? It's a very good scripting language, but the current definition seems to skip around the obvious. --[[User:Paddy3118|Paddy3118]] 21:29, 22 May 2011 (UTC)

== GAWK vs MAWK vs NetBSD-AWK vs ... ==


```awk
$ gawk --version | head -1
GNU Awk 4.1.4, API: 1.1 (GNU MPFR 3.1.5, GNU MP 6.1.2)
$ gawk 'BEGIN { print "0x1f"+0 }'
0
$ gawk 'BEGIN { print strtonum("0x1f") }'
31

```



```awk
$ dpkg --status mawk | grep ^Version:
Version: 1.3.3-17+b3
$ mawk 'BEGIN { print "0x1f"+0 }'
31
$ mawk 'BEGIN { print strtonum("0x1f") }'
mawk: line 2: function strtonum never defined

```



```awk
$ uname -a
NetBSD sdf 8.0_RC1 NetBSD 8.0_RC1 (GENERIC) #6: Thu Apr 26 07:48:08 EDT 2018  christos@sixthavenue.astron.com:/usr/src/sys/arch/amd64/compile/GENERIC amd64
$ awk 'BEGIN { print "0x1f"+0 }'
31
$ awk 'BEGIN { print strtonum("0x1f") }'
awk: calling undefined function strtonum
 source line number 1

```


Ok, GNU utilities are known to get bloated by tons of features until they are capable of doing everything except making coffee but in this case GAWK seems to have done a step away from MAWK and NetBSD's AWK. From the original AWK too? How does the oldest AWK you can get your hands on behave for...

```awk
$ awk 'BEGIN { print "0x1f"+0 }'
```


--[[User:Yeti|Yeti]] ([[User talk:Yeti|talk]]) 01:01, 1 July 2018 (UTC)
