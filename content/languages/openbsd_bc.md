+++
title = "OpenBSD bc"
description = ""
date = 2011-02-08T02:12:01Z
aliases = []
[extra]
id = 9225
[taxonomies]
categories = []
tags = []
+++
This is the <tt>/usr/bin/bc</tt> on [OpenBSD](https://rosettacode.org/wiki/OpenBSD) systems. It has some, but not all, of the features as [GNU bc](https://rosettacode.org/wiki/GNU_bc).

* [http://www.openbsd.org/cgi-bin/man.cgi?query=bc&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html OpenBSD bc(1) manual page]
* [http://www.openbsd.org/cgi-bin/cvsweb/src/usr.bin/bc/ src/usr.bin/bc/]

These extensions are in OpenBSD bc but not in [POSIX](https://rosettacode.org/wiki/POSIX):

* Two new options, <tt>bc -c</tt> and <tt>bc -e</tt>.
* Long names (more than one letter) for variables and functions.
* <tt># Line comments.</tt>
* Relational operators (<tt>== <= >= !- < ></tt>) in any expression, not only in 'if', 'while' or 'for'.
* Boolean operators (<tt>! && ||</tt>).
* <tt>else</tt> branch of an 'if' statement.
* <tt>print</tt> statement, with escapes like '\n'.
* Special variable <tt>last</tt>, also known as <tt>.</tt> (a single dot).

The most unique feature of OpenBSD bc is that it runs on top of [dc](https://rosettacode.org/wiki/dc). (The original AT&T bc also did this, but GNU bc does not.) OpenBSD bc translates the entire program from bc to dc, then calls [OpenBSD dc](https://rosettacode.org/wiki/OpenBSD_dc) <tt>/usr/bin/dc</tt> to run the program. (OpenBSD dc has several extensions to allow this to work.)

The <tt>bc -c</tt> option just skips the second step, so we can see how OpenBSD translates a program from [bc](https://rosettacode.org/wiki/bc) to [dc](https://rosettacode.org/wiki/dc). Here follows an example with the [Fibonacci sequence](https://rosettacode.org/wiki/Fibonacci_sequence).

* '''Program using bc''' 
```bc
$ cat prog.bc
# compute fib[0] thru fib[19]
fib[0] = 0
fib[1] = 1
for (i = 2; i < 20; i++) {
	fib[i] = fib[i - 1] + fib[i - 2]
}

# print fib[a] thru fib[b]
define fib(a, b) {
	auto i
	print "fib ", a, "..", b, " = "
	for (i = a; i <= b; i++) {
		print fib[i]
		if (i < b) print ", " else print "\n"
	}
}

# assignments to prevent printing of trash
trash = fib(0, 4)
trash = fib(15, 19)
quit
```

* '''Translation to dc''' 
```dc
$ bc -c prog.bc | vis

 0 0:\M^?\^A\^A
 1 1:\M^?\^A\^A
[li 1-;\M^?\^A\^Ali 2-;\M^?\^A\^A+li:\M^?\^A\^Alid1+sis.li 20>0]s0
 2dsis.li 20>0 


[[, ]n]s1
[[
]n]s2
[li;\M^?\^A\^Ads.nlilb>1e2 lid1+sis.lilb!<0]s0
[0SiSbSa[fib ]nlads.n[..]nlbds.n[ = ]nladsis.lilb!<0 Las.Lbs.Lis.0 1Q]s\M^?\^A\^B




 0 4l\M^?\^A\^Bxs\M^?\^A\^C
 15 19l\M^?\^A\^Bxs\M^?\^A\^C
q
```

* '''Output''' <lang>$ bc -c prog.bc | dc -x  # or simply $ bc prog.bc
fib 0..4 = 0, 1, 1, 2, 3
fib 15..19 = 610, 987, 1597, 2584, 4181
```

