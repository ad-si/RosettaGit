+++
title = "Red"
description = ""
date = 2016-10-07T08:50:54Z
aliases = []
[extra]
id = 21148
[taxonomies]
categories = []
tags = []
+++
Red is a paradigm-neutral [homoiconic](https://en.wikipedia.org/wiki/Homoiconicity) language, strongly inspired by [Rebol](https://rosettacode.org/wiki/Rebol).

Like Rebol, Red has a low memory footprint, is garbage collected, and has a [low disk footprint (< 1MB)](http://www.red-lang.org/p/download.html).  But while Rebol is an interpreted language written in ANSI-C, Red seeks to be a "full-stack" language whose methodology is independent of any other toolchain.  It compiles that which can be known ahead of time, JIT-compiles that which cannot, and embeds a small interpreter into its executables to handle constructions which are not amenable to any compilation.

Red embeds several DSLs, among which Red/System (C semantics meet Red syntax), dedicated to low-level and system programming. It is also used as an intermediate language (IL) when Red is compiled. The Red executable is able to build Red/System files directly (`*.reds`) as well as Red files (`*.red`), and Red/System code may be embedded freely in Red code.

* [Red Language Website](http://www.red-lang.org)
* [@red_lang](https://twitter.com/red_lang) on Twitter.
* [Red community chat](https://gitter.im/red/red) on Gitter.
* [Mailing-List](https://groups.google.com/group/red-lang?hl=en)
