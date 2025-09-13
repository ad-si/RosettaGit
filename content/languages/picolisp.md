+++
title = "PicoLisp"
description = ""
date = 2018-04-09T10:38:04Z
aliases = []
[extra]
id = 6101
[taxonomies]
categories = []
tags = []
+++
[Category:Lisp Implementations](https://rosettacode.org/wiki/Category:Lisp_Implementations)

'''PicoLisp''' is a small and fast interpreted [Lisp](https://rosettacode.org/wiki/Lisp) dialect.
It runs on [Linux](https://rosettacode.org/wiki/Linux) and other [POSIX](https://rosettacode.org/wiki/POSIX)-compliant systems, and - in a reduced version - on [JVM](https://rosettacode.org/wiki/JVM).

Its most prominent feature is "simplicity". It is built on top of a single internal data type (the cell), without giving up flexibility and expressive power.
On the language level, it supports just three data types (numbers, symbols and lists), constructed from internal cells.

PicoLisp programs are often more succinct - and at the same time faster - than those of other interpreted languages.
A special feature is the intrinsic database functionality: Persistent symbols are first-class objects, and applications are written using a class hierarchy of entities and relations.

Other features include a [Prolog](https://rosettacode.org/wiki/Prolog) engine for logic programming and database queries, distributed databases, inlining of [C](https://rosettacode.org/wiki/C)/asm functions and native C/asm function calls, child process management, interprocess communication, Browser GUI, Internationalization and localization.

To try the RosettaCode tasks, download [http://software-lab.de/picoLisp.tgz picoLisp.tgz], unpack it, and follow the instructions in README and INSTALL. Unless stated otherwise, the examples assume that the interpreter was started with the command
```bash
$ pil +
:
```

(i.e. with a '+' for "debug mode", as recommended in the documentation.
Note: Do not call just the 'picolisp' binary, this is only the bare kernel of PicoLisp)

## See Also
* [http://software-lab.de/doc/ref.html Reference Manual]
* [http://software-lab.de/doc/faq.html Frequently asked questions]

## Todo
[Reports:Tasks_not_implemented_in_PicoLisp](https://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_PicoLisp)
