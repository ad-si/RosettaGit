+++
title = "GNU Compiler Collection"
description = ""
date = 2017-08-24T06:00:43Z
aliases = []
[extra]
id = 1512
[taxonomies]
categories = []
tags = []
+++

{{Compiler}}[[Category:Encyclopedia]]The '''GNU Compiler Collection''', or '''GCC''', is a multi-language compiler supporting multiple target architectures. As of version 4.1, the main branch includes support for [[Ada]], [[C]], [[C++]], [[Fortran]], [[Java]], [[Objective-C]], and [[Objective-C++]]. Support for other languages is possible through the creation of a compiler front-end.

==Basic Usage==

Any of GCC's supported languages may be compiled through the simple command-line construct:

```txt
gcc (source-file)
```

However, some languages depend on the [[link time|linking]] of libraries, such as C++'s [[Standard Template Library]], to reach their full potential. In GCC, one way to include the STL is to change the way the compiler is called:

```txt
g++ (source-file)
```

In the above two examples, GCC will produce a binary file named <tt>a.out</tt>, barring any [[compile time|compile-time]] errors. This is the executable form of the code compiled. If it is preferable to have a binary of a different name, and it usually is, one can use the <tt>-o</tt> command-line option:

```txt
gcc (source-file) -o mybinary
```

''or''

```txt
g++ (source-file) -o mybinary
```

These example behaves the same as their sibling examples, with the exception that they create a binary named <tt>mybinary</tt> instead of <tt>a.out</tt>.

==See Also==
* [http://gcc.gnu.org/ GCC official home page]
* [[wp:GNU Compiler Collection|GNU Compiler Collection on Wikipedia]]
* [http://gcc.gnu.org/onlinedocs/ GCC online documentation]
* [http://gcc.gnu.org/install/binaries.html An incomplete list of third-party binary distributions] for systems that don't already have a compiler installed.
* [http://mingw.org/ MinGW], a widely-used port of GCC to [[Windows]]
* [https://mingw-w64.org MinGW-w64], a popular fork of MinGW that adds Win64 support and additional tools and APIs
* [http://tdm-gcc.tdragon.net/ TDM-GCC], another Windows port (not listed in the third-party distribution page above)
* [http://www.delorie.com/djgpp/ DJGPP], a widely-used port of GCC to [[DOS]]
