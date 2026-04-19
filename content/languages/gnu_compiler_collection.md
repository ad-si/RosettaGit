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

[Category:Encyclopedia](https://rosettacode.org/wiki/Category:Encyclopedia)The '''GNU Compiler Collection''', or '''GCC''', is a multi-language compiler supporting multiple target architectures. As of version 4.1, the main branch includes support for [Ada](https://rosettacode.org/wiki/Ada), [C](https://rosettacode.org/wiki/C), [C++](https://rosettacode.org/wiki/C++), [Fortran](https://rosettacode.org/wiki/Fortran), [Java](https://rosettacode.org/wiki/Java), [Objective-C](https://rosettacode.org/wiki/Objective-C), and [Objective-C++](https://rosettacode.org/wiki/Objective-C++). Support for other languages is possible through the creation of a compiler front-end.

## Basic Usage
Any of GCC's supported languages may be compiled through the simple command-line construct:

```txt
gcc (source-file)
```

However, some languages depend on the [linking](https://rosettacode.org/wiki/link_time) of libraries, such as C++'s [Standard Template Library](https://rosettacode.org/wiki/Standard_Template_Library), to reach their full potential. In GCC, one way to include the STL is to change the way the compiler is called:

```txt
g++ (source-file)
```

In the above two examples, GCC will produce a binary file named <tt>a.out</tt>, barring any [compile-time](https://rosettacode.org/wiki/compile_time) errors. This is the executable form of the code compiled. If it is preferable to have a binary of a different name, and it usually is, one can use the <tt>-o</tt> command-line option:

```txt
gcc (source-file) -o mybinary
```

''or''

```txt
g++ (source-file) -o mybinary
```

These example behaves the same as their sibling examples, with the exception that they create a binary named <tt>mybinary</tt> instead of <tt>a.out</tt>.

## See Also
* [GCC official home page](https://gcc.gnu.org/)
* [GNU Compiler Collection on Wikipedia](https://en.wikipedia.org/wiki/GNU_Compiler_Collection)
* [GCC online documentation](https://gcc.gnu.org/onlinedocs/)
* [An incomplete list of third-party binary distributions](https://gcc.gnu.org/install/binaries.html) for systems that don't already have a compiler installed.
* [MinGW](http://mingw.org/), a widely-used port of GCC to [Windows](https://rosettacode.org/wiki/Windows)
* [MinGW-w64](https://mingw-w64.org), a popular fork of MinGW that adds Win64 support and additional tools and APIs
* [TDM-GCC](http://tdm-gcc.tdragon.net/), another Windows port (not listed in the third-party distribution page above)
* [DJGPP](http://www.delorie.com/djgpp/), a widely-used port of GCC to [DOS](https://rosettacode.org/wiki/DOS)
