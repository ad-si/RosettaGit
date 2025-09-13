+++
title = "MinGW"
description = ""
date = 2011-04-28T17:34:07Z
aliases = []
[extra]
id = 7420
[taxonomies]
categories = []
tags = []
+++
[http://www.mingw.org/ MinGW] is a port of [GCC](https://rosettacode.org/wiki/GCC) to [Windows](https://rosettacode.org/wiki/Windows).

MinGW can compile Ada, C, C++, Fortran and Objective-C. The compiled programs are <tt>.exe</tt> files; they can use the Microsoft C runtime <tt>MSVCRT.DLL</tt> and several other Microsoft libraries.

MinGW also provides the import libraries (like <tt>libadvapi32.a</tt> and <tt>libmscvcrt.a</tt>) and the header files (like <windows.h> and <stdio.h>). GCC needs these to compile programs that use the Windows API. The compiled programs only need the DLL libraries. Microsoft includes the DLL libraries with Windows.

MinGW always produces 32-bit code for x86. MinGW never produces 16-bit nor 64-bit code.
