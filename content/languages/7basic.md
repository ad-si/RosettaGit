+++
title = "7Basic"
description = ""
date = 2014-09-07T23:01:06Z
aliases = []
[extra]
id = 10345
[taxonomies]
categories = []
tags = []
+++


'''7Basic''' is both a programming language and the cross-platform compiler that generates x86 assembly code from 7Basic source files. The language is heavily based on other BASIC-derivatives like [QuickBASIC](https://rosettacode.org/wiki/QuickBASIC) while also providing classes and pointers, although the spec. for the language is in constant flux at the moment.

## Compiler
The compiler currently is capable of generating x86 assembly code (with x87 FPU instructions) that target the Linux operating system. Plans are underway to have the compiler generate native x86 machine code not only for Linux but for other operating systems as well. The language currently supports simple constructs such as PRINT, INPUT, WHILE, and variable declarations. Nearly all math and logic operators have been implemented.

## Sample
The following example can be compiled by supplying it as a parameter to the compiler:


```basic

' Store the user's age
DIM age AS INTEGER

' Ask the user for their age
PRINT "Enter your age:"
INPUT age

' Tell them their age in 5 years
PRINT "In 5 years, you will be:"
PRINT age+5

```


## Links
* Project page: https://launchpad.net/7basic
