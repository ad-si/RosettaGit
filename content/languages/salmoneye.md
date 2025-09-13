+++
title = "SalmonEye"
description = ""
date = 2013-09-15T06:40:48Z
aliases = []
[extra]
id = 10785
[taxonomies]
categories = []
tags = []
+++
SalmonEye is an interpreter for the Salmon language.  It is written in [C](https://rosettacode.org/wiki/C) and designed to be very portable.  The core of the interpreter uses only portable ANSI C (C90 to be specific).  Some optional features of the interpreter require more than what standard C provides, so those optional features are only available on systems that support more than ANSI C.  These optional features include expanded runtime library support that can be used by Salmon programs and a plug-in system to dynamically load additional native code modules and let Salmon programs call that code.

SalmonEye also provides a library form of the interpreter, allowing it to be incorporated into other programs.  This allows Salmon to be used as an extension language by other programs.  These programs can provide new built-in functions that the Salmon code they use can call to get functionality specific to the program.

SalmonEye is in the public domain.

## External Links
* [http://salmonpl.net SalmonEye official site]
