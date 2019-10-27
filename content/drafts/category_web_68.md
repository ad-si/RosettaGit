+++
title = "Category:Web 68"
description = ""
date = 2012-09-22T08:58:30Z
aliases = []
[extra]
id = 12326
[taxonomies]
categories = []
tags = []
+++

Web 68 is a literate programming language which was designed to produce Algol 68 programs. In fact, it is quite possible that
Web 68 could be used for other languages since, AFAIK, there is nothing in the preprocessor which depends on the source code
being Algol 68. The preprocessor
contains a hash function for recognising Algol 68 constructs, but unknown tags are simply stored in its symbol table.

The manual page for Web 68 can be found at [http://www.poenikatu.co.uk/html/Web68man.html]. The source code of '''tang''', the
preprocessor, is in the algol68 directory of that web site. It can be compiled with the a68toc compiler found in the
algol68toc Debian/RPM package. See [http://www.poenikatu.co.uk/algol68/].

Several Web 68 include files have been prepared which provide access to some C library functions, general procedures such as
sub-string operators, sorts and the binary chop (implemented as a macro so that it can be used for multiples of any mode).
A Web 68 binding to the Xforms library has been in use now for several years. A regular expression library is also available.
It cannot be used to replace UTF-8 character strings. A Unicode regular expression library is under development.

The Xforms forms designer '''fdesign''' will output a form definition file. The program '''fdtow68''' will read the form definition
file and output compilable Web 68 code. The resulting module will be put into a static library and used in a main program.

A new file and directory selector has been written in Web 68 as a compilable module. It uses the Xforms library and is meant to
replace the standard file selector provided by that library.
--[[User:Phoenix2275|Sian Mountbatten]] 08:58, 22 September 2012 (UTC)
