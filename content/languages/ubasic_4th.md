+++
title = "UBasic/4tH"
description = ""
date = 2015-10-31T09:40:25Z
aliases = []
[extra]
id = 17465
[taxonomies]
categories = []
tags = []
+++

'''uBasic/4tH''' (not to be confused with [UBASIC](https://rosettacode.org/wiki/UBASIC)) is an integer Basic interpreter in the tradition of Tiny BASIC, with which it is largely compatible. This version is entirely written in [4tH](https://rosettacode.org/wiki/4tH), some bugs have been removed and several additional features have been added, like user stack support and structured programming:

*<tt>PUSH, POP()</tt> and <tt>TOS()</tt> can be used to emulate <tt>DATA</tt> statements, pass parameters to subroutines or make recursive subroutines;
*<tt>DO, LOOP, UNTIL, WHILE, CONTINUE</tt> and <tt>BREAK</tt> supported;
*Multi line <tt>IF..THEN..ELSE..ENDIF</tt> supported;
*Parameter passing supported by <tt>GOSUB</tt>, <tt>RETURN</tt> and <tt>FUNC()</tt> extensions;
*Local variables supported, both initialized and uninitialized;
*Alphanumeric labels supported;
*"Structured Basic" commenting style.

## See also:
* [http://sourceforge.net/p/forth-4th/wiki/uBasic%20and%20Tiny%20BASIC/ uBasic and Tiny BASIC]
* [http://sourceforge.net/p/forth-4th/wiki/Basically%20a%20touch%20of%20Forth/ Basically a touch of Forth]
