+++
title = "Bc"
description = ""
date = 2014-11-21T14:55:36Z
aliases = []
[extra]
id = 3277
[taxonomies]
categories = []
tags = []
+++


'''bc''' ("basic calculator") is the standard calculator for [Unix](https://rosettacode.org/wiki/Unix) systems.

bc boasts unlimited precision, to handle numbers with very many digits.

With the bc language, you can write programs that perform numeric calculations
and print the results.

;See also :
* [Bc programming language](https://en.wikipedia.org/wiki/Bc_programming_language) on Wikipedia
* [dc](https://rosettacode.org/wiki/:Category:Dc) - the unix '''d'''esktop '''c'''alculator.

bc has an interactive mode, which is convenient for brief calculations:

 $ '''bc'''
 '''2 + 3'''
 5
 '''2 ^ 200'''
 1606938044258990275541962092341162602522202993782792835301376
 '''i = -5'''
 '''3 - i * 4'''
 23
 '''obase = 2'''
 '''i'''
 -101
 '''ibase = 16'''
 '''FE80'''
 1111111010000000

Division and <tt>sqrt</tt> will not give an infinite number of digits.

The special variable <tt>scale</tt> controls when to stop.

 $ '''bc'''
 '''65.9 / 3'''
 21
 '''scale = 6'''
 '''65.9 / 3'''
 21.966666
 '''sqrt(2)'''
 1.414213
 '''scale = 60'''
 '''sqrt(2)'''
 1.414213562373095048801688724209698078569671875376948073176679

bc language resembles [C language](https://rosettacode.org/wiki/C): bc has most of the same operators
and control structures ('if', 'while', 'for').

Expressions print themselves, unless they are assignments.

The newline is a statement separator, like the semicolon.

bc has excellent numeric operations, but is a poor language.
The original bc, the "Bell Calculator" of Unix V7,
translated the program to [dc](https://rosettacode.org/wiki/dc) and inherited the limitations of [dc](https://rosettacode.org/wiki/dc).

* Names of variables, and custom functions, may have only one letter.
* There is no 'else' branch of an 'if' statement.
* Relational operators (== <= => != < >) only work in the condition of an 'if', 'while' or 'for' statement.
* There are no boolean operators (! && ||).

Some newer implementations, like [GNU bc](https://rosettacode.org/wiki/GNU_bc) and [OpenBSD bc](https://rosettacode.org/wiki/OpenBSD_bc),
discard these silly limitations, but users can still feel their effects.

OpenBSD says, "a = b < c is interpreted as "(a = b) < c, which is probably not what the programmer intended," and "!a < b is interpreted as !(a < b)".

bc can only print a string, and has no other string operations,
so bc cannot do tasks like [reverse a string](https://rosettacode.org/wiki/reverse_a_string).

bc has no way to read user input, except to go to interactive mode
after loading a program.

[Category:Mathematical programming languages](https://rosettacode.org/wiki/Category:Mathematical_programming_languages)
[Category:Utility](https://rosettacode.org/wiki/Category:Utility)
