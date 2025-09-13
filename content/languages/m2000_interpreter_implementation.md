+++
title = "M2000 Interpreter Implementation"
description = ""
date = 2019-01-26T12:11:27Z
aliases = []
[extra]
id = 21889
[taxonomies]
categories = []
tags = []
+++
[M2000 Interpreter](https://rosettacode.org/wiki/M2000_Interpreter) is an open source Interpreter, written by George Karras, in Preveza Greece, for pupils for education. Is not a language for production, but can be very good for implementation of any algorithm. We can use types, or not for calling purposes, so for small problems there is no need for using types, and can be used like BASIC. There are labels, Goto, Gosub, On Goto and On Gosub to use like retro programming, but there are new types like Events (with a list of functions for multicast calls), Lambda functions with closures, Objects named Groups, with hierarchy of Groups inside Groups, and objects as containers, arrays, inventories (as vectors) and stacks. We can use Threads, and Environment support graphics (using GDI and GDI+), databases, multimedia including midi score player.

Interpreter is an activeX dll, which works by calling it from another program. We call this using a m2000.exe a small program to do that for us. We can run m2000.exe many times, so we can use multiple m2000 programs like any any other program.

M2000 Implementation can be found on GitHub [https://github.com/M2000Interpreter/Version9/wiki], written in Visual Basic 6, so it run on Windows OS and using Wine on Linux too. Scope of this implementation was to expand the language, starting from a BASIC flavor with a combination of Forth's Stack, as Stack of Values, with new features like Events, Lambda Functions, Threads and internal GUI manager. Speed was not the target, but at some developing stages a careful code refactoring produce more speedy results. Master task was to run the code, and anyone to observe the code as it runs. So M2000 works with "heavy" interpret, always code as is, and produce objects which hold state as we wish. Major steps in development was:

1. Using Groups as the main type for objects for OOP programming, as values, which we can pass them by reference, and from 9 version we can use pointers (not references), so we can hold them alive by a pointer to a group. In previous versions a group can be hold by name in block of code, or unnamed in a container like an array, and other types, or can be returned from a function, as unnamed group also. Now we can get pointer from group, and we can return pointer for group from a function too. A group may have pointers to group(s) also.

2. Event driven programming. This implemented for internal GUI elements, for ActiveX Objects  (we can declare them using WithEvents), and for groups (the user objects)

3. The adaptation of many numerical types,after a careful programing on expression's parser, written in VB6 using Variant type variables (from double type at earlier stages of development). We can use many numeric types, like double, single, decimal, currency, integer (16bit), long (32bit), and Boolean. 

4. From 9.7 version tab character allowed in EditBox(a gui control) and Internal Editor, and in M2000 programs. We can use tabs for indentation or we can use spaces. We can change the behavior using switches. Switches can be invoked from command line as arguments when we call a gsb file (a M2000 program), and or by using statement switch from m2000 console, or set switch inside a module or function.

M2000 can use external C (CCall) and Systen DLL (stdCall), can define structures and buffers, can declare objects known in Windows, with events (can open Word and handle it).

Start of development was at 1999, and this continue today, removing bus and writing examples, tutorials, many of them in Greek Language. M2000 can be freely implement to any language, developer wish, to any degree of compliance with latest version.

George Karras
Preveza, 2019
