+++
title = "Libraries"
description = ""
date = 2017-11-07T22:43:05Z
aliases = []
[extra]
id = 1753
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]'''Libraries''' are software which extend the functionality of a programming language, usually by providing an [[API]] to complete a specific task.  Different languages may have their own name for libraries, such as [[Perl]] modules, or [[Java]] packages.  They also may also be implemented in different ways.

=Dynamically-linked Libraries=
Dynamically-linked libraries are libraries that are contained in a file separate from an application's primary executable, and are loaded at [[run time|run-time]].  This has the benefit of allowing multiple programs use of the same code both on disk and in memory, saving space.  It also allows a program to load additional, optional or interchangeable portions of itself into memory at runtime.

On Windows, these files are known as, naturally enough, as '''Dynamically-Linked Libraries''', from which their file extension '''DLL''' is derived.

On UNIX-derived systems, these files are known as '''Shared Objects''', from which their file extension '''so''' is derived.

==Implicit vs Explicit linking==

In [[implicit linking]], an operating system sees that a program will require the use of a library, and loads it automatically.  In [[explicit linking]], the running program asks the operating system to load the library.

=Statically-linked Libraries=
Statically-linked libraries are combined with a program's code at [[compile-time]], their code and the program's code combined to form a single executable.

=Executable libraries=
Some languages can create a library that when used as a library does one thing; but has the ability to be run directly, where it will do some extra task using, but not limited to, functionality that it makes available as a library.

=See also=
* [[:Category:Solutions by Library]]
* [[Executable library]]
