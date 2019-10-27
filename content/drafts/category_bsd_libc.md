+++
title = "Category:BSD libc"
description = ""
date = 2011-06-18T23:51:16Z
aliases = []
[extra]
id = 9449
[taxonomies]
categories = []
tags = []
+++

{{library}}
BSD libc is the core library for the [[C]] language with the [[BSD]] operating systems. This category collects examples that use functions or macros which are part of BSD libc, but not part of [[POSIX]].

These examples require a BSD system, or another system with the same extension. (For example, <err.h> originates from 4.4BSD but also appears in GNU libc.) Some of these examples will also work with [[Mac OS X]].

* ''What with BSD sockets?'' Rosetta Code has some examples using BSD sockets, but we have not moved them into this category, because BSD sockets might later obtain their own category.

== Partial list of BSD extensions ==
New headers:
* <sys/queue.h>: macros for linked lists.
* <sys/tree.h>: macros for splay trees and red-black trees.
* <db.h>: Berkeley DB 1.85.
* <err.h>: functions like err() for error messages.
* <fts.h>: functions to [[Walk a directory/Recursively|traverse a directory tree]].
* <vis.h>: vis().

New functions in standard headers:
* <stdio.h>: fgetln().
* <stdlib.h>: arc4random(), daemon(), radixsort().
* <string.h>: strlcat() and strlcpy().
