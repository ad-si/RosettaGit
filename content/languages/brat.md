+++
title = "Brat"
description = ""
date = 2012-05-18T23:32:16Z
aliases = []
[extra]
id = 9155
[taxonomies]
categories = []
tags = []
+++

Brat is a little language which tries to let you do what you want to do, because it knows no one is the boss of you.
While influenced by [Ruby](https://rosettacode.org/wiki/Ruby) in many ways, it accidentally resembles [Javascript](https://rosettacode.org/wiki/Javascript).
The language design attempts to avoid "special cases" as much as possible, and therefore has no keywords and very few special symbols.

In Brat, everything is either an object or a function, and all functions are closures.
Objects are essentially just collections of functions which can have inheritance relationships with other objects.
The object system in Brat uses a prototyping approach, so new objects are always created as children of some existing object.
Functions in Brat are first-class values which can be passed around like any other value.

Brat is also a very eager language.
The only way to delay code evaluation is to enclose it in a function.
Any use of a variable containing a function is assumed to be calling the function.
