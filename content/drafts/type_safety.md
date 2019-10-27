+++
title = "Type safety"
description = ""
date = 2008-08-08T20:30:44Z
aliases = []
[extra]
id = 2921
[taxonomies]
categories = []
tags = []
+++

[[Category:Type System]]'''Type safety''' is a property of typed [[:Category:programming Languages|programming languages]]. A language is type-safe if any improperly typed program is illegal in the sense that it may not pass the compiler.

The relation between safe typing and dynamic type checks is that when such a check fails, possibly as late as at run-time, this outcome cannot make the program improperly typed. The most common example is downcasting. In a type-safe language it may propagate an exception in order to keep the program properly typed. So the operations not supported by the actual type will never be executed on its objects. Similarly, in dynamically typed languages any operation is defined on any type by adding a  "''does not understand''" exception to its contract. Thus, it becomes properly typed to call "unsupported" methods.

Often type safe languages provide some mechanisms to circumvent type checks. The need in such tools depends on the maturity of the types system.
