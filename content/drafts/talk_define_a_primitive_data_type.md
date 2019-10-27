+++
title = "Talk:Define a primitive data type"
description = ""
date = 2010-02-06T13:03:25Z
aliases = []
[extra]
id = 3990
[taxonomies]
categories = []
tags = []
+++

Couldn't a class be considered ''primitive'' type (?) in Java? Or anyway, for languages like Java (Python could be another one?) the only (at least I can't see another one) way of defining a "data types" with an "active behaviour" (data+code...) is to "encapsulate" the whole stuff into a class... --[[User:ShinTakezou|ShinTakezou]] 16:32, 1 March 2009 (UTC)

:I guess the intention was something like "built-in scalar type." I think the difference lies between a type, which operations are defined by a single (primitive?) types algebra construct like '''int T''' in C++, and a full-blown user-defined type produced by another types algebra construct like '''class T {}''' of same C++. Personally, I don't see any great difference between them, but the task creator probably did. He could also add a pile of secondary thoughts about value vs. reference semantics of "primitive" and "non-primitive" types, he probably had (which, to me, would have no sense at all). My 2&cent;. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 17:29, 1 March 2009 (UTC)

:: After posting my Fortran code, I've got another look at Java, and discovered that the only Java code issue is not the fact that it "uses" a class: it does the same the C++ code, with the difference that C++ allows operator overloading, Java does not. The real issue with Java is that it really does not accomplish the task... (2+2&cent; makes 4, day by day we can become rich:D) --[[User:ShinTakezou|ShinTakezou]] 18:13, 1 March 2009 (UTC)

All the Java documentation I have read separates primitive types (int, char, float) from all other Java types. In Java the primitive types do not inherit from the Class class. They can only be passed by value to a function, while all classes are passed by reference. Java provides wrapper classes for the primitive types to bridge the gap between the primitive types and everything else in Java. These wrapper classes allow the programmer to use the primitive types with container classes. For instance, one cannot create a vector of int. One must create a vector of the Integer wrapper class. I was not looking exactly for "built-in scalar types". Some languages allow the definition of scalar types which are not built-in. Ada is one such language. Ada allows the programmer to define a wide variety of scalar types, none of which are built-into the language. For instance, it is possible to define a modular integer type with a range of values from 0 through 9. That user-defined type can be used as an actual generic parameter whenever a discrete scalar type is specified as the type of the formal generic parameter. Ada tagged types, which support inheritance, cannot be used as an actual generic parameter when a discrete scalar type is specified as the type of the formal generic parameter. 

The task is intended to identify how such primitive data types are defined in your language of choice. If the language does not support the definition of user-defined primitive types, then the language cannot support the task. This task is intended to be different than a task to define a class. --[[User:Waldorf|Waldorf]] 03:35, 2 March 2009 (UTC)

: Then Java suffers for the same problem of C++, and many more languages. "Few" (among the most common and considered the large amount of computer languages available) languages then support really the definition of new primitive data types. But the task says ''Include all bounds checking you need to write, '''or'''...'', and this suggests that the C++ or the Java approach (and the Fortran, Perl, Visual Basic ...) are not so wrong; only the word ''primitive'' (the meaning of it is IMO disputable) makes the task restrictive. To me, only the following languages accomplish the task according to your idea: Ada, E, Modula-3, (maybe Haskell?). My memory about Pascal tells me that the task can be accomplished in this language too, but I can be wrong. --[[User:ShinTakezou|ShinTakezou]] 11:05, 2 March 2009 (UTC)
