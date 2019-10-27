+++
title = "Category talk:Programming paradigm/Generic"
description = ""
date = 2010-07-22T17:19:03Z
aliases = []
[extra]
id = 2971
[taxonomies]
categories = []
tags = []
+++

I don't think the description of "generic programming" given here is correct. Basically, it seems to me that the author understood "generic programming" basically as "anything you can do with templates". However, according to http://www.generic-programming.org/ the term "generic programming" is quite specific for a method which tries to generalize algorithms as much as possible. This doesn't necessarily mean templates/macros (in dynamic typed languages you surely wouldn't need them), nor does it cover every use of macros/templates. Especially it is to be differentiated from ''[http://www.program-transformation.org/Transform/GenerativeProgramming generative programming]'', which in C++ is usually done using template metaprogramming. Some descriptions on the article page seem to fit generative programming more than generic programming. --[[User:Ce|Ce]] 13:36, 5 August 2008 (UTC)

I agree. The article describes "programming with templates", i.e. genericity expressed solely by parametric polymorphism. '''Generic programming''' is more universal, it is programming in terms of sets of types. E.g. generalization is achieved by designing a program that would work for a set of types from some class, rather than for a specific type. For example, generic sorting would deal with any comparable type. Generic sum woold with any additive type etc. It is clear that generic programming can be achieved using other forms of polymorphism. Moreover, those may have a sufficient advantage over templates, because they do not require meta programming (i.e. programming in a meta language of templates) in order to achieve the required abstraction level of a set of types. I think the article should be revised. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 21:40, 21 March 2009 (UTC)
