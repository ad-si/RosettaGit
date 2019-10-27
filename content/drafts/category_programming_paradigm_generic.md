+++
title = "Category:Programming paradigm/Generic"
description = ""
date = 2010-07-22T13:48:46Z
aliases = []
[extra]
id = 3388
[taxonomies]
categories = []
tags = []
+++

{{feature|Programming paradigm}}'''Generic programming''' is a programming paradigm that deals with sets of language entities rather than with individual instances. The sets can consist of:

* subprograms (generic function);
* types and classes of (generic types);
* objects (generic object);
* compilation units (generic package).

For example, a generic function describes a set of functions, a generic type describes a set of types, etc.

Such generic sets are called '''generics''' or '''templates'''. Being a set, the generics itself is not yet a subject of the [[programming language]]. It must first be constrained to an element of the set in order to identify a concrete subprogram, type, object, package, etc. So the language of generics constitutes a meta language relative to the underlying programming language (the object language).

The process of constraining a generic is called '''instantiation'''. The result of instantiation is a generic instance. Instantiation can be

* automatic (like in [[C++]]);
* explicit (like in [[Ada]]).

Instantiation constrains the generic set by defining generic formal parameters. What can serve as a generic formal parameter depends on the language. In the most general case it can be a subprogram, type, object, instance of another generic.

Partial definition or constraining of some of generic formal parameters of a generics is called '''generic specialization'''. Specialization produces another generic rather than an instance of a generic.

Like any other programming language, the meta language of generics can be typed. In a typed language of generics, substitution of an actual generic parameter is checked upon instantiation against the formal generic type. The choice does not necessarily depend on the object language. For example, [[C++]] is typed, but its language of generics is not.

Generics are sometimes said to be ''parametric'' or else ''static'' [[polymorphism|polymorphic]]. The effect of generic programming in [[object-oriented programming]] is achieved by means ''dynamic'' polymorphism. In this case generic programming is narrowed to the programming in terms of sets of types (i.e. classes), rather than arbitrary sets of language entities.

Historically generic programming first appeared in the form of macro languages and preprocessors.
