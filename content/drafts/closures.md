+++
title = "Closures"
description = ""
date = 2009-11-19T00:18:00Z
aliases = []
[extra]
id = 2869
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]A '''closure''' is a subprogram passed to another subprogram, or specified in a language construct, in a way that it can refer to its declaration context. Closures are further subdivided into:
* '''Downward''', or else inward;
* '''Upward''', else outward

A closure is downward when neither the subprogram, nor the context to which it refers, as well as any types they use, leave the corresponding scopes upon passing the closure. It is passed '''down''' the scope. A closure is upward otherwise.

Implementation of downward closures does not require much effort from the language designer. Usually it is merely an ability to pass a subprogram or a pointer to it as a parameter to another subprogram. Problems arise when a nested subprogram has to be returned out of its containing subprogram while referencing to the context of the latter. This is an upward closure. The problem here is that the context of the containing subprogram, which has been left and abandoned, is still referenced by the closure. This significantly complicates the language scoping structure. As a result scoping alone can no longer serve for determination of the objects' life times.
