+++
title = "Property talk:Allows"
description = ""
date = 2010-08-15T15:32:55Z
aliases = []
[extra]
id = 7975
[taxonomies]
categories = []
tags = []
+++

Are we talking about explicit support here, or are we talking about a looser connection between a capability and a language? Should this property be subsected between explicit, implicit and by-binding? (where by-binding might mean dynamic linking against a shared library or the platform kernel) --[[User:Short Circuit|Michael Mol]] 08:33, 15 August 2010 (UTC)

: The idea was that "provides" means that the feature is directly supported by the language or its standard library (I think this is what you mean be "explicit?"), while "allows" means that there are ways to access the feature from the language, possibly through third-party libraries. For example, when the next C++ standard gets published, "concurrency" will have to be changed from "allows" to "provides" because the next version of C++ will have a standard threads library.
: I'm not sure what's the difference between "implicit" and "by-binding" (and I also don't see why there should be a difference between features accessed through statically or dynamically linked libraries; e.g. in C++ many libraries can be used in both ways). --[[User:Ce|Ce]] 15:32, 15 August 2010 (UTC)
