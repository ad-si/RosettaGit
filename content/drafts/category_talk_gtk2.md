+++
title = "Category talk:Gtk2"
description = ""
date = 2009-07-09T12:52:50Z
aliases = []
[extra]
id = 4485
[taxonomies]
categories = []
tags = []
+++

A similar issue to Gtk+/Gtk here: Gtk2 is not a different lib, it is a just a version of the GTK (ok, with a lot of changes... but still GTK after all); even, while transforming GTK+ to GTK noticed here and there some examples use something like libheader|gtkD , which, I suppose, it's just D bindings to GTK... A similar speech could be done for gtkmm, it's just the C++ bindings. A suppose similar questions can arise in even other cases; maybe we should think about a more coherent and detailed way of categorizing by libraries or about a way of adding to libheader something like library version or needed bindings for the specific language...? --[[User:ShinTakezou|ShinTakezou]] 15:23, 8 July 2009 (UTC)
:I feel like this talk has gone on before, but I can't find where. The problem is that these different implementations of the same library seem like they are maintained and discussed separately. So GTK, GTKD, and GTKAda are different things that all accomplish the same tasks. Maybe we could just have cross links? They're probably already there. --[[User:Mwn3d|Mwn3d]] 15:37, 8 July 2009 (UTC)
:: I meant, more clearly: GtkD, GTKAda, Gtkmm ... are just bindings created to make it possible the usage of the GTK library through those languages (D, Ada, C++...). So their usage is fine, but it would mean that for every library, they exist a list of bindings for the language X, and then it should exist a category for each binding, which it sounds like having a GTK page for C, a GTK page for Ada, a GTK page for D, a GTK page for C++ and so on.
:: Then, I was wondering if it would be better to identify libraries in a bindings-agnostic way (simply GTK), and at most to keep in the library page the list of available bindings (it is rather obvious that to use the library X in the language Y, we likely need proper bindings for the language Y to the library X)...
:: The problem of the version is different: as it is possible using the "works with" to specify a (minimal) required version, so it should be possible for libraries, so that this category (Gtk2) could be absorbed by GTK; something like <tt>libheader|GTK|2</tt> just to say.
:: I am just scattering ideas. --[[User:ShinTakezou|ShinTakezou]] 12:52, 9 July 2009 (UTC)
