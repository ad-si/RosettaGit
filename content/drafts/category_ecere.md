+++
title = "Category:Ecere"
description = ""
date = 2008-07-25T13:48:19Z
aliases = []
[extra]
id = 2952
[taxonomies]
categories = []
tags = []
+++

{{Library}}The Ecere Runtime Library is part of the Ecere SDK. It is a cross-platform library including a GUI toolkit 2D/3D graphics engine, as well as networking and other system functionality. It is written in eC and as such contains the core runtime engine for the [[eC]] language.

The Ecere runtime components are what enables the cross platform aspect of the SDK. It offers the same functionality across the multiple [[:Category:Platforms|platforms]] it supports, and therefore allows for platform independent application development and deployment.
Programmers can therefore spend their time working on the essence of their application.
They don't have to worry about maintaining multiple versions of their application for each platform on which it will be deployed.

Currently, [[Windows]], [[Linux]] and [[Mac OS X]] (through X11) are supported.
It can be built for other [[UNIX]] systems, therefore if you are interested in a particular system please let us know.
A more native Mac OS X support is planned for the future, and PDAs and game consoles are being considered as well.

The runtime component has a very small footprint. It can either take the form of a shared or static library. In its simplest form it can be bundled into a single application executable smaller than 1 megabyte on Windows.

== Cross Platform [[GUI|Graphical User Interface]] ==
One of the most important feature of the runtime component is the custom cross platform GUI engine.
It is an alternative and is analogous to other GUI toolkits such as wxWindows, GTK or Qt.
It also offers a rich 2D text, graphics and image manipulation system supporting a variety of graphics file formats.

== System Functionality ==
The runtime component covers many powerful general functionality covering advanced file access, multi-threading, time. It also greatly simplifies socket programming (either through UDP or TCP/IP).

== 3D Graphics Engine ==
The runtime component contains a powerful and easy to use 3D graphics engine, introducing concepts of cameras, meshes, materials, lights and objects.
It currently supports loading models from the 3DS file format. Meshes can also be built dynamically through the [[API]]. The engine supports both Direct3D and [[OpenGL]].

== Component Object Model ==
The runtime component also enables the entire Ecere Component Object Model and its advanced features such as distributed objects, dynamic class hierarchy amalgamation, dynamic injection and ejection of modules and their components as well as automatic data type management.
