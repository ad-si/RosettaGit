+++
title = "BlitzMax"
description = ""
date = 2011-11-10T17:37:09Z
aliases = []
[extra]
id = 10127
[taxonomies]
categories = []
tags = []
+++
The first BlitzMax compiler was released in December 2004 for [Mac OS X](https://rosettacode.org/wiki/Mac_OS_X). This made it the first Blitz dialect that could be compiled on *nix platforms. Compilers for [Microsoft](https://rosettacode.org/wiki/Microsoft) [Windows](https://rosettacode.org/wiki/Windows) and [Linux](https://rosettacode.org/wiki/Linux) were subsequently released in May 2005. BlitzMax brought the largest change of language structure to the modern range of Blitz products by extending the type system to include object-oriented concepts and modifying the graphics API to better suit [OpenGL](https://rosettacode.org/wiki/OpenGL). BlitzMax was also the first of the Blitz languages to represent strings internally using UCS2, allowing native-support for strings literals composed of non-ASCII characters.

BlitzMax's platform-agnostic command-set allows developers to compile and run source code on multiple platforms. However the official compiler and build chain will only generate binaries for the platform that it is executing on. Unofficially, users have been able to get Linux and Mac OS X to cross-compile to the Windows platform.

BlitzMax is also the first modular version of the Blitz languages, improving the extensibility of the command-set. In addition, all of the standard modules shipped with the compiler are open-source and so can be tweaked and recompiled by the programmer if necessary. The official BlitzMax cross-platform GUI module (known as MaxGUI) allows developers to write GUI interfaces for their applications on Linux (FLTK), Mac ([Cocoa](https://rosettacode.org/wiki/Cocoa)) and Windows. Various user-contributed modules extend the use of the language by wrapping such libraries as wxWidgets, Cairo, Fontconfig as well as a selection of database modules. There are also a selection of third-party 3D modules available namely MiniB3D - an open-source OpenGL engine which can be compiled and used on all 3 of BlitzMax's supported platforms.

In October 2007, BlitzMax 1.26 was released which included the addition of a reflection module. BlitzMax 1.32 shipped new threading and Lua scripting modules and most of the standard library functions have been updated so that they are unicode friendly.
