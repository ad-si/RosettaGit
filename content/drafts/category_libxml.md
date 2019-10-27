+++
title = "Category:LibXML"
description = ""
date = 2018-03-10T01:20:28Z
aliases = []
[extra]
id = 2518
[taxonomies]
categories = []
tags = []
+++

{{library}}

The library '''libxml''' is a software library for parsing XML documents and it is part, through [[wp:GNOME|GNOME]], of the [[wp:GNU Project|GNU Project]].

Several bindings for several languages exist.

Homepage is at [http://xmlsoft.org/index.html Libxml2 homepage]

To compile a sample program that use the libxml2:
 gcc -o example -I /usr/include/libxml2 -lxml2 example.c

If libxml2 headers are in the same folder as the Standard C headers, then the following will work :
 gcc -o example -lxml2 example.c
