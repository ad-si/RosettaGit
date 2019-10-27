+++
title = "Category:Ozsqlite"
description = ""
date = 2010-01-19T22:14:13Z
aliases = []
[extra]
id = 5340
[taxonomies]
categories = []
tags = []
+++

{{library}}{{implementation|SQL}}

OzSqlite is an Oz interface to {{libheader|SQLite}}, a lightweight embeddable sql database engine. It comes with its own version of SQLite.

==Installation==
Download and unpack with

```txt

ozmake --extract package=mogul:/elansary/ozsqlite

```


On Unix-like systems, follow the instructions in README.txt

On Windows with cygwin, in order to make it compatible with the current cygwin version:

```txt

cd libsqlite

```


Edit "Makefile" as indicated in the file itself.

Insert in sqlite.h before line 16:

```txt

#include "os.h"

```


Insert in os.h before line 134:

```txt

#define	_OFF_T_

```


Insert in parse.c before line 6:

```txt

#include "os.h"

```


Then build it:

```txt

make

cd..
ozmake --build -L ./libsqlite
ozmake --install

```


==External Links==
*[http://www.mozart-oz.org/mogul/info/elansary/ozsqlite.html Ozsqlite site]
