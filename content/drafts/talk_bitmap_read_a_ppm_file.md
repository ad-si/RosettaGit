+++
title = "Talk:Bitmap/Read a PPM file"
description = ""
date = 2010-02-06T14:42:26Z
aliases = []
[extra]
id = 3309
[taxonomies]
categories = []
tags = []
+++

The C code uses a type <code>image</code> and functions <code>alloc_img</code> and <code>free_img</code>, which are nowhere defined (they are definitively not part of standard C). I guess they come from some library; in that case
* the needed library should be given
* the necessary <code>#include</code> should be added to the samples (A quick guess: The commented-out <code>#include "imglib.h"</code> from the calling example code might provide those types; maybe "imglib" is even the name of that library)
The same is also true for the related tasks, of course. --[[User:Ce|Ce]] 17:35, 15 January 2009 (UTC)
: Many of these image tasks depend on other image tasks, such as [[Basic bitmap storage]]. The dependencies are mentioned in the task description. --[[User:IanOsgood|IanOsgood]] 21:12, 15 January 2009 (UTC)
