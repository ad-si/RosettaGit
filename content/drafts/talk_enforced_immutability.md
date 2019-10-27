+++
title = "Talk:Enforced immutability"
description = ""
date = 2011-06-25T15:07:09Z
aliases = []
[extra]
id = 9967
[taxonomies]
categories = []
tags = []
+++

== C const: a joke ==

The C <code>const</code> is such a joke. 
```C
void some_func(const int const * const a)
{
    ((int*)a)[0]++;
}
```
<i>Not even a compiler warning!</i> So much for "immutable".
