+++
title = "Talk:Find the intersection of a line with a plane"
description = ""
date = 2018-11-18T18:17:12Z
aliases = []
[extra]
id = 21509
[taxonomies]
categories = []
tags = []
+++

== How is this to be done? ==
I think this should be a draft task as there seems to be nothing in the description about how to complete the task in a comparable manner. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:45, 15 April 2018 (UTC)

== A more general case should be added ==
such as:

E: n=(1,2,3) P=(3,3,3)

g: A=(0,2,4) v=(3,2,1)

..Walter Pachl 20:10, 3 July 2017 (UTC)

== Java output for special cases ==

I tested some special cases:

```txt
If the ray is parallel to the plane:
        Vector3D rv = new Vector3D(1.0, 1.0, 0.0);
        Vector3D rp = new Vector3D(0.0, 0.0, 1.0);
        Vector3D pn = new Vector3D(0.0, 0.0, 3.0);
        Vector3D pp = new Vector3D(0.0, 0.0, 0.0);
The ray intersects the plane at (-Infinity, -Infinity, NaN)
REXX: Line is parallel to the plane

If the ray is within the plane:
        Vector3D rv = new Vector3D(1.0, 1.0, 0.0);
        Vector3D rp = new Vector3D(1.0, 1.0, 0.0);
        Vector3D pn = new Vector3D(0.0, 0.0, 3.0);
        Vector3D pp = new Vector3D(0.0, 0.0, 0.0);
The ray intersects the plane at (NaN, NaN, NaN)
REXX: Line is part of the plane
```

How could that be improved? ..Walter Pachl 18:16, 18 November 2018 (UTC)
