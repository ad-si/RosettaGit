+++
title = "Template:PARI/GP select"
description = ""
date = 2011-05-07T20:43:23Z
aliases = []
[extra]
id = 9563
[taxonomies]
categories = []
tags = []
+++

Works with PARI/GP 2.4.3 and above.

This code uses the [this select function](
http://pari.math.u-bordeaux.fr/dochtml/html/Programming_in_GP:_other_specific_functions.html#select),
which was added in PARI version 2.4.2.

The order of the arguments changed between versions;
to use in 2.4.2 change `select(function, vector)` to
`select(vector, function)`.
