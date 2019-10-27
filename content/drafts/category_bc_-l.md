+++
title = "Category:Bc -l"
description = ""
date = 2011-07-08T21:50:31Z
aliases = []
[extra]
id = 10057
[taxonomies]
categories = []
tags = []
+++

{{library}}
The command <code>bc -l</code> loads the [[bc]] standard library. One must use <code>bc -l</code> to run the programs in this category. The standard library defines six functions:

{| class="wikitable"
|| <code>s(x)</code>   || sine of x radians
|-
|| <code>c(x)</code>   || cosine
|-
|| <code>e(x)</code>   || exponential
|-
|| <code>l(x)</code>   || natural logarithm
|-
|| <code>a(x)</code>   || arctangent, range -&pi;/2 to &pi;/2
|-
|| <code>j(n,x)</code> || Bessel function
|}

The standard library also sets <code>scale = 20</code>, so by default, the sines and other calculations go to 20 digits after the decimal point. The programmer can still change the scale. The standard library has no other purpose, except to define the six functions and change the default scale.

* Reference: [http://www.openbsd.org/cgi-bin/man.cgi?query=bc&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html OpenBSD bc(1)]
