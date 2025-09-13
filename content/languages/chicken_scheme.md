+++
title = "Chicken Scheme"
description = ""
date = 2011-06-21T21:56:57Z
aliases = []
[extra]
id = 3306
[taxonomies]
categories = []
tags = []
+++

'''Chicken Scheme''' is an R5RS compliant compiler for the [Scheme programming language](https://rosettacode.org/wiki/Scheme). It produces [C](https://rosettacode.org/wiki/C) code as output.

Chicken Scheme is implemented in C, using Henry Baker's "Cheney on the MTA" algorithm which uses the C [stack](https://rosettacode.org/wiki/system_stack) pointer as it's [garbage collection](https://rosettacode.org/wiki/garbage_collection) allocation pointer.

Chicken Scheme uses libraries called '''eggs''' as well as [SRFI](https://rosettacode.org/wiki/SRFI)s.


### External Links

* [http://www.call-with-current-continuation.org/ Chicken Website]
* [http://wiki.call-cc.org/chicken-projects/egg-index-4.html Eggs Unlimited (library modules)]
* [http://home.pipeline.com/~hbaker1/CheneyMTA.html Henry Baker's article "Cheney on the MTA"]
