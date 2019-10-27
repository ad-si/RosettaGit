+++
title = "Talk:Matrix multiplication"
description = ""
date = 2016-08-15T01:20:22Z
aliases = []
[extra]
id = 21051
[taxonomies]
categories = []
tags = []
+++

== Hints about optimization ==
The task looks easy, but between two implementations of this basic function, speed may differ by several orders of magnitude.
There are several ways to optimize a matrix product (optimizing cache usage by loop order and block product, transposing, using SIMD processor instructions, OpenMP...). Here is a lecture I like at MIT OpenCourseWare: ''[http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-172-performance-engineering-of-software-systems-fall-2010/video-lectures/lecture-1-matrix-multiply-a-case-study/ Matrix Multiply: A Case Study]''. In real life, one would use an optimized BLAS library like what is found in ATLAS or Intel MKL. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 23:05, 14 August 2016 (UTC)
:That depends on the size and structure of the matrices, and also the application, and available resources, and on the people involved, for a variety of reasons. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:20, 15 August 2016 (UTC)
