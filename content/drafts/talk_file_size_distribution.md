+++
title = "Talk:File size distribution"
description = ""
date = 2016-10-09T15:37:46Z
aliases = []
[extra]
id = 21155
[taxonomies]
categories = []
tags = []
+++

== Clarification on logarithms ==
It does not change anything to compute logarithms: if sizes P and Q are different, then log(P) and log(Q) will by different as well, unless the values are truncated. The task should be clarified about this. Anyway, it's very easy to adapt the program, for instance in the Python code, either by computing an aggregated key before storing in the dictionary, either by computing everuthng in the end (but then the dictionary will grow larger if there are many different sizes).

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 15:37, 9 October 2016 (UTC)
