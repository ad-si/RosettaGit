+++
title = "O"
description = ""
date = 2008-08-07T13:41:49Z
aliases = []
[extra]
id = 2919
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]'''Complexity'''. In computer science the notation O(f(n)) is used to denote an upper limit of the asymptotic behavior of an algorithm, usually its complexity in terms of execution time or memory consumption (space). Here ''f'' is a function of ''n'', often a power or logarithm. ''n'' describes the size of the problem. The meaning of O(f(n)) is that the complexity grows with n at most as f(n) does.

The notation can also be used to describe a computational problem. In that case it characterizes the problem's complexity through the best known algorithm that solves it.

Examples: searching in an unordered container of ''n'' elements has the complexity of O(''n''). [[Binary search]] in an ordered container with random element access is O(log ''n''). The term random access actually means that the complexity of access is constant, i.e. O(1).

Here are some typical complexity classes listed from 'slowest' to 'fastest' (that is, slower algorithms have Big-O's near the top):
*O(e<sup>n</sup>) ('exponential')
*O(n<sup>k</sup>) for some fixed ''k'' ('polynomial')
*O(n<sup>3</sup>) ('cubic')
*O(n<sup>2</sup>) ('quadratic')
*O(n*log(n)) (fastest possible time for a [[:Category:Sorting Algorithms|comparison sort]])
*O(n) ('linear')
*O(log(n)) ('logarithmic')
*O(1) ('constant')

See also [[wp:Big_O_notation|Big O notation]]
