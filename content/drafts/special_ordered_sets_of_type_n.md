+++
title = "Special Ordered Sets of Type N"
description = ""
date = 2012-01-26T14:36:03Z
aliases = []
[extra]
id = 11303
[taxonomies]
categories = []
tags = []
+++

see [[wp:Special_ordered_set]].

In discrete optimization, a special ordered set is an ordered set of variables. There are two sorts of Special Ordered Sets. Special Ordered Sets of type 1 (SOS1 or S1) are a set of variables, at most one of which can take a strictly positive value, all others being at 0. They most frequently apply where a set of variables are actually 0-1 variables: in other words, we have to choose one from a set of possibilities. These might arise for instance where we are deciding on what size of factory to build, when we have a set of options, perhaps small, medium, large or no factory at all, and we have to choose one and only one size.

Special Ordered Sets of type 2 (SOS2 or S2): an ordered set of non-negative variables, of which at most two can be non-zero, and if two are non-zero these must be consecutive in their ordering. Special Ordered Sets of type 2 are typically used to model non-linear functions of a variable. They are the natural extension of the concepts of Separable Programming, but when embedded in a Branch and Bound code enable truly global optima to be found, and not just local optima.

The task is to find the maximum value for the sum of N consecutive elements in a map, array, list, or vector. For the list  9 3 7 6 6 the answer is 7+6=13.
