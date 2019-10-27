+++
title = "Talk:AVL Tree/Performance"
description = ""
date = 2016-07-16T10:29:53Z
aliases = []
[extra]
id = 21009
[taxonomies]
categories = []
tags = []
+++

The D Version on the main page will also have O(N<sup>2</sup>) performance. It too descends the tree during balancing.[[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 23:06, 15 July 2016 (UTC)

Note that well written Managed Code is quite rapid. The Native C++ Code is just a tad faster.[[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 23:09, 15 July 2016 (UTC)

From the first performance test in C++, it is easy to see why a lot of people think that AVL is slower - because of the rotations. The second test in C# of Red/Black vs AVL reveals the truth. Properly written rotations are very fast. AVL does a few more rotations but these rotations have very high performance (just exchanging a few pointers or references). The overall result is that AVL is superior. [[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 10:29, 16 July 2016 (UTC)
