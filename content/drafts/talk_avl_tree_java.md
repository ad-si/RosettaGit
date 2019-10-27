+++
title = "Talk:AVL tree/Java"
description = ""
date = 2017-05-23T12:05:23Z
aliases = []
[extra]
id = 21000
[taxonomies]
categories = []
tags = []
+++

The Java version was ported from C# in 2016. The port of the full [http://nncnannara.net/Html/English/Java/index.html Calculus] class library took only a week. [User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 10:20, 11 July 2016 (UTC)

The abbreviated version of Java on the main page will suffer in performance just like the C++ version on the main page - see [http://rosettacode.org/wiki/Talk:AVL_tree/C%2B%2B C++ Talk].

The Java implementation of AVL Trees is a bit different to the C# version in that references to references are not available. This means that functions like RotateLeft and RotateRight return the new node rather than the function updating the reference for you. It is only a cosmetic change to the code. [[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 11:25, 16 July 2016 (UTC)
