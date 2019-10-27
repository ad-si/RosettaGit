+++
title = "Talk:AVL tree/C sharp"
description = ""
date = 2019-03-22T03:35:28Z
aliases = []
[extra]
id = 20999
[taxonomies]
categories = []
tags = []
+++

The C# version of AVL was invented in 2006. It was in C# that I got rid of the recursion (the C++ version was then recursive). In C# I used parent references to balance the tree from leaf to root. I then promptly ported the changes back to [[AVL_tree/C%2B%2B|C++]]. The first version of C# was ported from C++ and it was recursive, top down. That code has long since disappeared. The [[AVL_tree/Java|Java]] version appeared in 2016 - long after.[[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 09:52, 11 July 2016 (UTC)

== Possible bug ==

The sample code in Main seems to work fine: (but only because the items are always added to the "right"?)

Set<string> s = new Set<string>() {"S0","S1","S2","S3","S4","S5","S6","S7","S8","S9"};

but when I test with this code:

Set<string> a = new Set<string>() {"S1", "S0"};

I get "AVLTree.EntryAlreadyExistsException".

The problem is that when "S0" is added left of "S1", the "for (;;)" loop is not exited so when the compare is made, the key that was just added, "S0", is now found.  The "right" add has a "break" after a new Node is added.  The "left" add is missing the "break" after the add.

You are quite right - so I added a break in the code. Much appreciated, clearly you have an understanding of the code - cudos.[[User:NNcNannara|NNcNannara]]
