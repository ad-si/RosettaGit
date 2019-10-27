+++
title = "Talk:AVL tree/C++"
description = ""
date = 2017-04-11T15:59:07Z
aliases = []
[extra]
id = 21005
[taxonomies]
categories = []
tags = []
+++

The algorithms started out life in Pascal in a book called "Data Structures and Program Design" by Robert L Kruse. These were top/down (i.e. root to leaf) recursive algorithms without parent pointers. In 2006, the algorithms were made bottom/up (i.e. leaf to root) and the recursion was abolished (using parent pointers). These algorithms may be compared to the red/black set algorithms of STL. [[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 12:30, 13 July 2016 (UTC)

I tested the elaborate code versus the shorter C++ version (on the main page) with the following results:

```txt

AVLtree insertions took: 00:00:02.5097816
Set insertions took: 00:00:00.0080027

```


Clearly, AVLtree is more like O(N<sup>2</sup>) or worse than O(log N). This is because it descends the tree during rotations (to adjust the balance factor). Set is clearly very fast at O(log N). The test was for 10000 insertions. If 100000 or 1000000 insertions are used Set rips through it but AVLtree stalls the machine.[[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 12:45, 15 July 2016 (UTC)

The abbreviated Java and D samples also have the same problem in that they descend the tree to ascertain the depth. You must be careful not to do anything to upset the performance of RotateLeft and RotateRight.[[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 12:26, 16 July 2016 (UTC)
