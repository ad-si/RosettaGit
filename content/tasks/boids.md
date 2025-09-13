+++
title = "Boids"
description = ""
date = 2019-06-08T07:40:53Z
aliases = []
[extra]
id = 12755
[taxonomies]
categories = ["task"]
tags = []
+++

A [[wp:Boids|Boids]] algorithm simulates the flocking behavior of birds. This task requires the creation of a graphical or purely textual simulation of a flock of birds navigating the cave with obstacles depicted below.


```txt
.......###############...............................................................
.......###############...............................................................
......#################..............................................................
O......###############...............................................................
OO.....###############...............................................................
OO.....###############....................#.........................#................
OO......#############...................#####...................#########............
OO......#############...................#####..................###########...........
OO.......###########...................#######................#############..........
OO.........#######......................#####................###############.........
OO............#.........................#####...............#################........
OO........................................#.................#################........
O...........................................................#################........
............................................................#################........
...........................................................###################.......
............................................................#################........
```


If you implement a purely textual simulation, this is a possible board representation, where "O" are the boids that should go toward the right, "#" are fixed walls that should be avoided by the boids, and "." is free space (using a space is also acceptable for free space, if you add some kind of frame around the board).

A simulation that doesn't contain obstacles but only shows flocking behavior is acceptable.



## See also

* http://www.red3d.com/cwr/boids/
* http://natureofcode.com/book/chapter-6-autonomous-agents/




## C

See [[Boids/C]]


## Java

See [[Boids/Java]]


## Julia

See [[Boids/Julia]]


## Phix

See [[Boids/Phix]]

Screenshot: [http://phix.x10.mx/shots/boids.png http://phix.x10.mx/shots/boids.png]
