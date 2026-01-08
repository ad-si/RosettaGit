+++
title = "dc"
description = ""
date = 2019-10-18T20:29:35Z
aliases = []
[extra]
id = 16950
[taxonomies]
categories = []
tags = []
+++

{{collection|The Twelve Days of Christmas}} [[implementation of task::The Twelve Days of Christmas| ]]
[The Twelve Days of Christmas](../) done in dc.


## dc

```dc
0

d [first]                       r :n
d [A partridge in a pear tree.] r :g 1 +

d [second]                      r :n
d [Two turtle doves and]        r :g 1 +

d [third]                       r :n
d [Three French hens,]          r :g 1 +

d [fourth]                      r :n
d [Four calling birds,]         r :g 1 +

d [fifth]                       r :n
d [Five gold rings,]            r :g 1 +

d [sixth]                       r :n
d [Six geese a-laying,]         r :g 1 +

d [seventh]                     r :n
d [Seven swans a-swimming,]     r :g 1 +

d [eighth]                      r :n
d [Eight maids a-milking,]      r :g 1 +

d [ninth]                       r :n
d [Nine ladies dancing,]        r :g 1 +

d [tenth]                       r :n
d [Ten lords a-leaping,]        r :g 1 +

d [eleventh]                    r :n
d [Eleven pipers piping,]       r :g 1 +

d [twelfth]                     r :n
  [Twelve drummers drumming,]   r :g

[
  d
  ;g n
  10 P
] sp

[
  d
  0 r !<p
  1 -
  d
  0 r !<r
] sr

[
  [On the ] n
  d ;n n
  [ day of Christmas, my true love sent to me:] n
  10 P
  d
  lr x s_
  10 P
  1 +
  d
  12 r <l
] sl

0 ll x

```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```
