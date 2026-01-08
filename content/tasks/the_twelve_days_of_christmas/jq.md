+++
title = "jq"
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
[The Twelve Days of Christmas](../) done in jq.


## jq

```jq
[ "st","nd","rd" ] as $nth |
[ "a partridge in a pear tree.", "turtle doves", "French hens", "calling birds", "gold rings", "geese a-laying", "swans a-swimming", "maids a-milking", "ladies dancing", "lords a-leaping", "pipers piping", "drummers drumming" ] as $gifts |
range(12) | . as $i |
"On the " + (.+1|tostring)+if $i < ($nth|length) then $nth[$i] else "th" end + " day of Christmas, my true love gave to me\n" + if $i > 0 then [[range($i)]|reverse[]|((.+2|tostring) + " " + $gifts[.+1] + if $i > 1 then "," else "" end +"\n")]|join("") + "and " else "" end + $gifts[0] + "\n"
```


Run with
```txt
jq -rnf programfile.jq
```
 to yield this result:

```txt
On the 1st day of Christmas, my true love gave to me
a partridge in a pear tree.

On the 2nd day of Christmas, my true love gave to me
2 turtle doves
and a partridge in a pear tree.

On the 3rd day of Christmas, my true love gave to me
3 French hens,
2 turtle doves,
and a partridge in a pear tree.

[...]

On the 12th day of Christmas, my true love gave to me
12 drummers drumming,
11 pipers piping,
10 lords a-leaping,
9 ladies dancing,
8 maids a-milking,
7 swans a-swimming,
6 geese a-laying,
5 gold rings,
4 calling birds,
3 French hens,
2 turtle doves,
and a partridge in a pear tree.
```
