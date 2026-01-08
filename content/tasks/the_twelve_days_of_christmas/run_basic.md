+++
title = "Run BASIC"
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
[The Twelve Days of Christmas](../) done in Run BASIC.


## Run BASIC

```Runbasic
gifts$ = "
A partridge in a pear tree.,
Two turtle doves,
Three french hens,
Four calling birds,
Five golden rings,
Six geese a-laying,
Seven swans a-swimming,
Eight maids a-milking,
Nine ladies dancing,
Ten lords a-leaping,
Eleven pipers piping,
Twelve drummers drumming"

days$ = "first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth"

for i = 1 to 12
    print "On the ";word$(days$,i," ");" day of Christmas"
    print "My true love gave to me:"
    for j = i to 1 step -1
    if i > 1 and j = 1 then print "and ";
    print mid$(word$(gifts$,j,","),2)
    next j
    print
next i

```
Output:

```txt
On the first day of Christmas
My true love gave to me:
A partridge in a pear tree.
.
.
On the twelfth day of Christmas
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and A partridge in a pear tree.
```
