+++
title = "Smart BASIC"
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
[The Twelve Days of Christmas](../) done in Smart BASIC.


## Smart BASIC

```smart BASIC
' by rbytes
dim d$(12),x$(15)!s=15
for t=0 to 11!read d$(t)!next t
for t=0 to 14!read x$(t)!next t
for u=0 to 11!s-=1
print x$(0)&d$(u)&x$(1)&chr$(10)&x$(2)
for t=s to 14!print x$(t)!next t
print!next u!data "first","second","third","fourth","fifth","sixth","seventh","eight","ninth","tenth","eleventh","Twelfth","On the "," day of Christmas","My true love gave to me:","Twelve drummers drumming","Eleven pipers piping","Ten lords a-leaping","Nine ladies dancing","Eight maids a-milking","Seven swans a-swimming,","Six geese a-laying","Five golden rings","Four calling birds","Three french hens","Two turtle doves and","A partridge in a pear tree."
```

```txt
On the first day of Christmas
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas
My true love gave to me:
Two turtle doves and
A partridge in a pear tree.

On the third day of Christmas
My true love gave to me:
Three french hens
Two turtle doves and
A partridge in a pear tree.

[ ... ]

On the eleventh day of Christmas
My true love gave to me:
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming,
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree.

On the Twelfth day of Christmas
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming,
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree.
```
