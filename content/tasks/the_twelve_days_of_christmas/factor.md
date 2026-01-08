+++
title = "Factor"
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
[The Twelve Days of Christmas](../) done in Factor.


## Factor

```factor
USING: formatting io kernel math math.ranges qw sequences ;
IN: rosetta-code.twelve-days-of-christmas

CONSTANT: opener
    "On the %s day of Christmas, my true love sent to me:\n"

CONSTANT: ordinals qw{
    first second third fourth fifth sixth seventh eighth ninth
    tenth eleventh twelfth
}

CONSTANT: gifts {
    "A partridge in a pear tree."
    "Two turtle doves, and"
    "Three french hens,"
    "Four calling birds,"
    "Five golden rings,"
    "Six geese a-laying,"
    "Seven swans a-swimming,"
    "Eight maids a-milking,"
    "Nine ladies dancing,"
    "Ten lords a-leaping,"
    "Eleven pipers piping,"
    "Twelve drummers drumming,"
}

: descend ( n -- ) 0 [a,b] [ gifts nth print ] each nl ;

: verse ( n -- )
    1 - [ ordinals nth opener printf ] [ descend ] bi ;

: twelve-days-of-christmas ( -- ) 12 [1,b] [ verse ] each ;

MAIN: twelve-days-of-christmas
```

```txt

On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves, and
A partridge in a pear tree.

...

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

```
