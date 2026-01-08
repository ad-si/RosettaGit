+++
title = "MiniScript"
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
[The Twelve Days of Christmas](../) done in MiniScript.


## MiniScript

```MiniScript
days = ["first","second","third", "fourth","fifth","sixth",
        "seventh","eigth","nineth","tenth","eleventh","twelfth"]
gifts = ["A partridge in a pear tree.","Two turtle doves, and",
        "Three French hens,","Four calling birds,",
        "Five gold rings,","Six geese a-laying,",
        "Seven swans a-swimming,","Eight maids a-milking,",
        "Nine ladies dancing,","Ten lords a-leaping,",
        "Eleven pipers piping,","Twelve drummers drumming,"]

for i in range(0,11)
    print "On the " + days[i] + " day of Christmas,"
    print "my true love gave to me,"
    for j in range(i,0)
        print " " + gifts[j]
    end for
    print "     ----------"
end for

```

```txt

On the first day of Christmas,
my true love gave to me,
A partridge in a pear tree.
----------

<and so on until the last>

On the twelfth day of Christmas,
my true love gave to me,
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
Two turtle doves, and
A partridge in a pear tree.
----------

```
