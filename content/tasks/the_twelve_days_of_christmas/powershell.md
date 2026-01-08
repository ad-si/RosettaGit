+++
title = "PowerShell"
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
[The Twelve Days of Christmas](../) done in PowerShell.


## PowerShell

```PowerShell
$days = @{
    1 = "first";
    2 = "second";
    3 = "third";
    4 = "fourth";
    5 = "fifth";
    6 = "sixth";
    7 = "seventh";
    8 = "eight";
    9 = "ninth";
    10 = "tenth";
    11 = "eleventh";
    12 = "twelfth";
}

$gifts = @{
    1 = 'A partridge in a pear tree';
    2 = 'Two turtle doves';
    3 = 'Three french hens';
    4 = 'Four calling birds';
    5 = 'Five golden rings';
    6 = 'Six geese a-laying';
    7 = 'Seven swans a-swimming';
    8 = 'Eight maids a-milking';
    9 = 'Nine ladies dancing';
    10 = 'Ten lords a-leaping';
    11 = 'Eleven pipers piping';
    12 = 'Twelve drummers drumming';
}

1 .. 12 | % {
    "On the $($days[$_]) day of Christmas`nMy true love gave to me"
    $_ .. 1 | % {
        $gift = $gifts[$_]
        if ($_ -eq 2) { $gift += " and" }
        $gift
    }
    ""
}
```


```txt
On the first day of Christmas
My true love gave to me
A partridge in a pear tree

On the second day of Christmas
My true love gave to me
Two turtle doves and
A partridge in a pear tree

...

On the twelfth day of Christmas
My true love gave to me
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
Two turtle doves and
A partridge in a pear tree

```
