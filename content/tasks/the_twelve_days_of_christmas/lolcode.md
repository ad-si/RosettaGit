+++
title = "LOLCODE"
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
[The Twelve Days of Christmas](../) done in LOLCODE.


## LOLCODE

<lang>CAN HAS STDIO?
HAI 1.2

I HAS A Dayz ITZ A BUKKIT
Dayz HAS A SRS  1 ITZ "first"
Dayz HAS A SRS  2 ITZ "second"
Dayz HAS A SRS  3 ITZ "third"
Dayz HAS A SRS  4 ITZ "fourth"
Dayz HAS A SRS  5 ITZ "fifth"
Dayz HAS A SRS  6 ITZ "sixth"
Dayz HAS A SRS  7 ITZ "seventh"
Dayz HAS A SRS  8 ITZ "eighth"
Dayz HAS A SRS  9 ITZ "ninth"
Dayz HAS A SRS 10 ITZ "tenth"
Dayz HAS A SRS 11 ITZ "eleventh"
Dayz HAS A SRS 12 ITZ "twelfth"

I HAS A Prezents ITZ A BUKKIT
Prezents HAS A SRS  1 ITZ "A partridge in a pear tree"
Prezents HAS A SRS  2 ITZ "Two turtle doves"
Prezents HAS A SRS  3 ITZ "Three French hens"
Prezents HAS A SRS  4 ITZ "Four calling birds"
Prezents HAS A SRS  5 ITZ "Five gold rings"
Prezents HAS A SRS  6 ITZ "Six geese a-laying"
Prezents HAS A SRS  7 ITZ "Seven swans a-swimming"
Prezents HAS A SRS  8 ITZ "Eight maids a-milking"
Prezents HAS A SRS  9 ITZ "Nine ladies dancing"
Prezents HAS A SRS 10 ITZ "Ten lords a-leaping"
Prezents HAS A SRS 11 ITZ "Eleven pipers piping"
Prezents HAS A SRS 12 ITZ "Twelve drummers drumming"

IM IN YR Outer UPPIN YR i WILE DIFFRINT i AN 12
  I HAS A Day ITZ SUM OF i AN 1
  VISIBLE "On the " !
  VISIBLE Dayz'Z SRS Day !
  VISIBLE " day of Christmas, my true love sent to me"
  IM IN YR Inner UPPIN YR j WILE DIFFRINT j AN  Day
    I HAS A Count ITZ DIFFERENCE OF Day AN j
    VISIBLE Prezents'Z SRS Count
  IM OUTTA YR Inner
  BOTH SAEM i AN 0
  O RLY?
    YA RLY
      Prezents'Z SRS 1 R "And a partridge in a pear tree"
  OIC
  DIFFRINT i AN 11
    O RLY?
      YA RLY
        VISIBLE ""
  OIC
IM OUTTA YR Outer

KTHXBYE
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
