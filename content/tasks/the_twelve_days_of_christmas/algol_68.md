+++
title = "ALGOL 68"
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
[The Twelve Days of Christmas](../) done in ALGOL 68.


## ALGOL 68

```algol68
BEGIN
  []STRING labels = ("first", "second", "third",    "fourth",
                     "fifth", "sixth",  "seventh",  "eighth",
                     "ninth", "tenth",  "eleventh", "twelfth");

  []STRING gifts = ("A partridge in a pear tree.",
                    "Two turtle doves, and",
                    "Three French hens,",
                    "Four calling birds,",
                    "Five gold rings,",
                    "Six geese a-laying,",
                    "Seven swans a-swimming,",
                    "Eight maids a-milking,",
                    "Nine ladies dancing,",
                    "Ten lords a-leaping,",
                    "Eleven pipers piping,",
                    "Twelve drummers drumming,");
  FOR day TO 12 DO
    print(("On the ", labels[day],
           " day of Christmas, my true love sent to me:", newline));
    FOR gift FROM day BY -1 TO 1 DO
      print((gifts[gift], newline))
    OD;
    print(newline)
  OD
END
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves, and
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
Two turtle doves, and
A partridge in a pear tree.
```
