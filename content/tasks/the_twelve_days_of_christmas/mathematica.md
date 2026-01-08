+++
title = "Mathematica"
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
[The Twelve Days of Christmas](../) done in Mathematica.


## Mathematica

```Mathematica

daysarray = {"first", "second", "third", "fourth", "fifth", "sixth",
   "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"};
giftsarray = {"And a partridge in a pear tree.", "Two turtle doves",
   "Three french hens", "Four calling birds", "FIVE GOLDEN RINGS",
   "Six geese a-laying", "Seven swans a-swimming,",
   "Eight maids a-milking", "Nine ladies dancing",
   "Ten lords a-leaping", "Eleven pipers piping",
   "Twelve drummers drumming"};
Do[Print[StringForm[
   "On the `1` day of Christmas, my true love gave to me: `2`",
   daysarray[[i]],
   If[i == 1, "A partridge in a pear tree.",
    Row[Reverse[Take[giftsarray, i]], ","]]]], {i, 1, 12}]

```

<pre style="height:55ex;overflow:scroll">
On the first day of Christmas, my true love gave to me:
A partridge in a pear tree.

On the second day of Christmas, my true love gave to me:
Two turtle doves,
And a partridge in a pear tree.

On the third day of Christmas, my true love gave to me:
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the fourth day of Christmas, my true love gave to me:
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the fifth day of Christmas, my true love gave to me:
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the sixth day of Christmas, my true love gave to me:
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the seventh day of Christmas, my true love gave to me:
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the eighth day of Christmas, my true love gave to me:
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the ninth day of Christmas, my true love gave to me:
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the tenth day of Christmas, my true love gave to me:
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the eleventh day of Christmas, my true love gave to me:
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the twelfth day of Christmas, my true love gave to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

```
