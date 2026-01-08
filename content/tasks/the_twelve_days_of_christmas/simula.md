+++
title = "Simula"
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
[The Twelve Days of Christmas](../) done in Simula.


## Simula

```simula
Begin
  Text Array days(1:12), gifts(1:12);
  Integer day, gift;

  days(1)  :- "first";
  days(2)  :- "second";
  days(3)  :- "third";
  days(4)  :- "fourth";
  days(5)  :- "fifth";
  days(6)  :- "sixth";
  days(7)  :- "seventh";
  days(8)  :- "eighth";
  days(9)  :- "ninth";
  days(10) :- "tenth";
  days(11) :- "eleventh";
  days(12) :- "twelfth";


  gifts(1)  :- "A partridge in a pear tree.";
  gifts(2)  :- "Two turtle doves and";
  gifts(3)  :- "Three French hens,";
  gifts(4)  :- "Four calling birds,";
  gifts(5)  :- "Five gold rings,";
  gifts(6)  :- "Six geese a-laying,";
  gifts(7)  :- "Seven swans a-swimming,";
  gifts(8)  :- "Eight maids a-milking,";
  gifts(9)  :- "Nine ladies dancing,";
  gifts(10) :- "Ten lords a-leaping,";
  gifts(11) :- "Eleven pipers piping,";
  gifts(12) :- "Twelve drummers drumming,";

  For day := 1 Step 1 Until 12 Do Begin
    outtext("On the "); outtext(days(day));
    outtext(" day of Christmas, my true love sent to me:"); outimage;
    For gift := day Step -1 Until 1 Do Begin
      outtext(gifts(gift)); outimage
    End;
    outimage
  End
End

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
