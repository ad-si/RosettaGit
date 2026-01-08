+++
title = "Seed7"
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
[The Twelve Days of Christmas](../) done in Seed7.


## Seed7

```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const array string: gifts is [] (
        "A partridge in a pear tree.", "Two turtle doves and",
        "Three french hens", "Four calling birds",
        "Five golden rings", "Six geese a-laying",
        "Seven swans a-swimming", "Eight maids a-milking",
        "Nine ladies dancing", "Ten lords a-leaping",
        "Eleven pipers piping", "Twelve drummers drumming");
    const array string: days is [] (
        "first", "second", "third", "fourth", "fifth", "sixth",
        "seventh", "eighth", "ninth", "tenth", "eleventh", "Twelfth");
    var integer: day is 0;
    var integer: gift is 0;
  begin
    for day range 1 to length(days) do
      writeln;
      writeln("On the " <& days[day] <& " day of Christmas,");
      writeln("My true love gave to me:");
      for gift range day downto 1 do
        writeln(gifts[gift]);
      end for;
    end for;
  end func;
```


```txt

On the first day of Christmas,
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas,
My true love gave to me:
Two turtle doves and
A partridge in a pear tree.

On the third day of Christmas,
My true love gave to me:
Three french hens
Two turtle doves and
A partridge in a pear tree.

...

On the Twelfth day of Christmas,
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
Two turtle doves and
A partridge in a pear tree.

```
