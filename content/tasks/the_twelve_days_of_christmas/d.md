+++
title = "D"
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
[The Twelve Days of Christmas](../) done in D.


## D

```d
immutable gifts =
"A partridge in a pear tree.
Two turtle doves
Three french hens
Four calling birds
Five golden rings
Six geese a-laying
Seven swans a-swimming
Eight maids a-milking
Nine ladies dancing
Ten lords a-leaping
Eleven pipers piping
Twelve drummers drumming";

immutable days = "first second third fourth fifth
                  sixth seventh eighth ninth tenth
                  eleventh twelfth";

void main() @safe {
    import std.stdio, std.string, std.range;

    foreach (immutable n, immutable day; days.split) {
        auto g = gifts.splitLines.take(n + 1).retro;
        writeln("On the ", day,
                " day of Christmas\nMy true love gave to me:\n",
                g[0 .. $ - 1].join('\n'),
                (n > 0 ? " and\n" ~ g.back : g.back.capitalize), '\n');
    }
}
```
