+++
title = "Perl"
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
[The Twelve Days of Christmas](../) done in Perl.


## Perl

```perl
use v5.10;

my @days = qw{ first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth };

chomp ( my @gifts = grep { /\S/ } <DATA> );

while ( my $day = shift @days ) {
    say "On the $day day of Christmas,\nMy true love gave to me:";
    say for map { $day eq 'first' ? s/And a/A/r : $_ } @gifts[@days .. @gifts-1];
    say "";
}

__DATA__
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
Two turtle doves
And a partridge in a pear tree.
```

```txt
On the first day of Christmas,
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas,
My true love gave to me:
Two turtle doves
And a partridge in a pear tree.

...

On the twelfth day of Christmas,
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
Two turtle doves
And a partridge in a pear tree.
```
