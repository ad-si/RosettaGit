+++
title = "Tailspin"
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
[The Twelve Days of Christmas](../) done in Tailspin.


## Tailspin

```tailspin

def ordinal: ['first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth'];
def gift: [
  'a partridge in a pear tree',
  'two turtle-doves',
  'three French hens',
  'four calling birds',
  'five golden rings',
  'six geese a-laying',
  'seven swans a-swimming',
  'eight maids a-milking',
  'nine ladies dancing',
  'ten lords a-leaping',
  'eleven pipers piping',
  'twelve drummers drumming'
];
templates punctuation
  <1> '.' !
  <2> ' and' !
  <5> ';' !
  <> ',' !
end punctuation

1..12 -> (
  'On the $ordinal($); day of Christmas,
my true love gave to me:
' !
  $..1:-1 -> '$gift($);$->punctuation;
' !
'
' !
) -> !OUT::write

```

```txt

On the first day of Christmas,
my true love gave to me:
a partridge in a pear tree.

On the second day of Christmas,
my true love gave to me:
two turtle-doves and
a partridge in a pear tree.

...

On the twelfth day of Christmas,
my true love gave to me:
twelve drummers drumming,
eleven pipers piping,
ten lords a-leaping,
nine ladies dancing,
eight maids a-milking,
seven swans a-swimming,
six geese a-laying,
five golden rings;
four calling birds,
three French hens,
two turtle-doves and
a partridge in a pear tree.



```
