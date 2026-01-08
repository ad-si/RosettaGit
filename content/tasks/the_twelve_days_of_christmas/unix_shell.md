+++
title = "UNIX Shell"
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
[The Twelve Days of Christmas](../) done in UNIX Shell.


## UNIX Shell

```bash
#!/usr/bin/env bash
ordinals=(first   second third fourth fifth    sixth
          seventh eighth ninth tenth  eleventh twelfth)

gifts=( "A partridge in a pear tree." "Two turtle doves and"
        "Three French hens,"          "Four calling birds,"
        "Five gold rings,"            "Six geese a-laying,"
        "Seven swans a-swimming,"     "Eight maids a-milking,"
        "Nine ladies dancing,"        "Ten lords a-leaping,"
        "Eleven pipers piping,"       "Twelve drummers drumming," )

echo_gifts() {
  local i day=$1
  echo "On the ${ordinals[day]} day of Christmas, my true love sent to me:"
  for (( i=day; i >=0; --i )); do
    echo "${gifts[i]}"
  done
  echo
}

for (( day=0; day < 12; ++day )); do
  echo_gifts $day
done
```


The above will also work in zsh if the index range is changed from 0..11 to 1..12.

(requires the '''seq''' command)


```sh
#!/bin/sh
ordinal() {
  n=$1
  set first   second third fourth fifth    sixth \
      seventh eighth ninth tenth  eleventh twelfth
  shift $n
  echo $1
}

gift() {
  n=$1
  set "A partridge in a pear tree." "Two turtle doves and"      \
      "Three French hens,"          "Four calling birds,"       \
      "Five gold rings,"            "Six geese a-laying,"       \
      "Seven swans a-swimming,"     "Eight maids a-milking,"    \
      "Nine ladies dancing,"        "Ten lords a-leaping,"      \
      "Eleven pipers piping,"       "Twelve drummers drumming,"
  shift $n
  echo "$1"
}

echo_gifts() {
  day=$1
  echo "On the `ordinal $day` day of Christmas, my true love sent to me:"
  for i in `seq $day 0`; do
    gift $i
  done
  echo
}

for day in `seq 0 11`; do
  echo_gifts $day
done
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
