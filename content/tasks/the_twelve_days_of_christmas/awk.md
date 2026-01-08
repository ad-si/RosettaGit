+++
title = "AWK"
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
[The Twelve Days of Christmas](../) done in AWK.


## AWK

```AWK

# syntax: GAWK -f THE_TWELVE_DAYS_OF_CHRISTMAS.AWK
BEGIN {
    gifts[++i] = "a partridge in a pear tree."
    gifts[++i] = "two turtle doves, and"
    gifts[++i] = "three french hens,"
    gifts[++i] = "four calling birds,"
    gifts[++i] = "five golden rings,"
    gifts[++i] = "six geese a-laying,"
    gifts[++i] = "seven swans a-swimming,"
    gifts[++i] = "eight maids a-milking,"
    gifts[++i] = "nine ladies dancing,"
    gifts[++i] = "ten lords a-leaping,"
    gifts[++i] = "eleven pipers piping,"
    gifts[++i] = "twelve drummers drumming,"
    split("first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth",days_arr," ")
    for (i=1; i<=12; i++) {
      printf("On the %s day of Christmas,\n",days_arr[i])
      print("my true love gave to me:")
      for (j=i; j>0; j--) {
        printf("%s\n",gifts[j])
      }
      print("")
    }
    exit(0)
}

```

```txt

On the first day of Christmas,
my true love gave to me:
a partridge in a pear tree.

On the second day of Christmas,
my true love gave to me:
two turtle doves, and
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
five golden rings,
four calling birds,
three french hens,
two turtle doves, and
a partridge in a pear tree.

```
