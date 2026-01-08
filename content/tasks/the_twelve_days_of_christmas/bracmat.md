+++
title = "Bracmat"
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
[The Twelve Days of Christmas](../) done in Bracmat.


## Bracmat

```bracmat
(     first
      second
      third
      fourth
      fifth
      sixth
      seventh
      eighth
      ninth
      tenth
      eleventh
      twelveth
  : ?days
&     "A partridge in a pear tree."
      "Two turtle doves and"
      "Three french hens,"
      "Four calling birds,"
      "Five golden rings,"
      "Six geese a-laying,"
      "Seven swans a-swimming,"
      "Eight maids a-milking,"
      "Nine ladies dancing,"
      "Ten lords a-leaping,"
      "Eleven pipers piping,"
      "Twelve drummers drumming,"
  : ?gifts
& :?given
&   whl
  ' ( !gifts:%?gift ?gifts
    & !gift \n !given:?given
    & !days:%?day ?days
    &   out
      $ ( str
        $ ("\nOn the " !day " day of Christmas my true love gave to me:
" !given)
        )
    )
);
```
