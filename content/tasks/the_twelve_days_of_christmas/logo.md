+++
title = "Logo"
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
[The Twelve Days of Christmas](../) done in Logo.


## Logo

```logo
make "numbers [first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth]
make "gifts   [[And a partridge in a pear tree] [Two turtle doves]     [Three French hens]
               [Four calling birds]             [Five gold rings]      [Six geese a-laying]
               [Seven swans a-swimming]         [Eight maids a-miling] [Nine ladies dancing]
               [Ten lords a-leaping]            [Eleven pipers piping] [Twelve drummers drumming]]

to nth :n
  print (sentence [On the] (item :n :numbers) [day of Christmas, my true love sent to me])
end

nth 1
print [A partridge in a pear tree]

for [d 2 12] [
  print []
  nth :d
  for [g :d 1] [
    print item :g gifts
  ]
]
bye
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
