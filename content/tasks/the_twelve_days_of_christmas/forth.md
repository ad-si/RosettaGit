+++
title = "Forth"
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
[The Twelve Days of Christmas](../) done in Forth.


## Forth

```forth
create ordinals s" first" 2, s" second" 2, s" third"    2, s" fourth" 2,
                s" fifth" 2, s" sixth"  2, s" seventh"  2, s" eighth" 2,
                s" ninth" 2, s" tenth"  2, s" eleventh" 2, s" twelfth" 2,
: ordinal ordinals swap 2 * cells + 2@ ;

create gifts s" A partridge in a pear tree." 2,
             s" Two turtle doves and" 2,
             s" Three French hens," 2,
             s" Four calling birds," 2,
             s" Five gold rings," 2,
             s" Six geese a-laying," 2,
             s" Seven swans a-swimming," 2,
             s" Eight maids a-milking," 2,
             s" Nine ladies dancing," 2,
             s" Ten lords a-leaping," 2,
             s" Eleven pipers piping," 2,
             s" Twelve drummers drumming," 2,
: gift gifts swap 2 * cells + 2@ ;

: day
  s" On the " type
  dup ordinal type
  s"  day of Christmas, my true love sent to me:" type
  cr
  -1 swap -do
    i gift type cr
  1 -loop
  cr
  ;

: main
  12 0 do i day loop
;

main
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
