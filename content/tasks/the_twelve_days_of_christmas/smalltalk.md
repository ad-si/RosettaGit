+++
title = "Smalltalk"
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
[The Twelve Days of Christmas](../) done in Smalltalk.


## Smalltalk

```smalltalk
Object subclass: TwelveDays [
  Ordinals := #('first'   'second' 'third' 'fourth' 'fifth'    'sixth'
                'seventh' 'eighth' 'ninth' 'tenth'  'eleventh' 'twelfth').

  Gifts := #( 'A partridge in a pear tree.' 'Two turtle doves and'
              'Three French hens,'          'Four calling birds,'
              'Five gold rings,'            'Six geese a-laying,'
              'Seven swans a-swimming,'     'Eight maids a-milking,'
              'Nine ladies dancing,'        'Ten lords a-leaping,'
              'Eleven pipers piping,'       'Twelve drummers drumming,' ).
]

TwelveDays class extend [
  giftsFor: day [
    |newLine ordinal giftList|
    newLine := $<10> asString.
    ordinal := Ordinals at: day.
    giftList := (Gifts first: day) reverse.

    ^'On the ', ordinal, ' day of Christmas, my true love sent to me:',
      newLine, (giftList join: newLine), newLine.
  ]
]

1 to: 12 do: [:i |
  Transcript show: (TwelveDays giftsFor: i); cr.
].
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
