+++
title = "Pascal"
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
[The Twelve Days of Christmas](../) done in Pascal.


## Pascal

This should work with any modern Pascal implementation that has a '''string''' type, e.g. [[Free Pascal]].


```pascal
program twelve_days(output);

const
  days:  array[1..12] of string =
    ( 'first',   'second', 'third', 'fourth', 'fifth',    'sixth',
      'seventh', 'eighth', 'ninth', 'tenth',  'eleventh', 'twelfth' );

  gifts: array[1..12] of string =
    ( 'A partridge in a pear tree.',
      'Two turtle doves and',
      'Three French hens,',
      'Four calling birds,',
      'Five gold rings,',
      'Six geese a-laying,',
      'Seven swans a-swimming,',
      'Eight maids a-milking,',
      'Nine ladies dancing,',
      'Ten lords a-leaping,',
      'Eleven pipers piping,',
      'Twelve drummers drumming,' );

var
   day, gift: integer;

begin
   for day := 1 to 12 do begin
     writeln('On the ', days[day], ' day of Christmas, my true love sent to me:');
     for gift := day downto 1 do
       writeln(gifts[gift]);
     writeln
   end
end.
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


Here's a version that works in ISO Standard Pascal, albeit with extraneous spaces in the output:


```pascal
program twelve_days_iso(output);

const
  days:  array[1..12, 1..8] of char =
    ( 'first   ', 'second  ', 'third   ', 'fourth  ',
      'fifth   ', 'sixth   ', 'seventh ', 'eighth  ',
      'ninth   ', 'tenth   ', 'eleventh', 'twelfth ' );

  gifts: array[1..12, 1..27] of char =
    ( 'A partridge in a pear tree.',
      'Two turtle doves and       ',
      'Three French hens,         ',
      'Four calling birds,        ',
      'Five gold rings,           ',
      'Six geese a-laying,        ',
      'Seven swans a-swimming,    ',
      'Eight maids a-milking,     ',
      'Nine ladies dancing,       ',
      'Ten lords a-leaping,       ',
      'Eleven pipers piping,      ',
      'Twelve drummers drumming,  ' );

var
   day, gift: integer;

begin
   for day := 1 to 12 do begin
     writeln('On the ', days[day], ' day of Christmas, my true love gave to me:');
     for gift := day downto 1 do
       writeln(gifts[gift]);
     writeln
   end
end.
```


```txt
On the first    day of Christmas, my true love gave to me:
A partridge in a pear tree.

On the second   day of Christmas, my true love gave to me:
Two turtle doves and
A partridge in a pear tree.
```


[...]


```txt
On the twelfth  day of Christmas, my true love gave to me:
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
