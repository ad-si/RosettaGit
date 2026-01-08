+++
title = "Fortran"
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
[The Twelve Days of Christmas](../) done in Fortran.


## Fortran

```fortran
      program twelve_days

      character days(12)*8
      data days/'first', 'second', 'third',    'fourth',
     c          'fifth', 'sixth',  'seventh',  'eighth',
     c          'ninth', 'tenth',  'eleventh', 'twelfth'/

      character gifts(12)*27
      data gifts/'A partridge in a pear tree.',
     c           'Two turtle doves and',
     c           'Three French hens,',
     c           'Four calling birds,',
     c           'Five gold rings,',
     c           'Six geese a-laying,',
     c           'Seven swans a-swimming,',
     c           'Eight maids a-milking,',
     c           'Nine ladies dancing,',
     c           'Ten lords a-leaping,',
     c           'Eleven pipers piping,',
     c           'Twelve drummers drumming,'/

      integer day, gift

      do 10 day=1,12
        write (*,*) 'On the ', trim(days(day)),
     c              ' day of Christmas, my true love sent to me:'
        do 20 gift=day,1,-1
          write (*,*) trim(gifts(gift))
  20    continue
        write(*,*)
  10  continue
      end
```


 {{Out}}

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
