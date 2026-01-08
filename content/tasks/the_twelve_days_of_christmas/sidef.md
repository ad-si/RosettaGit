+++
title = "Sidef"
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
[The Twelve Days of Christmas](../) done in Sidef.


## Sidef

```ruby
var days = <first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth>
;

var gifts = <<'EOT'.lines;
  And a partridge in a pear tree.
  Two turtle doves,
  Three french hens,
  Four calling birds,
  Five golden rings,
  Six geese a-laying,
  Seven swans a-swimming,
  Eight maids a-milking,
  Nine ladies dancing,
  Ten lords a-leaping,
  Eleven pipers piping,
  Twelve drummers drumming,
EOT

func nth(n) { say "On the #{days[n]} day of Christmas, my true love gave to me:" };

nth(0);
say gifts[0].sub('And a', 'A');

range(1, 11).each { |d|
    say '';
    nth(d);
    d.downto(0).each { |i|
        say gifts[i];
    }
}
```


```txt

On the first day of Christmas, my true love gave to me:
  A partridge in a pear tree.

On the second day of Christmas, my true love gave to me:
  Two turtle doves,
  And a partridge in a pear tree.

On the third day of Christmas, my true love gave to me:
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.

On the fourth day of Christmas, my true love gave to me:
  Four calling birds,
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.
.
.
.
On the twelfth day of Christmas, my true love gave to me:
  Twelve drummers drumming,
  Eleven pipers piping,
  Ten lords a-leaping,
  Nine ladies dancing,
  Eight maids a-milking,
  Seven swans a-swimming,
  Six geese a-laying,
  Five golden rings,
  Four calling birds,
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.

```
