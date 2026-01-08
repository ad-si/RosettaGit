+++
title = "Ring"
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
[The Twelve Days of Christmas](../) done in Ring.


## Ring

```ring

# Project : The Twelve Days of Christmas

gifts = "A partridge in a pear tree,Two turtle doves,Three french hens,Four calling birds,Five golden rings,Six geese a-laying,Seven swans a-swimming,Eight maids a-milking,Nine ladies dancing,Ten lords a-leaping,Eleven pipers piping,Twelve drummers drumming"
days = "first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth"
lstgifts = str2list(substr(gifts,",", nl))
lstdays = str2list(substr(days, " ", nl))
for i = 1 to 12
     see "On the "+ lstdays[i]+ " day of Christmas" + nl
     see "My true love gave to me:" + nl
     for j = i to 1 step -1
          if i > 1 and j = 1
             see "and " + nl
          ok
          see "" + lstgifts[j] + nl
     next
     see nl
next


```

Output:

```txt

On the first day of Christmas
My true love gave to me:
A partridge in a pear tree

On the second day of Christmas
My true love gave to me:
Two turtle doves
and
A partridge in a pear tree

On the third day of Christmas
My true love gave to me:
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the fourth day of Christmas
My true love gave to me:
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the fifth day of Christmas
My true love gave to me:
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the sixth day of Christmas
My true love gave to me:
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the seventh day of Christmas
My true love gave to me:
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the eighth day of Christmas
My true love gave to me:
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the ninth day of Christmas
My true love gave to me:
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the tenth day of Christmas
My true love gave to me:
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the eleventh day of Christmas
My true love gave to me:
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the twelfth day of Christmas
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

```
