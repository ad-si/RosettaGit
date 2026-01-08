+++
title = "PureBasic"
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
[The Twelve Days of Christmas](../) done in PureBasic.


## PureBasic

```PureBasic
#TXT$ = "On the * day of Christmas, my true love sent to me:"
days$ = ~"first\nsecond\nthird\nfourth\nfifth\nsixth\nseventh\neighth\nninth\ntenth\neleventh\ntwelfth\n"
gifts$= ~"Twelve drummers drumming,\nEleven pipers piping,\nTen lords a-leaping,\nNine ladies dancing,\n"+
        ~"Eight maids a-milking,\nSeven swans a-swimming,\nSix geese a-laying,\nFive golden rings,\n"+
        ~"Four calling birds,\nThree french hens,\nTwo turtle doves,\nA partridge in a pear tree.\n"
Define  I.i, J.i

If OpenConsole("The twelve days of Christmas")
  For I = 1 To 12
    PrintN(ReplaceString(#TXT$,"*",StringField(days$,I,~"\n")))
    For J = 13-I To 12
      PrintN(" -> "+StringField(gifts$,J,~"\n"))
    Next J
  Next I
  Input()
EndIf
```

```txt
On the first day of Christmas, my true love sent to me:
 -> A partridge in a pear tree.
On the second day of Christmas, my true love sent to me:
 -> Two turtle doves,
 -> A partridge in a pear tree.
.
.
.
On the eleventh day of Christmas, my true love sent to me:
 -> Eleven pipers piping,
 -> Ten lords a-leaping,
 -> Nine ladies dancing,
 -> Eight maids a-milking,
 -> Seven swans a-swimming,
 -> Six geese a-laying,
 -> Five golden rings,
 -> Four calling birds,
 -> Three french hens,
 -> Two turtle doves,
 -> A partridge in a pear tree.
On the twelfth day of Christmas, my true love sent to me:
 -> Twelve drummers drumming,
 -> Eleven pipers piping,
 -> Ten lords a-leaping,
 -> Nine ladies dancing,
 -> Eight maids a-milking,
 -> Seven swans a-swimming,
 -> Six geese a-laying,
 -> Five golden rings,
 -> Four calling birds,
 -> Three french hens,
 -> Two turtle doves,
 -> A partridge in a pear tree.
```
