+++
title = "REXX"
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
[The Twelve Days of Christmas](../) done in REXX.


## REXX

This version:
::*   doesn't capitalize the word   '''Twelfth'''
::*   capitalizes the   '''French'''   (in French hen)
::*   capitalized   '''True Love'''   as it (may) refer to a deity
::*   added indentation to make verses resemble song lyrics

```rexx
/*REXX program displays the verses of the song:    "The 12 days of Christmas".          */
ordD= 'first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth'
pad= left('', 20)                                /*used for indenting the shown verses. */
                   @.1= 'A partridge in a pear-tree.';   @.7 = "Seven swans a-swimming,"
                   @.2= 'Two Turtle Doves, and'      ;   @.8 = "Eight maids a-milking,"
                   @.3= 'Three French Hens,'         ;   @.9 = "Nine ladies dancing,"
                   @.4= 'Four Calling Birds,'        ;   @.10= "Ten lords a-leaping,"
                   @.5= 'Five Golden Rings,'         ;   @.11= "Eleven pipers piping,"
                   @.6= 'Six geese a-laying,'        ;   @.12= "Twelve drummers drumming,"
  do day=1  for 12
  say pad  'On the'   word(ordD, day)   "day of Christmas"    /*display line 1 prologue.*/
  say pad  'My True Love gave to me:'                         /*   "      "  2     "    */
              do j=day  by -1  to 1;       say pad @.j        /*   "    the daily gifts.*/
              end   /*j*/
  say                                            /*add a blank line between the verses. */
  end               /*day*/                      /*stick a fork in it,  we're all done. */
```

(Shown at three-quarters size.)

<pre style="font-size:75%;height:55ex">
                     On the first day of Christmas
                     My True Love gave to me:
                     A partridge in a pear-tree.

                     On the second day of Christmas
                     My True Love gave to me:
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the third day of Christmas
                     My True Love gave to me:
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the fourth day of Christmas
                     My True Love gave to me:
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the fifth day of Christmas
                     My True Love gave to me:
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the sixth day of Christmas
                     My True Love gave to me:
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the seventh day of Christmas
                     My True Love gave to me:
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the eighth day of Christmas
                     My True Love gave to me:
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the ninth day of Christmas
                     My True Love gave to me:
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the tenth day of Christmas
                     My True Love gave to me:
                     Ten lords a-leaping,
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the eleventh day of Christmas
                     My True Love gave to me:
                     Eleven pipers piping,
                     Ten lords a-leaping,
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the twelfth day of Christmas
                     My True Love gave to me:
                     Twelve drummers drumming,
                     Eleven pipers piping,
                     Ten lords a-leaping,
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

```
