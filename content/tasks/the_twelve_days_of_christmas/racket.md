+++
title = "Racket"
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
[The Twelve Days of Christmas](../) done in Racket.


## Racket

This version:
* doesn't capitalise the word '''Twelfth'''
* capitalises the '''French'''   (in French hen)
* capitalised '''True Love''' as it may save me a lot of grief when I get home.
* British Variant: changes '''golden''' to '''go-old''' rings. Anyone who still has enough breath left to sing the second syllable after sustaining the first syllable of '''golden''' simply wasn't making enough effort in the first place.
* British Variant: capitalises '''FIVE GO-OLD RINGS''' since it needs to be sung at top volume. If you want to change this back; the source is there. But I guarantee you won't have as much fun singing it.


```racket
#lang racket
(define (ordinal-text d)
  (vector-ref
   (vector
    "zeroth" "first" "second" "third" "fourth"
    "fifth" "sixth" "seventh" "eighth" "ninth"
    "tenth" "eleventh" "twelfth")
   d))

(define (on-the... day)
  (printf "On the ~a day of Christmas,~%" (ordinal-text day))
  (printf "My True Love gave to me,~%"))

(define (prezzy prezzy-line day)
  (match prezzy-line
    [1 (string-append (if (= day 1) "A " "And a")" partridge in a pear tree")]
    [2 "Two turtle doves"]
    [3 "Three French hens"]
    [4 "Four calling birds"]
    [5 "FIVE GO-OLD RINGS"]
    [6 "Six geese a-laying"]
    [7 "Seven swans a-swimming"]
    [8 "Eight maids a-milking"]
    [9 "Nine ladies dancing"]
    [10 "Ten lords a-leaping"]
    [11 "Eleven pipers piping"]
    [12 "Twelve drummers drumming"]))

(define (line-end prezzy-line day)
  (match* (day prezzy-line) [(12 1) "."] [(x 1) ".\n"] [(_ _) ","]))

(for ((day (sequence-map add1 (in-range 12)))
      #:when (on-the... day)
      (prezzy-line (in-range day 0 -1)))
  (printf "~a~a~%" (prezzy prezzy-line day) (line-end prezzy-line day)))
```


```txt
On the first day of Christmas,
My True Love gave to me,
A  partridge in a pear tree.

On the second day of Christmas,
My True Love gave to me,
Two turtle doves,
And a partridge in a pear tree.

On the third day of Christmas,
My True Love gave to me,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the fourth day of Christmas,
My True Love gave to me,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the fifth day of Christmas,
My True Love gave to me,
FIVE GO-OLD RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the sixth day of Christmas,
My True Love gave to me,
Six geese a-laying,
FIVE GO-OLD RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the seventh day of Christmas, ...
On the eighth day of Christmas, ...
On the ninth day of Christmas, ...
On the tenth day of Christmas, ...
On the eleventh day of Christmas, ...

On the twelfth day of Christmas,
My True Love gave to me,
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GO-OLD RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.
```
