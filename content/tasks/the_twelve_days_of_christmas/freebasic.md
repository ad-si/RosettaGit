+++
title = "FreeBASIC"
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
[The Twelve Days of Christmas](../) done in FreeBASIC.


## FreeBASIC

```freebasic
' version 10-01-2017
' compile with: fbc -s console

Dim As ULong d, r

Dim As String days(1 To ...) = { "first", "second", "third", "fourth", _
                                "fifth", "sixth", "seventh", "eighth", _
                              "ninth", "tenth", "eleventh", "twelfth" }

Dim As String gifts(1 To ...) = { "", " Two turtle doves", _
              " Three french hens", " Four calling birds", _
              " Five golden rings", " Six geese a-laying", _
      " Seven swans a-swimming", " Eight maids a-milking", _
           " Nine ladies dancing", " Ten lords a-leaping", _
     " Eleven pipers piping", " Twelve drummers drumming" }

For d = 1 To 12
    Print " On the " + days(d) + " day of Christmas"
    Print " My true love gave to me:"
    For r = d To 3 Step -1
        Print gifts(r)
    Next
    ' print " Two turtle doves" for the twelfth day and add "and" for the other days
    If d > 1 Then
        Print gifts(2); iif(d = 12, "", " and")
    End If
    ' print "A partridge...", on the twelfth day print "And a partrige..."
    Print " A" & IIf(d = 12, "nd a", "" ) & " partridge in a pear tree"
    Print
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : 'Print "hit any key to end program"
Sleep
End
```

```txt
 On the first day of Christmas
 My true love gave to me:
 A partridge in a pear tree

 On the second day of Christmas
 My true love gave to me:
 Two turtle doves and
 A partridge in a pear tree

 '''

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
 And a partridge in a pear tree
```
