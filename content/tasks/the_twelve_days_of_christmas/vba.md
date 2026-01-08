+++
title = "VBA"
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
[The Twelve Days of Christmas](../) done in VBA.


## VBA

```vb
Sub Main()
Dim i As Integer, c As Integer, j As Integer, strReturn() As String
Dim s, n
   s = Split(SING_, "$")
   n = Split(NUMBERS_, " ")
   ReDim strReturn(UBound(s))
   For i = LBound(s) To UBound(s)
      strReturn(i) = Replace(BASE_, "(X)", n(i))
      For j = c To 0 Step -1
         strReturn(i) = strReturn(i) & s(j) & vbCrLf
      Next
      c = c + 1
   Next i
   strReturn(UBound(strReturn)) = Replace(strReturn(UBound(strReturn)), "and" & vbCrLf & "A", vbCrLf & "And a")
   Debug.Print Join(strReturn, vbCrLf)
End Sub
```

```txt
On the first day of Christmas,
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas,
My true love gave to me:
Two turtle doves and
A partridge in a pear tree.

On the third day of Christmas,
My true love gave to me:
Three french hens
Two turtle doves and
A partridge in a pear tree.
[...]
On the twelfth day of Christmas,
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
And a partridge in a pear tree.
```
