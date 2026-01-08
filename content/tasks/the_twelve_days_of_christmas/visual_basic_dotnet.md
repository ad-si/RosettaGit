+++
title = "Visual Basic .NET"
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
[The Twelve Days of Christmas](../) done in Visual Basic .NET.


## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic (language version >= 14, e.g. with Visual Studio 2015)


```vbnet
Module Program
    Sub Main()
        Dim days = New String(11) {"first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"}
        Dim gifts = New String(11) {
            "A partridge in a pear tree",
            "Two turtle doves",
            "Three french hens",
            "Four calling birds",
            "Five golden rings",
            "Six geese a-laying",
            "Seven swans a-swimming",
            "Eight maids a-milking",
            "Nine ladies dancing",
            "Ten lords a-leaping",
            "Eleven pipers piping",
            "Twelve drummers drumming"
        }

        For i = 0 To 11
            Console.WriteLine($"On the {days(i)} day of Christmas, my true love gave to me")

            For j = i To 0 Step -1
                Console.WriteLine(gifts(j))
            Next

            Console.WriteLine()

            If i = 0 Then gifts(0) = "And a partridge in a pear tree"
        Next
    End Sub
End Module
```
