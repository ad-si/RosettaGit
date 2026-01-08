+++
title = "C"
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
[The Twelve Days of Christmas](../) done in C.


## C

```C

#include<stdio.h>

int main()
{
    int i,j;

    char days[12][10] =
    {
        "First",
        "Second",
        "Third",
        "Fourth",
        "Fifth",
        "Sixth",
        "Seventh",
        "Eighth",
        "Ninth",
        "Tenth",
        "Eleventh",
        "Twelfth"
    };

    char gifts[12][33] =
    {
        "Twelve drummers drumming",
        "Eleven pipers piping",
        "Ten lords a-leaping",
        "Nine ladies dancing",
        "Eight maids a-milking",
        "Seven swans a-swimming",
        "Six geese a-laying",
        "Five golden rings",
        "Four calling birds",
        "Three french hens",
        "Two turtle doves",
        "And a partridge in a pear tree."
    };

    for(i=0;i<12;i++)
    {
        printf("\n\nOn the %s day of Christmas\nMy true love gave to me:",days[i]);

        for(j=i;j>=0;j--)
        {
            (i==0)?printf("\nA partridge in a pear tree."):printf("\n%s%c",gifts[11-j],(j!=0)?',':' ');
        }
    }

    return 0;
}
```
