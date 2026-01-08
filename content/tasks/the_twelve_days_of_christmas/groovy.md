+++
title = "Groovy"
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
[The Twelve Days of Christmas](../) done in Groovy.


## Groovy

```groovy
def presents = ['A partridge in a pear tree.', 'Two turtle doves', 'Three french hens', 'Four calling birds',
        'Five golden rings', 'Six geese a-laying', 'Seven swans a-swimming', 'Eight maids a-milking',
        'Nine ladies dancing', 'Ten lords a-leaping', 'Eleven pipers piping', 'Twelve drummers drumming']
['first', 'second', 'third', 'forth', 'fifth', 'sixth', 'seventh', 'eight', 'ninth', 'tenth', 'eleventh', 'Twelfth'].eachWithIndex{ day, dayIndex ->
    println "On the $day day of Christmas"
    println 'My true love gave to me:'
    (dayIndex..0).each { p ->
        print presents[p]
        println p == 1 ? ' and' : ''
    }
    println()
}
```
