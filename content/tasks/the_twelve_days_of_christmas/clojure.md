+++
title = "Clojure"
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
[The Twelve Days of Christmas](../) done in Clojure.


## Clojure

```clojure
(let [numbers '(first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth)

      gifts   ["And a partridge in a pear tree",   "Two turtle doves",     "Three French hens",
               "Four calling birds",               "Five gold rings",      "Six geese a-laying",
               "Seven swans a-swimming",           "Eight maids a-miling", "Nine ladies dancing",
               "Ten lords a-leaping",              "Eleven pipers piping", "Twelve drummers drumming"]

       day     (fn [n] (printf "On the %s day of Christmas, my true love sent to me\n" (nth numbers n)))]

     (day 0)
     (println  (clojure.string/replace (first gifts) "And a" "A"))
     (dorun (for [d (range 1 12)] (do
       (println)
       (day d)
       (dorun (for [n (range d -1 -1)]
         (println (nth gifts n))))))))
```

```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```
