+++
title = "Swift"
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
[The Twelve Days of Christmas](../) done in Swift.


## Swift

```swift
let gifts = [ "partridge in a pear tree", "Two turtle doves",
              "Three French hens",        "Four calling birds",
              "Five gold rings",          "Six geese a-laying",
              "Seven swans a-swimming",   "Eight maids a-milking",
              "Nine ladies dancing",      "Ten lords a-leaping",
              "Eleven pipers piping",     "Twelve drummers drumming" ]

let nth = [ "first",   "second", "third", "fourth", "fifth",    "sixth",
            "seventh", "eighth", "ninth", "tenth",  "eleventh", "twelfth" ]

func giftsForDay(day: Int) -> String {
  var result = "On the \(nth[day-1]) day of Christmas, my true love sent to me:\n"
  if day > 1 {
    for again in 1...day-1 {
      let n = day - again
      result += gifts[n]
      if n > 1 { result += "," }
      result += "\n"
    }
    result += "And a "
  } else {
    result += "A "
  }
  return result + gifts[0] + ".\n";
}

for day in 1...12 {
  print(giftsForDay(day))
}
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
