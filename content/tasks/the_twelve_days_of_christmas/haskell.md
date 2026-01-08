+++
title = "Haskell"
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
[The Twelve Days of Christmas](../) done in Haskell.


## Haskell

```haskell
gifts :: [String]
gifts = [
    "And a partridge in a pear tree!",
    "Two turtle doves,",
    "Three french hens,",
    "Four calling birds,",
    "FIVE GOLDEN RINGS,",
    "Six geese a-laying,",
    "Seven swans a-swimming,",
    "Eight maids a-milking,",
    "Nine ladies dancing,",
    "Ten lords a-leaping,",
    "Eleven pipers piping,",
    "Twelve drummers drumming," ]

days :: [String]
days = [
    "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth",
    "ninth", "tenth", "eleventh", "twelfth" ]

verseOfTheDay :: Int -> IO ()
verseOfTheDay day = do
    putStrLn $ "On the " ++ days !! day ++ " day of Christmas my true love gave to me... "
    mapM_ putStrLn [dayGift day d | d <- [day, day-1..0]]
    putStrLn ""
    where dayGift 0 _ = "A partridge in a pear tree!"
          dayGift _ gift = gifts !! gift

main :: IO ()
main = mapM_ verseOfTheDay [0..11]

```

<pre style="font-size:80%">On the first day of Christmas my true love gave to me...
A partridge in a pear tree!

On the second day of Christmas my true love gave to me...
Two turtle doves,
And a partridge in a pear tree!

On the third day of Christmas my true love gave to me...
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the fourth day of Christmas my true love gave to me...
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the fifth day of Christmas my true love gave to me...
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the sixth day of Christmas my true love gave to me...
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the seventh day of Christmas my true love gave to me...
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the eighth day of Christmas my true love gave to me...
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the ninth day of Christmas my true love gave to me...
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the tenth day of Christmas my true love gave to me...
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the eleventh day of Christmas my true love gave to me...
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the twelfth day of Christmas my true love gave to me...
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!


```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages.

```unicon
procedure main()
    days := ["first","second","third","fourth","fifth","sixth","seventh",
             "eighth","ninth","tenth","eleventh","twelveth"]
    gifts := ["A partridge in a pear tree.", "Two turtle doves and",
              "Three french hens,", "Four calling birds,",
              "Five golden rings,", "Six geese a-laying,",
              "Seven swans a-swimming,", "Eight maids a-milking,",
              "Nine ladies dancing,", "Ten lords a-leaping,",
              "Eleven pipers piping,", "Twelve drummers drumming,"]

    every write("\nOn the ",days[day := 1 to 12]," day of Christmas my true love gave to me:") do
        every write(" ",gifts[day to 1 by -1])
end
```
