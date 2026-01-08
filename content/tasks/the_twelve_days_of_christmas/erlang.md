+++
title = "Erlang"
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
[The Twelve Days of Christmas](../) done in Erlang.


## Erlang

```erlang
-module(twelve_days).
-export([gifts_for_day/1]).

names(N) -> lists:nth(N,
              ["first",   "second", "third", "fourth", "fifth",    "sixth",
               "seventh", "eighth", "ninth", "tenth",  "eleventh", "twelfth" ]).

gifts() -> [ "A partridge in a pear tree.", "Two turtle doves and",
             "Three French hens,",          "Four calling birds,",
             "Five gold rings,",            "Six geese a-laying,",
             "Seven swans a-swimming,",     "Eight maids a-milking,",
             "Nine ladies dancing,",        "Ten lords a-leaping,",
             "Eleven pipers piping,",       "Twelve drummers drumming," ].

gifts_for_day(N) ->
  "On the " ++ names(N) ++ " day of Christmas, my true love sent to me:\n" ++
  string:join(lists:reverse(lists:sublist(gifts(), N)), "\n").

main(_) -> lists:map(fun(N) -> io:fwrite("~s~n~n", [gifts_for_day(N)]) end,
                     lists:seq(1,12)).

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



=={{header|F Sharp|F#}}==

```fsharp
let gifts = [
    "And a partridge in a pear tree";
    "Two turtle doves";
    "Three french hens";
    "Four calling birds";
    "FIVE GOLDEN RINGS";
    "Six geese a-laying";
    "Seven swans a-swimming";
    "Eight maids a-milking";
    "Nine ladies dancing";
    "Ten lords a-leaping";
    "Eleven pipers piping";
    "Twelve drummers drumming"
]

let days = [
    "first"; "second"; "third"; "fourth"; "fifth"; "sixth"; "seventh"; "eighth";
    "ninth"; "tenth"; "eleventh"; "twelfth"
]

let displayGifts day =
    printfn "On the %s day of Christmas, my true love gave to me" days.[day]
    if day = 0 then
        printfn "A partridge in a pear tree"
    else
        List.iter (fun i -> printfn "%s" gifts.[i]) [day..(-1)..0]
    printf "\n"

List.iter displayGifts [0..11]
```
