+++
title = "The Twelve Days of Christmas"
description = ""
date = 2019-10-18T20:29:35Z
aliases = []
[extra]
id = 16950
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "actionscript",
  "ada",
  "algol_68",
  "applescript",
  "autohotkey",
  "awk",
  "batch_file",
  "befunge",
  "bracmat",
  "c",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "dc",
  "eiffel",
  "elena",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "fsharp",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "logo",
  "lolcode",
  "lua",
  "maple",
  "mathematica",
  "miniscript",
  "nim",
  "objeck",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "seed7",
  "self",
  "sidef",
  "simula",
  "smalltalk",
  "smart_basic",
  "snobol",
  "sql",
  "swift",
  "tailspin",
  "tcl",
  "unicon",
  "unix_shell",
  "vba",
  "vbscript",
  "visual_basic_dotnet",
  "zkl",
]
+++

## Task

Write a program that outputs the lyrics of the Christmas carol ''The Twelve Days of Christmas''.
The lyrics can be found [here](http://www.lyricsmode.com/lyrics/c/christmas_carols/the_twelve_days_of_christmas.html).

(You must reproduce the words in the correct order, but case, format, and punctuation are left to your discretion.)


## Related tasks

*   [[99 Bottles of Beer]]
*   [[Old_lady_swallowed_a_fly]]
*   [[Comma quibbling]]





## ActionScript

This program outputs the lyrics to a TextField object.
The text field can be scrolled using the mouse wheel (Windows only) or by using the up/down arrow keys on the keyboard.

{{works with|Adobe AIR|AIR|1.5}} (Although the code can work in Flash Player 9 by replacing the Vectors with Arrays)


{{ code(src="content/tasks/the_twelve_days_of_christmas/actionscript.as", lang="ActionScript") }}




## ALGOL 68


{{ code(src="content/tasks/the_twelve_days_of_christmas/algol_68.a68", lang="algol68") }}



```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves, and
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
Two turtle doves, and
A partridge in a pear tree.
```



## AppleScript


### Iterative



{{ code(src="content/tasks/the_twelve_days_of_christmas/applescript_1.applescript", lang="applescript") }}



```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves, and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves, and
A partridge in a pear tree.
```




### Functional composition


Drawing on some functional primitives, and post-Yosemite AppleScript's ability to import Foundation classes:



{{ code(src="content/tasks/the_twelve_days_of_christmas/applescript_2.applescript", lang="AppleScript") }}



```txt
On the first day of Xmas, my true love gave to me ...
A partridge in a pear tree !

On the second day of Xmas, my true love gave to me ...
Two turtle doves,
And a partridge in a pear tree !

...

On the twelfth day of Xmas, my true love gave to me ...
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three French hens,
Two turtle doves,
And a partridge in a pear tree !
```



## Ada



{{ code(src="content/tasks/the_twelve_days_of_christmas/ada.adb", lang="Ada") }}




## AutoHotkey



{{ code(src="content/tasks/the_twelve_days_of_christmas/autohotkey.ahk", lang="AutoHotkey") }}



## AWK



{{ code(src="content/tasks/the_twelve_days_of_christmas/awk.awk", lang="AWK") }}


```txt

On the first day of Christmas,
my true love gave to me:
a partridge in a pear tree.

On the second day of Christmas,
my true love gave to me:
two turtle doves, and
a partridge in a pear tree.

...

On the twelfth day of Christmas,
my true love gave to me:
twelve drummers drumming,
eleven pipers piping,
ten lords a-leaping,
nine ladies dancing,
eight maids a-milking,
seven swans a-swimming,
six geese a-laying,
five golden rings,
four calling birds,
three french hens,
two turtle doves, and
a partridge in a pear tree.

```



## Batch File



{{ code(src="content/tasks/the_twelve_days_of_christmas/batch_file.bat", lang="dos") }}




## Bracmat



{{ code(src="content/tasks/the_twelve_days_of_christmas/bracmat.bra", lang="bracmat") }}




## Befunge

This is essentially the same algorithm as [[Old_lady_swallowed_a_fly#Befunge|Old lady swallowed a fly]] - just a different set of phrases and a simpler song pattern.


{{ code(src="content/tasks/the_twelve_days_of_christmas/befunge.befunge", lang="befunge") }}




## C



{{ code(src="content/tasks/the_twelve_days_of_christmas/c.c", lang="C") }}




## C++



{{ code(src="content/tasks/the_twelve_days_of_christmas/cpp.cpp", lang="cpp") }}



## C#


{{ code(src="content/tasks/the_twelve_days_of_christmas/csharp.cs", lang="c#") }}




## Clojure


{{ code(src="content/tasks/the_twelve_days_of_christmas/clojure.clj", lang="clojure") }}


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



## COBOL


{{ code(src="content/tasks/the_twelve_days_of_christmas/cobol.cob", lang="cobol") }}




## Common Lisp




{{ code(src="content/tasks/the_twelve_days_of_christmas/common_lisp.lisp", lang="lisp") }}



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



## Crystal


{{ code(src="content/tasks/the_twelve_days_of_christmas/crystal.cr", lang="ruby") }}




## D


{{ code(src="content/tasks/the_twelve_days_of_christmas/d.d", lang="d") }}




## dc



{{ code(src="content/tasks/the_twelve_days_of_christmas/dc.dc", lang="dc") }}



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



## Eiffel



{{ code(src="content/tasks/the_twelve_days_of_christmas/eiffel.e", lang="Eiffel") }}



## Elena

ELENA 4.1 :


{{ code(src="content/tasks/the_twelve_days_of_christmas/elena.l", lang="elena") }}




## Elixir



{{ code(src="content/tasks/the_twelve_days_of_christmas/elixir.exs", lang="elixir") }}



```txt

On the first day of Christmas
My true love gave to me:
A partridge in a pear tree

On the second day of Christmas
My true love gave to me:
Two turtle doves and
A partridge in a pear tree

...

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
Two turtle doves and
A partridge in a pear tree
```



## Erlang



{{ code(src="content/tasks/the_twelve_days_of_christmas/erlang.erl", lang="erlang") }}


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



## F#


{{ code(src="content/tasks/the_twelve_days_of_christmas/fsharp.fs", lang="fsharp") }}




## Factor



{{ code(src="content/tasks/the_twelve_days_of_christmas/factor.factor", lang="factor") }}


```txt

On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves, and
A partridge in a pear tree.

...

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

```



## Forth


{{ code(src="content/tasks/the_twelve_days_of_christmas/forth.4th", lang="forth") }}



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



## Fortran


{{ code(src="content/tasks/the_twelve_days_of_christmas/fortran.f90", lang="fortran") }}



 {{Out}}

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


## FreeBASIC



{{ code(src="content/tasks/the_twelve_days_of_christmas/freebasic.bas", lang="freebasic") }}


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



## Go

[Go Playground](https://play.golang.org/p/dnDyx8ee_G)


{{ code(src="content/tasks/the_twelve_days_of_christmas/go.go", lang="go") }}




## Groovy



{{ code(src="content/tasks/the_twelve_days_of_christmas/groovy.groovy", lang="groovy") }}




## Haskell


{{ code(src="content/tasks/the_twelve_days_of_christmas/haskell.hs", lang="haskell") }}


```txt
On the first day of Christmas my true love gave to me...
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


## Icon and Unicon

Works in both languages.


{{ code(src="content/tasks/the_twelve_days_of_christmas/unicon.icn", lang="unicon") }}




## J



{{ code(src="content/tasks/the_twelve_days_of_christmas/j.ijs", lang="j") }}




## Java



{{ code(src="content/tasks/the_twelve_days_of_christmas/java.java", lang="java") }}




## JavaScript



{{ code(src="content/tasks/the_twelve_days_of_christmas/javascript_1.js", lang="JavaScript") }}




Alternatively, in a functional style of JavaScript, we can define the ancient song "strPrepn the lstOrdinal[i] strUnit of strHoliday" as an expression, and return that expression in a human-legible and machine-parseable JSON string translation, for further analysis and processing :-)



{{ code(src="content/tasks/the_twelve_days_of_christmas/javascript_2.js", lang="JavaScript") }}



Note that the Google Closure compiler's translation of this would be half the size, but rather less legible.
(It does make interesting suggestions though – the semi-colon segmentation of the verses below is a trick that might be worth remembering).



{{ code(src="content/tasks/the_twelve_days_of_christmas/javascript_3.js", lang="JavaScript") }}



Formatted JSON output (the expanded and Closure-compiled versions above both yield the same output).



{{ code(src="content/tasks/the_twelve_days_of_christmas/javascript_4.js", lang="JavaScript") }}




## jq



{{ code(src="content/tasks/the_twelve_days_of_christmas/jq.jq", lang="jq") }}



Run with
```txt
jq -rnf programfile.jq
```
 to yield this result:

```txt
On the 1st day of Christmas, my true love gave to me
a partridge in a pear tree.

On the 2nd day of Christmas, my true love gave to me
2 turtle doves
and a partridge in a pear tree.

On the 3rd day of Christmas, my true love gave to me
3 French hens,
2 turtle doves,
and a partridge in a pear tree.

[...]

On the 12th day of Christmas, my true love gave to me
12 drummers drumming,
11 pipers piping,
10 lords a-leaping,
9 ladies dancing,
8 maids a-milking,
7 swans a-swimming,
6 geese a-laying,
5 gold rings,
4 calling birds,
3 French hens,
2 turtle doves,
and a partridge in a pear tree.
```



## Jsish

Based on Javascript entry, almost identical, added unitTest.


{{ code(src="content/tasks/the_twelve_days_of_christmas/jsish.jsi", lang="javascript") }}



The verses are in the unitTest. --U mode to show the echo mode output skipped here.

```txt
prompt$ jsish -u twelveDaysOfChristmas.jsi
[PASS] twelveDaysOfChristmas.jsi
```



## Julia



{{ code(src="content/tasks/the_twelve_days_of_christmas/julia.jl", lang="julia") }}



```txt
On the first day of Christmas
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas
My true love gave to me:
Two turtle doves and
A partridge in a pear tree.

[...]

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
Two turtle doves and
A partridge in a pear tree.
```



## Kotlin


{{ code(src="content/tasks/the_twelve_days_of_christmas/kotlin.kt", lang="scala") }}




## Logo




{{ code(src="content/tasks/the_twelve_days_of_christmas/logo.lg", lang="logo") }}



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



## LOLCODE


{{ code(src="content/tasks/the_twelve_days_of_christmas/lolcode.lol", lang="lolcode") }}



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



## Lua



{{ code(src="content/tasks/the_twelve_days_of_christmas/lua.lua", lang="Lua") }}




## Maple



{{ code(src="content/tasks/the_twelve_days_of_christmas/maple.mpl", lang="maple") }}


```txt

On the first day of Christmas
My true love gave to me:
A partridge in a pear tree

On the second day of Christmas
My true love gave to me:
Two turtle doves and
A partridge in a pear tree

On the third day of Christmas
My true love gave to me:
Three french hens
Two turtle doves and
A partridge in a pear tree

...

On the eleventh day of Christmas
My true love gave to me:
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

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
Two turtle doves and
A partridge in a pear tree

```



## Mathematica



{{ code(src="content/tasks/the_twelve_days_of_christmas/mathematica.m", lang="Mathematica") }}


```txt
On the first day of Christmas, my true love gave to me:
A partridge in a pear tree.

On the second day of Christmas, my true love gave to me:
Two turtle doves,
And a partridge in a pear tree.

On the third day of Christmas, my true love gave to me:
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the fourth day of Christmas, my true love gave to me:
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the fifth day of Christmas, my true love gave to me:
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the sixth day of Christmas, my true love gave to me:
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the seventh day of Christmas, my true love gave to me:
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the eighth day of Christmas, my true love gave to me:
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the ninth day of Christmas, my true love gave to me:
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the tenth day of Christmas, my true love gave to me:
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the eleventh day of Christmas, my true love gave to me:
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
And a partridge in a pear tree.

On the twelfth day of Christmas, my true love gave to me:
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
And a partridge in a pear tree.

```



## MiniScript



{{ code(src="content/tasks/the_twelve_days_of_christmas/miniscript.ms", lang="MiniScript") }}


```txt

On the first day of Christmas,
my true love gave to me,
A partridge in a pear tree.
----------

<and so on until the last>

On the twelfth day of Christmas,
my true love gave to me,
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
Two turtle doves, and
A partridge in a pear tree.
----------

```



## Nim


{{ code(src="content/tasks/the_twelve_days_of_christmas/nim.nim", lang="nim") }}




## Objeck


{{ code(src="content/tasks/the_twelve_days_of_christmas/objeck.obs", lang="objeck") }}




## PARI/GP



{{ code(src="content/tasks/the_twelve_days_of_christmas/pari_gp.gp", lang="parigp") }}


```txt
On the first day of Christmas, my true love gave to me:
        A partridge in a pear tree.
On the second day of Christmas, my true love gave to me:
        Two turtle doves,
        And a partridge in a pear tree.
[...]
On the twelfth day of Christmas, my true love gave to me:
        Twelve drummers drumming,
        Eleven pipers piping,
        Ten lords a-leaping,
        Nine ladies dancing,
        Eight maids a-milking,
        Seven swans a-swimming,
        Six geese a-laying,
        Five golden rings,
        Four calling birds,
        Three french hens,
        Two turtle doves,
        And a partridge in a pear tree.
```



## Pascal

This should work with any modern Pascal implementation that has a '''string''' type, e.g. [[Free Pascal]].



{{ code(src="content/tasks/the_twelve_days_of_christmas/pascal_1.pas", lang="pascal") }}


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


Here's a version that works in ISO Standard Pascal, albeit with extraneous spaces in the output:



{{ code(src="content/tasks/the_twelve_days_of_christmas/pascal_2.pas", lang="pascal") }}



```txt
On the first    day of Christmas, my true love gave to me:
A partridge in a pear tree.

On the second   day of Christmas, my true love gave to me:
Two turtle doves and
A partridge in a pear tree.
```


[...]


```txt
On the twelfth  day of Christmas, my true love gave to me:
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



## Perl



{{ code(src="content/tasks/the_twelve_days_of_christmas/perl.pl", lang="perl") }}


```txt
On the first day of Christmas,
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas,
My true love gave to me:
Two turtle doves
And a partridge in a pear tree.

...

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



## Perl 6



{{ code(src="content/tasks/the_twelve_days_of_christmas/perl_6.p6", lang="perl6") }}


```txt
On the first day of Christmas, my true love gave to me:
  A partridge in a pear tree.

On the second day of Christmas, my true love gave to me:
  Two turtle doves,
  And a partridge in a pear tree.

On the third day of Christmas, my true love gave to me:
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.

On the fourth day of Christmas, my true love gave to me:
  Four calling birds,
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.
.
.
.
On the twelfth day of Christmas, my true love gave to me:
  Twelve drummers drumming,
  Eleven pipers piping,
  Ten lords a-leaping,
  Nine ladies dancing,
  Eight maids a-milking,
  Seven swans a-swimming,
  Six geese a-laying,
  Five golden rings,
  Four calling birds,
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.
```



## Phix



{{ code(src="content/tasks/the_twelve_days_of_christmas/phix.exw", lang="Phix") }}




## PicoLisp



{{ code(src="content/tasks/the_twelve_days_of_christmas/picolisp.l", lang="PicoLisp") }}




## PHP



{{ code(src="content/tasks/the_twelve_days_of_christmas/php_1.php", lang="PHP") }}



Or using recursion:



{{ code(src="content/tasks/the_twelve_days_of_christmas/php_2.php", lang="PHP") }}


```txt
On the first day of Xmas, my true love gave to me
A partridge in a pear tree

On the second day of Xmas, my true love gave to me
Two turtle doves and
A partridge in a pear tree

On the third day of Xmas, my true love gave to me
Three french hens
Two turtle doves and
A partridge in a pear tree

On the fourth day of Xmas, my true love gave to me
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the fifth day of Xmas, my true love gave to me
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the sixth day of Xmas, my true love gave to me
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the seventh day of Xmas, my true love gave to me
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the eighth day of Xmas, my true love gave to me
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the ninth day of Xmas, my true love gave to me
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the tenth day of Xmas, my true love gave to me
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the eleventh day of Xmas, my true love gave to me
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the twelfth day of Xmas, my true love gave to me
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
Two turtle doves and
A partridge in a pear tree

```



## PowerShell



{{ code(src="content/tasks/the_twelve_days_of_christmas/powershell.ps1", lang="PowerShell") }}



```txt
On the first day of Christmas
My true love gave to me
A partridge in a pear tree

On the second day of Christmas
My true love gave to me
Two turtle doves and
A partridge in a pear tree

...

On the twelfth day of Christmas
My true love gave to me
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
Two turtle doves and
A partridge in a pear tree

```



## Prolog



{{ code(src="content/tasks/the_twelve_days_of_christmas/prolog.pro", lang="prolog") }}



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



## PureBasic



{{ code(src="content/tasks/the_twelve_days_of_christmas/purebasic.pb", lang="PureBasic") }}


```txt
On the first day of Christmas, my true love sent to me:
 -> A partridge in a pear tree.
On the second day of Christmas, my true love sent to me:
 -> Two turtle doves,
 -> A partridge in a pear tree.
.
.
.
On the eleventh day of Christmas, my true love sent to me:
 -> Eleven pipers piping,
 -> Ten lords a-leaping,
 -> Nine ladies dancing,
 -> Eight maids a-milking,
 -> Seven swans a-swimming,
 -> Six geese a-laying,
 -> Five golden rings,
 -> Four calling birds,
 -> Three french hens,
 -> Two turtle doves,
 -> A partridge in a pear tree.
On the twelfth day of Christmas, my true love sent to me:
 -> Twelve drummers drumming,
 -> Eleven pipers piping,
 -> Ten lords a-leaping,
 -> Nine ladies dancing,
 -> Eight maids a-milking,
 -> Seven swans a-swimming,
 -> Six geese a-laying,
 -> Five golden rings,
 -> Four calling birds,
 -> Three french hens,
 -> Two turtle doves,
 -> A partridge in a pear tree.
```



## Python



{{ code(src="content/tasks/the_twelve_days_of_christmas/python.py", lang="python") }}



```txt
On the first day of Christmas
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas
My true love gave to me:
Two turtle doves and
A partridge in a pear tree.

On the third day of Christmas
My true love gave to me:
Three french hens
Two turtle doves and
A partridge in a pear tree.

On the fourth day of Christmas
My true love gave to me:
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree.
.
.
.

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
Two turtle doves and
A partridge in a pear tree.
```



## Racket


This version:
* doesn't capitalise the word '''Twelfth'''
* capitalises the '''French'''   (in French hen)
* capitalised '''True Love''' as it may save me a lot of grief when I get home.
* British Variant: changes '''golden''' to '''go-old''' rings. Anyone who still has enough breath left to sing the second syllable after sustaining the first syllable of '''golden''' simply wasn't making enough effort in the first place.
* British Variant: capitalises '''FIVE GO-OLD RINGS''' since it needs to be sung at top volume. If you want to change this back; the source is there. But I guarantee you won't have as much fun singing it.



{{ code(src="content/tasks/the_twelve_days_of_christmas/racket.rkt", lang="racket") }}



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



## REXX

This version:
::*   doesn't capitalize the word   '''Twelfth'''
::*   capitalizes the   '''French'''   (in French hen)
::*   capitalized   '''True Love'''   as it (may) refer to a deity
::*   added indentation to make verses resemble song lyrics


{{ code(src="content/tasks/the_twelve_days_of_christmas/rexx.rexx", lang="rexx") }}


(Shown at three-quarters size.)

```txt
                     On the first day of Christmas
                     My True Love gave to me:
                     A partridge in a pear-tree.

                     On the second day of Christmas
                     My True Love gave to me:
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the third day of Christmas
                     My True Love gave to me:
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the fourth day of Christmas
                     My True Love gave to me:
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the fifth day of Christmas
                     My True Love gave to me:
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the sixth day of Christmas
                     My True Love gave to me:
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the seventh day of Christmas
                     My True Love gave to me:
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the eighth day of Christmas
                     My True Love gave to me:
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the ninth day of Christmas
                     My True Love gave to me:
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the tenth day of Christmas
                     My True Love gave to me:
                     Ten lords a-leaping,
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the eleventh day of Christmas
                     My True Love gave to me:
                     Eleven pipers piping,
                     Ten lords a-leaping,
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the twelfth day of Christmas
                     My True Love gave to me:
                     Twelve drummers drumming,
                     Eleven pipers piping,
                     Ten lords a-leaping,
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

```



## Ring



{{ code(src="content/tasks/the_twelve_days_of_christmas/ring.ring", lang="ring") }}


Output:

```txt

On the first day of Christmas
My true love gave to me:
A partridge in a pear tree

On the second day of Christmas
My true love gave to me:
Two turtle doves
and
A partridge in a pear tree

On the third day of Christmas
My true love gave to me:
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the fourth day of Christmas
My true love gave to me:
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the fifth day of Christmas
My true love gave to me:
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the sixth day of Christmas
My true love gave to me:
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the seventh day of Christmas
My true love gave to me:
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the eighth day of Christmas
My true love gave to me:
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the ninth day of Christmas
My true love gave to me:
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the tenth day of Christmas
My true love gave to me:
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the eleventh day of Christmas
My true love gave to me:
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
and
A partridge in a pear tree

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
and
A partridge in a pear tree

```



## Ruby



{{ code(src="content/tasks/the_twelve_days_of_christmas/ruby.rb", lang="ruby") }}




## Run BASIC



{{ code(src="content/tasks/the_twelve_days_of_christmas/run_basic.bas", lang="Runbasic") }}

Output:

```txt
On the first day of Christmas
My true love gave to me:
A partridge in a pear tree.
.
.
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
and A partridge in a pear tree.
```



## Rust

[Rust Playground](https://play.rust-lang.org/?gist=773d4af97e7c4b374574a3e1656b5029&version=stable&backtrace=0)


{{ code(src="content/tasks/the_twelve_days_of_christmas/rust.rs", lang="rust") }}




## Scala



{{ code(src="content/tasks/the_twelve_days_of_christmas/scala.scala", lang="scala") }}



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



## Seed7



{{ code(src="content/tasks/the_twelve_days_of_christmas/seed7.sd7", lang="seed7") }}



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

...

On the Twelfth day of Christmas,
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
Two turtle doves and
A partridge in a pear tree.

```



## Self

Nicely factored:


{{ code(src="content/tasks/the_twelve_days_of_christmas/self.self", lang="self") }}




## Sidef


{{ code(src="content/tasks/the_twelve_days_of_christmas/sidef.sf", lang="ruby") }}



```txt

On the first day of Christmas, my true love gave to me:
  A partridge in a pear tree.

On the second day of Christmas, my true love gave to me:
  Two turtle doves,
  And a partridge in a pear tree.

On the third day of Christmas, my true love gave to me:
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.

On the fourth day of Christmas, my true love gave to me:
  Four calling birds,
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.
.
.
.
On the twelfth day of Christmas, my true love gave to me:
  Twelve drummers drumming,
  Eleven pipers piping,
  Ten lords a-leaping,
  Nine ladies dancing,
  Eight maids a-milking,
  Seven swans a-swimming,
  Six geese a-laying,
  Five golden rings,
  Four calling birds,
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.

```



## Simula


{{ code(src="content/tasks/the_twelve_days_of_christmas/simula.sim", lang="simula") }}



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



## Smalltalk


{{ code(src="content/tasks/the_twelve_days_of_christmas/smalltalk.st", lang="smalltalk") }}



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




## Smart BASIC



{{ code(src="content/tasks/the_twelve_days_of_christmas/smart_basic.bas", lang="smart BASIC") }}


```txt
On the first day of Christmas
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas
My true love gave to me:
Two turtle doves and
A partridge in a pear tree.

On the third day of Christmas
My true love gave to me:
Three french hens
Two turtle doves and
A partridge in a pear tree.

[ ... ]

On the eleventh day of Christmas
My true love gave to me:
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming,
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree.

On the Twelfth day of Christmas
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming,
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree.
```



## Snobol


{{ code(src="content/tasks/the_twelve_days_of_christmas/snobol.sno", lang="snobol") }}



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



## SQL

Demonstration of Oracle 12c "with" clause enhancement.



{{ code(src="content/tasks/the_twelve_days_of_christmas/sql.sql", lang="SQL") }}


output:

```txt

The Twelve Days of Christmas
--------------------------------------------------------------------------------
On the first day of Christmas,
my true love sent to me:
A partridge in a pear tree.

...

On the twelfth day of Christmas,
my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings!
Four calling birds,
Three French hens,
Two turtle doves,
And a partridge in a pear tree.

```



## Swift


{{ code(src="content/tasks/the_twelve_days_of_christmas/swift.swift", lang="swift") }}



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



## Tailspin



{{ code(src="content/tasks/the_twelve_days_of_christmas/tailspin.tt", lang="tailspin") }}


```txt

On the first day of Christmas,
my true love gave to me:
a partridge in a pear tree.

On the second day of Christmas,
my true love gave to me:
two turtle-doves and
a partridge in a pear tree.

...

On the twelfth day of Christmas,
my true love gave to me:
twelve drummers drumming,
eleven pipers piping,
ten lords a-leaping,
nine ladies dancing,
eight maids a-milking,
seven swans a-swimming,
six geese a-laying,
five golden rings;
four calling birds,
three French hens,
two turtle-doves and
a partridge in a pear tree.



```



## Tcl


{{ code(src="content/tasks/the_twelve_days_of_christmas/tcl.tcl", lang="tcl") }}


```txt
On the first day of Christmas,
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas,
My true love gave to me:
Two turtle doves, and
A partridge in a pear tree.

On the third day of Christmas,
My true love gave to me:
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the fourth day of Christmas,
My true love gave to me:
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the fifth day of Christmas,
My true love gave to me:
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the sixth day of Christmas,
My true love gave to me:
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the seventh day of Christmas,
My true love gave to me:
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the eighth day of Christmas,
My true love gave to me:
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the ninth day of Christmas,
My true love gave to me:
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the tenth day of Christmas,
My true love gave to me:
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the eleventh day of Christmas,
My true love gave to me:
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the twelfth day of Christmas,
My true love gave to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

```



## UNIX Shell


{{ code(src="content/tasks/the_twelve_days_of_christmas/unix_shell_1.sh", lang="bash") }}



The above will also work in zsh if the index range is changed from 0..11 to 1..12.

(requires the '''seq''' command)



{{ code(src="content/tasks/the_twelve_days_of_christmas/unix_shell_2.sh", lang="sh") }}



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



## VBA



{{ code(src="content/tasks/the_twelve_days_of_christmas/vba.vba", lang="vb") }}


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



## VBScript



{{ code(src="content/tasks/the_twelve_days_of_christmas/vbscript.vbs", lang="vb") }}




## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic (language version >= 14, e.g. with Visual Studio 2015)



{{ code(src="content/tasks/the_twelve_days_of_christmas/visual_basic_dotnet.vb", lang="vbnet") }}




## zkl


{{ code(src="content/tasks/the_twelve_days_of_christmas/zkl.zkl", lang="zkl") }}



```txt
On the first day of Christmas
My true love gave to me:
A beer, in a tree.

On the second day of Christmas
My true love gave to me:
Two turtlenecks and
A beer, in a tree.

On the third day of Christmas
My true love gave to me:
Three french toast
Two turtlenecks and
A beer, in a tree.

On the fourth day of Christmas
My true love gave to me:
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the fifth day of Christmas
My true love gave to me:
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the sixth day of Christmas
My true love gave to me:
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the seventh day of Christmas
My true love gave to me:
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the eighth day of Christmas
My true love gave to me:
Eight comic books
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the ninth day of Christmas
My true love gave to me:
Nine back up singers
Eight comic books
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the tenth day of Christmas
My true love gave to me:
Ten feet of snow
Nine back up singers
Eight comic books
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the eleventh day of Christmas
My true love gave to me:
Eleven hosers hosing
Ten feet of snow
Nine back up singers
Eight comic books
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the twelfth day of Christmas
My true love gave to me:
Twelve dozen donuts
Eleven hosers hosing
Ten feet of snow
Nine back up singers
Eight comic books
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.


```

