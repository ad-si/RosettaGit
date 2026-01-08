+++
title = "AutoHotkey"
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
[The Twelve Days of Christmas](../) done in AutoHotkey.


## AutoHotkey

```AutoHotkey
nth := ["first","second","third","fourth","fifth","sixth","seventh","eighth","ninth","tenth","eleventh","twelfth"]
lines := ["A partridge in a pear tree."
		,"Two turtle doves and"
		,"Three french hens"
		,"Four calling birds"
		,"Five golden rings"
		,"Six geese a-laying"
		,"Seven swans a-swimming"
		,"Eight maids a-milking"
		,"Nine ladies dancing"
		,"Ten lords a-leaping"
		,"Eleven pipers piping"
		,"Twelve drummers drumming"]

full:="", mid:=""
loop % lines.MaxIndex()
{
	top:="On the " . nth[A_Index] . " day of Christmas,`nMy true love gave to me:"
	mid:= lines[A_Index] . "`n" . mid
	full:= full . top . "`n" . mid . ((A_Index<lines.MaxIndex())?"`n":"")
}
MsgBox % full
```
