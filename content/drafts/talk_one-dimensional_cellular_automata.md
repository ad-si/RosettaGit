+++
title = "Talk:One-dimensional cellular automata"
description = ""
date = 2016-11-16T14:32:16Z
aliases = []
[extra]
id = 18520
[taxonomies]
categories = []
tags = []
+++

I propose a different script to achieve this goal as :

```txt

#!/usr/local/bin/gawk -f

# User defined functions
function ASCII_to_Binary(str_) {
	gsub("_","0",str_); gsub("@","1",str_)
	return str_
}

function Binary_to_ASCII(bit_) {
	gsub("0","_",bit_); gsub("1","@",bit_)
	return bit_
}

function automate(b1,b2,b3) {
	a = and(b1,b2,b3)
	b = or(b1,b2,b3)
	c = xor(b1,b2,b3)
	d = a + b + c
	return d == 1 ? 1 : 0
}

# For each line in input do
{
str_=$0
gen=0
taille=length(str_)
print "0: " str_
do {
	gen ? str_previous=str_ : str_previous=""
	gen=gen + 1
	str_=ASCII_to_Binary(str_)
	split(str_,tab,"")
	str_=and(tab[1],tab[2])
	for (i=1; i<=taille-2; i++) {
		str_ = str_ automate(tab[i],tab[i+1],tab[i+2])
		}
	str_ = str_ and(tab[taille-1],tab[taille])
	print gen ": " Binary_to_ASCII(str_)
   } while (str_ != str_previous)
}

Then :
$ echo ".@@@.@@.@.@.@.@..@.." | awk -f automata.awk
0: .@@@.@@.@.@.@.@..@..
1: _@_@@@@@_@_@_@______
2: __@@___@@_@_@_______
3: __@@___@@@_@________
4: __@@___@_@@_________
5: __@@____@@@_________
6: __@@____@_@_________
7: __@@_____@__________
8: __@@________________
9: __@@________________

```

--[[User:Poulteki|Poulteki]] ([[User talk:Poulteki|talk]]) 15:31, 15 November 2016 (UTC)

:Please sign your posts and try and fix formatting issues, thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:41, 17 January 2015 (UTC)
