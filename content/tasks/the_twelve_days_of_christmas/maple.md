+++
title = "Maple"
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
[The Twelve Days of Christmas](../) done in Maple.


## Maple

```maple
gifts := ["Twelve drummers drumming",
		"Eleven pipers piping", "Ten lords a-leaping",
		"Nine ladies dancing", "Eight maids a-milking",
		"Seven swans a-swimming", "Six geese a-laying",
		"Five golden rings", "Four calling birds",
		"Three french hens", "Two turtle doves and", "A partridge in a pear tree"]:
days := ["first", "second", "third", "fourth", "fifth", "sixth",
		"seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]:
for i to 12 do
	printf("On the %s day of Christmas\nMy true love gave to me:\n", days[i]);
	for j from 13-i to 12 do
		printf("%s\n", gifts[j]);
	end do;
	printf("\n");
end do;
```

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
