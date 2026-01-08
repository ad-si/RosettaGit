+++
title = "zkl"
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
[The Twelve Days of Christmas](../) done in zkl.


## zkl

```zkl
gifts:=
#<<<
"A beer, in a tree.; Two turtlenecks; Three french toast;
Four pounds of backbacon; Five golden touques; Six packs of two-four;
Seven packs of smokes; Eight comic books; Nine back up singers;
Ten feet of snow; Eleven hosers hosing; Twelve dozen donuts"
#<<<
.split(";").apply("strip");

days:=("first second third fourth fifth sixth seventh eighth ninth tenth "
      "eleventh twelfth").split();

foreach n,day in (days.enumerate()){ n+=1;
   g:=gifts[0,n].reverse();
   println("On the %s day of Christmas\nMy true love gave to me:\n".fmt(day),
         g[0,-1].concat("\n"), (n>1) and " and\n" or "", g[-1], "\n");
}
```


<pre style="height:25ex;overflow:scroll">
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
