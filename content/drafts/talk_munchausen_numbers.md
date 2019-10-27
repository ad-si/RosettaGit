+++
title = "Talk:Munchausen numbers"
description = ""
date = 2016-09-25T09:48:36Z
aliases = []
[extra]
id = 21130
[taxonomies]
categories = []
tags = []
+++

== 0 to the power 0 is considered as 0 for Munchausen numbers ==
...according to the Wikipedia page - might be worth mentioning in the task as it is normally 1 according to mathematicians. --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 18:25, 23 September 2016 (UTC)
: This changes the output only for the number 0 which is not between 1 and 5000 (the task's requirement) --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:16, 23 September 2016 (UTC)
:: True, it would only be an issue if anyone tried to extend this to the next Munchausen number (438 579 088 - also the last known one it seems). --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 21:12, 23 September 2016 (UTC)
:: Also, if you are using an algorithm where the digits are already split up (so not derived by using mod and division), you don't have to worry about leading zeros e.g. 0030 is "Munchausen" if 0^0 = 1. The non-standard 0^0 = 0 simplifies things. --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 21:55, 23 September 2016 (UTC)
::: Where would those leading zeros come from? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:33, 24 September 2016 (UTC)
:::: See the current ALGOL 68 sample which holds the 4 digits as separate numbers and indexes a table of powers - the non-standard 0^0 allows indexing the table of powers without having to worry whether the digit is a leading zero or not. The sample does this to avoids any multiplication, division or modulo operations. --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 19:51, 24 September 2016 (UTC)
::::: Ok, so it's a hack that lets the programmer use a fixed width array of digits without having to bother with the relevant array length. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:22, 24 September 2016 (UTC)
:::::: Or it's a feature of the definition that can be exploited :) --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 09:40, 25 September 2016 (UTC)
::::::: How is that any different? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 09:48, 25 September 2016 (UTC)
