+++
title = "Talk:Pentomino tiling"
description = ""
date = 2017-10-01T17:30:18Z
aliases = []
[extra]
id = 21623
[taxonomies]
categories = []
tags = []
+++

There appears to be a bug in the Java entry for this task - and hence probably in all the other entries which are direct translations of this entry. This bug occasionally manifests itself by throwing an ArrayIndexOutOfBoundsException in the printResult() method and, as far as I can tell, is always caused by some value in the last row of the grid array being equal to -1.

For now I have applied a temporary fix to my Kotlin entry whereby out of range values are replaced by a blank (i.e. a dash). However, the author of the Java entry may like to see if there is a way to prevent out of bounds values from occurring in the first place so we can use that instead.--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 17:50, 29 September 2017 (UTC)
: Sometimes the code places a blank where there already is one, fixed that. Thanks. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 23:36, 29 September 2017 (UTC)
:: Many thanks for fixing that. I've amended the Kotlin entry to match.--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 17:29, 1 October 2017 (UTC)
