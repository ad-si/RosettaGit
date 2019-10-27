+++
title = "Talk:Sorting algorithms/Shell sort"
description = ""
date = 2010-02-06T14:52:03Z
aliases = []
[extra]
id = 2862
[taxonomies]
categories = []
tags = []
+++

I'm trying to translate the fortran code and I can't figure out what the line:
 DO i = increment+1, SIZE(a)
means? Could that be translated into C-style code? --[[User:Mwn3d|Mwn3d]] 08:34, 20 May 2008 (MDT)

Try
 for (i=increment; i<=(Number of elements in array)-1; i++)
Note by default fortran arrays are 1 based while C arrays are 0 based
:That did it. Thanks. --[[User:Mwn3d|Mwn3d]] 11:32, 20 May 2008 (MDT)
