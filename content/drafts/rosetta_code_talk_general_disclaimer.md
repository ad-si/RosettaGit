+++
title = "Rosetta Code talk:General disclaimer"
description = ""
date = 2010-02-02T03:47:15Z
aliases = []
[extra]
id = 5401
[taxonomies]
categories = []
tags = []
+++

The "Works with APL2" code looks a wee bit mucked up:

w←1 2 3 1 2 3 4 1
     ((⍳⍨w)=⍳⍴w)/w
1 2 3 4

I think it should be ((w {iota} w) = {iota}{rho}w)/w

where {iota} is the APL iota symbol
and   {rho}  is the APL rho  symbol

— (Unsigned by [[User:69.254.121.12]],  02:41, 2 February 2010)

This belongs on the talk page for [[Create a Sequence of unique elements]]. I have copied it there. —[[User:Kevin Reid|Kevin Reid]] 03:47, 2 February 2010 (UTC)
