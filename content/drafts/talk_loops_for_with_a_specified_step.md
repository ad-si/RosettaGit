+++
title = "Talk:Loops/For with a specified step"
description = ""
date = 2010-02-06T13:41:40Z
aliases = []
[extra]
id = 4494
[taxonomies]
categories = []
tags = []
+++

The [[Ada]] solution might also consider using simple loop and then applying a (simple) function to generate the “loop variable”. For example, to loop from 0 to 10 by 2, you might loop from 0 to 5 by 1 and then set another var to the loop var times 2 at each step. (This technique is much more common when trying to loop by floating-point quantities, where feeding them directly into a [[C]]-like loop is inadvisable in many languages due to the need to manage imprecision in a way that doesn't blow up in your face...) —[[User:Dkf|Donal Fellows]] 23:38, 12 July 2009 (UTC)
