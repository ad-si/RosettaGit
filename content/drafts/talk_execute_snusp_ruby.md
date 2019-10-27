+++
title = "Talk:Execute SNUSP/Ruby"
description = ""
date = 2010-02-06T14:36:41Z
aliases = []
[extra]
id = 4621
[taxonomies]
categories = []
tags = []
+++

The problem with 'BangBang' is almost certainly due to output buffering and/or input blocking. I had to tinker with those to make it work for [[RCSNUSP/Tcl]], guessing what assumptions the original describer of the language had for the I/O. —[[User:Dkf|Donal Fellows]] 08:58, 31 July 2009 (UTC)
: True.  However, I just found the real problem:  my threads are sharing ''references'' to the data pointer, not separate copies.  D'oh! --[[User:Glennj|glennj]] 15:40, 31 July 2009 (UTC)
