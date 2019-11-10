+++
title = "Talk:Execute Brainfuck/BASIC/QuickBasic"
description = ""
date = 2010-02-06T14:17:00Z
aliases = []
[extra]
id = 2795
[taxonomies]
categories = []
tags = []
+++

My name's segin <segin2005@gmail.com> and I wrote a Brainfuck interpeter in C, then rewrote it in FreeBASIC (it's almost exactly the same). It includes one extensions, a '0' instruction which zeros out the memory at the current cell. The current implementation is not Turing complete, however, but it's fast.

WARNING!!! It uses a lot of pointer arthimetic and is not secure, i.e. "<+" is all the code needed to overflow since there's no bounds checking.
:If you have another BF interpreter for a BASIC language, I'm not sure how we should do it. This one should maybe move to RCBF/QuickBASIC? We need a little discussion on this. --[[User:Mwn3d|Mwn3d]] 12:13, 4 April 2008 (MDT)
::Works for me.  The BASIC/QBASIC/QuickBASIC/FreeBASIC/AppleSoft BASIC/etc. ambiguity of the basic nature of the BASIC examples on RC give me a headache.  Basically, I'd like to see some basic separation and distinction. --[[User:Short Circuit|Short Circuit]] 20:24, 10 April 2008 (MDT)
