+++
title = "Talk:Delegates"
description = ""
date = 2010-02-06T13:03:56Z
aliases = []
[extra]
id = 3233
[taxonomies]
categories = []
tags = []
+++

==Objective-C code==

The Objective-C code gets no compiled. First, imports lacking. I added:

```txt

#import <Foundation/Foundation.h>

```


I am using GNUstep and gcc 4.2.2. The raised errors are:


```txt

delegator.m:25: error: cannot find interface declaration for ‘NXConstantString’

```


referring to the @"default implementation" part, and


```txt

delegator.m:55: error: ‘isEqualToString’ undeclared (first use in this function)

```


referring to <tt>assert([isEqualToString:@"delegate implementation"]);</tt>. What is lacking in order to be able to compile this code?!
--[[User:ShinTakezou|ShinTakezou]] 14:59, 8 December 2008 (UTC)
