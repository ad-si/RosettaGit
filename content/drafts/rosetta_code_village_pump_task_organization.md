+++
title = "Rosetta Code:Village Pump/Task organization"
description = ""
date = 2010-11-10T02:11:14Z
aliases = []
[extra]
id = 3848
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Task organization
|summary=Restructuring tasks.
}}

==Type algebra==
'''Further restructuring'''. I propose to group the tasks related to the language ''types algebra'' under this category, like it was done with control flow statements. A types algebra is a set of predefined types and the operations constructing new types out of given ones. For example, the array type is produced by such an operation. It takes the type of the index, the type of the element and produces a new container type called array. Here is an incomplete list of algebraic operations most common to higher-level typed languages:
* Array type construction;
* Record type construction;
* Enumeration type construction;
* Reference/pointer type construction;
* Parametrization, aka parametric polymorphism of types, aka class template, aka generic type;
* Inheritance, aka dynamic polymorphism, aka type extension;
* Type constraining, aka subtyping (non-Liskov subtyping), aka specialization;
* Type cloning (produces a type which is a copy of the argument type);
* Class rooted in the type (polymorphism).
The predefined types are well known:
* Boolean;
* Integer;
* Modular integer;
* Unconstrained integer;
* String;
* Floating-point number;
* Fixed-point number;
* Complex number;
* Address.
Partially some of the predefined types are rather constructed by algebraic operations. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 20:03, 28 October 2008 (UTC)

I worry that this organization will create unnatural distinctions and clutter for languages which do not have or do not emphasize static type systems. --[[User:Kevin Reid|Kevin Reid]] 00:47, 24 February 2009 (UTC)
: Fortunately, MediaWiki categories are not exclusive.  This means we can have multiple parallel sorting trees. (As I started to work through last week with the language table, pages and language property templates.)  So as long as tasks aren't excluded from creation or other organization by not fitting into this categorization tree, I don't think it'll be a problem. --[[User:Short Circuit|Short Circuit]] 06:36, 24 February 2009 (UTC)

:Dynamically typed languages have a types algebra as well, obviously. The difference if any, is that some of the algebraic operations become proper functions as well as the types. I.e. you can construct an array type at run-time, type is a first class object, etc. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:58, 24 February 2009 (UTC)
==Stirring up old ideas==
So we had a sort of organized task category tree before at [[:Category|Solutions by Programming Task]]. Right now people are directed to the flat list in the sidebar. Do we want to organize the tasks with categories anymore? I think new users might have a hard time dealing with the flat list, but maybe they don't go there first. Is there a SMW way to organize the tasks? Ideas? --[[User:Mwn3d|Mwn3d]] 19:06, 16 October 2010 (UTC)
