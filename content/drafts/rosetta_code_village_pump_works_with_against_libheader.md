+++
title = "Rosetta Code:Village Pump/Works with against libheader"
description = ""
date = 2010-11-10T01:53:53Z
aliases = []
[extra]
id = 3369
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Works with against libheader
|summary=Difference between uses of {{tmpl|works with}} vs {{tmpl|libheader}}
}}
Going through some Obj-C codes given by someone else I've added the '''works with''' template, since all the examples always use a OpenStep (likely Cocoa, but works almost always on GNUstep too) framework. When I've found already a "works with Objective version 2.0+" notice, a dilemma came into my mind: which is the exact difference (in this case) between Works With and Libheader?! From a point of view, GNUstep (and Cocoa) is (are) a (set of) library(ies), built "upon" Obj-C OO mechanisms / runtime. So it seems Libheader can apply (in fact we must specify #import <Cocoa/Cocoa.h> or #import <Foundation/Foundation.h> and so on... and when compiling, we need linking properly with that framework. But Works With too... being this maybe more generic? Or should ''Works with'' be kept for language versions, operating systems, or implementations, and Libheader for libraries? (And in this case, I should go back changing WorksWith with Libheader in all GNUstep/Cocoa cases?)...

I am a little bit confused on the exact use of the two templates in object. --[[User:ShinTakezou|ShinTakezou]] 15:24, 12 February 2009 (UTC)

The libheader template actually puts the task in the library's category. The works with template just marks which compiler, interpreter, language version, or implementation the code works under. So the libheader should be used for libraries so that the library categories are updated and the works with template should be used for everything else. --[[User:Mwn3d|Mwn3d]] 15:47, 12 February 2009 (UTC)

:Right. {Works with} is used to describe your test environment, such as which compiler, compiler version, operating system, or language variant is required to run the example. {Libheader} is used to describe optional libraries that are required to run the example.  Objective-C class libraries are kind of on the fuzzy line between the two templates. To some extent, the whole point of Objective-C was to define an object system in which a standard class library could be written, and OpenStep/Cocoa is that class library. I don't think anyone uses Objective-C without the class library and most of its core classes are the same between the different implementations, so it is probably fine to use {works with|OpenStep} and consider it a standard library. Also I don't think there are any other compilers other than [[gcc]]. --[[User:IanOsgood|IanOsgood]] 17:09, 12 February 2009 (UTC)
