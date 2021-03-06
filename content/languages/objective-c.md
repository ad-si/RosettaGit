+++
title = "Objective-C"
description = ""
date = 2014-10-16T14:30:30Z
aliases = []
[extra]
id = 1856
[taxonomies]
categories = []
tags = []
+++

{{language|Objective-C
|exec=machine
|strength=weak
|express=explicit
|checking=static
|parampass=value
|gc=allowed
|site=http://developer.apple.com/documentation/Cocoa/Conceptual/ObjectiveC/
|LCT=yes}}{{language programming paradigm|Object-oriented}}
[[wp:Objective-C|Objective-C]] is an [[object-oriented]] superset of the [[derived from::compatible with::C]] language. It mostly copies the message passing system from [[derived from::Smalltalk]]. It was popularized by NeXT, and then again by [[Apple Inc]] with [[Mac OS X]] and [[iOS]] to implement the [[Cocoa]] frameworks. Its main reference implementation is within the [[gcc]] compiler, maintained mostly by Apple.

As of October 2011, with the release of [http://developer.apple.com/library/ios/#documentation/DeveloperTools/Conceptual/WhatsNewXcode/Articles/xcode_4_2.html#//apple_ref/doc/uid/00200-SW1 XCode 4.2], Apple switched from [[gcc]] to [[wp:Clang|Clang]] as its default compiler. [[wp:Clang|Clang]]/[[wp:LLVM|LLVM]] offers competitive execution times, better compile times, improved error messages, and supports a simpler alternative syntax for expressing NSArray & NSDictionary literals and indexing. In the same release Apple also introduced automatic reference counting (ARC) which eliminates the need to manually release/retain memory. With ARC the compiler reports an error any time it encounters a call to release, autorelease, retain, or dealloc.

The release of Xcode 4.4 (4.5 for iOS) added [http://clang.llvm.org/docs/ObjectiveCLiterals.html syntax to specify literals] for <code>NSArray</code>, <code>NSDictionary</code>, <code>NSNumber</code>, and <code>NSString</code>, as well as subscript syntax to access elements of <code>NSArray</code> and <code>NSDictionary</code>.

Unless otherwise stated, Objective-C code samples will assume that they are compiled with ARC enabled, and that the compiler supports Objective-C literals and Blocks.

For details of how to compile and run examples of Rosetta Code tasks written in Objective C under Linux or Windows see [http://www.gnu.org/software/gnustep/ GNUstep]
<br clear=right>
