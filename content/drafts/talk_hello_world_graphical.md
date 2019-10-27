+++
title = "Talk:Hello world/Graphical"
description = ""
date = 2010-02-06T15:10:39Z
aliases = []
[extra]
id = 3095
[taxonomies]
categories = []
tags = []
+++

== Graphical console? ==
What is a graphical console? I thought a console is always text mode.

It seems that, again, there are many interpretations of what the task means, because of the vague task specification.

--[[User:PauliKL|PauliKL]] 09:31, 27 October 2008 (UTC)
:I changed the description. Is that more clear? --[[User:Mwn3d|Mwn3d]] 17:00, 27 October 2008 (UTC)
::I guess. But since there are multiple different ways to output the message, I think it should be mentioned in each implementation, where and how the message will appear. --[[User:PauliKL|PauliKL]] 09:19, 28 October 2008 (UTC)

==Objective-C==

This is the second fragment of Obj-C code that I try to run without success!! :) First, it is not a complete example (copy-pasting and simply compiling won't work...). Ok, I could try to complete the code... I obtained


```txt
#import <AppKit/AppKit.h>

int main()
{
NSAlert *alert = [[[NSAlert alloc] init] autorelease];
[alert setMessageText: @"Goodbye, World!"];
[alert runModal];
}
```


After "several" effort about understanding a NXConstantString vs NSConstantString problem, I succeeded compiling it with


```txt

gcc -lobjc -fconstant-string-class=NSConstantString  _box.m -lgnustep-gui -lgnustep-base -o box

```


but running it, I obtain:


```txt
2008-12-09 15:39:11.749 box[9570] autorelease called without pool for object (80cf8c0) of class NSAlert in thread <NSThread: 0x8081690>
Segmentation fault

```


Ok this is not a place for debugging or what, but shouldn't the given codes work properly?! It seems not to work. --[[User:ShinTakezou|ShinTakezou]] 14:42, 9 December 2008 (UTC)

: I'm guessing that most of the Objective-C samples were written by a [[Mac OS X]]/[[Cocoa]] developer, since they are the prime users of Objective-C. Perhaps you could prefix the non-working-with-GNUStep examples with the {works with|Cocoa} template? --[[User:IanOsgood|IanOsgood]] 17:45, 9 December 2008 (UTC)

::Don't know really... I've also a Cocoa/Cocoa.h header, which is just an inclusion of Foundation/Foundation.h and AppKit/AppKit.h, with some more thing... the header stating "Cocoa compatible declarations". So hopely I should be able to do my Cocoa programming under GNU/Linux :D ... The fact is that this example uses just basic language things, and NSAlert, which exist in GNUstep framework, and I believed that such a code should compile and run on my platform. I succeeded compiling, ... if I change the code this way:


```txt
#import <AppKit/AppKit.h>

int main()
{
NSAlert *alert = [[NSAlert alloc] init];
[alert setMessageText: @"Goodbye, World!"];
[alert runModal];
[alert release];
}
```


::the "autorelease called without pool" error is not risen... but a great ''segmentation fault'' is the only thing I obtain anyway :(. I need to take a deeper tour on Obj-C on the web; I want it working :) --[[User:ShinTakezou|ShinTakezou]] 18:59, 9 December 2008 (UTC)

:Continuing learning how to develop GNUstep or Cocoa apps (under GNU/Linux), ... now I am (almost) ''sure'' that ''Objective-C example lacks basic stuffs'' to work! Looking [http://wiki.gnustep.org/index.php/Cocoa here] I know that I can compile and see NSAlert... but the provided code is missing initialization and is usable just by people already knowing how to put altogether full working app with Cocoa/GNUstep in Obj-C. After experimenting, I produced:


```txt
#import <Cocoa/Cocoa.h>

int main( int argc, const char *argv[] )
{
  NSApplication *app;
  NSAutoreleasePool *pool;
  
  pool = [NSAutoreleasePool new];
  app = [NSApplication sharedApplication];
  //NSApplicationMain(argc, argv);
  NSAlert *alert = [[NSAlert new] autorelease];
  [alert setMessageText: @"Goodbye, World!"];
  [alert runModal];
}
```


:that still is not working but resolved the ''autorelease called without pool for object'' problem allocating the lacking pool... At least... a step beyond... I will continue and when I will find it, I will fix the code... (promise I won't write more here, but I would like the person who put the code to complete it...) --[[User:ShinTakezou|ShinTakezou]] 21:26, 9 December 2008 (UTC)

:When you do come up with workable boilerplate required for using OpenStep/Cocoa, please add it to the Objective-C section of [[Empty Program]]. Although one could use Objective-C without the class libraries, in practice nobody does. --[[User:IanOsgood|IanOsgood]] 03:46, 10 December 2008 (UTC)

== Smalltalk code ==

The Smalltalk code needs a '''works with''', but I've failed identifying the implementation that can run it (surely it is not GNU Smalltalk). Anybody having an idea about it? --[[User:ShinTakezou|ShinTakezou]] 22:59, 4 May 2009 (UTC)
