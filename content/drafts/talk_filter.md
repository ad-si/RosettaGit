+++
title = "Talk:Filter"
description = ""
date = 2010-02-06T14:48:52Z
aliases = []
[extra]
id = 1875
[taxonomies]
categories = []
tags = []
+++

I fail to see where the "alternate" C# tests for evenness... [[User:Sgeier|Sgeier]] 23:43, 30 January 2007 (EST)
:Looks like the confusion stems from whether or not the "select even numbers" portion of the task description is required.  I'll change the wording and remove the alternate C# example. --[[User:Short Circuit|Short Circuit]] 10:28, 31 January 2007 (EST)

== Objective-C code ==

Does it really works?! It does not on GNUstep, and looking at Apple's doc, I doubt it works for Mac OS X. NSPredicate does not cite the possibility of writing directly ''selectors-like string'' for NSExpression that way ([http://developer.apple.com/DOCUMENTATION/Cocoa/Conceptual/Predicates/Articles/pSyntax.html Cocoa predicates]); NSExpression has the expressionForFunction:arguments: where expressionForFunction can be @"modulus:by:" (and arguments is a NSArray) (see [http://developer.apple.com/DOCUMENTATION/Cocoa/Reference/Foundation/Classes/NSExpression_Class/Reference/NSExpression.html here]), but again, I've found no example about, nor citing, the fact that one can directly write a predicateWithFormat string expressing expressionForFunction ''directly'' with that syntax you've used, i.e. @"modulus:by:(SELF, 2)". Instead, I've tried a simpler @"intValue < 4" and it works like expected (the selector IntValue is for NSNumber object into the array...). GNUstep quirks or Cocoa facts? --[[User:ShinTakezou|ShinTakezou]] 18:16, 22 February 2009 (UTC)

The only way I get it working:


```objc>#import <Foundation/Foundation.h


@interface NSNumber ( ExtFunc )
-(int) modulo2;
@end

@implementation NSNumber ( ExtFunc )
-(int) modulo2
{
  return [self intValue] % 2;
}
@end

int main()
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  NSArray *numbers = [NSArray arrayWithObjects:[NSNumber numberWithInt:1],
			      [NSNumber numberWithInt:2],
			      [NSNumber numberWithInt:3],
			      [NSNumber numberWithInt:4],
			      [NSNumber numberWithInt:5], nil];

  NSPredicate *isEven = [NSPredicate predicateWithFormat:@"modulo2 == 0"];
  NSArray *evens = [numbers filteredArrayUsingPredicate:isEven];
  
  NSLog(@"%@", evens);


  [pool release];
  return 0;
}
```


but maybe there's a way (a syntax) where I can extend NSNumber with a modulo:(int)num, which is more useful... (I've tried; then @"modulo: 2 == 0", @"(modulo: 2) == 0", @"modulo(2) == 0" all rise an error) --[[User:ShinTakezou|ShinTakezou]] 18:31, 22 February 2009 (UTC)

: Is anyone able to test the Objective-C code on a "real" Cocoa Mac OS X framework? I believe it won't work. I have not found any reason why it should work reading into the Apple doc, like [http://developer.apple.com/DOCUMENTATION/Cocoa/Conceptual/Predicates/Articles/pBNF.html here] and related. The modulus:by: selector is understood by NSExpression, but nothing in NSPredicate let me think that selector is "passed" to a NSExpression object... --[[User:ShinTakezou|ShinTakezou]] 23:01, 17 March 2009 (UTC)
