+++
title = "Run-length encoding/Objective-C"
description = ""
date = 2014-02-27T07:20:59Z
aliases = []
[extra]
id = 5324
[taxonomies]
categories = []
tags = []
+++

{{collection|Run-length encoding}}

{{works with|GNUstep}}

The class <tt>RCRunLengthEncoder</tt> represents internally data with which it was feeded as pair character - repetition counter: it does not implement a binary representation of itself; it is left to another class, so that different input/output encodings are possible starting from the same class.


```objc
#import <Foundation/Foundation.h


@interface RCRunLengthEncoder : NSObject
{
  NSMutableArray *contents;
  NSMutableArray *counters;
}
+ (instancetype)encoderWithData: (NSData *)data;
- (instancetype)initWithData: (NSData *)data;
- (void)addByte: (char)aByte;
- (void)addByte: (char)aByte repeated: (int)repetitionCount;
- (void)addData: (NSData *)data;
- (NSData *)data;
- (NSArray *)counters;
- (NSArray *)contents;
@end


@implementation RCRunLengthEncoder
+ (instancetype)encoderWithData: (NSData *)data
{
  return [[self alloc] initWithData: data];
}

- (instancetype)initWithData: (NSData *)data
{
  if ((self = [self init]) != nil) {
    [self addData: data];
  }
  return self;
}

- (instancetype)init
{
  if ((self = [super init]) != nil) {
    contents = [[NSMutableArray alloc] init];
    counters = [[NSMutableArray alloc] init];
  }
  return self;
}

- (void)addByte: (char)aByte
{
  [self addByte: aByte repeated: 1];
}

- (void)addByte: (char)aByte repeated: (int)repetitionCount
{
  if ( ([contents count] == 0) || ([[contents lastObject] charValue] != aByte) ) {
    [contents addObject: @(aByte)];
    [counters addObject: @(repetitionCount)];
  } else {
    int a = [[counters lastObject] intValue];
    [counters removeLastObject];
    [counters addObject: @(a + repetitionCount)];
  }
}

- (void)addData: (NSData *)data
{
  const char *d = [data bytes];
  NSUInteger i;
  for(i=0; i < [data length]; i++) [self addByte: d[i]];
}

- (NSArray *)contents
{
  return contents;
}

- (NSArray *)counters
{
  return counters;
}

- (NSData *)data
{
  NSMutableData *d = [[NSMutableData alloc] initWithCapacity: 256];
  char buf[256];
  int i;

  for(i=0; i < [contents count]; i++) {
    char c = [contents[i] charValue];
    int n = [counters[i] intValue];
    memset(buf, c, 256);
    while ( n > 0 ) {
      [d appendBytes: buf length: MIN(256, n)];
      n -= 256;
    }
  }
  return d;
}
@end
```


The class <tt>codecRLE</tt> is derived from the previous, adding the methods that allow to binary encode the data internally held, and to create a internal representation from the encoded data. The specification here used are:

* byte N &gt;= 128, then the next byte must be repeated N-128 times (if N-128 is 0, the the byte must be repeated 128 times)
* byte N &lt; 128, then the next N bytes must be taken as they are (if N is 0, the next 128 bytes are literal)


```objc
@interface codecRLE : RCRunLengthEncoder
- (NSData *)encode;
- (void)decode: (NSData *)enc;
@end

@implementation codecRLE
- (void)decode: (NSData *)enc
{
  const char *buf = [enc bytes];
  int i;

  for(i = 0; i < [enc length]; ) {
    if ( (buf[i] & 0x80) != 0) {
      [self addByte: buf[i+1] repeated: ( ((buf[i]&0x7f) == 0) ? 128 : (buf[i]&0x7f) ) ];
      i += 2;
    } else {
      int rc = (buf[i] == 0) ? 128 : buf[i];
      [self addData: [NSData dataWithBytes: &buf[i+1] length: rc]];
      i += rc + 1;
    }
  }
}

- (NSData *)encode
{
  int literalCount=0;
  int i;
  char buf[129];

  NSMutableData *r = [[NSMutableData alloc] initWithCapacity: 256];

  for(i=0; i < [counters count]; i++) {
    char c = [contents[i] charValue];
    int howMany = [counters[i] intValue];
    if ( literalCount == 128 ) {
      buf[0] = 0;
      [r appendBytes: buf length: 129];
      literalCount = 0;
    }
    if ( howMany == 1 ) {
      buf[literalCount+1] = c;
      literalCount++;
    } else {
      if ( literalCount > 0 ) {
	buf[0] = literalCount & 0x7f;
	[r appendBytes: buf length: (literalCount+1) ];
	literalCount = 0;
      }
      buf[1] = c;
      while( howMany > 127 ) {
	buf[0] = 0x80;
	[r appendBytes: buf length: 2];
	howMany -= 128;
      }
      if (howMany > 0) {
	buf[0] = howMany | 0x80;
	[r appendBytes: buf length: 2];
      }
    }
  }
  return r;
}
@end
```


Usage example:


```objc
const char *s = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";

int main()
{
  @autoreleasepool {

    codecRLE *enc = [[codecRLE alloc]
		      initWithData: [NSData dataWithBytes: s
					    length: strlen(s)] ];

    NSData *repr = [enc encode];
    fwrite([repr bytes], 1, [repr length], stdout);

    enc = [[codecRLE alloc] init];
    [enc decode: repr];
    NSData *d = [enc data];
    fwrite([d bytes], 1, [d length], stdout);

  }
  return EXIT_SUCCESS;
}
```


'''Notes'''

* The code is not deeply tested yet
