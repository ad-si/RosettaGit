+++
title = "Closest-pair problem/Objective-C"
description = ""
date = 2014-02-25T09:45:39Z
aliases = []
[extra]
id = 5322
[taxonomies]
categories = []
tags = []
+++

{{collection|Closest pair problem}}

{{works with|GNUstep}}

{{works with|Cocoa}}

```objc>#import <Foundation/Foundation.h

#import <math.h>

@interface Point : NSObject {
  double xCoord, yCoord;
}
+ (instancetype)x: (double)x y: (double)y;
- (instancetype)initWithX: (double)x andY: (double)y;
- (double)x;
- (double)y;
- (double)dist: (Point *)pt;
- (NSComparisonResult)compareX: (Point *)pt;
- (NSComparisonResult)compareY: (Point *)pt;
@end

@implementation Point

+ (instancetype)x: (double)x y: (double)y {
  return [[self alloc] initWithX: x andY: y];
}

- (instancetype)initWithX: (double)x andY: (double)y {
  if ((self = [super init])) {
    xCoord = x;
    yCoord = y;
  }
  return self;
}

- (double)x { return xCoord; }
- (double)y { return yCoord; }

- (double)dist: (Point *)pt {
  return hypot([self x] - [pt x], [self y] - [pt y]);
}

- (NSComparisonResult)compareX: (Point *)pt {
  if      ( [self x] < [pt x] ) return NSOrderedAscending;
  else if ( [self x] > [pt x] ) return NSOrderedDescending;
  else                          return NSOrderedSame;
}

- (NSComparisonResult)compareY: (Point *)pt {
  if      ( [self y] < [pt y] ) return NSOrderedAscending;
  else if ( [self y] > [pt y] ) return NSOrderedDescending;
  else                          return NSOrderedSame;
}
@end
```



```objc
@interface ClosestPair : NSObject
+ (NSArray *)closestPairSimple: (NSArray *)pts;
+ (NSArray *)closestPair: (NSArray *)pts;
+ (NSArray *)closestPairPriv: (NSArray *)xP and: (NSArray *)yP;
+ (id)minBetween: (id)minA and: (id)minB;
@end

@implementation ClosestPair

+ (NSArray *)closestPairSimple: (NSArray *)pts {
  if ( [pts count] < 2 ) return @[ @HUGE_VAL ];
  double c = [ pts[0] dist: pts[1] ];
  NSArray *r = @[ @(c), pts[0], pts[1] ];
  for(int i=0; i < ([pts count] - 1); i++) {
    for(int j=i+1; j < [pts count]; j++) {
      double t = [ pts[i] dist: pts[j] ];
      if ( t < c ) {
        c = t;
        r = @[ @(t), pts[i], pts[j] ];
      }
    }
  }
  return r;
}

+ (NSArray *)closestPair: (NSArray *)pts {
  return [self closestPairPriv: [pts sortedArrayUsingSelector: @selector(compareX:)]
                           and: [pts sortedArrayUsingSelector: @selector(compareY:)]
    ];
}

+ (NSArray *)closestPairPriv: (NSArray *)xP and: (NSArray *)yP {
  int nP, k;

  if ( [xP count] <= 3 ) {
    return [self closestPairSimple: xP];
  } else {
    int midx = ceil([xP count]/2.0);
    NSArray *pL = [xP subarrayWithRange: NSMakeRange(0, midx)];
    NSArray *pR = [xP subarrayWithRange: NSMakeRange(midx, [xP count] - midx)];
    NSMutableArray *yL = [[NSMutableArray alloc] init];
    NSMutableArray *yR = [[NSMutableArray alloc] init];
    double middleVLine = [pL[midx-1] x];
    for(int i=0; i < [yP count]; i++) {
      if ( [yP[i] x] <= middleVLine ) {
        [yL addObject: yP[i]];
      } else {
        [yR addObject: yP[i]];
      }
    }
    NSArray *minR = [ClosestPair closestPairPriv: pR and: yR];
    NSArray *minL = [ClosestPair closestPairPriv: pL and: yL];
    NSMutableArray *minDist = [ClosestPair minBetween: minR and: minL];
    NSMutableArray *joiningStrip = [NSMutableArray arrayWithCapacity: [xP count]];
    for(int i=0; i < [yP count]; i++) {
      if ( fabs([yP[i] x] - middleVLine) <
           [minDist[0] doubleValue] ) {
        [joiningStrip addObject: yP[i]];
      }
    }
    NSMutableArray *tDist = minDist;
    int nP = [joiningStrip count];
    for(int i=0; i < (nP - 1); i++) {
      int k = i + 1;
      while( (k < nP) &&
             ( ([joiningStrip[k] y] - [joiningStrip[i] y]) < [minDist[0] doubleValue] ) ) {
        double d = [joiningStrip[i] dist: joiningStrip[k]];
        if ( d < [tDist[0] doubleValue] ) {
          tDist = @[ @(d), joiningStrip[i], joiningStrip[k] ];
        }
        k++;
      }
    }
    return tDist;
  }
}

+ (id)minBetween: (id)minA and: (id)minB {
  if ( [minA[0] doubleValue] < [minB[0] doubleValue] ) {
    return minA;
  } else {
    return minB;
  }
}

@end
```


'''Testing'''


```objc
#define NP 10000

int main()
{
  @autoreleasepool {

    NSMutableArray *p = [[NSMutableArray alloc] init];
    srand(0);
    for(int i = 0; i < NP; i++) {
      [p addObject:
           [Point x: 20.0*((double)rand()/(RAND_MAX+1.0)) - 10.0
                  y: 20.0*((double)rand()/(RAND_MAX+1.0)) - 10.0]
        ];
    }

    //NSArray *r1 = [ClosestPair closestPairSimple: p];
    NSArray *r2 = [ClosestPair closestPair: p];

    //NSLog(@"%lf", [r1[0] doubleValue]);
    NSLog(@"%lf", [r2[0] doubleValue]);

  }
  return EXIT_SUCCESS;
}
```


Timing (with the <tt>time</tt> command):


```txt
d&amp;c:         0.22user 0.00system 0:00.41elapsed
brute force: 13.53user 0.06system 0:13.87elapsed
```

