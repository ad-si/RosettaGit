+++
title = "Arithmetic/Rational/Objective-C"
description = ""
date = 2014-02-25T09:13:53Z
aliases = []
[extra]
id = 5289
[taxonomies]
categories = []
tags = []
+++

<noinclude>{{collection|Rational Arithmetic}}</noinclude>
;File <tt>frac.h</tt>

```objc>#import <Foundation/Foundation.h


@interface RCRationalNumber : NSObject
{
 @private
  int numerator;
  int denominator;
  BOOL autoSimplify;
  BOOL withSign;
}
+(instancetype)valueWithNumerator:(int)num andDenominator: (int)den;
+(instancetype)valueWithDouble: (double)fnum;
+(instancetype)valueWithInteger: (int)inum;
+(instancetype)valueWithRational: (RCRationalNumber *)rnum;
-(instancetype)initWithNumerator: (int)num andDenominator: (int)den;
-(instancetype)initWithDouble: (double)fnum precision: (int)prec;
-(instancetype)initWithInteger: (int)inum;
-(instancetype)initWithRational: (RCRationalNumber *)rnum;
-(NSComparisonResult)compare: (RCRationalNumber *)rnum;
-(id)simplify: (BOOL)act;
-(void)setAutoSimplify: (BOOL)v;
-(void)setWithSign: (BOOL)v;
-(BOOL)autoSimplify;
-(BOOL)withSign;
-(NSString *)description;
// ops
-(id)multiply: (RCRationalNumber *)rnum;
-(id)divide: (RCRationalNumber *)rnum;
-(id)add: (RCRationalNumber *)rnum;
-(id)sub: (RCRationalNumber *)rnum;
-(id)abs;
-(id)neg;
-(id)mod: (RCRationalNumber *)rnum;
-(int)sign;
-(BOOL)isNegative;
-(id)reciprocal;
// getter
-(int)numerator;
-(int)denominator;
//setter
-(void)setNumerator: (int)num;
-(void)setDenominator: (int)num;
// defraction
-(double)number;
-(int)integer;
@end
```

;File <tt>frac.m</tt>

```objc>#import <Foundation/Foundation.h

#import <math.h>
#import "frac.h"

// gcd: [[Greatest common divisor#Recursive_Euclid_algorithm]]
// if built in as "private" function, add static.

static int lcm(int a, int b)
{
  return a / gcd(a,b) * b;
}

@implementation RCRationalNumber
// initializers
-(instancetype)init
{
  NSLog(@"initialized to unity");
  return [self initWithInteger: 1];
}

-(instancetype)initWithNumerator: (int)num andDenominator: (int)den
{
  if ((self = [super init]) != nil) {
    if (den == 0) {
      NSLog(@"denominator is zero");
      return nil;
    }
    [self setNumerator: num];
    [self setDenominator: den];
    [self setWithSign: YES];
    [self setAutoSimplify: YES];
    [self simplify: YES];
  }
  return self;
}

-(instancetype)initWithInteger:(int)inum
{
  return [self initWithNumerator: inum andDenominator: 1];
}

-(instancetype)initWithDouble: (double)fnum precision: (int)prec
{
  if ( prec > 9 ) prec = 9;
  double p = pow(10.0, (double)prec);
  int nd = (int)(fnum * p);
  return [self initWithNumerator: nd andDenominator: (int)p ];
}

-(instancetype)initWithRational: (RCRationalNumber *)rnum
{
  return [self initWithNumerator: [rnum numerator] andDenominator: [rnum denominator]];
}

// comparing
-(NSComparisonResult)compare: (RCRationalNumber *)rnum
{
  if ( [self number] > [rnum number] ) return NSOrderedDescending;
  if ( [self number] < [rnum number] ) return NSOrderedAscending;
  return NSOrderedSame;
}

// string rapresentation of the Q
-(NSString *)description
{
  [self simplify: [self autoSimplify]];
  return [NSString stringWithFormat: @"%@%d/%d", [self isNegative] ? @"-" : 
		     ( [self withSign] ? @"+" : @"" ),
		   abs([self numerator]), [self denominator]];
}

// setter options
-(void)setAutoSimplify: (BOOL)v
{
  autoSimplify = v;
  [self simplify: v];
}
-(void)setWithSign: (BOOL)v
{
  withSign = v;
}

// getter for options
-(BOOL)autoSimplify
{
  return autoSimplify;
}

-(BOOL)withSign
{
  return withSign;
}

// "simplify" the fraction ...
-(id)simplify: (BOOL)act
{
  if ( act ) {
    int common = gcd([self numerator], [self denominator]);
    [self setNumerator: [self numerator]/common];
    [self setDenominator: [self denominator]/common];
  }
  return self;
}

// diadic operators
-(id)multiply: (RCRationalNumber *)rnum
{
  int newnum = [self numerator] * [rnum numerator];
  int newden = [self denominator] * [rnum denominator];
  return [RCRationalNumber valueWithNumerator: newnum
			   andDenominator: newden];
}

-(id)divide: (RCRationalNumber *)rnum
{
  return [self multiply: [rnum reciprocal]];
}
 
-(id)add: (RCRationalNumber *)rnum
{
  int common = lcm([self denominator], [rnum denominator]);
  int resnum = common / [self denominator] * [self numerator] +
    common / [rnum denominator] * [rnum numerator];
  return [RCRationalNumber valueWithNumerator: resnum andDenominator: common];
}

-(id)sub: (RCRationalNumber *)rnum
{
  return [self add: [rnum neg]];
}

-(id)mod: (RCRationalNumber *)rnum
{
  return [[self divide: rnum] 
	   sub: [RCRationalNumber valueWithInteger: [[self divide: rnum] integer]]];
}

// unary operators
-(id)neg
{
  return [RCRationalNumber valueWithNumerator: -1*[self numerator]
			   andDenominator: [self denominator]];
}

-(id)abs
{
  return [RCRationalNumber valueWithNumerator: abs([self numerator])
			   andDenominator: [self denominator]];
}

-(id)reciprocal
{
  return [RCRationalNumber valueWithNumerator: [self denominator]
			   andDenominator: [self numerator]];
}

// get the sign
-(int)sign
{
  return ([self numerator] < 0) ? -1 : 1;
}

// or just test if negative
-(BOOL)isNegative
{
  return [self numerator] < 0;
}

// Q as real floating point
-(double)number
{
  return (double)[self numerator] / (double)[self denominator];
}

// Q as (truncated) integer
-(int)integer
{
  return [self numerator] / [self denominator];
}

// set num and den indipendently, fixing sign accordingly
-(void)setNumerator: (int)num
{
  numerator = num;
}

-(void)setDenominator: (int)num
{
  if ( num < 0 ) numerator = -numerator;
  denominator = abs(num);
}

// getter
-(int)numerator
{
  return numerator;
}

-(int)denominator
{
  return denominator;
}

// class method
+(instancetype)valueWithNumerator:(int)num andDenominator: (int)den
{
  return [[self alloc] initWithNumerator: num andDenominator: den];
}

+(instancetype)valueWithDouble: (double)fnum
{
  return [[self alloc] initWithDouble: fnum];
}

+(instancetype)valueWithInteger: (int)inum
{
  return [[self alloc] initWithInteger: inum];
}

+(instancetype)valueWithRational: (RCRationalNumber *)rnum
{
  return [[self alloc] initWithRational: rnum];
}
@end
```

;Testing

```objc>#import <Foundation/Foundation.h

#import "frac.h"
#import <math.h>

int main()
{
  @autoreleasepool {

    int i;
    for(i=2; i < 0x80000; i++) {
      int candidate = i;
      RCRationalNumber *sum = [RCRationalNumber valueWithNumerator: 1
 			                            andDenominator: candidate];
      int factor;
      for(factor=2; factor < sqrt((double)candidate); factor++) {
        if ( (candidate % factor) == 0 ) {
 	  sum = [[sum add: [RCRationalNumber valueWithNumerator: 1
					         andDenominator: factor]]
		  add: [RCRationalNumber valueWithNumerator: 1
					     andDenominator: (candidate/factor)]];
        }
      }
      if ( [sum denominator] == 1 ) {
        printf("Sum of recipr. factors of %d = %d exactly %s\n",
	       candidate, [sum integer], ([sum integer]==1) ? "perfect!" : "");
      }
    }

  }
  return 0;
}
```

