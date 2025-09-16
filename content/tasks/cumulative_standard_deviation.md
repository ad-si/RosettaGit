+++
title = "Cumulative standard deviation"
description = ""
date = 2019-07-06T21:29:21Z
aliases = []
[extra]
id = 4395
[taxonomies]
categories = ["task", "Probability and statistics"]
tags = []
languages = [
  "11l",
  "360_assembly",
  "ada",
  "algol_68",
  "algol_w",
  "applescript",
  "autohotkey",
  "awk",
  "axiom",
  "bbc_basic",
  "c",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "component_pascal",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "e",
  "elixir",
  "emacs_lisp",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "haxe",
  "hicest",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "lua",
  "mathematica",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sas",
  "scala",
  "scheme",
  "scilab",
  "sidef",
  "smalltalk",
  "sql",
  "swift",
  "tcl",
  "vbscript",
  "visual_basic",
  "xpl0",
  "zkl",
]
+++

## Task

Write a stateful function, class, generator or co-routine that takes a series of floating point numbers, ''one at a time'', and returns the running [[wp:Standard Deviation|standard deviation]] of the series.

The task implementation should use the most natural programming style of those listed for the function in the implementation language; the task ''must'' state which is being used.

Do not apply [[wp:Bessel's correction|Bessel's correction]]; the returned standard deviation should always be computed as if the sample seen so far is the entire population.


;Test case:
Use this to compute the standard deviation of this demonstration set, <math>\{2, 4, 4, 4, 5, 5, 7, 9\}</math>, which is <math>2</math>.


## Related tasks

* [[Random numbers]]


<hr>


## 11l

```11l
T SD
   sum = 0.0
   sum2 = 0.0
   n = 0.0

   F ()(x)
      .sum += x
      .sum2 += x ^ 2
      .n += 1.0
      R sqrt(.sum2 / .n - (.sum / .n) ^ 2)

V sd_inst = SD()
L(value) [2, 4, 4, 4, 5, 5, 7, 9]
   print(value‘ ’sd_inst(value))
```


```txt

2 0
4 1
4 0.942809042
4 0.866025404
5 0.979795897
5 1
7 1.399708424
9 2

```



## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set.
Part of the code length is due to the square root algorithm and to the nice output.

```360asm
******** Standard deviation of a population
STDDEV   CSECT
         USING  STDDEV,R13
SAVEAREA B      STM-SAVEAREA(R15)
         DC     17F'0'
         DC     CL8'STDDEV'
STM      STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15
         SR     R8,R8           s=0
         SR     R9,R9           ss=0
         SR     R4,R4           i=0
         LA     R6,1
         LH     R7,N
LOOPI    BXH    R4,R6,ENDLOOPI
         LR     R1,R4           i
         BCTR   R1,0
         SLA    R1,1
         LH     R5,T(R1)
         ST     R5,WW           ww=t(i)
         MH     R5,=H'1000'     w=ww*1000
         AR     R8,R5           s=s+w
         LR     R15,R5
         MR     R14,R5          w*w
         AR     R9,R15          ss=ss+w*w
         LR     R14,R8          s
         SRDA   R14,32
         DR     R14,R4          /i
         ST     R15,AVG         avg=s/i
         LR     R14,R9          ss
         SRDA   R14,32
         DR     R14,R4          ss/i
         LR     R2,R15          ss/i
         LR     R15,R8          s
         MR     R14,R8          s*s
         LR     R3,R15
         LR     R15,R4          i
         MR     R14,R4          i*i
         LR     R1,R15
         LA     R14,0
         LR     R15,R3
         DR     R14,R1          (s*s)/(i*i)
         SR     R2,R15
         LR     R10,R2          std=ss/i-(s*s)/(i*i)
         LR     R11,R10         std
         SRA    R11,1           x=std/2
         LR     R12,R10         px=std
LOOPWHIL EQU    *
         CR     R12,R11         while px<>=x
         BE     ENDWHILE
         LR     R12,R11         px=x
         LR     R15,R10         std
         LA     R14,0
         DR     R14,R12         /px
         LR     R1,R12          px
         AR     R1,R15          px+std/px
         SRA    R1,1            /2
         LR     R11,R1          x=(px+std/px)/2
         B      LOOPWHIL
ENDWHILE EQU    *
         LR     R10,R11
         CVD    R4,P8           i
         MVC    C17,MASK17
         ED     C17,P8
         MVC    BUF+2(1),C17+15
         L      R1,WW
         CVD    R1,P8
         MVC    C17,MASK17
         ED     C17,P8
         MVC    BUF+10(1),C17+15
         L      R1,AVG
         CVD    R1,P8
         MVC    C18,MASK18
         ED     C18,P8
         MVC    BUF+17(5),C18+12
         CVD    R10,P8          std
         MVC    C18,MASK18
         ED     C18,P8
         MVC    BUF+31(5),C18+12
         WTO    MF=(E,WTOMSG)
         B      LOOPI
ENDLOOPI EQU    *
         L      R13,4(0,R13)
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14
         DS     0D
N        DC     H'8'
T        DC     H'2',H'4',H'4',H'4',H'5',H'5',H'7',H'9'
WW       DS     F
AVG      DS     F
P8       DS     PL8
MASK17   DC     C' ',13X'20',X'2120',C'-'
MASK18   DC     C' ',10X'20',X'2120',C'.',3X'20',C'-'
C17      DS     CL17
C18      DS     CL18
WTOMSG   DS     0F
         DC     H'80',XL2'0000'
BUF      DC     CL80'N=1  ITEM=1  AVG=1.234  STDDEV=1.234 '
         YREGS
         END    STDDEV
```

```txt
N=1  ITEM=2  AVG=2.000  STDDEV=0.000
N=2  ITEM=4  AVG=3.000  STDDEV=1.000
N=3  ITEM=4  AVG=3.333  STDDEV=0.942
N=4  ITEM=4  AVG=3.500  STDDEV=0.866
N=5  ITEM=5  AVG=3.800  STDDEV=0.979
N=6  ITEM=5  AVG=4.000  STDDEV=1.000
N=7  ITEM=7  AVG=4.428  STDDEV=1.399
N=8  ITEM=9  AVG=5.000  STDDEV=2.000
```



## Ada


```ada

with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Float_Text_IO;                  use Ada.Float_Text_IO;
with Ada.Integer_Text_IO;                use Ada.Integer_Text_IO;

procedure Test_Deviation is
   type Sample is record
      N            : Natural := 0;
      Sum          : Float := 0.0;
      SumOfSquares : Float := 0.0;
   end record;
   procedure Add (Data : in out Sample; Point : Float) is
   begin
      Data.N       := Data.N + 1;
      Data.Sum    := Data.Sum    + Point;
      Data.SumOfSquares := Data.SumOfSquares + Point ** 2;
   end Add;
   function Deviation (Data : Sample) return Float is
   begin
      return Sqrt (Data.SumOfSquares / Float (Data.N) - (Data.Sum / Float (Data.N)) ** 2);
   end Deviation;

   Data : Sample;
   Test : array (1..8) of Integer := (2, 4, 4, 4, 5, 5, 7, 9);
begin
   for Index in Test'Range loop
      Add (Data, Float(Test(Index)));
      Put("N="); Put(Item => Index, Width => 1);
      Put(" ITEM="); Put(Item => Test(Index), Width => 1);
      Put(" AVG="); Put(Item => Float(Data.Sum)/Float(Index), Fore => 1, Aft => 3, Exp => 0);
      Put("  STDDEV="); Put(Item => Deviation (Data), Fore => 1, Aft => 3, Exp => 0);
      New_line;
   end loop;
end Test_Deviation;

```

```txt

N=1 ITEM=2 AVG=2.000  STDDEV=0.000
N=2 ITEM=4 AVG=3.000  STDDEV=1.000
N=3 ITEM=4 AVG=3.333  STDDEV=0.943
N=4 ITEM=4 AVG=3.500  STDDEV=0.866
N=5 ITEM=5 AVG=3.800  STDDEV=0.980
N=6 ITEM=5 AVG=4.000  STDDEV=1.000
N=7 ITEM=7 AVG=4.429  STDDEV=1.400
N=8 ITEM=9 AVG=5.000  STDDEV=2.000

```



## ALGOL 68

<!-- {{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8.8d.fc9.i386]}} -->
Note: the use of a UNION to mimic C's enumerated types is "experimental" and probably not typical of "production code".  However it is a example of '''ALGOL 68'''s ''conformity CASE clause'' useful for classroom dissection.

```Algol68
MODE VALUE = STRUCT(CHAR value),
     STDDEV = STRUCT(CHAR stddev),
     MEAN = STRUCT(CHAR mean),
     VAR = STRUCT(CHAR var),
     COUNT = STRUCT(CHAR count),
     RESET = STRUCT(CHAR reset);

MODE ACTION = UNION ( VALUE, STDDEV, MEAN, VAR, COUNT, RESET );

LONG REAL sum := 0;
LONG REAL sum2 := 0;
INT num := 0;

PROC stat object = (LONG REAL v, ACTION action)LONG REAL:
(

  LONG REAL m;

  CASE action IN
  (VALUE):(
    num +:= 1;
    sum +:= v;
    sum2 +:= v*v;
    stat object(0, LOC STDDEV)
  ),
  (STDDEV):
    long sqrt(stat object(0, LOC VAR)),
  (MEAN):
    IF num>0 THEN sum/LONG REAL(num) ELSE 0 FI,
  (VAR):(
    m := stat object(0, LOC MEAN);
    IF num>0 THEN sum2/LONG REAL(num)-m*m ELSE 0 FI
  ),
  (COUNT):
    num,
  (RESET):
    sum := sum2 := num := 0
  ESAC
);

[]LONG REAL v = ( 2,4,4,4,5,5,7,9 );

main:
(
  LONG REAL sd;

  FOR i FROM LWB v TO UPB v DO
    sd := stat object(v[i], LOC VALUE);
    printf(($"value: "g(0,6)," standard dev := "g(0,6)l$, v[i], sd))
  OD

)
```

```txt

value: 2.000000 standard dev := .000000
value: 4.000000 standard dev := 1.000000
value: 4.000000 standard dev := .942809
value: 4.000000 standard dev := .866025
value: 5.000000 standard dev := .979796
value: 5.000000 standard dev := 1.000000
value: 7.000000 standard dev := 1.399708
value: 9.000000 standard dev := 2.000000

```


<!-- {{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8.8d.fc9.i386]}} -->

A code sample in an object oriented style:

```Algol68
MODE STAT = STRUCT(
  LONG REAL sum,
  LONG REAL sum2,
  INT num
);

OP INIT = (REF STAT new)REF STAT:
  (init OF class stat)(new);

MODE CLASSSTAT = STRUCT(
  PROC (REF STAT, LONG REAL #value#)VOID plusab,
  PROC (REF STAT)LONG REAL stddev, mean, variance, count,
  PROC (REF STAT)REF STAT init
);

CLASSSTAT class stat;

plusab OF class stat := (REF STAT self, LONG REAL value)VOID:(
    num OF self +:= 1;
    sum OF self +:= value;
    sum2 OF self +:= value*value
  );

OP +:= = (REF STAT lhs, LONG REAL rhs)VOID: # some syntatic sugar #
  (plusab OF class stat)(lhs, rhs);

stddev OF class stat := (REF STAT self)LONG REAL:
    long sqrt((variance OF class stat)(self));

OP STDDEV = ([]LONG REAL value)LONG REAL: ( # more syntatic sugar #
  REF STAT stat = INIT LOC STAT;
  FOR i FROM LWB value TO UPB value DO
    stat +:= value[i]
  OD;
  (stddev OF class stat)(stat)
);

mean OF class stat := (REF STAT self)LONG REAL:
    sum OF self/LONG REAL(num OF self);

variance OF class stat := (REF STAT self)LONG REAL:(
    LONG REAL m = (mean OF class stat)(self);
    sum2 OF self/LONG REAL(num OF self)-m*m
  );

count OF class stat := (REF STAT self)LONG REAL:
    num OF self;

init OF class stat := (REF STAT self)REF STAT:(
    sum OF self := sum2 OF self := num OF self := 0;
    self
  );

[]LONG REAL value = ( 2,4,4,4,5,5,7,9 );

main:
(
#  printf(($"standard deviation operator = "g(0,6)l$, STDDEV value));
#

  REF STAT stat = INIT LOC STAT;

  FOR i FROM LWB value TO UPB value DO
    stat +:= value[i];
    printf(($"value: "g(0,6)," standard dev := "g(0,6)l$, value[i], (stddev OF class stat)(stat)))
  OD
#
;
  printf(($"standard deviation = "g(0,6)l$, (stddev OF class stat)(stat)));
  printf(($"mean = "g(0,6)l$, (mean OF class stat)(stat)));
  printf(($"variance = "g(0,6)l$, (variance OF class stat)(stat)));
  printf(($"count = "g(0,6)l$, (count OF class stat)(stat)))
#

)

```

```txt

value: 2.000000 standard dev := .000000
value: 4.000000 standard dev := 1.000000
value: 4.000000 standard dev := .942809
value: 4.000000 standard dev := .866025
value: 5.000000 standard dev := .979796
value: 5.000000 standard dev := 1.000000
value: 7.000000 standard dev := 1.399708
value: 9.000000 standard dev := 2.000000

```


<!-- {{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8.8d.fc9.i386]}} -->

A simple - but "unpackaged" - code example, useful if the standard deviation is required on only one set of concurrent data:

```Algol68
LONG REAL sum, sum2;
INT n;

PROC sd = (LONG REAL x)LONG REAL:(
    sum  +:= x;
    sum2 +:= x*x;
    n    +:= 1;
    IF n = 0 THEN 0 ELSE long sqrt(sum2/n - sum*sum/n/n) FI
);

sum := sum2 := n := 0;
[]LONG REAL values = (2,4,4,4,5,5,7,9);
FOR i TO UPB values DO
    LONG REAL value = values[i];
    printf(($2(xg(0,6))l$, value, sd(value)))
OD
```

```txt

 2.000000 .000000
 4.000000 1.000000
 4.000000 .942809
 4.000000 .866025
 5.000000 .979796
 5.000000 1.000000
 7.000000 1.399708
 9.000000 2.000000

```



## ALGOL W

This is an Algol W version of the third, "unpackaged" Algol 68 sample, which was itself translated from Python.

```algolw
begin

    long real sum, sum2;
    integer   n;

    long real procedure sd (long real value x) ;
    begin
        sum  := sum  + x;
        sum2 := sum2 + (x*x);
        n    := n    + 1;
        if n = 0 then 0 else longsqrt(sum2/n - sum*sum/n/n)
    end sd;

    sum := sum2 := n := 0;

    r_format := "A"; r_w := 14; r_d := 6; % set output to fixed point format %

    for i := 2,4,4,4,5,5,7,9
    do begin
        long real val;
        val := i;
        write(val, sd(val))
    end for_i

end.
```

```txt

      2.000000        0.000000
      4.000000        1.000000
      4.000000        0.942809
      4.000000        0.866025
      5.000000        0.979795
      5.000000        1.000000
      7.000000        1.399708
      9.000000        2.000000

```



## AppleScript


Accumulation across a fold


```AppleScript
-- stdDevInc :: Accumulator -> Num -> Index -> Accumulator
-- stdDevInc :: {sum:, squaresSum:, stages:} -> Real -> Integer
--                -> {sum:, squaresSum:, stages:}
on stdDevInc(a, n, i)
    set sum to (sum of a) + n
    set squaresSum to (squaresSum of a) + (n ^ 2)
    set stages to (stages of a) & ¬
        ((squaresSum / i) - ((sum / i) ^ 2)) ^ 0.5

    {sum:sum, squaresSum:squaresSum, stages:stages}
end stdDevInc


-- TEST
on run
    set lstSample to [2, 4, 4, 4, 5, 5, 7, 9]

    stages of foldl(stdDevInc, ¬
        {sum:0, squaresSum:0, stages:[]}, lstSample)

    --> {0.0, 1.0, 0.942809041582, 0.866025403784, 0.979795897113, 1.0, 1.399708424448, 2.0}
end run



-- GENERIC FUNCTIONS  ----------------------------------------------------------------------

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to lambda(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property lambda : f
        end script
    end if
end mReturn
```


```AppleScrip
{0.0, 1.0, 0.942809041582, 0.866025403784,
0.979795897113, 1.0, 1.399708424448, 2.0}
```



## AutoHotkey


```AutoHotkey
Data := [2,4,4,4,5,5,7,9]

for k, v in Data {
    FileAppend, % "#" a_index " value = " v " stddev = " stddev(v) "`n", * ; send to stdout
}
return

stddev(x) {
	static n, sum, sum2
	n++
	sum += x
	sum2 += x*x

	return sqrt((sum2/n) - (((sum*sum)/n)/n))
}
```

```txt

#1 value = 2 stddev 0 0.000000
#2 value = 4 stddev 0 1.000000
#3 value = 4 stddev 0 0.942809
#4 value = 4 stddev 0 0.866025
#5 value = 5 stddev 0 0.979796
#6 value = 5 stddev 0 1.000000
#7 value = 7 stddev 0 1.399708
#8 value = 9 stddev 0 2.000000

```



## AWK


```AWK

# syntax: GAWK -f STANDARD_DEVIATION.AWK
BEGIN {
    n = split("2,4,4,4,5,5,7,9",arr,",")
    for (i=1; i<=n; i++) {
      temp[i] = arr[i]
      printf("%g %g\n",arr[i],stdev(temp))
    }
    exit(0)
}
function stdev(arr,  i,n,s1,s2,variance,x) {
    for (i in arr) {
      n++
      x = arr[i]
      s1 += x ^ 2
      s2 += x
    }
    variance = ((n * s1) - (s2 ^ 2)) / (n ^ 2)
    return(sqrt(variance))
}

```

```txt

2 0
4 1
4 0.942809
4 0.866025
5 0.979796
5 1
7 1.39971
9 2

```



## Axiom

We implement a domain with dependent type T with the operation + and identity 0:
```Axiom
)abbrev package TESTD TestDomain
TestDomain(T : Join(Field,RadicalCategory)): Exports == Implementation where
  R ==> Record(n : Integer, sum : T, ssq : T)
  Exports == AbelianMonoid with
    _+ : (%,T) -> %
    _+ : (T,%) -> %
    sd : % -> T
  Implementation == R add
    Rep := R   -- similar representation and implementation
    obj : %
    0 == [0,0,0]
    obj + (obj2:%) == [obj.n + obj2.n, obj.sum + obj2.sum, obj.ssq + obj2.ssq]
    obj + (x:T) == obj + [1, x, x*x]
    (x:T) + obj == obj + x
    sd obj ==
      mean : T := obj.sum / (obj.n::T)
      sqrt(obj.ssq / (obj.n::T) - mean*mean)
```
This can be called using:
```axiom
T ==
 Expression Integer
D ==> TestDomain(T)
items := [2,4,4,4,5,5,7,9+x] :: List T;
map(sd, scan(+, items, 0$D))
                                        +---------------+
                +-+  +-+   +-+     +-+  |  2
              2\|2  \|3  2\|6    4\|6  \|7x  + 64x + 256
    (1)  [0,1,-----,----,-----,1,-----,------------------]
                3     2    5       7            8
                                              Type: List(Expression(Integer))
eval subst(last %,x=0)

    (2)  2
                                                    Type: Expression(Integer)
```



## BBC BASIC

Uses the MOD(array()) and SUM(array()) functions.

```bbcbasic
      MAXITEMS = 100
      FOR i% = 1 TO 8
        READ n
        PRINT "Value = "; n ", running SD = " FNrunningsd(n)
      NEXT
      END

      DATA 2,4,4,4,5,5,7,9

      DEF FNrunningsd(n)
      PRIVATE list(), i%
      DIM list(MAXITEMS)
      i% += 1
      list(i%) = n
      = SQR(MOD(list())^2/i% - (SUM(list())/i%)^2)
```

```txt

Value = 2, running SD = 0
Value = 4, running SD = 1
Value = 4, running SD = 0.942809043
Value = 4, running SD = 0.866025404
Value = 5, running SD = 0.979795901
Value = 5, running SD = 1
Value = 7, running SD = 1.39970842
Value = 9, running SD = 2

```



## C



```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef enum Action { STDDEV, MEAN, VAR, COUNT } Action;

typedef struct stat_obj_struct {
   double sum, sum2;
   size_t num;
   Action action;
} sStatObject, *StatObject;

StatObject NewStatObject( Action action )
{
  StatObject so;

  so = malloc(sizeof(sStatObject));
  so->sum = 0.0;
  so->sum2 = 0.0;
  so->num = 0;
  so->action = action;
  return so;
}
#define FREE_STAT_OBJECT(so) \
   free(so); so = NULL
double stat_obj_value(StatObject so, Action action)
{
  double num, mean, var, stddev;

  if (so->num == 0.0) return 0.0;
  num = so->num;
  if (action==COUNT) return num;
  mean = so->sum/num;
  if (action==MEAN) return mean;
  var = so->sum2/num - mean*mean;
  if (action==VAR) return var;
  stddev = sqrt(var);
  if (action==STDDEV) return stddev;
  return 0;
}

double stat_object_add(StatObject so, double v)
{
  so->num++;
  so->sum += v;
  so->sum2 += v*v;
  return stat_obj_value(so, so->action);
}
```



```c
double v[] = { 2,4,4,4,5,5,7,9 };

int main()
{
  int i;
  StatObject so = NewStatObject( STDDEV );

  for(i=0; i < sizeof(v)/sizeof(double) ; i++)
    printf("val: %lf  std dev: %lf\n", v[i], stat_object_add(so, v[i]));

  FREE_STAT_OBJECT(so);
  return 0;
}
```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace standardDeviation
{
    class Program
    {
        static void Main(string[] args)
        {
            List<double> nums = new List<double> { 2, 4, 4, 4, 5, 5, 7, 9 };
            for (int i = 1; i <= nums.Count; i++)
                Console.WriteLine(sdev(nums.GetRange(0, i)));
        }

        static double sdev(List<double> nums)
        {
            List<double> store = new List<double>();
            foreach (double n in nums)
                store.Add((n - nums.Average()) * (n - nums.Average()));

            return Math.Sqrt(store.Sum() / store.Count);
        }
    }
}
```


```txt
0
1
0,942809041582063
0,866025403784439
0,979795897113271
1
1,39970842444753
2
```



## C++

No attempt to handle different types -- standard deviation is intrinsically a real number.

```cpp

#include <assert.h>
#include <cmath>
#include <vector>
#include <iostream>

template<int N> struct MomentsAccumulator_
{
	std::vector<double> m_;
	MomentsAccumulator_() : m_(N + 1, 0.0) {}
	void operator()(double v)
	{
		double inc = 1.0;
		for (auto& mi : m_)
		{
			mi += inc;
			inc *= v;
		}
	}
};

double Stdev(const std::vector<double>& moments)
{
	assert(moments.size() > 2);
	assert(moments[0] > 0.0);
	const double mean = moments[1] / moments[0];
	const double meanSquare = moments[2] / moments[0];
	return sqrt(meanSquare - mean * mean);
}

int main(void)
{
	std::vector<int> data({ 2, 4, 4, 4, 5, 5, 7, 9 });
	MomentsAccumulator_<2> accum;
	for (auto d : data)
	{
		accum(d);
		std::cout << "Running stdev:  " << Stdev(accum.m_) << "\n";
	}
}

```



## Clojure



```lisp

(defn stateful-std-deviation[x]
  (letfn [(std-dev[x]
            (let [v (deref (find-var (symbol (str *ns* "/v"))))]
              (swap! v conj x)
              (let [m (/ (reduce + @v) (count @v))]
                (Math/sqrt (/ (reduce + (map #(* (- m %) (- m %)) @v)) (count @v))))))]
    (when (nil? (resolve 'v))
      (intern *ns* 'v (atom [])))
    (std-dev x)))

```



## COBOL


```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. run-stddev.
environment division.
input-output section.
file-control.
  select input-file assign to "input.txt"
    organization is line sequential.
data division.
file section.
fd input-file.
  01  inp-record.
    03  inp-fld  pic 9(03).
working-storage section.
01  filler pic 9(01)   value 0.
  88 no-more-input     value 1.
01  ws-tb-data.
  03  ws-tb-size         pic 9(03).
  03  ws-tb-table.
    05  ws-tb-fld     pic s9(05)v9999 comp-3 occurs 0 to 100 times
        depending on ws-tb-size.
01 ws-stddev       pic s9(05)v9999 comp-3.
PROCEDURE DIVISION.
  move 0 to ws-tb-size
  open  input input-file
    read input-file
    at end
      set no-more-input to true
    end-read
    perform
      test after
    until no-more-input
      add 1 to ws-tb-size
      move inp-fld to ws-tb-fld (ws-tb-size)
      call 'stddev' using  by reference ws-tb-data
         ws-stddev
      display  'inp=' inp-fld ' stddev=' ws-stddev
      read input-file at end set no-more-input to true end-read
    end-perform
  close input-file
  stop run.
end program run-stddev.
IDENTIFICATION DIVISION.
PROGRAM-ID. stddev.
data division.
working-storage section.
01 ws-tbx             pic s9(03) comp.
01 ws-tb-work.
  03  ws-sum          pic s9(05)v9999 comp-3 value +0.
  03  ws-sumsq        pic s9(05)v9999 comp-3 value +0.
  03  ws-avg          pic s9(05)v9999 comp-3 value +0.
linkage section.
01  ws-tb-data.
  03  ws-tb-size         pic 9(03).
  03  ws-tb-table.
    05  ws-tb-fld     pic s9(05)v9999 comp-3 occurs 0 to 100 times
        depending on ws-tb-size.
01  ws-stddev       pic s9(05)v9999 comp-3.
PROCEDURE DIVISION using  ws-tb-data  ws-stddev.
    compute ws-sum = 0
    perform test before varying ws-tbx from 1 by +1 until ws-tbx > ws-tb-size
        compute ws-sum = ws-sum + ws-tb-fld (ws-tbx)
    end-perform
    compute ws-avg rounded = ws-sum / ws-tb-size
    compute ws-sumsq = 0
    perform test before varying ws-tbx from 1 by +1 until ws-tbx > ws-tb-size
        compute ws-sumsq = ws-sumsq
        + (ws-tb-fld (ws-tbx) - ws-avg) ** 2.0
    end-perform
    compute ws-stddev = ( ws-sumsq / ws-tb-size) ** 0.5
    goback.
end program stddev.

```


```cobol
sample output:
inp=002 stddev=+00000.0000
inp=004 stddev=+00001.0000
inp=004 stddev=+00000.9427
inp=004 stddev=+00000.8660
inp=005 stddev=+00000.9797
inp=005 stddev=+00001.0000
inp=007 stddev=+00001.3996
inp=009 stddev=+00002.0000

```



## CoffeeScript

Uses a class instance to maintain state.


```coffeescript

class StandardDeviation
    constructor: ->
        @sum = 0
        @sumOfSquares = 0
        @values = 0
        @deviation = 0

    include: ( n ) ->
        @values += 1
        @sum += n
        @sumOfSquares += n * n
        mean = @sum / @values
        mean *= mean
        @deviation = Math.sqrt @sumOfSquares / @values - mean

dev = new StandardDeviation
values = [ 2, 4, 4, 4, 5, 5, 7, 9 ]
tmp = []

for value in values
    tmp.push value
    dev.include value
    console.log """
        Values: #{ tmp }
        Standard deviation: #{ dev.deviation }

    """

```


```txt

Values: 2
Standard deviation: 0

Values: 2,4
Standard deviation: 1

Values: 2,4,4
Standard deviation: 0.9428090415820626

Values: 2,4,4,4
Standard deviation: 0.8660254037844386

Values: 2,4,4,4,5
Standard deviation: 0.9797958971132716

Values: 2,4,4,4,5,5
Standard deviation: 1

Values: 2,4,4,4,5,5,7
Standard deviation: 1.3997084244475297

Values: 2,4,4,4,5,5,7,9
Standard deviation: 2

```



## Common Lisp

Since we don't care about the sample values once std dev is computed, we only need to keep track of their sum and square sums, hence:
```lisp
(defun running-stddev ()
  (let ((sum 0) (sq 0) (n 0))
    (lambda (x)
      (incf sum x) (incf sq (* x x)) (incf n)
      (/ (sqrt (- (* n sq) (* sum sum))) n))))

CL-USER> (loop with f = (running-stddev) for i in '(2 4 4 4 5 5 7 9) do
	(format t "~a ~a~%" i (funcall f i)))
NIL
2 0.0
4 1.0
4 0.94280905
4 0.8660254
5 0.97979593
5 1.0
7 1.3997085
9 2.0
```


In the REPL, one step at a time:

```lisp
CL-USER> (setf fn (running-stddev))
#<Interpreted Closure (:INTERNAL RUNNING-STDDEV) @ #x21b9a492>
CL-USER> (funcall fn 2)
0.0
CL-USER> (funcall fn 4)
1.0
CL-USER> (funcall fn 4)
0.94280905
CL-USER> (funcall fn 4)
0.8660254
CL-USER> (funcall fn 5)
0.97979593
CL-USER> (funcall fn 5)
1.0
CL-USER> (funcall fn 7)
1.3997085
CL-USER> (funcall fn 9)
2.0

```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE StandardDeviation;
IMPORT StdLog, Args,Strings,Math;

PROCEDURE Mean(x: ARRAY OF REAL; n: INTEGER; OUT mean: REAL);
VAR
	i: INTEGER;
	total: REAL;
BEGIN
	total := 0.0;
	FOR i := 0 TO n - 1 DO total := total + x[i] END;
	mean := total /n
END Mean;

PROCEDURE SDeviation(x : ARRAY OF REAL;n: INTEGER): REAL;
VAR
	i: INTEGER;
	mean,sum: REAL;
BEGIN
	Mean(x,n,mean);
	sum := 0.0;
	FOR i := 0 TO n - 1 DO
		sum:= sum +  ((x[i] - mean) * (x[i] - mean));
	END;
	RETURN Math.Sqrt(sum/n);
END SDeviation;

PROCEDURE Do*;
VAR
	p: Args.Params;
	x: POINTER TO ARRAY OF REAL;
	i,done: INTEGER;
BEGIN
	Args.Get(p);
	IF p.argc > 0 THEN
		NEW(x,p.argc);
		FOR i := 0 TO p.argc - 1 DO x[i] := 0.0 END;
		FOR i  := 0 TO p.argc - 1 DO
			Strings.StringToReal(p.args[i],x[i],done);
			StdLog.Int(i + 1);StdLog.String(" :> ");StdLog.Real(SDeviation(x,i + 1));StdLog.Ln
		END
	END
END Do;
END StandardDeviation.

```

Execute: ^Q StandardDeviation.Do 2 4 4 4 5 5 7 9 ~<br/>
```txt

 1 :>  0.0
 2 :>  1.0
 3 :>  0.9428090415820634
 4 :>  0.8660254037844386
 5 :>  0.9797958971132712
 6 :>  1.0
 7 :>  1.39970842444753
 8 :>  2.0

```



## D


```d
import std.stdio, std.math;

struct StdDev {
    real sum = 0.0, sqSum = 0.0;
    long nvalues;

    void addNumber(in real input) pure nothrow {
        nvalues++;
        sum += input;
        sqSum += input ^^ 2;
    }

    real getStdDev() const pure nothrow {
        if (nvalues == 0)
            return 0.0;
        immutable real mean = sum / nvalues;
        return sqrt(sqSum / nvalues - mean ^^ 2);
    }
}

void main() {
    StdDev stdev;

    foreach (el; [2.0, 4, 4, 4, 5, 5, 7, 9]) {
        stdev.addNumber(el);
        writefln("%e", stdev.getStdDev());
    }
}
```

```txt
0.000000e+00
1.000000e+00
9.428090e-01
8.660254e-01
9.797959e-01
1.000000e+00
1.399708e+00
2.000000e+00
```



## Delphi

See: [[#Pascal]]


## E


This implementation produces two (function) objects sharing state.
It is idiomatic in E to separate input from output (read from write)
rather than combining them into one object.

The algorithm is {{trans|Perl}} and the results were checked against [[#Python]].


```e
def makeRunningStdDev() {
    var sum := 0.0
    var sumSquares := 0.0
    var count := 0.0

    def insert(v) {
        sum += v
        sumSquares += v ** 2
        count += 1
    }

    /** Returns the standard deviation of the inputs so far, or null if there
        have been no inputs. */
    def stddev() {
        if (count > 0) {
            def meanSquares := sumSquares/count
            def mean := sum/count
            def variance := meanSquares - mean**2
            return variance.sqrt()
        }
    }

    return [insert, stddev]
}
```



```e
? def [insert, stddev] := makeRunningStdDev()
# value: <insert>, <stddev>

? [stddev()]
# value: [null]

? for value in [2,4,4,4,5,5,7,9] {
>     insert(value)
>     println(stddev())
> }
0.0
1.0
0.9428090415820626
0.8660254037844386
0.9797958971132716
1.0
1.3997084244475297
2.0
```



## Elixir

```elixir
defmodule Standard_deviation do
  def add_sample( pid, n ), do: send( pid, {:add, n} )

  def create, do: spawn_link( fn -> loop( [] ) end )

  def destroy( pid ), do: send( pid, :stop )

  def get( pid ) do
    send( pid, {:get, self()} )
    receive do
      { :get, value, _pid } -> value
    end
  end

  def task do
    pid = create()
    for x <- [2,4,4,4,5,5,7,9], do: add_print( pid, x, add_sample(pid, x) )
    destroy( pid )
  end

  defp add_print( pid, n, _add ) do
    IO.puts "Standard deviation #{ get(pid) } when adding #{ n }"
  end

  defp loop( ns ) do
    receive do
      {:add, n} -> loop( [n | ns] )
      {:get, pid} ->
        send( pid, {:get, loop_calculate( ns ), self()} )
        loop( ns )
      :stop -> :ok
    end
  end

  defp loop_calculate( ns ) do
    average = loop_calculate_average( ns )
    :math.sqrt( loop_calculate_average( for x <- ns, do: :math.pow(x - average, 2) ) )
  end

  defp loop_calculate_average( ns ), do: Enum.sum( ns ) / length( ns )
end

Standard_deviation.task
```


```txt

Standard deviation 0.0 when adding 2
Standard deviation 1.0 when adding 4
Standard deviation 0.9428090415820634 when adding 4
Standard deviation 0.8660254037844386 when adding 4
Standard deviation 0.9797958971132712 when adding 5
Standard deviation 1.0 when adding 5
Standard deviation 1.3997084244475302 when adding 7
Standard deviation 2.0 when adding 9

```



## Emacs Lisp


This implementation uses a temporary buffer (the central data structure of emacs) to have simple local variables.


```lisp
(defun running-std (x)
  ; ensure that we have a float to avoid potential integer math errors.
  (setq x (float x))
  ; define variables to use
  (defvar running-sum 0 "the running sum of all known values")
  (defvar running-len 0 "the running number of all known values")
  (defvar running-squared-sum 0 "the running squared sum of all known values")
  ; and make them local to this buffer
  (make-local-variable 'running-sum)
  (make-local-variable 'running-len)
  (make-local-variable 'running-squared-sum)
  ; now process the new value
  (setq running-sum (+ running-sum x))
  (setq running-len (1+ running-len))
  (setq running-squared-sum (+ running-squared-sum (* x x)))
  ; and calculate the new standard deviation
  (sqrt (- (/ running-squared-sum
              running-len) (/ (* running-sum running-sum)
                                 (* running-len running-len )))))
```



```lisp
(with-temp-buffer
  (loop for i in '(2 4 4 4 5 5 7 9) do
        (insert (number-to-string (running-std i)))
        (newline))
  (message (buffer-substring (point-min) (1- (point-max)))))

"0.0
1.0
0.9428090415820636
0.8660254037844386
0.9797958971132716
1.0
1.399708424447531
2.0"
```


Emacs Lisp with built-in Emacs Calc

<lang emacs-lisp>
(setq x '[2 4 4 4 5 5 7 9])
(string-to-number (calc-eval (format "sqrt(vpvar(%s))" x)))
```


Emacs Lisp with generator library (introduced in Emacs 25.1)

<lang emacs-lisp>
(require 'generator)
(setq lexical-binding t)
(iter-defun std-dev-gen (lst)
  (let ((sum 0)
	(avg 0)
	(tmp '())
	(std 0))
    (dolist (i lst)
      (setq i (float i))
      (push i tmp)
      (setq sum (+ sum i))
      (setq avg (/ sum (length tmp)))
      (setq std 0)
      (dolist (j tmp)
	(setq std (+ std (expt (- j avg) 2))))
      (setq std (/ std (length tmp)))
      (setq std (sqrt std))
      (iter-yield std))))

(let* ((test-data '(2 4 4 4 5 5 7 9))
      (generator (std-dev-gen test-data)))
  (dolist (i test-data)
    (princ (format "with %d : " i))
    (princ (format "%f\n" (iter-next generator)))))
```



## Erlang


```Erlang

-module( standard_deviation ).

-export( [add_sample/2, create/0, destroy/1, get/1, task/0] ).

-compile({no_auto_import,[get/1]}).

add_sample( Pid, N ) -> Pid ! {add, N}.

create() -> erlang:spawn_link( fun() -> loop( [] ) end ).

destroy( Pid ) -> Pid ! stop.

get( Pid ) ->
	Pid ! {get, erlang:self()},
	receive
	{get, Value, Pid} -> Value
	end.

task() ->
	Pid = create(),
	[add_print(Pid, X, add_sample(Pid, X)) || X <- [2,4,4,4,5,5,7,9]],
	destroy( Pid ).

add_print( Pid, N, _Add ) -> io:fwrite( "Standard deviation ~p when adding ~p~n", [get(Pid), N] ).

loop( Ns ) ->
	receive
	{add, N} -> loop( [N | Ns] );
	{get, Pid} ->
		Pid ! {get, loop_calculate( Ns ), erlang:self()},
		loop( Ns );
	stop -> ok
	end.

loop_calculate( Ns ) ->
	Average = loop_calculate_average( Ns ),
	math:sqrt( loop_calculate_average([math:pow(X - Average, 2) || X <- Ns]) ).

loop_calculate_average( Ns ) -> lists:sum( Ns ) / erlang:length( Ns ).

```

```txt

9> standard_deviation:task().
Standard deviation 0.0 when adding 2
Standard deviation 1.0 when adding 4
Standard deviation 0.9428090415820634 when adding 4
Standard deviation 0.8660254037844386 when adding 4
Standard deviation 0.9797958971132712 when adding 5
Standard deviation 1.0 when adding 5
Standard deviation 1.3997084244475302 when adding 7
Standard deviation 2.0 when adding 9

```



## Factor


```factor
USING: accessors io kernel math math.functions math.parser
sequences ;
IN: standard-deviator

TUPLE: standard-deviator sum sum^2 n ;

: <standard-deviator> ( -- standard-deviator )
    0.0 0.0 0 standard-deviator boa ;

: current-std ( standard-deviator -- std )
    [ [ sum^2>> ] [ n>> ] bi / ]
    [ [ sum>> ] [ n>> ] bi / sq ] bi - sqrt ;

: add-value ( value standard-deviator -- )
    [ nip [ 1 + ] change-n drop ]
    [ [ + ] change-sum drop ]
    [ [ [ sq ] dip + ] change-sum^2 drop ] 2tri ;

: main ( -- )
    { 2 4 4 4 5 5 7 9 }
    <standard-deviator> [ [ add-value ] curry each ] keep
    current-std number>string print ;
```



## Forth


```forth
: f+! ( x addr -- ) dup f@ f+ f! ;

: st-count ( stats -- n )                  f@ ;
: st-sum   ( stats -- sum )       float+   f@ ;
: st-sumsq ( stats -- sum*sum ) 2 floats + f@ ;

: st-mean ( stats -- mean )
  dup st-sum st-count f/ ;

: st-variance ( stats -- var )
  dup st-sumsq
  dup st-mean fdup f* dup st-count f*  f-
  st-count f/ ;

: st-stddev ( stats -- stddev )
  st-variance fsqrt ;

: st-add ( fnum stats -- )
  dup
    1e dup f+!  float+
  fdup dup f+!  float+
  fdup f*  f+!
  std-stddev ;
```


This variation is more numerically stable when there are large numbers of samples or large sample ranges.

```forth
: st-count ( stats -- n )                f@ ;
: st-mean  ( stats -- mean )    float+   f@ ;
: st-nvar  ( stats -- n*var ) 2 floats + f@ ;

: st-variance ( stats -- var ) dup st-nvar st-count f/ ;
: st-stddev ( stats -- stddev ) st-variance fsqrt ;

: st-add ( x stats -- )
  dup
  1e dup f+!			\ update count
  fdup dup st-mean f- fswap
  ( delta x )
  fover dup st-count f/
  ( delta x delta/n )
  float+ dup f+!		\ update mean
  ( delta x )
  dup f@ f-  f*  float+ f+!	\ update nvar
  st-stddev ;
```

Usage example:

```forth
create stats 0e f, 0e f, 0e f,

2e stats st-add f. \ 0.
4e stats st-add f. \ 1.
4e stats st-add f. \ 0.942809041582063
4e stats st-add f. \ 0.866025403784439
5e stats st-add f. \ 0.979795897113271
5e stats st-add f. \ 1.
7e stats st-add f. \ 1.39970842444753
9e stats st-add f. \ 2.

```



## Fortran

```fortran

program standard_deviation
  implicit none
  integer(kind=4), parameter :: dp = kind(0.0d0)

  real(kind=dp), dimension(:), allocatable :: vals
  integer(kind=4) :: i

  real(kind=dp), dimension(8) :: sample_data = (/ 2, 4, 4, 4, 5, 5, 7, 9 /)

  do i = lbound(sample_data, 1), ubound(sample_data, 1)
    call sample_add(vals, sample_data(i))
    write(*, fmt='(''#'',I1,1X,''value = '',F3.1,1X,''stddev ='',1X,F10.8)') &
      i, sample_data(i), stddev(vals)
  end do

  if (allocated(vals)) deallocate(vals)
contains
  ! Adds value :val: to array :population: dynamically resizing array
  subroutine sample_add(population, val)
    real(kind=dp), dimension(:), allocatable, intent (inout) :: population
    real(kind=dp), intent (in) :: val

    real(kind=dp), dimension(:), allocatable :: tmp
    integer(kind=4) :: n

    if (.not. allocated(population)) then
      allocate(population(1))
      population(1) = val
    else
      n = size(population)
      call move_alloc(population, tmp)

      allocate(population(n + 1))
      population(1:n) = tmp
      population(n + 1) = val
    endif
  end subroutine sample_add

  ! Calculates standard deviation for given set of values
  real(kind=dp) function stddev(vals)
    real(kind=dp), dimension(:), intent(in) :: vals
    real(kind=dp) :: mean
    integer(kind=4) :: n

    n = size(vals)
    mean = sum(vals)/n
    stddev = sqrt(sum((vals - mean)**2)/n)
  end function stddev
end program standard_deviation

```

```txt

#1 value = 2.0 stddev = 0.00000000
#2 value = 4.0 stddev = 1.00000000
#3 value = 4.0 stddev = 0.94280904
#4 value = 4.0 stddev = 0.86602540
#5 value = 5.0 stddev = 0.97979590
#6 value = 5.0 stddev = 1.00000000
#7 value = 7.0 stddev = 1.39970842
#8 value = 9.0 stddev = 2.00000000

```


===Old style, four ways===
Early computers loaded the entire programme and its working storage into memory and left it there throughout the run. Uninitialised variables would start with whatever had been left in memory at their address by whatever last used those addresses, though some systems would clear all of memory to zero or possibly some other value before each load. Either way, if a routine was invoked a second time, its variables would have the values left in them by their previous invocation. The DATA statement allows initial values to be specified, and allows repeat counts when specifying such values as well. It is not an executable statement: it is not re-executed on second and subsequent invocations of the containing routine. Thus, it is easy to have a routine employ counters and the like, visible only within themselves and initialised to zero or whatever suited.

With more complex operating systems, routines that relied on retaining values across invocations might no longer work - perhaps a fresh version of the routine would be loaded to memory (perhaps at odd intervals), or, on exit, the working storage would be discarded. There was a half-way scheme, whereby variables that had appeared in DATA statements would be retained while the others would be discarded. This subtle indication has been discarded in favour of the explicit SAVE statement, naming those variables whose values are to be retained between invocations, though compilers might also offer an option such as "automatic" (for each invocation always allocate then discard working memory) and "static" (retain values), possibly introducing non-standard keywords as well. Otherwise, the routines would have to use storage global to them such as additional parameters, or, COMMON storage and in later Fortran, the MODULE arrangements for shared items. The persistence of such storage can still be limited, but by naming them in the main line can be ensured for the life of the run. The other routines with access to such storage could enable re-initialisation, additional reports, or multiple accumulations, etc.

Since the standard deviation can be calculated in a single pass through the data, producing values for the standard deviation of all values so far supplied is easily done without re-calculation. Accuracy is quite another matter. Calculations using deviances from a working mean are much better, and capturing the first X as the working mean would be easy, just test on N = 0. The sum and sum-of-squares method is quite capable of generating a negative variance, but the second method cannot, because the terms being added in to V are never negative. This is demonstrated by comparing the results computed from StdDev(A), StdDev(A + 10), StdDev(A + 100), StdDev(A + 1000), etc.

Incidentally, Fortran implementations rarely enable re-entrancy for the WRITE statement, so, since here the functions are invoked in a WRITE statement, the functions cannot themselves use WRITE statements, say for debugging.

```Fortran

      REAL FUNCTION STDDEV(X)	!Standard deviation for successive values.
       REAL X		!The latest value.
       REAL V		!Scratchpad.
       INTEGER N	!Ongoing: count of the values.
       REAL EX,EX2	!Ongoing: sum of X and X**2.
       SAVE N,EX,EX2		!Retain values from one invocation to the next.
       DATA N,EX,EX2/0,0.0,0.0/	!Initial values.
        N = N + 1		!Another value arrives.
        EX = X + EX		!Augment the total.
        EX2 = X**2 + EX2	!Augment the sum of squares.
        V = EX2/N - (EX/N)**2	!The variance, but, it might come out negative!
        STDDEV = SIGN(SQRT(ABS(V)),V)	!Protect the SQRT, but produce a negative result if so.
      END FUNCTION STDDEV	!For the sequence of received X values.

      REAL FUNCTION STDDEVP(X)	!Standard deviation for successive values.
       REAL X		!The latest value.
       INTEGER N	!Ongoing: count of the values.
       REAL A,V		!Ongoing: average, and sum of squared deviations.
       SAVE N,A,V		!Retain values from one invocation to the next.
       DATA N,A,V/0,0.0,0.0/	!Initial values.
        N = N + 1		!Another value arrives.
        V = (N - 1)*(X - A)**2 /N + V	!First, as it requires the existing average.
        A = (X - A)/N + A		!= [x + (n - 1).A)]/n: recover the total from the average.
        STDDEVP = SQRT(V/N)	!V can never be negative, even with limited precision.
      END FUNCTION STDDEVP	!For the sequence of received X values.

      REAL FUNCTION STDDEVW(X)	!Standard deviation for successive values.
       REAL X		!The latest value.
       REAL V,D		!Scratchpads.
       INTEGER N	!Ongoing: count of the values.
       REAL EX,EX2	!Ongoing: sum of X and X**2.
       REAL W		!Ongoing: working mean.
       SAVE N,EX,EX2,W		!Retain values from one invocation to the next.
       DATA N,EX,EX2/0,0.0,0.0/	!Initial values.
        IF (N.LE.0) W = X	!Take the first value as the working mean.
        N = N + 1		!Another value arrives.
        D = X - W		!Its deviation from the working mean.
        EX = D + EX		!Augment the total.
        EX2 = D**2 + EX2	!Augment the sum of squares.
        V = EX2/N - (EX/N)**2	!The variance, but, it might come out negative!
        STDDEVW = SIGN(SQRT(ABS(V)),V)	!Protect the SQRT, but produce a negative result if so.
      END FUNCTION STDDEVW	!For the sequence of received X values.

      REAL FUNCTION STDDEVPW(X)	!Standard deviation for successive values.
       REAL X		!The latest value.
       INTEGER N	!Ongoing: count of the values.
       REAL A,V		!Ongoing: average, and sum of squared deviations.
       REAL W		!Ongoing: working mean.
       SAVE N,A,V,W		!Retain values from one invocation to the next.
       DATA N,A,V/0,0.0,0.0/	!Initial values.
        IF (N.LE.0) W = X	!Oh for self-modifying code!
        N = N + 1		!Another value arrives.
        D = X - W		!Its deviation from the working mean.
        V = (N - 1)*(D - A)**2 /N + V	!First, as it requires the existing average.
        A = (D - A)/N + A		!= [x + (n - 1).A)]/n: recover the total from the average.
        STDDEVPW = SQRT(V/N)	!V can never be negative, even with limited precision.
      END FUNCTION STDDEVPW	!For the sequence of received X values.

      PROGRAM TEST
      INTEGER I		!A stepper.
      REAL A(8)		!The example data.
      DATA A/2.0,3*4.0,2*5.0,7.0,9.0/	!Alas, another opportunity to use @ passed over.
      REAL B		!An offsetting base.
      WRITE (6,1)
    1 FORMAT ("Progressive calculation of the standard deviation."/
     1 " I",7X,"A(I)       EX EX2      Av V*N      Ed Ed2     wAv V*N")
      B = 1000000		!Provoke truncation error.
      DO I = 1,8			!Step along the data series,
        WRITE (6,2) I,INT(A(I) + B),		!No fractional part, so I don't want F11.0.
     1   STDDEV(A(I) + B),STDDEVP(A(I) + B),	!Showing progressive values.
     2  STDDEVW(A(I) + B),STDDEVPW(A(I) + B)	!These with a working mean.
    2   FORMAT (I2,I11,1X,4F12.6)		!Should do for the example.
      END DO				!On to the next value.
      END

```


Output: the second pair of columns have the calculations done with a working mean and thus accumulate deviations from that.
        Progressive calculation of the standard deviation.
 I       A(I)       EX EX2      Av V*N      Ed Ed2     wAv V*N
 1          2     0.000000    0.000000    0.000000    0.000000
 2          4     1.000000    1.000000    1.000000    1.000000
 3          4     0.942809    0.942809    0.942809    0.942809
 4          4     0.866025    0.866025    0.866025    0.866025
 5          5     0.979796    0.979796    0.979796    0.979796
 6          5     1.000000    1.000000    1.000000    1.000000
 7          7     1.399708    1.399708    1.399708    1.399708
 8          9     2.000000    2.000000    2.000000    2.000000

 I       A(I)       EX EX2      Av V*N      Ed Ed2     wAv V*N
 1         12     0.000000    0.000000    0.000000    0.000000
 2         14     1.000000    1.000000    1.000000    1.000000
 3         14     0.942809    0.942809    0.942809    0.942809
 4         14     0.866025    0.866025    0.866025    0.866025
 5         15     0.979796    0.979796    0.979796    0.979796
 6         15     1.000000    1.000000    1.000000    1.000000
 7         17     1.399708    1.399708    1.399708    1.399708
 8         19     2.000000    2.000000    2.000000    2.000000

 I       A(I)       EX EX2      Av V*N      Ed Ed2     wAv V*N
 1        102     0.000000    0.000000    0.000000    0.000000
 2        104     1.000000    1.000000    1.000000    1.000000
 3        104     0.942809    0.942809    0.942809    0.942809
 4        104     0.866025    0.866025    0.866025    0.866025
 5        105     0.979796    0.979796    0.979796    0.979796
 6        105     1.000000    0.999999    1.000000    1.000000
 7        107     1.399708    1.399708    1.399708    1.399708
 8        109     2.000000    1.999999    2.000000    2.000000

 I       A(I)       EX EX2      Av V*N      Ed Ed2     wAv V*N
 1       1002     0.000000    0.000000    0.000000    0.000000
 2       1004     1.000000    1.000000    1.000000    1.000000
 3       1004     0.942809    0.942809    0.942809    0.942809
 4       1004     0.866025    0.866028    0.866025    0.866025
 5       1005     0.979796    0.979798    0.979796    0.979796
 6       1005     1.000000    1.000004    1.000000    1.000000
 7       1007     1.399708    1.399711    1.399708    1.399708
 8       1009     2.000000    1.999997    2.000000    2.000000

 I       A(I)       EX EX2      Av V*N      Ed Ed2     wAv V*N
 1      10002    -2.000000    0.000000    0.000000    0.000000
 2      10004    -1.000000    1.000000    1.000000    1.000000
 3      10004    -0.666667    0.942809    0.942809    0.942809
 4      10004     1.936492    0.866072    0.866025    0.866025
 5      10005     2.181742    0.979829    0.979796    0.979796
 6      10005     2.309401    1.000060    1.000000    1.000000
 7      10007     1.801360    1.399745    1.399708    1.399708
 8      10009     2.645751    1.999987    2.000000    2.000000

 I       A(I)       EX EX2      Av V*N      Ed Ed2     wAv V*N
 1     100002    19.493589    0.000000    0.000000    0.000000
 2     100004     7.416198    1.000000    1.000000    1.000000
 3     100004    -7.333333    0.942809    0.942809    0.942809
 4     100004    20.093531    0.865650    0.866025    0.866025
 5     100005    -1.280625    0.979531    0.979796    0.979796
 6     100005   -16.492422    1.000305    1.000000    1.000000
 7     100007    17.851427    1.399895    1.399708    1.399708
 8     100009    20.566963    1.999835    2.000000    2.000000

 I       A(I)       EX EX2      Av V*N      Ed Ed2     wAv V*N
 1    1000002   -80.024994    0.000000    0.000000    0.000000
 2    1000004   158.767120    1.000000    1.000000    1.000000
 3    1000004   -89.146576    0.942809    0.942809    0.942809
 4    1000004    90.795097    0.869074    0.866025    0.866025
 5    1000005   193.357590    0.981953    0.979796    0.979796
 6    1000005   238.361069    0.999691    1.000000    1.000000
 7    1000007   153.462296    1.399519    1.399708    1.399708
 8    1000009   151.284500    1.997653    2.000000    2.000000

Speaking loosely, to square a number of d digits accurately requires the ability to represent 2d digits accurately, with more digits needed if many such squares are to be added together accurately. In this example, 1000 when squared, is pushing at the limits of single precision. The average&variance method is resistant to this problem (and does not generate negative variances either!) because it manipulates differences from the running average, but it is still better to use a working mean.

In other words, a two-pass method will be more accurate (where the second pass calculates the variance by accumulating deviations from the actual average, itself calculated with a working mean) but at the cost of that second pass and the saving of all the values. Higher precision variables for the accumulations are the easiest way towards accurate results.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function calcStandardDeviation(number As Double) As Double
  Static a() As Double
  Redim Preserve a(0 To UBound(a) + 1)
  Dim ub As UInteger = UBound(a)
  a(ub) = number
  Dim sum As Double = 0.0
  For i As UInteger = 0 To ub
    sum += a(i)
  Next
  Dim mean As Double = sum / (ub + 1)
  Dim diff As Double
  sum  = 0.0
  For i As UInteger = 0 To ub
    diff = a(i) - mean
    sum += diff * diff
  Next
  Return Sqr(sum/ (ub + 1))
End Function

Dim a(0 To 7) As Double = {2, 4, 4, 4, 5, 5, 7, 9}

For i As UInteger = 0 To 7
  Print "Added"; a(i); " SD now : "; calcStandardDeviation(a(i))
Next

Print
Print "Press any key to quit"
Sleep
```


```txt

Added 2 SD now :  0
Added 4 SD now :  1
Added 4 SD now :  0.9428090415820634
Added 4 SD now :  0.8660254037844386
Added 5 SD now :  0.9797958971132712
Added 5 SD now :  1
Added 7 SD now :  1.39970842444753
Added 9 SD now :  2

```



## Go

Algorithm to reduce rounding errors from WP article.

State maintained with a closure.

```go
package main

import (
    "fmt"
    "math"
)

func newRsdv() func(float64) float64 {
    var n, a, q  float64
    return func(x float64) float64 {
        n++
        a1 := a+(x-a)/n
        q, a = q+(x-a)*(x-a1), a1
        return math.Sqrt(q/n)
    }
}

func main() {
    r := newRsdv()
    for _, x := range []float64{2,4,4,4,5,5,7,9} {
        fmt.Println(r(x))
    }
}
```

```txt

0
1
0.9428090415820634
0.8660254037844386
0.9797958971132713
1
1.3997084244475304
2

```



## Groovy

Solution:

```groovy
List samples = []

def stdDev = { def sample ->
    samples << sample
    def sum = samples.sum()
    def sumSq = samples.sum { it * it }
    def count = samples.size()
    (sumSq/count - (sum/count)**2)**0.5
}

[2,4,4,4,5,5,7,9].each {
    println "${stdDev(it)}"
}
```


```txt
0
1
0.9428090416999145
0.8660254037844386
0.9797958971132712
1
1.3997084243469262
2
```



## Haskell


We store the state in the <code>ST</code> monad using an <code>STRef</code>.


```haskell
{-# LANGUAGE BangPatterns #-}

import Data.List (foldl') -- '
import Data.STRef
import Control.Monad.ST

data Pair a b = Pair !a !b

sumLen :: [Double] -> Pair Double Double
sumLen = fiof2 . foldl' (\(Pair s l) x -> Pair (s+x) (l+1)) (Pair 0.0 0) --'
  where fiof2 (Pair s l) = Pair s (fromIntegral l)

divl :: Pair Double Double -> Double
divl (Pair _ 0.0) = 0.0
divl (Pair s   l) = s / l

sd :: [Double] -> Double
sd xs = sqrt $ foldl' (\a x -> a+(x-m)^2) 0 xs / l --'
  where p@(Pair s l) = sumLen xs
        m = divl p

mkSD :: ST s (Double -> ST s Double)
mkSD = go <$> newSTRef []
  where go acc x =
          modifySTRef acc (x:) >> (sd <$> readSTRef acc)

main = mapM_ print $ runST $
  mkSD >>= forM [2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0]
```



Or, simply accumulating across a fold:
```Haskell
type Index = Int
type DataPoint = Float

type Sum = Float
type SumOfSquares = Float

type Deviations = [Float]
type Accumulator = (Sum, SumOfSquares, Deviations)

stdDevInc :: Accumulator -> (DataPoint, Index) -> Accumulator
stdDevInc (s, q, ds) (x, i) = (_s, _q, _ds)
  where
    _s = s + x
    _q = q + (x ^ 2)
    _i = fromIntegral i
    _ds = ds ++ [sqrt ((_q / _i) - ((_s / _i) ^ 2))]

sample :: [DataPoint]
sample = [2, 4, 4, 4, 5, 5, 7, 9]

-- The Prelude definition of foldl' --'
-- adjusted to avoid wiki formatting glitches.
foldl_ :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl_ f z0 xs = foldr f_ id xs z0
  where f_ x k z = k $! f z x

main :: IO ()
main = mapM_ print devns
  where
    (_, _, devns) = foldl_ stdDevInc (0, 0, []) $ zip sample [1 .. ]
```

```txt
0.0
1.0
0.9428093
0.8660254
0.97979593
1.0
1.3997087
2.0
```



## Haxe


```haxe
using Lambda;

class Main {
	static function main():Void {
		var nums = [2, 4, 4, 4, 5, 5, 7, 9];
		for (i in 1...nums.length+1)
			Sys.println(sdev(nums.slice(0, i)));
	}

	static function average<T:Float>(nums:Array<T>):Float {
		return nums.fold(function(n, t) return n + t, 0) / nums.length;
	}

	static function sdev<T:Float>(nums:Array<T>):Float {
		var store = [];
		var avg = average(nums);
		for (n in nums) {
			store.push((n - avg) * (n - avg));
		}

		return Math.sqrt(average(store));
	}
}
```


```txt
0
1
0.942809041582063
0.866025403784439
0.979795897113271
1
1.39970842444753
2
```



## HicEst


```HicEst
REAL :: n=8, set(n), sum=0, sum2=0

set = (2,4,4,4,5,5,7,9)

DO k = 1, n
   WRITE() 'Adding ' // set(k) // 'stdev = ' // stdev(set(k))
ENDDO

END ! end of "main"

FUNCTION stdev(x)
   USE : sum, sum2, k
   sum = sum + x
   sum2 = sum2 + x*x
   stdev = ( sum2/k - (sum/k)^2) ^ 0.5
 END
```


```txt
Adding 2 stdev = 0
Adding 4 stdev = 1
Adding 4 stdev = 0.9428090416
Adding 4 stdev = 0.8660254038
Adding 5 stdev = 0.9797958971
Adding 5 stdev = 1
Adding 7 stdev = 1.399708424
Adding 9 stdev = 2
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()

stddev() # reset state / empty
every  s := stddev(![2,4,4,4,5,5,7,9]) do
   write("stddev (so far) := ",s)

end

procedure stddev(x)  /: running standard deviation
static X,sumX,sum2X

   if /x then {   # reset state
      X := []
      sumX := sum2X := 0.
      }
   else {         # accumulate
      put(X,x)
      sumX +:= x
      sum2X +:= x^2
      return sqrt( (sum2X / *X) - (sumX / *X)^2 )
      }
end
```

```txt
stddev (so far) := 0.0
stddev (so far) := 1.0
stddev (so far) := 0.9428090415820626
stddev (so far) := 0.8660254037844386
stddev (so far) := 0.9797958971132716
stddev (so far) := 1.0
stddev (so far) := 1.39970842444753
stddev (so far) := 2.0
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "StDev.bas"
110 LET N=8
120 NUMERIC ARR(1 TO N)
130 FOR I=1 TO N
140   READ ARR(I)
150 NEXT
160 DEF STDEV(N)
170   LET S1,S2=0
180   FOR I=1 TO N
190     LET S1=S1+ARR(I)^2:LET S2=S2+ARR(I)
200   NEXT
210   LET STDEV=SQR((N*S1-S2^2)/N^2)
220 END DEF
230 FOR J=1 TO N
240   PRINT J;"item =";ARR(J),"standard dev =";STDEV(J)
250 NEXT
260 DATA 2,4,4,4,5,5,7,9
```



## J


J is block-oriented; it expresses algorithms with the semantics of all the data being available at once.  It does not have native lexical closure or coroutine semantics.  It is possible to implement these semantics in J; see [[Moving Average]] for an example.  We will not reprise that here.

```j
   mean=: +/ % #
   dev=: - mean
   stddevP=: [: %:@mean *:@dev          NB. A) 3 equivalent defs for stddevP
   stddevP=: [: mean&.:*: dev           NB. B) uses Under (&.:) to apply inverse of *: after mean
   stddevP=: %:@(mean@:*: - *:@mean)    NB. C) sqrt of ((mean of squares) - (square of mean))


   stddevP\ 2 4 4 4 5 5 7 9
0 1 0.942809 0.866025 0.979796 1 1.39971 2
```


'''Alternatives:'''

Using verbose names for J primitives.

```j
   of     =: @:
   sqrt   =: %:
   sum    =: +/
   squares=: *:
   data   =: ]
   mean   =: sum % #

   stddevP=: sqrt of mean of squares of (data-mean)

   stddevP\ 2 4 4 4 5 5 7 9
0 1 0.942809 0.866025 0.979796 1 1.39971 2
```


{{trans|R}}<BR>
Or we could take a cue from the [[#R|R implementation]] and reverse the Bessel correction to stddev:


```j
   require'stats'
   (%:@:(%~<:)@:# * stddev)\ 2 4 4 4 5 5 7 9
0 1 0.942809 0.866025 0.979796 1 1.39971 2
```



## Java


```java
public class StdDev {
    int n = 0;
    double sum = 0;
    double sum2 = 0;

    public double sd(double x) {
	n++;
	sum += x;
	sum2 += x*x;

	return Math.sqrt(sum2/n - sum*sum/n/n);
    }

    public static void main(String[] args) {
        double[] testData = {2,4,4,4,5,5,7,9};
        StdDev sd = new StdDev();

        for (double x : testData) {
            System.out.println(sd.sd(x));
        }
    }
}
```



## JavaScript



### Imperative


Uses a closure.

```javascript
function running_stddev() {
    var n = 0;
    var sum = 0.0;
    var sum_sq = 0.0;
    return function(num) {
        n++;
        sum += num;
        sum_sq += num*num;
        return Math.sqrt( (sum_sq / n) - Math.pow(sum / n, 2) );
    }
}

var sd = running_stddev();
var nums = [2,4,4,4,5,5,7,9];
var stddev = [];
for (var i in nums)
    stddev.push( sd(nums[i]) );

// using WSH
WScript.Echo(stddev.join(', ');
```


```txt
0, 1, 0.942809041582063, 0.866025403784439, 0.979795897113273, 1, 1.39970842444753, 2
```


===Functional (ES 5)===

Accumulating across a fold


```JavaScript
(function (xs) {

    return xs.reduce(function (a, x, i) {
        var n = i + 1,
            sum_ = a.sum + x,
            squaresSum_ = a.squaresSum + (x * x);

        return {
            sum: sum_,
            squaresSum: squaresSum_,
            stages: a.stages.concat(
                Math.sqrt((squaresSum_ / n) - Math.pow((sum_ / n), 2))
            )
        };

    }, {
        sum: 0,
        squaresSum: 0,
        stages: []
    }).stages

})([2, 4, 4, 4, 5, 5, 7, 9]);
```


```JavaScript
[0, 1, 0.9428090415820626, 0.8660254037844386,
0.9797958971132716, 1, 1.3997084244475297, 2]
```



## jq


### =Observations from a file or array=

We first define a filter, "simulate", that, if given a file of
observations, will emit the standard deviations of the observations
seen so far.
The current state is stored in a JSON object, with this structure:

 { "n": _, "ssd": _, "mean": _ }

where "n" is the number of observations seen, "mean" is their average, and "ssd" is the sum of squared deviations about that mean.

The challenge here is to ensure accuracy for very large n, without sacrificing efficiency.  The key idea in that regard is that if m is the mean of a series of n observations, x, we then have for any a:

 SIGMA( (x - a)^2 ) == SIGMA( (x-m)^2 ) + n * (a-m)^2 == SSD + n*(a-m)^2
 where SSD is the sum of squared deviations about the mean.


```jq
# Compute the standard deviation of the observations
# seen so far, given the current state as input:
def standard_deviation: .ssd / .n | sqrt;

def update_state(observation):
  def sq: .*.;
  ((.mean * .n + observation) / (.n + 1)) as $newmean
  | (.ssd + .n * ((.mean - $newmean) | sq)) as $ssd
  | { "n": (.n + 1),
      "ssd":  ($ssd + ((observation - $newmean) | sq)),
      "mean": $newmean }
;

def initial_state: { "n": 0, "ssd": 0, "mean": 0 };

# Given an array of observations presented as input:
def simulate:
  def _simulate(i; observations):
    if (observations|length) <= i then empty
    else update_state(observations[i])
      | standard_deviation, _simulate(i+1; observations)
    end ;
  . as $in | initial_state | _simulate(0; $in);

# Begin:
simulate
```

'''Example 1'''
 # observations.txt
 2
 4
 4
 4
 5
 5
 7
 9
```sh

$ jq -s -f Dynamic_standard_deviation.jq observations.txt
0
1
0.9428090415820634
0.8660254037844386
0.9797958971132711
0.9999999999999999
1.3997084244475302
1.9999999999999998

```


### =Observations from a stream=

Recent versions of jq (after 1.4) support retention of state while processing a stream. This means that any generator (including generators that produce items indefinitely) can be used as the source of observations, without first having to capture all the observations, e.g. in a file or array.

```jq
# requires jq version
 1.4
def simulate(stream):
  foreach stream as $observation
    (initial_state;
     update_state($observation);
     standard_deviation);
```

'''Example 2''':
 simulate( range(0;10) )
 0
 0.5
 0.816496580927726
 1.118033988749895
 1.4142135623730951
 1.707825127659933
 2
 2.29128784747792
 2.581988897471611
 2.8722813232690143


### =Observations from an external stream=

The following illustrates how jq can be used to process observations from an external (potentially unbounded) stream, one at a time.  Here we use <tt>bash</tt> to manage the calls to jq.

The definitions of the filters update_state/1 and initial_state/0 are as above but are repeated so that this script is self-contained.

```sh
#!/bin/bash

# jq is assumed to be on PATH

PROGRAM='
def standard_deviation: .ssd / .n | sqrt;

def update_state(observation):
  def sq: .*.;
  ((.mean * .n + observation) / (.n + 1)) as $newmean
  | (.ssd + .n * ((.mean - $newmean) | sq)) as $ssd
  | { "n": (.n + 1),
      "ssd":  ($ssd + ((observation - $newmean) | sq)),
      "mean": $newmean }
;

def initial_state: { "n": 0, "ssd": 0, "mean": 0 };

# Input should be [observation, null] or [observation, state]
def standard_deviations:
  . as $in
  | if type == "array" then
      (if .[1] == null then initial_state else .[1] end) as $state
      | $state | update_state($in[0])
      | standard_deviation, .
    else empty
    end
;

standard_deviations
'
state=null
while read -p "Next observation: " observation
do
  result=$(echo "[ $observation, $state ]" | jq -c "$PROGRAM")
  sed -n 1p <<< "$result"
  state=$(sed -n 2p <<< "$result")
done
```

'''Example 3'''

```sh
$ ./standard_deviation_server.sh
Next observation: 10
0
Next observation: 20
5
Next observation: 0
8.16496580927726

```



## Julia

Use a closure to create a running standard deviation function.

```julia
function makerunningstd(::Type{T} = Float64) where T
    ∑x = ∑x² = zero(T)
    n = 0
    function runningstd(x)
        ∑x  += x
        ∑x² += x ^ 2
        n   += 1
        s   = ∑x² / n - (∑x / n) ^ 2
        return s
    end
    return runningstd
end

test = Float64[2, 4, 4, 4, 5, 5, 7, 9]
rstd = makerunningstd()

println("Perform a running standard deviation of ", test)
for i in test
    println(" - add $i → ", rstd(i))
end
```


```txt
Perform a running standard deviation of [2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0]
 - add 2.0 → 0.0
 - add 4.0 → 1.0
 - add 4.0 → 0.8888888888888875
 - add 4.0 → 0.75
 - add 5.0 → 0.9600000000000009
 - add 5.0 → 1.0
 - add 7.0 → 1.9591836734693864
 - add 9.0 → 4.0

```



## Kotlin

Using a class to keep the running sum, sum of squares and number of elements added so far:

```scala
// version 1.0.5-2

class CumStdDev {
    private var n = 0
    private var sum = 0.0
    private var sum2 = 0.0

    fun sd(x: Double): Double {
        n++
        sum += x
        sum2 += x * x
        return Math.sqrt(sum2 / n - sum * sum / n / n)
    }
}

fun main(args: Array<String>) {
    val testData = doubleArrayOf(2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0)
    val csd = CumStdDev()
    for (d in testData) println("Add $d => ${csd.sd(d)}")
}
```


```txt

Add 2.0 => 0.0
Add 4.0 => 1.0
Add 4.0 => 0.9428090415820626
Add 4.0 => 0.8660254037844386
Add 5.0 => 0.9797958971132708
Add 5.0 => 1.0
Add 7.0 => 1.399708424447531
Add 9.0 => 2.0

```



## Liberty BASIC

Using a global array to maintain the state. Implements definition explicitly.

```lb

    dim SD.storage$( 100)   '   can call up to 100 versions, using ID to identify.. arrays are global.
                            '   holds (space-separated) number of data items so far, current sum.of.values and current sum.of.squares

    for i =1 to 8
        read x
        print "New data "; x; " so S.D. now = "; using( "###.######", standard.deviation( 1, x))
    next i

    end

function standard.deviation( ID, in)
  if SD.storage$( ID) ="" then SD.storage$( ID) ="0 0 0"
  num.so.far =val( word$( SD.storage$( ID), 1))
  sum.vals   =val( word$( SD.storage$( ID), 2))
  sum.sqs    =val( word$( SD.storage$( ID), 3))
  num.so.far =num.so.far +1
  sum.vals   =sum.vals   +in
  sum.sqs    =sum.sqs    +in^2

  ' standard deviation = square root of (the average of the squares less the square of the average)
  standard.deviation   =(               ( sum.sqs /num.so.far)      -    ( sum.vals /num.so.far)^2)^0.5

  SD.storage$( ID) =str$( num.so.far) +" " +str$( sum.vals) +" " +str$( sum.sqs)
end function

    Data 2, 4, 4, 4, 5, 5, 7, 9

```


```txt

New data 2 so S.D. now =   0.000000
New data 4 so S.D. now =   1.000000
New data 4 so S.D. now =   0.942809
New data 4 so S.D. now =   0.866025
New data 5 so S.D. now =   0.979796
New data 5 so S.D. now =   1.000000
New data 7 so S.D. now =   1.399708
New data 9 so S.D. now =   2.000000

```



## Lua

Uses a closure. Translation of JavaScript.

```lua
function stdev()
  local sum, sumsq, k = 0,0,0
  return function(n)
    sum, sumsq, k = sum + n, sumsq + n^2, k+1
    return math.sqrt((sumsq / k) - (sum/k)^2)
  end
end

ldev = stdev()
for i, v in ipairs{2,4,4,4,5,5,7,9} do
  print(ldev(v))
end
```



## Mathematica


```Mathematica
runningSTDDev[n_] := (If[Not[ValueQ[$Data]], $Data = {}];
  StandardDeviation[AppendTo[$Data, n]])
```


=={{header|MATLAB}} / {{header|Octave}}==
The simple form is, computing only the standand deviation of the whole data set:


```Matlab
  x = [2,4,4,4,5,5,7,9];
  n = length (x);

  m  = mean (x);
  x2 = mean (x .* x);
  dev= sqrt (x2 - m * m)
  dev = 2
```


When the intermediate results are also needed, one can use this vectorized form:


```Matlab
  m = cumsum(x) ./ [1:n];	% running mean
  x2= cumsum(x.^2) ./ [1:n];   % running squares

  dev = sqrt(x2 - m .* m)
  dev =
   0.00000   1.00000   0.94281   0.86603   0.97980   1.00000   1.39971   2.00000


```


Here is a vectorized one line solution as a function

```Matlab

function  stdDevEval(n)
disp(sqrt(sum((n-sum(n)/length(n)).^2)/length(n)));
end

```


=={{header|MK-61/52}}==
<lang>0	П4	П5	П6	С/П	П0	ИП5	+	П5	ИП0
x^2	ИП6	+	П6	КИП4	ИП6	ИП4	/	ИП5	ИП4
/	x^2	-	КвКор	БП	04
```


Instruction: В/О С/П ''number'' С/П ''number'' С/П ...


## Nim


```nim
import math, strutils

var sdSum, sdSum2, sdN = 0.0
proc sd(x: float): float =
  sdN    += 1
  sdSum  += x
  sdSum2 += x * x
  sqrt(sdSum2/sdN - sdSum*sdSum/sdN/sdN)

for value in [2,4,4,4,5,5,7,9]:
  echo value, " ", formatFloat(sd(value.float), precision = -1)
```

```txt

2 0
4 1
4 0.942809
4 0.866025
5 0.979796
5 1
7 1.39971
9 2
```



## Objeck

```objeck

use Structure;

bundle Default {
  class StdDev {
    nums : FloatVector;

    New() {
      nums := FloatVector->New();
    }

    function : Main(args : String[]) ~ Nil {
      sd := StdDev->New();
      test_data := [2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0];
      each(i : test_data) {
        sd->AddNum(test_data[i]);
        sd->GetSD()->PrintLine();
      };
    }

    method : public : AddNum(num : Float) ~ Nil {
      nums->AddBack(num);
    }

    method : public : native : GetSD() ~ Float {
      sq_diffs := 0.0;
      avg := nums->Average();
      each(i : nums) {
        num := nums->Get(i);
        sq_diffs += (num - avg) * (num - avg);
      };

      return (sq_diffs / nums->Size())->SquareRoot();
    }
  }
}

```


=={{header|Objective-C}}==

```objc
#import <Foundation/Foundation.h


@interface SDAccum : NSObject
{
  double sum, sum2;
  unsigned int num;
}
-(double)value: (double)v;
-(unsigned int)count;
-(double)mean;
-(double)variance;
-(double)stddev;
@end

@implementation SDAccum
-(double)value: (double)v
{
  sum += v;
  sum2 += v*v;
  num++;
  return [self stddev];
}
-(unsigned int)count
{
  return num;
}
-(double)mean
{
  return (num>0) ? sum/(double)num : 0.0;
}
-(double)variance
{
  double m = [self mean];
  return (num>0) ? (sum2/(double)num - m*m) : 0.0;
}
-(double)stddev
{
  return sqrt([self variance]);
}
@end

int main()
{
  @autoreleasepool {

    double v[] = { 2,4,4,4,5,5,7,9 };

    SDAccum *sdacc = [[SDAccum alloc] init];

    for(int i=0; i < sizeof(v)/sizeof(*v) ; i++)
      printf("adding %f\tstddev = %f\n", v[i], [sdacc value: v[i]]);

  }
  return 0;
}
```



### Blocks

```objc
#import <Foundation/Foundation.h


typedef double (^Func)(double); // a block that takes a double and returns a double

Func sdCreator() {
  __block int n = 0;
  __block double sum = 0;
  __block double sum2 = 0;
  return ^(double x) {
    sum += x;
    sum2 += x*x;
    n++;
    return sqrt(sum2/n - sum*sum/n/n);
  };
}

int main()
{
  @autoreleasepool {

    double v[] = { 2,4,4,4,5,5,7,9 };

    Func sdacc = sdCreator();

    for(int i=0; i < sizeof(v)/sizeof(*v) ; i++)
      printf("adding %f\tstddev = %f\n", v[i], sdacc(v[i]));

  }
  return 0;
}
```



## OCaml


```ocaml
let sqr x = x *. x

let stddev l =
  let n, sx, sx2 =
    List.fold_left
      (fun (n, sx, sx2) x -> succ n, sx +. x, sx2 +. sqr x)
      (0, 0., 0.) l
  in
  sqrt ((sx2 -. sqr sx /. float n) /. float n)

let _ =
  let l = [ 2.;4.;4.;4.;5.;5.;7.;9. ] in
  Printf.printf "List: ";
  List.iter (Printf.printf "%g  ") l;
  Printf.printf "\nStandard deviation: %g\n" (stddev l)
```


```txt

List: 2  4  4  4  5  5  7  9
Standard deviation: 2

```



## Oforth


Oforth does not have global variables that can be used to create statefull functions.

Here, we create a channel to hold current list of numbers. Constraint is that this channel can't hold mutable objects. On the other hand, stddev function is thread safe and can be called by tasks running in parallel.


```Oforth
Channel new [ ] over send drop const: StdValues

: stddev(x)
| l |
   StdValues receive x + dup ->l StdValues send drop
   #qs l map sum l size asFloat / l avg sq - sqrt ;
```


```txt

>[ 2, 4, 4, 4, 5, 5, 7, 9 ] apply(#[ stddev println ])
0
1
0.942809041582063
0.866025403784439
0.979795897113272
1
1.39970842444753
2
ok
>

```



## ooRexx

```rexx
sdacc = .SDAccum~new
x = .array~of(2,4,4,4,5,5,7,9)
sd = 0
do i = 1 to x~size
   sd = sdacc~value(x[i])
   Say '#'i 'value =' x[i] 'stdev =' sd
end

::class SDAccum
::method sum attribute
::method sum2 attribute
::method count attribute
::method init
  self~sum = 0.0
  self~sum2 = 0.0
  self~count = 0
::method value
  expose sum sum2 count
  parse arg x
  sum = sum + x
  sum2 = sum2 + x*x
  count = count + 1
  return self~stddev
::method mean
  expose sum count
  return sum/count
::method variance
  expose sum2  count
  m = self~mean
  return sum2/count - m*m
::method stddev
  return self~sqrt(self~variance)
::method sqrt
  arg n
  if n = 0 then return 0
  ans = n / 2
  prev = n
  do until prev = ans
    prev = ans
    ans = ( prev + ( n / prev ) ) / 2
  end
  return ans
```

```txt
#1 value = 2 stdev = 0
#2 value = 4 stdev = 1
#3 value = 4 stdev = 0.94280905
#4 value = 4 stdev = 0.866025405
#5 value = 5 stdev = 0.979795895
#6 value = 5 stdev = 1
#7 value = 7 stdev = 1.39970844
#8 value = 9 stdev = 2
```



## PARI/GP

Uses the Cramer-Young updating algorithm.  For demonstration it displays the mean and variance at each step.

```parigp
newpoint(x)={
  myT=x;
  myS=0;
  myN=1;
  [myT,myS]/myN
};
addpoint(x)={
  myT+=x;
  myN++;
  myS+=(myN*x-myT)^2/myN/(myN-1);
  [myT,myS]/myN
};
addpoints(v)={
  print(newpoint(v[1]));
  for(i=2,#v,print(addpoint(v[i])));
  print("Mean: ",myT/myN);
  print("Standard deviation: ",sqrt(myS/myN))
};
addpoints([2,4,4,4,5,5,7,9])
```



## Pascal


### Std.Pascal

```pascal
program stddev;
uses math;
const
  n=8;
var
  arr: array[1..n] of real =(2,4,4,4,5,5,7,9);
function stddev(n: integer): real;
var
   i: integer;
   s1,s2,variance,x: real;
begin
    for i:=1 to n do
    begin
      x:=arr[i];
      s1:=s1+power(x,2);
      s2:=s2+x
    end;
    variance:=((n*s1)-(power(s2,2)))/(power(n,2));
    stddev:=sqrt(variance)
end;
var
   i: integer;
begin
    for i:=1 to n do
    begin
      writeln(i,' item=',arr[i]:2:0,' stddev=',stddev(i):18:15)
    end
end.
```

```txt
1 item= 2 stddev= 0.000000000000000
2 item= 4 stddev= 1.000000000000000
3 item= 4 stddev= 0.942809041582064
4 item= 4 stddev= 0.866025403784439
5 item= 5 stddev= 0.979795897113271
6 item= 5 stddev= 1.000000000000000
7 item= 7 stddev= 1.399708424447530
8 item= 9 stddev= 2.000000000000000
```

=
## Delphi
=

```Delphi
program prj_CalcStdDerv;

{$APPTYPE CONSOLE}

uses
  Math;

var Series:Array of Extended;
    UserString:String;


function AppendAndCalc(NewVal:Extended):Extended;

begin
  setlength(Series,high(Series)+2);
  Series[high(Series)] := NewVal;
  result := PopnStdDev(Series);
end;

const data:array[0..7] of Extended =
  (2,4,4,4,5,5,7,9);

var rr: Extended;
begin
  setlength(Series,0);
  for rr in data do
    begin
      writeln(rr,' -> ',AppendAndCalc(rr));
    end;
  Readln;
end.
```

```txt

 2.0000000000000000E+0000 ->  0.0000000000000000E+0000
 4.0000000000000000E+0000 ->  1.0000000000000000E+0000
 4.0000000000000000E+0000 ->  9.4280904158206337E-0001
 4.0000000000000000E+0000 ->  8.6602540378443865E-0001
 5.0000000000000000E+0000 ->  9.7979589711327124E-0001
 5.0000000000000000E+0000 ->  1.0000000000000000E+0000
 7.0000000000000000E+0000 ->  1.3997084244475303E+0000
 9.0000000000000000E+0000 ->  2.0000000000000000E+0000

```



## Perl


```perl
{
    package SDAccum;
    sub new {
	my $class = shift;
	my $self = {};
	$self->{sum} = 0.0;
	$self->{sum2} = 0.0;
	$self->{num} = 0;
	bless $self, $class;
	return $self;
    }
    sub count {
	my $self = shift;
	return $self->{num};
    }
    sub mean {
	my $self = shift;
	return ($self->{num}>0) ? $self->{sum}/$self->{num} : 0.0;
    }
    sub variance {
	my $self = shift;
	my $m = $self->mean;
	return ($self->{num}>0) ? $self->{sum2}/$self->{num} - $m * $m : 0.0;
    }
    sub stddev {
	my $self = shift;
	return sqrt($self->variance);
    }
    sub value {
	my $self = shift;
	my $v = shift;
	$self->{sum} += $v;
	$self->{sum2} += $v * $v;
	$self->{num}++;
	return $self->stddev;
    }
}
```



```perl
my $sdacc = SDAccum->new;
my $sd;

foreach my $v ( 2,4,4,4,5,5,7,9 ) {
    $sd = $sdacc->value($v);
}
print "std dev = $sd\n";
```


A much shorter version using a closure and a property of the variance:


```perl
# <(x - <x>)²> = <x²> - <x>²
{
    my $num, $sum, $sum2;
    sub stddev {
	my $x = shift;
	$num++;
	return sqrt(
	    ($sum2 += $x**2) / $num -
	    (($sum += $x) / $num)**2
	);
    }
}

print stddev($_), "\n" for qw(2 4 4 4 5 5 7 9);
```


```txt
0
1
0.942809041582063
0.866025403784439
0.979795897113272
1
1.39970842444753
2
```



## Perl 6

Using a closure:

```perl6
sub sd (@a) {
    my $mean = @a R/ [+] @a;
    sqrt @a R/ [+] map (* - $mean)**2, @a;
}

sub sdaccum {
    my @a;
    return { push @a, $^x; sd @a; };
}

my &f = sdaccum;
say f $_ for 2, 4, 4, 4, 5, 5, 7, 9;
```


Using a state variable:

```perl6
# remember that <(x-<x>)²> = <x²> - <x>²
sub stddev($x) {
    sqrt
        (.[2] += $x**2) / ++.[0] -
        ((.[1] += $x) / .[0])**2
    given state @;
}

say stddev $_ for <2 4 4 4 5 5 7 9>;
```


```txt
0
1
0.942809041582063
0.866025403784439
0.979795897113271
1
1.39970842444753
2
```



## Phix

demo\rosetta\Standard_deviation.exw contains a copy of this code and a version that could be the basis for a library version that can handle multiple active data sets concurrently.

```Phix
atom sdn = 0, sdsum = 0, sdsumsq = 0

procedure sdadd(atom n)
    sdn += 1
    sdsum += n
    sdsumsq += n*n
end procedure

function sdavg()
    return sdsum/sdn
end function

function sddev()
    return sqrt(sdsumsq/sdn - power(sdsum/sdn,2))
end function

--test code:
constant testset = {2, 4, 4, 4, 5, 5, 7, 9}
integer ti
for i=1 to length(testset) do
    ti = testset[i]
    sdadd(ti)
    printf(1,"N=%d Item=%d Avg=%5.3f StdDev=%5.3f\n",{i,ti,sdavg(),sddev()})
end for
```

```txt

N=1 Item=2 Avg=2.000 StdDev=0.000
N=2 Item=4 Avg=3.000 StdDev=1.000
N=3 Item=4 Avg=3.333 StdDev=0.943
N=4 Item=4 Avg=3.500 StdDev=0.866
N=5 Item=5 Avg=3.800 StdDev=0.980
N=6 Item=5 Avg=4.000 StdDev=1.000
N=7 Item=7 Avg=4.429 StdDev=1.400
N=8 Item=9 Avg=5.000 StdDev=2.000

```



## PHP

This is just straight PHP class usage, respecting the specifications "stateful" and "one at a time":

```PHP
<?php
class sdcalc {
    private  $cnt, $sumup, $square;

    function __construct() {
       $this->reset();
    }
    # callable on an instance
    function reset() {
       $this->cnt=0; $this->sumup=0; $this->square=0;
    }
    function add($f) {
        $this->cnt++;
        $this->sumup  += $f;
        $this->square += pow($f, 2);
        return $this->calc();
    }
    function calc() {
        if ($this->cnt==0 || $this->sumup==0) {
            return 0;
        } else {
            return sqrt($this->square / $this->cnt - pow(($this->sumup / $this->cnt),2));
        }
    }
 }

# start test, adding test data one by one
$c = new sdcalc();
foreach ([2,4,4,4,5,5,7,9] as $v) {
    printf('Adding %g: result %g%s', $v, $c->add($v), PHP_EOL);
}
```


This will produce the output:


```txt
Adding 2: result 0
Adding 4: result 1
Adding 4: result 0.942809
Adding 4: result 0.866025
Adding 5: result 0.979796
Adding 5: result 1
Adding 7: result 1.39971
Adding 9: result 2

```



## PicoLisp


```PicoLisp
(scl 2)

(de stdDev ()
   (curry ((Data)) (N)
      (push 'Data N)
      (let (Len (length Data)  M (*/ (apply + Data) Len))
         (sqrt
            (*/
               (sum
                  '((N) (*/ (- N M) (- N M) 1.0))
                  Data )
               1.0
               Len )
            T ) ) ) )

(let Fun (stdDev)
   (for N (2.0 4.0 4.0 4.0 5.0 5.0 7.0 9.0)
      (prinl (format N *Scl) " -> " (format (Fun N) *Scl)) ) )
```

```txt
2.00 -> 0.00
4.00 -> 1.00
4.00 -> 0.94
4.00 -> 0.87
5.00 -> 0.98
5.00 -> 1.00
7.00 -> 1.40
9.00 -> 2.00
```



## PL/I


```pli
*process source attributes xref;
 stddev: proc options(main);
   declare a(10) float init(1,2,3,4,5,6,7,8,9,10);
   declare stdev float;
   declare i fixed binary;

   stdev=std_dev(a);
   put skip list('Standard deviation', stdev);

   std_dev: procedure(a) returns(float);
     declare a(*) float, n fixed binary;
     n=hbound(a,1);
     begin;
       declare b(n) float, average float;
       declare i fixed binary;
       do i=1 to n;
         b(i)=a(i);
       end;
       average=sum(a)/n;
       put skip data(average);
       return( sqrt(sum(b**2)/n - average**2) );
     end;
   end std_dev;

 end;
```

```txt
AVERAGE= 5.50000E+0000;
Standard deviation       2.87228E+0000
```



## PowerShell

This implementation takes the form of an advanced function
which can act like a cmdlet and receive input from the pipeline.

```powershell
function Get-StandardDeviation {
    begin {
        $avg = 0
        $nums = @()
    }
    process {
        $nums += $_
        $avg = ($nums | Measure-Object -Average).Average
        $sum = 0;
        $nums | ForEach-Object { $sum += ($avg - $_) * ($avg - $_) }
        [Math]::Sqrt($sum / $nums.Length)
    }
}
```

Usage as follows:

```txt
PS> 2,4,4,4,5,5,7,9 | Get-StandardDeviation
0
1
0.942809041582063
0.866025403784439
0.979795897113271
1
1.39970842444753
2
```



## PureBasic


```PureBasic
;Define our Standard deviation function
Declare.d Standard_deviation(x)

; Main program
If OpenConsole()
  Define i, x
  Restore MyList
  For i=1 To 8
    Read.i x
    PrintN(StrD(Standard_deviation(x)))
  Next i
  Print(#CRLF$+"Press ENTER to exit"): Input()
EndIf

;Calculation procedure, with memory
Procedure.d Standard_deviation(In)
  Static in_summa, antal
  Static in_kvadrater.q
  in_summa+in
  in_kvadrater+in*in
  antal+1
  ProcedureReturn Pow((in_kvadrater/antal)-Pow(in_summa/antal,2),0.50)
EndProcedure

;data section
DataSection
MyList:
  Data.i  2,4,4,4,5,5,7,9
EndDataSection
```


```txt

 0.0000000000
 1.0000000000
 0.9428090416
 0.8660254038
 0.9797958971
 1.0000000000
 1.3997084244
 2.0000000000

```



## Python


### Python: Using a function with attached properties

The program should work with Python 2.x and 3.x,
although the output would not be a tuple in 3.x

```python
>>
 from math import sqrt
>>> def sd(x):
    sd.sum  += x
    sd.sum2 += x*x
    sd.n    += 1.0
    sum, sum2, n = sd.sum, sd.sum2, sd.n
    return sqrt(sum2/n - sum*sum/n/n)

>>> sd.sum = sd.sum2 = sd.n = 0
>>> for value in (2,4,4,4,5,5,7,9):
    print (value, sd(value))


(2, 0.0)
(4, 1.0)
(4, 0.94280904158206258)
(4, 0.8660254037844386)
(5, 0.97979589711327075)
(5, 1.0)
(7, 1.3997084244475311)
(9, 2.0)
>>>
```



### Python: Using a class instance


```python
>>
 class SD(object): # Plain () for python 3.x
	def __init__(self):
		self.sum, self.sum2, self.n = (0,0,0)
	def sd(self, x):
		self.sum  += x
		self.sum2 += x*x
		self.n    += 1.0
		sum, sum2, n = self.sum, self.sum2, self.n
		return sqrt(sum2/n - sum*sum/n/n)

>>> sd_inst = SD()
>>> for value in (2,4,4,4,5,5,7,9):
	print (value, sd_inst.sd(value))
```



### =Python: Callable class=

You could rename the method <code>sd</code> to <code>__call__</code> this would make the class instance callable like a function so instead of using <code>sd_inst.sd(value)</code> it would change to <code>sd_inst(value)</code> for the same results.


### Python: Using a Closure

```python
>>
 from math import sqrt
>>> def sdcreator():
	sum = sum2 = n = 0
	def sd(x):
		nonlocal sum, sum2, n

		sum  += x
		sum2 += x*x
		n    += 1.0
		return sqrt(sum2/n - sum*sum/n/n)
	return sd

>>> sd = sdcreator()
>>> for value in (2,4,4,4,5,5,7,9):
	print (value, sd(value))


2 0.0
4 1.0
4 0.942809041582
4 0.866025403784
5 0.979795897113
5 1.0
7 1.39970842445
9 2.0
```



### Python: Using an extended generator

```python
>>
 from math import sqrt
>>> def sdcreator():
	sum = sum2 = n = 0
	while True:
		x = yield sqrt(sum2/n - sum*sum/n/n) if n else None

		sum  += x
		sum2 += x*x
		n    += 1.0

>>> sd = sdcreator()
>>> sd.send(None)
>>> for value in (2,4,4,4,5,5,7,9):
	print (value, sd.send(value))


2 0.0
4 1.0
4 0.942809041582
4 0.866025403784
5 0.979795897113
5 1.0
7 1.39970842445
9 2.0
```


===Python: In a couple of 'functional' lines===

```python
>>
 myMean = lambda MyList : reduce(lambda x, y: x + y, MyList) / float(len(MyList))
>>> myStd = lambda MyList : (reduce(lambda x,y : x + y , map(lambda x: (x-myMean(MyList))**2 , MyList)) / float(len(MyList)))**.5

>>> print myStd([2,4,4,4,5,5,7,9])
2.0

```



## R

===Built-in Std Dev fn===

```rsplus
#The built-in standard deviation function applies the Bessel correction.  To reverse this, we can apply an uncorrection.
#If na.rm is true, missing data points (NA values) are removed.
 reverseBesselCorrection <- function(x, na.rm=FALSE)
 {
   if(na.rm) x <- x[!is.na(x)]
   len <- length(x)
   if(len < 2) stop("2 or more data points required")
   sqrt((len-1)/len)
 }
 testdata <- c(2,4,4,4,5,5,7,9)
 reverseBesselCorrection(testdata)*sd(testdata) #2
```


### From scratch


```rsplus
#Again, if na.rm is true, missing data points (NA values) are removed.
 uncorrectedsd <- function(x, na.rm=FALSE)
 {
   len <- length(x)
   if(len < 2) stop("2 or more data points required")
   mu <- mean(x, na.rm=na.rm)
   ssq <- sum((x - mu)^2, na.rm=na.rm)
   usd <- sqrt(ssq/len)
   usd
 }
 uncorrectedsd(testdata) #2
```



## Racket



```racket

#lang racket
(require math)
(define running-stddev
  (let ([ns '()])
    (λ(n) (set! ns (cons n ns)) (stddev ns))))
;; run it on each number, return the last result
(last (map running-stddev '(2 4 4 4 5 5 7 9)))

```



## REXX

These REXX versions use   ''running sums''.

### show running sums


```rexx
/*REXX program calculates and displays the standard deviation of a given set of numbers.*/
parse arg #                                      /*obtain optional arguments from the CL*/
if #=''  then  #=2 4 4 4 5 5 7 9                 /*None specified?  Then use the default*/
n=words(#);    $=0;       $$=0;     L=length(n)  /*N:  # items; $,$$:  sums to be zeroed*/
                                                 /* [↓]  process each number in the list*/
     do j=1  for n;  _=word(#,j);   $ =$  + _
                                    $$=$$ + _**2
     say  '   item'  right(j,L)":"    right(_,4)    '  average='    left($/j,12),
          '   standard deviation='    sqrt($$/j - ($/j)**2)
     end   /*j*/                       /* [↑]  prettify output with whitespace*/
say 'standard deviation: ' sqrt($$/n - ($/n)**2) /*calculate & display the std deviation*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x; if x=0  then return 0; d=digits(); h=d+6; m.=9; numeric form
      numeric digits; parse value format(x,2,1,,0) 'E0' with g 'E' _ .;   g=g * .5'e'_ % 2
                   do j=0  while h>9;      m.j=h;               h=h%2+1;        end  /*j*/
                   do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;   end  /*k*/
      numeric digits d;                    return g/1
```

```txt

   item 1:    2    average= 2               standard deviation= 0
   item 2:    4    average= 3               standard deviation= 1
   item 3:    4    average= 3.33333333      standard deviation= 0.942809047
   item 4:    4    average= 3.5             standard deviation= 0.866025404
   item 5:    5    average= 3.8             standard deviation= 0.979795897
   item 6:    5    average= 4               standard deviation= 1
   item 7:    7    average= 4.42857143      standard deviation= 1.39970843
   item 8:    9    average= 5               standard deviation= 2
standard deviation:  2

```



### only show standard deviation


```rexx
/*REXX program calculates and displays the standard deviation of a given set of numbers.*/
parse arg #                                      /*obtain optional arguments from the CL*/
if #=''  then  #=2 4 4 4 5 5 7 9                 /*None specified?  Then use the default*/
n=words(#);                     $=0;    $$=0     /*N:  # items; $,$$:  sums to be zeroed*/
                                                 /* [↓]  process each number in the list*/
   do j=1  for n; _=word(#,j);  $ =$  + _        /*perform summation on two sets of #'s.*/
                                $$=$$ + _**2     /*perform summation on two sets of #'s.*/
   end   /*j*/
say 'standard deviation: ' sqrt($$/n - ($/n)**2) /*calculate&display the std, deviation.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x; if x=0  then return 0; d=digits(); h=d+6; m.=9; numeric form
      numeric digits; parse value format(x,2,1,,0) 'E0' with g 'E' _ .;   g=g * .5'e'_ % 2
                   do j=0  while h>9;      m.j=h;               h=h%2+1;        end  /*j*/
                   do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;   end  /*k*/
      numeric digits d;                    return g/1
```

```txt

standard deviation:  2

```



## Ring


```ring

# Project : Cumulative standard deviation

decimals(6)
sdsave = list(100)
sd = "2,4,4,4,5,5,7,9"
sumval = 0
sumsqs = 0

for num = 1 to 8
     sd = substr(sd, ",", "")
     stddata = number(sd[num])
     sumval = sumval + stddata
     sumsqs = sumsqs + pow(stddata,2)
     standdev = pow(((sumsqs / num) - pow((sumval /num),2)),0.5)
     sdsave[num] = string(num) + " " + string(sumval) +" " + string(sumsqs)
     see "" + num + " value in = " + stddata + " Stand Dev = " + standdev + nl
next

```

Output:

```txt

1 value in = 2 Stand Dev = 0
2 value in = 4 Stand Dev = 1
3 value in = 4 Stand Dev = 0.942809
4 value in = 4 Stand Dev = 0.866025
5 value in = 5 Stand Dev = 0.979796
6 value in = 5 Stand Dev = 1
7 value in = 7 Stand Dev = 1.399708
8 value in = 9 Stand Dev = 2

```



## Ruby


###  Object

Uses an object to keep state.

"Simplification of the formula [...] for standard deviation [...] can be memorized as taking the square root of (the average of the squares less the square of the average)." [[wp:Standard_deviation#Simplification_of_the_formula|c.f. wikipedia]].


```ruby
class StdDevAccumulator
  def initialize
    @n, @sum, @sumofsquares = 0, 0.0, 0.0
  end

  def <<(num)
    # return self to make this possible:  sd << 1 << 2 << 3 # => 0.816496580927726
    @n += 1
    @sum += num
    @sumofsquares += num**2
    self
  end

  def stddev
    Math.sqrt( (@sumofsquares / @n) - (@sum / @n)**2 )
  end

  def to_s
    stddev.to_s
  end
end

sd = StdDevAccumulator.new
i = 0
[2,4,4,4,5,5,7,9].each {|n| puts "adding #{n}: stddev of #{i+=1} samples is #{sd << n}" }
```



```txt
adding 2: stddev of 1 samples is 0.0
adding 4: stddev of 2 samples is 1.0
adding 4: stddev of 3 samples is 0.942809041582063
adding 4: stddev of 4 samples is 0.866025403784439
adding 5: stddev of 5 samples is 0.979795897113272
adding 5: stddev of 6 samples is 1.0
adding 7: stddev of 7 samples is 1.39970842444753
adding 9: stddev of 8 samples is 2.0
```



###  Closure


```ruby
def sdaccum
  n, sum, sum2 = 0, 0.0, 0.0
  lambda do |num|
    n += 1
    sum += num
    sum2 += num**2
    Math.sqrt( (sum2 / n) - (sum / n)**2 )
  end
end

sd = sdaccum
[2,4,4,4,5,5,7,9].each {|n| print sd.call(n), ", "}
```



```txt
0.0, 1.0, 0.942809041582063, 0.866025403784439, 0.979795897113272, 1.0, 1.39970842444753, 2.0,
```



## Run BASIC


```runbasic
dim sdSave$(100) 'can call up to 100 versions
                  'holds (space-separated) number of data , sum of values and sum of squares
sd$ = "2,4,4,4,5,5,7,9"

for num = 1 to 8
 stdData = val(word$(sd$,num,","))
  sumVal = sumVal + stdData
  sumSqs = sumSqs + stdData^2

  ' standard deviation = square root of (the average of the squares less the square of the average)
  standDev   =((sumSqs / num) - (sumVal /num) ^ 2) ^ 0.5

  sdSave$(num) = str$(num);" ";str$(sumVal);" ";str$(sumSqs)
  print num;" value in = ";stdData; " Stand Dev = "; using("###.######", standDev)

next num
```


```txt
1 value in = 2 Stand Dev =   0.000000
2 value in = 4 Stand Dev =   1.000000
3 value in = 4 Stand Dev =   0.942809
4 value in = 4 Stand Dev =   0.866025
5 value in = 5 Stand Dev =   0.979796
6 value in = 5 Stand Dev =   1.000000
7 value in = 7 Stand Dev =   1.399708
8 value in = 9 Stand Dev =   2.000000
```



## Rust

Using a struct:
```rust
pub struct CumulativeStandardDeviation {
    n: f64,
    sum: f64,
    sum_sq: f64
}

impl CumulativeStandardDeviation {
    pub fn new() -> Self {
        CumulativeStandardDeviation {
            n: 0.,
            sum: 0.,
            sum_sq: 0.
        }
    }

    fn push(&mut self, x: f64) -> f64 {
        self.n += 1.;
        self.sum += x;
        self.sum_sq += x * x;

        (self.sum_sq / self.n - self.sum * self.sum / self.n / self.n).sqrt()
    }
}

fn main() {
    let nums = [2, 4, 4, 4, 5, 5, 7, 9];

    let mut cum_stdev = CumulativeStandardDeviation::new();
    for num in nums.iter() {
        println!("{}", cum_stdev.push(*num as f64));
    }
}
```

```txt

0
1
0.9428090415820626
0.8660254037844386
0.9797958971132708
1
1.399708424447531
2

```


Using a closure:

```rust
fn sd_creator() -> impl FnMut(f64) -> f64 {
    let mut n = 0.0;
    let mut sum = 0.0;
    let mut sum_sq = 0.0;
    move |x| {
        sum += x;
        sum_sq += x*x;
        n += 1.0;
        (sum_sq / n - sum * sum / n / n).sqrt()
    }
}

fn main() {
    let nums = [2, 4, 4, 4, 5, 5, 7, 9];

    let mut sd_acc = sd_creator();
    for num in nums.iter() {
        println!("{}", sd_acc(*num as f64));
    }
}
```

```txt

0
1
0.9428090415820626
0.8660254037844386
0.9797958971132708
1
1.399708424447531
2

```



## SAS


```SAS

*--Load the test data;
data test1;
   input x @@;
   obs=_n_;
datalines;
2 4 4 4 5 5 7 9
;
run;

*--Create a dataset with the cummulative data for each set of data for which the SD should be calculated;
data test2 (drop=i obs);
   set test1;
   y=x;
   do i=1 to n;
      set test1 (rename=(obs=setid)) nobs=n point=i;
      if obs<=setid then output;
   end;
proc sort;
   by setid;
run;

*--Calulate the standards deviation (and mean) using PROC MEANS;
proc means data=test2 vardef=n noprint; *--use vardef=n option to calculate the population SD;
   by setid;
   var y;
   output out=stat1 n=n mean=mean std=sd;
run;

*--Output the calculated standard deviations;
proc print data=stat1 noobs;
   var n sd /*mean*/;
run;

```


```txt

N       SD

1    0.00000
2    1.00000
3    0.94281
4    0.86603
5    0.97980
6    1.00000
7    1.39971
8    2.00000

```



## Scala


### Generic for any numeric type

```Scala
import scala.math.sqrt

object StddevCalc extends App {

  def calcAvgAndStddev[T](ts: Iterable[T])(implicit num: Fractional[T]): (T, Double) = {
    def avg(ts: Iterable[T])(implicit num: Fractional[T]): T =
      num.div(ts.sum, num.fromInt(ts.size)) // Leaving with type of function T

    val mean: T = avg(ts) // Leave val type of T
    // Root of mean diffs
    val stdDev = sqrt(ts.map { x =>
      val diff = num.toDouble(num.minus(x, mean))
      diff * diff
    }.sum / ts.size)

    (mean, stdDev)
  }

  println(calcAvgAndStddev(List(2.0E0, 4.0, 4, 4, 5, 5, 7, 9)))
  println(calcAvgAndStddev(Set(1.0, 2, 3, 4)))
  println(calcAvgAndStddev(0.1 to 1.1 by 0.05))
  println(calcAvgAndStddev(List(BigDecimal(120), BigDecimal(1200))))

  println(s"Successfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart}ms]")

}
```



## Scheme


```scheme

(define (standart-deviation-generator)
  (let ((nums '()))
    (lambda (x)
      (set! nums (cons x nums))
      (let* ((mean (/ (apply + nums) (length nums)))
      (mean-sqr (lambda (y) (expt (- y mean) 2)))
      (variance (/ (apply + (map mean-sqr nums)) (length nums))))
    (sqrt variance)))))

(let loop ((f (standart-deviation-generator))
           (input '(2 4 4 4 5 5 7 9)))
  (if (not (null? input))
    (begin
      (display (f (car input)))
      (newline)
      (loop f (cdr input)))))

```



## Scilab

Scilab has the built-in function '''stdev''' to compute the standard deviation of a sample so it is straightforward to have the standard deviation of a sample with a correction of the bias.
<lang>T=[2,4,4,4,5,5,7,9];
stdev(T)*sqrt((length(T)-1)/length(T))
```

```txt
-->T=[2,4,4,4,5,5,7,9];
-->stdev(T)*sqrt((length(T)-1)/length(T))
   ans  =     2.
```



## Sidef

Using an object to keep state:

```ruby
class StdDevAccumulator(n=0, sum=0, sumofsquares=0) {
  method <<(num) {
    n += 1
    sum += num
    sumofsquares += num**2
    self
  }
 
  method stddev {
    sqrt(sumofsquares/n - pow(sum/n, 2))
  }
 
  method to_s {
    self.stddev.to_s
  }
}
 
var i = 0
var sd = StdDevAccumulator()
[2,4,4,4,5,5,7,9].each {|n|
    say "adding #{n}: stddev of #{i+=1} samples is #{sd << n}"
}
```

```txt

adding 2: stddev of 1 samples is 0
adding 4: stddev of 2 samples is 1
adding 4: stddev of 3 samples is 0.942809041582063365867792482806465385713114583585
adding 4: stddev of 4 samples is 0.866025403784438646763723170752936183471402626905
adding 5: stddev of 5 samples is 0.979795897113271239278913629882356556786378992263
adding 5: stddev of 6 samples is 1
adding 7: stddev of 7 samples is 1.39970842444753034182701947126050936683768427466
adding 9: stddev of 8 samples is 2

```


Using ''static'' variables:

```ruby
func stddev(x) {
    static(num=0, sum=0, sum2=0)
    num++
    sqrt(
        (sum2 += x**2) / num -
        (((sum += x) / num)**2)
    )
}
 
%n(2 4 4 4 5 5 7 9).each { say stddev(_) }
```

```txt

0
1
0.942809041582063365867792482806465385713114583585
0.866025403784438646763723170752936183471402626905
0.979795897113271239278913629882356556786378992263
1
1.39970842444753034182701947126050936683768427466
2

```



## Smalltalk

```smalltalk
Object subclass: SDAccum [
    |sum sum2 num|
    SDAccum class >> new [  |o|
        o := super basicNew.
        ^ o init.
    ]
    init [ sum := 0. sum2 := 0. num := 0 ]
    value: aValue [
      sum := sum + aValue.
      sum2 := sum2 + ( aValue * aValue ).
      num := num + 1.
      ^ self stddev
    ]
    count [ ^ num ]
    mean [ num>0 ifTrue: [^ sum / num] ifFalse: [ ^ 0.0 ] ]
    variance [ |m| m := self mean.
               num>0 ifTrue: [^ (sum2/num) - (m*m) ] ifFalse: [ ^ 0.0 ]
             ]
    stddev [ ^ (self variance) sqrt ]
].
```



```smalltalk
|sdacc sd|
sdacc := SDAccum new.

#( 2 4 4 4 5 5 7 9 ) do: [ :v | sd := sdacc value: v ].
('std dev = %1' % { sd }) displayNl.
```



## SQL

```SQL
-- the minimal table
create table if not exists teststd (n double precision not null);

-- code modularity with view, we could have used a common table expression instead
create view  vteststd as
  select count(n) as cnt,
  sum(n) as tsum,
  sum(power(n,2)) as tsqr
from teststd;

-- you can of course put this code into every query
create or replace function std_dev() returns double precision as $$
 select sqrt(tsqr/cnt - (tsum/cnt)^2) from vteststd;
$$ language sql;

-- test data is: 2,4,4,4,5,5,7,9
insert into teststd values (2);
select std_dev() as std_deviation;
insert into teststd values (4);
select std_dev() as std_deviation;
insert into teststd values (4);
select std_dev() as std_deviation;
insert into teststd values (4);
select std_dev() as std_deviation;
insert into teststd values (5);
select std_dev() as std_deviation;
insert into teststd values (5);
select std_dev() as std_deviation;
insert into teststd values (7);
select std_dev() as std_deviation;
insert into teststd values (9);
select std_dev() as std_deviation;
-- cleanup test data
delete from teststd;

```

With a command like '''psql <rosetta-std-dev.sql''' you will get an output like this: (duplicate lines generously deleted, locale is DE)

```txt

CREATE TABLE
FEHLER:  Relation »vteststd« existiert bereits
CREATE FUNCTION
INSERT 0 1
 std_deviation
---------------
             0
(1 Zeile)

INSERT 0 1
 std_deviation
---------------
             1
 0.942809041582063
 0.866025403784439
 0.979795897113272
             1
 1.39970842444753
             2
DELETE 8

```



## Swift


```Swift
import Darwin
class stdDev{

    var n:Double = 0.0
    var sum:Double = 0.0
    var sum2:Double = 0.0

    init(){

        let testData:[Double] = [2,4,4,4,5,5,7,9];
        for x in testData{

            var a:Double = calcSd(x)
            println("value \(Int(x)) SD = \(a)");
        }

    }

    func calcSd(x:Double)->Double{

        n += 1
        sum += x
        sum2 += x*x
        return sqrt( sum2 / n - sum*sum / n / n)
    }

}
var aa = stdDev()
```

```txt

value 2 SD = 0.0
value 4 SD = 1.0
value 4 SD = 0.942809041582063
value 4 SD = 0.866025403784439
value 5 SD = 0.979795897113271
value 5 SD = 1.0
value 7 SD = 1.39970842444753
value 9 SD = 2.0

```


Functional:


```Swift

func standardDeviation(arr : [Double]) -> Double
{
    let length = Double(arr.count)
    let avg = arr.reduce(0, { $0 + $1 }) / length
    let sumOfSquaredAvgDiff = arr.map { pow($0 - avg, 2.0)}.reduce(0, {$0 + $1})
    return sqrt(sumOfSquaredAvgDiff / length)
}

let responseTimes = [ 18.0, 21.0, 41.0, 42.0, 48.0, 50.0, 55.0, 90.0 ]

standardDeviation(responseTimes) // 20.8742514835862
standardDeviation([2,4,4,4,5,5,7,9]) // 2.0

```



## Tcl


### With a Class

```tcl
oo::class create SDAccum {
    variable sum sum2 num
    constructor {} {
        set sum 0.0
        set sum2 0.0
        set num 0
    }
    method value {x} {
        set sum2 [expr {$sum2 + $x**2}]
        set sum [expr {$sum + $x}]
        incr num
        return [my stddev]
    }
    method count {} {
        return $num
    }
    method mean {} {
        expr {$sum / $num}
    }
    method variance {} {
        expr {$sum2/$num - [my mean]**2}
    }
    method stddev {} {
        expr {sqrt([my variance])}
    }
}

# Demonstration
set sdacc [SDAccum new]
foreach val {2 4 4 4 5 5 7 9} {
    set sd [$sdacc value $val]
}
puts "the standard deviation is: $sd"
```

```txt
the standard deviation is: 2.0
```



### With a Coroutine

```tcl
# Make a coroutine out of a lambda application
coroutine sd apply {{} {
    set sum 0.0
    set sum2 0.0
    set sd {}
    # Keep processing argument values until told not to...
    while {[set val [yield $sd]] ne "stop"} {
        incr n
        set sum [expr {$sum + $val}]
        set sum2 [expr {$sum2 + $val**2}]
        set sd [expr {sqrt($sum2/$n - ($sum/$n)**2)}]
    }
}}

# Demonstration
foreach val {2 4 4 4 5 5 7 9} {
    set sd [sd $val]
}
sd stop
puts "the standard deviation is: $sd"
```


=={{header|TI-83 BASIC}}==
On the TI-83 family, standard deviation of a population is
a builtin function (σx):
 • Press [STAT] select [EDIT] followed by [ENTER]
 • then enter for list L1 in the table : 2, 4, 4, 4, 5, 5, 7, 9
 • Or enter {2,4,4,4,5,5,7,9}→L1
 • Press [STAT] select [CALC] then [1-Var Stats] select list L1 followed by [ENTER]
 • Then σx (=2) gives the standard deviation of the population


## VBScript


```vb
data = Array(2,4,4,4,5,5,7,9)

For i = 0 To UBound(data)
	WScript.StdOut.Write "value = " & data(i) &_
		" running sd = " & sd(data,i)
	WScript.StdOut.WriteLine
Next

Function sd(arr,n)
	mean = 0
	variance = 0
	For j = 0 To n
		mean = mean + arr(j)
	Next
	mean = mean/(n+1)
	For k = 0 To n
		variance = variance + ((arr(k)-mean)^2)
	Next
	variance = variance/(n+1)
	sd = FormatNumber(Sqr(variance),6)
End Function
```


```txt

value = 2 running sd = 0.000000
value = 4 running sd = 1.000000
value = 4 running sd = 0.942809
value = 4 running sd = 0.866025
value = 5 running sd = 0.979796
value = 5 running sd = 1.000000
value = 7 running sd = 1.399708
value = 9 running sd = 2.000000

```



## Visual Basic


Note that the helper function <code>avg</code> is not named <code>average</code> to avoid a name conflict with <code>WorksheetFunction.Average</code> in MS Excel.


```vb
Function avg(what() As Variant) As Variant
    'treats non-numeric strings as zero
    Dim L0 As Variant, total As Variant
    For L0 = LBound(what) To UBound(what)
        If IsNumeric(what(L0)) Then total = total + what(L0)
    Next
    avg = total / (1 + UBound(what) - LBound(what))
End Function

Function standardDeviation(fp As Variant) As Variant
    Static list() As Variant
    Dim av As Variant, tmp As Variant, L0 As Variant

    'add to sequence if numeric
    If IsNumeric(fp) Then
        On Error GoTo makeArr   'catch undimensioned list
        ReDim Preserve list(UBound(list) + 1)
        On Error GoTo 0
        list(UBound(list)) = fp
    End If

    'get average
    av = avg(list())

    'the actual work
    For L0 = 0 To UBound(list)
        tmp = tmp + ((list(L0) - av) ^ 2)
    Next
    tmp = Sqr(tmp / (UBound(list) + 1))

    standardDeviation = tmp

    Exit Function
makeArr:
    If 9 = Err.Number Then
        ReDim list(0)
    Else
        'something's wrong
        Err.Raise Err.Number
    End If
    Resume Next
End Function

Sub tester()
    Dim x As Variant
    x = Array(2, 4, 4, 4, 5, 5, 7, 9)
    For L0 = 0 To UBound(x)
        Debug.Print standardDeviation(x(L0))
    Next
End Sub
```


```txt

 0
 1
 0.942809041582063
 0.866025403784439
 0.979795897113271
 1
 1.39970842444753
 2

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
int  A, I;
real N, S, S2;
[A:= [2,4,4,4,5,5,7,9];
N:= 0.0;  S:= 0.0;  S2:= 0.0;
for I:= 0 to 8-1 do
        [N:= N + 1.0;
        S:= S + float(A(I));
        S2:= S2 + float(sq(A(I)));
        RlOut(0, sqrt((S2/N) - sq(S/N)));
        ];
CrLf(0);
]
```


```txt

    0.00000    1.00000    0.94281    0.86603    0.97980    1.00000    1.39971    2.00000

```



## zkl


```zkl
fcn sdf{ fcn(x,xs){
      m:=xs.append(x.toFloat()).sum(0.0)/xs.len();
      (xs.reduce('wrap(p,x){(x-m)*(x-m) +p},0.0)/xs.len()).sqrt()
    }.fp1(L())
}
```

```txt

zkl: T(2,4,4,4,5,5,7,9).pump(Void,sdf())
2

zkl: sd:=sdf(); sd(2);sd(4);sd(4);sd(4);sd(5);sd(5);sd(7);sd(9)
2

```

