+++
title = "Monte Carlo methods"
description = ""
date = 2019-06-05T08:24:32Z
aliases = []
[extra]
id = 3051
[taxonomies]
categories = []
tags = []
+++

{{task|Probability and statistics}}

A '''Monte Carlo Simulation''' is a way of approximating the value of a function 
where calculating the actual value is difficult or impossible. 

It uses random sampling to define constraints on the value 
and then makes a sort of "best guess."

A simple Monte Carlo Simulation can be used to calculate the value for <big><math>\pi</math></big>. 

If you had a circle and a square where the length of a side of the square 
was the same as the diameter of the circle, the ratio of the area of the circle 
to the area of the square would be <big><math>\pi/4</math></big>.
 
So, if you put this circle inside the square and select many random points 
inside the square, the number of points inside the circle 
divided by the number of points inside the square and the circle 
would be approximately <big><math>\pi/4</math></big>.


;Task:
Write a function to run a simulation like this, with a variable number of random points to select.

Also, show the results of a few different sample sizes.

For software where the number <big><math>\pi</math></big> is not built-in, 
we give <big><math>\pi</math></big> as a number of digits: 
             3.141592653589793238462643383280





## 360 Assembly


```360asm
*        Monte Carlo methods       08/03/2017
MONTECAR CSECT
         USING  MONTECAR,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R8,1000            isamples=1000
         LA     R6,4               i=4
       DO WHILE=(C,R6,LE,=F'7')    do i=4 to 7
         MH     R8,=H'10'            isamples=isamples*10
         ZAP    HITS,=P'0'           hits=0
         LA     R7,1                 j=1
       DO WHILE=(CR,R7,LE,R8)        do j=1 to isamples
         BAL    R14,RNDPK              call random
         ZAP    X,RND                  x=rnd
         BAL    R14,RNDPK              call random
         ZAP    Y,RND                  y=rnd
         ZAP    WP,X                   x
         MP     WP,X                   x**2
         DP     WP,ONE                 ~
         ZAP    XX,WP(8)               x**2   normalized
         ZAP    WP,Y                   y
         MP     WP,Y                   y**2
         DP     WP,ONE                 ~
         ZAP    YY,WP(8)               y**2   normalized
         AP     XX,YY                  xx=x**2+y**2
       IF CP,XX,LT,ONE THEN            if x**2+y**2<1 then
         AP     HITS,=P'1'               hits=hits+1
       ENDIF    ,                      endif
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         CVD    R8,PSAMPLES          psamples=isamples
         ZAP    WP,=P'4'             4
         MP     WP,ONE               ~
         MP     WP,HITS              *hits
         DP     WP,PSAMPLES          /psamples
         ZAP    MCPI,WP(8)           mcpi=4*hits/psamples
         XDECO  R6,WC                edit i
         MVC    PG+4(1),WC+11        output i
         MVC    WC,MASK              load mask
         ED     WC,PSAMPLES          edit psamples
         MVC    PG+6(8),WC+8         output psamples
         UNPK   WC,MCPI              unpack mcpi
         OI     WC+15,X'F0'          zap sign
         MVC    PG+31(1),WC+6        output mcpi
         MVC    PG+33(6),WC+7        output mcpi decimals
         XPRNT  PG,L'PG              print buffer
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
RNDPK    EQU    *             ---- random number generator
         ZAP    WP,RNDSEED         w=seed    
         MP     WP,RNDCNSTA        w*=cnsta
         AP     WP,RNDCNSTB        w+=cnstb
         MVC    RNDSEED,WP+8       seed=w mod 10**15
         MVC    RND,=PL8'0'        0<=rnd<1
         MVC    RND+3(5),RNDSEED+3 return rnd
         BR     R14           ---- return
PSAMPLES DS     0D,PL8             F(15,0)
RNDSEED  DC     PL8'613058151221121'  linear congruential constant
RNDCNSTA DC     PL8'944021285986747'  "
RNDCNSTB DC     PL8'852529586767995'  "
RND      DS     PL8                fixed(15,9)
ONE      DC     PL8'1.000000000'   1 fixed(15,9)
HITS     DS     PL8                fixed(15,0)
X        DS     PL8                fixed(15,9)
Y        DS     PL8                fixed(15,9)
MCPI     DS     PL8                fixed(15,9)
XX       DS     PL8                fixed(15,9)
YY       DS     PL8                fixed(15,9)
PG       DC     CL80'10**x xxxxxxxx samples give Pi=x.xxxxxx'  buffer
MASK     DC     X'40202020202020202020202020202120'  mask CL16 15num
WC       DS     PL16               character 16
WP       DS     PL16               packed decimal 16
         YREGS
         END    MONTECAR
```

{{out}}

```txt

10**4    10000 samples give Pi=3.129200
10**5   100000 samples give Pi=3.145000
10**6  1000000 samples give Pi=3.141180
10**7 10000000 samples give Pi=3.141677

```





## Ada


```ada
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;

procedure Test_Monte_Carlo is
   Dice : Generator;
   
   function Pi (Throws : Positive) return Float is
      Inside : Natural := 0;
   begin
      for Throw in 1..Throws loop
         if Random (Dice) ** 2 + Random (Dice) ** 2 <= 1.0 then
            Inside := Inside + 1;
         end if;
      end loop;
      return 4.0 * Float (Inside) / Float (Throws);
   end Pi;
begin
   Put_Line ("     10_000:" & Float'Image (Pi (     10_000)));
   Put_Line ("    100_000:" & Float'Image (Pi (    100_000)));
   Put_Line ("  1_000_000:" & Float'Image (Pi (  1_000_000)));
   Put_Line (" 10_000_000:" & Float'Image (Pi ( 10_000_000)));
   Put_Line ("100_000_000:" & Float'Image (Pi (100_000_000)));
end Test_Monte_Carlo;
```

The implementation uses built-in uniformly distributed on [0,1] random numbers. 
Note that the accuracy of the result depends on the quality of the pseudo random generator: its circle length and correlation to the function being simulated. 
{{out}}

```txt

     10_000: 3.13920E+00
    100_000: 3.14684E+00
  1_000_000: 3.14197E+00
 10_000_000: 3.14215E+00
100_000_000: 3.14151E+00

```



## ALGOL 68

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}} 

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}} 

```algol68
PROC pi = (INT throws)REAL:
BEGIN
   INT inside := 0;
   TO throws DO
      IF random ** 2 + random ** 2 <= 1 THEN
         inside +:= 1
      FI
   OD;
   4 * inside / throws
END # pi #;

print (("     10 000:",pi (     10 000),new line));
print (("    100 000:",pi (    100 000),new line));
print (("  1 000 000:",pi (  1 000 000),new line));
print ((" 10 000 000:",pi ( 10 000 000),new line));
print (("100 000 000:",pi (100 000 000),new line))
```

{{out}}

```txt

     10 000:+3.15480000000000e  +0
    100 000:+3.12948000000000e  +0
  1 000 000:+3.14169200000000e  +0
 10 000 000:+3.14142040000000e  +0
100 000 000:+3.14153276000000e  +0

```


=={{Header|AutoHotkey}}==
{{AutoHotkey case}}
Source: [http://www.autohotkey.com/forum/topic44657.html AutoHotkey forum] by Laszlo

```autohotkey

MsgBox % MontePi(10000)   ; 3.154400 
MsgBox % MontePi(100000)  ; 3.142040 
MsgBox % MontePi(1000000) ; 3.142096 

MontePi(n) { 
   Loop %n% { 
      Random x, -1, 1.0 
      Random y, -1, 1.0 
      p += x*x+y*y < 1 
   } 
   Return 4*p/n 
}

```



## AWK


```AWK

# --- with command line argument "throws" ---

BEGIN{ th=ARGV[1];
 for(i=0; i<th; i++) cin += (rand()^2 + rand()^2) < 1 
 printf("Pi = %8.5f\n",4*cin/th)
}

usage: awk -f pi 2300

Pi =  3.14333


```



## BASIC

{{works with|QuickBasic|4.5}}
{{trans|Java}}

```qbasic
DECLARE FUNCTION getPi! (throws!)
CLS
PRINT getPi(10000)
PRINT getPi(100000)
PRINT getPi(1000000)
PRINT getPi(10000000)

FUNCTION getPi (throws)
	inCircle = 0
		FOR i = 1 TO throws
			'a square with a side of length 2 centered at 0 has
			'x and y range of -1 to 1
			randX = (RND * 2) - 1'range -1 to 1
			randY = (RND * 2) - 1'range -1 to 1
			'distance from (0,0) = sqrt((x-0)^2+(y-0)^2)
			dist = SQR(randX ^ 2 + randY ^ 2)
			IF dist < 1 THEN 'circle with diameter of 2 has radius of 1
				inCircle = inCircle + 1
			END IF
		NEXT i
	getPi = 4! * inCircle / throws
END FUNCTION
```

{{out}}

```txt

3.16
3.13648
3.142828
3.141679

```



## BBC BASIC


```bbcbasic
      PRINT FNmontecarlo(1000)
      PRINT FNmontecarlo(10000)
      PRINT FNmontecarlo(100000)
      PRINT FNmontecarlo(1000000)
      PRINT FNmontecarlo(10000000)
      END
      
      DEF FNmontecarlo(t%)
      LOCAL i%, n%
      FOR i% = 1 TO t%
        IF RND(1)^2 + RND(1)^2 < 1 n% += 1
      NEXT
      = 4 * n% / t%
```

{{out}}

```txt

     3.136
    3.1396
   3.13756
  3.143624
 3.1412816

```



## C


```C>#include <stdio.h

#include <stdlib.h>
#include <math.h>
 
double pi(double tolerance)
{
	double x, y, val, error;
	unsigned long sampled = 0, hit = 0, i;

	do {
		/* don't check error every turn, make loop tight */
		for (i = 1000000; i; i--, sampled++) {
			x = rand() / (RAND_MAX + 1.0);
			y = rand() / (RAND_MAX + 1.0);
			if (x * x + y * y < 1) hit ++;
		}

		val = (double) hit / sampled;
		error = sqrt(val * (1 - val) / sampled) * 4;
		val *= 4;

		/* some feedback, or user gets bored */
		fprintf(stderr, "Pi = %f +/- %5.3e at %ldM samples.\r",
			val, error, sampled/1000000);
	} while (!hit || error > tolerance);
              /* !hit is for completeness's sake; if no hit after 1M samples,
                 your rand() is BROKEN */

	return val;
}

int main()
{
	printf("Pi is %f\n", pi(3e-4)); /* set to 1e-4 for some fun */
	return 0;
}
```



## C++


```cpp

#include<iostream>
#include<math.h>
#include<stdlib.h>
#include<time.h>
 
using namespace std;
int main(){
    int jmax=1000; // maximum value of HIT number. (Length of output file)
    int imax=1000; // maximum value of random numbers for producing HITs.
    double x,y;    // Coordinates
    int hit;       // storage variable of number of HITs
    srand(time(0));
    for (int j=0;j<jmax;j++){
        hit=0;
        x=0; y=0;
        for(int i=0;i<imax;i++){
            x=double(rand())/double(RAND_MAX);
            y=double(rand())/double(RAND_MAX);
        if(y<=sqrt(1-pow(x,2))) hit+=1; }          //Choosing HITs according to analytic formula of circle
    cout<<""<<4*double(hit)/double(imax)<<endl; }  // Print out Pi number
}

```


=={{header|C sharp|C#}}==

```csharp
using System;

class Program {
    static double MonteCarloPi(int n) {
        int inside = 0;
        Random r = new Random();

        for (int i = 0; i < n; i++) {
            if (Math.Pow(r.NextDouble(), 2)+ Math.Pow(r.NextDouble(), 2) <= 1) {
                inside++;
            }
        }

        return 4.0 * inside / n;
    }

    static void Main(string[] args) {
        int value = 1000;
        for (int n = 0; n < 5; n++) {
            value *= 10;
            Console.WriteLine("{0}:{1}", value.ToString("#,###").PadLeft(11, ' '), MonteCarloPi(value));
        }
    }
}
```


{{out}}

```txt

     10,000:3.1436
    100,000:3.14632
  1,000,000:3.139476
 10,000,000:3.1424476
100,000,000:3.1413976

```



## Clojure


```lisp
(defn calc-pi [iterations]
  (loop [x (rand) y (rand) in 0 total 1] 
    (if (< total iterations)
      (recur (rand) (rand) (if (<= (+ (* x x) (* y y)) 1) (inc in) in) (inc total))
      (double (* (/ in total) 4)))))

(doseq [x (take 5 (iterate #(* 10 %) 10))] (println (str (format "% 8d" x) ": " (calc-pi x))))
```


{{out}}

```txt

     100: 3.2
    1000: 3.124
   10000: 3.1376
  100000: 3.14104
 1000000: 3.141064

```



```lisp
(defn experiment 
  [] 
  (if (<= (+ (Math/pow (rand) 2) (Math/pow (rand) 2)) 1) 1 0))

(defn pi-estimate 
  [n] 
  (* 4 (float (/ (reduce + (take n (repeatedly experiment))) n))))

(pi-estimate 10000)

```


{{out}}

```txt

     3.1347999572753906

```



## Common Lisp


```lisp
(defun approximate-pi (n)
  (/ (loop repeat n count (<= (abs (complex (random 1.0) (random 1.0))) 1.0)) n 0.25))

(dolist (n (loop repeat 5 for n = 1000 then (* n 10) collect n))
  (format t "~%~8d -> ~f" n (approximate-pi n)))
```


{{out}}

```txt

    1000 -> 3.132
   10000 -> 3.1184
  100000 -> 3.1352
 1000000 -> 3.142072
10000000 -> 3.1420677

```



## D


```d
import std.stdio, std.random, std.math;

double pi(in uint nthrows) /*nothrow*/ @safe /*@nogc*/ {
    uint inside;
    foreach (immutable i; 0 .. nthrows)
        if (hypot(uniform01, uniform01) <= 1)
            inside++;
    return 4.0 * inside / nthrows;
}

void main() {
    foreach (immutable p; 1 .. 8)
        writefln("%10s: %07f", 10 ^^ p, pi(10 ^^ p));
}
```

{{out}}

```txt
        10: 3.200000
       100: 3.120000
      1000: 3.076000
     10000: 3.140400
    100000: 3.146520
   1000000: 3.140192
  10000000: 3.141476
```

{{out}} with foreach(p;1..10):

```txt
        10: 3.200000
       100: 3.240000
      1000: 3.180000
     10000: 3.150400
    100000: 3.143080
   1000000: 3.140996
  10000000: 3.141442
 100000000: 3.141439
1000000000: 3.141559
```



### More Functional Style


```d
void main() {
    import std.stdio, std.random, std.math, std.algorithm, std.range;

    immutable isIn = (int) => hypot(uniform01, uniform01) <= 1;
    immutable pi = (in int n)  => 4.0 * n.iota.count!isIn / n;

    foreach (immutable p; 1 .. 8)
        writefln("%10s: %07f", 10 ^^ p, pi(10 ^^ p));
}
```

{{out}}

```txt
        10: 3.200000
       100: 3.320000
      1000: 3.128000
     10000: 3.140800
    100000: 3.128400
   1000000: 3.142836
  10000000: 3.141550
```



## Dart


From example at [https://www.dartlang.org/ Dart Official Website]


```dart

import 'dart:async';
import 'dart:html';
import 'dart:math' show Random;

// We changed 5 lines of code to make this sample nicer on
// the web (so that the execution waits for animation frame, 
// the number gets updated in the DOM, and the program ends 
// after 500 iterations).

main() async {
  print('Compute π using the Monte Carlo method.');
  var output = querySelector("#output");
  await for (var estimate in computePi().take(500)) {
    print('π ≅ $estimate');
    output.text = estimate.toStringAsFixed(5);
    await window.animationFrame;
  }
}

/// Generates a stream of increasingly accurate estimates of π.
Stream<double> computePi({int batch: 100000}) async* {
  var total = 0;
  var count = 0;
  while (true) {
    var points = generateRandom().take(batch);
    var inside = points.where((p) => p.isInsideUnitCircle);
    total += batch;
    count += inside.length;
    var ratio = count / total;
    // Area of a circle is A = π⋅r², therefore π = A/r².
    // So, when given random points with x ∈ <0,1>,
    // y ∈ <0,1>, the ratio of those inside a unit circle
    // should approach π / 4. Therefore, the value of π
    // should be:
    yield ratio * 4;
  }
}

Iterable<Point> generateRandom([int seed]) sync* {
  final random = new Random(seed);
  while (true) {
    yield new Point(random.nextDouble(), random.nextDouble());
  }
}

class Point {
  final double x, y;
  const Point(this.x, this.y);
  bool get isInsideUnitCircle => x * x + y * y <= 1;
}

```

{{out}}
The script give in reality an output formatted in HTML

```txt
π ≅ 3.14139
```



## E


This computes a single quadrant of the described square and circle; the effect should be the same since the other three are symmetric.


```e
def pi(n) {
    var inside := 0
    for _ ? (entropy.nextFloat() ** 2 + entropy.nextFloat() ** 2 < 1) in 1..n {
         inside += 1
    }
    return inside * 4 / n
}
```


Some sample runs:

 ? pi(10)
 # value: 2.8
 
 ? pi(10)
 # value: 2.0
 
 ? pi(100) 
 # value: 2.96
 
 ? pi(10000)
 # value: 3.1216
 
 ? pi(100000)
 # value: 3.13088 

 ? pi(100000)
 # value: 3.13848


## EasyLang


```easyprog.online
func mc n . .
  for i range n
    x# = randomf
    y# = randomf
    if x# * x# + y# * y# < 1
      hit += 1
    .
  .
  print 4.0 * hit / n
.
call mc 1000
call mc 10000
call mc 100000
call mc 1000000
```

Output:
 3.224
 3.161
 3.135
 3.141


## Elixir


```elixir
defmodule MonteCarlo do
  def pi(n) do
    count = Enum.count(1..n, fn _ ->
      x = :rand.uniform
      y = :rand.uniform
      :math.sqrt(x*x + y*y) <= 1
    end)
    4 * count / n
  end
end

Enum.each([1000, 10000, 100000, 1000000, 10000000], fn n ->
  :io.format "~8w samples: PI = ~f~n", [n, MonteCarlo.pi(n)]
end)
```


{{out}}

```txt

    1000 samples: PI = 3.112000
   10000 samples: PI = 3.127200
  100000 samples: PI = 3.145440
 1000000 samples: PI = 3.142904
10000000 samples: PI = 3.141124

```



## Erlang


### With inline test


```ERLANG

-module(monte).
-export([main/1]).

monte(N)->
    monte(N,0,0).

monte(0,InCircle,NumPoints) ->
    4 * InCircle / NumPoints;

monte(N,InCircle,NumPoints)->
    Xcoord = rand:uniform(),
    Ycoord = rand:uniform(),
    monte(N-1,
          if Xcoord*Xcoord + Ycoord*Ycoord < 1 -> InCircle + 1; true -> InCircle end, 
          NumPoints + 1).

main(N) -> io:format("PI: ~w~n", [ monte(N) ]).

```

{{out}}

```txt

8> [monte:main(X) || X <- [10000,100000,100000,10000000] ].     
PI: 3.136
PI: 3.1464
PI: 3.1412
PI: 3.1416704
[ok,ok,ok,ok]


```


### With test in a function


```ERLANG

-module(monte2).
-export([main/1]).

monte(N)->
    monte(N,0,0).

monte(0,InCircle,NumPoints) ->
    4 * InCircle / NumPoints;

monte(N,InCircle,NumPoints)->
    X = rand:uniform(),
    Y = rand:uniform(),
    monte(N-1, within(X,Y,InCircle), NumPoints + 1).

within(X,Y,IN)->
          if X*X + Y*Y < 1 -> IN + 1;
          true -> IN
          end.

main(N) -> io:format("PI: ~w~n", [ monte(N) ]).

```

{{out}}

```txt
Xcoord
6> [monte2:main(X) || X <- [10000000,1000000,100000,10000] ].
PI: 3.1424172
PI: 3.140544
PI: 3.14296
PI: 3.1252
[ok,ok,ok,ok]



```



## ERRE


```ERRE

PROGRAM RANDOM_PI

!
! for rosettacode.org
!

!$DOUBLE

PROCEDURE MONTECARLO(T->RES)
      LOCAL I,N
      FOR I=1 TO T DO
        IF RND(1)^2+RND(1)^2<1 THEN N+=1 END IF
      END FOR
      RES=4*N/T
END PROCEDURE

BEGIN
      RANDOMIZE(TIMER) ! init rnd number generator
      MONTECARLO(1000->RES)     PRINT(RES)
      MONTECARLO(10000->RES)    PRINT(RES)
      MONTECARLO(100000->RES)   PRINT(RES)
      MONTECARLO(1000000->RES)  PRINT(RES)
      MONTECARLO(10000000->RES) PRINT(RES)
END PROGRAM
```

{{out}}

```txt

 3.136
 3.1468
 3.14392
 3.143824
 3.141514

```



## Euler Math Toolbox


```Euler Math Toolbox

>function map MonteCarloPI (n,plot=false) ...
$  X:=random(1,n);
$  Y:=random(1,n);
$  if plot then
$      plot2d(X,Y,>points,style="."); 
$      plot2d("sqrt(1-x^2)",color=2,>add); 
$  endif
$  return sum(X^2+Y^2<1)/n*4;
$endfunction
>MonteCarloPI(10^(1:7))
 [ 3.6  2.96  3.224  3.1404  3.1398  3.141548  3.1421492 ]
>pi
 3.14159265359
>MonteCarloPI(10000,true):

```


[[File:Test.png]]


## F Sharp

There is some support and test expressions.


```fsharp

let print x = printfn "%A" x

let MonteCarloPiGreco niter =
    let eng = System.Random()
    let action () =
        let x: float = eng.NextDouble()
        let y: float = eng.NextDouble()
        let res: float = System.Math.Sqrt(x**2.0 + y**2.0)
        if res < 1.0 then
            1
        else
            0
    let res = [ for x in 1..niter do yield action() ]
    let tmp: float = float(List.reduce (+) res) / float(res.Length)
    4.0*tmp

MonteCarloPiGreco 1000 |> print
MonteCarloPiGreco 10000 |> print
MonteCarloPiGreco 100000 |> print

```

{{out}}

```txt

3.164
3.122
3.1436

```



## Factor

Since Factor lets the user choose the range of the random generator, we use 2^32.


```factor
USING: kernel math math.functions random sequences ;

: limit ( -- n ) 2 32 ^ ; inline
: in-circle ( x y -- ? ) limit [ sq ] tri@ [ + ] [ <= ] bi* ;
: rand ( -- r ) limit random ;
: pi ( n -- pi ) [ [ drop rand rand in-circle ] count ] keep / 4 * >float ;
```


Example use:


```factor
10000 pi .
3.1412
```



## Fantom



```fantom

class MontyCarlo
{
  // assume square/circle of width 1 unit
  static Float findPi (Int samples)
  {
    Int insideCircle := 0
    samples.times 
    {
      x := Float.random
      y := Float.random
      if ((x*x + y*y).sqrt <= 1.0f) insideCircle += 1
    }
    return insideCircle * 4.0f / samples
  }

  public static Void main () 
  {
    [100, 1000, 10000, 1000000, 10000000].each |sample|
    {
      echo ("Sample size $sample gives PI as ${findPi(sample)}")
    }
  }
}

```


{{out}}

```txt

Sample size 100 gives PI as 3.2
Sample size 1000 gives PI as 3.132
Sample size 10000 gives PI as 3.1612
Sample size 1000000 gives PI as 3.139316
Sample size 10000000 gives PI as 3.1409272

```



## Forth

{{works with|GNU Forth}}
 include random.fs
 
 10000 value r
 
 : hit? ( -- ? )
   r random dup *
   r random dup * +
   r dup * < ;
 
 : sims ( n -- hits )
   0 swap 0 do hit? if 1+ then loop ;

 1000 sims 4 * . 3232  ok
 10000 sims 4 * . 31448  ok
 100000 sims 4 * . 313704  ok
 1000000 sims 4 * . 3141224  ok
 10000000 sims 4 * . 31409400  ok


## Fortran

{{works with|Fortran|90 and later}}

```fortran
MODULE Simulation
 
   IMPLICIT NONE
 
   CONTAINS
 
   FUNCTION Pi(samples)
     REAL :: Pi
     REAL :: coords(2), length
     INTEGER :: i, in_circle, samples
  
     in_circle = 0
     DO i=1, samples
       CALL RANDOM_NUMBER(coords)
       coords = coords * 2 - 1
       length = SQRT(coords(1)*coords(1) + coords(2)*coords(2))
       IF (length <= 1) in_circle = in_circle + 1
     END DO
     Pi = 4.0 * REAL(in_circle) / REAL(samples)
   END FUNCTION Pi
 
 END MODULE Simulation
  
 PROGRAM MONTE_CARLO
 
   USE Simulation 
   
   INTEGER :: n = 10000
 
   DO WHILE (n <= 100000000)
     WRITE (*,*) n, Pi(n)
     n = n * 10
   END DO
     
 END PROGRAM MONTE_CARLO
```


{{out}}

```txt

        10000     3.12120
       100000     3.13772
      1000000     3.13934
     10000000     3.14114
    100000000     3.14147

```

{{works with|Fortran|2008 and later}}

```fortran

        program mc
        integer :: n,i
        real(8) :: pi
        n=10000
        do i=1,5
          print*,n,pi(n)
          n = n * 10
        end do
        end program

        function  pi(n)
        integer :: n
        real(8) :: x(2,n),pi
        call random_number(x)
        pi = 4.d0 * dble( count( hypot(x(1,:),x(2,:)) <= 1.d0 ) ) / n
        end function

```


## FreeBASIC


```freebasic
' version 23-10-2016
' compile with: fbc -s console

Randomize Timer  'seed the random function

Dim As Double x, y, pi, error_
Dim As UInteger m = 10, n, n_start, n_stop = m, p

Print
Print " Mumber of throws  Ratio (Pi)     Error"
Print

Do
    For n = n_start To n_stop -1
        x = Rnd
        y = Rnd
        If (x * x + y * y) <= 1 Then p = p +1
    Next
    Print Using "    ############,  "; m ;
    pi = p * 4 / m
    error_ = 3.141592653589793238462643383280 - pi
    Print RTrim(Str(pi),"0");Tab(35); Using "##.#############"; error_
    m = m * 10
    n_start = n_stop
    n_stop = m
Loop Until m > 1000000000 ' 1,000,000,000


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
 Mumber of throws  Ratio (Pi)     Error

               10  3.2            -0.0584073464102
              100  3.16           -0.0184073464102
            1,000  3.048           0.0935926535898
           10,000  3.1272          0.0143926535898
          100,000  3.13672         0.0048726535898
        1,000,000  3.14148         0.0001126535898
       10,000,000  3.1417668      -0.0001741464102
      100,000,000  3.14141         0.0001826535898
    1,000,000,000  3.14169192     -0.0000992664102
```



## Futhark


Since Futhark is a pure language, random numbers are implemented using Sobol sequences.


```Futhark

import "futlib/math"

default(f32)

fun dirvcts(): [2][30]i32 =
    [
            [
                536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608, 4194304, 2097152, 1048576, 524288, 262144, 131072, 65536, 32768, 16384, 8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1
            ],
            [
                536870912, 805306368, 671088640, 1006632960, 570425344, 855638016, 713031680, 1069547520, 538968064, 808452096, 673710080, 1010565120, 572653568, 858980352, 715816960, 1073725440, 536879104, 805318656, 671098880, 1006648320, 570434048, 855651072, 713042560, 1069563840, 538976288, 808464432, 673720360, 1010580540, 572662306, 858993459
            ]
    ]


fun grayCode(x: i32): i32 = (x >> 1) ^ x

----------------------------------------
--- Sobol Generator
----------------------------------------
fun testBit(n: i32, ind: i32): bool =
    let t = (1 << ind) in (n & t) == t

fun xorInds(n: i32) (dir_vs: [num_bits]i32): i32 =
    let reldv_vals = zipWith (\ dv i  ->
                                if testBit(grayCode n,i)
                                then dv else 0)
                             dir_vs (iota num_bits)
    in reduce (^) 0 reldv_vals

fun sobolIndI (dir_vs: [m][num_bits]i32, n: i32): [m]i32 =
    map (xorInds n) dir_vs

fun sobolIndR(dir_vs:  [m][num_bits]i32) (n: i32 ): [m]f32 =
    let divisor = 2.0 ** f32(num_bits)
    let arri    = sobolIndI( dir_vs, n )
    in map (\ (x: i32): f32  -> f32(x) / divisor) arri

fun main(n: i32): f32 =
    let rand_nums = map (sobolIndR (dirvcts())) (iota n)
    let dists     = map (\xy ->
                           let (x,y) = (xy[0],xy[1]) in f32.sqrt(x*x + y*y))
                        rand_nums

    let bs        = map (\d -> if d <= 1.0f32 then 1 else 0) dists

    let inside    = reduce (+) 0 bs
    in 4.0f32*f32(inside)/f32(n)

```



## Go

'''Using standard library math/rand:'''

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

func getPi(numThrows int) float64 {
    inCircle := 0
    for i := 0; i < numThrows; i++ {
        //a square with a side of length 2 centered at 0 has 
        //x and y range of -1 to 1
        randX := rand.Float64()*2 - 1 //range -1 to 1
        randY := rand.Float64()*2 - 1 //range -1 to 1
        //distance from (0,0) = sqrt((x-0)^2+(y-0)^2)
        dist := math.Hypot(randX, randY)
        if dist < 1 { //circle with diameter of 2 has radius of 1
            inCircle++
        }
    }
    return 4 * float64(inCircle) / float64(numThrows)
}

func main() {
    rand.Seed(time.Now().UnixNano())
    fmt.Println(getPi(10000))
    fmt.Println(getPi(100000))
    fmt.Println(getPi(1000000))
    fmt.Println(getPi(10000000))
    fmt.Println(getPi(100000000))
}
```

{{out}}

```txt

3.1164
3.1462
3.142892
3.1419692
3.14149596

```

'''Using x/exp/rand:'''

For very careful Monte Carlo studies, you might consider the subrepository rand library.  The random number generator there has some advantages such as better known statistical properties and better use of memory.

```go
package main

import (
    "fmt"
    "math"
    "time"

    "golang.org/x/exp/rand"
)

func getPi(numThrows int) float64 {
    inCircle := 0
    for i := 0; i < numThrows; i++ {
        //a square with a side of length 2 centered at 0 has
        //x and y range of -1 to 1
        randX := rand.Float64()*2 - 1 //range -1 to 1
        randY := rand.Float64()*2 - 1 //range -1 to 1
        //distance from (0,0) = sqrt((x-0)^2+(y-0)^2)
        dist := math.Hypot(randX, randY)
        if dist < 1 { //circle with diameter of 2 has radius of 1
            inCircle++
        }
    }
    return 4 * float64(inCircle) / float64(numThrows)
}

func main() {
    rand.Seed(uint64(time.Now().UnixNano()))
    fmt.Println(getPi(10000))
    fmt.Println(getPi(100000))
    fmt.Println(getPi(1000000))
    fmt.Println(getPi(10000000))
    fmt.Println(getPi(100000000))
}
```



## Haskell


```haskell

import System.Random
import Control.Monad

get_pi throws = do results <- replicateM throws one_trial
                   return (4 * fromIntegral (foldl (+) 0 results) / fromIntegral throws)
  where
    one_trial = do rand_x <- randomRIO (-1, 1)
                   rand_y <- randomRIO (-1, 1)
                   let dist :: Double
                       dist = sqrt (rand_x*rand_x + rand_y*rand_y)
                   return (if dist < 1 then 1 else 0)

```

Example:
 Prelude System.Random Control.Monad> get_pi 10000
 3.1352
 Prelude System.Random Control.Monad> get_pi 100000
 3.15184
 Prelude System.Random Control.Monad> get_pi 1000000
 3.145024


## HicEst


```HicEst
FUNCTION Pi(samples)
   inside = 0
   DO i = 1, samples
      inside = inside + ( (RAN(1)^2 + RAN(1)^2)^0.5 <= 1)
   ENDDO
   Pi = 4 * inside / samples
END

   WRITE(ClipBoard) Pi(1E4) ! 3.1504
   WRITE(ClipBoard) Pi(1E5) ! 3.14204
   WRITE(ClipBoard) Pi(1E6) ! 3.141672
   WRITE(ClipBoard) Pi(1E7) ! 3.1412856
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
  every t := 10 ^ ( 5 to 9 ) do
     printf("Rounds=%d Pi ~ %r\n",t,getPi(t))
end

link printf

procedure getPi(rounds)
   incircle := 0. 
   every 1 to rounds do 
      if 1 > sqrt((?0 * 2 - 1) ^ 2 + (?0 * 2 - 1) ^ 2) then 
         incircle +:= 1
   return 4 * incircle / rounds
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf] 

{{out}}

```txt
Rounds=100000 Pi ~ 3.143400
Rounds=1000000 Pi ~ 3.141656
Rounds=10000000 Pi ~ 3.140437
Rounds=100000000 Pi ~ 3.141375
Rounds=1000000000 Pi ~ 3.141604
```



## J

'''Explicit Solution:'''

```j
piMC=: monad define "0
  4* y%~ +/ 1>: %: +/ *: <: +: (2,y) ?@$ 0
)
```


'''Tacit Solution:'''

```j
piMCt=: (0.25&* %~ +/@(1 >: [: +/&.:*: _1 2 p. 0 ?@$~ 2&,))"0
```


'''Examples:'''

```j
   piMC 1e6
3.1426
   piMC 10^i.7
4 2.8 3.24 3.168 3.1432 3.14256 3.14014
```



## Java


```java
public class MC {
	public static void main(String[] args) {
		System.out.println(getPi(10000));
		System.out.println(getPi(100000));
		System.out.println(getPi(1000000));
		System.out.println(getPi(10000000));
		System.out.println(getPi(100000000));
		
	}
	public static double getPi(int numThrows){
		int inCircle= 0;
		for(int i= 0;i < numThrows;i++){
			//a square with a side of length 2 centered at 0 has 
			//x and y range of -1 to 1
			double randX= (Math.random() * 2) - 1;//range -1 to 1
			double randY= (Math.random() * 2) - 1;//range -1 to 1
			//distance from (0,0) = sqrt((x-0)^2+(y-0)^2)
			double dist= Math.sqrt(randX * randX + randY * randY);
			//^ or in Java 1.5+: double dist= Math.hypot(randX, randY);
			if(dist < 1){//circle with diameter of 2 has radius of 1
				inCircle++;
			}
		}
		return 4.0 * inCircle / numThrows;
	}
}
```

{{out}}
 3.1396
 3.14256
 3.141516
 3.1418692
 3.14168604
{{works with|Java|8+}}

```java
package montecarlo;

import java.util.stream.IntStream;
import java.util.stream.DoubleStream;

import static java.lang.Math.random;
import static java.lang.Math.hypot;
import static java.lang.System.out;

public interface MonteCarlo {
  public static void main(String... arguments) {
    IntStream.of(
      10000,
      100000,
      1000000,
      10000000,
      100000000
    )
      .mapToDouble(MonteCarlo::pi)
      .forEach(out::println)
    ;
  }

  public static double range() {
    //a square with a side of length 2 centered at 0 has 
    //x and y range of -1 to 1
    return (random() * 2) - 1;
  }

  public static double pi(int numThrows){
    long inCircle = DoubleStream.generate(
      //distance from (0,0) = hypot(x, y)
      () -> hypot(range(), range())
    )
      .limit(numThrows)
      .unordered()
      .parallel()
      //circle with diameter of 2 has radius of 1
      .filter(d -> d < 1)
      .count()
    ;
    return (4.0 * inCircle) / numThrows;
  }
}
```

{{out}}
 3.1556
 3.14416
 3.14098
 3.1419512
 3.14160312


## JavaScript


### ES5


```JavaScript
function mcpi(n) {
    var x, y, m = 0;

    for (var i = 0; i < n; i += 1) {
        x = Math.random();
        y = Math.random();

        if (x * x + y * y < 1) {
            m += 1;
        }
    }

    return 4 * m / n;
}

console.log(mcpi(1000));
console.log(mcpi(10000));
console.log(mcpi(100000));
console.log(mcpi(1000000));
console.log(mcpi(10000000));
```


```txt
3.168
3.1396
3.13692
3.140512
3.1417656

```



### ES6


```JavaScript
(() => {
    'use strict';

    // monteCarloPi :: Int -> Float
    const monteCarloPi = n =>
        4 * range(1, n)
        .reduce(a => {
            const [x, y] = [rnd(), rnd()];
            return x * x + y * y < 1 ? a + 1 : a;
        }, 0) / n;


    // GENERIC FUNCTIONS

    // range :: Int -> Int -> [Int]
    const range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // rnd :: () -> Float
    const rnd = Math.random;


    // TEST with from 1000 samples to 10E8 samples
    return range(3, 8)
        .map(x => monteCarloPi(Math.pow(10, x)));

    // e.g. -> [3.14, 3.1404, 3.13304, 3.142408, 3.1420304, 3.14156788]
})();

```


{{Out}} (5 sample runs with increasing sample sizes)

```JavaScript
[3.14, 3.1404, 3.13304, 3.142408, 3.1420304, 3.14156788]
```



## Jsish

From Javascript ES5 entry, with PRNG seeded during unit testing for reproducibility.

```javascript
/* Monte Carlo methods, in Jsish */
function mcpi(n) {
    var x, y, m = 0;

    for (var i = 0; i < n; i += 1) {
        x = Math.random();
        y = Math.random();

        if (x * x + y * y < 1) {
            m += 1;
        }
    }

    return 4 * m / n;
}

if (Interp.conf('unitTest')) {
    Math.srand(0);
;    mcpi(1000);
;    mcpi(10000);
;    mcpi(100000);
;    mcpi(1000000);
}

/*
=!EXPECTSTART!=
mcpi(1000) ==> 3.108
mcpi(10000) ==> 3.1236
mcpi(100000) ==> 3.13732
mcpi(1000000) ==> 3.142124
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u monteCarlos.jsi
[PASS] monteCarlos.jsi
```



## Julia

{{works with|Julia|0.6}}


```julia
function monteπ(n)
    s = count(rand() ^ 2 + rand() ^ 2 < 1 for _ in 1:n)
    return 4s / n
end

for n in 10 .^ (3:8)
    p = monteπ(n)
    println("$(lpad(n, 9)): π ≈ $(lpad(p, 10)), pct.err = ", @sprintf("%2.5f%%", abs(p - π) / π))
end
```


{{out}}

```txt
     1000: π ≈      3.224, pct.err = 0.02623%
    10000: π ≈     3.1336, pct.err = 0.00254%
   100000: π ≈    3.13468, pct.err = 0.00220%
  1000000: π ≈    3.14156, pct.err = 0.00001%
 10000000: π ≈  3.1412348, pct.err = 0.00011%
100000000: π ≈ 3.14123216, pct.err = 0.00011%
```



## K


```K
   sim:{4*(+/{~1<+/(2_draw 0)^2}'!x)%x}

   sim 10000
3.103

   sim'10^!8
4 2.8 3.4 3.072 3.1212 3.14104 3.14366 3.1413
```



## Kotlin


```scala
// version 1.1.0

fun mcPi(n: Int): Double {
    var inside = 0
    (1..n).forEach {
        val x = Math.random()
        val y = Math.random()
        if (x * x + y * y <= 1.0) inside++
    }
    return 4.0 * inside / n
}

fun main(args: Array<String>) {   
    println("Iterations -> Approx Pi  -> Error%")
    println("----------    ----------    ------")
    var n = 1_000
    while (n <= 100_000_000) {
        val pi = mcPi(n)
        val err = Math.abs(Math.PI - pi) / Math.PI * 100.0
        println(String.format("%9d  -> %10.8f -> %6.4f", n, pi, err))
        n *= 10
    }
}
```

Sample output:
{{out}}

```txt

Iterations -> Approx Pi  -> Error%
----------    ----------    ------
     1000  -> 3.12800000 -> 0.4327
    10000  -> 3.15040000 -> 0.2803
   100000  -> 3.14468000 -> 0.0983
  1000000  -> 3.13982000 -> 0.0564
 10000000  -> 3.14182040 -> 0.0072
100000000  -> 3.14160244 -> 0.0003    

```



## Liberty BASIC


```lb

 for pow = 2 to 6
    n = 10^pow
    print n, getPi(n)
next

end

function getPi(n)
    incircle = 0
    for throws=0 to n
        scan
        incircle = incircle + (rnd(1)^2+rnd(1)^2 < 1)
    next
    getPi = 4*incircle/throws
end function
 
 
```


{{out}}

```txt

100           2.89108911
1000          3.12887113
10000         3.13928607
100000        3.13864861
1000000       3.13945686

```



## Locomotive Basic



```locobasic
10 mode 1:randomize time:defint a-z
20 input "How many samples";n
30 u=n/100+1
40 r=100
50 for i=1 to n
60 if i mod u=0 then locate 1,3:print using "##% done"; i/n*100
70 x=rnd*2*r-r
80 y=rnd*2*r-r
90 if sqr(x*x+y*y)<r then m=m+1
100 next
110 pi2!=4*m/n
120 locate 1,3
130 print m;"points in circle"
140 print "Computed value of pi:"pi2!
150 print "Difference to real value of pi: ";
160 print using "+#.##%"; (pi2!-pi)/pi*100
```


[[File:Monte Carlo, 200 points, Locomotive BASIC.png]]
[[File:Monte Carlo, 5000 points, Locomotive BASIC.png]]


## Logo


```logo

to square :n
  output :n * :n
end
to trial :r
  output less? sum square random :r square random :r  square :r
end
to sim :n :r
  make "hits 0
  repeat :n [if trial :r [make "hits :hits + 1]]
  output 4 * :hits / :n
end

show sim    1000 10000  ; 3.18
show sim   10000 10000  ; 3.1612
show sim  100000 10000  ; 3.145
show sim 1000000 10000  ; 3.140828

```



## LSL

To test it yourself; rez a box on the ground, and add the following as a New Script.  
(Be prepared to wait... LSL can be slow, but the Servers are typically running thousands of scripts in parallel so what do you expect?)

```LSL
integer iMIN_SAMPLE_POWER = 0;
integer iMAX_SAMPLE_POWER = 6;
default {
	state_entry() {
		llOwnerSay("Estimating Pi ("+(string)PI+")");
		integer iSample = 0;
		for(iSample=iMIN_SAMPLE_POWER ; iSample<=iMAX_SAMPLE_POWER  ; iSample++) {
			integer iInCircle = 0;
			integer x = 0;
			integer iMaxSamples = (integer)llPow(10, iSample);
			for(x=0 ; x<iMaxSamples ; x++) {
				if(llSqrt(llPow(llFrand(2.0)-1.0, 2.0)+llPow(llFrand(2.0)-1.0, 2.0))<1.0) {
					iInCircle++;
				}
			}
			float fPi = ((4.0*iInCircle)/llPow(10, iSample));
			float fError = llFabs(100.0*(PI-fPi)/PI);
			llOwnerSay((string)iSample+": "+(string)iMaxSamples+" = "+(string)fPi+", Error = "+(string)fError+"%");
		}
		llOwnerSay("Done.");
	}
}
```

{{out}}

```txt
Estimating Pi (3.141593)
0: 1 = 4.000000, Error = 27.323954%
1: 10 = 4.000000, Error = 27.323954%
2: 100 = 2.880000, Error = 8.326753%
3: 1000 = 3.188000, Error = 1.477192%
4: 10000 = 3.133600, Error = 0.254414%
5: 100000 = 3.138840, Error = 0.087620%
6: 1000000 = 3.142684, Error = 0.034739%
Done.
```



## Lua


```lua
function MonteCarlo ( n_throws )
    math.randomseed( os.time() )

    n_inside = 0
    for i = 1, n_throws do
    	if math.random()^2 + math.random()^2 <= 1.0 then
            n_inside = n_inside + 1
    	end
    end    

    return 4 * n_inside / n_throws
end

print( MonteCarlo( 10000 ) )
print( MonteCarlo( 100000 ) )
print( MonteCarlo( 1000000 ) )
print( MonteCarlo( 10000000 ) )
```

{{out}}

```txt
3.1436
3.13636
3.14376
3.1420188
```



## Mathematica

We define a function with variable sample size:

```Mathematica

 MonteCarloPi[samplesize_Integer] := N[4Mean[If[# > 1, 0, 1] & /@ Norm /@ RandomReal[1, {samplesize, 2}]]]

```

Example (samplesize=10,100,1000,....10000000):

```Mathematica

 {#, MonteCarloPi[#]} & /@ (10^Range[1, 7]) // Grid

```

gives back:

```Mathematica

10		3.2
100		3.16
1000		3.152
10000		3.1228
100000		3.14872
1000000		3.1408
10000000	3.14134

```



```Mathematica

monteCarloPi = 4. Mean[UnitStep[1 - Total[RandomReal[1, {2, #}]^2]]] &;
monteCarloPi /@ (10^Range@6)

```



## MATLAB

See: [http://www.mathworks.com/discovery/monte-carlo-simulation.html Monte Carlo Simulation] in MATLAB for more examples

The first example given is not vectorized. MATLAB has a self-imposed memory limitation that prevents this simulation from having more than 3 decimal digits of accuracy. Because of this limitation it is best to vectorize the code as much as possible so extra memory isn't consumed by unneeded variables. Therefore, I have provided a second solution that is maximally vectorized.

Minimally Vectorized:

```MATLAB
function piEstimate = monteCarloPi(numDarts)

    %The square has a sides of length 2, which means the circle has radius
    %1.
    
    %Generate a table of random x-y value pairs in the range [0,1] sampled
    %from the uniform distribution for each axis.
    darts = rand(numDarts,2);
    
    %Any darts that are in the circle will have position vector whose
    %length is less than or equal to 1 squared.
    dartsInside = ( sum(darts.^2,2) <= 1 );
    
    piEstimate = 4*sum(dartsInside)/numDarts;

end

```


Completely Vectorized:

```MATLAB
function piEstimate = monteCarloPi(numDarts)
    
    piEstimate = 4*sum( sum(rand(numDarts,2).^2,2) <= 1 )/numDarts;

end
```


{{out}}

```MATLAB>>
 monteCarloPi(7000000)

ans =

   3.141512000000000
```


## Maxima


```Maxima
load("distrib");
approx_pi(n):= block(
  [x: random_continuous_uniform(0, 1, n),
   y: random_continuous_uniform(0, 1, n),
   r, cin: 0, listarith: true],
   r: x^2 + y^2,
   for r0 in r do if r0<1 then cin: cin + 1,
   4*cin/n);
 
float(approx_pi(100));
```



## MAXScript


```txt

fn monteCarlo iterations =
(
    radius = 1.0
    pointsInCircle = 0
    for i in 1 to iterations do
    (
        testPoint = [(random -radius radius), (random -radius radius)]
        if length testPoint <= radius then
        (
            pointsInCircle += 1
        )
    )
    4.0 * pointsInCircle / iterations
)

```


=={{header|МК-61/52}}==
<lang>П0	П1	0	П4	СЧ	x^2	^	СЧ	x^2	+
1	-	x<0	15	КИП4	L0	04	ИП4	4	*
ИП1	/	С/П
```


''Example:'' for n = ''100'' the output is ''3.2''.


## Nim


```nim
import math
randomize()

proc pi(nthrows): float =
  var inside = 0
  for i in 1..int64(nthrows):
    if hypot(random(1.0), random(1.0)) < 1:
      inc inside
  return float(4 * inside) / nthrows

for n in [10e4, 10e6, 10e7, 10e8]:
  echo pi(n)
```

{{out}}

```txt
3.15336
3.1405116
3.14163332
3.141486144
```



## OCaml


```ocaml
let get_pi throws =
  let rec helper i count =
    if i = throws then count
    else
      let rand_x = Random.float 2.0 -. 1.0
      and rand_y = Random.float 2.0 -. 1.0 in
      let dist = sqrt (rand_x *. rand_x +. rand_y *. rand_y) in
      if dist < 1.0 then
        helper (i+1) (count+1)
      else
        helper (i+1) count
  in float (4 * helper 0 0) /. float throws
```

Example:
 # get_pi 10000;;
 - : float = 3.15
 # get_pi 100000;;
 - : float = 3.13272
 # get_pi 1000000;;
 - : float = 3.143808
 # get_pi 10000000;;
 - : float = 3.1421704
 # get_pi 100000000;;
 - : float = 3.14153872


## Octave



```octave
function p = montepi(samples)
  in_circle = 0;
  for samp = 1:samples
    v = [ unifrnd(-1,1), unifrnd(-1,1) ];
    if ( v*v.' <= 1.0 )
      in_circle++;
    endif
  endfor
  p = 4*in_circle/samples;
endfunction

l = 1e4;
while (l < 1e7)
  disp(montepi(l));
  l *= 10;
endwhile
```


Since it runs slow, I've stopped it at the second iteration, obtaining:

```txt
 3.1560
 3.1496
```



###  Much faster implementation 



```octave

function result = montepi(n)
  result = sum(rand(1,n).^2+rand(1,n).^2<1)/n*4;
endfunction

```



## PARI/GP


```parigp
MonteCarloPi(tests)=4.*sum(i=1,tests,norml2([random(1.),random(1.)])<1)/tests;
```

A hundred million tests (about a minute) yielded 3.14149000, slightly more precise (and round!) than would have been expected.  A million gave 3.14162000 and a thousand 3.14800000.


## Pascal

{{libheader|Math}}

```pascal
Program MonteCarlo(output);

uses
  Math;

function MC_Pi(expo: integer): real;
  var
    x, y: real;
    i, hits, samples: longint;
  begin
    samples := 10**expo;
    hits := 0;
    randomize;
    for i := 1 to samples do
    begin
      x := random;
      y := random;
      if sqrt(x*x + y*y) < 1.0 then
        inc(hits);
    end;
    MC_Pi := 4.0 * hits / samples;
  end;

var
  i: integer;
begin
  for i := 4 to 8 do
    writeln (10**i, ' samples give ', MC_Pi(i):7:5, ' as pi.');
end.

```

{{out}}

```txt
:> ./MonteCarlo
10000 samples give 3.14480 as pi.
100000 samples give 3.14484 as pi.
1000000 samples give 3.13970 as pi.
10000000 samples give 3.14100 as pi.
100000000 samples give 3.14162 as pi.

```



## Perl


```perl
sub pi {
  my $nthrows = shift;
  my $inside = 0;
  foreach (1 .. $nthrows) {
    my $x = rand() * 2 - 1;
    my $y = rand() * 2 - 1;
    if (sqrt($x*$x + $y*$y) < 1) {
      $inside++;
    }
  }
  return 4 * $inside / $nthrows;
}

printf "%9d: %07f\n", $_, pi($_) for 10**4, 10**6;
```

{{out}}

```txt

    10000: 3.132000
  1000000: 3.141596

```



## Perl 6

{{works with|rakudo|2015-09-24}}
We'll consider the upper-right quarter of the unitary disk centered at the origin.  Its area is <math>\pi \over 4</math>.

```Perl 6
my @random_distances = ([+] rand**2 xx 2) xx *;

sub approximate_pi(Int $n) {
    4 * @random_distances[^$n].grep(* < 1) / $n
}

say "Monte-Carlo π approximation:";
say "$_ iterations:  ", approximate_pi $_
    for 100, 1_000, 10_000;

```

{{out}}

```txt
Monte-Carlo π approximation:
100 iterations:  2.88
1000 iterations:  3.096
10000 iterations:  3.1168
```


We don't really need to write a function, though.  A lazy list would do:


```perl6
my @pi = ([\+] 4 * (1 > [+] rand**2 xx 2) xx *) Z/ 1 .. *;
say @pi[10, 1000, 10_000];
```



## Phix


```Phix
integer N = 100
for i=1 to 6 do
    integer inside = 0
    for i=1 to N do
        integer x = rand(N),
                y = rand(N)
        inside += (x*x+y*y<N*N)
    end for
    ?{N,4*inside/N}
    N *= 10
end for
```

{{out}}

```txt

{100,3.2}
{1000,3.116}
{10000,3.1736}
{100000,3.13996}
{1000000,3.141856}
{10000000,3.1415728}

```



## PHP


```PHP
<?
$loop = 1000000; # loop to 1,000,000
$count = 0;
for ($i=0; $i<$loop; $i++) {
  $x = rand() / getrandmax();
  $y = rand() / getrandmax();
  if(($x*$x) + ($y*$y)<=1) $count++;
}
echo "loop=".number_format($loop).", count=".number_format($count).", pi=".($count/$loop*4);
?>
```

{{out}}

```txt
loop=1,000,000, count=785,462, pi=3.141848
```



## PicoLisp


```PicoLisp
(de carloPi (Scl)
   (let (Dim (** 10 Scl)  Dim2 (* Dim Dim)  Pi 0)
      (do (* 4 Dim)
         (let (X (rand 0 Dim)  Y (rand 0 Dim))
            (when (>= Dim2 (+ (* X X) (* Y Y)))
               (inc 'Pi) ) ) )
      (format Pi Scl) ) )

(for N 6
   (prinl (carloPi N)) )
```

{{out}}

```txt
3.4
3.23
3.137
3.1299
3.14360
3.140964
```



## PowerShell

{{works with|PowerShell|2}}

```powershell
function Get-Pi ($Iterations = 10000) {
    $InCircle = 0
    for ($i = 0; $i -lt $Iterations; $i++) {
        $x = Get-Random 1.0
        $y = Get-Random 1.0
        if ([Math]::Sqrt($x * $x + $y * $y) -le 1) {
            $InCircle++
        }
    }
    $Pi = [decimal] $InCircle / $Iterations * 4
    $RealPi = [decimal] "3.141592653589793238462643383280"
    $Diff = [Math]::Abs(($Pi - $RealPi) / $RealPi * 100)
    New-Object PSObject `
        | Add-Member -PassThru NoteProperty Iterations $Iterations `
        | Add-Member -PassThru NoteProperty Pi $Pi `
        | Add-Member -PassThru NoteProperty "% Difference" $Diff
}
```

This returns a custom object with appropriate properties which automatically enables a nice tabular display:

```txt
PS Home:\> 10,100,1e3,1e4,1e5,1e6 | ForEach-Object { Get-Pi $_ }

 Iterations          Pi                      % Difference
 ----------          --                      ------------
         10         3,6    14,591559026164641753596309630
        100        3,40     8,225361302488828322840959090
       1000       3,208    2,1138114877600474293158225800
      10000      3,1444    0,0893606116311387583356211100
     100000     3,14712    0,1759409006731298209938938800
    1000000    3,141364    0,0072782698142600895432451100
```



## PureBasic


```PureBasic
OpenConsole()
 
Procedure.d MonteCarloPi(throws.d)
	inCircle.d = 0
		For i = 1 To throws.d
			randX.d = (Random(2147483647)/2147483647)*2-1
			randY.d = (Random(2147483647)/2147483647)*2-1 
			dist.d  = Sqr(randX.d*randX.d + randY.d*randY.d)
			If dist.d < 1 
				inCircle = inCircle + 1
			EndIf
		Next i
	pi.d = (4 * inCircle / throws.d)	
	ProcedureReturn pi.d
	
EndProcedure

PrintN ("'built-in' #Pi         = " + StrD(#PI,20))
PrintN ("MonteCarloPi(10000)    = " + StrD(MonteCarloPi(10000),20))
PrintN ("MonteCarloPi(100000)   = " + StrD(MonteCarloPi(100000),20))
PrintN ("MonteCarloPi(1000000)  = " + StrD(MonteCarloPi(1000000),20))
PrintN ("MonteCarloPi(10000000) = " + StrD(MonteCarloPi(10000000),20))

PrintN("Press any key"): Repeat: Until Inkey() <> ""

```

{{out}}

```txt
'built-in' #PI         = 3.14159265358979310000
MonteCarloPi(10000)    = 3.17119999999999980000
MonteCarloPi(100000)   = 3.14395999999999990000
MonteCarloPi(1000000)  = 3.14349599999999980000
MonteCarloPi(10000000) = 3.14127720000000020000
Press any key
```
 


## Python


### At the interactive prompt

Python 2.6rc2 (r26rc2:66507, Sep 18 2008, 14:27:33) [MSC v.1500 32 bit (Intel)] on win32
IDLE 2.6rc2      

One use of the "sum" function is to count how many times something is true (because True = 1, False = 0):

```python>>>
 import random, math
>>> throws = 1000
>>> 4.0 * sum(math.hypot(*[random.random()*2-1
	                 for q in [0,1]]) < 1
              for p in xrange(throws)) / float(throws)
3.1520000000000001
>>> throws = 1000000
>>> 4.0 * sum(math.hypot(*[random.random()*2-1
	                 for q in [0,1]]) < 1
              for p in xrange(throws)) / float(throws)
3.1396359999999999
>>> throws = 100000000
>>> 4.0 * sum(math.hypot(*[random.random()*2-1
	                 for q in [0,1]]) < 1
              for p in xrange(throws)) / float(throws)
3.1415666400000002
```



### As a program using a function


```python

from random import random
from math import hypot
try:
    import psyco
    psyco.full()
except:
    pass

def pi(nthrows):
    inside = 0
    for i in xrange(nthrows):
        if hypot(random(), random()) < 1:
            inside += 1
    return 4.0 * inside / nthrows

for n in [10**4, 10**6, 10**7, 10**8]:
    print "%9d: %07f" % (n, pi(n))

```



### Faster implementation using Numpy


```python

import numpy as np

n = input('Number of samples: ')
print np.sum(np.random.rand(n)**2+np.random.rand(n)**2<1)/float(n)*4

```



## R


```R
# nice but not suitable for big samples!
monteCarloPi <- function(samples) {
  x <- runif(samples, -1, 1) # for big samples, you need a lot of memory!
  y <- runif(samples, -1, 1)
  l <- sqrt(x*x + y*y)
  return(4*sum(l<=1)/samples)
}

# this second function changes the samples number to be
# multiple of group parameter (default 100).
monteCarlo2Pi <- function(samples, group=100) {
  lim <- ceiling(samples/group)
  olim <- lim
  c <- 0
  while(lim > 0) {
    x <- runif(group, -1, 1)
    y <- runif(group, -1, 1)
    l <- sqrt(x*x + y*y)
    c <- c + sum(l <= 1)
    lim <- lim - 1
  }
  return(4*c/(olim*group))
}

print(monteCarloPi(1e4))
print(monteCarloPi(1e5))
print(monteCarlo2Pi(1e7))
```



## Racket


```racket
#lang racket

(define (in-unit-circle? x y) (<= (sqrt (+ (sqr x) (sqr y))) 1))
;; point in ([-1,1], [-1,1])
(define (random-point-in-2x2-square) (values (* 2 (- (random) 1/2)) (* 2 (- (random) 1/2))))

;; Area of circle is (pi r^2). r is 1, area of circle is pi
;; Area of square is 2^2 = 4
;; There is a pi/4 chance of landing in circle
;; .: pi = 4*(proportion passed) = 4*(passed/samples)
(define (passed:samples->pi passed samples) (* 4 (/ passed samples)))

;; generic kind of monte-carlo simulation
(define (monte-carlo run-length report-frequency
                     sample-generator pass?
                     interpret-result)
  (let inner ((samples 0) (passed 0) (cnt report-frequency))
    (cond
      [(= samples run-length) (interpret-result passed samples)]
      [(zero? cnt) ; intermediate report
       (printf "~a samples of ~a: ~a passed -> ~a~%"
               samples run-length passed (interpret-result passed samples))
       (inner samples passed report-frequency)]
      [else
       (inner (add1 samples)
              (if (call-with-values sample-generator pass?)
                  (add1 passed) passed) (sub1 cnt))])))

;; (monte-carlo ...) gives an "exact" result... which will be a fraction.
;; to see how it looks as a decimal we can exact->inexact it
(let ((mc (monte-carlo 10000000 1000000 random-point-in-2x2-square in-unit-circle? passed:samples->pi)))
  (printf "exact = ~a~%inexact = ~a~%(pi - guess) = ~a~%" mc (exact->inexact mc) (- pi mc)))
```

{{out}}

```txt
1000000 samples of 10000000: 785763 passed -> 785763/250000
2000000 samples of 10000000: 1571487 passed -> 1571487/500000
3000000 samples of 10000000: 2356776 passed -> 98199/31250
4000000 samples of 10000000: 3141924 passed -> 785481/250000
5000000 samples of 10000000: 3927540 passed -> 196377/62500
6000000 samples of 10000000: 4713072 passed -> 98189/31250
7000000 samples of 10000000: 5498300 passed -> 54983/17500
8000000 samples of 10000000: 6283199 passed -> 6283199/2000000
9000000 samples of 10000000: 7068065 passed -> 1413613/450000
exact = 3926793/1250000
inexact = 3.1414344
(pi - guess) = 0.00015825358979304482
```


A little more Racket-like is the use of an iterator (in this case '''for/fold'''),
which is clearer than an inner function:

```racket
#lang racket
(define (in-unit-circle? x y) (<= (sqrt (+ (sqr x) (sqr y))) 1))
;; Good idea made in another task that:
;;  The proportions of hits is the same in the unit square and 1/4 of a circle.
;; point in ([0,1], [0,1])
(define (random-point-in-unit-square) (values (random) (random)))
;; generic kind of monte-carlo simulation
;; Area of circle is (pi r^2). r is 1, area of circle is pi
;; Area of square is 2^2 = 4
;; There is a pi/4 chance of landing in circle
;; .: pi = 4*(proportion passed) = 4*(passed/samples)
(define (passed:samples->pi passed samples) (* 4 (/ passed samples)))

(define (monte-carlo/2 run-length report-frequency sample-generator pass? interpret-result)
  (interpret-result
   (for/fold ((pass 0))
     ([n (in-range run-length)]
      #:when (when (and (not (zero? n)) (zero? (modulo n report-frequency)))
               (printf "~a samples of ~a: ~a passed -> ~a~%"
                       n run-length pass (interpret-result pass n)))
      #:when (call-with-values sample-generator pass?))
     (add1 pass))
   run-length))

;; (monte-carlo ...) gives an "exact" result... which will be a fraction.
;; to see how it looks as a decimal we can exact->inexact it
(let ((mc (monte-carlo/2 10000000 1000000 random-point-in-unit-square in-unit-circle? passed:samples->pi)))
  (printf "exact = ~a~%inexact = ~a~%(pi - guess) = ~a~%" mc (exact->inexact mc) (- pi mc)))
```


[Similar output]


## REXX

A specific-purpose commatizer function is included to format the number of iterations.

```rexx
/*REXX program computes and displays the value of  pi÷4  using the Monte Carlo algorithm*/
/*true pi*/ pi=3.141592653589793238462643383279502884197169399375105820974944592307816406
say '                    1         2         3         4         5         6         7   '
say 'scale:    1·234567890123456789012345678901234567890123456789012345678901234567890123'
say                                              /* [↑]  a two-line scale for showing pi*/
say 'true pi= '       pi"+"                      /*we might as well brag about true  pi.*/
numeric digits length(pi) - 1                    /*this program uses these decimal digs.*/
parse arg times chunk .                          /*does user want a specific number?    */
if times=='' | times==","  then times=5e12       /*five trillion should do it, hopefully*/
if chunk=='' | chunk=="."  then chunk=100000     /*perform Monte Carlo in  100k  chunks.*/
limit=10000 - 1                                  /*REXX random generates only integers. */
limitSq=limit**2                                 /*··· so, instead of one, use limit**2.*/
accuracy=0                                       /*accuracy of Monte Carlo pi  (so far).*/
!=0;  @reps= 'repetitions:  Monte Carlo  pi  is' /*pi  decimal digit accuracy  (so far).*/
say                                              /*a blank line,  just for the eyeballs.*/
      do j=1  for times % chunk
                       do chunk                  /*do Monte Carlo,  one chunk at-a-time.*/
                       if random(, limit)**2 + random(, limit)**2 <= limitSq  then !=! + 1
                       end   /*chunk*/
      reps=chunk * j                             /*calculate the number of repetitions. */
      _=compare(4*! / reps, pi)                  /*compare apples and  ···  crabapples. */
      if _<=accuracy  then iterate               /*Not better accuracy?  Keep truckin'. */
      say right(comma(reps), 20) @reps  'accurate to'  _-1  "places."  /*─1 ≡ dec. point*/
      accuracy=_                                 /*use this accuracy for next baseline. */
      end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
comma: procedure; arg _;  do k=length(_)-3  to 1  by -3; _=insert(',',_,k); end;  return _
```

{{out|output|text=  when using the default input:}}

```txt

                    1         2         3         4         5         6         7
scale:    1·234567890123456789012345678901234567890123456789012345678901234567890123

true pi=  3.141592653589793238462643383279502884197169399375105820974944592307816406+

             10,000 repetitions:  Monte Carlo  pi  is accurate to 3 places.
             50,000 repetitions:  Monte Carlo  pi  is accurate to 4 places.
            850,000 repetitions:  Monte Carlo  pi  is accurate to 5 places.
            890,000 repetitions:  Monte Carlo  pi  is accurate to 6 places.
          5,130,000 repetitions:  Monte Carlo  pi  is accurate to 7 places.
          8,620,000 repetitions:  Monte Carlo  pi  is accurate to 8 places.
         10,390,000 repetitions:  Monte Carlo  pi  is accurate to 9 places.

```

For more example runs using REXX, see the   ''discussion''   page.


## Ring


```ring

decimals(8)
see "monteCarlo(1000) = " + monteCarlo(1000) + nl
see "monteCarlo(10000) = " + monteCarlo(10000) + nl
see "monteCarlo(100000) = " + monteCarlo(100000) + nl
 
func monteCarlo t
     n=0
     for i = 1 to t
         if sqrt(pow(random(1),2) + pow(random(1),2)) <= 1 n += 1 ok
     next
     t = (4 * n) / t
     return t

```

Output:

```txt

monteCarlo(1000) = 3.11600000
monteCarlo(10000) = 3.00320000
monteCarlo(100000) = 2.99536000

```



## Ruby


```ruby
def approx_pi(throws)
  times_inside = throws.times.count {Math.hypot(rand, rand) <= 1.0}
  4.0 * times_inside / throws
end

[1000, 10_000, 100_000, 1_000_000, 10_000_000].each do |n| 
   puts "%8d samples: PI = %s" % [n, approx_pi(n)]
end
```

{{out}}

```txt
    1000 samples: PI = 3.2
   10000 samples: PI = 3.14
  100000 samples: PI = 3.13244
 1000000 samples: PI = 3.145124
10000000 samples: PI = 3.1414788
```



## Rust


```Rust
extern crate rand;

use rand::Rng;
use std::f64::consts::PI;

// `(f32, f32)` would be faster for some RNGs (including `rand::thread_rng` on 32-bit platforms
// and `rand::weak_rng` as of rand v0.4) as `next_u64` combines two `next_u32`s if not natively
// supported by the RNG.  It would less accurate however.
fn is_inside_circle((x, y): (f64, f64)) -> bool {
    x * x + y * y <= 1.0
}

fn simulate<R: Rng>(rng: &mut R, samples: usize) -> f64 {
    let mut count = 0;
    for _ in 0..samples {
        if is_inside_circle(rng.gen()) {
            count += 1;
        }
    }
    (count as f64) / (samples as f64)
}

fn main() {
    let mut rng = rand::weak_rng();

    println!("Real pi: {}", PI);

    for samples in (3..9).map(|e| 10_usize.pow(e)) {
        let estimate = 4.0 * simulate(&mut rng, samples);
        let deviation = 100.0 * (1.0 - estimate / PI).abs();
        println!("{:9}: {:<11} dev: {:.5}%", samples, estimate, deviation);
    }
}
```

{{out}}

```txt
Real pi: 3.141592653589793
     1000: 3.212       dev: 2.24114%
    10000: 3.156       dev: 0.45860%
   100000: 3.14112     dev: 0.01505%
  1000000: 3.14122     dev: 0.01186%
 10000000: 3.1408112   dev: 0.02487%
100000000: 3.14186092  dev: 0.00854%
```



## Scala


```scala
object MonteCarlo {
  private val random = new scala.util.Random

  /** Returns a random number between -1 and 1 */
  def nextThrow: Double = (random.nextDouble * 2.0) - 1.0

  /** Returns true if the argument point would be 'inside' the unit circle with
    * center at the origin, and bounded by a square with side lengths of 2
    * units. */
  def insideCircle(pt: (Double, Double)): Boolean = pt match {
    case (x, y) => (x * x) + (y * y) <= 1.0
  }
  
  /** Runs the simulation the specified number of times. Uses the result to 
    * estimate a value of pi */
  def simulate(times: Int): Double = {
    val inside = Iterator.tabulate (times) (_ => (nextThrow, nextThrow)) count insideCircle
    inside.toDouble / times.toDouble * 4.0
  }

  def main(args: Array[String]): Unit = {
    val sims = Seq(10000, 100000, 1000000, 10000000, 100000000)
    sims.foreach { n =>
      println(n+" simulations; pi estimation: "+ simulate(n))
    }
  }
}
```

{{out}}

```txt
10000 simulations; pi estimation: 3.1492
100000 simulations; pi estimation: 3.1396
1000000 simulations; pi estimation: 3.14208
10000000 simulations; pi estimation: 3.1409944
100000000 simulations; pi estimation: 3.1414386
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const func float: pi (in integer: throws) is func
  result
    var float: pi is 0.0;
  local
    var integer: throw is 0;
    var integer: inside is 0;
  begin
    for throw range 1 to throws do
      if rand(0.0, 1.0) ** 2 + rand(0.0, 1.0) ** 2 <= 1.0 then
        incr(inside);
      end if;
    end for;
    pi := flt(4 * inside) / flt(throws);
  end func;

const proc: main is func
  begin
    writeln("    10000: " <& pi(    10000) digits 5);
    writeln("   100000: " <& pi(   100000) digits 5);
    writeln("  1000000: " <& pi(  1000000) digits 5);
    writeln(" 10000000: " <& pi( 10000000) digits 5);
    writeln("100000000: " <& pi(100000000) digits 5);
  end func;
```


{{out}}

```txt

    10000: 3.14520
   100000: 3.15000
  1000000: 3.14058
 10000000: 3.14223
100000000: 3.14159

```



## SequenceL

First solution is serial due to the use of random numbers. Will always give the same result for a given n and seed

```sequenceL

import <Utilities/Random.sl>;
import <Utilities/Conversion.sl>;

main(args(2)) := monteCarlo(stringToInt(args[1]), stringToInt(args[2]));

monteCarlo(n, seed) :=
	let
		totalHits := monteCarloHelper(n, seedRandom(seed), 0);
	in
		(totalHits / intToFloat(n))*4.0;

monteCarloHelper(n, generator, result) :=
	let
		xRand := getRandom(generator);
		x := xRand.Value/(generator.RandomMax + 1.0);
		yRand := getRandom(xRand.Generator);
		y := yRand.Value/(generator.RandomMax + 1.0);
		
		newResult := result + 1 when x^2 + y^2 < 1.0 else
					 result;
	in
		result when n < 0 else
		monteCarloHelper(n - 1, yRand.Generator, newResult);

```


The second solution will run in parallel. It will also always give the same result for a given n and seed. (Note, the function monteCarloHelper is the same in both versions).


```sequenceL

import <Utilities/Random.sl>;
import <Utilities/Conversion.sl>;

main(args(2)) := monteCarlo(stringToInt(args[1]), stringToInt(args[2]));

chunks := 100;
monteCarlo3(n, seed) :=
	let
		newSeeds := getRandomSequence(seedRandom(seed), chunks).Value;
		totalHits := monteCarloHelper(n / chunks, seedRandom(newSeeds), 0);
	in
		(sum(totalHits) / intToFloat((n / chunks)*chunks))*4.0;

monteCarloHelper(n, generator, result) :=
	let
		xRand := getRandom(generator);
		x := xRand.Value/(generator.RandomMax + 1.0);
		yRand := getRandom(xRand.Generator);
		y := yRand.Value/(generator.RandomMax + 1.0);
		
		newResult := result + 1 when x^2 + y^2 < 1.0 else
					 result;
	in
		result when n < 0 else
		monteCarloHelper(n - 1, yRand.Generator, newResult);

```



## Sidef


```ruby
func monteCarloPi(nthrows) {
    4 * (^nthrows -> count_by {
        hypot(1.rand(2) - 1, 1.rand(2) - 1) < 1
    }) / nthrows
}

for n in [1e2, 1e3, 1e4, 1e5, 1e6] {
    printf("%9d: %07f\n", n, monteCarloPi(n))
}
```

{{out}}

```txt

      100: 3.320000
     1000: 3.120000
    10000: 3.169600
   100000: 3.138920
  1000000: 3.142344

```



## Stata


```stata
program define mcdisk
	clear all
	quietly set obs `1'
	gen x=2*runiform()
	gen y=2*runiform()
	quietly count if (x-1)^2+(y-1)^2<1
	display 4*r(N)/_N
end

. mcdisk 10000
3.1424

. mcdisk 1000000
3.141904

. mcdisk 100000000
3.1416253
```



## Swift

{{trans|JavaScript}}

```Swift
import Foundation

func mcpi(sampleSize size:Int) -> Double {
    var x = 0 as Double
    var y = 0 as Double
    var m = 0 as Double
    
    for i in 0..<size {
        x = Double(arc4random()) / Double(UINT32_MAX)
        y = Double(arc4random()) / Double(UINT32_MAX)
        
        if ((x * x) + (y * y) < 1) {
            m += 1
        }
    }
    
    return (4.0 * m) / Double(size)
}

println(mcpi(sampleSize: 100))
println(mcpi(sampleSize: 1000))
println(mcpi(sampleSize: 10000))
println(mcpi(sampleSize: 100000))
println(mcpi(sampleSize: 1000000))
println(mcpi(sampleSize: 10000000))
println(mcpi(sampleSize: 100000000))
```

{{out}}

```txt

3.08
3.128
3.1548
3.149
3.142032
3.1414772
3.14166832

```



## Tcl


```tcl
proc pi {samples} {
    set i 0
    set inside 0
    while {[incr i] <= $samples} {
        if {sqrt(rand()**2 + rand()**2) <= 1.0} {
            incr inside
        }
    }
    return [expr {4.0 * $inside / $samples}]
}

puts "PI is approx [expr {atan(1)*4}]\n"
foreach runs {1e2 1e4 1e6 1e8} {
    puts "$runs => [pi $runs]"
}
```

result

```txt
PI is approx 3.141592653589793

1e2 => 2.92
1e4 => 3.1344
1e6 => 3.141924
1e8 => 3.14167724
```



## Ursala


```Ursala
#import std
#import flo

mcp "n" = times/4. div\float"n" (rep"n" (fleq/.5+ sqrt+ plus+ ~~ sqr+ minus/.5+ rand)?/~& plus/1.) 0.
```

Here's a walk through.
* <code>mcp "n" = </code>... defines a function named <code>mcp</code> in terms of a dummy variable <code>"n"</code>, which will be the number of iterations used in the simulation
* <code>rand</code> ignores its argument and returns a uniformly distributed number between 0 and 1
* <code>minus/.5</code> is composed with <code>rand</code> to compute the difference between the random number and 0.5
* <code>sqr</code> squares the difference
* <code>~~</code> says to apply the function twice and return the pair of results
* <code>plus</code> composed with that adds the pair of results
* <code>sqrt</code> takes the square root of the sum
* <code>fleq/.5</code> is floating point comparison with a fixed right side of <code>.5</code>, returning true if its argument is greater or equal
* Everything from <code>fleq</code> to <code>rand</code> forms the predicate for the <code>?</code> conditional operator.
* If the condition is true, the identity function is applied, <code>~&</code>
* If the condition is false, the <code>plus/1.</code> function is applied, which adds one to its argument.
* <code>rep"n"</code> applied to a function has the effect of composing that function with itself <code>"n"</code> times, with <code>"n"</code> in this case being the parameter to the <code>mcp</code> function.
* The function being repeated <code>"n"</code> times is applied to an argument of 0.
* A division of the result by the number <code>"n"</code> converted to a floating point value is performed by <code>div\float"n"</code>.
* The result of the division is quadrupled by <code>times/4.</code>.
test program:

```Ursala
#cast %eL

pis = mcp* <10,100,1000,10000,100000,1000000>
```

{{out}}

```txt
<
   2.800000e+00,
   3.600000e+00,
   3.164000e+00,
   3.118800e+00,
   3.144480e+00,
   3.141668e+00>
```



## XPL0


```XPL0
code Ran=1, CrLf=9;
code real RlOut=48;

func real MontePi(N);   \Calculate pi using Monte Carlo method
int  N;                 \number of randomly selected points
int  I, X, Y, C;
def  R = 10000;         \radius of circle
[C:= 0;                 \initialize count of points in circle
for I:= 0 to N-1 do
        [X:= Ran(R);
         Y:= Ran(R);
        if X*X + Y*Y <= R*R then C:= C+1;
        ];
return float(C)*4.0 / float(N);   \Acir/Asqr = pi*R^2/4*R^2 = pi/4
];

[RlOut(0, MontePi(        100));  CrLf(0);
 RlOut(0, MontePi(     10_000));  CrLf(0);
 RlOut(0, MontePi(  1_000_000));  CrLf(0);
 RlOut(0, MontePi(100_000_000));  CrLf(0);
]
```


{{out}}

```txt

    2.92000
    3.13200
    3.14375
    3.14192

```



## zkl


```zkl
fcn monty(n){
   inCircle:=0; 
   do(n){
      x:=(0.0).random(1); y:=(0.0).random(1);
      if(x*x + y*y < 1.0) inCircle+=1;
   }
   4.0*inCircle/n
}
```

Or, in a more functional style (using a reference for state info):

```zkl
fcn monty(n){
   4.0 * (1).pump(n,Void,fcn(r){
      x:=(0.0).random(1); y:=(0.0).random(1);
      if(x*x + y*y < 1.0) r.inc(); 
      r
   }.fp(Ref(0)) ).value/n;
}
```
 
{{out}}

```txt

T(100,1000,10000,0d100_000,0d1_000_000,0d10_000_000)
   .apply2(fcn(n){"%10,d : %+f".fmt(n,monty(n)-(1.0).pi).println()})
       100 : -0.061593
     1,000 : +0.018407
    10,000 : -0.013993
   100,000 : -0.000833
 1,000,000 : -0.004385
10,000,000 : +0.000619

```

