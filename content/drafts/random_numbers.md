+++
title = "Random numbers"
description = ""
date = 2019-10-18T11:54:16Z
aliases = []
[extra]
id = 2024
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Probability and statistics]]
[[Category:Randomness]]
{{omit from|GUISS}}
{{omit from|UNIX Shell|From the shell, we simply invoke the awk solution}}

;Task:
Generate a collection filled with   '''1000'''   normally distributed random (or pseudo-random) numbers
with a mean of   '''1.0'''   and a   [[wp:Standard_deviation|standard deviation]]   of   '''0.5'''


Many libraries only generate uniformly distributed random numbers.

If so, use [[wp:Normal_distribution#Generating_values_from_normal_distribution|this formula]] to convert them to a normal distribution.


;Related task:
*   [[Standard deviation]]





## Ada


```ada
with Ada.Numerics;                       use Ada.Numerics;
with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;

procedure Normal_Random is
   function Normal_Distribution
            (  Seed  : Generator;
               Mu    : Float := 1.0;
               Sigma : Float := 0.5
            )  return Float is
   begin
      return
         Mu + (Sigma * Sqrt (-2.0 * Log (Random (Seed), 10.0)) * Cos (2.0 * Pi * Random (Seed)));
   end Normal_Distribution;

   Seed         : Generator;
   Distribution : array (1..1_000) of Float;
begin
   Reset (Seed);
   for I in Distribution'Range loop
      Distribution (I) := Normal_Distribution (Seed);
   end loop;
end Normal_Random;
```


## ALGOL 68

{{trans|C}}

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
PROC random normal = REAL:  # normal distribution, centered on 0, std dev 1 #
(
  sqrt(-2*log(random)) * cos(2*pi*random)
);

test:(
  [1000]REAL rands;
  FOR i TO UPB rands DO
    rands[i] := 1 + random normal/2
  OD;
  INT limit=10;
  printf(($"("n(limit-1)(-d.6d",")-d.5d" ... )"$, rands[:limit]))
)
```

{{out}}

```txt

( 0.693461, 0.948424, 0.482261, 1.045939, 0.890818, 1.467935, 0.604153, 0.804811, 0.690227, 0.83462 ... )

```



## AutoHotkey

contributed by Laszlo on the ahk
[http://www.autohotkey.com/forum/post-276261.html#276261 forum]

```AutoHotkey
Loop 40
   R .= RandN(1,0.5) "`n"  ; mean = 1.0, standard deviation = 0.5
MsgBox %R%

RandN(m,s) { ; Normally distributed random numbers of mean = m, std.dev = s by Box-Muller method
   Static i, Y
   If (i := !i) { ; every other call
      Random U, 0, 1.0
      Random V, 0, 6.2831853071795862
      U := sqrt(-2*ln(U))*s
      Y := m + U*sin(V)
      Return m + U*cos(V)
   }
   Return Y
}
```



## AWK

'''One-liner:'''

```awk
$ awk 'func r(){return sqrt(-2*log(rand()))*cos(6.2831853*rand())}BEGIN{for(i=0;i<1000;i++)s=s" "1+0.5*r();print s}'
```


'''Readable version:'''

```awk

function r() {
  return sqrt( -2*log( rand() ) ) * cos(6.2831853*rand() )
}

BEGIN {
  n=1000
  for(i=0;i<n;i++) {
    x = 1 + 0.5*r()
    s = s" "x
  }
  print s
}

```

{{out}} first few values only

```txt

0.783753 1.16682 1.17989 1.14975 1.34784 0.29296 0.979227 1.04402 0.567835 1.58812 0.465559 1.27186 0.324533 0.725827 -0.0626549 0.632273 1.0145 1.3387 0.861667 1.04147 1.2576 1.02497 0.58453 0.9619 1.26902 0.851048 -0.126259 0.863256
```

...


## BASIC

{{works with|QuickBasic|4.5}}
 RANDOMIZE TIMER 'seeds random number generator with the system time
 pi = 3.141592653589793#
 DIM a(1 TO 1000) AS DOUBLE
 CLS
 FOR i = 1 TO 1000
    a(i) = 1 + SQR(-2 * LOG(RND)) * COS(2 * pi * RND)
 NEXT i


## BBC BASIC


```bbcbasic
      DIM array(999)
      FOR number% = 0 TO 999
        array(number%) = 1.0 + 0.5 * SQR(-2*LN(RND(1))) * COS(2*PI*RND(1))
      NEXT

      mean = SUM(array()) / (DIM(array(),1) + 1)
      array() -= mean
      stdev = MOD(array()) / SQR(DIM(array(),1) + 1)

      PRINT "Mean = " ; mean
      PRINT "Standard deviation = " ; stdev
```

{{out}}

```txt
Mean = 1.01848064
Standard deviation = 0.503551814
```



## C


```cpp
#include <iostream>
#include <math.h>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

double drand()   /* uniform distribution, (0..1] */
{
  return (rand()+1.0)/(RAND_MAX+1.0);
}
double random_normal()  /* normal distribution, centered on 0, std dev 1 */
{
  return sqrt(-2*log(drand())) * cos(2*M_PI*drand());
}
int main()
{
  int i;
  double rands[1000];
  for (i=0; i<1000; i++)
    rands[i] = 1.0 + 0.5*random_normal();
  return 0;
}
```



## C#

{{trans|JavaScript}}

```c#

private static double randomNormal()
{
	return Math.Cos(2 * Math.PI * tRand.NextDouble()) * Math.Sqrt(-2 * Math.Log(tRand.NextDouble()));
}

```


Then the methods in [[Random numbers#Metafont]] are used to calculate the average and the Standard Deviation:

```c#

static Random tRand = new Random();

static void Main(string[] args)
{
	double[] a = new double[1000];

	double tAvg = 0;
	for (int x = 0; x < a.Length; x++)
	{
		a[x] = randomNormal() / 2 + 1;
		tAvg += a[x];
	}

	tAvg /= a.Length;
	Console.WriteLine("Average: " + tAvg.ToString());

	double s = 0;
	for (int x = 0; x < a.Length; x++)
	{
		s += Math.Pow((a[x] - tAvg), 2);
	}
	s = Math.Sqrt(s / 1000);

	Console.WriteLine("Standard Deviation: " + s.ToString());

	Console.ReadLine();
}

```


An example result:

```txt

Average: 1,00510073053613
Standard Deviation: 0,502540443430955

```



## C++

{{works with|C++11}}

The new C++ standard looks very similar to the Boost library example below.


```cpp
#include <random>
#include <functional>
#include <vector>
#include <algorithm>
using namespace std;

int main()
{
  random_device seed;
  mt19937 engine(seed());
  normal_distribution<double> dist(1.0, 0.5);
  auto rnd = bind(dist, engine);

  vector<double> v(1000);
  generate(v.begin(), v.end(), rnd);
  return 0;
}
```


{{works with|C++03}}

```cpp
#include <cstdlib>   // for rand
#include <cmath>     // for atan, sqrt, log, cos
#include <algorithm> // for generate_n

double const pi = 4*std::atan(1.0);

// simple functor for normal distribution
class normal_distribution
{
public:
  normal_distribution(double m, double s): mu(m), sigma(s) {}
  double operator() const // returns a single normally distributed number
  {
    double r1 = (std::rand() + 1.0)/(RAND_MAX + 1.0); // gives equal distribution in (0, 1]
    double r2 = (std::rand() + 1.0)/(RAND_MAX + 1.0);
    return mu + sigma * std::sqrt(-2*std::log(r1))*std::cos(2*pi*r2);
  }
private:
  const double mu, sigma;
};

int main()
{
  double array[1000];
  std::generate_n(array, 1000, normal_distribution(1.0, 0.5));
  return 0;
}
```


{{libheader|Boost}}

This example used Mersenne Twister generator. It can be changed by changing the typedef.


```cpp

#include <vector>
#include "boost/random.hpp"
#include "boost/generator_iterator.hpp"
#include <boost/random/normal_distribution.hpp>
#include <algorithm>

typedef boost::mt19937 RNGType; ///< mersenne twister generator

int main() {
    RNGType rng;
    boost::normal_distribution<> rdist(1.0,0.5); /**< normal distribution
                           with mean of 1.0 and standard deviation of 0.5 */

    boost::variate_generator< RNGType, boost::normal_distribution<> >
                    get_rand(rng, rdist);

    std::vector<double> v(1000);
    generate(v.begin(),v.end(),get_rand);
    return 0;
}

```



## Clojure


```lisp
(import '(java.util Random))
(def normals
  (let [r (Random.)]
    (take 1000 (repeatedly #(-> r .nextGaussian (* 0.5) (+ 1.0))))))
```



## Common Lisp


```lisp
(loop for i from 1 to 1000
      collect (1+ (* (sqrt (* -2 (log (random 1.0)))) (cos (* 2 pi (random 1.0))) 0.5)))
```



## D


```d
import std.stdio, std.random, std.math;

struct NormalRandom {
    double mean, stdDev;

    // Necessary because it also defines an opCall.
    this(in double mean_, in double stdDev_) pure nothrow {
        this.mean = mean_;
        this.stdDev = stdDev_;
    }

    double opCall() const /*nothrow*/ {
        immutable r1 = uniform01, r2 = uniform01; // Not nothrow.
        return mean + stdDev * sqrt(-2 * r1.log) * cos(2 * PI * r2);
    }
}

void main() {
    double[1000] array;
    auto nRnd = NormalRandom(1.0, 0.5);
    foreach (ref x; array)
        //x = nRnd;
        x = nRnd();
}
```



### Alternative Version

(Untested)
{{libheader|tango}}


```d
import tango.math.random.Random;

void main() {
    double[1000] list;
    auto r = new Random();
    foreach (ref l; list) {
        r.normalSource!(double)()(l);
        l = 1.0 + 0.5 * l;
    }
}
```



## Delphi


Delphi has RandG function which generates random numbers with normal distribution using Marsaglia-Bray algorithm:


```Delphi
program Randoms;

{$APPTYPE CONSOLE}

uses
  Math;

var
  Values: array[0..999] of Double;
  I: Integer;

begin
//  Randomize;   Commented to obtain reproducible results
  for I:= Low(Values) to High(Values) do
    Values[I]:= RandG(1.0, 0.5);  // Mean = 1.0, StdDev = 0.5
  Writeln('Mean          = ', Mean(Values):6:4);
  Writeln('Std Deviation = ', StdDev(Values):6:4);
  Readln;
end.
```

{{out}}

```txt
Mean          = 1.0098
Std deviation = 0.5016
```



## DWScript


```delphi
var values : array [0..999] of Float;
var i : Integer;

for i := values.Low to values.High do
   values[i] := RandG(1, 0.5);
```



## E


```e
accum [] for _ in 1..1000 { _.with(entropy.nextGaussian()) }
```



## EasyLang


<lang>floatvars
len a[] 1000
for i% range len a[]
  a[i%] = 1 + 0.5 * sqrt (-2 * logn randomf) * cos (360 * randomf)
.
```



## Eiffel


```eiffel

class
	APPLICATION

inherit
	ARGUMENTS

create
	make

feature {NONE} -- Initialization

	l_time: TIME
	l_seed: INTEGER
	math:DOUBLE_MATH
	rnd:RANDOM
	Size:INTEGER
		once
			Result:= 1000
		end

	make
			-- Run application.
		local
			ergebnis:ARRAY[DOUBLE]
			tavg: DOUBLE
			x: INTEGER
			tmp: DOUBLE
			text : STRING

		do
			-- initialize random generator
			create l_time.make_now
     		        l_seed := l_time.hour
      		        l_seed := l_seed * 60 + l_time.minute
      		        l_seed := l_seed * 60 + l_time.second
      		        l_seed := l_seed * 1000 + l_time.milli_second
      		        create rnd.set_seed (l_seed)

			-- initialize random number container and math
			create ergebnis.make_filled (0.0, 1, size)
			tavg := 0;
			create math

			from
				x := 1
			until
				x > ergebnis.count
			loop
				tmp := randomNormal / 2 + 1
				tavg := tavg + tmp
				ergebnis.enter (tmp , x)
				x := x + 1
			end

			tavg := tavg / ergebnis.count
			text := "Average: "
			text.append_double (tavg)
			text.append ("%N")
			print(text)

			tmp := 0
			from
				x:= 1
			until
				x > ergebnis.count
			loop
				tmp := tmp + (ergebnis.item (x) - tavg)^2
				x := x + 1
			end

			tmp := math.sqrt (tmp / ergebnis.count)
			text := "Standard Deviation: "
			text.append_double (tmp)
			text.append ("%N")
			print(text)

		end

	randomNormal:DOUBLE

		local

      		        first: DOUBLE
      		        second: DOUBLE

		do
                        rnd.forth
                        first := rnd.double_item
                        rnd.forth
                        second := rnd.double_item

                        Result := math.cosine (2 * math.pi * first) * math.sqrt (-2 * math.log (second))

		end
end

```


Example Result

```txt

Average: 1.0079398405028137
Standard Deviation: 0.49042787564453988

```


## Elena

{{trans|C#}}
ELENA 4.1 :

```elena
import extensions;
import extensions'math;

randomNormal()
{
    ^ cos(2 * Pi_value * randomGenerator.nextReal())
                      * sqrt(-2 * ln(randomGenerator.nextReal()))
}

public program()
{
    real[] a := new real[](1000);

    real tAvg := 0;
    for (int x := 0, x < a.Length, x += 1)
    {
        a[x] := (randomNormal()) / 2 + 1;
        tAvg += a[x]
    };

    tAvg /= a.Length;
    console.printLine("Average: ", tAvg);

    real s := 0;
    for (int x := 0, x < a.Length, x += 1)
    {
        s += power(a[x] - tAvg, 2)
    };

    s := sqrt(s / 1000);

    console.printLine("Standard Deviation: ", s);

    console.readChar()
}
```

{{out}}

```txt

Average: 0.9842420481571
Standard Deviation: 0.5109070975558

```



## Elixir


```elixir
defmodule Random do
  def normal(mean, sd) do
    {a, b} = {:rand.uniform, :rand.uniform}
    mean + sd * (:math.sqrt(-2 * :math.log(a)) * :math.cos(2 * :math.pi * b))
  end
end

std_dev = fn (list) ->
            mean = Enum.sum(list) / length(list)
            sd = Enum.reduce(list, 0, fn x,acc -> acc + (x-mean)*(x-mean) end) / length(list)
                 |> :math.sqrt
            IO.puts "Mean: #{mean},\tStdDev: #{sd}"
          end

xs = for _ <- 1..1000, do: Random.normal(1.0, 0.5)
std_dev.(xs)
```

{{out}}

```txt

Mean: 1.009079383094275,        StdDev: 0.4991894476975088

```


used Erlang function <code>:rand.normal</code>

```elixir
xs = for _ <- 1..1000, do: 1.0 + :rand.normal * 0.5
std_dev.(xs)
```

{{out}}

```txt

Mean: 0.9955701150615597,       StdDev: 0.5036412260426065

```



## Erlang

{{works with|Erlang}}


```erlang

mean(Values) ->
    mean(tl(Values), hd(Values), 1).

mean([], Acc, Length) ->
    Acc / Length;
mean(Values, Acc, Length) ->
    mean(tl(Values), hd(Values)+Acc, Length+1).

variance(Values) ->
    Mean = mean(Values),
    variance(Values, Mean, 0) / length(Values).

variance([], _, Acc) ->
    Acc;
variance(Values, Mean, Acc) ->
    Diff = hd(Values) - Mean,
    DiffSqr = Diff * Diff,
    variance(tl(Values), Mean, Acc + DiffSqr).

stddev(Values) ->
    math:sqrt(variance(Values)).

normal(Mean, StdDev) ->
    U = random:uniform(),
    V = random:uniform(),
    Mean + StdDev * ( math:sqrt(-2 * math:log(U)) * math:cos(2 * math:pi() * V) ).  % Erlang's math:log is the natural logarithm.

main(_) ->
    X = [ normal(1.0, 0.5) || _ <- lists:seq(1, 1000) ],
    io:format("mean = ~w\n", [mean(X)]),
    io:format("stddev = ~w\n", [stddev(X)]).

```

{{out}}

```txt

mean = 1.0118289913718608
stddev = 0.5021636849524854

```



## ERRE

<lang>
PROGRAM DISTRIBUTION

!
! for rosettacode.org
!

! formulas taken from TI-59 Master Library manual

CONST NUM_ITEM=1000

!VAR SUMX#,SUMX2#,R1#,R2#,Z#,I%

DIM A#[1000]

BEGIN
! seeds random number generator with system time
   RANDOMIZE(TIMER)

   PRINT(CHR$(12);)  !CLS
   SUMX#=0  SUMX2#=0

   FOR I%=1 TO NUM_ITEM DO
      R1#=RND(1)  R2#=RND(1)
      Z#=SQR(-2*LOG(R1#))*COS(2*π*R2#)
      A#[I%]=Z#/2+1   ! I want a normal distribution with
                      !      mean=1 and std.dev=0.5
      SUMX#+=A#[I%]  SUMX2#+=A#[I%]*A#[I%]
   END FOR

   Z#=SUMX#/NUM_ITEM

   PRINT("Average is";Z#)
   PRINT("Standard dev. is";SQR(SUMX2#/NUM_ITEM-Z#*Z#))

END PROGRAM

```



## Euler Math Toolbox



```Euler Math Toolbox

>v=normal(1,1000)*0.5+1;
>mean(v), dev(v)
 1.00291801071
 0.498226876528

```



## Euphoria

{{trans|PureBasic}}

```euphoria
include misc.e

function RandomNormal()
    atom x1, x2
    x1 = rand(999999) / 1000000
    x2 = rand(999999) / 1000000
    return sqrt(-2*log(x1)) * cos(2*PI*x2)
end function

constant n = 1000
sequence s
s = repeat(0,n)
for i = 1 to n do
    s[i] = 1 + 0.5 * RandomNormal()
end for
```



=={{header|F_Sharp|F#}}==

```fsharp

let n = MathNet.Numerics.Distributions.Normal(1.0,0.5)
List.init 1000 (fun _->n.Sample())

```

{{out}}

```txt

  [0.734433576; 1.54225304; 0.4407528678; 1.177675412; 0.4318617021;
   0.6026656337; 0.769764924; 1.104693934; 0.6297500925; 0.9594598077;
   1.684736389; 1.160376323; 0.883354356; 0.9513968363; 0.9727698268;
   0.5315570949; 0.9599239266; 1.564976755; 0.7232002879; 1.084139442;
   1.220914517; 0.3553085946; 1.112549824; 1.989443553; 0.5752307543;
   1.156682549; 0.7886670467; 0.02050745923; 1.532060208; 1.18789591;
   1.408946777; 1.038812004; 1.724679503; 1.671565045; 1.266831442;
   1.363611654; 1.705819067; 0.5772366328; 0.4503488498; 1.496891481;
   0.9831877282; 0.3845460366; 0.8253240671; 1.298969969; 0.4265904553;
   0.9303696876; 0.445003361; 0.753175816; 0.6143534043; 1.059982235;
   0.7143206784; 0.2233328038; 1.005178481; 0.7697392436; 0.5904948577;
   0.5127953044; 0.6467346747; 0.7929387604; -0.1501790761; 0.8750780903;
   0.941704369; 1.37941579; 0.4739006145; 1.998886344; 1.219428519;
   0.06270791476; 1.097739804; 0.7584232803; 1.042177217; 1.166561247;
   1.502357164; 1.171525776; 0.1528807432; 0.2289389756; 1.36208422;
   0.3714421124; 1.299571092; 1.171553369; 1.317807265; 1.616662281;
   1.724223246; 1.059580642; 1.270520918; -0.1827677907; 1.938593232;
   1.420362143; 1.888357595; 0.7851629936; 0.7080554899; 0.7747215818;
   1.403719877; 0.5765950249; 1.275206565; 0.6292054813; 1.525562798;
   0.6224640457; 0.8524078517; 0.7646595627; 0.6799834691; 0.773111053; ...]

```


## Factor


```factor
USING: random ;
1000 [ 1.0 0.5 normal-random-float ] replicate
```


=={{header|Falcon|}}==

```falcon
a = []
for i in [0:1000] : a+= norm_rand_num()

function norm_rand_num()
   pi = 2*acos(0)
   return 1 + (cos(2 * pi * random()) * pow(-2 * log(random()) ,1/2)) /2
end
```



## Fantom


Two solutions.  The first uses Fantom's random-number generator, which produces a uniform distribution.  So, convert to a normal distribution using a formula:


```fantom

class Main
{
  static const Float PI := 0.0f.acos * 2  // we need to precompute PI

  static Float randomNormal ()
  {
    return (Float.random * PI * 2).cos * (Float.random.log * -2).sqrt
  }

  public static Void main ()
  {
    mean := 1.0f
    sd := 0.5f
    Float[] values := [,] // this is the collection to fill with random numbers
    1000.times { values.add (randomNormal * sd + mean) }
  }
}

```


The second calls out to Java's Gaussian random-number generator:


```fantom

using [java] java.util::Random

class Main
{
  Random generator := Random()

  Float randomNormal ()
  {
    return generator.nextGaussian
  }

  public static Void main ()
  {
    rnd := Main()  // create an instance of Main class, which holds the generator
    mean := 1.0f
    sd := 0.5f
    Float[] values := [,] // this is the collection to fill with random numbers
    1000.times { values.add (rnd.randomNormal * sd + mean) }
  }
}

```



## Forth

{{works with|gforth|0.6.2}}


```forth
require random.fs
here to seed

-1. 1 rshift 2constant MAX-D	\ or s" MAX-D" ENVIRONMENT? drop

: frnd ( -- f )			\ uniform distribution 0..1
  rnd rnd dabs d>f MAX-D d>f f/ ;

: frnd-normal ( -- f )		\ centered on 0, std dev 1
  frnd pi f* 2e f* fcos
  frnd fln -2e f* fsqrt f* ;

: ,normals ( n -- )		\ store many, centered on 1, std dev 0.5
  0 do frnd-normal 0.5e f* 1e f+ f, loop ;

create rnd-array 1000 ,normals
```


For newer versions of gforth (tested on 0.7.3), it seems you need to use <tt>HERE SEED !</tt> instead of <tt>HERE TO SEED</tt>, because <tt>SEED</tt> has been made a variable instead of a value.


## Fortran

{{works with|Fortran|90 and later}}

```fortran
PROGRAM Random

  INTEGER, PARAMETER :: n = 1000
  INTEGER :: i
  REAL :: array(n), pi, temp, mean = 1.0, sd = 0.5

  pi = 4.0*ATAN(1.0)
  CALL RANDOM_NUMBER(array) ! Uniform distribution

! Now convert to normal distribution
  DO i = 1, n-1, 2
    temp = sd * SQRT(-2.0*LOG(array(i))) * COS(2*pi*array(i+1)) + mean
    array(i+1) = sd * SQRT(-2.0*LOG(array(i))) * SIN(2*pi*array(i+1)) + mean
    array(i) = temp
  END DO

! Check mean and standard deviation
  mean = SUM(array)/n
  sd = SQRT(SUM((array - mean)**2)/n)

  WRITE(*, "(A,F8.6)") "Mean = ", mean
  WRITE(*, "(A,F8.6)") "Standard Deviation = ", sd

END PROGRAM Random
```


{{out}}

```txt

 Mean = 0.995112
 Standard Deviation = 0.503373

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Const pi As Double = 3.141592653589793
Randomize

' Generates normally distributed random numbers with mean 0 and standard deviation 1
Function randomNormal() As Double
  Return Cos(2.0 * pi * Rnd) * Sqr(-2.0 * Log(Rnd))
End Function

Dim r(0 To 999) As Double
Dim sum As Double = 0.0

' Generate 1000 normally distributed random numbers
' with mean 1 and standard deviation 0.5
' and calculate their sum
For i As Integer = 0 To 999
   r(i) = 1.0 + randomNormal/2.0
   sum += r(i)
Next

Dim mean As Double = sum / 1000.0

Dim sd As Double
sum = 0.0
' Now calculate their standard deviation
For i As Integer = 0 To 999
  sum += (r(i) - mean) ^ 2.0
Next
sd  = Sqr(sum/1000.0)

Print "Mean is              "; mean
Print "Standard Deviation is"; sd
Print
Print "Press any key to quit"
Sleep
```

Sample result:
{{out}}

```txt

Mean is               1.000763573902885
Standard Deviation is 0.500653063426955

```



## Free Pascal

Free Pascal provides the '''randg''' function in the RTL math unit that produces Gaussian-distributed random numbers with the Box-Müller algorithm.


```pascal

function randg(mean,stddev: float): float;

```


=={{header|F_Sharp|F#}}==

```fsharp
let gaussianRand count =
    let o = new System.Random()
    let pi = System.Math.PI
    let gaussrnd =
        (fun _ -> 1. + 0.5 * sqrt(-2. * log(o.NextDouble())) * cos(2. * pi * o.NextDouble()))
    [ for i in {0 .. (int count)} -> gaussrnd() ]
```



## Go

This solution uses math/rand package in the standard library.  See also though the subrepository rand package at https://godoc.org/golang.org/x/exp/rand, which also has a NormFloat64 and has a rand source with a number of advantages over the one in standard library.

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "strings"
    "time"
)

const mean = 1.0
const stdv = .5
const n = 1000

func main() {
    var list [n]float64
    rand.Seed(time.Now().UnixNano())
    for i := range list {
        list[i] = mean + stdv*rand.NormFloat64()
    }
    // show computed mean and stdv of list
    var s, sq float64
    for _, v := range list {
        s += v
    }
    cm := s / n
    for _, v := range list {
        d := v - cm
        sq += d * d
    }
    fmt.Printf("mean %.3f, stdv %.3f\n", cm, math.Sqrt(sq/(n-1)))
    // show histogram by hdiv divisions per stdv over +/-hrange stdv
    const hdiv = 3
    const hrange = 2
    var h [1 + 2*hrange*hdiv]int
    for _, v := range list {
        bin := hrange*hdiv + int(math.Floor((v-mean)/stdv*hdiv+.5))
        if bin >= 0 && bin < len(h) {
            h[bin]++
        }
    }
    const hscale = 10
    for _, c := range h {
        fmt.Println(strings.Repeat("*", (c+hscale/2)/hscale))
    }
}
```

{{out}}

```txt

mean 0.995, stdv 0.503
**
****
******
********
************
************
*************
************
**********
********
*****
***
**

```



## FutureBasic

Note: To generate the random number, rather than using FB's native "rnd" function, this code wraps C code into the RandomZeroToOne function.

```futurebasic

include "ConsoleWindow"

local fn RandomZeroToOne as double
dim as double result
BeginCCode
  result = (double)( (rand() % 100000 ) * 0.00001 );
EndC
end fn = result

local fn RandomGaussian as double
dim as double r

r = fn RandomZeroToOne
end fn = 1 + .5 * ( sqr( -2 * log(r) ) * cos( 2 * pi * r ) )

dim as long i
dim as double mean, std, a(1000)

for i = 1 to 1000
  a(i) = fn RandomGaussian
  mean += a(i)
next
mean = mean / 1000

for i = 1 to 1000
  std += ( a(i) - mean )^2
next
std = std / 1000

print "           Average:"; mean
print "Standard Deviation:"; std

```

Output:

```txt

           Average: 1.0258434498
Standard Deviation: 0.2771047023

```



## Groovy


```groovy
rnd = new Random()
result = (1..1000).inject([]) { r, i -> r << rnd.nextGaussian() }
```



## Haskell



```haskell
import System.Random

pairs :: [a] -> [(a,a)]
pairs (x:y:zs) = (x,y):pairs zs
pairs _        = []

gauss mu sigma (r1,r2) =
  mu + sigma * sqrt (-2 * log r1) * cos (2 * pi * r2)

gaussians :: (RandomGen g, Random a, Floating a) => Int -> g -> [a]
gaussians n g = take n $ map (gauss 1.0 0.5) $ pairs $ randoms g

result :: IO [Double]
result = getStdGen >>= \g -> return $ gaussians 1000 g
```


Or using Data.Random from random-fu package:

```haskell
replicateM 1000 $ normal 1 0.5
```

To print them:

```haskell
import  Data.Random
import Control.Monad

thousandRandomNumbers :: RVar [Double]
thousandRandomNumbers =  replicateM 1000 $ normal 1 0.5

main = do
   x <- sample thousandRandomNumbers
   print x
```



## HicEst


```hicest
REAL :: n=1000, m=1, s=0.5, array(n)

pi = 4 * ATAN(1)
array = s * (-2*LOG(RAN(1)))^0.5  * COS(2*pi*RAN(1)) + m
```


=={{header|Icon}} and {{header|Unicon}}==
The seed '''&amp;random''' may be assigned in either language; either to randomly seed or to pick a fixed starting point. ?i is the random number generator, returning an integer from 0 to i - 1 for non-zero integer i. As a special case, ?0 yields a random floating point number from 0.0 <= r < 1.0

Note that Unicon randomly seeds it's generator.

```icon

procedure main()
    local L
    L := list(1000)
    every L[1 to 1000] := 1.0 + 0.5 * sqrt(-2.0 * log(?0)) * cos(2.0 * &pi * ?0)

    every write(!L)
end

```



## IDL


```idl
result = 1.0 + 0.5*randomn(seed,1000)
```



## J

'''Solution:'''

```j
urand=: ?@$ 0:
zrand=: (2 o. 2p1 * urand) * [: %: _2 * [: ^. urand

1 + 0.5 * zrand 100
```


'''Alternative Solution:'''

Using the normal script from the [[j:Addons/stats/distribs|stats/distribs addon]].

```j
   require 'stats/distribs/normal'
   1 0.5 rnorm 1000
1.44868803 1.21548637 0.812460657 1.54295452 1.2470606 ...
```



## Java


```java
double[] list = new double[1000];
double mean = 1.0, std = 0.5;
Random rng = new Random();
for(int i = 0;i<list.length;i++) {
  list[i] = mean + std * rng.nextGaussian();
}
```



## JavaScript


```javascript
function randomNormal() {
  return Math.cos(2 * Math.PI * Math.random()) * Math.sqrt(-2 * Math.log(Math.random()))
}

var a = []
for (var i=0; i < 1000; i++){
  a[i] = randomNormal() / 2 + 1
}
```



## jq

{{works with|jq|1.4}}

Since jq is a purely functional language, it is convenient
to define the pseudo-random number generator functions as filters
whose inputs and outputs are arrays containing a "seed".

The following uses the same pseudo-random number generator as the
Microsoft C Runtime (see [[Linear congruential generator]]).

''''A Pseudo-Random Number Generator''''

```jq
# 15-bit integers generated using the same formula as rand() from the Microsoft C Runtime.
# The random numbers are in [0 -- 32767] inclusive.
# Input: an array of length at least 2 interpreted as [count, state, ...]
# Output: [count+1, newstate, r] where r is the next pseudo-random number.
def next_rand_Microsoft:
  .[0] as $count | .[1] as $state
  | ( (214013 * $state) + 2531011) % 2147483648 # mod 2^31
  | [$count+1 , ., (. / 65536 | floor) ] ;
```

''''Box-Muller Method''''

```jq
# Generate a single number following the normal distribution with mean 0, variance 1,
# using the Box-Muller method: X = sqrt(-2 ln U) * cos(2 pi V) where U and V are uniform on [0,1].
# Input: [n, state]
# Output [n+1, nextstate, r]
def next_rand_normal:
  def u: next_rand_Microsoft | .[2] /= 32767;
  u as $u1
  | ($u1 | u) as $u2
  | ((( (8*(1|atan)) * $u1[2]) | cos)
     * ((-2 * (($u2[2]) | log)) | sqrt)) as $r
  | [ (.[0]+1), $u2[1], $r] ;

# Generate "count" arrays, each containing a random normal variate with the given mean and standard deviation.
# Input: [count, state]
# Output: [updatedcount, updatedstate, rnv]
# where "state" is a seed and "updatedstate" can be used as a seed.
def random_normal_variate(mean; sd; count):
  next_rand_normal
  | recurse( if .[0] < count then next_rand_normal else empty end)
  | .[2] = (.[2] * sd) + mean;
```

'''Example'''
The task can be completed using: [0,1] | random_normal_variate(1; 0.5; 1000) | .[2]

We show just the sample average and standard deviation:

```jq
def summary:
  length as $l | add as $sum | ($sum/$l) as $a
  | reduce .[] as $x (0; . + ( ($x - $a) | .*. ))
  | [ $a, (./$l | sqrt)] ;

[ [0,1] | random_normal_variate(1; 0.5; 1000) | .[2] ] | summary
```

{{out}}
 $ jq -n -c -f Random_numbers.jq
 [0.9932830741018853,0.4977760644490579]


## Julia

Julia's standard library provides a <code>randn</code> function to generate normally distributed random numbers (with mean 0 and standard deviation 0.5, which can be easily rescaled to any desired values):

```julia
randn(1000) * 0.5 + 1
```



## Kotlin


```scala
// version 1.0.6

import java.util.Random

fun main(args: Array<String>) {
    val r = Random()
    val da = DoubleArray(1000)
    for (i in 0 until 1000)  da[i] = 1.0 + 0.5 * r.nextGaussian()
    // now check actual mean and SD
    val mean = da.average()
    val sd = Math.sqrt(da.map { (it - mean) * (it - mean) }.average())
    println("Mean is $mean")
    println("S.D. is $sd")
}
```

Sample output:
{{out}}

```txt

Mean is 1.0071784073168768
S.D. is 0.48567118114896807

```



## LabVIEW

{{works with|LabVIEW|8.6}}
[[File:LV_array_of_randoms_with_given_mean_and_stdev.png]]


## Liberty BASIC


```lb
dim a(1000)
mean =1
sd =0.5
for i = 1 to 1000   '   throw 1000 normal variates
   a( i)  =mean +sd *( sqr( -2 * log( rnd( 0))) * cos( 2 * pi * rnd( 0)))
next i
```



## Logo

{{works with|UCB Logo}}
The earliest Logos only have a RANDOM function for picking a random non-negative integer. Many modern Logos have floating point random generators built-in.

```logo
to random.float   ; 0..1
  localmake "max.int lshift -1 -1
  output quotient random :max.int :max.int
end

to random.gaussian
  output product cos random 360  sqrt -2 / ln random.float
end

make "randoms cascade 1000 [fput random.gaussian / 2 + 1 ?] []
```



## Lingo


```Lingo
-- Returns a random float value in range 0..1
on randf ()
    n = random(the maxinteger)-1
    return n / float(the maxinteger-1)
end
```



```Lingo
normal = []
repeat with i = 1 to 1000
    normal.add(1 + sqrt(-2 * log(randf())) * cos(2 * PI * randf()) / 2)
end repeat
```



## Lua


```lua
local list = {}
for i = 1, 1000 do
  list[i] = 1 + math.sqrt(-2 * math.log(math.random())) * math.cos(2 * math.pi * math.random()) / 2
end
```



## M2000 Interpreter

M2000 use a Wichmann - Hill Pseudo Random Number Generator.

```M2000 Interpreter

Module CheckIt {
      Function StdDev (A()) {
          \\ A()  has a copy of values
            N=Len(A())
            if N<1 then Error "Empty Array"
            M=Each(A())
            k=0
            \\ make sum, dev same type as A(k)
            sum=A(k)-A(k)
            dev=sum
            \\ find mean
            While M {
                  sum+=Array(M)
            }
            Mean=sum/N
            \\ make a pointet to A()
            P=A()
            \\ subtruct from each item
            P-=Mean

            M=Each(P)
            While M {
                  dev+=Array(M)*Array(M)
            }
            \\ as pointer to arrray
             =(if(dev>0->Sqrt(dev/N), 0), Mean)
      }
      Function randomNormal {
            \\ by default all numbers are double
            \\ cos() get degrees
          =1+Cos(360 * rnd) * Sqrt(-2 * Ln(rnd)) /2
      }
      \\ fill array calling  randomNormal() for each item
      Dim A(1000)<<randomNormal()
      \\ we can pass a pointer to array and place it to stack of values
      DisplayMeanAndStdDeviation(A())  ' mean ~ 1 std deviation ~0.5
      \\ check M2000 rnd only
      Dim B(1000)<<rnd
      DisplayMeanAndStdDeviation(B())  ' mean ~ 0.5 std deviation ~0.28


      DisplayMeanAndStdDeviation((0,0,14,14))  ' mean = 7 std deviation = 7
      DisplayMeanAndStdDeviation((0,6,8,14))  ' mean = 7 std deviation = 5
      DisplayMeanAndStdDeviation((6,6,8,8))  ' mean = 7 std deviation = 1

      Sub DisplayMeanAndStdDeviation(A)
            \\ push to stack all items of an array (need an array pointer)
            Push  ! StdDev(A)
            \\ read from strack two numbers
            Print "Mean is               "; Number
            Print "Standard Deviation is "; Number
      End Sub
}
Checkit

```



## Maple


```maple
with(Statistics):
Sample(Normal(1, 0.5), 1000);
```



## Mathematica

Built-in function RandomReal with built-in distribution NormalDistribution as an argument:

```Mathematica
RandomReal[NormalDistribution[1, 1/2], 1000]
```



## MATLAB


Native support :

```MATLAB
    mu = 1; sd = 0.5;
    x = randn(1000,1) * sd + mu;

```


The statistics toolbox provides this function

```MATLAB
   x = normrnd(mu, sd, [1000,1]);
```


This script uses the Box-Mueller Transform to transform a number from the uniform distribution to a normal distribution of mean = mu0 and standard deviation = chi2.


```MATLAB
function randNum = randNorm(mu0,chi2, sz)

    radiusSquared = +Inf;

    while (radiusSquared >= 1)
        u = ( 2 * rand(sz) ) - 1;
        v = ( 2 * rand(sz) ) - 1;

        radiusSquared = u.^2 + v.^2;
    end

    scaleFactor = sqrt( ( -2*log(radiusSquared) )./ radiusSquared );
    randNum = (v .* scaleFactor .* chi2) + mu0;

end
```


Output:

```MATLAB>>
 randNorm(1,.5, [1000,1])

ans =

   0.693984121077029
```



## Maxima


```maxima
load(distrib)$

random_normal(1.0, 0.5, 1000);
```



## MAXScript



```maxscript
arr = #()
for i in 1 to 1000 do
(
    a = random 0.0 1.0
    b = random 0.0 1.0
    c = 1.0 + 0.5 * sqrt (-2*log a) * cos (360*b) -- Maxscript cos takes degrees
    append arr c
)
```



## Metafont


Metafont has <code>normaldeviate</code> which produces pseudorandom normal distributed numbers with mean 0 and variance one. So the following complete the task:


```metafont
numeric col[];

m := 0;               % m holds the mean, for testing purposes
for i = 1 upto 1000:
  col[i] := 1 + .5normaldeviate;
  m := m + col[i];
endfor

% testing
m := m / 1000;       % finalize the computation of the mean

s := 0;              % in s we compute the standard deviation
for i = 1 upto 1000:
  s := s + (col[i] - m)**2;
endfor
s := sqrt(s / 1000);

show m, s;    % and let's show that really they get what we wanted
end
```


A run gave


```txt
>> 0.99947
>> 0.50533
```


Assigning a value to the special variable '''randomseed''' will allow to have always
the same sequence of pseudorandom numbers


## Mirah


```mirah
import java.util.Random

list = double[999]
mean = 1.0
std = 0.5
rng = Random.new
0.upto(998) do | i |
    list[i] = mean + std * rng.nextGaussian
end

```


=={{header|MK-61/52}}==
<lang>П7	<->	П8	1/x	П6	ИП6	П9	СЧ	П6	1/x
ln	ИП8	*	2	*	КвКор	ИП9	2	*	пи
*	sin	*	ИП7	+	С/П	БП	05
```


''Input'': РY - variance, РX - expectation.

Or:

<lang>3	10^x	П0	ПП	13	2	/	1	+	С/П	L0	03	С/П
СЧ	lg	2	/-/	*	КвКор	2	пи	^	СЧ	*	*	cos	*	В/О
```


to generate 1000 numbers with a mean of 1.0 and a standard deviation of 0.5.

=={{header|Modula-3}}==
{{trans|C}}


```modula3
MODULE Rand EXPORTS Main;

IMPORT Random;
FROM Math IMPORT log, cos, sqrt, Pi;

VAR rands: ARRAY [1..1000] OF LONGREAL;

(* Normal distribution. *)
PROCEDURE RandNorm(): LONGREAL =
  BEGIN
    WITH rand = NEW(Random.Default).init() DO
      RETURN
        sqrt(-2.0D0 * log(rand.longreal())) * cos(2.0D0 * Pi * rand.longreal());
    END;
  END RandNorm;

BEGIN
  FOR i := FIRST(rands) TO LAST(rands) DO
    rands[i] := 1.0D0 + 0.5D0 * RandNorm();
  END;
END Rand.
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.math.BigDecimal
import java.math.MathContext

-- prologue
numeric digits 20

-- get input, set defaults
parse arg dp mu sigma ec .
if mu    = '' | mu    = '.' then mean             =  1.0; else mean             = mu
if sigma = '' | sigma = '.' then stdDeviation     =  0.5; else stdDeviation     = sigma
if dp    = '' | dp    = '.' then displayPrecision =    1; else displayPrecision = dp
if ec    = '' | ec    = '.' then elements         = 1000; else elements         = ec

-- set up
RNG = Random()
numberList = java.util.List
numberList = ArrayList()

-- generate list of random numbers
loop for elements
  rn = mean + stdDeviation * RNG.nextGaussian()
  numberList.add(BigDecimal(rn, MathContext.DECIMAL128))
  end

-- report
say "Mean:              " mean
say "Standard Deviation:" stdDeviation
say "Precision:         " displayPrecision
say
drawBellCurve(numberList, displayPrecision)

return

-- -----------------------------------------------------------------------------
method drawBellCurve(numberList = java.util.List, precision) static
  Collections.sort(numberList)
  val = BigDecimal
  lastN = ''
  nextN = ''
  loop val over numberList
    nextN = Rexx(val.toPlainString()).format(5, precision)
    select
      when lastN = '' then nop
      when lastN \= nextN then say lastN
      otherwise nop
      end
    say '*\-'
    lastN = nextN
    end val
  say lastN

  return

```

{{out}}

```txt

Mean:               1.0
Standard Deviation: 0.5
Precision:          1

*    2.7
**    2.5
*    2.4
***    2.3
*****    2.2
*******    2.1
*************    2.0
*************    1.9
*****************************    1.8
*************************    1.7
*************************************    1.6
******************************************************    1.5
********************************************    1.4
********************************************************************    1.3
*****************************************************************    1.2
**************************************************************************    1.1
*********************************************************************************************    1.0
*************************************************************    0.9
**********************************************************************    0.8
**************************************************************    0.7
***********************************************************************    0.6
**************************************************************    0.5
******************************************    0.4
*******************************    0.3
***************************    0.2
***************    0.1
*********    0.0
******   -0.1
***   -0.2
***   -0.3
*   -0.4
*   -0.6
**   -0.7

```



## NewLISP


```NewLISP
(normal 1 .5 1000)
```



## Nim


```nim
import math, strutils

const  precisn = 5
var rs: TRunningStat

proc normGauss: float {.inline.} = 1 + 0.76 * cos(2*PI*random(1.0)) * sqrt(-2*log10(random(1.0)))

randomize()

for j in 0..5:
   for i in 0..1000:
      rs.push(normGauss())
   echo("mean: ", $formatFloat(rs.mean,ffDecimal,precisn),
        " stdDev: ", $formatFloat(rs.standardDeviation(),ffDecimal,precisn))
```

{{out}}

```txt
mean: 1.01703 stdDev: 0.50324
mean: 1.01187 stdDev: 0.50060
mean: 1.00216 stdDev: 0.49969
mean: 1.00335 stdDev: 0.50184
mean: 1.00120 stdDev: 0.49830
mean: 1.00217 stdDev: 0.49911
```



## Objeck


```objeck
bundle Default {
  class RandomNumbers {
    function : Main(args : String[]) ~ Nil {
      rands := Float->New[1000];
      for(i := 0; i < rands->Size(); i += 1;) {
        rands[i] := 1.0 + 0.5 * RandomNormal();
      };

      each(i : rands) {
        rands[i]->PrintLine();
      };
    }

    function : native : RandomNormal() ~ Float {
      return (2 * Float->Pi() * Float->Random())->Cos() * (-2 * (Float->Random()->Log()))->SquareRoot();
    }
  }
}
```



## OCaml


```ocaml
let pi = 4. *. atan 1.;;
let random_gaussian () =
  1. +. sqrt (-2. *. log (Random.float 1.)) *. cos (2. *. pi *. Random.float 1.);;
let a = Array.init 1000 (fun _ -> random_gaussian ());;
```



## Octave


```octave
p = normrnd(1.0, 0.5, 1000, 1);
disp(mean(p));
disp(sqrt(sum((p - mean(p)).^2)/numel(p)));
```


{{out}}

```txt
1.0209
0.51048
```



## ooRexx

{{trans|REXX}}

### version 1


```oorexx
/*REXX pgm gens 1,000 normally distributed #s: mean=1, standard dev.=0.5*/
  pi=RxCalcPi()                     /* get value of pi                */
  Parse Arg n seed .                /* allow specification of N & seed*/
  If n==''|n==',' Then
    n=1000                          /* N  is the size of the array.   */
  If seed\=='' Then
    Call random,,seed               /* use seed for repeatable RANDOM#*/
  mean=1                            /* desired new mean (arith. avg.) */
  sd=1/2                            /* desired new standard deviation.*/
  Do g=1 For n                      /* generate N uniform random nums.*/
    n.g=random(0,1e5)/1e5           /* REXX gens uniform rand integers*/
    End

  Say '              old mean=' mean()
  Say 'old standard deviation=' stddev()
  Say
  Do j=1 To n-1 By 2
    m=j+1
                                    /*use Box-Muller method           */
    _=sd*RxCalcPower(-2*RxCalcLog(n.j),.5)*RxCalcCos(2*pi*n.m,,'R')+mean
    n.m=sd*RxCalcpower(-2*RxCalcLog(n.j),.5)*RxCalcSin(2*pi*n.m,,'R')+,
  mean                              /* rand # must be 0???1.          */
    n.j=_
    End                             /* j                              */
  Say '              new mean=' mean()
  Say 'new standard deviation=' stddev()
  Exit
mean:
  _=0
  Do k=1 For n
    _=_+n.k
    End
  Return _/n
stddev:
  _avg=mean()
  _=0
  Do k=1 For n
    _=_+(n.k-_avg)**2
    End
  Return RxCalcPower(_/n,.5)

:: requires rxmath library
```

{{out}}

```txt
              old mean= 0.49830002
old standard deviation= 0.283199568

              new mean= 1.00377404
new standard deviation= 0.501444536
```


### version 2

Using the nice function names in the algorithm.

```oorexx
/*REXX pgm gens 1,000 normally distributed #s: mean=1, standard dev.=0.5*/
  pi=RxCalcPi()                     /* get value of pi                */
  Parse Arg n seed .                /* allow specification of N & seed*/
  If n==''|n==',' Then
    n=1000                          /* N  is the size of the array.   */
  If seed\=='' Then
    Call random,,seed               /* use seed for repeatable RANDOM#*/
  mean=1                            /* desired new mean (arith. avg.) */
  sd=1/2                            /* desired new standard deviation.*/
  Do g=1 For n                      /* generate N uniform random nums.*/
    n.g=random(0,1e5)/1e5           /* REXX gens uniform rand integers*/
    End

  Say '              old mean=' mean()
  Say 'old standard deviation=' stddev()
  Say
  Do j=1 To n-1 By 2
    m=j+1
                                    /*use Box-Muller method           */
    _=sd*sqrt(-2*ln(n.j))*cos(2*pi*n.m)+mean
    n.m=sd*sqrt(-2*ln(n.j))*sin(2*pi*n.m)+mean
    n.j=_
    End
  Say '              new mean=' mean()
  Say 'new standard deviation=' stddev()
  Exit
mean:
  _=0
  Do k=1 For n
    _=_+n.k
    End
  Return _/n
stddev:
  _avg=mean()
  _=0
  Do k=1 For n
    _=_+(n.k-_avg)**2
    End
  Return sqrt(_/n)

sqrt: Return RxCalcSqrt(arg(1))
ln:   Return RxCalcLog(arg(1))
cos:  Return RxCalcCos(arg(1),,'R')
sin:  Return RxCalcSin(arg(1),,'R')

:: requires rxmath library
```



## PARI/GP


```parigp
rnormal()={
	my(pr=32*ceil(default(realprecision)*log(10)/log(4294967296)),u1=random(2^pr)*1.>>pr,u2=random(2^pr)*1.>>pr);
	sqrt(-2*log(u1))*cos(2*Pi*u1)
	\\ Could easily be extended with a second normal at very little cost.
};
vector(1000,unused,rnormal()/2+1)
```



## Pascal


The following function calculates Gaussian-distributed random numbers with the Box-Müller algorithm:

```pascal

function rnorm (mean, sd: real): real;
 {Calculates Gaussian random numbers according to the Box-Müller approach}
var
  u1, u2: real;
begin
  u1 := random;
  u2 := random;
  rnorm := mean * abs(1 + sqrt(-2 * (ln(u1))) * cos(2 * pi * u2) * sd);
end;

```


[[#Delphi | Delphi]] and [[#Free Pascal|Free Pascal]] support implement a '''randg''' function that delivers Gaussian-distributed random numbers.


## Perl


```perl
my $PI = 2 * atan2 1, 0;

my @nums = map {
    1 + 0.5 * sqrt(-2 * log rand) * cos(2 * $PI * rand)
} 1..1000;
```



## Perl 6

{{works with|Rakudo|#22 "Thousand Oaks"}}


```perl6
sub randnorm ($mean, $stddev) {
    $mean + $stddev * sqrt(-2 * log rand) * cos(2 * pi * rand)
}

my @nums = randnorm(1, 0.5) xx 1000;

# Checking
say my $mean = @nums R/ [+] @nums;
say my $stddev = sqrt $mean**2 R- @nums R/ [+] @nums X** 2;

```



## Phix

{{Trans|Euphoria}}

```Phix
function RandomNormal()
    return sqrt(-2*log(rnd())) * cos(2*PI*rnd())
end function

sequence s = repeat(0,1000)
for i=1 to length(s) do
    s[i] = 1 + 0.5 * RandomNormal()
end for
```



## PHP



```php
function random() {
    return mt_rand() / mt_getrandmax();
}

$pi 	= pi();          // Set PI

$a = array();
for ($i = 0; $i < 1000; $i++) {
    $a[$i] = 1.0 + ((sqrt(-2 * log(random())) * cos(2 * $pi * random())) * 0.5);

}
```



## PicoLisp

{{trans|C}}

```PicoLisp
(load "@lib/math.l")

(de randomNormal ()  # Normal distribution, centered on 0, std dev 1
   (*/
      (sqrt (* -2.0 (log (rand 0 1.0))))
      (cos (*/ 2.0 pi (rand 0 1.0) `(* 1.0 1.0)))
      1.0 ) )

(seed (time))                                      # Randomize

(let Result
   (make                                           # Build list
      (do 1000                                     # of 1000 elements
         (link (+ 1.0 (/ (randomNormal) 2))) ) )
   (for N (head 7 Result)                          # Print first 7 results
      (prin (format N *Scl) " ") ) )
```

{{out}}

```txt
1.500334 1.212931 1.095283 0.433122 0.459116 1.302446 0.402477
```



## PL/I


```PL/I

/* CONVERTED FROM WIKI FORTRAN */
Normal_Random: procedure options (main);
   declare (array(1000), pi, temp,
            mean initial (1.0), sd initial (0.5)) float (18);
   declare (i, n) fixed binary;

   n = hbound(array, 1);
   pi = 4.0*ATAN(1.0);
   array = random(); /* Uniform distribution */
   /* Now convert to normal distribution */
   DO i = 1 to n-1 by 2;
      temp = sd * SQRT(-2.0*LOG(array(i))) * COS(2*pi*array(i+1)) + mean;
      array(i+1) = sd * SQRT(-2.0*LOG(array(i))) * SIN(2*pi*array(i+1)) + mean;
      array(i) = temp;
   END;
   /* Check mean and standard deviation */
   mean = SUM(array)/n;
   sd = SQRT(SUM((array - mean)**2)/n);
   put skip edit ( "Mean = ", mean ) (a, F(18,16) );
   put skip edit ( "Standard Deviation = ", sd) (a, F(18,16));
END Normal_Random;

```

{{out}}

```txt

Mean = 1.0125630677913652  Standard Deviation = 0.5067289784535284
3 runs with different seeds to random():
Mean = 1.0008390411168471  Standard Deviation = 0.5095810511317908
Mean = 0.9754351286894838  Standard Deviation = 0.4804376530558166
Mean = 1.0177411222687990  Standard Deviation = 0.5165899662493400

```



## PL/SQL


```PL/SQL

DECLARE
  --The desired collection
  type t_coll is table of number index by binary_integer;
  l_coll t_coll;

  c_max pls_integer := 1000;
BEGIN
   FOR l_counter IN 1 .. c_max
   LOOP
      -- dbms_random.normal delivers normal distributed random numbers with a mean of 0 and a variance of 1
      -- We just adjust the values and get the desired result:
      l_coll(l_counter) := DBMS_RANDOM.normal * 0.5 + 1;
      DBMS_OUTPUT.put_line (l_coll(l_counter));
   END LOOP;
END;

```



## Pop11


```pop11
;;; Choose radians as arguments to trigonometic functions
true -> popradians;

;;; procedure generating standard normal distribution
define random_normal() -> result;
lvars r1 = random0(1.0), r2 = random0(1.0);
     cos(2*pi*r1)*sqrt(-2*log(r2)) -> result
enddefine;

lvars array, i;

;;; Put numbers on the stack
for i from 1 to 1000 do 1.0+0.5*random_normal() endfor;
;;; collect them into array
consvector(1000) -> array;
```



## PowerShell

Equation adapted from Liberty BASIC

```powershell
function Get-RandomNormal
    {
    [CmdletBinding()]
    Param ( [double]$Mean, [double]$StandardDeviation )

    $RandomNormal = $Mean + $StandardDeviation * [math]::Sqrt( -2 * [math]::Log( ( Get-Random -Minimum 0.0 -Maximum 1.0 ) ) ) * [math]::Cos( 2 * [math]::PI * ( Get-Random -Minimum 0.0 -Maximum 1.0 ) )

    return $RandomNormal
    }

#  Standard deviation function for testing
function Get-StandardDeviation
    {
    [CmdletBinding()]
    param ( [double[]]$Numbers )

    $Measure = $Numbers | Measure-Object -Average
    $PopulationDeviation = 0
    ForEach ($Number in $Numbers) { $PopulationDeviation += [math]::Pow( ( $Number - $Measure.Average ), 2 ) }
    $StandardDeviation = [math]::Sqrt( $PopulationDeviation / ( $Measure.Count - 1 ) )
    return $StandardDeviation
    }

#  Test
$RandomNormalNumbers = 1..1000 | ForEach { Get-RandomNormal -Mean 1 -StandardDeviation 0.5 }

$Measure = $RandomNormalNumbers | Measure-Object -Average

$Stats = [PSCustomObject]@{
    Count             = $Measure.Count
    Average           = $Measure.Average
    StandardDeviation = Get-StandardDeviation -Numbers $RandomNormalNumbers
}

$Stats | Format-List

```

{{out}}

```txt

Count             : 1000
Average           : 1.01206560135809
StandardDeviation : 0.489099623426272

```



## PureBasic


```PureBasic
Procedure.f RandomNormal()
   ; This procedure can return any real number.
   Protected.f x1, x2

   ; random numbers from the open interval ]0, 1[
   x1 = (Random(999998)+1) / 1000000       ; must be > 0 because of Log(x1)
   x2 = (Random(999998)+1) / 1000000

   ProcedureReturn Sqr(-2*Log(x1)) * Cos(2*#PI*x2)
EndProcedure


Define i, n=1000

Dim a.q(n-1)
For i = 0 To n-1
   a(i) = 1 + 0.5 * RandomNormal()
Next
```



## Python

;Using random.gauss:

```python>>>
 import random
>>> values = [random.gauss(1, .5) for i in range(1000)]
>>>
```


;Quick check of distribution:

```python>>>
 def quick_check(numbers):
    count = len(numbers)
    mean = sum(numbers) / count
    sdeviation = (sum((i - mean)**2 for i in numbers) / count)**0.5
    return mean, sdeviation

>>> quick_check(values)
(1.0140373306786599, 0.49943411329234066)
>>>
```


Note that the ''random'' module in the Python standard library supports a number of statistical distribution methods.

;Alternatively using random.normalvariate:

```python>>>
 values = [ random.normalvariate(1, 0.5) for i in range(1000)]
>>> quick_check(values)
(0.990099111944864, 0.5029847005836282)
>>>
```



## R


```r
result <- rnorm(1000, mean=1, sd=0.5)
```



## Racket


```Racket

#lang racket
(for/list ([i 1000])
  (add1 (* (sqrt (* -2 (log (random)))) (cos (* 2 pi (random))) 0.5)))

```



## Raven


```raven
define PI
   -1 acos

define rand1
   9999999 choose 1 + 10000000.0 /

define randNormal
   rand1 PI * 2 * cos
   rand1 log -2 * sqrt
   *
   2 / 1 +

1000 each drop randNormal "%f\n" print
```

Quick Check (on linux with code in file rand.rv)

```raven
raven rand.rv | awk '{sum+=$1; sumsq+=$1*$1;} END {print "stdev = " sqrt(sumsq/NR - (sum/NR)**2); print "mean = " sum/NR}'
stdev = 0.497773
mean = 1.01497
```



## REXX

The REXX language doesn't have any "higher math" functions like SQRT/SIN/COS/LN/LOG/EXP/POW/etc.,

so we ''hoi polloi'' REXX programmers have to roll our own.

Programming note:   note the range of the random numbers:   (0,1]

(that is, random numbers from   zero──►unity,   excluding zero, including unity).

```rexx
/*REXX pgm generates 1,000 normally distributed numbers:  mean=1,  standard deviation=½.*/
numeric digits 20                                /*the default decimal digit precision=9*/
parse arg n seed .                               /*allow specification of N and the seed*/
if n==''  |  n==","    then n=1000               /*N:    is the size of the array.      */
if datatype(seed,'W')  then call random ,,seed   /*SEED: for repeatable random numbers. */
newMean=1                                        /*the desired new mean (arithmetic avg)*/
sd=1/2                                           /*the desired new standard deviation.  */
       do g=1  for n                             /*generate  N uniform random #'s (0,1].*/
       #.g = random(1, 1e5)  /  1e5              /*REXX's RANDOM BIF generates integers.*/
       end   /*g*/                               /* [↑]  random integers ──► fractions. */
say '              old mean='   mean()
say 'old standard deviation='   stdDev()
call pi;       pi2=pi * 2                        /*define   pi    and also    2 * pi.   */
say
       do j=1  to n-1  by 2;    m=j+1            /*step through the iterations by two.  */
           _=sd *  sqrt(ln(#.j) * -2)            /*calculate the  used-twice expression.*/
       #.j=_ * cos(pi2 * #.m)  +  newMean        /*utilize the  Box─Muller method.      */
       #.m=_ * sin(pi2 * #.m)  +  newMean        /*random number must be:      (0,1]    */
       end   /*j*/
say '              new mean='     mean()
say 'new standard deviation='     stdDev()
exit                                             /*stick a fork in it,  we're all done. */
/*───────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
mean:   _=0;                   do k=1  for n;  _=_ + #.k;              end;                return      _/n
stdDev: _avg=mean();  _=0;     do k=1  for n;  _=_ + (#.k - _avg)**2;  end;                return sqrt(_/n)
e:      e =2.7182818284590452353602874713526624977572470936999595749669676277240766303535; return e   /*digs overkill*/
pi:     pi=3.1415926535897932384626433832795028841971693993751058209749445923078164062862; return pi  /*  "      "   */
r2r:    return arg(1)  //  (pi() * 2)                                                                 /*normalize ang*/
sin:    procedure; parse arg x;x=r2r(x);numeric fuzz min(5,digits()-3);if abs(x)=pi then return 0;return .sincos(x,x,1)
.sincos:parse arg z,_,i; x=x*x; p=z;    do k=2 by 2; _=-_*x/(k*(k+i)); z=z+_; if z=p then leave; p=z; end;     return z
/*───────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
ln:     procedure; parse arg x,f;   call e;   ig= x>1.5;     is=1 - 2 * (ig\==1);           ii=0;             xx=x
          do while ig&xx>1.5|\ig&xx<.5;_=e;do k=-1;iz=xx*_**-is;if k>=0&(ig&iz<1|\ig&iz>.5) then leave;_=_*_;izz=iz;end
        xx=izz;ii=ii+is*2**k;end;x=x*e**-ii-1;z=0;_=-1;p=z;do k=1;_=-_*x;z=z+_/k;if z=p then leave;p=z;end; return z+ii
/*───────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
cos:    procedure; parse arg x;       x=r2r(x);        a=abs(x);               hpi=pi * .5
            numeric fuzz min(6, digits() - 3);      if a=pi    then return -1
            if a=hpi | a=hpi*3  then return 0;      if a=pi/3  then return .5
            if a=pi * 2/3       then return -.5;                    return .sinCos(1,1,-1)
/*───────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
sqrt:   procedure; parse arg x; if x=0  then return 0;  d=digits();  numeric digits; h=d+6
        numeric form; parse value format(x,2,1,,0) 'E0'  with  g 'E' _ .;  g=g * .5'e'_ %2
        m.=9;     do j=0  while h>9;       m.j=h;                h=h%2 + 1;      end /*j*/
                  do k=j+5  to 0  by -1;   numeric digits m.k;   g=(g+x/g)*.5;   end /*k*/
        numeric digits d;     return g/1
```

'''output'''   when using the default inputs:

```txt

              old mean= 0.5015724
old standard deviation= 0.28652466389342471402

              new mean= 0.98807025356443262689
new standard deviation= 0.50002924192766720838

```



## Ring


```ring

for i = 1 to 10
    see random(i) + nl
next i

```


## Ruby


```ruby
Array.new(1000) { 1 + Math.sqrt(-2 * Math.log(rand)) * Math.cos(2 * Math::PI * rand) }
```



## Run BASIC


```runbasic
dim a(1000)
pi = 22/7
for i = 1 to 1000
   a( i)  = 1 + .5 * (sqr(-2 * log(rnd(0))) * cos(2 * pi * rnd(0)))
next i
```



## Rust

{{libheader|rand}}
'''Using a for-loop:'''

```rust
extern crate rand;
use rand::distributions::{Normal, IndependentSample};

fn main() {
    let mut rands = [0.0; 1000];
    let normal = Normal::new(1.0, 0.5);
    let mut rng = rand::thread_rng();
    for num in rands.iter_mut() {
        *num = normal.ind_sample(&mut rng);
    }
}
```


'''Using iterators:'''

```rust
extern crate rand;
use rand::distributions::{Normal, IndependentSample};

fn main() {
    let rands: Vec<_> = {
        let normal = Normal::new(1.0, 0.5);
        let mut rng = rand::thread_rng();
        (0..1000).map(|_| normal.ind_sample(&mut rng)).collect()
    };
}
```



## SAS


```SAS

/* Generate 1000 random numbers with mean 1 and standard deviation 0.5.
  SAS version 9.2 was used to create this code.*/

data norm1000;
  call streaminit(123456);
/* Set the starting point, so we can replicate results.
   If you want different results each time, comment the above line. */
  do i=1 to 1000;
    r=rand('normal',1,0.5);
    output;
  end;
run;

```

Results:

```txt

 The MEANS Procedure

                     Analysis Variable : r

                          Mean         Std Dev
                  ----------------------------
                     0.9907408       0.4844051
                  ----------------------------

```



## Sather


```sather
class MAIN is
  main is
    a:ARRAY{FLTD} := #(1000);
    i:INT;

    RND::seed(2010);
    loop i := 1.upto!(1000) - 1;
      a[i] := 1.0d + 0.5d * RND::standard_normal;
    end;

    -- testing the distribution
    mean ::= a.reduce(bind(_.plus(_))) / a.size.fltd;
    #OUT + "mean " + mean + "\n";
    a.map(bind(_.minus(mean)));
    a.map(bind(_.pow(2.0d)));
    dev ::= (a.reduce(bind(_.plus(_))) / a.size.fltd).sqrt;
    #OUT + "dev  " + dev + "\n";
  end;
end;
```



## Scala


### One liner


```scala
List.fill(1000)(1.0 + 0.5 * scala.util.Random.nextGaussian)
```


### Academic


```scala
val distrubution = {
  def randomNormal = 1.0 + 0.5 * scala.util.Random.nextGaussian

  def normalDistribution(a: Double): Stream[Double] = a #:: normalDistribution(randomNormal)

  normalDistribution(randomNormal)
}

/*
 * Let's test it
 */
  def calcAvgAndStddev[T](ts: Iterable[T])(implicit num: Fractional[T]): (T, Double) = {
    val mean: T =
      num.div(ts.sum, num.fromInt(ts.size)) // Leaving with type of function T

    // Root of mean diffs
    val stdDev = sqrt(ts.map { x =>
      val diff = num.toDouble(num.minus(x, mean))
      diff * diff
    }.sum / ts.size)

    (mean, stdDev)
  }

println(calcAvgAndStddev(distrubution.take(1000))) // e.g. (1.0061433267806525,0.5291834867560893)
```



## Scheme


```scheme
; linear congruential generator given in C99 section 7.20.2.1
(define ((c-rand seed)) (set! seed (remainder (+ (* 1103515245 seed) 12345) 2147483648)) (quotient seed 65536))

; uniform real numbers in open interval (0, 1)
(define (unif-rand seed) (let ((r (c-rand seed))) (lambda () (/ (+ (r) 1) 32769.0))))

; Box-Muller method to generate normal distribution
(define (normal-rand unif m s)
(let ((? #t) (! 0.0) (twopi (* 2.0 (acos -1.0))))
(lambda ()
   (set! ? (not ?))
   (if ? !
         (let ((a (sqrt (* -2.0 (log (unif))))) (b (* twopi (unif))))
              (set! ! (+ m (* s a (sin b))))
              (+ m (* s a (cos b))))))))

(define rnorm (normal-rand (unif-rand 0) 1.0 0.5))

; auxiliary function to get a list of 'n random numbers from generator 'r
(define (rand-list r n) = (if (zero? n) '() (cons (r) (rand-list r (- n 1)))))

(define v (rand-list rnorm 1000))

v
#|
(-0.27965824722565835
 -0.8870860825789542
 0.6499618744638194
 0.31336141955110863
 ...
 0.5648743998193049
 0.8282656735558756
 0.6399951934564637
 0.7699535302478072)
|#

; check mean and standard deviation
(define (mean-sdev v)
(let loop ((v v) (a 0) (b 0) (n 0))
(if (null? v)
    (let ((mean (/ a n)))
         (list mean (sqrt (/ (- b (* n mean mean)) (- n 1)))))
    (let ((x (car v)))
         (loop (cdr v) (+ a x) (+ b (* x x)) (+ n 1))))))

(mean-sdev v)
; (0.9562156817697293 0.5097087109575911)
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";

const func float: frand is func  # Uniform distribution, (0..1]
  result
    var float: frand is 0.0;
  begin
    repeat
      frand := rand(0.0, 1.0);
    until frand <> 0.0;
  end func;

const func float: randomNormal is  # Normal distribution, centered on 0, std dev 1
  return sqrt(-2.0 * log(frand)) * cos(2.0 * PI * frand);

const proc: main is func
  local
    var integer: i is 0;
    var array float: rands is 1000 times 0.0;
  begin
    for i range 1 to length(rands) do
      rands[i] := 1.0 + 0.5 * randomNormal;
    end for;
  end func;
```



## Sidef


```ruby
var arr = 1000.of { 1 + (0.5 * sqrt(-2 * 1.rand.log) * cos(Num.tau * 1.rand)) }
arr.each { .say }
```



## Standard ML

{{works with|SML/NJ}}
SML/NJ has two structures for random numbers:

1) Rand (a linear congruential generator).
You create the generator by calling <code>Rand.mkRandom</code> with a seed (of <code>word</code> type).
You can call the generator with <code>()</code> repeatedly to get a word in the range <code>[Rand.randMin, Rand.randMax]</code>.
You can use the <code>Rand.norm</code> function to transform the output into a <code>real</code> from 0 to 1, or use the <code>Rand.range (i,j)</code> function to transform the output into an <code>int</code> of the given range.

```sml
val seed = 0w42;
val gen = Rand.mkRandom seed;
fun random_gaussian () =
  1.0 + Math.sqrt (~2.0 * Math.ln (Rand.norm (gen ()))) * Math.cos (2.0 * Math.pi * Rand.norm (gen ()));
val a = List.tabulate (1000, fn _ => random_gaussian ());
```


2) Random (a subtract-with-borrow generator). You create the generator by calling <code>Random.rand</code> with a seed (of a pair of <code>int</code>s). You can use the <code>Random.randInt</code> function to generate a random int over its whole range; <code>Random.randNat</code> to generate a non-negative random int; <code>Random.randReal</code> to generate a <code>real</code> between 0 and 1; or <code>Random.randRange (i,j)</code> to generate an <code>int</code> in the given range.

```sml
val seed = (47,42);
val gen = Random.rand seed;
fun random_gaussian () =
  1.0 + Math.sqrt (~2.0 * Math.ln (Random.randReal gen)) * Math.cos (2.0 * Math.pi * Random.randReal gen);
val a = List.tabulate (1000, fn _ => random_gaussian ());
```


Other implementations of Standard ML have their own random number generators. For example, Moscow ML has a <code>Random</code> structure that is different from the one from SML/NJ.
{{works with|PolyML}}
The SML Basis Library does not provide a routine for uniform deviate generation, and PolyML does not have one. Using a routine from "Monte Carlo" by Fishman (Springer), in the function uniformdeviate, and avoiding the slow IntInf's:

```smlh

val urandomlist =  fn seed => fn n =>
let
	val uniformdeviate = fn seed =>
	let
	  val in31m = (Real.fromInt o Int32.toInt ) (getOpt (Int32.maxInt,0) );
	  val in31 = in31m +1.0;
	  val s1 = 41160.0;
	  val s2 = 950665216.0;
	  val v = Real.realFloor seed;
	  val val1 = v*s1;
	  val val2 = v*s2;
	  val next1 = Real.fromLargeInt (Real.toLargeInt IEEEReal.TO_NEGINF (val1/in31)) ;
	  val next2 = Real.rem(Real.realFloor(val2/in31) , in31m );
	  val valt = val1+val2 - (next1+next2)*in31m;
	  val nextt = Real.realFloor(valt/in31m);
	  val valt = valt - nextt*in31m;
	in
	  (valt/in31m,valt)
	end;
val store =  ref (0.0,0.0);
val rec u =  fn S => fn 0 => [] | n=> (store:=uniformdeviate S; (#1 (!store)):: (u (#2 (!store)) (n-1))) ;
in
	u seed n
end;

local
	open Math
in
	val bmconv = fn urand => fn vrand => 1.0+0.5*(sqrt(~2.0*ln urand)*cos (2.0*pi*vrand) )
end;

val rec makeNormals = fn once => fn u::v::[] => [once u v] |
	u::v::rm => (once u v )::(makeNormals once rm );

val anyrealseed=1009.0 ;
makeNormals bmconv (urandomlist anyrealseed 2000);

```



## Stata


```stata
clear all
set obs 1000
gen x=rnormal(1,0.5)
```



###  Mata


```stata
a = rnormal(1000,1,1,0.5)
```



## Tcl


```tcl
package require Tcl 8.5
variable ::pi [expr acos(0)]
proc ::tcl::mathfunc::nrand {} {
    expr {sqrt(-2*log(rand())) * cos(2*$::pi*rand())}
}

set mean 1.0
set stddev 0.5
for {set i 0} {$i < 1000} {incr i} {
    lappend result [expr {$mean + $stddev*nrand()}]
}
```


=={{header|TI-83 BASIC}}==
Builtin function: randNorm()
  randNorm(1,.5)

Or by a program:

Calculator symbol translations:

"STO" arrow: &#8594;

Square root sign: &#8730;

 ClrList L<sub>1</sub>
 Radian
 For(A,1,1000)
 √(-2*ln(rand))*cos(2*π*A)→L<sub>1</sub>(A)
 End


## TorqueScript


```tqs
for (%i = 0; %i < 1000; %i++)
	%list[%i] = 1 + mSqrt(-2 * mLog(getRandom())) * mCos(2 * $pi * getRandom());
```



## Ursala

There are two ways of interpreting the task, either to simulate
sampling a population described by the given statistics, or to
construct a sample exhibiting the given statistics. Both are
illustrated below. The functions parameterized by the mean
and standard deviation take a sample size and return a sample
of that size, represented as a list of floating point numbers.
The Z library function simulates a draw from
a standard normal distribution. Mean and standard deviation
library functions are also used in this example.

```Ursala
#import nat
#import flo

pop_stats("mu","sigma") = plus/*"mu"+ times/*"sigma"+ Z*+ iota

sample_stats("mu","sigma") = plus^*D(minus/"mu"+ mean,~&)+ vid^*D(div\"sigma"+ stdev,~&)+ Z*+ iota

#cast %eWL

test =

^(mean,stdev)* <
   pop_stats(1.,0.5) 1000,
   sample_stats(1.,0.5) 1000>
```

The output shows the mean and standard deviation for both sample vectors,
the latter being exact by construction.

```txt
<
   (1.004504e+00,4.915525e-01),
   (1.000000e+00,5.000000e-01)>
```



## Visual FoxPro


```vfp

LOCAL i As Integer, m As Double, n As Integer, sd As Double
py = PI()
SET TALK OFF
SET DECIMALS TO 6
CREATE CURSOR gdev (deviate B(6))
RAND(-1)
n = 1000
m = 1
sd = 0.5
CLEAR
FOR i = 1 TO n
	INSERT INTO gdev VALUES (GaussDev(m, 1/sd))
ENDFOR
CALCULATE AVG(deviate), STD(deviate) TO m, sd
? "Mean", m, "Std Dev", sd
SET TALK ON
SET DECIMALS TO

FUNCTION GaussDev(mean As Double, sdev As Double) As Double
LOCAL z As Double
z = SQRT(-2*LOG(RAND()))*COS(py*RAND())
IF sdev # 0
	z = mean + z/sdev
ENDIF
RETURN z
ENDFUNC

```



## Yorick

Returns array of ''count'' random numbers with mean 0 and standard deviation 1.

```yorick
func random_normal(count) {
   return sqrt(-2*log(random(count))) * cos(2*pi*random(count));
}
```


Example of basic use:

```txt
> nums = random_normal(1000); // create an array 1000 random numbers
> nums(avg); // show the mean
0.00901216
> nums(rms); // show the standard deviation
0.990265
```


Example with a mean of 1.0 and a standard deviation of 0.5:

```txt
> nums = random_normal(1000) * 0.5 + 1;
> nums(avg);
1.00612
> nums(rms);
0.496853
```



## zkl


```zkl
fcn mkRand(mean,sd){ //normally distributed random w/mean & standard deviation
   pi:=(0.0).pi;    // using the Box–Muller transform
   rz1:=fcn{1.0-(0.0).random(1)}  // from [0,1) to (0,1]
   return('wrap(){((-2.0*rz1().log()).sqrt() * (2.0*pi*rz1()).cos())*sd + mean })
}
```

This creates a new random number generator, now to use it:

```zkl
var g=mkRand(1,0.5);
ns:=(0).pump(1000,List,g); // 1000 rands with mean==1 & sd==1/2
mean:=(ns.sum(0.0)/1000);  //-->1.00379
   // calc sd of list of numbers:
(ns.reduce('wrap(p,n){p+(n-mean).pow(2)},0.0)/1000).sqrt() //-->0.494844
```



## ZX Spectrum Basic


Here we have converted the QBasic code to suit the ZX Spectrum:


```zxbasic
10 RANDOMIZE 0 : REM seeds random number generator based on uptime
20 DIM a(1000)
30 CLS
40 FOR i = 1 TO 1000
50 LET a(i) = 1 + SQR(-2 * LN(RND)) * COS(2 * PI * RND)
60 NEXT i
```

