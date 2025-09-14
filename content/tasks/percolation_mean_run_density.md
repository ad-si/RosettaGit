+++
title = "Percolation/Mean run density"
description = ""
date = 2018-10-05T14:03:29Z
aliases = []
[extra]
id = 15846
[taxonomies]
categories = ["task", "Percolation Simulations"]
tags = []
languages = [
  "c",
  "c_plus_plus",
  "d",
  "echo_lisp",
  "factor",
  "fortran",
  "go",
  "j",
  "julia",
  "kotlin",
  "mathematica",
  "perl_6",
  "phix",
  "python",
  "racket",
  "tcl",
  "zkl",
]
+++

Let <math>v</math> be a vector of <math>n</math> values of either <tt>1</tt> or <tt>0</tt> where the probability of any
value being <tt>1</tt> is <math>p</math>; the probability of a value being <tt>0</tt> is therefore <math>1-p</math>.
Define a run of <tt>1</tt>s as being a group of consecutive <tt>1</tt>s in the vector bounded
either by the limits of the vector or by a <tt>0</tt>. Let the number of such runs in a given
vector of length <math>n</math> be <math>R_n</math>.

For example, the following vector has <math>R_{10} = 3</math>

```txt

[1 1 0 0 0 1 0 1 1 1]
 ^^^       ^   ^^^^^

```

Percolation theory states that

:<math>K(p) = \lim_{n\to\infty} R_n / n = p(1 - p)</math>

## Task

Any calculation of <math>R_n / n</math> for finite <math>n</math> is subject to randomness so should be
computed as the average of <math>t</math> runs, where <math>t \ge 100</math>.

For values of <math>p</math> of 0.1, 0.3, 0.5, 0.7, and 0.9, show the effect of varying <math>n</math>
on the accuracy of simulated <math>K(p)</math>.

Show your output here.

## See also

* [http://mathworld.wolfram.com/s-Run.html s-Run] on Wolfram mathworld.


## C


```c
#include <stdio.h>
#include <stdlib.h>

// just generate 0s and 1s without storing them
double run_test(double p, int len, int runs)
{
	int r, x, y, i, cnt = 0, thresh = p * RAND_MAX;

	for (r = 0; r < runs; r++)
		for (x = 0, i = len; i--; x = y)
			cnt += x < (y = rand() < thresh);

	return (double)cnt / runs / len;
}

int main(void)
{
	double p, p1p, K;
	int ip, n;

	puts(	"running 1000 tests each:\n"
		" p\t   n\tK\tp(1-p)\t     diff\n"
		"-----------------------------------------------");
	for (ip = 1; ip < 10; ip += 2) {
		p = ip / 10., p1p = p * (1 - p);

		for (n = 100; n <= 100000; n *= 10) {
			K = run_test(p, n, 1000);
			printf("%.1f\t%6d\t%.4f\t%.4f\t%+.4f (%+.2f%%)\n",
				p, n, K, p1p, K - p1p, (K - p1p) / p1p * 100);
		}
		putchar('\n');
	}

	return 0;
}
```

```txt

running 1000 tests each:
 p         n    K       p(1-p)       diff
-----------------------------------------------
0.1        100  0.0900  0.0900  -0.0001 (-0.06%)
0.1       1000  0.0899  0.0900  -0.0001 (-0.11%)
0.1      10000  0.0902  0.0900  +0.0002 (+0.17%)
0.1     100000  0.0900  0.0900  -0.0000 (-0.03%)

0.3        100  0.2110  0.2100  +0.0010 (+0.46%)
0.3       1000  0.2104  0.2100  +0.0004 (+0.19%)
0.3      10000  0.2100  0.2100  -0.0000 (-0.02%)
0.3     100000  0.2100  0.2100  -0.0000 (-0.01%)

0.5        100  0.2516  0.2500  +0.0016 (+0.66%)
0.5       1000  0.2498  0.2500  -0.0002 (-0.10%)
0.5      10000  0.2500  0.2500  +0.0000 (+0.01%)
0.5     100000  0.2500  0.2500  +0.0000 (+0.01%)

0.7        100  0.2162  0.2100  +0.0062 (+2.93%)
0.7       1000  0.2107  0.2100  +0.0007 (+0.33%)
0.7      10000  0.2101  0.2100  +0.0001 (+0.06%)
0.7     100000  0.2100  0.2100  -0.0000 (-0.02%)

0.9        100  0.0982  0.0900  +0.0082 (+9.07%)
0.9       1000  0.0905  0.0900  +0.0005 (+0.57%)
0.9      10000  0.0901  0.0900  +0.0001 (+0.09%)
0.9     100000  0.0900  0.0900  +0.0000 (+0.03%)

```


## C++


```cpp
#include <algorithm>
#include <random>
#include <vector>
#include <iostream>
#include <numeric>
#include <iomanip>
using VecIt = std::vector<int>::const_iterator ;

//creates vector of length n, based on probability p for 1
std::vector<int> createVector( int n, double p ) {
   std::vector<int> result( n ) ;
   std::random_device rd ;
   std::mt19937 gen( rd( ) ) ;
   std::uniform_real_distribution<> dis( 0 , 1 ) ;
   for ( int i = 0 ; i < n ; i++ ) {
      double number = dis( gen ) ;
      if ( number <= p )
	 result[ i ] = 1 ;
      else
	 result[ i ] = 0 ;
   }
   return result ;
}

//find number of 1 runs in the vector
int find_Runs( const std::vector<int> & numberVector ) {
   int runs = 0 ;
   VecIt found = numberVector.begin( ) ;
   while ( ( found = std::find( found , numberVector.end( ) , 1 ) )
	 != numberVector.end( ) ) {
      runs++ ;
      while ( found != numberVector.end( ) && ( *found == 1 ) )
	 std::advance( found , 1 ) ;
      if ( found == numberVector.end( ) )
	 break ;
   }
   return runs ;
}

int main( ) {
   std::cout << "t = 100\n" ;
   std::vector<double> p_values { 0.1 , 0.3 , 0.5 , 0.7 , 0.9 } ;
   for ( double p : p_values ) {
      std::cout << "p = " << p << " , K(p) = " << p * ( 1 - p ) << std::endl ;
      for ( int n = 10 ; n < 100000 ; n *= 10 ) {
	 std::vector<double> runsFound ;
	 for ( int i = 0 ; i < 100 ; i++ ) {
	    std::vector<int> ones_and_zeroes = createVector( n , p ) ;
	    runsFound.push_back( find_Runs( ones_and_zeroes ) / static_cast<double>( n ) ) ;
	 }
	 double average = std::accumulate( runsFound.begin( ) , runsFound.end( ) , 0.0 ) / runsFound.size( ) ;
	 std::cout << "  R(" << std::setw( 6 ) << std::right << n << ", p) = " << average << std::endl ;
      }
   }
   return 0 ;
}
```

```txt
t = 100
p = 0.1 , K(p) = 0.09
  R(    10, p) = 0.088
  R(   100, p) = 0.0931
  R(  1000, p) = 0.09013
  R( 10000, p) = 0.089947
p = 0.3 , K(p) = 0.21
  R(    10, p) = 0.225
  R(   100, p) = 0.2089
  R(  1000, p) = 0.21043
  R( 10000, p) = 0.20991
p = 0.5 , K(p) = 0.25
  R(    10, p) = 0.271
  R(   100, p) = 0.253
  R(  1000, p) = 0.25039
  R( 10000, p) = 0.250278
p = 0.7 , K(p) = 0.21
  R(    10, p) = 0.264
  R(   100, p) = 0.2155
  R(  1000, p) = 0.20829
  R( 10000, p) = 0.209977
p = 0.9 , K(p) = 0.09
  R(    10, p) = 0.167
  R(   100, p) = 0.0928
  R(  1000, p) = 0.09071
  R( 10000, p) = 0.090341

```



## D

```d
import std.stdio, std.range, std.algorithm, std.random, std.math;

enum n = 100, p = 0.5, t = 500;

double meanRunDensity(in size_t n, in double prob) {
    return n.iota.map!(_ => uniform01 < prob)
           .array.uniq.sum / double(n);
}

void main() {
    foreach (immutable p; iota(0.1, 1.0, 0.2)) {
        immutable limit = p * (1 - p);
        writeln;
        foreach (immutable n2; iota(10, 16, 2)) {
            immutable n = 2 ^^ n2;
            immutable sim = t.iota.map!(_ => meanRunDensity(n, p))
                            .sum / t;
            writefln("t=%3d, p=%4.2f, n=%5d, p(1-p)=%5.5f, " ~
                     "sim=%5.5f, delta=%3.1f%%", t, p, n, limit, sim,
                     limit ? abs(sim - limit) / limit * 100 : sim*100);
        }
    }
}
```

```txt
t=500, p=0.10, n= 1024, p(1-p)=0.09000, sim=0.08949, delta=0.6%
t=500, p=0.10, n= 4096, p(1-p)=0.09000, sim=0.08976, delta=0.3%
t=500, p=0.10, n=16384, p(1-p)=0.09000, sim=0.08988, delta=0.1%

t=500, p=0.30, n= 1024, p(1-p)=0.21000, sim=0.20979, delta=0.1%
t=500, p=0.30, n= 4096, p(1-p)=0.21000, sim=0.21020, delta=0.1%
t=500, p=0.30, n=16384, p(1-p)=0.21000, sim=0.21005, delta=0.0%

t=500, p=0.50, n= 1024, p(1-p)=0.25000, sim=0.25016, delta=0.1%
t=500, p=0.50, n= 4096, p(1-p)=0.25000, sim=0.25026, delta=0.1%
t=500, p=0.50, n=16384, p(1-p)=0.25000, sim=0.24990, delta=0.0%

t=500, p=0.70, n= 1024, p(1-p)=0.21000, sim=0.21050, delta=0.2%
t=500, p=0.70, n= 4096, p(1-p)=0.21000, sim=0.20993, delta=0.0%
t=500, p=0.70, n=16384, p(1-p)=0.21000, sim=0.21009, delta=0.0%

t=500, p=0.90, n= 1024, p(1-p)=0.09000, sim=0.09019, delta=0.2%
t=500, p=0.90, n= 4096, p(1-p)=0.09000, sim=0.09047, delta=0.5%
t=500, p=0.90, n=16384, p(1-p)=0.09000, sim=0.09007, delta=0.1%
```



## EchoLisp


```scheme

;; count 1-runs - The vector is not stored
(define (runs p n)
(define ct 0)
(define run-1 #t)
    (for ([i n])
        (if (< (random) p)
        (set! run-1 #t) ;; 0 case
        (begin ;; 1 case
            (when run-1 (set! ct (1+ ct)))
            (set! run-1 #f))))
    (// ct n))

;; mean of t counts
(define (truns p (n 1000 )  (t 1000))
    (// (for/sum ([i t]) (runs p n)) t))

(define (task)
    (for ([p (in-range 0.1 1.0 0.2)])
    (writeln)
    (writeln '🔸 'p p 'Kp (* p (- 1 p)))
        (for ([n '(10 100 1000)])
        (printf "\t-- n %5d   →  %d" n (truns p n)))))

```

```txt

(task) ;; t = 1000
🔸     p     0.1     Kp     0.09
 -- n    10 → 0.171
 -- n   100 → 0.0974
 -- n  1000 → 0.0907
🔸     p     0.3     Kp     0.21
 -- n    10 → 0.2642
 -- n   100 → 0.2161
 -- n  1000 → 0.2105
🔸     p     0.5     Kp     0.25
 -- n    10 → 0.2764
 -- n   100 → 0.2519
 -- n  1000 → 0.2503
🔸     p     0.7     Kp     0.21
 -- n    10 → 0.2218
 -- n   100 → 0.2106
 -- n  1000 → 0.2098
🔸     p     0.9     Kp     0.09
 -- n    10 → 0.087
 -- n   100 → 0.0894
 -- n  1000 → 0.0905

```



## Factor


```factor
USING: formatting fry io kernel math math.ranges math.statistics
random sequences ;
IN: rosetta-code.mean-run-density

: rising? ( ? ? -- ? ) [ f = ] [ t = ] bi* and ;

: count-run ( n ? ? -- m ? )
    2dup rising? [ [ 1 + ] 2dip ] when nip ;

: runs ( n p -- n )
    [ 0 f ] 2dip '[ random-unit _ < count-run ] times drop ;

: rn ( n p -- x ) over [ runs ] dip /f ;

: sim ( n p -- avg )
    [ 1000 ] 2dip [ rn ] 2curry replicate mean ;

: theory ( p -- x ) 1 over - * ;

: result ( n p -- )
    [ swap ] [ sim ] [ nip theory ] 2tri 2dup - abs
    "%.1f  %-5d  %.4f  %.4f  %.4f\n" printf ;

: test ( p -- )
    { 100 1,000 10,000 } [ swap result ] with each nl ;

: header ( -- )
    "1000 tests each:\np    n      K       p(1-p)  diff" print ;

: main ( -- ) header .1 .9 .2 <range> [ test ] each ;

MAIN: main
```

```txt

1000 tests each:
p    n      K       p(1-p)  diff
0.1  100    0.0909  0.0900  0.0009
0.1  1000   0.0902  0.0900  0.0002
0.1  10000  0.0899  0.0900  0.0001

0.3  100    0.2111  0.2100  0.0011
0.3  1000   0.2101  0.2100  0.0001
0.3  10000  0.2100  0.2100  0.0000

0.5  100    0.2524  0.2500  0.0024
0.5  1000   0.2504  0.2500  0.0004
0.5  10000  0.2501  0.2500  0.0001

0.7  100    0.2149  0.2100  0.0049
0.7  1000   0.2106  0.2100  0.0006
0.7  10000  0.2100  0.2100  0.0000

0.9  100    0.0978  0.0900  0.0078
0.9  1000   0.0905  0.0900  0.0005
0.9  10000  0.0901  0.0900  0.0001

```



## Fortran


```fortran

! loosely translated from python.  We do not need to generate and store the entire vector at once.
! compilation: gfortran -Wall -std=f2008 -o thisfile thisfile.f08

program percolation_mean_run_density
  implicit none
  integer :: i, p10, n2, n, t
  real :: p, limit, sim, delta
  data n,p,t/100,0.5,500/
  write(6,'(a3,a5,4a7)')'t','p','n','p(1-p)','sim','delta%'
  do p10=1,10,2
     p = p10/10.0
     limit = p*(1-p)
     write(6,'()')
     do n2=10,15,2
        n = 2**n2
        sim = 0
        do i=1,t
           sim = sim + mean_run_density(n,p)
        end do
        sim = sim/t
        if (limit /= 0) then
           delta = abs(sim-limit)/limit
        else
           delta = sim
        end if
        delta = delta * 100
        write(6,'(i3,f5.2,i7,2f7.3,f5.1)')t,p,n,limit,sim,delta
     end do
  end do

contains

  integer function runs(n, p)
    integer, intent(in) :: n
    real, intent(in) :: p
    real :: harvest
    logical :: q
    integer :: count, i
    count = 0
    q = .false.
    do i=1,n
       call random_number(harvest)
       if (harvest < p) then
          q = .true.
       else
          if (q) count = count+1
          q = .false.
       end if
    end do
    runs = count
  end function runs

  real function mean_run_density(n, p)
    integer, intent(in) :: n
    real, intent(in) :: p
    mean_run_density = real(runs(n,p))/real(n)
  end function mean_run_density

end program percolation_mean_run_density

```



```txt

$ ./f
  t    p      n p(1-p)    sim  delta%

500 0.10   1024  0.090  0.090  0.2
500 0.10   4096  0.090  0.090  0.2
500 0.10  16384  0.090  0.090  0.0

500 0.30   1024  0.210  0.210  0.2
500 0.30   4096  0.210  0.210  0.0
500 0.30  16384  0.210  0.210  0.0

500 0.50   1024  0.250  0.250  0.1
500 0.50   4096  0.250  0.250  0.1
500 0.50  16384  0.250  0.250  0.1

500 0.70   1024  0.210  0.210  0.1
500 0.70   4096  0.210  0.210  0.1
500 0.70  16384  0.210  0.210  0.0

500 0.90   1024  0.090  0.090  0.1
500 0.90   4096  0.090  0.090  0.4
500 0.90  16384  0.090  0.090  0.1

```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
)

var (
    pList = []float64{.1, .3, .5, .7, .9}
    nList = []int{1e2, 1e3, 1e4, 1e5}
    t     = 100
)

func main() {
    for _, p := range pList {
        theory := p * (1 - p)
        fmt.Printf("\np: %.4f  theory: %.4f  t: %d\n", p, theory, t)
        fmt.Println("        n          sim     sim-theory")
        for _, n := range nList {
            sum := 0
            for i := 0; i < t; i++ {
                run := false
                for j := 0; j < n; j++ {
                    one := rand.Float64() < p
                    if one && !run {
                        sum++
                    }
                    run = one
                }
            }
            K := float64(sum) / float64(t) / float64(n)
            fmt.Printf("%9d %15.4f %9.6f\n", n, K, K-theory)
        }
    }
}
```

```txt

p: 0.1000  theory: 0.0900  t: 100
        n          sim     sim-theory
      100          0.0883 -0.001700
     1000          0.0903  0.000300
    10000          0.0898 -0.000242
   100000          0.0900 -0.000024

p: 0.3000  theory: 0.2100  t: 100
        n          sim     sim-theory
      100          0.2080 -0.002000
     1000          0.2106  0.000600
    10000          0.2097 -0.000341
   100000          0.2100  0.000018

p: 0.5000  theory: 0.2500  t: 100
        n          sim     sim-theory
      100          0.2512  0.001200
     1000          0.2486 -0.001440
    10000          0.2500  0.000021
   100000          0.2500 -0.000025

p: 0.7000  theory: 0.2100  t: 100
        n          sim     sim-theory
      100          0.2108  0.000800
     1000          0.2086 -0.001370
    10000          0.2102  0.000247
   100000          0.2100 -0.000031

p: 0.9000  theory: 0.0900  t: 100
        n          sim     sim-theory
      100          0.0970  0.007000
     1000          0.0916  0.001580
    10000          0.0905  0.000501
   100000          0.0900  0.000050

```



## Haskell


```Haskell
import Control.Monad.Random
import Control.Applicative
import Text.Printf
import Control.Monad
import Data.Bits

data OneRun = OutRun | InRun deriving (Eq, Show)

randomList :: Int -> Double -> Rand StdGen [Int]
randomList n p = take n . map f <$> getRandomRs (0,1)
  where f n = if (n > p) then 0 else 1

countRuns xs = fromIntegral . sum $
               zipWith (\x y -> x .&. xor y 1) xs (tail xs ++ [0])

calcK :: Int -> Double -> Rand StdGen Double
calcK n p = (/ fromIntegral n) . countRuns <$> randomList n p

printKs :: StdGen -> Double -> IO ()
printKs g p = do
  printf "p= %.1f, K(p)= %.3f\n" p (p * (1 - p))
  forM_ [1..5] $ \n -> do
    let est = evalRand (calcK (10^n) p) g
    printf "n=%7d, estimated K(p)= %5.3f\n" (10^n::Int) est

main = do
  x <- newStdGen
  forM_ [0.1,0.3,0.5,0.7,0.9] $ printKs x

```


```txt
./percolation
p= 0.1, K(p)= 0.090
n=     10, estimated K(p)= 0.000
n=    100, estimated K(p)= 0.130
n=   1000, estimated K(p)= 0.099
n=  10000, estimated K(p)= 0.090
n= 100000, estimated K(p)= 0.091
p= 0.3, K(p)= 0.210
n=     10, estimated K(p)= 0.200
n=    100, estimated K(p)= 0.250
n=   1000, estimated K(p)= 0.209
n=  10000, estimated K(p)= 0.209
n= 100000, estimated K(p)= 0.211
p= 0.5, K(p)= 0.250
n=     10, estimated K(p)= 0.200
n=    100, estimated K(p)= 0.290
n=   1000, estimated K(p)= 0.252
n=  10000, estimated K(p)= 0.250
n= 100000, estimated K(p)= 0.250
p= 0.7, K(p)= 0.210
n=     10, estimated K(p)= 0.300
n=    100, estimated K(p)= 0.200
n=   1000, estimated K(p)= 0.210
n=  10000, estimated K(p)= 0.209
n= 100000, estimated K(p)= 0.210
p= 0.9, K(p)= 0.090
n=     10, estimated K(p)= 0.200
n=    100, estimated K(p)= 0.090
n=   1000, estimated K(p)= 0.089
n=  10000, estimated K(p)= 0.095
n= 100000, estimated K(p)= 0.090

```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages:


```unicon
procedure main(A)
    t := integer(A[2]) | 500

    write(left("p",8)," ",left("n",8)," ",left("p(1-p)",10)," ",left("SimK(p)",10))
    every (p := 0.1 | 0.3 | 0.5 | 0.7 | 0.9, n := 1000 | 2000 | 3000) do {
        Ka := 0.0
        every !t do {
            every (v := "", !n) do v ||:= |((?0.1 > p,"0")|"1")
            R := 0
            v ? while tab(upto('1')) do R +:= (tab(many('1')), 1)
            Ka +:= real(R)/n
            }
        write(left(p,8)," ",left(n,8)," ",left(p*(1-p),10)," ",left(Ka/t, 10))
        }
end
```


Output:


```txt

->pmrd
p        n        p(1-p)     SimK(p)
0.1      1000     0.09000000 0.09021400
0.1      2000     0.09000000 0.08984799
0.1      3000     0.09000000 0.08993666
0.3      1000     0.21       0.21080999
0.3      2000     0.21       0.209953
0.3      3000     0.21       0.210564
0.5      1000     0.25       0.250024
0.5      2000     0.25       0.25007399
0.5      3000     0.25       0.24975266
0.7      1000     0.21       0.21098799
0.7      2000     0.21       0.20987700
0.7      3000     0.21       0.21047333
0.9      1000     0.08999999 0.09016400
0.9      2000     0.08999999 0.09004800
0.9      3000     0.08999999 0.09023200
->

```



## J


```J

NB. translation of python

NB.  'N P T' =: 100 0.5 500            NB. hypothetical example values, to aid comprehension...

newv =: (> ?@(#&0))~                   NB. generate a random binary vector.  Use:  N newv P
runs =: {: + [: +/ 1 0&E.              NB. add the tail to the sum of 1 0 occurrences  Use: runs V
mean_run_density =: [ %~ [: runs newv  NB. perform experiment.  Use: N mean_run_density P

main =: 3 : 0                          NB.Usage: main T
 T =. y
 smoutput'  T  P    N    P(1-P) SIM   DELTA%'
 for_P. 10 %~ >: +: i. 5 do.
   LIMIT =. (* -.) P
   smoutput ''
   for_N. 2 ^ 10 + +: i. 3 do.
     SIM =. T %~ +/ (N mean_run_density P"_)^:(<T) 0
     smoutput 4 5j2 6 6j3 6j3 4j1 ": T, P, N, LIMIT, SIM, SIM (100 * [`(|@:(- % ]))@.(0 ~: ])) LIMIT
   end.
 end.
 EMPTY
)

```

Session:

```txt

  main 500
  T  P    N    P(1-P) SIM   DELTA%

 500 0.10  1024 0.090 0.090 0.1
 500 0.10  4096 0.090 0.090 0.2
 500 0.10 16384 0.090 0.090 0.2

 500 0.30  1024 0.210 0.210 0.2
 500 0.30  4096 0.210 0.209 0.3
 500 0.30 16384 0.210 0.210 0.1

 500 0.50  1024 0.250 0.250 0.2
 500 0.50  4096 0.250 0.250 0.1
 500 0.50 16384 0.250 0.250 0.2

 500 0.70  1024 0.210 0.210 0.0
 500 0.70  4096 0.210 0.210 0.2
 500 0.70 16384 0.210 0.210 0.2

 500 0.90  1024 0.090 0.091 1.1
 500 0.90  4096 0.090 0.090 0.1
 500 0.90 16384 0.090 0.090 0.1

```



## Julia

```julia
using Distributions, IterTools

newv(n::Int, p::Float64) = rand(Bernoulli(p), n)
runs(v::Vector{Int}) = sum((a & ~b) for (a, b) in zip(v, IterTools.chain(v[2:end], v[1])))

mrd(n::Int, p::Float64) = runs(newv(n, p)) / n

nrep = 500

for p in 0.1:0.2:1
    lim = p * (1 - p)

    println()
    for ex in 10:2:14
        n = 2 ^ ex
        sim = mean(mrd.(n, p) for _ in 1:nrep)
        @printf("nrep = %3i\tp = %4.2f\tn = %5i\np · (1 - p) = %5.3f\tsim = %5.3f\tΔ = %3.1f%%\n",
                nrep, p, n, lim, sim, lim > 0 ? abs(sim - lim) / lim * 100 : sim * 100)
    end
end
```


```txt

nrep = 500	p = 0.10	n =  1024
p · (1 - p) = 0.090	sim = 0.090	Δ = 0.4%
nrep = 500	p = 0.10	n =  4096
p · (1 - p) = 0.090	sim = 0.090	Δ = 0.2%
nrep = 500	p = 0.10	n = 16384
p · (1 - p) = 0.090	sim = 0.090	Δ = 0.0%

nrep = 500	p = 0.30	n =  1024
p · (1 - p) = 0.210	sim = 0.211	Δ = 0.5%
nrep = 500	p = 0.30	n =  4096
p · (1 - p) = 0.210	sim = 0.210	Δ = 0.1%
nrep = 500	p = 0.30	n = 16384
p · (1 - p) = 0.210	sim = 0.210	Δ = 0.0%

nrep = 500	p = 0.50	n =  1024
p · (1 - p) = 0.250	sim = 0.250	Δ = 0.0%
nrep = 500	p = 0.50	n =  4096
p · (1 - p) = 0.250	sim = 0.250	Δ = 0.1%
nrep = 500	p = 0.50	n = 16384
p · (1 - p) = 0.250	sim = 0.250	Δ = 0.0%

nrep = 500	p = 0.70	n =  1024
p · (1 - p) = 0.210	sim = 0.209	Δ = 0.3%
nrep = 500	p = 0.70	n =  4096
p · (1 - p) = 0.210	sim = 0.210	Δ = 0.1%
nrep = 500	p = 0.70	n = 16384
p · (1 - p) = 0.210	sim = 0.210	Δ = 0.0%

nrep = 500	p = 0.90	n =  1024
p · (1 - p) = 0.090	sim = 0.090	Δ = 0.0%
nrep = 500	p = 0.90	n =  4096
p · (1 - p) = 0.090	sim = 0.090	Δ = 0.0%
nrep = 500	p = 0.90	n = 16384
p · (1 - p) = 0.090	sim = 0.090	Δ = 0.1%
```



## Kotlin

```scala
// version 1.2.10

import java.util.Random

val rand = Random()
const val RAND_MAX = 32767

// just generate 0s and 1s without storing them
fun runTest(p: Double, len: Int, runs: Int): Double {
    var cnt = 0
    val thresh = (p * RAND_MAX).toInt()
    for (r in 0 until runs) {
        var x = 0
        var i = len
        while (i-- > 0) {
            val y = if (rand.nextInt(RAND_MAX + 1) < thresh) 1 else 0
            if (x < y) cnt++
            x = y
        }
    }
    return cnt.toDouble() / runs / len
}

fun main(args: Array<String>) {
    println("running 1000 tests each:")
    println(" p\t   n\tK\tp(1-p)\t     diff")
    println("------------------------------------------------")
    val fmt = "%.1f\t%6d\t%.4f\t%.4f\t%+.4f (%+.2f%%)"
    for (ip in 1..9 step 2) {
        val p = ip / 10.0
        val p1p = p * (1.0 - p)
        var n = 100
        while (n <= 100_000) {
            val k = runTest(p, n, 1000)
            println(fmt.format(p, n, k, p1p, k - p1p, (k - p1p) / p1p * 100))
            n *= 10
        }
        println()
    }
}
```


Sample output:

```txt

running 1000 tests each:
 p	   n	K	p(1-p)	     diff
------------------------------------------------
0.1	   100	0.0908	0.0900	+0.0008 (+0.93%)
0.1	  1000	0.0900	0.0900	+0.0000 (+0.02%)
0.1	 10000	0.0899	0.0900	-0.0001 (-0.08%)
0.1	100000	0.0900	0.0900	-0.0000 (-0.05%)

0.3	   100	0.2112	0.2100	+0.0012 (+0.56%)
0.3	  1000	0.2096	0.2100	-0.0004 (-0.21%)
0.3	 10000	0.2101	0.2100	+0.0001 (+0.05%)
0.3	100000	0.2101	0.2100	+0.0001 (+0.03%)

0.5	   100	0.2522	0.2500	+0.0022 (+0.90%)
0.5	  1000	0.2504	0.2500	+0.0004 (+0.15%)
0.5	 10000	0.2500	0.2500	-0.0000 (-0.00%)
0.5	100000	0.2500	0.2500	+0.0000 (+0.00%)

0.7	   100	0.2162	0.2100	+0.0062 (+2.95%)
0.7	  1000	0.2106	0.2100	+0.0006 (+0.29%)
0.7	 10000	0.2101	0.2100	+0.0001 (+0.03%)
0.7	100000	0.2100	0.2100	+0.0000 (+0.01%)

0.9	   100	0.0982	0.0900	+0.0083 (+9.17%)
0.9	  1000	0.0911	0.0900	+0.0011 (+1.17%)
0.9	 10000	0.0902	0.0900	+0.0002 (+0.18%)
0.9	100000	0.0900	0.0900	-0.0000 (-0.02%)

```



## Mathematica


```Mathematica
meanRunDensity[p_, len_, trials_] :=
 Mean[Length[Cases[Split@#, {1, ___}]] & /@
    Unitize[Chop[RandomReal[1, {trials, len}], 1 - p]]]/len

Column@Table[
  Grid[Join[{{p, n, K, diff}},
    Table[{q, n, x = meanRunDensity[q, n, 100] // N,
      q (1 - q) - x}, {n, {100, 1000, 10000, 100000}}], {}],
   Alignment -> Left], {q, {.1, .3, .5, .7, .9}}]
```

```txt

p	n	K	diff
0.1	100	0.0905	-0.0005
0.1	1000	0.0900	-0.00001
0.1	10000	0.0902	-0.00015
0.1	100000	0.0901	-0.0001265

p	n	K	diff
0.3	100	0.2088	 0.0012
0.3	1000	0.2101	-0.00011
0.3	10000	0.2099	 0.000049
0.3	100000	0.2100	-0.0000352

p	n	K	diff
0.5	100	0.2533	-0.0033
0.5	1000	0.2515	-0.00146
0.5	10000	0.2501	-0.000131
0.5	100000	0.2500	-0.0000425

p	n	K	diff
0.7	100	0.2172 	-0.0072
0.7	1000	0.2106	-0.0006
0.7	10000	0.2098	 0.000194
0.7	100000	0.2102	-0.0002176

p	n	K	diff
0.9	100	0.0924	-0.0024
0.9	1000	0.0895	 0.00049
0.9	10000	0.0899	 0.00013
0.9	100000	0.0900	-0.0000144

```



## Pascal

```pascal

{$MODE objFPC}//for using result,parameter runs becomes for variable..
uses
  sysutils;//Format
const
  MaxN = 100*1000;

function run_test(p:double;len,runs: NativeInt):double;
var
   x, y, i,cnt : NativeInt;
Begin
  result := 1/ (runs * len);
  cnt := 0;
  for  runs := runs-1 downto 0 do
  Begin
    x := 0;
    y := 0;
    for i := len-1 downto 0 do
    begin
      x := y;
      y := Ord(Random() < p);
      cnt := cnt+ord(x < y);
    end;
  end;
  result := result *cnt;
end;

//main
var
  p, p1p, K : double;
  ip, n : nativeInt;
Begin
  randomize;
  writeln( 'running 1000 tests each:'#13#10,
    ' p      n      K     p(1-p)   diff'#13#10,
    '-----------------------------------------------');
  ip:= 1;
  while ip < 10 do
  Begin
     p := ip / 10;
     p1p := p * (1 - p);
     n := 100;
     While n <= MaxN do
     Begin
       K := run_test(p, n, 1000);
       writeln(Format('%4.1f %6d  %6.4f  %6.4f %7.4f (%5.2f %%)',
         [p, n, K, p1p, K - p1p, (K - p1p) / p1p * 100]));
       n := n*10;
     end;
     writeln;
     ip := ip+2;
   end;
end.
```

Output

```txt
running 1000 tests each:
 p      n      K     p(1-p)   diff
-----------------------------------------------
 0.1    100  0.0894  0.0900 -0.0006 (-0.70 %)
 0.1   1000  0.0898  0.0900 -0.0002 (-0.17 %)
 0.1  10000  0.0900  0.0900  0.0000 ( 0.02 %)
 0.1 100000  0.0900  0.0900  0.0000 ( 0.04 %)

 0.3    100  0.2112  0.2100  0.0012 ( 0.57 %)
 0.3   1000  0.2101  0.2100  0.0001 ( 0.04 %)
 0.3  10000  0.2099  0.2100 -0.0001 (-0.04 %)
 0.3 100000  0.2099  0.2100 -0.0001 (-0.03 %)

 0.5    100  0.2516  0.2500  0.0016 ( 0.66 %)
 0.5   1000  0.2497  0.2500 -0.0003 (-0.14 %)
 0.5  10000  0.2501  0.2500  0.0001 ( 0.03 %)
 0.5 100000  0.2500  0.2500  0.0000 ( 0.01 %)

 0.7    100  0.2144  0.2100  0.0044 ( 2.08 %)
 0.7   1000  0.2107  0.2100  0.0007 ( 0.32 %)
 0.7  10000  0.2101  0.2100  0.0001 ( 0.02 %)
 0.7 100000  0.2100  0.2100  0.0000 ( 0.01 %)

 0.9    100  0.0978  0.0900  0.0078 ( 8.69 %)
 0.9   1000  0.0909  0.0900  0.0009 ( 0.96 %)
 0.9  10000  0.0901  0.0900  0.0001 ( 0.10 %)
 0.9 100000  0.0900  0.0900  0.0000 ( 0.02 %)

```



## Perl

```perl
sub R {
    my ($n, $p) = @_;
    my $r = join '',
    map { rand() < $p ? 1 : 0 } 1 .. $n;
    0+ $r =~ s/1+//g;
}

use constant t => 100;

printf "t= %d\n", t;
for my $p (qw(.1 .3 .5 .7 .9)) {
    printf "p= %f, K(p)= %f\n", $p, $p*(1-$p);
    for my $n (qw(10 100 1000)) {
        my $r; $r += R($n, $p) for 1 .. t; $r /= $n;
        printf " R(n, p)= %f\n", $r / t;
    }
}
```

```txt
t= 100
p= 0.100000, K(p)= 0.090000
 R(n, p)= 0.095000
 R(n, p)= 0.088100
 R(n, p)= 0.089420
p= 0.300000, K(p)= 0.210000
 R(n, p)= 0.225000
 R(n, p)= 0.208800
 R(n, p)= 0.210020
p= 0.500000, K(p)= 0.250000
 R(n, p)= 0.289000
 R(n, p)= 0.249900
 R(n, p)= 0.248980
p= 0.700000, K(p)= 0.210000
 R(n, p)= 0.262000
 R(n, p)= 0.213200
 R(n, p)= 0.209690
p= 0.900000, K(p)= 0.090000
 R(n, p)= 0.177000
 R(n, p)= 0.096200
 R(n, p)= 0.091730
```



## Perl 6


```perl6
sub R($n, $p) { [+] ((rand < $p) xx $n).squish }

say 't= ', constant t = 100;

for .1, .3 ... .9 -> $p {
    say "p= $p, K(p)= {$p*(1-$p)}";
    for 10, 100, 1000 -> $n {
	printf "  R(%6d, p)= %f\n", $n, t R/ [+] R($n, $p)/$n xx t
    }
}
```

```txt
t= 100
p= 0.1, K(p)= 0.09
  R(    10, p)= 0.088000
  R(   100, p)= 0.085600
  R(  1000, p)= 0.089150
p= 0.3, K(p)= 0.21
  R(    10, p)= 0.211000
  R(   100, p)= 0.214600
  R(  1000, p)= 0.211160
p= 0.5, K(p)= 0.25
  R(    10, p)= 0.279000
  R(   100, p)= 0.249200
  R(  1000, p)= 0.250870
p= 0.7, K(p)= 0.21
  R(    10, p)= 0.258000
  R(   100, p)= 0.215400
  R(  1000, p)= 0.209560
p= 0.9, K(p)= 0.09
  R(    10, p)= 0.181000
  R(   100, p)= 0.094500
  R(  1000, p)= 0.091330
```



## Phix

```Phix
function run_test(atom p, integer len, runs)
    integer count = 0
    for r=1 to runs do
        bool v, pv = false
        for l=1 to len do
            v = rnd()<p
            count += pv<v
            pv = v
        end for
    end for
    return count/runs/len
end function

procedure main()
    printf(1,"Running 1000 tests each:\n")
    printf(1," p        n  K       p(1-p)       delta\n")
    printf(1,"--------------------------------------------\n")
    for ip=1 to 10 by 2 do
        atom p = ip/10,
             p1p = p*(1-p)
        integer n = 100
        while n<=100000 do
            atom K = run_test(p, n, 1000)
            printf(1,"%.1f  %6d  %6.4f  %6.4f  %+7.4f (%+5.2f%%)\n",
                   {p, n, K, p1p, K-p1p, (K-p1p)/p1p*100})
            n *= 10
        end while
        printf(1,"\n")
    end for
end procedure
main()
```

```txt

Running 1000 tests each:
 p        n  K       p(1-p)       delta
--------------------------------------------
0.1     100  0.0889  0.0900  -0.0011 (-1.20%)
0.1    1000  0.0896  0.0900  -0.0004 (-0.45%)
0.1   10000  0.0900  0.0900  -0.0000 (-0.02%)
0.1  100000  0.0900  0.0900  -0.0000 (-0.04%)

0.3     100  0.2112  0.2100  +0.0012 (+0.57%)
0.3    1000  0.2101  0.2100  +0.0001 (+0.07%)
0.3   10000  0.2101  0.2100  +0.0001 (+0.06%)
0.3  100000  0.2100  0.2100  -0.0000 (-0.01%)

0.5     100  0.2528  0.2500  +0.0028 (+1.13%)
0.5    1000  0.2500  0.2500  +0.0000 (+0.01%)
0.5   10000  0.2500  0.2500  +0.0000 (+0.00%)
0.5  100000  0.2500  0.2500  -0.0000 (-0.00%)

0.7     100  0.2174  0.2100  +0.0074 (+3.50%)
0.7    1000  0.2105  0.2100  +0.0005 (+0.26%)
0.7   10000  0.2101  0.2100  +0.0001 (+0.06%)
0.7  100000  0.2100  0.2100  +0.0000 (+0.01%)

0.9     100  0.0986  0.0900  +0.0086 (+9.53%)
0.9    1000  0.0908  0.0900  +0.0008 (+0.88%)
0.9   10000  0.0901  0.0900  +0.0001 (+0.11%)
0.9  100000  0.0900  0.0900  +0.0000 (+0.03%)

```



## Python


```python
from __future__ import division
from random import random
from math import fsum

n, p, t = 100, 0.5, 500

def newv(n, p):
    return [int(random() < p) for i in range(n)]

def runs(v):
    return sum((a & ~b) for a, b in zip(v, v[1:] + [0]))

def mean_run_density(n, p):
    return runs(newv(n, p)) / n

for p10 in range(1, 10, 2):
    p = p10 / 10
    limit = p * (1 - p)
    print('')
    for n2 in range(10, 16, 2):
        n = 2**n2
        sim = fsum(mean_run_density(n, p) for i in range(t)) / t
        print('t=%3i p=%4.2f n=%5i p(1-p)=%5.3f sim=%5.3f delta=%3.1f%%'
              % (t, p, n, limit, sim, abs(sim - limit) / limit * 100 if limit else sim * 100))
```


```txt
t=500 p=0.10 n= 1024 p(1-p)=0.090 sim=0.090 delta=0.2%
t=500 p=0.10 n= 4096 p(1-p)=0.090 sim=0.090 delta=0.0%
t=500 p=0.10 n=16384 p(1-p)=0.090 sim=0.090 delta=0.1%

t=500 p=0.30 n= 1024 p(1-p)=0.210 sim=0.210 delta=0.0%
t=500 p=0.30 n= 4096 p(1-p)=0.210 sim=0.210 delta=0.0%
t=500 p=0.30 n=16384 p(1-p)=0.210 sim=0.210 delta=0.0%

t=500 p=0.50 n= 1024 p(1-p)=0.250 sim=0.251 delta=0.3%
t=500 p=0.50 n= 4096 p(1-p)=0.250 sim=0.250 delta=0.0%
t=500 p=0.50 n=16384 p(1-p)=0.250 sim=0.250 delta=0.0%

t=500 p=0.70 n= 1024 p(1-p)=0.210 sim=0.210 delta=0.0%
t=500 p=0.70 n= 4096 p(1-p)=0.210 sim=0.210 delta=0.1%
t=500 p=0.70 n=16384 p(1-p)=0.210 sim=0.210 delta=0.0%

t=500 p=0.90 n= 1024 p(1-p)=0.090 sim=0.091 delta=0.6%
t=500 p=0.90 n= 4096 p(1-p)=0.090 sim=0.090 delta=0.2%
t=500 p=0.90 n=16384 p(1-p)=0.090 sim=0.090 delta=0.0%
```




## Racket



```racket
#lang racket
(require racket/fixnum)
(define t (make-parameter 100))

(define (Rn v)
  (define (inner-Rn rv idx b-1)
    (define b (fxvector-ref v idx))
    (define rv+ (if (and (= b 1) (= b-1 0)) (add1 rv) rv))
    (if (zero? idx) rv+ (inner-Rn rv+ (sub1 idx) b)))
  (inner-Rn 0 (sub1 (fxvector-length v)) 0))

(define ((make-random-bit-vector p) n)
  (for/fxvector
   #:length n ((i n))
   (if (<= (random) p) 1 0)))

(define (Rn/n l->p n) (/ (Rn (l->p n)) n))

(for ((p (in-list '(1/10 3/10 1/2 7/10 9/10))))
  (define l->p (make-random-bit-vector p))
  (define Kp (* p (- 1 p)))
  (printf "p = ~a\tK(p) =\t~a\t~a~%" p Kp (real->decimal-string Kp 4))
  (for ((n (in-list '(10 100 1000 10000))))
    (define sum-Rn/n (for/sum ((i (in-range (t)))) (Rn/n l->p n)))
    (define sum-Rn/n/t (/ sum-Rn/n (t)))
    (printf "mean(R_~a/~a) =\t~a\t~a~%"
            n n sum-Rn/n/t (real->decimal-string sum-Rn/n/t 4)))
  (newline))

(module+ test
  (require rackunit)
  (check-eq? (Rn (fxvector 1 1 0 0 0 1 0 1 1 1)) 3))
```

```txt

p = 1/10	K(p) =	9/100	0.0900
mean(R_10/10) =	3/40	0.0750
mean(R_100/100) =	221/2500	0.0884
mean(R_1000/1000) =	4469/50000	0.0894
mean(R_10000/10000) =	90313/1000000	0.0903

p = 3/10	K(p) =	21/100	0.2100
mean(R_10/10) =	231/1000	0.2310
mean(R_100/100) =	1049/5000	0.2098
mean(R_1000/1000) =	131/625	0.2096
mean(R_10000/10000) =	209873/1000000	0.2099

p = 1/2	K(p) =	1/4	0.2500
mean(R_10/10) =	297/1000	0.2970
mean(R_100/100) =	1263/5000	0.2526
mean(R_1000/1000) =	24893/100000	0.2489
mean(R_10000/10000) =	124963/500000	0.2499

p = 7/10	K(p) =	21/100	0.2100
mean(R_10/10) =	131/500	0.2620
mean(R_100/100) =	2147/10000	0.2147
mean(R_1000/1000) =	1049/5000	0.2098
mean(R_10000/10000) =	210453/1000000	0.2105

p = 9/10	K(p) =	9/100	0.0900
mean(R_10/10) =	169/1000	0.1690
mean(R_100/100) =	119/1250	0.0952
mean(R_1000/1000) =	4503/50000	0.0901
mean(R_10000/10000) =	89939/1000000	0.0899


```


==REXX==
```rexx
/* REXX */
Numeric Digits 20
Call random(,12345) /* make the run reproducable */
pList = '.1 .3 .5 .7 .9'
nList = '1e2 1e3 1e4 1e5'
t     = 100
Do While plist<>''
  Parse Var plist p plist
  theory=p*(1-p)
  Say ' '
  Say 'p:' format(p,2,4)'  theory:'format(theory,2,4)'  t:'format(t,4)
  Say '         n          sim     sim-theory'
  nl=nlist
  Do While nl<>''
    Parse Var nl n nl
    sum=0
    Do i=1 To t
      run=0
      Do j=1 To n
        one=random(1000)<p*1000
        If one & (run=0) Then
          sum=sum+1
        run=one
        End
      End
    sim=sum/(n*100)
    Say format(n,10)'        ' format(sim,2,4)' 'format(sim-theory,2,6)
    End
  End
```

```txt
p:  0.1000  theory: 0.0900  t: 100
         n          sim     sim-theory
       100          0.0875 -0.002500
      1000          0.0894 -0.000560
     10000          0.0902  0.000237
    100000          0.0899 -0.000112

p:  0.3000  theory: 0.2100  t: 100
         n          sim     sim-theory
       100          0.2088 -0.001200
      1000          0.2116  0.001570
     10000          0.2101  0.000056
    100000          0.2099 -0.000120

p:  0.5000  theory: 0.2500  t: 100
         n          sim     sim-theory
       100          0.2557  0.005700
      1000          0.2513  0.001280
     10000          0.2497 -0.000267
    100000          0.2501  0.000107

p:  0.7000  theory: 0.2100  t: 100
         n          sim     sim-theory
       100          0.2171  0.007100
      1000          0.2095 -0.000490
     10000          0.2099 -0.000137
    100000          0.2103  0.000321

p:  0.9000  theory: 0.0900  t: 100
         n          sim     sim-theory
       100          0.0999  0.009900
      1000          0.0898 -0.000240
     10000          0.0906  0.000568
    100000          0.0908  0.000775
```



## Sidef

```ruby
func R(n,p) {
    n.of { 1.rand < p ? 1 : 0}.sum;
}

const t = 100;
say ('t=', t);

range(.1, .9, .2).each { |p|
    printf("p= %f, K(p)= %f\n", p, p*(1-p));
    [10, 100, 1000].each { |n|
        printf (" R(n, p)= %f\n", t.of { R(n, p) }.sum/n / t);
    }
}
```


```txt

t=100
p= 0.100000, K(p)= 0.090000
 R(n, p)= 0.099000
 R(n, p)= 0.105000
 R(n, p)= 0.099810
p= 0.300000, K(p)= 0.210000
 R(n, p)= 0.301000
 R(n, p)= 0.289800
 R(n, p)= 0.300720
p= 0.500000, K(p)= 0.250000
 R(n, p)= 0.481000
 R(n, p)= 0.501800
 R(n, p)= 0.498260
p= 0.700000, K(p)= 0.210000
 R(n, p)= 0.695000
 R(n, p)= 0.698400
 R(n, p)= 0.701220
p= 0.900000, K(p)= 0.090000
 R(n, p)= 0.910000
 R(n, p)= 0.898500
 R(n, p)= 0.899080

```



## Tcl


```tcl
proc randomString {length probability} {
    for {set s ""} {[string length $s] < $length} {} {
	append s [expr {rand() < $probability}]
    }
    return $s
}

# By default, [regexp -all] gives the number of times that the RE matches
proc runs {str} {
    regexp -all {1+} $str
}

# Compute the mean run density
proc mrd {t p n} {
    for {set i 0;set total 0.0} {$i < $t} {incr i} {
	set run [randomString $n $p]
	set total [expr {$total + double([runs $run])/$n}]
    }
    return [expr {$total / $t}]
}

# Parameter sweep with nested [foreach]
set runs 500
foreach p {0.10 0.30 0.50 0.70 0.90} {
    foreach n {1024 4096 16384} {
	set theory [expr {$p * (1 - $p)}]
	set sim [mrd $runs $p $n]
	set diffpc [expr {abs($theory-$sim)*100/$theory}]
	puts [format "t=%d, p=%.2f, n=%5d, p(1-p)=%.3f, sim=%.3f, delta=%.2f%%" \
		      $runs $p $n $theory $sim $diffpc]
    }
    puts ""
}
```

```txt

t=500, p=0.10, n= 1024, p(1-p)=0.090, sim=0.090, delta=0.07%
t=500, p=0.10, n= 4096, p(1-p)=0.090, sim=0.090, delta=0.06%
t=500, p=0.10, n=16384, p(1-p)=0.090, sim=0.090, delta=0.17%

t=500, p=0.30, n= 1024, p(1-p)=0.210, sim=0.210, delta=0.23%
t=500, p=0.30, n= 4096, p(1-p)=0.210, sim=0.210, delta=0.09%
t=500, p=0.30, n=16384, p(1-p)=0.210, sim=0.210, delta=0.01%

t=500, p=0.50, n= 1024, p(1-p)=0.250, sim=0.250, delta=0.10%
t=500, p=0.50, n= 4096, p(1-p)=0.250, sim=0.250, delta=0.07%
t=500, p=0.50, n=16384, p(1-p)=0.250, sim=0.250, delta=0.08%

t=500, p=0.70, n= 1024, p(1-p)=0.210, sim=0.211, delta=0.33%
t=500, p=0.70, n= 4096, p(1-p)=0.210, sim=0.210, delta=0.00%
t=500, p=0.70, n=16384, p(1-p)=0.210, sim=0.210, delta=0.01%

t=500, p=0.90, n= 1024, p(1-p)=0.090, sim=0.091, delta=1.61%
t=500, p=0.90, n= 4096, p(1-p)=0.090, sim=0.090, delta=0.08%
t=500, p=0.90, n=16384, p(1-p)=0.090, sim=0.090, delta=0.09%

```



## zkl

```zkl
fcn run_test(p,len,runs){
   cnt:=0; do(runs){
      pv:=0; do(len){
         v:=0 + ((0.0).random(1.0)<p);  // 0 or 1, value of V[n]
         cnt += (pv<v);  // if v is 1 & prev v was zero, inc cnt
         pv = v;
      }
   }
   return(cnt.toFloat() / runs / len);
}
```


```zkl
println("Running 1000 tests each:\n"
	" p\t   n\tK\tp(1-p)\t     diff\n"
	"-----------------------------------------------");
foreach p in ([0.1..0.9,0.2]) {
   p1p:=p*(1.0 - p);
   n:=100; while(n <= 100000) {
      K:=run_test(p, n, 1000);
      "%.1f\t%6d\t%.4f\t%.4f\t%+.4f (%+.2f%%)".fmt(
	      p, n, K, p1p, K - p1p, (K - p1p) / p1p * 100).println();
      n *= 10;
   }
   println();
}
```

```txt

Running 1000 tests each:
 p	   n	K	p(1-p)	     diff
-----------------------------------------------
0.1	   100	0.0903	0.0900	+0.0003 (+0.36%)
0.1	  1000	0.0900	0.0900	-0.0000 (-0.01%)
0.1	 10000	0.0901	0.0900	+0.0001 (+0.16%)
0.1	100000	0.0900	0.0900	+0.0000 (+0.01%)

0.3	   100	0.2115	0.2100	+0.0015 (+0.73%)
0.3	  1000	0.2105	0.2100	+0.0005 (+0.23%)
0.3	 10000	0.2098	0.2100	-0.0002 (-0.07%)
0.3	100000	0.2100	0.2100	+0.0000 (+0.00%)

0.5	   100	0.2521	0.2500	+0.0021 (+0.86%)
0.5	  1000	0.2503	0.2500	+0.0003 (+0.13%)
0.5	 10000	0.2500	0.2500	-0.0000 (-0.01%)
0.5	100000	0.2500	0.2500	-0.0000 (-0.00%)

0.7	   100	0.2151	0.2100	+0.0051 (+2.41%)
0.7	  1000	0.2103	0.2100	+0.0003 (+0.16%)
0.7	 10000	0.2100	0.2100	+0.0000 (+0.00%)
0.7	100000	0.2100	0.2100	-0.0000 (-0.01%)

0.9	   100	0.0979	0.0900	+0.0079 (+8.74%)
0.9	  1000	0.0911	0.0900	+0.0011 (+1.17%)
0.9	 10000	0.0902	0.0900	+0.0002 (+0.18%)
0.9	100000	0.0900	0.0900	-0.0000 (-0.00%)

```
