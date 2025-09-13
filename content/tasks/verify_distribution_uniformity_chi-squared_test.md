+++
title = "Verify distribution uniformity/Chi-squared test"
description = ""
date = 2019-09-10T19:51:36Z
aliases = []
[extra]
id = 4688
[taxonomies]
categories = ["task", "Probability and statistics"]
tags = []
+++

## Task

Write a function to verify that a given distribution of values is uniform by using the [[wp:Pearson's chi-square test|<math>\chi^2</math> test]] to see if the distribution has a likelihood of happening of at least the significance level (conventionally 5%). The function should return a boolean that is true if the distribution is one that a uniform distribution (with appropriate number of degrees of freedom) may be expected to produce.


;Reference:
:*   an entry at the MathWorld website:   [http://mathworld.wolfram.com/Chi-SquaredDistribution.html chi-squared distribution].





## Ada

First, we specify a simple package to compute the Chi-Square Distance from the uniform distribution:

```Ada
package Chi_Square is

   type Flt is digits 18;
   type Bins_Type is array(Positive range <>) of Natural;

   function Distance(Bins: Bins_Type) return Flt;

end Chi_Square;
```


Next, we implement that package:


```Ada
package body Chi_Square is

   function Distance(Bins: Bins_Type) return Flt is
      Bad_Bins: Natural := 0;
      Sum: Natural := 0;
      Expected: Flt;
      Result: Flt;
   begin
      for I in Bins'Range loop
         if Bins(I) < 5 then
            Bad_Bins := Bad_Bins + 1;
         end if;
         Sum := Sum + Bins(I);
      end loop;
      if 5*Bad_Bins > Bins'Length then
         raise Program_Error with "too many (almost) empty bins";
      end if;

      Expected := Flt(Sum) / Flt(Bins'Length);
      Result := 0.0;
      for I in Bins'Range loop
         Result := Result + ((Flt(Bins(I)) - Expected)**2) / Expected;
      end loop;
      return Result;
   end Distance;

end Chi_Square;
```


Finally, we actually implement the Chi-square test. We do not actually compute the Chi-square probability; rather we hardcode a table of values for 5% significance level, which has been picked from Wikipedia [http://en.wikipedia.org/wiki/Chi-squared_distribution]:

```Ada
with Ada.Text_IO, Ada.Command_Line, Chi_Square; use Ada.Text_IO;

procedure Test_Chi_Square is

   package Ch2 renames Chi_Square; use Ch2;
   package FIO is new Float_IO(Flt);

   B: Bins_Type(1 .. Ada.Command_Line.Argument_Count);
   Bound_For_5_Per_Cent: constant array(Positive range <>) of Flt :=
     ( 1 => 3.84,   2 =>  5.99,  3 =>  7.82,  4 => 9.49,   5 =>  11.07,
       6 => 12.59,  7 => 14.07,  8 => 15.51,  9 => 16.92, 10 =>  18.31);
     -- picked from http://en.wikipedia.org/wiki/Chi-squared_distribution

   Dist: Flt;

begin
   for I in B'Range loop
      B(I) := Natural'Value(Ada.Command_Line.Argument(I));
   end loop;
   Dist := Distance(B);
   Put("Degrees of Freedom:" & Integer'Image(B'Length-1) & ", Distance: ");
   FIO.Put(Dist, Fore => 6, Aft => 2, Exp => 0);
   if Dist <= Bound_For_5_Per_Cent(B'Length-1) then
      Put_Line("; (apparently uniform)");
   else
      Put_Line("; (deviates significantly from uniform)");
   end if;
end;
```


```txt
$ ./Test_Chi_Square 199809 200665 199607 200270 199649
Degrees of Freedom: 4, Distance:      4.15; (apparently uniform)
$ ./Test_Chi_Square 522573 244456 139979 71531 21461
Degrees of Freedom: 4, Distance: 790063.28; (deviates significantly from uniform)
```



## C

This first sections contains the functions required to compute the Chi-Squared probability.
These are not needed if a library containing the necessary function is availabile (e.g. see [[Numerical Integration]], [[Gamma function]]).

```cpp
#include <iostream>
#include <stdio.h>
#include <math.h>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

typedef double (* Ifctn)( double t);
/* Numerical integration method */
double Simpson3_8( Ifctn f, double a, double b, int N)
{
    int j;
    double l1;
    double h = (b-a)/N;
    double h1 = h/3.0;
    double sum = f(a) + f(b);

    for (j=3*N-1; j>0; j--) {
        l1 = (j%3)? 3.0 : 2.0;
        sum += l1*f(a+h1*j) ;
    }
    return h*sum/8.0;
}

#define A 12
double Gamma_Spouge( double z )
{
    int k;
    static double cspace[A];
    static double *coefs = NULL;
    double accum;
    double a = A;

    if (!coefs) {
        double k1_factrl = 1.0;
        coefs = cspace;
        coefs[0] = sqrt(2.0*M_PI);
        for(k=1; k<A; k++) {
            coefs[k] = exp(a-k) * pow(a-k,k-0.5) / k1_factrl;
            k1_factrl *= -k;
        }
    }

    accum = coefs[0];
    for (k=1; k<A; k++) {
        accum += coefs[k]/(z+k);
    }
    accum *= exp(-(z+a)) * pow(z+a, z+0.5);
    return accum/z;
}

double aa1;
double f0( double t)
{
    return  pow(t, aa1)*exp(-t);
}

double GammaIncomplete_Q( double a, double x)
{
    double y, h = 1.5e-2;  /* approximate integration step size */

    /* this cuts off the tail of the integration to speed things up */
    y = aa1 = a-1;
    while((f0(y) * (x-y) > 2.0e-8) && (y < x))   y += .4;
    if (y>x) y=x;

    return 1.0 - Simpson3_8( &f0, 0, y, (int)(y/h))/Gamma_Spouge(a);
}
```

This section contains the functions specific to the task.

```c
double chi2UniformDistance( double *ds, int dslen)
{
    double expected = 0.0;
    double sum = 0.0;
    int k;

    for (k=0; k<dslen; k++)
        expected += ds[k];
    expected /= k;

    for (k=0; k<dslen; k++) {
        double x = ds[k] - expected;
        sum += x*x;
    }
    return sum/expected;
}

double chi2Probability( int dof, double distance)
{
    return GammaIncomplete_Q( 0.5*dof, 0.5*distance);
}

int chiIsUniform( double *dset, int dslen, double significance)
{
    int dof = dslen -1;
    double dist = chi2UniformDistance( dset, dslen);
    return chi2Probability( dof, dist ) > significance;
}
```

Testing

```c
int main(int argc, char **argv)
{
    double dset1[] = { 199809., 200665., 199607., 200270., 199649. };
    double dset2[] = { 522573., 244456., 139979.,  71531.,  21461. };
    double *dsets[] = { dset1, dset2 };
    int     dslens[] = { 5, 5 };
    int k, l;
    double  dist, prob;
    int dof;

    for (k=0; k<2; k++) {
        printf("Dataset: [ ");
        for(l=0;l<dslens[k]; l++)
            printf("%.0f, ", dsets[k][l]);

        printf("]\n");
        dist = chi2UniformDistance(dsets[k], dslens[k]);
        dof = dslens[k]-1;
        printf("dof: %d  distance: %.4f", dof, dist);
        prob = chi2Probability( dof, dist );
        printf(" probability: %.6f", prob);
        printf(" uniform? %s\n", chiIsUniform(dsets[k], dslens[k], 0.05)? "Yes":"No");
    }
    return 0;
}
```



## D


```d
import std.stdio, std.algorithm, std.mathspecial;

real x2Dist(T)(in T[] data) pure nothrow @safe @nogc {
    immutable avg = data.sum / data.length;
    immutable sqs = reduce!((a, b) => a + (b - avg) ^^ 2)(0.0L, data);
    return sqs / avg;
}

real x2Prob(in real dof, in real distance) pure nothrow @safe @nogc {
    return gammaIncompleteCompl(dof / 2, distance / 2);
}

bool x2IsUniform(T)(in T[] data, in real significance=0.05L)
pure nothrow @safe @nogc {
    return x2Prob(data.length - 1.0L, x2Dist(data)) > significance;
}

void main() {
    immutable dataSets = [[199809, 200665, 199607, 200270, 199649],
                          [522573, 244456, 139979,  71531,  21461]];
    writefln(" %4s %12s  %12s %8s   %s",
             "dof", "distance", "probability", "Uniform?", "dataset");
    foreach (immutable ds; dataSets) {
        immutable dof = ds.length - 1;
        immutable dist = ds.x2Dist;
        immutable prob = x2Prob(dof, dist);
        writefln("%4d %12.3f  %12.8f    %5s    %6s",
                 dof, dist, prob, ds.x2IsUniform ? "YES" : "NO", ds);
    }
}
```

```txt
  dof     distance   probability Uniform?   dataset
   4        4.146    0.38657083      YES    [199809, 200665, 199607, 200270, 199649]
   4   790063.276    0.00000000       NO    [522573, 244456, 139979,  71531,  21461]
```



## Elixir

```elixir
defmodule Verify do
  defp gammaInc_Q(a, x) do
    a1 = a-1
    f0  = fn t -> :math.pow(t, a1) * :math.exp(-t) end
    df0 = fn t -> (a1-t) * :math.pow(t, a-2) * :math.exp(-t) end
    y = while_loop(f0, x, a1)
    n = trunc(y / 3.0e-4)
    h = y / n
    hh = 0.5 * h
    sum = Enum.reduce(n-1 .. 0, 0, fn j,sum ->
      t = h * j
      sum + f0.(t) + hh * df0.(t)
    end)
    h * sum / gamma_spounge(a, make_coef)
  end

  defp while_loop(f, x, y) do
    if f.(y)*(x-y) > 2.0e-8 and y < x, do: while_loop(f, x, y+0.3), else: min(x, y)
  end

  @a  12
  defp make_coef do
    coef0 = [:math.sqrt(2.0 * :math.pi)]
    {_, coef} = Enum.reduce(1..@a-1, {1.0, coef0}, fn k,{k1_factrl,c} ->
      h = :math.exp(@a-k) * :math.pow(@a-k, k-0.5) / k1_factrl
      {-k1_factrl*k, [h | c]}
    end)
    Enum.reverse(coef) |> List.to_tuple
  end

  defp gamma_spounge(z, coef) do
    accm = Enum.reduce(1..@a-1, elem(coef,0), fn k,res -> res + elem(coef,k) / (z+k) end)
    accm * :math.exp(-(z+@a)) * :math.pow(z+@a, z+0.5) / z
  end

  def chi2UniformDistance(dataSet) do
    expected = Enum.sum(dataSet) / length(dataSet)
    Enum.reduce(dataSet, 0, fn d,sum -> sum + (d-expected)*(d-expected) end) / expected
  end

  def chi2Probability(dof, distance) do
    1.0 - gammaInc_Q(0.5*dof, 0.5*distance)
  end

  def chi2IsUniform(dataSet, significance\\0.05) do
    dof = length(dataSet) - 1
    dist = chi2UniformDistance(dataSet)
    chi2Probability(dof, dist) > significance
  end
end

dsets = [ [ 199809, 200665, 199607, 200270, 199649 ],
          [ 522573, 244456, 139979,  71531,  21461 ] ]

Enum.each(dsets, fn ds ->
  IO.puts "Data set:#{inspect ds}"
  dof = length(ds) - 1
  IO.puts "  degrees of freedom: #{dof}"
  distance = Verify.chi2UniformDistance(ds)
  :io.fwrite "  distance:           ~.4f~n", [distance]
  :io.fwrite "  probability:        ~.4f~n", [Verify.chi2Probability(dof, distance)]
  :io.fwrite "  uniform?            ~s~n", [(if Verify.chi2IsUniform(ds), do: "Yes", else: "No")]
end)
```


```txt

Data set:[199809, 200665, 199607, 200270, 199649]
  degrees of freedom: 4
  distance:           4.1463
  probability:        0.3866
  uniform?            Yes
Data set:[522573, 244456, 139979, 71531, 21461]
  degrees of freedom: 4
  distance:           790063.2759
  probability:        -0.0000
  uniform?            No

```



## Fortran

Instead of implementing the incomplete gamma function by ourselves, we bind to GNU Scientific Library; so we need a module to interface to the function we need (<tt>gsl_sf_gamma_inc</tt>)


```fortran
module GSLMiniBind
  implicit none

  interface
     real(c_double) function gsl_sf_gamma_inc(a, x) bind(C)
       use iso_c_binding
       real(c_double), value, intent(in) :: a, x
     end function gsl_sf_gamma_inc
  end interface
end module GSLMiniBind
```


Now we're ready to complete the task.


```fortran
program ChiTest
  use GSLMiniBind
  use iso_c_binding
  implicit none

  real, dimension(5) :: dset1 = (/ 199809., 200665., 199607., 200270., 199649. /)
  real, dimension(5) :: dset2 = (/ 522573., 244456., 139979.,  71531.,  21461. /)

  real :: dist, prob
  integer :: dof

  print *, "Dataset 1:"
  print *, dset1
  dist = chi2UniformDistance(dset1)
  dof = size(dset1) - 1
  write(*, '(A,I4,A,F12.4)') 'dof: ', dof, '   distance: ', dist
  prob = chi2Probability(dof, dist)
  write(*, '(A,F9.6)') 'probability: ', prob
  write(*, '(A,L)') 'uniform? ', chiIsUniform(dset1, 0.05)

  ! Lazy copy/past :|
  print *, "Dataset 2:"
  print *, dset2
  dist = chi2UniformDistance(dset2)
  dof = size(dset2) - 1
  write(*, '(A,I4,A,F12.4)') 'dof: ', dof, '   distance: ', dist
  prob = chi2Probability(dof, dist)
  write(*, '(A,F9.6)') 'probability: ', prob
  write(*, '(A,L)') 'uniform? ', chiIsUniform(dset2, 0.05)

contains

function chi2Probability(dof, distance)
  real :: chi2Probability
  integer, intent(in) :: dof
  real, intent(in) :: distance

  ! in order to make this work, we need linking with GSL library
  chi2Probability = gsl_sf_gamma_inc(real(0.5*dof, c_double), real(0.5*distance, c_double))
end function chi2Probability

function chiIsUniform(dset, significance)
  logical :: chiIsUniform
  real, dimension(:), intent(in) :: dset
  real, intent(in) :: significance

  integer :: dof
  real :: dist

  dof = size(dset) - 1
  dist = chi2UniformDistance(dset)
  chiIsUniform = chi2Probability(dof, dist) > significance
end function chiIsUniform

function chi2UniformDistance(ds)
  real :: chi2UniformDistance
  real, dimension(:), intent(in) :: ds

  real :: expected, summa = 0.0

  expected = sum(ds) / size(ds)
  summa = sum( (ds - expected) ** 2 )

  chi2UniformDistance = summa / expected
end function chi2UniformDistance

end program ChiTest
```



## Go

Go has a nice gamma function in the library.  Otherwise, it's mostly a port from C.  Note, this implementation of the incomplete gamma function works for these two test cases, but, I believe, has serious limitations.  See talk page.

```go
package main

import (
    "fmt"
    "math"
)

type ifctn func(float64) float64

func simpson38(f ifctn, a, b float64, n int) float64 {
    h := (b - a) / float64(n)
    h1 := h / 3
    sum := f(a) + f(b)
    for j := 3*n - 1; j > 0; j-- {
        if j%3 == 0 {
            sum += 2 * f(a+h1*float64(j))
        } else {
            sum += 3 * f(a+h1*float64(j))
        }
    }
    return h * sum / 8
}

func gammaIncQ(a, x float64) float64 {
    aa1 := a - 1
    var f ifctn = func(t float64) float64 {
        return math.Pow(t, aa1) * math.Exp(-t)
    }
    y := aa1
    h := 1.5e-2
    for f(y)*(x-y) > 2e-8 && y < x {
        y += .4
    }
    if y > x {
        y = x
    }
    return 1 - simpson38(f, 0, y, int(y/h/math.Gamma(a)))
}

func chi2ud(ds []int) float64 {
    var sum, expected float64
    for _, d := range ds {
        expected += float64(d)
    }
    expected /= float64(len(ds))
    for _, d := range ds {
        x := float64(d) - expected
        sum += x * x
    }
    return sum / expected
}

func chi2p(dof int, distance float64) float64 {
    return gammaIncQ(.5*float64(dof), .5*distance)
}

const sigLevel = .05

func main() {
    for _, dset := range [][]int{
        {199809, 200665, 199607, 200270, 199649},
        {522573, 244456, 139979, 71531, 21461},
    } {
        utest(dset)
    }
}

func utest(dset []int) {
    fmt.Println("Uniform distribution test")
    var sum int
    for _, c := range dset {
        sum += c
    }
    fmt.Println(" dataset:", dset)
    fmt.Println(" samples:                      ", sum)
    fmt.Println(" categories:                   ", len(dset))

    dof := len(dset) - 1
    fmt.Println(" degrees of freedom:           ", dof)

    dist := chi2ud(dset)
    fmt.Println(" chi square test statistic:    ", dist)

    p := chi2p(dof, dist)
    fmt.Println(" p-value of test statistic:    ", p)

    sig := p < sigLevel
    fmt.Printf(" significant at %2.0f%% level?      %t\n", sigLevel*100, sig)
    fmt.Println(" uniform?                      ", !sig, "\n")
}
```

Output:

```txt

Uniform distribution test
 dataset: [199809 200665 199607 200270 199649]
 samples:                       1000000
 categories:                    5
 degrees of freedom:            4
 chi square test statistic:     4.14628
 p-value of test statistic:     0.3865708330827673
 significant at  5% level?      false
 uniform?                       true

Uniform distribution test
 dataset: [522573 244456 139979 71531 21461]
 samples:                       1000000
 categories:                    5
 degrees of freedom:            4
 chi square test statistic:     790063.27594
 p-value of test statistic:     2.3528290427066167e-11
 significant at  5% level?      true
 uniform?                       false

```



## Hy


```lisp
(import
  [scipy.stats [chisquare]]
  [collections [Counter]])

(defn uniform? [f repeats &optional [alpha .05]]
  "Call 'f' 'repeats' times and do a chi-squared test for uniformity
  of the resulting discrete distribution. Return false iff the
  null hypothesis of uniformity is rejected for the test with
  size 'alpha'."
  (<= alpha (second (chisquare
    (.values (Counter (take repeats (repeatedly f))))))))
```


Examples of use:


```lisp
(import [random [randint]])

(for [f [
    (fn [] (randint 1 10))
    (fn [] (if (randint 0 1) (randint 1 9) (randint 1 10)))]]
  (print (uniform? f 5000)))
```



## J

'''Solution (Tacit):'''

```j
require 'stats/base'

countCats=: #@~.                    NB. counts the number of unique items
getExpected=: #@] % [               NB. divides no of items by category count
getObserved=: #/.~@]                NB. counts frequency for each category
calcX2=: [: +/ *:@(getObserved - getExpected) % getExpected   NB. calculates test statistic
calcDf=: <:@[                       NB. calculates degrees of freedom for uniform distribution

NB.*isUniform v Tests (5%) whether y is uniformly distributed
NB. result is: boolean describing if distribution y is uniform
NB. y is: distribution to test
NB. x is: optionally specify number of categories possible
isUniform=: (countCats $: ]) : (0.95 > calcDf chisqcdf :: 1: calcX2)
```


'''Solution (Explicit):'''

```j
require 'stats/base'

NB.*isUniformX v Tests (5%) whether y is uniformly distributed
NB. result is: boolean describing if distribution y is uniform
NB. y is: distribution to test
NB. x is: optionally specify number of categories possible
isUniformX=: verb define
  (#~. y) isUniformX y
:
  signif=. 0.95                    NB. set significance level
  expected=. (#y) % x              NB. number of items divided by the category count
  observed=. #/.~ y                NB. frequency count for each category
  X2=. +/ (*: observed - expected) % expected  NB. the test statistic
  degfreedom=. <: x                NB. degrees of freedom
  signif > degfreedom chisqcdf :: 1: X2
)
```


'''Example Usage:'''

```j
   FairDistrib=:      1e6 ?@$ 5
   UnfairDistrib=: (9.5e5 ?@$ 5) , (5e4 ?@$ 4)
   isUniformX FairDistrib
1
   isUniformX UnfairDistrib
0
   isUniform 4 4 4 5 5 5 5 5 5 5     NB. uniform if only 2 categories possible
1
   4 isUniform 4 4 4 5 5 5 5 5 5 5   NB. not uniform if 4 categories possible
0
```



## Java

```java
import static java.lang.Math.pow;
import java.util.Arrays;
import static java.util.Arrays.stream;
import org.apache.commons.math3.special.Gamma;

public class Test {

    static double x2Dist(double[] data) {
        double avg = stream(data).sum() / data.length;
        double sqs = stream(data).reduce(0, (a, b) -> a + pow((b - avg), 2));
        return sqs / avg;
    }

    static double x2Prob(double dof, double distance) {
        return Gamma.regularizedGammaQ(dof / 2, distance / 2);
    }

    static boolean x2IsUniform(double[] data, double significance) {
        return x2Prob(data.length - 1.0, x2Dist(data)) > significance;
    }

    public static void main(String[] a) {
        double[][] dataSets = {{199809, 200665, 199607, 200270, 199649},
        {522573, 244456, 139979, 71531, 21461}};

        System.out.printf(" %4s %12s  %12s %8s   %s%n",
                "dof", "distance", "probability", "Uniform?", "dataset");

        for (double[] ds : dataSets) {
            int dof = ds.length - 1;
            double dist = x2Dist(ds);
            double prob = x2Prob(dof, dist);
            System.out.printf("%4d %12.3f  %12.8f    %5s    %6s%n",
                    dof, dist, prob, x2IsUniform(ds, 0.05) ? "YES" : "NO",
                    Arrays.toString(ds));
        }
    }
}
```


```txt
  dof     distance   probability Uniform?   dataset
   4        4,146    0,38657083      YES    [199809.0, 200665.0, 199607.0, 200270.0, 199649.0]
   4   790063,276    0,00000000       NO    [522573.0, 244456.0, 139979.0, 71531.0, 21461.0]
```



## Julia


```julia
# v0.6

using Distributions

function eqdist(data::Vector{T}, α::Float64=0.05)::Bool where T <: Real
    if ! (0 ≤ α ≤ 1); error("α must be in [0, 1]") end
    exp = mean(data)
    chisqval = sum((x - exp) ^ 2 for x in data) / exp
    pval = ccdf(Chisq(2), chisqval)
    return pval > α
end

data1 = [199809, 200665, 199607, 200270, 199649]
data2 = [522573, 244456, 139979,  71531,  21461]

for data in (data1, data2)
    println("Data:\n$data")
    println("Hypothesis test: the original population is ", (eqdist(data) ? "" : "not "), "uniform.\n")
end
```


```txt
Data:
[199809, 200665, 199607, 200270, 199649]
Hypothesis test: the original population is uniform.

Data:
[522573, 244456, 139979, 71531, 21461]
Hypothesis test: the original population is not uniform.

```



## Kotlin

This program reuses Kotlin code from the [[Gamma function]] and [[Numerical Integration]] tasks but otherwise is a translation of the C entry for this task.

```scala
// version 1.1.51

typealias Func = (Double) -> Double

fun gammaLanczos(x: Double): Double {
    var xx = x
    val p = doubleArrayOf(
        0.99999999999980993,
      676.5203681218851,
    -1259.1392167224028,
      771.32342877765313,
     -176.61502916214059,
       12.507343278686905,
       -0.13857109526572012,
        9.9843695780195716e-6,
        1.5056327351493116e-7
    )
    val g = 7
    if (xx < 0.5) return Math.PI / (Math.sin(Math.PI * xx) * gammaLanczos(1.0 - xx))
    xx--
    var a = p[0]
    val t = xx + g + 0.5
    for (i in 1 until p.size) a += p[i] / (xx + i)
    return Math.sqrt(2.0 * Math.PI) * Math.pow(t, xx + 0.5) * Math.exp(-t) * a
}

fun integrate(a: Double, b: Double, n: Int, f: Func): Double {
    val h = (b - a) / n
    var sum = 0.0
    for (i in 0 until n) {
        val x = a + i * h
        sum += (f(x) + 4.0 * f(x + h / 2.0) + f(x + h)) / 6.0
    }
    return sum * h
}

fun gammaIncompleteQ(a: Double, x: Double): Double {
    val aa1 = a - 1.0
    fun f0(t: Double) = Math.pow(t, aa1) * Math.exp(-t)
    val h = 1.5e-2
    var y = aa1
    while ((f0(y) * (x - y) > 2.0e-8) && y < x) y += 0.4
    if (y > x) y = x
    return 1.0 - integrate(0.0, y, (y / h).toInt(), ::f0) / gammaLanczos(a)
}

fun chi2UniformDistance(ds: DoubleArray): Double {
    val expected = ds.average()
    val sum = ds.map { val x = it - expected; x * x }.sum()
    return sum / expected
}

fun chi2Probability(dof: Int, distance: Double) =
    gammaIncompleteQ(0.5 * dof, 0.5 * distance)

fun chiIsUniform(ds: DoubleArray, significance: Double):Boolean {
    val dof = ds.size - 1
    val dist = chi2UniformDistance(ds)
    return chi2Probability(dof, dist) > significance
}

fun main(args: Array<String>) {
    val dsets = listOf(
        doubleArrayOf(199809.0, 200665.0, 199607.0, 200270.0, 199649.0),
        doubleArrayOf(522573.0, 244456.0, 139979.0,  71531.0,  21461.0)
    )
    for (ds in dsets) {
        println("Dataset: ${ds.asList()}")
        val dist = chi2UniformDistance(ds)
        val dof = ds.size - 1
        print("DOF: $dof  Distance: ${"%.4f".format(dist)}")
        val prob = chi2Probability(dof, dist)
        print("  Probability: ${"%.6f".format(prob)}")
        val uniform = if (chiIsUniform(ds, 0.05)) "Yes" else "No"
        println("  Uniform? $uniform\n")
    }
}
```


```txt

Dataset: [199809.0, 200665.0, 199607.0, 200270.0, 199649.0]
DOF: 4  Distance: 4.1463  Probability: 0.386571  Uniform? Yes

Dataset: [522573.0, 244456.0, 139979.0, 71531.0, 21461.0]
DOF: 4  Distance: 790063.2759  Probability: 0.000000  Uniform? No

```



## Mathematica

This code explicity assumes a discrete uniform distribution since the chi square test is a poor test choice for continuous distributions and requires Mathematica version 2 or later

```Mathematica
discreteUniformDistributionQ[data_, {min_Integer, max_Integer}, confLevel_: .05] :=
If[$VersionNumber >= 8,
  confLevel <= PearsonChiSquareTest[data, DiscreteUniformDistribution[{min, max}]],
  Block[{v, k = max - min, n = Length@data},
   v = (k + 1) (Plus @@ (((Length /@ Split[Sort@data]))^2))/n - n;
   GammaRegularized[k/2, 0, v/2] <= 1 - confLevel]]

discreteUniformDistributionQ[data_] :=discreteUniformDistributionQ[data, data[[Ordering[data][[{1, -1}]]]]]
```

code used to create test data requires Mathematica version 6 or later

```Mathematica
uniformData = RandomInteger[10, 100];
nonUniformData = Total@RandomInteger[10, {5, 100}];
```


```Mathematica
{discreteUniformDistributionQ[uniformData],discreteUniformDistributionQ[nonUniformData]}
```

```txt
{True,False}
```



## OCaml

This code needs to be compiled with library [http://oandrieu.nerim.net/ocaml/gsl/ gsl.cma].


```ocaml
let sqr x = x *. x

let chi2UniformDistance distrib =
  let count, len = Array.fold_left (fun (s, c) e -> s + e, succ c)
  				   (0, 0) distrib in
  let expected = float count /. float len in
  let distance = Array.fold_left (fun s e ->
    s +. sqr (float e -. expected) /. expected
  ) 0. distrib in
  let dof = float (pred len) in
  dof, distance

let chi2Proba dof distance =
  Gsl_sf.gamma_inc_Q (0.5 *. dof) (0.5 *. distance)

let chi2IsUniform distrib significance =
  let dof, distance = chi2UniformDistance distrib in
  let likelihoodOfRandom = chi2Proba dof distance in
  likelihoodOfRandom > significance

let _ =
  List.iter (fun distrib ->
    let dof, distance = chi2UniformDistance distrib in
    Printf.printf "distribution ";
    Array.iter (Printf.printf "\t%d") distrib;
    Printf.printf "\tdistance %g" distance;
    Printf.printf "\t[%g > 0.05]" (chi2Proba dof distance);
    if chi2IsUniform distrib 0.05 then Printf.printf " fair\n"
    else Printf.printf " unfair\n"
  )
  [
    [| 199809; 200665; 199607; 200270; 199649 |];
    [| 522573; 244456; 139979; 71531; 21461 |]
  ]
```


Output

```txt

distribution    199809  200665  199607  200270  199649  distance 4.14628        [0.386571 > 0.05] fair
distribution    522573  244456  139979  71531   21461   distance 790063 [0 > 0.05] unfair

```



## PARI/GP

The solution is very easy in GP since PARI includes a good incomplete gamma implementation; the sum function is also useful for clarity.  Most of the code is just for displaying results.

The sample data for the test was taken from [[#Go|Go]].

```parigp
cumChi2(chi2,dof)={
	my(g=gamma(dof/2));
	incgam(dof/2,chi2/2,g)/g
};
test(v,alpha=.05)={
	my(chi2,p,s=sum(i=1,#v,v[i]),ave=s/#v);
	print("chi^2 statistic: ",chi2=sum(i=1,#v,(v[i]-ave)^2)/ave);
	print("p-value: ",p=cumChi2(chi2,#v-1));
	if(p<alpha,
		print("Significant at the alpha = "alpha" level: not uniform");
	,
		print("Not significant at the alpha = "alpha" level: uniform");
	)
};

test([199809, 200665, 199607, 200270, 199649])
test([522573, 244456, 139979, 71531, 21461])
```



## Perl

```perl
use List::Util qw(sum reduce);
use constant pi => 3.14159265;

sub incomplete_G_series {
    my($s, $z) = @_;
    my $n = 10;
    push @numers, $z**$_ for 1..$n;
    my @denoms = $s+1;
    push @denoms, $denoms[-1]*($s+$_) for 2..$n;
    my $M = 1;
    $M += $numers[$_-1]/$denoms[$_-1] for 1..$n;
    $z**$s / $s * exp(-$z) * $M;
}

sub G_of_half {
    my($n) = @_;
    if ($n % 2) { f(2*$_) / (4**$_ * f($_)) * sqrt(pi) for int ($n-1) / 2 }
    else        { f(($n/2)-1) }
}

sub f { reduce { $a * $b } 1, 1 .. $_[0] } # factorial

sub chi_squared_cdf {
    my($k, $x) = @_;
    my $f = $k < 20 ? 20 : 10;
    if ($x == 0)                  { 0.0 }
    elsif ($x < $k + $f*sqrt($k)) { incomplete_G_series($k/2, $x/2) / G_of_half($k) }
    else                          { 1.0 }
}
sub chi_squared_test {
    my(@bins) = @_;
    $significance = 0.05;
    my $n = @bins;
    my $N = sum @bins;
    my $expected = $N / $n;
    my $chi_squared = sum map { ($_ - $expected)**2 / $expected } @bins;
    my $p_value = 1 - chi_squared_cdf($n-1, $chi_squared);
    return $chi_squared, $p_value, $p_value > $significance ? 'True' : 'False';
}

for $dataset ([199809, 200665, 199607, 200270, 199649], [522573, 244456, 139979, 71531, 21461]) {
    printf "C2 = %10.3f, p-value = %.3f, uniform = %s\n", chi_squared_test(@$dataset);
}
```

```txt
C2 =      4.146, p-value = 0.387, uniform = True
C2 = 790063.276, p-value = 0.000, uniform = False
```



## Perl 6


For the incomplete gamma function we use a series expansion related to Kummer's confluent hypergeometric function
(see http://en.wikipedia.org/wiki/Incomplete_gamma_function#Evaluation_formulae). The gamma function is calculated
in closed form, as we only need its value at integers and half integers.


```perl6
sub incomplete-γ-series($s, $z) {
    my \numers = $z X** 1..*;
    my \denoms = [\*] $s X+ 1..*;
    my $M = 1 + [+] (numers Z/ denoms) ... * < 1e-6;
    $z**$s / $s * exp(-$z) * $M;
}

sub postfix:<!>(Int $n) { [*] 2..$n }

sub Γ-of-half(Int $n where * > 0) {
    ($n %% 2) ?? (($_-1)!                            given  $n    div 2)
              !! ((2*$_)! / (4**$_ * $_!) * sqrt(pi) given ($n-1) div 2);
}

# degrees of freedom constrained due to numerical limitations
sub chi-squared-cdf(Int $k where 1..200, $x where * >= 0) {
    my $f = $k < 20 ?? 20 !! 10;
    given $x {
        when 0                    { 0.0 }
        when * < $k + $f*sqrt($k) { incomplete-γ-series($k/2, $x/2) / Γ-of-half($k) }
        default                   { 1.0 }
    }
}

sub chi-squared-test(@bins, :$significance = 0.05) {
    my $n = +@bins;
    my $N = [+] @bins;
    my $expected = $N / $n;
    my $chi-squared = [+] @bins.map: { ($^bin - $expected)**2 / $expected }
    my $p-value = 1 - chi-squared-cdf($n-1, $chi-squared);
    return (:$chi-squared, :$p-value, :uniform($p-value > $significance));
}

for [< 199809 200665 199607 200270 199649 >],
    [< 522573 244456 139979  71531  21461 >]
    -> $dataset
{
    my %t = chi-squared-test($dataset);
    say 'data: ', $dataset;
    say "χ² = {%t<chi-squared>}, p-value = {%t<p-value>.fmt('%.4f')}, uniform = {%t<uniform>}";
}
```

```txt
data: 199809 200665 199607 200270 199649
χ² = 4.14628, p-value = 0.3866, uniform = True
data: 522573 244456 139979 71531 21461
χ² = 790063.27594, p-value = 0.0000, uniform = False
```



## Phix

using gamma() from [[Gamma_function#Phix]]

```Phix
function f(atom aa1, t)
    return power(t, aa1) * exp(-t)
end function

function simpson38(atom aa1, a, b, integer n)
    atom h := (b - a) / n,
         h1 := h / 3,
         tot := f(aa1,a) + f(aa1,b)
    for j=3*n-1 to 1 by -1 do
        tot += (3-(mod(j,3)=0)) * f(aa1,a+h1*j)
    end for
    return h * tot / 8
end function

function gammaIncQ(atom a, x)
    atom aa1 := a - 1,
         y := aa1,
         h := 1.5e-2
    while f(aa1,y)*(x-y) > 2e-8 and y < x do
        y += 0.4
    end while
    if y > x then y = x end if
    return 1 - simpson38(aa1, 0, y, floor(y/h/gamma(a)))
end function

function chi2ud(sequence ds)
    atom expected = sum(ds)/length(ds),
         tot = sum(sq_power(sq_sub(ds,expected),2))
    return tot / expected
end function

function chi2p(integer dof, atom distance)
    return gammaIncQ(0.5*dof, 0.5*distance)
end function

constant sigLevel = 0.05
constant tf = {"true","false"}

procedure utest(sequence dset)
    printf(1,"Uniform distribution test\n")
    integer tot = sum(dset)
    printf(1," dataset:%s\n",{sprint(dset)})
    printf(1," samples:                      %d\n", tot)
    printf(1," categories:                   %d\n", length(dset))

    integer dof := length(dset) - 1
    printf(1," degrees of freedom:           %d\n", dof)

    atom dist := chi2ud(dset)
    printf(1," chi square test statistic:    %g\n", dist)

    atom p := chi2p(dof, dist)
    printf(1," p-value of test statistic:    %g\n", p)

    bool sig := p < sigLevel
    printf(1," significant at %2.0f%% level?     %s\n", {sigLevel*100, tf[2-sig]})
    printf(1," uniform?                      %s\n",{tf[sig+1]})
end procedure

utest({199809, 200665, 199607, 200270, 199649})
utest({522573, 244456, 139979, 71531, 21461})
```

```txt

Uniform distribution test
 dataset:{199809,200665,199607,200270,199649}
 samples:                      1000000
 categories:                   5
 degrees of freedom:           4
 chi square test statistic:    4.14628
 p-value of test statistic:    0.386571
 significant at  5% level?     false
 uniform?                      true
Uniform distribution test
 dataset:{522573,244456,139979,71531,21461}
 samples:                      1000000
 categories:                   5
 degrees of freedom:           4
 chi square test statistic:    790063
 p-value of test statistic:    2.35282e-11
 significant at  5% level?     true
 uniform?                      false

```



## Python

Implements the Chi Square Probability function with an integration. I'm
sure there are better ways to do this. Compare to OCaml implementation.

```python
import math
import random

def GammaInc_Q( a, x):
    a1 = a-1
    a2 = a-2
    def f0( t ):
        return t**a1*math.exp(-t)

    def df0(t):
        return (a1-t)*t**a2*math.exp(-t)

    y = a1
    while f0(y)*(x-y) >2.0e-8 and y < x: y += .3
    if y > x: y = x

    h = 3.0e-4
    n = int(y/h)
    h = y/n
    hh = 0.5*h
    gamax = h * sum( f0(t)+hh*df0(t) for t in ( h*j for j in xrange(n-1, -1, -1)))

    return gamax/gamma_spounge(a)

c = None
def gamma_spounge( z):
    global c
    a = 12

    if c is None:
       k1_factrl = 1.0
       c = []
       c.append(math.sqrt(2.0*math.pi))
       for k in range(1,a):
          c.append( math.exp(a-k) * (a-k)**(k-0.5) / k1_factrl )
          k1_factrl *= -k

    accm = c[0]
    for k in range(1,a):
        accm += c[k] / (z+k)
    accm *= math.exp( -(z+a)) * (z+a)**(z+0.5)
    return accm/z;

def chi2UniformDistance( dataSet ):
    expected = sum(dataSet)*1.0/len(dataSet)
    cntrd = (d-expected for d in dataSet)
    return sum(x*x for x in cntrd)/expected

def chi2Probability(dof, distance):
    return 1.0 - GammaInc_Q( 0.5*dof, 0.5*distance)

def chi2IsUniform(dataSet, significance):
    dof = len(dataSet)-1
    dist = chi2UniformDistance(dataSet)
    return chi2Probability( dof, dist ) > significance

dset1 = [ 199809, 200665, 199607, 200270, 199649 ]
dset2 = [ 522573, 244456, 139979,  71531,  21461 ]

for ds in (dset1, dset2):
    print "Data set:", ds
    dof = len(ds)-1
    distance =chi2UniformDistance(ds)
    print "dof: %d distance: %.4f" % (dof, distance),
    prob = chi2Probability( dof, distance)
    print "probability: %.4f"%prob,
    print "uniform? ", "Yes"if chi2IsUniform(ds,0.05) else "No"
```

Output:

```txt
Data set: [199809, 200665, 199607, 200270, 199649]
dof: 4 distance: 4.146280 probability: 0.3866 uniform?  Yes
Data set: [522573, 244456, 139979, 71531, 21461]
dof: 4 distance: 790063.275940 probability: 0.0000 uniform?  No
```



## R

R being a statistical computating language, the chi-squared test is built in with the function "chisq.test"

```tcl

dset1=c(199809,200665,199607,200270,199649)
dset2=c(522573,244456,139979,71531,21461)

chi2IsUniform<-function(dataset,significance=0.05){
  chi2IsUniform=(chisq.test(dataset)$p.value>significance)
}

for (ds in list(dset1,dset2)){
  print(c("Data set:",ds))
  print(chisq.test(ds))
  print(paste("uniform?",chi2IsUniform(ds)))
}

```


Output:

```txt

[1] "Data set:" "199809"    "200665"    "199607"    "200270"    "199649"

        Chi-squared test for given probabilities

data:  ds
X-squared = 4.1463, df = 4, p-value = 0.3866

[1] "uniform? TRUE"
[1] "Data set:" "522573"    "244456"    "139979"    "71531"     "21461"

        Chi-squared test for given probabilities

data:  ds
X-squared = 790063.3, df = 4, p-value < 2.2e-16

[1] "uniform? FALSE"

```



## Racket


```racket

#lang racket
(require
 racket/flonum (planet williams/science:4:5/science)
 (only-in (planet williams/science/unsafe-ops-utils) real->float))

; (chi^2-goodness-of-fit-test observed expected df)
;  Given:   observed, a sequence of observed frequencies
;           expected, a sequence of expected frequencies
;           df,       the degrees of freedom
;  Result:  P-value  = 1-chi^2cdf(X^2,df) , the p-value
(define (chi^2-goodness-of-fit-test observed expected df)
  (define X^2 (for/sum ([o observed] [e expected])
                (/ (sqr (- o e)) e)))
  (- 1.0 (chi-squared-cdf X^2 df)))

(define (is-uniform? rand n α)
  ; Use significance level α to test whether
  ; n small random numbers generated by rand
  ; have a uniform distribution.

  ; Observed values:
  (define o (make-vector 10 0))
  ; generate n random integers from 0 to 9.
  (for ([_ (+ n 1)])
    (define r (rand 10))
    (vector-set! o r (+ (vector-ref o r) 1)))
  ; Expected values:
  (define ex (make-vector 10 (/ n 10)))

  ; Calculate the P-value:
  (define P (chi^2-goodness-of-fit-test o ex (- n 1)))

  ; If the P-value is larger than α we accept the
  ; hypothesis that the numbers are distributed uniformly.
  (> P α))

; Test whether the builtin generator is uniform:
(is-uniform? random 1000 0.05)
; Test whether the constant generator fails:
(is-uniform? (λ(_) 5) 1000 0.05)

```

Output:

```racket

#t
#f

```



## REXX

Programming notes:

The use of the   '''pow'''   was elided as it can just be replaced with   '''t**(a-1)'''.

The   '''gamma'''   was replaced with a simple version.   The argument
for   '''gamma'''   is (in the cases used herein)   always

positive,   and is
either an integer,   or a number which is a multiple of   <big>'''<sup>1</sup>/<sub>2</sub>'''</big>,   both of these cases can be calculated with

a straight─forward calculation.

```rexx
/*REXX program performs a chi─squared test to verify a given distribution is uniform.   */
numeric digits length( pi() )  - length(.)       /*enough decimal digs for calculations.*/
@.=;                                        @.1= 199809 200665 199607 200270 199649
                                            @.2= 522573 244456 139979  71531  21461
        do s=1  while @.s\=='';  call uTest @.s  /*invoke  uTest with a data set of #'s.*/
        end   /*s*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
!:      procedure; parse arg x;  p=1;    do j=2  to x;   p= p*j;   end  /*j*/;    return p
chi2p:  procedure;  parse arg dof, distance;       return gammaI( dof/2,  distance/2 )
f:      parse arg t;   if t=0  then return 0;      return t ** (a-1)    *    exp(-t)
e:      e =2.718281828459045235360287471352662497757247093699959574966967627724; return e
pi:     pi=3.141592653589793238462643383279502884197169399375105820974944592308; return pi
/*──────────────────────────────────────────────────────────────────────────────────────*/
!!:     procedure; parse arg x;                   if x<2  then return 1;    p= x
                     do k=2+x//2  to x-1  by 2;   p= p*k;   end  /*k*/;          return p
/*──────────────────────────────────────────────────────────────────────────────────────*/
chi2ud: procedure: parse arg ds; sum=0;                       expect= 0
                                       do j=1  for words(ds); expect= expect + word(ds, j)
                                       end   /*j*/
        expect = expect / words(ds)
                                       do k=1  for words(ds)
                                       sum= sum   +   (word(ds, k) - expect) **2
                                       end   /*k*/
        return sum / expect
/*──────────────────────────────────────────────────────────────────────────────────────*/
exp:    procedure; parse arg x; ix= x%1;  if abs(x-ix)>.5  then ix= ix + sign(x);  x= x-ix
        z=1; _=1; w=z;    do j=1;  _= _*x/j;  z= (z + _)/1;  if z==w  then leave;      w=z
                          end  /*j*/;         if z\==0  then z= z * e()**ix;      return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
gamma:  procedure; parse arg x; if datatype(x, 'W')  then return !(x-1) /*Int?  Use fact*/
        n= trunc(x)                     /*at this point, X is pos and a multiple of 1/2.*/
        d= !!(n+n - 1)                  /*compute the double factorial of:    2*n - 1.  */
        if n//2  then p= -1             /*if  N  is  odd,   then use a negative unity.  */
                 else p=  1             /*if  N  is even,   then use a positive unity.  */
        if x>0   then return p * d * sqrt(pi()) / (2**n)
                      return p * (2**n) * sqrt(pi()) / d
/*──────────────────────────────────────────────────────────────────────────────────────*/
gammaI: procedure; parse arg a,x;  y= a-1;   do  while f(y)*(x-y) > 2e-8 & y<x;  y= y + .4
                                             end  /*while*/
        y= min(x, y)
        return 1   -   simp38(0, y,   y / 0.015 / gamma(a-1) % 1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
simp38: procedure; parse arg a, b, n;                h= (b-a) / n;        h1= h / 3
        sum= f(a) + f(b)
                             do j=3*n-1   by -1   while j>0
                             if j//3 == 0  then sum= sum   +   2 * f(a + h1*j)
                                           else sum= sum   +   3 * f(a + h1*j)
                             end   /*j*/
        return h * sum / 8
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt:   procedure; parse arg x;  if x=0  then return 0; d=digits(); numeric digits; h= d+6
        numeric form; m.=9; parse value format(x,2,1,,0) 'E0' with g "E" _ .;g=g *.5'e'_%2
          do j=0  while h>9;      m.j=h;               h=h%2+1;       end  /*j*/
          do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;  end  /*k*/; return g
/*──────────────────────────────────────────────────────────────────────────────────────*/
uTest:  procedure; parse arg dset;  sum= 0;   pad= left('', 11);      sigLev= 1/20  /*5%*/
        say;   say '     '   center(" Uniform distribution test ", 75, '═')
        #= words(dset);                                              sigPC= sigLev*100/1
                             do j=1  for #;      sum= sum  +  word(dset, j)
                             end   /*j*/
                  say pad "                  dataset: "  dset
                  say pad "                  samples: "  sum
                  say pad "               categories: "  #
                  say pad "       degrees of freedom: "  # - 1
        dist= chi2ud(dset)
        P= chi2p(# - 1,  dist)
        sig = (abs(P) < dist * sigLev)
                  say pad "significant at " sigPC'%  level? '  word('no yes',    sig  + 1)
                  say pad "   is the dataset uniform? "        word('no yes', (\(sig))+ 1)
        return
```

```txt

      ════════════════════════ Uniform distribution test ════════════════════════
                              dataset:  199809 200665 199607 200270 199649
                              samples:  1000000
                           categories:  5
                   degrees of freedom:  4
            significant at  5%  level?  no
               is the dataset uniform?  yes

      ════════════════════════ Uniform distribution test ════════════════════════
                              dataset:  522573 244456 139979 71531 21461
                              samples:  1000000
                           categories:  5
                   degrees of freedom:  4
            significant at  5%  level?  yes
               is the dataset uniform?  no

```



## Ruby

```ruby
def gammaInc_Q(a, x)
  a1, a2 = a-1, a-2
  f0  = lambda {|t| t**a1 * Math.exp(-t)}
  df0 = lambda {|t| (a1-t) * t**a2 * Math.exp(-t)}

  y = a1
  y += 0.3  while f0[y]*(x-y) > 2.0e-8 and y < x
  y = x  if y > x

  h = 3.0e-4
  n = (y/h).to_i
  h = y/n
  hh = 0.5 * h
  sum = 0
  (n-1).step(0, -1) do |j|
    t = h * j
    sum += f0[t] + hh * df0[t]
  end
  h * sum / gamma_spounge(a)
end

A = 12
k1_factrl = 1.0
coef = [Math.sqrt(2.0*Math::PI)]
COEF = (1...A).each_with_object(coef) do |k,c|
  c << Math.exp(A-k) * (A-k)**(k-0.5) / k1_factrl
  k1_factrl *= -k
end

def gamma_spounge(z)
  accm = (1...A).inject(COEF[0]){|res,k| res += COEF[k] / (z+k)}
  accm * Math.exp(-(z+A)) * (z+A)**(z+0.5) / z
end

def chi2UniformDistance(dataSet)
  expected = dataSet.inject(:+).to_f / dataSet.size
  dataSet.map{|d|(d-expected)**2}.inject(:+) / expected
end

def chi2Probability(dof, distance)
  1.0 - gammaInc_Q(0.5*dof, 0.5*distance)
end

def chi2IsUniform(dataSet, significance=0.05)
  dof = dataSet.size - 1
  dist = chi2UniformDistance(dataSet)
  chi2Probability(dof, dist) > significance
end

dsets = [ [ 199809, 200665, 199607, 200270, 199649 ],
          [ 522573, 244456, 139979,  71531,  21461 ] ]

for ds in dsets
  puts "Data set:#{ds}"
  dof = ds.size - 1
  puts "  degrees of freedom: %d" % dof
  distance = chi2UniformDistance(ds)
  puts "  distance:           %.4f" % distance
  puts "  probability:        %.4f" % chi2Probability(dof, distance)
  puts "  uniform?            %s" % (chi2IsUniform(ds) ? "Yes" : "No")
end
```


```txt

Data set:[199809, 200665, 199607, 200270, 199649]
  degrees of freedom: 4
  distance:           4.1463
  probability:        0.3866
  uniform?            Yes
Data set:[522573, 244456, 139979, 71531, 21461]
  degrees of freedom: 4
  distance:           790063.2759
  probability:        -0.0000
  uniform?            No

```



## Scala

{{Out}}See it yourself by running in your browser [https://scastie.scala-lang.org/WUFeFG5WQkq2MZ51kBqaYA Scastie (remote JVM)].
```Scala
import org.apache.commons.math3.special.Gamma.regularizedGammaQ

object ChiSquare extends App {
  private val dataSets: Seq[Seq[Double]] =
    Seq(
      Seq(199809, 200665, 199607, 200270, 199649),
      Seq(522573, 244456, 139979, 71531, 21461)
    )

  private def χ2IsUniform(data: Seq[Double], significance: Double) =
    χ2Prob(data.size - 1.0, χ2Dist(data)) > significance

  private def χ2Dist(data: Seq[Double]) = {
    val avg = data.sum / data.size

    data.reduce((a, b) => a + math.pow(b - avg, 2)) / avg
  }

  private def χ2Prob(dof: Double, distance: Double) =
    regularizedGammaQ(dof / 2, distance / 2)

  printf(" %4s %10s  %12s %8s   %s%n",
    "d.f.", "χ²distance", "χ²probability", "Uniform?", "dataset")
  dataSets.foreach { ds =>
    val (dist, dof) = (χ2Dist(ds), ds.size - 1)

    printf("%4d %11.3f  %13.8f    %5s    %6s%n",
      dof, dist, χ2Prob(dof.toDouble, dist), if (χ2IsUniform(ds, 0.05)) "YES" else "NO", ds.mkString(", "))
  }
}
```


## Sidef


```ruby
# Confluent hypergeometric function of the first kind F_1(a;b;z)
func F1(a, b, z, limit=100) {
    sum(0..limit, {|k|
        rising_factorial(a, k) / rising_factorial(b, k) * z**k / k!
    })
}

func γ(a,x) {    # lower incomplete gamma function γ(a,x)
    #a**(-1) * x**a * F1(a, a+1, -x)            # simpler formula
    a**(-1) * x**a * exp(-x) * F1(1, a+1, x)    # slightly better convergence
}

func P(a,z) {    # regularized gamma function P(a,z)
    γ(a,z) / Γ(a)
}

func chi_squared_cdf (k, x) {
    var f = (k<20 ? 20 : 10)
    given(x) {
        when (0) { 0 }
        case (. < (k + f*sqrt(k))) { P(k/2, x/2) }
        else { 1 }
    }
}

func chi_squared_test(arr, significance = 0.05) {
    var n = arr.len
    var N = arr.sum
    var expected = N/n
    var χ_squared = arr.sum_by {|v| (v-expected)**2 / expected }
    var p_value = (1 - chi_squared_cdf(n-1, χ_squared))
    [χ_squared, p_value, p_value > significance]
}

[
    %n< 199809 200665 199607 200270 199649 >,
    %n< 522573 244456 139979  71531  21461 >,
].each {|dataset|
    var r = chi_squared_test(dataset)
    say "data: #{dataset}"
    say "χ² = #{r[0]}, p-value = #{r[1].round(-4)}, uniform = #{r[2]}\n"
}
```

```txt

data: [199809, 200665, 199607, 200270, 199649]
χ² = 4.14628, p-value = 0.3866, uniform = true

data: [522573, 244456, 139979, 71531, 21461]
χ² = 790063.27594, p-value = 0, uniform = false

```



## Tcl

```tcl
package require Tcl 8.5
package require math::statistics

proc isUniform {distribution {significance 0.05}} {
    set count [tcl::mathop::+ {*}[dict values $distribution]]
    set expected [expr {double($count) / [dict size $distribution]}]
    set X2 0.0
    foreach value [dict values $distribution] {
	set X2 [expr {$X2 + ($value - $expected)**2 / $expected}]
    }
    set degreesOfFreedom [expr {[dict size $distribution] - 1}]
    set likelihoodOfRandom [::math::statistics::incompleteGamma \
	[expr {$degreesOfFreedom / 2.0}] [expr {$X2 / 2.0}]]
    expr {$likelihoodOfRandom > $significance}
}
```

Testing:

```tcl
proc makeDistribution {operation {count 1000000}} {
    for {set i 0} {$i<$count} {incr i} {incr distribution([uplevel 1 $operation])}
    return [array get distribution]
}

set distFair [makeDistribution {expr int(rand()*5)}]
puts "distribution \"$distFair\" assessed as [expr [isUniform $distFair]?{fair}:{unfair}]"
set distUnfair [makeDistribution {expr int(rand()*rand()*5)}]
puts "distribution \"$distUnfair\" assessed as [expr [isUniform $distUnfair]?{fair}:{unfair}]"
```

Output:

```txt
distribution "0 199809 4 199649 1 200665 2 199607 3 200270" assessed as fair
distribution "4 21461 0 522573 1 244456 2 139979 3 71531" assessed as unfair
```



## VBA

The built in worksheetfunction ChiSq_Dist of Excel VBA is used. Output formatted like R.

```vb
Private Function Test4DiscreteUniformDistribution(ObservationFrequencies() As Variant, Significance As Single) As Boolean
    'Returns true if the observed frequencies pass the Pearson Chi-squared test at the required significance level.
    Dim Total As Long, Ei As Long, i As Integer
    Dim ChiSquared As Double, DegreesOfFreedom As Integer, p_value As Double
    Debug.Print "[1] ""Data set:"" ";
    For i = LBound(ObservationFrequencies) To UBound(ObservationFrequencies)
        Total = Total + ObservationFrequencies(i)
        Debug.Print ObservationFrequencies(i); " ";
    Next i
    DegreesOfFreedom = UBound(ObservationFrequencies) - LBound(ObservationFrequencies)
    'This is exactly the number of different categories minus 1
    Ei = Total / (DegreesOfFreedom + 1)
    For i = LBound(ObservationFrequencies) To UBound(ObservationFrequencies)
        ChiSquared = ChiSquared + (ObservationFrequencies(i) - Ei) ^ 2 / Ei
    Next i
    p_value = 1 - WorksheetFunction.ChiSq_Dist(ChiSquared, DegreesOfFreedom, True)
    Debug.Print
    Debug.Print "   Chi-squared test for given frequencies"
    Debug.Print "X-squared ="; ChiSquared; ", ";
    Debug.Print "df ="; DegreesOfFreedom; ", ";
    Debug.Print "p-value = "; Format(p_value, "0.0000")
    Test4DiscreteUniformDistribution = p_value > Significance
End Function
Public Sub test()
    Dim O() As Variant
    O = [{199809,200665,199607,200270,199649}]
    Debug.Print "[1] ""Uniform? "; Test4DiscreteUniformDistribution(O, 0.05); """"
    O = [{522573,244456,139979,71531,21461}]
    Debug.Print "[1] ""Uniform? "; Test4DiscreteUniformDistribution(O, 0.05); """"
End Sub
```

{{out}
```txt
[1] "Data set:"  199809   200665   199607   200270   199649
   Chi-squared test for given frequencies
X-squared = 4.14628 , df = 4 , p-value = 0.3866
[1] "Uniform? True"
[1] "Data set:"  522573   244456   139979   71531   21461
   Chi-squared test for given frequencies
X-squared = 790063.27594 , df = 4 , p-value = 0.0000
[1] "Uniform? False"
```


## zkl

```zkl
/* Numerical integration method */
fcn Simpson3_8(f,a,b,N){  // fcn,double,double,Int --> double
   h,h1:=(b - a)/N, h/3.0;
   h*[1..3*N - 1].reduce('wrap(sum,j){
      l1:=(if(j%3) 3.0 else 2.0);
      sum + l1*f(a + h1*j);
   },f(a) + f(b))/8.0;
}

const A=12;
fcn Gamma_Spouge(z){  // double --> double
   var coefs=fcn{  // this runs only once, at construction time
      a,coefs:=A.toFloat(),(A).pump(List(),0.0);
      k1_factrl:=1.0;
      coefs[0]=(2.0*(0.0).pi).sqrt();
      foreach k in ([1.0..A-1]){
         coefs[k]=(a - k).exp() * (a - k).pow(k - 0.5) / k1_factrl;
	 k1_factrl*=-k;
      }
      coefs
   }();

   ( [1..A-1].reduce('wrap(accum,k){ accum + coefs[k]/(z + k) },coefs[0])
     * (-(z + A)).exp()*(z + A).pow(z + 0.5) )
   / z;
}

fcn f0(t,aa1){ t.pow(aa1)*(-t).exp() }

fcn GammaIncomplete_Q(a,x){  // double,double --> double
   h:=1.5e-2;  /* approximate integration step size */
   /* this cuts off the tail of the integration to speed things up */
   y:=a - 1; f:=f0.fp1(y);
   while((f(y)*(x - y)>2.0e-8) and (y<x)){ y+=0.4; }
   if(y>x) y=x;
   1.0 - Simpson3_8(f,0.0,y,(y/h).toInt())/Gamma_Spouge(a);
}
```


```zkl
fcn chi2UniformDistance(ds){ // --> double
   dslen   :=ds.len();
   expected:=dslen.reduce('wrap(sum,k){ sum + ds[k] },0.0)/dslen;
   sum     := dslen.reduce('wrap(sum,k){ x:=ds[k] - expected; sum + x*x },0.0);
   sum/expected
}

fcn chi2Probability(dof,distance){ GammaIncomplete_Q(0.5*dof, 0.5*distance) }

fcn chiIsUniform(dset,significance=0.05){
   significance < chi2Probability(-1.0 + dset.len(),chi2UniformDistance(dset))
}
```


```zkl
datasets:=T( T(199809.0, 200665.0, 199607.0, 200270.0, 199649.0),
             T(522573.0, 244456.0, 139979.0,  71531.0,  21461.0) );
println(" %4s %12s  %12s %8s   %s".fmt(
        "dof", "distance", "probability", "Uniform?", "dataset"));
foreach ds in (datasets){
   dof :=ds.len() - 1;
   dist:=chi2UniformDistance(ds);
   prob:=chi2Probability(dof,dist);
   println("%4d %12.3f  %12.8f    %5s    %6s".fmt(
            dof, dist, prob, chiIsUniform(ds) and "YES" or "NO",
	    ds.concat(",")));
}
```

```txt

  dof     distance   probability Uniform?   dataset
   4        4.146    0.38657083      YES    199809,200665,199607,200270,199649
   4   790063.276    0.00000000       NO    522573,244456,139979,71531,21461

```


