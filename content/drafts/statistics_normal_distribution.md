+++
title = "Statistics/Normal distribution"
description = ""
date = 2019-03-24T21:29:06Z
aliases = []
[extra]
id = 10159
[taxonomies]
categories = []
tags = []
+++

{{task}}
The [[wp:Normal distribution|Normal]] (or Gaussian) distribution is a frequently used distribution in statistics. While most programming languages provide a uniformly distributed random number generator, one can [[wp:Normal distribution#Generating_values_from_normal_distribution|derive]] normally distributed random numbers from a uniform generator.


;The task:
# Take a uniform random number generator and create a large (you decide how large) set of numbers that follow a normal (Gaussian) distribution.  Calculate the dataset's mean and stddev, and show the histogram here.
# Mention any native language support for the generation of normally distributed random numbers.


;Reference:
* You may refer to code in [[Statistics/Basic]] if available.





## C


```C
/*
 * RosettaCode example: Statistics/Normal distribution in C
 *
 * The random number generator rand() of the standard C library is obsolete
 * and should not be used in more demanding applications. There are plenty
 * libraries with advanced features (eg. GSL) with functions to calculate
 * the mean, the standard deviation, generating random numbers etc.
 * However, these features are not the core of the standard C library.
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>


#define NMAX 10000000


double mean(double* values, int n)
{
    int i;
    double s = 0;

    for ( i = 0; i < n; i++ )
        s += values[i];
    return s / n;
}


double stddev(double* values, int n)
{
    int i;
    double average = mean(values,n);
    double s = 0;

    for ( i = 0; i < n; i++ )
        s += (values[i] - average) * (values[i] - average);
    return sqrt(s / (n - 1));
}

/*
 * Normal random numbers generator - Marsaglia algorithm.
 */
double* generate(int n)
{
    int i;
    int m = n + n % 2;
    double* values = (double*)calloc(m,sizeof(double));
    double average, deviation;

    if ( values )
    {
        for ( i = 0; i < m; i += 2 )
        {
            double x,y,rsq,f;
            do {
                x = 2.0 * rand() / (double)RAND_MAX - 1.0;
                y = 2.0 * rand() / (double)RAND_MAX - 1.0;
                rsq = x * x + y * y;
            }while( rsq >= 1. || rsq == 0. );
            f = sqrt( -2.0 * log(rsq) / rsq );
            values[i]   = x * f;
            values[i+1] = y * f;
        }
    }
    return values;
}


void printHistogram(double* values, int n)
{
    const int width = 50;
    int max = 0;

    const double low   = -3.05;
    const double high  =  3.05;
    const double delta =  0.1;

    int i,j,k;
    int nbins = (int)((high - low) / delta);
    int* bins = (int*)calloc(nbins,sizeof(int));
    if ( bins != NULL )
    {
        for ( i = 0; i < n; i++ )
        {
            int j = (int)( (values[i] - low) / delta );
            if ( 0 <= j  &&  j < nbins )
                bins[j]++;
        }

        for ( j = 0; j < nbins; j++ )
            if ( max < bins[j] )
                max = bins[j];

        for ( j = 0; j < nbins; j++ )
        {
            printf("(%5.2f, %5.2f) |", low + j * delta, low + (j + 1) * delta );
            k = (int)( (double)width * (double)bins[j] / (double)max );
            while(k-- > 0) putchar('*');
            printf("  %-.1f%%", bins[j] * 100.0 / (double)n);
            putchar('\n');
        }

        free(bins);
    }
}


int main(void)
{
    double* seq;

    srand((unsigned int)time(NULL));

    if ( (seq = generate(NMAX)) != NULL )
    {
        printf("mean = %g, stddev = %g\n\n", mean(seq,NMAX), stddev(seq,NMAX));
        printHistogram(seq,NMAX);
        free(seq);

        printf("\n%s\n", "press enter");
        getchar();
        return EXIT_SUCCESS;
    }
    return EXIT_FAILURE;
}
```

{{out}}

```txt
mean = 0.000477941, stddev = 0.999945

(-3.05, -2.95) |  0.1%
(-2.95, -2.85) |  0.1%
(-2.85, -2.75) |*  0.1%
(-2.75, -2.65) |*  0.1%
(-2.65, -2.55) |*  0.1%
(-2.55, -2.45) |**  0.2%
(-2.45, -2.35) |**  0.2%
(-2.35, -2.25) |***  0.3%
(-2.25, -2.15) |****  0.4%
(-2.15, -2.05) |*****  0.4%
(-2.05, -1.95) |******  0.5%
(-1.95, -1.85) |********  0.7%
(-1.85, -1.75) |*********  0.8%
(-1.75, -1.65) |***********  0.9%
(-1.65, -1.55) |*************  1.1%
(-1.55, -1.45) |****************  1.3%
(-1.45, -1.35) |******************  1.5%
(-1.35, -1.25) |*********************  1.7%
(-1.25, -1.15) |************************  1.9%
(-1.15, -1.05) |***************************  2.2%
(-1.05, -0.95) |******************************  2.4%
(-0.95, -0.85) |*********************************  2.7%
(-0.85, -0.75) |************************************  2.9%
(-0.75, -0.65) |***************************************  3.1%
(-0.65, -0.55) |*****************************************  3.3%
(-0.55, -0.45) |********************************************  3.5%
(-0.45, -0.35) |**********************************************  3.7%
(-0.35, -0.25) |***********************************************  3.8%
(-0.25, -0.15) |*************************************************  3.9%
(-0.15, -0.05) |*************************************************  4.0%
(-0.05,  0.05) |**************************************************  4.0%
( 0.05,  0.15) |*************************************************  4.0%
( 0.15,  0.25) |*************************************************  3.9%
( 0.25,  0.35) |***********************************************  3.8%
( 0.35,  0.45) |**********************************************  3.7%
( 0.45,  0.55) |********************************************  3.5%
( 0.55,  0.65) |*****************************************  3.3%
( 0.65,  0.75) |***************************************  3.1%
( 0.75,  0.85) |************************************  2.9%
( 0.85,  0.95) |*********************************  2.7%
( 0.95,  1.05) |******************************  2.4%
( 1.05,  1.15) |***************************  2.2%
( 1.15,  1.25) |************************  1.9%
( 1.25,  1.35) |*********************  1.7%
( 1.35,  1.45) |******************  1.5%
( 1.45,  1.55) |****************  1.3%
( 1.55,  1.65) |*************  1.1%
( 1.65,  1.75) |***********  0.9%
( 1.75,  1.85) |*********  0.8%
( 1.85,  1.95) |********  0.7%
( 1.95,  2.05) |******  0.5%
( 2.05,  2.15) |*****  0.4%
( 2.15,  2.25) |****  0.4%
( 2.25,  2.35) |***  0.3%
( 2.35,  2.45) |**  0.2%
( 2.45,  2.55) |**  0.2%
( 2.55,  2.65) |*  0.1%
( 2.65,  2.75) |*  0.1%
( 2.75,  2.85) |*  0.1%
( 2.85,  2.95) |  0.1%

press enter
```


=={{header|C sharp|C#}}==
{{libheader|Math.Net}}

```csharp
using System;
using MathNet.Numerics.Distributions;
using MathNet.Numerics.Statistics;

class Program
{
    static void RunNormal(int sampleSize)
    {
        double[] X = new double[sampleSize];
        var norm = new Normal(new Random());
        norm.Samples(X);

        const int numBuckets = 10;
        var histogram = new Histogram(X, numBuckets);
        Console.WriteLine("Sample size: {0:N0}", sampleSize);
        for (int i = 0; i < numBuckets; i++)
        {
            string bar = new String('#', (int)(histogram[i].Count * 360 / sampleSize));
            Console.WriteLine(" {0:0.00} : {1}", histogram[i].LowerBound, bar);
        }
        var statistics = new DescriptiveStatistics(X);
        Console.WriteLine("  Mean: " + statistics.Mean);
        Console.WriteLine("StdDev: " + statistics.StandardDeviation);
        Console.WriteLine();
    }
    static void Main(string[] args)
    {
        RunNormal(100);
        RunNormal(1000);
        RunNormal(10000);
    }
}
```

{{out}}

```txt
Sample size: 100
 -2.12 : #######
 -1.66 : ############################
 -1.19 : #######################################
 -0.72 : ##############################################
 -0.26 : ###############################################################################
 0.21 : ######################################################################################
 0.68 : ################################
 1.14 : #########################
 1.61 : ###
 2.07 : ##########
  Mean: 0.0394411345297757
StdDev: 0.925286665513647

Sample size: 1,000
 -2.98 : ##
 -2.34 : ##########
 -1.69 : ##############################
 -1.05 : ################################################################
 -0.40 : ###########################################################################################
 0.24 : ########################################################################################
 0.88 : ##############################################
 1.53 : ##################
 2.17 : #####
 2.82 : ##
  Mean: 0.0868718238400114
StdDev: 0.989120264661867

Sample size: 10,000
 -3.88 :
 -3.12 : ##
 -2.35 : #################
 -1.59 : ####################################################
 -0.82 : ################################################################################################
 -0.06 : ####################################################################################################
 0.71 : ###############################################################
 1.47 : #####################
 2.23 : ####
 3.00 :
  Mean: 0.0208920122989818
StdDev: 1.00046328880424
```



## C++

showing features of C++11 here

```cpp
#include <random>
#include <map>
#include <string>
#include <iostream>
#include <cmath>
#include <iomanip>

int main( ) {
   std::random_device myseed ;
   std::mt19937 engine ( myseed( ) ) ;
   std::normal_distribution<> normDistri ( 2 , 3 ) ;
   std::map<int , int> normalFreq ;
   int sum = 0 ; //holds the sum of the randomly created numbers
   double mean = 0.0 ;
   double stddev = 0.0 ;
   for ( int i = 1 ; i < 10001 ; i++ )
      ++normalFreq[ normDistri ( engine ) ] ;
   for ( auto MapIt : normalFreq ) {
      sum += MapIt.first * MapIt.second ;
   }
   mean = sum / 10000 ;
   stddev = sqrt( sum / 10000 ) ;
   std::cout << "The mean of the distribution is " << mean << " , the " ;
   std::cout << "standard deviation " << stddev << " !\n" ;
   std::cout << "And now the histogram:\n" ;
   for ( auto MapIt : normalFreq ) {
      std::cout << std::left << std::setw( 4 ) << MapIt.first <<
	 std::string( MapIt.second / 100 , '*' ) << std::endl ;
   }
   return 0 ;
}
```

Output:

```txt
The mean of the distribution is 1 , the standard deviation 1 !
And now the histogram:
-10
-9
-8
-7
-6
-5
-4  *
-3  **
-2  ****
-1  ******
0   *********************
1   ************
2   ************
3   ***********
4   *********
5   ******
6   ****
7   **
8   *
9
10
11
12
13

```



## D

This uses the Box-Muller method as in the Go entry, and the module from the Statistics/Basic. A ziggurat-based normal generator for the Phobos standard library is in the works.

```d
import std.stdio, std.random, std.math, std.range, std.algorithm,
       statistics_basic;

struct Normals {
    double mu, sigma;
    double[2] state;
    size_t index = state.length;
    enum empty = false;

    void popFront() pure nothrow { index++; }

    @property double front() {
        if (index >= state.length) {
            immutable r = sqrt(-2 * uniform!"]["(0., 1.0).log) * sigma;
            immutable x = 2 * PI * uniform01;
            state = [mu + r * x.sin, mu + r * x.cos];
            index = 0;
        }
        return state[index];
    }
}

void main() {
    const data = Normals(0.0, 0.5).take(100_000).array;
    writefln("Mean: %8.6f, SD: %8.6f\n", data.meanStdDev[]);
    data.map!q{ max(0.0, min(0.9999, a / 3 + 0.5)) }.showHistogram01;
}
```

{{out}}

```txt
Mean: 0.000528, SD: 0.502245

 0.0: *
 0.1: ******
 0.2: *****************
 0.3: ***********************************
 0.4: *************************************************
 0.5: **************************************************
 0.6: **********************************
 0.7: *****************
 0.8: ******
 0.9: *
```



## Elixir


```elixir
defmodule Statistics do
  def normal_distribution(n, w\\5) do
    {sum, sum2, hist} = generate(n, w)
    mean = sum / n
    stddev = :math.sqrt(sum2 / n - mean*mean)

    IO.puts "size:   #{n}"
    IO.puts "mean:   #{mean}"
    IO.puts "stddev: #{stddev}"
    {min, max} = Map.to_list(hist)
                 |> Enum.filter_map(fn {_k,v} -> v >= n/120/w end, fn {k,_v} -> k end)
                 |> Enum.min_max
    Enum.each(min..max, fn i ->
      bar = String.duplicate("=", trunc(120 * w * Map.get(hist, i, 0) / n))
      :io.fwrite "~4.1f: ~s~n", [i/w, bar]
    end)
    IO.puts ""
  end

  defp generate(n, w) do
    Enum.reduce(1..n, {0, 0, %{}}, fn _,{sum, sum2, hist} ->
      z = :rand.normal
      {sum+z, sum2+z*z, Map.update(hist, round(w*z), 1, &(&1+1))}
    end)
  end
end

Enum.each([100,1000,10000], fn n ->
  Statistics.normal_distribution(n)
end)
```


{{out}}
<pre style="height: 120ex; overflow: scroll">
size:   100
mean:   0.027742416007234007
stddev: 1.0209597927405403
-2.6:
### ======

-2.4:
-2.2:
### ======

-2.0: ======
-1.8:
-1.6:
-1.4:
### ========================

-1.2: ======
-1.0:
### ========================

-0.8:
### ====================================

-0.6:
### ====================================

-0.4:
### ==========================================

-0.2:
### ==========================================

 0.0:
### ========================

 0.2:
### ==============================

 0.4:
### ====================================

 0.6:
### ================================================

 0.8:
### ====================================

 1.0:
### ==========================================

 1.2:
### ========================

 1.4: ======
 1.6:
### ======

 1.8:
### ======

 2.0:
 2.2:
 2.4: ======
 2.6: ======

size:   1000
mean:   -0.025562168667763084
stddev: 1.0338288521306742
-3.2: =
-3.0:
-2.8: =
-2.6: ===
-2.4: ==
-2.2: ======
-2.0: ==
-1.8:
### =======

-1.6:
### =========

-1.4:
### ===========

-1.2:
### ===========

-1.0:
### ==============================

-0.8:
### =============================

-0.6:
### ======================================

-0.4:
### ======================================

-0.2:
### =========================================

 0.0:
### ===================================

 0.2:
### =====================================

 0.4:
### =======================================

 0.6:
### =================================

 0.8:
### ==========================

 1.0:
### ======================

 1.2:
### ==================

 1.4:
### ============

 1.6:
### ====

 1.8: =====
 2.0:
### ==

 2.2: ====
 2.4: =====
 2.6: =
 2.8: =

size:   10000
mean:   -0.009132420943007152
stddev: 0.9979508347451509
-2.6: =
-2.4: ===
-2.2: ====
-2.0: =====
-1.8:
### ===

-1.6:
### ========

-1.4:
### ==========

-1.2:
### =================

-1.0:
### ======================

-0.8:
### ===========================

-0.6:
### ======================================

-0.4:
### =====================================

-0.2:
### ========================================

 0.0:
### ============================================

 0.2:
### ======================================

 0.4:
### =====================================

 0.6:
### =================================

 0.8:
### ===============================

 1.0:
### ======================

 1.2:
### =================

 1.4:
### ==========

 1.6:
### ========

 1.8:
### ===

 2.0: ======
 2.2: ===
 2.4: ==
 2.6: =

```



## Fortran

{{works with|Fortran|95 and later}}
Using the Marsaglia polar method

```fortran
program Normal_Distribution
  implicit none

  integer, parameter :: i64 = selected_int_kind(18)
  integer, parameter :: r64 = selected_real_kind(15)
  integer(i64), parameter :: samples = 1000000_i64
  real(r64) :: mean, stddev
  real(r64) :: sumn = 0, sumnsq = 0
  integer(i64) :: n = 0
  integer(i64) :: bin(-50:50) = 0
  integer :: i, ind
  real(r64) :: ur1, ur2, nr1, nr2, s

  n = 0
  do while(n <= samples)
    call random_number(ur1)
    call random_number(ur2)
    ur1 = ur1 * 2.0 - 1.0
    ur2 = ur2 * 2.0 - 1.0

    s = ur1*ur1 + ur2*ur2
    if(s >= 1.0_r64) cycle

    nr1 = ur1 * sqrt(-2.0*log(s)/s)
    ind = floor(5.0*nr1)
    bin(ind) = bin(ind) + 1_i64
    sumn = sumn + nr1
    sumnsq = sumnsq + nr1*nr1

    nr2 = ur2 * sqrt(-2.0*log(s)/s)
    ind = floor(5.0*nr2)
    bin(ind) = bin(ind) + 1_i64
    sumn = sumn + nr2
    sumnsq = sumnsq + nr2*nr2
    n = n + 2_i64
  end do

  mean = sumn / n
  stddev = sqrt(sumnsq/n - mean*mean)

  write(*, "(a, i0)") "sample size = ", samples
  write(*, "(a, f17.15)") "Mean :   ", mean,
  write(*, "(a, f17.15)") "Stddev : ", stddev

  do i = -15, 15
    write(*, "(f4.1, a, a)") real(i)/5.0, ": ", repeat("=", int(bin(i)*500/samples))
  end do

end program
```

{{out}}

```txt

sample size = 1000
Mean :   0.043096320705032
Stddev : 0.981532585231540
-3.0:
-2.8:
-2.6: ==
-2.4: ==
-2.2: ====
-2.0: ======
-1.8:
### =

-1.6:
### ======

-1.4:
### ==========

-1.2:
### ===============

-1.0:
### =====================

-0.8:
### =================

-0.6:
### ============================

-0.4:
### ===============================

-0.2:
### ====================================

 0.0:
### =========================================

 0.2:
### ==============================

 0.4:
### ===========================

 0.6:
### ============================

 0.8:
### =======================

 1.0:
### ==============

 1.2:
### ====================

 1.4:
### =====

 1.6:
### ===

 1.8: ====
 2.0: ======
 2.2: ===
 2.4:
 2.6:
 2.8: =
 3.0:

sample size = 1000000
Mean :   0.000166653231289
Stddev : 1.000025612171690
-3.0:
-2.8: =
-2.6: =
-2.4: ==
-2.2: ====
-2.0: ======
-1.8:
### ===

-1.6:
### ======

-1.4:
### ===========

-1.2:
### ===============

-1.0:
### ====================

-0.8:
### =========================

-0.6:
### =============================

-0.4:
### ================================

-0.2:
### =================================

 0.0:
### =================================

 0.2:
### ================================

 0.4:
### ============================

 0.6:
### =========================

 0.8:
### ====================

 1.0:
### ===============

 1.2:
### ===========

 1.4:
### ======

 1.6:
### ===

 1.8: ======
 2.0: ====
 2.2: ==
 2.4: =
 2.6: =
 2.8:
 3.0:

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

Sub normalStats(sampleSize As Integer)
  If sampleSize < 1 Then Return
  Dim r(1 To sampleSize) As Double
  Dim h(-1 To 10) As Integer '' all zero by default
  Dim sum As Double = 0.0
  Dim hSum As Integer = 0

  ' Generate 'sampleSize' normally distributed random numbers with mean 0.5 and standard deviation 0.25
  ' calculate their sum
  ' and in which box they will fall when drawing the histogram
  For i As Integer = 1 To sampleSize
    r(i) = 0.5 + randomNormal / 4.0
    sum += r(i)
    If r(i) < 0.0 Then
      h(-1) += 1
    ElseIf r(i) >= 1.0 Then
      h(10) += 1
    Else
      h(Int(r(i) * 10)) += 1
    End If
  Next

  For i As Integer = -1 To 10 : hSum += h(i) :  Next
  ' adjust one of the h() values if necessary to ensure hSum = sampleSize
  Dim adj As Integer = sampleSize - hSum
  If adj <> 0 Then
    For i As Integer = -1 To 10
      h(i) += adj
      If h(i) >= 0 Then Exit For
      h(i) -= adj
    Next
  End If

  Dim mean As Double = sum / sampleSize

  Dim sd As Double
  sum = 0.0
  ' Now calculate their standard deviation
  For i As Integer = 1 To sampleSize
    sum += (r(i) - mean) ^ 2.0
  Next
  sd  = Sqr(sum/sampleSize)

  ' Draw a histogram of the data with interval 0.1
  Dim numStars As Integer
  ' If sample size > 300 then normalize histogram to 300
  Dim scale As Double = 1.0
  If sampleSize > 300 Then scale = 300.0 / sampleSize
  Print "Sample size "; sampleSize
  Print
  Print Using "  Mean #.######"; mean;
  Print Using "  SD #.######"; sd
  Print
  For i As Integer = -1 To 10
    If i = -1 Then
      Print Using "< 0.00 : ";
    ElseIf i = 10 Then
      Print Using ">=1.00 : ";
    Else
      Print Using "  #.## : "; i/10.0;
    End If
    Print Using "##### " ; h(i);
    numStars = Int(h(i) * scale + 0.5)
    Print String(numStars, "*")
  Next
End Sub

normalStats 100
Print
normalStats 1000
Print
normalStats 10000
Print
normalStats 100000
Print
Print "Press any key to quit"
Sleep
```

Sample output:
{{out}}

```txt

Sample size  100

  Mean 0.486977  SD 0.244147

< 0.00 :     2 **
  0.00 :     5 *****
  0.10 :     4 ****
  0.20 :    14 **************
  0.30 :    12 ************
  0.40 :    15 ***************
  0.50 :    17 *****************
  0.60 :    11 ***********
  0.70 :     9 *********
  0.80 :     7 *******
  0.90 :     1 *
>=1.00 :     3 ***

Sample size  1000

  Mean 0.489234  SD 0.247606

< 0.00 :    18 *****
  0.00 :    32 **********
  0.10 :    73 **********************
  0.20 :   111 *********************************
  0.30 :   138 *****************************************
  0.40 :   151 *********************************************
  0.50 :   153 **********************************************
  0.60 :   114 **********************************
  0.70 :   101 ******************************
  0.80 :    51 ***************
  0.90 :    38 ***********
>=1.00 :    20 ******

Sample size  10000

  Mean 0.498239  SD 0.249235

< 0.00 :   225 *******
  0.00 :   333 **********
  0.10 :   589 ******************
  0.20 :   999 ******************************
  0.30 :  1320 ****************************************
  0.40 :  1542 **********************************************
  0.50 :  1581 ***********************************************
  0.60 :  1323 ****************************************
  0.70 :   963 *****************************
  0.80 :   591 ******************
  0.90 :   314 *********
>=1.00 :   220 *******

Sample size  100000

  Mean 0.500925  SD 0.248910

< 0.00 :  2173 *******
  0.00 :  3192 **********
  0.10 :  5938 ******************
  0.20 :  9715 *****************************
  0.30 : 13351 ****************************************
  0.40 : 15399 **********************************************
  0.50 : 15680 ***********************************************
  0.60 : 13422 ****************************************
  0.70 :  9633 *****************************
  0.80 :  5993 ******************
  0.90 :  3207 **********
>=1.00 :  2297 *******

```



## Go

Box-Muller method shown here.  Go has a normally distributed random function in the standard library, as shown in the Go [[Random numbers]] solution.  It uses the ziggurat method.

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "strings"
)

// Box-Muller
func norm2() (s, c float64) {
    r := math.Sqrt(-2 * math.Log(rand.Float64()))
    s, c = math.Sincos(2 * math.Pi * rand.Float64())
    return s * r, c * r
}

func main() {
    const (
        n     = 10000
        bins  = 12
        sig   = 3
        scale = 100
    )
    var sum, sumSq float64
    h := make([]int, bins)
    for i, accum := 0, func(v float64) {
        sum += v
        sumSq += v * v
        b := int((v + sig) * bins / sig / 2)
        if b >= 0 && b < bins {
            h[b]++
        }
    }; i < n/2; i++ {
        v1, v2 := norm2()
        accum(v1)
        accum(v2)
    }
    m := sum / n
    fmt.Println("mean:", m)
    fmt.Println("stddev:", math.Sqrt(sumSq/float64(n)-m*m))
    for _, p := range h {
        fmt.Println(strings.Repeat("*", p/scale))
    }
}
```

Output:

```txt

mean: -0.0034970888831523488
stddev: 1.0040682925006286

*
****
*********
***************
*******************
******************
**************
*********
****
*

```


## Haskell


```haskell
import Data.Map (Map, empty, insert, findWithDefault, toList)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Data.Function (on)
import Data.List (sort, maximumBy, minimumBy)
import Control.Monad.Random (RandomGen, Rand, evalRandIO, getRandomR)
import Control.Monad (replicateM)

-- Box-Muller
getNorm :: RandomGen g => Rand g Double
getNorm = do
    u0 <- getRandomR (0.0, 1.0)
    u1 <- getRandomR (0.0, 1.0)
    let r = sqrt $ (-2.0) * log u0
        theta = 2.0 * pi * u1
    return $ r * sin theta

putInBin :: Double -> Map Int Int -> Double -> Map Int Int
putInBin binWidth t v =
    let bin = round (v / binWidth)
        count = findWithDefault 0 bin t
    in insert bin (count+1) t

runTest :: Int -> IO ()
runTest n = do
    rs <- evalRandIO $ replicateM n getNorm
    let binWidth = 0.1

        tally v (sv, sv2, t) = (sv+v, sv2 + v*v, putInBin binWidth t v)

        (sum, sum2, tallies) = foldr tally (0.0, 0.0, empty) rs

        tallyList = sort $ toList tallies

        printStars tallies binWidth maxCount selection =
            let count = findWithDefault 0 selection tallies
                bin = binWidth * fromIntegral selection
                maxStars = 100
                starCount = if maxCount <= maxStars
                            then count
                            else maxStars * count `div` maxCount
                stars = replicate  starCount '*'
            in printf "%5.2f: %s  %d\n" bin stars count

        mean = sum / fromIntegral n
        stddev = sqrt (sum2/fromIntegral n - mean*mean)

    printf "\n"
    printf "sample count: %d\n" n
    printf "mean:         %9.7f\n" mean
    printf "stddev:       %9.7f\n" stddev

    let maxCount = snd $ maximumBy (compare `on` snd) tallyList
        maxBin = fst $ maximumBy (compare `on` fst) tallyList
        minBin = fst $ minimumBy (compare `on` fst) tallyList

    mapM_ (printStars tallies binWidth maxCount) [minBin..maxBin]

main = do
    runTest 1000
    runTest 2000000
```


{{out}}
<pre style="font-size:80%">sample count: 1000
mean:         -0.0269949
stddev:       0.9795285
-3.10: **  2
-3.00:   0
-2.90:   0
-2.80: **  2
-2.70: *  1
-2.60: ****  4
-2.50: **  2
-2.40: **  2
-2.30:   0
-2.20: ***  3
-2.10: *****  5
-2.00: ******  6
-1.90: ******  6
-1.80: ***********  11
-1.70: ************  12
-1.60: *******  7
-1.50: *************  13
-1.40: *****************  17
-1.30: ********************  20
-1.20: ****************  16
-1.10: *****************  17
-1.00: **********************  22
-0.90: ***************************  27
-0.80: **********************  22
-0.70: ********************************  32
-0.60: *********************************  33
-0.50: ******************************************  42
-0.40: ***********************************************  47
-0.30: ************************************************  48
-0.20: ***************************  27
-0.10: *****************************  29
 0.00: ***********************************************  47
 0.10: ***************************************************  51
 0.20: ******************************************  42
 0.30: ********************************  32
 0.40: *********************************  33
 0.50: *****************************************  41
 0.60: ******************************************  42
 0.70: ****************************  28
 0.80: **********************  22
 0.90: ***************************  27
 1.00: *******************  19
 1.10: **********************  22
 1.20: ************************  24
 1.30: ********************  20
 1.40: *****************  17
 1.50: **********  10
 1.60: *************  13
 1.70: ****  4
 1.80: ***  3
 1.90: *******  7
 2.00: ******  6
 2.10: *  1
 2.20: *  1
 2.30: *******  7
 2.40: ***  3
 2.50:   0
 2.60: *  1
 2.70:   0
 2.80:   0
 2.90:   0
 3.00: *  1
 3.10:   0
 3.20:   0
 3.30: *  1

sample count: 2000000
mean:         0.0001017
stddev:       0.9994329
-4.60:   3
-4.50:   2
-4.40:   3
-4.30:   9
-4.20:   18
-4.10:   19
-4.00:   20
-3.90:   41
-3.80:   77
-3.70:   84
-3.60:   105
-3.50:   189
-3.40:   245
-3.30:   350
-3.20:   460
-3.10:   619
-3.00: *  838
-2.90: *  1234
-2.80: *  1586
-2.70: **  2063
-2.60: ***  2716
-2.50: ****  3503
-2.40: *****  4345
-2.30: *******  5678
-2.20: ********  7160
-2.10: ***********  8856
-2.00: *************  10915
-1.90: ****************  13299
-1.80: *******************  15599
-1.70: ***********************  19004
-1.60: ***************************  22321
-1.50: ********************************  25940
-1.40: *************************************  29622
-1.30: ******************************************  34213
-1.20: ************************************************  38922
-1.10: ******************************************************  43415
-1.00: ************************************************************  48250
-0.90: ******************************************************************  53210
-0.80: ************************************************************************  58127
-0.70: ******************************************************************************  62438
-0.60: ***********************************************************************************  66650
-0.50: ****************************************************************************************  70298
-0.40: ********************************************************************************************  73739
-0.30: ***********************************************************************************************  75831
-0.20: **************************************************************************************************  78222
-0.10: ***************************************************************************************************  79412
 0.00: ****************************************************************************************************  79801
 0.10: ***************************************************************************************************  79255
 0.20: *************************************************************************************************  78163
 0.30: ************************************************************************************************  76667
 0.40: ********************************************************************************************  73554
 0.50: ****************************************************************************************  70391
 0.60: ***********************************************************************************  66566
 0.70: ******************************************************************************  62857
 0.80: ************************************************************************  57962
 0.90: ******************************************************************  53407
 1.00: ************************************************************  48565
 1.10: ******************************************************  43496
 1.20: ************************************************  38799
 1.30: ******************************************  34156
 1.40: *************************************  29713
 1.50: ********************************  25946
 1.60: ***************************  22264
 1.70: ***********************  18843
 1.80: *******************  15780
 1.90: ****************  13151
 2.00: *************  10905
 2.10: **********  8690
 2.20: ********  7102
 2.30: *******  5693
 2.40: *****  4459
 2.50: ****  3550
 2.60: ***  2603
 2.70: **  2155
 2.80: **  1619
 2.90: *  1121
 3.00: *  914
 3.10:   607
 3.20:   478
 3.30:   349
 3.40:   216
 3.50:   170
 3.60:   113
 3.70:   79
 3.80:   58
 3.90:   48
 4.00:   33
 4.10:   20
 4.20:   9
 4.30:   8
 4.40:   7
 4.50:   3
 4.60:   3
 4.70:   0
 4.80:   1
 4.90:   1

```


## J

'''Solution'''

```j
runif01=: ?@$ 0:                                           NB. random uniform number generator
rnorm01=. (2 o. 2p1 * runif01) * [: %: _2 * ^.@runif01     NB. random normal number generator (Box-Muller)

mean=: +/ % #                        NB. mean
stddev=: (<:@# %~ +/)&.:*:@(- mean)  NB. standard deviation
histogram=: <:@(#/.~)@(i.@#@[ , I.)
```

'''Example Usage'''

```j
   DataSet=: rnorm01 1e5
   (mean , stddev) DataSet
0.000781667 1.00154
   require 'plot'
   plot (5 %~ i: 25) ([;histogram) DataSet
```



## Java

{{trans|D}}
{{works with|Java|8}}

```java
import static java.lang.Math.*;
import static java.util.Arrays.stream;
import java.util.Locale;
import java.util.function.DoubleSupplier;
import static java.util.stream.Collectors.joining;
import java.util.stream.DoubleStream;
import static java.util.stream.IntStream.range;

public class Test implements DoubleSupplier {

    private double mu, sigma;
    private double[] state = new double[2];
    private int index = state.length;

    Test(double m, double s) {
        mu = m;
        sigma = s;
    }

    static double[] meanStdDev(double[] numbers) {
        if (numbers.length == 0)
            return new double[]{0.0, 0.0};

        double sx = 0.0, sxx = 0.0;
        long n = 0;
        for (double x : numbers) {
            sx += x;
            sxx += pow(x, 2);
            n++;
        }

        return new double[]{sx / n, pow((n * sxx - pow(sx, 2)), 0.5) / n};
    }

    static String replicate(int n, String s) {
        return range(0, n + 1).mapToObj(i -> s).collect(joining());
    }

    static void showHistogram01(double[] numbers) {
        final int maxWidth = 50;
        long[] bins = new long[10];

        for (double x : numbers)
            bins[(int) (x * bins.length)]++;

        double maxFreq = stream(bins).max().getAsLong();

        for (int i = 0; i < bins.length; i++)
            System.out.printf(" %3.1f: %s%n", i / (double) bins.length,
                    replicate((int) (bins[i] / maxFreq * maxWidth), "*"));
        System.out.println();
    }

    @Override
    public double getAsDouble() {
        index++;
        if (index >= state.length) {
            double r = sqrt(-2 * log(random())) * sigma;
            double x = 2 * PI * random();
            state = new double[]{mu + r * sin(x), mu + r * cos(x)};
            index = 0;
        }
        return state[index];

    }

    public static void main(String[] args) {
        Locale.setDefault(Locale.US);
        double[] data = DoubleStream.generate(new Test(0.0, 0.5)).limit(100_000)
                .toArray();

        double[] res = meanStdDev(data);
        System.out.printf("Mean: %8.6f, SD: %8.6f%n", res[0], res[1]);

        showHistogram01(stream(data).map(a -> max(0.0, min(0.9999, a / 3 + 0.5)))
                .toArray());
    }
}
```


```txt
Mean: -0.001870, SD: 0.500539
 0.0: **
 0.1: *******
 0.2: ******************
 0.3: ************************************
 0.4: ***************************************************
 0.5: **************************************************
 0.6: ***********************************
 0.7: ******************
 0.8: *******
 0.9: **
```



## Julia

Julia has the builtin package "Distributions" to generate random numbers from a standard distribution (Normal, Chisq etc.).

```julia
using Distributions, Gadfly

data = rand(Normal(0, 1), 1000)
@printf("N = %i\n", length(data))
@printf("μ = %2.2f\tσ = %2.2f\n", mean(data), std(data))
@printf("range = (%2.2f, %2.2f\n)", minimum(data), maximum(data))
h = plot(x=data, Geom.histogram)
draw(PNG("norm_hist.png", 10cm, 10cm), h)
```


{{out}}

```txt
N = 1000
μ = 0.02	σ = 0.97
range = (-2.76, 3.42)
```



## Kotlin

{{trans|FreeBASIC}}

```scala
// version 1.1.2

val rand = java.util.Random()

fun normalStats(sampleSize: Int) {
    if (sampleSize < 1) return
    val r = DoubleArray(sampleSize)
    val h = IntArray(12) // all zero by default
    /*
       Generate 'sampleSize' normally distributed random numbers with mean 0.5 and SD 0.25
       and calculate in which box they will fall when drawing the histogram
    */
    for (i in 0 until sampleSize) {
        r[i] = 0.5 + rand.nextGaussian() / 4.0
        when {
            r[i] <  0.0 -> h[0]++
            r[i] >= 1.0 -> h[11]++
            else        -> h[1 + (r[i] * 10).toInt()]++
        }
    }

    // adjust one of the h[] values if necessary to ensure they sum to sampleSize
    val adj = sampleSize - h.sum()
    if (adj != 0) {
        for (i in 0..11) {
            h[i] += adj
            if (h[i] >= 0) break
            h[i] -= adj
        }
    }

    val mean = r.average()
    val sd = Math.sqrt(r.map { (it - mean) * (it - mean) }.average())

    // Draw a histogram of the data with interval 0.1
    var numStars: Int
    // If sample size > 300 then normalize histogram to 300
    val scale = if (sampleSize <= 300) 1.0 else 300.0 / sampleSize
    println("Sample size $sampleSize\n")
    println("  Mean ${"%1.6f".format(mean)}  SD ${"%1.6f".format(sd)}\n")
    for (i in 0..11) {
        when (i) {
            0    -> print("< 0.00 : ")
            11   -> print(">=1.00 : ")
            else -> print("  %1.2f : ".format(i / 10.0))
        }
        print("%5d ".format(h[i]))
        numStars = (h[i] * scale + 0.5).toInt()
        println("*".repeat(numStars))
    }
    println()
}

fun main(args: Array<String>) {
    val sampleSizes = intArrayOf(100, 1_000, 10_000, 100_000)
    for (sampleSize in sampleSizes) normalStats(sampleSize)
}
```


{{out}}

```txt

Sample size 100

  Mean 0.525211  SD 0.266316

< 0.00 :     3 ***
  0.10 :     1 *
  0.20 :     3 ***
  0.30 :    11 ***********
  0.40 :    14 **************
  0.50 :    13 *************
  0.60 :    15 ***************
  0.70 :    13 *************
  0.80 :    10 **********
  0.90 :    11 ***********
  1.00 :     4 ****
>=1.00 :     2 **

Sample size 1000

  Mean 0.500948  SD 0.255757

< 0.00 :    29 *********
  0.10 :    35 ***********
  0.20 :    70 *********************
  0.30 :    71 *********************
  0.40 :   138 *****************************************
  0.50 :   139 ******************************************
  0.60 :   168 **************************************************
  0.70 :   123 *************************************
  0.80 :   110 *********************************
  0.90 :    62 *******************
  1.00 :    32 **********
>=1.00 :    23 *******

Sample size 10000

  Mean 0.501376  SD 0.248317

< 0.00 :   240 *******
  0.10 :   305 *********
  0.20 :   617 *******************
  0.30 :   927 ****************************
  0.40 :  1291 ***************************************
  0.50 :  1554 ***********************************************
  0.60 :  1609 ************************************************
  0.70 :  1319 ****************************************
  0.80 :   983 *****************************
  0.90 :   609 ******************
  1.00 :   324 **********
>=1.00 :   222 *******

Sample size 100000

  Mean 0.499427  SD 0.250533

< 0.00 :  2341 *******
  0.10 :  3246 **********
  0.20 :  6005 ******************
  0.30 :  9718 *****************************
  0.40 : 13247 ****************************************
  0.50 : 15595 ***********************************************
  0.60 : 15271 **********************************************
  0.70 : 13405 ****************************************
  0.80 :  9653 *****************************
  0.90 :  5990 ******************
  1.00 :  3257 **********
>=1.00 :  2272 *******

```



## Lasso


```Lasso
define stat1(a) => {
	if(#a->size) => {
		local(mean = (with n in #a sum #n) / #a->size)
		local(sdev = math_pow(((with n in #a sum Math_Pow((#n - #mean),2)) / #a->size),0.5))
		return (:#sdev, #mean)
	else
		return (:0,0)
	}
}
define stat2(a) => {
	if(#a->size) => {
		local(sx = 0, sxx = 0)
		with x in #a do => {
			#sx += #x
			#sxx += #x*#x
		}
		local(sdev = math_pow((#a->size * #sxx - #sx * #sx),0.5) / #a->size)
		return (:#sdev, #sx / #a->size)
	else
		return (:0,0)
	}
}
define histogram(a) => {
	local(
		out = '\r',
		h = array(0,0,0,0,0,0,0,0,0,0,0),
		maxwidth = 50,
		sc = 0
	)
	with n in #a do => {
		if((#n * 10) <= 0) => {
			#h->get(1) += 1
		else((#n * 10) >= 10)
			#h->get(#h->size) += 1
		else
			#h->get(integer(decimal(#n)*10)+1) += 1
		}

	}
	local(mx = decimal(with n in #h max #n))
	with i in #h do => {
		#out->append((#sc/10.0)->asString(-precision=1)+': '+('+' * integer(#i / #mx * #maxwidth))+'\r')
		#sc++
	}
	return #out
}
define normalDist(mean,sdev) => {
	// Uses Box-Muller transform
	return ((-2 * decimal_random->log)->sqrt * (2 * pi * decimal_random)->cos) * #sdev + #mean
}

with scale in array(100,1000,10000) do => {^
	local(n = array)
	loop(#scale) => { #n->insert(normalDist(0.5, 0.2)) }
	local(sdev1,mean1) = stat1(#n)
	local(sdev2,mean2) = stat2(#n)
	#scale' numbers:\r'
    'Naive  method: sd: '+#sdev1+', mean: '+#mean1+'\r'
    'Second  method: sd: '+#sdev2+', mean: '+#mean2+'\r'
    histogram(#n)
    '\r\r'
^}
```


{{out}}

```txt
100 numbers:
Naive  method: sd: 0.199518, mean: 0.506059
Second  method: sd: 0.199518, mean: 0.506059

0.0: ++
0.1: ++++
0.2: +++++++++++++++++
0.3: ++++++++++++++++++++++
0.4: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.5: +++++++++++++++++++++++++++++++++++++++
0.6: +++++++++++++++++++++++++++++++++
0.7: ++++++++++++++++++++++++
0.8: ++++++++++++++++++++
0.9: ++++
1.0: ++


1000 numbers:
Naive  method: sd: 0.199653, mean: 0.504046
Second  method: sd: 0.199653, mean: 0.504046

0.0: +++
0.1: ++++++
0.2: ++++++++++++++++
0.3: ++++++++++++++++++++++++++++++
0.4: +++++++++++++++++++++++++++++++++++++++++++++++
0.5: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.6: ++++++++++++++++++++++++++++++++++++++++++++++
0.7: +++++++++++++++++++++++++
0.8: +++++++++++++++++++
0.9: +++++++
1.0: ++++


10000 numbers:
Naive  method: sd: 0.202354, mean: 0.502519
Second  method: sd: 0.202354, mean: 0.502519

0.0: +++
0.1: +++++++
0.2: +++++++++++++++
0.3: +++++++++++++++++++++++++++++
0.4: ++++++++++++++++++++++++++++++++++++++++++
0.5: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.6: +++++++++++++++++++++++++++++++++++++++++++
0.7: ++++++++++++++++++++++++++++++
0.8: ++++++++++++++++
0.9: +++++++
1.0: ++++
```



## Liberty BASIC

Uses LB Statistics/Basic

```lb
call sample 100000

end

sub sample n
    dim dat( n)
    for i =1 to n
        dat( i) =normalDist( 1, 0.2)
    next i

    '// show mean, standard deviation. Find max, min.
    mx  =-1000
    mn  = 1000
    sum =0
    sSq =0
    for i =1 to n
        d =dat( i)
        mx =max( mx, d)
        mn =min( mn, d)
        sum =sum +d
        sSq =sSq +d^2
    next i
    print n; " data terms used."

    mean =sum / n
    print "Largest term was "; mx; " & smallest was "; mn
    range =mx -mn
    print "Mean ="; mean

    print "Stddev ="; ( sSq /n -mean^2)^0.5

    '// show histogram
    nBins =50
    dim bins( nBins)
    for i =1 to n
        z =int( ( dat( i) -mn) /range *nBins)
        bins( z) =bins( z) +1
    next i
    for b =0 to nBins -1
        for j =1 to int( nBins *bins( b)) /n *30)
            print "#";
        next j
        print
    next b
    print
end sub

function normalDist( m, s)  '   Box Muller method
    u =rnd( 1)
    v =rnd( 1)
    normalDist =( -2 *log( u))^0.5 *cos( 2 *3.14159265 *v)
end function
```

 100000 data terms used.
 Largest term was 4.12950792 & smallest was -4.37934139
 Mean =-0.26785425e-2
 Stddev =1.00097669


 #
 ##
 ###
 #####
 ########
 ############
 ################
 ########################
 ##############################
 ######################################
 ##############################################
 ########################################################
 ###################################################################
 ##############################################################################
 #######################################################################################
 ################################################################################################
 ####################################################################################################
 ########################################################################################################
 #####################################################################################################
 ##############################################################################################
 #########################################################################################
 ##################################################################################
 #########################################################################
 ##############################################################
 ####################################################
 ##########################################
 #################################
 ##########################
 ##################
 #############
 #########
 ######
 ####
 ##
 #
 #


## Lua

Lua provides math.random() to generate uniformly distributed random numbers.  The function gaussian() shown here uses math.random() to generate normally distributed random numbers with given mean and variance.

```Lua
function gaussian (mean, variance)
    return  math.sqrt(-2 * variance * math.log(math.random())) *
            math.cos(2 * math.pi * math.random()) + mean
end

function mean (t)
    local sum = 0
    for k, v in pairs(t) do
        sum = sum + v
    end
    return sum / #t
end

function std (t)
    local squares, avg = 0, mean(t)
    for k, v in pairs(t) do
        squares = squares + ((avg - v) ^ 2)
    end
    local variance = squares / #t
    return math.sqrt(variance)
end

function showHistogram (t)
    local lo = math.ceil(math.min(unpack(t)))
    local hi = math.floor(math.max(unpack(t)))
    local hist, barScale = {}, 200
    for i = lo, hi do
        hist[i] = 0
        for k, v in pairs(t) do
            if math.ceil(v - 0.5) == i then
                hist[i] = hist[i] + 1
            end
        end
        io.write(i .. "\t" .. string.rep('=', hist[i] / #t * barScale))
        print(" " .. hist[i])
    end
end

math.randomseed(os.time())
local t, average, variance = {}, 50, 10
for i = 1, 1000 do
    table.insert(t, gaussian(average, variance))
end
print("Mean:", mean(t) .. ", expected " .. average)
print("StdDev:", std(t) .. ", expected " .. math.sqrt(variance) .. "\n")
showHistogram(t)
```

{{out}}

```txt
Mean:   50.008328894275, expected 50
StdDev: 3.2374717425824, expected 3.1622776601684

41       3
42      = 8
43      == 11
44      ==== 22
45
### =
 38
46
### ======
 60
47
### ========
 73
48
### ============
 92
49
### =================
 118
50
### =====================
 136
51
### ===================
 128
52
### ===========
 89
53
### ===========
 89
54
### =====
 56
55
### =
 37
56      === 18
57      = 7
58      = 5
59      = 6
60       2
```



## Maple

Maple has a built-in for sampling directly from [http://www.maplesoft.com/support/help/Maple/view.aspx?path=Statistics/Distributions/Normal Normal] distributions:

```maple
with(Statistics):
n := 100000:
X := Sample( Normal(0,1), n );
Mean( X );
StandardDeviation( X );
Histogram( X );
```



## Mathematica


```Mathematica
x:= RandomReal[1]
SampleNormal[n_] := (Print[#//Length, " numbers, Mean : ", #//Mean, ", StandardDeviation : ", #//StandardDeviation];
    Histogram[#, BarOrigin -> Left,Axes -> False])& [(Table[(-2*Log[x])^0.5*Cos[2*Pi*x], {n} ]]

Invocation:
SampleNormal[ 10000 ]
->10000 numbers, Mean : -0.0122308, StandardDeviation : 1.00646

```

[[File:Mma_NormalDistribution.png]]

=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
  N = 100000;
  x = randn(N,1);
  mean(x)
  std(x)
  [nn,xx] = hist(x,100);
  bar(xx,nn);
```



## PARI/GP

{{works with|PARI/GP|2.4.3 and above}}

```parigp
rnormal()={
	my(u1=random(1.),u2=random(1.);
	sqrt(-2*log(u1))*cos(2*Pi*u1)
	\\ Could easily be extended with a second normal at very little cost.
};
mean(v)={
  sum(i=1,#v,v[i])/#v
};
stdev(v,mu="")={
  if(mu=="",mu=mean(v));
  sqrt(sum(i=1,#v,(v[i]-mu)^2))/#v
};
histogram(v,bins=16,low=0,high=1)={
  my(u=vector(bins),width=(high-low)/bins);
  for(i=1,#v,u[(v[i]-low)\width+1]++);
  u
};
show(n)={
  my(v=vector(n,i,rnormal()),m=mean(v),s=stdev(v,m),h,sz=ceil(n/300));
  h=histogram(v,,vecmin(v)-.1,vecmax(v)+.1);
  for(i=1,#h,for(j=1,h[i]\sz,print1("#"));print());
};
show(10^4)
```


For versions before 2.4.3, define

```parigp
rreal()={
  my(pr=32*ceil(default(realprecision)*log(10)/log(4294967296))); \\ Current precision
  random(2^pr)*1.>>pr
};
```

and use <code>rreal()</code> in place of <code>random(1.)</code>.

A PARI implementation:

```C
GEN
rnormal(long prec)
{
	pari_sp ltop = avma;
	GEN u1, u2, left, right, ret;
	u1 = randomr(prec);
	u2 = randomr(prec);
	left = sqrtr_abs(shiftr(mplog(u1), 1));
	right = mpcos(mulrr(shiftr(mppi(prec), 1), u2));

	ret = mulrr(left, right);
	ret = gerepileupto(ltop, ret);
	return ret;
}
```

Use <code>mpsincos</code> and caching to generate two values at nearly the same cost.


## Pascal

{{works with|free Pascal}}
//not neccessary include unit math if using function rnorm

got some trouble with math.randg needs this call randg(cMean,cMean*cStdDiv), whereas randg(cMean,cStdDiv) to get the same results??

From  [http://www.freepascal.org/docs-html/rtl/math/randg.html Free Pascal Docs unit math]

```pascal
Program Example40;
{$IFDEF FPC}
  {$MOde objFPC}
{$ENDIF}
{ Program to demonstrate the randg function. }
Uses Math;

type
  tTestData =  extended;//because of math.randg
  ttstfunc = function  (mean, sd: tTestData): tTestData;
  tExArray = Array of tTestData;
  tSolution = record
                SolExArr : tExArray;
                SollowVal,
                SolHighVal,
                SolMean,
                SolStdDiv : tTestData;
                SolSmpCnt : LongInt;
              end;

function getSol(genFunc:ttstfunc;Mean,StdDiv: tTestData;smpCnt: LongInt): tSolution;
var
  GenValue,
  sumValue,
  sumsqrVal : extended;
Begin
  with result do
  Begin
    SolSmpCnt  := smpCnt;
    SolMean    := 0;
    SolStdDiv  := 0;
    SolLowVal  := Mean+50* StdDiv;
    SolHighVal := Mean-50* StdDiv;
    setlength(SolExArr,smpCnt);
    if smpCnt <= 0 then
      EXIT;
    sumValue   := 0;
    sumsqrVal  := 0;
    repeat
      GenValue   := genFunc(Mean,StdDiv);
      sumValue   := sumvalue+GenValue;
      sumsqrVal  :=  sumsqrVal+sqr(GenValue);
      IF GenValue < SollowVal then
        SollowVal:= GenValue
      else
        IF GenValue > SolHighVal then
           SolHighVal := GenValue;
      dec(smpCnt);
      SolExArr[smpCnt] := GenValue;
    until smpCnt<= 0;
    SolMean := sumValue/SolSmpCnt;
    SolStdDiv := sqrt(sumsqrVal/SolSmpCnt-sqr(SolMean));
  end;
end;

//http://wiki.freepascal.org/Generating_Random_Numbers#Normal_.28Gaussian.29_Distribution
function rnorm (mean, sd: tTestData): tTestData;
 {Calculates Gaussian random numbers according to the Box-Müller approach}
  var
   u1, u2: extended;
 begin
   u1 := random;
   u2 := random;
   rnorm := mean * abs(1 + sqrt(-2 * (ln(u1))) * cos(2 * pi * u2) * sd);
  end;

procedure Histo(const sol:TSolution;Colcnt,ColLen :LongInt);
var
  CntHisto : array of integer;
  LoLmt,HiLmt,span : tTestData;
  i, j,cnt,maxCnt: LongInt;
  sCross : Ansistring;
Begin
  setlength(CntHisto,Colcnt);
  with Sol do
  Begin
    span := solHighVal-solLowVal;
    LoLmt := solLowVal;
    writeln('Count: ',SolSmpCnt:10,' Mean ',SolMean:10:6,' StdDiv ',SolStdDIv:10:6);
    writeln('span : ',span:10:5,' Low  ',solLowVal:10:6,'   high ',solHighVal:10:6);

  end;
  maxCnt := 0;
  For j := 0 to Colcnt-1 do
  Begin
    HiLmt:= LoLmt+span/Colcnt;
    cnt := 0;
    with sol do
      For i := 0 to High(SolExArr) do
         IF (HiLmt > SolExArr[i]) AND  (SolExArr[i]>= LoLmt) then
            inc(cnt);
    CntHisto[j] := cnt;
    IF maxCnt < cnt then
      maxCnt := cnt;
    LoLmt:=  HiLmt;
  end;
  inc(CntHisto[Colcnt]); // for HiLmt itself
  writeln;
  LoLmt := sol.solLowVal;
  For i := 0 to Colcnt-1 do
  Begin
    Writeln(LoLmt:8:4,': ');
    cnt:= Round(CntHisto[i]*ColLen/maxCnt);
    setlength(sCross,cnt+3);
    fillChar(sCross[1],3,' ');
    fillChar(sCross[4],cnt,'#');
    writeln(CntHisto[i]:10,sCross);
    LoLmt := LoLmt+span/Colcnt;
  end;
  Writeln(sol.solHighVal:8:4,': ');
end;

const
  cHistCnt = 11;
  cColLen = 65;

  cStdDiv = 0.25;
  cMean   = 20*cStdDiv;
var
  mySol : tSolution;
begin
  Randomize;
  // test of randg of unit math
  Writeln('function randg');
  mySol := getSol(@randg,cMean,cMean*cStdDiv,100000);
  Histo(mySol,cHistCnt,cColLen);
  writeln;
  // test of rnorm from wiki
  Writeln('function rnorm');
  mySol := getSol(@rnorm,cMean,cStdDiv,1000000);
  Histo(mySol,cHistCnt,cColLen);
end.
```

{{out}}
```txt

function randg
Count:     100000 Mean   5.000326 StdDiv   1.250027
span :   10.65123 Low   -0.333310   high  10.317922

 -0.3333:
        25
  0.6350:
       287   #
  1.6033:
      2291   #####
  2.5716:
      9531   #####################
  3.5399:
     22608   #################################################
  4.5082:
     29953   #################################################################
  5.4765:
     22917   ##################################################
  6.4447:
      9716   #####################
  7.4130:
      2352   #####
  8.3813:
       295   #
  9.3496:
        24
 10.3179:

function rnorm
Count:    1000000 Mean   4.998391 StdDiv   1.251103
span :   11.08994 Low    0.001521   high  11.091461

  0.0015:
       704
  1.0097:
      7797   ##
  2.0179:
     49235   ###########
  3.0261:
    162761   ####################################
  4.0342:
    293242   #################################################################
  5.0424:
    285818   ###############################################################
  6.0506:
    150781   #################################
  7.0588:
     42641   #########
  8.0669:
      6467   #
  9.0751:
       528
 10.0833:
        25
 11.0915:

```



## Perl

{{trans|Perl 6}}

```perl>use constant pi =
 3.14159265;
use List::Util qw(sum reduce min max);

sub normdist {
    my($m, $sigma) = @_;
    my $r = sqrt -2 * log rand;
    my $theta = 2 * pi * rand;
    $r * cos($theta) * $sigma + $m;
}

$size = 100000; $mean = 50; $stddev = 4;

push @dataset, normdist($mean,$stddev) for 1..$size;

my $m = sum(@dataset) / $size;
print "m = $m\n";

my $sigma = sqrt( (reduce { $a + $b **2 } 0,@dataset) / $size - $m**2 );
print "sigma = $sigma\n";

    $hash{int $_}++ for @dataset;
    my $scale = 180 * $stddev / $size;
    my @subbar = < ⎸ ▏ ▎ ▍ ▌ ▋ ▊ ▉ █ >;
    for $i (min(@dataset)..max(@dataset)) {
        my $x = ($hash{$i} // 0) * $scale;
        my $full = int $x;
        my $part = 8 * ($x - $full);
        my $t1 = '█' x $full;
        my $t2 = $subbar[$part];
        print "$i\t$t1$t2\n";
    }

```

{{out}}
<pre  style="height:35ex">32  ⎸
33  ⎸
34  ⎸
35  ⎸
36  ▎
37  ▋
38  █▏
39  ██▍
40  ████▍
41  ███████▌
42  ████████████⎸
43  ███████████████████▏
44  ████████████████████████████⎸
45  ██████████████████████████████████████▎
46  █████████████████████████████████████████████████▎
47  ██████████████████████████████████████████████████████████▋
48  ██████████████████████████████████████████████████████████████████▋
49  ███████████████████████████████████████████████████████████████████████▍
50  ██████████████████████████████████████████████████████████████████████▋
51  ██████████████████████████████████████████████████████████████████▌
52  ████████████████████████████████████████████████████████████▎
53  ████████████████████████████████████████████████▏
54  █████████████████████████████████████▊
55  ███████████████████████████▍
56  ███████████████████▊
57  ████████████▌
58  ███████▌
59  ████▍
60  ██▏
61  █⎸
62  ▌
63  ▏
64  ⎸
65  ⎸
66  ⎸
```



## Perl 6

{{works with|Rakudo|2018.03}}

```perl6
sub normdist ($m, $σ) {
    my $r = sqrt -2 * log rand;
    my $Θ = τ * rand;
    $r * cos($Θ) * $σ + $m;
}

sub MAIN ($size = 100000, $mean = 50, $stddev = 4) {
    my @dataset = normdist($mean,$stddev) xx $size;

    my $m = [+](@dataset) / $size;
    say (:$m);

    my $σ = sqrt [+](@dataset X** 2) / $size - $m**2;
    say (:$σ);

    (my %hash){.round}++ for @dataset;
    my $scale = 180 * $stddev / $size;
    constant @subbar = < ⎸ ▏ ▎ ▍ ▌ ▋ ▊ ▉ █ >;
    for %hash.keys».Int.minmax(+*) -> $i {
        my $x = (%hash{$i} // 0) * $scale;
        my $full = floor $x;
        my $part = 8 * ($x - $full);
        say $i, "\t", '█' x $full, @subbar[$part];
    }
}
```

{{out}}

```txt
"m" => 50.006107405837142e0
"σ" => 4.0814435639885254e0
33	⎸
34	⎸
35	⎸
36	▏
37	▎
38	▊
39	█▋
40	███⎸
41	█████▊
42	██████████⎸
43	███████████████▋
44	███████████████████████▏
45	████████████████████████████████▌
46	███████████████████████████████████████████▍
47	██████████████████████████████████████████████████████▏
48	███████████████████████████████████████████████████████████████▏
49	█████████████████████████████████████████████████████████████████████▋
50	███████████████████████████████████████████████████████████████████████▊
51	█████████████████████████████████████████████████████████████████████▌
52	███████████████████████████████████████████████████████████████⎸
53	██████████████████████████████████████████████████████▎
54	███████████████████████████████████████████⎸
55	████████████████████████████████▌
56	███████████████████████▍
57	███████████████▉
58	█████████▉
59	█████▍
60	███▍
61	█▋
62	▊
63	▍
64	▏
65	⎸
66	⎸
67	⎸
```



## Phix

{{trans|Liberty_BASIC}}

```Phix
procedure sample(integer n)
-- show mean, standard deviation. Find max, min.
sequence dat = repeat(0,n)
    for i=1 to n do
        dat[i] = sqrt(-2*log(rnd()))*cos(2*PI*rnd())
    end for
    printf(1,"%d data terms used.\n",{n})

    atom mean = sum(dat)/n,
         mx = max(dat),
         mn = min(dat),
         range = mx-mn
    printf(1,"Largest term was %g & smallest was %g\n",{mx,mn})
    printf(1,"Mean = %g\n",{mean})
    printf(1,"Stddev = %g\n",sqrt(sum(sq_mul(dat,dat))/n-mean*mean))

    -- show histogram
    integer nBins = 50
    sequence bins = repeat(0,nBins+1)
    for i=1 to n do
        bins[floor((dat[i]-mn)/range*nBins)+1] += 1
    end for
    for b=1 to nBins do
        puts(1,repeat('#',floor(nBins*bins[b]/n*30))&"\n")
    end for
end procedure

sample(100000)
```

{{Out}}

```txt

100000 data terms used.
Largest term was 4.30779 & smallest was -4.11902
Mean = -0.00252597
Stddev = 1.00067

#
##
####
######
##########
#############
##################
########################
#################################
########################################
####################################################
#############################################################
######################################################################
###############################################################################
#######################################################################################
###############################################################################################
#################################################################################################
#####################################################################################################
###################################################################################################
################################################################################################
########################################################################################
###############################################################################
#######################################################################
############################################################
#################################################
#######################################
##############################
#########################
################
############
#########
######
####
##
#

```

{{trans|Lua}}

```Phix
function gaussian(atom mean, atom variance)
    return sqrt(-2 * variance * log(rnd())) *
           cos(2 * variance * PI * rnd()) + mean
end function

function mean(sequence t)
    return sum(t)/length(t)
end function

function std(sequence t)
    atom squares = 0,
         avg = mean(t)
    for i=1 to length(t) do
        squares += power(avg-t[i],2)
    end for
    atom variance = squares/length(t)
    return sqrt(variance)
end function

procedure showHistogram(sequence t)
    for i=ceil(min(t)) to floor(max(t)) do
        integer n = 0
        for k=1 to length(t) do
            n += ceil(t[k]-0.5)=i
        end for
        integer l = floor(n/length(t)*200)
        printf(1,"%d %s %d\n",{i,repeat('=',l),n})
    end for
end procedure

sequence t = repeat(0,100000)
integer avg = 50, variance = 10
for i=1 to length(t) do
    t[i] = gaussian(avg, variance)
end for
printf(1,"Mean: %g, expected %g\n",{mean(t),avg})
printf(1,"StdDev: %g, expected %g\n",{std(t),sqrt(variance)})
showHistogram(t)
```

{{Out}}

```txt

Mean: 50.0041, expected 50
StdDev: 3.1673, expected 3.16228
37  2
38  7
39  30
40  92
41  220
42 = 523
43 == 1098
44 ==== 2140
45
### =
 3690
46
### =====
 5753
47
### =========
 7906
48
### ==============
 10299
49
### =================
 11813
50
### ===================
 12555
51
### =================
 11934
52
### ==============
 10327
53
### ==========
 8099
54
### =====
 5733
55
### =
 3684
56 ==== 2126
57 == 1098
58  487
59  226
60  106
61  36
62  9
63  7

```



## PureBasic


```purebasic
Procedure.f randomf(resolution = 2147483647)
  ProcedureReturn Random(resolution) / resolution
EndProcedure

Procedure.f normalDist() ;Box Muller method
   ProcedureReturn Sqr(-2 * Log(randomf())) * Cos(2 * #PI * randomf())
EndProcedure

Procedure sample(n, nBins = 50)
  Protected i, maxBinValue, binNumber
  Protected.f d, mean, sum, sumSq, mx, mn, range

  Dim dat.f(n)
  For i = 1 To n
    dat(i) = normalDist()
  Next

  ;show mean, standard deviation, find max & min.
  mx  = -1000
  mn  =  1000
  sum = 0
  sumSq = 0
  For i = 1 To n
    d = dat(i)
    If d > mx: mx = d: EndIf
    If d < mn: mn = d: EndIf
    sum + d
    sumSq + d * d
  Next

  PrintN(Str(n) + " data terms used.")
  PrintN("Largest term was " + StrF(mx) + " & smallest was " + StrF(mn))
  mean = sum / n
  PrintN("Mean = " + StrF(mean))
  PrintN("Stddev = " + StrF((sumSq / n) - Sqr(mean * mean)))

  ;show histogram
  range = mx - mn
  Dim bins(nBins)
  For i = 1 To n
    binNumber = Int(nBins * (dat(i) - mn) / range)
    bins(binNumber) + 1
  Next

  maxBinValue = 1
  For i = 0 To nBins
    If bins(i) > maxBinValue
      maxBinValue = bins(i)
    EndIf
  Next

  #normalizedMaxValue = 70
  For binNumber = 0 To nBins
    tickMarks = Round(bins(binNumber) * #normalizedMaxValue / maxBinValue, #PB_Round_Nearest)
    PrintN(ReplaceString(Space(tickMarks), " ", "#"))
  Next
  PrintN("")
EndProcedure

If OpenConsole()
  sample(100000)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
100000 data terms used.
Largest term was 4.5352029800 & smallest was -4.5405135155
Mean = 0.0012346541
Stddev = 0.9959455132





#
###
######
##########
##################
############################
#########################################
#####################################################
################################################################
######################################################################
######################################################################
################################################################
#####################################################
#########################################
#############################
##################
##########
######
###
#





```



## Python

This uses the external [http://matplotlib.org/ matplotlib] package as well as the built-in standardlib function [http://docs.python.org/2/library/random.html?highlight=gauss#random.gauss random.gauss].

```python
from __future__ import division
import matplotlib.pyplot as plt
import random

mean, stddev, size = 50, 4, 100000
data = [random.gauss(mean, stddev) for c in range(size)]

mn = sum(data) / size
sd = (sum(x*x for x in data) / size
      - (sum(data) / size) ** 2) ** 0.5

print("Sample mean = %g; Stddev = %g; max = %g; min = %g for %i values"
      % (mn, sd, max(data), min(data), size))

plt.hist(data,bins=50)
```


{{out}}

```txt
Sample mean = 49.9822; Stddev = 4.00938; max = 66.8091; min = 33.5283 for 100000 values
```


[[File:Normal_distribution_py.svg]]


## R

R can generate random normal distributed numbers using the [https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Normal.html rnorm] command:

```r
n = 100000;
X = rnorm(n, mean  = 0, sd = 1);
mean( X );
sd( X );
hist( X );
```



## Racket

This shows how one would generate samples from a normal distribution,
compute statistics and plot a histogram.
[[File:histogram-racket.png|thumb|right]]

```racket

#lang racket
(require math (planet williams/science/histogram-with-graphics))

(define data (sample (normal-dist 50 4) 100000))

(displayln (~a "Mean:\t"   (mean data)))
(displayln (~a "Stddev:\t" (stddev data)))
(displayln (~a "Max:\t"    (apply max data)))
(displayln (~a "Min:\t"    (apply min data)))

(define h (make-histogram-with-ranges-uniform 40 30 70))
(for ([x data]) (histogram-increment! h x))
(histogram-plot h "Normal distribution μ=50 σ=4")

```


The other part of the task was to produce normal distributed numbers from a unit distribution.
The following code is an implementation of the polar method. It is a slightly modified
version of [http://planet.plt-scheme.org/package-source/schematics/random.plt/1/0/random.ss code]
originally written by Sebastian Egner.

```racket

#lang racket
(require math)

(define random-normal
  (let ([unit (uniform-dist)]
        [next #f])
    (λ (μ σ)
      (if next
          (begin0
            (+ μ (* σ next))
            (set! next #f))
          (let loop ()
            (let* ([v1 (- (* 2.0 (sample unit)) 1.0)]
                   [v2 (- (* 2.0 (sample unit)) 1.0)]
                   [s (+ (sqr v1) (sqr v2))])
              (cond [(>= s 1) (loop)]
                    [else (define scale (sqrt (/ (* -2.0 (log s)) s)))
                          (set! next (* scale v2))
                          (+ μ (* σ scale v1))])))))))

```



## REXX

The REXX language doesn't have any "higher math" BIF functions like   SIN, COS, LN, LOG, SQRT, EXP, POW, etc,

so we hoi polloi programmers have to roll our own.

```rexx
/*REXX program generates  10,000  normally distributed numbers  (Gaussian distribution).*/
numeric digits 20                                /*use  20  decimal digits for accuracy.*/
parse arg n seed .                               /*obtain optional arguments from the CL*/
if n==''  |  n==","     then n= 10000            /*Not specified?  Then use the default.*/
if datatype(seed, 'W')  then call random ,,seed  /*seed is for repeatable RANDOM numbers*/
call pi                                          /*call subroutine to define pi constant*/
        do g=1  for n;   #.g= sqrt( -2 * ln( rand() ) )      *      cos( 2 * pi * rand() )
        end   /*g*/                              /* [↑]  uniform random number ───► #.g */
s= 0
mn= #.1;        mx= mn;        noise= n * .0005  /*calculate the noise:  1/20th %  of N.*/
ss= 0
        do j=1  for n;  _=#.j; s=s+_;  ss=ss+_*_ /*the sum,  and  the sum of squares.   */
        mn= min(mn, _);        mx= max(mx, _)    /*find the minimum  and the maximum.   */
        end   /*j*/
!.= 0
say 'number of data points = '   aa(n  )
say '              minimum = '   aa(mn )
say '              maximum = '   aa(mx )
say '      arithmetic mean = '   aa(s/n)
say '   standard deviation = '   aa(sqrt( ss/n - (s/n) **2) )
?mn= !.1;    ?mx= ?mn                            /*define minimum & maximum value so far*/
parse value  scrSize()  with  sd sw .            /*obtain the (true) screen size of term*/  /*◄──not all REXXes have this BIF*/
sdE= sd - 4                                      /*the effective (useable) screen depth.*/
swE= sw - 1                                      /* "      "         "        "   width.*/
$= 1 / max(1, mx-mn) * sdE                       /*$  is used for scaling depth of histo*/
            do i=1  for n;        ?= trunc( (#.i-mn) * $) /*calculate the relative line.*/
            !.?= !.? + 1                                  /*bump the counter.           */
            ?mn= min(?mn, !.?);   ?mx= max(?mx, !.?)      /*find the minimum and maximum*/
            end   /*i*/
f=swE/?mx                                                 /*limit graph to 1 full screen*/
            do h=0  for sdE;      _= !.h                  /*obtain a data point.        */
            if _>noise  then say copies('─', trunc(_*f) ) /*display a bar of histogram. */
            end   /*h*/                                   /*[↑]  use a hyphen for histo.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
aa:      parse arg a; return left('', (a>=0) + 2 * datatype(a, 'W'))a  /*prepend a blank if #>=0, add 2 blanks if whole.*/
e:       e = 2.7182818284590452353602874713526624977572470936999595749669676277240766303535;                     return e
pi:      pi= 3.1415926535897932384626433832795028841971693993751058209749445923078164062862;                     return pi
r2r:     return arg(1)  //  (pi() * 2)                                 /*normalize the given angle (in radians) to ±2pi.*/
rand:    return random(1, 1e5)  /  1e5                                 /*REXX generates uniform random postive integers.*/
.sincos: parse arg z,_,i; x= x*x; p= z;  do k=2  by 2; _= -_*x/(k*(k+i)); z= z+_; if z=p  then leave; p= z; end;  return z
/*──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
ln:      procedure; parse arg x,f; call e; ig= x>1.5; is= 1 -2*(ig\==1); ii= 0; xx= x;  do while ig & xx>1.5 | \ig & xx<.5
         _= e;  do k=-1; iz= xx*_ **-is;  if k>=0 & (ig & iz<1 | \ig & iz>.5)  then leave;  _= _*_; izz= iz; end;  xx= izz
         ii= ii +is*2**k; end; x= x*e**-ii-1; z=0; _=-1; p=z; do k=1;_=-_*x;z=z+_/k;if z=p then leave;p=z;end; return z+ii
/*──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
cos:     procedure; parse arg x; x=r2r(x); a=abs(x); hpi= pi*.5;  numeric fuzz min(6, digits()-3); if a=pi  then return -1
         if a=hpi | a=hpi*3  then return 0; if a=pi/3  then return .5; if a=pi*2/3 then return -.5; return .sinCos(1,1,-1)
/*──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
sqrt:    procedure; parse arg x;  if x=0  then return 0;  d= digits();   m.= 9;   numeric digits;   numeric form;   h= d+6
         parse value format(x,2,1,,0) 'E0'  with  g 'E' _ .; g=g*.5'e'_%2;    do j=0  while h>9; m.j=h; h=h%2+1; end /*j*/
           do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;  end  /*k*/;          numeric digits d;   return g/1
```

This REXX program makes use of   '''scrsize'''   REXX program (or BIF) which is used to determine the screen size of the terminal (console);   this is to aid in maximizing the width of the horizontal histogram.

The   '''SCRSIZE.REX'''   REXX program is included here   ──►   [[SCRSIZE.REX]].

{{out|output|text=  when using the default input:}}

(The output shown when the screen size is 62<small>x</small>140.)

The graph is shown at   <big>'''<sup>3</sup>/<sub>4</sub>'''   size.
<pre style="font-size:75%">
number of data points =     10000
              minimum =  -3.8181072371544448250
              maximum =   3.5445917138265268562
      arithmetic mean =  -0.01406470979976873427
   standard deviation =   0.99486092191249231518
─
─
───
────
─────
─────
────────
───────────
──────────────
─────────────────────
──────────────────────
──────────────────────────────────
────────────────────────────────────────
───────────────────────────────────────────────
─────────────────────────────────────────────────────
─────────────────────────────────────────────────────────────────────────
─────────────────────────────────────────────────────────────────
─────────────────────────────────────────────────────────────────────────────────────
──────────────────────────────────────────────────────────────────────────────────────────────────
──────────────────────────────────────────────────────────────────────────────────────────────
────────────────────────────────────────────────────────────────────────────────────────────────────────────────
────────────────────────────────────────────────────────────────────────────────────────────────────────────────
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
──────────────────────────────────────────────────────────────────────────────────────────────────────────
──────────────────────────────────────────────────────────────────────────────────────────────────────────────────
────────────────────────────────────────────────────────────────────────────────────────────────────────────────
───────────────────────────────────────────────────────────────────────────────────────────────
────────────────────────────────────────────────────────────────────────────
───────────────────────────────────────────────────────────────────────
───────────────────────────────────────────────────────────────────
─────────────────────────────────────────────────────
─────────────────────────────────────────────────
─────────────────────────────────
──────────────────────────────────
───────────────────────
──────────────────────
──────────────────
───────────
──────────
──────
───
────
──
─

```



## Run BASIC


```runbasic

s	= 100000
h$	= "
### =======================================================
"
h$	= h$ + h$
dim ndis(s)
' mean and standard deviation.
mx	= -9999
mn	=  9999
sum	= 0
sumSqr	= 0
for i = 1 to s	' find minimum and maximum
	ms	= rnd(1)
	ss	= rnd(1)
	nd 	= (-2 * log(ms))^0.5 * cos(2 *3.14159265 * ss) ' normal distribution
	ndis(i)	= nd
	mx	= max(mx, nd)
	mn	= min(mn, nd)
	sum	= sum + nd
	sumSqr	= sumSqr + nd ^ 2
next i

mean	= sum / s
range	= mx - mn

print "Samples   :"; s
print "Largest   :"; mx
print "Smallest  :"; mn
print "Range     :"; range
print "Mean      :"; mean
print "Stand Dev :"; (sumSqr /s -mean^2)^0.5

'Show chart of histogram
nBins	= 50
dim bins(nBins)
for i = 1 to s
	z	= int((ndis(i) -mn) /range *nBins)
	bins(z)	= bins(z) + 1
	mb	= max(bins(z),mb)
next i
for b = 0 to nBins -1
 print using("##",b);" ";using("#####",bins(b));" ";left$(h$,(bins(b) / mb) * 90)
next b
END
```

{{out}}

```txt

Samples   :100000
Largest   :4.61187177
Smallest  :-4.21695424
Range     :8.82882601
Mean      :-9.25042513e-4
Stand Dev :1.00680067

=
==
===
=====

### ==


### =======


### ===========


### =================


### ========================


### =================================


### =========================================


### ===================================================


### =============================================================


### =====================================================================


### =============================================================================


### =================================================================================


### ====================================================================================


### ==================================================================================


### ================================================================================


### ===========================================================================


### ======================================================================


### ============================================================


### ==================================================


### ========================================


### ===============================


### ======================


### ===============


### =========


### ====


### =

=====
===
=
=


```



## SAS


```sas
data test;
n=100000;
twopi=2*constant('pi');
do i=1 to n;
	u=ranuni(0);
	v=ranuni(0);
	r=sqrt(-2*log(u));
	x=r*cos(twopi*v);
	y=r*sin(twopi*v);
	z=rannor(0);
	output;
end;
keep x y z;

proc means mean stddev;

proc univariate;
histogram /normal;

run;

/*
Variable            Mean         Std Dev
----------------------------------------
x             -0.0052720       0.9988467
y            0.000023995       1.0019996
z              0.0012857       1.0056536
*/
```



## Sidef

{{trans|Perl 6}}

```ruby
define τ = Num.tau

func normdist (m, σ) {
    var r = sqrt(-2 * 1.rand.log)
    var Θ = (τ * 1.rand)
    r * Θ.cos * σ + m
}

var size = 100_000
var mean = 50
var stddev = 4

var dataset = size.of { normdist(mean, stddev) }
var m = (dataset.sum / size)
say ("m: #{m}")

var σ = sqrt(dataset »**» 2 -> sum / size - m**2)
say ("s: #{σ}")

var hash = Hash()
dataset.each { |n| hash{ n.round } := 0 ++ }

var scale = (180 * stddev / size)
const subbar = < ⎸ ▏ ▎ ▍ ▌ ▋ ▊ ▉ █ >

for i in (hash.keys.map{.to_i}.sort) {
    var x = (hash{i} * scale)
    var full = x.int
    var part = (8 * (x - full))
    say (i, "\t", '█' * full, subbar[part])
}
```

{{out}}

```txt

m: 49.99538275618550306540055142077589
s: 4.00295544816687358837821680496471
33	⎸
34	⎸
35	⎸
36	▏
37	▎
38	▊
39	█▋
40	███▏
41	██████▏
42	█████████▍
43	███████████████▌
44	███████████████████████▋
45	████████████████████████████████▍
46	████████████████████████████████████████████▎
47	█████████████████████████████████████████████████████▍
48	███████████████████████████████████████████████████████████████▍
49	█████████████████████████████████████████████████████████████████████▌
50	████████████████████████████████████████████████████████████████████████▋
51	█████████████████████████████████████████████████████████████████████▊
52	██████████████████████████████████████████████████████████████▏
53	████████████████████████████████████████████████████▉
54	███████████████████████████████████████████▉
55	█████████████████████████████████▎
56	███████████████████████⎸
57	███████████████▋
58	█████████▋
59	█████▍
60	███▍
61	█▊
62	▋
63	▍
64	▏
65	⎸
66	⎸

```



## Stata

Pairs of normal numbers are generated from pairs of uniform numbers using the polar form of [https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform Box-Muller method]. A normal density is added to the histogram for comparison. See '''[http://www.stata.com/help.cgi?histogram histogram]''' in Stata help. A [https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot Q-Q plot] is also drawn.


```stata
. clear all
. set obs 100000
. gen u=runiform()
. gen v=runiform()
. gen r=sqrt(-2*log(u))
. gen x=r*cos(2*_pi*v)
. gen y=r*sin(2*_pi*v)
. gen z=rnormal()
. summarize x y z

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
           x |    100,000    .0025861    1.002346  -4.508192   4.164336
           y |    100,000    .0017389    1.001586  -4.631144   4.460274
           z |    100,000     .005054    .9998861  -5.134265   4.449522
. hist x, normal
. hist y, normal
. hist z, normal
. qqplot x z, msize(tiny)
```



## Tcl


```tcl
package require Tcl 8.5
# Uses the Box-Muller transform to compute a pair of normal random numbers
proc tcl::mathfunc::nrand {mean stddev} {
    variable savednormalrandom
    if {[info exists savednormalrandom]} {
	return [expr {$savednormalrandom*$stddev + $mean}][unset savednormalrandom]
    }
    set r [expr {sqrt(-2*log(rand()))}]
    set theta [expr {2*3.1415927*rand()}]
    set savednormalrandom [expr {$r*sin($theta)}]
    expr {$r*cos($theta)*$stddev + $mean}
}
proc stats {size {slotfactor 10}} {
    set sum 0.0
    set sum2 0.0
    for {set i 0} {$i < $size} {incr i} {
	set r [expr { nrand(0.5, 0.2) }]

	incr histo([expr {int(floor($r*$slotfactor))}])
	set sum [expr {$sum + $r}]
	set sum2 [expr {$sum2 + $r**2}]
    }
    set mean [expr {$sum / $size}]
    set stddev [expr {sqrt($sum2/$size - $mean**2)}]
    puts "$size numbers"
    puts "Mean:   $mean"
    puts "StdDev: $stddev"
    foreach i [lsort -integer [array names histo]] {
	puts [string repeat "*" [expr {$histo($i)*350/int($size)}]]
    }
}

stats 100
puts ""
stats 1000
puts ""
stats 10000
puts ""
stats 100000 20
```

Sample output:

```txt

100 numbers
Mean:   0.49355955990390254
StdDev: 0.19651396178121985
***
*******
**************
***********************************
********************************************************
******************************************************************
*************************************************************************
******************************************
**************************************
**************

1000 numbers
Mean:   0.5066940614105869
StdDev: 0.2016794788065389


*
*****
**************
****************************
**********************************************************
****************************************************************
*************************************************************
******************************************************
***********************************
************
*********
*

10000 numbers
Mean:   0.49980964730768285
StdDev: 0.1968441612522318

*
*****
***************
*******************************
*****************************************************
******************************************************************
*******************************************************************
****************************************************
*********************************
***************
*****
*



100000 numbers
Mean:   0.49960438950922254
StdDev: 0.20060211160998606





*
**
***
******
*********
**************
******************
***********************
*****************************
********************************
**********************************
**********************************
********************************
****************************
***********************
******************
*************
*********
******
***
**
*








```

The blank lines in the output are where the number of samples is too small to even merit a single unit on the histogram.


## VBA


```vb
Public Sub standard_normal()
    Dim s() As Variant, bins(71) As Single
    ReDim s(20000)
    For i = 1 To 20000
        s(i) = WorksheetFunction.Norm_S_Inv(Rnd())
    Next i
    For i = -35 To 35
        bins(i + 36) = i / 10
    Next i
    Debug.Print "sample size"; UBound(s), "mean"; mean(s), "standard deviation"; standard_deviation(s)
            t = WorksheetFunction.Frequency(s, bins)
    For i = -35 To 35
        Debug.Print Format((i - 1) / 10, "0.00");
        Debug.Print "-"; Format(i / 10, "0.00"),
        Debug.Print String$(t(i + 36, 1) / 10, "X");
        Debug.Print
    Next i
End Sub
```
{{out}}

```txt
sample size 20000           mean-5,26306310478751E-03   standard deviation 1,00355037427319
-3,60--3,50
-3,50--3,40
-3,40--3,30
-3,30--3,20
-3,20--3,10
-3,10--3,00
-3,00--2,90   XX
-2,90--2,80   X
-2,80--2,70   XX
-2,70--2,60   XX
-2,60--2,50   XXX
-2,50--2,40   XXXX
-2,40--2,30   XXXXX
-2,30--2,20   XXXXXXXX
-2,20--2,10   XXXXXXXX
-2,10--2,00   XXXXXXXXXXX
-2,00--1,90   XXXXXXXXXXXXX
-1,90--1,80   XXXXXXXXXXXXXXX
-1,80--1,70   XXXXXXXXXXXXXXXX
-1,70--1,60   XXXXXXXXXXXXXXXXXXXX
-1,60--1,50   XXXXXXXXXXXXXXXXXXXXXXXX
-1,50--1,40   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-1,40--1,30   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-1,30--1,20   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-1,20--1,10   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-1,10--1,00   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-1,00--0,90   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-0,90--0,80   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-0,80--0,70   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-0,70--0,60   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-0,60--0,50   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-0,50--0,40   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-0,40--0,30   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-0,30--0,20   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-0,20--0,10   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
-0,10-0,00    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0,00-0,10     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0,10-0,20     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0,20-0,30     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0,30-0,40     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0,40-0,50     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0,50-0,60     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0,60-0,70     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0,70-0,80     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0,80-0,90     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0,90-1,00     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
1,00-1,10     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
1,10-1,20     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
1,20-1,30     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
1,30-1,40     XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
1,40-1,50     XXXXXXXXXXXXXXXXXXXXXXXXXX
1,50-1,60     XXXXXXXXXXXXXXXXXXXXXXXXX
1,60-1,70     XXXXXXXXXXXXXXXXXXXXXX
1,70-1,80     XXXXXXXXXXXXXXXXXX
1,80-1,90     XXXXXXXXXXXXXXX
1,90-2,00     XXXXXXXXXXX
2,00-2,10     XXXXXXXXXXXX
2,10-2,20     XXXXXXX
2,20-2,30     XXXXXX
2,30-2,40     XXXXX
2,40-2,50     XXX
2,50-2,60     XXXX
2,60-2,70     XX
2,70-2,80     XX
2,80-2,90     X
2,90-3,00     X
3,00-3,10     X
3,10-3,20     X
3,20-3,30
3,30-3,40
3,40-3,50
```


## zkl

{{trans|Go}}

```zkl
fcn norm2{   // Box-Muller
   const PI2=(0.0).pi*2;;
   rnd:=(0.0).random.fp(1);  // random number in [0,1), using partial application
   r,a:=(-2.0*rnd().log()).sqrt(), PI2*rnd();
   return(r*a.cos(), r*a.sin());  // z0,z1
}
const N=100000, BINS=12, SIG=3, SCALE=500;
var sum=0.0,sumSq=0.0, h=BINS.pump(List(),0);	// (0,0,0,...)
fcn accum(v){
   sum+=v;
   sumSq+=v*v;
   b:=(v + SIG)*BINS/SIG/2;
   if(0<=b<BINS) h[b]+=1;
};
```

Partial application: rnd() --> (0.0).random(1). Basically, the fp method fixes the call parameters, which are then used when the partial thing is run.

```zkl
foreach i in (N/2){ v1,v2:=norm2(); accum(v1); accum(v2); }
println("Samples: %,d".fmt(N));
println("Mean:    ", m:=sum/N);
println("Stddev:  ", (sumSq/N - m*m).sqrt());
foreach p in (h){ println("*"*(p/SCALE)) }
```

{{out}}

```txt

Samples: 100,000
Mean:    0.0005999
Stddev:  1.003
*
***
********
******************
*****************************
**************************************
**************************************
*****************************
******************
********
***
*

```

