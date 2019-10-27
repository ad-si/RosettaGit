+++
title = "Statistics/Basic"
description = ""
date = 2019-05-18T00:39:58Z
aliases = []
[extra]
id = 10013
[taxonomies]
categories = []
tags = []
+++

{{task|Mathematics}}

[[Statistics|Statistics]] is all about large groups of numbers.  
When talking about a set of sampled data, most frequently used is their [[wp:Mean|mean value]] and [[wp:Standard_deviation|standard deviation (stddev)]].  
If you have set of data <math>x_i</math> where <math>i = 1, 2, \ldots, n\,\!</math>, the mean is <math>\bar{x}\equiv {1\over n}\sum_i x_i</math>, while the stddev is <math>\sigma\equiv\sqrt{{1\over n}\sum_i \left(x_i - \bar x \right)^2}</math>.

When examining a large quantity of data, one often uses a [[wp:Histogram|histogram]], which shows the counts of data samples falling into a prechosen set of intervals (or bins).  
When plotted, often as bar graphs, it visually indicates how often each data value occurs.

'''Task''' Using your language's random number routine, generate real numbers in the range of [0, 1].  It doesn't matter if you chose to use open or closed range.  
Create 100 of such numbers (i.e. sample size 100) and calculate their mean and stddev.  
Do so for sample size of 1,000 and 10,000, maybe even higher if you feel like.  
Show a histogram of any of these sets.  
Do you notice some patterns about the standard deviation?

'''Extra''' Sometimes so much data need to be processed that it's impossible to keep all of them at once.  Can you calculate the mean, stddev and histogram of a trillion numbers? (You don't really need to do a trillion numbers, just show how it can be done.)

;Hint:
For a finite population with equal probabilities at all points, one can derive:

:<math>\overline{(x - \overline{x})^2} = \overline{x^2} - \overline{x}^2</math>

Or, more verbosely:

:<math>
\frac{1}{N}\sum_{i=1}^N(x_i-\overline{x})^2 = \frac{1}{N} \left(\sum_{i=1}^N x_i^2\right) - \overline{x}^2.
</math>

{{task heading|See also}}

* [[Statistics/Normal_distribution|Statistics/Normal distribution]]

{{Related tasks/Statistical measures}}


<hr>


## Ada



### A plain solution for moderate sample sizes



```Ada
with Ada.Text_IO, Ada.Command_Line, Ada.Numerics.Float_Random,
  Ada.Numerics.Generic_Elementary_Functions;

procedure Basic_Stat is

   package FRG renames Ada.Numerics.Float_Random;
   package TIO renames Ada.Text_IO;

   type Counter is range 0 .. 2**31-1;
   type Result_Array is array(Natural range <>) of Counter;

   package FIO is new TIO.Float_IO(Float);

   procedure Put_Histogram(R: Result_Array; Scale, Full: Counter) is
   begin
      for I in R'Range loop
         FIO.Put(Float'Max(0.0, Float(I)/10.0 - 0.05),
                 Fore => 1, Aft => 2, Exp => 0);       TIO.Put("..");
         FIO.Put(Float'Min(1.0, Float(I)/10.0 + 0.05),
                 Fore => 1, Aft => 2, Exp => 0);       TIO.Put(": ");
         for J in 1 .. (R(I)* Scale)/Full loop
            Ada.Text_IO.Put("X");
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Put_Histogram;

   procedure Put_Mean_Et_Al(Sample_Size: Counter;
                            Val_Sum, Square_Sum: Float) is
      Mean: constant Float := Val_Sum / Float(Sample_Size);
      package Math is new Ada.Numerics.Generic_Elementary_Functions(Float);
   begin
      TIO.Put("Mean: ");
      FIO.Put(Mean,  Fore => 1, Aft => 5, Exp => 0);
      TIO.Put(",  Standard Deviation: ");
      FIO.Put(Math.Sqrt(abs(Square_Sum / Float(Sample_Size)
                           - (Mean * Mean))), Fore => 1, Aft => 5, Exp => 0);
      TIO.New_Line;
   end Put_Mean_Et_Al;

   N: Counter := Counter'Value(Ada.Command_Line.Argument(1));
   Gen: FRG.Generator;
   Results: Result_Array(0 .. 10) := (others => 0);
   X: Float;
   Val_Sum, Squ_Sum: Float := 0.0;

begin
   FRG.Reset(Gen);
   for I in 1 .. N loop
      X := FRG.Random(Gen);
      Val_Sum   := Val_Sum + X;
      Squ_Sum := Squ_Sum + X*X;
      declare
         Index: Integer := Integer(X*10.0);
      begin
         Results(Index) := Results(Index) + 1;
      end;
   end loop;
   TIO.Put_Line("After sampling" & Counter'Image(N) & " random numnbers: ");
   Put_Histogram(Results, Scale => 600, Full => N);
   TIO.New_Line;
   Put_Mean_Et_Al(Sample_Size => N, Val_Sum => Val_Sum, Square_Sum => Squ_Sum);
end Basic_Stat;
```


{{out}} from a few sample runs: 

```txt
> ./basic_stat 1000
After sampling 1000 random numnbers: 
0.00..0.05: XXXXXXXXXXXXXXXXXXXXXXX
0.05..0.15: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.15..0.25: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.25..0.35: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.35..0.45: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.45..0.55: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.55..0.65: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.65..0.75: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.75..0.85: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.85..0.95: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.95..1.00: XXXXXXXXXXXXXXXXXXXXXXXXXXXX

Mean: 0.48727,  Standard Deviation: 0.28502



> ./basic_stat 10_000
After sampling 10000 random numnbers: 
0.00..0.05: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.05..0.15: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.15..0.25: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.25..0.35: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.35..0.45: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.45..0.55: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.55..0.65: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.65..0.75: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.75..0.85: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.85..0.95: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.95..1.00: XXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Mean: 0.50096,  Standard Deviation: 0.28869



> ./basic_stat 100_000
After sampling 100000 random numnbers: 
0.00..0.05: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.05..0.15: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.15..0.25: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.25..0.35: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.35..0.45: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.45..0.55: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.55..0.65: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.65..0.75: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.75..0.85: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.85..0.95: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.95..1.00: XXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Mean: 0.50178,  Standard Deviation: 0.28805
```



### Making the solution ready for one trillion samples


Depending on where you live, one trillion is either 10^12 or 10^18 [https://en.wikipedia.org/wiki/Trillion].
Below, I'll assume 10^12, which implies a number of operations I can still perform on my PC.

The above program will fail with such large inputs for two reasons:

1. The type Counter cannot hold such large numbers. 

2. The variables Val_Sum and Squ_Sum will numerically fail, because the type Float only provides about six decimal digits of accuracy. I.e., at some point, Val_Sum and (a little bit later) Squ_Sum are so large that adding a value below 1 has no effect, any more. 

To make the program ready for sample size 10^12, we modify it as follows.

1. Change the type Counter to hold such large numbers.

2. Define a type High_Precision, that will hold (at least) 15 decimal digits. Define Val_Sum and Squ_Sum as being from that type. Include the neccessary type conversions. 

3. Provide some progress report, during the running time.

This is the modified program


```Ada
with Ada.Text_IO, Ada.Command_Line, Ada.Numerics.Float_Random,
  Ada.Numerics.Generic_Elementary_Functions;

procedure Long_Basic_Stat is

   package FRG renames Ada.Numerics.Float_Random;
   package TIO renames Ada.Text_IO;

   type Counter is range 0 .. 2**63-1;
   type Result_Array is array(Natural range <>) of Counter;
   type High_Precision is digits 15;

   package FIO is new TIO.Float_IO(Float);

   procedure Put_Histogram(R: Result_Array; Scale, Full: Counter) is
   begin
      for I in R'Range loop
         FIO.Put(Float'Max(0.0, Float(I)/10.0 - 0.05),
                 Fore => 1, Aft => 2, Exp => 0);       TIO.Put("..");
         FIO.Put(Float'Min(1.0, Float(I)/10.0 + 0.05),
                 Fore => 1, Aft => 2, Exp => 0);       TIO.Put(": ");
         for J in 1 .. (R(I)* Scale)/Full loop
            Ada.Text_IO.Put("X");
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Put_Histogram;

   procedure Put_Mean_Et_Al(Sample_Size: Counter;
                            Val_Sum, Square_Sum: Float) is
      Mean: constant Float := Val_Sum / Float(Sample_Size);
      package Math is new Ada.Numerics.Generic_Elementary_Functions(Float);
   begin
      TIO.Put("Mean: ");
      FIO.Put(Mean,  Fore => 1, Aft => 5, Exp => 0);
      TIO.Put(",  Standard Deviation: ");
      FIO.Put(Math.Sqrt(abs(Square_Sum / Float(Sample_Size)
                           - (Mean * Mean))), Fore => 1, Aft => 5, Exp => 0);
      TIO.New_Line;
   end Put_Mean_Et_Al;

   N: Counter := Counter'Value(Ada.Command_Line.Argument(1));
   Gen: FRG.Generator;
   Results: Result_Array(0 .. 10) := (others => 0);
   X: Float;
   Val_Sum, Squ_Sum: High_Precision := 0.0;

begin
   FRG.Reset(Gen);
   for Outer in 1 .. 1000 loop
      for I in 1 .. N/1000 loop
         X := FRG.Random(Gen);
         Val_Sum   := Val_Sum + High_Precision(X);
         Squ_Sum := Squ_Sum + High_Precision(X)*High_Precision(X);
         declare
            Index: Integer := Integer(X*10.0);
         begin
            Results(Index) := Results(Index) + 1;
         end;
      end loop;
      if Outer mod 50 = 0 then
         TIO.New_Line(1);
         TIO.Put_Line(Integer'Image(Outer/10) &"% done; current results:");
         Put_Mean_Et_Al(Sample_Size => (Counter(Outer)*N)/1000,
                        Val_Sum     => Float(Val_Sum),
                        Square_Sum  => Float(Squ_Sum));
      else
         Ada.Text_IO.Put(".");
      end if;
   end loop;
   TIO.New_Line(4);
   TIO.Put_Line("After sampling" & Counter'Image(N) & " random numnbers: ");
   Put_Histogram(Results, Scale => 600, Full => N);
   TIO.New_Line;
   Put_Mean_Et_Al(Sample_Size => N,
                  Val_Sum => Float(Val_Sum), Square_Sum => Float(Squ_Sum));
end Long_Basic_Stat;
```


{{out}} for sample size 10^12 took one night on my PC:

```txt
.................................................
 5% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28867
.................................................
 10% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28867
.................................................
 15% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 20% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 25% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 30% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 35% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 40% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 45% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 50% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 55% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 60% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 65% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 70% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 75% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 80% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 85% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 90% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 95% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868
.................................................
 100% done; current results:
Mean: 0.50000,  Standard Deviation: 0.28868




After sampling 1000000000000 random numnbers: 
0.00..0.05: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.05..0.15: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.15..0.25: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.25..0.35: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.35..0.45: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.45..0.55: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.55..0.65: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.65..0.75: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.75..0.85: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.85..0.95: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
0.95..1.00: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Mean: 0.50000,  Standard Deviation: 0.28868
```


The same program should still work fine for sample size 10^18, but I'll need my PC in the meantime. ;-)


## C

Sample code.

```C>#include <stdio.h

#include <stdlib.h>
#include <math.h>
#include <stdint.h>

#define n_bins 10

double rand01() { return rand() / (RAND_MAX + 1.0); }

double avg(int count, double *stddev, int *hist)
{
	double x[count];
	double m = 0, s = 0;

	for (int i = 0; i < n_bins; i++) hist[i] = 0;
	for (int i = 0; i < count; i++) {
		m += (x[i] = rand01());
		hist[(int)(x[i] * n_bins)] ++;
	}

	m /= count;
	for (int i = 0; i < count; i++)
		s += x[i] * x[i];
	*stddev = sqrt(s / count - m * m);
	return m;
}

void hist_plot(int *hist)
{
	int max = 0, step = 1;
	double inc = 1.0 / n_bins;

	for (int i = 0; i < n_bins; i++)
		if (hist[i] > max) max = hist[i];

	/* scale if numbers are too big */
	if (max >= 60) step = (max + 59) / 60;

	for (int i = 0; i < n_bins; i++) {
		printf("[%5.2g,%5.2g]%5d ", i * inc, (i + 1) * inc, hist[i]);
		for (int j = 0; j < hist[i]; j += step)
			printf("#");
		printf("\n");
	}
}

/*  record for moving average and stddev.  Values kept are sums and sum data^2
 *  to avoid excessive precision loss due to divisions, but some loss is inevitable
 */
typedef struct {
	uint64_t size;
	double sum, x2;
	uint64_t hist[n_bins];
} moving_rec;

void moving_avg(moving_rec *rec, double *data, int count)
{
	double sum = 0, x2 = 0;
	/* not adding data directly to the sum in case both recorded sum and 
	 * count of this batch are large; slightly less likely to lose precision*/
	for (int i = 0; i < count; i++) {
		sum += data[i];
		x2 += data[i] * data[i];
		rec->hist[(int)(data[i] * n_bins)]++;
	}

	rec->sum += sum;
	rec->x2 += x2;
	rec->size += count;
}

int main()
{
	double m, stddev;
	int hist[n_bins], samples = 10;

	while (samples <= 10000) {
		m = avg(samples, &stddev, hist);
		printf("size %5d: %g %g\n", samples, m, stddev);
		samples *= 10;
	}

	printf("\nHistograph:\n");
	hist_plot(hist);

	printf("\nMoving average:\n  N     Mean    Sigma\n");
	moving_rec rec = { 0, 0, 0, {0} };
	double data[100];
	for (int i = 0; i < 10000; i++) {
		for (int j = 0; j < 100; j++) data[j] = rand01();

		moving_avg(&rec, data, 100);

		if ((i % 1000) == 999) {
			printf("%4lluk %f %f\n",
				rec.size/1000,
				rec.sum / rec.size,
				sqrt(rec.x2 * rec.size - rec.sum * rec.sum)/rec.size
			);
		}
	}
}
```


=={{header|C sharp|C#}}==
{{libheader|Math.Net}}

```csharp
using System;
using MathNet.Numerics.Statistics;

class Program
{
    static void Run(int sampleSize)
    {
        double[] X = new double[sampleSize];
        var r = new Random();
        for (int i = 0; i < sampleSize; i++)
            X[i] = r.NextDouble();

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
        Run(100);
        Run(1000);
        Run(10000);
    }
}
```

{{out}}

```txt
Sample size: 100
 0.00 : ##################################################
 0.10 : ############################
 0.20 : ###########################################
 0.30 : ############################
 0.40 : ###########################################
 0.50 : #########################
 0.60 : ##############################################
 0.70 : #########################
 0.80 : #########################
 0.90 : ###########################################
  Mean: 0.481181871658741
StdDev: 0.301957945953801

Sample size: 1,000
 0.00 : ###################################
 0.10 : ###################################
 0.20 : ############################
 0.30 : #################################
 0.40 : #######################################
 0.50 : #########################################
 0.60 : ######################################
 0.70 : #################################
 0.80 : ##################################
 0.90 : ######################################
  Mean: 0.508802390412802
StdDev: 0.28593657047378

Sample size: 10,000
 0.00 : ##################################
 0.10 : #######################################
 0.20 : #################################
 0.30 : ####################################
 0.40 : ###################################
 0.50 : #####################################
 0.60 : ####################################
 0.70 : ###################################
 0.80 : ##################################
 0.90 : ###################################
  Mean: 0.499069400830039
StdDev: 0.287103198996064
```



## C++


```Cpp>#include <iostream

#include <random>
#include <vector>
#include <cstdlib>
#include <algorithm>
#include <cmath>

void printStars ( int number ) {
   if ( number > 0 ) { 
      for ( int i = 0 ; i < number + 1 ; i++ ) 
	 std::cout << '*' ;
   }
   std::cout << '\n' ;
}

int main( int argc , char *argv[] ) {
   const int numberOfRandoms = std::atoi( argv[1] ) ;
   std::random_device rd ;
   std::mt19937 gen( rd( ) ) ;
   std::uniform_real_distribution<> distri( 0.0 , 1.0 ) ;
   std::vector<double> randoms ;
   for ( int i = 0 ; i < numberOfRandoms + 1 ; i++ ) 
      randoms.push_back ( distri( gen ) ) ;
   std::sort ( randoms.begin( ) , randoms.end( ) ) ;
   double start = 0.0 ;
   for ( int i = 0 ;  i < 9 ; i++ ) {
      double to = start + 0.1 ;
      int howmany =  std::count_if ( randoms.begin( ) , randoms.end( ),
	        [&start , &to] ( double c ) { return c >= start 
		  && c < to ; } ) ;
      if ( start == 0.0 ) //double 0.0 output as 0
	 std::cout << "0.0" << " - " << to << ": " ;
      else 
	 std::cout << start << " - " << to << ": " ;
      if ( howmany > 50 ) //scales big interval numbers to printable length
	 howmany = howmany / ( howmany / 50 ) ;
      printStars ( howmany ) ;
      start += 0.1 ;
   }
   double mean = std::accumulate( randoms.begin( ) , randoms.end( ) , 0.0 ) / randoms.size( ) ;
   double sum = 0.0 ;
   for ( double num : randoms ) 
      sum += std::pow( num - mean , 2 ) ;
   double stddev = std::pow( sum / randoms.size( ) , 0.5 ) ;
   std::cout << "The mean is " << mean << " !" << std::endl ;
   std::cout << "Standard deviation is " << stddev << " !" << std::endl ;
   return 0 ;
}
```

{{out}}

```txt
./statistics 100
0.0 - 0.1: **********
0.1 - 0.2: ***************
0.2 - 0.3: **********
0.3 - 0.4: *************
0.4 - 0.5: **********
0.5 - 0.6: *********
0.6 - 0.7: *********
0.7 - 0.8: ************
0.8 - 0.9: *********
The mean is 0.493563 !
Standard deviation is 0.297152 !
```



## CoffeeScript


```coffeescript

generate_statistics = (n) ->
  hist = {}

  update_hist = (r) ->
    hist[Math.floor 10*r] ||= 0
    hist[Math.floor 10*r] += 1

  sum = 0
  sum_squares = 0.0

  for i in [1..n]
    r = Math.random()
    sum += r
    sum_squares += r*r
    update_hist r
  mean = sum / n
  stddev = Math.sqrt((sum_squares / n) - mean*mean)

  [n, mean, stddev, hist]
  
display_statistics = (n, mean, stddev, hist) ->
  console.log "-- Stats for sample size #{n}"
  console.log "mean: #{mean}"
  console.log "sdev: #{stddev}"
  for x, cnt of hist
    bars = repeat "=", Math.floor(cnt*300/n) 
    console.log "#{x/10}: #{bars} #{cnt}"

repeat = (c, n) ->
  s = ''
  s += c for i in [1..n]
  s
 
for n in [100, 1000, 10000, 1000000]
  [n, mean, stddev, hist] = generate_statistics n
  display_statistics n, mean, stddev, hist


```


{{out}}

```txt

> coffee stats.coffee 
-- Stats for sample size 100
mean: 0.5058459933893755
sdev: 0.2752669422150894
0: 
### ============
 6
0.1: 
### =======================================
 15
0.2: 
### =====================
 9
0.3: 
### ===============
 7
0.4: 
### =======================================
 15
0.5: 
### ==================
 8
0.6: 
### ===========================
 11
0.7: 
### ====================================
 14
0.8: 
### ===============
 7
0.9: 
### ==================
 8
-- Stats for sample size 1000
mean: 0.49664502244861797
sdev: 0.2942483939245344
0: 
### ====================
 89
0.1: 
### ===============================
 126
0.2: 
### =====================
 93
0.3: 
### ==============================
 121
0.4: 
### =====================
 93
0.5: 
### ================
 75
0.6: 
### ==========================
 108
0.7: 
### ==================
 82
0.8: 
### ========================
 101
0.9: 
### ===========================
 112
-- Stats for sample size 10000
mean: 0.4985696110446239
sdev: 0.29007446138438986
0: 
### ========================
 1005
0.1: 
### ========================
 1016
0.2: 
### ========================
 1022
0.3: 
### ========================
 1012
0.4: 
### ======================
 958
0.5: 
### =========================
 1035
0.6: 
### =======================
 974
0.7: 
### =======================
 968
0.8: 
### =======================
 973
0.9: 
### =========================
 1037
-- Stats for sample size 1000000
mean: 0.5001718024678293
sdev: 0.2887130780006248
0: 
### ========================
 100113
0.1: 
### =======================
 99830
0.2: 
### ========================
 100029
0.3: 
### =======================
 99732
0.4: 
### =======================
 99911
0.5: 
### =======================
 99722
0.6: 
### ========================
 100780
0.7: 
### =======================
 99812
0.8: 
### =======================
 99875
0.9: 
### ========================
 100196

```



## D

{{trans|Python}}

```d
import std.stdio, std.algorithm, std.array, std.typecons,
       std.range, std.exception;

auto meanStdDev(R)(R numbers) /*nothrow*/ @safe /*@nogc*/ {
    if (numbers.empty)
        return tuple(0.0L, 0.0L);

    real sx = 0.0, sxx = 0.0;
    ulong n;
    foreach (x; numbers) {
        sx += x;
        sxx += x ^^ 2;
        n++;
    }
    return tuple(sx / n, (n * sxx - sx ^^ 2) ^^ 0.5L / n);
}

void showHistogram01(R)(R numbers) /*@safe*/ {
    enum maxWidth = 50; // N. characters.
    ulong[10] bins;
    foreach (immutable x; numbers) {
        immutable index = cast(size_t)(x * bins.length);
        enforce(index >= 0 && index < bins.length);
        bins[index]++;
    }
    immutable real maxFreq = bins.reduce!max;

    foreach (immutable n, immutable i; bins)
        writefln(" %3.1f: %s", n / real(bins.length),
                 replicate("*", cast(int)(i / maxFreq * maxWidth)));
    writeln;
}

version (statistics_basic_main) {
    void main() @safe {
        import std.random;

        foreach (immutable p; 1 .. 7) {
            auto n = iota(10L ^^ p).map!(_ => uniform(0.0L, 1.0L));
            writeln(10L ^^ p, " numbers:");
            writefln(" Mean: %8.6f, SD: %8.6f", n.meanStdDev.tupleof);
            n.showHistogram01;
        }
    }
}
```

Compile with "-version=statistics_basic_main" to run the main function.
{{out}}

```txt
10 numbers:
 Mean: 0.651336, SD: 0.220208
 0.0: *************************
 0.1: **************************************************
 0.2: 
 0.3: **************************************************
 0.4: 
 0.5: *************************
 0.6: *************************
 0.7: *************************
 0.8: *************************
 0.9: *************************

100 numbers:
 Mean: 0.470756, SD: 0.291080
 0.0: *************************************
 0.1: *******************************************
 0.2: *******************************
 0.3: *******************************
 0.4: ******************
 0.5: *********************
 0.6: ****************************
 0.7: **************************************************
 0.8: *******************************
 0.9: ******************

1000 numbers:
 Mean: 0.519127, SD: 0.287775
 0.0: ***************************************
 0.1: *******************************************
 0.2: ****************************************
 0.3: ****************************************
 0.4: ************************************
 0.5: ******************************************
 0.6: **************************************************
 0.7: **************************************
 0.8: ********************************************
 0.9: **********************************

10000 numbers:
 Mean: 0.503266, SD: 0.289198
 0.0: **********************************************
 0.1: **********************************************
 0.2: **************************************************
 0.3: ************************************************
 0.4: ***********************************************
 0.5: *********************************************
 0.6: ***********************************************
 0.7: ************************************************
 0.8: **********************************************
 0.9: **********************************************

100000 numbers:
 Mean: 0.500945, SD: 0.289076
 0.0: *************************************************
 0.1: *************************************************
 0.2: *************************************************
 0.3: *************************************************
 0.4: *************************************************
 0.5: *************************************************
 0.6: *************************************************
 0.7: *************************************************
 0.8: **************************************************
 0.9: *************************************************

1000000 numbers:
 Mean: 0.499970, SD: 0.288635
 0.0: *************************************************
 0.1: *************************************************
 0.2: *************************************************
 0.3: *************************************************
 0.4: *************************************************
 0.5: **************************************************
 0.6: *************************************************
 0.7: *************************************************
 0.8: *************************************************
 0.9: *************************************************
```



## Dart


```d
/* Import math library to get:
 *     	1) Square root function 	        : Math.sqrt(x)
 *	2) Power function 		: Math.pow(base, exponent)
 *	3) Random number generator 	: Math.Random()
 */		
import 'dart:math' as Math show sqrt, pow, Random;

// Returns average/mean of a list of numbers
num mean(List<num> l)  => l.reduce((num value,num element)=>value+element)/l.length;

// Returns standard deviation of a list of numbers
num stdev(List<num> l) => Math.sqrt((1/l.length)*l.map((num x)=>x*x).reduce((num value,num element) => value+element) - Math.pow(mean(l),2));

/* CODE TO PRINT THE HISTOGRAM STARTS HERE
 *
 * 	Histogram has ten fields, one for every tenth between 0 and 1
 * 	To do this, we save the histogram as a global variable
 * 	that will hold the number of occurences of each tenth in the sample
 */
List<num> histogram = new List.filled(10,0);

/*
 * METHOD TO CREATE A RANDOM SAMPLE OF n NUMBERS (Returns a list)
 *
 * 	While creating each value, this method also increments the
 * 	appropriate index of the histogram
 */
List<num> randomsample(num n){
  List<num> l = new List<num>(n);
  histogram = new List.filled(10,0);
  num random = new Math.Random();
  for (int i = 0; i < n; i++){
    l[i] = random.nextDouble();
    histogram[conv(l[i])] += 1; 
  }
  return l;
}

/*
 * METHOD TO RETURN A STRING OF n ASTERIXES (yay ASCII art)
 */
String stars(num n){
  String s = '';
  for (int i = 0; i < n; i++){
    s = s + '*';
  }
  return s;
}

/*
 * METHOD TO DRAW THE HISTOGRAM
 * 1) Get to total for all the values in the histogram
 * 2) For every field in the histogram:
 * 		a) Compute the frequency for every field in the histogram
 * 		b) Print the frequency as asterixes 
 */
void drawhistogram(){
  int total = histogram.reduce((num element,num value)=>element+value);
  double freq;
  for (int i = 0; i < 10; i++){
    freq = histogram[i]/total;
    print('${i/10} - ${(i+1)/10} : ' + stars(conv(30*freq)));
  }
}

/* HELPER METHOD: 
 * 	converts values between 0-1 to integers between 0-9 inclusive
 * 	useful to figure out which random value generated 
 *	corresponds to which field in the histogram
 */
int conv(num i) => (10*i).floor();


/* MAIN FUNCTION 
 *
 * Create 5 histograms and print the mean and standard deviation for each:
 * 	1) Sample Size = 100
 *	2) Sample Size = 1000
 *	3) Sample Size = 10000
 *	4) Sample Size = 100000
 *	5) Sample Size = 1000000
 *  
 */
void main(){
  List<num> l;
  num m;
  num s;
  List<int> sampleSizes = [100,1000,10000,100000,1000000];
  for (int samplesize in sampleSizes){
    print('---------------  Sample size $samplesize   ----------------');
    l = randomsample(samplesize);
    m = mean(l);
    s = stdev(l);
    drawhistogram();
    print('');
    print('mean: ${m.toStringAsPrecision(8)}   standard deviation: ${s.toStringAsPrecision(8)}');
    print('');
  }
}
```

{{out}}

```txt

---------------  Sample size 100   ----------------
0.0 - 0.1 : ******************************
0.1 - 0.2 : ******************************
0.2 - 0.3 : **************************
0.3 - 0.4 : **************************
0.4 - 0.5 : ***************************************
0.5 - 0.6 : *********************************
0.6 - 0.7 : ******************************
0.7 - 0.8 : *********************************
0.8 - 0.9 : ************************
0.9 - 1.0 : **************************

mean: 0.49246975   standard deviation: 0.27789056

---------------  Sample size 1000   ----------------
0.0 - 0.1 : *************************
0.1 - 0.2 : *************************
0.2 - 0.3 : ******************************
0.3 - 0.4 : *******************************
0.4 - 0.5 : *********************************
0.5 - 0.6 : **********************************
0.6 - 0.7 : ********************************
0.7 - 0.8 : ****************************
0.8 - 0.9 : ****************************
0.9 - 1.0 : *******************************

mean: 0.51170283   standard deviation: 0.28170178

--------------   Sample size 10000   ----------------
0.0 - 0.1 : *****************************
0.1 - 0.2 : ******************************
0.2 - 0.3 : ****************************
0.3 - 0.4 : *****************************
0.4 - 0.5 : *****************************
0.5 - 0.6 : ******************************
0.6 - 0.7 : *******************************
0.7 - 0.8 : ******************************
0.8 - 0.9 : ******************************
0.9 - 1.0 : *******************************

mean: 0.50517609  standard deviation: 0.28923152

--------------   Sample size 100000  ----------------
0.0 - 0.1 : ******************************
0.1 - 0.2 : ******************************
0.2 - 0.3 : *****************************
0.3 - 0.4 : *****************************
0.4 - 0.5 : *****************************
0.5 - 0.6 : ******************************
0.6 - 0.7 : ******************************
0.7 - 0.8 : *****************************
0.8 - 0.9 : *****************************
0.9 - 1.0 : ******************************

mean: 0.49994544   standard deviation: 0.28879394

--------------  Sample size 1000000  ----------------
0.0 - 0.1 : *****************************
0.1 - 0.2 : ******************************
0.2 - 0.3 : *****************************
0.3 - 0.4 : *****************************
0.4 - 0.5 : ******************************
0.5 - 0.6 : *****************************
0.6 - 0.7 : ******************************
0.7 - 0.8 : *****************************
0.8 - 0.9 : *****************************
0.9 - 1.0 : ******************************

mean: 0.50013331   standard deviation: 0.28864180


```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Statistics do
  def basic(n) do
    {sum, sum2, hist} = generate(n)
    mean = sum / n
    stddev = :math.sqrt(sum2 / n - mean*mean)
    
    IO.puts "size:   #{n}"
    IO.puts "mean:   #{mean}"
    IO.puts "stddev: #{stddev}"
    Enum.each(0..9, fn i ->
      :io.fwrite "~.1f:~s~n", [0.1*i, String.duplicate("=", trunc(500 * hist[i] / n))]
    end)
    IO.puts ""
  end
  
  defp generate(n) do
    hist = for i <- 0..9, into: %{}, do: {i,0}
    Enum.reduce(1..n, {0, 0, hist}, fn _,{sum, sum2, h} ->
      r = :rand.uniform
      {sum+r, sum2+r*r, Map.update!(h, trunc(10*r), &(&1+1))}
    end)
  end
end

Enum.each([100,1000,10000], fn n ->
  Statistics.basic(n)
end)
```


{{out}}

```txt

size:   100
mean:   0.5360891830207845
stddev: 0.2934821336243825
0.0:
### =================================================

0.1:
### ===================

0.2:
### ======================================================

0.3:
### =======================================

0.4:
### ========================

0.5:
### ==================================

0.6:
### =====================================================================

0.7:
### =================================================

0.8:
### =================================================

0.9:
### ======================================================


size:   1000
mean:   0.4928249370693845
stddev: 0.2877164661860377
0.0:
### ===================================================

0.1:
### ========================================

0.2:
### ==========================================

0.3:
### ==============================================

0.4:
### ==========================================

0.5:
### ================================================

0.6:
### ==========================================

0.7:
### ============================================

0.8:
### =============================================

0.9:
### =====================================


size:   10000
mean:   0.4969580860984137
stddev: 0.289282008094715
0.0:
### ============================================

0.1:
### ==============================================

0.2:
### ==========================================

0.3:
### ===========================================

0.4:
### ==========================================

0.5:
### =============================================

0.6:
### ============================================

0.7:
### ==========================================

0.8:
### ===========================================

0.9:
### ===========================================


```



## Factor


```factor
USING: assocs formatting grouping io kernel literals math
math.functions math.order math.statistics prettyprint random
sequences sequences.deep sequences.repeating ;
IN: rosetta-code.statistics-basic

CONSTANT: granularity
    $[ 11 iota [ 10 /f ] map 2 clump ]

: mean/std ( seq -- a b )
    [ mean ] [ population-std ] bi ;

: .mean/std ( seq -- )
    mean/std [ "Mean: " write . ] [ "STD:  " write . ] bi* ;
    
: count-between ( seq a b -- n )
    [ between? ] 2curry count ;
    
: histo ( seq -- seq )
    granularity [ first2 count-between ] with map ;
    
: bar ( n -- str )
    [ dup 50 < ] [ 10 / ] until 2 * >integer "*" swap repeat ;
    
: (.histo) ( seq -- seq' )
    [ bar ] map granularity swap zip flatten 3 group ;
    
: .histo ( seq -- )
    (.histo) [ "%.1f - %.1f %s\n" vprintf ] each ;
    
: stats ( n -- )
    dup "Statistics %d:\n" printf
    random-units [ histo .histo ] [ .mean/std nl ] bi ;
    
: main ( -- )
    { 100 1,000 10,000 } [ stats ] each ;
    
MAIN: main
```

{{out}}

```txt

Statistics 100:
0.0 - 0.1 ************************
0.1 - 0.2 **************
0.2 - 0.3 **********************
0.3 - 0.4 ********************
0.4 - 0.5 ******
0.5 - 0.6 ****************************
0.6 - 0.7 **********************
0.7 - 0.8 **********************
0.8 - 0.9 ************
0.9 - 1.0 ******************************
Mean: 0.5125865184454739
STD:  0.3011535351273979

Statistics 1000:
0.0 - 0.1 ******************
0.1 - 0.2 **************************
0.2 - 0.3 ********************
0.3 - 0.4 ********************
0.4 - 0.5 ********************
0.5 - 0.6 *********************
0.6 - 0.7 *****************
0.7 - 0.8 ******************
0.8 - 0.9 ******************
0.9 - 1.0 ******************
Mean: 0.4822182628505952
STD:  0.2874411306988986

Statistics 10000:
0.0 - 0.1 *******************
0.1 - 0.2 ********************
0.2 - 0.3 *******************
0.3 - 0.4 *******************
0.4 - 0.5 ********************
0.5 - 0.6 *******************
0.6 - 0.7 *******************
0.7 - 0.8 ********************
0.8 - 0.9 ********************
0.9 - 1.0 ********************
Mean: 0.5030027112958179
STD:  0.2895932850375331

```



## Fortran

{{works with|Fortran|95 and later}}
This version will handle numbers as large as 1 trillion or more if you are prepared to wait long enough

```fortran
program basic_stats
  implicit none
  
  integer, parameter :: i64 = selected_int_kind(18)
  integer, parameter :: r64 = selected_real_kind(15)
  integer(i64), parameter :: samples = 1000000000_i64
     
  real(r64) :: r
  real(r64) :: mean, stddev
  real(r64) :: sumn = 0, sumnsq = 0
  integer(i64) :: n = 0 
  integer(i64) :: bin(10) = 0
  integer :: i, ind
  
  call random_seed

  n = 0
  do while(n <= samples)
    call random_number(r)
    ind = r * 10 + 1
    bin(ind) = bin(ind) + 1_i64
    sumn = sumn + r
    sumnsq = sumnsq + r*r
    n = n + 1_i64
  end do

  mean = sumn / n
  stddev = sqrt(sumnsq/n - mean*mean)
  write(*, "(a, i0)") "sample size = ", samples
  write(*, "(a, f17.15)") "Mean :   ", mean,
  write(*, "(a, f17.15)") "Stddev : ", stddev  
  do i = 1, 10
    write(*, "(f3.1, a, a)") real(i)/10.0, ": ", repeat("=", int(bin(i)*500/samples))
  end do
 
end program
```

{{out}}

```txt

sample size = 100
Mean :   0.507952672404959
Stddev : 0.290452178516586
0.1: 
### =======================================

0.2: 
### ======================================================

0.3: 
### ========================

0.4: 
### ===========================================================

0.5: 
### =======================================

0.6: 
### =================================================

0.7: 
### ===========================================================

0.8: 
### ============================================

0.9: 
### ===================

1.0: 
### ===========================================================


sample size = 1000
Mean :   0.505018948813265
Stddev : 0.287904987339785
0.1: 
### ========================================

0.2: 
### ==========================================

0.3: 
### ==================================================

0.4: 
### =========================================

0.5: 
### ============================================

0.6: 
### =====================================

0.7: 
### ==================================================

0.8: 
### ============================================

0.9: 
### =============================================

1.0: 
### =============================================


sample size = 10000
Mean :   0.508929669066967
Stddev : 0.287243609812712
0.1: 
### ========================================

0.2: 
### ==========================================

0.3: 
### ===========================================

0.4: 
### ============================================

0.5: 
### ==========================================

0.6: 
### =============================================

0.7: 
### ============================================

0.8: 
### ============================================

0.9: 
### ==============================================

1.0: 
### =============================================


sample size = 1000000000
Mean :   0.500005969962249
Stddev : 0.288673875345505
0.1: 
### ===========================================

0.2: 
### ===========================================

0.3: 
### ===========================================

0.4: 
### ===========================================

0.5: 
### ============================================

0.6: 
### ===========================================

0.7: 
### ============================================

0.8: 
### ===========================================

0.9: 
### ============================================

1.0: 
### ===========================================


```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Randomize

Sub basicStats(sampleSize As Integer)
  If sampleSize < 1 Then Return 
  Dim r(1 To sampleSize) As Double
  Dim h(0 To 9) As Integer '' all zero by default
  Dim sum As Double = 0.0
  Dim hSum As Integer = 0

  ' Generate 'sampleSize' random numbers in the interval [0, 1)
  ' calculate their sum
  ' and in which box they will fall when drawing the histogram
  For i As Integer = 1 To sampleSize
    r(i) = Rnd
    sum += r(i)
    h(Int(r(i) * 10)) += 1
  Next

  For i As Integer = 0 To 9 : hSum += h(i) :  Next
  ' adjust one of the h() values if necessary to ensure hSum = sampleSize
  Dim adj As Integer = sampleSize - hSum
  If adj <> 0 Then
    For i As Integer = 0 To 9 
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
  ' If sample size > 500 then normalize histogram to 500
  Dim scale As Double = 1.0
  If sampleSize > 500 Then scale = 500.0 / sampleSize 
  Print "Sample size "; sampleSize
  Print
  Print Using "  Mean #.######"; mean;
  Print Using "  SD #.######"; sd
  Print
  For i As Integer = 0 To 9
    Print Using "  #.## : "; i/10.0;
    Print Using "##### " ; h(i);
    numStars = Int(h(i) * scale + 0.5)
    Print String(numStars, "*")
  Next 
End Sub
    
basicStats 100
Print
basicStats 1000
Print
basicStats 10000
Print
basicStats 100000 
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Sample size  100

  Mean 0.485580  SD 0.269003

  0.00 :     7 *******
  0.10 :    10 **********
  0.20 :    12 ************
  0.30 :    17 *****************
  0.40 :     8 ********
  0.50 :    10 **********
  0.60 :    11 ***********
  0.70 :     9 *********
  0.80 :     9 *********
  0.90 :     7 *******

Sample size  1000

  Mean 0.504629  SD 0.292029

  0.00 :    99 **************************************************
  0.10 :    99 **************************************************
  0.20 :    93 ***********************************************
  0.30 :   108 ******************************************************
  0.40 :   101 ***************************************************
  0.50 :    97 *************************************************
  0.60 :    90 *********************************************
  0.70 :   110 *******************************************************
  0.80 :   102 ***************************************************
  0.90 :   101 ***************************************************

Sample size  10000

  Mean 0.500027  SD 0.290618

  0.00 :  1039 ****************************************************
  0.10 :   997 **************************************************
  0.20 :   978 *************************************************
  0.30 :   988 *************************************************
  0.40 :   998 **************************************************
  0.50 :   959 ************************************************
  0.60 :  1037 ****************************************************
  0.70 :  1004 **************************************************
  0.80 :   965 ************************************************
  0.90 :  1035 ****************************************************

Sample size  100000

  Mean 0.499503  SD 0.288730

  0.00 : 10194 ***************************************************
  0.10 :  9895 *************************************************
  0.20 :  9875 *************************************************
  0.30 :  9922 **************************************************
  0.40 : 10202 ***************************************************
  0.50 :  9981 **************************************************
  0.60 : 10034 **************************************************
  0.70 : 10012 **************************************************
  0.80 :  9957 **************************************************
  0.90 :  9928 **************************************************

```



## Go


```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "strings"
)

func main() {
    sample(100)
    sample(1000)
    sample(10000)
}

func sample(n int) {
    // generate data
    d := make([]float64, n)
    for i := range d {
        d[i] = rand.Float64()
    }
    // show mean, standard deviation
    var sum, ssq float64
    for _, s := range d {
        sum += s
        ssq += s * s
    }
    fmt.Println(n, "numbers")
    m := sum / float64(n)
    fmt.Println("Mean:  ", m)
    fmt.Println("Stddev:", math.Sqrt(ssq/float64(n)-m*m))
    // show histogram
    h := make([]int, 10)
    for _, s := range d {
        h[int(s*10)]++
    }
    for _, c := range h {
        fmt.Println(strings.Repeat("*", c*205/int(n)))
    }
    fmt.Println()
}
```

{{out}}

```txt

100 numbers
Mean:   0.5231064889267764
Stddev: 0.292668237816841
****************
****************
************************
**********************
******************
******************
****************
**************************
************************
********************

1000 numbers
Mean:   0.496026080160094
Stddev: 0.2880988956436907
*********************
********************
*****************
***********************
******************
**********************
********************
*********************
******************
*******************

10000 numbers
Mean:   0.5009091903581223
Stddev: 0.289269693719711
*******************
********************
********************
********************
*********************
********************
*******************
*******************
********************
*********************

```

The usual approach to the extra problem is [http://en.wikipedia.org/wiki/Sampling_%28statistics%29 sampling.]  That is, to not do it.

To show really show how computations could be done a trillion numbers however, here is an outline of a map reduce strategy.  The main task indicated that numbers should be generated before doing any computations on them.  Consistent with that, The function getSegment returns data based on a starting and ending index, as if it were accessing some large data store.

The following runs comfortably on a simulated data size of 10 million.  To scale to a trillion, and to use real data, you would want to use a technique like [[Distributed_programming#Go]] to distribute work across multiple computers, and on each computer, use a technique like [[Parallel_calculations#Go]] to distribute work across multiple cores within each computer.  You would tune parameters like the constant <tt>threshold</tt> in the code below to optimize cache performance.

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "strings"
)

func main() {
    bigSample(1e7)
}

func bigSample(n int64) {
    sum, ssq, h := reduce(0, n)
    // compute final statistics and output as above
    fmt.Println(n, "numbers")
    m := sum / float64(n)
    fmt.Println("Mean:  ", m)
    fmt.Println("Stddev:", math.Sqrt(ssq/float64(n)-m*m))
    for _, c := range h {
        fmt.Println(strings.Repeat("*", c*205/int(n)))
    }
    fmt.Println()
}

const threshold = 1e6

func reduce(start, end int64) (sum, ssq float64, h []int) {
    n := end - start
    if n < threshold {
        d := getSegment(start, end)
        return computeSegment(d)
    }
    // map to two sub problems
    half := (start + end) / 2
    sum1, ssq1, h1 := reduce(start, half)
    sum2, ssq2, h2 := reduce(half, end)
    // combine results
    for i, c := range h2 {
        h1[i] += c
    }
    return sum1 + sum2, ssq1 + ssq2, h1
}

func getSegment(start, end int64) []float64 {
    d := make([]float64, end-start)
    for i := range d {
        d[i] = rand.Float64()
    }
    return d
}

func computeSegment(d []float64) (sum, ssq float64, h []int) {
    for _, s := range d {
        sum += s
        ssq += s * s
    }
    h = make([]int, 10)
    for _, s := range d {
        h[int(s*10)]++
    }
    return
}
```

{{out}}

```txt

10000000 numbers
Mean:   0.4999673191148989
Stddev: 0.2886663876567514
********************
********************
********************
********************
********************
********************
********************
********************
********************
********************

```



## Haskell


```Haskell
import Data.Foldable (foldl') --'
import System.Random (randomRs, newStdGen)
import Control.Monad (zipWithM_)
import System.Environment (getArgs)

intervals :: [(Double, Double)]
intervals = map conv [0 .. 9]
  where
    xs = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
    conv s =
      let [h, l] = take 2 $ drop s xs
      in (h, l)

count :: [Double] -> [Int]
count rands = map (\iv -> foldl'' (loop iv) 0 rands) intervals
  where
    loop :: (Double, Double) -> Int -> Double -> Int
    loop (lo, hi) n x
      | lo <= x && x < hi = n + 1
      | otherwise = n

-- ^ fuses length and filter within (lo,hi)
data Pair a b =
  Pair !a
       !b

-- accumulate sum and length in one fold
sumLen :: [Double] -> Pair Double Double
sumLen = fion2 . foldl'' (\(Pair s l) x -> Pair (s + x) (l + 1)) (Pair 0.0 0)
  where
    fion2 :: Pair Double Int -> Pair Double Double
    fion2 (Pair s l) = Pair s (fromIntegral l)

-- safe division on pairs
divl :: Pair Double Double -> Double
divl (Pair _ 0.0) = 0.0
divl (Pair s l) = s / l

-- sumLen and divl are separate for stddev below
mean :: [Double] -> Double
mean = divl . sumLen

stddev :: [Double] -> Double
stddev xs = sqrt $ foldl'' (\s x -> s + (x - m) ^ 2) 0 xs / l
  where
    p@(Pair s l) = sumLen xs
    m = divl p

main = do
  nr <- read . head <$> getArgs
  -- or in code, e.g.  let nr = 1000
  rands <- take nr . randomRs (0.0, 1.0) <$> newStdGen
  putStrLn $ "The mean is " ++ show (mean rands) ++ " !"
  putStrLn $ "The standard deviation is " ++ show (stddev rands) ++ " !"
  zipWithM_
    (\iv fq -> putStrLn $ ivstr iv ++ ": " ++ fqstr fq)
    intervals
    (count rands)
  where
    fqstr i =
      replicate
        (if i > 50
           then div i (div i 50)
           else i)
        '*'
    ivstr (lo, hi) = show lo ++ " - " ++ show hi

-- To avoid Wiki formatting issue
foldl'' = foldl'
```

{{out}}

```txt
./Statistics 100
The mean is 0.5007604927009823 !
The standard deviation is 0.2933668702954616 !
0.0 - 0.1: ********
0.1 - 0.2: ************
0.2 - 0.3: ***********
0.3 - 0.4: *************
0.4 - 0.5: *****
0.5 - 0.6: ************
0.6 - 0.7: *********
0.7 - 0.8: ********
0.8 - 0.9: *********
0.9 - 1.0: *************
./Statistics 10000
The mean is 0.49399049116152155 !
The standard deviation is 0.28782134281196275 !
0.0 - 0.1: **************************************************
0.1 - 0.2: **************************************************
0.2 - 0.3: ***************************************************
0.3 - 0.4: **************************************************
0.4 - 0.5: **************************************************
0.5 - 0.6: ***************************************************
0.6 - 0.7: ***************************************************
0.7 - 0.8: ***************************************************
0.8 - 0.9: ****************************************************
0.9 - 1.0: ***************************************************

```



## Hy



```lisp
(import
  [numpy.random [random]]
  [numpy [mean std]]
  [matplotlib.pyplot :as plt])

(for [n [100 1000 10000]]
  (setv v (random n))
  (print "Mean:" (mean v) "SD:" (std v)))

(plt.hist (random 1000))
(plt.show)
```


=={{header|Icon}} and {{header|Unicon}}==

The following uses the ''stddev'' procedure from the [[Standard_deviation]] task.
In this example, 


```Icon
procedure main(A) 

W := 50                         # avg width for histogram bar
B := 10                         # histogram bins
if *A = 0 then put(A,100)       # 100 if none specified 

while N := get(A) do {          # once per argument
   write("\nN=",N)

   N := 0 < integer(N) | next   # skip if invalid 
   
   stddev() # reset
   m := 0.
   H := list(B,0)               # Histogram of 
   every i := 1 to N do {       # calc running ...
      s := stddev(r := ?0)      # ... std dev 
      m +:= r/N                 # ... mean
      H[integer(*H*r)+1] +:= 1  # ... histogram
      }

   write("mean=",m)
   write("stddev=",s)
   every i := 1 to *H do        # show histogram 
      write(right(real(i)/*H,5)," : ",repl("*",integer(*H*50./N*H[i]))) 
   }
end
```


{{out}}

```txt

N=100
mean=0.4941076275054806
stddev=0.2812938788216594
  0.1 : ****************************************
  0.2 : *******************************************************
  0.3 : *******************************************************
  0.4 : **********************************************************************
  0.5 : ****************************************
  0.6 : *********************************************
  0.7 : ****************************************
  0.8 : *****************************************************************
  0.9 : ****************************************
  1.0 : **************************************************

N=10000
mean=0.4935428224375008
stddev=0.2884171825227816
  0.1 : ***************************************************
  0.2 : ***************************************************
  0.3 : ***************************************************
  0.4 : **************************************************
  0.5 : ****************************************************
  0.6 : *************************************************
  0.7 : ***********************************************
  0.8 : ************************************************
  0.9 : **************************************************
  1.0 : ***********************************************

N=1000000
mean=0.4997503773607869
stddev=0.2886322440610256
  0.1 : *************************************************
  0.2 : **************************************************
  0.3 : **************************************************
  0.4 : **************************************************
  0.5 : *************************************************
  0.6 : **************************************************
  0.7 : *************************************************
  0.8 : *************************************************
  0.9 : **************************************************
  1.0 : *************************************************
```



## J


J has library routines to compute mean and standard deviation:

```j
   require 'stats'
   (mean,stddev) 1000 ?@$ 0
0.484669 0.287482
   (mean,stddev) 10000 ?@$ 0
0.503642 0.290777
   (mean,stddev) 100000 ?@$ 0
0.499677 0.288726
```


And, for a histogram:


```j
histogram=: <: @ (#/.~) @ (i.@#@[ , I.)
require'plot'
plot ((% * 1 + i.)100) ([;histogram) 10000 ?@$ 0
```


but these are not quite what is being asked for here.

Instead:


```j
histogram=: <: @ (#/.~) @ (i.@#@[ , I.)

meanstddevP=: 3 :0
  NB. compute mean and std dev of y random numbers 
  NB. picked from even distribution between 0 and 1
  NB. and display a normalized ascii histogram for this sample
  NB. note: uses population mean (0.5), not sample mean, for stddev
  NB.       given the equation specified for this task.
  h=.s=.t=. 0
  chunk=. 1e6
  bins=. (%~ 1 + i.) 10
  for. i. <.y%chunk do.
    data=. chunk ?@$ 0
    h=. h+ bins histogram data
    s=. s+ +/ data
    t=. t+ +/ *: data-0.5
  end.
  data=. (chunk|y) ?@$ 0
  h=. h+ bins histogram data
  s=. s+ +/ data
  t=. t+ +/ *: data - 0.5
  smoutput (<.300*h%y) #"0 '#'
  (s%y) , %:t%y
)
```


Example use:


```j
   meanstddevP 1000
#############################       
####################################
###########################         
##############################      
################################### 
########################            
###########################         
############################        
################################    
##########################          
0.488441 0.289744
   meanstddevP 10000
############################## 
############################## 
#############################  
#############################  
###############################
############################## 
############################   
############################## 
#############################  
#############################  
0.49697 0.289433
   meanstddevP 100000
############################# 
##############################
############################# 
############################# 
############################# 
##############################
##############################
##############################
##############################
############################# 
0.500872 0.288241
```


(That said, note that these numbers are random, so reported standard deviation will vary with the random sample being tested.)

This could handle a trillion random numbers on a bog-standard computer, but I am not inclined to wait that long.


## Java

Translation of [[Statistics/Basic#Python|Python]] via [[Statistics/Basic#D|D]]
{{works with|Java|8}}

```java
import static java.lang.Math.pow;
import static java.util.Arrays.stream;
import static java.util.stream.Collectors.joining;
import static java.util.stream.IntStream.range;

public class Test {
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

    public static void main(String[] a) {
        Locale.setDefault(Locale.US);
        for (int p = 1; p < 7; p++) {
            double[] n = range(0, (int) pow(10, p))
                    .mapToDouble(i -> Math.random()).toArray();

            System.out.println((int)pow(10, p) + " numbers:");
            double[] res = meanStdDev(n);
            System.out.printf(" Mean: %8.6f, SD: %8.6f%n", res[0], res[1]);
            showHistogram01(n);
        }
    }
}
```


```txt
10 numbers:
 Mean: 0.564409, SD: 0.249601
 0.0: *
 0.1: *****************
 0.2: *****************
 0.3: *****************
 0.4: *****************
 0.5: *****************
 0.6: *
 0.7: ***************************************************
 0.8: **********************************
 0.9: *

100 numbers:
 Mean: 0.487440, SD: 0.283866
 0.0: ************************************
 0.1: ************************************
 0.2: **********************
 0.3: ***************************************************
 0.4: ***************************************************
 0.5: *****************************
 0.6: ************************************
 0.7: ************************************
 0.8: ************************************
 0.9: *****************************

1000 numbers:
 Mean: 0.500521, SD: 0.285790
 0.0: **********************************************
 0.1: ********************************************
 0.2: ******************************************
 0.3: ****************************************
 0.4: **************************************************
 0.5: ***************************************************
 0.6: ************************************************
 0.7: ************************************************
 0.8: ****************************************
 0.9: *******************************************

10000 numbers:
 Mean: 0.499363, SD: 0.288427
 0.0: *************************************************
 0.1: *************************************************
 0.2: ************************************************
 0.3: *************************************************
 0.4: ***************************************************
 0.5: ************************************************
 0.6: ***************************************************
 0.7: ************************************************
 0.8: ************************************************
 0.9: ************************************************

100000 numbers:
 Mean: 0.500154, SD: 0.287981
 0.0: *************************************************
 0.1: **************************************************
 0.2: **************************************************
 0.3: **************************************************
 0.4: **************************************************
 0.5: ***************************************************
 0.6: **************************************************
 0.7: **************************************************
 0.8: *************************************************
 0.9: **************************************************

1000000 numbers:
 Mean: 0.500189, SD: 0.288560
 0.0: **************************************************
 0.1: **************************************************
 0.2: **************************************************
 0.3: ***************************************************
 0.4: **************************************************
 0.5: **************************************************
 0.6: **************************************************
 0.7: **************************************************
 0.8: **************************************************
 0.9: **************************************************
```



## Jsish


```javascript
#!/usr/bin/env jsish
"use strict";

function statisticsBasic(args:array|string=void, conf:object=void) {
    var options = { // Rosetta Code, Statistics/Basic
        rootdir      :'',      // Root directory.
        samples      : 0       // Set sample size from options
    };
    var self = { };
    parseOpts(self, options, conf);

    function generateStats(n:number):object {
        var i, sum = 0, sum2 = 0;
        var hist = new Array(10);
        hist.fill(0);
        for (i = 0; i < n; i++) {
            var r = Math.random();
            sum += r;
            sum2 += r*r;
            hist[Math.floor((r*10))] += 1;
        }
        var mean = sum/n;
        var stddev = Math.sqrt((sum2 / n) - mean*mean);
        var obj = {n:n, sum:sum, mean:mean, stddev:stddev};
        return {n:n, sum:sum, mean:mean, stddev:stddev, hist:hist};
    }

    function reportStats(summary:object):void {
        printf("Samples: %d, mean: %f, stddev: %f\n", summary.n, summary.mean, summary.stddev);
        var max = Math.max.apply(summary, summary.hist);
        for (var i = 0; i < 10; i++) {
            printf("%3.1f+ %-70s %5d\n", i * 0.1, 'X'.repeat(70 * summary.hist[i] / max), summary.hist[i]);
        }
        return;
    }
    
    function main() {
        LogTest('Starting', args);
        switch (typeof(args)) {
            case 'string': args = [args]; break;
            case 'array': break;
            default: args = [];
        }
        if (self.rootdir === '')
            self.rootdir=Info.scriptDir();

        Math.srand(0);
        if (self.samples > 0) reportStats(generateStats(self.samples));
        else if (args[0] && parseInt(args[0])) reportStats(generateStats(parseInt(args[0])));  
        else for (var n of [100, 1000, 10000]) reportStats(generateStats(n));

        debugger;
        LogDebug('Done');
        return 0;
    }
    
    return main();
}

provide(statisticsBasic, 1);

if (isMain()) {
    if (!Interp.conf('unitTest'))
        return runModule(statisticsBasic);
    
;'  statisticsBasic unit-test';
;   statisticsBasic();

}


/*
=!EXPECTSTART!=
'  statisticsBasic unit-test'
statisticsBasic() ==> Samples: 100, mean: 0.534517, stddev: 0.287124
0.0+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        8
0.1+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                          11
0.2+ XXXXXXXXXXXXXXXXXXXXXXXXXX                                                 6
0.3+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                               10
0.4+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                               10
0.5+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                          11
0.6+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        8
0.7+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX    16
0.8+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                             7
0.9+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                  13
Samples: 1000, mean: 0.490335, stddev: 0.286562
0.0+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                  98
0.1+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   122
0.2+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                          85
0.3+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX             106
0.4+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX             105
0.5+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                101
0.6+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                     93
0.7+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX             106
0.8+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                  98
0.9+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                         86
Samples: 10000, mean: 0.499492, stddev: 0.287689
0.0+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX          969
0.1+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        992
0.2+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  1067
0.3+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX      1011
0.4+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX          973
0.5+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX     1031
0.6+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX          971
0.7+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        999
0.8+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        991
0.9+ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        996
0
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u statisticsBasic.jsi
[PASS] statisticsBasic.jsi
```



## Julia

{{works with|Julia|0.6}}


```julia
function hist(numbers)
    maxwidth = 50
    h = fill(0, 10)
    for n in numbers
        h[ceil(Int, 10n)] += 1
    end
    mx = maximum(h)
    for (n, i) in enumerate(h)
        @printf("%3.1f: %s\n", n / 10, "+" ^ floor(Int, i / mx * maxwidth))
    end
end

for i in 1:6
    n = rand(10 ^ i)
    println("\n##\n## $(10 ^ i) numbers")
    @printf("μ: %8.6f; σ: %8.6f\n", mean(n), std(n))
    hist(n)
end
```


{{out}}

```txt

##
## 10 numbers
μ: 0.513345; σ: 0.261532
0.1: 
0.2: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.3: 
0.4: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.5: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.6: 
0.7: +++++++++++++++++++++++++
0.8: +++++++++++++++++++++++++
0.9: ++++++++++++++++++++++++++++++++++++++++++++++++++
1.0: 

##
## 100 numbers
μ: 0.483039; σ: 0.289858
0.1: ++++++++++++++++++++++++++++++++++++++++++
0.2: ++++++++++++++++++++++++++++++++++++++++++
0.3: ++++++++++++++++++++++++++++++++++++++++++
0.4: ++++++++++++++++++++++++++++++
0.5: ++++++++++++++++++++++++++++++++++++++++++++++
0.6: ++++++++++++++++++++++++++++++
0.7: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.8: +++++++++++++++++++
0.9: ++++++++++++++++++++++++++++++++++++++++++++++
1.0: ++++++++++++++++++++++++++++++++++

##
## 1000 numbers
μ: 0.482115; σ: 0.288932
0.1: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.2: ++++++++++++++++++++++++++++++++++++++++
0.3: ++++++++++++++++++++++++++++++++++++++++
0.4: ++++++++++++++++++++++++++++++++++++++++++
0.5: ++++++++++++++++++++++++++++++++++++
0.6: ++++++++++++++++++++++++++++++++++++++++++++++++
0.7: +++++++++++++++++++++++++++++++++++++++
0.8: ++++++++++++++++++++++++++++++++++++++
0.9: ++++++++++++++++++++++++++++++++++++++++
1.0: +++++++++++++++++++++++++++++++++++

##
## 10000 numbers
μ: 0.502500; σ: 0.288759
0.1: ++++++++++++++++++++++++++++++++++++++++++++++++
0.2: ++++++++++++++++++++++++++++++++++++++++++++++
0.3: ++++++++++++++++++++++++++++++++++++++++++++++
0.4: +++++++++++++++++++++++++++++++++++++++++++++++++
0.5: +++++++++++++++++++++++++++++++++++++++++++++++
0.6: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.7: +++++++++++++++++++++++++++++++++++++++++++++++
0.8: ++++++++++++++++++++++++++++++++++++++++++++++++
0.9: ++++++++++++++++++++++++++++++++++++++++++++++++
1.0: +++++++++++++++++++++++++++++++++++++++++++++++++

##
## 100000 numbers
μ: 0.499489; σ: 0.288911
0.1: +++++++++++++++++++++++++++++++++++++++++++++++++
0.2: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.3: ++++++++++++++++++++++++++++++++++++++++++++++++
0.4: ++++++++++++++++++++++++++++++++++++++++++++++++
0.5: ++++++++++++++++++++++++++++++++++++++++++++++++
0.6: +++++++++++++++++++++++++++++++++++++++++++++++++
0.7: ++++++++++++++++++++++++++++++++++++++++++++++++
0.8: +++++++++++++++++++++++++++++++++++++++++++++++++
0.9: +++++++++++++++++++++++++++++++++++++++++++++++++
1.0: ++++++++++++++++++++++++++++++++++++++++++++++++

##
## 1000000 numbers
μ: 0.500268; σ: 0.288622
0.1: +++++++++++++++++++++++++++++++++++++++++++++++++
0.2: +++++++++++++++++++++++++++++++++++++++++++++++++
0.3: +++++++++++++++++++++++++++++++++++++++++++++++++
0.4: +++++++++++++++++++++++++++++++++++++++++++++++++
0.5: +++++++++++++++++++++++++++++++++++++++++++++++++
0.6: +++++++++++++++++++++++++++++++++++++++++++++++++
0.7: +++++++++++++++++++++++++++++++++++++++++++++++++
0.8: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.9: +++++++++++++++++++++++++++++++++++++++++++++++++
1.0: +++++++++++++++++++++++++++++++++++++++++++++++++
```



## Klong

Using the "mu" (mean) and "sd" (standard deviation) functions from the
Klong statistics library:

```K

.l("nstat.kg")
bar::{x{x;.d("*")}:*0;.p("")}
hist10::{[s];#'=s@<s::_x*10}
plot::{[s];.p("");.p("n = ",$x);
       (!10){.d(x%10);.d(" ");bar(y)}'_(100%x)*(hist10(s::{x;.rn()}'!x));
       .p("mean = ",$mu(s));.p("sd   = ",$sd(s))}
plot(100)  
plot(1000) 
plot(10000)

```

{{out}}

```txt

n = 100
0.0 *****************
0.1 ******
0.2 ********
0.3 **********
0.4 ***********
0.5 *********
0.6 ***********
0.7 **********
0.8 ******
0.9 ************
mean = 0.482634518758
sd   = 0.300804579739938409

n = 1000
0.0 *******
0.1 ********
0.2 ***********
0.3 ***********
0.4 *********
0.5 ***********
0.6 ********
0.7 ************
0.8 **********
0.9 ********
mean = 0.510119356421
sd   = 0.277396945925369919

n = 10000
0.0 **********
0.1 *********
0.2 *********
0.3 **********
0.4 *********
0.5 **********
0.6 *********
0.7 *********
0.8 **********
0.9 **********
mean = 0.49854591894824
sd   = 0.290375399458904972

```



## Kotlin

{{trans|FreeBASIC}}

```scala
// version 1.1.2

val rand = java.util.Random()

fun basicStats(sampleSize: Int) {
    if (sampleSize < 1) return
    val r = DoubleArray(sampleSize)
    val h = IntArray(10) // all zero by default
    /*
       Generate 'sampleSize' random numbers in the interval [0, 1)
       and calculate in which box they will fall when drawing the histogram
    */
    for (i in 0 until sampleSize) {
        r[i] = rand.nextDouble()
        h[(r[i] * 10).toInt()]++
    }

    // adjust one of the h[] values if necessary to ensure they sum to sampleSize
    val adj = sampleSize - h.sum()
    if (adj != 0) {
        for (i in 0..9) {
            h[i] += adj
            if (h[i] >= 0) break
            h[i] -= adj
        }
    }

    val mean = r.average()
    val sd = Math.sqrt(r.map { (it - mean) * (it - mean) }.average())
  
    // Draw a histogram of the data with interval 0.1 
    var numStars: Int
    // If sample size > 500 then normalize histogram to 500 
    val scale = if (sampleSize <= 500) 1.0 else 500.0 / sampleSize 
    println("Sample size $sampleSize\n")
    println("  Mean ${"%1.6f".format(mean)}  SD ${"%1.6f".format(sd)}\n") 
    for (i in 0..9) {
        print("  %1.2f : ".format(i / 10.0))
        print("%5d ".format(h[i]))
        numStars = (h[i] * scale + 0.5).toInt()
        println("*".repeat(numStars))
    }
    println()
}

fun main(args: Array<String>) {
    val sampleSizes = intArrayOf(100, 1_000, 10_000, 100_000) 
    for (sampleSize in sampleSizes) basicStats(sampleSize)
}
```

Sample run:
{{out}}

```txt

Sample size 100

  Mean 0.489679  SD 0.286151

  0.00 :    12 ************
  0.10 :     7 *******
  0.20 :    13 *************
  0.30 :     9 *********
  0.40 :    10 **********
  0.50 :     8 ********
  0.60 :    14 **************
  0.70 :    10 **********
  0.80 :     8 ********
  0.90 :     9 *********

Sample size 1000

  Mean 0.497003  SD 0.290002

  0.00 :   104 ****************************************************
  0.10 :    92 **********************************************
  0.20 :   107 ******************************************************
  0.30 :   109 *******************************************************
  0.40 :    96 ************************************************
  0.50 :   111 ********************************************************
  0.60 :    87 ********************************************
  0.70 :    79 ****************************************
  0.80 :   117 ***********************************************************
  0.90 :    98 *************************************************

Sample size 10000

  Mean 0.505243  SD 0.288944

  0.00 :   991 **************************************************
  0.10 :   938 ***********************************************
  0.20 :  1034 ****************************************************
  0.30 :   958 ************************************************
  0.40 :   963 ************************************************
  0.50 :  1003 **************************************************
  0.60 :  1081 ******************************************************
  0.70 :   995 **************************************************
  0.80 :  1001 **************************************************
  0.90 :  1036 ****************************************************

Sample size 100000

  Mean 0.500501  SD 0.288766

  0.00 : 10015 **************************************************
  0.10 :  9844 *************************************************
  0.20 : 10012 **************************************************
  0.30 : 10160 ***************************************************
  0.40 : 10051 **************************************************
  0.50 :  9938 **************************************************
  0.60 :  9934 **************************************************
  0.70 :  9914 **************************************************
  0.80 : 10057 **************************************************
  0.90 : 10075 **************************************************

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
		#h->get(integer(#n*10)+1) += 1
	}
	local(mx = decimal(with n in #h max #n))
	with i in #h do => {
		#out->append((#sc/10.0)->asString(-precision=1)+': '+('+' * integer(#i / #mx * #maxwidth))+'\r')
		#sc++
	}
	return #out
}

with scale in array(100,1000,10000,100000) do => {^
	local(n = array)
	loop(#scale) => { #n->insert(decimal_random) }
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
Naive  method: sd: 0.291640, mean: 0.549633
Second  method: sd: 0.291640, mean: 0.549633

0.0: ++++++++++++++++++
0.1: ++++++++++++++++++
0.2: ++++++++++++++++++++++++++++++++++++
0.3: +++++++++++++++++++++++++++++++++++++++++++
0.4: ++++++++++++++++++++++++++++++++
0.5: +++++++++++++++++++++++++++++
0.6: ++++++++++++++++++++++++++++++++
0.7: +++++++++++++++++++++++++++++
0.8: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.9: +++++++++++++++++++++++++++++++++++++++++++
1.0: +++++++++++++++++++++++++++++


1000 numbers:
Naive  method: sd: 0.288696, mean: 0.500533
Second  method: sd: 0.288696, mean: 0.500533

0.0: +++++++++++++++++++++
0.1: +++++++++++++++++++++++++++++++++++++++
0.2: ++++++++++++++++++++++++++++++++++++++++
0.3: +++++++++++++++++++++++++++++++
0.4: +++++++++++++++++++++++++++++++++++++
0.5: ++++++++++++++++++++++++++++++++++
0.6: ++++++++++++++++++++++++++++++++++++++
0.7: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.8: ++++++++++++++++++++++++++++++++++++
0.9: ++++++++++++++++++++++++++++++++++
1.0: +++++++++++++++++++


10000 numbers:
Naive  method: sd: 0.289180, mean: 0.496726
Second  method: sd: 0.289180, mean: 0.496726

0.0: ++++++++++++++++++++++++
0.1: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.2: ++++++++++++++++++++++++++++++++++++++++++++++
0.3: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.4: +++++++++++++++++++++++++++++++++++++++++++++++
0.5: +++++++++++++++++++++++++++++++++++++++++++++++
0.6: +++++++++++++++++++++++++++++++++++++++++++++++
0.7: +++++++++++++++++++++++++++++++++++++++++++++++
0.8: ++++++++++++++++++++++++++++++++++++++++++++++++
0.9: +++++++++++++++++++++++++++++++++++++++++++++++
1.0: +++++++++++++++++++++++


100000 numbers:
Naive  method: sd: 0.288785, mean: 0.500985
Second  method: sd: 0.288785, mean: 0.500985

0.0: +++++++++++++++++++++++++
0.1: +++++++++++++++++++++++++++++++++++++++++++++++++
0.2: ++++++++++++++++++++++++++++++++++++++++++++++++
0.3: +++++++++++++++++++++++++++++++++++++++++++++++++
0.4: +++++++++++++++++++++++++++++++++++++++++++++++++
0.5: +++++++++++++++++++++++++++++++++++++++++++++++++
0.6: +++++++++++++++++++++++++++++++++++++++++++++++++
0.7: +++++++++++++++++++++++++++++++++++++++++++++++++
0.8: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.9: ++++++++++++++++++++++++++++++++++++++++++++++++++
1.0: ++++++++++++++++++++++++
```



## Liberty BASIC

Be aware that the PRNG in LB has a SLIGHT bias.

```lb

call sample    100
call sample   1000
call sample  10000

end

sub sample n
    dim dat( n)
    for i =1 to n
        dat( i) =rnd( 1)
    next i

    '// show mean, standard deviation
    sum =0
    sSq =0
    for i =1 to n
        sum =sum +dat( i)
        sSq =sSq +dat( i)^2
    next i
    print n; " data terms used."

    mean =sum / n
    print "Mean ="; mean

    print "Stddev ="; ( sSq /n -mean^2)^0.5

    '// show histogram
    nBins =10
    dim bins( nBins)
    for i =1 to n
        z =int( nBins *dat( i))
        bins( z) =bins( z) +1
    next i
    for b =0 to nBins -1
        for j =1 to int( nBins *bins( b)) /n *70)
            print "#";
        next j
        print
    next b
    print
end sub

```

 100000 data terms used.
 Mean =0.49870232
 Stddev =0.28926563
 ######################################################################
 ######################################################################
 ######################################################################
 ######################################################################
 #####################################################################
 #####################################################################
 #####################################################################
 #####################################################################
 ######################################################################
 #####################################################################


## Lua

The standard deviation seems to converge to around 0.28.  I expect there's a good reason for this, though it's entirely beyond me.

```lua

math.randomseed(os.time())

function randList (n)  -- Build table of size n
	local numbers = {}
	for i = 1, n do
		table.insert(numbers, math.random()) -- range correct by default
	end
	return numbers
end

function mean (t)  -- Find mean average of values in table t
	local sum = 0
	for k, v in pairs(t) do
		sum = sum + v
	end
	return sum / #t
end

function stdDev (t)  -- Find population standard deviation of table t
	local squares, avg = 0, mean(t)
	for k, v in pairs(t) do
		squares = squares + ((avg - v) ^ 2)
	end
	local variance = squares / #t
	return math.sqrt(variance)
end

function showHistogram (t)  -- Draw histogram of given table to stdout
	local histBars, compVal = {}
	for range = 0, 9 do
		histBars[range] = 0
		for k, v in pairs(t) do
			compVal = tonumber(string.format("%0.1f", v - 0.05))
			if compVal == range / 10 then
				histBars[range] = histBars[range] + 1
			end
		end
	end
	for k, v in pairs(histBars) do
		io.write("0." .. k .. " " .. string.rep('=', v / #t * 200))
		print(" " .. v)
	end
	print()
end

function showStats (tabSize)  -- Create and display statistics info
	local numList = randList(tabSize)
	print("Table of size " .. #numList)
	print("Mean average: " .. mean(numList))
	print("Standard dev: " .. stdDev(numList))
	showHistogram(numList)
end

for power = 2, 5 do  -- Start of main procedure
	showStats(10 ^ power)
end

```



## Maple

The following samples 100 uniformly distributed numbers between 0 and 1:

```maple
with(Statistics):
X_100 := Sample( Uniform(0,1), 100 );
Mean( X_100 );
StandardDeviation( X_100 );
Histogram( X_100 );
```

It is also possible to make a procedure that outputs the mean, standard deviation, and a histogram for a given number of random uniformly distributed numbers:

```maple
sample := proc( n )
    local data;
    data := Sample( Uniform(0,1), n );
    printf( "Mean: %.4f\nStandard Deviation: %.4f", 
             Statistics:-Mean( data ),
             Statistics:-StandardDeviation( data ) );
    return Statistics:-Histogram( data );
end proc:
sample( 1000 );
```



## Mathematica


```Mathematica
Sample[n_]:= (Print[#//Length," numbers, Mean : ",#//Mean,", StandardDeviation : ",#//StandardDeviation ];
        BarChart[BinCounts[#,{0,1,.1}], Axes->False, BarOrigin->Left])&[(RandomReal[1,#])&[ n ]]

Sample/@{100,1 000,10 000,1 000 000} 
```

{{out}}

```txt
100 numbers, Mean : 0.478899, StandardDeviation : 0.322265
1000 numbers, Mean : 0.503383, StandardDeviation : 0.278352
10000 numbers, Mean : 0.498278, StandardDeviation : 0.28925
1000000 numbers, Mean : 0.500248, StandardDeviation : 0.288713
```

[[File:mma_basicstat.PNG]]

=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
  % Initialize
  N = 0; S=0; S2 = 0; 
  binlist = 0:.1:1;	
  h = zeros(1,length(binlist));  % initialize histogram

  % read data and perform computation
  while (1)
	% read next sample x
        if (no_data_available) break; end; 
        N = N + 1;
        S = S + x;
        S2= S2+ x*x;
        ix= sum(x < binlist);
        h(ix) = h(ix)+1; 
  end 	

  % generate output
  m  = S/N;   % mean  
  sd = sqrt(S2/N-mean*mean);  % standard deviation 
  bar(binlist,h)
```



## Nim


```nim
import math, strutils
randomize()

proc sd(ns): auto =
  var sx, sxx = 0.0
  for x in ns:
    sx += x
    sxx += x * x
  let sd = if ns.len > 0: sqrt(float(ns.len) * sxx - sx * sx) / float(ns.len)
           else: 0
  (sd, sx / float(ns.len))

proc histogram(ns) =
  var h = newSeq[int](10)
  for n in ns:
    let pos = int(n * 10)
    inc h[pos]

  const maxWidth = 50
  let mx = max(h)
  echo ""
  for n, i in h:
    echo n/10,": ",repeatChar(int(i / mx * maxWidth), '+')
  echo ""

for i in [10, 100, 1_000, 10_000, 100_000]:
  var n = newSeq[float](i)
  for x in 0..n.high: n[x] = random(1.0)
  echo "\n##\n## ",i," numbers\n##"
  let (sd, mean) = sd(n)
  echo "sd: ",sd,", mean: ",mean
  histogram(n)
```

{{out}}

```txt
##
## 10 numbers
##
sd: 0.2738118959385979, mean: 0.4717111448227304

0.0: +++++++++++++++++++++++++
0.1: +++++++++++++++++++++++++
0.2: 
0.3: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.4: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.5: 
0.6: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.7: +++++++++++++++++++++++++
0.8: 
0.9: +++++++++++++++++++++++++

[...]

##
## 100000 numbers
##
sd: 0.2884329643843962, mean: 0.4997598571602153

0.0: ++++++++++++++++++++++++++++++++++++++++++++++++
0.1: +++++++++++++++++++++++++++++++++++++++++++++++++
0.2: ++++++++++++++++++++++++++++++++++++++++++++++++
0.3: +++++++++++++++++++++++++++++++++++++++++++++++++
0.4: ++++++++++++++++++++++++++++++++++++++++++++++++
0.5: ++++++++++++++++++++++++++++++++++++++++++++++++
0.6: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.7: +++++++++++++++++++++++++++++++++++++++++++++++++
0.8: +++++++++++++++++++++++++++++++++++++++++++++++++
0.9: ++++++++++++++++++++++++++++++++++++++++++++++++
```



## Oforth



```Oforth
: main(n)
| l m std i nb |

   // Create list and calculate avg and stddev
   ListBuffer init(n, #[ Float rand ]) dup ->l avg ->m
   0 l apply(#[ sq +]) n / m sq - sqrt ->std
   System.Out "n = " << n << ", avg = " << m << ", std = " << std << cr

   // Histo
   0.0 0.9 0.1 step: i [ 
      l count(#[ between(i, i 0.1 +) ]) 400 * n / asInteger ->nb
      System.Out i <<wjp(3, JUSTIFY_RIGHT, 2) " - " << 
                 i 0.1 + <<wjp(3, JUSTIFY_RIGHT, 2) " - " <<
                 StringBuffer new "*" <<n(nb) << cr
      ] ;
```


{{out}}

```txt

>100 main
n = 100, avg = 0.483425493606762, std = 0.280986417046947
  0 - 0.1 - ********************************
0.1 - 0.2 - ****************************************************
0.2 - 0.3 - ************************************************
0.3 - 0.4 - ************************************
0.4 - 0.5 - ********************************
0.5 - 0.6 - ****************************************************
0.6 - 0.7 - ********************************
0.7 - 0.8 - ****************************************************
0.8 - 0.9 - ****************************************
0.9 -   1 - ************************
ok
>main(1000)
n = 1000, avg = 0.514985138392994, std = 0.288119541786792
  0 - 0.1 - ************************************
0.1 - 0.2 - **************************************
0.2 - 0.3 - ********************************
0.3 - 0.4 - ***********************************************
0.4 - 0.5 - ************************************
0.5 - 0.6 - ***************************************
0.6 - 0.7 - ***************************************
0.7 - 0.8 - ****************************************
0.8 - 0.9 - *******************************************
0.9 -   1 - *********************************************
ok
>main(10000)
n = 10000, avg = 0.501457911440693, std = 0.289120988428389
  0 - 0.1 - ***************************************
0.1 - 0.2 - ****************************************
0.2 - 0.3 - ****************************************
0.3 - 0.4 - ***************************************
0.4 - 0.5 - **************************************
0.5 - 0.6 - ***************************************
0.6 - 0.7 - *****************************************
0.7 - 0.8 - *****************************************
0.8 - 0.9 - ***************************************
0.9 -   1 - ****************************************
ok
>main(100000)
n = 100000, avg = 0.499807481461133, std = 0.28907281580804
  0 - 0.1 - ****************************************
0.1 - 0.2 - ***************************************
0.2 - 0.3 - ***************************************
0.3 - 0.4 - ***************************************
0.4 - 0.5 - ***************************************
0.5 - 0.6 - ****************************************
0.6 - 0.7 - ***************************************
0.7 - 0.8 - ****************************************
0.8 - 0.9 - ***************************************
0.9 -   1 - ****************************************
ok
>main(1000000)
n = 1000000, avg = 0.500078448259022, std = 0.288580229525348
  0 - 0.1 - ***************************************
0.1 - 0.2 - ****************************************
0.2 - 0.3 - ****************************************
0.3 - 0.4 - ****************************************
0.4 - 0.5 - ***************************************
0.5 - 0.6 - ****************************************
0.6 - 0.7 - ****************************************
0.7 - 0.8 - ****************************************
0.8 - 0.9 - ***************************************
0.9 -   1 - ***************************************
ok
>

```



## PARI/GP

{{works with|PARI/GP|2.4.3 and above}}

```parigp
mean(v)={
  vecsum(v)/#v
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
  my(v=vector(n,i,random(1.)),mu=mean(v),s=stdev(v,mu),h=histogram(v),sz=ceil(n/50/16));
  for(i=1,16,for(j=1,h[i]\sz,print1("#"));print());
  print("Mean: "mu);
  print("Stdev: "s);
};
show(100);
show(1000);
show(10000);
```


For versions before 2.4.3, define

```parigp
rreal()={
  my(pr=32*ceil(default(realprecision)*log(10)/log(4294967296))); \\ Current precision
  random(2^pr)*1.>>pr
};
```

and use <code>rreal()</code> in place of <code>random(1.)</code>.


## Perl


```perl
my @histogram = (0) x 10;
my $sum = 0;
my $sum_squares = 0;
my $n = $ARGV[0];

for (1..$n) { 
  my $current = rand();
  $sum+= $current;
  $sum_squares+= $current ** 2;
  $histogram[$current * @histogram]+= 1;
}

my $mean = $sum / $n;

print "$n numbers\n", 
      "Mean:   $mean\n",
      "Stddev: ", sqrt(($sum_squares / $n) - ($mean ** 2)), "\n";

for my $i (0..$#histogram) {
  printf "%.1f - %.1f : ", $i/@histogram, (1 + $i)/@histogram;

  print "*" x (30 * $histogram[$i] * @histogram/$n); # 30 stars expected per row
  print "\n";
}
```


Usage: 
```txt
perl rand_statistics.pl (number of values)
```



```txt
$ perl rand_statistics.pl 100
100 numbers
Mean:   0.531591369804339
Stddev: 0.28440375340793
0.0 - 0.1 : ***************************
0.1 - 0.2 : ************************
0.2 - 0.3 : ***************************
0.3 - 0.4 : ************************
0.4 - 0.5 : *********************************
0.5 - 0.6 : ************************************
0.6 - 0.7 : ************************************
0.7 - 0.8 : ******************
0.8 - 0.9 : ***************************************
0.9 - 1.0 : ************************************

$ perl rand_statistics.pl 1000
1000 numbers
Mean:   0.51011452684812
Stddev: 0.29490201218115
0.0 - 0.1 : ******************************
0.1 - 0.2 : *******************************
0.2 - 0.3 : ***************************
0.3 - 0.4 : *****************************
0.4 - 0.5 : **********************************
0.5 - 0.6 : ****************************
0.6 - 0.7 : ************************
0.7 - 0.8 : *************************************
0.8 - 0.9 : ********************************
0.9 - 1.0 : *********************************

$ perl rand_statistics.pl 10000
10000 numbers
Mean:   0.495329167703333
Stddev: 0.285944419431566
0.0 - 0.1 : *****************************
0.1 - 0.2 : *******************************
0.2 - 0.3 : *********************************
0.3 - 0.4 : *******************************
0.4 - 0.5 : ******************************
0.5 - 0.6 : *******************************
0.6 - 0.7 : ******************************
0.7 - 0.8 : ******************************
0.8 - 0.9 : *****************************
0.9 - 1.0 : ******************************

$ perl rand_statistics.pl 10000000
10000000 numbers
Mean:   0.499973935749229
Stddev: 0.2887231680817
0.0 - 0.1 : ******************************
0.1 - 0.2 : *******************************
0.2 - 0.3 : ******************************
0.3 - 0.4 : *******************************
0.4 - 0.5 : ******************************
0.5 - 0.6 : *******************************
0.6 - 0.7 : ******************************
0.7 - 0.8 : ******************************
0.8 - 0.9 : *******************************
0.9 - 1.0 : *******************************
```



## Perl 6

{{Works with|rakudo|2018.03}}

```perl6
for 100, 1_000, 10_000 -> $N {
    say "size: $N";
    my @data = rand xx $N;
    printf "mean: %f\n", my $mean = $N R/ [+] @data;
    printf "stddev: %f\n", sqrt
    $mean**2 R- $N R/ [+] @data »**» 2;
    printf "%.1f %s\n", .key, '=' x (500 * .value.elems / $N)
        for sort @data.classify: (10 * *).Int / 10;
    say '';
}
```

{{out}}

```txt
size: 100
mean: 0.52518699464629726
stddev: 0.28484207464779548
0.0	
### ========================

0.1	
### ================================================================

0.2	
### =============================

0.3	
### ============================================

0.4	
### ======================================================

0.5	
### =======================================

0.6	
### ==============

0.7	
### =====================================================================

0.8	
### ================================================================

0.9	
### =======================================


size: 1000
mean: 0.51043974182914975
stddev: 0.29146336553431618
0.0	
### ========================================

0.1	
### ============================================

0.2	
### =====================================

0.3	
### ==================================================

0.4	
### =============================================

0.5	
### =================================

0.6	
### =====================================================

0.7	
### ==============================================

0.8	
### ========================================

0.9	
### ==================================================


size: 10000
mean: 0.50371817503544458
stddev: 0.2900716333092252
0.0	
### =============================================

0.1	
### ===========================================

0.2	
### =======================================

0.3	
### ==============================================

0.4	
### ========================================

0.5	
### ==============================================

0.6	
### ==========================================

0.7	
### =============================================

0.8	
### ==============================================

0.9	
### ============================================

```



## Phix

{{trans|CoffeeScript}}
To do a trillion samples, I would change the existing generate loop into an inner 100_000_000 loop that still uses the fast native types, with everything outside that changed to bigatom, and of course add an outer loop which sums into them.

```Phix
function generate_statistics(integer n)
sequence hist = repeat(0,10)
atom sum_r = 0,
     sum_squares = 0.0
 
    for i=1 to n do
        atom r = rnd()
        sum_r += r
        sum_squares += r*r
        hist[floor(10*r)+1] += 1
    end for
    atom mean = sum_r / n
    atom stddev = sqrt((sum_squares / n) - mean*mean)
 
     return {n, mean, stddev, hist}
end function
 
procedure display_statistics(sequence x)
atom n, mean, stddev
sequence hist
    {n, mean, stddev, hist} = x
    printf(1,"-- Stats for sample size %d\n",{n})
    printf(1,"mean: %g\n",{mean})
    printf(1,"sdev: %g\n",{stddev})
    for i=1 to length(hist) do
        integer cnt = hist[i]
        string bars = repeat('=',floor(cnt*300/n))
        printf(1,"%.1f: %s %d\n",{i/10,bars,cnt})
    end for
end procedure
 
for n=2 to 5 do
    display_statistics(generate_statistics(power(10,n+(n=5))))
end for
```

{{Out}}
<pre style="float:left; font-size: 10px">
-- Stats for sample size 100
mean: 0.530925
sdev: 0.303564
0.1: 
### ==================
 8
0.2: 
### =================================
 13
0.3: 
### ========================
 10
0.4: 
### ============
 6
0.5: 
### ===============
 7
0.6: 
### ===========================
 11
0.7: 
### ===========================
 11
0.8: 
### ===============
 7
0.9: 
### =================================
 13
1.0: 
### ====================================
 14

```

<pre style="float:left; font-size: 10px">
-- Stats for sample size 1000
mean: 0.50576
sdev: 0.288862
0.1: 
### ======================
 95
0.2: 
### ========================
 103
0.3: 
### =======================
 98
0.4: 
### =====================
 93
0.5: 
### ========================
 101
0.6: 
### =======================
 99
0.7: 
### =========================
 105
0.8: 
### =======================
 97
0.9: 
### ==========================
 108
1.0: 
### ========================
 101

```

<pre style="float:left; font-size: 10px">
-- Stats for sample size 10000
mean: 0.498831
sdev: 0.28841
0.1: 
### =======================
 987
0.2: 
### =========================
 1060
0.3: 
### ======================
 953
0.4: 
### =======================
 980
0.5: 
### ========================
 1013
0.6: 
### =======================
 997
0.7: 
### ==========================
 1089
0.8: 
### ======================
 948
0.9: 
### =======================
 974
1.0: 
### =======================
 999

```

<pre style="font-size: 10px">
-- Stats for sample size 1000000
mean: 0.499937
sdev: 0.288898
0.1: 
### ========================
 100071
0.2: 
### ========================
 100943
0.3: 
### =======================
 99594
0.4: 
### =======================
 99436
0.5: 
### =======================
 99806
0.6: 
### =======================
 99723
0.7: 
### ========================
 100040
0.8: 
### ========================
 100280
0.9: 
### ========================
 100264
1.0: 
### =======================
 99843

```



## PicoLisp

The following has no limit on the number of samples. The 'statistics' function accepts an executable body 'Prg', which it calls repeatedly to get the samples.

```PicoLisp

(seed (time))

(scl 8)
 
(de statistics (Cnt . Prg)
   (prinl Cnt " numbers")
   (let (Sum 0  Sqr 0  Hist (need 10 NIL 0))
      (do Cnt
         (let N (run Prg 1)  # Get next number
            (inc 'Sum N)
            (inc 'Sqr (*/ N N 1.0))
            (inc (nth Hist (inc (/ N 0.1)))) ) )
      (let M (*/ Sum Cnt)
         (prinl "Mean:   " (round M))
         (prinl "StdDev: "
            (round
               (sqrt
                  (- (*/ Sqr Cnt) (*/ M M 1.0))
                  1.0 ) ) ) )
      (for (I . H) Hist
         (prin (format I 1) " ")
         (do (*/ H 400 Cnt) (prin '=))
         (prinl) ) ) )

(for I (2 4 6)
   (statistics (** 10 I)
      (rand 0 (dec 1.0)) )
   (prinl) )

```

{{out}}

```txt
100 numbers
Mean:   0.501
StdDev: 0.284
0.1 
### ==================================

0.2 
### ==============================

0.3 
### ==============================================

0.4 
### ==================

0.5 
### ==================

0.6 
### ==========================================================

0.7 
### ==================================================

0.8 
### ==============================

0.9 
### ==================

1.0 
### ======================================


10000 numbers
Mean:   0.501
StdDev: 0.288
0.1 
### =================================

0.2 
### ==================================

0.3 
### =================================

0.4 
### ===================================

0.5 
### ===================================

0.6 
### ==================================

0.7 
### ===================================

0.8 
### ==================================

0.9 
### ==================================

1.0 
### ==================================


1000000 numbers
Mean:   0.500
StdDev: 0.289
0.1 
### ==================================

0.2 
### ==================================

0.3 
### ==================================

0.4 
### ==================================

0.5 
### ==================================

0.6 
### ==================================

0.7 
### ==================================

0.8 
### ==================================

0.9 
### ==================================

1.0 
### ==================================

```



## PL/I


```pli
 stat: procedure options (main); /* 21 May 2014 */

stats: procedure (values, mean, standard_deviation);
   declare (values(*), mean, standard_deviation) float;
   declare n fixed binary (31) initial ( (hbound(values,1)) );

   mean = sum(values)/n;

   standard_deviation = sqrt( sum(values - mean)**2 / n);

end stats;

   declare values (*) float controlled;
   declare (mean, stddev) float;
   declare bin(0:9) fixed;
   declare (i, n) fixed binary (31);

   do n = 100, 1000, 10000, 100000;
      allocate values(n);
      values = random();
      call stats (values, mean, stddev);

      if n = 100 then
         do;
            bin = 0;
            do i = 1 to 100;
               bin(10*values(i)) += 1;
            end;
            put skip list ('Histogram for 100 values:');
            do i = 0 to 9;  /* display histogram */
               put skip list (repeat('.', bin(i)) );
            end;
         end;

      put skip list (n || ' values: mean=' || mean, 'stddev=' || stddev);
      free values;
   end;

end stat;
```

{{out}}

```txt

Histogram for 100 values: 
....... 
.............. 
.............. 
........... 
............... 
........ 
........... 
......... 
....... 
.............. 
           100 values: mean= 4.89708E-0001      stddev= 1.64285E-0007 
          1000 values: mean= 4.97079E-0001      stddev= 1.07871E-0005 
         10000 values: mean= 4.99119E-0001      stddev= 8.35870E-0005 
        100000 values: mean= 5.00280E-0001      stddev= 7.88976E-0004 

```



## PureBasic

{{trans|Liberty BASIC}}
Changes were made from the Liberty BASIC version to normalize the histogram as well as implement a random float function.

```purebasic
Procedure.f randomf()
  #RNG_max_resolution = 2147483647
  ProcedureReturn Random(#RNG_max_resolution) / #RNG_max_resolution
EndProcedure

Procedure sample(n)
  Protected i, nBins, binNumber, tickMarks, maxBinValue
  Protected.f sum, sumSq, mean
  
  Dim dat.f(n)
  For i = 1 To n
    dat(i) = randomf()
  Next
  
  ;show mean, standard deviation
  For i = 1 To n
    sum + dat(i)
    sumSq + dat(i) * dat(i)
  Next i
  
  PrintN(Str(n) + " data terms used.")
  mean = sum / n
  PrintN("Mean =" + StrF(mean))
  PrintN("Stddev =" + StrF((sumSq / n) - Sqr(mean * mean)))
  
  ;show histogram
  nBins = 10
  Dim bins(nBins)
  For i = 1 To n
    binNumber = Int(nBins * dat(i))
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
    tickMarks = Int(bins(binNumber) * #normalizedMaxValue / maxBinValue)
    PrintN(ReplaceString(Space(tickMarks), " ", "#"))
  Next
  PrintN("")
EndProcedure

If OpenConsole()
  sample(100)
  sample(1000)
  sample(10000)
   
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
100 data terms used.
Mean =0.4349198639
Stddev =-0.1744846404
#########################################################
#########################################
################################
#################################################################
################################
#####################################################
######################################################################
################
########################
################


1000 data terms used.
Mean =0.4960154891
Stddev =-0.1691310555
###############################################################
#######################################################
#############################################################
######################################################################
##########################################################
##############################################################
####################################################################
###############################################################
#############################################################
#####################################################


10000 data terms used.
Mean =0.5042046309
Stddev =-0.1668083966
##################################################################
################################################################
##################################################################
####################################################################
################################################################
######################################################################
####################################################################
###################################################################
####################################################################
####################################################################
```



## Python

The second function, sd2 only needs to go once through the numbers and so can more efficiently handle large streams of numbers.

```python
def sd1(numbers):
    if numbers:
        mean = sum(numbers) / len(numbers)
        sd = (sum((n - mean)**2 for n in numbers) / len(numbers))**0.5
        return sd, mean
    else:
        return 0, 0

def sd2(numbers):
    if numbers:
        sx = sxx = n = 0
        for x in numbers:
            sx += x
            sxx += x*x
            n += 1
        sd = (n * sxx - sx*sx)**0.5 / n
        return sd, sx / n
    else:
        return 0, 0

def histogram(numbers):
    h = [0] * 10
    maxwidth = 50 # characters
    for n in numbers:
        h[int(n*10)] += 1
    mx = max(h)
    print()
    for n, i in enumerate(h):
        print('%3.1f: %s' % (n / 10, '+' * int(i / mx * maxwidth)))
    print()

if __name__ == '__main__':
    import random
    for i in range(1, 6):
        n = [random.random() for j in range(10**i)]
        print("\n##\n## %i numbers\n##" % 10**i)
        print('  Naive  method: sd: %8.6f, mean: %8.6f' % sd1(n))
        print('  Second method: sd: %8.6f, mean: %8.6f' % sd2(n))
        histogram(n)
```


{{out}}
for larger sets of random numbers, 
the distribution of numbers between the bins of the histogram evens out.

```txt
...
##
## 100 numbers
##
  Naive  method: sd: 0.288911, mean: 0.508686
  Second method: sd: 0.288911, mean: 0.508686

0.0: +++++++++++++++++++++++++++++++
0.1: ++++++++++++++++++++++++++++
0.2: +++++++++++++++++++++++++
0.3: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.4: ++++++++++++++++++
0.5: +++++++++++++++++++++++++++++++
0.6: ++++++++++++++++++
0.7: +++++++++++++++++++++++++++++++++++++
0.8: ++++++++++++++++++++++++++++++++++++++++
0.9: +++++++++++++++++++++++++++++++

...

##
## 10000000 numbers
##
  Naive  method: sd: 0.288750, mean: 0.499839
  Second method: sd: 0.288750, mean: 0.499839

0.0: ++++++++++++++++++++++++++++++++++++++++++++++++++
0.1: +++++++++++++++++++++++++++++++++++++++++++++++++
0.2: +++++++++++++++++++++++++++++++++++++++++++++++++
0.3: +++++++++++++++++++++++++++++++++++++++++++++++++
0.4: +++++++++++++++++++++++++++++++++++++++++++++++++
0.5: +++++++++++++++++++++++++++++++++++++++++++++++++
0.6: +++++++++++++++++++++++++++++++++++++++++++++++++
0.7: +++++++++++++++++++++++++++++++++++++++++++++++++
0.8: +++++++++++++++++++++++++++++++++++++++++++++++++
0.9: +++++++++++++++++++++++++++++++++++++++++++++++++
```



## R

The challenge of processing a trillion numbers is generating them in the first place. As the errors below show, allocating 7.5 TB for such a vector is simply impractical. The workaround is to generate them, process individual data points and then discard them. The downside in this case is the time.

```R

#Generate the sets
a = runif(10,min=0,max=1)
b = runif(100,min=0,max=1)
c = runif(1000,min=0,max=1)
d = runif(10000,min=0,max=1)

#Print out the set of 10 values
cat("a = ",a)

#Print out the Mean and Standard Deviations of each of the sets
cat("Mean of a : ",mean(a))
cat("Standard Deviation of a : ", sd(a))
cat("Mean of b : ",mean(b))
cat("Standard Deviation of b : ", sd(b))
cat("Mean of c : ",mean(c))
cat("Standard Deviation of c : ", sd(c))
cat("Mean of d : ",mean(d))
cat("Standard Deviation of d : ", sd(d))

#Plotting the histogram of d
hist(d)

#Following lines error out due to insufficient memory

cat("Mean of a trillion random values in the range [0,1] : ",mean(runif(10^12,min=0,max=1)))
cat("Standard Deviation of a trillion random values in the range [0,1] : ", sd(runif(10^12,min=0,max=1)))

```

Output

```txt


a =  0.3884718 0.6324655 0.9288667 0.1948398 0.5636742 0.2746207 0.4712035 0.2624648 0.45492 0.3328236> 

Mean of a :  0.4504351
Standard Deviation of a :  0.2171919
Mean of b :  0.5240795
Standard Deviation of b :  0.2654211
Mean of c :  0.5000978
Standard Deviation of c :  0.2882098
Mean of d :  0.4991501
Standard Deviation of d :  0.2911486

Error: cannot allocate vector of size 7450.6 Gb

Error: cannot allocate vector of size 7450.6 Gb

```



## Racket


```racket

#lang racket
(require math (only-in srfi/27 random-real))

(define (histogram n xs Δx)
  (define (r x) (~r x #:precision 1 #:min-width 3))
  (define (len count) (exact-floor (/ (* count 200) n)))
  (for ([b (bin-samples (range 0 1 Δx) <= xs)])
    (displayln (~a (r (sample-bin-min b)) "-" (r (sample-bin-max b)) ": " 
                   (make-string (len (length (sample-bin-values b))) #\*)))))

(define (task n)
  (define xs (for/list ([_ n]) (random-real)))
  (displayln (~a "Number of samples: " n))
  (displayln (~a "Mean: " (mean xs)))
  (displayln (~a "Standard deviance: " (stddev xs)))
  (histogram n xs 0.1)
  (newline))

(task 100)
(task 1000)
(task 10000)

```

{{out}}

```txt

Number of samples: 100
Mean: 0.5466640451797568
Standard deviance: 0.29309099509716496
  0-0.1: ************
0.1-0.2: ************************
0.2-0.3: ********************
0.3-0.4: ************
0.4-0.5: ****************
0.5-0.6: ********************
0.6-0.7: ********************
0.7-0.8: **************************
0.8-0.9: **************************
0.9-  1: ************************

Number of samples: 1000
Mean: 0.48116201801707503
Standard deviance: 0.2873408579602762
  0-0.1: *********************
0.1-0.2: *********************
0.2-0.3: ********************
0.3-0.4: ***********************
0.4-0.5: *******************
0.5-0.6: *******************
0.6-0.7: *******************
0.7-0.8: *****************
0.8-0.9: ******************
0.9-  1: ******************

Number of samples: 10000
Mean: 0.4988839808467469
Standard deviance: 0.2892924816935072
  0-0.1: ********************
0.1-0.2: *******************
0.2-0.3: ********************
0.3-0.4: *******************
0.4-0.5: *******************
0.5-0.6: ********************
0.6-0.7: ********************
0.7-0.8: *******************
0.8-0.9: ********************
0.9-  1: *******************

```



## REXX

Twenty decimal digits are used for the calculations, but only half that (ten digits) are displayed in the output. 

```rexx
/*REXX program generates some random numbers, shows bin histogram, finds mean & stdDev. */
numeric digits 20                                /*use twenty decimal digits precision, */
showDigs=digits()%2                              /* ··· but only show ten decimal digits*/
parse arg size seed .                            /*allow specification:  size, and seed.*/
if size=='' | size==","  then size=100           /*Not specified?  Then use the default.*/
if datatype(seed,'W')    then call random ,,seed /*allow a  seed  for the  RANDOM  BIF. */
#.=0                                             /*count of the numbers in each bin.    */
                do j=1  for size                 /*generate some random numbers.        */
                @.j=random(, 99999)  /  100000   /*express random number as a fraction. */
                _=substr(@.j'00', 3, 1)          /*determine which bin the number is in,*/
                #._=#._ + 1                      /*    ···  and bump its count.         */
                end   /*j*/

     do k=0  for 10;    kp=k + 1                 /*show a histogram of the bins.        */
     lr='0.'k      ;    if k==0  then lr= "0  "  /*adjust for the  low range.           */
     hr='0.'kp     ;    if k==9  then hr= "1  "  /*   "    "   "  high range.           */
     barPC=right( strip( left( format( 100*#.k / size, , 2), 5)), 5)   /*compute the %. */
     say lr"──►"hr' '   barPC  copies("─", barPC * 2  % 1 )            /*show histogram.*/
     end   /*k*/
say
say 'sample size = ' size;          say
avg=  mean(size)         ;          say '       mean = '           format(avg, , showDigs)
std=stdDev(size)         ;          say '     stdDev = '           format(std, , showDigs)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
mean:   arg N;   $=0;    do m=1  for N;  $=$ + @.m;           end;     return      $/N
stdDev: arg N;   $=0;    do s=1  for N;  $=$ + (@.s-avg)**2;  end;     return sqrt($/N) /1
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x; if x=0  then return 0; d=digits(); m.=9; numeric form; h=d+6
      numeric digits;  parse value format(x,2,1,,0) 'E0'  with  g 'E' _ .;  g=g*.5'e'_ % 2
         do j=0  while h>9;      m.j=h;               h=h%2+1;        end /*j*/
         do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;   end /*k*/;  return g
```

{{out|output|text=  when using the default input of:     <tt> 100 </tt>}}

```txt

0  ──►0.1  12.00 ────────────────────────
0.1──►0.2  12.00 ────────────────────────
0.2──►0.3  10.00 ────────────────────
0.3──►0.4   8.00 ────────────────
0.4──►0.5  12.00 ────────────────────────
0.5──►0.6   8.00 ────────────────
0.6──►0.7  11.00 ──────────────────────
0.7──►0.8  11.00 ──────────────────────
0.8──►0.9   6.00 ────────────
0.9──►1    10.00 ────────────────────

sample size =  100

       mean =  0.4711358000
     stdDev =  0.2920169478

```

{{out|output|text=  when using the default input of:     <tt> 1000 </tt>}}

```txt

0  ──►0.1   9.50 ───────────────────
0.1──►0.2   9.90 ───────────────────
0.2──►0.3  11.70 ───────────────────────
0.3──►0.4   8.80 ─────────────────
0.4──►0.5   8.40 ────────────────
0.5──►0.6  10.20 ────────────────────
0.6──►0.7  10.30 ────────────────────
0.7──►0.8  11.40 ──────────────────────
0.8──►0.9   9.10 ──────────────────
0.9──►1    10.70 ─────────────────────

sample size =  1000

       mean =  0.5037752500
     stdDev =  0.2886365539

```

{{out|output|text=  when using the default input of:     <tt> 10000 </tt>}}

```txt

0  ──►0.1   9.61 ───────────────────
0.1──►0.2  10.45 ────────────────────
0.2──►0.3   9.96 ───────────────────
0.3──►0.4  10.56 ─────────────────────
0.4──►0.5   9.91 ───────────────────
0.5──►0.6  10.13 ────────────────────
0.6──►0.7  10.12 ────────────────────
0.7──►0.8   9.84 ───────────────────
0.8──►0.9   9.61 ───────────────────
0.9──►1     9.81 ───────────────────

sample size =  10000

       mean =  0.4968579550
     stdDev =  0.2863756713

```

{{out|output|text=  when using the default input of:     <tt> 100000 </tt>}}

```txt

0  ──►0.1  10.13 ────────────────────
0.1──►0.2   9.84 ───────────────────
0.2──►0.3   9.91 ───────────────────
0.3──►0.4   9.94 ───────────────────
0.4──►0.5  10.19 ────────────────────
0.5──►0.6  10.08 ────────────────────
0.6──►0.7  10.12 ────────────────────
0.7──►0.8   9.78 ───────────────────
0.8──►0.9  10.07 ────────────────────
0.9──►1     9.95 ───────────────────

sample size =  100000

       mean =  0.4999883642
     stdDev =  0.2884109515

```

{{out|output|text=  when using the default input of:     <tt> 1000000 </tt>}}

```txt

0  ──►0.1   9.94 ───────────────────
0.1──►0.2  10.03 ────────────────────
0.2──►0.3  10.03 ────────────────────
0.3──►0.4   9.98 ───────────────────
0.4──►0.5  10.00 ────────────────────
0.5──►0.6  10.03 ────────────────────
0.6──►0.7   9.99 ───────────────────
0.7──►0.8  10.03 ────────────────────
0.8──►0.9   9.97 ───────────────────
0.9──►1     9.99 ───────────────────

sample size =  1000000

       mean =  0.5000687045
     stdDev =  0.2885125537

```



## Ring


```ring

# Project : Statistics/Basic

decimals(9)
sample(100)
sample(1000)
sample(10000)
 
func sample(n)
       samp = list(n)
       for i =1 to n
           samp[i] =random(9)/10
       next 
       sum = 0
       sumSq = 0
       for i = 1 to n
            sum = sum + samp[i]
            sumSq	= sumSq +pow(samp[i],2)
       next
       see n + " Samples used." + nl 
       mean = sum / n
       see "Mean    = " + mean + nl 
       see "Std Dev = " + pow((sumSq /n -pow(mean,2)),0.5) + nl
       bins2 = 10
       bins = list(bins2)
       for i = 1 to n
            z = floor(bins2 * samp[i])
            if z != 0
               bins[z] = bins[z] +1
            ok
       next
       for b = 1 to bins2 
            see b + " " + nl
            for j = 1 to floor(bins2 *bins[b]) /n *70
                see "*"
            next
            see nl
       next
       see nl

```

Output:

```txt

100
Mean    = 0.482000000
Std Dev = 0.276904316
1
***************************************************************
2
********************************************************
3
********************************************************
4
*****************************************************************************
5
**********************************************************************
6
*****************************************************************************
7
***********************************************************************************************************************
8
********************************************************
9
**********************************************************************

1000
Mean    = 0.436600000
Std Dev = 0.284605762
1
*******************************************************************************
2
**********************************************************************
3
***********************************************************************************
4
************************************************************************
5
***********************************************************************
6
******************************************************************
7
*******************************************************
8
****************************************************************
9
**********************************************************************

10000
Mean    = 0.451940000
Std Dev = 0.287183280
1
********************************************************************
2
***********************************************************************
3
*******************************************************************
4
*********************************************************************
5
*********************************************************************
6
***********************************************************************
7
*********************************************************************
8
************************************************************************
9
*********************************************************************

```



## Ruby


```ruby
def generate_statistics(n)
  sum = sum2 = 0.0
  hist = Array.new(10, 0)
  n.times do
    r = rand
    sum += r
    sum2 += r**2
    hist[(10*r).to_i] += 1
  end
  mean = sum / n
  stddev = Math::sqrt((sum2 / n) - mean**2)
  
  puts "size: #{n}"
  puts "mean:   #{mean}"
  puts "stddev: #{stddev}"
  hist.each_with_index {|x,i| puts "%.1f:%s" % [0.1*i, "=" * (70*x/hist.max)]}
  puts
end

[100, 1000, 10000].each {|n| generate_statistics n}
```


{{out}}
<pre  style="height: 40ex; overflow: scroll">
size: 100
mean:   0.5565132836634081
stddev: 0.30678831716883026
0.0:
### ==========================

0.1:
### ======================================================

0.2:
### ==========================

0.3:
### ======================

0.4:
### ========================================

0.5:
### =================

0.6:
### ==================================================

0.7:
### ==================================================

0.8:
### ======================================================

0.9:
### ================================================================


size: 1000
mean:   0.4910962662424557
stddev: 0.28325915710008404
0.0:
### ================================================

0.1:
### ============================================

0.2:
### =================================================

0.3:
### ================================================================

0.4:
### ===============================================

0.5:
### ===========================================

0.6:
### ===========================================

0.7:
### =======================================================

0.8:
### ==========================================

0.9:
### ===========================================


size: 10000
mean:   0.5036461506004852
stddev: 0.28754747617166443
0.0:
### ========================================================

0.1:
### ===========================================================

0.2:
### ==============================================================

0.3:
### ==========================================================

0.4:
### ==========================================================

0.5:
### ===========================================================

0.6:
### ================================================================

0.7:
### =============================================================

0.8:
### =============================================================

0.9:
### ===========================================================


```




## Run BASIC


```runbasic
call sample    100
call sample   1000
call sample  10000
 
end
 
sub sample n
    dim samp(n)
    for i =1 to n
        samp(i) =rnd(1)
    next i
 
    ' calculate mean, standard deviation
    sum		= 0
    sumSq	= 0
    for i = 1 to n
        sum	= sum + samp(i)
        sumSq	= sumSq + samp(i)^2
    next i
    print n; " Samples used."
 
    mean	= sum / n
    print "Mean    = "; mean
 
    print "Std Dev = "; (sumSq /n -mean^2)^0.5
 
    '------- Show histogram
    bins = 10
    dim bins(bins)
    for i = 1 to n
        z	= int(bins * samp(i))
        bins(z) = bins(z) +1
    next i
    for b = 0 to bins -1
    print b;" ";
       for j = 1 to int(bins *bins(b)) /n *70
            print "*";
        next j
        print
    next b
    print
end sub
```

<pre  style="height: 40ex; overflow: scroll">
100 Samples used.
Mean    = 0.514312738
Std Dev = 0.291627558
0 **************************************************************************************************
1 **********************************************************************
2 *********************
3 ***********************************
4 ***************************************************************
5 *******************************************************************************************
6 ***********************************************************************************************************************
7 **********************************************************************
8 ***************************************************************
9 **********************************************************************

1000 Samples used.
Mean    = 0.495704208
Std Dev = 0.281389168
0 ***************************************************************
1 ********************************************************************
2 **************************************************************************
3 *******************************************************************************
4 **************************************************************************
5 **********************************************************************
6 ************************************************************************
7 **********************************************************************
8 ********************************************************
9 **********************************************************************

10000 Samples used.
Mean    = 0.493594211
Std Dev = 0.288635912
0 ************************************************************************
1 ************************************************************************
2 **********************************************************************
3 *******************************************************************
4 **********************************************************************
5 ************************************************************************
6 ************************************************************************
7 *****************************************************************
8 **********************************************************************
9 ******************************************************************

```



## Rust

{{libheader|rand}}

```rust
#![feature(iter_arith)]
extern crate rand;

use rand::distributions::{IndependentSample, Range};

pub fn mean(data: &[f32]) -> Option<f32> {
    if data.is_empty() {
        None
    } else {
        let sum: f32 = data.iter().sum();
        Some(sum / data.len() as f32)
    }
}

pub fn variance(data: &[f32]) -> Option<f32> {
    if data.is_empty() {
        None
    } else {
        let mean = mean(data).unwrap();
        let mut sum = 0f32;
        for &x in data {
            sum += (x - mean).powi(2);
        }
        Some(sum / data.len() as f32)
    }
}

pub fn standard_deviation(data: &[f32]) -> Option<f32> {
    if data.is_empty() {
        None
    } else {
        let variance = variance(data).unwrap();
        Some(variance.sqrt())
    }
}

fn print_histogram(width: u32, data: &[f32]) {
    let mut histogram = [0; 10];
    let len = histogram.len() as f32;
    for &x in data {
        histogram[(x * len) as usize] += 1;
    }
    let max_frequency = *histogram.iter().max().unwrap() as f32;
    for (i, &frequency) in histogram.iter().enumerate() {
        let bar_width = frequency as f32 * width as f32 / max_frequency;
        print!("{:3.1}: ", i as f32 / len);
        for _ in 0..bar_width as usize {
            print!("*");
        }
        println!("");
    }
}

fn main() {
    let range = Range::new(0f32, 1f32);
    let mut rng = rand::thread_rng();

    for &number_of_samples in [1000, 10_000, 1_000_000].iter() {
        let mut data = vec![];
        for _ in 0..number_of_samples {
            let x = range.ind_sample(&mut rng);
            data.push(x);
        }
        println!("  Statistics for sample size {}", number_of_samples);
        println!("Mean:               {:?}", mean(&data));
        println!("Variance:           {:?}", variance(&data));
        println!("Standard deviation: {:?}", standard_deviation(&data));
        print_histogram(40, &data);
    }
}
```

{{out}}

```txt
  Statistics for sample size 1000
Mean:               Some(0.50145197)
Variance:           Some(0.08201705)
Standard deviation: Some(0.2863862)
0.0: *********************************
0.1: ****************************
0.2: **********************************
0.3: ************************************
0.4: **************************************
0.5: *********************************
0.6: ******************************
0.7: ******************************
0.8: ****************************************
0.9: ******************************
  Statistics for sample size 10000
Mean:               Some(0.49700406)
Variance:           Some(0.08357173)
Standard deviation: Some(0.28908777)
0.0: **************************************
0.1: ***************************************
0.2: ***************************************
0.3: ***************************************
0.4: ***********************************
0.5: ***************************************
0.6: *************************************
0.7: ****************************************
0.8: **************************************
0.9: *************************************
  Statistics for sample size 1000000
Mean:               Some(0.50038373)
Variance:           Some(0.08325759)
Standard deviation: Some(0.2885439)
0.0: ***************************************
0.1: ***************************************
0.2: ***************************************
0.3: ****************************************
0.4: ***************************************
0.5: ***************************************
0.6: ***************************************
0.7: ***************************************
0.8: ***************************************
0.9: ***************************************
```



## Scala


```scala
def mean(a:Array[Double])=a.sum / a.size
def stddev(a:Array[Double])={
   val sum = a.fold(0.0)((a, b) => a + math.pow(b,2))
   math.sqrt((sum/a.size) - math.pow(mean(a),2))
}
def hist(a:Array[Double]) = {
   val grouped=(SortedMap[Double, Array[Double]]() ++ (a groupBy (x => math.rint(x*10)/10)))
   grouped.map(v => (v._1, v._2.size))
}
def printHist(a:Array[Double])=for((g,v) <- hist(a)){
   println(s"$g: ${"*"*(205*v/a.size)} $v")
}

for(n <- Seq(100,1000,10000)){
   val a = Array.fill(n)(Random.nextDouble)
   println(s"$n numbers")
   println(s"Mean: ${mean(a)}")
   println(s"StdDev: ${stddev(a)}")
   printHist(a)
   println
}
```

{{out}}

```txt
100 numbers
Mean: 0.5151424022100874
StdDev: 0.25045766440922146
0.0: **** 2
0.1: **************** 8
0.2: **************** 8
0.3: ******************** 10
0.4: ************************ 12
0.5: ****************************** 15
0.6: ****************************** 15
0.7: **************** 8
0.8: ******************** 10
0.9: ********************** 11
1.0: ** 1

1000 numbers
Mean: 0.4954605718792786
StdDev: 0.28350795290401604
0.0: ********* 48
0.1: ******************* 93
0.2: *********************** 117
0.3: ******************** 99
0.4: ***************** 87
0.5: ********************** 108
0.6: ************************* 122
0.7: ****************** 88
0.8: ******************** 100
0.9: ****************** 88
1.0: ********** 50

10000 numbers
Mean: 0.502395544726441
StdDev: 0.2874443665645294
0.0: ********** 496
0.1: ******************** 979
0.2: ******************* 962
0.3: ******************** 1010
0.4: ******************** 998
0.5: ********************* 1035
0.6: ******************** 984
0.7: ********************* 1031
0.8: ********************* 1027
0.9: ******************** 991
1.0: ********* 487
```



## Sidef

{{trans|Ruby}}

```ruby
func generate_statistics(n) {
    var(sum=0, sum2=0);
    var hist = 10.of(0);

    n.times {
        var r = 1.rand;
        sum += r;
        sum2 += r**2;
        hist[10*r] += 1;
    }

    var mean = sum/n;
    var stddev = Math.sqrt(sum2/n - mean**2);

    say "size: #{n}";
    say "mean:   #{mean}";
    say "stddev: #{stddev}";

    var max = hist.max;
    hist.range.each {|i|
        printf("%.1f:%s\n", 0.1*i, "=" * 70*hist[i]/max);
    }
    print "\n";
}

[100, 1000, 10000].each {|n| generate_statistics(n) }
```

{{out}}
<pre  style="height: 40ex; overflow: scroll">
size: 100
mean:   0.4585051431752446588
stddev: 0.2870559459562831101619581273667538623484
0.0:
### ===========================================================

0.1:
### ============================================

0.2:
### ================================================================

0.3:
### =======================================

0.4:
### =================================================

0.5:
### ========================

0.6:
### ============================================

0.7:
### ============================================

0.8:
### ============================================

0.9:
### =============================


size: 1000
mean:   0.51292239343467439552
stddev: 0.2832968595790956540009121237087699143503
0.0:
### =============================================

0.1:
### ==================================================

0.2:
### ==================================================

0.3:
### ==================================================

0.4:
### ================================================================

0.5:
### ============================================================

0.6:
### =========================================================

0.7:
### ===================================================

0.8:
### ==================================================

0.9:
### ==============================================================


size: 10000
mean:   0.49883638025449614521145
stddev: 0.2898083000452161646017460189689302069547
0.0:
### ==============================================================

0.1:
### ======================================================

0.2:
### ================================================================

0.3:
### ========================================================

0.4:
### =========================================================

0.5:
### ===========================================================

0.6:
### =========================================================

0.7:
### ===========================================================

0.8:
### ============================================================

0.9:
### =========================================================


```



## Stata


For a uniform distribution on [0,1], the mean is 1/2 and the variance is 1/12 (hence the standard deviation is 0.28867513). With a large sample, one can check the convergence to these values.


```stata
. clear all
. set obs 100000
number of observations (_N) was 0, now 100,000
. gen x=runiform()
. summarize x

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
           x |    100,000    .4991874    .2885253   1.18e-06   .9999939
. hist x
```



## Tcl


```tcl
package require Tcl 8.5
proc stats {size} {
    set sum 0.0
    set sum2 0.0
    for {set i 0} {$i < $size} {incr i} {
	set r [expr {rand()}]

	incr histo([expr {int(floor($r*10))}])
	set sum [expr {$sum + $r}]
	set sum2 [expr {$sum2 + $r**2}]
    }
    set mean [expr {$sum / $size}]
    set stddev [expr {sqrt($sum2/$size - $mean**2)}]
    puts "$size numbers"
    puts "Mean:   $mean"
    puts "StdDev: $stddev"
    foreach i {0 1 2 3 4 5 6 7 8 9} {
	# The 205 is a magic factor stolen from the Go solution
	puts [string repeat "*" [expr {$histo($i)*205/int($size)}]]
    }
}

stats 100
puts ""
stats 1000
puts ""
stats 10000
```

{{out}}

```txt

100 numbers
Mean:   0.4801193240797704
StdDev: 0.28697057708153784
**************
**********************************
********************
**************
****************************
****************
**************
****************************
****************
****************

1000 numbers
Mean:   0.49478823525495275
StdDev: 0.2821543810265757
*******************
******************
************************
********************
*******************
**********************
*********************
********************
******************
******************

10000 numbers
Mean:   0.49928563715870816
StdDev: 0.2888258479070212
********************
*********************
********************
********************
*******************
*********************
*******************
********************
*********************
********************

```

As can be seen, increasing the sample size reduces the variation between the buckets, showing that the <code>rand()</code> function at least approximates a uniform distribution. (Because Tcl 8.5 supports arbitrary precision integer arithmetic there is no reason in principle why the details for a trillion numbers couldn't be calculated, but it would take quite a while.)


## VBA


```vb
Option Base 1
Private Function mean(s() As Variant) As Double
    mean = WorksheetFunction.Average(s)
End Function
Private Function standard_deviation(s() As Variant) As Double
    standard_deviation = WorksheetFunction.StDev(s)
End Function
Public Sub basic_statistics()
    Dim s() As Variant
    For e = 2 To 4
        ReDim s(10 ^ e)
        For i = 1 To 10 ^ e
            s(i) = Rnd()
        Next i
        Debug.Print "sample size"; UBound(s), "mean"; mean(s), "standard deviation"; standard_deviation(s)
        t = WorksheetFunction.Frequency(s, [{0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0}])
        For i = 1 To 10
            Debug.Print Format((i - 1) / 10, "0.00");
            Debug.Print "-"; Format(i / 10, "0.00"),
            Debug.Print String$(t(i, 1) / (10 ^ (e - 2)), "X");
            Debug.Print
        Next i
        Debug.Print
    Next e
End Sub
```
{{out}}

```txt
sample size 100             mean 0,472405961751938      standard deviation 0,260463885857138 
0,00-0,10     XXXXXX
0,10-0,20     XXXXXXXXX
0,20-0,30     XXXXXXXXXXXXXXX
0,30-0,40     XXXXXXXXXXXXXXX
0,40-0,50     XXXXXXXXXXXXXX
0,50-0,60     XXXXXXX
0,60-0,70     XXXXXXXXXXX
0,70-0,80     XXXXXXXX
0,80-0,90     XXXXXXXXXX
0,90-1,00     XXXXX

sample size 1000            mean 0,500459910154343      standard deviation 0,278991757028358 
0,00-0,10     XXXXXXXX
0,10-0,20     XXXXXXXXXX
0,20-0,30     XXXXXXXXXX
0,30-0,40     XXXXXXXXXX
0,40-0,50     XXXXXXXXXX
0,50-0,60     XXXXXXXXXXXX
0,60-0,70     XXXXXXXXXXX
0,70-0,80     XXXXXXXXX
0,80-0,90     XXXXXXXXX
0,90-1,00     XXXXXXXXXX

sample size 10000           mean 0,496753623914719      standard deviation 0,28740805585887 
0,00-0,10     XXXXXXXXXX
0,10-0,20     XXXXXXXXXX
0,20-0,30     XXXXXXXXXX
0,30-0,40     XXXXXXXXXX
0,40-0,50     XXXXXXXXXX
0,50-0,60     XXXXXXXXXX
0,60-0,70     XXXXXXXXXX
0,70-0,80     XXXXXXXXXX
0,80-0,90     XXXXXXXXXX
0,90-1,00     XXXXXXXXXX
```


## zkl


```zkl
fcn mean(ns)  { ns.sum(0.0)/ns.len() }
fcn stdDev(ns){ 
   m:=mean(ns); (ns.reduce('wrap(p,n){ x:=(n-m); p+x*x },0.0)/ns.len()).sqrt() 
}
```


```zkl
reg ns;
foreach n in (T(100,1000,10000)){
   ns=(0).pump(n,List,(0.0).random.fp(1.0));
   println("N:%,6d  mean:%.5f std dev:%.5f".fmt(n,mean(ns),stdDev(ns)));
}
foreach r in ([0.0 .. 0.9, 0.1]){  // using the last data set (10000 randoms)
   n:=ns.filter('wrap(x){ r<=x<(r+0.1) }).len();
   println("%.2f..%.2f:%4d%s".fmt(r,r+0.1,n,"*"*(n/20)));
}
```

(0.0).random(1.0) generates a [uniform] random number between 0 (inclusive) and 1 (exclusive).
{{out}}

```txt

N:   100  mean:0.48521 std dev:0.27073
N: 1,000  mean:0.49362 std dev:0.28921
N:10,000  mean:0.49899 std dev:0.28813
0.00..0.10: 986*************************************************
0.10..0.20:1043****************************************************
0.20..0.30: 992*************************************************
0.30..0.40: 974************************************************
0.40..0.50:1001**************************************************
0.50..0.60: 998*************************************************
0.60..0.70: 995*************************************************
0.70..0.80:1043****************************************************
0.80..0.90:1005**************************************************
0.90..1.00: 963************************************************

```

For the extra credit, pretend we have a device that spews random numbers in the range [0..1) forever. We connect this device to a measuring device that calculates mean and std deviation, printing results on a regular basis. 

```zkl
var pipe=Thread.Pipe(); // used to connect the two threads
fcn{ while(1){ pipe.write((0.0).random(1.0)) } }.launch();  // generator
fcn{    // consumer/calculator
   N:=0; M:=SD:=sum:=ssum:=0.0; 
   while(1){
      x:=pipe.read(); N+=1; sum+=x; ssum+=x*x; 
      M=sum/N; SD=(ssum/N - M*M).sqrt();
      if(0==N%100000) 
	 println("N:%,10d  mean:%.5f std dev:%.5f".fmt(N,M,SD));
   }
}.launch();

Atomic.sleep(60*60);  // wait because exiting the VM kills the threads
```

{{out}}

```txt

...
N:45,800,000  mean:0.49997 std dev:0.28869
N:45,900,000  mean:0.49997 std dev:0.28869
N:46,000,000  mean:0.49997 std dev:0.28869
N:46,100,000  mean:0.49998 std dev:0.28869
N:46,200,000  mean:0.49997 std dev:0.28870
N:46,300,000  mean:0.49997 std dev:0.28870
N:46,400,000  mean:0.49997 std dev:0.28870
...

```

