+++
title = "Probabilistic choice"
description = ""
date = 2019-03-17T21:30:13Z
aliases = []
[extra]
id = 3120
[taxonomies]
categories = []
tags = []
+++

{{task|Probability and statistics}}
Given a mapping between items and their required probability of occurrence, generate a million items ''randomly'' subject to the given probabilities and compare the target probability of occurrence versus the generated values.

The total of all the probabilities should equal one. (Because floating point arithmetic is involved, this is subject to rounding errors).

Use the following mapping to test your programs:
```txt

aleph   1/5.0
beth    1/6.0
gimel   1/7.0
daleth  1/8.0
he      1/9.0
waw     1/10.0
zayin   1/11.0
heth    1759/27720 # adjusted so that probabilities add to 1
```


;Related task:
* [[Random number generator (device)]]





## Ada


```ada
with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Random_Distribution is
   Trials : constant := 1_000_000;
   type Outcome is (Aleph, Beth, Gimel, Daleth, He, Waw, Zayin, Heth);
   Pr : constant array (Outcome) of Uniformly_Distributed :=
        (1.0/5.0, 1.0/6.0, 1.0/7.0, 1.0/8.0, 1.0/9.0, 1.0/10.0, 1.0/11.0, 1.0);
   Samples : array (Outcome) of Natural := (others => 0);
   Value   : Uniformly_Distributed;
   Dice    : Generator;
begin
   for Try in 1..Trials loop
      Value := Random (Dice);
      for I in Pr'Range loop
         if Value <= Pr (I) then
            Samples (I) := Samples (I) + 1;
            exit;
         else
            Value := Value - Pr (I);
         end if;
      end loop;
   end loop;
      -- Printing the results
   for I in Pr'Range loop
      Put (Outcome'Image (I) & Character'Val (9));
      Put (Float'Image (Float (Samples (I)) / Float (Trials)) & Character'Val (9));
      if I = Heth then
         Put_Line (" rest");
      else
         Put_Line (Uniformly_Distributed'Image (Pr (I)));
      end if;
   end loop;
end Random_Distribution;
```

Sample output:

```txt

ALEPH    2.00167E-01     2.00000E-01
BETH     1.67212E-01     1.66667E-01
GIMEL    1.42290E-01     1.42857E-01
DALETH   1.24186E-01     1.25000E-01
HE       1.11455E-01     1.11111E-01
WAW      1.00325E-01     1.00000E-01
ZAYIN    9.10220E-02     9.09091E-02
HETH     6.33430E-02     rest

```



## ALGOL 68

{{trans|C}}

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
INT trials = 1 000 000;

MODE LREAL = LONG REAL;

MODE ITEM = STRUCT(
  STRING name,
  INT prob count,
  LREAL expect,
        mapping
);
INT col width = 9;
FORMAT real repr = $g(-col width+1, 6)$,
       item repr = $"Name: "g", Prob count: "g(0)", Expect: "f(real repr)", Mapping: ", f(real repr)l$;

[8]ITEM items := (
  ( "aleph",  0, ~, ~ ),
  ( "beth",   0, ~, ~ ),
  ( "gimel",  0, ~, ~ ),
  ( "daleth", 0, ~, ~ ),
  ( "he",     0, ~, ~ ),
  ( "waw",    0, ~, ~ ),
  ( "zayin",  0, ~, ~ ),
  ( "heth",   0, ~, ~ )
);

main:
(
  LREAL offset = 5; # const #

# initialise items #
  LREAL total sum := 0;
  FOR i FROM LWB items TO UPB items - 1 DO
    expect OF items[i] := 1/(i-1+offset);
    total sum +:= expect OF items[i]
  OD;
  expect OF items[UPB items] := 1 - total sum;

  mapping OF items[LWB items] := expect OF items[LWB items];
  FOR i FROM LWB items + 1 TO UPB items DO
    mapping OF items[i] := mapping OF items[i-1] + expect OF items[i]
  OD;

  # printf((item repr, items)) #

# perform the sampling #
  PROC sample = (REF[]LREAL mapping)INT:(
    INT out;
    LREAL rand real = random;
    FOR j FROM LWB items TO UPB items DO
      IF rand real < mapping[j] THEN
        out := j;
	done
      FI
    OD;
    done: out
  );

  FOR i TO trials DO
      prob count OF items[sample(mapping OF items)] +:= 1
  OD;

  FORMAT indent = $17k$;

# print the results #
  printf(($"Trials: "g(0)l$, trials));
  printf(($"Items:"$,indent));
  FOR i FROM LWB items TO UPB items DO printf(($gn(col width)k" "$, name OF items[i])) OD;
  printf(($l"Target prob.:"$, indent, $f(real repr)" "$, expect OF items));
  printf(($l"Attained prob.:"$, indent));
  FOR i FROM LWB items TO UPB items DO printf(($f(real repr)" "$, prob count OF items[i]/trials)) OD;
  printf($l$)
)
```

Sample output:

```txt

Trials: 1000000
Items:          aleph    beth     gimel    daleth   he       waw      zayin    heth
Target prob.:   0.200000 0.166667 0.142857 0.125000 0.111111 0.100000 0.090909 0.063456
Attained prob.: 0.199987 0.166917 0.142531 0.124203 0.111338 0.099702 0.091660 0.063662

```



## AutoHotkey

contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276279.html#276279 forum]

```AutoHotkey
s1 := "aleph",   p1 := 1/5.0                       ; Input
s2 := "beth",    p2 := 1/6.0
s3 := "gimel",   p3 := 1/7.0
s4 := "daleth",  p4 := 1/8.0
s5 := "he",      p5 := 1/9.0
s6 := "waw",     p6 := 1/10.0
s7 := "zayin",   p7 := 1/11.0
s8 := "heth",    p8 := 1-p1-p2-p3-p4-p5-p6-p7
n := 8, r0 := 0, r%n% := 1                         ; auxiliary data

Loop % n-1
   i := A_Index-1, r%A_Index% := r%i% + p%A_Index% ; cummulative distribution

Loop 1000000 {
   Random R, 0, 1.0
   Loop %n%                                        ; linear search
      If (R < r%A_Index%) {
          c%A_Index%++
          Break
      }
}
                                                   ; Output
Loop %n%
   t .= s%A_Index% "`t" p%A_Index% "`t" c%A_Index%*1.0e-6 "`n"
Msgbox %t%

/*
output:
---------------------------
aleph  0.200000   0.199960
beth   0.166667   0.166146
gimel  0.142857   0.142624
daleth 0.125000   0.124924
he     0.111111   0.111226
waw    0.100000   0.100434
zayin  0.090909   0.091344
heth   0.063456   0.063342
---------------------------
*/
```



## AWK



```awk
#!/usr/bin/awk -f

BEGIN {
    ITERATIONS = 1000000
    delete symbMap
    delete probMap
    delete counts
    initData();

    for (i = 0; i < ITERATIONS; i++) {
        distribute(rand())
    }
    showDistributions()

    exit
}

function distribute(rnd,    cnt, symNum, sym, symPrb) {
    cnt = length(symbMap)
    for (symNum = 1; symNum <= cnt; symNum++) {
        sym = symbMap[symNum];
        symPrb = probMap[sym];
        rnd -= symPrb;
        if (rnd <= 0) {
            counts[sym]++
            return;
        }
    }
}

function showDistributions(    s, sym, prb, actSum, expSum, totItr) {
    actSum = 0.0
    expSum = 0.0
    totItr = 0
    printf "%-7s  %-7s  %-5s  %-5s\n", "symb", "num.", "act.", "expt."
    print  "-------  -------  -----  -----"
    for (s = 1; s <= length(symbMap); s++) {
        sym = symbMap[s]
        prb = counts[sym]/ITERATIONS
        actSum += prb
        expSum += probMap[sym]
        totItr += counts[sym]
        printf "%-7s  %7d  %1.3f  %1.3f\n", sym, counts[sym], prb, probMap[sym]
    }
    print  "-------  -------  -----  -----"
    printf "Totals:  %7d  %1.3f  %1.3f\n", totItr, actSum, expSum
}

function initData(    sym) {
    srand()

    probMap["aleph"]  = 1.0 / 5.0
    probMap["beth"]   = 1.0 / 6.0
    probMap["gimel"]  = 1.0 / 7.0
    probMap["daleth"] = 1.0 / 8.0
    probMap["he"]     = 1.0 / 9.0
    probMap["waw"]    = 1.0 / 10.0
    probMap["zyin"]   = 1.0 / 11.0
    probMap["heth"]   = 1759.0 / 27720.0

    symbMap[1] = "aleph"
    symbMap[2] = "beth"
    symbMap[3] = "gimel"
    symbMap[4] = "daleth"
    symbMap[5] = "he"
    symbMap[6] = "waw"
    symbMap[7] = "zyin"
    symbMap[8] = "heth"

    for (sym in probMap)
        counts[sym] = 0;
}

```


Example output:

 symb     num.     act.   expt.
 -------  -------  -----  -----
 aleph     200598  0.201  0.200
 beth      166317  0.166  0.167
 gimel     142391  0.142  0.143
 daleth    125051  0.125  0.125
 he        110658  0.111  0.111
 waw       100464  0.100  0.100
 zyin       90649  0.091  0.091
 heth       63872  0.064  0.063
 -------  -------  -----  -----
 Totals:  1000000  1.000  1.000

Rounding off makes the results look perfect.


## BBC BASIC


```bbcbasic
      DIM item$(7), prob(7), cnt%(7)
      item$() = "aleph","beth","gimel","daleth","he","waw","zayin","heth"
      prob()  = 1/5.0, 1/6.0, 1/7.0, 1/8.0, 1/9.0, 1/10.0, 1/11.0, 1759/27720
      IF ABS(SUM(prob())-1) > 1E-6 ERROR 100, "Probabilities don't sum to 1"

      FOR trial% = 1 TO 1E6
        r = RND(1)
        p = 0
        FOR i% = 0 TO DIM(prob(),1)
          p += prob(i%)
          IF r < p cnt%(i%) += 1 : EXIT FOR
        NEXT
      NEXT

      @% = &2060A
      PRINT "Item        actual    theoretical"
      FOR i% = 0 TO DIM(item$(),1)
        PRINT item$(i%), cnt%(i%)/1E6, prob(i%)
      NEXT
```

'''Output:'''

```txt

Item        actual    theoretical
aleph       0.200306  0.200000
beth        0.165963  0.166667
gimel       0.143089  0.142857
daleth      0.125387  0.125000
he          0.111057  0.111111
waw         0.100098  0.100000
zayin       0.091031  0.090909
heth        0.063069  0.063456

```



## C


```c
#include <stdio.h>
#include <stdlib.h>

/* pick a random index from 0 to n-1, according to probablities listed
   in p[] which is assumed to have a sum of 1. The values in the probablity
   list matters up to the point where the sum goes over 1 */
int rand_idx(double *p, int n)
{
	double s = rand() / (RAND_MAX + 1.0);
	int i;
	for (i = 0; i < n - 1 && (s -= p[i]) >= 0; i++);
	return i;
}

#define LEN 8
#define N 1000000
int main()
{
	const char *names[LEN] = { "aleph", "beth", "gimel", "daleth",
			  "he", "waw", "zayin", "heth" };
	double s, p[LEN] = { 1./5, 1./6, 1./7, 1./8, 1./9, 1./10, 1./11, 1e300 };
	int i, count[LEN] = {0};

	for (i = 0; i < N; i++) count[rand_idx(p, LEN)] ++;

	printf("  Name  Count    Ratio Expected\n");
	for (i = 0, s = 1; i < LEN; s -= p[i++])
		printf("%6s%7d %7.4f%% %7.4f%%\n",
			names[i], count[i],
			(double)count[i] / N * 100,
			((i < LEN - 1) ? p[i] : s) * 100);

	return 0;
}
```
output<lang>  Name  Count    Ratio Expected
 aleph 199928 19.9928% 20.0000%
  beth 166489 16.6489% 16.6667%
 gimel 143211 14.3211% 14.2857%
daleth 125257 12.5257% 12.5000%
    he 110849 11.0849% 11.1111%
   waw  99935  9.9935% 10.0000%
 zayin  91001  9.1001%  9.0909%
  heth  63330  6.3330%  6.3456%
```



## C++


```cpp
#include <cstdlib>
#include <iostream>
#include <vector>
#include <utility>
#include <algorithm>
#include <ctime>
#include <iomanip>

int main( ) {
   typedef std::vector<std::pair<std::string, double> >::const_iterator SPI ;
   typedef std::vector<std::pair<std::string , double> > ProbType ;
   ProbType probabilities ;
   probabilities.push_back( std::make_pair( "aleph" , 1/5.0 ) ) ;
   probabilities.push_back( std::make_pair( "beth" , 1/6.0 ) ) ;
   probabilities.push_back( std::make_pair( "gimel" , 1/7.0 ) ) ;
   probabilities.push_back( std::make_pair( "daleth" , 1/8.0 ) ) ;
   probabilities.push_back( std::make_pair( "he" , 1/9.0 ) ) ;
   probabilities.push_back( std::make_pair( "waw" , 1/10.0 ) ) ;
   probabilities.push_back( std::make_pair( "zayin" , 1/11.0 ) ) ;
   probabilities.push_back( std::make_pair( "heth" , 1759/27720.0 ) ) ;
   std::vector<std::string> generated ; //for the strings that are generatod
   std::vector<int> decider ; //holds the numbers that determine the choice of letters
   for ( int i = 0 ; i < probabilities.size( ) ; i++ ) {
      if ( i == 0 ) {
	 decider.push_back( 27720 * (probabilities[ i ].second) ) ;
      }
      else {
	 int number = 0 ;
	 for ( int j = 0 ; j < i ; j++ ) {
	    number +=  27720 * ( probabilities[ j ].second ) ;
	 }
	 number += 27720 * probabilities[ i ].second ;
	 decider.push_back( number ) ;
      }
   }
   srand( time( 0 ) ) ;
   for ( int i = 0 ; i < 1000000 ; i++ ) {
      int randnumber = rand( ) % 27721 ;
      int j = 0 ;
      while ( randnumber > decider[ j ] )
	 j++ ;
      generated.push_back( ( probabilities[ j ]).first ) ;
   }
   std::cout << "letter  frequency attained   frequency expected\n" ;
   for ( SPI i = probabilities.begin( ) ; i != probabilities.end( ) ; i++ ) {
      std::cout << std::left << std::setw( 8 ) << i->first ;
      int found = std::count ( generated.begin( ) , generated.end( ) , i->first ) ;
      std::cout << std::left << std::setw( 21 ) << found / 1000000.0 ;
      std::cout << std::left << std::setw( 17 ) << i->second << '\n' ;
   }
   return 0 ;
}
```

Output:
<PRE>letter  frequency attained   frequency expected
aleph   0.200089             0.2
beth    0.16695              0.166667
gimel   0.142693             0.142857
daleth  0.124859             0.125
he      0.111258             0.111111
waw     0.099665             0.1
zayin   0.090654             0.0909091
heth    0.063832             0.063456
</PRE>

=={{header|C sharp|C#}}==

{{trans|Java}}


```csharp

using System;

class Program
{
    static long TRIALS = 1000000L;
    private class Expv
    {
        public string name;
        public int probcount;
        public double expect;
        public double mapping;

        public Expv(string name, int probcount, double expect, double mapping)
        {
            this.name = name;
            this.probcount = probcount;
            this.expect = expect;
            this.mapping = mapping;
        }
    }

    static Expv[] items = {
        new Expv("aleph", 0, 0.0, 0.0), new Expv("beth", 0, 0.0, 0.0),
        new Expv("gimel", 0, 0.0, 0.0), new Expv("daleth", 0, 0.0, 0.0),
	new Expv("he", 0, 0.0, 0.0),    new Expv("waw", 0, 0.0, 0.0),
	new Expv("zayin", 0, 0.0, 0.0), new Expv("heth", 0, 0.0, 0.0)
    };

    static void Main(string[] args)
    {
        double rnum, tsum = 0.0;
        Random random = new Random();

        for (int i = 0, rnum = 5.0; i < 7; i++, rnum += 1.0)
        {
            items[i].expect = 1.0 / rnum;
            tsum += items[i].expect;
        }
        items[7].expect = 1.0 - tsum;

        items[0].mapping = 1.0 / 5.0;
        for (int i = 1; i < 7; i++)
            items[i].mapping = items[i - 1].mapping + 1.0 / ((double)i + 5.0);
        items[7].mapping = 1.0;

        for (int i = 0; i < TRIALS; i++)
        {
            rnum = random.NextDouble();
            for (int j = 0; j < 8; j++)
                if (rnum < items[j].mapping)
                {
                    items[j].probcount++;
                    break;
                }
        }

        Console.WriteLine("Trials: {0}", TRIALS);
        Console.Write("Items:          ");
        for (int i = 0; i < 8; i++)
            Console.Write(items[i].name.PadRight(9));
        Console.WriteLine();
        Console.Write("Target prob.:   ");
        for (int i = 0; i < 8; i++)
            Console.Write("{0:0.000000} ", items[i].expect);
        Console.WriteLine();
        Console.Write("Attained prob.: ");
        for (int i = 0; i < 8; i++)
            Console.Write("{0:0.000000} ", (double)items[i].probcount / (double)TRIALS);
        Console.WriteLine();
    }
}

```


Output:


```txt
Trials: 1000000
Items:          aleph    beth     gimel    daleth   he       waw      zayin    heth
Target prob.:   0.200000 0.166667 0.142857 0.125000 0.111111 0.100000 0.090909 0.063456
Attained prob.: 0.199975 0.166460 0.142290 0.125510 0.111374 0.100018 0.090746 0.063627
```



## Clojure

Works by first converting the provided Probability Distribution Function into a Cumulative Distribution Function, so that it can simply scan through the CDF list and return the current item as soon as the CDF at that point is greater than the random number generated. The code could be made more concise by skipping this step and instead tracking the whole PDF for each random number; but this code is both faster and more readable.

It uses the language built-in (frequencies) to count the number of occurrences of each distinct name. Note that while we actually generate a sequence of num-trials random samples, the sequence is lazily generated and lazily consumed. This means that the program will scale to an arbitrarily-large num-trials with no ill effects, by throwing away elements it's already processed.


```Clojure
(defn to-cdf [pdf]
  (reduce
    (fn [acc n] (conj acc (+ (or (last acc) 0) n)))
    []
    pdf))

(defn choose [cdf]
  (let [r (rand)]
    (count
      (filter (partial > r) cdf))))

(def *names* '[aleph beth gimel daleth he waw zayin heth])
(def *pdf* (map double [1/5 1/6 1/7 1/8 1/9 1/10 1/11 1759/27720]))

(let [num-trials 1000000
      cdf (to-cdf *pdf*)
      indexes (range (count *names*)) ;; use integer key internally, not name
      expected (into (sorted-map) (zipmap indexes *pdf*))
      actual (frequencies (repeatedly num-trials #(choose cdf)))]
  (doseq [[idx exp] expected]
    (println "Expected number of" (*names* idx) "was"
             (* num-trials exp) "and actually got" (actual idx))))
```



```txt
Expected number of aleph was 200000.0 and actually got 199300
Expected number of beth was 166666.66666666672 and actually got 166291
Expected number of gimel was 142857.1428571429 and actually got 143297
Expected number of daleth was 125000.0 and actually got 125032
Expected number of he was 111111.11111111111 and actually got 111540
Expected number of waw was 100000.0 and actually got 100062
Expected number of zayin was 90909.09090909091 and actually got 90719
Expected number of heth was 63455.98845598846 and actually got 63759
```



## Common Lisp


This is a straightforward, if a little verbose implementation based upon the Perl one.

```lisp
(defvar *probabilities* '((aleph  1/5)
                          (beth   1/6)
                          (gimel  1/7)
                          (daleth 1/8)
                          (he     1/9)
                          (waw    1/10)
                          (zayin  1/11)
                          (heth   1759/27720)))
(defun calculate-probabilities (choices &key (repetitions 1000000))
  (assert (= 1 (reduce #'+ choices :key #'second)))
  (labels ((make-ranges ()
             (loop for (datum probability) in choices
                   sum (coerce probability 'double-float) into total
                   collect (list datum total)))
           (pick (ranges)
             (declare (optimize (speed 3) (safety 0) (debug 0)))
             (loop with random = (random 1.0d0)
                   for (datum below) of-type (t double-float) in ranges
                   when (< random below)
                     do (return datum)))
           (populate-hash (ranges)
             (declare (optimize (speed 3) (safety 0) (debug 0)))
             (loop repeat (the fixnum repetitions)
                   with hash = (make-hash-table)
                   do (incf (the fixnum (gethash (pick ranges) hash 0)))
                   finally (return hash)))
           (make-table-data (hash)
             (loop for (datum probability) in choices
                   collect (list datum
                                 (float (/ (gethash datum hash)
                                           repetitions))
                                 (float probability)))))
    (format t "Datum~10,2TOccured~20,2TExpected~%")
    (format t "~{~{~A~10,2T~F~20,2T~F~}~%~}"
                 (make-table-data (populate-hash (make-ranges))))))

CL-USER> (calculate-probabilities *probabilities*)
Datum     Occured   Expected
ALEPH     0.200156  0.2
BETH      0.166521  0.16666667
GIMEL     0.142936  0.14285715
DALETH    0.124779  0.125
HE        0.111601  0.11111111
WAW       0.100068  0.1
ZAYIN     0.090458  0.09090909
HETH      0.063481  0.06345599
```



## D


### Basic Version


```d
void main() {
  import std.stdio, std.random, std.string, std.range;

  enum int nTrials = 1_000_000;
  const items = "aleph beth gimel daleth he waw zayin heth".split;
  const pr = [1/5., 1/6., 1/7., 1/8., 1/9., 1/10., 1/11., 1759/27720.];

  double[pr.length] counts = 0.0;
  foreach (immutable _; 0 .. nTrials)
    counts[pr.dice]++;

  writeln("Item    Target prob  Attained prob");
  foreach (name, p, co; zip(items, pr, counts[]))
    writefln("%-7s %.8f   %.8f", name, p, co / nTrials);
}
```

{{out}}

```txt
Item    Target prob  Attained prob
aleph   0.20000000   0.19964000
beth    0.16666667   0.16753600
gimel   0.14285714   0.14283300
daleth  0.12500000   0.12515400
he      0.11111111   0.11074300
waw     0.10000000   0.10025800
zayin   0.09090909   0.09070400
heth    0.06345598   0.06313200
```



### A Faster Version


```d
void main() {
  import std.stdio, std.random, std.algorithm, std.range;

  enum int nTrials = 1_000_000;
  const items = "aleph beth gimel daleth he waw zayin heth".split;
  const pr = [1/5., 1/6., 1/7., 1/8., 1/9., 1/10., 1/11., 1759/27720.];

  double[pr.length] cumulatives = pr[];
  foreach (immutable i, ref c; cumulatives[1 .. $ - 1])
    c += cumulatives[i];
  cumulatives[$ - 1] = 1.0;

  double[pr.length] counts = 0.0;
  auto rnd = Xorshift(unpredictableSeed);
  foreach (immutable _; 0 .. nTrials)
    counts[cumulatives[].countUntil!(c => c >= rnd.uniform01)]++;

  writeln("Item    Target prob  Attained prob");
  foreach (name, p, co; zip(items, pr, counts[]))
    writefln("%-7s %.8f   %.8f", name, p, co / nTrials);
}
```



## E


This implementation converts the list of probabilities to sub-intervals of [0.0,1.0), then arranges those intervals in a binary tree for searching based on a random number input.

It is rather verbose, due to using the tree rather than a linear search, and having code to print the tree (which was used to debug it).


```e
pragma.syntax("0.9")
```


First, the algorithm:


```e
/** Makes leaves of the binary tree */
def leaf(value) {
    return def leaf {
        to run(_) { return value }
        to __printOn(out) { out.print("=> ", value) }
    }
}
/** Makes branches of the binary tree */
def split(leastRight, left, right) {
    return def tree {
        to run(specimen) {
            return if (specimen < leastRight) {
                left(specimen)
            } else {
                right(specimen)
            }
        }
        to __printOn(out) {
            out.print("    ")
            out.indent().print(left)
            out.lnPrint("< ")
            out.print(leastRight)
            out.indent().lnPrint(right)
        }
    }
}
def makeIntervalTree(assocs :List[Tuple[any, float64]]) {
    def size :int := assocs.size()
    if (size > 1) {
        def midpoint := size // 2
        return split(assocs[midpoint][1], makeIntervalTree(assocs.run(0, midpoint)),
                                          makeIntervalTree(assocs.run(midpoint)))
    } else {
        def [[value, _]] := assocs
        return leaf(value)
    }
}
def setupProbabilisticChoice(entropy, table :Map[any, float64]) {
    var cumulative := 0.0
    var intervalTable := []
    for value => probability in table {
        intervalTable with= [value, cumulative]
        cumulative += probability
    }
    def total := cumulative
    def selector := makeIntervalTree(intervalTable)
    return def probChoice {
        # Multiplying by the total helps correct for any error in the sum of the inputs
        to run() { return selector(entropy.nextDouble() * total) }
        to __printOn(out) {
            out.print("Probabilistic choice using tree:")
            out.indent().lnPrint(selector)
        }
    }
}
```


Then the test setup:


```e
def rosetta := setupProbabilisticChoice(entropy, def probTable := [
    "aleph"  => 1/5,
    "beth"   => 1/6.0,
    "gimel"  => 1/7.0,
    "daleth" => 1/8.0,
    "he"     => 1/9.0,
    "waw"    => 1/10.0,
    "zayin"  => 1/11.0,
    "heth"   => 0.063455988455988432,
])

var trials := 1000000
var timesFound := [].asMap()
for i in 1..trials {
    if (i % 1000 == 0) { print(`${i//1000} `) }
    def value := rosetta()
    timesFound with= (value, timesFound.fetch(value, fn { 0 }) + 1)
}
stdout.println()
for item in probTable.domain() {
    stdout.print(item, "\t", timesFound[item] / trials, "\t", probTable[item], "\n")
}
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Probabilistic do
  @tries 1000000
  @probs [aleph:  1/5,
          beth:   1/6,
          gimel:  1/7,
          daleth: 1/8,
          he:     1/9,
          waw:    1/10,
          zayin:  1/11,
          heth:   1759/27720]

  def test do
    trials = for _ <- 1..@tries, do: get_choice(@probs, :rand.uniform)
    IO.puts "Item      Expected   Actual"
    fmt = " ~-8s ~.6f  ~.6f~n"
    Enum.each(@probs, fn {glyph,expected} ->
      actual = length(for ^glyph <- trials, do: glyph) / @tries
      :io.format fmt, [glyph, expected, actual]
    end)
  end

  defp get_choice([{glyph,_}], _), do: glyph
  defp get_choice([{glyph,prob}|_], ran) when ran < prob, do: glyph
  defp get_choice([{_,prob}|t], ran), do: get_choice(t, ran - prob)
end

Probabilistic.test
```


{{out}}

```txt

Item      Expected   Actual
 aleph    0.200000  0.200676
 beth     0.166667  0.166103
 gimel    0.142857  0.142543
 daleth   0.125000  0.125055
 he       0.111111  0.111165
 waw      0.100000  0.100439
 zayin    0.090909  0.090894
 heth     0.063456  0.063125

```



## Erlang

{{trans|Java}}
The optimized version of Java.


```erlang

-module(probabilistic_choice).

-export([test/0]).

-define(TRIES, 1000000).

test() ->
	Probs =
		[{aleph,1/5},
		{beth,1/6},
		{gimel,1/7},
		{daleth,1/8},
		{he,1/9},
		{waw,1/10},
		{zayin,1/11},
		{heth,1759/27720}],
    random:seed(now()),
    Trials =
    	[get_choice(Probs,random:uniform()) || _ <- lists:seq(1,?TRIES)],
    [{Glyph,Expected,(length([Glyph || Glyph_ <- Trials, Glyph_ == Glyph])/?TRIES)}
    	 || {Glyph,Expected} <- Probs].

get_choice([{Glyph,_}],_) ->
	Glyph;
get_choice([{Glyph,Prob}|T],Ran) ->
	case (Ran < Prob) of
		true ->
			Glyph;
		false ->
			get_choice(T,Ran - Prob)
	end.

```


Output:


```txt

[{aleph,0.2,0.200325},
 {beth,0.16666666666666666,0.167108},
 {gimel,0.14285714285714285,0.142246},
 {daleth,0.125,0.124851},
 {he,0.1111111111111111,0.111345},
 {waw,0.1,0.099912},
 {zayin,0.09090909090909091,0.091352},
 {heth,0.06345598845598846,0.062861}]

```



## ERRE


```ERRE
PROGRAM PROB_CHOICE

   DIM ITEM$[7],PROB[7],CNT[7]

BEGIN
   ITEM$[]=("aleph","beth","gimel","daleth","he","waw","zayin","heth")

   PROB[0]=1/5.0  PROB[1]=1/6.0  PROB[2]=1/7.0   PROB[3]=1/8.0
   PROB[4]=1/9.0  PROB[5]=1/10.0 PROB[6]=1/11.0  PROB[7]=1759/27720
   SUM=0
   FOR I%=0 TO UBOUND(PROB,1) DO
      SUM=SUM+PROB[I%]
   END FOR

   IF ABS(SUM-1)>1E-6 THEN
        PRINT("Probabilities don't sum to 1")
      ELSE
        FOR TRIAL=1 TO 1E6 DO
           R=RND(1)
           P=0
           FOR I%=0 TO UBOUND(PROB,1) DO
              P+=PROB[I%]
              IF R<P THEN
                 CNT[I%]+=1
                 EXIT
              END IF
           END FOR
        END FOR
        PRINT("Item        actual    theoretical")
        PRINT("---------------------------------")
        FOR I%=0 TO UBOUND(ITEM$,1) DO
           WRITE("\      \    #.######  #.######";ITEM$[I%],CNT[I%]/1E6,PROB[I%])
        END FOR
   END IF
END PROGRAM
```


Output:


```txt

Item        actual    theoretical
---------------------------------
aleph       0.199769  0.200000
beth        0.167277  0.166667
gimel       0.142914  0.142857
daleth      0.124991  0.125000
he          0.111227  0.111111
waw         0.099732  0.100000
zayin       0.090757  0.090909
heth        0.063333  0.063456

```



## Euphoria

{{trans|PureBasic}}

```euphoria
constant MAX = #3FFFFFFF
constant times = 1e6
atom d,e
sequence Mapps
Mapps = {
    { "aleph",  1/5,        0},
    { "beth",   1/6,        0},
    { "gimel",  1/7,        0},
    { "daleth", 1/8,        0},
    { "he",     1/9,        0},
    { "waw",    1/10,       0},
    { "zayin",  1/11,       0},
    { "heth",   1759/27720, 0}
}

for i = 1 to times do
    d = (rand(MAX)-1)/MAX
    e = 0
    for j = 1 to length(Mapps) do
        e += Mapps[j][2]
        if d <= e then
            Mapps[j][3] += 1
            exit
        end if
    end for
end for

printf(1,"Sample times: %d\n",times)
for j = 1 to length(Mapps) do
    d = Mapps[j][3]/times
    printf(1,"%-7s should be %f is %f | Deviatation %6.3f%%\n",
                {Mapps[j][1],Mapps[j][2],d,(1-Mapps[j][2]/d)*100})
end for
```


Output:

```txt
Sample times: 1000000
aleph   should be 0.200000 is 0.200492 | Deviatation  0.245%
beth    should be 0.166667 is 0.166855 | Deviatation  0.113%
gimel   should be 0.142857 is 0.143169 | Deviatation  0.218%
daleth  should be 0.125000 is 0.124923 | Deviatation -0.062%
he      should be 0.111111 is 0.110511 | Deviatation -0.543%
waw     should be 0.100000 is 0.099963 | Deviatation -0.037%
zayin   should be 0.090909 is 0.090647 | Deviatation -0.289%
heth    should be 0.063456 is 0.063440 | Deviatation -0.025%

```



## Factor


```factor
USING: arrays assocs combinators.random io kernel macros math
math.statistics prettyprint quotations sequences sorting formatting ;
IN: rosettacode.proba

CONSTANT: data
{
    { "aleph"   1/5.0 }
    { "beth"    1/6.0 }
    { "gimel"   1/7.0 }
    { "daleth"  1/8.0 }
    { "he"      1/9.0 }
    { "waw"     1/10.0 }
    { "zayin"   1/11.0 }
    { "heth"    f }
}

MACRO: case-probas ( data -- case-probas )
    [ first2 [ swap 1quotation 2array ] [ 1quotation ] if* ] map 1quotation ;

: expected ( name data -- float )
    2dup at [ 2nip ] [ nip values sift sum 1 swap - ] if* ;
: generate ( # case-probas -- seq )
    H{ } clone
    [ [ [ casep ] [ inc-at ] bi* ] 2curry times ] keep ; inline
: normalize ( seq # -- seq )
    [ clone ] dip [ /f ] curry assoc-map ;
: summarize1 ( name value data -- )
    [ over ] dip expected
    "%6s: %10f %10f\n" printf ;
: summarize ( generated data -- )
    "Key" "Value" "expected" "%6s  %10s %10s\n" printf
    [ summarize1 ] curry assoc-each ;
: generate-normalized ( # proba -- seq )
    [ generate ] [ drop normalize ] 2bi ; inline
: example ( # data -- )
    [ case-probas generate-normalized ]
    [ summarize ] bi ; inline
```


In a REPL:
<lang>USE: rosettacode.proba
1000000 data example
```

outputs
<lang>   Key       Value   expected
  heth:   0.063469   0.063456
   waw:   0.100226   0.100000
daleth:   0.125844   0.125000
  beth:   0.166264   0.166667
 zayin:   0.090806   0.090909
    he:   0.110562   0.111111
 aleph:   0.199868   0.200000
 gimel:   0.142961   0.142857
```



## Forth


```forth
include random.fs

\ common factors of desired probabilities (1/5 .. 1/11)
2 2 * 2 * 3 * 3 * 5 * 7 * 11 * constant denom   \ 27720

\ represent each probability as the numerator with 27720 as the denominator
: ,numerators ( max min -- )
  do denom i / , loop ;

\  final item is 27720 - sum(probs)
: ,remainder ( denom addr len -- )
  cells bounds do  i @ -  1 cells +loop , ;

create probs 12 5 ,numerators  denom probs 7 ,remainder
create bins 8 cells allot

: choose ( -- 0..7 )
  denom random
  8 0 do
    probs i cells + @ -
    dup 0< if drop i unloop exit then
  loop
  abort" can't get here" ;

: trials ( n -- )
  0 do  1  bins choose cells +  +!  loop ;

: str-table
  create ( c-str ... n -- ) 0 do , loop
  does> ( n -- str len ) swap cells + @ count ;

here ," heth"   here ," zayin" here ," waw"  here ," he"
here ," daleth" here ," gimel" here ," beth" here ," aleph"
8 str-table names

: .header
  cr ." Name" #tab emit ." Prob" #tab emit ." Actual" #tab emit ." Error" ;
: .result ( n -- )
  cr dup names type #tab emit
  dup cells probs + @ s>f denom s>f f/ fdup f. #tab emit
  dup cells bins  + @ s>f 1e6       f/ fdup f. #tab emit
  f- fabs fs. ;

: .results   .header 8 0 do i .result loop ;
```



```txt

bins 8 cells erase
3 set-precision
1000000 trials .results
Name    Prob    Actual  Error
aleph   0.2     0.2     9.90E-5
beth    0.167   0.167   4.51E-4
gimel   0.143   0.142   4.99E-4
daleth  0.125   0.125   1.82E-4
he      0.111   0.111   2.10E-4
waw     0.1     0.1     3.30E-5
zayin   0.0909  0.0912  2.77E-4
heth    0.0635  0.0636  9.70E-5  ok

```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
PROGRAM PROBS

  IMPLICIT NONE

  INTEGER, PARAMETER :: trials = 1000000
  INTEGER :: i, j, probcount(8) = 0
  REAL :: expected(8), mapping(8), rnum
  CHARACTER(6) :: items(8) = (/ "aleph ", "beth  ", "gimel ", "daleth", "he    ", "waw   ", "zayin ", "heth  " /)

  expected(1:7) = (/ (1.0/i, i=5,11) /)
  expected(8) = 1.0 - SUM(expected(1:7))
  mapping(1) = 1.0 / 5.0
  DO i = 2, 7
     mapping(i) = mapping(i-1) + 1.0/(i+4.0)
  END DO
  mapping(8) = 1.0

  DO i = 1, trials
     CALL RANDOM_NUMBER(rnum)
     DO j = 1, 8
        IF (rnum < mapping(j)) THEN
           probcount(j) = probcount(j) + 1
           EXIT
        END IF
     END DO
  END DO

  WRITE(*, "(A,I10)") "Trials:             ", trials
  WRITE(*, "(A,8A10)") "Items:             ", items
  WRITE(*, "(A,8F10.6)") "Target Probability:  ", expected
  WRITE(*, "(A,8F10.6)") "Attained Probability:", REAL(probcount) / REAL(trials)

ENDPROGRAM PROBS
```

Sample Output:

```txt

Trials:                1000000
Items:                 aleph     beth      gimel     daleth    he        waw       zayin     heth
Target Probability:    0.200000  0.166667  0.142857  0.125000  0.111111  0.100000  0.090909  0.063456
Attained Probability:  0.199631  0.166907  0.142488  0.124920  0.110906  0.099943  0.091775  0.063430
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim letters  (0 To 7) As String = {"aleph", "beth", "gimel", "daleth", "he", "waw", "zayin", "heth"}
Dim actual   (0 To 7) As Integer '' all zero by default
Dim probs (0 To 7)    As Double = {1/5.0, 1/6.0, 1/7.0, 1/8.0, 1/9.0, 1/10.0, 1/11.0}
Dim cumProbs (0 To 7) As Double

cumProbs(0) = probs(0)
For i As Integer = 1 To 6
  cumProbs(i) = cumProbs(i - 1) + probs(i)
Next
cumProbs(7) = 1.0
probs(7) = 1.0 - cumProbs(6)

Randomize
Dim rand As Double
Dim n As Double = 1000000
Dim sum As Double = 0.0

For i As Integer = 1 To n
  rand = Rnd  '' random number where 0 <= rand < 1
  Select case rand
    Case Is <= cumProbs(0)
      actual(0) += 1
    Case Is <= cumProbs(1)
      actual(1) += 1
    Case Is <= cumProbs(2)
      actual(2) += 1
    Case Is <= cumProbs(3)
      actual(3) += 1
    Case Is <= cumProbs(4)
      actual(4) += 1
    Case Is <= cumProbs(5)
      actual(5) += 1
    Case Is <= cumProbs(6)
      actual(6) += 1
    Case Else
      actual(7) += 1
  End Select
Next

Dim sumActual As Double = 0

Print "Letter", " Actual", "Expected"
Print "------", "--------", "--------"
For i As Integer = 0 To 7
  Print letters(i),
  Print Using "#.######"; actual(i)/n;
  sumActual += actual(i)/n
  Print , Using "#.######"; probs(i)
Next

Print , "--------", "--------"
Print , Using "#.######"; sumActual;
Print , Using "#.######"; 1.000000

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Letter         Actual       Expected
------        --------      --------
aleph         0.199987      0.200000
beth          0.166663      0.166667
gimel         0.143134      0.142857
daleth        0.125132      0.125000
he            0.110772      0.111111
waw           0.100236      0.100000
zayin         0.090664      0.090909
heth          0.063412      0.063456
              --------      --------
              1.000000      1.000000

```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

type mapping struct {
    item string
    pr   float64
}

func main() {
    // input mapping
    m := []mapping{
        {"aleph", 1 / 5.},
        {"beth", 1 / 6.},
        {"gimel", 1 / 7.},
        {"daleth", 1 / 8.},
        {"he", 1 / 9.},
        {"waw", 1 / 10.},
        {"zayin", 1 / 11.},
        {"heth", 1759 / 27720.}} // adjusted so that probabilities add to 1

    // cumulative probability
    cpr := make([]float64, len(m)-1)
    var c float64
    for i := 0; i < len(m)-1; i++ {
        c += m[i].pr
        cpr[i] = c
    }

    // generate
    const samples = 1e6
    occ := make([]int, len(m))
    rand.Seed(time.Now().UnixNano())
    for i := 0; i < samples; i++ {
        r := rand.Float64()
        for j := 0; ; j++ {
            if r < cpr[j] {
                occ[j]++
                break
            }
            if j == len(cpr)-1 {
                occ[len(cpr)]++
                break
            }
        }
    }

    // report
    fmt.Println("  Item  Target   Generated")
    var totalTarget, totalGenerated float64
    for i := 0; i < len(m); i++ {
        target := m[i].pr
        generated := float64(occ[i]) / samples
        fmt.Printf("%6s  %8.6f  %8.6f\n", m[i].item, target, generated)
        totalTarget += target
        totalGenerated += generated
    }
    fmt.Printf("Totals  %8.6f  %8.6f\n", totalTarget, totalGenerated)
}
```

Output:

```txt

  Item  Target   Generated
 aleph  0.200000  0.199509
  beth  0.166667  0.167194
 gimel  0.142857  0.143293
daleth  0.125000  0.124869
    he  0.111111  0.110896
   waw  0.100000  0.099849
 zayin  0.090909  0.090789
  heth  0.063456  0.063601
Totals  1.000000  1.000000

```



## Haskell


```haskell
import System.Random (newStdGen, randomRs)

dataBinCounts :: [Float] -> [Float] -> [Int]
dataBinCounts thresholds range =
  let sampleSize = length range
      xs = ((-) sampleSize . length . flip filter range . (<)) <$> thresholds
  in zipWith (-) (xs ++ [sampleSize]) (0 : xs)

main :: IO ()
main = do
  g <- newStdGen
  let fractions = recip <$> [5 .. 11] :: [Float]
      expected = fractions ++ [1 - sum fractions]
      actual =
        ((/ 1000000.0) . fromIntegral) <$>
        dataBinCounts (scanl1 (+) expected) (take 1000000 (randomRs (0, 1) g))

      piv n = take n . (++ repeat ' ')

  putStrLn "       expected     actual"
  mapM_ putStrLn $
    zipWith3
      (\l s c -> piv 7 l ++ piv 13 (show s) ++ piv 12 (show c))
      ["aleph", "beth", "gimel", "daleth", "he", "waw", "zayin", "heth"]
      expected
      actual
```

{{Out}}
Sample

```txt
       expected     actual
aleph  0.2          0.200597
beth   0.16666667   0.167192
gimel  0.14285715   0.142781
daleth 0.125        0.124556
he     0.11111111   0.111128
waw    0.1          9.9671e-2
zayin  9.090909e-2  9.0294e-2
heth   6.345594e-2  6.3781e-2
```



## HicEst


```HicEst
REAL :: trials=1E6, n=8, map(n), limit(n), expected(n), outcome(n)

expected = 1 / ($ + 4)
expected(n) = 1 - SUM(expected) + expected(n)

map = expected
map = map($) + map($-1)

DO i = 1, trials
   random = RAN(1)
   limit = random > map
   item = INDEX(limit, 0)
   outcome(item) = outcome(item) + 1
ENDDO
outcome = outcome / trials

DLG(Text=expected, Text=outcome, Y=0)
```

Exported from the spreadsheet-like DLG function:

```txt
0.2        0.199908
0.1666667  0.166169
0.1428571  0.142722
0.125      0.124929
0.1111111  0.111706
0.1        0.099863
0.0909091  0.090965
0.063456   0.063738
```


=={{header|Icon}} and {{header|Unicon}}==


```Icon

record Item(value, probability)

procedure find_item (items, v)
  sum := 0.0
  every item := !items do {
    if v < sum+item.probability
     then return item.value
     else sum +:= item.probability
  }
  fail # v exceeded 1.0
end

# -- helper procedures

# count the number of occurrences of i in list l,
# assuming the items are strings
procedure count (l, i)
  result := 0.0
  every x := !l do
    if x == i then result +:= 1
  return result
end

procedure rand_float ()
  return ?1000/1000.0
end

# -- test the procedure
procedure main ()
  items := [
    Item("aleph",   1/5.0),
    Item("beth",    1/6.0),
    Item("gimel",   1/7.0),
    Item("daleth",  1/8.0),
    Item("he",      1/9.0),
    Item("waw",     1/10.0),
    Item("zayin",   1/11.0),
    Item("heth",    1759/27720.0)
  ]

  # collect a sample of results
  sample := []
  every (1 to 1000000) do push (sample, find_item(items, rand_float ()))

  # return comparison of expected vs actual probability
  every item := !items do
    write (right(item.value, 7) || " " ||
           left(item.probability, 15) || " " ||
           left(count(sample, item.value)/*sample, 6))
end

```


Output:

```txt

  aleph 0.2             0.1988
   beth 0.1666666667    0.1676
  gimel 0.1428571429    0.1431
 daleth 0.125           0.1249
     he 0.1111111111    0.1112
    waw 0.1             0.0996
  zayin 0.09090909091   0.0908
   heth 0.06345598846   0.0636

```



## J


```J

main=: verb define
  hdr=.  '       target   actual  '
  lbls=. ; ,:&.> ;:'aleph beth gimel daleth he waw zayin heth'
  prtn=. +/\ pt=. (, 1-+/)1r1%5+i.7
  da=.   prtn I. ?y # 0
  pa=.   y%~ +/ da =/ i.8
  hdr, lbls,. 9j6 ": |: pt,:pa
)

Note 'named abbreviations'
     hdr  (header)
     lbls (labels)
     pt   (target proportions)
     prtn (partitions corresponding to target proportions)
     da   (distribution of actual values among partitions)
     pa   (actual proportions)
)
```

Example use:

```j
main 1e6
       target   actual
aleph  0.200000 0.200344
beth   0.166667 0.166733
gimel  0.142857 0.142611
daleth 0.125000 0.124458
he     0.111111 0.111455
waw    0.100000 0.099751
zayin  0.090909 0.091121
heth   0.063456 0.063527
```

Note that there is no rounding error in summing the proportions, as they are represented as rational numbers, not floating-point approximations.

```J
   pt=. (, 1-+/)1r1%5+i.7
   pt
1r5 1r6 1r7 1r8 1r9 1r10 1r11 1759r27720
   +/pt
1
```



## Java

{{trans|C}}

```java
public class Prob{
	static long TRIALS= 1000000;

	private static class Expv{
		public String name;
		public int probcount;
		public double expect;
		public double mapping;

		public Expv(String name, int probcount, double expect, double mapping){
			this.name= name;
			this.probcount= probcount;
			this.expect= expect;
			this.mapping= mapping;
		}
	}

	static Expv[] items=
			{new Expv("aleph", 0, 0.0, 0.0), new Expv("beth", 0, 0.0, 0.0),
					new Expv("gimel", 0, 0.0, 0.0),
					new Expv("daleth", 0, 0.0, 0.0),
					new Expv("he", 0, 0.0, 0.0), new Expv("waw", 0, 0.0, 0.0),
					new Expv("zayin", 0, 0.0, 0.0),
					new Expv("heth", 0, 0.0, 0.0)};

	public static void main(String[] args){
		int i, j;
		double rnum, tsum= 0.0;

		for(i= 0, rnum= 5.0;i < 7;i++, rnum+= 1.0){
			items[i].expect= 1.0 / rnum;
			tsum+= items[i].expect;
		}
		items[7].expect= 1.0 - tsum;

		items[0].mapping= 1.0 / 5.0;
		for(i= 1;i < 7;i++){
			items[i].mapping= items[i - 1].mapping + 1.0 / ((double)i + 5.0);
		}
		items[7].mapping= 1.0;


		for(i= 0;i < TRIALS;i++){
			rnum= Math.random();
			for(j= 0;j < 8;j++){
				if(rnum < items[j].mapping){
					items[j].probcount++;
					break;
				}
			}
		}

		System.out.printf("Trials: %d\n", TRIALS);
		System.out.printf("Items:          ");
		for(i= 0;i < 8;i++)
			System.out.printf("%-8s ", items[i].name);
		System.out.printf("\nTarget prob.:   ");
		for(i= 0;i < 8;i++)
			System.out.printf("%8.6f ", items[i].expect);
		System.out.printf("\nAttained prob.: ");
		for(i= 0;i < 8;i++)
			System.out.printf("%8.6f ", (double)(items[i].probcount)
					/ (double)TRIALS);
		System.out.printf("\n");

	}
}
```

Output:

```txt
Trials: 1000000
Items:          aleph    beth     gimel    daleth   he       waw      zayin    heth
Target prob.:   0.200000 0.166667 0.142857 0.125000 0.111111 0.100000 0.090909 0.063456
Attained prob.: 0.199615 0.167517 0.142612 0.125211 0.110970 0.099614 0.091002 0.063459
```

{{works with|Java|1.5+}}

```java5
import java.util.EnumMap;

public class Prob {
	public static long TRIALS= 1000000;
	public enum Glyph{
		ALEPH, BETH, GIMEL, DALETH, HE, WAW, ZAYIN, HETH;
	}

	public static EnumMap<Glyph, Double> probs = new EnumMap<Glyph, Double>(Glyph.class){{
		put(Glyph.ALEPH,   1/5.0);
		put(Glyph.BETH,    1/6.0);
		put(Glyph.GIMEL,   1/7.0);
		put(Glyph.DALETH,  1/8.0);
		put(Glyph.HE,      1/9.0);
		put(Glyph.WAW,     1/10.0);
		put(Glyph.ZAYIN,   1/11.0);
		put(Glyph.HETH,    1759./27720);
	}};

	public static EnumMap<Glyph, Double> counts = new EnumMap<Glyph, Double>(Glyph.class){{
		put(Glyph.ALEPH, 0.);put(Glyph.BETH,   0.);
		put(Glyph.GIMEL, 0.);put(Glyph.DALETH, 0.);
		put(Glyph.HE,    0.);put(Glyph.WAW,    0.);
		put(Glyph.ZAYIN, 0.);put(Glyph.HETH,   0.);
	}};

	public static void main(String[] args){
		System.out.println("Target probabliities:\t" + probs);
		for(long i = 0; i < TRIALS; i++){
			Glyph choice = getChoice();
			counts.put(choice, counts.get(choice) + 1);
		}

		//correct the counts to probablities in (0..1]
		for(Glyph glyph:counts.keySet()){
			counts.put(glyph, counts.get(glyph) / TRIALS);
		}

		System.out.println("Actual probabliities:\t" + counts);
	}

	private static Glyph getChoice() {
		double rand = Math.random();
		for(Glyph item:Glyph.values()){
			if(rand < probs.get(item)){
				return item;
			}
			rand -= probs.get(item);
		}
		return null;
	}
}
```

Output:

```txt
Target probabliities:	{ALEPH=0.2, BETH=0.16666666666666666, GIMEL=0.14285714285714285, DALETH=0.125, HE=0.1111111111111111, WAW=0.1, ZAYIN=0.09090909090909091, HETH=0.06345598845598846}
Actual probabliities:	{ALEPH=0.200794, BETH=0.165916, GIMEL=0.143286, DALETH=0.124727, HE=0.110818, WAW=0.100168, ZAYIN=0.090878, HETH=0.063413}
```



## JavaScript


### ES5

Fortunately, iterating over properties added to an object maintains the insertion order.

```javascript
var probabilities = {
    aleph:  1/5.0,
    beth:   1/6.0,
    gimel:  1/7.0,
    daleth: 1/8.0,
    he:     1/9.0,
    waw:    1/10.0,
    zayin:  1/11.0,
    heth:   1759/27720
};

var sum = 0;
var iterations = 1000000;
var cumulative = {};
var randomly = {};
for (var name in probabilities) {
    sum += probabilities[name];
    cumulative[name] = sum;
    randomly[name] = 0;
}
for (var i = 0; i < iterations; i++) {
    var r = Math.random();
    for (var name in cumulative) {
        if (r <= cumulative[name]) {
            randomly[name]++;
            break;
        }
    }
}
for (var name in probabilities)
    // using WSH
    WScript.Echo(name + "\t" + probabilities[name] + "\t" + randomly[name]/iterations);
```

output:

```txt
aleph   0.2     0.200597
beth    0.16666666666666666     0.166527
gimel   0.14285714285714285     0.142646
daleth  0.125   0.124613
he      0.1111111111111111      0.111342
waw     0.1     0.099888
zayin   0.09090909090909091     0.091141
heth    0.06345598845598846     0.063246
```



### ES6

By functional composition:
{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS -----------------------------------------------------

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, iCol) => xs.map(row => row[iCol]));

    // justifyLeft :: Int -> Char -> Text -> Text
    const justifyLeft = (n, cFiller, strText) =>
        n > strText.length ? (
            (strText + cFiller.repeat(n))
            .substr(0, n)
        ) : strText;

    // 2 or more arguments
    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat([].slice.apply(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const ny = ys.length;
        return (xs.length <= ny ? xs : xs.slice(0, ny))
            .map((x, i) => f(x, ys[i]));
    };

    // subtract :: (Num a) => a -> a -> a
    const subtract = (x, y) => y - x;

    // scanl1 :: (a -> a -> a) -> [a] -> [a]
    const scanl1 = (f, xs) =>
        xs.length > 0 ? scanl(f, xs[0], xs.slice(1)) : [];

    // scanl :: (b -> a -> b) -> b -> [a] -> [b]
    const scanl = (f, startValue, xs) =>
        xs.reduce((a, x) => {
            const v = f(a.acc, x);
            return {
                acc: v,
                scan: a.scan.concat(v)
            };
        }, {
            acc: startValue,
            scan: [startValue]
        })
        .scan;

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');


    // PROBABILISTIC CHOICE --------------------------------------------------

    // samples :: Int -> Int -> [Float]
    const samples = n =>
        Array.from({
            length: n
        }, Math.random);

    // thresholds :: Float
    const thresholds = scanl1(
            (a, b) => a + b, [5, 6, 7, 8, 9, 10, 11].map(x => 1 / x)
        )
        .concat(1);

    // expected :: Float -> Float
    const expected = limits =>
        limits.map((x, i, xs) => i > 0 ? (x - xs[i - 1]) : x);

    // dataBinCounts :: [Float] -> [Float] -> [Int]
    const dataBinCounts = (thresholds, samples) => {
        const
            lng = samples.length,
            xs = thresholds
            .map(x => lng - samples.filter(v => v > x)
                .length);
        return zipWith(subtract, [0].concat(xs), xs.concat(lng));
    };

    // intSamples :: Integer
    const intSamples = 1000000;

    // aligned :: a -> String
    const aligned = x => justifyLeft(12, ' ', isNaN(x) ? x : x.toFixed(7));

    return transpose([
            ['', 'Aleph', 'Beit', 'Gimel', 'Dalet', 'He', 'Vav', 'Zayin', 'Chet']
            .map(curry(justifyLeft)(7, ' ')),

            ['Expected'].concat(expected(thresholds))
            .map(aligned),

            ['Observed'].concat(dataBinCounts(thresholds, samples(intSamples))
                .map(x => x / intSamples))
            .map(aligned)
        ])
        .map(unwords)
        .join('\n');
})();
```

{{Out}}
Sample:

```txt
        Expected     Observed
Aleph   0.2000000    0.2002440
Beit    0.1666667    0.1665330
Gimel   0.1428571    0.1433880
Dalet   0.1250000    0.1244630
He      0.1111111    0.1112830
Vav     0.1000000    0.0998390
Zayin   0.0909091    0.0909630
Chet    0.0634560    0.0632870
```



## Julia

I made the solution to this task more difficult than I had anticipated by using the Hebrew characters (rather than their anglicised names) as labels for the sampled collection of objects.  In doing so, I encountered an interesting subtlety of bidirectional text in Unicode.  Namely, that strong right-to-left characters, such as those of Hebrew, override the directionality of European digits, which have weak directionality.  Because of this property of Unicode, my table of items and yields had its lines of data interpreted as if it were entirely Hebrew and output in reverse order (from my English speaking perspective).  I was able to get the table to display as I liked on my terminal by preceding the the Hebrew characters by the Unicode RLI (right-to-left isolate) control character (<tt>U+2067</tt>).  However, when I pasted this output into this Rosetta Code entry, the display reverted to the "backwards" version.  Rather than continue the struggle, trying to force this entry to display as it does on my terminal, I created an alternative version of the table.  This "Displayable Here" table adds "yields" to to each line, and this strong left-to-right text makes the whole line display as left-to-right (without the need for a RLI characer).

```Julia

p = [1/i for i in 5:11]
plen = length(p)
q = [0.0, [sum(p[1:i]) for i = 1:plen]]
plab = [char(i) for i in 0x05d0:(0x05d0+plen)]
hi = 10^6
push!(p, 1.0 - sum(p))
plen += 1

accum = zeros(Int, plen)

for i in 1:hi
    accum[sum(rand() .>= q)] += 1
end

r = accum/hi

println("Rates at which items are selected (", hi, " trials).")
println(" Item  Expected   Actual")
for i in 1:plen
    println(@sprintf("   \u2067%s   %8.6f  %8.6f", plab[i], p[i], r[i]))
end

println()
println("Rates at which items are selected (", hi, " trials).")
println(" Item         Count   Expected   Actual")
for i in 1:plen
    println(@sprintf("   %s yields  %6d   %8.6f  %8.6f",
                     plab[i], accum[i], p[i], r[i]))
end

```


{{out}}
'''Original'''

This table displays properly on my terminal, but the lines of data are reversed in this display.

```txt

Rates at which items are selected (1000000 trials).
 Item  Expected   Actual
   ⁧א   0.200000  0.199872
   ⁧ב   0.166667  0.166618
   ⁧ג   0.142857  0.143302
   ⁧ד   0.125000  0.125040
   ⁧ה   0.111111  0.110602
   ⁧ו   0.100000  0.099833
   ⁧ז   0.090909  0.091313
   ⁧ח   0.063456  0.063420

```

'''Displayable Here'''

This is the same data, less elegantly presented but accurately displayed on both my terminal and here at Rosetta Code.

```txt

Rates at which items are selected (1000000 trials).
 Item         Count   Expected   Actual
   א yields  199872   0.200000  0.199872
   ב yields  166618   0.166667  0.166618
   ג yields  143302   0.142857  0.143302
   ד yields  125040   0.125000  0.125040
   ה yields  110602   0.111111  0.110602
   ו yields   99833   0.100000  0.099833
   ז yields   91313   0.090909  0.091313
   ח yields   63420   0.063456  0.063420

```



## Kotlin

{{trans|FreeBASIC}}

```scala
// version 1.0.6

fun main(args: Array<String>) {
    val letters  = arrayOf("aleph", "beth", "gimel", "daleth", "he", "waw", "zayin", "heth")
    val actual   = IntArray(8)
    val probs    = doubleArrayOf(1/5.0, 1/6.0, 1/7.0, 1/8.0, 1/9.0, 1/10.0, 1/11.0, 0.0)
    val cumProbs = DoubleArray(8)

    cumProbs[0] = probs[0]
    for (i in 1..6) cumProbs[i] = cumProbs[i - 1] + probs[i]
    cumProbs[7] = 1.0
    probs[7] = 1.0 - cumProbs[6]
    val n = 1000000
    (1..n).forEach {
        val rand = Math.random()
        when {
             rand <= cumProbs[0] -> actual[0]++
             rand <= cumProbs[1] -> actual[1]++
             rand <= cumProbs[2] -> actual[2]++
             rand <= cumProbs[3] -> actual[3]++
             rand <= cumProbs[4] -> actual[4]++
             rand <= cumProbs[5] -> actual[5]++
             rand <= cumProbs[6] -> actual[6]++
             else                -> actual[7]++
        }
    }

    var sumActual = 0.0
    println("Letter\t Actual    Expected")
    println("------\t--------   --------")
    for (i in 0..7) {
        val generated = actual[i].toDouble() / n
        println("${letters[i]}\t${String.format("%8.6f   %8.6f", generated, probs[i])}")
        sumActual += generated
    }
    println("\t--------   --------")
    println("\t${"%8.6f".format(sumActual)}   1.000000")
}
```


{{out}}

```txt

Letter   Actual    Expected
------  --------   --------
aleph   0.199427   0.200000
beth    0.166862   0.166667
gimel   0.142756   0.142857
daleth  0.125442   0.125000
he      0.110868   0.111111
waw     0.100405   0.100000
zayin   0.090799   0.090909
heth    0.063441   0.063456
        --------   --------
        1.000000   1.000000

```



## Liberty BASIC


```lb

names$="aleph beth gimel daleth he waw zayin heth"
dim sum(8)
dim counter(8)

s = 0
for i = 1 to 7
    s = s+1/(i+4)
    sum(i)=s
next

N =1000000     '  number of throws

for i =1 to N
    rand =rnd( 1)
    for j = 1 to 7
        if sum(j)> rand then exit for
    next
    counter(j)=counter(j)+1
next

print "Observed", "Intended"
for i = 1 to 8
    print word$(names$, i), using( "#.#####", counter(i)  /N), using( "#.#####", 1/(i+4))
next

```



## Lua


```lua
items = {}
items["aleph"]  = 1/5.0
items["beth"]   = 1/6.0
items["gimel"]  = 1/7.0
items["daleth"] = 1/8.0
items["he"]     = 1/9.0
items["waw"]    = 1/10.0
items["zayin"]  = 1/11.0
items["heth"]   = 1759/27720

num_trials = 1000000

samples = {}
for item, _ in pairs( items ) do
    samples[item] = 0
end

math.randomseed( os.time() )
for i = 1, num_trials do
    z = math.random()

    for item, _ in pairs( items ) do
	if z < items[item] then
	    samples[item] = samples[item] + 1
	    break;
	else
 	    z = z - items[item]
	end
    end
end

for item, _ in pairs( items ) do
    print( item, samples[item]/num_trials, items[item] )
end
```

Output

```txt
gimel	0.142606	0.14285714285714
heth	0.063434	0.063455988455988
beth	0.166788	0.16666666666667
zayin	0.091097	0.090909090909091
daleth	0.124772	0.125
aleph	0.200541	0.2
he	0.1107	        0.11111111111111
waw	0.100062	0.1
```



## Mathematica

Built-in function can already do a weighted random choosing. Example for making a million random choices would be:

```Mathematica
choices={{"aleph", 1/5},{"beth", 1/6},{"gimel", 1/7},{"daleth", 1/8},{"he", 1/9},{"waw", 1/10},{"zayin", 1/11},{"heth", 1759/27720}};
data=RandomChoice[choices[[All,2]]->choices[[All,1]],10^6];
```

To compare the data we use the following code to make a table:

```Mathematica
Grid[{#[[1]],N[Count[data,#[[1]]]/10^6],N[#[[2]]]}&/@choices]
```

gives back (item, attained prob., target prob.):

```txt
aleph		0.200036	0.2
beth		0.166591	0.166667
gimel		0.142699	0.142857
daleth		0.125018	0.125
he		0.111306	0.111111
waw		0.100433	0.1
zayin		0.090671	0.0909091
heth		0.063246	0.063456
```



## MATLAB

{{works with|MATLAB|with Statistics Toolbox}}

```MATLAB
function probChoice
    choices = {'aleph' 'beth' 'gimel' 'daleth' 'he' 'waw' 'zayin' 'heth'};
    w = [1/5 1/6 1/7 1/8 1/9 1/10 1/11 1759/27720];
    R = randsample(length(w), 1e6, true, w);
    T = tabulate(R);
    fprintf('Value\tCount\tPercent\tGoal\n')
    for k = 1:size(T, 1)
        fprintf('%6s\t%.f\t%.2f%%\t%.2f%%\n', ...
            choices{k}, T(k, 2), T(k, 3), 100*w(k))
    end
end
```

{{out}}

```txt
Value	Count	Percent	Goal
 aleph	199635	19.96%	20.00%
  beth	166427	16.64%	16.67%
 gimel	143342	14.33%	14.29%
daleth	125014	12.50%	12.50%
    he	111031	11.10%	11.11%
   waw	99920	9.99%	10.00%
 zayin	91460	9.15%	9.09%
  heth	63171	6.32%	6.35%
```

{{works with|MATLAB|without toolboxes}}

```MATLAB
function probChoice
    choices = {'aleph' 'beth' 'gimel' 'daleth' 'he' 'waw' 'zayin' 'heth'};
    w = [1/5 1/6 1/7 1/8 1/9 1/10 1/11 1759/27720];
    nSamp = 1e6;
    nChoice = length(w);
    R = rand(nSamp, 1);
    wCS = cumsum(w);
    results = zeros(1, nChoice);
    fprintf('Value\tCount\tPercent\tGoal\n')
    for k = 1:nChoice
        choiceKIdxs = R < wCS(k);
        R(choiceKIdxs) = k;
        results(k) = sum(choiceKIdxs);
        fprintf('%6s\t%.f\t%.2f%%\t%.2f%%\n', ...
            choices{k}, results(k), 100*results(k)/nSamp, 100*w(k))
    end
end
```

{{out}}

```txt
Value	Count	Percent	Goal
 aleph	200327	20.03%	20.00%
  beth	166318	16.63%	16.67%
 gimel	143040	14.30%	14.29%
daleth	125136	12.51%	12.50%
    he	111251	11.13%	11.11%
   waw	99946	9.99%	10.00%
 zayin	90974	9.10%	9.09%
  heth	63008	6.30%	6.35%
```



## Nim


```nim
import tables, math, strutils, times

const
   num_trials = 1000000
   precsn     = 6

var start = cpuTime()

var probs = initTable[string,float](16)
probs.add("aleph",  1/5.0)
probs.add("beth",   1/6.0)
probs.add("gimel",  1/7.0)
probs.add("daleth", 1/8.0)
probs.add("he",     1/9.0)
probs.add("waw",    1/10.0)
probs.add("zayin",  1/11.0)
probs.add("heth",   1759/27720)

var samples = initTable[string,int](16)
for i, j in pairs(probs):
    samples.add(i,0)

randomize()
for i in 1 .. num_trials:
    var z = random(1.0)

    for j,k in pairs(probs):
        if z < probs[j]:
            samples[j] = samples[j] + 1
            break
        else:
             z = z - probs[j]

var s1, s2: float

echo("Item  ","\t","Target  ","\t","Results  ","\t","Difference")
echo("====  ","\t","======  ","\t","
### =
  ","\t","
### ====
")
for i, j in pairs(probs):
    s1 += samples[i]/num_trials*100.0
    s2 += probs[i]*100.0
    echo( i,
             "\t", formatFloat(probs[i],ffDecimal,precsn),
             "\t", formatFloat(samples[i]/num_trials,ffDecimal,precsn),
             "\t", formatFloat(100.0*(1.0-(samples[i]/num_trials)/probs[i]),ffDecimal,precsn),"%")
echo("======","\t","
### =
 ","\t","
### ==
 ")
echo("Total:","\t",formatFloat(s2,ffDecimal,2),"  \t",formatFloat(s1,ffDecimal,2))
echo("\n",formatFloat(cpuTime()-start,ffDecimal,2)," secs")
```

{{out}}

```txt
Item  	Target  	Results  	Difference
====  	======
### =

### ====

he	0.111111	0.110760	0.316000%
heth	0.063456	0.063777	-0.505881%
beth	0.166667	0.166386	0.168400%
aleph	0.200000	0.200039	-0.019500%
zayin	0.090909	0.090923	-0.015300%
waw	0.100000	0.100513	-0.513000%
gimel	0.142857	0.142691	0.116300%
daleth	0.125000	0.124911	0.071200%
======
### =

### ==

Total:	100.00  	100.00

7.06 secs
```



## OCaml



```ocaml
let p = [
    "Aleph",   1.0 /. 5.0;
    "Beth",    1.0 /. 6.0;
    "Gimel",   1.0 /. 7.0;
    "Daleth",  1.0 /. 8.0;
    "He",      1.0 /. 9.0;
    "Waw",     1.0 /. 10.0;
    "Zayin",   1.0 /. 11.0;
    "Heth", 1759.0 /. 27720.0;
  ]

let rec take k = function
  | (v, p)::tl -> if k < p then v else take (k -. p) tl
  | _ -> invalid_arg "take"

let () =
  let n = 1_000_000 in
  Random.self_init();
  let h = Hashtbl.create 3 in
  List.iter (fun (v, _) -> Hashtbl.add h v 0) p;
  let tot = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 p in
  for i = 1 to n do
    let sel = take (Random.float tot) p in
    let n = Hashtbl.find h sel in
    Hashtbl.replace h sel (succ n)  (* count the number of each item *)
  done;
  List.iter (fun (v, p) ->
    let d = Hashtbl.find h v in
    Printf.printf "%s \t %f %f\n" v p (float d /. float n)
  ) p
```


Output:

```txt
Aleph    0.200000 0.200272
Beth     0.166667 0.166381
Gimel    0.142857 0.142497
Daleth   0.125000 0.125005
He       0.111111 0.111272
Waw      0.100000 0.100069
Zayin    0.090909 0.091136
Heth     0.063456 0.063368
```



## PARI/GP


```parigp
pc()={
  my(v=[5544,10164,14124,17589,20669,23441,25961,27720],u=vector(8),e);
  for(i=1,1e6,
    my(r=random(27720));
    for(j=1,8,
      if(r<v[j], u[j]++; break)
    )
  );
  e=precision([1/5,1/6,1/7,1/8,1/9,1/10,1/11,1759/27720]*1e6,9); \\ truncate to 9 decimal places
  print("Totals: "u);
  print("Expected: "e);
  print("Diff: ",u-e);
  print("StDev: ",vector(8,i,sqrt(abs(u[i]-v[i])/e[i])));
};
```



## Perl


```perl
use List::Util qw(first sum);
use constant TRIALS => 1e6;

sub prob_choice_picker {
  my %options = @_;
  my ($n, @a) = 0;
  while (my ($k,$v) = each %options) {
      $n += $v;
      push @a, [$n, $k];
  }
  return sub {
      my $r = rand;
      ( first {$r <= $_->[0]} @a )->[1];
  };
}

my %ps =
  (aleph  => 1/5,
   beth   => 1/6,
   gimel  => 1/7,
   daleth => 1/8,
   he     => 1/9,
   waw    => 1/10,
   zayin  => 1/11);
$ps{heth} = 1 - sum values %ps;

my $picker = prob_choice_picker %ps;
my %results;
for (my $n = 0 ; $n < TRIALS ; ++$n) {
    ++$results{$picker->()};
}

print "Event   Occurred  Expected  Difference\n";
foreach (sort {$results{$b} <=> $results{$a}} keys %results) {
    printf "%-6s  %f  %f  %f\n",
        $_, $results{$_}/TRIALS, $ps{$_},
        abs($results{$_}/TRIALS - $ps{$_});
}
```


Sample output:


```txt
Event   Occurred  Expected  Difference
aleph   0.198915  0.200000  0.001085
beth    0.166804  0.166667  0.000137
gimel   0.142992  0.142857  0.000135
daleth  0.125155  0.125000  0.000155
he      0.111160  0.111111  0.000049
waw     0.100229  0.100000  0.000229
zayin   0.091014  0.090909  0.000105
heth    0.063731  0.063456  0.000275
```



## Perl 6

{{works with|Rakudo|2018.10}}

```perl6
constant TRIALS = 1e6;

constant @event = <aleph beth gimel daleth he waw zayin heth>;

constant @P = flat (1 X/ 5 .. 11), 1759/27720;
constant @cP = [\+] @P;

my atomicint @results[+@event];
(^TRIALS).race.map: { @results[ @cP.first: { $_ > once rand }, :k ]⚛++; }

say  'Event    Occurred Expected  Difference';
for ^@results {
    my ($occurred, $expected) = @results[$_], @P[$_] * TRIALS;
    printf "%-9s%8.0f%9.1f%12.1f\n",
            @event[$_],
                $occurred,
                     $expected,
                          abs $occurred - $expected;
}
```

{{out}}

```txt
Event    Occurred Expected  Difference
aleph      200369 200000.0       369.0
beth       167005 166666.7       338.3
gimel      142690 142857.1       167.1
daleth     125061 125000.0        61.0
he         110563 111111.1       548.1
waw        100214 100000.0       214.0
zayin       90617  90909.1       292.1
heth        63481  63456.0        25.0
```



## Phix


```Phix
constant {names, probs} = columnize({{"aleph",  1/5},
                                     {"beth",   1/6},
                                     {"gimel",  1/7},
                                     {"daleth", 1/8},
                                     {"he",     1/9},
                                     {"waw",    1/10},
                                     {"zayin",  1/11},
                                     {"heth",   1759/27720}})

sequence results = repeat(0,length(names))

atom r
constant lim = 1000000
for j=1 to lim do
    r = rnd()
    for i=1 to length(probs) do
        r -= probs[i]
        if r<=0 then
            results[i]+=1
            exit
        end if
    end for
end for

printf(1,"  Name   Actual Expected\n")
for i=1 to length(probs) do
    printf(1,"%6s %8.6f %8.6f\n",{names[i],results[i]/lim,probs[i]})
end for
```

{{out}}

```txt

  Name   Actual Expected
 aleph 0.201010 0.200000
  beth 0.166311 0.166667
 gimel 0.143354 0.142857
daleth 0.124841 0.125000
    he 0.110544 0.111111
   waw 0.100228 0.100000
 zayin 0.090270 0.090909
  heth 0.063442 0.063456

```



## PicoLisp


```PicoLisp
(let (Count 1000000  Denom 27720  N Denom)
   (let Probs
      (mapcar
         '((I S)
            (prog1 (cons N (*/ Count I) 0 S)
               (dec 'N (/ Denom I)) ) )
         (range 5 12)
         '(aleph beth gimel daleth he waw zayin heth) )
      (do Count
         (inc (cddr (rank (rand 1 Denom) Probs T))) )
      (let Fmt (-6 12 12)
         (tab Fmt NIL "Probability" "Result")
         (for X Probs
            (tab Fmt
               (cdddr X)
               (format (cadr X) 6)
               (format (caddr X) 6) ) ) ) ) )
```

Output:

```txt
       Probability      Result
aleph     0.200000    0.199760
beth      0.166667    0.166878
gimel     0.142857    0.142977
daleth    0.125000    0.124983
he        0.111111    0.111200
waw       0.100000    0.100173
zayin     0.090909    0.090591
heth      0.083333    0.063438
```



## PL/I


```pli
 probch: Proc Options(main);
 Dcl prob(8) Dec Float(15) Init((1/5.0),      /* aleph  */
                                (1/6.0),      /* beth   */
                                (1/7.0),      /* gimel  */
                                (1/8.0),      /* daleth */
                                (1/9.0),      /* he     */
                                (1/10.0),     /* waw    */
                                (1/11.0),     /* zayin  */
                                (1759/27720));/* heth   */
 Dcl what(8) Char(6) Init('aleph ','beth  ','gimel ','daleth',
                          'he    ','waw   ','zayin ','heth  ');
 Dcl ulim(0:8) Dec Float(15) Init((9)0);
 Dcl i Bin Fixed(31);
 Dcl ifloat Dec Float(15);
 Dcl one    Dec Float(15) Init(1);
 Dcl num    Dec Float(15) Init(1759);
 Dcl denom  Dec Float(15) Init(27720);
 Dcl x      Dec Float(15) Init(0);
 Dcl pr     Dec Float(15) Init(0);
 Dcl (n,nn) Bin Fixed(31);
 Dcl cnt(8) Bin Fixed(31) Init((8)0);
 nn=1000000;
 Do i=1 To 8;
   ifloat=i+4;
   If i<8 Then
     prob(i)=one/ifloat;
   Else
     prob(i)=num/denom;
   Ulim(i)=ulim(i-1)+prob(i);
   /* Put Skip list(i,prob(i),ulim(i));*/
   End;
 Do n=1 To nn;
   x=random();
   Do i=1 To 8;
     If x<ulim(i) Then Leave;
     End;
   cnt(i)+=1;
   End;
 Put Edit('letter    occurs    frequency  expected ')(Skip,a);
 Put Edit('------    ------   ---------- ----------')(Skip,a);
 Do i=1 To 8;
   pr=float(cnt(i))/float(nn);
   Put Edit(what(i),cnt(i),pr,prob(i))(Skip,a,f(10),x(2),2(f(11,8)));
   End;
 End;
```

{{out}}

```txt
One million trials
letter    occurs    frequency  expected
------    ------   ---------- ---------
aleph     199989   0.19998900 0.20000000
beth      167338   0.16733800 0.16666667
gimel     142968   0.14296800 0.14285714
daleth    124840   0.12484000 0.12500000
he        110620   0.11062000 0.11111111
waw        99744   0.09974400 0.10000000
zayin      90930   0.09093000 0.09090909
heth       63571   0.06357100 0.06345599

One hundred million trials
letter    occurs    frequency  expected
------    ------   ---------- ----------
aleph   20002222   0.20002222 0.20000000
beth    16665226   0.16665226 0.16666667
gimel   14289674   0.14289674 0.14285714
daleth  12498182   0.12498182 0.12500000
he      11108704   0.11108704 0.11111111
waw     10002442   0.10002442 0.10000000
zayin    9087412   0.09087412 0.09090909
heth     6346138   0.06346138 0.06345599
```



## PowerShell

{{trans|Java Script}}
The guts of this script are translated from the Java Script entry.  Then I stole the idea to show the actual Hebrew character from Julia.

```PowerShell

$character = [PSCustomObject]@{
    aleph  = [PSCustomObject]@{Expected=1/5       ; Alpha="א"}
    beth   = [PSCustomObject]@{Expected=1/6       ; Alpha="ב"}
    gimel  = [PSCustomObject]@{Expected=1/7       ; Alpha="ג"}
    daleth = [PSCustomObject]@{Expected=1/8       ; Alpha="ד"}
    he     = [PSCustomObject]@{Expected=1/9       ; Alpha="ה"}
    waw    = [PSCustomObject]@{Expected=1/10      ; Alpha="ו"}
    zayin  = [PSCustomObject]@{Expected=1/11      ; Alpha="ז"}
    heth   = [PSCustomObject]@{Expected=1759/27720; Alpha="ח"}
}

$sum        = 0
$iterations = 1000000
$cumulative = [ordered]@{}
$randomly   = [ordered]@{}

foreach ($name in $character.PSObject.Properties.Name)
{
    $sum += $character.$name.Expected
    $cumulative.$name = $sum
    $randomly.$name = 0
}

for ($i = 0; $i -lt $iterations; $i++)
{
    $random = Get-Random -Minimum 0.0 -Maximum 1.0

    foreach ($name in $cumulative.Keys)
    {
        if ($random -le $cumulative.$name)
        {
            $randomly.$name++
            break
        }
    }
}

foreach ($name in $character.PSObject.Properties.Name)
{
    [PSCustomObject]@{
        Name      = $name
        Expected  = $character.$name.Expected
        Actual    = $randomly.$name / $iterations
        Character = $character.$name.Alpha
    }
}

```

{{Out}}

```txt

Name             Expected   Actual Character
----             --------   ------ ---------
aleph                 0.2 0.199823 א
beth    0.166666666666667 0.166744 ב
gimel   0.142857142857143 0.143312 ג
daleth              0.125 0.125153 ד
he      0.111111111111111 0.110984 ה
waw                   0.1 0.099667 ו
zayin  0.0909090909090909 0.091135 ז
heth   0.0634559884559885 0.063182 ח

```



## PureBasic


```PureBasic
#times=1000000

Structure Item
  name.s
  prob.d
  Amount.i
EndStructure

If OpenConsole()
  Define i, j, d.d, e.d, txt.s
  Dim Mapps.Item(7)
  Mapps(0)\name="aleph": Mapps(0)\prob=1/5.0
  Mapps(1)\name="beth":  Mapps(1)\prob=1/6.0
  Mapps(2)\name="gimel": Mapps(2)\prob=1/7.0
  Mapps(3)\name="daleth":Mapps(3)\prob=1/8.0
  Mapps(4)\name="he":    Mapps(4)\prob=1/9.0
  Mapps(5)\name="waw":   Mapps(5)\prob=1/10.0
  Mapps(6)\name="zayin": Mapps(6)\prob=1/11.0
  Mapps(7)\name="heth":  Mapps(7)\prob=1759/27720.0

  For i=1 To #times
    d=Random(#MAXLONG)/#MAXLONG  ; Get a random number
    e=0.0
    For j=0 To ArraySize(Mapps())
      e+Mapps(j)\prob            ; Get span for current itme
      If d<=e                    ; Check if it is within this span?
        Mapps(j)\Amount+1        ; If so, count it.
        Break
      EndIf
    Next j
  Next i

  PrintN("Sample times: "+Str(#times)+#CRLF$)
  For j=0 To ArraySize(Mapps())
      d=Mapps(j)\Amount/#times
      txt=LSet(Mapps(j)\name,7)+" should be "+StrD(Mapps(j)\prob)+" is "+StrD(d)
      PrintN(txt+" | Deviatation "+RSet(StrD(100.0-100.0*Mapps(j)\prob/d,3),6)+"%")
  Next

  Print(#CRLF$+"Press ENTER to exit"):Input()
  CloseConsole()
EndIf
```


Output may look like
 Sample times: 1000000

 aleph   should be 0.2000000000 is 0.1995520000 | Deviatation -0.225%
 beth    should be 0.1666666667 is 0.1673270000 | Deviatation  0.395%
 gimel   should be 0.1428571429 is 0.1432040000 | Deviatation  0.242%
 daleth  should be 0.1250000000 is 0.1251850000 | Deviatation  0.148%
 he      should be 0.1111111111 is 0.1109550000 | Deviatation -0.141%
 waw     should be 0.1000000000 is 0.0999220000 | Deviatation -0.078%
 zayin   should be 0.0909090909 is 0.0902240000 | Deviatation -0.759%
 heth    should be 0.0634559885 is 0.0636310000 | Deviatation  0.275%

 Press ENTER to exit


## Python

Two different algorithms are coded.

```python
import random, bisect

def probchoice(items, probs):
  '''\
  Splits the interval 0.0-1.0 in proportion to probs
  then finds where each random.random() choice lies
  '''

  prob_accumulator = 0
  accumulator = []
  for p in probs:
    prob_accumulator += p
    accumulator.append(prob_accumulator)

  while True:
    r = random.random()
    yield items[bisect.bisect(accumulator, r)]

def probchoice2(items, probs, bincount=10000):
  '''\
  Puts items in bins in proportion to probs
  then uses random.choice() to select items.

  Larger bincount for more memory use but
  higher accuracy (on avarage).
  '''

  bins = []
  for item,prob in zip(items, probs):
    bins += [item]*int(bincount*prob)
  while True:
    yield random.choice(bins)


def tester(func=probchoice, items='good bad ugly'.split(),
                    probs=[0.5, 0.3, 0.2],
                    trials = 100000
                    ):
  def problist2string(probs):
    '''\
    Turns a list of probabilities into a string
    Also rounds FP values
    '''
    return ",".join('%8.6f' % (p,) for p in probs)

  from collections import defaultdict

  counter = defaultdict(int)
  it = func(items, probs)
  for dummy in xrange(trials):
    counter[it.next()] += 1
  print "\n##\n## %s\n##" % func.func_name.upper()
  print "Trials:              ", trials
  print "Items:               ", ' '.join(items)
  print "Target probability:  ", problist2string(probs)
  print "Attained probability:", problist2string(
    counter[x]/float(trials) for x in items)

if __name__ == '__main__':
  items = 'aleph beth gimel daleth he waw zayin heth'.split()
  probs = [1/(float(n)+5) for n in range(len(items))]
  probs[-1] = 1-sum(probs[:-1])
  tester(probchoice, items, probs, 1000000)
  tester(probchoice2, items, probs, 1000000)
```


Sample output:

```txt

##
## PROBCHOICE
##
Trials:               1000000
Items:                aleph beth gimel daleth he waw zayin heth
Target probability:   0.200000,0.166667,0.142857,0.125000,0.111111,0.100000,0.090909,0.063456
Attained probability: 0.200050,0.167109,0.143364,0.124690,0.111237,0.099661,0.090338,0.063551

##
## PROBCHOICE2
##
Trials:               1000000
Items:                aleph beth gimel daleth he waw zayin heth
Target probability:   0.200000,0.166667,0.142857,0.125000,0.111111,0.100000,0.090909,0.063456
Attained probability: 0.199720,0.166424,0.142474,0.124561,0.111511,0.100313,0.091316,0.063681
```



## R


```R
prob = c(aleph=1/5, beth=1/6, gimel=1/7, daleth=1/8, he=1/9, waw=1/10, zayin=1/11, heth=1759/27720)
  # Note that R doesn't actually require the weights
  # vector for rmultinom to sum to 1.
hebrew = c(rmultinom(1, 1e6, prob))
d = data.frame(
    Requested = prob,
    Obtained = hebrew/sum(hebrew))
print(d)
```


Sample output:

```txt
        Requested Obtained
aleph  0.20000000 0.200311
beth   0.16666667 0.167160
gimel  0.14285714 0.141997
daleth 0.12500000 0.124644
he     0.11111111 0.110984
waw    0.10000000 0.099927
zayin  0.09090909 0.091365
heth   0.06345599 0.063612
```


A histogram of the data is also possible using, for example,

```R
library(ggplot2)
qplot(factor(names(prob), levels = names(prob)), hebrew, geom = "histogram")
```



## Racket


probabalistic-choice uses inexact (float) arithmetic

probabalistic-choice/exact uses fractions and greatest common denominators and the likes

The test submodule is used for unit tests, and is not run when this code is loaded
as a module. Either run the program in DrRacket or run `raco test prob-choice.rkt`


```racket
#lang racket
;;; returns a probabalistic choice from the sequence choices
;;; choices generates two values -- the chosen value and a
;;; probability (weight) of the choice.
;;;
;;; Note that a hash where keys are choices and values are probabilities
;;; is such a sequence.
;;;
;;; if the total probability < 1 then choice could return #f
;;; if the total probability > 1 then some choices may be impossible
(define (probabalistic-choice choices)
  (let-values
      (((_ choice) ;; the fold provides two values, we only need the second
                   ;; the first will always be a negative number showing that
                   ;; I've run out of random steam
        (for/fold
            ((rnd (random))
             (choice #f))
          (((v p) choices)
           #:break (<= rnd 0))
          (values (- rnd p) v))))
    choice))

;;; ditto, but all probabilities must be exact rationals
;;; the optional lcd
;;;
;;; not the most efficient, since it provides a wrapper (and demo)
;;; for p-c/i-w below
(define (probabalistic-choice/exact
         choices
         #:gcd (GCD (/ (apply gcd (hash-values choices)))))
  (probabalistic-choice/integer-weights
   (for/hash (((k v) choices))
     (values k (* v GCD)))
   #:sum-of-weights GCD))

;;; this proves useful in Rock-Paper-Scissors
(define (probabalistic-choice/integer-weights
         choices
         #:sum-of-weights (sum-of-weights (apply + (hash-values choices))))
  (let-values
      (((_ choice)
        (for/fold
            ((rnd (random sum-of-weights))
             (choice #f))
          (((v p) choices)
           #:break (< rnd 0))
          (values (- rnd p) v))))
    choice))

(module+ test
  (define test-samples (make-parameter 1000000))

  (define (test-p-c-function f w)
    (define test-selection (make-hash))
    (for* ((i (in-range 0 (test-samples)))
           (c (in-value (f w))))
      (when (zero? (modulo i 100000)) (eprintf "~a," (quotient i 100000)))
      (hash-update! test-selection c add1 0))
    (printf "~a~%choice\tcount\texpected\tratio\terror~%" f)
    (for* (((k v) (in-hash test-selection))
           (e (in-value (* (test-samples) (hash-ref w k)))))
      (printf "~a\t~a\t~a\t~a\t~a%~%"
              k v e
              (/ v (test-samples))
              (real->decimal-string
               (exact->inexact (* 100 (/ (- v e) e)))))))

  (define test-weightings/rosetta
    (hash
     'aleph 1/5
     'beth 1/6
     'gimel 1/7
     'daleth 1/8
     'he 1/9
     'waw 1/10
     'zayin 1/11
     'heth 1759/27720; adjusted so that probabilities add to 1
     ))

  (define test-weightings/50:50 (hash 'woo 1/2 'yay 1/2))
  (define test-weightings/1:2:3 (hash 'woo 1 'yay 2 'foo 3))

  (test-p-c-function probabalistic-choice test-weightings/50:50)
  (test-p-c-function probabalistic-choice/exact test-weightings/50:50)
  (test-p-c-function probabalistic-choice test-weightings/rosetta)
  (test-p-c-function probabalistic-choice/exact test-weightings/rosetta))
```

Output (note that the progress counts, which go to standard error, are
interleaved with the output on standard out)

```txt
0,1,2,3,4,5,6,7,8,9,#<procedure:probabalistic-choice>
choice	count	expected	ratio	error
yay	499744	500000	15617/31250	-0.05%
woo	500256	500000	15633/31250	0.05%
0,1,2,3,4,5,6,7,8,9,#<procedure:probabalistic-choice/exact>
choice	count	expected	ratio	error
yay	499852	500000	124963/250000	-0.03%
woo	500148	500000	125037/250000	0.03%
0,1,2,3,4,5,6,7,8,9,#<procedure:probabalistic-choice>
choice	count	expected	ratio	error
daleth	124964	125000	31241/250000	-0.03%
zayin	90233	1000000/11	90233/1000000	-0.74%
gimel	142494	1000000/7	71247/500000	-0.25%
heth	64045	43975000/693	12809/200000	0.93%
aleph	199690	200000	19969/100000	-0.15%
beth	166861	500000/3	166861/1000000	0.12%
waw	100075	100000	4003/40000	0.07%
he	111638	1000000/9	55819/500000	0.47%
0,1,2,3,4,5,6,7,8,9,#<procedure:probabalistic-choice/exact>
choice	count	expected	ratio	error
beth	166423	500000/3	166423/1000000	-0.15%
heth	63462	43975000/693	31731/500000	0.01%
daleth	125091	125000	125091/1000000	0.07%
waw	99820	100000	4991/50000	-0.18%
aleph	200669	200000	200669/1000000	0.33%
gimel	142782	1000000/7	71391/500000	-0.05%
zayin	90478	1000000/11	45239/500000	-0.47%
he	111275	1000000/9	4451/40000	0.15%
```



## REXX

Note:   REXX can generate random numbers up to   100,000.

```rexx
/*REXX program displays results of probabilistic choices, gen random #s per probability.*/
parse arg trials digs seed .                     /*obtain the optional arguments from CL*/
if trials=='' | trials==","  then trials=1000000 /*Not specified?  Then use the default.*/
if   digs=='' |   digs==","  then   digs=15      /* "      "         "   "   "     "    */
if datatype(seed, 'W')  then call random ,,seed  /*allows repeatability for RANDOM nums.*/
numeric digits digs                              /*use a specific number of decimal digs*/
names= 'aleph beth gimel daleth he waw zayin heth ───totals───►'   /*names of the cells.*/
HI=100000                                                          /*max REXX RANDOM num*/
z=words(names);        #=z - 1                   /*#≡the number of actual/useable names.*/
$=0                                              /*initialize sum of the probabilities. */
           do n=1  for #;   prob.n=1 / (n+4);   if n==#  then prob.n= 1759 / 27720
           $=$ + prob.n;   Hprob.n=prob.n * HI
           end   /*n*/
prob.z=$                                         /*define the value of the ───totals───.*/
@.=0                                             /*initialize all counters in the range.*/
@.z=trials                                       /*define the last counter of  "    "   */
           do j=1  for trials;    r=random(HI)   /*gen  TRIAL  number of random numbers.*/
              do k=1  for #                      /*for each cell, compute  percentages. */
              if r<=Hprob.k  then @.k=@.k + 1    /* "    "    "  range, bump the counter*/
              end   /*k*/
           end      /*j*/
_= '═'                                           /*_:  a literal used for CENTER BIF pad*/
w=digs + 6                                       /*W:  display width for the percentages*/
d=4 + max( length(trials), length('count') )     /* [↓]  display a formatted top header.*/
say center('name',15,_)  center('count',d,_) center('target %',w,_) center('actual %',w,_)

     do cell=1  for z                            /*display each of the cells and totals.*/
     say  ' '   left( word(names, cell), 13)                    right(@.cell, d-2)  " ",
                left( format(   prob.cell   * 100, d),   w-2),
                left( format( @.cell/trials * 100, d),   w-2)
     if cell==#  then  say  center(_,15,_)   center(_,d,_)   center(_,w,_)   center(_,w,_)
     end   /*c*/                                 /* [↑]  display a formatted foot header*/
                                                 /*stick a fork in it,  we are all done.*/
```

{{out|output|text=  when using the default input:}}

```txt

═════name══════ ═══count═══ ══════target %═══════ ══════actual %═══════
  aleph            200135            20                  20.0135
  beth             166912            16.6666666          16.6912
  gimel            143222            14.2857142          14.3222
  daleth           124991            12.5                12.4991
  he               111259            11.1111111          11.1259
  waw              100049            10                  10.0049
  zayin             90978             9.0909090           9.0978
  heth              63278             6.3455988           6.3278
═══════════════ ═══════════ ═════════════════════ ═════════════════════
  ───totals───►   1000000           100                 100

```



## Ring


```ring

# Project : Probabilistic choice

cnt = list(8)
item = ["aleph","beth","gimel","daleth","he","waw","zayin","heth"]
prob  = [1/5.0, 1/6.0, 1/7.0, 1/8.0, 1/9.0, 1/10.0, 1/11.0, 1759/27720]

for trial = 1 to 1000000
    r = random(10)/10
    p = 0
    for i = 1 to len(prob)
        p = p + prob[i]
        if r < p
           cnt[i] = cnt[i] + 1
           loop
        ok
    next
next

see "item     actual    theoretical" + nl
for i = 1 to len(item)
    see "" + item[i] + "    " + cnt[i]/1000000 + "    " + prob[i] + nl
next

```

Output:

```txt

item     actual      theoretical
aleph    0.091307    0.200000
beth     0.181073    0.166667
gimel    0.181884    0.142857
daleth   0.090985    0.125000
he       0.090958    0.111111
waw      0.091064    0.100000
zayin    0.091061    0.090909
heth     0           0.063456

```



## Ruby


```ruby
probabilities = {
  "aleph"  => 1/5.0,
  "beth"   => 1/6.0,
  "gimel"  => 1/7.0,
  "daleth" => 1/8.0,
  "he"     => 1/9.0,
  "waw"    => 1/10.0,
  "zayin"  => 1/11.0,
}
probabilities["heth"] = 1.0 - probabilities.each_value.inject(:+)
ordered_keys = probabilities.keys

sum, sums = 0.0, {}
ordered_keys.each do |key|
  sum += probabilities[key]
  sums[key] = sum
end

actual = Hash.new(0)

samples = 1_000_000
samples.times do
  r = rand
  for k in ordered_keys
    if r < sums[k]
      actual[k] += 1
      break
    end
  end
end

puts  "key     expected    actual        diff"
for k in ordered_keys
  act = Float(actual[k]) / samples
  val = probabilities[k]
  printf "%-8s%.8f  %.8f  %6.3f %%\n", k, val, act, 100*(act-val)/val
end
```


{{out}}

```txt

key     expected    actual        diff
aleph   0.20000000  0.19949200  -0.254 %
beth    0.16666667  0.16689900   0.139 %
gimel   0.14285714  0.14309300   0.165 %
daleth  0.12500000  0.12494200  -0.046 %
he      0.11111111  0.11037800  -0.660 %
waw     0.10000000  0.10030100   0.301 %
zayin   0.09090909  0.09162700   0.790 %
heth    0.06345599  0.06326800  -0.296 %

```



## Rust


```Rust
extern crate rand;

use rand::distributions::{IndependentSample, Sample, Weighted, WeightedChoice};
use rand::{weak_rng, Rng};

const DATA: [(&str, f64); 8] = [
    ("aleph", 1.0 / 5.0),
    ("beth", 1.0 / 6.0),
    ("gimel", 1.0 / 7.0),
    ("daleth", 1.0 / 8.0),
    ("he", 1.0 / 9.0),
    ("waw", 1.0 / 10.0),
    ("zayin", 1.0 / 11.0),
    ("heth", 1759.0 / 27720.0),
];

const SAMPLES: usize = 1_000_000;

/// Generate a mapping to be used by `WeightedChoice`
fn gen_mapping() -> Vec<Weighted<usize>> {
    DATA.iter()
        .enumerate()
        .map(|(i, &(_, p))| Weighted {
            // `WeightedChoice` requires `u32` weights rather than raw probabilities.  For each
            // probability, we convert it to a `u32` weight, and associate it with an index. We
            // multiply by a constant because small numbers such as 0.2 when casted to `u32`
            // become `0`.  This conversion decreases the accuracy of the mapping, which is why we
            // provide an implementation which uses `f64`s for the best accuracy.
            weight: (p * 1_000_000_000.0) as u32,
            item: i,
        })
        .collect()
}

/// Generate a mapping of the raw probabilities
fn gen_mapping_float() -> Vec<f64> {
    // This does the work of `WeightedChoice::new`, splitting a number into various ranges.  The
    // `item` of `Weighted` is represented here merely by the probability's position in the `Vec`.
    let mut running_total = 0.0;
    DATA.iter()
        .map(|&(_, p)| {
            running_total += p;
            running_total
        })
        .collect()
}

/// An implementation of `WeightedChoice` which uses probabilities rather than weights.  Refer to
/// the `WeightedChoice` source for serious usage.
struct WcFloat {
    mapping: Vec<f64>,
}

impl WcFloat {
    fn new(mapping: &[f64]) -> Self {
        Self {
            mapping: mapping.to_vec(),
        }
    }

    // This is roughly the same logic as `WeightedChoice::ind_sample` (though is likely slower)
    fn search(&self, sample_prob: f64) -> usize {
        let idx = self.mapping
            .binary_search_by(|p| p.partial_cmp(&sample_prob).unwrap());
        match idx {
            Ok(i) | Err(i) => i,
        }
    }
}

impl IndependentSample<usize> for WcFloat {
    fn ind_sample<R: Rng>(&self, rng: &mut R) -> usize {
        // Because we know the total is exactly 1.0, we can merely use a raw float value.
        // Otherwise caching `Range::new(0.0, running_total)` and sampling with
        // `range.ind_sample(&mut rng)` is recommended.
        let sample_prob = rng.next_f64();
        self.search(sample_prob)
    }
}

impl Sample<usize> for WcFloat {
    fn sample<R: Rng>(&mut self, rng: &mut R) -> usize {
        self.ind_sample(rng)
    }
}

fn take_samples<R: Rng, T>(rng: &mut R, wc: &T) -> [usize; 8]
where
    T: IndependentSample<usize>,
{
    let mut counts = [0; 8];
    for _ in 0..SAMPLES {
        let sample = wc.ind_sample(rng);
        counts[sample] += 1;
    }
    counts
}

fn print_mapping(counts: &[usize]) {
    println!("Item   | Expected | Actual   ");
    println!("-------+----------+----------");
    for (&(name, expected), &count) in DATA.iter().zip(counts.iter()) {
        let real = count as f64 / SAMPLES as f64;
        println!("{:6} | {:.6} | {:.6}", name, expected, real);
    }
}

fn main() {
    let mut rng = weak_rng();

    println!("    ~~~ U32 METHOD ~~~");
    let mut mapping = gen_mapping();
    let wc = WeightedChoice::new(&mut mapping);

    let counts = take_samples(&mut rng, &wc);
    print_mapping(&counts);

    println!();

    println!("   ~~~ FLOAT METHOD ~~~");
    // initialize the float version of `WeightedChoice`
    let mapping = gen_mapping_float();
    let wc = WcFloat::new(&mapping);

    let counts = take_samples(&mut rng, &wc);
    print_mapping(&counts);
}
```

{{out}}

```txt
    ~~~ U32 METHOD ~~~
Item   | Expected | Actual
-------+----------+----------
aleph  | 0.200000 | 0.200195
beth   | 0.166667 | 0.166182
gimel  | 0.142857 | 0.142502
daleth | 0.125000 | 0.125503
he     | 0.111111 | 0.110820
waw    | 0.100000 | 0.100166
zayin  | 0.090909 | 0.090927
heth   | 0.063456 | 0.063705

   ~~~ FLOAT METHOD ~~~
Item   | Expected | Actual
-------+----------+----------
aleph  | 0.200000 | 0.199984
beth   | 0.166667 | 0.166634
gimel  | 0.142857 | 0.143218
daleth | 0.125000 | 0.124956
he     | 0.111111 | 0.111047
waw    | 0.100000 | 0.099805
zayin  | 0.090909 | 0.090513
heth   | 0.063456 | 0.063843
```



## Scala

This algorithm consists of a concise two-line tail-recursive loop (<tt>def weighted</tt>). The rest of the code is for API robustness, testing and display. <tt>weightedProb</tt> is for the task as stated (0 < <i>p</i> < 1), and <tt>weightedFreq</tt> is the equivalent based on integer frequencies (<i>f</i> >= 0).

```Scala
object ProbabilisticChoice extends App {
  import scala.collection.mutable.LinkedHashMap

  def weightedProb[A](prob: LinkedHashMap[A,Double]): A = {
    require(prob.forall{case (_, p) => p > 0 && p < 1})
    assume(prob.values.sum == 1)
    def weighted(todo: Iterator[(A,Double)], rand: Double, accum: Double = 0): A = todo.next match {
      case (s, i) if rand < (accum + i) => s
      case (_, i) => weighted(todo, rand, accum + i)
    }
    weighted(prob.toIterator, scala.util.Random.nextDouble)
  }

  def weightedFreq[A](freq: LinkedHashMap[A,Int]): A = {
    require(freq.forall{case (_, f) => f >= 0})
    require(freq.values.sum > 0)
    def weighted(todo: Iterator[(A,Int)], rand: Int, accum: Int = 0): A = todo.next match {
      case (s, i) if rand < (accum + i) => s
      case (_, i) => weighted(todo, rand, accum + i)
    }
    weighted(freq.toIterator, scala.util.Random.nextInt(freq.values.sum))
  }

  // Tests:

  val probabilities = LinkedHashMap(
    'aleph  -> 1.0/5,
    'beth   -> 1.0/6,
    'gimel  -> 1.0/7,
    'daleth -> 1.0/8,
    'he     -> 1.0/9,
    'waw    -> 1.0/10,
    'zayin  -> 1.0/11,
    'heth   -> 1759.0/27720
  )

  val frequencies = LinkedHashMap(
    'aleph  -> 200,
    'beth   -> 167,
    'gimel  -> 143,
    'daleth -> 125,
    'he     -> 111,
    'waw    -> 100,
    'zayin  -> 91,
    'heth   -> 63
  )

  def check[A](original: LinkedHashMap[A,Double], results: Seq[A]) {
    val freq = results.groupBy(x => x).mapValues(_.size.toDouble/results.size)
    original.foreach{case (k, v) =>
      val a = v/original.values.sum
      val b = freq(k)
      val c = if (Math.abs(a - b) < 0.001) "ok" else "**"
      println(f"$k%10s  $a%.4f  $b%.4f  $c")
    }
    println(" "*10 + f"  ${1}%.4f  ${freq.values.sum}%.4f")
  }

  println("Checking weighted probabilities:")
  check(probabilities, for (i <- 1 to 1000000) yield weightedProb(probabilities))
  println
  println("Checking weighted frequencies:")
  check(frequencies.map{case (a, b) => a -> b.toDouble}, for (i <- 1 to 1000000) yield weightedFreq(frequencies))
}
```

{{out}}

```txt
Checking weighted probabilities:
    'aleph  0.2000  0.2001  ok
     'beth  0.1667  0.1665  ok
    'gimel  0.1429  0.1430  ok
   'daleth  0.1250  0.1248  ok
       'he  0.1111  0.1112  ok
      'waw  0.1000  0.1000  ok
    'zayin  0.0909  0.0911  ok
     'heth  0.0635  0.0632  ok
            1.0000  1.0000

Checking weighted frequencies:
    'aleph  0.2000  0.2000  ok
     'beth  0.1670  0.1672  ok
    'gimel  0.1430  0.1432  ok
   'daleth  0.1250  0.1243  ok
       'he  0.1110  0.1105  ok
      'waw  0.1000  0.1002  ok
    'zayin  0.0910  0.0913  ok
     'heth  0.0630  0.0632  ok
            1.0000  1.0000
```



## Seed7

To reduce the runtime this program should be compiled.

```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const type: letter is new enum
    aleph, beth, gimel, daleth, he, waw, zayin, heth
  end enum;

const func string: str (in letter: aLetter) is
    return [] ("aleph", "beth", "gimel", "daleth", "he", "waw", "zayin", "heth") [succ(ord(aLetter))];

enable_output(letter);

const array [letter] integer: table is [letter] (
    5544, 4620, 3960, 3465, 3080, 2772, 2520, 1759);

const func letter: randomLetter is func
  result
    var letter: resultLetter is aleph;
  local
    var integer: number is 0;
  begin
    number := rand(1, 27720);
    while number > table[resultLetter] do
      number -:= table[resultLetter];
      incr(resultLetter);
    end while;
  end func;

const proc: main is func
  local
    var integer: count is 0;
    var letter: aLetter is aleph;
    var array [letter] integer: occurrence is letter times 0;
  begin
    for count range 1 to 1000000 do
      aLetter := randomLetter;
      incr(occurrence[aLetter]);
    end for;
    writeln("Name   Count  Ratio    Expected");
    for aLetter range letter.first to letter.last do
      writeln(aLetter rpad 7 <& occurrence[aLetter] lpad 6 <&
              flt(occurrence[aLetter]) / 10000.9 digits 4 lpad 8 <& "%" <&
              100.0 * flt(table[aLetter]) / 27720.0 digits 4 lpad 8 <& "%");
    end for;
  end func;
```


Outout:

```txt

Name   Count  Ratio    Expected
aleph  199788 19.9770% 20.0000%
beth   166897 16.6882% 16.6667%
gimel  143103 14.3090% 14.2857%
daleth 125060 12.5049% 12.5000%
he     110848 11.0838% 11.1111%
waw     99550  9.9541% 10.0000%
zayin   90918  9.0910%  9.0909%
heth    63836  6.3830%  6.3456%

```



## Scheme


Using guile scheme 2.0.11.


```scheme
(use-modules (ice-9 format))

(define (random-choice probs)
  (define choice (random 1.0))
  (define (helper val prob-lis)
    (let ((nval (- val (cadar prob-lis))))
      (if
       (< nval 0)
       (caar prob-lis)
       (helper nval (cdr prob-lis)))))
  (helper choice probs))

(define (add-result result delta table)
  (cond
   ((null? table) (list (list result delta)))
   ((eq? (caar table) result)
    (cons (list result (+ (cadar table) delta)) (cdr table)))
   (#t (cons (car table) (add-result result delta (cdr table))))))

(define (choices trials probs)
  (define (helper trial-num freq-table)
    (if
     (= trial-num trials)
     freq-table
     (helper
      (+ trial-num 1)
      (add-result (random-choice probs) (/ 1 trials) freq-table))))
  (helper 0 '()))

(define (format-results probs results)
  (for-each
   (lambda (x)
     (format
      #t
      "~10a~10,5f~10,5f~%"
      (car x)
      (cadr x)
      (cadr (assoc (car x) results))))
   probs))

(define probs
  '((aleph 1/5) (beth 1/6) (gimel 1/7) (daleth 1/8)
    (he 1/9) (waw 1/10) (zayin 1/11) (heth 1759/27720)))

(format-results probs (choices 1000000 probs))
```


Example output:

 aleph        0.20000   0.20051
 beth         0.16667   0.16680
 gimel        0.14286   0.14231
 daleth       0.12500   0.12538
 he           0.11111   0.11136
 waw          0.10000   0.09955
 zayin        0.09091   0.09096
 heth         0.06346   0.06313


## Sidef

{{trans|Perl}}

```ruby
define TRIALS = 1e4;
 
func prob_choice_picker(options) {
    var n = 0;
    var a = [];
    options.each { |k,v|
        n += v;
        a << [n, k];
    }
    func {
        var r = 1.rand;
        a.first{|e| r <= e[0] }[1];
    }
}
 
var ps = Hash(
   aleph  => 1/5,
   beth   => 1/6,
   gimel  => 1/7,
   daleth => 1/8,
   he     => 1/9,
   waw    => 1/10,
   zayin  => 1/11
)
 
ps{:heth} = (1 - ps.values.sum)
 
var picker = prob_choice_picker(ps)
var results = Hash()
 
TRIALS.times {
    results{picker()} := 0 ++;
}
 
say "Event   Occurred  Expected  Difference";
for k,v in (results.sort_by {|k| results{k} }.reverse) {
    printf("%-6s  %f  %f  %f\n",
        k, v/TRIALS, ps{k},
        abs(v/TRIALS - ps{k})
    );
}
```


{{out}}

```txt

Event   Occurred  Expected  Difference
aleph   0.196300  0.200000  0.003700
beth    0.165600  0.166667  0.001067
gimel   0.143700  0.142857  0.000843
daleth  0.123900  0.125000  0.001100
he      0.111800  0.111111  0.000689
waw     0.101900  0.100000  0.001900
zayin   0.088100  0.090909  0.002809
heth    0.068800  0.063456  0.005344

```



## Stata


```stata
clear
mata
letters="aleph","beth","gimel","daleth","he","waw","zayin","heth"
a=letters[rdiscrete(10000,1,(1/5,1/6,1/7,1/8,1/9,1/10,1/11,1759/27720))]'
st_addobs(10000)
st_addvar("str10","a")
st_sstore(.,.,a)
end
```



## Tcl


```tcl
package require Tcl 8.5

set map [dict create]
set sum 0.0

foreach name {aleph beth gimel daleth he waw zayin} \
        prob {1/5.0 1/6.0 1/7.0 1/8.0 1/9.0 1/10.0 1/11.0} \
{
    set prob [expr $prob]
    set sum [expr {$sum + $prob}]
    dict set map $name [dict create probability $prob limit $sum count 0]
}
dict set map heth [dict create probability [expr {1.0 - $sum}] limit 1.0 count 0]

set samples 1000000
for {set i 0} {$i < $samples} {incr i} {
    set n [expr {rand()}]
    foreach name [dict keys $map] {
        if {$n <= [dict get $map $name limit]} {
            set count [dict get $map $name count]
            dict set map $name count [incr count]
            break
        }
    }
}

puts "using $samples samples:"
puts [format "%-10s %-21s %-9s %s" "" expected actual difference]

dict for {name submap} $map {
    dict with submap {
        set actual [expr {$count * 1.0 / $samples}]
        puts [format "%-10s %-21s %-9s %4.2f%%" $name $probability $actual \
                [expr {abs($actual - $probability)/$probability*100.0}]
             ]
    }
}
```


```txt
using 1000000 samples:
           expected              actual    difference
aleph      0.2                   0.199641  0.18%
beth       0.16666666666666666   0.1674    0.44%
gimel      0.14285714285714285   0.143121  0.18%
daleth     0.125                 0.124864  0.11%
he         0.1111111111111111    0.111036  0.07%
waw        0.1                   0.100021  0.02%
zayin      0.09090909090909091   0.09018   0.80%
heth       0.06345598845598843   0.063737  0.44%
```



## Ursala


The stochasm library function used here constructs a weighted non-deterministic choice of
a set of functions. The pseudo-random number generator is a 64 bit Mersenne twistor
implemented by the run time system.


```Ursala
#import std
#import nat
#import flo

outcomes = <'aleph ','beth  ','gimel ','daleth','he    ','waw   ','zayin ','heth  '>
probabilities = ^lrNCT(~&,minus/1.+ plus:-0) div/*1. float* skip/5 iota12

simulation =

^(~&rn,div+ float~~rmPlX)^*D/~& iota; ^A(~&h,length)*K2+ * stochasm@p/probabilities !* outcomes

format =

:/'        frequency   probability'+  * ^lrlrTPT/~&n (printf/'%12.8f')^~/~&m outcomes-$probabilities@n

#show+

results = format simulation 1000000
```

output:

```txt

        frequency   probability
daleth  0.12484500  0.12500000
beth    0.16680600  0.16666667
aleph   0.19973700  0.20000000
waw     0.10016900  0.10000000
gimel   0.14293100  0.14285714
he      0.11131100  0.11111111
zayin   0.09104700  0.09090909
heth    0.06315400  0.06345599
```



## VBScript

Derived from the BBC BASIC version

```vb

item = Array("aleph","beth","gimel","daleth","he","waw","zayin","heth")
prob = Array(1/5.0, 1/6.0, 1/7.0, 1/8.0, 1/9.0, 1/10.0, 1/11.0, 1759/27720)
Dim cnt(7)

'Terminate script if sum of probabilities <> 1.
sum = 0
For i = 0 To UBound(prob)
	sum = sum + prob(i)
Next

If sum <> 1 Then
	WScript.Quit
End If

For trial = 1 To 1000000
	r = Rnd(1)
	p = 0
	For i = 0 To UBound(prob)
		p = p + prob(i)
		If r < p Then
			cnt(i) = cnt(i) + 1
			Exit For
		End If
	Next
Next

WScript.StdOut.Write "item" & vbTab & "actual" & vbTab & vbTab & "theoretical"
WScript.StdOut.WriteLine
For i = 0 To UBound(item)
	WScript.StdOut.Write item(i) & vbTab & FormatNumber(cnt(i)/1000000,6) & vbTab & FormatNumber(prob(i),6)
	WScript.StdOut.WriteLine
Next
```


{{out}}

```txt

item	actual		theoretical
aleph	0.199755	0.200000
beth	0.166861	0.166667
gimel	0.143240	0.142857
daleth	0.124474	0.125000
he	0.110879	0.111111
waw	0.100341	0.100000
zayin	0.090745	0.090909
heth	0.063705	0.063456

```



## XPL0


```XPL0
include c:\cxpl\codes;
def Size = 10_000_000;
int Tbl(12+1);
int I, J, N;
real X, S0, S1;
[for J:= 5 to 12 do Tbl(J):= 0;
 for I:= 0 to 1_000_000-1 do                    \generate one million items
        [N:= Ran(Size);
        for J:= 5 to 11 do
                [N:= N - Size/J;
                if N < 0 then [Tbl(J):= Tbl(J)+1;  J:= 100];
                ];
        if J=12 then Tbl(12):= Tbl(12)+1;
        ];
S0:= 0.0;  S1:= 0.0;
for J:= 5 to 11 do
        [X:= 1.0/float(J);                      RlOut(0, X);  S0:= S0+X;
         X:= float(Tbl(J)) / 1_000_000.0;       RlOut(0, X);  S1:= S1+X;
        CrLf(0);
        ];
X:= 1759.0 / 27720.0;                           RlOut(0, X);  S0:= S0+X;
X:= float(Tbl(12)) / 1_000_000.0;               RlOut(0, X);  S1:= S1+X;
CrLf(0);
Text(0, "    -------    -------
");
RlOut(0, S0);  RlOut(0, S1);
]
```


Output:

```txt

    0.20000    0.20012
    0.16667    0.16679
    0.14286    0.14305
    0.12500    0.12510
    0.11111    0.11113
    0.10000    0.09990
    0.09091    0.09077
    0.06346    0.06313
    -------    -------
    1.00000    1.00000

```



## zkl

{{trans|C}}

```zkl
var names=T("aleph", "beth", "gimel", "daleth",
			  "he", "waw", "zayin", "heth");
var ptable=T(5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0).apply('/.fp(1.0));
ptable=ptable.append(1.0-ptable.sum(0.0)); // add last weight to sum to 1.0
var [const] N=ptable.len();

fcn ridx{ i:=0; s:=(0.0).random(1);
   while((s-=ptable[i]) > 0) { i+=1 }
   i
}

const M=0d1_000_000;
var r=(0).pump(N,List,T(Ref,0));  // list of references to int 0
(0).pump(M,Void,fcn{r[ridx()].inc()}); // 1,000,000 weighted random #s

r=r.apply("value").apply("toFloat"); // (reference to int)-->int-->float

println("  Name  Count    Ratio Expected");
foreach i in (N){
   "%6s%7d %7.4f%% %7.4f%%".fmt(names[i], r[i], r[i]/M*100,
		ptable[i]*100).println();
}
```

{{out}}

```txt

  Name  Count    Ratio Expected
 aleph 200214 20.0214% 20.0000%
  beth 166399 16.6399% 16.6667%
 gimel 143100 14.3100% 14.2857%
daleth 125197 12.5197% 12.5000%
    he 111167 11.1167% 11.1111%
   waw 100097 10.0097% 10.0000%
 zayin  90692  9.0692%  9.0909%
  heth  63162  6.3162%  6.3456%

```

