+++
title = "Birthday problem"
description = ""
date = 2019-10-01T02:44:32Z
aliases = []
[extra]
id = 13900
[taxonomies]
categories = ["Probability and statistics", "task"]
languages = [
  "ada",
  "algol_68",
  "c",
  "d",
  "go",
  "hy",
  "j",
  "java",
  "julia",
  "kotlin",
  "lasso",
  "pari_gp",
  "pl_i",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "sql",
  "tcl",
  "zkl",
]
tags = []
+++

## Task

*Reference: [Wikipedia: Birthday Problem](https://en.wikipedia.org/wiki/Birthday_problem)*
[[Category:Discrete math]]


In [[wp:probability theory|probability theory]], the '''birthday problem''', or '''birthday [[wp:paradox|paradox]]''' This is not a paradox in the sense of leading to a [[wp:logic|logic]]al contradiction, but is called a paradox because the mathematical truth contradicts naïve [[wp:intuition (knowledge)|intuition]]: most people estimate that the chance is much lower than 50%. pertains to the [[wp:probability|probability]] that in a set of [[wp:random|random]]ly chosen people some pair of them will have the same [[wp:birthday|birthday]]. In a group of at least 23 randomly chosen people, there is more than 50% probability that some pair of them will both have been born on the same day. For 57 or more people, the probability is more than 99%, and it reaches 100% when the number of people reaches 366 (by the [[wp:pigeon hole principle|pigeon hole principle]], ignoring leap years). The mathematics behind this problem leads to a well-known cryptographic attack called the [[wp:birthday attack|birthday attack]].


;Task
Using simulation, estimate the number of independent people required in a groups before we can expect a ''better than even chance'' that at least 2 independent people in a group share a common birthday.  Furthermore: Simulate and thus estimate when we can expect a ''better than even chance'' that at least 3, 4 & 5 independent people of the group share a common birthday.  For simplicity assume that all of the people are alive...


;Suggestions for improvement
* Estimating the error in the estimate to help ensure the estimate is accurate to 4 decimal places.
* Converging to the <math>n</math><sup>th</sup> solution using a root finding method, as opposed to using an extensive search.
* Kudos (κῦδος) for finding the solution by proof (in a programming language) rather than by construction and simulation.


;See also
* Wolfram entry:   {{Wolfram|Birthday|Problem}}





## Ada


This solution assumes a 4-year cycle, with three 365-day years and one leap year.


```Ada
with Ada.Command_Line, Ada.Text_IO, Ada.Numerics.Discrete_random;

procedure Birthday_Test is

   Samples: constant Positive := Integer'Value(Ada.Command_Line.Argument(1));
   -- our experiment: Generate a X (birth-)days and check for Y-collisions
   -- the constant "Samples" is the number of repetitions of this experiment

   subtype Day is integer range 0 .. 365; -- this includes leap_days
   subtype Extended_Day is Integer range 0 .. 365*4; -- a four-year cycle
   package ANDR is new Ada.Numerics.Discrete_Random(Extended_Day);
   Random_Generator: ANDR.Generator;

   function Random_Day return Day is (ANDR.Random(Random_Generator) / 4);
   -- days 0 .. 364 are equally probable, leap-day 365 is 4* less probable

   type Checkpoint is record
      Multiplicity:  Positive;
      Person_Count:   Positive;
   end record;
   Checkpoints: constant array(Positive range <>) of Checkpoint
     := ( (2, 22),  (2, 23),  (3, 86),  (3, 87), (3, 88),
	  (4, 186), (4, 187), (5, 312), (5, 313), (5, 314) );
   type Result_Type is array(Checkpoints'Range) of Natural;
   Result: Result_Type := (others => 0);
   -- how often is a 2-collision in a group of 22 or 23, ..., a 5-collision
   -- in a group of 312 .. 314

   procedure Experiment(Result: in out Result_Type) is
   -- run the experiment once!
      A_Year: array(Day) of Natural := (others => 0);
      A_Day: Day;
      Multiplicity: Natural := 0;
      People: Positive := 1;
   begin
      for I in Checkpoints'Range loop
	 while People <= Checkpoints(I).Person_Count loop
	    A_Day := Random_Day;
	    A_Year(A_Day) := A_Year(A_Day)+1;
	    if A_Year(A_Day) > Multiplicity then
	       Multiplicity := Multiplicity + 1;
	    end if;
	    People := People + 1;
	 end loop;
	 if Multiplicity >= Checkpoints(I).Multiplicity then
	    Result(I) := Result(I) + 1;
            -- found a Multipl.-collision in a group of Person_Cnt.
	 end if;
      end loop;
   end Experiment;

   package TIO renames Ada.Text_IO;
   package FIO is new TIO.Float_IO(Float);

begin
    -- initialize the random generator
    ANDR.Reset(Random_Generator);

    -- repeat the experiment Samples times
    for I in 1 .. Samples loop
       Experiment(Result);
    end loop;

    -- print the results
    TIO.Put_Line("Birthday-Test with" & Integer'Image(Samples) & " samples:");
    for I in Result'Range loop
       FIO.Put(Float(Result(I))/Float(Samples), Fore => 3, Aft => 6, Exp => 0);
       TIO.Put_Line
	 ("% of groups with" & Integer'Image(Checkpoints(I).Person_Count) &
	  " have"            & Integer'Image(Checkpoints(I).Multiplicity) &
	  " persons sharing a common birthday.");
    end loop;
end Birthday_Test;
```


{{out}}

Running the program with a sample size 500_000_000 took about 25 minutes on a slow pc.


```txt
./birthday_test 500_000_000
Birthday-Test with 500000000 samples:
  0.475292% of groups with 22 have 2 persons sharing a common birthday.
  0.506882% of groups with 23 have 2 persons sharing a common birthday.
  0.487155% of groups with 86 have 3 persons sharing a common birthday.
  0.498788% of groups with 87 have 3 persons sharing a common birthday.
  0.510391% of groups with 88 have 3 persons sharing a common birthday.
  0.494970% of groups with 186 have 4 persons sharing a common birthday.
  0.501825% of groups with 187 have 4 persons sharing a common birthday.
  0.495137% of groups with 312 have 5 persons sharing a common birthday.
  0.500010% of groups with 313 have 5 persons sharing a common birthday.
  0.504888% of groups with 314 have 5 persons sharing a common birthday.
```


An interesting observation:
The probability for groups of 313 persons having 5 persons sharing a common birthday is almost exactly 0.5. Note that a solution based on 365-day years, i.e., a solution ignoring leap days, would generate slightly but significantly larger probabilities.


## ALGOL 68

{{works with|ALGOL 68|Revision 1}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.6 algol68g-2.6].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: Birthday_problem.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

REAL desired probability := 0.5; # 50% #

REAL upb year = 365 + 1/4 # - 3/400 but alive, ignore those born prior to 1901 #,
INT upb sample size = 100 000,
    upb common = 5 ;

FORMAT name int fmt = $g": "g(-0)"; "$,
       name real fmt = $g": "g(-0,4)"; "$,
       name percent fmt = $g": "g(-0,2)"%; "$;

printf((
  name real fmt,
  "upb year",upb year,
  name int fmt,
  "upb common",upb common,
  "upb sample size",upb sample size,
  $l$
));

INT required common := 1; # initial value #
FOR group size FROM required common WHILE required common <= upb common DO
  INT sample with no required common := 0;
  TO upb sample size DO
  # generate sample #
    [group size]INT sample;
    FOR i TO UPB sample DO sample[i] := ENTIER(random * upb year) + 1 OD;
    FOR birthday i TO UPB sample DO
      INT birthday = sample[birthday i];
      INT number in common := 1;
    # special case = 1 #
      IF number in common >= required common THEN
        found required common
      FI;
      FOR birthday j FROM birthday i + 1 TO UPB sample DO
        IF birthday = sample[birthday j] THEN
          number in common +:= 1;
          IF number in common >= required common THEN
            found required common
          FI
        FI
      OD
    OD  # days in year #;
    sample with no required common +:= 1;
    found required common: SKIP
  OD # sample size #;
  REAL portion of years with required common birthdays =
    (upb sample size - sample with no required common) / upb sample size;
  print(".");
  IF portion of years with required common birthdays > desired probability THEN
    printf((
      $l$,
      name int fmt,
      "required common",required common,
      "group size",group size,
      # "sample with no required common",sample with no required common, #
      name percent fmt,
      "%age of years with required common birthdays",portion of years with required common birthdays*100,
      $l$
    ));
    required common +:= 1
  FI
OD # group size #
```
'''Output:'''

```txt

upb year: 365.2500; upb common: 5; upb sample size: 100000;
.
required common: 1; group size: 1; %age of years with required common birthdays: 100.00%;
......................
required common: 2; group size: 23; %age of years with required common birthdays: 50.71%;
.................................................................
required common: 3; group size: 88; %age of years with required common birthdays: 50.90%;
...................................................................................................
required common: 4; group size: 187; %age of years with required common birthdays: 50.25%;
...............................................................................................................................
required common: 5; group size: 314; %age of years with required common birthdays: 50.66%;

```



## C

Computing probabilities to 5 sigmas of confidence. It's very slow, chiefly because to make sure a probability like 0.5006 is indeed above .5 instead of just statistical fluctuation, you have to run the simulation millions of times.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define DEBUG 0 // set this to 2 for a lot of numbers on output
#define DAYS 365
#define EXCESS (RAND_MAX / DAYS * DAYS)

int days[DAYS];

inline int rand_day(void)
{
	int n;
	while ((n = rand()) >= EXCESS);
	return n / (EXCESS / DAYS);
}

// given p people, if n of them have same birthday in one run
int simulate1(int p, int n)
{
	memset(days, 0, sizeof(days));

	while (p--)
		if (++days[rand_day()] == n) return 1;

	return 0;
}

// decide if the probability of n out of np people sharing a birthday
// is above or below p_thresh, with n_sigmas sigmas confidence
// note that if p_thresh is very low or hi, minimum runs need to be much higher
double prob(int np, int n, double n_sigmas, double p_thresh, double *std_dev)
{
	double p, d; // prob and std dev
	int runs = 0, yes = 0;
	do {
		yes += simulate1(np, n);
		p = (double) yes / ++runs;
		d = sqrt(p * (1 - p) / runs);
		if (DEBUG > 1)
			printf("\t\t%d: %d %d %g %g        \r", np, yes, runs, p, d);
	} while (runs < 10 || fabs(p - p_thresh) < n_sigmas * d);
	if (DEBUG > 1) putchar('\n');

	*std_dev = d;
	return p;
}

// bisect for truth
int find_half_chance(int n, double *p, double *dev)
{
	int lo, hi, mid;

reset:
	lo = 0;
	hi = DAYS * (n - 1) + 1;
	do {
		mid = (hi + lo) / 2;

		// 5 sigma confidence. Conventionally people think 3 sigmas are good
		// enough, but for case of 5 people sharing birthday, 3 sigmas actually
		// sometimes give a slightly wrong answer
		*p = prob(mid, n, 5, .5, dev);

		if (DEBUG)
			printf("\t%d %d %d %g %g\n", lo, mid, hi, *p, *dev);

		if (*p < .5)	lo = mid + 1;
		else		hi = mid;

		if (hi < lo) {
			// this happens when previous precisions were too low;
			// easiest fix: reset
			if (DEBUG) puts("\tMade a mess, will redo.");
			goto reset;
		}
	} while (lo < mid || *p < .5);

	return mid;
}

int main(void)
{
	int n, np;
	double p, d;
	srand(time(0));

	for (n = 2; n <= 5; n++) {
		np = find_half_chance(n, &p, &d);
		printf("%d collision: %d people, P = %g +/- %g\n",
			n, np, p, d);
	}

	return 0;
}
```

{{out}}

```txt

2 collision: 23 people, P = 0.508741 +/- 0.00174794
3 collision: 88 people, P = 0.509034 +/- 0.00180628
4 collision: 187 people, P = 0.501812 +/- 0.000362394
5 collision: 313 people, P = 0.500641 +/- 0.000128174

```



## D

{{trans|Python}}

```d
import std.stdio, std.random, std.algorithm, std.conv;

/// For sharing common birthday must all share same common day.
double equalBirthdays(in uint nSharers, in uint groupSize,
                      in uint nRepetitions, ref Xorshift rng) {
    uint eq = 0;

    foreach (immutable _; 0 .. nRepetitions) {
        uint[365] group;
        foreach (immutable __; 0 .. groupSize)
            group[uniform(0, $, rng)]++;
        eq += group[].any!(c => c >= nSharers);
    }

    return (eq * 100.0) / nRepetitions;
}

void main() {
    auto rng = 1.Xorshift; // Fixed seed.
    auto groupEst = 2;

    foreach (immutable sharers; 2 .. 6) {
        // Coarse.
        auto groupSize = groupEst + 1;
        while (equalBirthdays(sharers, groupSize, 100, rng) < 50.0)
            groupSize++;

        // Finer.
        immutable inf = to!int(groupSize - (groupSize - groupEst) / 4.0);
        foreach (immutable gs; inf .. groupSize + 999) {
            immutable eq = equalBirthdays(sharers, groupSize, 250, rng);
            if (eq > 50.0) {
                groupSize = gs;
                break;
            }
        }

        // Finest.
        foreach (immutable gs; groupSize - 1 .. groupSize + 999) {
            immutable eq = equalBirthdays(sharers, gs, 50_000, rng);
            if (eq > 50.0) {
                groupEst = gs;
                writefln("%d independent people in a group of %s share a common birthday. (%5.1f)",
                         sharers, gs, eq);
                break;
            }
        }
    }
}
```

{{out}}

```txt
2 independent people in a group of 23 share a common birthday. ( 50.5)
3 independent people in a group of 87 share a common birthday. ( 50.1)
4 independent people in a group of 187 share a common birthday. ( 50.2)
5 independent people in a group of 313 share a common birthday. ( 50.3)
```

Run-time about 10.4 seconds with ldc2 compiler.

Alternative version:
{{trans|C}}

```d
import std.stdio, std.random, std.math;

enum nDays = 365;

// 5 sigma confidence. Conventionally people think 3 sigmas are good
// enough, but for case of 5 people sharing birthday, 3 sigmas
// actually sometimes give a slightly wrong answer.
enum double nSigmas = 3.0; // Currently 3 for smaller run time.

/// Given n people, if m of them have same birthday in one run.
bool simulate1(in uint nPeople, in uint nCollisions, ref Xorshift rng)
/*nothrow*/ @safe /*@nogc*/ {
    static uint[nDays] days;
    days[] = 0;

    foreach (immutable _; 0 .. nPeople) {
        immutable day = uniform(0, days.length, rng);
        days[day]++;
        if (days[day] == nCollisions)
            return true;
    }
    return false;
}

/** Decide if the probablity of n out of np people sharing a birthday
is above or below pThresh, with nSigmas sigmas confidence.
If pThresh is very low or hi, minimum runs need to be much higher. */
double prob(in uint np, in uint nCollisions, in double pThresh,
            out double stdDev, ref Xorshift rng) {
    double p, d; // Probablity and standard deviation.
    uint nRuns = 0, yes = 0;

    do {
        yes += simulate1(np, nCollisions, rng);
        nRuns++;
        p = double(yes) / nRuns;
        d = sqrt(p * (1 - p) / nRuns);
        debug if (yes % 50_000 == 0)
            printf("\t\t%d: %d %d %g %g        \r", np, yes, nRuns, p, d);
    } while (nRuns < 10 || abs(p - pThresh) < (nSigmas * d));

    debug '\n'.putchar;

    stdDev = d;
    return p;
}

/// Bisect for truth.
uint findHalfChance(in uint nCollisions, out double p, out double dev, ref Xorshift rng) {
    uint mid;

    RESET:
    uint lo = 0;
    uint hi = nDays * (nCollisions - 1) + 1;

    do {
        mid = (hi + lo) / 2;
        p = prob(mid, nCollisions, 0.5, dev, rng);

        debug printf("\t%d %d %d %g %g\n", lo, mid, hi, p, dev);

        if (p < 0.5)
            lo = mid + 1;
        else
            hi = mid;

        if (hi < lo) {
            // This happens when previous precisions were too low;
            // easiest fix: reset.
            debug "\tMade a mess, will redo.".puts;
            goto RESET;
        }
    } while (lo < mid || p < 0.5);

    return mid;
}

void main() {
    auto rng = Xorshift(unpredictableSeed);

    foreach (immutable uint nCollisions; 2 .. 6) {
        double p, d;
        immutable np = findHalfChance(nCollisions, p, d, rng);
        writefln("%d collision: %d people, P = %g +/- %g", nCollisions, np, p, d);
    }
}
```

{{out}}

```txt
2 collision: 23 people, P = 0.521934 +/- 0.00728933
3 collision: 88 people, P = 0.512367 +/- 0.00411469
4 collision: 187 people, P = 0.506974 +/- 0.00232306
5 collision: 313 people, P = 0.501588 +/- 0.000529277
```


Output with nSigmas = 5.0:

```txt
2 collision: 23 people, P = 0.508607 +/- 0.00172133
3 collision: 88 people, P = 0.511945 +/- 0.00238885
4 collision: 187 people, P = 0.503229 +/- 0.000645587
5 collision: 313 people, P = 0.501105 +/- 0.000221016
```


## Go

{{trans|C}}

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

const (
    DEBUG   = 0
    DAYS    = 365
    n_sigmas = 5.
    WORKERS = 16   // concurrent worker processes
    RUNS    = 1000 // runs per flight
)

func simulate1(p, n int, r *rand.Rand) int {
    var days [DAYS]int
    for i := 0; i < p; i++ {
        days[r.Intn(DAYS)]++
    }
    for _, d := range days {
        if d >= n {
            return 1
        }
    }
    return 0
}

// send yes's per fixed number of simulate1 runs until canceled
func work(p, n int, ych chan int, cancel chan bool) {
    r := rand.New(rand.NewSource(time.Now().Unix() + rand.Int63()))
    for {
        select {
        case <-cancel:
            return
        default:
        }
        y := 0
        for i := 0; i < RUNS; i++ {
            y += simulate1(p, n, r)
        }
        ych <- y
    }
}

func prob(np, n int) (p, d float64) {
    ych := make(chan int, WORKERS)
    cancel := make(chan bool)
    for i := 0; i < WORKERS; i++ {
        go work(np, n, ych, cancel)
    }
    var runs, yes int
    for {
        yes += <-ych
        runs += RUNS
        fr := float64(runs)
        p = float64(yes) / fr
        d = math.Sqrt(p * (1 - p) / fr)
        if DEBUG > 1 {
            fmt.Println("\t\t", np, yes, runs, p, d)
        }
        // .5 here is the "even chance" threshold
        if !(math.Abs(p-.5) < n_sigmas*d) {
            close(cancel)
            break
        }
    }
    if DEBUG > 1 {
        fmt.Println()
    }
    return
}

func find_half_chance(n int) (mid int, p, dev float64) {
reset:
    lo := 0
    hi := DAYS*(n-1) + 1
    for {
        mid = (hi + lo) / 2
        p, dev = prob(mid, n)

        if DEBUG > 0 {
            fmt.Println("\t", lo, mid, hi, p, dev)
        }
        if p < .5 {
            lo = mid + 1
        } else {
            hi = mid
        }
        if hi < lo {
            if DEBUG > 0 {
                fmt.Println("\tMade a mess, will redo.")
            }
            goto reset
        }
        if !(lo < mid || p < .5) {
            break
        }
    }
    return
}

func main() {
    for n := 2; n <= 5; n++ {
        np, p, d := find_half_chance(n)
        fmt.Printf("%d collision: %d people, P = %.4f ± %.4f\n",
            n, np, p, d)
    }
}
```


```txt

2 collision: 23 people, P = 0.5081 ± 0.0016
3 collision: 88 people, P = 0.5155 ± 0.0029
4 collision: 187 people, P = 0.5041 ± 0.0008
5 collision: 313 people, P = 0.5015 ± 0.0003

```

'''Also based on the C version:'''

```go
package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
	"time"
)

type ProbeRes struct {
	np   int
	p, d float64
}

type Frac struct {
	n int
	d int
}

var DaysInYear int = 365

func main() {
	sigma := 5.0
	for i := 2; i <= 5; i++ {
		res := GetNP(i, sigma, 0.5)
		fmt.Printf("%d collision: %d people, P = %.4f ± %.4f\n",
			i, res.np, res.p, res.d)
	}
}

func GetNP(n int, n_sigmas, p_thresh float64) (res ProbeRes) {
	res.np = DaysInYear * (n - 1)
	for i := 0; i < DaysInYear*(n-1); i++ {
		tmp := probe(i, n, n_sigmas, p_thresh)
		if tmp.p > p_thresh && tmp.np < res.np {
			res = tmp
		}
	}
	return
}

var numCPU = runtime.NumCPU()

func probe(np, n int, n_sigmas, p_thresh float64) ProbeRes {
	var p, d float64
	var runs, yes int
	cRes := make(chan Frac, numCPU)
	for i := 0; i < numCPU; i++ {
		go SimN(np, n, 25, cRes)
	}
	for math.Abs(p-p_thresh) < n_sigmas*d || runs < 100 {
		f := <-cRes
		yes += f.n
		runs += f.d
		p = float64(yes) / float64(runs)
		d = math.Sqrt(p * (1 - p) / float64(runs))
		go SimN(np, n, runs/3, cRes)

	}
	return ProbeRes{np, p, d}
}
func SimN(np, n, ssize int, c chan Frac) {
	r := rand.New(rand.NewSource(time.Now().UnixNano() + rand.Int63()))
	yes := 0
	for i := 0; i < ssize; i++ {
		if Sim(np, n, r) {
			yes++
		}

	}
	c <- Frac{yes, ssize}
}
func Sim(p, n int, r *rand.Rand) (res bool) {
	Cal := make([]int, DaysInYear)
	for i := 0; i < p; i++ {
		Cal[r.Intn(DaysInYear)]++
	}
	for _, v := range Cal {
		if v >= n {
			res = true
		}
	}
	return
}
```

{{Out}}

```txt

2 collision: 23 people, P = 0.5068 ± 0.0013
3 collision: 88 people, P = 0.5148 ± 0.0028
4 collision: 187 people, P = 0.5020 ± 0.0004
5 collision: 313 people, P = 0.5011 ± 0.0002

```



## Hy


We use a simple but not very accurate simulation method.


```lisp
(import
  [numpy :as np]
  [random [randint]])

(defmacro incf (place)
  `(+= ~place 1))

(defn birthday [required &optional [reps 20000] [ndays 365]]
  (setv days (np.zeros (, reps ndays) np.int_))
  (setv qualifying-reps (np.zeros reps np.bool_))
  (setv group-size 1)
  (setv count 0)
  (while True
    ;(print group-size)
    (for [r (range reps)]
      (unless (get qualifying-reps r)
        (setv day (randint 0 (dec ndays)))
        (incf (get days (, r day)))
        (when (= (get days (, r day)) required)
          (setv (get qualifying-reps r) True)
          (incf count))))
    (when (> (/ (float count) reps) .5)
      (break))
    (incf group-size))
  group-size)

(print (birthday 2))
(print (birthday 3))
(print (birthday 4))
(print (birthday 5))
```



## J


Quicky approach (use a population of 1e5 people to get a quick estimate and then refine against a population of 1e8 people):


```J
PopSmall=: 1e5 ?@# 365
PopBig=: 1e8 ?@# 365

countShared=: [: >./ #/.~
avg=: +/ % #

probShared=: (1 :0)("0)
:
  NB. y: shared birthday count
  NB. m: population
  NB. x: sample size
  avg ,y <: (-x) countShared\ m
)

estGroupSz=: 3 :0
  approx=. (PopSmall probShared&y i.365) I. 0.5
  n=. approx-(2+y)
  refine=. n+(PopBig probShared&y approx+i:2+y) I. 0.5
  assert. (2+y) > |approx-refine
  refine, refine PopBig probShared y
)
```


Task cases:


```J
   estGroupSz 2
23 0.507254
   estGroupSz 3
88 0.510737
   estGroupSz 4
187 0.502878
   estGroupSz 5
313 0.500903
```


So, for example, we need a group of 88 to have at least a 50% chance of 3 people in the group having the same birthday in a year of 365 days.  And, in that case, the simulated probability was 51.0737%


## Java

Translation of [[Birthday_problem#Python|Python]] via [[Birthday_problem#D|D]]
{{works with|Java|8}}

```java
import static java.util.Arrays.stream;
import java.util.Random;

public class Test {

    static double equalBirthdays(int nSharers, int groupSize, int nRepetitions) {
        Random rand = new Random(1);

        int eq = 0;

        for (int i = 0; i < nRepetitions; i++) {
            int[] group = new int[365];
            for (int j = 0; j < groupSize; j++)
                group[rand.nextInt(group.length)]++;
            eq += stream(group).anyMatch(c -> c >= nSharers) ? 1 : 0;
        }

        return (eq * 100.0) / nRepetitions;
    }

    public static void main(String[] a) {

        int groupEst = 2;

        for (int sharers = 2; sharers < 6; sharers++) {
            // Coarse.
            int groupSize = groupEst + 1;
            while (equalBirthdays(sharers, groupSize, 100) < 50.0)
                groupSize++;

            // Finer.
            int inf = (int) (groupSize - (groupSize - groupEst) / 4.0);
            for (int gs = inf; gs < groupSize + 999; gs++) {
                double eq = equalBirthdays(sharers, groupSize, 250);
                if (eq > 50.0) {
                    groupSize = gs;
                    break;
                }
            }

            // Finest.
            for (int gs = groupSize - 1; gs < groupSize + 999; gs++) {
                double eq = equalBirthdays(sharers, gs, 50_000);
                if (eq > 50.0) {
                    groupEst = gs;
                    System.out.printf("%d independent people in a group of "
                            + "%s share a common birthday. (%5.1f)%n",
                            sharers, gs, eq);
                    break;
                }
            }
        }
    }
}
```



```txt
2 independent people in a group of 23 share a common birthday. ( 50,6)
3 independent people in a group of 87 share a common birthday. ( 50,4)
4 independent people in a group of 187 share a common birthday. ( 50,1)
5 independent people in a group of 314 share a common birthday. ( 50,2)
```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}


```julia
function equalbirthdays(sharers::Int, groupsize::Int; nrep::Int = 10000)
    eq = 0
    for _ in 1:nrep
        group = rand(1:365, groupsize)
        grset = Set(group)
        if groupsize - length(grset) ≥ sharers - 1 &&
            any(count(x -> x == d, group) ≥ sharers for d in grset)
            eq += 1
        end
    end
    return eq / nrep
end

gsizes = [2]
for sh in (2, 3, 4, 5)
    local gsize = gsizes[end]
    local freq

    # Coarse
    while equalbirthdays(sh, gsize; nrep = 100) < .5
        gsize += 1
    end
    # Finer
    for gsize in trunc(Int, gsize - (gsize - gsizes[end]) / 4):(gsize + 999)
        if equalbirthdays(sh, gsize; nrep = 250) > 0.5
            break
        end
    end
    # Finest
    for gsize in (gsize - 1):(gsize + 999)
        freq = equalbirthdays(sh, gsize; nrep = 50000)
        if freq > 0.5
            break
        end
    end

    push!(gsizes, gsize)
    @printf("%i independent people in a group of %s share a common birthday. (%5.3f)\n", sh, gsize, freq)
end
```


{{out}}

```txt
2 independent people in a group of 23 share a common birthday. (0.506)
3 independent people in a group of 88 share a common birthday. (0.510)
4 independent people in a group of 187 share a common birthday. (0.500)
5 independent people in a group of 314 share a common birthday. (0.507)
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.3

import java.util.Random

fun equalBirthdays(nSharers: Int, groupSize: Int, nRepetitions: Int): Double {
    val rand = Random(1L)
    var eq = 0
    for (i in 0 until nRepetitions) {
        val group = IntArray(365)
        for (j in 0 until groupSize) {
            group[rand.nextInt(group.size)]++
        }
        eq += if (group.any { it >= nSharers}) 1 else 0
    }
    return eq * 100.0 / nRepetitions
}

fun main(args: Array<String>) {
    var groupEst = 2
    for (sharers in 2..5) {
        // Coarse
        var groupSize = groupEst + 1
        while (equalBirthdays(sharers, groupSize, 100) < 50.0) groupSize++

        // Finer
        val inf = (groupSize - (groupSize - groupEst) / 4.0).toInt()
        for (gs in inf until groupSize + 999) {
            val eq = equalBirthdays(sharers, groupSize, 250)
            if (eq > 50.0) {
                groupSize = gs
                break
            }
        }

        // Finest
        for (gs in groupSize - 1 until groupSize + 999) {
            val eq = equalBirthdays(sharers, gs, 50_000)
            if (eq > 50.0) {
                groupEst = gs
                print("$sharers independent people in a group of ${"%3d".format(gs)} ")
                println("share a common birthday (${"%2.1f%%".format(eq)})")
                break
            }
        }
    }
}
```


{{out}}
Expect runtime of about 15 seconds on a modest laptop:

```txt

2 independent people in a group of  23 share a common birthday (50.6%)
3 independent people in a group of  87 share a common birthday (50.4%)
4 independent people in a group of 187 share a common birthday (50.1%)
5 independent people in a group of 314 share a common birthday (50.2%)

```



## Lasso


```Lasso
if(sys_listunboundmethods !>> 'randomgen') => {
	define randomgen(len::integer,max::integer)::array => {
		#len <= 0 ? return
		local(out = array)
		loop(#len) => { #out->insert(math_random(#max,1)) }
		return #out
	}
}
if(sys_listunboundmethods !>> 'hasdupe') => {
	define hasdupe(a::array,threshold::integer) => {
		with i in #a do => {
			#a->find(#i)->size > #threshold-1 ? return true
		}

		return false
	}
}
local(threshold = 2)
local(qty = 22, probability = 0.00, samplesize = 10000)
while(#probability < 50.00) => {^
	local(dupeqty = 0)
	loop(#samplesize) => {
		local(x = randomgen(#qty,365))
		hasdupe(#x,#threshold) ? #dupeqty++
	}
	#probability = (#dupeqty / decimal(#samplesize)) * 100

	'Threshold: '+#threshold+', qty: '+#qty+' - probability: '+#probability+'\r'
	#qty += 1
^}
```


{{out}}

```txt
Threshold: 2, qty: 22 - probability: 47.810000
Threshold: 2, qty: 23 - probability: 51.070000

Threshold: 3, qty: 86 - probability: 48.400000
Threshold: 3, qty: 87 - probability: 49.200000
Threshold: 3, qty: 88 - probability: 52.900000

Threshold: 4, qty: 184 - probability: 48.000000
Threshold: 4, qty: 185 - probability: 49.800000
Threshold: 4, qty: 186 - probability: 49.600000
Threshold: 4, qty: 187 - probability: 48.900000
Threshold: 4, qty: 188 - probability: 50.700000

Threshold: 5, qty: 308 - probability: 48.130000
Threshold: 5, qty: 309 - probability: 48.430000
Threshold: 5, qty: 310 - probability: 48.640000
Threshold: 5, qty: 311 - probability: 49.370000
Threshold: 5, qty: 312 - probability: 49.180000
Threshold: 5, qty: 313 - probability: 49.540000
Threshold: 5, qty: 314 - probability: 50.000000

```



## PARI/GP


```parigp
simulate(n)=my(v=vecsort(vector(n,i,random(365))),t,c=1); for(i=2,n,if(v[i]>v[i-1],t=max(t,c);c=1,c++)); t
find(n)=my(guess=365*n-342,t);while(1, t=sum(i=1,1e3,simulate(guess)>=n)/1e3; if(t>550, guess--); if(t<450, guess++); if(450<=t && t<=550, return(guess)))
find(2)
find(3)
find(4)
find(5)
```



## PL/I


```PL/I
*process source attributes xref;
 bd: Proc Options(main);
 /*--------------------------------------------------------------------
 * 04.11.2013 Walter Pachl
 * Take samp samples of groups with gs persons and check
 *how many of the groups have at least match persons with same birthday
 *-------------------------------------------------------------------*/
 Dcl (float,random) Builtin;
 Dcl samp Bin Fixed(31) Init(1000000);
 Dcl arr(0:366) Bin Fixed(31);
 Dcl r Bin fixed(31);
 Dcl i Bin fixed(31);
 Dcl ok Bin fixed(31);
 Dcl g  Bin fixed(31);
 Dcl gs Bin fixed(31);
 Dcl match Bin fixed(31);
 Dcl cnt(0:1) Bin Fixed(31);
 Dcl lo(6) Bin Fixed(31) Init(0,21,85,185,311,458);
 Dcl hi(6) Bin Fixed(31) Init(0,25,89,189,315,462);
 Dcl rf Bin Float(63);
 Dcl hits  Bin Float(63);
 Dcl arrow Char(3);
 Do match=2 To 6;
   Put Edit(' ')(Skip,a);
   Put Edit(samp,' samples. Percentage of groups with at least',
            match,' matches')(Skip,f(8),a,f(2),a);
   Put Edit('Group size')(Skip,a);
   Do gs=lo(match) To hi(match);
     cnt=0;
     Do i=1 To samp;
       ok=0;
       arr=0;
       Do g=1 To gs;
         rf=random();
         r=rf*365+1;
         arr(r)+=1;
         If arr(r)=match Then Do;
           /* Put Edit(r)(Skip,f(4));*/
           ok=1;
           End;
         End;
       cnt(ok)+=1;
       End;
     hits=float(cnt(1))/samp;
     If hits>=.5 Then arrow=' <-';
                 Else arrow='';
     Put Edit(gs,cnt(0),cnt(1),100*hits,'%',arrow)
             (Skip,f(10),2(f(7)),f(8,3),a,a);
     End;
   End;
 End;
```

Output:

```txt

 1000000 samples. Percentage of groups with at least 2 matches
Group size                               3000000      500000 samples
        21 556903 443097  44.310%        44.343%      44.347%
        22 524741 475259  47.526%        47.549%      47.521%
        23 492034 507966  50.797% <-     50.735% <-   50.722% <-
        24 462172 537828  53.783% <-     53.815% <-   53.838% <-
        25 431507 568493  56.849% <-     56.849% <-   56.842% <-

 1000000 samples. Percentage of groups with at least 3 matches
Group size
        85 523287 476713  47.671%        47.638%      47.631%
        86 512219 487781  48.778%        48.776%      48.821%
        87 499874 500126  50.013% <-     49.902%      49.903%
        88 488197 511803  51.180% <-     51.127% <-   51.096% <-
        89 478044 521956  52.196% <-     52.263% <-   52.290% <-

 1000000 samples. Percentage of groups with at least 4 matches
Group size
       185 511352 488648  48.865%        48.868%      48.921%
       186 503888 496112  49.611%        49.601%      49.568%
       187 497844 502156  50.216% <-     50.258% <-   50.297% <-
       188 490490 509510  50.951% <-     50.916% <-   50.946% <-
       189 482893 517107  51.711% <-     51.645% <-   51.655% <-

 1000000 samples. Percentage of groups with at least 5 matches
Group size
       311 508743 491257  49.126%        49.158%      49.164%
       312 503524 496476  49.648%        49.631%      49.596%
       313 498244 501756  50.176% <-     50.139% <-   50.095% <-
       314 494032 505968  50.597% <-     50.636% <-   50.586% <-
       315 489821 510179  51.018% <-     51.107% <-   51.114% <-

 1000000 samples. Percentage of groups with at least 6 matches
Group size
       458 505225 494775  49.478%        49.498%      49.512%
       459 501871 498129  49.813%        49.893%      49.885%
       460 497719 502281  50.228% <-     50.278% <-   50.248% <-
       461 493948 506052  50.605% <-     50.622% <-   50.626% <-
       462 489416 510584  51.058% <-     51.029% <-   51.055% <-
```

extended to verify REXX results:

```txt
 1000000 samples. Percentage of groups with at least 7 matches
Group size
       621 503758 496242  49.624%
       622 500320 499680  49.968%
       623 497047 502953  50.295% <-
       624 493679 506321  50.632% <-
       625 491240 508760  50.876% <-

 1000000 samples. Percentage of groups with at least 8 matches
Group size
       796 504764 495236  49.524%
       797 502537 497463  49.746%
       798 499488 500512  50.051% <-
       799 496658 503342  50.334% <-
       800 494773 505227  50.523% <-

 1000000 samples. Percentage of groups with at least 9 matches
Group size
       983 502613 497387  49.739%
       984 501665 498335  49.834%
       985 498606 501394  50.139% <-
       986 497453 502547  50.255% <-
       987 493816 506184  50.618% <-

 1000000 samples. Percentage of groups with at least10 matches
Group size
      1179 502910 497090  49.709%
      1180 500906 499094  49.909%
      1181 499079 500921  50.092% <-
      1182 496957 503043  50.304% <-
      1183 494414 505586  50.559% <-
```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use List::AllUtils qw(max min uniqnum count_by any);
use Math::Random qw(random_uniform_integer);

sub simulation {
    my($c) = shift;
    my $max_trials = 1_000_000;
    my $min_trials =    10_000;
    my $n = int 47 * ($c-1.5)**1.5; # OEIS/A050256: 16 86 185 307
    my $N = min $max_trials, max $min_trials, 1000 * sqrt $n;

    while (1) {
        my $yes = 0;
        for (1..$N) {
            my %birthday_freq = count_by { $_ } random_uniform_integer($n, 1, 365);
            $yes++ if any { $birthday_freq{$_} >= $c } keys %birthday_freq;
        }
        my $p = $yes/$N;
        return($n, $p) if $p > 0.5;
        $N = min $max_trials, max $min_trials, int 1000/(0.5-$p)**1.75;
        $n++;
    }
}

printf "$_ people in a group of %s share a common birthday. (%.4f)\n", simulation($_) for 2..5
```

{{out}}

```txt
2 people in a group of 23 share a common birthday. (0.5083)
3 people in a group of 88 share a common birthday. (0.5120)
4 people in a group of 187 share a common birthday. (0.5034)
5 people in a group of 313 share a common birthday. (0.5008)
```



## Perl 6

Gives correct answers, but more of a proof-of-concept at this point, even with max-trials at 250K it is too slow to be practical.

```perl6
sub simulation ($c) {
    my $max-trials = 250_000;
    my $min-trials =   5_000;
    my $n = floor 47 * ($c-1.5)**1.5; # OEIS/A050256: 16 86 185 307
    my $N = min $max-trials, max $min-trials, 1000 * sqrt $n;

    loop {
        my $p = $N R/ elems grep { .elems > 0 }, ((grep { $_>=$c }, values bag (^365).roll($n)) xx $N);
        return($n, $p) if $p > 0.5;
        $N = min $max-trials, max $min-trials, floor 1000/(0.5-$p);
        $n++;
   }
}

printf "$_ people in a group of %s share a common birthday. (%.3f)\n", simulation($_) for 2..5;
```

{{out}}

```txt
2 people in a group of 23 share a common birthday. (0.506)
3 people in a group of 88 share a common birthday. (0.511)
4 people in a group of 187 share a common birthday. (0.500)
5 people in a group of 313 share a common birthday. (0.507)
```



## Phix

{{trans|D}}

```Phix
constant nDays = 365

-- 5 sigma confidence. Conventionally people think 3 sigmas are
-- good enough, but for the case of 5 people sharing a birthday,
-- 3 sigmas actually sometimes gives a slightly wrong answer.
constant nSigmas = 5.0; -- Currently 3 for smaller run time.

function simulate1(integer nPeople, nCollisions)
--
-- Given n people, if m of them have same birthday in one run.
--
    sequence days = repeat(0,nDays)
    for p=1 to nPeople do
        integer day = rand(nDays)
        days[day] += 1
        if days[day] == nCollisions then
            return true
        end if
    end for
    return false;
end function

function prob(integer np, nCollisions, atom pThresh)
--
-- Decide if the probablity of n out of np people sharing a birthday
-- is above or below pThresh, with nSigmas sigmas confidence.
-- If pThresh is very low or hi, minimum runs need to be much higher.
--
    atom p, d; -- Probablity and standard deviation.
    integer nRuns = 0, yes = 0;

    while nRuns<10 or (abs(p - pThresh) < (nSigmas * d)) do
        yes += simulate1(np, nCollisions)
        nRuns += 1
        p = yes/nRuns
        d = sqrt(p * (1 - p) / nRuns);
    end while

    return {p,d}
end function

function findHalfChance(integer nCollisions)
-- Bisect for truth.
    atom p, dev
    integer mid = 1,
            lo = 0,
            hi = nDays * (nCollisions - 1) + 1;

    while lo < mid or p < 0.5 do
        mid = floor((hi + lo) / 2)
        {p,dev} = prob(mid, nCollisions, 0.5)

        if (p < 0.5) then
            lo = mid + 1;
        else
            hi = mid;
        end if

        if (hi < lo) then
            return findHalfChance(nCollisions) -- reset
        end if
    end while

    return {p,dev,mid}
end function

for nCollisions=2 to 6 do
    atom {p,d,np} = findHalfChance(nCollisions)
    printf(1,"%d collision: %d people, P = %g +/- %g\n", {nCollisions, np, p, d})
end for
```

{{out}}

```txt

2 collision: 23 people, P = 0.520699 +/- 0.00688426
3 collision: 88 people, P = 0.507159 +/- 0.00238534
4 collision: 187 people, P = 0.504129 +/- 0.00137625
5 collision: 313 people, P = 0.501219 +/- 0.000406284
6 collision: 460 people, P = 0.502131 +/- 0.000710091

```

Output with nSigmas = 5.0:

```txt

2 collision: 23 people, P = 0.507817 +/- 0.00156278
3 collision: 88 people, P = 0.512042 +/- 0.00240772
4 collision: 187 people, P = 0.502546 +/- 0.000509275
5 collision: 313 people, P = 0.501218 +/- 0.000243516
6 collision: 460 people, P = 0.502901 +/- 0.000580137

```



## Python

Note: the first (unused), version of function equal_birthdays() uses a different but equally valid interpretation of the phrase "common birthday".

```python

from random import randint

def equal_birthdays(sharers=2, groupsize=23, rep=100000):
    'Note: 4 sharing common birthday may have 2 dates shared between two people each'
    g = range(groupsize)
    sh = sharers - 1
    eq = sum((groupsize - len(set(randint(1,365) for i in g)) >= sh)
             for j in range(rep))
    return (eq * 100.) / rep

def equal_birthdays(sharers=2, groupsize=23, rep=100000):
    'Note: 4 sharing common birthday must all share same common day'
    g = range(groupsize)
    sh = sharers - 1
    eq = 0
    for j in range(rep):
        group = [randint(1,365) for i in g]
        if (groupsize - len(set(group)) >= sh and
            any( group.count(member) >= sharers for member in set(group))):
            eq += 1
    return (eq * 100.) / rep

group_est = [2]
for sharers in (2, 3, 4, 5):
    groupsize = group_est[-1]+1
    while equal_birthdays(sharers, groupsize, 100) < 50.:
        # Coarse
        groupsize += 1
    for groupsize in range(int(groupsize - (groupsize - group_est[-1])/4.), groupsize + 999):
        # Finer
        eq = equal_birthdays(sharers, groupsize, 250)
        if eq > 50.:
            break
    for groupsize in range(groupsize - 1, groupsize +999):
        # Finest
        eq = equal_birthdays(sharers, groupsize, 50000)
        if eq > 50.:
            break
    group_est.append(groupsize)
    print("%i independent people in a group of %s share a common birthday. (%5.1f)" % (sharers, groupsize, eq))
```


{{out}}

```txt
2 independent people in a group of 23 share a common birthday. ( 50.9)
3 independent people in a group of 87 share a common birthday. ( 50.0)
4 independent people in a group of 188 share a common birthday. ( 50.9)
5 independent people in a group of 314 share a common birthday. ( 50.6)
```



### Enumeration method

The following enumerates all birthday distributation of n people in a year. It's patentedly unscalable.

```python
from collections import defaultdict
days = 365

def find_half(c):

    # inc_people takes birthday combinations of n people and generates the
    # new set for n+1
    def inc_people(din, over):
        # 'over' is the number of combinations that have at least c people
        # sharing a birthday. These are not contained in the set.

        dout,over = defaultdict(int), over * days
        for k,s in din.items():
            for i,v in enumerate(k):
                if v + 1 >= c:
                    over += s
                else:
                    dout[tuple(sorted(k[0:i] + (v + 1,) + k[i+1:]))] += s
            dout[(1,) + k] += s * (days - len(k))
        return dout, over

    d, combos, good, n = {():1}, 1, 0, 0

    # increase number of people until at least half of the cases have at
    # at least c people sharing a birthday
    while True:
        n += 1
        combos *= days # or, combos = sum(d.values()) + good
        d,good = inc_people(d, good)

        #!!! print d.items()
        if good * 2 >= combos:
            return n, good, combos

# In all fairness, I don't know if the code works for x >= 4: I probably don't
# have enough RAM for it, and certainly not enough patience. But it should.
# In theory.
for x in range(2, 5):
    n, good, combos = find_half(x)
    print "%d of %d people sharing birthday: %d out of %d combos"% (x, n, good, combos)

```

{{out}}

```txt
2 of 23 people sharing birthday: 43450860051057961364418604769486195435604861663267741453125 out of 85651679353150321236814267844395152689354622364044189453125 combos
3 of 88 people sharing birthday: 1549702400401473425983277424737696914087385196361193892581987189461901608374448849589919219974092878625057027641693544686424625999709818279964664633586995549680467629183956971001416481439048256933422687688148710727691650390625 out of 3032299345394764867793392128292779133654078653518318790345269064871742118915665927782934165016667902517875712171754287171746462419635313222013443107339730598579399174951673950890087953259632858049599235528148710727691650390625 combos
...?

```


### Enumeration method #2


```python
# ought to use a memoize class for all this
# factorial
def fact(n, cache={0:1}):
    if not n in cache:
        cache[n] = n * fact(n - 1)
    return cache[n]

# permutations
def perm(n, k, cache={}):
    if not (n,k) in cache:
        cache[(n,k)] = fact(n) / fact(n - k)
    return cache[(n,k)]

def choose(n, k, cache={}):
    if not (n,k) in cache:
        cache[(n,k)] = perm(n, k) / fact(k)
    return cache[(n, k)]

# ways of distribute p people's birthdays into d days, with
# no more than m sharing any one day
def combos(d, p, m, cache={}):
    if not p: return 1
    if not m: return 0
    if p <= m: return d**p        # any combo would satisfy

    k = (d, p, m)
    if not k in cache:
        result = 0
        for x in range(0, p//m + 1):
            c = combos(d - x, p - x * m, m - 1)
            # ways to occupy x days with m people each
            if c: result += c * choose(d, x) * perm(p, x * m) / fact(m)**x
        cache[k] = result

    return cache[k]

def find_half(m):
    n = 0
    while True:
        n += 1
        total = 365 ** n
        c = total - combos(365, n, m - 1)
        if c * 2 >= total:
            print "%d of %d people: %d/%d combos" % (n, m, c, total)
            return

for x in range(2, 6): find_half(x)
```

{{out}}

```txt

23 of 2 people: 43450860....3125/85651679....3125 combos
88 of 3 people: 15497...50390625/30322...50390625 combos
187 of 4 people: 708046698...0703125/1408528546...0703125 combos
313 of 5 people: 498385488882289...2578125/99464149835930...2578125 combos

```



## Racket

{{trans|Python}}Based on the Python task. For three digits precision use 250000 repetitions. For four digits precision use 25000000 repetitions, but it’s very slow. See discussion page.

```Racket
#lang racket

#;(define repetitions 25000000) ; for \sigma=1/10000
(define repetitions 250000) ; for \sigma=1/1000
(define coarse-repetitions 2500)

(define (vector-inc! v pos)
  (vector-set! v pos (add1 (vector-ref v pos))))

(define (equal-birthdays sharers group-size repetitions)
  (/ (for/sum ([j (in-range repetitions)])
        (let ([days (make-vector 365 0)])
          (for ([person (in-range group-size)])
            (vector-inc! days (random 365)))
          (if (>= (apply max (vector->list days)) sharers)
              1 0)))
      repetitions))

(define (search-coarse-group-size sharers)
  (let loop ([coarse-group-size 2])
    (let ([coarse-probability
          (equal-birthdays sharers coarse-group-size coarse-repetitions)])
      (if (> coarse-probability .5)
          coarse-group-size
          (loop (add1 coarse-group-size))))))

(define (search-upwards sharers group-size)
  (let ([probability (equal-birthdays sharers group-size repetitions)])
    (if (> probability .5)
        (values group-size probability)
        (search-upwards sharers (add1 group-size)))))

(define (search-downwards sharers group-size last-probability)
  (let ([probability (equal-birthdays sharers group-size repetitions)])
    (if (> probability .5)
        (search-downwards sharers (sub1 group-size) probability)
        (values (add1 group-size) last-probability))))

(define (search-from sharers group-size)
  (let ([probability (equal-birthdays sharers group-size repetitions)])
    (if (> probability .5)
        (search-downwards sharers (sub1 group-size) probability)
        (search-upwards sharers (add1 group-size)))))

(for ([sharers (in-range 2 6)])
  (let-values ([(group-size probability)
                (search-from sharers (search-coarse-group-size sharers))])
    (printf "~a independent people in a group of ~a share a common birthday. (~a%)\n"
            sharers group-size  (~r (* probability 100) #:precision '(= 2)))))
```

'''Output'''

```txt
2 independent people in a group of 23 share a common birthday. (50.80%)
3 independent people in a group of 88 share a common birthday. (51.19%)
4 independent people in a group of 187 share a common birthday. (50.18%)
5 independent people in a group of 313 share a common birthday. (50.17%)

```



## REXX


### version 1

The method used is to find the average number of people to share a birthday,   and then use the   '''floor'''   of that

value   (less the group size)   as a starting point to find a new group size with an expected size that exceeds

50%   duplicate birthdays of the required size.

```rexx
/*REXX pgm examines the birthday problem via random # simulation (with specifable parms)*/
parse arg dups samp seed .                       /*get optional arguments from the CL.  */
if dups=='' | dups==","  then dups=    10        /*Not specified?  Then use the default.*/
if samp=='' | samp==","  then samp= 10000        /* "      "         "   "   "     "    */
if datatype(seed, 'W')   then call random ,,seed /*RANDOM seed given for repeatability ?*/
diy =365                 /*or:  diy=365.25 */    /*the number of    Days In a Year.     */
diyM=diy*100                                     /*this expands the RANDOM  (BIF) range.*/
            do   g=2  to dups;     s=0           /*perform through  2 ──► duplicate size*/
              do  samp;            @.=0          /*perform some number of trials.       */
                     do j=0  until @.day==g      /*perform until G dup. birthdays found.*/
                     day=random(1, diyM)  % 100  /*expand range RANDOM number generation*/
                     @.day=@.day + 1             /*record the number of common birthdays*/
                     end   /*j*/                 /* [↓]  adjust for the  DO  loop index.*/
              s=s + j                            /*add number of birthday hits to sum.  */
              end          /*samp*/              /* [↓]  % 1   rounds down the division.*/
            start.g= s/samp % 1  -  g            /*define where the  try─outs  start.   */
            end            /*g*/                 /* [↑]  get a rough estimate for %.    */
say right('sample size is '   samp, 40);   say   /*display this run's sample size.      */
say '          required         trial       %  with required'
say '         duplicates         size       common birthdays'
say '        ────────────      ───────     ──────────────────'
   do   g=2  to dups                             /*perform through  2 ──► duplicate size*/
     do try=start.g  until s/samp>=.5;   s=0     /*   "    try─outs until average ≥ 50%.*/
       do samp;                          @.=0    /*   "    some number of trials.       */
         do try;     day=random(1, diyM) % 100   /*   "    until G dup. birthdays found.*/
         @.day=@.day + 1                         /*record the number of common birthdays*/
         if @.day==g  then do; s=s+1; leave; end /*found enough  G  (birthday)  hits ?  */
         end   /*try;*/
       end     /*samp*/
     end       /*try=start.g*/                   /* [↑]  where the  try─outs  happen.   */
   say right(g, 15)     right(try, 15)      center( format( s / samp * 100, , 4)'%',  30)
   end         /*g*/                             /*stick a fork in it,  we're all done. */
```

{{out|output|text=   when using the default inputs:}}

```txt

                   sample size is  10000

          required         trial       %  with required
         duplicates         size       common birthdays
        ────────────      ───────     ──────────────────
              2              23            50.2300%
              3              87            50.2400%
              4             187            50.3800%
              5             312            50.0100%
              6             458            50.5200%
              7             622            50.3900%
              8             798            50.1700%
              9             984            50.5700%
             10            1182            51.4000%

```



### version 2


```rexx
 /*--------------------------------------------------------------------
 * 04.11.2013 Walter Pachl translated from PL/I
 * Take samp samples of groups with gs persons and check
 *how many of the groups have at least match persons with same birthday
 *-------------------------------------------------------------------*/
 samp=100000
 lo='0 21 85 185 311 458'
 hi='0 25 89 189 315 462'
 Do match=2 To 6
   Say ' '
   Say samp' samples . Percentage of groups with at least',
            match ' matches'
   Say 'Group size'
   Do gs=word(lo,match) To word(hi,match)
     cnt.=0
     Do i=1 To samp
       ok=0
       arr.=0
       Do g=1 To gs
         r=random(1,365)
         arr.r=arr.r+1
         If arr.r=match Then
           ok=1
         End
       cnt.ok=cnt.ok+1
       End
     hits=cnt.1/samp
     If hits>=.5 Then arrow=' <-'
                 Else arrow=''
     Say format(gs,10) cnt.0 cnt.1 100*hits||'%'||arrow
     End
   End
```

Output:

```txt

100000 samples . Percentage of groups with at least 2  matches
Group size
        21 55737 44263 44.26300%
        22 52158 47842 47.84200%
        23 49141 50859 50.85900% <-
        24 46227 53773 53.77300% <-
        25 43091 56909 56.90900% <-

100000 samples . Percentage of groups with at least 3  matches
Group size
        85 52193 47807 47.80700%
        86 51489 48511 48.51100%
        87 50146 49854 49.85400%
        88 48790 51210 51.2100% <-
        89 47771 52229 52.22900% <-

100000 samples . Percentage of groups with at least 4  matches
Group size
       185 50930 49070 49.0700%
       186 50506 49494 49.49400%
       187 49739 50261 50.26100% <-
       188 49024 50976 50.97600% <-
       189 48283 51717 51.71700% <-

100000 samples . Percentage of groups with at least 5  matches
Group size
       311 50909 49091 49.09100%
       312 50441 49559 49.55900%
       313 49912 50088 50.08800% <-
       314 49425 50575 50.57500% <-
       315 48930 51070 51.0700% <-

100000 samples . Percentage of groups with at least 6  matches
Group size
       458 50580 49420 49.4200%
       459 49848 50152 50.15200% <-
       460 49975 50025 50.02500% <-
       461 49316 50684 50.68400% <-
       462 49121 50879 50.87900% <-
```



## SQL

birthday.sql

```SQL

with
c as
(
select
500 nrep,
50 maxgsiz
from dual
),
reps as
(
select level rep
from dual
cross join c
connect by level <= c.nrep
),
pers as
(
select
round(sqrt(2*level)) npers
from dual
cross join c
connect by level <= c.maxgsiz*(c.maxgsiz+1)/2
),
bds as
(
select
reps.rep,
pers.npers,
floor(dbms_random.value(1,366)) bd
from
reps
cross join pers
),
mtch as
(
select
bds.npers,
case count(distinct bds.bd ) when bds.npers then 0 else 1 end match
from bds
group by
bds.rep,
bds.npers,
null
order by
bds.npers
),
nm as
(
select mtch.npers, sum (mtch.match) nmatch
from mtch
group by mtch.npers
),
sol as
(
select first_value ( nm.npers ) over ( order by abs ( nm.nmatch - c.nrep / 2 ) ) npers
from
nm
cross join c
)
select npers
from sol where rownum = 1
;

```


SQL> @ birthday.sql
Connected.

     NPERS
----------
        23


## Tcl


```tcl
proc birthdays {num {same 2}} {
    for {set i 0} {$i < $num} {incr i} {
	set b [expr {int(rand() * 365)}]
	if {[incr bs($b)] >= $same} {
	    return 1
	}
    }
    return 0
}

proc estimateBirthdayChance {num same} {
    # Gives a reasonably close estimate with minimal execution time; the idea
    # is to keep the amount that one random value may influence the result
    # fairly constant.
    set count [expr {$num * 100 / $same}]
    set x 0
    for {set i 0} {$i < $count} {incr i} {
	incr x [birthdays $num $same]
    }
    return [expr {double($x) / $count}]
}

foreach {count from to} {2 20 25 3 85 90 4 183 190 5 310 315} {
    puts "identifying level for $count people with same birthday"
    for {set i $from} {$i <= $to} {incr i} {
	set chance [estimateBirthdayChance $i $count]
	puts [format "%d people => %%%.2f chance of %d people with same birthday" \
		  $i [expr {$chance * 100}] $count]
	if {$chance >= 0.5} {
	    puts "level found: $i people"
	    break
	}
    }
}
```

{{out}}

```txt

identifying level for 2 people with same birthday
20 people => %43.40 chance of 2 people with same birthday
21 people => %44.00 chance of 2 people with same birthday
22 people => %46.91 chance of 2 people with same birthday
23 people => %53.48 chance of 2 people with same birthday
level found: 23 people
identifying level for 3 people with same birthday
85 people => %47.97 chance of 3 people with same birthday
86 people => %48.46 chance of 3 people with same birthday
87 people => %49.55 chance of 3 people with same birthday
88 people => %50.66 chance of 3 people with same birthday
level found: 88 people
identifying level for 4 people with same birthday
183 people => %48.02 chance of 4 people with same birthday
184 people => %47.67 chance of 4 people with same birthday
185 people => %48.89 chance of 4 people with same birthday
186 people => %49.98 chance of 4 people with same birthday
187 people => %50.99 chance of 4 people with same birthday
level found: 187 people
identifying level for 5 people with same birthday
310 people => %48.52 chance of 5 people with same birthday
311 people => %48.14 chance of 5 people with same birthday
312 people => %49.07 chance of 5 people with same birthday
313 people => %49.63 chance of 5 people with same birthday
314 people => %49.59 chance of 5 people with same birthday
315 people => %51.79 chance of 5 people with same birthday
level found: 315 people

```



## zkl

Pure simulation; adding a person to a population until there are the required number of collisions, then repeating that a bunch of times to get an average.

```zkl
fcn bdays(N){ // N is shared birthdays in a population
   year:=(0).pump(365,List.createLong(365).write,0); // 365 days == one year
   shared:=people:=0; do{    // add a new person to population
      bday:=(0).random(365); // with this birthday [0..364]
      shared=shared.max(year[bday]+=1); people+=1;
   }while(shared<N);
   people   // size of simulated population that contains N shared birthdays
}
fcn simulate(N,T){ avg:=0.0; do(T){ avg+=bdays(N) } avg/=T; } // N shared, T trials

foreach n in ([1..5]){
   println("Average of %d people in a populatation of %s share birthdays"
           .fmt(n,simulate(n,0d10_000)));
}
```

{{out}}

```txt

Average of 1 people in a populatation of 1 share birthdays
Average of 2 people in a populatation of 24.7199 share birthdays
Average of 3 people in a populatation of 88.6416 share birthdays
Average of 4 people in a populatation of 186.849 share birthdays
Average of 5 people in a populatation of 312.399 share birthdays

```
