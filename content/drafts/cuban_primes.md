+++
title = "Cuban primes"
description = ""
date = 2019-10-21T02:55:32Z
aliases = []
[extra]
id = 22159
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}

The name   '''cuban'''   has nothing to do with Cuba,   but has to do with the
fact that cubes   (3<sup>rd</sup> powers)   play a role in its definition.


;Some definitions of cuban primes:
::*   primes which are the difference of two consecutive cubes.
::*   primes of the form:   (n+1)<sup>3</sup> -     n<sup>3</sup>.
::*   primes of the form:       n<sup>3</sup> - (n-1)<sup>3</sup>.
::*   primes   ''p''   such that   n<sup>2</sup>(''p''+n)   is a cube for some   n>0.
::*   primes   ''p''   such that   4''p'' = 1 + 3n<sup>2</sup>.


Cuban primes were named in 1923 by Allan Joseph Champneys Cunningham.


;Task requirements:
::*   show the first   200   cuban primes   (in a multi─line horizontal format).
::*   show the   100,000<sup>th</sup>   cuban prime.
::*   show all cuban primes with commas   (if appropriate).
::*   show all output here.


Note that   '''cuban prime'''   isn't capitalized   (as it doesn't refer to the nation of Cuba).


;Also see:
:*   Wikipedia entry:     [https://en.wikipedia.org/wiki/Cuban_prime <u>cuban prime</u>].
:*   MathWorld entry:          [http://mathworld.wolfram.com/CubanPrime.html <u>cuban prime</u>].
:*   The OEIS entry:      [http://oeis.org/A002407 <u>A002407</u>].     The   100,000<sup>th</sup>   cuban prime can be verified in the   2<sup>nd</sup>   ''example''   on this OEIS web page.





## ALGOL 68


```algol68
BEGIN
    # find some cuban primes (using the definition: a prime p is a cuban prime if   #
    #                                                   p = n^3 - ( n - 1 )^3       #
    #                                                   for some n > 0)             #

    # returns a string representation of n with commas                              #
    PROC commatise = ( LONG INT n )STRING:
         BEGIN
            STRING result      := "";
            STRING unformatted  = whole( n, 0 );
            INT    ch count    := 0;
            FOR c FROM UPB unformatted BY -1 TO LWB unformatted DO
                IF   ch count <= 2 THEN ch count +:= 1
                ELSE                    ch count  := 1; "," +=: result
                FI;
                unformatted[ c ] +=: result
            OD;
            result
         END # commatise # ;

    # sieve the primes                                                              #
    INT sieve max = 2 000 000;
    [ sieve max ]BOOL sieve; FOR i TO UPB sieve DO sieve[ i ] := TRUE OD;
    sieve[ 1 ] := FALSE;
    FOR s FROM 2 TO ENTIER sqrt( sieve max ) DO
        IF sieve[ s ] THEN
            FOR p FROM s * s BY s TO sieve max DO sieve[ p ] := FALSE OD
        FI
    OD;
    # count the primes, we can ignore 2, as we know it isn't a cuban prime          #
    sieve[ 2 ] := FALSE;
    INT prime count := 0;
    FOR s TO UPB sieve DO IF sieve[ s ] THEN prime count +:= 1 FI OD;
    # construct a list of the primes                                                #
    [ 1 : prime count ]INT primes;
    INT prime pos := LWB primes;
    FOR s FROM LWB sieve TO UPB sieve DO
        IF sieve[ s ] THEN primes[ prime pos ] := s; prime pos +:= 1 FI
    OD;

    # find the cuban primes                                                         #
    INT       cuban count   := 0;
    LONG INT  final cuban   := 0;
    INT       max cuban      = 100 000; # mximum number of cubans to find           #
    INT       print limit    =     200; # show all cubans up to this one            #
    print( ( "First ", commatise( print limit ), " cuban primes: ", newline ) );
    LONG INT prev cube      := 1;
    FOR n FROM 2 WHILE
        LONG INT this cube   = ( LENG n * n ) * n;
        LONG INT p           = this cube - prev cube;
        prev cube           := this cube;
        IF ODD p THEN
            # 2 is not a cuban prime so we only test odd numbers                    #
            BOOL is prime   := TRUE;
            INT max factor   = SHORTEN ENTIER long sqrt( p );
            FOR f FROM LWB primes WHILE is prime AND primes[ f ] <= max factor DO
                is prime    := p MOD primes[ f ] /= 0
            OD;
            IF is prime THEN
                # have a cuban prime                                                #
                cuban count +:= 1;
                IF cuban count <= print limit THEN
                    # must show this cuban                                          #
                    STRING p formatted = commatise( p );
                    print( ( "          "[ UPB p formatted : ], p formatted ) );
                    IF cuban count MOD 10 = 0 THEN print( ( newline ) ) FI
                FI;
                final cuban := p
            FI
        FI;
        cuban count < max cuban
    DO SKIP OD;
    IF cuban count MOD 10 /= 0 THEN print( ( newline ) ) FI;
    print( ( "The ", commatise( max cuban ), " cuban prime is: ", commatise( final cuban ), newline ) )
END
```

{{out}}

```txt

First 200 cuban primes:
          7         19         37         61        127        271        331        397        547        631
        919      1,657      1,801      1,951      2,269      2,437      2,791      3,169      3,571      4,219
      4,447      5,167      5,419      6,211      7,057      7,351      8,269      9,241     10,267     11,719
     12,097     13,267     13,669     16,651     19,441     19,927     22,447     23,497     24,571     25,117
     26,227     27,361     33,391     35,317     42,841     45,757     47,251     49,537     50,311     55,897
     59,221     60,919     65,269     70,687     73,477     74,419     75,367     81,181     82,171     87,211
     88,237     89,269     92,401     96,661    102,121    103,231    104,347    110,017    112,327    114,661
    115,837    126,691    129,169    131,671    135,469    140,617    144,541    145,861    151,201    155,269
    163,567    169,219    170,647    176,419    180,811    189,757    200,467    202,021    213,067    231,019
    234,361    241,117    246,247    251,431    260,191    263,737    267,307    276,337    279,991    283,669
    285,517    292,969    296,731    298,621    310,087    329,677    333,667    337,681    347,821    351,919
    360,187    368,551    372,769    374,887    377,011    383,419    387,721    398,581    407,377    423,001
    436,627    452,797    459,817    476,407    478,801    493,291    522,919    527,941    553,411    574,219
    584,767    590,077    592,741    595,411    603,457    608,851    611,557    619,711    627,919    650,071
    658,477    666,937    689,761    692,641    698,419    707,131    733,591    742,519    760,537    769,627
    772,669    784,897    791,047    812,761    825,301    837,937    847,477    863,497    879,667    886,177
    895,987    909,151    915,769    925,741    929,077    932,419    939,121    952,597    972,991    976,411
    986,707    990,151    997,057  1,021,417  1,024,921  1,035,469  1,074,607  1,085,407  1,110,817  1,114,471
  1,125,469  1,155,061  1,177,507  1,181,269  1,215,397  1,253,887  1,281,187  1,285,111  1,324,681  1,328,671
  1,372,957  1,409,731  1,422,097  1,426,231  1,442,827  1,451,161  1,480,519  1,484,737  1,527,247  1,570,357
The 100,000 cuban prime is: 1,792,617,147,127

```



## C

{{trans|C++}}

```C>#include <limits.h

#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef long long llong_t;
struct PrimeArray {
    llong_t *ptr;
    size_t size;
    size_t capacity;
};

struct PrimeArray allocate() {
    struct PrimeArray primes;

    primes.size = 0;
    primes.capacity = 10;
    primes.ptr = malloc(primes.capacity * sizeof(llong_t));

    return primes;
}

void deallocate(struct PrimeArray *primes) {
    free(primes->ptr);
    primes->ptr = NULL;
}

void push_back(struct PrimeArray *primes, llong_t p) {
    if (primes->size >= primes->capacity) {
        size_t new_capacity = (3 * primes->capacity) / 2 + 1;
        llong_t *temp = realloc(primes->ptr, new_capacity * sizeof(llong_t));
        if (NULL == temp) {
            fprintf(stderr, "Failed to reallocate the prime array.");
            exit(1);
        } else {
            primes->ptr = temp;
            primes->capacity = new_capacity;
        }
    }

    primes->ptr[primes->size++] = p;
}

int main() {
    const int cutOff = 200, bigUn = 100000, chunks = 50, little = bigUn / chunks;
    struct PrimeArray primes = allocate();
    int c = 0;
    bool showEach = true;
    llong_t u = 0, v = 1, i;

    push_back(&primes, 3);
    push_back(&primes, 5);

    printf("The first %d cuban primes:\n", cutOff);
    for (i = 1; i < LLONG_MAX; ++i) {
        bool found = false;
        llong_t mx = ceil(sqrt(v += (u += 6)));
        llong_t j;

        for (j = 0; j < primes.size; ++j) {
            if (primes.ptr[j] > mx) {
                break;
            }
            if (v % primes.ptr[j] == 0) {
                found = true;
                break;
            }
        }
        if (!found) {
            c += 1;
            if (showEach) {
                llong_t z;
                for (z = primes.ptr[primes.size - 1] + 2; z <= v - 2; z += 2) {
                    bool fnd = false;

                    for (j = 0; j < primes.size; ++j) {
                        if (primes.ptr[j] > mx) {
                            break;
                        }
                        if (z % primes.ptr[j] == 0) {
                            fnd = true;
                            break;
                        }
                    }
                    if (!fnd) {
                        push_back(&primes, z);
                    }
                }
                push_back(&primes, v);
                printf("%11lld", v);
                if (c % 10 == 0) {
                    printf("\n");
                }
                if (c == cutOff) {
                    showEach = false;
                    printf("\nProgress to the %dth cuban prime: ", bigUn);
                }
            }
            if (c % little == 0) {
                printf(".");
                if (c == bigUn) {
                    break;
                }
            }
        }
    }
    printf("\nThe %dth cuban prime is %lld\n", c, v);

    deallocate(&primes);
    return 0;
}
```

{{out}}

```txt
The first 200 cuban primes:
          7         19         37         61        127        271        331        397        547        631
        919       1657       1801       1951       2269       2437       2791       3169       3571       4219
       4447       5167       5419       6211       7057       7351       8269       9241      10267      11719
      12097      13267      13669      16651      19441      19927      22447      23497      24571      25117
      26227      27361      33391      35317      42841      45757      47251      49537      50311      55897
      59221      60919      65269      70687      73477      74419      75367      81181      82171      87211
      88237      89269      92401      96661     102121     103231     104347     110017     112327     114661
     115837     126691     129169     131671     135469     140617     144541     145861     151201     155269
     163567     169219     170647     176419     180811     189757     200467     202021     213067     231019
     234361     241117     246247     251431     260191     263737     267307     276337     279991     283669
     285517     292969     296731     298621     310087     329677     333667     337681     347821     351919
     360187     368551     372769     374887     377011     383419     387721     398581     407377     423001
     436627     452797     459817     476407     478801     493291     522919     527941     553411     574219
     584767     590077     592741     595411     603457     608851     611557     619711     627919     650071
     658477     666937     689761     692641     698419     707131     733591     742519     760537     769627
     772669     784897     791047     812761     825301     837937     847477     863497     879667     886177
     895987     909151     915769     925741     929077     932419     939121     952597     972991     976411
     986707     990151     997057    1021417    1024921    1035469    1074607    1085407    1110817    1114471
    1125469    1155061    1177507    1181269    1215397    1253887    1281187    1285111    1324681    1328671
    1372957    1409731    1422097    1426231    1442827    1451161    1480519    1484737    1527247    1570357

Progress to the 100000th cuban prime: ..................................................
The 100000th cuban prime is 1792617147127
```



## C++

{{trans|C#}}

```cpp
#include <iostream>
#include <vector>
#include <chrono>
#include <climits>
#include <cmath>

using namespace std;

vector <long long> primes{ 3, 5 };

int main()
{
	cout.imbue(locale(""));
	const int cutOff = 200, bigUn = 100000,
	          chunks = 50, little = bigUn / chunks;
    const char tn[] = " cuban prime";
	cout << "The first " << cutOff << tn << "s:" << endl;
	int c = 0;
	bool showEach = true;
	long long u = 0, v = 1;
	auto st = chrono::system_clock::now();

	for (long long i = 1; i <= LLONG_MAX; i++)
	{
		bool found = false;
		long long mx = (long long)(ceil(sqrt(v += (u += 6))));
		for (long long item : primes)
		{
			if (item > mx) break;
			if (v % item == 0) { found = true; break; }
		}
		if (!found)
		{
			c += 1; if (showEach)
			{
				for (long long z = primes.back() + 2; z <= v - 2; z += 2)
				{
					bool fnd = false;
					for (long long item : primes)
					{
						if (item > mx) break;
						if (z % item == 0) { fnd = true; break; }
					}
					if (!fnd) primes.push_back(z);
				}
				primes.push_back(v); cout.width(11); cout << v;
				if (c % 10 == 0) cout << endl;
				if (c == cutOff)
				{
					showEach = false;
					cout << "\nProgress to the " << bigUn << "th" << tn << ": ";
				}
			}
			if (c % little == 0) { cout << "."; if (c == bigUn) break; }
		}
	}
	cout << "\nThe " << c << "th" << tn << " is " << v;
	chrono::duration<double> elapsed_seconds = chrono::system_clock::now() - st;
	cout << "\nComputation time was " << elapsed_seconds.count() << " seconds" << endl;
	return 0;
}
```

{{out}}

```txt
The first 200 cuban primes:
          7         19         37         61        127        271        331        397        547        631
        919      1,657      1,801      1,951      2,269      2,437      2,791      3,169      3,571      4,219
      4,447      5,167      5,419      6,211      7,057      7,351      8,269      9,241     10,267     11,719
     12,097     13,267     13,669     16,651     19,441     19,927     22,447     23,497     24,571     25,117
     26,227     27,361     33,391     35,317     42,841     45,757     47,251     49,537     50,311     55,897
     59,221     60,919     65,269     70,687     73,477     74,419     75,367     81,181     82,171     87,211
     88,237     89,269     92,401     96,661    102,121    103,231    104,347    110,017    112,327    114,661
    115,837    126,691    129,169    131,671    135,469    140,617    144,541    145,861    151,201    155,269
    163,567    169,219    170,647    176,419    180,811    189,757    200,467    202,021    213,067    231,019
    234,361    241,117    246,247    251,431    260,191    263,737    267,307    276,337    279,991    283,669
    285,517    292,969    296,731    298,621    310,087    329,677    333,667    337,681    347,821    351,919
    360,187    368,551    372,769    374,887    377,011    383,419    387,721    398,581    407,377    423,001
    436,627    452,797    459,817    476,407    478,801    493,291    522,919    527,941    553,411    574,219
    584,767    590,077    592,741    595,411    603,457    608,851    611,557    619,711    627,919    650,071
    658,477    666,937    689,761    692,641    698,419    707,131    733,591    742,519    760,537    769,627
    772,669    784,897    791,047    812,761    825,301    837,937    847,477    863,497    879,667    886,177
    895,987    909,151    915,769    925,741    929,077    932,419    939,121    952,597    972,991    976,411
    986,707    990,151    997,057  1,021,417  1,024,921  1,035,469  1,074,607  1,085,407  1,110,817  1,114,471
  1,125,469  1,155,061  1,177,507  1,181,269  1,215,397  1,253,887  1,281,187  1,285,111  1,324,681  1,328,671
  1,372,957  1,409,731  1,422,097  1,426,231  1,442,827  1,451,161  1,480,519  1,484,737  1,527,247  1,570,357

Progress to the 100,000th cuban prime: ..................................................
The 100,000th cuban prime is 1,792,617,147,127
Computation time was 35.5644 seconds
```



## C#

{{trans|Visual Basic .NET}}
(of the Snail Version)

```c#
using System;
using System.Collections.Generic;
using System.Linq;

static class Program
{
    static List<long> primes = new List<long>() { 3, 5 };

     static void Main(string[] args)
    {
        const int cutOff = 200;
        const int bigUn = 100000;
        const int chunks = 50;
        const int little = bigUn / chunks;
        const string tn = " cuban prime";
        Console.WriteLine("The first {0:n0}{1}s:", cutOff, tn);
        int c = 0;
        bool showEach = true;
        long u = 0, v = 1;
        DateTime st = DateTime.Now;
        for (long i = 1; i <= long.MaxValue; i++)
        {
            bool found = false;
            int mx = System.Convert.ToInt32(Math.Ceiling(Math.Sqrt(v += (u += 6))));
            foreach (long item in primes)
            {
                if (item > mx) break;
                if (v % item == 0) { found = true; break; }
            }
            if (!found)
            {
                c += 1; if (showEach)
                {
                    for (var z = primes.Last() + 2; z <= v - 2; z += 2)
                    {
                        bool fnd = false;
                        foreach (long item in primes)
                        {
                            if (item > mx) break;
                            if (z % item == 0) { fnd = true; break; }
                        }
                        if (!fnd) primes.Add(z);
                    }
                    primes.Add(v); Console.Write("{0,11:n0}", v);
                    if (c % 10 == 0) Console.WriteLine();
                    if (c == cutOff)
                    {
                        showEach = false;
                        Console.Write("\nProgress to the {0:n0}th{1}: ", bigUn, tn);
                    }
                }
                if (c % little == 0) { Console.Write("."); if (c == bigUn) break; }
            }
        }
        Console.WriteLine("\nThe {1:n0}th{2} is {0,17:n0}", v, c, tn);
        Console.WriteLine("Computation time was {0} seconds", (DateTime.Now - st).TotalSeconds);
        if (System.Diagnostics.Debugger.IsAttached) Console.ReadKey();
    }
}
```

{{out}}

```txt
The first 200 cuban primes:
          7         19         37         61        127        271        331        397        547        631
        919      1,657      1,801      1,951      2,269      2,437      2,791      3,169      3,571      4,219
      4,447      5,167      5,419      6,211      7,057      7,351      8,269      9,241     10,267     11,719
     12,097     13,267     13,669     16,651     19,441     19,927     22,447     23,497     24,571     25,117
     26,227     27,361     33,391     35,317     42,841     45,757     47,251     49,537     50,311     55,897
     59,221     60,919     65,269     70,687     73,477     74,419     75,367     81,181     82,171     87,211
     88,237     89,269     92,401     96,661    102,121    103,231    104,347    110,017    112,327    114,661
    115,837    126,691    129,169    131,671    135,469    140,617    144,541    145,861    151,201    155,269
    163,567    169,219    170,647    176,419    180,811    189,757    200,467    202,021    213,067    231,019
    234,361    241,117    246,247    251,431    260,191    263,737    267,307    276,337    279,991    283,669
    285,517    292,969    296,731    298,621    310,087    329,677    333,667    337,681    347,821    351,919
    360,187    368,551    372,769    374,887    377,011    383,419    387,721    398,581    407,377    423,001
    436,627    452,797    459,817    476,407    478,801    493,291    522,919    527,941    553,411    574,219
    584,767    590,077    592,741    595,411    603,457    608,851    611,557    619,711    627,919    650,071
    658,477    666,937    689,761    692,641    698,419    707,131    733,591    742,519    760,537    769,627
    772,669    784,897    791,047    812,761    825,301    837,937    847,477    863,497    879,667    886,177
    895,987    909,151    915,769    925,741    929,077    932,419    939,121    952,597    972,991    976,411
    986,707    990,151    997,057  1,021,417  1,024,921  1,035,469  1,074,607  1,085,407  1,110,817  1,114,471
  1,125,469  1,155,061  1,177,507  1,181,269  1,215,397  1,253,887  1,281,187  1,285,111  1,324,681  1,328,671
  1,372,957  1,409,731  1,422,097  1,426,231  1,442,827  1,451,161  1,480,519  1,484,737  1,527,247  1,570,357

Progress to the 100,000th cuban prime: ..................................................
The 100,000th cuban prime is 1,792,617,147,127
Computation time was 63.578673 seconds
```



## D

{{trans|C#}}

```d
import std.math;
import std.stdio;

void main() {
    long[] primes = [3, 5];

    immutable cutOff = 200;
    immutable bigUn = 100_000;
    immutable chunks = 50;
    immutable little = bigUn / chunks;
    immutable tn = " cuban prime";
    writefln("The first %s%ss:", cutOff, tn);
    int c;
    bool showEach = true;
    long u;
    long v = 1;
    for (long i = 1; i > 0; ++i) {
        bool found;
        u += 6;
        v += u;
        int mx = cast(int)ceil(sqrt(cast(real)v));
        foreach (item; primes) {
            if (item > mx) break;
            if (v % item == 0) {
                found = true;
                break;
            }
        }
        if (!found) {
            c++;
            if (showEach) {
                for (auto z = primes[$-1] + 2; z <= v - 2; z += 2) {
                    bool fnd;
                    foreach (item; primes) {
                        if (item > mx) break;
                        if (z % item == 0) {
                            fnd = true;
                            break;
                        }
                    }
                    if (!fnd) {
                        primes ~= z;
                    }
                }
                primes ~= v;
                writef("%11d", v);
                if (c % 10 == 0) writeln;
                if (c == cutOff) {
                    showEach = false;
                    writef("\nProgress to the %sth%s: ", bigUn, tn);
                }
            }
            if (c % little == 0) {
                write('.');
                if (c == bigUn) {
                    break;
                }
            }
        }
    }
    writefln("\nThe %sth%s is %17s", c, tn, v);
}
```

{{out}}

```txt
The first 200 cuban primes:
          7         19         37         61        127        271        331        397        547        631
        919       1657       1801       1951       2269       2437       2791       3169       3571       4219
       4447       5167       5419       6211       7057       7351       8269       9241      10267      11719
      12097      13267      13669      16651      19441      19927      22447      23497      24571      25117
      26227      27361      33391      35317      42841      45757      47251      49537      50311      55897
      59221      60919      65269      70687      73477      74419      75367      81181      82171      87211
      88237      89269      92401      96661     102121     103231     104347     110017     112327     114661
     115837     126691     129169     131671     135469     140617     144541     145861     151201     155269
     163567     169219     170647     176419     180811     189757     200467     202021     213067     231019
     234361     241117     246247     251431     260191     263737     267307     276337     279991     283669
     285517     292969     296731     298621     310087     329677     333667     337681     347821     351919
     360187     368551     372769     374887     377011     383419     387721     398581     407377     423001
     436627     452797     459817     476407     478801     493291     522919     527941     553411     574219
     584767     590077     592741     595411     603457     608851     611557     619711     627919     650071
     658477     666937     689761     692641     698419     707131     733591     742519     760537     769627
     772669     784897     791047     812761     825301     837937     847477     863497     879667     886177
     895987     909151     915769     925741     929077     932419     939121     952597     972991     976411
     986707     990151     997057    1021417    1024921    1035469    1074607    1085407    1110817    1114471
    1125469    1155061    1177507    1181269    1215397    1253887    1281187    1285111    1324681    1328671
    1372957    1409731    1422097    1426231    1442827    1451161    1480519    1484737    1527247    1570357

Progress to the 100000th cuban prime: ..................................................
The 100000th cuban prime is     1792617147127
```


=={{header|F_Sharp|F#}}==

### The functions

This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Generate cuban primes. Nigel Galloway: June 9th., 2019
let cubans=Seq.unfold(fun n->Some(n*n*n,n+1L)) 1L|>Seq.pairwise|>Seq.map(fun(n,g)->g-n)|>Seq.filter(isPrime64)
let cL=let g=System.Globalization.CultureInfo("en-GB") in (fun (n:int64)->n.ToString("N0",g))

```


### The Task


```fsharp

cubans|>Seq.take 200|>List.ofSeq|>List.iteri(fun n g->if n%8=7 then printfn "%12s" (cL(g)) else printf "%12s" (cL(g)))

```

{{out}}

```txt

           7          19          37          61         127         271         331         397
         547         631         919       1,657       1,801       1,951       2,269       2,437
       2,791       3,169       3,571       4,219       4,447       5,167       5,419       6,211
       7,057       7,351       8,269       9,241      10,267      11,719      12,097      13,267
      13,669      16,651      19,441      19,927      22,447      23,497      24,571      25,117
      26,227      27,361      33,391      35,317      42,841      45,757      47,251      49,537
      50,311      55,897      59,221      60,919      65,269      70,687      73,477      74,419
      75,367      81,181      82,171      87,211      88,237      89,269      92,401      96,661
     102,121     103,231     104,347     110,017     112,327     114,661     115,837     126,691
     129,169     131,671     135,469     140,617     144,541     145,861     151,201     155,269
     163,567     169,219     170,647     176,419     180,811     189,757     200,467     202,021
     213,067     231,019     234,361     241,117     246,247     251,431     260,191     263,737
     267,307     276,337     279,991     283,669     285,517     292,969     296,731     298,621
     310,087     329,677     333,667     337,681     347,821     351,919     360,187     368,551
     372,769     374,887     377,011     383,419     387,721     398,581     407,377     423,001
     436,627     452,797     459,817     476,407     478,801     493,291     522,919     527,941
     553,411     574,219     584,767     590,077     592,741     595,411     603,457     608,851
     611,557     619,711     627,919     650,071     658,477     666,937     689,761     692,641
     698,419     707,131     733,591     742,519     760,537     769,627     772,669     784,897
     791,047     812,761     825,301     837,937     847,477     863,497     879,667     886,177
     895,987     909,151     915,769     925,741     929,077     932,419     939,121     952,597
     972,991     976,411     986,707     990,151     997,057   1,021,417   1,024,921   1,035,469
   1,074,607   1,085,407   1,110,817   1,114,471   1,125,469   1,155,061   1,177,507   1,181,269
   1,215,397   1,253,887   1,281,187   1,285,111   1,324,681   1,328,671   1,372,957   1,409,731
   1,422,097   1,426,231   1,442,827   1,451,161   1,480,519   1,484,737   1,527,247   1,570,357

```


```fsharp

printfn "\n\n%s" (cL(Seq.item 99999 cubans))

```

{{out}}

```txt

1,792,617,147,127

```



## Factor

{{trans|Sidef}}

```factor
USING: formatting grouping io kernel lists lists.lazy math
math.primes sequences tools.memory.private ;
IN: rosetta-code.cuban-primes

: cuban-primes ( n -- seq )
    1 lfrom [ [ 3 * ] [ 1 + * ] bi 1 + ] <lazy-map>
    [ prime? ] <lazy-filter> ltake list>array ;

200 cuban-primes 10 <groups>
[ [ commas ] map [ "%10s" printf ] each nl ] each nl

1e5 cuban-primes last commas "100,000th cuban prime is: %s\n"
printf
```

{{out}}

```txt

         7        19        37        61       127       271       331       397       547       631
       919     1,657     1,801     1,951     2,269     2,437     2,791     3,169     3,571     4,219
     4,447     5,167     5,419     6,211     7,057     7,351     8,269     9,241    10,267    11,719
    12,097    13,267    13,669    16,651    19,441    19,927    22,447    23,497    24,571    25,117
    26,227    27,361    33,391    35,317    42,841    45,757    47,251    49,537    50,311    55,897
    59,221    60,919    65,269    70,687    73,477    74,419    75,367    81,181    82,171    87,211
    88,237    89,269    92,401    96,661   102,121   103,231   104,347   110,017   112,327   114,661
   115,837   126,691   129,169   131,671   135,469   140,617   144,541   145,861   151,201   155,269
   163,567   169,219   170,647   176,419   180,811   189,757   200,467   202,021   213,067   231,019
   234,361   241,117   246,247   251,431   260,191   263,737   267,307   276,337   279,991   283,669
   285,517   292,969   296,731   298,621   310,087   329,677   333,667   337,681   347,821   351,919
   360,187   368,551   372,769   374,887   377,011   383,419   387,721   398,581   407,377   423,001
   436,627   452,797   459,817   476,407   478,801   493,291   522,919   527,941   553,411   574,219
   584,767   590,077   592,741   595,411   603,457   608,851   611,557   619,711   627,919   650,071
   658,477   666,937   689,761   692,641   698,419   707,131   733,591   742,519   760,537   769,627
   772,669   784,897   791,047   812,761   825,301   837,937   847,477   863,497   879,667   886,177
   895,987   909,151   915,769   925,741   929,077   932,419   939,121   952,597   972,991   976,411
   986,707   990,151   997,057 1,021,417 1,024,921 1,035,469 1,074,607 1,085,407 1,110,817 1,114,471
 1,125,469 1,155,061 1,177,507 1,181,269 1,215,397 1,253,887 1,281,187 1,285,111 1,324,681 1,328,671
 1,372,957 1,409,731 1,422,097 1,426,231 1,442,827 1,451,161 1,480,519 1,484,737 1,527,247 1,570,357

100,000th cuban prime is: 1,792,617,147,127

```



## Go


```go
package main

import (
    "fmt"
    "math/big"
)

func commatize(n uint64) string {
    s := fmt.Sprintf("%d", n)
    le := len(s)
    for i := le - 3; i >= 1; i -= 3 {
        s = s[0:i] + "," + s[i:]
    }
    return s
}

func main() {
    var z big.Int
    var cube1, cube2, cube100k, diff uint64
    cubans := make([]string, 200)
    cube1 = 1
    count := 0
    for i := 1; ; i++ {
        j := i + 1
        cube2 = uint64(j * j * j)
        diff = cube2 - cube1
        z.SetUint64(diff)
        if z.ProbablyPrime(0) { // 100% accurate for z < 2 ^ 64
            if count < 200 {
                cubans[count] = commatize(diff)
            }
            count++
            if count == 100000 {
                cube100k = diff
                break
            }
        }
        cube1 = cube2
    }
    fmt.Println("The first 200 cuban primes are:-")
    for i := 0; i < 20; i++ {
        j := i * 10
        fmt.Printf("%9s\n", cubans[j : j+10]) // 10 per line say
    }
    fmt.Println("\nThe 100,000th cuban prime is", commatize(cube100k))
}
```


{{out}}

```txt

The first 200 cuban primes are:-
[        7        19        37        61       127       271       331       397       547       631]
[      919     1,657     1,801     1,951     2,269     2,437     2,791     3,169     3,571     4,219]
[    4,447     5,167     5,419     6,211     7,057     7,351     8,269     9,241    10,267    11,719]
[   12,097    13,267    13,669    16,651    19,441    19,927    22,447    23,497    24,571    25,117]
[   26,227    27,361    33,391    35,317    42,841    45,757    47,251    49,537    50,311    55,897]
[   59,221    60,919    65,269    70,687    73,477    74,419    75,367    81,181    82,171    87,211]
[   88,237    89,269    92,401    96,661   102,121   103,231   104,347   110,017   112,327   114,661]
[  115,837   126,691   129,169   131,671   135,469   140,617   144,541   145,861   151,201   155,269]
[  163,567   169,219   170,647   176,419   180,811   189,757   200,467   202,021   213,067   231,019]
[  234,361   241,117   246,247   251,431   260,191   263,737   267,307   276,337   279,991   283,669]
[  285,517   292,969   296,731   298,621   310,087   329,677   333,667   337,681   347,821   351,919]
[  360,187   368,551   372,769   374,887   377,011   383,419   387,721   398,581   407,377   423,001]
[  436,627   452,797   459,817   476,407   478,801   493,291   522,919   527,941   553,411   574,219]
[  584,767   590,077   592,741   595,411   603,457   608,851   611,557   619,711   627,919   650,071]
[  658,477   666,937   689,761   692,641   698,419   707,131   733,591   742,519   760,537   769,627]
[  772,669   784,897   791,047   812,761   825,301   837,937   847,477   863,497   879,667   886,177]
[  895,987   909,151   915,769   925,741   929,077   932,419   939,121   952,597   972,991   976,411]
[  986,707   990,151   997,057 1,021,417 1,024,921 1,035,469 1,074,607 1,085,407 1,110,817 1,114,471]
[1,125,469 1,155,061 1,177,507 1,181,269 1,215,397 1,253,887 1,281,187 1,285,111 1,324,681 1,328,671]
[1,372,957 1,409,731 1,422,097 1,426,231 1,442,827 1,451,161 1,480,519 1,484,737 1,527,247 1,570,357]

The 100,000th cuban prime is 1,792,617,147,127

```




## J

I've used assertions to demonstrate and to prove the defined verbs

```j


isPrime =: 1&p:
assert 1 0 -: isPrime 3 9


NB. difference, but first cube, of incremented y with y
dcc =: -&(^&3)~ >:
assert ((8 9 13^3)-7 8 12^3) -: dcc  7 8 12

Filter =: (#~`)(`:6)
assert 2 3 5 7 11 13 -: isPrime Filter i. 16

cubanPrime =: [: isPrime Filter dcc
assert 7 19 37 61 127 271 331 397 547 631 919 -: cubanPrime i. 20

NB. comatose copies with comma fill
comatose =: (#!.','~ (1 1 1j1 1 1 1j1 1 1 1j1 1 1 1j1 1 1 1j1 1 1 1j1 1 1 1 {.~ -@:#))@:":&>
assert (comatose 1000 1238 12  989832) -: [;._2 ] 0 :0
1,000
1,238
12
989,832
)

   CP =: cubanPrime i. 800000x
   # CP  NB. tally, I've stored more than 100000 cuban primes
103278
   NB. granted, I used wolframalpha Solve[(n+1)^3-n^3==1792617147127,n]

   9!:17]2 2 NB. specify bottom right position in box

   comatose&.> 10 20 $ CP
┌─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┐
│        7│       19│       37│       61│      127│      271│      331│      397│      547│      631│      919│    1,657│    1,801│    1,951│    2,269│    2,437│    2,791│    3,169│    3,571│    4,219│
├─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│    4,447│    5,167│    5,419│    6,211│    7,057│    7,351│    8,269│    9,241│   10,267│   11,719│   12,097│   13,267│   13,669│   16,651│   19,441│   19,927│   22,447│   23,497│   24,571│   25,117│
├─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│   26,227│   27,361│   33,391│   35,317│   42,841│   45,757│   47,251│   49,537│   50,311│   55,897│   59,221│   60,919│   65,269│   70,687│   73,477│   74,419│   75,367│   81,181│   82,171│   87,211│
├─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│   88,237│   89,269│   92,401│   96,661│  102,121│  103,231│  104,347│  110,017│  112,327│  114,661│  115,837│  126,691│  129,169│  131,671│  135,469│  140,617│  144,541│  145,861│  151,201│  155,269│
├─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│  163,567│  169,219│  170,647│  176,419│  180,811│  189,757│  200,467│  202,021│  213,067│  231,019│  234,361│  241,117│  246,247│  251,431│  260,191│  263,737│  267,307│  276,337│  279,991│  283,669│
├─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│  285,517│  292,969│  296,731│  298,621│  310,087│  329,677│  333,667│  337,681│  347,821│  351,919│  360,187│  368,551│  372,769│  374,887│  377,011│  383,419│  387,721│  398,581│  407,377│  423,001│
├─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│  436,627│  452,797│  459,817│  476,407│  478,801│  493,291│  522,919│  527,941│  553,411│  574,219│  584,767│  590,077│  592,741│  595,411│  603,457│  608,851│  611,557│  619,711│  627,919│  650,071│
├─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│  658,477│  666,937│  689,761│  692,641│  698,419│  707,131│  733,591│  742,519│  760,537│  769,627│  772,669│  784,897│  791,047│  812,761│  825,301│  837,937│  847,477│  863,497│  879,667│  886,177│
├─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│  895,987│  909,151│  915,769│  925,741│  929,077│  932,419│  939,121│  952,597│  972,991│  976,411│  986,707│  990,151│  997,057│1,021,417│1,024,921│1,035,469│1,074,607│1,085,407│1,110,817│1,114,471│
├─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│1,125,469│1,155,061│1,177,507│1,181,269│1,215,397│1,253,887│1,281,187│1,285,111│1,324,681│1,328,671│1,372,957│1,409,731│1,422,097│1,426,231│1,442,827│1,451,161│1,480,519│1,484,737│1,527,247│1,570,357│
└─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┘

   NB. the one hundred thousandth cuban prime
   comatose (<: 100000) { CP
1,792,617,147,127


   cubanPrime f.              NB. cubanPrime with fixed adverbs
[: (#~ 1&p:) (-&(^&3)~ >:)


```



## Julia


{{trans|Go}}
{{works with|Julia|1.2}}

```julia
using Primes

function cubanprimes(N)
    cubans = zeros(Int, N)
    cube100k, cube1, count = 0, 1, 1
    for i in Iterators.countfrom(1)
        j = BigInt(i + 1)
        cube2 = j^3
        diff = cube2 - cube1
        if isprime(diff)
            count ≤ N && (cubans[count] = diff)
            if count == 100000
                cube100k = diff
                break
            end
            count += 1
        end
        cube1 = cube2
    end
    println("The first $N cuban primes are: ")
    foreach(x -> print(lpad(cubans[x] == 0 ? "" : cubans[x], 10), x % 8 == 0 ? "\n" : ""), 1:N)
    println("\nThe 100,000th cuban prime is ", cube100k)
end

cubanprimes(200)

```
{{out}}

```txt

The first 200 cuban primes are:
         7        19        37        61       127       271       331       397
       547       631       919      1657      1801      1951      2269      2437
      2791      3169      3571      4219      4447      5167      5419      6211
      7057      7351      8269      9241     10267     11719     12097     13267
     13669     16651     19441     19927     22447     23497     24571     25117
     26227     27361     33391     35317     42841     45757     47251     49537
     50311     55897     59221     60919     65269     70687     73477     74419
     75367     81181     82171     87211     88237     89269     92401     96661
    102121    103231    104347    110017    112327    114661    115837    126691
    129169    131671    135469    140617    144541    145861    151201    155269
    163567    169219    170647    176419    180811    189757    200467    202021
    213067    231019    234361    241117    246247    251431    260191    263737
    267307    276337    279991    283669    285517    292969    296731    298621
    310087    329677    333667    337681    347821    351919    360187    368551
    372769    374887    377011    383419    387721    398581    407377    423001
    436627    452797    459817    476407    478801    493291    522919    527941
    553411    574219    584767    590077    592741    595411    603457    608851
    611557    619711    627919    650071    658477    666937    689761    692641
    698419    707131    733591    742519    760537    769627    772669    784897
    791047    812761    825301    837937    847477    863497    879667    886177
    895987    909151    915769    925741    929077    932419    939121    952597
    972991    976411    986707    990151    997057   1021417   1024921   1035469
   1074607   1085407   1110817   1114471   1125469   1155061   1177507   1181269
   1215397   1253887   1281187   1285111   1324681   1328671   1372957   1409731
   1422097   1426231   1442827   1451161   1480519   1484737   1527247   1570357

The 100,000th cuban prime is 1,792,617,147,127

```



## Kotlin

{{trans|D}}

```scala
import kotlin.math.ceil
import kotlin.math.sqrt

fun main() {
    val primes = mutableListOf(3L, 5L)
    val cutOff = 200
    val bigUn = 100_000
    val chunks = 50
    val little = bigUn / chunks

    println("The first $cutOff cuban primes:")
    var showEach = true
    var c = 0
    var u = 0L
    var v = 1L
    var i = 1L
    while (i > 0) {
        var found = false
        u += 6
        v += u
        val mx = ceil(sqrt(v.toDouble())).toInt()
        for (item in primes) {
            if (item > mx) break
            if (v % item == 0L) {
                found = true
                break
            }
        }
        if (!found) {
            c++
            if (showEach) {
                var z = primes.last() + 2
                while (z <= v - 2) {
                    var fnd = false
                    for (item in primes) {
                        if (item > mx) break
                        if (z % item == 0L) {
                            fnd = true
                            break
                        }
                    }
                    if (!fnd) {
                        primes.add(z)
                    }
                    z += 2
                }
                primes.add(v)
                print("%11d".format(v))
                if (c % 10 == 0) println()
                if (c == cutOff) {
                    showEach = false
                    print("\nProgress to the ${bigUn}th cuban prime: ")
                }
            }
            if (c % little == 0) {
                print(".")
                if (c == bigUn) break
            }
        }
        i++
    }
    println("\nThe %dth cuban prime is %17d".format(c, v))
}
```

{{out}}

```txt
The first 200 cuban primes:
          7         19         37         61        127        271        331        397        547        631
        919       1657       1801       1951       2269       2437       2791       3169       3571       4219
       4447       5167       5419       6211       7057       7351       8269       9241      10267      11719
      12097      13267      13669      16651      19441      19927      22447      23497      24571      25117
      26227      27361      33391      35317      42841      45757      47251      49537      50311      55897
      59221      60919      65269      70687      73477      74419      75367      81181      82171      87211
      88237      89269      92401      96661     102121     103231     104347     110017     112327     114661
     115837     126691     129169     131671     135469     140617     144541     145861     151201     155269
     163567     169219     170647     176419     180811     189757     200467     202021     213067     231019
     234361     241117     246247     251431     260191     263737     267307     276337     279991     283669
     285517     292969     296731     298621     310087     329677     333667     337681     347821     351919
     360187     368551     372769     374887     377011     383419     387721     398581     407377     423001
     436627     452797     459817     476407     478801     493291     522919     527941     553411     574219
     584767     590077     592741     595411     603457     608851     611557     619711     627919     650071
     658477     666937     689761     692641     698419     707131     733591     742519     760537     769627
     772669     784897     791047     812761     825301     837937     847477     863497     879667     886177
     895987     909151     915769     925741     929077     932419     939121     952597     972991     976411
     986707     990151     997057    1021417    1024921    1035469    1074607    1085407    1110817    1114471
    1125469    1155061    1177507    1181269    1215397    1253887    1281187    1285111    1324681    1328671
    1372957    1409731    1422097    1426231    1442827    1451161    1480519    1484737    1527247    1570357

Progress to the 100000th cuban prime: ..................................................
The 100000th cuban prime is     1792617147127
```



## Maple


{{incorrect|Maple|

 The output is still incorrect.

 It appears that the Maple solution isn't using a correct formula for computing cuban primes.

 See output from other entries for the first 200 cuban primes.

 The first three cuban primes are:       7     19     37     ···


 It also appears that most of the program is missing.

}}


```maple
CubanPrimes := proc(n) local i, cp;
               cp := Array([]);
               for i by 2 while numelems(cp) < n do
                   if isprime(3/4*i^2 + 1/4) then
                      ArrayTools:-Append(cp, 3/4*i^2 + 1/4);
                   end if;
               end do;
               return cp;
               end proc;
```

{{out}}

```txt
 The first 200 cuban primes are
[1407, 3819, 7437, 12261, 25527, 54471, 66531, 79797, 109947, 126831, 184719, 333057, 362001, 392151, 456069, 489837, 560991, 636969, 717771, 848019, 893847, 1038567, 1089219, 1248411, 1418457, 1477551, 1662069, 1857441, 2063667, 2355519, 2431497, 2666667, 2747469, 3346851, 3907641, 4005327, 4511847, 4722897, 4938771, 5048517, 5271627, 5499561, 6711591, 7098717, 8611041, 9197157, 9497451, 9956937, 10112511, 11235297, 11903421, 12244719, 13119069, 14208087, 14768877, 14958219, 15148767, 16317381, 16516371, 17529411, 17735637, 17943069, 18572601, 19428861, 20526321, 20749431, 20973747, 22113417, 22577727, 23046861, 23283237, 25464891, 25962969, 26465871, 27229269, 28264017, 29052741, 29318061, 30391401, 31209069, 32876967, 34013019, 34300047, 35460219, 36343011, 38141157, 40293867, 40606221, 42826467, 46434819, 47106561, 48464517, 49495647, 50537631, 52298391, 53011137, 53728707, 55543737, 56278191, 57017469, 57388917, 58886769, 59642931, 60022821, 62327487, 66265077, 67067067, 67873881, 69912021, 70735719, 72397587, 74078751, 74926569, 75352287, 75779211, 77067219, 77931921, 80114781, 81882777, 85023201, 87762027, 91012197, 92423217, 95757807, 96239001, 99151491, 105106719, 106116141, 111235611, 115418019, 117538167, 118605477, 119140941, 119677611, 121294857, 122379051, 122922957, 124561911, 126211719, 130664271, 132353877, 134054337, 138641961, 139220841, 140382219, 142133331, 147451791, 149246319, 152867937, 154695027, 155306469, 157764297, 159000447, 163364961, 165885501, 168425337, 170342877, 173562897, 176813067, 178121577, 180093387, 182739351, 184069569, 186073941, 186744477, 187416219, 188763321, 191471997, 195571191, 196258611, 198328107, 199020351, 200408457, 205304817, 206009121, 208129269, 215996007, 218166807, 223274217, 224008671, 226219269, 232167261, 236678907, 237435069, 244294797, 252031287, 257518587, 258307311, 266260881, 267062871, 275964357, 283355931, 285841497, 286672431, 290008227, 291683361, 297584319, 298432137, 306976647, 315641757]
The 200th cuban prime is: 1,570,357
The 100000th cuban prime is: 1792617147127
```



## Nim

{{trans|C#}}

```nim
import strformat
import math

const cutOff = 200
const bigUn = 100000
const chunks = 50
const little = bigUn div chunks

var primes: seq[int] = @[3, 5]
echo fmt"The first {cutOff} cuban primes"
var c, u = 0
var showEach: bool = true
var v = 1
for i in 1..high(BiggestInt):
  var found: bool
  inc u, 6
  inc v, u
  var mx = cast[int](ceil(sqrt(cast[float](v))))
  for item in primes:
    if item > mx:
      break
    if v mod item == 0:
      found = true
      break
  if not found:
    inc c
    if showEach:
      for z in countup(primes[^1] + 2, v - 2, step=2):
        var fnd: bool = false
        for item in primes:
          if item > mx:
            break
          if z mod item == 0:
            fnd = true
            break
        if not fnd:
          primes.add(z)
      primes.add(v)
      write(stdout, fmt"{v:11}")
      if c mod 10 == 0:
        write(stdout, "\n")
      if c == cutOff:
        showEach = false
        write(stdout, fmt"Progress to the {bigUn}th cuban prime: ")
    if c mod little == 0:
      write(stdout, ".")
      if c == bigUn:
        break
write(stdout, "\n")
echo fmt"The {c}th cuban prime is {v}"
```

{{out}}

```txt
The first 200 cuban primes
          7         19         37         61        127        271        331        397        547        631
        919       1657       1801       1951       2269       2437       2791       3169       3571       4219
       4447       5167       5419       6211       7057       7351       8269       9241      10267      11719
      12097      13267      13669      16651      19441      19927      22447      23497      24571      25117
      26227      27361      33391      35317      42841      45757      47251      49537      50311      55897
      59221      60919      65269      70687      73477      74419      75367      81181      82171      87211
      88237      89269      92401      96661     102121     103231     104347     110017     112327     114661
     115837     126691     129169     131671     135469     140617     144541     145861     151201     155269
     163567     169219     170647     176419     180811     189757     200467     202021     213067     231019
     234361     241117     246247     251431     260191     263737     267307     276337     279991     283669
     285517     292969     296731     298621     310087     329677     333667     337681     347821     351919
     360187     368551     372769     374887     377011     383419     387721     398581     407377     423001
     436627     452797     459817     476407     478801     493291     522919     527941     553411     574219
     584767     590077     592741     595411     603457     608851     611557     619711     627919     650071
     658477     666937     689761     692641     698419     707131     733591     742519     760537     769627
     772669     784897     791047     812761     825301     837937     847477     863497     879667     886177
     895987     909151     915769     925741     929077     932419     939121     952597     972991     976411
     986707     990151     997057    1021417    1024921    1035469    1074607    1085407    1110817    1114471
    1125469    1155061    1177507    1181269    1215397    1253887    1281187    1285111    1324681    1328671
    1372957    1409731    1422097    1426231    1442827    1451161    1480519    1484737    1527247    1570357
Progress to the 100000th cuban prime: ..................................................
The 100000th cuban prime is 1792617147127
```



## Pascal

{{libheader|primTrial}}{{works with|Free Pascal}}
uses trial division to check primility.Slow in such number ranges.<BR>OutNthCubPrime(10000) takes only 0,950 s.<BR>
100: 283,669; 1000: 65,524,807; 10000: 11,712,188,419; 100000: 1,792,617,147,127


```pascal
program CubanPrimes;
{$IFDEF FPC}
  {$MODE DELPHI}
  {$OPTIMIZATION ON,Regvar,PEEPHOLE,CSE,ASMCSE}
  {$CODEALIGN proc=32}
{$ENDIF}
uses
  primTrial;
const
  COLUMNCOUNT = 10*10;

procedure FormOut(Cuban:Uint64;ColSize:Uint32);
var
  s : String;
  pI,pJ :pChar;
  i,j : NativeInt;
Begin
  str(Cuban,s);
  i := length(s);
  If i>3 then
  Begin
    //extend s by the count of comma to be inserted
    j := i+ (i-1) div 3;
    setlength(s,j);
    pI := @s[i];
    pJ := @s[j];
    while i > 3 do
    Begin
       // copy 3 digits
       pJ^ := pI^;dec(pJ);dec(pI);
       pJ^ := pI^;dec(pJ);dec(pI);
       pJ^ := pI^;dec(pJ);dec(pI);
       // insert comma
       pJ^ := ',';dec(pJ);
       dec(i,3);
    end;
    //the digits in front are in the right place
  end;
  write(s:ColSize);
end;

procedure OutFirstCntCubPrimes(Cnt : Int32;ColCnt : Int32);
var
  cbDelta1,
  cbDelta2  : Uint64;
  ClCnt,ColSize : NativeInt;
Begin
  If Cnt <= 0 then
    EXIT;
  IF ColCnt <= 0 then
    ColCnt := 1;
  ColSize := COLUMNCOUNT DIV ColCnt;
  dec(ColCnt);

  ClCnt := ColCnt;
  cbDelta1 := 0;
  cbDelta2 := 1;

  repeat
    if isPrime(cbDelta2) then
    Begin
      FormOut(cbDelta2,ColSize);
      dec(Cnt);

      dec(ClCnt);
      If ClCnt < 0 then
      Begin
        Writeln;
        ClCnt := ColCnt;
      end;
    end;
    inc(cbDelta1,6);// 0,6,12,18...
    inc(cbDelta2,cbDelta1);//1,7,19,35...
  until Cnt<= 0;

  writeln;
end;

procedure OutNthCubPrime(n : Int32);
var
  cbDelta1,
  cbDelta2  : Uint64;
Begin
  If n <= 0 then
    EXIT;
  cbDelta1 := 0;
  cbDelta2 := 1;

  repeat
    inc(cbDelta1,6);
    inc(cbDelta2,cbDelta1);
    if isPrime(cbDelta2) then
      dec(n);
  until n<=0;

  FormOut(cbDelta2,20);
  writeln;
end;

Begin
  OutFirstCntCubPrimes(200,10);
  OutNthCubPrime(100000);
end.
```

{{out}}

```txt
         7        19        37        61       127       271       331       397       547       631
       919     1,657     1,801     1,951     2,269     2,437     2,791     3,169     3,571     4,219
     4,447     5,167     5,419     6,211     7,057     7,351     8,269     9,241    10,267    11,719
    12,097    13,267    13,669    16,651    19,441    19,927    22,447    23,497    24,571    25,117
    26,227    27,361    33,391    35,317    42,841    45,757    47,251    49,537    50,311    55,897
    59,221    60,919    65,269    70,687    73,477    74,419    75,367    81,181    82,171    87,211
    88,237    89,269    92,401    96,661   102,121   103,231   104,347   110,017   112,327   114,661
   115,837   126,691   129,169   131,671   135,469   140,617   144,541   145,861   151,201   155,269
   163,567   169,219   170,647   176,419   180,811   189,757   200,467   202,021   213,067   231,019
   234,361   241,117   246,247   251,431   260,191   263,737   267,307   276,337   279,991   283,669
   285,517   292,969   296,731   298,621   310,087   329,677   333,667   337,681   347,821   351,919
   360,187   368,551   372,769   374,887   377,011   383,419   387,721   398,581   407,377   423,001
   436,627   452,797   459,817   476,407   478,801   493,291   522,919   527,941   553,411   574,219
   584,767   590,077   592,741   595,411   603,457   608,851   611,557   619,711   627,919   650,071
   658,477   666,937   689,761   692,641   698,419   707,131   733,591   742,519   760,537   769,627
   772,669   784,897   791,047   812,761   825,301   837,937   847,477   863,497   879,667   886,177
   895,987   909,151   915,769   925,741   929,077   932,419   939,121   952,597   972,991   976,411
   986,707   990,151   997,057 1,021,417 1,024,921 1,035,469 1,074,607 1,085,407 1,110,817 1,114,471
 1,125,469 1,155,061 1,177,507 1,181,269 1,215,397 1,253,887 1,281,187 1,285,111 1,324,681 1,328,671
 1,372,957 1,409,731 1,422,097 1,426,231 1,442,827 1,451,161 1,480,519 1,484,737 1,527,247 1,570,357

   1,792,617,147,127  //user    2m1.950s
```



## Perl

{{libheader|ntheory}}

```perl
use feature 'say';
use ntheory 'is_prime';

sub cuban_primes {
    my ($n) = @_;

    my @primes;
    for (my $k = 1 ; ; ++$k) {
        my $p = 3 * $k * ($k + 1) + 1;
        if (is_prime($p)) {
            push @primes, $p;
            last if @primes >= $n;
        }
    }

    return @primes;
}

sub commify {
    scalar reverse join ',', unpack '(A3)*', reverse shift;
}

my @c = cuban_primes(200);

while (@c) {
    say join ' ', map { sprintf "%9s", commify $_ } splice(@c, 0, 10);
}

say '';
for my $n (1 .. 6) {
    say "10^$n-th cuban prime is: ", commify((cuban_primes(10**$n))[-1]);
}
```

{{out}}

```txt

        7        19        37        61       127       271       331       397       547       631
      919     1,657     1,801     1,951     2,269     2,437     2,791     3,169     3,571     4,219
    4,447     5,167     5,419     6,211     7,057     7,351     8,269     9,241    10,267    11,719
   12,097    13,267    13,669    16,651    19,441    19,927    22,447    23,497    24,571    25,117
   26,227    27,361    33,391    35,317    42,841    45,757    47,251    49,537    50,311    55,897
   59,221    60,919    65,269    70,687    73,477    74,419    75,367    81,181    82,171    87,211
   88,237    89,269    92,401    96,661   102,121   103,231   104,347   110,017   112,327   114,661
  115,837   126,691   129,169   131,671   135,469   140,617   144,541   145,861   151,201   155,269
  163,567   169,219   170,647   176,419   180,811   189,757   200,467   202,021   213,067   231,019
  234,361   241,117   246,247   251,431   260,191   263,737   267,307   276,337   279,991   283,669
  285,517   292,969   296,731   298,621   310,087   329,677   333,667   337,681   347,821   351,919
  360,187   368,551   372,769   374,887   377,011   383,419   387,721   398,581   407,377   423,001
  436,627   452,797   459,817   476,407   478,801   493,291   522,919   527,941   553,411   574,219
  584,767   590,077   592,741   595,411   603,457   608,851   611,557   619,711   627,919   650,071
  658,477   666,937   689,761   692,641   698,419   707,131   733,591   742,519   760,537   769,627
  772,669   784,897   791,047   812,761   825,301   837,937   847,477   863,497   879,667   886,177
  895,987   909,151   915,769   925,741   929,077   932,419   939,121   952,597   972,991   976,411
  986,707   990,151   997,057 1,021,417 1,024,921 1,035,469 1,074,607 1,085,407 1,110,817 1,114,471
1,125,469 1,155,061 1,177,507 1,181,269 1,215,397 1,253,887 1,281,187 1,285,111 1,324,681 1,328,671
1,372,957 1,409,731 1,422,097 1,426,231 1,442,827 1,451,161 1,480,519 1,484,737 1,527,247 1,570,357

10^1-th cuban prime is: 631
10^2-th cuban prime is: 283,669
10^3-th cuban prime is: 65,524,807
10^4-th cuban prime is: 11,712,188,419
10^5-th cuban prime is: 1,792,617,147,127
10^6-th cuban prime is: 255,155,578,239,277

```



## Perl 6

{{works with|Rakudo|2018.12}}

===The task (k == 1)===
Not the most efficient, but concise, and good enough for this task. Use the ntheory library for prime testing; gets it down to around 20 seconds.

```perl6
use Lingua::EN::Numbers;
use ntheory:from<Perl5> <:all>;

my @cubans = lazy (1..Inf).map({ ($_+1)³ - .³ }).grep: *.&is_prime;

put @cubans[^200]».&comma».fmt("%9s").rotor(10).join: "\n";

put '';

put @cubans[99_999].&comma; # zero indexed
```


{{out}}

```txt
        7        19        37        61       127       271       331       397       547       631
      919     1,657     1,801     1,951     2,269     2,437     2,791     3,169     3,571     4,219
    4,447     5,167     5,419     6,211     7,057     7,351     8,269     9,241    10,267    11,719
   12,097    13,267    13,669    16,651    19,441    19,927    22,447    23,497    24,571    25,117
   26,227    27,361    33,391    35,317    42,841    45,757    47,251    49,537    50,311    55,897
   59,221    60,919    65,269    70,687    73,477    74,419    75,367    81,181    82,171    87,211
   88,237    89,269    92,401    96,661   102,121   103,231   104,347   110,017   112,327   114,661
  115,837   126,691   129,169   131,671   135,469   140,617   144,541   145,861   151,201   155,269
  163,567   169,219   170,647   176,419   180,811   189,757   200,467   202,021   213,067   231,019
  234,361   241,117   246,247   251,431   260,191   263,737   267,307   276,337   279,991   283,669
  285,517   292,969   296,731   298,621   310,087   329,677   333,667   337,681   347,821   351,919
  360,187   368,551   372,769   374,887   377,011   383,419   387,721   398,581   407,377   423,001
  436,627   452,797   459,817   476,407   478,801   493,291   522,919   527,941   553,411   574,219
  584,767   590,077   592,741   595,411   603,457   608,851   611,557   619,711   627,919   650,071
  658,477   666,937   689,761   692,641   698,419   707,131   733,591   742,519   760,537   769,627
  772,669   784,897   791,047   812,761   825,301   837,937   847,477   863,497   879,667   886,177
  895,987   909,151   915,769   925,741   929,077   932,419   939,121   952,597   972,991   976,411
  986,707   990,151   997,057 1,021,417 1,024,921 1,035,469 1,074,607 1,085,407 1,110,817 1,114,471
1,125,469 1,155,061 1,177,507 1,181,269 1,215,397 1,253,887 1,281,187 1,285,111 1,324,681 1,328,671
1,372,957 1,409,731 1,422,097 1,426,231 1,442,827 1,451,161 1,480,519 1,484,737 1,527,247 1,570,357

1,792,617,147,127
```



### k == 2 through 10

After reading up a bit, the general equation for cuban primes is prime numbers of the form {{math|((<var>x</var>+<var>k</var>)<sup>3</sup> - <var>x</var><sup>3</sup>)/<var>k</var> }} where k mod 3 is not equal 0.

The cubans where k == 1 (the focus of this task) is one of many possible groups. In general, it seems like the cubans where k == 1 and k == 2 are the two primary cases, but it is possible to have cubans with a k of any integer that is not a multiple of 3.

Here are the first 20 for each valid k up to 10:

```perl6
sub comma { $^i.flip.comb(3).join(',').flip }

for 2..10 -> \k {
    next if k %% 3;
    my @cubans = lazy (1..Inf).map({ (($_+k)³ - .³)/k }).grep: *.is-prime;
    put "First 20 cuban primes where k = {k}:";
    put @cubans[^20]».&comma».fmt("%7s").rotor(10).join: "\n";
    put '';
}

```

{{out}}

```txt
First 20 cuban primes where k = 2:
     13     109     193     433     769   1,201   1,453   2,029   3,469   3,889
  4,801  10,093  12,289  13,873  18,253  20,173  21,169  22,189  28,813  37,633

First 20 cuban primes where k = 4:
     31      79     151     367   1,087   1,327   1,879   2,887   3,271   4,111
  4,567   6,079   7,207   8,431  15,991  16,879  17,791  19,687  23,767  24,847

First 20 cuban primes where k = 5:
     43      67      97     223     277     337     727     823   1,033   1,663
  2,113   2,617   2,797   3,373   4,003   5,683   6,217   7,963  10,273  10,627

First 20 cuban primes where k = 7:
     73     103     139     181     229     283     409     643     733     829
  1,039   1,153   1,399   1,531   1,669   2,281   2,803   3,181   3,583   3,793

First 20 cuban primes where k = 8:
    163     379     523     691     883   2,203   2,539   3,691   5,059   5,563
  6,091   7,219   8,443   9,091  10,459  11,923  15,139  19,699  24,859  27,091

First 20 cuban primes where k = 10:
    457     613     997   1,753   2,053   2,377   4,357   6,373   9,433  13,093
 16,453  21,193  27,673  28,837  31,237  37,657  46,153  47,653  49,177  62,233
```


===k == 2^128===
Note that Perl 6 has native support for arbitrarily large integers and does not need to generate primes to test for primality. Using k of 2^128; finishes in ''well'' under a second.

```perl6
sub comma { $^i.flip.comb(3).join(',').flip }

my \k = 2**128;
put "First 10 cuban primes where k = {k}:";
.&comma.put for (lazy (0..Inf).map({ (($_+k)³ - .³)/k }).grep: *.is-prime)[^10];
```


```txt
First 10 cuban primes where k = 340282366920938463463374607431768211456:
115,792,089,237,316,195,423,570,985,008,687,908,160,544,961,995,247,996,546,884,854,518,799,824,856,507
115,792,089,237,316,195,423,570,985,008,687,908,174,836,821,405,927,412,012,346,588,030,934,089,763,531
115,792,089,237,316,195,423,570,985,008,687,908,219,754,093,839,491,289,189,512,036,211,927,493,764,691
115,792,089,237,316,195,423,570,985,008,687,908,383,089,629,961,541,751,651,931,847,779,176,235,685,011
115,792,089,237,316,195,423,570,985,008,687,908,491,299,422,642,400,183,033,284,972,942,478,527,291,811
115,792,089,237,316,195,423,570,985,008,687,908,771,011,528,251,411,600,000,178,900,251,391,998,361,371
115,792,089,237,316,195,423,570,985,008,687,908,875,137,932,529,218,769,819,971,530,125,513,071,648,307
115,792,089,237,316,195,423,570,985,008,687,908,956,805,700,590,244,001,051,181,435,909,137,442,897,427
115,792,089,237,316,195,423,570,985,008,687,909,028,264,997,643,641,078,378,490,103,469,808,767,771,907
115,792,089,237,316,195,423,570,985,008,687,909,158,933,426,541,281,448,348,425,952,723,607,761,904,131
```



## Phix

{{libheader|mpfr}}

```Phix
include mpfr.e
integer np = 0,
        i = 2
mpz p3 = mpz_init(1*1*1),
    i3 = mpz_init(),
    p = mpz_init(),
    pn = mpz_init()
atom randstate = gmp_randinit_mt()

printf(1,"The first 200 cuban primes are:\n")
sequence first200 = {}
atom t0 = time()
while np<100000 do
    mpz_ui_pow_ui(i3,i,3)
    mpz_sub(p,i3,p3)
    if mpz_probable_prime_p(p,randstate) then
        mpz_set(pn,p)
        np += 1
        if np<=200 then
            first200 = append(first200,sprintf("%,9d",mpz_get_integer(pn)))
            if mod(np,10)=0 then
                printf(1,"%s\n",join(first200[-10..-1]))
            end if
        end if
    end if
    mpz_set(p3,i3)
    i += 1
end while
printf(1,"\nThe %,dth cuban prime is %s\n",{np,mpz_get_str(pn,comma_fill:=true)})
randstate = gmp_randclear(randstate)
{p3,i3,p} = mpz_free({p3,i3,p})
?elapsed(time()-t0)
```

{{out}}

```txt

The first 200 cuban primes are:
        7        19        37        61       127       271       331       397       547       631
      919     1,657     1,801     1,951     2,269     2,437     2,791     3,169     3,571     4,219
    4,447     5,167     5,419     6,211     7,057     7,351     8,269     9,241    10,267    11,719
   12,097    13,267    13,669    16,651    19,441    19,927    22,447    23,497    24,571    25,117
   26,227    27,361    33,391    35,317    42,841    45,757    47,251    49,537    50,311    55,897
   59,221    60,919    65,269    70,687    73,477    74,419    75,367    81,181    82,171    87,211
   88,237    89,269    92,401    96,661   102,121   103,231   104,347   110,017   112,327   114,661
  115,837   126,691   129,169   131,671   135,469   140,617   144,541   145,861   151,201   155,269
  163,567   169,219   170,647   176,419   180,811   189,757   200,467   202,021   213,067   231,019
  234,361   241,117   246,247   251,431   260,191   263,737   267,307   276,337   279,991   283,669
  285,517   292,969   296,731   298,621   310,087   329,677   333,667   337,681   347,821   351,919
  360,187   368,551   372,769   374,887   377,011   383,419   387,721   398,581   407,377   423,001
  436,627   452,797   459,817   476,407   478,801   493,291   522,919   527,941   553,411   574,219
  584,767   590,077   592,741   595,411   603,457   608,851   611,557   619,711   627,919   650,071
  658,477   666,937   689,761   692,641   698,419   707,131   733,591   742,519   760,537   769,627
  772,669   784,897   791,047   812,761   825,301   837,937   847,477   863,497   879,667   886,177
  895,987   909,151   915,769   925,741   929,077   932,419   939,121   952,597   972,991   976,411
  986,707   990,151   997,057 1,021,417 1,024,921 1,035,469 1,074,607 1,085,407 1,110,817 1,114,471
1,125,469 1,155,061 1,177,507 1,181,269 1,215,397 1,253,887 1,281,187 1,285,111 1,324,681 1,328,671
1,372,957 1,409,731 1,422,097 1,426,231 1,442,827 1,451,161 1,480,519 1,484,737 1,527,247 1,570,357

The 100,000th cuban prime is 1,792,617,147,127
"5.9s"

```



## Python


{{trans|C#}}

```Python

import datetime
import math

primes = [ 3, 5 ]

cutOff = 200

bigUn =  100_000
chunks = 50
little = bigUn / chunks

tn = " cuban prime"
print ("The first {:,}{}s:".format(cutOff, tn))

c = 0
showEach = True
u = 0
v = 1
st = datetime.datetime.now()

for i in range(1, int(math.pow(2,20))):
	found = False
	u += 6
	v += u
	mx = int(math.sqrt(v))

	for item in primes:
		if (item > mx):
			break
		if (v % item == 0):
			found = True
			break

	if (found == 0):
		c += 1
		if (showEach):
			z = primes[-1]
			while (z <= v - 2):
				z += 2

				fnd = False
				for item in primes:
					if (item > mx):
						break
					if (z % item == 0):
						fnd = True
						break

				if (not fnd):
					primes.append(z)

			primes.append(v)
			print("{:>11,}".format(v), end='')

			if (c % 10 == 0):
				print("");
			if (c == cutOff):
				showEach = False
				print ("Progress to the {:,}th {}:".format(bigUn, tn), end='')
		if (c % little == 0):
			print('.', end='')
		if (c == bigUn):
			break

print("");
print ("The {:,}th{} is {:,}".format(c, tn, v))
print("Computation time was {} seconds".format((datetime.datetime.now() - st).seconds))

```


{{out}}

```txt
The first 200 cuban primes:
          7         19         37         61        127        271        331        397        547        631
        919      1,657      1,801      1,951      2,269      2,437      2,791      3,169      3,571      4,219
      4,447      5,167      5,419      6,211      7,057      7,351      8,269      9,241     10,267     11,719
     12,097     13,267     13,669     16,651     19,441     19,927     22,447     23,497     24,571     25,117
     26,227     27,361     33,391     35,317     42,841     45,757     47,251     49,537     50,311     55,897
     59,221     60,919     65,269     70,687     73,477     74,419     75,367     81,181     82,171     87,211
     88,237     89,269     92,401     96,661    102,121    103,231    104,347    110,017    112,327    114,661
    115,837    126,691    129,169    131,671    135,469    140,617    144,541    145,861    151,201    155,269
    163,567    169,219    170,647    176,419    180,811    189,757    200,467    202,021    213,067    231,019
    234,361    241,117    246,247    251,431    260,191    263,737    267,307    276,337    279,991    283,669
    285,517    292,969    296,731    298,621    310,087    329,677    333,667    337,681    347,821    351,919
    360,187    368,551    372,769    374,887    377,011    383,419    387,721    398,581    407,377    423,001
    436,627    452,797    459,817    476,407    478,801    493,291    522,919    527,941    553,411    574,219
    584,767    590,077    592,741    595,411    603,457    608,851    611,557    619,711    627,919    650,071
    658,477    666,937    689,761    692,641    698,419    707,131    733,591    742,519    760,537    769,627
    772,669    784,897    791,047    812,761    825,301    837,937    847,477    863,497    879,667    886,177
    895,987    909,151    915,769    925,741    929,077    932,419    939,121    952,597    972,991    976,411
    986,707    990,151    997,057  1,021,417  1,024,921  1,035,469  1,074,607  1,085,407  1,110,817  1,114,471
  1,125,469  1,155,061  1,177,507  1,181,269  1,215,397  1,253,887  1,281,187  1,285,111  1,324,681  1,328,671
  1,372,957  1,409,731  1,422,097  1,426,231  1,442,827  1,451,161  1,480,519  1,484,737  1,527,247  1,570,357
Progress to the 100,000th  cuban prime:..................................................
The 100,000th cuban prime is 1,792,617,147,127
Computation time was 856 seconds
```



## REXX

Cuban primes can't end in an even (decimal) digit,   or the digit   '''5'''.

Also, by their construction, cuban primes can't have a
factor of   '''6*k + 1''',   where   '''k'''   is any positive integer.

```rexx
/*REXX program finds and displays a number of  cuban  primes  or the  Nth  cuban prime. */
numeric digits 20                                /*ensure enough decimal digits for #s. */
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 200                   /*Not specified?  Then use the default.*/
Nth= N<0;               N= abs(N)                /*used for finding the Nth cuban prime.*/
@.=0; @.0=1; @.2=1; @.3=1; @.4=1; @.5=1; @.6=1; @.8=1  /*ending digs that aren't cubans.*/
sw= linesize() - 1;    if sw<1  then sw= 79      /*obtain width of the terminal screen. */
w=12;              #= 1;    $= right(7, w)       /*start with first cuban prime;  count.*/
     do j=1  until #=>N;    x= (j+1)**3 - j**3   /*compute a possible cuban prime.      */
     parse var x '' -1 _;   if @._  then iterate /*check last digit for non─cuban prime.*/
            do k=1  until km*km>x;  km= k*6 + 1  /*cuban primes can't be   ÷   by  6k+1 */
            if x//km==0  then iterate j          /*Divisible?   Then not a cuban prime. */
            end   /*k*/
     #= #+1                                      /*bump the number of cuban primes found*/
     if Nth  then do;  if #==N  then do;  say commas(x);  leave j;  end /*display 1 num.*/
                                else iterate /*j*/                      /*keep searching*/
                  end                            /* [↑]  try to fit as many #s per line.*/
     cx= commas(x);  L= length(cx)               /*insert commas──►X; obtain the length.*/
     cx= right(cx, max(w, L) );   new= $  cx     /*right justify  CX; concat to new list*/
     if length(new)>sw  then do;  say $;  $= cx  /*line is too long, display #'s so far.*/
                             end                 /* [↑]  initialize the (new) next line.*/
                        else              $= new /*start with cuban # that wouldn't fit.*/
     end   /*j*/
                  if \Nth  &  $\==''  then say $ /*check for residual cuban primes in $.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;  do jc=length(_)-3  to 1  by -3; _=insert(',', _, jc); end;  return _
```

This REXX program makes use of   '''LINESIZE'''   REXX program   (or
BIF)   which is used to determine the screen width

(or linesize)   of the terminal (console).   Some REXXes don't have this BIF.

The   '''LINESIZE.REX'''   REXX program is included
here   ───►   [[LINESIZE.REX]].

{{out|output|text=  when using the default input of:     <tt> 200 </tt>}}

(Shown at three-quarter size.)

<pre style="font-size:75%">
           7           19           37           61          127          271          331          397          547          631          919
       1,657        1,801        1,951        2,269        2,437        2,791        3,169        3,571        4,219        4,447        5,167
       5,419        6,211        7,057        7,351        8,269        9,241       10,267       11,719       12,097       13,267       13,669
      16,651       19,441       19,927       22,447       23,497       24,571       25,117       26,227       27,361       33,391       35,317
      42,841       45,757       47,251       49,537       50,311       55,897       59,221       60,919       65,269       70,687       73,477
      74,419       75,367       81,181       82,171       87,211       88,237       89,269       92,401       96,661      102,121      103,231
     104,347      110,017      112,327      114,661      115,837      126,691      129,169      131,671      135,469      140,617      144,541
     145,861      151,201      155,269      163,567      169,219      170,647      176,419      180,811      189,757      200,467      202,021
     213,067      231,019      234,361      241,117      246,247      251,431      260,191      263,737      267,307      276,337      279,991
     283,669      285,517      292,969      296,731      298,621      310,087      329,677      333,667      337,681      347,821      351,919
     360,187      368,551      372,769      374,887      377,011      383,419      387,721      398,581      407,377      423,001      436,627
     452,797      459,817      476,407      478,801      493,291      522,919      527,941      553,411      574,219      584,767      590,077
     592,741      595,411      603,457      608,851      611,557      619,711      627,919      650,071      658,477      666,937      689,761
     692,641      698,419      707,131      733,591      742,519      760,537      769,627      772,669      784,897      791,047      812,761
     825,301      837,937      847,477      863,497      879,667      886,177      895,987      909,151      915,769      925,741      929,077
     932,419      939,121      952,597      972,991      976,411      986,707      990,151      997,057    1,021,417    1,024,921    1,035,469
   1,074,607    1,085,407    1,110,817    1,114,471    1,125,469    1,155,061    1,177,507    1,181,269    1,215,397    1,253,887    1,281,187
   1,285,111    1,324,681    1,328,671    1,372,957    1,409,731    1,422,097    1,426,231    1,442,827    1,451,161    1,480,519    1,484,737
   1,527,247    1,570,357

```

{{out|output|text=  when using the input of:     <tt> -100000 </tt>}}

```txt

1,792,617,147,127

```



## Ruby


```ruby
require "openssl"

RE = /(\d)(?=(\d\d\d)+(?!\d))/ # Activesupport uses this for commatizing
cuban_primes = Enumerator.new do |y|
  (1..).each do |n|
    cand = 3*n*(n+1) + 1
    y << cand if OpenSSL::BN.new(cand).prime?
  end
end

def commatize(num)
  num.to_s.gsub(RE, "\\1,")
end

cbs = cuban_primes.take(200)
formatted = cbs.map{|cb| commatize(cb).rjust(10) }
puts formatted.each_slice(10).map(&:join)

t0 = Time.now
puts "
100_000th cuban prime is #{commatize( cuban_primes.take(100_000).last)}
which took #{(Time.now-t0).round} seconds to calculate."
```

{{out}}

```txt
         7        19        37        61       127       271       331       397       547       631
       919     1,657     1,801     1,951     2,269     2,437     2,791     3,169     3,571     4,219
     4,447     5,167     5,419     6,211     7,057     7,351     8,269     9,241    10,267    11,719
    12,097    13,267    13,669    16,651    19,441    19,927    22,447    23,497    24,571    25,117
    26,227    27,361    33,391    35,317    42,841    45,757    47,251    49,537    50,311    55,897
    59,221    60,919    65,269    70,687    73,477    74,419    75,367    81,181    82,171    87,211
    88,237    89,269    92,401    96,661   102,121   103,231   104,347   110,017   112,327   114,661
   115,837   126,691   129,169   131,671   135,469   140,617   144,541   145,861   151,201   155,269
   163,567   169,219   170,647   176,419   180,811   189,757   200,467   202,021   213,067   231,019
   234,361   241,117   246,247   251,431   260,191   263,737   267,307   276,337   279,991   283,669
   285,517   292,969   296,731   298,621   310,087   329,677   333,667   337,681   347,821   351,919
   360,187   368,551   372,769   374,887   377,011   383,419   387,721   398,581   407,377   423,001
   436,627   452,797   459,817   476,407   478,801   493,291   522,919   527,941   553,411   574,219
   584,767   590,077   592,741   595,411   603,457   608,851   611,557   619,711   627,919   650,071
   658,477   666,937   689,761   692,641   698,419   707,131   733,591   742,519   760,537   769,627
   772,669   784,897   791,047   812,761   825,301   837,937   847,477   863,497   879,667   886,177
   895,987   909,151   915,769   925,741   929,077   932,419   939,121   952,597   972,991   976,411
   986,707   990,151   997,057 1,021,417 1,024,921 1,035,469 1,074,607 1,085,407 1,110,817 1,114,471
 1,125,469 1,155,061 1,177,507 1,181,269 1,215,397 1,253,887 1,281,187 1,285,111 1,324,681 1,328,671
 1,372,957 1,409,731 1,422,097 1,426,231 1,442,827 1,451,161 1,480,519 1,484,737 1,527,247 1,570,357

100_000th cuban prime is 1,792,617,147,127
which took 31 seconds to calculate.

```



## Rust

Uses the libraries [https://crates.io/crates/primal primal] and [https://crates.io/crates/separator separator]

```rust
use std::time::Instant;
use separator::Separatable;

const NUMBER_OF_CUBAN_PRIMES: usize = 200;
const COLUMNS: usize = 10;
const LAST_CUBAN_PRIME: usize = 100_000;

fn main() {
    println!("Calculating the first {} cuban primes and the {}th cuban prime...", NUMBER_OF_CUBAN_PRIMES, LAST_CUBAN_PRIME);
    let start = Instant::now();

    let mut i: u64 = 0;
    let mut j: u64 = 1;
    let mut index: usize = 0;
    let mut cuban_primes = Vec::new();
    let mut cuban: u64 = 0;
    while index < 100_000 {
        cuban = {j += 1; j}.pow(3) - {i += 1; i}.pow(3);
        if primal::is_prime(cuban) {
            if index < NUMBER_OF_CUBAN_PRIMES {
                cuban_primes.push(cuban);
            }
            index += 1;
        }
    }

    let elapsed = start.elapsed();
    println!("THE {} FIRST CUBAN PRIMES:", NUMBER_OF_CUBAN_PRIMES);
    cuban_primes
        .chunks(COLUMNS)
        .map(|chunk| {
            chunk.iter()
                .map(|item| {
                    print!("{}\t", item)
                })
                .for_each(drop);
            println!("");
        })
        .for_each(drop);
    println!("The {}th cuban prime number is {}", LAST_CUBAN_PRIME, cuban.separated_string());
    println!("Elapsed time: {:?}", elapsed);
}
```

{{out}}

```txt
Calculating the first 200 cuban primes and the 100000th cuban prime...
THE 200 FIRST CUBAN PRIMES:
7       19      37      61      127     271     331     397     547     631
919     1657    1801    1951    2269    2437    2791    3169    3571    4219
4447    5167    5419    6211    7057    7351    8269    9241    10267   11719
12097   13267   13669   16651   19441   19927   22447   23497   24571   25117
26227   27361   33391   35317   42841   45757   47251   49537   50311   55897
59221   60919   65269   70687   73477   74419   75367   81181   82171   87211
88237   89269   92401   96661   102121  103231  104347  110017  112327  114661
115837  126691  129169  131671  135469  140617  144541  145861  151201  155269
163567  169219  170647  176419  180811  189757  200467  202021  213067  231019
234361  241117  246247  251431  260191  263737  267307  276337  279991  283669
285517  292969  296731  298621  310087  329677  333667  337681  347821  351919
360187  368551  372769  374887  377011  383419  387721  398581  407377  423001
436627  452797  459817  476407  478801  493291  522919  527941  553411  574219
584767  590077  592741  595411  603457  608851  611557  619711  627919  650071
658477  666937  689761  692641  698419  707131  733591  742519  760537  769627
772669  784897  791047  812761  825301  837937  847477  863497  879667  886177
895987  909151  915769  925741  929077  932419  939121  952597  972991  976411
986707  990151  997057  1021417 1024921 1035469 1074607 1085407 1110817 1114471
1125469 1155061 1177507 1181269 1215397 1253887 1281187 1285111 1324681 1328671
1372957 1409731 1422097 1426231 1442827 1451161 1480519 1484737 1527247 1570357
The 100000th cuban prime number is 1,792,617,147,127
Elapsed time: 11.005581564s
```



## Scala

In this example, we start by building an infinite lazy list of cubans and filter out non-primes. This gives us a lazily evaluated list of all cuban primes, and finding the first 200 simply involves taking 200 elements off the list.

To find the 100,000th cuban prime, performance becomes an issue. To remedy this, we write a function that breaks off a chunk from the front of the list of cubans and filters it using a parallel vector, repeating this process until it's found enough cuban primes. This allows us to benefit from the memory efficiency of lazy lists and the number-crunching speed of parallel vectors at the same time.

Spire's SafeLong is used instead of Java's BigInt for performance.

```scala
import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector

object CubanPrimes {
  def main(args: Array[String]): Unit = {
    println(formatTable(cubanPrimes.take(200).toVector, 10))
    println(f"The 100,000th cuban prime is: ${getNthCubanPrime(100000).toBigInt}%,d")
  }

  def cubanPrimes: LazyList[SafeLong] = cubans.filter(isPrime)
  def cubans: LazyList[SafeLong] = LazyList.iterate(SafeLong(0))(_ + 1).map(n => (n + 1).pow(3) - n.pow(3))
  def isPrime(num: SafeLong): Boolean = (num > 1) && !(SafeLong(2) #:: LazyList.iterate(SafeLong(3)){n => n + 2}).takeWhile(n => n*n <= num).exists(num%_ == 0)

  def getNthCubanPrime(num: Int): SafeLong = {
    @tailrec
    def nHelper(rem: Int, src: LazyList[SafeLong]): SafeLong = {
      val cprimes = src.take(100000).to(ParVector).filter(isPrime)
      if(cprimes.size < rem) nHelper(rem - cprimes.size, src.drop(100000))
      else cprimes.toVector.sortWith(_<_)(rem - 1)
    }

    nHelper(num, cubans)
  }

  def formatTable(lst: Vector[SafeLong], rlen: Int): String = {
    @tailrec
    def fHelper(ac: Vector[String], src: Vector[String]): String = {
      if(src.nonEmpty) fHelper(ac :+ src.take(rlen).mkString, src.drop(rlen))
      else ac.mkString("\n")
    }

    val maxLen = lst.map(n => f"${n.toBigInt}%,d".length).max
    val formatted = lst.map(n => s"%,${maxLen + 2}d".format(n.toInt))

    fHelper(Vector[String](), formatted)
  }
}
```


{{out}}

```txt
          7         19         37         61        127        271        331        397        547        631
        919      1,657      1,801      1,951      2,269      2,437      2,791      3,169      3,571      4,219
      4,447      5,167      5,419      6,211      7,057      7,351      8,269      9,241     10,267     11,719
     12,097     13,267     13,669     16,651     19,441     19,927     22,447     23,497     24,571     25,117
     26,227     27,361     33,391     35,317     42,841     45,757     47,251     49,537     50,311     55,897
     59,221     60,919     65,269     70,687     73,477     74,419     75,367     81,181     82,171     87,211
     88,237     89,269     92,401     96,661    102,121    103,231    104,347    110,017    112,327    114,661
    115,837    126,691    129,169    131,671    135,469    140,617    144,541    145,861    151,201    155,269
    163,567    169,219    170,647    176,419    180,811    189,757    200,467    202,021    213,067    231,019
    234,361    241,117    246,247    251,431    260,191    263,737    267,307    276,337    279,991    283,669
    285,517    292,969    296,731    298,621    310,087    329,677    333,667    337,681    347,821    351,919
    360,187    368,551    372,769    374,887    377,011    383,419    387,721    398,581    407,377    423,001
    436,627    452,797    459,817    476,407    478,801    493,291    522,919    527,941    553,411    574,219
    584,767    590,077    592,741    595,411    603,457    608,851    611,557    619,711    627,919    650,071
    658,477    666,937    689,761    692,641    698,419    707,131    733,591    742,519    760,537    769,627
    772,669    784,897    791,047    812,761    825,301    837,937    847,477    863,497    879,667    886,177
    895,987    909,151    915,769    925,741    929,077    932,419    939,121    952,597    972,991    976,411
    986,707    990,151    997,057  1,021,417  1,024,921  1,035,469  1,074,607  1,085,407  1,110,817  1,114,471
  1,125,469  1,155,061  1,177,507  1,181,269  1,215,397  1,253,887  1,281,187  1,285,111  1,324,681  1,328,671
  1,372,957  1,409,731  1,422,097  1,426,231  1,442,827  1,451,161  1,480,519  1,484,737  1,527,247  1,570,357
The 100,000th cuban prime is: 1,792,617,147,127
```



## Sidef


```ruby
func cuban_primes(n) {
    1..Inf -> lazy.map {|k| 3*k*(k+1) + 1 }\
                  .grep{ .is_prime }\
                  .first(n)
}

cuban_primes(200).slices(10).each {
    say .map { "%9s" % .commify }.join(' ')
}

say ("\n100,000th cuban prime is: ", cuban_primes(1e5).last.commify)
```

{{out}}

```txt

        7        19        37        61       127       271       331       397       547       631
      919     1,657     1,801     1,951     2,269     2,437     2,791     3,169     3,571     4,219
    4,447     5,167     5,419     6,211     7,057     7,351     8,269     9,241    10,267    11,719
   12,097    13,267    13,669    16,651    19,441    19,927    22,447    23,497    24,571    25,117
   26,227    27,361    33,391    35,317    42,841    45,757    47,251    49,537    50,311    55,897
   59,221    60,919    65,269    70,687    73,477    74,419    75,367    81,181    82,171    87,211
   88,237    89,269    92,401    96,661   102,121   103,231   104,347   110,017   112,327   114,661
  115,837   126,691   129,169   131,671   135,469   140,617   144,541   145,861   151,201   155,269
  163,567   169,219   170,647   176,419   180,811   189,757   200,467   202,021   213,067   231,019
  234,361   241,117   246,247   251,431   260,191   263,737   267,307   276,337   279,991   283,669
  285,517   292,969   296,731   298,621   310,087   329,677   333,667   337,681   347,821   351,919
  360,187   368,551   372,769   374,887   377,011   383,419   387,721   398,581   407,377   423,001
  436,627   452,797   459,817   476,407   478,801   493,291   522,919   527,941   553,411   574,219
  584,767   590,077   592,741   595,411   603,457   608,851   611,557   619,711   627,919   650,071
  658,477   666,937   689,761   692,641   698,419   707,131   733,591   742,519   760,537   769,627
  772,669   784,897   791,047   812,761   825,301   837,937   847,477   863,497   879,667   886,177
  895,987   909,151   915,769   925,741   929,077   932,419   939,121   952,597   972,991   976,411
  986,707   990,151   997,057 1,021,417 1,024,921 1,035,469 1,074,607 1,085,407 1,110,817 1,114,471
1,125,469 1,155,061 1,177,507 1,181,269 1,215,397 1,253,887 1,281,187 1,285,111 1,324,681 1,328,671
1,372,957 1,409,731 1,422,097 1,426,231 1,442,827 1,451,161 1,480,519 1,484,737 1,527,247 1,570,357

100,000th cuban prime is: 1,792,617,147,127

```



## Visual Basic .NET


### Corner Cutting Version

This language doesn't have a built-in for a ''IsPrime()'' function, so I was surprised to find that this runs so quickly.  It builds a list of primes while it is creating the output table.  Since the last item on the table is larger than the square root of the 100,000<sup>th</sup> cuban prime, there is no need to continue adding to the prime list while checking up to the 100,000<sup>th</sup> cuban prime.  I found a bit of a shortcut, if you skip the iterator by just the right amount, only one value is tested for the final result.  It's hard-coded in the program, so if another final cuban prime were to be selected for output, the program would need a re-write.  If not skipping ahead to the answer, it takes a few seconds over a minute to eventually get to it (see Snail Version below).

```vbnet
Module Module1
    Dim primes As List(Of Long) = {3L, 5L}.ToList()

    Sub Main(args As String())
        Const cutOff As Integer = 200, bigUn As Integer = 100000,
              tn As String = " cuban prime"
        Console.WriteLine("The first {0:n0}{1}s:", cutOff, tn)
        Dim c As Integer = 0, showEach As Boolean = True, skip As Boolean = True,
            v As Long = 0, st As DateTime = DateTime.Now
        For i As Long = 1 To Long.MaxValue
            v = 3 * i : v = v * i + v + 1
            Dim found As Boolean = False, mx As Integer = Math.Ceiling(Math.Sqrt(v))
            For Each item In primes
                If item > mx Then Exit For
                If v Mod item = 0 Then found = True : Exit For
            Next : If Not found Then
                c += 1 : If showEach Then
                    For z = primes.Last + 2 To v - 2 Step 2
                        Dim fnd As Boolean = False
                        For Each item In primes
                            If item > mx Then Exit For
                            If z Mod item = 0 Then fnd = True : Exit For
                        Next : If Not fnd Then primes.Add(z)
                    Next : primes.Add(v) : Console.Write("{0,11:n0}", v)
                    If c Mod 10 = 0 Then Console.WriteLine()
                    If c = cutOff Then showEach = False
                Else
                    If skip Then skip = False : i += 772279 : c = bigUn - 1
                End If
                If c = bigUn Then Exit For
            End If
        Next
        Console.WriteLine("{1}The {2:n0}th{3} is {0,17:n0}", v, vbLf, c, tn)
        Console.WriteLine("Computation time was {0} seconds", (DateTime.Now - st).TotalSeconds)
        If System.Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub
End Module
```

{{out}}

```txt
The first 200 cuban primes:
          7         19         37         61        127        271        331        397        547        631
        919      1,657      1,801      1,951      2,269      2,437      2,791      3,169      3,571      4,219
      4,447      5,167      5,419      6,211      7,057      7,351      8,269      9,241     10,267     11,719
     12,097     13,267     13,669     16,651     19,441     19,927     22,447     23,497     24,571     25,117
     26,227     27,361     33,391     35,317     42,841     45,757     47,251     49,537     50,311     55,897
     59,221     60,919     65,269     70,687     73,477     74,419     75,367     81,181     82,171     87,211
     88,237     89,269     92,401     96,661    102,121    103,231    104,347    110,017    112,327    114,661
    115,837    126,691    129,169    131,671    135,469    140,617    144,541    145,861    151,201    155,269
    163,567    169,219    170,647    176,419    180,811    189,757    200,467    202,021    213,067    231,019
    234,361    241,117    246,247    251,431    260,191    263,737    267,307    276,337    279,991    283,669
    285,517    292,969    296,731    298,621    310,087    329,677    333,667    337,681    347,821    351,919
    360,187    368,551    372,769    374,887    377,011    383,419    387,721    398,581    407,377    423,001
    436,627    452,797    459,817    476,407    478,801    493,291    522,919    527,941    553,411    574,219
    584,767    590,077    592,741    595,411    603,457    608,851    611,557    619,711    627,919    650,071
    658,477    666,937    689,761    692,641    698,419    707,131    733,591    742,519    760,537    769,627
    772,669    784,897    791,047    812,761    825,301    837,937    847,477    863,497    879,667    886,177
    895,987    909,151    915,769    925,741    929,077    932,419    939,121    952,597    972,991    976,411
    986,707    990,151    997,057  1,021,417  1,024,921  1,035,469  1,074,607  1,085,407  1,110,817  1,114,471
  1,125,469  1,155,061  1,177,507  1,181,269  1,215,397  1,253,887  1,281,187  1,285,111  1,324,681  1,328,671
  1,372,957  1,409,731  1,422,097  1,426,231  1,442,827  1,451,161  1,480,519  1,484,737  1,527,247  1,570,357

The 100,000th cuban prime is 1,792,617,147,127
Computation time was 0.2989494 seconds
```


### Snail Version

This one doesn't take any shortcuts.  It could be sped up (Execution time about 15 seconds) by threading chunks of the search for the 100,000<sup>th</sup> cuban prime, but you would have to take a guess about how far to go, which would be hard-coded, so one might as well use the short-cut version if you are willing to overlook that difficulty.

```vbnet
Module Program
    Dim primes As List(Of Long) = {3L, 5L}.ToList()

    Sub Main(args As String())
        Dim taskList As New List(Of Task(Of Integer))
        Const cutOff As Integer = 200, bigUn As Integer = 100000,
              chunks As Integer = 50, little As Integer = bigUn / chunks,
              tn As String = " cuban prime"
        Console.WriteLine("The first {0:n0}{1}s:", cutOff, tn)
        Dim c As Integer = 0, showEach As Boolean = True,
            u As Long = 0, v As Long = 1,
            st As DateTime = DateTime.Now
        For i As Long = 1 To Long.MaxValue
            u += 6 : v += u
            Dim found As Boolean = False, mx As Integer = Math.Ceiling(Math.Sqrt(v))
            For Each item In primes
                If item > mx Then Exit For
                If v Mod item = 0 Then found = True : Exit For
            Next : If Not found Then
                c += 1 : If showEach Then
                    For z = primes.Last + 2 To v - 2 Step 2
                        Dim fnd As Boolean = False
                        For Each item In primes
                            If item > mx Then Exit For
                            If z Mod item = 0 Then fnd = True : Exit For
                        Next : If Not fnd Then primes.Add(z)
                    Next : primes.Add(v) : Console.Write("{0,11:n0}", v)
                    If c Mod 10 = 0 Then Console.WriteLine()
                    If c = cutOff Then showEach = False : _
                        Console.Write("{0}Progress to the {1:n0}th{2}: ", vbLf, bigUn, tn)
                End If
                If c Mod little = 0 Then Console.Write(".") : If c = bigUn Then Exit For
            End If
        Next
        Console.WriteLine("{1}The {2:n0}th{3} is {0,17:n0}", v, vbLf, c, tn)
        Console.WriteLine("Computation time was {0} seconds", (DateTime.Now - st).TotalSeconds)
        If System.Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub
End Module
```

{{out}}

```txt
The first 200 cuban primes:
          7         19         37         61        127        271        331        397        547        631
        919      1,657      1,801      1,951      2,269      2,437      2,791      3,169      3,571      4,219
      4,447      5,167      5,419      6,211      7,057      7,351      8,269      9,241     10,267     11,719
     12,097     13,267     13,669     16,651     19,441     19,927     22,447     23,497     24,571     25,117
     26,227     27,361     33,391     35,317     42,841     45,757     47,251     49,537     50,311     55,897
     59,221     60,919     65,269     70,687     73,477     74,419     75,367     81,181     82,171     87,211
     88,237     89,269     92,401     96,661    102,121    103,231    104,347    110,017    112,327    114,661
    115,837    126,691    129,169    131,671    135,469    140,617    144,541    145,861    151,201    155,269
    163,567    169,219    170,647    176,419    180,811    189,757    200,467    202,021    213,067    231,019
    234,361    241,117    246,247    251,431    260,191    263,737    267,307    276,337    279,991    283,669
    285,517    292,969    296,731    298,621    310,087    329,677    333,667    337,681    347,821    351,919
    360,187    368,551    372,769    374,887    377,011    383,419    387,721    398,581    407,377    423,001
    436,627    452,797    459,817    476,407    478,801    493,291    522,919    527,941    553,411    574,219
    584,767    590,077    592,741    595,411    603,457    608,851    611,557    619,711    627,919    650,071
    658,477    666,937    689,761    692,641    698,419    707,131    733,591    742,519    760,537    769,627
    772,669    784,897    791,047    812,761    825,301    837,937    847,477    863,497    879,667    886,177
    895,987    909,151    915,769    925,741    929,077    932,419    939,121    952,597    972,991    976,411
    986,707    990,151    997,057  1,021,417  1,024,921  1,035,469  1,074,607  1,085,407  1,110,817  1,114,471
  1,125,469  1,155,061  1,177,507  1,181,269  1,215,397  1,253,887  1,281,187  1,285,111  1,324,681  1,328,671
  1,372,957  1,409,731  1,422,097  1,426,231  1,442,827  1,451,161  1,480,519  1,484,737  1,527,247  1,570,357

Progress to the 100,000th cuban prime: ..................................................
The 100,000th cuban prime is 1,792,617,147,127
Computation time was 49.5868152 seconds
```


### k > 1 Version

A VB.NET version of the [http://www.rosettacode.org/wiki/Cuban_primes#Perl_6 Perl 6] version where k > 1, linked at [https://tio.run/##fVVtb9owEP7OrzjlU9KmGbSaJiExqSuthFboNND2cTKJAxaJzWIHqFB/O71zQqghW6SQ3HHP43t5Djbzm1gV/HAYq6TMOFSPXgfwGooc1oXIuYZ7DRO@hWehjf@SwkgavuBF0LFx03KOHmF@2Fg/sM4jQZYbQtcIGMDt524IacYW2sfvAvrym1IZZ7LBPakC5BkKZspy9WGUwkSZmkIGMFtyCX8a8JFg5RJIuMK7JpkavkarX5OsAgyYFSVHz4TvTMNFBvqITrTn4xzrZCbqzKoORixJyAUOtzUeZUIt7EDTzDET0mfFwvZ9agohF35wauuDktpAXJqXND3LCltr5AmFLg8D56xOw3NGE7tgxGZ3wDSIyhXC2jGdUjcfsCFoO@MhM3yGpyDX8TWaqG2Da9VIy6huwSjodatRryBXCdxRflU/sXojJM4KkU5K1BbUUfS7EIY/C8l970kUmNq@@7bvvWnYLnnBkXAA@9u3vhfWLaSWhbAKHLLYHtg6@h6NvjajMdv9YlnJHTBdGaUsUHTV3admkuMaE7DG1cB@0kcf2zkAH99uEBfAJ1hd8NHAUlWiVk4Lg6AnlmkeQr5zcxwzs4weuMhIOtaY/i2MvwmCC2Kq8JHFS8Cu5UhRC/Yirha4jfpKB9ppPO6EuZjEh/AN/aJUoGaCVRnNxv2Tot6@41ZZFBG0nhXDNY0G4x0d@N6@G37pSysBHPkmRD4/tlmhxKzqN/NnFIHntTSnroLUUO/bUYNnUgv@Vwht@CjtnNfWaTVaZPyg8nVpmBFKgqEF2@JaoqpB81jJRGNd/seNQxlpE0QzZVg2rUJOpWE501eNA4mGgi2k0kbEOhryebkgQY/0vTEoB564pf7kLPnOX@vNPf5m0bP6yzgc3gE Try It Online!]


## zkl

Using GMP (GNU Multiple Precision Arithmetic Library, probabilistic
primes), because it is easy and fast to test for primeness.

[[Extensible prime generator#zkl]] could be used instead.

```zkl
var [const] BI=Import("zklBigNum");  // libGMP
cubans:=(1).walker(*).tweak('wrap(n){ // lazy iterator
   p:=3*n*(n + 1) + 1;
   BI(p).probablyPrime() and p or Void.Skip
});
println("First 200 cuban primes:");
do(20){ (10).pump(String, cubans.next, "%10,d".fmt).println() }

cubans.drop(100_000 - cubans.n).value :
   println("\nThe 100,000th cuban prime is: %,d".fmt(_));
```

{{out}}
<pre style="font-size:83%">
First 200 cuban primes:
         7        19        37        61       127       271       331       397       547       631
       919     1,657     1,801     1,951     2,269     2,437     2,791     3,169     3,571     4,219
     4,447     5,167     5,419     6,211     7,057     7,351     8,269     9,241    10,267    11,719
    12,097    13,267    13,669    16,651    19,441    19,927    22,447    23,497    24,571    25,117
    26,227    27,361    33,391    35,317    42,841    45,757    47,251    49,537    50,311    55,897
    59,221    60,919    65,269    70,687    73,477    74,419    75,367    81,181    82,171    87,211
    88,237    89,269    92,401    96,661   102,121   103,231   104,347   110,017   112,327   114,661
   115,837   126,691   129,169   131,671   135,469   140,617   144,541   145,861   151,201   155,269
   163,567   169,219   170,647   176,419   180,811   189,757   200,467   202,021   213,067   231,019
   234,361   241,117   246,247   251,431   260,191   263,737   267,307   276,337   279,991   283,669
   285,517   292,969   296,731   298,621   310,087   329,677   333,667   337,681   347,821   351,919
   360,187   368,551   372,769   374,887   377,011   383,419   387,721   398,581   407,377   423,001
   436,627   452,797   459,817   476,407   478,801   493,291   522,919   527,941   553,411   574,219
   584,767   590,077   592,741   595,411   603,457   608,851   611,557   619,711   627,919   650,071
   658,477   666,937   689,761   692,641   698,419   707,131   733,591   742,519   760,537   769,627
   772,669   784,897   791,047   812,761   825,301   837,937   847,477   863,497   879,667   886,177
   895,987   909,151   915,769   925,741   929,077   932,419   939,121   952,597   972,991   976,411
   986,707   990,151   997,057 1,021,417 1,024,921 1,035,469 1,074,607 1,085,407 1,110,817 1,114,471
 1,125,469 1,155,061 1,177,507 1,181,269 1,215,397 1,253,887 1,281,187 1,285,111 1,324,681 1,328,671
 1,372,957 1,409,731 1,422,097 1,426,231 1,442,827 1,451,161 1,480,519 1,484,737 1,527,247 1,570,357

The 100,000th cuban prime is: 1,792,617,147,127

```

Now lets get big.

```zkl
k,z := BI(2).pow(128), 10;
println("First %d cuban primes where k = %,d:".fmt(z,k));
foreach n in ([BI(1)..]){
   p:=( (k + n).pow(3) - n.pow(3) )/k;
   if(p.probablyPrime()){ println("%,d".fmt(p)); z-=1; }
   if(z<=0) break;
}
```

{{out}}

```txt

First 10 cuban primes where k = 340,282,366,920,938,463,463,374,607,431,768,211,456:
115,792,089,237,316,195,423,570,985,008,687,908,160,544,961,995,247,996,546,884,854,518,799,824,856,507
115,792,089,237,316,195,423,570,985,008,687,908,174,836,821,405,927,412,012,346,588,030,934,089,763,531
115,792,089,237,316,195,423,570,985,008,687,908,219,754,093,839,491,289,189,512,036,211,927,493,764,691
115,792,089,237,316,195,423,570,985,008,687,908,383,089,629,961,541,751,651,931,847,779,176,235,685,011
115,792,089,237,316,195,423,570,985,008,687,908,491,299,422,642,400,183,033,284,972,942,478,527,291,811
115,792,089,237,316,195,423,570,985,008,687,908,771,011,528,251,411,600,000,178,900,251,391,998,361,371
115,792,089,237,316,195,423,570,985,008,687,908,875,137,932,529,218,769,819,971,530,125,513,071,648,307
115,792,089,237,316,195,423,570,985,008,687,908,956,805,700,590,244,001,051,181,435,909,137,442,897,427
115,792,089,237,316,195,423,570,985,008,687,909,028,264,997,643,641,078,378,490,103,469,808,767,771,907
115,792,089,237,316,195,423,570,985,008,687,909,158,933,426,541,281,448,348,425,952,723,607,761,904,131

```

