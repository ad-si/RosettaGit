+++
title = "Chowla numbers"
description = ""
date = 2019-09-18T00:10:29Z
aliases = []
[extra]
id = 22212
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
languages = [
  "awk",
  "c",
  "cpp",
  "d",
  "dyalect",
  "easylang",
  "factor",
  "freebasic",
  "go",
  "groovy",
  "j",
  "julia",
  "kotlin",
  "lua",
  "maple",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powerbasic",
  "python",
  "related_tasks",
  "rexx",
  "scala",
  "visual_basic",
  "visual_basic_.net",
  "zkl",
]
+++

Chowla numbers are also known as:
::*   Chowla's function
::*   the chowla function
::*   the chowla number
::*   the chowla sequence




The chowla number of   <big>'''n'''</big>   is   (as defined by Chowla's function):
::*   the sum of the divisors of   <big>'''n'''</big>     excluding unity and   <big>'''n'''</big>
::*   where   <big>'''n'''</big>   is a positive integer



The sequence is named after   Sarvadaman D. S. Chowla,   (22 October 1907 ──► 10 December 1995),

a London born Indian American mathematician specializing in ''number theory''.



German mathematician Carl Friedrich Gauss (1777─1855) said,   "Mathematics is the queen of the sciences ─
and number theory is the queen of mathematics".



;Definitions:
Chowla numbers can also be expressed as:
    chowla(<big>'''n'''</big>) = sum of divisors of  <big>'''n'''</big>  excluding unity and  <big>'''n'''</big>
    chowla(<big>'''n'''</big>) = sum(       divisors(<big>'''n'''</big>))   <big>'''-1  -  n''' </big>
    chowla(<big>'''n'''</big>) = sum( properDivisors(<big>'''n'''</big>))   <big>'''-1'''       </big>
    chowla(<big>'''n'''</big>) = sum(aliquotDivisors(<big>'''n'''</big>))   <big>'''-1'''       </big>
    chowla(<big>'''n'''</big>) = aliquot(<big>'''n'''</big>)                <big>'''-1'''       </big>
    chowla(<big>'''n'''</big>) = sigma(<big>'''n'''</big>)                  <big>'''-1  -  n''' </big>
    chowla(<big>'''n'''</big>) = sigmaProperDivisiors(<big>'''n'''</big>)   <big>'''-1'''       </big>
    chowla(<big>'''a'''*'''b'''</big>) = <big>'''a''' + '''b'''</big>,    ''if''  <big>'''a'''</big>  and  <big>'''b'''</big>  are distinct primes
    if  chowla(<big>'''n'''</big>) =  <big>'''0'''</big>,       and <big>'''n > 1'''</big>,  then   <big>'''n'''</big>   is prime
    if  chowla(<big>'''n'''</big>) =  <big>'''n - 1'''</big>,  and <big>'''n > 1'''</big>,  then   <big>'''n'''</big>   is a perfect number


## Task

::*   create a   '''chowla'''   function that returns the   '''chowla number'''   for a positive integer   '''n'''
::*   Find and display   (1 per line)   for the 1<sup>st</sup>   '''37'''   integers:
::::*   the integer   (the index)
::::*   the chowla number for that integer
::*   For finding primes, use the   '''chowla'''   function to find values of zero
::*   Find and display the   ''count''   of the primes up to                 '''100'''
::*   Find and display the   ''count''   of the primes up to                         '''1,000'''
::*   Find and display the   ''count''   of the primes up to                             '''10,000'''
::*   Find and display the   ''count''   of the primes up to                                 '''100,000'''
::*   Find and display the   ''count''   of the primes up to                                   '''1,000,000'''
::*   Find and display the   ''count''   of the primes up to                                       '''10,000,000'''
::*   For finding perfect numbers, use the   '''chowla'''   function to find values of   '''n - 1'''
::*   Find and display all   perfect numbers   up to   '''35,000,000'''
::*   use commas within appropriate numbers
::*   show all output here




## Related tasks

:*   [[Totient_function| totient function]]
:*   [[Perfect_numbers|  perfect numbers]]
:*   [[Proper divisors]]
:*   [[Sieve of Eratosthenes]]



## See also

:*   the OEIS entry for   [http://oeis.org/A048050 A48050 Chowla's function].





## AWK


```AWK

# syntax: GAWK -f CHOWLA_NUMBERS.AWK
# converted from Go
BEGIN {
    for (i=1; i<=37; i++) {
      printf("chowla(%2d) = %s\n",i,chowla(i))
    }
    printf("\nCount of primes up to:\n")
    count = 1
    limit = 1e7
    sieve(limit)
    power = 100
    for (i=3; i<limit; i+=2) {
      if (!c[i]) {
        count++
      }
      if (i == power-1) {
        printf("%10s = %s\n",commatize(power),commatize(count))
        power *= 10
      }
    }
    printf("\nPerfect numbers:")
    count = 0
    limit = 35000000
    k = 2
    kk = 3
    while (1) {
      if ((p = k * kk) > limit) {
        break
      }
      if (chowla(p) == p-1) {
        printf("  %s",commatize(p))
        count++
      }
      k = kk + 1
      kk += k
    }
    printf("\nThere are %d perfect numbers <= %s\n",count,commatize(limit))
    exit(0)
}
function chowla(n,  i,j,sum) {
    if (n < 1 || n != int(n)) {
      return sprintf("%s is invalid",n)
    }
    for (i=2; i*i<=n; i++) {
      if (n%i == 0) {
        j = n / i
        sum += (i == j) ? i : i + j
      }
    }
    return(sum+0)
}
function commatize(x,  num) {
    if (x < 0) {
      return "-" commatize(-x)
    }
    x = int(x)
    num = sprintf("%d.",x)
    while (num ~ /^[0-9][0-9][0-9][0-9]/) {
      sub(/[0-9][0-9][0-9][,.]/,",&",num)
    }
    sub(/\.$/,"",num)
    return(num)
}
function sieve(limit,  i,j) {
    for (i=1; i<=limit; i++) {
      c[i] = 0
    }
    for (i=3; i*3<limit; i+=2) {
      if (!c[i] && chowla(i) == 0) {
        for (j=3*i; j<limit; j+=2*i) {
          c[j] = 1
        }
      }
    }
}

```

```txt

chowla( 1) = 0
chowla( 2) = 0
chowla( 3) = 0
chowla( 4) = 2
chowla( 5) = 0
chowla( 6) = 5
chowla( 7) = 0
chowla( 8) = 6
chowla( 9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0

Count of primes up to:
       100 = 25
     1,000 = 168
    10,000 = 1,229
   100,000 = 9,592
 1,000,000 = 78,498
10,000,000 = 664,579

Perfect numbers:  6  28  496  8,128  33,550,336
There are 5 perfect numbers <= 35,000,000

```




## C


```c
#include <stdio.h>

unsigned chowla(const unsigned n) {
  unsigned sum = 0;
  for (unsigned i = 2, j; i * i <= n; i ++) if (n % i == 0) sum += i + (i == (j = n / i) ? 0 : j);
  return sum;
}

int main(int argc, char const *argv[]) {
  unsigned a;
  for (unsigned n = 1; n < 38; n ++) printf("chowla(%u) = %u\n", n, chowla(n));

  unsigned n, count = 0, power = 100;
  for (n = 2; n < 10000001; n ++) {
    if (chowla(n) == 0) count ++;
    if (n % power == 0) printf("There is %u primes < %u\n", count, power), power *= 10;
  }

  count = 0;
  unsigned limit = 350000000;
  unsigned k = 2, kk = 3, p;
  for ( ; ; ) {
    if ((p = k * kk) > limit) break;
    if (chowla(p) == p - 1) {
      printf("%d is a perfect number\n", p);
      count ++;
    }
    k = kk + 1; kk += k;
  }
  printf("There are %u perfect numbers < %u\n", count, limit);
  return 0;
}
```

```txt
chowla(1) = 0
chowla(2) = 0
chowla(3) = 0
chowla(4) = 2
chowla(5) = 0
chowla(6) = 5
chowla(7) = 0
chowla(8) = 6
chowla(9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0
There is 25 primes < 100
There is 168 primes < 1000
There is 1229 primes < 10000
There is 9592 primes < 100000
There is 78498 primes < 1000000
There is 664579 primes < 10000000
6 is a perfect number
28 is a perfect number
496 is a perfect number
8128 is a perfect number
33550336 is a perfect number
There are 5 perfect numbers < 350000000
```



=={{header|C#|csharp}}==
```c#
using System;

namespace chowla_cs
{
    class Program
    {
        static int chowla(int n)
        {
            int sum = 0;
            for (int i = 2, j; i * i <= n; i++)
                if (n % i == 0) sum += i + (i == (j = n / i) ? 0 : j);
            return sum;
        }

        static bool[] sieve(int limit)
        {
            // True denotes composite, false denotes prime.
            // Only interested in odd numbers >= 3
            bool[] c = new bool[limit];
            for (int i = 3; i * 3 < limit; i += 2)
                if (!c[i] && (chowla(i) == 0))
                    for (int j = 3 * i; j < limit; j += 2 * i)
                        c[j] = true;
            return c;
        }

        static void Main(string[] args)
        {
            for (int i = 1; i <= 37; i++)
                Console.WriteLine("chowla({0}) = {1}", i, chowla(i));
            int count = 1, limit = (int)(1e7), power = 100;
            bool[] c = sieve(limit);
            for (int i = 3; i < limit; i += 2)
            {
                if (!c[i]) count++;
                if (i == power - 1)
                {
                    Console.WriteLine("Count of primes up to {0,10:n0} = {1:n0}", power, count);
                    power *= 10;
                }
            }

            count = 0; limit = 35000000;
            int k = 2, kk = 3, p;
            for (int i = 2; ; i++)
            {
                if ((p = k * kk) > limit) break;
                if (chowla(p) == p - 1)
                {
                    Console.WriteLine("{0,10:n0} is a number that is perfect", p);
                    count++;
                }
                k = kk + 1; kk += k;
            }
            Console.WriteLine("There are {0} perfect numbers <= 35,000,000", count);
            if (System.Diagnostics.Debugger.IsAttached) Console.ReadKey();
        }
    }
}
```

```txt
chowla(1) = 0
chowla(2) = 0
chowla(3) = 0
chowla(4) = 2
chowla(5) = 0
chowla(6) = 5
chowla(7) = 0
chowla(8) = 6
chowla(9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0
Count of primes up to        100 = 25
Count of primes up to      1,000 = 168
Count of primes up to     10,000 = 1,229
Count of primes up to    100,000 = 9,592
Count of primes up to  1,000,000 = 78,498
Count of primes up to 10,000,000 = 664,579
         6 is a number that is perfect
        28 is a number that is perfect
       496 is a number that is perfect
     8,128 is a number that is perfect
33,550,336 is a number that is perfect
There are 5 perfect numbers <= 35,000,000

```



## C++

```cpp
#include <vector>
#include <iostream>

using namespace std;

int chowla(int n)
{
	int sum = 0;
	for (int i = 2, j; i * i <= n; i++)
		if (n % i == 0) sum += i + (i == (j = n / i) ? 0 : j);
	return sum;
}

vector<bool> sieve(int limit)
{
	// True denotes composite, false denotes prime.
	// Only interested in odd numbers >= 3
	vector<bool> c(limit);
	for (int i = 3; i * 3 < limit; i += 2)
		if (!c[i] && (chowla(i) == 0))
			for (int j = 3 * i; j < limit; j += 2 * i)
				c[j] = true;
	return c;
}

int main()
{
	cout.imbue(locale(""));
	for (int i = 1; i <= 37; i++)
		cout << "chowla(" << i << ") = " << chowla(i) << "\n";
	int count = 1, limit = (int)(1e7), power = 100;
	vector<bool> c = sieve(limit);
	for (int i = 3; i < limit; i += 2)
	{
		if (!c[i]) count++;
		if (i == power - 1)
		{
			cout << "Count of primes up to " << power << " = "<< count <<"\n";
			power *= 10;
		}
	}

	count = 0; limit = 35000000;
	int k = 2, kk = 3, p;
	for (int i = 2; ; i++)
	{
		if ((p = k * kk) > limit) break;
		if (chowla(p) == p - 1)
		{
			cout << p << " is a number that is perfect\n";
			count++;
		}
		k = kk + 1; kk += k;
	}
	cout << "There are " << count << " perfect numbers <= 35,000,000\n";
	return 0;
}
```

```txt
chowla(1) = 0
chowla(2) = 0
chowla(3) = 0
chowla(4) = 2
chowla(5) = 0
chowla(6) = 5
chowla(7) = 0
chowla(8) = 6
chowla(9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0
Count of primes up to 100 = 25
Count of primes up to 1,000 = 168
Count of primes up to 10,000 = 1,229
Count of primes up to 100,000 = 9,592
Count of primes up to 1,000,000 = 78,498
Count of primes up to 10,000,000 = 664,579
6 is a number that is perfect
28 is a number that is perfect
496 is a number that is perfect
8,128 is a number that is perfect
33,550,336 is a number that is perfect
There are 5 perfect numbers <= 35,000,000
```



## D

```d
import std.stdio;

int chowla(int n) {
    int sum;
    for (int i = 2, j; i * i <= n; ++i) {
        if (n % i == 0) {
            sum += i + (i == (j = n / i) ? 0 : j);
        }
    }
    return sum;
}

bool[] sieve(int limit) {
    // True denotes composite, false denotes prime.
    // Only interested in odd numbers >= 3
    auto c = new bool[limit];
    for (int i = 3; i * 3 < limit; i += 2) {
        if (!c[i] && (chowla(i) == 0)) {
            for (int j = 3 * i; j < limit; j += 2 * i) {
                c[j] = true;
            }
        }
    }
    return c;
}

void main() {
    foreach (i; 1..38) {
        writefln("chowla(%d) = %d", i, chowla(i));
    }
    int count = 1;
    int limit = cast(int)1e7;
    int power = 100;
    bool[] c = sieve(limit);
    for (int i = 3; i < limit; i += 2) {
        if (!c[i]) {
            count++;
        }
        if (i == power - 1) {
            writefln("Count of primes up to %10d = %d", power, count);
            power *= 10;
        }
    }

    count = 0;
    limit = 350_000_000;
    int k = 2;
    int kk = 3;
    int p;
    for (int i = 2; ; ++i) {
        p = k * kk;
        if (p > limit) {
            break;
        }
        if (chowla(p) == p - 1) {
            writefln("%10d is a number that is perfect", p);
            count++;
        }
        k = kk + 1;
        kk += k;
    }
    writefln("There are %d perfect numbers <= 35,000,000", count);
}
```

```txt
chowla(1) = 0
chowla(2) = 0
chowla(3) = 0
chowla(4) = 2
chowla(5) = 0
chowla(6) = 5
chowla(7) = 0
chowla(8) = 6
chowla(9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0
Count of primes up to        100 = 25
Count of primes up to       1000 = 168
Count of primes up to      10000 = 1229
Count of primes up to     100000 = 9592
Count of primes up to    1000000 = 78498
Count of primes up to   10000000 = 664579
         6 is a number that is perfect
        28 is a number that is perfect
       496 is a number that is perfect
      8128 is a number that is perfect
  33550336 is a number that is perfect
There are 5 perfect numbers <= 35,000,000
```



## Dyalect


```dyalect
func chowla(n) {
    var sum = 0
    var i = 2
    var j = 0
    while i * i <= n {
        if n % i == 0 {
            var app = if i == (j = n / i) {
                0
            } else {
                j
            }
            sum += i + app
        }
        i += 1
    }
    return sum
}

func sieve(limit) {
    var c = Array.empty(limit)
    var i = 3
    while i * 3 < limit {
        if !c[i] && (chowla(i) == 0) {
            var j = 3 * i
            while j < limit {
                c[j] = true
                j += 2 * i
            }
        }
        i += 2
    }
    return c
}

for i in 1..37 {
    print("chowla(\(i)) = \(chowla(i))")
}

var count = 1
var limit = 10000000
var power = 100
var c = sieve(limit);

var i = 3
while i < limit {
    if !c[i] {
        count += 1
    }
    if i == power - 1 {
        print("Count of primes up to \(power) = \(count)")
        power *= 10
    }
    i += 2
}

count = 0
limit = 35000000;
var k = 2
var kk = 3
var p
i = 2

while true {
    if (p = k * kk) > limit {
        break
    }
    if chowla(p) == p - 1 {
        print("\(p) is a number that is perfect")
        count += 1
    }
    k = kk + 1
    kk += k
}

print("There are \(count) perfect numbers <= 35,000,000")
```


```txt
chowla(1) = 0
chowla(2) = 0
chowla(3) = 0
chowla(4) = 2
chowla(5) = 0
chowla(6) = 5
chowla(7) = 0
chowla(8) = 6
chowla(9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0
Count of primes up to 100 = 25
Count of primes up to 1000 = 168
Count of primes up to 10000 = 1229
Count of primes up to 100000 = 9592
Count of primes up to 1000000 = 78498
Count of primes up to 10000000 = 664579
6 is a number that is perfect
28 is a number that is perfect
496 is a number that is perfect
8128 is a number that is perfect
33550336 is a number that is perfect
There are 5 perfect numbers <= 35,000,000
```



## EasyLang

<lang>func chowla n . sum .
  sum = 0
  i = 2
  while i * i <= n
    if n mod i = 0
      j = n / i
      if i = j
        sum += i
      else
        sum += i + j
      .
    .
    i += 1
  .
.
func sieve . c[] .
  i = 3
  while i * 3 < len c[]
    if c[i] = 0
      call chowla i h
      if h = 0
        j = 3 * i
        while j < len c[]
          c[j] = 1
          j += 2 * i
        .
      .
    .
    i += 2
  .
.
func commatize n . s$ .
  s$[] = str_split n
  s$ = ""
  l = len s$[]
  for i range len s$[]
    if i > 0 and l mod 3 = 0
      s$ &= ","
    .
    l -= 1
    s$ &= s$[i]
  .
.
print "chowla number from 1 to 37"
for i = 1 to 37
  call chowla i h
  print "  " & i & ": " & h
.
func main . .
  print ""
  len c[] 10000000
  count = 1
  call sieve c[]
  power = 100
  i = 3
  while i < len c[]
    if c[i] = 0
      count += 1
    .
    if i = power - 1
      call commatize power p$
      call commatize count c$
      print "There are " & c$ & " primes up to " & p$
      power *= 10
    .
    i += 2
  .
  print ""
  limit = 35000000
  count = 0
  i = 2
  k = 2
  kk = 3
  repeat
    p = k * kk
    until p > limit
    call chowla p h
    if h = p - 1
      call commatize p s$
      print s$ & " is a perfect number"
      count += 1
    .
    k = kk + 1
    kk += k
    i += 1
  .
  call commatize limit s$
  print "There are " & count & " perfect mumbers up to " & s$
.
call main
```

```txt

chowla number from 1 to 37
  1: 0
  2: 0
  3: 0
  4: 2
  5: 0
  6: 5
  7: 0
  8: 6
  9: 3
  10: 7
  11: 0
  12: 15
  13: 0
  14: 9
  15: 8
  16: 14
  17: 0
  18: 20
  19: 0
  20: 21
  21: 10
  22: 13
  23: 0
  24: 35
  25: 5
  26: 15
  27: 12
  28: 27
  29: 0
  30: 41
  31: 0
  32: 30
  33: 14
  34: 19
  35: 12
  36: 54
  37: 0

There are 25 primes up to 100
There are 168 primes up to 1,000
There are 1,229 primes up to 10,000
There are 9,592 primes up to 100,000
There are 78,498 primes up to 1,000,000
There are 664,579 primes up to 10,000,000

6 is a perfect number
28 is a perfect number
496 is a perfect number
8,128 is a perfect number
33,550,336 is a perfect number
There are 5 perfect mumbers up to 35,000,000

```



## Factor


```factor
USING: formatting fry grouping.extras io kernel math
math.primes.factors math.ranges math.statistics sequences
tools.memory.private ;
IN: rosetta-code.chowla-numbers

: chowla ( n -- m )
    dup 1 = [ 1 - ] [ [ divisors sum ] [ - 1 - ] bi ] if ;

: show-chowla ( n -- )
    [1,b] [ dup chowla "chowla(%02d) = %d\n" printf ] each ;

: count-primes ( seq -- )
    dup 0 prefix [ [ 1 + ] dip 2 <range> ] 2clump-map
    [ [ chowla zero? ] count ] map cum-sum
    [ [ commas ] bi@ "Primes up to %s: %s\n" printf ] 2each ;

: show-perfect ( n -- )
    [ 2 3 ] dip '[ 2dup * dup _ > ] [
        dup [ chowla ] [ 1 - = ] bi
        [ commas "%s is perfect\n" printf ] [ drop ] if
        [ nip 1 + ] [ nip dupd + ] 2bi
    ] until 3drop ;

: chowla-demo ( -- )
    37 show-chowla nl { 100 1000 10000 100000 1000000 10000000 }
    count-primes nl 35e7 show-perfect ;

MAIN: chowla-demo
```

```txt

chowla(01) = 0
chowla(02) = 0
chowla(03) = 0
chowla(04) = 2
chowla(05) = 0
chowla(06) = 5
chowla(07) = 0
chowla(08) = 6
chowla(09) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0

Primes up to 100: 25
Primes up to 1,000: 168
Primes up to 10,000: 1,229
Primes up to 100,000: 9,592
Primes up to 1,000,000: 78,498
Primes up to 10,000,000: 664,579

6 is perfect
28 is perfect
496 is perfect
8,128 is perfect
33,550,336 is perfect

```



## FreeBASIC

```freebasic

' Chowla_numbers

#include "string.bi"

Dim Shared As Long limite
limite = 10000000
Dim Shared As Boolean c(limite)
Dim As Long count, topenumprimo, a
count = 1
topenumprimo = 100
Dim As Longint p, k, kk, limitenumperfect
limitenumperfect = 35000000
k = 2: kk = 3

Declare Function chowla(Byval n As Longint) As Longint
Declare Sub sieve(Byval limite As Long, c() As Boolean)

Function chowla(Byval n As Longint) As Longint
    Dim As Long i, j, r
    i = 2
    Do While i * i <= n
        j = n \ i
        If n Mod i = 0 Then
            r += i
            If i <> j Then r += j
        End If
        i += 1
    Loop
    chowla = r
End Function

Sub sieve(Byval limite As Long, c() As Boolean)
    Dim As Long i, j
    Redim As Boolean c(limite - 1)
    i = 3
    Do While i * 3 < limite
        If Not c(i) Then
            If chowla(i) = false Then
                j = 3 * i
                Do While j < limite
                    c(j) = true
                    j += 2 * i
                Loop
            End If
        End If
        i += 2
    Loop
End Sub

Print "Chowla numbers"
For a = 1 To 37
    Print "chowla(" & Trim(Str(a)) & ") = " & Trim(Str(chowla(a)))
Next a

' Si chowla(n) = falso and n > 1 Entonces n es primo
Print: Print "Contando los numeros primos hasta: "
sieve(limite, c())
For a = 3 To limite - 1 Step 2
    If Not c(a) Then count += 1
    If a = topenumprimo - 1 Then
        Print Using "########## hay"; topenumprimo;
        Print count
        topenumprimo *= 10
    End If
Next a

' Si chowla(n) = n - 1 and n > 1 Entonces n es un número perfecto
Print: Print "Buscando numeros perfectos... "
count = 0
Do
    p = k * kk : If p > limitenumperfect Then Exit Do
    If chowla(p) = p - 1 Then
        Print Using "##########,# es un numero perfecto"; p
        count += 1
    End If
    k = kk + 1 : kk += k
Loop
Print: Print "Hay " & count & " numeros perfectos <= " & Format(limitenumperfect, "###############################,#")

Print: Print "Pulsa una tecla para salir"
Sleep
End

```

```txt

Chowla numbers
chowla(1) = 0
chowla(2) = 0
chowla(3) = 0
chowla(4) = 2
chowla(5) = 0
chowla(6) = 5
chowla(7) = 0
chowla(8) = 6
chowla(9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0

Contando los numeros primos hasta:
       100 hay 25
      1000 hay 168
     10000 hay 1229
    100000 hay 9592
   1000000 hay 78498
  10000000 hay 664579

Buscando numeros perfectos...
           6 es un numero perfecto
          28 es un numero perfecto
         496 es un numero perfecto
       8,128 es un numero perfecto
  33,550,336 es un numero perfecto

Hay 5 numeros perfectos <= 35.000.000

Pulsa una tecla para salir

```



## Go


```go
package main

import "fmt"

func chowla(n int) int {
    if n < 1 {
        panic("argument must be a positive integer")
    }
    sum := 0
    for i := 2; i*i <= n; i++ {
        if n%i == 0 {
            j := n / i
            if i == j {
                sum += i
            } else {
                sum += i + j
            }
        }
    }
    return sum
}

func sieve(limit int) []bool {
    // True denotes composite, false denotes prime.
    // Only interested in odd numbers >= 3
    c := make([]bool, limit)
    for i := 3; i*3 < limit; i += 2 {
        if !c[i] && chowla(i) == 0 {
            for j := 3 * i; j < limit; j += 2 * i {
                c[j] = true
            }
        }
    }
    return c
}

func commatize(n int) string {
    s := fmt.Sprintf("%d", n)
    le := len(s)
    for i := le - 3; i >= 1; i -= 3 {
        s = s[0:i] + "," + s[i:]
    }
    return s
}

func main() {
    for i := 1; i <= 37; i++ {
        fmt.Printf("chowla(%2d) = %d\n", i, chowla(i))
    }
    fmt.Println()

    count := 1
    limit := int(1e7)
    c := sieve(limit)
    power := 100
    for i := 3; i < limit; i += 2 {
        if !c[i] {
            count++
        }
        if i == power-1 {
            fmt.Printf("Count of primes up to %-10s = %s\n", commatize(power), commatize(count))
            power *= 10
        }
    }

    fmt.Println()
    count = 0
    limit = 35000000
    for i := uint(2); ; i++ {
        p := 1 << (i - 1) * (1<<i - 1) // perfect numbers must be of this form
        if p > limit {
            break
        }
        if chowla(p) == p-1 {
            fmt.Printf("%s is a perfect number\n", commatize(p))
            count++
        }
    }
    fmt.Println("There are", count, "perfect numbers <= 35,000,000")
}
```


```txt

chowla( 1) = 0
chowla( 2) = 0
chowla( 3) = 0
chowla( 4) = 2
chowla( 5) = 0
chowla( 6) = 5
chowla( 7) = 0
chowla( 8) = 6
chowla( 9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0

Count of primes up to 100        = 25
Count of primes up to 1,000      = 168
Count of primes up to 10,000     = 1,229
Count of primes up to 100,000    = 9,592
Count of primes up to 1,000,000  = 78,498
Count of primes up to 10,000,000 = 664,579

6 is a perfect number
28 is a perfect number
496 is a perfect number
8,128 is a perfect number
33,550,336 is a perfect number
There are 5 perfect numbers <= 35,000,000

```



## Groovy

```groovy
class Chowla {
    static int chowla(int n) {
        if (n < 1) throw new RuntimeException("argument must be a positive integer")
        int sum = 0
        int i = 2
        while (i * i <= n) {
            if (n % i == 0) {
                int j = (int) (n / i)
                sum += (i == j) ? i : i + j
            }
            i++
        }
        return sum
    }

    static boolean[] sieve(int limit) {
        // True denotes composite, false denotes prime.
        // Only interested in odd numbers >= 3
        boolean[] c = new boolean[limit]
        for (int i = 3; i < limit / 3; i += 2) {
            if (!c[i] && chowla(i) == 0) {
                for (int j = 3 * i; j < limit; j += 2 * i) {
                    c[j] = true
                }
            }
        }
        return c
    }

    static void main(String[] args) {
        for (int i = 1; i <= 37; i++) {
            printf("chowla(%2d) = %d\n", i, chowla(i))
        }
        println()

        int count = 1
        int limit = 10_000_000
        boolean[] c = sieve(limit)
        int power = 100
        for (int i = 3; i < limit; i += 2) {
            if (!c[i]) {
                count++
            }
            if (i == power - 1) {
                printf("Count of primes up to %,10d = %,7d\n", power, count)
                power *= 10
            }
        }
        println()

        count = 0
        limit = 35_000_000
        int i = 2
        while (true) {
            int p = (1 << (i - 1)) * ((1 << i) - 1) // perfect numbers must be of this form
            if (p > limit) break
            if (chowla(p) == p - 1) {
                printf("%,d is a perfect number\n", p)
                count++
            }
            i++
        }
        printf("There are %,d perfect numbers <= %,d\n", count, limit)
    }
}
```

```txt
chowla( 1) = 0
chowla( 2) = 0
chowla( 3) = 0
chowla( 4) = 2
chowla( 5) = 0
chowla( 6) = 5
chowla( 7) = 0
chowla( 8) = 6
chowla( 9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0

Count of primes up to        100 =      25
Count of primes up to      1,000 =     168
Count of primes up to     10,000 =   1,229
Count of primes up to    100,000 =   9,592
Count of primes up to  1,000,000 =  78,498
Count of primes up to 10,000,000 = 664,579

6 is a perfect number
28 is a perfect number
496 is a perfect number
8,128 is a perfect number
33,550,336 is a perfect number
There are 5 perfect numbers <= 35,000,000
```



## J

'''Solution:'''

```j>chowla=:
: -~ >:@#.~/.~&.q:     NB. sum of factors - (n + 1)

intsbelow=: (2 }. i.)"0
countPrimesbelow=: +/@(0 = chowla)@intsbelow
findPerfectsbelow=: (#~ <: = chowla)@intsbelow
```

'''Tasks:'''

```j
   (] ,. chowla) >: i. 37    NB. chowla numbers 1-37
 1  0
 2  0
 3  0
 4  2
 5  0
 6  5
 7  0
 8  6
 9  3
10  7
11  0
12 15
13  0
14  9
15  8
16 14
17  0
18 20
19  0
20 21
21 10
22 13
23  0
24 35
25  5
26 15
27 12
28 27
29  0
30 41
31  0
32 30
33 14
34 19
35 12
36 54
37  0
   countPrimesbelow 100 1000 10000 100000 1000000 10000000
25 168 1229 9592 78498 664579
   findPerfectsbelow 35000000
6 28 496 8128 33550336
```



## Julia


```julia
using Primes, Formatting

function chowla(n)
    if n < 1
        throw("Chowla function argument must be positive")
    elseif n < 4
        return zero(n)
    else
        f = [one(n)]
        for (p,e) in factor(n)
            f = reduce(vcat, [f*p^j for j in 1:e], init=f)
        end
        return sum(f) - one(n) - n
    end
end

function countchowlas(n, asperfect=false, verbose=false)
    count = 0
    for i in 2:n  # 1 is not prime or perfect so skip
        chow = chowla(i)
        if (asperfect && chow == i - 1) || (!asperfect && chow == 0)
            count += 1
            verbose && println("The number $(format(i, commas=true)) is ", asperfect ? "perfect." : "prime.")
        end
    end
    count
end

function testchowla()
    println("The first 37 chowla numbers are:")
    for i in 1:37
        println("Chowla($i) is ", chowla(i))
    end
    for i in [100, 1000, 10000, 100000, 1000000, 10000000]
        println("The count of the primes up to $(format(i, commas=true)) is $(format(countchowlas(i), commas=true))")
    end
    println("The count of perfect numbers up to 35,000,000 is $(countchowlas(35000000, true, true)).")
end

testchowla()

```
```txt

The first 37 chowla numbers are:
Chowla(1) is 0
Chowla(2) is 0
Chowla(3) is 0
Chowla(4) is 2
Chowla(5) is 0
Chowla(6) is 5
Chowla(7) is 0
Chowla(8) is 6
Chowla(9) is 3
Chowla(10) is 7
Chowla(11) is 0
Chowla(12) is 15
Chowla(13) is 0
Chowla(14) is 9
Chowla(15) is 8
Chowla(16) is 14
Chowla(17) is 0
Chowla(18) is 20
Chowla(19) is 0
Chowla(20) is 21
Chowla(21) is 10
Chowla(22) is 13
Chowla(23) is 0
Chowla(24) is 35
Chowla(25) is 5
Chowla(26) is 15
Chowla(27) is 12
Chowla(28) is 27
Chowla(29) is 0
Chowla(30) is 41
Chowla(31) is 0
Chowla(32) is 30
Chowla(33) is 14
Chowla(34) is 19
Chowla(35) is 12
Chowla(36) is 54
Chowla(37) is 0
The count of the primes up to 100 is 25
The count of the primes up to 1,000 is 168
The count of the primes up to 10,000 is 1,229
The count of the primes up to 100,000 is 9,592
The count of the primes up to 1,000,000 is 78,498
The count of the primes up to 10,000,000 is 664,579
The number 6 is perfect.
The number 28 is perfect.
The number 496 is perfect.
The number 8,128 is perfect.
The number 33,550,336 is perfect.
The count of perfect numbers up to 35,000,000 is 5.

```



## Kotlin

```scala
// Version 1.3.21

fun chowla(n: Int): Int {
    if (n < 1) throw RuntimeException("argument must be a positive integer")
    var sum = 0
    var i = 2
    while (i * i <= n) {
        if (n % i == 0) {
            val j = n / i
            sum += if (i == j) i else i + j
        }
        i++
    }
    return sum
}

fun sieve(limit: Int): BooleanArray {
    // True denotes composite, false denotes prime.
    // Only interested in odd numbers >= 3
    val c = BooleanArray(limit)
    for (i in 3 until limit / 3 step 2) {
        if (!c[i] && chowla(i) == 0) {
            for (j in 3 * i until limit step 2 * i) c[j] = true
        }
    }
    return c
}

fun main() {
    for (i in 1..37) {
        System.out.printf("chowla(%2d) = %d\n", i, chowla(i))
    }
    println()

    var count = 1
    var limit = 10_000_000
    val c = sieve(limit)
    var power = 100
    for (i in 3 until limit step 2) {
        if (!c[i]) count++
        if (i == power - 1) {
            System.out.printf("Count of primes up to %,-10d = %,d\n", power, count)
            power *= 10
        }
    }

    println()
    count = 0
    limit = 35_000_000
    var i = 2
    while (true) {
        val p = (1 shl (i - 1)) * ((1 shl i) - 1) // perfect numbers must be of this form
        if (p > limit) break
        if (chowla(p) == p - 1) {
            System.out.printf("%,d is a perfect number\n", p)
            count++
        }
        i++
    }
    println("There are $count perfect numbers <= 35,000,000")
}
```


```txt

Same as Go example.

```



## Lua

```lua
function chowla(n)
    local sum = 0
    local i = 2
    local j = 0
    while i * i <= n do
        if n % i == 0 then
            j = math.floor(n / i)
            sum = sum + i
            if i ~= j then
                sum = sum + j
            end
        end
        i = i + 1
    end
    return sum
end

function sieve(limit)
    -- True denotes composite, false denotes prime.
    -- Only interested in odd numbers >= 3
    local c = {}
    local i = 3
    while i * 3 < limit do
        if not c[i] and (chowla(i) == 0) then
            local j = 3 * i
            while j < limit do
                c[j] = true
                j = j + 2 * i
            end
        end
        i = i + 2
    end
    return c
end

function main()
    for i = 1, 37 do
        print(string.format("chowla(%d) = %d", i, chowla(i)))
    end
    local count = 1
    local limit = math.floor(1e7)
    local power = 100
    local c = sieve(limit)
    local i = 3
    while i < limit do
        if not c[i] then
            count = count + 1
        end
        if i == power - 1 then
            print(string.format("Count of primes up to %10d = %d", power, count))
            power = power * 10
        end
        i = i + 2
    end

    count = 0
    limit = 350000000
    local k = 2
    local kk = 3
    local p = 0
    i = 2
    while true do
        p = k * kk
        if p > limit then
            break
        end
        if chowla(p) == p - 1 then
            print(string.format("%10d is a number that is perfect", p))
            count = count + 1
        end
        k = kk + 1
        kk = kk + k
        i = i + 1
    end
    print(string.format("There are %d perfect numbers <= 35,000,000", count))
end

main()
```

```txt
chowla(1) = 0
chowla(2) = 0
chowla(3) = 0
chowla(4) = 2
chowla(5) = 0
chowla(6) = 5
chowla(7) = 0
chowla(8) = 6
chowla(9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0
Count of primes up to        100 = 25
Count of primes up to       1000 = 168
Count of primes up to      10000 = 1229
Count of primes up to     100000 = 9592
Count of primes up to    1000000 = 78498
Count of primes up to   10000000 = 664579
         6 is a number that is perfect
        28 is a number that is perfect
       496 is a number that is perfect
      8128 is a number that is perfect
  33550336 is a number that is perfect
There are 5 perfect numbers <= 35,000,000
```



## Maple


{{incorrect|Maple|

 The output for Chowla(1) is incorrect.

 }}


```Maple
ChowlaFunction := n -> NumberTheory:-SumOfDivisors(n) - n - 1;

PrintChowla := proc(n::posint) local i;
printf("Integer : Chowla Number\n");
for i to n do
  printf("%d  :  %d\n", i, ChowlaFunction(i));
end do;
end proc:

countPrimes := n -> nops([ListTools[SearchAll](0, map(ChowlaFunction, [seq(1 .. n)]))]);

findPerfect := proc(n::posint) local to_check, found, k;
to_check := map(ChowlaFunction, [seq(1 .. n)]);
found := [];
for k to n do
  if to_check(k) = k - 1 then
    found := [found, k];
  end if;
end do;
end proc:

PrintChowla(37);
countPrimes(100);
countPrimes(1000);
countPrimes(10000);
countPrimes(100000);
countPrimes(1000000);
countPrimes(10000000);
findPerfect(35000000)
```

```txt

Integer : Chowla Number
1  :  -1
2  :  0
3  :  0
4  :  2
5  :  0
6  :  5
7  :  0
8  :  6
9  :  3
10  :  7
11  :  0
12  :  15
13  :  0
14  :  9
15  :  8
16  :  14
17  :  0
18  :  20
19  :  0
20  :  21
21  :  10
22  :  13
23  :  0
24  :  35
25  :  5
26  :  15
27  :  12
28  :  27
29  :  0
30  :  41
31  :  0
32  :  30
33  :  14
34  :  19
35  :  12
36  :  54
37  :  0
25
168
1229
9592
78498
664579
[6, 28, 496, 8128, 33550336]
```



## Pascal

{{trans|Go}} but not using a sieve, cause a sieve doesn't need precalculated small primes.<BR>
So runtime is as bad as trial division.

```pascal
program Chowla;
{$IFDEF FPC}
   {$MODE Delphi}
{$ENDIF}
uses
  sysutils,strUtils{for Numb2USA};

function Chowla(n:NativeUint):NativeUint;
var
  Divisor,Quotient : NativeUint;
Begin
  result := 0;
  Divisor := 2;
  while sqr(Divisor)< n do
  Begin
    Quotient := n DIV Divisor;
    IF Quotient*Divisor = n then
      inc(result, Divisor+Quotient);
    inc(Divisor);
  end;
  IF sqr(Divisor) = n then
    inc(result,Divisor);
end;

procedure Count10Primes(Limit:NativeUInt);
var
  n,i,cnt : integer;
Begin
  writeln;
  writeln(' primes til |     count');
  i := 100;
  n:= 2;
  cnt := 0;
  repeat
    repeat
      // Ord (true) = 1 ,Ord (false) = 0
      inc(cnt,ORD(chowla(n) = 0));
      inc(n);
    until n > i;
    writeln(Numb2USA(IntToStr(i)):12,'|',Numb2USA(IntToStr(cnt)):10);
    i := i*10;
  until i > Limit;
end;

procedure CheckPerf;
var
  k,kk, p,cnt,limit : NativeInt;
Begin
  writeln;
  writeln(' number that is perfect');
  cnt := 0;
  limit := 35000000;
  k:= 2;
  kk:= 3;
  repeat
    p := k*kk;
    if p >limit then
      BREAK;
    if chowla(p) = (p - 1) then
    Begin
      writeln(Numb2USA(IntToStr(p)):12);
      inc(cnt);
    end;
    k := kk + 1;
    inc(kk,k);
  until false;
end;
var
  i : integer;
Begin
  For i := 2 to 37 do
    writeln('chowla(',i:2,') =',chowla(i):3);
  Count10Primes(10*1000*1000);
  CheckPerf;
end.
```

```txt

chowla( 2) =  0
chowla( 3) =  0
chowla( 4) =  2
chowla( 5) =  0
chowla( 6) =  5
chowla( 7) =  0
chowla( 8) =  6
chowla( 9) =  3
chowla(10) =  7
chowla(11) =  0
chowla(12) = 15
chowla(13) =  0
chowla(14) =  9
chowla(15) =  8
chowla(16) = 14
chowla(17) =  0
chowla(18) = 20
chowla(19) =  0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) =  0
chowla(24) = 35
chowla(25) =  5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) =  0
chowla(30) = 41
chowla(31) =  0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) =  0

 primes til |     count
         100|        25
       1,000|       168
      10,000|     1,229
     100,000|     9,592
   1,000,000|    78,498
  10,000,000|   664,579

 number that is perfect
           6
          28
         496
       8,128
  33,550,336
real  1m54,534s

```


## Perl

```perl
use strict;
use warnings;
use ntheory 'divisor_sum';

sub comma { reverse ((reverse shift) =~ s/(.{3})/$1,/gr) =~ s/^,//r }

sub chowla {
    my($n) = @_;
    $n < 2 ? 0 : divisor_sum($n) - ($n + 1);
}

sub prime_cnt {
    my($n) = @_;
    my $cnt = 1;
    for (3..$n) {
        $cnt++ if $_%2 and chowla($_) == 0
    }
    $cnt;
}

sub perfect {
    my($n) = @_;
    my @p;
    for my $i (1..$n) {
        push @p, $i if $i > 1 and chowla($i) == $i-1;
    }
    # map { push @p, $_ if $_ > 1 and chowla($_) == $_-1 } 1..$n; # speed penalty
    @p;
}

printf "chowla(%2d) = %2d\n", $_, chowla($_) for 1..37;
print "\nCount of primes up to:\n";
printf "%10s %s\n", comma(10**$_), comma(prime_cnt(10**$_)) for 2..7;
my @perfect = perfect(my $limit = 35_000_000);
printf "\nThere are %d perfect numbers up to %s: %s\n",
    1+$#perfect, comma($limit), join(' ', map { comma($_) } @perfect);
```

```txt
chowla( 1) =  0
chowla( 2) =  0
chowla( 3) =  0
chowla( 4) =  2
chowla( 5) =  0
chowla( 6) =  5
chowla( 7) =  0
chowla( 8) =  6
chowla( 9) =  3
chowla(10) =  7
chowla(11) =  0
chowla(12) = 15
chowla(13) =  0
chowla(14) =  9
chowla(15) =  8
chowla(16) = 14
chowla(17) =  0
chowla(18) = 20
chowla(19) =  0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) =  0
chowla(24) = 35
chowla(25) =  5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) =  0
chowla(30) = 41
chowla(31) =  0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) =  0

Count of primes up to:
       100 25
     1,000 168
    10,000 1,229
   100,000 9,592
 1,000,000 78,498
10,000,000 664,579

There are 5 perfect numbers up to 35,000,000: 6 28 496 8,128 33,550,336
```



## Perl 6

Much like in the [[Totient_function|Totient function]] task, we are using a thing poorly suited to finding prime numbers, to find large quantities of prime numbers.

(For a more reasonable test, reduce the orders-of-magnitude range in the "Primes count" line from 2..7 to 2..5)


```perl6
sub comma { $^i.flip.comb(3).join(',').flip }

sub schnitzel (\Radda, \radDA = 0) {
    Radda.is-prime ?? !Radda !! ?radDA ?? Radda
    !! sum flat (2 .. Radda.sqrt.floor).map: -> \RAdda {
        my \RADDA = Radda div RAdda;
        next if RADDA * RAdda !== Radda;
        RAdda !== RADDA ?? (RAdda, RADDA) !! RADDA
    }
}

my \chowder = cache (1..Inf).hyper(:8degree).grep( !*.&schnitzel: 'panini' );

my \mung-daal = lazy gather for chowder -> \panini {
    my \gazpacho = 2**panini - 1;
    take gazpacho * 2**(panini - 1) unless schnitzel gazpacho, panini;
}

printf "chowla(%2d) = %2d\n", $_, .&schnitzel for 1..37;

say '';

printf "Count of primes up to %10s: %s\n", comma(10**$_),
  comma chowder.first( * > 10**$_, :k) for 2..7;

say "\nPerfect numbers less than 35,000,000";

.&comma.say for mung-daal[^5];
```


```txt
chowla( 1) =  0
chowla( 2) =  0
chowla( 3) =  0
chowla( 4) =  2
chowla( 5) =  0
chowla( 6) =  5
chowla( 7) =  0
chowla( 8) =  6
chowla( 9) =  3
chowla(10) =  7
chowla(11) =  0
chowla(12) = 15
chowla(13) =  0
chowla(14) =  9
chowla(15) =  8
chowla(16) = 14
chowla(17) =  0
chowla(18) = 20
chowla(19) =  0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) =  0
chowla(24) = 35
chowla(25) =  5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) =  0
chowla(30) = 41
chowla(31) =  0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) =  0

Count of primes up to        100: 25
Count of primes up to      1,000: 168
Count of primes up to     10,000: 1,229
Count of primes up to    100,000: 9,592
Count of primes up to  1,000,000: 78,498
Count of primes up to 10,000,000: 664,579

Perfect numbers less than 35,000,000
6
28
496
8,128
33,550,336

```



## Phix


```Phix
function chowla(atom n)
    return sum(factors(n))
end function

function sieve(integer limit)
    -- True denotes composite, false denotes prime.
    -- Only interested in odd numbers >= 3
    sequence c = repeat(false,limit)
    for i=3 to floor(limit/3) by 2 do
--      if not c[i] and chowla(i)==0 then
        if not c[i] then -- (see note below)
            for j=3*i to limit by 2*i do
                c[j] = true
            end for
        end if
    end for
    return c
end function

atom limit = 1e7, count = 1, pow10 = 100, t0 = time()
sequence s = {}
for i=1 to 37 do
    s &= chowla(i)
end for
printf(1,"chowla[1..37]: %v\n",{s})
s = sieve(limit)
for i=3 to limit by 2 do
    if not s[i] then count += 1 end if
    if i==pow10-1 then
        printf(1,"Count of primes up to %,d = %,d\n", {pow10, count})
        pow10 *= 10
    end if
end for

count = 0
limit = iff(machine_bits()=32?1.4e11:2.4e18)
--limit = power(2,iff(machine_bits()=32?53:64)) -- (see note below)
integer i=2
while true do
    atom p = power(2,i-1)*(power(2,i)-1) -- perfect numbers must be of this form
    if p>limit then exit end if
    if chowla(p)==p-1 then
        printf(1,"%,d is a perfect number\n", p)
        count += 1
    end if
    i += 1
end while
printf(1,"There are %d perfect numbers <= %,d\n",{count,limit})
?elapsed(time()-t0)
```

The use of chowla() in sieve() does not actually achieve anything other than slow it down, so I took it out.
```txt

chowla[1..37]: {0,0,0,2,0,5,0,6,3,7,0,15,0,9,8,14,0,20,0,21,10,13,0,35,5,15,12,27,0,41,0,30,14,19,12,54,0}
Count of primes up to 100 = 25
Count of primes up to 1,000 = 168
Count of primes up to 10,000 = 1,229
Count of primes up to 100,000 = 9,592
Count of primes up to 1,000,000 = 78,498
Count of primes up to 10,000,000 = 664,579
6 is a perfect number
28 is a perfect number
496 is a perfect number
8,128 is a perfect number
33,550,336 is a perfect number
8,589,869,056 is a perfect number
137,438,691,328 is a perfect number
2,305,843,008,139,952,128 is a perfect number
There are 8 perfect numbers <= 9,223,372,036,854,775,808

```

Note that 32-bit only finds the first 7 perfect numbers, but does so in 0.4s, whereas 64-bit
takes just under 45s to find the 8th one. Using the theoretical (power 2) limits, those times
become 4s and 90s respectively, without finding anything else. Obviously 1.4e11 and 2.4e18
were picked to minimise the run times.


## PicoLisp


```PicoLisp
(de accu1 (Var Key)
   (if (assoc Key (val Var))
      (con @ (inc (cdr @)))
      (push Var (cons Key 1)) )
   Key )
(de factor (N)
   (let
      (R NIL
         D 2
         L (1 2 2 . (4 2 4 2 4 6 2 6 .))
         M (sqrt N) )
      (while (>= M D)
         (if (=0 (% N D))
            (setq M
               (sqrt (setq N (/ N (accu1 'R D)))) )
            (inc 'D (pop 'L)) ) )
      (accu1 'R N)
      (mapcar
         '((L)
            (make
               (for N (cdr L)
                  (link (** (car L) N)) ) ) )
         R ) ) )
(de chowla (N)
   (let F (factor N)
      (-
         (sum
            prog
            (make
               (link 1)
               (mapc
                  '((A)
                     (chain
                        (mapcan
                           '((B)
                              (mapcar '((C) (* C B)) (made)) )
                           A ) ) )
                  F ) ) )
         N
         1 ) ) )
(de prime (N)
   (and (> N 1) (=0 (chowla N))) )
(de perfect (N)
   (and
      (> N 1)
      (= (chowla N) (dec N))) )
(de countP (N)
   (let C 0
      (for I N
         (and (prime I) (inc 'C)) )
      C ) )
(de listP (N)
   (make
      (for I N
         (and (perfect I) (link I)) ) ) )
(for I 37
   (prinl "chowla(" I ") = " (chowla I)) )
(prinl "Count of primes up to      100 = " (countP 100))
(prinl "Count of primes up to     1000 = " (countP 1000))
(prinl "Count of primes up to    10000 = " (countP 10000))
(prinl "Count of primes up to   100000 = " (countP 100000))
(prinl "Count of primes up to  1000000 = " (countP 1000000))
(prinl "Count of primes up to 10000000 = " (countP 10000000))
(println (listP 35000000))
```


```txt

chowla(1) = 0
chowla(2) = 0
chowla(3) = 0
chowla(4) = 2
chowla(5) = 0
chowla(6) = 5
chowla(7) = 0
chowla(8) = 6
chowla(9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0
Count of primes up to      100 = 25
Count of primes up to     1000 = 168
Count of primes up to    10000 = 1229
Count of primes up to   100000 = 9592
Count of primes up to  1000000 = 78498
Count of primes up to 10000000 = 664579
(6 28 496 8128 33550336)

```



## PowerBASIC


{{incorrect|PowerBASIC|

 The 8<sup>th</sup> perfect number is off by '''2'''   (it is too high),
 it should end in   ... 952,128}}

```powerbasic
#COMPILE EXE
#DIM ALL
#COMPILER PBCC 6

FUNCTION chowla(BYVAL n AS LONG) AS LONG
REGISTER i AS LONG, j AS LONG
LOCAL r AS LONG
    i = 2
    DO WHILE i * i <= n
        j = n \ i
        IF n MOD i = 0 THEN
            r += i
            IF i <> j THEN
                r += j
            END IF
        END IF
        INCR i
    LOOP
    FUNCTION = r
END FUNCTION

FUNCTION chowla1(BYVAL n AS QUAD) AS QUAD
LOCAL i, j, r AS QUAD
    i = 2
    DO WHILE i * i <= n
        j = n \ i
        IF n MOD i = 0 THEN
            r += i
            IF i <> j THEN
                r += j
            END IF
        END IF
        INCR i
    LOOP
    FUNCTION = r
END FUNCTION

SUB sieve(BYVAL limit AS LONG, BYREF c() AS INTEGER)
LOCAL i, j AS LONG
REDIM c(limit - 1)
    i = 3
    DO WHILE i * 3 < limit
        IF NOT c(i) THEN
            IF chowla(i) = 0 THEN
                j = 3 * i
                DO WHILE j < limit
                    c(j) = -1
                    j += 2 * i
                LOOP
            END IF
        END IF
        i += 2
    LOOP
END SUB

FUNCTION PBMAIN () AS LONG
LOCAL i, count, limit, power AS LONG
LOCAL c() AS INTEGER
LOCAL s AS STRING
LOCAL s30 AS STRING * 30
LOCAL p, k, kk, r, ql AS QUAD
    FOR i = 1 TO 37
        s = "chowla(" & TRIM$(STR$(i)) & ") = " & TRIM$(STR$(chowla(i)))
        CON.PRINT s
    NEXT i
    count = 1
    limit = 10000000
    power = 100
    CALL sieve(limit, c())
    FOR i = 3 TO limit - 1 STEP 2
        IF ISFALSE c(i) THEN count += 1
        IF i = power - 1 THEN
            RSET s30 = FORMAT$(power, "#,##0")
            s = "Count of primes up to " & s30 & " =" & STR$(count)
            CON.PRINT s
            power *= 10
        END IF
    NEXT i

    ql = 2 ^ 61
    k = 2: kk = 3
    RESET count
    DO
        p = k * kk : IF p > ql THEN EXIT DO
        IF chowla1(p) = p - 1 THEN
            RSET s30 = FORMAT$(p, "#,##0")
            s = s30 & " is a number that is perfect"
            CON.PRINT s
            count += 1
        END IF
        k = kk + 1 : kk += k
    LOOP
    s = "There are" & STR$(count) & " perfect numbers <= " & FORMAT$(ql, "#,##0")
    CON.PRINT s

    CON.PRINT "press any key to exit program"
    CON.WAITKEY$
END FUNCTION
```

```txt
chowla(1) = 0
chowla(2) = 0
chowla(3) = 0
chowla(4) = 2
chowla(5) = 0
chowla(6) = 5
chowla(7) = 0
chowla(8) = 6
chowla(9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0
Count of primes up to                            100 = 25
Count of primes up to                          1,000 = 168
Count of primes up to                         10,000 = 1229
Count of primes up to                        100,000 = 9592
Count of primes up to                      1,000,000 = 78498
Count of primes up to                     10,000,000 = 664579
                             6 is a number that is perfect
                            28 is a number that is perfect
                           496 is a number that is perfect
                         8,128 is a number that is perfect
                    33,550,336 is a number that is perfect
                 8,589,869,056 is a number that is perfect
               137,438,691,328 is a number that is perfect
     2,305,843,008,139,952,130 is a number that is perfect
There are 8 perfect numbers <= 2,305,843,009,213,693,950
press any key to exit program
```



## Python

Uses [https://www.python.org/dev/peps/pep-0515/ underscores to separate digits] in numbers, and th [https://www.sympy.org/en/index.htm sympy library] to aid calculations.


```python
# https://docs.sympy.org/latest/modules/ntheory.html#sympy.ntheory.factor_.divisors
from sympy import divisors

def chowla(n):
    return 0 if n < 2 else sum(divisors(n, generator=True)) - 1 -n

def is_prime(n):
    return chowla(n) == 0

def primes_to(n):
    return sum(chowla(i) == 0 for i in range(2, n))

def perfect_between(n, m):
    c = 0
    print(f"\nPerfect numbers between [{n:_}, {m:_})")
    for i in range(n, m):
        if i > 1 and chowla(i) == i - 1:
            print(f"  {i:_}")
            c += 1
    print(f"Found {c} Perfect numbers between [{n:_}, {m:_})")


if __name__ == '__main__':
    for i in range(1, 38):
        print(f"chowla({i:2}) == {chowla(i)}")
    for i in range(2, 6):
        print(f"primes_to({10**i:_}) == {primes_to(10**i):_}")
    perfect_between(1, 1_000_000)
    print()
    for i in range(6, 8):
        print(f"primes_to({10**i:_}) == {primes_to(10**i):_}")
    perfect_between(1_000_000, 35_000_000)
```


```txt
chowla( 1) == 0
chowla( 2) == 0
chowla( 3) == 0
chowla( 4) == 2
chowla( 5) == 0
chowla( 6) == 5
chowla( 7) == 0
chowla( 8) == 6
chowla( 9) == 3
chowla(10) == 7
chowla(11) == 0
chowla(12) == 15
chowla(13) == 0
chowla(14) == 9
chowla(15) == 8
chowla(16) == 14
chowla(17) == 0
chowla(18) == 20
chowla(19) == 0
chowla(20) == 21
chowla(21) == 10
chowla(22) == 13
chowla(23) == 0
chowla(24) == 35
chowla(25) == 5
chowla(26) == 15
chowla(27) == 12
chowla(28) == 27
chowla(29) == 0
chowla(30) == 41
chowla(31) == 0
chowla(32) == 30
chowla(33) == 14
chowla(34) == 19
chowla(35) == 12
chowla(36) == 54
chowla(37) == 0
primes_to(100) == 25
primes_to(1_000) == 168
primes_to(10_000) == 1_229
primes_to(100_000) == 9_592

Perfect numbers between [1, 1_000_000)
  6
  28
  496
  8_128
Found 4 Perfect numbers between [1, 1_000_000)

primes_to(1_000_000) == 78_498
primes_to(10_000_000) == 664_579

Perfect numbers between [1_000_000, 35_000_000)
  33_550_336
Found 1 Perfect numbers between [1_000_000, 35_000_000)
```



### Python: Numba

(Elementary) use of the [http://numba.pydata.org/ numba] library needs
* library install and import
*use of `@jit` decorator on some functions
* Rewrite to remove use of `sum()`
* Splitting one function for the jit compiler to digest.


```python
from numba import jit

# https://docs.sympy.org/latest/modules/ntheory.html#sympy.ntheory.factor_.divisors
from sympy import divisors

@jit
def chowla(n):
    return 0 if n < 2 else sum(divisors(n, generator=True)) - 1 -n

@jit
def is_prime(n):
    return chowla(n) == 0

@jit
def primes_to(n):
    acc = 0
    for i in range(2, n):
        if chowla(i) == 0:
            acc += 1
    return acc

@jit
def _perfect_between(n, m):
    for i in range(n, m):
        if i > 1 and chowla(i) == i - 1:
            yield i

def perfect_between(n, m):
    c = 0
    print(f"\nPerfect numbers between [{n:_}, {m:_})")
    for i in _perfect_between(n, m):
        print(f"  {i:_}")
        c += 1
    print(f"Found {c} Perfect numbers between [{n:_}, {m:_})")
```


Same as above for use of same __main__ block.

Speedup - not much, subjectively...


## REXX


```rexx
/*REXX program computes/displays chowla numbers (and may count primes & perfect numbers.*/
parse arg LO HI .                                /*obtain optional arguments from the CL*/
if LO=='' | LO==","  then LO=  1                 /*Not specified?  Then use the default.*/
perf= LO<0;               LO= abs(LO)            /*Negative?  Then determine if perfect.*/
if HI=='' | HI==","  then HI= LO                 /*Not specified?  Then use the default.*/
prim= HI<0;               HI= abs(HI)            /*Negative?  Then determine if a prime.*/
numeric digits max(9, length(HI) + 1 )           /*use enough decimal digits for   //   */
w= length( commas(HI) )                          /*W:   used in aligning output numbers.*/
tell= \(prim | perf)                             /*set boolean value for showing chowlas*/
p= 0                                             /*the number of primes found  (so far).*/
     do j=LO  to HI;       #= chowla(j)          /*compute the  cholwa number  for  J.  */
     if tell  then say right('chowla('commas(j)")", w+9)    ' = '    right( commas(#), w)
              else if #==0  then if j>1  then p= p+1
     if perf  then if j-1==# & j>1  then say right(commas(j), w)   ' is a perfect number.'
     end   /*j*/

if prim & \perf  then say 'number of primes found for the range '   commas(LO)    " to " ,
                           commas(HI)        " (inclusive)  is: "   commas(p)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
chowla: procedure; parse arg x;         if x<2  then return 0;          odd= x // 2
        s=0                                      /* [↓]  use EVEN or ODD integers.   ___*/
            do k=2+odd  by 1+odd  while k*k<x    /*divide by all the integers up to √ X */
            if x//k==0  then  s=s + k + x%k      /*add the two divisors to the sum.     */
            end   /*k*/                          /* [↓]  adkust for square.          ___*/
        if k*k==x  then  s=s + k                 /*Was  X  a square?    If so, add  √ X */
        return s                                 /*return     "     "    "      "     " */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;  do k=length(_)-3  to 1  by -3; _= insert(',', _, k); end;   return _
```

```txt

  chowla(1)  =   0
  chowla(2)  =   0
  chowla(3)  =   0
  chowla(4)  =   2
  chowla(5)  =   0
  chowla(6)  =   5
  chowla(7)  =   0
  chowla(8)  =   6
  chowla(9)  =   3
 chowla(10)  =   7
 chowla(11)  =   0
 chowla(12)  =  15
 chowla(13)  =   0
 chowla(14)  =   9
 chowla(15)  =   8
 chowla(16)  =  14
 chowla(17)  =   0
 chowla(18)  =  20
 chowla(19)  =   0
 chowla(20)  =  21
 chowla(21)  =  10
 chowla(22)  =  13
 chowla(23)  =   0
 chowla(24)  =  35
 chowla(25)  =   5
 chowla(26)  =  15
 chowla(27)  =  12
 chowla(28)  =  27
 chowla(29)  =   0
 chowla(30)  =  41
 chowla(31)  =   0
 chowla(32)  =  30
 chowla(33)  =  14
 chowla(34)  =  19
 chowla(35)  =  12
 chowla(36)  =  54
 chowla(37)  =   0

```

```txt

number of primes found for the range  1  to  100  (inclusive)  is:  25

```

```txt

number of primes found for the range  1  to  1,000  (inclusive)  is:  168

```

```txt

number of primes found for the range  1  to  10,000  (inclusive)  is:  1,229

```

```txt

number of primes found for the range  1  to  100,000  (inclusive)  is:  9,592

```

```txt

number of primes found for the range  1  to  1,000,000  (inclusive)  is:  78.498

```

```txt

number of primes found for the range  1  to  10,000,000  (inclusive)  is:  664,579

```

```txt

number of primes found for the range  1  to  100,000,000  (inclusive)  is:  5,761,455

```

```txt

         6  is a perfect number.
        28  is a perfect number.
       496  is a perfect number.
     8,128  is a perfect number.
33,550,336  is a perfect number.

```



## Scala

This solution uses a lazily-evaluated iterator to find and sum the divisors of a number, and speeds up the large searches using parallel vectors.

```scala
object ChowlaNumbers {
  def main(args: Array[String]): Unit = {
    println("Chowla Numbers...")
    for(n <- 1 to 37){println(s"$n: ${chowlaNum(n)}")}
    println("\nPrime Counts...")
    for(i <- (2 to 7).map(math.pow(10, _).toInt)){println(f"$i%,d: ${primesPar(i).size}%,d")}
    println("\nPerfect Numbers...")
    print(perfectsPar(35000000).toVector.sorted.zipWithIndex.map{case (n, i) => f"${i + 1}%,d: $n%,d"}.mkString("\n"))
  }

  def primesPar(num: Int): ParVector[Int] = ParVector.range(2, num + 1).filter(n => chowlaNum(n) == 0)
  def perfectsPar(num: Int): ParVector[Int] = ParVector.range(6, num + 1).filter(n => chowlaNum(n) + 1 == n)

  def chowlaNum(num: Int): Int = Iterator.range(2, math.sqrt(num).toInt + 1).filter(n => num%n == 0).foldLeft(0){case (s, n) => if(n*n == num) s + n else s + n + (num/n)}
}
```


```txt
Chowla Numbers...
1: 0
2: 0
3: 0
4: 2
5: 0
6: 5
7: 0
8: 6
9: 3
10: 7
11: 0
12: 15
13: 0
14: 9
15: 8
16: 14
17: 0
18: 20
19: 0
20: 21
21: 10
22: 13
23: 0
24: 35
25: 5
26: 15
27: 12
28: 27
29: 0
30: 41
31: 0
32: 30
33: 14
34: 19
35: 12
36: 54
37: 0

Prime Counts...
100: 25
1,000: 168
10,000: 1,229
100,000: 9,592
1,000,000: 78,498
10,000,000: 664,579

Perfect Numbers...
1: 6
2: 28
3: 496
4: 8,128
5: 33,550,336
```



## Visual Basic

```vb
Option Explicit

Private Declare Function AllocConsole Lib "kernel32.dll" () As Long
Private Declare Function FreeConsole Lib "kernel32.dll" () As Long
Dim mStdOut As Scripting.TextStream

Function chowla(ByVal n As Long) As Long
Dim j As Long, i As Long
  i = 2
  Do While i * i <= n
    j = n \ i
    If n Mod i = 0 Then
    chowla = chowla + i
      If i <> j Then
      chowla = chowla + j
      End If
    End If
    i = i + 1
  Loop
End Function

Function sieve(ByVal limit As Long) As Boolean()
Dim c() As Boolean
Dim i As Long
Dim j As Long
  i = 3
  ReDim c(limit - 1)
    Do While i * 3 < limit
      If Not c(i) Then
        If (chowla(i) = 0) Then
          j = 3 * i
          Do While j < limit
            c(j) = True
            j = j + 2 * i
          Loop
        End If
    End If
    i = i + 2
    Loop
  sieve = c()
End Function

Sub Display(ByVal s As String)
  Debug.Print s
  mStdOut.Write s & vbNewLine
End Sub

Sub Main()
Dim i As Long
Dim count As Long
Dim limit As Long
Dim power As Long
Dim c() As Boolean
Dim p As Long
Dim k As Long
Dim kk As Long
Dim s As String * 30
Dim mFSO As Scripting.FileSystemObject
Dim mStdIn As Scripting.TextStream

  AllocConsole
  Set mFSO = New Scripting.FileSystemObject
  Set mStdIn = mFSO.GetStandardStream(StdIn)
  Set mStdOut = mFSO.GetStandardStream(StdOut)

  For i = 1 To 37
    Display "chowla(" & i & ")=" & chowla(i)
  Next i

  count = 1
  limit = 10000000
  power = 100
  c = sieve(limit)

  For i = 3 To limit - 1 Step 2
    If Not c(i) Then
      count = count + 1
    End If
    If i = power - 1 Then
      RSet s = FormatNumber(power, 0, vbUseDefault, vbUseDefault, True)
      Display "Count of primes up to " & s & " = " & FormatNumber(count, 0, vbUseDefault, vbUseDefault, True)
      power = power * 10
    End If
  Next i

  count = 0: limit = 35000000
  k = 2:     kk = 3

  Do
    p = k * kk
    If p > limit Then
      Exit Do
    End If

    If chowla(p) = p - 1 Then
      RSet s = FormatNumber(p, 0, vbUseDefault, vbUseDefault, True)
      Display s & " is a number that is perfect"
      count = count + 1
    End If
    k = kk + 1
    kk = kk + k
  Loop

  Display "There are " & CStr(count) & " perfect numbers <= 35.000.000"

  mStdOut.Write "press enter to quit program."
  mStdIn.Read 1

  FreeConsole

End Sub
```

```txt
chowla(1)=0
chowla(2)=0
chowla(3)=0
chowla(4)=2
chowla(5)=0
chowla(6)=5
chowla(7)=0
chowla(8)=6
chowla(9)=3
chowla(10)=7
chowla(11)=0
chowla(12)=15
chowla(13)=0
chowla(14)=9
chowla(15)=8
chowla(16)=14
chowla(17)=0
chowla(18)=20
chowla(19)=0
chowla(20)=21
chowla(21)=10
chowla(22)=13
chowla(23)=0
chowla(24)=35
chowla(25)=5
chowla(26)=15
chowla(27)=12
chowla(28)=27
chowla(29)=0
chowla(30)=41
chowla(31)=0
chowla(32)=30
chowla(33)=14
chowla(34)=19
chowla(35)=12
chowla(36)=54
chowla(37)=0
Count of primes up to                            100 = 25
Count of primes up to                          1.000 = 168
Count of primes up to                         10.000 = 1.229
Count of primes up to                        100.000 = 9.592
Count of primes up to                      1.000.000 = 78.498
Count of primes up to                     10.000.000 = 664.579
                             6 is a number that is perfect
                            28 is a number that is perfect
                           496 is a number that is perfect
                         8.128 is a number that is perfect
                    33.550.336 is a number that is perfect
There are 5 perfect numbers <= 35.000.000
press enter to quit program.
```



## Visual Basic .NET

```vbnet
Imports System

Module Program
    Function chowla(ByVal n As Integer) As Integer
        chowla = 0 : Dim j As Integer, i As Integer = 2
        While i * i <= n
            j = n / i : If n Mod i = 0 Then chowla += i + (If(i = j, 0, j))
            i += 1
        End While
    End Function

    Function sieve(ByVal limit As Integer) As Boolean()
        Dim c As Boolean() = New Boolean(limit - 1) {}, i As Integer = 3
        While i * 3 < limit
            If Not c(i) AndAlso (chowla(i) = 0) Then
                Dim j As Integer = 3 * i
                While j < limit : c(j) = True : j += 2 * i : End While
            End If : i += 2
        End While
        Return c
    End Function

    Sub Main(args As String())
        For i As Integer = 1 To 37
            Console.WriteLine("chowla({0}) = {1}", i, chowla(i))
        Next
        Dim count As Integer = 1, limit As Integer = CInt((10000000.0)), power As Integer = 100,
            c As Boolean() = sieve(limit)
        For i As Integer = 3 To limit - 1 Step 2
            If Not c(i) Then count += 1
            If i = power - 1 Then
                Console.WriteLine("Count of primes up to {0,10:n0} = {1:n0}", power, count)
                power = power * 10
            End If
        Next
        count = 0 : limit = 35000000
        Dim p As Integer, k As Integer = 2, kk As Integer = 3
        While True
            p = k * kk : If p > limit Then Exit While
            If chowla(p) = p - 1 Then
                Console.WriteLine("{0,10:n0} is a number that is perfect", p)
                count += 1
            End If
            k = kk + 1 : kk += k
        End While
        Console.WriteLine("There are {0} perfect numbers <= 35,000,000", count)
        If System.Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub
End Module
```

```txt
chowla(1) = 0
chowla(2) = 0
chowla(3) = 0
chowla(4) = 2
chowla(5) = 0
chowla(6) = 5
chowla(7) = 0
chowla(8) = 6
chowla(9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0
Count of primes up to        100 = 25
Count of primes up to      1,000 = 168
Count of primes up to     10,000 = 1,229
Count of primes up to    100,000 = 9,592
Count of primes up to  1,000,000 = 78,498
Count of primes up to 10,000,000 = 664,579
         6 is a number that is perfect
        28 is a number that is perfect
       496 is a number that is perfect
     8,128 is a number that is perfect
33,550,336 is a number that is perfect
There are 5 perfect numbers <= 35,000,000

```


### More Cowbell

{{libheader|System.Numerics}}One can get a little further, but that 8th perfect number takes nearly a minute to verify. The 9th takes longer than I have patience.  If you care to see the 9th and 10th perfect numbers, change the 31 to 61 or 89 where indicated by the comment.

```vbnet
Imports System.Numerics

Module Program
    Function chowla(n As Integer) As Integer
        chowla = 0 : Dim j As Integer, i As Integer = 2
        While i * i <= n
            If n Mod i = 0 Then j = n / i : chowla += i : If i <> j Then chowla += j
            i += 1
        End While
    End Function

    Function chowla1(ByRef n As BigInteger, x As Integer) As BigInteger
        chowla1 = 1 : Dim j As BigInteger, lim As BigInteger = BigInteger.Pow(2, x - 1)
        For i As BigInteger = 2 To lim
            If n Mod i = 0 Then j = n / i : chowla1 += i : If i <> j Then chowla1 += j
        Next
    End Function

    Function sieve(ByVal limit As Integer) As Boolean()
        Dim c As Boolean() = New Boolean(limit - 1) {}, i As Integer = 3
        While i * 3 < limit
            If Not c(i) AndAlso (chowla(i) = 0) Then
                Dim j As Integer = 3 * i
                While j < limit : c(j) = True : j += 2 * i : End While
            End If : i += 2
        End While
        Return c
    End Function

    Sub Main(args As String())
        For i As Integer = 1 To 37
            Console.WriteLine("chowla({0}) = {1}", i, chowla(i))
        Next
        Dim count As Integer = 1, limit As Integer = CInt((10000000.0)), power As Integer = 100,
            c As Boolean() = sieve(limit)
        For i As Integer = 3 To limit - 1 Step 2
            If Not c(i) Then count += 1
            If i = power - 1 Then
                Console.WriteLine("Count of primes up to {0,10:n0} = {1:n0}", power, count)
                power = power * 10
            End If
        Next
        count = 0
        Dim p As BigInteger, k As BigInteger = 2, kk As BigInteger = 3
        For i As Integer = 2 To 31 ' if you dare, change the 31 to 61 or 89
            If {2, 3, 5, 7, 13, 17, 19, 31, 61, 89}.Contains(i) Then
                p = k * kk
                If chowla1(p, i) = p Then
                    Console.WriteLine("{0,25:n0} is a number that is perfect", p)
                    st = DateTime.Now
                    count += 1
                End If
            End If
            k = kk + 1 : kk += k
        Next
        Console.WriteLine("There are {0} perfect numbers <= {1:n0}", count, 25 * BigInteger.Pow(10, 18))
        If System.Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub
End Module
```

```txt
chowla(1) = 0
chowla(2) = 0
chowla(3) = 0
chowla(4) = 2
chowla(5) = 0
chowla(6) = 5
chowla(7) = 0
chowla(8) = 6
chowla(9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0
Count of primes up to        100 = 25
Count of primes up to      1,000 = 168
Count of primes up to     10,000 = 1,229
Count of primes up to    100,000 = 9,592
Count of primes up to  1,000,000 = 78,498
Count of primes up to 10,000,000 = 664,579
                        6 is a number that is perfect
                       28 is a number that is perfect
                      496 is a number that is perfect
                    8,128 is a number that is perfect
               33,550,336 is a number that is perfect
            8,589,869,056 is a number that is perfect
          137,438,691,328 is a number that is perfect
2,305,843,008,139,952,128 is a number that is perfect
There are 8 perfect numbers <= 25,000,000,000,000,000,000
```



## zkl

```zkl
fcn chowla(n){
   if(n<1)
      throw(Exception.ValueError("Chowla function argument must be positive"));
   sum:=0;
   foreach i in ([2..n.toFloat().sqrt()]){
      if(n%i == 0){
	 j:=n/i;
	 if(i==j) sum+=i;
	 else     sum+=i+j;
      }
   }
   sum
}

fcn chowlaSieve(limit){
    // True denotes composite, false denotes prime.
    // Only interested in odd numbers >= 3
   c:=Data(limit+100).fill(0); # slop at the end (for reverse wrap around)
   foreach i in ([3..limit/3,2]){
      if(not c[i] and chowla(i)==0)
         { foreach j in ([3*i..limit,2*i]){ c[j]=True } }
   }
   c
}
```


```zkl
fcn testChowla{
   println("The first 37 Chowla numbers:\n",
      [1..37].apply(chowla).concat(" ","[","]"), "\n");

   count,limit,power := 1, (1e7).toInt(), 100;
   c:=chowlaSieve(limit);
   foreach i in ([3..limit-1,2]){
      if(not c[i]) count+=1;
      if(i == power - 1){
	 println("The count of the primes up to %10,d is %8,d".fmt(power,count));
	 power*=10;
      }
   }

   println();
   count, limit = 0, 35_000_000;
   foreach i in ([2..]){
      p:=(1).shiftLeft(i - 1) * ((1).shiftLeft(i)-1); // perfect numbers must be of this form
      if(p>limit) break;
      if(p-1 == chowla(p)){
         println("%,d is a perfect number".fmt(p));
	 count+=1;
      }
   }
   println("There are %,d perfect numbers <= %,d".fmt(count,limit));
}();
```


```txt

The first 37 Chowla numbers:
[0 0 0 2 0 5 0 6 3 7 0 15 0 9 8 14 0 20 0 21 10 13 0 35 5 15 12 27 0 41 0 30 14 19 12 54 0]

The count of the primes up to        100 is       25
The count of the primes up to      1,000 is      168
The count of the primes up to     10,000 is    1,229
The count of the primes up to    100,000 is    9,592
The count of the primes up to  1,000,000 is   78,498
The count of the primes up to 10,000,000 is  664,579

6 is a perfect number
28 is a perfect number
496 is a perfect number
8,128 is a perfect number
33,550,336 is a perfect number
There are 5 perfect numbers <= 35,000,000

```

