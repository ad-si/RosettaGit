+++
title = "Emirp primes"
description = ""
date = 2019-10-09T09:24:31Z
aliases = []
[extra]
id = 17444
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}

An   ''emirp''   ('''prime''' spelled backwards)   are primes that when reversed   (in their decimal representation)   are a different prime.

(This rules out palindromic primes.)


;Task:
::*   show the first   twenty   emirps
::*   show all emirps between   7,700   and   8,000
::*   show the   10,000<sup>th</sup>   emirp


In each list, the numbers should be in order.

Invoke the (same) program once per task requirement, this will show what limit is used as the upper bound for calculating surplus (regular) primes.

The specific method of how to determine if a range or if specific values are to be shown will be left to the programmer.


;See also:
*   [https://en.wikipedia.org/wiki/Emirp Wikipedia, Emirp].
*   [http://primes.utm.edu/glossary/xpage/emirp.html The Prime Pages, emirp].
*   [http://mathworld.wolfram.com/Emirp.html Wolfram MathWorld&trade;, Emirp].
*   [https://oeis.org/A006567 The On‑Line Encyclopedia of Integer Sequences, emirps (A6567)].





## Ada


he solution uses the package Miller_Rabin from the [[Miller-Rabin primality test]].


```Ada
with Ada.Text_IO, Miller_Rabin;

procedure Emirp_Gen is

   type Num is range 0 .. 2**63-1; -- maximum for the gnat Ada compiler

   MR_Iterations: constant Positive := 25;
     -- the probability Pr[Is_Prime(N, MR_Iterations) = Probably_Prime]
     -- is 1 for prime N and < 4**(-MR_Iterations) for composed N

   function Is_Emirp(E: Num) return Boolean is
      package MR is new Miller_Rabin(Num); use MR;

      function Rev(E: Num) return Num is
	 N: Num := E;
	 R: Num := 0;
      begin
	 while N > 0 loop
	    R := 10*R + N mod 10; -- N mod 10 is least significant digit of N
	    N := N / 10;          -- delete least significant digit of N
	 end loop;
	 return R;
      end Rev;

      R: Num := Rev(E);
   begin
      return E /= R and then
	     (Is_Prime(E, MR_Iterations) = Probably_Prime) and then
	     (Is_Prime(R, MR_Iterations) = Probably_Prime);
   end Is_Emirp;

   function Next(P: Num) return Num is
      N: Num := P+1;
   begin
      while not (Is_Emirp(N)) Loop
	 N := N + 1;
      end loop;
      return N;
   end Next;

   Current: Num;
   Count: Num := 0;

begin
   -- show the first twenty emirps
   Ada.Text_IO.Put("First 20 emirps:");
   Current := 1;
   for I in 1 .. 20 loop
      Current := Next(Current);
      Ada.Text_IO.Put(Num'Image(Current));
   end loop;
   Ada.Text_IO.New_Line;

   -- show the emirps between 7700 and 8000
   Ada.Text_IO.Put("Emirps between 7700 and 8000:");
   Current := 7699;
   loop
      Current := Next(Current);
      exit when Current > 8000;
       Ada.Text_IO.Put(Num'Image(Current));
   end loop;

   -- the 10_000th emirp
   Ada.Text_IO.Put("The 10_000'th emirp:");
   for I in 1 .. 10_000 loop
      Current := Next(Current);
   end loop;
   Ada.Text_IO.Put_Line(Num'Image(Current));
end Emirp_Gen;
```


{{out}}

```txt
First 20 emirps: 13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
Emirps between 7700 and 8000: 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
The 10_000'th emirp: 948349
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}
Uses Algol 68G specific argc and argv procedures to access to command line.
Allows the user to specify the from and to range values or ordinals on the command line. The sieve size can also be specified. As suggested by the Fortran sample, from = to is treated as a special case for labeling the output.

```algol68
# sieve of Eratosthenes: sets s[i] to TRUE if i is prime, FALSE otherwise     #
PROC sieve = ( REF[]BOOL s )VOID:
     BEGIN
        # start with everything flagged as prime                              #
        FOR i TO UPB s DO s[ i ] := TRUE OD;
        # sieve out the non-primes                                            #
        s[ 1 ] := FALSE;
        FOR i FROM 2 TO ENTIER sqrt( UPB s ) DO
            IF s[ i ] THEN FOR p FROM i * i BY i TO UPB s DO s[ p ] := FALSE OD FI
        OD
     END # sieve # ;

# parse the command line - ignore errors                                      #
INT  emirp from  :=  1;        # lowest emirp required                        #
INT  emirp to    := 10;        # highest emirp required                       #
BOOL value range := FALSE;     # TRUE if the range is the value of the emirps #
                               # FALSE if the range is the ordinal of the     #
                               # emirps                                       #
INT  max number  := 1 000 000; # sieve size                                   #
# returns s converted to an integer - does not check s is a valid integer     #
PROC to int = ( STRING s )INT:
     BEGIN
        INT result := 0;
        FOR ch pos FROM LWB s TO UPB s DO
            result *:= 10;
            result +:= ABS s[ ch pos ] - ABS "0"
        OD;
        result
     END # to int # ;
FOR arg pos TO argc DO
    IF   argv( arg pos ) = "FROM"    THEN
        emirp from  := to int( argv( arg pos + 1 ) )
    ELIF argv( arg pos ) = "TO"      THEN
        emirp to    := to int( argv( arg pos + 1 ) )
    ELIF argv( arg pos ) = "VALUE"   THEN
        value range := TRUE
    ELIF argv( arg pos ) = "ORDINAL" THEN
        value range := FALSE
    ELIF argv( arg pos ) = "SIEVE"   THEN
        max number  := to int( argv( arg pos + 1 ) )
    FI
OD;

# construct a sieve of primes up to the maximum number required for the task  #
[ 1 : max number ]BOOL is prime;
sieve( is prime );

# return TRUE if p is an emirp, FALSE otherwise                               #
PROC is emirp = ( INT p )BOOL:
     IF NOT is prime[ p ] THEN
        FALSE
     ELSE
        # reverse the digits of p, if this is a prime different from p,       #
        # p is an emirp                                                       #
        INT q    := 0;
        INT rest := ABS p;
        WHILE rest > 0 DO
            q    TIMESAB 10;
            q    PLUSAB  rest MOD 10;
            rest OVERAB  10
        OD;
        is prime[ q ] AND q /= p
     FI # is emirp # ;

# generate the required emirp list                                            #
IF value range THEN
    # find emirps with values in the specified range                          #
    print( ( "emirps between ", whole( emirp from, 0 ), " and ", whole( emirp to, 0 ), ":" ) );
    FOR p FROM emirp from TO emirp to DO
        IF is emirp( p ) THEN
            print( ( " ", whole( p, 0 ) ) )
        FI
    OD
ELSE
    # find emirps with ordinals in the specified range                        #
    INT emirp count := 0;
    IF emirp from = emirp to THEN
        print( ( "emirp ", whole( emirp from, 0 ), ":" ) )
    ELSE
        print( ( "emirps ", whole( emirp from, 0 ), " to ", whole( emirp to, 0 ), ":" ) )
    FI;
    FOR p TO max number WHILE emirp count < emirp to DO
        IF is emirp( p ) THEN
            # have another emirp                                              #
            emirp count +:= 1;
            IF emirp count >= emirp from THEN
                print( ( " ", whole( p, 0 ) ) )
            FI
        FI
    OD
FI;
print( ( newline ) )
```

{{out}}
a68g emirpPrimes.a68 - FROM 1 TO 20

```txt

emirps 1 to 20: 13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389

```

a68g emirpPrimes.a68 - FROM 7700 TO 8000 VALUE

```txt

emirps between 7700 and 8000: 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963

```

a68g emirpPrimes.a68 - FROM 10000 TO 10000

```txt

emirp 10000: 948349

```



## AutoHotkey


```AutoHotkey
SetBatchLines, -1
p := 1
Loop, 20 {
	p := NextEmirp(p)
	a .= p " "
}
p := 7700
Loop {
	p := NextEmirp(p)
	if (p > 8000)
		break
	b .= p " "
}
p :=1
Loop, 10000
	p := NextEmirp(p)
MsgBox, % "First twenty emirps: " a
	. "`nEmirps between 7,700 and 8,000: " b
	. "`n10,000th emirp: " p

IsPrime(n) {
	if (n < 2)
		return, 0
	else if (n < 4)
		return, 1
	else if (!Mod(n, 2))
		return, 0
	else if (n < 9)
		return 1
	else if (!Mod(n, 3))
		return, 0
	else {
		r := Floor(Sqrt(n))
		f := 5
		while (f <= r) {
			if (!Mod(n, f))
				return, 0
			if (!Mod(n, (f + 2)))
				return, 0
			f += 6
		}
		return, 1
	}
}

NextEmirp(n) {
	Loop
		if (IsPrime(++n)) {
			rev := Reverse(n)
			if (rev = n)
				continue
			if (IsPrime(rev))
				return n
		}
}

Reverse(s) {
	Loop, Parse, s
		r := A_LoopField r
	return r
}
```

{{Output}}

```txt
First twenty emirps: 13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
Emirps between 7,700 and 8,000: 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
10,000th emirp: 948349
```



## AWK

Based on C example here :

cat emirp.awk

```AWK

function is_prime(n,	p)
{
        if (!(n%2) || !(n%3)) {
		return 0 }
        p = 1
        while(p*p < n)
                if (n%(p += 4) == 0 || n%(p += 2) == 0) {
                        return 0 }
        return 1
}

function reverse(n,	r)
{
	r = 0
        for (r = 0; int(n) != 0; n /= 10)
                r = r*10 + int(n%10);
        return r
}

function is_emirp(n,   r)
{
        r = reverse(n)
	return ((r != n) && is_prime(n) && is_prime(r)) ? 1 : 0
}

BEGIN {
	c = 0
	for (x = 11; c < 20; x += 2) {
		if (is_emirp(x)) {
			printf(" %i,", x); ++c }
	}
	printf("\n")
	for (x = 7701; x < 8000; x += 2) {
		if (is_emirp(x)) {
			printf(" %i,", x); ++c }
	}
	printf("\n")
	c = 0
	for (x = 11; ; x += 2)
                        if (is_emirp(x) && ++c == 10000) {
                                printf(" %i", x);
                                break;
                        }
	printf("\n")
}

```

{{Output}}

```txt

$ awk -f emirp.awk
 13, 17, 31, 37, 71, 73, 79, 97, 107, 113, 149, 157, 167, 179, 199, 311, 337, 347, 359, 389,
 7717, 7757, 7817, 7841, 7867, 7879, 7901, 7927, 7949, 7951, 7963,
 948349

```



## C

Note the unusual commandline argument parsing to sastisfy the "invoke three times" magic requirement.

```c
#include <stdio.h>

typedef unsigned uint;
int is_prime(uint n)
{
        if (!(n%2) || !(n%3)) return 0;
        uint p = 1;
        while(p*p < n)
                if (n%(p += 4) == 0 || n%(p += 2) == 0)
                        return 0;
        return 1;
}

uint reverse(uint n)
{
        uint r;
        for (r = 0; n; n /= 10)
                r = r*10 + (n%10);
        return r;
}

int is_emirp(uint n)
{
        uint r = reverse(n);
        return r != n && is_prime(n) && is_prime(r);
}

int main(int argc, char **argv)
{
        uint x, c = 0;
        switch(argc) { // advanced args parsing
        case 1: for (x = 11; c < 20; x += 2)
                        if (is_emirp(x))
                                printf(" %u", x), ++c;
                break;

        case 2: for (x = 7701; x < 8000; x += 2)
                        if (is_emirp(x))
                                printf(" %u", x);
                break;

        default:
                for (x = 11; ; x += 2)
                        if (is_emirp(x) && ++c == 10000) {
                                printf("%u", x);
                                break;
                        }
        }

        putchar('\n');
        return 0;
}
```

{{out}}

```txt

% ./a.out           # no argument: task 1
 13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
% ./a.out a         # one argument: task 2
 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
% ./a.out a b       # you get the idea
948349

```



## C++


```cpp
#include <vector>
#include <iostream>
#include <algorithm>
#include <sstream>
#include <string>
#include <cmath>

bool isPrime ( int number ) {
   if ( number <= 1 )
      return false ;
   if ( number == 2 )
      return true ;
   for ( int i = 2 ; i <= std::sqrt( number ) ; i++ ) {
      if ( number % i == 0 )
	 return false ;
   }
   return true ;
}

int reverseNumber ( int n ) {
   std::ostringstream oss ;
   oss << n ;
   std::string numberstring ( oss.str( ) ) ;
   std::reverse ( numberstring.begin( ) , numberstring.end( ) ) ;
   return std::stoi ( numberstring ) ;
}

bool isEmirp ( int n ) {
   return isPrime ( n ) && isPrime ( reverseNumber ( n ) )
      && n != reverseNumber ( n ) ;
}

int main( ) {
   std::vector<int> emirps ;
   int i = 1 ;
   while ( emirps.size( ) < 20 ) {
      if ( isEmirp( i ) ) {
         emirps.push_back( i ) ;
      }
      i++ ;
   }
   std::cout << "The first 20 emirps:\n" ;
   for ( int i : emirps )
      std::cout << i << " " ;
   std::cout << '\n' ;
   int newstart = 7700 ;
   while ( newstart < 8001 ) {
      if ( isEmirp ( newstart ) )
	std::cout << newstart << '\n' ;
      newstart++ ;
   }
   while ( emirps.size( ) < 10000 ) {
      if ( isEmirp ( i ) ) {
	 emirps.push_back( i ) ;
      }
      i++ ;
   }
   std::cout << "the 10000th emirp is " << emirps[9999] << " !\n" ;

   return 0 ;
}
```

{{out}}

```txt
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
7717
7757
7817
7841
7867
7879
7901
7927
7949
7951
7963
the 10000th emirp is 948349 !

```



## C sharp

{{works with|C sharp|7}}

```csharp
using static System.Console;
using System;
using System.Linq;
using System.Collections.Generic;

public class Program
{
    public static void Main() {
        const int limit = 1_000_000;
        WriteLine("First 20:");
        WriteLine(FindEmirpPrimes(limit).Take(20).Delimit());
        WriteLine();

        WriteLine("Between 7700 and 8000:");
        WriteLine(FindEmirpPrimes(limit).SkipWhile(p => p < 7700).TakeWhile(p => p < 8000).Delimit());
        WriteLine();

        WriteLine("10000th:");
        WriteLine(FindEmirpPrimes(limit).ElementAt(9999));
    }

    private static IEnumerable<int> FindEmirpPrimes(int limit)
    {
        var primes = Primes(limit).ToHashSet();

        foreach (int prime in primes) {
            int reverse = prime.Reverse();
            if (reverse != prime && primes.Contains(reverse)) yield return prime;
	}
    }

    private static IEnumerable<int> Primes(int bound) {
        if (bound < 2) yield break;
        yield return 2;

        BitArray composite = new BitArray((bound - 1) / 2);
        int limit = ((int)(Math.Sqrt(bound)) - 1) / 2;
        for (int i = 0; i < limit; i++) {
            if (composite[i]) continue;
	    int prime = 2 * i + 3;
	    yield return prime;

	    for (int j = (prime * prime - 2) / 2; j < composite.Count; j += prime)
	        composite[j] = true;
        }
	for (int i = limit; i < composite.Count; i++)
	    if (!composite[i]) yield return 2 * i + 3;
    }
}

public static class Extensions
{
    public static HashSet<T> ToHashSet<T>(this IEnumerable<T> source) => new HashSet<T>(source);

    private const string defaultSeparator = " ";
    public static string Delimit<T>(this IEnumerable<T> source, string separator = defaultSeparator) =>
        string.Join(separator ?? defaultSeparator, source);

    public static int Reverse(this int number)
    {
	if (number < 0) return -Reverse(-number);
	if (number < 10) return number;
	int reverse = 0;
	while (number > 0) {
	    reverse = reverse * 10 + number % 10;
	    number /= 10;
	}
	return reverse;
    }
}
```

{{out}}

```txt

First 20:
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389

Between 7700 and 8000:
7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963

10000th:
948349

```



## Clojure

===Using biginteger's isProbablePrime()===
The isProbablePrime() method performs a Miller-Rabin primality test to within a given certainty.

```clojure
(defn emirp? [v]
  (let [a (biginteger v)
        b (biginteger (clojure.string/reverse (str v)))]
    (and (not= a b)
         (.isProbablePrime a 16)
         (.isProbablePrime b 16))))

; Generate the output
(println "first20:    " (clojure.string/join " " (take 20 (filter emirp? (iterate inc 0)))))
(println "7700-8000:  " (clojure.string/join " " (filter emirp? (range 7700 8000))))
(println "10,000:     " (nth (filter emirp? (iterate inc 0)) 9999))


```

{{out}}

```txt
first20:     13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
7700-8000:   7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
10,000:      948349

```





## Common Lisp


It uses a primitive prime function found in http://www.rosettacode.org/wiki/Primality_by_trial_division, not optimized at all.

```Lisp
(defun primep (n)
  "Is N prime?"
  (and (> n 1)
       (or (= n 2) (oddp n))
       (loop for i from 3 to (isqrt n) by 2
	  never (zerop (rem n i)))))

(defun reverse-digits (n)
  (labels ((next (n v)
             (if (zerop n) v
                 (multiple-value-bind (q r)
                     (truncate n 10)
                   (next q (+ (* v 10) r))))))
    (next n 0)))

(defun emirp (&key (count nil) (start 10) (end nil) (print-all nil))
  (do* ((n start (1+ n))
        (c count) )
       ((or (and count (<= c 0)) (and end (>= n end))))
    (when (and (primep n) (not (= n (reverse-digits n))) (primep (reverse-digits n)))
      (when print-all (format t "~a " n))
      (when count (decf c)) )))


(progn
  (format t "First 20 emirps: ") (emirp :count 20 :print-all t)
  (format t "~%Emirps between 7700 and 8000: ") (emirp :start 7700 :end 8000 :print-all t)
  (format t "~%The 10,000'th emirp: ") (emirp :count 10000 :print-all nil) )

```


{{out}}

```txt
First 20 emirps: 13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
Emirps between 7700 and 8000: 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
The 10,000'th emirp: 948349
```





## D


```d
bool isEmirp(uint n) pure nothrow @nogc {
    bool isPrime(in uint n) pure nothrow @nogc {
        if (n == 2 || n == 3)
            return true;
        else if (n < 2 || n % 2 == 0 || n % 3 == 0)
            return false;
        for (uint div = 5, inc = 2; div ^^ 2 <= n;
             div += inc, inc = 6 - inc)
            if (n % div == 0)
                return false;

        return true;
    }

    uint reverse(uint n) pure nothrow @nogc {
        uint r;
        for (r = 0; n; n /= 10)
            r = r * 10 + (n % 10);
        return r;
    }

    immutable r = reverse(n);
    return r != n && isPrime(n) && isPrime(r);
}

void main() {
    import std.stdio, std.algorithm, std.range;

    auto uints = uint.max.iota;
    writeln("First 20:\n", uints.filter!isEmirp.take(20));
    writeln("Between 7700 and 8000:\n",
            iota(7_700, 8_001).filter!isEmirp);
    writeln("10000th: ", uints.filter!isEmirp.drop(9_999).front);
}
```

{{out}}

```txt
First 20:
[13, 17, 31, 37, 71, 73, 79, 97, 107, 113, 149, 157, 167, 179, 199, 311, 337, 347, 359, 389]
Between 7700 and 8000:
[7717, 7757, 7817, 7841, 7867, 7879, 7901, 7927, 7949, 7951, 7963]
10000th: 948349
```

This code is not efficient, but the run-time is acceptable, about 0.33 seconds with the ldc2 compiler.

===Sieve-Based Version===

```d
import std.stdio, std.algorithm, std.range, std.bitmanip;

/// Not extendible Sieve of Eratosthenes.
BitArray sieve(in uint n) pure nothrow /*@safe*/ {
    BitArray composites;
    composites.init([true, true]);
    composites.length = n;
    if (n < 2)
        return composites;

    foreach (immutable uint i; 2 .. cast(uint)(n ^^ 0.5) + 1)
        if (!composites[i])
            for (uint k = i * i; k < n; k += i)
                composites[k] = true;

    return composites;
}

__gshared BitArray composites;

bool isEmirp(uint n) nothrow @nogc {
    uint reverse(uint n) pure nothrow @safe @nogc {
        uint r;
        for (r = 0; n; n /= 10)
            r = r * 10 + (n % 10);
        return r;
    }

    immutable r = reverse(n);
    // BitArray doesn't perform bound tests yet.
    assert(n < composites.length && r < composites.length);
    return r != n && !composites[n] && !composites[r];
}

void main() {
    composites = 1_000_000.sieve;

    auto uints = uint.max.iota;
    writeln("First 20:\n", uints.filter!isEmirp.take(20));
    writeln("Between 7700 and 8000:\n",
            iota(7_700, 8_001).filter!isEmirp);
    writeln("10000th: ", uints.filter!isEmirp.drop(9_999).front);
}
```

The output is the same. With ldc2 compiler the run-time is about 0.06 seconds.


## Elixir


```elixir
defmodule Emirp do
  defp prime?(2), do: true
  defp prime?(n) when n<2 or rem(n,2)==0, do: false
  defp prime?(n), do: prime?(n,3)

  defp prime?(n,k) when n<k*k, do: true
  defp prime?(n,k) when rem(n,k)==0, do: false
  defp prime?(n,k), do: prime?(n,k+2)

  def emirp?(n) do
    if prime?(n) do
      reverse = to_string(n) |> String.reverse |> String.to_integer
      n != reverse and prime?(reverse)
    end
  end

  def task do
    emirps = Stream.iterate(1, &(&1+1)) |> Stream.filter(&emirp?/1)
    first = Enum.take(emirps,20) |> Enum.join(" ")
    IO.puts "First 20 emirps: #{first}"
    between = Enum.reduce_while(emirps, [], fn x,acc ->
      cond do
        x < 7700        -> {:cont, acc}
        x in 7700..8000 -> {:cont, [x | acc]}
        true            -> {:halt, Enum.reverse(acc)}
      end
    end) |> Enum.join(" ")
    IO.puts "Emirps between 7,700 and 8,000: #{between}"
    IO.puts "10,000th emirp: #{Enum.at(emirps, 9999)}"
  end
end

Emirp.task
```


{{out}}

```txt

First 20 emirps: 13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
Emirps between 7,700 and 8,000: 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
10,000th emirp: 948349

```


=={{header|F_Sharp|F#}}==

### The function

This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Generate emirps. Nigel Galloway: November 19th., 2017
let emirp =
  let rec fN n g = match n with |0->g |_->fN (n/10) (g*10+n%10)
  let     fG n g = n<>g && isPrime g
  primes |> Seq.filter (fun n -> fG n (fN n 0))

```


### The Task


```fsharp

emirps |> (Seq.take 20) |> Seq.iter (printf "%d ")

```

{{out}}

```txt

13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389

```


```fsharp

emirps |> Seq.skipWhile (fun n->n<7700) |> Seq.takeWhile (fun n->n<=8000) |> Seq.iter (printf "%d ")

```

{{out}}

```txt

7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963

```


```fsharp

printfn "%d" (Seq.item 9999 emirps)

```

{{out}}

```txt

948349

```


```fsharp

// count # of emirps with n = 2 to 7 digits. Nigel Galloway: August 8th., 2018
let n=emirp |> Seq.takeWhile(fun n->n<10000000) |> Seq.countBy(fun n->match n with |n when n>999999->7
                                                                                   |n when n> 99999->6
                                                                                   |n when n>  9999->5
                                                                                   |n when n>   999->4
                                                                                   |n when n>    99->3
                                                                                   |_              ->2)
for n,g in n do printfn "%d -> %d" n g

```

{{out}}

```txt

2 -> 8
3 -> 28
4 -> 204
5 -> 1406
6 -> 9538
7 -> 70474
Real: 00:07:19.408, CPU: 00:07:23.250, GC gen0: 59744, gen1: 3

```



## Factor


```factor
USING: io kernel lists lists.lazy math.extras math.parser
    math.primes sequences ;
FROM: prettyprint => . pprint ;
IN: rosetta-code.emirp

: rev ( n -- n' )
    number>string reverse string>number ;

: emirp? ( n -- ? )
    dup rev [ = not ] [ [ prime? ] bi@ ] 2bi and and ;

: nemirps ( n -- seq )
    0 lfrom [ emirp? ] lfilter ltake list>array ;

: print-seq ( seq -- )
    [ pprint bl ] each nl ;

: part1 ( -- )
    "First 20 emirps:" print 20 nemirps print-seq ;

: part2 ( -- )
    "Emirps between 7700 and 8000:" print
    7700 ... 8000 [ emirp? ] filter print-seq ;

: part3 ( -- )
    "10,000th emirp:" print 10,000 nemirps last . ;

: main ( -- )
    part1 nl part2 nl part3 ;

MAIN: main
```

{{out}}

```txt

First 20 emirps:
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389

Emirps between 7700 and 8000:
7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963

10,000th emirp:
948349

```



## Fortran

Fortran has no standard interface arrangements whereby a run can be supplied with parameters from a command line. Some implementations do provide a routine, possibly called something like GETARG and with a variety of parameters and usages. One can of course read a disc file containing suitable parameters, but this is not as specified. So, to meet the three invocations, a subroutine is devised with parameters that allow it to perform the three different tasks. To handle "the first twenty", the parameters are easy 1,20. For "the 10,000'th", they are 10000,10000. Meeting the requirement for an invocation that lists all emirPs between 7,700 and 8,000 involved further bending, with the result that the subroutine has four parameters. Then I thought: why not specify the base for the numerology? So, five.

Now arise such questions as well-based emirPs. For instance, 17 is a base ten emirP, and in (say) base thirteen, 17 is 14 (and not an even number); 41 in base thirteen is 53 and that is a prime also. So, 17 (the number) is an emirP in both base ten and base thirteen. Is there a maximally-based emirP? But, for now, onwards in base ten.

The source would be F77, except for the idea of having the assistant routines GETPRIME(i), NEXTPRIME(n), and ISPRIME(n) all share the responsibility for the updating of a stash of prime numbers as they find the need rather than pre-emptively calculating a table of primes that is large enough for any expected usage, possibly by some high-speed trickery. Since the routines invoke each other back and forth, the dreaded attribute of RECURSIVE must be declared to encourage the compiler and so F90 is required. Otherwise, each routine would have to be careful over its own usage. Each would separately have to be able to proceed past the end of the current stash of prime numbers should its need arise, and augment the table as possible. For use in factoring numbers, the table need not be large as P(4792) = 46337, and the square of this exceeds the capacity of a signed thirty-two bit integer. But in this task, actual prime numbers are required well beyond that. Each run announces the table size, thereby showing the limit of its table of primes; there seemed no point in clearing the table each time to more closely follow the notion of separate runs.

For factoring numbers up to the 32-bit two's complement integer limit, the table need not be large, and it can easily enough be stored as a collection of sixteen and thirty-two bit numbers to save some space. Accessing an array PRIME(i) can be made a function GETPRIME(i) without a change in syntax (as needed in pascal: Prime[i] for an array, GetPrime(i) for a function), at least for reading. So, instead of 4792x4 = 19168 bytes, 12144 are needed, to set against the additional code complexity. These days, this is a difference of small importance. Actually, a further value is needed to hold Prime(4793) = 46349. Function ISPRIME does not determine its stepping point via the near universal usage of SQRT(n). If calculated in double precision this will give acceptable results for a 32-bit integer, but I have been burnt by an ad-hoc calculation nDgits = LOG10(x) + 1 failing for x = 10 because Log10(10) = 0·9999etc. which may well round to one, but truncates to zero. So, a SQRT-free demonstration, needed if the MOD function were unavailable. Actually, if P(i) is the last factor to be checked, this suffices up to the square of P(i + 1), not P(i). But this bound is only useful when successive numbers are being tested; for an individual factorisation it is too messy.

The initial version ran very slowly once past the first run, and this prompted some instrumentation, the addition of counters for the invocations. It transpired that GETPRIME(i) was being invoked thousands of millions of times... Once again, a N<sup>2</sup> process is to be avoided, here when NEXTPRIME(n) was stepping linearly along the array of primes (in the hope of knowing the next prime along without having to recalculate it) and being invoked many times to do so. This was fixed by introducing a binary search, the list of primes being of course in order. The early version of NEXTPRIME(n) also did not attempt to save new primes, as it might be invoked with a value well beyond the end of the table and the next value on from ''n'' might be past many lesser primes. But by working on from PRIME(NP) up to ''n'' they can be found and saved along the way. Saving new primes in NEXTPRIME meant that GETPRIME should no longer itself attempt saving, as it is invoking NEXTPRIME. Mutual recursion is all very well, but organisation is important also.
```Fortran
      MODULE BAG	!A mixed assortment.
       INTEGER MSG	!I/O unit number to share about.
       INTEGER PF16LIMIT,PF32LIMIT,NP			!Know that P(3512) = 32749, the last within two's complement 16-bit integers.
       PARAMETER (PF16LIMIT = 3512, PF32LIMIT = 4793)	!32749² = 1,072,497,001; the integer limit is 2,147,483,647 in 32-bit integers.
       INTEGER*2 PRIME16(PF16LIMIT)			!P(4792) =  46337, next is 46349 and 46337² = 2,147,117,569.
       INTEGER*4 PRIME32(PF16LIMIT + 1:PF32LIMIT)	!Let the compiler track the offsets.
       DATA NP,PRIME16(1),PRIME16(2)/2,2,3/	!But, start off with this. Note that Prime(NP) is odd...
       INTEGER NGP,NNP,NIP	!Invocation counts.
       DATA NGP,NNP,NIP/3*0/	!Starting at zero.
       CONTAINS		!Some co-operating routines.
        RECURSIVE INTEGER FUNCTION GETPRIME(I)	!They are numbered. As if in an array Prime(i).
Chooses from amongst two arrays, of sizes known from previous work.
         INTEGER I		!The desired index.
         INTEGER P		!A potential prime.
         INTEGER MP		!Counts beyond NP.
          NGP = NGP + 1		!Another try.
          IF (I.LE.0) THEN	!A silly question?
            GETPRIME = -666		!This should cause trouble!
          ELSE IF (I.LE.NP) THEN	!I have a little list.
            IF (I.LE.PF16LIMIT) THEN		!Well actually, two little lists.
              GETPRIME = PRIME16(I)		!So, direct access from this.
             ELSE				!Or, for the larger numbers,
              GETPRIME = PRIME32(I)		!This.
            END IF			!So much for previous effort.
          ELSE IF (I.LE.PF32LIMIT) THEN	!My list may not yet be completely filled.
            MP = NP			!This is the last stashed so far.
            P = GETPRIME(NP)		!I'll ask me to figure out where this is stashed.
   10       P = NEXTPRIME(P)		!Go for the next one along.
            MP = MP + 1			!Advance my count.
            IF (MP.LT.I) GO TO 10	!Are we there yet?
            GETPRIME = P		!Yep.
           ELSE			!But, my list may be too short.
            WRITE (MSG,*) "Hic!",I	!So, give an indication.
            STOP "Too far..."		!And quit.
          END IF		!For factoring 32-bit, need only 4792 elements.
        END FUNCTION GETPRIME	!This is probably faster than reading from a monster disc file.

        SUBROUTINE STASHPRIME(P)	!Saves a value in the stash.
         INTEGER P	!The prime to be stashed.
          NP = NP + 1		!Count another in.
          IF (NP.LE.PF16LIMIT) THEN	!But, where to?
            PRIME16(NP) = P			!The short list.
          ELSE IF (NP.LE.PF32LIMIT) THEN!Or,
            PRIME32(NP) = P			!The long list (which is shorter)
          ELSE				!Or,
            STOP "Stash overflow!"		!Oh dear.
          END IF			!It is stashed.
        END SUBROUTINE STASHPRIME	!The checking should be redundant.

        INTEGER FUNCTION FINDPRIME(IT)	!Via binary search.
         INTEGER IT	!The value to be found.
         INTEGER L,R,P	!Assistants.
          L = 0		!This is the *exclusive bounds* version.
          R = NP + 1	!Thus, L = first - 1; R = Last + 1.
    1     P = (R - L)/2		!Probe offset.
          IF (P.LE.0) THEN	!No span?
            FINDPRIME = -L		!Not found. IT follows Prime(L).
            RETURN			!Escape.
          END IF		!But otherwise,
          P = P + L		!Convert to an index into array PRIME, manifested via GETPRIME.
          IF (IT - GETPRIME(P)) 2,4,3	!Compare... Three way result.
    2     R = P; GO TO 1	!IT < PRIME(P): move R back.
    3     L = P; GO TO 1	!PRIME(P) < IT: move L forward.
    4     FINDPRIME = P		!PRIME(P) = IT: Found here!
        END FUNCTION FINDPRIME	!Simple and fast.

        RECURSIVE INTEGER FUNCTION NEXTPRIME(P)	!Some effort may ensue.
Checks the stash in PRIME in the hope of finding the next prime directly, otherwise advances from P.
Collates a stash of primes in PRIME16 and PRIME32, advancing NP from 2 to PF32LIMIT as it goes.
         INTEGER P	!Not necessarily itself a prime number.
         INTEGER PI	!A possibly prime increment.
         INTEGER IT	!A finger.
          NNP = NNP + 1	!Another try
          IF (P.LE.1) THEN	!Dodge annoying effects.	Otherwise, FINDPRIME(P) would be zero.
            PI = 2		!The first prime is known.	Because P precedes Prime(1).
           ELSE			!The first stashed value is two.
            IT = (ABS(FINDPRIME(P)))	!The stash is ordered, and P = 2 will be found at 1.
            IF (IT.LT.NP) THEN		!Before my last-known prime? FINDPRIME(4) = -2 as it follows Prime(NP=2).
              PI = GETPRIME(IT + 1)	!Yes, so I know the next along already.
             ELSE	!Otherwise, it is past Prime(NP). and IT = NP thanks to the ABS.
              IF (NP.LT.PF32LIMIT) THEN	!If my stash is not yet filled,
                PI = GETPRIME(IT)	!I want to start with its last entry, known to be an odd number.
               ELSE			!So that I can stash each next prime along the way.
                PI = P			!Otherwise, start with P.
                IF (MOD(PI,2).EQ.0) PI = PI - 1	!And some suspicion.
              END IF			!So  much for a starting position.
              DO WHILE (PI.LE.P)	!Perhaps I must go further.
   11           PI = PI + 2			!Advance to a possibility.
                IF (.NOT.ISPRIME(PI)) GO TO 11	!Discard it?
                IF (IT.EQ.NP .AND. IT.LT.PF32LIMIT) THEN	!Am I one further on from NP?
                  CALL STASHPRIME(PI)		!Yes, and there is space to stash it.
                  IT = IT + 1			!Ready for the next one along, if it comes.
                END IF			!All are candidates for my stash.
              END DO		!Perhaps this prime will be big enough.
            END IF		!It may be a long way past PRIME(NP).
          END IF		!And I may have filled my stash along the way.
          NEXTPRIME = PI	!Take that.
        END FUNCTION NEXTPRIME	!Messy.

        RECURSIVE LOGICAL FUNCTION ISPRIME(N)	!Checks an arbitrary number, though limited by INTEGER size.
Crunches up to SQRT(N), and at worst needs to be able to reach Prime(4793) = 46349; greater than SQRT(2147483647) = 46340·95...
         INTEGER N	!The number.
         INTEGER I,F,Q	!Assistants.
          NIP = NIP + 1	!Another try.
          IF (N.LT.2) THEN	!Dodge annoyances.
            ISPRIME = .FALSE.	!Such as N = 1, and the first F being 2.
          ELSE			!Otherwise, some effort.
            ISPRIME = .FALSE.	!The usual result.
            I = 1		!Start at the start with PRIME(1).
   10       F = GETPRIME(I)	!Thus, no special case with F = 2.
            Q = N/F		!So, how many times? (Truncation, remember)
            IF (Q .GE. F) THEN	!Q < F means F² > N.
              IF (Q*F .EQ. N) RETURN	!A factor is found!
              I = I + 1		!Very well.
              GO TO 10		!Try the next possible factor.
            END IF		!And if we get through all that,
            ISPRIME = .TRUE.	!It is a prime number.
          END IF		!And we're done.
        END FUNCTION ISPRIME	!After a lot of divisions.

        INTEGER FUNCTION ESREVER(IT,BASE)	!Reversed digits.
         INTEGER IT	!The number to be reversed. Presumably positive.
         INTEGER BASE	!For the numerology.
         INTEGER N,R	!Assistants.
          IF (BASE.LE.1) STOP "Base 2 at least!"	!Ah, distrust.
          N = IT	!A copy I can damage.
          R = 0		!Here we go.
          DO WHILE(N.GT.0)	!A digit remains?
            R = R*BASE + MOD(N,BASE)	!Yes. Grab the low-order digit of N.
            N = N/BASE			!And reduce N by another power of BASE.
          END DO		!Test afresh.
          ESREVER = R		!That's it.
        END FUNCTION ESREVER	!Easy enough.

        SUBROUTINE EMIRP(BASE,N1,N2,I1,I2)	!Two-part interface.
         INTEGER BASE	!Avoid decimalist chauvinism.
         INTEGER N1,N2	!Count span to show those found.
         INTEGER I1,I2	!Search span.
         INTEGER N	!Counter.
         INTEGER P,R	!Assistants.
          WRITE (MSG,1) N1,N2,BASE,I1,I2	!Declare the purpose.
    1     FORMAT ("Show the first ",I0," to ",I0,	!So as to encompass
     &     " emirP numbers (base ",I0,") between ",I0," and ",I0)	!The specified options.
          N = 0		!None found so far.
          P = I1 - 1	!Syncopation. The starting position might itself be a prime number.
Chase another emirP.
   10     P = NEXTPRIME(P)		!I want the next prime.
          IF (P.LT.I1) GO TO 10		!Up to the starting mark yet?
          IF (P.GT.I2) GO TO 900	!Past the finishing mark?
          R = ESREVER(P,BASE)		!Righto, a candidate.
          IF (P .EQ. R) GO TO 10	!Palindromes are rejected.
          IF (.NOT.ISPRIME(R)) GO TO 10	!As are non-primes.
          N = N + 1			!Aha, a success!
c          if (mod(n,100) .eq. 0) then
c            write (6,66) N,P,R,NP,NGP,NNP,NIP
c   66       format ("N=",I5,",p=",I6,",R=",I6,",NP=",I6,3I12)
c          end if
          IF (N.GE.N1) WRITE (6,*) P,R	!Are we within the count span?
          IF (N.LT.N2) GO TO 10		!Past the end?
Closedown.
  900     WRITE (MSG,901) NP,GETPRIME(NP)	!Might be of interest.
  901     FORMAT ("Stashed up to Prime(",I0,") = ",I0,/)
        END SUBROUTINE EMIRP	!Well, that was odd.
      END MODULE BAG	!Mixed.

      PROGRAM POKE	!Now put it all to the test.
      USE BAG		!With ease.
      MSG = 6		!Standard output.

      CALL EMIRP(10,    1,   20,   1,   1000)	!These parameters
      CALL EMIRP(10,    1,   28,7700,   8000)	!Meet the specifiction
      CALL EMIRP(10,10000,10000,   1,1000000)	!Of three separate invocations.

      END	!Whee!
```

Output:

```txt

Show the first 1 to 20 emirP numbers (base 10) between 1 and 1000
          13          31
          17          71
          31          13
          37          73
          71          17
          73          37
          79          97
          97          79
         107         701
         113         311
         149         941
         157         751
         167         761
         179         971
         199         991
         311         113
         337         733
         347         743
         359         953
         389         983
Stashed up to Prime(77) = 389

Show the first 1 to 28 emirP numbers (base 10) between 7700 and 8000
        7717        7177
        7757        7577
        7817        7187
        7841        1487
        7867        7687
        7879        9787
        7901        1097
        7927        7297
        7949        9497
        7951        1597
        7963        3697
Stashed up to Prime(1008) = 8009

Show the first 10000 to 10000 emirP numbers (base 10) between 1 and 1000000
      948349      943849
Stashed up to Prime(4793) = 46349

```

And the invocation counts:  GETPRIME 15,200,926; NEXTPRIME 74,799; ISPRIME 548,944. The execution time is small: the run completes even as the new output window stabilises on the screen.

An earlier version used a larger table of primes (size 123,456) as EMIRP advanced via <code>I = I + 1; P = GETPRIME(I)</code> thereby only considering successive primes as candidates without having to check factors to find them. By converting to <code>P = NEXTPRIME(P)</code> the table could be made smaller, but this meant being clear within NEXTPRIME that if P was greater than the last stashed prime, ''and'' the table was filled, then the table no longer offered an advantage and the search should start from P. With larger P, starting from Prime(NP) meant more and more catching up.

Function ISPRIME uses GETPRIME(i) for its successive factor trials, and thus works only up to the table limit unless GETPRIME were to be extended. If NEXTPRIME were used instead the table would be accessed where possible, otherwise a march would begin. If ISPRIME were to be changed to accept say a 64-bit integer the table size limit could be increased, but alas a complete table would require around 139,094,144 entries, and all those trial divisions would take a while. Still, the possible factors go no further than F = SQRT(N), approximately calculated now, and to check that F has no factors requires only tests up to SQRT(F)...

Project [[Extensible_prime_generator#Fortran]] offers a scheme supporting such routines as PRIME(i) instead of GETPRIME(i), NEXTPRIME(N), and ISPRIME(N), using a disc file in place of a large array in memory - whose values would be lost when the run finishes. But instead of about a hundred lines of Fortran to provide primes for EMIRP, module PRIMEBAG requires 311 lines.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function isPrime(n As UInteger) As Boolean
  If n < 2 Then Return False
  If n Mod 2 = 0 Then Return n = 2
  If n Mod 3 = 0 Then Return n = 3
  Dim d As Integer = 5
  While d * d <= n
    If n Mod d = 0 Then Return False
    d += 2
    If n Mod d = 0 Then Return False
    d += 4
  Wend
  Return True
End Function

Function reverseNumber(n As UInteger) As UInteger
  If n < 10 Then Return n
  Dim As Integer sum = 0
  While n > 0
    sum = 10 * sum  + (n Mod 10)
    n \= 10
  Wend
  Return sum
End Function

Function isEmirp(n As UInteger) As Boolean
  If Not isPrime(n) Then Return False
  Dim As UInteger reversed = reverseNumber(n)
  Return reversed <> n AndAlso CInt(isPrime(reversed))
End Function

' We can immediately rule out all primes from 2 to 11 as these are palindromic
' and not therefore Emirp primes
Print "The first 20 Emirp primes are :"
Dim As UInteger count = 0, i = 13
Do
  If isEmirp(i) Then
    Print Using "####"; i;
    count + = 1
  End If
  i += 2
Loop Until count = 20
Print : Print
Print "The Emirp primes between 7700 and 8000 are:"
i = 7701
Do
  If isEmirp(i) Then Print Using "#####"; i;
  i += 2
Loop While i < 8000
Print : Print
Print "The 10,000th Emirp prime is : ";
i = 13 : count = 0
Do
  If isEmirp(i) Then count += 1
  If count = 10000 Then Exit Do
  i += 2
Loop
Print i
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

The first 20 Emirp primes are :
  13  17  31  37  71  73  79  97 107 113 149 157 167 179 199 311 337 347 359 389


The Emirp primes between 7700 and 8000 are:
 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963

The 10,000th Emirp prime is : 948349

```



## Go

This has a bit more to it than required but little optimization, other than using a fast Sieve of Atkin implementation for the prime numbers and skipping some tests on ranges of impossible Emirps (thanks to a comment on the discussion page).

As a side note, by using the same API as the prime number generator this also demonstrates how Go interfaces can be used (and note it doesn't require the existing code/package to know anything about the interface being defined).

```go
package main

import (
	"flag"
	"fmt"
	"github.com/jbarham/primegen.go" // Sieve of Atkin implementation
	"math"
)

// primeCache is a simple cache of small prime numbers, it very
// well might be faster to just regenerate them as needed.
type primeCache struct {
	gen    *primegen.Primegen
	primes []uint64
}

func NewPrimeCache() primeCache {
	g := primegen.New()
	return primeCache{gen: g, primes: []uint64{g.Next()}}
}

// upto returns a slice of primes <= n.
// The returned slice is shared with all callers, do not modify it!
func (pc *primeCache) upto(n uint64) []uint64 {
	if p := pc.primes[len(pc.primes)-1]; p <= n {
		for p <= n {
			p = pc.gen.Next()
			pc.primes = append(pc.primes, p)
		}
		return pc.primes[:len(pc.primes)-1]
	}
	for i, p := range pc.primes {
		if p > n {
			return pc.primes[:i]
		}
	}
	panic("not reached")
}

var cache = NewPrimeCache()

func sqrt(x uint64) uint64 { return uint64(math.Sqrt(float64(x))) }

// isprime does a simple test if n is prime.
// See also math/big.ProbablyPrime().
func isprime(n uint64) bool {
	for _, p := range cache.upto(sqrt(n)) {
		if n%p == 0 {
			return false
		}
	}
	return true
}

func reverse(n uint64) (r uint64) {
	for n > 0 {
		r = 10*r + n%10
		n /= 10
	}
	return
}

// isEmirp does a simple test if n is Emirp, n must be prime
func isEmirp(n uint64) bool {
	r := reverse(n)
	return r != n && isprime(r)
}

// EmirpGen is a sequence generator for Emirp primes
type EmirpGen struct {
	pgen     *primegen.Primegen
	nextn    uint64
	r1l, r1h uint64
	r2l, r2h uint64
	r3l, r3h uint64
}

func NewEmirpGen() *EmirpGen {
	e := &EmirpGen{pgen: primegen.New()}
	e.Reset()
	return e
}

func (e *EmirpGen) Reset() {
	e.pgen.Reset()
	e.nextn = 0
	// Primes >7 cannot end in 2,4,5,6,8 (leaving 1,3,7)
	e.r1l, e.r1h = 20, 30
	e.r2l, e.r2h = 40, 70
	e.r3l, e.r3h = 80, 90
}

func (e *EmirpGen) next() (n uint64) {
	for n = e.pgen.Next(); !isEmirp(n); n = e.pgen.Next() {
		// Skip over inpossible ranges
		// Benchmarks show this saves ~20% when generating n upto 1e6
		switch {
		case e.r1l <= n && n < e.r1h:
			e.pgen.SkipTo(e.r1h)
		case e.r2l <= n && n < e.r2h:
			e.pgen.SkipTo(e.r2h)
		case e.r3l <= n && n < e.r3h:
			e.pgen.SkipTo(e.r3h)
		case n > e.r3h:
			e.r1l *= 10
			e.r1h *= 10
			e.r2l *= 10
			e.r2h *= 10
			e.r3l *= 10
			e.r3h *= 10
		}
	}
	return
}

func (e *EmirpGen) Next() (n uint64) {
	if n = e.nextn; n != 0 {
		e.nextn = 0
		return
	}
	return e.next()
}

func (e *EmirpGen) Peek() uint64 {
	if e.nextn == 0 {
		e.nextn = e.next()
	}
	return e.nextn
}

func (e *EmirpGen) SkipTo(nn uint64) {
	e.pgen.SkipTo(nn)
	e.nextn = 0
	return
}

// SequenceGen defines an arbitrary sequence generator.
// Both *primegen.Primegen and *EmirpGen implement this.
type SequenceGen interface {
	Next() uint64
	Peek() uint64
	Reset()
	SkipTo(uint64)
	//Count(uint64) uint64 // not implemented for *EmirpGen
}

func main() {
	var start, end uint64
	var n, skip uint
	var oneline, primes bool
	flag.UintVar(&n, "n", math.MaxUint64, "number of emirps to print")
	flag.UintVar(&skip, "skip", 0, "number of emirps to skip")
	flag.Uint64Var(&start, "start", 0, "start at x>=start")
	flag.Uint64Var(&end, "end", math.MaxUint64, "stop at x<=end")
	flag.BoolVar(&oneline, "oneline", false, "output on a single line")
	flag.BoolVar(&primes, "primes", false, "generate primes rather than emirps")
	flag.Parse()

	sep := "\n"
	if oneline {
		sep = " "
	}

	// Here's where making SequenceGen an interface comes in handy:
	var seq SequenceGen
	if primes {
		seq = primegen.New()
	} else {
		seq = NewEmirpGen()
	}

	for seq.Peek() < start {
		seq.Next()
	}
	for ; skip > 0; skip-- {
		seq.Next()
	}
	for ; n > 0 && seq.Peek() <= end; n-- {
		fmt.Print(seq.Next(), sep)
	}
	if oneline {
		fmt.Println()
	}
}
```

{{out}}

```txt

$ ./emirp -h
Usage of ./emirp:
  -end=18446744073709551615: stop at x<=end
  -n=18446744073709551615: number of emirps to print
  -oneline=false: output on a single line
  -primes=false: generate primes rather than emirps
  -skip=0: number of emirps to skip
  -start=0: start at x>=start

$ ./emirp -oneline -n 20 -primes # not asked for, just demonstrating SequenceGen interface
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71

$ ./emirp -oneline -n 20
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389

$ ./emirp -oneline -start 7800 -end 8000
7817 7841 7867 7879 7901 7927 7949 7951 7963

$ ./emirp -skip 9999 -n 1
948349

```



## Haskell


{{libheader|primes}}
{{Works with|GHC|7.8.3}}
{{Works with|primes|0.2.1.0}}


```haskell
#!/usr/bin/env runghc

import Data.HashSet (HashSet, fromList, member)
import Data.List
import Data.Numbers.Primes
import System.Environment
import System.Exit
import System.IO

-- optimization mentioned on the talk page
startDigOK :: Integer -> Bool
startDigOK n = head (show n) `elem` "1379"

-- infinite list of primes that have an acceptable first digit
filtPrimes :: [Integer]
filtPrimes = filter startDigOK primes

-- finite list of primes that have an acceptable first digit and
-- are the specified number of digits in length
nDigsFPr :: Integer -> [Integer]
nDigsFPr n =
  takeWhile (< hi) $ dropWhile (< lo) filtPrimes
  where lo = 10 ^ (n - 1)
        hi = 10 ^ n

-- hash set of the filtered primes of the specified number of digits
nDigsFPrHS :: Integer -> HashSet Integer
nDigsFPrHS n = fromList $ nDigsFPr n

-- infinite list of hash sets, where each hash set contains primes of
-- a specific number of digits, i. e. index 2 contains 2 digit primes,
-- index 3 contains 3 digit primes, etc.
-- Don't access index 0, because it will return an error
fPrByDigs :: [HashSet Integer]
fPrByDigs = map nDigsFPrHS [0 ..]

isEmirp :: Integer -> Bool
isEmirp n =
  let revStr = reverse $ show n
      reversed = read revStr
      hs = fPrByDigs !! length revStr
  in (startDigOK n) && (reversed /= n) && (reversed `member` hs)

emirps :: [Integer]
emirps = filter isEmirp primes

emirpSlice :: Integer -> Integer -> [Integer]
emirpSlice from to =
  genericTake numToTake $ genericDrop numToDrop emirps
  where
    numToDrop = from - 1
    numToTake = 1 + to - from

emirpValues :: Integer -> Integer -> [Integer]
emirpValues lo hi =
  dropWhile (< lo) $ takeWhile (<= hi) emirps

usage = do
  name <- getProgName
  putStrLn $ "usage: " ++ name ++ " lo hi [slice | values]"
  exitFailure

main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  fixedArgs <- case length args of
    1 -> return $ args ++ args ++ ["slice"]
    2 -> return $ args ++ ["slice"]
    3 -> return args
    _ -> usage
  let lo = read $ fixedArgs !! 0
      hi = read $ fixedArgs !! 1
  case fixedArgs !! 2 of
   "slice" -> print $ emirpSlice lo hi
   "values" -> print $ emirpValues lo hi
   _ -> usage
```


{{out}}
This program uses the same format for command line arguments as the Perl 6 example.


```txt

$ ./Emirp.hs 1 20
[13,17,31,37,71,73,79,97,107,113,149,157,167,179,199,311,337,347,359,389]
$ ./Emirp.hs 7700 8000 values
[7717,7757,7817,7841,7867,7879,7901,7927,7949,7951,7963]
$ ./Emirp.hs 10000
[948349]

```


===List-based===
Using list-based incremental sieve from [[Sieve_of_Eratosthenes#With_Wheel|here]] and trial division from [[Primality_by_trial_division#Haskell|here]],

```haskell
 λ> let emirp p = let q=(read.reverse.show) p in q /= p && noDivsBy primesW q

 λ> take 20 . filter emirp $ primesW
[13,17,31,37,71,73,79,97,107,113,149,157,167,179,199,311,337,347,359,389]

 λ> filter emirp . takeWhile (< 8000) . dropWhile (< 7700) $ primesW
[7717,7757,7817,7841,7867,7879,7901,7927,7949,7951,7963]  --  0.02 secs

 λ> (!! (10000-1)) . filter emirp $ primesW
948349                                                    -- 0.69 secs
```



## J


'''Solution''':
```j
   emirp =: (] #~ ~: *. 1 p: ]) |.&.:":"0  NB. Input is array of primes
```


In other words: select numbers from the argument list whose decimal reverse is both different and prime and return those decimal reversed values as numbers. (For simplicity, we require that our argument be a list of prime numbers.)

'''Examples'''
```j
   /:~ emirp p: 2+i.75
13 17 31 37 71 73 79 97 113 311 701 733 743 751 761 941 953 971 983 991

   (#~ 7700&< * 8000&>) /:~ emirp i.&.(_1&p:) 9999
7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963

   # emirp p: i.74791                          NB. 10,000th emirp is 74,790th prime
10000
   p: 74790
948349

   NB. alternative approach (first emirp value would be at index 0):
   9999 { /:~ emirp p:i.1e5
943849
```



## Java

This implementation uses a slight optimization discussed in the talk page. It will not actually check the primality (forwards or backwards) for a number that starts or ends with the digits 2, 4, 5, 6, or 8 since no primes greater than 7 end with those digits.

```java
public class Emirp{

	//trivial prime algorithm, sub in whatever algorithm you want
	public static boolean isPrime(long x){
		if(x < 2) return false;
		if(x == 2) return true;
		if((x & 1) == 0) return false;

		for(long i = 3; i <= Math.sqrt(x);i+=2){
			if(x % i == 0) return false;
		}

		return true;
	}

	public static boolean isEmirp(long x){
		String xString = Long.toString(x);
		if(xString.length() == 1) return false;
		if(xString.matches("[24568].*") || xString.matches(".*[24568]")) return false; //eliminate some easy rejects
		long xR = Long.parseLong(new StringBuilder(xString).reverse().toString());
		if(xR == x) return false;
		return isPrime(x) && isPrime(xR);
	}

	public static void main(String[] args){
		int count = 0;
		long x = 1;

		System.out.println("First 20 emirps:");
		while(count < 20){
			if(isEmirp(x)){
				count++;
				System.out.print(x + " ");
			}
			x++;
		}

		System.out.println("\nEmirps between 7700 and 8000:");
		for(x = 7700; x <= 8000; x++){
			if(isEmirp(x)){
				System.out.print(x +" ");
			}
		}

		System.out.println("\n10,000th emirp:");
		for(x = 1, count = 0;count < 10000; x++){
			if(isEmirp(x)){
				count++;
			}
		}
		//--x to fix the last increment from the loop
		System.out.println(--x);
	}
}
```

{{out}}

```txt
First 20 emirps:
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
Emirps between 7700 and 8000:
7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
10,000th emirp:
948349
```



## JavaScript

Script source

```javascript
function isPrime(n) {
    if (!(n % 2) || !(n % 3)) return 0;

    var p = 1;
    while (p * p < n) {
        if (n % (p += 4) == 0 || n % (p += 2) == 0) {
            return false
        }
    }
    return true
}

function isEmirp(n) {
    var s = n.toString();
    var r = s.split("").reverse().join("");
    return r != n && isPrime(n) && isPrime(r);
}

function main() {
    var out = document.getElementById("content");

    var c = 0;
    var x = 11;
    var last;
    var str;

    while (c < 10000) {
        if (isEmirp(x)) {
            c += 1;

            // first twenty emirps
            if (c == 1) {
                str = "<p>" + x;
            }
            else if (c < 20) {
                str += " " + x;
            }
            else if (c == 20) {
                out.innerHTML = str + " " + x + "</p>";
            }

            // all emirps between 7,700 and 8,000
            else if (7700 <= x && x <= 8001) {
                if (last < 7700) {
                    str = "<p>" + x;
                } else {
                    str += " " + x;
                }
            }
            else if (x > 7700 && last < 8001) {
                out.innerHTML += str + "</p>";
            }

            // the 10,000th emirp
            else if (c == 10000) {
                out.innerHTML += "<p>" + x + "</p>";
            }

            last = x;
        }
        x += 2;
    }
}

```


Solution page

```html
<!DOCTYPE html>
<html>
    <head>
        <title>Emirp primes</title>
        <script src="emirp.js"></script>
    </head>
    <body onload="main()">
        <div id="content"></div>
    </body>
</html>
```

{{out}}

```txt
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
948349
```



## jq

{{works with| jq | with foreach}}

The given tasks are simple to implement in jq if unbounded streams can be harnessed,
which is possible in versions of jq that support "foreach" and "break". This article accordingly
showcases the use of these builtins, which have been available since July 7, 2014.

'''Infrastructure: prime numbers'''

```jq
def is_prime:
  if  . == 2 then true
  else
     2 < . and . % 2 == 1 and
       (. as $in
       | (($in + 1) | sqrt) as $m
       | [false, 3] | until( .[0] or .[1] > $m; [$in % .[1] == 0, .[1] + 2])
       | .[0]
       | not)
  end ;

def relatively_prime:
  .[0] as $n
  | .[1] as $primes
  | ($n | sqrt) as $s
  | (.[1] | length) as $length
  | [0, true]
  | until( .[0] > $length or ($primes[.[0]] > $s) or .[1] == false;
          [.[0] + 1, ($n % $primes[.[0]] != 0)] )
  | .[1] ;

def primes:
  # The helper function, next, has arity 0 for tail recursion optimization;
  # its input must be an array of primes of length at least 2,
  # the last also being the greatest.
  def next:
     . as $previous
     | .[length-1] as $last
     | [(2 + $last), $previous]
     | until( relatively_prime ; .[0] += 2) as $nextp
     | ( $previous + [$nextp[0]] );
  2, ([2,3] | recurse( next ) | .[-1]) ;
```


'''Emirps'''

```jq
def is_emirp:
  . as $n
  | tostring | explode | reverse | implode | tonumber | (. != $n) and is_prime ;

# emirps(n) emits [i, p] where p is the i-th emirp, up to and including i == n
def emirps(n):
  label $start
  | # state: [count, $emirp]
  foreach primes as $p ([0, null];
    if .[0] >= n then break $start
    else if ($p | is_emirp) then [.[0] + 1, $p] else .[1] = null end
    end;
    if .[1] then . else empty end ) ;
```


'''The tasks'''

(0) The three separate subtasks can be accomplished in one step as follows:

```jq
emirps(10000)
| select( .[0] <= 20 or (7700 <= .[1] and .[1] <= 8000) or .[0] == 10000)
```


The output of the above is shown below.

To accomplish the three subtasks separately:

(1) First twenty:

```jq
emirps(20)
```


(2) Selection by value
<lang>label $top
| primes
| if (7700 <= .) and (. <= 8000) and is_emirp then .
   elif . > 8000 then break $top
   else empty
   end
```


(3) 10,000th
<lang>last(emirps(10000)) | .[1]
```


{{out}}

```sh
$ jq -c -n -f Emirp_primes.jq
[1,13]
[2,17]
[3,31]
[4,37]
[5,71]
[6,73]
[7,79]
[8,97]
[9,107]
[10,113]
[11,149]
[12,157]
[13,167]
[14,179]
[15,199]
[16,311]
[17,337]
[18,347]
[19,359]
[20,389]
[180,7717]
[181,7757]
[182,7817]
[183,7841]
[184,7867]
[185,7879]
[186,7901]
[187,7927]
[188,7949]
[189,7951]
[190,7963]
[10000,948349]
```



## Julia


```julia
using Primes

function collapse(n::Array{<:Integer})
    sum = 0
    for (p, d) in enumerate(n)
        sum += d * 10 ^ (p - 1)
    end
    return sum
end

Base.reverse(n::Integer) = collapse(reverse(digits(n)))
isemirp(n::Integer) = (if isprime(n) m = reverse(n); return m != n && isprime(m) end; false)

function firstnemirps(m::Integer)
    rst = zeros(typeof(m), m)
    i, n = 1, 2
    while i ≤ m
        if isemirp(n)
            rst[i] = n
            i += 1
        end
        n += 1
    end
    return rst
end

emirps = firstnemirps(10000)
println("First 20:\n", emirps[1:20])
println("Between 7700 and 8000:\n", filter(x -> 7700 ≤ x ≤ 8000, emirps))
println("10000th:\n", emirps[10000])

```
{{out}}

```txt

First 20:
[13, 17, 31, 37, 71, 73, 79, 97, 107, 113, 149, 157, 167, 179, 199, 311, 337, 347, 359, 389]
Between 7700 and 8000:
[7717, 7757, 7817, 7841, 7867, 7879, 7901, 7927, 7949, 7951, 7963]
10000th:
948349

```



## Kotlin

{{trans|FreeBASIC}}

```scala
//  version 1.1.4

fun isPrime(n: Int) : Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}

fun reverseNumber(n: Int) : Int {
    if (n < 10) return n
    var sum = 0
    var nn = n
    while (nn > 0) {
        sum = 10 * sum + nn % 10
        nn /= 10
    }
    return sum
}

fun isEmirp(n: Int) : Boolean {
    if (!isPrime(n)) return false
    val reversed = reverseNumber(n)
    return reversed != n && isPrime(reversed)
}

fun main(args: Array<String>) {
    println("The first 20 Emirp primes are :")
    var count = 0
    var i = 13
    do {
        if (isEmirp(i)) {
            print(i.toString() + " ")
            count++
        }
        i += 2
    }
    while (count < 20)
    println()
    println()
    println("The Emirp primes between 7700 and 8000 are :")
    i = 7701
    do {
        if (isEmirp(i)) print(i.toString() + " ")
        i += 2
    }
    while (i < 8000)
    println()
    println()
    print("The 10,000th Emirp prime is : ")
    i = 13
    count = 0
    do {
        if (isEmirp(i)) count++
        if (count == 10000) break
        i += 2
    }
    while(true)
    print(i)
}
```


{{out}}

```txt

The first 20 Emirp primes are :
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389

The Emirp primes between 7700 and 8000 are :
7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963

The 10,000th Emirp prime is : 948349

```



## Lua


```Lua

function isPrime (n)
    if n < 2 then return false end
    if n < 4 then return true end
    if n % 2 == 0 then return false end
    for d = 3, math.sqrt(n), 2 do
        if n % d == 0 then return false end
    end
    return true
end

function isEmirp (n)
    if not isPrime(n) then return false end
    local rev = tonumber(string.reverse(n))
    if rev == n then return false end
    return isPrime(rev)
end

function emirpGen (mode, a, b)
    local count, n, eString = 0, 0, ""
    if mode == "between" then
        for n = a, b do
            if isEmirp(n) then eString = eString .. n .. " " end
        end
        return eString
    end
    while count < a do
        n = n + 1
        if isEmirp(n) then
            eString = eString .. n .. " "
            count = count + 1
        end
    end
    if mode == "first" then return eString end
    if mode == "Nth" then return n end
end

if #arg > 1 and #arg < 4 then
    print(emirpGen(arg[1], tonumber(arg[2]), tonumber(arg[3])))
else
    print("Wrong number of arguments")
end

```

Command prompt session:

```txt

>lua emirp.lua first 20
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389

>lua emirp.lua between 7700 8000
7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963

>lua emirp.lua Nth 10000
948349

```



## Maple


```Maple
EmirpPrime := proc(n)
    local eprime;
    eprime := parse(StringTools:-Reverse(convert(n,string)));
    if n <> eprime and isprime(n) and isprime(eprime) then
        return n;
    end if;
end proc:
EmirpsList := proc( n )
    local i, values;
    values := Array([]):
    i := 0:
    do
        i := i + 1;
        if EmirpPrime(i) <> NULL then
            ArrayTools:-Append(values, i);
        end if;
    until numelems(values) = n;
    return convert(values,list);
end proc:
EmirpsList(20);
EmirpPrime~([seq(7700..8000)]);
EmirpsList(10000)[-1];

```

{{out}}

```txt
[13, 17, 31, 37, 71, 73, 79, 97, 107, 113, 149, 157, 167, 179, 199, 311, 337, 347, 359, 389]
[7717, 7757, 7817, 7841, 7867, 7879, 7901, 7927, 7949, 7951, 7963]
948349
```



## Mathematica

First a simple helper function

```Mathematica
reverseDigits[n_Integer] := FromDigits@Reverse@IntegerDigits@n
```

A function to test whether n is an emirp prime

```Mathematica
emirpQ[n_Integer] :=
 Block[{rev = reverseDigits@n}, And[n != rev, PrimeQ[rev]]]
```

Note, this test function assumes n is prime. Adding a check to verify n is prime will have
an impact on execution time for finding the mth emirp prime particularly when m is large.

Finally, a function which returns the first emirp prime larger than the supplied argument

```Mathematica
nextEmirp[n_Integer] :=
 NestWhile[NextPrime, NextPrime[n], ! emirpQ[#] &]
```


With these the first 20 emirp primes are computed as:

```Mathematica
Rest@NestList[nextEmirp, 1, 20]
```

{{out}}

```txt

{13,17,31,37,71,73,79,97,107,113,149,157,167,179,199,311,337,347,359,389}
```


The emirp primes betweewn 7700 and 8000 are:

```Mathematica
Rest@NestWhileList[nextEmirp, 7700, # < 8000 &]
```

{{out}}

```txt

{7717,7757,7817,7841,7867,7879,7901,7927,7949,7951,7963,9001}
```


The 10,000th emirp prime is:

```Mathematica
Nest[nextEmirp, 1, 10000]
```

{{out}}

```txt

948349
```



## MATLAB


```Matlab

NN=(1:1:1e6); %Natural numbers between 1 and t
pns=NN(isprime(NN)); %prime numbers
p=fliplr(str2num(fliplr(num2str(pns))));
a=pns(isprime(p)); b=p(isprime(p)); c=a-b;
emirps=NN(a(c~=0));

```

{{out}}

```txt
the first twenty emirps are: emirps(1:20)
ans =

  Columns 1 through 14

    13    17    31    37    71    73    79    97   107   113   149   157   167   179

  Columns 15 through 20

   199   311   337   347   359   389

```


```txt
The emirp primes betweewn 7700 and 8000 are:
emirps(emirps>=7700 & emirps<=8000)
ans =

  Columns 1 through 7

        7717        7757        7817        7841        7867        7879        7901

  Columns 8 through 11

        7927        7949        7951        7963

```


```txt
The 10,000th emirp prime is: emirps(10000)
ans =

      948349

```

=={{header|Modula-2}}==

```modula2
MODULE Emirp;
FROM Conversions IMPORT StrToLong;
FROM FormatString IMPORT FormatString;
FROM LongMath IMPORT sqrt;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE IsPrime(x : LONGINT) : BOOLEAN;
VAR
    i : LONGINT;
    u : LONGREAL;
    v : LONGINT;
BEGIN
    IF x<2 THEN RETURN FALSE END;
    IF x=2 THEN RETURN TRUE END;
    IF x MOD 2 = 0 THEN RETURN FALSE END;

    u := sqrt(FLOAT(x));
    v := TRUNC(u);

    FOR i:=3 TO v BY 2 DO
        IF x MOD i = 0 THEN RETURN FALSE END
    END;

    RETURN TRUE
END IsPrime;

PROCEDURE IsEmirp(x : LONGINT) : BOOLEAN;
VAR
    buf,rev : ARRAY[0..9] OF CHAR;
    i,j : INTEGER;
    y : LONGINT;
BEGIN
    (* Terminate early if the number is even *)
    IF x MOD 2 = 0 THEN RETURN FALSE END;

    (* First convert the input to a string *)
    FormatString("%l", buf, x);

    (* Create a copy of the string revered *)
    j := 0;
    WHILE buf[j] # 0C DO INC(j) END;
    DEC(j);
    i := 0;
    WHILE buf[i] # 0C DO
        rev[i] := buf[j];
        INC(i);
        DEC(j)
    END;
    rev[i] := 0C;

    (* Convert the reversed copy to a number *)
    StrToLong(rev,y);

    (* Terminate early if the number is even *)
    IF y MOD 2 = 0 THEN RETURN FALSE END;

    (* Discard palindromes *)
    IF x=y THEN RETURN FALSE END;

    RETURN IsPrime(x) AND IsPrime(y)
END IsEmirp;

VAR
    buf : ARRAY[0..63] OF CHAR;
    x,count : LONGINT;
BEGIN
    count := 0;
    x := 1;

    WriteString("First 20 emirps:");
    WriteLn;
    WHILE count<20 DO
        IF IsEmirp(x) THEN
            INC(count);
            FormatString("%l ", buf, x);
            WriteString(buf)
        END;
        INC(x)
    END;
    WriteLn;

    WriteString("Emirps between 7700 and 8000:");
    WriteLn;
    FOR x:=7700 TO 8000 DO
        IF IsEmirp(x) THEN
            FormatString("%l ", buf, x);
            WriteString(buf)
        END
    END;
    WriteLn;

    WriteString("10,000th emirp:");
    WriteLn;
    count := 0;
    x := 1;
    WHILE count<10000 DO
        IF IsEmirp(x) THEN
            INC(count);
        END;
        INC(x)
    END;
    FormatString("%l ", buf, x-1);
    WriteString(buf);
    WriteLn;

    ReadChar
END Emirp.
```



## Oforth


Using isPrime function of Primality by trial division task :


```Oforth
: isEmirp(n)
   n isPrime ifFalse: [ false return ]
   n asString reverse asInteger dup n == ifTrue: [ drop false ] else: [ isPrime ] ;

: main(min, max, length)
| l |
   ListBuffer new ->l
   min while(l size length < ) [
      dup max > ifTrue: [ break ]
      dup isEmirp ifTrue: [ dup l add ] 1 +
      ]
   drop l ;
```


{{out}}

```txt

>main(2, 9999999, 20) println
[13, 17, 31, 37, 71, 73, 79, 97, 107, 113, 149, 157, 167, 179, 199, 311, 337, 347, 359, 389]

>main(7700, 8000, 300) println
[7717, 7757, 7817, 7841, 7867, 7879, 7901, 7927, 7949, 7951, 7963]

>main(2, 9999999999, 10000) last println
948349

```



## PARI/GP


```parigp
rev(n)=subst(Polrev(digits(n)),'x,10);
emirp(n)=my(r=rev(n)); isprime(r) && isprime(n) && n!=r
select(emirp, primes(100))[1..20]
select(emirp, primes([7700,8000]))
s=10000; forprime(p=2,,if(emirp(p) && s--==0, return(p)))
```

{{out}}

```txt
%1 = [13, 17, 31, 37, 71, 73, 79, 97, 107, 113, 149, 157, 167, 179, 199, 311, 337, 347, 359, 389]
%2 = [7717, 7757, 7817, 7841, 7867, 7879, 7901, 7927, 7949, 7951, 7963]
%3 = 948349
```



## Pascal

{{libheader|primTrial}}
using trial division unit , but jumping over number ranges, where the reversed numbers can't be a prime.
Compiles with Delphi and Free Pascal.

```pascal
program Emirp;
//palindrome prime 13 <-> 31
{$IFDEF FPC}
  {$MODE DELPHI}
  {$OPTIMIZATION ON}
  {$OPTIMIZATION REGVAR}
  {$OPTIMIZATION PEEPHOLE}
  {$OPTIMIZATION CSE}
  {$OPTIMIZATION ASMCSE}
  {$Smartlink ON}
  {$CODEALIGN proc=32}
{$ELSE}
  {$APPLICATION CONSOLE}
{$ENDIF}
uses
  primtrial,sysutils; //IntToStr
const
  helptext : array[0..5] of string =
     ('  usage ',
      '  t     -> test of functions',
      '  b l u -> Emirps betwenn l,u       b 7700 8000',
      '  c n   -> count of Emirps up to n  c 99999',
      '  f n   -> output n first Emirp     f 20',
      '  n     -> output the n.th Emirps   10000');

  StepToNextPrimeEnd : Array[0..9] of byte =
              (1,0,3,0,7,7,7,0,9,0);

  base = 10;

var
  s: AnsiString;
  pow,
  powLen  : NativeUint;

procedure OutputHelp;
var
  i : NativeUint;
Begin
  For i := Low(helptext) to High(helptext) do
    writeln(helptext[i]);
  writeln;
end;

function GetNumber(const s: string;var n:NativeUint):boolean;
var
  ErrCode: Word;
Begin
  val(s,n,Errcode);
  result := ErrCode = 0;
end;

procedure RvsStr(var s: AnsiString);
var
  i, j: NativeUint;
  swapChar : Ansichar;
Begin
  i := 1;
  j := Length(s);
  While j>i do Begin
    swapChar:= s[i];s[i] := s[j];s[j] := swapChar;
    inc(i);dec(j) end;
end;

function RvsNumL(var n: NativeUint):NativeUint;
//reverse and last digit
var
  q, c: NativeUint;
Begin
  result := n;
  q := 0;
  repeat
    c:= result div Base;
    q := q*Base+(result-c*Base);
    result := c;
  until result < Base;
  n := q*Base+result;

end;

procedure InitP(var p: NativeUint);
Begin
  powLen := 2;
  pow := Base;
  InitPrime;
  repeat p :=NextPrime until p >= 11;
end;

function isEmirp(p: NativeUint):boolean;
var
  rvsp: NativeUint;
Begin
  s := IntToStr(p);
  result := StepToNextPrimeEnd[Ord(s[1])-48] = 0;
  IF result then
  Begin
    RvsStr(s);
    rvsp := StrToInt(s);
    result := false;
    IF rvsp<>p then
      result := isPrime(rvsp);
  end;
end;

function NextEmirp:NativeUint;
var
 r,Ldgt: NativeUint;
Begin
  result:= NextPrime;
  repeat
    r := result;
    //reverse
    Ldgt := RvsNumL(r);
    Ldgt := StepToNextPrimeEnd[Ldgt];
    IF Ldgt = 0 then
    Begin
      IF r<>result then
        IF isPrime(r) then
          EXIT;
      result:= NextPrime;
    end
    else
    Begin
      while actPrime > pow*Base do
      Begin
        inc(PowLen);
        pow := pow*base;
      end;
      result := Ldgt*pow;
      result := PrimeGELimit(result);
    end;
  until false;
end;

function GetIthEmirp(i: NativeUint):NativeUint;
var
  p : NativeUint;
Begin
  InitP(p);
  Repeat
    dec(i);
    p:= NextEmirp;
  until i = 0;
  result := p;
end;

procedure nFirstEmirp(n: NativeUint);
var
  p : NativeUint;

Begin
  InitP(p);
  Writeln('the first ',n,' Emirp primE: ');
  Repeat
    dec(n);
    p:= NextEmirp;
    write(p,' ');
  until n = 0;
  Writeln;
end;

function CntToLimit(n: NativeUint):NativeUint;
var
  p,cnt : NativeUint;
Begin
  cnt := 0;
  InitP(p);
  p:= NextEmirp;
  While p <= n do
  Begin
    inc(cnt);
    p:= NextEmirp;
  end;
  result := cnt;
end;

procedure InRange(l,u:NativeUint);
var
  p : NativeUint;
  b : boolean;
Begin
  InitP(p);
  IF l > u then Begin p:=l;l:=u;u:=p end;
  Writeln('Emirp primes between ',l,' and ',u,' : ');
  p := PrimeGELimit(l);

  b := IsEmirp(p);
  if b then
    write(p,' ');
  p:= NextEmirp;
  IF (p> u) AND NOT b  then
    Writeln('none')
  else
  Begin
    while p < u do
      Begin
      write(p,' ');
      p:= NextEmirp;
    end;
    Writeln;
  end;
end;

var
  i,u: NativeUint;
  select : char;
Begin
  IF paramcount >= 1 then
    select := Lowercase(paramstr(1)[1]);
  case paramcount of
  1: Begin
       if select='t' then
       Begin
         nFirstEmirp(20);
         InRange(7700,8000);
         Writeln('the ',10000,'.th Emirp prime: ',GetIthEmirp(10000));
         writeln(CntToLimit(9999),' Emirp primes up to ',9999);
         // as a gag
         InRange(400000000,700000000);
       end
       else
         IF GetNumber(paramstr(1),i) then
           Writeln('the ',i,'.th Emirp prime: ',GetIthEmirp(i))
         else
           OutPutHelp;
     end;
  2: Begin
       case select of
       'c': If GetNumber(paramstr(2),i) then
              writeln(CntToLimit(i),' Eemirp primes up to ',i)
            else
              OutPutHelp;
       'f': If GetNumber(paramstr(2),i) then
              nFirstEmirp(i)
            else
              OutPutHelp;
        else
          OutPutHelp;
        end;
     end;
  3: IF (select ='b') AND
        GetNumber(paramstr(2),i) AND GetNumber(paramstr(3),u) Then
       InRange(i,u)
     else
        OutPutHelp;
  else
    OutPutHelp;
  end;
End.
```

;output:

```txt

./Emirp t
the first 20 Emirp primE:
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
Emirp primes between 7700 and 8000 :
7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
the 10000.th Emirp prime: 948349
240 Emirp primes up to 9999
Emirp primes between 400000000 and 700000000 :
none

real    0m0.033s
...
a little "stress test"
Emirp primes between 300000000 and 400000000 :
   1058667
rumtime for this: 2m 3 secs

```


### Using static sieve

is much faster. Only Counting Emirps. http://rosettacode.org/wiki/Extensible_prime_generator#Pascal
It would be nice, if someone could check the results.Like F# today did
;output:

```txt

Count Emirps
               Emirp         Total
Decimals       Count         Count
       2           8             8
       3          28            36
       4         204           240
       5        1406          1646
       6        9538         11184
       7       70474         81658
       8      535578        617236
       9     4192024       4809260
      10    33619380      38428640
      11   274890232     313318872
```



## Perl

{{libheader|ntheory}}

```perl
use feature 'say';
use ntheory qw(forprimes is_prime);

# Return the first $count emirps using expanding segments.
# Can efficiently generate millions of emirps.
sub emirp_list {
  my $count = shift;
  my($i, $inc, @n) = (13, 100+10*$count);
  while (@n < $count) {
    forprimes {
      push @n, $_ if is_prime(reverse $_) && $_ ne reverse($_);
    } $i, $i+$inc-1;
    ($i, $inc) = ($i+$inc, int($inc * 1.03) + 1000);
  }
  splice @n, $count;  # Trim off excess emirps
  @n;
}

say "First 20: ", join " ", emirp_list(20);
print "Between 7700 and 8000:";
forprimes { print " $_" if is_prime(reverse $_) && $_ ne reverse($_) } 7700,8000;
print "\n";
say "The 10_000'th emirp: ", (emirp_list(10000))[-1];
```

{{out}}

```txt
First 20: 13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
Between 7700 and 8000: 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
The 10_000'th emirp: 948349
```



## Perl 6

{{Works with|Rakudo|2018.10}}
For better performance, build the lazy list using module <code>Math::Primesieve</code>, not the built-in, then display results based on parameters passed in. The default is to display an array slice starting and stopping at the given indices. Alternately, ask for all values between two endpoints.


```perl6
use Math::Primesieve;

sub prime-hash (Int $max) {
    my $sieve = Math::Primesieve.new;
    my @primes = $sieve.primes($max);
    @primes.Set;
}

sub MAIN ($start, $stop = Nil, $display = <slice>) {
    my $end = $stop // $start;
    my %primes = prime-hash(100*$end);
    my @emirps = lazy gather for 1 .. * -> $n {
        take $n if %primes{$n} and %primes{$n.flip} and $n != $n.flip
    }

    given $display {
        when 'slice'  { return @emirps[$start-1 .. $end-1] };
        when 'values' {
            my @values = gather for @emirps {
                .take if $start < $_ < $end;
                last if $_> $end
            }
            return @values
        }
    }
}
```

{{out}}
Run with passed parameters: 1 20

('slice' is the default. you <i>could</i> pass it in, but it isn't necessary.)

```txt
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
```

Run with passed parameters: 7700 8000 values

```txt
7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
```

Run with passed parameter: 10000

```txt
948349
```



## Phix

Using [[Extensible_prime_generator#Phix]], not that this task makes trial division onerous.

Does not assume anywhere that some pre-guessed value will be enough.

```Phix
sequence primes = {2,3,5,7}
atom sieved = 10

procedure add_block()
integer N = min((sieved-1)*sieved,400000)
sequence sieve = repeat(1,N)    -- sieve[i] is really i+sieved
    for i=2 to length(primes) do -- (evens filtered on output)
        atom p = primes[i], p2 = p*p
        if p2>sieved+N then exit end if
        if p2<sieved+1 then
            p2 += ceil((sieved+1-p2)/p)*p
        end if
        p2 -= sieved
        if and_bits(p2,1)=0 then p2 += p end if
        for k=p2 to N by p*2 do
            sieve[k] = 0
        end for
    end for
    for i=1 to N by 2 do
        if sieve[i] then
            primes &= i+sieved
        end if
    end for
    sieved += N
end procedure

function is_prime(integer n)
    while sieved<n do
        add_block()
    end while
    return binary_search(n,primes)>0
end function

sequence emirps = {}

function rev(integer n)
integer res = 0
    while n do
        res = res*10+remainder(n,10)
        n = floor(n/10)
    end while
    return res
end function

function emirp(integer n)
    if is_prime(n) then
        integer r = rev(n)
        if r!=n and is_prime(r) then
            return 1
        end if
    end if
    return 0
end function

procedure usage()
    printf(1,"use a single command line argument, with no spaces, eg \"1-20\" (first 20), \n")
    printf(1,"\"7700..8000\" (between 7700 and 8000), or \"10000\" (the 10,000th).\n")
    {} = wait_key()
    abort(0)
end procedure

procedure main(string arg3)
sequence args
integer n,m
    if find('-',arg3) then      -- nth to mth emirp range
        args = scanf(arg3,"%d-%d")
        if length(args)!=1 then usage() end if
        {{n,m}} = args
        integer k = 1
        while length(emirps)<m do
            if emirp(k) then emirps &= k end if
            k += 1
        end while
        printf(1,"emirps %d to %d: ",{n,m})
        ?emirps[n..m]
    elsif match("..",arg3) then -- emirps between n amd m
        args = scanf(arg3,"%d..%d")
        if length(args)!=1 then usage() end if
        {{n,m}} = args
        integer k = 1
        while length(emirps)=0 or emirps[$]<m do
            if emirp(k) then emirps &= k end if
            k += 1
        end while
        sequence s = {}
        for i=1 to length(emirps) do
            if emirps[i]>n then
                for j=i to length(emirps) do
                    if emirps[j]>m then
                        printf(1,"emirps between %d and %d: ",{n,m})
                        ?emirps[i..j-1]
                        exit
                    end if
                end for
                exit
            end if
        end for
    else                        -- nth emirp
        args = scanf(arg3,"%d")
        if length(args)!=1 then usage() end if
        {{n}} = args
        integer k = 1
        while length(emirps)<n do
            if emirp(k) then emirps &= k end if
            k += 1
        end while
        printf(1,"emirp %d: ",{n})
        ?emirps[n]
    end if
end procedure

sequence cl = command_line()
    if length(cl)=2 then
        main("1-20")
        main("7700..8000")
        main("10000")
    elsif length(cl)!=3 then
        usage()
    else
        main(cl[3])
    end if
    {} = wait_key()
```

{{Out}}

```txt

emirps 1 to 20: {13,17,31,37,71,73,79,97,107,113,149,157,167,179,199,311,337,347,359,389}
emirps between 7700 and 8000: {7717,7757,7817,7841,7867,7879,7901,7927,7949,7951,7963}
emirp 10000: 938033

```



## PicoLisp


```PicoLisp
(de prime? (N)
   (and
      (bit? 1 N)
      (let S (sqrt N)
         (for (D 3  T  (+ D 2))
            (T (> D S) N)
            (T (=0 (% N D)) NIL) ) ) ) )
(de palindr? (A)
   (and
      (<>
         (setq A (chop A))
         (setq @@ (reverse A)) )
      (format @@) ) )
(de emirp? (N)
   (and (palindr? N) (prime? @) (prime? N)) )
(de take1 (N)
   (let I 11
      (make
         (for (X 1 (>= 20 X))
            (and
               (emirp? (inc 'I 2))
               (link @)
               (inc 'X) ) ) ) ) )
(de take2 (NIL)
   (make
      (for (I 7701 (> 8000 I) (+ I 2))
         (and (emirp? I) (link @)) ) ) )
(de take3 (NIL)
   (let I 11
      (for (X 1 (>= 10000 X))
         (and (emirp? (inc 'I 2)) (inc 'X)) )
      I ) )

(println (take1 20))
(println (take2))
(println (take3))
```

{{out}}

```txt
(13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389)
(7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963)
948349
```



## PL/I


```pli
*process or(!);
 pt1: Proc(run) Options(main);
 /*********************************************************************
 * 25.03.2014 Walter Pachl
 * Note: Prime number computations are extended as needed
 *********************************************************************/
 Dcl debug Bit(1) Init('0'b);
 Dcl run Char(100) Var;
 Dcl primes(200000) Bin Fixed(31) Init(2,3,5,7,11,13,17,(200000-7)0);
 Dcl nn Bin Fixed(31) Init(0);
 Dcl np Bin Fixed(31) Init(7);
 Dcl hp Bin Fixed(31) Init(17);
 Dcl ip Bin Fixed(31);
 Dcl (p,r) Bin Fixed(31);
 Put Edit('run=',run,'<')(Skip,a,a,a);
 np=7;
 call cprimes(20,1,'A');

 main_loop:
 Do ip=1 To 100000;                    /* loop over all primes       */
   p=primes(ip);                       /* candidate                  */
   If p=0 Then
     call cprimes(20,hp+1,'.');
   p=primes(ip);                       /* candidate                  */
   r=rev(p);                           /* reversed candidate         */
   If p=r Then;                        /* skip palindromic prime     */
   Else Do;                            /* p is eligible              */
     If is_prime(r) Then Do;           /* reversed p is a prime      */
       nn=nn+1;                        /* increment number of hits   */
       Select;
         When(run<='1') Do;
           If nn<21 Then Call show_1;  /* call appropriate output    */
           If nn=20 Then
             Leave main_loop;
           End;
         When(run='2') Do;
           If hp<8000 Then
             Call cprimes(1,8000,'B');
           If 7700<p & p<8000 Then Call show_2;
           If p>8000 Then
             Leave main_loop;
           End;
         When(run='3') Do;
           If np<10000 Then
             Call cprimes(10000,1,'C');
           If nn=10000 Then Do;
             Call show_3;
             Leave main_loop;
             End;
           End;
         Otherwise Do;
           Put skip list('Invoke as pt1 1/2/3');
           Return;
           End;
         End;
       End;
     End;
   End;

 show_1: Proc;
 Dcl first Bit(1) Static Init('1'b);
 If first Then Do;
   Put Edit('the first 20 emirps:')(Skip,a);
   first='0'b;
   Put Skip;
   End;
 If nn=11 Then
   Put Skip;
 Put Edit(p)(F(4));
 End;

 show_2: Proc;
 Dcl first Bit(1) Static Init('1'b);
 If first Then Do;
   Put Edit('emirps between 7700 and 8000:')(Skip,a);
   first='0'b;
   Put Skip;
   End;
 Put Edit(p)(F(5));
 End;

 show_3: Proc;
 Dcl first Bit(1) Static Init('1'b);
 If first Then Do;
   Put Edit('the 10000th emirp:')(Skip,a);
   first='0'b;
   Put Skip;
   End;
 Put Edit(p)(F(6));
 End;

 cprimes: Proc(num,mp,s);
 /*********************************************************************
 * Fill the array primes with prime numbers
 * so that it contains at least num primes and all primes<=mp
 *********************************************************************/
 dcl o Char(60) Var;
 If debug Then
   Put String(o) Edit('cprimes: ',s,np,hp)(a,a,2(f(6)));
 Dcl num Bin Fixed(31);                /* number of primes needed    */
 Dcl mp  Bin Fixed(31);                /* max prime must be > mp     */
 Dcl p   Bin Fixed(31);                /* candidate for next prime   */
 Dcl s   Char(1);                      /* place of invocation        */
 loop:
 Do p=hp+2 By 2 Until(np>=num & hp>mp); /* only odd numbers are elig.*/
   If mod(p, 3)=0 Then Iterate;
   If mod(p, 5)=0 Then Iterate;
   If mod(p, 7)=0 Then Iterate;
   If mod(p,11)=0 Then Iterate;
   If mod(p,13)=0 Then Iterate;
   Do k=7 By 1 While(primes(k)**2<=p);
     If mod(p,primes(k))=0 Then
       Iterate loop;
     End;
   np=np+1;
   primes(np)=p;
   hp=p;
   End;
  If debug Then
    Put Edit(o,' -> ',np,hp)(Skip,a,a,2(f(6)));
  End;

 rev: Proc(x) Returns(Bin Fixed(31));
 /*********************************************************************
 * reverse the given number
 *********************************************************************/
 Dcl x Bin Fixed(31);
 Dcl p Pic'ZZZZZZ9';
 Dcl qq Char(7) Init('');
 Dcl q Pic'ZZZZZZ9' based(addr(qq));
 Dcl v Char(8) Var;
 p=x;
 v=trim(p);
 v=reverse(v);
 substr(qq,8-length(v))=v;
 Return(q);
 End;

 is_prime: Proc(x) Returns(Bit(1));
 /*********************************************************************
 * check if x is a prime number (binary search in primes)
 *********************************************************************/
 Dcl x  Bin Fixed(31);
 Dcl lo Bin Fixed(31) Init(1);
 Dcl hi Bin Fixed(31);
 Dcl m  Bin Fixed(31);
 If x>hp Then Do;                 /* x is outside of range in primes */
   If debug Then
     Put Edit('is_prime x=',x,'hp=',hp)(Skip,2(a,f(8),x(1)));
   Call cprimes(1,x,'D');         /* extend range of primes          */
   End;
 hi=np;
 Do While(lo<=hi);                /* lookup                          */
   m=(lo+hi)/2;
   Select;
     When        (x=primes(m))   Return('1'b); /* x is a prime number*/
     When        (x<primes(m))   hi=m-1;
     Otherwise /* x>primes(m) */ lo=m+1;
     End;
   End;
 Return('0'b);                    /* x is not a prime number         */
 End;

 End;
```


{{out}}

```txt
run=1 <
the first 20 emirps:
  13  17  31  37  71  73  79  97 107 113
 149 157 167 179 199 311 337 347 359 389

run=2 <
emirps between 7700 and 8000:
 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963

run=3 <
the 10000th emirp:
948349
```



## Python

This uses [[Prime_decomposition#Python:_Using_Croft_Spiral_sieve]] and so the prime number generator self-extends to generate ever larger primes automatically.

There is no explicit hard-coded ceiling added to the code for the prime generator, which is the reason given for the need to invoke a program three times in the task description.

```python
from __future__ import print_function
from prime_decomposition import primes, is_prime
from heapq import *
from itertools import islice

def emirp():
    largest = set()
    emirps = []
    heapify(emirps)
    for pr in primes():
        while emirps and pr > emirps[0]:
            yield heappop(emirps)
        if pr in largest:
            yield pr
        else:
            rp = int(str(pr)[::-1])
            if rp > pr and is_prime(rp):
                heappush(emirps, pr)
                largest.add(rp)

print('First 20:\n  ', list(islice(emirp(), 20)))
print('Between 7700 and 8000:\n  [', end='')
for pr in emirp():
    if pr >= 8000: break
    if pr >= 7700: print(pr, end=', ')
print(']')
print('10000th:\n  ', list(islice(emirp(), 10000-1, 10000)))
```


{{out}}

```txt
First 20:
   [13, 17, 31, 37, 71, 73, 79, 97, 107, 113, 149, 157, 167, 179, 199, 311, 337, 347, 359, 389]
Between 7700 and 8000:
  [7717, 7757, 7817, 7841, 7867, 7879, 7901, 7927, 7949, 7951, 7963, ]
10000th:
   [948349]
```



## Racket

This implementation seems to have exploded somewhat due to
* the need to "account" for the greatest tested prime
* the need to reset memory between runs
* the need for a main (to support the above)
* and a (possibly misguided) thought that performance might be a consideration
  (my naive version finds the 10,0000th in ... ms)

So there are two versions presented below. The first is minimalist, providing
basic functions, unburdened by accounting or (too many) performance
considerations (please don't mark this as needing attention... I know it falls short of

```racket
#lang racket
(require math/number-theory)

(define (stigid n)
  (define (inr n a) (if (= 0 n) a (inr (quotient n 10) (+ (* 10 a) (modulo n 10)))))
  (inr n 0))

(define (emirp-prime? n)
  (define u (stigid n))
  (and (not (= u n)) (prime? n) (prime? u)))

(printf "\"show the first twenty emirps.\"~%")
(for/list ((n (sequence-filter emirp-prime? (in-range 11 +Inf.0 2))) (_ (in-range 20))) n)

(printf "\"show all emirps between 7,700 and 8,000\"~%")
(for/list ((n (sequence-filter emirp-prime? (in-range 7701 8000 2)))) n)

(printf "\"show the 10,000th emirp\"~%")
(let loop ((i 10000) (p 9))
  (define p+2 (+ p 2))
  (cond [(not (emirp-prime? p+2)) (loop i p+2)] [(= i 1) p+2] [else (loop (- i 1) p+2)]))
```


The second is somewhat larger and seems to be a playground for all sorts of code.

```racket
#lang racket
;; ---------------------------------------------------------------------------------------------------
;; There are two distinct requirements here...
;; 1. to test for emirp-primality - this can be done as easily as testing for primality.
;;    We use math/number-theory's "prime?" for this, which has no bounds
;; 2. to find the nth emirp-prime. Even when were doing this with normal primes, we wouldn't test
;;    each number; rather sieve them. Prime sieves by their very nature are at least memory bound...
;;    so I'm happy in this case that they are kept within the bounds of "fixnum" integers. Once we
;;    accept that, we can use the unsafe-ops on fixnums which allow for a performance boost. The
;;    fixnum / sieve code is after this simpler stuff.
;; ---------------------------------------------------------------------------------------------------
(require math/number-theory)

;; this slows things down, having to unbox, test and rebox the m.p.g -- but the task asks for some
;; accounting to be performed, so account we do!
(define max-prime-tested (box 0))

(define (report-mpg)
  (printf "Max prime tested (using math/number-theory): ~a~%" (unbox max-prime-tested)))

(define (prime?/remember-max n)
  (define rv (prime? n))
  (when (and rv (> n (unbox max-prime-tested))) (set-box! max-prime-tested n))
  rv)

(define (stigid n)
  (define (inner-stigid n a) (if (= 0 n) a (inner-stigid (quotient n 10) (+ (* 10 a) (modulo n 10)))))
  (inner-stigid n 0))

(define (emirp-prime? n)
  (define u (stigid n))
  (and (not (= u n)) (prime?/remember-max n) (prime?/remember-max u)))

;; ---------------------------------------------------------------------------------------------------
(require
  racket/require
  (except-in
   (filtered-in (lambda (n) (regexp-replace #rx"unsafe-" n "")) racket/unsafe/ops) unbox set-box!))

;; NB using fixnum below limits stigid to "fixnum" (about 2^60) range of numbers
;; but, unleashed, unsafe-fx... are fast
(define (fxstigid n)
  (define (inner-fxstigid n a)
    (if (fx= 0 n) a (inner-fxstigid (fxquotient n 10) (fx+ (fx* 10 a) (fxmodulo n 10)))))
  (inner-fxstigid n 0))

;; Grows the sieve to n (so n is included in the sieve)
;; Values in the sieve are: = 0 - known non-prime
;;                          > 0 - known prime
;; The new sieve does not alter non-zero values in the old sieve; to preserve cachceing of e.g. emirps
;; Always returns a copy (so it is caller responsibility to determine the necessity of this function)
(define (extend-prime-sieve sieve n)
  (define sieve-size (bytes-length sieve))
  (define sieve-size+ (fx+ 1 n))
  (define new-sieve (make-bytes sieve-size+ 1))
  (bytes-copy! new-sieve 0 sieve 0 (fxmin sieve-size+ sieve-size))
  (for* ((f (in-range 2 (add1 (integer-sqrt sieve-size+))))
         #:unless (fx= (bytes-ref new-sieve f) 0) ; the only case of non-prime
         (f+ (in-range (fx* f (fxmax 2 (fxquotient sieve-size f))) sieve-size+ f)))
    (bytes-set! new-sieve f+ 0))
  (values sieve-size+ new-sieve))

;; task three *needs* a sieve to operate sub-second:
;; values in sieve are:
;;   0 - known non-prime
;;   1 - known prime, unknown emirp-ality (freshly generated from extend-prime-sieve)
;;   2 - known prime, known non-emirp -- needed for sieve extension
;;   3 - known emirp (and .: known prime)
(define-values
  (emirp-prime?/sieve reset-sieve! report-mpg/sieved extend-sieve!)
  (let [(sieve-size 2) (the-sieve (bytes 0 0))]
    (define (extend-sieve! n)
      (when (fx>= n sieve-size)
        (define-values (sieve-size+ new-sieve) (extend-prime-sieve the-sieve n))
        (set! the-sieve new-sieve) (set! sieve-size sieve-size+)))
    (values
     (lambda (n)
       (extend-sieve! n)
       (case (bytes-ref the-sieve n)
         [(0) #f] ; it's not even prime
         [(1) ; it's a prime... but is is emirp?
          (define u (fxstigid n))
          (define new-sieve-n
            (cond
              [(fx= u n) 2]
              [(fx> u n) (if (emirp-prime?/sieve u) 3 2)]
              [(fx= (bytes-ref the-sieve u) 1) 3]
              [else 2]))
          (bytes-set! the-sieve n new-sieve-n)
          (fx= new-sieve-n 3)]
         [(2) #f] ; we know it's not emirp
         [(3) #t])) ; we already knew it's an emirp
     (lambda () (set! sieve-size 2) (set! the-sieve (bytes 0 0)))
     (lambda () (printf "Sieve size: ~a~%Max prime generated (sieve): ~a~%" sieve-size
                        (for/last ((n the-sieve) (p (in-naturals)) #:unless (fx= 0 n)) p)))
     extend-sieve!)))

;; ---------------------------------------------------------------------------------------------------
;; testing *-primality is a lot cheaper than generating, and we'll use math/number-theory to do
;; this... it's fast enough. Because they cannot be palindromic and because 2 is the only even prime
;; (and is palindromic), all emirps are odd - hence our sequences starting with an odd (>= 11),
;; stepping by 2.
(define (task1 (emirp?-test emirp-prime?))
  (printf "\"show the first twenty emirps.\" [~s]~%" emirp?-test)
  (for/list ((n (sequence-filter emirp?-test (in-range 11 +Inf.0 2))) (_ (in-range 20))) n))

(define (task2 (emirp?-test emirp-prime?))
  (printf "\"show all emirps between 7,700 and 8,000\" [~s]~%" emirp?-test)
  (for/list ((n (sequence-filter emirp?-test (in-range 7701 8000 2)))) n))

(define (task3 (emirp?-test emirp-prime?) (extend-sieve-fn #f))
  (printf "\"show the 10,000th emirp\" [~s]~%" emirp?-test)
  (when extend-sieve-fn
    (extend-sieve-fn (nth-prime 10000))) ; at a guess, the 10000th emirp will be > the 10000th prime
  (let loop ((i 10000) (p 9))
    (define p+2 (fx+ p 2))
    (cond [(not (emirp?-test p+2)) (loop i p+2)] [(fx= i 1) p+2] [else (loop (fx- i 1) p+2)])))

;; -| MAIN |------------------------------------------------------------------------------------------
(provide main)
(define (main task)
  ;; to avoid the *necessity* of calling from the command line multiple times, we reset the sieve on
  ;; each invocation of main
  (reset-sieve!)
  (set-box! max-prime-tested 0)
  (match task
    ["1" (displayln (task1)) (report-mpg)]
    ["2" (displayln (task2)) (report-mpg)]
    ["3" (displayln (task3 emirp-prime?/sieve extend-sieve!)) (report-mpg/sieved)]))

;; -| TESTS |-----------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  (check-false (emirp-prime?/sieve 12))
  (check-false (emirp-prime?/sieve 23))
  (check-true (emirp-prime?/sieve 13))
  (check-equal?
   (for/list
       ((n (sequence-filter emirp-prime?/sieve (in-range 11 100000 2)))
        (_ (in-range 3))) n)
   '(13 17 31))
  (check-equal? (time (task1 emirp-prime?/sieve)) (time (task1)))
  (check-equal? (time (task2 emirp-prime?/sieve)) (time (task2)))
  (check-equal? (time (task3 emirp-prime?/sieve extend-sieve!)) (time (task3))))

```


{{out}}

```txt
"show the first twenty emirps."
'(13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389)
"show all emirps between 7,700 and 8,000"
'(7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963)
"show the 10,000th emirp"
948349
```


Second program, run from Linux bash shell:

```txt
$ for i in 1 2 3; do racket -t Emirp-primes.rkt -m $i; echo; done
"show the first twenty emirps." [#<procedure:emirp-prime?>]
(13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389)
Max prime tested (using math/number-theory): 991

"show all emirps between 7,700 and 8,000" [#<procedure:emirp-prime?>]
(7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963)
Max prime tested (using math/number-theory): 9787

"show the 10,000th emirp" [#<procedure:...Emirp-primes.rkt:77:5>]
948349
Sieve size: 999998
```



## REXX


### version 1

Specifications of arguments note:   The following REXX program accepts:
:*   a single number       '''N''',   indicates to display the   '''N'''<sup>th</sup>   emirp prime
:*   two numbers     '''N     M''',   indicates to display the   '''N'''<sup>th</sup>   ──►   '''M'''<sup>th</sup>   emirp primes.
:*   two numbers     '''N   -M''',   indicates to display the emirp primes between   '''N'''   and  '''<big><big><b>│</b></big></big>M<big><big><b>│</b></big></big>'''   (inclusive).

Programming note:   the trial division method of generating (regular) primes is a bit on the slow side, so some

memoization was added (assisting with the <big>√{{overline| j }}</big>),   and some of the trial divisions were hard-coded to minimize

the CPU time a bit.

```rexx
/*REXX program finds  emirp  primes (base 10):  when a prime reversed, is another prime.*/
parse arg x y .                                  /*obtain optional arguments from the CL*/
if x=='' | x==","  then do;  x=1;  y=20;  end    /*Not specified?  Then use the default.*/
if y==''  then y=x                               /* "      "         "   "   "     "    */
r=y<0;    y=abs(y)                               /*display a  range  of  emirp primes ? */
rly=length(y) + \r                               /*adjusted length of the  Y  value.    */
!.=0;  c=0;   _=2 3 5 7 11 13 17;   $=           /*isP; emirp count; low primes; emirps.*/
    do #=1  for words(_);   p=word(_,#);   @.#=p;    !.p=1;    end  /*#*/
#=#-1;   ip=#;  s.#=@.#**2                       /*adjust # (for the DO loop);  last P².*/
                            /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒ [↓]   generate more primes within range.   */
    do j=@.#+2  by 2                             /*only find  odd  primes from here on. */
    if length(#)>rly  then leave                 /*have we enough primes for emirps?    */
    if j//3      ==0  then iterate               /*is  J  divisible by three?           */
    if right(j,1)==5  then iterate               /*is the right-most digit a "5" ?      */
    if j//7      ==0  then iterate               /*is  J  divisible by seven?           */
    if j//11     ==0  then iterate               /*is  J  divisible by eleven?          */
    if j//13     ==0  then iterate               /*is  J  divisible by thirteen?        */
                                                 /*[↑]  the above five lines saves time.*/
          do k=ip  while  s.k<=j                 /*divide by the known  odd  primes.    */
          if j//@.k==0  then iterate j           /*J divisible by X?  Then ¬prime.   ___*/
          end   /*k*/                            /* [↑]  divide by odd primes up to √ j */
    #=#+1                                        /*bump the number of primes found.     */
    @.#=j;      s.#=j*j;     !.j=1               /*assign to sparse array; prime²; prime*/
    end         /*j*/                            /* [↑]  keep generating until enough.  */
                            /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒ [↓]    filter  emirps  for the display.    */
    do j=6  to @.#;   _=@.j                      /*traipse through the regular primes.  */
    if (r&_>y) | (\r&c==y)  then leave           /*is the prime not within the range?   */
    __=reverse(_)                                /*reverse (digits) of the regular prime*/
    if \!.__   | _==__    then iterate           /*is the  reverse  a different prime ? */
    c=c+1                                        /*bump the emirp prime counter.        */
    if (r&_<x) | (\r&c<x) then iterate           /*is  emirp  not within allowed range? */
    $=$ _                                        /*append prime to the emirpPrime list. */
    end   /*j*/                                  /* [↑]  list:  by value  or  by range. */
                                                 /* [↓]  display the emirp list.        */
say strip($);   say;   n=words($);   ?=(n\==1)   /*display the  emirp primes  wanted.   */
if ?  then say  n   'emirp primes shown.'        /*stick a fork in it,  we're all done. */
```

'''output'''   when using the following for input:   <tt> 1   20 </tt>

```txt

13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389

20 emirp primes shown.

```

'''output'''   when using the following for input:   <tt> 7700   -8000</tt>

```txt

7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963

11 emirp primes shown.

```

'''output'''   when using the following for input:   <tt> 10000 </tt>

```txt

948349

```



### version 2


```rexx
 /*********************************************************************
 * 27.03.2014 Walter Pachl
 *********************************************************************/
 Parse Arg run
 first.=1
 nn=0
 ol=''
 lb='00'x
 If run='' Then run=1
 call cprimes 20,20,'A'
 main_loop:
 Do ip=1 To 1000000                    /* loop over all primes       */
   p=primes.ip                         /* candidate                  */
   If p=0 Then
     call cprimes 20,hp+1,'B'
   p=primes.ip                        /* candidate                  */
   r=reverse(p)                       /* reversed candidate         */
   If p<>r Then Do                    /* not a palindromic prime    */
     If is_prime(r) Then Do           /* reversed p is a prime      */
       nn=nn+1                        /* increment number of hits   */
       Select
         When run<='1' Then Do
           If nn<21 Then Call show 1,'the first 20 emirps:',4
           If nn=20 Then
             Leave
           End
         When(run='2') Then Do
           If hp<8000 Then
             Call cprimes 1,8000,'C'
           If 7700<p & p<8000 Then Call show 2,'emirps between 7700 and 8000:',5
           If p>8000 Then
             Leave
           End
         When(run='3') Then Do
           If nn=10000 Then Do
             Call show 3,'the 10.000th emirp:',6
             Leave
             End
           End
         When(run='4') Then Do
           Call cprimes 1,999999    /* dirty trick to speed thins up */
           If nn=10000 Then Do
             Call show 4,'the 10.000th emirp (alternate version):',6
             Leave
             End
           End
         Otherwise Do
           Say 'Invoke as ptx 1/2/3'
           Exit
           End
         End
       End
     End
  End
 Call oo
 Say 'largest prime:' hp
 Exit

 show:
 Parse Arg task,header,nl
 If first.task Then Do
   Call o header||lb
   first.task=0
   End
 Call o right(p,nl)
 If nn=10 Then
   Call o lb
 Return

cprimes: Procedure Expose primes. psquare. is_prime. nprimes hp
/*********************************************************************
* adapted for my needs from REXX's Extensible prime generation
* Fill the array primes with prime numbers
* so that it contains at least num primes and all primes<=mp
*********************************************************************/
  Parse Arg num,mp
  If symbol('primes.0')=='LIT' Then Do  /* 1st time here? Initialize */
    primes.=0                           /* prime numbers             */
    is_prime.=0                         /* is_prime.x -> x is prime  */
    psquare.=0                          /* psquare.x = square of     */
    plist='2 3 5 7 11 13 17 19 23'      /* knows low primes.         */
    Do i=1 For words(plist)
      p=word(plist,i)
      primes.i=p
      is_prime.p=1
      End
    nprimes=i-1
    primes.0=nprimes+1
    psquare.nprimes=primes.nprimes**2   /* square of this prime      */
    End                             /* [?]  done with building low Ps */
  Do j=primes.nprimes+2 By 2 While nprimes<num | primes.nprimes<mp
    If j//3==0 Then       Iterate
    If right(j,1)==5 Then Iterate
    If j//7==0 Then       Iterate
    If j//11==0 Then      Iterate
    If j//13==0 Then      Iterate
    If j//17==0 Then      Iterate
    If j//19==0 Then      Iterate
    If j//23==0 Then      Iterate
    Do k=primes.0-1 While psquare.k<=j  /* check for other known primes */
      If j//primes.k==0 Then      /* J is divisible by k-th prime    */
        Iterate j                 /* j is not prime                  */
      End
    nprimes=nprimes+1             /* bump number of primes found.    */
    primes.nprimes=j
    psquare.nprimes=j*j
    is_prime.j=1
    hp=j
    End
  Return

 is_prime: Procedure Expose primes. psquare. is_prime. nprimes hp
 /*********************************************************************
 * check if x is a prime number
 *********************************************************************/
 Parse Arg x
 If x>hp Then
   Call cprimes 1,x
 Return is_prime.x

o: ol=ol||arg(1)
   Return
oo: Do While ol<>''
      Parse Var ol l (lb) ol
      Say l
      End
    Return
```

'''output'''
```txt
rexx ptz 1
the first 20 emirps:
  13  17  31  37  71  73  79  97 107 113
 149 157 167 179 199 311 337 347 359 389
largest prime: 991

rexx ptz 2
emirps between 7700 and 8000:
 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
largest prime: 10007

rexx ptz 3
the 10.000th emirp:
948349
largest prime: 1000003

rexx ptz 4 (slightly faster that rexx ptz 3)
the 10.000th emirp (alternate version):
948349
largest prime: 1000003
```



## Ring


```ring

nr = 1
m = 2
see "first 20 :" + nl
while nr < 21
      emirp = isEmirp(m)
      if emirp = 1 see m see " "
         nr++ ok
      m++
end
see nl + nl

nr = 1
m = 7701
see "between 7700 8000 :" + nl
while m > 7700 and m < 8000
      emirp = isEmirp(m)
      if emirp = 1 see m see " " nr++ ok
      m++
end
see nl + nl

nr = 1
m = 2
see "Nth 10000 :" + nl
while nr > 0 and nr < 101
      emirp = isEmirp(m)
      if emirp = 1 nr++ ok
      m++
end
see m + nl

func isEmirp n
     if not isPrime(n) return false ok
     cStr = string(n)
     cstr2 = ""
     for x = len(cStr) to 1 step -1 cStr2 += cStr[x] next
     rev = number(cstr2)
     if rev = n return false ok
     return isPrime(rev)

func isPrime n
     if n < 2 return false ok
     if n < 4 return true ok
     if n % 2 = 0 return false ok
     for d = 3 to sqrt(n) step 2
         if n % d = 0 return false ok
     next
     return true

```



## Ruby


```ruby
require 'prime'

emirp = Enumerator.new do |y|
  Prime.each do |prime|
    rev = prime.to_s.reverse.to_i
    y << prime  if rev.prime? and rev != prime
  end
end

puts "First 20 emirps:", emirp.first(20).join(" ")
puts "Emirps between 7,700 and 8,000:"
emirp.with_index(1) do |prime,i|
  print "#{prime} "  if (7700..8000).cover?(prime)
  if i==10000
    puts "", "10,000th emirp:", prime
    break
  end
end
```


{{out}}

```txt

First 20 emirps:
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
Emirps between 7,700 and 8,000:
7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
10,000th emirp:
948349

```



## Rust

<lang>#![feature(iterator_step_by)]

extern crate primal;

fn is_prime(n: u64) -> bool {
    if n == 2 || n == 3 || n == 5 || n == 7 || n == 11 || n == 13 { return true; }
    if n % 2 == 0 || n % 3 == 0 || n % 5 == 0 || n % 7 == 0 || n % 11 == 0 || n % 13 == 0 { return false; }
    let root = (n as f64).sqrt() as u64 + 1;
    (17..root).step_by(2).all(|i| n % i != 0)
}

fn is_emirp(n: u64) -> bool {
    let mut aux = n;
    let mut rev_prime = 0;
    while aux > 0 {
        rev_prime = rev_prime * 10 + aux  % 10;
        aux /= 10;
    }
    if n == rev_prime { return false; }
    is_prime(rev_prime)
}

fn calculate() -> (Vec<usize>, Vec<usize>, usize) {
    let mut count = 1;
    let mut vec1 = Vec::new();
    let mut vec2 = Vec::new();
    let mut emirp_10_000 = 0;

    for i in primal::Primes::all() {
        if is_emirp(i as u64) {
            if count < 21 { vec1.push(i) }
            if i > 7_700 && i < 8_000 { vec2.push(i) }
            if count == 10_000 {
                emirp_10_000 = i;
                break;
            }
            count += 1;
        }
    }

    (vec1, vec2, emirp_10_000)
}

fn main() {
    let (vec1, vec2, emirp_10_000) = calculate();

    println!("First 20 emirp-s : {:?}", vec1);
    println!("Emirps-s between 7700 and 8000 : {:?}", vec2);
    println!("10.000-th emirp : {}", emirp_10_000);
}
```

{{out}}

```txt

First 20 primes : [13, 17, 31, 37, 71, 73, 79, 97, 107, 113, 149, 157, 167, 179, 199, 311, 337, 347, 359, 389]
Emirps-s between 7700 and 8000 : [7717, 7757, 7817, 7841, 7867, 7879, 7901, 7927, 7949, 7951, 7963]
10.000-th emirp : 948349

real	0m0.040s
user	0m0.036s
sys	0m0.003s

```



## Scala

===Using BigInt's isProbablePrime()===
The isProbablePrime() method performs a Miller-Rabin primality test to within a given certainty.

```scala
def isEmirp( v:Long ) : Boolean = {
 val b = BigInt(v.toLong)
 val r = BigInt(v.toString.reverse.toLong)
 b != r && b.isProbablePrime(16) && r.isProbablePrime(16)
}

// Generate the output
{
  val (a,b1,b2,c) = (20,7700,8000,10000)
  println( "%32s".format(          "First %d emirps: ".format( a )) + Stream.from(2).filter( isEmirp(_) ).take(a).toList.mkString(",") )
  println( "%32s".format( "Emirps between %d and %d: ".format( b1, b2 )) + {for( i <- b1 to b2 if( isEmirp(i) ) ) yield i}.mkString(",") )
  println( "%32s".format(                "%,d emirp: ".format( c )) + Iterator.from(2).filter( isEmirp(_) ).drop(c-1).next )
}
```

{{out}}

```txt
               First 20 emirps: 13,17,31,37,71,73,79,97,107,113,149,157,167,179,199,311,337,347,359,389
  Emirps between 7700 and 8000: 7717,7757,7817,7841,7867,7879,7901,7927,7949,7951,7963
                  10,000 emirp: 948349
```



## Sidef

{{trans|Perl}}

```ruby
func forprimes(a, b, callback) {
    for (var p = a.dec.next_prime; p <= b; p.next_prime!) {
        callback(p)
    }
}

func is_emirp(p) {
    var str = Str(p)
    var rev = str.reverse
    (str != rev) && is_prime(Num(rev))
}

func emirp_list(count) {
    var i = 13
    var inc = (100 + 10*count)
    var n = []
    while (n.len < count) {
        forprimes(i, i+inc - 1, {|p|
            is_emirp(p) && (n << p)
        })
        (i, inc) = (i+inc, int(inc * 1.03) + 1000)
    }
    n.splice(count)
    return n
}

say ("First 20: ", emirp_list(20).join(' '))
say ("Between 7700 and 8000: ", gather {
        forprimes(7700, 8000, {|p| is_emirp(p) && take(p) })
    }.join(' '))
say ("The 10,000'th emirp: ", emirp_list(10000)[-1])
```

{{out}}

```txt

First 20: 13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
Between 7700 and 8000: 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
The 10,000'th emirp: 948349

```


## Smalltalk

Works with Smalltalk/X

This uses a builtin class called LazyCons, which is useful to implement infinite lists.
The code is functional, looking somewhat Scheme'isch (that's what blocks are for).

First an emirp checker:

```smalltalk
isEmirp :=
    [:p | |e|
        (e := p asString reversed asNumber) isPrime
        and:[ e ~= p ]
    ].
```


an infinite list of primes:

```smalltalk
primeGen :=
    [:n |
        LazyCons car:n cdr:[primeGen value:(n nextPrime)]
    ].
```


an infinite list of emirps, taking an infinite list of primes as arg:

```smalltalk
emirpGen :=
    [:l | |rest el|
        rest := l.
        [ el := rest car. rest := rest cdr. isEmirp value:el ] whileFalse.
        LazyCons car:el cdr:[emirpGen value:rest]
    ].
```

two infinite lists:

```smalltalk
listOfPrimes := primeGen value:2.
listOfEmirps := emirpGen value:listOfPrimes.
```

generating output:

```smalltalk
Transcript
    show:'first 20 emirps: ';
    showCR:(listOfEmirps take:20) asArray.

Transcript
    show:'emirps between 7700 and 8000 are: ';
    showCR:((7700 to:8000) select:[:n | n isPrime and:[isEmirp value:n]]).

Transcript
    show:'10000''th emirp: ';
    showCR:(listOfEmirps nth:10000).
```


Generates:
```txt

first 20 emirps: #(13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389)
emirps between 7700 and 8000 are: OrderedCollection(7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963)
10000'th emirp: 948349
```


LazyCons is easily defined as:

```smalltalk
Object subclass: #Cons
    instancevariableNames:'car cdr'.

car:newCar cdr:newCdr
    car := newCar. cdr := newCdr

car
    ^car

cdr
    ^cdr

Cons subclass:#LazyCons

cdr
    cdr := cdr value.
    self changeClassTo:Cons.
    ^cdr
```



## Stata



```stata
emirp 1000
list in 1/20, noobs noh

  +-----+
  |  13 |
  |  17 |
  |  31 |
  |  37 |
  |  71 |
  |-----|
  |  73 |
  |  79 |
  |  97 |
  | 107 |
  | 113 |
  |-----|
  | 149 |
  | 157 |
  | 167 |
  | 179 |
  | 199 |
  |-----|
  | 311 |
  | 337 |
  | 347 |
  | 359 |
  | 389 |
  +-----+

emirp 10000
list if 7700<p & p<8000, noobs noh

  +------+
  | 7717 |
  | 7757 |
  | 7817 |
  | 7841 |
  | 7867 |
  |------|
  | 7879 |
  | 7901 |
  | 7927 |
  | 7949 |
  | 7951 |
  |------|
  | 7963 |
  +------+

emirp 1000000
list if _n==10000, noobs noh

  +--------+
  | 948349 |
  +--------+
```


Now the definition of ''emirp.ado'':


```stata
program emirp
	args n
	qui clear
	qui mata: build(`n')
	qui save temp, replace
	qui replace p=real(strreverse(strofreal(p)))
	qui merge 1:1 p using temp, keep(3) nogen
	qui drop if real(strreverse(strofreal(p)))==p
end

mata
real colvector sieve(real scalar n) {
	real colvector a
	real scalar i,j
	if (n<2) return(J(0,1,.))
	a=J(n,1,1)
	a[1]=0
	for (i=1; i<=n; i++) {
		if (a[i]) {
			j=i*i
			if (j>n) return(select(1::n,a))
			for (; j<=n; j=j+i) a[j]=0
		}
	}
}

function build(n) {
	a=sieve(n)
	st_addobs(rows(a))
	st_addvar("long","p")
	st_store(.,1,a)
}
end
```



## Tcl

{{tcllib|math::numtheory}}

```tcl
package require math::numtheory

# Import only to keep line lengths down
namespace import math::numtheory::isprime
proc emirp? {n} {
    set r [string reverse $n]
    expr {$n != $r && [isprime $n] && [isprime $r]}
}

# Generate the various emirps
for {set n 2;set emirps {}} {[llength $emirps] < 20} {incr n} {
    if {[emirp? $n]} {lappend emirps $n}
}
puts "first20: $emirps"

for {set n 7700;set emirps {}} {$n <= 8000} {incr n} {
    if {[emirp? $n]} {lappend emirps $n}
}
puts "7700-8000: $emirps"

for {set n 2;set ne 0} true {incr n} {
    if {[emirp? $n] && [incr ne] == 10000} break
}
puts "10,000: $n"
```

{{out}}

```txt

first20: 13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389
7700-8000: 7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963
10,000: 948349

```




## VBA



```vb
Option Explicit

Private Const MAX As Long = 5000000
Private Emirps As New Collection
Private CollTemp As New Collection

Sub Main()
Dim t
t = Timer
    FillCollectionOfEmirps
    Debug.Print "At this point : Execution time = " & Timer - t & " seconds."
    Debug.Print "We have a Collection of the " & Emirps.Count & " first Emirps."
    Debug.Print "---------------------------"
    'show the first   twenty   emirps
    Debug.Print "the first 20 emirps: "; ExtractEmirps(1, 20)
    'show all emirps between 7,700 and 8,000
    Debug.Print "all emirps between 7,700 and 8,000: "; ExtractEmirps(7700, 8000, True)
    'show the   10,000th   emirp
    Debug.Print "the 10,000th emirp: "; ExtractEmirps(10000, 10000)
  End Sub

Private Function ExtractEmirps(First As Long, Last As Long, Optional Value = False) As String
Dim temp$, i As Long, e
    If First = Last Then
        ExtractEmirps = Emirps(First)
    Else
        If Not Value Then
            For i = First To Last
                temp = temp & ", " & Emirps(i)
            Next
        Else
            For Each e In Emirps
                If e > First And e < Last Then
                    temp = temp & ", " & e
                End If
                If e = Last Then Exit For
            Next e
        End If
        ExtractEmirps = Mid(temp, 3)
    End If
End Function

Private Sub FillCollectionOfEmirps()
Dim Primes() As Long, e, i As Long
    Primes = Atkin
    For i = LBound(Primes) To UBound(Primes)
        CollTemp.Add Primes(i), CStr(Primes(i))
    Next i
    For Each e In CollTemp
        If IsEmirp(e) Then Emirps.Add e
    Next
End Sub

Private Function Atkin() As Long()
Dim MyBool() As Boolean
Dim SQRT_MAX As Long, i&, j&, N&, cpt&, MAX_TEMP As Long, temp() As Long
    ReDim MyBool(MAX)
    SQRT_MAX = Sqr(MAX) + 1
    MAX_TEMP = Sqr(MAX / 4) + 1
    For i = 1 To MAX_TEMP
        For j = 1 To SQRT_MAX
            N = 4 * i * i + j * j
            If N <= MAX And (N Mod 12 = 1 Or N Mod 12 = 5) Then
                MyBool(N) = True
            End If
        Next j
    Next i
    MAX_TEMP = Sqr(MAX / 3) + 1
    For i = 1 To MAX_TEMP
        For j = 1 To SQRT_MAX
            N = 3 * i * i + j * j
            If N <= MAX And N Mod 12 = 7 Then
                MyBool(N) = True
            End If
        Next j
    Next i
    For i = 1 To SQRT_MAX
        For j = 1 To SQRT_MAX
            N = 3 * i * i - j * j
            If i > j And N <= MAX And N Mod 12 = 11 Then
                MyBool(N) = True
            End If
        Next j
    Next i
    For i = 5 To SQRT_MAX Step 2
        If MyBool(i) Then
            For j = i * i To MAX Step i
                MyBool(j) = False
            Next
        End If
    Next
    ReDim temp(MAX / 2)
    temp(0) = 2: temp(1) = 3: cpt = 2
    For i = 5 To MAX Step 2
        If MyBool(i) Then temp(cpt) = i: cpt = cpt + 1
    Next
    ReDim Preserve temp(cpt - 1)
    Atkin = temp
End Function

Private Function IsEmirp(N) As Boolean
Dim a As String, b As String
    a = StrReverse(CStr(N)): b = CStr(N)
    If a <> b Then
        On Error Resume Next
        CollTemp.Add a, a
        If Err.Number > 0 Then
            IsEmirp = True
        Else
            CollTemp.Remove a
        End If
        On Error GoTo 0
    End If
End Function
```

{{out}}

```txt
At this point : Execution time = 13,23047 seconds.
We have a Collection of the 29952 first Emirps.
---------------------------
the first 20 emirps: 13, 17, 31, 37, 71, 73, 79, 97, 107, 113, 149, 157, 167, 179, 199, 311, 337, 347, 359, 389
all emirps between 7,700 and 8,000: 7717, 7757, 7817, 7841, 7867, 7879, 7901, 7927, 7949, 7951, 7963
the 10,000th emirp: 948349
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Runtime.CompilerServices

Module Module1
    <Extension()>
    Function ToHashSet(Of T)(source As IEnumerable(Of T)) As HashSet(Of T)
        Return New HashSet(Of T)(source)
    End Function

    <Extension()>
    Function Reverse(number As Integer) As Integer
        If number < 0 Then
            Return -Reverse(-number)
        End If
        If number < 10 Then
            Return number
        End If

        Dim rev = 0
        While number > 0
            rev = rev * 10 + number Mod 10
            number = number \ 10
        End While

        Return rev
    End Function

    <Extension()>
    Function Delimit(Of T)(source As IEnumerable(Of T), Optional seperator As String = " ") As String
        Return String.Join(If(seperator, " "), source)
    End Function

    Iterator Function Primes(bound As Integer) As IEnumerable(Of Integer)
        If bound < 2 Then
            Return
        End If
        Yield 2

        Dim composite As New BitArray((bound - 1) / 2)
        Dim limit As Integer = Int((Int(Math.Sqrt(bound)) - 1) / 2)
        For i = 0 To limit - 1
            If composite(i) Then
                Continue For
            End If
            Dim prime = 2 * i + 3
            Yield prime

            For j As Integer = Int((prime * prime - 2) / 2) To composite.Count - 1 Step prime
                composite(j) = True
            Next
        Next
        For i = limit To composite.Count - 1
            If Not composite(i) Then
                Yield 2 * i + 3
            End If
        Next
    End Function

    Iterator Function FindEmirpPrimes(limit As Integer) As IEnumerable(Of Integer)
        Dim ps = Primes(limit).ToHashSet()

        For Each p In ps
            Dim rev = p.Reverse()
            If rev <> p AndAlso ps.Contains(rev) Then
                Yield p
            End If
        Next
    End Function

    Sub Main()
        Dim limit = 1_000_000
        Console.WriteLine("First 20:")
        Console.WriteLine(FindEmirpPrimes(limit).Take(20).Delimit())
        Console.WriteLine()

        Console.WriteLine("Between 7700 and 8000:")
        Console.WriteLine(FindEmirpPrimes(limit).SkipWhile(Function(p) p < 7700).TakeWhile(Function(p) p < 8000).Delimit())
        Console.WriteLine()

        Console.WriteLine("10000th:")
        Console.WriteLine(FindEmirpPrimes(limit).ElementAt(9999))
    End Sub

End Module
```

{{out}}

```txt
First 20:
13 17 31 37 71 73 79 97 107 113 149 157 167 179 199 311 337 347 359 389

Between 7700 and 8000:
7717 7757 7817 7841 7867 7879 7901 7927 7949 7951 7963

10000th:
948349
```



## zkl

Uses the solution from task [[Extensible prime generator#zkl]]. Saves the primes to a list, which gets pretty big.

```zkl
var PS=Import("Src/ZenKinetic/sieve").postponed_sieve;
var ps=Utils.Generator(PS), plist=ps.walk(10).copy();

fcn isEmirp(p){ rp:=p.toString().reverse().toInt();
   if(p==rp) return(False);
   if(plist.holds(rp)) return(True);
   tp:=p; mp:=p.max(rp); while(tp<mp) { plist.append(tp=ps.next()) }
   return(tp==rp);
}

Utils.Generator(PS).filter(20,isEmirp);

Utils.Generator(PS).filter(fcn(p){if(p>8000)return(Void.Stop); p>7700 and isEmirp(p)});

Utils.Generator(PS).reduce(fcn(N,p){N+=isEmirp(p); (N==10000) and T(Void.Stop,p) or N },0);
```

{{out}}

```txt

L(13,17,31,37,71,73,79,97,107,113,149,157,167,179,199,311,337,347,359,389)
L(7817,7841,7867,7879,7901,7927,7949,7951,7963)
948349

```

