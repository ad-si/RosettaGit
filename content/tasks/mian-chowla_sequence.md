+++
title = "Mian-Chowla sequence"
description = ""
date = 2019-10-21T12:04:41Z
aliases = []
[extra]
id = 22215
[taxonomies]
categories = ["task"]
tags = []
+++

The [[wp:Mian–Chowla sequence|Mian–Chowla sequence]] is an integer sequence defined recursively.

The sequence starts with:

::<span style="font-style:italic;font-weight:bold;font-size:125%;">a<sub>1</sub> = 1</span>

then for <span style="font-style:italic;font-weight:bold;font-size:125%;padding:.3em;">n > 1,</span> <span style="font-style:italic;font-weight:bold;font-size:125%;padding:.3em;">a<sub>n</sub></span> is the smallest positive integer such that every pairwise sum

::<span style="font-style:italic;font-weight:bold;font-size:125%;">a<sub>i</sub> + a<sub>j</sub>

is distinct, for all <span style="font-style:italic;font-weight:bold;font-size:125%;">i</span> and <span style="font-style:italic;font-weight:bold;font-size:125%;">j</span> less than or equal to <span style="font-style:italic;font-weight:bold;font-size:125%;">n</span>.

;The Task:

:* Find and display, here, on this page the first 30 terms of the Mian–Chowla sequence.
:* Find and display, here, on this page the 91st through 100th terms of the Mian–Chowla sequence.


Demonstrating working through the first few terms longhand:

::<span style="font-style:italic;font-weight:bold;font-size:125%;">a<sub>1</sub> = 1</span>

::<span style="font-weight:bold;">1 + 1 = 2</span>

Speculatively try <span style="font-style:italic;font-weight:bold;font-size:125%;padding:.3em;">a<sub>2</sub> = 2</span>

::<span style="font-weight:bold;">1 + 1 = 2</span>
::<span style="font-weight:bold;">1 + 2 = 3</span>
::<span style="font-weight:bold;">2 + 2 = 4</span>

There are no repeated sums so '''2''' is the next number in the sequence.

Speculatively try <span style="font-style:italic;font-weight:bold;font-size:125%;padding:.3em;">a<sub>3</sub> = 3</span>

::<span style="font-weight:bold;">1 + 1 = 2</span>
::<span style="font-weight:bold;">1 + 2 = 3</span>
::<span style="font-weight:bold;">1 + 3 = <span style="background-color:yellow;">4</span></span>
::<span style="font-weight:bold;">2 + 2 = <span style="background-color:yellow;">4</span></span>
::<span style="font-weight:bold;">2 + 3 = 5</span>
::<span style="font-weight:bold;">3 + 3 = 6</span>

Sum of '''4''' is repeated so '''3''' is rejected.

Speculatively try <span style="font-style:italic;font-weight:bold;font-size:125%;padding:.3em;">a<sub>3</sub> = 4</span>

::<span style="font-weight:bold;">1 + 1 = 2</span>
::<span style="font-weight:bold;">1 + 2 = 3</span>
::<span style="font-weight:bold;">1 + 4 = 5</span>
::<span style="font-weight:bold;">2 + 2 = 4</span>
::<span style="font-weight:bold;">2 + 4 = 6</span>
::<span style="font-weight:bold;">4 + 4 = 8</span>

There are no repeated sums so '''4''' is the next number in the sequence.

And so on...

## See also

:* [[oeis:A005282|OEIS:A005282 Mian-Chowla sequence]]



## Ada

```Ada
with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

procedure Mian_Chowla_Sequence
is
   type Natural_Array is array(Positive range <>) of Natural;

   function Hash(P : in Positive) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type(P);
   end Hash;

   package Positive_Sets is new Ada.Containers.Hashed_Sets(Positive, Hash, "=");

   function Mian_Chowla(N : in Positive) return Natural_Array
   is
      return_array : Natural_Array(1 .. N) := (others => 0);
      nth : Positive := 1;
      candidate : Positive := 1;
      seen : Positive_Sets.Set;
   begin
      while nth <= N loop
         declare
            sums : Positive_Sets.Set;
            terms : constant Natural_Array := return_array(1 .. nth-1) & candidate;
            found : Boolean := False;
         begin
            for term of terms loop
               if seen.Contains(term + candidate) then
                  found := True;
                  exit;
               else
                  sums.Insert(term + candidate);
               end if;
            end loop;

            if not found then
               return_array(nth) := candidate;
               seen.Union(sums);
               nth := nth + 1;
            end if;
            candidate := candidate + 1;
         end;
      end loop;
      return return_array;
   end Mian_Chowla;

   length : constant Positive := 100;
   sequence : constant Natural_Array(1 .. length) := Mian_Chowla(length);
begin
   Ada.Text_IO.Put_Line("Mian Chowla sequence first 30 terms :");
   for term of sequence(1 .. 30) loop
      Ada.Text_IO.Put(term'Img);
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("Mian Chowla sequence terms 91 to 100 :");
   for term of sequence(91 .. 100) loop
      Ada.Text_IO.Put(term'Img);
   end loop;
end Mian_Chowla_Sequence;
```

```txt
Mian Chowla sequence first 30 terms :
 1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312
Mian Chowla sequence terms 91 to 100 :
 22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

```




## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}Allocating a large-enough array initially would gain some performance but might be considered cheating - 60 000 elements would be enough for the task.

```algol68
# Find Mian-Chowla numbers: an
                     where: ai = 1,
                       and: an = smallest integer such that ai + aj is unique
                                             for all i, j in 1 .. n && i <= j
#
BEGIN
    INT max mc           = 100;
    [ max mc ]INT mc;
    INT curr size       :=      0; # initial size of the array     #
    INT size increment   = 10 000; # size to increase the array by #
    REF[]BOOL is sum    := HEAP[ 1 : 0 ]BOOL;
    INT mc count        := 1;
    FOR i WHILE mc count <= max mc DO
        # assume i will be part of the sequence                    #
        mc[ mc count ]  := i;
        # check the sums                                           #
        IF  ( 2 * i ) > curr size THEN
            # the is sum array is too small - make a larger one    #
            REF[]BOOL new sum = HEAP[ curr size + size increment ]BOOL;
            new sum[ 1 : curr size ] := is sum;
            FOR n TO size increment DO new sum[ curr size + n ] := FALSE OD;
            curr size  +:= size increment;
            is sum      := new sum
        FI;
        BOOL is unique  := TRUE;
        FOR mc pos TO mc count WHILE is unique := NOT is sum[ i + mc[ mc pos ] ] DO SKIP OD;
        IF is unique THEN
            # i is a sequence element - store the sums             #
            FOR k TO mc count DO is sum[ i + mc[ k ] ] := TRUE OD;
            mc count +:= 1
        FI
    OD;

    # print parts of the sequence                                  #
    print( ( "Mian Chowla sequence elements 1..30:", newline ) );
    FOR i TO 30 DO print( ( " ", whole( mc[ i ], 0 ) ) ) OD;
    print( ( newline ) );
    print( ( "Mian Chowla sequence elements 91..100:", newline ) );
    FOR i FROM 91 TO 100 DO print( ( " ", whole( mc[ i ], 0 ) ) ) OD

END
```

```txt

Mian Chowla sequence elements 1..30:
 1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312
Mian Chowla sequence elements 91..100:
 22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

```

elapsed time approx 0.25 seconds on my Windows 7 system (note under Windows, A68G runs as an interpreter only).


## AWK

Translation of the ALGOL 68 - largely implements the "by hand" method in the task.

```awk
# Find Mian-Chowla numbers: an
#                    where: ai = 1,
#                      and: an = smallest integer such that ai + aj is unique
#                                            for all i, j in 1 .. n && i <= j
#
BEGIN \
{

    FALSE      = 0;
    TRUE       = 1;

    mcCount    = 1;

    for( i = 1; mcCount <= 100; i ++ )
    {
        # assume i will be part of the sequence
        mc[ mcCount ] = i;
        # check the sums
        isUnique = TRUE;
        for( mcPos = 1; mcPos <= mcCount && isUnique; mcPos ++ )
        {
            isUnique = ! ( ( i + mc[ mcPos ] ) in isSum );
        } # for j
        if( isUnique )
        {
            # i is a sequence element - store the sums
            for( k = 1; k <= mcCount; k ++ )
            {
                isSum[ i + mc[ k ] ] = TRUE;
            } # for k
            mcCount ++;
        } # if isUnique
    } # for i
    # print the sequence
    printf( "Mian Chowla sequence elements 1..30:\n" );
    for( i = 1; i <= 30; i ++ )
    {
        printf( " %d", mc[ i ] );
    } # for i
    printf( "\n" );
    printf( "Mian Chowla sequence elements 91..100:\n" );
    for( i = 91; i <= 100; i ++ )
    {
        printf( " %d", mc[ i ] );
    } # for i
    printf( "\n" );

} # BEGIN
```

```txt

Mian Chowla sequence elements 1..30:
 1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312
Mian Chowla sequence elements 91..100:
 22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

```

elapsed time approx 0.20 seconds on my Windows 7 system.


### Alternate

{{trans|Go}}Hopefully the comments help explain the algorithm.

```awk
# helper functions
#
#    determine if a list is empty or not
function isEmpty(a) { for (ii in a) return 0; return 1 }
#    list concatination
function concat(a, b) { for (cc in b) a[cc] = cc }

BEGIN \
{
    mc[0] = 1; sums[2] = 0;      # initialize lists
    for ( i = 1; i < 100; i ++ ) # iterate for each item in result
    {
        for ( j = mc[i-1]+1; ; j ++ ) # iterate thru trial values
        {
            mc[i] = j;           # set trial value into result
            for ( k = 0; k <= i; k ++ ) # test new iteration of sums
            {
                # test trial sum against old sums list
                if ((sum = mc[k] + j) in sums)
                {                # collision, so
                    delete ts;   # toss out any accumulated items,
                    break;       #  and break out to the next j
                }
                ts[sum] = sum;   # (else) accumulate to new sum list
            } # for k
            if ( isEmpty( ts ) ) # nothing to add,
                continue;        #  so try next j
            concat( sums, ts );  # combine new sums to old,
            delete ts;           #  clear out the new,
            break;               #  break out to next i
        } # for j
    } # for i
    # print the sequence
    ps = "Mian Chowla sequence elements %d..%d:\n";
    for ( i = 0; i < 100; i ++ )
    {
        if ( i == 0 )  printf ps, 1, 30;
        if ( i == 90 ) printf "\n\n" ps, 91, 100;
        if ( i < 30 || i >= 90 ) printf "%d ", mc[ i ];
    } # for i
    print "\n"
} # BEGIN
```

```txt
Mian Chowla sequence elements 1..30:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Mian Chowla sequence elements 91..100:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219
```
Computation time is about 110 ms on ''tio.run
''


## C

```c
#include <stdio.h>
#include <stdbool.h>
#include <time.h>

#define n 100
#define nn ((n * (n + 1)) >> 1)

bool Contains(int lst[], int item, int size) {
	for (int i = size - 1; i >= 0; i--)
 		if (item == lst[i]) return true;
	return false;
}

int * MianChowla()
{
	static int mc[n]; mc[0] = 1;
	int sums[nn];	sums[0] = 2;
	int sum, le, ss = 1;
	for (int i = 1; i < n; i++) {
		le = ss;
		for (int j = mc[i - 1] + 1; ; j++) {
			mc[i] = j;
			for (int k = 0; k <= i; k++) {
				sum = mc[k] + j;
				if (Contains(sums, sum, ss)) {
					ss = le; goto nxtJ;
				}
				sums[ss++] = sum;
			}
			break;
		nxtJ:;
		}
	}
	return mc;
}

int main() {
	clock_t st = clock(); int * mc; mc = MianChowla();
        double et = ((double)(clock() - st)) / CLOCKS_PER_SEC;
	printf("The first 30 terms of the Mian-Chowla sequence are:\n");
	for (int i = 0; i < 30; i++) printf("%d ", mc[i]);
	printf("\n\nTerms 91 to 100 of the Mian-Chowla sequence are:\n");
	for (int i = 90; i < 100; i++) printf("%d ", mc[i]);
	printf("\n\nComputation time was %f seconds.", et);
}
```

```txt
The first 30 terms of the Mian-Chowla sequence are:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Terms 91 to 100 of the Mian-Chowla sequence are:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

Computation time was 1.575556 seconds.
```

===Quick, but...===
...is memory hungry. This will allocate a bigger buffer as needed to keep track of the sums involved.  Based on the '''ALGOL 68''' version.  The minimum memory needed is double of the highest entry calculated.  This program doubles the buffer size each time needed, so it will use more than the minimum.  The '''ALGOL 68''' increments by a fixed increment size. Which could be just as wasteful if the increment is too large and slower if the increment is too small).

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>

// helper function for indicating memory used.
void approx(char* buf, double count)
{
    const char* suffixes[] = { "Bytes", "KiB", "MiB" };
    uint s = 0;
    while (count >= 1024 && s < 3) { s++; count /= 1024; }
    if (count - (double)((int)count) == 0.0)
        sprintf(buf, "%d %s", (int)count, suffixes[s]);
    else
        sprintf(buf, "%.1f %s", count, suffixes[s]);
}

int main() {
    int i, j, k, c = 0, n = 100, nn = 110;
    int* mc = (int*) malloc((n) * sizeof(int));
    bool* isSum = (bool*) calloc(nn, sizeof(bool));
    char em[] = "unable to increase isSum array to %ld.";
    if (n > 100)  printf("Computing terms 1 to %d...\n", n);
    clock_t st = clock();
    for (i = 1; c < n; i++) {
        mc[c] = i;
        if (i + i > nn) {
            bool* newIs = (bool*)realloc(isSum, (nn <<= 1) * sizeof(bool));
            if (newIs == NULL) { printf(em, nn); return -1; }
            isSum = newIs;
            for (j = (nn >> 1); j < nn; j++) isSum[j] = false;
        }
        bool isUnique = true;
        for (j = 0; (j < c) && isUnique; j++) isUnique = !isSum[i + mc[j]];
        if (isUnique) {
            for (k = 1; k <= c; k++) isSum[i + mc[k]] = true;
            c++;
        }
    }
    double et = 1e3 * ((double)(clock() - st)) / CLOCKS_PER_SEC;
    free(isSum);
    printf("The first 30 terms of the Mian-Chowla sequence are:\n");
    for (i = 0; i < 30; i++) printf("%d ", mc[i]);
    printf("\n\nTerms 91 to 100 of the Mian-Chowla sequence are:\n");
    for (i = 90; i < 100; i++) printf("%d ", mc[i]);
    if (c > 100) printf("\nTerm %d is: %d" ,c , mc[c - 1]);
    free(mc);
    char buf[100]; approx(buf, nn * sizeof(bool));
    printf("\n\nComputation time was %6.3f ms.  Allocation was %s.", et, buf);
}
```

```txt
The first 30 terms of the Mian-Chowla sequence are:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Terms 91 to 100 of the Mian-Chowla sequence are:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

Computation time was  1.773 ms.  Allocation was 55 KiB.
```

Here is the output for a larger calculation:

```txt
Computing terms 1 to 1300...
The first 30 terms of the Mian-Chowla sequence are:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Terms 91 to 100 of the Mian-Chowla sequence are:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219
Term 1300 is: 29079927

Computation time was 7979.042 ms.  Allocation was 110 MiB.
```


=={{header|C#|CSharp}}==
```c#
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

static class Program {
    static int[] MianChowla(int n) {
        int[] mc = new int[n - 1 + 1];
        HashSet<int> sums = new HashSet<int>(), ts = new HashSet<int>();
        int sum; mc[0] = 1; sums.Add(2);
        for (int i = 1; i <= n - 1; i++) {
            for (int j = mc[i - 1] + 1; ; j++) {
                mc[i] = j;
                for (int k = 0; k <= i; k++) {
                    sum = mc[k] + j;
                    if (sums.Contains(sum)) { ts.Clear(); break; }
                    ts.Add(sum);
                }
                if (ts.Count > 0) { sums.UnionWith(ts); break; }
            }
        }
        return mc;
    }

    static void Main(string[] args)
    {
        const int n = 100; Stopwatch sw = new Stopwatch();
        string str = " of the Mian-Chowla sequence are:\n";
        sw.Start(); int[] mc = MianChowla(n); sw.Stop();
        Console.Write("The first 30 terms{1}{2}{0}{0}Terms 91 to 100{1}{3}{0}{0}" +
            "Computation time was {4}ms.{0}", '\n', str, string.Join(" ", mc.Take(30)),
            string.Join(" ", mc.Skip(n - 10)), sw.ElapsedMilliseconds);
    }
}
```

```txt
The first 30 terms of the Mian-Chowla sequence are:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Terms 91 to 100 of the Mian-Chowla sequence are:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

Computation time was 17ms.
```



## C++

The '''sums''' array expands by "i" on each iteration from 1 to n, so the max array length can be pre-calculated to the nth triangular number (n * (n + 1) / 2).

```cpp
using namespace std;

#include <iostream>
#include <ctime>

#define n 100
#define nn ((n * (n + 1)) >> 1)

bool Contains(int lst[], int item, int size) {
	for (int i = 0; i < size; i++) if (item == lst[i]) return true;
	return false;
}

int * MianChowla()
{
	static int mc[n]; mc[0] = 1;
	int sums[nn];	sums[0] = 2;
	int sum, le, ss = 1;
	for (int i = 1; i < n; i++) {
		le = ss;
		for (int j = mc[i - 1] + 1; ; j++) {
			mc[i] = j;
			for (int k = 0; k <= i; k++) {
				sum = mc[k] + j;
				if (Contains(sums, sum, ss)) {
					ss = le; goto nxtJ;
				}
				sums[ss++] = sum;
			}
			break;
		nxtJ:;
		}
	}
	return mc;
}

int main() {
	clock_t st = clock(); int * mc; mc = MianChowla();
	double et = ((double)(clock() - st)) / CLOCKS_PER_SEC;
	cout << "The first 30 terms of the Mian-Chowla sequence are:\n";
	for (int i = 0; i < 30; i++) { cout << mc[i] << ' '; }
	cout << "\n\nTerms 91 to 100 of the Mian-Chowla sequence are:\n";
	for (int i = 90; i < 100; i++) { cout << mc[i] << ' '; }
	cout << "\n\nComputation time was " << et << " seconds.";
}
```

```txt
The first 30 terms of the Mian-Chowla sequence are:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Terms 91 to 100 of the Mian-Chowla sequence are:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

Computation time was 1.92958 seconds.
```


=={{header|F_Sharp|F#}}==

### The function


```fsharp

// Generate Mian-Chowla sequence. Nigel Galloway: March 23rd., 2019
let mC=let rec fN i g l=seq{
         let a=(l*2)::[for i in i do yield i+l]@g
         let b=[l+1..l*2]|>Seq.find(fun e->Seq.forall(fun g->(Seq.contains (g-e)>>not) i) a)
         yield b; yield! fN (l::i) (a|>List.filter(fun n->n>b)) b}
       seq{yield 1; yield! fN [] [] 1}

```


### The Tasks

;First 30

```fsharp

mC |> Seq.take 30 |> Seq.iter(printf "%d ");printfn ""

```

```txt

1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

```

;91 to 100

```fsharp

mC |> Seq.skip 90 |> Seq.take 10 |> Seq.iter(printf "%d ");printfn ""

```

```txt

22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

```


## Go


```go
package main

import "fmt"

func contains(is []int, s int) bool {
    for _, i := range is {
        if s == i {
            return true
        }
    }
    return false
}

func mianChowla(n int) []int {
    mc := make([]int, n)
    mc[0] = 1
    is := []int{2}
    var sum int
    for i := 1; i < n; i++ {
        le := len(is)
    jloop:
        for j := mc[i-1] + 1; ; j++ {
            mc[i] = j
            for k := 0; k <= i; k++ {
                sum = mc[k] + j
                if contains(is, sum) {
                    is = is[0:le]
                    continue jloop
                }
                is = append(is, sum)
            }
            break
        }
    }
    return mc
}

func main() {
    mc := mianChowla(100)
    fmt.Println("The first 30 terms of the Mian-Chowla sequence are:")
    fmt.Println(mc[0:30])
    fmt.Println("\nTerms 91 to 100 of the Mian-Chowla sequence are:")
    fmt.Println(mc[90:100])
}
```


```txt

The first 30 terms of the Mian-Chowla sequence are:
[1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312]

Terms 91 to 100 of the Mian-Chowla sequence are:
[22526 23291 23564 23881 24596 24768 25631 26037 26255 27219]

```



Quicker version (runs in less than 0.02 seconds on Celeron N3050 @1.6 GHz), output as before:

```go
package main

import "fmt"

type set map[int]bool

func mianChowla(n int) []int {
    mc := make([]int, n)
    mc[0] = 1
    is := make(set, n*(n+1)/2)
    is[2] = true
    var sum int
    isx := make([]int, 0, n)
    for i := 1; i < n; i++ {
        isx = isx[:0]
    jloop:
        for j := mc[i-1] + 1; ; j++ {
            mc[i] = j
            for k := 0; k <= i; k++ {
                sum = mc[k] + j
                if is[sum] {
                    isx = isx[:0]
                    continue jloop
                }
                isx = append(isx, sum)
            }
            for _, x := range isx {
                is[x] = true
            }
            break
        }
    }
    return mc
}

func main() {
    mc := mianChowla(100)
    fmt.Println("The first 30 terms of the Mian-Chowla sequence are:")
    fmt.Println(mc[0:30])
    fmt.Println("\nTerms 91 to 100 of the Mian-Chowla sequence are:")
    fmt.Println(mc[90:100])
}
```



## Haskell

```haskell
import Data.Set (Set, fromList, insert, member)

mianChowlas :: Int -> [Int]
mianChowlas n =
  let (_, cm, _) = unzip3 $ iterate nextMC (fromList [2], [1], 1)
  in reverse $ cm !! (n - 1)

nextMC :: (Set Int, [Int], Int) -> (Set Int, [Int], Int)
nextMC (sumSet, mcs, n) =
  let valid x = all (not . flip member sumSet . (x +)) mcs
      m = until valid succ n
  in (foldr insert sumSet ((2 * m) : fmap (m +) mcs), m : mcs, m)

main :: IO ()
main =
  (putStrLn . unlines)
    [ "First 30 terms of the Mian-Chowla series:"
    , show (mianChowlas 30)
    , []
    , "Terms 91 to 100 of the Mian-Chowla series:"
    , show $ drop 90 (mianChowlas 100)
    ]
```

```txt
First 30 terms of the Mian-Chowla series:
[1,2,4,8,13,21,31,45,66,81,97,123,148,182,204,252,290,361,401,475,565,593,662,775,822,916,970,1016,1159,1312]

Terms 91 to 100 of the Mian-Chowla series:
[22526,23291,23564,23881,24596,24768,25631,26037,26255,27219]
```



## J



```j

NB. http://rosettacode.org/wiki/Mian-Chowla_sequence

NB. Dreadfully inefficient implementation recomputes all the sums to n-1
NB. and computes the full addition table rather than just a triangular region
NB. However, this implementation is sufficiently quick to meet the requirements.

NB. The vector head is the next speculative value
NB. Beheaded, the vector is Mian-Chowla sequence.


Until =: conjunction def 'u^:(0 = v)^:_'
unique =: -:&# ~.   NB. tally of list matches that of set

next_mc =: [: (, {.) (>:@:{. , }.)Until(unique@:((<:/~@i.@# #&, +/~)@:(}. , {.)))


prime_q =: 1&p:   NB. for fun look at prime generation suitability

```



```txt

   NB. generate sufficient terms of sequence

   A =: (next_mc^:108) 1 1

   NB. first 30 terms
   (,:prime_q)30{.}.A
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312
0 1 0 0  1  0  1  0  0  0  1   0   0   0   0   0   0   0   1   0   0   1   0   0   0   0   0    0    0    0

   NB. terms 91 through 100
   (,: prime_q) A {~ 91+i.10
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219
    0     1     0     0     0     0     0     0     0     0

```



## JavaScript

{{Trans|Python}} (Functional Python version)

```javascript
(() => {
    'use strict';

    const main = () => {

        const genMianChowla = mianChowlas();
        console.log([
            'Mian-Chowla terms 1-30:',
            take(30, genMianChowla),

            '\nMian-Chowla terms 91-100:',
            (() => {
                drop(60, genMianChowla);
                return take(10, genMianChowla);
            })()
        ].join('\n') + '\n');
    };

    // mianChowlas :: Gen [Int]
    function* mianChowlas() {
        let
            mcs = [1],
            sumSet = new Set([2]),
            x = 1;
        while (true) {
            yield x;
            [sumSet, mcs, x] = nextMC(sumSet, mcs, x);
        }
    }

    // nextMC :: Set Int -> [Int] -> Int -> (Set Int, [Int], Int)
    const nextMC = (setSums, mcs, n) => {
        // Set of sums -> Series up to n -> Next term in series
        const valid = x => {
            for (const m of mcs) {
                if (setSums.has(x + m)) return false;
            }
            return true;
        };
        const x = until(valid, succ, n);
        return [
            sumList(mcs, x)
            .reduce(
                (a, n) => (a.add(n), a),
                setSums
            ),
            mcs.concat(x),
            x
        ]

    };

    // sumList :: [Int] -> Int -> [Int]
    const sumList = (xs, n) =>
        // Series so far -> additional term -> new sums
        [2 * n].concat(map(x => n + x, xs));


    // GENERIC FUNCTIONS ----------------------------

    // drop :: Int -> [a] -> [a]
    // drop :: Int -> Generator [a] -> Generator [a]
    // drop :: Int -> String -> String
    const drop = (n, xs) =>
        Infinity > length(xs) ? (
            xs.slice(n)
        ) : (take(n, xs), xs);


    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // succ :: Int -> Int
    const succ = x => 1 + x;

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // MAIN ---
    return main();
})();
```

```txt
Mian-Chowla terms 1-30:
1,2,4,8,13,21,31,45,66,81,97,123,148,182,204,252,290,361,401,475,565,593,662,775,822,916,970,1016,1159,1312

Mian-Chowla terms 91-100:
22526,23291,23564,23881,24596,24768,25631,26037,26255,27219

[Finished in 0.184s]

(Executed in the Atom editor, using Run Script)
```



## Julia

Optimization in Julia can be an incremental process. The first version of this program ran in over 2 seconds. Using a hash table for lookup of sums and avoiding reallocation of arrays helps considerably.

```julia
function mianchowla(n)
    seq = ones(Int, n)
    sums = Dict{Int,Int}()
    tempsums = Dict{Int,Int}()
    for i in 2:n
        seq[i] = seq[i - 1] + 1
        incrementing = true
        while incrementing
            for j in 1:i
                tsum = seq[j] + seq[i]
                if haskey(sums, tsum)
                    seq[i] += 1
                    empty!(tempsums)
                    break
                else
                    tempsums[tsum] = 0
                    if j == i
                        merge!(sums, tempsums)
                        empty!(tempsums)
                        incrementing = false
                    end
                end
            end
        end
    end
    seq
end

function testmianchowla()
    println("The first 30 terms of the Mian-Chowla sequence are $(mianchowla(30)).")
    println("The 91st through 100th terms of the Mian-Chowla sequence are $(mianchowla(100)[91:100]).")
end

testmianchowla()
@time testmianchowla()


```
```txt

...
The first 30 terms of the Mian-Chowla sequence are [1, 2, 4, 8, 13, 21, 31, 45, 66, 81, 97, 123, 148, 182, 204, 252, 290, 361, 401, 475, 565, 593, 662, 775, 822, 916, 970, 1016, 1159, 1312].
The 91st through 100th terms of the Mian-Chowla sequence are [22526, 23291, 23564, 23881, 24596, 24768, 25631, 26037, 26255, 27219].
  0.007524 seconds (168 allocations: 404.031 KiB)

```


## Kotlin

```scala
// Version 1.3.21

fun mianChowla(n: Int): List<Int> {
    val mc = MutableList(n) { 0 }
    mc[0] = 1
    val hs = HashSet<Int>(n * (n + 1) / 2)
    hs.add(2)
    val hsx = mutableListOf<Int>()
    for (i in 1 until n) {
        hsx.clear()
        var j = mc[i - 1]
        outer@ while (true) {
            j++
            mc[i] = j
            for (k in 0..i) {
                val sum = mc[k] + j
                if (hs.contains(sum)) {
                    hsx.clear()
                    continue@outer
                }
                hsx.add(sum)
            }
            hs.addAll(hsx)
            break
        }
    }
    return mc
}

fun main() {
    val mc = mianChowla(100)
    println("The first 30 terms of the Mian-Chowla sequence are:")
    println(mc.subList(0, 30))
    println("\nTerms 91 to 100 of the Mian-Chowla sequence are:")
    println(mc.subList(90, 100))
}
```


```txt

The first 30 terms of the Mian-Chowla sequence are:
[1, 2, 4, 8, 13, 21, 31, 45, 66, 81, 97, 123, 148, 182, 204, 252, 290, 361, 401, 475, 565, 593, 662, 775, 822, 916, 970, 1016, 1159, 1312]

Terms 91 to 100 of the Mian-Chowla sequence are:
[22526, 23291, 23564, 23881, 24596, 24768, 25631, 26037, 26255, 27219]

```



## Pascal

keep sum of all sorted.Memorizing the compare positions speeds up.
<BR>

```txt
const
  deltaK = 250;
  maxCnt = 25000;
 Using
  tElem = Uint64;
  t_n_sum_all = array of tElem; //dynamic array
    n          mian-chowla[n]  average dist    runtime
   250                317739           1270       429 ms// runtime setlength of 2.35 GB ~ 400ms
   500               2085045           7055       589 ms
   750               6265086          16632      1053 ms
..
  1500              43205712          67697      6669 ms
..
  3000             303314913         264489     65040 ms //2xn -> runtime x9,75
..
  6000            2189067236        1019161    719208 ms //2xn -> runtime x11,0
  6250            2451223363        1047116    825486 ms
..
 12000           15799915996        3589137   8180177 ms //2xn -> runtime x11,3
 12250           16737557137        3742360   8783711 ms
 12500           17758426186        4051041   9455371 ms
..
 24000          115709049568       13738671  99959526 ms  //2xn -> runtime x12
 24250          119117015697       13492623 103691559 ms
 24500          122795614247       14644721 107758962 ms
 24750          126491059919       14708578 111875949 ms
 25000          130098289096       14414457 115954691 ms //dt = 4078s ->16s/per number

 real  1932m34,698s => 1d8h12m35
```


```pascal
program MianChowla;
//compiling with /usr/lib/fpc/3.2.0/ppcx64.2 -MDelphi -O4 -al "%f"
{$CODEALIGN proc=8,loop=4 }
uses
  sysutils;
const
  deltaK = 100;
  maxCnt = 1000;
type
  tElem  = Uint32;
  tpElem = pUint32;
  t_n = array[0..maxCnt+1] of tElem;
  t_n_sum_all = array[0..(maxCnt+1)*(maxCnt+2) DIV 2] of tElem;

var
  n_LastPos,
  n : t_n;

  n_sum_all : t_n_sum_all;

  maxIdx,
  maxN,
  max_SumIdx : NativeUInt;

procedure Init;
var
  i : NativeInt;
begin
  maxIdx := 1;
  maxN   := 1;
  n[maxIdx] := maxN;
  max_SumIdx := 1;
  n_sum_all[max_SumIdx] := 2*maxN;

  For i := 0 to maxCnt do
    n_LastPos[i] := 1;
end;

procedure InsertNew_sum(NewValue:NativeUint);
//insertion already knowning the positions
var
  pElem :tpElem;
  InsIdx,chkIdx,oldIdx,newIdx : nativeInt;
Begin
  newIdx := maxIdx;
  oldIdx := max_SumIdx;
  //append new value
  inc(maxIdx);
  n[maxIdx] := NewValue;
  //extend sum_
  inc(max_SumIdx,maxIdx);
  //heighest value already known
  InsIdx := max_SumIdx;
  n_sum_all[InsIdx] := 2*NewValue;
  //stop mark
  n_sum_all[InsIdx+1] := High(tElem);
  pElem := @n_sum_all[0];
  dec(InsIdx);
  //n_LastPos[newIdx]+newIdx-1 == InsIdx
  repeat
    //move old bigger values
    chkIdx := n_LastPos[newIdx]+newIdx-1;
    while InsIdx > chkIdx do
    Begin
      pElem[InsIdx] := pElem[oldIdx];
      dec(InsIdx);
      dec(oldIdx);
    end;
    //insert new value
    pElem[InsIdx] := NewValue+n[newIdx];
    dec(InsIdx);
    dec(newIdx);
    //all inserted
  until newIdx <= 0;
  //new minimum search position one behind, oldidx is one to small
  inc(oldidx,2);
  For newIdx := 1 to maxIdx do
    n_LastPos[newIdx] := oldIdx;
end;
procedure FindNew;
var
  pSumAll,pn : tpElem;
  i,LastCheckPos,newValue,newSum : NativeUint;
  TestRes : boolean;
begin
  //start value = last inserted value
  newValue := n[maxIdx];
  pSumAll := @n_sum_all[0];
  pn := @n[0];
  repeat
    //try next number
    inc(newValue);
    LastCheckPos := n_LastPos[1];
    i := 1;
    //check if sum = new is already n all_sum
    repeat
      newSum := newValue+pn[i];
      IF LastCheckPos < n_LastPos[i] then
        LastCheckPos := n_LastPos[i];
      while pSumAll[LastCheckPos] < newSum do
        inc(LastCheckPos);
      //memorize LastCheckPos;
      n_LastPos[i] := LastCheckPos;
      TestRes:= pSumAll[LastCheckPos] = newSum;
      IF TestRes then
        BREAK;
      inc(i);
    until i>maxIdx;
    //found?
    If not(TestRes) then
      BREAK;
  until false;
  InsertNew_sum(newValue);
end;

var
  T1,T0: Int64;
  i,k : NativeInt;

procedure Out_num(k:NativeInt);
Begin
  T1 := GetTickCount64;
  //     k      n[k]     average dist last deltak          total time
  writeln(k:6,n[k]:12,(n[k]-n[k-deltaK+1]) DIV deltaK:8,T1-T0:8,' ms');
end;

BEGIN
  writeln('Allocated memory ',2*SizeOf(t_n)+Sizeof(t_n_sum_all));
  T0 := GetTickCount64;
  while t0 = GetTickCount64 do;
  T0 := GetTickCount64;
  Init;

  k := deltaK;
  i := 1;
  repeat
    repeat
      FindNew;
      inc(i);
    until i=k;
    Out_num(k);
    k := k+deltaK;
  until k>maxCnt;
  writeln;
  writeln(#13,'The first 30 terms of the Mian-Chowla sequence are');
  For i := 1 to 30 do
    write(n[i],' ');
  writeln;
  writeln;
  writeln('The terms 91 - 100 of the Mian-Chowla sequence are');
  For i := 91 to 100 do
    write(n[i],' ');
  writeln;
END.

```

```txt
Allocated memory 2014024
   100       27219     272   0.002 s
   200      172922    1443   0.011 s
   300      514644    3404   0.037 s
   400     1144080    6197   0.090 s
   500     2085045    9398   0.179 s
   600     3375910   12689   0.311 s
   700     5253584   18705   0.520 s
   800     7600544   23438   0.801 s
   900    10441056   28339   1.160 s
  1000    14018951   35611   1.640 s
The first 30 terms of the Mian-Chowla sequence are
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

The terms 91 - 100 of the Mian-Chowla sequence are
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219
```



## Perl


```perl
use strict;
use warnings;
use feature 'say';

sub generate_mc {
    my($max)  = @_;
    my $index = 0;
    my $test  = 1;
    my %sums  = (2 => 1);
    my @mc    = 1;
    while ($test++) {
        my %these = %sums;
        map { next if ++$these{$_ + $test} > 1 } @mc[0..$index], $test;
        %sums = %these;
        $index++;
        return @mc if (push @mc, $test) > $max-1;
    }
}

my @mian_chowla = generate_mc(100);
say "First 30 terms in the Mian–Chowla sequence:\n", join(' ', @mian_chowla[ 0..29]),
    "\nTerms 91 through 100:\n",                     join(' ', @mian_chowla[90..99]);
```

```txt
First 30 terms in the Mian–Chowla sequence:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Terms 91 through 100:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219
```



## Perl 6


```perl6
my @mian-chowla = 1, |(2..Inf).map: -> $test {
    state $index = 1;
    state %sums  = 2 => 1;
    my $next;
    my %these;
    @mian-chowla[^$index].map: { ++$next and last if %sums{$_ + $test}:exists; ++%these{$_ + $test} };
    next if $next;
    ++%sums{$test + $test};
    %sums.push: %these;
    ++$index;
    $test
};

put "First 30 terms in the Mian–Chowla sequence:\n", @mian-chowla[^30];
put "\nTerms 91 through 100:\n", @mian-chowla[90..99];
```

```txt
First 30 terms in the Mian–Chowla sequence:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Terms 91 through 100:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219
```



## Phix


```Phix
function mian_chowla(integer n)
sequence mc = {1},
         is = {false,true}
    integer len_is = 2, s
    for i=2 to n do
        sequence isx = {}
        integer j = mc[i-1]+1
        mc = append(mc,j)
        while true do
            for k=1 to length(mc) do
                s = mc[k] + j
                if s<=len_is and is[s] then
                    isx = {}
                    exit
                end if
                isx = append(isx,s)
            end for
            if length(isx) then
                s = isx[$]
                if s>len_is then
                    is &= repeat(false,s-len_is)
                    len_is = length(is)
                end if
                for k=1 to length(isx) do
                    is[isx[k]] = true
                end for
                exit
            end if
            j += 1
            mc[i] = j
        end while
    end for
    return mc
end function

atom t0 = time()
sequence mc = mian_chowla(100)
printf(1,"The first 30 terms of the Mian-Chowla sequence are:\n %v\n",{mc[1..30]})
printf(1,"Terms 91 to 100 of the Mian-Chowla sequence are:\n %v\n",{mc[91..100]})
printf(1,"completed in %s\n",{elapsed(time()-t0)})
```

```txt

The first 30 terms of the Mian-Chowla sequence are:
 {1,2,4,8,13,21,31,45,66,81,97,123,148,182,204,252,290,361,401,475,565,593,662,775,822,916,970,1016,1159,1312}
Terms 91 to 100 of the Mian-Chowla sequence are:
 {22526,23291,23564,23881,24596,24768,25631,26037,26255,27219}
completed in 0.1s

```



## Python


### Procedural


```python
from itertools import count, islice, chain
import time

def mian_chowla():
    mc = [1]
    yield mc[-1]
    psums = set([2])
    newsums = set([])
    for trial in count(2):
        for n in chain(mc, [trial]):
            sum = n + trial
            if sum in psums:
                newsums.clear()
                break
            newsums.add(sum)
        else:
            psums |= newsums
            newsums.clear()
            mc.append(trial)
            yield trial

def pretty(p, t, s, f):
    print(p, t, " ".join(str(n) for n in (islice(mian_chowla(), s, f))))

if __name__ == '__main__':
    st = time.time()
    ts = "of the Mian-Chowla sequence are:\n"
    pretty("The first 30 terms", ts, 0, 30)
    pretty("\nTerms 91 to 100", ts, 90, 100)
    print("\nComputation time was", (time.time()-st) * 1000, "ms")
```

```txt
The first 30 terms of the Mian-Chowla sequence are:
 1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Terms 91 to 100 of the Mian-Chowla sequence are:
 22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

Computation time was 53.58004570007324 ms
```



### Functional

```python
'''Mian-Chowla series'''

from itertools import (islice)
from time import time


# mianChowlas :: Gen [Int]
def mianChowlas():
    '''Mian-Chowla series - Generator constructor
    '''
    mcs = [1]
    sumSet = set([2])
    x = 1
    while True:
        yield x
        (sumSet, mcs, x) = nextMC(sumSet, mcs, x)


# nextMC :: (Set Int, [Int], Int) -> (Set Int, [Int], Int)
def nextMC(setSums, mcs, n):
    '''(Set of sums, series so far, current term) ->
        (updated sum set, updated series, next term)
    '''
    def valid(x):
        for m in mcs:
            if x + m in setSums:
                return False
        return True

    x = until(valid)(succ)(n)
    setSums.update(
        [x + y for y in mcs] + [2 * x]
    )
    return (setSums, mcs + [x], x)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tests'''

    start = time()
    genMianChowlas = mianChowlas()
    print(
        'First 30 terms of the Mian-Chowla series:\n',
        take(30)(genMianChowlas)
    )
    drop(60)(genMianChowlas)
    print(
        '\n\nTerms 91 to 100 of the Mian-Chowla series:\n',
        take(10)(genMianChowlas),
        '\n'
    )
    print(
        '(Computation time c. ' + str(round(
            1000 * (time() - start)
        )) + ' ms)'
    )


# GENERIC -------------------------------------------------

# drop :: Int -> [a] -> [a]
# drop :: Int -> String -> String
def drop(n):
    '''The suffix of xs after the
       first n elements, or [] if n > length xs'''
    def go(xs):
        if isinstance(xs, list):
            return xs[n:]
        else:
            take(n)(xs)
            return xs
    return lambda xs: go(xs)


# succ :: Int -> Int
def succ(x):
    '''The successor of a numeric value (1 +)'''
    return 1 + x


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of applying f until p holds.
       The initial seed value is x.'''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


if __name__ == '__main__':
    main()
```

```txt
First 30 terms of the Mian-Chowla series:
 [1, 2, 4, 8, 13, 21, 31, 45, 66, 81, 97, 123, 148, 182, 204, 252, 290, 361, 401, 475, 565, 593, 662, 775, 822, 916, 970, 1016, 1159, 1312]

Terms 91 to 100 of the Mian-Chowla series:
 [22526, 23291, 23564, 23881, 24596, 24768, 25631, 26037, 26255, 27219]

(Computation time c. 27 ms)
```



## REXX

Programming note:   the   '''do'''   loop   (line ten):
       do j=i  for t-i+1;  ···
can be coded as:
       do j=i  to t;       ···
but the 1<sup>st</sup> version is faster.

```rexx
/*REXX program  computes and displays  any range of the  Mian─Chowla  integer sequence. */
parse arg LO HI .                                /*obtain optional arguments from the CL*/
if LO=='' | LO==","  then LO=  1                 /*Not specified?  Then use the default.*/
if HI=='' | HI==","  then HI= 30                 /* "       "        "   "   "     "    */
r.= 0                                            /*initialize the rejects stemmed array.*/
#= 0                                             /*count of numbers in sequence (so far)*/
$=                                               /*the Mian─Chowla sequence  (so far).  */
   do t=1  until #=HI;      !.= r.0              /*process numbers until range is filled*/
     do i=1    for t;       if r.i  then iterate /*I  already rejected?  Then ignore it.*/
       do j=i  for t-i+1;   if r.j  then iterate /*J     "        "        "     "    " */
       _= i + j                                  /*calculate the sum of   I   and   J.  */
       if !._  then do;  r.t= 1; iterate t;  end /*reject  T  from the Mian─Chowla seq. */
       !._= 1                                    /*mark _ as one of the sums in sequence*/
       end   /*j*/
     end     /*i*/
   #= # + 1                                      /*bump the counter of terms in the list*/
   if #>=LO  &  #<=HI  then $= $ t               /*In the specified range?  Add to list.*/
   end       /*t*/

say 'The Mian─Chowla sequence for terms '      LO      "──►"       HI      ' (inclusive):'
say strip($)                                     /*ignore the leading superfluous blank.*/
```

```txt

The Mian─Chowla sequence for terms  1 ──► 30  (inclusive):
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

```

```txt

The Mian─Chowla sequence for terms 91 ──► 100  (inclusive):
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

```



## Ruby

```ruby
require 'set'
n, ts, mc, sums = 100, [], [1], Set.new
sums << 2
st = Time.now
for i in (1 .. (n-1))
   for j in mc[i-1]+1 .. Float::INFINITY
      mc[i] = j
      for k in (0 .. i)
         if (sums.include?(sum = mc[k]+j))
            ts.clear
            break
         end
         ts << sum
      end
      if (ts.length > 0)
         sums = sums | ts
         break
      end
   end
end
et = (Time.now - st) * 1000
s = " of the Mian-Chowla sequence are:\n"
puts "The first 30 terms#{s}#{mc.slice(0..29).join(' ')}\n\n"
puts "Terms 91 to 100#{s}#{mc.slice(90..99).join(' ')}\n\n"
puts "Computation time was #{et.round(1)}ms."
```

```txt
The first 30 terms of the Mian-Chowla sequence are:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Terms 91 to 100 of the Mian-Chowla sequence are:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

Computation time was 63.0ms.
```


Or using an Enumerator:


```ruby
mian_chowla = Enumerator.new do |yielder|
  mc, sums  = [1], {}
  1.step do |n|
    mc << n
    if  mc.none?{|k| sums[k+n] } then
      mc.each{|k| sums[k+n] = true }
      yielder << n
    else
      mc.pop # n didn't work, get rid of it.
    end
  end
end

res = mian_chowla.take(100).to_a

s = " of the Mian-Chowla sequence are:\n"
puts "The first 30 terms#{s}#{res[0,30].join(' ')}\n
Terms 91 to 100#{s}#{res[90,10].join(' ')}"

```



## Sidef

```ruby
var (n, sums, ts, mc) = (100, Set([2]), [], [1])
var st = Time.micro_sec
for i in (1 ..^ n) {
   for j in (mc[i-1]+1 .. Inf) {
      mc[i] = j
      for k in (0 .. i) {
         var sum = mc[k]+j
         if (sums.exists(sum)) {
            ts.clear
            break
         }
         ts << sum
      }
      if (ts.len > 0) {
         sums = (sums|Set(ts...))
         break
      }
   }
}
var et = (Time.micro_sec - st)
var s = " of the Mian-Chowla sequence are:\n"
say "The first 30 terms#{s}#{mc.ft(0, 29).join(' ')}\n"
say "Terms 91 to 100#{s}#{mc.ft(90, 99).join(' ')}\n"
say "Computation time was #{et} seconds."
```

```txt
The first 30 terms of the Mian-Chowla sequence are:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Terms 91 to 100 of the Mian-Chowla sequence are:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

Computation time was 3.9831 seconds.
```



## Swift


```Swift
public func mianChowla(n: Int) -> [Int] {
  var mc = Array(repeating: 0, count: n)
  var ls = [2: true]
  var sum = 0

  mc[0] = 1

  for i in 1..<n {
    var lsx = [Int]()

    jLoop: for j in (mc[i-1]+1)... {
      mc[i] = j

      for k in 0...i {
        sum = mc[k] + j

        if ls[sum] ?? false {
          lsx = []
          continue jLoop
        }

        lsx.append(sum)
      }

      for n in lsx {
        ls[n] = true
      }

      break
    }
  }

  return mc
}

let seq = mianChowla(n: 100)

print("First 30 terms in sequence are: \(Array(seq.prefix(30)))")
print("Terms 91 to 100 are: \(Array(seq[90..<100]))")
```


```txt
First 30 terms in sequence are: [1, 2, 4, 8, 13, 21, 31, 45, 66, 81, 97, 123, 148, 182, 204, 252, 290, 361, 401, 475, 565, 593, 662, 775, 822, 916, 970, 1016, 1159, 1312]
Terms 91 to 100 are: [22526, 23291, 23564, 23881, 24596, 24768, 25631, 26037, 26255, 27219]
```



## VBScript


```vb
' Mian-Chowla sequence - VBScript - 15/03/2019
    Const m = 100, mm=28000
    ReDim r(mm), v(mm * 2)
    Dim n, t, i, j, l, s1, s2, iterate_t
    ReDim seq(m)
    t0=Timer
    s1 = "1": s2 = ""
    seq(1) = 1: n = 1: t = 1
    Do While n < m
        t = t + 1
        iterate_t = False
        For i = 1 to t * 2
            v(i) = 0
        Next
        i = 1
        Do While i <= t And Not iterate_t
            If r(i) = 0 Then
                j = i
                Do While j <= t And Not iterate_t
                    If r(j) = 0 Then
                        l = i + j
                        If v(l) = 1 Then
                            r(t) = 1
                            iterate_t = True
                        End If
                        If Not iterate_t Then v(l) = 1
                    End If
                    j = j + 1
                Loop
            End If
            i = i + 1
        Loop
        If Not iterate_t Then
            n = n + 1
            seq(n) = t
            if           n<= 30 then s1 = s1 & " " & t
            if n>=91 and n<=100 then s2 = s2 & " " & t
        End If
    Loop
    wscript.echo "t="& t
    wscript.echo "The Mian-Chowla sequence for elements 1 to 30:"
    wscript.echo s1
    wscript.echo "The Mian-Chowla sequence for elements 91 to 100:"
    wscript.echo s2
    wscript.echo "Computation time: "&  Int(Timer-t0) &" sec"
```

```txt

The Mian-Chowla sequence for elements 1 to 30:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312
The Mian-Chowla sequence for elements 91 to 100:
 22526 23291 23564 23881 24596 24768 25631 26037 26255 27219
Computation time: 2381 sec

```

Execution time: 40 min

'''Shorter execution time'''
```vb
' Mian-Chowla sequence - VBScript - March 19th, 2019

    Function Find(x(), val) ' finds val on a pre-sorted list
        Dim l, u, h : l = 0 : u = ubound(x) : Do : h = (l + u) \ 2
            If val = x(h) Then Find = h : Exit Function
            If val > x(h) Then l = h + 1 Else u = h - 1
        Loop Until l > u : Find = -1
    End Function

    ' adds next item from a() to result (r()), adds all remaining items
    ' from b(), once a() is exhausted
    Sub Shuffle(ByRef r(), a(), b(), ByRef i, ByRef ai, ByRef bi, al, bl)
        r(i) = a(ai) : ai = ai + 1 : If ai > al Then Do : i = i + 1 : _
            r(i) = b(bi) : bi = bi + 1 : Loop until bi = bl
    End Sub

    Function Merger(a(), b(), bl) ' merges two pre-sorted lists
        Dim res(), ai, bi, i : ReDim res(ubound(a) + bl) : ai = 0 : bi = 0
        For i = 0 To ubound(res)
            If a(ai) < b(bi) Then Shuffle res, a, b, i, ai, bi, ubound(a), bl _
            Else Shuffle res, b, a, i, bi, ai, bl, ubound(a)
        Next : Merger = res
    End Function

    Const n = 100 : Dim mc(), sums(), ts(), sp, tc : sp = 1 : tc = 0
    ReDim mc(n - 1), sums(0), ts(n - 1) : mc(0) = 1 : sums(sp - 1) = 2
    Dim sum, i, j, k, st : st = Timer
    wscript.echo "The Mian-Chowla sequence for elements 1 to 30:"
    wscript.stdout.write("1 ")
    For i = 1 To n - 1 : j = mc(i - 1) + 1 : Do
            mc(i) = j : For k = 0 To i
                sum = mc(k) + j : If Find(sums, sum) >= 0 Then _
                    tc = 0 : Exit For Else ts(tc) = sum : tc = tc + 1
            Next : If tc > 0 Then
              nu = Merger(sums, ts, tc) : ReDim sums(ubound(nu))
              For e = 0 To ubound(nu) : sums(e) = nu(e) : Next
              tc = 0 : Exit Do
            End If : j = j + 1 : Loop
        if i = 90 then wscript.echo vblf & vbLf & _
            "The Mian-Chowla sequence for elements 91 to 100:"
        If i < 30 or i >= 90 Then wscript.stdout.write(mc(i) & " ")
    Next
    wscript.echo vblf & vbLf & "Computation time: "& Timer - st &" seconds."
```

''Hint:'' save the code to a .vbs file (such as "mc.vbs") and start it with this command Line: "cscript.exe /nologo mc.vbs".  This will send the output to the console instead of a series of message boxes.<br/>This goes faster because the cache of sums is maintained throughout the computation instead of being reinitialized at each iteration.  Also the ''sums()'' array is kept sorted to find any previous values quicker.

```txt
The Mian-Chowla sequence for elements 1 to 30:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

The Mian-Chowla sequence for elements 91 to 100:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

Computation time: 1.328125 seconds.
```



## Visual Basic .NET

```vbnet
Module Module1
Function MianChowla(ByVal n As Integer) As Integer()
        Dim mc(n - 1) As Integer, sums, ts As New HashSet(Of Integer),
        sum As Integer : mc(0) = 1 : sums.Add(2)
        For i As Integer = 1 To n - 1
            For j As Integer = mc(i - 1) + 1 To Integer.MaxValue
                mc(i) = j
                For k As Integer = 0 To i
                    sum = mc(k) + j
                    If sums.Contains(sum) Then ts.Clear() : Exit For
                    ts.Add(sum)
                Next
                If ts.Count > 0 Then sums.UnionWith(ts) : Exit For
            Next
        Next
        Return mc
    End Function

    Sub Main(ByVal args As String())
        Const n As Integer = 100
        Dim sw As New Stopwatch(), str As String = " of the Mian-Chowla sequence are:" & vbLf
        sw.Start() : Dim mc As Integer() = MianChowla(n) : sw.Stop()
        Console.Write("The first 30 terms{1}{2}{0}{0}Terms 91 to 100{1}{3}{0}{0}" &
            "Computation time was {4}ms.{0}", vbLf, str,
            String.Join(" ", mc.Take(30)), String.Join(" ", mc.Skip(n - 10)), sw.ElapsedMilliseconds)
    End Sub
End Module
```

```txt
The first 30 terms of the Mian-Chowla sequence are:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 204 252 290 361 401 475 565 593 662 775 822 916 970 1016 1159 1312

Terms 91 to 100 of the Mian-Chowla sequence are:
22526 23291 23564 23881 24596 24768 25631 26037 26255 27219

Computation time was 18ms.
```

