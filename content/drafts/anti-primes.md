+++
title = "Anti-primes"
description = ""
date = 2019-09-18T01:52:50Z
aliases = []
[extra]
id = 22097
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Prime Numbers]]

The [https://youtu.be/2JM2oImb9Qg anti-primes]
(or [https://en.wikipedia.org/wiki/Highly_composite_number highly composite numbers], sequence [https://oeis.org/A002182 A002182] in the [https://oeis.org/ OEIS])
are the natural numbers with more factors than any smaller than itself.

;Task:
Generate and show here, the first twenty anti-primes.

;Related tasks:
# [[Factors of an integer]]
# [[Sieve of Eratosthenes]]


## 11l


```11l
V max_divisors = 0
V c = 0
V n = 1
L
   V divisors = 1
   L(i) 1 .. n I/ 2
      I n % i == 0
         divisors++

   I divisors > max_divisors
      max_divisors = divisors
      print(n, end' ‘ ’)
      c++
      I c == 20
         L.break

   n++
```

{{out}}

```txt
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560
```


## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Antiprimes is

   function Count_Divisors (N : Integer) return Integer is
      Count : Integer := 1;
   begin
      for i in 1 .. N / 2 loop
         if N mod i = 0 then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Divisors;

   Results      : array (1 .. 20) of Integer;
   Candidate    : Integer := 1;
   Divisors     : Integer;
   Max_Divisors : Integer := 0;

begin
   for i in Results'Range loop
      loop
         Divisors := Count_Divisors (Candidate);
         if Max_Divisors < Divisors then
            Results (i)  := Candidate;
            Max_Divisors := Divisors;
            exit;
         end if;
         Candidate := Candidate + 1;
      end loop;
   end loop;
   Put_Line ("The first 20 anti-primes are:");
   for I in Results'Range loop
      Put (Integer'Image (Results (I)));
   end loop;
   New_Line;
end Antiprimes;
```

{{out}}

```txt

The first 20 anti-primes are:
 1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## AWK

{{trans|Go}}
```AWK
# syntax: GAWK -f ANTI-PRIMES.AWK
BEGIN {
    print("The first 20 anti-primes are:")
    while (count < 20) {
      d = count_divisors(++n)
      if (d > max_divisors) {
        printf("%d ",n)
        max_divisors = d
        count++
      }
    }
    printf("\n")
    exit(0)
}
function count_divisors(n,  count,i) {
    if (n < 2) {
      return(1)
    }
    count = 2
    for (i=2; i<=n/2; i++) {
      if (n % i == 0) {
        count++
      }
    }
    return(count)
}
```

{{out}}

```txt
The first 20 anti-primes are:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560
```



## BASIC256


```BASIC256

Dim Results(20)
Candidate = 1
max_divisors = 0

Print "Los primeros 20 anti-primos son:"
For j = 0 To 19
	Do
		divisors = count_divisors(Candidate)
		If max_divisors < divisors Then
			Results[j] = Candidate
			max_divisors = divisors
			Exit Do
		End If
		Candidate += 1
	Until false
	Print Results[j];" ";
Next j

Function count_divisors(n)
	cont = 1
	For i = 1 To n/2
		If (n % i) = 0 Then cont += 1
	Next i
	count_divisors = cont
End Function

```

{{out}}

```txt

Los primeros 20 anti-primos son:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## C

{{trans|Go}}

```c
#include <stdio.h>

int countDivisors(int n) {
    int i, count;
    if (n < 2) return 1;
    count = 2; // 1 and n
    for (i = 2; i <= n/2; ++i) {
        if (n%i == 0) ++count;
    }
    return count;
}

int main() {
    int n, d, maxDiv = 0, count = 0;
    printf("The first 20 anti-primes are:\n");
    for (n = 1; count < 20; ++n) {
        d = countDivisors(n);
        if (d > maxDiv) {
            printf("%d ", n);
            maxDiv = d;
            count++;
        }
    }
    printf("\n");
    return 0;
}
```

{{out}}

```txt

The first 20 anti-primes are:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```




## C++

{{trans|C}}

```cpp
#include <iostream>

int countDivisors(int n) {
    if (n < 2) return 1;
    int count = 2; // 1 and n
    for (int i = 2; i <= n/2; ++i) {
        if (n%i == 0) ++count;
    }
    return count;
}

int main() {
    int maxDiv = 0, count = 0;
    std::cout << "The first 20 anti-primes are:" << std::endl;
    for (int n = 1; count < 20; ++n) {
        int d = countDivisors(n);
        if (d > maxDiv) {
            std::cout << n << " ";
            maxDiv = d;
            count++;
        }
    }
    std::cout << std::endl;
    return 0;
}
```


{{out}}

```txt

The first 20 anti-primes are:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## C#

{{works with|C sharp|7}}

```c#
using System;
using System.Linq;
using System.Collections.Generic;

public static class Program
{
    public static void Main() =>
        Console.WriteLine(string.Join(" ", FindAntiPrimes().Take(20)));

    static IEnumerable<int> FindAntiPrimes() {
        int max = 0;
        for (int i = 1; ; i++) {
            int divisors = CountDivisors(i);
            if (divisors > max) {
                max = divisors;
                yield return i;
            }
        }

        int CountDivisors(int n) => Enumerable.Range(1, n / 2).Count(i => n % i == 0) + 1;
    }
}
```

{{out}}

```txt

1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## D

{{trans|C++}}

```d
import std.stdio;

int countDivisors(int n) {
    if (n < 2) {
        return 1;
    }
    int count = 2; // 1 and n
    for (int i = 2; i <= n/2; ++i) {
        if (n % i == 0) {
            ++count;
        }
    }
    return count;
}

void main() {
    int maxDiv, count;
    writeln("The first 20 anti-primes are:");
    for (int n = 1; count < 20; ++n) {
        int d = countDivisors(n);
        if (d > maxDiv) {
            write(n, ' ');
            maxDiv = d;
            count++;
        }
    }
    writeln;
}
```

{{out}}

```txt
The first 20 anti-primes are:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560
```




## Erlang


```erlang
divcount(N) -> divcount(N, 1, 0).

divcount(N, D, Count) when D*D > N -> Count;
divcount(N, D, Count) ->
    Divs = case N rem D of
        0 ->
            case N - D*D of
                0 -> 1;
                _ -> 2
            end;
        _ -> 0
    end,
    divcount(N, D + 1, Count + Divs).


antiprimes(N) -> antiprimes(N, 1, 0, []).

antiprimes(0, _, _, L) -> lists:reverse(L);
antiprimes(N, M, Max, L) ->
    Count = divcount(M),
    case Count > Max of
        true  -> antiprimes(N-1, M+1, Count, [M|L]);
        false -> antiprimes(N, M+1, Max, L)
    end.


main(_) ->
    io:format("The first 20 anti-primes are ~w~n", [antiprimes(20)]).

```

{{out}}

```txt

The first 20 anti-primes are [1,2,4,6,12,24,36,48,60,120,180,240,360,720,840,1260,1680,2520,5040,7560]

```



## Factor


```factor
USING: assocs formatting kernel locals make math
math.primes.factors sequences.extras ;
IN: rosetta-code.anti-primes

<PRIVATE

: count-divisors ( n -- m )
    dup 1 = [ group-factors values [ 1 + ] map-product ] unless ;

: (n-anti-primes) ( md n count -- ?md' n' ?count' )
    dup 0 >
    [| max-div! n count! |
        n count-divisors :> d
        d max-div > [ d max-div! n , count 1 - count! ] when
        max-div n dup 60 >= 20 1 ? + count (n-anti-primes)
    ] when ;

PRIVATE>

: n-anti-primes ( n -- seq )
    [ 0 1 ] dip [ (n-anti-primes) 3drop ] { } make ;

: anti-primes-demo ( -- )
    20 n-anti-primes "First 20 anti-primes:\n%[%d, %]\n" printf ;

MAIN: anti-primes-demo
```

{{out}}

```txt

First 20 anti-primes:
{ 1, 2, 4, 6, 12, 24, 36, 48, 60, 120, 180, 240, 360, 720, 840, 1260, 1680, 2520, 5040, 7560 }

```



## FreeBASIC


```freebasic

' convertido desde Ada
Declare Function count_divisors(n As Integer) As Integer

Dim As Integer max_divisors, divisors, results(1 To 20), candidate, j
candidate = 1

Function count_divisors(n As Integer) As Integer
    Dim As Integer i, count = 1
    For i = 1 To n/2
        If (n Mod i) = 0 Then count += 1
    Next i
    count_divisors = count
End Function

Print "Los primeros 20 anti-primos son:"
For j = 1 To 20
    Do
        divisors = count_divisors(Candidate)
        If max_divisors < divisors Then
            Results(j) = Candidate
            max_divisors = divisors
            Exit Do
        End If
        Candidate += 1
    Loop
Next j
For j = 1 To 20
    Print Results(j);
Next j
Print
Sleep

```

{{out}}

```txt

Los primeros 20 anti-primos son:
 1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```


=={{header|F_Sharp|F#}}==

### The Function

This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Find Antı-Primes. Nigel Galloway: Secember 10th., 2018
let  N=200000000000000000000000000I
let fI,_=Seq.scan(fun (_,g) e->(e,e*g)) (2I,4I) (primes|>Seq.skip 1|>Seq.map bigint)|>Seq.takeWhile(fun(_,n)->n<N)|>List.ofSeq|>List.unzip
let fG g=Seq.unfold(fun ((n,i,e) as z)->Some(z,(n+1,i+1,(e*g)))) (1,2,g)|>Seq.takeWhile(fun(_,_,n)->n<N)
let fE n i=n|>Seq.collect(fun(n,e,g)->Seq.map(fun(a,c,b)->(a,c*e,g*b)) (i|>Seq.takeWhile(fun(g,_,_)->g<=n)) |> Seq.takeWhile(fun(_,_,n)->n<N))
let fL,_=Seq.concat(Seq.scan(fun n g->fE n (fG g)) (seq[(2147483647,1,1I)]) fI)|>List.ofSeq|>List.sortBy(fun(_,_,n)->n)|>List.fold(fun ((a,b) as z) (_,n,g)->if n>b then ((n,g)::a,n) else z) ([],0)

```


### The Task


```fsharp

printfn "The first 20 anti-primes are :-"; for (_,g) in (List.rev fL)|>List.take 20 do printfn "%A" g

```

{{out}}

```txt

The first 20 anti-primes are :-
1
2
4
6
12
24
36
48
60
120
180
240
360
720
840
1260
1680
2520
5040
7560

```


### Extra Credit


```fsharp

printfn "There are %d anti-primes less than %A:-" (List.length fL) N; for (n,g) in (List.rev fL) do printfn "%A has %d dividers" g n

```

{{out}}
<pre style="height:35ex">
There are 245 anti-primes less than 200000000000000000000000000:-
1 has 1 dividers
2 has 2 dividers
4 has 3 dividers
6 has 4 dividers
12 has 6 dividers
24 has 8 dividers
36 has 9 dividers
48 has 10 dividers
60 has 12 dividers
120 has 16 dividers
180 has 18 dividers
240 has 20 dividers
360 has 24 dividers
720 has 30 dividers
840 has 32 dividers
1260 has 36 dividers
1680 has 40 dividers
2520 has 48 dividers
5040 has 60 dividers
7560 has 64 dividers
10080 has 72 dividers
15120 has 80 dividers
20160 has 84 dividers
25200 has 90 dividers
27720 has 96 dividers
45360 has 100 dividers
50400 has 108 dividers
55440 has 120 dividers
83160 has 128 dividers
110880 has 144 dividers
166320 has 160 dividers
221760 has 168 dividers
277200 has 180 dividers
332640 has 192 dividers
498960 has 200 dividers
554400 has 216 dividers
665280 has 224 dividers
720720 has 240 dividers
1081080 has 256 dividers
1441440 has 288 dividers
2162160 has 320 dividers
2882880 has 336 dividers
3603600 has 360 dividers
4324320 has 384 dividers
6486480 has 400 dividers
7207200 has 432 dividers
8648640 has 448 dividers
10810800 has 480 dividers
14414400 has 504 dividers
17297280 has 512 dividers
21621600 has 576 dividers
32432400 has 600 dividers
36756720 has 640 dividers
43243200 has 672 dividers
61261200 has 720 dividers
73513440 has 768 dividers
110270160 has 800 dividers
122522400 has 864 dividers
147026880 has 896 dividers
183783600 has 960 dividers
245044800 has 1008 dividers
294053760 has 1024 dividers
367567200 has 1152 dividers
551350800 has 1200 dividers
698377680 has 1280 dividers
735134400 has 1344 dividers
1102701600 has 1440 dividers
1396755360 has 1536 dividers
2095133040 has 1600 dividers
2205403200 has 1680 dividers
2327925600 has 1728 dividers
2793510720 has 1792 dividers
3491888400 has 1920 dividers
4655851200 has 2016 dividers
5587021440 has 2048 dividers
6983776800 has 2304 dividers
10475665200 has 2400 dividers
13967553600 has 2688 dividers
20951330400 has 2880 dividers
27935107200 has 3072 dividers
41902660800 has 3360 dividers
48886437600 has 3456 dividers
64250746560 has 3584 dividers
73329656400 has 3600 dividers
80313433200 has 3840 dividers
97772875200 has 4032 dividers
128501493120 has 4096 dividers
146659312800 has 4320 dividers
160626866400 has 4608 dividers
240940299600 has 4800 dividers
293318625600 has 5040 dividers
321253732800 has 5376 dividers
481880599200 has 5760 dividers
642507465600 has 6144 dividers
963761198400 has 6720 dividers
1124388064800 has 6912 dividers
1606268664000 has 7168 dividers
1686582097200 has 7200 dividers
1927522396800 has 7680 dividers
2248776129600 has 8064 dividers
3212537328000 has 8192 dividers
3373164194400 has 8640 dividers
4497552259200 has 9216 dividers
6746328388800 has 10080 dividers
8995104518400 has 10368 dividers
9316358251200 has 10752 dividers
13492656777600 has 11520 dividers
18632716502400 has 12288 dividers
26985313555200 has 12960 dividers
27949074753600 has 13440 dividers
32607253879200 has 13824 dividers
46581791256000 has 14336 dividers
48910880818800 has 14400 dividers
55898149507200 has 15360 dividers
65214507758400 has 16128 dividers
93163582512000 has 16384 dividers
97821761637600 has 17280 dividers
130429015516800 has 18432 dividers
195643523275200 has 20160 dividers
260858031033600 has 20736 dividers
288807105787200 has 21504 dividers
391287046550400 has 23040 dividers
577614211574400 has 24576 dividers
782574093100800 has 25920 dividers
866421317361600 has 26880 dividers
1010824870255200 has 27648 dividers
1444035528936000 has 28672 dividers
1516237305382800 has 28800 dividers
1732842634723200 has 30720 dividers
2021649740510400 has 32256 dividers
2888071057872000 has 32768 dividers
3032474610765600 has 34560 dividers
4043299481020800 has 36864 dividers
6064949221531200 has 40320 dividers
8086598962041600 has 41472 dividers
10108248702552000 has 43008 dividers
12129898443062400 has 46080 dividers
18194847664593600 has 48384 dividers
20216497405104000 has 49152 dividers
24259796886124800 has 51840 dividers
30324746107656000 has 53760 dividers
36389695329187200 has 55296 dividers
48519593772249600 has 57600 dividers
60649492215312000 has 61440 dividers
72779390658374400 has 62208 dividers
74801040398884800 has 64512 dividers
106858629141264000 has 65536 dividers
112201560598327200 has 69120 dividers
149602080797769600 has 73728 dividers
224403121196654400 has 80640 dividers
299204161595539200 has 82944 dividers
374005201994424000 has 86016 dividers
448806242393308800 has 92160 dividers
673209363589963200 has 96768 dividers
748010403988848000 has 98304 dividers
897612484786617600 has 103680 dividers
1122015605983272000 has 107520 dividers
1346418727179926400 has 110592 dividers
1795224969573235200 has 115200 dividers
2244031211966544000 has 122880 dividers
2692837454359852800 has 124416 dividers
3066842656354276800 has 129024 dividers
4381203794791824000 has 131072 dividers
4488062423933088000 has 138240 dividers
6133685312708553600 has 147456 dividers
8976124847866176000 has 153600 dividers
9200527969062830400 has 161280 dividers
12267370625417107200 has 165888 dividers
15334213281771384000 has 172032 dividers
18401055938125660800 has 184320 dividers
27601583907188491200 has 193536 dividers
30668426563542768000 has 196608 dividers
36802111876251321600 has 207360 dividers
46002639845314152000 has 215040 dividers
55203167814376982400 has 221184 dividers
73604223752502643200 has 230400 dividers
92005279690628304000 has 245760 dividers
110406335628753964800 has 248832 dividers
131874234223233902400 has 258048 dividers
184010559381256608000 has 276480 dividers
263748468446467804800 has 294912 dividers
368021118762513216000 has 307200 dividers
395622702669701707200 has 322560 dividers
527496936892935609600 has 331776 dividers
659371171116169512000 has 344064 dividers
791245405339403414400 has 368640 dividers
1186868108009105121600 has 387072 dividers
1318742342232339024000 has 393216 dividers
1582490810678806828800 has 414720 dividers
1978113513348508536000 has 430080 dividers
2373736216018210243200 has 442368 dividers
3164981621357613657600 has 460800 dividers
3956227026697017072000 has 491520 dividers
4747472432036420486400 has 497664 dividers
5934340540045525608000 has 516096 dividers
7912454053394034144000 has 552960 dividers
11868681080091051216000 has 589824 dividers
15824908106788068288000 has 614400 dividers
17407398917466875116800 has 622080 dividers
18594267025475980238400 has 645120 dividers
23737362160182102432000 has 663552 dividers
30990445042459967064000 has 688128 dividers
34814797834933750233600 has 691200 dividers
37188534050951960476800 has 737280 dividers
52222196752400625350400 has 746496 dividers
55782801076427940715200 has 774144 dividers
61980890084919934128000 has 786432 dividers
74377068101903920953600 has 829440 dividers
92971335127379901192000 has 860160 dividers
111565602152855881430400 has 884736 dividers
148754136203807841907200 has 921600 dividers
185942670254759802384000 has 983040 dividers
223131204305711762860800 has 995328 dividers
278914005382139703576000 has 1032192 dividers
371885340509519604768000 has 1105920 dividers
557828010764279407152000 has 1179648 dividers
743770681019039209536000 has 1228800 dividers
818147749120943130489600 has 1244160 dividers
985496152350226952635200 has 1290240 dividers
1115656021528558814304000 has 1327104 dividers
1487541362038078419072000 has 1351680 dividers
1636295498241886260979200 has 1382400 dividers
1970992304700453905270400 has 1474560 dividers
2454443247362829391468800 has 1492992 dividers
2956488457050680857905600 has 1548288 dividers
3284987174500756508784000 has 1572864 dividers
3941984609400907810540800 has 1658880 dividers
4927480761751134763176000 has 1720320 dividers
5912976914101361715811200 has 1769472 dividers
7883969218801815621081600 has 1843200 dividers
9854961523502269526352000 has 1966080 dividers
11825953828202723431622400 has 1990656 dividers
14782442285253404289528000 has 2064384 dividers
19709923047004539052704000 has 2211840 dividers
29564884570506808579056000 has 2359296 dividers
39419846094009078105408000 has 2457600 dividers
43361830703409985915948800 has 2488320 dividers
54202288379262482394936000 has 2580480 dividers
59129769141013617158112000 has 2654208 dividers
78839692188018156210816000 has 2703360 dividers
86723661406819971831897600 has 2764800 dividers
108404576758524964789872000 has 2949120 dividers
130085492110229957747846400 has 2985984 dividers
162606865137787447184808000 has 3096576 dividers
193814243295544634018256000 has 3145728 dividers

```



## Go

Simple brute force approach which is quick enough here.

```go
package main

import "fmt"

func countDivisors(n int) int {
    if n < 2 {
        return 1
    }
    count := 2 // 1 and n
    for i := 2; i <= n/2; i++ {
        if n%i == 0 {
            count++
        }
    }
    return count
}

func main() {
    fmt.Println("The first 20 anti-primes are:")
    maxDiv := 0
    count := 0
    for n := 1; count < 20; n++ {
        d := countDivisors(n)
        if d > maxDiv {
            fmt.Printf("%d ", n)
            maxDiv = d
            count++
        }
    }
    fmt.Println()
}
```


{{out}}

```txt

The first 20 anti-primes are:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## Groovy

Solution (uses [[Factors_of_an_integer#Groovy|Factors of an integer]] function "factorize()"):

```groovy
def getAntiPrimes(def limit = 10) {
    def antiPrimes = []
    def candidate = 1L
    def maxFactors = 0

    while (antiPrimes.size() < limit) {
        def factors = factorize(candidate)
        if (factors.size() > maxFactors) {
            maxFactors = factors.size()
            antiPrimes << candidate
        }
        candidate++
    }
    antiPrimes
}
```


Test:

```groovy
println (getAntiPrimes(20))
```


Output:

```txt
[1, 2, 4, 6, 12, 24, 36, 48, 60, 120, 180, 240, 360, 720, 840, 1260, 1680, 2520, 5040, 7560]
```



## Haskell


```haskell
import Data.List (find, group)
import Data.Maybe (fromJust)

firstPrimeFactor :: Int -> Int
firstPrimeFactor n = head $ filter ((0 ==) . mod n) [2 .. n]

allPrimeFactors :: Int -> [Int]
allPrimeFactors 1 = []
allPrimeFactors n =
  let first = firstPrimeFactor n
  in first : allPrimeFactors (n `div` first)

factorCount :: Int -> Int
factorCount 1 = 1
factorCount n = product ((succ . length) <$> group (allPrimeFactors n))

divisorCount :: Int -> (Int, Int)
divisorCount = (,) <*> factorCount

hcnNext :: (Int, Int) -> (Int, Int)
hcnNext (num, factors) =
  fromJust $ find ((> factors) . snd) (divisorCount <$> [num ..])

hcnSequence :: [Int]
hcnSequence = fst <$> iterate hcnNext (1, 1)

main :: IO ()
main = print $ take 20 hcnSequence
```

{{output}}

```txt

[1,2,4,6,12,24,36,48,60,120,180,240,360,720,840,1260,1680,2520,5040,7560]

```



## J


```J

   NB. factor count is the product of the incremented powers of prime factors
   factor_count =: [: */ [: >: _&q:

   NB. N are the integers 1 to 10000
   NB. FC are the corresponding factor counts
   FC =: factor_count&> N=: >: i. 10000

   NB. take from the integers N{~
   NB. the indexes of truth   I.
   NB. the vector which doesn't equal itself when rotated by one position  (~: _1&|.)
   NB. where that vector is the maximum over all prefixes of the factor counts  >./\FC
   N{~I.(~: _1&|.)>./\FC
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## Java

{{trans|Go}}

```java
public class Antiprime {

    static int countDivisors(int n) {
        if (n < 2) return 1;
        int count = 2; // 1 and n
        for (int i = 2; i <= n/2; ++i) {
            if (n%i == 0) ++count;
        }
        return count;
    }

    public static void main(String[] args) {
        int maxDiv = 0, count = 0;
        System.out.println("The first 20 anti-primes are:");
        for (int n = 1; count < 20; ++n) {
            int d = countDivisors(n);
            if (d > maxDiv) {
                System.out.printf("%d ", n);
                maxDiv = d;
                count++;
            }
        }
        System.out.println();
    }
}
```


{{output}}

```txt

The first 20 anti-primes are:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## Javascript


```javascript

function factors(n) {
  var factors = [];
  for (var i = 1; i <= n; i++) {
    if (n % i == 0) {
      factors.push(i);
    }
  }
  return factors;
}

function generateAntiprimes(n) {
  var antiprimes = [];
  var maxFactors = 0;
  for (var i = 1; antiprimes.length < n; i++) {
    var ifactors = factors(i);
    if (ifactors.length > maxFactors) {
      antiprimes.push(i);
      maxFactors = ifactors.length;
    }
  }
  return antiprimes;
}

function go() {
  var number = document.getElementById("n").value;
  document.body.removeChild(document.getElementById("result-list"));
  document.body.appendChild(showList(generateAntiprimes(number)));
}

function showList(array) {
  var list = document.createElement("ul");
  list.id = "result-list";
  for (var i = 0; i < array.length; i++) {
    var item = document.createElement("li");
    item.appendChild(document.createTextNode(array[i]));
    list.appendChild(item);
  }
  return list;
}

```

Html to test with some styling

```txt

<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <meta http-equiv="X-UA-Compatible" content="ie=edge" />
    <script src="antiprimes.js"></script>
    <title>Anti-Primes</title>
    <style>
      body {padding: 50px;width: 50%;box-shadow: 0 0 15px 0 rgba(0, 0, 0, 0.25);margin: 15px auto;font-family: "Gill Sans", "Gill Sans MT", Calibri, "Trebuchet MS", sans-serif;letter-spacing: 1px;}
      a {color: #00aadd;text-decoration: none;}
      input {width: 50px;text-align: center;}
      ul {list-style: none;padding: 0;margin: 0;width: 25%;margin: auto;border: 1px solid #aaa;}
      li {text-align: center;background-color: #eaeaea;}
      li:nth-child(even) {background: #fff;}
    </style>
  </head>
  <body onload="go()">
    <h1>Anti-Primes</h1>
    <div class="info">
      The <a href="https://youtu.be/2JM2oImb9Qg">anti-primes</a> (or
      <a href="https://en.wikipedia.org/wiki/Highly_composite_number">highly composite numbers</a>, sequence
      <a href="https://oeis.org/A002182">A002182</a> in the <a href="https://oeis.org/">OEIS</a>) are the natural numbers with more factors than any
      smaller than itself.
    </div>
    <p>Generate first <input id="n" type="text" placeholder="Enter the number" value="20" /> anti-primes. <button onclick="go()">Go</button></p>
    <ul id="result-list"></ul>
  </body>
</html>

```



## Julia



```julia
using Primes, Combinatorics

function antiprimes(N, maxn = 2000000)
    antip = [1]  # special case: 1 is antiprime
    count = 1
    maxfactors = 1
    for i in 2:2:maxn # antiprimes > 2 should be even
        lenfac = length(unique(sort(collect(combinations(factor(Vector, i)))))) + 1
        if lenfac > maxfactors
            push!(antip, i)
            if length(antip) >= N
                return antip
            end
            maxfactors = lenfac
        end
    end
    antip
end

println("The first 20 anti-primes are:\n", antiprimes(20))
println("The first 40 anti-primes are:\n", antiprimes(40))

```
{{output}}
```txt

 The first 20 anti-primes are:
 [1, 2, 4, 6, 12, 24, 36, 48, 60, 120, 180, 240, 360, 720, 840, 1260, 1680, 2520, 5040, 7560]
 The first 40 anti-primes are:
 [1, 2, 4, 6, 12, 24, 36, 48, 60, 120, 180, 240, 360, 720, 840, 1260, 1680, 2520, 5040, 7560,
 10080, 15120, 20160, 25200, 27720, 45360, 50400, 55440, 83160, 110880, 166320, 221760, 277200,
 332640, 498960, 554400, 665280, 720720, 1081080, 1441440]

```



## Kotlin

{{trans|Go}}

```scala
// Version 1.3.10

fun countDivisors(n: Int): Int {
    if (n < 2) return 1;
    var count = 2 // 1 and n
    for (i in 2..n / 2) {
        if (n % i == 0) count++
    }
    return count;
}

fun main(args: Array<String>) {
    println("The first 20 anti-primes are:")
    var maxDiv = 0
    var count = 0
    var n = 1
    while (count < 20) {
        val d = countDivisors(n)
        if (d > maxDiv) {
            print("$n ")
            maxDiv = d
            count++
        }
        n++
    }
    println()
}
```


{{output}}

```txt

The first 20 anti-primes are:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## Lua


```lua
-- First 20 antiprimes.

function count_factors(number)
	local count = 0
	for attempt = 1, number do
		local remainder = number % attempt
		if remainder == 0 then
			count = count + 1
		end
	end
	return count
end

function antiprimes(goal)
	local list, number, mostFactors = {}, 1, 0
	while #list < goal do
		local factors = count_factors(number)
		if factors > mostFactors then
			table.insert(list, number)
			mostFactors = factors
		end
		number = number + 1
	end
	return list
end

function recite(list)
	for index, item in ipairs(list) do
		print(item)
	end
end

print("The first 20 antiprimes:")
recite(antiprimes(20))
print("Done.")

```


{{output}}

```txt
The first 20 antiprimes:
1
2
4
6
12
24
36
48
60
120
180
240
360
720
840
1260
1680
2520
5040
7560
Done.
```



## Maple


```Maple
antiprimes := proc(n)
local ap, i, max_divisors, num_divisors;
max_divisors := 0;
ap := [];

for i from 1 while numelems(ap) < n do
    num_divisors := numelems(NumberTheory:-Divisors(i));
    if num_divisors > max_divisors then
       ap := [op(ap), i];
       max_divisors := num_divisors;
    end if;
end do;

return ap;
end proc:
antiprimes(20);
```

{{out}}

```txt
[1, 2, 4, 6, 12, 24, 36, 48, 60, 120, 180, 240, 360, 720, 840, 1260, 1680, 2520, 5040, 7560]
```



## Nim


```nim
# First 20 antiprimes

proc countDivisors(n: int): int =
    if (n < 2):
        return 1
    var count = 2
    for i in countup(2, (n / 2).toInt()):
        if (n %% i == 0):
            count += 1
    return count

proc antiPrimes(n: int) =
    echo("The first ", n, " anti-primes:")
    var maxDiv = 0
    var count = 0
    var i = 1
    while(count < n):
        let d = countDivisors(i)
        if (d > maxDiv):
            echo(i)
            maxDiv = d
            count+=1
        i += 1

antiPrimes(20)

```

{{output}}

```txt
The first 20 anti-primes:
1
2
4
6
12
24
36
48
60
120
180
240
360
720
840
1260
1680
2520
5040
7560
```



## Objeck

{{trans|Java}}

```objeck
class AntiPrimes {
  function : Main(args : String[]) ~ Nil {
    maxDiv := 0; count := 0;
    "The first 20 anti-primes are:"->PrintLine();
    for(n := 1; count < 20; ++n;) {
      d := CountDivisors(n);
      if(d > maxDiv) {
        "{$n} "->Print();
        maxDiv := d;
        count++;
      };
    };
    '\n'->Print();
  }

  function : native : CountDivisors(n : Int) ~ Int {
    if (n < 2) { return 1; };
    count := 2;
    for(i := 2; i <= n/2; ++i;) {
      if(n%i = 0) { ++count; };
    };
    return count;
  }
}
```


{{output}}

```txt

1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## Pascal

Easy factoring without primes.Decided to show count of factors.

```pascal
program AntiPrimes;
{$IFdef FPC}
  {$MOde Delphi}
{$IFEND}
function getFactorCnt(n:NativeUint):NativeUint;
var
  divi,quot,pot,lmt : NativeUint;
begin
  result := 1;
  divi  := 1;
  lmt := trunc(sqrt(n));
  while divi < n do
  Begin
    inc(divi);
    pot := 0;
    repeat
      quot := n div divi;
      if n <> quot*divi then
        BREAK;
      n := quot;
      inc(pot);
    until false;
    result := result*(1+pot);
    //IF n= prime leave now
    if divi > lmt then
      BREAK;
  end;
end;

var
  i,Count,FacCnt,lastCnt: NativeUint;
begin
  count := 0;
  lastCnt := 0;
  i := 1;
  repeat
    FacCnt := getFactorCnt(i);
    if  lastCnt < FacCnt then
    Begin
      write(i,'(',FacCnt,'),');
      lastCnt:= FacCnt;
      inc(Count);
      if count = 12 then
        Writeln;
    end;
    inc(i);
  until Count >= 20;
  writeln;
end.
```
;Output:
```txt
1(1),2(2),4(3),6(4),12(6),24(8),36(9),48(10),60(12),120(16),180(18),240(20),
360(24),720(30),840(32),1260(36),1680(40),2520(48),5040(60),7560(64)
```



## Perl

{{libheader|ntheory}}

```perl
use ntheory qw(divisors);

my @anti_primes;

for (my ($k, $m) = (1, 0) ; @anti_primes < 20 ; ++$k) {
    my $sigma0 = divisors($k);

    if ($sigma0 > $m) {
        $m = $sigma0;
        push @anti_primes, $k;
    }
}

printf("%s\n", join(' ', @anti_primes));
```

{{out}}

```txt

1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## Perl 6

{{works with|Rakudo|2018.11}}
At its heart, this task is almost exactly the same as [[Proper_Divisors]], it is just asking for slightly different results. Much of this code is lifted straight from there.

Implemented as an auto-extending lazy list. Displaying the count of anti-primes less than 5e5 also because... why not.


```perl6
sub propdiv (\x) {
    my @l = 1 if x > 1;
    (2 .. x.sqrt.floor).map: -> \d {
        unless x % d { @l.push: d; my \y = x div d; @l.push: y if y != d }
    }
    @l
}

my $last = 0;

my @anti-primes = lazy 1, |(|(2..59), 60, *+60 … *).grep: -> $c {
    my \mx = +propdiv($c);
    next if mx <= $last;
    $last = mx;
    $c
}

my $upto = 5e5;

put "First 20 anti-primes:\n{ @anti-primes[^20] }";

put "\nCount of anti-primes <= $upto: {+@anti-primes[^(@anti-primes.first: * > $upto, :k)]}";
```

{{out}}

```txt
First 20 anti-primes:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

Count of anti-primes <= 500000: 35
```



## Phix


```Phix
integer n=1, maxd = -1
sequence res = {}
while length(res)<20 do
    integer lf = length(factors(n,1))
    if lf>maxd then
        res &= n
        maxd = lf
    end if
    n += iff(n>1?2:1)
end while
printf(1,"The first 20 anti-primes are: ") ?res
```

{{out}}

```txt

The first 20 anti-primes are: {1,2,4,6,12,24,36,48,60,120,180,240,360,720,840,1260,1680,2520,5040,7560}

```



## PicoLisp


```PicoLisp
(de factors (N)
   (let C 1
      (when (>= N 2)
         (inc 'C)
         (for (I 2 (>= (/ N 2) I) (inc I))
            (and (=0 (% N I)) (inc 'C)) ) )
      C ) )
(de anti (X)
   (let (M 0  I 0  N 0)
      (make
         (while (> X I)
            (inc 'N)
            (let R (factors N)
               (when (> R M)
                  (link N)
                  (setq M R)
                  (inc 'I) ) ) ) ) ) )
(println (anti 20))
```

{{out}}

```txt
(1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560)
```



## Prolog

{{trans|Erlang}}
```prolog

divcount(N, Count) :- divcount(N, 1, 0, Count).

divcount(N, D, C, C) :- D*D > N, !.
divcount(N, D, C, Count) :-
    succ(D, D2),
    divs(N, D, A), plus(A, C, C2),
    divcount(N, D2, C2, Count).

divs(N, D, 0) :- N mod D =\= 0, !.
divs(N, D, 1) :- D*D =:= N, !.
divs(_, _, 2).


antiprimes(N, L) :- antiprimes(N, 1, 0, [], L).

antiprimes(0, _, _, L, R) :- reverse(L, R), !.
antiprimes(N, M, Max, L, R) :-
    divcount(M, Count),
    succ(M, M2),
    (Count > Max
        -> succ(N0, N), antiprimes(N0, M2, Count, [M|L], R)
         ; antiprimes(N, M2, Max, L, R)).

main :-
    antiprimes(20, X),
    write("The first twenty anti-primes are "), write(X), nl,
    halt.

?- main.

```

{{out}}

```txt

The first twenty anti-primes are [1,2,4,6,12,24,36,48,60,120,180,240,360,720,840,1260,1680,2520,5040,7560]

```



## Python

Uses the fast prime function from [[Factors of an integer#Python]]

```python
from itertools import chain, count, cycle, islice, accumulate

def factors(n):
    def prime_powers(n):
        for c in accumulate(chain([2, 1, 2], cycle([2,4]))):
            if c*c > n: break
            if n%c: continue
            d,p = (), c
            while not n%c:
                n,p,d = n//c, p*c, d + (p,)
            yield(d)
        if n > 1: yield((n,))

    r = [1]
    for e in prime_powers(n):
        r += [a*b for a in r for b in e]
    return r

def antiprimes():
    mx = 0
    for c in count(1):
        ln = len(factors(c))
        if ln > mx:
            yield c
            mx = ln

if __name__ == '__main__':
    print(list(islice(antiprimes(), 20)))
```


{{out}}

```txt
[1, 2, 4, 6, 12, 24, 36, 48, 60, 120, 180, 240, 360, 720, 840, 1260, 1680, 2520, 5040, 7560]
```



## R

Uses brute force. My first entry!

```R
# Antiprimes

max_divisors <- 0

findFactors <- function(x){
  myseq <- seq(x)
  myseq[(x %% myseq) == 0]
}

antiprimes <- vector()
x <- 1
n <- 1
while(length(antiprimes) < 20){
  y <- findFactors(x)
  if (length(y) > max_divisors){
    antiprimes <- c(antiprimes, x)
    max_divisors <- length(y)
    n <- n + 1
  }
  x <- x + 1
}

antiprimes
```


{{out}}

```txt
 [1]    1    2    4    6   12   24   36   48   60  120  180  240  360  720  840 1260 1680 2520 5040 7560
```



## Racket



```racket
#lang racket

(require racket/generator
         math/number-theory)

(define (get-divisors n)
  (apply * (map (λ (factor) (add1 (second factor))) (factorize n))))

(define antiprimes
  (in-generator
   (for/fold ([prev 0]) ([i (in-naturals 1)])
     (define divisors (get-divisors i))
     (when (> divisors prev) (yield i))
     (max prev divisors))))

(for/list ([i (in-range 20)] [antiprime antiprimes]) antiprime)
```


{{out}}

```txt

'(1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560)

```



## REXX


### even and odd numbers

<!-- This REXX version does what the Rosetta Code task requires.  Note that the first DO loop is fixed at 59, but the second DO loop if open-ended.  Please read/observe the entire REXX program before flagging as incorrect.   --- Gerard Schildberger.  ~-->

This REXX version is using a modified version of a highly optimized   ''proper divisors''   function.

Programming note:   although the solution to this Rosetta Code task is trivial, a fair amount of optimization was incorporated into the REXX program to find larger anti─primes (also known as   ''highly─composite numbers'').

The   #DIVS   function could be further optimized by only processing   ''even''   numbers, with unity being treated as a special case.

```rexx
/*REXX program finds and displays  N  number of anti─primes or highly─composite numbers.*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N=20                     /*Not specified?  Then use the default.*/
maxD= 0                                          /*the maximum number of divisors so far*/
say '─index─ ──anti─prime──'                     /*display a title for the numbers shown*/
#= 0                                             /*the count of anti─primes found  "  " */
     do once=1  for 1
        do i=1  for 59                           /*step through possible numbers by twos*/
        d= #divs(i);  if d<=maxD  then iterate   /*get # divisors;  Is too small?  Skip.*/
        #= # + 1;     maxD= d                    /*found an anti─prime #;  set new minD.*/
        say center(#, 7)  right(i, 10)           /*display the index and the anti─prime.*/
        if #>=N  then leave once                 /*if we have enough anti─primes, done. */
        end   /*i*/

        do j=60  by 20                           /*step through possible numbers by 20. */
        d= #divs(j);  if d<=maxD  then iterate   /*get # divisors;  Is too small?  Skip.*/
        #= # + 1;     maxD= d                    /*found an anti─prime #;  set new minD.*/
        say center(#, 7)  right(j, 10)           /*display the index and the anti─prime.*/
        if #>=N  then leave                      /*if we have enough anti─primes, done. */
        end   /*j*/
     end      /*once*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
#divs: procedure; parse arg x 1 y                /*X and Y:  both set from 1st argument.*/
       if x<3   then return x                    /*handle special cases for one and two.*/
       if x==4  then return 3                    /*   "      "      "    " four.        */
       if x<6   then return 2                    /*   "      "      "    " three or five*/
       odd= x // 2                               /*check if   X   is  odd  or not.      */
       if odd  then      #= 1                    /*Odd?   Assume  Pdivisors  count of 1.*/
               else do;  #= 3;    y= x%2;  end   /*Even?     "        "        "    " 3.*/
                                                 /* [↑]   start with known num of Pdivs.*/
                  do k=3  for x%2-3  by 1+odd  while k<y  /*for odd numbers, skip evens.*/
                  if x//k==0  then do            /*if no remainder, then found a divisor*/
                                   #=#+2;  y=x%k /*bump  #  Pdivs,  calculate limit  Y. */
                                   if k>=y  then do;  #= #-1;  leave;  end      /*limit?*/
                                   end                                          /*  ___ */
                              else if k*k>x  then leave        /*only divide up to √ x  */
                  end   /*k*/                    /* [↑]  this form of DO loop is faster.*/
       return #+1                                /*bump "proper divisors" to "divisors".*/
```

{{out|output|text=  when using the default input of:     <tt> 20 </tt>}}

```txt

─index─ ──anti─prime──
   1             1
   2             2
   3             4
   4             6
   5            12
   6            24
   7            36
   8            48
   9            60
  10           120
  11           180
  12           240
  13           360
  14           720
  15           840
  16          1260
  17          1680
  18          2520
  19          5040
  20          7560

```


{{out|output|text=  when using the default input of:     <tt> 55 </tt>}}

```txt

─index─ ──anti─prime──
   1             1
   2             2
   3             4
   4             6
   5            12
   6            24
   7            36
   8            48
   9            60
  10           120
  11           180
  12           240
  13           360
  14           720
  15           840
  16          1260
  17          1680
  18          2520
  19          5040
  20          7560
  21         10080
  22         15120
  23         20160
  24         25200
  25         27720
  26         45360
  27         50400
  28         55440
  29         83160
  30        110880
  31        166320
  32        221760
  33        277200
  34        332640
  35        498960
  36        554400
  37        665280
  38        720720
  39       1081080
  40       1441440
  41       2162160
  42       2882880
  43       3603600
  44       4324320
  45       6486480
  46       7207200
  47       8648640
  48      10810800
  49      14414400
  50      17297280
  51      21621600
  52      32432400
  53      36756720
  54      43243200
  55      61261200

```



### only even numbers

This REXX version only processes   ''even''   numbers   (unity is treated as a special case.)

It's about   '''10%'''   faster than the 1<sup>st</sup> REXX version.

```rexx
/*REXX program finds and displays  N  number of anti─primes or highly─composite numbers.*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N=20                     /*Not specified?  Then use the default.*/
@.=.;   @.1= 1;   @.2= 2;   @.4= 3;   @.5= 2;   @.6= 4
say '─index─ ──anti─prime──'                     /*display a title for the numbers shown*/
#= 1                                             /*the count of anti─primes found  "  " */
maxD= 1                                          /*the maximum number of divisors so far*/
say center(#, 7)  right(1, 10)                   /*display the index and the anti─prime.*/
     do once=1  for 1
        do i=2  by  2  to 59                     /*step through possible numbers by twos*/
        d= #divs(i);  if d<=maxD  then iterate   /*get # divisors;  Is too small?  Skip.*/
        #= # + 1;     maxD= d                    /*found an anti─prime #;  set new minD.*/
        say center(#, 7)  right(i, 10)           /*display the index and the anti─prime.*/
        if #>=N  then leave once                 /*if we have enough anti─primes, done. */
        end   /*i*/

        do j=60  by 20                           /*step through possible numbers by 20. */
        d= #divs(j);  if d<=maxD  then iterate   /*get # divisors;  Is too small?  Skip.*/
        #= # + 1;     maxD= d                    /*found an anti─prime #;  set new minD.*/
        say center(#, 7)  right(j, 10)           /*display the index and the anti─prime.*/
        if #>=N  then leave once                 /*if we have enough anti─primes, done. */
        end   /*j*/
     end      /*once*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
#divs: parse arg x;  if @.x\==.  then return @.x /*if pre─computed, then return shortcut*/
       $= 3;    y= x%2
                                                 /* [↑]   start with known num of Pdivs.*/
                  do k=3  for x%2-3  while k<y
                  if x//k==0  then do            /*if no remainder, then found a divisor*/
                                   $=$+2;  y=x%k /*bump  $  Pdivs,  calculate limit  Y. */
                                   if k>=y  then do;  $= $-1;  leave;  end      /*limit?*/
                                   end                                          /*  ___ */
                              else if k*k>x  then leave        /*only divide up to √ x  */
                  end   /*k*/                    /* [↑]  this form of DO loop is faster.*/
       return $+1                                /*bump "proper divisors" to "divisors".*/
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




## Ring


```ring

# Project : ANti-primes

see "working..." + nl
see "wait for done..." + nl + nl
see "the first 20 anti-primes are:" + nl + nl
maxDivisor = 0
num = 0
n = 0
result = list(20)
while num < 20
      n = n + 1
      div = factors(n)
      if (div > maxDivisor)
         maxDivisor = div
         num = num + 1
         result[num] = n
      ok
end
see "["
for n = 1 to len(result)
    if n < len(result)
       see string(result[n]) + ","
    else
       see string(result[n]) + "]" + nl + nl
    ok
next
see "done..." + nl

func factors(an)
     ansum = 2
     if an < 2
        return(1)
     ok
     for nr = 2 to an/2
         if an%nr = 0
            ansum = ansum+1
         ok
     next
     return ansum

```

{{out}}

```txt

working...
wait for done...

the first 20 anti-primes are:

[1,2,4,6,12,24,36,48,60,120,180,240,360,720,840,1260,1680,2520,5040,7560]

done...

```



## Ruby


```ruby
require 'prime'

def num_divisors(n)
  n.prime_division.inject(1){|prod, (_p,n)| prod *= (n + 1) }
end

anti_primes = Enumerator.new do |y| # y is the yielder
  max = 0
  y << 1                            # yield 1
  2.step(nil,2) do |candidate|      # nil is taken as Infinity
     num = num_divisors(candidate)
     if  num > max
       y << candidate               # yield the candidate
       max = num
     end
  end
end

puts anti_primes.take(20).join(" ")

```

{{out}}

```txt

1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## Rust

{{trans|Go}}

```Rust
fn count_divisors(n: u64) -> usize {
    if n < 2 {
        return 1;
    }
    2 + (2..=(n / 2)).filter(|i| n % i == 0).count()
}

fn main() {
    println!("The first 20 anti-primes are:");
    (1..)
        .scan(0, |max, n| {
            let d = count_divisors(n);
            Some(if d > *max {
                *max = d;
                Some(n)
            } else {
                None
            })
        })
        .flatten()
        .take(20)
        .for_each(|n| print!("{} ", n));
    println!();
}
```

{{out}}

```txt
The first 20 anti-primes are:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560
```



## Scala

This program uses an iterator to count the factors of a number, then builds a lazily evaluated list of all anti-primes. Finding the first 20 anti-primes involves merely taking the first 20 elements of the list.

```scala
def factorCount(num: Int): Int = Iterator.range(1, num/2 + 1).count(num%_ == 0) + 1
def antiPrimes: LazyList[Int] = LazyList.iterate((1: Int, 1: Int)){case (n, facs) => Iterator.from(n + 1).map(i => (i, factorCount(i))).dropWhile(_._2 <= facs).next}.map(_._1)
```

{{out}}

```txt
scala> print(antiPrimes.take(20).mkString(", "))
1, 2, 4, 6, 12, 24, 36, 48, 60, 120, 180, 240, 360, 720, 840, 1260, 1680, 2520, 5040, 7560
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: countDivisors (in integer: number) is func
  result
    var integer: count is 1;
  local
    var integer: num is 0;
  begin
    for num range 1 to number div 2 do
      if number rem num = 0 then
        incr(count);
      end if;
    end for;
  end func;

const proc: main is func
  local
    var integer: maxDiv is 0;
    var integer: count is 0;
    var integer: number is 1;
    var integer: divisors is 1;
  begin
    writeln("The first 20 anti-primes are:");
    while count < 20 do
      divisors := countDivisors(number);
      if divisors > maxDiv then
        write(number <& " ");
        maxDiv := divisors;
        incr(count);
      end if;
      incr(number);
    end while;
    writeln;
  end func;
```


{{out}}

```txt
The first 20 anti-primes are:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```



## Sidef

Using the built-in ''Number.sigma0'' method to count the number of divisors.

```ruby
say with (0) {|max|
    1..Inf -> lazy.grep { (.sigma0 > max) && (max = .sigma0) }.first(20)
}
```

{{out}}

```txt

[1, 2, 4, 6, 12, 24, 36, 48, 60, 120, 180, 240, 360, 720, 840, 1260, 1680, 2520, 5040, 7560]

```



## VBA

{{trans|Phix}}

```vb
Private Function factors(n As Integer) As Collection
    Dim f As New Collection
    For i = 1 To Sqr(n)
        If n Mod i = 0 Then
            f.Add i
            If n / i <> i Then f.Add n / i
        End If
    Next i
    f.Add n
    Set factors = f
End Function
Public Sub anti_primes()
    Dim n As Integer, maxd As Integer
    Dim res As New Collection, lenght As Integer
    Dim lf As Integer
    n = 1: maxd = -1
    Length = 0
    Do While res.count < 20
        lf = factors(n).count
        If lf > maxd Then
            res.Add n
            maxd = lf
        End If
        n = n + IIf(n > 1, 2, 1)
    Loop
    Debug.Print "The first 20 anti-primes are:";
    For Each x In res
        Debug.Print x;
    Next x
End Sub
```
{{out}}

```txt
The first 20 anti-primes are: 1  2  4  6  12  24  36  48  60  120  180  240  360  720  840  1260  1680  2520  5040  7560

```


## Tcl

{{trans|Java}}


```tcl

proc countDivisors {n} {
  if {$n < 2} {return 1}
  set count 2
  set n2 [expr $n / 2]
  for {set i 2} {$i <= $n2} {incr i} {
    if {[expr $n % $i] == 0} {incr count}
  }
  return $count
}

# main
set maxDiv 0
set count 0

puts "The first 20 anti-primes are:"
for {set n 1} {$count < 20} {incr n} {
  set d [countDivisors $n]
  if {$d > $maxDiv} {
    puts $n
    set maxDiv $d
    incr count
  }
}

```

{{out}}
```txt

 ./anti_primes.tcl
The first 20 anti-primes are:
1
2
4
6
12
24
36
48
60
120
180
240
360
720
840
1260
1680
2520
5040
7560

```



## Visual Basic .NET

{{trans|D}}

```vbnet
Module Module1

    Function CountDivisors(n As Integer) As Integer
        If n < 2 Then
            Return 1
        End If
        Dim count = 2 '1 and n
        For i = 2 To n \ 2
            If n Mod i = 0 Then
                count += 1
            End If
        Next
        Return count
    End Function

    Sub Main()
        Dim maxDiv, count As Integer
        Console.WriteLine("The first 20 anti-primes are:")

        Dim n = 1
        While count < 20
            Dim d = CountDivisors(n)

            If d > maxDiv Then
                Console.Write("{0} ", n)
                maxDiv = d
                count += 1
            End If
            n += 1
        End While

        Console.WriteLine()
    End Sub

End Module
```

{{out}}

```txt
The first 20 anti-primes are:
1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560
```



## Yabasic

{{Output?|Yabasic}}
{{trans|AWK}}

```Yabasic
print "The first 20 anti-primes are:"

while (count < 20)
    n = n + 1
    d = count_divisors(n)
    if d > max_divisors then
        print n;
        max_divisors = d
        count = count + 1
    end if
wend
print

sub count_divisors(n)
    local count, i

    if n < 2 return 1

    count = 2
    for i = 2 to n/2
      if not(mod(n,  i)) count = count + 1
    next
    return count
end sub
```


{{trans|Lua}}

```Yabasic
// First 20 antiprimes.

sub count_factors(number)
    local count, attempt

    for attempt = 1 to number
        if not mod(number, attempt) count = count + 1
    next
    return count
end sub

sub antiprimes$(goal)
    local factors, list$, number, mostFactors, nitems

    number = 1

    while nitems < goal
        factors = count_factors(number)
        if factors > mostFactors then
            list$ = list$ + ", " + str$(number)
            nitems = nitems + 1
            mostFactors = factors
        endif
        number = number + 1
    wend
    return list$
end sub

print "The first 20 antiprimes:"
print mid$(antiprimes$(20), 3)
print "Done."
```



## zkl

{{trans|Perl6}}

```zkl
fcn properDivsN(n) //--> count of proper divisors. 1-->1, wrong but OK here
   { [1.. (n + 1)/2 + 1].reduce('wrap(p,i){ p + (n%i==0 and n!=i) }) }
fcn antiPrimes{		// -->iterator
   Walker.chain([2..59],[60..*,30]).tweak(fcn(c,rlast){
      last,mx := rlast.value, properDivsN(c);
      if(mx<=last) return(Void.Skip);
      rlast.set(mx);
      c
   }.fp1(Ref(0))).push(1);	// 1 has no proper divisors
}
```


```zkl
println("First 20 anti-primes:\n  ",antiPrimes().walk(20).concat(" "));
```

{{out}}

```txt

First 20 anti-primes:
  1 2 4 6 12 24 36 48 60 120 180 240 360 720 840 1260 1680 2520 5040 7560

```

