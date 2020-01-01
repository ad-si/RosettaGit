+++
title = "Successive prime differences"
description = ""
date = 2019-07-04T06:18:38Z
aliases = []
[extra]
id = 22295
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}
The series of increasing prime numbers begins: <code>2, 3, 5, 7, 11, 13, 17, 19, 23, 29, ...</code>

The task applies a filter to the series returning groups of ''successive'' primes, (s'primes),  that differ from the next by a given value or values.

'''Example 1:''' Specifying that the difference between s'primes be <code>2</code> leads to the groups:
:<code>(3, 5), (5, 7), (11, 13), (17, 19), (29, 31), ...</code>

(Known as [[wp:Twin prime|Twin primes]] or [https://oeis.org/A077800 Prime pairs])

'''Example 2:''' Specifying more than one difference ''between'' s'primes leads to groups of size one greater than the number of differences. Differences of <code>2, 4</code> leads to the groups:
:<code>(5, 7, 11), (11, 13, 17), (17, 19, 23), (41, 43, 47), ...</code>.

In the first group 7 is two more than 5 and 11 is four more than 7; as well as 5, 7, and 11 being ''successive'' primes.
Differences are checked in the order of the values given, (differences of <code>4, 2</code> would give different groups entirely).

;Task:
* In each case use a list of primes less than 1_000_000
* For the following Differences show the first and last group, as well as the number of groups found:
:# Differences of <code>2</code>.
:# Differences of <code>1</code>.
:# Differences of <code>2, 2</code>.
:# Differences of <code>2, 4</code>.
:# Differences of <code>4, 2</code>.
:# Differences of <code>6, 4, 2</code>.
* Show output here.


Note: Generation of a list of primes is a secondary aspect of the task. Use of a built in function, well known library, or importing/use of prime generators from other [[Sieve of Eratosthenes|Rosetta Code tasks]] is encouraged.

;references
:#https://pdfs.semanticscholar.org/78a1/7349819304863ae061df88dbcb26b4908f03.pdf
:#https://www.primepuzzles.net/puzzles/puzz_011.htm
:#https://matheplanet.de/matheplanet/nuke/html/viewtopic.php?topic=232720&start=0

## C#


```c#
using System;
using System.Collections.Generic;
using static System.Linq.Enumerable;

public static class SuccessivePrimeDifferences
{
    public static void Main() {
        var primes = GeneratePrimes(1_000_000).ToList();
        foreach (var d in new[] {
            new [] { 2 },
            new [] { 1 },
            new [] { 2, 2 },
            new [] { 2, 4 },
            new [] { 4, 2 },
            new [] { 6, 4, 2 },
        }) {
            IEnumerable<int> first = null, last = null;
            int count = 0;
            foreach (var grp in FindDifferenceGroups(d)) {
                if (first == null) first = grp;
                last = grp;
                count++;
            }
            Console.WriteLine($"{$"({string.Join(", ", first)})"}, {$"({string.Join(", ", last)})"}, {count}");
        }

        IEnumerable<IEnumerable<int>> FindDifferenceGroups(int[] diffs) {
            for (int pi = diffs.Length; pi < primes.Count; pi++) {
                if (Range(0, diffs.Length).All(di => primes[pi-diffs.Length+di+1] - primes[pi-diffs.Length+di] == diffs[di])) {
                    yield return Range(pi - diffs.Length, diffs.Length + 1).Select(i => primes[i]);
                }
            }
        }
    }

}
```

{{out}}

```txt

(3, 5), (999959, 999961), 8169
(2, 3), (2, 3), 1
(3, 5, 7), (3, 5, 7), 1
(5, 7, 11), (999431, 999433, 999437), 1393
(7, 11, 13), (997807, 997811, 997813), 1444
(31, 37, 41, 43), (997141, 997147, 997151, 997153), 306

```


=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Successive primes. Nigel Galloway: May 6th., 2019
let sP n=let sP=pCache|>Seq.takeWhile(fun n->n<1000000)|>Seq.windowed(Array.length n+1)|>Seq.filter(fun g->g=(Array.scan(fun n g->n+g) g.[0] n))
         printfn "sP %A\t-> Min element = %A Max element = %A of %d elements" n (Seq.head sP) (Seq.last sP) (Seq.length sP)
List.iter sP [[|2|];[|1|];[|2;2|];[|2;4|];[|4;2|];[|6;4;2|]]

```

{{out}}

```txt

sP [|2|]        -> Min element = [|3; 5|] Max element = [|999959; 999961|] of 8169 elements
sP [|1|]        -> Min element = [|2; 3|] Max element = [|2; 3|] of 1 elements
sP [|2; 2|]     -> Min element = [|3; 5; 7|] Max element = [|3; 5; 7|] of 1 elements
sP [|2; 4|]     -> Min element = [|5; 7; 11|] Max element = [|999431; 999433; 999437|] of 1393 elements
sP [|4; 2|]     -> Min element = [|7; 11; 13|] Max element = [|997807; 997811; 997813|] of 1444 elements
sP [|6; 4; 2|]  -> Min element = [|31; 37; 41; 43|] Max element = [|997141; 997147; 997151; 997153|] of 306 elements

```


## Factor

{{works with|Factor|0.99}}

```factor
USING: formatting fry grouping kernel math math.primes
math.statistics sequences ;
IN: rosetta-code.successive-prime-differences

: seq-diff ( seq diffs -- seq' quot )
    dup [ length 1 + <clumps> ] dip '[ differences _ sequence= ]
    ; inline

: show ( seq diffs -- )
    [ "...for differences %u:\n" printf ] keep seq-diff
    [ find nip { } like ]
    [ find-last nip { } like ]
    [ count ] 2tri
    "First group: %u\nLast group: %u\nCount: %d\n\n" printf ;

: successive-prime-differences ( -- )
    "Groups of successive primes up to one million...\n" printf
    1,000,000 primes-upto {
        { 2 }
        { 1 }
        { 2 2 }
        { 2 4 }
        { 4 2 }
        { 6 4 2 }
    } [ show ] with each ;

MAIN: successive-prime-differences
```

{{out}}

```txt

Groups of successive primes up to one million...
...for differences { 2 }:
First group: { 3 5 }
Last group: { 999959 999961 }
Count: 8169

...for differences { 1 }:
First group: { 2 3 }
Last group: { 2 3 }
Count: 1

...for differences { 2 2 }:
First group: { 3 5 7 }
Last group: { 3 5 7 }
Count: 1

...for differences { 2 4 }:
First group: { 5 7 11 }
Last group: { 999431 999433 999437 }
Count: 1393

...for differences { 4 2 }:
First group: { 7 11 13 }
Last group: { 997807 997811 997813 }
Count: 1444

...for differences { 6 4 2 }:
First group: { 31 37 41 43 }
Last group: { 997141 997147 997151 997153 }
Count: 306

```



## Go


```go
package main

import "fmt"

func sieve(limit int) []int {
    primes := []int{2}
    c := make([]bool, limit+1) // composite = true
    // no need to process even numbers > 2
    p := 3
    for {
        p2 := p * p
        if p2 > limit {
            break
        }
        for i := p2; i <= limit; i += 2 * p {
            c[i] = true
        }
        for {
            p += 2
            if !c[p] {
                break
            }
        }
    }
    for i := 3; i <= limit; i += 2 {
        if !c[i] {
            primes = append(primes, i)
        }
    }
    return primes
}

func successivePrimes(primes, diffs []int) [][]int {
    var results [][]int
    dl := len(diffs)
outer:
    for i := 0; i < len(primes)-dl; i++ {
        group := make([]int, dl+1)
        group[0] = primes[i]
        for j := i; j < i+dl; j++ {
            if primes[j+1]-primes[j] != diffs[j-i] {
                group = nil
                continue outer
            }
            group[j-i+1] = primes[j+1]
        }
        results = append(results, group)
        group = nil
    }
    return results
}

func main() {
    primes := sieve(999999)
    diffsList := [][]int{{2}, {1}, {2, 2}, {2, 4}, {4, 2}, {6, 4, 2}}
    fmt.Println("For primes less than 1,000,000:-\n")
    for _, diffs := range diffsList {
        fmt.Println("  For differences of", diffs, "->")
        sp := successivePrimes(primes, diffs)
        if len(sp) == 0 {
            fmt.Println("    No groups found")
            continue
        }
        fmt.Println("    First group   = ", sp[0])
        fmt.Println("    Last group    = ", sp[len(sp)-1])
        fmt.Println("    Number found  = ", len(sp))
        fmt.Println()
    }
}
```


{{out}}

```txt

For primes less than 1,000,000:-

  For differences of [2] ->
    First group   =  [3 5]
    Last group    =  [999959 999961]
    Number found  =  8169

  For differences of [1] ->
    First group   =  [2 3]
    Last group    =  [2 3]
    Number found  =  1

  For differences of [2 2] ->
    First group   =  [3 5 7]
    Last group    =  [3 5 7]
    Number found  =  1

  For differences of [2 4] ->
    First group   =  [5 7 11]
    Last group    =  [999431 999433 999437]
    Number found  =  1393

  For differences of [4 2] ->
    First group   =  [7 11 13]
    Last group    =  [997807 997811 997813]
    Number found  =  1444

  For differences of [6 4 2] ->
    First group   =  [31 37 41 43]
    Last group    =  [997141 997147 997151 997153]
    Number found  =  306

```



## J


```j

   primes_less_than=: i.&.:(p:inv)
   assert 2 3 5 -: primes_less_than 7
   PRIMES=: primes_less_than 1e6

   NB. Insert minus `-/' into the length two infixes `\'.
   NB. Passive `~' swaps the arguments producing the positive differences.
   SUCCESSIVE_DIFFERENCES=: 2 -~/\ PRIMES
   assert 8169 -: +/ 2 = SUCCESSIVE_DIFFERENCES   NB. twin prime tally

   INTERVALS=: 2 ; 1 ; 2 2 ; 2 4 ; 4 2 ; 6 4 2

   sequence_index=: [: I. E.
   end_groups=: PRIMES {~  ({. , {:)@:] +/ i.@:>:@:#@:[

   HEAD=: <;._2 'group;tally;end occurrences;'

   HEAD , INTERVALS (([ ([ ; #@] ; end_groups) sequence_index)~ >)"1 0~ SUCCESSIVE_DIFFERENCES
┌─────┬─────┬───────────────────────────┐
│group│tally│end occurrences            │
├─────┼─────┼───────────────────────────┤
│2    │8169 │     3      5              │
│     │     │999959 999961              │
├─────┼─────┼───────────────────────────┤
│1    │1    │2 3                        │
│     │     │2 3                        │
├─────┼─────┼───────────────────────────┤
│2 2  │1    │3 5 7                      │
│     │     │3 5 7                      │
├─────┼─────┼───────────────────────────┤
│2 4  │1393 │     5      7     11       │
│     │     │999431 999433 999437       │
├─────┼─────┼───────────────────────────┤
│4 2  │1444 │     7     11     13       │
│     │     │997807 997811 997813       │
├─────┼─────┼───────────────────────────┤
│6 4 2│306  │    31     37     41     43│
│     │     │997141 997147 997151 997153│
└─────┴─────┴───────────────────────────┘
```



## Julia


```julia
using Primes

function filterdifferences(deltas, N)
    allprimes = primes(N)
    differences = map(i -> allprimes[i + 1] - allprimes[i], 1:length(allprimes) - 1)
    println("Diff Sequence   Count         First           Last")
    for delt in deltas
        ret = trues(length(allprimes) - length(delt))
        for j in 1:length(ret)
            for (i, d) in enumerate(delt)
                if differences[j - 1 + i] != d
                    ret[j] = false
                    break
                end
            end
        end
        count, p1, pn, n = sum(ret), findfirst(ret), findlast(ret), length(delt)
        println(rpad(string(delt), 16), lpad(count, 4),
            lpad(string(allprimes[p1:p1 + n]), 18), "...", rpad(allprimes[pn:pn+n], 15))
    end
end

filterdifferences([[2], [1], [2, 2], [2, 4], [4, 2], [6, 4, 2]], 1000000)

```
{{out}}

```txt

Diff Sequence   Count         First           Last
[2]             8169            [3, 5]...[999959, 999961]
[1]                1            [2, 3]...[2, 3]
[2, 2]             1         [3, 5, 7]...[3, 5, 7]
[2, 4]          1393        [5, 7, 11]...[999431, 999433, 999437]
[4, 2]          1444       [7, 11, 13]...[997807, 997811, 997813]
[6, 4, 2]        306  [31, 37, 41, 43]...[997141, 997147, 997151, 997153]

```



## Perl

{{libheader|ntheory}}

```perl
use 5.010;
use strict;
use warnings;
no warnings 'experimental::smartmatch';

use List::EachCons;
use ntheory 'primes';

my $limit = 1E6;
my @primes = (2, @{ primes($limit) });
my @intervals = map { $primes[$_] - $primes[$_-1] } 1..$#primes;

say "Groups of successive primes <= $limit";

for my $diffs ([2], [1], [2,2], [2,4], [4,2], [6,4,2]) {
    my $n = -1;
    my @offsets = grep {$_} each_cons @$diffs, @intervals, sub { $n++; $n if @_ ~~ @$diffs };
    printf "%10s has %5d sets: %15s … %s\n",
       '(' . join(' ',@$diffs) . ')',
        scalar @offsets,
        join(' ', @primes[$offsets[ 0]..($offsets[ 0]+@$diffs)]),
        join(' ', @primes[$offsets[-1]..($offsets[-1]+@$diffs)]);
}
```

{{out}}

```txt
       (2) has  8169 sets:             3 5 … 999959 999961
       (1) has     1 sets:             2 3 … 2 3
     (2 2) has     1 sets:           3 5 7 … 3 5 7
     (2 4) has  1393 sets:          5 7 11 … 999431 999433 999437
     (4 2) has  1444 sets:         7 11 13 … 997807 997811 997813
   (6 4 2) has   306 sets:     31 37 41 43 … 997141 997147 997151 997153
```



## Perl 6


### Categorized by Successive

{{works with|Rakudo|2019.03}}
Essentially the code from the [[Sexy_primes#Perl_6|Sexy primes]] task with minor tweaks.


```perl6
use Math::Primesieve;
my $sieve = Math::Primesieve.new;

my $max = 1_000_000;
my @primes = $sieve.primes($max);
my $filter = @primes.Set;
my $primes = @primes.categorize: &successive;

sub successive ($i) {
    gather {
        take '2' if $filter{$i + 2};
        take '1' if $filter{$i + 1};
        take '2_2' if all($filter{$i «+« (2,4)});
        take '2_4' if all($filter{$i «+« (2,6)});
        take '4_2' if all($filter{$i «+« (4,6)});
        take '6_4_2' if all($filter{$i «+« (6,10,12)}) and
            none($filter{$i «+« (2,4,8)});
    }
}

sub comma { $^i.flip.comb(3).join(',').flip }

for (2,), (1,), (2,2), (2,4), (4,2), (6,4,2) -> $succ {
    say "## Sets of {1+$succ} successive primes <= { comma $max } with " ~
        "successive differences of { $succ.join: ', ' }";
    my $i = $succ.join: '_';
    for 'First', 0, ' Last', * - 1 -> $where, $ind {
        say "$where group: ", join ', ', [\+] flat $primes{$i}[$ind], |$succ
    }
    say '      Count: ', +$primes{$i}, "\n";
}
```

{{out}}

```txt
## Sets of 2 successive primes <= 1,000,000 with successive differences of 2
First group: 3, 5
 Last group: 999959, 999961
      Count: 8169

## Sets of 2 successive primes <= 1,000,000 with successive differences of 1
First group: 2, 3
 Last group: 2, 3
      Count: 1

## Sets of 3 successive primes <= 1,000,000 with successive differences of 2, 2
First group: 3, 5, 7
 Last group: 3, 5, 7
      Count: 1

## Sets of 3 successive primes <= 1,000,000 with successive differences of 2, 4
First group: 5, 7, 11
 Last group: 999431, 999433, 999437
      Count: 1393

## Sets of 3 successive primes <= 1,000,000 with successive differences of 4, 2
First group: 7, 11, 13
 Last group: 997807, 997811, 997813
      Count: 1444

## Sets of 4 successive primes <= 1,000,000 with successive differences of 6, 4, 2
First group: 31, 37, 41, 43
 Last group: 997141, 997147, 997151, 997153
      Count: 306

```


### Precomputed Differences

{{works with|Rakudo|2019.03}}

```perl6
use Math::Primesieve;

constant $max    = 1_000_000;
constant @primes = Math::Primesieve.primes($max);
constant @diffs  = @primes.skip Z- @primes;

say "Given all ordered primes <= $max, sets with successive differences of:";
for (2,), (1,), (2,2), (2,4), (4,2), (6,4,2) -> @succ {
    my $size = @succ.elems;

    my @group_start_offsets = @diffs.rotor( $size => 1-$size )
                                    .grep(:k, { $_ eqv @succ });

    my ($first, $last) = @group_start_offsets[0, *-1]
                         .map: { @primes.skip($_).head($size + 1) };

    say sprintf '%10s has %5d sets: %15s … %s',
        @succ.gist, @group_start_offsets.elems, $first, $last;
}
```

{{Out}}

```txt
Given all ordered primes <= 1000000, sets with successive differences of:
       (2) has  8169 sets:             3 5 … 999959 999961
       (1) has     1 sets:             2 3 … 2 3
     (2 2) has     1 sets:           3 5 7 … 3 5 7
     (2 4) has  1393 sets:          5 7 11 … 999431 999433 999437
     (4 2) has  1444 sets:         7 11 13 … 997807 997811 997813
   (6 4 2) has   306 sets:     31 37 41 43 … 997141 997147 997151 997153
```



## Phix

Uses primes[] and add_block() from [[Extensible_prime_generator#Phix|Extensible_prime_generator]]

```Phix
procedure test(sequence differences)
    sequence res = {}
    integer ld = length(differences)
    for i=1 to length(primes)-ld do
        integer pi = primes[i]
        for j=1 to ld do
            pi += differences[j]
            if pi!=primes[i+j] then
                pi = 0
                exit
            end if
        end for
        if pi!=0 then
            res = append(res,primes[i..i+ld])
        end if
    end for
    res = {differences,length(res),res[1],res[$]}
    printf(1,"%8v : %8d %14v...%v\n",res)
end procedure

while primes[$]<1_000_000 do add_block() end while
primes = primes[1..abs(binary_search(1_000_000,primes))-1]
constant differences = {{2},{1},{2,2},{2,4},{4,2},{6,4,2}}
printf(1,"Differences   Count          First   Last\n")
for i=1 to length(differences) do test(differences[i]) end for
```

{{out}}

```txt

Differences   Count          First   Last
     {2} :     8169          {3,5}...{999959,999961}
     {1} :        1          {2,3}...{2,3}
   {2,2} :        1        {3,5,7}...{3,5,7}
   {2,4} :     1393       {5,7,11}...{999431,999433,999437}
   {4,2} :     1444      {7,11,13}...{997807,997811,997813}
 {6,4,2} :      306  {31,37,41,43}...{997141,997147,997151,997153}

```



## Python

Uses the [https://www.sympy.org/en/index.html Sympy] library.


```python
# https://docs.sympy.org/latest/index.html
from sympy import Sieve

def nsuccprimes(count, mx):
    "return tuple of <count> successive primes <= mx (generator)"
    sieve = Sieve()
    sieve.extend(mx)
    primes = sieve._list
    return zip(*(primes[n:] for n in range(count)))

def check_value_diffs(diffs, values):
    "Differences between successive values given by successive items in diffs?"
    return all(v[1] - v[0] == d
               for d, v in zip(diffs, zip(values, values[1:])))

def successive_primes(offsets=(2, ), primes_max=1_000_000):
    return (sp for sp in nsuccprimes(len(offsets) + 1, primes_max)
            if check_value_diffs(offsets, sp))

if __name__ == '__main__':
    for offsets, mx in [((2,),      1_000_000),
                        ((1,),      1_000_000),
                        ((2, 2),    1_000_000),
                        ((2, 4),    1_000_000),
                        ((4, 2),    1_000_000),
                        ((6, 4, 2), 1_000_000),
                       ]:
        print(f"## SETS OF {len(offsets)+1} SUCCESSIVE PRIMES <={mx:_} WITH "
              f"SUCCESSIVE DIFFERENCES OF {str(list(offsets))[1:-1]}")
        for count, last in enumerate(successive_primes(offsets, mx), 1):
            if count == 1:
                first = last
        print("  First group:", str(first)[1:-1])
        print("   Last group:", str(last)[1:-1])
        print("        Count:", count)
```


{{out}}

```txt
## SETS OF 2 SUCCESSIVE PRIMES <=1_000_000 WITH SUCCESSIVE DIFFERENCES OF 2
  First group: 3, 5
   Last group: 999959, 999961
        Count: 8169
## SETS OF 2 SUCCESSIVE PRIMES <=1_000_000 WITH SUCCESSIVE DIFFERENCES OF 1
  First group: 2, 3
   Last group: 2, 3
        Count: 1
## SETS OF 3 SUCCESSIVE PRIMES <=1_000_000 WITH SUCCESSIVE DIFFERENCES OF 2, 2
  First group: 3, 5, 7
   Last group: 3, 5, 7
        Count: 1
## SETS OF 3 SUCCESSIVE PRIMES <=1_000_000 WITH SUCCESSIVE DIFFERENCES OF 2, 4
  First group: 5, 7, 11
   Last group: 999431, 999433, 999437
        Count: 1393
## SETS OF 3 SUCCESSIVE PRIMES <=1_000_000 WITH SUCCESSIVE DIFFERENCES OF 4, 2
  First group: 7, 11, 13
   Last group: 997807, 997811, 997813
        Count: 1444
## SETS OF 4 SUCCESSIVE PRIMES <=1_000_000 WITH SUCCESSIVE DIFFERENCES OF 6, 4, 2
  First group: 31, 37, 41, 43
   Last group: 997141, 997147, 997151, 997153
        Count: 306
```



## REXX


```rexx
/*REXX program finds and displays  primes  with successive differences  (up to a limit).*/
parse arg H . 1 . difs                           /*allow the highest number be specified*/
if H=='' | H==","  then  H= 1000000              /*Not specified?  Then use the default.*/
if difs=''   then  difs= 2 1 2.2 2.4 4.2 6.4.2   /* "      "         "   "   "     "    */
call genP H

        do j=1  for words(difs)                             /*traipse through the lists.*/
        dif= translate( word(difs, j),,.);  dw= words(dif)  /*obtain true differences.  */
            do i=1  for dw;  dif.i= word(dif, i)            /*build an array of diffs.  */
            end   /*i*/                                     /* [↑]  for optimization.   */
        say center('primes with differences of:'  dif,  50, '─')        /*display title.*/
        p= 1;                        c= 0;        grp=      /*init. prime#,  count, grp.*/
             do a=1;  p= nextP(p+1);  if p==0  then leave   /*find the next  DIF  primes*/
             aa= p;   !.=                                   /*AA: nextP;  the group #'s.*/
             !.1= p                                         /*assign 1st prime in group.*/
                    do g=2  for dw                          /*get the rest of the group.*/
                    aa= nextP(aa+1); if aa==0  then leave a /*obtain the next prime.    */
                    !.g= aa;         _= g-1                 /*prepare to add difference.*/
                    if !._ + dif._\==!.g  then iterate a    /*determine if fits criteria*/
                    end   /*g*/
             c= c+1                                         /*bump count of # of groups.*/
             grp= !.1;       do b=2  for dw;  grp= grp !.b  /*build a list of primes.   */
                             end   /*b*/
             if c==1  then say '     first group: '   grp   /*display the first group.  */
             end   /*a*/
                                                            /* [↓]  test if group found.*/
        if grp==''   then say "         (none)"             /*display the  last group.  */
                     else say '      last group: '   grp    /*   "     "     "    "     */
                          say '           count: '   c      /*   "     "  group count.  */
        say
        end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
nextP:    do nxt=arg(1)  to H;  if @.nxt==.  then return nxt;  end /*nxt*/;     return 0
/*──────────────────────────────────────────────────────────────────────────────────────*/
genP: procedure expose @.; parse arg N;  != 0;  @.=.;  @.1=      /*initialize the array.*/
          do e=4  by 2  for (N-1)%2;  @.e=;  end /*treat the even integers > 2  special.*/
                                                 /*all primes are indicated with a  "." */
        do j=1  by 2  for (N-1)%2;  k= j         /*use odd integers up to  N  inclusive.*/
        if @.j==.  then do;  if !  then iterate  /*Prime?   Should skip the top part ?  */
                             jj= j * j           /*compute the square of  J.        ___ */
                             if jj>N  then != 1  /*indicate skip top part  if  j > √ N  */
                               do m=jj  to N  by j+j;  @.m=;  end       /*odd multiples.*/
                        end                      /* [↑]  strike odd multiples  ¬ prime. */
```

{{out|output|text=  when using the default inputs:}}

```txt

──────────primes with differences of: 2───────────
     first group:  3 5
      last group:  999959 999961
           count:  8169

──────────primes with differences of: 1───────────
     first group:  2 3
      last group:  2 3
           count:  1

─────────primes with differences of: 2 2──────────
     first group:  3 5 7
      last group:  3 5 7
           count:  1

─────────primes with differences of: 2 4──────────
     first group:  5 7 11
      last group:  999431 999433 999437
           count:  1393

─────────primes with differences of: 4 2──────────
     first group:  7 11 13
      last group:  997807 997811 997813
           count:  1444

────────primes with differences of: 6 4 2─────────
     first group:  31 37 41 43
      last group:  997141 997147 997151 997153
           count:  306

```


## Ruby


```ruby
require 'prime'
PRIMES = Prime.each(1_000_000).to_a
difs = [[2], [1], [2,2], [2,4], [4,2], [6,4,2]]

difs.each do |ar|
  res = PRIMES.each_cons(ar.size+1).select do |slice|
    slice.each_cons(2).zip(ar).all? {|(a,b), c| a+c == b}
  end
  puts "#{ar} has #{res.size} sets. #{res.first}...#{res.last}"
end

```

{{output}}

```txt
[2] has 8169 sets. [3, 5]...[999959, 999961]
[1] has 1 sets. [2, 3]...[2, 3]
[2, 2] has 1 sets. [3, 5, 7]...[3, 5, 7]
[2, 4] has 1393 sets. [5, 7, 11]...[999431, 999433, 999437]
[4, 2] has 1444 sets. [7, 11, 13]...[997807, 997811, 997813]
[6, 4, 2] has 306 sets. [31, 37, 41, 43]...[997141, 997147, 997151, 997153]

```


## Scala


```scala
object SuccessivePrimeDiffs {
  def main(args: Array[String]): Unit = {
    val d2 = primesByDiffs(2)(1000000)
    val d1 = primesByDiffs(1)(1000000)
    val d22 = primesByDiffs(2, 2)(1000000)
    val d24 = primesByDiffs(2, 4)(1000000)
    val d42 = primesByDiffs(4, 2)(1000000)
    val d642 = primesByDiffs(6, 4, 2)(1000000)

    if(true) println(
      s"""|Diffs: (First), (Last), Count
          |2:     (${d2.head.mkString(", ")}), (${d2.last.mkString(", ")}), ${d2.size}
          |1:     (${d1.head.mkString(", ")}), (${d1.last.mkString(", ")}), ${d1.size}
          |2-2:   (${d22.head.mkString(", ")}), (${d22.last.mkString(", ")}), ${d22.size}
          |2-4:   (${d24.head.mkString(", ")}), (${d24.last.mkString(", ")}), ${d24.size}
          |4-2:   (${d42.head.mkString(", ")}), (${d42.last.mkString(", ")}), ${d42.size}
          |6-4-2: (${d642.head.mkString(", ")}), (${d642.last.mkString(", ")}), ${d642.size}
          |""".stripMargin)
  }

  def primesByDiffs(diffs: Int*)(max: Int): LazyList[Vector[Int]] = {
    primesSliding(diffs.size + 1)
      .takeWhile(_.last <= max)
      .filter{vec => diffs.zip(vec.init).map{case (a, b) => a + b} == vec.tail}
      .to(LazyList)
  }

  def primesSliding(len: Int): Iterator[Vector[Int]] = primes.sliding(len).map(_.toVector)
  def primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(n => !Iterator.range(3, math.sqrt(n).toInt + 1, 2).exists(n%_ == 0))
}
```


{{out}}

```txt
Diffs: (First), (Last), Count
2:     (3, 5), (999959, 999961), 8169
1:     (2, 3), (2, 3), 1
2-2:   (3, 5, 7), (3, 5, 7), 1
2-4:   (5, 7, 11), (999431, 999433, 999437), 1393
4-2:   (7, 11, 13), (997807, 997811, 997813), 1444
6-4-2: (31, 37, 41, 43), (997141, 997147, 997151, 997153), 306
```



## Sidef


```ruby
var limit  = 1e6
var primes = limit.primes

say "Groups of successive primes <= #{limit.commify}:"

for diffs in [[2], [1], [2,2], [2,4], [4,2], [6,4,2]] {

    var groups = []
    primes.each_cons(diffs.len+1, {|*group|
        if (group.map_cons(2, {|a,b| b-a}) == diffs) {
            groups << group
        }
    })

    say ("...for differences #{diffs}, there are #{groups.len} groups, where ",
         "the first group = #{groups.first} and the last group = #{groups.last}")
}
```

{{out}}

```txt

Groups of successive primes <= 1,000,000:
...for differences [2], there are 8169 groups, where the first group = [3, 5] and the last group = [999959, 999961]
...for differences [1], there are 1 groups, where the first group = [2, 3] and the last group = [2, 3]
...for differences [2, 2], there are 1 groups, where the first group = [3, 5, 7] and the last group = [3, 5, 7]
...for differences [2, 4], there are 1393 groups, where the first group = [5, 7, 11] and the last group = [999431, 999433, 999437]
...for differences [4, 2], there are 1444 groups, where the first group = [7, 11, 13] and the last group = [997807, 997811, 997813]
...for differences [6, 4, 2], there are 306 groups, where the first group = [31, 37, 41, 43] and the last group = [997141, 997147, 997151, 997153]

```



## zkl

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library
Using GMP ( probabilistic primes),
because it is easy and fast to generate primes.

[[Extensible prime generator#zkl]] could be used instead.

Treat this as a string search problem.

```zkl
const PRIME_LIMIT=1_000_000;
var [const] BI=Import("zklBigNum");  // libGMP
var [const] primeBitMap=Data(PRIME_LIMIT).fill(0x30);   // one big string
p:= BI(1);
while(p.nextPrime()<=PRIME_LIMIT){ primeBitMap[p]="1" } // bitmap of primes

fcn primeWindows(m,deltas){ // eg (6,4,2)
   n,r := 0,List();
   ds:=deltas.len().pump(List,'wrap(n){ deltas[0,n+1].sum(0) });  // (6,10,12)
   sp:=Data(Void,"1" + "0"*deltas.sum(0));
   foreach n in (ds){ sp[n]="1" } // "1000001000101"
   while(n=primeBitMap.find(sp,n+1)){ r.append(n) }  //  (31, 61, 271,...)
   r.apply('wrap(n){ T(n).extend(ds.apply('+(n))) }) //( (31,37,41,43), (61,67,71,73), (271,277,281,283) ...)
}
```


```zkl
foreach w in (T( T(2), T(1), T(2,2), T(2,4), T(4,2), T(6,4,2) )){
   r:=primeWindows(PRIME_LIMIT,w);
   println("Successive primes (<=%,d) with deltas of %s (%,d groups):"
      .fmt(PRIME_LIMIT,w.concat(","),r.len()));
   println("   First group: %s;  Last group: %s\n"
      .fmt(r[0].concat(", "),r[-1].concat(", ")));
}
```

{{out}}

```txt

Successive primes (<=1,000,000) with deltas of 2 (8,169 groups):
   First group: 3, 5;  Last group: 999959, 999961

Successive primes (<=1,000,000) with deltas of 1 (1 groups):
   First group: 2, 3;  Last group: 2, 3

Successive primes (<=1,000,000) with deltas of 2,2 (1 groups):
   First group: 3, 5, 7;  Last group: 3, 5, 7

Successive primes (<=1,000,000) with deltas of 2,4 (1,393 groups):
   First group: 5, 7, 11;  Last group: 999431, 999433, 999437

Successive primes (<=1,000,000) with deltas of 4,2 (1,444 groups):
   First group: 7, 11, 13;  Last group: 997807, 997811, 997813

Successive primes (<=1,000,000) with deltas of 6,4,2 (306 groups):
   First group: 31, 37, 41, 43;  Last group: 997141, 997147, 997151, 997153

```

