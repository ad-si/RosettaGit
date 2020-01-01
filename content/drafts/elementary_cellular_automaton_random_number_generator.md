+++
title = "Elementary cellular automaton/Random Number Generator"
description = ""
date = 2019-08-02T10:45:16Z
aliases = []
[extra]
id = 17411
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
[[wp:Rule 30|Rule 30]] is considered to be chaotic enough to generate good pseudo-random numbers.  As a matter of fact, rule 30 is used by the [[wp:Mathematica|Mathematica]] software for its default random number generator.

Steven Wolfram's recommendation for random number generation from rule 30 consists in extracting successive bits in a fixed position in the array of cells, as the automaton changes state.

The purpose of this task is to demonstrate this.  With the code written in the [[Elementary cellular automaton|parent task]], which you don't need to re-write here, show the ten first bytes that emerge from this recommendation.  To be precise, you will start with a state of all cells but one equal to zero, and you'll follow the evolution of the particular cell whose state was initially one.  Then you'll regroup those bits by packets of eight, reconstituting bytes with the first bit being the [[wp:most significant bit|most significant]].

You can pick which ever length you want for the initial array but it should be visible in the code so that your output can be reproduced with an other language.

For extra-credits, you will make this algorithm run as fast as possible in your language, for instance with an extensive use of bitwise logic.

;Reference:
* [http://www.cs.indiana.edu/~dgerman/2005midwestNKSconference/dgelbm.pdf Cellular automata: Is Rule 30 random]? (PDF).

## C

64-bits array size, cyclic borders.

```c
#include <stdio.h>
#include <limits.h>

typedef unsigned long long ull;
#define N (sizeof(ull) * CHAR_BIT)
#define B(x) (1ULL << (x))

void evolve(ull state, int rule)
{
	int i, p, q, b;

	for (p = 0; p < 10; p++) {
		for (b = 0, q = 8; q--; ) {
			ull st = state;
			b |= (st&1) << q;

			for (state = i = 0; i < N; i++)
				if (rule & B(7 & (st>>(i-1) | st<<(N+1-i))))
					state |= B(i);
		}
		printf(" %d", b);
	}
	putchar('\n');
	return;
}

int main(void)
{
	evolve(1, 30);
	return 0;
}
```

{{out}}

```txt
 220 197 147 174 117 97 149 171 100 151
```



## C++

We'll re-write the code of the parent task here.

```cpp
#include <bitset>
#include <stdio.h>

#define SIZE	           80
#define RULE               30
#define RULE_TEST(x)       (RULE & 1 << (7 & (x)))

void evolve(std::bitset<SIZE> &s) {
    int i;
    std::bitset<SIZE> t(0);
    t[SIZE-1] = RULE_TEST( s[0] << 2 | s[SIZE-1] << 1 | s[SIZE-2] );
    t[     0] = RULE_TEST( s[1] << 2 | s[     0] << 1 | s[SIZE-1] );
    for (i = 1; i < SIZE-1; i++)
	t[i] = RULE_TEST( s[i+1] << 2 | s[i] << 1 | s[i-1] );
    for (i = 0; i < SIZE; i++) s[i] = t[i];
}
void show(std::bitset<SIZE> s) {
    int i;
    for (i = SIZE; i--; ) printf("%c", s[i] ? '#' : ' ');
    printf("|\n");
}
unsigned char byte(std::bitset<SIZE> &s) {
    unsigned char b = 0;
    int i;
    for (i=8; i--; ) {
	b |= s[0] << i;
	evolve(s);
    }
    return b;
}

int main() {
    int i;
    std::bitset<SIZE> state(1);
    for (i=10; i--; )
	printf("%u%c", byte(state), i ? ' ' : '\n');
    return 0;
}
```

{{out}}

```txt
220 197 147 174 117 97 149 171 240 241
```



## D

{{trans|C}}
Adapted from the C version, with improvements and bug fixes. Optimized for performance as requested in the task description. This is a lazy range.

```d
import std.stdio, std.range, std.typecons;

struct CellularRNG {
    private uint current;
    private immutable uint rule;
    private ulong state;

    this(in ulong state_, in uint rule_) pure nothrow @safe @nogc {
        this.state = state_;
        this.rule = rule_;
        popFront;
    }

    public enum bool empty = false;
    @property uint front() pure nothrow @safe @nogc { return current; }

    void popFront() pure nothrow @safe @nogc {
        enum uint nBit = 8;
        enum uint NU = ulong.sizeof * nBit;
        current = 0;

        foreach_reverse (immutable i; 0 .. nBit) {
            immutable state2 = state;
            current |= (state2 & 1) << i;

            state = 0;
            /*static*/ foreach (immutable j; staticIota!(0, NU)) {
                // To avoid undefined behavior with out-of-range shifts.
                static if (j > 0)
                    immutable aux1 = state2 >> (j - 1);
                else
                    immutable aux1 = state2 >> 63;

                static if (j == 0)
                    immutable aux2 = state2 << 1;
                else static if (j == 1)
                    immutable aux2 = state2 << 63;
                else
                    immutable aux2 = state2 << (NU + 1 - j);

                immutable aux = 7 & (aux1 | aux2);
                if (rule & (1UL << aux))
                    state |= 1UL << j;
            }
        }
    }
}

void main() {
    CellularRNG(1, 30).take(10).writeln;
    CellularRNG(1, 30).drop(2_000_000).front.writeln;
}
```

{{out}}

```txt
[220, 197, 147, 174, 117, 97, 149, 171, 100, 151]
44
```

Run-time: less than two seconds with the ldc2 compiler.

=={{header|F_Sharp|F#}}==
This task uses [[Elementary cellular automaton#The_Function]]

```fsharp

// Generate random numbers using Rule 30. Nigel Galloway: August 1st., 2019
eca 30 [|yield 1; yield! Array.zeroCreate 99|]|>Seq.chunkBySize 8|>Seq.map(fun n->n|>Array.mapi(fun n g->g.[0]<<<(7-n))|>Array.sum)|>Seq.take 10|>Seq.iter(printf "%d "); printfn ""

```

{{out}}

```txt

220 197 147 174 117 97 149 171 240 241

```



## Go

{{trans|C}}

```go
package main

import "fmt"

const n = 64

func pow2(x uint) uint64 {
    return uint64(1) << x
}

func evolve(state uint64, rule int) {
    for p := 0; p < 10; p++ {
        b := uint64(0)
        for q := 7; q >= 0; q-- {
            st := state
            b |= (st & 1) << uint(q)
            state = 0
            for i := uint(0); i < n; i++ {
                var t1, t2, t3 uint64
                if i > 0 {
                    t1 = st >> (i - 1)
                } else {
                    t1 = st >> 63
                }
                if i == 0 {
                    t2 = st << 1
                } else if i == 1 {
                    t2 = st << 63

                } else {
                    t2 = st << (n + 1 - i)
                }
                t3 = 7 & (t1 | t2)
                if (uint64(rule) & pow2(uint(t3))) != 0 {
                    state |= pow2(i)
                }
            }
        }
        fmt.Printf("%d ", b)
    }
    fmt.Println()
}

func main() {
    evolve(1, 30)
}
```


{{out}}

```txt

220 197 147 174 117 97 149 171 100 151

```



## Haskell


Assume the comonadic solution given at [[Elementary cellular automaton#Haskell]] is packed in a module <code>CellularAutomata</code>


```Haskell
import CellularAutomata (runCA, rule, fromList)
import Data.List (unfoldr)
import Control.Comonad

rnd = fromBits <$> unfoldr (pure . splitAt 8) bits
  where size = 80
        bits = extract <$> runCA (rule 30) (fromList (1:replicate size 0))

fromBits = foldl (\res x -> 2*res + x) 0
```


{{Out}}

```txt
λ> take 10 rnd
[220,197,147,174,117,97,149,171,240,241]
```


Using the rule 30 CA it is possible to determine the <code>RandomGen</code> instance which could be utilized by the <code>Random</code> class:


```Haskell
import System.Random

instance RandomGen (Cycle Int) where
  next c = let x = c =>> step (rule 30) in (fromBits (view x), x)
  split c = (c, fromList (reverse (view c)))
```



```txt
λ> let r30 = fromList [1,0,1,0,1,0,1,0,1,0,1,0,1] :: Cycle Int

λ> take 15 $ randoms r30
[7509,4949,2517,2229,2365,2067,6753,5662,5609,7576,2885,3017,2912,5081,2356]

λ> take 30 $ randomRs ('A','J') r30
"DHJHHFJHBDDFCBHACHDEHDHFBAEJFE"
```


We can compare it with standard generator on a small integer range, using simple bin counter:


```txt
λ> let bins lst = [ (n, length (filter (==n) lst)) | n <- nub lst]

λ> bins . take 10000 . randomRs ('A','J') $ r30
[('D',1098),('H',1097),('J',1093),('F',850),('B',848),('C',1014),('A',1012),('E',1011),('G',1253),('I',724)]

λ> bins . take 10000 . randomRs ('A','J') <$> getStdGen
[('G',975),('B',1035),('F',970),('J',1034),('I',956),('H',984),('C',1009),('E',1023),('A',1009),('D',1005)]
```



## J

ca is a cellular automata class.  The rng class inherits ca and extends it with bit and byte verbs to sample the ca.

```J

coclass'ca'
DOC =: 'locale creation: (RULE ; INITIAL_STATE) conew ''ca'''
create =: 3 :'''RULE STATE'' =: y'
next =: 3 :'STATE =: RULE (((8$2) #: [) {~ [: #. [: -. [: |: |.~"1 0&_1 0 1@]) STATE'
coclass'base'

coclass'rng'
coinsert'ca'
bit =: 3 :'([ next) ({. STATE)'
byte =: [: #. [: , [: bit"0 (i.8)"_
coclass'base'

```

Having installed these into a j session we create and use the mathematica prng.

```txt

   m =: (30 ; 64 {. 1) conew 'rng'
   byte__m"0 i.10
220 197 147 174 117 97 149 171 100 151

```



## Kotlin

{{trans|C}}

```scala
// version 1.1.51

const val N = 64

fun pow2(x: Int) = 1L shl x

fun evolve(state: Long, rule: Int) {
    var state2 = state
    for (p in 0..9) {
        var b = 0
        for (q in 7 downTo 0) {
            val st = state2
            b = (b.toLong() or ((st and 1L) shl q)).toInt()
            state2 = 0L
            for (i in 0 until N) {
                val t = ((st ushr (i - 1)) or (st shl (N + 1 - i)) and 7L).toInt()
                if ((rule.toLong() and pow2(t)) != 0L) state2 = state2 or pow2(i)
            }
        }
        print(" $b")
    }
    println()
}

fun main(args: Array<String>) {
    evolve(1, 30)
}
```


{{out}}

```txt

 220 197 147 174 117 97 149 171 100 151

```



## Pascal

{{Works with|Free Pascal}}
Using ROR and ROL is as fast as assembler and more portable.<BR>[https://tio.run/##7VZdb@pGEH33r5iHSEAvYJsQ0kBTifBxawmwC6a9bVVFjr3AKmZtrZdwaZS/Xjq7iwPckOThvvQhSHx45szMmbPD7qZBFgZxZZaG263HkzkPljBexeTcahmmuRAibZomYdU1vacpiWhQTfjclE/miHwVtxMRCHIrI26PQpaBWKwTHkfVdRLPMGs1TJamzlxdiGVsPJ45/W6vD32v82QAPJ4Nk4hAl8Tpgj49nrUnw6Hb7YEz8nsDDXA93xk6f7Z9xx2BOyq3B@gwTenqILQ9cD6PIOVJeG0/YfreYNLTgW3P8//wetBxRxN30FPOUdfpPxmrjGQImWyyqaBx1jLChGUCLcvg6zhZsSiDa6j9YFuWpT5a6OLajq9rsNFiPAQczbo3LQg0YUqZaNTRO1uxUNCEwWciOt701qdL0oSdV2xSgrF@J11hNk7ChEcGHLx@oegqH5kGiUQ3oYv6Rq29izB80lwQIBAh07aMOzKnDI1BtpQ0u/6kI6OG7m86BXiCw18I9asq9d/lXvvLKwBFBwFdCVAFAdZULHTFKFFsOMlWMda/1l0WMcibliBbxHBeg0@6gZahwg25XiRacQIOo@JQxBZlMWVk38ChE5PbL1OcGMk8iRaGr1gZR4Q8lBlC96uUl0A/SOJHPNDuYYiyu@NfpSRFmcguSZdMpF2Db11HIyExRQV2x7JOCb7gD8kl7@N5UmQbOvZmg62OAkEfiMOE/H816pUbx4cwYIDDi3PKNij4nGaC8OydLnGsNy5T0@loy807fe@X0tqHgxYfwDSnjCcxCjyHgEWgtJYPL9cB7jawQMxRzqL@@Ul/laQwUj1oj7q61HsCv7EseUevSf5B5IPIB5H/LZHj0/S9nXN/AkxSQiJBMpHv@L5d9i3c8ZzRbgukTTjYUPMNcM2pIDErFp4TwCzhkMn6Gf2HQDKDQrlRLxfgjoqsUNL73@8BX8IqlTguQCSwDu6JNOCJh4A@pqBqu9zdH9RHxcaTcs0QbeVH5qm7lCRvyeCDi4Os@uKc3BXSlZ4vLq9U2Z8rLaXOiQK5Fsfw0qGrEG7CmGSQEg7SiQIXykXfrgirZD5TaFrN2mHYy@Xyg@w@XymqbkVkTviJZfEXBASCQSv/tga2XIndNW3Xukr0TUfN@ilyeWk1CL6aJNjxzNMU4KceXkvHP0s2nATRLrqK5zNec1MakwjkQU2F8cY8Nepqlox63XgpJ16Try4MI@/bgFrNAvvqEuw6vi/rYNuXAOr5Cp9tOWJgX9hGzs04JHNe@y4ydu3H6kXju9hst/@GsziYZ9uKe76tTB7@Aw Try it online!] counting CPU-Cycles 32 vs 31 on Ryzen Zen1 per Byte -> 100Mb/s

```pascal
Program Rule30;
//http://en.wikipedia.org/wiki/Next_State_Rule_30;
//http://mathworld.wolfram.com/Rule30.html
{$IFDEF FPC}
  {$Mode Delphi}{$ASMMODE INTEL}
  {$OPTIMIZATION ON,ALL}
//  {$CODEALIGN proc=1}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  SysUtils;
const
  maxRounds = 2*1000*1000;
  rounds    = 10;

var
  Rule30_State : Uint64;

function GetCPU_Time: int64;
type
  TCpu = record
            HiCpu,
            LoCpu : Dword;
         end;
var
  Cput : TCpu;
begin
  asm
  RDTSC;
  MOV Dword Ptr [CpuT.LoCpu],EAX
  MOV Dword Ptr [CpuT.HiCpu],EDX
  end;
  with Cput do
    result := int64(HiCPU) shl 32 + LoCpu;
end;

procedure InitRule30_State;inline;
begin
  Rule30_State:= 1;
end;

procedure Next_State_Rule_30;inline;
var
  run, prev,next: Uint64;
begin
  run  := Rule30_State;
  Prev := RORQword(run,1);
  next := ROLQword(run,1);
  Rule30_State  := (next OR run) XOR prev;
end;

function NextRule30Byte:NativeInt;
//64-BIT can use many registers
//32-Bit still fast
var
  run, prev,next: Uint64;
  myOne : UInt64;
Begin
  run  := Rule30_State;
  result := 0;
  myOne  := 1;
  //Unrolling and inlining Next_State_Rule_30 by hand
  result := (result+result) OR (run AND myOne);
  next := ROLQword(run,1);
  Prev := RORQword(run,1);
  run  := (next OR run) XOR prev;

  result := (result+result) OR (run AND myOne);
  next := ROLQword(run,1);
  Prev := RORQword(run,1);
  run  := (next OR run) XOR prev;

  result := (result+result) OR (run AND myOne);
  next := ROLQword(run,1);
  Prev := RORQword(run,1);
  run  := (next OR run) XOR prev;

  result := (result+result) OR (run AND myOne);
  next := ROLQword(run,1);
  Prev := RORQword(run,1);
  run  := (next OR run) XOR prev;

  result := (result+result) OR (run AND myOne);
  next := ROLQword(run,1);
  Prev := RORQword(run,1);
  run  := (next OR run) XOR prev;

  result := (result+result) OR (run AND myOne);
  next := ROLQword(run,1);
  Prev := RORQword(run,1);
  run  := (next OR run) XOR prev;

  result := (result+result) OR (run AND myOne);
  next := ROLQword(run,1);
  Prev := RORQword(run,1);
  run  := (next OR run) XOR prev;

  result := (result+result) OR (run AND myOne);
  next := ROLQword(run,1);
  Prev := RORQword(run,1);
  Rule30_State := (next OR run) XOR prev;
end;

procedure Speedtest;
var
  T1,T0 : INt64;
  i: NativeInt;
Begin
  writeln('Speedtest for statesize of ',64,' bits');
  //Warm up start to wake up CPU takes some time
  For i := 100*1000*1000-1 downto 0 do
    Next_State_Rule_30;

  T0 := GetCPU_Time;
  InitRule30_State;
  For  i := maxRounds-1 downto 0 do
    NextRule30Byte;
  T1 := GetCPU_Time;
  writeln(NextRule30Byte);
  writeln('cycles per Byte : ',(T1-t0)/maxRounds:0:2);
  writeln;
end;

procedure Task;
var
  i: integer;
Begin
  writeln('The task ');
  InitRule30_State;
  For  i := 1 to rounds do
    write(NextRule30Byte:4);
  writeln;
end;

Begin
  SpeedTest;
  Task;
  write(' <ENTER> ');readln;
end.
```

{{out}}

```txt
//compiled 64-Bit
Speedtest for statesize of 64 bits
44
cycles per Byte : 30.95

The task
 220 197 147 174 117  97 149 171 100 151
 <ENTER>

//compiled 32-Bit
Speedtest for statesize of 64 bits
44
cycles per Byte : 128.56

The task
 220 197 147 174 117  97 149 171 100 151
 <ENTER>
```



## Perl

{{trans|Perl 6}}

```perl
package Automaton {
    sub new {
    my $class = shift;
    my $rule = [ reverse split //, sprintf "%08b", shift ];
    return bless { rule => $rule, cells => [ @_ ] }, $class;
    }
    sub next {
    my $this = shift;
    my @previous = @{$this->{cells}};
    $this->{cells} = [
        @{$this->{rule}}[
        map {
          4*$previous[($_ - 1) % @previous]
        + 2*$previous[$_]
        +   $previous[($_ + 1) % @previous]
        } 0 .. @previous - 1
        ]
    ];
    return $this;
    }
    use overload
    q{""} => sub {
    my $this = shift;
    join '', map { $_ ? '#' : ' ' } @{$this->{cells}}
    };
}

my $a = Automaton->new(30, 1, map 0, 1 .. 100);

for my $n (1 .. 10) {
    my $sum = 0;
    for my $b (1 .. 8) {
	$sum = $sum * 2 + $a->{cells}[0];
	$a->next;
    }
    print $sum, $n == 10 ? "\n" : " ";
}
```

{{out}}

```txt
220 197 147 174 117 97 149 171 240 241
```



## Perl 6


```perl6
class Automaton {
    has $.rule;
    has @.cells;
    has @.code = $!rule.fmt('%08b').flip.comb».Int;

    method gist { "|{ @!cells.map({+$_ ?? '#' !! ' '}).join }|" }

    method succ {
        self.new: :$!rule, :@!code, :cells(
            @!code[
                    4 «*« @!cells.rotate(-1)
                »+« 2 «*« @!cells
                »+«       @!cells.rotate(1)
            ]
        )
    }
}

my Automaton $a .= new: :rule(30), :cells( flat 1, 0 xx 100 );

say :2[$a++.cells[0] xx 8] xx 10;
```

{{out}}

```txt
220 197 147 174 117 97 149 171 240 241
```



## Phix

Making the minimum possible changes to [[Elementary_cellular_automaton#Phix]], output matches C, D, Go, J, Kotlin, Racket, and zkl,
and with the changes marked [2] C++, Haskell, Perl, Python, Ruby, Scheme, and Sidef, but completely different to Rust and Tcl.
No attempt to optimise.

```Phix
--string s = ".........#.........", --(original)
string s = "...............................#"&
           "................................",
--string s = "#"&repeat('.',100),   -- [2]
       t=s, r = "........"
integer rule = 30, k, l = length(s), w = 0
for i=1 to 8 do
    r[i] = iff(mod(rule,2)?'#':'.')
    rule = floor(rule/2)
end for
sequence res = {}
for i=0 to 80 do
    w = w*2 + (s[32]='#')
--  w = w*2 + (s[1]='#')            -- [2]
    if mod(i+1,8)=0 then res&=w w=0 end if
    for j=1 to l do
        k = (s[iff(j=1?l:j-1)]='#')*4
          + (s[          j   ]='#')*2
          + (s[iff(j=l?1:j+1)]='#')+1
        t[j] = r[k]
    end for
    s = t
end for
?res
```

{{out}}

```txt

{220,197,147,174,117,97,149,171,100,151}

```

{{out}}
with the changes marked [2]

```txt

{220,197,147,174,117,97,149,171,240,241}

```



## Python


### Python: With zero padded ends


```python
from elementary_cellular_automaton import eca, eca_wrap

def rule30bytes(lencells=100):
    cells = '1' + '0' * (lencells - 1)
    gen = eca(cells, 30)
    while True:
        yield int(''.join(next(gen)[0] for i in range(8)), 2)

if __name__ == '__main__':
    print([b for i,b in zip(range(10), rule30bytes())])
```


{{out}}

```txt
[255, 255, 255, 255, 255, 255, 255, 255, 255, 255]
```

!


### Python: With wrapping of end cells


```python
def rule30bytes(lencells=100):
    cells = '1' + '0' * (lencells - 1)
    gen = eca_wrap(cells, 30)
    while True:
        yield int(''.join(next(gen)[0] for i in range(8)), 2))
```


{{out}}

```txt
[220, 197, 147, 174, 117, 97, 149, 171, 240, 241]
```



## Racket


Implementation of [[Elementary cellular automaton]] is saved in "Elementary_cellular_automata.rkt"


```racket
#lang racket
;; below is the code from the parent task
(require "Elementary_cellular_automata.rkt")
(require racket/fixnum)

;; This is the RNG automaton
(define (CA30-random-generator
         #:rule [rule 30] ; rule 30 is random, maybe you're interested in using others
         ;; width of the CA... this is implemented as a number of words plus,
         ;; maybe, another word containing the spare bits
         #:bits [bits 256])
  (define-values [full-words more-bits]
    (quotient/remainder bits usable-bits/fixnum))
  (define wrap-rule
    (and (positive? more-bits) (wrap-rule-truncate-left-word more-bits)))
  (define next-gen (CA-next-generation 30 #:wrap-rule wrap-rule))
  (define v (make-fxvector (+ full-words (if more-bits 1 0))))
  (fxvector-set! v 0 1) ; this bit will always have significance

  (define (next-word)
    (define-values [v+ o] (next-gen v 0))
    (begin0 (fxvector-ref v 0) (set! v v+)))

  (lambda (bits)
    (for/fold ([acc 0]) ([_ (in-range bits)])
      ;; the CA is fixnum, but this function returns integers of arbitrary width
      (bitwise-ior (arithmetic-shift acc 1) (bitwise-and (next-word) 1)))))

(module+ main
  ;; To match the other examples on this page, the automaton is 30+30+4 bits long
  ;; (i.e. 64 bits)
  (define C30-rand-64 (CA30-random-generator #:bits 64))
  ;; this should be the list from "C"
  (for/list ([i 10]) (C30-rand-64 8))

  ; we also do big numbers...
  (number->string (C30-rand-64 256) 16)
  (number->string (C30-rand-64 256) 16)
  (number->string (C30-rand-64 256) 16)
  (number->string (C30-rand-64 256) 16))
```


{{out}}


```txt
(220 197 147 174 117 97 149 171 100 151)
"ecd9fbcdcc34604d833950deb58447124b98706e74ccc74d9337cb4e53f38c5e"
"9c8b6471a4bc2cb3508f10b6635e4eb959ad8bbe484480695e8ddb5795f956a"
"6d85153a987dad6f013bc6159a41bf95b9d9b14af87733e17c702a3dc9052172"
"fc6fd302f5ea8f2fba6f476cfe9d090dc877dbd558e5afba49044d05b14d258"
```



## Ruby


```ruby
size = 100
eca = ElemCellAutomat.new("1"+"0"*(size-1), 30)
eca.take(80).map{|line| line[0]}.each_slice(8){|bin| p bin.join.to_i(2)}
```

{{out}}

```txt

220
197
147
174
117
97
149
171
240
241

```


## Rust


```rust

//Assuming the code from the Elementary cellular automaton task is in the namespace.
fn main() {
    struct WolfGen(ElementaryCA);
    impl WolfGen {
        fn new() -> WolfGen {
            let (_, ca) = ElementaryCA::new(30);
            WolfGen(ca)
        }
        fn next(&mut self) -> u8 {
            let mut out = 0;
            for i in 0..8 {
                out |= ((1 & self.0.next())<<i)as u8;
            }
            out
        }
    }
    let mut gen = WolfGen::new();
    for _ in 0..10 {
        print!("{} ", gen.next());
    }
}

```

{{out}}

```txt

157 209 228 58 87 195 212 106 147 244

```


## Scheme


```scheme

; uses SRFI-1 library http://srfi.schemers.org/srfi-1/srfi-1.html

(define (random-r30 n)
  (let ((r30 (vector 0 1 1 1 1 0 0 0)))
    (fold
      (lambda (x y ls)
	(if (= x 1)
	  (cons (* x y) ls)
	  (cons (+ (car ls) (* x y)) (cdr ls))))
      '()
      (circular-list 1 2 4 8 16 32 64 128)
      (unfold-right
	(lambda (x) (zero? (car x)))
	cadr
	(lambda (x) (cons (- (car x) 1)
			  (evolve (cdr x) r30)))
	(cons (* 8 n) (cons 1 (make-list 79 0))))))) ; list

(random-r30 10)

```


{{out}}


```txt

(220 197 147 174 117 97 149 171 240 241)

```



## Sidef


```ruby
var auto = Automaton(30, [1] + 100.of(0));

10.times {
    var sum = 0;
    8.times {
        sum = (2*sum + auto.cells[0]);
        auto.next;
    };
    say sum;
};
```

{{out}}

```txt

220
197
147
174
117
97
149
171
240
241

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
oo::class create RandomGenerator {
    superclass ElementaryAutomaton
    variable s
    constructor {stateLength} {
	next 30
	set s [split 1[string repeat 0 $stateLength] ""]
    }

    method rand {} {
	set bits {}
	while {[llength $bits] < 8} {
	    lappend bits [lindex $s 0]
	    set s [my evolve $s]
	}
	return [scan [join $bits ""] %b]
    }
}
```

Demonstrating:

```tcl
set rng [RandomGenerator new 31]
for {set r {}} {[llength $r]<10} {} {
    lappend r [$rng rand]
}
puts [join $r ,]
```

{{out}}
 220,197,147,174,241,126,135,130,143,234
Note that as the number of state bits is increased (the parameter to the constructor), the sequence tends to a limit of <math>220,</math> <math>197,</math> <math>147,</math> <math>174,</math> <math>117,</math> <math>97,</math> <math>149,</math> <math>171,</math> <math>240,</math> <math>241,</math> <math>\ldots</math> and that deviations from this are due to interactions between the state modification “wavefront” as the automaton wraps round.


## zkl

No attempts at extra credit and not fast.

```zkl
fcn rule(n){ n=n.toString(2); "00000000"[n.len() - 8,*] + n }
fcn applyRule(rule,cells){
   cells=String(cells[-1],cells,cells[0]);  // wrap edges
   (cells.len() - 2).pump(String,'wrap(n){ rule[7 - cells[n,3].toInt(2)] })
}
fcn rand30{
   var r30=rule(30), cells="0"*63 + 1; // 64 bits (8 bytes), arbitrary
   n:=0;
   do(8){
      n=n*2 + cells[-1];          // append bit 0
      cells=applyRule(r30,cells); // next state
   }
   n
}
```

Note that "var" in a function is "static" in C, ie function local variables, initialized once.

```zkl
do(10){ rand30().print(","); }
```

{{out}}

```txt
220,197,147,174,117,97,149,171,100,151,
```

