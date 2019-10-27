+++
title = "Unbias a random generator"
description = ""
date = 2019-07-01T15:34:41Z
aliases = []
[extra]
id = 9284
[taxonomies]
categories = []
tags = []
+++

{{task}}Given a weighted one bit generator of random numbers where the probability of a one occuring, <math>P_1</math>, is not the same as <math>P_0</math>, the probability of a zero occuring, the probability of the occurrence of a one followed by a zero is <math>P_1</math> × <math>P_0</math>. This is the same as the probability of a zero followed by a one: <math>P_0</math> × <math>P_1</math>.


;Task details:
* Use your language's random number generator to create a function/method/subroutine/... '''randN''' that returns a one or a zero, but with one occurring, on average, 1 out of N times, where N is an integer from the range 3 to 6 inclusive.
* Create a function '''unbiased''' that uses only randN as its source of randomness to become an unbiased generator of random ones and zeroes.
* For N over its range, generate and show counts of the outputs of randN and unbiased(randN).



The actual unbiasing should be done by generating two numbers at a time from randN and only returning a 1 or 0 if they are different. As long as you always return the first number or always return the second number, the probabilities discussed above should take over the biased probability of randN.

This task is an implementation of [http://en.wikipedia.org/wiki/Randomness_extractor#Von_Neumann_extractor Von Neumann debiasing], first described in a 1951 paper.





## Ada


```Ada
with Ada.Text_IO; with Ada.Numerics.Discrete_Random;

procedure Bias_Unbias is

   Modulus: constant Integer := 60; -- lcm of {3,4,5,6}
   type M is mod Modulus;
   package Rand is new Ada.Numerics.Discrete_Random(M);
   Gen: Rand.Generator;

   subtype Bit is Integer range 0 .. 1;

   function Biased_Bit(Bias_Base: Integer) return Bit is
   begin
      if (Integer(Rand.Random(Gen))* Bias_Base) / Modulus > 0 then
         return 0;
      else
         return 1;
      end if;
   end Biased_Bit;

   function Unbiased_Bit(Bias_Base: Integer) return Bit is
      A, B: Bit := 0;
   begin
      while A = B loop
         A := Biased_Bit(Bias_Base);
         B := Biased_Bit(Bias_Base);
      end loop;
      return A;
   end Unbiased_Bit;

   package FIO is new Ada.Text_IO.Float_IO(Float);

   Counter_B, Counter_U: Natural;
   Number_Of_Samples: constant Natural := 10_000;

begin
   Rand.Reset(Gen);
   Ada.Text_IO.Put_Line(" I  Biased% UnBiased%");
   for I in 3 .. 6 loop
      Counter_B := 0;
      Counter_U := 0;
      for J in 1 .. Number_Of_Samples loop
         Counter_B := Counter_B + Biased_Bit(I);
         Counter_U := Counter_U + Unbiased_Bit(I);
      end loop;
      Ada.Text_IO.Put(Integer'Image(I));
      FIO.Put(100.0 * Float(Counter_B) / Float(Number_Of_Samples), 5, 2, 0);
      FIO.Put(100.0 * Float(Counter_U) / Float(Number_Of_Samples), 5, 2, 0);
      Ada.Text_IO.New_Line;
   end loop;
end Bias_Unbias;
```

Output:
```txt
 I  Biased% UnBiased%
 3   32.87   49.80
 4   24.49   50.22
 5   19.73   50.05
 6   16.75   50.19

```



## Aime

{{trans|C}}

```aime
integer
biased(integer bias)
{
    1 ^ min(drand(bias - 1), 1);
}

integer
unbiased(integer bias)
{
    integer a;

    while ((a = biased(bias)) == biased(bias)) {
    }

    a;
}

integer
main(void)
{
    integer b, n, cb, cu, i;

    n = 10000;
    b = 3;
    while (b <= 6) {
        i = cb = cu = 0;
        while ((i += 1) <= n) {
            cb += biased(b);
            cu += unbiased(b);
        }

        o_form("bias ~: /d2p2/%% vs /d2p2/%%\n", b, 100r * cb / n,
               100r * cu / n);

        b += 1;
    }

    0;
}
```

Output:
```txt
bias 3: 33.51% vs 50.27%
bias 4: 24.97% vs 49.99%
bias 5: 19.93% vs 49.92%
bias 6: 16.32% vs 49.36%
```



## AutoHotkey

{{output?}}

```AutoHotkey
Biased(){
   Random, q, 0, 4
   return q=4
}
Unbiased(){
   Loop
      If ((a := Biased()) != biased())
          return a
}
Loop 1000
   t .= biased(), t2 .= unbiased()
StringReplace, junk, t2, 1, , UseErrorLevel
MsgBox % "Unbiased probability of a 1 occurring: " Errorlevel/1000
StringReplace, junk, t, 1, , UseErrorLevel
MsgBox % "biased probability of a 1 occurring: " Errorlevel/1000
```



## BBC BASIC


```bbcbasic
      FOR N% = 3 TO 6
        biased% = 0
        unbiased% = 0
        FOR I% = 1 TO 10000
          IF FNrandN(N%) biased% += 1
          IF FNunbiased(N%) unbiased% += 1
        NEXT
        PRINT "N = ";N% " : biased = "; biased%/100 "%, unbiased = "; unbiased%/100 "%"
      NEXT
      END
      
      DEF FNunbiased(N%)
      LOCAL A%,B%
      REPEAT
        A% = FNrandN(N%)
        B% = FNrandN(N%)
      UNTIL A%<>B%
      = A%
      
      DEF FNrandN(N%) = -(RND(N%) = 1)
```

Output:

```txt

N = 3 : biased = 33.57%, unbiased = 49.94%
N = 4 : biased = 25.34%, unbiased = 50.76%
N = 5 : biased = 20.06%, unbiased = 50.04%
N = 6 : biased = 16.25%, unbiased = 50.13%

```



## C


```C>#include <stdio.h

#include <stdlib.h>
 
int biased(int bias)
{
	/* balance out the bins, being pedantic */
	int r, rand_max = RAND_MAX - (RAND_MAX % bias);
	while ((r = rand()) > rand_max);
	return r < rand_max / bias;
}

int unbiased(int bias)
{
	int a;
	while ((a = biased(bias)) == biased(bias));
	return a;
}

int main()
{
	int b, n = 10000, cb, cu, i;
	for (b = 3; b <= 6; b++) {
		for (i = cb = cu = 0; i < n; i++) {
			cb += biased(b);
			cu += unbiased(b);
		}
		printf("bias %d: %5.3f%% vs %5.3f%%\n", b,
			100. * cb / n, 100. * cu / n);
	}

	return 0;
}
```

output

```txt
bias 3: 33.090% vs 49.710%
bias 4: 25.130% vs 49.430%
bias 5: 19.760% vs 49.650%
bias 6: 16.740% vs 50.030%
```



## C sharp



```c sharp
using System;

namespace Unbias
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            // Demonstrate.
            for (int n = 3; n <= 6; n++)
            {
                int biasedZero = 0, biasedOne = 0, unbiasedZero = 0, unbiasedOne = 0;
                for (int i = 0; i < 100000; i++)
                {
                    if (randN(n))
                        biasedOne++;
                    else
                        biasedZero++;
                    if (Unbiased(n))
                        unbiasedOne++;
                    else
                        unbiasedZero++;
                }

                Console.WriteLine("(N = {0}):".PadRight(17) + "# of 0\t# of 1\t% of 0\t% of 1", n);
                Console.WriteLine("Biased:".PadRight(15) + "{0}\t{1}\t{2}\t{3}",
                                  biasedZero, biasedOne,
                                  biasedZero/1000, biasedOne/1000);
                Console.WriteLine("Unbiased:".PadRight(15) + "{0}\t{1}\t{2}\t{3}",
                                  unbiasedZero, unbiasedOne,
                                  unbiasedZero/1000, unbiasedOne/1000);
            }
        }

        private static bool Unbiased(int n)
        {
            bool flip1, flip2;

            /* Flip twice, and check if the values are the same.
             * If so, flip again. Otherwise, return the value of the first flip. */

            do
            {
                flip1 = randN(n);
                flip2 = randN(n);
            } while (flip1 == flip2);

            return flip1;
        }

        private static readonly Random random = new Random();

        private static bool randN(int n)
        {
            // Has an 1/n chance of returning 1. Otherwise it returns 0.
            return random.Next(0, n) == 0;
        }
    }
}
```


'''Sample Output'''


```txt
(N = 3):       # of 0   # of 1  % of 0  % of 1
Biased:        66867    33133   66      33
Unbiased:      49843    50157   49      50
(N = 4):       # of 0   # of 1  % of 0  % of 1
Biased:        74942    25058   74      25
Unbiased:      50192    49808   50      49
(N = 5):       # of 0   # of 1  % of 0  % of 1
Biased:        80203    19797   80      19
Unbiased:      49928    50072   49      50
(N = 6):       # of 0   # of 1  % of 0  % of 1
Biased:        83205    16795   83      16
Unbiased:      49744    50256   49      50

```



## Clojure


```Clojure
(defn biased [n]
  (if (< (rand 2) (/ n)) 0 1))

(defn unbiased [n]
  (loop [a 0 b 0]
    (if (= a b)
      (recur (biased n) (biased n))
      a)))

(for [n (range 3 7)]
  [n
   (double (/ (apply + (take 50000 (repeatedly #(biased n)))) 50000))
   (double (/ (apply + (take 50000 (repeatedly #(unbiased n)))) 50000))])
([3 0.83292 0.50422]
 [4 0.87684 0.5023]
 [5 0.90122 0.49728]
 [6 0.91526 0.5])
```



## CoffeeScript


```coffeescript

biased_rand_function = (n) ->
  # return a function that returns 0/1  with
  # 1 appearing only 1/Nth of the time
  cap = 1/n
  ->
    if Math.random() < cap 
      1
    else
      0
    
unbiased_function = (f) ->
  ->
    while true
      [n1, n2] = [f(), f()]
      return n1 if n1 + n2 == 1

stats = (label, f) ->
  cnt = 0
  sample_size = 10000000
  for i in [1...sample_size]
    cnt += 1 if f() == 1
  console.log "ratio of 1s: #{cnt / sample_size} [#{label}]"
  
for n in [3..6]
  console.log "\n---------- n = #{n}"
  f_biased = biased_rand_function(n)
  f_unbiased = unbiased_function f_biased
  stats "biased", f_biased
  stats "unbiased", f_unbiased

```

output

```txt

> coffee unbiased.coffee 

---------- n = 3
ratio of 1s: 0.3333343 [biased]
ratio of 1s: 0.4999514 [unbiased]

---------- n = 4
ratio of 1s: 0.2499751 [biased]
ratio of 1s: 0.4998067 [unbiased]

---------- n = 5
ratio of 1s: 0.199729 [biased]
ratio of 1s: 0.5003183 [unbiased]

---------- n = 6
ratio of 1s: 0.1664843 [biased]
ratio of 1s: 0.4997813 [unbiased]

```



## Common Lisp


```lisp
(defun biased (n) (if (zerop (random n)) 0 1))

(defun unbiased (n)
    (loop with x do
      (if (/= (setf x (biased n)) (biased n))
	    (return x))))

(loop for n from 3 to 6 do
      (let ((u (loop repeat 10000 collect (unbiased n)))
	    (b (loop repeat 10000 collect (biased n))))
	(format t "~a: unbiased ~d biased ~d~%" n (count 0 u) (count 0 b))))
```

output

```txt
3: unbiased 4992 biased 3361
4: unbiased 4988 biased 2472
5: unbiased 5019 biased 1987
6: unbiased 4913 biased 1658
```



## D


```d
import std.stdio, std.random, std.algorithm, std.range, std.functional;

enum biased = (in int n) /*nothrow*/ => uniform01 < (1.0 / n);

int unbiased(in int bias) /*nothrow*/ {
    int a;
    while ((a = bias.biased) == bias.biased) {}
    return a;
}

void main() {
    enum M = 500_000;
    foreach (immutable n; 3 .. 7)
        writefln("%d: %2.3f%%  %2.3f%%", n,
                 M.iota.map!(_=> n.biased).sum * 100.0 / M,
                 M.iota.map!(_=> n.unbiased).sum * 100.0 / M);
}
```

{{out}}

```txt
3: 33.441%  49.964%
4: 24.953%  49.910%
5: 19.958%  49.987%
6: 16.660%  49.890%
```


## Elena

{{trans|C#}}
ELENA 4.x :

```elena
import extensions;
 
extension op : IntNumber
{
    bool randN()
        = randomGenerator.nextInt(self) == 0;
 
    get bool Unbiased()
    {
        bool flip1 := self.randN();
        bool flip2 := self.randN();
 
        while (flip1 == flip2)
        {
            flip1 := self.randN();
            flip2 := self.randN()
        };
 
        ^ flip1
    }
}
 
public program()
{
    for(int n := 3, n <= 6, n += 1)
    {
        int biasedZero := 0;
        int biasedOne := 0;
        int unbiasedZero := 0;
        int unbiasedOne := 0;
 
        for(int i := 0, i < 100000, i += 1)
        {
            if(n.randN()) { biasedOne += 1 } else { biasedZero += 1 };
            if(n.Unbiased) { unbiasedOne += 1 } else { unbiasedZero += 1 }
        };
 
        console
            .printLineFormatted("(N = {0}):".padRight(17) + "# of 0"$9"# of 1"$9"% of 0"$9"% of 1", n)
            .printLineFormatted("Biased:".padRight(15) + "{0}"$9"{1}"$9"{2}"$9"{3}", 
                                    biasedZero, biasedOne, biasedZero / 1000, biasedOne / 1000)
            .printLineFormatted("Unbiased:".padRight(15) + "{0}"$9"{1}"$9"{2}"$9"{3}", 
                                    unbiasedZero, unbiasedOne, unbiasedZero / 1000, unbiasedOne / 1000)
    }
}
```

{{out}}

```txt

(N = 3):       # of 0	# of 1	% of 0	% of 1
Biased:        66793	33207	66	33
Unbiased:      49965	50035	49	50
(N = 4):       # of 0	# of 1	% of 0	% of 1
Biased:        75233	24767	75	24
Unbiased:      50106	49894	50	49
(N = 5):       # of 0	# of 1	% of 0	% of 1
Biased:        80209	19791	80	19
Unbiased:      50080	49920	50	49
(N = 6):       # of 0	# of 1	% of 0	% of 1
Biased:        83349	16651	83	16
Unbiased:      49699	50301	49	50

```



## Elixir


```elixir
defmodule Random do
  def randN(n) do
    if :rand.uniform(n) == 1, do: 1, else: 0
  end 
  def unbiased(n) do
    {x, y} = {randN(n), randN(n)}
    if x != y, do: x, else: unbiased(n)
  end 
end
 
IO.puts "N  biased  unbiased"
m = 10000
for n <- 3..6 do
  xs = for _ <- 1..m, do: Random.randN(n)
  ys = for _ <- 1..m, do: Random.unbiased(n)
  IO.puts "#{n}  #{Enum.sum(xs) / m}  #{Enum.sum(ys) / m}"
end
```


{{out}}

```txt

N  biased  unbiased
3  0.3356  0.5043
4  0.2523  0.4996
5  0.2027  0.5041
6  0.1647  0.4912

```



## ERRE


```ERRE
PROGRAM UNBIAS

FUNCTION RANDN(N)
   RANDN=INT(1+N*RND(1))=1
END FUNCTION

PROCEDURE UNBIASED(N->RIS)
      LOCAL A,B
      REPEAT
        A=RANDN(N)
        B=RANDN(N)
      UNTIL A<>B
      RIS=A
END PROCEDURE

BEGIN
  PRINT(CHR$(12);) ! CLS
  RANDOMIZE(TIMER)

  FOR N=3 TO 6 DO
        BIASED=0
        UNBIASED=0
        FOR I=1 TO 10000 DO
          IF RANDN(N) THEN biased+=1
          UNBIASED(N->RIS)
          IF RIS THEN unbiased+=+1
        END FOR
        PRINT("N =";N;" : biased =";biased/100;", unbiased =";unbiased/100)
  END FOR
END PROGRAM

```

{{out}}

```txt
N = 3  : biased = 32.66 , unbiased = 49.14
N = 4  : biased = 25.49 , unbiased = 49.92
N = 5  : biased = 20.53 , unbiased = 50
N = 6  : biased = 17.43 , unbiased = 50.43

```



## Euphoria


```euphoria
function randN(integer N)
    return rand(N) = 1
end function

function unbiased(integer N)
    integer a
    while 1 do
        a = randN(N)
        if a != randN(N) then
            return a
        end if
    end while
end function

constant n = 10000
integer cb, cu
for b = 3 to 6 do
    cb = 0
    cu = 0
    for i = 1 to n do
        cb += randN(b)
        cu += unbiased(b)
    end for
    printf(1, "%d: %5.2f%%  %5.2f%%\n", {b, 100 * cb / n, 100 * cu / n})
end for
```


Output:

```txt
3: 33.68%  49.94%
4: 24.93%  50.48%
5: 20.32%  49.97%
6: 16.98%  50.05%

```


=={{header|F_Sharp|F#}}==

```fsharp
open System

let random = Random()

let randN = random.Next >> (=)0 >> Convert.ToInt32

let rec unbiased n =
    let a = randN n
    if a <> randN n then a else unbiased n

[<EntryPoint>]
let main argv =
    let n = if argv.Length > 0 then UInt32.Parse(argv.[0]) |> int else 100000
    for b = 3 to 6 do
        let cb = ref 0
        let cu = ref 0
        for i = 1 to n do
            cb := !cb + randN b
            cu := !cu + unbiased b
        printfn "%d: %5.2f%%  %5.2f%%"
            b (100. * float !cb / float n) (100. * float !cu / float n)
    0
```

{{out}}

```txt
3: 33.26%  49.97%
4: 25.02%  50.22%
5: 19.98%  50.00%
6: 16.64%  49.69%
```



## Factor


```factor
USING: formatting kernel math math.ranges random sequences ;
IN: rosetta-code.unbias

: randN ( n -- m ) random zero? 1 0 ? ;

: unbiased ( n -- m )
    dup [ randN ] dup bi 2dup = not
    [ drop nip ] [ 2drop unbiased ] if ;

: test-generator ( quot -- x )
    [ 1,000,000 dup ] dip replicate sum 100 * swap / ; inline

: main ( -- )
    3 6 [a,b] [
        dup [ randN ] [ unbiased ] bi-curry
        [ test-generator ] bi@ "%d: %.2f%%  %.2f%%\n" printf
    ] each ;

MAIN: main
```

{{out}}

```txt

3: 33.25%  50.03%
4: 24.98%  50.02%
5: 20.03%  50.04%
6: 16.66%  49.99%

```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program Bias_Unbias
  implicit none

  integer, parameter :: samples = 1000000
  integer :: i, j
  integer :: c1, c2, rand
 
  do i = 3, 6
    c1 = 0
    c2 = 0
    do j = 1, samples
      rand = bias(i)
      if (rand == 1) c1 = c1 + 1
      rand = unbias(i)
      if (rand == 1) c2 = c2 + 1
    end do
    write(*, "(i2,a,f8.3,a,f8.3,a)") i, ":", real(c1) * 100.0 / real(samples), &
                                     "%", real(c2) * 100.0 / real(samples), "%"
  end do
 
contains

function bias(n)
  integer :: bias
  integer, intent(in) :: n
  real :: r

  call random_number(r)
  if (r > 1 / real(n)) then
    bias = 0
  else
    bias = 1
  end if
end function

function unbias(n)
  integer :: unbias
  integer, intent(in) :: n
  integer :: a, b

  do
    a = bias(n)
    b = bias(n)
    if (a /= b) exit
  end do
  unbias = a     
end function

end program
```

Output:

```txt
3:  33.337%  49.971%
4:  24.945%  49.944%
5:  19.971%  49.987%
6:  16.688%  50.097%
```



## GAP


```gap
RandNGen := function(n)
	local v, rand;
	v := [1 .. n - 1]*0;
	Add(v, 1);
	rand := function()
		return Random(v);
	end;
	return rand;
end;

UnbiasedGen := function(rand)
	local unbiased;
	unbiased := function()
		local a, b;
		while true do
			a := rand();
			b := rand();
			if a <> b then
				break;
			fi;
		od;
		return a;
	end;
	return unbiased;
end;

range := [2 .. 6];
v := List(range, RandNGen);
w := List(v, UnbiasedGen);
apply := gen -> Sum([1 .. 1000000], n -> gen());

# Some tests (2 is added as a witness, since in this case RandN is already unbiased)
PrintArray(TransposedMat([range, List(v, apply), List(w, apply)]));
# [ [       2,  499991,  499041 ],
#   [       3,  333310,  500044 ],
#   [       4,  249851,  500663 ],
#   [       5,  200532,  500448 ],
#   [       6,  166746,  499859 ] ]
```


## Go


```go
package main

import (
    "fmt"
    "math/rand"
)

const samples = 1e6

func main() {
    fmt.Println("Generator  1 count  0 count  % 1 count")
    for n := 3; n <= 6; n++ {
        // function randN, per task description
        randN := func() int {
            if rand.Intn(n) == 0 {
                return 1
            }
            return 0
        }
        var b [2]int
        for x := 0; x < samples; x++ {
            b[randN()]++
        }
        fmt.Printf("randN(%d)   %7d  %7d    %5.2f%%\n",
            n, b[1], b[0], float64(b[1])*100/samples)

        // function unbiased, per task description
        unbiased := func() (b int) {
            for b = randN(); b == randN(); b = randN() {
            }
            return
        }
        var u [2]int
        for x := 0; x < samples; x++ {
            u[unbiased()]++
        }
        fmt.Printf("unbiased   %7d  %7d    %5.2f%%\n",
            u[1], u[0], float64(u[1])*100/samples)
    }
}
```

Output:

```txt

Generator  1 count  0 count  % 1 count
randN(3)    332711   667289    33.27%
unbiased    499649   500351    49.96%
randN(4)    249742   750258    24.97%
unbiased    499434   500566    49.94%
randN(5)    200318   799682    20.03%
unbiased    499100   500900    49.91%
randN(6)    166900   833100    16.69%
unbiased    499973   500027    50.00%

```



## Haskell

The first task:

```haskell
import Control.Monad.Random
import Control.Monad
import Text.Printf

randN :: MonadRandom m => Int -> m Int
randN n = fromList [(0, fromIntegral n-1), (1, 1)]
```


Examples of use:

```txt
λ> replicateM 20 (randN 2)
[0,0,1,0,0,1,0,1,1,0,0,1,1,1,1,0,1,1,0,0]
λ> replicateM 20 (randN 5)
[0,1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,1,0]
```


The second task. Returns the unbiased generator for any given random generator.

```Haskell
unbiased :: (MonadRandom m, Eq x) => m x -> m x
unbiased g = do x <- g
                y <- g
                if x /= y then return y else unbiased g
```


Examples of use:

```txt
λ> replicateM 20 (unbiased (randN 5))
[0,0,1,0,1,1,1,0,0,0,1,1,1,0,1,1,0,0,1,0]
λ> replicateM 20 (unbiased (fromList [(True,10),(False,1)]))
[True,True,False,True,True,True,False,True,False,True,True,False,False,True,False,True,True,False,False,True]
```


The third task:

```Haskell
main = forM_ [3..6] showCounts
  where
    showCounts b = do
      r1 <- counts (randN b)
      r2 <- counts (unbiased (randN b))
      printf "n = %d  biased: %d%%  unbiased: %d%%\n" b r1 r2
        
    counts g = (`div` 100) . length . filter (== 1) <$> replicateM 10000 g
```


Output:


```txt

n = 3  biased: 33%  unbiased: 49%
n = 4  biased: 24%  unbiased: 50%
n = 5  biased: 19%  unbiased: 50%
n = 6  biased: 16%  unbiased: 49%

```


=={{header|Icon}} and {{header|Unicon}}==
This solution works in both languages.  Both <tt>randN</tt> and
<tt>unbiased</tt> are generators in the Icon/Unicon sense.

```unicon
procedure main(A)
    iters := \A[1] | 10000
    write("ratios of 0 to 1 from ",iters," trials:")
    every n := 3 to 6 do {
        results_randN := table(0)
        results_unbiased := table(0)
        every 1 to iters do {
            results_randN[randN(n)] +:= 1
            results_unbiased[unbiased(n)] +:= 1
            }
        showResults(n, "randN", results_randN)
        showResults(n, "unbiased", results_unbiased)
        }
end

procedure showResults(n, s, t)
    write(n," ",left(s,9),":",t[0],"/",t[1]," = ",t[0]/real(t[1]))
end

procedure unbiased(n)
    repeat {
        n1 := randN(n)
        n2 := randN(n)
        if n1 ~= n2 then suspend n1
        }
end

procedure randN(n)
    repeat suspend if 1 = ?n then 1 else 0
end
```

and a sample run:

```txt
->ubrn 100000
ratios of 0 to 1 from 100000 trials:
3 randN    :66804/33196 = 2.012411133871551
3 unbiased :49812/50188 = 0.9925081692834941
4 randN    :75017/24983 = 3.002721850858584
4 unbiased :50000/50000 = 1.0
5 randN    :79990/20010 = 3.997501249375312
5 unbiased :50073/49927 = 1.002924269433373
6 randN    :83305/16695 = 4.989817310572027
6 unbiased :49911/50089 = 0.9964463255405378
->
```



## J


```j
randN=: 0 = ?
unbiased=: i.@# { ::$: 2 | 0 3 -.~ _2 #.\ 4&* randN@# ]
```


Example use:


```j
   randN 10#6
1 0 0 0 1 0 0 0 0 0
   unbiased 10#6
1 0 0 1 0 0 1 0 1 1
```


Some example counts (these are counts of the number of 1s which appear in a test involving 100 random numbers):


```j
   +/randN 100#3
30
   +/randN 100#4
20
   +/randN 100#5
18
   +/randN 100#6
18
   +/unbiased 100#3
49
   +/unbiased 100#4
46
   +/unbiased 100#5
49
   +/unbiased 100#6
47
```
 

Note that these results are random.  For example, a re-run of <code>+/randN 100#5</code> gave 25 as its result, and a re-run of <code>+/unbiased 100#5</code> gave 52 as its result.


## Java


```java
public class Bias {
    public static boolean biased(int n) {
        return Math.random() < 1.0 / n;
    }

    public static boolean unbiased(int n) {
        boolean a, b;
        do {
            a = biased(n);
            b = biased(n);
        } while (a == b);
        return a;
    }

    public static void main(String[] args) {
        final int M = 50000;
        for (int n = 3; n < 7; n++) {
            int c1 = 0, c2 = 0;
            for (int i = 0; i < M; i++) {
                c1 += biased(n) ? 1 : 0;
                c2 += unbiased(n) ? 1 : 0;
            }
            System.out.format("%d: %2.2f%%  %2.2f%%\n",
                              n, 100.0*c1/M, 100.0*c2/M);
        }
    }
}
```

Output:

```txt
3: 33,11%  50,23%
4: 24.97%  49.78%
5: 20.05%  50.00%
6: 17.00%  49.88%
```


jhead
## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

fun biased(n: Int) = Math.random() < 1.0 / n

fun unbiased(n: Int): Boolean {
    var a: Boolean
    var b: Boolean
    do {
        a = biased(n)  
        b = biased(n)
    }
    while (a == b)
    return a
}

fun main(args: Array<String>) {
    val m = 50_000
    val f = "%d: %2.2f%%  %2.2f%%"
    for (n in 3..6) {
        var c1 = 0
        var c2 = 0 
        for (i in 0 until m) {
            if (biased(n)) c1++
            if (unbiased(n)) c2++
        }
        println(f.format(n, 100.0 * c1 / m, 100.0 * c2 / m))
    }
}
```


Sample output:

```txt

3: 33.19%  50.19%
4: 25.29%  49.85%
5: 19.91%  50.07%
6: 16.71%  50.14%

```



## Julia

{{works with|Julia|0.6}}


```julia
randN(N) = () -> rand(1:N) == 1 ? 1 : 0
function unbiased(biased::Function)
    this, that = biased(), biased()
    while this == that this, that = biased(), biased() end
    return this
end

@printf "%2s | %10s | %5s | %5s | %8s" "N" "bias./unb." "1s" "0s" "pct ratio"
const nrep = 10000
for N in 3:6
    biased = randN(N)

    v = collect(biased() for __ in 1:nrep)
    v1, v0 = count(v .== 1), count(v .== 0)
    @printf("%2i | %10s | %5i | %5i | %5.2f%%\n", N, "biased", v1, v0, 100 * v1 / nrep)

    v = collect(unbiased(biased) for __ in 1:nrep)
    v1, v0 = count(v .== 1), count(v .== 0)
    @printf("%2i | %10s | %5i | %5i | %5.2f%%\n", N, "unbiased", v1, v0, 100 * v1 / nrep)
end
```


{{out}}

```txt
 N | bias./unb. |    1s |    0s | pct ratio
 3 |     biased |  3286 |  6714 | 32.86%
 3 |   unbiased |  4986 |  5014 | 49.86%
 4 |     biased |  2473 |  7527 | 24.73%
 4 |   unbiased |  4986 |  5014 | 49.86%
 5 |     biased |  1992 |  8008 | 19.92%
 5 |   unbiased |  5121 |  4879 | 51.21%
 6 |     biased |  1663 |  8337 | 16.63%
 6 |   unbiased |  5040 |  4960 | 50.40%
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

fun biased(n: Int) = Math.random() < 1.0 / n

fun unbiased(n: Int): Boolean {
    var a: Boolean
    var b: Boolean
    do {
        a = biased(n)  
        b = biased(n)
    }
    while (a == b)
    return a
}

fun main(args: Array<String>) {
    val m = 50_000
    val f = "%d: %2.2f%%  %2.2f%%"
    for (n in 3..6) {
        var c1 = 0
        var c2 = 0 
        for (i in 0 until m) {
            if (biased(n)) c1++
            if (unbiased(n)) c2++
        }
        println(f.format(n, 100.0 * c1 / m, 100.0 * c2 / m))
    }
}
```


Sample output:

```txt

3: 33.19%  50.19%
4: 25.29%  49.85%
5: 19.91%  50.07%
6: 16.71%  50.14%

```



## Lua


```lua

local function randN(n)
  return function()
    if math.random() < 1/n then return 1 else return 0 end
  end
end

local function unbiased(n)
  local biased = randN (n)
  return function()
    local a, b = biased(), biased()
    while a==b do
      a, b = biased(), biased()
    end
    return a
  end
end

local function demonstrate (samples)
  for n = 3, 6 do
    biased = randN(n)
    unbias = unbiased(n)
    local bcounts = {[0]=0,[1]=0}
    local ucounts = {[0]=0,[1]=0}
    for i=1, samples do
      local bnum = biased()
      local unum = unbias()
      bcounts[bnum] = bcounts[bnum]+1
      ucounts[unum] = ucounts[unum]+1
    end
    print(string.format("N = %d",n),
      "# 0", "# 1",
      "% 0", "% 1")
    print("biased", bcounts[0], bcounts[1],
      bcounts[0] / samples * 100,
      bcounts[1] / samples * 100)
    print("unbias", ucounts[0], ucounts[1],
      ucounts[0] / samples * 100,
      ucounts[1] / samples * 100)
  end
end

demonstrate(100000)

```


Output:

```txt

N = 3	# 0	# 1	% 0	% 1
biased	66832	33168	66.832	33.168
unbias	50207	49793	50.207	49.793
N = 4	# 0	# 1	% 0	% 1
biased	75098	24902	75.098	24.902
unbias	49872	50128	49.872	50.128
N = 5	# 0	# 1	% 0	% 1
biased	80142	19858	80.142	19.858
unbias	50049	49951	50.049	49.951
N = 6	# 0	# 1	% 0	% 1
biased	83407	16593	83.407	16.593
unbias	49820	50180	49.82	50.18

```



## Mathematica


```Mathematica
rand[bias_, n_] := 1 - Unitize@RandomInteger[bias - 1, n]

unbiased[bias_, n_] := 
 DeleteCases[rand[bias, {n, 2}], {a_, a_}][[All, 1]]
```



```txt
count = 1000000;
TableForm[
 Table[{n, Total[rand[n, count]]/count // N, 
   Total[#]/Length[#] &@unbiased[n, count] // N}, {n, 3, 6}], 
 TableHeadings -> {None, {n, "biased", "unbiased"}}]

n	biased	unbiased
3	0.33312	0.500074
4	0.24932	0.499883
5	0.1998 	0.498421
6	0.16620	0.49805


```



## NetRexx

{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method biased(n = int) public static returns boolean
  return Math.random() < 1.0 / n

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method unbiased(n = int) public static returns boolean
  a = boolean
  b = boolean
  loop until a \= b
    a = biased(n)
    b = biased(n)
    end
  return a

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg Mx .
  if Mx.length <= 0 then Mx = 50000
  M = int Mx
  loop n = int 3 to 6
    c1 = int 0
    c2 = int 0
    loop for M
      if biased(n)   then c1 = c1 + 1
      if unbiased(n) then c2 = c2 + 1
      end
      say Rexx(n).right(3)':' Rexx(100.0 * c1 / M).format(6, 2)'%' Rexx(100.0 * c2 / M).format(6, 2)'%'
    end n
  return

```

'''Output:'''

```txt

  3:     32.78%     49.98%
  4:     24.72%     50.31%
  5:     19.95%     50.34%
  6:     17.20%     50.20%

```



## Nim

{{trans|Python}}

```nim
import math, strutils
randomize()

template newSeqWith(len: int, init: expr): expr =
  var result {.gensym.} = newSeq[type(init)](len)
  for i in 0..<len:
    result[i] = init

proc randN(n): (proc: range[0..1]) =
  proc: range[0..1] = ord(random(n) == 0)

proc unbiased(biased): range[0..1] =
  result = biased()
  var that = biased()
  while result == that:
    result = biased()
    that = biased()

for n in 3..6:
  var biased = randN(n)
  var v = newSeqWith(1_000_000, biased())
  var cnt0, cnt1 = 0
  for x in v:
    if x == 0: inc cnt0
    else:      inc cnt1
  echo "Biased(",n,")  = count1=",cnt1,", count0=",cnt0,", percent=",
       formatFloat(100 * float(cnt1)/float(cnt1+cnt0), ffDecimal, 3)

  v = newSeqWith(1_000_000, unbiased(biased))
  cnt0 = 0
  cnt1 = 0
  for x in v:
    if x == 0: inc cnt0
    else:      inc cnt1
  echo "  Unbiased = count1=",cnt1,", count0=",cnt0,", percent=",
       formatFloat(100 * float(cnt1)/float(cnt1+cnt0), ffDecimal, 3)
```

Output:

```txt
Biased(3)  = count1=332805, count0=667195, percent=33.281
  Unbiased = count1=500157, count0=499843, percent=50.016
Biased(4)  = count1=249575, count0=750425, percent=24.957
  Unbiased = count1=500072, count0=499928, percent=50.007
Biased(5)  = count1=199537, count0=800463, percent=19.954
  Unbiased = count1=499396, count0=500604, percent=49.940
Biased(6)  = count1=166728, count0=833272, percent=16.673
  Unbiased = count1=499712, count0=500288, percent=49.971
```



## OCaml



```ocaml
let randN n =
  if Random.int n = 0 then 1 else 0

let rec unbiased n =
  let a = randN n in
  if a <> randN n then a else unbiased n

let () =
  Random.self_init();
  let n = 50_000 in
  for b = 3 to 6 do
    let cb = ref 0 in
    let cu = ref 0 in
    for i = 1 to n do
      cb := !cb + (randN b);
      cu := !cu + (unbiased b);
    done;
    Printf.printf "%d: %5.2f%%  %5.2f%%\n"
      b (100.0 *. float !cb /. float n) (100.0 *. float !cu /. float n)
  done
```


Output:


```txt
3: 33.07%  49.90%
4: 25.11%  49.85%
5: 19.82%  50.09%
6: 16.51%  50.51%
```



## PARI/GP

GP's random number generation is high-quality, using Brent's [http://maths.anu.edu.au/~brent/random.html XORGEN]. Thus this program is slow: the required 400,000 unbiased numbers generated through this bias/unbias scheme take nearly a second. This requires about two million calls to <code>random</code>, which in turn generate a total of about three million calls to the underlying random number generator through the rejection strategy. The overall efficiency of the scheme is 0.8% for 32-bit and 0.4% for 64-bit...

```parigp
randN(N)=!random(N);
unbiased(N)={
  my(a,b);
  while(1,
    a=randN(N);
    b=randN(N);
    if(a!=b, return(a))
  )
};
for(n=3,6,print(n"\t"sum(k=1,1e5,unbiased(n))"\t"sum(k=1,1e5,randN(n))))
```


Output:

```txt
3	49997	33540
4	49988	24714
5	50143	20057
6	49913	16770
```



## Perl


```perl
sub randn {
        my $n = shift;
        return int(rand($n) / ($n - 1));
}
 
for my $n (3 .. 6) {
        print "Bias $n: ";
        my (@raw, @fixed);
        for (1 .. 10000) {
                my $x = randn($n);
                $raw[$x]++;
                $fixed[$x]++    if randn($n) != $x
        }
        print "@raw, ";
        printf("%3g+-%.3g%%\tfixed: ", $raw[0]/100,
		100 * sqrt($raw[0] * $raw[1]) / ($raw[0] + $raw[1])**1.5);
        print "@fixed, ";
        printf("%3g+-%.3g%%\n", 100*$fixed[0]/($fixed[0] + $fixed[1]),
		100 * sqrt($fixed[0] * $fixed[1]) / ($fixed[0] + $fixed[1])**1.5);

}
```

Output:

```txt
Bias 3: 6684 3316, 66.84+-0.471%        fixed: 2188 2228, 49.5471+-0.752%
Bias 4: 7537 2463, 75.37+-0.431%        fixed: 1924 1845, 51.048+-0.814%
Bias 5: 7993 2007, 79.93+-0.401%        fixed: 1564 1597, 49.478+-0.889%
Bias 6: 8309 1691, 83.09+-0.375%        fixed: 1403 1410, 49.8756+-0.943%
```



## Perl 6

{{trans|Perl}}

```perl6
sub randN ( $n where 3..6 ) {
    return ( $n.rand / ($n - 1) ).Int;
}

sub unbiased ( $n where 3..6 ) {
    my $n1;
    repeat { $n1 = randN($n) } until $n1 != randN($n);
    return $n1;
}

my $iterations = 1000;
for 3 .. 6 -> $n {
    my ( @raw, @fixed );
    for ^$iterations {
        @raw[      randN($n) ]++;
        @fixed[ unbiased($n) ]++;
    }
    printf "N=%d   randN: %s, %4.1f%%   unbiased: %s, %4.1f%%\n",
        $n, map { .perl, .[1] * 100 / $iterations }, @raw, @fixed;
}
```


Output:
```txt
N=3   randN: [676, 324], 32.4%   unbiased: [517, 483], 48.3%
N=4   randN: [734, 266], 26.6%   unbiased: [489, 511], 51.1%
N=5   randN: [792, 208], 20.8%   unbiased: [494, 506], 50.6%
N=6   randN: [834, 166], 16.6%   unbiased: [514, 486], 48.6%
```



## Phix

Copy of [[Unbias_a_random_generator#Euphoria|Euphoria]]

```Phix
function randN(integer N)
    return rand(N) = 1
end function
 
function unbiased(integer N)
integer a
    while 1 do
        a = randN(N)
        if a!=randN(N) then
            return a
        end if
    end while
end function
 
constant n = 10000
integer cb, cu
for b=3 to 6 do
    cb = 0
    cu = 0
    for i=1 to n do
        cb += randN(b)
        cu += unbiased(b)
    end for
    printf(1, "%d: %5.2f%%  %5.2f%%\n", {b, 100 * cb / n, 100 * cu / n})
end for
```

{{out}}

```txt

3: 32.83%  50.34%
4: 24.78%  50.01%
5: 20.21%  49.71%
6: 16.68%  49.67%

```



## PicoLisp


```PicoLisp
(de randN (N)
   (if (= 1 (rand 1 N)) 1 0) )

(de unbiased (N)
   (use (A B)
      (while
         (=
            (setq A (randN N))
            (setq B (randN N)) ) )
      A ) )
```

Test:

```PicoLisp
(for N (range 3 6)
   (tab (2 1 7 2 7 2)
      N ":"
      (format
         (let S 0 (do 10000 (inc 'S (randN N))))
         2 )
      "%"
      (format
         (let S 0 (do 10000 (inc 'S (unbiased N))))
         2 )
      "%" ) )
```

Output:

```txt
 3:  33.21 %  50.48 %
 4:  25.06 %  49.79 %
 5:  20.04 %  49.75 %
 6:  16.32 %  49.02 %
```



## PL/I


```PL/I

test: procedure options (main); /* 20 Nov. 2012 */

randN: procedure(N) returns (bit (1));
   declare N fixed (1);
   declare random builtin;
   declare r fixed (2) external initial (-1);
   if r >= 0 then do; r = r-1; return ('0'b); end;
   r = random()*2*N;
   return ('1'b);
end randN;

random: procedure returns (bit(1));
   declare (r1, r2) bit (1);
   do until (r1 ^= r2);
      r1 = randN(N); r2 = randN(N);
   end;
   return (r1);
end random;

   declare (biasedrn, unbiasedrn) (100) bit (1);
   declare N fixed (1);

   put ('N     Biased  Unbiased (tally of 100 random numbers)');
   do N = 3 to 6;
      biasedrn = randN(N); unbiasedrn = random;
      put skip edit (N, sum(biasedrn), sum(unbiasedrn)) (F(1), 2 F(10));
   end;

end test;

```

Results:

```txt

N     Biased  Unbiased (tally of 100 random numbers) 
3        24        42
4        18        47
5        16        41
6        11        53

```



## PowerShell

{{works with|PowerShell|2}}

```PowerShell

function randN ( [int]$N )
    {
    [int]( ( Get-Random -Maximum $N ) -eq 0 )
    }
 
function unbiased ( [int]$N )
    {
    do  {
        $X = randN $N
        $Y = randN $N
        }
    While ( $X -eq $Y )
 
    return $X
    }

```

Note: The [pscustomobject] type accelerator, used to simplify making the test output look pretty, requires version 3.0 or higher.

```PowerShell

$Tests = 1000
ForEach ( $N in 3..6 )
    {
    $Biased   = 0
    $Unbiased = 0
 
    ForEach ( $Test in 1..$Tests )
        {
        $Biased   += randN $N
        $Unbiased += unbiased $N
        }
    [pscustomobject]@{ N = $N
                       "Biased Ones out of $Test" = $Biased
                       "Unbiased Ones out of $Test" = $Unbiased }
    }

```

{{out}}

```txt

N Biased Ones out of 1000 Unbiased Ones out of 1000
- ----------------------- -------------------------
3                     322                       503
4                     273                       518
5                     217                       515
6                     173                       486

```



## PureBasic


```PureBasic
Procedure biased(n)
  If Random(n) <> 1
    ProcedureReturn 0
  EndIf 
  ProcedureReturn 1
EndProcedure

Procedure unbiased(n)
  Protected a, b
  Repeat
    a = biased(n)
    b = biased(n)
  Until a <> b
  ProcedureReturn a
EndProcedure

#count = 100000

Define n, m, output.s
For n = 3 To 6
  Dim b_count(1)
  Dim u_count(1)
  For m = 1 To #count
    x = biased(n)
    b_count(x) + 1
    x = unbiased(n)
    u_count(x) + 1
  Next
  output + "N = " + Str(n) + #LF$
  output + "  biased =>" + #tab$ + "#0=" + Str(b_count(0)) + #tab$ + "#1=" +Str(b_count(1))
  output + #tab$ + " ratio=" + StrF(b_count(1) / #count * 100, 2) + "%" + #LF$
  output + "  unbiased =>" + #tab$ + "#0=" + Str(u_count(0)) + #tab$ + "#1=" + Str(u_count(1))
  output + #tab$ + " ratio=" + StrF(u_count(1) / #count * 100, 2) + "%" + #LF$
Next
MessageRequester("Biased and Unbiased random number results", output)
```

Sample output:

```txt
---------------------------
Biased and Unbiased random number results
---------------------------
N = 3
  biased =>	#0=74856	#1=25144	 ratio=25.14%
  unbiased =>	#0=50066	#1=49934	 ratio=49.93%
N = 4
  biased =>	#0=80003	#1=19997	 ratio=20.00%
  unbiased =>	#0=49819	#1=50181	 ratio=50.18%
N = 5
  biased =>	#0=83256	#1=16744	 ratio=16.74%
  unbiased =>	#0=50268	#1=49732	 ratio=49.73%
N = 6
  biased =>	#0=85853	#1=14147	 ratio=14.15%
  unbiased =>	#0=49967	#1=50033	 ratio=50.03%
```



## Python


```python
from __future__ import print_function
import random

def randN(N):
    " 1,0 random generator factory with 1 appearing 1/N'th of the time"
    return lambda: random.randrange(N) == 0

def unbiased(biased):
    'uses a biased() generator of 1 or 0, to create an unbiased one'
    this, that = biased(), biased()
    while this == that: # Loop until 10 or 01
        this, that = biased(), biased()
    return this         # return the first

if __name__ == '__main__':
    from collections import namedtuple

    Stats = namedtuple('Stats', 'count1 count0 percent')

    for N in range(3, 7):
        biased = randN(N)
        v = [biased() for x in range(1000000)]
        v1, v0 = v.count(1), v.count(0)
        print ( "Biased(%i)  = %r" % (N, Stats(v1, v0, 100. * v1/(v1 + v0))) )

        v = [unbiased(biased) for x in range(1000000)]
        v1, v0 = v.count(1), v.count(0)
        print ( "  Unbiased = %r" % (Stats(v1, v0, 100. * v1/(v1 + v0)), ) )
```


'''Sample output'''

```txt

Biased(3)  = Stats(count1=331800, count0=668200, percent=33.18)
  Unbiased = Stats(count1=499740, count0=500260, percent=49.973999999999997)
Biased(4)  = Stats(count1=249770, count0=750230, percent=24.977)
  Unbiased = Stats(count1=499707, count0=500293, percent=49.970700000000001)
Biased(5)  = Stats(count1=199764, count0=800236, percent=19.976400000000002)
  Unbiased = Stats(count1=499456, count0=500544, percent=49.945599999999999)
Biased(6)  = Stats(count1=167561, count0=832439, percent=16.7561)
  Unbiased = Stats(count1=499963, count0=500037, percent=49.996299999999998)
```



## R



```rsplus
randN = function(N) sample.int(N, 1) == 1

unbiased = function(f)
   {while ((x <- f()) == f()) {}
    x}

samples = 10000
print(t(round(d = 2, sapply(3:6, function(N) c(
    N = N,
    biased = mean(replicate(samples, randN(N))),
    unbiased = mean(replicate(samples, unbiased(function() randN(N)))))))))
```


Sample output:


```txt
     N biased unbiased
[1,] 3   0.32     0.50
[2,] 4   0.24     0.50
[3,] 5   0.21     0.49
[4,] 6   0.16     0.51
```



## Racket


```racket

#lang racket
;; Using boolean #t/#f instead of 1/0
(define ((randN n)) (zero? (random n)))
(define ((unbiased biased))
  (let loop () (let ([r (biased)]) (if (eq? r (biased)) (loop) r))))

;; Counts
(define N 1000000)
(for ([n (in-range 3 7)])
  (define (try% R) (round (/ (for/sum ([i N]) (if (R) 1 0)) N 1/100)))
  (define biased (randN n))
  (printf "Count: ~a => Biased: ~a%; Unbiased: ~a%.\n"
          n (try% biased) (try% (unbiased biased))))

```

{{out}}

```txt

Count: 3 => Biased: 33%; Unbiased: 50%.
Count: 4 => Biased: 25%; Unbiased: 50%.
Count: 5 => Biased: 20%; Unbiased: 50%.
Count: 6 => Biased: 17%; Unbiased: 50%.

```



## REXX


```rexx
/*REXX program generates  unbiased random numbers  and displays the results to terminal.*/
parse arg # R seed .                             /*obtain optional arguments from the CL*/
if #==''  |  #==","     then #=1000              /*#:  the number of SAMPLES to be used.*/
if R==''  |  R==","     then R=6                 /*R:  the high number for the  range.  */
if datatype(seed, 'W')  then call random ,,seed  /*Specified?  Then use for RANDOM seed.*/
dash='─';    @b="biased";         @ub='un'@b     /*literals for the SAY column headers. */
say left('',5)   ctr("N",5)   ctr(@b)   ctr(@b'%')  ctr(@ub)  ctr(@ub"%")   ctr('samples')
dash=
       do N=3  to R;      b=0;                u=0
         do j=1  for #;   b=b + randN(N);     u=u + unbiased()
         end   /*j*/
       say  left('', 5)     ctr(N, 5)     ctr(b)    pct(b)    ctr(u)    pct(u)    ctr(#)
       end     /*N*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ctr:       return center( arg(1), word(arg(2) 12, 1), left(dash, 1)) /*show hdr│numbers.*/
pct:       return ctr( format(arg(1) / # * 100, , 2)'%' )            /*2 decimal digits.*/
randN:     parse arg z;            return random(1, z)==z            /*ret 1 if rand==Z.*/
unbiased:  do  until x\==randN(N); x=randN(N);  end;       return x  /* "  unbiased RAND*/
```

{{out|output|text=  when using the default inputs:}}

```txt

      ──N── ───biased─── ──biased%─── ──unbiased── ─unbiased%── ──samples───
        3       348         34.80%        541         54.10%        1000
        4       259         25.90%        479         47.90%        1000
        5       188         18.80%        475         47.50%        1000
        6       178         17.80%        488         48.80%        1000

```

{{out|output|text=  when using the input of:   <tt> 10000 </tt>}}

```txt

      ──N── ───biased─── ──biased%─── ──unbiased── ─unbiased%── ──samples───
        3       3435        34.35%        4995        49.95%       10000
        4       2535        25.35%        4957        49.57%       10000
        5       2019        20.19%        4958        49.58%       10000
        6       1644        16.44%        4982        49.82%       10000

```

{{out|output|text=  when using the input of:   <tt> 100000   30 </tt>}}

```txt

      ──N── ───biased─── ──biased%─── ──unbiased── ─unbiased%── ──samples───
        3      33301        33.30%       50066        50.07%       100000
        4      25359        25.36%       49401        49.40%       100000
        5      20026        20.03%       49966        49.97%       100000
        6      16579        16.58%       49956        49.96%       100000
        7      14294        14.29%       50008        50.01%       100000
        8      12402        12.40%       50479        50.48%       100000
        9      11138        11.14%       50099        50.10%       100000
       10       9973        9.97%        49988        49.99%       100000
       11       9062        9.06%        50009        50.01%       100000
       12       8270        8.27%        49929        49.93%       100000
       13       7704        7.70%        49876        49.88%       100000
       14       7223        7.22%        50414        50.41%       100000
       15       6725        6.73%        50043        50.04%       100000
       16       6348        6.35%        50252        50.25%       100000
       17       5900        5.90%        49977        49.98%       100000
       18       5583        5.58%        49991        49.99%       100000
       19       5139        5.14%        49958        49.96%       100000
       20       4913        4.91%        50198        50.20%       100000
       21       4714        4.71%        49892        49.89%       100000
       22       4517        4.52%        49760        49.76%       100000
       23       4226        4.23%        50021        50.02%       100000
       24       4174        4.17%        50141        50.14%       100000
       25       4005        4.01%        49816        49.82%       100000
       26       3890        3.89%        49819        49.82%       100000
       27       3705        3.71%        50036        50.04%       100000
       28       3567        3.57%        49665        49.67%       100000
       29       3481        3.48%        50094        50.09%       100000
       30       3355        3.36%        49831        49.83%       100000

```



## Ring


```ring

for n = 3 to 6
    biased = 0
    unb = 0
    for i = 1 to 10000
        biased += randN(n)
        unb += unbiased(n) 
    next
    see "N = " + n + " : biased = " + biased/100  + "%, unbiased = " + unb/100 + "%" + nl
next

func unbiased nr
     while 1 
           a = randN(nr)
           if a != randN(nr) return a ok
     end
 
func randN m
     m = (random(m) = 1)
     return m

```

Output:

```txt

N = 3 : biased = 25.38%, unbiased = 50.12%
N = 4 : biased = 20.34%, unbiased = 49.17%
N = 5 : biased = 16.65%, unbiased = 48.86%
N = 6 : biased = 13.31%, unbiased = 49.96%

```



## Ruby


```ruby
def rand_n(bias)
  rand(bias) == 0 ? 1 : 0
end

def unbiased(bias)
  a, b = rand_n(bias), rand_n(bias) until a != b #loop until a and b are 0,1 or 1,0
  a
end

runs = 1_000_000
keys = %i(bias biased unbiased) #use [:bias,:biased,:unbiased] in Ruby < 2.0
puts keys.join("\t")

(3..6).each do |bias|
  counter = Hash.new(0) # counter will respond with 0 when key is not known
  runs.times do
    counter[:biased] += 1 if rand_n(bias) == 1 #the first time, counter has no key for :biased, so it will respond 0
    counter[:unbiased] += 1 if unbiased(bias) == 1
  end
  counter[:bias] = bias
  puts counter.values_at(*keys).join("\t")
end
```

{{output}}

```txt

bias	biased	unbiased
3	333043	500161
4	249133	499393
5	199767	500354
6	166163	499809

```



## Rust


```Rust
#![feature(inclusive_range_syntax)]

extern crate rand;

use rand::Rng;

fn rand_n<R: Rng>(rng: &mut R, n: u32) -> usize {
    rng.gen_weighted_bool(n) as usize // maps `false` to 0 and `true` to 1
}

fn unbiased<R: Rng>(rng: &mut R, n: u32) -> usize {
    let mut bit = rand_n(rng, n);
    while bit == rand_n(rng, n) {
        bit = rand_n(rng, n);
    }
    bit
}

fn main() {
    const SAMPLES: usize = 100_000;
    let mut rng = rand::weak_rng();

    println!(" Bias    rand_n  unbiased");
    for n in 3..=6 {
        let mut count_biased = 0;
        let mut count_unbiased = 0;
        for _ in 0..SAMPLES {
            count_biased += rand_n(&mut rng, n);
            count_unbiased += unbiased(&mut rng, n);
        }

        let b_percentage = 100.0 * count_biased as f64 / SAMPLES as f64;
        let ub_percentage = 100.0 * count_unbiased as f64 / SAMPLES as f64;
        println!(
            "bias {}:  {:0.2}%   {:0.2}%",
            n, b_percentage, ub_percentage
        );
    }
}
```

{{output}}

```txt

 Bias    rand_n  unbiased
bias 3:  33.32%   49.80%
bias 4:  25.22%   50.16%
bias 5:  19.91%   50.00%
bias 6:  16.66%   49.95%

```



## Scala


```scala
def biased( n:Int ) = scala.util.Random.nextFloat < 1.0 / n

def unbiased( n:Int ) = { def loop : Boolean = { val a = biased(n); if( a != biased(n) ) a else loop }; loop }

for( i <- (3 until 7) ) println { 

  val m = 50000
  var c1,c2 = 0
  
  (0 until m) foreach { j => if( biased(i) ) c1 += 1; if( unbiased(i) ) c2 += 1 }
  
  "%d: %2.2f%%  %2.2f%%".format(i, 100.0*c1/m, 100.0*c2/m)
}
```

{{output}}

```txt
3: 33.09%  49.79%
4: 24.92%  49.92%
5: 19.75%  49.92%
6: 16.67%  50.23%
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const func integer: randN (in integer: n) is
  return ord(rand(1, n) = 1);

const func integer: unbiased (in integer: n) is func
  result
    var integer: unbiased is 0;
  begin
    repeat
      unbiased := randN(n);
    until unbiased <> randN(n);
  end func;

const proc: main is func
  local
    const integer: tests is 50000;
    var integer: n is 0;
    var integer: sumBiased is 0;
    var integer: sumUnbiased is 0;
    var integer: count is 0;
  begin
    for n range 3 to 6 do
      sumBiased := 0;
      sumUnbiased := 0;
      for count range 1 to tests do
        sumBiased +:= randN(n);
        sumUnbiased +:= unbiased(n);
      end for;
      writeln(n <& ": " <& flt(100 * sumBiased) / flt(tests) digits 3 lpad 6 <&
                   "  " <& flt(100 * sumUnbiased) / flt(tests) digits 3 lpad 6);
    end for;
  end func;
```


Output:

```txt

3: 33.004  50.024
4: 25.158  50.278
5: 20.186  49.978
6: 16.570  49.936

```



## Sidef

{{trans|Perl 6}}

```ruby
func randN (n) {
    n.rand / (n-1) -> int
}

func unbiased(n) {
    var n1 = nil
    do { n1 = randN(n) } while (n1 == randN(n))
    return n1
}

var iterations = 1000

for n in (3..6) {
    var raw = []
    var fixed = []
    iterations.times {
          raw[    randN(n) ] := 0 ++
        fixed[ unbiased(n) ] := 0 ++
    }
    printf("N=%d   randN: %s, %4.1f%%   unbiased: %s, %4.1f%%\n",
        n, [raw, fixed].map {|a| (a.dump, a[1] * 100 / iterations) }...)
}
```

{{out}}

```txt

N=3   randN: [661, 339], 33.9%   unbiased: [497, 503], 50.3%
N=4   randN: [765, 235], 23.5%   unbiased: [493, 507], 50.7%
N=5   randN: [812, 188], 18.8%   unbiased: [509, 491], 49.1%
N=6   randN: [820, 180], 18.0%   unbiased: [510, 490], 49.0%

```



## Tcl


```tcl
# 1,0 random generator factory with 1 appearing 1/N'th of the time
proc randN n {expr {rand()*$n < 1}}

# uses a biased generator of 1 or 0, to create an unbiased one
proc unbiased {biased} {
    while 1 {
	if {[set a [eval $biased]] != [eval $biased]} {return $a}
    }
}

for {set n 3} {$n <= 6} {incr n} {
    set biased [list randN $n]
    for {set i 0;array set c {0 0 1 0}} {$i < 1000000} {incr i} {
	incr c([eval $biased])
    }
    puts [format "  biased %d => #0=%d #1=%d ratio=%.2f%%" $n $c(0) $c(1) \
	      [expr {100.*$c(1)/$i}]]
    for {set i 0;array set c {0 0 1 0}} {$i < 1000000} {incr i} {
	incr c([unbiased $biased])
    }
    puts [format "unbiased %d => #0=%d #1=%d ratio=%.2f%%" $n $c(0) $c(1) \
	      [expr {100.*$c(1)/$i}]]
}
```

Sample output:

```txt

  biased 3 => #0=667076 #1=332924 ratio=33.29%
unbiased 3 => #0=500263 #1=499737 ratio=49.97%
  biased 4 => #0=750470 #1=249530 ratio=24.95%
unbiased 4 => #0=500644 #1=499356 ratio=49.94%
  biased 5 => #0=800243 #1=199757 ratio=19.98%
unbiased 5 => #0=500878 #1=499122 ratio=49.91%
  biased 6 => #0=833623 #1=166377 ratio=16.64%
unbiased 6 => #0=500518 #1=499482 ratio=49.95%

```



## zkl


```zkl
fcn randN(N){ (not (0).random(N)).toInt() }
fcn unbiased(randN){ while((a:=randN())==randN()){} a }
```


```zkl
const Z=0d100_000;
foreach N in ([3..6]){
   "%d: biased: %3.2f%%, unbiased: %3.2f%%".fmt(N,
       (0).reduce(Z,'wrap(s,_){ s+randN(N) },0.0)/Z*100,
       (0).reduce(Z,'wrap(s,_){ s+unbiased(randN.fp(N)) },0.0)/Z*100)
   .println();
}
```

{{out}}

```txt

3: biased: 33.46%, unbiased: 49.80%
4: biased: 24.95%, unbiased: 50.01%
5: biased: 19.89%, unbiased: 50.18%
6: biased: 16.75%, unbiased: 50.22%

```


{{omit from|GUISS}}

[[Category:Randomness]]
