+++
title = "Subtractive generator"
description = ""
date = 2018-09-30T09:46:45Z
aliases = []
[extra]
id = 10198
[taxonomies]
categories = []
tags = []
+++

{{task|Randomness}}

A ''subtractive generator'' calculates a sequence of [[random number generator|random numbers]], where each number is congruent to the subtraction of two previous numbers from the sequence. 

The formula is

* <big><math>r_n = r_{(n - i)} - r_{(n - j)} \pmod m</math></big>

for some fixed values of <big><math>i</math></big>, <big><math>j</math></big> and <big><math>m</math></big>, all positive integers. Supposing that <big><math>i > j</math></big>, then the state of this generator is the list of the previous numbers from <big><math>r_{n - i}</math></big> to <big><math>r_{n - 1}</math></big>. Many states generate uniform random integers from <big><math>0</math></big> to <big><math>m - 1</math></big>, but some states are bad. A state, filled with zeros, generates only zeros. If <big><math>m</math></big> is even, then a state, filled with even numbers, generates only even numbers. More generally, if <big><math>f</math></big> is a factor of <big><math>m</math></big>, then a state, filled with multiples of <big><math>f</math></big>, generates only multiples of <big><math>f</math></big>.

All subtractive generators have some weaknesses. The formula correlates <big><math>r_n</math></big>, <big><math>r_{(n - i)}</math></big> and <big><math>r_{(n - j)}</math></big>; these three numbers are not independent, as true random numbers would be. Anyone who observes <big><math>i</math></big> consecutive numbers can predict the next numbers, so the generator is not cryptographically secure. The authors of ''Freeciv'' ([http://svn.gna.org/viewcvs/freeciv/trunk/utility/rand.c?view=markup utility/rand.c]) and ''xpat2'' (src/testit2.c) knew another problem: the low bits are less random than the high bits.

The subtractive generator has a better reputation than the [[linear congruential generator]], perhaps because it holds more state. A subtractive generator might never multiply numbers: this helps where multiplication is slow. A subtractive generator might also avoid division: the value of <big><math>r_{(n - i)} - r_{(n - j)}</math></big> is always between <big><math>-m</math></big> and <big><math>m</math></big>, so a program only needs to add <big><math>m</math></big> to negative numbers.

The choice of <big><math>i</math></big> and <big><math>j</math></big> affects the period of the generator. A popular choice is <big><math>i = 55</math></big> and <big><math>j = 24</math></big>, so the formula is

* <big><math>r_n = r_{(n - 55)} - r_{(n - 24)} \pmod m</math></big>

The subtractive generator from ''xpat2'' uses

* <big><math>r_n = r_{(n - 55)} - r_{(n - 24)} \pmod{10^9}</math></big>

The implementation is by J. Bentley and comes from program_tools/universal.c of [ftp://dimacs.rutgers.edu/pub/netflow/ the DIMACS (netflow) archive] at Rutgers University. It credits Knuth, [[wp:The Art of Computer Programming|''TAOCP'']], Volume 2, Section 3.2.2 (Algorithm A).

Bentley uses this clever algorithm to seed the generator.

# Start with a single <big><math>seed</math></big> in range <big><math>0</math></big> to <big><math>10^9 - 1</math></big>.
# Set <big><math>s_0 = seed</math></big> and <big><math>s_1 = 1</math></big>. The inclusion of <big><math>s_1 = 1</math></big> avoids some bad states (like all zeros, or all multiples of 10).
# Compute <big><math>s_2, s_3, ..., s_{54}</math></big> using the subtractive formula <big><math>s_n = s_{(n - 2)} - s_{(n - 1)} \pmod{10^9}</math></big>.
# Reorder these 55 values so <big><math>r_0 = s_{34}</math></big>, <big><math>r_1 = s_{13}</math></big>, <big><math>r_2 = s_{47}</math></big>, ..., <big><math>r_n = s_{(34 * (n + 1) \pmod{55})}</math></big>.
#* This is the same order as <big><math>s_0 = r_{54}</math></big>, <big><math>s_1 = r_{33}</math></big>, <big><math>s_2 = r_{12}</math></big>, ..., <big><math>s_n = r_{((34 * n) - 1 \pmod{55})}</math></big>.
#* This rearrangement exploits how 34 and 55 are relatively prime.
# Compute the next 165 values <big><math>r_{55}</math></big> to <big><math>r_{219}</math></big>. Store the last 55 values.

This generator yields the sequence <big><math>r_{220}</math></big>, <big><math>r_{221}</math></big>, <big><math>r_{222}</math></big> and so on. For example, if the seed is 292929, then the sequence begins with <big><math>r_{220} = 467478574</math></big>, <big><math>r_{221} = 512932792</math></big>, <big><math>r_{222} = 539453717</math></big>. By starting at <big><math>r_{220}</math></big>, this generator avoids a bias from the first numbers of the sequence. This generator must store the last 55 numbers of the sequence, so to compute the next <big><math>r_n</math></big>. Any array or list would work; a [[ring buffer]] is ideal but not necessary.

Implement a subtractive generator that replicates the sequences from ''xpat2''.





## Ada


subtractive_generator.ads:

```Ada
package Subtractive_Generator is
   type State is private;
   procedure Initialize (Generator : in out State; Seed : Natural);
   procedure Next (Generator : in out State; N : out Natural);
private
   type Number_Array is array (Natural range <>) of Natural;
   type State is record
      R    : Number_Array (0 .. 54);
      Last : Natural;
   end record;
end Subtractive_Generator;
```


subtractive_generator.adb:

```Ada
package body Subtractive_Generator is

   procedure Initialize (Generator : in out State; Seed : Natural) is
      S : Number_Array (0 .. 1);
      I : Natural := 0;
      J : Natural := 1;
   begin
      S (0) := Seed;
      S (1) := 1;
      Generator.R (54) := S (0);
      Generator.R (33) := S (1);
      for N in 2 .. Generator.R'Last loop
         S (I) := (S (I) - S (J)) mod 10 ** 9;
         Generator.R ((34 * N - 1) mod 55) := S (I);
         I := (I + 1) mod 2;
         J := (J + 1) mod 2;
      end loop;
      Generator.Last := 54;
      for I in 1 .. 165 loop
         Subtractive_Generator.Next (Generator => Generator, N => J);
      end loop;
   end Initialize;

   procedure Next (Generator : in out State; N : out Natural) is
   begin
      Generator.Last := (Generator.Last + 1) mod 55;
      Generator.R (Generator.Last) :=
        (Generator.R (Generator.Last)
         - Generator.R ((Generator.Last - 24) mod 55)) mod 10 ** 9;
      N := Generator.R (Generator.Last);
   end Next;

end Subtractive_Generator;
```


Example main.adb:

```Ada
with Ada.Text_IO;
with Subtractive_Generator;

procedure Main is
   Random : Subtractive_Generator.State;
   N      : Natural;
begin
   Subtractive_Generator.Initialize (Generator => Random,
                                     Seed      => 292929);
   for I in 220 .. 222 loop
      Subtractive_Generator.Next (Generator => Random, N => N);
      Ada.Text_IO.Put_Line (Integer'Image (I) & ":" & Integer'Image (N));
   end loop;
end Main;
```


{{out}}

```txt
 220: 467478574
 221: 512932792
 222: 539453717
```



## AutoHotkey

{{works with|AutoHotkey_L}}

```AutoHotkey
r := InitR(292929)

Loop, 10
	Out .= (A_Index + 219) ":`t" GetRand(r) "`n"

MsgBox, % Out

GetRand(r) {
	i := Mod(r["j"], 55)
	, r[i] := Mod(r[i] - r[Mod(i + 31, 55)], r["m"])
	, r["j"] += 1
	return, (r[i] < 0 ? r[i] + r["m"] : r[i])
}

InitR(Seed) {
	r := {"j": 0, "m": 10 ** 9}, s := {0: Seed, 1: 1}
	Loop, 53
		s[A_Index + 1] := Mod(s[A_Index - 1] - s[A_Index], r["m"])
	Loop, 55
		r[A_Index - 1] := s[Mod(34 * A_Index, 55)]
	Loop, 165
		i := Mod(A_Index + 54, 55)
		, r[i] := Mod(r[i] - r[Mod(A_Index + 30, 55)], r["m"])
	return, r
}
```

{{out}}

```txt
220:	467478574
221:	512932792
222:	539453717
223:	20349702
224:	615542081
225:	378707948
226:	933204586
227:	824858649
228:	506003769
229:	380969305
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      dummy% = FNsubrand(292929)
      FOR i% = 1 TO 10
        PRINT FNsubrand(0)
      NEXT
      END
      
      DEF FNsubrand(s%)
      PRIVATE r%(), p% : DIM r%(54)
      IF s% = 0 THEN
        p% = (p% + 1) MOD 55
        r%(p%) = r%(p%) - r%((p% + 31) MOD 55)
        IF r%(p%) < 0 r%(p%) += 10^9
        = r%(p%)
      ENDIF
      LOCAL i%
      r%(54) = s% : r%(33) = 1
      p% = 12
      FOR i% = 2 TO 54
        r%(p%) = r%((p%+42) MOD 55) - r%((p%+21) MOD 55)
        IF r%(p%) < 0 r%(p%) += 10^9
        p% = (p% + 34) MOD 55
      NEXT
      FOR i% = 55 TO 219
        IF FNsubrand(0)
      NEXT
      = 0
```

{{out}}

```txt

 467478574
 512932792
 539453717
  20349702
 615542081
 378707948
 933204586
 824858649
 506003769
 380969305

```



## Bracmat

This is a translation of the C example.


```bracmat
1000000000:?MOD;
tbl$(state,55);
0:?si:?sj;
 
(subrand-seed=
  i,j,p2
.   1:?p2
  & mod$(!arg,!MOD):?(0$?state)
  & 1:?i
  & 21:?j
  &   whl
    ' ( !i:<55
      & (!j:~<55&!j+-55:?j|)
      & !p2:?(!j$?state)
      & (   !arg+-1*!p2:?p2:<0
          & !p2+!MOD:?p2
        |
        )
      & !(!j$state):?arg
      & !i+1:?i
      & !j+21:?j
      )
  & 0:?s1:?i
  & 24:?sj
  &   whl
    ' ( !i:<165
      & subrand$
      & !i+1:?i
      ));
             
(subrand=
  x
.   (!si:!sj&subrand-seed$0|)
  & (!si:>0&!si+-1|54):?si
  & (!sj:>0&!sj+-1|54):?sj
  & (   !(!si$state)+-1*!(!sj$state):?x:<0
      & !x+!MOD:?x
    |
    )
  & !x:?(!si$?state));
   
(Main=
  i
.   subrand-seed$292929
  & 0:?i
  &   whl
    ' ( !i:<10
      & out$(subrand$)
      & !i+1:?i
      ));

Main$;
```
  

{{out}}

```txt
467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305
```



## C

This is basically the same as the reference C code, only differs in that it's C89.

```c>#include<stdio.h


#define MOD 1000000000
int state[55], si = 0, sj = 0;

int subrand();

void subrand_seed(int p1)
{
	int i, j, p2 = 1;

	state[0] = p1 % MOD;
	for (i = 1, j = 21; i < 55; i++, j += 21) {
		if (j >= 55) j -= 55;
		state[j] = p2;
		if ((p2 = p1 - p2) < 0) p2 += MOD;
		p1 = state[j];
	}
	si = 0;
	sj = 24;
	for (i = 0; i < 165; i++) subrand();
}

int subrand()
{
	int x;
	if (si == sj) subrand_seed(0);

	if (!si--) si = 54;
	if (!sj--) sj = 54;
	if ((x = state[si] - state[sj]) < 0) x += MOD;

	return state[si] = x;
}

int main()
{
	subrand_seed(292929);
	int i;
	for (i = 0; i < 10; i++) printf("%d\n", subrand());

	return 0;
}
```



## C++

{{libheader|Boost}}

```cpp

// written for clarity not efficiency.

#include <iostream>
using std::cout;
using std::endl;

#include <boost/array.hpp>
#include <boost/circular_buffer.hpp>

class Subtractive_generator {
private:
    static const int param_i = 55;
    static const int param_j = 24;
    static const int initial_load = 219;
    static const int mod = 1e9;
    boost::circular_buffer<int> r;
public:
    Subtractive_generator(int seed);
    int next(); 
    int operator()(){return next();} 
};

Subtractive_generator::Subtractive_generator(int seed)
:r(param_i)
{
    boost::array<int, param_i> s;
    s[0] = seed;
    s[1] = 1;
    for(int n = 2; n < param_i; ++n){
        int t = s[n-2]-s[n-1];
        if (t < 0 ) t+= mod;
        s[n] = t;
    }

    for(int n = 0; n < param_i; ++n){
	int i = (34 * (n+1)) % param_i;
        r.push_back(s[i]);
    }
    for(int n = param_i; n <= initial_load; ++n) next();
}

int Subtractive_generator::next()
{
    int t = r[0]-r[31];
    if (t < 0) t += mod;
    r.push_back(t);
    return r[param_i-1];
}

int main()
{
    Subtractive_generator rg(292929);

    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;

    return 0;
} 

```


{{out}}

```txt

result = 467478574
result = 512932792
result = 539453717
result = 20349702
result = 615542081
result = 378707948
result = 933204586

```


=={{header|C sharp|C#}}==

```csharp

public class SubtractiveGenerator {
    public static int MAX = 1000000000;
    private int[] state;
    private int pos;

    private int mod(int n) {
        return ((n % MAX) + MAX) % MAX;
    }

    public SubtractiveGenerator(int seed) {
        state = new int[55];

        int[] temp = new int[55];
        temp[0] = mod(seed);
        temp[1] = 1;
        for(int i = 2; i < 55; ++i)
            temp[i] = mod(temp[i - 2] - temp[i - 1]);

        for(int i = 0; i < 55; ++i)
            state[i] = temp[(34 * (i + 1)) % 55];

        pos = 54;
        for(int i = 55; i < 220; ++i)
            next();
    }

    public int next() {
        int temp = mod(state[(pos + 1) % 55] - state[(pos + 32) % 55]);
        pos = (pos + 1) % 55;
        state[pos] = temp;
        return temp;
    }

    static void Main(string[] args) {
        SubtractiveGenerator gen = new SubtractiveGenerator(292929);
        for(int i = 220; i < 230; ++i)
            Console.WriteLine(i.ToString() + ": " + gen.next().ToString());
    }
}

```


{{out}}

```txt

220: 467478574
221: 512932792
222: 539453717
223: 20349702
224: 615542081
225: 378707948
226: 933204586
227: 824858649
228: 506003769
229: 380969305

```



## Clojure


```clojure
(defn xpat2-with-seed
  "produces an xpat2 function initialized from seed"
  [seed]
  (let [e9 1000000000
        fs (fn [[i j]] [j (mod (- i j) e9)])
        s (->> [seed 1] (iterate fs) (map first) (take 55) vec)
        rinit (map #(-> % inc (* 34) (mod 55) s) (range 55))
        r-atom (atom [54 (int-array rinit)])
        update (fn [[nprev r]]
                  (let [n (-> nprev inc (mod 55))
                        rx #(get r (-> n (- %) (mod 55)))
                        rn (-> (rx 55) (- (rx 24)) (mod e9))
                        _ (aset-int r n rn)]
                    [n r]))
        xpat2 #(let [[n r] (swap! r-atom update)]
                (get r n))
        _ (dotimes [_ 165] (xpat2))]
    xpat2))
    
(def xpat2 (xpat2-with-seed 292929))

(println (xpat2) (xpat2) (xpat2)) ; prints: 467478574 512932792 539453717

```



## Common Lisp


```lisp
(defun sub-rand (state)
  (let ((x (last state)) (y (last state 25)))
    ;; I take "circular buffer" very seriously (until some guru
    ;; points out it's utterly wrong thing to do)
    (setf (cdr x) state)
    (lambda () (setf x (cdr x)
		     y (cdr y)
		     (car x) (mod (- (car x) (car y)) (expt 10 9))))))

;; returns an RNG with Bentley seeding
(defun bentley-clever (seed)
  (let ((s (list 1 seed))  f)
    (dotimes (i 53)
      (push (mod (- (cadr s) (car s)) (expt 10 9)) s))
    (setf f (sub-rand
	      (loop for i from 1 to 55 collect
		    (elt s (- 54 (mod (* 34 i) 55))))))
    (dotimes (x 165) (funcall f))
    f))

;; test it (output same as everyone else's)
(let ((f (bentley-clever 292929)))
  (dotimes (x 10) (format t "~a~%" (funcall f))))
```



## D

{{trans|C}}


```d
import std.stdio;

struct Subtractive {
    enum MOD = 1_000_000_000;
    private int[55] state;
    private int si, sj;

    this(in int p1) pure nothrow {
        subrandSeed(p1);
    }

    void subrandSeed(int p1) pure nothrow {
        int p2 = 1;

        state[0] = p1 % MOD;
        for (int i = 1, j = 21; i < 55; i++, j += 21) {
            if (j >= 55)
                j -= 55;
            state[j] = p2;
            if ((p2 = p1 - p2) < 0)
                p2 += MOD;
            p1 = state[j];
        }

        si = 0;
        sj = 24;
        foreach (i; 0 .. 165)
            subrand();
    }

    int subrand() pure nothrow {
        if (si == sj)
            subrandSeed(0);

        if (!si--)
            si = 54;
        if (!sj--)
            sj = 54;

        int x = state[si] - state[sj];
        if (x < 0)
            x += MOD;

        return state[si] = x;
    }
}

void main() {
    auto gen = Subtractive(292_929);
    foreach (i; 0 .. 10)
        writeln(gen.subrand());
}
```

{{out}}

```txt
467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305
```



## dc


```dc
[*
 * (seed) lsx --
 * Seeds the subtractive generator.
 * Uses register R to hold the state.
 *]sz
[
 [* Fill ring buffer R[0] to R[54]. *]sz
 d 54:R SA              [A = R[54] = seed]sz
 1 d 33:R SB            [B = R[33] = 1]sz
 12 SC                  [C = index 12, into array R.]sz
 [55 -]SI
 [                      [Loop until C is 54:]sz
  lA lB - d lC:R         [R[C] = A - B]sz
  lB sA sB               [Parallel let A = B and B = A - B]sz
  lC 34 + d 55 !>I d sC  [C += 34 (mod 55)]sz
  54 !=L
 ]d SL x
 [* Point R[55] and R[56] into ring buffer. *]sz
 0 55:R                 [R[55] = index 0, of 55th last number.]sz
 31 56:R                [R[56] = index 31, of 24th last number.]sz
 [* Stir ring buffer. *]sz
 165 [                  [Loop 165 times:]sz
  55;R;R 56;R;R - 55;R:R [Discard a random number.]sz
  55;R 1 + d 55 !>I 55:R [R[55] += 1 (mod 55)]sz
  56;R 1 + d 55 !>I 56:R [R[56] += 1 (mod 55)]sz
  1 - d 0 <L
 ]d sL x
 LAsz LBsz LCsz LIsz LLsz
]ss

[*
 * lrx -- (random number from 0 to 10^9 - 1)
 * Returns the next number from the subtractive generator.
 * Uses register R, seeded by lsx.
 *]sz
[
 55;R;R 56;R;R -        [R[R[55]] - R[R[56]] is next random number.]sz
 d 55;R:R               [Put it in R[R[55]]. Also leave it on stack.]sz
 [55 -]SI
 55;R 1 + d 55 !>I 55:R [R[55] += 1 (mod 55)]sz
 56;R 1 + d 55 !>I 56:R [R[56] += 1 (mod 55)]sz
 [1000000000 +]sI
 1000000000 % d 0 >I    [Random number = it (mod 10^9)]sz
 LIsz
]sr


[* Seed with 292929 and print first three random numbers. *]sz
292929 lsx
lrx psz
lrx psz
lrx psz
```


This program prints 467478574, 512932792, 539453717.

This implementation never uses multiplication, but it does use modulus (remainder from division) to put each random number in range from 0 to 10^9 - 1.


## Elixir

{{trans|Ruby}}

```elixir
defmodule Subtractive do
  def new(seed) when seed in 0..999_999_999 do
    s = Enum.reduce(1..53, [1, seed], fn _,[a,b|_]=acc -> [b-a | acc] end)
        |> Enum.reverse
        |> List.to_tuple
    state = for i <- 1..55, do: elem(s, rem(34*i, 55))
    {:ok, _pid} = Agent.start_link(fn -> state end, name: :Subtractive)
    Enum.each(1..220, fn _ -> rand end) # Discard first 220 elements of sequence.
  end
  
  def rand do
    state = Agent.get(:Subtractive, &(&1))
    n = rem(Enum.at(state, -55) - Enum.at(state, -24) + 1_000_000_000, 1_000_000_000)
    :ok = Agent.update(:Subtractive, fn _ -> tl(state) ++ [n] end)
    hd(state)
  end
end

Subtractive.new(292929)
for _ <- 1..10, do: IO.puts Subtractive.rand
```


{{out}}

```txt

467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305

```


=={{header|F_Sharp|F#}}==
<p>Similar to Haskell, using lazy evaluation.</p>

```fsharp>[<EntryPoint
]
let main argv =
    let m = 1000000000
    let init = Seq.unfold (fun ((i, s2, s1)) -> Some((s2,i), (i+1, s1, (m+s2-s1)%m))) (0, 292929, 1)
            |> Seq.take 55
            |> Seq.sortBy (fun (_,i) -> (34*i+54)%55)
            |> Seq.map fst
    let rec r = seq {
        yield! init
        yield! Seq.map2 (fun u v -> (m+u-v)%m) r (Seq.skip 31 r)
    }
        
    r |> Seq.skip 220 |> Seq.take 3
    |> Seq.iter (printfn "%d")
    0
```

{{out}}

```txt
467478574
512932792
539453717
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
module subgenerator
  implicit none

  integer, parameter :: modulus = 1000000000
  integer :: s(0:54), r(0:54)
  
contains 

subroutine initgen(seed)
  integer :: seed
  integer :: n, rnum

  s(0) = seed
  s(1) = 1

  do n = 2, 54
    s(n) = mod(s(n-2) - s(n-1), modulus)
    if (s(n) < 0) s(n) = s(n) + modulus
  end do
   
  do n = 0, 54
    r(n) = s(mod(34*(n+1), 55))
  end do

  do n = 1, 165
    rnum = subrand()
  end do 
  
end subroutine initgen

integer function subrand()
  integer, save :: p1 = 0
  integer, save :: p2 = 31

  r(p1) = mod(r(p1) - r(p2), modulus)
  if (r(p1) < 0) r(p1) = r(p1) + modulus
  subrand = r(p1)
  p1 = mod(p1 + 1, 55)
  p2 = mod(p2 + 1, 55)

end function subrand  
end module subgenerator
  
program subgen_test
  use subgenerator
  implicit none
 
  integer :: seed = 292929
  integer :: i
  
  call initgen(seed)
  do i = 1, 10
    write(*,*) subrand()
  end do 
 
end program
```

{{out}}

```txt

    467478574
    512932792
    539453717
    20349702
    615542081
    378707948
    933204586
    824858649
    506003769
    380969305

```



## Go


```go
package main

import (
    "fmt"
    "os"
)

// A fairly close port of the Bentley code, but parameterized to better
// conform to the algorithm description in the task, which didn't assume
// constants for i, j, m, and seed.  also parameterized here are k,
// the reordering factor, and s, the number of intial numbers to discard,
// as these are dependant on i.
func newSG(i, j, k, s, m, seed int) func() int {
    // check parameters for range and mutual consistency
    assert(i > 0, "i must be > 0")
    assert(j > 0, "j must be > 0")
    assert(i > j, "i must be > j")
    assert(k > 0, "k must be > 0")
    p, q := i, k
    if p < q {
        p, q = q, p
    }
    for q > 0 {
        p, q = q, p%q
    }
    assert(p == 1, "k, i must be relatively prime")
    assert(s >= i, "s must be >= i")
    assert(m > 0, "m must be > 0")
    assert(seed >= 0, "seed must be >= 0")
    // variables for closure f
    arr := make([]int, i)
    a := 0
    b := j
    // f is Bently RNG lprand
    f := func() int {
        if a == 0 {
            a = i
        }
        a--
        if b == 0 {
            b = i
        }
        b--
        t := arr[a] - arr[b]
        if t < 0 {
            t += m
        }
        arr[a] = t
        return t
    }
    // Bentley seed algorithm sprand
    last := seed
    arr[0] = last
    next := 1
    for i0 := 1; i0 < i; i0++ {
        ii := k * i0 % i
        arr[ii] = next
        next = last - next
        if next < 0 {
            next += m
        }
        last = arr[ii]
    }
    for i0 := i; i0 < s; i0++ {
        f()
    }
    // return the fully initialized RNG
    return f
}

func assert(p bool, m string) {
    if !p {
        fmt.Println(m)
        os.Exit(1)
    }
}

func main() {
    // 1st test case included in program_tools/universal.c.
    // (2nd test case fails.  A single digit is missing, indicating a typo.)
    ptTest(0, 1, []int{921674862, 250065336, 377506581})

    // reproduce 3 values given in task description
    skip := 220
    sg := newSG(55, 24, 21, skip, 1e9, 292929)
    for n := skip; n <= 222; n++ {
        fmt.Printf("r(%d) = %d\n", n, sg())
    }
}

func ptTest(nd, s int, rs []int) {
    sg := newSG(55, 24, 21, 220+nd, 1e9, s)
    for _, r := range rs {
        a := sg()
        if r != a {
            fmt.Println("Fail")
            os.Exit(1) 
        }
    }
}
```

{{out}}

```txt

r(220) = 467478574
r(221) = 512932792
r(222) = 539453717

```



## Haskell


```haskell
subtractgen :: Int -> [Int]
subtractgen seed = drop 220 out
  where
    out = mmod $ r ++ zipWith (-) out (drop 31 out)
      where
        r = take 55 $ shuffle $ cycle $ take 55 s
        shuffle x = head xx : shuffle xx
          where
            xx = drop 34 x
        s = mmod $ seed : 1 : zipWith (-) s (tail s)
        mmod = map (`mod` 10 ^ 9)

main :: IO ()
main = mapM_ print $ take 10 $ subtractgen 292929
```

{{out}}

```txt

467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
   every 1 to 10 do 
      write(rand_sub(292929))
end
 
procedure rand_sub(x)
static ring,m
   if /ring then {
      m := 10^9
      every (seed | ring) := list(55)
      seed[1] := \x | ?(m-1)
      seed[2] := 1
      every seed[n := 3 to 55] := (seed[n-2]-seed[n-1])%m
      every ring[(n := 0 to 54) + 1] := seed[1 + (34 * (n + 1)%55)]
      every  n := *ring to 219 do {
         ring[1] -:= ring[-24]    
         ring[1] %=  m
         put(ring,get(ring))     
         }
   }
   ring[1] -:= ring[-24]
   ring[1] %:= m
   if ring[1] < 0 then ring[1] +:= m
   put(ring,get(ring))
   return ring[-1]
end
```


{{out}}

```txt
467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305
```



## J

sg.ijs

Loops are hidden in a generalized power conjunction '''^:''' .
'''f^:n y''' evaluates '''f n''' times, as in  '''f(f(f(...f(y)))...)''' .
Yes!  '''f^:(-1)''' IS the inverse of '''f''' .  When known.


```J
came_from_locale_sg_=: coname''
cocurrent'sg' NB. install the state of rng sg into locale sg

SEED=: 292929
'I J M first_Bentley_number B2'=: 55 24 1e9 34 165
SG=: 1 : 'M&|@:-/@:(m&{)'
r=: (I|(first_Bentley_number*>:i.I)) { (, _2 _1 SG)^:(I-2) 1,~SEED

sg=: 3 : 0
t=. (, (-I,J)SG)^:y r
r=: y }. t
t {.~ -y
)
discard=. sg B2

cocurrent came_from_locale  NB. return to previous locale
sg=: sg_sg_                 NB. make a local name for sg in locale sg

```


Use:

```sh
$ jconsole
   load'sg.ijs'
   sg 2
467478574 512932792
   sg 4
539453717 20349702 615542081 378707948
   

```



## Java

Translation of [[Subtractive_generator#C|C]] via [[Subtractive_generator#D|D]]
{{works with|Java|8}}

```java
import java.util.function.IntSupplier;
import static java.util.stream.IntStream.generate;

public class SubtractiveGenerator implements IntSupplier {
    static final int MOD = 1_000_000_000;
    private int[] state = new int[55];
    private int si, sj;

    public SubtractiveGenerator(int p1) {
        subrandSeed(p1);
    }

    void subrandSeed(int p1) {
        int p2 = 1;

        state[0] = p1 % MOD;
        for (int i = 1, j = 21; i < 55; i++, j += 21) {
            if (j >= 55)
                j -= 55;
            state[j] = p2;
            if ((p2 = p1 - p2) < 0)
                p2 += MOD;
            p1 = state[j];
        }

        si = 0;
        sj = 24;
        for (int i = 0; i < 165; i++)
            getAsInt();
    }

    @Override
    public int getAsInt() {
        if (si == sj)
            subrandSeed(0);

        if (si-- == 0)
            si = 54;
        if (sj-- == 0)
            sj = 54;

        int x = state[si] - state[sj];
        if (x < 0)
            x += MOD;

        return state[si] = x;
    }

    public static void main(String[] args) {
        generate(new SubtractiveGenerator(292_929)).limit(10)
                .forEach(System.out::println);
    }
}
```



```txt
467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305
```



## Julia

Here is a script, which does not use multiplicative operators, without relying on the optimizer.
{{works with|Julia|1.0}}


```julia
i,j,m,d,seed = 55,24,10^9,34,292929     # parameters
s = Array{Int32}(undef,i); r = similar(s)
s[1:2] = [seed,1]                       # table initialization
for n = 3:i; (s[n] = s[n-2]-s[n-1]) < 0 && (s[n] += m) end
t = 1; for u=1:i; (global t+=d)>i && (t-=i); r[u]=s[t] end # permutation, r = s[(d*(1:i) .% i).+1]

u,v,n = i,i-j,i-1
while (n += 1) > 0
    (global u += 1) > i && (u = 1)      # circular indexing: u,v = ((n,n-j) .% i).+1
    (global v += 1) > i && (v = 1)
    (r[u] -= r[v]) < 0 && (r[u] += m)   # table update
    n < 220 && continue                 # 165 silent values
    print((n,r[u]))                     # show (index,value) of next pseudorandom number
    x = readline(stdin)                 # wait until the ENTER key is pressed
    length(x) > 0 && break              # any other key before ENTER => exit
end
```

{{out}}

```txt
(220, 467478574)
(221, 512932792)
(222, 539453717)
(223, 20349702)
(224, 615542081)
(225, 378707948)
(226, 933204586)
(227, 824858649)
(228, 506003769)
(229, 380969305)
(230, 442823364)
(231, 994162810)
(232, 261423281)
```



## Kotlin

{{trans|C}}

```scala
// version 1.1.51

const val MOD = 1_000_000_000

val state = IntArray(55)
var si = 0
var sj = 0

fun subrandSeed(p: Int) {
    var p1 = p
    var p2 = 1
    state[0] = p1 % MOD
    var j = 21
    for (i in 1..54) {       
        if (j >=55) j -= 55
        state[j] = p2
        p2 = p1 - p2
        if (p2 < 0) p2 += MOD
        p1 = state[j]
        j += 21
    }
    si = 0
    sj = 24
    repeat(165) { subrand() }
}

fun subrand(): Int {
    if (si == sj) subrandSeed(0)
    if (si-- == 0) si = 54
    if (sj-- == 0) sj = 54
    var x = state[si] - state[sj]
    if (x < 0) x += MOD
    state[si] = x
    return x
}

fun main(args: Array<String>) {
    subrandSeed(292_929)
    for (i in 0..9) println("r[${i + 220}] = ${subrand()}")
}
```


{{out}}

```txt

r[220] = 467478574
r[221] = 512932792
r[222] = 539453717
r[223] = 20349702
r[224] = 615542081
r[225] = 378707948
r[226] = 933204586
r[227] = 824858649
r[228] = 506003769
r[229] = 380969305

```



## Mathematica


```Mathematica
initialize[n_] :=
 Module[{buffer},
  buffer = 
   Join[Nest[Flatten@{#, Mod[Subtract @@ #[[-2 ;;]], 10^9]} &, {n, 1},
       53][[1 + Mod[34 Range@54, 55]]], {n}];
  Nest[nextValue, buffer, 165]]
  
  nextValue[buffer_] := 
 Flatten@{Rest@buffer, Mod[Subtract @@ buffer[[{1, 32}]], 10^9]}
```



```txt
buffer = initialize[292929];
Do[Print@Last[buffer = nextValue[buffer]], {10}]

467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305


```



## OCaml

{{trans|C}}


```ocaml
let _mod = 1_000_000_000
let state = Array.create 55 0
let si = ref 0
let sj = ref 0

let rec subrand_seed _p1 =
  let p1 = ref _p1 in
  let p2 = ref 1 in
  state.(0) <- !p1 mod _mod;
  let j = ref 21 in
  for i = 1 to pred 55 do
    if !j >= 55 then j := !j - 55;
    state.(!j) <- !p2;
    p2 := !p1 - !p2;
    if !p2 < 0 then p2 := !p2 + _mod;
    p1 := state.(!j);
    j := !j + 21;
  done;
  si := 0;
  sj := 24;
  for i = 0 to pred 165 do ignore (subrand()) done

and subrand() =
  if !si = !sj then subrand_seed 0;
  decr si;  if !si < 0 then si := 54;
  decr sj;  if !sj < 0 then sj := 54;
  let x = state.(!si) - state.(!sj) in
  let x = if x < 0 then x + _mod else x in
  state.(!si) <- x;
  (x)

let () =
  subrand_seed 292929;
  for i = 1 to 10 do Printf.printf "%d\n" (subrand()) done
```


{{out}}

```txt
$ ocaml sub_gen.ml
467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305
```



## ooREXX

{{trans|REXX}}

```oorexx
/*REXX program uses a subtractive generaTor,and creates a sequence of ranDom numbers. */
/* array index must be positive! */
s=.array~new
r=.array~new
s[1]=292929
s[2]=1
billion=1e9
numeric digits 20
ci=55
Do i=2 To ci-1
  s[i+1]=mod(s[i-1]-s[i],billion)
  End
cp=34
Do j=0 To ci-1
  r[j+1]=s[mod(cp*(j+1),ci)+1]
  End
m=219
cj= 24
Do k=ci To m
  _=k//ci
  r[_+1]=mod(r[mod(k-ci,ci)+1]-r[mod(k-cj,ci)+1],billion)
  End
t=235
Do n=m+1 To t
  _=n//ci
  r[_+1]=mod(r[mod(n-ci,ci)+1]-r[mod(n-cj,ci)+1],billion)
  Say right(r[_+1],40)
  End
Exit
mod: Procedure
Parse Arg a,b
Return ((a//b)+b)//b  
```

{{out|output|text=  when using the default input:}}

```txt
same as with REXX
```



## PARI/GP


```parigp
sgv=vector(55,i,random(10^9));sgi=1;
sg()=sgv[sgi=sgi%55+1]=(sgv[sgi]-sgv[(sgi+30)%55+1])%10^9
```



## Perl


```perl
use 5.10.0;
use strict;

{ # bracket state data into a lexical scope
	my @state;
	my $mod = 1_000_000_000;

	sub bentley_clever {
		my @s = ( shift() % $mod, 1);
		push @s, ($s[-2] - $s[-1]) % $mod	while @s < 55;
		@state = map($s[(34 + 34 * $_) % 55], 0 .. 54);
		subrand() for (55 .. 219);
	}

	sub subrand()
	{
		bentley_clever(0) unless @state; # just incase

		my $x = (shift(@state) - $state[-24]) % $mod;
		push @state, $x;
		$x;
	}
}

bentley_clever(292929);
say subrand() for (1 .. 10);
```

{{out}}

```txt
467478574
512932792
539453717
20349702
615542081
...
```



## Perl 6

{{trans|Perl}}
{{works with|Rakudo|2018.03}}


```perl6
sub bentley-clever($seed) {
    constant $mod = 1_000_000_000;
    my @seeds = ($seed % $mod, 1, (* - *) % $mod ... *)[^55];
    my @state = @seeds[ 34, (* + 34 ) % 55 ... 0 ];

    sub subrand() {
        push @state, (my $x = (@state.shift - @state[*-24]) % $mod);
        $x;
    }

    subrand for 55 .. 219;

    &subrand ... *;
}

my @sr = bentley-clever(292929);
.say for @sr[^10];
```

Here we just make the seeder return the random sequence as a lazy list.

{{out}}

```txt
467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305
```



## Phix

{{trans|C#}}

```Phix
sequence state = repeat(0,55)
integer pos
 
constant MAX = 1e9
function cap(integer n)
    if n<0 then n += MAX end if
    return n
end function

function next()
    pos = mod(pos,55)+1
    integer temp = cap(state[pos]-state[mod(pos+30,55)+1])
    state[pos] = temp
    return temp
end function

procedure init(integer seed)
    sequence temp = repeat(0,55)
    temp[1] = cap(seed)
    temp[2] = 1
    for i=3 to 55 do
        temp[i] = cap(temp[i-2]-temp[i-1])
    end for 
    for i=1 to 55 do
        state[i] = temp[mod(34*i,55)+1]
    end for 
    pos = 55
    for i=55 to 219 do
        {} = next()
    end for
end procedure
 
init(292929)
for i=220 to 222 do
    printf(1,"%d: %d\n",{i,next()})
end for
```

{{out}}

```txt

220: 467478574
221: 512932792
222: 539453717

```



## PicoLisp

Using a circular list (as a true "ring" buffer).

```PicoLisp
(setq
   *Bentley (apply circ (need 55))
   *Bentley2 (nth *Bentley 32) )

(de subRandSeed (S)
   (let (N 1  P (nth *Bentley 55))
      (set P S)
      (do 54
         (set (setq P (nth P 35)) N)
         (when (lt0 (setq N (- S N)))
            (inc 'N 1000000000) )
         (setq S (car P)) ) )
   (do 165 (subRand)) )

(de subRand ()
   (when (lt0 (dec *Bentley (pop '*Bentley2)))
      (inc *Bentley 1000000000) )
   (pop '*Bentley) )
```

Test:

```PicoLisp
(subRandSeed 292929)
(do 7 (println (subRand)))
```

{{out}}

```txt
467478574
512932792
539453717
20349702
615542081
378707948
933204586
```



## PL/I


```PL/I

subtractive_generator: procedure options (main);

   declare (r, s) (0:54) fixed binary (31);
   declare (i, n, seed)  fixed binary (31);

   /* Bentley's initialization */
   seed = 292929;
   s(0) = seed; s(1) = 1;

   /* Compute s2,s3,...,s54 using the subtractive formula sn = s(n-2) - s(n-1)(mod 10**9). */
   do n = 2 to hbound(s,1);
      s(n) = mod ( s(n-2) - s(n-1), 1000000000);
   end;

   /* Rearrange initial values. */
   do n = 0 to hbound(r,1);
      r(n) = s( mod(34*(n+1), 55));
   end;

   do n = 55 to 219;
      i = mod (n, 55);
      r(i) = mod ( r(mod(n-55, 55)) - r(mod(n-24, 55)), 1000000000);
   end;

   do n = 220 to 235;
      i = mod(n, 55);
      r(i) = mod ( r(mod(n-55, 55)) - r(mod(n-24, 55)), 1000000000);
      put skip list (r(i));
   end;

end subtractive_generator;

```


```txt

Required 3 results:
     467478574
     512932792 
     539453717
Subsequent values:
      20349702 
     615542081 
     378707948 
     933204586 
     824858649 
     506003769 
     380969305 
     442823364 
     994162810 
     261423281 
     139610325 
      80746560 
     563900213

```



## PowerShell

{{works with|PowerShell|2}}
The so-called modulus operator in PowerShell (%) returns a remainder not a modulus. Hence the need for the custom Mod function when working with negative numbers.
( X % M + M ) % M can be replaced with ( X + M ) % M when X is always between -M and M, as is the case in this task, but the former is used for clarity.
The first 55 generated values are placed directly into their reordered slots in the ring.
An array object is used along with a rotating index object to simulate a ring.

```PowerShell

function Get-SubtractiveRandom ( [int]$Seed )
    {
    function Mod ( [int]$X, [int]$M = 1000000000 ) { ( $X % $M + $M ) % $M }
 
    If ( $Seed )
        {
        $R = New-Object int[] 55
 
        $N1 = 55 - 1
        $N2 = ( $N1 + 34 ) % 55
 
        $R[$N1] = $Seed
        $R[$N2] = 1
 
        ForEach ( $x in 2..(55-1) )
            {
            $N0, $N1, $N2 = $N1, $N2, ( ( $N2 + 34 ) % 55 )
            $R[$N2] = Mod ( $R[$N0] - $R[$N1] )
            }
 
        $i = -55 - 1
        $j = -24 - 1
 
        ForEach ( $x in 55..219 )
            {
            $i = ++$i % 55
            $j = ++$j % 55
            $R[$i] = Mod ( $R[$i] - $R[$j] )
            }
 
        $Script:RandomRing  = $R
        $Script:RandomIndex = $i
        }
 
    $i = $Script:RandomIndex = ++$Script:RandomIndex % 55
    $j = ( $i + 55 - 24 ) % 55
 
    return ( $Script:RandomRing[$i] = Mod ( $Script:RandomRing[$i] - $Script:RandomRing[$j] ) )
    }
 
 
Get-SubtractiveRandom 292929
Get-SubtractiveRandom
Get-SubtractiveRandom
Get-SubtractiveRandom
Get-SubtractiveRandom

```

{{out}}

```txt

467478574
512932792
539453717
20349702
615542081

```



## Python


### Python: With explanation

Uses collections.deque as a ring buffer


```python

import collections
s= collections.deque(maxlen=55)
#    Start with a single seed in range 0 to 10**9 - 1.
seed = 292929

#    Set s0 = seed and s1 = 1. 
#    The inclusion of s1 = 1 avoids some bad states 
#    (like all zeros, or all multiples of 10).
s.append(seed)
s.append(1)

#    Compute s2,s3,...,s54 using the subtractive formula 
#    sn = s(n - 2) - s(n - 1)(mod 10**9).
for n in xrange(2, 55):
    s.append((s[n-2] - s[n-1]) % 10**9)

#    Reorder these 55 values so r0 = s34, r1 = s13, r2 = s47, ..., 
#                               rn = s(34 * (n + 1)(mod 55)).

r = collections.deque(maxlen=55)
for n in xrange(55):
    i = (34 * (n+1)) % 55
    r.append(s[i])
#        This is the same order as s0 = r54, s1 = r33, s2 = r12, ..., 
#                                  sn = r((34 * n) - 1(mod 55)).
#        This rearrangement exploits how 34 and 55 are relatively prime. 
#    Compute the next 165 values r55 to r219. Store the last 55 values.


def getnextr():
    """get next random number"""
    r.append((r[0]-r[31])%10**9)
    return r[54]

# rn = r(n - 55) - r(n - 24)(mod 10**9) for n >= 55
for n in xrange(219 - 54):
    getnextr()

# now fully initilised
# print first five numbers
for i in xrange(5):
    print "result = ", getnextr()

```



### Python: As a class within a module

Python 2 and 3 compatable.

```python
import collections

_ten2nine = 10**9

class Subtractive_generator():
    
    def __init__(self, seed=292929):
        self.r = collections.deque(maxlen=55)
        s = collections.deque(maxlen=55)
        s.extend([seed, 1])
        s.extend((s[n-2] - s[n-1]) % _ten2nine for n in range(2, 55))
        self.r.extend(s[(34 * (n+1)) % 55] for n in range(55)) 
        for n in range(219 - 54):
            self()
     
    def __call__(self):
        r = self.r
        r.append((r[0] - r[31]) % _ten2nine)
        return r[54]
     
if __name__ == '__main__':
    srand = Subtractive_generator()
    print([srand() for i in range(5)])
```


{{out}}

```txt
[467478574, 512932792, 539453717, 20349702, 615542081]
```



## Racket


```Racket
#lang racket
(define (make-initial-state a-list max-i)
  (for/fold ((state a-list))
            ((i (in-range (length a-list) max-i)))
    (append state (list (- (list-ref state (- i 2)) (list-ref state (- i 1))))))) ;from the seed and 1 creates the initial state

(define (shuffle a-list)
  (for/list ((i (in-range (length a-list))))
    (list-ref a-list (modulo (* 34 (add1 i)) 55))))  ;shuffles the state

(define (advance-state state (times 1))
  (cond ((= 0 times) state)
        (else (advance-state
               (cdr (append state
                            (list (modulo (- (list-ref state 0) (list-ref state 31))
                                          (expt 10 9)))))
                             (sub1 times)))))  ;takes a state and the times it must be advanced, and returns the new state

(define (create-substractive-generator s0)
  (define s1 1)
  (define first-state (make-initial-state (list s0 s1) 55))
  (define shuffled-state (shuffle first-state))
  (define last-state (advance-state shuffled-state 165))
  (lambda ((m (expt 10 9)))
    (define new-state (advance-state last-state))
    (set! last-state new-state)
    (modulo (car (reverse last-state)) m)))                    ;the lambda is a function with an optional argument
                                                               ;that returns a new random number each time it's called
(define rand (create-substractive-generator 292929))
(build-list 3 (lambda (_) (rand)))  ;returns a list made from the 3 wanted numbers
```



## REXX

{{trans|PL/I}}


```rexx
/*REXX program uses a  subtractive generator, and creates a sequence of random numbers. */
s.0= 292929;       s.1= 1;                                billion= 1e9    /* ◄────────┐ */
numeric digits 20;                                        billion= 10**9  /*same as─►─┘ */
cI= 55;            do i=2    to cI-1
                   s.i= mod( s(i-2)   -   s(i-1), billion)
                   end   /*i*/
Cp= 34
                   do j=0    to cI-1
                   r.j= s( mod( cP * (j+1), cI))
                   end   /*j*/
m= 219;   Cj= 24
                   do k=cI   to m;     _= k // cI
                   r._= mod( r( mod(k-cI, cI))   -   r( mod(k-cJ, cI)), billion)
                   end   /*k*/
t= 235
                   do n=m+1  to t;     _= n // cI
                   r._= mod( r( mod(n-cI, cI))   -   r( mod(n-cJ, cI)), billion)
                   say   right(r._, 40)
                   end   /*n*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
mod: procedure;    parse arg a,b;      return  ( (a // b)  +  b)   //   b
r:                 parse arg #;        return  r.#
s:                 parse arg #;        return  s.#
```

{{out|output|text=  when using the default input:}}

```txt

                               467478574
                               512932792
                               539453717
                                20349702
                               615542081
                               378707948
                               933204586
                               824858649
                               506003769
                               380969305
                               442823364
                               994162810
                               261423281
                               139610325
                                80746560
                               563900213

```



## Ruby

This implementation aims for simplicity, not speed. <code>SubRandom#rand</code> pushes to and shifts from an array; this might be slower than a ring buffer. The seeding method must call <code>rand</code> 55 extra times (220 times instead of 165 times). The code also calls [[Arithmetic/Integer#Ruby|Ruby's modulus operator]], which always returns a non-negative integer if the modulus is positive.


```ruby
# SubRandom is a subtractive random number generator which generates
# the same sequences as Bentley's generator, as used in xpat2.
class SubRandom
  # The original seed of this generator.
  attr_reader :seed

  # Creates a SubRandom generator with the given _seed_.
  # The _seed_ must be an integer from 0 to 999_999_999.
  def initialize(seed = Kernel.rand(1_000_000_000))
    (0..999_999_999).include? seed or
      raise ArgumentError, "seed not in 0..999_999_999"

    # @state = 55 elements.
    ary = [seed, 1]
    53.times { ary << ary[-2] - ary[-1] }
    @state = []
    34.step(1870, 34) {|i| @state << ary[i % 55] }

    220.times { rand }  # Discard first 220 elements of sequence.

    @seed = seed        # Save original seed.
  end

  # Duplicates internal state so SubRandom#dup never shares state.
  def initialize_copy(orig)
    @state = @state.dup
  end

  # Returns the next random integer, from 0 to 999_999_999.
  def rand
    @state << (@state[-55] - @state[-24]) % 1_000_000_000
    @state.shift
  end
end

rng = SubRandom.new(292929)
p (1..3).map { rng.rand }
```



```txt
[467478574, 512932792, 539453717]
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const integer: MOD is 1000000000;

const type: subtractiveGenerator is new struct
    var array integer: state is [0 .. 54] times 0;
    var integer: si is 0;
    var integer: sj is 24;
  end struct;
 
const func integer: subrand (inout subtractiveGenerator: generator) is forward;

const func subtractiveGenerator: subrandSeed (in var integer: p1) is func
  result
    var subtractiveGenerator: generator is subtractiveGenerator.value;
  local
    var integer: p2 is 1;
    var integer: i is 0;
    var integer: j is 21;
  begin
    generator.state[0] := p1 mod MOD;
    for i range 1 to 54 do
      generator.state[j] := p2;
      p2 := (p1 - p2) mod MOD;
      p1 := generator.state[j];
      j := (j + 21) mod 55;
    end for;
    for i range 1 to 165 do
      ignore(subrand(generator));
    end for;
  end func;
 
const func integer: subrand (inout subtractiveGenerator: generator) is func
  result
    var integer: subrand is 0;
  begin
    if generator.si = generator.sj then
      generator := subrandSeed(0);
    end if;
    generator.si := pred(generator.si) mod 55;
    generator.sj := pred(generator.sj) mod 55;
    subrand := (generator.state[generator.si] - generator.state[generator.sj]) mod MOD;
    generator.state[generator.si] := subrand;
  end func;
 
const proc: main is func
  local
    var subtractiveGenerator: gen is subrandSeed(292929);
    var integer: i is 0;
  begin
    for i range 1 to 10 do
      writeln(subrand(gen));
    end for;
  end func;
```


{{out}}

```txt

467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305

```



## Sidef


```ruby
class SubRandom(seed, state=[]) {

    const mod = 1_000_000_000;

    method init {
        var s = [seed % mod, 1];
        53.times {
            s.append((s[-2] - s[-1]) % mod);
        }
        state = s.range.map {|i| s[(34 + 34*i) % 55] };
        range(55, 219).each { self.subrand };
    }

    method subrand {
        var x = ((state.shift - state[-24]) % mod);
        state.append(x);
        return x;
    }
}

var r = SubRandom(292929);
10.times { say r.subrand };
```

{{out}}

```txt

467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305

```



## Tcl

{{trans|C}}

```tcl
package require Tcl 8.5
namespace eval subrand {
    variable mod 1000000000 state [lrepeat 55 0] si 0 sj 0

    proc seed p1 {
	global subrand::mod subrand::state subrand::si subrand::sj
	set p2 1
	lset state 0 [expr {$p1 % $mod}]
	for {set i 1; set j 21} {$i < 55} {incr i; incr j 21} {
	    if {$j >= 55} {incr j -55}
	    lset state $j $p2
	    if {[set p2 [expr {$p1 - $p2}]] < 0} {incr p2 $mod}
	    set p1 [lindex $state $j]
	}
	set si 0
	set sj 24
	for {set i 0} {$i < 165} {incr i} { gen }
    }

    proc gen {} {
	global subrand::mod subrand::state subrand::si subrand::sj
	if {$si == $sj} {seed 0}
	if {[incr si -1] < 0} {set si 54}
	if {[incr sj -1] < 0} {set sj 54}
	set x [expr {[lindex $state $si] - [lindex $state $sj]}]
	if {$x < 0} {incr x $mod}
	lset state $si $x
	return $x
    }
}

subrand::seed 292929
for {set i 0} {$i < 10} {incr i} {
    puts [subrand::gen]
}
```



## uBasic/4tH

<lang>Push 292929 : Gosub 100 : d = Pop()

For i = 1 To 10
  Push 0 : Gosub 100
  Print Pop()
Next

End

100 s = Pop()
    If s = 0 Then
       p = (p + 1) % 55
       @(p) = @(p) - @((p + 31) % 55)
       If @(p) < 0 Then
          @(p) = @(p) + 1000000000
       Endif
       Push (@(p)) : Return
    Endif

    @(54) = s : @(33) = 1
    p = 12

    For i = 2 To 54
      @(p) = @((p + 42) % 55) - @((p + 21) % 55)
      If @(p) < 0 Then
          @(p) = @(p) + 1000000000
      Endif
      p = (p + 34) % 55
    Next

    For i = 55 To 219
        Push 0 : Gosub 100 : d = Pop()
    Next

    Push 0 : Return
```

{{out}}

```txt
467478574
512932792
539453717
20349702
615542081
378707948
933204586
824858649
506003769
380969305

0 OK, 0:864

```



## zkl

{{trans|Icon and Unicon}}
{{trans|Python}}
{{trans|C}}

```zkl
fcn rand_sub(x){
   var ring=L(),m=(1e9).toInt();
   mod:='wrap(n){ if(n<0) n+m else n };
   if(not ring){
      seed:=L( (if(vm.numArgs) x else m-1), 1);
      foreach n in ([2 .. 54]){ seed.append((seed[n-2]-seed[n-1]):mod(_)) }
      foreach n in (55){ ring.append(seed[(34*(n+1))%55]) }
      do(220-ring.len()){ self.fcn() } // 165
   }
   ring.append((ring.pop(0)-ring[-24]):mod(_));
   return(ring[-1]);
}
```


```zkl
do(4){ println(rand_sub(292929)) } //seed ignored after first call
```

{{out}}

```txt

467478574
512932792
539453717
20349702

```

