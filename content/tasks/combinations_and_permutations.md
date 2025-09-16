+++
title = "Combinations and permutations"
description = ""
date = 2019-04-23T20:05:46Z
aliases = []
[extra]
id = 13300
[taxonomies]
categories = ["task", "Probability and statistics"]
tags = []
languages = [
  "algol_68",
  "bracmat",
  "c",
  "common_lisp",
  "crystal",
  "d",
  "echolisp",
  "elixir",
  "erlang",
  "go",
  "haskell",
  "j",
  "jq",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "maple",
  "nim",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ruby",
  "scheme",
  "sidef",
  "stata",
  "swift",
  "tcl",
  "vbscript",
  "visual_basic_.net",
]
+++

## Task

Implement the [[wp:Combination|combination]]   <big> (<sup>n</sup>C<sub>k</sub>) </big>   and [[wp:Permutation|permutation]]   <big> (<sup>n</sup>P<sub>k</sub>) </big>   operators in the target language:

:::* <math>^n\operatorname C_k =\binom nk = \frac{n(n-1)\ldots(n-k+1)}{k(k-1)\dots1} </math>

:::* <math>^n\operatorname P_k = n\cdot(n-1)\cdot(n-2)\cdots(n-k+1)</math>



See the Wikipedia articles for a more detailed description.

'''To test''', generate and print examples of:
*   A sample of permutations from 1 to 12 and Combinations from 10 to 60 using exact Integer arithmetic.
*   A sample of permutations from 5 to 15000 and Combinations from 100 to 1000 using approximate Floating point arithmetic.
 This 'floating point' code could be implemented using an approximation, e.g., by calling the [[Gamma function]].


## Related tasks

*   [[Evaluate binomial coefficients]]

## ALGOL 68

'''File: prelude_combinations_and_permutations.a68'''
```algol68
# -*- coding: utf-8 -*- #

COMMENT REQUIRED by "prelude_combinations_and_permutations.a68" CO
  MODE CPINT = #LONG# ~;
  MODE CPOUT = #LONG# ~; # the answer, can be REAL #
  MODE CPREAL = ~; # the answer, can be REAL #
  PROC cp fix value error = (#REF# CPARGS args)BOOL: ~;
#PROVIDES:#
# OP C = (CP~,CP~)CP~: ~ #
# OP P = (CP~,CP~)CP~: ~ #
END COMMENT

MODE CPARGS = STRUCT(CHAR name, #REF# CPINT n,k);

PRIO C = 8, P = 8; # should be 7.5, a priority between *,/ and **,SHL,SHR etc #

# I suspect there is a more reliable way of doing this using the Gamma Function approx #

OP P = (CPINT n, r)CPOUT: (
  IF n < r ORF r < 0 THEN IF NOT cp fix value error(CPARGS("P",n,r)) THEN stop FI FI;
  CPOUT out := 1;
# basically nPk = (n-r+1)(n-r+2)...(n-2)(n-1)n = n!/(n-r)! #
  FOR i FROM n-r+1 TO n DO out *:= i OD;
  out
);

OP P = (CPREAL n, r)CPREAL: # 'ln gamma' requires GSL library #
  exp(ln gamma(n+1)-ln gamma(n-r+1));

# basically nPk = (n-r+1)(n-r+2)...(n-2)(n-1)n = n!/(n-r)! #
COMMENT # alternate slower version #
OP P = (CPREAL n, r)CPREAL: ( # alternate slower version #
  IF n < r ORF r < 0 THEN IF NOT cp fix value error(CPARGS("P",ENTIER n,ENTIER r)) THEN stop FI FI;
  CPREAL out := 1;
# basically nPk = (n-r+1)(n-r+2)...(n-2)(n-1)n = n!/(n-r)! #
  CPREAL i := n-r+1;
  WHILE i <= n DO
    out*:= i;
# a crude check for underflow #
    IF i = i + 1 THEN IF NOT cp fix value error(CPARGS("P",ENTIER n,ENTIER r)) THEN stop FI FI;
    i+:=1
  OD;
  out
);
END COMMENT

# basically C(n,r) = nCk = nPk/r! = n!/(n-r)!/r! #
OP C = (CPINT n, r)CPOUT: (
  IF n < r ORF r < 0 THEN IF NOT cp fix value error(("C",n,r)) THEN stop FI FI;
  CPINT largest = ( r > n - r | r | n - r );
  CPINT smallest = n - largest;
  CPOUT out := 1;
  INT smaller fact := 2;
  FOR larger fact FROM largest+1 TO n DO
# try and prevent overflow, p.s. there must be a smarter way to do this #
#   Problems: loop stalls when 'smaller fact' is a largeish co prime #
    out *:= larger fact;
    WHILE smaller fact <= smallest ANDF out MOD smaller fact = 0 DO
      out OVERAB smaller fact;
      smaller fact +:= 1
    OD
  OD;
  out # EXIT with: n P r OVER r P r #
);

OP C = (CPREAL n, CPREAL r)CPREAL: # 'ln gamma' requires GSL library #
  exp(ln gamma(n+1)-ln gamma(n-r+1)-ln gamma(r+1));

# basically C(n,r) = nCk = nPk/r! = n!/(n-r)!/r! #
COMMENT # alternate slower version #
OP C = (CPREAL n, REAL r)CPREAL: (
  IF n < r ORF r < 0 THEN IF NOT cp fix value error(("C",ENTIER n,ENTIER r)) THEN stop FI FI;
  CPREAL largest = ( r > n - r | r | n - r );
  CPREAL smallest = n - largest;
  CPREAL out := 1;
  REAL smaller fact := 2;
  REAL larger fact := largest+1;
  WHILE larger fact <= n DO # todo: check underflow here #
# try and prevent overflow, p.s. there must be a smarter way to do this #
    out *:= larger fact;
    WHILE smaller fact <= smallest ANDF out > smaller fact DO
      out /:= smaller fact;
      smaller fact +:= 1
    OD;
    larger fact +:= 1
  OD;
  out # EXIT with: n P r OVER r P r #
);
END COMMENT

SKIP
```
'''File: test_combinations_and_permutations.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

CO REQUIRED by "prelude_combinations_and_permutations.a68" CO
  MODE CPINT = #LONG# INT;
  MODE CPOUT = #LONG# INT; # the answer, can be REAL #
  MODE CPREAL = REAL; # the answer, can be REAL #
  PROC cp fix value error = (#REF# CPARGS args)BOOL: (
    putf(stand error, ($"Value error: "g(0)gg(0)"arg out of range"l$,
                         n OF args, name OF args, k OF args));
    FALSE # unfixable #
  );
#PROVIDES:#
# OP C = (CP~,CP~)CP~: ~ #
# OP P = (CP~,CP~)CP~: ~ #
PR READ "prelude_combinations_and_permutations.a68" PR;

printf($"A sample of Permutations from 1 to 12:"l$);
FOR i FROM 4 BY 1 TO 12 DO
  INT first = i - 2,
      second = i - ENTIER sqrt(i);
  printf(($g(0)" P "g(0)" = "g(0)$, i, first, i P first, $", "$));
  printf(($g(0)" P "g(0)" = "g(0)$, i, second, i P second, $l$))
OD;

printf($l"A sample of Combinations from 10 to 60:"l$);
FOR i FROM 10 BY 10 TO 60 DO
  INT first = i - 2,
      second = i - ENTIER sqrt(i);
  printf(($"("g(0)" C "g(0)") = "g(0)$, i, first, i C first, $", "$));
  printf(($"("g(0)" C "g(0)") = "g(0)$, i, second, i C second, $l$))
OD;

printf($l"A sample of Permutations from 5 to 15000:"l$);
FOR i FROM 5 BY 10 TO 150 DO
  REAL r = i,
       first = r - 2,
       second = r - ENTIER sqrt(r);
  printf(($g(0)" P "g(0)" = "g(-real width,real width-5,-1)$, r, first, r P first, $", "$));
  printf(($g(0)" P "g(0)" = "g(-real width,real width-5,-1)$, r, second, r P second, $l$))
OD;

printf($l"A sample of Combinations from 10 to 190:"l$);
FOR i FROM 100 BY 100 TO 1000 DO
  REAL r = i,
       first = r - 2,
       second = r - ENTIER sqrt(r);
  printf(($"("g(0)" C "g(0)") = "g(0,1)$, r, first, r C first, $", "$));
  printf(($"("g(0)" C "g(0)") = "g(0,1)$, r, second, r C second, $l$))
OD
```
'''Output:'''

```txt

A sample of Permutations from 1 to 12:
4 P 2 = 12, 4 P 2 = 12
5 P 3 = 60, 5 P 3 = 60
6 P 4 = 360, 6 P 4 = 360
7 P 5 = 2520, 7 P 5 = 2520
8 P 6 = 20160, 8 P 6 = 20160
9 P 7 = 181440, 9 P 6 = 60480
10 P 8 = 1814400, 10 P 7 = 604800
11 P 9 = 19958400, 11 P 8 = 6652800
12 P 10 = 239500800, 12 P 9 = 79833600

A sample of Combinations from 10 to 60:
(10 C 8) = 45, (10 C 7) = 120
(20 C 18) = 190, (20 C 16) = 4845
(30 C 28) = 435, (30 C 25) = 142506
(40 C 38) = 780, (40 C 34) = 3838380
(50 C 48) = 1225, (50 C 43) = 99884400
(60 C 58) = 1770, (60 C 53) = 386206920

A sample of Permutations from 5 to 15000:
5 P 3 =  6.0000000000e1, 5 P 3 =  6.0000000000e1
15 P 13 =  6.538371840e11, 15 P 12 =  2.179457280e11
25 P 23 =  7.755605022e24, 25 P 20 =  1.292600837e23
35 P 33 =  5.166573983e39, 35 P 30 =  8.610956639e37
45 P 43 =  5.981111043e55, 45 P 39 =  1.661419734e53
55 P 53 =  6.348201677e72, 55 P 48 =  2.519127650e69
65 P 63 =  4.123825296e90, 65 P 57 =  2.045548262e86
75 P 73 =  1.24045704e109, 75 P 67 =  6.15306072e104
85 P 83 =  1.40855206e128, 85 P 76 =  7.76318374e122
95 P 93 =  5.16498924e147, 95 P 86 =  2.84666515e142
105 P 103 =  5.40698379e167, 105 P 95 =  2.98003957e161
115 P 113 =  1.46254685e188, 115 P 105 =  8.06077407e181
125 P 123 =  9.41338588e208, 125 P 114 =  4.71650327e201
135 P 133 =  1.34523635e230, 135 P 124 =  6.74020139e222
145 P 143 =  4.02396303e251, 145 P 133 =  1.68014597e243

A sample of Combinations from 10 to 190:
(100 C 98) = 4950.0, (100 C 90) = 17310309456438.8
(200 C 198) = 19900.0, (200 C 186) = 1179791641436960000000.0
(300 C 298) = 44850.0, (300 C 283) = 2287708142022840000000000000.0
(400 C 398) = 79800.0, (400 C 380) = 2788360983670300000000000000000000.0
(500 C 498) = 124750.0, (500 C 478) = 132736424690773000000000000000000000000.0
(600 C 598) = 179700.0, (600 C 576) = 4791686682467800000000000000000000000000000.0
(700 C 698) = 244650.0, (700 C 674) = 145478651313640000000000000000000000000000000000.0
(800 C 798) = 319600.0, (800 C 772) = 3933526871034430000000000000000000000000000000000000.0
(900 C 898) = 404550.0, (900 C 870) = 98033481673646900000000000000000000000000000000000000000.0
(1000 C 998) = 499500.0, (1000 C 969) = 76023224077705100000000000000000000000000000000000000000000.0

```



## Bracmat

Bracmat cannot handle floating point numbers. Instead, this solution shows the first 50 digits and a count of the digits that are not shown.

```Bracmat
( ( C
  =   n k coef
    .   !arg:(?n,?k)
      & (!n+-1*!k:<!k:?k|)
      & 1:?coef
      &   whl
        ' ( !k:>0
          & !coef*!n*!k^-1:?coef
          & !k+-1:?k
          & !n+-1:?n
          )
      & !coef
  )
& ( P
  =   n k result
    .   !arg:(?n,?k)
      & !n+-1*!k:?k
      & 1:?result
      &   whl
        ' ( !n:>!k
          & !n*!result:?result
          & !n+-1:?n
          )
      & !result
  )
& 0:?i
&   whl
  ' ( 1+!i:~>12:?i
    & div$(!i.3):?k
    & out$(!i P !k "=" P$(!i,!k))
    )
& 0:?i
&   whl
  ' ( 10+!i:~>60:?i
    & div$(!i.3):?k
    & out$(!i C !k "=" C$(!i,!k))
    )
& ( displayBig
  =
    .     @(!arg:?show [50 ? [?length)
        & !show "... (" !length+-50 " more digits)"
      | !arg
  )
& 5 50 500 1000 5000 15000:?is
&   whl
  ' ( !is:%?i ?is
    & div$(!i.3):?k
    & out$(str$(!i " P " !k " = " displayBig$(P$(!i,!k))))
    )
& 0:?i
&   whl
  ' ( 100+!i:~>1000:?i
    & div$(!i.3):?k
    & out$(str$(!i " C " !k " = " displayBig$(C$(!i,!k))))
    )
);
```

Output:

```txt
1 P 0 = 1
2 P 0 = 1
3 P 1 = 3
4 P 1 = 4
5 P 1 = 5
6 P 2 = 30
7 P 2 = 42
8 P 2 = 56
9 P 3 = 504
10 P 3 = 720
11 P 3 = 990
12 P 4 = 11880
10 C 3 = 120
20 C 6 = 38760
30 C 10 = 30045015
40 C 13 = 12033222880
50 C 16 = 4923689695575
60 C 20 = 4191844505805495
5 P 1 = 5
50 P 16 = 103017324974226408345600000
500 P 166 = 35348749217429427876093618266017623068440028791060... (385 more digits)
1000 P 333 = 59693262885034150890397017659007842809981894765670... (922 more digits)
5000 P 1666 = 68567457572556742754845369402488960622341567102448... (5976 more digits)
15000 P 5000 = 96498539887274939220148588059312959807922816886808... (20420 more digits)
100 C 33 = 294692427022540894366527900
200 C 66 = 72697525451692783415270666511927389767550269141935... (4 more digits)
300 C 100 = 41582514632585647447833835263264055802804660057436... (32 more digits)
400 C 133 = 12579486841821087021333484756519650044917494358375... (60 more digits)
500 C 166 = 39260283861944227552204083450723314281973490135301... (87 more digits)
600 C 200 = 25060177832214028050056167705132288352025510250879... (115 more digits)
700 C 233 = 81032035633395999047404536440311382329449203119421... (142 more digits)
800 C 266 = 26456233626836270342888292995561242550915240450150... (170 more digits)
900 C 300 = 17433563732964466429607307650857183476303419689548... (198 more digits)
1000 C 333 = 57761345531476516697774863235496017223394580195002... (225 more digits)
```



## C

Using big integers. GMP in fact has a factorial function which is quite possibly more efficient, though using it would make code longer.

```c
#include <gmp.h>

void perm(mpz_t out, int n, int k)
{
	mpz_set_ui(out, 1);
	k = n - k;
	while (n > k) mpz_mul_ui(out, out, n--);
}

void comb(mpz_t out, int n, int k)
{
	perm(out, n, k);
	while (k) mpz_divexact_ui(out, out, k--);
}

int main(void)
{
	mpz_t x;
	mpz_init(x);

	perm(x, 1000, 969);
	gmp_printf("P(1000,969) = %Zd\n", x);

	comb(x, 1000, 969);
	gmp_printf("C(1000,969) = %Zd\n", x);
	return 0;
}
```



## Common Lisp



```Lisp
(defun combinations (n k)
  (cond ((or (< n k) (< k 0) (< n 0)) 0)
	((= k 0) 1)
	(t (do* ((i 1 (1+ i))
		 (m n (1- m))
		 (a m (* a m))
		 (b i (* b i)))
	       ((= i k) (/ a b))))))

(defun permutations (n k)
  (cond ((or (< n k) (< k 0) (< n 0)) 0)
	((= k 0) 1)
	(t (do* ((i 1 (1+ i))
		 (m n (1- m))
		 (a m (* a m)))
	       ((= i k) a)))))

```



## Crystal

```ruby
require "big"
include Math

struct Int

  def permutation(k)
    (self-k+1..self).product(1.to_big_i)
  end

  def combination(k)
    self.permutation(k) / (1..k).product(1.to_big_i)
  end

  def big_permutation(k)
    exp(lgamma_plus(self) - lgamma_plus(self-k))
  end

  def big_combination(k)
    exp( lgamma_plus(self) - lgamma_plus(self - k) - lgamma_plus(k))
  end

  private def lgamma_plus(n)
    lgamma(n+1)  #lgamma is the natural log of gamma
  end

end

p 12.permutation(9)               #=> 79833600
p 12.big_permutation(9)           #=> 79833600.00000021
p 60.combination(53)              #=> 386206920
p 145.big_permutation(133)        #=> 1.6801459655817956e+243
p 900.big_combination(450)        #=> 2.247471882064647e+269
p 1000.big_combination(969)       #=> 7.602322407770517e+58
p 15000.big_permutation(73)       #=> 6.004137561717704e+304
#That's about the maximum of Float:
p 15000.big_permutation(74)       #=> Infinity
#Fixnum has no maximum:
p 15000.permutation(74)           #=> 896237613852967826239917238565433149353074416025197784301593335243699358040738127950872384197159884905490054194835376498534786047382445592358843238688903318467070575184552953997615178973027752714539513893159815472948987921587671399790410958903188816684444202526779550201576117111844818124800000000000000000000


```

```txt

79833600
79833600.00000021
386206920
1.6801459655817956e+243
2.247471882064647e+269
7.602322407770517e+58
6.004137561717704e+304
Infinity
896237613852967826239917238565433149353074416025197784301593335243699358040738127950872384197159884905490054194835376498534786047382445592358843238688903318467070575184552953997615178973027752714539513893159815472948987921587671399790410958903188816684444202526779550201576117111844818124800000000000000000000

```



## D

```d
import std.stdio, std.mathspecial, std.range, std.algorithm,
       std.bigint, std.conv;

enum permutation = (in uint n, in uint k) pure =>
    reduce!q{a * b}(1.BigInt, iota(n - k + 1, n + 1));

enum combination = (in uint n, in uint k) pure =>
    n.permutation(k) / reduce!q{a * b}(1.BigInt, iota(1, k + 1));

enum bigPermutation = (in uint n, in uint k) =>
    exp(logGamma(n + 1) - logGamma(n - k + 1));

enum bigCombination = (in uint n, in uint k) =>
    exp(logGamma(n + 1) - logGamma(n - k + 1) - logGamma(k + 1));

void main() {
    12.permutation(9).writeln;
    12.bigPermutation(9).writeln;
    60.combination(53).writeln;
    145.bigPermutation(133).writeln;
    900.bigCombination(450).writeln;
    1_000.bigCombination(969).writeln;
    15_000.bigPermutation(73).writeln;
    15_000.bigPermutation(1185).writeln;
    writefln("%(%s\\\n%)", 15_000.permutation(74).text.chunks(50));
}
```

```txt
79833600
7.98336e+07
386206920
1.68015e+243
2.24747e+269
7.60232e+58
6.00414e+304
6.31335e+4927
89623761385296782623991723856543314935307441602519\
77843015933352436993580407381279508723841971598849\
05490054194835376498534786047382445592358843238688\
90331846707057518455295399761517897302775271453951\
38931598154729489879215876713997904109589031888166\
84444202526779550201576117111844818124800000000000\
000000000
```



## EchoLisp


```scheme

;; rename native functions according to task
(define-syntax-rule (Cnk n k) (Cnp n k))
(define-syntax-rule (Ank n k) (Anp n k))


(Cnk 10 1)
    → 10
(lib 'bigint) ;; no floating point needed : use large integers

(Cnk 100 10)
    → 17310309456440
(Cnk 1000 42)
    → 297242911333923795640059429176065863139989673213703918037987737481286092000
(Ank 10 10)
    → 3628800
(factorial 10)
    → 3628800
(Ank 666 42)
    → 1029024198692120734765388598788124551227594950478035495578451793852872815678512303375588360
1398831219998720000000000000

```



## Elixir

```elixir
defmodule Combinations_permutations do
  def perm(n, k), do: product(n - k + 1 .. n)

  def comb(n, k), do: div( perm(n, k), product(1 .. k) )

  defp product(a..b) when a>b, do: 1
  defp product(list), do: Enum.reduce(list, 1, fn n, acc -> n * acc end)

  def test do
    IO.puts "\nA sample of permutations from 1 to 12:"
    Enum.each(1..12, &show_perm(&1, div(&1, 3)))
    IO.puts "\nA sample of combinations from 10 to 60:"
    Enum.take_every(10..60, 10) |> Enum.each(&show_comb(&1, div(&1, 3)))
    IO.puts "\nA sample of permutations from 5 to 15000:"
    Enum.each([5,50,500,1000,5000,15000], &show_perm(&1, div(&1, 3)))
    IO.puts "\nA sample of combinations from 100 to 1000:"
    Enum.take_every(100..1000, 100) |> Enum.each(&show_comb(&1, div(&1, 3)))
  end

  defp show_perm(n, k), do: show_gen(n, k, "perm", &perm/2)

  defp show_comb(n, k), do: show_gen(n, k, "comb", &comb/2)

  defp show_gen(n, k, strfun, fun), do:
    IO.puts "#{strfun}(#{n}, #{k}) = #{show_big(fun.(n, k), 40)}"

  defp show_big(n, limit) do
    strn = to_string(n)
    if String.length(strn) < limit do
      strn
    else
      {shown, hidden} = String.split_at(strn, limit)
      "#{shown}... (#{String.length(hidden)} more digits)"
    end
  end
end

Combinations_permutations.test
```


```txt

A sample of permutations from 1 to 12:
perm(1, 0) = 1
perm(2, 0) = 1
perm(3, 1) = 3
perm(4, 1) = 4
perm(5, 1) = 5
perm(6, 2) = 30
perm(7, 2) = 42
perm(8, 2) = 56
perm(9, 3) = 504
perm(10, 3) = 720
perm(11, 3) = 990
perm(12, 4) = 11880

A sample of combinations from 10 to 60:
comb(10, 3) = 120
comb(20, 6) = 38760
comb(30, 10) = 30045015
comb(40, 13) = 12033222880
comb(50, 16) = 4923689695575
comb(60, 20) = 4191844505805495

A sample of permutations from 5 to 15000:
perm(5, 1) = 5
perm(50, 16) = 103017324974226408345600000
perm(500, 166) = 3534874921742942787609361826601762306844... (395 more digits)
perm(1000, 333) = 5969326288503415089039701765900784280998... (932 more digits)
perm(5000, 1666) = 6856745757255674275484536940248896062234... (5986 more digits)
perm(15000, 5000) = 9649853988727493922014858805931295980792... (20430 more digits)

A sample of combinations from 100 to 1000:
comb(100, 33) = 294692427022540894366527900
comb(200, 66) = 7269752545169278341527066651192738976755... (14 more digits)
comb(300, 100) = 4158251463258564744783383526326405580280... (42 more digits)
comb(400, 133) = 1257948684182108702133348475651965004491... (70 more digits)
comb(500, 166) = 3926028386194422755220408345072331428197... (97 more digits)
comb(600, 200) = 2506017783221402805005616770513228835202... (125 more digits)
comb(700, 233) = 8103203563339599904740453644031138232944... (152 more digits)
comb(800, 266) = 2645623362683627034288829299556124255091... (180 more digits)
comb(900, 300) = 1743356373296446642960730765085718347630... (208 more digits)
comb(1000, 333) = 5776134553147651669777486323549601722339... (235 more digits)

```



## Erlang

```erlang

-module(combinations_permutations).

-export([test/0]).

perm(N, K) ->
    product(lists:seq(N - K + 1, N)).

comb(N, K) ->
    perm(N, K) div product(lists:seq(1, K)).

product(List) ->
    lists:foldl(fun(N, Acc) -> N * Acc end, 1, List).

test() ->
    io:format("\nA sample of permutations from 1 to 12:\n"),
    [show_perm({N, N div 3}) || N <- lists:seq(1, 12)],
    io:format("\nA sample of combinations from 10 to 60:\n"),
    [show_comb({N, N div 3}) || N <- lists:seq(10, 60, 10)],
    io:format("\nA sample of permutations from 5 to 15000:\n"),
    [show_perm({N, N div 3}) || N <- [5,50,500,1000,5000,15000]],
    io:format("\nA sample of combinations from 100 to 1000:\n"),
    [show_comb({N, N div 3}) || N <- lists:seq(100, 1000, 100)],
    ok.

show_perm({N, K}) ->
    show_gen(N, K, "perm", fun perm/2).

show_comb({N, K}) ->
    show_gen(N, K, "comb", fun comb/2).

show_gen(N, K, StrFun, Fun) ->
    io:format("~s(~p, ~p) = ~s\n",[StrFun, N, K, show_big(Fun(N, K), 40)]).

show_big(N, Limit) ->
    StrN = integer_to_list(N),
    case length(StrN) < Limit of
        true ->
            StrN;
        false ->
            {Shown, Hidden} = lists:split(Limit, StrN),
            io_lib:format("~s... (~p more digits)", [Shown, length(Hidden)])
    end.

```


Output:

```txt
A sample of permutations from 1 to 12:
perm(1, 0) = 1
perm(2, 0) = 1
perm(3, 1) = 3
perm(4, 1) = 4
perm(5, 1) = 5
perm(6, 2) = 30
perm(7, 2) = 42
perm(8, 2) = 56
perm(9, 3) = 504
perm(10, 3) = 720
perm(11, 3) = 990
perm(12, 4) = 11880

A sample of combinations from 10 to 60:
comb(10, 3) = 120
comb(20, 6) = 38760
comb(30, 10) = 30045015
comb(40, 13) = 12033222880
comb(50, 16) = 4923689695575
comb(60, 20) = 4191844505805495

A sample of permutations from 5 to 15000:
perm(5, 1) = 5
perm(50, 16) = 103017324974226408345600000
perm(500, 166) = 3534874921742942787609361826601762306844... (395 more digits)
perm(1000, 333) = 5969326288503415089039701765900784280998... (932 more digits)
perm(5000, 1666) = 6856745757255674275484536940248896062234... (5986 more digits)
perm(15000, 5000) = 9649853988727493922014858805931295980792... (20430 more digits)

A sample of combinations from 100 to 1000:
comb(100, 33) = 294692427022540894366527900
comb(200, 66) = 7269752545169278341527066651192738976755... (14 more digits)
comb(300, 100) = 4158251463258564744783383526326405580280... (42 more digits)
comb(400, 133) = 1257948684182108702133348475651965004491... (70 more digits)
comb(500, 166) = 3926028386194422755220408345072331428197... (97 more digits)
comb(600, 200) = 2506017783221402805005616770513228835202... (125 more digits)
comb(700, 233) = 8103203563339599904740453644031138232944... (152 more digits)
comb(800, 266) = 2645623362683627034288829299556124255091... (180 more digits)
comb(900, 300) = 1743356373296446642960730765085718347630... (208 more digits)
comb(1000, 333) = 5776134553147651669777486323549601722339... (235 more digits)

```



## Go

Go has arbitrary-length maths in the standard math/big library; no need for floating-point approximations at this level.

```go

package main

import (
	"fmt"
	"math/big"
)

func main() {
	var n, p int64
	fmt.Printf("A sample of permutations from 1 to 12:\n")
	for n = 1; n < 13; n++ {
		p = n / 3
		fmt.Printf("P(%d,%d) = %d\n", n, p, perm(big.NewInt(n), big.NewInt(p)))
	}
	fmt.Printf("\nA sample of combinations from 10 to 60:\n")
	for n = 10; n < 61; n += 10 {
		p = n / 3
		fmt.Printf("C(%d,%d) = %d\n", n, p, comb(big.NewInt(n), big.NewInt(p)))
	}
	fmt.Printf("\nA sample of permutations from 5 to 15000:\n")
	nArr := [...]int64{5, 50, 500, 1000, 5000, 15000}
	for _, n = range nArr {
		p = n / 3
		fmt.Printf("P(%d,%d) = %d\n", n, p, perm(big.NewInt(n), big.NewInt(p)))
	}
	fmt.Printf("\nA sample of combinations from 100 to 1000:\n")
	for n = 100; n < 1001; n += 100 {
		p = n / 3
		fmt.Printf("C(%d,%d) = %d\n", n, p, comb(big.NewInt(n), big.NewInt(p)))
	}
}

func fact(n *big.Int) *big.Int {
	if n.Sign() < 1 {
		return big.NewInt(0)
	}
	r := big.NewInt(1)
	i := big.NewInt(2)
	for i.Cmp(n) < 1 {
		r.Mul(r, i)
		i.Add(i, big.NewInt(1))
	}
	return r
}

func perm(n, k *big.Int) *big.Int {
	r := fact(n)
	r.Div(r, fact(n.Sub(n, k)))
	return r
}

func comb(n, r *big.Int) *big.Int {
	if r.Cmp(n) == 1 {
		return big.NewInt(0)
	}
	if r.Cmp(n) == 0 {
		return big.NewInt(1)
	}
	c := fact(n)
	den := fact(n.Sub(n, r))
	den.Mul(den, fact(r))
	c.Div(c, den)
	return c
}

```

Output:

```txt
A sample of permutations from 1 to 12:
P(1,0) = 1
P(2,0) = 1
P(3,1) = 3
P(4,1) = 4
P(5,1) = 5
P(6,2) = 30
P(7,2) = 42
P(8,2) = 56
P(9,3) = 504
P(10,3) = 720
P(11,3) = 990
P(12,4) = 11880

A sample of combinations from 10 to 60:
C(10,3) = 120
C(20,6) = 38760
C(30,10) = 30045015
C(40,13) = 12033222880
C(50,16) = 4923689695575
C(60,20) = 4191844505805495

A sample of permutations from 5 to 15000:
P(5,1) = 5
P(50,16) = 103017324974226408345600000
P(500,166) = 353487492174294278760936182660176230684400287910609032932176169251145051223056590013516735448538086130105926216996155913554025250125337170813019594283712354977999534430770809915541863294344717641926832713607917635838943102385935821177067602075180371673503765613359169210620516084434587852075431010684540863423686685437846488590916158047347611875355166780660833377163468853354607169353747005440000000000000000000000000000000000000000000
P(1000,333) = 596932628850341508903970176590078428099818947656708993181003187015748137551596090897395098770177006143741943000028735297176540996715216223470117008188290845824893667956971794591724165149886070514986743432208287422668258597883938335639789463537748828480742878588232053442156529356123644254034620250998013239063050800571901268462622323786888857845397534124006543754044331948142416200311556601875197681493636464229808874876131434533721937546154413110195799580966669097512255452752067457629468146295647406985227990184437533735467426422585063839734564755495294687791091277426861971582062496498138090957027659529000216472781766770467939751274131497380920782541878343751496719223481266229644276166815775979972070711062842180838624567600323874889368489920534418201154996144776696986749038638993937273035934558675329887334851616951614782042039326589303359315137179445599073086196250614391361241088000000000000000000000000000000000000000000000000000000000000000000000000000000000000
P(5000,1666) = 685674575725567427548453694024889606223415671024481949274389525763165050940278456955930987049278913288741555958551429473866794447094151777406284837935079289013939976368320856320075398810321021264609367677596630017492662476653457696667818345302032079158497998678962485919859205035598026552321633034034950067207086196635562039504231368320682342175536748176816241129369211418155064764285518551122913802314264757797230065345837585112169021234001053819461909997341171332285483908469793127022101882927191957584158820572953454348525922376264896938701700503579[cut for brevity]
P(15000,5000) = 964985398872749392201485880593129598079228168868086089709069882934904327326041434448685068557423519588782990996839148935743422697293448340149394869739166392123184483900811562453898657822593544251663672426442538772790754374376708208571908721887559582227891355612508675254239851818491849618848602192999811776790423150460291684931309968390568370095954400889615967984376006072547506893620140168365357740946620575769516153753735678390720704316317251046076250691800213698137901479234159657387339301806781869228964210268691780310799227446113809960921205463002689680686581917548426454723087176124161728203856922785131850458595700123281745279678944233081992533251647291980003523846698046974475328766195265764029672828687155439288711515137638802309221117722696141292043413736618293499869841412502567093112620576877254953509763190120514494690844850900732795773648193199871673637389424514221019995024902360799221264651063770768194819355718580618775786102220736660007307021195929713637615667876228769039112504881578385857681786265840651390718446642041061982261630233585694136722143048873223923363402206453540375800805584706454386378553117095912890292078608807203370350094921116703330118357576158786202069866488676813049328910886961677198798213135372964228023956951408480830806905842004749290253667472325116488994202675719755525469268037451046892994419861436389890344224919839492952550142350743[cut for brevity]

A sample of combinations from 100 to 1000:
C(100,33) = 294692427022540894366527900
C(200,66) = 726975254516927834152706665119273897675502691419359300
C(300,100) = 4158251463258564744783383526326405580280466005743648708663033657304756328324008620
C(400,133) = 12579486841821087021333484756519650044917494358375678689903944407062661887691782714561293494166382210572895600
C(500,166) = 39260283861944227552204083450723314281973490135301528555644859907093815055309467400410307687655531369748267877388321068535356654999949000
C(600,200) = 250601778322140280500561677051322883520255102508793389473094343332441398315528846878026090182866148274621477126087990864486283260622128340138769443055475567389095596
C(700,233) = 810320356333959990474045364403113823294492031194214481466666874664632951279413378341573227990559808332117096088495529108312831240206642074673862825014526456696556162909686658807978793453220000
C(800,266) = 2645623362683627034288829299556124255091524045015061559880110850588357798837813526621954238671844949835878984342140544523564918954064307529607727040078866833961879433579846596361407958192581870170248962672479120257018000
C(900,300) = 17433563732964466429607307650857183476303419689548896834573896609295907147982188408607179689430757813632301567003238290404591375673438892792913992016448098783043956457942357838233534288303678577768883350818012950587783262800434058273110416350965200
C(1000,333) = 57761345531476516697774863235496017223394580195002114171799054793660405129610824218694609475292335509897216233568933163108481350037180876217607177657236327948642711456536116349650593787069554795812874358426845137087373717847050642650744775784499136594696491030795647099932000

```



## Haskell

The Haskell Integer type supports arbitrary precision so floating point approximation is not needed.

```haskell
perm :: Integer -> Integer -> Integer
perm n k = product [n-k+1..n]

comb :: Integer -> Integer -> Integer
comb n k = perm n k `div` product [1..k]

main :: IO ()
main = do
        let showBig maxlen b =
                let st = show b
                    stlen = length st
                in if stlen < maxlen then st else take maxlen st ++ "... ("  ++ show (stlen-maxlen) ++  " more digits)"

        let showPerm pr =
                putStrLn $ "perm(" ++ show n ++ "," ++ show k ++ ") = "  ++ showBig 40 (perm n k)
                where n = fst pr
                      k = snd pr

        let showComb pr =
                putStrLn $ "comb(" ++ show n ++ "," ++ show k ++ ") = "  ++ showBig 40 (comb n k)
                where n = fst pr
                      k = snd pr

        putStrLn "A sample of permutations from 1 to 12:"
        mapM_  showPerm [(n, n `div` 3) | n <- [1..12] ]

        putStrLn ""
        putStrLn "A sample of combinations from 10 to 60:"
        mapM_  showComb [(n, n `div` 3) | n <- [10,20..60] ]

        putStrLn ""
        putStrLn "A sample of permutations from 5 to 15000:"
        mapM_  showPerm [(n, n `div` 3) | n <- [5,50,500,1000,5000,15000] ]

        putStrLn ""
        putStrLn "A sample of combinations from 100 to 1000:"
        mapM_  showComb [(n, n `div` 3) | n <- [100,200..1000] ]


```


```txt
A sample of permutations from 1 to 12:
perm(1,0) = 1
perm(2,0) = 1
perm(3,1) = 3
perm(4,1) = 4
perm(5,1) = 5
perm(6,2) = 30
perm(7,2) = 42
perm(8,2) = 56
perm(9,3) = 504
perm(10,3) = 720
perm(11,3) = 990
perm(12,4) = 11880

A sample of combinations from 10 to 60:
comb(10,3) = 120
comb(20,6) = 38760
comb(30,10) = 30045015
comb(40,13) = 12033222880
comb(50,16) = 4923689695575
comb(60,20) = 4191844505805495

A sample of permutations from 5 to 15000:
perm(5,1) = 5
perm(50,16) = 103017324974226408345600000
perm(500,166) = 3534874921742942787609361826601762306844... (395 more digits)
perm(1000,333) = 5969326288503415089039701765900784280998... (932 more digits)
perm(5000,1666) = 6856745757255674275484536940248896062234... (5986 more digits)
perm(15000,5000) = 9649853988727493922014858805931295980792... (20430 more digits)

A sample of combinations from 100 to 1000:
comb(100,33) = 294692427022540894366527900
comb(200,66) = 7269752545169278341527066651192738976755... (14 more digits)
comb(300,100) = 4158251463258564744783383526326405580280... (42 more digits)
comb(400,133) = 1257948684182108702133348475651965004491... (70 more digits)
comb(500,166) = 3926028386194422755220408345072331428197... (97 more digits)
comb(600,200) = 2506017783221402805005616770513228835202... (125 more digits)
comb(700,233) = 8103203563339599904740453644031138232944... (152 more digits)
comb(800,266) = 2645623362683627034288829299556124255091... (180 more digits)
comb(900,300) = 1743356373296446642960730765085718347630... (208 more digits)
comb(1000,333) = 5776134553147651669777486323549601722339... (235 more digits)

```


=={{header|Icon}} and {{header|Unicon}}==

As with several other languages here, Icon and Unicon can handle unlimited integers so
floating point approximation isn't needed.
The sample here gives a few representative values to shorten the output.


```unicon
procedure main()
    write("P(4,2) = ",P(4,2))
    write("P(8,2) = ",P(8,2))
    write("P(10,8) = ",P(10,8))
    write("C(10,8) = ",C(10,8))
    write("C(20,8) = ",C(20,8))
    write("C(60,58) = ",C(60,58))
    write("P(1000,10) = ",P(1000,10))
    write("P(1000,20) = ",P(1000,20))
    write("P(15000,2) = ",P(15000,2))
    write("C(1000,10) = ",C(1000,10))
    write("C(1000,999) = ",C(1000,999))
    write("C(1000,1000) = ",C(1000,1000))
    write("C(15000,14998) = ",C(15000,14998))
end

procedure C(n,k)
    every (d:=1) *:= 2 to k
    return P(n,k)/d
end

procedure P(n,k)
    every (p:=1) *:= (n-k+1) to n
    return p
end
```


Output:

```txt

->cap
P(4,2) = 12
P(8,2) = 56
P(10,8) = 1814400
C(10,8) = 45
C(20,8) = 125970
C(60,58) = 1770
P(1000,10) = 955860613004397508326213120000
P(1000,20) = 825928413359200443640727373872992573951185652339949568000000
P(15000,2) = 224985000
C(1000,10) = 263409560461970212832400
C(1000,999) = 1000
C(1000,1000) = 1
C(15000,14998) = 112492500
->

```



## J


It looks like this task wants a count of the available combinations or permutations (given a set of 3 things, there are three distinct combinations of 2 of them) rather than a representation of the available combinations or permutations (given a set of three things, the distinct combinations of 2 of them may be identified by <0,1>, <0,2> and <1,2>)).

Also, this task allows a language to show off its abilities to support floating point numbers outside the usual range of 64 bit IEEE floating point numbers. We'll neglect that part.

Implementation:


```J
C=: !
P=: (%&!&x:~ * <:)"0
```


! is a primitive, but we will give it a name (<code>C</code>) for this task.

Example use (P is permutations, C is combinations):


```J
   P table 1+i.12
┌──┬─────────────────────────────────────────────────────────────┐
│P │1 2 3  4   5   6    7     8      9      10       11        12│
├──┼─────────────────────────────────────────────────────────────┤
│ 1│1 2 6 24 120 720 5040 40320 362880 3628800 39916800 479001600│
│ 2│0 1 3 12  60 360 2520 20160 181440 1814400 19958400 239500800│
│ 3│0 0 1  4  20 120  840  6720  60480  604800  6652800  79833600│
│ 4│0 0 0  1   5  30  210  1680  15120  151200  1663200  19958400│
│ 5│0 0 0  0   1   6   42   336   3024   30240   332640   3991680│
│ 6│0 0 0  0   0   1    7    56    504    5040    55440    665280│
│ 7│0 0 0  0   0   0    1     8     72     720     7920     95040│
│ 8│0 0 0  0   0   0    0     1      9      90      990     11880│
│ 9│0 0 0  0   0   0    0     0      1      10      110      1320│
│10│0 0 0  0   0   0    0     0      0       1       11       132│
│11│0 0 0  0   0   0    0     0      0       0        1        12│
│12│0 0 0  0   0   0    0     0      0       0        0         1│
└──┴─────────────────────────────────────────────────────────────┘

   C table 10+10*i.6x
┌──┬─────────────────────────────────────────────────────────────────┐
│C │10     20       30           40             50                 60│
├──┼─────────────────────────────────────────────────────────────────┤
│10│ 1 184756 30045015    847660528    10272278170        75394027566│
│20│ 0      1 30045015 137846528820 47129212243960   4191844505805495│
│30│ 0      0        1    847660528 47129212243960 118264581564861424│
│40│ 0      0        0            1    10272278170   4191844505805495│
│50│ 0      0        0            0              1        75394027566│
│60│ 0      0        0            0              0                  1│
└──┴─────────────────────────────────────────────────────────────────┘

   5 P 100
7.77718e155
   100 P 200
8.45055e216
   300 P 400
2.09224e254
   700 P 800
3.18349e287

   5 C 100
75287520
   100 C 200
9.05485e58
   300 C 400
2.24185e96
   700 C 800
3.41114e129
```



## jq

Currently, jq approximates large integers by IEEE 754 64-bit floats, and only supports tgamma (true gamma). Thus beyond about 1e308 all accuracy is lost.

```jq
def permutation(k): . as $n
  | reduce range($n-k+1; 1+$n) as $i (1; . * $i);

def combination(k): . as $n
  | if k > ($n/2) then combination($n-k)
    else reduce range(0; k) as $i (1; (. * ($n - $i)) / ($i + 1))
    end;

# natural log of n!
def log_factorial: (1+.) | tgamma | log;

def log_permutation(k):
  (log_factorial - ((.-k) | log_factorial));

def log_combination(k):
  (log_factorial - ((. - k)|log_factorial) - (k|log_factorial));

def big_permutation(k): log_permutation(k) | exp;

def big_combination(k): log_combination(k) | exp;
```

'''Examples''':
 12 | permutation(9)               #=> 79833600

 12 | big_permutation(9)           #=> 79833599.99999964

 60 | combination(53)              #=> 386206920

 60 | big_combination(53)          #=> 386206920.0000046

 145 | big_permutation(133)        #=> 1.6801459655817e+243

 170 | big_permutation(133)        #=> 5.272846415658284e+263


## Julia

Infix operators are interpreted by Julia's parser, and (to my knowledge) it isn't possible to define arbitrary characters as such operators.  However one can define Unicode "Symbol, Math" characters as infix operators.  This solution uses ⊞ for combinations and ⊠ for permutations.  An alternative to using the <tt>FloatingPoint</tt> versions of these operators for large numbers would be to use arbitrary precision integers, <tt>BigInt</tt>.

'''Functions'''

```Julia

function Base.binomial{T<:FloatingPoint}(n::T, k::T)
    exp(lfact(n) - lfact(n - k) - lfact(k))
end

function Base.factorial{T<:FloatingPoint}(n::T, k::T)
    exp(lfact(n) - lfact(k))
end

⊞{T<:Real}(n::T, k::T) = binomial(n, k)
⊠{T<:Real}(n::T, k::T) = factorial(n, n-k)

```


'''Main'''

```Julia

function picknk{T<:Integer}(lo::T, hi::T)
    n = rand(lo:hi)
    k = rand(1:n)
    return (n, k)
end

nsamp = 10

print("Tests of the combinations (⊞) and permutations (⊠) operators for ")
println("integer values.")
println()
lo, hi = 1, 12
print(nsamp, " samples of n ⊠ k with n in [", lo, ", ", hi, "] ")
println("and k in [1, n].")
for i in 1:nsamp
    (n, k) = picknk(lo, hi)
    println(@sprintf "    %2d ⊠ %2d = %18d" n k n ⊠ k)
end

lo, hi = 10, 60
println()
print(nsamp, " samples of n ⊞ k with n in [", lo, ", ", hi, "] ")
println("and k in [1, n].")
for i in 1:nsamp
    (n, k) = picknk(lo, hi)
    println(@sprintf "    %2d ⊞ %2d = %18d" n k n ⊞ k)
end

println()
print("Tests of the combinations (⊞) and permutations (⊠) operators for ")
println("(big) float values.")
println()
lo, hi = 5, 15000
print(nsamp, " samples of n ⊠ k with n in [", lo, ", ", hi, "] ")
println("and k in [1, n].")
for i in 1:nsamp
    (n, k) = picknk(lo, hi)
    n = BigFloat(n)
    k = BigFloat(k)
    println(@sprintf "    %7.1f ⊠ %7.1f = %10.6e" n k n ⊠ k)
end

lo, hi = 100, 1000
println()
print(nsamp, " samples of n ⊞ k with n in [", lo, ", ", hi, "] ")
println("and k in [1, n].")
for i in 1:nsamp
    (n, k) = picknk(lo, hi)
    n = BigFloat(n)
    k = BigFloat(k)
    println(@sprintf "    %7.1f ⊞ %7.1f = %10.6e" n k n ⊞ k)
end

```


```txt

Tests of the combinations (⊞) and permutations (⊠) operators for integer values.

10 samples of n ⊠ k with n in [1, 12] and k in [1, n].
     4 ⊠  2 =                 12
     9 ⊠  2 =                 72
     2 ⊠  1 =                  2
     8 ⊠  2 =                 56
     7 ⊠  5 =               2520
     4 ⊠  2 =                 12
     9 ⊠  8 =             362880
    11 ⊠  6 =             332640
     1 ⊠  1 =                  1
     8 ⊠  5 =               6720

10 samples of n ⊞ k with n in [10, 60] and k in [1, n].
    58 ⊞ 26 =  22150361247847371
    22 ⊞ 21 =                 22
    32 ⊞ 30 =                496
    11 ⊞  4 =                330
    32 ⊞ 29 =               4960
    16 ⊞  7 =              11440
    31 ⊞ 25 =             736281
    13 ⊞ 13 =                  1
    43 ⊞ 28 =       151532656696
    49 ⊞ 37 =        92263734836

Tests of the combinations (⊞) and permutations (⊠) operators for (big) float values.

10 samples of n ⊠ k with n in [5, 15000] and k in [1, n].
     8375.0 ⊠  5578.0 = 2.200496e+20792
     1556.0 ⊠   592.0 = 1.313059e+1833
     1234.0 ⊠   910.0 = 2.231762e+2606
    12531.0 ⊠  9361.0 = 2.339542e+36188
    12418.0 ⊠  6119.0 = 1.049662e+24251
     9435.0 ⊠  8960.0 = 4.273644e+32339
     9430.0 ⊠  5385.0 = 1.471741e+20551
     9876.0 ⊠  5386.0 = 9.073417e+20712
      941.0 ⊠   911.0 = 8.689430e+2358
     8145.0 ⊠  4357.0 = 1.368129e+16407

10 samples of n ⊞ k with n in [100, 1000] and k in [1, n].
      757.0 ⊞   237.0 = 6.813837e+202
      457.0 ⊞   413.0 = 4.816707e+61
      493.0 ⊞   372.0 = 8.607443e+117
      206.0 ⊞    26.0 = 6.911828e+32
      432.0 ⊞   300.0 = 1.248351e+114
      650.0 ⊞   469.0 = 3.284854e+165
      203.0 ⊞   115.0 = 1.198089e+59
      583.0 ⊞   429.0 = 5.700279e+144
      329.0 ⊞    34.0 = 2.225630e+46
      464.0 ⊞   178.0 = 5.615925e+132

```



## Kotlin

As Kotlin/JVM can use the java.math.BigInteger class, there is no need to use floating point approximations and so we use exact integer arithmetic for all parts of the task.

```scala
// version 1.1.2

import java.math.BigInteger

fun perm(n: Int, k: Int): BigInteger {
    require(n > 0 && k >= 0)
    return (n - k + 1 .. n).fold(BigInteger.ONE) { acc, i -> acc * BigInteger.valueOf(i.toLong()) }
}

fun comb(n: Int, k: Int): BigInteger {
    require(n > 0 && k >= 0)
    val fact = (2..k).fold(BigInteger.ONE) { acc, i -> acc * BigInteger.valueOf(i.toLong()) }
    return perm(n, k) / fact
}

fun main(args: Array<String>) {
    println("A sample of permutations from 1 to 12:")
    for (n in 1..12) System.out.printf("%2d P %-2d = %d\n", n, n / 3, perm(n, n / 3))

    println("\nA sample of combinations from 10 to 60:")
    for (n in 10..60 step 10) System.out.printf("%2d C %-2d = %d\n", n, n / 3, comb(n, n / 3))

    println("\nA sample of permutations from 5 to 15000:")
    val na = intArrayOf(5, 50, 500, 1000, 5000, 15000)
    for (n in na) {
        val k = n / 3
        val s = perm(n, k).toString()
        val l = s.length
        val e = if (l <= 40) "" else "... (${l - 40} more digits)"
        System.out.printf("%5d P %-4d = %s%s\n", n, k, s.take(40), e)
    }

    println("\nA sample of combinations from 100 to 1000:")
    for (n in 100..1000 step 100) {
        val k = n / 3
        val s = comb(n, k).toString()
        val l = s.length
        val e = if (l <= 40) "" else "... (${l - 40} more digits)"
        System.out.printf("%4d C %-3d = %s%s\n", n, k, s.take(40), e)
    }
}
```


```txt

A sample of permutations from 1 to 12:
 1 P 0  = 1
 2 P 0  = 1
 3 P 1  = 3
 4 P 1  = 4
 5 P 1  = 5
 6 P 2  = 30
 7 P 2  = 42
 8 P 2  = 56
 9 P 3  = 504
10 P 3  = 720
11 P 3  = 990
12 P 4  = 11880

A sample of combinations from 10 to 60:
10 C 3  = 120
20 C 6  = 38760
30 C 10 = 30045015
40 C 13 = 12033222880
50 C 16 = 4923689695575
60 C 20 = 4191844505805495

A sample of permutations from 5 to 15000:
    5 P 1    = 5
   50 P 16   = 103017324974226408345600000
  500 P 166  = 3534874921742942787609361826601762306844... (395 more digits)
 1000 P 333  = 5969326288503415089039701765900784280998... (932 more digits)
 5000 P 1666 = 6856745757255674275484536940248896062234... (5986 more digits)
15000 P 5000 = 9649853988727493922014858805931295980792... (20430 more digits)

A sample of combinations from 100 to 1000:
 100 C 33  = 294692427022540894366527900
 200 C 66  = 7269752545169278341527066651192738976755... (14 more digits)
 300 C 100 = 4158251463258564744783383526326405580280... (42 more digits)
 400 C 133 = 1257948684182108702133348475651965004491... (70 more digits)
 500 C 166 = 3926028386194422755220408345072331428197... (97 more digits)
 600 C 200 = 2506017783221402805005616770513228835202... (125 more digits)
 700 C 233 = 8103203563339599904740453644031138232944... (152 more digits)
 800 C 266 = 2645623362683627034288829299556124255091... (180 more digits)
 900 C 300 = 1743356373296446642960730765085718347630... (208 more digits)
1000 C 333 = 5776134553147651669777486323549601722339... (235 more digits)

```


## M2000 Interpreter


```M2000 Interpreter

Module PermComb {
      Form 80, 50
      perm=lambda (x,y) ->{
            def i,z
            z=1
            For i=x-y+1 to x :z*=i:next i
           =z
      }
      fact=lambda (x) ->{
            def i,z
            z=1
            For i=2 to x :z*=i:next i
            =z
      }
      comb=lambda  (x as decimal, y as decimal) ->{
            If y>x then {
                  =0
            } else.if x=y then  {
                  =1
            } else {
                  if x-y<y then y=x-y
                  def decimal  i, z=1, ym
                  ym=y
                  For i=x to x-y+1
                        z*=i
                        z=z/ym
                       ym-- : if ym<1 then ym=1@
                  next  i
                  =round(z,0)

            }
      }
      Document Doc$
      WriteLn("-- Permutations - from 1 to 12")
      For i=1 to 12
      l$="" : For j=1 to i : l$+= format$("P({0},{1})={2}  ",i, j,perm(i, j)) :next j
      Writetext(l$)
      next i
      WriteLn("-- Combinations from 10 to 60")
      For i=10  to 60 step 10
      l$="" : For j=1 to i step i div 5 : l$+= format$("C({0},{1})={2}  ",i, j,comb(i, j)) :next j
      Writetext(l$)
      Next i
      WriteLn("-- Permutations from 5000 to 15000")
      For i=5000 to 15000 step 5000
      l$="" : For j=10 to 70  step  20: l$+= format$("P({0},{1})={2}  ",i, j,perm(i, j)) :next j
      Writetext(l$)
      Next i
      WriteLn("-- Combinations from 200 to 1000")
      For i=200 to 1000 step 200
      l$="" : For j=20 to 100 step 20: l$+= format$("C({0},{1})={2}  ",i, j,comb(i, j)) :next j
      Writetext(l$)
      Next i
      ClipBoard Doc$
      Sub WriteText(a$)
      doc$=a$+{
      }
      Report a$
      End Sub
      Sub WriteLn(a$)
      doc$=a$+{
      }
      Print a$
      End Sub
}
PermComb

```

-- Permutations - from 1 to 12

P(1,1)=1
P(2,1)=2  P(2,2)=2
P(3,1)=3  P(3,2)=6  P(3,3)=6
P(4,1)=4  P(4,2)=12  P(4,3)=24  P(4,4)=24
P(5,1)=5  P(5,2)=20  P(5,3)=60  P(5,4)=120  P(5,5)=120
P(6,1)=6  P(6,2)=30  P(6,3)=120  P(6,4)=360  P(6,5)=720  P(6,6)=720
P(7,1)=7  P(7,2)=42  P(7,3)=210  P(7,4)=840  P(7,5)=2520  P(7,6)=5040  P(7,7)=5040
P(8,1)=8  P(8,2)=56  P(8,3)=336  P(8,4)=1680  P(8,5)=6720  P(8,6)=20160  P(8,7)=40320  P(8,8)=40320
P(9,1)=9  P(9,2)=72  P(9,3)=504  P(9,4)=3024  P(9,5)=15120  P(9,6)=60480  P(9,7)=181440  P(9,8)=362880  P(9,9)=362880
P(10,1)=10  P(10,2)=90  P(10,3)=720  P(10,4)=5040  P(10,5)=30240  P(10,6)=151200  P(10,7)=604800  P(10,8)=1814400  P(10,9)=3628800  P(10,10)=3628800
P(11,1)=11  P(11,2)=110  P(11,3)=990  P(11,4)=7920  P(11,5)=55440  P(11,6)=332640  P(11,7)=1663200  P(11,8)=6652800  P(11,9)=19958400  P(11,10)=39916800  P(11,11)=39916800
P(12,1)=12  P(12,2)=132  P(12,3)=1320  P(12,4)=11880  P(12,5)=95040  P(12,6)=665280  P(12,7)=3991680  P(12,8)=19958400  P(12,9)=79833600  P(12,10)=239500800  P(12,11)=479001600  P(12,12)=479001600

-- Combinations from 10 to 60

C(10,1)=10  C(10,3)=120  C(10,5)=252  C(10,7)=120  C(10,9)=10
C(20,1)=20  C(20,5)=15504  C(20,9)=167960  C(20,13)=77520  C(20,17)=1140
C(30,1)=30  C(30,7)=2035800  C(30,13)=119759850  C(30,19)=54627300  C(30,25)=142506
C(40,1)=40  C(40,9)=273438880  C(40,17)=88732378800  C(40,25)=40225345056  C(40,33)=18643560
C(50,1)=50  C(50,11)=37353738800  C(50,21)=67327446062800  C(50,31)=30405943383200  C(50,41)=2505433700
C(60,1)=60  C(60,13)=5166863427600  C(60,25)=51915437974328292  C(60,37)=23385332420868600  C(60,49)=342700125300

-- Permutations from 5000 to 15000

P(5000,10)=9,67807348145655E+36  P(5000,30)=8,53575581200676E+110  P(5000,50)=6,94616656703754E+184  P(5000,70)=5,21383580146195E+258
P(10000,10)=9,95508690556325E+39  P(10000,30)=9,57391540294832E+119  P(10000,50)=8,84526658067387E+199  P(10000,70)=7,850079552152E+279
P(15000,10)=5,74922667554068E+41  P(15000,30)=1,86266591363916E+125  P(15000,50)=5,87565776023335E+208  P(15000,70)=1,80450662858719E+292

-- Combinations from 200 to 1000

C(200,20)=1613587787967350073386147640  C(200,40)=721126811024990370  C(200,60)=286107190317772000463240955  C(200,80)=900482  C(200,100)=478205104
C(400,20)=3558235073  C(400,40)=130321  C(400,60)=74780600187861332802765  C(400,80)=9521248771125  C(400,100)=447355513982663594791392
C(600,20)=2801445153584  C(600,40)=266319106596345  C(600,60)=14409368913  C(600,80)=271441  C(600,100)=52868467287780595308
C(800,20)=1925279023672620  C(800,40)=23121069591511231041  C(800,60)=18702067923763447158  C(800,80)=1193559552292625  C(800,100)=172727802
C(1000,20)=1239329180287869852  C(1000,40)=1937726921514640866484017  C(1000,60)=149470629867337347460963500  C(1000,80)=1269150275532146867313740  C(1000,100)=10088410532027029794548


## Maple


```Maple

comb := proc (n::integer, k::integer)
    return factorial(n)/(factorial(k)*factorial(n-k));
end proc;
perm := proc (n::integer, k::integer)
    return factorial(n)/factorial(n-k);
end proc;

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
ClearAll[Combination,Permutation]
Combination[n_,k_]:=Binomial[n,k]
Permutation[n_,k_]:=Binomial[n,k]k!

TableForm[Array[Permutation,{12,12}],TableHeadings->{Range[12],Range[12]}]
TableForm[Array[Combination,{6,6},{{10,60},{10,60}}],TableHeadings->{Range[10,60,10],Range[10,60,10]}]
{Row[{#,"P",#-2},"  "],N@Permutation[#,#-2]}&/@{5,1000,5000,10000,15000}//Grid
{Row[{#,"C",#/2},"  "],N@Combination[#,#/2]}&/@Range[100,1000,100]//Grid
```


```txt

 	1	2	3	4	5	6	7	8		9		10		11		12
1	1	0	0	0	0	0	0	0		0		0		0		0
2	2	2	0	0	0	0	0	0		0		0		0		0
3	3	6	6	0	0	0	0	0		0		0		0		0
4	4	12	24	24	0	0	0	0		0		0		0		0
5	5	20	60	120	120	0	0	0		0		0		0		0
6	6	30	120	360	720	720	0	0		0		0		0		0
7	7	42	210	840	2520	5040	5040	0		0		0		0		0
8	8	56	336	1680	6720	20160	40320	40320		0		0		0		0
9	9	72	504	3024	15120	60480	181440	362880		362880		0		0		0
10	10	90	720	5040	30240	151200	604800	1814400		3628800		3628800		0		0
11	11	110	990	7920	55440	332640	1663200	6652800		19958400	39916800	39916800	0
12	12	132	1320	11880	95040	665280	3991680	19958400	79833600	239500800	479001600	479001600

 	10		20			30			40			50		60
10	1		0			0			0			0		0
20	184756		1			0			0			0		0
30	30045015	30045015		1			0			0		0
40	847660528	137846528820		847660528		1			0		0
50	10272278170	47129212243960		47129212243960		10272278170		1		0
60	75394027566	4191844505805495	118264581564861424	4191844505805495	75394027566	1

5  P  3			60.
1000  P  998		2.011936300385469*10^2567
5000  P  4998		2.114288963302772*10^16325
10000  P  9998		1.423129840458527*10^35659
15000  P  14998		1.373299516742584*10^56129

100  C  50	1.00891*10^29
200  C  100	9.05485*10^58
300  C  150	9.37597*10^88
400  C  200	1.02953*10^119
500  C  250	1.16744*10^149
600  C  300	1.35108*10^179
700  C  350	1.58574*10^209
800  C  400	1.88042*10^239
900  C  450	2.24747*10^269
1000  C  500	2.70288*10^299

```

Note that Mathematica can easily handle very big numbers with exact integer arithmetic:

```Mathematica

Permutation[200000, 100000]

```

The output is 516777 digits longs:

```txt

50287180689616781338617355322585606........0321815299686400000000000000000000......(lots of zeroes)

```


=={{header|MK-61/52}}==
<lang>П2	<->	П1	->	<->	П7	КПП7	С/П
ИП1	ИП2	-	ПП	53	П3	ИП1	ПП	53	ИП3	/	В/О
1	ИП1	*	L2	21	В/О
ИП1	ИП2	-	ПП	53	П3	ИП2	ПП	53	ИП3	*	П3	ИП1	ПП	53	ИП3	/	В/О
ИП1	ИП2	+	1	-	П1	ПП	26	В/О
ВП    П0    1    ИП0    *    L0    56    В/О
```


''Input'': ''x ^ n ^ k В/О С/П'', where ''x'' = 8 for permutations; 20 for permutations with repetitions; 26 for combinations; 44 for combinations with repetitions.

Printing of test cases is performed incrementally, which is associated with the characteristics of the device output.


## Nim


```nim
import bigints

proc perm(n, k: int32): BigInt =
  result = initBigInt 1
  var
    k = n - k
    n = n
  while n > k:
    result *= n
    dec n

proc comb(n, k: int32): BigInt =
  result = perm(n, k)
  var k = k
  while k > 0:
    result = result div k
    dec k

echo "P(1000, 969) = ", perm(1000, 969)
echo "C(1000, 969) = ", comb(1000, 969)
```



## PARI/GP


```parigp
sample(f,a,b)=for(i=1,4, my(n1=random(b-a)+a,n2=random(b-a)+a); [n1,n2]=[max(n1,n2),min(n1,n2)]; print(n1", "n2": "f(n1,n2)))
permExact(m,n)=factorback([m-n+1..m]);
combExact=binomial;
permApprox(m,n)=exp(lngamma(m+1)-lngamma(m-n+1));
combApprox(m,n)=exp(lngamma(m+1)-lngamma(n+1)-lngamma(m-n+1));

sample(permExact, 1, 12);
sample(combExact, 10, 60);
sample(permApprox, 5, 15000);
sample(combApprox, 100, 1000);
```

```txt
?sample(permExact, 1, 12);
8, 2: 56
11, 8: 6652800
9, 9: 362880
6, 1: 6
?sample(combExact, 10, 60);
46, 14: 239877544005
34, 22: 548354040
51, 20: 77535155627160
49, 26: 58343356817424
?sample(permApprox, 5, 15000);
8374, 8306: 6.6786635386843773562533982329356314192 E29119
4064, 2497: 7.7325589445068984950461595444827041944 E8575
13234, 784: 1.3439405881429921844444755481930625437 E3221
14136, 1523: 9.7219281356264565060667995087812528666 E6283
?sample(combApprox, 100, 1000);
988, 702: 4.1430346142101709187524161097370204275 E256
861, 225: 1.9423942269910057792279495652023745087 E213
580, 350: 4.9721729266474994835623000459244303642 E167
977, 846: 6.0586575447000334467351859308510379521 E165
```



## Perl

Although perl can handle arbitrarily large numbers using Math::BigInt and Math::BigFloat, it's
native integers and floats are limited to what the computer's native types can handle.

As with the perl6 code, some special handling was done for those values which would have overflowed the native floating point type.


```perl
use strict;
use warnings;

showoff( "Permutations", \&P, "P", 1 .. 12 );
showoff( "Combinations", \&C, "C", map $_*10, 1..6 );
showoff( "Permutations", \&P_big, "P", 5, 50, 500, 1000, 5000, 15000 );
showoff( "Combinations", \&C_big, "C", map $_*100, 1..10 );

sub showoff {
	my ($text, $code, $fname, @n) = @_;
	print "\nA sample of $text from $n[0] to $n[-1]\n";
	for my $n ( @n ) {
		my $k = int( $n / 3 );
		print $n, " $fname $k = ", $code->($n, $k), "\n";
	}
}

sub P {
	my ($n, $k) = @_;
	my $x = 1;
	$x *= $_ for $n - $k + 1 .. $n ;
	$x;
}

sub P_big {
	my ($n, $k) = @_;
	my $x = 0;
	$x += log($_) for $n - $k + 1 .. $n ;
	eshow($x);
}

sub C {
	my ($n, $k) = @_;
	my $x = 1;
	$x *= ($n - $_ + 1) / $_ for 1 .. $k;
	$x;
}

sub C_big {
	my ($n, $k) = @_;
	my $x = 0;
	$x += log($n - $_ + 1) - log($_) for 1 .. $k;
	exp($x);
}

sub eshow {
	my ($x) = @_;
	my $e = int( $x / log(10) );
	sprintf "%.8Fe%+d", exp($x - $e * log(10)), $e;
}

```


Since the output is almost the same as perl6's, and this is only a Draft RosettaCode task, I'm not going to bother including the output of the program.


## Perl 6

Perl 6 can't compute arbitrary large floating point values, thus we will use logarithms, as is often needed when dealing with combinations.   We'll also use a Stirling method to approximate <math>\ln(n!)</math>:

<math>\ln n! \approx
\frac{1}{2}\ln(2\pi n) + n\ln\left(\frac{n}{e} + \frac{1}{12 e n}\right)</math>

Notice that Perl6 can process arbitrary long integers, though.  So it's not clear whether using floating points is useful in this case.


```perl6
multi P($n, $k) { [*] $n - $k + 1 .. $n }
multi C($n, $k) { P($n, $k) / [*] 1 .. $k }

sub lstirling(\n) {
    n < 10 ?? lstirling(n+1) - log(n+1) !!
    .5*log(2*pi*n)+ n*log(n/e+1/(12*e*n))
}

role Logarithm {
    method gist {
	my $e = (self/10.log).Int;
	sprintf "%.8fE%+d", exp(self - $e*10.log), $e;
    }
}
multi P($n, $k, :$float!) {
    (lstirling($n) - lstirling($n -$k))
    but Logarithm
}
multi C($n, $k, :$float!) {
    (lstirling($n) - lstirling($n -$k) - lstirling($k))
    but Logarithm
}

say "Exact results:";
for 1..12 -> $n {
    my $p = $n div 3;
    say "P($n, $p) = ", P($n, $p);
}

for 10, 20 ... 60 -> $n {
    my $p = $n div 3;
    say "C($n, $p) = ", C($n, $p);
}

say '';
say "Floating point approximations:";
for 5, 50, 500, 1000, 5000, 15000 -> $n {
    my $p = $n div 3;
    say "P($n, $p) = ", P($n, $p, :float);
}

for 100, 200 ... 1000 -> $n {
    my $p = $n div 3;
    say "C($n, $p) = ", C($n, $p, :float);
}
```

```txt
Exact results:
P(1, 0) = 1
P(2, 0) = 1
P(3, 1) = 3
P(4, 1) = 4
P(5, 1) = 5
P(6, 2) = 30
P(7, 2) = 42
P(8, 2) = 56
P(9, 3) = 504
P(10, 3) = 720
P(11, 3) = 990
P(12, 4) = 11880
C(10, 3) = 120
C(20, 6) = 38760
C(30, 10) = 30045015
C(40, 13) = 12033222880
C(50, 16) = 4923689695575
C(60, 20) = 4191844505805495

Floating point approximations:
P(5, 1) = 5.00000000E+0
P(50, 16) = 1.03017326E+26
P(500, 166) = 3.53487492E+434
P(1000, 333) = 5.96932629E+971
P(5000, 1666) = 6.85674576E+6025
P(15000, 5000) = 9.64985399E+20469
C(100, 33) = 2.94692433E+26
C(200, 66) = 7.26975256E+53
C(300, 100) = 4.15825147E+81
C(400, 133) = 1.25794868E+109
C(500, 166) = 3.92602839E+136
C(600, 200) = 2.50601778E+164
C(700, 233) = 8.10320356E+191
C(800, 266) = 2.64562336E+219
C(900, 300) = 1.74335637E+247
C(1000, 333) = 5.77613455E+274
```



## Phix

Translation of Perl 6/Sidef, same results

Update: there are now builtin routines k_perm(n,k) and choose(n,k), slightly more efficient equivalents of P() and C() respectively.

```Phix
function P(integer n,k)
    return factorial(n)/factorial(n-k)
end function

function C(integer n,k)
    return P(n,k)/factorial(k)
end function

function lstirling(atom n)
    if n<10 then
        return lstirling(n+1)-log(n+1)
    end if
    return 0.5*log(2*PI*n) + n*log(n/E + 1/(12*E*n))
end function

function P_approx(integer n, k)
    return lstirling(n)-lstirling(n-k)
end function

function C_approx(integer n, k)
    return lstirling(n)-lstirling(n-k)-lstirling(k)
end function

function to_s(atom v)
integer e = floor(v/log(10))
    return sprintf("%.9ge%d",{power(E,v-e*log(10)),e})
end function
```

Test code

```Phix
printf(1,"=> Exact results:\n")
for n=1 to 12 do
    integer p = floor(n/3)
    printf(1,"P(%d,%d) = %d\n",{n,p,P(n,p)})
end for

for n=10 to 60 by 10 do
    integer p = floor(n/3)
    printf(1,"C(%d,%d) = %d\n",{n,p,C(n,p)})
end for

printf(1,"=> Floating point approximations:\n")
constant tests = {5, 50, 500, 1000, 5000, 15000}
for i=1 to length(tests) do
    integer n=tests[i], p = floor(n/3)
    printf(1,"P(%d,%d) = %s\n",{n,p,to_s(P_approx(n,p))})
end for

for n=100 to 1000 by 100 do
    integer p = floor(n/3)
    printf(1,"C(%d,%d) = %s\n",{n,p,to_s(C_approx(n,p))})
end for
```



## Python

==={{libheader|SciPy}}===

```python
from __future__ import print_function

from scipy.misc import factorial as fact
from scipy.misc import comb

def perm(N, k, exact=0):
    return comb(N, k, exact) * fact(k, exact)

exact=True
print('Sample Perms 1..12')
for N in range(1, 13):
    k = max(N-2, 1)
    print('%iP%i =' % (N, k), perm(N, k, exact), end=', ' if N % 5 else '\n')

print('\n\nSample Combs 10..60')
for N in range(10, 61, 10):
    k = N-2
    print('%iC%i =' % (N, k), comb(N, k, exact), end=', ' if N % 50 else '\n')

exact=False
print('\n\nSample Perms 5..1500 Using FP approximations')
for N in [5, 15, 150, 1500, 15000]:
    k = N-2
    print('%iP%i =' % (N, k), perm(N, k, exact))

print('\nSample Combs 100..1000 Using FP approximations')
for N in range(100, 1001, 100):
    k = N-2
    print('%iC%i =' % (N, k), comb(N, k, exact))

```


```txt
Sample Perms 1..12
1P1 = 1, 2P1 = 2, 3P1 = 3, 4P2 = 12, 5P3 = 60
6P4 = 360, 7P5 = 2520, 8P6 = 20160, 9P7 = 181440, 10P8 = 1814400
11P9 = 19958400, 12P10 = 239500800,

Sample Combs 10..60
10C8 = 45, 20C18 = 190, 30C28 = 435, 40C38 = 780, 50C48 = 1225
60C58 = 1770,

Sample Perms 5..1500 Using FP approximations
5P3 = 60.0
15P13 = 653837184000.0
150P148 = 2.85669197822e+262
1500P1498 = inf
15000P14998 = inf

Sample Combs 100..1000 Using FP approximations
100C98 = 4950.0
200C198 = 19900.0
300C298 = 44850.0
400C398 = 79800.0
500C498 = 124750.0
600C598 = 179700.0
700C698 = 244650.0
800C798 = 319600.0
900C898 = 404550.0
1000C998 = 499500.0
```



## Racket


Racket's "math" library has two functions that compute nCk and nPk.
They work only on integers, but since Racket supports unlimited integers
there is no need for a floating point estimate:


```Racket

#lang racket
(require math)
(define C binomial)
(define P permutations)

(C 1000 10) ; -> 263409560461970212832400
(P 1000 10) ; -> 955860613004397508326213120000

```


(I'll spare this page from yet another big listing of samples...)


## REXX

The hard part of this REXX program was coding the   '''DO'''   loops for the various ranges.

```rexx
/*REXX program  compute and displays  a sampling of  combinations  and  permutations.   */
numeric digits 100                               /*use 100 decimal digits of precision. */

      do      j=1  for  12;                _=    /*show all permutations from  1 ──► 12.*/
           do k=1  for   j                       /*step through all  J  permutations.   */
           _=_  'P('j","k')='perm(j,k)" "        /*add an extra blank between numbers.  */
           end       /*k*/
      say strip(_)                               /*show the permutations horizontally.  */
      end           /*j*/
say                                              /*display a blank line for readability.*/
      do      j=10  to  60  by 10;         _=    /*show some combinations  10 ──►  60.  */
           do k= 1  to   j  by j%5               /*step through some combinations.      */
           _=_  'C('j","k')='comb(j,k)" "        /*add an extra blank between numbers.  */
           end   /*k*/
      say strip(_)                               /*show the combinations horizontally.  */
      end           /*j*/
say                                              /*display a blank line for readability.*/
numeric digits 20                                /*force floating point for big numbers.*/

      do      j=5  to 15000      by 1000;  _=    /*show a few permutations, big numbers.*/
           do k=1  to  j  for 5  by j%10         /*step through some  J  permutations.  */
           _=_  'P('j","k')='perm(j,k)" "        /*add an extra blank between numbers.  */
           end      /*k*/
      say strip(_)                               /*show the permutations horizontally.  */
      end           /*j*/
say                                              /*display a blank line for readability.*/
      do      j=100  to 1000  by 100;      _=    /*show a few combinations, big numbers.*/
           do k=  1  to    j  by j%5             /*step through some combinations.      */
           _=_  'C('j","k')='comb(j,k)" "        /*add an extra blank between numbers.  */
           end      /*k*/
      say strip(_)                               /*show the combinations horizontally.  */
      end           /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
perm:      procedure; parse arg x,y;      call .combPerm;                         return _
.combPerm:                          _=1;    do j=x-y+1  to x;    _=_*j;  end;     return _
!:         procedure; parse arg x;  !=1;    do j=2      to x;    !=!*j;  end;     return !
/*──────────────────────────────────────────────────────────────────────────────────────*/
comb:      procedure; parse arg x,y              /*arguments:  X  things,  Y  at-a-time.*/
                      if y   >x   then return 0  /*oops-say, an error,  too big a chunk.*/
                      if x   =y   then return 1  /*X  things are the same as chunk size.*/
                      if x-y <y   then y=x - y   /*switch things around for speed.      */
                      call .combPerm             /*call subroutine to do heavy lifting. */
                      return _ / !(y)            /*just perform one last division.      */
```

'''output'''

```txt

P(1,1)=1
P(2,1)=2  P(2,2)=2
P(3,1)=3  P(3,2)=6  P(3,3)=6
P(4,1)=4  P(4,2)=12  P(4,3)=24  P(4,4)=24
P(5,1)=5  P(5,2)=20  P(5,3)=60  P(5,4)=120  P(5,5)=120
P(6,1)=6  P(6,2)=30  P(6,3)=120  P(6,4)=360  P(6,5)=720  P(6,6)=720
P(7,1)=7  P(7,2)=42  P(7,3)=210  P(7,4)=840  P(7,5)=2520  P(7,6)=5040  P(7,7)=5040
P(8,1)=8  P(8,2)=56  P(8,3)=336  P(8,4)=1680  P(8,5)=6720  P(8,6)=20160  P(8,7)=40320  P(8,8)=40320
P(9,1)=9  P(9,2)=72  P(9,3)=504  P(9,4)=3024  P(9,5)=15120  P(9,6)=60480  P(9,7)=181440  P(9,8)=362880  P(9,9)=362880
P(10,1)=10  P(10,2)=90  P(10,3)=720  P(10,4)=5040  P(10,5)=30240  P(10,6)=151200  P(10,7)=604800  P(10,8)=1814400  P(10,9)=3628800  P(10,10)=3628800
P(11,1)=11  P(11,2)=110  P(11,3)=990  P(11,4)=7920  P(11,5)=55440  P(11,6)=332640  P(11,7)=1663200  P(11,8)=6652800  P(11,9)=19958400  P(11,10)=39916800  P(11,11)=39916800
P(12,1)=12  P(12,2)=132  P(12,3)=1320  P(12,4)=11880  P(12,5)=95040  P(12,6)=665280  P(12,7)=3991680  P(12,8)=19958400  P(12,9)=79833600  P(12,10)=239500800  P(12,11)=479001600  P(12,12)=479001600

C(10,1)=10  C(10,3)=120  C(10,5)=252  C(10,7)=120  C(10,9)=10
C(20,1)=20  C(20,5)=15504  C(20,9)=167960  C(20,13)=77520  C(20,17)=1140
C(30,1)=30  C(30,7)=2035800  C(30,13)=119759850  C(30,19)=54627300  C(30,25)=142506
C(40,1)=40  C(40,9)=273438880  C(40,17)=88732378800  C(40,25)=40225345056  C(40,33)=18643560
C(50,1)=50  C(50,11)=37353738800  C(50,21)=67327446062800  C(50,31)=30405943383200  C(50,41)=2505433700
C(60,1)=60  C(60,13)=5166863427600  C(60,25)=51915437974328292  C(60,37)=23385332420868600  C(60,49)=342700125300

P(5,1)=5  P(5,1)=5  P(5,1)=5  P(5,1)=5  P(5,1)=5
P(1005,1)=1005  P(1005,101)=9.1176524923776877363E+300  P(1005,201)=1.2772738260896333926E+594  P(1005,301)=6.9244021230613662196E+881  P(1005,401)=2.4492580742838357278E+1163
P(2005,1)=2005  P(2005,201)=1.6533543480914610058E+659  P(2005,401)=3.0753126526205309249E+1305  P(2005,601)=7.9852540678709597130E+1940  P(2005,801)=8.0516979630356802995E+2563
P(3005,1)=3005  P(3005,301)=1.1935689764209015622E+1040  P(3005,601)=1.5619600532077469150E+2062  P(3005,901)=1.0291767405881430479E+3068  P(3005,1201)=1.5669988662999668720E+4055
P(4005,1)=4005  P(4005,401)=4.3808609526948101266E+1435  P(4005,801)=2.3060742016678396933E+2848  P(4005,1201)=2.2044072986703009755E+4239  P(4005,1601)=2.8973897505902543204E+5605
P(5005,1)=5005  P(5005,501)=1.4180262672357809801E+1842  P(5005,1001)=2.8239356430641573722E+3656  P(5005,1501)=3.6832518654277810594E+5443  P(5005,2001)=3.9303728189857603162E+7199
P(6005,1)=6005  P(6005,601)=2.4482219222979658097E+2257  P(6005,1201)=1.0247320583108167487E+4482  P(6005,1801)=1.0131515875595211375E+6674  P(6005,2401)=4.8762853043004294329E+8828
P(7005,1)=7005  P(7005,701)=7.6900396347210828241E+2679  P(7005,1401)=1.2659048848290269952E+5322  P(7005,2101)=1.7753130713788487191E+7926  P(7005,2801)=7.2114365718218704695E+10486
P(8005,1)=8005  P(8005,801)=3.9769062582658855959E+3108  P(8005,1601)=4.3272401446508603181E+6174  P(8005,2401)=1.4466844005282015778E+9197  P(8005,3201)=8.3354759867215982278E+12169
P(9005,1)=9005  P(9005,901)=5.6135384805755901099E+3542  P(9005,1801)=1.1194389175552115248E+7038  P(9005,2701)=2.4737530806300682806E+10484  P(9005,3601)=5.6056491332873455398E+13874
P(10005,1)=10005  P(10005,1001)=5.3580936683833889197E+3981  P(10005,2001)=1.3407350644082770778E+7911  P(10005,3001)=1.3407953461588097193E+11786  P(10005,4001)=8.1811569565040010437E+15598
P(11005,1)=11005  P(11005,1101)=1.1340564277915775963E+4425  P(11005,2201)=7.9753039151558717610E+8792  P(11005,3301)=8.0842724022079710248E+13100  P(11005,4401)=2.9749926937463675736E+17340
P(12005,1)=12005  P(12005,1201)=2.1391703159775094656E+4872  P(12005,2401)=3.7994859265471124812E+9682  P(12005,3601)=3.5081603331307865953E+14427  P(12005,4801)=6.9968644993020337359E+19097
P(13005,1)=13005  P(13005,1301)=1.6832659142209713962E+5323  P(13005,2601)=3.1719005022749408296E+10579  P(13005,3901)=1.1206120643200361213E+15765  P(13005,5201)=5.0883658993886790178E+20869
P(14005,1)=14005  P(14005,1401)=2.9074578200382556975E+5777  P(14005,2801)=1.2835011192416517281E+11483  P(14005,4201)=3.8312600202917546343E+17112  P(14005,5601)=8.7456467698123057261E+22654

C(100,1)=100  C(100,21)=2.0418414110621321255E+21  C(100,41)=2.0116440213369968048E+28  C(100,61)=9.0139240300346304925E+27  C(100,81)=1.3234157293921226741E+20
C(200,1)=200  C(200,41)=8.0006165666286406037E+42  C(200,81)=2.4404128184470558197E+57  C(200,121)=1.0891098528606695394E+57  C(200,161)=5.0935602365182339252E+41
C(300,1)=300  C(300,61)=3.5574671252567510894E+64  C(300,121)=3.3878557197772169409E+86  C(300,181)=1.5098730832156289128E+86  C(300,241)=2.2510943427454545270E+63
C(400,1)=400  C(400,81)=1.6703771503944415835E+86  C(400,161)=4.9770797199515347150E+115  C(400,241)=2.2166247162163128334E+115  C(400,321)=1.0537425948749981954E+85
C(500,1)=500  C(500,101)=8.0859177660929770887E+107  C(500,201)=7.5447012685604958486E+144  C(500,301)=3.3587706644089915087E+144  C(500,401)=5.0915068227892187414E+106
C(600,1)=600  C(600,121)=3.9913554739811382543E+129  C(600,241)=1.1667430218545073615E+174  C(600,361)=5.1927067085306791157E+173  C(600,481)=2.5101559893540422495E+128
C(700,1)=700  C(700,141)=1.9971304197729039382E+151  C(700,281)=1.8294137562513979560E+203  C(700,421)=8.1403842518866639077E+202  C(700,561)=1.2548814134936695871E+150
C(800,1)=800  C(800,161)=1.0093242166331589874E+173  C(800,321)=2.8977104736455704539E+232  C(800,481)=1.2892100651978213666E+232  C(800,641)=6.3378002682503352943E+171
C(900,1)=900  C(900,181)=5.1402207737939392620E+194  C(900,361)=4.6256239559539226532E+261  C(900,541)=2.0577328996911473555E+261  C(900,721)=3.2260054093505652100E+193
C(1000,1)=1000  C(1000,201)=2.6336937554862900107E+216  C(1000,401)=7.4293352412781479131E+290  C(1000,601)=3.3046738011675400053E+290  C(1000,801)=1.6522236106515115238E+215

```



## Ruby

Float calculation as Tcl.

```ruby
include Math

class Integer

  def permutation(k)
    (self-k+1 .. self).inject( :*)
  end

  def combination(k)
    self.permutation(k) / (1 .. k).inject( :*)
  end

  def big_permutation(k)
    exp( lgamma_plus(self) - lgamma_plus(self -k))
  end

  def big_combination(k)
    exp( lgamma_plus(self) - lgamma_plus(self - k) - lgamma_plus(k))
  end

  private
  def lgamma_plus(n)
    lgamma(n+1)[0]  #lgamma is the natural log of gamma
  end

end

p 12.permutation(9)               #=> 79833600
p 12.big_permutation(9)           #=> 79833600.00000021
p 60.combination(53)              #=> 386206920
p 145.big_permutation(133)        #=> 1.6801459655817956e+243
p 900.big_combination(450)        #=> 2.247471882064647e+269
p 1000.big_combination(969)       #=> 7.602322407770517e+58
p 15000.big_permutation(73)       #=> 6.004137561717704e+304
#That's about the maximum of Float:
p 15000.big_permutation(74)       #=> Infinity
#Fixnum has no maximum:
p 15000.permutation(74)           #=> 896237613852967826239917238565433149353074416025197784301593335243699358040738127950872384197159884905490054194835376498534786047382445592358843238688903318467070575184552953997615178973027752714539513893159815472948987921587671399790410958903188816684444202526779550201576117111844818124800000000000000000000

```

Ruby's Arrays have a permutation and a combination method which result in (lazy) enumerators. These Enumerators have a "size" method, which returns the size of the enumerator, or nil if it can’t be calculated lazily. (Since Ruby 2.0)

```ruby
(1..60).to_a.combination(53).size  #=> 386206920
```



## Scheme



```scheme

(define (combinations n k)
  (do ((i 0 (+ 1 i))
       (res 1 (/ (* res (- n i))
                 (- k i))))
    ((= i k) res)))

(define (permutations n k)
  (do ((i 0 (+ 1 i))
       (res 1 (* res (- n i))))
    ((= i k) res)))

(display "P(4,2) = ") (display (permutations 4 2)) (newline)
(display "P(8,2) = ") (display (permutations 8 2)) (newline)
(display "P(10,8) = ") (display (permutations 10 8)) (newline)
(display "C(10,8) = ") (display (combinations 10 8)) (newline)
(display "C(20,8) = ") (display (combinations 20 8)) (newline)
(display "C(60,58) = ") (display (combinations 60 58)) (newline)
(display "P(1000,10) = ") (display (permutations 1000 10)) (newline)
(display "P(1000,20) = ") (display (permutations 1000 20)) (newline)
(display "P(15000,2) = ") (display (permutations 15000 3)) (newline)
(display "C(1000,10) = ") (display (combinations 1000 10)) (newline)
(display "C(1000,999) = ") (display (combinations 1000 999)) (newline)
(display "C(1000,1000) = ") (display (combinations 1000 1000)) (newline)
(display "C(15000,14998) = ") (display (combinations 15000 14998)) (newline)

```


```txt

P(4,2) = 12
P(8,2) = 56
P(10,8) = 1814400
C(10,8) = 45
C(20,8) = 125970
C(60,58) = 1770
P(1000,10) = 955860613004397508326213120000
P(1000,20) = 825928413359200443640727373872992573951185652339949568000000
P(15000,2) = 3374325030000
C(1000,10) = 263409560461970212832400
C(1000,999) = 1000
C(1000,1000) = 1
C(15000,14998) = 112492500

```



## Sidef

```ruby
func P(n, k) { n! / ((n-k)!) }
func C(n, k) { binomial(n, k) }

class Logarithm(value) {
    method to_s {
        var e = int(value/10.log)
        "%.8fE%+d" % (exp(value - e*10.log), e)
    }
}

func lstirling(n) {
    n < 10 ? (lstirling(n+1) - log(n+1))
           : (0.5*log(2*Num.pi*n) + n*log(n/Num.e + 1/(12*Num.e*n)))
}

func P_approx(n, k) {
    Logarithm((lstirling(n) - lstirling(n -k)))
}

func C_approx(n, k) {
    Logarithm((lstirling(n) - lstirling(n -k) - lstirling(k)))
}

say "=> Exact results:"
for n (1..12) {
    var p = n//3
    say "P(#{n}, #{p}) = #{P(n, p)}"
}

for n (10..60 `by` 10) {
    var p = n//3
    say "C(#{n}, #{p}) = #{C(n, p)}"
}

say '';
say "=> Floating point approximations:"
for n ([5, 50, 500, 1000, 5000, 15000]) {
    var p = n//3
    say "P(#{n}, #{p}) = #{P_approx(n, p)}"
}

for n (100..1000 `by` 100) {
    var p = n//3
    say "C(#{n}, #{p}) = #{C_approx(n, p)}"
}
```

```txt

=> Exact results:
P(1, 0) = 1
P(2, 0) = 1
P(3, 1) = 3
P(4, 1) = 4
P(5, 1) = 5
P(6, 2) = 30
P(7, 2) = 42
P(8, 2) = 56
P(9, 3) = 504
P(10, 3) = 720
P(11, 3) = 990
P(12, 4) = 11880
C(10, 3) = 120
C(20, 6) = 38760
C(30, 10) = 30045015
C(40, 13) = 12033222880
C(50, 16) = 4923689695575
C(60, 20) = 4191844505805495

=> Floating point approximations:
P(5, 1) = 5.00000000E+0
P(50, 16) = 1.03017326E+26
P(500, 166) = 3.53487492E+434
P(1000, 333) = 5.96932629E+971
P(5000, 1666) = 6.85674576E+6025
P(15000, 5000) = 9.64985399E+20469
C(100, 33) = 2.94692433E+26
C(200, 66) = 7.26975256E+53
C(300, 100) = 4.15825147E+81
C(400, 133) = 1.25794868E+109
C(500, 166) = 3.92602839E+136
C(600, 200) = 2.50601778E+164
C(700, 233) = 8.10320356E+191
C(800, 266) = 2.64562336E+219
C(900, 300) = 1.74335637E+247
C(1000, 333) = 5.77613455E+274

```



## Stata

The '''[https://www.stata.com/help.cgi?mf_comb comb]''' function is builtin. Here is an implementation, together with perm:


```stata
real scalar comb1(n, k) {
	return(exp(lnfactorial(n)-lnfactorial(k)-lnfactorial(n-k)))
}

real scalar perm(n, k) {
	return(exp(lnfactorial(n)-lnfactorial(n-k)))
}
```



## Swift


Using AttaSwift's BigInt


```swift
import BigInt

func permutations(n: Int, k: Int) -> BigInt {
  let l = n - k + 1

  guard l <= n else {
    return 1
  }

  return (l...n).reduce(BigInt(1), { $0 * BigInt($1) })
}

func combinations(n: Int, k: Int) -> BigInt {
  let fact = {() -> BigInt in
    guard k > 1 else {
      return 1
    }

    return (2...k).map({ BigInt($0) }).reduce(1, *)
  }()

  return permutations(n: n, k: k) / fact
}

print("Sample of permutations from 1 to 12")

for i in 1...12 {
  print("\(i) P \(i / 3) = \(permutations(n: i, k: i / 3))")
}

print("\nSample of combinations from 10 to 60")

for i in stride(from: 10, through: 60, by: 10) {
  print("\(i) C \(i / 3) = \(combinations(n: i, k: i / 3))")
}

print("\nSample of permutations from 5 to 15,000")

for i in [5, 50, 500, 1000, 5000, 15000] {
  let k = i / 3
  let res = permutations(n: i, k: k).description
  let extra = res.count > 40 ? "... (\(res.count - 40) more digits)" : ""

  print("\(i) P \(k) = \(res.prefix(40))\(extra)")
}

print("\nSample of combinations from 100 to 1000")

for i in stride(from: 100, through: 1000, by: 100) {
  let k = i / 3
  let res = combinations(n: i, k: k).description
  let extra = res.count > 40 ? "... (\(res.count - 40) more digits)" : ""

  print("\(i) C \(k) = \(res.prefix(40))\(extra)")
}
```


```txt
Sample of permutations from 1 to 12
1 P 0 = 1
2 P 0 = 1
3 P 1 = 3
4 P 1 = 4
5 P 1 = 5
6 P 2 = 30
7 P 2 = 42
8 P 2 = 56
9 P 3 = 504
10 P 3 = 720
11 P 3 = 990
12 P 4 = 11880

Sample of combinations from 10 to 60
10 C 3 = 120
20 C 6 = 38760
30 C 10 = 30045015
40 C 13 = 12033222880
50 C 16 = 4923689695575
60 C 20 = 4191844505805495

Sample of permutations from 5 to 15,000
5 P 1 = 5
50 P 16 = 103017324974226408345600000
500 P 166 = 3534874921742942787609361826601762306844... (395 more digits)
1000 P 333 = 5969326288503415089039701765900784280998... (932 more digits)
5000 P 1666 = 6856745757255674275484536940248896062234... (5986 more digits)
15000 P 5000 = 9649853988727493922014858805931295980792... (20430 more digits)

Sample of combinations from 100 to 1000
100 C 33 = 294692427022540894366527900
200 C 66 = 7269752545169278341527066651192738976755... (14 more digits)
300 C 100 = 4158251463258564744783383526326405580280... (42 more digits)
400 C 133 = 1257948684182108702133348475651965004491... (70 more digits)
500 C 166 = 3926028386194422755220408345072331428197... (97 more digits)
600 C 200 = 2506017783221402805005616770513228835202... (125 more digits)
700 C 233 = 8103203563339599904740453644031138232944... (152 more digits)
800 C 266 = 2645623362683627034288829299556124255091... (180 more digits)
900 C 300 = 1743356373296446642960730765085718347630... (208 more digits)
1000 C 333 = 5776134553147651669777486323549601722339... (235 more digits)
```



## Tcl

Tcl doesn't allow the definition of new infix operators, so we define <math>P</math> and <math>C</math> as ordinary functions. There are no problems with loss of significance though: Tcl has supported arbitrary precision integer arithmetic since 8.5.
```tcl
# Exact integer versions
proc tcl::mathfunc::P {n k} {
    set t 1
    for {set i $n} {$i > $n-$k} {incr i -1} {
	set t [expr {$t * $i}]
    }
    return $t
}
proc tcl::mathfunc::C {n k} {
    set t [P $n $k]
    for {set i $k} {$i > 1} {incr i -1} {
	set t [expr {$t / $i}]
    }
    return $t
}

# Floating point versions using the Gamma function
package require math
proc tcl::mathfunc::lnGamma n {math::ln_Gamma $n}
proc tcl::mathfunc::fP {n k} {
    expr {exp(lnGamma($n+1) - lnGamma($n-$k+1))}
}
proc tcl::mathfunc::fC {n k} {
    expr {exp(lnGamma($n+1) - lnGamma($n-$k+1) - lnGamma($k+1))}
}
```

Demonstrating:

```tcl
# Using the exact integer versions
puts "A sample of Permutations from 1 to 12:"
for {set i 4} {$i <= 12} {incr i} {
    set ii [expr {$i - 2}]
    set iii [expr {$i - int(sqrt($i))}]
    puts "$i P $ii = [expr {P($i,$ii)}], $i P $iii = [expr {P($i,$iii)}]"
}
puts "A sample of Combinations from 10 to 60:"
for {set i 10} {$i <= 60} {incr i 10} {
    set ii [expr {$i - 2}]
    set iii [expr {$i - int(sqrt($i))}]
    puts "$i C $ii = [expr {C($i,$ii)}], $i C $iii = [expr {C($i,$iii)}]"
}
# Using the approximate floating point versions
puts "A sample of Permutations from 5 to 15000:"
for {set i 5} {$i <= 150} {incr i 10} {
    set ii [expr {$i - 2}]
    set iii [expr {$i - int(sqrt($i))}]
    puts "$i P $ii = [expr {fP($i,$ii)}], $i P $iii = [expr {fP($i,$iii)}]"
}
puts "A sample of Combinations from 100 to 1000:"
for {set i 100} {$i <= 1000} {incr i 100} {
    set ii [expr {$i - 2}]
    set iii [expr {$i - int(sqrt($i))}]
    puts "$i C $ii = [expr {fC($i,$ii)}], $i C $iii = [expr {fC($i,$iii)}]"
}
```

```txt

A sample of Permutations from 1 to 12:
4 P 2 = 12, 4 P 2 = 12
5 P 3 = 60, 5 P 3 = 60
6 P 4 = 360, 6 P 4 = 360
7 P 5 = 2520, 7 P 5 = 2520
8 P 6 = 20160, 8 P 6 = 20160
9 P 7 = 181440, 9 P 6 = 60480
10 P 8 = 1814400, 10 P 7 = 604800
11 P 9 = 19958400, 11 P 8 = 6652800
12 P 10 = 239500800, 12 P 9 = 79833600
A sample of Combinations from 10 to 60:
10 C 8 = 45, 10 C 7 = 120
20 C 18 = 190, 20 C 16 = 4845
30 C 28 = 435, 30 C 25 = 142506
40 C 38 = 780, 40 C 34 = 3838380
50 C 48 = 1225, 50 C 43 = 99884400
60 C 58 = 1770, 60 C 53 = 386206920
A sample of Permutations from 5 to 15000:
5 P 3 = 59.9999999964319, 5 P 3 = 59.9999999964319
15 P 13 = 653837183936.7548, 15 P 12 = 217945727984.54794
25 P 23 = 7.755605021026223e+24, 25 P 20 = 1.2926008369145724e+23
35 P 33 = 5.166573982873315e+39, 35 P 30 = 8.610956638634269e+37
45 P 43 = 5.981111043018166e+55, 45 P 39 = 1.6614197342883882e+53
55 P 53 = 6.348201676661335e+72, 55 P 48 = 2.5191276496660396e+69
65 P 63 = 4.123825295988996e+90, 65 P 57 = 2.0455482620718488e+86
75 P 73 = 1.2404570405684596e+109, 75 P 67 = 6.153060717624475e+104
85 P 83 = 1.4085520572027225e+128, 85 P 76 = 7.763183737477006e+122
95 P 93 = 5.164989244208789e+147, 95 P 86 = 2.846665148075141e+142
105 P 103 = 5.406983791334563e+167, 105 P 95 = 2.980039567808848e+161
115 P 113 = 1.462546846791721e+188, 115 P 105 = 8.060774068156828e+181
125 P 123 = 9.413385884788385e+208, 125 P 114 = 4.716503269639238e+201
135 P 133 = 1.345236353714729e+230, 135 P 124 = 6.74020138809567e+222
145 P 143 = 4.0239630289197437e+251, 145 P 133 = 1.6801459658196038e+243
A sample of Combinations from 100 to 1000:
100 C 98 = 4950.000000564707, 100 C 90 = 17310309460118.861
200 C 198 = 19900.000002250566, 200 C 186 = 1.1797916416885855e+21
300 C 298 = 44850.00000506082, 300 C 283 = 2.287708142503998e+27
400 C 398 = 79800.00000901309, 400 C 380 = 2.788360984244711e+33
500 C 498 = 124750.00001405331, 500 C 478 = 1.327364247175741e+38
600 C 598 = 179700.00002031153, 600 C 576 = 4.7916866834178515e+42
700 C 698 = 244650.00002750417, 700 C 674 = 1.454786513417567e+47
800 C 798 = 319600.0000360682, 800 C 772 = 3.933526871778561e+51
900 C 898 = 404550.0000452471, 900 C 870 = 9.803348169192494e+55
1000 C 998 = 499500.0000564987, 1000 C 969 = 7.602322409167201e+58

```

It should be noted that for large values, it can be ''much'' faster to use the floating point version (at a cost of losing significance). In particular <code>expr C(1000,500)</code> takes approximately 1000 times longer to compute than <code>expr fC(1000,500)</code>


## VBScript


```vb
' Combinations and permutations - vbs - 10/04/2017
dim i,j
Wscript.StdOut.WriteLine  "-- Long Integer - Permutations - from 1 to 12"
for i=1 to 12
	for j=1 to i
		Wscript.StdOut.Write "P(" & i & "," & j & ")=" & perm(i,j) & "  "
	next 'j
	Wscript.StdOut.WriteLine ""
next 'i
Wscript.StdOut.WriteLine  "-- Float integer - Combinations from 10 to 60"
for i=10 to 60 step 10
	for j=1 to i step i\5
		Wscript.StdOut.Write  "C(" & i & "," & j & ")=" & comb(i,j) & "  "
	next 'j
	Wscript.StdOut.WriteLine ""
next 'i
Wscript.StdOut.WriteLine  "-- Float integer - Permutations from 5000 to 15000"
for i=5000 to 15000 step 5000
	for j=10 to 70 step 20
		Wscript.StdOut.Write  "C(" & i & "," & j & ")=" & perm(i,j) & "  "
	next 'j
	Wscript.StdOut.WriteLine ""
next 'i
Wscript.StdOut.WriteLine  "-- Float integer - Combinations from 200 to 1000"
for i=200 to 1000 step 200
	for j=20 to 100 step 20
		Wscript.StdOut.Write "P(" & i & "," & j & ")=" & comb(i,j) & "  "
	next 'j
	Wscript.StdOut.WriteLine ""
next 'i

function perm(x,y)
	dim i,z
	z=1
	for i=x-y+1 to x
		z=z*i
	next 'i
	perm=z
end function 'perm

function fact(x)
	dim i,z
	z=1
	for i=2 to x
		z=z*i
	next 'i
	fact=z
end function 'fact

function comb(byval x,byval y)
	if y>x then
		comb=0
	elseif x=y then
		comb=1
	else
		if x-y<y then y=x-y
		comb=perm(x,y)/fact(y)
	end if
end function 'comb
```


```txt
-- Long Integer - Permutations - from 1 to 12
P(1,1)=1
P(2,1)=2  P(2,2)=2
P(3,1)=3  P(3,2)=6  P(3,3)=6
P(4,1)=4  P(4,2)=12  P(4,3)=24  P(4,4)=24
P(5,1)=5  P(5,2)=20  P(5,3)=60  P(5,4)=120  P(5,5)=120
P(6,1)=6  P(6,2)=30  P(6,3)=120  P(6,4)=360  P(6,5)=720  P(6,6)=720
P(7,1)=7  P(7,2)=42  P(7,3)=210  P(7,4)=840  P(7,5)=2520  P(7,6)=5040  P(7,7)=5040
P(8,1)=8  P(8,2)=56  P(8,3)=336  P(8,4)=1680  P(8,5)=6720  P(8,6)=20160  P(8,7)=40320  P(8,8)=40320
P(9,1)=9  P(9,2)=72  P(9,3)=504  P(9,4)=3024  P(9,5)=15120  P(9,6)=60480  P(9,7)=181440  P(9,8)=362880  P(9,9)=362880
P(10,1)=10  P(10,2)=90  P(10,3)=720  P(10,4)=5040  P(10,5)=30240  P(10,6)=151200  P(10,7)=604800  P(10,8)=1814400  P(10,9)=3628800  P(10,10)=3628800
P(11,1)=11  P(11,2)=110  P(11,3)=990  P(11,4)=7920  P(11,5)=55440  P(11,6)=332640  P(11,7)=1663200  P(11,8)=6652800  P(11,9)=19958400  P(11,10)=39916800  P(11,11)=39916800
P(12,1)=12  P(12,2)=132  P(12,3)=1320  P(12,4)=11880  P(12,5)=95040  P(12,6)=665280  P(12,7)=3991680  P(12,8)=19958400  P(12,9)=79833600  P(12,10)=239500800  P(12,11)=479001600  P(12,12)=479001600
-- Float integer - Combinations from 10 to 60
C(10,1)=10  C(10,3)=120  C(10,5)=252  C(10,7)=120  C(10,9)=10
C(20,1)=20  C(20,5)=15504  C(20,9)=167960  C(20,13)=77520  C(20,17)=1140
C(30,1)=30  C(30,7)=2035800  C(30,13)=119759850  C(30,19)=54627300  C(30,25)=142506
C(40,1)=40  C(40,9)=273438880  C(40,17)=88732378800  C(40,25)=40225345056  C(40,33)=18643560
C(50,1)=50  C(50,11)=37353738800  C(50,21)=67327446062800  C(50,31)=30405943383200  C(50,41)=2505433700
C(60,1)=60  C(60,13)=5166863427600  C(60,25)=5,19154379743283E+16  C(60,37)=2,33853324208686E+16  C(60,49)=342700125300
-- Float integer - Permutations from 5000 to 15000
C(5000,10)=9,67807348145655E+36  C(5000,30)=8,53575581200676E+110  C(5000,50)=6,94616656703754E+184  C(5000,70)=5,21383580146194E+258
C(10000,10)=9,95508690556325E+39  C(10000,30)=9,57391540294832E+119  C(10000,50)=8,84526658067387E+199  C(10000,70)=7,850079552152E+279
C(15000,10)=5,74922667554068E+41  C(15000,30)=1,86266591363916E+125  C(15000,50)=5,87565776023335E+208  C(15000,70)=1,80450662858719E+292
-- Float integer - Combinations from 200 to 1000
P(200,20)=1,61358778796735E+27  P(200,40)=2,05015799519859E+42  P(200,60)=7,04050484926892E+51  P(200,80)=1,64727865245176E+57  P(200,100)=9,05485146561033E+58
P(400,20)=2,7883609836709E+33  P(400,40)=1,9703374084393E+55  P(400,60)=1,50867447857277E+72  P(400,80)=4,22814216193593E+85  P(400,100)=2,24185479155434E+96
P(600,20)=1,09108668819553E+37  P(600,40)=4,33518929550349E+62  P(600,60)=2,77426667704894E+83  P(600,80)=1,00412999166192E+101  P(600,100)=1,11141121906619E+116
P(800,20)=3,72976760205571E+39  P(800,40)=6,04464684067502E+67  P(800,60)=1,90370080982158E+91  P(800,80)=4,14170924105943E+111  P(800,100)=3,4111376846871E+129
P(1000,20)=3,39482811302458E+41  P(1000,40)=5,55974423571664E+71  P(1000,60)=1,97427486218598E+97  P(1000,80)=5,43269728730706E+119  P(1000,100)=6,38505119263051E+139

```



## Visual Basic .NET

```vbnet
' Combinations and permutations - 10/04/2017
Imports System.Numerics 'BigInteger
Module CombPermRc

    Sub Main()
        Dim i, j As Long
        For i = 1 To 12
            For j = 1 To i
                Console.Write("P(" & i & "," & j & ")=" & PermBig(i, j).ToString & "  ")
            Next j
            Console.WriteLine("")
        Next i
        Console.WriteLine("--")
        For i = 10 To 60 Step 10
            For j = 1 To i Step i \ 5
                Console.Write("C(" & i & "," & j & ")=" & CombBig(i, j).ToString & "  ")
            Next j
            Console.WriteLine("")
        Next i
        Console.WriteLine("--")
        For i = 5000 To 15000 Step 5000
            For j = 4000 To 5000 Step 1000
                Console.Write("P(" & i & "," & j & ")=" & PermBig(i, j).ToString("E") & "  ")
            Next j
            Console.WriteLine("")
        Next i
        Console.WriteLine("--")
        For i = 5000 To 15000 Step 5000
            For j = 4000 To 5000 Step 1000
                Console.Write("C(" & i & "," & j & ")=" & CombBig(i, j).ToString("E") & "  ")

            Next j
            Console.WriteLine("")
        Next i
        Console.WriteLine("--")
        i = 5000 : j = 4000
        Console.WriteLine("C(" & i & "," & j & ")=" & CombBig(i, j).ToString)
    End Sub 'Main

    Function PermBig(x As Long, y As Long) As BigInteger
        Dim i As Long, z As BigInteger
        z = 1
        For i = x - y + 1 To x
            z = z * i
        Next i
        Return (z)
    End Function 'PermBig

    Function FactBig(x As Long) As BigInteger
        Dim i As Long, z As BigInteger
        z = 1
        For i = 2 To x
            z = z * i
        Next i
        Return (z)
    End Function 'FactBig

    Function CombBig(ByVal x As Long, ByVal y As Long) As BigInteger
        If y > x Then
            Return (0)
        ElseIf x = y Then
            Return (1)
        Else
            If x - y < y Then y = x - y
            Return (PermBig(x, y) / FactBig(y))
        End If
    End Function 'CombBig

End Module
```


```txt
P(1,1)=1
P(2,1)=2  P(2,2)=2
P(3,1)=3  P(3,2)=6  P(3,3)=6
P(4,1)=4  P(4,2)=12  P(4,3)=24  P(4,4)=24
P(5,1)=5  P(5,2)=20  P(5,3)=60  P(5,4)=120  P(5,5)=120
P(6,1)=6  P(6,2)=30  P(6,3)=120  P(6,4)=360  P(6,5)=720  P(6,6)=720
P(7,1)=7  P(7,2)=42  P(7,3)=210  P(7,4)=840  P(7,5)=2520  P(7,6)=5040  P(7,7)=5040
P(8,1)=8  P(8,2)=56  P(8,3)=336  P(8,4)=1680  P(8,5)=6720  P(8,6)=20160  P(8,7)=40320  P(8,8)=40320
P(9,1)=9  P(9,2)=72  P(9,3)=504  P(9,4)=3024  P(9,5)=15120  P(9,6)=60480  P(9,7)=181440  P(9,8)=362880  P(9,9)=362880
P(10,1)=10  P(10,2)=90  P(10,3)=720  P(10,4)=5040  P(10,5)=30240  P(10,6)=151200  P(10,7)=604800  P(10,8)=1814400  P(10,9)=3628800  P(10,10)=3628800
P(11,1)=11  P(11,2)=110  P(11,3)=990  P(11,4)=7920  P(11,5)=55440  P(11,6)=332640  P(11,7)=1663200  P(11,8)=6652800  P(11,9)=19958400  P(11,10)=39916800  P(11,11)=39916800
P(12,1)=12  P(12,2)=132  P(12,3)=1320  P(12,4)=11880  P(12,5)=95040  P(12,6)=665280  P(12,7)=3991680  P(12,8)=19958400  P(12,9)=79833600  P(12,10)=239500800  P(12,11)=479001600  P(12,12)=479001600
--
C(10,1)=10  C(10,3)=120  C(10,5)=252  C(10,7)=120  C(10,9)=10
C(20,1)=20  C(20,5)=15504  C(20,9)=167960  C(20,13)=77520  C(20,17)=1140
C(30,1)=30  C(30,7)=2035800  C(30,13)=119759850  C(30,19)=54627300  C(30,25)=142506
C(40,1)=40  C(40,9)=273438880  C(40,17)=88732378800  C(40,25)=40225345056  C(40,33)=18643560
C(50,1)=50  C(50,11)=37353738800  C(50,21)=67327446062800  C(50,31)=30405943383200  C(50,41)=2505433700
C(60,1)=60  C(60,13)=5166863427600  C(60,25)=51915437974328292  C(60,37)=23385332420868600  C(60,49)=342700125300
--
P(5000,4000)=1,050873E+13758  P(5000,5000)=4,228578E+16325
P(10000,4000)=1,060455E+15594  P(10000,5000)=6,731009E+19333
P(15000,4000)=8,685001E+16448  P(15000,5000)=9,649854E+20469
--
C(5000,4000)=5,746236E+1084  C(5000,5000)=1,000000E+000
C(10000,4000)=5,798630E+2920  C(10000,5000)=1,591790E+3008
C(15000,4000)=4,749011E+3775  C(15000,5000)=2,282057E+4144
--
C(5000,4000)=57462357505803375604893834658665168251899919793850512934468881710397678593302188064618445132583370701755893065787216750992391223467601994741594656878559929037277303674963658032197224327768110236651567704673226756781828332650887849150208195780031161286578505113618731045004523840401144118298192191997565735245181433457469532981432785237769191864102953974244072964471109551273603780184330987071947790993108191904370472373403157802158903129815170101708451875442019845175637901995588390614304812103202403626211504997668649346891167495657556154392183988627948442807346603688457854135114491955258804187129028256547543888109987151649038111791932035229202856007767332717845596528598314477979861265222941138323298702349967224867703420888363395662988291273283611081068577160905840445308086112429900453394212790633910614322699210850302387512579976209123523546689147207974269396548873838155355768985614160932799226261509866933143702889005270480654844312094564956178277998090168826124850606847021667494019587077659107276117413835912767949017954979839258481340540145909025953956582025656306426226560

```

