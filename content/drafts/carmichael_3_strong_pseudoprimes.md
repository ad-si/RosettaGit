+++
title = "Carmichael 3 strong pseudoprimes"
description = ""
date = 2019-10-11T18:31:43Z
aliases = []
[extra]
id = 12638
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}

A lot of composite numbers can be separated from primes by Fermat's Little Theorem, but there are some that completely confound it.

The   [[Miller-Rabin primality test|Miller Rabin Test]]   uses a combination of Fermat's Little Theorem and Chinese Division Theorem to overcome this.

The purpose of this task is to investigate such numbers using a method based on   [[wp:Carmichael number|Carmichael numbers]],   as suggested in   [http://www.maths.lancs.ac.uk/~jameson/carfind.pdf Notes by G.J.O Jameson March 2010].


;Task:
Find Carmichael numbers of the form:
:::: <big> <i>Prime</i><sub>1</sub> &times; <i>Prime</i><sub>2</sub> &times; <i>Prime</i><sub>3</sub> </big>

where   <big> (<i>Prime</i><sub>1</sub>   <   <i>Prime</i><sub>2</sub>   <   <i>Prime</i><sub>3</sub>) </big>   for all     <big> <i>Prime</i><sub>1</sub> </big>      up to   '''61'''.

(See page 7 of   [http://www.maths.lancs.ac.uk/~jameson/carfind.pdf Notes by G.J.O Jameson March 2010]   for solutions.)


;Pseudocode:
For a given   <math>Prime_1</math>

 <tt>for 1 < h3 < Prime<sub>1</sub></tt>
     <tt>for 0 < d < h3+Prime<sub>1</sub></tt>
          <tt>if (h3+Prime<sub>1</sub>)*(Prime<sub>1</sub>-1) mod d == 0 and -Prime<sub>1</sub> squared mod h3 == d mod h3</tt>
          <tt>then</tt>
                <tt>Prime<sub>2</sub> = 1 + ((Prime<sub>1</sub>-1) * (h3+Prime<sub>1</sub>)/d)</tt>
                <tt>next d if Prime<sub>2</sub> is not prime</tt>
                <tt>Prime<sub>3</sub> = 1 + (Prime<sub>1</sub>*Prime<sub>2</sub>/h3)</tt>
                <tt>next d if Prime<sub>3</sub> is not prime</tt>
                <tt>next d if (Prime<sub>2</sub>*Prime<sub>3</sub>) mod (Prime<sub>1</sub>-1) not equal 1</tt>
                <tt>Prime<sub>1</sub> * Prime<sub>2</sub> * Prime<sub>3</sub> is a Carmichael Number</tt>



;related task
[[Chernick's Carmichael numbers]]




## 11l

{{trans|D}}

```11l
F mod_(n, m)
   R ((n % m) + m) % m
 
F is_prime(n)
   I n C (2, 3)
      R 1B
   E I n < 2 | n % 2 == 0 | n % 3 == 0
      R 0B
   V div = 5
   V inc = 2
   L div ^ 2 <= n
      I n % div == 0
         R 0B
      div += inc
      inc = 6 - inc
   R 1B
 
L(p) 2 .< 62
   I !is_prime(p)
      L.continue
   L(h3) 2 .< p
      V g = h3 + p
      L(d) 1 .< g
         I (g * (p - 1)) % d != 0 | mod_(-p * p, h3) != d % h3
            L.continue;
         V q = 1 + (p - 1) * g I/ d;
         I !is_prime(q)
            L.continue
         V r = 1 + (p * q I/ h3)
         I !is_prime(r) | (q * r) % (p - 1) != 1
            L.continue
         print(p‘ x ’q‘ x ’r)
```

{{out}}

```txt

3 x 11 x 17
5 x 29 x 73
5 x 17 x 29
5 x 13 x 17
7 x 19 x 67
7 x 31 x 73
7 x 13 x 31
7 x 23 x 41
...
61 x 1301 x 19841
61 x 277 x 2113
61 x 181 x 1381
61 x 541 x 3001
61 x 661 x 2521
61 x 271 x 571
61 x 241 x 421
61 x 3361 x 4021

```



## Ada


Uses the Miller_Rabin package from 
[[Miller-Rabin primality test#ordinary integers]].

```Ada
with Ada.Text_IO, Miller_Rabin;

procedure Nemesis is

   type Number is range 0 .. 2**40-1; -- sufficiently large for the task

   function Is_Prime(N: Number) return Boolean is
      package MR is new Miller_Rabin(Number); use MR;
   begin
      return MR.Is_Prime(N) = Probably_Prime;
   end Is_Prime;

begin
   for P1 in Number(2) .. 61 loop
      if Is_Prime(P1) then
         for H3 in Number(1) .. P1 loop
            declare
               G: Number := H3 + P1;
               P2, P3: Number;
            begin
               Inner:
               for D in 1 .. G-1 loop
                  if ((H3+P1) * (P1-1)) mod D = 0 and then
                    (-(P1 * P1)) mod H3 = D mod H3
                  then
                     P2 := 1 + ((P1-1) * G / D);
                     P3 := 1 +(P1*P2/H3);
                     if Is_Prime(P2) and then Is_Prime(P3)
                       and then (P2*P3) mod (P1-1) = 1
                     then
                       Ada.Text_IO.Put_Line
                        ( Number'Image(P1) & " *"   & Number'Image(P2) & " *" &
                          Number'Image(P3) & "  = " & Number'Image(P1*P2*P3) );
                     end if;
                  end if;
               end loop Inner;
            end;
         end loop;
      end if;
   end loop;
end Nemesis;
```


{{out}}

```txt
 3 * 11 * 17  =  561
 5 * 29 * 73  =  10585
 5 * 17 * 29  =  2465
 5 * 13 * 17  =  1105
 7 * 19 * 67  =  8911

... (the full output is 69 lines long) ...

 61 * 271 * 571  =  9439201
 61 * 241 * 421  =  6189121
 61 * 3361 * 4021  =  824389441
```



## ALGOL 68

Uses the Sieve of Eratosthenes code from the Smith Numbers task with an increased upper-bound (included here for convenience).

```algol68
# sieve of Eratosthene: sets s[i] to TRUE if i is prime, FALSE otherwise #
PROC sieve = ( REF[]BOOL s )VOID:
     BEGIN
        # start with everything flagged as prime                             # 
        FOR i TO UPB s DO s[ i ] := TRUE OD;
        # sieve out the non-primes                                           #
        s[ 1 ] := FALSE;
        FOR i FROM 2 TO ENTIER sqrt( UPB s ) DO
            IF s[ i ] THEN FOR p FROM i * i BY i TO UPB s DO s[ p ] := FALSE OD FI
        OD
     END # sieve # ;

# construct a sieve of primes up to the maximum number required for the task #
# For Prime1, we need to check numbers up to around 120 000                  #
INT max number = 200 000;
[ 1 : max number ]BOOL is prime;
sieve( is prime );

# Find the Carmichael 3 Stromg Pseudoprimes for Prime1 up to 61              #

FOR prime1 FROM 2 TO 61 DO
    IF is prime[ prime 1 ] THEN
        FOR h3 TO prime1 - 1 DO
            FOR d TO ( h3 + prime1 ) - 1 DO
                IF   ( h3 + prime1 ) * ( prime1 - 1 ) MOD d = 0
                AND ( - ( prime1 * prime1 ) ) MOD h3 = d MOD h3
                THEN
                    INT prime2 = 1 + ( ( prime1 - 1 ) * ( h3 + prime1 ) OVER d );
                    IF is prime[ prime2 ] THEN
                        INT prime3 = 1 + ( prime1 * prime2 OVER h3 );
                        IF is prime[ prime3 ] THEN 
                            IF ( prime2 * prime3 ) MOD ( prime1 - 1 ) = 1 THEN
                                print( ( whole( prime1, 0 ), " ", whole( prime2, 0 ), " ", whole( prime3, 0 ), newline ) )
                            FI
                        FI
                    FI
                FI
            OD
        OD
    FI
OD
```

{{out}}

```txt

3 11 17
5 29 73
5 17 29
5 13 17
7 19 67
7 31 73
7 13 31
7 23 41
7 73 103
7 13 19
13 61 397
13 37 241
13 97 421
13 37 97
13 37 61
...
59 1451 2089
61 421 12841
61 181 5521
61 1301 19841
61 277 2113
61 181 1381
61 541 3001
61 661 2521
61 271 571
61 241 421
61 3361 4021

```


## AWK


```AWK

# syntax: GAWK -f CARMICHAEL_3_STRONG_PSEUDOPRIMES.AWK
# converted from C
BEGIN {
    printf("%5s%8s%8s%13s\n","P1","P2","P3","PRODUCT")
    for (p1=2; p1<62; p1++) {
      if (!is_prime(p1)) { continue }
      for (h3=1; h3<p1; h3++) {
        for (d=1; d<h3+p1; d++) {
          if ((h3+p1)*(p1-1)%d == 0 && mod(-p1*p1,h3) == d%h3) {
            p2 = int(1+((p1-1)*(h3+p1)/d))
            if (!is_prime(p2)) { continue }
            p3 = int(1+(p1*p2/h3))
            if (!is_prime(p3) || (p2*p3)%(p1-1) != 1) { continue }
            printf("%5d x %5d x %5d = %10d\n",p1,p2,p3,p1*p2*p3)
            count++
          }
        }
      }
    }
    printf("%d numbers\n",count)
    exit(0)
}
function is_prime(n,  i) {
    if (n <= 3) {
      return(n > 1)
    }
    else if (!(n%2) || !(n%3)) {
      return(0)
    }
    else {
      for (i=5; i*i<=n; i+=6) {
        if (!(n%i) || !(n%(i+2))) {
          return(0)
        }
      }
      return(1)
    }
}
function mod(n,m) {
# the % operator actually calculates the remainder of a / b so we need a small adjustment so it works as expected for negative values
    return(((n%m)+m)%m)
}

```

{{out}}

```txt

   P1      P2      P3      PRODUCT
    3 x    11 x    17 =        561
    5 x    29 x    73 =      10585
    5 x    17 x    29 =       2465
    5 x    13 x    17 =       1105
    7 x    19 x    67 =       8911
    7 x    31 x    73 =      15841
    7 x    13 x    31 =       2821
    7 x    23 x    41 =       6601
    7 x    73 x   103 =      52633
    7 x    13 x    19 =       1729
   13 x    61 x   397 =     314821
   13 x    37 x   241 =     115921
   13 x    97 x   421 =     530881
   13 x    37 x    97 =      46657
   13 x    37 x    61 =      29341
   17 x    41 x   233 =     162401
   17 x   353 x  1201 =    7207201
   19 x    43 x   409 =     334153
   19 x   199 x   271 =    1024651
   23 x   199 x   353 =    1615681
   29 x   113 x  1093 =    3581761
   29 x   197 x   953 =    5444489
   31 x   991 x 15361 =  471905281
   31 x    61 x   631 =    1193221
   31 x   151 x  1171 =    5481451
   31 x    61 x   271 =     512461
   31 x    61 x   211 =     399001
   31 x   271 x   601 =    5049001
   31 x   181 x   331 =    1857241
   37 x   109 x  2017 =    8134561
   37 x    73 x   541 =    1461241
   37 x   613 x  1621 =   36765901
   37 x    73 x   181 =     488881
   37 x    73 x   109 =     294409
   41 x  1721 x 35281 = 2489462641
   41 x   881 x 12041 =  434932961
   41 x   101 x   461 =    1909001
   41 x   241 x   761 =    7519441
   41 x   241 x   521 =    5148001
   41 x    73 x   137 =     410041
   41 x    61 x   101 =     252601
   43 x   631 x 13567 =  368113411
   43 x   271 x  5827 =   67902031
   43 x   127 x  2731 =   14913991
   43 x   127 x  1093 =    5968873
   43 x   211 x   757 =    6868261
   43 x   631 x  1597 =   43331401
   43 x   127 x   211 =    1152271
   43 x   211 x   337 =    3057601
   43 x   433 x   643 =   11972017
   43 x   547 x   673 =   15829633
   43 x  3361 x  3907 =  564651361
   47 x  3359 x  6073 =  958762729
   47 x  1151 x  1933 =  104569501
   47 x  3727 x  5153 =  902645857
   53 x   157 x  2081 =   17316001
   53 x    79 x   599 =    2508013
   53 x   157 x   521 =    4335241
   59 x  1451 x  2089 =  178837201
   61 x   421 x 12841 =  329769721
   61 x   181 x  5521 =   60957361
   61 x  1301 x 19841 = 1574601601
   61 x   277 x  2113 =   35703361
   61 x   181 x  1381 =   15247621
   61 x   541 x  3001 =   99036001
   61 x   661 x  2521 =  101649241
   61 x   271 x   571 =    9439201
   61 x   241 x   421 =    6189121
   61 x  3361 x  4021 =  824389441
69 numbers

```



## C


```C

#include <stdio.h>

/* C's % operator actually calculates the remainder of a / b so we need a
 * small adjustment so it works as expected for negative values */
#define mod(n,m) ((((n) % (m)) + (m)) % (m))

int is_prime(unsigned int n)
{
    if (n <= 3) {
        return n > 1;
    }
    else if (!(n % 2) || !(n % 3)) {
        return 0;
    }
    else {
        unsigned int i;
        for (i = 5; i*i <= n; i += 6)
            if (!(n % i) || !(n % (i + 2)))
                return 0;
        return 1;
    }
}

void carmichael3(int p1)
{
    if (!is_prime(p1)) return;

    int h3, d, p2, p3;
    for (h3 = 1; h3 < p1; ++h3) {
        for (d = 1; d < h3 + p1; ++d) {
            if ((h3 + p1)*(p1 - 1) % d == 0 && mod(-p1 * p1, h3) == d % h3) {
                p2 = 1 + ((p1 - 1) * (h3 + p1)/d);
                if (!is_prime(p2)) continue;
                p3 = 1 + (p1 * p2 / h3);
                if (!is_prime(p3) || (p2 * p3) % (p1 - 1) != 1) continue;
                printf("%d %d %d\n", p1, p2, p3);
            }
        }
    }
}

int main(void)
{
    int p1;
    for (p1 = 2; p1 < 62; ++p1)
        carmichael3(p1);
    return 0;
}

```

{{out}}

```txt

3 11 17
5 29 73
5 17 29
5 13 17
7 19 67
7 31 73
.
.
.
61 181 1381
61 541 3001
61 661 2521
61 271 571
61 241 421
61 3361 4021

```



## Clojure


```lisp

(ns example
  (:gen-class))

(defn prime? [n]
  " Prime number test (using Java) "
  (.isProbablePrime (biginteger n) 16))

(defn carmichael [p1]
  " Triplets of Carmichael primes, with first element prime p1 "
  (if (prime? p1)
    (into [] (for [h3 (range 2 p1)
          :let [g (+ h3 p1)]
          d (range 1 g)
          :when (and (= (mod (* g (dec p1)) d) 0)
                     (= (mod (- (* p1 p1)) h3) (mod d h3)))
          :let [p2 (inc (quot (* (dec p1) g) d))]
          :when (prime? p2)
          :let [p3 (inc (quot (* p1 p2) h3))]
          :when (prime? p3)
          :when (= (mod (* p2 p3) (dec p1)) 1)]
         [p1 p2 p3]))))

; Generate Result
(def numbers (mapcat carmichael (range 2 62)))
(println (count numbers) "Carmichael numbers found:")
(doseq [t numbers]
  (println (format "%5d x %5d x %5d = %10d" (first t) (second t) (last t) (apply * t))))

```

{{Out}}

```txt

69 Carmichael numbers found
    3 x    11 x    17 =        561
    5 x    29 x    73 =      10585
    5 x    17 x    29 =       2465
    5 x    13 x    17 =       1105
    7 x    19 x    67 =       8911
    7 x    31 x    73 =      15841
    7 x    13 x    31 =       2821
    7 x    23 x    41 =       6601
    7 x    73 x   103 =      52633
    7 x    13 x    19 =       1729
   13 x    61 x   397 =     314821
   13 x    37 x   241 =     115921
   13 x    97 x   421 =     530881
   13 x    37 x    97 =      46657
   13 x    37 x    61 =      29341
   17 x    41 x   233 =     162401
   17 x   353 x  1201 =    7207201
   19 x    43 x   409 =     334153
   19 x   199 x   271 =    1024651
   23 x   199 x   353 =    1615681
   29 x   113 x  1093 =    3581761
   29 x   197 x   953 =    5444489
   31 x   991 x 15361 =  471905281
   31 x    61 x   631 =    1193221
   31 x   151 x  1171 =    5481451
   31 x    61 x   271 =     512461
   31 x    61 x   211 =     399001
   31 x   271 x   601 =    5049001
   31 x   181 x   331 =    1857241
   37 x   109 x  2017 =    8134561
   37 x    73 x   541 =    1461241
   37 x   613 x  1621 =   36765901
   37 x    73 x   181 =     488881
   37 x    73 x   109 =     294409
   41 x  1721 x 35281 = 2489462641
   41 x   881 x 12041 =  434932961
   41 x   101 x   461 =    1909001
   41 x   241 x   761 =    7519441
   41 x   241 x   521 =    5148001
   41 x    73 x   137 =     410041
   41 x    61 x   101 =     252601
   43 x   631 x 13567 =  368113411
   43 x   271 x  5827 =   67902031
   43 x   127 x  2731 =   14913991
   43 x   127 x  1093 =    5968873
   43 x   211 x   757 =    6868261
   43 x   631 x  1597 =   43331401
   43 x   127 x   211 =    1152271
   43 x   211 x   337 =    3057601
   43 x   433 x   643 =   11972017
   43 x   547 x   673 =   15829633
   43 x  3361 x  3907 =  564651361
   47 x  3359 x  6073 =  958762729
   47 x  1151 x  1933 =  104569501
   47 x  3727 x  5153 =  902645857
   53 x   157 x  2081 =   17316001
   53 x    79 x   599 =    2508013
   53 x   157 x   521 =    4335241
   59 x  1451 x  2089 =  178837201
   61 x   421 x 12841 =  329769721
   61 x   181 x  5521 =   60957361
   61 x  1301 x 19841 = 1574601601
   61 x   277 x  2113 =   35703361
   61 x   181 x  1381 =   15247621
   61 x   541 x  3001 =   99036001
   61 x   661 x  2521 =  101649241
   61 x   271 x   571 =    9439201
   61 x   241 x   421 =    6189121
   61 x  3361 x  4021 =  824389441


```


## D


```d
enum mod = (in int n, in int m) pure nothrow @nogc=> ((n % m) + m) % m;

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

void main() {
  import std.stdio;

  foreach (immutable p; 2 .. 62) {
    if (!p.isPrime) continue;
    foreach (immutable h3; 2 .. p) {
      immutable g = h3 + p;
      foreach (immutable d; 1 .. g) {
        if ((g * (p - 1)) % d != 0 || mod(-p * p, h3) != d % h3)
          continue;
        immutable q = 1 + (p - 1) * g / d;
        if (!q.isPrime) continue;
        immutable r = 1 + (p * q / h3);
        if (!r.isPrime || (q * r) % (p - 1) != 1) continue;
        writeln(p, " x ", q, " x ", r);
      }
    }
  }
}
```

{{out}}

```txt
3 x 11 x 17
5 x 29 x 73
5 x 17 x 29
5 x 13 x 17
7 x 19 x 67
7 x 31 x 73
7 x 13 x 31
7 x 23 x 41
7 x 73 x 103
7 x 13 x 19
13 x 61 x 397
13 x 37 x 241
13 x 97 x 421
13 x 37 x 97
13 x 37 x 61
17 x 41 x 233
17 x 353 x 1201
19 x 43 x 409
19 x 199 x 271
23 x 199 x 353
29 x 113 x 1093
29 x 197 x 953
31 x 991 x 15361
31 x 61 x 631
31 x 151 x 1171
31 x 61 x 271
31 x 61 x 211
31 x 271 x 601
31 x 181 x 331
37 x 109 x 2017
37 x 73 x 541
37 x 613 x 1621
37 x 73 x 181
37 x 73 x 109
41 x 1721 x 35281
41 x 881 x 12041
41 x 101 x 461
41 x 241 x 761
41 x 241 x 521
41 x 73 x 137
41 x 61 x 101
43 x 631 x 13567
43 x 271 x 5827
43 x 127 x 2731
43 x 127 x 1093
43 x 211 x 757
43 x 631 x 1597
43 x 127 x 211
43 x 211 x 337
43 x 433 x 643
43 x 547 x 673
43 x 3361 x 3907
47 x 3359 x 6073
47 x 1151 x 1933
47 x 3727 x 5153
53 x 157 x 2081
53 x 79 x 599
53 x 157 x 521
59 x 1451 x 2089
61 x 421 x 12841
61 x 181 x 5521
61 x 1301 x 19841
61 x 277 x 2113
61 x 181 x 1381
61 x 541 x 3001
61 x 661 x 2521
61 x 271 x 571
61 x 241 x 421
61 x 3361 x 4021
```



## EchoLisp


```scheme

;; charmichaël numbers up to N-th prime ; 61 is 18-th prime
(define (charms (N 18) local: (h31 0) (Prime2 0) (Prime3 0))
(for* ((Prime1 (primes N))
       (h3 (in-range 1 Prime1))
       (d  (+ h3 Prime1)))
      (set! h31 (+ h3 Prime1))
      #:continue (!zero? (modulo (* h31 (1- Prime1)) d))
      #:continue (!= (modulo d h3) (modulo (- (* Prime1 Prime1)) h3))
      (set! Prime2 (1+ ( * (1- Prime1) (quotient h31 d))))
      #:when (prime? Prime2)
      (set! Prime3 (1+ (quotient (*  Prime1  Prime2)  h3)))
      #:when (prime? Prime3)
      #:when (= 1 (modulo (* Prime2 Prime3) (1- Prime1)))
      (printf " 💥 %12d = %d x %d x %d"  (* Prime1 Prime2 Prime3) Prime1 Prime2 Prime3)))

```

{{out}}

```scheme

(charms 3)
💥          561 = 3 x 11 x 17
💥        10585 = 5 x 29 x 73
💥         2465 = 5 x 17 x 29
💥         1105 = 5 x 13 x 17

(charms 18)
;; skipped ....
💥    902645857 = 47 x 3727 x 5153
💥      2632033 = 53 x 53 x 937
💥     17316001 = 53 x 157 x 2081
💥      4335241 = 53 x 157 x 521
💥    178837201 = 59 x 1451 x 2089
💥    329769721 = 61 x 421 x 12841
💥     60957361 = 61 x 181 x 5521
💥      6924781 = 61 x 61 x 1861
💥      6924781 = 61 x 61 x 1861
💥     15247621 = 61 x 181 x 1381
💥     99036001 = 61 x 541 x 3001
💥    101649241 = 61 x 661 x 2521
💥      6189121 = 61 x 241 x 421
💥    824389441 = 61 x 3361 x 4021

```


=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Carmichael Number . Nigel Galloway: November 19th., 2017
let fN n = Seq.collect ((fun g->(Seq.map(fun e->(n,1+(n-1)*(n+g)/e,g,e))){1..(n+g-1)})){2..(n-1)}
let fG (P1,P2,h3,d) =
  let mod' n g = (n%g+g)%g
  let fN P3 = if isPrime P3 && (P2*P3)%(P1-1)=1 then Some (P1,P2,P3) else None
  if isPrime P2 && ((h3+P1)*(P1-1))%d=0 && mod' (-P1*P1) h3=d%h3 then fN (1+P1*P2/h3) else None
let carms g = primes|>Seq.takeWhile(fun n->n<=g)|>Seq.collect fN|>Seq.choose fG
carms 61 |> Seq.iter (fun (P1,P2,P3)->printfn "%2d x %4d x %5d = %10d" P1 P2 P3 ((uint64 P3)*(uint64 (P1*P2))))

```

{{out}}

```txt

 3 x   11 x    17 =        561
 5 x   29 x    73 =      10585
 5 x   17 x    29 =       2465
 5 x   13 x    17 =       1105
 7 x   19 x    67 =       8911
 7 x   31 x    73 =      15841
 7 x   13 x    31 =       2821
 7 x   23 x    41 =       6601
 7 x   73 x   103 =      52633
 7 x   13 x    19 =       1729
13 x   61 x   397 =     314821
13 x   37 x   241 =     115921
13 x   97 x   421 =     530881
13 x   37 x    97 =      46657
13 x   37 x    61 =      29341
17 x   41 x   233 =     162401
17 x  353 x  1201 =    7207201
19 x   43 x   409 =     334153
19 x  199 x   271 =    1024651
23 x  199 x   353 =    1615681
29 x  113 x  1093 =    3581761
29 x  197 x   953 =    5444489
31 x  991 x 15361 =  471905281
31 x   61 x   631 =    1193221
31 x  151 x  1171 =    5481451
31 x   61 x   271 =     512461
31 x   61 x   211 =     399001
31 x  271 x   601 =    5049001
31 x  181 x   331 =    1857241
37 x  109 x  2017 =    8134561
37 x   73 x   541 =    1461241
37 x  613 x  1621 =   36765901
37 x   73 x   181 =     488881
37 x   73 x   109 =     294409
41 x 1721 x 35281 = 2489462641
41 x  881 x 12041 =  434932961
41 x  101 x   461 =    1909001
41 x  241 x   761 =    7519441
41 x  241 x   521 =    5148001
41 x   73 x   137 =     410041
41 x   61 x   101 =     252601
43 x  631 x 13567 =  368113411
43 x  271 x  5827 =   67902031
43 x  127 x  2731 =   14913991
43 x  127 x  1093 =    5968873
43 x  211 x   757 =    6868261
43 x  631 x  1597 =   43331401
43 x  127 x   211 =    1152271
43 x  211 x   337 =    3057601
43 x  433 x   643 =   11972017
43 x  547 x   673 =   15829633
43 x 3361 x  3907 =  564651361
47 x 3359 x  6073 =  958762729
47 x 1151 x  1933 =  104569501
47 x 3727 x  5153 =  902645857
53 x  157 x  2081 =   17316001
53 x   79 x   599 =    2508013
53 x  157 x   521 =    4335241
59 x 1451 x  2089 =  178837201
61 x  421 x 12841 =  329769721
61 x  181 x  5521 =   60957361
61 x 1301 x 19841 = 1574601601
61 x  277 x  2113 =   35703361
61 x  181 x  1381 =   15247621
61 x  541 x  3001 =   99036001
61 x  661 x  2521 =  101649241
61 x  271 x   571 =    9439201
61 x  241 x   421 =    6189121
61 x 3361 x  4021 =  824389441

```



## Factor

Note the use of <code>rem</code> instead of <code>mod</code> when the remainder should always be positive.

```factor
USING: formatting kernel locals math math.primes math.ranges
sequences ;
IN: rosetta-code.carmichael

:: carmichael ( p1 -- )
    1 p1 (a,b) [| h3 |
        h3 p1 + [1,b) [| d |
            h3 p1 + p1 1 - * d mod zero?
            p1 neg p1 * h3 rem d h3 mod = and
            [
                p1 1 - h3 p1 + * d /i 1 +  :> p2
                p1 p2 * h3 /i 1 +          :> p3
                p2 p3 [ prime? ] both?
                p2 p3 * p1 1 - mod 1 = and
                [ p1 p2 p3 "%d %d %d\n" printf ] when
            ] when
        ] each
    ] each
;

: carmichael-demo ( -- ) 61 primes-upto [ carmichael ] each ;

MAIN: carmichael-demo
```

{{out}}

```txt

3 11 17
5 29 73
5 17 29
5 13 17
7 19 67
7 31 73
7 13 31
7 23 41
7 73 103
7 13 19
13 61 397
13 37 241
13 97 421
13 37 97
13 37 61
17 41 233
17 353 1201
19 43 409
19 199 271
23 199 353
29 113 1093
29 197 953
31 991 15361
31 61 631
31 151 1171
31 61 271
31 61 211
31 271 601
31 181 331
37 109 2017
37 73 541
37 613 1621
37 73 181
37 73 109
41 1721 35281
41 881 12041
41 101 461
41 241 761
41 241 521
41 73 137
41 61 101
43 631 13567
43 271 5827
43 127 2731
43 127 1093
43 211 757
43 631 1597
43 127 211
43 211 337
43 433 643
43 547 673
43 3361 3907
47 3359 6073
47 1151 1933
47 3727 5153
53 157 2081
53 79 599
53 157 521
59 1451 2089
61 421 12841
61 181 5521
61 1301 19841
61 277 2113
61 181 1381
61 541 3001
61 661 2521
61 271 571
61 241 421
61 3361 4021

```



## Fortran


### Plan

This is F77 style, and directly translates the given calculation as per ''formula translation''. It turns out that the normal integers suffice for the demonstration, except for just one of the products of the three primes: 41x1721x35281 = 2489462641, which is bigger than 2147483647, the 32-bit limit. Fortunately, INTEGER*8 variables are also available, so the extension is easy. Otherwise, one would have to mess about with using two integers in a bignum style, one holding say the millions, and the second the number up to a million.

### Source

So, using the double MOD approach (see the ''Discussion'') - which gives the same result for either style of MOD... 
```Fortran
      LOGICAL FUNCTION ISPRIME(N)	!Ad-hoc, since N is not going to be big...
       INTEGER N			!Despite this intimidating allowance of 32 bits...
       INTEGER F			!A possible factor.
        ISPRIME = .FALSE.		!Most numbers aren't prime.
        DO F = 2,SQRT(DFLOAT(N))	!Wince...
          IF (MOD(N,F).EQ.0) RETURN	!Not even avoiding even numbers beyond two.
        END DO				!Nice and brief, though.
        ISPRIME = .TRUE.		!No factor found.
      END FUNCTION ISPRIME		!So, done. Hopefully, not often.

      PROGRAM CHASE
      INTEGER P1,P2,P3	!The three primes to be tested.
      INTEGER H3,D	!Assistants.
      INTEGER MSG	!File unit number.
      MSG = 6		!Standard output.
      WRITE (MSG,1)	!A heading would be good.
    1 FORMAT ("Carmichael numbers that are the product of three primes:"
     & /"    P1  x P2  x P3 =",9X,"C")
      DO P1 = 2,61	!Step through the specified range.
        IF (ISPRIME(P1)) THEN	!Selecting only the primes.
          DO H3 = 2,P1 - 1		!For 1 < H3 < P1.
            DO D = 1,H3 + P1 - 1		!For 0 < D < H3 + P1.
              IF (MOD((H3 + P1)*(P1 - 1),D).EQ.0	!Filter.
     &        .AND. (MOD(H3 + MOD(-P1**2,H3),H3) .EQ. MOD(D,H3))) THEN	!Beware MOD for negative numbers! MOD(-P1**2, may surprise...
                P2 = 1 + (P1 - 1)*(H3 + P1)/D	!Candidate for the second prime.
                IF (ISPRIME(P2)) THEN		!Is it prime?
                  P3 = 1 + P1*P2/H3			!Yes. Candidate for the third prime.
                  IF (ISPRIME(P3)) THEN			!Is it prime?
                    IF (MOD(P2*P3,P1 - 1).EQ.1) THEN		!Yes! Final test.
                      WRITE (MSG,2) P1,P2,P3, INT8(P1)*P2*P3		!Result!
    2                 FORMAT (3I6,I12)
                    END IF
                  END IF
                END IF
              END IF
            END DO
          END DO
        END IF
      END DO
      END
```



### Output


```txt

Carmichael numbers that are the product of three primes:
    P1  x P2  x P3 =         C
     3    11    17         561
     5    29    73       10585
     5    17    29        2465
     5    13    17        1105
     7    19    67        8911
     7    31    73       15841
     7    13    31        2821
     7    23    41        6601
     7    73   103       52633
     7    13    19        1729
    13    61   397      314821
    13    37   241      115921
    13    97   421      530881
    13    37    97       46657
    13    37    61       29341
    17    41   233      162401
    17   353  1201     7207201
    19    43   409      334153
    19   199   271     1024651
    23   199   353     1615681
    29   113  1093     3581761
    29   197   953     5444489
    31   991 15361   471905281
    31    61   631     1193221
    31   151  1171     5481451
    31    61   271      512461
    31    61   211      399001
    31   271   601     5049001
    31   181   331     1857241
    37   109  2017     8134561
    37    73   541     1461241
    37   613  1621    36765901
    37    73   181      488881
    37    73   109      294409
    41  1721 35281  2489462641
    41   881 12041   434932961
    41   101   461     1909001
    41   241   761     7519441
    41   241   521     5148001
    41    73   137      410041
    41    61   101      252601
    43   631 13567   368113411
    43   271  5827    67902031
    43   127  2731    14913991
    43   127  1093     5968873
    43   211   757     6868261
    43   631  1597    43331401
    43   127   211     1152271
    43   211   337     3057601
    43   433   643    11972017
    43   547   673    15829633
    43  3361  3907   564651361
    47  3359  6073   958762729
    47  1151  1933   104569501
    47  3727  5153   902645857
    53   157  2081    17316001
    53    79   599     2508013
    53   157   521     4335241
    59  1451  2089   178837201
    61   421 12841   329769721
    61   181  5521    60957361
    61  1301 19841  1574601601
    61   277  2113    35703361
    61   181  1381    15247621
    61   541  3001    99036001
    61   661  2521   101649241
    61   271   571     9439201
    61   241   421     6189121
    61  3361  4021   824389441

```



## FreeBASIC


```freebasic
' version 17-10-2016
' compile with: fbc -s console

' using a sieve for finding primes

#Define max_sieve 10000000 ' 10^7
ReDim Shared As Byte isprime(max_sieve)

' translated the pseudo code to FreeBASIC 
Sub carmichael3(p1 As Integer) 

  If isprime(p1) = 0 Then Exit Sub

  Dim As Integer h3, d, p2, p3, t1, t2

  For h3 = 1 To p1 -1
    t1 = (h3 + p1) * (p1 -1)
    t2 = (-p1 * p1) Mod h3
    If t2 < 0 Then t2 = t2 + h3
    For d = 1 To h3 + p1 -1
      If t1 Mod d = 0 And t2 = (d Mod h3) Then
        p2 = 1 + (t1 \ d)
        If isprime(p2) = 0 Then Continue For
        p3 = 1 + (p1 * p2 \ h3)
        If isprime(p3) = 0 Or ((p2 * p3) Mod (p1 -1)) <> 1 Then Continue For
        Print Using "### * #### * #####"; p1; p2; p3
      End If
    Next d
  Next h3
End Sub


' ------=< MAIN >=------

Dim As UInteger i, j

'set up sieve
For i = 3 To max_sieve Step 2
  isprime(i) = 1
Next i

isprime(2) = 1
For i = 3 To Sqr(max_sieve) Step 2
  If isprime(i) = 1 Then
    For j = i * i To max_sieve Step i * 2
      isprime(j) = 0
    Next j
  End If
Next i

For i = 2 To 61
  carmichael3(i)
Next i

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
  3 *   11 *    17
  5 *   29 *    73
  5 *   17 *    29
  5 *   13 *    17
  7 *   19 *    67
  7 *   31 *    73
  7 *   13 *    31
  7 *   23 *    41
  7 *   73 *   103
  7 *   13 *    19
 13 *   61 *   397
 13 *   37 *   241
 13 *   97 *   421
 13 *   37 *    97
 13 *   37 *    61
 17 *   41 *   233
 17 *  353 *  1201
 19 *   43 *   409
 19 *  199 *   271
 23 *  199 *   353
 29 *  113 *  1093
 29 *  197 *   953
 31 *  991 * 15361
 31 *   61 *   631
 31 *  151 *  1171
 31 *   61 *   271
 31 *   61 *   211
 31 *  271 *   601
 31 *  181 *   331
 37 *  109 *  2017
 37 *   73 *   541
 37 *  613 *  1621
 37 *   73 *   181
 37 *   73 *   109
 41 * 1721 * 35281
 41 *  881 * 12041
 41 *  101 *   461
 41 *  241 *   761
 41 *  241 *   521
 41 *   73 *   137
 41 *   61 *   101
 43 *  631 * 13567
 43 *  271 *  5827
 43 *  127 *  2731
 43 *  127 *  1093
 43 *  211 *   757
 43 *  631 *  1597
 43 *  127 *   211
 43 *  211 *   337
 43 *  433 *   643
 43 *  547 *   673
 43 * 3361 *  3907
 47 * 3359 *  6073
 47 * 1151 *  1933
 47 * 3727 *  5153
 53 *  157 *  2081
 53 *   79 *   599
 53 *  157 *   521
 59 * 1451 *  2089
 61 *  421 * 12841
 61 *  181 *  5521
 61 * 1301 * 19841
 61 *  277 *  2113
 61 *  181 *  1381
 61 *  541 *  3001
 61 *  661 *  2521
 61 *  271 *   571
 61 *  241 *   421
 61 * 3361 *  4021
```



## Go


```go
package main

import "fmt"

// Use this rather than % for negative integers
func mod(n, m int) int {
    return ((n % m) + m) % m
}

func isPrime(n int) bool {
    if n < 2 { return false }
    if n % 2 == 0 { return n == 2 }
    if n % 3 == 0 { return n == 3 }
    d := 5
    for d * d <= n {
        if n % d == 0 { return false }
        d += 2
        if n % d == 0 { return false }
        d += 4
    }
    return true
}

func carmichael(p1 int) {
    for h3 := 2; h3 < p1; h3++ {
        for d := 1; d < h3 + p1; d++ {
            if (h3 + p1) * (p1 - 1) % d == 0 && mod(-p1 * p1, h3) == d % h3 {
                p2 := 1 + (p1 - 1) * (h3 + p1) / d
                if !isPrime(p2) { continue }
                p3 := 1 + p1 * p2 / h3
                if !isPrime(p3) { continue }
                if p2 * p3 % (p1 - 1) != 1 { continue }
                c := p1 * p2 * p3
                fmt.Printf("%2d   %4d   %5d     %d\n", p1, p2, p3, c)
            }
        }
    }
}

func main() {
    fmt.Println("The following are Carmichael munbers for p1 <= 61:\n")
    fmt.Println("p1     p2      p3     product")
    fmt.Println("==     ==      ==     
### =
")

    for p1 := 2; p1 <= 61; p1++ {
        if isPrime(p1) { carmichael(p1) }
    }
}
```


{{out}}

```txt

The following are Carmichael munbers for p1 <= 61:

p1     p2      p3     product
==     ==      ==     
### =

 3     11      17     561
 5     29      73     10585
 5     17      29     2465
 5     13      17     1105
 7     19      67     8911
 7     31      73     15841
 7     13      31     2821
 7     23      41     6601
 7     73     103     52633
 7     13      19     1729
13     61     397     314821
13     37     241     115921
13     97     421     530881
13     37      97     46657
13     37      61     29341
17     41     233     162401
17    353    1201     7207201
19     43     409     334153
19    199     271     1024651
23    199     353     1615681
29    113    1093     3581761
29    197     953     5444489
31    991   15361     471905281
31     61     631     1193221
31    151    1171     5481451
31     61     271     512461
31     61     211     399001
31    271     601     5049001
31    181     331     1857241
37    109    2017     8134561
37     73     541     1461241
37    613    1621     36765901
37     73     181     488881
37     73     109     294409
41   1721   35281     2489462641
41    881   12041     434932961
41    101     461     1909001
41    241     761     7519441
41    241     521     5148001
41     73     137     410041
41     61     101     252601
43    631   13567     368113411
43    271    5827     67902031
43    127    2731     14913991
43    127    1093     5968873
43    211     757     6868261
43    631    1597     43331401
43    127     211     1152271
43    211     337     3057601
43    433     643     11972017
43    547     673     15829633
43   3361    3907     564651361
47   3359    6073     958762729
47   1151    1933     104569501
47   3727    5153     902645857
53    157    2081     17316001
53     79     599     2508013
53    157     521     4335241
59   1451    2089     178837201
61    421   12841     329769721
61    181    5521     60957361
61   1301   19841     1574601601
61    277    2113     35703361
61    181    1381     15247621
61    541    3001     99036001
61    661    2521     101649241
61    271     571     9439201
61    241     421     6189121
61   3361    4021     824389441

```



## Haskell

{{trans|Ruby}}
{{libheader|primes}}
{{Works with|GHC|7.4.1}}
{{Works with|primes|0.2.1.0}}

```haskell
#!/usr/bin/runhaskell

import Data.Numbers.Primes
import Control.Monad (guard)

carmichaels = do
  p <- takeWhile (<= 61) primes
  h3 <- [2..(p-1)]
  let g = h3 + p
  d <- [1..(g-1)]
  guard $ (g * (p - 1)) `mod` d == 0 && (-1 * p * p) `mod` h3 == d `mod` h3
  let q = 1 + (((p - 1) * g) `div` d)
  guard $ isPrime q
  let r = 1 + ((p * q) `div` h3)
  guard $ isPrime r && (q * r) `mod` (p - 1) == 1
  return (p, q, r)

main = putStr $ unlines $ map show carmichaels
```

{{out}}

```txt

(3,11,17)
(5,29,73)
(5,17,29)
(5,13,17)
(7,19,67)
(7,31,73)
(7,13,31)
(7,23,41)
(7,73,103)
(7,13,19)
(13,61,397)
(13,37,241)
(13,97,421)
(13,37,97)
(13,37,61)
(17,41,233)
(17,353,1201)
(19,43,409)
(19,199,271)
(23,199,353)
(29,113,1093)
(29,197,953)
(31,991,15361)
(31,61,631)
(31,151,1171)
(31,61,271)
(31,61,211)
(31,271,601)
(31,181,331)
(37,109,2017)
(37,73,541)
(37,613,1621)
(37,73,181)
(37,73,109)
(41,1721,35281)
(41,881,12041)
(41,101,461)
(41,241,761)
(41,241,521)
(41,73,137)
(41,61,101)
(43,631,13567)
(43,271,5827)
(43,127,2731)
(43,127,1093)
(43,211,757)
(43,631,1597)
(43,127,211)
(43,211,337)
(43,433,643)
(43,547,673)
(43,3361,3907)
(47,3359,6073)
(47,1151,1933)
(47,3727,5153)
(53,157,2081)
(53,79,599)
(53,157,521)
(59,1451,2089)
(61,421,12841)
(61,181,5521)
(61,1301,19841)
(61,277,2113)
(61,181,1381)
(61,541,3001)
(61,661,2521)
(61,271,571)
(61,241,421)
(61,3361,4021)

```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.

```unicon
link "factors"

procedure main(A)
    n := integer(!A) | 61
    every write(carmichael3(!n))
end

procedure carmichael3(p1)
    every (isprime(p1), (h := 1+!(p1-1)), (d := !(h+p1-1))) do
        if (mod(((h+p1)*(p1-1)),d) = 0, mod((-p1*p1),h) = mod(d,h)) then {
            p2 := 1 + (p1-1)*(h+p1)/d
            p3 := 1 + p1*p2/h
            if (isprime(p2), isprime(p3), mod((p2*p3),(p1-1)) = 1) then
                suspend format(p1,p2,p3)
            }
end

procedure mod(n,d)
   return (d+n%d)%d
end

procedure format(p1,p2,p3)
    return left(p1||" * "||p2||" * "||p3,20)||" = "||(p1*p2*p3)
end
```


Output, with middle lines elided:

```txt

->c3sp
3 * 11 * 17          = 561
5 * 29 * 73          = 10585
5 * 17 * 29          = 2465
5 * 13 * 17          = 1105
7 * 19 * 67          = 8911
7 * 31 * 73          = 15841
7 * 13 * 31          = 2821
7 * 23 * 41          = 6601
7 * 73 * 103         = 52633
7 * 13 * 19          = 1729
13 * 61 * 397        = 314821
13 * 37 * 241        = 115921
...
53 * 157 * 2081      = 17316001
53 * 79 * 599        = 2508013
53 * 157 * 521       = 4335241
59 * 1451 * 2089     = 178837201
61 * 421 * 12841     = 329769721
61 * 181 * 5521      = 60957361
61 * 1301 * 19841    = 1574601601
61 * 277 * 2113      = 35703361
61 * 181 * 1381      = 15247621
61 * 541 * 3001      = 99036001
61 * 661 * 2521      = 101649241
61 * 271 * 571       = 9439201
61 * 241 * 421       = 6189121
61 * 3361 * 4021     = 824389441
->

```



## J


```J

q =: (,"0 1~ >:@i.@<:@+/"1)&.>@(,&.>"0 1~ >:@i.)&.>@I.@(1&p:@i.)@>:
f1 =: (0: = {. | <:@{: * 1&{ + {:) *. ((1&{ | -@*:@{:) = 1&{ | {.)
f2 =: 1: = <:@{. | ({: * 1&{)
p2 =: 0:`((* 1&p:)@(<.@(1: + <:@{: * {. %~ 1&{ + {:)))@.f1
p3 =: 3:$0:`((* 1&p:)@({: , {. , (<.@>:@(1&{ %~ {. * {:))))@.(*@{.)@(p2 , }.)
(-. 3:$0:)@(((*"0 f2)@p3"1)@;@;@q) 61

```

Output

```txt

 3   11    17
 5   29    73
 5   17    29
 5   13    17
 7   19    67
 7   31    73
 7   13    31
 7   23    41
 7   73   103
 7   13    19
13   61   397
13   37   241
13   97   421
13   37    97
13   37    61
17   41   233
17  353  1201
19   43   409
19  199   271
23  199   353
29  113  1093
29  197   953
31  991 15361
31   61   631
31  151  1171
31   61   271
31   61   211
31  271   601
31  181   331
37  109  2017
37   73   541
37  613  1621
37   73   181
37   73   109
41 1721 35281
41  881 12041
41  101   461
41  241   761
41  241   521
41   73   137
41   61   101
43  631 13567
43  271  5827
43  127  2731
43  127  1093
43  211   757
43  631  1597
43  127   211
43  211   337
43  433   643
43  547   673
43 3361  3907
47 3359  6073
47 1151  1933
47 3727  5153
53  157  2081
53   79   599
53  157   521
59 1451  2089
61  421 12841
61  181  5521
61 1301 19841
61  277  2113
61  181  1381
61  541  3001
61  661  2521
61  271   571
61  241   421
61 3361  4021

```



## Java

{{trans|D}}

```java
public class Test {

    static int mod(int n, int m) {
        return ((n % m) + m) % m;
    }

    static boolean isPrime(int n) {
        if (n == 2 || n == 3)
            return true;
        else if (n < 2 || n % 2 == 0 || n % 3 == 0)
            return false;
        for (int div = 5, inc = 2; Math.pow(div, 2) <= n;
                div += inc, inc = 6 - inc)
            if (n % div == 0)
                return false;
        return true;
    }

    public static void main(String[] args) {
        for (int p = 2; p < 62; p++) {
            if (!isPrime(p))
                continue;
            for (int h3 = 2; h3 < p; h3++) {
                int g = h3 + p;
                for (int d = 1; d < g; d++) {
                    if ((g * (p - 1)) % d != 0 || mod(-p * p, h3) != d % h3)
                        continue;
                    int q = 1 + (p - 1) * g / d;
                    if (!isPrime(q))
                        continue;
                    int r = 1 + (p * q / h3);
                    if (!isPrime(r) || (q * r) % (p - 1) != 1)
                        continue;
                    System.out.printf("%d x %d x %d%n", p, q, r);
                }
            }
        }
    }
}
```


```txt
3 x 11 x 17
5 x 29 x 73
5 x 17 x 29
5 x 13 x 17
7 x 19 x 67
7 x 31 x 73
7 x 13 x 31
7 x 23 x 41
7 x 73 x 103
7 x 13 x 19
13 x 61 x 397
13 x 37 x 241
13 x 97 x 421
13 x 37 x 97
13 x 37 x 61
17 x 41 x 233
17 x 353 x 1201
19 x 43 x 409
19 x 199 x 271
23 x 199 x 353
29 x 113 x 1093
29 x 197 x 953
31 x 991 x 15361
31 x 61 x 631
31 x 151 x 1171
31 x 61 x 271
31 x 61 x 211
31 x 271 x 601
31 x 181 x 331
37 x 109 x 2017
37 x 73 x 541
37 x 613 x 1621
37 x 73 x 181
37 x 73 x 109
41 x 1721 x 35281
41 x 881 x 12041
41 x 101 x 461
41 x 241 x 761
41 x 241 x 521
41 x 73 x 137
41 x 61 x 101
43 x 631 x 13567
43 x 271 x 5827
43 x 127 x 2731
43 x 127 x 1093
43 x 211 x 757
43 x 631 x 1597
43 x 127 x 211
43 x 211 x 337
43 x 433 x 643
43 x 547 x 673
43 x 3361 x 3907
47 x 3359 x 6073
47 x 1151 x 1933
47 x 3727 x 5153
53 x 157 x 2081
53 x 79 x 599
53 x 157 x 521
59 x 1451 x 2089
61 x 421 x 12841
61 x 181 x 5521
61 x 1301 x 19841
61 x 277 x 2113
61 x 181 x 1381
61 x 541 x 3001
61 x 661 x 2521
61 x 271 x 571
61 x 241 x 421
61 x 3361 x 4021
```



## Julia

This solution is a straightforward implementation of the algorithm of the Jameson paper cited in the task description.  Just for fun, I use Julia's capacity to accommodate Unicode identifiers to match some of the paper's symbols to the variables used in the <tt>carmichael</tt> function.

'''Function'''

```julia
using Primes

function carmichael(pmax::Integer)
    if pmax ≤ 0 throw(DomainError("pmax must be strictly positive")) end
    car = Vector{typeof(pmax)}(0)
    for p in primes(pmax)
        for h₃ in 2:(p-1)
            m = (p - 1) * (h₃ + p)
            pmh = mod(-p ^ 2, h₃)
            for Δ in 1:(h₃+p-1)
                if m % Δ != 0 || Δ % h₃ != pmh continue end
                q = m ÷ Δ + 1
                if !isprime(q) continue end
                r = (p * q - 1) ÷ h₃ + 1
                if !isprime(r) || mod(q * r, p - 1) == 1 continue end
                append!(car, [p, q, r])
            end
        end
    end
    return reshape(car, 3, length(car) ÷ 3)
end
```


'''Main'''

```julia
hi = 61
car = carmichael(hi)

curp = tcnt = 0
print("Carmichael 3 (p×q×r) pseudoprimes, up to p = $hi:")
for j in sortperm(1:size(car)[2], by=x->(car[1,x], car[2,x], car[3,x]))
    p, q, r = car[:, j]
    c = prod(car[:, j])
    if p != curp
        curp = p
        @printf("\n\np = %d\n  ", p)
        tcnt = 0
    end
    if tcnt == 4
        print("\n  ")
        tcnt = 1
    else
        tcnt += 1
    end
    @printf("p× %d × %d = %d  ", q, r, c)
end
println("\n\n", size(car)[2], " results in total.")
```


{{out}}

```txt
Carmichael 3 (p×q×r) pseudoprimes, up to p = 61:


p = 11
  p× 29 × 107 = 34133  p× 37 × 59 = 24013  

p = 17
  p× 23 × 79 = 30889  p× 53 × 101 = 91001  

p = 19
  p× 59 × 113 = 126673  p× 139 × 661 = 1745701  p× 193 × 283 = 1037761  

p = 23
  p× 43 × 53 = 52417  p× 59 × 227 = 308039  p× 71 × 137 = 223721  p× 83 × 107 = 204263  

p = 29
  p× 41 × 109 = 129601  p× 89 × 173 = 446513  p× 97 × 149 = 419137  p× 149 × 541 = 2337661  

p = 31
  p× 67 × 1039 = 2158003  p× 73 × 79 = 178777  p× 79 × 307 = 751843  p× 223 × 1153 = 7970689  
  p× 313 × 463 = 4492489  

p = 41
  p× 89 × 1217 = 4440833  p× 97 × 569 = 2262913  

p = 43
  p× 67 × 241 = 694321  p× 107 × 461 = 2121061  p× 131 × 257 = 1447681  p× 139 × 1993 = 11912161  
  p× 157 × 751 = 5070001  p× 199 × 373 = 3191761  

p = 47
  p× 53 × 499 = 1243009  p× 89 × 103 = 430849  p× 101 × 1583 = 7514501  p× 107 × 839 = 4219331  
  p× 157 × 239 = 1763581  

p = 53
  p× 113 × 1997 = 11960033  p× 197 × 233 = 2432753  p× 281 × 877 = 13061161  

p = 59
  p× 131 × 1289 = 9962681  p× 139 × 821 = 6733021  p× 149 × 587 = 5160317  p× 173 × 379 = 3868453  
  p× 179 × 353 = 3728033  

p = 61
  p× 1009 × 2677 = 164766673  

42 results in total.
```



## Kotlin

{{trans|D}}

```scala
fun Int.isPrime(): Boolean {
    return when {
        this == 2 -> true
        this <= 1 || this % 2 == 0 -> false
        else -> {
            val max = Math.sqrt(toDouble()).toInt()
            (3..max step 2)
                .filter { this % it == 0 }
                .forEach { return false }
            true
        }
    }
}

fun mod(n: Int, m: Int) = ((n % m) + m) % m

fun main(args: Array<String>) {
    for (p1 in 3..61) {
        if (p1.isPrime()) {
            for (h3 in 2 until p1) {
                val g = h3 + p1
                for (d in 1 until g) {
                    if ((g * (p1 - 1)) % d == 0 && mod(-p1 * p1, h3) == d % h3) {
                        val q = 1 + (p1 - 1) * g / d
                        if (q.isPrime()) {
                            val r = 1 + (p1 * q / h3)
                            if (r.isPrime() && (q * r) % (p1 - 1) == 1) {
                                println("$p1 x $q x $r")
                            }
                        }
                    }
                }
            }
        }
    }
}
```

{{out}}
See D output.

=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
Cases[Cases[
  Cases[Table[{p1, h3, d}, {p1, Array[Prime, PrimePi@61]}, {h3, 2, 
     p1 - 1}, {d, 1, h3 + p1 - 1}], {p1_Integer, h3_, d_} /; 
     PrimeQ[1 + (p1 - 1) (h3 + p1)/d] && 
      Divisible[p1^2 + d, h3] :> {p1, 1 + (p1 - 1) (h3 + p1)/d, h3}, 
   Infinity], {p1_, p2_, h3_} /; PrimeQ[1 + Floor[p1 p2/h3]] :> {p1, 
    p2, 1 + Floor[p1 p2/h3]}], {p1_, p2_, p3_} /; 
   Mod[p2 p3, p1 - 1] == 1 :> Print[p1, "*", p2, "*", p3]]
```



## PARI/GP


```parigp
f(p)={
  my(v=List(),q,r);
  for(h=2,p-1,
    for(d=1,h+p-1,
      if((h+p)*(p-1)%d==0 && Mod(p,h)^2==-d && isprime(q=(p-1)*(h+p)/d+1) && isprime(r=p*q\h+1)&&q*r%(p-1)==1,
        listput(v,p*q*r)
      )
    )
  );
  Set(v)
};
forprime(p=3,67,v=f(p); for(i=1,#v,print1(v[i]", ")))
```

{{out}}

```txt
561, 1105, 2465, 10585, 1729, 2821, 6601, 8911, 15841, 52633, 29341, 46657, 115921, 314821, 530881, 162401, 7207201, 334153, 1024651, 1615681, 3581761, 5444489, 399001, 512461, 1193221, 1857241, 5049001, 5481451, 471905281, 294409, 488881, 1461241, 8134561, 36765901, 252601, 410041, 1909001, 5148001, 7519441, 434932961, 2489462641, 1152271, 3057601, 5968873, 6868261, 11972017, 14913991, 15829633, 43331401, 67902031, 368113411, 564651361, 104569501, 902645857, 958762729, 2508013, 4335241, 17316001, 178837201, 6189121, 9439201, 15247621, 35703361, 60957361, 99036001, 101649241, 329769721, 824389441, 1574601601, 10267951, 163954561, 7991602081,
```



## Perl

{{libheader|ntheory}}

```perl
use ntheory qw/forprimes is_prime vecprod/;

forprimes { my $p = $_;
   for my $h3 (2 .. $p-1) {
      my $ph3 = $p + $h3;
      for my $d (1 .. $ph3-1) {               # Jameseon procedure page 6
         next if ((-$p*$p) % $h3) != ($d % $h3);
         next if (($p-1)*$ph3) % $d;
         my $q = 1 + ($p-1)*$ph3 / $d;        # Jameson eq 7
         next unless is_prime($q);
         my $r = 1 + ($p*$q-1) / $h3;         # Jameson eq 6
         next unless is_prime($r);
         next unless ($q*$r) % ($p-1) == 1;
         printf "%2d x %5d x %5d = %s\n",$p,$q,$r,vecprod($p,$q,$r);
      }
   }
} 3,61;
```

{{out}}

```txt

 3 x    11 x    17 = 561
 5 x    29 x    73 = 10585
 5 x    17 x    29 = 2465
 5 x    13 x    17 = 1105
 ... full output is 69 lines ...
61 x   661 x  2521 = 101649241
61 x   271 x   571 = 9439201
61 x   241 x   421 = 6189121
61 x  3361 x  4021 = 824389441

```



## Perl 6

{{works with|Rakudo|2015.12}}
An almost direct translation of the pseudocode.  We take the liberty of going up to 67 to show we aren't limited to 32-bit integers.  (Perl 6 uses arbitrary precision in any case.)

```perl6
for (2..67).grep: *.is-prime -> \Prime1 {
    for 1 ^..^ Prime1 -> \h3 {
        my \g = h3 + Prime1;
        for 0 ^..^ h3 + Prime1 -> \d {
            if (h3 + Prime1) * (Prime1 - 1) %% d and -Prime1**2 % h3 == d % h3  {
                my \Prime2 = floor 1 + (Prime1 - 1) * g / d;
                next unless Prime2.is-prime;
                my \Prime3 = floor 1 + Prime1 * Prime2 / h3;
                next unless Prime3.is-prime;
                next unless (Prime2 * Prime3) % (Prime1 - 1) == 1;
                say "{Prime1} × {Prime2} × {Prime3} == {Prime1 * Prime2 * Prime3}";
            }
        }
    }
}
```

{{out}}

```txt
3 × 11 × 17 == 561
5 × 29 × 73 == 10585
5 × 17 × 29 == 2465
5 × 13 × 17 == 1105
7 × 19 × 67 == 8911
7 × 31 × 73 == 15841
7 × 13 × 31 == 2821
7 × 23 × 41 == 6601
7 × 73 × 103 == 52633
7 × 13 × 19 == 1729
13 × 61 × 397 == 314821
13 × 37 × 241 == 115921
13 × 97 × 421 == 530881
13 × 37 × 97 == 46657
13 × 37 × 61 == 29341
17 × 41 × 233 == 162401
17 × 353 × 1201 == 7207201
19 × 43 × 409 == 334153
19 × 199 × 271 == 1024651
23 × 199 × 353 == 1615681
29 × 113 × 1093 == 3581761
29 × 197 × 953 == 5444489
31 × 991 × 15361 == 471905281
31 × 61 × 631 == 1193221
31 × 151 × 1171 == 5481451
31 × 61 × 271 == 512461
31 × 61 × 211 == 399001
31 × 271 × 601 == 5049001
31 × 181 × 331 == 1857241
37 × 109 × 2017 == 8134561
37 × 73 × 541 == 1461241
37 × 613 × 1621 == 36765901
37 × 73 × 181 == 488881
37 × 73 × 109 == 294409
41 × 1721 × 35281 == 2489462641
41 × 881 × 12041 == 434932961
41 × 101 × 461 == 1909001
41 × 241 × 761 == 7519441
41 × 241 × 521 == 5148001
41 × 73 × 137 == 410041
41 × 61 × 101 == 252601
43 × 631 × 13567 == 368113411
43 × 271 × 5827 == 67902031
43 × 127 × 2731 == 14913991
43 × 127 × 1093 == 5968873
43 × 211 × 757 == 6868261
43 × 631 × 1597 == 43331401
43 × 127 × 211 == 1152271
43 × 211 × 337 == 3057601
43 × 433 × 643 == 11972017
43 × 547 × 673 == 15829633
43 × 3361 × 3907 == 564651361
47 × 3359 × 6073 == 958762729
47 × 1151 × 1933 == 104569501
47 × 3727 × 5153 == 902645857
53 × 157 × 2081 == 17316001
53 × 79 × 599 == 2508013
53 × 157 × 521 == 4335241
59 × 1451 × 2089 == 178837201
61 × 421 × 12841 == 329769721
61 × 181 × 5521 == 60957361
61 × 1301 × 19841 == 1574601601
61 × 277 × 2113 == 35703361
61 × 181 × 1381 == 15247621
61 × 541 × 3001 == 99036001
61 × 661 × 2521 == 101649241
61 × 271 × 571 == 9439201
61 × 241 × 421 == 6189121
61 × 3361 × 4021 == 824389441
67 × 2311 × 51613 == 7991602081
67 × 331 × 7393 == 163954561
67 × 331 × 463 == 10267951
```



## Phix

Uses is_prime() from [[Extensible_prime_generator#Phix|Extensible_prime_generator]]

```Phix
integer count = 0
for p1=1 to 61 do
    if is_prime(p1) then
        for h3=1 to p1 do
            atom h3p1 = h3 + p1
            for d=1 to h3p1-1 do
                if mod(h3p1*(p1-1),d)=0
                and mod(-(p1*p1),h3) = mod(d,h3) then
                    atom p2 := 1 + floor(((p1-1)*h3p1)/d),
                         p3 := 1 +floor(p1*p2/h3)
                    if is_prime(p2) 
                    and is_prime(p3)
                    and mod(p2*p3,p1-1)=1 then
                        if count<5 or count>55 then
                            printf(1,"%d * %d * %d = %d\n",{p1,p2,p3,p1*p2*p3})
                        elsif count=35 then puts(1,"...\n") end if
                        count += 1
                    end if
                end if
            end for
        end for
    end if
end for
printf(1,"%d Carmichael numbers found\n",count)
```

{{out}}

```txt

3 * 11 * 17 = 561
5 * 29 * 73 = 10585
5 * 17 * 29 = 2465
5 * 13 * 17 = 1105
7 * 19 * 67 = 8911
...
61 * 271 * 571 = 9439201
61 * 241 * 421 = 6189121
61 * 3361 * 4021 = 824389441
69 Carmichael numbers found

```



## PicoLisp


```PicoLisp
(de modulo (X Y)
   (% (+ Y (% X Y)) Y) )
 
(de prime? (N)
   (let D 0
      (or
         (= N 2)
         (and
            (> N 1)
            (bit? 1 N)
            (for (D 3  T  (+ D 2))
               (T (> D (sqrt N)) T)
               (T (=0 (% N D)) NIL) ) ) ) ) )
 
(for P1 61
   (when (prime? P1)
      (for (H3 2 (> P1 H3) (inc H3))
         (let G (+ H3 P1)
            (for (D 1 (> G D) (inc D))
               (when
                  (and
                     (=0
                        (% (* G (dec P1)) D) )
                     (=
                        (modulo (* (- P1) P1) H3)
                        (% D H3)) )
                  (let
                     (P2
                        (inc
                           (/ (* (dec P1) G) D) )
                        P3 (inc (/ (* P1 P2) H3)) )
                     (when
                        (and
                           (prime? P2)
                           (prime? P3)
                           (= 1 (modulo (* P2 P3) (dec P1))) )
                        (print (list P1 P2 P3)) ) ) ) ) ) ) ) )
(prinl)
 
(bye)
```



## PL/I


```PL/I
Carmichael: procedure options (main, reorder);  /* 24 January 2014 */
   declare (Prime1, Prime2, Prime3, h3, d) fixed binary (31);

   put ('Carmichael numbers are:');

   do Prime1 = 1 to 61;

      do h3 = 2 to Prime1;

d_loop:  do d = 1 to h3+Prime1-1;
            if (mod((h3+Prime1)*(Prime1-1), d) = 0) &
               (mod(-Prime1*Prime1, h3) = mod(d, h3)) then
               do;
                  Prime2 = (Prime1-1) * (h3+Prime1)/d; Prime2 = Prime2 + 1;
                  if ^is_prime(Prime2) then iterate d_loop;
                  Prime3 = Prime1*Prime2/h3; Prime3 = Prime3 + 1;
                  if ^is_prime(Prime3) then iterate d_loop;
                  if mod(Prime2*Prime3, Prime1-1) ^= 1 then iterate d_loop;
                  put skip edit (trim(Prime1), ' x ', trim(Prime2), ' x ', trim(Prime3)) (A);
               end;
         end;
      end;
   end;

   /* Uses is_prime from Rosetta Code PL/I. */

end Carmichael;
```

Results:

```txt

Carmichael numbers are: 
3 x 11 x 17
5 x 29 x 73
5 x 17 x 29
5 x 13 x 17
7 x 19 x 67
7 x 31 x 73
7 x 13 x 31
7 x 23 x 41
7 x 73 x 103
7 x 13 x 19
9 x 89 x 401
9 x 29 x 53
13 x 61 x 397
13 x 37 x 241
13 x 97 x 421
13 x 37 x 97
13 x 37 x 61
17 x 41 x 233
17 x 353 x 1201
19 x 43 x 409
19 x 199 x 271
21 x 761 x 941
23 x 199 x 353
27 x 131 x 443
27 x 53 x 131
29 x 113 x 1093
29 x 197 x 953
31 x 991 x 15361
31 x 61 x 631
31 x 151 x 1171
31 x 61 x 271
31 x 61 x 211
31 x 271 x 601
31 x 181 x 331
35 x 647 x 7549
35 x 443 x 3877
37 x 109 x 2017
37 x 73 x 541
37 x 613 x 1621
37 x 73 x 181
37 x 73 x 109
41 x 1721 x 35281
41 x 881 x 12041
41 x 101 x 461
41 x 241 x 761
41 x 241 x 521
41 x 73 x 137
41 x 61 x 101
43 x 631 x 13567
43 x 271 x 5827
43 x 127 x 2731
43 x 127 x 1093
43 x 211 x 757
43 x 631 x 1597
43 x 127 x 211
43 x 211 x 337
43 x 433 x 643
43 x 547 x 673
43 x 3361 x 3907
47 x 3359 x 6073
47 x 1151 x 1933
47 x 3727 x 5153
49 x 313 x 5113
49 x 97 x 433
51 x 701 x 7151
53 x 157 x 2081
53 x 79 x 599
53 x 157 x 521
55 x 3079 x 84673
55 x 163 x 4483
55 x 1567 x 28729
55 x 109 x 1999
55 x 433 x 2647
55 x 919 x 3889
55 x 139 x 547
55 x 3889 x 12583
55 x 109 x 163
55 x 433 x 487
57 x 113 x 1289
57 x 113 x 281
57 x 4649 x 10193
59 x 1451 x 2089
61 x 421 x 12841
61 x 181 x 5521
61 x 1301 x 19841
61 x 277 x 2113
61 x 181 x 1381
61 x 541 x 3001
61 x 661 x 2521
61 x 271 x 571
61 x 241 x 421
61 x 3361 x 4021

```



## Python


```python
class Isprime():
    '''
    Extensible sieve of Eratosthenes
    
    >>> isprime.check(11)
    True
    >>> isprime.multiples
    {2, 4, 6, 8, 9, 10}
    >>> isprime.primes
    [2, 3, 5, 7, 11]
    >>> isprime(13)
    True
    >>> isprime.multiples
    {2, 4, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 18, 20, 21, 22}
    >>> isprime.primes
    [2, 3, 5, 7, 11, 13, 17, 19]
    >>> isprime.nmax
    22
    >>> 
    '''
    multiples = {2}
    primes = [2]
    nmax = 2
    
    def __init__(self, nmax):
        if nmax > self.nmax:
            self.check(nmax)

    def check(self, n):
        if type(n) == float:
            if not n.is_integer(): return False
            n = int(n)
        multiples = self.multiples
        if n <= self.nmax:
            return n not in multiples
        else:
            # Extend the sieve
            primes, nmax = self.primes, self.nmax
            newmax = max(nmax*2, n)
            for p in primes:
                multiples.update(range(p*((nmax + p + 1) // p), newmax+1, p))
            for i in range(nmax+1, newmax+1):
                if i not in multiples:
                    primes.append(i)
                    multiples.update(range(i*2, newmax+1, i))
            self.nmax = newmax
            return n not in multiples

    __call__ = check
            
        
def carmichael(p1):
    ans = []
    if isprime(p1):
        for h3 in range(2, p1):
            g = h3 + p1
            for d in range(1, g):
                if (g * (p1 - 1)) % d == 0 and (-p1 * p1) % h3 == d % h3:
                    p2 = 1 + ((p1 - 1)* g // d)
                    if isprime(p2):
                        p3 = 1 + (p1 * p2 // h3)
                        if isprime(p3):
                            if (p2 * p3) % (p1 - 1) == 1:
                                #print('%i X %i X %i' % (p1, p2, p3))
                                ans += [tuple(sorted((p1, p2, p3)))]
    return ans
                
isprime = Isprime(2)
 
ans = sorted(sum((carmichael(n) for n in range(62) if isprime(n)), []))
print(',\n'.join(repr(ans[i:i+5])[1:-1] for i in range(0, len(ans)+1, 5)))
```

{{out}}

```txt
(3, 11, 17), (5, 13, 17), (5, 17, 29), (5, 29, 73), (7, 13, 19),
(7, 13, 31), (7, 19, 67), (7, 23, 41), (7, 31, 73), (7, 73, 103),
(13, 37, 61), (13, 37, 97), (13, 37, 241), (13, 61, 397), (13, 97, 421),
(17, 41, 233), (17, 353, 1201), (19, 43, 409), (19, 199, 271), (23, 199, 353),
(29, 113, 1093), (29, 197, 953), (31, 61, 211), (31, 61, 271), (31, 61, 631),
(31, 151, 1171), (31, 181, 331), (31, 271, 601), (31, 991, 15361), (37, 73, 109),
(37, 73, 181), (37, 73, 541), (37, 109, 2017), (37, 613, 1621), (41, 61, 101),
(41, 73, 137), (41, 101, 461), (41, 241, 521), (41, 241, 761), (41, 881, 12041),
(41, 1721, 35281), (43, 127, 211), (43, 127, 1093), (43, 127, 2731), (43, 211, 337),
(43, 211, 757), (43, 271, 5827), (43, 433, 643), (43, 547, 673), (43, 631, 1597),
(43, 631, 13567), (43, 3361, 3907), (47, 1151, 1933), (47, 3359, 6073), (47, 3727, 5153),
(53, 79, 599), (53, 157, 521), (53, 157, 2081), (59, 1451, 2089), (61, 181, 1381),
(61, 181, 5521), (61, 241, 421), (61, 271, 571), (61, 277, 2113), (61, 421, 12841),
(61, 541, 3001), (61, 661, 2521), (61, 1301, 19841), (61, 3361, 4021)
```



## Racket


```racket

#lang racket
(require math)

(for ([p1 (in-range 3 62)] #:when (prime? p1))
  (for ([h3 (in-range 2 p1)])
    (define g (+ p1 h3))
    (let next ([d 1])
      (when (< d g)
        (when (and (zero? (modulo (* g (- p1 1)) d))
                   (= (modulo (- (sqr p1)) h3) (modulo d h3)))
          (define p2 (+ 1 (quotient (* g (- p1 1)) d)))
          (when (prime? p2)
            (define p3 (+ 1 (quotient (* p1 p2) h3)))
            (when (and (prime? p3) (= 1 (modulo (* p2 p3) (- p1 1))))
              (displayln (list p1 p2 p3 '=> (* p1 p2 p3))))))
        (next (+ d 1))))))

```

Output:

```racket

(3 11 17 => 561)
(5 29 73 => 10585)
(5 17 29 => 2465)
(5 13 17 => 1105)
(7 19 67 => 8911)
(7 31 73 => 15841)
(7 23 41 => 6601)
(7 73 103 => 52633)
(13 61 397 => 314821)
(13 97 421 => 530881)
(13 37 97 => 46657)
(13 37 61 => 29341)
(17 41 233 => 162401)
(17 353 1201 => 7207201)
(19 43 409 => 334153)
(19 199 271 => 1024651)
(23 199 353 => 1615681)
(29 113 1093 => 3581761)
(29 197 953 => 5444489)
(31 991 15361 => 471905281)
(31 61 631 => 1193221)
(31 151 1171 => 5481451)
(31 61 271 => 512461)
(31 61 211 => 399001)
(31 271 601 => 5049001)
(31 181 331 => 1857241)
(37 109 2017 => 8134561)
(37 73 541 => 1461241)
(37 613 1621 => 36765901)
(37 73 181 => 488881)
(37 73 109 => 294409)
(41 1721 35281 => 2489462641)
(41 881 12041 => 434932961)
(41 101 461 => 1909001)
(41 241 761 => 7519441)
(41 241 521 => 5148001)
(41 73 137 => 410041)
(41 61 101 => 252601)
(43 631 13567 => 368113411)
(43 127 1093 => 5968873)
(43 211 757 => 6868261)
(43 631 1597 => 43331401)
(43 127 211 => 1152271)
(43 211 337 => 3057601)
(43 433 643 => 11972017)
(43 547 673 => 15829633)
(43 3361 3907 => 564651361)
(47 3359 6073 => 958762729)
(47 1151 1933 => 104569501)
(47 3727 5153 => 902645857)
(53 157 2081 => 17316001)
(53 79 599 => 2508013)
(53 157 521 => 4335241)
(59 1451 2089 => 178837201)
(61 421 12841 => 329769721)
(61 1301 19841 => 1574601601)
(61 277 2113 => 35703361)
(61 541 3001 => 99036001)
(61 661 2521 => 101649241)
(61 271 571 => 9439201)
(61 241 421 => 6189121)
(61 3361 4021 => 824389441)

```



## REXX

Note that REXX's version of   '''modulus'''    (<big><code>'''//'''</code></big>)   is really a   ''remainder''   function. 

The Carmichael numbers are shown in numerical order.

Some code optimization was done, while not necessary for the small default number ('''61'''),   it was significant for larger numbers.

```rexx
/*REXX program calculates  Carmichael  3─strong  pseudoprimes  (up to and including N). */
numeric digits 18                                /*handle big dig #s (9 is the default).*/
parse arg N .;    if N=='' | N==","  then N=61   /*allow user to specify for the search.*/
tell= N>0;           N= abs(N)                   /*N>0?  Then display Carmichael numbers*/
#= 0                                             /*number of Carmichael numbers so far. */
@.=0;   @.2=1; @.3=1; @.5=1; @.7=1; @.11=1; @.13=1; @.17=1; @.19=1; @.23=1; @.29=1; @.31=1
                                                 /*[↑]  prime number memoization array. */
    do p=3  to N  by 2;  pm= p-1;  bot=0;  top=0 /*step through some (odd) prime numbers*/
    if \isPrime(p)  then iterate;  nps= -p*p     /*is   P   a prime?   No, then skip it.*/
    c.= 0                                        /*the list of Carmichael #'s  (so far).*/
             do h3=2  for  pm-1;   g= h3 + p     /*get Carmichael numbers for this prime*/
             npsH3= ((nps // h3) + h3) // h3     /*define a couple of shortcuts for pgm.*/
             gPM= g * pm                         /*define a couple of shortcuts for pgm.*/
                                                 /* [↓] perform some weeding of D values*/
                 do d=1  for g-1;                   if gPM // d    \== 0      then iterate
                                                    if npsH3       \== d//h3  then iterate
                             q= 1  +  gPM   % d;    if \isPrime(q)            then iterate
                             r= 1  +  p * q % h3;   if q * r // pm \== 1      then iterate
                                                    if \isPrime(r)            then iterate
                 #= # + 1;   c.q= r              /*bump Carmichael counter; add to array*/
                 if bot==0  then bot= q;   bot= min(bot, q);             top= max(top, q)
                 end   /*d*/
             end       /*h3*/
    $=                                           /*build list of some Carmichael numbers*/
    if tell  then  do j=bot  to top  by 2;          if c.j\==0  then $= $  p"∙"j'∙'c.j
                   end           /*j*/

    if $\==''  then say  'Carmichael number: '      strip($)
    end                /*p*/
say
say '──────── '     #     " Carmichael numbers found."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: parse arg x;             if @.x      then return 1       /*is X  a known prime?*/
         if x<37  then return 0;  if x//2==0  then return 0; if x// 3==0     then return 0
         parse var x  ''  -1  _;  if _==5     then return 0; if x// 7==0     then return 0
         if x//11==0  then return 0; if x//13==0  then return 0; if x//17==0 then return 0
         if x//19==0  then return 0; if x//23==0  then return 0; if x//29==0 then return 0
                           do k=29  by 6  until k*k>x;    if x//k       ==0  then return 0
                                                          if x//(k+2)   ==0  then return 0
                           end   /*k*/
         @.x=1;                                                                   return 1
```

'''output'''   when using the default input:

```txt

Carmichael number:  3∙11∙17
Carmichael number:  5∙13∙17 5∙17∙29 5∙29∙73
Carmichael number:  7∙13∙19 7∙19∙67 7∙23∙41 7∙31∙73 7∙73∙103
Carmichael number:  13∙37∙61 13∙61∙397 13∙97∙421
Carmichael number:  17∙41∙233 17∙353∙1201
Carmichael number:  19∙43∙409 19∙199∙271
Carmichael number:  23∙199∙353
Carmichael number:  29∙113∙1093 29∙197∙953
Carmichael number:  31∙61∙211 31∙151∙1171 31∙181∙331 31∙271∙601 31∙991∙15361
Carmichael number:  37∙73∙109 37∙109∙2017 37∙613∙1621
Carmichael number:  41∙61∙101 41∙73∙137 41∙101∙461 41∙241∙521 41∙881∙12041 41∙1721∙35281
Carmichael number:  43∙127∙211 43∙211∙337 43∙271∙5827 43∙433∙643 43∙547∙673 43∙631∙1597 43∙3361∙3907
Carmichael number:  47∙1151∙1933 47∙3359∙6073 47∙3727∙5153
Carmichael number:  53∙79∙599 53∙157∙521
Carmichael number:  59∙1451∙2089
Carmichael number:  61∙181∙1381 61∙241∙421 61∙271∙571 61∙277∙2113 61∙421∙12841 61∙541∙3001 61∙661∙2521 61∙1301∙19841 61∙3361∙4021

────────  69  Carmichael numbers found.

```

'''output'''   when using the input of:   <tt> -1000 </tt>

```txt

────────  1038  Carmichael numbers found.

```

'''output'''   when using the input of:   <tt> -10000 </tt>

```txt

────────  8716  Carmichael numbers found.

```



## Ring


```ring

# Project : Carmichael 3 strong pseudoprimes

see "The following are Carmichael munbers for p1 <= 61:" + nl
see "p1     p2      p3     product" + nl

for p = 2 to 61
    carmichael3(p)
next

func carmichael3(p1) 
       if isprime(p1) = 0  return ok
       for h3 = 1 to p1 -1
            t1 = (h3 + p1) * (p1 -1)
            t2 = (-p1 * p1) % h3
            if t2 < 0
               t2 = t2 + h3
            ok
            for d = 1 to h3 + p1 -1
                 if t1 % d = 0 and t2 = (d % h3) 
                   p2 = 1 + (t1 / d)
                   if isprime(p2) = 0
                      loop
                   ok
                   p3 = 1 + floor((p1 * p2 / h3))
                   if isprime(p3) = 0 or ((p2 * p3) % (p1 -1)) != 1 
                      loop
                   ok
                   see "" + p1 + "       " + p2 + "      " + p3 + "    " + p1*p2*p3 + nl
                ok
            next 
     next 
        
func isprime(num)
       if (num <= 1) return 0 ok
       if (num % 2 = 0) and num != 2
          return 0
       ok
       for i = 3 to floor(num / 2) -1 step 2
           if (num % i = 0) 
              return 0
           ok
       next
       return 1

```

Output:

```txt

The following are Carmichael munbers for p1 <= 61:
p1     p2      p3     product
==     ==      ==     
### =

 3     11      17     561
 5     29      73     10585
 5     17      29     2465
 5     13      17     1105
 7     19      67     8911
 7     31      73     15841
 7     13      31     2821
 7     23      41     6601
 7     73     103     52633
 7     13      19     1729
13     61     397     314821
13     37     241     115921
13     97     421     530881
13     37      97     46657
13     37      61     29341
17     41     233     162401
17    353    1201     7207201
19     43     409     334153
19    199     271     1024651
23    199     353     1615681
29    113    1093     3581761
29    197     953     5444489
31    991   15361     471905281
31     61     631     1193221
31    151    1171     5481451
31     61     271     512461
31     61     211     399001
31    271     601     5049001
31    181     331     1857241
37    109    2017     8134561
37     73     541     1461241
37    613    1621     36765901
37     73     181     488881
37     73     109     294409
41   1721   35281     2489462641
41    881   12041     434932961
41    101     461     1909001
41    241     761     7519441
41    241     521     5148001
41     73     137     410041
41     61     101     252601
43    631   13567     368113411
43    271    5827     67902031
43    127    2731     14913991
43    127    1093     5968873
43    211     757     6868261
43    631    1597     43331401
43    127     211     1152271
43    211     337     3057601
43    433     643     11972017
43    547     673     15829633
43   3361    3907     564651361
47   3359    6073     958762729
47   1151    1933     104569501
47   3727    5153     902645857
53    157    2081     17316001
53     79     599     2508013
53    157     521     4335241
59   1451    2089     178837201
61    421   12841     329769721
61    181    5521     60957361
61   1301   19841     1574601601
61    277    2113     35703361
61    181    1381     15247621
61    541    3001     99036001
61    661    2521     101649241
61    271     571     9439201
61    241     421     6189121
61   3361    4021     824389441

```



## Ruby

{{works with|Ruby|1.9}}

```ruby
# Generate Charmichael Numbers

require 'prime'

Prime.each(61) do |p|
  (2...p).each do |h3|
    g = h3 + p
    (1...g).each do |d|
      next if (g*(p-1)) % d != 0 or (-p*p) % h3 != d % h3
      q = 1 + ((p - 1) * g / d)
      next unless q.prime?
      r = 1 + (p * q / h3)
      next unless r.prime? and (q * r) % (p - 1) == 1
      puts "#{p} x #{q} x #{r}" 
    end
  end
  puts
end
```


{{out}}
<pre style="height:30ex;overflow:scroll">
3 x 11 x 17

5 x 29 x 73
5 x 17 x 29
5 x 13 x 17

7 x 19 x 67
7 x 31 x 73
7 x 13 x 31
7 x 23 x 41
7 x 73 x 103
7 x 13 x 19


13 x 61 x 397
13 x 37 x 241
13 x 97 x 421
13 x 37 x 97
13 x 37 x 61

17 x 41 x 233
17 x 353 x 1201

19 x 43 x 409
19 x 199 x 271

23 x 199 x 353

29 x 113 x 1093
29 x 197 x 953

31 x 991 x 15361
31 x 61 x 631
31 x 151 x 1171
31 x 61 x 271
31 x 61 x 211
31 x 271 x 601
31 x 181 x 331

37 x 109 x 2017
37 x 73 x 541
37 x 613 x 1621
37 x 73 x 181
37 x 73 x 109

41 x 1721 x 35281
41 x 881 x 12041
41 x 101 x 461
41 x 241 x 761
41 x 241 x 521
41 x 73 x 137
41 x 61 x 101

43 x 631 x 13567
43 x 271 x 5827
43 x 127 x 2731
43 x 127 x 1093
43 x 211 x 757
43 x 631 x 1597
43 x 127 x 211
43 x 211 x 337
43 x 433 x 643
43 x 547 x 673
43 x 3361 x 3907

47 x 3359 x 6073
47 x 1151 x 1933
47 x 3727 x 5153

53 x 157 x 2081
53 x 79 x 599
53 x 157 x 521

59 x 1451 x 2089

61 x 421 x 12841
61 x 181 x 5521
61 x 1301 x 19841
61 x 277 x 2113
61 x 181 x 1381
61 x 541 x 3001
61 x 661 x 2521
61 x 271 x 571
61 x 241 x 421
61 x 3361 x 4021

```



## Rust


```rust

fn is_prime(n: i64) -> bool {
    if n > 1 {
        (2..((n / 2) + 1)).all(|x| n % x != 0)
    } else {
        false
    }
}

// The modulo operator actually calculates the remainder.
fn modulo(n: i64, m: i64) -> i64 {
    ((n % m) + m) % m
}

fn carmichael(p1: i64) -> Vec<(i64, i64, i64)> {
    let mut results = Vec::new();
    if !is_prime(p1) {
        return results;
    }

    for h3 in 2..p1 {
        for d in 1..(h3 + p1) {
            if (h3 + p1) * (p1 - 1) % d != 0 || modulo(-p1 * p1, h3) != d % h3 {
                continue;
            }

            let p2 = 1 + ((p1 - 1) * (h3 + p1) / d);
            if !is_prime(p2) {
                continue;
            }

            let p3 = 1 + (p1 * p2 / h3);
            if !is_prime(p3) || ((p2 * p3) % (p1 - 1) != 1) {
                continue;
            }

            results.push((p1, p2, p3));
        }
    }

    results
}

fn main() {
    (1..62)
        .filter(|&x| is_prime(x))
        .map(carmichael)
        .filter(|x| !x.is_empty())
        .flat_map(|x| x)
        .inspect(|x| println!("{:?}", x))
        .count(); // Evaluate entire iterator
}

```

{{out}}

```txt

(3, 11, 17)
(5, 29, 73)
(5, 17, 29)
(5, 13, 17)
.
.
.
(61, 661, 2521)
(61, 271, 571)
(61, 241, 421)
(61, 3361, 4021)

```



## Seed7

The function [http://seed7.sourceforge.net/algorith/math.htm#isPrime isPrime] below is borrowed from the [http://seed7.sourceforge.net/algorith Seed7 algorithm collection].


```seed7
$ include "seed7_05.s7i";
 
const func boolean: isPrime (in integer: number) is func
  result
    var boolean: prime is FALSE;
  local
    var integer: upTo is 0;
    var integer: testNum is 3;
  begin
    if number = 2 then
      prime := TRUE;
    elsif odd(number) and number > 2 then
      upTo := sqrt(number);
      while number rem testNum <> 0 and testNum <= upTo do
        testNum +:= 2;
      end while;
      prime := testNum > upTo;
    end if;
  end func;

const proc: main is func
  local
    var integer: p1 is 0;
    var integer: h3 is 0;
    var integer: g is 0;
    var integer: d is 0;
    var integer: p2 is 0;
    var integer: p3 is 0;
  begin
    for p1 range 2 to 61 do
      if isPrime(p1) then
        for h3 range 2 to p1 do
          g := h3 + p1;
          for d range 1 to pred(g) do
            if (g * pred(p1)) mod d = 0 and -p1 ** 2 mod h3 = d mod h3 then
              p2 := 1 + pred(p1) * g div d;
              if isPrime(p2) then
                p3 := 1 + p1 * p2 div h3;
                if isPrime(p3) and (p2 * p3) mod pred(p1) = 1 then
                  writeln(p1 <& " * " <& p2 <& " * " <& p3 <& " = " <& p1*p2*p3);
                end if;
              end if;
            end if;
          end for;
        end for;
      end if;
    end for;
  end func;
```


{{out}}

```txt

3 * 11 * 17 = 561
5 * 29 * 73 = 10585
5 * 17 * 29 = 2465
5 * 13 * 17 = 1105
7 * 19 * 67 = 8911
7 * 31 * 73 = 15841
7 * 13 * 31 = 2821
7 * 23 * 41 = 6601
7 * 73 * 103 = 52633
7 * 13 * 19 = 1729
13 * 61 * 397 = 314821
13 * 37 * 241 = 115921
13 * 97 * 421 = 530881
13 * 37 * 97 = 46657
13 * 37 * 61 = 29341
17 * 41 * 233 = 162401
17 * 353 * 1201 = 7207201
19 * 43 * 409 = 334153
19 * 199 * 271 = 1024651
23 * 199 * 353 = 1615681
29 * 113 * 1093 = 3581761
29 * 197 * 953 = 5444489
31 * 991 * 15361 = 471905281
31 * 61 * 631 = 1193221
31 * 151 * 1171 = 5481451
31 * 61 * 271 = 512461
31 * 61 * 211 = 399001
31 * 271 * 601 = 5049001
31 * 181 * 331 = 1857241
37 * 109 * 2017 = 8134561
37 * 73 * 541 = 1461241
37 * 613 * 1621 = 36765901
37 * 73 * 181 = 488881
37 * 73 * 109 = 294409
41 * 1721 * 35281 = 2489462641
41 * 881 * 12041 = 434932961                                                                                                                                                 
41 * 101 * 461 = 1909001                                                                                                                                                     
41 * 241 * 761 = 7519441                                                                                                                                                     
41 * 241 * 521 = 5148001                                                                                                                                                     
41 * 73 * 137 = 410041                                                                                                                                                       
41 * 61 * 101 = 252601                                                                                                                                                       
43 * 631 * 13567 = 368113411                                                                                                                                                 
43 * 271 * 5827 = 67902031                                                                                                                                                   
43 * 127 * 2731 = 14913991                                                                                                                                                   
43 * 127 * 1093 = 5968873                                                                                                                                                    
43 * 211 * 757 = 6868261                                                                                                                                                     
43 * 631 * 1597 = 43331401                                                                                                                                                   
43 * 127 * 211 = 1152271
43 * 211 * 337 = 3057601
43 * 433 * 643 = 11972017
43 * 547 * 673 = 15829633
43 * 3361 * 3907 = 564651361
47 * 3359 * 6073 = 958762729
47 * 1151 * 1933 = 104569501
47 * 3727 * 5153 = 902645857
53 * 157 * 2081 = 17316001
53 * 79 * 599 = 2508013
53 * 157 * 521 = 4335241
59 * 1451 * 2089 = 178837201
61 * 421 * 12841 = 329769721
61 * 181 * 5521 = 60957361
61 * 1301 * 19841 = 1574601601
61 * 277 * 2113 = 35703361
61 * 181 * 1381 = 15247621
61 * 541 * 3001 = 99036001
61 * 661 * 2521 = 101649241
61 * 271 * 571 = 9439201
61 * 241 * 421 = 6189121
61 * 3361 * 4021 = 824389441

```



## Sidef

{{trans|Perl}}

```ruby
func forprimes(a, b, callback) {
    for (a = (a-1 -> next_prime); a <= b; a.next_prime!) {
        callback(a)
    }
}

forprimes(3, 61, func(p) {
   for h3 in (2 ..^ p) {
      var ph3 = (p + h3)
      for d in (1 ..^ ph3) {
         ((-p * p) % h3) != (d % h3) && next
         ((p-1) * ph3) % d && next
         var q = 1+((p-1) * ph3 / d)
         q.is_prime || next
         var r = 1+((p*q - 1)/h3)
         r.is_prime || next
         (q*r) % (p-1) == 1 || next
         printf("%2d x %5d x %5d = %s\n",p,q,r, p*q*r)
      }
   }
})
```


{{out}}

```txt

 3 x    11 x    17 = 561
 5 x    29 x    73 = 10585
 5 x    17 x    29 = 2465
 5 x    13 x    17 = 1105
 ... full output is 69 lines ...
61 x   661 x  2521 = 101649241
61 x   271 x   571 = 9439201
61 x   241 x   421 = 6189121
61 x  3361 x  4021 = 824389441

```



## Tcl

Using the primality tester from [[Miller-Rabin primality test#Tcl|the Miller-Rabin task]]...

```tcl
proc carmichael {limit {rounds 10}} {
    set carmichaels {}
    for {set p1 2} {$p1 <= $limit} {incr p1} {
	if {![miller_rabin $p1 $rounds]} continue
	for {set h3 2} {$h3 < $p1} {incr h3} {
	    set g [expr {$h3 + $p1}]
	    for {set d 1} {$d < $h3+$p1} {incr d} {
		if {(($h3+$p1)*($p1-1))%$d != 0} continue
		if {(-($p1**2))%$h3 != $d%$h3} continue

		set p2 [expr {1 + ($p1-1)*$g/$d}]
		if {![miller_rabin $p2 $rounds]} continue

		set p3 [expr {1 + $p1*$p2/$h3}]
		if {![miller_rabin $p3 $rounds]} continue

		if {($p2*$p3)%($p1-1) != 1} continue
		lappend carmichaels $p1 $p2 $p3 [expr {$p1*$p2*$p3}]
	    }
	}
    }
    return $carmichaels
}
```

Demonstrating:

```tcl
set results [carmichael 61 2]
puts "[expr {[llength $results]/4}] Carmichael numbers found"
foreach {p1 p2 p3 c} $results {
    puts "$p1 x $p2 x $p3 = $c"
}
```

{{out}}

```txt

69 Carmichael numbers found
3 x 11 x 17 = 561
5 x 29 x 73 = 10585
5 x 17 x 29 = 2465
5 x 13 x 17 = 1105
7 x 19 x 67 = 8911
7 x 31 x 73 = 15841
7 x 13 x 31 = 2821
7 x 23 x 41 = 6601
7 x 73 x 103 = 52633
7 x 13 x 19 = 1729
13 x 61 x 397 = 314821
13 x 37 x 241 = 115921
13 x 97 x 421 = 530881
13 x 37 x 97 = 46657
13 x 37 x 61 = 29341
17 x 41 x 233 = 162401
17 x 353 x 1201 = 7207201
19 x 43 x 409 = 334153
19 x 199 x 271 = 1024651
23 x 199 x 353 = 1615681
29 x 113 x 1093 = 3581761
29 x 197 x 953 = 5444489
31 x 991 x 15361 = 471905281
31 x 61 x 631 = 1193221
31 x 151 x 1171 = 5481451
31 x 61 x 271 = 512461
31 x 61 x 211 = 399001
31 x 271 x 601 = 5049001
31 x 181 x 331 = 1857241
37 x 109 x 2017 = 8134561
37 x 73 x 541 = 1461241
37 x 613 x 1621 = 36765901
37 x 73 x 181 = 488881
37 x 73 x 109 = 294409
41 x 1721 x 35281 = 2489462641
41 x 881 x 12041 = 434932961
41 x 101 x 461 = 1909001
41 x 241 x 761 = 7519441
41 x 241 x 521 = 5148001
41 x 73 x 137 = 410041
41 x 61 x 101 = 252601
43 x 631 x 13567 = 368113411
43 x 271 x 5827 = 67902031
43 x 127 x 2731 = 14913991
43 x 127 x 1093 = 5968873
43 x 211 x 757 = 6868261
43 x 631 x 1597 = 43331401
43 x 127 x 211 = 1152271
43 x 211 x 337 = 3057601
43 x 433 x 643 = 11972017
43 x 547 x 673 = 15829633
43 x 3361 x 3907 = 564651361
47 x 3359 x 6073 = 958762729
47 x 1151 x 1933 = 104569501
47 x 3727 x 5153 = 902645857
53 x 157 x 2081 = 17316001
53 x 79 x 599 = 2508013
53 x 157 x 521 = 4335241
59 x 1451 x 2089 = 178837201
61 x 421 x 12841 = 329769721
61 x 181 x 5521 = 60957361
61 x 1301 x 19841 = 1574601601
61 x 277 x 2113 = 35703361
61 x 181 x 1381 = 15247621
61 x 541 x 3001 = 99036001
61 x 661 x 2521 = 101649241
61 x 271 x 571 = 9439201
61 x 241 x 421 = 6189121
61 x 3361 x 4021 = 824389441

```



## zkl

Using the Miller-Rabin primality test in lib GMP.

```zkl
var BN=Import("zklBigNum"), bi=BN(0); // gonna recycle bi
primes:=T(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61);
var p2,p3;
cs:=[[(p1,h3,d); primes; { [2..p1 - 1] }; // list comprehension
      { [1..h3 + p1 - 1] },
	{ ((h3 + p1)*(p1 - 1)%d == 0 and ((-p1*p1):mod(_,h3) == d%h3)) },//guard
	{ (p2=1 + (p1 - 1)*(h3 + p1)/d):bi.set(_).probablyPrime() },//guard
	{ (p3=1 + (p1*p2/h3)):bi.set(_).probablyPrime() },	 //guard
	{ 1==(p2*p3)%(p1 - 1) };				 //guard
   { T(p1,p2,p3) }  // return list of three primes in Carmichael number
]];
fcn mod(a,b) { m:=a%b; if(m<0) m+b else m }
```

<lang>cs.len().println(" Carmichael numbers found:");
cs.pump(Console.println,fcn([(p1,p2,p3)]){
   "%2d * %4d * %5d = %d".fmt(p1,p2,p3,p1*p2*p3) });
```

{{out}}

```txt

69 Carmichael numbers found:
 3 *   11 *    17 = 561
 5 *   29 *    73 = 10585
 5 *   17 *    29 = 2465
 5 *   13 *    17 = 1105
 7 *   19 *    67 = 8911
...
61 *  181 *  1381 = 15247621
61 *  541 *  3001 = 99036001
61 *  661 *  2521 = 101649241
61 *  271 *   571 = 9439201
61 *  241 *   421 = 6189121
61 * 3361 *  4021 = 824389441

```



## ZX Spectrum Basic

{{trans|C}}

```zxbasic
10 FOR p=2 TO 61
20 LET n=p: GO SUB 1000
30 IF NOT n THEN GO TO 200
40 FOR h=1 TO p-1
50 FOR d=1 TO h-1+p
60 IF NOT (FN m((h+p)*(p-1),d)=0 AND FN w(-p*p,h)=FN m(d,h)) THEN GO TO 180
70 LET q=INT (1+((p-1)*(h+p)/d))
80 LET n=q: GO SUB 1000
90 IF NOT n THEN GO TO 180
100 LET r=INT (1+(p*q/h))
110 LET n=r: GO SUB 1000
120 IF (NOT n) OR ((FN m((q*r),(p-1))<>1)) THEN GO TO 180
130 PRINT p;" ";q;" ";r
180 NEXT d
190 NEXT h
200 NEXT p
210 STOP 
1000 IF n<4 THEN LET n=(n>1): RETURN 
1010 IF (NOT FN m(n,2)) OR (NOT FN m(n,3)) THEN LET n=0: RETURN 
1020 LET i=5
1030 IF NOT ((i*i)<=n) THEN LET n=1: RETURN 
1040 IF (NOT FN m(n,i)) OR NOT FN m(n,(i+2)) THEN LET n=0: RETURN 
1050 LET i=i+6
1060 GO TO 1030
2000 DEF FN m(a,b)=a-(INT (a/b)*b): REM Mod function
2010 DEF FN w(a,b)=FN m(FN m(a,b)+b,b): REM Mod function modified

```

