+++
title = "Factors of a Mersenne number"
description = ""
date = 2018-11-16T12:52:13Z
aliases = []
[extra]
id = 3247
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
+++

A Mersenne number is a number in the form of 2<sup>P</sup>-1.

If P is prime, the Mersenne number may be a Mersenne prime
(if P is not prime, the Mersenne number is also not prime).

In the search for Mersenne prime numbers it is advantageous to eliminate exponents by finding a small factor before starting a,  potentially lengthy, [[Lucas-Lehmer test]].

There are very efficient algorithms for determining if a number divides 2<sup>P</sup>-1 (or equivalently, if 2<sup>P</sup> mod (the number) = 1).
Some languages already have built-in implementations of this exponent-and-mod operation (called ''modPow'' or similar).

The following is how to implement this ''modPow'' yourself:

For example, let's compute 2<sup>23</sup> mod 47.
Convert the exponent 23 to binary, you get 10111. Starting with <tt>square</tt> = 1, repeatedly square it.
Remove the top bit of the exponent, and if it's 1 multiply <tt>square</tt> by the base of the exponentiation (2), then compute <tt>square</tt> modulo 47.
Use the result of the modulo from the last step as the initial value of <tt>square</tt> in the next step:

                   remove       optional
       square      top bit   multiply by 2   mod 47
    ────────────   ───────   ─────────────   ──────
    1*1 = 1        1  0111   1*2 = 2            2
    2*2 = 4        0   111      no              4
    4*4 = 16       1    11   16*2 = 32         32
    32*32 = 1024   1     1   1024*2 = 2048     27
    27*27 = 729    1         729*2 = 1458       1

Since 2<sup>23</sup> mod 47 = 1, 47 is a factor of 2<sup>P</sup>-1.
(To see this, subtract 1 from both sides: 2<sup>23</sup>-1 = 0 mod 47.)
Since we've shown that 47 is a factor, 2<sup>23</sup>-1 is not prime.
Further properties of Mersenne numbers allow us to refine the process even more.
Any factor q of 2<sup>P</sup>-1 must be of the form 2kP+1, k being a positive integer or zero. Furthermore, q must be 1 or 7 mod 8.
Finally any potential factor q must be [[Primality by Trial Division|prime]].
As in other trial division algorithms, the algorithm stops when 2kP+1 > sqrt(N).

These primality tests only work on Mersenne numbers where P is prime. For example, M<sub>4</sub>=15 yields no factors using these techniques, but factors into 3 and 5, neither of which fit 2kP+1.


## Task

Using the above method find a factor of  2<sup>929</sup>-1 (aka M929)


## Related tasks

*   [[count in factors]]
*   [[prime decomposition]]
*   [[factors of an integer]]
*   [[Sieve of Eratosthenes]]
*   [[primality by trial division]]
*   [[trial factoring of a Mersenne number]]
*   [[partition an integer X into N primes]]
*   [[sequence of primes by Trial Division]]


## See also

*   [https://www.youtube.com/watch?v=SNwvJ7psoow Computers in 1948: 2¹²⁷-1]





## 360 Assembly

Use of bitwise operations
(TM (Test under Mask), SLA (Shift Left Arithmetic),SRA (Shift Right Arithmetic)).
<lang>*        Factors of a Mersenne number  11/09/2015
MERSENNE CSECT
         USING  MERSENNE,R15
         MVC    Q,=F'929'          q=929   (M929=2**929-1)
         LA     R6,1               k=1
LOOPK    C      R6,=F'1048576'     do k=1 to 2**20
         BNL    ELOOPK
         LR     R5,R6              k
         M      R4,Q               *q
         SLA    R5,1               *2   by shift left 1
         LA     R5,1(R5)           +1
         ST     R5,P               p=k*q*2+1
         L      R2,P               p
         N      R2,=F'7'           p&7
         C      R2,=F'1'           if    ((p&7)=1)    p='*001'
         BE     OK
         C      R2,=F'7'           or if ((p&7)=7)    p='*111'
         BNE    NOTOK
OK       MVI    PRIME,X'00'        then prime=false   is prime?
         LA     R2,2               loop count=2
         LA     R1,2               j=2 and after j=3
J2J3     L      R4,P               p
         SRDA   R4,32              r4>>r5
         DR     R4,R1              p/j
         LTR    R4,R4              if p//j=0
         BZ     NOTPRIME           then goto notprime
         LA     R1,1(R1)           j=j+1
         BCT    R2,J2J3
         LA     R7,5               d=5
WHILED   LR     R5,R7              d
         MR     R4,R7              *d
         C      R5,P               do while(d*d<=p)
         BH     EWHILED
         LA     R2,2               loop count=2
         LA     R1,2               j=2 and after j=4
J2J4     L      R4,P               p
         SRDA   R4,32              r4>>r5
         DR     R4,R7              /d
         LTR    R4,R4              if p//d=0
         BZ     NOTPRIME           then goto notprime
         AR     R7,R1              d=d+j
         LA     R1,2(R1)           j=j+2
         BCT    R2,J2J4
         B      WHILED
EWHILED  MVI    PRIME,X'01'        prime=true      so is prime
NOTPRIME L      R8,Q               i=q
         MVC    Y,=F'1'            y=1
         MVC    Z,=F'2'            z=2
WHILEI   LTR    R8,R8              do while(i^=0)
         BZ     EWHILEI
         ST     R8,PG              i
         TM     PG+3,B'00000001'   if first bit of i not 1
         BZ     NOTFIRST
         L      R5,Y               y
         M      R4,Z               *z
         LA     R4,0
         D      R4,P               /p
         ST     R4,Y               y=(y*z)//p
NOTFIRST L      R5,Z               z
         M      R4,Z               *z
         LA     R4,0
         D      R4,P               /p
         ST     R4,Z               z=(z*z)//p
         SRA    R8,1               i=i/2   by shift right 1
         B      WHILEI
EWHILEI  CLI    PRIME,X'01'        if prime
         BNE    NOTOK
         CLC    Y,=F'1'            and if y=1
         BNE    NOTOK
         MVC    FACTOR,P           then factor=p
         B      OKFACTOR
NOTOK    LA     R6,1(R6)           k=k+1
         B      LOOPK
ELOOPK   MVC    FACTOR,=F'0'       factor=0
OKFACTOR L      R1,Q
         XDECO  R1,PG              edit q
         L      R1,FACTOR
         XDECO  R1,PG+12           edit factor
         XPRNT  PG,24              print
         XR     R15,R15
         BR     R14
PRIME    DS     X                  flag for prime
Q        DS     F
P        DS     F
Y        DS     F
Z        DS     F
FACTOR   DS     F                  a factor of q
PG       DS     CL24               buffer
         YREGS
         END    MERSENNE
```

```txt

         929       13007

```



## Ada

mersenne.adb:

```Ada
with Ada.Text_IO;
--  reuse Is_Prime from [[Primality by Trial Division]]
with Is_Prime;

procedure Mersenne is
   function Is_Set (Number : Natural; Bit : Positive) return Boolean is
   begin
      return Number / 2 ** (Bit - 1) mod 2 = 1;
   end Is_Set;

   function Get_Max_Bit (Number : Natural) return Natural is
      Test : Natural := 0;
   begin
      while 2 ** Test <= Number loop
         Test := Test + 1;
      end loop;
      return Test;
   end Get_Max_Bit;

   function Modular_Power (Base, Exponent, Modulus : Positive) return Natural is
      Maximum_Bit : constant Natural := Get_Max_Bit (Exponent);
      Square      : Natural := 1;
   begin
      for Bit in reverse 1 .. Maximum_Bit loop
         Square := Square ** 2;
         if Is_Set (Exponent, Bit) then
            Square := Square * Base;
         end if;
         Square := Square mod Modulus;
      end loop;
      return Square;
   end Modular_Power;

   Not_A_Prime_Exponent : exception;

   function Get_Factor (Exponent : Positive) return Natural is
      Factor : Positive;
   begin
      if not Is_Prime (Exponent) then
         raise Not_A_Prime_Exponent;
      end if;
      for K in 1 .. 16384 / Exponent loop
         Factor := 2 * K * Exponent + 1;
         if Factor mod 8 = 1 or else Factor mod 8 = 7 then
            if Is_Prime (Factor) and then Modular_Power (2, Exponent, Factor) = 1 then
               return Factor;
            end if;
         end if;
      end loop;
      return 0;
   end Get_Factor;

   To_Test : constant Positive := 929;
   Factor  : Natural;
begin
   Ada.Text_IO.Put ("2 **" & Integer'Image (To_Test) & " - 1 ");
   begin
      Factor := Get_Factor (To_Test);
      if Factor = 0 then
         Ada.Text_IO.Put_Line ("is prime.");
      else
         Ada.Text_IO.Put_Line ("has factor" & Integer'Image (Factor));
      end if;
   exception
      when Not_A_Prime_Exponent =>
         Ada.Text_IO.Put_Line ("is not a Mersenne number");
   end;
end Mersenne;
```


```txt
2 ** 929 - 1 has factor 13007
```



## ALGOL 68

<!--  {{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}
Compiles, but I couldn't maxint not in library, works with manually entered maxint, bits width. Leaving some issue with newline -->

```algol68
MODE ISPRIMEINT = INT;
PR READ "prelude/is_prime.a68" PR;

MODE POWMODSTRUCT = INT;
PR READ "prelude/pow_mod.a68" PR;

PROC m factor = (INT p)INT:BEGIN
  INT m factor;
  INT max k, msb, n, q;

  FOR i FROM bits width - 2 BY -1 TO 0 WHILE ( BIN p SHR i AND 2r1 ) = 2r0 DO
      msb := i
  OD;

  max k := ENTIER sqrt(max int) OVER p; # limit for k to prevent overflow of max int #
  FOR k FROM 1 TO max k DO
    q := 2*p*k + 1;
    IF NOT is prime(q) THEN
      SKIP
    ELIF q MOD 8 /= 1 AND q MOD 8 /= 7 THEN
      SKIP
    ELSE
      n := pow mod(2,p,q);
      IF n = 1 THEN
        m factor := q;
        return
      FI
    FI
  OD;
  m factor := 0;
  return:
    m factor
END;

BEGIN

  INT exponent, factor;
  print("Enter exponent of Mersenne number:");
  read(exponent);
  IF NOT is prime(exponent) THEN
    print(("Exponent is not prime: ", exponent, new line))
  ELSE
    factor := m factor(exponent);
    IF factor = 0 THEN
      print(("No factor found for M", exponent, new line))
    ELSE
      print(("M", exponent, " has a factor: ", factor, new line))
    FI
  FI

END
```

Example:

```txt

Enter exponent of Mersenne number:929
M       +929 has a factor:      +13007

```



## AutoHotkey

ahk [http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=144 discussion]

```autohotkey
MsgBox % MFact(27)  ;-1: 27 is not prime
MsgBox % MFact(2)   ; 0
MsgBox % MFact(3)   ; 0
MsgBox % MFact(5)   ; 0
MsgBox % MFact(7)   ; 0
MsgBox % MFact(11)  ; 23
MsgBox % MFact(13)  ; 0
MsgBox % MFact(17)  ; 0
MsgBox % MFact(19)  ; 0
MsgBox % MFact(23)  ; 47
MsgBox % MFact(29)  ; 233
MsgBox % MFact(31)  ; 0
MsgBox % MFact(37)  ; 223
MsgBox % MFact(41)  ; 13367
MsgBox % MFact(43)  ; 431
MsgBox % MFact(47)  ; 2351
MsgBox % MFact(53)  ; 6361
MsgBox % MFact(929) ; 13007

MFact(p) { ; blank if 2**p-1 can be prime, otherwise a prime divisor < 2**32
   If !IsPrime32(p)
      Return -1                      ; Error (p must be prime)
   Loop % 2.0**(p<64 ? p/2-1 : 31)/p ; test prime divisors < 2**32, up to sqrt(2**p-1)
      If (((q:=2*p*A_Index+1)&7 = 1 || q&7 = 7) && IsPrime32(q) && PowMod(2,p,q)=1)
         Return q
   Return 0
}

IsPrime32(n) { ; n < 2**32
   If n in 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97
      Return 1
   If (!(n&1)||!mod(n,3)||!mod(n,5)||!mod(n,7)||!mod(n,11)||!mod(n,13)||!mod(n,17)||!mod(n,19))
      Return 0
   n1 := d := n-1, s := 0
   While !(d&1)
      d>>=1, s++
   Loop 3 {
      x := PowMod( A_Index=1 ? 2 : A_Index=2 ? 7 : 61, d, n)
      If (x=1 || x=n1)
         Continue
      Loop % s-1
         If (1 = x:=PowMod(x,2,n))
            Return 0
         Else If (x = n1)
            Break
      IfLess x,%n1%, Return 0
   }
   Return 1
}

PowMod(x,n,m) { ; x**n mod m
   y := 1, i := n, z := x
   While i>0
      y := i&1 ? mod(y*z,m) : y, z := mod(z*z,m), i >>= 1
   Return y
}
```



## BBC BASIC


```bbcbasic
      PRINT "A factor of M929 is "; FNmersenne_factor(929)
      PRINT "A factor of M937 is "; FNmersenne_factor(937)
      END

      DEF FNmersenne_factor(P%)
      LOCAL K%, Q%
      IF NOT FNisprime(P%) THEN = -1
      FOR K% = 1 TO 1000000
        Q% = 2*K%*P% + 1
        IF (Q% AND 7) = 1 OR (Q% AND 7) = 7 THEN
          IF FNisprime(Q%) IF FNmodpow(2, P%, Q%) = 1 THEN = Q%
        ENDIF
      NEXT K%
      = 0

      DEF FNisprime(N%)
      LOCAL D%
      IF N% MOD 2=0 THEN = (N% = 2)
      IF N% MOD 3=0 THEN = (N% = 3)
      D% = 5
      WHILE D% * D% <= N%
        IF N% MOD D% = 0 THEN = FALSE
        D% += 2
        IF N% MOD D% = 0 THEN = FALSE
        D% += 4
      ENDWHILE
      = TRUE

      DEF FNmodpow(X%, N%, M%)
      LOCAL I%, Y%, Z%
      I% = N% : Y% = 1 : Z% = X%
      WHILE I%
        IF I% AND 1 THEN Y% = (Y% * Z%) MOD M%
        Z% = (Z% * Z%) MOD M%
        I% = I% >>> 1
      ENDWHILE
      = Y%

```

```txt
A factor of M929 is 13007
A factor of M937 is 28111
```



## Bracmat


```Bracmat
( ( modPow
  =   square P divisor highbit log 2pow
    .   !arg:(?P.?divisor)
      & 1:?square
      & 2\L!P:#%?log+?
      & 2^!log:?2pow
      &   whl
        ' (     mod
              $ (   ( div$(!P.!2pow):1&2
                    | 1
                    )
                  * !square^2
                . !divisor
                )
            : ?square
          & mod$(!P.!2pow):?P
          & 1/2*!2pow:~/:?2pow
          )
      & !square
  )
& ( isPrime
  =   incs nextincs primeCandidate nextPrimeCandidate quotient
    .     1 1 2 2 (4 2 4 2 4 6 2 6:?incs)
        : ?nextincs
      & 1:?primeCandidate
      & ( nextPrimeCandidate
        =   ( !nextincs:&!incs:?nextincs
            |
            )
          & !nextincs:%?inc ?nextincs
          & !inc+!primeCandidate:?primeCandidate
        )
      &   whl
        ' ( (!nextPrimeCandidate:?divisor)^2:~>!arg
          & !arg*!divisor^-1:?quotient:/
          )
      & !quotient:/
  )
& ( Factors-of-a-Mersenne-Number
  =   P k candidate bignum
    .   !arg:?P
      & 2^!P+-1:?bignum
      & 0:?k
      &   whl
        ' ( 2*(1+!k:?k)*!P+1:?candidate
          & !candidate^2:~>!bignum
          & ( ~(mod$(!candidate.8):(1|7))
            | ~(isPrime$!candidate)
            | modPow$(!P.!candidate):?mp:~1
            )
          )
      & !mp:1
      & (!candidate.(2^!P+-1)*!candidate^-1)
  )
& (   Factors-of-a-Mersenne-Number$929:(?divisorA.?divisorB)
    &   out
      $ ( str
        $ ("found some divisors of 2^" !P "-1 : " !divisorA " and " !divisorB)
        )
  | out$"no divisors found"
  )
);
```

```txt
found some divisors of 2^!P-1 : 13007 and 348890248924938259750454781163390930305120269538278042934009621348894657205785
201247454118966026150852149399410259938217062100192168747352450719561908445272675574320888385228421992652298715687625495
638077382028762529439880103124705348782610789919949159935587158612289264184273

```



## C


```C
int isPrime(int n){
	if (n%2==0) return n==2;
	if (n%3==0) return n==3;
	int d=5;
	while(d*d<=n){
		if(n%d==0) return 0;
		d+=2;
		if(n%d==0) return 0;
		d+=4;}
	return 1;}

main() {int i,d,p,r,q=929;
	if (!isPrime(q)) return 1;
	r=q;
	while(r>0) r<<=1;
	d=2*q+1;
	do { 	for(p=r, i= 1; p; p<<= 1){
			i=((long long)i * i) % d;
			if (p < 0) i *= 2;
			if (i > d) i -= d;}
		if (i != 1) d += 2*q;
		else break;
	} while(1);
	printf("2^%d - 1 = 0 (mod %d)\n", q, d);}
```



## C#


```c#
using System;

namespace prog
{
	class MainClass
	{
		public static void Main (string[] args)
		{
			int q = 929;
			if ( !isPrime(q) ) return;
			int r = q;
			while( r > 0 )
				r <<= 1;
			int d = 2 * q + 1;
			do
			{
				int i = 1;
				for( int p=r; p!=0; p<<=1 )
				{
					i = (i*i) % d;
					if (p < 0) i *= 2;
					if (i > d) i -= d;
				}
				if (i != 1) d += 2 * q; else break;
			}
			while(true);

			Console.WriteLine("2^"+q+"-1 = 0 (mod "+d+")");
		}

		static bool isPrime(int n)
		{
			if ( n % 2 == 0 ) return n == 2;
			if ( n % 3 == 0 ) return n == 3;
			int d = 5;
			while( d*d <= n )
			{
				if ( n % d == 0 ) return false;
				d += 2;
				if ( n % d == 0 ) return false;
				d += 4;
			}
			return true;
		}
	}
}
```



## Clojure

```lisp
(ns mersennenumber
  (:gen-class))

(defn m* [p q m]
  " Computes (p*q) mod m "
  (mod (*' p q) m))

(defn power
  "modular exponentiation (i.e. b^e mod m"
  [b e m]
  (loop [b b, e e, x 1]
    (if (zero? e)
      x
      (if (even? e) (recur (m* b b m) (quot e 2) x)
                    (recur (m* b b m) (quot e 2) (m* b x m))))))

(defn divides? [k n]
  " checks if k divides n "
  (= (rem n k) 0))

(defn is-prime? [n]
  " checks if n is prime "
  (cond
    (< n 2) false             ; 0, 1 not prime (i.e. primes are greater than one)
    (= n 2) true              ; 2 is prime
    (= 0 (mod n 2)) false     ; all other evens are not prime
    :else                     ; check for divisors up to sqrt(n)
      (empty? (filter #(divides? % n) (take-while #(<= (* % %) n) (range 2 n))))))

;; Max k to check
(def MAX-K 16384)

(defn trial-factor  [p k]
  " check if k satisfies 2*k*P + 1 divides 2^p - 1 "
  (let [q  (+ (* 2 p k) 1)
        mq (mod q 8)]
    (cond
      (not (is-prime? q))     nil
      (and (not= 1 mq)
           (not= 7 mq))       nil
      (= 1 (power 2 p q))     q
      :else                   nil)))

(defn m-factor [p]
  " searches for k-factor "
  (some #(trial-factor p %) (range 16384)))

(defn -main [p]
  (if-not (is-prime? p)
    (format "M%d = 2^%d - 1 exponent is not prime" p p)
    (if-let [factor (m-factor p)]
      (format "M%d = 2^%d - 1 is composite with factor %d" p p factor)
      (format "M%d = 2^%d - 1 is prime" p p))))

;; Tests different p values
(doseq [p [2,3,4,5,7,11,13,17,19,23,29,31,37,41,43,47,53,929]
        :let [s (-main p)]]
  (println s))

```

```txt

M2 = 2^2 - 1 is prime
M3 = 2^3 - 1 is composite with factor 7
M4 = 2^4 - 1 exponent is not prime
M5 = 2^5 - 1 is composite with factor 31
M7 = 2^7 - 1 is composite with factor 127
M11 = 2^11 - 1 is composite with factor 23
M13 = 2^13 - 1 is composite with factor 8191
M17 = 2^17 - 1 is composite with factor 131071
M19 = 2^19 - 1 is composite with factor 524287
M23 = 2^23 - 1 is composite with factor 47
M29 = 2^29 - 1 is composite with factor 233
M31 = 2^31 - 1 is prime
M37 = 2^37 - 1 is composite with factor 223
M41 = 2^41 - 1 is composite with factor 13367
M43 = 2^43 - 1 is composite with factor 431
M47 = 2^47 - 1 is composite with factor 2351
M53 = 2^53 - 1 is composite with factor 6361
M929 = 2^929 - 1 is composite with factor 13007

```



## CoffeeScript


```coffeescript
mersenneFactor = (p) ->
    limit = Math.sqrt(Math.pow(2,p) - 1)
    k = 1
    while (2*k*p - 1) < limit
        q = 2*k*p + 1
        if isPrime(q) and (q % 8 == 1 or q % 8 == 7) and trialFactor(2,p,q)
            return q
        k++
    return null

isPrime = (value) ->
    for i in [2...value]
        return false if value % i == 0
        return true  if value % i != 0

trialFactor = (base, exp, mod) ->
    square = 1
    bits = exp.toString(2).split('')
    for bit in bits
        square = Math.pow(square, 2) * (if +bit is 1 then base else 1) % mod
    return square == 1

checkMersenne = (p) ->
    factor = mersenneFactor(+p)
    console.log "M#{p} = 2^#{p}-1 is #{if factor is null then "prime" else "composite with #{factor}"}"

checkMersenne(prime) for prime in ["2","3","4","5","7","11","13","17","19","23","29","31","37","41","43","47","53","929"]

```



```txt
M2 = 2^2-1 is prime
M3 = 2^3-1 is prime
M4 = 2^4-1 is prime
M5 = 2^5-1 is prime
M7 = 2^7-1 is prime
M11 = 2^11-1 is composite with 23
M13 = 2^13-1 is prime
M17 = 2^17-1 is prime
M19 = 2^19-1 is prime
M23 = 2^23-1 is composite with 47
M29 = 2^29-1 is composite with 233
M31 = 2^31-1 is prime
M37 = 2^37-1 is composite with 223
M41 = 2^41-1 is composite with 13367
M43 = 2^43-1 is composite with 431
M47 = 2^47-1 is composite with 2351
M53 = 2^53-1 is composite with 6361
M929 = 2^929-1 is composite with 13007

```



## Common Lisp

```lisp
(defun mersenne-fac (p &aux (m (1- (expt 2 p))))
  (loop for k from 1
        for n = (1+ (* 2 k p))
        until (zerop (mod m n))
        finally (return n)))

(print (mersenne-fac 929))
```


 13007


###  Version 2


We can use a primality test from the [[Primality by Trial Division#Common_Lisp|Primality by Trial Division]] task.


```lisp
(defun primep (n)
  "Is N prime?"
  (and (> n 1)
       (or (= n 2) (oddp n))
       (loop for i from 3 to (isqrt n) by 2
	  never (zerop (rem n i)))))
```


Specific to this task, we define modulo-power and mersenne-prime-p.


```lisp
(defun modulo-power (base power modulus)
  (loop with square = 1
        for bit across (format nil "~b" power)
        do (setf square (* square square))
        when (char= bit #\1) do (setf square (* square base))
        do (setf square (mod square modulus))
        finally (return square)))

(defun mersenne-prime-p (power)
  (do* ((N (1- (expt 2 power)))
        (sqN (isqrt N))
        (k 1 (1+ k))
        (q (1+ (* 2 power k)) (1+ (* 2 power k)))
        (m (mod q 8) (mod q 8)))
      ((> q sqN) (values t))
    (when (and (or (= 1 m) (= 7 m))
               (primep q)
               (= 1 (modulo-power 2 power q)))
      (return (values nil q)))))
```


We can run the same tests from the [[#Ruby|Ruby]] entry.


```txt
> (loop for p in '(2 3 4 5 7 11 13 17 19 23 29 31 37 41 43 47 53 929)
        do (multiple-value-bind (primep factor)
               (mersenne-prime-p p)
             (format t "~&M~w = 2**~:*~w-1 is ~:[composite with factor ~w~;prime~]."
                     p primep factor)))
M2 = 2**2-1 is prime.
M3 = 2**3-1 is prime.
M4 = 2**4-1 is prime.
M5 = 2**5-1 is prime.
M7 = 2**7-1 is prime.
M11 = 2**11-1 is composite with factor 23.
M13 = 2**13-1 is prime.
M17 = 2**17-1 is prime.
M19 = 2**19-1 is prime.
M23 = 2**23-1 is composite with factor 47.
M29 = 2**29-1 is composite with factor 233.
M31 = 2**31-1 is prime.
M37 = 2**37-1 is composite with factor 223.
M41 = 2**41-1 is composite with factor 13367.
M43 = 2**43-1 is composite with factor 431.
M47 = 2**47-1 is composite with factor 2351.
M53 = 2**53-1 is composite with factor 6361.
M929 = 2**929-1 is composite with factor 13007.
```



## D


```d
import std.stdio, std.math, std.traits;

ulong mersenneFactor(in ulong p) pure nothrow @nogc {
    static bool isPrime(T)(in T n) pure nothrow @nogc {
        if (n < 2 || n % 2 == 0)
            return n == 2;
        for (Unqual!T i = 3; i ^^ 2 <= n; i += 2)
            if (n % i == 0)
                return false;
        return true;
    }

    static ulong modPow(in ulong cb, in ulong ce,in ulong m)
    pure nothrow @nogc {
        ulong b = cb;
        ulong result = 1;
        for (ulong e = ce; e > 0; e >>= 1) {
            if ((e & 1) == 1)
                result = (result * b) % m;
            b = (b ^^ 2) % m;
        }
        return result;
    }

    immutable ulong limit = p <= 64 ? cast(ulong)(real(2.0) ^^ p - 1).sqrt : uint.max; // prevents silent overflows
    for (ulong k = 1; (2 * p * k + 1) < limit; k++) {
        immutable ulong q = 2 * p * k + 1;
        if ((q % 8 == 1 || q % 8 == 7) && isPrime(q) &&
            modPow(2, p, q) == 1)
            return q;
    }
    return 1; // returns a sensible smallest factor
}

void main() {
    writefln("Factor of M929: %d", 929.mersenneFactor);
}
```

```txt
Factor of M929: 13007
```



## EchoLisp


```scheme

;; M = 2^P - 1 , P prime
;; look for a prime divisor q such as : q < √ M, q = 1 or 7 modulo 8, q = 1 + 2kP
;; q is divisor if (powmod 2 P q) = 1
;; m-divisor returns q or #f

(define  ( m-divisor P )
;; must limit the search as √ M may be HUGE
(define  maxprime  (min 1_000_000_000 (sqrt (expt 2 P))))
(for ((q (in-range 1 maxprime (* 2 P))))
	#:when (member (modulo q 8) '(1 7))
	#:when (prime? q)
	#:break (= 1 (powmod 2 P q)) => q
	#f ))

(m-divisor 929)
    → 13007
(m-divisor 4423)
    → #f

(lib 'bigint)
(prime? (1- (expt 2 4423))) ;; 2^4423 -1 is a Mersenne prime
    → #t


```



## Elixir

```elixir
defmodule Mersenne do
  def mersenne_factor(p) do
    limit = :math.sqrt(:math.pow(2, p) - 1)
    mersenne_loop(p, limit, 1)
  end

  defp mersenne_loop(p, limit, k) when (2*k*p - 1) > limit, do: nil
  defp mersenne_loop(p, limit, k) do
    q = 2*k*p + 1
    if prime?(q) and rem(q,8) in [1,7] and trial_factor(2,p,q),
      do: q, else: mersenne_loop(p, limit, k+1)
  end

  defp trial_factor(base, exp, mod) do
    Integer.digits(exp, 2)
    |> Enum.reduce(1, fn bit,square ->
      (square * square * (if bit==1, do: base, else: 1)) |> rem(mod)
    end) == 1
  end

  def check_mersenne(p) do
    IO.write "M#{p} = 2**#{p}-1 is "
    f = mersenne_factor(p)
    IO.puts if f, do: "composite with factor #{f}", else: "prime"
  end

  def prime?(n), do: prime?(n, :math.sqrt(n), 2)

  defp prime?(_, limit, i) when limit < i, do: true
  defp prime?(n, limit, i) do
    if rem(n,i) == 0, do: false, else: prime?(n, limit, i+1)
  end
end

[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,929]
|> Enum.each(fn p -> Mersenne.check_mersenne(p) end)
```


```txt

M2 = 2**2-1 is prime
M3 = 2**3-1 is prime
M5 = 2**5-1 is prime
M7 = 2**7-1 is prime
M11 = 2**11-1 is composite with factor 23
M13 = 2**13-1 is prime
M17 = 2**17-1 is prime
M19 = 2**19-1 is prime
M23 = 2**23-1 is composite with factor 47
M29 = 2**29-1 is composite with factor 233
M31 = 2**31-1 is prime
M37 = 2**37-1 is composite with factor 223
M41 = 2**41-1 is composite with factor 13367
M43 = 2**43-1 is composite with factor 431
M47 = 2**47-1 is composite with factor 2351
M53 = 2**53-1 is composite with factor 6361
M929 = 2**929-1 is composite with factor 13007

```



## Erlang


The modpow function is not my original. This is a translation of python, more or less.


```erlang

-module(mersene2).
-export([prime/1,modpow/3,mf/1]).

mf(P) -> merseneFactor(P,math:sqrt(math:pow(2,P)-1),2).

merseneFactor(P,Limit,Acc) when Acc >= Limit -> io:write("None found");
merseneFactor(P,Limit,Acc) ->
        Q = 2 * P * Acc + 1,
        Isprime = prime(Q),
        Mod = modpow(2,P,Q),

        if
            Isprime == false ->
               merseneFactor(P,Limit,Acc+1);

            Q rem 8 =/= 1 andalso Q rem 8 =/= 7 ->
               merseneFactor(P,Limit,Acc+1);

             Mod == 1 ->
                io:format("M~w is composite with Factor: ~w~n",[P,Q]);

            true -> merseneFactor(P,Limit,Acc+1)
        end.

modpow(B, E, M) -> modpow(B, E, M, 1).

modpow(_B, E, _M, R) when E =< 0 -> R;
modpow(B, E, M, R) ->
    R1 = case E band 1 =:= 1 of
             true -> (R * B) rem M;
             false  -> R
         end,
    modpow( (B*B) rem M, E bsr 1, M, R1).

prime(N) -> divisors(N, N-1).

divisors(N, 1) -> true;
divisors(N, C) ->
   case N rem C =:= 0 of
      true  -> false;
      false -> divisors(N, C-1)
   end.

```

```txt

30> [ mersene2:mf(X) || X <- [37,41,43,47,53,92,929]].
M37 is composite with Factor: 223
M41 is composite with Factor: 13367
M43 is composite with Factor: 431
M47 is composite with Factor: 2351
M53 is composite with Factor: 6361
M92 is composite with Factor: 1657
M929 is composite with Factor: 13007
[ok,ok,ok,ok,ok,ok,ok]

```



## Factor


```factor
USING: combinators.short-circuit interpolate io kernel locals
math math.bits math.functions math.primes sequences ;
IN: rosetta-code.mersenne-factors

: mod-pow-step ( square bit m -- square' )
    [ [ sq ] [ [ 2 * ] when ] bi* ] dip mod ;

:: mod-pow ( m q -- n )
    1 :> s! m make-bits <reversed>
    [ s swap q mod-pow-step s! ] each s ;

: halt-search? ( m q N -- ? )
    dupd > [
        {
            [ nip 8 mod [ 1 ] [ 7 ] bi [ = ] 2bi@ or ]
            [ mod-pow 1 = ] [ nip prime? ]
        } 2&&
    ] dip or ;

:: find-mersenne-factor ( m -- factor/f )
    1          :> k!
    2 m * 1 +  :> q!                 ! the tentative factor.
    2 m ^ sqrt :> N                  ! upper bound on search.
    [ m q N halt-search? ] [ k 1 + k! 2 k * m * 1 + q! ] until
    q N > f q ? ;

: test-mersenne ( m -- )
    dup find-mersenne-factor
    [ [I M${1} is not prime: factor ${0} found.I] ]
    [ [I No factor found for M${}.I] ] if* nl ;

929 test-mersenne
```

```txt

M929 is not prime: factor 13007 found.

```



## Forth


```forth
: prime? ( odd -- ? )
  3
  begin 2dup dup * >=
  while 2dup mod 0=
        if 2drop false exit
        then 2 +
  repeat   2drop true ;

: 2-exp-mod { e m -- 2^e mod m }
  1
  0 30 do
    e 1 i lshift >= if
      dup *
      e 1 i lshift and if 2* then
      m mod
    then
  -1 +loop ;

: factor-mersenne ( exponent -- factor )
  16384 over /  dup 2 < abort" Exponent too large!"
  1 do
    dup i * 2* 1+      ( q )
    dup prime? if
      dup 7 and  dup 1 = swap 7 = or if
        2dup 2-exp-mod 1 = if
          nip unloop exit
        then
      then
    then drop
  loop drop 0 ;

 929 factor-mersenne .  \ 13007
4423 factor-mersenne .  \ 0
```



## Fortran

```fortran
PROGRAM EXAMPLE
  IMPLICIT NONE
  INTEGER :: exponent, factor

  WRITE(*,*) "Enter exponent of Mersenne number"
  READ(*,*) exponent
  factor = Mfactor(exponent)
  IF (factor == 0) THEN
    WRITE(*,*) "No Factor found"
  ELSE
    WRITE(*,"(A,I0,A,I0)") "M", exponent, " has a factor: ", factor
  END IF

CONTAINS

FUNCTION isPrime(number)
!   code omitted - see [[Primality by Trial Division]]
END FUNCTION

FUNCTION  Mfactor(p)
  INTEGER :: Mfactor
  INTEGER, INTENT(IN) :: p
  INTEGER :: i, k,  maxk, msb, n, q

  DO i = 30, 0 , -1
    IF(BTEST(p, i)) THEN
      msb = i
      EXIT
    END IF
  END DO

  maxk = 16384  / p     ! limit for k to prevent overflow of 32 bit signed integer
  DO k = 1, maxk
    q = 2*p*k + 1
    IF (.NOT. isPrime(q)) CYCLE
    IF (MOD(q, 8) /= 1 .AND. MOD(q, 8) /= 7) CYCLE
    n = 1
    DO i = msb, 0, -1
      IF (BTEST(p, i)) THEN
        n = MOD(n*n*2, q)
      ELSE
        n = MOD(n*n, q)
      ENDIF
    END DO
    IF (n == 1) THEN
      Mfactor = q
      RETURN
    END IF
  END DO
  Mfactor = 0
END FUNCTION
END PROGRAM EXAMPLE
```

 M929 has a factor: 13007


## FreeBASIC

```freebasic
' FB 1.05.0 Win64

Function isPrime(n As Integer) As Boolean
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

' test 929 plus all prime numbers below 100 which are known not to be Mersenne primes
Dim q(1 To 16) As Integer = {11, 23, 29, 37, 41, 43, 47, 53, 59, 67, 71, 73, 79, 83, 97, 929}
For k As Integer = 1 To 16
  If isPrime(q(k)) Then
    Dim As Integer d, i, p, r = q(k)
    While r > 0 : r Shl= 1 : Wend
    d = 2 * q(k) + 1
    Do
      i = 1
      p = r
      While p <> 0
        i = (i * i) Mod d
        If p < 0 Then i *= 2
        If i > d Then i -= d
        p Shl= 1
      Wend
      If i <> 1 Then
        d += 2 * q(k)
      Else
        Exit Do
      End If
    Loop
    Print "2^"; Str(q(k)); Tab(6); " - 1 = 0 (mod"; d; ")"
  Else
    Print Str(q(k)); " is not prime"
  End If
Next
Print
Print "Press any key to quit"
Sleep
```


```txt

2^11  - 1 = 0 (mod 23)
2^23  - 1 = 0 (mod 47)
2^29  - 1 = 0 (mod 233)
2^37  - 1 = 0 (mod 223)
2^41  - 1 = 0 (mod 13367)
2^43  - 1 = 0 (mod 431)
2^47  - 1 = 0 (mod 2351)
2^53  - 1 = 0 (mod 6361)
2^59  - 1 = 0 (mod 179951)
2^67  - 1 = 0 (mod 193707721)
2^71  - 1 = 0 (mod 228479)
2^73  - 1 = 0 (mod 439)
2^79  - 1 = 0 (mod 2687)
2^83  - 1 = 0 (mod 167)
2^97  - 1 = 0 (mod 11447)
2^929 - 1 = 0 (mod 13007)

```



## GAP


```gap
MersenneSmallFactor := function(n)
    local k, m, d;
    if IsPrime(n) then
        d := 2*n;
        m := 1;
        for k in [1 .. 1000000] do
            m := m + d;
            if PowerModInt(2, n, m) = 1 then
                return m;
            fi;
        od;
    fi;
    return fail;
end;


# If n is not prime, fail immediately
MersenneSmallFactor(15);
# fail

MersenneSmallFactor(929);
# 13007

MersenneSmallFactor(1009);
# 3454817

# We stop at k = 1000000 in 2*k*n + 1, so it may fail if 2^n - 1 has only larger factors
MersenneSmallFactor(101);
# fail

FactorsInt(2^101-1);
# [ 7432339208719, 341117531003194129 ]
```



## Go


```go
package main

import (
    "fmt"
    "math"
)

// limit search to small primes.  really this is higher than
// you'd want it, but it's fun to factor M67.
const qlimit = 2e8

func main() {
    mtest(31)
    mtest(67)
    mtest(929)
}

func mtest(m int32) {
    // the function finds odd prime factors by
    // searching no farther than sqrt(N), where N = 2^m-1.
    // the first odd prime is 3, 3^2 = 9, so M3 = 7 is still too small.
    // M4 = 15 is first number for which test is meaningful.
    if m < 4 {
        fmt.Printf("%d < 4.  M%d not tested.\n", m, m)
        return
    }
    flimit := math.Sqrt(math.Pow(2, float64(m)) - 1)
    var qlast int32
    if flimit < qlimit {
        qlast = int32(flimit)
    } else {
        qlast = qlimit
    }
    composite := make([]bool, qlast+1)
    sq := int32(math.Sqrt(float64(qlast)))
loop:
    for q := int32(3); ; {
        if q <= sq {
            for i := q * q; i <= qlast; i += q {
                composite[i] = true
            }
        }
        if q8 := q % 8; (q8 == 1 || q8 == 7) && modPow(2, m, q) == 1 {
            fmt.Printf("M%d has factor %d\n", m, q)
            return
        }
        for {
            q += 2
            if q > qlast {
                break loop
            }
            if !composite[q] {
                break
            }
        }
    }
    fmt.Printf("No factors of M%d found.\n", m)
}

// base b to power p, mod m
func modPow(b, p, m int32) int32 {
    pow := int64(1)
    b64 := int64(b)
    m64 := int64(m)
    bit := uint(30)
    for 1<<bit&p == 0 {
        bit--
    }
    for {
        pow *= pow
        if 1<<bit&p != 0 {
            pow *= b64
        }
        pow %= m64
        if bit == 0 {
            break
        }
        bit--
    }
    return int32(pow)
}
```

```txt

No factors of M31 found.
M67 has factor 193707721
M929 has factor 13007

```



## Haskell


Using David Amos module Primes [https://web.archive.org/web/20121130222921/http://www.polyomino.f2s.com/david/haskell/codeindex.html] for prime number testing:


```haskell
import Data.List
import HFM.Primes (isPrime)
import Control.Monad
import Control.Arrow

int2bin = reverse.unfoldr(\x -> if x==0 then Nothing
                                else Just ((uncurry.flip$(,))$divMod x 2))

trialfac m = take 1. dropWhile ((/=1).(\q -> foldl (((`mod` q).).pm) 1 bs)) $ qs
  where qs = filter (liftM2 (&&) (liftM2 (||) (==1) (==7) .(`mod`8)) isPrime ).
              map (succ.(2*m*)). enumFromTo 1 $ m `div` 2
        bs = int2bin m
        pm n b = 2^b*n*n
```



```haskell
*Main> trialfac 929
[13007]
```


=={{header|Icon}} and {{header|Unicon}}==
The following works in both languages:

```unicon
procedure main(A)
    p := integer(A[1]) | 929
    write("M",p," has a factor ",mfactor(p))
end

procedure mfactor(p)
    if isPrime(p) then {
        limit := sqrt(2^p)-1
        k := 1
        while 2*p*k-1 < limit do {
            q := 2*p*k+1
            if isPrime(q) & (q%8 = (1|7)) & btest(p,q) then return q
            k +:= 1
            }
        }
end

procedure btest(p, q)
   return (2^p % q) = 1
end

procedure isPrime(n)
    if n%(i := 2|3) = 0 then return n = i;
    d := 5
    while d*d <= n do {
        if n%d = 0 then fail
        d +:= 2
        if n%d = 0 then fail
        d +:= 4
        }
    return
end
```


Sample runs:

```txt

->fmn
M929 has a factor 13007
->fmn 41
M41 has a factor 13367
->

```



## J



```j
trialfac=: 3 : 0
  qs=. (#~8&(1=|+.7=|))(#~1&p:)1+(*(1x+i.@<:@<.)&.-:)y
  qs#~1=qs&|@(2&^@[**:@])/ 1,~ |.#: y
)
```


```j
trialfac 929
13007
```


```j>trialfac 44497</lang

Empty output --> No factors found.


## Java


```java

import java.math.BigInteger;

class MersenneFactorCheck
{

  private final static BigInteger TWO = BigInteger.valueOf(2);

  public static boolean isPrime(long n)
  {
    if (n == 2)
      return true;
    if ((n < 2) || ((n & 1) == 0))
      return false;
    long maxFactor = (long)Math.sqrt((double)n);
    for (long possibleFactor = 3; possibleFactor <= maxFactor; possibleFactor += 2)
      if ((n % possibleFactor) == 0)
        return false;
    return true;
  }

  public static BigInteger findFactorMersenneNumber(int primeP)
  {
    if (primeP <= 0)
      throw new IllegalArgumentException();
    BigInteger bigP = BigInteger.valueOf(primeP);
    BigInteger m = BigInteger.ONE.shiftLeft(primeP).subtract(BigInteger.ONE);
    // There are more complicated ways of getting closer to sqrt(), but not that important here, so go with simple
    BigInteger maxFactor = BigInteger.ONE.shiftLeft((primeP + 1) >>> 1);
    BigInteger twoP = BigInteger.valueOf(primeP << 1);
    BigInteger possibleFactor = BigInteger.ONE;
    int possibleFactorBits12 = 0;
    int twoPBits12 = primeP & 3;

    while ((possibleFactor = possibleFactor.add(twoP)).compareTo(maxFactor) <= 0)
    {
      possibleFactorBits12 = (possibleFactorBits12 + twoPBits12) & 3;
      // "Furthermore, q must be 1 or 7 mod 8". We know it's odd due to the +1 done above, so bit 0 is set. Therefore, we only care about bits 1 and 2 equaling 00 or 11
      if ((possibleFactorBits12 == 0) || (possibleFactorBits12 == 3))
        if (TWO.modPow(bigP, possibleFactor).equals(BigInteger.ONE))
          return possibleFactor;
    }
    return null;
  }

  public static void checkMersenneNumber(int p)
  {
    if (!isPrime(p))
    {
      System.out.println("M" + p + " is not prime");
      return;
    }
    BigInteger factor = findFactorMersenneNumber(p);
    if (factor == null)
      System.out.println("M" + p + " is prime");
    else
      System.out.println("M" + p + " is not prime, has factor " + factor);
    return;
  }

  public static void main(String[] args)
  {
    for (int p = 1; p <= 50; p++)
      checkMersenneNumber(p);
    checkMersenneNumber(929);
    return;
  }

}

```


```txt
M1 is not prime
M2 is prime
M3 is prime
M4 is not prime
M5 is prime
M6 is not prime
M7 is prime
M8 is not prime
M9 is not prime
M10 is not prime
M11 is not prime, has factor 23
M12 is not prime
M13 is prime
M14 is not prime
...
M47 is not prime, has factor 2351
M48 is not prime
M49 is not prime
M50 is not prime
M929 is not prime, has factor 13007

```



## JavaScript



```javascript
function mersenne_factor(p){
  var limit, k, q
  limit = Math.sqrt(Math.pow(2,p) - 1)
  k = 1
  while ((2*k*p - 1) < limit){
    q = 2*k*p + 1
    if (isPrime(q) && (q % 8 == 1 || q % 8 == 7) && trial_factor(2,p,q)){
      return q // q is a factor of 2**p-1
    }
    k++
  }
  return null
}

function isPrime(value){
  for (var i=2; i < value; i++){
    if (value % i == 0){
      return false
    }
    if (value % i != 0){
      return true;
	 }
  }
}

function trial_factor(base, exp, mod){
  var square, bits
  square = 1
  bits = exp.toString(2).split('')
  for (var i=0,ln=bits.length; i<ln; i++){
    square = Math.pow(square, 2) * (bits[i] == 1 ? base : 1) % mod
  }
  return (square == 1)
}

function check_mersenne(p){
  var f, result
  console.log("M"+p+" = 2^"+p+"-1 is ")
  f = mersenne_factor(p)
  console.log(f == null ? "prime" : "composite with factor "+f)
}
```



```txt

> check_mersenne(3)
"M3 = 2**3-1 is prime"
> check_mersenne(23)
"M23 = 2**23-1 is composite with factor 47"
> check_mersenne(929)
"M929 = 2**929-1 is composite with factor 13007"

```



## Julia


```julia
# v0.6

using Primes

function mersennefactor(p::Int)::Int
    q = 2p + 1
    while true
        if log2(q) > p / 2
            return -1
        elseif q % 8 in (1, 7) && Primes.isprime(q) && powermod(2, p, q) == 1
            return q
        end
    q += 2p
    end
end

for i in filter(Primes.isprime, push!(collect(1:20), 929))
    mf = mersennefactor(i)
    if mf != -1 println("M$i = ", mf, " × ", (big(2) ^ i - 1) ÷ mf)
    else println("M$i is prime") end
end
```


```txt
M2 is prime
M3 is prime
M5 is prime
M7 is prime
M11 = 23 × 89
M13 is prime
M17 is prime
M19 is prime
M929 = 13007 × 34889024892493825975045478116339093030512026953827804293400962134
88946572057852012474541189660261508521493994102599382170621001921687473524507195
61908445272675574320888385228421992652298715687625495638077382028762529439880103
124705348782610789919949159935587158612289264184273
```



## Kotlin

```scala
// version 1.0.6

fun isPrime(n: Int): Boolean {
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

fun main(args: Array<String>) {
    // test 929 plus all prime numbers below 100 which are known not to be Mersenne primes
    val q = intArrayOf(11, 23, 29, 37, 41, 43, 47, 53, 59, 67, 71, 73, 79, 83, 97, 929)
    for (k in 0 until q.size) {
        if (isPrime(q[k])) {
            var i: Long
            var d: Int
            var p: Int
            var r: Int = q[k]
            while (r > 0) r = r shl 1
            d = 2 * q[k] + 1
            while (true) {
                i = 1L
                p = r
                while (p != 0) {
                    i = (i * i) % d
                    if (p < 0) i *= 2
                    if (i > d) i -= d
                    p = p shl 1
                }
                if (i != 1L)
                    d += 2 * q[k]
                else
                    break
            }
            println("2^${"%3d".format(q[k])} - 1 = 0 (mod $d)")
        } else {
            println("${q[k]} is not prime")
        }
    }
}
```


```txt

2^ 11 - 1 = 0 (mod 23)
2^ 23 - 1 = 0 (mod 47)
2^ 29 - 1 = 0 (mod 233)
2^ 37 - 1 = 0 (mod 223)
2^ 41 - 1 = 0 (mod 13367)
2^ 43 - 1 = 0 (mod 431)
2^ 47 - 1 = 0 (mod 2351)
2^ 53 - 1 = 0 (mod 6361)
2^ 59 - 1 = 0 (mod 179951)
2^ 67 - 1 = 0 (mod 193707721)
2^ 71 - 1 = 0 (mod 228479)
2^ 73 - 1 = 0 (mod 439)
2^ 79 - 1 = 0 (mod 2687)
2^ 83 - 1 = 0 (mod 167)
2^ 97 - 1 = 0 (mod 11447)
2^929 - 1 = 0 (mod 13007)

```



## Lingo


```Lingo
on modPow (b, e, m)
    bits = getBits(e)
    sq = 1
    repeat while TRUE
        tb = bits[1]
        bits.deleteAt(1)
        sq = sq*sq
        if tb then sq=sq*b
        sq = sq mod m
        if bits.count=0 then return sq
    end repeat
end

on getBits (n)
    bits = []
    f = 1
    repeat while TRUE
        bits.addAt(1, bitAnd(f, n)>0)
        f = f * 2
        if f>n then exit repeat
    end repeat
    return bits
end
```



```Lingo
repeat with i = 2 to the maxInteger
    if modPow(2, 929, i)=1 then
        put "M929 has a factor: " & i
        exit repeat
    end if
end repeat
```


```txt

-- "M929 has a factor: 13007"

```



## Mathematica

Believe it or not, this type of test runs faster in Mathematica than the squaring version described above.


```mathematica

For[i = 2, i < Prime[1000000], i = NextPrime[i],
 If[Mod[2^44497, i] == 1,
  Print["divisible by "<>i]]]; Print["prime test passed; call Lucas and Lehmer"]
```



## Maxima


```maxima
mersenne_fac(p) := block([m: 2^p - 1, k: 1],
   while mod(m, 2 * k * p + 1) # 0 do k: k + 1,
   2 * k * p + 1
)$

mersenne_fac(929);
/* 13007 */
```



## Nim

```nim
import math

proc isPrime(a: int): bool =
  if a == 2: return true
  if a < 2 or a mod 2 == 0: return false
  for i in countup(3, int sqrt(float a), 2):
    if a mod i == 0:
      return false
  return true

const q = 929
if not isPrime q: quit 1
var r = q
while r > 0: r = r shl 1
var d = 2 * q + 1
while true:
  var i = 1
  var p = r
  while p != 0:
    i = (i * i) mod d
    if p < 0: i *= 2
    if i > d: i -= d
    p = p shl 1
  if i != 1: d += 2 * q
  else: break
echo "2^",q," - 1 = 0 (mod ",d,")"
```

```txt
2^929 - 1 = 0 (mod 13007)
```



## Octave

(GNU Octave has a <code>isprime</code> built-in test)


```octave
% test a bit; lsb is 1 (like built-in bit* ops)
function b = bittst(n, p)
  b = bitand(n, 2^(p-1)) > 0;
endfunction

function f = Mfactor(p)
  % msb is the index of the first non-zero bit
  [b, msb] = max(bitand(p, 2 .^ [32:-1:1]) > 0);
  maxk = floor(sqrt(intmax()) / p);
  for k = 1 : maxk
    q = 2*p*k + 1;
    if ( ! isprime(q) )
      continue;
    endif
    if ( (mod(q, 8) != 1) && ( mod(q, 8) != 7) )
      continue;
    endif
    n = 1;
    for i = msb:-1:1
      if ( bittst(p, i) )
	n = mod(n*n*2, q);
      else
	n = mod(n*n, q);
      endif
    endfor
    if ( n==1 )
      f = q;
      return
    endif
  endfor
  f = 0;
endfunction

printf("%d\n", Mfactor(929));
```



## PARI/GP

This version takes about 15 microseconds to find a factor of 2<sup>929</sup> &minus; 1.

```parigp
factorMersenne(p)={
  forstep(q=2*p+1,sqrt(2)<<(p\2),2*p,
    [1,0,0,0,0,0,1][q%8] && Mod(2, q)^p==1 && return(q)
  );
  1<<p-1
};
factorMersenne(929)
```


This implementation seems to be broken:

```parigp
TM(p) = local(status=1, i=1, len=0, S=0);{
printp("Test TM \t...");
S=2*p+1;
len = length(binary(p));
B = Vecsmall(binary(p));
q = B[i]*B[i];
while( i<=len & status ==1,
       if( B[i] != 0,
           q = q*2;
       );
       r = q%S;
       q = r*r;
       if( i == len & r == 1,
           status = 0;
           printp("Not Prime!");
       );
       i++;
);
return(status);
}
```



## Pascal

```pascal
program FactorsMersenneNumber(input, output);

function isPrime(n: longint): boolean;
  var
    d: longint;
  begin
    isPrime := true;
    if (n mod 2) = 0 then
    begin
      isPrime := (n = 2);
      exit;
    end;
    if (n mod 3) = 0 then
    begin
      isPrime := (n = 3);
      exit;
    end;
    d := 5;
    while d*d <= n do
    begin
      if (n mod d) = 0 then
      begin
	isPrime := false;
	exit;
      end;
      d := d + 2;
    end;
  end;

function btest(n, pos: longint): boolean;
  begin
    btest := (n shr pos) mod 2 = 1;
  end;

function MFactor(p: longint): longint;
  var
    i, k,  maxk, msb, n, q: longint;
  begin
    for i := 30 downto 0 do
      if btest(p, i) then
      begin
	msb := i;
	break;
      end;
    maxk := 16384 div p;     // limit for k to prevent overflow of 32 bit signed integer
    for k := 1 to maxk do
    begin
      q := 2*p*k + 1;
      if not isprime(q) then
	continue;
      if ((q mod 8) <> 1) and ((q mod 8) <> 7) then
	continue;
      n := 1;
      for i := msb downto 0 do
	if btest(p, i) then
	  n := (n*n*2) mod q
	else
	  n := (n*n) mod q;
      if n = 1 then
      begin
	mfactor := q;
	exit;
      end;
    end;
    mfactor := 0;
  end;

var
  exponent, factor: longint;

begin
  write('Enter the exponent of the Mersenne number (suggestion: 929): ');
  readln(exponent);
  if not isPrime(exponent) then
  begin
    writeln('M', exponent, ' (2**', exponent, ' - 1) is not prime.');
    exit;
  end;
  factor := MFactor(exponent);
  if factor = 0 then
    writeln('M', exponent, ' (2**', exponent, ' - 1) has no factor.')
  else
    writeln('M', exponent, ' (2**', exponent, ' - 1) has the factor: ', factor);
end.
```

```txt

:> ./FactorsMersenneNumber
Enter the exponent of the Mersenne number (suggestion: 929): 929
M929 (2**929 - 1) has the factor: 13007

```



## Perl


```perl
use strict;
use utf8;

sub factors {
	my $n = shift;
	my $p = 2;
	my @out;

	while ($n >= $p * $p) {
		while ($n % $p == 0) {
			push @out, $p;
			$n /= $p;
		}
		$p = next_prime($p);
	}
	push @out, $n if $n > 1 || !@out;
	@out;
}

sub next_prime {
	my $p = shift;
	do { $p = $p == 2 ? 3 : $p + 2 } until is_prime($p);
	$p;
}

my %pcache;
sub is_prime {
	my $x = shift;
	$pcache{$x} //=	(factors($x) == 1)
}

sub mtest {
	my @bits = split "", sprintf("%b", shift);
	my $p = shift;
	my $sq = 1;
	while (@bits) {
		$sq = $sq * $sq;
		$sq *= 2 if shift @bits;
		$sq %= $p;
	}
	$sq == 1;
}

for my $m (2 .. 60, 929) {
	next unless is_prime($m);
	use bigint;

	my ($f, $k, $x) = (0, 0, 2**$m - 1);

	my $q;
	while (++$k) {
		$q = 2 * $k * $m + 1;
		next if (($q & 7) != 1 && ($q & 7) != 7);
		next unless is_prime($q);
		last if $q * $q > $x;
		last if $f = mtest($m, $q);
	}

	print $f? "M$m = $x = $q × @{[$x / $q]}\n"
		: "M$m = $x is prime\n";
}
```

```txt
M2 = 3 is prime
M2 = 3 is prime
M3 = 7 is prime
M5 = 31 is prime
M7 = 127 is prime
M11 = 2047 = 23  × 89
M13 = 8191 is prime
...
M53 = 9007199254740991 = 6361 × 1416003655831
M59 = 576460752303423487 = 179951 × 3203431780337
M929 = 4538..<yadda yadda>..8911 = 13007 × 348890..<blah blah>..84273
```


Following the task introduction, this uses GMP's modular exponentiation and simple probable prime test for the small numbers, then looks for small factors before doing a Lucas-Lehmer test.  For ranges above about p=2000, looking for small factors this way saves time (the amount of testing should be adjusted based on the input size and platform -- this example just uses a fixed amount).  Note as well that the Lucas-Lehmer test shown here is ignoring the large speedup we can get by optimizing the modulo operation, but that's a different task.

```perl
use Math::GMP;

# Use GMP's simple probable prime test.
sub is_prime { Math::GMP->new(shift)->probab_prime(20); }

# Lucas-Lehmer test, deterministic for 2^p-1 given p
sub is_mersenne_prime {
  my($p, $mp, $s) = ($_[0], Math::GMP->new(2)**$_[0]-1, Math::GMP->new(4));
  return 1 if $p == 2;
  $s = ($s * $s - 2) % $mp  for 3 .. $p;
  $s == 0;
}

for my $p (2 .. 100, 929) {
  next unless is_prime($p);
  my $mp = Math::GMP->new(2) ** $p - 1;
  my $lim = $mp->bsqrt();
  $lim = 1000000 if $lim > 1000000;   # We're using it as a pre-test
  my $factor;
  for (my $q = Math::GMP->new(2*$p+1);  $q <= $lim && !$factor;  $q += 2*$p) {
    next unless ($q & 7) == 1 || ($q & 7) == 7;
    next unless is_prime($q);
    $factor = $q if Math::GMP->new(2)->powm_gmp($p,$q) == 1;  #  $mp % $q == 0
  }
  if ($factor) {
    print "M$p = $factor * ",$mp/$factor,"\n";
  } else {
    print "M$p is ", is_mersenne_prime($p) ? "prime" : "composite", "\n";
  }
}
```

```txt

M2 is prime
M3 is prime
M5 is prime
M7 is prime
M11 = 23 * 89
M13 is prime
M17 is prime
M19 is prime
M23 = 47 * 178481
M29 = 233 * 2304167
M31 is prime
M37 = 223 * 616318177
M41 = 13367 * 164511353
M43 = 431 * 20408568497
M47 = 2351 * 59862819377
M53 = 6361 * 1416003655831
M59 = 179951 * 3203431780337
M61 is prime
M67 is composite
M71 = 228479 * 10334355636337793
M73 = 439 * 21514198099633918969
M79 = 2687 * 224958284260258499201
M83 = 167 * 57912614113275649087721
M89 is prime
M97 = 11447 * 13842607235828485645766393
M929 = 13007 * 348890248924[.....]64184273

```



## Perl 6

```perl6
my @primes = 2, 3, -> $n is copy {
    repeat { $n += 2 } until $n %% none do for @primes -> $p {
        last if $p > sqrt($n);
        $p;
    }
    $n;
} ... *;

multi factors(1) { 1 }
multi factors(Int $remainder is copy) {
  gather for @primes -> $factor {
    if $factor * $factor > $remainder {
      take $remainder if $remainder > 1;
      last;
    }
    while $remainder %% $factor {
        take $factor;
        last if ($remainder div= $factor) === 1;
    }
  }
}

sub is_prime($x) { (state %){$x} //= factors($x) == 1 }

sub mtest($bits, $p) {
    my @bits = $bits.base(2).comb;
    loop (my $sq = 1; @bits; $sq %= $p) {
	$sq *= $sq;
	$sq += $sq if 1 == @bits.shift;
    }
    $sq == 1;
}

for flat 2 .. 60, 929 -> $m {
    next unless is_prime($m);
    my $f = 0;
    my $x = 2**$m - 1;
    my $q;
    for 1..* -> $k {
	$q = 2 * $k * $m + 1;
	next unless $q % 8 == 1|7 or is_prime($q);
	last if $q * $q > $x or $f = mtest($m, $q);
    }

    say $f ?? "M$m = $x\n\t= $q × { $x div $q }"
           !! "M$m = $x is prime";
}
```

```txt
M2 = 3 is prime
M3 = 7 is prime
M5 = 31 is prime
M7 = 127 is prime
M11 = 2047
	= 23 × 89
M13 = 8191 is prime
M17 = 131071 is prime
M19 = 524287 is prime
M23 = 8388607
	= 47 × 178481
M29 = 536870911
	= 233 × 2304167
M31 = 2147483647 is prime
M37 = 137438953471
	= 223 × 616318177
M41 = 2199023255551
	= 13367 × 164511353
M43 = 8796093022207
	= 431 × 20408568497
M47 = 140737488355327
	= 2351 × 59862819377
M53 = 9007199254740991
	= 6361 × 1416003655831
M59 = 576460752303423487
	= 179951 × 3203431780337
M929 = 4538015467766671944574165338592225830478699345884382504442663144885072806275648112625635725391102144133907238129251016389326737199538896813326509341743147661691195191795226666084858428449394948944821764472508048114220424520501343042471615418544488778723282182172070046459244838911
	= 13007 × 348890248924938259750454781163390930305120269538278042934009621348894657205785201247454118966026150852149399410259938217062100192168747352450719561908445272675574320888385228421992652298715687625495638077382028762529439880103124705348782610789919949159935587158612289264184273
```



## Phix

Translation/Amalgamation of BBC BASIC, D, and Go

```Phix
function is_prime(integer n)
    if n<2 then return 0 end if
    if n=2 then return 1 end if
    if remainder(n,2)=0 then return 0 end if
    for i=3 to floor(sqrt(n)) by 2 do
        if remainder(n,i)=0 then
            return 0
        end if
    end for
    return 1
end function

function modpow(atom x, atom n, atom m)
    atom i = n,
         y = 1,
         z = x
    while i do
        if and_bits(i,1) then
            y = mod(y*z,m)
        end if
        z = mod(z*z,m)
        i = floor(i/2)
    end while
    return y
end function

function mersenne_factor(integer p)
    if not is_prime(p) then return -1 end if
    atom limit = sqrt(power(2,p))-1
    integer k = 1
    while 1 do
        atom q = 2*p*k + 1
        if q>=limit then exit end if
        if find(mod(q,8),{1,7})
        and is_prime(q)
        and modpow(2,p,q)=1 then
            return q
        end if
        k += 1
    end while
    return 0
end function

sequence tests = {11, 23, 29, 37, 41, 43, 47, 53, 59, 67, 71, 73, 79, 83, 97, 929, 937}
for i=1 to length(tests) do
    integer ti = tests[i]
    printf(1,"A factor of M%d is %d\n",{ti,mersenne_factor(ti)})
end for
```

```txt

A factor of M11 is 23
A factor of M23 is 47
A factor of M29 is 233
A factor of M37 is 223
A factor of M41 is 13367
A factor of M43 is 431
A factor of M47 is 2351
A factor of M53 is 6361
A factor of M59 is 179951
A factor of M67 is 193707721
A factor of M71 is 228479
A factor of M73 is 439
A factor of M79 is 2687
A factor of M83 is 167
A factor of M97 is 11447
A factor of M929 is 13007
A factor of M937 is 28111

```



## PHP

Requires bcmath

```php
echo 'M929 has a factor: ',  mersenneFactor(929), '</br>';

function mersenneFactor($p) {
    $limit = sqrt(pow(2, $p) - 1);
    for ($k = 1; 2 * $p * $k - 1 < $limit; $k++) {
        $q = 2 * $p * $k + 1;
        if (isPrime($q) && ($q % 8 == 1 || $q % 8 == 7) && bcpowmod("2", "$p", "$q") == "1") {
            return $q;
        }
    }
    return 0;
}

function isPrime($n) {
    if ($n < 2 || $n % 2 == 0) return $n == 2;
    for ($i = 3; $i * $i <= $n; $i += 2) {
        if ($n % $i == 0) {
            return false;
        }
    }
    return true;
}
```


```txt
M929 has a factor: 13007
```



## PicoLisp


```PicoLisp
(de **Mod (X Y N)
   (let M 1
      (loop
         (when (bit? 1 Y)
            (setq M (% (* M X) N)) )
         (T (=0 (setq Y (>> 1 Y)))
            M )
         (setq X (% (* X X) N)) ) ) )

(de prime? (N)
   (or
      (= N 2)
      (and
         (> N 1)
         (bit? 1 N)
         (let S (sqrt N)
            (for (D 3  T  (+ D 2))
               (T (> D S) T)
               (T (=0 (% N D)) NIL) ) ) ) ) )

(de mFactor (P)
   (let (Lim (sqrt (dec (** 2 P)))  K 0  Q)
      (loop
         (setq Q (inc (* 2 (inc 'K) P)))
         (T (>= Q Lim) NIL)
         (T
            (and
               (member (% Q 8) (1 7))
               (prime? Q)
               (= 1 (**Mod 2 P Q)) )
            Q ) ) ) )
```

```txt
: (for P (2 3 4 5 7 11 13 17 19 23 29 31 37 41 43 47 53 929)
   (prinl
      "M" P " = 2**" P "-1 is "
      (cond
         ((not (prime? P)) "not prime")
         ((mFactor P) (pack "composite with factor " @))
         (T "prime") ) ) )
M2 = 2**2-1 is prime
M3 = 2**3-1 is prime
M4 = 2**4-1 is not prime
M5 = 2**5-1 is prime
M7 = 2**7-1 is prime
M11 = 2**11-1 is composite with factor 23
M13 = 2**13-1 is prime
M17 = 2**17-1 is prime
M19 = 2**19-1 is prime
M23 = 2**23-1 is composite with factor 47
M29 = 2**29-1 is composite with factor 233
M31 = 2**31-1 is prime
M37 = 2**37-1 is composite with factor 223
M41 = 2**41-1 is composite with factor 13367
M43 = 2**43-1 is composite with factor 431
M47 = 2**47-1 is composite with factor 2351
M53 = 2**53-1 is composite with factor 6361
M929 = 2**929-1 is composite with factor 13007
```



## Python



```python
def is_prime(number):
    return True # code omitted - see Primality by Trial Division

def m_factor(p):
    max_k = 16384 / p # arbitrary limit; since Python automatically uses long's, it doesn't overflow
    for k in xrange(max_k):
        q = 2*p*k + 1
        if not is_prime(q):
            continue
        elif q % 8 != 1 and q % 8 != 7:
            continue
        elif pow(2, p, q) == 1:
            return q
    return None

if __name__ == '__main__':
    exponent = int(raw_input("Enter exponent of Mersenne number: "))
    if not is_prime(exponent):
        print "Exponent is not prime: %d" % exponent
    else:
        factor = m_factor(exponent)
        if not factor:
            print "No factor found for M%d" % exponent
        else:
            print "M%d has a factor: %d" % (exponent, factor)
```


```txt

Enter exponent of Mersenne number: 929
M929 has a factor: 13007

```



## Racket


```racket

#lang racket

(define (number->digits n)
  (map (compose1 string->number string)
       (string->list (number->string n 2))))

(define (modpow exp base)
  (for/fold ([square 1])
    ([d (number->digits exp)])
    (modulo (* (if (= d 1) 2 1) square square) base)))

; Search through all integers from 1 on to find the first divisor.
; Returns #f if 2^p-1 is prime.
(define (mersenne-factor p)
  (for/first ([i (in-range 1 (floor (expt 2 (quotient p 2))) (* 2 p))]
              #:when (and (member (modulo i 8) '(1 7))
                          (= 1 (modpow p i))))
    i))

(mersenne-factor 929)

```

```racket

13007

```



## REXX

REXX practically has no limit (well, up to around 8 million) on the number of decimal digits (precision).

This REXX version automatically adjusts the '''numeric digits''' to whatever is needed.

```rexx
/*REXX program uses  exponent─and─mod  operator to test possible Mersenne numbers.      */
numeric digits 20                                /*this will be increased if necessary. */
parse arg N spec                                 /*obtain optional arguments from the CL*/
if    N=='' |  N==","     then    N=  88         /*Not specified?  Then use the default.*/
if spec=='' | spec==","   then spec= 920 970     /* "      "         "   "   "     "    */
      do j=1;      z=j                           /*process a range, & then do one more #*/
      if j=N  then j=word(spec, 1);              /*now, use  the high range of numbers. */
      if j>word(spec, 2)  then leave             /*done with the high range of numbers? */
      if \isPrime(z)  then iterate               /*if  Z  isn't a prime,  keep plugging.*/
      r=testMer(z)                               /*If Z is prime, give Z the 3rd degree.*/
      r=commas(r);    L=length(r)                /*add commas to R; get it's new length.*/
      if r==0  then say right('M'z, 10)  "──────── is a Mersenne prime."
               else say right('M'z, 50)  "is composite, a factor:"   right(r, max(L, 11) )
      end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas:  parse arg _; do jc=length(_)-3  to 1  by -3; _=insert(',', _, jc); end;  return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure; parse arg x;             if wordpos(x, '2 3 5 7') \== 0  then return 1
         if x<11  then return 0;             if x//2 == 0 | x//3       == 0  then return 0
              do j=5  by 6;                  if x//j == 0 | x//(j+2)   == 0  then return 0
              if j*j>x   then return 1                 /*◄─┐         ___                */
              end   /*j*/                              /*  └─◄ Is j>√ x ?  Then return 1*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt:   procedure; parse arg x;   #=1;      r=0;              do while #<=x;  #=#*4;  end
           do while #>1;  #=#%4;   _=x-r-#;  r=r%2;   if _>=0  then do;  x=_;  r=r+#;  end
           end   /*while*/                             /*iSqrt ≡    integer square root.*/
         return r                                      /*─────      ─       ──     ─  ─ */
/*──────────────────────────────────────────────────────────────────────────────────────*/
testMer: procedure;  parse arg x;              p=2**x  /* [↓]  do we have enough digits?*/
         $$=x2b( d2x(x) ) + 0
         if pos('E',p)\==0  then do; parse var p "E" _;  numeric digits _+2;  p=2**x;  end
         !.=1;  !.1=0;  !.7=0                          /*array used for a quicker test. */
         R=iSqrt(p)                                    /*obtain integer square root of P*/
                    do k=2  by 2;        q=k*x  +  1   /*(shortcut) compute value of Q. */
                    m=q // 8                           /*obtain the remainder when ÷ 8. */
                    if !.m               then iterate  /*M  must be either one or seven.*/
                    parse var q '' -1 _; if _==5  then iterate    /*last digit a five ? */
                    if q//3==0  then iterate                      /*divisible by three? */
                    if q//7==0  then iterate           /*      ____     "      " seven? */
                    if q>R               then return 0 /*Is q>√2**x ?   A Mersenne prime*/
                    sq=1;         $=$$                 /*obtain binary version from  $. */
                        do  until $=='';      sq=sq*sq
                        parse var $  _  2  $           /*obtain 1st digit and the rest. */
                        if _  then sq=(sq+sq) // q
                        end   /*until*/
                    if sq==1  then return q            /*Not a prime?   Return a factor.*/
                    end   /*k*/
```

Program note:   the   '''iSqrt'''   function computes the integer square root of a non-negative integer without using any floating point, just integers.

```txt

        M2 ──────── is a Mersenne prime.
        M3 ──────── is a Mersenne prime.
        M5 ──────── is a Mersenne prime.
        M7 ──────── is a Mersenne prime.
                                               M11 is composite, a factor:          23
       M13 ──────── is a Mersenne prime.
       M17 ──────── is a Mersenne prime.
       M19 ──────── is a Mersenne prime.
                                               M23 is composite, a factor:          47
                                               M29 is composite, a factor:         233
       M31 ──────── is a Mersenne prime.
                                               M37 is composite, a factor:         223
                                               M41 is composite, a factor:      13,367
                                               M43 is composite, a factor:         431
                                               M47 is composite, a factor:       2,351
                                               M53 is composite, a factor:       6,361
                                               M59 is composite, a factor:     179,951
       M61 ──────── is a Mersenne prime.
                                               M67 is composite, a factor: 193,707,721
                                               M71 is composite, a factor:     228,479
                                               M73 is composite, a factor:         439
                                               M79 is composite, a factor:       2,687
                                               M83 is composite, a factor:         167
                                              M929 is composite, a factor:      13,007
                                              M937 is composite, a factor:      28,111
                                              M941 is composite, a factor:       7,529
                                              M947 is composite, a factor: 295,130,657
                                              M953 is composite, a factor:     343,081
                                              M967 is composite, a factor:      23,209

```



## Ring


```ring

# Project : Factors of a Mersenne number

see "A factor of M929 is " + mersennefactor(929) + nl
see "A factor of M937 is " + mersennefactor(937) + nl

func mersennefactor(p)
       if not isprime(p)
         return -1
       ok
       for k = 1 to 50
            q = 2*k*p + 1
            if (q && 7) = 1 or (q && 7) = 7
               if isprime(q)
                  if modpow(2, p, q) = 1
                     return q
                  ok
               ok
            ok
       next
       return 0

func isprime(num)
       if (num <= 1) return 0 ok
       if (num % 2 = 0) and num != 2 return 0 ok
       for i = 3 to floor(num / 2) -1 step 2
            if (num % i = 0) return 0 ok
       next
       return 1

func modpow(x,n,m)
       i = n
       y = 1
       z = x
       while i > 0
               if i & 1
                  y = (y * z) % m
               ok
               z = (z * z) % m
               i = (i >> 1)
        end
        return y

```

Output:

```txt

A factor of M929 is 13007
A factor of M937 is 28111

```



## Ruby

```ruby
require 'prime'

def mersenne_factor(p)
  limit = Math.sqrt(2**p - 1)
  k = 1
  while (2*k*p - 1) < limit
    q = 2*k*p + 1
    if q.prime? and (q % 8 == 1 or q % 8 == 7) and trial_factor(2,p,q)
      # q is a factor of 2**p-1
      return q
    end
    k += 1
  end
  nil
end

def trial_factor(base, exp, mod)
  square = 1
  ("%b" % exp).each_char {|bit| square = square**2 * (bit == "1" ? base : 1) % mod}
  (square == 1)
end

def check_mersenne(p)
  print "M#{p} = 2**#{p}-1 is "
  f = mersenne_factor(p)
  if f.nil?
    puts "prime"
  else
    puts "composite with factor #{f}"
  end
end

Prime.each(53) { |p| check_mersenne p }
check_mersenne 929
```


```txt
M2 = 2**2-1 is prime
M3 = 2**3-1 is prime
M5 = 2**5-1 is prime
M7 = 2**7-1 is prime
M11 = 2**11-1 is composite with factor 23
M13 = 2**13-1 is prime
M17 = 2**17-1 is prime
M19 = 2**19-1 is prime
M23 = 2**23-1 is composite with factor 47
M29 = 2**29-1 is composite with factor 233
M31 = 2**31-1 is prime
M37 = 2**37-1 is composite with factor 223
M41 = 2**41-1 is composite with factor 13367
M43 = 2**43-1 is composite with factor 431
M47 = 2**47-1 is composite with factor 2351
M53 = 2**53-1 is composite with factor 6361
M929 = 2**929-1 is composite with factor 13007
```



## Scala

===Full-blown version===

```Scala
/** Find factors of a Mersenne number
 *
 *  The implementation finds factors for M929 and further.
 *
 *  @example  M59 = 2^059 - 1 =             576460752303423487  (   2 msec)
 *  @example        = 179951 × 3203431780337.
 */
object FactorMersenne extends App {

  val two: BigInt = 2

  def sieve(nums: Stream[Int]): Stream[Int] =
    Stream.cons(nums.head, sieve((nums.tail) filter (_ % nums.head != 0)))
  // An infinite stream of primes, lazy evaluation and memo-ized
  val oddPrimes = sieve(Stream.from(3, 2))
  def primes = sieve(2 #:: oddPrimes)

  def mersenne(p: Int) = (two pow p) - 1

  def factorMersenne(p: Int): Option[Long] = {
    val limit = (mersenne(p) - 1 min Int.MaxValue).toLong

    def factorTest(p: Long, q: Long): Boolean = {
      (List(1, 7) contains (q % 8)) && two.modPow(p, q) == 1 && BigInt(q).isProbablePrime(7)
    }

    // Build a stream of factors from (2*p+1) step-by (2*p)
    def s(a: Long): Stream[Long] = a #:: s(a + (2 * p)) // Build stream of possible factors

    // Limit and Filter Stream and then take the head element
    val e = s(2 * p + 1).takeWhile(_ < limit).filter(factorTest(p, _))
    e.headOption
  }

  // Test
  (primes takeWhile (_ <= 97)) ++ List(929, 937) foreach { p =>
    { // Needs some intermediate results for nice formatting
      val nMersenne = mersenne(p); val lit = f"${nMersenne}%30d"
      val preAmble = f"${s"M${p}"}%4s = 2^$p%03d - 1 = ${lit}%s"

      val datum = System.nanoTime
      val result = factorMersenne(p)
      val mSec = ((System.nanoTime - datum) / 1.e+6).round

      def decStr = { if (lit.length > 30) f"(M has ${lit.length}%3d dec)" else "" }
      def sPrime = { if (result.isEmpty) " is a Mersenne prime number." else " " * 28 }

      println(f"$preAmble${sPrime} ${f"($mSec%,1d"}%13s msec)")
      if (!result.isEmpty)
        println(f"${decStr}%-17s = ${result.get} × ${nMersenne / result.get}")
    }
  }
}
```

<pre style="height:40ex;overflow:scroll">  M2 = 2^002 - 1 =                              3 is a Mersenne prime number.           (63 msec)
  M3 = 2^003 - 1 =                              7 is a Mersenne prime number.            (0 msec)
  M5 = 2^005 - 1 =                             31 is a Mersenne prime number.            (1 msec)
  M7 = 2^007 - 1 =                            127 is a Mersenne prime number.            (2 msec)
 M11 = 2^011 - 1 =                           2047                                    (2.097 msec)
                  = 23 × 89
 M13 = 2^013 - 1 =                           8191 is a Mersenne prime number.           (33 msec)
 M17 = 2^017 - 1 =                         131071 is a Mersenne prime number.          (254 msec)
 M19 = 2^019 - 1 =                         524287 is a Mersenne prime number.          (524 msec)
 M23 = 2^023 - 1 =                        8388607                                        (0 msec)
                  = 47 × 178481
 M29 = 2^029 - 1 =                      536870911                                        (0 msec)
                  = 233 × 2304167
 M31 = 2^031 - 1 =                     2147483647 is a Mersenne prime number.       (31.484 msec)
 M37 = 2^037 - 1 =                   137438953471                                        (0 msec)
                  = 223 × 616318177
 M41 = 2^041 - 1 =                  2199023255551                                        (0 msec)
                  = 13367 × 164511353
 M43 = 2^043 - 1 =                  8796093022207                                        (0 msec)
                  = 431 × 20408568497
 M47 = 2^047 - 1 =                140737488355327                                        (0 msec)
                  = 2351 × 59862819377
 M53 = 2^053 - 1 =               9007199254740991                                        (0 msec)
                  = 6361 × 1416003655831
 M59 = 2^059 - 1 =             576460752303423487                                        (1 msec)
                  = 179951 × 3203431780337
 M61 = 2^061 - 1 =            2305843009213693951 is a Mersenne prime number.       (16.756 msec)
 M67 = 2^067 - 1 =          147573952589676412927                                    (1.435 msec)
                  = 193707721 × 761838257287
 M71 = 2^071 - 1 =         2361183241434822606847                                        (2 msec)
                  = 228479 × 10334355636337793
 M73 = 2^073 - 1 =         9444732965739290427391                                        (0 msec)
                  = 439 × 21514198099633918969
 M79 = 2^079 - 1 =       604462909807314587353087                                        (0 msec)
                  = 2687 × 224958284260258499201
 M83 = 2^083 - 1 =      9671406556917033397649407                                        (0 msec)
                  = 167 × 57912614113275649087721
 M89 = 2^089 - 1 =    618970019642690137449562111 is a Mersenne prime number.       (11.097 msec)
 M97 = 2^097 - 1 = 158456325028528675187087900671                                        (0 msec)
                  = 11447 × 13842607235828485645766393
M929 = 2^929 - 1 = 4538015467766671944574165338592225830478699345884382504442663144885072806275648112625635725391102144133907238129251016389326737199538896813326509341743147661691195191795226666084858428449394948944821764472508048114220424520501343042471615418544488778723282182172070046459244838911                                        (0 msec)
(M has 280 dec)   = 13007 × 348890248924938259750454781163390930305120269538278042934009621348894657205785201247454118966026150852149399410259938217062100192168747352450719561908445272675574320888385228421992652298715687625495638077382028762529439880103124705348782610789919949159935587158612289264184273
M937 = 2^937 - 1 = 1161731959748268017810986326679609812602547032546401921137321765090578638406565916832162745700122148898280252961088260195667644723081957584211586391486245801392945969099578026517723757683045106929874371704962060317240428677248343818872733547147389127353160238636049931893566678761471                                        (0 msec)
(M has 283 dec)   = 28111 × 41326596696960905617409068573854000661753300577937530544531385048222355604801178073784737138491058621119143856891902109340387916583613446131819799775399160520541637405271175928203328152077304504637841830776637626453716647477796727931156257235508844486256634009321971181870679761

```



## Scheme


This works with PLT Scheme, other implementations only need to change the inclusion.


```scheme

#lang scheme

;;; this needs to be changed for other R6RS implementations
(require rnrs/arithmetic/bitwise-6)

;;; modpow, as per the task description.
(define (modpow exponent base)
  (let loop ([square 1] [index (- (bitwise-length exponent) 1)])
    (if (< index 0)
        square
        (loop (modulo (* (if (bitwise-bit-set? exponent index) 2 1)
                      square square) base)
              (- index 1)))))

;;; search through all integers from 1 on to find the first divisor
;;; returns #f if 2^p-1 is prime
(define (mersenne-factor p)
  (for/first ((i (in-range 1 (floor (expt 2 (quotient p 2))) (* 2 p)))
              #:when (and (or (= 1 (modulo i 8)) (= 7 (modulo i 8)))
                          (= 1 (modpow p i))))
    i))

```

```txt

> (mersenne-factor 929)
13007
> (mersenne-factor 23)
47
> (mersenne-factor 3)
#f

```



## Seed7


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

const func integer: modPow (in var integer: base,
    in var integer: exponent, in integer: modulus) is func
  result
    var integer: power is 1;
  begin
    if exponent < 0 or modulus < 0 then
      raise RANGE_ERROR;
    else
      while exponent > 0 do
        if odd(exponent) then
          power := (power * base) mod modulus;
        end if;
        exponent >>:= 1;
        base := base ** 2 mod modulus;
      end while;
    end if;
  end func;

const func integer: mersenneFactor (in integer: exponent) is func
  result
    var integer: factor is 0;
  local
    var integer: maxk is 0;
    var integer: k is 1;
    var boolean: searching is TRUE;
  begin
    maxk := 16384 div exponent; # Limit for k to prevent overflow of 32 bit signed integer
    while k <= maxk and searching do
      factor := 2 * exponent * k + 1;
      if (factor mod 8 = 1 or factor mod 8 = 7) and
          isPrime(factor) and modPow(2, exponent, factor) = 1 then
        searching := FALSE;
      end if;
      incr(k);
    end while;
    if searching then
      factor := 0;
    end if;
  end func;

const proc: main is func
  begin
    writeln("Factor of M929: " <& mersenneFactor(929));
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#isPrime isPrime],
[http://seed7.sourceforge.net/algorith/math.htm#modPow modPow] (modified to use integer instead of bigInteger).

```txt

Factor of M929: 13007

```



## Sidef


```ruby
func mtest(b, p) {
    var bits = b.base(2).digits
    for (var sq = 1; bits; sq %= p) {
        sq *= sq
        sq += sq if bits.shift==1
    }
    sq == 1
}

for m (2..60 -> grep{ .is_prime }, 929) {
    var f = 0
    var x = (2**m - 1)
    var q
    { |k|
        q = (2*k*m + 1)
        q%8 ~~ [1,7] || q.is_prime || next
        q*q > x || (f = mtest(m, q)) && break
    } << 1..Inf
    say (f ? "M#{m} is composite with factor #{q}"
           : "M#{m} is prime")
}
```

```txt

M2 is prime
M3 is prime
M5 is prime
M7 is prime
M11 is composite with factor 23
M13 is prime
M17 is prime
M19 is prime
M23 is composite with factor 47
M29 is composite with factor 233
M31 is prime
M37 is composite with factor 223
M41 is composite with factor 13367
M43 is composite with factor 431
M47 is composite with factor 2351
M53 is composite with factor 6361
M59 is composite with factor 179951
M929 is composite with factor 13007

```



## Tcl

For <code>primes::is_prime</code> see [[Prime decomposition#Tcl]]

```tcl
proc int2bits {n} {
    binary scan [binary format I1 $n] B* binstring
    return [split [string trimleft $binstring 0] ""]

    # another method
    if {$n == 0} {return 0}
    set bits [list]
    while {$n > 0} {
        lappend bits [expr {$n % 2}]
        set n [expr {$n / 2}]
    }
    return [lreverse $bits]
}

proc trial_factor {base exp mod} {
    set square 1
    foreach bit [int2bits $exp] {
        set square [expr {($square ** 2) * ($bit == 1 ? $base : 1) % $mod}]
    }
    return [expr {$square == 1}]
}

proc m_factor p {
    set limit [expr {sqrt(2**$p - 1)}]
    for {set k 1} {2 * $k * $p - 1 < $limit} {incr k} {
        set q [expr {2 * $k * $p + 1}]
        if { ! [primes::is_prime $q]} {
            continue
        } elseif { ! ($q % 8 == 1 || $q % 8 == 7)} {
            # optimization
            continue
        } elseif {[trial_factor 2 $p $q]} {
            # $q is a factor of 2**$p-1
            return $q
        }
    }
    return -1
}

set exp 929
if {[set fact [m_factor 929]] > 0} {
    puts "M$exp has a factor: $fact"
} else {
    puts "no factor found for M$exp"
}
```


=={{header|TI-83 BASIC}}==
The program uses the new remainder function from OS 2.53MP, if not available it can be replaced by:

```ti83b
remainder(A,B)   equivalent to   iPart(B*fPart(A/B))
```
Due to several problems, no Goto has been used. As a matter of fact the version is clearer.

```ti83b
Prompt Q
1→K:0→T
While K≤2^20 and T=0
2KQ+1→P
remainder(P,8)→W
If W=1 or W=7
Then
0→E:0→M
If remainder(P,2)=0:1→M
If remainder(P,3)=0:1→M
5→D
While M=0 and DD≤P
If remainder(P,D)=0:1→M
D+2→D
If remainder(P,D)=0:1→M
D+4→D
End
If M=0:1→E
Q→I:1→Y:2→Z
While I≠0
If remainder(I,2)=1:remainder(YZ,P)→Y
remainder(ZZ,P)→Z
iPart(I/2)→I
End
If E=1 and Y=1
Then
P→F:1→T
End
End
K+1→K
End
If T=0:0→F
Disp Q,F
```

```txt

Q=?929

```

```txt

             929
           13007
            Done

```



## uBasic/4tH

<lang>Print "A factor of M929 is "; FUNC(_FNmersenne_factor(929))
Print "A factor of M937 is "; FUNC(_FNmersenne_factor(937))

End

_FNmersenne_factor Param(1)
  Local(2)

  If (FUNC(_FNisprime(a@)) = 0) Then Return (-1)

  For b@ = 1 TO 99999
    c@ = (2*a@*b@) + 1
    If (FUNC(_FNisprime(c@))) Then
      If (AND (c@, 7) = 1) + (AND (c@, 7) = 7) Then
        Until FUNC(_ModPow2 (a@, c@)) = 1
      EndIf
    EndIf
  Next

Return (c@ * (b@<100000))


_FNisprime Param(1)
  Local (1)

  If ((a@ % 2) = 0) Then Return (a@ = 2)
  If ((a@ % 3) = 0) Then Return (a@ = 3)

  b@ = 5

  Do Until ((b@ * b@) > a@) + ((a@ % b@) = 0)
    b@ = b@ + 2
  Until (a@ % b@) = 0
    b@ = b@ + 4
  Loop

Return ((b@ * b@) > a@)


_ModPow2 Param(2)
  Local(2)

  d@ = 1
  For c@ = 30 To 0 Step -1
    If ((a@+1) > SHL(1,c@)) Then
       d@ = d@ * d@
       If AND (a@, SHL(1,c@)) Then
          d@ = d@ * 2
       EndIf
       d@ = d@ % b@
    EndIf
  Next

Return (d@)
```

```txt
A factor of M929 is 13007
A factor of M937 is 28111

0 OK, 0:123
```



## VBScript

```vb
' Factors of a Mersenne number
    for i=1 to 59
        z=i
        if z=59 then z=929  ':) 61 turns into 929.
        if isPrime(z) then
            r=testM(z)
            zz=left("M" & z & space(4),4)
            if r=0 then
                Wscript.echo zz & " prime."
            else
                Wscript.echo zz & " not prime, a factor: " & r
            end if
        end if
    next

function modPow(base,n,div)
    dim i,y,z
    i = n : y = 1 : z = base
    do while i
        if i and 1 then y = (y * z) mod div
        z = (z * z) mod div
        i = i \ 2
    loop
    modPow= y
end function

function isPrime(x)
    dim i
    if x=2 or x=3 or _
       x=5 or x=7 _
                  then isPrime=1: exit function
    if x<11       then isPrime=0: exit function
    if x mod 2=0  then isPrime=0: exit function
    if x mod 3=0  then isPrime=0: exit function
    i=5
    do
        if (x mod i)     =0 or _
           (x mod (i+2)) =0 _
                  then isPrime=0: exit function
        if i*i>x  then isPrime=1: exit function
        i=i+6
    loop
end function

function testM(x)
    dim sqroot,k,q
    sqroot=Sqr(2^x)
    k=1
    do
        q=2*k*x+1
        if q>sqroot then exit do
        if (q and 7)=1 or (q and 7)=7 then
            if isPrime(q) then
                if modPow(2,x,q)=1 then
                    testM=q
                    exit function
                end if
            end if
        end if
        k=k+1
    loop
    testM=0
end function
```

```txt

M2   prime.
M3   prime.
M5   prime.
M7   prime.
M11  not prime, a factor: 23
M13  prime.
M17  prime.
M19  prime.
M23  not prime, a factor: 47
M29  not prime, a factor: 233
M31  prime.
M37  not prime, a factor: 223
M41  not prime, a factor: 13367
M43  not prime, a factor: 431
M47  not prime, a factor: 2351
M53  not prime, a factor: 6361
M929 not prime, a factor: 13007

```



## Visual Basic

```vb
Sub mersenne()
    Dim q As Long, k As Long, p As Long, d As Long
    Dim factor As Long, i As Long, y As Long, z As Long
    Dim prime As Boolean
    q = 929   'input value
    For k = 1 To 1048576   '2**20
        p = 2 * k * q + 1
        If (p And 7) = 1 Or (p And 7) = 7 Then    'p=*001 or p=*111
            'p is prime?
            prime = False
            If p Mod 2 = 0 Then GoTo notprime
            If p Mod 3 = 0 Then GoTo notprime
            d = 5
            Do While d * d <= p
                If p Mod d = 0 Then GoTo notprime
                d = d + 2
                If p Mod d = 0 Then GoTo notprime
                d = d + 4
            Loop
            prime = True
        notprime:   'modpow
            i = q: y = 1: z = 2
            Do While i   'i <> 0
                On Error GoTo okfactor
                If i And 1 Then y = (y * z) Mod p  'test first bit
                z = (z * z) Mod p
                On Error GoTo 0
                i = i \ 2
            Loop
            If prime And y = 1 Then factor = p: GoTo okfactor
        End If
    Next k
    factor = 0
okfactor:
    Debug.Print "M" & q, "factor=" & factor
End Sub
```

```txt

M47           factor=2351

```



## zkl

```zkl
var [const] BN=Import("zklBigNum");  // libGMP

    // M = 2^P - 1 , P prime
    // Look for a prime divisor q such as:
    //     q < M.sqrt(), q = 1 or 7 modulo 8, q = 1 + 2kP
    // q is divisor if 2.powmod(P,q) == 1
    // m-divisor returns q or False
fcn m_divisor(P){
   // must limit the search as M.sqrt() may be HUGE and I'm slow
   maxPrime:='wrap{ BN(2).pow(P).sqrt().min(0d5_000_000) };
   t,b2:=BN(0),BN(2);  // so I can do some in place BigInt math
   foreach q in (maxPrime(P*2)){ // 0..maxPrime -1, faster than just odd #s
      if((q%8==1 or q%8==7) and t.set(q).probablyPrime() and
	 b2.powm(P,q)==1) return(q);
   }
   False
}
```


```zkl
m_divisor(929).println();	// 13007
m_divisor(4423).println();	// False
(BN(2).pow(4423) - 1).probablyPrime().println();  // True
```

```txt

13007
False
True

```

