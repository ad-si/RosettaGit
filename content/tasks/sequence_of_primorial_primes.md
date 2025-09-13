+++
title = "Sequence of primorial primes"
description = ""
date = 2019-08-01T21:09:00Z
aliases = []
[extra]
id = 19255
[taxonomies]
categories = ["task"]
tags = []
+++

The sequence of primorial primes is given as the increasing values of n where [[Primorial numbers|primorial]](n) ± 1 is prime.

Noting that the n'th primorial is defined as the multiplication of the smallest n primes, the sequence is of the number of primes, in order that when multiplied together is one-off being a prime number itself.


## Task

Generate and show here the first ten values of the sequence. 


;Optional extended task:
Show the first twenty members of the series.


;Notes: 
* This task asks for the primorial ''indices'' that create the final primorial prime numbers, so there should be no ten-or-more digit numbers in the program output (although extended precision integers ''will'' be needed for intermediate results).
* There is some confusion in the references, but for the purposes of this task the '''[[wp:Primorial prime|sequence]] begins with n = 1'''.
* Probabilistic primality tests are allowed, as long as they are good enough that output shown is correct.


;Cf.
* [[wp:Primorial prime|Primorial prime]] Wikipedia.
* [https://primes.utm.edu/glossary/page.php?sort=PrimorialPrime Primorial prime] from The Prime Glossary.
* [https://oeis.org/A088411 Sequence A088411] from The On-Line Encyclopedia of Integer Sequences


## Related tasks

* [[Primorial numbers]]





## C

```c

#include <gmp.h>

int main(void)
{
    mpz_t p, s;
    mpz_init_set_ui(p, 1);
    mpz_init_set_ui(s, 1);

    for (int n = 1, i = 0; i < 20; n++) {
        mpz_nextprime(s, s);
        mpz_mul(p, p, s);

        mpz_add_ui(p, p, 1);
        if (mpz_probab_prime_p(p, 25)) {
            mpz_sub_ui(p, p, 1);
            gmp_printf("%d\n", n);
            i++;
            continue;
        }

        mpz_sub_ui(p, p, 2);
        if (mpz_probab_prime_p(p, 25)) {
            mpz_add_ui(p, p, 1);
            gmp_printf("%d\n", n);
            i++;
            continue;
        }

        mpz_add_ui(p, p, 1);
    }

    mpz_clear(s);
    mpz_clear(p);
}

```

```txt

1
2
3
4
5
6
11
13
24
66
68
75
167
171
172
287
310
352
384
457

```



## Clojure

; Uses Java Interoop to use Java for 1) Prime Test and 2) Prime sequence generation

```lisp

(ns example
  (:gen-class))

; Lazy Sequence of primes (starting with number 2)
(def primes (iterate #(.nextProbablePrime %) (biginteger 2)))

(defn primorial-prime? [v]
  " Test if value is a primorial prime "
  (let [a (biginteger (inc v))
        b (biginteger (dec v))]
    (or (.isProbablePrime a 16)
      (.isProbablePrime b 16))))

; Generate indexes for first 20 primorial primes
(println (take 20 (keep-indexed                                 ; take the first 20
                          #(if (primorial-prime? %2) (inc %1))  ; filters out non-primorials, passing on the index + 1 (since sequence begins with 1 (not 0)
                          (reductions *' primes))))             ; computes the lazy sequence of product of 1 prime, 2 primes, 3 primes, etc.


```

```txt

(1 2 3 4 5 6 11 13 24 66 68 75 167 171 172 287 310 352 384 457)

```


## EchoLisp


```lisp

(lib 'timer) ;; for (every (proc t) interval)
(lib 'bigint)

;; memoize primorial
(define p1000 (cons 1 (primes 1000))) ; remember first 1000 primes
(define (primorial n) 
    (if(zero? n) 1 
    (* (list-ref p1000 n) (primorial (1- n)))))
(remember 'primorial)

(define N 0)

;; search one at a time
(define (search t) ;; time parameter, set by (every),  not used
(set! N (1+ N)) 
(if (or (prime? (1+ (primorial N))) (prime? (1- (primorial N)))) 
	(writeln 'HIT N )
 	(writeln N (date->time-string (current-date )))))

```

```lisp

;; run in batch mode to make the browser happy
;; search next N every 2000 msec
(every 2000 search)
    →
    HIT     1    
    HIT     2    
    HIT     3    
    HIT     4    
    HIT     5    
    HIT     6    
    7     "13:47:03"    
    HIT     11    
    HIT     13    
    HIT     24      
    HIT     66     
    HIT     68    
    HIT     75    
    HIT     167    
    HIT     171    
    HIT     172    
    HIT     287    
    HIT     310    
    HIT     352    
    HIT     384      
    455     "14:07:08"    
    456     "14:07:12"    
    HIT     457    
    458     "14:07:25"    
    459     "14:07:29" 

```



## Factor


```factor
USING: kernel lists lists.lazy math math.primes prettyprint
sequences ;

: pprime? ( n -- ? )
    nprimes product [ 1 + ] [ 1 - ] bi [ prime? ] either? ;

10 1 lfrom [ pprime? ] <lazy-filter> ltake list>array .
```

```txt

{ 1 2 3 4 5 6 11 13 24 66 }

```



## Fortran


### Stage one: a probe

First comes a probe of the problem, using ordinary arithmetic and some routines written for other problems. Module PRIMEBAG can be found in [[Extensible_prime_generator#Fortran]] and the basic version of module BIGNUMBERS in [[Primorial_numbers#Fortran]] 
```Fortran
      PROGRAM PRIMORIALP	!Simple enough, with some assistants.
      USE PRIMEBAG		!Some prime numbers are wanted.
      USE BIGNUMBERS		!Just so.
      TYPE(BIGNUM) B		!I'll have one.
      INTEGER MAXF		!Largest factor to consider by direct division.
      PARAMETER (MAXF = 18000000)	!Some determination.
      INTEGER I			!Step stuff.
      INTEGER FU,FD		!Found factors.
      INTEGER NHIT,HIT(666)	!A little list.
      CHARACTER*4 WOT		!A remark.
      CHARACTER*66 ALINE	!A scratchpad.
      REAL T0,T1		!In memory of lost time.
      MSG = 6	!Standard output.
      WRITE (MSG,1) BIGLIMIT,BIGBASE,HUGE(I)	!Announce.
    1 FORMAT ('Calculates primorial "primes"',/,
     1 "A primorial prime is a value N such that",/,
     2 "    Primorial(N) - 1 is prime, OR",/,
     3 "    Primorial(N) + 1 is prime, or both.",/,
     4 "and Primorial(N) is the product of the first N prime numbers.",/
     5 "Working with up to ",I0," digits in base ",I0,"."/
     6 "The integer limit is ",I0,/)

c      CALL PREPARE PRIMES	!First, catch your rabbit. Via ERATOSTHENES.
      IF (.NOT.GRASPPRIMEBAG(66)) STOP "Gan't grab my file!"	!Attempt in hope.
      WRITE (MSG,2)
    2 FORMAT ("Primorial#",3X,"Approx.",8X," -1 Factor +1 Factor Hit")

Commence prime mashing.
  100 NHIT = 0		!My list is empty.
      B.LAST = 1	!Begin at the beginning.
      B.DIGIT(1) = 1	!With one. The same, whatever BIGBASE.
      CALL CPU_TIME(T0)	!Start the timing.
      DO I = 1,30	!69	!Step along the primorials.
        CALL BIGMULTN(B,PRIME(I))	!Multiply by the next prime.
c        WRITE (MSG,101) I,PRIME(I),I,B.DIGIT(B.LAST:1:-1)	!Digits in Arabic/Hindu order.
  101   FORMAT ("Prime(",I0,") = ",I0,", Primorial(",I0,") = ",	!For a possibly multi-BIGBASE sequence.
     1   I0,9I<BIGORDER>.<BIGORDER>,/,(10I<BIGORDER>.<BIGORDER>))	!The first without leading zero digits.
        FU = -1		!No factor for up one.
        FD = -1		!No factor for down one.
        CALL BIGADDN(B,+1)	!Go up one.
        FU = BIGFACTOR(B,MAXF)	!Find a factor, maybe.
        CALL BIGADDN(B,-2)	!Now test down one.
        IF (FU.NE.1) FD = BIGFACTOR(B,MAXF)	!But only if FU didn't report "prime".
        IF (FU.EQ.1 .OR. FD.EQ.1) THEN	!Since if either candidate is a prime,
          WOT = "Yes!"				!Then a hit.
          NHIT = NHIT + 1			!So count up a success.
          HIT(NHIT) = I				!And append to my list.
        ELSE IF (FU.GT.1 .AND. FD.GT.1) THEN	!But if both have factors,
          WOT = "No."				!Then definitely not a hit.
        ELSE				!Otherwise,
          WOT = "?"				!I can't decide.
        END IF				!So much for that candidate.
        CALL BIGADDN(B,1)		!Recover the original primorial value.
        WRITE (ALINE,102) I,BIGVALUE(B),FD,FU,WOT	!Prepare a report.
  102   FORMAT (I10,1PE18.10,I10,I10,1X,A)	!A table.
        IF (FD.EQ.-1) ALINE(37:38) = ""		!Wasn't looked for, so no remark.
        IF (FD.EQ. 0) ALINE(38:38) = "?"	!Recode a zero.
        IF (FU.EQ. 0) ALINE(48:48) = "?"	!Since it represents "Don't know".
        WRITE (MSG,"(A)") ALINE		!Show the report.
      END DO		!On to the next prime.
      CALL CPU_TIME(T1)	!Completed the run.

Cast forth some pearls.
      WRITE (MSG,201) HIT(1:NHIT)	!The list.
  201 FORMAT (/,"Hit list: ",I0,666(",",I0:))	!Don't actually expect so many.
      WRITE (MSG,*) "CPU time:",T1 - T0	!The cost.
      END	!So much for that.

```

Output, somewhat edited, because the limit on direct factorisation is soon exceeded: five lines such as "Passed the limit of 18000000 with 18000041, but Sqrt(n) =  0.43849291E+11:" and similar have been removed, corresponding to the five "?" reports.

```txt

Calculates primorial "primes"
A primorial prime is a value N such that
    Primorial(N) - 1 is prime, OR
    Primorial(N) + 1 is prime, or both.
and Primorial(N) is the product of the first N prime numbers.
Working with up to 8888 digits in base 10.
The integer limit is 2147483647

Primorial#   Approx.         -1 Factor +1 Factor Hit
         1  2.0000000000E+00                   1 Yes!
         2  6.0000000000E+00                   1 Yes!
         3  3.0000000000E+01                   1 Yes!
         4  2.1000000000E+02                   1 Yes!
         5  2.3100000000E+03                   1 Yes!
         6  3.0030000000E+04         1        59 Yes!
         7  5.1051000000E+05        61        19 No.
         8  9.6996900000E+06        53       347 No.
         9  2.2309287000E+08        37       317 No.
        10  6.4696932300E+09        79       331 No.
        11  2.0056049013E+11                   1 Yes!
        12  7.4207381348E+12       229       181 No.
        13  3.0425026353E+14         1        61 Yes!
        14  1.3082761332E+16    141269       167 No.
        15  6.1488978259E+17       191       953 No.
        16  3.2589158477E+19     87337        73 No.
        17  1.9227603502E+21         ?       277 ?
        18  1.1728838136E+23      1193       223 No.
        19  7.8583215511E+24       163         ? ?
        20  5.5794083013E+26         ?      1063 ?
        21  4.0729680599E+28       313      2521 No.
        22  3.2176447673E+30       163     22093 No.
        23  2.6706451569E+32       139    265739 No.
        24  2.3768741896E+34         ?       131 ?
        25  2.3055679639E+36     66683   2336993 No.
        26  2.3286236436E+38         ?    960703 ?
        27  2.3984823529E+40     15649      2297 No.
        28  2.5663761176E+42  17515703       149 No.
        29  2.7973499682E+44       719    334507 No.
        30  3.1610054640E+46    295201   5122427 No.

Hit list: 1,2,3,4,5,6,11,13
 CPU time:   2.968750

```

The direct factorisation assault was extended up to 18,000,000 because one factor was 17,515,703 and it was pleasing to have a definite result. However there is not much hope for this approach because the 32-bit integer limit is exceeded after Primorial(9) and the 64-bit limit after Primorial(15), so definitive factorisation in a reasonable time is out of reach. Already, bignumber arithmetic has been introduced to calculate this far, and only the first eight candidates have been found when the demand is for at least ten.


### Stage two: the plan

Although there would be no difficulty in principle in a Fortran compiler accepting syntax such as INTEGER*1000, this sort of ability is not required in the modern standard. Fortran II of 1957 on a decimal computer such as the IBM1620 allowed a control card to state how many digits were to be used for fixed-point numbers (i.e. integers) but although the hardware delivered integer arithmetic of arbitrary precision (up to memory limits), thousand-digit arithmetic probably was out of bounds, just as INTEGER*8 or perhaps INTEGER*16 is the upper limit for modern compilers. Thus, there is no "built-in" facility for arithmetic involving larger numbers available via a standard specification. So, one must devise appropriate routines.


### =Functions or Subroutines?=

Rather than always employing in-line arithmetic expressions of ever greater complexity, defining a function offers multiple advantages. This might be via an "arithmetic statement" function if it can be written as a single expression, but more generally, a function can be named and defined as a separate unit for compilation, and then used within arithmetic expressions. This is unexceptional, and easily understood. Whatever the parameters and workings of the function, its result is returned in the computer's accumulator, or top-of-stack, or a standard register. Thus, an expression such as ''y:=x;'' would produce code such as Load x; Store y; whereas with an expression such as ''y:=F(x);'' instead of the Load there is an invocation of the function which would perform its calculation then return, whereupon the code Store y; would be executed just as before. This is easily extended for usage within an expression such as ''y:=y + 7*F(x) - 3;'' and so forth.

Within the function's definition would be a statement such as <code>F = ''expression''</code> followed by a return (perhaps by reaching the end of the function's code) but more complex functions introduce additional considerations. It is often the case that the final result will be developed via multiple statements and conditional branches. For instance, 
```Fortran
      FUNCTION SUM(A,N)
       DIMENSION A(*)
        SUM = 0       !Unconditionally, clear SUM.
        DO I = 1,N    !There may be less than one element to sum.
          SUM = A(I) + SUM
        END DO        !Some early compilers executed a DO-loop at least once.
      END
```

Prior to F90, there were compilers for which this did ''not'' work. If an assignment to SUM were to be regarded as an assignment to the accumulator, as above, then when that is ''not'' followed by a RETURN but by other statements, the value that had been saved in the accumulator will be lost by the accumulator's usage in the execution of the subsequent statements. Indeed, some compilers would not allow any usage of SUM within an expression. Thus, a local variable, perhaps called ASUM, would be defined and used instead, and then, before exiting the function (by whatever path) one must remember to have an assignment <code>SUM = ASUM</code>

This difficulty no longer exists, and whether or not a local variable is used, it is possible that the compiler will generate code that avoids the unnecessary shuffling of data from one location to another. Or perhaps not. With the introduction of CHARACTER*n variables, the amount of data being copied about can now be much larger than an accumulator or a data "word" or two. Consider an expression such as <code>TEXT = UPPERCASE(TEXT)</code> It would be pleasant to think that the function's work area would be the storage belonging to the recipient of the result of the function so that no copying from place to place need delay the computer's speedy completion of its assigned task...

Or, one could define SUBROUTINE UPPERCASE(TEXT) that produces its result in-place, and deal patiently with the requirement to convert a simple expression into a suitable sequence of subroutine calls.


### =Long multiplication=

The method taught in primary school is adequate, but variations are possible. For instance, instead of writing out the multi-line scheme of partial products then adding them in a second pass to produce the final product, the terms can be added as they are produced by working down a column rather than across a row - see the tableau in BIGMULT. This however requires an accumulator able to hold not just a two-digit number, but the sum of many such numbers given that the numbers have many digits. If calculations are proceeding with a base close to or equal to the word size of the computer's arithmetic, this becomes less convenient because then carries must be propagated to prevent overflow while working down a column rather than once after the column sum is complete. The expectation here is that the base will be some power of ten, and on a computer using binary arithmetic the word size will not be fully used.

More interesting is the opportunity of working left-to-right rather than the usual right-to-left, which is to say that the digits of the product will be produced from the highest order down, except for the propagation of carries leftwards to higher order digits. This in turn means that when multiplying A by B with the result to appear in A, the product can be placed in A as it is developed, rather than being copied from some work area, and it also allows BIGMULT(A,A) to calculate A² - though a dedicated routine BIGSQUARE(A) could employ its own trickery.


### =Long division=

The straightforward way to calculate B mod M when a MOD operation is unavailable is to calculate <code>B - B/M*M</code> (hoping that an enthusiastic compiler won't convert .../M*M to a no-operation) where the division is integer division that truncates any remainder. Some languages such as pl/i retain the fractional part (producing a floating-point number), but division of integers in Fortran produces an integer result via truncation. Directly translating this formula requires a divide, a multiply, and a subtract, so one instead turns to consider variations on division.


### ==Proffesor Knuth==

In ''The Art of Comuter Programming'', volume 2 ''Seminumerical Algorithims'' Professor Knuth remarks "Here the ordinary pencil-and-paper method involves a certain amount of guesswork and ingenuity on the part of the person doing the division; we must either eliminate this guesswork from the algorithm or develop some theory to explain it more carefully."

As will be remembered from primary school, at each step the key lies in the choice of a value for Q, the trial quotient for subtracting Q*M from the working number, with the digits of Q shifted left to align with the current high-order digits of B. One makes an estimate from considering the leading digits of B and M, and annoyingly, misguesses: the paper soon becomes decorated with side calculations of 3*Q, 7*Q, ''etc.'' to avoid repeated mistakes. In base ten there can be only ten possible values for Q at each cycle, but in larger bases any such list becomes ridiculously long and anyway, the chance of a recurrence of a value for Q falls.

Professor Knuth remarks that most computer arithmetic units allow for a double-word value to be divided by a single-word value with the hardware typically employing a register pair, and after the division one register holds the result and the other the remainder. Unfortunately, high-level languages typically provide no access to this facility and so code for bignumber arithmetic is littered by the likes of ''Digit:=DD mod Base; Carry:=DD div Base;'' - two divisions where one should suffice. If one allowed ''mod'' to be represented by \ and ''div'' by /, it would be nice to have syntax something like Digit,Carry:=DD/\Base;''

The plan then is to guess a value for Q by inspecting the first two digits of B and the first only of M by calculating <code>DD = B.Digit(B.Last)*BigBase + B.Digit(B.Last - 1)</code> - this is a double-digit value, where B.Last fingers the highest-order digit of B and thus (B.Last - 1) the second digit. Then calculate <code>Q = DD/M.Digit(M.Last)</code> Immediate difficulties arise: in base ten this might be 31/2 and thus Q is a gross over-estimate. The estimate can be restrained: <code>Q = MIN(DD/M.Digit(M.Last),BigBase - 1)</code> but this is not the only problem.

How good is this estimate? Not very. The difficulty is that M is approximated by a single digit only, and when that digit is small, its relative accuracy is low. A lead digit of one would represent M = 1000... and also M = 1999... which involves almost a factor of two, thus the accuracy of DD/M.Digit(M.Last) varies by a factor of two likewise. The accuracy for higher digits is much better, to the degree that Professor Knuth recommends multiplying both B and M by a factor F such that the lead digit of the revised M is as large as possible: <code>F = BigBase/(M.Digit(M.Last) + 1)</code>, but even so, Q may still be over-estimated, though now only by one and sometimes by two. Further tests are required to reduce the frequency of one-too-many, and preclude two-too-many.

Equipped with a good estimate for Q, the next step is to subtract Q*M from the high-order digits of B that align with the digits of M. Because now the full value of M is employed, the result can be negative: overshoot. If so, add back one M to produce the reduced value of B, ready for the next attempt with M shifted over to again align with the high-order digits of B.


### ==Not followed...==

Although the intention of BIGMOD(B,M) is for the value of B to hold the result, changing the value of M is likely to be unexpected, even if it is later changed back to what it had been. M might even be a constant, protected from change. Similarly, the rescale for B may require another digit and storage for this may not be available. Copies to working variables could be made, but anyway, although small compared to the effort of BIGMOD, the rescaling of big numbers is not so trivial. Instead, recognising that in high-level languages there is no access to to the double/single features of the hardware mentioned above, one can rely on the availability of equal-sized division - that is, one word divided by one word, or double by double. Thus, there is the opportunity for DD to be the first two digits of B and MM the first two of M, and then <code>Q = DD/MM</code> The minimum accuracy of this calculation is always better than one digit since (in base ten) MM = 10 represents 100... to 109... and so no tricky adjustment checks are required. There is still the overflow issue with the required add back when using the full precision of M, but the overflow is always only by one extra M value and so the shift and add ploy discussed below is safe.

The intention is to calculate B ''mod'' M by subtracting from B various multiples of M until the result no longer exceeds M; this is the required remainder. This will be done by following the method of long division except without bothering to record the quotient, although in other use producing both the quotient and the remainder in one go may be helpful. To this end, BIGMOD(B,M) first checks for a single-digit M and for that case, invokes BIGMODN(B,M.Digit(1)): any further proceedings can be assured of at least two digits in M so that MM may be produced. Iteration proceeds to reduce B while B > M. Subroutine BIGNORM ensures that any leading zero digits produced (possibly followed by existing zero digits) will be trimmed off ready for BIGSIGN to compare the new B against M.


### ===Consequences===

Alas, DD/MM sometimes yields zero, as in 1001/11 (where DD/M.Digit(M.Last) would yield 10, reduced by the MIN to 9) or 9800/99, or 4100/588, and many other cases. One could pre-emptively subtract M once and then engage in the overflow recovery procedure, or, shift M right one digit place and reconsider. This means calculating <code>Q = (DD*BIGBASE + B.DIGIT(L - 1))/MM</code> and proceeding with that, again at a risk of going one too far when using M instead of its approximation MM. Even if the first two digits of M exceed the first two digits of B, this shift is always possible because B > M is assured via the earlier test. However, this recalculation involves a ''three''-digit value (DD is already a two-digit value) and until now the working assumption has been for arithmetic of up to ''two''-digits only. This means the size of the big base is limited. The difficulty is that the proper value for Q is anything from zero to BIGBASE - 1, and with division by a two-digit number MM, for the higher values of Q to emerge, DD must have a three-digit number. This means that calculating a provisional Q value (from DD divided by the lead digit of M) then considering Q*MM - (top three digits of B) to determine an improved estimate will still have trouble. A multi-digit divide isn't available (as via recursion) since this ''is'' the multi-digit divide scheme and it is in trouble. Still further possibilities open up by considering the reciprocal of MM (calculated once at the start) so that DD/MM can be calculated using multiplication, but enough: accept a severe limitation on the possible base of arithmetic. For various powers of ten, when using two's complement signed integers,

 Word size:            16-bit    32-bit    64-bit
 Limit                  3E+4      2E+9      9E+18
 Base 10         = E1:  b**4      b**9      b**18
 Base 100        = E2:  b**2      b**4      b**9
 Base 1000       = E3:  b         b**3      b**6
 Base 10000      = E4:  b         b**2      b**4
 Base 100000     = E5:  -         b         b**3
 Base 1000000    = E6:  -         b         b**3
 Base 10000000   = E7:  -         b         b**2
 Base 100000000  = E8:  -         b         b**2
 Base 1000000000 = E9:  -         b         b**2

Thus, when 64-bit signed integer arithmetic is available, the highest possible decimal base is one million if triple-digit numbers are contemplated, but if only double-digit numbers are needed, base one thousand million can be used, which is a reasonable fit in a 32-bit variable.

Since these proceedings are conducted in a high-level language, not machine code, floating-point arithmetic is available and could be used for the DD/MM estimates, with attention to rounding possible. However, integer arithmetic is exact while floating-point is not. If the digits were 32-bit integers, DD and MM would require 64-bit precision (or 62 - some quibbling over signed or unsigned integers), and a 64-bit floating-point number does not offer that. Thus, there would still be constraints on the maximum utilisation of a digit's integer variable. So cheating won't help.


### ==Overshoot==

Anyone who has cranked the handle of a mechanical calculator such as the Odhner will recall a better procedure for when the accumulator goes negative and a loud "Ding!" sounds. Rather than adding back M to the accumulator then shifting one and proceeding at the next digit position, instead shift one then add rather than subtract M with each crank of the handle (you feel the force as you hear the cogs engage and turn, viewing the churn of digits in the display windows: this is number crunching in actuality!) until the accumulator again changes sign ("Ding!") to positive. Hearing these "dings" soon elicits automatic reversal of rotation, though without salivation. This is the equivalent of multiplying by nine through shifting one left to add once (i.e. ten times), then shifting back right to subtract once: two crank twirls, rather than adding nine times with nine twirls and no shifts. As ever, reducing "brute force" effort requires more intelligence. Naturally, Q*M is subtracted once (rather than subtracting M with each of Q turns of the crank handle), and this is done without multiplying out Q*M first.

This recovery from overshoot relies on having overflowed B by no more than one too many multiples of M, which has been ensured by the calculation of Q, Perhaps (in base ten) Q should have been 2·6 rather than 3; if so, shifting M along one place means that instead of 30*M, the equivalent of 26*M should have been used, and this can be attained by adding back 4*M. By this means the recovery also reduces B by at least one digit place. Reversing the overflow by adding back one M costs a multi-digit add without such progress.

Just as the subtraction of Q*M could make zero multiple leading digits of B, not just one, so also can an overflow lead to multiple leading zero digits after the recovery. The extreme case is for a long M number, with the overflow being by just one in the units position of M as currently aligned with B. The overflow will manifest as complemented numbers in the high-order digits, a sequence such as 99999123 (in base ten) with an outstanding borrow, or C = -1. Rather than shifting M over one place for the add back, a multi-place shift will enable a single add back to clear many leading digits, though the shifts must go no further than aligning the units digit of M and B since only integers are in consideration.

In ''16-Bit Modern Microcomputers'', G.W. Corsline remarks "... multiplication and division are somewhat difficult and involve rather esoteric solutions in the general case." (p193) and "... although straightforward, are not short, simple, or easy to understand." (p124).


### =Storage=

=====Run-time allocation=====
As usual, the question "How long is a piece of string?" arises with regard to the arrays of digits to be manipulated. In old-style Fortran, one would define arrays with a size that seemed "big enough" and have an associated variable that counted the number of digits in use. Having two variables to be used in tandem was tiresome and invited mistakes: a possible ploy would be to reserve the first element of the array for the count, with the digits following. The EQUIVALENCE statement could be used to help in some ways (though this is not allowed for parameters to routines), somewhat as follows: 
```Fortran
      INTEGER XLAST,XDIGIT(66),XFIELD(67)
      EQUIVALENCE (XFIELD(1),XLAST)	!The count at the start.
      EQUIVALENCE (XFIELD(3),XDIGIT(1))	!Followed by XDIGIT(0)
```

This introduces ''three'' names, if in a systematic way... But such array indexing invites its own mistakes, especially when relying on lax bound checking. For example, an accidental assignment to XDIGIT(-1) would change XLAST.

With F90 came the ability to declare arrays with a lower bound other than one, and also to define compound data aggregates. Rather dubious code can be replaced by something like 
```Fortran
      TYPE(BIGNUM)	!Define an aggregate.
        INTEGER LAST		!Count of digits in use.
        INTEGER DIGIT(many)	!The first digit is number one.
      END TYPE BIGNUM
      TYPE(BIGNUM) X	!I'll have one.
```

And in this case the X.LAST variable is not a part of the digit array and so can't be damaged via misindexing X.DIGIT as with the earlier attempt using EQUIVALENCE - providing that array bound checking ''is'' engaged. Thus, one gains the ability to use X in a way that is somewhat more self-documenting, but there is still the annoyance of deciding on how many digits will be enough, and further, the BIGNUM type allows for only a fixed size. A different size would entail a different type and thereby cause parameter mismatching - should that be checked...

A further feature of F90 enables the determination of storage needs at run time, and via the ALLOCATE statement, a variable may be defined that is indeed "big enough" for the expected need. First, change the specification of DIGIT in BIGNUM to something like <code>INTEGER, ALLOCATABLE:: DIGIT(:)</code> then after calculating a suitable value for MANY, put <code>ALLOCATE(X.DIGIT(MANY))</code> By this means, different-sized variables all have the same type, and can be passed as parameters, etc. But before acclaiming this as another demonstration of the merit of modern features, consider the following code snippet from BIGMULTN, as produced by the Compaq F90/95 compiler. First, with fixed-size DIGIT arrays:

```txt

153:              DO I = 1,B.LAST !Step through the digits, upwards powers.
004018E1   mov         esi,dword ptr [ebx]
004018E3   mov         dword ptr [ebp-8],esi
004018E6   mov         edi,1
004018EB   mov         dword ptr [I (0046b9bc)],edi
004018F1   cmp         esi,0
004018F4   jle         BIGNUMBERS_mp_BIGMULTN+260h (00401adf)
154:                D = B.DIGIT(I)*V + C  !Grab a digit and apply the multiply.
004018FA   cmp         dword ptr [I (0046b9bc)],1
00401901   jl          BIGNUMBERS_mp_BIGMULTN+90h (0040190f)
00401903   cmp         dword ptr [I (0046b9bc)],8AEh
0040190D   jle         BIGNUMBERS_mp_BIGMULTN+99h (00401918)
0040190F   xor         eax,eax
00401911   mov         dword ptr [ebp-34h],eax
00401914   dec         eax
00401915   bound       eax,qword ptr [ebp-34h]
00401918   imul        edi,dword ptr [I (0046b9bc)],4
0040191F   mov         esi,dword ptr [V (0046b9b8)]
00401925   imul        esi,dword ptr [ebx+edi]
00401929   add         esi,dword ptr [C (0046b9c4)]
0040192F   mov         dword ptr [D (0046b9c0)],esi
155:                B.DIGIT(I) = MOD(D,BIGBASE)   !Place the resulting digit.
00401935   cmp         dword ptr [I (0046b9bc)],1
0040193C   jl          BIGNUMBERS_mp_BIGMULTN+0CBh (0040194a)
0040193E   cmp         dword ptr [I (0046b9bc)],8AEh
00401948   jle         BIGNUMBERS_mp_BIGMULTN+0D4h (00401953)
0040194A   xor         eax,eax
0040194C   mov         dword ptr [ebp-34h],eax
0040194F   dec         eax
00401950   bound       eax,qword ptr [ebp-34h]
00401953   imul        edi,dword ptr [I (0046b9bc)],4
0040195A   mov         esi,2710h
0040195F   mov         eax,dword ptr [D (0046b9c0)]
00401965   cdq
00401966   idiv        eax,esi
00401968   mov         eax,edx
0040196A   mov         dword ptr [ebx+edi],eax
156:                C = D/BIGBASE     !Agony! TWO divisions per step!!

```

With the ALLOCATE usage, the same source code leads to...

```txt

154:              DO I = 1,B.LAST !Step through the digits, upwards powers.
00401A55   mov         ebx,dword ptr [B]
00401A58   mov         edx,dword ptr [ebx]
00401A5A   mov         dword ptr [ebp-8],edx
00401A5D   mov         eax,1
00401A62   mov         dword ptr [I (0046cacc)],eax
00401A68   cmp         edx,0
00401A6B   jle         BIGNUMBERS_mp_BIGMULTN+2E3h (00401ca2)
155:                D = B.DIGIT(I)*V + C  !Grab a digit and apply the multiply.
00401A71   mov         ebx,dword ptr [B]
00401A74   mov         esi,ebx
00401A76   mov         edi,dword ptr [esi+20h]
00401A79   add         edi,dword ptr [esi+18h]
00401A7C   sub         edi,1
00401A7F   mov         eax,dword ptr [I (0046cacc)]
00401A85   cmp         eax,dword ptr [esi+20h]
00401A88   jl          BIGNUMBERS_mp_BIGMULTN+0CFh (00401a8e)
00401A8A   cmp         eax,edi
00401A8C   jle         BIGNUMBERS_mp_BIGMULTN+0D8h (00401a97)
00401A8E   xor         edi,edi
00401A90   mov         dword ptr [ebp-34h],edi
00401A93   dec         edi
00401A94   bound       edi,qword ptr [ebp-34h]
00401A97   imul        eax,eax,4
00401A9A   imul        edx,dword ptr [esi+20h],4
00401A9E   mov         edi,dword ptr [esi+4]
00401AA1   sub         edi,edx
00401AA3   mov         edi,dword ptr [edi+eax]
00401AA6   imul        edi,dword ptr [V (0046cac8)]
00401AAD   add         edi,dword ptr [C (0046cad4)]
00401AB3   lea         esi,[esi+4]
00401AB6   mov         dword ptr [D (0046cad0)],edi
156:                B.DIGIT(I) = MOD(D,BIGBASE)   !Place the resulting digit.
00401ABC   mov         ebx,dword ptr [B]
00401ABF   mov         ecx,ebx
00401AC1   mov         eax,dword ptr [ecx+20h]
00401AC4   add         eax,dword ptr [ecx+18h]
00401AC7   sub         eax,1
00401ACC   mov         edx,dword ptr [I (0046cacc)]
00401AD2   cmp         edx,dword ptr [ecx+20h]
00401AD5   jl          BIGNUMBERS_mp_BIGMULTN+11Ch (00401adb)
00401AD7   cmp         edx,eax
00401AD9   jle         BIGNUMBERS_mp_BIGMULTN+125h (00401ae4)
00401ADB   xor         eax,eax
00401ADD   mov         dword ptr [ebp-34h],eax
00401AE0   dec         eax
00401AE1   bound       eax,qword ptr [ebp-34h]
00401AE4   imul        esi,edx,4
00401AE7   imul        edi,dword ptr [ecx+20h],4
00401AEB   mov         eax,dword ptr [ecx+4]
00401AEE   sub         eax,edi
00401AF0   mov         edi,eax
00401AF2   mov         ebx,2710h
00401AF7   mov         eax,dword ptr [D (0046cad0)]
00401AFD   cdq
00401AFE   idiv        eax,ebx
00401B00   mov         eax,edx
00401B02   lea         ecx,[ecx+4]
00401B05   mov         dword ptr [edi+esi],eax
157:                C = D/BIGBASE     !Agony! TWO divisions per step!!

```

Evidently, every reference to an allocatable-size array involves quite a lot more code. A test run up to primorial 80 with run-time allocations of BIGLIMIT took ~4·42 seconds while fixed-size runs took ~4·13, a 7% increase. This is discouraging.


### ==Simple array==

In Fortran there has never been a problem with passing arrays of a different size to a subroutine (unlike in Pascal), so, representing the big number in a simple array (with DIGIT.LAST as the first element) would allow diferent sizes of a big number. One will just have to take care with the indexing and offsets, especially if indexing always starts with one. It so happens that type BIGNUM declared array DIGIT starting with DIGIT(1), so, taking advantage of F90's ability to allow indexing to start with zero enables <code>B.LAST</code> to be replaced by <code>B(0)</code> and <code>B.DIGIT(i)</code> to be replaced by <code>B(i)</code>, though if the array indexing had started with zero, B(-1) could be reserved for the count. The type BIGNUM vanishes in favour of a simple <code>INTEGER B(0:BIGLIMIT)</code> - all this at a loss of self-documentation, though no fearsome EQUIVALENCE statements. The resulting test run takes about the same time at ~4·12 seconds, but, when the parameter declarations are changed from <code>INTEGER B(0:BIGLIMIT)</code> to <code>INTEGER B(0:)</code> to signify that the upper bound is flexible, the test run now takes ~4·5 seconds. Here is the same code from BIGMULTN for when the upper bound is specified:

```txt

153:              DO I = 1,B(0)   !Step through the digits, upwards powers.
004018CE   mov         esi,dword ptr [ebx]
004018D0   mov         dword ptr [ebp-8],esi
004018D3   mov         edi,1
004018D8   mov         dword ptr [I (0046b9bc)],edi
004018DE   cmp         esi,0
004018E1   jle         BIGNUMBERS_mp_BIGMULTN+260h (00401acc)
154:                D = B(I)*V + C    !Grab a digit and apply the multiply.
004018E7   cmp         dword ptr [I (0046b9bc)],0
004018EE   jl          BIGNUMBERS_mp_BIGMULTN+90h (004018fc)
004018F0   cmp         dword ptr [I (0046b9bc)],8AEh
004018FA   jle         BIGNUMBERS_mp_BIGMULTN+99h (00401905)
004018FC   xor         eax,eax
004018FE   mov         dword ptr [ebp-34h],eax
00401901   dec         eax
00401902   bound       eax,qword ptr [ebp-34h]
00401905   imul        edi,dword ptr [I (0046b9bc)],4
0040190C   mov         esi,dword ptr [V (0046b9b8)]
00401912   imul        esi,dword ptr [ebx+edi]
00401916   add         esi,dword ptr [C (0046b9c4)]
0040191C   mov         dword ptr [D (0046b9c0)],esi
155:                B(I) = MOD(D,BIGBASE) !Place the resulting digit.
00401922   cmp         dword ptr [I (0046b9bc)],0
00401929   jl          BIGNUMBERS_mp_BIGMULTN+0CBh (00401937)
0040192B   cmp         dword ptr [I (0046b9bc)],8AEh
00401935   jle         BIGNUMBERS_mp_BIGMULTN+0D4h (00401940)
00401937   xor         eax,eax
00401939   mov         dword ptr [ebp-34h],eax
0040193C   dec         eax
0040193D   bound       eax,qword ptr [ebp-34h]
00401940   imul        edi,dword ptr [I (0046b9bc)],4
00401947   mov         esi,2710h
0040194C   mov         eax,dword ptr [D (0046b9c0)]
00401952   cdq
00401953   idiv        eax,esi
00401955   mov         eax,edx
00401957   mov         dword ptr [ebx+edi],eax
156:                C = D/BIGBASE     !Agony! TWO divisions per step!!

```

It is much the same as for the non-allocated BIGNUM code. But when the specification is <code>INTEGER B(0:)</code> it becomes:

```txt

153:              DO I = 1,B(0)   !Step through the digits, upwards powers.
00402021   mov         edi,dword ptr [ebx+1Ch]
00402024   add         edi,dword ptr [ebx+14h]
00402027   sub         edi,dword ptr [ebx+1Ch]
0040202A   sub         edi,1
0040202D   mov         eax,dword ptr [ebx]
0040202F   xor         esi,esi
00402031   cmp         esi,edi
00402033   jle         BIGNUMBERS_mp_BIGMULTN+0E1h (0040203e)
00402035   xor         edi,edi
00402037   mov         dword ptr [ebp-38h],edi
0040203A   dec         edi
0040203B   bound       edi,qword ptr [ebp-38h]
0040203E   imul        esi,dword ptr [ebx+18h]
00402042   mov         edi,dword ptr [eax+esi]
00402045   mov         dword ptr [ebp-8],edi
00402048   mov         esi,1
0040204D   mov         dword ptr [I (0046d9bc)],esi
00402053   cmp         edi,0
00402056   jle         BIGNUMBERS_mp_BIGMULTN+327h (00402284)
154:                D = B(I)*V + C    !Grab a digit and apply the multiply.
0040205C   mov         eax,dword ptr [ebx+1Ch]
0040205F   add         eax,dword ptr [ebx+14h]
00402062   sub         eax,dword ptr [ebx+1Ch]
00402065   sub         eax,1
0040206A   mov         esi,dword ptr [ebx]
0040206C   mov         edi,dword ptr [I (0046d9bc)]
00402072   cmp         edi,0
00402075   jl          BIGNUMBERS_mp_BIGMULTN+11Eh (0040207b)
00402077   cmp         edi,eax
00402079   jle         BIGNUMBERS_mp_BIGMULTN+127h (00402084)
0040207B   xor         eax,eax
0040207D   mov         dword ptr [ebp-38h],eax
00402080   dec         eax
00402081   bound       eax,qword ptr [ebp-38h]
00402084   imul        edi,dword ptr [ebx+18h]
00402088   mov         esi,dword ptr [esi+edi]
0040208B   imul        esi,dword ptr [V (0046d9b8)]
00402092   add         esi,dword ptr [C (0046d9c4)]
00402098   mov         dword ptr [D (0046d9c0)],esi
155:                B(I) = MOD(D,BIGBASE) !Place the resulting digit.
0040209E   mov         edi,dword ptr [ebx+1Ch]
004020A1   add         edi,dword ptr [ebx+14h]
004020A4   sub         edi,dword ptr [ebx+1Ch]
004020A7   sub         edi,1
004020AA   mov         esi,dword ptr [ebx]
004020AC   mov         eax,dword ptr [I (0046d9bc)]
004020B2   cmp         eax,0
004020B7   jl          BIGNUMBERS_mp_BIGMULTN+160h (004020bd)
004020B9   cmp         eax,edi
004020BB   jle         BIGNUMBERS_mp_BIGMULTN+169h (004020c6)
004020BD   xor         edi,edi
004020BF   mov         dword ptr [ebp-38h],edi
004020C2   dec         edi
004020C3   bound       edi,qword ptr [ebp-38h]
004020C6   imul        eax,dword ptr [ebx+18h]
004020CA   mov         ecx,eax
004020CC   mov         edi,2710h
004020D1   mov         eax,dword ptr [D (0046d9c0)]
004020D7   cdq
004020D8   idiv        eax,edi
004020DA   mov         eax,edx
004020DC   mov         dword ptr [esi+ecx],eax
156:                C = D/BIGBASE     !Agony! TWO divisions per step!!

```

Accessing the secret additional parameter that gives the array parameter's actual upper bound for array bound checking takes more time. All this is a long way away from notions such as <code>D = B.DIGIT(I)*V + C</code> producing code like
      Load  I
      Index B.DIGIT
      Mult  V
      Add   C
      Store D

=====Fixed-size data aggregates=====
Since the BIGNUM type with fixed storage runs no slower but offers better documentation, the possibility of big numbers sized to fit is abandoned for demonstration purposes. Storage is plentiful these days, and can be expended with far less angst.


### =Source=


### ==Organisation==

This uses the MODULE facilities of F90 to save on communication (though it seems that fixed-size work areas in COMMON may enable faster code) and although the DIGIT array could start with index zero so that the index value would correspond to the powers of the base, it starts at one so that the first digit, the units digit, is at index one. As ever, care is required with the various counting on digits.

Most of the source code is devoted to providing the BIGNUM facilities, since there is no built-in facility for this nor a universally-used library such as the "stdio.h" collection in C and similar languages. Via a great deal of additional syntax it is possible to extend F90 so as to provide the appearance of multi-precision arithmetic via the ordinary usage of expressions, as is exemplified for rational arithmetic in [[Arithmetic/Rational#Fortran]]. However, the "formula translation" that this invites will not work well for the likes of <code>MOD(A**B,M)</code> when the variables are hundred-digit numbers because A**B will overflow any possible storage scheme. The modulo arithmetic feature must be pushed into the calculation of A**B, and even if one has prepared a suitable routine such as BIGMODEXP, the language extension facilities are not going to recognise its suitability for calculating <code>MOD(A**B,M)</code> in a general context because they manifest "formula translation". One must employ explicit coding, as in BIGMRPRIME.


### ==Support==

 
```Fortran
      MODULE BIGNUMBERS	!Limited services: decimal integers, no negative numbers.
       INTEGER BIGORDER		!A limited attempt at generality.
       PARAMETER (BIGORDER = 4)	!This is the order of the base of the big number arithmetic.
       INTEGER BIGBASE,BIGLIMIT	!Sized thusly.
       PARAMETER (BIGBASE = 10**BIGORDER, BIGLIMIT = 8888/BIGORDER)	!Enough?
       TYPE BIGNUM	!So, a big number is simple.
        INTEGER LAST		!This many digits (of size BIGBASE) are in use.
        INTEGER DIGIT(BIGLIMIT)	!The digits, in ascending power order.
       END TYPE BIGNUM	!So much for that.
       INTEGER BIGLEADN	!Additional stuff for its rounding.
       PARAMETER (BIGLEADN = 8/BIGORDER)	!Sufficient digits to show. With a struggle.
       INTEGER BIGLEAD(BIGLEADN + 1)		!Followed by an exponent.
Collect some statistics on the working of BIGMRPRIME
       INTEGER BIGMRTRIALS		!How many trials, at most.
       PARAMETER (BIGMRTRIALS = 6)	!This might do.
       INTEGER BIGMRCOUNT(BIGMRTRIALS)	!BIGMRPRIME may not use all its trials.
       DATA BIGMRCOUNT/BIGMRTRIALS*0/	!None so far.
       CONTAINS		!Now for some assistants.
        SUBROUTINE BIGWRITE(F,B)	!Show B.
         INTEGER F	!I/O unit number.
         TYPE(BIGNUM) B	!The number.
          WRITE (F,1,ADVANCE="NO") B.DIGIT(B.LAST:1:-1)	!Roll the digits in base ten.
    1     FORMAT (665I<BIGORDER + 1>.<BIGORDER>)	!Leading zeroes after the first digit.
        END SUBROUTINE BIGWRITE		!Simple, but messy.

        INTEGER FUNCTION BIGSIGN(A,B)	!Sign(A - B) returns -ve, 0, +ve. Not just -1, 0, +1.
         TYPE(BIGNUM) A,B	!The two numbers.
         INTEGER L		!A finger.
          BIGSIGN = A.LAST - B.LAST	!Compare the number of digits.
          IF (BIGSIGN.EQ.0) THEN	!The same?
            DO L = A.LAST,1,-1			!Alas. Compare the digits themselves.
              BIGSIGN = A.DIGIT(L) - B.DIGIT(L)		!From the high-order, down.
              IF (BIGSIGN.NE.0) RETURN			!A difference yet?
            END DO				!Descend to the next pair of digits.
          END IF			!So much for shortcuts.
        END FUNCTION BIGSIGN	!Hopefully, not many digits will have to be compared.

        SUBROUTINE BIGLOADN(B,N)!B:=N	!Convert a normal number.
         TYPE(BIGNUM) B	!The multi-BIGBASE number.
         INTEGER N	!An ordinary number. Negatives need not apply.
         INTEGER C	!The carry.
          B.LAST = 1	!Start with one digit.
          C = N		!Perhaps more than one will be needed.
    1     B.DIGIT(B.LAST) = MOD(C,BIGBASE)	!Place the digit.
          C = C/BIGBASE		!Lose that digit.
          IF (C.GT.0) THEN	!More remains?
            B.LAST = B.LAST + 1		!Yes. Another digit is needed.
            GO TO 1			!And try again.
          END IF		!Just in case N >= BIGBASE.
        END SUBROUTINE BIGLOADN	!Worry over negative numbers sometime later.

        DOUBLE PRECISION FUNCTION BIGVALUE(B)	!Convert back to an ordinary number. Limited range!
         TYPE(BIGNUM) B	!The mysterious big number.
         REAL*8 F	!The mantissa to come. "Infinity" starts at ~10**306.
         INTEGER D	!Counts BIGBASE digits.
         INTEGER I	!A stepper.
          F = 0		!I'm not messing with negative numbers yet.
          D = 0		!No digits assimilated.
          DO I = B.LAST,MAX(1,B.LAST - 18/BIGORDER),-1	!Work from the high order down.
            D = D + 1			!Another one in.
            F = F*BIGBASE + B.DIGIT(I)	!Thus.
          END DO			!Perhaps more to come.
          BIGVALUE = F*DFLOAT(BIGBASE)**(B.LAST - D)	!Beware floating-point overflow!
        END FUNCTION BIGVALUE	!An alternative would be BIGLOG(B).

        SUBROUTINE BIGTASTE(B)	!Represent B in BIGLEAD as a floating-point number.
Copies the first few digits to BIGLEAD, with rounding, followed by the base ten exponent.
         TYPE(BIGNUM) B		!The number to taste.
         INTEGER L,IT		!Fingers.
         INTEGER D,E		!A digit and an exponent.
          IF (MOD(BIGBASE,10).NE.0) STOP "BIGTASTE expects powers of 10"	!Alas. Otherwise the "E" formalism fails.         
          BIGLEAD = 0			!Scrub, on general principles.
          L = MIN(BIGLEADN,B.LAST)	!I'm looking to taste the leading digits; too few?.
          BIGLEAD(1:L) = B.DIGIT(B.LAST:B.LAST - L + 1:-1)	!Reverse, to have normal order.
          E = (B.LAST - 1)*BIGORDER	!Convert from 10**BIGORDER to base 10.
          D = B.DIGIT(B.LAST)		!Grab the high-order digit.
          DO WHILE(D.GT.0)		!It is not zero..
            E = E + 1			!So it is at least one base ten digit.
            D = D/10			!Snip.
          END DO			!And perhaps there will be more.
          D = 0				!We should consider rounding up.
          IF (B.LAST.GT.BIGLEADN) THEN	!Are there even more digits?
            IT = L			!Yes. This is now the low-order digit tasted.
            IF (B.DIGIT(B.LAST - L).GE.BIGBASE/2) D = 1	!If the next digit is big enough.
          C:DO WHILE (D.GT.0)		!Spread the carry.
              D = 0				!This one is used up.
              BIGLEAD(IT) = BIGLEAD(IT) + 1	!Thusly.
              IF (BIGLEAD(IT).GE.BIGBASE) THEN	!But, maybe, overflow!
                BIGLEAD(IT) = BIGLEAD(IT) - BIGBASE	!Yes!
                D = 1					!Reassert a carry.
                IT = IT - 1				!Step back to the next recipient up.
                IF (IT.LE.0) EXIT C			!If there isn't one, quit!
              END IF				!So much for that overflow.
            END DO C			!Mostly, a carry doesn't propagate far.
          END IF			!So, no test for IT > 0 in a compound "while".
          IF (D.NE.0) THEN	!If a carry remains, the rounding propagated all the way up!
            E = E + 1			!So, count another power of ten up.
            BIGLEAD(1) = 1		!And thus maintain the 0.etc. style.
c           BIGLEAD(2:) = 0		!These are already zero.
          END IF		!Enough carrying.
          BIGLEAD(BIGLEADN + 1) = E	!Place the exponent.
        END SUBROUTINE BIGTASTE	!That was messy.

        SUBROUTINE BIGNORM(B)	!Normalise B to avoide a horde of leading zero digits.
         TYPE(BIGNUM) B		!The number.
Can't rely on DO WHILE(B.LAST.GT.1 .AND. B.DIGIT(B.LAST)).NE.0)  beause *both* parts *might* be evaluated. Bah.
   10     IF (B.LAST.GT.1) THEN	!If B has a single digit only, it can be zero.
            IF (B.DIGIT(B.LAST).NE.0) RETURN	!If it is not zero, we're done.
            B.LAST = B.LAST - 1		!Otherwise, step down one digit,
            GO TO 10			!And check afresh.
          END IF		!So much for normalisation. Rather like floating-point numbers.
        END SUBROUTINE BIGNORM	!In memory of Norman Kirk, Labour party leader and prime minister.

        SUBROUTINE BIGADDN(B,N)	!B:=B + N	Add a (small) ordinary number to a big number.
         TYPE(BIGNUM) B	!The big number to be augmented.
         INTEGER N	!The addend. Should be smaller than the integer limit less BIGBASE.
         INTEGER I	!A stepper.
         INTEGER C,D	!Assistants for the arithmetic.
          C = N			!Start as if already in progress.
          DO I = 1,B.LAST	!Spread the carry to higher digits as needed.
            D = B.DIGIT(I) + C		!Thus.
            IF (D.GE.0) THEN		!Negative N might require a borrow.
              B.DIGIT(I) = MOD(D,BIGBASE)	!Not this time. Correct the digit.
              C = D/BIGBASE			!And calculate the carry to continue.
             ELSE			!Otherwise, borrow one from the next digit up.
              D = D + BIGBASE			!Thus.
              B.DIGIT(I) = MOD(D,BIGBASE)	!MOD for negative number can behave unexpectedly.
              C = D/BIGBASE - 1			!Repay the borrow.
            END IF			!So much for DIGIT(I).
            IF (C.EQ.0) RETURN		!Finished already?
          END DO		!On to the next digit up.
Completed work with the original number of digits in the big number.
          DO WHILE(C .NE. 0)	!Now spread the last carry to further digits.
            B.LAST = B.LAST + 1		!Up one more.
            IF (B.LAST .GT. BIGLIMIT) STOP "Overflow by addition!"	!Perhaps not.
            B.DIGIT(B.LAST) = MOD(C,BIGBASE)	!The digit.
            C = C/BIGBASE		!The carry may be large, if N is large.
          END DO		!So slog on until it is gone.
        END SUBROUTINE BIGADDN	!Negative  values for B are not considered.

        SUBROUTINE BIGMULTN(B,N)	!B:=B*N;	Multiply by an integer possibly bigger than the base.
         TYPE(BIGNUM) B	!The worker.
         INTEGER N	!A computer number, not a multi-digit number.
         INTEGER D	!Must be able to hold (BIGBASE - 1)*N + C
         INTEGER C	!The carry to the next digit.
         INTEGER*8 DD	!N may be large...
         INTEGER V	!Beware of BIGMULT(B,B); B.DIGIT(1) is N.
         INTEGER I	!A stepper.
          IF (N.EQ.1) RETURN	!Phooey.
          IF (N.EQ.0) THEN	!This takes a little more effort.
            B.LAST = 1			!One digit.
            B.DIGIT(1) = 0		!Zero.
            RETURN			!Done.
          END IF		!Otherwise, start work.
          V = N			!Grab a local copy in case of doubled reference.
          C = 0			!No previous digit to carry from.
          IF (N .LT. HUGE(D)/BIGBASE) THEN	!Can D hold N*BIGBASE?
            DO I = 1,B.LAST	!Step through the digits, upwards powers.
              D = B.DIGIT(I)*V + C	!Grab a digit and apply the multiply.
              B.DIGIT(I) = MOD(D,BIGBASE)	!Place the resulting digit.
              C = D/BIGBASE		!Agony! TWO divisions per step!!
            END DO		!On to the next digit up.
           ELSE		!Larger N means escalating to a proper double-digit product.
            DO I = 1,B.LAST	!Step through the same digits.
              DD = INT8(B.DIGIT(I))*V + C	!Grab a digit and apply the multiply.
              B.DIGIT(I) = MOD(DD,BIGBASE)	!Place the resulting digit.
              C = DD/BIGBASE		!Agony! TWO divisions per step!!
            END DO		!On to the next digit up.
          END IF		!Either way, the carry is a single digit.
          DO WHILE(C .GT. 0)	!Now spread the last carry to further digits.
            B.LAST = B.LAST + 1		!Up one more.
            IF (B.LAST .GT. BIGLIMIT) STOP "Overflow by multiply!"	!Perhaps not.
            B.DIGIT(B.LAST) = MOD(C,BIGBASE)	!The digit.
            C = C/BIGBASE		!The carry may be large, if N is large.
          END DO		!So slog on until it is gone.
        END SUBROUTINE BIGMULTN	!Primary school stuff.

        SUBROUTINE BIGMULT(A,B)	!A:=A*B, and yes, BIGMULT(A,A) will square A.
C   Calculates from the high-order end downwards, with carries going against that flow.
C   This enables the result to be calculated in-place, even with BIGMULT(A,A).
C
C                                 a5   a4   a3   a2   a1  ...Five digits.
c                                  x   b4   b3   b2   b1  ...Four digits.
c              -----------------------------------------
c                               a5b1 a4b1 a3b1 a2b1 a1b1
c                          a5b2 a4b2 a3b2 a2b2 a1b2      ... five wide.
c                     a5b3 a4b3 a3b3 a2b3 a1b3           ... four high.
c                a5b4 a4b4 a3b4 a2b4 a1b4
c          ---------------------------------------------
c          carry    8    7    6    5    4    3    2    1  At most nine digits.
c          ---------------------------------------------
C   A column sum of many digit products might overflow S given many B digits and a big base.
         TYPE(BIGNUM) A,B	!The numbers.
         INTEGER IA,FA,LA	!For A: Index, First (highest order), Last (lowest order).
         INTEGER IB,FB,LB	!For B: Index, First (highest order), Last (lowest order).
         INTEGER L		!Fingers a digit for a result.
         INTEGER*8 S		!Scratchpad for digit multiply and summation.
          IF ((BIGBASE - 1)*B.LAST.GE.HUGE(S)/(BIGBASE - 1))	!Max. digit product, summed,
     1     STOP "BIGMULT: too many B digits! Could overflow S!"	!Rather than only ever one at a time..
Check for some simple situations in the hope of evading big nullities.
          LB = B.LAST		!I'll need a copy of the original layout.
          IF (LB.EQ.1) THEN	!A single digit B may have some special values.
            IF (B.DIGIT(1).EQ.0) THEN	!Multiplying A by zero?
              CALL BIGLOADN(A,0)		!Easy.
            ELSE IF (B.DIGIT(1).NE.1) THEN	!And if we're not multiplying by one,
              CALL BIGMULTN(A,B.DIGIT(1))		!This is easier.
            END IF			!These values do not appear at random.
            RETURN			!Done.
          END IF		!Single-digit big numbers? Hummm.
          LA = A.LAST		!Copy for later also.
          IF (LA.EQ.1) THEN		!Perhaps a single-digit here instead..
            IF (A.DIGIT(1).EQ.0) RETURN	!Zero times something? Zero!
            IF (A.DIGIT(1).EQ.1) THEN	!One times something?
              A.LAST = B.LAST			!Yes!
              A.DIGIT(1:A.LAST) = B.DIGIT(1:B.LAST)	!Copy that something.
              RETURN				!That was easy.
            END IF			!So much for one.
          END IF			!Might as well avoid wasted effort.
Can't avoid work any longer. If the big numbers are *big*, then a lot can be avoided. But, single-digits surely would be rare?
          IF (LA + LB .GT. BIGLIMIT + 1) STOP "BigMult will overflow!"	!Fixed storage sizes.
          A.LAST = LA + LB - 1		!Where the first result digit will go.
          FA = LA			!Parallelogram parsing control.
          FB = LB			!These finger the high-order digit.
Commence producing digits. Work down each column, starting with the high-order side.
          S = 0				!The grand sum, of many digit products.
   10     IB = FB			!Index of B's digit of the moment.
          DO IA = FA,LA,-1		!Accumulate, working down a column, though bottom to top would do also.
            S = INT8(A.DIGIT(IA))*B.DIGIT(IB) + S	!Another digit product added in.
            IB = IB + 1					!NB: IA + IB is constant in this loop.
          END DO			!It is the digit power, plus two since DIGIT arrays start with one.
          IF (S.LT.0) STOP "BIGMULT: S has flipped sign!"	!Oh dear.
          L = FA + FB - 1		!Finger the recipient digit.
          A.DIGIT(L) = MOD(S,BIGBASE)	!Place the digit.
Completed the column sum. Now spread any carry into higher digits as necessary.
          S = S/BIGBASE			!The sum's carry to the next digit up.
          DO WHILE(S > 0)		!It may well be multi-digit itself, being the sum of many digit products.
            L = L + 1				!Go back (up) a power.
            IF (L.LE.A.LAST) THEN		!Since we're going high to low,
              S = S + A.DIGIT(L)			!We add to prior work as we go back up.
             ELSE				!Or else (once, sigh), extending.
              A.LAST = L				!Because the high-order product carried up one.
              IF (L.GE.BIGLIMIT) STOP "BigMult has overflowed!"	!Perhaps too far!
            END IF			!Righto, S is ready.
            A.DIGIT(L) = MOD(S,BIGBASE)	!Place the digit.
            S = S/BIGBASE		!Drop a power as we climb to a still higher digit.
          END DO			!This may be many powers large.
Contemplate the parallelogram.
          IF (FB.GT.1) THEN		!The topmost term of a column
            FB = FB - 1			!Starts with this B-digit.
          ELSE IF (FA.GT.1) THEN	!But after the units B-digit is reached,
            FA = FA - 1			!The column starts with lesser A-digits.
          ELSE				!And when the units A-digit has been reached,
            RETURN			!We're done.
          END IF			!So much for the start elements of the parallelogram.
          IF (LA.GT.1) LA = LA - 1	!The lowest-order A-digit of a column.
          GO TO 10			!Peruse the diagram.
        END SUBROUTINE BIGMULT	!Not the Primary School order, but equivalent.

        SUBROUTINE BIGSQUARE(B)	!B:=B*B.
c     The special feature here is that half the effort of digit multiplying can be avoided by noting
c  that many digit products are paired, since Bi*Bj = Bj*Bi. Working along the rows from right to left
c  in the usual manner is easy: start with the Bi*Bi term, then roll along adding 2*Bi*Bj terms in long arithmetic.
c  The loop control is simple enough, but the doubling requirement is annoying since with fully-packed words
c  there would be overflows to deal with. Another way is to go down the columns.
c     "But you still have to double the terms" said Bruce Christianson, whom I met on the way back from lunch.
c     "Yes, but this way you can add them all up and double them once at the end."
c     Howl of anguish from Bruce, bewildered blinking from me: "What did I say?"
c     Bruce had spent most of a lunchtime discussing with John Rumsey, VAX expert, the opportunities offered
c  by Digital's VAX cpu, especially the feature that allows custom microcode to be loaded for unused op-codes
c  and which might help with the irritating task of the doubling of each term...
c     Actually, this can be done in yet a third way, in two passes: first, form the result (with no doubling)
c  and no squared terms (that are not to be doubled) in any way convenient, then in the second pass,
c  perform the doubling and add in the squared terms. Running along the rows has the advantage that
c  one of the digits is constant, so only the other has to be extracted from the array.
c     But, results must be placed in array elements, so there would still be two array accesses per step.
c
C
C                                          b6    b5    b4    b3    b2    b1  ... Six digits.
c                                        x b6    b5    b4    b3    b2    b1
c   -----------------------------------------------------------------------
c                                        b6b1  b5b1  b4b1  b3b1  b2b1  b1b1
c                                  b6b2  b5b2  b4b2  b3b2  b2b2  b1b2        ... six wide.
c                            b6b3  b5b3  b4b3  b3b3  b2b3  b1b3              ... six high.
c                      b6b4  b5b4  b4b4  b3b4  b2b4  b1b4
c                b6b5  b5b5  b4b5  b3b5  b2b5  b1b5
c          b6b6  b5b6  b4b6  b3b6  b2b6  b1b6
c
c   But, since b2b1 = b1b2, etc.,
c
c                                      |2b6b1 2b5b1|2b4b1 2b3b1|2b2b1  b1b1  ... six,
c                                 2b6b2|2b5b2 2b4b2|2b3b2  b2b2|             ... five,
c                          |2b6b3 2b5b3|2b4b3  b3b3|           |             ... four,
c                     2b6b4|2b5b4  b4b4|           |           |             ... three,
c              |2b6b5  b5b5|           |           |           |             ... two,
c          b6b6|           |           |           |           |             ... one.
c   -----------------------------------------------------------------------
c   carry    11    10     9     8     7     6     5     4     3     2     1  At most twelve digits.
c   -----------------------------------------------------------------------
C  A column sum of many digit products might overflow S given many B digits and a big base.
c  Working from the high-order end downwards (with carries going against that flow) enables the result to be calculated in-place.
         TYPE(BIGNUM) B	!The number to be squared.
         INTEGER FA,LA	!Index for the first and last of the lead digit in the mixed digit products. a6 in a6a5, etc.
         INTEGER FB	!Index for the first of the second digit in the product pairs.
         INTEGER IA,IB	!Index for the first and second parts of each digit product a5a4, etc.
         INTEGER L	!Fingers a digit for a result.
         INTEGER*8 S	!Scratchpad for digit multiply and summation.
         LOGICAL FLIP	!Some columns have a lone term, others have only doubled terms.
c          write (6,*) "B-digit limit",HUGE(S)/INT8(BIGBASE - 1)**2
          IF ((BIGBASE - 1)*B.LAST.GE.HUGE(S)/(BIGBASE - 1))	!Max. digit product, summed,
     1     STOP "BIGSQUARE: too many B digits! Could overflow S!"	!Rather than only ever one at a time..
Check for some simple situations in the hope of evading big nullities.
          FB = B.LAST		!I'll need a copy of the original layout.
          IF (FB.EQ.1) THEN	!A single digit B doesn't have many digits.
            CALL BIGMULTN(B,B.DIGIT(1))		!This is easier.
            RETURN			!Done.
          END IF		!Single-digit big numbers? Hummm.
Can't avoid work any longer. If the big numbers are *big*, then a lot can be avoided. But, single-digits surely would be rare?
          L = 2*FB - 1		!Locate the highest order product: b6b6 in the schedule.
          IF (L .GT. BIGLIMIT) STOP "BigSquare will overflow!"	!Fixed storage sizes.
          B.DIGIT(FB + 1:L) = 0	!Scrub, ready for carry propagation.
          B.LAST = L		!This might be extended by one.
          FA = FB		!Triangle parsing control. These finger the high-order digit.
          LA = FA + 1		!Syncopation: the a6a6 lone term has no sum of doubled products.
          FLIP = .TRUE.		!So the first loop for them won't. L is odd.
Commence producing digits. Work down each column, starting with the high-order side.
          S = 0			!The grand sum, of many digit products.
   10     IB = FB			!Index of the second digit in each product.
          DO IA = FA,LA,-1		!Accumulate, working down a column, though bottom to top would do also.
            S = INT8(B.DIGIT(IA))*B.DIGIT(IB) + S	!Another digit product added in.
            IB = IB + 1					!NB: IA + IB is constant in this loop.
          END DO			!It is the digit power, plus two since DIGIT arrays start with one..
          S = S + S			!Thus the sum of the 2* terms in one go...
          IF (FLIP) THEN		!For odd powers, there is also an undoubled product.
            LA = LA - 1				!Its index is one less than the last digit for a doubled product.
            S = S + INT8(B.DIGIT(LA))**2	!Another chance for overflowing S.
          END IF			!The column sum is now complete.
          IF (S.LT.0) STOP "BigSquare: S has flipped sign!"	!Oh dear.
          FLIP = .NOT.FLIP		!Thus the adjustment on every odd power of the result.
          L = FA + FB - 1		!Finger the recipient digit.
          B.DIGIT(L) = MOD(S,BIGBASE)	!Place the digit.
Completed the column sum. Now spread any carry into higher digits as necessary.
          S = S/BIGBASE			!The sum's carry to the next digit up.
          DO WHILE(S > 0)		!It may well be multi-digit itself, being the sum of many digit products.
            L = L + 1				!Go back (up) a power.
            IF (L.LE.B.LAST) THEN		!Since we're going high to low,
              S = S + B.DIGIT(L)			!We add to prior work as we go back up.
             ELSE				!Or else (once, sigh), extending.
              B.LAST = L				!Because the high-order product carried up one.
              IF (L.GE.BIGLIMIT) STOP "BIGSQUARE has overflowed!"	!Perhaps too far.
            END IF			!Righto, S is ready.
            B.DIGIT(L) = MOD(S,BIGBASE)	!Place the digit.
            S = S/BIGBASE		!Drop a power as we climb to a still higher digit.
          END DO			!This may be many powers large for big summations.
Contemplate the parallelogram.
          IF (FB.GT.1) THEN		!The topmost term of a column
            FB = FB - 1			!Starts with this second digit of each product pair.
          ELSE IF (FA.GT.1) THEN	!But after the units is reached,
            FA = FA - 1			!The column starts with lesser first digits.
          ELSE				!And when the units first digit has been reached,
            RETURN			!We're done.
          END IF			!So much for the start elements of the parallelogram.
          GO TO 10			!Peruse the diagram.
        END SUBROUTINE BIGSQUARE	!Not the Primary School order, but equivalent.

        INTEGER FUNCTION BIGMOD2(B)	!B mod 2.
         TYPE(BIGNUM) B		!The number.
Compilers ought to notice that BIGBASE is a constant, so MOD(BIGBASE,2).EQ.0 is also constant, and not generate pointless code...
Could also hope that MOD 2 will be effected via an "and" on binary computers.
          IF (MOD(BIGBASE,2).EQ.0) THEN	!If the base is even,
            BIGMOD2 = MOD(B.DIGIT(1),2)		!Only the units digit need be considered.
           ELSE				!But for odd bases, each digit must be inspected.
            BIGMOD2 = MOD(COUNT(MOD(B.DIGIT(1:B.LAST),2) .EQ. 1),2)	!Whee!
          END IF			!That was fun!
        END FUNCTION BIGMOD2	!This should be swifter than BIGMODN(B,2)

        INTEGER FUNCTION BIGMODN(B,N)	!Calculate MOD(B,N), for ordinary N.
Causes 32-bit overflow if any C*BIGBASE + DIGIT(i) exceeds the 32-bit integer limit.
         TYPE(BIGNUM) B	!The big number, presumed positive.
         INTEGER N	!The divisor, presumed positive.
         INTEGER I	!The stepper.
         INTEGER C	!The carry. Values are in 0:N - 1.
          IF (N.LE.0) STOP "BIGMODN: positive divisors only!"	!Grr.
C         IF (N.GT.46340) STOP "BIGMODN: N limited to 46340!"	!If INT8, etc. is unavailable.
          C = 0			!Here we go.                      carry          +  digit
          IF (N.GE.HUGE(C)/BIGBASE) THEN	!Maximum term is (N - 1)*BIGBASE + (BIGBASE - 1).
            DO I = B.LAST,1,-1			!So, N*BIGBASE - 1 must not exceed HUGE(C).
              C = MOD(C*INT8(BIGBASE) + B.DIGIT(I),N)	!Or, N*BIGBASE .GT. HUGE(C) + 1.
            END DO		!Next digit down.	!Or, N         .GT. HUGE(C)/BIGBASE + 1/BIGBASE
           ELSE		!Thus, if N is not so big, no escalation is needed.
            DO I = B.LAST,1,-1	!From the highest-order digit, downwards.
              C = MOD(C*BIGBASE + B.DIGIT(I),N)	!Whee!
            END DO		!Next digit down.
          END IF	!If INTEGER*8 is not available, then a proper BIGMODN is needed.
          BIGMODN = C	!The remainder.
        END FUNCTION BIGMODN	!A simple idea, made complex.

        SUBROUTINE BIGMOD(B,M)	!B:=B mod M.
c  Clear? Hah! Why a four year old child could understand this.
c  Run out and find me a four year old child, I can't make head or tail out of it.
C          Groucho Marx as Rufus T. Firefly in Duck Soup. 18min.
         TYPE(BIGNUM) B	!The number, and the result.
         TYPE(BIGNUM) M	!The modulus.
         INTEGER L	!Location in B of M's high-order digit.
         INTEGER IB,IM	!Fingers to the digits of B and M.
         INTEGER Q,P	!Quotient and provisional reversal.
         INTEGER C	!Carry.
         INTEGER*8 DD,MM!Working numbers. BIGBASE might be large...
          IF (HUGE(DD).LT.FLOAT(BIGBASE - 1)**3) STOP "BIGBASE too big!"	!Triple digit size.
Could be easy...
          IF (M.LAST.LE.1) THEN	!Perhaps a single-digit divisor?
            B.DIGIT(1) = BIGMODN(B,M.DIGIT(1))	!Yes! This is much easier.
            B.LAST = 1				!A single-digit result.
            RETURN			!Done. Digits above B.LAST should not be referenced.
          END IF		!Otherwise, the fun begins. By here, know that M.LAST > 1.
          MM = M.DIGIT(M.LAST)*INT8(BIGBASE) + M.DIGIT(M.LAST - 1)	!Min. is BIGBASE, = 10 in every base.
Could still be easy...
    1     IF (BIGSIGN(B,M)) 3,2,10	!Sign of B - M.
    2     B.LAST = 1			!B = M.
          B.DIGIT(1) = 0		!Unlikely, but the result is clear.
    3     RETURN			!B < M: no change.
Can't evade the calculation via detection of silly parameters. Now know that B > M, and so B also has at least two digits.
   10     L = B.LAST		!Align M to the high-order digit of B.
          DD = B.DIGIT(L)*INT8(BIGBASE) + B.DIGIT(L - 1)	!Combine the top two digits of B. Max. is (b - 1)*b + (b - 1) = b² - 1.
          Q = DD/MM	!Estimate the quotient. Max. is BIGBASE - 1, because MM is two digits. Max is (max of DD)/(min of MM; b) = b - 1/b.
          IF (Q.LE.0) THEN		!E.g. in b = 10, 100 mod 11: 10/11 gives Q = 0. Thus know DD < MM.
            L = L - 1			!Move M one over. Must be possible, because B > M.
            Q = (DD*BIGBASE + B.DIGIT(L - 1))/MM	!But this requires three-digit arithmetic.
          END IF		!A quotient has been chosen.
Calculate B = B - Q*M, with M aligned to the high order of B via L.
   20     C = 0		!Clear the carry, which for subtraction is a "borrow".
          IB = L - M.LAST + 1	!Finger the lowest-order in B that is aligned with M's lowest order.
          DO IM = 1,M.LAST	!Step up through the digits of M.
            DD = C + B.DIGIT(IB) - INT8(Q)*M.DIGIT(IM)	!Subtract Q*M. Q may be BIGBASE - 1 so a double size result.
            IF (DD.LT.0) THEN	!Is a borrow needed?
              C = (DD + 1)/BIGBASE - 1		!Yes. Perhaps many, if Q > 1. C is negative.
              B.DIGIT(IB) = DD - C*BIGBASE	!Borrowing -C lots of BIGBASE produces one digit.
             ELSE		!Otherwise, a positive digit.
              B.DIGIT(IB) = DD		!No need for MOD(DD,BIGBASE).
              C = 0			!Clear any carry.
            END IF		!So much for that digit.
            IB = IB + 1		!Advance one digit up in B.
          END DO		!And also one digit up in M.
          IF (IB.EQ.B.LAST) THEN!If M had been shifted one over,
            DD = C + B.DIGIT(B.LAST)	!The top digit in B has no corresponding Q*M digit
            IF (DD.LT.0) THEN		!A borrow for the topmost digit?
              C = (DD + 1)/BIGBASE - 1		!Yes. This many.
              B.DIGIT(IB) = DD - C*BIGBASE	!Correct the digit.
             ELSE			!But without a borrow,
              B.DIGIT(IB) = DD			!No need for MOD(DD,BIGBASE)
              C = 0				!Clear any previous carry.
            END IF			!So much for the lonely topmost digit.
          END IF		!The carry may not be zero, and if so, upper digits of B are complemented in BIGBASE.
Check for overflow. Q might have been too big, when using M instead of just MM. Hopefully, not by much.
Consider 1005/101 in base 10. DD = 10, MM = 10 so Q = 1 above: proceed. But, subtracting 101 from 100|5 overflows.
Could add back the 101 then shift (and try afresh), or, shift and add back. Quotient is down 10 up 1, thus, down 9.
   30     IF (C.LT.0) THEN	!Has there been a borrow for the highest digit?
   31       C = C*INT8(BIGBASE) + B.DIGIT(L)	!Yes. Reconcile the two.
            IF (C.EQ.-1 .AND. L.GT.M.LAST) THEN	!The borrow came from below?
              L = L - 1				!Yes. Shift M along one, if not already all the way.
              GO TO 31				!And reconcile further. B - Q*M may clear more than one digit.
            END IF			!C is negative, and digit-sized.
            DD = C*INT8(BIGBASE) + B.DIGIT(L - 1)	!The overshoot, a negative number.
            P = 1 - DD/MM	!Convert to multiples of MM, possibly shifted further.
            IB = L - M.LAST + 1	!M's units digit is aligned with this B digit.
            C = 0		!Restart, unworried by the existing overflow..
            DO IM = 1,M.LAST	!Step through the digits of M.
              DD = C + B.DIGIT(IB) + INT8(P)*M.DIGIT(IM)	!Add in P*M.
              B.DIGIT(IB) = MOD(DD,BIGBASE)	!Place the digit.
              C = DD/BIGBASE			!Another divide for the carry.
              IB = IB + 1	!Step up one digit in B
            END DO		!And also one digit in M.
            DO IB = IB,B.LAST	!Convert any higher inverted digits.
              C = C + B.DIGIT(IB)	!C is one, and an inverted digit = (BIGBASE - 1).
              B.DIGIT(IB) = MOD(C,BIGBASE)	!So, this will be zero.
              C = C/BIGBASE		!And C will be one again.
            END DO		!All the way to the top of the number.
          END IF		!So much for any overflow.
Contemplate the result.
          CALL BIGNORM(B)	!Hopefully, many leading zero digits have appeared...
          IF (B.LAST.GT.M.LAST) GO TO 10!If B has more digits, there is definitely more to do.
          GO TO 1		!Possibly, B <= M.
        END SUBROUTINE BIGMOD	!Memories of a hand-cranked adding machine.

        INTEGER FUNCTION BIGDIVRN(B,N)	!B:=B/N; but, returns the remainder too.
         TYPE(BIGNUM) B	!The big number, presumed positive, to be divided by N..
         INTEGER N	!The divisor, presumed positive.
         INTEGER I	!The stepper.
         INTEGER R	!The remainder. Values are in 0:N - 1.
         INTEGER D	!A double digit, if N is not too big..
         INTEGER*8 DD	!For use with the larger values of N.
c         IF (N.GT.46340) STOP "BIGDIVRN: N limited to 46340!"	!If INT8, etc. is unavailable.
          R = 0		!Here we go.
          IF (N.GE.HUGE(D)/BIGBASE) THEN!Edging towards multi-precision arithmetic?
            DO I = B.LAST,1,-1	!Step down the digits.
              DD = R*INT8(BIGBASE) + B.DIGIT(I)	!Form the current digit.
              B.DIGIT(I) = DD/N		!Place it.
              R = MOD(DD,N)		!Remainder = R - B.DIGIT(I)*N, in primary school.
            END DO		!On to the next digit.
           ELSE		!If a remainder*BIGBASE + digit can fit into D, no need for DD.
            DO I = B.LAST,1,-1	!Step down the digits.
              D = R*BIGBASE + B.DIGIT(I)!Form the current digit.
              B.DIGIT(I) = D/N		!Place it.
              R = MOD(D,N)		!Remainder = D - B.DIGIT(I)*N, in primary school.
            END DO		!On to the next digit.
          END IF		!In larger or smaller gulps.
          CALL BIGNORM(B)	!Trim any leading zero digits.
          BIGDIVRN = R		!Pass back the remainder as well.
        END FUNCTION BIGDIVRN	!A pity that two divides per step can't be dodged outside assembler.

        SUBROUTINE BIGDIVN(B,N)	!B:=B div N	!Divide a big number by an ordinary number.
         TYPE(BIGNUM) B	!The big number to divide.
         INTEGER N	!The divisor, an ordinary number.
         INTEGER D,R	!Scratchpads for the calculation.
         INTEGER*8 DD	!A double-size digit is available...
         INTEGER I	!A stepper.
          IF (N.LE.0) STOP "BIGDIVN: positive divisors only!"	!Grr.
C         IF (N.GT.46340) STOP "BIGDIVN: N limited to 46340!"	!If INT8, etc. is unavailable.
          IF (N.EQ.1) GO TO 10	!Nothing need be done, but normalisation might be good.
          R = 0		!No higher-order remainder.
          IF (N.GE.HUGE(D)/BIGBASE) THEN!Edging towards multi-precision arithmetic?
            DO I = B.LAST,1,-1		!Starting at the high-order end and working down.
              DD = R*INT8(BIGBASE) + B.DIGIT(I)	!A term, double-digit size.
              B.DIGIT(I) = DD/N			!Divide, losing any remainder.
              R = MOD(DD,N)			!Max. remainder is N - 1.
            END DO			!On to the next digit, multiplying the remainder by BIGBASE.
           ELSE		!With N not large, (N - 1)*BIGBASE + DIGIT won't overflow.
            DO I = B.LAST,1,-1		!Starting at the high-order end and working down.
              D = R*BIGBASE + B.DIGIT(I)	!A term, double-digit size being not too big for D.
              B.DIGIT(I) = D/N			!Divide, losing any remainder.
              R = MOD(D,N)			!Recover the remainder.
            END DO			!On to the next digit.
          END IF	!Either way, this is as taught in primary school, except there R = D - B.DIGIT(I)*N.
   10     CALL BIGNORM(B)	!Check for leading zero digits, since B will have been reduced.
        END SUBROUTINE BIGDIVN	!The remainder could be returned as well.

        INTEGER FUNCTION MODEXP(N,X,P)	!Calculate X**P mod N without overflowing...
C  Relies on a.b mod n = (a mod n)(b mod n) mod n
         INTEGER N,X,P	!All presumed positive, and X < N.
         INTEGER I	!A stepper.
         INTEGER*8 V,W	!Broad scratchpads, otherwise N > 46340 may incur overflow in 32-bit.
          V = 1		!=X**0
          IF (P.GT.0) THEN	!Something to do?
            I = P			!Yes. Get a copy I can mess with.
            W = X			!=X**1, X**2, X**4, X**8, ... except, all are mod N.
    1       IF (MOD(I,2).EQ.1) V = MOD(V*W,N)	!Incorporate W if the low-end calls for it.
            I = I/2			!Used. Shift the next one down.
            IF (I.GT.0) THEN		!Still something to do?
              W = MOD(W**2,N)			!Yes. Square W ready for the next bit up.
              GO TO 1				!Consider it.
            END IF				!Don't square W if nothing remains. It might overflow.
          END IF		!Negative powers are ignored.
          MODEXP = V		!Done, in lb(P) iterations!
        END FUNCTION MODEXP	!"Bit" presence by arithmetic: works for non-binary arithmetic too.

        SUBROUTINE BIGMODEXP(V,N,X,P)	!Calculate V = X**P mod N without overflowing...
C  Relies on a.b mod n = (a mod n)(b mod n) mod n
         TYPE(BIGNUM) V,N,X,P	!All presumed positive.
         TYPE(BIGNUM) I		!A stepper.
         TYPE(BIGNUM) W		!Broad scratchpads, otherwise N > 46340 may incur overflow in 32-bit.
          CALL BIGLOADN(V,1)		!=X**0
          IF (P.LAST.GT.1 .OR. P.DIGIT(1).GT.0) THEN	!Something to do?
            I.LAST = P.LAST				!Yes. Get a copy I can mess with.
            I.DIGIT(1:I.LAST) = P.DIGIT(1:P.LAST)	!Only copying the digits in use.
            W.LAST = X.LAST				!=X**1, X**2, X**4, X**8, ... except, all are mod N.
            W.DIGIT(1:W.LAST) = X.DIGIT(1:X.LAST)	!Used according to the bits in P.
    1       IF (BIGMOD2(I).EQ.1) THEN	!Incorporate W if the low-end calls for it.
              CALL BIGMULT(V,W)			!V:=V*W ...
              CALL BIGMOD(V,N)			!   ... mod N.
            END IF			!So much for that bit.
            CALL BIGDIVN(I,2)		!Used. Shift the next one down.
            IF (I.LAST.GT.1 .OR. I.DIGIT(1).GT.0) THEN	!Still something to do?
              CALL BIGSQUARE(W)			!Yes. Square W ready for the next bit up.
              CALL BIGMOD(W,N)			!Reduced modulo N.
              GO TO 1				!Consider it.
            END IF			!Don't square W if nothing remains. A waste of effort.
          END IF		!Negative powers are ignored.
        END SUBROUTINE BIGMODEXP	!"Bit" presence by arithmetic: works for non-binary arithmetic too.

        LOGICAL FUNCTION BIGMRPRIME(N,TRIALS)	!Could N be a prime number?
         USE DFPORT	!To get RAND, which returns a "random" number in 0.0 to 1.0. Inclusive.
         TYPE(BIGNUM) N		!The number to taste.
         INTEGER TRIALS		!The count of trials to make.
         INTEGER S		!Counts powers of two in N - 1.
         TYPE(BIGNUM) NL1	!Holds N - 1 for comparisons.
         TYPE(BIGNUM) D		!N - 1 with twos divided out.
         TYPE(BIGNUM) A		!A number in [2:N - 1]. Any number...
         TYPE(BIGNUM) X		!Scratchpad.
         INTEGER A1,N1		!Assistants for juggling a "random" number.
         INTEGER TRIAL,R	!Counters.
          IF (BIGBASE.LE.3) STOP "BIGMRPRIME: BigBase too small!"	!Multi-digit even for small numbers!
Catch some annoying cases.
          IF (N.LAST.EQ.1) THEN	!A single-digit number?
            IF (N.DIGIT(1).LE.4) THEN	!Yes. Some special values are known.
              BIGMRPRIME = N.DIGIT(1).GE.2 .AND. N.DIGIT(1).LE.3	!Like, the neighbours.
              RETURN		!Thus allow 2 to be reported as prime.
            END IF		!Yet, test for 2 as a possible factor for larger numbers.
          END IF		!Without struggling over SQRT and suchlike.
          BIGMRPRIME = .FALSE.	!Most numbers are not primes.
          IF (BIGMOD2(N).EQ.0) RETURN	!A single expression using .OR. risks always evaluating BOTH parts, damnit,
          IF (BIGMODN(N,3).EQ.0) RETURN	!Even for even numbers. Possibly doing so "in parallel" is no consolation.
Construct D such that N - 1 = D*2**S. By here, N is odd, and greater than three.
          D.LAST = N.LAST	!Could just put D = N,
          D.DIGIT(1:D.LAST) = N.DIGIT(1:D.LAST)	!But this copies only the digits in use.
          CALL BIGADDN(D,-1)	!Thus, D becomes an even number.
          NL1.LAST = D.LAST	!For later testing of X against N - 1,
          NL1.DIGIT(1:NL1.LAST) = D.DIGIT(1:D.LAST)	!Retain N - 1.
          N1 = MIN(20000000D0,BIGVALUE(NL1))	!Maximum value is N - 1.
          S = 1			!Since D is even, it has at least one power of two.
   10     CALL BIGDIVN(D,2)	!Divide out a power of two.
          IF (BIGMOD2(D).EQ.0) THEN	!If there is another,
            S = S + 1			!Count it,
            GO TO 10			!And divide it out also.
          END IF		!So, D is no longer even. N - 1 = D*2**S
Convince through repetition...
        T:DO TRIAL = 1,TRIALS	!Some trials come to a definite result.
            BIGMRCOUNT(TRIAL) = BIGMRCOUNT(TRIAL) + 1	!
            A1 = RAND(0)*(N1 - 2) + 2	!For small N, the birthday problem. NB! RAND can generate 1.
            CALL BIGLOADN(A,A1)		!A1 is in (0 + 2) = 2 to N - 1 = (1*(N1 - 2) + 2).
            CALL BIGMODEXP(X,N,A,D)		!X = A**D mod N.
            IF (X.LAST.EQ.1 .AND. X.DIGIT(1).EQ.1) CYCLE T	!Pox. A prime yields these: 1 or NL1.
            IF (BIGSIGN(X,NL1).EQ.0) CYCLE T	!A test with .OR. might always evaluate both, damnit.
            DO R = 1,S - 1	!Step through the powers of two in N - 1.
              CALL BIGSQUARE(X)		!X**2 ...
              CALL BIGMOD(X,N)		! ... mod N
              IF (X.LAST.EQ.1 .AND. X.DIGIT(1).EQ.1) RETURN	!X = 1? Definitely composite. No prime does this.
              IF (BIGSIGN(X,NL1).EQ.0) CYCLE T	!Pox. Try something else.
            END DO		!Another power of two?
            RETURN		!Definitely composite.
          END DO T		!Have another try.
          BIGMRPRIME = .TRUE.	!Would further trials yield greater assurance?
        END FUNCTION BIGMRPRIME	!Are some numbers resistant to this scheme?

        INTEGER FUNCTION BIGFACTOR(B,LIMIT)	!Find the first factor of B. Limited assault.
Careful. The 32-bit integer limit is 2,147,483,647. P(1358124) = 21,474,829 and BIGBASE might be 100.
         USE PRIMEBAG	!I want a supply.
         TYPE(BIGNUM) B	!The number to factorise.
         INTEGER LIMIT	!A limit on the search.
         REAL*8 S	!The square root of B.
         INTEGER F	!A candidate factor.
          S = SQRT(BIGVALUE(B))	!Pious hope for accuracy...
          F = 2		!Here we go.
          DO WHILE(F.LE.S)	!Thus avoid declaring 2 to have a factor of 2.
            IF (BIGMODN(B,F).EQ.0) THEN	!Divisible?
              BIGFACTOR = F		!Yes!
              RETURN			!Done.
            END IF		!But otherwise,
            F = NEXTPRIME(F)	!Find the next prime number.
            IF (F.LE.0 .OR. F.GT.LIMIT) THEN	!Overflow? Or, too far already?
              BIGFACTOR = 0		!Alas. Sez: Don't know.
c              WRITE (6,1) LIMIT,F,S	!Make a report.
    1         FORMAT ("Passed the limit of ",I0," with ",I0,
     1         ", but Sqrt(n) =",E16.8)
              IF (BIGMRPRIME(B,BIGMRTRIALS)) THEN	!Crank up a Miller-Rabin probe.
                BIGFACTOR = -1		!Non-priminess not observed in this probe.
               ELSE			!Or possibly,
                BIGFACTOR = -2		!Non-primeness definitely observed.
              END IF			!That was mysterious.
              RETURN			!Pass the word..
            END IF		!But otherwise,
          END DO	!Consider the next factor.
          BIGFACTOR = 1	!If we pass SQRT(B), B is definitely prime.
        END FUNCTION BIGFACTOR	!The first factor suffices.

      END MODULE BIGNUMBERS	!No fancy tricks.

```



### ==Use==

With all that in hand, big numbers can be manipulated, and function BIGFACTOR can be used where others use something like IsProbablyPrime. So the task is solvable via 
```Fortran
      PROGRAM PRIMORIALP	!Simple enough, with some assistants.
      USE PRIMEBAG		!Some prime numbers are wanted.
      USE BIGNUMBERS		!Just so.
      TYPE(BIGNUM) B		!I'll have one.
      INTEGER MAXF		!Largest factor to consider by direct division.
      PARAMETER (MAXF = 1000000)	!Some determination.
      INTEGER I,L,P			!Step stuff.
      INTEGER FU,FD,FUD(2)	!Found factors.
      EQUIVALENCE (FUD(1),FD),(FUD(2),FU)	!A struggle to prepare an array.
      INTEGER NHIT,HIT(666)	!A little list.
      CHARACTER*4 WOT		!A remark.
      CHARACTER*66 ALINE	!A scratchpad.
      REAL T0,T1		!In memory of lost time.
      MSG = 6	!Standard output.
      WRITE (MSG,1) BIGLIMIT,BIGBASE,HUGE(P),HUGE(INT8(P)),MAXF	!Announce.
    1 FORMAT ('Calculates primorial "primes"',/,
     1 "A primorial prime is a value N such that",/,
     2 "    Primorial(N) - 1 is prime, OR",/,
     3 "    Primorial(N) + 1 is prime, or both.",/,
     4 "and Primorial(N) is the product of the first N prime numbers.",/
     5 "Working with up to ",I0," digits in base ",I0,"."/
     6 "Integer limits are ",I0," and ",I0/
     7 "Primes up to ",I0," are tried as possible factors.")

      IF (.NOT.GRASPPRIMEBAG(66)) STOP "Gan't grab my file!"	!Attempt in hope.
      WRITE (MSG,2)
    2 FORMAT (34X,"First     First"/,
     1 "Primorial#",5X,"Approx.",7X," -1 Factor +1 Factor Hit")

Commence prime mashing.
  100 NHIT = 0		!My list is empty.
      B.LAST = 1	!Begin at the beginning.
      B.DIGIT(1) = 1	!With one. The same, whatever BIGBASE.
      CALL CPU_TIME(T0)	!Start the timing.
      DO P = 1,80 !460	!Step along the primorials. 457 is the 20'th.
        CALL BIGMULTN(B,PRIME(P))	!Multiply by the next prime.
c        WRITE (MSG,101) P,PRIME(P),P,B.DIGIT(B.LAST:1:-1)	!Digits in Arabic/Hindu order.
  101   FORMAT ("Prime(",I0,") = ",I0,", Primorial(",I0,") = ",	!For a possibly multi-BIGBASE sequence.
     1   I0,29I<BIGORDER>.<BIGORDER>,/,(10I<BIGORDER>.<BIGORDER>))	!The first without leading zero digits.
        FU = 0		!No factor for up one.
        FD = 0		!No factor for down one.
        CALL BIGADDN(B,+1)	!Go up one.
        FU = BIGFACTOR(B,MAXF)	!Find a factor, maybe.
        CALL BIGADDN(B,-2)	!Now test down one.
        IF (ABS(FU).NE.1) FD = BIGFACTOR(B,MAXF)	!But only if FU didn't report "prime".
        IF (ABS(FU).EQ.1 .OR. ABS(FD).EQ.1) THEN	!Since if either candidate is a prime,
          WOT = "Yes!"				!Then a hit.
          NHIT = NHIT + 1			!So count up a success.
          HIT(NHIT) = P				!And append to my list.
        ELSE IF (ABS(FU).GT.1 .AND. ABS(FD).GT.1) THEN	!But if both have factors,
          WOT = "No."				!Then definitely not a hit.
        ELSE				!Otherwise,
          WOT = "?"				!I can't decide.
        END IF				!So much for that candidate.
        CALL BIGADDN(B,1)	!Recover the original primorial value.
        CALL BIGTASTE(B)	!Prepare an approximate value in BIGLEAD.
        WRITE (ALINE,102) P,BIGLEAD,FD,FU,WOT	!Prepare a report.
  102   FORMAT (I10,"  0.",<BIGLEADN>I0,T24,"E+",I0,T30,I10,I10,1X,A)	!A table.
        DO I = 1,2	!Two columns to the table.
          L = 20 + I*10		!Each with the same interpretation.
          SELECT CASE(FUD(I))	!So, one set of code.
           CASE(-2); ALINE(L:L+9) = " Composite"
           CASE(-1); ALINE(L:L+9) = "  MR prime"
           CASE( 0); ALINE(L+9:L+9) = "?"     	!Don't know. Didn't look.
           CASE DEFAULT		!All other values stay. The first factor.
          END SELECT		!So much for re-interpretation.
        END DO		!On to the next column.
        WRITE (MSG,"(A)") ALINE		!Show the report.
      END DO		!On to the next prime.
      CALL CPU_TIME(T1)	!Completed the run.

Cast forth some pearls.
  200 WRITE (MSG,201)	!First, some statistics.
  201 FORMAT (/,"The MR prime test makes a series of trials, "
     1 "stopping early",/'only when a "definitely composite" ',
     2 "result is encountered.")
      WRITE (MSG,202) "Trial",(I,I = 1,BIGMRTRIALS)	!Roll the trial number.
      WRITE (MSG,202) "Count",BIGMRCOUNT		!Now the counts.
  202 FORMAT (A6,": ",666I6)	!This should do.
      WRITE (MSG,203) NHIT,HIT(1:NHIT)	!The list of primorial "primes"..
  203 FORMAT (/,I0," in the hit list: ",I0,666(",",I0:))	!Don't actually expect so many.
      WRITE (MSG,*) "CPU time:",T1 - T0	!The cost.
      END	!So much for that.

```

A slightly more complex scheme to interpret the more complex mumbling from BIGFACTOR, which is less sure of its reports in more ways.


### =Output=

A somewhat lesser upper bound for factoring leads to a faster run.

```txt

Calculates primorial "primes"
A primorial prime is a value N such that
    Primorial(N) - 1 is prime, OR
    Primorial(N) + 1 is prime, or both.
and Primorial(N) is the product of the first N prime numbers.
Working with up to 2222 digits in base 10000.
Integer limits are 2147483647 and 9223372036854775807
Primes up to 1000000 are tried as possible factors.
                                  First     First
Primorial#     Approx.        -1 Factor +1 Factor Hit
         1  0.20       E+1            ?         1 Yes!
         2  0.60       E+1            ?         1 Yes!
         3  0.300      E+2            ?         1 Yes!
         4  0.2100     E+3            ?         1 Yes!
         5  0.23100    E+4            ?         1 Yes!
         6  0.330      E+5            1        59 Yes!
         7  0.51510    E+6           61        19 No.
         8  0.9699690  E+7           53       347 No.
         9  0.22309    E+9           37       317 No.
        10  0.646969   E+10          79       331 No.
        11  0.20056049 E+12           ?         1 Yes!
        12  0.74207    E+13         229       181 No.
        13  0.3042503  E+15    MR prime        61 Yes!
        14  0.13083    E+17      141269       167 No.
        15  0.614890   E+18         191       953 No.
        16  0.32589158 E+20       87337        73 No.
        17  0.192276   E+22   Composite       277 No.
        18  0.11728838 E+24        1193       223 No.
        19  0.78583    E+25         163 Composite No.
        20  0.5579408  E+27   Composite      1063 No.
        21  0.4730     E+29         313      2521 No.
        22  0.3217645  E+31         163     22093 No.
        23  0.26706    E+33         139    265739 No.
        24  0.2376874  E+35    MR prime       131 Yes!
        25  0.23056    E+37       66683 Composite No.
        26  0.2328624  E+39   Composite    960703 No.
        27  0.23985    E+41       15649      2297 No.
        28  0.2566376  E+43   Composite       149 No.
        29  0.27973    E+45         719    334507 No.
        30  0.3161005  E+47      295201 Composite No.
        31  0.4145     E+49   Composite      1543 No.
        32  0.5258965  E+51   Composite      1951 No.
        33  0.72048    E+53   Composite       881 No.
        34  0.10014647 E+56        4871 Composite No.
        35  0.149218   E+58         673 Composite No.
        36  0.22531953 E+60         311 Composite No.
        37  0.353752   E+62        1409      1381 No.
        38  0.57661522 E+64        1291      1361 No.
        39  0.962947   E+66         331 Composite No.
        40  0.16659    E+69   Composite Composite No.
        41  0.2981959  E+71       23497       601 No.
        42  0.53973    E+73      711427    107453 No.
        43  0.10308931 E+76         521     32999 No.
        44  0.198962   E+78         673 Composite No.
        45  0.39195588 E+80      519577    521831 No.
        46  0.779992   E+82   Composite       467 No.
        47  0.16458    E+85       56543      1051 No.
        48  0.36797    E+87         811 Composite No.
        49  0.83311    E+89      182309      3187 No.
        50  0.19078267 E+92       53077    126173 No.
        51  0.444524   E+94         641      5981 No.
        52  0.1624     E+97         349       313 No.
        53  0.256412   E+99         389      3499 No.
        54  0.64266    E+101     565921 Composite No.
        55  0.16516447 E+104     777041     34259 No.
        56  0.434383   E+106  Composite       271 No.
        57  0.11685    E+109        757     17291 No.
        58  0.3166605  E+111       2341     15559 No.
        59  0.87715    E+113      44753    243233 No.
        60  0.24647906 E+116  Composite     62131 No.
        61  0.697536   E+118       7027       307 No.
        62  0.2438     E+121       4111 Composite No.
        63  0.6274404  E+123       1571 Composite No.
        64  0.195134   E+126  Composite Composite No.
        65  0.61076929 E+128  Composite      3761 No.
        66  0.1936139  E+131   MR prime Composite Yes!
        67  0.64086    E+133  Composite      8089 No.
        68  0.21597046 E+136   MR prime     38453 Yes!
        69  0.749417   E+138       1609      1579 No.
        70  0.26155    E+141      33409     44953 No.
        71  0.9232599  E+143  Composite      2143 No.
        72  0.331450   E+146       1069 Composite No.
        73  0.12164    E+149  Composite Composite No.
        74  0.4537256  E+151  Composite      4973 No.
        75  0.171962   E+154          ?  MR prime Yes!
        76  0.65861450 E+156       5407 Composite No.
        77  0.2562010  E+159     104707    216401 No.
        78  0.101712   E+162        599      8629 No.
        79  0.40786437 E+164       4139    356647 No.
        80  0.1668165  E+167      35447 Composite No.

The MR prime test makes a series of trials, stopping early
only when a "definitely composite" result is encountered.
 Trial:      1     2     3     4     5     6
 Count:     41     5     5     5     5     5

12 in the hit list: 1,2,3,4,5,6,11,13,24,66,68,75
 CPU time:   4.125000

```

Function BIGFACTOR calls upon BIGMRPRIME to make six trials. Somewhere I saw a note that a MR test might fail to denounce a "random" composite number about a quarter of the time, so, up to six trials perhaps reduces this to less than one in a thousand. Except that none of the tested numbers can be considered as a "random" number! They have special properties! Being one off from a number with a sequence of prime factors, a primorial number, and note that none of the values tested turn out to have a factor of two, or three, or five, when "on average", surely half the "random" numbers would be divisible by two, a third by three, etc. But, being one away from Primorial N, a number which ''is'' a multiple of two, three, five, ''etc.'' means that they can't be divisible by two, three, five, ''etc.'' until a possible factor exceeds the N'th prime.

Evidently, a candidate is denounced as definitely composite in the first trial, at least for these candidates. The following trials seem wasted effort...

Different-size bases have an effect, though not a regular one.
 Base      10    100   1000  10000 100000 1000000
 Time   24.55   8.20   4.45   4.11   3.48    2.92 Using 64-bit integers for digit products..
        11.17   4.09   2.55                       Using 32-bit integers throughout.
For software compatibility reasons too irritating to recount, this system runs 32-bit Windows XP even though its AMD Six-Core processor is 64-bit (and when running GNU-Linux, uses 64-bit) thus the INTEGER*8 variables used to handle double-digit products (and even triple-digit products) have their arithmetic done via software, not hardware. While it is amusing to have software for multi-precision integer arithmetic supported via software for double-precision integer arithmetic, this comes at a cost! Advantage is to be found in using the available hardware to its fullest, and not beyond. There seems to be no attempt to use the 64-bit facilities of the floating-point arithmetic unit.

Converting from BIGDIVN(B,2) to BIGDIV2(B) had very little effect on the speed, but inspection of the code demonstrated that the Compaq compiler carried forward the result of expressions involving constants such as <code>IF (MOD(BIGBASE,2).EQ.0) THEN</code> at compile time so that no run-time test was made, and "dead code" for the ELSE option was not generated. Further, MOD(N,2) generated code involving an '''and''', so there is no need to mess about by coding an AND or fiddling with special syntax that would only work on a binary computer. With such a compiler, a sort of "conditional compilation" is available without needing the often grotesque syntax of systems explicitly offering such facilities.

Replacing BIGMULT(B,B) by BIGSQUARE(B) converted ~4·28 seconds to ~4·14 seconds for the test run; three percent.


### Stage three: faster?


### =The plan=

Already, the formula translation of ''A<sup>B</sup> mod N'' into <code>MOD(A**B,N)</code> has first been abandoned for "bignumber" arithmetic to encompass numbers far beyond ordinary integer arithmetic, and secondly, the  ''mod'' operation has been pushed into the evaluation of ''A<sup>B</sup>'' because otherwise the value would increase to a size beyond any possible storage capacity since the variables might be hundred-digit numbers and more. A third push is possible: into the evaluation of multiply itself.

Suppose for specificity, all three numbers are four digits long and A*B is to be calculated. The scheme followed is to produce the product via BIGMULT, then reduce it via BIGMOD. Thus an eight-digit number is produced by the multiply and after the ''mod'' it is reduced to a four-digit number. In positional notation the product is X<sub>8</sub>X<sub>7</sub>X<sub>6</sub>X<sub>5</sub>X<sub>4</sub>X<sub>3</sub>X<sub>2</sub>X<sub>1</sub>, which is of course for base b a summation of X<sub>8</sub>b<sup>7</sup> + X<sub>7</sub>b<sup>6</sup> + ''etc''. and it would be possible to apply the ''mod'' to high-order digits separately. Imagine a table of the values ''of b<sup>i</sup> mod N'' for the digits above four. This table need have only the same four digits as N, and, given that A and B are already reduced modulo N, it need only extend as far as to handle an eight-digit number. There need be no entries for numbers up to four digits. Once prepared, it can be used for all multiplications involving modulo N.

Referring to the template multiply presented in BIGMULT, once a column sum S is prepared, it will be placed in the result if it is for the first four digits, and for higher digits, the result will be augmented by <code>MOD(S,BIGNUM)*REM(L)</code> via subroutine BADDREM where <code>REM(L)</code> would be the four-digit remainder corresponding to digit L (<code>MOD(BIGBASE**(L - 1),N)</code>, remembering that digit one is the units digits with power zero) and <code>MOD(S,BIGNUM)</code> would normally be the value for <code>DIGIT(L)</code> and <code>S/BIGBASE</code> would be its carry to the next digit up. All this is more convenient to arrange working from the low-order up (as in primary-school multiplication) rather than the high-order end down used by BIGMULT that allows the result to be constructed in-place on top of variable A. In short, a temporary variable is used, which means additional effort in copying the result to replace the value in A. But this temporary variable will not grow much longer than the length of N, a digit or two depending on how many additions are made, but definitely not as far as twice as many digits. There will be no production of an eight-digit number, laboriously reduced by BIGMOD to four digits. There will still be a BIGMOD at the end, but of a much smaller number. This also means that numbers approaching BIGLIMIT in size can still be multiplied by BMODMULT because unlike with BIGMULT there will be no double-size result that would otherwise limit the size of numbers to BIGLIMIT/2, remembering that variable-sized numbers incur a performance penalty.


### =The source=

This requires modification to function BIGMRPRIME, which now invokes revised versions of its bignumber routines and conveniently, is the routine that decides on the value N for the modulus (it is the number whose primality is to be tested) and so is in a position to declare the REM table empty each time it is presented with a new N. Since Algol in the 1960s it has been possible for routines to be defined inside other routines (thus sharing variables) but this facility has only become available to Fortran with F90, if in a rather odd way. As for the REM array, in the past it would be defined as "big enough", but with F90 it is possible for a routine to declare an array of a size determined on entry to the routine. It could be defined as a BIGNUMBER, but the whole point of the exercise is that its numbers need no more digits than are in N, specifically N.LAST digits, rather than BIGLIMIT digits. Aside from avoiding wastage, it is better to keep related items close together in memory. Similarly for the number of entries in the REM table: instead of some large collection, the highest-order value corresponds to 2*N.LAST, and further, the first needed entry is at index N.LAST + 1. In the past, the lower bound of an array in Fortran was fixed at one, and the code could either ignore the wasted entries, or else employ suitable offsets so as to employ them - at the cost of careful thought and checking. But employing such offsets is a simple clerical task, and computers excel at simple clerical tasks, and with F90 can be called upon to do so. The declaration is <code>INTEGER REM(N.LAST,N.LAST + 1:2*N.LAST)</code> and it is because adjacent storage is indexed by the first index, not the last, that the digits of a REM entry are accessed by the first index, not the last. The table is cleared, not by setting all its digits to zero even though <code>REM = 0</code> would be easy, but instead by setting <code>NR = N.LAST</code>, which is one short of the first entry in the REM table, it being known that entries will be produced progressively. 
```Fortran
        LOGICAL FUNCTION BIGMRPRIME(N,TRIALS)	!Could N be a prime number?
         USE DFPORT	!To get RAND, which returns a "random" number in 0.0 to 1.0. Inclusive.
         TYPE(BIGNUM) N		!The number to taste.
         INTEGER TRIALS		!The count of trials to make.
         INTEGER REM(N.LAST,N.LAST + 1:2*N.LAST)	!Table of remainders. See BMODMULT.
         INTEGER NR		!Notes the uppermost entry in the table. See BADDREM.
         INTEGER S		!Counts powers of two in N - 1.
         TYPE(BIGNUM) NL1	!Holds N - 1 for comparisons.
         TYPE(BIGNUM) D		!N - 1 with twos divided out.
         TYPE(BIGNUM) A		!A number in [2:N - 1]. Any number...
         TYPE(BIGNUM) X		!Scratchpad.
         INTEGER A1,N1		!Assistants for juggling a "random" number.
         INTEGER TRIAL,R	!Counters.
          IF (BIGBASE.LE.3) STOP "BIGMRPRIME: BigBase too small!"	!Multi-digit even for small numbers!
Catch some annoying cases.
          IF (N.LAST.EQ.1) THEN	!A single-digit number?
            IF (N.DIGIT(1).LE.4) THEN	!Yes. Some special values are known.
              BIGMRPRIME = N.DIGIT(1).GE.2 .AND. N.DIGIT(1).LE.3	!Like, the neighbours.
              RETURN		!Thus allow 2 to be reported as prime.
            END IF		!Yet, test for 2 as a possible factor for larger numbers.
          END IF		!Without struggling over SQRT and suchlike.
          BIGMRPRIME = .FALSE.	!Most numbers are not primes.
          IF (BIGMOD2(N).EQ.0) RETURN	!A single expression using .OR. risks always evaluating BOTH parts, damnit,
          IF (BIGMODN(N,3).EQ.0) RETURN	!Even for even numbers. Possibly doing so "in parallel" is no consolation.
          NR = N.LAST		!Clear the REM table. One short of the first REM entry.
Construct D such that N - 1 = D*2**S. By here, N is odd, and greater than three.
          D.LAST = N.LAST	!Could just put D = N,
          D.DIGIT(1:D.LAST) = N.DIGIT(1:D.LAST)	!But this copies only the digits in use.
          CALL BIGADDN(D,-1)	!Thus, D becomes an even number.
          NL1.LAST = D.LAST	!For later testing of X against N - 1,
          NL1.DIGIT(1:NL1.LAST) = D.DIGIT(1:D.LAST)	!Retain N - 1.
          N1 = MIN(20000000D0,BIGVALUE(NL1))	!Maximum value is N - 1 for smallish N.
          S = 1			!Since D is even, it has at least one power of two.
   10     CALL BIGDIVN(D,2)	!Divide out a power of two.
          IF (BIGMOD2(D).EQ.0) THEN	!If there is another,
            S = S + 1			!Count it,
            GO TO 10			!And divide it out also.
          END IF		!So, D is no longer even. N - 1 = D*2**S
Convince through repetition...
        T:DO TRIAL = 1,TRIALS	!Some trials come to a definite result.
            BIGMRCOUNT(TRIAL) = BIGMRCOUNT(TRIAL) + 1	!Count the attempts.
            A1 = RAND(0)*(N1 - 2) + 2	!For small N, the birthday problem. NB! RAND can generate 1.
            CALL BIGLOADN(A,A1)		!A1 is in (0 + 2) = 2 to N - 1 = (1*(N1 - 2) + 2).
c            CALL BIGMODEXP(X,N,A,D)	!X = A**D mod N.
            CALL BMODEXP(X,A,D)		!X = A**D mod N.
            IF (X.LAST.EQ.1 .AND. X.DIGIT(1).EQ.1) CYCLE T	!Pox. A prime yields these: 1 or NL1.
            IF (BIGSIGN(X,NL1).EQ.0) CYCLE T	!A test with .OR. might always evaluate both, damnit.
            DO R = 1,S - 1	!Step through the powers of two in N - 1.
              CALL BIGSQUARE(X)		!X**2 ...
              CALL BIGMOD(X,N)		! ... mod N
              IF (X.LAST.EQ.1 .AND. X.DIGIT(1).EQ.1) RETURN	!X = 1? Definitely composite. No prime does this.
              IF (BIGSIGN(X,NL1).EQ.0) CYCLE T	!Pox. Try something else.
            END DO		!Another power of two?
            RETURN		!Definitely composite.
          END DO T		!Have another try.
          BIGMRPRIME = .TRUE.	!Would further trials yield greater assurance?
         CONTAINS	!Special versions incorporating "mod" not just at the end.
          SUBROUTINE BMODMULT(A,B)	!A:=A*B mod N.
Calculates from the low-order end upwards, thus requiring a scratchpad. But it does not get the full A*B size.
           TYPE(BIGNUM) A,B	!The numbers.
           TYPE(BIGNUM) T	!The product is developed here.
           INTEGER IA,FA,LA	!For A: Index, First (highest order), Last (lowest order).
           INTEGER IB,FB	!For B: Index, First (highest order).
           INTEGER L		!Fingers a digit for a result.
           INTEGER*8 S		!Scratchpad for digit multiply and summation.
            IF ((BIGBASE - 1)*B.LAST.GE.HUGE(S)/(BIGBASE - 1))	!Max. digit product, summed,
     1       STOP "BMODMULT: too many B digits! Could overflow S!"	!Rather than only ever one at a time..
            IF (A.LAST + B.LAST .GT. BIGLIMIT + 1)	!The case when the topmost digit doesn't carry up one more.
     1       STOP "BigMult will overflow!"	!Fixed storage sizes.
            FA = 1		!Parallelogram parsing control, starting at the top right corner.
            FB = 1		!These finger the digits of the first column's topmost product.
            LA = 1		!And this the last A-digit of the column's products.
            T.LAST = 1		!Prepare to accumulate column sums.
            T.DIGIT(1) = 0	!Starting with zero.
            L = 0		!No digits have been produced.
Commence producing digits. Work down each column, starting with the low-order side in the school style.
            S = 0			!The grand sum, of many digit products.
   10       IB = FB			!Index of B's digit of the moment.
            DO IA = FA,LA,-1		!Accumulate, working down a column, though bottom to top would do also.
              S = INT8(A.DIGIT(IA))*B.DIGIT(IB) + S	!Another digit product added in.
              IB = IB + 1				!NB: IA + IB is constant in this loop.
            END DO			!It is the digit power, plus two since DIGIT arrays start with one.
            IF (S.LT.0) STOP "BModMult: S has flipped sign!"	!Oh dear. Maybe not.
            L = L + 1			!Another digit is ready.
            IF (L.GT.N.LAST) THEN	!Reached REM territory?
              CALL BADDREM(T,INT4(MOD(S,BIGBASE)),L)	!Yes. Add (digit at L)*remainder(L) to T.
             ELSE		!Otherwise, below N, just place the new digit.
              T.LAST = L			!Keep T in proper form.
              T.DIGIT(L) = MOD(S,BIGBASE)	!Place the digit.
            END IF			!The sum's digit is assimilated.
            S = S/BIGBASE		!The sum's carry to the next digit up.
Contemplate the parallelogram, working right to left...
          IF (FA .LT. A.LAST) THEN	!The topmost term of a column
            FA = FA + 1				!Starts with this A-digit.
          ELSE IF (FB .LT. B.LAST) THEN	!But after the topmost A-digit is reached,
            FB = FB + 1				!The column starts with higher B-digits.
          ELSE				!And when the topmost B-digit has been reached,
            DO WHILE(S > 0)		!We're done. Extend the carry into higher powers.
              L = L + 1				!Up a power, up a digit.
              IF (L.GT.BIGLIMIT) STOP "BModMult has overflowed!"	!Perhaps too far!
              IF (L.GT.N.LAST) THEN		!Higher than N yet?
                CALL BADDREM(T,INT4(MOD(S,BIGBASE)),L)	!High-order digits are reduced.
               ELSE				!But up to N, just place single digits.
                T.LAST = L				!Still in strict sequence.
                T.DIGIT(L) = MOD(S,BIGBASE)		!There being no existing occupant.
              END IF				!So much for that digit.
              S = S/BIGBASE			!Drop a power as we climb to a still higher digit.
            END DO			!Not necessarily one digit worth. It is the sum of many two-digit products.
            CALL BIGMOD(T,N)		!Apply the MOD to clear the rabble...
            A.LAST = T.LAST				!Copy T to A.
            A.DIGIT(1:A.LAST) = T.DIGIT(1:T.LAST)	!Just the digits.
            RETURN				!Escape
          END IF			!So much for the start elements of the parallelogram.
          IF (IB.GT.B.LAST) LA = LA + 1	!The lowest-order A-digit of a column.
          GO TO 10			!Peruse the diagram.
          END SUBROUTINE BMODMULT	!As shown in BIGMULT.
          SUBROUTINE BADDREM(B,A,L)	!B = B + A*REM(L)
           TYPE(BIGNUM) B	!To be augmented.
           INTEGER A		!The sum to be added to B.
           INTEGER L		!At this digit. Presume L > N.LAST.
           TYPE(BIGNUM) T	!Scratchpad.
           INTEGER*8 C		!A carry for the addition.
           INTEGER I		!A stepper.
            IF (L.LE.N.LAST) STOP "BADDREM: digit order confusion!"	!Limited REM coverage.
Could need further entries in my table of mod N remainders for powers of BIGBASE.
            DO WHILE(NR.LT.L)	!If digit L is not encompassed,
              NR = NR + 1		!Step onwards one.
              T.LAST = NR		!Prepare a number having NR digits.
              T.DIGIT(NR) = 1		!Its highest-order digit being one, in any base.
              T.DIGIT(1:NR - 1) = 0	!And all lower digits zero, whatever BIGBASE is.
              CALL BIGMOD(T,N)		!Determine its remainder, mod N.
              REM(1:T.LAST,NR) = T.DIGIT(1:T.LAST)	!Place the digits in my table.
              REM(T.LAST + 1:N.LAST,NR) = 0		!Possible leading zeroes.
            END DO		!And check afresh.
Check that B has enough digits in use to receive a remainder's digits
            IF (N.LAST.GT.B.LAST) THEN	!Save on such checks during each digit of the addition.
              B.DIGIT(B.LAST + 1:N.LAST) = 0	!By placing zero digits once.
              B.LAST = N.LAST		!Not all REM entries use all N.LAST digits either.
            END IF			!Anyway, suspicion rules.
Calculate B = B + A*REM(L), which is A times the remainder for digit L of a big number.
            C = 0		!No carry from a previous digit.
            DO I = 1,N.LAST	!Modulo N means each remainder has N.LAST digits. At most.
              C = INT8(A)*REM(I,L) + B.DIGIT(I) + C	!Some might have high-end zero digits.
              B.DIGIT(I) = MOD(C,BIGBASE)	!But no matter. Crunch them all.
              C = C/BIGBASE			!And carry on.
            END DO		!On to the next digit up
Carry on to higher digits, as needed.
            I = N.LAST	!Not relying on it being N.LAST + 1.
            DO WHILE (C > 0)	!Some more carry?
              I = I + 1			!Yes. Another digit up.
              IF (I.GT.B.LAST) THEN	!Beyond the current spread?
              	IF (I.GT.BIGLIMIT) STOP "BAddRem has overflowed!"	!Perhaps another digit?
                B.LAST = B.LAST + 1		!Yes. Have another.
                B.DIGIT(B.LAST) = MOD(C,BIGBASE)!Knowing B's digit was zero.
               ELSE		!Otherwise, B exists this far up from previous usage.
                C = C + B.DIGIT(I)	!Its digit is unlikely to be zero.
                B.DIGIT(I) = MOD(C,BIGBASE)	!Place the revised digit.
              END IF		!So much for that digit.
              C = C/BIGBASE	!Reduce the carry.
            END DO		!And check afresh.
            CALL BIGNORM(B)	!Cancel any unused high-order zeroes.
          END SUBROUTINE BADDREM!Well, that was confusing.
          SUBROUTINE BMODEXP(V,X,P)	!Calculate V = X**P mod N without overflowing...
C  Relies on a.b mod n = (a mod n)(b mod n) mod n
           TYPE(BIGNUM) V,X,P	!All presumed positive.
           TYPE(BIGNUM) I		!A stepper.
           TYPE(BIGNUM) W		!Broad scratchpads, otherwise N > 46340 may incur overflow in 32-bit.
            CALL BIGLOADN(V,1)		!=X**0
            IF (P.LAST.GT.1 .OR. P.DIGIT(1).GT.0) THEN	!Something to do?
              I.LAST = P.LAST				!Yes. Get a copy I can mess with.
              I.DIGIT(1:I.LAST) = P.DIGIT(1:P.LAST)	!Only copying the digits in use.
              W.LAST = X.LAST				!=X**1, X**2, X**4, X**8, ... except, all are mod N.
              W.DIGIT(1:W.LAST) = X.DIGIT(1:X.LAST)	!Used according to the bits in P.
    1         IF (BIGMOD2(I).EQ.1) THEN	!Incorporate W if the low-end calls for it.
c                CALL BIGMULT(V,W)			!V:=V*W ...
c                CALL BIGMOD(V,N)			!   ... mod N.
                CALL BMODMULT(V,W)			!V:=V*W mod N.
              END IF			!So much for that bit.
              CALL BIGDIVN(I,2)		!Used. Shift the next one down.
              IF (I.LAST.GT.1 .OR. I.DIGIT(1).GT.0) THEN	!Still something to do?
c                CALL BIGSQUARE(W)			!Yes. Square W ready for the next bit up.
c                CALL BIGMOD(W,N)			!Reduced modulo N.
                CALL BMODMULT(W,W)			!W*W mod N.
                GO TO 1				!Consider it.
              END IF			!Don't square W if nothing remains. A waste of effort.
            END IF		!Negative powers are ignored.
          END SUBROUTINE BMODEXP	!"Bit" presence by arithmetic: works for non-binary arithmetic too.
        END FUNCTION BIGMRPRIME	!Are some numbers resistant to this scheme?

```



### =The results=

It was gratifying that the revised code worked first time. Less gratifying was that it ran slower... A test run up to 80 in base 10000 took 4·343 seconds, while without the fancy multiplies it took 4·0 seconds. It is possible that replacing the fancy on-the-fly declaration of the REM array by something fixed and simple would enable faster access as with the tests of a simple array from above, but this is a smallish difference. It would seem that placing the results of a multiply as simple digits (and without copying the result from a work area) for all eight of the result then reducing eight to four via BIGMOD probably using four subtraction stages (each involving four digits and a multiply), takes about the same time as placing four digits, then performing four four-digit additions (each with a multiply), followed by a BIGMOD of a five-digit number in one stage.

In further experiments, activating "maximum optimisations" (and "debugging none") caused a noticeable increase in the compilation time, and the run time went from 4·343 to 6·125. Similarly with the non REM version, from 4·0 to 5·48. Requesting that the (six-fold) Great Internet Mersenne Prime calculation pause reduced that to 3·6.

So, reverting to the non-REM version,

```txt

Calculates primorial "primes"
A primorial prime is a value N such that
    Primorial(N) - 1 is prime, OR
    Primorial(N) + 1 is prime, or both.
and Primorial(N) is the product of the first N prime numbers.
Working with up to 2222 digits in base 10000.
Integer limits are 2147483647 and 9223372036854775807
Primes up to 1000000 are tried as possible factors.
                                  First     First
Primorial#     Approx.        -1 Factor +1 Factor Hit
         1  0.20       E+1            ?         1 Yes!
         2  0.60       E+1            ?         1 Yes!
         3  0.300      E+2            ?         1 Yes!
         4  0.2100     E+3            ?         1 Yes!
         5  0.23100    E+4            ?         1 Yes!
         6  0.330      E+5            1        59 Yes!
         7  0.51510    E+6           61        19 No.
         8  0.9699690  E+7           53       347 No.
         9  0.22309    E+9           37       317 No.
        10  0.646969   E+10          79       331 No.
        11  0.20056049 E+12           ?         1 Yes!
        12  0.74207    E+13         229       181 No.
        13  0.3042503  E+15    MR prime        61 Yes!
        14  0.13083    E+17      141269       167 No.
        15  0.614890   E+18         191       953 No.
        16  0.32589158 E+20       87337        73 No.
        17  0.192276   E+22   Composite       277 No.
        18  0.11728838 E+24        1193       223 No.
        19  0.78583    E+25         163 Composite No.
        20  0.5579408  E+27   Composite      1063 No.
        21  0.4730     E+29         313      2521 No.
        22  0.3217645  E+31         163     22093 No.
        23  0.26706    E+33         139    265739 No.
        24  0.2376874  E+35    MR prime       131 Yes!
        25  0.23056    E+37       66683 Composite No.
        26  0.2328624  E+39   Composite    960703 No.
        27  0.23985    E+41       15649      2297 No.
        28  0.2566376  E+43   Composite       149 No.
        29  0.27973    E+45         719    334507 No.
        30  0.3161005  E+47      295201 Composite No.
        31  0.4145     E+49   Composite      1543 No.
        32  0.5258965  E+51   Composite      1951 No.
        33  0.72048    E+53   Composite       881 No.
        34  0.10014647 E+56        4871 Composite No.
        35  0.149218   E+58         673 Composite No.
        36  0.22531953 E+60         311 Composite No.
        37  0.353752   E+62        1409      1381 No.
        38  0.57661522 E+64        1291      1361 No.
        39  0.962947   E+66         331 Composite No.
        40  0.16659    E+69   Composite Composite No.
        41  0.2981959  E+71       23497       601 No.
        42  0.53973    E+73      711427    107453 No.
        43  0.10308931 E+76         521     32999 No.
        44  0.198962   E+78         673 Composite No.
        45  0.39195588 E+80      519577    521831 No.
        46  0.779992   E+82   Composite       467 No.
        47  0.16458    E+85       56543      1051 No.
        48  0.36797    E+87         811 Composite No.
        49  0.83311    E+89      182309      3187 No.
        50  0.19078267 E+92       53077    126173 No.
        51  0.444524   E+94         641      5981 No.
        52  0.1624     E+97         349       313 No.
        53  0.256412   E+99         389      3499 No.
        54  0.64266    E+101     565921 Composite No.
        55  0.16516447 E+104     777041     34259 No.
        56  0.434383   E+106  Composite       271 No.
        57  0.11685    E+109        757     17291 No.
        58  0.3166605  E+111       2341     15559 No.
        59  0.87715    E+113      44753    243233 No.
        60  0.24647906 E+116  Composite     62131 No.
        61  0.697536   E+118       7027       307 No.
        62  0.2438     E+121       4111 Composite No.
        63  0.6274404  E+123       1571 Composite No.
        64  0.195134   E+126  Composite Composite No.
        65  0.61076929 E+128  Composite      3761 No.
        66  0.1936139  E+131   MR prime Composite Yes!
        67  0.64086    E+133  Composite      8089 No.
        68  0.21597046 E+136   MR prime     38453 Yes!
        69  0.749417   E+138       1609      1579 No.
        70  0.26155    E+141      33409     44953 No.
        71  0.9232599  E+143  Composite      2143 No.
        72  0.331450   E+146       1069 Composite No.
        73  0.12164    E+149  Composite Composite No.
        74  0.4537256  E+151  Composite      4973 No.
        75  0.171962   E+154          ?  MR prime Yes!
        76  0.65861450 E+156       5407 Composite No.
        77  0.2562010  E+159     104707    216401 No.
        78  0.101712   E+162        599      8629 No.
        79  0.40786437 E+164       4139    356647 No.
        80  0.1668165  E+167      35447 Composite No.
        81  0.69896    E+169  Composite Composite No.
        82  0.29426269 E+172        761      2251 No.
        83  0.1268272  E+175  Composite    215983 No.
        84  0.54916    E+177  Composite      2089 No.
        85  0.24108205 E+180      33073      1291 No.
        86  0.1067993  E+183  Composite       463 No.
        87  0.47953    E+185  Composite     10567 No.
        88  0.21914479 E+188  Composite Composite No.
        89  0.101257   E+191  Composite Composite No.
        90  0.46775    E+193        631       983 No.
        91  0.21843888 E+196  Composite Composite No.
        92  0.1046322  E+199       2609 Composite No.
        93  0.5956     E+201     494441     11587 No.
        94  0.25019344 E+204      17929     14419 No.
        95  0.1248465  E+207        569       827 No.
        96  0.62798    E+209  Composite     75883 No.
        97  0.31964081 E+212  Composite      1283 No.
        98  0.1665329  E+215  Composite      3373 No.
        99  0.87097    E+217        563      1069 No.
       100  0.47119308 E+220  Composite      2879 No.
       101  0.2577426  E+223       1777     15823 No.
       102  0.143563   E+226  Composite      1657 No.
       103  0.80825764 E+228       7867     95257 No.
       104  0.4598986  E+231      12101 Composite No.
       105  0.262602   E+234      10753 Composite No.
       106  0.15152    E+237       1093 Composite No.
       107  0.8894307  E+239       2113      1033 No.
       108  0.527432   E+242      10859 Composite No.
       109  0.31593    E+245  Composite Composite No.
       110  0.18987514 E+248  Composite     20549 No.
       111  0.1152542  E+251      69473      3049 No.
       112  0.7651     E+253       5659      4871 No.
       113  0.43591562 E+256       5591 Composite No.
       114  0.2698318  E+259     296071 Composite No.
       115  0.17264    E+262       7927 Composite No.
       116  0.1914     E+265        881     17011 No.
       117  0.7017646  E+267     715927     10181 No.
       118  0.454042   E+270     109357 Composite No.
       119  0.29649    E+273     109673 Composite No.
       120  0.19538639 E+276  Composite       673 No.
       121  0.1291504  E+279  Composite Composite No.
       122  0.86918    E+281  Composite Composite No.
       123  0.58843637 E+284       2803       809 No.
       124  0.4019020  E+287  Composite      9391 No.
       125  0.277714   E+290       2063    311183 No.
       126  0.19468    E+293       2801 Composite No.
       127  0.13802651 E+296  Composite Composite No.
       128  0.992411   E+298     315643 Composite No.
       129  0.72148    E+301       8101 Composite No.
       130  0.52884668 E+304  Composite      2207 No.
       131  0.3908177  E+307     118387      1051 No.
       132  0.29378    E+310  Composite      8039 No.
       133  0.21807    E+313  Composite    122011 No.
       134  0.16508167 E+316       6637    118297 No.
       135  0.1256272  E+319  Composite Composite No.
       136  0.96607    E+321  Composite       997 No.
       137  0.74677427 E+324     638233     68281 No.
       138  0.5877113  E+327  Composite Composite No.
       139  0.468406   E+330        953 Composite No.
       140  0.37894    E+333      21991 Composite No.
       141  0.30732067 E+336  Composite      1279 No.
       142  0.2523103  E+339  Composite Composite No.
       143  0.207651   E+342  Composite      3271 No.
       144  0.17173    E+345      14503     32983 No.
       145  0.14236224 E+348      26987      2749 No.
       146  0.1194419  E+351  Composite      1109 No.
       147  0.101884   E+354  Composite     78643 No.
       148  0.87314550 E+356      35573 Composite No.
       149  0.750320   E+359  Composite Composite No.
       150  0.647278   E+362  Composite     26699 No.
       151  0.56766    E+365        911 Composite No.
       152  0.50011063 E+368  Composite Composite No.
       153  0.4415977  E+371  Composite Composite No.
       154  0.391697   E+374      11257      9001 No.
       155  0.35527    E+377        983      1259 No.
       156  0.32365034 E+380     109547    222023 No.
       157  0.2974347  E+383       1181     71899 No.
       158  0.276317   E+386  Composite    285227 No.
       159  0.25891    E+389      37811      1031 No.
       160  0.24363322 E+392       1433 Composite No.
       161  0.2307207  E+395       2153 Composite No.
       162  0.219877   E+398  Composite Composite No.
       163  0.21262    E+401  Composite Composite No.
       164  0.20645485 E+404       5737 Composite No.
       165  0.2017064  E+407      14083      6301 No.
       166  0.198277   E+410      26633 Composite No.
       167  0.19649    E+413   MR prime Composite Yes!
       168  0.1959341  E+416     106859     24889 No.
       169  0.1976665  E+419     521317 Composite No.
       170  0.20236    E+422  Composite    563113 No.
       171  0.2404     E+425          ?  MR prime Yes!
       172  0.20832554 E+428          ?  MR prime Yes!
       173  0.2147836  E+431      28771 Composite No.
       174  0.221871   E+434  Composite     26711 No.
       175  0.23052    E+437  Composite Composite No.
       176  0.24182018 E+440  Composite     18691 No.
       177  0.2541530  E+443      14969     91943 No.
       178  0.269656   E+446     784423      2029 No.
       179  0.28664    E+449     291503     31063 No.
       180  0.30642318 E+452       1327 Composite No.
       181  0.333820   E+455      17299 Composite No.
       182  0.363392   E+458  Composite      2539 No.
       183  0.39719    E+461  Composite     85621 No.
       184  0.43571519 E+464  Composite      1381 No.
       185  0.4805939  E+467       1283      3373 No.
       186  0.532979   E+470      24407    728423 No.
       187  0.59534    E+473       4157      3709 No.
       188  0.66856354 E+476  Composite Composite No.
       189  0.7548082  E+479       4943 Composite No.
       190  0.868784   E+482  Composite     10987 No.
       191  0.10171    E+486  Composite     29077 No.
       192  0.11650    E+489  Composite     49069 No.
       193  0.13641995 E+492  Composite Composite No.
       194  0.1611120  E+495     227629      1667 No.
       195  0.191240   E+498     102859 Composite No.
       196  0.22815    E+501  Composite Composite No.
       197  0.2740718  E+504  Composite Composite No.
       198  0.3323707  E+507  Composite      6133 No.
       199  0.404495   E+510  Composite Composite No.
       200  0.49470    E+513  Composite      9403 No.
       201  0.60798331 E+516  Composite     46993 No.
       202  0.7484275  E+519  Composite    149197 No.
       203  0.925805   E+522  Composite      1663 No.
       204  0.115633   E+526  Composite Composite No.
       205  0.14558    E+529  Composite      1867 No.
       206  0.1859817  E+532  Composite      3041 No.
       207  0.2377766  E+535  Composite      3643 No.
       208  0.305067   E+538      19687 Composite No.
       209  0.39323    E+541       1811 Composite No.
       210  0.50766221 E+544  Composite Composite No.
       211  0.6584379  E+547      43913     94841 No.
       212  0.856628   E+550  Composite Composite No.
       213  0.111619   E+554  Composite Composite No.
       214  0.14589    E+557  Composite     11059 No.
       215  0.19242297 E+560     391579      4877 No.
       216  0.2541907  E+563     260189      5147 No.
       217  0.337311   E+566  Composite Composite No.
       218  0.45908    E+569  Composite Composite No.
       219  0.62756294 E+572      20681      7219 No.
       220  0.8616439  E+575     756923 Composite No.
       221  0.1189930  E+579      48299 Composite No.
       222  0.166471   E+582     220307 Composite No.
       223  0.23456    E+585       3911     13033 No.
       224  0.33377601 E+588      34217      2609 No.
       225  0.4762984  E+591  Composite Composite No.
       226  0.68630    E+594       6421 Composite No.
       227  0.97534    E+597      86263      7159 No.
       228  0.14035    E+601       3557 Composite No.
       229  0.20308920 E+604  Composite Composite No.
       230  0.2946824  E+607  Composite Composite No.
       231  0.428174   E+610       8317 Composite No.
       232  0.62471    E+613       7691      1471 No.
       233  0.91894141 E+616  Composite Composite No.
       234  0.13609522 E+620  Composite      1619 No.
       235  0.2018292  E+623  Composite Composite No.
       236  0.30120    E+626  Composite      4957 No.
       237  0.44688    E+629  Composite Composite No.
       238  0.66718996 E+632      11423      5261 No.
       239  0.10001178 E+636  Composite     19793 No.
       240  0.1511178  E+639       5651     43237 No.
       241  0.23152    E+642       3637 Composite No.
       242  0.35236    E+645  Composite Composite No.
       243  0.54369661 E+648     120383 Composite No.
       244  0.8421860  E+651  Composite Composite No.
       245  0.1307915  E+655       8741      4919 No.
       246  0.203904   E+658       7457 Composite No.
       247  0.31952    E+661  Composite     16651 No.
       248  0.50196194 E+664      10091 Composite No.
       249  0.7925979  E+667      19207     35507 No.
       250  0.1254682  E+671  Composite Composite No.
       251  0.20373    E+674  Composite      7823 No.
       252  0.32080    E+677  Composite Composite No.
       253  0.51552053 E+680  Composite     10753 No.
       254  0.8294725  E+683  Composite Composite No.
       255  0.1337939  E+687  Composite Composite No.
       256  0.216612   E+690      77171      4519 No.
       257  0.35113    E+693       5867 Composite No.
       258  0.57128628 E+696  Composite Composite No.
       259  0.9351956  E+699  Composite Composite No.
       260  0.1549619  E+703  Composite      3203 No.
       261  0.257702   E+706      72931      2383 No.
       262  0.42959    E+709      51031 Composite No.
       263  0.71698350 E+712  Composite      3041 No.
       264  0.12138531 E+716      28933      4733 No.
       265  0.2059909  E+719  Composite Composite No.
       266  0.349978   E+722  Composite Composite No.
       267  0.59811    E+725       5857 Composite No.
       268  0.1294     E+729  Composite Composite No.
       269  0.17735750 E+732     213557     32647 No.
       270  0.3073605  E+735      13577 Composite No.
       271  0.535115   E+738  Composite Composite No.
       272  0.93485    E+741      15359 Composite No.
       273  0.16388    E+745  Composite Composite No.
       274  0.28826210 E+748  Composite Composite No.
       275  0.5122418  E+751  Composite Composite No.
       276  0.913327   E+754  Composite Composite No.
       277  0.163212   E+758       8429 Composite No.
       278  0.29199    E+761       4129 Composite No.
       279  0.52586580 E+764  Composite Composite No.
       280  0.9523430  E+767     220553     11057 No.
       281  0.1736121  E+771  Composite Composite No.
       282  0.317884   E+774     359633     50503 No.
       283  0.58713    E+777  Composite Composite No.
       284  0.1927     E+781       5189 Composite No.
       285  0.20399803 E+784  Composite Composite No.
       286  0.3816803  E+787       3467 Composite No.
       287  0.714887   E+790   MR prime Composite Yes!
       288  0.134184   E+794       2531     11149 No.
       289  0.25213    E+797      42223     11807 No.
       290  0.47627803 E+800      33199 Composite No.
       291  0.9054045  E+803  Composite Composite No.
       292  0.1726606  E+807  Composite Composite No.
       293  0.33300    E+810  Composite     33547 No.
       294  0.63781    E+813      11969 Composite No.
       295  0.12329    E+817      29333 Composite No.
       296  0.24028923 E+820  Composite Composite No.
       297  0.4688043  E+823     925579      8377 No.
       298  0.924951   E+826  Composite     27953 No.
       299  0.183048   E+830  Composite     73643 No.
       300  0.36372    E+833       6803      3581 No.
       301  0.72488583 E+836  Composite Composite No.
       302  0.14475970 E+840  Composite     13613 No.
       303  0.2893746  E+843     129169     88667 No.
       304  0.579617   E+846  Composite Composite No.
       305  0.116561   E+850  Composite     12809 No.
       306  0.23510    E+853       3323 Composite No.
       307  0.47655512 E+856      26423 Composite No.
       308  0.9669303  E+859  Composite     15473 No.
       309  0.1971571  E+863     402487     25763 No.
       310  0.404764   E+866   MR prime Composite Yes!
       311  0.83503    E+869      56131    551581 No.
       312  0.17277    E+873  Composite Composite No.
       313  0.35952836 E+876  Composite    268921 No.
       314  0.7488976  E+879      12637     51839 No.
       315  0.1562949  E+883  Composite     30323 No.
       316  0.326500   E+886  Composite    559841 No.
       317  0.68532    E+889  Composite      5573 No.
       318  0.14467    E+893     189713 Composite No.
       319  0.30569159 E+896  Composite     26357 No.
       320  0.6508174  E+899  Composite Composite No.
       321  0.1386892  E+903       3613 Composite No.
       322  0.296379   E+906  Composite Composite No.
       323  0.63455    E+909  Composite Composite No.
       324  0.13598    E+913      25577 Composite No.
       325  0.29277230 E+916  Composite Composite No.
       326  0.6326809  E+919  Composite Composite No.
       327  0.1378612  E+923       8893 Composite No.
       328  0.303708   E+926  Composite Composite No.
       329  0.67028    E+929  Composite Composite No.
       330  0.14833    E+933       3251    993247 No.
       331  0.32944945 E+936  Composite      4483 No.
       332  0.7369784  E+939  Composite Composite No.
       333  0.16595    E+943  Composite Composite No.
       334  0.37116    E+946  Composite Composite No.
       335  0.83313    E+949  Composite Composite No.
       336  0.18887    E+953  Composite Composite No.
       337  0.42854818 E+956  Composite Composite No.
       338  0.974900   E+959     379693 Composite No.
       339  0.2221899  E+963  Composite     18749 No.
       340  0.508148   E+966  Composite    281579 No.
       341  0.116518   E+970     234967 Composite No.
       342  0.26764    E+973  Composite Composite No.
       343  0.61798726 E+976  Composite     76949 No.
       344  0.14281685 E+980      18701 Composite No.
       345  0.3331917  E+983  Composite Composite No.
       346  0.779335   E+986     101839      8831 No.
       347  0.182442   E+990  Composite Composite No.
       348  0.42819    E+993       8293 Composite No.
       349  0.167      E+997  Composite      5399 No.
       350  0.23727454 E+1000 Composite     74381 No.
       351  0.5625779  E+1003 Composite Composite No.
       352  0.1337248  E+1007  MR prime    634097 Yes!
       353  0.318399   E+1010      6197 Composite No.
       354  0.75874    E+1013      7043 Composite No.
       355  0.18126    E+1017 Composite Composite No.
       356  0.43376466 E+1020 Composite Composite No.
       357  0.10406014 E+1024 Composite    131437 No.
       358  0.2508890  E+1027     31391     19531 No.
       359  0.606399   E+1030    162727      3881 No.
       360  0.146930   E+1034     19793 Composite No.
       361  0.35807    E+1037    432073 Composite No.
       362  0.87404742 E+1040 Composite     32983 No.
       363  0.21387940 E+1044 Composite    328421 No.
       364  0.5259295  E+1047 Composite Composite No.
       365  0.1297468  E+1051      3947 Composite No.
       366  0.32864    E+1054 Composite Composite No.
       367  0.79478    E+1057 Composite Composite No.
       368  0.19893    E+1061 Composite     52511 No.
       369  0.50151100 E+1064     20359    138517 No.
       370  0.12693243 E+1068     75931 Composite No.
       371  0.3222814  E+1071 Composite Composite No.
       372  0.819562   E+1074    273029 Composite No.
       373  0.208906   E+1078     35977     24223 No.
       374  0.53292    E+1081 Composite     42821 No.
       375  0.13627    E+1085    176227     38197 No.
       376  0.35143421 E+1088 Composite Composite No.
       377  0.9105660  E+1091 Composite Composite No.
       378  0.2361098  E+1095 Composite Composite No.
       379  0.616010   E+1098 Composite      3011 No.
       380  0.161210   E+1102     34313 Composite No.
       381  0.42253    E+1105 Composite Composite No.
       382  0.11125    E+1109 Composite     12149 No.
       383  0.29448527 E+1112     44449      3673 No.
       384  0.7824474  E+1115         ?  MR prime Yes!
       385  0.208528   E+1119 Composite Composite No.
       386  0.554044   E+1122    119033 Composite No.
       387  0.147985   E+1126 Composite      4093 No.
       388  0.39616    E+1129      4937      3533 No.
       389  0.1629     E+1133 Composite Composite No.
       390  0.28559805 E+1136    515143     37951 No.
       391  0.7679732  E+1139 Composite Composite No.
       392  0.2068152  E+1143 Composite Composite No.
       393  0.558194   E+1146 Composite      3001 No.
       394  0.151103   E+1150      8831     20399 No.
       395  0.4964     E+1153 Composite Composite No.
       396  0.11114    E+1157 Composite      4201 No.
       397  0.30217745 E+1160     84229      3499 No.
       398  0.8246423  E+1163     15973 Composite No.
       399  0.2252098  E+1167     11027      3061 No.
       400  0.617300   E+1170 Composite    176899 No.
       401  0.169696   E+1174 Composite      2999 No.
       402  0.46717    E+1177 Composite Composite No.
       403  0.12927    E+1181 Composite Composite No.
       404  0.35897344 E+1184 Composite    416693 No.
       405  0.10011769 E+1188      5849     49409 No.
       406  0.2794285  E+1191      7643 Composite No.
       407  0.781561   E+1194 Composite Composite No.
       408  0.218915   E+1198 Composite Composite No.
       409  0.61362    E+1201 Composite      3041 No.
       410  0.17298    E+1205 Composite     33547 No.
       411  0.49005067 E+1208 Composite      8689 No.
       412  0.13902738 E+1212      5347    153259 No.
       413  0.3952548  E+1215    114797      9491 No.
       414  0.1126872  E+1219 Composite Composite No.
       415  0.321947   E+1222 Composite Composite No.
       416  0.92109    E+1225 Composite Composite No.
       417  0.26518    E+1229 Composite      3301 No.
       418  0.76558065 E+1232 Composite Composite No.
       419  0.22178871 E+1236    126047     36251 No.
       420  0.6438526  E+1239 Composite    586627 No.
       421  0.1872967  E+1243 Composite Composite No.
       422  0.546345   E+1246     14009 Composite No.
       423  0.159915   E+1250      8597    425603 No.
       424  0.46999    E+1253 Composite     29483 No.
       425  0.13879    E+1257    123203 Composite No.
       426  0.41039656 E+1260 Composite Composite No.
       427  0.121650   E+1264     12433    579133 No.
       428  0.361319   E+1267      8117      3623 No.
       429  0.1072626  E+1271    112031     83773 No.
       430  0.321680   E+1274 Composite      3001 No.
       431  0.96536    E+1277 Composite Composite No.
       432  0.29067    E+1281     38669     14683 No.
       433  0.87753519 E+1284 Composite Composite No.
       434  0.26527889 E+1288 Composite Composite No.
       435  0.8056520  E+1291 Composite Composite No.
       436  0.2449988  E+1295    168899 Composite No.
       437  0.747001   E+1298 Composite Composite No.
       438  0.228657   E+1302 Composite Composite No.
       439  0.7129     E+1305     73189    148201 No.
       440  0.21593    E+1309 Composite Composite No.
       441  0.6657473  E+1312 Composite      4157 No.
       442  0.20563619 E+1316 Composite Composite No.
       443  0.6393229  E+1319 Composite Composite No.
       444  0.1994048  E+1323 Composite     18593 No.
       445  0.622342   E+1326     27823 Composite No.
       446  0.195229   E+1330    234947     15817 No.
       447  0.61751    E+1333 Composite Composite No.
       448  0.19557    E+1337 Composite      8933 No.
       449  0.61974557 E+1340 Composite    150833 No.
       450  0.19714107 E+1344     39239     38933 No.
       451  0.6282886  E+1347      7757     32587 No.
       452  0.2004869  E+1351 Composite     17257 No.
       453  0.642159   E+1354     11299 Composite No.
       454  0.206069   E+1358      3823 Composite No.
       455  0.66292    E+1361 Composite     14929 No.
       456  0.21353    E+1365 Composite    796339 No.
       457  0.68948124 E+1368         ?  MR prime Yes!
       458  0.22415035 E+1372 Composite Composite No.
       459  0.7291611  E+1375     50591    118687 No.
       460  0.2374878  E+1379 Composite     67733 No.

The MR prime test makes a series of trials, stopping early
only when a "definitely composite" result is encountered.
 Trial:      1     2     3     4     5     6
 Count:    449    13    13    13    13    13

20 in the hit list: 1,2,3,4,5,6,11,13,24,66,68,75,167,171,172,287,310,352,384,457
 CPU time:   5350.297

```


One and a half hours. Ho hum. This would be encouragement to convert to multi-precision routines written in assembler so as to take maximum advantage of the available arithmetic - say base 4294967296 with 32-bit unsigned products giving a 64-bit result, and a 64-bit division by a 32-bit giving a 32-bit result and remainder in separate registers, etc. Plus faith that such hardware does not contain errors, as with the Pentium bug.

And as before, all denunciations of "composite" were made by the first MR probe. This rather suggests that a speedier version could choose special values such as BIGBASE or BIGBASE - 1, for which special cases faster calculations can be done, and only if they fail to offer a definite report would other numbers be tried. As for checking a factor by explicit division, for these candidates, no factor was a small number once past the initial small numbers.


## FreeBASIC

```freebasic
' version 23-10-2016
' compile with: fbc -s console

#Define max 9999    ' max number for the sieve

#Include Once "gmp.bi"

Dim As mpz_ptr p, p1
p  = Allocate(Len(__mpz_struct)) : Mpz_init_set_ui(p, 1)
p1 = Allocate(Len(__mpz_struct)) : Mpz_init(p1)

Dim As UInteger i, n, x
Dim As Byte prime(max)

' Sieve of Eratosthenes
For i = 4 To max Step 2
  prime(i) = 1
Next
For i = 3 To Sqr(max) Step 2
  If prime(i) = 1 Then Continue For
  For n = i * i To max Step i * 2
    prime(n) = 1
  Next
Next

n = 0 : x = 0
For i = 2 To max
  If prime(i) = 1 Then Continue For
  x = x + 1
  mpz_mul_ui(p, p, i)
  mpz_sub_ui(p1, p, 1)
  If mpz_probab_prime_p(p1, 25) > 0 Then
    Print Using "####"; x; : Print ",";
    n += 1
    If n >= 20 Then Exit For
    Continue For
  End If
  mpz_add_ui(p1, p, 1)
  If mpz_probab_prime_p(p1, 25) > 0 Then
    Print Using "####"; x; : Print ",";
    n += 1
    If n >= 20 Then Exit For
  End If
Next

Print
mpz_clear(p)
mpz_clear(p1)

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
   1,   2,   3,   4,   5,   6,  11,  13,  24,  66,  68,  75, 167, 171, 172, 287, 310, 352, 384, 457,
```



## Go


```go
package main
  
import (
    "fmt"
    "math/big"
)

func main() {
    one := big.NewInt(1)
    pm := big.NewInt(1) // primorial
    var px, nx int
    var pb big.Int // a scratch value
    primes(4000, func(p int64) bool {
        pm.Mul(pm, pb.SetInt64(p))
        px++
        if pb.Add(pm, one).ProbablyPrime(0) ||
            pb.Sub(pm, one).ProbablyPrime(0) {
            fmt.Print(px, " ")
            nx++
            if nx == 20 {
                fmt.Println()
                return false
            }
        }
        return true
    })
}

// Code taken from task Sieve of Eratosthenes, and put into this function
// that calls callback function f for each prime < limit, but terminating
// if the callback returns false.
func primes(limit int, f func(int64) bool) {
    c := make([]bool, limit)
    c[0] = true
    c[1] = true
    lm := int64(limit)
    p := int64(2)
    for {
        f(p)
        p2 := p * p
        if p2 >= lm {
            break
        }
        for i := p2; i < lm; i += p {
            c[i] = true
        }
        for {
            p++
            if !c[p] {
                break
            }
        }
    }
    for p++; p < lm; p++ {
        if !c[p] && !f(p) {
            break
        }
    }
}
```

```txt

1 2 3 4 5 6 11 13 24 66 68 75 167 171 172 287 310 352 384 457 

```



## Haskell


```Haskell
import Data.List (scanl1, elemIndices, nub)

primes :: [Integer]
primes = 2 : filter isPrime [3,5 ..]

isPrime :: Integer -> Bool
isPrime = isPrime_ primes
  where
    isPrime_ :: [Integer] -> Integer -> Bool
    isPrime_ (p:ps) n
      | p * p > n = True
      | n `mod` p == 0 = False
      | otherwise = isPrime_ ps n

primorials :: [Integer]
primorials = 1 : scanl1 (*) primes

primorialsPlusMinusOne :: [Integer]
primorialsPlusMinusOne = concatMap (((:) . pred) <*> (return . succ)) primorials

sequenceOfPrimorialPrimes :: [Int]
sequenceOfPrimorialPrimes = (tail . nub) $ (`div` 2) <$> elemIndices True bools
  where
    bools = isPrime <$> primorialsPlusMinusOne

main :: IO ()
main = mapM_ print $ take 10 sequenceOfPrimorialPrimes
```

```txt
1
2
3
4
5
6
11
13
24
66
```



## J



```J
primoprim=: [: I. [: +./ 1 p: (1,_1) +/ */\@:p:@i.
```


This isn't particularly fast - J's current extended precision number implementation favors portability over speed.

Example use:


```J
   primoprim 600x
0 1 2 3 4 5 10 12 23 65 67 74 166 170 171 286 309 351 383 456 563 589
```


Note that the task specifies that index zero is not a part of the sequence for this task. So pretend you didn't see it.


## Java


```java
import java.math.BigInteger;

public class PrimorialPrimes {

    final static int sieveLimit = 1550_000;
    static boolean[] notPrime = sieve(sieveLimit);

    public static void main(String[] args) {

        int count = 0;
        for (int i = 1; i < 1000_000 && count < 20; i++) {
            BigInteger b = primorial(i);
            if (b.add(BigInteger.ONE).isProbablePrime(1)
                    || b.subtract(BigInteger.ONE).isProbablePrime(1)) {
                System.out.printf("%d ", i);
                count++;
            }
        }
    }

    static BigInteger primorial(int n) {
        if (n == 0)
            return BigInteger.ONE;

        BigInteger result = BigInteger.ONE;
        for (int i = 0; i < sieveLimit && n > 0; i++) {
            if (notPrime[i])
                continue;
            result = result.multiply(BigInteger.valueOf(i));
            n--;
        }
        return result;
    }

    public static boolean[] sieve(int limit) {
        boolean[] composite = new boolean[limit];
        composite[0] = composite[1] = true;

        int max = (int) Math.sqrt(limit);
        for (int n = 2; n <= max; n++) {
            if (!composite[n]) {
                for (int k = n * n; k < limit; k += n) {
                    composite[k] = true;
                }
            }
        }
        return composite;
    }
}
```



```txt
1 2 3 4 5 6 11 13 24 66 68 75 167 171 172 287 310 352 384 457
```



## Julia

```julia
using Primes

function primordials(N)
    print("The first $N primorial indices sequentially producing primorial primes are: 1 ")
    primorial = 1
    count = 1
    p = 3
    prod = BigInt(2)
    while true
        if isprime(p)
            prod *= p 
            primorial += 1
            if isprime(prod + 1) || isprime(prod - 1)
                print("$primorial ")
                count += 1
                if count == N
                    break
                end
            end
        end
        p += 2         
    end
end

primordials(20)

```
```txt

 The first 20 primorial indices sequentially producing primorial primes are: 1 2 3 4 5 6 11 13 24 66 68 75 167 171 172 287 310 352 384 457

```



## Kotlin


```scala
// version 1.0.6

import java.math.BigInteger

const val LIMIT = 20  // expect a run time of about 2 minutes on a typical laptop

fun isPrime(n: Int): Boolean {
    if (n < 2) return false 
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d : Int = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}
     
fun main(args: Array<String>) {
    println("The first $LIMIT primorial indices in the sequence are:")
    print("1 ")
    var primorial = 1
    var count = 1
    var p = 3
    var prod = BigInteger.valueOf(2L)
    while(true) {
        if (isPrime(p)) {
            prod *= BigInteger.valueOf(p.toLong()) 
            primorial++
            if ((prod + BigInteger.ONE).isProbablePrime(1) || (prod - BigInteger.ONE).isProbablePrime(1)) {
                print("$primorial ")
                count++
                if (count == LIMIT) {
                    println()
                    break
                }
            }
        }
        p += 2         
    } 
}
```


```txt

The first 20 primorial indices in the sequence are:
1 2 3 4 5 6 11 13 24 66 68 75 167 171 172 287 310 352 384 457

```



## PARI/GP

This program, in principle, generates arbitrarily many primirial prime indices. Practically speaking it depends on your patience; it generates them up to 643 in about 5 seconds.

```parigp
n=0; P=1; forprime(p=2,, P*=p; n++; if(ispseudoprime(P+1) || ispseudoprime(P-1), print1(n", ")))
```

```txt
1, 2, 3, 4, 5, 6, 11, 13, 24, 66, 68, 75, 167, 171, 172, 287, 310, 352, 384, 457, 564, 590, 616, 620, 643, 849,
```



## Perl

```perl
use ntheory ":all";
my $i = 0;
for (1..1e6) { 
  my $n = pn_primorial($_);
  if (is_prime($n-1) || is_prime($n+1)) {
    print "$_\n";
    last if ++$i >= 20;
  }
}
```

```txt
1
2
3
4
5
6
11
13
24
66
68
75
167
171
172
287
310
352
384
457
```



## Perl 6


```perl6
constant @primes     = |grep *.is-prime, 2..*;
constant @primorials = [\*] 1, @primes;

my @pp_indexes := |@primorials.pairs.map: {
    .key if ( .value + any(1, -1) ).is-prime
};

say ~ @pp_indexes[ 0 ^.. 20 ]; # Skipping bogus first element.
```

```txt
1 2 3 4 5 6 11 13 24 66 68 75 167 171 172 287 310 352 384 457
```



## Phix

```Phix
include primes.e
include mpfr.e

constant limit = 9999

mpz p = mpz_init(1),
    p1 = mpz_init()
randstate state =  gmp_randinit_mt()
 
atom t0 = time()
integer found = 0, i
for n=1 to limit do
    mpz_mul_si(p, p, get_prime(n))
    for i=-1 to +1 do
        mpz_add_si(p1, p, i)
        if mpz_probable_prime_p(p1,state,25) then
            integer l = mpz_sizeinbase(p,10)
            string ps = iff(l>20?sprintf("%d digits",l)
                                :mpz_get_str(p))
            printf(1,"%d (%s) is a primorial prime\n",{n,ps})
            found += 1
            exit
        end if
    end for
    if found>=20 then exit end if
end for
{p,p1} = mpz_free({p,p1})
state = gmp_randclear(state)
```

```txt

1 (2) is a primorial prime
2 (6) is a primorial prime
3 (30) is a primorial prime
4 (210) is a primorial prime
5 (2310) is a primorial prime
6 (30030) is a primorial prime
11 (200560490130) is a primorial prime
13 (304250263527210) is a primorial prime
24 (35 digits) is a primorial prime
66 (131 digits) is a primorial prime
68 (136 digits) is a primorial prime
75 (154 digits) is a primorial prime
167 (413 digits) is a primorial prime
171 (425 digits) is a primorial prime
172 (428 digits) is a primorial prime
287 (790 digits) is a primorial prime
310 (866 digits) is a primorial prime
352 (1007 digits) is a primorial prime
384 (1116 digits) is a primorial prime
457 (1368 digits) is a primorial prime

```



## Python

Uses the pure python library [https://pypi.python.org/pypi/pyprimes/0.1.1a pyprimes ]. Note that pyprimes.isprime switches to a fast and good probabilistic algorithm for larger integers.


```python
import pyprimes

def primorial_prime(_pmax=500):
    isprime = pyprimes.isprime
    n, primo = 0, 1
    for prime in pyprimes.nprimes(_pmax):
        n, primo = n+1, primo * prime
        if isprime(primo-1) or isprime(primo+1):
            yield n
        
if __name__ == '__main__':
    # Turn off warning on use of probabilistic formula for prime test
    pyprimes.warn_probably = False  
    for i, n in zip(range(20), primorial_prime()):
        print('Primorial prime %2i at primorial index: %3i' % (i+1, n))
```


```txt
Primorial prime  1 at primorial index:   1
Primorial prime  2 at primorial index:   2
Primorial prime  3 at primorial index:   3
Primorial prime  4 at primorial index:   4
Primorial prime  5 at primorial index:   5
Primorial prime  6 at primorial index:   6
Primorial prime  7 at primorial index:  11
Primorial prime  8 at primorial index:  13
Primorial prime  9 at primorial index:  24
Primorial prime 10 at primorial index:  66
Primorial prime 11 at primorial index:  68
Primorial prime 12 at primorial index:  75
Primorial prime 13 at primorial index: 167
Primorial prime 14 at primorial index: 171
Primorial prime 15 at primorial index: 172
Primorial prime 16 at primorial index: 287
Primorial prime 17 at primorial index: 310
Primorial prime 18 at primorial index: 352
Primorial prime 19 at primorial index: 384
Primorial prime 20 at primorial index: 457
```



## Racket

We use a memorized version of <code>primordial</code> to make it faster. 

```Racket
#lang racket

(require math/number-theory
         racket/generator)

(define-syntax-rule (define/cache (name arg) body ...)
  (begin
    (define cache (make-hash))
    (define (name arg)
      (hash-ref! cache arg (lambda () body ...)))))

(define/cache (primorial n)
  (if (zero? n)
     1
     (* (nth-prime (sub1 n))
        (primorial (sub1 n)))))

(for ([i (in-range 20)]
      [n (in-generator (for ([i (in-naturals 1)])
                         (define pr (primorial i))
                         (when (or (prime? (add1 pr)) (prime? (sub1 pr)))
                           (yield i))))])
  (displayln n))
```

```txt
1
2
3
4
5
6
11
13
24
66
68
75
167
171
172
287
310
352
384
457
```



## Ring


```ring

# Project : Sequence of primorial primes

max = 9999
primes = []
for n = 1 to max
     if isprime(n) = 1
        add(primes, n)
     ok
next
for n = 1 to len(primes)
     sum = 1
     for m = 1 to n
          sum = sum * primes[m]
     next
     if (isprime(sum+1) or isprime(sum-1)) = 1
        see "" + n + " "
     ok
next

func isprime(num)
       if (num <= 1) return 0 ok
       if (num % 2 = 0) and num != 2 return 0 ok
       for i = 3 to floor(num / 2) -1 step 2
            if (num % i = 0) return 0 ok
       next
       return 1

```

Output:

```txt

1,   2,   3,   4,   5,   6,  11,  13,  24,  66,  68,  75, 167, 171, 172, 287, 310, 352, 384, 457

```




## Ruby


```Ruby


# Sequence of primorial primes

require 'prime' # for creating prime_array
require 'openssl' # for using fast Miller–Rabin primality test (just need 10.14 seconds to complete)
 
i, urutan, primorial_number = 1, 1, OpenSSL::BN.new(1)
start = Time.now
prime_array = Prime.first (500)

until urutan > 20
  primorial_number *= prime_array[i-1] 
  if (primorial_number - 1).prime_fasttest? || (primorial_number + 1).prime_fasttest?
    puts "#{Time.now - start} \tPrimorial prime #{urutan}: #{i}"
    urutan += 1
  end 
  i += 1
end


```


Output:

```txt

0.000501        Primorial prime 1: 1
0.00094         Primorial prime 2: 2
0.001474        Primorial prime 3: 3
0.001969        Primorial prime 4: 4
0.002439        Primorial prime 5: 5
0.003557        Primorial prime 6: 6
0.004167        Primorial prime 7: 11
0.004849        Primorial prime 8: 13
0.006287        Primorial prime 9: 24
0.020018        Primorial prime 10: 66
0.022049        Primorial prime 11: 68
0.026954        Primorial prime 12: 75
0.213867        Primorial prime 13: 167
0.243797        Primorial prime 14: 171
0.256598        Primorial prime 15: 172
1.469281        Primorial prime 16: 287
2.012719        Primorial prime 17: 310
3.598149        Primorial prime 18: 352
5.252034        Primorial prime 19: 384
10.147519       Primorial prime 20: 457

```



## Sidef


```ruby
func primorial_primes(n) {

    var k = 1
    var p = 2
    var P = 2

    var seq = []
    for (var i = 0; i < n; ++k) {

        if (is_prime(P-1) || is_prime(P+1)) {
            seq << k
            ++i
        }

        p.next_prime!
        P *= p
    }

    return seq
}

say primorial_primes(20)
```

```txt

[1, 2, 3, 4, 5, 6, 11, 13, 24, 66, 68, 75, 167, 171, 172, 287, 310, 352, 384, 457]

```



## zkl

Uses libGMP (GNU MP Bignum Library)

```zkl
var [const] BN=Import("zklBigNum");  // libGMP
p,s,i,n:=BN(1),BN(1), 0,0;
do{ n+=1;
    s.nextPrime();	// in place, probabilistic
    p.mul(s);		// in place
    if((p+1).probablyPrime() or (p-1).probablyPrime()){
       println("%3d  %5d digits".fmt(n,p.len()));
       i+=1;
    }
}while(i<20);
```

```txt

  1      1 digits
  2      1 digits
  3      2 digits
  4      3 digits
  5      4 digits
  6      5 digits
 11     12 digits
 13     15 digits
 24     35 digits
 66    131 digits
 68    136 digits
 75    154 digits
167    413 digits
171    425 digits
172    428 digits
287    790 digits
310    866 digits
352   1007 digits
384   1116 digits
457   1368 digits

```

