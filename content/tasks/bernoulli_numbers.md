+++
title = "Bernoulli numbers"
description = ""
date = 2019-10-09T18:56:53Z
aliases = []
[extra]
id = 17350
[taxonomies]
categories = ["task", "Mathematics"]
tags = []
languages = [
  "ada",
  "algol_68",
  "bracmat",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "echolisp",
  "elixir",
  "factor",
  "freebasic",
  "funl",
  "gap",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "maple",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "spad",
  "tcl",
  "visual_basic_.net",
  "zkl",
]
+++

[[wp:Bernoulli number|Bernoulli numbers]] are used in some series expansions of several functions   (trigonometric, hyperbolic, gamma, etc.),   and are extremely important in number theory and analysis.

Note that there are two definitions of Bernoulli numbers;   this task will be using the modern usage   (as per   ''The National Institute of Standards and Technology convention'').

The   n<sup>th</sup>   Bernoulli number is expressed as   '''B'''<sub>n</sub>.



## Task

:*   show the Bernoulli numbers   '''B'''<sub>0</sub>   through   '''B'''<sub>60</sub>.
:*   suppress the output of values which are equal to zero.   (Other than   '''B'''<sub>1</sub> , all ''odd''  Bernoulli numbers have a value of zero.)
:*   express the Bernoulli numbers as fractions  (most are improper fractions).
:*   the fractions should be reduced.
:*   index each number in some way so that it can be discerned which Bernoulli number is being displayed.
:*   align the solidi   (<big><b>/</b></big>)   if used (extra credit).


;An algorithm
The Akiyama–Tanigawa algorithm for the "second Bernoulli numbers" as taken from [[wp:Bernoulli_number#Algorithmic_description|wikipedia]] is as follows:

  '''for''' ''m'' '''from''' 0 '''by''' 1 '''to''' ''n'' '''do'''
     ''A''[''m''] ← 1/(''m''+1)
     '''for''' ''j'' '''from''' ''m'' '''by''' -1 '''to''' 1 '''do'''
       ''A''[''j''-1] ← ''j''×(''A''[''j''-1] - ''A''[''j''])
   '''return''' ''A''[0] (which is ''B''<sub>''n''</sub>)

## See also

* Sequence [http://oeis.org/A027641 A027641 Numerator of Bernoulli number B_n] on The On-Line Encyclopedia of Integer Sequences.
* Sequence [http://oeis.org/A027642 A027642 Denominator of Bernoulli number B_n] on The On-Line Encyclopedia of Integer Sequences.
* Entry [http://mathworld.wolfram.com/BernoulliNumber.html Bernoulli number] on The Eric Weisstein's World of Mathematics (TM).
* Luschny's [http://luschny.de/math/zeta/The-Bernoulli-Manifesto.html The Bernoulli Manifesto] for a discussion on   <big> '''B<sub>1</sub>   =   -&frac12;'''   versus   '''+&frac12;'''. </big>





## Ada

Using a GMP thick binding available at http://www.codeforge.com/article/422541


```Ada
WITH GMP.Rationals, GMP.Integers, Ada.Text_IO, Ada.Strings.Fixed, Ada.Strings;
USE GMP.Rationals, GMP.Integers, Ada.Text_IO, Ada.Strings.Fixed, Ada.Strings;

PROCEDURE Main IS

   FUNCTION Bernoulli_Number (N : Natural) RETURN Unbounded_Fraction IS
      FUNCTION "/" (Left, Right : Natural) RETURN Unbounded_Fraction IS
        (To_Unbounded_Integer (Left) / To_Unbounded_Integer (Right));
      A : ARRAY (0 .. N) OF Unbounded_Fraction;
   BEGIN
      FOR M IN 0 .. N LOOP
         A (M) := 1 / (M + 1);
         FOR J IN REVERSE 1 .. M LOOP
            A (J - 1) := (J / 1 ) * (A (J - 1) - A (J));
         END LOOP;
      END LOOP;
      RETURN A (0);
   END Bernoulli_Number;

BEGIN
   FOR I IN 0 .. 60 LOOP
      IF I MOD 2 = 0 OR I = 1 THEN
         DECLARE
            B : Unbounded_Fraction := Bernoulli_Number (I);
            S : String := Image (GMP.Rationals.Numerator (B));
         BEGIN
            Put_Line ("B (" & (IF I < 10 THEN " " ELSE "") &  Trim (I'Img, Left)
                      & ")=" & (44 - S'Length) * " " & Image (B));
         END;
      END IF;
   END LOOP;
END Main;
```

```txt

B(0)=                                            1 / 1
B(1)=                                            1 / 2
B(2)=                                            1 / 6
B(4)=                                           -1 / 30
B(6)=                                            1 / 42
B(8)=                                           -1 / 30
B(10)=                                           5 / 66
B(12)=                                        -691 / 2730
B(14)=                                           7 / 6
B(16)=                                       -3617 / 510
B(18)=                                       43867 / 798
B(20)=                                     -174611 / 330
B(22)=                                      854513 / 138
B(24)=                                  -236364091 / 2730
B(26)=                                     8553103 / 6
B(28)=                                -23749461029 / 870
B(30)=                               8615841276005 / 14322
B(32)=                              -7709321041217 / 510
B(34)=                               2577687858367 / 6
B(36)=                       -26315271553053477373 / 1919190
B(38)=                            2929993913841559 / 6
B(40)=                      -261082718496449122051 / 13530
B(42)=                      1520097643918070802691 / 1806
B(44)=                    -27833269579301024235023 / 690
B(46)=                    596451111593912163277961 / 282
B(48)=               -5609403368997817686249127547 / 46410
B(50)=                 495057205241079648212477525 / 66
B(52)=             -801165718135489957347924991853 / 1590
B(54)=            29149963634884862421418123812691 / 798
B(56)=         -2479392929313226753685415739663229 / 870
B(58)=         84483613348880041862046775994036021 / 354
B(60)=-1215233140483755572040304994079820246041491 / 56786730

```



## ALGOL 68

Uses the LONG LONG INT mode of Algol 68G which allows large precision integers.

```algol68
BEGIN
    # Show Bernoulli numbers B0 to B60 as rational numbers           #

    # Uses code from the Arithmetic/Rational task modified to use    #
    # LONG LONG INT to allow for the large number of digits requried #

    PR precision 100 PR # sets the precision of LONG LONG INT        #

    # Code from the Arithmetic/Rational task                         #
    #
### ========================================================
 #

    MODE FRAC = STRUCT( LONG LONG INT num #erator#,  den #ominator#);

    PROC gcd = (LONG LONG INT a, b) LONG LONG INT: # greatest common divisor #
       (a = 0 | b |: b = 0 | a |: ABS a > ABS b  | gcd(b, a MOD b) | gcd(a, b MOD a));

    PROC lcm = (LONG LONG INT a, b)LONG LONG INT: # least common multiple #
       a OVER gcd(a, b) * b;

    PRIO // = 9; # higher then the ** operator #
    OP // = (LONG LONG INT num, den)FRAC: ( # initialise and normalise #
       LONG LONG INT common = gcd(num, den);
       IF den < 0 THEN
         ( -num OVER common, -den OVER common)
       ELSE
         ( num OVER common, den OVER common)
       FI
     );

    OP + = (FRAC a, b)FRAC: (
       LONG LONG INT common = lcm(den OF a, den OF b);
       FRAC result := ( common OVER den OF a * num OF a + common OVER den OF b * num OF b, common );
       num OF result//den OF result
    );

    OP - = (FRAC a, b)FRAC: a + -b,
       * = (FRAC a, b)FRAC: (
           LONG LONG INT num = num OF a * num OF b,
           den = den OF a * den OF b;
           LONG LONG INT common = gcd(num, den);
           (num OVER common) // (den OVER common)
         );

    OP - = (FRAC frac)FRAC: (-num OF frac, den OF frac);

    #
### ========================================================
 #
    # end code from the Arithmetic/Rational task                     #

    # Additional FRACrelated operators                               #
    OP *  = ( INT a, FRAC b )FRAC: ( num OF b * a ) // den OF b;
    OP // = ( INT a, INT  b )FRAC: LONG LONG INT( a ) // LONG LONG INT( b );

    # returns the nth Bernoulli number, n must be >= 0               #
    # Uses the algorithm suggested by the task, so B(1) is +1/2      #
    PROC bernoulli = ( INT n )FRAC:
         IF n < 0
         THEN # n is out of range # 0 // 1
         ELSE # n is valid        #
            [ 0 : n ]FRAC a;
            FOR i FROM LWB a TO UPB a DO a[ i ] := 0 // 1 OD;
            FOR m FROM 0 TO n DO
                a[ m ] := 1 // ( m + 1 );
                FOR j FROM m BY -1 TO 1 DO
                    a[ j - 1 ] := j * ( a[ j - 1 ] - a[ j ] )
                OD
            OD;
            a[ 0 ]
         FI # bernoulli # ;

    FOR n FROM 0 TO 60 DO
        FRAC bn := bernoulli( n );
        IF num OF bn /= 0 THEN
            # have a non-0 Bn #
            print( ( "B(", whole( n, -2 ), ") ", whole( num OF bn, -50 ), " / ", whole( den OF bn, 0 ), newline ) )
        FI
    OD
END

```

```txt

B( 0)                                                  1 / 1
B( 1)                                                  1 / 2
B( 2)                                                  1 / 6
B( 4)                                                 -1 / 30
B( 6)                                                  1 / 42
B( 8)                                                 -1 / 30
B(10)                                                  5 / 66
B(12)                                               -691 / 2730
B(14)                                                  7 / 6
B(16)                                              -3617 / 510
B(18)                                              43867 / 798
B(20)                                            -174611 / 330
B(22)                                             854513 / 138
B(24)                                         -236364091 / 2730
B(26)                                            8553103 / 6
B(28)                                       -23749461029 / 870
B(30)                                      8615841276005 / 14322
B(32)                                     -7709321041217 / 510
B(34)                                      2577687858367 / 6
B(36)                              -26315271553053477373 / 1919190
B(38)                                   2929993913841559 / 6
B(40)                             -261082718496449122051 / 13530
B(42)                             1520097643918070802691 / 1806
B(44)                           -27833269579301024235023 / 690
B(46)                           596451111593912163277961 / 282
B(48)                      -5609403368997817686249127547 / 46410
B(50)                        495057205241079648212477525 / 66
B(52)                    -801165718135489957347924991853 / 1590
B(54)                   29149963634884862421418123812691 / 798
B(56)                -2479392929313226753685415739663229 / 870
B(58)                84483613348880041862046775994036021 / 354
B(60)       -1215233140483755572040304994079820246041491 / 56786730

```



## Bracmat


```bracmat
  ( BernoulliList
  =     B Bs answer indLn indexLen indexPadding
      , n numberPadding p solPos solidusPos sp
    .   ( B
        =   m A a j b
          .   -1:?m
            & :?A
            &   whl
              ' ( 1+!m:~>!arg:?m
                &     ((!m+1:?j)^-1:?a)
                        map
                      $ ( (
                          = .(-1+!j:?j)*(!arg+-1*!a):?a
                          )
                        . !A
                        )
                  : ?A
                )
            & !A:? @?b
            & !b
        )
      & -1:?n
      & :?Bs
      &   whl
        ' ( 1+!n:~>!arg:?n
          & B$!n !Bs:?Bs
          )
      & @(!arg:? [?indexLen)
      & 1+!indexLen:?indexLen
      & !Bs:%@(?:? "/" [?solidusPos ?) ?
      & 1+!solidusPos:?solidusPos:?p
      & :?sp
      &   whl
        ' (!p+-1:~<0:?p&" " !sp:?sp)
      & :?answer
      &   whl
        ' ( !Bs:%?B ?Bs
          & ( !B:0
            |   (!B:/|str$(!B "/1"):?B)
              & @(!B:? "/" [?solPos ?)
              & @(!arg:? [?indLn)
              &   !sp
                : ? [(-1*!indexLen+!indLn) ?indexPadding
                : ? [(-1*!solidusPos+!solPos) ?numberPadding
              &     "B("
                    !arg
                    ")="
                    !indexPadding
                    !numberPadding
                    (!B:>0&" "|)
                    !B
                    \n
                    !answer
                : ?answer
            )
          & -1+!arg:?arg
          )
      & str$!answer
  )
& BernoulliList$60;
```


```txt
B(0)=                                            1/1
B(1)=                                            1/2
B(2)=                                            1/6
B(4)=                                           -1/30
B(6)=                                            1/42
B(8)=                                           -1/30
B(10)=                                           5/66
B(12)=                                        -691/2730
B(14)=                                           7/6
B(16)=                                       -3617/510
B(18)=                                       43867/798
B(20)=                                     -174611/330
B(22)=                                      854513/138
B(24)=                                  -236364091/2730
B(26)=                                     8553103/6
B(28)=                                -23749461029/870
B(30)=                               8615841276005/14322
B(32)=                              -7709321041217/510
B(34)=                               2577687858367/6
B(36)=                       -26315271553053477373/1919190
B(38)=                            2929993913841559/6
B(40)=                      -261082718496449122051/13530
B(42)=                      1520097643918070802691/1806
B(44)=                    -27833269579301024235023/690
B(46)=                    596451111593912163277961/282
B(48)=               -5609403368997817686249127547/46410
B(50)=                 495057205241079648212477525/66
B(52)=             -801165718135489957347924991853/1590
B(54)=            29149963634884862421418123812691/798
B(56)=         -2479392929313226753685415739663229/870
B(58)=         84483613348880041862046775994036021/354
B(60)=-1215233140483755572040304994079820246041491/56786730
```


## C

```C

#include <stdlib.h>
#include <gmp.h>

#define mpq_for(buf, op, n)\
    do {\
        size_t i;\
        for (i = 0; i < (n); ++i)\
            mpq_##op(buf[i]);\
    } while (0)

void bernoulli(mpq_t rop, unsigned int n)
{
    unsigned int m, j;
    mpq_t *a = malloc(sizeof(mpq_t) * (n + 1));
    mpq_for(a, init, n + 1);

    for (m = 0; m <= n; ++m) {
        mpq_set_ui(a[m], 1, m + 1);
        for (j = m; j > 0; --j) {
            mpq_sub(a[j-1], a[j], a[j-1]);
            mpq_set_ui(rop, j, 1);
            mpq_mul(a[j-1], a[j-1], rop);
        }
    }

    mpq_set(rop, a[0]);
    mpq_for(a, clear, n + 1);
    free(a);
}

int main(void)
{
    mpq_t rop;
    mpz_t n, d;
    mpq_init(rop);
    mpz_inits(n, d, NULL);

    unsigned int i;
    for (i = 0; i <= 60; ++i) {
        bernoulli(rop, i);
        if (mpq_cmp_ui(rop, 0, 1)) {
            mpq_get_num(n, rop);
            mpq_get_den(d, rop);
            gmp_printf("B(%-2u) = %44Zd / %Zd\n", i, n, d);
        }
    }

    mpz_clears(n, d, NULL);
    mpq_clear(rop);
    return 0;
}

```

```txt

B(0 ) =                                            1 / 1
B(1 ) =                                           -1 / 2
B(2 ) =                                            1 / 6
B(4 ) =                                           -1 / 30
B(6 ) =                                            1 / 42
B(8 ) =                                           -1 / 30
B(10) =                                            5 / 66
B(12) =                                         -691 / 2730
B(14) =                                            7 / 6
B(16) =                                        -3617 / 510
B(18) =                                        43867 / 798
B(20) =                                      -174611 / 330
B(22) =                                       854513 / 138
B(24) =                                   -236364091 / 2730
B(26) =                                      8553103 / 6
B(28) =                                 -23749461029 / 870
B(30) =                                8615841276005 / 14322
B(32) =                               -7709321041217 / 510
B(34) =                                2577687858367 / 6
B(36) =                        -26315271553053477373 / 1919190
B(38) =                             2929993913841559 / 6
B(40) =                       -261082718496449122051 / 13530
B(42) =                       1520097643918070802691 / 1806
B(44) =                     -27833269579301024235023 / 690
B(46) =                     596451111593912163277961 / 282
B(48) =                -5609403368997817686249127547 / 46410
B(50) =                  495057205241079648212477525 / 66
B(52) =              -801165718135489957347924991853 / 1590
B(54) =             29149963634884862421418123812691 / 798
B(56) =          -2479392929313226753685415739663229 / 870
B(58) =          84483613348880041862046775994036021 / 354
B(60) = -1215233140483755572040304994079820246041491 / 56786730

```



## C++


=== Using Boost | C++11 ===
```cpp


/**
 * Configured with: --prefix=/Library/Developer/CommandLineTools/usr --with-gxx-include-dir=/usr/include/c++/4.2.1
 * Apple LLVM version 9.1.0 (clang-902.0.39.1)
 * Target: x86_64-apple-darwin17.5.0
 * Thread model: posix
*/

#include <iostream> //std::cout
#include <iostream> //formatting
#include <vector> //Container
#include <boost/rational.hpp> // Rationals
#include <boost/multiprecision/cpp_int.hpp> //1024bit precision


typedef boost::rational<boost::multiprecision::int1024_t> rational; // reduce boilerplate

rational bernulli(size_t n){

     auto out = std::vector<rational>();

     for(size_t m=0;m<=n;m++){
         out.emplace_back(1,(m+1)); // automatically constructs object
         for (size_t j = m;j>=1;j--){
             out[j-1] = rational(j) * (out[j-1]-out[j]);
         }
     }
     return out[0];
 }

int main() {
    for(size_t n = 0; n <= 60;n+=n>=2?2:1){
        auto b = bernulli(n);
        std::cout << "B("<<std::right<<std::setw(2)<<n<<") = ";
        std::cout << std::right<<std::setw(44)<<b.numerator();
        std::cout << " / " << b.denominator() <<std::endl;
    }

    return 0;
}

```

```txt

B( 0) =                                            1 / 1
B( 1) =                                            1 / 2
B( 2) =                                            1 / 6
B( 4) =                                           -1 / 30
B( 6) =                                            1 / 42
B( 8) =                                           -1 / 30
B(10) =                                            5 / 66
B(12) =                                         -691 / 2730
B(14) =                                            7 / 6
B(16) =                                        -3617 / 510
B(18) =                                        43867 / 798
B(20) =                                      -174611 / 330
B(22) =                                       854513 / 138
B(24) =                                   -236364091 / 2730
B(26) =                                      8553103 / 6
B(28) =                                 -23749461029 / 870
B(30) =                                8615841276005 / 14322
B(32) =                               -7709321041217 / 510
B(34) =                                2577687858367 / 6
B(36) =                        -26315271553053477373 / 1919190
B(38) =                             2929993913841559 / 6
B(40) =                       -261082718496449122051 / 13530
B(42) =                       1520097643918070802691 / 1806
B(44) =                     -27833269579301024235023 / 690
B(46) =                     596451111593912163277961 / 282
B(48) =                -5609403368997817686249127547 / 46410
B(50) =                  495057205241079648212477525 / 66
B(52) =              -801165718135489957347924991853 / 1590
B(54) =             29149963634884862421418123812691 / 798
B(56) =          -2479392929313226753685415739663229 / 870
B(58) =          84483613348880041862046775994036021 / 354
B(60) = -1215233140483755572040304994079820246041491 / 56786730

```


## C#


###  Using Mpir.NET

Translation of the C implementation

```c#

using Mpir.NET;
using System;

namespace Bernoulli
{
    class Program
    {
        private static void bernoulli(mpq_t rop, uint n)
        {
            mpq_t[] a = new mpq_t[n + 1];

            for (uint i = 0; i < n + 1; i++)
            {
                a[i] = new mpq_t();
            }

            for (uint m = 0; m <= n; ++m)
            {
                mpir.mpq_set_ui(a[m], 1, m + 1);

                for (uint j = m; j > 0; --j)
                {
                    mpir.mpq_sub(a[j - 1], a[j], a[j - 1]);
                    mpir.mpq_set_ui(rop, j, 1);
                    mpir.mpq_mul(a[j - 1], a[j - 1], rop);
                }

                mpir.mpq_set(rop, a[0]);
            }
        }

        static void Main(string[] args)
        {
            mpq_t rop = new mpq_t();
            mpz_t n = new mpz_t();
            mpz_t d = new mpz_t();

            for (uint  i = 0; i <= 60; ++i)
            {
                bernoulli(rop, i);

                if (mpir.mpq_cmp_ui(rop, 0, 1) != 0)
                {
                    mpir.mpq_get_num(n, rop);
                    mpir.mpq_get_den(d, rop);
                    Console.WriteLine(string.Format("B({0, 2}) = {1, 44} / {2}", i, n, d));
                }
            }

            Console.ReadKey();
        }
    }
}

```

```txt

B(0 ) =                                            1 / 1
B(1 ) =                                           -1 / 2
B(2 ) =                                            1 / 6
B(4 ) =                                           -1 / 30
B(6 ) =                                            1 / 42
B(8 ) =                                           -1 / 30
B(10) =                                            5 / 66
B(12) =                                         -691 / 2730
B(14) =                                            7 / 6
B(16) =                                        -3617 / 510
B(18) =                                        43867 / 798
B(20) =                                      -174611 / 330
B(22) =                                       854513 / 138
B(24) =                                   -236364091 / 2730
B(26) =                                      8553103 / 6
B(28) =                                 -23749461029 / 870
B(30) =                                8615841276005 / 14322
B(32) =                               -7709321041217 / 510
B(34) =                                2577687858367 / 6
B(36) =                        -26315271553053477373 / 1919190
B(38) =                             2929993913841559 / 6
B(40) =                       -261082718496449122051 / 13530
B(42) =                       1520097643918070802691 / 1806
B(44) =                     -27833269579301024235023 / 690
B(46) =                     596451111593912163277961 / 282
B(48) =                -5609403368997817686249127547 / 46410
B(50) =                  495057205241079648212477525 / 66
B(52) =              -801165718135489957347924991853 / 1590
B(54) =             29149963634884862421418123812691 / 798
B(56) =          -2479392929313226753685415739663229 / 870
B(58) =          84483613348880041862046775994036021 / 354
B(60) = -1215233140483755572040304994079820246041491 / 56786730

```



###  Using Math.NET


```c#

using System;
using System.Console;
using System.Linq;
using MathNet.Numerics;

namespace Rosettacode.Rational.CS
{
    class Program
    {
        private static readonly Func<int, BigRational> ℚ = BigRational.FromInt;

        private static BigRational CalculateBernoulli(int n)
        {
            var a = InitializeArray(n);

            foreach(var m in Enumerable.Range(1,n))
            {
                a[m] = ℚ(1) / (ℚ(m) + ℚ(1));

                for (var j = m; j >= 1; j--)
                {
                    a[j-1] = ℚ(j) * (a[j-1] - a[j]);
                }
            }

            return a[0];
        }

        private static BigRational[] InitializeArray(int n)
        {
            var a = new BigRational[n + 1];

            for (var x = 0; x < a.Length; x++)
            {
                a[x] = ℚ(x + 1);
            }

            return a;
        }

        static void Main()
        {
            Enumerable.Range(0, 61) // the second parameter is the number of range elements, and is not the final item of the range.
                .Select(n => new {N = n, BernoulliNumber = CalculateBernoulli(n)})
                .Where(b => !b.BernoulliNumber.Numerator.IsZero)
                .Select(b => string.Format("B({0, 2}) = {1, 44} / {2}", b.N, b.BernoulliNumber.Numerator, b.BernoulliNumber.Denominator))
                .ToList()
                .ForEach(WriteLine);
        }
    }
}

```

```txt

B( 0) =                                            1 / 1
B( 1) =                                            1 / 2
B( 2) =                                            1 / 6
B( 4) =                                           -1 / 30
B( 6) =                                            1 / 42
B( 8) =                                           -1 / 30
B(10) =                                            5 / 66
B(12) =                                         -691 / 2730
B(14) =                                            7 / 6
B(16) =                                        -3617 / 510
B(18) =                                        43867 / 798
B(20) =                                      -174611 / 330
B(22) =                                       854513 / 138
B(24) =                                   -236364091 / 2730
B(26) =                                      8553103 / 6
B(28) =                                 -23749461029 / 870
B(30) =                                8615841276005 / 14322
B(32) =                               -7709321041217 / 510
B(34) =                                2577687858367 / 6
B(36) =                        -26315271553053477373 / 1919190
B(38) =                             2929993913841559 / 6
B(40) =                       -261082718496449122051 / 13530
B(42) =                       1520097643918070802691 / 1806
B(44) =                     -27833269579301024235023 / 690
B(46) =                     596451111593912163277961 / 282
B(48) =                -5609403368997817686249127547 / 46410
B(50) =                  495057205241079648212477525 / 66
B(52) =              -801165718135489957347924991853 / 1590
B(54) =             29149963634884862421418123812691 / 798
B(56) =          -2479392929313226753685415739663229 / 870
B(58) =          84483613348880041862046775994036021 / 354
B(60) = -1215233140483755572040304994079820246041491 / 56786730

```



###  Using System.Numerics

Algo based on the example provided in the header of this RC page (the one from Wikipedia). <br/> Extra feature - one can override the default of 60 by supplying a suitable number on the command line.  The column widths are not hard-coded, but will adapt to the widths of the items listed.

```c#
using System;
using System.Numerics;
using System.Collections.Generic;

namespace bern
{
    class Program
    {
        struct BerNum { public int index; public BigInteger Numer, Denomin; };
        static int w1 = 1, w2 = 1; // widths for formatting output
        static int max = 60; // default maximum, can override on command line

        // returns nth Bernoulli number
        static BerNum CalcBernoulli(int n)
        {
            BerNum res;
            BigInteger f;
            BigInteger[] nu = new BigInteger[n + 1],
                         de = new BigInteger[n + 1];
            for (int m = 0; m <= n; m++)
            {
                nu[m] = 1; de[m] = m + 1;
                for (int j = m; j > 0; j--)
                    if ((f = BigInteger.GreatestCommonDivisor(
                        nu[j - 1] = j * (de[j] * nu[j - 1] - de[j - 1] * nu[j]),
                        de[j - 1] *= de[j])) != BigInteger.One)
                    { nu[j - 1] /= f; de[j - 1] /= f; }
            }
            res.index = n; res.Numer = nu[0]; res.Denomin = de[0];
            w1 = Math.Max(n.ToString().Length, w1);             // ratchet up widths
            w2 = Math.Max(res.Numer.ToString().Length, w2);
            if (max > 50) Console.Write("."); // progress dots appear for larger values
            return res;
        }

        static void Main(string[] args)
        {
            List<BerNum> BNumbList = new List<BerNum>();
            // defaults to 60 when no (or invalid) command line parameter is present
            if (args.Length > 0) {
                int.TryParse(args[0], out max);
                if (max < 1 || max > Int16.MaxValue) max = 60;
                if (args[0] == "0") max = 0;
            }
            for (int i = 0; i <= max; i++) // fill list with values
            {
                BerNum BNumb = CalcBernoulli(i);
                if (BNumb.Numer != BigInteger.Zero) BNumbList.Add(BNumb);
            }
            if (max > 50) Console.WriteLine();
            string strFmt = "B({0, " + w1.ToString() + "}) = {1, " + w2.ToString() + "} / {2}";
            // display formatted list
            foreach (BerNum bn in BNumbList)
                Console.WriteLine(strFmt , bn.index, bn.Numer, bn.Denomin);
            if (System.Diagnostics.Debugger.IsAttached) Console.Read();
        }
    }
}

```

Default (nothing entered on command line):

```txt
.............................................................
B( 0) =                                            1 / 1
B( 1) =                                            1 / 2
B( 2) =                                            1 / 6
B( 4) =                                           -1 / 30
B( 6) =                                            1 / 42
B( 8) =                                           -1 / 30
B(10) =                                            5 / 66
B(12) =                                         -691 / 2730
B(14) =                                            7 / 6
B(16) =                                        -3617 / 510
B(18) =                                        43867 / 798
B(20) =                                      -174611 / 330
B(22) =                                       854513 / 138
B(24) =                                   -236364091 / 2730
B(26) =                                      8553103 / 6
B(28) =                                 -23749461029 / 870
B(30) =                                8615841276005 / 14322
B(32) =                               -7709321041217 / 510
B(34) =                                2577687858367 / 6
B(36) =                        -26315271553053477373 / 1919190
B(38) =                             2929993913841559 / 6
B(40) =                       -261082718496449122051 / 13530
B(42) =                       1520097643918070802691 / 1806
B(44) =                     -27833269579301024235023 / 690
B(46) =                     596451111593912163277961 / 282
B(48) =                -5609403368997817686249127547 / 46410
B(50) =                  495057205241079648212477525 / 66
B(52) =              -801165718135489957347924991853 / 1590
B(54) =             29149963634884862421418123812691 / 798
B(56) =          -2479392929313226753685415739663229 / 870
B(58) =          84483613348880041862046775994036021 / 354
B(60) = -1215233140483755572040304994079820246041491 / 56786730
```


Output with "8" entered on command line:

```txt
B(0) =  1 / 1
B(1) =  1 / 2
B(2) =  1 / 6
B(4) = -1 / 30
B(6) =  1 / 42
B(8) = -1 / 30
```

Output with "126" entered on the command line:

```txt
...............................................................................................................................
B(  0) =                                                                                                                      1 / 1
B(  1) =                                                                                                                      1 / 2
B(  2) =                                                                                                                      1 / 6
B(  4) =                                                                                                                     -1 / 30
B(  6) =                                                                                                                      1 / 42
B(  8) =                                                                                                                     -1 / 30
B( 10) =                                                                                                                      5 / 66
B( 12) =                                                                                                                   -691 / 2730
B( 14) =                                                                                                                      7 / 6
B( 16) =                                                                                                                  -3617 / 510
B( 18) =                                                                                                                  43867 / 798
B( 20) =                                                                                                                -174611 / 330
B( 22) =                                                                                                                 854513 / 138
B( 24) =                                                                                                             -236364091 / 2730
B( 26) =                                                                                                                8553103 / 6
B( 28) =                                                                                                           -23749461029 / 870
B( 30) =                                                                                                          8615841276005 / 14322
B( 32) =                                                                                                         -7709321041217 / 510
B( 34) =                                                                                                          2577687858367 / 6
B( 36) =                                                                                                  -26315271553053477373 / 1919190
B( 38) =                                                                                                       2929993913841559 / 6
B( 40) =                                                                                                 -261082718496449122051 / 13530
B( 42) =                                                                                                 1520097643918070802691 / 1806
B( 44) =                                                                                               -27833269579301024235023 / 690
B( 46) =                                                                                               596451111593912163277961 / 282
B( 48) =                                                                                          -5609403368997817686249127547 / 46410
B( 50) =                                                                                            495057205241079648212477525 / 66
B( 52) =                                                                                        -801165718135489957347924991853 / 1590
B( 54) =                                                                                       29149963634884862421418123812691 / 798
B( 56) =                                                                                    -2479392929313226753685415739663229 / 870
B( 58) =                                                                                    84483613348880041862046775994036021 / 354
B( 60) =                                                                           -1215233140483755572040304994079820246041491 / 56786730
B( 62) =                                                                                 12300585434086858541953039857403386151 / 6
B( 64) =                                                                            -106783830147866529886385444979142647942017 / 510
B( 66) =                                                                         1472600022126335654051619428551932342241899101 / 64722
B( 68) =                                                                          -78773130858718728141909149208474606244347001 / 30
B( 70) =                                                                      1505381347333367003803076567377857208511438160235 / 4686
B( 72) =                                                               -5827954961669944110438277244641067365282488301844260429 / 140100870
B( 74) =                                                                     34152417289221168014330073731472635186688307783087 / 6
B( 76) =                                                                 -24655088825935372707687196040585199904365267828865801 / 30
B( 78) =                                                              414846365575400828295179035549542073492199375372400483487 / 3318
B( 80) =                                                         -4603784299479457646935574969019046849794257872751288919656867 / 230010
B( 82) =                                                          1677014149185145836823154509786269900207736027570253414881613 / 498
B( 84) =                                                   -2024576195935290360231131160111731009989917391198090877281083932477 / 3404310
B( 86) =                                                        660714619417678653573847847426261496277830686653388931761996983 / 6
B( 88) =                                                -1311426488674017507995511424019311843345750275572028644296919890574047 / 61410
B( 90) =                                              1179057279021082799884123351249215083775254949669647116231545215727922535 / 272118
B( 92) =                                             -1295585948207537527989427828538576749659341483719435143023316326829946247 / 1410
B( 94) =                                              1220813806579744469607301679413201203958508415202696621436215105284649447 / 6
B( 96) =                                     -211600449597266513097597728109824233673043954389060234150638733420050668349987259 / 4501770
B( 98) =                                          67908260672905495624051117546403605607342195728504487509073961249992947058239 / 6
B(100) =                                   -94598037819122125295227433069493721872702841533066936133385696204311395415197247711 / 33330
B(102) =                                  3204019410860907078243020782116241775491817197152717450679002501086861530836678158791 / 4326
B(104) =                               -319533631363830011287103352796174274671189606078272738327103470162849568365549721224053 / 1590
B(106) =                              36373903172617414408151820151593427169231298640581690038930816378281879873386202346572901 / 642
B(108) =                     -3469342247847828789552088659323852541399766785760491146870005891371501266319724897592306597338057 / 209191710
B(110) =                         7645992940484742892248134246724347500528752413412307906683593870759797606269585779977930217515 / 1518
B(112) =                  -2650879602155099713352597214685162014443151499192509896451788427680966756514875515366781203552600109 / 1671270
B(114) =                     21737832319369163333310761086652991475721156679090831360806110114933605484234593650904188618562649 / 42
B(116) =                -309553916571842976912513458033841416869004128064329844245504045721008957524571968271388199595754752259 / 1770
B(118) =                 366963119969713111534947151585585006684606361080699204301059440676414485045806461889371776354517095799 / 6
B(120) =     -51507486535079109061843996857849983274095170353262675213092869167199297474922985358811329367077682677803282070131 / 2328255930
B(122) =            49633666079262581912532637475990757438722790311060139770309311793150683214100431329033113678098037968564431 / 6
B(124) =        -95876775334247128750774903107542444620578830013297336819553512729358593354435944413631943610268472689094609001 / 30
B(126) = 5556330281949274850616324408918951380525567307126747246796782304333594286400508981287241419934529638692081513802696639 / 4357878

```




## Clojure



```clojure


ns test-project-intellij.core
  (:gen-class))

(defn a-t [n]
  " Used Akiyama-Tanigawa algorithm with a single loop rather than double nested loop "
  " Clojure does fractional arithmetic automatically so that part is easy "
  (loop [m 0
         j m
         A (vec (map #(/ 1 %) (range 1 (+ n 2))))] ; Prefil A(m) with 1/(m+1), for m = 1 to n
    (cond                                          ; Three way conditional allows single loop
      (>= j 1) (recur m (dec j) (assoc A (dec j) (* j (- (nth A (dec j)) (nth A j))))) ; A[j-1] ← j×(A[j-1] - A[j]) ;
      (< m n) (recur (inc m) (inc m) A)                                                 ; increment m, reset j = m
      :else (nth A 0))))

(defn format-ans [ans]
  " Formats answer so that '/' is aligned for all answers "
  (if (= ans 1)
  (format "%50d / %8d" 1 1)
  (format "%50d / %8d" (numerator ans) (denominator ans))))

;; Generate a set of results for [0 1 2 4 ... 60]
(doseq [q (flatten [0 1 (range 2 62 2)])
        :let [ans (a-t q)]]
  (println q ":" (format-ans ans)))


```

```txt

0 :                                                  1 /        1
1 :                                                  1 /        2
2 :                                                  1 /        6
4 :                                                 -1 /       30
6 :                                                  1 /       42
8 :                                                 -1 /       30
10 :                                                  5 /       66
12 :                                               -691 /     2730
14 :                                                  7 /        6
16 :                                              -3617 /      510
18 :                                              43867 /      798
20 :                                            -174611 /      330
22 :                                             854513 /      138
24 :                                         -236364091 /     2730
26 :                                            8553103 /        6
28 :                                       -23749461029 /      870
30 :                                      8615841276005 /    14322
32 :                                     -7709321041217 /      510
34 :                                      2577687858367 /        6
36 :                              -26315271553053477373 /  1919190
38 :                                   2929993913841559 /        6
40 :                             -261082718496449122051 /    13530
42 :                             1520097643918070802691 /     1806
44 :                           -27833269579301024235023 /      690
46 :                           596451111593912163277961 /      282
48 :                      -5609403368997817686249127547 /    46410
50 :                        495057205241079648212477525 /       66
52 :                    -801165718135489957347924991853 /     1590
54 :                   29149963634884862421418123812691 /      798
56 :                -2479392929313226753685415739663229 /      870
58 :                84483613348880041862046775994036021 /      354
60 :       -1215233140483755572040304994079820246041491 / 56786730

```



## Common Lisp

An implementation of the simple algorithm.

Be advised that the pseudocode algorithm specifies (j * (a[j-1] - a[j])) in the inner loop; implementing that as-is gives the wrong value (1/2) where n = 1, whereas subtracting a[j]-a[j-1] yields the correct value (B[1]=-1/2). See [http://oeis.org/A027641 the numerator list].


```lisp
(defun bernouilli (n)
  (loop with a = (make-array (list (1+ n)))
     for m from 0 to n do
       (setf (aref a m) (/ 1 (+ m 1)))
       (loop for j from m downto 1 do
            (setf (aref a (- j 1))
                  (* j (- (aref a j) (aref a (- j 1))))))
     finally (return (aref a 0))))

;;Print outputs to stdout:

(loop for n from 0 to 60 do
     (let ((b (bernouilli n)))
       (when (not (zerop b))
         (format t "~a: ~a~%" n b))))


;;For the "extra credit" challenge, we need to align the slashes.

(let (results)
  ;;collect the results
  (loop for n from 0 to 60 do
       (let ((b (bernouilli n)))
         (when (not (zerop b)) (push (cons b n) results))))
  ;;parse the numerators into strings; save the greatest length in max-length
  (let ((max-length (apply #'max (mapcar (lambda (r)
                                           (length (format nil "~a" (numerator r))))
                                         (mapcar #'car results)))))
    ;;Print the numbers with using the fixed-width formatter: ~Nd, where N is
    ;;the number of leading spaces. We can't just pass in the width variable
    ;;but we can splice together a formatting string that includes it.

    ;;We also can't use the fixed-width formatter on a ratio, so we have to split
    ;;the ratio and splice it back together like idiots.
    (loop for n in (mapcar #'cdr (reverse results))
          for r in (mapcar #'car (reverse results)) do
         (format t (concatenate 'string
                                "B(~2d): ~"
                                (format nil "~a" max-length)
                                "d/~a~%")
                 n
                 (numerator r)
                 (denominator r)))))
```


```txt

B( 0):                                            1/1
B( 1):                                           -1/2
B( 2):                                            1/6
B( 4):                                           -1/30
B( 6):                                            1/42
B( 8):                                           -1/30
B(10):                                            5/66
B(12):                                         -691/2730
B(14):                                            7/6
B(16):                                        -3617/510
B(18):                                        43867/798
B(20):                                      -174611/330
B(22):                                       854513/138
B(24):                                   -236364091/2730
B(26):                                      8553103/6
B(28):                                 -23749461029/870
B(30):                                8615841276005/14322
B(32):                               -7709321041217/510
B(34):                                2577687858367/6
B(36):                        -26315271553053477373/1919190
B(38):                             2929993913841559/6
B(40):                       -261082718496449122051/13530
B(42):                       1520097643918070802691/1806
B(44):                     -27833269579301024235023/690
B(46):                     596451111593912163277961/282
B(48):                -5609403368997817686249127547/46410
B(50):                  495057205241079648212477525/66
B(52):              -801165718135489957347924991853/1590
B(54):             29149963634884862421418123812691/798
B(56):          -2479392929313226753685415739663229/870
B(58):          84483613348880041862046775994036021/354
B(60): -1215233140483755572040304994079820246041491/56786730

```



## Crystal


```ruby
require "big"

class Bernoulli
  include Iterator(Tuple(Int32, BigRational))

  def initialize
    @a = [] of BigRational
    @m = 0
  end

  def next
    @a << BigRational.new(1, @m+1)
    @m.downto(1) { |j| @a[j-1] = j*(@a[j-1] - @a[j]) }
    v = @m.odd? && @m != 1 ? BigRational.new(0, 1) : @a.first
    return {@m, v}
  ensure
    @m += 1
  end
end

b = Bernoulli.new
bn = b.first(61).to_a

max_width = bn.map { |_, v| v.numerator.to_s.size }.max
bn.reject { |i, v| v.zero? }.each do |i, v|
  puts "B(%2i) = %*i/%i" % [i, max_width, v.numerator, v.denominator]
end

```


Version 1: compute each number separately.

```ruby
require "big"

def bernoulli(n)
    ar = [] of BigRational
    (0..n).each do |m|
        ar << BigRational.new(1, m+1)
        m.downto(1) { |j| ar[j-1] = j * (ar[j-1] - ar[j]) }
    end
    ar[0] # (which is Bn)
end

b_nums = (0..61).map { |i| bernoulli(i) }
width  = b_nums.map{ |b| b.numerator.to_s.size }.max
b_nums.each_with_index { |b,i| puts "B(%2i) = %*i/%i" % [i, width, b.numerator, b.denominator] unless b.zero? }

```



Version 2: create faster generator to compute array of numbers once.

```ruby
require "big"

def bernoulli2(limit)
    ar = [] of BigRational
    (0..limit).each do |m|
      ar << BigRational.new(1, m+1)
      m.downto(1) { |j| ar[j-1] = j * (ar[j-1] - ar[j]) }
      yield ar[0] # use Bn value in required block
    end
end

b_nums = [] of BigRational
bernoulli2(61){ |b| b_nums << b }
width  = b_nums.map{ |b| b.numerator.to_s.size }.max
b_nums.each_with_index { |b,i| puts "B(%2i) = %*i/%i" % [i, width, b.numerator, b.denominator] unless b.zero? }

```

```txt

B( 0) =                                            1/1
B( 1) =                                            1/2
B( 2) =                                            1/6
B( 4) =                                           -1/30
B( 6) =                                            1/42
B( 8) =                                           -1/30
B(10) =                                            5/66
B(12) =                                         -691/2730
B(14) =                                            7/6
B(16) =                                        -3617/510
B(18) =                                        43867/798
B(20) =                                      -174611/330
B(22) =                                       854513/138
B(24) =                                   -236364091/2730
B(26) =                                      8553103/6
B(28) =                                 -23749461029/870
B(30) =                                8615841276005/14322
B(32) =                               -7709321041217/510
B(34) =                                2577687858367/6
B(36) =                        -26315271553053477373/1919190
B(38) =                             2929993913841559/6
B(40) =                       -261082718496449122051/13530
B(42) =                       1520097643918070802691/1806
B(44) =                     -27833269579301024235023/690
B(46) =                     596451111593912163277961/282
B(48) =                -5609403368997817686249127547/46410
B(50) =                  495057205241079648212477525/66
B(52) =              -801165718135489957347924991853/1590
B(54) =             29149963634884862421418123812691/798
B(56) =          -2479392929313226753685415739663229/870
B(58) =          84483613348880041862046775994036021/354
B(60) = -1215233140483755572040304994079820246041491/56786730

```



## D

This uses the D module from the Arithmetic/Rational task.
```d
import std.stdio, std.range, std.algorithm, std.conv, arithmetic_rational;

auto bernoulli(in uint n) pure nothrow /*@safe*/ {
    auto A = new Rational[n + 1];
    foreach (immutable m; 0 .. n + 1) {
        A[m] = Rational(1, m + 1);
        foreach_reverse (immutable j; 1 .. m + 1)
            A[j - 1] = j * (A[j - 1] - A[j]);
    }
    return A[0];
}

void main() {
    immutable berns = 61.iota.map!bernoulli.enumerate.filter!(t => t[1]).array;
    immutable width = berns.map!(b => b[1].numerator.text.length).reduce!max;
    foreach (immutable b; berns)
        writefln("B(%2d) = %*d/%d", b[0], width, b[1].tupleof);
}
```

The output is exactly the same as the Python entry.


## EchoLisp


{{improve|EchoLisp|
 Try to show '''B<sub>1</sub>'''   within the output proper as   -1/2.}}

Only 'small' rationals are supported in EchoLisp, i.e numerator and demominator < 2^31. So, we create a class of 'large' rationals, supported by the bigint library, and then apply the magic formula.

```lisp

(lib 'bigint) ;; lerge numbers
(lib 'gloops) ;; classes

(define-class Rational null ((a :initform #0) (b :initform #1)))
(define-method tostring  (Rational) (lambda (r) (format "%50d / %d" r.a r.b)))
(define-method normalize (Rational) (lambda (r) ;; divide a and b by gcd
		 (let ((g (gcd r.a r.b)))
		 (set! r.a (/ r.a g)) (set! r.b (/ r.b g))
 		 (when (< r.b 0) (set! r.a ( - r.a)) (set! r.b (- r.b))) ;; denominator > 0
 		r)))

(define-method initialize (Rational) (lambda (r) (normalize r)))
(define-method add (Rational) (lambda (r n)  ;; + Rational any number
			(normalize (Rational (+ (* (+ #0 n) r.b) r.a) r.b))))
(define-method add (Rational Rational) (lambda (r q) ;;; + Rational Rational
			(normalize (Rational (+ (* r.a q.b) (* r.b q.a)) (* r.b q.b)))))
(define-method sub (Rational Rational) (lambda (r q)
			(normalize (Rational (- (* r.a q.b) (* r.b q.a)) (* r.b q.b)))))
(define-method mul (Rational Rational) (lambda (r q)
			(normalize (Rational  (* r.a q.a)  (* r.b q.b)))))
(define-method mul (Rational) (lambda (r n)
			(normalize (Rational  (* r.a (+ #0 n))  r.b ))))
(define-method div (Rational Rational) (lambda (r q)
			(normalize (Rational  (* r.a q.b)  (* r.b q.a)))))

```

```lisp

;; Bernoulli numbers
;; http://rosettacode.org/wiki/Bernoulli_numbers
(define A (make-vector 100 0))

(define (B n)
(for ((m (1+ n))) ;; #1 creates a large integer
	(vector-set! A m (Rational #1 (+ #1 m)))
	(for ((j (in-range m 0 -1)))
	  (vector-set! A (1- j)
	  	(mul (sub (vector-ref A (1- j)) (vector-ref A j)) j))))
	  (vector-ref A 0))

    (for ((b (in-range 0 62 2))) (writeln b (B b)))  →

0                                                      1 / 1
2                                                      1 / 6
4                                                     -1 / 30
6                                                      1 / 42
8                                                     -1 / 30
10                                                      5 / 66
12                                                   -691 / 2730
14                                                      7 / 6
16                                                  -3617 / 510
18                                                  43867 / 798
20                                                -174611 / 330
22                                                 854513 / 138
24                                             -236364091 / 2730
26                                                8553103 / 6
28                                           -23749461029 / 870
30                                          8615841276005 / 14322
32                                         -7709321041217 / 510
34                                          2577687858367 / 6
36                                  -26315271553053477373 / 1919190
38                                       2929993913841559 / 6
40                                 -261082718496449122051 / 13530
42                                 1520097643918070802691 / 1806
44                               -27833269579301024235023 / 690
46                               596451111593912163277961 / 282
48                          -5609403368997817686249127547 / 46410
50                            495057205241079648212477525 / 66
52                        -801165718135489957347924991853 / 1590
54                       29149963634884862421418123812691 / 798
56                    -2479392929313226753685415739663229 / 870
58                    84483613348880041862046775994036021 / 354
60           -1215233140483755572040304994079820246041491 / 56786730

(B 1) → 1 / 2

```



## Elixir


```elixir
defmodule Bernoulli do
  defmodule Rational do
    import Kernel, except: [div: 2]

    defstruct numerator: 0, denominator: 1

    def new(numerator, denominator\\1) do
      sign = if numerator * denominator < 0, do: -1, else: 1
      {numerator, denominator} = {abs(numerator), abs(denominator)}
      gcd = gcd(numerator, denominator)
      %Rational{numerator: sign * Kernel.div(numerator, gcd),
                denominator: Kernel.div(denominator, gcd)}
    end

    def sub(a, b) do
      new(a.numerator * b.denominator - b.numerator * a.denominator,
          a.denominator * b.denominator)
    end

    def mul(a, b) when is_integer(a) do
      new(a * b.numerator, b.denominator)
    end

    defp gcd(a,0), do: a
    defp gcd(a,b), do: gcd(b, rem(a,b))
  end

  def numbers(n) do
    Stream.transform(0..n, {}, fn m,acc ->
      acc = Tuple.append(acc, Rational.new(1,m+1))
      if m>0 do
        new =
          Enum.reduce(m..1, acc, fn j,ar ->
            put_elem(ar, j-1, Rational.mul(j, Rational.sub(elem(ar,j-1), elem(ar,j))))
          end)
        {[elem(new,0)], new}
      else
        {[elem(acc,0)], acc}
      end
    end) |> Enum.to_list
  end

  def task(n \\ 61) do
    b_nums = numbers(n)
    width  = Enum.map(b_nums, fn b -> b.numerator |> to_string |> String.length end)
             |> Enum.max
    format = 'B(~2w) = ~#{width}w / ~w~n'
    Enum.with_index(b_nums)
    |> Enum.each(fn {b,i} ->
         if b.numerator != 0, do: :io.fwrite format, [i, b.numerator, b.denominator]
       end)
  end
end

Bernoulli.task
```


```txt

B( 0) =                                            1 / 1
B( 1) =                                            1 / 2
B( 2) =                                            1 / 6
B( 4) =                                           -1 / 30
B( 6) =                                            1 / 42
B( 8) =                                           -1 / 30
B(10) =                                            5 / 66
B(12) =                                         -691 / 2730
B(14) =                                            7 / 6
B(16) =                                        -3617 / 510
B(18) =                                        43867 / 798
B(20) =                                      -174611 / 330
B(22) =                                       854513 / 138
B(24) =                                   -236364091 / 2730
B(26) =                                      8553103 / 6
B(28) =                                 -23749461029 / 870
B(30) =                                8615841276005 / 14322
B(32) =                               -7709321041217 / 510
B(34) =                                2577687858367 / 6
B(36) =                        -26315271553053477373 / 1919190
B(38) =                             2929993913841559 / 6
B(40) =                       -261082718496449122051 / 13530
B(42) =                       1520097643918070802691 / 1806
B(44) =                     -27833269579301024235023 / 690
B(46) =                     596451111593912163277961 / 282
B(48) =                -5609403368997817686249127547 / 46410
B(50) =                  495057205241079648212477525 / 66
B(52) =              -801165718135489957347924991853 / 1590
B(54) =             29149963634884862421418123812691 / 798
B(56) =          -2479392929313226753685415739663229 / 870
B(58) =          84483613348880041862046775994036021 / 354
B(60) = -1215233140483755572040304994079820246041491 / 56786730

```


=={{header|F sharp|F#}}==
```fsharp

open MathNet.Numerics
open System
open System.Collections.Generic

let calculateBernoulli n =
    let ℚ(x) = BigRational.FromInt x
    let A = Array.init<BigRational> (n+1) (fun x -> ℚ(x+1))

    for m in [1..n] do
        A.[m] <- ℚ(1) / (ℚ(m) + ℚ(1))
        for j in [m..(-1)..1] do
            A.[j-1] <- ℚ(j) * (A.[j-1] - A.[j])
    A.[0]

[<EntryPoint>]
let main argv =
    for n in [0..60] do
        let bernoulliNumber = calculateBernoulli n
        match bernoulliNumber.Numerator.IsZero with
        | false ->
            let formatedString = String.Format("B({0, 2}) = {1, 44} / {2}", n, bernoulliNumber.Numerator, bernoulliNumber.Denominator)
            printfn "%s" formatedString
        | true ->
            printf ""
    0

```

```txt

B( 0) =                                            1 / 1
B( 1) =                                            1 / 2
B( 2) =                                            1 / 6
B( 4) =                                           -1 / 30
B( 6) =                                            1 / 42
B( 8) =                                           -1 / 30
B(10) =                                            5 / 66
B(12) =                                         -691 / 2730
B(14) =                                            7 / 6
B(16) =                                        -3617 / 510
B(18) =                                        43867 / 798
B(20) =                                      -174611 / 330
B(22) =                                       854513 / 138
B(24) =                                   -236364091 / 2730
B(26) =                                      8553103 / 6
B(28) =                                 -23749461029 / 870
B(30) =                                8615841276005 / 14322
B(32) =                               -7709321041217 / 510
B(34) =                                2577687858367 / 6
B(36) =                        -26315271553053477373 / 1919190
B(38) =                             2929993913841559 / 6
B(40) =                       -261082718496449122051 / 13530
B(42) =                       1520097643918070802691 / 1806
B(44) =                     -27833269579301024235023 / 690
B(46) =                     596451111593912163277961 / 282
B(48) =                -5609403368997817686249127547 / 46410
B(50) =                  495057205241079648212477525 / 66
B(52) =              -801165718135489957347924991853 / 1590
B(54) =             29149963634884862421418123812691 / 798
B(56) =          -2479392929313226753685415739663229 / 870
B(58) =          84483613348880041862046775994036021 / 354
B(60) = -1215233140483755572040304994079820246041491 / 56786730

```


## Factor

One could use the "bernoulli" word from the math.extras vocabulary as follows:
<lang>IN: scratchpad
    [
      0  1 1 "%2d : %d / %d\n" printf
      1 -1 2 "%2d : %d / %d\n" printf
      30 iota [
        1 + 2 * dup bernoulli [ numerator ] [ denominator ] bi
        "%2d : %d / %d\n" printf
      ] each
    ] time
 0 : 1 / 1
 1 : -1 / 2
 2 : 1 / 6
 4 : -1 / 30
 6 : 1 / 42
 8 : -1 / 30
10 : 5 / 66
12 : -691 / 2730
14 : 7 / 6
16 : -3617 / 510
18 : 43867 / 798
20 : -174611 / 330
22 : 854513 / 138
24 : -236364091 / 2730
26 : 8553103 / 6
28 : -23749461029 / 870
30 : 8615841276005 / 14322
32 : -7709321041217 / 510
34 : 2577687858367 / 6
36 : -26315271553053477373 / 1919190
38 : 2929993913841559 / 6
40 : -261082718496449122051 / 13530
42 : 1520097643918070802691 / 1806
44 : -27833269579301024235023 / 690
46 : 596451111593912163277961 / 282
48 : -5609403368997817686249127547 / 46410
50 : 495057205241079648212477525 / 66
52 : -801165718135489957347924991853 / 1590
54 : 29149963634884862421418123812691 / 798
56 : -2479392929313226753685415739663229 / 870
58 : 84483613348880041862046775994036021 / 354
60 : -1215233140483755572040304994079820246041491 / 56786730
Running time: 0.00489444 seconds
```

Alternatively a method described by Brent and Harvey (2011) in "Fast computation of Bernoulli, Tangent and Secant numbers" https://arxiv.org/pdf/1108.0286.pdf is shown.
<lang>:: bernoulli-numbers ( n -- )
  n 1 + 0 <array> :> tab
  1 1 tab set-nth
  2 n [a,b] [| k |
    k 1 - dup
    tab nth *
    k tab set-nth
  ] each
  2 n [a,b] [| k |
    k n [a,b] [| j |
      j tab nth
      j k - 2 + *
      j 1 - tab nth
      j k - * +
      j tab set-nth
    ] each
  ] each
  1 :> s!
  1 n [a,b] [| k |
    k 2 * dup
    2^ dup 1 - *
    k tab nth
    swap / *
    s * k tab set-nth
    s -1 * s!
  ] each

  0  1 1 "%2d : %d / %d\n" printf
  1 -1 2 "%2d : %d / %d\n" printf
  1 n [a,b] [| k |
    k 2 * k tab nth
    [ numerator ] [ denominator ] bi
    "%2d : %d / %d\n" printf
  ] each
;
```

It gives the same result as the native implementation, but is slightly faster.
<lang>[ 30 bernoulli-numbers ] time
...
Running time: 0.004331652 seconds
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Bernoulli_numbers this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## FreeBASIC

```freebasic
' version 08-10-2016
' compile with: fbc -s console
' uses gmp

#Include Once "gmp.bi"

#Define max 60

Dim As Long n
Dim As ZString Ptr gmp_str :gmp_str = Allocate(1000) ' 1000 char
Dim Shared As Mpq_ptr tmp, big_j
tmp = Allocate(Len(__mpq_struct)) :Mpq_init(tmp)
big_j = Allocate(Len(__mpq_struct)) :Mpq_init(big_j)

Dim Shared As Mpq_ptr a(max), b(max)
For n = 0 To max
  A(n) = Allocate(Len(__mpq_struct)) :Mpq_init(A(n))
  B(n) = Allocate(Len(__mpq_struct)) :Mpq_init(B(n))
Next

Function Bernoulli(n As Integer) As Mpq_ptr

  Dim As Long m, j

  For m = 0 To n
    Mpq_set_ui(A(m), 1, m + 1)
    For j = m To 1 Step - 1
      Mpq_sub(tmp, A(j - 1), A(j))
      Mpq_set_ui(big_j, j, 1)                 'big_j = j
      Mpq_mul(A(j - 1), big_j, tmp)
    Next
  Next

  Return A(0)
End Function

' ------=< MAIN >=------

For n = 0 To max
  Mpq_set(B(n), Bernoulli(n))
  Mpq_get_str(gmp_str, 10, B(n))
  If *gmp_str <> "0" Then
    If *gmp_str = "1" Then *gmp_str = "1/1"
    Print Using "B(##) = "; n;
    Print Space(45 - InStr(*gmp_str, "/")); *gmp_str
  End If
Next


' empty keyboard buffer
While Inkey <> "" :Wend
Print :Print "hit any key to end program"
Sleep
End
```

```txt
B( 0) =                                            1/1
B( 1) =                                            1/2
B( 2) =                                            1/6
B( 4) =                                           -1/30
B( 6) =                                            1/42
B( 8) =                                           -1/30
B(10) =                                            5/66
B(12) =                                         -691/2730
B(14) =                                            7/6
B(16) =                                        -3617/510
B(18) =                                        43867/798
B(20) =                                      -174611/330
B(22) =                                       854513/138
B(24) =                                   -236364091/2730
B(26) =                                      8553103/6
B(28) =                                 -23749461029/870
B(30) =                                8615841276005/14322
B(32) =                               -7709321041217/510
B(34) =                                2577687858367/6
B(36) =                        -26315271553053477373/1919190
B(38) =                             2929993913841559/6
B(40) =                       -261082718496449122051/13530
B(42) =                       1520097643918070802691/1806
B(44) =                     -27833269579301024235023/690
B(46) =                     596451111593912163277961/282
B(48) =                -5609403368997817686249127547/46410
B(50) =                  495057205241079648212477525/66
B(52) =              -801165718135489957347924991853/1590
B(54) =             29149963634884862421418123812691/798
B(56) =          -2479392929313226753685415739663229/870
B(58) =          84483613348880041862046775994036021/354
B(60) = -1215233140483755572040304994079820246041491/56786730
```



## FunL

FunL has pre-defined function <code>B</code> in module <code>integers</code>, which is defined as:

```funl
import integers.choose

def B( n ) = sum( 1/(k + 1)*sum((if 2|r then 1 else -1)*choose(k, r)*(r^n) | r <- 0..k) | k <- 0..n )

for i <- 0..60 if i == 1 or 2|i
  printf( "B(%2d) = %s\n", i, B(i) )
```


```txt

B( 0) = 1
B( 1) = -1/2
B( 2) = 1/6
B( 4) = -1/30
B( 6) = 1/42
B( 8) = -1/30
B(10) = 5/66
B(12) = -691/2730
B(14) = 7/6
B(16) = -3617/510
B(18) = 43867/798
B(20) = -174611/330
B(22) = 854513/138
B(24) = -236364091/2730
B(26) = 8553103/6
B(28) = -23749461029/870
B(30) = 8615841276005/14322
B(32) = -7709321041217/510
B(34) = 2577687858367/6
B(36) = -26315271553053477373/1919190
B(38) = 2929993913841559/6
B(40) = -261082718496449122051/13530
B(42) = 1520097643918070802691/1806
B(44) = -27833269579301024235023/690
B(46) = 596451111593912163277961/282
B(48) = -5609403368997817686249127547/46410
B(50) = 495057205241079648212477525/66
B(52) = -801165718135489957347924991853/1590
B(54) = 29149963634884862421418123812691/798
B(56) = -2479392929313226753685415739663229/870
B(58) = 84483613348880041862046775994036021/354
B(60) = -1215233140483755572040304994079820246041491/56786730

```



## GAP



```gap
for a in Filtered(List([0 .. 60], n -> [n, Bernoulli(n)]), x -> x[2] <> 0) do
    Print(a, "\n");
od;

[ 0, 1 ]
[ 1, -1/2 ]
[ 2, 1/6 ]
[ 4, -1/30 ]
[ 6, 1/42 ]
[ 8, -1/30 ]
[ 10, 5/66 ]
[ 12, -691/2730 ]
[ 14, 7/6 ]
[ 16, -3617/510 ]
[ 18, 43867/798 ]
[ 20, -174611/330 ]
[ 22, 854513/138 ]
[ 24, -236364091/2730 ]
[ 26, 8553103/6 ]
[ 28, -23749461029/870 ]
[ 30, 8615841276005/14322 ]
[ 32, -7709321041217/510 ]
[ 34, 2577687858367/6 ]
[ 36, -26315271553053477373/1919190 ]
[ 38, 2929993913841559/6 ]
[ 40, -261082718496449122051/13530 ]
[ 42, 1520097643918070802691/1806 ]
[ 44, -27833269579301024235023/690 ]
[ 46, 596451111593912163277961/282 ]
[ 48, -5609403368997817686249127547/46410 ]
[ 50, 495057205241079648212477525/66 ]
[ 52, -801165718135489957347924991853/1590 ]
[ 54, 29149963634884862421418123812691/798 ]
[ 56, -2479392929313226753685415739663229/870 ]
[ 58, 84483613348880041862046775994036021/354 ]
[ 60, -1215233140483755572040304994079820246041491/56786730 ]
```



## Go


```go
package main

import (
	"fmt"
	"math/big"
)

func b(n int) *big.Rat {
	var f big.Rat
	a := make([]big.Rat, n+1)
	for m := range a {
		a[m].SetFrac64(1, int64(m+1))
		for j := m; j >= 1; j-- {
			d := &a[j-1]
			d.Mul(f.SetInt64(int64(j)), d.Sub(d, &a[j]))
		}
	}
	return f.Set(&a[0])
}

func main() {
	for n := 0; n <= 60; n++ {
		if b := b(n); b.Num().BitLen() > 0 {
			fmt.Printf("B(%2d) =%45s/%s\n", n, b.Num(), b.Denom())
		}
	}
}
```

```txt

B( 0) =                                            1/1
B( 1) =                                            1/2
B( 2) =                                            1/6
B( 4) =                                           -1/30
B( 6) =                                            1/42
B( 8) =                                           -1/30
B(10) =                                            5/66
B(12) =                                         -691/2730
B(14) =                                            7/6
B(16) =                                        -3617/510
B(18) =                                        43867/798
B(20) =                                      -174611/330
B(22) =                                       854513/138
B(24) =                                   -236364091/2730
B(26) =                                      8553103/6
B(28) =                                 -23749461029/870
B(30) =                                8615841276005/14322
B(32) =                               -7709321041217/510
B(34) =                                2577687858367/6
B(36) =                        -26315271553053477373/1919190
B(38) =                             2929993913841559/6
B(40) =                       -261082718496449122051/13530
B(42) =                       1520097643918070802691/1806
B(44) =                     -27833269579301024235023/690
B(46) =                     596451111593912163277961/282
B(48) =                -5609403368997817686249127547/46410
B(50) =                  495057205241079648212477525/66
B(52) =              -801165718135489957347924991853/1590
B(54) =             29149963634884862421418123812691/798
B(56) =          -2479392929313226753685415739663229/870
B(58) =          84483613348880041862046775994036021/354
B(60) = -1215233140483755572040304994079820246041491/56786730

```



## Haskell


### =Task algorithm=

This program works as a command line utility, that reads from stdin the number of elements to compute (default 60) and prints them in stdout.
The implementation of the algorithm is in the function bernoullis. The rest is for printing the results.


```Haskell
import Data.Ratio
import System.Environment

main = getArgs >>= printM . defaultArg
  where
    defaultArg as =
      if null as
        then 60
        else read (head as)

printM m =
  mapM_ (putStrLn . printP) .
  takeWhile ((<= m) . fst) . filter (\(_, b) -> b /= 0 % 1) . zip [0 ..] $
  bernoullis

printP (i, r) =
  "B(" ++ show i ++ ") = " ++ show (numerator r) ++ "/" ++ show (denominator r)

bernoullis = map head . iterate (ulli 1) . map berno $ enumFrom 0
  where
    berno i = 1 % (i + 1)
    ulli _ [_] = []
    ulli i (x:y:xs) = (i % 1) * (x - y) : ulli (i + 1) (y : xs)
```

```txt
B(0) = 1/1
B(1) = 1/2
B(2) = 1/6
B(4) = -1/30
B(6) = 1/42
B(8) = -1/30
B(10) = 5/66
B(12) = -691/2730
B(14) = 7/6
B(16) = -3617/510
B(18) = 43867/798
B(20) = -174611/330
B(22) = 854513/138
B(24) = -236364091/2730
B(26) = 8553103/6
B(28) = -23749461029/870
B(30) = 8615841276005/14322
B(32) = -7709321041217/510
B(34) = 2577687858367/6
B(36) = -26315271553053477373/1919190
B(38) = 2929993913841559/6
B(40) = -261082718496449122051/13530
B(42) = 1520097643918070802691/1806
B(44) = -27833269579301024235023/690
B(46) = 596451111593912163277961/282
B(48) = -5609403368997817686249127547/46410
B(50) = 495057205241079648212477525/66
B(52) = -801165718135489957347924991853/1590
B(54) = 29149963634884862421418123812691/798
B(56) = -2479392929313226753685415739663229/870
B(58) = 84483613348880041862046775994036021/354
B(60) = -1215233140483755572040304994079820246041491/56786730
```


====Derivation from Faulhaber's triangle====

```haskell
import Data.Ratio (Ratio, numerator, denominator, (%))
import Data.Bool (bool)

bernouillis :: Integer -> [Rational]
bernouillis = fmap head . tail . scanl faulhaber [] . enumFromTo 0

faulhaber :: [Ratio Integer] -> Integer -> [Ratio Integer]
faulhaber rs n = (:) =<< (-) 1 . sum $ zipWith ((*) . (n %)) [2 ..] rs

-- TEST ---------------------------------------------------
main :: IO ()
main = do
  let xs = bernouillis 60
      w = length (show (numerator (last xs)))
  putStrLn $
    fTable
      "Bernouillis from Faulhaber triangle:\n"
      (show . fst)
      (showRatio w . snd)
      id
      (filter ((0 /=) . snd) $ zip [0 ..] xs)

-- FORMATTING ---------------------------------------------
fTable :: String -> (a -> String) -> (b -> String) -> (a -> b) -> [a] -> String
fTable s xShow fxShow f xs =
  let w = maximum (length . xShow <$> xs)
  in unlines $
     s : fmap (((++) . rjust w ' ' . xShow) <*> ((" -> " ++) . fxShow . f)) xs

showRatio :: Int -> Rational -> String
showRatio w r =
  let d = denominator r
  in rjust w ' ' (show (numerator r)) ++ bool [] (" / " ++ show d) (1 /= d)

rjust :: Int -> a -> [a] -> [a]
rjust n c = drop . length <*> (replicate n c ++)
```

```txt
Bernouillis from Faulhaber triangle:

 0 ->                                            1
 1 ->                                            1 / 2
 2 ->                                            1 / 6
 4 ->                                           -1 / 30
 6 ->                                            1 / 42
 8 ->                                           -1 / 30
10 ->                                            5 / 66
12 ->                                         -691 / 2730
14 ->                                            7 / 6
16 ->                                        -3617 / 510
18 ->                                        43867 / 798
20 ->                                      -174611 / 330
22 ->                                       854513 / 138
24 ->                                   -236364091 / 2730
26 ->                                      8553103 / 6
28 ->                                 -23749461029 / 870
30 ->                                8615841276005 / 14322
32 ->                               -7709321041217 / 510
34 ->                                2577687858367 / 6
36 ->                        -26315271553053477373 / 1919190
38 ->                             2929993913841559 / 6
40 ->                       -261082718496449122051 / 13530
42 ->                       1520097643918070802691 / 1806
44 ->                     -27833269579301024235023 / 690
46 ->                     596451111593912163277961 / 282
48 ->                -5609403368997817686249127547 / 46410
50 ->                  495057205241079648212477525 / 66
52 ->              -801165718135489957347924991853 / 1590
54 ->             29149963634884862421418123812691 / 798
56 ->          -2479392929313226753685415739663229 / 870
58 ->          84483613348880041862046775994036021 / 354
60 -> -1215233140483755572040304994079820246041491 / 56786730
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages:

```unicon
link "rational"

procedure main(args)
    limit := integer(!args) | 60
    every b := bernoulli(i := 0 to limit) do
        if b.numer > 0 then write(right(i,3),": ",align(rat2str(b),60))
end

procedure bernoulli(n)
    (A := table(0))[0] := rational(1,1,1)
    every m := 1 to n do {
        A[m] := rational(1,m+1,1)
        every j := m to 1 by -1 do A[j-1] := mpyrat(rational(j,1,1), subrat(A[j-1],A[j]))
        }
    return A[0]
end

procedure align(r,n)
    return repl(" ",n-find("/",r))||r
end
```


Sample run:

```txt

->bernoulli 60
  0:                                                          (1/1)
  1:                                                          (1/2)
  2:                                                          (1/6)
  4:                                                         (-1/30)
  6:                                                          (1/42)
  8:                                                         (-1/30)
 10:                                                          (5/66)
 12:                                                       (-691/2730)
 14:                                                          (7/6)
 16:                                                      (-3617/510)
 18:                                                      (43867/798)
 20:                                                    (-174611/330)
 22:                                                     (854513/138)
 24:                                                 (-236364091/2730)
 26:                                                    (8553103/6)
 28:                                               (-23749461029/870)
 30:                                              (8615841276005/14322)
 32:                                             (-7709321041217/510)
 34:                                              (2577687858367/6)
 36:                                      (-26315271553053477373/1919190)
 38:                                           (2929993913841559/6)
 40:                                     (-261082718496449122051/13530)
 42:                                     (1520097643918070802691/1806)
 44:                                   (-27833269579301024235023/690)
 46:                                   (596451111593912163277961/282)
 48:                              (-5609403368997817686249127547/46410)
 50:                                (495057205241079648212477525/66)
 52:                            (-801165718135489957347924991853/1590)
 54:                           (29149963634884862421418123812691/798)
 56:                        (-2479392929313226753685415739663229/870)
 58:                        (84483613348880041862046775994036021/354)
 60:               (-1215233140483755572040304994079820246041491/56786730)
->

```



## J


'''Implementation:'''

See [https://code.jsoftware.com/wiki/Essays/Bernoulli_Numbers Bernoulli Numbers Essay] on the J wiki.

```j
B=: {.&1 %. (i. ! ])@>:@i.@x:
```


'''Task:'''


```j
   'B' ,. rplc&'r/_-'"1": (#~ 0 ~: {:"1)(i. ,. B) 61
B 0                                                     1
B 1                                                  -1/2
B 2                                                   1/6
B 4                                                 -1/30
B 6                                                  1/42
B 8                                                 -1/30
B10                                                  5/66
B12                                             -691/2730
B14                                                   7/6
B16                                             -3617/510
B18                                             43867/798
B20                                           -174611/330
B22                                            854513/138
B24                                       -236364091/2730
B26                                             8553103/6
B28                                      -23749461029/870
B30                                   8615841276005/14322
B32                                    -7709321041217/510
B34                                       2577687858367/6
B36                         -26315271553053477373/1919190
B38                                    2929993913841559/6
B40                          -261082718496449122051/13530
B42                           1520097643918070802691/1806
B44                          -27833269579301024235023/690
B46                          596451111593912163277961/282
B48                   -5609403368997817686249127547/46410
B50                        495057205241079648212477525/66
B52                  -801165718135489957347924991853/1590
B54                  29149963634884862421418123812691/798
B56               -2479392929313226753685415739663229/870
B58               84483613348880041862046775994036021/354
B60 -1215233140483755572040304994079820246041491/56786730
```



## Java


```java
import org.apache.commons.math3.fraction.BigFraction;

public class BernoulliNumbers {

    public static void main(String[] args) {
        for (int n = 0; n <= 60; n++) {
            BigFraction b = bernouilli(n);
            if (!b.equals(BigFraction.ZERO))
                System.out.printf("B(%-2d) = %-1s%n", n , b);
        }
    }

    static BigFraction bernouilli(int n) {
        BigFraction[] A = new BigFraction[n + 1];
        for (int m = 0; m <= n; m++) {
            A[m] = new BigFraction(1, (m + 1));
            for (int j = m; j >= 1; j--)
                A[j - 1] = (A[j - 1].subtract(A[j])).multiply(new BigFraction(j));
        }
        return A[0];
    }
}
```


```txt
B(0 ) = 1
B(1 ) = 1 / 2
B(2 ) = 1 / 6
B(4 ) = -1 / 30
B(6 ) = 1 / 42
B(8 ) = -1 / 30
B(10) = 5 / 66
B(12) = -691 / 2730
B(14) = 7 / 6
B(16) = -3617 / 510
B(18) = 43867 / 798
B(20) = -174611 / 330
B(22) = 854513 / 138
B(24) = -236364091 / 2730
B(26) = 8553103 / 6
B(28) = -23749461029 / 870
B(30) = 8615841276005 / 14322
B(32) = -7709321041217 / 510
B(34) = 2577687858367 / 6
B(36) = -26315271553053477373 / 1919190
B(38) = 2929993913841559 / 6
B(40) = -261082718496449122051 / 13530
B(42) = 1520097643918070802691 / 1806
B(44) = -27833269579301024235023 / 690
B(46) = 596451111593912163277961 / 282
B(48) = -5609403368997817686249127547 / 46410
B(50) = 495057205241079648212477525 / 66
B(52) = -801165718135489957347924991853 / 1590
B(54) = 29149963634884862421418123812691 / 798
B(56) = -2479392929313226753685415739663229 / 870
B(58) = 84483613348880041862046775994036021 / 354
B(60) = -1215233140483755572040304994079820246041491 / 56786730
```



## jq

This section uses the Akiyama–Tanigawa algorithm for the second Bernoulli numbers, Bn. Therefore, the sign of B(1) differs from the modern definition.

The implementation presented here is intended for use with a "BigInt" library that uses string representations of decimal integers.
Such a library is at [https://gist.github.com/pkoppstein/d06a123f30c033195841 BigInt.jq].
To make the code in this section self-contained, stubs for the "BigInt" operations are provided in the first subsection.

'''BigInt Stubs''':

```jq
# def negate:
# def lessOrEqual(x; y): # x <= y
# def long_add(x;y):     # x+y
# def long_minus(x;y):   # x-y
# def long_multiply(x;y) # x*y
# def long_divide(x;y):  # x/y => [q,r]
# def long_div(x;y)      # integer division
# def long_mod(x;y)      # %

# In all cases, x and y must be strings

def negate: (- tonumber) | tostring;

def lessOrEqual(num1; num2): (num1|tonumber) <= (num2|tonumber);

def long_add(num1; num2): ((num1|tonumber) + (num2|tonumber)) | tostring;

def long_minus(x;y): ((num1|tonumber) - (num2|tonumber)) | tostring;

# multiply two decimal strings, which may be signed (+ or -)
def long_multiply(num1; num2):
  ((num1|tonumber) * (num2|tonumber)) | tostring;

# return [quotient, remainder]
# 0/0 = 1; n/0 => error
def long_divide(xx;yy):  # x/y => [q,r] imples x == (y * q) + r
  def ld(x;y):
    def abs: if . < 0 then -. else . end;
    (x|abs) as $x | (y|abs) as $y
    | (if (x >= 0 and y > 0) or (x < 0 and y < 0) then 1 else -1 end) as $sign
    | (if x >= 0 then 1 else -1 end) as $sx
    | [$sign * ($x / $y | floor), $sx * ($x % $y)];
  ld( xx|tonumber; yy|tonumber) | map(tostring);

def long_div(x;y):
  long_divide(x;y) | .[0];

def long_mod(x;y):
  ((x|tonumber) % (y|tonumber)) | tostring;
```


'''Fractions''':
```jq

# A fraction is represented by [numerator, denominator] in reduced form, with the sign on top

# a and b should be BigInt; return a BigInt
def gcd(a; b):
  def long_abs: . as $in | if lessOrEqual("0"; $in) then $in else negate end;

  # subfunction rgcd expects [a,b] as input
  # i.e. a ~ .[0] and b ~ .[1]
  def rgcd:
    .[0] as $a | .[1] as $b
    | if $b == "0" then $a
      else [$b, long_mod($a ; $b ) ] | rgcd
      end;

  a as $a | b as $b
  | [$a,$b] | rgcd | long_abs ;

def normalize:
  .[0] as $p | .[1] as $q
  | if $p == "0" then ["0", "1"]
    elif lessOrEqual($q ; "0") then [ ($p|negate), ($q|negate)] | normalize
    else gcd($p; $q) as $g
    | [ long_div($p;$g), long_div($q;$g) ]
    end ;

# a and b should be fractions expressed in the form [p, q]
def add(a; b):
  a as $a | b as $b
  | if $a[1] == "1" and $b[1] == "1" then [ long_add($a[0]; $b[0]) , "1"]
    elif $a[1] == $b[1] then [ long_add( $a[0]; $b[0]), $a[1] ] | normalize
    elif $a[0] == "0" then $b
    elif $b[0] == "0" then $a
    else [ long_add( long_multiply($a[0]; $b[1]) ; long_multiply($b[0]; $a[1])),
           long_multiply($a[1]; $b[1]) ]
    | normalize
    end ;

# a and/or b may be BigInts, or [p,q] fractions
def multiply(a; b):
  a as $a | b as $b
  | if ($a|type) == "string" and ($b|type) == "string" then [ long_multiply($a; $b), "1"]
    else
      if $a|type == "string" then [ long_multiply( $a; $b[0]), $b[1] ]
      elif $b|type == "string" then [ long_multiply( $b; $a[0]), $a[1] ]
      else  [ long_multiply( $a[0]; $b[0]), long_multiply($a[1]; $b[1]) ]
      end
      | normalize
  end ;

def minus(a; b):
  a as $a | b as $b
  | if $a == $b then ["0", "1"]
    else add($a; [ ($b[0]|negate), $b[1] ] )
    end ;
```


'''Bernoulli Numbers''':

```jq
# Using the algorithm in the task description:
def bernoulli(n):
  reduce range(0; n+1) as $m
    ( [];
      .[$m] = ["1", long_add($m|tostring; "1")]  # i.e. 1 / ($m+1)
      | reduce ($m - range(0 ; $m)) as $j
          (.;
            .[$j-1] = multiply( [($j|tostring), "1"]; minus( .[$j-1] ; .[$j]) ) ))
  | .[0] # (which is Bn)
  ;
```


'''The task''':

```jq
range(0;61)
| if . % 2 == 0 or . == 1 then "\(.): \(bernoulli(.) )" else empty end
```

The following output was obtained using the previously mentioned BigInt library.

```sh
$ jq -n -r -f Bernoulli.jq
0: ["1","1"]
1: ["1","2"]
2: ["1","6"]
4: ["-1","30"]
6: ["1","42"]
8: ["-1","30"]
10: ["5","66"]
12: ["-691","2730"]
14: ["7","6"]
16: ["-3617","510"]
18: ["43867","798"]
20: ["-174611","330"]
22: ["854513","138"]
24: ["-236364091","2730"]
26: ["8553103","6"]
28: ["-23749461029","870"]
30: ["8615841276005","14322"]
32: ["-7709321041217","510"]
34: ["2577687858367","6"]
36: ["-26315271553053477373","1919190"]
38: ["2929993913841559","6"]
40: ["-261082718496449122051","13530"]
42: ["1520097643918070802691","1806"]
44: ["-27833269579301024235023","690"]
46: ["596451111593912163277961","282"]
48: ["-5609403368997817686249127547","46410"]
50: ["495057205241079648212477525","66"]
52: ["-801165718135489957347924991853","1590"]
54: ["29149963634884862421418123812691","798"]
56: ["-2479392929313226753685415739663229","870"]
58: ["84483613348880041862046775994036021","354"]
60: ["-1215233140483755572040304994079820246041491","56786730"]
```



## Julia


```Julia
function bernoulli(n)
    A = Vector{Rational{BigInt}}(n + 1)
    for m = 0 : n
        A[m + 1] = 1 // (m + 1)
        for j = m : -1 : 1
            A[j] = j * (A[j] - A[j + 1])
        end
    end
    return A[1]
end

function display(n)
    B = map(bernoulli, 0 : n)
    pad = mapreduce(x -> ndigits(num(x)) + Int(x < 0), max, B)
    argdigits = ndigits(n)
    for i = 0 : n
        if num(B[i + 1]) & 1 == 1
            println(
                "B(", lpad(i, argdigits), ") = ",
                lpad(num(B[i + 1]), pad), " / ", den(B[i + 1])
            )
        end
    end
end

display(60)
```


Produces virtually the same output as the Python version.


## Kotlin

```scala
import org.apache.commons.math3.fraction.BigFraction

object Bernoulli {
    operator fun invoke(n: Int) : BigFraction {
        val A = Array(n + 1, init)
        for (m in 0..n)
            for (j in m downTo 1)
                A[j - 1] = A[j - 1].subtract(A[j]).multiply(integers[j])
        return A.first()
    }

    val max = 60

    private val init = { m: Int -> BigFraction(1, m + 1) }
    private val integers = Array(max + 1, { m: Int -> BigFraction(m) } )
}

fun main(args: Array<String>) {
    for (n in 0..Bernoulli.max)
        if (n % 2 == 0 || n == 1)
            System.out.printf("B(%-2d) = %-1s%n", n, Bernoulli(n))
}
```

Produces virtually the same output as the Java version.


## Maple


```Maple
print(select(n->n[2]<>0,[seq([n,bernoulli(n,1)],n=0..60)]));
```

```txt
[[0, 1], [1, 1/2], [2, 1/6], [4, -1/30], [6, 1/42], [8, -1/30], [10, 5/66], [12, -691/2730], [14, 7/6], [16, -3617/510], [18, 43867/798], [20, -174611/330], [22, 854513/138], [24, -236364091/2730], [26, 8553103/6], [28, -23749461029/870], [30, 8615841276005/14322], [32, -7709321041217/510], [34, 2577687858367/6], [36, -26315271553053477373/1919190], [38, 2929993913841559/6], [40, -261082718496449122051/13530], [42, 1520097643918070802691/1806], [44, -27833269579301024235023/690], [46, 596451111593912163277961/282], [48, -5609403368997817686249127547/46410], [50, 495057205241079648212477525/66], [52, -801165718135489957347924991853/1590], [54, 29149963634884862421418123812691/798], [56, -2479392929313226753685415739663229/870], [58, 84483613348880041862046775994036021/354], [60, -1215233140483755572040304994079820246041491/56786730]]
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica has no native way for starting an array at index 0. I therefore had to build the array from 1 to n+1 instead of from 0 to n, adjusting the formula accordingly.

```Mathematica
bernoulli[n_] := Module[{a = ConstantArray[0, n + 2]},
  Do[
    a[[m]] = 1/m;
    If[m == 1 && a[[1]] != 0, Print[{m - 1, a[[1]]}]];
    Do[
     a[[j - 1]] = (j - 1)*(a[[j - 1]] - a[[j]]);
     If[j == 2 && a[[1]] != 0, Print[{m - 1, a[[1]]}]];
     , {j, m, 2, -1}];
    , {m, 1, n + 1}];
  ]
bernoulli[60]
```

```txt
{0,1}
{1,1/2}
{2,1/6}
{4,-(1/30)}
{6,1/42}
{8,-(1/30)}
{10,5/66}
{12,-(691/2730)}
{14,7/6}
{16,-(3617/510)}
{18,43867/798}
{20,-(174611/330)}
{22,854513/138}
{24,-(236364091/2730)}
{26,8553103/6}
{28,-(23749461029/870)}
{30,8615841276005/14322}
{32,-(7709321041217/510)}
{34,2577687858367/6}
{36,-(26315271553053477373/1919190)}
{38,2929993913841559/6}
{40,-(261082718496449122051/13530)}
{42,1520097643918070802691/1806}
{44,-(27833269579301024235023/690)}
{46,596451111593912163277961/282}
{48,-(5609403368997817686249127547/46410)}
{50,495057205241079648212477525/66}
{52,-(801165718135489957347924991853/1590)}
{54,29149963634884862421418123812691/798}
{56,-(2479392929313226753685415739663229/870)}
{58,84483613348880041862046775994036021/354}
{60,-(1215233140483755572040304994079820246041491/56786730)}
```

Or, it's permissible to use the native Bernoulli number function instead of being forced to use the specified algorithm, we very simply have:

(Note from task's author: nobody is forced to use any specific algorithm, the one shown is just a suggestion.)


```Mathematica
Table[{i, BernoulliB[i]}, {i, 0, 60}];
Select[%, #[[2]] != 0 &] // TableForm
```

```txt
0	1
1	-(1/2)
2	1/6
4	-(1/30)
6	1/42
8	-(1/30)
10	5/66
12	-(691/2730)
14	7/6
16	-(3617/510)
18	43867/798
20	-(174611/330)
22	854513/138
24	-(236364091/2730)
26	8553103/6
28	-(23749461029/870)
30	8615841276005/14322
32	-(7709321041217/510)
34	2577687858367/6
36	-(26315271553053477373/1919190)
38	2929993913841559/6
40	-(261082718496449122051/13530)
42	1520097643918070802691/1806
44	-(27833269579301024235023/690)
46	596451111593912163277961/282
48	-(5609403368997817686249127547/46410)
50	495057205241079648212477525/66
52	-(801165718135489957347924991853/1590)
54	29149963634884862421418123812691/798
56	-(2479392929313226753685415739663229/870)
58	84483613348880041862046775994036021/354
60	-(1215233140483755572040304994079820246041491/56786730)
```



## PARI/GP


```parigp
for(n=0,60,t=bernfrac(n);if(t,print(n" "t)))
```

```txt
0 1
1 -1/2
2 1/6
4 -1/30
6 1/42
8 -1/30
10 5/66
12 -691/2730
14 7/6
16 -3617/510
18 43867/798
20 -174611/330
22 854513/138
24 -236364091/2730
26 8553103/6
28 -23749461029/870
30 8615841276005/14322
32 -7709321041217/510
34 2577687858367/6
36 -26315271553053477373/1919190
38 2929993913841559/6
40 -261082718496449122051/13530
42 1520097643918070802691/1806
44 -27833269579301024235023/690
46 596451111593912163277961/282
48 -5609403368997817686249127547/46410
50 495057205241079648212477525/66
52 -801165718135489957347924991853/1590
54 29149963634884862421418123812691/798
56 -2479392929313226753685415739663229/870
58 84483613348880041862046775994036021/354
60 -1215233140483755572040304994079820246041491/56786730
```



=={{header|Pascal|FreePascal}}==
Tested with fpc 3.0.4

```Pascal

(* Taken from the 'Ada 99' project, https://marquisdegeek.com/code_ada99 *)

program BernoulliForAda99;

uses BigDecimalMath; {library for arbitary high precision BCD numbers}

type
  Fraction = object
  private
    numerator, denominator: BigDecimal;

  public
    procedure assign(n, d: Int64);
    procedure subtract(rhs: Fraction);
    procedure multiply(value: Int64);
    procedure reduce();
    procedure writeOutput();
end;


function gcd(a, b: BigDecimal):BigDecimal;
begin
  if (b = 0) then begin
    gcd := a;
    end
  else begin
    gcd := gcd(b, a mod b);
 end;
end;


procedure Fraction.writeOutput();
var sign : char;
begin
  sign := ' ';
  if (numerator<0) then sign := '-';
  if (denominator<0) then sign := '-';
  write(sign + BigDecimalToStr(abs(numerator)):45);
  write(' / ');
  write(BigDecimalToStr(abs(denominator)));
end;


procedure Fraction.assign(n, d: Int64);
begin

  numerator := n;
  denominator := d;
end;


procedure Fraction.subtract(rhs: Fraction);
begin
  numerator := numerator * rhs.denominator;
  numerator := numerator - (rhs.numerator * denominator);
  denominator := denominator * rhs.denominator;
end;


procedure Fraction.multiply(value: Int64);
var
  temp :BigDecimal;
begin
  temp := value;
  numerator := numerator * temp;
end;


procedure Fraction.reduce();
var gcdResult: BigDecimal;
begin
  gcdResult := gcd(numerator, denominator);
  begin
    numerator := numerator div gcdResult;     (* div is Int64 division *)
    denominator := denominator div gcdResult; (* could also use round(d/r) *)
  end;
end;


function calculateBernoulli(n: Int64) : Fraction;
var
  m, j: Int64;
  results: array of Fraction;

  begin
    setlength(results, 60) ; {largest value 60}
    for m:= 0 to n do
    begin
      results[m].assign(1, m+1);

      for j:= m downto 1 do
        begin
          results[j-1].subtract(results[j]);
          results[j-1].multiply(j);
          results[j-1].reduce();
        end;
    end;

    calculateBernoulli := results[0];
end;


(* Main program starts here *)

var
  b: Int64;
  result: Fraction;

begin
  writeln('Calculating Bernoulli numbers...');
  writeln('B( 0) :                                             1 / 1');
  for b:= 1 to 60  do
    begin
	if (b<3) or ((b mod 2) = 0) then begin
          result := calculateBernoulli(b);
          write('B(',b:2,')');
          write(' : ');
          result.writeOutput();
        writeln;
      end;
  end;
end.

```


```txt

Calculating Bernoulli numbers...
B( 0) :                                             1 / 1
B( 1) :                                             1 / 2
B( 2) :                                             1 / 6
B( 4) :                                            -1 / 30
B( 6) :                                             1 / 42
B( 8) :                                            -1 / 30
B(10) :                                             5 / 66
B(12) :                                          -691 / 2730
B(14) :                                            -7 / 6
B(16) :                                         -3617 / 510
B(18) :                                         43867 / 798
B(20) :                                       -174611 / 330
B(22) :                                        854513 / 138
B(24) :                                    -236364091 / 2730
B(26) :                                       8553103 / 6
B(28) :                                  -23749461029 / 870
B(30) :                                 8615841276005 / 14322
B(32) :                                -7709321041217 / 510
B(34) :                                 2577687858367 / 6
B(36) :                         -26315271553053477373 / 1919190
B(38) :                              2929993913841559 / 6
B(40) :                        -261082718496449122051 / 13530
B(42) :                        1520097643918070802691 / 1806
B(44) :                      -27833269579301024235023 / 690
B(46) :                     -596451111593912163277961 / 282
B(48) :                 -5609403368997817686249127547 / 46410
B(50) :                   495057205241079648212477525 / 66
B(52) :               -801165718135489957347924991853 / 1590
B(54) :              29149963634884862421418123812691 / 798
B(56) :           -2479392929313226753685415739663229 / 870
B(58) :           84483613348880041862046775994036021 / 354
B(60) :  -1215233140483755572040304994079820246041491 / 56786730

```



## Perl

The only thing in the suggested algorithm which depends on N is the number of times through the inner block.  This means that all but the last iteration through the loop produce the exact same values of A.

Instead of doing the same calculations over and over again, I retain the A array until the final Bernoulli number is produced.


```perl
#!perl
use strict;
use warnings;
use List::Util qw(max);
use Math::BigRat;

my $one = Math::BigRat->new(1);
sub bernoulli_print {
	my @a;
	for my $m ( 0 .. 60 ) {
		push @a, $one / ($m + 1);
		for my $j ( reverse 1 .. $m ) {
				# This line:
				( $a[$j-1] -= $a[$j] ) *= $j;
				# is a faster version of the following line:
				# $a[$j-1] = $j * ($a[$j-1] - $a[$j]);
				# since it avoids unnecessary object creation.
		}
		next unless $a[0];
		printf "B(%2d) = %44s/%s\n", $m, $a[0]->parts;
	}
}

bernoulli_print();

```

The output is exactly the same as the Python entry.

We can also use modules for faster results.  E.g.
```perl
use ntheory qw/bernfrac/;

for my $n (0 .. 60) {
  my($num,$den) = bernfrac($n);
  printf "B(%2d) = %44s/%s\n", $n, $num, $den if $num != 0;
}
```

with identical output.  Or:

```perl
use Math::Pari qw/bernfrac/;

for my $n (0 .. 60) {
  my($num,$den) = split "/", bernfrac($n);
  printf("B(%2d) = %44s/%s\n", $n, $num, $den||1) if $num != 0;
}
```

with the difference being that Pari chooses <math>B_1</math> = -&frac12;.


## Perl 6



### Simple


First, a straighforward implementation of the naïve algorithm in the task description.
```perl6
sub bernoulli($n) {
    my @a;
    for 0..$n -> $m {
        @a[$m] = FatRat.new(1, $m + 1);
        for reverse 1..$m -> $j {
          @a[$j - 1] = $j * (@a[$j - 1] - @a[$j]);
        }
    }
    return @a[0];
}

constant @bpairs = grep *.value.so, ($_ => bernoulli($_) for 0..60);

my $width = max @bpairs.map: *.value.numerator.chars;
my $form = "B(%2d) = \%{$width}d/%d\n";

printf $form, .key, .value.nude for @bpairs;
```

```txt
B( 0) =                                            1/1
B( 1) =                                            1/2
B( 2) =                                            1/6
B( 4) =                                           -1/30
B( 6) =                                            1/42
B( 8) =                                           -1/30
B(10) =                                            5/66
B(12) =                                         -691/2730
B(14) =                                            7/6
B(16) =                                        -3617/510
B(18) =                                        43867/798
B(20) =                                      -174611/330
B(22) =                                       854513/138
B(24) =                                   -236364091/2730
B(26) =                                      8553103/6
B(28) =                                 -23749461029/870
B(30) =                                8615841276005/14322
B(32) =                               -7709321041217/510
B(34) =                                2577687858367/6
B(36) =                        -26315271553053477373/1919190
B(38) =                             2929993913841559/6
B(40) =                       -261082718496449122051/13530
B(42) =                       1520097643918070802691/1806
B(44) =                     -27833269579301024235023/690
B(46) =                     596451111593912163277961/282
B(48) =                -5609403368997817686249127547/46410
B(50) =                  495057205241079648212477525/66
B(52) =              -801165718135489957347924991853/1590
B(54) =             29149963634884862421418123812691/798
B(56) =          -2479392929313226753685415739663229/870
B(58) =          84483613348880041862046775994036021/354
B(60) = -1215233140483755572040304994079820246041491/56786730
```



### With memoization


Here is a much faster way, following the Perl solution that avoids recalculating previous values each time through the function.  We do this in Perl 6 by not defining it as a function at all, but by defining it as an infinite sequence that we can read however many values we like from (52, in this case, to get up to B(100)).  In this solution we've also avoided subscripting operations; rather we use a sequence operator (<tt>...</tt>) iterated over the list of the previous solution to find the next solution.  We reverse the array in this case to make reference to the previous value in the list more natural, which means we take the last value of the list rather than the first value, and do so conditionally to avoid 0 values.

```perl6
constant bernoulli = gather {
    my @a;
    for 0..* -> $m {
        @a = FatRat.new(1, $m + 1),
                -> $prev {
                    my $j = @a.elems;
                    $j * (@a.shift - $prev);
                } ... { not @a.elems }
        take $m => @a[*-1] if @a[*-1];
    }
}

constant @bpairs = bernoulli[^52];

my $width = max @bpairs.map: *.value.numerator.chars;
my $form = "B(%d)\t= \%{$width}d/%d\n";

printf $form, .key, .value.nude for @bpairs;
```

```txt
B(0)	=                                                                                    1/1
B(1)	=                                                                                    1/2
B(2)	=                                                                                    1/6
B(4)	=                                                                                   -1/30
B(6)	=                                                                                    1/42
B(8)	=                                                                                   -1/30
B(10)	=                                                                                    5/66
B(12)	=                                                                                 -691/2730
B(14)	=                                                                                    7/6
B(16)	=                                                                                -3617/510
B(18)	=                                                                                43867/798
B(20)	=                                                                              -174611/330
B(22)	=                                                                               854513/138
B(24)	=                                                                           -236364091/2730
B(26)	=                                                                              8553103/6
B(28)	=                                                                         -23749461029/870
B(30)	=                                                                        8615841276005/14322
B(32)	=                                                                       -7709321041217/510
B(34)	=                                                                        2577687858367/6
B(36)	=                                                                -26315271553053477373/1919190
B(38)	=                                                                     2929993913841559/6
B(40)	=                                                               -261082718496449122051/13530
B(42)	=                                                               1520097643918070802691/1806
B(44)	=                                                             -27833269579301024235023/690
B(46)	=                                                             596451111593912163277961/282
B(48)	=                                                        -5609403368997817686249127547/46410
B(50)	=                                                          495057205241079648212477525/66
B(52)	=                                                      -801165718135489957347924991853/1590
B(54)	=                                                     29149963634884862421418123812691/798
B(56)	=                                                  -2479392929313226753685415739663229/870
B(58)	=                                                  84483613348880041862046775994036021/354
B(60)	=                                         -1215233140483755572040304994079820246041491/56786730
B(62)	=                                               12300585434086858541953039857403386151/6
B(64)	=                                          -106783830147866529886385444979142647942017/510
B(66)	=                                       1472600022126335654051619428551932342241899101/64722
B(68)	=                                        -78773130858718728141909149208474606244347001/30
B(70)	=                                    1505381347333367003803076567377857208511438160235/4686
B(72)	=                             -5827954961669944110438277244641067365282488301844260429/140100870
B(74)	=                                   34152417289221168014330073731472635186688307783087/6
B(76)	=                               -24655088825935372707687196040585199904365267828865801/30
B(78)	=                            414846365575400828295179035549542073492199375372400483487/3318
B(80)	=                       -4603784299479457646935574969019046849794257872751288919656867/230010
B(82)	=                        1677014149185145836823154509786269900207736027570253414881613/498
B(84)	=                 -2024576195935290360231131160111731009989917391198090877281083932477/3404310
B(86)	=                      660714619417678653573847847426261496277830686653388931761996983/6
B(88)	=              -1311426488674017507995511424019311843345750275572028644296919890574047/61410
B(90)	=            1179057279021082799884123351249215083775254949669647116231545215727922535/272118
B(92)	=           -1295585948207537527989427828538576749659341483719435143023316326829946247/1410
B(94)	=            1220813806579744469607301679413201203958508415202696621436215105284649447/6
B(96)	=   -211600449597266513097597728109824233673043954389060234150638733420050668349987259/4501770
B(98)	=        67908260672905495624051117546403605607342195728504487509073961249992947058239/6
B(100)	= -94598037819122125295227433069493721872702841533066936133385696204311395415197247711/33330
```



### Functional


And if you're a pure enough FP programmer to dislike destroying and reconstructing the array each time, here's the same algorithm without side effects.  We use zip with the pair constructor <tt>=></tt> to keep values associated with their indices. This provides sufficient local information that we can define our own binary operator "bop" to reduce between each two terms, using the "triangle" form (called "scan" in Haskell) to return the intermediate results that will be important to compute the next Bernoulli number.
```perl6
sub infix:<bop
(\prev, \this) {
    this.key => this.key * (this.value - prev.value)
}

sub next-bernoulli ( (:key($pm), :value(@pa)) ) {
    $pm + 1 => [
        map *.value,
        [\bop] ($pm + 2 ... 1) Z=> FatRat.new(1, $pm + 2), |@pa
    ]
}

constant bernoulli =
    grep *.value,
    map { .key => .value[*-1] },
    (0 => [FatRat.new(1,1)], &next-bernoulli ... *)
;

constant @bpairs = bernoulli[^52];

my $width = max @bpairs.map: *.value.numerator.chars;
my $form = "B(%d)\t= \%{$width}d/%d\n";

printf $form, .key, .value.nude for @bpairs;
```


Same output as memoization example


## Phix

```Phix
include builtins/mpfr.e
procedure bernoulli(mpq rop, integer n)
    sequence a = mpq_init(n+1)
    for m=1 to n+1 do
        mpq_set_si(a[m], 1, m)
        for j=m-1 to 1 by -1 do
            mpq_sub(a[j], a[j+1], a[j])
            mpq_set_si(rop, j, 1)
            mpq_mul(a[j], a[j], rop)
        end for
    end for
    mpq_set(rop, a[1])
    a = mpq_free(a)
end procedure

mpq rop = mpq_init()
mpz n = mpz_init(),
    d = mpz_init()
for i=0 to 60 do
    bernoulli(rop, i)
    if mpq_cmp_si(rop, 0, 1) then
        mpq_get_num(n, rop)
        mpq_get_den(d, rop)
        string ns = mpfr_sprintf("%44Zd",n),
               ds = mpfr_sprintf("%Zd",d)
        printf(1,"B(%2d) = %s / %s\n", {i,ns,ds})
    end if
end for
{n,d} = mpz_free({n,d})
rop = mpq_free(rop)
```

```txt

B( 0) =                                            1 / 1
B( 1) =                                           -1 / 2
B( 2) =                                            1 / 6
B( 4) =                                           -1 / 30
B( 6) =                                            1 / 42
B( 8) =                                           -1 / 30
B(10) =                                            5 / 66
B(12) =                                         -691 / 2730
B(14) =                                            7 / 6
B(16) =                                        -3617 / 510
B(18) =                                        43867 / 798
B(20) =                                      -174611 / 330
B(22) =                                       854513 / 138
B(24) =                                   -236364091 / 2730
B(26) =                                      8553103 / 6
B(28) =                                 -23749461029 / 870
B(30) =                                8615841276005 / 14322
B(32) =                               -7709321041217 / 510
B(34) =                                2577687858367 / 6
B(36) =                        -26315271553053477373 / 1919190
B(38) =                             2929993913841559 / 6
B(40) =                       -261082718496449122051 / 13530
B(42) =                       1520097643918070802691 / 1806
B(44) =                     -27833269579301024235023 / 690
B(46) =                     596451111593912163277961 / 282
B(48) =                -5609403368997817686249127547 / 46410
B(50) =                  495057205241079648212477525 / 66
B(52) =              -801165718135489957347924991853 / 1590
B(54) =             29149963634884862421418123812691 / 798
B(56) =          -2479392929313226753685415739663229 / 870
B(58) =          84483613348880041862046775994036021 / 354
B(60) = -1215233140483755572040304994079820246041491 / 56786730

```



## PicoLisp

Brute force and method by Srinivasa Ramanujan.

```PicoLisp
(load "@lib/frac.l")

(de fact (N)
   (cache '(NIL) N
      (if (=0 N) 1 (apply * (range 1 N))) ) )

(de binomial (N K)
   (frac
      (/
         (fact N)
         (* (fact (- N K)) (fact K)) )
      1 ) )

(de A (N M)
   (let Sum (0 . 1)
      (for X M
         (setq Sum
            (f+
               Sum
               (f*
                  (binomial (+ N 3) (- N (* X 6)))
                  (berno (- N (* X 6)) ) ) ) ) )
      Sum ) )

(de berno (N)
   (cache '(NIL) N
      (cond
         ((=0 N) (1 . 1))
         ((= 1 N) (-1 . 2))
         ((bit? 1 N) (0 . 1))
         (T
            (case (% N 6)
               (0
                  (f/
                     (f-
                        (frac (+ N 3) 3)
                        (A N (/ N 6)) )
                     (binomial (+ N 3) N) ) )
               (2
                  (f/
                     (f-
                        (frac (+ N 3) 3)
                        (A N (/ (- N 2) 6)) )
                     (binomial (+ N 3) N) ) )
               (4
                  (f/
                     (f-
                        (f* (-1 . 1) (frac (+ N 3) 6))
                        (A N (/ (- N 4) 6)) )
                     (binomial (+ N 3) N) ) ) ) ) ) ) )

(de berno-brute (N)
   (cache '(NIL) N
      (let Sum (0 . 1)
         (cond
            ((=0 N) (1 . 1))
            ((= 1 N) (-1 . 2))
            ((bit? 1 N) (0 . 1))
            (T
               (for (X 0 (> N X) (inc X))
                  (setq Sum
                     (f+
                        Sum
                        (f* (binomial (inc N) X) (berno-brute X)) ) ) )
               (f/ (f* (-1 . 1) Sum) (binomial (inc N) N)) ) ) ) ) )

(for (N 0 (> 62 N) (inc N))
   (if (or (= N 1) (not (bit? 1 N)))
      (tab (2 4 -60) N " => " (sym (berno N))) ) )

(for (N 0 (> 400 N) (inc N))
   (test (berno N) (berno-brute N)) )

(bye)
```



## PL/I


```PL/I
Bern: procedure options (main); /* 4 July 2014 */
   declare i fixed binary;
   declare B complex fixed (31);

Bernoulli: procedure (n) returns (complex fixed (31));
   declare n      fixed binary;
   declare anum(0:n) fixed (31), aden(0:n) fixed (31);
   declare (j, m) fixed;
   declare F fixed (31);

   do m = 0 to n;
      anum(m) = 1;
      aden(m) = m+1;
      do j = m to 1 by -1;
         anum(j-1) = j*( aden(j)*anum(j-1) - aden(j-1)*anum(j) );
         aden(j-1) =   ( aden(j-1) * aden(j) );
         F = gcd(abs(anum(j-1)), abs(aden(j-1)) );
         if F ^= 1 then
            do;
               anum(j-1) = anum(j-1) / F;
               aden(j-1) = aden(j-1) / F;
            end;
      end;
   end;
   return ( complex(anum(0), aden(0)) );
end Bernoulli;

   do i = 0, 1, 2 to 36 by 2; /* 36 is upper limit imposed by hardware. */
      B = Bernoulli(i);
      put skip edit ('B(' , trim(i) , ')=' , real(B) , '/' , trim(imag(B)) )
                    (3 A, column(10), F(32), 2 A);
   end;
end Bern;
```

The above uses GCD (see Rosetta Code) extended for 31-digit working.

Results obtained by this program are limited to the entries shown below due to the restrictions imposed by storing numbers in fixed decimal (31 digits).

```txt

B(0)=                                   1/1
B(1)=                                   1/2
B(2)=                                   1/6
B(4)=                                  -1/30
B(6)=                                   1/42
B(8)=                                  -1/30
B(10)=                                  5/66
B(12)=                               -691/2730
B(14)=                                  7/6
B(16)=                              -3617/510
B(18)=                              43867/798
B(20)=                            -174611/330
B(22)=                             854513/138
B(24)=                         -236364091/2730
B(26)=                            8553103/6
B(28)=                       -23749461029/870
B(30)=                      8615841276005/14322
B(32)=                     -7709321041217/510
B(34)=                      2577687858367/6
B(36)=              -26315271553053477373/1919190

```



## Python


### Python: Using task algorithm


```python
from fractions import Fraction as Fr

def bernoulli(n):
    A = [0] * (n+1)
    for m in range(n+1):
        A[m] = Fr(1, m+1)
        for j in range(m, 0, -1):
          A[j-1] = j*(A[j-1] - A[j])
    return A[0] # (which is Bn)

bn = [(i, bernoulli(i)) for i in range(61)]
bn = [(i, b) for i,b in bn if b]
width = max(len(str(b.numerator)) for i,b in bn)
for i,b in bn:
    print('B(%2i) = %*i/%i' % (i, width, b.numerator, b.denominator))
```


```txt
B( 0) =                                            1/1
B( 1) =                                            1/2
B( 2) =                                            1/6
B( 4) =                                           -1/30
B( 6) =                                            1/42
B( 8) =                                           -1/30
B(10) =                                            5/66
B(12) =                                         -691/2730
B(14) =                                            7/6
B(16) =                                        -3617/510
B(18) =                                        43867/798
B(20) =                                      -174611/330
B(22) =                                       854513/138
B(24) =                                   -236364091/2730
B(26) =                                      8553103/6
B(28) =                                 -23749461029/870
B(30) =                                8615841276005/14322
B(32) =                               -7709321041217/510
B(34) =                                2577687858367/6
B(36) =                        -26315271553053477373/1919190
B(38) =                             2929993913841559/6
B(40) =                       -261082718496449122051/13530
B(42) =                       1520097643918070802691/1806
B(44) =                     -27833269579301024235023/690
B(46) =                     596451111593912163277961/282
B(48) =                -5609403368997817686249127547/46410
B(50) =                  495057205241079648212477525/66
B(52) =              -801165718135489957347924991853/1590
B(54) =             29149963634884862421418123812691/798
B(56) =          -2479392929313226753685415739663229/870
B(58) =          84483613348880041862046775994036021/354
B(60) = -1215233140483755572040304994079820246041491/56786730
```



### Python: Optimised task algorithm

Using the optimization mentioned in the Perl entry to reduce intermediate calculations we create and use the generator bernoulli2():

```python
def bernoulli2():
    A, m = [], 0
    while True:
        A.append(Fr(1, m+1))
        for j in range(m, 0, -1):
          A[j-1] = j*(A[j-1] - A[j])
        yield A[0] # (which is Bm)
        m += 1

bn2 = [ix for ix in zip(range(61), bernoulli2())]
bn2 = [(i, b) for i,b in bn2 if b]
width = max(len(str(b.numerator)) for i,b in bn2)
for i,b in bn2:
    print('B(%2i) = %*i/%i' % (i, width, b.numerator, b.denominator))
```


Output is exactly the same as before.


## R


{{incorrect|Pascal|
 The index numbers are not correct.
 '''B<sub>0</sub>''' isn't shown.
 The Bernoulli numbers are not shown as (reduced) fractions.
 Bernoulli numbers equal to zero are to be suppressed.

}}

R has the built-in function bernoulli(n), where n is the index, a whole number greater or equal to 0.
It returns the first n+1 Bernoulli numbers, that are defined as a sequence of rational numbers.

```r

# Bernoulli numbers. 12/8/16 aev
require(pracma)
bernoulli(60)

```


```txt

> require(pracma)
Loading required package: pracma
> bernoulli(60)
 [1]  1.000000e+00 -5.000000e-01  1.666667e-01  0.000000e+00 -3.333333e-02
 [6]  0.000000e+00  2.380952e-02  0.000000e+00 -3.333333e-02  0.000000e+00
[11]  7.575758e-02  0.000000e+00 -2.531136e-01  0.000000e+00  1.166667e+00
[16]  0.000000e+00 -7.092157e+00  0.000000e+00  5.497118e+01  0.000000e+00
[21] -5.291242e+02  0.000000e+00  6.192123e+03  0.000000e+00 -8.658025e+04
[26]  0.000000e+00  1.425517e+06  0.000000e+00 -2.729823e+07  0.000000e+00
[31]  6.015809e+08  0.000000e+00 -1.511632e+10  0.000000e+00  4.296146e+11
[36]  0.000000e+00 -1.371166e+13  0.000000e+00  4.883323e+14  0.000000e+00
[41] -1.929658e+16  0.000000e+00  8.416930e+17  0.000000e+00 -4.033807e+19
[46]  0.000000e+00  2.115075e+21  0.000000e+00 -1.208663e+23  0.000000e+00
[51]  7.500867e+24  0.000000e+00 -5.038778e+26  0.000000e+00  3.652878e+28
[56]  0.000000e+00 -2.849877e+30  0.000000e+00  2.386543e+32  0.000000e+00
[61] -2.139995e+34
>

```



## Racket


This implements, firstly, the algorithm specified with the task... then the better performing ''bernoulli.3'',
which uses the "double sum formula" listed under REXX. The number generators all (there is also a ''bernoulli.2'')
use the same emmitter... it's just a matter of how long to wait for the emission.

<lang>#lang racket
;; For: http://rosettacode.org/wiki/Bernoulli_numbers

;; As described in task...
(define (bernoulli.1 n)
  (define A (make-vector (add1 n)))
  (for ((m (in-range 0 (add1 n))))
    (vector-set! A m (/ (add1 m)))
    (for ((j (in-range m (sub1 1) -1)))
      (define new-A_j-1 (* j (- (vector-ref A (sub1 j)) (vector-ref A j))))
      (vector-set! A (sub1 j) new-A_j-1)))
  (vector-ref A 0))

(define (non-zero-bernoulli-indices s)
  (sequence-filter (λ (n) (or (even? n) (= n 1))) s))
(define (bernoulli_0..n B N)
  (for/list ((n (non-zero-bernoulli-indices (in-range (add1 N))))) (B n)))

;; From REXX description / http://mathworld.wolfram.com/BernoulliNumber.html #33
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; bernoulli.2 is for illustrative purposes, binomial is very costly if there is no memoisation
;; (which math/number-theory doesn't do)
(require (only-in math/number-theory binomial))
(define (bernoulli.2 n)
  (for/sum ((k (in-range 0 (add1 n))))
    (* (/ (add1 k))
       (for/sum ((r (in-range 0 (add1 k))))
         (* (expt -1 r) (binomial k r) (expt r n))))))

;; Three things to do:
;; 1. (expt -1 r): is 1 for even r, -1 for odd r... split the sum between those two.
;; 2. splitting the sum might has arithmetic advantages, too. We're using rationals, so the smaller
;;    summations should require less normalisation of intermediate, fractional results
;; 3. a memoised binomial... although the one from math/number-theory is fast, it is (and its
;;    factorials are) computed every time which is redundant
(define kCr-memo (make-hasheq))
(define !-memo (make-vector 1000 #f))
(vector-set! !-memo 0 1) ;; seed the memo
(define (! k)
  (cond [(vector-ref !-memo k) => values]
        [else (define k! (* k (! (- k 1)))) (vector-set! !-memo k k!) k!]))
(define (kCr k r)
  ; If we want (kCr ... r>1000000) we'll have to reconsider this. However, until then...
  (define hash-key (+ (* 1000000 k) r))
  (hash-ref! kCr-memo hash-key (λ () (/ (! k) (! r) (! (- k r))))))

(define (bernoulli.3 n)
  (for/sum ((k (in-range 0 (add1 n))))
    (define k+1 (add1 k))
    (* (/ k+1)
       (- (for/sum ((r (in-range 0 k+1 2))) (* (kCr k r) (expt r n)))
          (for/sum ((r (in-range 1 k+1 2))) (* (kCr k r) (expt r n)))))))

(define (display/align-fractions caption/idx-fmt Bs)
  ;; widths are one more than the order of magnitude
  (define oom+1 (compose add1 order-of-magnitude))
  (define-values (I-width N-width D-width)
    (for/fold ((I 0) (N 0) (D 0))
      ((b Bs) (n (non-zero-bernoulli-indices (in-naturals))))
      (define +b (abs b))
      (values (max I (oom+1 (max n 1)))
              (max N (+ (oom+1 (numerator +b)) (if (negative? b) 1 0)))
              (max D (oom+1 (denominator +b))))))
  (define (~a/w/a n w a) (~a n #:width w #:align a))
  (for ((n (non-zero-bernoulli-indices (in-naturals))) (b Bs))
    (printf "~a ~a/~a~%"
            (format caption/idx-fmt (~a/w/a n I-width 'right))
            (~a/w/a (numerator b) N-width 'right)
            (~a/w/a (denominator b) D-width 'left))))

(module+ main
  (display/align-fractions "B(~a) =" (bernoulli_0..n bernoulli.3 60)))

(module+ test
  (require rackunit)
  ; correctness and timing tests
  (check-match (time (bernoulli_0..n bernoulli.1 60))
               (list 1/1 (app abs 1/2) 1/6 -1/30 1/42 -1/30 _ ...))
  (check-match (time (bernoulli_0..n bernoulli.2 60))
               (list 1/1 (app abs 1/2) 1/6 -1/30 1/42 -1/30 _ ...))
  (check-match (time (bernoulli_0..n bernoulli.3 60))
               (list 1/1 (app abs 1/2) 1/6 -1/30 1/42 -1/30 _ ...))
  ; timing only ...
  (void (time (bernoulli_0..n bernoulli.3 100))))
```


```txt
B( 0) =                                            1/1
B( 1) =                                           -1/2
B( 2) =                                            1/6
B( 4) =                                           -1/30
B( 6) =                                            1/42
B( 8) =                                           -1/30
B(10) =                                            5/66
B(12) =                                         -691/2730
B(14) =                                            7/6
B(16) =                                        -3617/510
B(18) =                                        43867/798
B(20) =                                      -174611/330
B(22) =                                       854513/138
B(24) =                                   -236364091/2730
B(26) =                                      8553103/6
B(28) =                                 -23749461029/870
B(30) =                                8615841276005/14322
B(32) =                               -7709321041217/510
B(34) =                                2577687858367/6
B(36) =                        -26315271553053477373/1919190
B(38) =                             2929993913841559/6
B(40) =                       -261082718496449122051/13530
B(42) =                       1520097643918070802691/1806
B(44) =                     -27833269579301024235023/690
B(46) =                     596451111593912163277961/282
B(48) =                -5609403368997817686249127547/46410
B(50) =                  495057205241079648212477525/66
B(52) =              -801165718135489957347924991853/1590
B(54) =             29149963634884862421418123812691/798
B(56) =          -2479392929313226753685415739663229/870
B(58) =          84483613348880041862046775994036021/354
B(60) = -1215233140483755572040304994079820246041491/56786730
```



## REXX

The double sum formula used is number   '''(33)'''   from the entry [http://mathworld.wolfram.com/BernoulliNumber.html Bernoulli number] on Wolfram MathWorld<sup>TM</sup>.



::::::: <big><big> <math> B_n = \sum_{k=0}^n \frac{1}{k+1} \sum_{r=0}^k (-1)^r \binom kr r^n </math> </big></big>


:::::::::::: where           <big><big> <math> \binom kr</math> </big></big>       is a binomial coefficient.


```rexx
/*REXX program calculates    N   number of  Bernoulli numbers  expressed as  fractions. */
parse arg N .;     if N=='' | N==","  then N= 60 /*Not specified?  Then use the default.*/
d= n*2;    if d>digits()  then numeric digits d  /*increase the decimal digits if needed*/
!.=0;   w= max(length(N), 4);  Nw= N + w + N % 4 /*used for aligning (output) fractions.*/
say 'B(n)'   center("Bernoulli number expressed as a fraction", max(78-w, Nw))  /*title.*/
say copies('─',w)  copies("─", max(78-w,Nw+2*w)) /*display 2nd line of title, separators*/
        do #=0  to  N                            /*process the numbers from  0  ──►  N. */
        b= bern(#);      if b==0  then iterate   /*calculate Bernoulli number, skip if 0*/
        indent= max(0, nW - pos('/', b) )        /*calculate the alignment (indentation)*/
        say right(#, w)  left('', indent)  b     /*display the indented Bernoulli number*/
        end   /*#*/                              /* [↑]  align the Bernoulli fractions. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bern: parse arg x; if x==0  then return  '1/1'   /*handle the special case of  zero.    */
                   if x==1  then return '-1/2'   /*   "    "     "      "   "  one.     */
                   if x//2  then return   0      /*   "    "     "      "   "  odds > 1.*/
        do j=2  to x  by 2;   jp= j+1;    d= j+j /*process the positive integers up to X*/
        sn= 1 - j                                /*define the  numerator.               */
        sd= 2                                    /*   "    "   denominator.             */
                   do k=2  to j-1  by 2          /*calculate a  SN/SD  sequence.        */
                   parse var  @.k    bn  '/'  ad /*get a previously calculated fraction.*/
                   an= comb(jp, k) * bn          /*use  COMBination  for the next term. */
                   $LCM= LCM(sd, ad)             /*use Least Common Denominator function*/
                   sn= $LCM % sd * sn;  sd= $LCM /*calculate the   current  numerator.  */
                   an= $LCM % ad * an;  ad= $LCM /*    "      "      next      "        */
                   sn= sn + an                   /*    "      "    current     "        */
                   end   /*k*/                   /* [↑]  calculate the  SN/SD  sequence.*/
        sn= -sn                                  /*adjust the sign for the numerator.   */
        sd= sd * jp                              /*calculate           the denominator. */
        if sn\==1  then do;  _= GCD(sn, sd)      /*get the  Greatest Common Denominator.*/
                            sn= sn%_;  sd= sd%_  /*reduce the numerator and denominator.*/
                        end                      /* [↑]   done with the reduction(s).   */
        @.j= sn'/'sd                             /*save the result for the next round.  */
        end     /*j*/                            /* [↑]  done calculating Bernoulli #'s.*/
      return sn'/'sd
/*──────────────────────────────────────────────────────────────────────────────────────*/
comb: procedure expose !.; parse arg x,y;        if x==y  then return 1
      if !.c.x.y \== 0  then return !.c.x.y               /*combination computed before?*/
      if x-y < y   then y= x-y;     z= perm(x, y);    do j=2  to y;   z= z % j;  end /*J*/
      !.c.x.y= z;     return z                            /*assign memoization & return.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
GCD:  procedure;           parse arg x,y;       x= abs(x)
        do  until y==0;    parse value  x//y  y    with    y  x;  end;            return x
/*──────────────────────────────────────────────────────────────────────────────────────*/
LCM:  procedure; parse arg x,y  /*x=abs(x);  y=abs(y)   not needed for Bernoulli numbers*/
                 if y==0  then return 0               /*if zero, then LCM is also zero. */
                 d= x * y                             /*calculate part of the LCM here. */
                          do  until y==0;   parse  value   x//y  y      with      y  x
                          end   /*until*/             /* [↑]  this is a short & fast GCD*/
                 return d % x                         /*divide the pre─calculated value.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
perm: procedure expose !.;  parse arg x,y;          if !.p.x.y \== 0  then return !.p.x.y
      z= 1;       do j=x-y+1  to x;     z= z*j;     end;        !.p.x.y= z;       return z
```

```txt

B(n)                    Bernoulli number expressed as a fraction
──── ───────────────────────────────────────────────────────────────────────────────────────
   0                                                                               1/1
   1                                                                              -1/2
   2                                                                               1/6
   4                                                                              -1/30
   6                                                                               1/42
   8                                                                              -1/30
  10                                                                               5/66
  12                                                                            -691/2730
  14                                                                               7/6
  16                                                                           -3617/510
  18                                                                           43867/798
  20                                                                         -174611/330
  22                                                                          854513/138
  24                                                                      -236364091/2730
  26                                                                         8553103/6
  28                                                                    -23749461029/870
  30                                                                   8615841276005/14322
  32                                                                  -7709321041217/510
  34                                                                   2577687858367/6
  36                                                           -26315271553053477373/1919190
  38                                                                2929993913841559/6
  40                                                          -261082718496449122051/13530
  42                                                          1520097643918070802691/1806
  44                                                        -27833269579301024235023/690
  46                                                        596451111593912163277961/282
  48                                                   -5609403368997817686249127547/46410
  50                                                     495057205241079648212477525/66
  52                                                 -801165718135489957347924991853/1590
  54                                                29149963634884862421418123812691/798
  56                                             -2479392929313226753685415739663229/870
  58                                             84483613348880041862046775994036021/354
  60                                    -1215233140483755572040304994079820246041491/56786730

```



## Ruby

```ruby
bernoulli = Enumerator.new do |y|
  ar = []
  0.step do |m|
    ar << Rational(1, m+1)
    m.downto(1){|j| ar[j-1] = j*(ar[j-1] - ar[j]) }
    y << ar.first  # yield
  end
end

b_nums = bernoulli.take(61)
width  = b_nums.map{|b| b.numerator.to_s.size}.max
b_nums.each_with_index {|b,i| puts "B(%2i) = %*i/%i" % [i, width, b.numerator, b.denominator] unless b.zero? }


```

```txt

B( 0) =                                            1/1
B( 1) =                                            1/2
B( 2) =                                            1/6
B( 4) =                                           -1/30
B( 6) =                                            1/42
B( 8) =                                           -1/30
B(10) =                                            5/66
B(12) =                                         -691/2730
B(14) =                                            7/6
B(16) =                                        -3617/510
B(18) =                                        43867/798
B(20) =                                      -174611/330
B(22) =                                       854513/138
B(24) =                                   -236364091/2730
B(26) =                                      8553103/6
B(28) =                                 -23749461029/870
B(30) =                                8615841276005/14322
B(32) =                               -7709321041217/510
B(34) =                                2577687858367/6
B(36) =                        -26315271553053477373/1919190
B(38) =                             2929993913841559/6
B(40) =                       -261082718496449122051/13530
B(42) =                       1520097643918070802691/1806
B(44) =                     -27833269579301024235023/690
B(46) =                     596451111593912163277961/282
B(48) =                -5609403368997817686249127547/46410
B(50) =                  495057205241079648212477525/66
B(52) =              -801165718135489957347924991853/1590
B(54) =             29149963634884862421418123812691/798
B(56) =          -2479392929313226753685415739663229/870
B(58) =          84483613348880041862046775994036021/354
B(60) = -1215233140483755572040304994079820246041491/56786730

```



## Rust


{{incorrect|Rust|

 '''B<sub>1</sub>''' isn't shown.

}}


```rust
// 2.5 implementations presented here:  naive, optimized, and an iterator using
// the optimized function. The speeds vary significantly: relative
// speeds of optimized:iterator:naive implementations is 625:25:1.

#![feature(test)]

extern crate num;
extern crate test;

use num::bigint::{BigInt, ToBigInt};
use num::rational::{BigRational};
use std::cmp::max;
use std::env;
use std::ops::{Mul, Sub};
use std::process;

struct Bn {
    value: BigRational,
    index: i32
}

struct Context {
    bigone_const: BigInt,
    a: Vec<BigRational>,
    index: i32              // Counter for iterator implementation
}

impl Context {
    pub fn new() -> Context {
        let bigone = 1.to_bigint().unwrap();
        let a_vec: Vec<BigRational> = vec![];
        Context {
            bigone_const: bigone,
            a: a_vec,
            index: -1
        }
    }
}

impl Iterator for Context {
    type Item = Bn;

    fn next(&mut self) -> Option<Bn> {
        self.index += 1;
        Some(Bn { value: bernoulli(self.index as usize, self), index: self.index })
    }
}

fn help() {
    println!("Usage: bernoulli_numbers <up_to>");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut up_to: usize = 60;

    match args.len() {
        1 => {},
        2 => {
            up_to = args[1].parse::<usize>().unwrap();
        },
        _ => {
            help();
            process::exit(0);
        }
    }

    let context = Context::new();
    // Collect the solutions by using the Context iterator
    // (this is not as fast as calling the optimized function directly).
    let res = context.take(up_to + 1).collect::<Vec<_>>();
    let width = res.iter().fold(0, |a, r| max(a, r.value.numer().to_string().len()));

    for r in res.iter().filter(|r| r.index % 2 == 0) {
        println!("B({:>2}) = {:>2$} / {denom}", r.index, r.value.numer(), width,
            denom = r.value.denom());
    }
}

// Implementation with no reused calculations.
fn _bernoulli_naive(n: usize, c: &mut Context) -> BigRational {
    for m in 0..n + 1 {
        c.a.push(BigRational::new(c.bigone_const.clone(), (m + 1).to_bigint().unwrap()));
        for j in (1..m + 1).rev() {
            c.a[j - 1] = (c.a[j - 1].clone().sub(c.a[j].clone())).mul(
                BigRational::new(j.to_bigint().unwrap(), c.bigone_const.clone())
            );
        }
    }
    c.a[0].reduced()
}

// Implementation with reused calculations (does not require sequential calls).
fn bernoulli(n: usize, c: &mut Context) -> BigRational {
    for i in 0..n + 1 {
        if i >= c.a.len() {
            c.a.push(BigRational::new(c.bigone_const.clone(), (i + 1).to_bigint().unwrap()));
            for j in (1..i + 1).rev() {
                c.a[j - 1] = (c.a[j - 1].clone().sub(c.a[j].clone())).mul(
                    BigRational::new(j.to_bigint().unwrap(), c.bigone_const.clone())
                );
            }
        }
    }
    c.a[0].reduced()
}


#[cfg(test)]
mod tests {
    use super::{Bn, Context, bernoulli, _bernoulli_naive};
    use num::rational::{BigRational};
    use std::str::FromStr;
    use test::Bencher;

    // [tests elided]

    #[bench]
    fn bench_bernoulli_naive(b: &mut Bencher) {
        let mut context = Context::new();
        b.iter(|| {
            let mut res: Vec<Bn> = vec![];
            for n in 0..30 + 1 {
                let b = _bernoulli_naive(n, &mut context);
                res.push(Bn { value:b.clone(), index: n as i32});
            }
        });
    }

    #[bench]
    fn bench_bernoulli(b: &mut Bencher) {
        let mut context = Context::new();
        b.iter(|| {
            let mut res: Vec<Bn> = vec![];
            for n in 0..30 + 1 {
                let b = bernoulli(n, &mut context);
                res.push(Bn { value:b.clone(), index: n as i32});
            }
        });
    }

    #[bench]
    fn bench_bernoulli_iter(b: &mut Bencher) {
        b.iter(|| {
            let context = Context::new();
            let _res = context.take(30 + 1).collect::<Vec<_>>();
        });
    }
}

```

```txt

B( 0) =                                            1 / 1
B( 2) =                                            1 / 6
B( 4) =                                           -1 / 30
B( 6) =                                            1 / 42
B( 8) =                                           -1 / 30
B(10) =                                            5 / 66
B(12) =                                         -691 / 2730
B(14) =                                            7 / 6
B(16) =                                        -3617 / 510
B(18) =                                        43867 / 798
B(20) =                                      -174611 / 330
B(22) =                                       854513 / 138
B(24) =                                   -236364091 / 2730
B(26) =                                      8553103 / 6
B(28) =                                 -23749461029 / 870
B(30) =                                8615841276005 / 14322
B(32) =                               -7709321041217 / 510
B(34) =                                2577687858367 / 6
B(36) =                        -26315271553053477373 / 1919190
B(38) =                             2929993913841559 / 6
B(40) =                       -261082718496449122051 / 13530
B(42) =                       1520097643918070802691 / 1806
B(44) =                     -27833269579301024235023 / 690
B(46) =                     596451111593912163277961 / 282
B(48) =                -5609403368997817686249127547 / 46410
B(50) =                  495057205241079648212477525 / 66
B(52) =              -801165718135489957347924991853 / 1590
B(54) =             29149963634884862421418123812691 / 798
B(56) =          -2479392929313226753685415739663229 / 870
B(58) =          84483613348880041862046775994036021 / 354
B(60) = -1215233140483755572040304994079820246041491 / 56786730

```



## Scala

'''With Custom Rational Number Class'''<br/>
(code will run in Scala REPL with a cut-and-paste without need for a third-party library)

```scala
/** Roll our own pared-down BigFraction class just for these Bernoulli Numbers */
case class BFraction( numerator:BigInt, denominator:BigInt ) {
  require( denominator != BigInt(0), "Denominator cannot be zero" )

  val gcd = numerator.gcd(denominator)

  val num = numerator / gcd
  val den = denominator / gcd

  def unary_- = BFraction(-num, den)
  def -( that:BFraction ) = that match {
    case f if f.num == BigInt(0) => this
    case f if f.den == this.den => BFraction(this.num - f.num, this.den)
    case f => BFraction(((this.num * f.den) - (f.num * this.den)), this.den * f.den )
  }

  def *( that:Int ) = BFraction( num * that, den )

  override def toString = num + " / " + den
}


def bernoulliB( n:Int ) : BFraction = {

  val aa : Array[BFraction] = Array.ofDim(n+1)

  for( m <- 0 to n ) {
    aa(m) = BFraction(1,(m+1))

    for( n <- m to 1 by -1 ) {
      aa(n-1) = (aa(n-1) - aa(n)) * n
    }
  }

  aa(0)
}

assert( {val b12 = bernoulliB(12); b12.num == -691 && b12.den == 2730 } )

val r = for( n <- 0 to 60; b = bernoulliB(n) if b.num != 0 ) yield (n, b)

val numeratorSize = r.map(_._2.num.toString.length).max

// Print the results
r foreach{ case (i,b) => {
  val label = f"b($i)"
  val num = (" " * (numeratorSize - b.num.toString.length)) + b.num
  println( f"$label%-6s $num / ${b.den}" )
}}

```

```txt
b(0)                                              1 / 1
b(1)                                              1 / 2
b(2)                                              1 / 6
b(4)                                             -1 / 30
b(6)                                              1 / 42
b(8)                                             -1 / 30
b(10)                                             5 / 66
b(12)                                          -691 / 2730
b(14)                                             7 / 6
b(16)                                         -3617 / 510
b(18)                                         43867 / 798
b(20)                                       -174611 / 330
b(22)                                        854513 / 138
b(24)                                    -236364091 / 2730
b(26)                                       8553103 / 6
b(28)                                  -23749461029 / 870
b(30)                                 8615841276005 / 14322
b(32)                                -7709321041217 / 510
b(34)                                 2577687858367 / 6
b(36)                         -26315271553053477373 / 1919190
b(38)                              2929993913841559 / 6
b(40)                        -261082718496449122051 / 13530
b(42)                        1520097643918070802691 / 1806
b(44)                      -27833269579301024235023 / 690
b(46)                      596451111593912163277961 / 282
b(48)                 -5609403368997817686249127547 / 46410
b(50)                   495057205241079648212477525 / 66
b(52)               -801165718135489957347924991853 / 1590
b(54)              29149963634884862421418123812691 / 798
b(56)           -2479392929313226753685415739663229 / 870
b(58)           84483613348880041862046775994036021 / 354
b(60)  -1215233140483755572040304994079820246041491 / 56786730

```



## Seed7

The program below uses [http://seed7.sourceforge.net/manual/types.htm#bigRational bigRational]
numbers. The Bernoulli numbers are written as fraction and as decimal number, with possible repeating decimals.
The conversion of a bigRational number to [http://seed7.sourceforge.net/manual/types.htm#string string] is done
with the function [http://seed7.sourceforge.net/libraries/bigrat.htm#str(in_bigRational) str]. This
function automatically writes repeating decimals in parentheses, when necessary.


```seed7
$ include "seed7_05.s7i";
  include "bigrat.s7i";

const func bigRational: bernoulli (in integer: n) is func
  result
    var bigRational: bernoulli is bigRational.value;
  local
    var integer: m is 0;
    var integer: j is 0;
    var array bigRational: a is 0 times bigRational.value;
  begin
    a := [0 .. n] times bigRational.value;
    for m range 0 to n do
      a[m] := 1_ / bigInteger(succ(m));
      for j range m downto 1 do
        a[pred(j)] := bigRational(j) * (a[j] - a[pred(j)]);
      end for;
    end for;
    bernoulli := a[0];
  end func;

const proc: main is func
  local
    var bigRational: bernoulli is bigRational.value;
    var integer: i is 0;
  begin
    for i range 0 to 60 do
      bernoulli := bernoulli(i);
      if bernoulli <> bigRational.value then
        writeln("B(" <& i lpad 2 <& ") = " <& bernoulli.numerator lpad 44 <&
                " / " <& bernoulli.denominator rpad 8 <& " " <& bernoulli);
      end if;
    end for;
  end func;
```


```txt

B( 0) =                                            1 / 1        1.0
B( 1) =                                           -1 / 2        -0.5
B( 2) =                                            1 / 6        0.1(6)
B( 4) =                                           -1 / 30       -0.0(3)
B( 6) =                                            1 / 42       0.0(238095)
B( 8) =                                           -1 / 30       -0.0(3)
B(10) =                                            5 / 66       0.0(75)
B(12) =                                         -691 / 2730     -0.2(531135)
B(14) =                                            7 / 6        1.1(6)
B(16) =                                        -3617 / 510      -7.0(9215686274509803)
B(18) =                                        43867 / 798      54.9(711779448621553884)
B(20) =                                      -174611 / 330      -529.1(24)
B(22) =                                       854513 / 138      6192.1(2318840579710144927536)
B(24) =                                   -236364091 / 2730     -86580.2(531135)
B(26) =                                      8553103 / 6        1425517.1(6)
B(28) =                                 -23749461029 / 870      -27298231.0(6781609195402298850574712643)
B(30) =                                8615841276005 / 14322    601580873.9(006423683843038681748359167714)
B(32) =                               -7709321041217 / 510      -15116315767.0(9215686274509803)
B(34) =                                2577687858367 / 6        429614643061.1(6)
B(36) =                        -26315271553053477373 / 1919190  -13711655205088.3(327721590879485616)
B(38) =                             2929993913841559 / 6        488332318973593.1(6)
B(40) =                       -261082718496449122051 / 13530    -19296579341940068.1(4863266814)
B(42) =                       1520097643918070802691 / 1806     841693047573682615.0(005537098560354374307862679955703211517165)
B(44) =                     -27833269579301024235023 / 690      -40338071854059455413.0(7681159420289855072463)
B(46) =                     596451111593912163277961 / 282      2115074863808199160560.1(4539007092198581560283687943262411347517730496)
B(48) =                -5609403368997817686249127547 / 46410    -120866265222965259346027.3(119370825253178194354664942900237017884076707606)
B(50) =                  495057205241079648212477525 / 66       7500866746076964366855720.0(75)
B(52) =              -801165718135489957347924991853 / 1590     -503877810148106891413789303.0(5220125786163)
B(54) =             29149963634884862421418123812691 / 798      36528776484818123335110430842.9(711779448621553884)
B(56) =          -2479392929313226753685415739663229 / 870      -2849876930245088222626914643291.0(6781609195402298850574712643)
B(58) =          84483613348880041862046775994036021 / 354      238654274996836276446459819192192.1(4971751412429378531073446327683615819209039548022598870056)
B(60) = -1215233140483755572040304994079820246041491 / 56786730 -21399949257225333665810744765191097.3(926741511617238745742183076926598872659158222352299560126106)

```



## Sidef

Recursive solution (with auto-memoization):

```ruby
func bernoulli_number{}

func bern_helper(n, k) {
    binomial(n, k) * (bernoulli_number(k) / (n - k + 1))
}

func bern_diff(n, k, d) {
    n < k ? d : bern_diff(n, k + 1, d - bern_helper(n + 1, k))
}

bernoulli_number = func(n) is cached {

    n.is_one && return 1/2
    n.is_odd && return   0

    n > 0 ? bern_diff(n - 1, 0, 1) : 1
}

for i (0..60) {
    var num = bernoulli_number(i) || next
    printf("B(%2d) = %44s / %s\n", i, num.nude)
}
```


Iterative solution:

```ruby
func bernoulli_print {
    var a = []
    for m (0..60) {
        a.append(1/(m+1))
        for j (flip(1..m)) {
            (a[j-1] -= a[j]) *= j
        }
        a[0] || next
        printf("B(%2d) = %44s / %s\n", m, a[0].nude)
    }
}
 
bernoulli_print()
```


```txt

B( 0) =                                            1 / 1
B( 1) =                                            1 / 2
B( 2) =                                            1 / 6
B( 4) =                                           -1 / 30
B( 6) =                                            1 / 42
B( 8) =                                           -1 / 30
B(10) =                                            5 / 66
B(12) =                                         -691 / 2730
B(14) =                                            7 / 6
B(16) =                                        -3617 / 510
B(18) =                                        43867 / 798
B(20) =                                      -174611 / 330
B(22) =                                       854513 / 138
B(24) =                                   -236364091 / 2730
B(26) =                                      8553103 / 6
B(28) =                                 -23749461029 / 870
B(30) =                                8615841276005 / 14322
B(32) =                               -7709321041217 / 510
B(34) =                                2577687858367 / 6
B(36) =                        -26315271553053477373 / 1919190
B(38) =                             2929993913841559 / 6
B(40) =                       -261082718496449122051 / 13530
B(42) =                       1520097643918070802691 / 1806
B(44) =                     -27833269579301024235023 / 690
B(46) =                     596451111593912163277961 / 282
B(48) =                -5609403368997817686249127547 / 46410
B(50) =                  495057205241079648212477525 / 66
B(52) =              -801165718135489957347924991853 / 1590
B(54) =             29149963634884862421418123812691 / 798
B(56) =          -2479392929313226753685415739663229 / 870
B(58) =          84483613348880041862046775994036021 / 354
B(60) = -1215233140483755572040304994079820246041491 / 56786730

```



## SPAD

```SPAD

for n in 0..60 | (b:=bernoulli(n)$INTHEORY; b~=0) repeat print [n,b]

```

Package:[http://fricas.github.io/api/IntegerNumberTheoryFunctions.html?highlight=bernoulli IntegerNumberTheoryFunctions]

```txt


### =========

Format: [n,B_n]

### =========

   [0,1]
        1
   [1,- -]
        2
      1
   [2,-]
      6
         1
   [4,- --]
        30
       1
   [6,--]
      42
         1
   [8,- --]
        30
        5
   [10,--]
       66
          691
   [12,- ----]
         2730
       7
   [14,-]
       6
         3617
   [16,- ----]
          510
       43867
   [18,-----]
        798
         174611
   [20,- ------]
           330
       854513
   [22,------]
         138
         236364091
   [24,- ---------]
            2730
       8553103
   [26,-------]
          6
         23749461029
   [28,- -----------]
             870
       8615841276005
   [30,-------------]
           14322
         7709321041217
   [32,- -------------]
              510
       2577687858367
   [34,-------------]
             6
         26315271553053477373
   [36,- --------------------]
                1919190
       2929993913841559
   [38,----------------]
               6
         261082718496449122051
   [40,- ---------------------]
                 13530
       1520097643918070802691
   [42,----------------------]
                1806
         27833269579301024235023
   [44,- -----------------------]
                   690
       596451111593912163277961
   [46,------------------------]
                  282
         5609403368997817686249127547
   [48,- ----------------------------]
                     46410
       495057205241079648212477525
   [50,---------------------------]
                    66
         801165718135489957347924991853
   [52,- ------------------------------]
                      1590
       29149963634884862421418123812691
   [54,--------------------------------]
                      798
         2479392929313226753685415739663229
   [56,- ----------------------------------]
                         870
       84483613348880041862046775994036021
   [58,-----------------------------------]
                       354
         1215233140483755572040304994079820246041491
   [60,- -------------------------------------------]
                           56786730
                                                                   Type: Void

```



## Tcl


```tcl
proc bernoulli {n} {
    for {set m 0} {$m <= $n} {incr m} {
	lappend A [list 1 [expr {$m + 1}]]
	for {set j $m} {[set i $j] >= 1} {} {
	    lassign [lindex $A [incr j -1]] a1 b1
	    lassign [lindex $A $i] a2 b2
	    set x [set p [expr {$i * ($a1*$b2 - $a2*$b1)}]]
	    set y [set q [expr {$b1 * $b2}]]
	    while {$q} {set q [expr {$p % [set p $q]}]}
	    lset A $j [list [expr {$x/$p}] [expr {$y/$p}]]
	}
    }
    return [lindex $A 0]
}

set len 0
for {set n 0} {$n <= 60} {incr n} {
    set b [bernoulli $n]
    if {[lindex $b 0]} {
	lappend result $n {*}$b
	set len [expr {max($len, [string length [lindex $b 0]])}]
    }
}
foreach {n num denom} $result {
    puts [format {B_%-2d = %*lld/%lld} $n $len $num $denom]
}
```

```txt

B_0  =                                            1/1
B_1  =                                            1/2
B_2  =                                            1/6
B_4  =                                           -1/30
B_6  =                                            1/42
B_8  =                                           -1/30
B_10 =                                            5/66
B_12 =                                         -691/2730
B_14 =                                            7/6
B_16 =                                        -3617/510
B_18 =                                        43867/798
B_20 =                                      -174611/330
B_22 =                                       854513/138
B_24 =                                   -236364091/2730
B_26 =                                      8553103/6
B_28 =                                 -23749461029/870
B_30 =                                8615841276005/14322
B_32 =                               -7709321041217/510
B_34 =                                2577687858367/6
B_36 =                        -26315271553053477373/1919190
B_38 =                             2929993913841559/6
B_40 =                       -261082718496449122051/13530
B_42 =                       1520097643918070802691/1806
B_44 =                     -27833269579301024235023/690
B_46 =                     596451111593912163277961/282
B_48 =                -5609403368997817686249127547/46410
B_50 =                  495057205241079648212477525/66
B_52 =              -801165718135489957347924991853/1590
B_54 =             29149963634884862421418123812691/798
B_56 =          -2479392929313226753685415739663229/870
B_58 =          84483613348880041862046775994036021/354
B_60 = -1215233140483755572040304994079820246041491/56786730

```



## Visual Basic .NET

```vbnet
' Bernoulli numbers - vb.net - 06/03/2017
Imports System.Numerics 'BigInteger

Module Bernoulli_numbers

    Function gcd_BigInt(ByVal x As BigInteger, ByVal y As BigInteger) As BigInteger
        Dim y2 As BigInteger
        x = BigInteger.Abs(x)
        Do
            y2 = BigInteger.Remainder(x, y)
            x = y
            y = y2
        Loop Until y = 0
        Return x
    End Function 'gcd_BigInt

    Sub bernoul_BigInt(n As Integer, ByRef bnum As BigInteger, ByRef bden As BigInteger)
        Dim j, m As Integer
        Dim f As BigInteger
        Dim anum(), aden() As BigInteger
        ReDim anum(n + 1), aden(n + 1)
        For m = 0 To n
            anum(m + 1) = 1
            aden(m + 1) = m + 1
            For j = m To 1 Step -1
                anum(j) = j * (aden(j + 1) * anum(j) - aden(j) * anum(j + 1))
                aden(j) = aden(j) * aden(j + 1)
                f = gcd_BigInt(BigInteger.Abs(anum(j)), BigInteger.Abs(aden(j)))
                If f <> 1 Then
                    anum(j) = anum(j) / f
                    aden(j) = aden(j) / f
                End If
            Next
        Next
        bnum = anum(1) : bden = aden(1)
    End Sub 'bernoul_BigInt

    Sub bernoulli_BigInt()
        Dim i As Integer
        Dim bnum, bden As BigInteger
        bnum = 0 : bden = 0
        For i = 0 To 60
            bernoul_BigInt(i, bnum, bden)
            If bnum <> 0 Then
                Console.WriteLine("B(" & i & ")=" & bnum.ToString("D") & "/" & bden.ToString("D"))
            End If
        Next i
    End Sub 'bernoulli_BigInt

End Module 'Bernoulli_numbers
```

```txt

B(0)=1/1
B(1)=1/2
B(2)=1/6
B(4)=-1/30
B(6)=1/42
B(8)=-1/30
B(10)=5/66
B(12)=-691/2730
B(14)=7/6
B(16)=-3617/510
B(18)=43867/798
B(20)=-174611/330
B(22)=854513/138
B(24)=-236364091/2730
B(26)=8553103/6
B(28)=-23749461029/870
B(30)=8615841276005/14322
B(32)=-7709321041217/510
B(34)=2577687858367/6
B(36)=-26315271553053477373/1919190
B(38)=2929993913841559/6
B(40)=-261082718496449122051/13530
B(42)=1520097643918070802691/1806
B(44)=-27833269579301024235023/690
B(46)=596451111593912163277961/282
B(48)=-5609403368997817686249127547/46410
B(50)=495057205241079648212477525/66
B(52)=-801165718135489957347924991853/1590
B(54)=29149963634884862421418123812691/798
B(56)=-2479392929313226753685415739663229/870
B(58)=84483613348880041862046775994036021/354
B(60)=-1215233140483755572040304994079820246041491/56786730

```



## zkl

Uses lib GMP (GNU MP Bignum Library).

```zkl
class Rational{  // Weenie Rational class, can handle BigInts
   fcn init(_a,_b){ var a=_a, b=_b; normalize(); }
   fcn toString{ "%50d / %d".fmt(a,b) }
   fcn normalize{  // divide a and b by gcd
      g:= a.gcd(b);
      a/=g; b/=g;
      if(b<0){ a=-a; b=-b; } // denominator > 0
      self
   }
   fcn __opAdd(n){
      if(Rational.isChildOf(n)) self(a*n.b + b*n.a, b*n.b); // Rat + Rat
      else self(b*n + a, b);				    // Rat + Int
   }
   fcn __opSub(n){ self(a*n.b - b*n.a, b*n.b) }		    // Rat - Rat
   fcn __opMul(n){
      if(Rational.isChildOf(n)) self(a*n.a, b*n.b);	    // Rat * Rat
      else self(a*n, b);				    // Rat * Int
   }
   fcn __opDiv(n){ self(a*n.b,b*n.a) }			    // Rat / Rat
}
```


```zkl
var [const] BN=Import.lib("zklBigNum");	// libGMP (GNU MP Bignum Library)
fcn B(N){				// calculate Bernoulli(n)
   var A=List.createLong(100,0);  // aka static aka not thread safe
   foreach m in (N+1){
      A[m]=Rational(BN(1),BN(m+1));
      foreach j in ([m..1, -1]){ A[j-1]= (A[j-1] - A[j])*j; }
   }
   A[0]
}
```


```zkl
foreach b in ([0..1].chain([2..60,2])){ println("B(%2d)%s".fmt(b,B(b))) }
```

```txt

B( 0)                                                 1 / 1
B( 1)                                                 1 / 2
B( 2)                                                 1 / 6
B( 4)                                                -1 / 30
B( 6)                                                 1 / 42
B( 8)                                                -1 / 30
B(10)                                                 5 / 66
B(12)                                              -691 / 2730
B(14)                                                 7 / 6
B(16)                                             -3617 / 510
B(18)                                             43867 / 798
B(20)                                           -174611 / 330
B(22)                                            854513 / 138
B(24)                                        -236364091 / 2730
B(26)                                           8553103 / 6
B(28)                                      -23749461029 / 870
B(30)                                     8615841276005 / 14322
B(32)                                    -7709321041217 / 510
B(34)                                     2577687858367 / 6
B(36)                             -26315271553053477373 / 1919190
B(38)                                  2929993913841559 / 6
B(40)                            -261082718496449122051 / 13530
B(42)                            1520097643918070802691 / 1806
B(44)                          -27833269579301024235023 / 690
B(46)                          596451111593912163277961 / 282
B(48)                     -5609403368997817686249127547 / 46410
B(50)                       495057205241079648212477525 / 66
B(52)                   -801165718135489957347924991853 / 1590
B(54)                  29149963634884862421418123812691 / 798
B(56)               -2479392929313226753685415739663229 / 870
B(58)               84483613348880041862046775994036021 / 354
B(60)      -1215233140483755572040304994079820246041491 / 56786730

```

