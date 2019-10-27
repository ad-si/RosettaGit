+++
title = "Left factorials"
description = ""
date = 2019-09-20T21:00:57Z
aliases = []
[extra]
id = 17467
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Mathematics]]
<!-- Left Factorial !-->

'''Left factorials''',   <big><big>!n</big></big>,   may refer to either   ''subfactorials''   or to   ''factorial sums''; 

the same notation can be confusingly seen used for the two different definitions.

Sometimes,   ''subfactorials''   (also known as ''derangements'')   may use any of the notations: 
:::::::*   <big><big> <span style="font-family:serif">!''n''`</span> </big></big> 
:::::::*   <big><big> <span style="font-family:serif">!''n''</span>  </big></big>
:::::::*   <big><big> <span style="font-family:serif">''n''¡</span>  </big></big>

(It may not be visually obvious, but the last example uses an upside-down exclamation mark.)



This Rosetta Code task will be using this formula for '''left factorial''':
<big><big>
:::   <math> !n = \sum_{k=0}^{n-1} k! </math>
</big></big>
where
<big><big>
:::   <math>!0 = 0</math>
</big></big>


;Task
Display the left factorials for:
* zero through ten (inclusive)
* 20 through 110 (inclusive) by tens



Display the length (in decimal digits) of the left factorials for:
* 1,000,   2,000   through   10,000   (inclusive), by thousands.


;Also see
*   The OEIS entry: [http://oeis.org/A003422 A003422 left factorials]
*   The MathWorld entry: [http://mathworld.wolfram.com/LeftFactorial.html left factorial]
*   The MathWorld entry: [http://mathworld.wolfram.com/FactorialSums.html factorial sums]
*   The MathWorld entry: [http://mathworld.wolfram.com/Subfactorial.html subfactorial]


;Related task:
*   [http://rosettacode.org/wiki/Permutations/Derangements permutations/derangements (subfactorials)]





## ALGOL 68

Uses the Algol 68G LONG LONG INT type which has programmer definable precision.
{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}

```algol68
# set the precision of LONG LONG INT - large enough for !n up to ! 10 000 #
PR precision 36000 PR
# stores left factorials in an array #
# we calculate the left factorials, storing their values in the "values" array #
# if step is <= 1, we store we store every left factorial, otherwise we store !x when x MOD step = 0 #
# note this means values[ 0 ] is always !0 #
PROC get left factorials = ( REF[]LONG LONG INT values, INT step )VOID:
     BEGIN
         INT store position        := LWB values;
         INT max values            := UPB values;
         LONG LONG INT result      := 0;
         LONG LONG INT factorial k := 1;
         FOR k FROM 0
         WHILE
             IF IF step <= 1 THEN TRUE ELSE k MOD step = 0 FI THEN
                 values[ store position ] := result;
                 store position +:= 1
             FI;
             store position <= max values
         DO
             result      +:= factorial k;
             factorial k *:= ( k + 1 )
         OD
     END # get left factorials # ;

# returns the number of digits in n #
OP DIGITCOUNT = ( LONG LONG INT n )INT:
   BEGIN
        INT result := 1;
        LONG LONG INT v := ABS n;
        WHILE v > 100 000 000 DO
            result +:= 8;
            v OVERAB 100 000 000
        OD;
        WHILE v > 10 DO
            result +:= 1;
            v OVERAB 10
        OD;
        result
   END # DIGITCOUNT # ;

BEGIN
    print( ( "!n for n = 0(1)10", newline ) );
    [ 0 : 10 ]LONG LONG INT v;
    get left factorials( v, 1 );
    FOR i FROM 0 TO UPB v DO
        print( ( whole( v[ i ], 0 ), newline ) )
    OD
END;

BEGIN
    print( ( "!n for n = 20(10)110", newline ) );
    [ 0 : 11 ]LONG LONG INT v;
    get left factorials( v, 10 );
    FOR i FROM 2 TO UPB v DO
        print( ( whole( v[ i ], 0 ), newline ) )
    OD
END;

BEGIN
    print( ( "digit counts of !n for n = 1000(1000)10 000", newline ) );
    [ 0 : 10 ]LONG LONG INT v;
    get left factorials( v, 1 000 );
    FOR i FROM 1 TO UPB v DO
        print( ( whole( DIGITCOUNT v[ i ], 0 ), newline ) )
    OD
END

```

{{out}}

```txt

!n for n = 0(1)10
0
1
2
4
10
34
154
874
5914
46234
409114
!n for n = 20(10)110
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
digit counts of !n for n = 1000(1000)10 000
2565
5733
9128
12670
16322
20062
23875
27749
31678
35656

```



## Bracmat

{{trans|D}}

```bracmat
( ( leftFact
  =   result factorial i
    .   0:?result
      & 1:?factorial
      & 0:?i
      &   whl
        ' ( !i+1:~>!arg:?i
          & !factorial+!result:?result
          & !factorial*!i:?factorial
          )
      & !result
  )
& ( iterate
  =   from to step c fun
    .   !arg:(?from.?to.?step.?fun)
      & !from+-1*!step:?from
      & !step:?c
      &   whl
        ' ( !step+!from:~>!to:?from
          & !fun$(leftFact$!from)
          )
      & 
  )
& out$"First 11 left factorials:"
& iterate$(0.10.1.out)
& out$"
20 through 110 (inclusive) by tens:"
& iterate$(20.110.10.out)
& out$"
Digits in 1,000 through 10,000 by thousands:"
&   iterate
  $ ( 1000
    . 10000
    . 1000
    . (=L.@(!arg:? [?L)&out$!L)
    )
)
```

{{out}}

```txt
First 11 left factorials:
0
1
2
4
10
34
154
874
5914
46234
409114

20 through 110 (inclusive) by tens:
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

Digits in 1,000 through 10,000 by thousands:
2565
5733
9128
12670
16322
20062
23875
27749
31678
35656
```



## C

{{libheader|GMP}}

```C

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gmp.h>

void mpz_left_fac_ui(mpz_t rop, unsigned long op)
{
    mpz_t t1;
    mpz_init_set_ui(t1, 1);
    mpz_set_ui(rop, 0);

    size_t i;
    for (i = 1; i <= op; ++i) {
        mpz_add(rop, rop, t1);
        mpz_mul_ui(t1, t1, i);
    }

    mpz_clear(t1);
}

size_t mpz_digitcount(mpz_t op)
{
    /* mpz_sizeinbase can not be trusted to give accurate base 10 length */
    char *t    = mpz_get_str(NULL, 10, op);
    size_t ret = strlen(t);
    free(t);
    return ret;
}

int main(void)
{
    mpz_t t;
    mpz_init(t);
    size_t i;

    for (i = 0; i <= 110; ++i) {
        if (i <= 10 || i % 10 == 0) {
            mpz_left_fac_ui(t, i);
            gmp_printf("!%u = %Zd\n", i, t);
        }
    }

    for (i = 1000; i <= 10000; i += 1000) {
        mpz_left_fac_ui(t, i);
        printf("!%u has %u digits\n", i, mpz_digitcount(t));
    }

    mpz_clear(t);
    return 0;
}

```

{{out}}

```txt

!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits
!2000 has 5733 digits
!3000 has 9128 digits
!4000 has 12670 digits
!5000 has 16322 digits
!6000 has 20062 digits
!7000 has 23875 digits
!8000 has 27749 digits
!9000 has 31678 digits
!10000 has 35656 digits

```



## C++


```CPP

#include <vector>
#include <string>
#include <algorithm>
#include <iostream>
#include <sstream>
using namespace std;

#if 1 // optimized for 64-bit architecture
typedef unsigned long usingle;
typedef unsigned long long udouble;
const int word_len = 32;
#else // optimized for 32-bit architecture
typedef unsigned short usingle;
typedef unsigned long udouble;
const int word_len = 16;
#endif

class bignum {
private:
    // rep_.size() == 0 if and only if the value is zero.
    // Otherwise, the word rep_[0] keeps the least significant bits.
    vector<usingle> rep_;
public:
    explicit bignum(usingle n = 0) { if (n > 0) rep_.push_back(n); }
    bool equals(usingle n) const {
        if (n == 0) return rep_.empty();
        if (rep_.size() > 1) return false;
        return rep_[0] == n;
    }
    bignum add(usingle addend) const {
        bignum result(0);
        udouble sum = addend;
        for (size_t i = 0; i < rep_.size(); ++i) {
            sum += rep_[i];
            result.rep_.push_back(sum & (((udouble)1 << word_len) - 1));
            sum >>= word_len;
        }
        if (sum > 0) result.rep_.push_back((usingle)sum);
        return result;
    }
    bignum add(const bignum& addend) const {
        bignum result(0);
        udouble sum = 0;
        size_t sz1 = rep_.size();
        size_t sz2 = addend.rep_.size();
        for (size_t i = 0; i < max(sz1, sz2); ++i) {
            if (i < sz1) sum += rep_[i];
            if (i < sz2) sum += addend.rep_[i];
            result.rep_.push_back(sum & (((udouble)1 << word_len) - 1));
            sum >>= word_len;
        }
        if (sum > 0) result.rep_.push_back((usingle)sum);
        return result;
    }
    bignum multiply(usingle factor) const {
        bignum result(0);
        udouble product = 0;
        for (size_t i = 0; i < rep_.size(); ++i) {
            product += (udouble)rep_[i] * factor;
            result.rep_.push_back(product & (((udouble)1 << word_len) - 1));
            product >>= word_len;
        }
        if (product > 0)
            result.rep_.push_back((usingle)product);
        return result;
    }
    void divide(usingle divisor, bignum& quotient, usingle& remainder) const {
        quotient.rep_.resize(0);
        udouble dividend = 0;
        remainder = 0;
        for (size_t i = rep_.size(); i > 0; --i) {
            dividend = ((udouble)remainder << word_len) + rep_[i - 1];
            usingle quo = (usingle)(dividend / divisor);
            remainder = (usingle)(dividend % divisor);
            if (quo > 0 || i < rep_.size())
                quotient.rep_.push_back(quo);
        }
        reverse(quotient.rep_.begin(), quotient.rep_.end());
    }
};

ostream& operator<<(ostream& os, const bignum& x);

ostream& operator<<(ostream& os, const bignum& x) {
    string rep;
    bignum dividend = x;
    bignum quotient;
    usingle remainder;
    while (true) {
        dividend.divide(10, quotient, remainder);
        rep += (char)('0' + remainder);
        if (quotient.equals(0)) break;
        dividend = quotient;
    }
    reverse(rep.begin(), rep.end());
    os << rep;
    return os;
}

bignum lfact(usingle n);

bignum lfact(usingle n) {
    bignum result(0);
    bignum f(1);
    for (usingle k = 1; k <= n; ++k) {
        result = result.add(f);
        f = f.multiply(k);
    }
    return result;
}

int main() {
    for (usingle i = 0; i <= 10; ++i) {
        cout << "!" << i << " = " << lfact(i) << endl;
    }

    for (usingle i = 20; i <= 110; i += 10) {
        cout << "!" << i << " = " << lfact(i) << endl;
    }

    for (usingle i = 1000; i <= 10000; i += 1000) {
        stringstream ss;
        ss << lfact(i);
        cout << "!" << i << " has " << ss.str().size()
            << " digits." << endl;
    }
}

```

{{out}}

```txt

!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits.
!2000 has 5733 digits.
!3000 has 9128 digits.
!4000 has 12670 digits.
!5000 has 16322 digits.
!6000 has 20062 digits.
!7000 has 23875 digits.
!8000 has 27749 digits.
!9000 has 31678 digits.
!10000 has 35656 digits.

```


=={{header|C sharp|C#}}==

```csharp

using System;
using System.Numerics;

namespace LeftFactorial
{
    class Program
    {
        static void Main(string[] args)
        {
            for (int i = 0; i <= 10; i++)
            {
                Console.WriteLine(string.Format("!{0} = {1}", i, LeftFactorial(i)));
            }

            for (int j = 20; j <= 110; j += 10)
            {
                Console.WriteLine(string.Format("!{0} = {1}", j, LeftFactorial(j)));
            }

            for (int k = 1000; k <= 10000; k += 1000)
            {
                Console.WriteLine(string.Format("!{0} has {1} digits", k, LeftFactorial(k).ToString().Length));
            }

            Console.ReadKey();
        }

        private static BigInteger Factorial(int number)
        {
            BigInteger accumulator = 1;

            for (int factor = 1; factor <= number; factor++)
            {
                accumulator *= factor;
            }

            return accumulator;
        }

        private static BigInteger LeftFactorial(int n)
        {
            BigInteger result = 0;

            for (int i = 0; i < n; i++)
            {
                result += Factorial(i);
            }

            return result;
        }
    }
}

```

{{out}}

```txt

!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits
!2000 has 5733 digits
!3000 has 9128 digits
!4000 has 12670 digits
!5000 has 16322 digits
!6000 has 20062 digits
!7000 has 23875 digits
!8000 has 27749 digits
!9000 has 31678 digits
!10000 has 35656 digits

```


Faster Implementation


```csharp

using System;
using System.Numerics;

namespace LeftFactorial
{
    class Program
    {
        static void Main(string[] args)
        {
            for (int i = 0; i <= 10; i++)
            {
                Console.WriteLine(string.Format("!{0} : {1}", i, LeftFactorial(i)));
            }

            for (int j = 20; j <= 110; j += 10)
            {
                Console.WriteLine(string.Format("!{0} : {1}", j, LeftFactorial(j)));
            }

            for (int k = 1000; k <= 10000; k += 1000)
            {
                Console.WriteLine(string.Format("!{0} : has {1} digits", k, LeftFactorial(k).ToString().Length));
            }

            Console.ReadKey();
        }

        private static BigInteger LeftFactorial(int n)
        {
            BigInteger result = 0;
            BigInteger subResult = 1;

            for (int i = 0; i < n; i++)
            {
                if (i == 0)
                {
                    subResult = 1;
                }
                else
                {
                    subResult *= i;
                }

                result += subResult;
            }

            return result;
        }
    }
}

```



## Clojure


```lisp
(ns left-factorial
  (:gen-class))

(defn left-factorial [n]
  " Compute by updating the state [fact summ] for each k, where k equals 1 to n
    Update is next state is [k*fact (summ+k)"
  (second
    (reduce (fn [[fact summ] k]
              [(*' fact k) (+ summ fact)])
            [1 0] (range 1 (inc n)))))

(doseq [n (range 11)]
  (println (format "!%-3d = %5d" n (left-factorial n))))

(doseq [n (range 20 111 10)]
(println (format "!%-3d = %5d" n (biginteger (left-factorial n)))))

(doseq [n (range 1000 10001 1000)]
  (println (format "!%-5d has %5d digits" n (count (str (biginteger (left-factorial n)))))))

```

{{Output}}

```txt

!0   =     0
!1   =     1
!2   =     2
!3   =     4
!4   =    10
!5   =    34
!6   =   154
!7   =   874
!8   =  5914
!9   = 46234
!10  = 409114
!20  = 128425485935180314
!30  = 9157958657951075573395300940314
!40  = 20935051082417771847631371547939998232420940314
!50  = 620960027832821612639424806694551108812720525606160920420940314
!60  = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70  = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80  = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90  = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000  has  2565 digits
!2000  has  5733 digits
!3000  has  9128 digits
!4000  has 12670 digits
!5000  has 16322 digits
!6000  has 20062 digits
!7000  has 23875 digits
!8000  has 27749 digits
!9000  has 31678 digits
!10000 has 35656 digits

```



## Common Lisp


```lisp

(defun fact (n)
  (reduce #'* (loop for i from 1 to n collect i)))

(defun left-fac (n)
  (reduce #'+ (loop for i below n collect (fact i))))

(format t "0 -> 10~&")
(format t "~a~&" (loop for i upto 10 collect (left-fac i)))
(format t "20 -> 110 by 10~&")
(format t "~{~a~&~}" (loop for i from 20 upto 110 by 10 collect (left-fac i)))
(format t "1000 -> 10000 by 1000~&")
(format t "~{~a digits~&~}" (loop for i from 1000 upto 10000 by 1000 collect (length (format nil "~a" (left-fac i)))))

```

{{out}}

```txt

0 -> 10
(0 1 2 4 10 34 154 874 5914 46234 409114)
20 -> 110 by 10
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
1000 -> 10000 by 1000
2565 digits
5733 digits
9128 digits
12670 digits
16322 digits
20062 digits
23875 digits
27749 digits
31678 digits
35656 digits

```



## D


```d
import std.stdio, std.bigint, std.range, std.algorithm, std.conv;

BigInt leftFact(in uint n) pure nothrow /*@safe*/ {
    BigInt result = 0, factorial = 1;
    foreach (immutable i; 1 .. n + 1) {
        result += factorial;
        factorial *= i;
    }
    return result;
}

void main() {
    writeln("First 11 left factorials:\n", 11.iota.map!leftFact);
    writefln("\n20 through 110 (inclusive) by tens:\n%(%s\n%)",
             iota(20, 111, 10).map!leftFact);
    writefln("\nDigits in 1,000 through 10,000 by thousands:\n%s",
             iota(1_000, 10_001, 1_000).map!(i => i.leftFact.text.length));
}
```

{{out}}

```txt
First 11 left factorials:
[0, 1, 2, 4, 10, 34, 154, 874, 5914, 46234, 409114]

20 through 110 (inclusive) by tens:
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

Digits in 1,000 through 10,000 by thousands:
[2565, 5733, 9128, 12670, 16322, 20062, 23875, 27749, 31678, 35656]
```



## EchoLisp

We use the 'bigint' library and memoization : (remember 'function).

```lisp

(lib 'bigint)
(define (!n n) 
	(if (zero? n)  0
	(+ (!n (1- n)) (factorial (1- n)))))
(remember '!n)

```

Output:

```lisp

(for ((n 11)) (printf "!n(%d) = %d" n (!n n)))
(for ((n (in-range 20 120 10))) (printf "!n(%d) = %d" n (!n n)))
!n(0) = 0
!n(1) = 1
!n(2) = 2
!n(3) = 4
!n(4) = 10
!n(5) = 34
!n(6) = 154
!n(7) = 874
!n(8) = 5914
!n(9) = 46234
!n(10) = 409114
!n(20) = 128425485935180314
!n(30) = 9157958657951075573395300940314
!n(40) = 20935051082417771847631371547939998232420940314
!n(50) = 620960027832821612639424806694551108812720525606160920420940314
!n(60) = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!n(70) = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!n(80) = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!n(90) = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!n(100) = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!n(110) = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

; Compute !n : 5 seconds
(for ((n (in-range 1000 10001 500))) (!n n) (writeln n))
; Display results : 12 seconds
(for ((n (in-range 1000 10001 1000))) (printf "Digits of !n(%d) = %d" n (number-length (!n n))))
Digits of !n(1000) = 2565
Digits of !n(2000) = 5733
Digits of !n(3000) = 9128
Digits of !n(4000) = 12670
Digits of !n(5000) = 16322
Digits of !n(6000) = 20062
Digits of !n(7000) = 23875
Digits of !n(8000) = 27749
Digits of !n(9000) = 31678
Digits of !n(10000) = 35656

```



## Elixir


```elixir
defmodule LeftFactorial do
  def calc(0), do: 0
  def calc(n) do
    {result, _factorial} = Enum.reduce(1..n, {0, 1}, fn i,{res, fact} ->
      {res + fact, fact * i}
    end)
    result
  end
end

Enum.each(0..10, fn i ->
  IO.puts "!#{i} = #{LeftFactorial.calc(i)}"
end)
Enum.each(Enum.take_every(20..110, 10), fn i ->
  IO.puts "!#{i} = #{LeftFactorial.calc(i)}"
end)
Enum.each(Enum.take_every(1000..10000, 1000), fn i ->
  digits = LeftFactorial.calc(i) |> to_char_list |> length
  IO.puts "!#{i} has #{digits} digits"
end)
```


{{out}}

```txt

!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits
!2000 has 5733 digits
!3000 has 9128 digits
!4000 has 12670 digits
!5000 has 16322 digits
!6000 has 20062 digits
!7000 has 23875 digits
!8000 has 27749 digits
!9000 has 31678 digits
!10000 has 35656 digits

```


=={{header|F_Sharp|F#}}==
===The Functıon===

```fsharp

// Generate Sequence of Left Factorials: Nigel Galloway, March 5th., 2019.
let LF=Seq.unfold(fun (Σ,n,g)->Some(Σ,(Σ+n,n*g,g+1I))) (0I,1I,1I)

```


### The Tasks

;Display LF 0..10

```fsharp

LF |> Seq.take 11|>Seq.iter(printfn "%A")

```

{{out}}

```txt

0
1
2
4
10
34
154
874
5914
46234
409114

```

;Display LF 20..110 in steps of 10

```fsharp

LF |> Seq.skip 20 |> Seq.take 91 |> Seq.iteri(fun n g->if n%10=0 then printfn "%A" g)

```

{{out}}

```txt

128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

```

;Display the length (in decimal digits) of LF 1000 .. 10000 in steps of 1000

```fsharp

LF |> Seq.skip 1000 |> Seq.take 9001 |> Seq.iteri(fun n g->if n%1000=0 then printfn "%d" (string g).Length)

```

{{out}}

```txt

2565
5733
9128
12670
16322
20062
23875
27749
31678
35656

```





## Factor

{{works with|Factor|0.98}}
<lang>USING: formatting fry io kernel math math.factorials
math.functions math.parser math.ranges sequences ;
IN: rosetta-code.left-factorials

: left-factorial ( n -- m ) <iota> [ n! ] map-sum ;

: print-left-factorials ( seq quot -- )
    '[
        dup left-factorial @
        [ number>string "!" prepend ] dip
        "%6s   %-6d\n" printf
    ] each nl ; inline
    
: digit-count ( n -- count ) log10 >integer 1 + ;

: part1 ( -- ) 11 <iota> [ ] print-left-factorials ;
    
: part2 ( -- ) 20 110 10 <range> [ ] print-left-factorials ;

: part3 ( -- )
    "Number of digits for" print
    1,000 10,000 1,000 <range>
    [ digit-count ] print-left-factorials ;
    
: main ( -- ) part1 part2 part3 ;

MAIN: main
```

{{out}}

```txt

    !0   0     
    !1   1     
    !2   2     
    !3   4     
    !4   10    
    !5   34    
    !6   154   
    !7   874   
    !8   5914  
    !9   46234 
   !10   409114

   !20   128425485935180314
   !30   9157958657951075573395300940314
   !40   20935051082417771847631371547939998232420940314
   !50   620960027832821612639424806694551108812720525606160920420940314
   !60   141074930726669571000530822087000522211656242116439949000980378746128920420940314
   !70   173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
   !80   906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
   !90   16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
  !100   942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
  !110   145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

Number of digits for
 !1000   2565  
 !2000   5733  
 !3000   9128  
 !4000   12670 
 !5000   16322 
 !6000   20062 
 !7000   23875 
 !8000   27749 
 !9000   31678 
!10000   35656 

```



## Forth

{{works with|Gforth 0.7.3}}
This solution inspired by the Fortran one.

```Forth
36000 CONSTANT #DIGITS  \ Enough for !10000
CREATE S #DIGITS ALLOT  S #DIGITS ERASE  VARIABLE S#
CREATE F #DIGITS ALLOT  F #DIGITS ERASE  VARIABLE F#
1 F C!  1 F# !  \ F = 1 = 0! 

\ "Bignums": represented by two cells on the stack:
\ 1) An address pointing to the least-significant unit
\ 2) An integer size representing the number of character-size units
: mod/   /mod swap ;
: B+ ( addr u addr' u' -- u'')  \ Add the second "bignum" into the first
   over + >R  -rot over + >R ( addr' addr R:end' R:end)
   swap >R 0 over R>  ( addr 0 addr addr' R:end' R:end)
   \ 0: Assume second has equal or more digits, as in our problem
   BEGIN over R@ < WHILE  \ 1: add all digits from S
     dup >R C@ swap dup >R C@ ( addr c a a' R:end' R:end R:addr'* R:addr*)
     + +  10 mod/ R@ C!  R> 1+ R> 1+
   REPEAT R> drop  ( addr c addr* addr'* R:end')
   BEGIN dup R@ < WHILE   \ 2: add any remaining digits from F
     dup >R C@ swap >R        ( addr c a' R:end' R:addr'* R:addr*)
     +    10 mod/ R@ C!  R> 1+ R> 1+
   REPEAT R> drop drop  ( addr c addr*)
   BEGIN over WHILE       \ 3: add any carry digits
     >R 10 mod/ ( addr m d R:addr*) R@ C! R> 1+
   REPEAT  rot - nip ;  \ calculate travel distance, discard 0 carry
: B* ( addr u u' -- u'')  \ Multiply "bignum" inplace by U' 
   0 2swap over >R dup >R bounds  ( u' 0 addr+u addr R:addr R:u)
   DO ( u' c) over I C@ * +  10 mod/ I C! LOOP
   nip R> BEGIN ( c u) over WHILE  \ insert carry, may have multiple digits
     >R  10 mod/  R@ swap R> R@ + ( m u d addr+u R:addr) C!  1+
   REPEAT  nip R> ( u'' addr) drop ;
: .B ( addr u)  over +  BEGIN 1-  \ print bignum
     dup C@ [char] 0 + EMIT  over over >=
   UNTIL  drop drop ;
: .!n   0 <# #s [char] ! hold #> 6 over - spaces type space ; 
: REPORT ( n)
   dup 10 <=  over dup  20 111 within  swap 10 mod 0= and or
   IF .!n [char] = emit space S S# @ .B cr
   ELSE dup 1000 mod 0=
     IF .!n ." has " S# @ . ." digits" cr
     ELSE drop THEN
   THEN ;
: GO   0 REPORT
   1 BEGIN dup 10000 <=
   WHILE
     S S# @  F F# @      B+ S# !
     dup REPORT
     dup     F F# @  rot B* F# !
   1+ REPEAT  drop ;
```

{{out}}

```txt
$ gforth left-factorials.fs -e 'GO bye'
    !0 = 0
    !1 = 1
    !2 = 2
    !3 = 4
    !4 = 10
    !5 = 34
    !6 = 154
    !7 = 874
    !8 = 5914
    !9 = 46234
   !10 = 409114
   !20 = 128425485935180314
   !30 = 9157958657951075573395300940314
   !40 = 20935051082417771847631371547939998232420940314
   !50 = 620960027832821612639424806694551108812720525606160920420940314
   !60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
   !70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
   !80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
   !90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
  !100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
  !110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
 !1000 has 2565 digits
 !2000 has 5733 digits
 !3000 has 9128 digits
 !4000 has 12670 digits
 !5000 has 16322 digits
 !6000 has 20062 digits
 !7000 has 23875 digits
 !8000 has 27749 digits
 !9000 has 31678 digits
!10000 has 35656 digits
```



## Fortran

First, to see how far INTEGER*8 arithmetic can reach. This is the largest size likely available, even though the syntax could easily allow INTEGER*2400 or the like. The F90 MODULE protocol is used simply to avoid the tedium of declaring the type of the function FACT(n) in all routines invoking it, though at the cost of typing out the required blather. Otherwise, this would be acceptable to older Fortran compilers, except for the appending of name information on END statements.

The function names are used as ordinary variables within the function while building the result; earlier compilers often did not allow such usage or produced incorrect code in certain cases. Ordinary integer variables are used since it is obvious that their range will not be exercised before the function result overflows, even with sixty-four bit integers for them. With two's complement arithmetic, negative numbers can appear in spite of the mathematics involved only being able to generate positive numbers, but this relies on the "sign bit" happening to become set and cannot be regarded as a definite check. Only a proper test such as the IF OVERFLOW found in First Fortran (1958) will do, but the modernisers have long abandoned this detail.

Because this calculation won't get far, no attempt is made to save intermediate results (such as the factorial numbers) nor develop the results progressively even though they are to be produced in sequence. Each result is computed from the start, as per the specified formulae. 

For output, to have the exclamation mark precede the number without a gap, format sequence <code>"!",I0</code> will do, the <code>I0</code> format code being standardised in F90. However, this produces varying-length digit sequences, which will mean that the following output changes position likewise. Rather than use say <code>I20</code> for the result and have a wide gap, code <code>I0</code> will do, and to start each such number in the same place, the code <code>T6</code> will start it in column six, far enough along not to clash with the first number on the line, given that it will not be large. 
```Fortran
      MODULE LAIROTCAF	!Calculates "left factorials".
       CONTAINS		!The usual suspects.
        INTEGER*8 FUNCTION FACT(N)	!Factorial, the ordinary.
         INTEGER N	!The number won't ever get far.
         INTEGER I	!The stepper.
          FACT = 1	!Here we go.
          DO I = 2,N		!Does nothing for N < 2.
            FACT = FACT*I		!Perhaps this overflows.
            IF (FACT.LE.0) STOP "Factorial: Overflow!"	!Two's complement arithmetic.
          END DO		!No longer any IF OVERFLOW tests.
        END FUNCTION FACT	!Simple enough.

        INTEGER*8 FUNCTION LFACT(N)	!Left factorial.
         INTEGER N	!This number won't get far either.
         INTEGER K	!A stepper.
          LFACT = 0	!Here we go.
          DO K = 0,N - 1	!Apply the definition.
            LFACT = LFACT + FACT(K)	!Perhaps this overflows.
            IF (LFACT.LE.0) STOP "Lfact: Overflow!"	!Unreliable test.
          END DO		!On to the next step in the summation.
        END FUNCTION LFACT	!No attempts at saving effort.
      END MODULE LAIROTCAF	!Just the minimum.

      PROGRAM POKE
      USE LAIROTCAF
      INTEGER I

      WRITE (6,*) "Left factorials, from 0 to 10..."
      DO I = 0,10
        WRITE (6,1) I,LFACT(I)
    1   FORMAT ("!",I0,T6,I0)
      END DO

      WRITE (6,*) "Left factorials, from 20 to 110 by tens..."
      DO I = 20,110,10
        WRITE (6,1) I,LFACT(I)
      END DO
      END
```


Output:

```txt

 Left factorials, from 0 to 10...
!0   0
!1   1
!2   2
!3   4
!4   10
!5   34
!6   154
!7   874
!8   5914
!9   46234
!10  409114
 Left factorials, from 20 to 110 by tens...
!20  128425485935180314
Factorial: Overflow!

```


Obviously, one could proceed using the services of some collection of "bignum" routines, and then the code would merely depict their uses for this problem. Since the task is to produce consecutive values, all that need be done is to maintain a S value holding the accumulated sum, and a F value for the successive factorials to be added into S. The only difficulty is to arrange the proper phasing of the starting values so that the calculation will work. Since only one multiply and one addition is needed per step, explicit code might as well be used, as follows: 
```Fortran
Calculates "left factorials", in sequence, and shows some.
      INTEGER ENUFF,BASE	!Some parameters.
      PARAMETER (BASE = 10, ENUFF = 40000)	!This should do.
      INTEGER LF,F(ENUFF),LS,S(ENUFF)	!Big numbers in digits F(1:LF), S(1:LS)
      INTEGER N		!A stepper.
      INTEGER L		!Locates digits.
      INTEGER C		!A carry for arithmetic.
      INTEGER MSG	!I/O unit number.

      MSG = 6	!Standard output.
      LF = 1; F(1) = 1	!Set F = 1 = 0!
      LS = 1; S(1) = 0	!Set S = 0 = !0
      WRITE (MSG,1) 0,0	!Pre-emptive first result.
    1 FORMAT ("!",I0,T6,666I1)	!This will do for reasonable sizes.

   10 DO N = 1,10000	!Step away.
Commence the addition of F to S.
   20   C = 0		!Clear the carry.
        DO L = 1,MIN(LF,LS)	!First, both S and F have low-order digits.
          C = S(L) + F(L) + C		!So, a three-part addition.
          S(L) = MOD(C,BASE)		!Place the digit.
          C = C/BASE			!Carry to the next digit up.
        END DO			!Ends with L and C important.
Careful. L fingers the next digit up, and C is to carry in to that digit.
        IF (LF.GT.LS) THEN	!Has F more digits than S?
          DO L = L,LF		!Yes. Continue adding, with leading zero digits from S.
            C = F(L) + C		!Thus.
            LS = LS + 1			!Another digit for S.
            S(LS) = MOD(C,BASE)		!Place.
            C = C/BASE			!Carry to the next digit up.
          END DO		!Continue to the end of F.
        END IF			!Either way, F has been added in.
Continue carrying, with C for digit L.
        DO WHILE(C .GT. 0)	!Extend the carry into S.
          IF (L.LE.LS) THEN		!If F had fewer digits than S,
            C = C + S(L)		!S digits await.
           ELSE			!Otherwise,
            LS = LS + 1			!Extend S.
          END IF		!C is ready.
          S(L) = MOD(C,BASE)	!Place it.
          C = C/BASE		!The carry for the next digit up.
          L = L + 1		!Locate it.
        END DO			!Perhaps a multi-digit carry.
Contemplate what to do with the current S.
        IF (N.LE.10) THEN		!First selection: !N for 0 to 10.
          WRITE (MSG,1) N,S(LS:1:-1)		!Show the value. Digits from the high-order end down.
        ELSE IF (20.LE.N .AND. N.LE.110) THEN	!Second selection: for 20 to 110,
          IF (MOD(N,10).EQ.0) WRITE (MSG,1) N,S(LS:1:-1)	!Show only every tenth.
        ELSE					!Third selection
          IF (MOD(N,1000).EQ.0) WRITE (MSG,21) N,LS	!Show only the number of digits.
   21     FORMAT ("!",I0," has ",I0," digits.")		!Which is why BASE is only 10.
        END IF				!So much for the selection of output.
Calculate the next factorial, ready for the next one up.
        C = 0		!Start a multiply.
        DO L = 1,LF	!Step up the digits to produce N! in F.
          C = F(L)*N + C	!A digit.
          F(L) = MOD(C,BASE)	!Place.
          C = C/BASE		!Extract the carry.
        END DO		!On to the next digit.
        DO WHILE(C .GT. 0)	!While any carry remains,
          LF = LF + 1			!Add another digit to F.
          IF (LF.GT.ENUFF) STOP "F overflow!"	!Perhaps not.
          F(LF) = MOD(C,BASE)		!The digit.
          C = C/BASE			!Carry to the next digit up.
        END DO			!If there is one, as when N > BASE.
      END DO		!On to the next result.
      END	!Ends with a new factorial that won't be used.
```

Output: achieved in a few seconds. A larger BASE would give a faster calculation, but would complicate the digit count.

```txt

!0   0
!1   1
!2   2
!3   4
!4   10
!5   34
!6   154
!7   874
!8   5914
!9   46234
!10  409114
!20  128425485935180314
!30  9157958657951075573395300940314
!40  20935051082417771847631371547939998232420940314
!50  620960027832821612639424806694551108812720525606160920420940314
!60  141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70  173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80  906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90  16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits.
!2000 has 5733 digits.
!3000 has 9128 digits.
!4000 has 12670 digits.
!5000 has 16322 digits.
!6000 has 20062 digits.
!7000 has 23875 digits.
!8000 has 27749 digits.
!9000 has 31678 digits.
!10000 has 35656 digits.

```



## FreeBASIC

{{trans|C}}
{{libheader|GMP}}

```freebasic
' FB 1.05.0 Win64

#include "gmp.bi"

Sub leftFactorial(rop As __mpz_struct, op As ULong)
  Dim As __mpz_struct t1
  mpz_init_set_ui(@t1, 1)
  mpz_set_ui(@rop, 0)
  For i As ULong = 1 To op
    mpz_add(@rop, @rop, @t1)
    mpz_mul_ui(@t1, @t1, i)
  Next
  mpz_clear(@t1)
End Sub

Function digitCount(op As __mpz_struct) As ULong
  Dim As ZString Ptr t = mpz_get_str(0, 10, @op)
  Dim As ULong ret = Len(*t)
  Deallocate(t)
  Return ret
End Function

Dim As __mpz_struct t
mpz_init(@t)

For i As ULong = 0 To 110
  If i <= 10 OrElse i Mod 10 = 0 Then
    leftFactorial(t, i)
    gmp_printf(!"!%u = %Zd\n", i, @t)
  End If
Next

Print
 
For i As ULong = 1000 To 10000 Step 1000
  leftFactorial(t, i)
  Print "!"; Str(i); " has "; digitCount(t); " digits" 
Next

mpz_clear(@t)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

!1000 has 2565 digits
!2000 has 5733 digits
!3000 has 9128 digits
!4000 has 12670 digits
!5000 has 16322 digits
!6000 has 20062 digits
!7000 has 23875 digits
!8000 has 27749 digits
!9000 has 31678 digits
!10000 has 35656 digits

```



## Go


```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    fmt.Print("!0 through !10: 0")
    one := big.NewInt(1)
    n := big.NewInt(1)
    f := big.NewInt(1)
    l := big.NewInt(1)
    next := func() { f.Mul(f, n); l.Add(l, f); n.Add(n, one) }
    for ; ; next() {
        fmt.Print(" ", l)
        if n.Int64() == 10 {
            break
        }
    }
    fmt.Println()
    for {
        for i := 0; i < 10; i++ {
            next()
        }
        fmt.Printf("!%d: %d\n", n, l)
        if n.Int64() == 110 {
            break
        }
    }
    fmt.Println("Lengths of !1000 through !10000 by thousands:")
    for i := 110; i < 1000; i++ {
        next()
    }
    for {
        fmt.Print(" ", len(l.String()))
        if n.Int64() == 10000 {
            break
        }
        for i := 0; i < 1000; i++ {
            next()
        }
    }
    fmt.Println()
}
```

{{out}}

```txt

!0 through !10: 0 1 2 4 10 34 154 874 5914 46234 409114
!20: 128425485935180314
!30: 9157958657951075573395300940314
!40: 20935051082417771847631371547939998232420940314
!50: 620960027832821612639424806694551108812720525606160920420940314
!60: 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70: 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80: 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90: 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100: 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110: 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
Lengths of !1000 through !10000 by thousands:
 2565 5733 9128 12670 16322 20062 23875 27749 31678 35656

```



## Haskell


```haskell
fact :: [Integer]
fact = scanl (*) 1 [1 ..]

leftFact :: [Integer]
leftFact = scanl (+) 0 fact

main :: IO ()
main =
  mapM_
    putStrLn
    [ "0 ~ 10:"
    , show $ (leftFact !!) <$> [0 .. 10]
    , ""
    , "20 ~ 110 by tens:"
    , unlines $ show . (leftFact !!) <$> [20,30 .. 110]
    , ""
    , "length of 1,000 ~ 10,000 by thousands:"
    , show $ (length . show . (leftFact !!)) <$> [1000,2000 .. 10000]
    , ""
    ]
```

{{Out}}

```txt
0 ~ 10:
[0,1,2,4,10,34,154,874,5914,46234,409114]

20 ~ 110 by tens:
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314


length of 1,000 ~ 10,000 by thousands:
[2565,5733,9128,12670,16322,20062,23875,27749,31678,35656]
```


=={{header|Icon}} and {{header|Unicon}}==
{{trans|D}}
The following works in both languages:
<lang>procedure main()
    every writes(lfact(0 | !10)," ")
    write()
    write()
    every write(lfact(20 to 110 by 10))
    write()
    every writes(*lfact(1000 to 10000 by 1000)," ")
    write()
end

procedure lfact(n)
    r := 0
    f := 1
    every (i := !n, r +:= .f, f *:= .i)
    return r
end
```


{{out}}

```txt

->lfact
0 1 2 4 10 34 154 874 5914 46234 409114 

128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

2565 5733 9128 12670 16322 20062 23875 27749 31678 35656 
->

```



## J


This could be made more efficient (in terms of machine time), is there a practical application for this? The more efficient machine approach would require a more specialized interface or memory dedicated to caching.


```J
leftFact=: +/@:!@i."0
```


Task examples:


```J
   (,. leftFact) i.11
 0      0
 1      1
 2      2
 3      4
 4     10
 5     34
 6    154
 7    874
 8   5914
 9  46234
10 409114
   (,. leftFact) 10*2+i.10x
 20                                                                                                                                                                128425485935180314
 30                                                                                                                                                   9157958657951075573395300940314
 40                                                                                                                                   20935051082417771847631371547939998232420940314
 50                                                                                                                   620960027832821612639424806694551108812720525606160920420940314
 60                                                                                                 141074930726669571000530822087000522211656242116439949000980378746128920420940314
 70                                                                               173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
 80                                                             906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
 90                                         16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
100                      942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
110 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
   (,. #@":@leftFact) 1000*1+i.10x
 1000  2565
 2000  5733
 3000  9128
 4000 12670
 5000 16322
 6000 20062
 7000 23875
 8000 27749
 9000 31678
10000 35656
```



## Java


```java
import java.math.BigInteger;

public class LeftFac{
	public static BigInteger factorial(BigInteger n){
		BigInteger ans = BigInteger.ONE;
		for(BigInteger x = BigInteger.ONE; x.compareTo(n) <= 0; x = x.add(BigInteger.ONE)){
			ans = ans.multiply(x);
		}
		return ans;
	}
	
	public static BigInteger leftFact(BigInteger n){
		BigInteger ans = BigInteger.ZERO;
		for(BigInteger k = BigInteger.ZERO; k.compareTo(n.subtract(BigInteger.ONE)) <= 0; k = k.add(BigInteger.ONE)){
			ans = ans.add(factorial(k));
		}
		return ans;
	}
	
	public static void main(String[] args){
		for(int i = 0; i <= 10; i++){
			System.out.println("!" + i + " = " + leftFact(BigInteger.valueOf(i)));
		}
		
		for(int i = 20; i <= 110; i += 10){
			System.out.println("!" + i + " = " + leftFact(BigInteger.valueOf(i)));
		}
		
		for(int i = 1000; i <= 10000; i += 1000){
			System.out.println("!" + i + " has " + leftFact(BigInteger.valueOf(i)).toString().length() + " digits");
		}
	}
}
```

{{out}}

```txt
!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits
!2000 has 5733 digits
!3000 has 9128 digits
!4000 has 12670 digits
!5000 has 16322 digits
!6000 has 20062 digits
!7000 has 23875 digits
!8000 has 27749 digits
!9000 has 31678 digits
!10000 has 35656 digits
```



## jq

jq currently only has builtin support for IEEE 64-bit numbers, so in this section we will first present the algorithm using the builtin arithmetic
operators and then adapt it for use with the BigInt library at https://gist.github.com/pkoppstein/d06a123f30c033195841

'''Using builtin arithmetic''':

```jq
def left_factorial:
  reduce range(1; .+1) as $i
  # state: [i!, !i]
    ([1,0]; .[1] += .[0] | .[0] *= $i)
  | .[1];
```


'''Using BigInt.jq''':

The BigInt library can be used with jq 1.4, but we will take this opportunity to showcase jq 1.5's support for importing such libraries as modules.
If your jq does not have support for modules, add the BigInt.jq file, remove the 'import' statement and strip off the "BigInt::" prefix.

To compute the lengths of the decimal representation without having to recompute !n,
we also define left_factorial_lengths(gap) to emit [n, ( !n|length) ] when n % gap == 0.

```jq
import "BigInt" as BigInt;

# integer input
def long_left_factorial:
  reduce range(1; .+1) as $i
  # state: [i!, !i]
    ( ["1", "0"];
      .[1] = BigInt::long_add(.[0]; .[1])
    | .[0] = BigInt::long_multiply(.[0]; $i | tostring) )
  | .[1];

# input and gap should be integers
def long_left_factorial_lengths(gap):
  reduce range(1; .+1) as $i
  # state: [i!, !i, gap]
    (["1", "0", []];
    .[1] = BigInt::long_add(.[0]; .[1])
    | .[0] = BigInt::long_multiply(.[0]; $i|tostring)
    | (.[1] | tostring | length) as $lf
    | if $i % gap == 0 then .[2] += [[$i, $lf]] else . end)
  | .[2];
```


'''The specific tasks''':

```sh
((range(0;11), (range(2; 12) * 10)) |  "\(.): \(long_left_factorial)"),

(10000 | long_left_factorial_lengths(1000) | .[] | "\(.[0]): length is \(.[1])")
```


{{out}}
(scrollable)
<div style="overflow:scroll; height:200px;">

```sh
$ jq -r -n -L . -f Long_left_factorial.jq
0: 0
1: 1
2: 2
3: 4
4: 10
5: 34
6: 154
7: 874
8: 5914
9: 46234
10: 409114
20: 128425485935180314
30: 9157958657951075573395300940314
40: 20935051082417771847631371547939998232420940314
50: 620960027832821612639424806694551108812720525606160920420940314
60: 141074930726669571000530822087000522211656242116439949000980378746128920420940314
70: 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
80: 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
90: 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
100: 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
110: 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
1000: length is 2565
2000: length is 5733
3000: length is 9128
4000: length is 12670
5000: length is 16322
6000: length is 20062
7000: length is 23875
8000: length is 27749
9000: length is 31678
10000: length is 35656
```

</div>


## Julia

{{works with|Julia|0.6}}


```julia
leftfactorial(n::Integer) = n ≤ 0 ? zero(n) : sum(factorial, 0:n-1)

@show leftfactorial.(0:10)
@show ndigits.(leftfactorial.(big.(1000:1000:10_000)))
```


{{out}}

```txt
leftfactorial.(0:10) = [0, 1, 2, 4, 10, 34, 154, 874, 5914, 46234, 409114]
ndigits.(leftfactorial.(big.(1000:1000:10000))) = [2565, 5733, 9128, 12670, 16322, 20062, 23875, 27749, 31678, 35656]
```



## Kotlin


```scala
// version 1.0.6

import java.math.BigInteger

fun leftFactorial(n: Int): BigInteger {
    if (n == 0) return BigInteger.ZERO
    var fact = BigInteger.ONE
    var sum = fact
    for (i in 1 until n) {
        fact *= BigInteger.valueOf(i.toLong())
        sum += fact
    }        
    return sum
}
        
fun main(args: Array<String>) {
    for (i in 0..110) 
        if (i <= 10 || (i % 10) == 0) 
            println("!${i.toString().padEnd(3)} = ${leftFactorial(i)}")
    println("\nLength of the following left factorials:")
    for (i in 1000..10000 step 1000) 
        println("!${i.toString().padEnd(5)} has ${leftFactorial(i).toString().length} digits")
}
```


{{out}}

```txt

!0   = 0
!1   = 1
!2   = 2
!3   = 4
!4   = 10
!5   = 34
!6   = 154
!7   = 874
!8   = 5914
!9   = 46234
!10  = 409114
!20  = 128425485935180314
!30  = 9157958657951075573395300940314
!40  = 20935051082417771847631371547939998232420940314
!50  = 620960027832821612639424806694551108812720525606160920420940314
!60  = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70  = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80  = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90  = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

Length of the following left factorials:
!1000  has 2565 digits
!2000  has 5733 digits
!3000  has 9128 digits
!4000  has 12670 digits
!5000  has 16322 digits
!6000  has 20062 digits
!7000  has 23875 digits
!8000  has 27749 digits
!9000  has 31678 digits
!10000 has 35656 digits

```



## Lua

Takes about five seconds...

```Lua
-- Lua bindings for GNU bc
require("bc")

-- Return table of factorials from 0 to n
function facsUpTo (n)
    local f, fList = bc.number(1), {}
    fList[0] = 1
    for i = 1, n do
        f = bc.mul(f, i)
        fList[i] = f
    end
    return fList
end

-- Return left factorial of n
function leftFac (n)
    local sum = bc.number(0)
    for k = 0, n - 1 do sum = bc.add(sum, facList[k]) end
    return bc.tostring(sum)
end

-- Main procedure
facList = facsUpTo(10000)
for i = 0, 10 do print("!" .. i .. " = " .. leftFac(i)) end
for i = 20, 110, 10 do print("!" .. i .. " = " .. leftFac(i)) end
for i = 1000, 10000, 1000 do
    print("!" .. i .. " contains " .. #leftFac(i) .. " digits")
end
```

{{out}}

```txt
!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 contains 2565 digits
!2000 contains 5733 digits
!3000 contains 9128 digits
!4000 contains 12670 digits
!5000 contains 16322 digits
!6000 contains 20062 digits
!7000 contains 23875 digits
!8000 contains 27749 digits
!9000 contains 31678 digits
!10000 contains 35656 digits
```



## Maple


```Maple
left_factorial := n -> sum(k!, k = 1 .. n - 1);
seq(left_factorial(i), i = 1 .. 10);
seq(left_factorial(i), i = 20 .. 110, 10);
seq(length(left_factorial(i)), i = 1000 .. 10000, 1000);
```

{{out}}

```txt
0, 1, 3, 9, 33, 153, 873, 5913, 46233, 409113

128425485935180313, 9157958657951075573395300940313, 20935051082417771847631371547939998232420940313, 620960027832821612639424806694551108812720525606160920420940313, 141074930726669571000530822087000522211656242116439949000980378746128920420940313, 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940313, 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940313, 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940313, 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940313, 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940313

2565, 5733, 9128, 12670, 16322, 20062, 23875, 27749, 31678, 35656
```



## Mathematica


```Mathematica
left[n_] := left[n] = Sum[k!, {k, 0, n - 1}]
Print["left factorials 0 through 10:"]
Print[left /@ Range[0, 10] // TableForm]
Print["left factorials 20 through 110, by tens:"]
Print[left /@ Range[20, 110, 10] // TableForm]
Print["Digits in left factorials 1,000 through 10,000, by thousands:"]
Print[Length[IntegerDigits[left[#]]] & /@ Range[1000, 10000, 1000] // TableForm]
```

{{out}}

```txt
left factorials 0 through 10:
0
1
2
4
10
34
154
874
5914
46234
409114

left factorials 20 through 110, by tens:
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

Digits in left factorials 1,000 through 10,000, by thousands:
2565
5733
9128
12670
16322
20062
23875
27749
31678
35656

```



## Nim

{{trans|Python}}

```nim
import iterutils, bigints

proc lfact: iterator: BigInt =
  result = iterator: BigInt =
    yield 0.initBigInt
    var
      fact = 1.initBigInt
      sum = 0.initBigInt
      n = 1.initBigInt
    while true:
      sum += fact
      fact *= n
      n += 1
      yield sum

echo "first 11:\n  "
for i in lfact().slice(last = 10):
  echo "  ", i

echo "20 through 110 (inclusive) by tens:"
for i in lfact().slice(20, 110, 10):
  echo "  ", i

echo "Digits in 1,000 through 10,000 (inclusive) by thousands:"
for i in lfact().slice(1_000, 10_000, 1_000):
  echo "  ", ($i).len
```

{{out}}

```txt
first 11:
  0
  1
  2
  4
  10
  34
  154
  874
  5914
  46234
  409114
20 through 110 (inclusive) by tens:
  128425485935180314
  9157958657951075573395300940314
  20935051082417771847631371547939998232420940314
  620960027832821612639424806694551108812720525606160920420940314
  141074930726669571000530822087000522211656242116439949000980378746128920420940314
  173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
  906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
  16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
  942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
  145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
Digits in 1,000 through 10,000 (inclusive) by thousands:
  2565
  5733
  9128
  12670
  16322
  20062
  23875
  27749
  31678
  35656
```



## Oforth



```Oforth
: leftFact  | i | 0 1 rot loop: i [ tuck + swap i * ] drop ;
```


{{out}}

```txt

>seqFrom(0, 10) map(#leftFact) println
[0, 1, 2, 4, 10, 34, 154, 874, 5914, 46234, 409114]

```



```txt

>seqFrom(2, 11) apply(#[ 10 * leftFact println ])
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

```



```txt

>seq(10) map(#[ 1000 * leftFact asString size ]) println
[2565, 5733, 9128, 12670, 16322, 20062, 23875, 27749, 31678, 35656]

```



## PARI/GP


```parigp
lf(n)=sum(k=0,n-1,k!);
apply(lf, [0..10])
apply(lf, 10*[2..11])
forstep(n=1000,1e4,1000,print1(#digits(lf(n))", "))
```

{{out}}

```txt
%1 = [0, 1, 2, 4, 10, 34, 154, 874, 5914, 46234, 409114]
%2 = [128425485935180314, 9157958657951075573395300940314, 20935051082417771847631371547939998232420940314, 620960027832821612639424806694551108812720525606160920420940314, 141074930726669571000530822087000522211656242116439949000980378746128920420940314, 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314, 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314, 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314, 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314, 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314]
2565, 5733, 9128, 12670, 16322, 20062, 23875, 27749, 31678, 35656, 
```



## Perl

By caching the last used factorial and left factorial values, I avoid needless recomputation.  By only retaining the most recently used values, instead of all past values, I avoid the need to store twenty thousand enormous numbers.

If performance is a concern, this will run over 100x faster by replacing the line "use bigint" with "use Math::GMP qw/:constant/" (after installing that module).


```perl
#!perl
use 5.010;
use strict;
use warnings;
use bigint;

sub leftfact {
	my ($n) = @_;
	state $cached = 0;
	state $factorial = 1;
	state $leftfact = 0;
	if( $n < $cached ) {
		($cached, $factorial, $leftfact) = (0, 1, 0);
	}
	while( $n > $cached ) {
		$leftfact += $factorial;
		$factorial *= ++$cached;
	}
	return $leftfact;
}

printf "!%d = %s\n", $_, leftfact($_) for 0 .. 10, map $_*10, 2..11;
printf "!%d has %d digits.\n", $_, length leftfact($_) for map $_*1000, 1..10;


```


Since I copied the printf format strings from the perl6 implementation, 
the output from the code above is identical to the output of the perl6 code.


## Perl 6


Implement left factorial as a prefix !. Note that this redefines the core prefix ! (not) function.


```perl6
sub prefix:<!> ($k) { (constant l = 0, |[\+] 1, (|[\*] 1..*))[$k] }

$ = !10000; # Pre-initialize

.say for ( 0 … 10, 20 … 110 ).hyper(:4batch).map: { sprintf "!%d  = %s", $_, !$_ };
.say for (1000, 2000 … 10000).hyper(:4batch).map: { sprintf "!%d has %d digits.", $_, chars !$_ };
```

{{out}}

```txt
!0  = 0
!1  = 1
!2  = 2
!3  = 4
!4  = 10
!5  = 34
!6  = 154
!7  = 874
!8  = 5914
!9  = 46234
!10  = 409114
!20  = 128425485935180314
!30  = 9157958657951075573395300940314
!40  = 20935051082417771847631371547939998232420940314
!50  = 620960027832821612639424806694551108812720525606160920420940314
!60  = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70  = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80  = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90  = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100  = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110  = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits.
!2000 has 5733 digits.
!3000 has 9128 digits.
!4000 has 12670 digits.
!5000 has 16322 digits.
!6000 has 20062 digits.
!7000 has 23875 digits.
!8000 has 27749 digits.
!9000 has 31678 digits.
!10000 has 35656 digits.
```

If you would rather not override prefix ! operator and you can live with just defining lazy lists and indexing into them, this should suffice; (and is in fact very slightly faster than the first example since it avoids routine dispatch overhead):

```perl6
constant leftfact = 0, |[\+] 1, (|[\*] 1..*);

$ = leftfact[10000]; # Pre-initialize

.say for ( 0 … 10, 20 … 110 ).hyper(:4batch).map: { sprintf "!%d  = %s", $_, leftfact[$_] };
.say for (1000, 2000 … 10000).hyper(:4batch).map: { sprintf "!%d has %d digits.", $_, chars leftfact[$_] };
```


Same output.


## Phix

{{trans|Lua}}
{{libheader|mpfr}}
(now over 1500 times faster than the previous bigatom version.)

```Phix
include mpfr.e
 
sequence lf_list
 
procedure init(integer n)
    mpz f = mpz_init(1)
    lf_list = repeat(f,n+1)
    for i=1 to n do
        f = mpz_init_set(f)
        mpz_mul_si(f,f,i)
        lf_list[i+1] = f
    end for
end procedure
 
function lf(integer n, bool len=false)
-- Returns left factorial of n, or it's length, as a string
    mpz sumf = mpz_init(0)
    for k=1 to n do mpz_add(sumf,sumf,lf_list[k]) end for
    return iff(len?sprintf("%d",mpz_sizeinbase(sumf,10))
                  :mpz_get_str(sumf))
end function
 
-- Main procedure
atom t0 = time()
init(10000)
for i=0 to 10 do printf(1,"!%d = %s\n",{i,lf(i)}) end for
for i=20 to 110 by 10 do printf(1,"!%d = %s\n",{i,lf(i)}) end for
for i=1000 to 10000 by 1000 do printf(1,"!%d contains %s digits\n",{i,lf(i,true)}) end for
printf(1,"complete (%3.2fs)\n",{time()-t0})
```

{{out}}

```txt

!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 contains 2565 digits
!2000 contains 5733 digits
!3000 contains 9128 digits
!4000 contains 12670 digits
!5000 contains 16323 digits
!6000 contains 20062 digits
!7000 contains 23875 digits
!8000 contains 27749 digits
!9000 contains 31678 digits
!10000 contains 35656 digits
complete (0.25s)

```



## PicoLisp


```PicoLisp
(de n! (N)
       (cache '(NIL) N
          (if (> 2 N) 1
	      (* N (n! (dec N))))))

(de !n (Num)
  (if (= Num 0) 1
      (sum n! (range 0 (dec Num)))))

(de pril (List) (mapcar 'println List))

(prinl "0-10")
(pril (mapcar '!n (range 0 10)))
(prinl "20 - 110")
(pril (mapcar '!n (range 20 110 10)))
(prinl "length of 1000 - 10000")
(pril (mapcar 'length (mapcar '!n (range 1000 10000 1000))))

```

{{out}}
<lang>0-10 
1 
1
2
4
10
34
154
874
5914
46234
409114
20 - 110
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
1000 - 10000
2565
5733
9128
12670
16322
20062
23875
27749
31678
35656

```



## PL/I

In PL/I the biggest integer type is <tt>'''fixed decimal(31)'''</tt> i.e. 31 digits. 
To the best of my knowledge, no big integers exist.
Results are shown for the first 11 integers, as required;
then for the integers from 20 through 30 only, because factorials for n = 40 and larger are not possible.

```pli
lf: procedure (n) returns (fixed decimal (31) );
   declare n fixed binary;
   declare (s, f) fixed (31);
   declare (i, j) fixed;

   s = 0;
   do i = n-1 to 0 by -1;
      f = 1;
      do j = i to 1 by -1;
         f = f * j;
      end;
      s = s + f;
   end;
   return (s);
end lf;

   declare n fixed binary;

   do n = 0 to 10, 20 to 30;
      put skip list ('Left factorial of ' || n || '=' || lf(n) );
   end;

end left_factorials;
```

{{out}}

```txt

Left factorial of         0=                                 0 
Left factorial of         1=                                 1 
Left factorial of         2=                                 2 
Left factorial of         3=                                 4 
Left factorial of         4=                                10 
Left factorial of         5=                                34 
Left factorial of         6=                               154 
Left factorial of         7=                               874 
Left factorial of         8=                              5914 
Left factorial of         9=                             46234 
Left factorial of        10=                            409114 
Left factorial of        20=                128425485935180314 
Left factorial of        21=               2561327494111820314 
Left factorial of        22=              53652269665821260314 
Left factorial of        23=            1177652997443428940314 
Left factorial of        24=           27029669736328405580314 
Left factorial of        25=          647478071469567844940314 
Left factorial of        26=        16158688114800553828940314 
Left factorial of        27=       419450149241406189412940314 
Left factorial of        28=     11308319599659758350180940314 
Left factorial of        29=    316196664211373618851684940314 
Left factorial of        30=   9157958657951075573395300940314 

```



## PowerShell

{{works with|PowerShell|4.0}}

```PowerShell

function left-factorial ([BigInt]$n) {
    [BigInt]$k, [BigInt]$fact = ([BigInt]::Zero), ([BigInt]::One)
    [BigInt]$lfact = ([BigInt]::Zero)
    while($k -lt $n){        
        if($k -gt ([BigInt]::Zero)) {
            $fact = [BigInt]::Multiply($fact, $k)
            $lfact = [BigInt]::Add($lfact, $fact)
        } else {
            $lfact = ([BigInt]::One)
        }
        $k = [BigInt]::Add($k, [BigInt]::One)
    }
    $lfact
}
0..9 | foreach{
    "!$_ = $(left-factorial $_)"
}
for($i = 10; $i -le 110; $i += 10) {
    "!$i = $(left-factorial $i)"
}
for($i = 1000; $i -le 10000; $i += 1000) {
    $digits = [BigInt]::Log10($(left-factorial $i)) 
    $digits = [Math]::Floor($digits) + 1
    if($digits -gt 1) {"!$i has $digits digits"}
    else {"!$i has $digits digit"}
}

```

<b>Output:</b>

```txt

!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 9060895879876953465345168046502906376940248300119563651843276746197520942896963148820085319918409223365289204
20940314
!90 = 1669557007262421076703416768839462336073351516357586413634591033592403996240486951022572307223584266878750799
3136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203
520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337
422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits
!2000 has 5733 digits
!3000 has 9128 digits
!4000 has 12670 digits
!5000 has 16322 digits
!6000 has 20062 digits
!7000 has 23875 digits
!8000 has 27749 digits
!9000 has 31678 digits
!10000 has 35656 digits 

```



## Python


```python
from itertools import islice

def lfact():
    yield 0
    fact, summ, n = 1, 0, 1 
    while 1:
        fact, summ, n = fact*n, summ + fact, n + 1
        yield summ

print('first 11:\n  %r' % [lf for i, lf in zip(range(11), lfact())])
print('20 through 110 (inclusive) by tens:')
for lf in islice(lfact(), 20, 111, 10):
    print(lf)
print('Digits in 1,000 through 10,000 (inclusive) by thousands:\n  %r' 
      % [len(str(lf)) for lf in islice(lfact(), 1000, 10001, 1000)] )
```


{{out}}

```txt
first 11:
  [0, 1, 2, 4, 10, 34, 154, 874, 5914, 46234, 409114]
20 through 110 (inclusive) by tens:
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
Digits in 1,000 through 10,000 (inclusive) by thousands:
  [2565, 5733, 9128, 12670, 16322, 20062, 23875, 27749, 31678, 35656]
```



Or, sidestepping the use of '''while''' and '''yield''', we can directly define left factorials in terms of the '''scanl''' abstraction
(a fold or catamorphism, like functools.reduce, but one which returns the whole accumulation of intermediate values – see, for example, The Algebra of Programming, Bird and de Moor, 1997).

'''scanl''' in turn, has a natural definition in terms of the itertools functions '''accumulate''' and '''chain'''.

{{Trans|Haskell}}

```python
"""Left factorials"""

from itertools import (accumulate, chain, count, islice)
from operator import (mul, add)


# leftFact :: [Integer]
def leftFact():
    '''Left factorial series defined in terms of the factorial series'''
    return scanl(add)(0)(
        fact()
    )


# fact :: [Integer]
def fact():
    '''Factorial series – a non-finite list'''
    return scanl(mul)(1)(
        enumFrom(1)
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tests'''
    print(
        'Terms 0 thru 10 inclusive:\n  %r'
        % take(11)(leftFact())
    )

    print('\nTerms 20 thru 110 (inclusive) by tens:')
    for x in takeFromThenTo(20)(30)(110)(leftFact()):
        print(x)

    print(
        '\n\nDigit counts for terms 1k through 10k (inclusive) by k:\n  %r'
        % list(map(
            compose(len)(str),
            takeFromThenTo(1000)(2000)(10000)(
                leftFact()
            )
        ))
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Function composition.'''
    return lambda f: lambda x: g(f(x))


# enumFrom :: Enum a => a -> [a]
def enumFrom(x):
    '''A non-finite stream of enumerable values,
       starting from the given value.'''
    return count(x) if isinstance(x, int) else (
        map(chr, count(ord(x)))
    )


# scanl :: (b -> a -> b) -> b -> [a] -> [b]
def scanl(f):
    '''scanl is like reduce, but returns a succession of
       intermediate values, building from the left.'''
    return lambda a: lambda xs: (
        accumulate(chain([a], xs), f)
    )


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# takeFromThenTo :: Int -> Int -> Int -> [a] -> [a]
def takeFromThenTo(a):
    '''Values drawn from a series betweens positions a and b
       at intervals of size z'''
    return lambda b: lambda z: lambda xs: islice(
        xs, a, 1 + z, b - a
    )


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Terms 0 thru 10 inclusive:
  [0, 1, 2, 4, 10, 34, 154, 874, 5914, 46234, 409114]

Terms 20 thru 110 (inclusive) by tens:
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

Digit counts for terms 1k through 10k (inclusive) by k:
  [2565, 5733, 9128, 12670, 16322, 20062, 23875, 27749, 31678, 35656]
```



## Racket



```racket
#lang racket
(define ! (let ((rv# (make-hash))) (λ (n) (hash-ref! rv# n (λ () (if (= n 0) 1 (* n (! (- n 1)))))))))

(define (!n n)
  ;; note that in-range n is from 0 to n-1 inclusive
  (for/sum ((k (in-range n))) (! k)))

(define (dnl. s) (for-each displayln s))
(dnl
  "Display the left factorials for:"
  "zero through ten (inclusive)"
  (pretty-format (for/list ((i (in-range 0 (add1 10)))) (!n i)))
  "20 through 110 (inclusive) by tens"
  (pretty-format (for/list ((i (in-range 20 (add1 110) 10))) (!n i)))
  "Display the length (in decimal digits) of the left factorials for:"
  "1,000, 2,000 through 10,000 (inclusive), by thousands."
  (pretty-format (for/list ((i (in-range 1000 10001 1000))) (add1 (order-of-magnitude (!n i))))))
```


{{out}}

```txt
Display the left factorials for:
zero through ten (inclusive)
'(0 1 2 4 10 34 154 874 5914 46234 409114)
20 through 110 (inclusive) by tens
'(128425485935180314
  9157958657951075573395300940314
  20935051082417771847631371547939998232420940314
  620960027832821612639424806694551108812720525606160920420940314
  141074930726669571000530822087000522211656242116439949000980378746128920420940314
  173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
  906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
  16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
  942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
  145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314)
Display the length (in decimal digits) of the left factorials for:
1,000, 2,000 through 10,000 (inclusive), by thousands.
'(2565 5733 9128 12670 16322 20062 23875 27749 31678 35656)
```



## REXX


```rexx
/*REXX program  computes/display the  left factorial  (or its width) of  N  (or range). */
parse arg bot top inc .                          /*obtain optional argumenst from the CL*/
if bot=='' | bot==","  then bot=  1              /*Not specified:  Then use the default.*/
if top=='' | top==","  then top=bot              /* "      "         "   "   "     "    */
if inc=''  | inc==","  then inc=  1              /* "      "         "   "   "     "    */
tellDigs= (bot<0)                                /*if BOT < 0,   only show # of digits. */
bot=abs(bot)                                     /*use the  │bot│  for the   DO   loop. */
@= 'left ! of '                                  /*a handy literal used in the display. */
w=length(H)                                      /*width of the largest number request. */
               do j=bot  to top  by inc          /*traipse through the numbers requested*/
               if tellDigs  then say @ right(j,w)   " ───► "   length(L!(j))     ' digits'
                            else say @ right(j,w)   " ───► "          L!(j)
               end   /*j*/                       /* [↑]  show either  L!  or # of digits*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
L!: procedure; parse arg x .;  if x<3  then return x;  s=4        /*some shortcuts.     */
!=2;     do f=3  to x-1                          /*compute  L!  for all numbers ─── ► X.*/
         !=!*f                                   /*compute intermediate factorial.      */
         if pos(.,!)\==0 then numeric digits digits()*1.5%1       /*bump decimal digits.*/
         s=s+!                                   /*add the factorial ───►  L!  sum.     */
         end   /*f*/                             /* [↑]  handles gihugeic numbers.      */
return s                                         /*return the sum  (L!)  to the invoker.*/
```

'''output'''   when using the input:   <tt> 0   10 </tt>

```txt

left ! of   0  ───►  0
left ! of   1  ───►  1
left ! of   2  ───►  2
left ! of   3  ───►  4
left ! of   4  ───►  10
left ! of   5  ───►  34
left ! of   6  ───►  154
left ! of   7  ───►  874
left ! of   8  ───►  5914
left ! of   9  ───►  46234
left ! of  10  ───►  409114

```

'''output'''   when using the input:   <tt> 20   110   10 </tt>

```txt

left ! of   20  ───►  128425485935180314
left ! of   30  ───►  9157958657951075573395300940314
left ! of   40  ───►  20935051082417771847631371547939998232420940314
left ! of   50  ───►  620960027832821612639424806694551108812720525606160920420940314
left ! of   60  ───►  141074930726669571000530822087000522211656242116439949000980378746128920420940314
left ! of   70  ───►  173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
left ! of   80  ───►  906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
left ! of   90  ───►  16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
left ! of  100  ───►  942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
left ! of  110  ───►  145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

```

'''output'''   when using the input:   <tt> -1000   10000   1000 </tt>

```txt

left ! of   1000  ───►  2565  digits
left ! of   2000  ───►  5733  digits
left ! of   3000  ───►  9128  digits
left ! of   4000  ───►  12670  digits
left ! of   5000  ───►  16322  digits
left ! of   6000  ───►  20062  digits
left ! of   7000  ───►  23875  digits
left ! of   8000  ───►  27749  digits
left ! of   9000  ───►  31678  digits
left ! of  10000  ───►  35656  digits

```



## Ring


```ring

a = leftFact(0,10,1)
see "" + a + nl
 
func leftFact f,t,s
     see "------ From " + f + " --To -> " + t +" Step " + s + " -------" + nl
     for i = f to t step s
         leftFact = 1
         fct = 1
         for j = 1 to i - 1
             fct = fct * j
             leftFact = leftFact + fct
         next
         if i >= 1000 see "" + i + " " + len(string(leftFact)) + " digits" + nl
         else see "" + i + " " + leftFact + nl ok
     next

```



## Ruby


```ruby
left_fact = Enumerator.new do |y|
  f, lf = 1, 0
  1.step do |n|
    y  << lf #yield left_factorial
    lf += f
    f  *= n
  end
end
```

'''Test:'''

```ruby
tens = 20.step(110, 10)
thousands = 1000.step(10_000, 1000)

10001.times do |n|
  lf = left_fact.next
  case n
  when 0..10, *tens
    puts "!#{n} = #{lf}"
  when *thousands
    puts "!#{n} has #{lf.to_s.size} digits"
  end
end
```

{{out}}

```txt
!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits
!2000 has 5733 digits
!3000 has 9128 digits
!4000 has 12670 digits
!5000 has 16322 digits
!6000 has 20062 digits
!7000 has 23875 digits
!8000 has 27749 digits
!9000 has 31678 digits
!10000 has 35656 digits

```



## Run BASIC


```Runbasic
a = lftFct(0,10,1)
a = lftFct(20,110,10)
a = lftFct(1000,10000,1000)

function lftFct(f,t,s)
print :print "------ From ";f;" --To-> ";t;" Step ";s;" -------"
for i = f to t step s
	lftFct	= 1
	fct	= 1
	for j = 1 to i-1
		fct	= fct * j
		lftFct	= lftFct + fct
	next j
	if i >= 1000 then
		print i;" ";len(str$(lftFct));" "digits"
	  else 
		print i;" ";lftFct
	end if
next i
end function
```
Output:

```txt
------ From 0 --To-> 10 Step 1 -------
0 1
1 1
2 2
3 4
4 10
5 34
6 154
7 874
8 5914
9 46234
10 409114

------ From 20 --To-> 110 Step 10 -------
20 128425485935180314
30 9157958657951075573395300940314
40 20935051082417771847631371547939998232420940314
50 620960027832821612639424806694551108812720525606160920420940314
60 141074930726669571000530822087000522211656242116439949000980378746128920420940314
70 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
80 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
90 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
100 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
110 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

------ From 1000 --To-> 10000 Step 1000 -------
1000 2565 digits
2000 5733 digits
3000 9128 digits
4000 12670 digits
5000 16322 digits
6000 20062 digits
7000 23875 digits
8000 27749 digits
9000 31678 digits
10000 35656 digits
```



## Rust


```Rust

#[cfg(target_pointer_width = "64")]
type USingle = u32;
#[cfg(target_pointer_width = "64")]
type UDouble = u64;
#[cfg(target_pointer_width = "64")]
const WORD_LEN: i32 = 32;

#[cfg(not(target_pointer_width = "64"))]
type USingle = u16;
#[cfg(not(target_pointer_width = "64"))]
type UDouble = u32;
#[cfg(not(target_pointer_width = "64"))]
const WORD_LEN: i32 = 16;

use std::cmp;

#[derive(Debug,Clone)]
struct BigNum {
    // rep_.size() == 0 if and only if the value is zero.
    // Otherwise, the word rep_[0] keeps the least significant bits.
    rep_: Vec<USingle>,
}

impl BigNum {
    pub fn new(n: USingle) -> BigNum {
        let mut result = BigNum { rep_: vec![] };
        if n > 0 { result.rep_.push(n); }
        result
    }
    pub fn equals(&self, n: USingle) -> bool {
        if n == 0 { return self.rep_.is_empty() }
        if self.rep_.len() > 1 { return false }
        self.rep_[0] == n
    }
    pub fn add_big(&self, addend: &BigNum) -> BigNum {
        let mut result = BigNum::new(0);
        let mut sum = 0 as UDouble;
        let sz1 = self.rep_.len();
        let sz2 = addend.rep_.len();
        for i in 0..cmp::max(sz1, sz2) {
            if i < sz1 { sum += self.rep_[i] as UDouble }
            if i < sz2 { sum += addend.rep_[i] as UDouble }
            result.rep_.push(sum as USingle);
            sum >>= WORD_LEN;
        }
        if sum > 0 { result.rep_.push(sum as USingle) }
        result
    }
    pub fn multiply(&self, factor: USingle) -> BigNum {
        let mut result = BigNum::new(0);
        let mut product = 0 as UDouble;
        for i in 0..self.rep_.len() {
            product += self.rep_[i] as UDouble * factor as UDouble;
            result.rep_.push(product as USingle);
            product >>= WORD_LEN;
        }
        if product > 0 {
            result.rep_.push(product as USingle);
        }
        result
    }
    pub fn divide(&self, divisor: USingle, quotient: &mut BigNum,
        remainder: &mut USingle) {
        quotient.rep_.truncate(0);
        let mut dividend: UDouble;
        *remainder = 0;
        for i in 0..self.rep_.len() {
            let j = self.rep_.len() - 1 - i;
            dividend = ((*remainder as UDouble) << WORD_LEN)
                + self.rep_[j] as UDouble;
            let quo = (dividend / divisor as UDouble) as USingle;
            *remainder = (dividend % divisor as UDouble) as USingle;
            if quo > 0 || j < self.rep_.len() - 1 {
                quotient.rep_.push(quo);
            }
        }
        quotient.rep_.reverse();
    }
    fn to_string(&self) -> String {
        let mut rep = String::new();
        let mut dividend = (*self).clone();
        let mut remainder = 0 as USingle;
        let mut quotient = BigNum::new(0);
        loop {
            dividend.divide(10, &mut quotient, &mut remainder);
            rep.push(('0' as USingle + remainder) as u8 as char);
            if quotient.equals(0) { break; }
            dividend = quotient.clone();
        }
        rep.chars().rev().collect::<String>()
    }
}

use std::fmt;
impl fmt::Display for BigNum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

fn lfact(n: USingle) -> BigNum {
    let mut result = BigNum::new(0);
    let mut f = BigNum::new(1);
    for k in 1 as USingle..n + 1 {
        result = result.add_big(&f);
        f = f.multiply(k);
    }
    result
}

fn main() {
    for i in 0..11 {
        println!("!{} = {}", i, lfact(i));
    }
    for i in 2..12 {
        let j = i * 10;
        println!("!{} = {}", j, lfact(j));
    }
    for i in 1..11 {
        let j = i * 1000;
        println!("!{} has {} digits.", j, lfact(j).to_string().len());
    }
}

```

{{out}}

```txt

!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits.
!2000 has 5733 digits.
!3000 has 9128 digits.
!4000 has 12670 digits.
!5000 has 16322 digits.
!6000 has 20062 digits.
!7000 has 23875 digits.
!8000 has 27749 digits.
!9000 has 31678 digits.
!10000 has 35656 digits.

```



## Scala


```scala
object LeftFactorial extends App {

  // this part isn't really necessary, it just shows off Scala's ability
  // to match the mathematical syntax: !n
  implicit class RichInt(n:Int) {
    def unary_!() = factorial.take(n).sum
  }

  val factorial: Stream[BigInt] = 1 #:: factorial.zip(Stream.from(1)).map(n => n._2 * factorial(n._2 - 1))

  for (n <- (0 to 10) ++
            (20 to 110 by 10);
       value = !n) {
    println(s"!${n} = ${value}")
  }
  for (n <- 1000 to 10000 by 1000;
       length = (!n).toString.length) {
    println(s"length !${n} = ${length}")
  }
}

```

{{out}}

```txt

!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
length !1000 = 2565
length !2000 = 5733
length !3000 = 9128
length !4000 = 12670
length !5000 = 16322
length !6000 = 20062
length !7000 = 23875
length !8000 = 27749
length !9000 = 31678
length !10000 = 35656

```



## Scheme


This version uses the iota method in the standard lists library.
iota takes three values, a count, an optional start value (defaults to 0), and an optional step value (defaults to 1)
so (iota 5) produces a list (0 1 2 3 4)
and (iota 5 100 2) produces a list (100 102 104 106 108)


```scheme

(import (scheme base)     ;; library imports in R7RS style
        (scheme write)
        (srfi 1 lists))

(define (factorial n)
  (fold * 1 (iota n 1)))

(define (left-factorial n)
  (fold + 0 (map factorial (iota n))))

(define (show i r) ; to pretty print the results
  (display "!") (display i) (display " ") (display r) (newline))

;; show left factorials for zero through ten (inclusive)
(for-each 
  (lambda (i) (show i (left-factorial i)))
  (iota 11))

;; show left factorials for 20 through 110 (inclusive) by tens
(for-each 
  (lambda (i) (show i (left-factorial i)))
  (iota 10 20 10))

;; number of digits in 1000 through 10000 by thousands:
(for-each 
  (lambda (i) (show i (string-length (number->string (left-factorial i)))))
  (iota 10 1000 1000))

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const func bigInteger: leftFact (in integer: n) is func
  result
    var bigInteger: leftFact is 0_;
  local
    var bigInteger: factorial is 1_;
    var integer: i is 0;
  begin
    for i range 1 to n do
      leftFact +:= factorial;
      factorial *:= bigInteger conv i;
    end for;
  end func;
 
const proc: main is func
  local
    var integer: n is 0;
  begin
    writeln("First 11 left factorials:");
    for n range 0 to 10 do
      write(" " <& leftFact(n));
    end for;
    writeln;
    writeln("20 through 110 (inclusive) by tens:");
    for n range 20 to 110 step 10 do
      writeln(leftFact(n));
    end for;
    writeln;
    writeln("Digits in 1,000 through 10,000 by thousands:");
    for n range 1000 to 10000 step 1000 do
      writeln(length(str(leftFact(n))));
    end for;
    writeln;
  end func;
```


{{out}}

```txt

First 11 left factorials:
 0 1 2 4 10 34 154 874 5914 46234 409114
20 through 110 (inclusive) by tens:
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

Digits in 1,000 through 10,000 by thousands:
2565
5733
9128
12670
16322
20062
23875
27749
31678
35656

```



## Sidef

Straightforward:

```ruby
func left_fact(k) {
    ^k -> map {|n| n! } -> sum
}
```


Memory efficient with ''Range.reduce()'':

```ruby
func left_fact(k) {
    ^k -> reduce({ |a,b| a + b! }, 0)
}
```


A much faster approach:

```ruby
func left_fact(n) {
    static cached    = 0
    static factorial = 1
    static leftfact  = 0
 
    if (n < cached) {
        cached    = 0
        factorial = 1
        leftfact  = 0
    }
 
    while (n > cached) {
        leftfact  += factorial
        factorial *= ++cached
    }
 
    leftfact
}
```


Completing the task:

```ruby
for i (0..10, 20..110 `by` 10) {
    printf("!%d  = %s\n", i, left_fact(i))
}

for i (1000..10000 `by` 1000) {
    printf("!%d has %d digits.\n", i, left_fact(i).len)
}
```


{{out}}

```txt
!0  = 0
!1  = 1
!2  = 2
!3  = 4
!4  = 10
!5  = 34
!6  = 154
!7  = 874
!8  = 5914
!9  = 46234
!10  = 409114
!20  = 128425485935180314
!30  = 9157958657951075573395300940314
!40  = 20935051082417771847631371547939998232420940314
!50  = 620960027832821612639424806694551108812720525606160920420940314
!60  = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70  = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80  = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90  = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100  = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110  = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits.
!2000 has 5733 digits.
!3000 has 9128 digits.
!4000 has 12670 digits.
!5000 has 16322 digits.
!6000 has 20062 digits.
!7000 has 23875 digits.
!8000 has 27749 digits.
!9000 has 31678 digits.
!10000 has 35656 digits.

```


## Standard ML


```sml

(* reuse earlier factorial calculations in dfac, apply to listed arguments in cumlfac *)
(* example: left factorial n, is #3 (dfac (0,n-1,1,1) ) *)
(* output list contains (number, factorial, left factorial) *)
(* tested in PolyML *)


val store = ref 0;

val rec dfac = fn 
        (from,to,acc,cm) => if from = to then (from,acc,cm) else (store:=(from+1)*acc;dfac (from+1,to,!store,!store+cm ) );

val rec cumlfac = fn 
        (x::y::rm) => x :: cumlfac ( dfac (#1 x, #1 y, #2 x, #3 x) :: rm ) |
        rm =>rm ;

val arguments = List.tabulate (10,fn 0=>(0,1,1)|i=>(i,0,0)) @ 
                List.tabulate (10,fn i=> (10*i+19,0,0) )    @ 
                List.tabulate ( 10,fn i=> (1000*i+999,0,0));

val result = (~1,0,0)::(cumlfac arguments);

(* done *)
(* display: *)

List.app (fn triple :int*int*int =>
        print(Int.toString (1+ #1 triple ) ^ " : " ^ Int.fmt StringCvt.DEC (#3 triple ) ^" \n" )
        ) (List.take(result,21)  ) ;
List.app (fn triple :int*int*int =>
        print( Int.toString (1+ #1 triple ) ^ " : " ^ Int.toString  (size(Int.toString (#3 triple ))) ^" \n" ) ) (List.drop(result,21)  );

```

{{out}}

```txt

time poly --script thisscript
0 : 0 
1 : 1 
2 : 2 
3 : 4 
4 : 10 
5 : 34 
6 : 154 
7 : 874 
8 : 5914 
9 : 46234 
10 : 409114 
20 : 128425485935180314 
30 : 9157958657951075573395300940314 
40 : 20935051082417771847631371547939998232420940314 
50 : 620960027832821612639424806694551108812720525606160920420940314 
60 : 141074930726669571000530822087000522211656242116439949000980378746128920420940314 
70 : 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314 
80 : 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314 
90 : 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314 
100 : 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314 
110 : 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314 
1000 : 2565 
2000 : 5733 
3000 : 9128 
4000 : 12670 
5000 : 16322 
6000 : 20062 
7000 : 23875 
8000 : 27749 
9000 : 31678 
10000 : 35656 
(CPU 2.1Ghz:)        0.36 real         0.29 user         0.08 sys

```


## Tcl


```tcl
proc leftfact {n} {
    set s 0
    for {set i [set f 1]} {$i <= $n} {incr i} {
	incr s $f
	set f [expr {$f * $i}]
    }
    return $s
}

for {set i 0} {$i <= 110} {incr i [expr {$i>9?10:1}]} {
    puts "!$i = [leftfact $i]"
}
for {set i 1000} {$i <= 10000} {incr i 1000} {
    puts "!$i has [string length [leftfact $i]] digits"
}
```

{{out}}

```txt

!0 = 0
!1 = 1
!2 = 2
!3 = 4
!4 = 10
!5 = 34
!6 = 154
!7 = 874
!8 = 5914
!9 = 46234
!10 = 409114
!20 = 128425485935180314
!30 = 9157958657951075573395300940314
!40 = 20935051082417771847631371547939998232420940314
!50 = 620960027832821612639424806694551108812720525606160920420940314
!60 = 141074930726669571000530822087000522211656242116439949000980378746128920420940314
!70 = 173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
!80 = 906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
!90 = 16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
!100 = 942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
!110 = 145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314
!1000 has 2565 digits
!2000 has 5733 digits
!3000 has 9128 digits
!4000 has 12670 digits
!5000 has 16322 digits
!6000 has 20062 digits
!7000 has 23875 digits
!8000 has 27749 digits
!9000 has 31678 digits
!10000 has 35656 digits

```



## zkl

{{trans|D}}

```zkl
var BN=Import("zklBigNum");

fcn leftFact(n){
   [1..n].reduce(fcn(p,n,rf){ p+=rf.value; rf.set(rf.value*n); p },
      BN(0),Ref(BN(1)));
}
```


```zkl
println("First 11 left factorials:\n", [0..10].apply(leftFact));
lfs:=[20..111,10].apply(leftFact);
println(("\n20 through 110 (inclusive) by tens:\n" + 
	 "%d\n"*lfs.len()).fmt(lfs.xplode()));

println("Digits in 1,000 through 10,000 by thousands:\n",
     [0d1_000..0d10_000, 1000].pump(List,fcn(n){leftFact(n).toString().len()}));
```

{{out}}

```txt

First 11 left factorials:
L(0,1,2,4,10,34,154,874,5914,46234,409114)

20 through 110 (inclusive) by tens:
128425485935180314
9157958657951075573395300940314
20935051082417771847631371547939998232420940314
620960027832821612639424806694551108812720525606160920420940314
141074930726669571000530822087000522211656242116439949000980378746128920420940314
173639511802987526699717162409282876065556519849603157850853034644815111221599509216528920420940314
906089587987695346534516804650290637694024830011956365184327674619752094289696314882008531991840922336528920420940314
16695570072624210767034167688394623360733515163575864136345910335924039962404869510225723072235842668787507993136908442336528920420940314
942786239765826579160595268206839381354754349601050974345395410407078230249590414458830117442618180732911203520208889371641659121356556442336528920420940314
145722981061585297004706728001906071948635199234860720988658042536179281328615541936083296163475394237524337422204397431927131629058103519228197429698252556442336528920420940314

Digits in 1,000 through 10,000 by thousands:
L(2565,5733,9128,12670,16322,20062,23875,27749,31678,35656)

```

