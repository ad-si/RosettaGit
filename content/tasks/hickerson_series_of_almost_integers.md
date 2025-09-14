+++
title = "Hickerson series of almost integers"
description = ""
date = 2019-07-01T02:56:36Z
aliases = []
[extra]
id = 17027
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "awk",
  "bracmat",
  "c",
  "c_plus_plus",
  "cobol",
  "clojure",
  "crystal",
  "d",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "ml",
  "pari_gp",
  "pl_i",
  "perl",
  "phix",
  "python",
  "rexx",
  "racket",
  "ring",
  "ruby",
  "scala",
  "seed7",
  "sidef",
  "tcl",
  "jq",
  "mlite",
  "zkl",
  "wolfram_language"
]
+++

The following function, due to D Hickerson, is said to generate "Almost integers" by the [http://mathworld.wolfram.com/AlmostInteger.html "Almost Integer" page of Wolfram Mathworld]. (December 31 2013).

The function is:

:<math>h(n) = {\operatorname{n}!\over2(\ln{2})^{n+1}}</math>

It is said to produce "almost integers" for n between 1 and 17.
The purpose of the task is to verify this assertion.

Assume that an "almost integer" has '''either a nine or a zero as its first digit after the decimal point''' of its decimal string representation

The task is to calculate all values of the function checking and stating which are "almost integers".

Note: Use extended/arbitrary precision numbers in your calculation if necessary to ensure you have adequate precision of results as for example:
    h(18) = 3385534663256845326.39...


## ALGOL 68

```algol68
# determine whether the first few Hickerson numbers really are "near integers" #
# The Hickerson number n is defined by: h(n) = n! / ( 2 * ( (ln 2)^(n+1) ) )   #
#                                   so: h(1) = 1 / ( 2 * ( ( ln 2 ) ^ 2 )      #
#                                  and: h(n) = ( n / ln 2 ) * h(n-1)           #

# set the precision of LONG LONG numbers                                       #
PR precision 100 PR

# calculate the Hickerson numbers                                              #
LONG LONG REAL ln2 = long long ln( 2 );
[ 1 : 18 ]LONG LONG REAL h;

h[ 1 ] := 0.5 / ( ln2 * ln2 );
FOR n FROM 2 TO UPB h
DO
    h[ n ] := ( n * h[ n - 1 ] ) / ln2
OD;

# determine the first digit after the point in each h(n) - if it is 0 or 9     #
# the number is a "near integer"                                               #

FOR n TO UPB h
DO
    INT first decimal = SHORTEN SHORTEN ( ( ENTIER ( h[ n ] * 10 ) ) MOD 10 );
    print( ( whole( n, -4 )
           , " "
           , fixed( h[ n ], 40, 4 )
           , IF first decimal = 0 OR first decimal = 9
             THEN
                 "     a near integer"
             ELSE
                 " NOT a near integer"
             FI
           , newline
           )
        )
OD
```

```txt

   1                                  +1.0407     a near integer
   2                                  +3.0028     a near integer
   3                                 +12.9963     a near integer
   4                                 +74.9987     a near integer
   5                                +541.0015     a near integer
   6                               +4683.0012     a near integer
   7                              +47292.9987     a near integer
   8                             +545834.9979     a near integer
   9                            +7087261.0016     a near integer
  10                          +102247563.0053     a near integer
  11                         +1622632572.9976     a near integer
  12                        +28091567594.9816     a near integer
  13                       +526858348381.0012     a near integer
  14                     +10641342970443.0845     a near integer
  15                    +230283190977853.0374     a near integer
  16                   +5315654681981354.5131 NOT a near integer
  17                 +130370767029135900.4580 NOT a near integer
  18                +3385534663256845326.3903 NOT a near integer

```



## AWK


```AWK

# syntax: GAWK -M -f HICKERSON_SERIES_OF_ALMOST_INTEGERS.AWK
# using GNU Awk 4.1.0, API: 1.0 (GNU MPFR 3.1.2, GNU MP 5.1.2)
BEGIN {
    PREC = 100
    for (i=1; i<=17; i++) {
      h = sprintf("%25.5f",factorial(i) / (2 * log(2) ^ (i + 1)))
      msg = (h ~ /\.[09]/) ? "true" : "false"
      printf("%2d %s almost integer: %s\n",i,h,msg)
    }
    exit(0)
}
function factorial(n,  i,out) {
    out = 1
    for (i=2; i<=n; i++) {
      out *= i
    }
    return(out)
}

```

<p>Output:</p>

```txt

 1                   1.04068 almost integer: true
 2                   3.00278 almost integer: true
 3                  12.99629 almost integer: true
 4                  74.99874 almost integer: true
 5                 541.00152 almost integer: true
 6                4683.00125 almost integer: true
 7               47292.99873 almost integer: true
 8              545834.99791 almost integer: true
 9             7087261.00162 almost integer: true
10           102247563.00527 almost integer: true
11          1622632572.99755 almost integer: true
12         28091567594.98157 almost integer: true
13        526858348381.00125 almost integer: true
14      10641342970443.08453 almost integer: true
15     230283190977853.03744 almost integer: true
16    5315654681981354.51308 almost integer: false
17  130370767029135900.45799 almost integer: false

```



## Bracmat

This solution approximates <math>\log_e{2}</math>
with enough terms to make the first two decimals right with high likelyhood.
(It stops adding more terms when the first three decimals in two consecutive approximations are the same.)
See [https://en.wikipedia.org/wiki/Natural_logarithm_of_2 Natural logarithm of 2]

Bracmat has no built in support for fixed point notation of rational numbers.
Therefore each Hickerson number is split in an integer part (using integer division)
and a decimal part (using a string operation) before outputting.


```bracmat
( 0:?n
& 1:?fac
&   whl
  ' ( 1+!n:~>17:?n
    & !n*!fac:?fac
    & -1:?k
    & 0:?L2
    & 0:?oldN
    &   whl
      ' ( 1+!k:?k
        & ((2*!k+1)*9^!k)^-1+!L2:?L2
        & !fac*1/2*(2/3*!L2)^(-1*(!n+1)):?N
        & div$(1000*!N+1/2.1):?newN
        & !newN:~!oldN:?oldN
        )
    &   out
      $ ( str$("h(" !n ") =")
          ( div$(!newN.1000):?intpart
          & @(!newN:!intpart ?decimalpart)
          & str$(!intpart "." !decimalpart)
          )
          (   @(!decimalpart:(0|9) ?)
            & "is an almost-integer."
          | "is NOT an almost-integer."
          )
        )
    )
);
```

```txt
h(1) = 1.041 is an almost-integer.
h(2) = 3.003 is an almost-integer.
h(3) = 12.996 is an almost-integer.
h(4) = 74.999 is an almost-integer.
h(5) = 541.002 is an almost-integer.
h(6) = 4683.001 is an almost-integer.
h(7) = 47292.999 is an almost-integer.
h(8) = 545834.998 is an almost-integer.
h(9) = 7087261.002 is an almost-integer.
h(10) = 102247563.005 is an almost-integer.
h(11) = 1622632572.998 is an almost-integer.
h(12) = 28091567594.982 is an almost-integer.
h(13) = 526858348381.001 is an almost-integer.
h(14) = 10641342970443.085 is an almost-integer.
h(15) = 230283190977853.038 is an almost-integer.
h(16) = 5315654681981354.513 is NOT an almost-integer.
h(17) = 130370767029135900.458 is NOT an almost-integer.
```



## C

```c
#include <stdio.h>
#include <mpfr.h>

void h(int n)
{
	MPFR_DECL_INIT(a, 200);
	MPFR_DECL_INIT(b, 200);

	mpfr_fac_ui(a, n, MPFR_RNDD);		// a = n!

	mpfr_set_ui(b, 2, MPFR_RNDD);		// b = 2
	mpfr_log(b, b, MPFR_RNDD);		// b = log(b)
	mpfr_pow_ui(b, b, n + 1, MPFR_RNDD);	// b = b^(n+1)

	mpfr_div(a, a, b, MPFR_RNDD);		// a = a / b
	mpfr_div_ui(a, a, 2, MPFR_RNDD);	// a = a / 2

	mpfr_frac(b, a, MPFR_RNDD);		// b = a - [a]
	mpfr_printf("%2d: %23.4Rf  %c\n", n, a,
		mpfr_cmp_d(b, .1) * mpfr_cmp_d(b, .9) > 0 ? 'Y' : 'N');
}

int main(void)
{
	int n;
	for (n = 1; n <= 17; n++) h(n);

	return 0;
}
```

```txt

 1:                  1.0407  Y
 2:                  3.0028  Y
 3:                 12.9963  Y
 4:                 74.9987  Y
 5:                541.0015  Y
 6:               4683.0012  Y
 7:              47292.9987  Y
 8:             545834.9979  Y
 9:            7087261.0016  Y
10:          102247563.0053  Y
11:         1622632572.9976  Y
12:        28091567594.9816  Y
13:       526858348381.0012  Y
14:     10641342970443.0845  Y
15:    230283190977853.0374  Y
16:   5315654681981354.5131  N
17: 130370767029135900.4580  N

```



## C++

```cpp
#include <iostream>
#include <iomanip>
#include <boost/multiprecision/cpp_dec_float.hpp>
#include <boost/math/constants/constants.hpp>
typedef boost::multiprecision::cpp_dec_float_50 decfloat;

int main()
{
    const decfloat ln_two = boost::math::constants::ln_two<decfloat>();
    decfloat numerator = 1, denominator = ln_two;

    for(int n = 1; n <= 17; n++) {
        decfloat h = (numerator *= n) / (denominator *= ln_two) / 2;
        decfloat tenths_dig = floor((h - floor(h)) * 10);
        std::cout << "h(" << std::setw(2) << n << ") = " << std::setw(25) << std::fixed << h <<
            (tenths_dig == 0 || tenths_dig == 9 ? " is " : " is NOT ") << "an almost-integer.\n";
    }
}

```

```txt

h( 1) =                  1.040684 is an almost-integer.
h( 2) =                  3.002781 is an almost-integer.
h( 3) =                 12.996291 is an almost-integer.
h( 4) =                 74.998735 is an almost-integer.
h( 5) =                541.001519 is an almost-integer.
h( 6) =               4683.001247 is an almost-integer.
h( 7) =              47292.998731 is an almost-integer.
h( 8) =             545834.997907 is an almost-integer.
h( 9) =            7087261.001623 is an almost-integer.
h(10) =          102247563.005271 is an almost-integer.
h(11) =         1622632572.997550 is an almost-integer.
h(12) =        28091567594.981572 is an almost-integer.
h(13) =       526858348381.001248 is an almost-integer.
h(14) =     10641342970443.084532 is an almost-integer.
h(15) =    230283190977853.037436 is an almost-integer.
h(16) =   5315654681981354.513077 is NOT an almost-integer.
h(17) = 130370767029135900.457985 is NOT an almost-integer.

```



## Clojure

In order to get enough precision, the natural logarithm of 2 had to be entered manually; the BigDecimal implementation does not have a high-precision logarithm function.


```clojure
(defn hickerson
  "Hickerson number, calculated with BigDecimals and manually-entered high-precision value for ln(2)."
  [n]
  (let [n! (apply *' (range 1M (inc n)))]
    (.divide n! (*' 2 (.pow 0.693147180559945309417232121458M (inc n)))
                30 BigDecimal/ROUND_HALF_UP)))

(defn almost-integer?
  "Tests whether the first digit after the decimal is 0 or 9."
  [x]
  (let [first-digit (int (mod (.divide (*' x 10) 1.0M 0 BigDecimal/ROUND_DOWN)
                              10))]
    (or (= 0 first-digit) (= 9 first-digit))))

; Execute for side effects
(doseq [n (range 1 18) :let [h (hickerson n)]]
  (println (format "%2d %24.5f" n h)
           (if (almost-integer? h)
             "almost integer"
             "NOT almost integer")))
```

```txt
1                  1.04068 almost integer
 2                  3.00278 almost integer
 3                 12.99629 almost integer
 4                 74.99874 almost integer
 5                541.00152 almost integer
 6               4683.00125 almost integer
 7              47292.99873 almost integer
 8             545834.99791 almost integer
 9            7087261.00162 almost integer
10          102247563.00527 almost integer
11         1622632572.99755 almost integer
12        28091567594.98157 almost integer
13       526858348381.00125 almost integer
14     10641342970443.08453 almost integer
15    230283190977853.03744 almost integer
16   5315654681981354.51308 NOT almost integer
17 130370767029135900.45799 NOT almost integer
```



## COBOL

```cobol>       >
SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. hickerson-series.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION ALL INTRINSIC
    .
DATA DIVISION.
WORKING-STORAGE SECTION.
01  n                                   PIC 99 COMP.

01  h                                   PIC Z(19)9.9(10).

01  First-Decimal-Digit-Pos             CONSTANT 22.

PROCEDURE DIVISION.
    PERFORM VARYING n FROM 0 BY 1 UNTIL n > 17
        COMPUTE h = FACTORIAL(n) / (2 * LOG(2) ** (n + 1))
        DISPLAY "h(" n ") = " h " which is " NO ADVANCING
        IF h (First-Decimal-Digit-Pos:1) = "0" OR "9"
            DISPLAY "an almost integer."
        ELSE
            DISPLAY "not an almost integer."
        END-IF
    END-PERFORM
    .
END PROGRAM hickerson-series.
```


```txt

h(00) =                    0.7213475204 which is not an almost integer.
h(01) =                    1.0406844905 which is an almost integer.
h(02) =                    3.0027807071 which is an almost integer.
h(03) =                   12.9962905052 which is an almost integer.
h(04) =                   74.9987354476 which is an almost integer.
h(05) =                  541.0015185164 which is an almost integer.
h(06) =                 4683.0012472622 which is an almost integer.
h(07) =                47292.9987313146 which is an almost integer.
h(08) =               545834.9979074851 which is an almost integer.
h(09) =              7087261.0016228991 which is an almost integer.
h(10) =            102247563.0052710420 which is an almost integer.
h(11) =           1622632572.9975500498 which is an almost integer.
h(12) =          28091567594.9815724407 which is an almost integer.
h(13) =         526858348381.0012482861 which is an almost integer.
h(14) =       10641342970443.0845319270 which is an almost integer.
h(15) =      230283190977853.0374360391 which is an almost integer.
h(16) =     5315654681981354.5130767434 which is not an almost integer.
h(17) =   130370767029135900.4579853491 which is not an almost integer.

```



## Crystal

```ruby
require "big"

LN2 = Math.log(2).to_big_f

FACTORIALS = Hash(Int32, Float64).new{|h,k| h[k] = k * h[k-1]}
FACTORIALS[0] = 1

def hickerson(n)
  FACTORIALS[n] / (2 * LN2 ** (n+1))
end

def nearly_int?(n)
  int = n.round
  (int - 0.1..int + 0.1).includes? n
end

1.upto(17) do |n|
  h = hickerson(n)
  str = nearly_int?(h) ? "nearly integer" : "NOT nearly integer"
  puts "n:%3i h: %s\t%s" % [n, h, str]
end

```

```txt

n:  1 h: 1.04068449050280396857 nearly integer
n:  2 h: 3.00278070715690574485 nearly integer
n:  3 h: 12.9962905052769682015 nearly integer
n:  4 h: 74.9987354476616126737 nearly integer
n:  5 h: 541.00151851642361617  nearly integer
n:  6 h: 4683.00124726225853393 nearly integer
n:  7 h: 47292.998731314636563  nearly integer
n:  8 h: 545834.997907485331424 nearly integer
n:  9 h: 7087261.00162290149215 nearly integer
n: 10 h: 102247563.005271079641 nearly integer
n: 11 h: 1622632572.99755070131 nearly integer
n: 12 h: 28091567594.9815846588 nearly integer
n: 13 h: 526858348381.001495064 nearly integer
n: 14 h: 10641342970443.0898723 nearly integer
n: 15 h: 230283190977853.160709 NOT nearly integer
n: 16 h: 5315654681981357.53644 NOT nearly integer
n: 17 h: 130370767029135978.97  NOT nearly integer

```



## D

The D <code>real</code> type has enough precision for this task.

```d
void main() {
    import std.stdio, std.algorithm, std.mathspecial;

    foreach (immutable n; 1 .. 18) {
        immutable x = gamma(n + 1) / (2 * LN2 ^^ (n + 1)),
                  tenths = cast(int)floor((x - x.floor) * 10);
        writefln("H(%2d)=%22.2f is %snearly integer.", n, x,
                  tenths.among!(0, 9) ? "" : "NOT ");
    }
}
```


```txt
H( 1)=                  1.04 is nearly integer.
H( 2)=                  3.00 is nearly integer.
H( 3)=                 13.00 is nearly integer.
H( 4)=                 75.00 is nearly integer.
H( 5)=                541.00 is nearly integer.
H( 6)=               4683.00 is nearly integer.
H( 7)=              47293.00 is nearly integer.
H( 8)=             545835.00 is nearly integer.
H( 9)=            7087261.00 is nearly integer.
H(10)=          102247563.01 is nearly integer.
H(11)=         1622632573.00 is nearly integer.
H(12)=        28091567594.98 is nearly integer.
H(13)=       526858348381.00 is nearly integer.
H(14)=     10641342970443.08 is nearly integer.
H(15)=    230283190977853.04 is nearly integer.
H(16)=   5315654681981354.51 is NOT nearly integer.
H(17)= 130370767029135900.50 is NOT nearly integer.
```



## Factor


```factor
USING: formatting kernel math math.factorials math.functions
math.ranges sequences ;
IN: rosetta-code.hickerson

: ln2 ( -- x )
    99 [1,b] [ [ 2 swap ^ ] [ * ] bi recip ] map-sum ;

: hickerson ( n -- x ) [ n! ] [ 1 + ln2 swap ^ 2 * ] bi / ;

: almost-int? ( x -- ? ) 10 * truncate 10 mod { 0 9 } member? ;

: hickerson-demo ( -- )
    18 [1,b] [
        dup hickerson dup almost-int?
        "h(%02d) = %23.3f   almost integer? %u\n" printf
    ] each ;

MAIN: hickerson-demo
```

```txt

h(01) =                   1.041   almost integer? t
h(02) =                   3.003   almost integer? t
h(03) =                  12.996   almost integer? t
h(04) =                  74.999   almost integer? t
h(05) =                 541.002   almost integer? t
h(06) =                4683.001   almost integer? t
h(07) =               47292.999   almost integer? t
h(08) =              545834.998   almost integer? t
h(09) =             7087261.002   almost integer? t
h(10) =           102247563.005   almost integer? t
h(11) =          1622632572.998   almost integer? t
h(12) =         28091567594.982   almost integer? t
h(13) =        526858348381.001   almost integer? t
h(14) =      10641342970443.085   almost integer? t
h(15) =     230283190977853.037   almost integer? t
h(16) =    5315654681981354.513   almost integer? f
h(17) =  130370767029135900.458   almost integer? f
h(18) = 3385534663256845326.390   almost integer? f

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Hickerson_series_of_almost_integers this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran


```fortran
Program Hickerson
! 3 February 2014
! not all Fortran compilers provide REAL*16 and INTEGER*8
implicit none
real(kind=kind(1q0)) :: s
integer(kind=kind(1_8)) :: i,n,f,is

do n = 1, 17
      s = 0.5q0 / log(2q0)
      do i = 1,n
         s = (s * i) / log(2q0)
      end do

      is = s
      f = (s-is)*10                 !first digit after decimal point
      if (f == 0 .or. f == 9) then
         write(*,10)n,s,''
      else
         write(*,10)n,s,' NOT'
      endif
end do
10 format('h(',i2,') = ',F23.3,' is',A,' an almost-integer')
end program Hickerson
```

```txt

h( 1) =                   1.041 is an almost-integer
h( 2) =                   3.003 is an almost-integer
h( 3) =                  12.996 is an almost-integer
h( 4) =                  74.999 is an almost-integer
h( 5) =                 541.002 is an almost-integer
h( 6) =                4683.001 is an almost-integer
h( 7) =               47292.999 is an almost-integer
h( 8) =              545834.998 is an almost-integer
h( 9) =             7087261.002 is an almost-integer
h(10) =           102247563.005 is an almost-integer
h(11) =          1622632572.998 is an almost-integer
h(12) =         28091567594.982 is an almost-integer
h(13) =        526858348381.001 is an almost-integer
h(14) =      10641342970443.085 is an almost-integer
h(15) =     230283190977853.037 is an almost-integer
h(16) =    5315654681981354.513 is NOT an almost-integer
h(17) =  130370767029135900.458 is NOT an almost-integer

```


## FreeBASIC

```freebasic
' version 08-10-2016
' compile with: fbc -s gui

#Include Once "gmp.bi"

#Macro init_float_size (big_float ,size)
  Dim As Mpf_ptr big_float = Allocate( Len( __mpf_struct))
  Mpf_init2( big_float,size)
#EndMacro

#Macro mpf_remove(big_float)
  Mpf_clear(big_float)
  DeAllocate (big_float)
#EndMacro

Screen 20

init_float_size(tmp,1024)
init_float_size(ln2,1024)
init_float_size(pow_ln2,2048)
init_float_size(answer,2048)

Dim As UInteger n, num
Dim As String st
Dim As ZString Ptr text
text = Allocate (1500) ' size 1500 char.

' need to calculate ln(2)

'     x    1     1         1           1
' ln --- = - + ------ + ------ ... + ------
'    x-1   x    2x^2     3x^3         nx^n

Mpf_set_ui(answer, 1)
Mpf_div_ui(answer, answer, 2)   ' answer = 1/2

For n = 2 To 1024
  Mpf_set_ui(tmp, 2)            ' tmp = x = 2
  Mpf_pow_ui(tmp, tmp, n)       ' tmp = x ^ n
  Mpf_mul_ui(tmp, tmp, n)       ' tmp = n * (x ^ n)
  Mpf_ui_div(tmp, 1, tmp)       ' tmp = 1 / (n * (x ^ n))
  Mpf_add(answer, answer, tmp)  ' answer += tmp
Next

/'  remove this line if you want to print ln(2)
Gmp_sprintf(text,!"ln(2) =\t %2.100Ff ", answer)
Print *text
Print
'/

Mpf_set_ui(tmp, 1)
Mpf_set(ln2, answer )
Mpf_set(pow_ln2, ln2)

Print
For n = 1 To 40
  Mpf_mul_ui(tmp, tmp, n)                           ' n!
  Mpf_mul(pow_ln2, pow_ln2, ln2)                    ' ln(2)^(n+1)
  Mpf_div_ui(answer, tmp , 2)                       ' / 2
  Mpf_div(answer, answer, pow_ln2)                  ' / ln(2)^(n+1)
  Gmp_sprintf(text,!"h(%i) =\t %65.5Ff ",n, answer)
  st = *text
  num = st[InStr(st,".")]  ' get the first character after the "."
  If num = Asc("0") Or num = Asc("9") Then
    Color 10
    Print st;" is a almost integer"
    Color 15
  Else
    Color 12
    Print st;" not close enough"
    Color 15
  End If
Next

mpf_remove(answer)  'cleanup
mpf_remove(pow_ln2)
mpf_remove(ln2)
mpf_remove(tmp)
DeAllocate (text)


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
h(1) =	                                                           1.04068  is a almost integer
h(2) =	                                                           3.00278  is a almost integer
h(3) =	                                                          12.99629  is a almost integer
h(4) =	                                                          74.99874  is a almost integer
h(5) =	                                                         541.00152  is a almost integer
h(6) =	                                                        4683.00125  is a almost integer
h(7) =	                                                       47292.99873  is a almost integer
h(8) =	                                                      545834.99791  is a almost integer
h(9) =	                                                     7087261.00162  is a almost integer
h(10) =	                                                   102247563.00527  is a almost integer
h(11) =	                                                  1622632572.99755  is a almost integer
h(12) =	                                                 28091567594.98157  is a almost integer
h(13) =	                                                526858348381.00125  is a almost integer
h(14) =	                                              10641342970443.08453  is a almost integer
h(15) =	                                             230283190977853.03744  is a almost integer
h(16) =	                                            5315654681981354.51308  not close enough
h(17) =	                                          130370767029135900.45799  not close enough
h(18) =	                                         3385534663256845326.39030  not close enough
h(19) =	                                        92801587319328411139.87379  not close enough
h(20) =	                                      2677687796244384203087.52849  not close enough
h(21) =	                                     81124824998504073881728.73642  not close enough
h(22) =	                                   2574844419803190384544450.20255  not close enough
h(23) =	                                  85438451336745709294581778.58838  not close enough
h(24) =	                                2958279121074145472650646597.12769  not close enough
h(25) =	                              106697365438475775825583475660.43285  not close enough
h(26) =	                             4002225759844168492486127555858.62763  not close enough
h(27) =	                           155897763918621623249276226664346.97380  is a almost integer
h(28) =	                          6297562064950066033518373935416160.57852  not close enough
h(29) =	                        263478385263023690020893329036314027.57946  not close enough
h(30) =	                      11403568794011880483742464196174527074.25258  not close enough
h(31) =	                     510008036574269388430841024076099269561.72065  not close enough
h(32) =	                   23545154085734896649184490637145314147689.71316  not close enough
h(33) =	                 1120959742203056268267494209293002620410998.73593  not close enough
h(34) =	                54984904077825684862426868390301031843623475.17448  not close enough
h(35) =	              2776425695289206002630310219593685601350868792.44731  not close enough
h(36) =	            144199280951655469628360978109406918279521398522.98537  is a almost integer
h(37) =	           7697316738562185268347644943000493477791292787264.22720  not close enough
h(38) =	         421985466101260424678587486718115935816310614908533.21973  not close enough
h(39) =	       23743057231588741419119534567705900419845239863116450.98123  is a almost integer
h(40) =	     1370159636942236704917645663312384364387429049720377464.47614  not close enough
```



## Go


```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    ln2, _ := new(big.Rat).SetString("0.6931471805599453094172")
    h := big.NewRat(1, 2)
    h.Quo(h, ln2)
    var f big.Rat
    var w big.Int
    for i := int64(1); i <= 17; i++ {
        h.Quo(h.Mul(h, f.SetInt64(i)), ln2)
        w.Quo(h.Num(), h.Denom())
        f.Sub(h, f.SetInt(&w))
        y, _ := f.Float64()
        d := fmt.Sprintf("%.3f", y)
        fmt.Printf("n: %2d  h: %18d%s  Nearly integer: %t\n",
            i, &w, d[1:], d[2] == '0' || d[2] == '9')
    }
}
```

```txt

n:  1  h:                  1.041  Nearly integer: true
n:  2  h:                  3.003  Nearly integer: true
n:  3  h:                 12.996  Nearly integer: true
n:  4  h:                 74.999  Nearly integer: true
n:  5  h:                541.002  Nearly integer: true
n:  6  h:               4683.001  Nearly integer: true
n:  7  h:              47292.999  Nearly integer: true
n:  8  h:             545834.998  Nearly integer: true
n:  9  h:            7087261.002  Nearly integer: true
n: 10  h:          102247563.005  Nearly integer: true
n: 11  h:         1622632572.998  Nearly integer: true
n: 12  h:        28091567594.982  Nearly integer: true
n: 13  h:       526858348381.001  Nearly integer: true
n: 14  h:     10641342970443.085  Nearly integer: true
n: 15  h:    230283190977853.037  Nearly integer: true
n: 16  h:   5315654681981354.513  Nearly integer: false
n: 17  h: 130370767029135900.458  Nearly integer: false

```


## Haskell


```haskell
import Data.Number.CReal -- from numbers

import qualified Data.Number.CReal as C

hickerson :: Int -> C.CReal
hickerson n = (fromIntegral $ product [1..n]) / (2 * (log 2 ^ (n + 1)))

charAfter :: Char -> String -> Char
charAfter ch string = ( dropWhile (/= ch) string ) !! 1

isAlmostInteger :: C.CReal -> Bool
isAlmostInteger = (`elem` ['0', '9']) . charAfter '.' . show

checkHickerson :: Int -> String
checkHickerson n  = show $ (n, hickerson n, isAlmostInteger $ hickerson n)

main :: IO ()
main = mapM_ putStrLn $ map checkHickerson [1..18]



```

<pre style="font-size:80%">
(1,1.040684490502803898934790801867495712532,True)
(2,3.0027807071569054434997674072193048895102,True)
(3,12.9962905052769664622248845429643076304964,True)
(4,74.9987354476616001276345503756447016691321,True)
(5,541.0015185164235075692027746182564709468146,True)
(6,4683.0012472622574371804671524790095859510537,True)
(7,47292.9987313146239048228354877863088058057962,True)
(8,545834.9979074851670672910397944923634653083422,True)
(9,7087261.0016228991209791875159082204876452222335,True)
(10,102247563.0052710420110883885941994702319297523418,True)
(11,1622632572.9975500498528748610784990790541604568286,True)
(12,28091567594.9815724407151891760995257984907158464557,True)
(13,526858348381.0012482861804893808362085163957120038669,True)
(14,10641342970443.0845319270950701503926391962048486244252,True)
(15,230283190977853.0374360391259771068310386307484746008223,True)
(16,5315654681981354.5130767434568055779953901730514005313021,False)
(17,130370767029135900.4579853491967736302239049400128248109361,False)
(18,3385534663256845326.3903018015167499502289072143201827212596,False)


```



## J


Using <math>\ln 2 = \sum_{k\ge 1} \frac{1}{k2^k}.</math> to approximate the natural log of 2 (defining a function whose argument is the limit value for k).

Definitions:


```J
ln2=: [: +/ 1 % [: (*2x&^) 1+i.
h=: ! % 2*(ln2 99)^>:
```


Implementation (1 in initial column indicates an almost integer, results displayed to 3 digits after the decimal point):


```J
   1 23j3": (h ,.~ 0 9 e.~ 10 <.@* 1 | h) x:1+i.17
1                  1.041
1                  3.003
1                 12.996
1                 74.999
1                541.002
1               4683.001
1              47292.999
1             545834.998
1            7087261.002
1          102247563.005
1         1622632572.998
1        28091567594.982
1       526858348381.001
1     10641342970443.085
1    230283190977853.037
0   5315654681981354.513
0 130370767029135900.458
```


In other words, multiply the fractional part of h by ten, find the largest integer not greater than this result, and check if it's in the set {0,9}. Then format the result of h along with this set membership result.


## Java


```java
import java.math.*;

public class Hickerson {

    final static String LN2 = "0.693147180559945309417232121458";

    public static void main(String[] args) {
        for (int n = 1; n <= 17; n++)
            System.out.printf("%2s is almost integer: %s%n", n, almostInteger(n));
    }

    static boolean almostInteger(int n) {
        BigDecimal a = new BigDecimal(LN2);
        a = a.pow(n + 1).multiply(BigDecimal.valueOf(2));

        long f = n;
        while (--n > 1)
            f *= n;

        BigDecimal b = new BigDecimal(f);
        b = b.divide(a, MathContext.DECIMAL128);

        BigInteger c = b.movePointRight(1).toBigInteger().mod(BigInteger.TEN);

        return c.toString().matches("0|9");
    }
}
```



```txt
 1 is almost integer: true
 2 is almost integer: true
 3 is almost integer: true
 4 is almost integer: true
 5 is almost integer: true
 6 is almost integer: true
 7 is almost integer: true
 8 is almost integer: true
 9 is almost integer: true
10 is almost integer: true
11 is almost integer: true
12 is almost integer: true
13 is almost integer: true
14 is almost integer: true
15 is almost integer: true
16 is almost integer: false
17 is almost integer: false
```



## jq

jq currently uses IEEE 754 64-bit numbers, and therefore the built-in arithmetic functions lack adequate precision to solve the task completely.  In the following, therefore, we include a check for adequate precision.

```jq
def hickerson:
  . as $n
  | (2|log) as $log2
  | reduce range(1;$n+1) as $i ( 0.5/$log2; . * $i / $log2) ;

def precise:
   (. - 0.05) as $x | . != ($x + 0.1) ;

def almost_an_integer:
  tostring
  | index(".") as $ix
  | if $ix == null then true
    else .[$ix+1:$ix+2] | (. == "9" or . == "0")
    end ;

range(1;18)
  | . as $i
  | hickerson
  | if precise then
      if almost_an_integer
        then "hickerson(\($i)) is \(.) -- almost an integer"
      else "hickerson(\($i)) is \(.)"
      end
    else "insufficient precision for hickerson(\($i))"
    end
```

```sh
$ jq -M -r -n -f Hickerson_series.jq
hickerson(1) is 1.0406844905028039 -- almost an integer
hickerson(2) is 3.0027807071569055 -- almost an integer
hickerson(3) is 12.996290505276967 -- almost an integer
hickerson(4) is 74.9987354476616 -- almost an integer
hickerson(5) is 541.0015185164235 -- almost an integer
hickerson(6) is 4683.001247262258 -- almost an integer
hickerson(7) is 47292.99873131463 -- almost an integer
hickerson(8) is 545834.9979074852 -- almost an integer
hickerson(9) is 7087261.001622899 -- almost an integer
hickerson(10) is 102247563.00527105 -- almost an integer
hickerson(11) is 1622632572.9975502 -- almost an integer
hickerson(12) is 28091567594.98158 -- almost an integer
hickerson(13) is 526858348381.00134 -- almost an integer
hickerson(14) is 10641342970443.086 -- almost an integer
hickerson(15) is 230283190977853.06 -- almost an integer
insufficient precision for hickerson(16)
insufficient precision for hickerson(17)
```



## Julia

This solution implements its Hickerson Series function as a closure.  It explores the effects of datatype precision, implementing a rather conservative "guard number" scheme to identify when the results may have inadequate precision.  One can not be confident of the 64-bit floating point results beyond <tt>n=13</tt>, but the 256-bit precision floating point results are easily precise enough for the entire calculation to <tt>n=17</tt>.  (A slight variant of this calculation, not shown here, indicates that these extended precision numbers are adequate to <tt>n=50</tt>.)

```Julia

function makehickerson{T<:Real}(x::T)
    n = 0
    h = one(T)/2x
    function hickerson()
        n += 1
        h *= n/x
    end
end

function reporthickerson{T<:Real,U<:Integer}(a::T, nmax::U)
    h = makehickerson(a)
    hgm = makehickerson(prevfloat(a))
    hgp = makehickerson(nextfloat(a))

    println()
    print("Performing calculations using ", typeof(a))
    println(", which has ", precision(a), "-bit precision.")
    for i in 1:nmax
        x = h()
        xm = hgm()
        xp = hgp()
        y = ifloor(10x)
        ym = ifloor(10xm)
        yp = ifloor(10xp)
        println()
        println("Hickerson series result for n = ", i)
        println(@sprintf("    ->  %25.4f ", xm))
        println(@sprintf("    0>  %25.4f ", x))
        println(@sprintf("    +>  %25.4f ", xp))
        isprecok =
        isint =
        if ym == y == yp
            print("The precision is adequate, ")
            if  digits(y)[1] in [0, 9]
                println("and the result is an almost integer.")
            else
                println("but the result is not an almost integer.")
            end
        else
            println("The precision is inadequate for a definite result.")
        end
    end
end

a = log(big(2.0))
reporthickerson(a, 17)

a = log(2.0)
reporthickerson(a, 17)

```


```txt

Performing calculations using BigFloat, which has 256-bit precision.

Hickerson series result for n = 1
    ->                     1.0407
    0>                     1.0407
    +>                     1.0407
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 2
    ->                     3.0028
    0>                     3.0028
    +>                     3.0028
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 3
    ->                    12.9963
    0>                    12.9963
    +>                    12.9963
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 4
    ->                    74.9987
    0>                    74.9987
    +>                    74.9987
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 5
    ->                   541.0015
    0>                   541.0015
    +>                   541.0015
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 6
    ->                  4683.0012
    0>                  4683.0012
    +>                  4683.0012
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 7
    ->                 47292.9987
    0>                 47292.9987
    +>                 47292.9987
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 8
    ->                545834.9979
    0>                545834.9979
    +>                545834.9979
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 9
    ->               7087261.0016
    0>               7087261.0016
    +>               7087261.0016
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 10
    ->             102247563.0053
    0>             102247563.0053
    +>             102247563.0053
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 11
    ->            1622632572.9976
    0>            1622632572.9976
    +>            1622632572.9976
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 12
    ->           28091567594.9816
    0>           28091567594.9816
    +>           28091567594.9816
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 13
    ->          526858348381.0012
    0>          526858348381.0012
    +>          526858348381.0012
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 14
    ->        10641342970443.0845
    0>        10641342970443.0845
    +>        10641342970443.0845
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 15
    ->       230283190977853.0374
    0>       230283190977853.0374
    +>       230283190977853.0374
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 16
    ->      5315654681981354.5131
    0>      5315654681981354.5131
    +>      5315654681981354.5131
The precision is adequate, but the result is not an almost integer.

Hickerson series result for n = 17
    ->    130370767029135900.4580
    0>    130370767029135900.4580
    +>    130370767029135900.4580
The precision is adequate, but the result is not an almost integer.

Performing calculations using Float64, which has 53-bit precision.

Hickerson series result for n = 1
    ->                     1.0407
    0>                     1.0407
    +>                     1.0407
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 2
    ->                     3.0028
    0>                     3.0028
    +>                     3.0028
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 3
    ->                    12.9963
    0>                    12.9963
    +>                    12.9963
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 4
    ->                    74.9987
    0>                    74.9987
    +>                    74.9987
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 5
    ->                   541.0015
    0>                   541.0015
    +>                   541.0015
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 6
    ->                  4683.0012
    0>                  4683.0012
    +>                  4683.0012
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 7
    ->                 47292.9987
    0>                 47292.9987
    +>                 47292.9987
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 8
    ->                545834.9979
    0>                545834.9979
    +>                545834.9979
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 9
    ->               7087261.0016
    0>               7087261.0016
    +>               7087261.0016
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 10
    ->             102247563.0053
    0>             102247563.0053
    +>             102247563.0053
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 11
    ->            1622632572.9976
    0>            1622632572.9976
    +>            1622632572.9975
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 12
    ->           28091567594.9816
    0>           28091567594.9816
    +>           28091567594.9815
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 13
    ->          526858348381.0024
    0>          526858348381.0015
    +>          526858348381.0003
The precision is adequate, and the result is an almost integer.

Hickerson series result for n = 14
    ->        10641342970443.1094
    0>        10641342970443.0898
    +>        10641342970443.0645
The precision is inadequate for a definite result.

Hickerson series result for n = 15
    ->       230283190977853.5938
    0>       230283190977853.1563
    +>       230283190977852.5625
The precision is inadequate for a definite result.

Hickerson series result for n = 16
    ->      5315654681981368.0000
    0>      5315654681981357.0000
    +>      5315654681981343.0000
The precision is inadequate for a definite result.

Hickerson series result for n = 17
    ->    130370767029136256.0000
    0>    130370767029135968.0000
    +>    130370767029135600.0000
The precision is inadequate for a definite result.

```



## Kotlin

```kotlin
// version 1.1.4

import java.math.BigDecimal
import java.math.BigInteger
import java.math.MathContext

object Hickerson {
    private const val LN2 = "0.693147180559945309417232121458"

    fun almostInteger(n: Int): Boolean {
        val a = BigDecimal(LN2).pow(n + 1) * BigDecimal(2)
        var nn = n
        var f = n.toLong()
        while (--nn > 1) f *= nn
        val b = BigDecimal(f).divide(a, MathContext.DECIMAL128)
        val c = b.movePointRight(1).toBigInteger() % BigInteger.TEN
        return c.toString().matches(Regex("[09]"))
    }
}

fun main(args: Array<String>) {
    for (n in 1..17) println("${"%2d".format(n)} is almost integer: ${Hickerson.almostInteger(n)}")
}
```


```txt

 1 is almost integer: true
 2 is almost integer: true
 3 is almost integer: true
 4 is almost integer: true
 5 is almost integer: true
 6 is almost integer: true
 7 is almost integer: true
 8 is almost integer: true
 9 is almost integer: true
10 is almost integer: true
11 is almost integer: true
12 is almost integer: true
13 is almost integer: true
14 is almost integer: true
15 is almost integer: true
16 is almost integer: false
17 is almost integer: false

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
h[n_] = n!/(2 (Log[2])^(n + 1));
firstdecimal[x_] := Floor[10 x] - 10 Floor[x];
almostIntegerQ[x_] := firstdecimal[x] == 0 || firstdecimal[x] == 9;
Table[{i, AccountingForm[N[h[i], 50], {Infinity, 5}], almostIntegerQ[N[h[i], 50]]}, {i, 1, 17}] // TableForm
```

```txt
1	1.04068				True
2	3.00278				True
3	12.99629			True
4	74.99874			True
5	541.00152			True
6	4683.00125			True
7	47292.99873			True
8	545834.99791			True
9	7087261.00162			True
10	102247563.00527			True
11	1622632572.99755		True
12	28091567594.98157		True
13	526858348381.00125		True
14	10641342970443.08453		True
15	230283190977853.03744		True
16	5315654681981354.51308		False
17	130370767029135900.45799	False
```



## PARI/GP


```parigp
h(n)=n!/2/log(2)^(n+1)
almost(x)=abs(x-round(x))<.1
select(n->almost(h(n)),[1..17])

```

```txt
%1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
```



## ML

=
## mLite
=

```ocaml
local
  fun log (n, k, last = sum, sum) = sum
        | (n, k, last, sum) = log (n, k + 1, sum, sum + ( 1 / (k * n ^ k)))
        | n = log (n, 1, 1, 0);
  val ln2 = log 2
in
  fun hickerson n = (fold (opt *,1) ` iota n) / (2 * ln2 ^ (n+1));
  fun almost_int ([]) = false
               | (h :: h' :: t) where (h <> #".") = almost_int (h' :: t)
               | (h :: h' :: t) = h' = #"0" or h' = #"9"
               | s = almost_int (explode s)
  and show (n, h) = println (n, ntos h, almost_int ` ntos ` h)
         | n = show (n, hickerson n)
end
;
foreach show ` iota 17;

```

Output

```txt
(1, 1.04068449050280389893479080186749587, true)
(2, 3.00278070715690544349976740721930557, true)
(3, 12.9962905052769664622248845429643116, true)
(4, 74.9987354476616001276345503756447306, true)
(5, 541.001518516423507569202774618256721, true)
(6, 4683.0012472622574371804671524790121, true)
(7, 47292.9987313146239048228354877863377, true)
(8, 545834.997907485167067291039794492739, true)
(9, 7087261.00162289912097918751590822596, true)
(10, 102247563.005271042011088388594199557, true)
(11, 1622632572.99755004985287486107850058, true)
(12, 28091567594.9815724407151891760995539, true)
(13, 526858348381.001248286180489380836777, true)
(14, 10641342970443.0845319270950701504049, true)
(15, 230283190977853.037436039125977107112, true)
(16, 5315654681981354.51307674345680558492, false)
(17, 130370767029135900.457985349196773809, false)

```




## Perl

We'll use Math::BigFloat, which comes with perl.

Conveniently, it includes a sufficiently precise logarithm function.

To avoid doing factorials or exponentiations or divisions in the loop, we first
calculate the inverse of the natural log of 2, and then compute a running value of h.


```perl
use strict;
use warnings;
use Math::BigFloat;

my $iln2 = 1 / Math::BigFloat->new(2)->blog;
my $h = $iln2 / 2;

for my $n ( 1 .. 17 ) {
	$h *= $iln2;
	$h *= $n;
	my $s = $h->copy->bfround(-3)->bstr;
	printf "h(%2d) = %22s is%s almost an integer.\n",
		$n, $s, ($s =~ /\.[09]/ ? "" : " NOT");
}

```

```txt
h( 1) =                  1.041 is almost an integer.
h( 2) =                  3.003 is almost an integer.
h( 3) =                 12.996 is almost an integer.
h( 4) =                 74.999 is almost an integer.
h( 5) =                541.002 is almost an integer.
h( 6) =               4683.001 is almost an integer.
h( 7) =              47292.999 is almost an integer.
h( 8) =             545834.998 is almost an integer.
h( 9) =            7087261.002 is almost an integer.
h(10) =          102247563.005 is almost an integer.
h(11) =         1622632572.998 is almost an integer.
h(12) =        28091567594.982 is almost an integer.
h(13) =       526858348381.001 is almost an integer.
h(14) =     10641342970443.085 is almost an integer.
h(15) =    230283190977853.037 is almost an integer.
h(16) =   5315654681981354.513 is NOT almost an integer.
h(17) = 130370767029135900.458 is NOT almost an integer.
```



## Perl 6

We'll use [http://doc.perl6.org/type/FatRat FatRat] values, and a series for an [http://mathworld.wolfram.com/NaturalLogarithmof2.html approximation of ln(2)].


```perl6
constant ln2 = [+] (1/2.FatRat, */2 ... *) Z/ 1 .. 100;
constant h = [\*] 1/2, |(1..*) X/ ln2;

use Test;
plan *;

for h[1..17] {
    ok m/'.'<[09]>/, .round(0.001)
}
```

```txt
ok 1 - 1.041
ok 2 - 3.003
ok 3 - 12.996
ok 4 - 74.999
ok 5 - 541.002
ok 6 - 4683.001
ok 7 - 47292.999
ok 8 - 545834.998
ok 9 - 7087261.002
ok 10 - 102247563.005
ok 11 - 1622632572.998
ok 12 - 28091567594.982
ok 13 - 526858348381.001
ok 14 - 10641342970443.085
ok 15 - 230283190977853.037
not ok 16 - 5315654681981354.513

# Failed test '5315654681981354.513'
# at hickerson line 8
not ok 17 - 130370767029135900.458

# Failed test '130370767029135900.458'
# at hickerson line 8
```



## Phix


```Phix
include bigatom.e
bigatom ln2 = ba_log(2),
        hn = ba_divide(0.5,ln2)
for n=1 to 17 do
    hn = ba_divide(ba_multiply(n,hn),ln2)
    string n10 = ba_sprintf("%24.4aB ",hn)
    n10 &= iff(find(n10[21],"09")?"Y":"N")
    printf(1,"%2d:%s\n",{n,n10})
end for
```

```txt

 1:                  1.0407 Y
 2:                  3.0028 Y
 3:                 12.9963 Y
 4:                 74.9987 Y
 5:                541.0015 Y
 6:               4683.0012 Y
 7:              47292.9987 Y
 8:             545834.9979 Y
 9:            7087261.0016 Y
10:          102247563.0053 Y
11:         1622632572.9976 Y
12:        28091567594.9816 Y
13:       526858348381.0012 Y
14:     10641342970443.0845 Y
15:    230283190977853.0374 Y
16:   5315654681981354.5131 N
17: 130370767029135900.4580 N

```



## PL/I


```pli
Hickerson: procedure options (main); /* 12 January 2014 */
   declare s float (18), (i, n) fixed binary;
   declare is fixed decimal (30);
   declare f fixed decimal (4,3);

   do n = 1 to 16; /* 17 exceeds hardware precision */
      s = 0.5Q0 / 0.693147180559945309417232121458;
      do i = 1 to n;
         s = (s * i) / 0.693147180559945309417232121458;
      end;

      is = s;
      f = s-is;
      is = 10*f;
      put skip edit (n, s) (f(2), f(30,3));
      if is = 0 | is = 9 then
         put edit (' is a near integer') (A);
      else
         put edit (' is not a near integer') (A);
   end;

end Hickerson;
```

```txt

 1                         1.041 is a near integer
 2                         3.003 is a near integer
 3                        12.996 is a near integer
 4                        74.999 is a near integer
 5                       541.002 is a near integer
 6                      4683.001 is a near integer
 7                     47292.999 is a near integer
 8                    545834.998 is a near integer
 9                   7087261.002 is a near integer
10                 102247563.005 is a near integer
11                1622632572.998 is a near integer
12               28091567594.982 is a near integer
13              526858348381.001 is a near integer
14            10641342970443.085 is a near integer
15           230283190977853.037 is a near integer
16          5315654681981354.510 is not a near integer

```

Using extended precision available by software:

```pli

do n = 1 to 18;
      s = divide( float(0.5), float(0.693147180559945309417232121458) );
      do i = 1 to n;
         s = divide ( multiply(s, float(i)), float(0.693147180559945309417232121458) );
      end;
      is = trunc(s);
      f  = subtract(s, is);
      is = multiply(float(10), f);
      is = trunc(is);
      put skip edit (n) (f(3)); call output_fixed (s, 'F(60,4)' );

      if compare(is, float(0)) = 0 | compare(is, float(9)) = 0 then
         put edit (' is a near integer') (A);
      else
         put edit (' is not a near integer') (A);
   end;

```

{{out}} Results:

```txt

  1                                            1.0406 is a near integer
  2                                            3.0027 is a near integer
  3                                           12.9962 is a near integer
  4                                           74.9987 is a near integer
  5                                          541.0015 is a near integer
  6                                         4683.0012 is a near integer
  7                                        47292.9987 is a near integer
  8                                       545834.9979 is a near integer
  9                                      7087261.0016 is a near integer
 10                                    102247563.0052 is a near integer
 11                                   1622632572.9975 is a near integer
 12                                  28091567594.9815 is a near integer
 13                                 526858348381.0012 is a near integer
 14                               10641342970443.0845 is a near integer
 15                              230283190977853.0374 is a near integer
 16                             5315654681981354.5130 is not a near integer
 17                           130370767029135900.4579 is not a near integer
 18                          3385534663256845326.3903 is not a near integer

```



## Python

This uses Pythons [http://docs.python.org/2/library/decimal.html decimal module] of fixed precision decimal floating point calculations.


```python
from decimal import Decimal
import math

def h(n):
    'Simple, reduced precision calculation'
    return math.factorial(n) / (2 * math.log(2) ** (n + 1))

def h2(n):
    'Extended precision Hickerson function'
    return Decimal(math.factorial(n)) / (2 * Decimal(2).ln() ** (n + 1))

for n in range(18):
    x = h2(n)
    norm = str(x.normalize())
    almostinteger = (' Nearly integer'
                     if 'E' not in norm and ('.0' in norm or '.9' in norm)
                     else ' NOT nearly integer!')
    print('n:%2i h:%s%s' % (n, norm, almostinteger))
```


```txt
n: 0 h:0.7213475204444817036799623405 NOT nearly integer!
n: 1 h:1.040684490502803898934790802 Nearly integer
n: 2 h:3.002780707156905443499767406 Nearly integer
n: 3 h:12.99629050527696646222488454 Nearly integer
n: 4 h:74.99873544766160012763455035 Nearly integer
n: 5 h:541.0015185164235075692027746 Nearly integer
n: 6 h:4683.001247262257437180467151 Nearly integer
n: 7 h:47292.99873131462390482283547 Nearly integer
n: 8 h:545834.9979074851670672910395 Nearly integer
n: 9 h:7087261.001622899120979187513 Nearly integer
n:10 h:102247563.0052710420110883885 Nearly integer
n:11 h:1622632572.997550049852874859 Nearly integer
n:12 h:28091567594.98157244071518915 Nearly integer
n:13 h:526858348381.0012482861804887 Nearly integer
n:14 h:10641342970443.08453192709506 Nearly integer
n:15 h:230283190977853.0374360391257 Nearly integer
n:16 h:5315654681981354.513076743451 NOT nearly integer!
n:17 h:130370767029135900.4579853491 NOT nearly integer!
```


'''The range for <math>n</math> should be reduced''' to be <math>1<= n <= 15</math> for this definition of almost integer.


## Racket

```Racket
#lang racket
(require math/bigfloat)

(define (hickerson n)
 (bf/ (bffactorial n)
      2.bf
      (bfexpt (bflog 2.bf) (bf (+ n 1)))))

(for ([n (in-range 18)])
  (define hickerson-n (hickerson n))
  (define first-decimal
    (bigfloat->integer (bftruncate (bf* 10.bf (bffrac hickerson-n)))))
  (define almost-integer (if (or (= first-decimal 0) (= first-decimal 9))
                           "is Nearly integer"
                           "is not Nearly integer!"))
  (printf "~a: ~a ~a\n"
          (~r n #:min-width 2)
          (bigfloat->string hickerson-n)
          almost-integer))
```

```txt
 0: 0.7213475204444817036799623405009460687136 is not Nearly integer!
 1: 1.040684490502803898934790801867495712535 is Nearly integer
 2: 3.002780707156905443499767407219304889517 is Nearly integer
 3: 12.99629050527696646222488454296430763056 is Nearly integer
 4: 74.99873544766160012763455037564470166964 is Nearly integer
 5: 541.0015185164235075692027746182564709501 is Nearly integer
 6: 4683.001247262257437180467152479009585976 is Nearly integer
 7: 47292.99873131462390482283548778630880614 is Nearly integer
 8: 545834.9979074851670672910397944923634685 is Nearly integer
 9: 7087261.001622899120979187515908220487726 is Nearly integer
10: 102247563.005271042011088388594199470233 is Nearly integer
11: 1622632572.997550049852874861078499079071 is Nearly integer
12: 28091567594.98157244071518917609952579893 is Nearly integer
13: 526858348381.0012482861804893808362085239 is Nearly integer
14: 10641342970443.08453192709507015039263936 is Nearly integer
15: 230283190977853.0374360391259771068310425 is Nearly integer
16: 5315654681981354.513076743456805577995494 is not Nearly integer!
17: 130370767029135900.4579853491967736302263 is not Nearly integer!
```



## REXX


### version 1


```rexx
/* REXX ---------------------------------------------------------------
* 04.01.2014 Walter Pachl - using a rather aged ln function of mine
*                           with probably unreasonably high precision
* 05.01.2014 added n=18
*--------------------------------------------------------------------*/
Numeric Digits 100
Do n=1 To 18
  x=format(def(),20,10)
  Parse Var x '.' +1 d +1
  If pos(d,'09')>0 Then
    tag='almost an integer'
  Else
    tag=''
  Say right(n,2) x tag
  End
Exit

def:
 x=fact(n)/(2*ln(2,200)**(n + 1))
 Return x

ln: Procedure
/***********************************************************************
* Return ln(x) -- with specified precision
* Three different series are used for the ranges  0 to 0.5
*                                                 0.5 to 1.5
*                                                 1.5 to infinity
* 920903 Walter Pachl
***********************************************************************/
  Parse Arg x,prec,b
  If prec='' Then prec=9
  Numeric Digits (2*prec)
  Numeric Fuzz   3
  Select
    When x<=0 Then r='*** invalid argument ***'
    When x<0.5 Then Do
      z=(x-1)/(x+1)
      o=z
      r=z
      k=1
      Do i=3 By 2
        ra=r
        k=k+1
        o=o*z*z
        r=r+o/i
        If r=ra Then Leave
        End
      r=2*r
      End
    When x<1.5 Then Do
      z=(x-1)
      o=z
      r=z
      k=1
      Do i=2 By 1
        ra=r
        k=k+1
        o=-o*z
        r=r+o/i
        If r=ra Then Leave
        End
      End
    Otherwise /* 1.5<=x */ Do
      z=(x+1)/(x-1)
      o=1/z
      r=o
      k=1
      Do i=3 By 2
        ra=r
        k=k+1
        o=o/(z*z)
        r=r+o/i
        If r=ra Then Leave
        End
      r=2*r
      End
    End
  If b<>'' Then
    r=r/ln(b)
  Numeric Digits (prec)
  Return r+0

fact: Procedure
 Parse Arg m
 fact=1
 Do i=2 To m
   fact=fact*i
   End
 Return fact
```

```txt
 1                    1.0406844905 almost an integer
 2                    3.0027807072 almost an integer
 3                   12.9962905053 almost an integer
 4                   74.9987354477 almost an integer
 5                  541.0015185164 almost an integer
 6                 4683.0012472623 almost an integer
 7                47292.9987313146 almost an integer
 8               545834.9979074852 almost an integer
 9              7087261.0016228991 almost an integer
10            102247563.0052710420 almost an integer
11           1622632572.9975500499 almost an integer
12          28091567594.9815724407 almost an integer
13         526858348381.0012482862 almost an integer
14       10641342970443.0845319271 almost an integer
15      230283190977853.0374360391 almost an integer
16     5315654681981354.5130767435
17   130370767029135900.4579853492
18  3385534663256845326.3903018015
```



### version 2

This REXX version can calculate the Hickerson series to any number up to the length (in decimal digits)

up to the size of the number returned by the   '''ln2'''   function.

This version supports up to   '''507'''   decimal digits.

```rexx
/*REXX program to calculate and show the Hickerson series (are near integer). */
numeric digits length(ln2()) - 1       /*be able to calculate big factorials. */
parse arg N .                          /*get optional number of values to use.*/
if N==''  then N=18                    /*Not specified?  Then use the default.*/
                                       /* [↓]  compute possible Hickerson #s. */
     do j=1  for N;  #=Hickerson(j)    /*traipse thru a range of Hickerson #s.*/
     ?=right(#*10%1, 1)                /*get 1st decimal digit past dec. point*/
     if ?==0 | ?==9  then _= 'almost integer'                /*the number is, */
                     else _= '              '                /*  or it ain't. */
     say right(j,3) _ format(#,,5)     /*show number with 5 dec digs fraction.*/
     end   /*j*/
exit                                   /*stick a fork in it,  we're all done. */
/*─────────────────────────────────────────────────────────────────────────────────────────────────────────*/
!:         procedure; parse arg x; !=1;     do j=2  to x; !=!*j; end;   return !  /* ◄──compute X factorial*/
Hickerson: procedure; parse arg z;          return  !(z)  /  (2*ln2() ** (z+1))
ln2: return .69314718055994530941723212145817656807550013436025525412068000949339362196969471560586332699641,
  || 8687542001481020570685733685520235758130557032670751635075961930727570828371435190307038623891673471123,
  || 3501153644979552391204751726815749320651555247341395258829504530070953263666426541042391578149520437404,
  || 3038550080194417064167151864471283996817178454695702627163106454615025720740248163777338963855069526066,
  || 83411372738737229289564935470257626520988596932019650585547647033067936544325476327449512504060694381470
```

'''output''' when using the input of:   <tt> 197 </tt>
<pre style="height:65ex">
  1 almost integer 1.04068
  2 almost integer 3.00278
  3 almost integer 12.99629
  4 almost integer 74.99874
  5 almost integer 541.00152
  6 almost integer 4683.00125
  7 almost integer 47292.99873
  8 almost integer 545834.99791
  9 almost integer 7087261.00162
 10 almost integer 102247563.00527
 11 almost integer 1622632572.99755
 12 almost integer 28091567594.98157
 13 almost integer 526858348381.00125
 14 almost integer 10641342970443.08453
 15 almost integer 230283190977853.03744
 16                5315654681981354.51308
 17                130370767029135900.45799
 18                3385534663256845326.39030
 19                92801587319328411139.87379
 20                2677687796244384203087.52849
 21                81124824998504073881728.73642
 22                2574844419803190384544450.20255
 23                85438451336745709294581778.58838
 24                2958279121074145472650646597.12769
 25                106697365438475775825583475660.43285
 26                4002225759844168492486127555858.62763
 27 almost integer 155897763918621623249276226664346.97380
 28                6297562064950066033518373935416160.57852
 29                263478385263023690020893329036314027.57946
 30                11403568794011880483742464196174527074.25258
 31                510008036574269388430841024076099269561.72065
 32                23545154085734896649184490637145314147689.71316
 33                1120959742203056268267494209293002620410998.73593
 34                54984904077825684862426868390301031843623475.17448
 35                2776425695289206002630310219593685601350868792.44731
 36 almost integer 144199280951655469628360978109406918279521398522.98537
 37                7697316738562185268347644943000493477791292787264.22720
 38                421985466101260424678587486718115935816310614908533.21973
 39 almost integer 23743057231588741419119534567705900419845239863116450.98123
 40                1370159636942236704917645663312384364387429049720377464.47614
 41                81045623051154285047127402304207782853156218361814319021.28280
 42                4910812975389574954318759599939388855544732309560436182821.35633
 43                304646637632091740261982544696657582136519509662728787385223.78746
 44                19338536506753895707368358095646384573117827333115778939657189.34970
 45                1255482482235481041484313695469155949742941813368110258119125666.54828
 46 almost integer 83318804148028351409201335290659562069258599819486505697819558094.95701
 47                5649570401186486930330812460375430692673276471701214788305128308192.63131
 48                391229145645351175841837029639030040330277058722445073547950709497937.24947
 49                27656793065414932606012896651489726461435178241054470446176414701300759.68492
 50                1995015910118319790635433747742913123711612309012803454341376306544687943.66054
 51 almost integer 146788177561136377730260697283316629866051820967150940915169721150316606892.05918
 52                11012069943086163504795579947992458193990796847590872523330083892852015565626.28941
 53                842014110931079332783532220015836831045327371743757681552681009426224153091637.23798
 54                65597557438735074090994025536102763729036609801731831230533885501204932903110195.70938
 55                5205049894621061289218474196429496131424689228776492621372505509295323231187794811.18045
 56 almost integer 420520781550767894712366209445415465306816295045705598150082071298498563284781170169.03701
 57 almost integer 34580945029640504393370721771069401074083502173524617793574204974469998047025866870097.02237
 58                2893605958403939796891998166457291344321113091137014546099293839148904090004806176427638.60317
 59                246300867022091084352691958585695642066952827239759473981562041651101948387611615902530101.23216
 60 almost integer 21320222365165370148450725694202652367764740960338726836059360731081589392256498745759003771.01903
 61                1876273323689316794552898932003548830214301148023334732450711143880068675273459337334678356510.54479
 62 almost integer 167827193605207470382244239593751346592925059206583127133257042316428312353166326463498634995723.96506
 63 almost integer 15253777976254320482680028798302887920587781886722393040786662312902435640815721495160588622047541.92840
 64                1408419189834457368564204829523128565178390518208834734403870767459047244122454099122442802088884321.73467
 65                132074759743356507265849312929567147234908587643858117900265185019357132816098895359362429954543792250.41307
 66                12575877659950554472502916897483150472428551212111494310419777827319052677957994891799291825191389852999.17201
 67                1215591474434076764736685372169536683582142127785487987047068194303571231853163199814716027890404260071569.88314
 68 almost integer 119253489850080307228410908979332327602349548167435756238732178160360682219616027578362147289795251646339953.92335
 69 almost integer 11871202870663510214658796273851712937873144830499188934497638900478929836299976837994531733424834494413388915.95250
 70                1198856785762515085433047378179373006408841522298485701569549736200218795168354301822880350705072744254029177974.27548
 71                122800516508452069087008691134168306922157250579355968528220277637680521063341532176649945604606722169422881949772.27313
 72                12755786125344990028594343963936309999106975215043630050197141450003929225895159286410920090232666677567781668116842.80093
 73                1343397785154308762546986584839633417565562502594324180627238421955041065318679414963300088378553801208008677342105129.20781
 74                143420385871167038557536299613944875171590207217392651473614091977309061787666287331580822225139240178311453147278916363.35957
 75                15518390959403568033551566827678607140363326576559695820270383745035406196325951714541143408200761091941463352557779137479.59341
 76 almost integer 1701511231657781450488490657187463365688823186902664849351256426751462495076884249622278025237121805817453644028642475582941.04704
 77                189016659826575619359816463566590900567665644326734831940281352836144822599463416848350164874138510566588334766260387690390882.63636
 78                21270085026621350415799651921499346803894882526119364175378674541895973741657242545930525335438800297104058164210172490673537333.32584
 79                2424213448788264179583061306089147620134038095441867764757968971686974539641887227399315328126639570887068398905864468400203147379.13428
 80                279792057649852783175737713059565268313828016234531102244960765250050772031108759182312138156720330707487174908389146019680769950627.13659
 81                32696023738176487003017678437152831751829536269695148312421532512438766629331326025007830371276220306319964008365309960094718678123025.72933
 82                3867972086916113699289650561448774985264647530291049879502944190830421004287744690833208017718554836505317767698155032416161235799978153.55849
 83 almost integer 463165244291537377407938377908509856879151672553650449432252233056386498022670707383927182825531345754093757771505246691827175409837333706.03165
 84                56129320888328204336159568944138094624862468930689936507266999153981195420801200053053466593736461319060626598320474363329095816269614248570.26351
 85                6883086895994794564193512707959076687449938175712429198130223275797343967727966873924934477871130478678856374089795957959702559434133276875558.36346
 86                853996798453916859286707332364040151020472833921624157042891999067692861031567447908099758601565668309504205967050579936439746813730932889524110.10646
 87 almost integer 107188954307613016000523311392809231433160190250667607690375475681677727720687471319593137707247355623545849957670511278864612577019674851693529129.02379
 88                13608405607954695161891393427685656221057735793458790720461284212774892489827559568850855692362627497225211166876726460186939885169175550464962373899.34048
 89                1747317356365160010675286089449398905752478506010237186666200281047566557280027666701240190903491017547173702078796478817511283115112530023987934885247.58668
 90 almost integer 226876147639850697035498164180665227340867432554801341517337271449480622375293088050301117860485783921944898315478899240634590962769072809212926135838226.00884
 91                29785491471736445918952653271075531391540819876778975402174887327425599452642406068862719850771752593361583104778259003866002872441102216080600657543852651.65325
 92                3953367036977750808685756007120090736939292055073874653365943059202128556342358488546018279923077047134125498022074379841398482263974194955368267319577754995.27286
 93 almost integer 530425780772737757181826423434492823619343330766440565137249239877583887700661622746755246932052806628510661960680329286085407045591157790622109718215821384486.92752
 94                71932808487165612459817313420088981223441387545508285691783916644870517242503482223932321736730312940138541874552901642390084976544951541683244641434588659192963.57883
 95 almost integer 9858825077756689892365727469356584444697619626867085315546360659481108594876947783837934569215103038786902497849668159869688315692702653014667407310567738222438995.92792
 96                1365434692672447254549334110811913069561967885901464921338743306353836651045642292387627910698830813843896593548335731977839012531770052413043446025832262165782270740.75950
 97                191080868398299691145372728810441155882257482078053536497729039741289151762976230835088279174422983509793335031628712195215374587400810640276485642254628643079607175392.85664
 98                27015799282204393652757486182039839007928724239614076724115229683229744474973964185696283577921463067421150497137337005875520322135625742598745273146982124791039386353415.38534
 99 almost integer 3858580405358701701178592349999764119100383919493174924234254289226946169670282634143611442423604879965913160770963193114640928490544697626354358018070202709736290269337380.08317
100 almost integer 556675481568232514984982818042190343148558399534414077721290961776960540637206877790763456361060917814220537590278336850556859288553005678671299997903793583385604976819964755.04034
101                81114408620939424986242514379582957449016424524099565267891545593328093462760476004369461266098743327042738370557610164286909371887321059005699649656712559364600524668046794528.44599
102                11936382216331169543459489823962269247340103082621860186296631343330490903201372446908668847970901657125884938029330188049336491356935287124813184542465509756888606391507242520194.23361
103 almost integer 1773717621254587807345808358381304237732498110887124158117318309098848032800262430393573334097474125584687058988428219977893011195082866094273627682650470062670514333460416907621582.90899
104                266129096076621696587457570500527320905565521509692908178076934476696035458451901526371083769423579154339533869506520423226593390860325287170949115309735105100004938678805354930046450.14803
105                40314028350330484015245036711230598747596286811487007250653044228411079693097670122719321789136650982285647520969570440822516442552260314433245938212396974979161155010282225695876805160.16471
106                6165049970603559971568615064523283028584617037618201386263234271314730434154454509949110928686550351412525867537013817108514711812479711616073350289020590644957335849472737850469615043925.64606
107                951688711078197399813808296545760872147628716236561389481424327075244763209386401065062331631986034894116821764081819058541983719890778797125461842371654127768373378615484518917157035254556.54084
108                148283631065792687061419360565375379308559840321757580470062533908445626372097828715429287981460169451241740535123834324194616768608497984202142352223761497549980831803636384122583512842707655.33220
109                23318158451015424228950671773583457159746952971274726427350763676476539678958570327282116705779386714745323345804539112039982994738995010310011179032182763747601490575140661761177951120275275414.20366
110 almost integer 3700509071593732759743387879122534531093459460019546885341496988364005534089830811680294792747194445415434375962412398953950450394440991138741768593093575004323295113512001416462649321025727206777.03051
111                592596375585171933390750659287342641654953297691010994963323859733044942221570118622885921630262738836076821004543838157482529421781108680909554473521249711877254209263603384825050196032113797263696.34724
112 almost integer 95752815458216127576890837767142061080070498026215229450388035105997326196134526961277649242964336060360357296674263698547346506504900657846222052210432065624714479019752542356577276908638116789448673.95076
113                15610058657437866646486934261623564713499145696125747834762160156265986835065327251656537539201315989971943162457656519855358769881410817736735403605534922557214764577081238208145608789268376863146495730.39713
114                2567343180290144188759052538844932809564341142956576847230230487853327344139915083551758055830823684090597642898626979213291475790727266282798189612798100442732998346450643792139372114338488731182587049563.86795
115                425947726563439456193357598283577430239944402421297999785709691652897108771516590505203130432105264038019852624481756516667266902102644075604636778483744921373972236321486367038793538738854208093492028088565.17068
116                71283470043756265773806339672676821238242181932992368764625814367671298825779365158678502776867824272395232669215251372909147291139380532044974951121123207441178223791555965821296957200406804842537713764008645.84444
117 almost integer 12032316121349644850261476325442362736118338476108342105408800024255837142801156909123609843739117921351404061777680000018958582197196407532758363447141948123030690218513263569153882064669617607059311406089315336.90537
118                2048357610244176217330259927929847195453492343074137059661813165405040134253938592377265334895794412518463666025202476107488959851932569276952996042106178781645512010209168334419132358293725616440374103949203978524.76656
119                351663488585706500220392195482117025377561436720676055038587055644194823743159881388779433156511639282333484629603694972572921812890322525081218896524486295705268021137592791746181973698521613516225076361190717472541.82289
120                60881180525317362698513562979673962450025251047234028540125599471773516387024940354882385257726476473204265820991508715368377388906337376806686459529640985898339445398031822351297800244319449448813273107679917409516460.58411
121                10627790244508272528263788704322466474482213858819073204023396314191334618336896960206146402613463406035641455746398372326998207611674498599407823545810401380537417083650327674133141270259979345581373425266938122082191401.24869
122                1870584554325943014563006390580938172260407123002591745530970502010727043208383927087458942432087733561430123492538962350460691683215234200910063673621903226072403136121459096743264021915954387839998790722854720867499522059.51197
123 almost integer 331938016391012159200939423808212256954486959317325379153498995020380297837206876083071758514294279289479655646269592221929040450657682954776842594350306680674708239162806939968379542746178247931908432124439231430446904958884.94518
124                59381780936099253900024661626072197009857295709261723604551628599957794012202410240026900090302502263389590677759710488195460998699206781872870740681011922997152790163436589714940766411485497067266525120301159315132621943217694.16327
125                10708725109458147608165205725435802215704214844760298635570819965684624486627412080715266124791151793287808468866379936761039506767764805818963962499187469108991484372520189443772190361643170492716242748101112374819071345802032878.11885
126                1946627500816957316862735389502281981716828076909629690407535950224352728592825798896739822054566370290647926964588299879354885209355036220711853610774911427147405543829607921589470255735029312123267581915647233921115103765346004441.27874
127                356665509919611012384376913461143176864132405138530009887254419313626010505703984507267604908662754612386567030614629832375793684171422171439587289040402236399944896905102473713814773343335971318810379308728487064165088061196487012629.56912
128                65863623989395992740124360717699657982839698584715608144791847453230524585085759181826949515835116573507176771403177301606042312078030471071888582685211011536060589922300265432676819051748436156504108373566950976993584687015595405959621.55179
129                12257724957877528217291429249369099757060427301862538465552411576010691795696634260205879000405735350780182127716440309555705462438537642240526383539494737345126658052456772742388107648219305834033668796254389667736167544250454255901818530.46242
130                2298940671210402418105657910981997459313944147851439632728905908873193349059129764773970832226363706618646944685899693812935317708260050606544167718707995193159757381151390336002964587250907772172802432439332278112472829320919830152236196376.77989
131 almost integer 434483810040568217159734115405472141287609929559445180855862969158818103113484334418341406739576386365148427335248295576038177978563092484396569247490348587405453357084979961639084186436157378222072543299291467575928141891356731691270881628528.02298
132                82741248228153262954057773673730030611283904675989680259962714117411595883906951844757357736952235912832587373353965463643667005832840759304445904790252558341099425225062876948903674498628150667862804330300848091618473057457502383900726307128488.77321
133                15876261669930685889440397986313869878743245523736989306112243867485545713974518175619363175944865701658783526093610246983539669560898987523270857179648987194543905850842365655707017358924167069000903325455983663525581946869138633792065926434901438.23666
134                3069216933194647468462222127254056619887822603880400428509346167460486251364431718575779562306542640246031542819774224225952518884472271550011794564583535798331098565401047954541850212839053240557469264776149843335095015280622415383162754832235136184.49124
135 almost integer 597772446605867358021397017199698115152668557436242478624117144785666140988452016755857329055183003638912539958254797455780798704347986155374919206190129972960512094944774366058552165781746481022774515984514316460920816379717172299891546839575721041339.94477
136                117286854824575260445536568550437995938000212919475892926398411117464128133471718133308608262700171765580275936647528331081654931146788619804680436984702308754700102260981947251843964607079762838591919720324600104139011044169905668080049454521018778990097.58267
137 almost integer 23181655442912357302316784788025853877080767602012790530291610199635654201205240854990983045336644322793616019066562766998722182800308745766989947927760634108987435605025929926479134421187199097791344728772227160390884825359243093524640268709056468645764314.98542
138 almost integer 4615280189897910013483874023750835748597618984464840101947048537983598920251721222529905475263687103741818933428861125367171977941831323673631467653998042626372184979884067134452552273359962896842365510099790551827599944932235750929263901833454524597688104091.02264
139                925523416076751544002917504374040362136244811470543300813668700399559505181858582214600534319716250300109073015079232639945499640345390871532302750672882246657852717054558574294189478776177176846450415429643518924627120062549889134798066077605632651756877907628.81045
140 almost integer 186934725964075902594927204560221922085969293947470908896819363007093040377758711566390296620565298857152798563564056520601988894908654597816755920667108060781261246706284370433266258089950973112115142078714251962303366015576187328097705702023541560504586724922846.07644
141                38026262098681661186384205690327525101407436854637223015330813229067707022878468254480507804768525560978262710512065180265715782489940129125971190413927951162359704963971312604258695645983663167371166091964933011768027236774790932949490442618701211855732172013564488.48496
142                7790162564970300969288691908792766839406438559991969830393442450335565202895452083886450857185816396699466683100621114038743679819061734337398353978208489150664399388232152137540785594049487420615427921277122155398735407304006474770711539288410926499056211525468617200.63357
143                1607152532728814559108151606007439999191121780095423089313359857324291486973513167190930576475618060338455999952705709410097310076733024694550833791333564258005049475138013291683942726327641970883538288927017438705420326148460298723770251017095717186949595008531997038160.70147
144                333882862404479743845147408828684693537435802265676518564307659991695421126033106303577150656155073103889598936841473799200805297702209366557994796969121996433896242382533282336596673453543702403635541030417437496472331066269319984000926983494965312020566875109448739465735.19528
145                69845216725169481850965205888599442488694626899984285216809790758005059208253039374308508998881189677965533327414382535934484130726432434071372479309566028814424037179341402341594628057353112401691656510812584932397658657822866112485400744846469835220059229342925780148422144.50109
146                14711740778685666885139261313061044455515206836997347024479132923675111701671268598677009107091025176503782114365572906090794826594499583585292716848702468215413178533754439431310017771077393718609730240524654194459140058678504038367768960672045353216264051444869472573348509080.39306
147 almost integer 3120009653245301036129556532211329257120397502374103300648918370097718733496613213116666577338957660501803703819985656436184743823798779061582697830471860305341787298433052118486970534497005167734377565171298096515927259570870366054767430105610386560634274821870433517740058206205.93043
148                666180923230878137830414241408144935088285984624081366577273304454344039776100919154846239859264419055860248552872536868249820747399404157688963013669180874255972166549677168453341797339366224910502735118393846709165723481015327996214702394425158382269122010547831246727560575803368.69243
149 almost integer 143203291227722850031155646256862209447729113591002376247444546645535989285187860008550750462141963859545911478560400362411196583026265264941713807318345977172365546177639121425603366376945925505899176677609598618757891050783857896663444352920590969295112396791145394860074070605942978.96464
150                30989801713982062792434631927766259065743852346381353424655819468138215874397172529395309518244577736192554160906706019950120882541695044517657605104205361160853936162041446040804841628479633462249454750941098914991558994800055110823193129657330134825925771119695294364934131171578277211.86520
151                6751033820885027275067272158501710820224477291596666820140278220759670988286332280849821525521779400199079747885945949717520800713168267936131470860990593270248090549037001175990610734016515324284675818324027917368764147687961521706115979211989820989673804436189131917112050203095317873851.31529
152                1480431818168203891870287804843184873937879041449750391843965961804860626346490048124930633467391764945316451110286306128085191544389725974837198845173815724740004384768959593359376561918234498793028251118274454695329622572056212519670370105062799915152080315463353822495069644198759302136867.15345
153                326779181294161401154780774576319502506058456803961724679673948298027377083365384654284835663797790769336091151633470256968508779508644550575235042937154358240044713756956859672078402919023534840148083665222263158931136898528152735474645078364080666405552992224156321545389607390847392314469898.73597
154                72602176465101694004901503616793052682855926956259480319034656717689680850314879266865240871104084097431007792109084621290271066656333360031646400142421772848790412933664796780081712665269831629025137646745379116823290861876458330127638140913114730886962821051660888766168022973464094711969060909.13190
155                16235133991311881907338032643044452802932428234787601417304166592301609929567611017874830293973748714363590084283668417308148101106520292361975061079561047469069669896906073761876358816402423087300575017384369590147797240707648375531622320896078804446154187805466344721631282144290630300483195600524.84459
156                3653886178399625243812442730999484119324407061199933147807976079375653071438899177450573129322404111005507881331131643516239447979048497158368196323256390104855469886767561507306152001636194017056103393187182141228161643495982917194676297916242749065422653559965557272297357023851601055649202917502578.65806
157                827616624719328896744907209058776918488781262156240644152032589316310497917704278573041113320597973119052610891389955361675777280300864839798359318495709104405786183612422601907783121589048096310881843679773121995685487830798002687709349322579493477773619254140840812652652469623324919939156489093831587.22972
158                188651747237894417613400784714319430164487181309579572110462632930512663014657539519344465377549109559228618759073925619411673901972744772786479386562066146971411513364087599050195564367675620465382731491389020259045701812198099022750166591979801131774098298537336944673630511186276533671731001239967223800.23746
159 almost integer 43274543491028607741803469061010767430332337084204918320429970472522761733456850206230925917936911783068676793548484810501547707312243095701424289819741084975826041304289858217729559322645951397447088012570536933829919072607637917254177914793769476183640402383097529542299702743634513731188261671332932254628.07663
160                9989115086598518803349681088519177460149178912488412410905059070244738604774689634274070344436555109402659279232110783535274490750358665151829196799179716495498826260758985902415268744476672973019520046160095133088274392471418309497263207396112897853003214776015310211027892792035832782966576587524663300243062.25586
161                2320210734527074624586011355806062683946622201634170565761132603458136065426380617916539493562484900112525539171909627978684218622477066387351352845528355000246544687019145818030193274227941652190418350735454909748123225319064463065002910202358153633808321795173996711047052884364401107319960844624662750865561963.20776
162                542271756324167059007254730978145324569124764875811981803578051577656245145879252641769859044467735573174592878116902045688747745079890277328347439794926681405621617981500180835424211252093255259903866534501865120685941658015737501895542311796479627598797326235343292210998561711225288780556898235001591216862725682.35473
163                127520242107072944009636177539130894358409589467102276260639790163371648861969709950516325279197817888285773926484472321283834216795388524136788659362779131237642675745397364042721604539976866655177493575034472745318348010012382781078243724022664220550101966613289531158142222329055683803057796155184173435861289973823.16542
164                30171542627736794724169612701427008582854369260042238387694742648058009964996834533589653680196239838317041984854026336281448496304993967416452879847212774724439333084375639673369302753662088142090325545010483165187174455160308639000197727945458754444704770401110185737095370647953529247377682732926103776720308628000488.33801
165                7182175262625963189763104517731807198163649605294105861716403612516532133687002375109909822741597959894153507370918409234589663667969892331293771579535970230044905359683702097584790839563641516580452271152663054524797454555329441341659482619876707261768739390251005189046412826383903234673140562013460009392441624262637220.51842
166                1720040313274853666827115335939091650739208250799593023794829861952587626343860099432952148698888990897295690628134387270888413342083800245117174234831381625095854557797716804813993012909304429681208415451198686479714240349038342604241143810569834775743662808633598741043340892803617600895299198196064399721817770044370505112.32434
167                414409436225151983403603852149304547581842164354292978467632632560401997303344674386242278831025612457704277528075242210607785266248344344663661851587833561570013662143232324419788688671014374221536333447219660646579022588058146410304314573551354314922876516430033902532996487838990672869313297667368392570699069082842853436153.43428
168                100441561674655809580788161810936965607189711720727416661659064246699501831104282085382730092104867454351751579064156729879126294628818833387603865089866762699959202878446326748308584017662836802828135227244735619776439091590813877717579820222829492638893857207642330137085201207594855199600919206851477807987505851278970154472451.57729
169                24489205754691544623264832329594550104750853925618866853382162849683652515715090977664067926480897235302073129364480701709338396134050340013671850496093926231108425879351359460445875226843014928838964575168673581203100915797227282539217075185492794469982160707903045326677772156317343177126999667581083264403045609175732466087110246.24208
170                6006177468592502511355791298395715124982596151228973649401523395200900647133762244772091883935027971890070865975758343168759452961489649354858915118944128747601433827214419713707475109799941113127161984233038247249072766616399402313589555085097246982564733515792318111675980988024732784902601817651552719755556317693182733797695765534.51567
171                1481729098717000725283529050885774872123071341812319576351243601747285494557142539758067809356317663991602721366785252212373620468392461313042826938754215037253953762298790184170482669666995837879660248346430492047604605587870642265092964724183636165425471369178296552024714764261444814501846303803630123301434196854497920587387583186006.24709
172 almost integer 367681514297501124448715601012426383418150674500888613542161292058441474746316292257972416503541790335661531397785637711246410802747777691146646712451135574736900873957913842341015573640963581196399239954444636805487102152839329298006952033850848583741621812385868444034816812174342274343569782647775261745518433501670112266497462622652825.08196
173                91768247433514040734886437712739826078327056113306090031027099378379697640778177531200378293326294001012074268214026085595706915762156548630348782467866354607272025890590788082362564218263450123188287557021163420246318837169541073750480646617268384295217290699246972624245520821118907213537078219699714335106753896811467034738897315692936899.58903
174                23036485614111956746027162683618244602188820378803609032834216340093032087791810103306425439855234901089463246693708577281212859686726654730377311932172439126837982309765376053643189697657268158562871152504039902114886290015033818044134122625123278803570210856199759147225205941101842897498644098638248187067196282046465293313978644913571530147.59048
175                5816059122123121680778686393841149006273979824960504151711449238828259896339226553746785362471111776362242260455372409039557066074932750066305507622517682852364746763784297116912561779957570132184065907900619877187659024982961072877956548847161227025770393481155605345571132796031195822949077909287637273416921421363974482535299551785573674582078.72344
176 almost integer 1476780738928712034890198147146225599087596218552173442905221528436311623561444583662020128478087399864056039375838911024176231719307227165874467990904946775525650858998573905129478687474298711472839349006770880844086171038332612991615888598251568768584342847973282448668573705812314159657483183512324566105008304534750188963184100605497214008671621.07247
177                377106331990304149294896872050092930132116474753599922954529326802619413622971225766742702548754608609454708009827853436034438560140180540820383942469119841599328216836126996739352417299859793224817469350629506481034280275531770417163692882611783311386501693801643067930256050022975052203140081254231931736809926089274886361570213956235894636325499176.67892
178                96840799438942516030204230592888799221679367600797051179685806535473753548848641415136212271487572370153155280101458026640117710046285694419846676046547800720261388350881018230216261724286852209164827386783490240838364598265378754342229605496863591198587321905187604954540103259350234581106261635706160273139601897912303347023476384552515588948963571021.64996
179                25008401658025028919504962351581650606718047972297435681206563234864966573472931972036844140298996719126410658861248255504209669079596565082464939724765420806656242814938544134981993550186199251246672467872316851725281753767080796912780228328476467753179298309639610719108374758363655571985655559384063598204751405597578996842966497571138748727170348735310.64768
180                6494309469466567086342579845166376654935638986889609805298460864216507262095371598194373427571474926401222709972747664258580761067238049192677039572270457032465612687641160178983698173005216905899553958944123869822741100156768013260839027819619655090511856209219523061125380814521071117015048386970255272717052892881640243860526925509274745902515782132164029.76749
181                1695844759873174877285413377396817112669976741852260874200735707930334414192385993144414320988374494074378133115407091196613356125414993181046323854465020908495904895147058337509986843627730499298353430773472189950871794159444657661392179137667734766539478680306504586964527022381980737868623219614528428801921341705267857071997284981919738882437758101800013566.23523
182                445278802183954713629674174603551191654793853953148417270972255830999255160587007015932893701984750316171858905551248455842900867393174289396318458257456649020562122497170030803606592059446271396777115344453280860683671509737991107400289350554758286057586837050548246295079508314561202637063386195384236520417911229084708300685612840401045338651700916470083111481.39278
183                117559478109449762529952441563012971701433703710113343869704738204429351330990686746535747417829590052672540165173788500956291558260467455659340936798584747326874941226279758505310846091173621461084852500063050676770262026365269184990837020957169217121928771299228077617085981609324939863489293440558300157592667251794268189151172827613899261547989348309284302129465.23738
184                31206855598351599571613896444740010235823585807731722934156408627299605903464873284476090077121160984024354654267890077886380630249203258036129476015582192583529852444687280126694739379960323785325984725662202495624155397654135410622420806707802094572556751729398572449684179554529600423613022435251636670238111014121333403517490953093478147969463086642979756276284930.41255
185                8329065525493769949290655372038740278137351458379387272349672875965256974983771970375346747860499359955301342221761168768456057562657886823790331948991674160490509197405680200830606364480811167749317168640189980434755459343147250606084552601443594914422160458120218410355324926949800092659839971905367805947579718202980213757649571918539556571055577265322074490622441712.56721
186                2235032084369650726312204826316402353778555601746014369964473249023457712114934392384632268442469841076649647640300893152103005353054229432313330332672250455274590082608407771793758050779915654193457402038846489936505607515051335018178771316058131098014777471217386823496003058312198413572725358633469583468689482568351970425927432579084454047967667131694944066288632557211.25494
187                602975834713042034565983760390962608446431303895722457995414678890188031470369266019780074929626688611457349647474497917267528648892995102545678928041346048494399199264588012170290495187441392359823534205306765303476437562787104379493419043960492139460205528325742158264624472132202641785816589155924637114889169577288885528885039064855426102173623196865078272518812522781388.18523
188 almost integer 163543126345081135602466137557749797590586206478282853905652970085911912813158883884068533840548392490115699283608583721860533629613601753080576003315822331750679238035668377926389844065259324423929794560628523358971363428091752799784920258264095085234001939293781297844267730989922619757055777459014482005577310277003828763433825158703123653048113733432476739802104790585163697.07762
189                44593200039060364402102763699797035075668618780430757051572667376752268447817349238487814866546473731608722211688933951926851146905998770106131622809571161242441299360784008808002750154987508653648044543859518823284823573194254364938185486290451148539674806392621473401832449669703857953258707619516982055147506880075692250258927074876203616210272628015827010314526899890875372763.40413
190                12223533825207164236709289628526713711069819006290621785574070251194627588208132432145236203776132867004016048262192760092224738967672588039432329243044750419905108012710044731764354402919010639476252488807281881811659391434938893283071701692564866778899674179465512541450467047465701698223156593453222314161976352369107570568436392889020183382101259313707110811949466236803131876690.59385
191                3368252841667091525336254561443385846396301564470520256678533308760465357354817346323732580835898766758914068846133814314211303682039257324794096983662063419228045403527024400705335469880788169232561608744965635532537199883362293010613618563333514597666656553765101973336837059394780382142727422589059170040675086965790207256344779802469598528724647899509576935928934277023431740544057.78753
192 almost integer 932997440857588185178900869059757369538699967004525686039278814096413151816173030647842747305719921637293430570379820797507833232778321392034195309952284583489560078533696809574016963567760015515665419179267879307962345099511334826340353301910367705105827822710052806327089212933909151983339689407897810158756292306505647881853532693367832940591380018181127870675633979870981783490894069.99807
193                259783940749855926204189267140666406648171997016362190076968024884191920478871863745049203419286917117047865500302802662670740620368419829656778308248004754077183230330662278572677059682358974922306973068691887802341833208261510955958920871771985300506145447764082200812075032916301076421225725345863862138486689778400620147987819478193432082296181248984982596711555129842753347115931149703.67529
194                72709066586347431863579495206220499035405827729562949784089657445038184675939091263524059320871387677161439172120061124957114929788756760510401924340830740894355500172745495949404576950296521181414894394095194671955046816327059445092851798945679004021616461986863451490409473076458605570450859236721071700831215540774296947468474322510928650085713992079160373287984099781702113981409085350037.17803
195                20454916909398829892752215164920989897208967508911145569541639071798681151367986151586637417168878982759356217949945115064965970993199375332927173439923870829104067936753896348357732166411910443832892811336800002927524585573115237023068345456169550408501590655925574970453057389125931654011665876389108586726951383759931974258179854734476370812870633833368156050966001274942172376159862046835677.50930
196                5784000608649156804619652565433848911508149743675643946536150725436532389116172322612741989340830339373306970661331751696655753418948276412526377712932882811326796973392139574256671911388075367805652556711180740588202536114918285200690316037231737291427576180606631821287075764855299754138094663119369564089069749821374920588257646142446777715914950624540907820274843935902766556895110704576908045.69749
197                1643876151935586248754399939921773116810182486644547522574931394691560184496243423318050284392287070630741841933378019077143506399048999971329130724719290951693185394006413229467436103698095895857668552124021504010516518577784964721473716632131171833401903867648995563495500287568657134216864406410565598048745050529812285114555953856738899125594156828764334624325053726883824339337487192462825547564.49176

```



### version 3

This REXX version takes advantage that the Hickerson series are computed in order, so that the factorials need not be (re-)computed from scratch.

```rexx
/*REXX program to calculate and show the Hickerson series (are near integer). */
numeric digits length(ln2()) - 1       /*be able to calculate big factorials. */
parse arg N .                          /*get optional number of values to use.*/
if N==''  then N=18                    /*Not specified?  Then use the default.*/
!=1                                    /* [↓]  compute possible Hickerson #s. */
     do j=1  for N;  #=Hickerson(j)    /*traipse thru a range of Hickerson #s.*/
     ?=right(#*10%1, 1)                /*get 1st decimal digit past dec. point*/
     if ?==0 | ?==9  then _= 'almost integer'                /*the number is, */
                     else _= '              '                /*  or it ain't. */
     say right(j,3) _ format(#,,5)     /*show number with 5 dec digs fraction.*/
     end   /*j*/
exit                                   /*stick a fork in it,  we're all done. */
/*─────────────────────────────────────────────────────────────────────────────────────────────────────────*/
Hickerson: parse arg z;     !=!*z;       return  !  /  (2*ln2() ** (z+1))
ln2: return .69314718055994530941723212145817656807550013436025525412068000949339362196969471560586332699641,
  || 8687542001481020570685733685520235758130557032670751635075961930727570828371435190307038623891673471123,
  || 3501153644979552391204751726815749320651555247341395258829504530070953263666426541042391578149520437404,
  || 3038550080194417064167151864471283996817178454695702627163106454615025720740248163777338963855069526066,
  || 83411372738737229289564935470257626520988596932019650585547647033067936544325476327449512504060694381470
```

'''output'''   is identical to the 2<sup>nd</sup> REXX version.




## Ring


```ring

n = 12
hick = 0
decimals(8)

for i = 1 to n
    see "h(" + string(i) + ") = "
    see "" + hickersonSeries(i) + " "
    if nearly(hick) = 1 see "nearly integer" + nl
    else see "not nearly integer" + nl ok
next

func hickersonSeries nr
     hick =  fact(nr) / (2 * pow(log(2), nr+1))
     return hick

func fact nr if nr = 1 return 1 else return nr * fact(nr-1) ok

func nearly nr
     strNr = string(nr)
     sub = substr(strNr,".")
     sub2 = substr(strNr,sub+1,1)
     if (sub2 = "0" or sub2="9") return 1
     else return 0 ok
     return sub

```



## Ruby

Using the BigDecimal standard library:

```ruby
require "bigdecimal"

LN2 = BigMath::log(2,16)  #Use LN2 = Math::log(2) to see the difference with floats
FACTORIALS = Hash.new{|h,k| h[k] = k * h[k-1]}
FACTORIALS[0] = 1

def hickerson(n)
  FACTORIALS[n] / (2 * LN2 ** (n+1))
end

def nearly_int?(n)
  int = n.round
  n.between?(int - 0.1, int + 0.1)
end

1.upto(17) do |n|
  h = hickerson(n)
  str = nearly_int?(h) ? "nearly integer" : "NOT nearly integer"
  puts "n:%3i h: %s\t%s" % [n, h.to_s('F')[0,25], str] #increase the 25 to print more digits, there are 856 of them
end
```

```txt

n:  1 h: 1.04068449050280389893479	nearly integer
n:  2 h: 3.00278070715690544349976	nearly integer
n:  3 h: 12.9962905052769664622248	nearly integer
n:  4 h: 74.9987354476616001276345	nearly integer
n:  5 h: 541.001518516423507569202	nearly integer
n:  6 h: 4683.00124726225743718046	nearly integer
n:  7 h: 47292.9987313146239048228	nearly integer
n:  8 h: 545834.997907485167067291	nearly integer
n:  9 h: 7087261.00162289912097918	nearly integer
n: 10 h: 102247563.005271042011088	nearly integer
n: 11 h: 1622632572.99755004985287	nearly integer
n: 12 h: 28091567594.9815724407151	nearly integer
n: 13 h: 526858348381.001248286180	nearly integer
n: 14 h: 10641342970443.0845319270	nearly integer
n: 15 h: 230283190977853.037436039	nearly integer
n: 16 h: 5315654681981354.51307674	NOT nearly integer
n: 17 h: 130370767029135900.457985	NOT nearly integer
```


## Scala

===Functional Programming ♫===

```Scala
import scala.annotation.tailrec

object Hickerson extends App {

  def almostInteger(n: Int): Boolean = {
    def ln2 = BigDecimal("0.69314718055994530941723212145818")

    def div: BigDecimal = ln2.pow(n + 1) * 2

    def factorial(num: Int): Long = {
      @tailrec
      def accumulated(acc: Long, n: Long): Long =
        if (n <= 0) acc else accumulated(acc * n, n - 1)

      accumulated(1, num)
    }

    ((BigDecimal(factorial(n)) / div * 10).toBigInt() % 10).toString.matches("0|9")
  }

  val aa = (1 to 17).map(n => n -> almostInteger(n))

  println(s"Function h(n) gives a almost integer with a n of ${aa.filter(_._2).map(_._1).mkString(", ")}.")
  println(s"While h(n) gives NOT an almost integer with a n of ${aa.filter(!_._2).map(_._1).mkString(", ")}.")

}
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/tNNk9jB/2 ScalaFiddle (JavaScript executed in browser)]
or by [https://scastie.scala-lang.org/aX6X59zrTkWpEbCOXmpmAg Scastie (remote JVM)].


## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigrat.s7i";

const proc: main is func
  local
    const bigRational: ln2 is 6931471805599453094172_ / 10000000000000000000000_;
    var bigRational: h is 1_ / 2_ / ln2;
    var integer: n is 0;
    var string: stri is "";
  begin
    for n range 1 to 17 do
      h := h * bigRational conv n / ln2;
      stri := h digits 3;
      writeln(n lpad 2 <& stri lpad 23 <& " is almost integer: " <&
              stri[succ(pos(stri, '.'))] in {'0', '9'});
    end for;
  end func;
```


```txt

 1                  1.041 is almost integer: TRUE
 2                  3.003 is almost integer: TRUE
 3                 12.996 is almost integer: TRUE
 4                 74.999 is almost integer: TRUE
 5                541.002 is almost integer: TRUE
 6               4683.001 is almost integer: TRUE
 7              47292.999 is almost integer: TRUE
 8             545834.998 is almost integer: TRUE
 9            7087261.002 is almost integer: TRUE
10          102247563.005 is almost integer: TRUE
11         1622632572.998 is almost integer: TRUE
12        28091567594.982 is almost integer: TRUE
13       526858348381.001 is almost integer: TRUE
14     10641342970443.085 is almost integer: TRUE
15    230283190977853.037 is almost integer: TRUE
16   5315654681981354.513 is almost integer: FALSE
17 130370767029135900.458 is almost integer: FALSE

```



## Sidef


```ruby
func h(n) {
    n! / (2 * pow(2.log, n+1))
}
 
{ |n|
    "h(%2d) = %22s is%s almost an integer.\n".printf(
        n, var hn = h(n).round(-3), hn.to_s ~~ /\.[09]/ ? '' : ' NOT')
} << 1..17
```

```txt
h( 1) =                  1.041 is almost an integer.
h( 2) =                  3.003 is almost an integer.
h( 3) =                 12.996 is almost an integer.
h( 4) =                 74.999 is almost an integer.
h( 5) =                541.002 is almost an integer.
h( 6) =               4683.001 is almost an integer.
h( 7) =              47292.999 is almost an integer.
h( 8) =             545834.998 is almost an integer.
h( 9) =            7087261.002 is almost an integer.
h(10) =          102247563.005 is almost an integer.
h(11) =         1622632572.998 is almost an integer.
h(12) =        28091567594.982 is almost an integer.
h(13) =       526858348381.001 is almost an integer.
h(14) =     10641342970443.085 is almost an integer.
h(15) =    230283190977853.037 is almost an integer.
h(16) =   5315654681981354.513 is NOT almost an integer.
h(17) = 130370767029135900.458 is NOT almost an integer.
```


=={{header|TI-83 BASIC}}==

```ti83b
For(N,1,17
N!/(2ln(2)^(N+1→H
Disp N,H,"IS
round(H,1)-iPart(H)
If not(Ans=.9 or not(Ans
Disp "NOT
Disp "ALMOST INTEGER
End
```

(untested)


## Tcl

```tcl
package require math::bigfloat
namespace import math::bigfloat::*

# Precision is an arbitrary value large enough to provide clear demonstration
proc hickerson {n {precision 28}} {
    set fac 1
    for {set i 1} {$i <= $n} {incr i} {set fac [expr {$fac * $i}]}
    set numerator [int2float $fac $precision]
    set 2 [int2float 2 $precision]
    set denominator [mul $2 [pow [log $2] [expr {$n + 1}]]]
    return [tostr -nosci [div $numerator $denominator]]
}

for {set n 1} {$n <= 17} {incr n} {
    set h [hickerson $n]
    set almost [regexp {\.[09]} $h]
    puts [format "h(%d) = %s (%salmost integer)" $n $h [expr {$almost ? "" : "not "}]]
}
```

```txt

h(1) = 1.040684490502803898934790802 (almost integer)
h(2) = 3.00278070715690544349976741 (almost integer)
h(3) = 12.9962905052769664622248845 (almost integer)
h(4) = 74.998735447661600127634550 (almost integer)
h(5) = 541.00151851642350756920277 (almost integer)
h(6) = 4683.001247262257437180467 (almost integer)
h(7) = 47292.9987313146239048228355 (almost integer)
h(8) = 545834.99790748516706729104 (almost integer)
h(9) = 7087261.0016228991209791875 (almost integer)
h(10) = 102247563.005271042011088389 (almost integer)
h(11) = 1622632572.99755004985287486 (almost integer)
h(12) = 28091567594.981572440715189 (almost integer)
h(13) = 526858348381.00124828618049 (almost integer)
h(14) = 10641342970443.0845319270951 (almost integer)
h(15) = 230283190977853.037436039126 (almost integer)
h(16) = 5315654681981354.5130767435 (not almost integer)
h(17) = 130370767029135900.457985349 (not almost integer)

```



## zkl

Uses lib GMP (integer) and some fiddling to fake up the floating point math.

```zkl
var [const] BN=Import("zklBigNum"),
   X   =BN("1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
   ln2X=BN("693147180559945309417232121458176568075500134360255254120680009493393621969694715605863326996418687")
   ;
fcn hickerson(n){ BN(n).factorial()*X.pow(n+1)*10/2/ln2X.pow(n+1) }
```


```zkl
foreach n in ([1..18]){
   hs,ld,isH:=hickerson(n).toString(),hs[-1],"90".holds(ld);
   println("h(%2d)=%s.%s almost integer: %b".fmt(n,hs[0,-1],ld,isH));
}
```

```txt

h( 1)=1.0 almost integer: True
h( 2)=3.0 almost integer: True
h( 3)=12.9 almost integer: True
h( 4)=74.9 almost integer: True
h( 5)=541.0 almost integer: True
h( 6)=4683.0 almost integer: True
h( 7)=47292.9 almost integer: True
h( 8)=545834.9 almost integer: True
h( 9)=7087261.0 almost integer: True
h(10)=102247563.0 almost integer: True
h(11)=1622632572.9 almost integer: True
h(12)=28091567594.9 almost integer: True
h(13)=526858348381.0 almost integer: True
h(14)=10641342970443.0 almost integer: True
h(15)=230283190977853.0 almost integer: True
h(16)=5315654681981354.5 almost integer: False
h(17)=130370767029135900.4 almost integer: False
h(18)=3385534663256845326.3 almost integer: False

```
