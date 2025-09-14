+++
title = "Egyptian fractions"
description = ""
date = 2019-08-03T17:29:58Z
aliases = []
[extra]
id = 17475
[taxonomies]
categories = ["Mathematics", "Fractions", "Ancient mathematics", "task"]
tags = []
languages = [
  "common_lisp",
  "csharp",
  "d",
  "erlang",
  "factor",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "mathematica",
  "microsoft_small_basic",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ruby",
  "sidef",
  "tcl",
  "visual_basic_dotnet",
  "zkl",
]
+++

## Task

An   [[wp:Egyptian fraction|<u>Egyptian fraction</u>]]   is the sum of distinct unit fractions such as:

:::: <big><big> <math> \tfrac{1}{2} + \tfrac{1}{3} + \tfrac{1}{16} \,(= \tfrac{43}{48})</math> </big></big>

Each fraction in the expression has a numerator equal to   '''1'''   (unity)   and a denominator that is a positive integer,   and all the denominators are distinct   (i.e., no repetitions).

Fibonacci's   [[wp:Greedy algorithm for Egyptian fractions|<u>Greedy algorithm for Egyptian fractions</u>]]   expands the fraction   <big> <math> \tfrac{x}{y} </math> </big>   to be represented by repeatedly performing the replacement

:::: <big> <math> \frac{x}{y} = \frac{1}{\lceil y/x\rceil} + \frac{(-y)\!\!\!\!\mod x}{y\lceil y/x\rceil} </math> </big>


(simplifying the 2<sup>nd</sup> term in this replacement as necessary, and where   <big> <math> \lceil x \rceil </math> </big>   is the   ''ceiling''   function).

<!--
This Rosetta Code task will be using the Fibonacci greedy algorithm for computing Egyptian fractions; however, if different method is used instead, please note which method is being employed.   Having all the programming examples use the Fibonacci greedy algorithm will make for easier comparison and compatible results.
-->

For this task,   [[wp:Fraction (mathematics)#Simple.2C_common.2C_or_vulgar_fractions|<u>Proper and improper fractions</u>]]   must be able to be expressed.


Proper  fractions   are of the form   <big> <math>\tfrac{a}{b}</math> </big>   where   <big> <math>a</math> </big>   and   <big> <math>b</math> </big>   are positive integers, such that   <big> <math>a < b</math></big>,     and

improper fractions are of the form   <big> <math>\tfrac{a}{b}</math> </big>   where   <big> <math>a</math> </big>   and   <big> <math>b</math> </big>   are positive integers, such that   <big> <span style="font-family:times">''a'' ≥ ''b''</span></big>.


(See the [[#REXX|REXX programming example]] to view one method of expressing the whole number part of an improper fraction.)

For improper fractions, the integer part of any improper fraction should be first isolated and shown preceding the Egyptian unit fractions, and be surrounded by square brackets <tt>[''n'']</tt>.


;Task requirements:
*   show the Egyptian fractions for: <math> \tfrac{43}{48} </math> and <math> \tfrac{5}{121} </math> and <math> \tfrac{2014}{59} </math>
*   for all proper fractions,   <big> <math>\tfrac{a}{b}</math> </big>   where   <big> <math>a</math> </big>   and   <big> <math>b</math> </big>   are positive one-or two-digit (decimal) integers, find and show an Egyptian fraction that has:
::*   the largest number of terms,
::*   the largest denominator.
*   for all one-, two-, and three-digit integers,   find and show (as above).     {extra credit}


;Also see:
*   Wolfram MathWorld&trade; entry: [http://mathworld.wolfram.com/EgyptianFraction.html Egyptian fraction]




## C#
{{trans|Visual Basic .NET}}

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;
using System.Threading.Tasks;

namespace EgyptianFractions {
    class Program {
        class Rational : IComparable<Rational>, IComparable<int> {
            public BigInteger Num { get; }
            public BigInteger Den { get; }

            public Rational(BigInteger n, BigInteger d) {
                var c = Gcd(n, d);
                Num = n / c;
                Den = d / c;
                if (Den < 0) {
                    Num = -Num;
                    Den = -Den;
                }
            }

            public Rational(BigInteger n) {
                Num = n;
                Den = 1;
            }

            public override string ToString() {
                if (Den == 1) {
                    return Num.ToString();
                } else {
                    return string.Format("{0}/{1}", Num, Den);
                }
            }

            public Rational Add(Rational rhs) {
                return new Rational(Num * rhs.Den + rhs.Num * Den, Den * rhs.Den);
            }

            public Rational Sub(Rational rhs) {
                return new Rational(Num * rhs.Den - rhs.Num * Den, Den * rhs.Den);
            }

            public int CompareTo(Rational rhs) {
                var ad = Num * rhs.Den;
                var bc = Den * rhs.Num;
                return ad.CompareTo(bc);
            }

            public int CompareTo(int rhs) {
                var ad = Num * rhs;
                var bc = Den * rhs;
                return ad.CompareTo(bc);
            }
        }

        static BigInteger Gcd(BigInteger a, BigInteger b) {
            if (b == 0) {
                if (a < 0) {
                    return -a;
                } else {
                    return a;
                }
            } else {
                return Gcd(b, a % b);
            }
        }

        static List<Rational> Egyptian(Rational r) {
            List<Rational> result = new List<Rational>();

            if (r.CompareTo(1) >= 0) {
                if (r.Den == 1) {
                    result.Add(r);
                    result.Add(new Rational(0));
                    return result;
                }
                result.Add(new Rational(r.Num / r.Den));
                r = r.Sub(result[0]);
            }

            BigInteger modFunc(BigInteger m, BigInteger n) {
                return ((m % n) + n) % n;
            }

            while (r.Num != 1) {
                var q = (r.Den + r.Num - 1) / r.Num;
                result.Add(new Rational(1, q));
                r = new Rational(modFunc(-r.Den, r.Num), r.Den * q);
            }

            result.Add(r);
            return result;
        }

        static string FormatList<T>(IEnumerable<T> col) {
            StringBuilder sb = new StringBuilder();
            var iter = col.GetEnumerator();

            sb.Append('[');
            if (iter.MoveNext()) {
                sb.Append(iter.Current);
            }
            while (iter.MoveNext()) {
                sb.AppendFormat(", {0}", iter.Current);
            }
            sb.Append(']');

            return sb.ToString();
        }

        static void Main() {
            List<Rational> rs = new List<Rational> {
                new Rational(43, 48),
                new Rational(5, 121),
                new Rational(2014, 59)
            };
            foreach (var r in rs) {
                Console.WriteLine("{0} => {1}", r, FormatList(Egyptian(r)));
            }

            var lenMax = Tuple.Create(0UL, new Rational(0));
            var denomMax = Tuple.Create(BigInteger.Zero, new Rational(0));

            var query = (from i in Enumerable.Range(1, 100)
                         from j in Enumerable.Range(1, 100)
                         select new Rational(i, j))
                         .Distinct()
                         .ToList();
            foreach (var r in query) {
                var e = Egyptian(r);
                ulong eLen = (ulong) e.Count;
                var eDenom = e.Last().Den;
                if (eLen > lenMax.Item1) {
                    lenMax = Tuple.Create(eLen, r);
                }
                if (eDenom > denomMax.Item1) {
                    denomMax = Tuple.Create(eDenom, r);
                }
            }

            Console.WriteLine("Term max is {0} with {1} terms", lenMax.Item2, lenMax.Item1);
            var dStr = denomMax.Item1.ToString();
            Console.WriteLine("Denominator max is {0} with {1} digits {2}...{3}", denomMax.Item2, dStr.Length, dStr.Substring(0, 5), dStr.Substring(dStr.Length - 5, 5));
        }
    }
}
```

{{out}}

```txt
43/48 => [1/2, 1/3, 1/16]
5/121 => [1/25, 1/757, 1/763309, 1/873960180913, 1/1527612795642093418846225]
2014/59 => [34, 1/8, 1/95, 1/14947, 1/670223480]
Term max is 97/53 with 9 terms
Denominator max is 8/97 with 150 digits 57950...89665
```



## Common Lisp


```lisp
(defun egyption-fractions (x y &optional acc)
  (let* ((a (/ x y)))
    (cond
     ((> (numerator a) (denominator a))
      (multiple-value-bind (q r) (floor x y)
	(if (zerop r)
	    (cons q acc)
	    (egyption-fractions r y (cons q acc)))))
     ((= (numerator a) 1) (reverse (cons a acc)))
     (t (let ((b (ceiling y x)))
	  (egyption-fractions (mod (- y) x) (* y b) (cons (/ b) acc)))))))

(defun test (n fn)
  (car (sort (loop for i from 1 to n append
		   (loop for j from 2 to n collect
			 (cons (/ i j) (funcall fn (egyption-fractions i j)))))
	     #'>
	     :key #'cdr)))

```


{{out}}
Basic tests:

```txt
(egyption-fractions 43 48)
(egyption-fractions 5 121)
(egyption-fractions 2014 59)
(egyption-fractions 8 97)
```



```txt
(1/2 1/3 1/16)
(1/25 1/757 1/763309 1/873960180913 1/1527612795642093418846225)
(34 1/8 1/95 1/14947 1/670223480)
(1/13 1/181 1/38041 1/1736503177 1/3769304102927363485
 1/18943537893793408504192074528154430149
 1/538286441900380211365817285104907086347439746130226973253778132494225813153
 1/579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665)
```


Other tests:

```txt
(test 999 #'length)
(test 999 (lambda (xs) (loop for x in xs maximizing (denominator x))))
```



```txt
(493/457 . 13)
(36/457
 . 839018826833450186636781520007011999269820404906753180244759299287837378895397605613261469995626498719289835112392530430840514102146998625666594756995273418015600023494049208108894185781774002683063204252356172520941088783702738286944210460710059319691268110283467445381026653628599765684739105388642310044785844902157076919003735231543781785073393176144167688252446541416466418608465458502997971425428342769433127784560570193376772878336217849260872114137931351960543608384244009505664253173875705234889570853924105640193619301332776989688248555027054395237907581951261868280899150574360164800187964167274323078311078867593844043149124596271281252530924719121766925749760855109100066731841478262812686642693395896229983745226277793055820609058348269152190083695704685769622011655159174272326647342695589818127126303038171968768650476413027459205291075571637957597356820188031655122749743652301268394542123970892422944335857917641636041892192547135178153602038877677614358281581103685526041329841496863410305888255234495015115912388514981113593387572720476744188169200130515719608747338810136728267784013352396910979904545913458536243327311977805126410065576961237640824852114328884086581542091492600312838425666927627674227053793897767395465326589843035773944346372949759909905561209334216847158156644884281300512699910530092870919061876615770708519243818676366245477462042294267674677954783726990349386117468071932874021023714524610740225814235147693954027910741673103980749749728106483987721602738673173009362802337092908847797499475895347112889339502928407808058670297722175686638678788738689803945574002805677250463286479363670076942509109589495377221095405979217163821481666646160815221224686562530536116613645305335922819524037829878961518170177968768364853399057357772141655622381280196908637031556436461404285930426436983658106288733881761514992109680298995922754466040011586713812553117621857109517258943846004179432521131844156242428351270188803919554398620084668514054504414062276012292497375238210886595006249453460414790147611422121782194848803348777061816460876697945418158442269512987729152441940326466631610424906158237288218706447963113019239557885486647314085357651895226117364760315394354624547919209138539180807829672545924239541758108877100331729470119526373928796447673951888289511964811633025369821156695934557103429921063387965046715070102916811976552584464153981214277622597308113449320462341683055200576571910241686615924531368198770946893858410058348221985603151428153382461711196734214085852523778422630907646235900752317571022131569421231196329080023952364788544301495422061066036911772385739659997665503832444529713544286955548310166168837889046149061296461059432238621602179724809510024772127497080258401694929973105184832214622785679651550368465524821062859837409907538269572622296774545103747438431266995525592705)

```



## D

Assuming the Python entry is correct, this code is equivalent. This requires the D module of the Arithmetic/Rational task.
{{trans|Python}}

```d
import std.stdio, std.bigint, std.algorithm, std.range, std.conv, std.typecons,
       arithmetic_rational: Rat = Rational;

Rat[] egyptian(Rat r) pure nothrow {
    typeof(return) result;

    if (r >= 1) {
        if (r.denominator == 1)
            return [r, Rat(0, 1)];
        result = [Rat(r.numerator / r.denominator, 1)];
        r -= result[0];
    }

    static enum mod = (in BigInt m, in BigInt n) pure nothrow =>
        ((m % n) + n) % n;

    while (r.numerator != 1) {
        immutable q = (r.denominator + r.numerator - 1) / r.numerator;
        result ~= Rat(1, q);
        r = Rat(mod(-r.denominator, r.numerator), r.denominator * q);
    }

    result ~= r;
    return result;
}

void main() {
    foreach (immutable r; [Rat(43, 48), Rat(5, 121), Rat(2014, 59)])
        writefln("%s => %(%s %)", r, r.egyptian);

    Tuple!(size_t, Rat) lenMax;
    Tuple!(BigInt, Rat) denomMax;

    foreach (immutable r; iota(1, 100).cartesianProduct(iota(1, 100))
                          .map!(nd => nd[].Rat).array.sort().uniq) {
        immutable e = r.egyptian;
        immutable eLen = e.length;
        immutable eDenom = e.back.denominator;
        if (eLen > lenMax[0])
            lenMax = tuple(eLen, r);
        if (eDenom > denomMax[0])
            denomMax = tuple(eDenom, r);
    }
    writefln("Term max is %s with %d terms", lenMax[1], lenMax[0]);
    immutable dStr = denomMax[0].text;
    writefln("Denominator max is %s with %d digits %s...%s",
             denomMax[1], dStr.length, dStr[0 .. 5], dStr[$ - 5 .. $]);
}
```

{{out}}

```txt
43/48 => 1/2 1/3 1/16
5/121 => 1/25 1/757 1/763309 1/873960180913 1/1527612795642093418846225
2014/59 => 34 1/8 1/95 1/14947 1/670223480
Term max is 97/53 with 9 terms
Denominator max is 8/97 with 150 digits 57950...89665
```



## Factor


```factor
USING: backtrack formatting fry kernel locals make math
math.functions math.ranges sequences ;
IN: rosetta-code.egyptian-fractions

: >improper ( r -- str ) >fraction "%d/%d" sprintf ;

: improper ( x y -- a b ) [ /i ] [ [ rem ] [ nip ] 2bi / ] 2bi ;

:: proper ( x y -- a b )
    y x / ceiling :> d1 1 d1 / y neg x rem y d1 * / ;

: expand ( a -- b c )
    >fraction 2dup > [ improper ] [ proper ] if ;

: egyptian-fractions ( x -- seq )
    [ [ expand [ , ] dip dup 0 = not ] loop drop ] { } make ;

: part1 ( -- )
    43/48 5/121 2014/59 [
        [ >improper ] [ egyptian-fractions ] bi
        "%s => %[%u, %]\n" printf
    ] tri@ ;

: all-longest ( seq -- seq )
    dup longest length '[ length _ = ] filter ;

: (largest-denominator) ( seq -- n )
    [ denominator ] map supremum ;

: most-terms ( seq -- )
    all-longest [ [ sum ] map ] [ first length ] bi
    "most terms: %[%u, %] => %d\n" printf ;

: largest-denominator ( seq -- )
    [ (largest-denominator) ] supremum-by
    [ sum ] [ (largest-denominator) ] bi
    "largest denominator: %u => %d\n" printf ;

: part2 ( -- )
    [
        99 [1,b] amb-lazy dup [1,b] amb-lazy swap /
        egyptian-fractions
    ] bag-of [ most-terms ] [ largest-denominator ] bi ;

: egyptian-fractions-demo ( -- ) part1 part2 ;

MAIN: egyptian-fractions-demo
```

{{out}}

```txt

43/48 => { 1/2, 1/3, 1/16 }
5/121 => { 1/25, 1/757, 1/763309, 1/873960180913, 1/1527612795642093418846225 }
2014/59 => { 34, 1/8, 1/95, 1/14947, 1/670223480 }
most terms: { 44/53, 8/97 } => 8
largest denominator: 8/97 => 579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665

```



## FreeBASIC

{{libheader|GMP}}

```freebasic
' version 16-01-2017
' compile with: fbc -s console

#Define max 30

#Include Once "gmp.bi"

Dim Shared As Mpz_ptr num(max), den(max)

Function Egyptian_fraction(fraction As String, ByRef whole As Integer, range As Integer = 0) As Integer

    If InStr(fraction,"/") = 0 Then
        Print "Not a fraction, program will end"
        Sleep 5000, 1
        End
    End If

    Dim As Integer i, count

    Dim As Mpz_ptr tmp_num, tmp_den, x, y, q
    tmp_num = Allocate(Len(__Mpz_struct)) : Mpz_init(tmp_num)
    tmp_den = Allocate(Len(__Mpz_struct)) : Mpz_init(tmp_den)
    x = Allocate(Len(__Mpz_struct)) : Mpz_init(x)
    y = Allocate(Len(__Mpz_struct)) : Mpz_init(y)
    q = Allocate(Len(__Mpz_struct)) : Mpz_init(q)

    For i = 1 To max ' clear the list
        Mpz_set_ui(num(i), 0)
        Mpz_set_ui(den(i), 0)
    Next

    i = InStr(fraction,"/")
    Mpz_set_str(x, Left(fraction, i -1), 10)
    Mpz_set_str(y, Right(fraction, Len(fraction) - i), 10)

    ' if it's a improper fraction make it proper fraction
    If Mpz_cmp(x , y) > 0  Then
        Mpz_fdiv_q(q, x, y)
        whole = Mpz_get_ui(q)
        Mpz_fdiv_r(x, x, q)
    Else
        whole = 0
    End If

    Mpz_gcd(q, x, y) ' check if reduction is possible
    If Mpz_cmp_ui(q, 1) > 0 Then
        If range <> 0 Then ' return if we do a range test
            Return -1
        Else
            Mpz_fdiv_q(x, x, q)
            Mpz_fdiv_q(y, y, q)
        End If
    End If

    Mpz_set(num(count), x)
    Mpz_set(den(count), y)
    ' Fibonacci's Greedy algorithm for Egyptian fractions
    Do
        If Mpz_cmp_ui(num(count), 1) = 0 Then Exit Do
        Mpz_set(x, num(count))
        Mpz_set(y, den(count))
        Mpz_cdiv_q(q, y, x)
        Mpz_set_ui(num(count), 1)
        Mpz_set(den(count), q)
        Mpz_mul(tmp_den, y, q)
        Mpz_neg(y, y)
        Mpz_mod(tmp_num, y, x)
        count += 1
        Mpz_gcd(q, tmp_num, tmp_den) ' check if reduction is possible
        If Mpz_cmp_ui(q, 1) > 0 Then
            Mpz_fdiv_q(tmp_num, tmp_num, q)
            Mpz_fdiv_q(tmp_den, tmp_den, q)
        End If
        Mpz_set(num(count), tmp_num)
        Mpz_set(den(count), tmp_den)
    Loop

    Mpz_clear(tmp_num) : Mpz_clear(tmp_den)
    Mpz_clear(x) : Mpz_clear(y) :Mpz_clear(q)

    Return count

End Function

Sub prt_solution(fraction As String, whole As Integer, count As Integer)

    Print fraction; " = ";

    If whole <> 0 Then
        Print "["; Str(whole); "] + ";
    End If

    For i As Integer = 0 To count
        Gmp_printf("%Zd/%Zd ", num(i), den(i))
        If i <> count Then Print "+ ";
    Next
    Print

End Sub

' ------=< MAIN >=------

Dim As Integer n, d, number, improper, max_term,  max_size
Dim As String str_in, max_term_str, max_size_str, m_str
Dim As ZString Ptr gmp_str : gmp_str = Allocate(1000000)

For n = 0 To max
    num(n) = Allocate(Len(__Mpz_struct)) : Mpz_init(num(n))
    den(n) = Allocate(Len(__Mpz_struct)) : Mpz_init(den(n))
Next

Data "43/48", "5/121", "2014/59"
' 4/121 = 12/363 = 11/363 + 1/363 = 1/33 + 1/363
' 5/121 = 4/121 + 1/121 = 1/33 + 1/121 + 1/363
' 2014/59 = 34 + 8/59
' 8/59 = 1/8 + 5/472 = 1/8 + 4/472 + 1/472 = 1/8 + 1/118 + 1/472

For n = 1 To 3
    Read str_in
    number = Egyptian_fraction(str_in, improper)
    prt_solution(str_in, improper, number)
    Print
Next

Dim As Integer a = 1 , b = 99

Do
    For d = a To b
        For n = 1 To d -1
            str_in = Str(n) + "/" + Str(d)
            number = Egyptian_fraction(str_in, improper,1)
            If number = -1 Then Continue For ' skip
            If number > max_term Then
                max_term = number
                max_term_str = str_in
            ElseIf number = max_term Then
                max_term_str += ", " & str_in
            End If
            Mpz_get_str(gmp_str, 10, den(number))
            If Len(*gmp_str) > max_size Then
                max_size = Len(*gmp_str)
                max_size_str = str_in
                m_str = *gmp_str
            ElseIf max_size = Len(*gmp_str) Then
                max_size_str += ", " & str_in
            End If
        Next
    Next
    Print
    Print "for 1 to"; Len(Str(b)); " digits"
    Print "Largest number of terms is"; max_term +1; " for "; max_term_str
    Print "Largest size for denominator is"; max_size; " for "; max_size_str

    If b = 999 Then Exit Do
    a = b +1 : b = b * 10 +9
Loop

For n = 0 To max
    Mpz_clear(num(n))
    Mpz_clear(den(n))
Next

DeAllocate(gmp_str)

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
43/48 = 1/2 + 1/3 + 1/16

5/121 = 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225

2014/59 = [34] + 1/8 + 1/95 + 1/14947 + 1/670223480


for 1 to 2 digits
Largest number of terms is 8 for 44/53, 8/97
Largest size for denominator is 150 for 8/97

for 1 to 3 digits
Largest number of terms is 13 for 641/796, 529/914
Largest size for denominator is 2847 for 36/457, 529/914
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Egyptian_fractions this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Erlang


```erlang
-module(egypt).

-import(lists, [reverse/1, seq/2]).
-export([frac/2, show/2, rosetta/0]).

rosetta() ->
    Fractions = [{N, D, second(frac(N, D))} || N <- seq(2,99), D <- seq(N+1, 99)],
    {Longest, A1, B1} = findmax(fun length/1, Fractions),
    io:format("~b/~b has ~b terms.~n", [A1, B1, Longest]),
    {Largest, A2, B2} = findmax(fun (L) -> hd(reverse(L)) end, Fractions),
    io:format("~b/~b has a really long denominator. (~b)~n", [A2, B2, Largest]).

second({_, B}) -> B.

findmax(Fn, L) -> findmax(Fn, L, 0, 0, 0).
findmax(_, [], M, A, B) -> {M, A, B};
findmax(Fn, [{A,B,Frac}|Fracs], M, A0, B0) ->
    Val = Fn(Frac),
    case Val > M of
        true  -> findmax(Fn, Fracs, Val, A, B);
        false -> findmax(Fn, Fracs, M, A0, B0)
    end.

show(A, B) ->
    {W, R} = frac(A, B),
    case W of
        0 -> ok;
        _ -> io:format("[~b] ", [W])
    end,
    case R of
        [] -> ok;
        [D0|Ds] ->
            io:format("1/~b ", [D0]),
            [io:format("+ 1/~b ", [D]) || D <- Ds],
            ok
    end.

frac(A, B) ->
    {A div B, reverse(proper(A rem B, B, []))}.

proper(0, _, L) -> L;
proper(1, Y, L) -> [Y|L];
proper(X, Y, L) ->
    D = ceildiv(Y, X),
    X2 = mod(-Y, X),
    Y2 = Y*ceildiv(Y, X),
    proper(X2, Y2, [D|L]).

ceildiv(A, B) ->
    Q = A div B,
    case A rem B of
        0 -> Q;
        _ -> Q+1
    end.

mod(A, M) ->
    B = A rem M,
    if
        B < 0 -> B + M;
        true -> B
    end.

```


{{out}}

```txt

129> egypt:show(43,48).
1/2 + 1/3 + 1/16 ok
130> egypt:show(5,121).
1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225 ok
131> egypt:show(2014,59).
[34] 1/8 + 1/95 + 1/14947 + 1/670223480 ok
132> egypt:rosetta().
8/97 has 8 terms.
8/97 has a really long denominator. (579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665)
ok

```



## Go

{{trans|Kotlin}}
... except that Go already has support for arbitrary precision rational numbers in its standard library.

```go
package main

import (
    "fmt"
    "math/big"
    "strings"
)

var zero = new(big.Int)
var one = big.NewInt(1)

func toEgyptianRecursive(br *big.Rat, fracs []*big.Rat) []*big.Rat {
    if br.Num().Cmp(zero) == 0 {
        return fracs
    }
    iquo := new(big.Int)
    irem := new(big.Int)
    iquo.QuoRem(br.Denom(), br.Num(), irem)
    if irem.Cmp(zero) > 0 {
        iquo.Add(iquo, one)
    }
    rquo := new(big.Rat).SetFrac(one, iquo)
    fracs = append(fracs, rquo)
    num2 := new(big.Int).Neg(br.Denom())
    num2.Rem(num2, br.Num())
    if num2.Cmp(zero) < 0 {
        num2.Add(num2, br.Num())
    }
    denom2 := new(big.Int)
    denom2.Mul(br.Denom(), iquo)
    f := new(big.Rat).SetFrac(num2, denom2)
    if f.Num().Cmp(one) == 0 {
        fracs = append(fracs, f)
        return fracs
    }
    fracs = toEgyptianRecursive(f, fracs)
    return fracs
}

func toEgyptian(rat *big.Rat) []*big.Rat {
    if rat.Num().Cmp(zero) == 0 {
        return []*big.Rat{rat}
    }
    var fracs []*big.Rat
    if rat.Num().CmpAbs(rat.Denom()) >= 0 {
        iquo := new(big.Int)
        iquo.Quo(rat.Num(), rat.Denom())
        rquo := new(big.Rat).SetFrac(iquo, one)
        rrem := new(big.Rat)
        rrem.Sub(rat, rquo)
        fracs = append(fracs, rquo)
        fracs = toEgyptianRecursive(rrem, fracs)
    } else {
        fracs = toEgyptianRecursive(rat, fracs)
    }
    return fracs
}

func main() {
    fracs := []*big.Rat{big.NewRat(43, 48), big.NewRat(5, 121), big.NewRat(2014, 59)}
    for _, frac := range fracs {
        list := toEgyptian(frac)
        if list[0].Denom().Cmp(one) == 0 {
            first := fmt.Sprintf("[%v]", list[0].Num())
            temp := make([]string, len(list)-1)
            for i := 1; i < len(list); i++ {
                temp[i-1] = list[i].String()
            }
            rest := strings.Join(temp, " + ")
            fmt.Printf("%v -> %v + %s\n", frac, first, rest)
        } else {
            temp := make([]string, len(list))
            for i := 0; i < len(list); i++ {
                temp[i] = list[i].String()
            }
            all := strings.Join(temp, " + ")
            fmt.Printf("%v -> %s\n", frac, all)
        }
    }

    for _, r := range [2]int{98, 998} {
        if r == 98 {
            fmt.Println("\nFor proper fractions with 1 or 2 digits:")
        } else {
            fmt.Println("\nFor proper fractions with 1, 2 or 3 digits:")
        }
        maxSize := 0
        var maxSizeFracs []*big.Rat
        maxDen := zero
        var maxDenFracs []*big.Rat
        var sieve = make([][]bool, r+1) // to eliminate duplicates
        for i := 0; i <= r; i++ {
            sieve[i] = make([]bool, r+2)
        }
        for i := 1; i <= r; i++ {
            for j := i + 1; j <= r+1; j++ {
                if sieve[i][j] {
                    continue
                }
                f := big.NewRat(int64(i), int64(j))
                list := toEgyptian(f)
                listSize := len(list)
                if listSize > maxSize {
                    maxSize = listSize
                    maxSizeFracs = maxSizeFracs[0:0]
                    maxSizeFracs = append(maxSizeFracs, f)
                } else if listSize == maxSize {
                    maxSizeFracs = append(maxSizeFracs, f)
                }
                listDen := list[len(list)-1].Denom()
                if listDen.Cmp(maxDen) > 0 {
                    maxDen = listDen
                    maxDenFracs = maxDenFracs[0:0]
                    maxDenFracs = append(maxDenFracs, f)
                } else if listDen.Cmp(maxDen) == 0 {
                    maxDenFracs = append(maxDenFracs, f)
                }
                if i < r/2 {
                    k := 2
                    for {
                        if j*k > r+1 {
                            break
                        }
                        sieve[i*k][j*k] = true
                        k++
                    }
                }
            }
        }
        fmt.Println("  largest number of items =", maxSize)
        fmt.Println("  fraction(s) with this number :", maxSizeFracs)
        md := maxDen.String()
        fmt.Print("  largest denominator = ", len(md), " digits, ")
        fmt.Print(md[0:20], "...", md[len(md)-20:], "\b\n")
        fmt.Println("  fraction(s) with this denominator :", maxDenFracs)
    }
}
```


{{out}}

```txt

43/48 -> 1/2 + 1/3 + 1/16
5/121 -> 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225
2014/59 -> [34] + 1/8 + 1/95 + 1/14947 + 1/670223480

For proper fractions with 1 or 2 digits:
  largest number of items = 8
  fraction(s) with this number : [8/97 44/53]
  largest denominator = 150 digits, 57950458706754280171...62011424993909789665
  fraction(s) with this denominator : [8/97]

For proper fractions with 1, 2 or 3 digits:
  largest number of items = 13
  fraction(s) with this number : [529/914 641/796]
  largest denominator = 2847 digits, 83901882683345018663...38431266995525592705
  fraction(s) with this denominator : [36/457 529/914]

```



## Haskell


```haskell
import Data.Ratio (Ratio, (%), denominator, numerator)

egyptianFraction :: Integral a => Ratio a -> [Ratio a]
egyptianFraction n
  | n < 0 = map negate (egyptianFraction (-n))
  | n == 0 = []
  | x == 1 = [n]
  | x > y = (x `div` y % 1) : egyptianFraction (x `mod` y % y)
  | otherwise = (1 % r) : egyptianFraction ((-y) `mod` x % (y * r))
  where
    x = numerator n
    y = denominator n
    r = y `div` x + 1
```


'''Testing''':

```haskell
λ> :m Test.QuickCheck
λ> quickCheck (\n -> n == (sum $ egyptianFraction n))
+++ OK, passed 100 tests.
```


'''Tasks''':

```haskell
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)

task1 = mapM_ run [43 % 48, 5 % 121, 2014 % 59]
  where
    run x = putStrLn $ show x ++ " = " ++ result x
    result x = intercalate " + " $ show <$> egyptianFraction x

task21 n =
  maximumBy
    (comparing snd)
    [ (a % b, length $ egyptianFraction (a % b))
    | a <- [1 .. n]
    , b <- [1 .. n]
    , a < b ]

task22 n =
  maximumBy
    (comparing snd)
    [ (a % b, maximum $ map denominator $ egyptianFraction (a % b))
    | a <- [1 .. n]
    , b <- [1 .. n]
    , a < b ]
```



```haskell
λ> task1
43 % 48 = 1 % 2 + 1 % 3 + 1 % 16
5 % 121 = 1 % 25 + 1 % 757 + 1 % 763309 + 1 % 873960180913 + 1 % 1527612795642093418846225
2014 % 59 = 34 % 1 + 1 % 8 + 1 % 95 + 1 % 14947 + 1 % 670223480
λ> task21 99
(44 % 53, 8)
λ> task22 99
(8 % 97, 579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665)
λ> task21 999
(641 % 796,13)
λ> task22 999
(529 % 914, 839018826833450186636781520007011999269820404906753180244759299287837378895397605613261469995626498719289835112392530430840514102146998625666594756995273418015600023494049208108894185781774002683063204252356172520941088783702738286944210460710059319691268110283467445381026653628599765684739105388642310044785844902157076919003735231543781785073393176144167688252446541416466418608465458502997971425428342769433127784560570193376772878336217849260872114137931351960543608384244009505664253173875705234889570853924105640193619301332776989688248555027054395237907581951261868280899150574360164800187964167274323078311078867593844043149124596271281252530924719121766925749760855109100066731841478262812686642693395896229983745226277793055820609058348269152190083695704685769622011655159174272326647342695589818127126303038171968768650476413027459205291075571637957597356820188031655122749743652301268394542123970892422944335857917641636041892192547135178153602038877677614358281581103685526041329841496863410305888255234495015115912388514981113593387572720476744188169200130515719608747338810136728267784013352396910979904545913458536243327311977805126410065576961237640824852114328884086581542091492600312838425666927627674227053793897767395465326589843035773944346372949759909905561209334216847158156644884281300512699910530092870919061876615770708519243818676366245477462042294267674677954783726990349386117468071932874021023714524610740225814235147693954027910741673103980749749728106483987721602738673173009362802337092908847797499475895347112889339502928407808058670297722175686638678788738689803945574002805677250463286479363670076942509109589495377221095405979217163821481666646160815221224686562530536116613645305335922819524037829878961518170177968768364853399057357772141655622381280196908637031556436461404285930426436983658106288733881761514992109680298995922754466040011586713812553117621857109517258943846004179432521131844156242428351270188803919554398620084668514054504414062276012292497375238210886595006249453460414790147611422121782194848803348777061816460876697945418158442269512987729152441940326466631610424906158237288218706447963113019239557885486647314085357651895226117364760315394354624547919209138539180807829672545924239541758108877100331729470119526373928796447673951888289511964811633025369821156695934557103429921063387965046715070102916811976552584464153981214277622597308113449320462341683055200576571910241686615924531368198770946893858410058348221985603151428153382461711196734214085852523778422630907646235900752317571022131569421231196329080023952364788544301495422061066036911772385739659997665503832444529713544286955548310166168837889046149061296461059432238621602179724809510024772127497080258401694929973105184832214622785679651550368465524821062859837409907538269572622296774545103747438431266995525592705)

```



## J

'''Solution''':
```j
   ef   =: [: (}.~ 0={.) [: (, r2ef)/ 0 1 #: x:
   r2ef =: (<(<0);0) { ((] , -) >:@:<.&.%)^:((~:<.)@:%)@:{:^:a:
```


'''Examples''' (''required''):
```j
   (; ef)&> 43r48 5r121 2014r59
+-------+--------------------------------------------------------------+
|43r48  |1r2 1r3 1r16                                                  |
+-------+--------------------------------------------------------------+
|5r121  |1r25 1r757 1r763309 1r873960180913 1r1527612795642093418846225|
+-------+--------------------------------------------------------------+
|2014r59|34 1r8 1r95 1r14947 1r670223480                               |
+-------+--------------------------------------------------------------+
```


'''Examples''' (''extended''):
```j
   NB. ef for all 1- and 2-digit fractions
   EF2  =:  ef :: _1:&.> (</~ * %/~) i. 10^2x


   NB. longest ef for 1- or 2-digit fraction
   ($ #: (i. >./)@:,)#&>EF2
8 97
   # ef 8r97
8

   NB. largest denom among for 1- and 2-digit fractions
   ($ #: (i. <./)@:|@:(<./&>)@:,) EF2
8 97
   _80 ]\ ": % <./ ef 8r97
57950458706754280171310319185991860825103029195219542358352935765389941868634236
0361798689053273749372615043661810228371898539583862011424993909789665

   NB. ef for all 1-,2-, and 3-digit fractions
   EF3  =:  ef :: _1:&.> (</~ * %/~) i. 10^3x

   NB. longest ef for 1-, 2-,or 3-digit fraction
   ($ #: (i. >./)@:,)#&>EF3
529 914
   # ef 529r914
13

   NB. largest denom among for 1-, 2-, and 3-digit fractions
   ($ #: (i. <./)@:|@:(<./&>)@:,) EF3
36 457
   _80 ]\ ": % <./ ef 36r457
83901882683345018663678152000701199926982040490675318024475929928783737889539760
56132614699956264987192898351123925304308405141021469986256665947569952734180156
00023494049208108894185781774002683063204252356172520941088783702738286944210460
71005931969126811028346744538102665362859976568473910538864231004478584490215707
69190037352315437817850733931761441676882524465414164664186084654585029979714254
28342769433127784560570193376772878336217849260872114137931351960543608384244009
50566425317387570523488957085392410564019361930133277698968824855502705439523790
75819512618682808991505743601648001879641672743230783110788675938440431491245962
71281252530924719121766925749760855109100066731841478262812686642693395896229983
74522627779305582060905834826915219008369570468576962201165515917427232664734269
55898181271263030381719687686504764130274592052910755716379575973568201880316551
22749743652301268394542123970892422944335857917641636041892192547135178153602038
87767761435828158110368552604132984149686341030588825523449501511591238851498111
35933875727204767441881692001305157196087473388101367282677840133523969109799045
45913458536243327311977805126410065576961237640824852114328884086581542091492600
31283842566692762767422705379389776739546532658984303577394434637294975990990556
12093342168471581566448842813005126999105300928709190618766157707085192438186763
66245477462042294267674677954783726990349386117468071932874021023714524610740225
81423514769395402791074167310398074974972810648398772160273867317300936280233709
29088477974994758953471128893395029284078080586702977221756866386787887386898039
45574002805677250463286479363670076942509109589495377221095405979217163821481666
64616081522122468656253053611661364530533592281952403782987896151817017796876836
48533990573577721416556223812801969086370315564364614042859304264369836581062887
33881761514992109680298995922754466040011586713812553117621857109517258943846004
17943252113184415624242835127018880391955439862008466851405450441406227601229249
73752382108865950062494534604147901476114221217821948488033487770618164608766979
45418158442269512987729152441940326466631610424906158237288218706447963113019239
55788548664731408535765189522611736476031539435462454791920913853918080782967254
59242395417581088771003317294701195263739287964476739518882895119648116330253698
21156695934557103429921063387965046715070102916811976552584464153981214277622597
30811344932046234168305520057657191024168661592453136819877094689385841005834822
19856031514281533824617111967342140858525237784226309076462359007523175710221315
69421231196329080023952364788544301495422061066036911772385739659997665503832444
52971354428695554831016616883788904614906129646105943223862160217972480951002477
21274970802584016949299731051848322146227856796515503684655248210628598374099075
38269572622296774545103747438431266995525592705
```



## Java

{{trans|Kotlin}}
{{works with|Java|9}}

```Java
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class EgyptianFractions {
    private static BigInteger gcd(BigInteger a, BigInteger b) {
        if (b.equals(BigInteger.ZERO)) {
            return a;
        }
        return gcd(b, a.mod(b));
    }

    private static class Frac implements Comparable<Frac> {
        private BigInteger num, denom;

        public Frac(BigInteger n, BigInteger d) {
            if (d.equals(BigInteger.ZERO)) {
                throw new IllegalArgumentException("Parameter d may not be zero.");
            }

            BigInteger nn = n;
            BigInteger dd = d;
            if (nn.equals(BigInteger.ZERO)) {
                dd = BigInteger.ONE;
            } else if (dd.compareTo(BigInteger.ZERO) < 0) {
                nn = nn.negate();
                dd = dd.negate();
            }
            BigInteger g = gcd(nn, dd).abs();
            if (g.compareTo(BigInteger.ZERO) > 0) {
                nn = nn.divide(g);
                dd = dd.divide(g);
            }
            num = nn;
            denom = dd;
        }

        public Frac(int n, int d) {
            this(BigInteger.valueOf(n), BigInteger.valueOf(d));
        }

        public Frac plus(Frac rhs) {
            return new Frac(
                num.multiply(rhs.denom).add(denom.multiply(rhs.num)),
                rhs.denom.multiply(denom)
            );
        }

        public Frac unaryMinus() {
            return new Frac(num.negate(), denom);
        }

        public Frac minus(Frac rhs) {
            return plus(rhs.unaryMinus());
        }

        @Override
        public int compareTo(Frac rhs) {
            BigDecimal diff = this.toBigDecimal().subtract(rhs.toBigDecimal());
            if (diff.compareTo(BigDecimal.ZERO) < 0) {
                return -1;
            }
            if (BigDecimal.ZERO.compareTo(diff) < 0) {
                return 1;
            }
            return 0;
        }

        @Override
        public boolean equals(Object obj) {
            if (null == obj || !(obj instanceof Frac)) {
                return false;
            }
            Frac rhs = (Frac) obj;
            return compareTo(rhs) == 0;
        }

        @Override
        public String toString() {
            if (denom.equals(BigInteger.ONE)) {
                return num.toString();
            }
            return String.format("%s/%s", num, denom);
        }

        public BigDecimal toBigDecimal() {
            BigDecimal bdn = new BigDecimal(num);
            BigDecimal bdd = new BigDecimal(denom);
            return bdn.divide(bdd, MathContext.DECIMAL128);
        }

        public List<Frac> toEgyptian() {
            if (num.equals(BigInteger.ZERO)) {
                return Collections.singletonList(this);
            }
            List<Frac> fracs = new ArrayList<>();
            if (num.abs().compareTo(denom.abs()) >= 0) {
                Frac div = new Frac(num.divide(denom), BigInteger.ONE);
                Frac rem = this.minus(div);
                fracs.add(div);
                toEgyptian(rem.num, rem.denom, fracs);
            } else {
                toEgyptian(num, denom, fracs);
            }
            return fracs;
        }

        public void toEgyptian(BigInteger n, BigInteger d, List<Frac> fracs) {
            if (n.equals(BigInteger.ZERO)) {
                return;
            }
            BigDecimal n2 = new BigDecimal(n);
            BigDecimal d2 = new BigDecimal(d);
            BigDecimal[] divRem = d2.divideAndRemainder(n2, MathContext.UNLIMITED);
            BigInteger div = divRem[0].toBigInteger();
            if (divRem[1].compareTo(BigDecimal.ZERO) > 0) {
                div = div.add(BigInteger.ONE);
            }
            fracs.add(new Frac(BigInteger.ONE, div));
            BigInteger n3 = d.negate().mod(n);
            if (n3.compareTo(BigInteger.ZERO) < 0) {
                n3 = n3.add(n);
            }
            BigInteger d3 = d.multiply(div);
            Frac f = new Frac(n3, d3);
            if (f.num.equals(BigInteger.ONE)) {
                fracs.add(f);
                return;
            }
            toEgyptian(f.num, f.denom, fracs);
        }
    }

    public static void main(String[] args) {
        List<Frac> fracs = List.of(
            new Frac(43, 48),
            new Frac(5, 121),
            new Frac(2014, 59)
        );
        for (Frac frac : fracs) {
            List<Frac> list = frac.toEgyptian();
            Frac first = list.get(0);
            if (first.denom.equals(BigInteger.ONE)) {
                System.out.printf("%s -> [%s] + ", frac, first);
            } else {
                System.out.printf("%s -> %s", frac, first);
            }
            for (int i = 1; i < list.size(); ++i) {
                System.out.printf(" + %s", list.get(i));
            }
            System.out.println();
        }

        for (Integer r : List.of(98, 998)) {
            if (r == 98) {
                System.out.println("\nFor proper fractions with 1 or 2 digits:");
            } else {
                System.out.println("\nFor proper fractions with 1, 2 or 3 digits:");
            }

            int maxSize = 0;
            List<Frac> maxSizeFracs = new ArrayList<>();
            BigInteger maxDen = BigInteger.ZERO;
            List<Frac> maxDenFracs = new ArrayList<>();
            boolean[][] sieve = new boolean[r + 1][];
            for (int i = 0; i < r + 1; ++i) {
                sieve[i] = new boolean[r + 2];
            }
            for (int i = 1; i < r; ++i) {
                for (int j = i + 1; j < r + 1; ++j) {
                    if (sieve[i][j]) continue;
                    Frac f = new Frac(i, j);
                    List<Frac> list = f.toEgyptian();
                    int listSize = list.size();
                    if (listSize > maxSize) {
                        maxSize = listSize;
                        maxSizeFracs.clear();
                        maxSizeFracs.add(f);
                    } else if (listSize == maxSize) {
                        maxSizeFracs.add(f);
                    }
                    BigInteger listDen = list.get(list.size() - 1).denom;
                    if (listDen.compareTo(maxDen) > 0) {
                        maxDen = listDen;
                        maxDenFracs.clear();
                        maxDenFracs.add(f);
                    } else if (listDen.equals(maxDen)) {
                        maxDenFracs.add(f);
                    }
                    if (i < r / 2) {
                        int k = 2;
                        while (true) {
                            if (j * k > r + 1) break;
                            sieve[i * k][j * k] = true;
                            k++;
                        }
                    }
                }
            }
            System.out.printf("  largest number of items = %s\n", maxSize);
            System.out.printf("fraction(s) with this number : %s\n", maxSizeFracs);
            String md = maxDen.toString();
            System.out.printf("  largest denominator = %s digits, ", md.length());
            System.out.printf("%s...%s\n", md.substring(0, 20), md.substring(md.length() - 20, md.length()));
            System.out.printf("fraction(s) with this denominator : %s\n", maxDenFracs);
        }
    }
}
```

{{out}}

```txt
43/48 -> 1/2 + 1/3 + 1/16
5/121 -> 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225
2014/59 -> [34] +  + 1/8 + 1/95 + 1/14947 + 1/670223480

For proper fractions with 1 or 2 digits:
  largest number of items = 8
fraction(s) with this number : [8/97, 44/53]
  largest denominator = 150 digits, 57950458706754280171...62011424993909789665
fraction(s) with this denominator : [8/97]

For proper fractions with 1, 2 or 3 digits:
  largest number of items = 13
fraction(s) with this number : [529/914, 641/796]
  largest denominator = 2847 digits, 83901882683345018663...38431266995525592705
fraction(s) with this denominator : [36/457, 529/914]
```



## Julia

{{works with|Julia|0.6}}


```julia
struct EgyptianFraction{T<:Integer} <: Real
    int::T
    frac::NTuple{N,Rational{T}} where N
end

Base.show(io::IO, ef::EgyptianFraction) = println(io, "[", ef.int, "] ", join(ef.frac, " + "))
Base.length(ef::EgyptianFraction) = !iszero(ef.int) + length(ef.frac)
function Base.convert(::Type{EgyptianFraction{T}}, fr::Rational) where T
    fr, int::T = modf(fr)
    fractions = Vector{Rational{T}}(0)
    x::T, y::T = numerator(fr), denominator(fr)
    iszero(x) && return EgyptianFraction{T}(int, (x // y,))
    while x != one(x)
        push!(fractions, one(T) // cld(y, x))
        x, y = mod1(-y, x), y * cld(y, x)
        d = gcd(x, y)
        x ÷= d
        y ÷= d
    end
    push!(fractions, x // y)
    return EgyptianFraction{T}(int, tuple(fractions...))
end
Base.convert(::Type{EgyptianFraction}, fr::Rational{T}) where T = convert(EgyptianFraction{T}, fr)
Base.convert(::Type{EgyptianFraction{T}}, fr::EgyptianFraction) where T = EgyptianFraction{T}(convert(T, fr.int), convert.(Rational{T}, fr.frac))
Base.convert(::Type{Rational{T}}, fr::EgyptianFraction) where T = T(fr.int) + sum(convert.(Rational{T}, fr.frac))
Base.convert(::Type{Rational}, fr::EgyptianFraction{T}) where T = convert(Rational{T}, fr)

@show EgyptianFraction(43 // 48)
@show EgyptianFraction{BigInt}(5 // 121)
@show EgyptianFraction(2014 // 59)

function task(fractions::AbstractVector)
    fracs = convert(Vector{EgyptianFraction{BigInt}}, fractions)
    local frlenmax::EgyptianFraction{BigInt}
    local lenmax = 0
    local frdenmax::EgyptianFraction{BigInt}
    local denmax = 0
    for f in fracs
        if length(f) ≥ lenmax
            lenmax = length(f)
            frlenmax = f
        end
        if denominator(last(f.frac)) ≥ denmax
            denmax = denominator(last(f.frac))
            frdenmax = f
        end
    end
    return frlenmax, lenmax, frdenmax, denmax
end

fr = collect((x // y) for x in 1:100 for y in 1:100 if x != y) |> unique
frlenmax, lenmax, frdenmax, denmax = task(fr)
println("Longest fraction, with length $lenmax: \n", Rational(frlenmax), "\n = ", frlenmax)
println("Fraction with greatest denominator\n(that is $denmax):\n", Rational(frdenmax), "\n = ", frdenmax)

println("\n# For 1 digit-integers:")
fr = collect((x // y) for x in 1:10 for y in 1:10 if x != y) |> unique
frlenmax, lenmax, frdenmax, denmax = task(fr)
println("Longest fraction, with length $lenmax: \n", Rational(frlenmax), "\n = ", frlenmax)
println("Fraction with greatest denominator\n(that is $denmax):\n", Rational(frdenmax), "\n = ", frdenmax)

println("# For 3 digit-integers:")
fr = collect((x // y) for x in 1:1000 for y in 1:1000 if x != y) |> unique
frlenmax, lenmax, frdenmax, denmax = task(fr)
println("Longest fraction, with length $lenmax: \n", Rational(frlenmax), "\n = ", frlenmax)
println("Fraction with greatest denominator\n(that is $denmax):\n", Rational(frdenmax), "\n = ", frdenmax)
```


{{out}}

```txt
EgyptianFraction(43 // 48) = [0] 1//2 + 1//3 + 1//16
EgyptianFraction{BigInt}(5 // 121) = [0] 1//25 + 1//757 + 1//763309 + 1//873960180913 + 1//1527612795642093418846225
EgyptianFraction(2014 // 59) = [34] 1//8 + 1//95 + 1//14947 + 1//670223480

Longest fraction, with length 9:
97//53
 = [1] 1//2 + 1//4 + 1//13 + 1//307 + 1//120871 + 1//20453597227 + 1//697249399186783218655 + 1//1458470173998990524806872692984177836808420
Fraction with greatest denominator
(that is 5795045870675428...424993909789665):
8//97
 = [0] 1//13 + 1//181 + 1//38041 + 1//1736503177 + 1//3769304102927363485 + 1//18943537893793408504192074528154430149 + [...]

# For 1 digit-integers:
Longest fraction, with length 4:
10//7
 = [1] 1//3 + 1//11 + 1//231
Fraction with greatest denominator
(that is 231):
10//7
 = [1] 1//3 + 1//11 + 1//231

# For 3 digit-integers:
Longest fraction, with length 13:
950//457
 = [2] 1//13 + 1//541 + 1//321409 + 1//114781617793 + 1//14821672255960844346913 + ...

Fraction with greatest denominator
(that is 8390188268334501866367815200...[2847 digits]):
950//457
 = [2] 1//13 + 1//541 + 1//321409 + 1//114781617793 + 1//14821672255960844346913...
```



## Kotlin

As the JDK lacks a fraction or rational class, I've included a basic one in the program.

```scala
// version 1.2.10

import java.math.BigInteger
import java.math.BigDecimal
import java.math.MathContext

val bigZero = BigInteger.ZERO
val bigOne  = BigInteger.ONE
val bdZero  = BigDecimal.ZERO
val context = MathContext.UNLIMITED

fun gcd(a: BigInteger, b: BigInteger): BigInteger
    = if (b == bigZero) a else gcd(b, a % b)

class Frac : Comparable<Frac> {
    val num: BigInteger
    val denom: BigInteger

    constructor(n: BigInteger, d: BigInteger) {
        require(d != bigZero)
        var nn = n
        var dd = d
        if (nn == bigZero) {
            dd = bigOne
        }
        else if (dd < bigZero) {
            nn = -nn
            dd = -dd
        }
        val g = gcd(nn, dd).abs()
        if (g > bigOne) {
            nn /= g
            dd /= g
        }
        num = nn
        denom = dd
    }

    constructor(n: Int, d: Int) : this(n.toBigInteger(), d.toBigInteger())

    operator fun plus(other: Frac) =
        Frac(num * other.denom + denom * other.num, other.denom * denom)

    operator fun unaryMinus() = Frac(-num, denom)

    operator fun minus(other: Frac) = this + (-other)

    override fun compareTo(other: Frac): Int {
        val diff = this.toBigDecimal() - other.toBigDecimal()
        return when {
            diff < bdZero  -> -1
            diff > bdZero  -> +1
            else           ->  0
        }
    }

    override fun equals(other: Any?): Boolean {
       if (other == null || other !is Frac) return false
       return this.compareTo(other) == 0
    }

    override fun toString() = if (denom == bigOne) "$num" else "$num/$denom"

    fun toBigDecimal() = num.toBigDecimal() / denom.toBigDecimal()

    fun toEgyptian(): List<Frac> {
        if (num == bigZero) return listOf(this)
        val fracs = mutableListOf<Frac>()
        if (num.abs() >= denom.abs()) {
            val div = Frac(num / denom, bigOne)
            val rem = this - div
            fracs.add(div)
            toEgyptian(rem.num, rem.denom, fracs)
        }
        else {
            toEgyptian(num, denom, fracs)
        }
        return fracs
    }

    private tailrec fun toEgyptian(
        n: BigInteger,
        d: BigInteger,
        fracs: MutableList<Frac>
    ) {
        if (n == bigZero) return
        val n2 = n.toBigDecimal()
        val d2 = d.toBigDecimal()
        var divRem = d2.divideAndRemainder(n2, context)
        var div = divRem[0].toBigInteger()
        if (divRem[1] > bdZero) div++
        fracs.add(Frac(bigOne, div))
        var n3 = (-d) % n
        if (n3 < bigZero) n3 += n
        val d3 = d * div
        val f = Frac(n3, d3)
        if (f.num == bigOne) {
            fracs.add(f)
            return
        }
        toEgyptian(f.num, f.denom, fracs)
    }
}

fun main(args: Array<String>) {
    val fracs = listOf(Frac(43, 48), Frac(5, 121), Frac(2014,59))
    for (frac in fracs) {
        val list = frac.toEgyptian()
        if (list[0].denom == bigOne) {
            val first = "[${list[0]}]"
            println("$frac -> $first + ${list.drop(1).joinToString(" + ")}")
        }
        else {
            println("$frac -> ${list.joinToString(" + ")}")
        }
    }

    for (r in listOf(98, 998)) {
        if (r == 98)
            println("\nFor proper fractions with 1 or 2 digits:")
        else
            println("\nFor proper fractions with 1, 2 or 3 digits:")
        var maxSize = 0
        var maxSizeFracs = mutableListOf<Frac>()
        var maxDen = bigZero
        var maxDenFracs = mutableListOf<Frac>()
        val sieve = List(r + 1) { BooleanArray(r + 2) }  // to eliminate duplicates
        for (i in 1..r) {
            for (j in (i + 1)..(r + 1)) {
                if (sieve[i][j]) continue
                val f = Frac(i, j)
                val list = f.toEgyptian()
                val listSize = list.size
                if (listSize > maxSize) {
                    maxSize = listSize
                    maxSizeFracs.clear()
                    maxSizeFracs.add(f)
                }
                else if (listSize == maxSize) {
                    maxSizeFracs.add(f)
                }
                val listDen = list[list.lastIndex].denom
                if (listDen > maxDen) {
                    maxDen = listDen
                    maxDenFracs.clear()
                    maxDenFracs.add(f)
                }
                else if (listDen == maxDen) {
                    maxDenFracs.add(f)
                }
                if (i < r / 2) {
                   var k = 2
                   while (true) {
                       if (j * k > r + 1) break
                       sieve[i * k][j * k] = true
                       k++
                   }
                }
            }
        }
        println("  largest number of items = $maxSize")
        println("  fraction(s) with this number : $maxSizeFracs")
        val md = maxDen.toString()
        print("  largest denominator = ${md.length} digits, ")
        println("${md.take(20)}...${md.takeLast(20)}")
        println("  fraction(s) with this denominator : $maxDenFracs")
    }
}
```


{{out}}

```txt

43/48 -> 1/2 + 1/3 + 1/16
5/121 -> 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225
2014/59 -> [34] + 1/8 + 1/95 + 1/14947 + 1/670223480

For proper fractions with 1 or 2 digits:
  largest number of items = 8
  fraction(s) with this number : [8/97, 44/53]
  largest denominator = 150 digits, 57950458706754280171...62011424993909789665
  fraction(s) with this denominator : [8/97]

For proper fractions with 1, 2 or 3 digits:
  largest number of items = 13
  fraction(s) with this number : [529/914, 641/796]
  largest denominator = 2847 digits, 83901882683345018663...38431266995525592705
  fraction(s) with this denominator : [36/457, 529/914]

```



## Mathematica


```Mathematica
frac[n_] /; IntegerQ[1/n] := frac[n] = {n};
frac[n_] :=
  frac[n] =
   With[{p = Numerator[n], q = Denominator[n]},
    Prepend[frac[Mod[-q, p]/(q Ceiling[1/n])], 1/Ceiling[1/n]]];
disp[f_] :=
  StringRiffle[
    SequenceCases[f,
     l : {_, 1 ...} :>
      If[Length[l] == 1 && l[[1]] < 1, ToString[l[[1]], InputForm],
       "[" <> ToString[Length[l]] <> "]"]], " + "] <> " = " <>
   ToString[Numerator[Total[f]]] <> "/" <>
   ToString[Denominator[Total[f]]];
Print[disp[frac[43/48]]];
Print[disp[frac[5/121]]];
Print[disp[frac[2014/59]]];
fracs = Flatten[Table[frac[p/q], {q, 99}, {p, q}], 1];
Print[disp[MaximalBy[fracs, Length@*Union][[1]]]];
Print[disp[MaximalBy[fracs, Denominator@*Last][[1]]]];
fracs = Flatten[Table[frac[p/q], {q, 999}, {p, q}], 1];
Print[disp[MaximalBy[fracs, Length@*Union][[1]]]];
Print[disp[MaximalBy[fracs, Denominator@*Last][[1]]]];
```

{{out}}

```txt
1/2 + 1/3 + 1/16 = 43/48
1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225 = 5/121
[34] + 1/8 + 1/95 + 1/14947 + 1/670223480 = 2014/59
1/2 + 1/4 + 1/13 + 1/307 + 1/120871 + 1/20453597227 + 1/697249399186783218655 + 1/1458470173998990524806872692984177836808420 = 44/53
1/13 + 1/181 + 1/38041 + 1/1736503177 + 1/3769304102927363485 + 1/18943537893793408504192074528154430149 + 1/538286441900380211365817285104907086347439746130226973253778132494225813153 + 1/579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665 = 8/97
1/2 + 1/4 + 1/19 + 1/379 + 1/159223 + 1/28520799973 + 1/929641178371338400861 + 1/1008271507277592391123742528036634174730681 + 1/1219933718865393655364635368068124756713122928811333803786753398211072842948484537833 + 1/1860297848030936654742608399135821395565274404917258533393305147319524009551744684579405649080712180254407780735949179513154143641842892458088536544987153757401025882029 + 1/4614277444518045184646591832326467411359277711335974416082881814986405515888533562332069783067894981850924485553345190160771506460024406127868096951360637582674289834858262576425271895218431296391169922044160278696744025988461165811212428548328350795432691637759392474030879286312785400132190057899968737693594392669884878193448874327093 + 1/31937334502481972335865307630139228000187060941658399518862518849553429993133277230560087986574331290756232125775998863890963263813589266879406694561350952988662850757053371133819179770003609046815203982179108798005308113258134895569927488690118483730232440575942894680942308888321353318333183158977270294582315388855860989819894602178852719674244639951777398683083694723999674418435726557523519535770015019287382321071804865681731226989916286199314883016472947639367666251368202759691810399195092598892275413777035275182318485652713871000041272524440519262054008953943029365257325370839037761555465335452562216651250516983405134378252470216494582635109781712938341456418881 + 1/2039986670246850822853427080268636607703538330430958135006350872460188775376402385474575383380701179275926633909293920375037781006938834602683282504456671345800481611955974906577358109966753513899436209725756764159504134559394933538420714469300931804842468643272796657406808805007786178371184391663721349034183315512035012402176731111044506314978549915206516847224339930494935465558632905912262959736737614637514921726288403470224139024425700070180324623265095949577758695292697562554242228453440276043742370033993859881981612938703208463591285870376619588297958810138295747858827756577616148419423031480258559516303907719233914603343421735341220080271152090557188286289527661792734931298102513902518914250419121432886312102736349552224188669212688846219382874287241971706387850290821170997846726526589069990513808709560793139660289273086403155344460608865436195352720549406793512677065107181955781264579349071905411393100989250722104770801720673437692418988638492506057962758754921169589084980707251205329924087857682559921447010465898318288868258062129919867004394488124710647843586978379399594154917914477913086776811741840849911967039211773201428676384229432761943488196359561416605048969002045397348240530911560634680322446588472763785839765588633770016209055874572792498932175778494089116461654628549726895871636209026849103988563732410165441 = 641/796
1/13 + 1/541 + 1/321409 + 1/114781617793 + 1/14821672255960844346913 + 1/251065106814993628596500876449600804290086881 + 1/73539302503361520198362339236500915390885795679264404865887253300925727812630083326272641 + 1/6489634815217096741758907148982381236931234341288936993640630568353888026513046373352130623124225014404918014072680355409470797372507720812828610332359154836067922616607391865217 + 1/52644200043597301715163084170074049765863371513744701000308778672552161021188727897845435784419167578097570308323221316037189809321236639774156001218848770417914304730719451756764847141999454715415348579218576135692260706546084789833559164567239198064491721524233401718052341737694961761810858726456915514545036448002629051435498625211733293978125476206145 + 1/3695215730973720191743335450900515442837964059737103132125137784392340041085824276783333540815086968586494259680343732030671448522298751008735945486795776365973142745077411841504712940444458881229478108614230774637316342940593842925604630011475333378620376362943942755446627099104200059416153812858633723638212819657597061963458758259287734950993940819872945202809437805131650984566124057319228963533088559443909352453788455968978250113376533423265233637558939144535732287317303130488802163512444658441011602922480039143050047663394967808639154754442570791381496210122415541628843804495020590646687354364355396925939868087995781911240513904752765014910531863571167632659092232428610030201325032663259931238141889 + 1/20481928947653467858867964360215698922460866349989714221296388791180533521147068328398292448571350580917144516243144419767021450972552458770890215041236338405232471846144964422722088363577942656244304369314740680337368003341749927848292268159627280776486153786277410225081205358330757686606252814923029488556248114378465151886875778980493919811102286892641254175976181063891774788890129279669791215911728886439002027991447164421080590166911130116483359749418047307595497010369457711350953018694479942850146580996402187310635505278301929397030213544531068769667892360925519410013180703331321321833900350008776368272790481252519169303988218210095146759870287941250090204506960847016059468728275311477613271084474766715488264771177830115028195215223644336345646870679050787515340804351339449474385172464387868299006904638274425855008729765086091731260299397062138670321522563954731398813138738073326593694555049353805161855854036423870334342280080335804850998490793742536882308453307029152812821729798744074167237835462214043679643723245065093600037959124662392297413473130606861784229249604290090458912391096328362137163951398211801143455350336317188806956746282700489013366856863803112203078858200161688528939040348825835610989725020068306497091337571398894447440161081470240965873628208205669354804691958270783090585006358905094926094885655359774269830169287513005586562246433405044654325439410730648108371520856384706590593 + 1/839018826833450186636781520007011999269820404906753180244759299287837378895397605613261469995626498719289835112392530430840514102146998625666594756995273418015600023494049208108894185781774002683063204252356172520941088783702738286944210460710059319691268110283467445381026653628599765684739105388642310044785844902157076919003735231543781785073393176144167688252446541416466418608465458502997971425428342769433127784560570193376772878336217849260872114137931351960543608384244009505664253173875705234889570853924105640193619301332776989688248555027054395237907581951261868280899150574360164800187964167274323078311078867593844043149124596271281252530924719121766925749760855109100066731841478262812686642693395896229983745226277793055820609058348269152190083695704685769622011655159174272326647342695589818127126303038171968768650476413027459205291075571637957597356820188031655122749743652301268394542123970892422944335857917641636041892192547135178153602038877677614358281581103685526041329841496863410305888255234495015115912388514981113593387572720476744188169200130515719608747338810136728267784013352396910979904545913458536243327311977805126410065576961237640824852114328884086581542091492600312838425666927627674227053793897767395465326589843035773944346372949759909905561209334216847158156644884281300512699910530092870919061876615770708519243818676366245477462042294267674677954783726990349386117468071932874021023714524610740225814235147693954027910741673103980749749728106483987721602738673173009362802337092908847797499475895347112889339502928407808058670297722175686638678788738689803945574002805677250463286479363670076942509109589495377221095405979217163821481666646160815221224686562530536116613645305335922819524037829878961518170177968768364853399057357772141655622381280196908637031556436461404285930426436983658106288733881761514992109680298995922754466040011586713812553117621857109517258943846004179432521131844156242428351270188803919554398620084668514054504414062276012292497375238210886595006249453460414790147611422121782194848803348777061816460876697945418158442269512987729152441940326466631610424906158237288218706447963113019239557885486647314085357651895226117364760315394354624547919209138539180807829672545924239541758108877100331729470119526373928796447673951888289511964811633025369821156695934557103429921063387965046715070102916811976552584464153981214277622597308113449320462341683055200576571910241686615924531368198770946893858410058348221985603151428153382461711196734214085852523778422630907646235900752317571022131569421231196329080023952364788544301495422061066036911772385739659997665503832444529713544286955548310166168837889046149061296461059432238621602179724809510024772127497080258401694929973105184832214622785679651550368465524821062859837409907538269572622296774545103747438431266995525592705 = 36/457
```



## Microsoft Small Basic

Small Basic but large (not huge) integers.

```smallbasic
'Egyptian fractions - 26/07/2018
  xx=2014
  yy=59
  x=xx
  y=yy
  If x>=y Then
    q=Math.Floor(x/y)
    tt="+("+q+")"
    x=Math.Remainder(x,y)
  EndIf
  If x<>0 Then
    While x<>1
      'i=modulo(-y,x)
      u=-y
      v=x
      modulo()
      i=ret
      k=Math.Ceiling(y/x)
      m=m+1
      tt=tt+"+1/"+k
      j=y*k
      If i=1 Then
        tt=tt+"+1/"+j
      EndIf
      'n=gcd(i,j)
      x=i
      y=j
      gcd()
      n=ret
      x=i/n
      y=j/n
    EndWhile
  EndIf
  TextWindow.WriteLine(xx+"/"+yy+"="+Text.GetSubTextToEnd(tt,2))

Sub modulo
  wr=Math.Remainder(u,v)
  While wr<0
    wr=wr+v
  EndWhile
  ret=wr
EndSub

Sub gcd
  wx=i
  wy=j
  wr=1
  While wr<>0
    wr=Math.Remainder(wx,wy)
    wx=wy
    wy=wr
  EndWhile
  ret=wx
EndSub
```

{{out}}
 43/48=1/2+1/3
 5/121=1/25+1/757+1/763309+1/873960180913+1/1527612795642093418846225
 2014/59=(34)+1/8+1/95+1/14947+1/670223480


## PARI/GP


```parigp

efrac(f)=my(v=List());while(f,my(x=numerator(f),y=denominator(f));listput(v,ceil(y/x));f=(-y)%x/y/v[#v]);Vec(v);
show(f)=my(n=f\1,v=efrac(f-n)); print1(f" = ["n"; "v[1]); for(i=2,#v,print1(", "v[i])); print("]");
best(n)=my(denom,denomAt,term,termAt,v); for(a=1,n-1,for(b=a+1,n, v=efrac(a/b); if(#v>term, termAt=a/b; term=#v); if(v[#v]>denom, denomAt=a/b; denom=v[#v]))); print("Most terms is "termAt" with "term); print("Biggest denominator is "denomAt" with "denom)
apply(show, [43/48, 5/121, 2014/59]);
best(9)
best(99)
best(999)

```

{{out}}

```txt
43/48 = [0; 2, 3, 16]
5/121 = [0; 25, 757, 763309, 873960180913, 1527612795642093418846225]
2014/59 = [34; 8, 95, 14947, 670223480]

Most terms is 3/7 with 3
Biggest denominator is 3/7 with 231

Most terms is 8/97 with 8
Biggest denominator is 8/97 with 579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665

Most terms is 529/914 with 13
Biggest denominator is 36/457 with 839...705
```


## Perl


```perl
use strict;
use warnings;
use bigint;
sub isEgyption{
    my $nr = int($_[0]);
    my $de = int($_[1]);
    if($nr == 0 or $de == 0){
	#Invalid input
	return;
    }
    if($de % $nr == 0){
	# They divide so print
	printf "1/" . int($de/$nr);
	return;
    }
    if($nr % $de == 0){
	# Invalid fraction
	printf $nr/$de;
	return;
    }
    if($nr > $de){
        printf int($nr/$de) . " + ";
	isEgyption($nr%$de, $de);
	return;
    }
    # Floor to find ceiling and print as fraction
    my $tmp = int($de/$nr) + 1;
    printf "1/" . $tmp . " + ";
    isEgyption($nr*$tmp-$de, $de*$tmp);
}

my $nrI = 2014;
my $deI = 59;
printf "\nEgyptian Fraction Representation of " . $nrI . "/" . $deI . " is: \n\n";
isEgyption($nrI,$deI);

```

{{out}}

```txt

Egyptian Fraction Representation of 2014/59 is:
34 + 1/8 + 1/95 + 1/14947 + 1/670223480

```


## Perl 6


```perl6
role Egyptian {
    method gist {
	join ' + ',
	    ("[{self.floor}]" if self.abs >= 1),
	    map {"1/$_"}, self.denominators;
    }
    method denominators {
	my ($x, $y) = self.nude;
	$x %= $y;
	my @denom = gather ($x, $y) = -$y % $x, $y * take ($y / $x).ceiling
	    while $x;
    }
}

say .nude.join('/'), " = ", $_ but Egyptian for 43/48, 5/121, 2014/59;

my @sample = map { $_ => .denominators },
    grep * < 1,
        map {$_ but Egyptian},
            (2 .. 99 X/ 2 .. 99);

say .key.nude.join("/"),
    " has max denominator, namely ",
    .value.max
        given max :by(*.value.max), @sample;

say .key.nude.join("/"),
    " has max number of denominators, namely ",
    .value.elems
        given max :by(*.value.elems), @sample;
```

{{out}}

```txt
43/48 = 1/2 + 1/3 + 1/16
5/121 = 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225
2014/59 = [34] + 1/8 + 1/95 + 1/14947 + 1/670223480
8/97 has max denominator, namely 579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665
8/97 has max number of denominators, namely 8
```


Because the harmonic series diverges (albeit very slowly), it is possible to write even improper fractions as a sum of distinct unit fractions.  Here is a code to do that:


```perl6
role Egyptian {
    method gist { join ' + ', map {"1/$_"}, self.list }
    method list {
	my $sum = 0;
	gather for 2 .. * {
	    last if $sum == self;
	    $sum += 1 / .take unless $sum + 1 / $_ > self;
	}
    }
}

say 5/4 but Egyptian;
```

{{out}}

```txt
1/2 + 1/3 + 1/4 + 1/6
```


The list of terms grows exponentially with the value of the fraction, though.


## Phix

{{trans|tcl}}
{{libheader|mpfr}}
The sieve copied from Go

```Phix
include mpfr.e
function egyptian(integer num, denom)
    mpz n = mpz_init(num),
        d = mpz_init(denom),
        t = mpz_init()
    sequence result = {}
    while mpz_cmp_si(n,0)!=0 do
        mpz_cdiv_q(t, d, n)
        result = append(result,"1/"&mpz_get_str(t))
        mpz_neg(d,d)
        mpz_mod(n,d,n)
        mpz_neg(d,d)
        mpz_mul(d,d,t)
    end while
    {n,d} = mpz_free({n,d})
    return result
end function

procedure efrac(integer num, denom)
    string fraction = sprintf("%d/%d",{num,denom}),
           prefix = ""
    if num>=denom then
        integer whole = floor(num/denom)
        num -= whole*denom
        prefix = sprintf("[%d] + ",whole)
    end if
    string e = join(egyptian(num, denom)," + ")
    printf(1,"%s -> %s%s\n",{fraction,prefix,e})
end procedure

efrac(43,48)
efrac(5,121)
efrac(2014,59)

integer maxt = 0,
        maxd = 0
string maxts = "",
       maxds = "",
       maxda = ""

for r=98 to 998 by 900 do   -- (iterates just twice!)
    sequence sieve = repeat(repeat(false,r+1),r) -- to eliminate duplicates
    for n=1 to r do
        for d=n+1 to r+1 do
            if sieve[n][d]=false then
                string term = sprintf("%d/%d",{n,d})
                sequence terms = egyptian(n,d)
                integer nterms = length(terms)
                if nterms>maxt then
                    maxt = nterms
                    maxts = term
                elsif nterms=maxt then
                    maxts &= ", " & term
                end if
                integer mlen = length(terms[$])-2
                if mlen>maxd then
                    maxd = mlen
                    maxds = term
                    maxda = terms[$]
                elsif mlen=maxd then
                    maxds &= ", " & term
                end if
                if n<r/2 then
                    for k=2 to 9999 do
                        if d*k > r+1 then exit end if
                        sieve[n*k][d*k] = true
                    end for
                end if
            end if
        end for
    end for
    printf(1,"\nfor proper fractions with 1 to %d digits\n",{length(sprint(r))})
    printf(1,"Largest number of terms is %d for %s\n",{maxt,maxts})
    maxda = maxda[3..$] -- (strip the "1/")
    maxda[6..-6]="..."  -- (show only first/last 5 digits)
    printf(1,"Largest size for denominator is %d digits (%s) for %s\n",{maxd,maxda,maxds})
end for
```

{{out}}

```txt

43/48 -> 1/2 + 1/3 + 1/16
5/121 -> 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225
2014/59 -> [34] + 1/8 + 1/95 + 1/14947 + 1/670223480

for proper fractions with 1 to 2 digits
Largest number of terms is 8 for 8/97, 44/53
Largest size for denominator is 150 digits (57950...89665) for 8/97

for proper fractions with 1 to 3 digits
Largest number of terms is 13 for 529/914, 641/796
Largest size for denominator is 2847 digits (83901...92705) for 36/457, 529/914

```



## Python


### Procedural


```python
from fractions import Fraction
from math import ceil

class Fr(Fraction):
    def __repr__(self):
        return '%s/%s' % (self.numerator, self.denominator)

def ef(fr):
    ans = []
    if fr >= 1:
        if fr.denominator == 1:
            return [[int(fr)], Fr(0, 1)]
        intfr = int(fr)
        ans, fr = [[intfr]], fr - intfr
    x, y = fr.numerator, fr.denominator
    while x != 1:
        ans.append(Fr(1, ceil(1/fr)))
        fr = Fr(-y % x, y* ceil(1/fr))
        x, y = fr.numerator, fr.denominator
    ans.append(fr)
    return ans

if __name__ == '__main__':
    for fr in [Fr(43, 48), Fr(5, 121), Fr(2014, 59)]:
        print('%r ─► %s' % (fr, ' '.join(str(x) for x in ef(fr))))
    lenmax = denommax = (0, None)
    for fr in set(Fr(a, b) for a in range(1,100) for b in range(1, 100)):
        e = ef(fr)
        #assert sum((f[0] if type(f) is list else f) for f in e) == fr, 'Whoops!'
        elen, edenom = len(e), e[-1].denominator
        if elen > lenmax[0]:
            lenmax = (elen, fr, e)
        if edenom > denommax[0]:
            denommax = (edenom, fr, e)
    print('Term max is %r with %i terms' % (lenmax[1], lenmax[0]))
    dstr = str(denommax[0])
    print('Denominator max is %r with %i digits %s...%s' %
          (denommax[1], len(dstr), dstr[:5], dstr[-5:]))
```


{{out}}

```txt
43/48 ─► 1/2 1/3 1/16
5/121 ─► 1/25 1/757 1/763309 1/873960180913 1/1527612795642093418846225
2014/59 ─► [34] 1/8 1/95 1/14947 1/670223480
Term max is 97/53 with 9 terms
Denominator max is 8/97 with 150 digits 57950...89665
```



### Composition of pure functions

The derivation of a sequence of unit fractions from a single fraction is a classic case of an anamorphism or '''unfold''' abstraction – dual to a fold or catamorphism. Rather than reducing, collapsing or summarizing a structure '''to''' a single value, it builds a structure '''from''' a single value.

See the '''unfoldr''' function below:
{{Works with|Python|3.7}}

```python
'''Egyptian fractions'''

from fractions import Fraction
from functools import reduce
from operator import neg


# eqyptianFraction :: Ratio Int -> Ratio Int
def eqyptianFraction(nd):
    '''The rational number nd as a sum
       of the series of unit fractions
       obtained by application of the
       greedy algorithm.'''
    def go(x):
        n, d = x.numerator, x.denominator
        r = 1 + d // n if n else None
        return Just((0, x) if 1 == n else (
            (fr(n % d, d), fr(n // d, 1)) if n > d else (
                fr(-d % n, d * r), fr(1, r)
            )
        )) if n else Nothing()
    fr = Fraction
    f = unfoldr(go)
    return list(map(neg, f(-nd))) if 0 > nd else f(nd)


# TESTS ---------------------------------------------------

# maxEqyptianFraction :: Int -> (Ratio Int -> a)
#                               -> (Ratio Int, a)
def maxEqyptianFraction(nDigits):
    '''An Egyptian Fraction, representing a
       proper fraction with numerators and
       denominators of up to n digits each,
       which returns a maximal value for the
       supplied function f.'''

    # maxVals :: ([Ratio Int], a) -> (Ratio Int, a)
    #                               -> ([Ratio Int], a)
    def maxima(xsv, ndfx):
        xs, v = xsv
        nd, fx = ndfx
        return ([nd], fx) if fx > v else (
            xs + [nd], v
        ) if fx == v and nd not in xs else xsv

    # go :: (Ratio Int -> a) -> ([Ratio Int], a)
    def go(f):
        iLast = int(nDigits * '9')
        fs, mx = reduce(
            maxima, [
                (nd, f(eqyptianFraction(nd))) for nd in [
                    Fraction(n, d)
                    for n in enumFromTo(1)(iLast)
                    for d in enumFromTo(1 + n)(iLast)
                ]
            ],
            ([], 0)
        )
        return f.__name__ + ' -> [' + ', '.join(
            map(str, fs)
        ) + '] -> ' + str(mx)
    return lambda f: go(f)


# main :: IO ()
def main():
    '''Tests'''

    ef = eqyptianFraction
    fr = Fraction

    print('Three values as Eqyptian fractions:')
    print('\n'.join([
        str(fr(*nd)) + ' -> ' + ' + '.join(map(str, ef(fr(*nd))))
        for nd in [(43, 48), (5, 121), (2014, 59)]
    ]))

    # maxDenominator :: [Ratio Int] -> Int
    def maxDenominator(ef):
        return max(map(lambda nd: nd.denominator, ef))

    # maxTermCount :: [Ratio Int] -> Int
    def maxTermCount(ef):
        return len(ef)

    for i in [1, 2, 3]:
        print(
            '\nMaxima for proper fractions with up to ' + (
                str(i) + ' digit(s):'
            )
        )
        for f in [maxTermCount, maxDenominator]:
            print(maxEqyptianFraction(i)(f))


# GENERIC -------------------------------------------------


# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# unfoldr :: (b -> Maybe (b, a)) -> b -> [a]
def unfoldr(f):
    '''Dual to reduce or foldr.
       Where catamorphism reduces a list to a summary value,
       the anamorphic unfoldr builds a list from a seed value.
       As long as f returns Just(a, b), a is prepended to the list,
       and the residual b is used as the argument for the next
       application of f.
       When f returns Nothing, the completed list is returned.'''
    def go(xr):
        mb = f(xr[0])
        if mb.get('Nothing'):
            return []
        else:
            y, r = mb.get('Just')
            return [r] + go((y, r))

    return lambda x: go((x, x))


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Three values as Eqyptian fractions:
43/48 -> 1/2 + 1/3 + 1/16
5/121 -> 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225
2014/59 -> 34 + 1/8 + 1/95 + 1/14947 + 1/670223480

Maxima for proper fractions with up to 1 digit(s):
maxTermCount -> [3/7, 4/5, 5/7, 6/7, 7/8, 7/9, 8/9] -> 3
maxDenominator -> [3/7] -> 231

Maxima for proper fractions with up to 2 digit(s):
maxTermCount -> [8/97, 44/53] -> 8
maxDenominator -> [8/97] -> 579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665

Maxima for proper fractions with up to 3 digit(s):
maxTermCount -> [529/914, 641/796] -> 13
maxDenominator -> [36/457, 529/914] -> 839018826833450186636781520007011999269820404906753180244759299287837378895397605613261469995626498719289835112392530430840514102146998625666594756995273418015600023494049208108894185781774002683063204252356172520941088783702738286944210460710059319691268110283467445381026653628599765684739105388642310044785844902157076919003735231543781785073393176144167688252446541416466418608465458502997971425428342769433127784560570193376772878336217849260872114137931351960543608384244009505664253173875705234889570853924105640193619301332776989688248555027054395237907581951261868280899150574360164800187964167274323078311078867593844043149124596271281252530924719121766925749760855109100066731841478262812686642693395896229983745226277793055820609058348269152190083695704685769622011655159174272326647342695589818127126303038171968768650476413027459205291075571637957597356820188031655122749743652301268394542123970892422944335857917641636041892192547135178153602038877677614358281581103685526041329841496863410305888255234495015115912388514981113593387572720476744188169200130515719608747338810136728267784013352396910979904545913458536243327311977805126410065576961237640824852114328884086581542091492600312838425666927627674227053793897767395465326589843035773944346372949759909905561209334216847158156644884281300512699910530092870919061876615770708519243818676366245477462042294267674677954783726990349386117468071932874021023714524610740225814235147693954027910741673103980749749728106483987721602738673173009362802337092908847797499475895347112889339502928407808058670297722175686638678788738689803945574002805677250463286479363670076942509109589495377221095405979217163821481666646160815221224686562530536116613645305335922819524037829878961518170177968768364853399057357772141655622381280196908637031556436461404285930426436983658106288733881761514992109680298995922754466040011586713812553117621857109517258943846004179432521131844156242428351270188803919554398620084668514054504414062276012292497375238210886595006249453460414790147611422121782194848803348777061816460876697945418158442269512987729152441940326466631610424906158237288218706447963113019239557885486647314085357651895226117364760315394354624547919209138539180807829672545924239541758108877100331729470119526373928796447673951888289511964811633025369821156695934557103429921063387965046715070102916811976552584464153981214277622597308113449320462341683055200576571910241686615924531368198770946893858410058348221985603151428153382461711196734214085852523778422630907646235900752317571022131569421231196329080023952364788544301495422061066036911772385739659997665503832444529713544286955548310166168837889046149061296461059432238621602179724809510024772127497080258401694929973105184832214622785679651550368465524821062859837409907538269572622296774545103747438431266995525592705
```



## Racket



```racket
#lang racket
(define (real->egyptian-list R)
  (define (inr r rv)
    (match* ((exact-floor r) (numerator r) (denominator r))
      [(0 0 1) (reverse rv)]
      [(0 1 d) (reverse (cons (/ d) rv))]
      [(0 x y) (let ((^y/x (exact-ceiling (/ y x))))
                 (inr (/ (modulo (- y) x) (* y ^y/x)) (cons (/ ^y/x) rv)))]
      [(flr _ _) (inr (- r flr) (cons flr rv))]))
  (inr R null))

(define (real->egyptian-string f)
  (define e.f.-list (real->egyptian-list f))
  (define fmt-part
    (match-lambda
      [(? integer? (app number->string s)) s]
      [(app (compose number->string /) s) (format "/~a"s)]))
  (string-join (map fmt-part e.f.-list) " + "))

(define (stat-egyptian-fractions max-b+1)
  (define-values (max-l max-l-f max-d max-d-f)
    (for*/fold ((max-l 0) (max-l-f #f) (max-d 0) (max-d-f #f))
               ((b (in-range 1 max-b+1)) (a (in-range 1 b)) #:when (= 1 (gcd a b)))
      (define f (/ a b))
      (define e.f (real->egyptian-list (/ a b)))
      (define l (length e.f))
      (define d (denominator (last e.f)))
      (values (max max-l l) (if (> l max-l) f max-l-f)
              (max max-d d) (if (> d max-d) f max-d-f))))
  (printf #<<EOS
max #terms: ~a has ~a
[~.a]
max denominator: ~a has ~a
[~.a]

EOS
          max-l-f max-l (real->egyptian-string max-l-f)
          max-d-f max-d (real->egyptian-string max-d-f)))

(displayln (real->egyptian-string 43/48))
(displayln (real->egyptian-string 5/121))
(displayln (real->egyptian-string 2014/59))
(newline)
(stat-egyptian-fractions 100)
(newline)
(stat-egyptian-fractions 1000)

(module+ test (require tests/eli-tester)
  (test (real->egyptian-list 43/48) => '(1/2 1/3 1/16)))
```


{{out}}
(Line continuations have been manually added to this "post-production")

```txt
/2 + /3 + /16
/25 + /757 + /763309 + /873960180913 + /1527612795642093418846225
34 + /8 + /95 + /14947 + /670223480

max #terms: 44/53 has 8
[/2 + /4 + /13 + /307 + /120871 + /20453597227 + /697249399186783218655 + /1458\
470173998990524806872692984177836808420]
max denominator: 8/97 has 57950458706754280171310319185991860825103029195219542\
3583529357653899418686342360361798689053273749372615043661810228371898539583862\
011424993909789665
[/13 + /181 + /38041 + /1736503177 + /3769304102927363485 + /189435378937934085\
04192074528154430149 + /5382864419003802113658172851049070863474397461302269732\
53778132494225813153 + /5795045870675428017131031918599186082510302919521954235\
83529357653...]

max #terms: 641/796 has 13
[/2 + /4 + /19 + /379 + /159223 + /28520799973 + /929641178371338400861 + /1008\
271507277592391123742528036634174730681 + /121993371886539365536463536806812475\
6713122928811333803786753398211072842948484537833 + /18602978480309366547426083\
99135821395...]
max denominator: 36/457 has 839018826833450186636781520007011999269820404906753\
1802447592992878373788953976056132614699956264987192898351123925304308405141021\
4699862566659475699527341801560002349404920810889418578177400268306320425235617\
2520941088783702738286944210460710059319691268110283467445381026653628599765684\
7391053886423100447858449021570769190037352315437817850733931761441676882524465\
4141646641860846545850299797142542834276943312778456057019337677287833621784926\
0872114137931351960543608384244009505664253173875705234889570853924105640193619\
3013327769896882485550270543952379075819512618682808991505743601648001879641672\
7432307831107886759384404314912459627128125253092471912176692574976085510910006\
6731841478262812686642693395896229983745226277793055820609058348269152190083695\
7046857696220116551591742723266473426955898181271263030381719687686504764130274\
5920529107557163795759735682018803165512274974365230126839454212397089242294433\
5857917641636041892192547135178153602038877677614358281581103685526041329841496\
8634103058882552344950151159123885149811135933875727204767441881692001305157196\
0874733881013672826778401335239691097990454591345853624332731197780512641006557\
6961237640824852114328884086581542091492600312838425666927627674227053793897767\
3954653265898430357739443463729497599099055612093342168471581566448842813005126\
9991053009287091906187661577070851924381867636624547746204229426767467795478372\
6990349386117468071932874021023714524610740225814235147693954027910741673103980\
7497497281064839877216027386731730093628023370929088477974994758953471128893395\
0292840780805867029772217568663867878873868980394557400280567725046328647936367\
0076942509109589495377221095405979217163821481666646160815221224686562530536116\
6136453053359228195240378298789615181701779687683648533990573577721416556223812\
8019690863703155643646140428593042643698365810628873388176151499210968029899592\
2754466040011586713812553117621857109517258943846004179432521131844156242428351\
2701888039195543986200846685140545044140622760122924973752382108865950062494534\
6041479014761142212178219484880334877706181646087669794541815844226951298772915\
2441940326466631610424906158237288218706447963113019239557885486647314085357651\
8952261173647603153943546245479192091385391808078296725459242395417581088771003\
3172947011952637392879644767395188828951196481163302536982115669593455710342992\
1063387965046715070102916811976552584464153981214277622597308113449320462341683\
0552005765719102416866159245313681987709468938584100583482219856031514281533824\
6171119673421408585252377842263090764623590075231757102213156942123119632908002\
3952364788544301495422061066036911772385739659997665503832444529713544286955548\
3101661688378890461490612964610594322386216021797248095100247721274970802584016\
9492997310518483221462278567965155036846552482106285983740990753826957262229677\
4545103747438431266995525592705
[/13 + /541 + /321409 + /114781617793 + /14821672255960844346913 + /25106510681\
4993628596500876449600804290086881 + /73539302503361520198362339236500915390885\
795679264404865887253300925727812630083326272641 + /648963481521709674175890714\
89823812369...]
1 test passed
```



## REXX


```rexx
/*REXX program converts a fraction (can be improper) to an Egyptian fraction. */
parse arg fract '' -1 t;  z=$egyptF(fract)  /*compute the Egyptian fraction.  */
if t\==.  then say  fract   ' ───► '   z    /*show Egyptian fraction from C.L.*/
return z                                    /*stick a fork in it,  we're done.*/
/*────────────────────────────────$EGYPTF subroutine──────────────────────────*/
$egyptF: parse arg z 1 zn '/' zd,,$;   if zd==''  then zd=1   /*whole number ?*/
if z=''               then call erx  "no fraction was specified."
if zd==0              then call erx  "denominator can't be zero:"       zd
if zn==0              then call erx  "numerator can't be zero:"         zn
if zd<0 | zn<0        then call erx  "fraction can't be negative"       z
if \datatype(zn,'W')  then call erx  "numerator must be an integer:"    zn
if \datatype(zd,'W')  then call erx  "denominator must be an integer:"  zd
_=zn%zd                                /*check if it's an improper fraction.  */
if _>=1  then do                       /*if improper fraction, then append it.*/
              $='['_"]"                /*append the whole # part of fraction. */
              zn=zn-_*zd               /*now, just use the proper fraction.   */
              if zn==0  then return $  /*Is there no fraction? Then we're done*/
              end
if zd//zn==0  then do;  zd=zd%zn;  zn=1;  end
  do  forever
  if zn==1 & datatype(zd,'W')  then return $ "1/"zd   /*append Egyptian fract.*/
  nd=zd%zn+1;      $=$ '1/'nd          /*add unity to integer fraction, append*/
  z=$fractSub(zn'/'zd,  "-",  1'/'nd)  /*go and subtract the two fractions.   */
  parse var z zn '/' zd                /*extract the numerator and denominator*/
  L=2*max(length(zn),length(zd))       /*calculate if need more decimal digits*/
  if L>=digits()  then numeric digits L+L  /*yes, then bump the decimal digits*/
  end   /*forever*/                    /* [↑]  the DO forever ends when zn==1.*/
/*────────────────────────────────$FRACTSUB subroutine────────────────────────*/
$fractSub: procedure;  parse arg z.1,,z.2 1 zz.2;  arg ,op
                             do j=1  for 2;    z.j=translate(z.j,'/',"_");   end
if z.1==''  then z.1=(op\=="+" & op\=='-')     /*unary +,-     first fraction.*/
if z.2==''  then z.2=(op\=="+" & op\=='-')     /*unary +.-    second fraction.*/
  do j=1  for 2                                /*process both of the fractions*/
  if pos('/',z.j)==0     then z.j=z.j"/1";     parse var  z.j  n.j  '/'  d.j
  if \datatype(n.j,'N')  then call erx  "numerator isn't an integer:"    n.j
  if \datatype(d.j,'N')  then call erx  "denominator isn't an integer:"  d.j
  n.j=n.j/1;   d.j=d.j/1                    /*normalize numerator/denominator.*/

      do  while \datatype(n.j,'W');  n.j=n.j*10/1;  d.j=d.j*10/1;  end /*while*/
                                            /* [↑]  normalize both numbers.   */
  if d.j=0  then call erx  "denominator can't be zero:"   z.j
  g=gcd(n.j,d.j);   if g=0  then iterate;     n.j=n.j/g;         d.j=d.j/g
  end    /*j*/
l=lcm(d.1 d.2);             do j=1  for 2;  n.j=l*n.j/d.j;  d.j=l;  end  /*j*/
if op=='-'  then n.2=-n.2
t=n.1+n.2;       u=l;                  if t==0  then return 0
g=gcd(t,u);      t=t/g;     u=u/g;     if u==1  then return t
                                                     return t'/'u
/*─────────────────────────────general 1─line subs────────────────────────────*/
erx:  say;  say '***error!***' arg(1);       say;          exit 13
gcd:procedure;$=;do i=1 for arg();$=$ arg(i);end;parse var $ x z .;if x=0 then x=z;x=abs(x);do j=2 to words($);y=abs(word($,j));if y=0 then iterate;do until _==0;_=x//y;x=y;y=_;end;end;return x
lcm:procedure;y=;do j=1 for arg();y=y arg(j);end;x=word(y,1);do k=2 to words(y);!=abs(word(y,k));if !=0 then return 0;x=x*!/gcd(x,!);end;return x
p:  return word(arg(1),1)
```

'''output'''   when the input used is:   <tt> 43/48 </tt>

```txt


43/48  ───►   1/2 1/3 1/16

```


'''output''' when the input used is:   <tt> 5/121 </tt>

```txt

5/121  ───►   1/25 1/757 1/763309 1/873960180913 1/1527612795642093418846225

```


'''output''' when the input used is:   <tt> 2014/59 </tt>

```txt

2014/59  ───►   [34] 1/8 1/95 1/14947 1/670223480

```


The following is a driver program to address the requirements to find
the largest number of terms for a

1- or 2-digit integer, and the largest denominator.

Also, the same program is used for the 1-, 2-, and 3-digit extra credit task.

```rexx
/*REXX pgm runs the EGYPTIAN program to find biggest denominator & # of terms.*/
parse arg top .                        /*get optional parameter from the C.L. */
if top==''  then top=99                /*Not specified?  Then use the default.*/
oTop=top;   top=abs(top)               /*oTop used as a flag to display maxD. */
maxT=0;     maxD=0;     bigD=;   bigT= /*initialize some REXX variables.      */
                                       /* [↓]  determine biggest andlongest.  */
      do n=2      to top               /*traipse through the  numerators.     */
          do d=n+1  to top             /*   "       "     "  denominators     */
          fract=n'/'d                  /*create the fraction to be used.      */
          y='EGYPTIAN'(fract||.)       /*invoke the REXX program  EGYPTIAN.REX*/
          t=words(y)                   /*number of terms in Egyptian fraction.*/
          if t>maxT  then bigT=fract   /*is this a new high for number terms? */
          maxT=max(maxT,T)             /*find the maximum number of terms.    */
          b=substr(word(y,t),3)        /*get denominator from Egyptian fract. */
          if b>maxD  then bigD=fract   /*is this a new denominator high ?     */
          maxD=max(maxD,b)             /*find the maximum denominator.        */
          end   /*d*/                  /* [↑]  only use proper fractions.     */
      end       /*n*/                  /* [↑]  ignore the   1/n   fractions.  */
                                       /* [↑]  display the longest and biggest*/
@= 'in the Egyptian fractions used is' /*literal is used to make a shorter SAY*/
say 'largest number of terms'  @   maxT   "terms for"   bigT
say
say 'highest denominator'      @   length(maxD)   "digits for"  bigD
if oTop>0  then say maxD               /*stick a fork in it,  we're all done. */
```

'''output'''   for all 1- and 2-digit integers when using the default input:

```txt

largest number of terms in the Egyptian fractions used is 8 terms for 8/97
largest denominator in the Egyptian fractions is 150 digits is for 8/97
579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665

```

'''output'''   for all 1-, 2-, and 3-digit integers when using for input:   <tt> -999 </tt>

```txt

largest number of terms in the Egyptian fractions used is 13 terms for 529/914
largest denominator in the Egyptian fractions is 2847 digits is for 36/457

```



## Ruby

{{trans|Python}}

```ruby
def ef(fr)
  ans = []
  if fr >= 1
    return [[fr.to_i], Rational(0, 1)]  if fr.denominator == 1
    intfr = fr.to_i
    ans, fr = [intfr], fr - intfr
  end
  x, y = fr.numerator, fr.denominator
  while x != 1
    ans << Rational(1, (1/fr).ceil)
    fr = Rational(-y % x, y * (1/fr).ceil)
    x, y = fr.numerator, fr.denominator
  end
  ans << fr
end

for fr in [Rational(43, 48), Rational(5, 121), Rational(2014, 59)]
  puts '%s => %s' % [fr, ef(fr).join(' + ')]
end

lenmax = denommax = [0]
for b in 2..99
  for a in 1...b
    fr = Rational(a,b)
    e = ef(fr)
    elen, edenom = e.length, e[-1].denominator
    lenmax = [elen, fr] if elen > lenmax[0]
    denommax = [edenom, fr] if edenom > denommax[0]
  end
end
puts 'Term max is %s with %i terms' % [lenmax[1], lenmax[0]]
dstr = denommax[0].to_s
puts 'Denominator max is %s with %i digits' % [denommax[1], dstr.size], dstr
```


{{out}}

```txt

43/48 => 1/2 + 1/3 + 1/16
5/121 => 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225
2014/59 => 34 + 1/8 + 1/95 + 1/14947 + 1/670223480
Term max is 44/53 with 8 terms
Denominator max is 8/97 with 150 digits
579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665

```



## Sidef

{{trans|Ruby}}

```ruby
func ef(fr) {
  var ans = []
  if (fr >= 1) {
    return([fr]) if (fr.is_int)
    var intfr = fr.int
    ans << intfr
    fr -= intfr
  }
  var (x, y) = fr.nude
  while (x != 1) {
    ans << fr.inv.ceil.inv
    fr = ((-y % x) / y*fr.inv.ceil)
    (x, y) = fr.nude
  }
  ans << fr
  return ans
}

for fr in [43/48, 5/121, 2014/59] {
  "%s => %s\n".printf(fr.as_rat, ef(fr).map{.as_rat}.join(' + '))
}

var lenmax = (var denommax = [0])
for b in range(2, 99) {
  for a in range(1, b-1) {
    var fr = a/b
    var e = ef(fr)
    var (elen, edenom) = (e.length, e[-1].denominator)
    lenmax = [elen, fr] if (elen > lenmax[0])
    denommax = [edenom, fr] if (edenom > denommax[0])
  }
}

"Term max is %s with %i terms\n".printf(lenmax[1].as_rat, lenmax[0])
"Denominator max is %s with %i digits\n".printf(denommax[1].as_rat, denommax[0].size)
say denommax[0]
```

{{out}}

```txt

43/48 => 1/2 + 1/3 + 1/16
5/121 => 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225
2014/59 => 34 + 1/8 + 1/95 + 1/14947 + 1/670223480
Term max is 44/53 with 8 terms
Denominator max is 8/97 with 150 digits
579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665

```



## Tcl


```tcl
# Just compute the denominator terms, as the numerators are always 1
proc egyptian {num denom} {
    set result {}
    while {$num} {
	# Compute ceil($denom/$num) without floating point inaccuracy
	set term [expr {$denom / $num + ($denom/$num*$num < $denom)}]
	lappend result $term
	set num [expr {-$denom % $num}]
	set denom [expr {$denom * $term}]
    }
    return $result
}
```

Demonstrating:
{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

proc efrac {fraction} {
    scan $fraction "%d/%d" x y
    set prefix ""
    if {$x > $y} {
	set whole [expr {$x / $y}]
	set x [expr {$x - $whole*$y}]
	set prefix "\[$whole\] + "
    }
    return $prefix[join [lmap y [egyptian $x $y] {format "1/%lld" $y}] " + "]
}

foreach f {43/48  5/121  2014/59} {
    puts "$f = [efrac $f]"
}
set maxt 0
set maxtf {}
set maxd 0
set maxdf {}
for {set d 1} {$d < 100} {incr d} {
    for {set n 1} {$n < $d} {incr n} {
	set e [egyptian $n $d]
	if {[llength $e] >= $maxt} {
	    set maxt [llength $e]
	    set maxtf $n/$d
	}
	if {[lindex $e end] > $maxd} {
	    set maxd [lindex $e end]
	    set maxdf $n/$d
	}
    }
}
puts "$maxtf has maximum number of terms = [efrac $maxtf]"
puts "$maxdf has maximum denominator = [efrac $maxdf]"
```

{{out}}

```txt

43/48 = 1/2 + 1/3 + 1/16
5/121 = 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1527612795642093418846225
2014/59 = [34] + 1/8 + 1/95 + 1/14947 + 1/670223480
8/97 has maximum number of terms = 1/13 + 1/181 + 1/38041 + 1/1736503177 + 1/3769304102927363485 + 1/18943537893793408504192074528154430149 + 1/538286441900380211365817285104907086347439746130226973253778132494225813153 + 1/579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665
8/97 has maximum denominator = 1/13 + 1/181 + 1/38041 + 1/1736503177 + 1/3769304102927363485 + 1/18943537893793408504192074528154430149 + 1/538286441900380211365817285104907086347439746130226973253778132494225813153 + 1/579504587067542801713103191859918608251030291952195423583529357653899418686342360361798689053273749372615043661810228371898539583862011424993909789665

```

Note also that <math>\tfrac{44}{53}</math> also has 8 terms.
:<math>\tfrac{1}{2} + \tfrac{1}{4} + \tfrac{1}{13} + \tfrac{1}{307} + \tfrac{1}{120871} + \tfrac{1}{20453597227} + \tfrac{1}{697249399186783218655} + \tfrac{1}{1458470173998990524806872692984177836808420}</math>


## Visual Basic .NET

{{trans|D}}

```vbnet
Imports System.Numerics
Imports System.Text

Module Module1

    Function Gcd(a As BigInteger, b As BigInteger) As BigInteger
        If b = 0 Then
            If a < 0 Then
                Return -a
            Else
                Return a
            End If
        Else
            Return Gcd(b, a Mod b)
        End If
    End Function

    Function Lcm(a As BigInteger, b As BigInteger) As BigInteger
        Return a / Gcd(a, b) * b
    End Function

    Public Class Rational
        Dim num As BigInteger
        Dim den As BigInteger

        Public Sub New(n As BigInteger, d As BigInteger)
            Dim c = Gcd(n, d)
            num = n / c
            den = d / c
            If den < 0 Then
                num = -num
                den = -den
            End If
        End Sub

        Public Sub New(n As BigInteger)
            num = n
            den = 1
        End Sub

        Public Function Numerator() As BigInteger
            Return num
        End Function

        Public Function Denominator() As BigInteger
            Return den
        End Function

        Public Overrides Function ToString() As String
            If den = 1 Then
                Return num.ToString()
            Else
                Return String.Format("{0}/{1}", num, den)
            End If
        End Function

        'Arithmetic operators
        Public Shared Operator +(lhs As Rational, rhs As Rational) As Rational
            Return New Rational(lhs.num * rhs.den + rhs.num * lhs.den, lhs.den * rhs.den)
        End Operator

        Public Shared Operator -(lhs As Rational, rhs As Rational) As Rational
            Return New Rational(lhs.num * rhs.den - rhs.num * lhs.den, lhs.den * rhs.den)
        End Operator

        'Comparison operators

        Public Shared Operator =(lhs As Rational, rhs As Rational) As Boolean
            Return lhs.num = rhs.num AndAlso lhs.den = rhs.den
        End Operator

        Public Shared Operator <>(lhs As Rational, rhs As Rational) As Boolean
            Return lhs.num <> rhs.num OrElse lhs.den <> rhs.den
        End Operator

        Public Shared Operator <(lhs As Rational, rhs As Rational) As Boolean
            'a/b < c/d
            'ad < bc
            Dim ad = lhs.num * rhs.den
            Dim bc = lhs.den * rhs.num
            Return ad < bc
        End Operator

        Public Shared Operator >(lhs As Rational, rhs As Rational) As Boolean
            'a/b > c/d
            'ad > bc
            Dim ad = lhs.num * rhs.den
            Dim bc = lhs.den * rhs.num
            Return ad > bc
        End Operator

        Public Shared Operator <=(lhs As Rational, rhs As Rational) As Boolean
            Return lhs < rhs OrElse lhs = rhs
        End Operator

        Public Shared Operator >=(lhs As Rational, rhs As Rational) As Boolean
            Return lhs > rhs OrElse lhs = rhs
        End Operator

        'Conversion operators
        Public Shared Widening Operator CType(ByVal bi As BigInteger) As Rational
            Return New Rational(bi)
        End Operator
        Public Shared Widening Operator CType(ByVal lo As Long) As Rational
            Return New Rational(lo)
        End Operator
    End Class

    Function Egyptian(r As Rational) As List(Of Rational)
        Dim result As New List(Of Rational)

        If r >= 1 Then
            If r.Denominator() = 1 Then
                result.Add(r)
                result.Add(New Rational(0))
                Return result
            End If
            result.Add(New Rational(r.Numerator / r.Denominator))
            r -= result(0)
        End If

        Dim modFunc = Function(m As BigInteger, n As BigInteger)
                          Return ((m Mod n) + n) Mod n
                      End Function

        While r.Numerator() <> 1
            Dim q = (r.Denominator() + r.Numerator() - 1) / r.Numerator()
            result.Add(New Rational(1, q))
            r = New Rational(modFunc(-r.Denominator(), r.Numerator()), r.Denominator * q)
        End While

        result.Add(r)
        Return result
    End Function

    Function FormatList(Of T)(col As List(Of T)) As String
        Dim iter = col.GetEnumerator()
        Dim sb As New StringBuilder

        sb.Append("[")
        If iter.MoveNext() Then
            sb.Append(iter.Current)
        End If
        While iter.MoveNext()
            sb.Append(", ")
            sb.Append(iter.Current)
        End While
        sb.Append("]")
        Return sb.ToString()
    End Function

    Sub Main()
        Dim rs = {New Rational(43, 48), New Rational(5, 121), New Rational(2014, 59)}
        For Each r In rs
            Console.WriteLine("{0} => {1}", r, FormatList(Egyptian(r)))
        Next

        Dim lenMax As Tuple(Of ULong, Rational) = Tuple.Create(0UL, New Rational(0))
        Dim denomMax As Tuple(Of BigInteger, Rational) = Tuple.Create(New BigInteger(0), New Rational(0))

        Dim query = (From i In Enumerable.Range(1, 100)
                     From j In Enumerable.Range(1, 100)
                     Select New Rational(i, j)).Distinct().ToList()
        For Each r In query
            Dim e = Egyptian(r)
            Dim eLen As ULong = e.Count
            Dim eDenom = e.Last().Denominator()
            If eLen > lenMax.Item1 Then
                lenMax = Tuple.Create(eLen, r)
            End If
            If eDenom > denomMax.Item1 Then
                denomMax = Tuple.Create(eDenom, r)
            End If
        Next

        Console.WriteLine("Term max is {0} with {1} terms", lenMax.Item2, lenMax.Item1)
        Dim dStr = denomMax.Item1.ToString()
        Console.WriteLine("Denominator max is {0} with {1} digits {2}...{3}", denomMax.Item2, dStr.Length, dStr.Substring(0, 5), dStr.Substring(dStr.Length - 5, 5))
    End Sub

End Module
```

{{out}}

```txt
43/48 => [1/2, 1/3, 1/16]
5/121 => [1/25, 1/757, 1/763309, 1/873960180913, 1/1527612795642093418846225]
2014/59 => [34, 1/8, 1/95, 1/14947, 1/670223480]
Term max is 97/53 with 9 terms
Denominator max is 8/97 with 150 digits 57950...89665
```



## zkl

{{trans|Tcl}}

```zkl
# Just compute the denominator terms, as the numerators are always 1
fcn egyptian(num,denom){
   result,t := List(),Void;
   t,num=num.divr(denom);      // reduce fraction
   if(t) result.append(T(t));  // signal t isn't a denominator
   while(num){
      # Compute ceil($denom/$num) without floating point inaccuracy
      term:=denom/num + (denom/num*num < denom);
      result.append(term);
      z:=denom%num;
      num=(if(z) num-z else 0);
      denom*=term;
   }
   result
}
fcn efrac(fraction){  // list to string, format list of denominators
   fraction.pump(List,fcn(denom){
      if(denom.isType(List)) denom[0]
      else 		     String("1/",denom);
   }).concat(" + ")
}
```


```zkl
foreach n,d in (T(T(43,48), T(5,121), T(2014,59))){
   println("%s/%s --> %s".fmt(n,d, egyptian(n,d):efrac(_)));
}
```

{{out}}

```txt

43/48 --> 1/2 + 1/3 + 1/16
5/121 --> 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1025410058030422033
2014/59 --> 34 + 1/8 + 1/95 + 1/14947 + 1/670223480

```

For the big denominators, use GMP (Gnu Multi Precision).

```zkl
var [const] BN=Import("zklBigNum");  // libGMP
lenMax,denomMax := List(0,Void),List(0,Void);
foreach n,d in (Walker.cproduct([1..99],[1..99])){ // 9801 fractions
   e,eLen,eDenom := egyptian(BN(n),BN(d)), e.len(), e[-1];
   if(eDenom.isType(List)) eDenom=1;
   if(eLen  >lenMax[0])   lenMax.clear(eLen,T(n,d));
   if(eDenom>denomMax[0]) denomMax.clear(eDenom,T(n,d));
}
println("Term max is %s/%s with %d terms".fmt(lenMax[1].xplode(), lenMax[0]));
dStr:=denomMax[0].toString();
println("Denominator max is %s/%s with %d digits %s...%s"
    .fmt(denomMax[1].xplode(), dStr.len(), dStr[0,5], dStr[-5,*]));
```

{{out}}

```txt

Term max is 97/53 with 9 terms
Denominator max is 8/97 with 150 digits 57950...89665

```

