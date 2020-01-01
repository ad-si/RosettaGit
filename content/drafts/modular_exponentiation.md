+++
title = "Modular exponentiation"
description = ""
date = 2019-08-20T14:02:42Z
aliases = []
[extra]
id = 11080
[taxonomies]
categories = []
tags = []
+++

{{task}}
Find the last 40 decimal digits of <math>a^b</math>, where

* <math>a = 2988348162058574136915891421498819466320163312926952423791023078876139</math>
* <math>b = 2351399303373464486466122544523690094744975233415544072992656881240319</math>



A computer is too slow to find the entire value of <math>a^b</math>.

Instead, the program must use a fast algorithm for [[wp:Modular exponentiation|modular exponentiation]]: <math>a^b \mod m</math>.

The algorithm must work for any integers <math>a, b, m</math>
where <math>b \ge 0</math> and <math>m > 0</math>.





## Ada


Using the big integer implementation from a cryptographic library [https://github.com/cforler/Ada-Crypto-Library/].


```Ada
with Ada.Text_IO, Ada.Command_Line, Crypto.Types.Big_Numbers;

procedure Mod_Exp is

   A: String :=
     "2988348162058574136915891421498819466320163312926952423791023078876139";
   B: String :=
     "2351399303373464486466122544523690094744975233415544072992656881240319";

   D: constant Positive := Positive'Max(Positive'Max(A'Length, B'Length), 40);
     -- the number of decimals to store A, B, and result
   Bits: constant Positive := (34*D)/10;
     -- (slightly more than) the number of bits to store A, B, and result
   package LN is new Crypto.Types.Big_Numbers (Bits + (32 - Bits mod 32));
     -- the actual number of bits has to be a multiple of 32
   use type LN.Big_Unsigned;

   function "+"(S: String) return LN.Big_Unsigned
     renames LN.Utils.To_Big_Unsigned;

   M: LN.Big_Unsigned := (+"10") ** (+"40");

begin
   Ada.Text_IO.Put("A**B (mod 10**40) = ");
   Ada.Text_IO.Put_Line(LN.Utils.To_String(LN.Mod_Utils.Pow((+A), (+B), M)));
end Mod_Exp;
```


{{out}}

```txt
A**B (mod 10**40) = 1527229998585248450016808958343740453059
```


## ALGOL 68

The code below uses Algol 68 Genie which provides arbitrary precision arithmetic for LONG LONG modes.


```algol68

BEGIN
   PR precision=1000 PR
   MODE LLI = LONG LONG INT;	CO For brevity CO
   PROC mod power = (LLI base, exponent, modulus) LLI :
   BEGIN
      LLI result := 1, b := base, e := exponent;
      IF exponent < 0
      THEN
	 put (stand error, (("Negative exponent", exponent, newline)))
      ELSE
	 WHILE e > 0
	 DO
	    (ODD e | result := (result * b) MOD modulus);
	    e OVERAB 2; b := (b * b) MOD modulus
	 OD
      FI;
      result
   END;
   LLI a = 2988348162058574136915891421498819466320163312926952423791023078876139;
   LLI b = 2351399303373464486466122544523690094744975233415544072992656881240319;
   LLI m = 10000000000000000000000000000000000000000;
   printf (($"Last 40 digits = ", 40dl$, mod power (a, b, m)))
END

```


{{out}}

```txt
Last 40 digits = 1527229998585248450016808958343740453059

```



## AutoHotkey

{{libheader|MPL}}

```autohotkey
#NoEnv
#SingleInstance, Force
SetBatchLines, -1
#Include mpl.ahk

  MP_SET(base, "2988348162058574136915891421498819466320163312926952423791023078876139")
, MP_SET(exponent, "2351399303373464486466122544523690094744975233415544072992656881240319")
, MP_SET(modulus, "10000000000000000000000000000000000000000")

, NumGet(exponent,0,"Int") = -1 ? return : ""
, MP_SET(result, "1")
, MP_SET(TWO, "2")
while !MP_IS0(exponent)
	MP_DIV(q, r, exponent, TWO)
	, (MP_DEC(r) = 1
		? (MP_MUL(temp, result, base)
		, MP_DIV(q, result, temp, modulus))
		: "")
	, MP_DIV(q, r, exponent, TWO)
	, MP_CPY(exponent, q)
	, MP_CPY(base1, base)
	, MP_MUL(base2, base1, base)
	, MP_DIV(q, base, base2, modulus)

msgbox % MP_DEC(result)
Return
```

{{out}}

```txt
1527229998585248450016808958343740453059
```



## Bracmat

{{trans|Icon_and_Unicon}}

```bracmat
  ( ( mod-power
    =   base exponent modulus result
      .   !arg:(?base,?exponent,?modulus)
        & !exponent:~<0
        & 1:?result
        &   whl
          ' ( !exponent:>0
            &     ( (   mod$(!exponent.2):1
                      & mod$(!result*!base.!modulus):?result
                      & -1
                    | 0
                    )
                  + !exponent
                  )
                * 1/2
              : ?exponent
            & mod$(!base^2.!modulus):?base
            )
        & !result
    )
  & ( a
    = 2988348162058574136915891421498819466320163312926952423791023078876139
    )
  & ( b
    = 2351399303373464486466122544523690094744975233415544072992656881240319
    )
  & out$("last 40 digits = " mod-power$(!a,!b,10^40))
  )
```

{{out}}

```txt
last 40 digits =  1527229998585248450016808958343740453059
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
Uses the Huge Integer Math & Encryption library.

```bbcbasic
      INSTALL @lib$+"HIMELIB"
      PROC_himeinit("")

      PROC_hiputdec(1, "2988348162058574136915891421498819466320163312926952423791023078876139")
      PROC_hiputdec(2, "2351399303373464486466122544523690094744975233415544072992656881240319")
      PROC_hiputdec(3, "10000000000000000000000000000000000000000")
      h1% = 1 : h2% = 2 : h3% = 3 : h4% = 4
      SYS `hi_PowMod`, ^h1%, ^h2%, ^h3%, ^h4%
      PRINT FN_higetdec(4)
```

{{out}}

```txt

1527229998585248450016808958343740453059

```



## C

Given numbers are too big for even 64 bit integers, so might as well take the lazy route and use GMP:
{{libheader|GMP}}

```c
#include <gmp.h>

int main()
{
	mpz_t a, b, m, r;

	mpz_init_set_str(a,	"2988348162058574136915891421498819466320"
				"163312926952423791023078876139", 0);
	mpz_init_set_str(b,	"2351399303373464486466122544523690094744"
				"975233415544072992656881240319", 0);
	mpz_init(m);
	mpz_ui_pow_ui(m, 10, 40);

	mpz_init(r);
	mpz_powm(r, a, b, m);

	gmp_printf("%Zd\n", r); /* ...16808958343740453059 */

	mpz_clear(a);
	mpz_clear(b);
	mpz_clear(m);
	mpz_clear(r);

	return 0;
}
```

Output:

```txt

1527229998585248450016808958343740453059

```



## C sharp

We can use the built-in function from BigInteger.

```csharp
using System;
using System.Numerics;

class Program
{
    static void Main() {
        var a = BigInteger.Parse("2988348162058574136915891421498819466320163312926952423791023078876139");
        var b = BigInteger.Parse("2351399303373464486466122544523690094744975233415544072992656881240319");
        var m = BigInteger.Pow(10, 40);
        Console.WriteLine(BigInteger.ModPow(a, b, m));
    }
}
```

{{out}}

```txt

1527229998585248450016808958343740453059

```



## Clojure


```clojure
(defn powerMod "modular exponentiation" [b e m]
  (defn m* [p q] (mod (* p q) m))
  (loop [b b, e e, x 1]
    (if (zero? e) x
      (if (even? e) (recur (m* b b) (/ e 2) x)
        (recur (m* b b) (quot e 2) (m* b x))))))
```



```clojure

(defn modpow
  " b^e mod m (using Java which solves some cases the pure clojure method has to be modified to tackle--i.e. with large b & e and
    calculation simplications when gcd(b, m) == 1 and gcd(e, m) == 1) "
  [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))
```



## Common Lisp


```lisp
(defun rosetta-mod-expt (base power divisor)
  "Return BASE raised to the POWER, modulo DIVISOR.
  This function is faster than (MOD (EXPT BASE POWER) DIVISOR), but
  only works when POWER is a non-negative integer."
  (setq base (mod base divisor))
  ;; Multiply product with base until power is zero.
  (do ((product 1))
      ((zerop power) product)
    ;; Square base, and divide power by 2, until power becomes odd.
    (do () ((oddp power))
      (setq base (mod (* base base) divisor)
	    power (ash power -1)))
    (setq product (mod (* product base) divisor)
	  power (1- power))))

(let ((a 2988348162058574136915891421498819466320163312926952423791023078876139)
      (b 2351399303373464486466122544523690094744975233415544072992656881240319))
  (format t "~A~%" (rosetta-mod-expt a b (expt 10 40))))
```


{{works with|CLISP}}

```lisp
;; CLISP provides EXT:MOD-EXPT
(let ((a 2988348162058574136915891421498819466320163312926952423791023078876139)
      (b 2351399303373464486466122544523690094744975233415544072992656881240319))
  (format t "~A~%" (mod-expt a b (expt 10 40))))
```



### Implementation with LOOP


```lisp
(defun mod-expt (a n m)
   (loop with c = 1 while (plusp n) do
      (if (oddp n) (setf c (mod (* a c) m)))
      (setf n (ash n -1))
      (setf a (mod (* a a) m))
      finally (return c)))
```



## D

{{trans|Icon_and_Unicon}}
Compile this module with <code>-version=modular_exponentiation</code> to see the output.

```d
module modular_exponentiation;

private import std.bigint;

BigInt powMod(BigInt base, BigInt exponent, in BigInt modulus)
pure nothrow /*@safe*/ in {
   assert(exponent >= 0);
} body {
    BigInt result = 1;

    while (exponent) {
        if (exponent & 1)
            result = (result * base) % modulus;
        exponent /= 2;
        base = base ^^ 2 % modulus;
    }

    return result;
}

version (modular_exponentiation)
    void main() {
        import std.stdio;

        powMod(BigInt("29883481620585741369158914214988194" ~
                      "66320163312926952423791023078876139"),
               BigInt("235139930337346448646612254452369009" ~
                      "4744975233415544072992656881240319"),
               BigInt(10) ^^ 40).writeln;
    }
```

{{out}}

```txt
1527229998585248450016808958343740453059
```



## Dc


```Dc
2988348162058574136915891421498819466320163312926952423791023078876139 2351399303373464486466122544523690094744975233415544072992656881240319 10 40^|p
```



## EchoLisp


```scheme

(lib 'bigint)

(define a 2988348162058574136915891421498819466320163312926952423791023078876139)
(define b 2351399303373464486466122544523690094744975233415544072992656881240319)
(define m 1e40)

(powmod a b m)
    → 1527229998585248450016808958343740453059

;; powmod is a native function
;; it could be defined as follows :

(define (xpowmod base exp mod)
    (define result 1)
    (while ( !zero? exp)
        (when (odd? exp) (set! result (% (* result base) mod)))
    (/= exp 2)
    (set! base (% (* base base) mod)))
result)

(xpowmod a b m)
    → 1527229998585248450016808958343740453059

```



## Emacs Lisp

{{libheader|Calc}}

```lisp
(let ((a "2988348162058574136915891421498819466320163312926952423791023078876139")
      (b "2351399303373464486466122544523690094744975233415544072992656881240319"))
  ;; "$ ^ $$ mod (10 ^ 40)" performs modular exponentiation.
  ;; "unpack(-5, x)_1" unpacks the integer from the modulo form.
  (message "%s" (calc-eval "unpack(-5, $ ^ $$ mod (10 ^ 40))_1" nil a b)))
```


=={{header|F_Sharp|F#}}==

```fsharp
let expMod a b n =
    let rec loop a b c =
        if b = 0I then c else
            loop (a*a%n) (b>>>1) (if b&&&1I = 0I then c else c*a%n)
    loop a b 1I

[<EntryPoint>]
let main argv =
    let a = 2988348162058574136915891421498819466320163312926952423791023078876139I
    let b = 2351399303373464486466122544523690094744975233415544072992656881240319I
    printfn "%A" (expMod a b (10I**40))    // -> 1527229998585248450016808958343740453059
    0
```



## Factor


```factor
! Built-in
2988348162058574136915891421498819466320163312926952423791023078876139
2351399303373464486466122544523690094744975233415544072992656881240319
10 40 ^
^mod .
```

{{out}}

```txt

1527229998585248450016808958343740453059

```



## FreeBASIC


```FreeBASIC


'From first principles (No external library)
Function _divide(n1 As String,n2 As String,decimal_places As Integer=10,dpflag As String="s") As String
    Dim As String number=n1,divisor=n2
    dpflag=Lcase(dpflag)
    'For MOD
    Dim As Integer modstop
    If dpflag="mod" Then
        If Len(n1)<Len(n2) Then Return n1
        If Len(n1)=Len(n2) Then
            If n1<n2 Then Return n1
        End If
        modstop=Len(n1)-Len(n2)+1
    End If
    If dpflag<>"mod" Then
        If dpflag<>"s"  Then dpflag="raw"
    End If
    Dim runcount As Integer
    '_______  LOOK UP TABLES ______________
    Dim Qmod(0 To 19) As Ubyte
    Dim bool(0 To 19) As Ubyte
    For z As Integer=0 To 19
        Qmod(z)=(z Mod 10+48)
        bool(z)=(-(10>z))
    Next z
    Dim answer As String   'THE ANSWER STRING

    '_______ SET THE DECIMAL WHERE IT SHOULD BE AT _______
    Dim As String part1,part2
    #macro set(decimal)
    #macro insert(s,char,position)
    If position > 0 And position <=Len(s) Then
        part1=Mid(s,1,position-1)
        part2=Mid(s,position)
        s=part1+char+part2
    End If
    #endmacro
    insert(answer,".",decpos)
    answer=thepoint+zeros+answer
    If dpflag="raw" Then
        answer=Mid(answer,1,decimal_places)
    End If
    #endmacro
    '______________________________________________
    '__________ SPLIT A STRING ABOUT A CHARACTRR __________
    Dim As String var1,var2
    Dim pst As Integer
    #macro split(stri,char,var1,var2)
    pst=Instr(stri,char)
    var1="":var2=""
    If pst<>0 Then
        var1=Rtrim(Mid(stri,1,pst),".")
        var2=Ltrim(Mid(stri,pst),".")
    Else
        var1=stri
    End If
    #endmacro

    #macro Removepoint(s)
    split(s,".",var1,var2)
    #endmacro
    '__________ GET THE SIGN AND CLEAR THE -ve __________________
    Dim sign As String
    If Left(number,1)="-" Xor Left (divisor,1)="-" Then sign="-"
    If Left(number,1)="-" Then  number=Ltrim(number,"-")
    If Left (divisor,1)="-" Then divisor=Ltrim(divisor,"-")

    'DETERMINE THE DECIMAL POSITION BEFORE THE DIVISION
    Dim As Integer lennint,lenddec,lend,lenn,difflen
    split(number,".",var1,var2)
    lennint=Len(var1)
    split(divisor,".",var1,var2)
    lenddec=Len(var2)

    If Instr(number,".") Then
        Removepoint(number)
        number=var1+var2
    End If
    If Instr(divisor,".") Then
        Removepoint(divisor)
        divisor=var1+var2
    End If
    Dim As Integer numzeros
    numzeros=Len(number)
    number=Ltrim(number,"0"):divisor=Ltrim (divisor,"0")
    numzeros=numzeros-Len(number)
    lend=Len(divisor):lenn=Len(number)
    If lend>lenn Then difflen=lend-lenn
    Dim decpos As Integer=lenddec+lennint-lend+2-numzeros 'THE POSITION INDICATOR
    Dim _sgn As Byte=-Sgn(decpos)
    If _sgn=0 Then _sgn=1
    Dim As String thepoint=String(_sgn,".") 'DECIMAL AT START (IF)
    Dim As String zeros=String(-decpos+1,"0")'ZEROS AT START (IF) e.g. .0009
    If dpflag<>"mod" Then
        If Len(zeros) =0 Then dpflag="s"
    End If
    Dim As Integer runlength
    If Len(zeros) Then
        runlength=decimal_places
        answer=String(Len(zeros)+runlength+10,"0")
        If dpflag="raw" Then
            runlength=1
            answer=String(Len(zeros)+runlength+10,"0")
            If decimal_places>Len(zeros) Then
                runlength=runlength+(decimal_places-Len(zeros))
                answer=String(Len(zeros)+runlength+10,"0")
            End If
        End If

    Else
        decimal_places=decimal_places+decpos
        runlength=decimal_places
        answer=String(Len(zeros)+runlength+10,"0")
    End If
    '___________DECIMAL POSITION DETERMINED  _____________

    'SET UP THE VARIABLES AND START UP CONDITIONS
    number=number+String(difflen+decimal_places,"0")
    Dim count As Integer
    Dim temp As String
    Dim copytemp As String
    Dim topstring As String
    Dim copytopstring As String
    Dim As Integer lenf,lens
    Dim As Ubyte takeaway,subtractcarry
    Dim As Integer n3,diff
    If Ltrim(divisor,"0")="" Then Return "Error :division by zero"
    lens=Len(divisor)
    topstring=Left(number,lend)
    copytopstring=topstring
    Do
        count=0
        Do
            count=count+1
            copytemp=temp

            Do
                '___________________ QUICK SUBTRACTION loop _________________

                lenf=Len(topstring)
                If  lens<lenf=0 Then 'not
                    If Lens>lenf Then
                        temp= "done"
                        Exit Do
                    End If
                    If divisor>topstring Then
                        temp= "done"
                        Exit Do
                    End If
                End If

                diff=lenf-lens
                temp=topstring
                subtractcarry=0

                For n3=lenf-1 To diff Step -1
                    takeaway= topstring[n3]-divisor[n3-diff]+10-subtractcarry
                    temp[n3]=Qmod(takeaway)
                    subtractcarry=bool(takeaway)
                Next n3
                If subtractcarry=0 Then Exit Do
                If n3=-1 Then Exit Do
                For n3=n3 To 0 Step -1
                    takeaway= topstring[n3]-38-subtractcarry
                    temp[n3]=Qmod(takeaway)
                    subtractcarry=bool(takeaway)
                    If subtractcarry=0 Then Exit Do
                Next n3
                Exit Do

            Loop 'single run
            temp=Ltrim(temp,"0")
            If temp="" Then temp= "0"
            topstring=temp
        Loop Until temp="done"
        ' INDIVIDUAL CHARACTERS CARVED OFF ________________
        runcount=runcount+1
        If count=1 Then
            topstring=copytopstring+Mid(number,lend+runcount,1)
        Else
            topstring=copytemp+Mid(number,lend+runcount,1)
        End If
        copytopstring=topstring
        topstring=Ltrim(topstring,"0")
        If dpflag="mod" Then
            If runcount=modstop Then
                If topstring="" Then Return "0"
                Return Mid(topstring,1,Len(topstring)-1)
            End If
        End If
        answer[runcount-1]=count+47
        If topstring="" And runcount>Len(n1)+1 Then
            Exit Do
        End If
    Loop Until runcount=runlength+1

    ' END OF RUN TO REQUIRED DECIMAL PLACES
    set(decimal) 'PUT IN THE DECIMAL POINT
    'THERE IS ALWAYS A DECIMAL POINT SOMEWHERE IN THE ANSWER
    'NOW GET RID OF IT IF IT IS REDUNDANT
    answer=Rtrim(answer,"0")
    answer=Rtrim(answer,".")
    answer=Ltrim(answer,"0")
    If answer="" Then Return "0"
    Return sign+answer
End Function

Dim Shared As Integer _Mod(0 To 99),_Div(0 To 99)
For z As Integer=0 To 99:_Mod(z)=(z Mod 10+48):_Div(z)=z\10:Next

    Function Qmult(a As String,b As String) As String
        Var flag=0,la = Len(a),lb = Len(b)
        If Len(b)>Len(a) Then flag=1:Swap a,b:Swap la,lb
        Dim As Ubyte n,carry,ai
        Var c =String(la+lb,"0")
        For i As Integer =la-1 To 0 Step -1
            carry=0:ai=a[i]-48
            For j As Integer =lb-1 To 0 Step -1
                n = ai * (b[j]-48) + (c[i+j+1]-48) + carry
                carry =_Div(n):c[i+j+1]=_Mod(n)
            Next j
            c[i]+=carry
        Next i
        If flag Then Swap a,b
        Return Ltrim(c,"0")
    End Function
    '
### =================================================================

    #define mod_(a,b) _divide((a),(b),,"mod")
    #define div_(a,b) iif(len((a))<len((b)),"0",_divide((a),(b),-2))

    Function Modular_Exponentiation(base_num As String, exponent As String, modulus As String) As String
        Dim b1 As String = base_num
        Dim e1 As String = exponent
        Dim As String result="1"
        b1 = mod_(b1,modulus)
        Do While e1 <> "0"
            Var L=Len(e1)-1
            If e1[L] And 1 Then
                result=mod_(Qmult(result,b1),modulus)
            End If
            b1=mod_(qmult(b1,b1),modulus)
            e1=div_(e1,"2")
        Loop
        Return result
    End Function




    var base_num="2988348162058574136915891421498819466320163312926952423791023078876139"
    var exponent="2351399303373464486466122544523690094744975233415544072992656881240319"
    var modulus="10000000000000000000000000000000000000000"
    dim as double t=timer
    var ans=Modular_Exponentiation(base_num,exponent,modulus)
    print "Result:"
    Print ans
    print "time taken  ";(timer-t)*1000;" milliseconds"
    Print "Done"
    Sleep




```


```txt

Result:
1527229998585248450016808958343740453059
time taken   93.09767815284431 milliseconds
Done


```






## GAP


```gap
# Built-in
a := 2988348162058574136915891421498819466320163312926952423791023078876139;
b := 2351399303373464486466122544523690094744975233415544072992656881240319;
PowerModInt(a, b, 10^40);
1527229998585248450016808958343740453059

# Implementation
PowerModAlt := function(a, n, m)
    local r;
    r := 1;
    while n > 0 do
        if IsOddInt(n) then
            r := RemInt(r*a, m);
        fi;
        n := QuoInt(n, 2);
        a := RemInt(a*a, m);
    od;
    return r;
end;

PowerModAlt(a, b, 10^40);
```



## gnuplot



```gnuplot
_powm(b, e, m, r) = (e == 0 ? r : (e % 2 == 1 ? _powm(b * b % m, e / 2, m, r * b % m) : _powm(b * b % m, e / 2, m, r)))
powm(b, e, m) = _powm(b, e, m, 1)
# Usage
print powm(2, 3453, 131)
# Where b is the base, e is the exponent, m is the modulus, i.e.: b^e mod m
```



## Go


```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    a, _ := new(big.Int).SetString(
        "2988348162058574136915891421498819466320163312926952423791023078876139", 10)
    b, _ := new(big.Int).SetString(
        "2351399303373464486466122544523690094744975233415544072992656881240319", 10)
    m := big.NewInt(10)
    r := big.NewInt(40)
    m.Exp(m, r, nil)

    r.Exp(a, b, m)
    fmt.Println(r)
}
```

{{out}}

```txt

1527229998585248450016808958343740453059

```



## Groovy


```groovy
println 2988348162058574136915891421498819466320163312926952423791023078876139.modPow(
            2351399303373464486466122544523690094744975233415544072992656881240319,
            10000000000000000000000000000000000000000)
```

Ouput:

```txt
1527229998585248450016808958343740453059
```



## Haskell


```haskell
powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

main :: IO ()
main =
  print $
  powm
    2988348162058574136915891421498819466320163312926952423791023078876139
    2351399303373464486466122544523690094744975233415544072992656881240319
    (10 ^ 40)
    1
```

{{out}}

```txt
1527229998585248450016808958343740453059
```


=={{header|Icon}} and {{header|Unicon}}==
This uses the exponentiation procedure from [[RSA_code#Icon_and_Unicon|RSA Code]] an example of the right to left binary method.

```Icon
procedure main()
    a := 2988348162058574136915891421498819466320163312926952423791023078876139
    b := 2351399303373464486466122544523690094744975233415544072992656881240319
    write("last 40 digits = ",mod_power(a,b,(10^40))
end

procedure mod_power(base, exponent, modulus)   # fast modular exponentation
   if exponent < 0 then runerr(205,m)          # added for this task
   result := 1
   while exponent > 0 do {
      if exponent % 2 = 1 then
         result := (result * base) % modulus
      exponent /:= 2
      base := base ^ 2 % modulus
      }
   return result
end
```


{{out}}

```txt
last 40 digits = 1527229998585248450016808958343740453059
```



## J

'''Solution''':
```j
   m&|@^
```

'''Example''':
```j
   a =: 2988348162058574136915891421498819466320163312926952423791023078876139x
   b =: 2351399303373464486466122544523690094744975233415544072992656881240319x
   m =: 10^40x

   a m&|@^ b
1527229998585248450016808958343740453059
```

'''Discussion''': The phrase <tt>a m&|@^ b</tt> is the natural expression of <tt>a^b mod m</tt> in J, and is recognized by the interpreter as an opportunity for optimization, by [http://www.jsoftware.com/help/dictionary/special.htm#recognized%20phrase avoiding the exponentiation].


## Java

<code>java.math.BigInteger.modPow</code> solves this task. Inside [[OpenJDK]], [http://hg.openjdk.java.net/jdk7/jdk7/jdk/file/f097ca2434b1/src/share/classes/java/math/BigInteger.java BigInteger.java] implements <code>BigInteger.modPow</code> with a fast algorithm from [http://philzimmermann.com/EN/bnlib/index.html Colin Plumb's bnlib]. This "window algorithm" caches odd powers of the base, to decrease the number of squares and multiplications. It also exploits both the Chinese remainder theorem and the [[Montgomery reduction]].


```java
import java.math.BigInteger;

public class PowMod {
    public static void main(String[] args){
        BigInteger a = new BigInteger(
      "2988348162058574136915891421498819466320163312926952423791023078876139");
        BigInteger b = new BigInteger(
      "2351399303373464486466122544523690094744975233415544072992656881240319");
        BigInteger m = new BigInteger("10000000000000000000000000000000000000000");

        System.out.println(a.modPow(b, m));
    }
}
```

{{out}}

```txt
1527229998585248450016808958343740453059
```



## Julia

{{works with|Julia|1.0}}
We can use the built-in <code>powermod</code> function with the built-in <code>BigInt</code> type (based on GMP):


```julia
a = 2988348162058574136915891421498819466320163312926952423791023078876139
b = 2351399303373464486466122544523690094744975233415544072992656881240319
m = big(10) ^ 40
@show powermod(a, b, m)
```


{{out}}

```txt
powermod(a, b, m) = 1527229998585248450016808958343740453059
```



## Kotlin


```scala
// version 1.0.6

import java.math.BigInteger

fun main(args: Array<String>) {
    val a = BigInteger("2988348162058574136915891421498819466320163312926952423791023078876139")
    val b = BigInteger("2351399303373464486466122544523690094744975233415544072992656881240319")
    val m = BigInteger.TEN.pow(40)
    println(a.modPow(b, m))
}
```


{{out}}

```txt

1527229998585248450016808958343740453059

```



## Maple


```Maple
a := 2988348162058574136915891421498819466320163312926952423791023078876139:
b := 2351399303373464486466122544523690094744975233415544072992656881240319:
a &^ b mod 10^40;
```

{{out}}

```txt
1527229998585248450016808958343740453059
```



## Mathematica


```Mathematica
a = 2988348162058574136915891421498819466320163312926952423791023078876139;
b = 2351399303373464486466122544523690094744975233415544072992656881240319;
m = 10^40;
PowerMod[a, b, m]
-> 1527229998585248450016808958343740453059
```



## Maxima


```maxima
a: 2988348162058574136915891421498819466320163312926952423791023078876139$
b: 2351399303373464486466122544523690094744975233415544072992656881240319$
power_mod(a, b, 10^40);
/* 1527229998585248450016808958343740453059 */
```



## Nim

{{libheader|bigints}}

```nim
import bigints

proc powmod(b, e, m: BigInt): BigInt =
  assert e >= 0
  var e = e
  var b = b
  result = initBigInt(1)
  while e > 0:
    if e mod 2 == 1:
      result = (result * b) mod m
    e = e div 2
    b = (b.pow 2) mod m

var
  a = initBigInt("2988348162058574136915891421498819466320163312926952423791023078876139")
  b = initBigInt("2351399303373464486466122544523690094744975233415544072992656881240319")

echo powmod(a, b, 10.pow 40)
```

{{out}}

```txt
1527229998585248450016808958343740453059
```



## Oforth


Usage : a b mod powmod


```Oforth
: powmod(base, exponent, modulus)
   1 exponent dup ifZero: [ return ]
    while ( dup 0 > ) [
      dup isEven ifFalse: [ swap base * modulus mod swap ]
      2 / base sq modulus mod ->base
      ] drop ;
```


{{out}}

```txt

>2988348162058574136915891421498819466320163312926952423791023078876139
ok
>2351399303373464486466122544523690094744975233415544072992656881240319
ok
>10 40 pow
ok
>powmod println
1527229998585248450016808958343740453059
ok

```



## ooRexx


```rexx
/* Modular exponentiation */

numeric digits 100
say powerMod(,
 2988348162058574136915891421498819466320163312926952423791023078876139,,
 2351399303373464486466122544523690094744975233415544072992656881240319,,
 1e40)
exit

powerMod: procedure

use strict arg base, exponent, modulus

exponent=exponent~d2x~x2b~strip('L','0')
result=1
base = base // modulus
do exponentPos=exponent~length to 1 by -1
  if (exponent~subChar(exponentPos) == '1')
    then result = (result * base) // modulus
  base = (base * base) // modulus
end
return result
```

{{out}}

```txt

1527229998585248450016808958343740453059

```



## PARI/GP


```parigp
a=2988348162058574136915891421498819466320163312926952423791023078876139;
b=2351399303373464486466122544523690094744975233415544072992656881240319;
lift(Mod(a,10^40)^b)
```



## Pascal

{{works with|Free_Pascal}}
{{libheader|GMP}}
A port of the C example using gmp.

```pascal
Program ModularExponentiation(output);

uses
  gmp;

var
  a, b, m, r: mpz_t;
  fmt: pchar;

begin
  mpz_init_set_str(a, '2988348162058574136915891421498819466320163312926952423791023078876139', 10);
  mpz_init_set_str(b, '2351399303373464486466122544523690094744975233415544072992656881240319', 10);
  mpz_init(m);
  mpz_ui_pow_ui(m, 10, 40);

  mpz_init(r);
  mpz_powm(r, a, b, m);

  fmt := '%Zd' + chr(13) + chr(10);
  mp_printf(fmt, @r); (* ...16808958343740453059 *)

  mpz_clear(a);
  mpz_clear(b);
  mpz_clear(m);
  mpz_clear(r);
end.
```

{{out}}

```txt
% ./ModularExponentiation
1527229998585248450016808958343740453059

```



## Perl

Calculating the result both with an explicit algorithm (borrowed from Perl 6 example) and with a built-in available when the <code>use bigint</code> pragma is invoked.  Note that <code>bmodpow</code> modifies the base value (here <code>$a</code>) while <code>expmod</code> does not.

```perl
use bigint;

sub expmod {
    my($a, $b, $n) = @_;
    my $c = 1;
    do {
        ($c *= $a) %= $n if $b % 2;
        ($a *= $a) %= $n;
    } while ($b = int $b/2);
    $c;
}

my $a = 2988348162058574136915891421498819466320163312926952423791023078876139;
my $b = 2351399303373464486466122544523690094744975233415544072992656881240319;
my $m = 10 ** 40;

print expmod($a, $b, $m), "\n";
print $a->bmodpow($b, $m), "\n";
```

{{out}}

```txt
1527229998585248450016808958343740453059
1527229998585248450016808958343740453059
```



## Perl 6

This is specced as a built-in, but here's an explicit version:

```perl6
sub expmod(Int $a is copy, Int $b is copy, $n) {
    my $c = 1;
    repeat while $b div= 2 {
        ($c *= $a) %= $n if $b % 2;
        ($a *= $a) %= $n;
    }
    $c;
}

say expmod
    2988348162058574136915891421498819466320163312926952423791023078876139,
    2351399303373464486466122544523690094744975233415544072992656881240319,
    10**40;
```

{{out}}

```txt
1527229998585248450016808958343740453059
```



## Phix

{{libheader|mpfr}}
Already builtin as mpz_powm, but here is an explicit version

```Phix
include mpfr.e
procedure mpz_mod_exp(mpz base, exponent, modulus, result)
    if mpz_cmp_si(exponent,1)=0 then
        mpz_set(result,base)
    else
        mpz _exp = mpz_init_set(exponent) -- (use a copy)
        bool odd = mpz_odd(_exp)
        if odd then
            mpz_sub_ui(_exp,_exp,1)
        end if
        mpz_fdiv_q_2exp(_exp,_exp,1)
        mpz_mod_exp(base,_exp,modulus,result)
        _exp = mpz_free(_exp)
        mpz_mul(result,result,result)
        if odd then
            mpz_mul(result,result,base)
        end if
    end if
    mpz_mod(result,result,modulus)
end procedure

mpz base     = mpz_init("2988348162058574136915891421498819466320163312926952423791023078876139"),
    exponent = mpz_init("2351399303373464486466122544523690094744975233415544072992656881240319"),
    modulus  = mpz_init("1"&repeat('0',40)),
    result   = mpz_init()

mpz_mod_exp(base,exponent,modulus,result)
?mpz_get_str(result)

-- check against the builtin:
mpz_powm(result,base,exponent,modulus)
?mpz_get_str(result)
```

{{out}}

```txt

"1527229998585248450016808958343740453059"
"1527229998585248450016808958343740453059"

```



## PHP


```php
<?php
$a = '2988348162058574136915891421498819466320163312926952423791023078876139';
$b = '2351399303373464486466122544523690094744975233415544072992656881240319';
$m = '1' . str_repeat('0', 40);
echo bcpowmod($a, $b, $m), "\n";
```

{{out}}

```txt
1527229998585248450016808958343740453059
```



## PicoLisp

The following function is taken from "lib/rsa.l":

```PicoLisp
(de **Mod (X Y N)
   (let M 1
      (loop
         (when (bit? 1 Y)
            (setq M (% (* M X) N)) )
         (T (=0 (setq Y (>> 1 Y)))
            M )
         (setq X (% (* X X) N)) ) ) )
```

Test:

```PicoLisp
: (**Mod
   2988348162058574136915891421498819466320163312926952423791023078876139
   2351399303373464486466122544523690094744975233415544072992656881240319
   10000000000000000000000000000000000000000 )
-> 1527229998585248450016808958343740453059
```



## Python


```python
a = 2988348162058574136915891421498819466320163312926952423791023078876139
b = 2351399303373464486466122544523690094744975233415544072992656881240319
m = 10 ** 40
print(pow(a, b, m))
```

{{out}}

```txt
1527229998585248450016808958343740453059
```



## OCaml


Using the zarith library:


```ocaml

let a = Z.of_string "2988348162058574136915891421498819466320163312926952423791023078876139" in
let b = Z.of_string "2351399303373464486466122544523690094744975233415544072992656881240319" in
let m = Z.pow (Z.of_int 10) 40 in
Z.powm a b m
|> Z.to_string
|> print_endline
```

{{out}}

```txt
1527229998585248450016808958343740453059
```



## Racket


```racket

#lang racket
(require math)
(define a 2988348162058574136915891421498819466320163312926952423791023078876139)
(define b 2351399303373464486466122544523690094744975233415544072992656881240319)
(define m (expt 10 40))
(modular-expt a b m)

```

{{out}}

```txt

1527229998585248450016808958343740453059

```



## REXX


### version 1

This REXX program attempts to handle   ''any''   '''a''', '''b''', or '''m''',   but there are limits for any computer language.

For some REXXes, it's around eight million digits for any arithmetic expression or value, which puts limitations on the

values of   <big>a<sup>2</sup></big>   or   <big>10<sup>m</sup></big>.

There is REXX code (below) to automatically adjust the number of digits to accommodate huge numbers which are

computed when raising large numbers to some arbitrary power.

```rexx
/*REXX program  displays  modular exponentiation of:             a**b  mod  M           */
parse arg a b mm                                      /*obtain optional args from the CL*/
if a=='' | a==","  then a=2988348162058574136915891421498819466320163312926952423791023078876139
if b=='' | b==","  then b=2351399303373464486466122544523690094744975233415544072992656881240319
if mm='' | mm=","  then mm=40                         /*MM not specified?   Use default.*/
say 'a=' a;   say "        ("length(a) 'digits)'      /*display the  value of  A.       */
say 'b=' b;   say "        ("length(b) 'digits)'      /*   "     "     "    "  B.       */

     do j=1  for words(mm);   m=word(mm,j)            /*use one of the MM powers (list).*/
     say copies('─', linesize()-1)                    /*show a nice separator fence line*/
     say 'a**b (mod 10**'m")="   powerMod(a,b,10**m)  /*display the answer ───► console.*/
     end   /*j*/
exit                                                  /*stick a fork in it, we're done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
powerMod: procedure;  parse arg x,p,n                 /*fast modular exponentiation code*/
          if p==0  then return 1                      /*special case of  P  being zero. */
          if p==1  then return x                      /*   "      "   "  "    "   unity.*/
          if p<0   then do;   say '***error*** power is negative:'  p;    exit 13;     end
          parse value max(x**2,p,n)'E0'  with  "E" e  /*obtain the biggest of the three.*/
          numeric digits max(20, e*2)                 /*big enough to handle  A².       */
          $=1                                         /*use this for the first value.   */
                     do  while p\==0                  /*perform  while   P   isn't zero.*/
                     if p//2  then $=$ * x  // n      /*is  P  odd?  (is ÷ remainder≡1).*/
                     p=p%2;        x=x * x  // n      /*halve  P;   calculate  x² mod n */
                     end   /*while*/                  /* [↑]  keep mod'ing 'til equal 0.*/
          return $
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console).

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]].


'''output'''   when using the input of:   <tt>   ,   ,   40   80   180   888 </tt>

Note the REXX program was executing within a window of 600 characters wide, and even with that, the last result wrapped.

```txt

a= 2988348162058574136915891421498819466320163312926952423791023078876139
        (70 digits)
b= 2351399303373464486466122544523690094744975233415544072992656881240319
        (70 digits)
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
a**b (mod 10**40)= 1527229998585248450016808958343740453059
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
a**b (mod 10**80)= 53259517041910225328867076245698908287781527229998585248450016808958343740453059
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
a**b (mod 10**180)= 31857295076204937005344367438778481743660325586328069392203762862423884839076695547212682454523811053259517041910225328867076245698908287781527229998585248450016808958343740453059
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
a**b (mod 10**888)= 2612849643808365153970307063634422265713972370574889513136845452410856423299436762487557161242604471887885300171829510516527484255607339748359444160694661767131561827274483018385170003434853270016569482853811730383390737793312301323406698998964489388587853627711904603124125798753498716559994462054260496622614506334484689315735068762556447491553489235236807309998697854727791160093566968169527719659307289405305177993299425901141782840092602984267350865792542825912897568403588118221513074793528568569833937153488707152390200379629380198479929609788498528506130631774711751914442
62586321233906926671000476591123695550566585083205841790404069511972417770392822283604206143472509425391114072344402850867571806031857295076204937005344367438778481743660325586328069392203762862423884839076695547212682454523811053259517041910225328867076245698908287781527229998585248450016808958343740453059

```



### version 2

This REXX version handles only up to 100 decimal digits.

```rexx
/* Modular exponentiation */

numeric digits 100
say powerMod(,
 2988348162058574136915891421498819466320163312926952423791023078876139,,
 2351399303373464486466122544523690094744975233415544072992656881240319,,
 1e40)
exit

powerMod: procedure

parse arg base, exponent, modulus

exponent = strip(x2b(d2x(exponent)), 'L', '0')
result = 1
base = base // modulus
do exponentPos=length(exponent) to 1 by -1
  if substr(exponent, exponentPos, 1) = '1'
    then result = (result * base) // modulus
  base = (base * base) // modulus
end
return result
```

{{out}}

```txt

1527229998585248450016808958343740453059

```



## Ruby


### Built in since version 2.5.


```ruby
a = 2988348162058574136915891421498819466320163312926952423791023078876139
b = 2351399303373464486466122544523690094744975233415544072992656881240319
m = 10**40
puts a.pow(b, m)
```


### Using OpenSSL standard library

{{libheader|OpenSSL}}

```ruby
require 'openssl'
a = 2988348162058574136915891421498819466320163312926952423791023078876139
b = 2351399303373464486466122544523690094744975233415544072992656881240319
m = 10 ** 40
puts a.to_bn.mod_exp(b, m)
```


### Written in Ruby


```ruby

def mod_exp(n, e, mod)
    fail ArgumentError, 'negative exponent' if e < 0
    prod = 1
    base = n % mod
    until e.zero?
      prod = (prod * base) % mod if e.odd?
      e >>= 1
      base = (base * base) % mod
    end
    prod
  end

```



## Rust


```rust
/* Add this line to the [dependencies] section of your Cargo.toml file:
num = "0.2.0"
*/


use num::bigint::BigInt;
use num::bigint::ToBigInt;


// The modular_exponentiation() function takes three identical types
// (which get cast to BigInt), and returns a BigInt:
fn modular_exponentiation<T: ToBigInt>(n: &T, e: &T, m: &T) -> BigInt {
    // Convert n, e, and m to BigInt:
    let n = n.to_bigint().unwrap();
    let e = e.to_bigint().unwrap();
    let m = m.to_bigint().unwrap();

    // Sanity check:  Verify that the exponent is not negative:
    assert!(e >= Zero::zero());

    use num::traits::{Zero, One};

    // As most modular exponentiations do, return 1 if the exponent is 0:
    if e == Zero::zero() {
        return One::one()
    }

    // Now do the modular exponentiation algorithm:
    let mut result: BigInt = One::one();
    let mut base = n % &m;
    let mut exp = e;

    // Loop until we can return out result:
    loop {
        if &exp % 2 == One::one() {
            result *= &base;
            result %= &m;
        }

        if exp == One::one() {
            return result
        }

        exp /= 2;
        base *= base.clone();
        base %= &m;
    }
}
```


'''Test code:'''


```rust
fn main() {
    let (a, b, num_digits) = (
  "2988348162058574136915891421498819466320163312926952423791023078876139",
  "2351399303373464486466122544523690094744975233415544072992656881240319",
  "40",
                    );

    // Covert a, b, and num_digits to numbers:
    let a = BigInt::parse_bytes(a.as_bytes(), 10).unwrap();
    let b = BigInt::parse_bytes(b.as_bytes(), 10).unwrap();
    let num_digits = num_digits.parse().unwrap();

    // Calculate m from num_digits:
    let m = num::pow::pow(10.to_bigint().unwrap(), num_digits);

    // Get the result and print it:
    let result = modular_exponentiation(&a, &b, &m);
    println!("The last {} digits of\n{}\nto the power of\n{}\nare:\n{}",
             num_digits, a, b, result);
}
```

{{out}}

```txt
The last 40 digits of
2988348162058574136915891421498819466320163312926952423791023078876139
to the power of
2351399303373464486466122544523690094744975233415544072992656881240319
are:
1527229998585248450016808958343740453059
```



## Scala


```scala
import scala.math.BigInt

val a = BigInt(
  "2988348162058574136915891421498819466320163312926952423791023078876139")
val b = BigInt(
  "2351399303373464486466122544523690094744975233415544072992656881240319")

println(a.modPow(b, BigInt(10).pow(40)))
```



## Scheme



```scheme

(define (square n)
  (* n n))

(define (mod-exp a n mod)
  (cond ((= n 0) 1)
        ((even? n)
         (remainder (square (mod-exp a (/ n 2) mod))
                    mod))
        (else (remainder (* a (mod-exp a (- n 1) mod))
                         mod))))

(define result
  (mod-exp 2988348162058574136915891421498819466320163312926952423791023078876139
           2351399303373464486466122544523690094744975233415544072992656881240319
           (expt 10 40)))
```


{{out}}

```txt

> result
1527229998585248450016808958343740453059

```



## Seed7

The library [http://seed7.sourceforge.net/libraries/bigint.htm bigint.s7i] defines the function
[http://seed7.sourceforge.net/libraries/bigint.htm#modPow%28in_var_bigInteger,in_var_bigInteger,in_bigInteger%29 modPow],
which does modular exponentiation.

```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const proc: main is func
  begin
    writeln(modPow(2988348162058574136915891421498819466320163312926952423791023078876139_,
                   2351399303373464486466122544523690094744975233415544072992656881240319_,
                   10_ ** 40));
  end func;
```


{{out}}

```txt

1527229998585248450016808958343740453059

```


The library bigint.s7i defines modPow with:

```seed7
const func bigInteger: modPow (in var bigInteger: base,
    in var bigInteger: exponent, in bigInteger: modulus) is func
  result
    var bigInteger: power is 1_;
  begin
    if exponent < 0_ or modulus < 0_ then
      raise RANGE_ERROR;
    else
      while exponent > 0_ do
        if odd(exponent) then
          power := (power * base) mod modulus;
        end if;
        exponent >>:= 1;
        base := base ** 2 mod modulus;
      end while;
    end if;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#modPow]


## Sidef

Built-in:

```ruby
say expmod(
    2988348162058574136915891421498819466320163312926952423791023078876139,
    2351399303373464486466122544523690094744975233415544072992656881240319,
    10**40)
```


User-defined:

```ruby
func expmod(a, b, n) {
    var c = 1
    do {
        (c *= a) %= n if b.is_odd
        (a *= a) %= n
    } while (b //= 2)
    c
}
```

{{out}}

```txt

1527229998585248450016808958343740453059

```



## Tcl

While Tcl does have arbitrary-precision arithmetic (from 8.5 onwards),
it doesn't expose a modular exponentiation function.
Thus we implement one ourselves.


### Recursive


```tcl
package require Tcl 8.5

# Algorithm from http://introcs.cs.princeton.edu/java/78crypto/ModExp.java.html
# but Tcl has arbitrary-width integers and an exponentiation operator, which
# helps simplify the code.
proc tcl::mathfunc::modexp {a b n} {
    if {$b == 0} {return 1}
    set c [expr {modexp($a, $b / 2, $n)**2 % $n}]
    if {$b & 1} {
	set c [expr {($c * $a) % $n}]
    }
    return $c
}
```

Demonstrating:

```tcl
set a 2988348162058574136915891421498819466320163312926952423791023078876139
set b 2351399303373464486466122544523690094744975233415544072992656881240319
set n [expr {10**40}]
puts [expr {modexp($a,$b,$n)}]
```

{{out}}

```txt

 1527229998585248450016808958343740453059

```



### Iterative


```tcl
package require Tcl 8.5
proc modexp {a b n} {
    for {set c 1} {$b} {set a [expr {$a*$a % $n}]} {
	if {$b & 1} {
	    set c [expr {$c*$a % $n}]
	}
	set b [expr {$b >> 1}]
    }
    return $c
}
```

Demonstrating:

```tcl
set a 2988348162058574136915891421498819466320163312926952423791023078876139
set b 2351399303373464486466122544523690094744975233415544072992656881240319
set n [expr {10**40}]
puts [modexp $a $b $n]
```

{{out}}

```txt

 1527229998585248450016808958343740453059

```



## TXR


From your system prompt:


```sh
$ txr -p '(exptmod 2988348162058574136915891421498819466320163312926952423791023078876139
                   2351399303373464486466122544523690094744975233415544072992656881240319
                   (expt 10 40)))'
1527229998585248450016808958343740453059
```



## Visual Basic .NET

{{works with|Visual Basic .NET|2011}}

```vbnet
' Modular exponentiation - VB.Net - 21/01/2019
    Imports System.Numerics

    Private Sub Main()
        Dim a, b, m, x As BigInteger
        a = BigInteger.Parse("2988348162058574136915891421498819466320163312926952423791023078876139")
        b = BigInteger.Parse("2351399303373464486466122544523690094744975233415544072992656881240319")
        m = BigInteger.Pow(10, 40)   '=10^40
        x = ModPowBig(a, b, m)
        Debug.Print("x=" & x.ToString)
    End Sub 'Main

    Function ModPowBig(ByVal base As BigInteger, ByVal exponent As BigInteger, ByVal modulus As BigInteger) As BigInteger
        Dim result As BigInteger
        result = 1
        Do While exponent > 0
            If (exponent Mod 2) = 1 Then
                result = (result * base) Mod modulus
            End If
            exponent = exponent / 2
            base = (base * base) Mod modulus
        Loop
        Return result
    End Function 'ModPowBig
```

{{out}}

```txt

x=1527229998585248450016808958343740453059

```



## zkl

Using the GMP big num library:

```zkl
var BN=Import("zklBigNum");
a:=BN("2988348162058574136915891421498819466320163312926952423791023078876139");
b:=BN("2351399303373464486466122544523690094744975233415544072992656881240319");
m:=BN(10).pow(40);
a.powm(b,m).println();
a.powm(b,m) : "%,d".fmt(_).println();
```

{{out}}

```txt

1527229998585248450016808958343740453059
1,527,229,998,585,248,450,016,808,958,343,740,453,059

```

