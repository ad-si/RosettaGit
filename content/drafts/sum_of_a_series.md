+++
title = "Sum of a series"
description = ""
date = 2019-10-13T00:27:16Z
aliases = []
[extra]
id = 2621
[taxonomies]
categories = []
tags = []
+++

{{task|Arithmetic operations}}
Compute the   '''n'''<sup>th</sup>   term of a [[wp:Series (mathematics)|series]],   i.e. the sum of the   '''n'''   first terms  of the corresponding [[wp:sequence|sequence]].

Informally this value, or its limit when   '''n'''   tends to infinity, is also called the ''sum of the series'', thus the title of this task.

For this task, use:
:::::: <big><math>S_n = \sum_{k=1}^n \frac{1}{k^2}</math></big>



:: and compute   <big><math>S_{1000}</math></big>


This approximates the    [[wp:Riemann zeta function|zeta function]]   for   <big>S=2</big>,   whose exact value

:::::: <big><math>\zeta(2) = {\pi^2\over 6}</math></big>

is the solution of the [[wp:Basel problem|Basel problem]].





## 360 Assembly


```360asm
*        Sum of a series           30/03/2017
SUMSER   CSECT
         USING  SUMSER,12          base register
         LR     12,15              set addressability
         LR     10,14              save r14
         LE     4,=E'0'            s=0
         LE     2,=E'1'            i=1
       DO WHILE=(CE,2,LE,=E'1000') do i=1 to 1000
         LER    0,2                  i
         MER    0,2                  *i
         LE     6,=E'1'              1
         DER    6,0                  1/i**2
         AER    4,6                  s=s+1/i**2
         AE     2,=E'1'              i=i+1
       ENDDO    ,                  enddo i
         LA     0,4                format F13.4
         LER    0,4                s
         BAL    14,FORMATF         call formatf
         MVC    PG(13),0(1)        retrieve result
         XPRNT  PG,80              print buffer
         BR     10                 exit
         COPY   FORMATF            formatf code
PG       DC     CL80' '            buffer
         END    SUMSER
```

{{out}}

```txt

       1.6439

```




## ACL2


```lisp
(defun sum-x^-2 (max-x)
   (if (zp max-x)
       0
       (+ (/ (* max-x max-x))
          (sum-x^-2 (1- max-x)))))
```



## ActionScript


```ActionScript
function partialSum(n:uint):Number
{
	var sum:Number = 0;
	for(var i:uint = 1; i <= n; i++)
		sum += 1/(i*i);
	return sum;
}
trace(partialSum(1000));
```


## Ada


```ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Sum_Series is
   function F(X : Long_Float) return Long_Float is
   begin
      return 1.0 / X**2;
   end F;
   package Lf_Io is new Ada.Text_Io.Float_Io(Long_Float);
   use Lf_Io;
   Sum : Long_Float := 0.0;
   subtype Param_Range is Integer range 1..1000;
begin
   for I in Param_Range loop
      Sum := Sum + F(Long_Float(I));
   end loop;
   Put("Sum of F(x) from" & Integer'Image(Param_Range'First) &
      " to" & Integer'Image(Param_Range'Last) & " is ");
   Put(Item => Sum, Aft => 10, Exp => 0);
   New_Line;
end Sum_Series;
```



## Aime


```aime
real
Invsqr(real n)
{
    1 / (n * n);
}

integer
main(void)
{
    integer i;
    real sum;

    sum = 0;

    i = 1;
    while (i < 1000) {
        sum += Invsqr(i);
        i += 1;
    }

    o_real(14, sum);
    o_byte('\n');

    0;
}
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
MODE RANGE = STRUCT(INT lwb, upb);

PROC sum = (PROC (INT)LONG REAL f, RANGE range)LONG REAL:(
  LONG REAL sum := LENG 0.0;
  FOR i FROM lwb OF range TO upb OF range DO
     sum := sum + f(i)
  OD;
  sum
);

test:(
  RANGE range = (1,100);
  PROC f = (INT x)LONG REAL: LENG REAL(1) / LENG REAL(x)**2;
  print(("Sum of f(x) from", lwb OF range, " to ",upb OF range," is ", SHORTEN sum(f,range),".", new line))
)
```

Output:

```txt

Sum of f(x) from         +1 to        +100 is +1.63498390018489e  +0.

```



## APL


```APL
      +/÷2*⍨⍳1000
1.64393
```




## AppleScript

{{Trans|JavaScript}}
{{Trans|Haskell}}

```AppleScript
-- SUM OF SERIES ------------------------------------------

-- seriesSum :: Num a => (a -> a) -> [a] -> a
on seriesSum(f, xs)
    script go
        property mf : |λ| of mReturn(f)
        on |λ|(a, x)
            a + mf(x)
        end |λ|
    end script

    foldl(go, 0, xs)
end seriesSum


-- TEST ---------------------------------------------------

-- inverseSquare :: Num -> Num
on inverseSquare(x)
    1 / (x ^ 2)
end inverseSquare

on run
    seriesSum(inverseSquare, enumFromTo(1, 1000))

    --> 1.643934566682
end run


-- GENERIC FUNCTIONS --------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m ≤ n then
        set lst to {}
        repeat with i from m to n
            set end of lst to i
        end repeat
        lst
    else
        {}
    end if
end enumFromTo

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn
```

{{Out}}

```AppleScript>1.643934566682</lang



## AutoHotkey

AutoHotkey allows the precision of floating point numbers generated by math operations to be adjusted via the SetFormat command. The default is 6 decimal places.

```autohotkey
SetFormat, FloatFast, 0.15
While A_Index <= 1000
 sum += 1/A_Index**2
MsgBox,% sum  ;1.643934566681554
```



## AWK


```awk
$ awk 'BEGIN{for(i=1;i<=1000;i++)s+=1/(i*i);print s}'
1.64393
```


## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
function s(x%)
   s = 1 / x ^ 2
end function

function sum(low%, high%)
   ret = 0
   for i = low to high
      ret = ret + s(i)
   next i
   sum = ret
end function
print sum(1, 1000)
```




## BBC BASIC


```bbcbasic
      FOR i% = 1 TO 1000
        sum += 1/i%^2
      NEXT
      PRINT sum
```



## bc


```bc
define f(x) {
    return(1 / (x * x))
}

define s(n) {
    auto i, s

    for (i = 1; i <= n; i++) {
        s += f(i)
    }

    return(s)
}

scale = 20
s(1000)
```


{{Out}}

```txt
1.64393456668155979824
```



## Befunge

Emulates fixed point arithmetic with a 32 bit integer so the result is not very accurate.

```befunge
05558***>::"~"%00p"~"/10p"( }}2"*v
v*8555$_^#!:-1+*"~"g01g00+/*:\***<
<@$_,#!>#:<+*<v+*86%+55:p00<6\0/**
   "."\55+%68^>\55+/00g1-:#^_$
```

{{out}}

```txt
1.643934
```



## Bracmat


```bracmat
( 0:?i
& 0:?S
& whl'(1+!i:~>1000:?i&!i^-2+!S:?S)
& out$!S
& out$(flt$(!S,10))
);
```

Output:

```txt
8354593848314...../5082072010432.....  (1732 digits and a slash)
1,6439345667*10E0
```



## Brat


```brat
p 1.to(1000).reduce 0 { sum, x | sum + 1.0 / x ^ 2 }  #Prints 1.6439345666816
```



## C


```c
#include <stdio.h>

double Invsqr(double n)
{
	return 1 / (n*n);
}

int main (int argc, char *argv[])
{
	int i, start = 1, end = 1000;
	double sum = 0.0;

	for( i = start; i <= end; i++)
		sum += Invsqr((double)i);

	printf("%16.14f\n", sum);

	return 0;
}
```



## C++


```cpp
#include <iostream>

double f(double x);

int main()
{
    unsigned int start = 1;
    unsigned int end = 1000;
    double sum = 0;

    for( unsigned int x = start; x <= end; ++x )
    {
        sum += f(x);
    }
    std::cout << "Sum of f(x) from " << start << " to " << end << " is " << sum << std::endl;
    return 0;
}


double f(double x)
{
    return ( 1.0 / ( x * x ) );
}
```


## C#

```c#
class Program
{
    static void Main(string[] args)
    {
        // Create and fill a list of number 1 to 1000

        List<double> myList = new List<double>();
        for (double i = 1; i < 1001; i++)
        {
            myList.Add(i);
        }
        // Calculate the sum of 1/x^2

        var sum = myList.Sum(x => 1/(x*x));

        Console.WriteLine(sum);
        Console.ReadLine();
    }
}
```


An alternative approach using Enumerable.Range() to generate the numbers.


```c#
class Program
{
    static void Main(string[] args)
    {
        double sum = Enumerable.Range(1, 1000).Sum(x => 1.0 / (x * x));

        Console.WriteLine(sum);
        Console.ReadLine();
    }
}
```



## CLIPS


```clips
(deffunction S (?x) (/ 1 (* ?x ?x)))
(deffunction partial-sum-S
  (?start ?stop)
  (bind ?sum 0)
  (loop-for-count (?i ?start ?stop) do
    (bind ?sum (+ ?sum (S ?i)))
  )
  (return ?sum)
)
```


Usage:

```txt
CLIPS> (partial-sum-S 1 1000)
1.64393456668156
```



## Clojure


```clojure
(reduce + (map #(/ 1.0 % %) (range 1 1001)))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. sum-of-series.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  N                       VALUE 1000.

       01  series-term             USAGE FLOAT-LONG.
       01  i                       PIC 9(4).

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL N < i
               COMPUTE series-term = series-term + (1 / i ** 2)
           END-PERFORM

           DISPLAY series-term

           GOBACK
           .
```

{{out}}

```txt

1.643933784000000120

```



## CoffeeScript


```CoffeeScript

console.log [1..1000].reduce((acc, x) -> acc + (1.0 / (x*x)))

```



## Common Lisp


```lisp
(loop for x from 1 to 1000 summing (expt x -2))
```



## Crystal

{{trans|Ruby}}

```ruby
puts (1..1000).sum{ |x| 1.0 / x ** 2 }
puts (1..5000).sum{ |x| 1.0 / x ** 2 }
puts (1..9999).sum{ |x| 1.0 / x ** 2 }
puts Math::PI ** 2 / 6
```

{{out}}

```txt

1.6439345666815615
1.6447340868469014
1.6448340618480652
1.6449340668482264

```



## D


### More Procedural Style


```d
import std.stdio, std.traits;

ReturnType!TF series(TF)(TF func, int end, int start=1)
pure nothrow @safe @nogc {
    typeof(return) sum = 0;
    foreach (immutable i; start .. end + 1)
        sum += func(i);
    return sum;
}

void main() {
    writeln("Sum: ", series((in int n) => 1.0L / (n ^^ 2), 1_000));
}
```

{{out}}

```txt
Sum: 1.64393
```



### More functional Style

Same output.

```d
import std.stdio, std.algorithm, std.range;

enum series(alias F) = (in int end, in int start=1)
    pure nothrow @nogc => iota(start, end + 1).map!F.sum;

void main() {
    writeln("Sum: ", series!q{1.0L / (a ^^ 2)}(1_000));
}
```



## Dart

{{trans|Scala}}

```dart
main() {
  var list = new List<int>.generate(1000, (i) => i + 1);

  num sum = 0;

  (list.map((x) => 1.0 / (x * x))).forEach((num e) {
    sum += e;
  });
  print(sum);
}
```


{{trans|F#}}

```dart
f(double x) {
  if (x == 0)
    return x;
  else
    return (1.0 / (x * x)) + f(x - 1.0);
}

main() {
  print(f(1000));
}
```



## Delphi


```Delphi

unit Form_SumOfASeries_Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormSumOfASeries = class(TForm)
    M_Log: TMemo;
    B_Calc: TButton;
    procedure B_CalcClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormSumOfASeries: TFormSumOfASeries;

implementation

{$R *.dfm}

function Sum_Of_A_Series(_from,_to:int64):extended;
begin
  result:=0;
  while _from<=_to do
  begin
    result:=result+1.0/(_from*_from);
    inc(_from);
  end;
end;

procedure TFormSumOfASeries.B_CalcClick(Sender: TObject);
begin
  try
    M_Log.Lines.Add(FloatToStr(Sum_Of_A_Series(1, 1000)));
  except
    M_Log.Lines.Add('Error');
  end;
end;

end.


```

{{out}}

```txt
1.64393456668156
```



## DWScript



```delphi

var s : Float;
for var i := 1 to 1000 do
   s += 1 / Sqr(i);

PrintLn(s);

```



## E



```e
pragma.enable("accumulator")
accum 0 for x in 1..1000 { _ + 1 / x ** 2 }
```



## EchoLisp


```lisp

(lib 'math) ;; for (sigma f(n) nfrom nto) function
(Σ (λ(n) (// (* n n))) 1 1000)
;; or
(sigma (lambda(n) (// (* n n))) 1 1000)
    → 1.6439345666815615

(// (* PI PI) 6)
    → 1.6449340668482264

```



## Eiffel


```eiffel

note
	description: "Compute the n-th term of a series"

class
	SUM_OF_SERIES_EXAMPLE

inherit
	MATH_CONST

create
	make

feature -- Initialization

	make
		local
			approximated, known: REAL_64
		do
			known := Pi^2 / 6

			approximated := sum_until (agent g, 1001)
			print ("%Nzeta function exact value: %N")
			print (known)
			print ("%Nzeta function approximated value: %N")
			print (approximated)
		end

feature -- Access

	g (k: INTEGER): REAL_64
			-- 'k'-th term of the serie
		require
			k_positive: k > 0
		do
			Result := 1 / (k * k)
		end

	sum_until (s: FUNCTION [ANY, TUPLE [INTEGER], REAL_64]; n: INTEGER): REAL_64
			-- sum of the 'n' first terms of 's'
		require
			n_positive: n > 0
			one_parameter: s.open_count = 1
		do
			Result := 0
			across 1 |..| n as it loop
				Result := Result + s.item ([it.item])
			end
		end

end


```



## Elixir


```elixir
iex(1)> Enum.reduce(1..1000, 0, fn x,sum -> sum + 1/(x*x) end)
1.6439345666815615
```


## Elena

ELENA 4.x :

```elena
import system'routines;
import extensions;

public program()
{
    var sum := new Range(1, 1000).selectBy:(x => 1.0r / (x * x)).summarize(new Real());

    console.printLine:sum
}
```

{{out}}

```txt

1.643933566682

```



## Emacs Lisp


```Emacs Lisp

(defun serie (n)
  (if (< 0 n)
      (apply '+ (mapcar (lambda (k) (/ 1.0 (* k k) )) (number-sequence 1 n) ))
    (error "input error") ))

(insert (format "%.10f" (serie 1000) ))

```

<b>Output:</b>

```txt

1.6439345667

```



## Erlang



```erlang
lists:sum([1/math:pow(X,2) || X <- lists:seq(1,1000)]).
```



## Euphoria

{{works with|Euphoria|4.0.0}}
This is based on the [[BASIC]] example.

```Euphoria

function s( atom x )
	return 1 / power( x, 2 )
end function

function sum( atom low, atom high )
	atom ret = 0.0
	for i = low to high do
		ret = ret + s( i )
	end for
	return ret
end function

printf( 1, "%.15f\n", sum( 1, 1000 ) )
```



## Ezhil


```Ezhil

## இந்த நிரல் தொடர் கூட்டல் (Sum Of Series) என்ற வகையைச் சேர்ந்தது

## இந்த நிரல் ஒன்று முதல் தரப்பட்ட எண் வரை 1/(எண் * எண்) எனக் கணக்கிட்டுக் கூட்டி விடை தரும்

நிரல்பாகம் தொடர்க்கூட்டல்(எண்1)

  எண்2 = 0

  @(எண்3 = 1, எண்3 <= எண்1, எண்3 = எண்3 + 1) ஆக

    ## ஒவ்வோர் எண்ணின் வர்க்கத்தைக் கணக்கிட்டு, ஒன்றை அதனால் வகுத்துக் கூட்டுகிறோம்

    எண்2 = எண்2 + (1 / (எண்3 * எண்3))

  முடி

  பின்கொடு (எண்2)

முடி

அ = int(உள்ளீடு("ஓர் எண்ணைச் சொல்லுங்கள்: "))

பதிப்பி "நீங்கள் தந்த எண் " அ
பதிப்பி "அதன் தொடர்க் கூட்டல் " தொடர்க்கூட்டல்(அ)


```



## Factor


```factor
1000 [1,b] [ >float sq recip ] map-sum
```



## Fantom


Within 'fansh':


```fantom

fansh> (1..1000).toList.reduce(0.0f) |Obj a, Int v -> Obj| { (Float)a + (1.0f/(v*v)) }
1.6439345666815615

```



## Fish


```fish
0&aaa**>::*1$,&v
    ;n&^?:-1&+ <
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Sum_of_a_series this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: sum ( fn start count -- fsum )
  0e
  bounds do
    i s>d d>f dup execute f+
  loop drop ;

:noname ( x -- 1/x^2 ) fdup f* 1/f ;   ( xt )
1 1000 sum f.       \ 1.64393456668156
pi pi f* 6e f/ f.   \ 1.64493406684823
```



## Fortran

In ISO Fortran 90 and later, use SUM intrinsic:

```fortran
real, dimension(1000) :: a = (/ (1.0/(i*i), i=1, 1000) /)
real :: result

result = sum(a);
```

Or in Fortran 77:

```fortran
      s=0
      do i=1,1000
          s=s+1./i**2
      end do
      write (*,*) s
      end
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Const pi As Double = 3.141592653589793

Function sumSeries (n As UInteger) As Double
  If n = 0 Then Return 0
  Dim sum As Double = 0
  For k As Integer = 1 To n
    sum += 1.0/(k * k)
  Next
  Return sum
End Function

Print "s(1000) = "; sumSeries(1000)
Print "zeta(2) = "; Pi * pi / 6
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

s(1000) =  1.643934566681562
zeta(2) =  1.644934066848226

```



## Frink

Frink can calculate the series with exact rational numbers or floating-point values.

```frink

sum[map[{|k| 1/k^2}, 1 to 1000]]

```

{{out}}

```txt

83545938483149689478187854264854884386044454314086472930763839512603803291207881839588904977469387999844962675327115010933903589145654299730231109091124308462732153297321867661093162618281746011828755017021645889046777854795025297006943669294330752479399654716368801794529682603741344724733173765262964463970763934463926259796895140901128384286333311745462863716753134735154188954742414035836608258393970996630553795415075904205673610359458498106833291961256452756993199997231825920203667952667546787052535763624910912251107083702817265087341966845358732584971361645348091123849687614886682117125784781422103460192439394780707024963279033532646857677925648889105430050030795563141941157379481719403833258405980463950499887302926152552848089894630843538497552630691676216896740675701385847032173192623833881016332493844186817408141003602396236858699094240207812766449/50820720104325812617835292273000760481839790754374852703215456050992581046448162621598030244504097240825920773913981926305208272518886258627010933716354037062979680120674828102224650586465553482032614190502746121717248161892239954030493982549422690846180552358769564169076876408783086920322038142618269982747137757706040198826719424371333781947889528085329853597116893889786983109597085041878513917342099206896166585859839289193299599163669641323895022932959750057616390808553697984192067774252834860398458100840611325353202165675189472559524948330224159123505567527375848194800452556940453530457590024173749704941834382709198515664897344438584947842793131829050180589581507273988682409028088248800576590497216884808783192565859896957125449502802395453976401743504938336291933628859306247684023233969172475385327442707968328512729836445886537101453118476390400000000 (approx. 1.6439345666815598)

```

Change <code>1/k^2</code> to <code>1.0/k^2</code> to use floating-point math.

=={{header|F_Sharp|F#}}==
The following function will do the task specified.

```fsharp
let rec f (x : float) =
    match x with
        | 0. -> x
        | x -> (1. / (x * x)) + f (x - 1.)
```

In the interactive F# console, using the above gives:

```fsharp>
 f 1000. ;;
val it : float = 1.643934567
```

However this recursive function will run out of stack space eventually (try 100000). A [[:Category:Recursion|tail-recursive]] implementation will not consume stack space and can therefore handle much larger ranges. Here is such a version:

```fsharp
#light
let sum_series (max : float) =
    let rec f (a:float, x : float) =
        match x with
            | 0. -> a
            | x -> f ((1. / (x * x) + a), x - 1.)
    f (0., max)

[<EntryPoint>]
let main args =
    let (b, max) = System.Double.TryParse(args.[0])
    printfn "%A" (sum_series max)
    0
```

This block can be compiled using ''fsc --target exe filename.fs'' or used interactively without the main function.

## GAP


```gap
# We will compute the sum exactly

# Computing an approximation of a rationnal (giving a string)
# Value is truncated toward zero
Approx := function(x, d)
	local neg, a, b, n, m, s;
	if x < 0 then
		x := -x;
		neg := true;
	else
		neg := false;
	fi;
	a := NumeratorRat(x);
	b := DenominatorRat(x);
	n := QuoInt(a, b);
	a := RemInt(a, b);
	m := 10^d;
	s := "";
	if neg then
		Append(s, "-");
	fi;
	Append(s, String(n));
	n := Size(s) + 1;
	Append(s, String(m + QuoInt(a*m, b)));
	s[n] := '.';
	return s;
end;

a := Sum([1 .. 1000], n -> 1/n^2);;
Approx(a, 10);
"1.6439345666"
# and pi^2/6 is 1.6449340668, truncated to ten digits
```



## Genie


```genie
[indent=4]
/*
   Sum of series, in Genie
   valac sumOfSeries.gs
   ./sumOfSeries
*/

delegate sumFunc(n:int):double

def sum_series(start:int, end:int, f:sumFunc):double
    sum:double = 0.0
    for var i = start to end do sum += f(i)
    return sum


def oneOverSquare(n:int):double
    return (1 / (double)(n * n))

init
    Intl.setlocale()
    print "ζ(2) approximation: %16.15f", sum_series(1, 1000, oneOverSquare)
    print "π² / 6            : %16.15f", Math.PI * Math.PI / 6.0
```


{{out}}

```txt
prompt$ valac sumOfSeries.gs
prompt$ ./sumOfSeries
ζ(2) approximation: 1.643934566681561
π² / 6            : 1.644934066848226
```



## GEORGE


```GEORGE

0 (s)
1, 1000 rep (i)
   s 1 i dup × / + (s) ;
]
P

```

Output:-

```txt

 1.643934566681561

```



## Go


```go
package main

import ("fmt"; "math")

func main() {
    fmt.Println("known:   ", math.Pi*math.Pi/6)
    sum := 0.
    for i := 1e3; i > 0; i-- {
        sum += 1 / (i * i)
    }
    fmt.Println("computed:", sum)
}
```

Output:

```txt
known:    1.6449340668482264
computed: 1.6439345666815597
```



## Groovy

Start with smallest terms first to minimize rounding error:

```groovy
println ((1000..1).collect { x -> 1/(x*x) }.sum())
```


Output:

```txt
1.6439345654
```



## Haskell

With a list comprehension:

```haskell
sum [1 / x ^ 2 | x <- [1..1000]]
```

With higher-order functions:

```haskell
sum $ map (\x -> 1 / x ^ 2) [1..1000]
```

In [http://haskell.org/haskellwiki/Pointfree point-free] style:

```haskell
(sum . map (1/) . map (^2)) [1..1000]
```

or

```haskell
(sum . map ((1 /) . (^ 2))) [1 .. 1000]
```


or, as a single fold:


```haskell
seriesSum f = foldr ((+) . f) 0

inverseSquare = (1 /) . (^ 2)

main :: IO ()
main = print $ seriesSum inverseSquare [1 .. 1000]
```

{{Out}}

```txt
1.6439345666815615
```



## HicEst


```hicest
REAL :: a(1000)
        a = 1 / $^2
        WRITE(ClipBoard, Format='F17.15') SUM(a)
```


```hicest>1.643934566681561</lang


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
   local i, sum
   sum := 0 & i := 0
   every sum +:= 1.0/((| i +:= 1 ) ^ 2) \1000
   write(sum)
end
```


or


```icon
procedure main()
    every (sum := 0) +:= 1.0/((1 to 1000)^2)
    write(sum)
end
```


Note: The terse version requires some explanation.  Icon expressions all return values or references if they succeed.  As a result, it is possible to have expressions like these below:

```icon

   x := y := 0   # := is right associative so, y is assigned 0, then x
   1 < x < 99    # comparison operators are left associative so, 1 < x returns x (if it is greater than 1), then x < 99 returns 99 if the comparison succeeds
   (sum := 0)    # returns a reference to sum which can in turn be used with augmented assignment +:=

```



## IDL



```idl
print,total( 1/(1+findgen(1000))^2)
```



## Io


```txt
Io 20110905
Io> sum := 0 ; Range 1 to(1000) foreach(k, sum = sum + 1/(k*k))
==> 1.6439345666815615
Io> 1 to(1000) map(k, 1/(k*k)) sum
==> 1.6439345666815615
Io>
```

The expression using <code>map</code> generates a list internally.  Using <code>foreach</code> does not.


## J


```j
   NB. sum of reciprocals of squares of first thousand positive integers
   +/ % *: >: i. 1000
1.64393

   (*:o.1)%6       NB. pi squared over six, for comparison
1.64493

   1r6p2           NB.  As a constant (J has a rich constant notation)
1.64493
```



## Java


```java
public class Sum{
    public static double f(double x){
       return 1/(x*x);
    }

    public static void main(String[] args){
       double start = 1;
       double end = 1000;
       double sum = 0;

       for(double x = start;x <= end;x++) sum += f(x);

       System.out.println("Sum of f(x) from " + start + " to " + end +" is " + sum);
    }
}
```



## JavaScript


### ES5


```javascript
function sum(a,b,fn) {
   var s = 0;
   for ( ; a <= b; a++) s += fn(a);
   return s;
}

 sum(1,1000, function(x) { return 1/(x*x) } )  // 1.64393456668156
```


or, in a functional idiom:


```JavaScript
(function () {

  function sum(fn, lstRange) {
    return lstRange.reduce(
      function (lngSum, x) {
        return lngSum + fn(x);
      }, 0
    );
  }

  function range(m, n) {
    return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
      return m + i;
    });
  }


  return sum(
    function (x) {
      return 1 / (x * x);
    },
    range(1, 1000)
  );

})();
```


{{Out}}


```JavaScript>1.6439345666815615</lang



### ES6

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // SUM OF A SERIES -------------------------------------------------------

    // seriesSum :: Num a => (a -> a) -> [a] -> a
    const seriesSum = (f, xs) =>
        foldl((a, x) => a + f(x), 0, xs);


    // GENERIC ---------------------------------------------------------------

    // enumFromToInt :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // TEST ------------------------------------------------------------------

    return seriesSum(x => 1 / (x * x), enumFromTo(1, 1000));
})();
```

{{Out}}

```JavaScript>1.6439345666815615</lang



## jq

The jq idiom for efficient computation of this kind of sum is to use "reduce", either directly or using a summation wrapper function.

Directly:

```jq
def s(n): reduce range(1; n+1) as $k (0; . + 1/($k * $k) );

s(1000)

```

{{Out}}
 1.6439345666815615

Using a generic summation wrapper function allows problems specified in "sigma" notation to be solved using syntax that closely resembles that notation:


```jq
def summation(s): reduce s as $k (0; . + $k);

summation( range(1; 1001) | (1/(. * .) ) )
```


An important point is that nothing is lost in efficiency using the declarative and quite elegant approach using "summation".


## Jsish

From Javascript ES5.


```javascript
#!/usr/bin/jsish
/* Sum of a series */
function sum(a:number, b:number , fn:function):number {
   var s = 0;
   for ( ; a <= b; a++) s += fn(a);
   return s;
}

;sum(1, 1000, function(x) { return 1/(x*x); } );

/*
=!EXPECTSTART!=
sum(1, 1000, function(x) { return 1/(x*x); } ) ==> 1.643934566681561
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U sumOfSeries.jsi
sum(1, 1000, function(x) { return 1/(x*x); } ) ==> 1.643934566681561
```



## Julia

Using a higher-order function:


```Julia>julia
 sum(k -> 1/k^2, 1:1000)
1.643934566681559

julia> pi^2/6
1.6449340668482264

```


A simple loop is more optimized:


```Julia>julia
 function f(n)
    s = 0.0
    for k = 1:n
      s += 1/k^2
    end
    return s
end

julia> f(1000)
1.6439345666815615
```



## K


```k
  ssr: +/1%_sqr
  ssr 1+!1000
1.643935
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val n = 1000
    val sum = (1..n).sumByDouble { 1.0 / (it * it) }
    println("Actual sum is $sum")
    println("zeta(2)    is ${Math.PI * Math.PI / 6.0}")
}
```


{{out}}

```txt

Actual sum is 1.6439345666815615
zeta(2)    is 1.6449340668482264

```



## Lang5


```lang5
1000 iota 1 + 1 swap / 2 ** '+ reduce .
```





## Lasso


```Lasso
define sum_of_a_series(n::integer,k::integer) => {
	local(sum = 0)
	loop(-from=#k,-to=#n) => {
		#sum += 1.00/(math_pow(loop_count,2))
	}
	return #sum
}
sum_of_a_series(1000,1)
```

{{out}}

```txt
1.643935
```



## LFE



###  With <code>lists:foldl</code>



```lisp

(defun sum-series (nums)
  (lists:foldl
    #'+/2
    0
    (lists:map
      (lambda (x) (/ 1 x x))
      nums)))

```



###  With <code>lists:sum</code>



```lisp

(defun sum-series (nums)
  (lists:sum
    (lists:map
      (lambda (x) (/ 1 x x))
      nums)))

```


Both have the same result:


```lisp

> (sum-series (lists:seq 1 100000))
1.6449240668982423

```



## Liberty BASIC


```lb

for i =1 to 1000
  sum =sum +1 /( i^2)
next i

print sum

end

```



## Lingo


```lingo
the floatprecision = 8
sum = 0
repeat with i = 1 to 1000
  sum = sum + 1/power(i, 2)
end repeat
put sum
-- 1.64393457
```



## LiveCode


```LiveCode
repeat with i = 1 to 1000
    add 1/(i^2) to summ
end repeat
put summ  //1.643935
```



## Logo


```logo
to series :fn :a :b
  localmake "sigma 0
  for [i :a :b] [make "sigma :sigma + invoke :fn :i]
  output :sigma
end
to zeta.2 :x
  output 1 / (:x * :x)
end
print series "zeta.2 1 1000
make "pi (radarctan 0 1) * 2
print :pi * :pi / 6
```



## Lua


```lua

sum = 0
for i = 1, 1000 do sum = sum + 1/i^2 end
print(sum)

```


## Lucid


```lucid>series = ssum asa  n
= 1000
   where
         num = 1 fby num + 1;
         ssum = ssum + 1/(num * num)
   end;
```



## Maple


```Maple
sum(1/k^2, k=1..1000);
```

{{Out|Output}}

```txt
-Psi(1, 1001)+(1/6)*Pi^2
```



## Mathematica

This is the straightforward solution of the task:

```mathematica
Sum[1/x^2, {x, 1, 1000}]
```

However this returns a quotient of two huge integers (namely the ''exact'' sum); to get a floating point approximation, use <tt>N</tt>:

```mathematica
N[Sum[1/x^2, {x, 1, 1000}]]
```

or better:

```mathematica
NSum[1/x^2, {x, 1, 1000}]
```

Which gives a higher or equal accuracy/precision. Alternatively, get Mathematica to do the whole calculation in floating point by using a floating point value in the formula:

```mathematica
Sum[1./x^2, {x, 1, 1000}]
```

Other ways include (exact, approximate,exact,approximate):

```mathematica
Total[Table[1/x^2, {x, 1, 1000}]]
Total[Table[1./x^2, {x, 1, 1000}]]
Plus@@Table[1/x^2, {x, 1, 1000}]
Plus@@Table[1./x^2, {x, 1, 1000}]
```



## MATLAB


```matlab
   sum([1:1000].^(-2))
```



## Maxima



```maxima
(%i45) sum(1/x^2, x, 1, 1000);
       835459384831496894781878542648[806 digits]396236858699094240207812766449
(%o45) ------------------------------------------------------------------------
       508207201043258126178352922730[806 digits]886537101453118476390400000000

(%i46) sum(1/x^2, x, 1, 1000),numer;
(%o46) 1.643934566681561
```



## MAXScript


```maxscript
total = 0
for i in 1 to 1000 do
(
    total += 1.0 / pow i 2
)
print total
```



## min

{{works with|min|0.19.3}}

```min
0 1 (
  ((dup * 1 swap /) (id)) cleave
  ((+) (succ)) spread
) 1000 times pop print
```

{{out}}

```txt

1.643934566681562

```



## MiniScript


```MiniScript
zeta = function(num)
    return 1 / num^2
end function

sum = function(start, finish, formula)
    total = 0
    for i in range(start, finish)
        total = total + formula(i)
    end for
    return total
end function

print sum(1, 1000, @zeta)

```

{{out}}

```txt

1.643935

```


=={{header|МК-61/52}}==
<lang>0	П0	П1	ИП1	1	+	П1	x^2	1/x	ИП0
+	П0	ИП1	1	0	0	0	-	x>=0	03
ИП0	С/П
```



## ML


=
## Standard ML
=

```Standard ML

(* 1.64393456668 *)
List.foldl op+ 0.0 (List.tabulate(1000, fn x => 1.0 / Math.pow(real(x + 1),2.0)))

```


=
## mLite
=

```ocaml
println ` fold (+, 0) ` map (fn x = 1 / x ^ 2) ` iota (1,1000);
```

Output:

```txt
1.6439345666815549
```



## MMIX


```mmix
x	IS	$1	% flt calculations
y	IS	$2	%   id
z	IS	$3	% z = sum series
t	IS	$4	% temp var

	LOC	Data_Segment
	GREG	@
BUF	OCTA	0,0,0		% print buffer

	LOC	#1000
	GREG	@

// print floating point number in scientific format: 0.xxx...ey..
// most of this routine is adopted from:
// http://www.pspu.ru/personal/eremin/emmi/rom_subs/printreal.html
// float number in z
	GREG	@
NaN	BYTE	"NaN..",0
NewLn	BYTE	#a,0
1H	LDA	x,NaN
	TRAP	0,Fputs,StdOut
	GO	$127,$127,0

prtFlt	FUN	x,z,z		% test if z == NaN
	BNZ	x,1B
	CMP	$73,z,0		% if necessary remember it is neg
	BNN	$73,4F
Sign	BYTE	'-'
	LDA	$255,Sign
	TRAP	0,Fputs,StdOut
	ANDNH	z,#8000		% make number pos
// normalizing float number
4H	SETH	$74,#4024	% initialize mulfactor = 10.0
	SETH	$73,#0023
	INCMH	$73,#86f2
	INCML	$73,#6fc1	%
	FLOT	$73,$73		% $73 = float 10^16
	SET	$75,16		% set # decimals to 16
8H	FCMP	$72,z,$73	% while z >= 10^16 do
	BN	$72,9F		%
	FDIV	z,z,$74		%  z = z / 10.0
	ADD	$75,$75,1	%  incr exponent
	JMP	8B		% wend
9H	FDIV	$73,$73,$74	% 10^16 / 10.0
5H	FCMP	$72,z,$73	% while z < 10^15 do
	BNN	$72,6F
	FMUL	z,z,$74		%  z = z * 10.0
	SUB	$75,$75,1	%  exp = exp - 1
	JMP	5B
NulPnt	BYTE	'0','.',#00
6H	LDA	$255,NulPnt	% print '0.' to StdOut
	TRAP	0,Fputs,StdOut
	FIX	z,0,z		% convert float z to integer
// print mantissa
0H	GREG	#3030303030303030
	STO	0B,BUF
	STO	0B,BUF+8	% store print mask in buffer
	LDA	$255,BUF+16	% points after LSD
				% repeat
2H	SUB	$255,$255,1	%   move pointer down
	DIV	z,z,10		%   (q,r) = divmod z 10
	GET	t,rR		%   get remainder
	INCL	t,'0'		%   convert to ascii digit
	STBU	t,$255,0	%   store digit in buffer
	BNZ	z,2B		% until q == 0
	TRAP	0,Fputs,StdOut	% print mantissa
Exp	BYTE	'e',#00
	LDA	$255,Exp	% print 'exponent' indicator
	TRAP	0,Fputs,StdOut
// print exponent
0H	GREG	#3030300000000000
	STO	0B,BUF
	LDA	$255,BUF+2	% store print mask in buffer
	CMP	$73,$75,0	% if exp neg then place - in buffer
	BNN	$73,2F
ExpSign	BYTE	'-'
	LDA	$255,ExpSign
	TRAP	0,Fputs,StdOut
	NEG	$75,$75		% make exp positive
2H	LDA	$255,BUF+3	% points after LSD
				% repeat
3H	SUB	$255,$255,1	%   move pointer down
	DIV	$75,$75,10	%   (q,r) = divmod exp 10
	GET	t,rR
	INCL	t,'0'
	STBU	t,$255,0	%   store exp. digit in buffer
	BNZ	$75,3B		% until q == 0
	TRAP	0,Fputs,StdOut	% print exponent
	LDA	$255,NewLn
	TRAP	0,Fputs,StdOut	% do a NL
	GO	$127,$127,0	% return

i  IS $5 ;iu IS $6
Main	SET	iu,1000
	SETH	y,#3ff0     y = 1.0
	SETH	z,#0000     z = 0.0
	SET	i,1          for (i=1;i<=1000; i++ ) {
1H	FLOT	x,i           x = int i
	FMUL	x,x,x         x = x^2
	FDIV	x,y,x         x = 1 / x
	FADD	z,z,x         s = s + x
	ADD	i,i,1
	CMP	t,i,iu
	PBNP	t,1B         } z = sum
	GO	$127,prtFlt  print sum --> StdOut
	TRAP	0,Halt,0
```

Output:

```txt
~/MIX/MMIX/Rosetta> mmix sumseries
0.1643934566681562e1
```


=={{header|Modula-3}}==
Modula-3 uses D0 after a floating point number as a literal for <tt>LONGREAL</tt>.

```modula3
MODULE Sum EXPORTS Main;

IMPORT IO, Fmt, Math;

VAR sum: LONGREAL := 0.0D0;

PROCEDURE F(x: LONGREAL): LONGREAL =
  BEGIN
    RETURN 1.0D0 / Math.pow(x, 2.0D0);
  END F;

BEGIN
  FOR i := 1 TO 1000 DO
    sum := sum + F(FLOAT(i, LONGREAL));
  END;
  IO.Put("Sum of F(x) from 1 to 1000 is ");
  IO.Put(Fmt.LongReal(sum));
  IO.Put("\n");
END Sum.
```

Output:

```txt

Sum of F(x) from 1 to 1000 is 1.6439345666815612

```



## MUMPS


```MUMPS

SOAS(N)
 NEW SUM,I SET SUM=0
 FOR I=1:1:N DO
 .SET SUM=SUM+(1/((I*I)))
 QUIT SUM

```

This is an extrinsic function so the usage is:

```txt

USER>SET X=$$SOAS^ROSETTA(1000) WRITE X
1.643934566681559806

```



## Nial


```nial
|sum (1 / power (count 1000) 2)
=1.64393
```



## NewLISP


```NewLISP
(let (s 0)
  (for (i 1 1000)
    (inc s (div 1 (* i i))))
  (println s))
```



## Nim


```nim
import math

var ls: seq[float] = @[]
for x in 1..1000:
  ls.add(1.0 / float(x * x))
echo sum(ls)
```



## Objeck


```objeck

bundle Default {
  class SumSeries {
    function : Main(args : String[]) ~ Nil {
      DoSumSeries();
    }

    function : native : DoSumSeries() ~ Nil {
      start := 1;
      end := 1000;

      sum := 0.0;

      for(x : Float := start; x <= end; x += 1;) {
        sum += f(x);
      };

      IO.Console->GetInstance()->Print("Sum of f(x) from ")->Print(start)->Print(" to ")->Print(end)->Print(" is ")->PrintLine(sum);
    }

    function : native : f(x : Float) ~ Float {
      return 1.0 / (x * x);
    }
  }
}

```



## OCaml


```ocaml
let sum a b fn =
  let result = ref 0. in
  for i = a to b do
    result := !result +. fn i
  done;
  !result
```


 # sum 1 1000 (fun x -> 1. /. (float x ** 2.))
 - : float = 1.64393456668156124

or in a functional programming style:

```ocaml
let sum a b fn =
  let rec aux i r =
    if i > b then r
    else aux (succ i) (r +. fn i)
  in
  aux a 0.
;;
```

Simple recursive solution:

```ocaml
let rec sum n = if n < 1 then 0.0 else sum (n-1) +. 1.0 /. float (n*n)
in sum 1000
```



## Octave

Given a vector, the sum of all its elements is simply <code>sum(vector)</code>; a range can be ''generated'' through the range notation: <code>sum(1:1000)</code> computes the sum of all numbers from 1 to 1000. To compute the requested series, we can simply write:


```octave
sum(1 ./ [1:1000] .^ 2)
```



## Oforth



```Oforth
: sumSerie(s, n)   0 n seq apply(#[ s perform + ]) ;
```


Usage :

```Oforth
 #[ sq inv ] 1000 sumSerie println
```


{{out}}

```txt

1.64393456668156

```



## OpenEdge/Progress


Conventionally like elsewhere:

<lang Progress (Openedge ABL)>def var dcResult as decimal no-undo.
def var n as int no-undo.

do n = 1 to 1000 :
  dcResult = dcResult + 1 / (n * n)  .
end.

display dcResult .
```


or like this:

<lang Progress (Openedge ABL)>def var n as int no-undo.

repeat n = 1 to 1000 :
  accumulate 1 / (n * n) (total).
end.

display ( accum total 1 / (n * n) )  .
```



## Oz

With higher-order functions:

```oz
declare
  fun {SumSeries S N}
     {FoldL {Map {List.number 1 N 1} S}
      Number.'+' 0.}
  end

  fun {S X}
     1. / {Int.toFloat X*X}
  end
in
  {Show {SumSeries S 1000}}
```


Iterative:

```oz
  fun {SumSeries S N}
     R = {NewCell 0.}
  in
     for I in 1..N do
        R := @R + {S I}
     end
     @R
  end
```



## PARI/GP

Exact rational solution:

```parigp
sum(n=1,1000,1/n^2)
```


Real number solution (accurate to <math>3\cdot10^{-36}</math> at standard precision):

```parigp
sum(n=1,1000,1./n^2)
```


Approximate solution (accurate to <math>9\cdot10^{-11}</math> at standard precision):

```parigp
zeta(2)-intnum(x=1000.5,[1],1/x^2)
```

or

```parigp
zeta(2)-1/1000.5
```



## Panda


```panda
sum{{1.0.divide(1..1000.sqr)}}
```

Output:

```txt
1.6439345666815615
```



## Pascal


```pascal
Program SumSeries;
type
  tOutput = double;//extended;
  tmyFunc = function(number: LongInt): tOutput;

function f(number: LongInt): tOutput;
begin
  f := 1/sqr(tOutput(number));
end;

function Sum(from,upto: LongInt;func:tmyFunc):tOutput;
var
  res: tOutput;
begin
  res := 0.0;
//  for from:= from to upto do res := res + f(from);
  for upTo := upto downto from do res := res + f(upTo);
  Sum := res;
end;

BEGIN
  writeln('The sum of 1/x^2 from 1 to 1000 is: ', Sum(1,1000,@f));
  writeln('Whereas pi^2/6 is:                  ', pi*pi/6:10:8);
end.
```

Output

```txt
different version of type and calculation
extended low to high 1.64393456668155980263E+0000
extended high to low 1.64393456668155980307E+0000
  double low to high 1.6439345666815612E+000
  double high to low 1.6439345666815597E+000
Out:
The sum of 1/x^2 from 1 to 1000 is:  1.6439345666815612E+000
Whereas pi^2/6 is:                  1.64493407

```



## Perl


```perl
my $sum = 0;
$sum += 1 / $_ ** 2 foreach 1..1000;
print "$sum\n";
```

or

```perl
use List::Util qw(reduce);
$sum = reduce { $a + 1 / $b ** 2 } 0, 1..1000;
print "$sum\n";
```

An other way of doing it is to define the series as a closure:

```perl
my $S = do { my ($sum, $k); sub { $sum += 1/++$k**2 } };
my @S = map &$S, 1 .. 1000;
print $S[-1];
```



## Perl 6

{{Works with|rakudo|2016.04}}

In general, the <code>$n</code>th partial sum of a series whose terms are given by a unary function <code>&f</code> is


```perl6
[+] map &f, 1 .. $n
```


So what's needed in this case is


```perl6
say [+] map { 1 / $^n**2 }, 1 .. 1000;
```


Or, using the "hyper" metaoperator to vectorize, we can use a more "point free" style while keeping traditional precedence:

```perl6
say [+] 1 «/« (1..1000) »**» 2;
```


Or we can use the <tt>X</tt> "cross" metaoperator, which is convenient even if one side or the other is a scalar.  In this case, we demonstrate a scalar on either side:


```perl6
say [+] 1 X/ (1..1000 X** 2);
```

Note that cross ops are parsed as list infix precedence rather than using the precedence of the base op as hypers do.  Hence the difference in parenthesization.

With list comprehensions, you can write:


```perl6
say [+] (1 / $_**2 for 1..1000);
```


That's fine for a single result, but if you're going to be evaluating the sequence multiple times, you don't want to be recalculating the sum each time, so it's more efficient to define the sequence as a constant to let the run-time automatically cache those values already calculated.  In a lazy language like Perl 6, it's generally considered a stronger abstraction to write the correct infinite sequence, and then take the part of it you're interested in.
Here we define an infinite sequence of partial sums (by adding a backslash into the reduction to make it look "triangular"), then take the 1000th term of that:

```perl6
constant @x = [\+] 0, { 1 / ++(state $n) ** 2 } ... *;
say @x[1000];  # prints 1.64393456668156
```

Note that infinite constant sequences can be lazily generated in Perl 6, or this wouldn't work so well...

A cleaner style is to combine these approaches with a more FP look:


```perl6
constant ζish = [\+] map -> \𝑖 { 1 / 𝑖**2 }, 1..*;
say ζish[1000];
```


Perhaps the cleanest way is to just define the zeta function and evaluate it for s=2, possibly using memoization:

```perl6
use experimental :cached;
sub ζ($s) is cached { [\+] 1..* X** -$s }
say ζ(2)[1000];
```


Notice how the thus-defined zeta function returns a lazy list of approximated values, which is arguably the closest we can get from the mathematical definition.


## Phix


```Phix
function sumto(atom n)
atom res = 0
    for i=1 to n do
        res += 1/(i*i)
    end for
    return res
end function
?sumto(1000)
```

{{out}}

```txt

1.643934567

```



## PHP


```PHP
<?php

/**
 * @author Elad Yosifon
 */

/**
 * @param int $n
 * @param int $k
 * @return float|int
 */
function sum_of_a_series($n,$k)
{
	$sum_of_a_series = 0;
	for($i=$k;$i<=$n;$i++)
	{
		$sum_of_a_series += (1/($i*$i));
	}
	return $sum_of_a_series;
}

echo sum_of_a_series(1000,1);

```

{{out}}

```txt
1.6439345666816
```



## PicoLisp


```PicoLisp
(scl 9)  # Calculate with 9 digits precision

(let S 0
   (for I 1000
      (inc 'S (*/ 1.0 (* I I))) )
   (prinl (round S 6)) )  # Round result to 6 digits
```

Output:

```txt
1.643935
```



## Pike


```Pike
array(int) x = enumerate(1000,1,1);
`+(@(1.0/pow(x[*],2)[*]));
Result: 1.64393
```



## PL/I


```pli
/* sum the first 1000 terms of the series 1/n**2. */
s = 0;

do i = 1000 to 1 by -1;
   s = s + 1/float(i**2);
end;

put skip list (s);
```


{{out}}

```txt

1.64393456668155980E+0000

```



## Pop11



```pop11
lvars s = 0, j;
for j from 1 to 1000 do
    s + 1.0/(j*j) -> s;
endfor;

s =>
```



## PostScript

<lang>
/aproxriemann{
/x exch def
/i 1 def
/sum 0 def
x{
/sum sum i -2 exp add def
/i i 1 add def
}repeat
sum ==
}def

1000 aproxriemann

```

Output:
<lang>
1.64393485

```


{{libheader|initlib}}

```postscript

% using map
[1 1000] 1 range {dup * 1 exch div} map 0 {+} fold

% just using fold
[1 1000] 1 range 0 {dup * 1 exch div +}  fold

```



## Potion


```potion
sum = 0.0
1 to 1000 (i): sum = sum + 1.0 / (i * i).
sum print
```



## PowerShell


```powershell
$x = 1..1000 `
       | ForEach-Object { 1 / ($_ * $_) } `
       | Measure-Object -Sum
Write-Host Sum = $x.Sum
```



## Prolog

Works with SWI-Prolog.

```Prolog
sum(S) :-
        findall(L, (between(1,1000,N),L is 1/N^2), Ls),
        sumlist(Ls, S).

```

Ouptput :

```txt
?- sum(S).
S = 1.643934566681562.

```



## PureBasic


```PureBasic
Define i, sum.d

For i=1 To 1000
  sum+1.0/(i*i)
Next i

Debug sum
```

<tt>
Answer = 1.6439345666815615
</tt>


## Python


```python
print ( sum(1.0 / (x * x) for x in range(1, 1001)) )
```


Or, as a generalised map, or fold / reduction – (see [[Catamorphism#Python]]):

```python
'''The sum of a series'''

from functools import reduce


# seriesSumA :: (a -> b) -> [a] -> b
def seriesSumA(f):
    '''The sum of the map of f over xs.'''
    return lambda xs: sum(map(f, xs))


# seriesSumB :: (a -> b) -> [a] -> b
def seriesSumB(f):
    '''Folding acc + f(x) over xs where acc begins at 0.'''
    return lambda xs: reduce(
        lambda a, x: a + f(x), xs, 0
    )


# TEST ----------------------------------------------------
# main:: IO ()
def main():
    '''Summing 1/x^2 over x = 1..1000'''

    def f(x):
        return 1 / (x * x)

    print(
        fTable(
            __doc__ + ':\n' + '(1/x^2 over x = 1..1000)'
        )(lambda f: '\tby ' + f.__name__)(str)(
            lambda g: g(f)(enumFromTo(1)(1000))
        )([seriesSumA, seriesSumB])
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# fTable :: String -> (a -> String) ->
#                     (b -> String) ->
#        (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + (
                ' -> '
            ) + fxShow(f(x)) for x in xs
        ])
    return lambda xShow: lambda fxShow: (
        lambda f: lambda xs: go(
            xShow, fxShow, f, xs
        )
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
The sum of a series:
(1/x^2 over x = 1..1000)
    by seriesSumA -> 1.6439345666815615
    by seriesSumB -> 1.6439345666815615
```



## Q


```q
sn:{sum xexp[;-2] 1+til x}
sn 1000
```


{{Out}}

```txt
1.643935
```



## R


```r
print( sum( 1/seq(1000)^2 ) )
```



## Racket


A solution using Typed Racket:


```racket

#lang typed/racket

(: S : Natural -> Real)
(define (S n)
  (for/sum: : Real ([k : Natural (in-range 1 (+ n 1))])
    (/ 1.0 (* k k))))

```



## Raven


```Raven
0 1 1000 1 range each 1.0 swap dup * / +
"%g\n" print
```

{{out}}

```txt
1.64393
```

Raven uses a 32 bit float, so precision limits the accuracy of the result for large iterations.

## Red


```Red
Red []
s: 0
repeat n 1000 [  s:   1.0 / n ** 2  + s  ]
print s

```


## REXX


### sums specific terms


```rexx
/*REXX program sums the first    N    terms of     1/(k**2),          k=1 ──►  N.       */
parse arg N D .                                  /*obtain optional arguments from the CL*/
if N=='' | N==","  then N=1000                   /*Not specified?  Then use the default.*/
if D=='' | D==","  then D=  60                   /* "      "         "   "   "     "    */
numeric digits D                                 /*use D digits (9 is the REXX default).*/
$=0                                              /*initialize the sum to zero.          */
          do k=1  for N                          /* [↓]  compute for   N   terms.       */
          $=$  +  1/k**2                         /*add a squared reciprocal to the sum. */
          end   /*k*/

say 'The sum of'     N     "terms is:"    $      /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default input:

```txt

The sum of 1000 terms is: 1.64393456668155980313905802382221558965210344649368531671713

```



### sums with running total

This REXX version shows the   ''running total''   for every 10<sup>th</sup> term.

```rexx
/*REXX program sums the first    N    terms o f    1/(k**2),          k=1 ──►  N.       */
parse arg N D .                                  /*obtain optional arguments from the CL*/
if N=='' | N==","  then N=1000                   /*Not specified?  Then use the default.*/
if D=='' | D==","  then D=  60                   /* "      "         "   "   "     "    */
numeric digits D                                 /*use D digits (9 is the REXX default).*/
w=length(N)                                      /*W   is used for aligning the output. */
$=0                                              /*initialize the sum to zero.          */
      do k=1  for N                              /* [↓]  compute for   N   terms.       */
      $=$  +  1/k**2                             /*add a squared reciprocal to the sum. */
      parse var k s 2 m '' -1 e                  /*obtain the start and end decimal digs*/
      if e\==0  then iterate                     /*does K  end  with the dec digit  0 ? */
      if s\==1  then iterate                     /*  "  " start   "   "   "    "    1 ? */
      if m\=0   then iterate                     /*  "  " middle  contain any non-zero ?*/
      if k==N   then iterate                     /*  "  " equal N, then skip running sum*/
      say  'The sum of'   right(k,w)     "terms is:"  $         /*display a running sum.*/
      end   /*k*/
say                                                             /*a blank line for sep. */
say        'The sum of'   right(k-1,w)   "terms is:"  $         /*display the final sum.*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   when using the input of:   <tt> 1000000000 </tt>

```txt

The sum of         10 terms is: 1.54976773116654069035021415973796926177878558830939783320736
The sum of        100 terms is: 1.63498390018489286507716949818032376668332170003126381385307
The sum of       1000 terms is: 1.64393456668155980313905802382221558965210344649368531671713
The sum of      10000 terms is: 1.64483407184805976980608183331031090353799751949684175308996
The sum of     100000 terms is: 1.64492406689822626980574850331269185564752132981156034248806
The sum of    1000000 terms is: 1.64493306684872643630574849997939185588561654406394129491321
The sum of   10000000 terms is: 1.64493396684823143647224849997935852288561656787346272343397
The sum of  100000000 terms is: 1.64493405684822648647241499997935852255228656787346510441026
The sum of 1000000000 terms is: 1.64493406584822643697241516647935852255228323457346510444171

```

'''output'''   from a calculator computing   <big><big><math>\pi</math></big><sup>2</sup>/6</big>,   (using 60 digits)   showing the correct number (nine) of decimal digits   [the superscripting of the digits was edited after-the-fact]:
<big>

 1.64493406<sup>684822643647241516664602518921894990120679843773556</sup>
</big>


### sums with running significance

This is a technique to show a   ''running significance''   (based on the previous calculation).

If the   '''old'''   REXX variable would be set to   '''1.64'''   (instead of   '''1'''), the first noise digits could be bypassed to make the display ''cleaner''.

```rexx
/*REXX program sums the first    N    terms of     1/(k**2),          k=1 ──►  N.       */
parse arg N D .                                  /*obtain optional arguments from the CL*/
if N=='' | N==","  then N=1000                   /*Not specified?  Then use the default.*/
if D=='' | D==","  then D=  60                   /* "      "         "   "   "     "    */
numeric digits D                                 /*use D digits (9 is the REXX default).*/
w=length(N)                                      /*W   is used for aligning the output. */
$=0                                              /*initialize the sum to zero.          */
old=1                                            /*the new sum to compared to the old.  */
p=0                                              /*significant decimal precision so far.*/
     do k=1  for N                               /* [↓]  compute for   N   terms.       */
     $=$  +  1/k**2                              /*add a squared reciprocal to the sum. */
     c=compare($,old)                            /*see how we're doing with precision.  */
     if c>p  then do                             /*Got another significant decimal dig? */
                  say 'The significant sum of'  right(k,w)      "terms is:"      left($,c)
                  p=c                            /*use the new significant precision.   */
                  end                            /* [↑]  display significant part of sum*/
     old=$                                       /*use "old" sum for the next compare.  */
     end   /*k*/
say                                              /*display blank line for the separator.*/
say 'The sum of'   right(N,w)    "terms is:"     /*display the  sum's  preamble line.   */
say $                                            /*stick a fork in it,  we're all done. */
```

'''output'''   when using the input of   (one billion [limit], and one hundred decimal digits):   <tt>   1000000000   100 </tt>

```txt

The significant sum of          3 terms is: 1.3
The significant sum of          5 terms is: 1.46
The significant sum of         14 terms is: 1.575
The significant sum of         34 terms is: 1.6159
The significant sum of        110 terms is: 1.63588
The significant sum of        328 terms is: 1.641889
The significant sum of       1024 terms is: 1.6439579
The significant sum of       3207 terms is: 1.64462229
The significant sum of      10043 terms is: 1.644834499
The significant sum of      31782 terms is: 1.6449026029
The significant sum of     100314 terms is: 1.64492409819
The significant sum of     316728 terms is: 1.644930909569
The significant sum of    1000853 terms is: 1.6449330677009
The significant sum of    3163463 terms is: 1.64493375073899
The significant sum of   10001199 terms is: 1.644933966860219
The significant sum of   31627592 terms is: 1.6449340352302649
The significant sum of  100009299 terms is: 1.64493405684915629
The significant sum of  316233759 terms is: 1.644934063686008709

The sum of 1000000000 terms is:
1.644934065848226436972415166479358522552283234573465104402224896012864613260343731009819376810240620

```

One can see a pattern in the number of significant digits computed based on the ''number of terms used''.   (See a discussion in the   ''talk''   section.)


## Ring


```Ring

sum = 0
for i =1 to 1000
    sum = sum + 1 /(pow(i,2))
next
decimals(8)
see sum

```



## RLaB


```RLaB

>> sum( (1 ./ [1:1000]) .^ 2 ) - const.pi^2/6
-0.000999500167

```



## Ruby


```ruby
puts (1..1000).inject{ |sum, x| sum + 1.0 / x ** 2 }
```

{{out}}

```txt

1.64393456668156

```



## Run BASIC


```runbasic

for i =1 to 1000
  sum = sum + 1 /( i^2)
next i
print sum
```



## Rust


```rust
const LOWER: i32 = 1;
const UPPER: i32 = 1000;

// Because the rule for our series is simply adding one, the number of terms are the number of
// digits between LOWER and UPPER
const NUMBER_OF_TERMS: i32 = (UPPER + 1) - LOWER;
fn main() {
    // Formulaic method
    println!("{}", (NUMBER_OF_TERMS * (LOWER + UPPER)) / 2);
    // Naive method
    println!("{}", (LOWER..UPPER + 1).fold(0, |sum, x| sum + x));
}

```



## SAS


```sas
data _null_;
s=0;
do n=1 to 1000;
   s+1/n**2;        /* s+x is synonym of s=s+x */
end;
e=s-constant('pi')**2/6;
put s e;
run;
```



## Scala


```scala>scala
 1 to 1000 map (x => 1.0 / (x * x)) sum
res30: Double = 1.6439345666815615
```



## Scheme


```scheme
(define (sum a b fn)
  (do ((i a (+ i 1))
       (result 0 (+ result (fn i))))
      ((> i b) result)))

(sum 1 1000 (lambda (x) (/ 1 (* x x)))) ; fraction
(exact->inexact (sum 1 1000 (lambda (x) (/ 1 (* x x))))) ; decimal
```


More idiomatic way (or so they say) by tail recursion:

```scheme
(define (invsq f to)
  (let loop ((f f) (s 0))
    (if (> f to)
      s
      (loop (+ 1 f) (+ s (/ 1 f f))))))

;; whether you get a rational or a float depends on implementation
(invsq 1 1000) ; 835459384831...766449/50820...90400000000
(exact->inexact (invsq 1 1000)) ; 1.64393456668156
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const func float: invsqr (in float: n) is
  return 1.0 / n**2;

const proc: main is func
  local
    var integer: i is 0;
    var float: sum is 0.0;
  begin
    for i range 1 to 1000 do
      sum +:= invsqr(flt(i));
    end for;
    writeln(sum digits 6 lpad 8);
  end func;
```



## Sidef


```ruby
say sum(1..1000, {|n| 1 / n**2 })
```


Alternatively, using the ''reduce{}'' method:

```ruby
say (1..1000 -> reduce { |a,b| a + (1 / b**2) })
```


{{out}}

```txt

1.64393456668155980313905802382221558965210344649369

```



## Slate


Manually coerce it to a float, otherwise you will get an exact (and slow) answer:


```slate
((1 to: 1000) reduce: [|:x :y | x + (y squared reciprocal as: Float)]).
```



## Smalltalk


```smalltalk
( (1 to: 1000) fold: [:sum :aNumber |
  sum + (aNumber squared reciprocal) ] ) asFloat displayNl.
```



## SQL


```SQL
create table t1 (n real);
-- this is postgresql specific, fill the table
insert into t1 (select generate_series(1,1000)::real);
with tt as (
  select 1/(n*n) as recip from t1
) select sum(recip) from tt;

```

Result of select (with locale DE):

```txt

       sum
------------------
 1.64393456668156
(1 Zeile)

```



## Stata


```stata
function series(n) {
	return(sum((n..1):^-2))
}

series(1000)-pi()^2/6
  -.0009995002
```



## Swift


```Swift

func sumSeries(var n: Int) -> Double {
    var ret: Double = 0

    for i in 1...n {
        ret += (1 / pow(Double(i), 2))
    }

    return ret
}

output: 1.64393456668156

```


<lang>
Swift also allows extension to datatypes.  Here's similar code using an extension to Int.

extension Int {
    func SumSeries() -> Double {
        var ret: Double = 0

        for i in 1...self {
           ret += (1 / pow(Double(i), 2))
        }

        return ret
    }
}

var x: Int = 1000
var y: Double

y = x.sumSeries()   /* y = 1.64393456668156 */

Swift also allows you to do this:

y = 1000.sumSeries()

```



## Tcl

{{works with|Tcl|8.5}}

```tcl
package require Tcl 8.5

proc partial_sum {func - start - stop} {
    for {set x $start; set sum 0} {$x <= $stop} {incr x} {
        set sum [expr {$sum + [apply $func $x]}]
    }
    return $sum
}

set S {x {expr {1.0 / $x**2}}}

partial_sum $S from 1 to 1000 ;# => 1.6439345666815615
```


{{tcllib|struct::list}}

```tcl
package require Tcl 8.5
package require struct::list

proc sum_of {lambda nums} {
    struct::list fold [struct::list map $nums [list apply $lambda]] 0 ::tcl::mathop::+
}

sum_of $S [range 1 1001] ;# ==> 1.6439345666815615
```


The helper <code>range</code> procedure is:

```tcl
# a range command akin to Python's
proc range args {
    foreach {start stop step} [switch -exact -- [llength $args] {
        1 {concat 0 $args 1}
        2 {concat   $args 1}
        3 {concat   $args  }
        default {error {wrong # of args: should be "range ?start? stop ?step?"}}
    }] break
    if {$step == 0} {error "cannot create a range when step == 0"}
    set range [list]
    while {$step > 0 ? $start < $stop : $stop < $start} {
        lappend range $start
        incr start $step
    }
    return $range
}
```


=={{header|TI-83 BASIC}}==
{{trans|TI-89 BASIC}}
{{works with|TI-83 BASIC|TI-84Plus 2.55MP}}

```ti83b

∑(1/X²,X,1,1000)

```

{{out}}

```txt

1.643934567

```


=={{header|TI-89 BASIC}}==


```ti89b
∑(1/x^2,x,1,1000)
```



## TXR


Reduce with + operator over a lazily generated list.

Variant A1: limit the list generation inside the <code>gen</code> operator.


```txr
txr -p '[reduce-left + (let ((i 0)) (gen (< i 1000) (/ 1.0 (* (inc i) i)))) 0]'
1.64393456668156
```


Variant A2: generate infinite list, but take only the first 1000 items using <code>[list-expr 0..999]</code>.


```txr
txr -p '[reduce-left + [(let ((i 0)) (gen t (/ 1.0 (* (inc i) i)))) 0..999] 0]'
1.64393456668156
```


Variant B: generate lazy integer range, and pump it through a series of function with the help of the <code>chain</code> functional combinator and the <code>op</code> partial evaluation/binding operator.


```txr
txr -p '[[chain range (op mapcar (op / 1.0 (* @1 @1))) (op reduce-left + @1 0)] 1 1000]'
1.64393456668156
```


Variant C: unravel the chain in Variant B using straightforward nesting.


```txr
txr -p '[reduce-left + (mapcar (op / 1.0 (* @1 @1)) (range 1 1000)) 0]'
1.64393456668156
```


Variant D: bring Variant B's inverse square calculation into the fold, eliminating mapcar. Final answer.


```txr
txr -p '[reduce-left (op + @1 (/ 1.0 (* @2 @2))) (range 1 1000) 0]'
1.64393456668156
```



## UnixPipes


```bash
term() {
   b=$1;res=$2
   echo "scale=5;1/($res*$res)+$b" | bc
}

sum() {
  (read B; res=$1;
  test -n "$B" && (term $B $res) || (term 0 $res))
}

fold() {
  func=$1
  (while read a ; do
      fold $func | $func $a
  done)
}

(echo 3; echo 1; echo 4) | fold sum
```



## Unicon

See [[#Icon|Icon]].


## Ursala

The expression plus:-0. represents a function returning the sum of
any given list of floating point numbers, or zero if it's empty,
using the built in reduction operator, :-, and the binary addition
function, plus. The rest the expression constructs the series
by inverting the square of each number in the list from 1 to 1000.

```Ursala
#import flo
#import nat

#cast %e

total = plus:-0 div/*1. sqr* float*t iota 1001
```

output:

```txt
1.643935e+00
```



## Vala


```vala

public static void main(){
	int i, start = 1, end = 1000;
	double sum = 0.0;

	for(i = start; i<= end; i++)
		sum += (1 / (double)(i * i));

	stdout.printf("%s\n", sum.to_string());
}

```


Output:

```txt

1.6439345666815615

```



## VBA


```vb
Private Function sumto(n As Integer) As Double
    Dim res As Double
    For i = 1 To n
        res = res + 1 / i ^ 2
    Next i
    sumto = res
End Function
Public Sub main()
    Debug.Print sumto(1000)
End Sub
```
{{out}}

```txt
 1,64393456668156
```


## VBScript


```vb
' Sum of a series
    for i=1 to 1000
        s=s+1/i^2
    next
    wscript.echo s
```

{{out}}

```txt

1.64393456668156

```



## Visual Basic .NET

{{trans|VBScript}}
{{works with|Visual Basic .NET|2013}}

```vbnet
' Sum of a series
    Sub SumOfaSeries()
        Dim s As Double
        s = 0
        For i = 1 To 1000
            s = s + 1 / i ^ 2
        Next 'i
        Console.WriteLine(s)
    End Sub
```

{{out}}

```txt

1.64393456668156

```



## WDTE


```WDTE>let s =
 import 'stream';

s.range 1 1001
-> s.map (@ inner k => / 1 (* k k))
-> s.reduce 0 +
-- io.writeln io.stdout
;
```


{{out}}

```txt
1.643933567
```



## Wortel


```wortel
@sum !*#~V1Sn @to 1000 ; returns 1.6439345666815615
```


```wortel
@to 1000 ; generates a list of 1 to 1000 (inclusive)
#~V1Sn ; number expression which stands for: square push(1) swap divide
!* ; maps the number expression over the list
@sum ; sums the list
```



## XPL0


```XPL0
code CrLf=9;  code real RlOut=48;
int  X;  real S;
[S:= 0.0;
for X:= 1 to 1000 do S:= S + 1.0/float(X*X);
RlOut(0, S);  CrLf(0);
]
```


Output:

```txt

    1.64393

```



## Yorick


```yorick
(1./indgen(1:1000)^2)(sum)
```



## zkl


```zkl
[1.0..1000].reduce(fcn(p,n){ p + 1.0/(n*n) },0.0)  //-->1.64394
```

