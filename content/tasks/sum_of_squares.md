+++
title = "Sum of squares"
description = ""
date = 2019-10-18T20:24:46Z
aliases = []
[extra]
id = 2411
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
+++

## Task

Write a program to find the sum of squares of a numeric vector.

The program should work on a zero-length vector (with an answer of   '''0''').


## Related tasks

*   [[Mean]]





## 0815


```0815

{x{*%<:d:~$<:1:~>><:2:~>><:3:~>><:4:~>><:5:~>><:6:~>><:7:
~>><:8:~>><:9:~>><:a:~>><:b:~>><:c:~>><:ffffffffffffffff:
~>{x{*>}:8f:{x{*&{=+>{~>&=x<:ffffffffffffffff:/#:8f:{{~%

```

```txt

0
28A

```



## 360 Assembly


```360asm
*        Sum of squares            27/08/2015
SUMOFSQR CSECT
         USING  SUMOFSQR,R12
         LR     R12,R15
         LA     R7,A               a(1)
         SR     R6,R6              sum=0
         LA     R3,1               i=1
LOOPI    CH     R3,N               do i=1 to hbound(a)
         BH     ELOOPI
         L      R5,0(R7)           a(i)
         M      R4,0(R7)           a(i)*a(i)
         AR     R6,R5              sum=sum+a(i)**2
         LA     R7,4(R7)           next a
         LA     R3,1(R3)           i=i+1
         B      LOOPI              end i
ELOOPI   XDECO  R6,PG+23           edit sum
         XPRNT  PG,80
         XR     R15,R15
         BR     R14
A        DC     F'1',F'2',F'3',F'4',F'5',F'6',F'7',F'8',F'9',F'10'
PG       DC     CL80'The sum of squares is: '
N        DC     AL2((PG-A)/4)
         YREGS
         END    SUMOFSQR
```

```txt

The sum of squares is:          385

```



## ACL2


```Lisp
(defun sum-of-squares (xs)
   (if (endp xs)
       0
       (+ (* (first xs) (first xs))
          (sum-of-squares (rest xs)))))
```



## ActionScript


```ActionScript
function sumOfSquares(vector:Vector.<Number>):Number
{
	var sum:Number = 0;
	for(var i:uint = 0; i < vector.length; i++)
		sum += vector[i]*vector[i];
	return sum;
}
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Sum_Of_Squares is
   type Float_Array is array (Integer range <>) of Float;

   function Sum_Of_Squares (X : Float_Array) return Float is
      Sum : Float := 0.0;
   begin
      for I in X'Range loop
         Sum := Sum + X (I) ** 2;
      end loop;
      return Sum;
   end Sum_Of_Squares;

begin
   Put_Line (Float'Image (Sum_Of_Squares ((1..0 => 1.0)))); -- Empty array
   Put_Line (Float'Image (Sum_Of_Squares ((3.0, 1.0, 4.0, 1.0, 5.0, 9.0))));
end Test_Sum_Of_Squares;
```

```txt

 0.00000E+00
 1.33000E+02

```


## Aime


```aime
real
squaredsum(list l)
{
    integer i;
    real s;

    s = 0;
    i = -~l;
    while (i) {
        s += sq(l[i += 1]);
    }

    s;
}

integer
main(void)
{
    list l;

    l = list(0, 1, 2, 3);

    o_form("~\n", squaredsum(l));
    o_form("~\n", squaredsum(list()));
    o_form("~\n", squaredsum(list(.5, -.5, 2)));

    0;
}
```

```txt
14
0
4.5
```



## ALGOL 68

The computation can be written as a loop.

```algol68
PROC sum of squares = ([]REAL argv)REAL:(
  REAL sum := 0;
  FOR i FROM LWB argv TO UPB argv DO
    sum +:= argv[i]**2
  OD;
  sum
);
test:(
  printf(($g(0)l$,sum of squares([]REAL(3, 1, 4, 1, 5, 9))));
)
```

```txt

133

```

Another implementation could define a procedure ('''proc''') or operator ('''op''') called '''map'''.

```algol68
[]REAL data = (3, 1, 4, 1, 5, 9);

PROC map = ( PROC(REAL)REAL func, []REAL argv)REAL:
    ( REAL out:=0; FOR i FROM LWB argv TO UPB argv DO out:=func(argv[i]) OD; out);

test:(
  REAL sum := 0;
  printf(($xg(0)l$, map ( ((REAL argv)REAL: sum +:= argv ** 2), data) ));

  PRIO MAP = 5; # the same priority as the operators <, =<, >=, & > maybe... #
  OP MAP = ( PROC(REAL)REAL func, []REAL argv)REAL:
    ( REAL out:=0; FOR i FROM LWB argv TO UPB argv DO out:=func(argv[i]) OD; out);

  sum := 0;
  printf(($g(0)l$, ((REAL argv)REAL: sum +:= argv ** 2) MAP data ))
)
```

```txt

133
133

```

The computation can be written as a generator.

```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

MODE YIELDREAL = PROC(REAL)VOID;
MODE GENREAL = PROC(YIELDREAL)VOID;

PROC gen real of vector = ([]REAL data, YIELDREAL yield)VOID:
  FOR i FROM LWB data TO UPB data DO yield(data[i]) OD;

PROC real sum sq of gen = (GENREAL gen real)REAL: (
  REAL sum:=0;
# FOR REAL value IN # gen real(#) DO (#
     (REAL value)VOID:(
       sum+:=value**2
# OD #));
  sum
);

PROC real sum map of gen = (PROC(REAL)REAL func, GENREAL gen real)REAL: (
  REAL sum:=0;
# FOR REAL value IN # gen real(#) DO (#
     (REAL value)VOID:(
       sum+:=func(value)
# OD #));
  sum
);

OP GEN = ([]REAL array)GENREAL:gen real of vector(array,);

OP (GENREAL #gen real#)REAL SUMSQ = real sum sq of gen;

PRIO SUMMAP = 5;
OP (PROC(REAL)REAL #func#, GENREAL #gen real#)REAL SUMMAP = real sum map of gen;

test:(
  []REAL data = (3, 1, 4, 1, 5, 9);
# Permutations of the above routines #
  printf(($"real sum sq GEN: "g(0)l$, real sum sq of gen(GEN data)));
  printf(($"real sum sq real gen: "g(0)l$, real sum sq of gen(gen real of vector(data,))));
  printf(($"real sum map real gen: "g(0)l$, real sum map of gen(((REAL x)REAL: x*x),gen real of vector(data,))));
  printf(($"SUMSQ real gen: "g(0)l$, SUMSQ gen real of vector(data,)));
  printf(($"SUMSQ GEN: "g(0)l$, SUMSQ GEN data));
  printf(($"sq SUMMAP GEN: "g(0)l$, ((REAL x)REAL: x*x)SUMMAP GEN data))
)
```

```txt

real sum sq GEN: 133
real sum sq real gen: 133
real sum map real gen: 133
SUMSQ real gen: 133
SUMSQ GEN: 133
sq SUMMAP GEN: 133

```



## ALGOL W


```algolw
begin
    % procedure to sum the elements of a vector. As the procedure can't find %
    % the bounds of the array for itself, we pass them in lb and ub          %
    real procedure sumSquares ( real    array vector ( * )
                              ; integer value lb
                              ; integer value ub
                              ) ;
    begin
        real sum;
        sum := 0;
        for i := lb until ub do sum := sum + ( vector( i ) * vector( i ) );
        sum
    end sumOfSquares ;

    % test the sumSquares procedure                                          %
    real array numbers ( 1 :: 5 );
    for i := 1 until 5 do numbers( i ) := i;
    r_format := "A"; r_w := 10; r_d := 1; % set fixed point output           %
    write( sumSquares( numbers, 1, 5 ) );
end.
```



## Alore



```Alore
def sum_squares(a)
   var sum = 0
   for i in a
      sum = sum + i**2
   end
   return sum
end

WriteLn(sum_squares([3,1,4,1,5,9]))
end
```



## APL


```apl
      square_sum←{+/⍵*2}
      square_sum 1 2 3 4 5
55
      square_sum ⍬ ⍝The empty vector
0
```



## AppleScript


Two ways of composing a sumOfSquares function:

```AppleScript
-- TWO APPROACHES – SUM OVER MAP, AND DIRECT FOLD ----------------------------

-- sumOfSquares :: Num a => [a] -> a
on sumOfSquares(xs)
    script squared
        on |λ|(x)
            x ^ 2
        end |λ|
    end script

    sum(map(squared, xs))

end sumOfSquares

-- sumOfSquares2 :: Num a => [a] -> a
on sumOfSquares2(xs)
    script plusSquare
        on |λ|(a, x)
            a + x ^ 2
        end |λ|
    end script

    foldl(plusSquare, 0, xs)

end sumOfSquares2


-- TEST ----------------------------------------------------------------------
on run
    set xs to [3, 1, 4, 1, 5, 9]

    {sumOfSquares(xs), sumOfSquares2(xs)}

    -- {133.0, 133.0}
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

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

-- sum :: Num a => [a] -> a
on sum(xs)
    script add
        on |λ|(a, b)
            a + b
        end |λ|
    end script

    foldl(add, 0, xs)
end sum
```

```AppleScript
{133.0, 133.0}
```



## Arturo


```arturo
arr $(range 1 10)

print $(sum $(map arr { &^2 }))
```


```txt
385
```



## Astro


```python
sum([1, 2, 3, 4]²)
```



## AutoHotkey


```autohotkey
list = 3 1 4 1 5 9
Loop, Parse, list, %A_Space%
 sum += A_LoopField**2
MsgBox,% sum
```



## AWK

Vectors are read, space-separated, from stdin; sum of squares goes to stdout. The empty line produces 0.

```awk
$ awk '{s=0;for(i=1;i<=NF;i++)s+=$i*$i;print s}'
3 1 4 1 5 9
133

0
```



## BASIC

Assume the numbers are in an array called <code>a</code>.

```qbasic
sum = 0
FOR I = LBOUND(a) TO UBOUND(a)
  sum = sum + a(I) ^ 2
NEXT I
PRINT "The sum of squares is: " + sum
```


=
## BaCon
=

```freebasic
' Sum of squares
FUNCTION ss(int arr[], NUMBER elem)
    sum = 0
    FOR i = 0 TO elem - 1
        sum = sum + POW(arr[i], 2)
    NEXT
    RETURN sum
END FUNCTION

' 1 to 10 in the test vector, or 1 to -s n
option = CMDLINE("s:")
IF option = ASC("s") THEN
    elem = VAL(ARGUMENT$)
ELSE
    elem = 10
END IF

DECLARE vector TYPE int ARRAY elem
FOR i = 0 TO elem - 1
    vector[i] = i + 1
NEXT
PRINT ss(vector, elem)
```


```txt
prompt$ ./sumsquares
385
prompt$ ./sumsquares -s 1000
333833500
```


=
## BBC BASIC
=
BBC BASIC cannot have a zero-length array.

```bbcbasic
      DIM vector(5)
      vector() = 1, 2, 3, 4, 5, 6

      PRINT "Sum of squares = " ; MOD(vector()) ^ 2
```

```txt
Sum of squares = 91
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 INPUT PROMPT "Number of elements: ":N
110 NUMERIC A(1 TO N)
120 FOR I=1 TO N
130   PRINT I;:INPUT PROMPT ". = ":A(I)
140 NEXT
150 PRINT "The sum of squares is:";SQ(A)
160 DEF SQ(REF T)
170   LET S=0
180   FOR I=LBOUND(T) TO UBOUND(T)
190     LET S=S+T(I)^2
200   NEXT
210   LET SQ=S
220 END DEF
```



## bc


```bc
define s(a[], n) {
    auto i, s

    for (i = 0; i < n; i++) {
        s += a[i] * a[i]
    }

    return(s)
}
```



## Bracmat


```bracmat
( ( sumOfSquares
  =   sum component
    .   0:?sum
      &   whl
        ' ( !arg:%?component ?arg
          & !component^2+!sum:?sum
          )
      & !sum
  )
& out$(sumOfSquares$(3 4))
& out$(sumOfSquares$(3 4 i*5))
& out$(sumOfSquares$(a b c))
);
```

```txt
25
0
a^2+b^2+c^2
```



## Brat


```brat
p 1.to(10).reduce 0 { res, n | res = res + n ^ 2 }  #Prints 385
```



## C



```c
#include <stdio.h>

double squaredsum(double *l, int e)
{
   int i; double sum = 0.0;
   for(i = 0 ; i < e ; i++) sum += l[i]*l[i];
   return sum;
}

int main()
{
   double list[6] = {3.0, 1.0, 4.0, 1.0, 5.0, 9.0};

   printf("%lf\n", squaredsum(list, 6));
   printf("%lf\n", squaredsum(list, 0));
   /* the same without using a real list as if it were 0-element long */
   printf("%lf\n", squaredsum(NULL, 0));
   return 0;
}
```



## C++


```cpp
#include <iostream>
#include <numeric>
#include <vector>

double add_square(double prev_sum, double new_val)
{
  return prev_sum + new_val*new_val;
}

double vec_add_squares(std::vector<double>& v)
{
  return std::accumulate(v.begin(), v.end(), 0.0, add_square);
}

int main()
{
  // first, show that for empty vectors we indeed get 0
  std::vector<double> v; // empty
  std::cout << vec_add_squares(v) << std::endl;

  // now, use some values
  double data[] = { 0, 1, 3, 1.5, 42, 0.1, -4 };
  v.assign(data, data+7);
  std::cout << vec_add_squares(v) << std::endl;
  return 0;
}
```


Alternative version using {{libheader|Boost.Lambda}}:

```cpp
#include <numeric>
#include <vector>
#include "boost/lambda/lambda.hpp"

double vec_add_squares(std::vector<double>& v)
{
  using namespace boost::lambda;

  return std::accumulate(v.begin(), v.end(), 0.0, _1 + _2 * _2);
}
```


## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static int SumOfSquares(IEnumerable<int> list)
    {
        return list.Sum(x => x * x);
    }
    static void Main(string[] args)
    {
        Console.WriteLine(SumOfSquares(new int[] { 4, 8, 15, 16, 23, 42 })); // 2854
        Console.WriteLine(SumOfSquares(new int[] { 1, 2, 3, 4, 5 })); // 55
        Console.WriteLine(SumOfSquares(new int[] { })); // 0
    }
}
```



## Chef


```Chef
Sum of squares.

First input is length of vector, then rest of input is vector.

Ingredients.
1 g eggs
0 g bacon

Method.
Put bacon into the 1st mixing bowl.
Take eggs from refrigerator.
Square the eggs.
Take bacon from refrigerator.
Put bacon into 2nd mixing bowl.
Combine bacon into 2nd mixing bowl.
Fold bacon into 2nd mixing bowl.
Add the bacon into the 1st mixing bowl.
Ask the eggs until squared.
Pour contents of the 1st mixing bowl into the 1st baking dish.

Serves 1.


```



## Clojure


```clojure
(defn sum-of-squares [v]
  (reduce #(+ %1 (* %2 %2)) 0 v))
```



## CoffeeScript


```coffeescript

sumOfSquares = ( list ) ->
    list.reduce (( sum, x ) -> sum + ( x * x )), 0

```



## Common Lisp



```lisp
(defun sum-of-squares (vector)
  (loop for x across vector sum (expt x 2)))
```



## Crystal



```Ruby

def sum_squares(a)
    a.map{|e| e*e}.sum()
end

puts sum_squares([1, 2, 3])
# => 14

```



## D


### Iterative Version


```d
T sumSquares(T)(T[] a) pure nothrow @safe @nogc {
    T sum = 0;
    foreach (e; a)
        sum += e ^^ 2;
    return sum;
}

void main() {
    import std.stdio: writeln;

    [3.1, 1.0, 4.0, 1.0, 5.0, 9.0].sumSquares.writeln;
}
```

```txt
133.61
```



### Polymorphic Functional Style


```d
import std.stdio, std.algorithm, std.traits, std.range;

auto sumSquares(Range)(Range data) pure nothrow @safe @nogc {
    return reduce!q{a + b ^^ 2}(ForeachType!Range(0), data);
}

void main() {
    immutable items = [3.1, 1.0, 4.0, 1.0, 5.0, 9.0];
    items.sumSquares.writeln;
    10.iota.sumSquares.writeln;
}
```

```txt
133.61
285
```



## Dart


### Iterative Version


```d
sumOfSquares(list) {
  var sum=0;
  list.forEach((var n) { sum+=(n*n); });
  return sum;
}

main() {
  print(sumOfSquares([]));
  print(sumOfSquares([1,2,3]));
  print(sumOfSquares([10]));
}
```

```txt
0
14
100
```



### Functional Style Version


```d
num sumOfSquares(List<num> l) => l.map((num x)=>x*x)
				  .fold(0, (num p,num n)=> p + n);

void main(){
  print(sumOfSquares([]));
  print(sumOfSquares([1,2,3]));
  print(sumOfSquares([10]));
}
```

```txt
0
14
100
```



## Delphi

Delphi has standard SumOfSquares function in Math unit:

```Delphi
program SumOfSq;

{$APPTYPE CONSOLE}

uses Math;

type
  TDblArray = array of Double;

var
  A: TDblArray;

begin
  Writeln(SumOfSquares([]):6:2);            //  0.00
  Writeln(SumOfSquares([1, 2, 3, 4]):6:2);  // 30.00
  A:= nil;
  Writeln(SumOfSquares(A):6:2);             //  0.00
  A:= TDblArray.Create(1, 2, 3, 4);
  Writeln(SumOfSquares(A):6:2);             // 30.00
  Readln;
end.
```



## E



```e
def sumOfSquares(numbers) {
    var sum := 0
    for x in numbers {
        sum += x**2
    }
    return sum
}
```




## Eiffel



```Eiffel

class
	APPLICATION

create
	make

feature -- Initialization

	make
		local
			a: ARRAY [INTEGER]
		do
			a := <<1, -2, 3>>
			print ("%NSquare sum of <<1, 2, 3>>: " + sum_of_square (a).out)

			a := <<>>
			print ("%NSquare sum of <<>>: " + sum_of_square (a).out)
		end

feature -- Access

	sum_of_square (a: ITERABLE [INTEGER]): NATURAL
			-- sum of square of each items
		do
			Result := 0
			across a as it loop
				Result := Result + (it.item * it.item).as_natural_32
			end
		end

end

```


## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

SumOfSquares(list)
    = list.selectBy:(x => x * x).summarize(new Integer());

public program()
{
    console
        .printLine(SumOfSquares(new int[]::(4, 8, 15, 16, 23, 42)))
        .printLine(SumOfSquares(new int[]::(1, 2, 3, 4, 5)))
        .printLine(SumOfSquares(Array.MinValue))
}
```

```txt

2854
55
0

```



## Elixir


```elixir
iex(1)> Enum.reduce([3,1,4,1,5,9], 0, fn x,sum -> sum + x*x end)
133
```



## Emacs Lisp


```Emacs Lisp

(defun sum-square (ls)
  (apply '+ (mapcar (lambda (k) (* k k) ) ls) ))

(insert (format "%d"(sum-square (number-sequence 0 3) )))

```

<b>Output:</b>

```txt

14

```



## Erlang


```erlang
lists:foldl(fun(X, Sum) -> X*X + Sum end, 0, [3,1,4,1,5,9]).
```



## Euphoria


```euphoria
function SumOfSquares(sequence v)
    atom sum
    sum = 0
    for i = 1 to length(v) do
        sum += v[i]*v[i]
    end for
    return sum
end function
```





## Excel

To find the sum of squares of values from A1 to A10, type in any other cell :


```Excel

=SUMSQ(A1:A10)

```


The above expression will return zero if there are no values in any cell.

<lang>
12	3	5	23	13	67	15	9	4	2

5691

```



## Factor


```factor
USE: math sequences ;

: sum-of-squares ( seq -- n ) [ sq ] map-sum ;

{ 1.0 2.0 4.0 8.0 16.0 } sum-of-squares
```



## FALSE


```FALSE

0 3 1 4 1 5 9$*\ [$0=~][$*+\]#%.

```



## Fantom


```fantom

class SumSquares
{
  static Int sumSquares (Int[] numbers)
  {
    Int sum := 0
    numbers.each |n| { sum += n * n }
    return sum
  }

  public static Void main ()
  {
    Int[] n := [,]
    echo ("Sum of squares of $n = ${sumSquares(n)}")
    n = [1,2,3,4,5]
    echo ("Sum of squares of $n = ${sumSquares(n)}")
  }
}

```



## Fish


```Fish
v
\0&
>l?!v:*&+&
    >&n;
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Sum_of_squares this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: fsum**2 ( addr n -- f )
  0e
  dup 0= if 2drop exit then
  floats bounds do
    i f@ fdup f* f+
  1 floats +loop ;

create test 3e f, 1e f, 4e f, 1e f, 5e f, 9e f,
test 6 fsum**2 f.     \ 133.
```



## Fortran

In ISO Fortran 90 orlater, use SUM intrinsic and implicit element-wise array arithmetic:

```fortran
real, dimension(1000) :: a = (/ (i, i=1, 1000) /)
real, pointer, dimension(:) :: p => a(2:1)       ! pointer to zero-length array
real :: result, zresult

result = sum(a*a)    ! Multiply array by itself to get squares

result = sum(a**2)   ! Use exponentiation operator to get squares

zresult = sum(p*p)   ! P is zero-length; P*P is valid zero-length array expression; SUM(P*P) == 0.0 as expected
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function SumSquares(a() As Double) As Double
  Dim As Integer length = UBound(a) - LBound(a) + 1
  If length = 0 Then Return 0.0
  Dim As Double sum = 0.0
  For i As Integer = LBound(a) To UBound(a)
    sum += a(i) * a(i)
  Next
  Return sum
End Function

Dim a(5) As Double = {1.0, 2.0, 3.0, -1.0, -2.0, -3.0}
Dim sum As Double = SumSquares(a())
Print "The sum of the squares is"; sum
Print
Print "Press any key to quit"
Sleep
```


```txt

The sum of the squares is 28

```



## Frink


```frink

f = {|x| x^2}   // Anonymous function which squares its argument
a = [1,2,3,5,7]
println[sum[map[f,a], 0]]

```


=={{header|F_Sharp|F#}}==

```fsharp
[1 .. 10] |> List.fold (fun a x -> a + x * x) 0
[|1 .. 10|] |> Array.fold (fun a x -> a + x * x) 0
```


## GAP


```gap
# Just multiplying a vector by itself yields the sum of squares (it's an inner product)
# It's necessary to check for the empty vector though
SumSq := function(v)
	if Size(v) = 0 then
		return 0;
	else
		return v*v;
	fi;
end;
```



## GEORGE


```GEORGE
read (n) print ;
0
1, n rep (i)
   read print dup mult +
   ]
print
```

data

```txt

11
 8
 12
 15
 6
 25
 19
 33
 27
 3
 37
 4

```

results:

```txt

 1.100000000000000E+0001  << number of values (11)
 8.000000000000000        << 11 data
 1.200000000000000E+0001
 1.500000000000000E+0001
 6.000000000000000
 2.500000000000000E+0001
 1.900000000000000E+0001
 3.300000000000000E+0001
 2.700000000000000E+0001
 3.000000000000000
 3.700000000000000E+0001
 4.000000000000000
 4.667000000000000E+0003  << sum of squares

```



## Go

;Implementation

```go
package main

import "fmt"

var v = []float32{1, 2, .5}

func main() {
    var sum float32
    for _, x := range v {
        sum += x * x
    }
    fmt.Println(sum)
}
```

```txt

5.25

```

;Library

```go
package main

import (
    "fmt"

    "github.com/gonum/floats"
)

var v = []float64{1, 2, .5}

func main() {
    fmt.Println(floats.Dot(v, v))
}
```

```txt

5.25

```



## Golfscript


```golfscript
{0\{.*+}%}:sqsum;
# usage example
[1 2 3]sqsum puts
```



## Groovy


```groovy
def array = 1..3

// square via multiplication
def sumSq = array.collect { it * it }.sum()
println sumSq

// square via exponentiation
sumSq = array.collect { it ** 2 }.sum()

println sumSq
```


```txt
14
14
```



## Haskell

Three approaches:

```haskell
main :: IO ()
main =
  mapM_ print $
  [ sum . fmap (^ 2)      -- ver 1
  , sum . ((^ 2) <$>)     -- ver 2
  , foldr ((+) . (^ 2)) 0 -- ver 3
  ] <*>
  [[3, 1, 4, 1, 5, 9], [1 .. 6], [], [1]]
```

```txt
133
91
0
1
133
91
0
1
133
91
0
1
```



## IDL



```idl
print,total(array^2)
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
   local lst
   lst := []
   #Construct a simple list and pass it to getsum
   every put(lst,seq()\2)
   write(getsum(lst))
end

procedure getsum(lst)
   local total
   total := 0
   every total +:= !lst ^ 2
   return total
end
```



## Inform 7


```inform7
Sum Of Squares is a room.

To decide which number is the sum of (N - number) and (M - number) (this is summing):
	decide on N + M.

To decide which number is (N - number) squared (this is squaring):
	decide on N * N.

To decide which number is the sum of squares of (L - list of numbers):
	decide on the summing reduction of squaring applied to L.

When play begins:
	say the sum of squares of {};
	say line break;
	say the sum of squares of {1, 2, 3};
	end the story.
```



## Io


```io
list(3,1,4,1,5,9) map(squared) sum
```



## J



```j
ss=: +/ @: *:
```


That is, sum composed with square.  The verb also works on higher-ranked arrays.  For example:


```j
   ss 3 1 4 1 5 9
133
   ss $0           NB. $0 is a zero-length vector
0
   x=: 20 4 ?@$ 0  NB. a 20-by-4 table of random (0,1) numbers
   ss x
9.09516 5.19512 5.84173 6.6916
```


The computation can also be written as a loop.  It is shown here for comparison only and
is highly non-preferred compared to the version above.


```j
ss1=: 3 : 0
 z=. 0
 for_i. i.#y do. z=. z+*:i{y end.
)

   ss1 3 1 4 1 5 9
133
   ss1 $0
0
   ss1 x
9.09516 5.19512 5.84173 6.6916
```



## Java

```java5
public class SumSquares
{
 public static void main(final String[] args)
 {
  double sum = 0;
  int[] nums = {1,2,3,4,5};
  for (int i : nums)
   sum += i * i;
  System.out.println("The sum of the squares is: " + sum);
 }
}
```



## JavaScript


### ES5


```javascript
function sumsq(array) {
  var sum = 0;
  var i, iLen;

  for (i = 0, iLen = array.length; i < iLen; i++) {
    sum += array[i] * array[i];
  }
  return sum;
}

alert(sumsq([1,2,3,4,5]));  // 55
```


An alternative using a while loop and Math.pow


```javascript
function sumsq(array) {
  var sum = 0,
      i = array.length;

  while (i--) sum += Math.pow(array[i], 2);

  return sum;
}

alert(sumsq([1,2,3,4,5])); // 55
```



```javascript
Functional.reduce("x+y*y", 0, [1,2,3,4,5])
```


map (JS 1.6) and reduce (JS 1.8)


```javascript
[3,1,4,1,5,9].map(function (n) { return Math.pow(n,2); }).reduce(function (sum,n) { return sum+n; });
```



### ES6


Two ways of composing a sumOfSquares function

```JavaScript
(() => {
    'use strict';

    // squared :: Num a => a -> a
    const squared = x => Math.pow(x, 2);

    // sum :: (Num a) => [a] -> a
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // sumOfSquares :: Num a => [a] -> a
    const sumOfSquares = xs => sum(xs.map(squared));

    // sumOfSquares2 :: Num a => [a] -> a
    const sumOfSquares2 = xs =>
        xs.reduce((a, x) => a + squared(x), 0);

    return [sumOfSquares, sumOfSquares2]
        .map(f => f([3, 1, 4, 1, 5, 9]))
        .join('\n');
})();
```


```txt
133
133
```



## jq

jq supports both arrays and streams, and so we illustrate how to handle both.

```jq
# ss for an input array:
def ss: map(.*.) | add;

# ss for a stream, S, without creating an intermediate array:
def ss(S): reduce S as $x (0; . + ($x * $x) );
```
We can also use a generic "SIGMA" filter that behaves like the mathematical SIGMA:
```jq

# SIGMA(exp) computes the sum of exp over the input array:
def SIGMA(exp): map(exp) | add;

# SIGMA(exp; S) computes the sum of exp over elements of the stream, S,
# without creating an intermediate array:
def SIGMA(exp; S): reduce (S|exp) as $x (0; . + $x);
```

Finally, a "mapreduce" filter:
```jq

def mapreduce(mapper; reducer; zero):
  if length == 0 then zero
  else map(mapper) | reducer
  end;

```

Demonstration:
```jq
def demo(n):
  "ss:           \( [range(0;n)] | ss )",
  "ss(S):        \( ss( range(0;n) ) )",
  "SIGMA(.*.):   \( [range(0;n)] | SIGMA(.*.) )",
  "SIGMA(.*.;S): \( SIGMA( .*.; range(0;n) ) )",
  "mapreduce(.*.; add; 0): \( [range(0;n)] | mapreduce(.*.; add; 0) )"
;

demo(3) # 0^2 + 1^2 + 2^2
```

```jq
"ss:           5"
"ss(S):        5"
"SIGMA(.*.):   5"
"SIGMA(.*.;S): 5"
"mapreduce(.*.; add; 0): 5"
```



## Julia

There are several easy ways to do this in Julia:

```julia>julia
 sum([1,2,3,4,5].^2)
55

julia> sum([x^2 for x in [1,2,3,4,5]])
55

julia> mapreduce(x->x^2,+,[1:5])
55

julia> sum([x^2 for x in []])
0
```



## K


```k

  ss: {+/x*x}
  ss 1 2 3 4 5
55
  ss@!0
0

```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val vector = doubleArrayOf(3.1, 1.0, 4.0, 1.0, 5.0, 9.0)
    println(vector.map { it * it }.sum())
    val vector2 = doubleArrayOf() // empty vector
    println(vector2.map { it * it }.sum())
}
```


```txt

133.61
0.0

```



## Lang5


```lang5
[1 2 3 4 5] 2 ** '+ reduce .
```




## Lasso


```Lasso
define sumofsquares(values::array) => {

	local(sum = 0)

	with value in #values do {
		#sum += #value * #value
	}

	return #sum
}

sumofsquares(array(1,2,3,4,5))
```

```txt
55
```



## LFE


```lisp

(defun sum-sq (nums)
  (lists:foldl
    (lambda (x acc)
      (+ acc (* x x)))
    0 nums))

```


Usage:

```lisp

> (sum-sq '(3 1 4 1 5 9))
133

```



## Liberty BASIC


```lb
'   [RC] Sum of Squares

    SourceList$     ="3 1 4 1 5 9"
    'SourceList$     =""

    '   If saved as an array we'd have to have a flag for last data.
    '   LB has the very useful word$() to read from delimited strings.
    '   The default delimiter is a space character, " ".

    SumOfSquares    =0
    n               =0
    data$           ="666"  '   temporary dummy to enter the loop.

    while data$ <>""                                '   we loop until no data left.
        data$           =word$( SourceList$, n +1)  '   first data, as a string
        NewVal          =val( data$)                '   convert string to number
        SumOfSquares    =SumOfSquares +NewVal^2     '   add to existing sum of squares
        n =n +1                                     '   increment number of data items found
    wend

    n =n -1

    print "Supplied data was ";         SourceList$
    print "This contained ";            n; " numbers."
    print "Sum of squares is ";         SumOfSquares

    end
```



## LiveCode


```LiveCode
put "1,2,3,4,5" into nums
repeat for each item n in nums
    add (n * n) to m
end repeat
put m  // 55
```



## Logo


```logo
print apply "sum map [? * ?] [1 2 3 4 5]  ; 55
```



## Logtalk


```logtalk
sum(List, Sum) :-
    sum(List, 0, Sum).

sum([], Sum, Sum).
sum([X| Xs], Acc, Sum) :-
    Acc2 is Acc + X,
    sum(Xs, Acc2, Sum).
```



## Lua


```lua
function squaresum(a, ...) return a and a^2 + squaresum(...) or 0 end
function squaresumt(t) return squaresum(unpack(t)) end

print(squaresumt{3, 5, 4, 1, 7})
```



## M2000 Interpreter

M2000 use two concepts for arrays: standard array like A() and pointer to array as A. Pointer arithmetic not allowed here. Standard arrays are values types, and pointers are reference types. So we can handle an array both with pointer and without.


```M2000 Interpreter

Dim A() 'make an array with zero items

A=(,) 'make a pointer to array with zero items

A=(1,) 'make a pointer to array with one item

A()=A 'make a copy of array pointed by A to A()

A=A() 'make A a pointer for A()

Dim A(10)=1 'redim A() and pass 1 to each item

k=lambda m=1->{=m:m++}  ' a lambda function with a closure m

Dim B(10)<<k()    'fill B() from 1 to 10

A()=B() ' copy B() to A(), A() object stay as is, but new items loaded, so pointer A points to A.

A+=100 ' add 100 to each element of A()

A(0)+=100 ' add 100 to first element

A()=Cons(A,A)

Now A and A() prints a 20 item array (Cons() add a list of arrays)
Print A   ' or Print A() print the same

```


And this is the task, using a lambda function (we can use a standard function, just use Function Square { code here })

Because M2000 modules and functions use stack for passing values, we use read statement to read a value. Functions in expressions has no return to stack because they have own stack, so passing values are filled in a fresh stack in every call. This not hold if we call function using Call (as a module), so stack is passed from parent (caller).

When we pass an array in stack, a pointer to array (to one of two interfaces) and depends the name type of a read to make this a copy or a pointer to array. So here we use: read a as a pointer to array (so it is a by reference pass). We can use Read a() and then a=a() (and remove Link a to a()), so we use by value pass, and that is a decision from callee, not the caller (this happen for objects)


```M2000 Interpreter

Module Checkit {
      A=(1,2,3,4,5)
      Square=lambda -> {
            read a
            if len(a)=0 then =0: exit
            link a to a()
            \\ make sum same type as a(0)
            sum=a(0)-a(0)
            for i=0 to len(a)-1 {sum+=a(i)*a(i)}
            =sum
      }
      Print Square(a)=55
      Print Square((,))=0 ' empty  array
      Dim k(10)=2, L()
      Print Square(K())=40
      Print Square(L())=0
      A=(1@,2@,3@,4@,5@)
      X=Square(A)
      Print Type$(X)="Decimal", X=55@
}
Checkit

```



## Maple


```Maple

F := V -> add(v^2, v in V):
F(<1,2,3,4,5>);

```



## Mathematica

As a function 1:

```mathematica
SumOfSquares[x_]:=Total[x^2]
SumOfSquares[{1,2,3,4,5}]
```

As a function 2:

```mathematica
SumOfSquares[x_]:=x.x
SumOfSquares[{1,2,3,4,5}]
```

Pure function 1: (postfix operator in the following examples)

```mathematica
{1,2,3,4,5} // Total[#^2] &
```

Pure function 2:

```mathematica
{1, 2, 3, 4, 5} // #^2 & // Total
```

Pure function 3:

```mathematica
{1, 2, 3, 4, 5} // #.#&
```



## MATLAB


```Matlab
function [squaredSum] = sumofsquares(inputVector)
   squaredSum = sum( inputVector.^2 );
```



## Maxima


```maxima
nums : [3,1,4,1,5,9];
sum(nums[i]^2,i,1,length(nums));
```

or

```maxima
nums : [3,1,4,1,5,9];
lsum(el^2, el, nums);
```



## Mercury

<lang>
:- module sum_of_squares.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

main(!IO) :-
    io.write_int(sum_of_squares([3, 1, 4, 1, 5, 9]), !IO),
    io.nl(!IO).

:- func sum_of_squares(list(int)) = int.

sum_of_squares(Ns) = list.foldl((func(N, Acc) = Acc + N * N), Ns, 0).

```



## min

```min
((bool) ((dup *) (+) map-reduce) (pop 0) if) :sq-sum

(1 2 3 4 5) sq-sum puts
() sq-sum puts
```

```txt

55
0

```



## MiniScript


```MiniScript
sumOfSquares = function(seq)
    sum = 0
    for item in seq
        sum = sum + item*item
    end for
    return sum
end function

print sumOfSquares([4, 8, 15, 16, 23, 42])
print sumOfSquares([1, 2, 3, 4, 5])
print sumOfSquares([])
```

```txt
2854
55
0
```


=={{header|MK-61/52}}==
<lang>x^2	+	С/П	БП	00
```


=={{header|Modula-3}}==

```modula3
MODULE SumSquares EXPORTS Main;

IMPORT IO, Fmt;

TYPE RealArray = ARRAY OF REAL;

PROCEDURE SumOfSquares(x: RealArray): REAL =
  VAR sum := 0.0;
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      sum := sum + x[i] * x[i];
    END;
    RETURN sum;
  END SumOfSquares;

BEGIN
  IO.Put(Fmt.Real(SumOfSquares(RealArray{3.0, 1.0, 4.0, 1.0, 5.0, 9.0})));
  IO.Put("\n");
END SumSquares.
```



## MOO


```moo
@verb #100:sum_squares this none this rd
@program #100:sum_squares
sum = 0;
list = args[1];
for i in (list)
  sum = sum + (i^2);
endfor
player:tell(toliteral(list), " => ", sum);
.

;#100:sum_squares({3,1,4,1,5,9})
{3, 1, 4, 1, 5, 9} => 133
;#100:sum_squares({})
{} => 0

```



## MUMPS


```MUMPS
SUMSQUARE(X)
 ;X is assumed to be a list of numbers separated by "^"
 NEW RESULT,I
 SET RESULT=0,I=1
 FOR  QUIT:(I>$LENGTH(X,"^"))  SET RESULT=($PIECE(X,"^",I)*$PIECE(X,"^",I))+RESULT,I=I+1
 QUIT RESULT
```



## Nemerle


```Nemerle
SS(x : list[double]) : double
{
    |[] => 0.0
    |_  => x.Map(fun (x) {x*x}).FoldLeft(0.0, _+_)
}
```



## NetRexx


```netrexx
/*NetRexx *************************************************************
* program to sum the squares of a vector of fifteen numbers.
* translated from REXX
* 14.05.2013 Walter Pachl
**********************************************************************/
numeric digits 50                   /*allow 50-digit # (default is 9)*/
v='-100 9 8 7 6 0 3 4 5 2 1 .5 10 11 12' /* vector with some #s.     */
n=v.words()
x=''
sum=0                               /*initialize   SUM   to zero.    */
                                    /*if vector is empty, sum = zero.*/
loop Until x=''                     /*loop until list is exhausted   */
  Parse v x v                       /* pick next number              */
  If x>'' Then                      /* there is a number             */
    sum=sum + x**2                  /*add its square to the sum.     */
  end
say "The sum of" n "elements for the V vector is:" sum
```

```txt
The sum of 15 elements for the V vector is: 10650.25
```



## NewLISP


```NewLISP
(apply + (map (fn(x) (* x x)) '(3 1 4 1 5 9)))
-> 133
(apply + (map (fn(x) (* x x)) '()))
-> 0
```



## Nim


```nim
import math, sequtils

echo sum(map(@[1,2,3,4,5], proc (x: int): int = x*x))
```



## Objeck


```objeck

bundle Default {
  class Sum {
    function : native : SquaredSum(values : Float[]) ~ Float {
       sum := 0.0;
       for(i := 0 ; i < values->Size()	; i += 1;) {
         sum += (values[i] * values[i]);
       };

       return sum;
    }

    function : Main(args : String[]) ~ Nil {
       SquaredSum([3.0, 1.0, 4.0, 1.0, 5.0, 9.0])->PrintLine();
     }
  }
}

```



## OCaml



```ocaml
List.fold_left (fun sum a -> sum + a * a) 0 ints
```



```ocaml
List.fold_left (fun sum a -> sum +. a *. a) 0. floats
```



## Oforth



```Oforth
#sq [1, 1.2, 3, 4.5 ] map sum
```



## Octave



```octave
a = [1:10];
sumsq = sum(a .^ 2);
```



## Ol


```scheme

(define (sum-of-squares l)
   (fold + 0 (map * l l)))

(print (sum-of-squares '(1 2 3 4 5 6 7 8 9 10)))
; ==> 385

```



## Order



```c
#include <order/interpreter.h>

ORDER_PP(8to_lit(
  8seq_fold(8plus, 0,
            8seq_map(8fn(8X, 8times(8X, 8X)), 8seq(3, 1, 4, 1, 5, 9)))
))
```



## Oz


```oz
declare
  fun {SumOfSquares Xs}
     for X in Xs sum:S do
        {S X*X}
     end
  end
in
  {Show {SumOfSquares [3 1 4 1 5 9]}}
```



## PARI/GP


### Generic

It is possible to apply a function, in this case ^2 to each element of an iterable and sum the result:

```parigp
ss(v)={
  sum(i=1,#v,v[i]^2)
};
```


### Specific

For this particular task the product of a row matrix and its transpose is the sum of squares:

```parigp

n=[2,5,23]
print(n*n~)
n=[]
print(n*n~)

```

```txt

558
0

```



## Pascal

Example from the documenation of the run time library:

```pascal
Program Example45;

{ Program to demonstrate the SumOfSquares function. }

Uses math;

Var
  I : 1..100;
  ExArray : Array[1..100] of Float;

begin
  Randomize;
  for I:=low(ExArray) to high(ExArray) do
    ExArray[i]:=(Random-Random)*100;
  Writeln('Max             : ',MaxValue(ExArray):8:4);
  Writeln('Min             : ',MinValue(ExArray):8:4);
  Writeln('Sum squares     : ',SumOfSquares(ExArray):8:4);
  Writeln('Sum squares (b) : ',SumOfSquares(@ExArray[1],100):8:4);
end.
```



## Perl


```perl
sub sum_of_squares {
  my $sum = 0;
  $sum += $_**2 foreach @_;
  return $sum;
}

print sum_of_squares(3, 1, 4, 1, 5, 9), "\n";
```

or

```perl
use List::Util qw(reduce);
sub sum_of_squares {
  reduce { $a + $b **2 } 0, @_;
}

print sum_of_squares(3, 1, 4, 1, 5, 9), "\n";
```



## Perl 6

```perl6
say [+] map * ** 2, 3, 1, 4, 1, 5, 9;
```


If this expression seems puzzling, note that <code>* ** 2</code> is equivalent to <code>{$^x ** 2}</code>— the leftmost asterisk is not the multiplication operator but the <code>Whatever</code> star, which specifies currying behavior.
Another convenient way to distribute the exponentiation is via the cross metaoperator, which
as a list infix is looser than comma in precedence but tighter than the reduction list operator:


```perl6
say [+] 3,1,4,1,5,9 X** 2
```



## Phix


```Phix
?sum(sq_power(tagset(10),2))    -- prints 385
```



## PHP


```php

function sum_squares(array $args) {
    return array_reduce(
        $args, create_function('$x, $y', 'return $x+$y*$y;'), 0
    );
}

```


In PHP5.3 support for anonymous functions was reworked. While the above code would still work, it is suggested to use


```php

function sum_squares(array $args) {
    return array_reduce($args, function($x, $y) {
        return $x+$y*$y;
    }, 0);
}

```

Usage for both examples: <code>sum_squares(array(1,2,3,4,5)); // 55</code>


## PicoLisp


```PicoLisp
: (sum '((N) (* N N)) (3 1 4 1 5 9))
-> 133
: (sum '((N) (* N N)) ())
-> 0
```



## PL/I


```PL/I

declare A(10) float initial (10, 9, 8, 7, 6, 5, 4, 3, 2, 1);

put (sum(A**2));

```



## Pop11



```pop11
define sum_squares(v);
    lvars s = 0, j;
    for j from 1 to length(v) do
        s + v(j)*v(j) -> s;
    endfor;
    s;
enddefine;

sum_squares({1 2 3 4 5}) =>
```



## PostScript

<lang>
/sqrsum{
/x exch def
/sum 0 def
/i 0 def
x length 0 eq
{}
{
x length{
/sum sum x i get 2 exp add def
/i i 1 add def
}repeat
}ifelse
sum ==
}def

```


```postscript

[3 1 4 1 5 9] 0 {dup * +} fold

```



## PowerShell


```powershell
function Get-SquareSum ($a) {
    if ($a.Length -eq 0) {
        return 0
    } else {
        $x = $a `
             | ForEach-Object { $_ * $_ } `
             | Measure-Object -Sum
        return $x.Sum
    }
}
```



## PureBasic


```PureBasic
Procedure SumOfSquares(List base())
  ForEach base()
    Sum + base()*base()
  Next
  ProcedureReturn Sum
EndProcedure
```



## Python

'''Using generator expression'''

```python
sum(x * x for x in [1, 2, 3, 4, 5])
# or
sum(x ** 2 for x in [1, 2, 3, 4, 5])
# or
sum(pow(x, 2) for x in [1, 2, 3, 4, 5])
```


'''Functional versions:'''

```python
# using lambda and map:
sum(map(lambda x: x * x, [1, 2, 3, 4, 5]))
# or
sum(map(lambda x: x ** 2, [1, 2, 3, 4, 5]))
# or
sum(map(lambda x: pow(x, 2), [1, 2, 3, 4, 5]))

# using pow and repeat
from itertools import repeat
sum(map(pow, [1, 2, 3, 4, 5], repeat(2)))

# using starmap and mul
from itertools import starmap
from operator import mul
a = [1, 2, 3, 4, 5]
sum(starmap(mul, zip(a, a)))

# using reduce
from functools import reduce
powers_of_two = (x * x for x in [1, 2, 3, 4, 5])
reduce(lambda x, y : x + y, powers_of_two)
# or
from operator import add
powers_of_two = (x * x for x in [1, 2, 3, 4, 5])
reduce(add, powers_of_two)
# or using a bit more complex lambda
reduce(lambda a, x: a + x*x, [1, 2, 3, 4, 5])
```


'''Using NumPy:'''

```python
import numpy as np
a = np.array([1, 2, 3, 4, 5])
np.sum(a ** 2)
```



## Prolog

    sum([],0).
    sum([H|T],S) :- sum(T, S1), S is S1 + (H * H).


## Q


```q
ssq:{sum x*x}
```



## R


```r
arr <- c(1,2,3,4,5)
result <- sum(arr^2)
```



## Racket


```racket

#lang racket
(for/sum ([x #(3 1 4 1 5 9)]) (* x x))

```



## Raven


```Raven
define sumOfSqrs use $lst
   0 $lst each dup * +

[ 1 2 3 4] sumOfSqrs "Sum of squares: %d\n" print
```

```txt
Sum of squares: 30
```



## REXX


### input from pgm


```rexx
/*REXX program  sums  the squares of the numbers  in a (numeric)  vector of 15 numbers. */
numeric digits 100                               /*allow 100─digit numbers; default is 9*/
v= -100 9 8 7 6 0 3 4 5 2 1 .5 10 11 12          /*define a vector with fifteen numbers.*/
#=words(v)                                       /*obtain number of words in the V list.*/
$= 0                                             /*initialize the  sum  ($)  to zero.   */
       do k=1  for #                             /*process each number in the V vector. */
       $=$ + word(v,k)**2                        /*add a squared element to the ($) sum.*/
       end   /*k*/                               /* [↑]  if vector is empty, then sum=0.*/
                                                 /*stick a fork in it,  we're all done. */
say 'The sum of '      #      " squared elements for the  V  vector is: "   $
```

'''output'''   using an internal vector (list) of numbers:

```txt

The sum of  15  squared elements for the  V  vector is:  10650.25

```



### input from C.L.


```rexx
/*REXX program  sums  the squares of the numbers  in a (numeric)  vector of 15 numbers. */
numeric digits 100                               /*allow 100─digit numbers; default is 9*/
parse arg v                                      /*get optional numbers from the C.L.   */
if v=''  then v= -100 9 8 7 6 0 3 4 5 2 1 .5 10 11 12      /*Not specified?  Use default*/
#=words(v)                                                 /*obtain number of words in V*/
say 'The vector of '    #     " elements is: "   space(v)  /*display the vector numbers.*/
$= 0                                             /*initialize the  sum  ($)  to zero.   */
             do  until v=='';   parse var v x v  /*process each number in the V vector. */
             $=$ + x**2                          /*add a squared element to the ($) sum.*/
             end   /*until*/                     /* [↑]  if vector is empty, then sum=0.*/
say                                              /*stick a fork in it,  we're all done. */
say 'The sum of '       #     " squared elements for the  V  vector is: "      $
```

'''output'''   using a vector (list) of numbers from the command line:

```txt

The vector of  10  elements is:  -1000 -100 -10 -1 0 +1 +10 100 1000 1e20

The sum of  10  squared elements for the  V  vector is:  10000000000000000000000000000000002020202

```



## Ring


```ring

aList = [1,2,3,4,5]
see sumOfSquares(aList)

func sumOfSquares sos
sumOfSquares = 0
for i=1 to len(sos)
    sumOfSquares = sumOfSquares + pow(sos[i],2)
next
return sumOfSquares

```


## Ruby


```ruby
[3,1,4,1,5,9].reduce(0){|sum,x| sum + x*x}
```


or with the Ruby 2.4+ method ''sum''.


```ruby
[3,1,4,1,5,9].sum{|x| x*x}
```



## Run BASIC


```runbasic
list$ = "1,2,3,4,5"
print sumOfSquares(list$)

FUNCTION sumOfSquares(sos$)
  while word$(sos$,i+1,",") <> ""
    i = i + 1
    sumOfSquares = sumOfSquares + val(word$(sos$,i,","))^2
  wend
END FUNCTION
```



## Rust


```rust
fn sq_sum(v: &[f64]) -> f64 {
    v.iter().fold(0., |sum, &num| sum + num*num)
}

fn main() {
    let v = vec![3.0, 1.0, 4.0, 1.0, 5.5, 9.7];
    println!("{}", sq_sum(&v));

    let u : Vec<f64> = vec![];
    println!("{}", sq_sum(&u));
}
```



## Sather


```sather
class MAIN is

  sqsum(s, e:FLT):FLT is
    return s + e*e;
  end;

  sum_of_squares(v :ARRAY{FLT}):FLT is
    return (#ARRAY{FLT}(|0.0|).append(v)).reduce(bind(sqsum(_,_)));
  end;

  main is
    v :ARRAY{FLT} := |3.0, 1.0, 4.0, 1.0, 5.0, 9.0|;
    #OUT + sum_of_squares(v) + "\n";
  end;

end;
```



## Scala

Unfortunately there is no common "Numeric" class that Int and Double both extend, since Scala's number representation maps closely to Java's. Those concerned about precision can define a similar procedure for integers.


```scala
def sum_of_squares(xs: Seq[Double]) = xs.foldLeft(0) {(a,x) => a + x*x}
```



## Scheme


```scheme
(define (sum-of-squares l)
  (apply + (map * l l)))
```


 > (sum-of-squares (list 3 1 4 1 5 9))
 133


## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const array float: list1 is [] (3.0, 1.0, 4.0, 1.0, 5.0, 9.0);
const array float: list2 is 0 times 0.0;

const func float: squaredSum (in array float: floatList) is func
  result
    var float: sum is 0.0;
  local
    var float: number is 0.0;
  begin
    for number range floatList do
      sum +:= number ** 2;
    end for;
  end func;

const proc: main is func
  begin
    writeln(squaredSum(list1));
    writeln(squaredSum(list2));
  end func;
```



## Sidef


```ruby
func sum_of_squares(vector) {
    var sum = 0;
    vector.each { |n| sum += n**2 };
    return sum;
}

say sum_of_squares([]);         # 0
say sum_of_squares([1,2,3]);    # 14
```



## Slate



```slate
{1. 2. 3} reduce: [|:x :y| y squared + x].
{} reduce: [|:x :y| y squared + x] ifEmpty: [0].
```



## Smalltalk


```smalltalk
#(3 1 4 1 5 9) inject: 0 into: [:sum :aNumber | sum + aNumber squared]
```



## SNOBOL4


```SNOBOL4
        define('ssq(a)i') :(ssq_end)
ssq     i = i + 1; ssq = ssq + (a<i> * a<i>) :s(sumsq)f(return)
ssq_end

*       # Fill array, test and display
        str = '1 2 3 5 7 11 13 17 19 23'; a = array(10)
loop    i = i + 1; str len(p) span('0123456789') . a<i> @p :s(loop)
        output = str ' -> ' sumsq(a)
end
```


```txt
 1 2 3 5 7 11 13 17 19 23 -> 1557
```



## Standard ML



```sml
foldl (fn (a, sum) => sum + a * a) 0 ints
```



```sml
foldl (fn (a, sum) => sum + a * a) 0.0 reals
```



## SQL



```sql
select sum(x*x) from vector
```


Note that this assumes that the values in our vector are named <code>x</code>.


## Stata


###  Mata


```stata
a = 1..100
sum(a:^2)
  338350

a = J(0, 1, .)
length(a)
  0
sum(a:^2)
  0
```



## Swift



```swift
func sumSq(s: [Int]) -> Int {
  return s.map{$0 * $0}.reduce(0, +)
}
```



## Tcl


```tcl
proc sumOfSquares {nums} {
    set sum 0
    foreach num $nums {
        set sum [expr {$sum + $num**2}]
    }
    return $sum
}
sumOfSquares {1 2 3 4 5} ;# ==> 55
sumOfSquares {} ;# ==> 0
```


```tcl
package require struct::list

proc square x {expr {$x * $x}}
proc + {a b} {expr {$a + $b}}
proc sumOfSquares {nums} {
    struct::list fold [struct::list map $nums square] 0 +
}
sumOfSquares {1 2 3 4 5} ;# ==> 55
sumOfSquares {} ;# ==> 0
```

Generic "sum of <i>function</i>"

```tcl
package require Tcl 8.5
package require struct::list
namespace path ::tcl::mathop

proc sum_of {lambda nums} {
    struct::list fold [struct::list map $nums [list apply $lambda]] 0 +
}

sum_of {x {* $x $x}} {1 2 3 4 5} ;# ==> 55
```



## Trith


```trith
[3 1 4 1 5 9] 0 [dup * +] foldl
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
array="3'1'4'1'5'9",sum=0
LOOP a=array
sum=sum+(a*a)
ENDLOOP
PRINT sum

```

```txt

133

```



## UnixPipes



```bash
folder() {
   (read B; res=$( expr $1 \* $1 ) ; test -n "$B" && expr $res + $B || echo $res)
}

fold() {
   (while read a ; do
       fold | folder $a
   done)
}


(echo 3; echo 1; echo 4;echo 1;echo 5; echo 9) | fold
```



## Ursala


The ssq function defined below zips two copies of its argument together,
maps the product function to all pairs, and then sums the
result by way of the reduction operator, -:.

```Ursala
#import nat

ssq = sum:-0+ product*iip

#cast %n

main = ssq <21,12,77,0,94,23,96,93,72,72,79,24,8,50,9,93>
```

```txt
62223
```



## V


```v
[sumsq [dup *] map 0 [+] fold].

[] sumsq
=0
[1 2 3] sumsq
```

 =14


## VBA


```vb
Public Sub sum_of_squares()
    Debug.Print WorksheetFunction.SumSq([{1,2,3,4,5,6,7,8,9,10}])
End Sub
```
```txt
 385
```


## VBScript


```vb

Function sum_of_squares(arr)
	If UBound(arr) = -1 Then
		sum_of_squares = 0
	End If
	For i = 0 To UBound(arr)
		sum_of_squares = sum_of_squares + (arr(i)^2)
	Next
End Function

WScript.StdOut.WriteLine sum_of_squares(Array(1,2,3,4,5))
WScript.StdOut.WriteLine sum_of_squares(Array())

```


```txt

55
0

```



## Visual Basic .NET


```vbnet

 Private Shared Function sumsq(ByVal i As ICollection(Of Integer)) As Integer
        If i Is Nothing OrElse i.Count = 0 Then
            Return 0
        End If
        Return i.[Select](Function(x) x * x).Sum()
 End Function

 Private Shared Sub Main()
        Dim a As Integer() = New Integer() {1, 2, 3, 4, 5}
        ' 55
        Console.WriteLine(sumsq(a))

        For K As Integer = 0 To 16
               Console.WriteLine("SumOfSquares({0}) = {1}", K, SumOfSquares(K))
        Next
 End Sub
 Function SumOfSquares(ByVal Max As Integer)
        Dim Square As Integer = 0
        Dim Add As Integer = 1
        Dim Sum As Integer = 0
        For J As Integer = 0 To Max - 1
            Square += Add
            Add += 2
            Sum += Square
        Next
        Return Sum
 End Function

 Function SumOfSquaresByMult(ByVal Max As Integer)
        Dim Sum As Integer = 0
        For J As Integer = 1 To Max
            Sum += J * J
        Next
        Return Sum
 End Function


```

```txt

55
SumOfSquares(0) = 0
SumOfSquares(1) = 1
SumOfSquares(2) = 5
SumOfSquares(3) = 14
SumOfSquares(4) = 30
SumOfSquares(5) = 55
SumOfSquares(6) = 91
SumOfSquares(7) = 140
SumOfSquares(8) = 204
SumOfSquares(9) = 285
SumOfSquares(10) = 385
SumOfSquares(11) = 506
SumOfSquares(12) = 650
SumOfSquares(13) = 819
SumOfSquares(14) = 1015
SumOfSquares(15) = 1240
SumOfSquares(16) = 1496

```



## Wortel


```wortel
@sum !*^@sq [3 1 4 1 5 9] ; returns 133
```


```wortel
@sum !*^@sq [] ; returns 0
```

As a function:

```wortel
^(@sum *^@sq)
```

Iterative function:

```wortel
&a [@var sum 0 @for x of a :!+sum *x x sum]
```




## XLISP

The task specification calls for a function that takes a numeric vector. If you want a function that takes a linked list (which would be more idiomatic), just extract the inner function <tt>SUMSQ</tt> and use that instead of <tt>SUM-OF-SQUARES</tt>.

```lisp
(defun sum-of-squares (vec)
    (defun sumsq (xs)
        (if (null xs)
            0
            (+ (expt (car xs) 2) (sumsq (cdr xs)))))
    (sumsq (vector->list vec)))

(define first-seven-primes #(2 3 5 7 11 13 17))

(define zero-length-vector #())

(print `(the sum of the squares of the first seven prime numbers is ,(sum-of-squares first-seven-primes)))

(print `(the sum of the squares of no numbers at all is ,(sum-of-squares zero-length-vector)))
```

```txt
(THE SUM OF THE SQUARES OF THE FIRST SEVEN PRIME NUMBERS IS 666)
(THE SUM OF THE SQUARES OF NO NUMBERS AT ALL IS 0)
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

func SumSq(V, L);
int  V, L;
int  S, I;
[S:= 0;
for I:= 0 to L-1 do S:= S+sq(V(I));
return S;
]; \SumSq

[IntOut(0, SumSq([1,2,3,4,5,6,7,8,9,10], 10));  CrLf(0);
 IntOut(0, SumSq([0], 0));  CrLf(0);    \zero-length vector "[]" doesn't compile
]
```


```txt

385
0

```



## zkl


```zkl
T().reduce(fcn(p,n){ p + n*n },0)               //-->0
T(3,1,4,1,5,9).reduce(fcn(p,n){ p + n*n },0.0) //-->133.0
[1..5].reduce(fcn(p,n){ p + n*n },0)          //-->55
```

