+++
title = "Dot product"
description = ""
date = 2019-10-17T19:26:06Z
aliases = []
[extra]
id = 6115
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Create a function/use an in-built function, to compute the   '''[[wp:Dot product|dot product]]''',   also known as the   '''scalar product'''   of two vectors.

If possible, make the vectors of arbitrary length.


As an example, compute the dot product of the vectors:
::::   <big> <code> [1,  3, -5] </code> </big>     and
::::   <big> <code> [4,      -2, -1] </code> </big>



If implementing the dot product of two vectors directly:
:::*   each vector must be the same length
:::*   multiply corresponding terms from each vector
:::*   sum the products   (to produce the answer)


;Related task:
*   [[Vector products]]





## 11l


```11l
print(dot((1,  3, -5), (4, -2, -1)))
```

{{out}}
```txt
3
```



## 360 Assembly


```360asm
*        Dot product               03/05/2016
DOTPROD  CSECT
         USING  DOTPROD,R15
         SR     R7,R7              p=0
         LA     R6,1               i=1
LOOPI    CH     R6,=AL2((B-A)/4)   do i=1 to hbound(a)
         BH     ELOOPI
         LR     R1,R6              i
         SLA    R1,2               *4
         L      R3,A-4(R1)         a(i)
         L      R4,B-4(R1)         b(i)
         MR     R2,R4              a(i)*b(i)
         AR     R7,R3              p=p+a(i)*b(i)
         LA     R6,1(R6)           i=i+1
         B      LOOPI
ELOOPI   XDECO  R7,PG              edit p
         XPRNT  PG,80              print buffer
         XR     R15,R15            rc=0
         BR     R14                return
A        DC     F'1',F'3',F'-5'
B        DC     F'4',F'-2',F'-1'
PG       DC     CL80' '            buffer
         YREGS
         END    DOTPROD
```

{{out}}

```txt

           3

```



## 8th


```Forth
[1,3,-5] [4,-2,-1] ' n:* ' n:+ a:dot . cr
```

{{out}}
```txt
3
```


## ABAP


```ABAP
report zdot_product
data: lv_n type i,
      lv_sum type i,
      lt_a type standard table of i,
      lt_b type standard table of i.

append: '1' to lt_a, '3' to lt_a, '-5' to lt_a.
append: '4' to lt_b, '-2' to lt_b, '-1' to lt_b.
describe table lt_a lines lv_n.

perform dot_product using lt_a lt_b lv_n changing lv_sum.

write lv_sum left-justified.

form dot_product using it_a like lt_a
                       it_b like lt_b
                       iv_n type i
                 changing
                       ev_sum type i.
  field-symbols: <wa_a> type i, <wa_b> type i.

  do iv_n times.
    read table: it_a assigning <wa_a> index sy-index, it_b assigning <wa_b> index sy-index.
    lv_sum = lv_sum + ( <wa_a> * <wa_b> ).
  enddo.
endform.
```

{{out}}
```txt
3
```



## ACL2


```Lisp
(defun dotp (v u)
   (if (or (endp v) (endp u))
       0
       (+ (* (first v) (first u))
          (dotp (rest v) (rest u)))))
```



```txt
&gt; (dotp '(1 3 -5) '(4 -2 -1))
3
```



## ActionScript


```ActionScript
function dotProduct(v1:Vector.<Number>, v2:Vector.<Number>):Number
{
	if(v1.length != v2.length) return NaN;
	var sum:Number = 0;
	for(var i:uint = 0; i < v1.length; i++)
		sum += v1[i]*v2[i];
	return sum;
}
trace(dotProduct(Vector.<Number>([1,3,-5]),Vector.<Number>([4,-2,-1])));
```



## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;
procedure dot_product is
	type vect is array(Positive range <>) of Integer;
	v1 : vect := (1,3,-5);
	v2 : vect := (4,-2,-1);

	function dotprod(a: vect; b: vect) return Integer is
		sum : Integer := 0;
		begin
		if not (a'Length=b'Length) then raise Constraint_Error; end if;
		for p in a'Range loop
			sum := sum + a(p)*b(p);
		end loop;
		return sum;
	end dotprod;

begin
put_line(Integer'Image(dotprod(v1,v2)));
end dot_product;
```

{{out}}
```txt
3
```



## Aime


```aime
real
dp(list a, list b)
{
    real p, v;
    integer i;

    p = 0;
    for (i, v in a) {
        p += v * b[i];
    }

    p;
}

integer
main(void)
{
    o_(dp(list(1r, 3r, -5r), list(4r, -2r, -1r)), "\n");

    0;
}
```

{{out}}
```txt
3
```



## ALGOL 68

{{trans|C++}}
{{works with|ALGOL 68|Standard - with prelude inserted manually}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
MODE DOTFIELD = REAL;
MODE DOTVEC = [1:0]DOTFIELD;

# The "Spread Sheet" way of doing a dot product:
  o Assume bounds are equal, and start at 1
  o Ignore round off error
#
PRIO SSDOT = 7;
OP SSDOT = (DOTVEC a,b)DOTFIELD: (
  DOTFIELD sum := 0;
  FOR i TO UPB a DO sum +:= a[i]*b[i] OD;
  sum
);

# An improved dot-product version:
  o Handles sparse vectors
  o Improves summation by gathering round off error
    with no additional multiplication - or LONG - operations.
#
OP * = (DOTVEC a,b)DOTFIELD: (
  DOTFIELD sum := 0, round off error:= 0;
  FOR i
# Assume bounds may not be equal, empty members are zero (sparse) #
    FROM LWB (LWB a > LWB b | a | b )
    TO UPB (UPB a < UPB b | a | b )
  DO
    DOTFIELD org = sum, prod = a[i]*b[i];
    sum +:= prod;
    round off error +:= sum - org - prod
  OD;
  sum - round off error
);

# Test: #
DOTVEC a=(1,3,-5), b=(4,-2,-1);

print(("a SSDOT b = ",fixed(a SSDOT b,0,real width), new line));
print(("a   *   b = ",fixed(a   *   b,0,real width), new line))
```

{{out}}
```txt
a SSDOT b = 3.000000000000000
a   *   b = 3.000000000000000
```



## ALGOL W


```algolw
begin
    % computes the dot product of two equal length integer vectors            %
    % (single dimension arrays ) the length of the vectors must be specified  %
    % in length.                                                              %
    integer procedure integerDotProduct( integer array a ( * )
                                       ; integer array b ( * )
                                       ; integer value length
                                       ) ;
    begin
        integer product;
        product := 0;
        for i := 1 until length do product := product + ( a(i) * b(i) );
        product
    end integerDotProduct ;

    % declare two vectors of length 3                                         %
    integer array v1, v2 ( 1 :: 3 );
    % initialise the vectors                                                  %
    v1(1) :=  1; v1(2) :=  3; v1(3) := -5;
    v2(1) :=  4; v2(2) := -2; v2(3) := -1;
    % output the dot product                                                  %
    write( integerDotProduct( v1, v2, 3 ) )
end.

```



## APL


```APL
1 3 ¯5 +.× 4 ¯2 ¯1
```

<b>Output:</b>

```txt
3
```



## AppleScript

{{trans|JavaScript}} ( functional version )

```AppleScript
-- DOT PRODUCT ---------------------------------------------------------------

-- dotProduct :: [Number] -> [Number] -> Number
on dotProduct(xs, ys)
    script product
        on |λ|(a, b)
            a * b
        end |λ|
    end script

    if length of xs = length of ys then
        sum(zipWith(product, xs, ys))
    else
        missing value -- arrays of differing dimension
    end if
end dotProduct


-- TEST ----------------------------------------------------------------------
on run

    dotProduct([1, 3, -5], [4, -2, -1])

    --> 3
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

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

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

-- sum :: [Number] -> Number
on sum(xs)
    script add
        on |λ|(a, b)
            a + b
        end |λ|
    end script

    foldl(add, 0, xs)
end sum

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```AppleScript>3</lang



## AutoHotkey


```AutoHotkey
Vet1 := "1,3,-5"
Vet2 := "4 , -2 , -1"
MsgBox % DotProduct( Vet1 , Vet2 )

;---------------------------

DotProduct( VectorA , VectorB )
{
  Sum := 0
  StringSplit, ArrayA, VectorA, `,, %A_Space%
  StringSplit, ArrayB, VectorB, `,, %A_Space%
  If ( ArrayA0 <> ArrayB0 )
    Return ERROR
  While ( A_Index <= ArrayA0 )
    Sum += ArrayA%A_Index% * ArrayB%A_Index%
  Return Sum
}
```



## AWK


```AWK

# syntax: GAWK -f DOT_PRODUCT.AWK
BEGIN {
    v1 = "1,3,-5"
    v2 = "4,-2,-1"
    if (split(v1,v1arr,",") != split(v2,v2arr,",")) {
      print("error: vectors are of unequal lengths")
      exit(1)
    }
    printf("%g\n",dot_product(v1arr,v2arr))
    exit(0)
}
function dot_product(v1,v2,  i,sum) {
    for (i in v1) {
      sum += v1[i] * v2[i]
    }
    return(sum)
}

```

{{out}}
```txt
3
```



## BASIC

=
## Applesoft BASIC
=
Calculates the dot product of two random vectors of length N.

```basic

 100 :
 110  REM  DOT PRODUCT
 120 :
 130  REM  INITIALIZE VECTORS OF LENGTH N
 140  N = 3
 150  DIM V1(N): DIM V2(N)
 160  FOR I = 1 TO N
 170  V1(I) =  INT ( RND (1) * 20 - 9.5)
 180  V2(I) =  INT ( RND (1) * 20 - 9.5)
 190  NEXT I
 300 :
 310  REM  CALCULATE THE DOT PRODUCT
 320 :
 330  FOR I = 1 TO N:DP = DP + V1(I) * V2(I): NEXT I
 400 :
 410  REM  DISPLAY RESULT
 420 :
 430  PRINT "[";: FOR I = 1 TO N: PRINT " ";V1(I);: NEXT I
 440  PRINT "] . [";: FOR I = 1 TO N: PRINT " ";V2(I);: NEXT I
 450  PRINT "] = ";DP

```

{{out}}

```txt
]RUN
[ 7 2 -2] . [ 7 -5 8] = 23
]RUN
[ -3 -4 -8] . [ -8 7 6] = -52
```


=
## BBC BASIC
=
BBC BASIC has a built-in dot-product operator:

```bbcbasic
      DIM vec1(2), vec2(2), dot(0)

      vec1() = 1, 3, -5
      vec2() = 4, -2, -1

      dot() = vec1() . vec2()
      PRINT "Result is "; dot(0)
```

{{out}}
```txt
Result is 3
```



## bc


```bc
/* Calculate the dot product of two vectors a and b (represented as
 * arrays) of size n.
 */
define d(a[], b[], n) {
    auto d, i

    for (i = 0; i < n; i++) {
        d += a[i] * b[i]
    }
    return(d)
}

a[0] = 1
a[1] = 3
a[2] = -5
b[0] = 4
b[1] = -2
b[2] = -1
d(a[], b[], 3)
```


{{Out}}

```txt
3
```



## Befunge 93


```befunge

v Space for variables
v Space for vector1
v Space for vector2
v http://rosettacode.org/wiki/Dot_product
                                            >00pv
>>55+":htgneL",,,,,,,,&:0`                  |
v,,,,,,,"Length can't be negative."+55<
>,,,,,,,,,,,,,,,,,,,@                 |!`-10<
                                      >0.@
v,")".g00,,,,,,,,,,,,,,"Vector a(size "         <
0v01g00,")".g00,,,,,,,,,,,,,,"Vector b"<
0pvp2g01&p01-1g01<                     "
g>>         10g0`|               @.g30<(
1                >03g:-03p>00g1-`     |s
0      vp00-1g00p30+g30*g2-1g00g1-1g00<i
p      >        v         #            z
vp1g01&p01-1g01<>         ^            e
>      10g0`   |        vp01-1g01.g1<
               >00g1-10p>10g:01-`   |  "
                                    >  ^

```

{{out}}
```txt
Length:
3
Vector a(size 3 )1
3
-5
1 3 -5 Vector b(size 3 )4
-2
-1
3
```



## Bracmat


```bracmat
  ( dot
  =   a A z Z
    .     !arg:(%?a ?z.%?A ?Z)
        & !a*!A+dot$(!z.!Z)
      | 0
  )
& out$(dot$(1 3 -5.4 -2 -1));
```

{{out}}
```txt
3
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

int dot_product(int *, int *, size_t);

int
main(void)
{
        int a[3] = {1, 3, -5};
        int b[3] = {4, -2, -1};

        printf("%d\n", dot_product(a, b, sizeof(a) / sizeof(a[0])));

        return EXIT_SUCCESS;
}

int
dot_product(int *a, int *b, size_t n)
{
        int sum = 0;
        size_t i;

        for (i = 0; i < n; i++) {
                sum += a[i] * b[i];
        }

        return sum;
}
```

{{out}}
```txt
3
```


## C#

```c#
static void Main(string[] args)
{
	Console.WriteLine(DotProduct(new decimal[] { 1, 3, -5 }, new decimal[] { 4, -2, -1 }));
	Console.Read();
}

private static decimal DotProduct(decimal[] vec1, decimal[] vec2)
{
	if (vec1 == null)
		return 0;

	if (vec2 == null)
		return 0;

	if (vec1.Length != vec2.Length)
		return 0;

	decimal tVal = 0;
	for (int x = 0; x < vec1.Length; x++)
	{
		tVal += vec1[x] * vec2[x];
	}

	return tVal;
}
```

{{out}}
```txt
3
```


===Alternative using Linq (C# 4)===
{{works with|C sharp|C#|4}}

```c#
public static decimal DotProduct(decimal[] a, decimal[] b) {
    return a.Zip(b, (x, y) => x * y).Sum();
}
```



## C++


```cpp
#include <iostream>
#include <numeric>

int main()
{
    int a[] = { 1, 3, -5 };
    int b[] = { 4, -2, -1 };

    std::cout << std::inner_product(a, a + sizeof(a) / sizeof(a[0]), b, 0) << std::endl;

    return 0;
}
```

{{out}}
```txt
3
```



### Alternative using std::valarray


```cpp

#include <valarray>
#include <iostream>

int main()
{
    std::valarray<double> xs = {1,3,-5};
    std::valarray<double> ys = {4,-2,-1};

    double result = (xs * ys).sum();

    std::cout << result << '\n';

    return 0;
}
```

{{out}}
```txt
3
```



## Clojure

{{works with|Clojure|1.1}}
Preconditions are new in 1.1. The actual code also works in older Clojure versions.

```clojure
(defn dot-product [& matrix]
  {:pre [(apply == (map count matrix))]}
  (apply + (apply map * matrix)))

(defn dot-product2 [x y]
 (->> (interleave x y)
      (partition 2 2)
      (map #(apply * %))
      (reduce +)))

(defn dot-product3
  "Dot product of vectors. Tested on version 1.8.0."
  [v1 v2]
  {:pre [(= (count v1) (count v2))]}
  (reduce + (map * v1 v2)))

;Example Usage
(println (dot-product [1 3 -5] [4 -2 -1]))
(println (dot-product2 [1 3 -5] [4 -2 -1]))
(println (dot-product3 [1 3 -5] [4 -2 -1]))

```



## CoffeeScript


```coffeescript
dot_product = (ary1, ary2) ->
  if ary1.length != ary2.length
    throw "can't find dot product: arrays have different lengths"
  dotprod = 0
  for v, i in ary1
    dotprod += v * ary2[i]
  dotprod

console.log dot_product([ 1, 3, -5 ], [ 4, -2, -1 ]) # 3
try
  console.log dot_product([ 1, 3, -5 ], [ 4, -2, -1, 0 ]) # exception
catch e
  console.log e
```

{{out}}
```txt
> coffee foo.coffee
3
can't find dot product: arrays have different lengths
```



## Common Lisp


```lisp
(defun dot-product (a b)
  (apply #'+ (mapcar #'* (coerce a 'list) (coerce b 'list))))
```

This works with any size vector, and (as usual for Common Lisp) all numeric types (rationals, bignums, complex numbers, etc.).

Maybe it is better to do it without coercing. Then we got a cleaner code.

```lisp
(defun dot-prod (a b)
  (reduce #'+ (map 'simple-vector #'* a b)))
```


## Component Pascal

{{Works with|BlackBox Component Builder}}

```oberon2

MODULE DotProduct;
IMPORT StdLog;

PROCEDURE Calculate*(x,y: ARRAY OF INTEGER): INTEGER;
VAR
	i,sum: INTEGER;
BEGIN
	sum := 0;
	FOR i:= 0 TO LEN(x) - 1 DO
		INC(sum,x[i] * y[i]);
	END;
	RETURN sum
END Calculate;

PROCEDURE Test*;
VAR
	i,sum: INTEGER;
	v1,v2: ARRAY 3 OF INTEGER;
BEGIN
	v1[0] := 1;v1[1] := 3;v1[2] := -5;
	v2[0] := 4;v2[1] := -2;v2[2] := -1;

	StdLog.Int(Calculate(v1,v2));StdLog.Ln
END Test;

END DotProduct.

```

Execute: ^Q DotProduct.Test
{{out}}

```txt

3

```



## D


```d
void main() {
    import std.stdio, std.numeric;

    [1.0, 3.0, -5.0].dotProduct([4.0, -2.0, -1.0]).writeln;
}
```

{{out}}

```txt
3
```

Using an array operation:

```d
void main() {
    import std.stdio, std.algorithm;

    double[3] a = [1.0, 3.0, -5.0];
    double[3] b = [4.0, -2.0, -1.0];
    double[3] c = a[] * b[];
    c[].sum.writeln;
}
```



## Dart


```dart
num dot(List<num> A, List<num> B){
  if (A.length != B.length){
    throw new Exception('Vectors must be of equal size');
  }
  num result = 0;
  for (int i = 0; i < A.length; i++){
    result += A[i] * B[i];
  }
  return result;
}

void main(){
  var l = [1,3,-5];
  var k = [4,-2,-1];
  print(dot(l,k));
}
```

{{out}}

```txt
3
```



## Delphi

{{works with|Lazarus}}

```delphi
program Project1;

{$APPTYPE CONSOLE}

type
  doublearray = array of Double;

function DotProduct(const A, B : doublearray): Double;
var
I: integer;
begin
  assert (Length(A) = Length(B), 'Input arrays must be the same length');
  Result := 0;
  for I := 0 to Length(A) - 1 do
    Result := Result + (A[I] * B[I]);
end;

var
  x,y: doublearray;
begin
  SetLength(x, 3);
  SetLength(y, 3);
  x[0] := 1; x[1] := 3; x[2] := -5;
  y[0] := 4; y[1] :=-2; y[2] := -1;
  WriteLn(DotProduct(x,y));
  ReadLn;
end.
```

{{out}}
```txt
 3.00000000000000E+0000
```

Note: Delphi does not like arrays being declared in procedure headings, so it is necessary to declare it beforehand. To use integers, modify doublearray to be an array of integer.

=={{header|Déjà Vu}}==

```dejavu
dot a b:
	if /= len a len b:
		Raise value-error "dot product needs two vectors with the same length"

	0
	while a:
		+ * pop-from a pop-from b

!. dot [ 1 3 -5 ] [ 4 -2 -1 ]
```

{{out}}
```txt
3
```



## DWScript

For arbitrary length vectors, using a precondition to check vector length:

```delphi
function DotProduct(a, b : array of Float) : Float;
require
   a.Length = b.Length;
var
   i : Integer;
begin
   Result := 0;
   for i := 0 to a.High do
      Result += a[i]*b[i];
end;

PrintLn(DotProduct([1,3,-5], [4,-2,-1]));
```

Using built-in 4D Vector type:

```delphi
var a := Vector(1, 3, -5, 0);
var b := Vector(4, -2, -1, 0);

PrintLn(a * b);
```

Ouput in both cases:
```txt
3
```



## EchoLisp


```lisp

(define a #(1 3 -5))
(define b #(4 -2 -1))

;; function definition
(define ( ⊗ a b) (for/sum ((x a)(y b)) (* x y)))
(⊗ a b) → 3

;; library
(lib 'math)
(dot-product a b) → 3

```



## Eiffel


```Eiffel
class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		do
			print(dot_product(<<1, 3, -5>>, <<4, -2, -1>>).out)
		end

feature -- Access

	dot_product (a, b: ARRAY[INTEGER]): INTEGER
			-- Dot product of vectors `a' and `b'.
		require
			a.lower = b.lower
			a.upper = b.upper
		local
			i: INTEGER
		do
			from
				i := a.lower
			until
				i > a.upper
			loop
				Result := Result + a[i] * b[i]
				i := i + 1
			end
		end
end
```


Ouput:
```txt
3
```



## Ela

{{Trans|Haskell}}

```ela
open list

dotp a b | length a == length b = sum (zipWith (*) a b)
         | else = fail "Vector sizes must match."

dotp [1,3,-5] [4,-2,-1]
```

{{out}}
```txt
3
```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'routines;

extension op
{
    method dotProduct(int[] array)
        = self.zipBy(array, (x,y => x * y)).summarize();
}

public program()
{
    console.printLine(new int[]::(1, 3, -5).dotProduct(new int[]::(4, -2, -1)))
}
```

{{out}}

```txt

3

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Vector do
  def dot_product(a,b) when length(a)==length(b), do: dot_product(a,b,0)
  def dot_product(_,_) do
    raise ArgumentError, message: "Vectors must have the same length."
  end

  defp dot_product([],[],product), do: product
  defp dot_product([h1|t1], [h2|t2], product), do: dot_product(t1, t2, product+h1*h2)
end

IO.puts Vector.dot_product([1,3,-5],[4,-2,-1])
```


{{out}}

```txt

3

```



## Emacs Lisp


```Emacs Lisp

(defun dot-product (v1 v2)
  (setq res 0)
  (dotimes (i (length v1))
    (setq res (+ (* (elt v1 i) (elt v2 i) ) res) ))
  res)

(progn
  (insert (format "%d\n" (dot-product [1 2 3] [1 2 3]) ))
  (insert (format "%d\n" (dot-product '(1 2 3) '(1 2 3) ))))

```

<b>Output:</b>

```txt

14
14

```



## Erlang


```erlang
dotProduct(A,B) when length(A) == length(B) -> dotProduct(A,B,0);
dotProduct(_,_) -> erlang:error('Vectors must have the same length.').

dotProduct([H1|T1],[H2|T2],P) -> dotProduct(T1,T2,P+H1*H2);
dotProduct([],[],P) -> P.

dotProduct([1,3,-5],[4,-2,-1]).
```

{{out}}
```txt
3
```



## Euphoria


```Euphoria
function dotprod(sequence a, sequence b)
    atom sum
    a *= b
    sum = 0
    for n = 1 to length(a) do
        sum += a[n]
    end for
    return sum
end function

? dotprod({1,3,-5},{4,-2,-1})
```

{{out}}

```txt
3
```


```Euphoria
-- Here is an alternative method,
-- using the standard Euphoria Version 4+ Math Library
include std/math.e
sequence a = {1,3,-5}, b = {4,-2,-1}  -- Make them any length you want
? sum(a * b)
```

{{out}}
```txt
3
```


=={{header|F_Sharp|F#}}==

```fsharp
let dot_product (a:array<'a>) (b:array<'a>) =
    if Array.length a <> Array.length b then failwith "invalid argument: vectors must have the same lengths"
    Array.fold2 (fun acc i j -> acc + (i * j)) 0 a b
```


```txt
&gt; dot_product [| 1; 3; -5 |] [| 4; -2; -1 |] ;;
val it : int = 3
```



## Factor

The built-in word <code>v.</code> is used to compute the dot product. It doesn't enforce that the vectors are of the same length, so here's a wrapper.

```factor
USING: kernel math.vectors sequences ;

: dot-product ( u v -- w )
    2dup [ length ] bi@ =
    [ v. ] [ "Vector lengths must be equal" throw ] if ;
```


 ( scratchpad ) { 1 3 -5 } { 4 -2 -1 } dot-product .
 3


## FALSE


```false
[[\1-$0=~][$d;2*1+\-ø\$d;2+\-ø@*@+]#]p:
3d: {Vectors' length}
1 3 5_ 4 2_ 1_ d;$1+ø@*p;!%. {Output: 3}
```



## Fantom

Dot product of lists of Int:

```fantom
class DotProduct
{
  static Int dotProduct (Int[] a, Int[] b)
  {
    Int result := 0
    [a.size,b.size].min.times |i|
    {
      result += a[i] * b[i]
    }
    return result
  }

  public static Void main ()
  {
    Int[] x := [1,2,3,4]
    Int[] y := [2,3,4]

    echo ("Dot product of $x and $y is ${dotProduct(x, y)}")
  }
}
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Dot_product this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: vector create cells allot ;
: th cells + ;

3 constant /vector
/vector vector a
/vector vector b

: dotproduct                           ( a1 a2 -- n)
  0 tuck ?do -rot over i th @ over i th @ * >r rot r> + loop nip nip
;

: vector! cells over + swap ?do i ! 1 cells +loop ;

-5  3 1 a /vector vector!
-1 -2 4 b /vector vector!

a b /vector dotproduct . 3 ok
```



## Fortran



```fortran
program test_dot_product

  write (*, '(i0)') dot_product ([1, 3, -5], [4, -2, -1])

end program test_dot_product
```

{{out}}
```txt
3
```


The intrinsic function <code>Dot_Product(X,Y)</code> accepts various precisions of integer, floating-point and complex arrays (for which it is <code>Sum(Conjg(x)*y)</code>) and even logical, for which it is <code>Any(x .AND. y)</code> returning zero if either array is of length zero, or ''false'' for logical types.


## FunL


```funl
import lists.zipWith

def dot( a, b )
  | a.length() == b.length() = sum( zipWith((*), a, b) )
  | otherwise = error( "Vector sizes must match" )

println( dot([1, 3, -5], [4, -2, -1]) )
```

{{out}}
```txt
3
```



## GAP


```gap
# Built-in

[1, 3, -5]*[4, -2, -1];
# 3
```



## GLSL

The dot product is built-in:

```glsl

float dot_product = dot(vec3(1, 3, -5), vec3(4, -2, -1));

```


## Go


### Implementation


```go
package main

import (
    "errors"
    "fmt"
    "log"
)

var (
    v1 = []int{1, 3, -5}
    v2 = []int{4, -2, -1}
)

func dot(x, y []int) (r int, err error) {
    if len(x) != len(y) {
        return 0, errors.New("incompatible lengths")
    }
    for i, xi := range x {
        r += xi * y[i]
    }
    return
}

func main() {
    d, err := dot([]int{1, 3, -5}, []int{4, -2, -1})
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(d)
}
```

{{out}}

```txt

3

```


### Library gonum/floats


```go
package main

import (
    "fmt"

    "github.com/gonum/floats"
)

var (
    v1 = []float64{1, 3, -5}
    v2 = []float64{4, -2, -1}
)

func main() {
    fmt.Println(floats.Dot(v1, v2))
}
```

{{out}}

```txt

3

```



## Groovy

Solution:

```groovy
def dotProduct = { x, y ->
    assert x && y && x.size() == y.size()
    [x, y].transpose().collect{ xx, yy -> xx * yy }.sum()
}
```

Test:

```groovy
println dotProduct([1, 3, -5], [4, -2, -1])
```

{{out}}
```txt
3
```



## Haskell



```haskell>dotp :: Num a =
 [a] -> [a] -> a
dotp a b | length a == length b = sum (zipWith (*) a b)
         | otherwise = error "Vector sizes must match"

main = print $ dotp [1, 3, -5] [4, -2, -1] -- prints 3
```


Or, using the Maybe monad to avoid exceptions and keep things composable:

```haskell
dotp
  :: Num a
  => [a] -> [a] -> Maybe a
dotp a b
  | length a == length b = Just $ sum (zipWith (*) a b)
  | otherwise = Nothing

main :: IO ()
main = mbPrint $ dotp [1, 3, -5] [4, -2, -1] -- prints 3

mbPrint
  :: Show a
  => Maybe a -> IO ()
mbPrint (Just x) = print x
mbPrint n = print n
```



## Hy


```clojure
(defn dotp [a b]
  (assert (= (len a) (len b)))
  (sum (genexpr (* aterm bterm)
                [(, aterm bterm) (zip a b)])))

(assert (= 3 (dotp [1 3 -5] [4 -2 -1])))
```


=={{header|Icon}} and {{header|Unicon}}==
The procedure below computes the dot product of two vectors of arbitrary length or generates a run time error if its arguments are the wrong type or shape.

```Icon
procedure main()
write("a dot b := ",dotproduct([1, 3, -5],[4, -2, -1]))
end

procedure dotproduct(a,b)   #: return dot product of vectors a & b or error
if *a ~= *b & type(a) == type(b) == "list" then runerr(205,a) # invalid value
every (dp := 0) +:= a[i := 1 to *a] * b[i]
return dp
end
```



## IDL


```IDL

a = [1, 3, -5]
b = [4, -2, -1]
c = a#TRANSPOSE(b)
c = TOTAL(a*b,/PRESERVE_TYPE)

```



## Idris


```idris
module Main

import Data.Vect

dotProduct : (Num a) => Vect n a -> Vect n a -> a
dotProduct = (sum .) . zipWith (*)

main : IO ()
main = printLn $ dotProduct [1,2,3] [1,2,3]

```



## J


```j
   1 3 _5  +/ . * 4 _2 _1
3
   dotp=: +/ . *                  NB. Or defined as a verb (function)
   1 3 _5  dotp 4 _2 _1
3
```

Note also: The verbs built using the conjunction <code> .</code> generally apply to matricies and arrays of higher dimensions and can be built with verbs (functions) other than sum ( <code>+/</code> ) and product ( <code>*</code> ).

Spelling issue: The conjunction <code> .</code> needs to be preceded by a space. This is because J's spelling rules say that if the character '.' is preceded by any other character, it is included in the same parser token that included that other character. In other words, <code>1.23e4</code>, <code>'...'</code> and <code>/.</code> are each examples of "parser tokens".


## Java


```java
public class DotProduct {

	public static void main(String[] args) {
		double[] a = {1, 3, -5};
		double[] b = {4, -2, -1};

		System.out.println(dotProd(a,b));
	}

	public static double dotProd(double[] a, double[] b){
		if(a.length != b.length){
			throw new IllegalArgumentException("The dimensions have to be equal!");
		}
		double sum = 0;
		for(int i = 0; i < a.length; i++){
			sum += a[i] * b[i];
		}
		return sum;
	}
}
```

{{out}}
```txt
3.0
```



## JavaScript


### ES5


```javascript
function dot_product(ary1, ary2) {
    if (ary1.length != ary2.length)
        throw "can't find dot product: arrays have different lengths";
    var dotprod = 0;
    for (var i = 0; i < ary1.length; i++)
        dotprod += ary1[i] * ary2[i];
    return dotprod;
}

print(dot_product([1,3,-5],[4,-2,-1])); // ==> 3
print(dot_product([1,3,-5],[4,-2,-1,0])); // ==> exception
```


We could also use map and reduce in lieu of iteration,


```javascript
function dotp(x,y) {
    function dotp_sum(a,b) { return a + b; }
    function dotp_times(a,i) { return x[i] * y[i]; }
    if (x.length != y.length)
        throw "can't find dot product: arrays have different lengths";
    return x.map(dotp_times).reduce(dotp_sum,0);
}

dotp([1,3,-5],[4,-2,-1]); // ==> 3
dotp([1,3,-5],[4,-2,-1,0]); // ==> exception
```



### ES6

Composing functional primitives into a '''dotProduct()''' which returns '''undefined''' (rather than an error) when the array lengths are unmatched.


```JavaScript
(() => {
    'use strict';

    // dotProduct :: [Int] -> [Int] -> Int
    const dotProduct = (xs, ys) => {
        const sum = xs => xs ? xs.reduce((a, b) => a + b, 0) : undefined;

        return xs.length === ys.length ? (
            sum(zipWith((a, b) => a * b, xs, ys))
        ) : undefined;
    }

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const ny = ys.length;
        return (xs.length <= ny ? xs : xs.slice(0, ny))
            .map((x, i) => f(x, ys[i]));
    }

    return dotProduct([1, 3, -5], [4, -2, -1]);
})();
```


{{Out}}

```JavaScript>3</lang



## jq

The dot-product of two arrays, x and y, can be computed using dot(x;y) defined as follows:

```jq

def dot(x; y):
  reduce range(0;x|length) as $i (0; . + x[$i] * y[$i]);

```


Suppose however that we are given an array of objects, each of which has an "x" field and a "y" field,
and that we wish to compute SIGMA( x * y ) where the sum is taken over the array, and where x and y denote the
values in the "x" and "y" fields respectively.

This can most usefully be accomplished in jq with the aid of SIGMA(f) defined as follows:
```jq
def SIGMA( f ): reduce .[] as $o (0; . + ($o | f )) ;
```

Given the array of objects as input, the dot-product is then simply <code>SIGMA( .x * .y )</code>.

Example:
```jq
dot( [1, 3, -5]; [4, -2, -1]) # => 3

[ {"x": 1, "y": 4},  {"x": 3, "y": -2},  {"x": -5, "y": -1} ]
  | SIGMA( .x * .y ) # => 3
```



## Jsish

From Javascript ES5 imperative entry.

```javascript
/* Dot product, in Jsish */
function dot_product(ary1, ary2) {
    if (ary1.length != ary2.length) throw "can't find dot product: arrays have different lengths";
    var dotprod = 0;
    for (var i = 0; i < ary1.length; i++) dotprod += ary1[i] * ary2[i];
    return dotprod;
}

;dot_product([1,3,-5],[4,-2,-1]);
;//dot_product([1,3,-5],[4,-2,-1,0]);

/*
=!EXPECTSTART!=
dot_product([1,3,-5],[4,-2,-1]) ==> 3
dot_product([1,3,-5],[4,-2,-1,0]) ==>
PASS!: err = can't find dot product: arrays have different lengths
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U dotProduct.jsi
dot_product([1,3,-5],[4,-2,-1]) ==> 3
dot_product([1,3,-5],[4,-2,-1,0]) ==>
PASS!: err = can't find dot product: arrays have different lengths

prompt$ jsish -u dotProduct.jsi
[PASS] dotProduct.jsi
```



## Julia

Dot products and many other linear-algebra functions are built-in functions in Julia (and are largely implemented by calling functions from [[wp:LAPACK|LAPACK]]).

```julia
x = [1, 3, -5]
y = [4, -2, -1]
z = dot(x, y)
z = x'*y
```



## K


```K
   +/1 3 -5 * 4 -2 -1
3

   1 3 -5 _dot 4 -2 -1
3
```



## Kotlin

{{works with|Kotlin|1.0+}}

```scala
fun dot(v1: Array<Double>, v2: Array<Double>) =
    v1.zip(v2).map { it.first * it.second }.reduce { a, b -> a + b }

fun main(args: Array<String>) {
    dot(arrayOf(1.0, 3.0, -5.0), arrayOf(4.0, -2.0, -1.0)).let { println(it) }
}
```

{{out}}

```txt
3.0
```



## LFE


```lisp
(defun dot-product (a b)
  (: lists foldl #'+/2 0
    (: lists zipwith #'*/2 a b)))

```



## Liberty BASIC


```lb
vectorA$ = "1, 3, -5"
vectorB$ = "4, -2, -1"
print "DotProduct of ";vectorA$;" and "; vectorB$;" is ";
print DotProduct(vectorA$, vectorB$)

'arbitrary length
vectorA$ = "3, 14, 15, 9, 26"
vectorB$ = "2, 71, 18, 28, 1"
print "DotProduct of ";vectorA$;" and "; vectorB$;" is ";
print DotProduct(vectorA$, vectorB$)

end

function DotProduct(a$, b$)
    DotProduct = 0
    i = 1
    while 1
        x$=word$( a$, i, ",")
        y$=word$( b$, i, ",")
        if x$="" or y$="" then exit function
        DotProduct = DotProduct + val(x$)*val(y$)
        i = i+1
    wend
end function
```



## LLVM


```llvm
; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; Additional comments have been inserted, as well as changes made from the output produced by clang such as putting more meaningful labels for the jumps

;--- The declarations for the external C functions
declare i32 @printf(i8*, ...)

$"INTEGER_FORMAT" = comdat any

@main.a = private unnamed_addr constant [3 x i32] [i32 1, i32 3, i32 -5], align 4
@main.b = private unnamed_addr constant [3 x i32] [i32 4, i32 -2, i32 -1], align 4
@"INTEGER_FORMAT" = linkonce_odr unnamed_addr constant [4 x i8] c"%d\0A\00", comdat, align 1

; Function Attrs: noinline nounwind optnone uwtable
define i32 @dot_product(i32*, i32*, i64) #0 {
  %4 = alloca i64, align 8                              ;-- allocate copy of n
  %5 = alloca i32*, align 8                             ;-- allocate copy of b
  %6 = alloca i32*, align 8                             ;-- allocate copy of a
  %7 = alloca i32, align 4                              ;-- allocate sum
  %8 = alloca i64, align 8                              ;-- allocate i
  store i64 %2, i64* %4, align 8                        ;-- store a copy of n
  store i32* %1, i32** %5, align 8                      ;-- store a copy of b
  store i32* %0, i32** %6, align 8                      ;-- store a copy of a
  store i32 0, i32* %7, align 4                         ;-- store 0 in sum
  store i64 0, i64* %8, align 8                         ;-- store 0 in i
  br label %loop

loop:
  %9 = load i64, i64* %8, align 8                       ;-- load i
  %10 = load i64, i64* %4, align 8                      ;-- load n
  %11 = icmp ult i64 %9, %10                            ;-- i < n
  br i1 %11, label %loop_body, label %exit

loop_body:
  %12 = load i32*, i32** %6, align 8                    ;-- load a
  %13 = load i64, i64* %8, align 8                      ;-- load i
  %14 = getelementptr inbounds i32, i32* %12, i64 %13   ;-- calculate a[i]
  %15 = load i32, i32* %14, align 4                     ;-- load a[i]

  %16 = load i32*, i32** %5, align 8                    ;-- load b
  %17 = load i64, i64* %8, align 8                      ;-- load i
  %18 = getelementptr inbounds i32, i32* %16, i64 %17   ;-- calculate b[i]
  %19 = load i32, i32* %18, align 4                     ;-- load b[i]

  %20 = mul nsw i32 %15, %19                            ;-- temp = a[i] * b[i]

  %21 = load i32, i32* %7, align 4                      ;-- load sum
  %22 = add nsw i32 %21, %20                            ;-- add sum and temp
  store i32 %22, i32* %7, align 4                       ;-- store sum

  %23 = load i64, i64* %8, align 8                      ;-- load i
  %24 = add i64 %23, 1                                  ;-- increment i
  store i64 %24, i64* %8, align 8                       ;-- store i
  br label %loop

exit:
  %25 = load i32, i32* %7, align 4                      ;-- load sum
  ret i32 %25                                           ;-- return sum
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca [3 x i32], align 4                        ;-- allocate a
  %2 = alloca [3 x i32], align 4                        ;-- allocate b

  %3 = bitcast [3 x i32]* %1 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %3, i8* bitcast ([3 x i32]* @main.a to i8*), i64 12, i32 4, i1 false)

  %4 = bitcast [3 x i32]* %2 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %4, i8* bitcast ([3 x i32]* @main.b to i8*), i64 12, i32 4, i1 false)

  %5 = getelementptr inbounds [3 x i32], [3 x i32]* %2, i32 0, i32 0
  %6 = getelementptr inbounds [3 x i32], [3 x i32]* %1, i32 0, i32 0
  %7 = call i32 @dot_product(i32* %6, i32* %5, i64 3)

  %8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @"INTEGER_FORMAT", i32 0, i32 0), i32 %7)
  ret i32 0
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i32, i1) #1

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
```

{{out}}

```txt
3
```



## Logo


```logo
to dotprod :a :b
  output apply "sum (map "product :a :b)
end

show dotprod [1 3 -5] [4 -2 -1]    ; 3
```



## Logtalk


```logtalk
dot_product(A, B, Sum) :-
    dot_product(A, B, 0, Sum).

dot_product([], [], Sum, Sum).
dot_product([A| As], [B| Bs], Acc, Sum) :-
    Acc2 is Acc + A*B,
    dot_product(As, Bs, Acc2, Sum).
```



## Lua


```lua
function dotprod(a, b)
  local ret = 0
  for i = 1, #a do
    ret = ret + a[i] * b[i]
  end
  return ret
end

print(dotprod({1, 3, -5}, {4, -2, 1}))
```



## M2000 Interpreter


```M2000 Interpreter

Module dot_product {
      A=(1,3,-5)
      B=(4,-2,-1)
      Function Dot(a, b) {
            if len(a)<>len(b) Then Error "not same length"
            if len(a)=0 then Error "empty vectors"
            Let a1=each(a), b1=each(b), sum=0
            While a1, b1 {sum+=array(a1)*array(b1)}
            =sum
      }
      Print Dot(A, B)
      Print Dot((1,3,-5), (4,-2,-1))
}
Module dot_product

```




## Maple

Between Arrays, Vectors, or Matrices you can use the dot operator:

```Maple
<1,2,3> . <4,5,6>
```


```Maple
Array([1,2,3]) . Array([4,5,6])
```


Between any of the above or lists, you can use the <code>LinearAlgebra[DotProduct]</code> function:

```Maple
LinearAlgebra( <1,2,3>, <4,5,6> )
```


```Maple
LinearAlgebra( Array([1,2,3]), Array([4,5,6]) )
```


```Maple
LinearAlgebra([1,2,3], [4,5,6] )
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
{1,3,-5}.{4,-2,-1}
```



## MATLAB

The dot product operation is a built-in function that operates on vectors of arbitrary length.

```matlab
A = [1 3 -5]
B = [4 -2 -1]
C = dot(A,B)
```

For the Octave implimentation:

```matlab
function C = DotPro(A,B)
  C = sum( A.*B );
end
```



## Maxima


```maxima
[1, 3, -5] . [4, -2, -1];
/* 3 */
```



## Mercury

This will cause a software_error/1 exception if the lists are of different lengths.

```mercury
:- module dot_product.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

main(!IO) :-
    io.write_int([1, 3, -5] `dot_product` [4, -2, -1], !IO),
    io.nl(!IO).

:- func dot_product(list(int), list(int)) = int.

dot_product(As, Bs) =
    list.foldl_corresponding((func(A, B, Acc) = Acc + A * B), As, Bs, 0).
```


=={{header|МК-61/52}}==
<lang>С/П	*	ИП0	+	П0	С/П	БП	00
```


''Input'': В/О x<sub>1</sub> С/П x<sub>2</sub> С/П y<sub>1</sub> С/П y<sub>2</sub> С/П ...

=={{header|Modula-2}}==

```modula2
MODULE DotProduct;
FROM RealStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE Vector =
    RECORD
        x,y,z : REAL
    END;

PROCEDURE DotProduct(u,v : Vector) : REAL;
BEGIN
    RETURN u.x*v.x + u.y*v.y + u.z*v.z
END DotProduct;

VAR
    buf : ARRAY[0..63] OF CHAR;
    dp : REAL;
BEGIN
    dp := DotProduct(Vector{1.0,3.0,-5.0},Vector{4.0,-2.0,-1.0});
    RealToStr(dp, buf);
    WriteString(buf);
    WriteLn;

    ReadChar
END DotProduct.
```



## MUMPS


```MUMPS
DOTPROD(A,B)
 ;Returns the dot product of two vectors. Vectors are assumed to be stored as caret-delimited strings of numbers.
 ;If the vectors are not of equal length, a null string is returned.
 QUIT:$LENGTH(A,"^")'=$LENGTH(B,"^") ""
 NEW I,SUM
 SET SUM=0
 FOR I=1:1:$LENGTH(A,"^") SET SUM=SUM+($PIECE(A,"^",I)*$PIECE(B,"^",I))
 KILL I
 QUIT SUM
```



## Nemerle

This will cause an exception if the arrays are different lengths.

```Nemerle
using System;
using System.Console;
using Nemerle.Collections.NCollectionsExtensions;

module DotProduct
{
    DotProduct(x : array[int], y : array[int]) : int
    {
        $[(a * b)|(a, b) in ZipLazy(x, y)].FoldLeft(0, _+_);
    }

    Main() : void
    {
        def arr1 = array[1, 3, -5]; def arr2 = array[4, -2, -1];
        WriteLine(DotProduct(arr1, arr2));
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

whatsTheVectorVictor = [[double 1.0, 3.0, -5.0], [double 4.0, -2.0, -1.0]]
dotProduct = Rexx dotProduct(whatsTheVectorVictor)
say dotProduct.format(null, 2)

return

method dotProduct(vec1 = double[], vec2 = double[]) public constant returns double signals IllegalArgumentException
  if vec1.length \= vec2.length then signal IllegalArgumentException('Vectors must be the same length')

  scalarProduct = double 0.0
  loop e_ = 0 to vec1.length - 1
    scalarProduct = vec1[e_] * vec2[e_] + scalarProduct
    end e_

  return scalarProduct

method dotProduct(vecs = double[,]) public constant returns double signals IllegalArgumentException
  return dotProduct(vecs[0], vecs[1])
```



## newLISP


```newLISP
(define (dot-product x y)
  (apply + (map * x y)))

(println (dot-product '(1 3 -5) '(4 -2 -1)))
```



## Nim


```nim
# Compile time error when a and b are differently sized arrays
# Runtime error when a and b are differently sized seqs
proc dotp[T](a,b: T): int =
  assert a.len == b.len
  for i in a.low..a.high:
    result += a[i] * b[i]

echo dotp([1,3,-5], [4,-2,-1])
echo dotp(@[1,2,3],@[4,5,6])
```


=={{header|Oberon-2}}==
{{Works with|oo2c version 2}}

```oberon2

MODULE DotProduct;
IMPORT
  Out := NPCT:Console;

VAR
  x,y: ARRAY 3 OF LONGINT;

PROCEDURE DotProduct(a,b: ARRAY OF LONGINT): LONGINT;
VAR
  resp, i: LONGINT;
BEGIN
  ASSERT(LEN(a) = LEN(b));
  resp := 0;
  FOR i := 0 TO LEN(x) - 1 DO
    INC(resp,x[i]*y[i])
  END;
  RETURN resp
END DotProduct;

BEGIN
  x[0] := 1;y[0] := 4;
  x[1] := 3;y[1] := -2;
  x[2] := -5;y[2] := -1;
  Out.Int(DotProduct(x,y),0);Out.Ln
END DotProduct.

```

{{out}}

```txt

3

```


=={{header|Objective-C}}==

```objc>#import <stdio.h

#import <stdint.h>
#import <stdlib.h>
#import <string.h>
#import <Foundation/Foundation.h>

// this class exists to return a result between two
// vectors: if vectors have different "size", valid
// must be NO
@interface VResult : NSObject
{
 @private
  double value;
  BOOL valid;
}
+(instancetype)new: (double)v isValid: (BOOL)y;
-(instancetype)init: (double)v isValid: (BOOL)y;
-(BOOL)isValid;
-(double)value;
@end

@implementation VResult
+(instancetype)new: (double)v isValid: (BOOL)y
{
  return [[self alloc] init: v isValid: y];
}
-(instancetype)init: (double)v isValid: (BOOL)y
{
  if ((self == [super init])) {
    value = v;
    valid = y;
  }
  return self;
}
-(BOOL)isValid { return valid; }
-(double)value { return value; }
@end


@interface RCVector : NSObject
{
 @private
  double *vec;
  uint32_t size;
}
+(instancetype)newWithArray: (double *)v ofLength: (uint32_t)l;
-(instancetype)initWithArray: (double *)v ofLength: (uint32_t)l;
-(VResult *)dotProductWith: (RCVector *)v;
-(uint32_t)size;
-(double *)array;
-(void)free;
@end

@implementation RCVector
+(instancetype)newWithArray: (double *)v ofLength: (uint32_t)l
{
  return [[self alloc] initWithArray: v ofLength: l];
}
-(instancetype)initWithArray: (double *)v ofLength: (uint32_t)l
{
  if ((self = [super init])) {
    size = l;
    vec = malloc(sizeof(double) * l);
    if ( vec == NULL )
      return nil;
    memcpy(vec, v, sizeof(double)*l);
  }
  return self;
}
-(void)dealloc
{
  free(vec);
}
-(uint32_t)size { return size; }
-(double *)array { return vec; }
-(VResult *)dotProductWith: (RCVector *)v
{
  double r = 0.0;
  uint32_t i, s;
  double *v1;
  if ( [self size] != [v size] ) return [VResult new: r isValid: NO];
  s = [self size];
  v1 = [v array];
  for(i = 0; i < s; i++) {
    r += vec[i] * v1[i];
  }
  return [VResult new: r isValid: YES];
}
@end

double val1[] = { 1, 3, -5 };
double val2[] = { 4,-2, -1 };

int main()
{
  @autoreleasepool {
    RCVector *v1 = [RCVector newWithArray: val1 ofLength: sizeof(val1)/sizeof(double)];
    RCVector *v2 = [RCVector newWithArray: val2 ofLength: sizeof(val1)/sizeof(double)];
    VResult *r = [v1 dotProductWith: v2];
    if ( [r isValid] ) {
      printf("%lf\n", [r value]);
    } else {
      fprintf(stderr, "length of vectors differ\n");
    }
  }
  return 0;
}
```



## Objeck


```objeck
bundle Default {
  class DotProduct {
    function : Main(args : String[]) ~ Nil {
      DotProduct([1, 3, -5], [4, -2, -1])->PrintLine();
    }

    function : DotProduct(array_a : Int[], array_b : Int[]) ~ Int {
      if(array_a = Nil) {
        return 0;
      };

      if(array_b = Nil) {
        return 0;
      };

      if(array_a->Size() <> array_b->Size()) {
        return 0;
      };

      val := 0;
      for(x := 0; x < array_a->Size(); x += 1;) {
        val += (array_a[x] * array_b[x]);
      };

      return val;
    }
  }
}
```



## OCaml

With lists:

```ocaml
let dot = List.fold_left2 (fun z x y -> z +. x *. y) 0.

(*
# dot [1.0; 3.0; -5.0] [4.0; -2.0; -1.0];;
- : float = 3.
*)
```


With arrays:

```ocaml
let dot v u =
  if Array.length v <> Array.length u
  then invalid_arg "Different array lengths";
  let times v u =
    Array.mapi (fun i v_i -> v_i *. u.(i)) v
  in Array.fold_left (+.) 0. (times v u)

(*
# dot [| 1.0; 3.0; -5.0 |] [| 4.0; -2.0; -1.0 |];;
- : float = 3.
*)
```



## Octave

See [[Dot product#MATLAB]] for an implementation. If we have a row-vector and a column-vector, we can use simply *.

```octave
a = [1, 3, -5]
b = [4, -2, -1] % or [4; -2; -1] and avoid transposition with '
disp( a * b' )  % ' means transpose
```



## Oforth



```Oforth
: dotProduct  zipWith(#*) sum ;
```


{{out}}

```txt

>[ 1, 3, -5] [ 4, -2, -1 ] dotProduct .
3

```



## Ol


```scheme

(define (dot-product a b)
  (apply + (map * a b)))

(print (dot-product '(1 3 -5) '(4 -2 -1)))
; ==> 3

```



## Oz

Vectors are represented as lists in this example.

```oz
declare
  fun {DotProduct Xs Ys}
     {Length Xs} = {Length Ys} %% assert
     {List.foldL {List.zip Xs Ys Number.'*'} Number.'+' 0}
  end
in
  {Show {DotProduct [1 3 ~5] [4 ~2 ~1]}}
```



## PARI/GP


```parigp
dot(u,v)={
  sum(i=1,#u,u[i]*v[i])
};
```



## Pascal

See [[Dot_product#Delphi | Delphi]]


## Perl


```perl
sub dotprod
{
        my($vec_a, $vec_b) = @_;
        die "they must have the same size\n" unless @$vec_a == @$vec_b;
        my $sum = 0;
        $sum += $vec_a->[$_] * $vec_b->[$_] for 0..$#$vec_a;
        return $sum;
}

my @vec_a = (1,3,-5);
my @vec_b = (4,-2,-1);

print dotprod(\@vec_a,\@vec_b), "\n"; # 3
```



## Perl 6

{{works with|Rakudo|2010.07}}
We use the square-bracket meta-operator to turn the infix operator <code>+</code> into a reducing list operator, and the guillemet meta-operator to vectorize the infix operator <code>*</code>.  Length validation is automatic in this form.

```perl6
say [+] (1, 3, -5) »*« (4, -2, -1);
```



## Phix


```Phix
?sum(sq_mul({1,3,-5},{4,-2,-1}))
```

{{out}}

```txt

3

```



## PHP


```php
<?php
function dot_product($v1, $v2) {
  if (count($v1) != count($v2))
    throw new Exception('Arrays have different lengths');
  return array_sum(array_map('bcmul', $v1, $v2));
}

echo dot_product(array(1, 3, -5), array(4, -2, -1)), "\n";
?>
```



## PicoLisp


```PicoLisp
(de dotProduct (A B)
   (sum * A B) )

(dotProduct (1 3 -5) (4 -2 -1))
```

{{out}}
```txt
-> 3
```



## PL/I


```PL/I
get (n);
begin;
   declare (A(n), B(n)) float;
   declare dot_product float;

   get list (A);
   get list (B);
   dot_product = sum(a*b);
   put (dot_product);
end;
```



## PostScript


```postscript
/dotproduct{
/x exch def
/y exch def
/sum 0 def
/i 0 def
x length y length eq %Check if both arrays have the same length
{
x length{
/sum x i get y i get mul sum add def
/i i 1 add def
}repeat
sum ==
}
{
-1 ==
}ifelse
}def
```



## PowerShell


```PowerShell

function dotproduct( $a, $b) {
    $a | foreach -Begin {$i = $res = 0} -Process { $res += $_*$b[$i++] } -End{$res}
}
dotproduct (1..2) (1..2)
dotproduct (1..10) (11..20)

```

<b>Output:</b>

```txt

5
935

```



## Prolog

Works with SWI-Prolog.

```Prolog
dot_product(L1, L2, N) :-
	maplist(mult, L1, L2, P),
	sumlist(P, N).

mult(A,B,C) :-
	C is A*B.
```

Example :

```txt
 ?- dot_product([1,3,-5], [4,-2,-1], N).
N = 3.
```



## PureBasic


```PureBasic
Procedure dotProduct(Array a(1),Array b(1))
  Protected i, sum, length = ArraySize(a())

  If ArraySize(a()) = ArraySize(b())
    For i = 0 To length
      sum + a(i) * b(i)
    Next
  EndIf

  ProcedureReturn sum
EndProcedure

If OpenConsole()
  Dim a(2)
  Dim b(2)

  a(0) = 1 : a(1) = 3 : a(2) = -5
  b(0) = 4 : b(1) = -2 : b(2) = -1

  PrintN(Str(dotProduct(a(),b())))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```



## Python


```python
def dotp(a,b):
    assert len(a) == len(b), 'Vector sizes must match'
    return sum(aterm * bterm for aterm,bterm in zip(a, b))

if __name__ == '__main__':
    a, b = [1, 3, -5], [4, -2, -1]
    assert dotp(a,b) == 3
```



Option types can provide a composable alternative to assertions and error-handling.
Here is an example of an '''Either''' type, which returns either a computed value (in a '''Right''' wrapping), or an explanatory string (in a '''Left''' wrapping).

A higher order '''either''' function can apply one of two supplied functions to an Either value - one for Left Either values, and one for Right Either values:

{{Works with|Python|3.7}}

```python
'''Dot product'''

from operator import (mul)


# dotProduct :: Num a => [a] -> [a] -> Either String a
def dotProduct(xs):
    '''Either the dot product of xs and ys,
       or a string reporting unmatched vector sizes.
    '''
    return lambda ys: Left('vector sizes differ') if (
        len(xs) != len(ys)
    ) else Right(sum(map(mul, xs, ys)))


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Dot product of other vectors with [1, 3, -5]'''

    print(
        fTable(main.__doc__ + ':\n')(str)(str)(
            compose(
                either(append('Undefined :: '))(str)
            )(dotProduct([1, 3, -5]))
        )([[4, -2, -1, 8], [4, -2], [4, 2, -1], [4, -2, -1]])
    )


# GENERIC -------------------------------------------------

# Left :: a -> Either a b
def Left(x):
    '''Constructor for an empty Either (option type) value
       with an associated string.
    '''
    return {'type': 'Either', 'Right': None, 'Left': x}


# Right :: b -> Either a b
def Right(x):
    '''Constructor for a populated Either (option type) value'''
    return {'type': 'Either', 'Left': None, 'Right': x}


# append (++) :: [a] -> [a] -> [a]
# append (++) :: String -> String -> String
def append(xs):
    '''Two lists or strings combined into one.'''
    return lambda ys: xs + ys


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# either :: (a -> c) -> (b -> c) -> Either a b -> c
def either(fl):
    '''The application of fl to e if e is a Left value,
       or the application of fr to e if e is a Right value.
    '''
    return lambda fr: lambda e: fl(e['Left']) if (
        None is e['Right']
    ) else fr(e['Right'])


# FORMATTING ----------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Dot product of other vectors with [1, 3, -5]:

[4, -2, -1, 8] -> Undefined :: vector sizes differ
       [4, -2] -> Undefined :: vector sizes differ
    [4, 2, -1] -> 15
   [4, -2, -1] -> 3
```



## R

Here are several ways to do the task.

```R
x <- c(1, 3, -5)
y <- c(4, -2, -1)

sum(x*y)  # compute products, then do the sum
x %*% y   # inner product

# loop implementation
dotp <- function(x, y) {
	n <- length(x)
	if(length(y) != n) stop("invalid argument")
	s <- 0
	for(i in 1:n) s <- s + x[i]*y[i]
	s
}

dotp(x, y)
```



## Racket


```Racket

#lang racket
(define (dot-product l r) (for/sum ([x l] [y r]) (* x y)))

(dot-product '(1 3 -5) '(4 -2 -1))

;; dot-product works on sequences such as vectors:
(dot-product #(1 2 3) #(4 5 6))

```



## Rascal



```Rascal
import List;

public int dotProduct(list[int] L, list[int] M){
	result = 0;
	if(size(L) == size(M)) {
		while(size(L) >= 1) {
		    result += (head(L) * head(M));
		    L = tail(L);
		    M = tail(M);
	        }
	        return result;
	}
	else {
		throw "vector sizes must match";
	}
}
```



### Alternative solution

If a matrix is represented by a relation of <x-coordinate, y-coordinate, value>, then function below can be used to find the Dot product.

```Rascal
import Prelude;

public real matrixDotproduct(rel[real x, real y, real v] column1, rel[real x, real y, real v] column2){
	return (0.0 | it + v1*v2 | <x1,y1,v1> <- column1, <x2,y2,v2> <- column2, y1==y2);
}

//a matrix, given by a relation of x-coordinate, y-coordinate, value.
public rel[real x, real y, real v] matrixA = {
<0.0,0.0,12.0>, <0.0,1.0, 6.0>, <0.0,2.0,-4.0>,
<1.0,0.0,-51.0>, <1.0,1.0,167.0>, <1.0,2.0,24.0>,
<2.0,0.0,4.0>, <2.0,1.0,-68.0>, <2.0,2.0,-41.0>
};
```



## REBOL



```REBOL
REBOL []

a: [1 3 -5]
b: [4 -2 -1]

dot-product: function [v1 v2] [sum] [
    if (length? v1) != (length? v2) [
        make error! "error: vector sizes must match"
    ]
    sum: 0
    repeat i length? v1 [
        sum: sum + ((pick v1 i) * (pick v2 i))
    ]
]

dot-product a b
```



## REXX


### no error checking


```rexx
/*REXX program  computes  the   dot product   of  two equal size vectors  (of any size).*/
                     vectorA =  '  1   3  -5  '  /*populate vector  A  with some numbers*/
                     vectorB =  '  4  -2  -1  '  /*    "       "    B    "    "     "   */
say  'vector A = '   vectorA                     /*display the elements in the vector A.*/
say  'vector B = '   vectorB                     /*   "     "     "      "  "    "    B.*/
p=.Prod(vectorA, vectorB)                        /*invoke function & compute dot product*/
say                                              /*display a blank line for readability.*/
say  'dot product = '   p                        /*display the dot product to terminal. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
.Prod:  procedure;  parse arg A,B                /*this function compute the dot product*/
        $=0                                      /*initialize the sum to  0 (zero).     */
                    do j=1  for words(A)         /*multiply each number in the vectors. */
                    $=$+word(A,j) * word(B,j)    /*  ··· and add the product to the sum.*/
                    end   /*j*/
        return $                                 /*return the sum to function's invoker.*/
```

'''output'''   using the default (internal) inputs:

```txt

vector A =   1   3  -5
vector B =   4  -2  -1

dot product =  3

```



### with error checking


```rexx
/*REXX program  computes  the   dot product   of  two equal size vectors  (of any size).*/
                     vectorA =  '  1   3  -5  '  /*populate vector  A  with some numbers*/
                     vectorB =  '  4  -2  -1  '  /*    "       "    B    "    "     "   */
say  'vector A = '   vectorA                     /*display the elements in the vector A.*/
say  'vector B = '   vectorB                     /*   "     "     "      "  "    "    B.*/
p=.prod(vectorA, vectorB)                        /*invoke function & compute dot product*/
say                                              /*display a blank line for readability.*/
say  'dot product = '   p                        /*display the dot product to terminal. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
.prod: procedure;  parse arg A,B                 /*this function compute the dot product*/
       lenA = words(A);           @.1= 'A'       /*the number of numbers in vector  A.  */
       lenB = words(B);           @.2= 'B'       /* "     "    "    "     "    "    B.  */
                                                 /*Also, define 2 literals to hold names*/
       if lenA\==lenB  then do;   say "***error*** vectors aren't the same size:" /*oops*/
                                  say '            vector  A  length = '   lenA
                                  say '            vector  B  length = '   lenB
                                  exit 13        /*exit pgm with bad─boy return code 13.*/
                            end
       $=0                                       /*initialize the  sum  to   0  (zero). */
                 do j=1  for lenA                /*multiply each number in the vectors. */
                 #.1=word(A,j)                   /*use array to hold 2 numbers at a time*/
                 #.2=word(B,j)
                                  do k=1  for 2;   if datatype(#.k,'N')  then iterate
                                  say "***error*** vector "      @.k      ' element'    j,
                                      " isn't numeric: "     n.k;                  exit 13
                                  end   /*k*/
                 $=$ + #.1 * #.2                 /*  ··· and add the product to the sum.*/
                 end      /*j*/
       return $                                  /*return the sum to function's invoker.*/
```

'''output'''   is the same as the 1<sup>st</sup> REXX version.




## Ring


```ring

aVector = [2, 3, 5]
bVector = [4, 2, 1]
sum = 0
see dotProduct(aVector, bVector)

func dotProduct cVector, dVector
     for n = 1 to len(aVector)
         sum = sum + cVector[n] * dVector[n]
     next
     return sum

```



## RLaB

In its simplest form dot product is a composition of two functions: element-by-element
multiplication '.*' followed by sumation of an array. Consider an example:

```RLaB
x = rand(1,10);
y = rand(1,10);
s = sum( x .* y );
```

Warning: element-by-element multiplication is matrix optimized. As the interpretation of the matrix
optimization is quite general, and unique to RLaB, any two matrices can be so multiplied irrespective
of their dimensions. It is up to user to check whether in his/her case the matrix optimization needs to
be restricted, and then to implement restrictions in his/her code.


## RPL

Being a language for a calculator, RPL makes this easy.

```RPL
<<
  [ 1  3 -5 ]
  [ 4 -2 -1 ]
  DOT
>>
```



## Ruby

With the '''standard library''', require 'matrix' and call Vector#inner_product.

```ruby
irb(main):001:0> require 'matrix'
=> true
irb(main):002:0> Vector[1, 3, -5].inner_product Vector[4, -2, -1]
=> 3
```

Or '''implement''' dot product.

```ruby
class Array
  def dot_product(other)
    raise "not the same size!" if self.length != other.length
    self.zip(other).inject(0) {|dp, (a, b)| dp += a*b}
  end
end

p [1, 3, -5].dot_product [4, -2, -1]   # => 3
```




## Run BASIC


```runbasic
v1$ = "1, 3, -5"
v2$ = "4, -2, -1"

print "DotProduct of ";v1$;" and "; v2$;" is ";dotProduct(v1$,v2$)
end

function dotProduct(a$, b$)
    while word$(a$,i + 1,",") <> ""
       i = i + 1
       v1$=word$(a$,i,",")
       v2$=word$(b$,i,",")
       dotProduct = dotProduct + val(v1$) * val(v2$)
    wend
end function
```



## Rust

Implemented as a simple function with check for equal length of vectors.

```rust
// alternatively, fn dot_product(a: &Vec<u32>, b: &Vec<u32>)
// but using slices is more general and rustic
fn dot_product(a: &[i32], b: &[i32]) -> Option<i32> {
    if a.len() != b.len() { return None }
    Some(
        a.iter()
            .zip( b.iter() )
            .fold(0, |sum, (el_a, el_b)| sum + el_a*el_b)
    )
}


fn main() {
    let v1 = vec![1, 3, -5];
    let v2 = vec![4, -2, -1];

    println!("{}", dot_product(&v1, &v2).unwrap());
}
```



Alternatively as a very generic function which works for any two types that can be multiplied to result in a third type which can be added with itself. Works with any argument convertible to an Iterator of known length (ExactSizeIterator).

'''Uses an unstable feature.'''

```rust
#![feature(zero_one)] // <-- unstable feature
use std::ops::{Add, Mul};
use std::num::Zero;

fn dot_product<T1, T2, U, I1, I2>(lhs: I1, rhs: I2) -> Option<U>
    where T1: Mul<T2, Output = U>,
          U: Add<U, Output = U> + Zero,
          I1: IntoIterator<Item = T1>,
          I2: IntoIterator<Item = T2>,
          I1::IntoIter: ExactSizeIterator,
          I2::IntoIter: ExactSizeIterator,
{
    let (iter_lhs, iter_rhs) = (lhs.into_iter(), rhs.into_iter());
    match (iter_lhs.len(), iter_rhs.len()) {
        (0, _) | (_, 0) => None,
        (a,b) if a != b => None,
        (_,_) => Some( iter_lhs.zip(iter_rhs)
           .fold(U::zero(), |sum, (a, b)| sum + (a * b)) )
    }
}



fn main() {
    let v1 = vec![1, 3, -5];
    let v2 = vec![4, -2, -1];

    println!("{}", dot_product(&v1, &v2).unwrap());
}
```



=={{header|S-lang}}==
<lang S-lang>print(sum([1, 3, -5] * [4, -2, -1]));
```

{{out}}

```txt
3.0
```


[sum() returns a double from integer arrays]


## Sather

Built-in class VEC "implements" euclidean (geometric) vectors.

```sather
class MAIN is
  main is
    x ::= #VEC(|1.0, 3.0, -5.0|);
    y ::= #VEC(|4.0, -2.0, -1.0|);
    #OUT + x.dot(y) + "\n";
  end;
end;
```



## Scala

{{libheader|Scala}}
```scala
class Dot[T](v1: Seq[T])(implicit n: Numeric[T]) {
  import n._ // import * operator
  def dot(v2: Seq[T]) = {
    require(v1.size == v2.size)
    (v1 zip v2).map{ Function.tupled(_ * _)}.sum
  }
}

object Main extends App {
  implicit def toDot[T: Numeric](v1: Seq[T]) = new Dot(v1)

  val v1 = List(1, 3, -5)
  val v2 = List(4, -2, -1)
  println(v1 dot v2)
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";

$ syntax expr: .().dot.() is  -> 6;  # priority of dot operator

const func integer: (in array integer: a) dot (in array integer: b) is func
  result
    var integer: sum is 0;
  local
    var integer: index is 0;
  begin
    if length(a) <> length(b) then
      raise RANGE_ERROR;
    else
      for index range 1 to length(a) do
        sum +:= a[index] * b[index];
      end for;
    end if;
  end func;

const proc: main is func
  begin
    writeln([](1, 3, -5) dot [](4, -2, -1));
  end func;
```



## Sidef


```ruby
func dot_product(a, b) {
    (a »*« b)«+»;
};
say dot_product([1,3,-5], [4,-2,-1]);   # => 3
```



## Scheme

{{Works with|Scheme|R<math>^5</math>RS}}

```scheme
(define (dot-product a b)
  (apply + (map * a b)))

(display (dot-product '(1 3 -5) '(4 -2 -1)))
(newline)
```

{{out}}
```txt
3
```



## Scilab


```Scilab
A = [1 3 -5]
B = [4 -2 -1]
C = sum(A.*B)
```



## Slate


```slate
v@(Vector traits) <dot> w@(Vector traits)
"Dot-product."
[
  (0 below: (v size min: w size)) inject: 0 into:
    [| :sum :index | sum + ((v at: index) * (w at: index))]
].
```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
Array extend
[
  * anotherArray [
       |acc| acc := 0.
       self with: anotherArray collect: [ :a :b |
          acc := acc + ( a * b )
       ].
       ^acc
  ]
]

( #(1 3 -5) * #(4 -2 -1 ) ) printNl.
```



## SNOBOL4


```snobol4
        define("dotp(a,b)sum,i")        :(dotp_end)
dotp    i = 1; sum = 0
loop    sum = sum + (a<i> * b<i>)
        i = i + 1 ?a<i> :s(loop)
        dotp = sum      :(return)
dotp_end

        a = array(3); a<1> = 1; a<2> = 3; a<3> = -5;
        b = array(3); b<1> = 4; b<2> = -2; b<3> = -1;
        output = dotp(a,b)
end
```



## SPARK

Works with SPARK GPL 2010 and GPS GPL 2010.

By defining numeric subtypes with suitable ranges we can prove statically that there will be no run-time errors. (The Simplifier leaves 2 VCs unproven, but these are clearly provable by inspection.)

The precondition enforces equality of the ranges of the two vectors.

```ada
with Spark_IO;
--# inherit Spark_IO;
--# main_program;
procedure Dot_Product_Main
--# global in out Spark_IO.Outputs;
--# derives Spark_IO.Outputs from *;
is
   Limit : constant := 1000;
   type V_Elem is range -Limit .. Limit;
   V_Size : constant := 100;
   type V_Index is range 1 .. V_Size;
   type Vector is array(V_Index range <>) of V_Elem;

   type V_Prod is range -(Limit**2)*V_Size .. (Limit**2)*V_Size;
   --# assert V_Prod'Base is Integer;

   subtype Index3 is V_Index range 1 .. 3;
   subtype Vector3 is Vector(Index3);
   Vect1 : constant Vector3 := Vector3'(1, 3, -5);
   Vect2 : constant Vector3 := Vector3'(4, -2, -1);

   function Dot_Product(V1, V2 : Vector) return V_Prod
   --# pre  V1'First = V2'First
   --#  and V1'Last  = V2'Last;
   is
      Sum : V_Prod := 0;
   begin
      for I in V_Index range V1'Range
      --# assert Sum in -(Limit**2)*V_Prod(I-1) .. (Limit**2)*V_Prod(I-1);
      loop
         Sum := Sum + V_Prod(V1(I)) * V_Prod(V2(I));
      end loop;
      return Sum;
   end Dot_Product;

begin
   Spark_IO.Put_Integer(File  => Spark_IO.Standard_Output,
                        Item  => Integer(Dot_Product(Vect1, Vect2)),
                        Width => 6,
                        Base  => 10);
end Dot_Product_Main;
```

{{out}}
```txt
     3
```



## SQL

ANSI sql does not support functions and is missing some other concepts that would be needed for a general case implementation of inner product (column names and tables would need to be first class in SQL -- capable of being passed to functions).

However, inner product is fairly simple to specify in SQL.

Given two tables <code>A</code> and <code>B</code> where A has key columns <code>i</code> and <code>j</code> and B has key columns <code>j</code> and <code>k</code> and both have value columns <code>N</code>, the inner product of A and B would be:

```sql
select i, k, sum(A.N*B.N) as N
        from A inner join B on A.j=B.j
        group by i, k
```



## Standard ML

With lists:

```sml
val dot = ListPair.foldlEq Real.*+ 0.0

(*
- dot ([1.0, 3.0, ~5.0], [4.0, ~2.0, ~1.0]);
val it = 3.0 : real
*)
```


With vectors:

```sml
fun dot (v, u) = (
  if Vector.length v <> Vector.length u then
    raise ListPair.UnequalLengths
  else ();
  Vector.foldli (fn (i, v_i, z) => v_i * Vector.sub (u, i) + z) 0.0 v
  )

(*
- dot (#[1.0, 3.0, ~5.0], #[4.0, ~2.0, ~1.0]);
val it = 3.0 : real
*)
```



## Stata

With row vectors:


```stata
matrix a=1,3,-5
matrix b=4,-2,-1
matrix c=a*b'
di el("c",1,1)
```


With column vectors:


```stata
matrix a=1\3\-5
matrix b=4\-2\-1
matrix c=a'*b
di el("c",1,1)
```



###  Mata

With row vectors:


```stata
a=1,3,-5
b=4,-2,-1
a*b'
```


With column vectors:


```stata
a=1\3\-5
b=4\-2\-1
a'*b
```


In both cases, one cas also write


```stata
sum(a:*b)
```



## Swift

{{works with|Swift|1.2+}}

```swift
func dot(v1: [Double], v2: [Double]) -> Double {
  return reduce(lazy(zip(v1, v2)).map(*), 0, +)
}

println(dot([1, 3, -5], [4, -2, -1]))
```

{{out}}

```txt
3.0
```



## Tcl

{{tcllib|math::linearalgebra}}

```tcl
package require math::linearalgebra

set a {1 3 -5}
set b {4 -2 -1}
set dotp [::math::linearalgebra::dotproduct $a $b]
proc pp vec {return \[[join $vec ,]\]}
puts "[pp $a] \u2219 [pp $b] = $dotp"
```

{{out}}
```txt
[1,3,-5] ∙ [4,-2,-1] = 3.0
```


=={{header|TI-83 BASIC}}==
To perform a matrix dot product on TI-83, the trick is to use lists (and not to use matrices).

```ti83b
sum({1,3,–5}*{4,–2,–1})
```

{{out}}

```txt

3

```


=={{header|TI-89 BASIC}}==

<!--lang ti89b--><pre style="font-family:'TI Uni'">dotP([1, 3, –5], [4, –2, –1])
```

{{out}}

```txt

3

```



## Ursala

A standard library function for dot products of floating point numbers exists, but a new one can be defined for integers as shown using the map operator (<code>*</code>) with the zip suffix (<code>p</code>) to construct a "zipwith" operator (<code>*p</code>), which operates on the integer <code>product</code> function. A catchable exception is thrown if the list lengths are unequal. This function is then composed (<code>+</code>) with a cumulative summation function, which is constructed from the binary <code>sum</code> function, and the reduction operator (<code>:-</code>) with <code>0</code> specified for the vacuous sum.

```Ursala
#import int

dot = sum:-0+ product*p

#cast %z

test = dot(<1,3,-5>,<4,-2,-1>)
```

{{out}}
```txt
3
```



## VBA


```vb
Private Function dot_product(x As Variant, y As Variant) As Double
    dot_product = WorksheetFunction.SumProduct(x, y)
End Function

Public Sub main()
    Debug.Print dot_product([{1,3,-5}], [{4,-2,-1}])
End Sub
```
{{out}}

```txt
 3
```


## VBScript


```vb

WScript.Echo DotProduct("1,3,-5","4,-2,-1")

Function DotProduct(vector1,vector2)
	arrv1 = Split(vector1,",")
	arrv2 = Split(vector2,",")
	If UBound(arrv1) <> UBound(arrv2) Then
		WScript.Echo "The vectors are not of the same length."
		Exit Function
	End If
	DotProduct = 0
	For i = 0 To UBound(arrv1)
		DotProduct = DotProduct + (arrv1(i) * arrv2(i))
	Next
End Function

```


{{Out}}

```txt
3
```



## Visual Basic

{{works with|Visual Basic|6}}

```vb
Option Explicit

Function DotProduct(a() As Long, b() As Long) As Long
Dim l As Long, u As Long, i As Long
  Debug.Assert DotProduct = 0 'return value automatically initialized with 0
  l = LBound(a())
  If l = LBound(b()) Then
    u = UBound(a())
    If u = UBound(b()) Then
      For i = l To u
        DotProduct = DotProduct + a(i) * b(i)
      Next i
    Exit Function
    End If
  End If
  Err.Raise vbObjectError + 123, , "invalid input"
End Function

Sub Main()
Dim a() As Long, b() As Long, x As Long
  ReDim a(2)
  a(0) = 1
  a(1) = 3
  a(2) = -5
  ReDim b(2)
  b(0) = 4
  b(1) = -2
  b(2) = -1
  x = DotProduct(a(), b())
  Debug.Assert x = 3
  ReDim Preserve a(3)
  a(3) = 10
  ReDim Preserve b(3)
  b(3) = 2
  x = DotProduct(a(), b())
  Debug.Assert x = 23
  ReDim Preserve a(4)
  a(4) = 10
  On Error Resume Next
  x = DotProduct(a(), b())
  Debug.Assert Err.Number = vbObjectError + 123
  Debug.Assert Err.Description = "invalid input"
End Sub

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Function DotProduct(a As Decimal(), b As Decimal()) As Decimal
        Return a.Zip(b, Function(x, y) x * y).Sum()
    End Function

    Sub Main()
        Console.WriteLine(DotProduct({1, 3, -5}, {4, -2, -1}))
        Console.ReadLine()
    End Sub

End Module
```

{{out}}

```txt
3
```



## Wart


```python
def (dot_product x y)
  (sum+map (*) x y)
```


<code>+</code> is punned (overloaded) here; when applied to functions it denotes composition. Also, <code>(*)</code> is used to skip infix expansion.
{{out}}
```txt
(dot_product '(1 3 -5) '(4 -2 -1))
=> 3
```



## X86 Assembly

Using FASM. Targets x64 Microsoft Windows.

```asm
format PE64 console
entry start

    include 'win64a.inc'

section '.text' code readable executable

    start:
        stdcall dotProduct, vA, vB
        invoke printf, msg_num, rax

        stdcall dotProduct, vA, vC
        invoke printf, msg_num, rax

        invoke ExitProcess, 0

    proc dotProduct vectorA, vectorB
        mov rax, [rcx]
        cmp rax, [rdx]
        je .calculate

        invoke printf, msg_sizeMismatch
        mov rax, 0
        ret

        .calculate:
        mov r8, rcx
        add r8, 8
        mov r9, rdx
        add r9, 8
        mov rcx, rax
        mov rax, 0
        mov rdx, 0

        .next:
            mov rbx, [r9]
            imul rbx, [r8]
            add rax, rbx
            add r8, 8
            add r9, 8
            loop .next

        ret
    endp

section '.data' data readable

    msg_num db "%d", 0x0D, 0x0A, 0
    msg_sizeMismatch db "Size mismatch; can't calculate.", 0x0D, 0x0A, 0

    struc Vector [symbols] {
        common
        .length dq (.end - .symbols) / 8
        .symbols dq symbols
        .end:
    }

    vA Vector 1, 3, -5
    vB Vector 4, -2, -1
    vC Vector 7, 2, 9, 0

section '.idata' import data readable writeable

    library kernel32, 'KERNEL32.DLL',\
            msvcrt, 'MSVCRT.DLL'

    include 'api/kernel32.inc'

    import  msvcrt,\
            printf, 'printf'
```

{{out}}<lang>3
Size mismatch; can't calculate.
0
```



## XPL0


```XPL0
include c:\cxpl\codes;

func DotProd(U, V, L);
int U, V, L;
int S, I;
[S:= 0;
for I:= 0 to L-1 do S:= S + U(I)*V(I);
return S;
];

[IntOut(0, DotProd([1, 3, -5], [4, -2, -1], 3));
CrLf(0);
]
```

{{out}}
```txt
3
```



## Yabasic


```Yabasic

sub sq_mul(a(), b(), c())
	local n, i

	n = arraysize(a(), 1)

	for i = 1 to n
		c(i) = a(i) * b(i)
	next i
end sub

sub sq_sum(a())
	local n, i, r

	n = arraysize(a(), 1)

	for i = 1 to n
		r = r + a(i)
	next i
	return r
end sub

dim a(3), b(3), c(3)

a(1) = 1 : a(2) = 3 : a(3) = -5
b(1) = 4 : b(2) = -2 : b(3) = -1
sq_mul(a(), b(), c())

print sq_sum(c())

```



## zkl


```zkl
fcn dotp(a,b){Utils.zipWith('*,a,b).sum()}
```

zipWith stops at the shortest of the lists
{{out}}
```txt
dotp(T(1,3,-5),T(4,-2,-1,666)) //-->3
```

If exact length is a requirement

```zkl
fcn dotp2(a,b){if(a.len()!=b.len())throw(Exception.ValueError);
   Utils.zipWith('*,a,b).sum()
}
```



## ZX Spectrum Basic


```zxbasic
10 DIM a(3): LET a(1)=1: LET a(2)=3: LET a(3)=-5
20 DIM b(3): LET b(1)=4: LET b(2)=-2: LET b(3)=-1
30 LET sum=0
40 FOR i=1 TO 3: LET sum=sum+a(i)*b(i): NEXT i
50 PRINT sum
```

[[Category:Geometry]]
