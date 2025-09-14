+++
title = "Sum and product of an array"
description = ""
date = 2019-10-18T20:23:46Z
aliases = []
[extra]
id = 2378
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "360_assembly",
  "4d",
  "acl2",
  "actionscript",
  "ada",
  "aime",
  "algol_68",
  "algol_w",
  "apl",
  "apple_script",
  "arturo",
  "autohotkey",
  "awk",
  "babel",
  "basic",
  "applesoft_basic",
  "bbc_basic",
  "bc",
  "befunge",
  "bracmat",
  "c",
  "c_sharp",
  "c_plus_plus",
  "chef",
  "clean",
  "clojure",
  "cobol",
  "coffee_script",
  "coldfusion",
  "common_lisp",
  "crystal",
  "d",
  "dc",
  "delphi",
  "e",
  "eiffel",
  "elena",
  "elixir",
  "emacs_lisp",
  "erlang",
  "euphoria",
  "factor",
  "false",
  "fantom",
  "gap",
  "gfa_basic",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "idl",
  "inform_7",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lang5",
  "lasso",
  "liberty_basic",
  "lingo",
  "livecode",
  "logo",
  "lua",
  "lucid",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "matlab",
  "maxima",
  "maxscript",
  "min",
  "mumps",
  "nemerle",
  "netrexx",
  "nial",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "ol",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "pop11",
  "postscript",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "raven",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sas",
  "sather",
  "scala",
  "scheme",
  "seed7",
  "setl",
  "sidef",
  "slate",
  "smalltalk",
  "snobol4",
  "sparkling",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "toka",
  "trith",
  "tuscript",
  "unix_shell",
  "unixpipes",
  "ursa",
  "ursala",
  "v",
  "vala",
  "vba",
  "vbscript",
  "visual_basic_net",
  "wart",
  "wdte",
  "wortel",
  "xpl0",
  "xslt",
  "zkl"
]
+++

## Task

{{task|Arithmetic operations}}[[Category:Iteration]]
Compute the sum and product of an array of integers.


## 360 Assembly


```360asm
*        Sum and product of an array  20/04/2017
SUMPROD  CSECT
         USING  SUMPROD,R15        base register
         SR     R3,R3              su=0
         LA     R5,1               pr=1
         LA     R6,1               i=1
       DO WHILE=(CH,R6,LE,=AL2((PG-A)/4))  do i=1 to hbound(a)
         LR     R1,R6                i
         SLA    R1,2                 *4
         A      R3,A-4(R1)           su=su+a(i)
         M      R4,A-4(R1)           pr=pr*a(i)
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         XDECO  R3,PG              su
         XDECO  R5,PG+12           pr
         XPRNT  PG,L'PG            print
         BR     R14                exit
A        DC     F'1',F'2',F'3',F'4',F'5',F'6',F'7',F'8',F'9',F'10'
PG       DS     CL24               buffer
         YREGS
         END    SUMPROD
```

```txt

          55     3628800

```




## 4D


```4d
ARRAY INTEGER($list;0)
For ($i;1;5)
       APPEND TO ARRAY($list;$i)
End for

$sum:=0
$product:=1
For ($i;1;Size of array($list))
   $sum:=$var+$list{$i}
   $product:=$product*$list{$i}
End for

// since 4D v13

$sum:=sum($list)

```



## ACL2


```Lisp
(defun sum (xs)
   (if (endp xs)
       0
       (+ (first xs)
          (sum (rest xs)))))

(defun prod (xs)
   (if (endp xs)
       1
       (* (first xs)
          (prod (rest xs)))))
```



## ActionScript


```actionscript
package {
	import flash.display.Sprite;

	public class SumAndProduct extends Sprite
	{
		public function SumAndProduct()
		{
			var arr:Array = [1, 2, 3, 4, 5];
			var sum:int = 0;
			var prod:int = 1;

			for (var i:int = 0; i < arr.length; i++)
			{
				sum += arr[i];
				prod *= arr[i];
			}

			trace("Sum: " + sum); // 15
			trace("Product: " + prod); // 120
		}
	}
}
```



## Ada


```ada
type Int_Array is array(Integer range <>) of Integer;

array : Int_Array := (1,2,3,4,5,6,7,8,9,10);
Sum : Integer := 0;
for I in array'range loop
   Sum := Sum + array(I);
end loop;
```

Define the product function

```ada
function Product(Item : Int_Array) return Integer is
  Prod : Integer := 1;
begin
  for I in Item'range loop
     Prod := Prod * Item(I);
  end loop;
  return Prod;
end Product;
```

This function will raise the predefined exception Constraint_Error if the product overflows the values represented by type Integer


## Aime


```aime
void
compute(integer &s, integer &p, list l)
{
    integer v;

    s = 0;
    p = 1;
    for (, v in l) {
        s += v;
        p *= v;
    }
}

integer
main(void)
{
    integer sum, product;

    compute(sum, product, list(2, 3, 5, 7, 11, 13, 17, 19));

    o_form("~\n~\n", sum, product);

    return 0;
}
```

```txt
77
9699690
```



## ALGOL 68


```algol68
main:(
  INT default upb := 3;
  MODE INTARRAY = [default upb]INT;

  INTARRAY array = (1,2,3,4,5,6,7,8,9,10);
  INT sum := 0;
  FOR i FROM LWB array TO UPB array DO
     sum +:= array[i]
  OD;

  # Define the product function #
  PROC int product = (INTARRAY item)INT:
  (
    INT prod :=1;
    FOR i FROM LWB item TO UPB item DO
       prod *:= item[i]
    OD;
    prod
  ) # int product # ;
  printf(($" Sum: "g(0)$,sum,$", Product:"g(0)";"l$,int product(array)))
)
```

```txt

 Sum: 55, Product:3628800;

```



## ALGOL W


```algolw
begin

    % computes the sum and product of intArray                               %
    % the results are returned in sum and product                            %
    % the bounds of the array must be specified in lb and ub                 %
    procedure sumAndProduct( integer array  intArray ( * )
                           ; integer value  lb, ub
                           ; integer result sum, product
                           ) ;
    begin

        sum     := 0;
        product := 1;

        for i := lb until ub
        do begin
            sum     :=     sum + intArray( i );
            product := product * intArray( i );
        end for_i ;

    end sumAndProduct ;

    % test the sumAndProduct procedure                                       %
    begin

        integer array v   ( 1 :: 10 );
        integer sum, product;

        for i := 1 until 10 do v( i ) := i;

        sumAndProduct( v, 1, 10, sum, product );
        write( sum, product );
    end
end.
```


```txt

            55         3628800

```



## APL

```apl
      sum  ←  +/
      prod ←  ×/

      list ←  1 2 3 4 5

      sum  list
15

      prod list
120
```


## AppleScript


```applescript
set array to {1, 2, 3, 4, 5}
set sum to 0
set product to 1
repeat with i in array
    set sum to sum + i
    set product to product * i
end repeat
```


Or, using an AppleScript implementation of '''fold'''/'''reduce''':


```AppleScript
on summed(a, b)
    a + b
end summed

on product(a, b)
    a * b
end product

-- TEST -----------------------------------------------------------------------
on run

    set xs to enumFromTo(1, 10)

    {xs, ¬
        {sum:foldl(summed, 0, xs)}, ¬
        {product:foldl(product, 1, xs)}}

    --> {{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {sum:55}, {product:3628800}}

end run

-- GENERIC FUNCTIONS ----------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if n < m then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
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

```AppleScript
```


## Arturo


```arturo
arr $(range 1 10)

print "Sum = " + $(sum arr)
print "Product = " + $(product arr)
```

```txt
Sum = 55
Product = 3628800
```


## AutoHotkey


```AutoHotkey
numbers = 1,2,3,4,5
product := 1
loop, parse, numbers, `,
{
sum += A_LoopField
product *= A_LoopField
}
msgbox, sum = %sum%`nproduct = %product%
```


## AWK

For array input, it is easiest to "deserialize" it from a string with the split() function.

```awk
$ awk 'func sum(s){split(s,a);r=0;for(i in a)r+=a[i];return r}{print sum($0)}'
1 2 3 4 5 6 7 8 9 10
55

$ awk 'func prod(s){split(s,a);r=1;for(i in a)r*=a[i];return r}{print prod($0)}'
1 2 3 4 5 6 7 8 9 10
3628800
```



## Babel


```babel
main: { [2 3 5 7 11 13] sp }

sum!    : { <- 0 -> { + } eachar }
product!: { <- 1 -> { * } eachar }

sp!:
    { dup
    sum %d cr <<
    product %d cr << }

Result:
41
30030
```


Perhaps better Babel:


```babel
main:
    { [2 3 5 7 11 13]
    ar2ls dup cp
    <- sum_stack ->
    prod_stack
    %d cr <<
    %d cr << }

sum_stack:
    { { give
        { + }
        { depth 1 > }
    do_while } nest }

prod_stack:
    { { give
        { * }
        { depth 1 > }
    do_while } nest }
```


The nest operator creates a kind of argument-passing context -
it saves whatever is on Top-of-Stack (TOS), saves the old stack,
clears the stack and places the saved TOS on the new, cleared stack.
This permits a section to monopolize the stack. At the end of the nest
context, whatever is on TOS will be "passed back" to the original stack
which will be restored.

The depth operator returns the current depth of the stack.


## BASIC

```freebasic
dim array(5) as integer = { 1, 2, 3, 4, 5 }

dim sum as integer = 0
dim prod as integer = 1
for index as integer = lbound(array) to ubound(array)
  sum += array(index)
  prod *= array(index)
next
```


=
## Applesoft BASIC
=

```ApplesoftBasic
 10 N = 5
 20 S = 0:P = 1: DATA 1,2,3,4,5
 30 N = N - 1: DIM A(N)
 40  FOR I = 0 TO N
 50  READ A(I): NEXT
 60  FOR I = 0 TO N
 70 S = S + A(I):P = P * A(I)
 80  NEXT
 90  PRINT "SUM="S,"PRODUCT="P
```


=
## BBC BASIC
=

```bbcbasic
      DIM array%(5)
      array%() = 1, 2, 3, 4, 5, 6

      PRINT "Sum of array elements = " ; SUM(array%())

      product% = 1
      FOR I% = 0 TO DIM(array%(),1)
        product% *= array%(I%)
      NEXT
      PRINT "Product of array elements = " ; product%
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 RANDOMIZE
110 LET N=5
120 NUMERIC A(1 TO N)
130 LET SUM=0:LET PROD=1
140 FOR I=1 TO N
150   LET A(I)=RND(9)+1
160   PRINT A(I);
170 NEXT
180 PRINT
190 FOR I=1 TO N
200   LET SUM=SUM+A(I):LET PROD=PROD*A(I)
210 NEXT
220 PRINT "Sum =";SUM,"Product =";PROD
```



## bc


```bc
a[0] = 3.0
a[1] = 1
a[2] = 4.0
a[3] = 1.0
a[4] = 5
a[5] = 9.00
n = 6
p = 1
for (i = 0; i < n; i++) {
    s += a[i]
    p *= a[i]
}
"Sum: "; s
"Product: "; p
```



## Befunge

The program first reads the number of elements in the array, then the elements themselves (each number on a separate line) and calculates their sum.

```Befunge
0 &>: #v_ $. @
       >1- \ & + \v
   ^              <
```



## Bracmat


```bracmat
( ( sumprod
  =   sum prod num
    .   0:?sum
      & 1:?prod
      & (   !arg
          :   ?
              ( #%?num ?
              & !num+!sum:?sum
              & !num*!prod:?prod
              & ~
              )
        | (!sum.!prod)
        )
  )
& out$sumprod$(2 3 5 7 11 13 17 19)
);
```

```txt
77.9699690
```



## C


```c
/* using pointer arithmetic (because we can, I guess) */
int arg[] = { 1,2,3,4,5 };
int arg_length = sizeof(arg)/sizeof(arg[0]);
int *end = arg+arg_length;
int sum = 0, prod = 1;
int *p;

for (p = arg; p!=end; ++p) {
   sum += *p;
   prod *= *p;
}
```


## C#

```c#
int sum = 0, prod = 1;
int[] arg = { 1, 2, 3, 4, 5 };
foreach (int value in arg) {
  sum += value;
  prod *= value;
}
```


===Alternative using Linq (C# 3)===
```c#
int[] arg = { 1, 2, 3, 4, 5 };
int sum = arg.Sum();
int prod = arg.Aggregate((runningProduct, nextFactor) => runningProduct * nextFactor);
```



## C++

```cpp
#include <numeric>
#include <functional>

int arg[] = { 1, 2, 3, 4, 5 };
int sum  = std::accumulate(arg, arg+5, 0, std::plus<int>());
// or just
// std::accumulate(arg, arg + 5, 0);
// since plus() is the default functor for accumulate
int prod = std::accumulate(arg, arg+5, 1, std::multiplies<int>());
```

Template alternative:

```cpp
// this would be more elegant using STL collections
template <typename T> T sum (const T *array, const unsigned n)
{
    T accum = 0;
    for (unsigned i=0; i<n; i++)
        accum += array[i];
    return accum;
}
template <typename T> T prod (const T *array, const unsigned n)
{
    T accum = 1;
    for (unsigned i=0; i<n; i++)
        accum *= array[i];
    return accum;
}

#include <iostream>
using std::cout;
using std::endl;

int main ()
{
    int aint[] = {1, 2, 3};
    cout << sum(aint,3) << " " << prod(aint, 3) << endl;
    float aflo[] = {1.1, 2.02, 3.003, 4.0004};
    cout << sum(aflo,4) << " " << prod(aflo,4) << endl;
    return 0;
}
```



## Chef



```chef
Sum and Product of Numbers as a Piece of Cake.

This recipe sums N given numbers.

Ingredients.
1 N
0 sum
1 product
1 number

Method.
Put sum into 1st mixing bowl.
Put product into 2nd mixing bowl.
Take N from refrigerator.
Chop N.
Take number from refrigerator.
Add number into 1st mixing bowl.
Combine number into 2nd mixing bowl.
Chop N until choped.
Pour contents of 2nd mixing bowl into the baking dish.
Pour contents of 1st mixing bowl into the baking dish.

Serves 1.
```



## Clean


```clean
array = {1, 2, 3, 4, 5}
Sum = sum [x \\ x <-: array]
Prod = foldl (*) 1 [x \\ x <-: array]
```



## Clojure



```lisp
(defn sum [vals] (reduce + vals))

(defn product [vals] (reduce * vals))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. array-sum-and-product.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  Array-Size              VALUE 10.
       01  array-area              VALUE "01020304050607080910".
           03  array               PIC 99 OCCURS Array-Size TIMES.

       01  array-sum               PIC 9(8).
       01  array-product           PIC 9(10) VALUE 1.

       01  i                       PIC 99.

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL Array-Size < i
               ADD array (i) TO array-sum
               MULTIPLY array (i) BY array-product
           END-PERFORM

           DISPLAY "Sum:     " array-sum
           DISPLAY "Product: " array-product

           GOBACK
           .
```



## CoffeeScript


```coffeescript

sum = (array) ->
  array.reduce (x, y) -> x + y

product = (array) ->
  array.reduce (x, y) -> x * y

```



## ColdFusion

Sum of an Array,

```cfm
<cfset Variables.myArray = [1,2,3,4,5,6,7,8,9,10]>
<cfoutput>#ArraySum(Variables.myArray)#</cfoutput>
```


Product of an Array,

```cfm
<cfset Variables.myArray = [1,2,3,4,5,6,7,8,9,10]>
<cfset Variables.Product = 1>
<cfloop array="#Variables.myArray#" index="i">
 <cfset Variables.Product *= i>
</cfloop>
<cfoutput>#Variables.Product#</cfoutput>
```



## Common Lisp


```lisp
(let ((data #(1 2 3 4 5)))     ; the array
  (values (reduce #'+ data)       ; sum
          (reduce #'* data)))     ; product
```


The loop macro also has support for sums.

```lisp
(loop for i in '(1 2 3 4 5) sum i)
```



## Crystal


### Declarative


```Ruby

def sum_product(a)
    { a.sum(), a.product() }
end

```



### Imperative


```Ruby

def sum_product_imperative(a)
    sum, product = 0, 1
    a.each do |e|
        sum += e
        product *= e
    end

    {sum, product}
end

```



```Ruby

require "benchmark"
Benchmark.ips do |x|
    x.report("declarative") { sum_product [1, 2, 3, 4, 5] }
    x.report("imperative") { sum_product_imperative [1, 2, 3, 4, 5] }
end

```



```Bash

declarative    8.1M (123.45ns) (± 2.99%)  65 B/op   1.30× slower
 imperative  10.57M ( 94.61ns) (± 2.96%)  65 B/op        fastest

```



## D


```d
import std.stdio;

void main() {
    immutable array = [1, 2, 3, 4, 5];

    int sum = 0;
    int prod = 1;

    foreach (x; array) {
        sum += x;
        prod *= x;
    }

    writeln("Sum: ", sum);
    writeln("Product: ", prod);
}
```

```txt
Sum: 15
Product: 120
```

Compute sum and product of array in one pass (same output):

```d
import std.stdio, std.algorithm, std.typecons;

void main() {
    immutable array = [1, 2, 3, 4, 5];

    // Results are stored in a 2-tuple
    immutable r = reduce!(q{a + b}, q{a * b})(tuple(0, 1), array);

    writeln("Sum: ", r[0]);
    writeln("Product: ", r[1]);
}
```



## dc


```dc
1 3 5 7 9 11 13 0ss1sp[dls+sslp*spz0!=a]dsax[Sum: ]Plsp[Product: ]Plpp
Sum: 49
Product: 135135
```



## Delphi


```delphi
program SumAndProductOfArray;

{$APPTYPE CONSOLE}

var
  i: integer;
  lIntArray: array [1 .. 5] of integer = (1, 2, 3, 4, 5);
  lSum: integer = 0;
  lProduct: integer = 1;
begin
  for i := 1 to length(lIntArray) do
  begin
    Inc(lSum, lIntArray[i]);
    lProduct := lProduct * lIntArray[i]
  end;

  Write('Sum: ');
  Writeln(lSum);
  Write('Product: ');
  Writeln(lProduct);
end.
```



## E


```e
pragma.enable("accumulator")
accum 0 for x in [1,2,3,4,5] { _ + x }
accum 1 for x in [1,2,3,4,5] { _ * x }
```



## Eiffel


```eiffel

class
	APPLICATION

create
	make

feature {NONE}

	make
		local
			test: ARRAY [INTEGER]
		do
			create test.make_empty
			test := <<5, 1, 9, 7>>
			io.put_string ("Sum: " + sum (test).out)
			io.new_line
			io.put_string ("Product: " + product (test).out)
		end

	sum (ar: ARRAY [INTEGER]): INTEGER
			-- Sum of the items of the array 'ar'.
		do
			across
				ar.lower |..| ar.upper as c
			loop
				Result := Result + ar [c.item]
			end
		end

	product (ar: ARRAY [INTEGER]): INTEGER
			-- Product of the items of the array 'ar'.
		do
			Result := 1
			across
				ar.lower |..| ar.upper as c
			loop
				Result := Result * ar [c.item]
			end
		end

end

```

```txt
Sum of the elements of the array: 30
Product of the elements of the array: 3840
```


## Elena

ELENA 4.1:

```elena
import system'routines;
import extensions;

public program()
{
    var list := new int[]::(1, 2, 3, 4, 5 );

    var sum := list.summarize(new Integer());
    var product := list.accumulate(new Integer(1), (var,val => var * val));
}
```



## Elixir

When an accumulator is omitted, the first element of the collection is used as the initial value of acc.

```elixir
iex(26)> Enum.reduce([1,2,3,4,5], 0, fn x,acc -> x+acc end)
15
iex(27)> Enum.reduce([1,2,3,4,5], 1, fn x,acc -> x*acc end)
120
iex(28)> Enum.reduce([1,2,3,4,5], fn x,acc -> x+acc end)
15
iex(29)> Enum.reduce([1,2,3,4,5], fn x,acc -> x*acc end)
120
iex(30)> Enum.reduce([], 0, fn x,acc -> x+acc end)
0
iex(31)> Enum.reduce([], 1, fn x,acc -> x*acc end)
1
iex(32)> Enum.reduce([], fn x,acc -> x+acc end)
** (Enum.EmptyError) empty error
    (elixir) lib/enum.ex:1287: Enum.reduce/2
iex(32)> Enum.reduce([], fn x,acc -> x*acc end)
** (Enum.EmptyError) empty error
    (elixir) lib/enum.ex:1287: Enum.reduce/2
```


The function with sum

```elixir
Enum.sum([1,2,3,4,5])           #=> 15
```



## Emacs Lisp

```lisp
(setq array [1 2 3 4 5])
(eval (concatenate 'list '(+) array))
(eval (concatenate 'list '(*) array))
```



### With a list



```lisp
(setq array '(1 2 3 4 5))
(apply '+ array)
(apply '* array)
```



### With explicit conversion



```lisp
(setq array [1 2 3 4 5])
(apply '+ (append array nil))
(apply '* (append array nil))
```



## Erlang

Using the standard libraries:

```erlang
% create the list:
L = lists:seq(1, 10).

% and compute its sum:
S = lists:sum(L).
P = lists:foldl(fun (X, P) -> X * P end, 1, L).
```

To compute sum and products in one pass:

```erlang

{Prod,Sum} = lists:foldl(fun (X, {P,S}) -> {P*X,S+X} end, {1,0}, lists:seq(1,10)).
```

Or defining our own versions:

```erlang
-module(list_sum).
-export([sum_rec/1, sum_tail/1]).

% recursive definition:
sum_rec([]) ->
    0;
sum_rec([Head|Tail]) ->
    Head + sum_rec(Tail).

% tail-recursive definition:
sum_tail(L) ->
    sum_tail(L, 0).
sum_tail([], Acc) ->
    Acc;
sum_tail([Head|Tail], Acc) ->
    sum_tail(Tail, Head + Acc).
```



## Euphoria


```euphoria
sequence array
integer sum,prod

array = { 1, 2, 3, 4, 5 }

sum = 0
prod = 1
for i = 1 to length(array) do
  sum += array[i]
  prod *= array[i]
end for

printf(1,"sum is %d\n",sum)
printf(1,"prod is %d\n",prod)
```


```txt

 sum is 15
 prod is 120

```


=={{header|F_Sharp|F#}}==

```fsharp

let numbers = [| 1..10 |]
let sum = numbers |> Array.sum
let product = numbers |> Array.reduce (*)

```



## Factor


```factor>1 5 1 <range
 [ sum . ] [ product . ] bi
    15 120
{ 1 2 3 4 } [ sum ] [ product ] bi
    10 24
```

sum and product are defined in the sequences vocabulary:

```factor
: sum ( seq -- n ) 0 [ + ] reduce ;
: product ( seq -- n ) 1 [ * ] reduce ;
```



## FALSE

Strictly speaking, there are no arrays in FALSE. However, a number of elements on the stack could be considered an array. The implementation below assumes the length of the array on top of the stack, and the actual items below it. Note that this implementation does remove the "array" from the stack, so in case the original values need to be retained, a copy should be provided before executing this logic.

```false
1 2 3 4 5 {input "array"}
5         {length of input}
0s:       {sum}
1p:       {product}

[$0=~][1-\$s;+s:p;*p:]#%

"Sum: "s;."
Product: "p;.
```

```txt
Sum: 15
Product: 120
```



## Fantom



```fantom

class Main
{
  public static Void main ()
  {
    Int[] array := (1..20).toList

    // you can use a loop
    Int sum := 0
    array.each |Int n| { sum += n }
    echo ("Sum of array is : $sum")

    Int product := 1
    array.each |Int n| { product *= n }
    echo ("Product of array is : $product")

    // or use 'reduce'
    // 'reduce' takes a function,
    //       the first argument is the accumulated value
    //       and the second is the next item in the list
    sum = array.reduce(0) |Obj r, Int v -> Obj|
    {
      return (Int)r + v
    }
    echo ("Sum of array : $sum")

    product = array.reduce(1) |Obj r, Int v -> Obj|
    {
      return (Int)r * v
    }
    echo ("Product of array : $product")
  }
}

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Sum_and_product_of_an_array this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: third ( a b c -- a b c a ) 2 pick ;
: reduce ( xt n addr cnt -- n' ) \ where xt ( a b -- n )
  cells bounds do i @ third execute  cell +loop nip ;

create a 1 , 2 , 3 , 4 , 5 ,

' + 0 a 5 reduce .    \ 15
' * 1 a 5 reduce .    \ 120
```



## Fortran

In ISO Fortran 90 and later, use SUM and PRODUCT intrinsics:

```fortran
integer, dimension(10) :: a = (/ (i, i=1, 10) /)
integer :: sresult, presult

sresult = sum(a)
presult = product(a)
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim a(1 To 4) As Integer = {1, 4, 6, 3}
Dim As Integer i, sum = 0, prod = 1
For i = 1 To 4
  sum  += a(i)
  prod *= a(i)
Next
Print "Sum     ="; sum
Print "Product ="; prod
Print
Print "Press any key to quit"
Sleep
```


```txt

Sum     = 14
Product = 72

```



## Frink


```frink

a = [1,2,3,5,7]
sum[a]
product[a]

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=4a4bdc35d661e2dc22d66d88991bef95 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim iList As Integer[] = [1, 2, 3, 4, 5]
Dim iSum, iCount As Integer
Dim iPrd As Integer = 1

For iCount = 0 To iList.Max
  iSum += iList[iCount]
  iPrd *= iList[iCount]
Next

Print "The Sum =\t" & iSum
Print "The Product =\t" & iPrd

End
```

Output:

```txt

The Sum =       15
The Product =   120

```



## GAP


```gap
v := [1 .. 8];

Sum(v);
# 36

Product(v);
# 40320

# You can sum or multiply the result of a function

Sum(v, n -> n^2);
# 204

Product(v, n -> 1/n);
# 1/40320
```



## GFA Basic



```basic

DIM a%(10)
' put some values into the array
FOR i%=1 TO 10
  a%(i%)=i%
NEXT i%
'
sum%=0
product%=1
FOR i%=1 TO 10
  sum%=sum%+a%(i%)
  product%=product%*a%(i%)
NEXT i%
'
PRINT "Sum is ";sum%
PRINT "Product is ";product%

```



## Go

;Implementation

```go
package main

import "fmt"

func main() {
    sum, prod := 0, 1
    for _, x := range []int{1,2,5} {
        sum += x
        prod *= x
    }
    fmt.Println(sum, prod)
}
```

```txt

8 10

```

;Library

```go
package main

import (
    "fmt"

    "github.com/gonum/floats"
)

var a = []float64{1, 2, 5}

func main() {
    fmt.Println("Sum:    ", floats.Sum(a))
    fmt.Println("Product:", floats.Prod(a))
}
```

```txt

Sum:     8
Product: 10

```



## Groovy

Groovy adds a "sum()" method for collections, but not a "product()" method:

```groovy
[1,2,3,4,5].sum()
```

However, for general purpose "reduction" or "folding" operations, Groovy does provide an "inject()" method for collections similar to "inject" in Ruby.

```groovy
[1,2,3,4,5].inject(0) { sum, val -> sum + val }
[1,2,3,4,5].inject(1) { prod, val -> prod * val }
```

You can also combine these operations:

```groovy
println ([1,2,3,4,5].inject([sum: 0, product: 1]) { result, value ->
    [sum: result.sum + value, product: result.product * value]})
```


=={{header|GW-BASIC}}==
```qbasic
10 REM Create an array with some test data in it
20 DIM A(5)
30 FOR I = 1 TO 5: READ A(I): NEXT I
40 DATA 1, 2, 3, 4, 5
50 REM Find the sum of elements in the array
60 S = 0
65 P = 1
70 FOR I = 1 TO 5
72 S = SUM + A(I)
75 P = P * A(I)
77 NEXT I
80 PRINT "The sum is "; S;
90 PRINT " and the product is "; P
```



## Haskell

For lists, ''sum'' and ''product'' are already defined in the Prelude:

```haskell
values = [1..10]

s = sum values           -- the easy way
p = product values

s1 = foldl (+) 0 values  -- the hard way
p1 = foldl (*) 1 values
```

To do the same for an array, just convert it lazily to a list:

```haskell
import Data.Array

values = listArray (1,10) [1..10]

s = sum . elems $ values
p = product . elems $ values
```


Or perhaps:

```haskell
import Data.Array (listArray, elems)

main :: IO ()
main = mapM_ print $ [sum, product] <*> [elems $ listArray (1, 10) [11 .. 20]]
```

```txt
155
670442572800
```



## HicEst


```hicest
array = $ ! 1, 2, ..., LEN(array)

sum = SUM(array)

product = 1 ! no built-in product function in HicEst
DO i = 1, LEN(array)
  product = product * array(i)
ENDDO

WRITE(ClipBoard, Name) n, sum, product ! n=100; sum=5050; product=9.33262154E157;
```


=={{header|Icon}} and {{header|Unicon}}==
The program below prints the sum and product of the arguments to the program.

```Icon
procedure main(arglist)
every ( sum := 0 ) +:= !arglist
every ( prod := 1 ) *:= !arglist
write("sum := ", sum,", prod := ",prod)
end
```



## IDL


```idl
array = [3,6,8]
print,total(array)
print,product(array)
```



## Inform 7


```inform7
Sum And Product is a room.

To decide which number is the sum of (N - number) and (M - number) (this is summing):
	decide on N + M.

To decide which number is the product of (N - number) and (M - number) (this is production):
	decide on N * M.

When play begins:
	let L be {1, 2, 3, 4, 5};
	say "List: [L in brace notation], sum = [summing reduction of L], product = [production reduction of L].";
	end the story.
```



## J



```j
sum     =: +/
product =: */
```


For example:


```j
   sum 1 3 5 7 9 11 13
49
   product 1 3 5 7 9 11 13
135135

   a=: 3 10 ?@$ 100  NB. random array
   a
90 47 58 29 22 32 55  5 55 73
58 50 40  5 69 46 34 40 46 84
29  8 75 97 24 40 21 82 77  9

   NB. on a table, each row is an item to be summed:
   sum a
177 105 173 131 115 118 110 127 178 166
   product a
151380 18800 174000 14065 36432 58880 39270 16400 194810 55188

   NB. but we can tell J to sum everything within each row, instead:
   sum"1 a
466 472 462
   product"1 a
5.53041e15 9.67411e15 1.93356e15
```



## Java

```java5
public class SumProd
{
 public static void main(final String[] args)
 {
  int sum = 0;
  int prod = 1;
  int[] arg = {1,2,3,4,5};
  for (int i : arg)
  {
   sum += i;
   prod *= i;
  }
 }
}
```


```java5
import java.util.Arrays;

public class SumProd
{
 public static void main(final String[] args)
 {
  int[] arg = {1,2,3,4,5};
  System.out.printf("sum = %d\n", Arrays.stream(arg).sum());
  System.out.printf("sum = %d\n", Arrays.stream(arg).reduce(0, (a, b) -> a + b));
  System.out.printf("product = %d\n", Arrays.stream(arg).reduce(1, (a, b) -> a * b));
 }
}
```

```txt

sum = 15
sum = 15
product = 120

```



## JavaScript


### ES5


```javascript
var array = [1, 2, 3, 4, 5],
    sum = 0,
    prod = 1,
    i;
for (i = 0; i < array.length; i += 1) {
    sum += array[i];
    prod *= array[i];
}
alert(sum + ' ' + prod);
```



Where supported, the reduce method can also be used:

```javascript
var array = [1, 2, 3, 4, 5],
    sum = array.reduce(function (a, b) {
        return a + b;
    }, 0),
    prod = array.reduce(function (a, b) {
        return a * b;
    }, 1);
alert(sum + ' ' + prod);
```



### ES6


```JavaScript
(() => {
    'use strict';

    // sum :: (Num a) => [a] -> a
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // product :: (Num a) => [a] -> a
    const product = xs => xs.reduce((a, x) => a * x, 1);


    // TEST
    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);

    return show(
        [sum, product]
        .map(f => f([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
    );
})();
```


```txt
[
  55,
  3628800
]
```



## jq

The builtin filter, add/0, computes the sum of an array:

```jq
[4,6,8] | add
# => 18
```


```jq
[range(2;5) * 2] | add
# => 18
```

An efficient companion filter for computing the product of the items in an array can be defined as follows:

```jq
def prod: reduce .[] as $i (1; . * $i);
```

Examples:

```jq
[4,6,8] | prod
 # => 192
```

10!

```jq
[range(1;11)] | prod
# =>3628800
```



## Julia


```julia>julia
 sum([4,6,8])
18

julia> +((1:10)...)
55

julia +([1,2,3]...)
6

julia> prod([4,6,8])
192
```



## K


```k
  sum: {+/}x
  product: {*/}x
  a: 1 3 5 7 9 11 13
  sum a
49
  product a
135135
```


It is easy to see the relationship of K to J here.


## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
    val a = intArrayOf(1, 5, 8, 11, 15)
    println("Array contains : ${a.contentToString()}")
    val sum = a.sum()
    println("Sum is $sum")
    val product = a.fold(1) { acc, i -> acc * i }
    println("Product is $product")
}
```


```txt

Array contains : [1, 5, 8, 11, 15]
Sum is 40
Product is 6600


```



## Lang5


```lang5
4 iota 1 + dup

'+ reduce
'* reduce
```



## Lasso


```Lasso
local(x = array(1,2,3,4,5,6,7,8,9,10))
// sum of array elements
'Sum: '
with n in #x
sum #n
'\r'
// product of arrray elements
'Product: '
local(product = 1)
with n in #x do => { #product *= #n }
#product
```

```txt
Sum: 55
Product: 3628800
```



## Liberty BASIC


```lb
Dim array(19)

For i = 0 To 19
    array(i) = Int(Rnd(1) * 20)
Next i

'product must first equal one or you will get 0 as the product
product = 1
For i = 0 To 19
    sum = (sum + array(i))
    product = (product * array(i))
next i

Print "Sum is " + str$(sum)
Print "Product is " + str$(product)
```



## Lingo


```lingo
on sum (intList)
  res = 0
  repeat with v in intList
    res = res + v
  end repeat
  return res
end

on product (intList)
  res = 1
  repeat with v in intList
    res = res * v
  end repeat
  return res
end
```



## LiveCode


```LiveCode
//sum
put "1,2,3,4" into nums
split nums using comma
answer sum(nums)

// product
local prodNums
repeat for each element n in nums
    if prodNums is empty then
        put n into prodNums
    else
        multiply prodnums by n
    end if
end repeat
answer prodnums
```



## Logo


```logo
print apply "sum arraytolist {1 2 3 4 5}
print apply "product arraytolist {1 2 3 4 5}
```



## Lua


```lua

function sumf(a, ...) return a and a + sumf(...) or 0 end
function sumt(t) return sumf(unpack(t)) end
function prodf(a, ...) return a and a * prodf(...) or 1 end
function prodt(t) return prodf(unpack(t)) end

print(sumt{1, 2, 3, 4, 5})
print(prodt{1, 2, 3, 4, 5})
```



```lua

function table.sum(arr, length)
      --same as if <> then <> else <>
      return length == 1 and arr[1] or arr[length] + table.sum(arr, length -1)
end

function table.product(arr, length)
      return length == 1 and arr[1] or arr[length] * table.product(arr, length -1)
end

t = {1,2,3}
print(table.sum(t,#t))
print(table.product(t,3))

```



## Lucid

prints a running sum and product of sequence 1,2,3...

```lucid
[%sum,product%]
 where
    x = 1 fby x + 1;
    sum = 0 fby sum + x;
    product = 1 fby product * x
 end
```


## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      a = (1,2,3,4,5,6,7,8,9,10)
      print a#sum() = 55
      sum = lambda->{push number+number}
      product = lambda->{Push number*number}
      print a#fold(lambda->{Push number*number}, 1), a#fold(lambda->{push number+number},0)
      dim a(2,2) = 5
      Print a()#sum() = 20
}
checkit

```


## Maple


```maple
a := Array([1, 2, 3, 4, 5, 6]);
	add(a);
	mul(a);
```



## Mathematica

Mathematica provides many ways of doing the sum of an array (any kind of numbers or symbols):

```Mathematica
a = {1, 2, 3, 4, 5}
Plus @@ a
Apply[Plus, a]
Total[a]
Total@a
a // Total
Sum[a[[i]], {i, 1, Length[a]}]
Sum[i, {i, a}]
```

all give 15. For product we also have a couple of choices:

```Mathematica
a = {1, 2, 3, 4, 5}
Times @@ a
Apply[Times, a]
Product[a[[i]], {i, 1, Length[a]}]
Product[i, {i, a}]
```

all give 120.


## MATLAB

These two function are built into MATLAB as the "sum(array)" and "prod(array)" functions.

Sample Usage:

```MATLAB>>
 array = [1 2 3;4 5 6;7 8 9]

array =

     1     2     3
     4     5     6
     7     8     9

>> sum(array,1)

ans =

    12    15    18

>> sum(array,2)

ans =

     6
    15
    24

>> prod(array,1)

ans =

    28    80   162

>> prod(array,2)

ans =

     6
   120
   504
```




## Maxima



```maxima
lreduce("+", [1, 2, 3, 4, 5, 6, 7, 8]);
36

lreduce("*", [1, 2, 3, 4, 5, 6, 7, 8]);
40320
```



## MAXScript


```maxscript
arr = #(1, 2, 3, 4, 5)
sum = 0
for i in arr do sum += i
product = 1
for i in arr do product *= i
```



## min

```min
(1 2 3 4 5) ((sum) (1 '* reduce)) cleave
"Sum: $1\nProduct: $2" get-stack % puts
```

```txt

Sum: 15
Product: 120

```


=={{header|MK-61/52}}==
<lang>^	1	ПE	+	П0	КИП0	x#0	18	^	ИПD
+	ПD	<->	ИПE	*	ПE	БП	05	С/П
```


''Instruction'': РX - array length, Р1:РC - array, РD and РE - sum and product of an array.

=={{header|Modula-3}}==

```modula3
MODULE Sumprod EXPORTS Main;

FROM IO IMPORT Put;
FROM Fmt IMPORT Int;

VAR a := ARRAY [1..5] OF INTEGER {1, 2, 3, 4, 5};
VAR sum: INTEGER := 0;
VAR prod: INTEGER := 1;

BEGIN
  FOR i := FIRST(a) TO LAST(a) DO
    INC(sum, a[i]);
    prod := prod * a[i];
  END;
  Put("Sum of array: " & Int(sum) & "\n");
  Put("Product of array: " & Int(prod) & "\n");
END Sumprod.
```

```txt
Sum of array: 15
Product of array: 120
```



## MUMPS


```MUMPS

SUMPROD(A)
 ;Compute the sum and product of the numbers in the array A
 NEW SUM,PROD,POS
 ;SUM is the running sum,
 ;PROD is the running product,
 ;POS is the position within the array A
 SET SUM=0,PROD=1,POS=""
 FOR  SET POS=$ORDER(A(POS)) Q:POS=""  SET SUM=SUM+A(POS),PROD=PROD*A(POS)
 WRITE !,"The sum of the array is "_SUM
 WRITE !,"The product of the array is "_PROD
 KILL SUM,PROD,POS
 QUIT
```

Example:
```txt

USER>SET C(-1)=2,C("A")=3,C(42)=1,C(0)=7

USER>D SUMPROD^ROSETTA(.C)

The sum of the array is 13
The product of the array is 42

```

Note - the string "A" converts to 0 when doing mathematical operations.

```txt

USER>SET C(-1)=2,C("A")="3H",C(42)=.1,C(0)=7.0,C("B")="A"

USER>D SUMPROD^ROSETTA(.C)

The sum of the array is 12.1
The product of the array is 0

```



## Nemerle

As mentioned for some of the other functional languages, it seems more natural to work with lists in Nemerle, but as the task specifies working on an array, this solution will work on either.

```Nemerle
using System;
using System.Console;
using System.Collections.Generic;
using Nemerle.Collections;

module SumProd
{
    Sum[T] (nums : T) : int
      where T : IEnumerable[int]
    {
        nums.FoldLeft(0, _+_)
    }

    Product[T] (nums : T) : int
      where T : IEnumerable[int]
    {
        nums.FoldLeft(1, _*_)
    }

    Main() : void
    {
        def arr = array[1, 2, 3, 4, 5];
        def lis = [1, 2, 3, 4, 5];

        def suml = Sum(lis);
        def proda = Product(arr);

        WriteLine("Sum is: {0}\tProduct is: {1}", suml, proda);
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

harry = [long 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

sum = long 0
product = long 1
entries = Rexx ''

loop n_ = int 0 to harry.length - 1
  nxt = harry[n_]
  entries = entries nxt
  sum = sum + nxt
  product = product * nxt
  end n_

entries = entries.strip

say 'Sum and product of' entries.changestr(' ', ',')':'
say '     Sum:' sum
say ' Product:' product

return

```

```txt

 Sum and product of 1,2,3,4,5,6,7,8,9,10:
     Sum: 55
 Product: 3628800

```



## NewLISP


```NewLISP
(setq a '(1 2 3 4 5))
(apply + a)
(apply * a)
```



## Nial

Nial being an array language, what applies to individual elements are extended to cover array operations by default strand notation

```nial
+ 1 2 3
= 6
* 1 2 3
= 6
```

array notation

```nial
+ [1,2,3]
```

grouped notation

```nial
(* 1 2 3)
= 6
* (1 2 3)
= 6
```

(All these notations are equivalent)


## Nim


```nim
var xs = @[1,2,3,4,5,6]

var sum, product: int

product = 1

for x in xs:
  sum += x
  product *= x
```


Or functionally:

```nim
import sequtils

let
  xs = @[1,2,3,4,5,6]
  sum = xs.foldl(a + b)
  product = xs.foldl(a * b)
```


Or using a math function:

```nim
import math

let numbers = @[1, 5, 4]
let total = sum(numbers)

var product = 1
for n in numbers:
  product *= n
```



## Objeck


```objeck

sum := 0;
prod := 1;
arg := [1, 2, 3, 4, 5];
each(i : arg) {
  sum += arg[i];
  prod *= arg[i];
};

```


=={{header|Objective-C}}==
Sum:

```objc
- (float) sum:(NSMutableArray *)array
{
	int i, sum, value;
	sum = 0;
	value = 0;

	for (i = 0; i < [array count]; i++) {
		value = [[array objectAtIndex: i] intValue];
		sum += value;
	}

	return suml;
}
```

Product:

```objc
- (float) prod:(NSMutableArray *)array
{
	int i, prod, value;
	prod = 0;
	value = 0;

	for (i = 0; i < [array count]; i++) {
		value = [[array objectAtIndex: i] intValue];
		prod *= value;
	}

	return suml;
}
```



## OCaml


### Arrays


```ocaml
(* ints *)
let a = [| 1; 2; 3; 4; 5 |];;
Array.fold_left (+) 0 a;;
Array.fold_left ( * ) 1 a;;
(* floats *)
let a = [| 1.0; 2.0; 3.0; 4.0; 5.0 |];;
Array.fold_left (+.) 0.0 a;;
Array.fold_left ( *.) 1.0 a;;
```


### Lists


```ocaml
(* ints *)
let x = [1; 2; 3; 4; 5];;
List.fold_left (+) 0 x;;
List.fold_left ( * ) 1 x;;
(* floats *)
let x = [1.0; 2.0; 3.0; 4.0; 5.0];;
List.fold_left (+.) 0.0 x;;
List.fold_left ( *.) 1.0 x;;
```



## Octave


```octave
a = [ 1, 2, 3, 4, 5, 6 ];
b = [ 10, 20, 30, 40, 50, 60 ];
vsum = a + b;
vprod = a .* b;
```



## Oforth



```Oforth
[1, 2, 3, 4, 5 ] sum println
[1, 3, 5, 7, 9 ] prod println
```


```txt

15
945

```



## Ol


```scheme

(print (fold + 0 '(1 2 3 4 5)))
(print (fold * 1 '(1 2 3 4 5)))

```



## ooRexx

```oorexx
a=.my_array~new(20)
do i=1 To 20
  a[i]=i
  End
s=a~makestring((LINE),',')
Say s
Say '    sum='a~sum
Say 'product='a~prod
::class my_array subclass array
::method sum
sum=0
Do i=1 To self~dimension(1)
  sum+=self[i]
  End
Return sum
::method prod
Numeric Digits 30
prod=1
Do i=1 To self~dimension(1)
  prod*=self[i]
  End
Return prod
```

```txt
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20
    sum=210
product=2432902008176640000
```



## Oz

Calculations like this are typically done on lists, not on arrays:

```oz
declare
  Xs = [1 2 3 4 5]
  Sum = {FoldL Xs Number.'+' 0}
  Product = {FoldL Xs Number.'*' 1}
in
  {Show Sum}
  {Show Product}
```


If you are actually working with arrays, a more imperative approach seems natural:

```oz
declare
  Arr = {Array.new 1 3 0}
  Sum = {NewCell 0}
in
  Arr.1 := 1
  Arr.2 := 2
  Arr.3 := 3

  for I in {Array.low Arr}..{Array.high Arr} do
     Sum := @Sum + Arr.I
  end
  {Show @Sum}
```



## PARI/GP

These are built in to GP: <code>vecsum</code> and <code>factorback</code> (the latter can also take factorization matrices, thus the name). They could be coded like so:

```parigp
vecsum1(v)={
  sum(i=1,#v,v[i])
};
vecprod(v)={
  prod(i=1,#v,v[i])
};
```


In 2.10.0 the function <code>vecprod</code> was introduced as well. Like <code>factorback</code> it gives the product of the elements of an array but unlike <code>factorback</code> it doesn't handle factorization matrices.


## Pascal

See [[Sum_and_product_of_an_array#Delphi | Delphi]]


## Perl


```perl
my @list = ( 1, 2, 3 );

my ( $sum, $prod ) = ( 0, 1 );
$sum  += $_ foreach @list;
$prod *= $_ foreach @list;
```

Or using the [https://metacpan.org/pod/List::Util List::Util] module:

```perl
use List::Util qw/sum0 product/;
my @list = (1..9);

say "Sum: ", sum0(@list);    # sum0 returns 0 for an empty list
say "Product: ", product(@list);
```

```txt
Sum: 45
Product: 362880
```



## Perl 6


```perl6
my @ary = 1, 5, 10, 100;
say 'Sum: ',     [+] @ary;
say 'Product: ', [*] @ary;
```



## Phix


```Phix
sequence s = {1,2,3,4,5}
integer asum = 0, aprod = 1
for i=1 to length(s) do
  asum += s[i]
  aprod *= s[i]
end for
printf(1,"sum is %d\n",asum)        -- or sum(s)
printf(1,"prod is %d\n",aprod)
```

```txt

sum is 15
prod is 120

```



## PHP


```php
$array = array(1,2,3,4,5,6,7,8,9);
echo array_sum($array);
echo array_product($array);
```



## PicoLisp


```PicoLisp
(let Data (1 2 3 4 5)
   (cons
      (apply + Data)
      (apply * Data) ) )
```

```txt
(15 . 120)
```



## PL/I


```pli
declare A(10) fixed binary static initial
   (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

put skip list (sum(A));
put skip list (prod(A));
```



## Pop11

Simple loop:

```pop11
lvars i, sum = 0, prod = 1, ar = {1 2 3 4 5 6 7 8 9};
for i from 1 to length(ar) do
    ar(i) + sum -> sum;
    ar(i) * prod -> prod;
endfor;
```

One can alternatively use second order iterator:

```pop11
lvars sum = 0, prod = 1, ar = {1 2 3 4 5 6 7 8 9};
appdata(ar, procedure(x); x + sum -> sum; endprocedure);
appdata(ar, procedure(x); x * prod -> prod; endprocedure);
```



## PostScript

<lang>
/sumandproduct
{
/x exch def
/sum 0 def
/prod 0 def
/i 0 def
x length 0 eq
{
}
{
/prod prod 1 add def
x length{
/sum sum x i get add def
/prod prod x i get mul def
/i i 1 add def
}repeat
}ifelse
sum ==
prod ==
}def

```


```postscript

% sum
[1 1 1 1 1] 0 {add} fold
% product
[1 1 1 1 1] 1 {mul} fold


```



## PowerShell

The <code>Measure-Object</code> cmdlet already knows how to compute a sum:

```powershell
function Get-Sum ($a) {
    return ($a | Measure-Object -Sum).Sum
}
```

But not how to compute a product:

```powershell
function Get-Product ($a) {
    if ($a.Length -eq 0) {
        return 0
    } else {
        $p = 1
        foreach ($x in $a) {
            $p *= $x
        }
        return $p
    }
}
```

One could also let PowerShell do all the work by simply creating an expression to evaluate:

```powershell
function Get-Product ($a) {
    if ($a.Length -eq 0) {
        return 0
    }
    $s = $a -join '*'
    return (Invoke-Expression $s)
}
```

Even nicer, however, is a function which computes both at once and returns a custom object with appropriate properties:

```powershell
function Get-SumAndProduct ($a) {
    $sum = 0
    if ($a.Length -eq 0) {
        $prod = 0
    } else {
        $prod = 1
        foreach ($x in $a) {
            $sum += $x
            $prod *= $x
        }
    }
    $ret = New-Object PSObject
    $ret | Add-Member NoteProperty Sum $sum
    $ret | Add-Member NoteProperty Product $prod
    return $ret
}
```

```txt
PS> Get-SumAndProduct 5,9,7,2,3,8,4

Sum Product
--- -------
 38   60480
```



## Prolog


```prolog
sum([],0).
sum([H|T],X) :- sum(T,Y), X is H + Y.
product([],1).
product([H|T],X) :- product(T,Y), X is H * X.
```


test
 :- sum([1,2,3,4,5,6,7,8,9],X).
 X =45;
 :- product([1,2,3,4,5],X).
 X = 120;


Using fold

```prolog

add(A,B,R):-
    R is A + B.

mul(A,B,R):-
    R is A * B.

% define fold now.
fold([], Act, Init, Init).

fold(Lst, Act, Init, Res):-
    head(Lst,Hd),
    tail(Lst,Tl),
    apply(Act,[Init, Hd, Ra]),
    fold(Tl, Act, Ra, Res).

sumproduct(Lst, Sum, Prod):-
    fold(Lst,mul,1, Prod),
    fold(Lst,add,0, Sum).

?- sumproduct([1,2,3,4],Sum,Prod).
Sum = 10,
Prod = 24 .


```



## PureBasic


```PureBasic
Dim MyArray(9)
Define a, sum=0, prod=1

For a = 0 To ArraySize(MyArray())     ; Create a list of some random numbers
  MyArray(a) = 1 + Random(9)          ; Insert a number [1...10] in current element
Next

For a = 0 To ArraySize(MyArray())     ; Calculate Sum and Product of this Array
  sum  + MyArray(a)
  prod * MyArray(a)
Next

Debug "The sum is " + Str(sum)        ; Present the results
Debug "Product is " + Str(prod)
```



## Python

```python
numbers = [1, 2, 3]
total = sum(numbers)

product = 1
for i in numbers:
    product *= i
```

Or functionally (faster but perhaps less clear):
```python
from operator import mul, add
sum = reduce(add, numbers) # note: this version doesn't work with empty lists
sum = reduce(add, numbers, 0)
product = reduce(mul, numbers) # note: this version doesn't work with empty lists
product = reduce(mul, numbers, 1)
```

```python
from numpy import r_
numbers = r_[1:4]
total = numbers.sum()
product = numbers.prod()
```


If you are summing floats in Python 2.6+, you should use <tt>math.fsum()</tt> to avoid loss of precision:
```python
import math
total = math.fsum(floats)
```



## R


```r
total <- sum(1:5)
product <- prod(1:5)
```



## Racket



```racket
#lang racket

(for/sum ([x #(3 1 4 1 5 9)]) x)
(for/product ([x #(3 1 4 1 5 9)]) x)
```



## Raven


```raven
0 [ 1 2 3 ] each +
1 [ 1 2 3 ] each *
```



## REBOL


```REBOL
REBOL [
    Title: "Sum and Product"
    URL: http://rosettacode.org/wiki/Sum_and_product_of_array
]

; Simple:

sum: func [a [block!] /local x] [x: 0  forall a [x: x + a/1]  x]

product: func [a [block!] /local x] [x: 1  forall a [x: x * a/1]  x]

; Way too fancy:

redux: func [
	"Applies an operation across an array to produce a reduced value."
	a [block!] "Array to operate on."
	op [word!] "Operation to perform."
	/init x    "Initial value (default 0)."
][if not init [x: 0]  forall a [x: do compose [x (op) (a/1)]]  x]

rsum: func [a [block!]][redux a '+]

rproduct: func [a [block!]][redux/init a '* 1]

; Tests:

assert: func [code][print [either do code ["  ok"]["FAIL"]  mold code]]

print "Simple dedicated functions:"
assert [55      = sum [1 2 3 4 5 6 7 8 9 10]]
assert [3628800 = product [1 2 3 4 5 6 7 8 9 10]]

print [crlf "Fancy reducing function:"]
assert [55      = rsum [1 2 3 4 5 6 7 8 9 10]]
assert [3628800 = rproduct [1 2 3 4 5 6 7 8 9 10]]
```


```txt
Simple dedicated functions:
  ok [55 = sum [1 2 3 4 5 6 7 8 9 10]]
  ok [3628800 = product [1 2 3 4 5 6 7 8 9 10]]

Fancy reducing function:
  ok [55 = rsum [1 2 3 4 5 6 7 8 9 10]]
  ok [3628800 = rproduct [1 2 3 4 5 6 7 8 9 10]]
```



## REXX


```rexx
/*REXX program adds and multiplies   N   elements of a (populated)  array  @. */
numeric digits 200                     /*200 decimal digit #s  (default is 9).*/
parse arg N .;  if N==''  then N=20    /*Not specified?  Then use the default.*/

          do j=1  for N                /*build array of  N  elements (or 20?).*/
          @.j=j                        /*set 1st to 1, 3rd to 3, 8th to 8 ··· */
          end   /*j*/
sum=0                                  /*initialize  SUM  (variable) to zero. */
prod=1                                 /*initialize  PROD (variable) to unity.*/
          do k=1  for N
          sum  = sum  + @.k            /*add the element to the running total.*/
          prod = prod * @.k            /*multiply element to running product. */
          end   /*k*/                  /* [↑]  this pgm:  same as N factorial.*/

say '     sum of '     m     " elements for the  @  array is: "     sum
say ' product of '     m     " elements for the  @  array is: "     prod
                                       /*stick a fork in it,  we're all done. */
```

'''output''' using the default input of:   <tt> 20 </tt>

```txt

     sum of  M  elements for the  @  array is:  210
 product of  M  elements for the  @  array is:  2432902008176640000

```



## Ring


```ring

aList = 1:10   nSum=0  nProduct=0
for x in aList nSum += x nProduct *= x next
See "Sum = " + nSum + nl
See "Product = " + nProduct + nl

```



## Ruby


```ruby
arr = [1,2,3,4,5]     # or ary = *1..5, or ary = (1..5).to_a
p sum = arr.inject(0) { |sum, item| sum + item }
# => 15
p product = arr.inject(1) { |prod, element| prod * element }
# => 120
```


```ruby
arr = [1,2,3,4,5]
p sum = arr.inject(0, :+)         #=> 15
p product = arr.inject(1, :*)     #=> 120

# If you do not explicitly specify an initial value for memo,
# then the first element of collection is used as the initial value of memo.
p sum = arr.inject(:+)            #=> 15
p product = arr.inject(:*)        #=> 120
```


Note: When the Array is empty, the initial value returns. However, nil returns if not giving an initial value.

```ruby
arr = []
p arr.inject(0, :+)               #=> 0
p arr.inject(1, :*)               #=> 1
p arr.inject(:+)                  #=> nil
p arr.inject(:*)                  #=> nil
```


Enumerable#reduce is the alias of Enumerable#inject.

```ruby
arr = [1,2,3,4,5]
p sum = arr.sum                   #=> 15
p [].sum                          #=> 0
```



## Run BASIC


```runbasic
dim array(100)
for i = 1 To 100
    array(i) = rnd(0) * 100
next i

product = 1
for i = 0 To 19
    sum     = (sum + array(i))
    product = (product * array(i))
next i

Print "    Sum is ";sum
Print "Product is ";product
```



## Rust


```rust


fn main() {
    let arr = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];

    // using fold
    let sum = arr.iter().fold(0i32, |a, &b| a + b);
    let product = arr.iter().fold(1i32, |a, &b| a * b);
    println!("the sum is {} and the product is {}", sum, product);

    // or using sum and product
    let sum = arr.iter().sum::<i32>();
    let product = arr.iter().product::<i32>();
    println!("the sum is {} and the product is {}", sum, product);
}

```


=={{header|S-lang}}==
<lang S-lang>variable a = [5, -2, 3, 4, 666, 7];
```


The sum of array elements is handled by an intrinsic.
[note: print is slsh-specific; if not available, use printf().]

<lang S-lang>print(sum(a));
```


The product is slightly more involved; I'll use this as a
chance to show the alternate stack-based use of 'foreach':
<lang S-lang>variable prod = a[0];

% Skipping the loop variable causes the val to be placed on the stack.
% Also note that the double-brackets ARE required. The inner one creates
% a "range array" based on the length of a.
foreach (a[[1:]])
  % () pops it off.
  prod *= ();

print(prod);
```



## SAS


```sas
data _null_;
   array a{*} a1-a100;
   do i=1 to 100;
      a{i}=i*i;
   end;
   b=sum(of a{*});
   put b c;
run;
```



## Sather


```sather
class MAIN is
  main is
    a :ARRAY{INT} := |10, 5, 5, 20, 60, 100|;
    sum, prod :INT;
    loop sum := sum + a.elt!; end;
    prod := 1;
    loop prod := prod * a.elt!; end;
    #OUT + sum + " " + prod + "\n";
  end;
end;
```



## Scala


```scala
val seq = Seq(1, 2, 3, 4, 5)
val sum = seq.foldLeft(0)(_ + _)
val product = seq.foldLeft(1)(_ * _)
```


Or even shorter:

```scala
val sum = seq.sum
val product = seq.product
```


Works with all data types for which a Numeric implicit is available.


## Scheme


```scheme
(apply + '(1 2 3 4 5))
(apply * '(1 2 3 4 5))
```

A tail-recursive solution, without the n-ary operator "trick". Because Scheme supports tail call optimization, this is as space-efficient as an imperative loop.

```scheme
(define (reduce f i l)
  (if (null? l)
    i
    (reduce f (f i (car l)) (cdr l))))

(reduce + 0 '(1 2 3 4 5)) ;; 0 is unit for +
(reduce * 1 '(1 2 3 4 5)) ;; 1 is unit for *
```


## Seed7


```seed7
const func integer: sumArray (in array integer: valueArray) is func
  result
    var integer: sum is 0;
  local
    var integer: value is 0;
  begin
    for value range valueArray do
      sum +:= value;
    end for;
  end func;

const func integer: prodArray (in array integer: valueArray) is func
  result
    var integer: prod is 1;
  local
    var integer: value is 0;
  begin
    for value range valueArray do
      prod *:= value;
    end for;
  end func;
```

Call these functions with:
 writeln(sumArray([](1, 2, 3, 4, 5)));
 writeln(prodArray([](1, 2, 3, 4, 5)));


## SETL


```SETL
numbers := [1 2 3 4 5 6 7 8 9];
print(+/ numbers, */ numbers);
```


=> <code>45 362880</code>


## Sidef

Using built-in methods:

```ruby
var ary = [1, 2, 3, 4, 5];
say ary.sum;                 # => 15
say ary.prod;                # => 120
```


Alternatively, using hyper-operators:

```ruby
var ary = [1, 2, 3, 4, 5];
say ary«+»;                  # => 15
say ary«*»;                  # => 120
```



## Slate


```slate
#(1 2 3 4 5) reduce: [:sum :number | sum + number]
#(1 2 3 4 5) reduce: [:product :number | product * number]
```

Shorthand for the above with a macro:

```slate
#(1 2 3 4 5) reduce: #+ `er
#(1 2 3 4 5) reduce: #* `er
```


## Smalltalk


```smalltalk
#(1 2 3 4 5) inject: 0 into: [:sum :number | sum + number]
#(1 2 3 4 5) inject: 1 into: [:product :number | product * number]
```

Some implementation also provide a ''fold:'' message:

```smalltalk
#(1 2 3 4 5) fold: [:sum :number | sum + number]
#(1 2 3 4 5) fold: [:product :number | product * number]
```


## SNOBOL4


```snobol
          t = table()
* read the integer from the std. input
init_tab  t<x = x + 1> = trim(input)    :s(init_tab)
          product = 1
          sum = 0

* counting backwards to 1
loop      i = t< x = ?gt(x,1) x - 1>	:f(out)
          sum = sum + i
          product = product * i         :(loop)
out       output = "Sum:  " sum
          output = "Prod: " product
end
```


Input
 1
 2
 3
 4
 5
```txt

 Sum:  15
 Prod: 120

```



## Sparkling


```Sparkling>spn:1
 reduce({ 1, 2, 3, 4, 5 }, 0, function(x, y) { return x + y; })
= 15
spn:2> reduce({ 1, 2, 3, 4, 5 }, 1, function(x, y) { return x * y; })
= 120
```



## Standard ML


### Arrays


```sml
(* ints *)
val a = Array.fromList [1, 2, 3, 4, 5];
Array.foldl op+ 0 a;
Array.foldl op* 1 a;
(* reals *)
val a = Array.fromList [1.0, 2.0, 3.0, 4.0, 5.0];
Array.foldl op+ 0.0 a;
Array.foldl op* 1.0 a;
```


### Lists


```sml
(* ints *)
val x = [1, 2, 3, 4, 5];
foldl op+ 0 x;
foldl op* 1 x;
(* reals *)
val x = [1.0, 2.0, 3.0, 4.0, 5.0];
foldl op+ 0.0 x;
foldl op* 1.0 x;
```



## Stata

Mata does not have a builtin product function, but one can do the following, which will compute the product of nonzero elements of the array:


```stata
a = 1,-2,-3,-4,5
sum(a)
  -3
(-1)^mod(sum(a:<0),2)*exp(sum(log(abs(a))))
  -120
```



## Swift


```swift
let a = [1, 2, 3, 4, 5]
println(a.reduce(0, +)) // prints 15
println(a.reduce(1, *)) // prints 120

println(reduce(a, 0, +)) // prints 15
println(reduce(a, 1, *)) // prints 120
```



## Tcl


```tcl
set arr [list 3 6 8]
set sum [expr [join $arr +]]
set prod [expr [join $arr *]]
```

```tcl
set arr [list 3 6 8]
set sum [tcl::mathop::+ {*}$arr]
set prod [tcl::mathop::* {*}$arr]
```


=={{header|TI-83 BASIC}}==
Use the built-in functions <code>sum()</code> and <code>prod()</code>.

```ti83b
seq(X,X,1,10,1)→L₁
{1 2 3 4 5 6 7 8 9 10}
sum(L₁)
55
prod(L₁)
3628800
```



## Toka


```toka
4 cells is-array foo

212 1 foo array.put
51 2 foo array.put
12 3 foo array.put
91 4 foo array.put

[ ( array size -- sum )
  >r 0 r> 0 [ over i swap array.get + ] countedLoop nip ] is sum-array

 ( product )
reset 1 4 0 [ i foo array.get * ] countedLoop .
```



## Trith


```trith
[1 2 3 4 5] 0 [+] foldl
```


```trith
[1 2 3 4 5] 1 [*] foldl
```


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
list="1'2'3'4'5"
sum=SUM(list)
PRINT "    sum: ",sum

product=1
LOOP l=list
product=product*l
ENDLOOP
PRINT "product: ",product

```

```txt

    sum: 15
product: 120

```



## UNIX Shell

From an internal variable, $IFS delimited:


```bash
sum=0
prod=1
list="1 2 3"
for n in $list
do sum="$(($sum + $n))"; prod="$(($prod * $n))"
done
echo $sum $prod
```


From the argument list (ARGV):


```bash
sum=0
prod=1
for n
do sum="$(($sum + $n))"; prod="$(($prod * $n))"
done
echo $sum $prod
```


From STDIN, one integer per line:


```bash
sum=0
prod=1
while read n
do sum="$(($sum + $n))"; prod="$(($prod * $n))"
done
echo $sum $prod
```


From variable:


```bash
LIST='20 20 2';
SUM=0; PROD=1;
for i in $LIST; do
  SUM=$[$SUM + $i]; PROD=$[$PROD * $i];
done;
echo $SUM $PROD
```



## UnixPipes

Uses [[ksh93]]-style process substitution.
```bash
prod() {
   (read B; res=$1; test -n "$B" && expr $res \* $B || echo $res)
}

sum() {
   (read B; res=$1; test -n "$B" && expr $res + $B || echo $res)
}

fold() {
   (func=$1; while read a ; do fold $func | $func $a ; done)
}


(echo 3; echo 1; echo 4;echo 1;echo 5;echo 9) |
  tee >(fold sum) >(fold prod) > /dev/null
```


There is a race between <code>fold sum</code> and <code>fold prod</code>, which run in parallel. The program might print sum before product, or print product before sum.


## Ursa

Ursa doesn't have arrays in the traditional sense. Its equivalent is the stream. All math operators take streams as arguments, so sums and products of streams can be found like this.

```ursa>declare int<
 stream
append 34 76 233 8 2 734 56 stream

# outputs 1143
out (+ stream) endl console

# outputs 3.95961079808E11
out (* stream) endl console
```



## Ursala

The reduction operator, :-, takes an associative binary function and a constant for the empty case.
Natural numbers are unsigned and of unlimited size.

```Ursala
#import nat
#cast %nW

sp = ^(sum:-0,product:-1) <62,43,46,40,29,55,51,82,59,92,48,73,93,35,42,25>
```


```txt
(875,2126997171723931187788800000)
```



## V


```v
[sp dup 0 [+] fold 'product=' put puts 1 [*] fold 'sum=' put puts].
```


```v
[1 2 3 4 5] sp
=
product=15
sum=120
```



## Vala


```vala

public static void main(){
	int sum = 0, product = 1;

	int[] array = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

	foreach (int number in array){
		sum += number;
		product *= number;
	}
}

```




## VBA

Assumes Excel is used.

```vb
Sub Demo()
Dim arr
    arr = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    Debug.Print "sum : " & Application.WorksheetFunction.Sum(arr)
    Debug.Print "product : " & Application.WorksheetFunction.Product(arr)
End Sub
```
```txt
sum : 55
product : 3628800
```



## VBScript


```vb
Function sum_and_product(arr)
	sum = 0
	product = 1
	For i = 0 To UBound(arr)
		sum = sum + arr(i)
		product = product * arr(i)
	Next
	WScript.StdOut.Write "Sum: " & sum
	WScript.StdOut.WriteLine
	WScript.StdOut.Write "Product: " & product
	WScript.StdOut.WriteLine
End Function

myarray = Array(1,2,3,4,5,6)
sum_and_product(myarray)

```


```txt

Sum: 21
Product: 720

```



## Visual Basic .NET

```vbnet
Module Program
    Sub Main()
        Dim arg As Integer() = {1, 2, 3, 4, 5}
        Dim sum = arg.Sum()
        Dim prod = arg.Aggregate(Function(runningProduct, nextFactor) runningProduct * nextFactor)
    End Sub
End Module
```



## Wart


```wart
def (sum_prod nums)
  (list (+ @nums) (* @nums))
```



## WDTE


```WDTE>let a =
 import 'arrays';
let s => import 'stream';

let sum array => a.stream array -> s.reduce 0 +;
let prod array => a.stream prod -> s.reduce 1 *;
```



## Wortel


```wortel
@sum [1 2 3 4] ; returns 10
@prod [1 2 3 4] ; returns 24
```



## XPL0


```XPL0
code CrLf=9, IntOut=11;

func SumProd(A, L);
int  A, L;
int  S, P, I;
[S:= 0;  P:= 1;
for I:= 0 to L-1 do [S:= S+A(I);  P:= P*A(I)];
IntOut(0, S);  CrLf(0);
IntOut(0, P);  CrLf(0);
]; \SumSq

SumProd([1,2,3,4,5,6,7,8,9,10], 10)
```


```txt

55
3628800

```



## XSLT

XSLT (or XPath rather) has a few built-in functions for reducing from a collection, but product is not among them. Because of referential transparency, one must resort to recursive solutions for general iterative operations upon collections. The following code represents the array by numeric values in <price> elements in the source document.


```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" />

  <xsl:template name="sum-prod">
    <xsl:param name="values" />
    <xsl:param name="sum"  select="0" />
    <xsl:param name="prod" select="1" />
    <xsl:choose>
      <xsl:when test="not($values)">
        <xsl:text>
Sum: </xsl:text>
        <xsl:value-of select="$sum" />
        <xsl:text>
Product: </xsl:text>
        <xsl:value-of select="$prod" />
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="sum-prod">
          <xsl:with-param name="values" select="$values[position() > 1]" />
          <xsl:with-param name="sum"  select="$sum  + $values[1]" />
          <xsl:with-param name="prod" select="$prod * $values[1]" />
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="/">
     <xsl:text>
Sum (built-in): </xsl:text>
    <xsl:value-of select="sum(//price)" />

    <xsl:call-template name="sum-prod">
      <xsl:with-param name="values" select="//price" />
    </xsl:call-template>
  </xsl:template>
</xsl:stylesheet>
```



## zkl

```zkl
fcn sum(vals){vals.reduce('+,0)}
fcn product(vals){vals.reduce('*,1)}
```


```txt

sum(T(1,2,3,4))     //-->10
product(T(1,2,3,4)) //-->24

```
