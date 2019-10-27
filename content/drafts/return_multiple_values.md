+++
title = "Return multiple values"
description = ""
date = 2019-10-18T18:33:01Z
aliases = []
[extra]
id = 10654
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}} 
[[Category:Functions and subroutines]]
{{omit from|GUISS}}

;Task:
Show how to return more than one value from a function.





## ACL2


```Lisp
;; To return multiple values:
(defun multiple-values (a b)
   (mv a b))

;; To extract the values:
(mv-let (x y)
        (multiple-values 1 2)
   (+ x y))
```






## Ada

Ada functions can only return one type. 
That type could be an array or record holding multiple values, 
but the usual method for returning several values is using 
a procedure with 'out' parameters.
By default, all parameters are 'in', but can also be 'out', 'in out' and 'access'. Writing to an 'out' parameter simply changes the value of the variable passed to the procedure.

```Ada

with Ada.Text_IO; use Ada.Text_IO;
procedure MultiReturn is
   procedure SumAndDiff (x, y : Integer; sum, diff : out Integer) is begin
      sum := x + y;
      diff := x - y;
   end SumAndDiff;
   inta : Integer := 5;
   intb : Integer := 3;
   thesum, thediff : Integer;
begin
   SumAndDiff (inta, intb, thesum, thediff);
   Put_Line ("Sum:" & Integer'Image (thesum));
   Put_Line ("Diff:" & Integer'Image (thediff));
end MultiReturn;

```

{{out}}

```txt

Sum: 8
Diff: 2

```



## Agena

Agena allows functions to return multiple values.


Tested with Agena 2.9.5 Win32

```agena
# define a function returning three values
mv := proc() is
    return 1, 2, "three"
end ; # mv

scope # test the mv() proc
    local a, b, c := mv();
    print( c, b, a )
epocs
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.6.win32}}
Procedures in Algol 68 can only return one value, so to return multiple values, 
a structure (or array if all the values have the same mode) can be used.

```algol68
# example mode for returning multiple values from a procedure #
MODE PAIR = STRUCT( STRING name, INT value );

# procedure returning multiple values via a structure #
PROC get pair = ( INT a )PAIR:
    CASE a
    IN #1#    ( "H",  0 )
    ,  #2#    ( "He", 1 )
    ,  #3#    ( "Li", 3 )
    OUT       ( "?",  a )
    ESAC
;

main: (
    # use the result as a whole #
    print( ( get pair( 3 ), newline ) );
    # access the components separately #
    print( ( name OF get pair( 1 ), value OF get pair( 2 ), newline ) )
)
```

{{out}}

```txt

Li         +3
H         +1

```


## ALGOL W

Algol W procedures can't return arrays but records can be used to return multiple values.

```algolw
begin
    % example using a record type to return multiple values from a procedure %
    record Element ( string(2) symbol; integer atomicNumber );
    reference(Element) procedure getElement( integer value n ) ;
    begin
        Element( if      n < 1 then   "?<"
                 else if n > 3 then   "?>"
                 else case n of ( %1% "H"
                                , %2% "He"
                                , %3% "Li"
                                )
               , n
               )
    end getElement ;
    % test the procedure %
    begin
        reference(Element) elementData;
        for n := 0 until 4 do begin
            elementData := getElement(n);
            write( s_w := 0, i_w := 1
                 , atomicNumber(elementData)
                 , " "
                 , symbol(elementData)
                 );
        end
    end

end.
```


## ANSI Standard BASIC

The most straightforward way of returning multiple values is to specify them as parameters.

```ANSI Standard BASIC
100 DECLARE EXTERNAL SUB sumdiff
110 !
120 CALL sumdiff(5, 3, sum, diff)
130 PRINT "Sum is "; sum
140 PRINT "Difference is "; diff
150 END
160 !
170 EXTERNAL SUB sumdiff(a, b, c, d)
180 LET c = a + b
190 LET d = a - b
200 END SUB
```



## ATS

Every function returns one value. The conventional way to return multiple values is to return a tuple.

```ATS
//
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

fun addsub
(
  x: int, y: int
) : (int, int) = (x+y, x-y)

(* ****** ****** *)

implement
main0 () = let
  val (sum, diff) = addsub (33, 12)
in
  println! ("33 + 12 = ", sum);
  println! ("33 - 12 = ", diff);
end (* end of [main0] *)
```



## AutoHotkey

{{works with|AutoHotkey_L}}
Functions may return one value. The conventional way to return multiple values is to bundle them into an Array.

```AutoHotkey
addsub(x, y) {
  return [x + y, x - y]
}
```



## AutoIt

Return an array.

```AutoIt

Func _AddSub($iX, $iY)
Local $aReturn[2]
$aReturn[0] = $iX + $iY
$aReturn[1] = $iX - $iY
Return $aReturn
EndFunc

```



## BASIC

=
## BaCon
=
BaCon can return homogeneous dynamic arrays, or RECORD data holding heterogeneous types.


```freebasic
' Return multiple values
RECORD multi
    LOCAL num
    LOCAL s$[2]
END RECORD

FUNCTION f(n) TYPE multi_type
    LOCAL r = { 0 } TYPE multi_type
    r.num = n
    r.s$[0] = "Hitchhiker's Guide"
    r.s$[1] = "Douglas Adams"
    RETURN r
END FUNCTION

DECLARE rec TYPE multi_type
rec = f(42)
PRINT rec.num
PRINT rec.s$[0]
PRINT rec.s$[1]
```


{{out}}

```txt
prompt$ ./return-multiple
42
Hitchhiker's Guide
Douglas Adams
```


=
## BBC BASIC
=
The most straightforward way of returning multiple values is to specify them as RETURNed parameters.

```bbcbasic
      PROCsumdiff(5, 3, sum, diff)
      PRINT "Sum is " ; sum
      PRINT "Difference is " ; diff
      END
      
      DEF PROCsumdiff(a, b, RETURN c, RETURN d)
      c = a + b
      d = a - b
      ENDPROC
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 NUMERIC SUM,DIFF
110 CALL SUMDIFF(5,3,SUM,DIFF)
120 PRINT "Sum is";SUM:PRINT "Difference is";DIFF
130 END 
140 DEF SUMDIFF(A,B,REF C,REF D)
150   LET C=A+B:LET D=A-B
160 END DEF
```



## Bracmat

{{trans|Haskell}}
Every function returns one value. The conventional way to return multiple values is to return a tuple.

```bracmat
(addsub=x y.!arg:(?x.?y)&(!x+!y.!x+-1*!y));
```

You can use pattern matching to extract the components:

```bracmat
( addsub$(33.12):(?sum.?difference)
& out$("33 + 12 = " !sum)
& out$("33 - 12 = " !difference)
);
```

{{out}}

```txt
33 + 12 =  45
33 - 12 =  21
```



## C

C has structures which can hold multiple data elements of varying types.

```c>#include<stdio.h


typedef struct{
	int integer;
	float decimal;
	char letter;
	char string[100];
	double bigDecimal;
}Composite;

Composite example()
{
	Composite C = {1, 2.3, 'a', "Hello World", 45.678};
	return C;
}


int main()
{
	Composite C = example();

	printf("Values from a function returning a structure : { %d, %f, %c, %s, %f}\n", C.integer, C.decimal, C.letter, C.string, C.bigDecimal);

	return 0;
}
```

{{out}}

```txt

Values from a function returning a structure : { 1, 2.300000, a, Hello World, 45.678000}

```


C99 and above also allow structure literals to refer to the name, rather than position, of the element to be initialized:

```c>#include <stdio.h


typedef struct {
    char *first, *last;
} Name;

Name whatsMyName() {
    return (Name) {
        .first = "James",
        .last = "Bond",
    };
}

int main() {
    Name me = whatsMyName();
    printf("The name's %s. %s %s.\n", me.last, me.first, me.last);
    return 0;
}
```

{{out}}

```txt
The name's Bond. James Bond.

```



## C++

Since C++11, the C++-standard-library includes tuples, as well as an easy way to destructure them.

```cpp>#include <algorithm

#include <array>
#include <cstdint>
#include <iostream>
#include <tuple>

std::tuple<int, int> minmax(const int * numbers, const std::size_t num) {
   const auto maximum = std::max_element(numbers, numbers + num);
   const auto minimum = std::min_element(numbers, numbers + num);
   return std::make_tuple(*minimum, *maximum) ;
}

int main( ) {
   const auto numbers = std::array<int, 8>{{17, 88, 9, 33, 4, 987, -10, 2}};
   int min{};
   int max{};
   std::tie(min, max) = minmax(numbers.data(), numbers.size());
   std::cout << "The smallest number is " << min << ", the biggest " << max << "!\n" ;
}
```

{{out}}
<PRE>The smallest number is -10, the biggest 987!</PRE>

=={{header|C sharp|C#}}==
The preferred way to return multiple values in C# is to use "out" paremeters on the method. This can be in addition to the value returned by the method.

```c sharp
using System;
using System.Collections.Generic;
using System.Linq;

class ReturnMultipleValues
{
    static void Main()
    {
        var values = new[] { 4, 51, 1, -3, 3, 6, 8, 26, 2, 4 };
        int max, min;
        MinMaxNum(values, out max, out min);

        Console.WriteLine("Min: {0}\nMax: {1}", min, max);
    }

    static void MinMaxNum(IEnumerable<int> nums, out int max, out int min)
    {
        var sortedNums = nums.OrderBy(num => num).ToArray();
        max = sortedNums.Last();
        min = sortedNums.First();
    }
}
```

{{out}}

```txt
Min: -3
Max: 51
```



## Clipper

Every function returns one value. 
The conventional way to return multiple values is to bundle them into an array.

```Clipper
Function Addsub( x, y )
Return { x+y, x-y }
```



## Clojure

Multiple values can be returned by packaging them in a vector. 
At receiving side, these arguments can be obtained individually by using [http://blog.jayfields.com/2010/07/clojure-destructuring.html destructuring].

```clojure
(defn quot-rem [m n] [(quot m n) (rem m n)])

; The following prints 3 2.
(let [[q r] (quot-rem 11 3)]
  (println q)
  (println r))
```
 
In complex cases, it would make more sense to return a map, which can be destructed in a similar manner.

```clojure
(defn quot-rem [m n]
  {:q (quot m n)
   :r (rem m n)})

; The following prints 3 2.
(let [{:keys [q r]} (quot-rem 11 3)]
  (println q)
  (println r))
```



## CMake


```cmake
# Returns the first and last characters of string.
function(firstlast string first last)
  # f = first character.
  string(SUBSTRING "${string}" 0 1 f)

  # g = last character.
  string(LENGTH "${string}" length)
  math(EXPR index "${length} - 1")
  string(SUBSTRING "${string}" ${index} 1 g)

  # Return both characters.
  set("${first}" "${f}" PARENT_SCOPE)
  set("${last}" "${g}" PARENT_SCOPE)
endfunction(firstlast)

firstlast("Rosetta Code" begin end)
message(STATUS "begins with ${begin}, ends with ${end}")
```



## COBOL

COBOL normally passes data <code>BY REFERENCE</code>, which is the default
mode, effectively making the arguments modifiable.

User Defined Functions return a single argument, but that argument can be a
group item.

Most large scale COBOL programs will attempt to keep from repeating itself,
in terms of data layouts, using external copy books and the COBOL COPY
statement.  ''This example uses in source REPLACE to avoid copy books.''

{{works with|GnuCOBOL}}


```COBOL

       identification division.
       program-id. multiple-values.

       environment division.
       configuration section.
       repository.
           function multiples
           function all intrinsic.

       REPLACE ==:linked-items:== BY ==
       01 a usage binary-long.
       01 b pic x(10).
       01 c usage float-short.
       ==
       ==:record-item:== BY ==
       01 master.
          05 ma usage binary-long.
          05 mb pic x(10).
          05 mc usage float-short.
       ==.

       data division.
       working-storage section.
       :linked-items:

       :record-item:
       
       procedure division.
       sample-main.

       move 41 to a
       move "aaaaabbbbb" to b
       move function e to c

       display "Original: " a ", " b ", " c
       call "subprogram" using a b c
       display "Modified: " a ", " b ", " c
       
       move multiples() to master
       display "Multiple: " ma ", " mb ", " mc

       goback.
       end program multiple-values.

      *> subprogram
       identification division.
       program-id. subprogram.

       data division.
       linkage section.
       :linked-items:

       procedure division using a b c.
       add 1 to a
       inspect b converting "a" to "b"
       divide 2 into c
       goback.
       end program subprogram.

      *> multiples function
       identification division.
       function-id. multiples.

       data division.
       linkage section.
       :record-item:

       procedure division returning master.
       move 84 to ma
       move "multiple" to mb
       move function pi to mc
       goback.
       end function multiples.

```


{{out}}

```txt

prompt$ cobc -xj multiple-values.cob
Original: +0000000041, aaaaabbbbb, 2.7182817
Modified: +0000000042, bbbbbbbbbb, 1.3591409
Multiple: +0000000084, multiple  , 3.1415927

```



## Common Lisp

Besides the obvious method of passing around a list, Common Lisp also allows a function to return multiple values.  When citing the return values, if no interest is shown for multiple values, only the first (the primary return value) is used. Multiple values are not a data structure such as a tuple, list or array. They are a true mechanism for returning multiple values.

Returning a single value is accomplished by evaluating an expression (which itself yields a single value) at the end of a body of forms.

```lisp
(defun return-three ()
  3)
```

The next possibility is that of returning no values at all. For this, the <code>values</code> function is used, with no arguments:

```lisp
(defun return-nothing ()
  (values))
```

To combine the values of multiple expressions into a multi-value return, <code>values</code> is used with arguments. The following is from an interactive [[CLISP]] session. CLISP's listener shows multiple values separated by a semicolon:

```lisp
[1]> (defun add-sub (x y) (values-list (list (+ x y) (- x y))))
ADD-SUB
[2]> (add-sub 4 2)    ; 6 (primary) and 2
6 ;
2
[3]> (add-sub 3 1)    ; 4 (primary) and 2
4 ;
2
[4]> (+ (add-sub 4 2) (add-sub 3 1))  ; 6 + 4
10
[5]> (multiple-value-call #'+ (add-sub 4 2) (add-sub 3 1)) ; 6+2+4+2
14
```

What happens if something tries to use the value of a form which returned <code>(values)</code>? In this case the behavior defaults to taking the value <code>nil</code>:

```lisp
(car (values)) ;; no error: same as (car nil)
```

What if the <code>values</code> function is applied to some expressions which also yield multiple values, or which do not yield any values? The answer is that only the primary value is taken from each expression, or the value <code>nil</code> for any expression which did not yield a value:

```lisp
(values (values 1 2 3) (values) 'a)
```

yields three values:

```txt
-> 1; NIL; A
```

This also means that <code>values</code> can be used to reduce a multiple value to a single value:

```lisp
;; return exactly one value, no matter how many expr returns,
;; nil if expr returns no values
(values expr)
```

Multiple values are extracted in several ways.

1. Binding to variables:

```lisp
(multiple-value-bind (dividend remainder) (truncate 16 3)
  ;; in this scope dividend is 5; remainder is 1
  )
```


2. Conversion to a list:

```lisp
(multiple-value-list (truncate 16 3)) ;; yields (5 1)
```


3. Reification of multiple values as arguments to another function:

```lisp
;; pass arguments 5 1 to +, resulting in 6:
(multiple-value-call #'+ (truncate 16 3))
```


4. Assignment to variables:

```lisp
;; assign 5 to dividend, 1 to remainder:
(multiple-value-setq (dividend remainder) (truncate 16 1))
```

<code>(values ...)</code> syntax is treated as a multiple value place by <code>setf</code> and other operators, allowing the above to be expressed this way:

```lisp
(setf (values dividend remainder) (truncate 16 1))
```



## D


```d
import std.stdio, std.typecons, std.algorithm;


mixin template ret(string z) {
    mixin({
        string res;

        auto r = z.split(" = ");
        auto m = r[0].split(", ");
        auto s = m.join("_");

        res ~= "auto " ~ s ~ " = " ~ r[1] ~ ";";
        foreach(i, n; m){
            res ~= "auto " ~ n ~ " = " ~ s ~ "[" ~ i.to!string ~ "];\n";
        }
        return res;
    }());
}

auto addSub(T)(T x, T y) {
    return tuple(x + y, x - y);
}

void main() {
    mixin ret!q{ a, b = addSub(33, 12) };

    writefln("33 + 12 = %d\n33 - 12 = %d", a, b);
}
```

{{out}}

```txt
33 + 12 = 45
33 - 12 = 21
```



## Dc

Define a divmod macro <code>~</code> which takes <code>a b</code> on the stack and returns <code>a/b a%b</code>.

```dc
[ S1 S2 l2 l1 / L2 L1 % ] s~
1337 42 l~ x f
```

{{out}}

```txt
35
31
```


=={{header|Déjà Vu}}==

```dejavu
function-returning-multiple-values:
     10 20

!print !print function-returning-multiple-values

```

{{out}}

```txt
10
20
```


=={{header|Delphi}}/{{header|Pascal}}==
Delphi functions return a single value, but var parameters of a function or procedure can be modified and act as return values.

```Delphi
program ReturnMultipleValues;

{$APPTYPE CONSOLE}

procedure GetTwoValues(var aParam1, aParam2: Integer);
begin
  aParam1 := 100;
  aParam2 := 200;
end;

var
  x, y: Integer;
begin
  GetTwoValues(x, y);
  Writeln(x);
  Writeln(y);
end.
```



## Dyalect


A typical way to return multiple values in Dyalect is to use tuples:


```Dyalect
func divRem(x, y) {
    (x / y, x % y)
}
```



## EchoLisp

One can return the result of the '''values''' function, or a list.

```scheme

(define (plus-minus x y)
    (values (+ x y) (- x y)))
(plus-minus 3 4)
    → 7
     -1

(define (plus-minus x y)
    (list (+ x y) (- x y)))
(plus-minus 3 4)
    → (7 -1)

```



## ECL

<lang>MyFunc(INTEGER i1,INTEGER i2) := FUNCTION
  RetMod := MODULE
    EXPORT INTEGER Add  := i1 + i2;
    EXPORT INTEGER Prod := i1 * i2;
  END;
  RETURN RetMod;
END;

//Reference each return value separately:
MyFunc(3,4).Add;
MyFunc(3,4).Prod;

```



## Eiffel

Every function returns one value. Multiple values can be returned in a tuple.

```Eiffel
some_feature: TUPLE
	do
		Result := [1, 'j', "r"]
	end
```

Greater control over the type of return values can also be enforced by explicitly declaring the type of the generic parameters.

```Eiffel
some_feature: TUPLE[INTEGER_32, CHARACTER_8, STRING_8]
	do
		--Result := [ ]			-- compile error	
		--Result := [1, "r", 'j']	-- also compile error	
		Result := [1, 'j', "r"]		-- okay
		Result := [1, 'j', "r", 1.23]	-- also okay
	end
```


## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;
 
extension op
{
    MinMax(ref int minVal, ref int maxVal)
    {
        var ordered := self.ascendant();
 
        minVal := ordered.FirstMember;
        maxVal := ordered.LastMember
    }
}
 
public program()
{
    var values := new int[]::(4, 51, 1, -3, 3, 6, 8, 26, 2, 4);
 
    values.MinMax(ref int min, ref int max);
 
    console.printLine("Min: ",min," Max: ",max)
}
```

{{out}}

```txt

Min: -3 Max: 51

```



## Elixir

Elixir returns in the tuple form when returning more than one value.

```elixir
defmodule RC do
  def addsub(a, b) do
    {a+b, a-b}
  end
end

{add, sub} = RC.addsub(7, 4)
IO.puts "Add: #{add},\tSub: #{sub}"
```


{{out}}

```txt

Add: 11,        Sub: 3

```



## Erlang


```erlang
% Put this code in return_multi.erl and run it as "escript return_multi.erl"

-module(return_multi).

main(_) ->
        {C, D, E} = multiply(3, 4),
        io:format("~p ~p ~p~n", [C, D, E]).

multiply(A, B) ->
        {A * B, A + B, A - B}.

```

{{out}}

```txt
12 7 -1

```



## ERRE

FUNCTIONs in ERRE language return always a single value, but PROCEDUREs can return multiple values defining a parameter output list in procedure declaration using '->' separator.

```ERRE

PROGRAM RETURN_VALUES

PROCEDURE SUM_DIFF(A,B->C,D)
   C=A+B
   D=A-B
END PROCEDURE

BEGIN
   SUM_DIFF(5,3->SUM,DIFF)
   PRINT("Sum is";SUM)
   PRINT("Difference is";DIFF)
END PROGRAM

```



## Euphoria

Any Euphoria object can be returned. A sequence of objects can be returned, made from multiple data types as in this example.

```euphoria
include std\console.e --only for any_key, to help make running this program easy on windows GUI

integer aWholeNumber = 1
atom aFloat = 1.999999
sequence aSequence = {3, 4}
sequence result = {} --empty initialized sequence

function addmultret(integer first, atom second, sequence third)--takes three kinds of input, adds them all into one element of the.. 
    return (first + second + third[1]) + third[2] & (first * second * third[1]) * third[2] --..output sequence and multiplies them into..
end function --..the second element

result = addmultret(aWholeNumber, aFloat, aSequence) --call function, assign what it gets into result - {9.999999, 23.999988}
? result
any_key()
```


{{out}}

```txt
{9.999999,23.999988}
Press Any Key to continue...
```


=={{header|F_Sharp|F#}}==
A function always returns exactly one value. 
To return multiple results, they are typically packed into a tuple:

```fsharp
let addSub x y = x + y, x - y

let sum, diff = addSub 33 12
printfn "33 + 12 = %d" sum
printfn "33 - 12 = %d" diff
```


Output parameters from .NET APIs are automatically converted to tuples by the compiler.
It is also possible to use output parameters explicitly with the <code>byref</code> keyword, but this is rarely necessary.


## Factor

With stack-oriented languages like Factor, a function returns multiple values by pushing them on the data stack. 
For example, this word ''*/'' pushes both x*y and x/y.

```factor
USING: io kernel math prettyprint ;
IN: script

: */ ( x y -- x*y x/y )
    [ * ] [ / ] 2bi ;

15 3 */

[ "15 * 3 = " write . ]
[ "15 / 3 = " write . ] bi*
```

Its stack effect declares that ''*/'' always returns 2 values. To return a variable number of values, a word must bundle those values into a [[sequence]] (perhaps an array or vector). For example, ''factors'' (defined in ''math.primes.factors'' and demonstrated at [[Prime decomposition#Factor]]) returns a sequence of prime factors.


## FALSE


```false
[\$@$@*@@/]f: { in: a b, out: a*b a/b }
6 2f;! .` ,.   { 3 12 }
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Return_multiple_values this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

It is natural to return multiple values on the parameter stack. Many built-in operators and functions do so as well ('''/mod''', '''open-file''', etc.).

```forth
: muldiv ( a b -- a*b a/b )
  2dup / >r * r> ;
```



## Fortran

{{trans|Haskell}}

```Fortran
module multiple_values
implicit none
type res
  integer :: p, m
end type

contains

function addsub(x,y) result(r)
  integer :: x, y
  type(res) :: r 
  r%p = x+y
  r%m = x-y
end function
end module

program main
  use multiple_values 
  print *, addsub(33, 22)
end program 

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' One way to return multiple values is to use ByRef parameters for the additional one(s)
Function tryOpenFile (fileName As String, ByRef fileNumber As Integer) As Boolean
   Dim result As Integer 
   fileNumber = FreeFile
   result = Open(fileName For Input As # fileNumber)
   If result <> 0 Then
     fileNumber = 0
     Return False
   Else
     Return True
   End If
End Function

Dim fn As Integer
Var b = tryOpenFile("xxx.zyz", fn) '' this file doesn't exist
Print b, fn
b = tryOpenFile("input.txt", fn) '' this file does exist
Print b, fn
Close # fn

' Another way is to use a user defined type

Type FileOpenInfo
  opened As Boolean
  fn As Integer
End Type

Function tryOpenFile2(fileName As String) As FileOpenInfo
   Dim foi As FileOpenInfo 
   foi.fn = FreeFile
   Dim result As Integer
   result = Open(fileName For Input As # foi.fn)   
   If  result <> 0 Then
     foi.fn = 0
     foi.opened = False
   Else
     foi.Opened = True
   End If
   Return foi
End Function

Print
Var foi = tryOpenFile2("xxx.zyz") 
Print foi.opened, foi.fn
foi = tryOpenFile2("input.txt")
Print foi.opened, foi.fn
Close # foi.fn

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

false          0
true           1

false          0
true           1

```



## Frink

The most common way of returning multiple values from a function is to return them as an array, which can be disassembled and set into individual variables on return.

```frink

divMod[a, b] := [a div b, a mod b]

[num, remainder] = divMod[10, 3]

```



## FunL

{{trans|Scala}}

```funl
def addsub( x, y ) = (x + y, x - y)

val (sum, difference) = addsub( 33, 12 )

println( sum, difference, addsub(33, 12) )
```


{{out}}


```txt

45, 21, (45, 21)

```



## FutureBasic

FutureBasic offers several ways to return multiple values from a function: by passing pointers to multiple values in and out of functions; global records (structures); global containers (imagine a global bit bucket that can hold up to 2GBs of data); and global arrays of either the standard kind, or of FB's dynamic arrays.

Here is an example of returning multiple values using pointers:

```futurebasic

include "ConsoleWindow"

local fn ReturnMultipleValues( strIn as Str255, strOut as ^Str255, letterCount as ^long )
dim as Str255 s

// Test if incoming string is empty, and exit function if it is
if strIn[0] == 0 then exit fn

// Prepend this string to incoming string and return it
s = "Here is your original string: "
strOut.nil$ = s + strIn

// Get length of combined string and return it
// Note: In FutureBasic string[0] is interchangeable with Len(string)
letterCount.nil& = strIn[0] + s[0]
end fn

dim as Str255 outStr
dim as long   outCount

fn ReturnMultipleValues( "Hello, World!", @outStr, @outCount )
print outStr; ". The combined strings have"; outCount; " letters in them."

```


Output:

```txt

Here is your original string: Hello, World!. The combined strings have 43 letters in them.

```


Another way to pass multiple values from a function is with records (AKA structures):
<lang>
include "ConsoleWindow"

// Elements in global array
_maxDim = 3

begin record Addresses
dim as Str63 name
dim as Str15 phone
dim as long zip
end record

begin globals
dim as Addresses  gAddressData(_maxDim) 
end globals

local fn FillRecord( array(_maxDim) as Addresses )
array.name(0) = "John Doe"
array.name(1) = "Mary Jones"
array.name(2) = "Bill Smith

array.phone(0) = "555-359-4411"
array.phone(1) = "555-111-2211"
array.phone(2) = "555-769-8071"

array.zip(0) = 12543
array.zip(1) = 67891
array.zip(2) = 54321
end fn

// Pass address of global array to fill it
fn FillRecord( gAddressData(0) )

dim as short i

for i = 0 to 2
   print gAddressData.name(i); ", ";
   print gAddressData.phone(i); ", Zip:";
   print gAddressData.zip(i)
next

```


Output:

```txt

John Doe, 555-359-4411, Zip: 12543
Mary Jones, 555-111-2211, Zip: 67891
Bill Smith, 555-769-8071, Zip: 54321

```


You can also use global arrays to return multiple values from a function as in this example:
<lang>
include "ConsoleWindow"

// Elements in global array
_maxDim = 3

begin globals
dim as Str31  gAddressArray(_maxDim, _maxDim) 
end globals

local fn FillRecord( array(_maxDim, _maxDim) as Str31 )
array( 0, 0 ) = "John Doe"
array( 1, 0 ) = "Mary Jones"
array( 2, 0 ) = "Bill Smith

array( 0, 1 ) = "555-359-4411"
array( 1, 1 ) = "555-111-2211"
array( 2, 1 ) = "555-769-8071"

array( 0, 2 ) = "12543"
array( 1, 2 ) = "67891"
array( 2, 2 ) = "54321"
end fn

// Pass address of global array to fill it
fn FillRecord( gAddressArray( 0, 0 ) )

dim as short i, j

for i = 0 to 2
   j = 0
   print gAddressArray(i, j    ); ", ";
   print gAddressArray(i, j + 1); ", Zip: ";
   print gAddressArray(i, j + 1)
next

```


Output:

```txt

John Doe, 555-359-4411, Zip: 555-359-4411
Mary Jones, 555-111-2211, Zip: 555-111-2211
Bill Smith, 555-769-8071, Zip: 555-769-8071

```


Here is another example using FB's containers -- bit buckets that can hold up to 2GB of data contingent on system memory.
<lang>
include "ConsoleWindow"

begin globals
// An FB container can hold up to 2GB of data, contingent on system memory
dim as container gC1, gC2
end globals

local fn ReturnMultipleValuesInContainers
// Fill container with strings from inside function
gC1  = "Twas brillig, and the slithy toves" + chr$(13)
gC1 += "Did gyre and gimble in the wabe;"   + chr$(13)
gC1 += "All mimsy were the borogoves,"      + chr$(13)
gC1 += "And the mome raths outgrabe."       + chr$(13)
gC1 += "'Beware the Jabberwock, my son!"    + chr$(13)
gC1 += "The jaws that bite, the claws that catch!" + chr$(13)
gC1 += "Beware the Jubjub bird, and shun"   + chr$(13)
gC1 += "The frumious Bandersnatch!'"        + chr$(13)

// Fill another container with numbers
gC2  = "10254"+ chr$(13)
gC2 += "37"   + chr$(13)
gC2 += "64"   + chr$(13)
end fn

local fn ReturnNewMultipleValuesInContainers
gC1  = "Jabberwocky is gone, but here is some new text." + chr$(13)
gC2  = "1000000"
end fn

// Test to see containers are empty:
print gC1 : print gC2

// Fill the containers using a function
fn ReturnMultipleValuesInContainers

// Check results
print gC1 : print : print gC2

// Empty the containers
gC1 = "" : gC2 = ""

// Fill with another function
fn ReturnNewMultipleValuesInContainers

// Check the new results
print gC1 : print gC2

```


Output:

```txt


Twas brillig, and the slithy toves
Did gyre and gimble in the wabe;
All mimsy were the borogoves,
And the mome raths outgrabe.
'Beware the Jabberwock, my son!
The jaws that bite, the claws that catch!
Beware the Jubjub bird, and shun
The frumious Bandersnatch!'

10254
37
64

Jabberwocky is gone, but here is some new text.

1000000

```



## Go

Functions can return multiple values in Go:

```go
func addsub(x, y int) (int, int) {
  return x + y, x - y
}
```

Or equivalently using named return style:

```go
func addsub(x, y int) (sum, difference int) {
  sum = x + y
  difference = x - y
  return
}
```

When a function returns multiple values, you must assign to a comma-separated list of targets:

```go
sum, difference := addsub(33, 12)
fmt.Printf("33 + 12 = %d\n", sum)
fmt.Printf("33 - 12 = %d\n", difference)
```



## Groovy

In Groovy functions return one value. One way to return multiple ones is to use anonymous maps as a sort of tuple.

```groovy
def addSub(x,y) {
 [
  sum: x+y,
  difference: x-y
 ]
}
```

Result:

```groovy
addSub(10,12) 
 
["sum":22, "difference":-2]
```


And although Groovy functions only return one value, Groovy ''assignments'' of Iterable objects (lists, arrays, sets, etc.) can be distributed across multiple ''variables'', like this:


```groovy
def addSub2(x,y) {
  [ x+y , x-y ]
}

def (sum, diff) = addSub2(50, 5)
assert sum == 55
assert diff == 45
```


If there are fewer elements than variables, the leftover variables are assigned null. If there are more elements than variables, the last variable is assigned the collected remainder of the elements.


## Harbour

Every function returns one value. The conventional way to return multiple values is to bundle them into an array.

```visualfoxpro
FUNCTION Addsub( x, y )
   RETURN { x + y, x - y }
```



## Haskell

Every function returns one value. The conventional way to return multiple values is to return a tuple.

```haskell
  addsub x y = (x + y, x - y)
```

You can use pattern matching to extract the components:

```haskell
main = do
  let (sum, difference) = addsub 33 12
  putStrLn ("33 + 12 = " ++ show sum)
  putStrLn ("33 - 12 = " ++ show difference)
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon values range from simple atomic values like integers and strings to structures like lists, tables, sets, records.  The contents of structures are heterogeneous and any of them could be used to return multiple values all at once.  Additionally, generators are supported that return multiple results one at a time as needed.

The following examples return 1, 2, 3 in different ways:

```Icon
procedure retList() # returns as ordered list
return [1,2,3]
end

procedure retSet()             # returns as un-ordered list
insert(S := set(),3,1,2)  
return S
end

procedure retLazy()            # return as a generator
suspend 1|2|3
end

procedure retTable()           # return as a table
T := table()
T["A"] := 1
T["B"] := 2 
T["C"] := 3
return T
end

record retdata(a,b,c)

procedure retRecord()          # return as a record, least general method
return retdata(1,2,3)
end
```



## J

To return multiple values in J, you return an array which contains multiple values.  Since the only data type in J is array (this is an oversimplification, from some perspectives - but those issues are out of scope for this task), this is sort of like asking how to return only one value in another language.

```j
   1 2+3 4
4 6
```



## Java

{{trans|NetRexx}}

```Java
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

// 
### =======================================================================

public class RReturnMultipleVals {
  public static final String K_lipsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.";
  public static final Long   K_1024   = 1024L;
  public static final String L        = "L";
  public static final String R        = "R";

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public static void main(String[] args) throws NumberFormatException{
    Long nv_;
    String sv_;
    switch (args.length) {
      case 0:
        nv_ = K_1024;
        sv_ = K_lipsum;
        break;
      case 1:
        nv_ = Long.parseLong(args[0]);
        sv_ = K_lipsum;
        break;
      case 2:
        nv_ = Long.parseLong(args[0]);
        sv_ = args[1];
        break;
      default:
        nv_ = Long.parseLong(args[0]);
        sv_ = args[1];
        for (int ix = 2; ix < args.length; ++ix) {
          sv_ = sv_ + " " + args[ix];
        }
        break;
    }

    RReturnMultipleVals lcl = new RReturnMultipleVals();

    Pair<Long, String> rvp = lcl.getPairFromPair(nv_, sv_); // values returned in a bespoke object
    System.out.println("Results extracted from a composite object:");
    System.out.printf("%s, %s%n%n", rvp.getLeftVal(), rvp.getRightVal());

    List<Object> rvl = lcl.getPairFromList(nv_, sv_); // values returned in a Java Collection object
    System.out.println("Results extracted from a Java Colections \"List\" object:");
    System.out.printf("%s, %s%n%n", rvl.get(0), rvl.get(1));

    Map<String, Object> rvm = lcl.getPairFromMap(nv_, sv_); // values returned in a Java Collection object
    System.out.println("Results extracted from a Java Colections \"Map\" object:");
    System.out.printf("%s, %s%n%n", rvm.get(L), rvm.get(R));
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Return a bespoke object.
  // Permits any number and type of value to be returned
  public <T, U> Pair<T, U> getPairFromPair(T vl_, U vr_) {
    return new Pair<T, U>(vl_, vr_);
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Exploit Java Collections classes to assemble a collection of results.
  // This example uses java.util.List
  public List<Object> getPairFromList(Object nv_, Object sv_) {
    List<Object> rset = new ArrayList<Object>();
    rset.add(nv_);
    rset.add(sv_);
    return rset;
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Exploit Java Collections classes to assemble a collection of results.
  // This example uses java.util.Map
  public Map<String, Object> getPairFromMap(Object nv_, Object sv_) {
    Map<String, Object> rset = new HashMap<String, Object>();
    rset.put(L, nv_);
    rset.put(R, sv_);
    return rset;
  }

  // 
### =====================================================================

  private static class Pair<L, R> {
    private L leftVal;
    private R rightVal;

    public Pair(L nv_, R sv_) {
      setLeftVal(nv_);
      setRightVal(sv_);
    }
    public void setLeftVal(L nv_) {
      leftVal = nv_;
    }
    public L getLeftVal() {
      return leftVal;
    }
    public void setRightVal(R sv_) {
      rightVal = sv_;
    }
    public R getRightVal() {
      return rightVal;
    }
  }
}
```

'''Otherwise'''

```Java
public class Values {
	private final Object[] objects;
	public Values(Object ... objects) {
		this.objects = objects;
	}
	public <T> T get(int i) {
		return (T) objects[i];
	}
	public Object[] get() {
		return objects;
	}
	
	// to test
	public static void main(String[] args) {
		Values v = getValues();
		int i = v.get(0);
		System.out.println(i);
		printValues(i, v.get(1));
		printValues(v.get());
	}
	private static Values getValues() {
		return new Values(1, 3.8, "text");
	}
	private static void printValues(int i, double d) {
		System.out.println(i + ", " + d);
	}
	private static void printValues(Object ... objects) {
		for (int i=0; i<objects.length; i+=1) System.out.print((i==0 ? "": ", ") + objects[i]);
		System.out.println();
	}
}
```

{{out}}

```txt

1
1, 3.8
1, 3.8, text

```



## JavaScript

Javascript does not support multi-value bind until ECMAScript 6 is released (still a draft as of May 2015). The multi-value return is actually a destructured binding. Support may not be present yet in most implementations.

```JavaScript
//returns array with three values
var arrBind = function () {
  return [1, 2, 3]; //return array of three items to assign
};

//returns object with three named values
var objBind = function () {
  return {foo: "abc", bar: "123", baz: "zzz"};
};

//keep all three values
var [a, b, c] = arrBind();//assigns a => 1, b => 2, c => 3
//skip a value
var [a, , c] = arrBind();//assigns a => 1, c => 3
//keep final values together as array
var [a, ...rest] = arrBind();//assigns a => 1, rest => [2, 3]


//same return name
var {foo, bar, baz} = objBind();//assigns foo => "abc", bar => "123", baz => "zzz"
//different return name (ignoring baz)
var {baz: foo, buz: bar} = objBind();//assigns baz => "abc", buz => "123"
//keep rest of values together as object
var {foo, ...rest} = objBind();//assigns foo => "abc, rest => {bar: "123", baz: "zzz"}
```



## jq

jq supports streams of JSON values, so there are two main ways in which a function can return multiple values: as a stream, or as an array.  Using the same example given for the Julia entry: 
```jq
# To produce a stream:
def addsub(x; y): (x + y), (x - y);

# To produce an array:
def add_subtract(x; y): [ x+y, x-y ];

```

The builtin filter .[] streams its input if the input is an array, e.g. the expression <code>[1,2] | .[]</code> produces the stream:
```jq
 
1
2
```


## Julia


```julia
function addsub(x, y)
  return x + y, x - y
end
```


```txt
julia> addsub(10,4)
(14,6)
```



## Kotlin

Although Kotlin doesn't support tuples as such, it does have generic Pair and Triple types which can be used to return 2 or 3 values from a function. To return more values, a data class can be used. All of these types can be automatically destructured to separate named variables.

```scala
// version 1.0.6

/* implicitly returns a Pair<Int, Int>*/
fun minmax(ia: IntArray) = ia.min() to ia.max()

fun main(args: Array<String>) {
    val ia = intArrayOf(17, 88, 9, 33, 4, 987, -10, 2)
    val(min, max) = minmax(ia) // destructuring declaration
    println("The smallest number is $min")
    println("The largest  number is $max")
}
```


{{out}}

```txt

The smallest number is -10
The largest  number is 987

```



## Lasso


```Lasso
define multi_value() => {
	return (:'hello word',date)
}
// shows that single method call will return multiple values
// the two values returned are assigned in order to the vars x and y
local(x,y) = multi_value

'x: '+#x
'\ry: '+#y
```

{{out}}

```txt
x: hello word
y: 2013-11-06 01:03:47

```



## Liberty BASIC

Using a space-delimited string to hold the array. LB functions return only one numeric or string value, so the function returns a string from which can be separated the two desired values.

```lb
data$ ="5 6 7 22 9 3 4 8 7 6 3 -5 2 1 8 9"

a$ =minMax$( data$)
print " Minimum was "; word$( a$, 1, " "); " & maximum was "; word$( a$, 2, " ")

end

function minMax$( i$)
min = 1E6
max =-1E6
i =1
do
    t$    =word$( i$, i, " ")
    if t$ ="" then exit do
    v     =val( t$)
    min   =min( min, v)
    max   =max( max, v)
    i =i +1
loop until 0
minMax$ =str$( min) +" " +str$( max)
end function
```


```txt

 Minimum was -5 & maximum was 22

```



## Lily

No support for returning multiple values, but (similar to Scala), a Tuple can be returned.


```Lily
define combine(a: Integer, b: String): Tuple[Integer, String]
{
  return <[a, b]>
}
```


The current version (0.17) has no support for destructuring Tuple assigns.


## Lua


```lua
function addsub( a, b )
    return a+b, a-b
end

s, d = addsub( 7, 5 )
print( s, d )
```



## Maple


```Maple>
 sumprod := ( a, b ) -> (a + b, a * b):
> sumprod( x, y );
                               x + y, x y

> sumprod( 2, 3 );
                                  5, 6
```

The parentheses are needed here only because of the use of arrow ("->") notation to define the procedure.  One could do, instead:

```Maple
sumprod := proc( a, b ) a + b, a * b end:
```



## Mathematica


```Mathematica
addsub [x_,y_]:= List [x+y,x-y]
addsub[4,2]
```

{{out}}

```txt
{6,2}
```


=={{header|MATLAB}} / {{header|Octave}}== 

```Matlab
  function [a,b,c]=foo(d)
    a = 1-d; 
    b = 2+d; 
    c = a+b;
  end;  
  [x,y,z] = foo(5) 
```

{{out}}

```Matlab>  
 [x,y,z] = foo(5) 
  x = -4
  y =  7
  z =  3 
```



## Maxima


```maxima
f(a, b) := [a * b, a + b]$

[u, v]: f(5, 6);
[30, 11]
```



## Mercury

Mercury is a logic language.  
Its unification semantics permit any number of output parameters (the closest equivalent to return values).  
The sample code provided here centres on the <code>addsub/4</code> predicate.  
The <code>mode</code> statement identifies the first two parameters as input parameters and the last two as output parameters, thus, in effect, returning two results.  
In this case the first output parameter returns the sum of the two inputs and the second output returns the difference of the two inputs.


### addsub.m


```mercury
:- module addsub.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    command_line_arguments(Args, !IO),
    filter_map(to_int, Args, CleanArgs),
    (length(CleanArgs, 2) ->
        X = det_index1(CleanArgs,1),
        Y = det_index1(CleanArgs,2),
        addsub(X, Y, S, D),
        format("%d + %d = %d\n%d - %d = %d\n", 
               [i(X), i(Y), i(S), i(X), i(Y), i(D)], !IO)
    ;
        write_string("Please pass two integers on the command line.\n", !IO)
    ).

:- pred addsub(int::in, int::in, int::out, int::out) is det.
addsub(X, Y, S, D) :-
    S = X + Y,
    D = X - Y.

:- end_module addsub.
```



### Use and output


```txt
<nowiki>$ mmc addsub.m -E && ./addsub 100 999   
100 + 999 = 1099
100 - 999 = -899</nowiki>
```



### Functions and tuples

Mercury is also a functional language, thus a function-based implementation is also possible.  
Functions in Mercury can only return a single value, but Mercury allows the use of arbitrary tuples containing multiple heterogeneous ad-hoc values which is, for all practical purposes, the same thing.  
The above code can be modified so that the definition of <code>addsub/4</code> is now instead this function <code>addsub/2</code>:


```Mercury
:- func addsub(int, int) = {int, int}.
addsub(X, Y) = { X + Y, X - Y }.
```


Instead, now, of a predicate with two input and two output parameters of type <code>int</code>, addsub is a function that takes two <code>int</code> parameters and returns a tuple containing two <code>int</code> values.  The call to <code>addsub/4</code> in the above code is now replaced by this:


```Mercury
        {S, D} = addsub(X, Y),
```


All other code remains exactly the same as does the use and output of it.


### Functions and type constructors

It should be noted that tuples as a construct are generally frowned upon in Mercury, relying as they do on structural type equivalence instead of nominative.  
The preferred approach is either to have multiple explicit output parameters on predicates or to have an explicit named type that covers the multi-return needs.  

An example of this follows:


```Mercury
:- module addsub.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

:- type my_result ---> twin(int, int).

main(!IO) :-
    command_line_arguments(Args, !IO),
    filter_map(to_int, Args, CleanArgs),
    (length(CleanArgs, 2) ->
        X = det_index1(CleanArgs,1),
        Y = det_index1(CleanArgs,2),
        twin(S, D) = addsub(X, Y),
        format("%d + %d = %d\n%d - %d = %d\n",
               [i(X), i(Y), i(S), i(X), i(Y), i(D)], !IO)
    ;
        write_string("Please pass two integers on the command line.\n", !IO)
    ).

:- func addsub(int, int) = my_result.
addsub(X, Y) = twin(X + Y, X - Y).

:- end_module addsub.
```


Here the type <code>my_result</code> has been provided with a <code>twin/2</code> constructor that accepts two <code>int</code> values.  Use and output of the code is, again, exactly the same.

<code>addsub/2</code> explicitly constructs a <code>my_result</code> value with the paired calculations and this is deconstructed in the call in the main predicate through unification.  While the resulting code is slightly more verbose than the tuple-based version it is more strongly protected against type errors and is more explicit in its intent at the same time.


## Nemerle

To return multiple values in Nemerle, package them into a tuple.

```Nemerle
using System;
using System.Console;
using Nemerle.Assertions;

module MultReturn
{
    MinMax[T] (ls : list[T]) : T * T
      where T : IComparable
      requires ls.Length > 0 otherwise throw ArgumentException("An empty list has no extreme values.")
    {
        def greaterOf(a, b) { if (a.CompareTo(b) > 0) a else b }
        def lesserOf(a, b)  { if (a.CompareTo(b) < 0) a else b }
        
        (ls.FoldLeft(ls.Head, lesserOf), ls.FoldLeft(ls.Head, greaterOf)) // packing tuple
    }
    
    Main() : void
    {
        def nums = [1, 34, 12, -5, 4, 0];
        def (min, max) = MinMax(nums);                                   // unpacking tuple
        WriteLine($"Min of nums = $min; max of nums = $max");
    }
}
```



## NetRexx

While a NetRexx method can only return a single &quot;thing&quot; to it's caller that &quot;thing&quot; can be an object which may contain a great deal of information.  Typical return objects can be composite objects, Java Collection Class objects, NetRexx ''indexed strings'' etc.

Another common idiom inherited from [[REXX]] is the ability to collect the return data into a simple NetRexx string. Caller can then use the <tt>PARSE</tt> instruction to deconstruct the return value and assign the parts to separate variables.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

-- 
### =======================================================================

class RReturnMultipleVals public
  properties constant
    L = 'L'
    R = 'R'
    K_lipsum = 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.'
    K_1024 = 1024

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method RReturnMultipleVals() public
    return

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method main(args = String[]) public static
    arg = Rexx(args)
    parse arg nv_ sv_ .
    if \nv_.datatype('n') then nv_ = K_1024
    if sv_ = '' then sv_ = K_lipsum

    lcl = RReturnMultipleVals()

    rvr = lcl.getPair(nv_, sv_) -- multiple values returned as a string.  Use PARSE to extract values
    parse rvr val1 val2
    say 'Results extracted from a NetRexx string:'
    say val1',' val2
    say

    rvr = lcl.getPairFromRexx(nv_, sv_) -- values returned in a NetRexx indexed string
    say 'Results extracted from a NetRexx "indexed string":'
    say rvr[L]',' rvr[R]
    say

    rvp = lcl.getPairFromPair(nv_, sv_) -- values returned in a bespoke object
    say 'Results extracted from a composite object:'
    say rvp.getLeftVal',' rvp.getRightVal
    say

    rvl = lcl.getPairFromList(nv_, sv_) -- values returned in a Java Collection "List" object
    say 'Results extracted from a Java Colections "List" object:'
    say rvl.get(0)',' rvl.get(1)
    say

    rvm = lcl.getPairFromMap(nv_, sv_) -- values returned in a Java Collection "Map" object
    say 'Results extracted from a Java Colections "Map" object:'
    say rvm.get(L)',' rvm.get(R)
    say

    return

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- returns the values in a NetRexx string.
  --  Caller can the power of PARSE to extract the results
  method getPair(nv_, sv_) public returns Rexx
    return nv_ sv_

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- Return the values as members of a NetRexx indexed string
  method getPairFromRexx(nv_, sv_) public returns Rexx
    rval = ''
    rval[L] = nv_
    rval[R] = sv_
    return rval

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- Return a bespoke object.
  -- Permits any number and type of value to be returned
  method getPairFromPair(nv_, sv_) public returns RReturnMultipleVals.Pair
    rset = RReturnMultipleVals.Pair(nv_, sv_)
    return rset

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- Exploit Java Collections classes to assemble a collection of results.
  -- This example uses java.util.List
  method getPairFromList(nv_, sv_) public returns java.util.List
    rset = ArrayList()
    rset.add(nv_)
    rset.add(sv_)
    return rset

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- This example uses java.util.Map
  method getPairFromMap(nv_, sv_) public returns java.util.Map
    rset = HashMap()
    rset.put(L, nv_)
    rset.put(R, sv_)
    return rset

-- 
### =======================================================================

class RReturnMultipleVals.Pair dependent

  properties indirect
    leftVal
    rightVal

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method Pair(nv_ = parent.K_1024, sv_ = parent.K_lipsum) public
    setLeftVal(nv_)
    setRightVal(sv_)
    return

```



## Nim

Every function returns one value. We can return a tuple instead:

```nim
proc addsub(x, y): auto =
  (x + y, x - y)

var (a,b) = addsub(12, 15)
```

Or manipulate the parameters directly:

```nim
proc addsub(x, y: int, a, b: var int) =
  a = x + y
  b = x - y

var a, b: int
addsub(12, 15, a, b)
```



## Objeck

Easiest way to return multiple values is to use in/out objects. The language also supports returning collections.

```objeck
class Program {
  function : Main(args : String[]) ~ Nil {
    a := IntHolder->New(3); b := IntHolder->New(7);
    Addon(a,b);
    a->Get()->PrintLine(); b->Get()->PrintLine();
  }
  
  function : Addon(a : IntHolder, b : IntHolder) ~ Nil {
    a->Set(a->Get() + 2); b->Set(b->Get() + 13);
  }
}
```



## OCaml

Every function returns one value. The conventional way to return multiple values is to return a tuple.

```ocaml
let addsub x y =
  x + y, x - y
```

(Note that parentheses are not necessary for a tuple literal in OCaml.)

You can use pattern matching to extract the components:

```ocaml
let sum, difference = addsub 33 12 in
  Printf.printf "33 + 12 = %d\n" sum;
  Printf.printf "33 - 12 = %d\n" difference
```



## Oforth


Oforth uses a data stack. A function return is everything left on the stack when the function ends, so a function can return as many objects as needed : 

```Oforth
import: date

: returnFourValues 12 13 14 15 ;
: returnOneObject  [ 12, 13, 14, 15, [16, 17 ], Date now, 1.2, "abcd" ] ;

"Showing four values returned on the parameter stack:" println
returnFourValues .s clr

"\nShowing one object containing four values returned on the parameter stack:" println
returnOneObject .s clr
```


Output:

```txt

Showing four values returned on the parameter stack:
[1] (Integer) 15
[2] (Integer) 14
[3] (Integer) 13
[4] (Integer) 12

Showing one object containing four values returned on the parameter stack:
[1] (List) [12, 13, 14, 15, [16, 17], 2016-02-05 20:55:15,778, 1.2, abcd]

```



## ooRexx

Functions and methods in ooRexx can only have a single return value, but that return value can be some sort of collection or other object that contains multiple values.  For example, an array:

```ooRexx

r = addsub(3, 4)
say r[1] r[2]

::routine addsub
  use arg x, y
  return .array~of(x + y, x - y)

```

Output:

```txt

7 -1

```





## OxygenBasic

Demonstrated with vectors, using OOP and a pseudo-assign trick:


```oxygenbasic


'
### ======

class vector4
'
### ======


float w,x,y,z

method values(float fw,fx,fy,fz)
this <= fw, fx, fy, fz
end method

method values(vector4 *v)
this <= v.w, v.x, v.y, v.z
end method

method values() as vector4
return this
end method

method ScaledValues(float fw,fx,fy,fz) as vector4
static vector4 v
v <= w*fw, x*fx, y*fy, z*fz
return v
end method

method ShowValues() as string
string cm=","
return w cm x cm y cm z
end method

end class

vector4 aa,bb

bb.values = 1,2,3,4

aa.values = bb.Values()

print aa.ShowValues() 'result 1,2,3,4

aa.values = bb.ScaledValues(100,100,-100,100)

print aa.ShowValues() 'result 100,200,-300,400 


```



## PARI/GP

The usual way to return multiple values is to put them in a vector:

```parigp
foo(x)={
  [x^2, x^3]
};
```



## Perl

Functions may return lists of values:

```perl
sub foo {
    my ($a, $b) = @_;
    return $a + $b, $a * $b;
}
```


## Perl 6


Each function officially returns one value, but by returning a List or Seq you can transparently return a list of arbitrary (even infinite) size. The calling scope can destructure the list using assignment, if it so chooses:


```perl6
sub addmul($a, $b) {
    $a + $b, $a * $b
}

my ($add, $mul) = addmul 3, 7;
```


In this example, the variable <tt>$add</tt> now holds the number 10, and <tt>$mul</tt> the number 21.


## Phix

Every function returns one value. You can return any number of items as elements of a sequence, and unpack them on receipt or not.

```Phix
function stuff()
    return {"PI",'=',3.1415926535}
end function
string what
integer op
object val
    {what,op,val} = stuff()

```



## PHP

Every function returns one value. The conventional way to return multiple values is to bundle them into an array.

```php
function addsub($x, $y) {
  return array($x + $y, $x - $y);
}
```

You can use the <code>list()</code> construct to assign to multiple variables:

```php
list($sum, $difference) = addsub(33, 12);
echo "33 + 12 = $sum\n";
echo "33 - 12 = $difference\n";
```


Additionally, if you specify a parameter as being a pointer, you do have the capacity to change that value. A built-in PHP example of this is <code>preg_match()</code> which returns a boolean value (to determine if a match was found or not), but which modifies the <code>$matches</code> parameter supplied to hold all the capture groups.

You can achieve this simply by adding the <code>&</code> before the desired parameter:

```php
function multiples($param1, &$param2) {
	if ($param1 == 'bob') {
		$param2 = 'is your grandmother';
		return true;
	}
	
	return false;
}

echo 'First run: ' . multiples('joe', $y) . "\r\n";
echo "Param 2 from first run: '${y}'\r\n";

echo 'Second run: ' . multiples('bob', $y) . "\r\n";
echo "Param 2 from second run: '${y}'\r\n";
```


The above will yield the following output:

```txt
First run: 
Param 2 from first run: ''
Second run: 1
Param 2 from second run: 'is your grandmother'
```



## PicoLisp

A PicoLisp function returns a single value. For multiple return values, a cons pair or a list may be used.

```PicoLisp
(de addsub (X Y)
   (list (+ X Y) (- X Y)) )
```

Test:

```PicoLisp
: (addsub 4 2)
-> (6 2)
: (addsub 3 1)
-> (4 2)
: (+ (car (addsub 4 2)) (car (addsub 3 1)))
-> 10
: (sum + (addsub 4 2) (addsub 3 1))
-> 14
```



## Pike

Multiple values are returned through an array.
An array can be assigned to separate variables.

```Pike
array(int) addsub(int x, int y)
{
    return ({ x+y, x-y });
}

[int z, int w] = addsub(5,4);
```



## PL/I

Example 1 illustrates a function that returns an array:

```PL/I
   define structure 1 h,
                      2 a (10) float;
   declare i fixed binary;

sub: procedure (a, b) returns (type(h));
   declare (a, b) float;
   declare p type (h);
   do i = 1 to 10;
      p.a(i) = i;
   end;
   return (p);
end sub;
```

Example 2 illustrates a function that returns a general data structure:

```PL/I
   define structure 1 customer,
                      2 name,
                        3 surname character (20),
                        3 given_name character (10),
                      2 address,
                        3 street character (20),
                        3 suburb character (20),
                        3 zip fixed decimal (7);

sub2: procedure() returns (type(customer));
   declare c type (customer);
   get edit (c.surname, c.given_name) (L);
   get edit (c.street, c.suburb, c.zip) (L);
   return (c);
end sub2;
```

Example 3 illustrates the return of two values as a complex value:

```PL/I
comp: procedure(a, b) returns (complex);
   declare (a, b) float;

   return (complex(a, b) );
end comp;
```



## PowerShell


```PowerShell

function multiple-value ($a, $b) {
    [pscustomobject]@{
        a = $a
        b = $b
    }
}
$m =  multiple-value "value" 1
$m.a
$m.b

```

<b>Output:</b>

```txt

value
1

```



## PureBasic

PureBasic's procedures return only a single value.  The value needs to be a standard numeric type or string.

An array, map, or list can be used as a parameter to a procedure and in the process contain values to be returned as well.  A pointer to memory or a structured variable may also be returned to reference multiple return values (requiring the memory to be manually freed afterwards).

```purebasic
;An array, map, or list can be used as a parameter to a procedure and in the
;process contain values to be returned as well.
Procedure example_1(x, y, Array r(1))  ;array r() will contain the return values
  Dim r(2) ;clear and resize the array
  r(0) = x + y  ;return these values in the array
  r(1) = x - y
  r(2) = x * y 
EndProcedure
  
;A pointer to memory or a structured variable may also be returned to reference
;multiple return values (requiring the memory to be manually freed afterwards).
Procedure example_2(x, y)
  Protected *result.POINT = AllocateMemory(SizeOf(POINT))
  *result\x = x
  *result\y = y

  ProcedureReturn *result ;*result points to a 'POINT' structure containing x and y
EndProcedure

If OpenConsole()
  Dim a(5)
  example_1(6, 5, a()) ;a() now contains {11, 1, 30}
  PrintN("Array returned with {" + Str(a(0)) + ", " + Str(a(1)) + ", " + Str(a(2)) + "}")
  
  Define *aPoint.POINT
  *aPoint = example_2(6, 5) ;*aPoint references structured memory containing {6, 5}
  
  PrintN("structured memory holds: (" + Str(*aPoint\x) + ", " + Str(*aPoint\y) + ")")
   FreeMemory(*aPoint) ;freememory
  
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```



## Python

Every function returns one value. The conventional way to return multiple values is to bundle them into a tuple.

```python
def addsub(x, y):
  return x + y, x - y
```

(Note that parentheses are not necessary for a tuple literal in Python.)

You can assign to a comma-separated list of targets:

```python
sum, difference = addsub(33, 12)
print "33 + 12 = %s" % sum
print "33 - 12 = %s" % difference
```

There is no discernible difference between "returning multiple values" and returning a single tuple of multiple values. It is just a more pedantic/accurate statement of the mechanism employed.


## R

The conventional way to return multiple values is to bundle them into a list.

```R
addsub <- function(x, y) list(add=(x + y), sub=(x - y))
```



## Racket

Racket has a defined function "values" that returns multiple values using continuations, a way it can be implemented is shown in "my-values"

```Racket
#lang racket
(values 4 5)

(define (my-values . return-list)
  (call/cc
   (lambda (return)
     (apply return return-list))))
```



## Raven


```Raven
define multiReturn use $v
   $v each 

3 multiReturn
```

{{out}}

```txt
2
1
0
```



## Retro

Functions take and return values via a stack. This makes returning multiple values easy.

```Retro
: addSubtract ( xy-nm )
  2over - [ + ] dip ;
```



## REXX

Strictly speaking, REXX only returns one value (or no values), but the value (a string) can comprise of 

multiple "values" or substrings.

If the multiple values are separated by blanks   [or some other unique character(s) such as a comma,

semicolon, backslash, ...],   it's a very simple matter to parse the multiple-value string into the desired

substrings   (or values, if you will)   with REXX's handy-dandy   '''parse'''   statement.

```REXX
/*REXX program shows and displays examples of multiple  RETURN  values  from a function.*/
numeric digits 70                                /*the default is:    NUMERIC DIGITS 9  */
parse arg a b .                                  /*obtain two numbers from command line.*/
if a=='' | a==","  then a= 82                    /*Not specified?  Then use the default.*/
if b=='' | b==","  then b= 20                    /* "      "         "   "   "     "    */
say '     a ='  a                                /*display the first number to the term.*/
say '     b ='  b                                /*   "     "  second   "    "  "    "  */
say copies('═', 50)                              /*display a separator line  "  "    "  */
z= arithmetics(a, b)                             /*call the function:   arithmetics     */
parse var z  abut sum diff rem div Idiv prod pow /*obtain the function's returned values*/
say '    || ='  abut                             /*display   abutment   to the terminal.*/
say '     + ='  sum                              /*   "        sum       "  "     "     */
say '     - ='  diff                             /*   "     difference   "  "     "     */
say '    // ='  rem                              /*   "     remainder    "  "     "     */
say '     / ='  div                              /*   "      quotient    "  "     "     */
say '     % ='  Idiv                             /*   "   int. quotient  "  "     "     */
say '     * ='  prod                             /*   "       product    "  "     "     */
say '    ** ='  pow                              /*   "        power     "  "     "     */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
arithmetics: procedure;  parse arg x,y;  return  x||y  x+y  x-y  x//y  x/y  x%y  x*y  x**y
```

{{out|output|text=  when using the default inputs:}}

```txt

     a = 82
     b = 20
══════════════════════════════════════════════════
    || = 8220
     + = 102
     - = 62
    // = 2
     / = 4.1
     % = 4
     * = 1640
    ** = 188919613181312032574569023867244773376

```



## Ring


```ring

Func AddSub x,y 
     Return [ x+y, x-y ]

```



## Ruby

Every function returns one value. The conventional way to return multiple values is to bundle them into an Array.

Use an array literal:

```ruby
def addsub(x, y)
  [x + y, x - y]
end
```

Or use <code>return</code> with 2 or more values:

```ruby
def addsub(x, y)
  return x + y, x - y
end
```


(With at least 2 values, <code>return</code> makes a new Array. With 1 value, <code>return</code> passes the value, without making any Array. With 0 values, <code>return</code> passes <code>nil</code>.)

Assignment can split the Array into separate variables.

```ruby
sum, difference = addsub(33, 12)
puts "33 + 12 = #{sum}"
puts "33 - 12 = #{difference}"
```



## Run BASIC

Courtesy http://dkokenge.com/rbp

Gets the UTC time from the web

```runbasic
a$ = timeInfo$()
print " UTC:";word$(a$,1,"|")
print "Date:";word$(a$,2,"|")
print "Time:";word$(a$,3,"|")
wait
function timeInfo$()
utc$ = word$(word$(httpget$("http://tycho.usno.navy.mil/cgi-bin/timer.pl"),1,"UTC"),2,"<BR>") ' Universal time
d$   = date$()
t$  = time$()
timeInfo$ = utc$;"|";d$;"|";t$
end function
```



## Rust


Rust supports ADT, thus function can return tuple.


```rust
fn multi_hello() -> (&'static str, i32) {
    ("Hello",42)
}

fn main() {
    let (str,num)=multi_hello();
    println!("{},{}",str,num);
}

```


{{out}}


```txt
Hello,42

```



## Scala

Every function returns one value. The conventional way to return multiple values is to return a tuple.

```scala
def addSubMult(x: Int, y: Int) = (x + y, x - y, x * y)
```

A more detailed declaration would be:

```scala

def addSubMult(x: Int, y:Int) : (Int, Int, Int) = {
  ...
  (x + y, x - y, x * y)
}

```

You can use pattern matching to extract the components:

```scala
val (sum, difference) = addsub(33, 12)
```

Scala borrows this idea from ML, and generalizes it into [http://www.scala-lang.org/node/112 extractors].


## Scheme

Scheme can return multiple values using the <code>values</code> function, which uses continuations:

```scheme
(define (addsub x y)
  (values (+ x y) (- x y)))
```

You can use the multiple values using the <code>call-with-values</code> function:

```scheme
(call-with-values
  (lambda () (addsub 33 12))
  (lambda (sum difference)
    (display "33 + 12 = ") (display sum) (newline)
    (display "33 - 12 = ") (display difference) (newline)))
```

The syntax is kinda awkward. SRFI 8 introduces a <code>receive</code> construct to make this simpler:

```scheme
(receive (sum difference) (addsub 33 12)
  ; in this scope you can use sum and difference
  (display "33 + 12 = ") (display sum) (newline)
  (display "33 - 12 = ") (display difference) (newline))
```

SRFI 11 introduces a <code>let-values</code> construct to make this simpler:

```scheme
(let-values (((sum difference) (addsub 33 12)))
  ; in this scope you can use sum and difference
  (display "33 + 12 = ") (display sum) (newline)
  (display "33 - 12 = ") (display difference) (newline))
```



## Seed7

Seed7 functions can only return one value. That value could be an array or record holding multiple values, but the usual method for returning several values is using a procedure with [http://seed7.sourceforge.net/manual/params.htm#inout_parameter inout] parameters:

```Seed7
$ include "seed7_05.s7i";

const proc: sumAndDiff (in integer: x, in integer: y, inout integer: sum, inout integer: diff) is func
  begin
    sum := x + y;
    diff := x - y;
 end func;

const proc: main is func
  local
    var integer: sum is 0;
    var integer: diff is 0;
  begin
    sumAndDiff(5, 3, sum, diff);
    writeln("Sum: " <& sum);
    writeln("Diff: " <& diff);
  end func;
```


{{out}}

```txt

Sum: 8
Diff: 2

```



## Sidef


```ruby
func foo(a,b) {
    return (a+b, a*b);
}
```


Catching the returned arguments:

```ruby
var (x, y) = foo(4, 5);
say x;   #=> 9
say y;   #=> 20
```



## Smalltalk


Smalltalk returns a single value from methods, so this task is usually implemented the scheme-way, by passing a lambda-closure which is invoked with the values to return and either operates on the values itself or sets them as the caller's locals (i.e. simular to call-with-values ... values):


```smalltalk
foo multipleValuesInto:[:a :b | 
   Transcript show:a; cr.
   Transcript show:b; cr.
]
```


or: 

```smalltalk
|val1 val2|
foo multipleValuesInto:[:a :b | 
   val1 := a.
   val2 := b.
].
... do something with val1 and val2...

```


The called method in foo looks like:

```smalltalk

multipleValuesInto: aTwoArgBlock
   ...
   aTwoArgBlock value:<value1> value:<value2>

```

i.e. it invokes the passed-in lambda closure with the two (return-)values.


## Standard ML

Every function returns one value. The conventional way to return multiple values is to return a tuple.

```sml
fun addsub (x, y) =
  (x + y, x - y)
```

You can use pattern matching to extract the components:

```sml
let
  val (sum, difference) = addsub (33, 12)
in
  print ("33 + 12 = " ^ Int.toString sum ^ "\n");
  print ("33 - 12 = " ^ Int.toString difference ^ "\n")
end
```



## Swift

Every function returns one value. The conventional way to return multiple values is to bundle them into a tuple.

```swift
func addsub(x: Int, y: Int) -> (Int, Int) {
  return (x + y, x - y)
}
```

You can use pattern matching to extract the components:

```swift
let (sum, difference) = addsub(33, 12)
println("33 + 12 = \(sum)")
println("33 - 12 = \(difference)")
```



## Tcl

Tcl commands all return a single value, but this value can be a compound value such as a list or dictionary. The result value of a procedure is either the value given to the <code>return</code> command or the result of the final command in the body in the procedure. (Commands that return “no” value actually return the empty string.)

```tcl
proc addsub {x y} {
    list [expr {$x+$y}] [expr {$x-$y}]
}
```

This can be then assigned to a single variable with <code>set</code> or to multiple variables with <code>lassign</code>.

```tcl
lassign [addsub 33 12] sum difference
puts "33 + 12 = $sum, 33 - 12 = $difference"
```



## TXR

TXR functions return material by binding unbound variables.

The following function potentially returns three values, which will happen if called with three arguments, each of which is an unbound variable:

```txr
@(define func (x y z))
@  (bind w "discarded")
@  (bind (x y z) ("a" "b" "c"))
@(end)
```

The binding <code>w</code>, if created, is discarded because <code>w</code> is not in the list of formal parameters. However, <code>w</code> can cause the function to fail because there can already exist a variable <code>w</code> with a value which doesn't match <code>"discarded"</code>.

Call:

```txr
@(func t r s)
```

If <code>t</code>, <code>r</code> and <code>s</code> are unbound variables, they get bound to <code>"a"</code>, <code>"b"</code> and <code>"c"</code>, respectively via a renaming mechanism. This may look like C++ reference parameters or Pascal "var" parameters, and can be used that way, but isn't really the same at all.

Failed call ("1" doesn't match "a"):

```txr
@(func "1" r s)
```

Successful call binding only one new variable:

```txr
@(func "a" "b" s)
```



## UNIX Shell

Shell scripts don't directly support returning values from a function, 
it can be simulated through some clunky code.


```bash

#!/bin/sh
funct1() {
  a=$1
  b=`expr $a + 1`
  echo $a $b
}

values=`funct1 5`

set $values
x=$1
y=$2
echo "x=$x"
echo "y=$y"

```


{{out}}

```txt

x=5
y=6

```



## Ursa

The most straightforward way to return multiple values from a function in Ursa is to return a stream. 

This example gets a specified amount of strings from the user, then returns a stream containing them.

```ursa
def getstrs (int n)
        decl string<> input

        while (> n 0)
                out ": " console
                append (in string console) input
                dec n
        end while

        return input
end getstrs

decl int amount
out "how many strings do you want to enter? " console
set amount (in int console)

decl string<> ret
set ret (getstrs amount)

out endl ret endl console
```

{{out}}

```txt
how many strings do you want to enter? 5
: these   
: are
: some
: test
: strings

class java.lang.String<these, are, some, test, strings>
```



## VBA

Firt way : User Defined Type

```vb

Type Contact
    Name As String
    firstname As String
    Age As Byte
End Type

Function SetContact(N As String, Fn As String, A As Byte) As Contact
    SetContact.Name = N
    SetContact.firstname = Fn
    SetContact.Age = A
End Function

'For use :
Sub Test_SetContact()
Dim Cont As Contact

    Cont = SetContact("SMITH", "John", 23)
    Debug.Print Cont.Name & " " & Cont.firstname & ", " & Cont.Age & " years old."
End Sub

```

{{out}}

```txt
SMITH John, 23 years old.
```

Second way : ByRef argument : (Note : the ByRef Arg could be an array)

```vb

Function Divide(Dividend As Integer, Divisor As Integer, ByRef Result As Double) As Boolean
    Divide = True
    On Error Resume Next
    Result = Dividend / Divisor
    If Err <> 0 Then
        Divide = False
        On Error GoTo 0
    End If
End Function

'For use :
Sub test_Divide()
Dim R As Double, Ddd As Integer, Dvs As Integer, B As Boolean

    Ddd = 10: Dvs = 3
    B = Divide(Ddd, Dvs, R)
    Debug.Print "Divide return : " & B & " Result = " & R
    Ddd = 10: Dvs = 0
    B = Divide(Ddd, Dvs, R)
    Debug.Print "Divide return : " & B & " Result = " & R
End Sub

```

{{out}}

```txt
Divide return : True Result = 3,33333333333333
Divide return : False Result = 1,#INF
```

Third way : ParramArray

```vb

Function Multiple_Divide(Dividend As Integer, Divisor As Integer, ParamArray numbers() As Variant) As Long
Dim i As Integer

    On Error GoTo ErrorHandler
    numbers(LBound(numbers)) = Dividend / Divisor
    For i = LBound(numbers) + 1 To UBound(numbers)
        numbers(i) = numbers(i - 1) / Divisor
    Next i
    Multiple_Divide = 1: Exit Function
ErrorHandler:
    Multiple_Divide = 0
End Function

'For use :
Sub test_Multiple_Divide()
Dim Arr(3) As Variant, Ddd As Integer, Dvs As Integer, L As Long, i As Integer

    Ddd = 10: Dvs = 3
    L = Multiple_Divide(Ddd, Dvs, Arr(0), Arr(1), Arr(2), Arr(3))
    Debug.Print "The function return : " & L
    Debug.Print "The values in return are : "
    For i = LBound(Arr) To UBound(Arr)
        Debug.Print Arr(i)
    Next i
    Erase Arr
    Debug.Print "--------------------------------------"
    Ddd = 10: Dvs = 0
    L = Multiple_Divide(Ddd, Dvs, Arr(0), Arr(1), Arr(2), Arr(3))
    Debug.Print "The function return : " & L
    Debug.Print "The values in return are : "
    For i = LBound(Arr) To UBound(Arr)
        Debug.Print IIf(Arr(i) = "", "vbNullString", "Null")
    Next i
End Sub

```

{{out}}

```txt
The function return : 1
The values in return are : 
 3,33333333333333 
 1,11111111111111 
 0,37037037037037 
 0,123456790123457 
--------------------------------------
The function return : 0
The values in return are : 
vbNullString
vbNullString
vbNullString
vbNullString
```

Fourth way : the variant() function

```vb

Function List() As String()
Dim i&, Temp(9) As String

    For i = 0 To 9
        Temp(i) = "Liste " & i + 1
    Next
    List = Temp
End Function

'For use :
Sub test_List()
Dim myArr() As String, i As Integer
'Note : you don't need to Dim your array !
    myArr = List()
    For i = LBound(myArr) To UBound(myArr)
        Debug.Print myArr(i)
    Next
End Sub

```

{{out}}

```txt
Liste 1
Liste 2
Liste 3
Liste 4
Liste 5
Liste 6
Liste 7
Liste 8
Liste 9
Liste 10
```



## Visual FoxPro


```vfp

*!* Return multiple values from a function
*!* The simplest way is to pass the parameters by reference
*!* either by SET UDFPARMS TO REFERENCE, or prefix the variables with @.
LOCAL a, b
a = 5
b = 6
? "Sum =", AddUp(@a, @b)    && Displays 11
? "a =", a, "b =", b        && Displays 4, 5
? "Sum =", AddUp(@a, @b)    && Displays 9

FUNCTION AddUp(n1, n2)
LOCAL n 
n = n1 + n2
n1 = n1 - 1
n2 = n2 - 1 
RETURN n
ENDFUNC

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

proc Rect2Polar(X,Y,A,D);       \Return two polar coordinate values
real X,Y,A,D;
[A(0):= ATan2(Y,X);
 D(0):= Sqrt(X*X+Y*Y);
]; \Rect2Polar

real Ang, Dist;
[Rect2Polar(4.0, 3.0, @Ang, @Dist);  \("@" is a new feature similar to 'addr')
RlOut(0, Ang); 
RlOut(0, Dist); 
CrLf(0);
]
```


{{out}} (angle is in radians):

```txt

    0.64350    5.00000

```



## zkl


```zkl
fcn f{return(1,2,"three")}
a,b,c:=f()  // a==1, b==2, c=="three"
```

