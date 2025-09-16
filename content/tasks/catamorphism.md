+++
title = "Catamorphism"
description = ""
date = 2019-08-21T15:22:31Z
aliases = []
[extra]
id = 12312
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "abap",
  "ada",
  "aime",
  "algol_68",
  "apple_script",
  "bracmat",
  "bbc_basic",
  "c",
  "c++",
  "c#",
  "clojure",
  "common_lisp",
  "d",
  "dcl",
  "echolisp",
  "elena",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "logtalk",
  "lolcode",
  "lua",
  "m2000_interpreter",
  "maple",
  "maxima",
  "min",
  "nemerle",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "prolog",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "sidef",
  "standard_ml",
  "swift",
  "tailspin",
  "tcl",
  "vba",
  "wdte",
  "wortel",
  "zkl",
  "zx_spectrum_basic"
]
+++

## Task

''Reduce'' is a function or method that is used to take the values in an array or a list and apply a function to successive members of the list to produce (or reduce them to), a single value.


;Task:
Show how ''reduce'' (or ''foldl'' or ''foldr'' etc), work (or would be implemented) in your language.


;See also:
* Wikipedia article:   [[wp:Fold (higher-order function)|Fold]]
* Wikipedia article:   [[wp:Catamorphism|Catamorphism]]





## 11l


```11l
print((1..3).reduce((x, y) -> x + y))
print((1..3).reduce(3, (x, y) -> x + y))
print([1, 1, 3].reduce((x, y) -> x + y))
print([1, 1, 3].reduce(2, (x, y) -> x + y))
```

{{out}}

```txt

6
9
5
7

```



## ABAP

This works in ABAP version 7.40 and above.


```ABAP

report z_catamorphism.

data(numbers) = value int4_table( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).

write: |numbers = { reduce string(
  init output = `[`
       index = 1
  for number in numbers
  next output = cond string(
         when index eq lines( numbers )
         then |{ output }, { number } ]|
         when index > 1
         then |{ output }, { number }|
         else |{ output } { number }| )
       index = index + 1 ) }|, /.

write: |sum(numbers) = { reduce int4(
  init result = 0
  for number in numbers
  next result = result + number ) }|, /.

write: |product(numbers) = { reduce int4(
  init result = 1
  for number in numbers
  next result = result * number ) }|, /.

data(strings) = value stringtab( ( `reduce` ) ( `in` ) ( `ABAP` ) ).

write: |strings = { reduce string(
  init output = `[`
       index = 1
  for string in strings
  next output = cond string(
         when index eq lines( strings )
         then |{ output }, { string } ]|
         when index > 1
         then |{ output }, { string }|
         else |{ output } { string }| )
       index = index + 1 ) }|, /.

write: |concatenation(strings) = { reduce string(
  init text = ``
  for string in strings
  next text = |{ text } { string }| ) }|, /.

```


{{out}}

```txt

numbers = [ 1, 2, 3, 4, 5 ]

sum(numbers) = 15

product(numbers) = 120

strings = [ reduce, in, ABAP ]

concatenation(strings) =  reduce in ABAP

```



## Ada



```Ada
with Ada.Text_IO;

procedure Catamorphism is

   type Fun is access function (Left, Right: Natural) return Natural;
   type Arr is array(Natural range <>) of Natural;

   function Fold_Left (F: Fun; A: Arr) return Natural is
      Result: Natural := A(A'First);
   begin
      for I in A'First+1 .. A'Last loop
	 Result := F(Result, A(I));
      end loop;
      return Result;
   end Fold_Left;

   function Max (L, R: Natural) return Natural is (if L > R then L else R);
   function Min (L, R: Natural) return Natural is (if L < R then L else R);
   function Add (Left, Right: Natural) return Natural is (Left + Right);
   function Mul (Left, Right: Natural) return Natural is (Left * Right);

   package NIO is new Ada.Text_IO.Integer_IO(Natural);

begin
   NIO.Put(Fold_Left(Min'Access, (1,2,3,4)), Width => 3);
   NIO.Put(Fold_Left(Max'Access, (1,2,3,4)), Width => 3);
   NIO.Put(Fold_Left(Add'Access, (1,2,3,4)), Width => 3);
   NIO.Put(Fold_Left(Mul'Access, (1,2,3,4)), Width => 3);
end Catamorphism;
```


{{out}}


```txt
  1  4 10 24
```



## Aime


```aime
integer s;

s = 0;
list(1, 2, 3, 4, 5, 6, 7, 8, 9).ucall(add_i, 1, s);
o_(s, "\n");
```

{{Out}}

```txt
45
```



## ALGOL 68


```algol68
# applies fn to successive elements of the array of values #
# the result is 0 if there are no values                   #
PROC reduce = ( []INT values, PROC( INT, INT )INT fn )INT:
     IF UPB values < LWB values
     THEN # no elements #
          0
     ELSE # there are some elements #
          INT result := values[ LWB values ];
          FOR pos FROM LWB values + 1 TO UPB values
          DO
              result := fn( result, values[ pos ] )
          OD;
          result
     FI; # reduce #

# test the reduce procedure #
BEGIN print( ( reduce( ( 1, 2, 3, 4, 5 ), ( INT a, b )INT: a + b ), newline ) ) # sum #
    ; print( ( reduce( ( 1, 2, 3, 4, 5 ), ( INT a, b )INT: a * b ), newline ) ) # product #
    ; print( ( reduce( ( 1, 2, 3, 4, 5 ), ( INT a, b )INT: a - b ), newline ) ) # difference #
END
```

{{out}}

```txt

        +15
       +120
        -13

```



## AppleScript

{{Trans|JavaScript}}

Iteratively implemented '''foldl''' and '''foldr''', using the same argument sequence as in the corresponding JavaScript array methods '''reduce()''' and '''reduceRight()'''.

(Note that to obtain first-class functions from user-defined AppleScript handlers, we have to 'lift' them into script objects).


```AppleScript
-- CATAMORPHISMS --------------------------------------------------

-- the arguments available to the called function f(a, x, i, l) are
-- a: current accumulator value
-- x: current item in list
-- i: [ 1-based index in list ] optional
-- l: [ a reference to the list itself ] optional

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

-- the arguments available to the called function f(a, x, i, l) are
-- a: current accumulator value
-- x: current item in list
-- i: [ 1-based index in list ] optional
-- l: [ a reference to the list itself ] optional

-- foldr :: (a -> b -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldr


-- OTHER FUNCTIONS DEFINED IN TERMS OF FOLDL AND FOLDR ------------

-- concat :: [[a]] -> [a] | [String] -> String
on concat(xs)
    script append
        on |λ|(a, b)
            a & b
        end |λ|
    end script

    if length of xs > 0 and class of (item 1 of xs) is string then
        set unit to ""
    else
        set unit to {}
    end if
    foldl(append, unit, xs)
end concat

-- product :: Num a => [a] -> a
on product(xs)
    script
        on |λ|(a, b)
            a * b
        end |λ|
    end script

    foldr(result, 1, xs)
end product

-- sum :: Num a => [a] -> a
on sum(xs)
    script
        on |λ|(a, b)
            a + b
        end |λ|
    end script

    foldl(result, 0, xs)
end sum


-- TEST -----------------------------------------------------------
on run
    set xs to {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

    {sum(xs), product(xs), concat(xs)}

    --> {55, 3628800, "10987654321"}
end run


-- GENERIC FUNCTION -----------------------------------------------

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

{{out}}

```txt
{55, 3628800, "10987654321"}
```



## Bracmat


```bracmat
( ( fold
  =   f xs init first rest
    .   !arg:(?f.?xs.?init)
      & ( !xs:&!init
        |   !xs:%?first ?rest
          & !f$(!first.fold$(!f.!rest.!init))
        )
  )
&   out
  $ ( fold
    $ ( (=a b.!arg:(?a.?b)&!a+!b)
      . 1 2 3 4 5
      . 0
      )
    )
& (product=a b.!arg:(?a.?b)&!a*!b)
& out$(fold$(product.1 2 3 4 5.1))
);
```

Output:

```txt
15
120
```



## BBC BASIC


```bbcbasic

      DIM a(4)
      a() = 1, 2, 3, 4, 5
      PRINT FNreduce(a(), "+")
      PRINT FNreduce(a(), "-")
      PRINT FNreduce(a(), "*")
      END

      DEF FNreduce(arr(), op$)
      REM!Keep tmp, arr()
      LOCAL I%, tmp
      tmp = arr(0)
      FOR I% = 1 TO DIM(arr(), 1)
        tmp = EVAL("tmp " + op$ + " arr(I%)")
      NEXT
      = tmp

```


{{out}}

```txt
        15
       -13
       120
```



## C


```c
#include <stdio.h>

typedef int (*intFn)(int, int);

int reduce(intFn fn, int size, int *elms)
{
    int i, val = *elms;
    for (i = 1; i < size; ++i)
        val = fn(val, elms[i]);
    return val;
}

int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }
int mul(int a, int b) { return a * b; }

int main(void)
{
    int nums[] = {1, 2, 3, 4, 5};
    printf("%d\n", reduce(add, 5, nums));
    printf("%d\n", reduce(sub, 5, nums));
    printf("%d\n", reduce(mul, 5, nums));
    return 0;
}
```


{{out}}

```txt
15
-13
120
```



## C++


```cpp
#include <iostream>
#include <numeric>
#include <functional>
#include <vector>

int main() {
	std::vector<int> nums = { 1, 2, 3, 4, 5 };
	auto nums_added = std::accumulate(std::begin(nums), std::end(nums), 0, std::plus<int>());
	auto nums_other = std::accumulate(std::begin(nums), std::end(nums), 0, [](const int& a, const int& b) {
		return a + 2 * b;
	});
	std::cout << "nums_added: " << nums_added << std::endl;
	std::cout << "nums_other: " << nums_other << std::endl;
}
```


{{out}}


```txt
nums_added: 15
nums_other: 30
```



## C#


```c#
var nums = Enumerable.Range(1, 10);

int summation = nums.Aggregate((a, b) => a + b);

int product = nums.Aggregate((a, b) => a * b);

string concatenation = nums.Aggregate(String.Empty, (a, b) => a.ToString() + b.ToString());

Console.WriteLine("{0} {1} {2}", summation, product, concatenation);
```



## Clojure

For more detail, check Rich Hickey's [http://clojure.com/blog/2012/05/08/reducers-a-library-and-model-for-collection-processing.html blog post on Reducers].


```clojure
; Basic usage
> (reduce * '(1 2 3 4 5))
120
; Using an initial value
> (reduce + 100 '(1 2 3 4 5))
115

```



## Common Lisp


```lisp
; Basic usage
> (reduce #'* '(1 2 3 4 5))
120
; Using an initial value
> (reduce #'+ '(1 2 3 4 5) :initial-value 100)
115
; Using only a subsequence
> (reduce #'+ '(1 2 3 4 5) :start 1 :end 4)
9
; Apply a function to each element first
> (reduce #'+ '((a 1) (b 2) (c 3)) :key #'cadr)
6
; Right-associative reduction
> (reduce #'expt '(2 3 4) :from-end T)
2417851639229258349412352
; Compare with
> (reduce #'expt '(2 3 4))
4096
```



## D


```d
void main() {
    import std.stdio, std.algorithm, std.range, std.meta, std.numeric,
           std.conv, std.typecons;

    auto list = iota(1, 11);
    alias ops = AliasSeq!(q{a + b}, q{a * b}, min, max, gcd);

    foreach (op; ops)
        writeln(op.stringof, ": ", list.reduce!op);

    // std.algorithm.reduce supports multiple functions in parallel:
    reduce!(ops[0], ops[3], text)(tuple(0, 0.0, ""), list).writeln;
}
```

{{out}}

```txt
"a + b": 55
"a * b": 3628800
min(T1,T2,T...) if (is(typeof(a < b))): 1
max(T1,T2,T...) if (is(typeof(a < b))): 10
gcd(T): 1
Tuple!(int,double,string)(55, 10, "12345678910")
```



## DCL


```DCL
$ list = "1,2,3,4,5"
$ call reduce list "+"
$ show symbol result
$
$ numbers = "5,4,3,2,1"
$ call reduce numbers "-"
$ show symbol result
$
$ call reduce list "*"
$ show symbol result
$ exit
$
$ reduce: subroutine
$ local_list = 'p1
$ value = f$integer( f$element( 0, ",", local_list ))
$ i = 1
$ loop:
$  element = f$element( i, ",", local_list )
$  if element .eqs. "," then $ goto done
$  value = value 'p2 f$integer( element )
$  i = i + 1
$  goto loop
$ done:
$ result == value
$ exit
$ endsubroutine
```

{{out}}

```txt
$ @catamorphism
  RESULT == 15   Hex = 0000000F  Octal = 00000000017
  RESULT == -5   Hex = FFFFFFFB  Octal = 37777777773
  RESULT == 120   Hex = 00000078  Octal = 00000000170
```


=={{header|Déjà Vu}}==
This is a foldl:

```dejavu
reduce f lst init:
	if lst:
		f reduce @f lst init pop-from lst
	else:
		init

!. reduce @+ [ 1 10 200 ] 4
!. reduce @- [ 1 10 200 ] 4

```

{{out}}

```txt
215
-207
```



## EchoLisp


```scheme

;; rem : the foldX family always need an initial value
;; fold left a list
(foldl + 0 (iota 10)) ;; 0 + 1 + .. + 9
  → 45

;; fold left a sequence
(lib 'sequences)
(foldl * 1 [ 1 .. 10])
    → 362880 ;; 10!

;; folding left and right
(foldl / 1 ' ( 1 2 3 4))
    → 8/3
(foldr / 1 '(1 2 3 4))
    → 3/8

;;scanl gives the list (or sequence) of intermediate values :
(scanl * 1 '( 1 2 3 4 5))
   → (1 1 2 6 24 120)

```


## Elena

ELENA 4.x :

```elena
import system'collections;
import system'routines;
import extensions;
import extensions'text;

public program()
{
    var numbers := new Range(1,10).summarize(new ArrayList());

    var summary := numbers.accumulate(new Variable(0), (a,b => a + b));

    var product := numbers.accumulate(new Variable(1), (a,b => a * b));

    var concatenation := numbers.accumulate(new StringWriter(), (a,b => a.Printable + b.Printable));

    console.printLine(summary," ",product," ",concatenation)
}
```

{{out}}

```txt

55 362880 12345678910

```



## Elixir


```elixir
iex(1)> Enum.reduce(1..10, fn i,acc -> i+acc end)
55
iex(2)> Enum.reduce(1..10, fn i,acc -> i*acc end)
3628800
iex(3)> Enum.reduce(10..-10, "", fn i,acc -> acc <> to_string(i) end)
"109876543210-1-2-3-4-5-6-7-8-9-10"
```



## Erlang

{{trans|Haskell}}


```erlang

-module(catamorphism).

-export([test/0]).

test() ->
	Nums = lists:seq(1,10),
	Summation =
		lists:foldl(fun(X, Acc) -> X + Acc end, 0, Nums),
	Product =
		lists:foldl(fun(X, Acc) -> X * Acc end, 1, Nums),
	Concatenation =
		lists:foldr(
			fun(X, Acc) -> integer_to_list(X) ++ Acc end,
			"",
			Nums),
	{Summation, Product, Concatenation}.

```


Output:

```txt

{55,3628800,"12345678910"}

```


=={{header|F_Sharp|F#}}==
<p>In the REPL:</p>

```txt

> let nums = [1 .. 10];;

val nums : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

> let summation = List.fold (+) 0 nums;;

val summation : int = 55

> let product = List.fold (*) 1 nums;;

val product : int = 3628800

> let concatenation = List.foldBack (fun x y -> x + y) (List.map (fun i -> i.ToString()) nums) "";;

val concatenation : string = "12345678910"

```



## Factor



```factor
{ 1 2 4 6 10 } 0 [ + ] reduce .
```

{{out}}

```txt

23

```



## Forth

Forth has three traditions for iterating over the members of a data
structure.  Under the first, the data structure has words that help
you navigate over it and normal Forth looping structures are used.
Under the second, the data structure has dedicated looping words and
you supply the code that's run for each member.  Under the third,
the data structure has a loop-over-members word that accepts a
function to be run against each member.

There's no need to distinguish between the different kinds of
looping ("this one collects function returns into a list; this one
threads an accumulator between the function-calls; this one threads
two accumulators through the function-calls; this one expects no
return values whatsoever from the function-calls") because in Forth
all that the looping words have to do is make the data stack
available for the function's use.  When that's the case, all of
these variations, that are so important in other languages, are
functionally equivalent.

Although it's possible to have a generic higher-order word that can
operate under all kinds of data structures -- this just requires
that one settle on an object system and then derive a collections
library from it -- this is rarely done.  Typically each data
structure has its own looping words.

To demonstrate the above points we'll just loop over the bytes of a
string.

Some helper words for these examples:


```forth
: lowercase? ( c -- f )
  [char] a [ char z 1+ ] literal within ;

: char-upcase ( c -- C )
  dup lowercase? if bl xor then ;
```


Using normal looping words:


```forth
: string-at ( c-addr u +n -- c )
  nip + c@ ;
: string-at! ( c-addr u +n c -- )
  rot drop  -rot  + c! ;

: type-lowercase ( c-addr u -- )
  dup 0 ?do
    2dup i string-at  dup lowercase?  if emit else drop then
  loop  2drop ;

: upcase ( 'string' -- 'STRING' )
  dup 0 ?do
    2dup 2dup  i string-at  char-upcase  i swap string-at!
  loop ;

: count-lowercase ( c-addr u -- n )
  0 -rot dup 0 ?do
    2dup i string-at  lowercase? if rot 1+ -rot then
  loop  2drop ;
```


Briefly, a variation:


```forth
: next-char ( a +n -- a' n' c -1 )  ( a 0 -- 0 )
  dup if 2dup  1 /string  2swap drop c@ true
  else 2drop 0 then ;

: type-lowercase ( c-addr u -- )
  begin next-char while
    dup lowercase? if emit else drop then
  repeat ;
```


Using dedicated looping words:


```forth
: each-char[ ( c-addr u -- )
  postpone BOUNDS postpone ?DO
  postpone I postpone C@ ;  immediate

  \ interim code: ( c -- )

: ]each-char ( -- )
  postpone LOOP ;  immediate

: type-lowercase ( c-addr u -- )
  each-char[ dup lowercase? if emit else drop then ]each-char ;

: upcase ( 'string' -- 'STRING' )
  2dup each-char[ char-upcase i c! ]each-char ;

: count-lowercase ( c-addr u -- n )
  0 -rot each-char[ lowercase? if 1+ then ]each-char ;
```


Using higher-order words:


```forth
: each-char ( c-addr u xt -- )
  {: xt :}  bounds ?do
    i c@ xt execute
  loop ;

: type-lowercase ( c-addr u -- )
  [: dup lowercase? if emit else drop then ;]
  each-char ;

\ producing a new string
: upcase ( 'string' -- 'STRING' )
  dup cell+ allocate throw -rot
  [: ( new-string-addr c -- new-string-addr )
    upcase over c+! ;] each-char  $@ ;

: count-lowercase ( c-addr u -- n )
  0 -rot [: lowercase? if 1+ then ;] each-char ;
```


In these examples COUNT-LOWERCASE updates an accumulator, UPCASE
(mostly) modifies the string in-place, and TYPE-LOWERCASE performs
side-effects and returns nothing to the higher-order word.


## Fortran

If Fortran were to offer the ability to pass a parameter "by name", as is used in [[Jensen's_Device#Fortran|Jensen's device]], then the code might be something like
```Fortran
      SUBROUTINE FOLD(t,F,i,ist,lst)
       INTEGER t
       BYNAME F
        DO i = ist,lst
          t = F
        END DO
      END SUBROUTINE FOLD      !Result in temp.

      temp = a(1); CALL FOLD(temp,temp*a(i),i,2,N)
```

Here, the function manifests as the expression that is the second parameter of subroutine FOLD, and the "by name" protocol for parameter F means that within the subroutine whenever there is a reference to F, its value is evaluated afresh in the caller's environment using the current values of ''temp'' and ''i'' as modified by the subroutine - they being passed by reference so that changes within the subroutine affect the originals. An evaluation for a different function requires merely another statement with a different expression.

Fortran however does not provide such a facility. Any parameter that is an expression is evaluated once in the caller's environment, the result placed in temporary storage, and the address of that storage location is passed to the subroutine. Repeated references to that parameter will elicit the same value. But there is special provision for passing a function to a routine, involving the special word EXTERNAL. For every different function in mind, one must diligently supply a name, and work through the overhead of declaring each such function. There is an additional word, INTRINSIC, for use when an intrinsic function (such as SIN) is to be passed as such a parameter since it will appear as its name only, and with the absence of the (...) that would be used for the function's parameters when in an arithmetic expression, it would otherwise be taken as being the name of an ordinary variable.

Here is such an arrangement, in the style of F77 though somewhat affected by F90 in that the END statement names the routine being ended. Similarly, to abate petty complaints about the types of the functions being undeclared, explicit types are specified, though unselecting the compiler diagnostic for that would match the habits of earlier compilers. Also in F90 is the MODULE protocol which involves rather more organised checking of types and additional facilities for arrays [[Array_length#Fortran|so that N need not be passed]] because secret additional parameters do so.

However, only programmer diligence in devising functions with the correct type of result and the correct type and number of parameters will evade mishaps. Note that the EXTERNAL statement does not specify the number or type of parameters. If the function is invoked multiple times within a subroutine, the compiler may check for consistency. This may cause trouble when [[Leonardo_numbers#Fortran|some parameters are optional]] so that different invocations do not match.

The function's name is used as a working variable within the function (as well as it holding the function's value on exit) so that the expression <code>F(IFOLD,A(I))</code> is ''not'' a recursive invocation of function <code>IFOLD</code> because there are no (parameters) appended to the function's name. Earlier compilers did not allow such usage so that a separate working variable would be required.
```Fortran
      INTEGER FUNCTION IFOLD(F,A,N)	!"Catamorphism"...
       INTEGER F	!We're working only with integers.
       EXTERNAL F	!This is a function, not an array.
       INTEGER A(*)	!An 1-D array, of unspecified size.
       INTEGER N	!The number of elements.
       INTEGER I	!A stepper.
        IFOLD = 0		!A default value.
        IF (N.LE.0) RETURN	!Dodge silly invocations.
        IFOLD = A(1)		!The function is to have two arguments.
        IF (N.EQ.1) RETURN	!So, if there is only one element, silly.
        DO I = 2,N		!Otherwise, stutter along the array.
          IFOLD = F(IFOLD,A(I))		!Applying the function.
        END DO			!On to the next element.
      END FUNCTION IFOLD!Thus, F(A(1),A(2)), or F(F(A(1),A(2)),A(3)), or F(F(F(A(1),A(2)),A(3)),A(4)), etc.

      INTEGER FUNCTION IADD(I,J)
       INTEGER I,J
        IADD = I + J
      END FUNCTION IADD

      INTEGER FUNCTION IMUL(I,J)
       INTEGER I,J
        IMUL = I*J
      END FUNCTION IMUL

      INTEGER FUNCTION IDIV(I,J)
       INTEGER I,J
        IDIV = I/J
      END FUNCTION IDIV

      INTEGER FUNCTION IVID(I,J)
       INTEGER I,J
        IVID = J/I
      END FUNCTION IVID

      PROGRAM POKE
      INTEGER ENUFF
      PARAMETER (ENUFF = 6)
      INTEGER A(ENUFF)
      PARAMETER (A = (/1,2,3,4,5,6/))
      INTEGER MSG
      EXTERNAL IADD,IMUL,IDIV,IVID	!Warn that these are the names of functions.

      MSG = 6	!Standard output.
      WRITE (MSG,1) ENUFF,A
    1 FORMAT ('To apply a function in the "catamorphic" style ',
     1 "to the ",I0," values ",/,(20I3))

      WRITE (MSG,*) "Iadd",IFOLD(IADD,A,ENUFF)
      WRITE (MSG,*) "Imul",IFOLD(IMUL,A,ENUFF)
      WRITE (MSG,*) "Idiv",IFOLD(IDIV,A,ENUFF)
      WRITE (MSG,*) "Ivid",IFOLD(IVID,A,ENUFF)
      END PROGRAM POKE

```

Output:

```txt

To apply a function in the "catamorphic" style to the 6 values
  1  2  3  4  5  6
 Iadd          21
 Imul         720
 Idiv           0
 Ivid           6

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type IntFunc As Function(As Integer, As Integer) As Integer

Function reduce(a() As Integer, f As IntFunc) As Integer
   '' if array is empty or function pointer is null, return 0 say
   If UBound(a) = -1 OrElse f = 0 Then Return 0
   Dim result As Integer = a(LBound(a))
   For i As Integer = LBound(a) + 1 To UBound(a)
     result = f(result, a(i))
   Next
   Return result
End Function

Function add(x As Integer, y As Integer) As Integer
  Return x + y
End Function

Function subtract(x As Integer, y As Integer) As Integer
  Return x - y
End Function

Function multiply(x As Integer, y As Integer) As Integer
  Return x * y
End Function

Function max(x As Integer, y As Integer) As Integer
  Return IIf(x > y, x, y)
End Function

Function min(x As Integer, y As Integer) As Integer
  Return IIf(x < y, x, y)
End Function

Dim a(4) As Integer = {1, 2, 3, 4, 5}
Print "Sum is        :"; reduce(a(), @add)
Print "Difference is :"; reduce(a(), @subtract)
Print "Product is    :"; reduce(a(), @multiply)
Print "Maximum is    :"; reduce(a(), @max)
Print "Minimum is    :"; reduce(a(), @min)
Print "No op is      :"; reduce(a(), 0)
Print
Print "Press any key to quit"
Sleep

```


{{out}}

```txt

Sum is        : 15
Difference is :-13
Product is    : 120
Maximum is    : 5
Minimum is    : 1
No op is      : 0

```



## Go


```go
package main

import (
	"fmt"
)

func main() {
	n := []int{1, 2, 3, 4, 5}

	fmt.Println(reduce(add, n))
	fmt.Println(reduce(sub, n))
	fmt.Println(reduce(mul, n))
}

func add(a int, b int) int { return a + b }
func sub(a int, b int) int { return a - b }
func mul(a int, b int) int { return a * b }

func reduce(rf func(int, int) int, m []int) int {
	r := m[0]
	for _, v := range m[1:] {
		r = rf(r, v)
	}
	return r
}
```

{{out}}

```txt

15
-13
120

```



## Groovy

Groovy provides an "inject" method for all aggregate classes that performs a classic tail-recursive reduction, driven by a closure argument. The result of each iteration (closure invocation) is used as the accumulated valued for the next iteration. If a first argument is provided as well as a second closure argument, that first argument is used as a seed accumulator for the first iteration. Otherwise, the first element of the aggregate is used as the seed accumulator, with reduction iteration proceeding across elements 2 through n.

```groovy
def vector1 = [1,2,3,4,5,6,7]
def vector2 = [7,6,5,4,3,2,1]
def map1 = [a:1, b:2, c:3, d:4]

println vector1.inject { acc, val -> acc + val }       // sum
println vector1.inject { acc, val -> acc + val*val }   // sum of squares
println vector1.inject { acc, val -> acc * val }       // product
println vector1.inject { acc, val -> acc<val?val:acc } // max
println ([vector1,vector2].transpose().inject(0) { acc, val -> acc + val[0]*val[1] }) //dot product (with seed 0)

println (map1.inject { Map.Entry accEntry, Map.Entry entry ->     // some sort of weird map-based reduction
    [(accEntry.key + entry.key):accEntry.value + entry.value ].entrySet().toList().pop()
})
```


{{out}}

```txt
28
140
5040
7
84
abcd=10
```



## Haskell


```haskell
main :: IO ()
main =
  putStrLn . unlines $
  [ show . foldr (+)    0  -- sum
  , show . foldr (*)    1  -- product
  , foldr ((++) . show) "" -- concatenation
  ] <*>
  [[1 .. 10]]
```

{{Out}}

```txt
55
3628800
12345678910
```


and the generality of folds is such that if we replace all three of these (function, identity) combinations ((+), 0), ((*), 1) ((++), "") with the Monoid operation '''mappend''' (<>) and identity '''mempty''', we can still obtain the same results:


```haskell
import Data.Monoid

main :: IO ()
main =
  let xs = [1 .. 10]
  in (putStrLn . unlines)
       [ (show . getSum     . foldr (<>) mempty) (Sum     <$> xs)
       , (show . getProduct . foldr (<>) mempty) (Product <$> xs)
       , (show .              foldr (<>) mempty) (show    <$> xs)
       , (show .              foldr (<>) mempty) (words
                     "Love is one damned thing after each other")
       ]
```

{{Out}}

```txt
55
3628800
"12345678910"
"Loveisonedamnedthingaftereachother"
```


Also available are ''foldl1'' and ''foldr1'' which implicitly take first element as starting value. However they are not safe as they fail on empty lists.

''Prelude'' folds work only on lists, module ''Data.Foldable'' a typeclass for more general fold - interface remains the same.

=={{header|Icon}} and {{header|Unicon}}==

Works in both languages:

```unicon
procedure main(A)
    write(A[1],": ",curry(A[1],A[2:0]))
end

procedure curry(f,A)
    r := A[1]
    every r := f(r, !A[2:0])
    return r
end
```


Sample runs:

```txt

->cata + 3 1 4 1 5 9
+: 23
->cata - 3 1 4 1 5 9
-: -17
->cata \* 3 1 4 1 5 9
*: 540
->cata "||" 3 1 4 1 5 9
||: 314159

```



## J

'''Solution''':
```j
    /
```

'''Example''':
```j
   +/ 1 2 3 4 5
15
   */ 1 2 3 4 5
120
   !/ 1 2 3 4 5  NB.  "n ! k" is "n choose k"
45
```

Insert * into 1 2 3 4 5
becomes
1 * 2 * 3 * 4 * 5
evaluated right to left
```j

1 * 2 * 3 * 20
1 * 2 * 60
1 * 120
120

```

What are the implications for -/  ?
For %/  ?


## Java

{{works with|Java|8}}

```java
import java.util.stream.Stream;

public class ReduceTask {

    public static void main(String[] args) {
        System.out.println(Stream.of(1, 2, 3, 4, 5).mapToInt(i -> i).sum());
        System.out.println(Stream.of(1, 2, 3, 4, 5).reduce(1, (a, b) -> a * b));
    }
}
```


{{out}}

```txt
15
120
```



## JavaScript



### ES5



```javascript
var nums = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

function add(a, b) {
    return a + b;
}

var summation = nums.reduce(add);

function mul(a, b) {
    return a * b;
}

var product = nums.reduce(mul, 1);

var concatenation = nums.reduce(add, "");

console.log(summation, product, concatenation);
```



Note that the JavaScript Array methods include a right fold ( '''.reduceRight()''' ) as well as a left fold:


```JavaScript
(function (xs) {
    'use strict';

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    function foldl(f, acc, xs) {
        return xs.reduce(f, acc);
    }

    // foldr :: (b -> a -> b) -> b -> [a] -> b
    function foldr(f, acc, xs) {
        return xs.reduceRight(f, acc);
    }

    // Test folds in both directions
    return [foldl, foldr].map(function (f) {
        return f(function (acc, x) {
            return acc + (x * 2).toString() + ' ';
        }, [], xs);
    });

})([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
```


{{Out}}


```txt
["0 2 4 6 8 10 12 14 16 18 ",
"18 16 14 12 10 8 6 4 2 0 "]
```



### ES6



```javascript
var nums = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

console.log(nums.reduce((a, b) => a + b, 0)); // sum of 1..10
console.log(nums.reduce((a, b) => a * b, 1)); // product of 1..10
console.log(nums.reduce((a, b) => a + b, '')); // concatenation of 1..10
```



## jq

jq has an unusual and unusually powerful "reduce" control structure. A full description is beyond the scope of this short article, but an important point is that "reduce" is stream-oriented.  Reduction of arrays is however trivially achieved using the ".[]" filter for converting an array to a stream of its values.

The simplest use of "reduce" can be illustrated by this definition of "factorial":

 def factorial: reduce range(2;.+1) as $i (1; . * $i);

If the input is a non-negative integer, n, this will compute n!.

To understand how this works, consider "3|factorial". The computation starts by setting the implicit state variable to 1; range(2;4) will generate the sequence of values (2,3). The variable $i is set to each value in the stream in turn so that the state variable is multiplied by 2 (". * $i") and then by 3.  Notice that since range/2 produces a stream, no array is ever constructed.

For a more complex illustration, see [[Sorting_algorithms/Strand_sort#jq|Strand sort]].

The "reduce" operator is typically used within a map/reduce framework, but the implicit state variable can be any JSON entity, and so "reduce" is also a general-purpose iterative control structure, the only limitation being that it does not have the equivalent of "break".  For that, the "foreach" control structure in recent versions of jq can be used.


## Julia

{{Works with|Julia 1.2}}

```Julia
println([reduce(op, 1:5) for op in [+, -, *]])
println([foldl(op, 1:5) for op in [+, -, *]])
println([foldr(op, 1:5) for op in [+, -, *]])
```

{{out}}

```txt
[15, -13, 120]
[15, -13, 120]
[15, 3, 120]
```



## Kotlin


```scala
fun main(args: Array<String>) {
    val a = intArrayOf(1, 2, 3, 4, 5)
    println("Array       : ${a.joinToString(", ")}")
    println("Sum         : ${a.reduce { x, y -> x + y }}")
    println("Difference  : ${a.reduce { x, y -> x - y }}")
    println("Product     : ${a.reduce { x, y -> x * y }}")
    println("Minimum     : ${a.reduce { x, y -> if (x < y) x else y }}")
    println("Maximum     : ${a.reduce { x, y -> if (x > y) x else y }}")
}
```


{{out}}

```txt

Array       : 1, 2, 3, 4, 5
Sum         : 15
Difference  : -13
Product     : 120
Minimum     : 1
Maximum     : 5

```



## Logtalk

The Logtalk standard library provides implementations of common meta-predicates such as fold left. The example that follow uses Logtalk's native support for lambda expressions to avoid the need for auxiliary predicates.

```logtalk

:- object(folding_examples).

    :- public(show/0).
    show :-
        integer::sequence(1, 10, List),
        write('List: '), write(List), nl,
        meta::fold_left([Acc,N,Sum0]>>(Sum0 is Acc+N), 0, List, Sum),
        write('Sum of all elements: '), write(Sum), nl,
        meta::fold_left([Acc,N,Product0]>>(Product0 is Acc*N), 1, List, Product),
        write('Product of all elements: '), write(Product), nl,
        meta::fold_left([Acc,N,Concat0]>>(number_codes(N,NC), atom_codes(NA,NC), atom_concat(Acc,NA,Concat0)), '', List, Concat),
        write('Concatenation of all elements: '), write(Concat), nl.

:- end_object.

```

{{out}}

```txt

| ?- folding_examples::show.
List: [1,2,3,4,5,6,7,8,9,10]
Sum of all elements: 55
Product of all elements: 3628800
Concatenation of all elements: 12345678910
yes

```



## LOLCODE


{{trans|C}}


```LOLCODE
HAI 1.3

HOW IZ I reducin YR array AN YR size AN YR fn
    I HAS A val ITZ array'Z SRS 0
    IM IN YR loop UPPIN YR i TIL BOTH SAEM i AN DIFF OF size AN 1
        val R I IZ fn YR val AN YR array'Z SRS SUM OF i AN 1 MKAY
    IM OUTTA YR loop
    FOUND YR val
IF U SAY SO

O HAI IM array
    I HAS A SRS 0 ITZ 1
    I HAS A SRS 1 ITZ 2
    I HAS A SRS 2 ITZ 3
    I HAS A SRS 3 ITZ 4
    I HAS A SRS 4 ITZ 5
KTHX

HOW IZ I add YR a AN YR b, FOUND YR     SUM OF a AN b, IF U SAY SO
HOW IZ I sub YR a AN YR b, FOUND YR    DIFF OF a AN b, IF U SAY SO
HOW IZ I mul YR a AN YR b, FOUND YR PRODUKT OF a AN b, IF U SAY SO

VISIBLE I IZ reducin YR array AN YR 5 AN YR add MKAY
VISIBLE I IZ reducin YR array AN YR 5 AN YR sub MKAY
VISIBLE I IZ reducin YR array AN YR 5 AN YR mul MKAY

KTHXBYE
```


{{out}}

```txt
15
-13
120
```



## Lua


```Lua

table.unpack = table.unpack or unpack -- 5.1 compatibility
local nums = {1,2,3,4,5,6,7,8,9}

function add(a,b)
   return a+b
end

function mult(a,b)
   return a*b
end

function cat(a,b)
   return tostring(a)..tostring(b)
end

local function reduce(fun,a,b,...)
   if ... then
      return reduce(fun,fun(a,b),...)
   else
      return fun(a,b)
   end
end

local arithmetic_sum = function (...) return reduce(add,...) end
local factorial5 = reduce(mult,5,4,3,2,1)

print("Σ(1..9)   : ",arithmetic_sum(table.unpack(nums)))
print("5!        : ",factorial5)
print("cat {1..9}: ",reduce(cat,table.unpack(nums)))


```


{{out}}

```txt

Σ(1..9)   : 	45
5!        : 	120
cat {1..9}: 	123456789

```




## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      Function Reduce (a, f) {
            if len(a)=0 then Error "Nothing to reduce"
            if len(a)=1 then  =Array(a) : Exit
            k=each(a, 2, -1)
            m=Array(a)
            While k {
                  m=f(m, array(k))
            }
            =m
      }
      a=(1, 2, 3, 4, 5)
      Print "Array", a
      Print "Sum", Reduce(a, lambda (x,y)->x+y)
      Print "Difference", Reduce(a, lambda (x,y)->x-y)
      Print "Product", Reduce(a, lambda (x,y)->x*y)
      Print "Minimum", Reduce(a, lambda (x,y)->if(x<y->x, y))
      Print "Maximum", Reduce(a, lambda (x,y)->if(x>y->x, y))
}
CheckIt

```

{{out}}

```txt

Array               1         2         3         4          5
Sum                15
Difference        -13
Product           120
Minimum             1
Maximum             5

```



## Maple

The left fold operator in Maple is foldl, and foldr is the right fold operator.

```maple

 nums := seq( 1 .. 10 );
                          nums := 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

> foldl( `+`, 0, nums ); # compute sum using foldl
                          55

> foldr( `*`, 1, nums ); # compute product using foldr
                          3628800
```

Compute the horner form of a (sorted) polynomial:

```maple

 foldl( (a,b) ->a*T+b, op(map2(op,1,[op( 72*T^5+37*T^4-23*T^3+87*T^2+44*T+29 )])));
                    ((((72 T + 37) T - 23) T + 87) T + 44) T + 29
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
Fold[f, x, {a, b, c, d}]
```

{{Out}}

```txt
f[f[f[f[x, a], b], c], d]
```



## Maxima


```maxima
lreduce(f, [a, b, c, d], x0);
/* (%o1)                     f(f(f(f(x0, a), b), c), d) */
```



```maxima
lreduce("+", [1, 2, 3, 4], 100);
/* (%o1)                                 110 */
```



## min

{{works with|min|0.19.3}}

```min
(1 2 3 4) 0 '+ reduce puts! ; sum
(1 2 3 4) 1 '* reduce puts! ; product
```

{{out}}

```txt

10
24

```



## Nemerle

The <tt>Nemerle.Collections</tt> namespace defines <tt>FoldLeft</tt>, <tt>FoldRight</tt> and <tt>Fold</tt> (an alias for <tt>FoldLeft</tt>) on any sequence that implements the <tt>IEnumerable[T]</tt> interface.

```Nemerle
def seq = [1, 4, 6, 3, 7];
def sum = seq.Fold(0, _ + _); // Fold takes an initial value and a function, here the + operator
```



## Nim


```nim
import sequtils

block:
  let
    numbers = @[5, 9, 11]
    addition = foldl(numbers, a + b)
    substraction = foldl(numbers, a - b)
    multiplication = foldl(numbers, a * b)
    words = @["nim", "is", "cool"]
    concatenation = foldl(words, a & b)

block:
  let
    numbers = @[5, 9, 11]
    addition = foldr(numbers, a + b)
    substraction = foldr(numbers, a - b)
    multiplication = foldr(numbers, a * b)
    words = @["nim", "is", "cool"]
    concatenation = foldr(words, a & b)
```


=={{header|Oberon-2}}==
{{Works with| oo2c Version 2}}

```oberon2

MODULE Catamorphism;
IMPORT
  Object,
  NPCT:Tools,
  NPCT:Args,
  IntStr,
  Out;

TYPE
  BinaryFunc= PROCEDURE (x,y: LONGINT): LONGINT;

VAR
  data: POINTER TO ARRAY OF LONGINT;
  i: LONGINT;

  PROCEDURE Sum(x,y: LONGINT): LONGINT;
  BEGIN
    RETURN x + y
  END Sum;

  PROCEDURE Sub(x,y: LONGINT): LONGINT;
  BEGIN
    RETURN x - y;
  END Sub;

  PROCEDURE Mul(x,y: LONGINT): LONGINT;
  BEGIN
    RETURN x * y;
  END Mul;

  PROCEDURE Reduce(x: ARRAY OF LONGINT; f: BinaryFunc): LONGINT;
  VAR
    i,res: LONGINT;
  BEGIN
    res := x[0];i := 1;
    WHILE (i < LEN(x)) DO;
      res := f(res,x[i]);
      INC(i)
    END;
    RETURN res
  END Reduce;

  PROCEDURE InitData(VAR x: ARRAY OF LONGINT);
  VAR
    i, j: LONGINT;
    res: IntStr.ConvResults;
    aux: Object.CharsLatin1;
  BEGIN
    i := 0;j := 1;
    WHILE (j <= LEN(x)) DO
      aux := Tools.AsString(Args.Get(j));
      IntStr.StrToInt(aux^,x[i],res);
      IF res # IntStr.strAllRight THEN
        Out.String("Incorrect format for data at index ");Out.LongInt(j,0);Out.Ln;
        HALT(1);
      END;
      INC(j);INC(i)
    END
  END InitData;

BEGIN
  IF Args.Number() = 1 THEN
    Out.String("Invalid number of arguments. ");Out.Ln;
    HALT(0)
  ELSE
    NEW(data,Args.Number() - 1);
    InitData(data^);
    Out.LongInt(Reduce(data^,Sum),0);Out.Ln;
    Out.LongInt(Reduce(data^,Sub),0);Out.Ln;
    Out.LongInt(Reduce(data^,Mul),0);Out.Ln
  END
END Catamorphism.

```

{{out}}

```txt

1
-11
-14400

```



## Objeck


```objeck

use Collection;

class Reducer {
  function : Main(args : String[]) ~ Nil {
    values := IntVector->New([1, 2, 3, 4, 5]);
    values->Reduce(Add(Int, Int) ~ Int)->PrintLine();
    values->Reduce(Mul(Int, Int) ~ Int)->PrintLine();
  }

  function : Add(a : Int, b : Int) ~ Int {
    return a + b;
  }

  function : Mul(a : Int, b : Int) ~ Int {
    return a * b;
  }
}
```

Output

```txt

15
120

```



## OCaml


```ocaml
# let nums = [1;2;3;4;5;6;7;8;9;10];;
val nums : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
# let sum = List.fold_left (+) 0 nums;;
val sum : int = 55
# let product = List.fold_left ( * ) 1 nums;;
val product : int = 3628800
```



## Oforth

reduce is already defined into Collection class :


```Oforth
[ 1, 2, 3, 4, 5 ] reduce(#max)
[ "abc", "def", "gfi" ] reduce(#+)
```



## PARI/GP


```parigp
reduce(f, v)={
  my(t=v[1]);
  for(i=2,#v,t=f(t,v[i]));
  t
};
reduce((a,b)->a+b, [1,2,3,4,5,6,7,8,9,10])
```


{{works with|PARI/GP|2.8.1+}}

```parigp
fold((a,b)->a+b, [1..10])
```


## Pascal

{{works with|Free Pascal}}
Should work with many pascal dialects

```pascal
program reduce;

type
//  tmyArray = array of LongInt;
  tmyArray = array[-5..5] of LongInt;
  tmyFunc = function (a,b:LongInt):LongInt;

function add(x,y:LongInt):LongInt;
begin
  add := x+y;
end;

function sub(k,l:LongInt):LongInt;
begin
  sub := k-l;
end;

function mul(r,t:LongInt):LongInt;
begin
  mul := r*t;
end;

function reduce(myFunc:tmyFunc;a:tmyArray):LongInt;
var
  i,res : LongInt;
begin
  res := a[low(a)];
  For i := low(a)+1 to high(a) do
    res := myFunc(res,a[i]);
  reduce := res;
end;

procedure InitMyArray(var a:tmyArray);
var
  i: LongInt;
begin
  For i := low(a) to high(a) do
  begin
    //no a[i] = 0
    a[i] := i + ord(i=0);
    write(a[i],',');
  end;
  writeln(#8#32);
end;

var
  ma : tmyArray;
BEGIN
  InitMyArray(ma);
  writeln(reduce(@add,ma));
  writeln(reduce(@sub,ma));
  writeln(reduce(@mul,ma));
END.
```

output

```txt
-5,-4,-3,-2,-1,1,1,2,3,4,5
1
-11
-1440
```



## Perl

Perl's reduce function is in a standard package.

```perl
use List::Util 'reduce';

# note the use of the odd $a and $b globals
print +(reduce {$a + $b} 1 .. 10), "\n";

# first argument is really an anon function; you could also do this:
sub func { $b & 1 ? "$a $b" : "$b $a" }
print +(reduce \&func, 1 .. 10), "\n"
```



## Perl 6

{{works with|Rakudo|2018.03}}
Any associative infix operator, either built-in or user-defined, may be turned into a reduce operator by putting it into square brackets (known as "the reduce metaoperator") and using it as a list operator.  The operations will work left-to-right or right-to-left automatically depending on the natural associativity of the base operator.

```perl6
my @list = 1..10;
say [+] @list;
say [*] @list;
say [~] @list;
say min @list;
say max @list;
say [lcm] @list;
```

{{out}}

```txt
55
3628800
12345678910
1
10
2520
```

In addition to the reduce metaoperator, a general higher-order function, <tt>reduce</tt>, can apply any appropriate function.  Reproducing the above in this form, using the function names of those operators, we have:

```perl6
my @list = 1..10;
say reduce &infix:<+>, @list;
say reduce &infix:<*>, @list;
say reduce &infix:<~>, @list;
say reduce &infix:<min>, @list;
say reduce &infix:<max>, @list;
say reduce &infix:<lcm>, @list;
```



## Phix

{{trans|C}}

```Phix
function add(integer a, integer b)
    return a + b
end function

function sub(integer a, integer b)
    return a - b
end function

function mul(integer a, integer b)
    return a * b
end function

function reduce(integer rid, sequence s)
object res = s[1]
    for i=2 to length(s) do
        res = call_func(rid,{res,s[i]})
    end for
    return res
end function

?reduce(routine_id("add"),tagset(5))
?reduce(routine_id("sub"),tagset(5))
?reduce(routine_id("mul"),tagset(5))
```

{{out}}

```txt

15
-13
120

```



## PicoLisp


```PicoLisp
(de reduce ("Fun" "Lst")
   (let "A" (car "Lst")
      (for "N" (cdr "Lst")
         (setq "A" ("Fun" "A" "N")) )
      "A" ) )

(println
   (reduce + (1 2 3 4 5))
   (reduce * (1 2 3 4 5)) )

(bye)
```



## PowerShell

'Filter' is a more common sequence function in PowerShell than 'reduce' or 'map', but here is one way to accomplish 'reduce':

```PowerShell

1..5 | ForEach-Object -Begin {$result = 0} -Process {$result += $_} -End {$result}

```

{{Out}}

```txt

15

```



## Prolog

SWI-Prolog has native foldl in version 6.3.1

Module lambda was written by '''Ulrich Neumerkel''' and can be found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl

```Prolog
:- use_module(library(lambda)).

% foldl is now a predicate of SWI-Prolog 6.3.1
%
catamorphism :-
	numlist(1,10,L),
	foldl(\XS^YS^ZS^(ZS is XS+YS), L, 0, Sum),
	format('Sum of ~w is ~w~n', [L, Sum]),
	foldl(\XP^YP^ZP^(ZP is XP*YP), L, 1, Prod),
	format('Prod of ~w is ~w~n', [L, Prod]),
	string_to_list(LV, ""),
	foldl(\XC^YC^ZC^(string_to_atom(XS, XC),string_concat(YC,XS,ZC)),
	      L, LV, Concat),
	format('Concat of ~w is ~w~n', [L, Concat]).
```

{{out}}

```txt
 ?- catamorphism.
Sum of [1,2,3,4,5,6,7,8,9,10] is 55
Prod of [1,2,3,4,5,6,7,8,9,10] is 3628800
Concat of [1,2,3,4,5,6,7,8,9,10] is 12345678910
true.

```



## Python


```python
>>
 # Python 2.X
>>> from operator import add
>>> listoflists = [['the', 'cat'], ['sat', 'on'], ['the', 'mat']]
>>> help(reduce)
Help on built-in function reduce in module __builtin__:

reduce(...)
    reduce(function, sequence[, initial]) -> value

    Apply a function of two arguments cumulatively to the items of a sequence,
    from left to right, so as to reduce the sequence to a single value.
    For example, reduce(lambda x, y: x+y, [1, 2, 3, 4, 5]) calculates
    ((((1+2)+3)+4)+5).  If initial is present, it is placed before the items
    of the sequence in the calculation, and serves as a default when the
    sequence is empty.

>>> reduce(add, listoflists, [])
['the', 'cat', 'sat', 'on', 'the', 'mat']
>>>
```


### Additional example


```python
# Python 3.X

from functools import reduce
from operator import add, mul

nums = range(1,11)

summation = reduce(add, nums)

product = reduce(mul, nums)

concatenation = reduce(lambda a, b: str(a) + str(b), nums)

print(summation, product, concatenation)
```



## R


Sum the numbers in a vector:


```R

Reduce('+', c(2,30,400,5000))
5432

```


Put a 0 between each pair of numbers:


```R

Reduce(function(a,b){c(a,0,b)},  c(2,3,4,5))
2 0 3 0 4 0 5

```


Generate all prefixes of a string:


```R

Reduce(paste0, unlist(strsplit("freedom", NULL)), accum=T)
"f"       "fr"      "fre"     "free"    "freed"   "freedo"  "freedom"

```


Filter and map:


```R

Reduce(function(x,acc){if (0==x%%3) c(x*x,acc) else acc}, 0:22,
       init=c(), right=T)
   0   9  36  81 144 225 324 441

```



## Racket


```racket

#lang racket
(define (fold f xs init)
  (if (empty? xs)
      init
      (f (first xs)
         (fold f (rest xs) init))))

(fold + '(1 2 3) 0)   ; the result is 6

```



## REXX

This REXX example is modeled after the Perl 6 example   (it is NOT a translation).

Also, a   '''list'''   and   '''show'''   function were added, although they
aren't a catamorphism, as they don't produce or reduce the values to a   ''single''   value, but
are included here to help display the values in the list.

```rexx
/*REXX program demonstrates a  method  for  catamorphism  for some simple functions.    */
@list= 1 2 3 4 5 6 7 8 9 10
                                say 'list:'     fold(@list,  "list")
                                say ' sum:'     fold(@list,  "+"   )
                                say 'prod:'     fold(@list,  "*"   )
                                say ' cat:'     fold(@list,  "||"  )
                                say ' min:'     fold(@list,  "min" )
                                say ' max:'     fold(@list,  "max" )
                                say ' avg:'     fold(@list,  "avg" )
                                say ' GCD:'     fold(@list,  "GCD" )
                                say ' LCM:'     fold(@list,  "LCM" )
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fold: procedure;  parse arg z;  arg ,f;         z = space(z);      BIFs= 'MIN MAX LCM GCD'
      za= translate(z, f, ' ');                 zf= f"("translate(z, ',' , " ")')'
      if f== '+' | f=="*"       then interpret  "return"  za
      if f== '||'               then return  space(z, 0)
      if f== 'AVG'              then interpret  "return"  fold(z, '+')    "/"    words(z)
      if wordpos(f, BIFs)\==0   then interpret  "return"  zf
      if f=='LIST' | f=="SHOW"  then return z
      return 'illegal function:'     arg(2)
/*──────────────────────────────────────────────────────────────────────────────────────*/
GCD:  procedure;  $=;                          do j=1  for arg();    $= $ arg(j)
                                               end   /*j*/
      parse var $ x z .;    if x=0  then x= z                  /* [↑] build an arg list.*/
      x= abs(x)
                         do k=2  to words($);  y= abs( word($, k));   if y=0  then iterate
                           do until _=0;       _= x // y;      x= y;     y= _
                           end   /*until*/
                         end   /*k*/
      return x
/*──────────────────────────────────────────────────────────────────────────────────────*/
LCM:  procedure;  $=;    do j=1  for arg();     $= $ arg(j)
                         end   /*j*/
      x= abs(word($, 1))                                       /* [↑] build an arg list.*/
                         do k=2  to words($);   != abs(word($, k));  if !=0  then return 0
                         x= x*!  /  GCD(x, !)                  /*GCD does the heavy work*/
                         end   /*k*/
      return x
```

{{out|output|:}}

```txt

list: 1 2 3 4 5 6 7 8 9 10
 sum: 55
prod: 3628800
 cat: 12345678910
 min: 1
 max: 10
 avg: 5.5
 GCD: 1
 LCM: 2520

```



## Ring


```ring

n = list(10)
for i = 1 to 10
    n[i] = i
next

see "  +: " + cat(10,"+") + nl+
    "  -: " + cat(10,"-") + nl +
    "  *: " + cat(10,"*") + nl +
    "  /: " + cat(10,"/") + nl+
    "  ^: " + cat(10,"^") + nl +
    "min: " + cat(10,"min") + nl+
    "max: " + cat(10,"max") + nl+
    "avg: " + cat(10,"avg") + nl +
    "cat: " + cat(10,"cat") + nl

func cat count,op
     cat = n[1]
     cat2 = ""
     for i = 2 to count
         switch op
                on "+" cat +=  n[i]
                on "-"  cat -=  n[i]
                on "*" cat *=  n[i]
                on "/" cat /=  n[i]
                on "^" cat ^=  n[i]
                on "max" cat = max(cat,n[i])
                on "min" cat = min(cat,n[i])
                on "avg" cat +=  n[i]
                on "cat" cat2 += string(n[i])
          off
     next
if op = "avg"  cat = cat / count ok
if op = "cat"  decimals(0) cat = string(n[1])+cat2 ok
return cat

```



## Ruby

The method inject (and it's alias reduce) can be used in several ways; the simplest is to give a methodname as argument:

```ruby
# sum:
p (1..10).inject(:+)
# smallest number divisible by all numbers from 1 to 20:
p (1..20).inject(:lcm) #lcm: lowest common multiple

```
The most versatile way uses a accumulator object (memo) and a block. In this example Pascal's triangle is generated by using an array [1,1] and inserting the sum of each consecutive pair of numbers from the previous row.

```ruby
p row = [1]
10.times{p row = row.each_cons(2).inject([1,1]){|ar,(a,b)| ar.insert(-2, a+b)} }

# [1]
# [1, 1]
# [1, 2, 1]
# [1, 3, 3, 1]
# [1, 4, 6, 4, 1]
# [1, 5, 10, 10, 5, 1]
# [1, 6, 15, 20, 15, 6, 1]
# etc

```



## Run BASIC


```runbasic
for i = 1 to 10 :n(i) = i:next i

print "  +: ";" ";cat(10,"+")
print "  -: ";" ";cat(10,"-")
print "  *: ";" ";cat(10,"*")
print "  /: ";" ";cat(10,"/")
print "  ^: ";" ";cat(10,"^")
print "min: ";" ";cat(10,"min")
print "max: ";" ";cat(10,"max")
print "avg: ";" ";cat(10,"avg")
print "cat: ";" ";cat(10,"cat")

function cat(count,op$)
cat = n(1)
for i = 2 to count
 if op$ = "+" 	then cat = cat + n(i)
 if op$ = "-" 	then cat = cat - n(i)
 if op$ = "*" 	then cat = cat * n(i)
 if op$ = "/" 	then cat = cat / n(i)
 if op$ = "^" 	then cat = cat ^ n(i)
 if op$ = "max"	then cat = max(cat,n(i))
 if op$ = "min"	then cat = min(cat,n(i))
 if op$ = "avg"	then cat = cat + n(i)
 if op$ = "cat"	then cat$ = cat$ + str$(n(i))
next i
if op$ = "avg" then cat = cat / count
if op$ = "cat" then cat = val(str$(n(1))+cat$)
end function
```


```txt
  +:  55
  -:  -53
  *:  3628800
  /:  2.75573205e-7
  ^:  1
min:  1
max:  10
avg:  5.5
cat:  12345678910
```



## Rust



```rust
fn main() {
    println!("Sum: {}", (1..10).fold(0, |acc, n| acc + n));
    println!("Product: {}", (1..10).fold(1, |acc, n| acc * n));
    let chars = ['a', 'b', 'c', 'd', 'e'];
    println!("Concatenation: {}",
             chars.iter().map(|&c| (c as u8 + 1) as char).collect::<String>());
}
```


{{out}}

```txt

Sum: 45
Product: 362880
Concatenation: bcdef

```



## Scala


```scala
object Main extends App {
  val a = Seq(1, 2, 3, 4, 5)
  println(s"Array       : ${a.mkString(", ")}")
  println(s"Sum         : ${a.sum}")
  println(s"Difference  : ${a.reduce { (x, y) => x - y }}")
  println(s"Product     : ${a.product}")
  println(s"Minimum     : ${a.min}")
  println(s"Maximum     : ${a.max}")
}
```


## Scheme


### Implementation

reduce implemented for a single list:

```scheme
(define (reduce fn init lst)
  (do ((val init (fn (car rem) val)) ; accumulated value passed as second argument
       (rem lst (cdr rem)))
    ((null? rem) val)))

(display (reduce + 0 '(1 2 3 4 5))) (newline) ; => 15
(display (reduce expt 2 '(3 4))) (newline)    ; => 262144
```


### Using SRFI 1

There is also an implementation of fold and fold-right in SRFI-1, for lists.

These take a two-argument procedure: (lambda (value acc) ...) where value is the next value in the list, and acc is the accumulated value.  The initial value is used for the first value of acc.


```txt

> (import (srfi 1))
> (fold + 0 '(1 2 3 4 5))
15
> (fold expt 2 '(3 4)) ; => (expt 4 (expt 3 2))
262144
> (fold-right expt 2 '(3 4)) ; => (expt 3 (expt 4 2))
43046721

```


More than one list may be folded over, when the function is passed one item from each list plus the accumulated value:


```txt

> (fold + 0 '(1 2 3) '(4 5 6)) ; add up all the numbers in all the lists
21

```



## Sidef


```ruby
say (1..10 -> reduce('+'));
say (1..10 -> reduce{|a,b| a + b});
```



## Standard ML


```sml
- val nums = [1,2,3,4,5,6,7,8,9,10];
val nums = [1,2,3,4,5,6,7,8,9,10] : int list
- val sum = foldl op+ 0 nums;
val sum = 55 : int
- val product = foldl op* 1 nums;
val product = 3628800 : int
```



## Swift


```swift
let nums = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

print(nums.reduce(0, +))
print(nums.reduce(1, *))
print(nums.reduce("", { $0 + String($1) }))
```


{{out}}

```txt
55
3628800
12345678910
```



## Tailspin

It is probably easier to just write the whole thing as an inline transform rather than create a utility.

```tailspin

[1..5] -> (@: $(1); $(2..-1)... -> @: $@ + $; $@!) -> '$;
' -> !OUT::write
[1..5] -> (@: $(1); $(2..-1)... -> @: $@ - $; $@!) -> '$;
' -> !OUT::write
[1..5] -> (@: $(1); $(2..-1)... -> @: $@ * $; $@!) -> '$;
' -> !OUT::write

```

{{out}}

```txt

15
-13
120

```


If you really want to make a utility, it could look like this:

```tailspin

templates fold@{op:}
  @: $(1);
  $(2..-1)... -> @: [$@, $] -> op;
  $@ !
end fold

templates add
  $(1) + $(2) !
end add

templates mul
  $(1) * $(2) !
end mul

[1..5] -> fold@{op:add} -> '$;
' -> !OUT::write

[1..5] -> fold@{op:mul} -> '$;
' -> !OUT::write

```

{{out}}

```txt

15
120

```



## Tcl

Tcl does not come with a built-in <tt>fold</tt> command, but it is easy to construct:

```tcl
proc fold {lambda zero list} {
    set accumulator $zero
    foreach item $list {
	set accumulator [apply $lambda $accumulator $item]
    }
    return $accumulator
}
```

Demonstrating:

```tcl
set 1to5 {1 2 3 4 5}

puts [fold {{a b} {expr {$a+$b}}} 0 $1to5]
puts [fold {{a b} {expr {$a*$b}}} 1 $1to5]
puts [fold {{a b} {return $a,$b}} x $1to5]
```

{{out}}

```txt

15
120
x,1,2,3,4,5

```

Note that these particular operations would more conventionally be written as:

```tcl
puts [::tcl::mathop::+ {*}$1to5]
puts [::tcl::mathop::* {*}$1to5]
puts x,[join $1to5 ,]
```

But those are not general catamorphisms.


## VBA


```vb
Public Sub reduce()
    s = [{1,2,3,4,5}]
    Debug.Print WorksheetFunction.Sum(s)
    Debug.Print WorksheetFunction.Product(s)
End Sub
```


## WDTE

Translated from the JavaScript ES6 example with a few modifications.


```WDTE
let a =
 import 'arrays';
let s => import 'stream';
let str => import 'strings';

# Sum of [1, 10]:
let nums => [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
a.stream nums -> s.reduce 0 + -- io.writeln io.stdout;

# As an alternative to an array, a range stream can be used. Here's the product of [1, 11):
s.range 1 11 -> s.reduce 1 * -- io.writeln io.stdout;

# And here's a concatenation:
s.range 1 11 -> s.reduce '' (str.format '{}{}') -- io.writeln io.stdout;
```



## Wortel

You can reduce an array with the <code>!/</code> operator.

```wortel
!/ ^+ [1 2 3] ; returns 6
```

If you want to reduce with an initial value, you'll need the <code>@fold</code> operator.

```wortel
@fold ^+ 1 [1 2 3] ; returns 7
```


{{out}}

```txt
55
3628800
12345678910
```



## zkl

Most sequence objects in zkl have a reduce method.

```zkl
T("foo","bar").reduce(fcn(p,n){p+n}) //--> "foobar"
"123four5".reduce(fcn(p,c){p+(c.matches("[0-9]") and c or 0)}, 0) //-->11
File("foo.zkl").reduce('+(1).fpM("0-"),0) //->5 (lines in file)
```



## ZX Spectrum Basic

{{trans|BBC_BASIC}}

```zxbasic
10 DIM a(5)
20 FOR i=1 TO 5
30 READ a(i)
40 NEXT i
50 DATA 1,2,3,4,5
60 LET o$="+": GO SUB 1000: PRINT tmp
70 LET o$="-": GO SUB 1000: PRINT tmp
80 LET o$="*": GO SUB 1000: PRINT tmp
90 STOP
1000 REM Reduce
1010 LET tmp=a(1)
1020 FOR i=2 TO 5
1030 LET tmp=VAL ("tmp"+o$+"a(i)")
1040 NEXT i
1050 RETURN
```
