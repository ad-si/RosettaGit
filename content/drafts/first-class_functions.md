+++
title = "First-class functions"
description = ""
date = 2019-10-18T10:26:26Z
aliases = []
[extra]
id = 3849
[taxonomies]
categories = []
tags = []
+++

{{task|Programming language concepts}}

A language has [[wp:First-class function|first-class functions]] if it can do each of the following without recursively invoking a compiler or interpreter or otherwise [[metaprogramming]]:

* Create new functions from preexisting functions at run-time
* Store functions in collections
* Use functions as arguments to other functions
* Use functions as return values of other functions




;Task:
Write a program to create an ordered collection ''A'' of functions of a real number. At least one function should be built-in and at least one should be user-defined; try using the sine, cosine, and cubing functions. Fill another collection ''B'' with the inverse of each function in ''A''. Implement function composition as in [[Functional Composition]]. Finally, demonstrate that the result of applying the composition of each function in ''A'' and its inverse in ''B'' to a value, is the original value. <small>(Within the limits of computational accuracy)</small>.

(A solution need not actually call the collections "A" and "B". These names are only used in the preceding paragraph for clarity.)
 

;Related task: 
[[First-class Numbers]]





## ActionScript


{{trans|JavaScript}}

```ActionScript

var cube:Function = function(x) {
  return Math.pow(x, 3);
};
var cuberoot:Function = function(x) {
  return Math.pow(x, 1/3);
};
  
function compose(f:Function, g:Function):Function {
	return function(x:Number) {return f(g(x));};
}
var functions:Array = [Math.cos, Math.tan, cube];
var inverse:Array = [Math.acos, Math.atan, cuberoot];
 
function test() {
	for (var i:uint = 0; i < functions.length; i++) {
        // Applying the composition to 0.5  
	trace(compose(functions[i], inverse[i])(0.5));
	}
}

test();
```

Output:

```txt

0.5000000000000001
0.5000000000000001
0.5000000000000001

```



## Ada


Even if the example below solves the task, there are some limitations to how dynamically you can create, store and use functions in Ada, so it is debatable if Ada really has first class functions.


```Ada
with Ada.Float_Text_IO,
     Ada.Integer_Text_IO,
     Ada.Text_IO,
     Ada.Numerics.Elementary_Functions;

procedure First_Class_Functions is
   use Ada.Float_Text_IO,
       Ada.Integer_Text_IO,
       Ada.Text_IO,
       Ada.Numerics.Elementary_Functions;

   function Sqr (X : Float) return Float is
   begin
      return X ** 2;
   end Sqr;

   type A_Function is access function (X : Float) return Float;

   generic
      F, G : A_Function;
   function Compose (X : Float) return Float;

   function Compose (X : Float) return Float is
   begin
      return F (G (X));
   end Compose;

   Functions : array (Positive range <>) of A_Function := (Sin'Access,
                                                           Cos'Access,
                                                           Sqr'Access);
   Inverses  : array (Positive range <>) of A_Function := (Arcsin'Access,
                                                           Arccos'Access,
                                                           Sqrt'Access);
begin
   for I in Functions'Range loop
      declare
         function Identity is new Compose (Functions (I), Inverses (I));
         Test_Value : Float := 0.5;
         Result     : Float;
      begin
         Result := Identity (Test_Value);

         if Result = Test_Value then
            Put      ("Example ");
            Put      (I, Width => 0);
            Put_Line (" is perfect for the given test value.");
         else
            Put      ("Example ");
            Put      (I, Width => 0);
            Put      (" is off by");
            Put      (abs (Result - Test_Value));
            Put_Line (" for the given test value.");
         end if;
      end;
   end loop;
end First_Class_Functions;
```


It is bad style (but an explicit requirement in the task description) to put the functions and their inverses in separate arrays rather than keeping each pair in a record and then having an array of that record type.


## Aikido

{{incomplete|Aikido|Fails to demonstrate that the result of applying the composition of each function in A and its inverse in B to a value, is the original value}}
{{trans|Javascript}}

```aikido

import math

function compose (f, g) {
    return function (x) { return f(g(x)) }
}

var fn  = [Math.sin, Math.cos, function(x) { return x*x*x }]
var inv = [Math.asin, Math.acos, function(x) { return Math.pow(x, 1.0/3) }]

for (var i=0; i<3; i++) {
    var f = compose(inv[i], fn[i])
    println(f(0.5))    // 0.5
}


```



## ALGOL 68

{{trans|Python}}

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 using non-standard compose}}

Note: Returning <code>PROC (REAL x)REAL: f1(f2(x))</code> from a function apparently
violates standard '''ALGOL 68''''s scoping rules.  [[ALGOL 68G]] warns about this during 
parsing, and then - if run out of scope - rejects during runtime.

```algol68
MODE F = PROC (REAL)REAL;
OP ** = (REAL x, power)REAL: exp(ln(x)*power);

# Add a user defined function and its inverse #
PROC cube = (REAL x)REAL: x * x * x;
PROC cube root = (REAL x)REAL: x ** (1/3);

# First class functions allow run-time creation of functions from functions #
# return function compose(f,g)(x) == f(g(x)) #
PROC non standard compose = (F f1, f2)F: (REAL x)REAL: f1(f2(x)); # eg ELLA ALGOL 68RS #
PROC compose = (F f, g)F: ((F f2, g2, REAL x)REAL: f2(g2(x)))(f, g, );

# Or the classic "o" functional operator #
PRIO O = 5;
OP (F,F)F O = compose;

# first class functions should be able to be members of collection types #
[]F func list = (sin, cos, cube);
[]F arc func list = (arc sin, arc cos, cube root);

# Apply functions from lists as easily as integers #
FOR index TO UPB func list DO
  STRUCT(F f, inverse f) this := (func list[index], arc func list[index]);
  print(((inverse f OF this O f OF this)(.5), new line))
OD
```

Output:

```txt
+.500000000000000e +0
+.500000000000000e +0
+.500000000000000e +0
```



## AppleScript

AppleScript does not have built-in functions like sine or cosine.


```applescript
-- Compose two functions, where each function is
-- a script object with a call(x) handler.
on compose(f, g)
    script
        on call(x)
            f's call(g's call(x))
        end call
    end script
end compose

script increment
    on call(n)
        n + 1
    end call
end script

script decrement
    on call(n)
        n - 1
    end call
end script

script twice
    on call(x)
        x * 2
    end call
end script

script half
    on call(x)
        x / 2
    end call
end script

script cube
    on call(x)
        x ^ 3
    end call
end script

script cuberoot
    on call(x)
        x ^ (1 / 3)
    end call
end script

set functions to {increment, twice, cube}
set inverses to {decrement, half, cuberoot}
set answers to {}
repeat with i from 1 to 3
    set end of answers to ¬
        compose(item i of inverses, ¬
            item i of functions)'s ¬
        call(0.5)
end repeat
answers -- Result: {0.5, 0.5, 0.5}
```


Putting math libraries aside for the moment (we can always shell out to bash functions like '''bc'''), a deeper issue is that the architectural position of functions in the AppleScript type system is simply a little too incoherent and second class to facilitate really frictionless work with first-class functions. (This is clearly not what AppleScript was originally designed for).

Incoherent, in the sense that built-in functions and operators do not have the same place in the type system as user functions. The former are described as 'commands' in parser errors, and have to be wrapped in user handlers if they are to be used interchangeably with other functions.

Second class, in the sense that user functions (or 'handlers' in the terminology of Apple's documentation), are properties of scripts. The scripts are autonomous first class objects, but the handlers are not. Functions which accept other functions as arguments will internally need to use an '''mReturn''' or '''mInject''' function which 'lifts' handlers into script object types. Functions which return functions will similarly have to return them embedded in such script objects.

Once we have a function like mReturn, however, we can readily write higher order functions like '''map''', '''zipWith''' and '''mCompose''' below.


```AppleScript
on run
    
    set fs to {sin_, cos_, cube_}
    set afs to {asin_, acos_, croot_}
    
    -- Form a list of three composed function objects, 
    -- and map testWithHalf() across the list to produce the results of
    -- application of each composed function (base function composed with inverse) to 0.5
    
    script testWithHalf
        on |λ|(f)
            mReturn(f)'s |λ|(0.5)
        end |λ|
    end script
    
    map(testWithHalf, zipWith(mCompose, fs, afs))
    
    --> {0.5, 0.5, 0.5}
end run

-- Simple composition of two unadorned handlers into
-- a method of a script object
on mCompose(f, g)
    script
        on |λ|(x)
            mReturn(f)'s |λ|(mReturn(g)'s |λ|(x))
        end |λ|
    end script
end mCompose

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

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

on sin:r
    (do shell script "echo 's(" & r & ")' | bc -l") as real
end sin:

on cos:r
    (do shell script "echo 'c(" & r & ")' | bc -l") as real
end cos:

on cube:x
    x ^ 3
end cube:

on croot:x
    x ^ (1 / 3)
end croot:

on asin:r
    (do shell script "echo 'a(" & r & "/sqrt(1-" & r & "^2))' | bc -l") as real
end asin:

on acos:r
    (do shell script "echo 'a(sqrt(1-" & r & "^2)/" & r & ")' | bc -l") as real
end acos:
```

{{Out}}

```AppleScript
{0.5, 0.5, 0.5}
```



## AutoHotkey

By '''just me'''. [http://ahkscript.org/boards/viewtopic.php?f=17&t=1363&p=16454#p16454 Forum Post]

```AutoHotkey
#NoEnv
; Set the floating-point precision
SetFormat, Float, 0.15
; Super-global variables for function objects
Global F, G
; User-defined functions
Cube(X) {
   Return X ** 3
}
CubeRoot(X) {
   Return X ** (1/3)
}
; Function arrays, Sin/ASin and Cos/ACos are built-in
FuncArray1 := [Func("Sin"),  Func("Cos"),  Func("Cube")]
FuncArray2 := [Func("ASin"), Func("ACos"), Func("CubeRoot")]
; Compose
Compose(FN1, FN2) {
   Static FG := Func("ComposedFunction")
   F := FN1, G:= FN2
   Return FG
}
ComposedFunction(X) {
   Return F.(G.(X))
}
; Run
X := 0.5 + 0
Result := "Input:`n" . X . "`n`nOutput:"
For Index In FuncArray1
   Result .= "`n" . Compose(FuncArray1[Index], FuncArray2[Index]).(X)
MsgBox, 0, First-Class Functions, % Result
ExitApp
```


{{Output}}

```txt
Input:
0.500000000000000

Output:
0.500000000000000
0.500000000000000
0.500000000000001
```



## Axiom

Using the interpreter:

```Axiom
fns := [sin$Float, cos$Float, (x:Float):Float +-> x^3]
inv := [asin$Float, acos$Float, (x:Float):Float +-> x^(1/3)]
[(f*g) 0.5 for f in fns for g in inv]
```

Using the Spad compiler:

```Axiom
)abbrev package TESTP TestPackage
TestPackage(T:SetCategory) : with
    _*: (List((T->T)),List((T->T))) -> (T -> List T)
  == add
    import MappingPackage3(T,T,T)
    fs * gs == 
      ((x:T):(List T) +-> [(f*g) x for f in fs for g in gs])
```

This would be called using:

```Axiom
(fns * inv) 0.5
```

Output:

```Axiom
[0.5,0.5,0.5]
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
Strictly speaking you cannot return a ''function'', but you can return a ''function pointer'' which allows the task to be implemented.

```bbcbasic
      REM Create some functions and their inverses:
      DEF FNsin(a) = SIN(a)
      DEF FNasn(a) = ASN(a)
      DEF FNcos(a) = COS(a)
      DEF FNacs(a) = ACS(a)
      DEF FNcube(a) = a^3
      DEF FNroot(a) = a^(1/3)
      
      dummy = FNsin(1)
      
      REM Create the collections (here structures are used):
      DIM cA{Sin%, Cos%, Cube%}
      DIM cB{Asn%, Acs%, Root%}
      cA.Sin% = ^FNsin() : cA.Cos% = ^FNcos() : cA.Cube% = ^FNcube()
      cB.Asn% = ^FNasn() : cB.Acs% = ^FNacs() : cB.Root% = ^FNroot()
      
      REM Create some function compositions:
      AsnSin% = FNcompose(cB.Asn%, cA.Sin%)
      AcsCos% = FNcompose(cB.Acs%, cA.Cos%)
      RootCube% = FNcompose(cB.Root%, cA.Cube%)
      
      REM Test applying the compositions:
      x = 1.234567 : PRINT x, FN(AsnSin%)(x)
      x = 2.345678 : PRINT x, FN(AcsCos%)(x)
      x = 3.456789 : PRINT x, FN(RootCube%)(x)
      END
      
      DEF FNcompose(f%,g%)
      LOCAL f$, p%
      f$ = "(x)=" + CHR$&A4 + "(&" + STR$~f% + ")(" + \
      \             CHR$&A4 + "(&" + STR$~g% + ")(x))"
      DIM p% LEN(f$) + 4
      $(p%+4) = f$ : !p% = p%+4
      = p%
```

'''Output:'''

```txt

  1.234567  1.234567
  2.345678  2.345678
  3.456789  3.456789

```



## Bori


```bori
double acos (double d)	{ return Math.acos(d); }
double asin (double d)	{ return Math.asin(d); }
double cos (double d)	{ return Math.cos(d); }
double sin (double d)	{ return Math.sin(d); }
double croot (double d)	{ return Math.pow(d, 1/3); }
double cube (double x)	{ return x * x * x; }

Var compose (Var f, Var g, double x)
{
	Func ff = f;
	Func fg = g;
	return ff(fg(x));
}

void button1_onClick (Widget widget)
{
	Array arr1 = [ sin, cos, cube ];
	Array arr2 = [ asin, acos, croot ];
	
	str s;
	for (int i = 1; i <= 3; i++)
	{
		s << compose(arr1.get(i), arr2.get(i), 0.5) << str.newline;	
	}
	label1.setText(s);
}
```

Output on Android phone:

```bori
0.5
0.4999999999999999
0.5000000000000001
```



## Bracmat

Bracmat has no built-in functions of real values. To say the truth, Bracmat has no real values. The only pair of currently defined built-in functions for which inverse functions exist are <code>d2x</code> and <code>x2d</code> for decimal to hexadecimal conversion and vice versa. These functions also happen to be each other's inverse. Because these two functions only take non-negative integer arguments, the example uses the argument <code>3210</code> for each pair of functions.

The lists <code>A</code> and <code>B</code> contain a mix of function names and function definitions, which illustrates that they always can take each other's role, except when a function definition is assigned to a function name, as for example in the first and second lines.

The <code>compose</code> function uses macro substitution.
   

```bracmat
( (sqrt=.!arg^1/2)
& (log=.e\L!arg)
& (A=x2d (=.!arg^2) log (=.!arg*pi))
& ( B
  = d2x sqrt (=.e^!arg) (=.!arg*pi^-1)
  )
& ( compose
  =   f g
    .   !arg:(?f.?g)
      & '(.($f)$(($g)$!arg))
  )
&   whl
  ' ( !A:%?F ?A
    & !B:%?G ?B
    & out$((compose$(!F.!G))$3210)
    )
)
```

Output:

```txt
3210
3210
3210
3210
```



## C

Since one can't create new functions dynamically within a C program, C doesn't have first class functions. But you can pass references to functions as parameters and return values and you can have a list of function references, so I guess you can say C has second class functions.

Here goes.


```c>#include <stdlib.h

#include <stdio.h>
#include <math.h>
 
/* declare a typedef for a function pointer */
typedef double (*Class2Func)(double);
 
/*A couple of functions with the above prototype */
double functionA( double v)
{
   return v*v*v;
}
double functionB(double v)
{
   return exp(log(v)/3);
}
 
/* A function taking a function as an argument */
double Function1( Class2Func f2, double val )
{
    return f2(val);
}
 
/*A function returning a function */
Class2Func WhichFunc( int idx)
{
   return (idx < 4) ? &functionA : &functionB;
}
 
/* A list of functions */
Class2Func funcListA[] = {&functionA, &sin, &cos, &tan };
Class2Func funcListB[] = {&functionB, &asin, &acos, &atan };
 
/* Composing Functions */
double InvokeComposed( Class2Func f1, Class2Func f2, double val )
{
   return f1(f2(val));
}
 
typedef struct sComposition {
   Class2Func f1;
   Class2Func f2;
} *Composition;
 
Composition Compose( Class2Func f1, Class2Func f2)
{
   Composition comp = malloc(sizeof(struct sComposition));
   comp->f1 = f1;
   comp->f2 = f2;
   return comp;
}
 
double CallComposed( Composition comp, double val )
{
    return comp->f1( comp->f2(val) );
}
/** * * * * * * * * * * * * * * * * * * * * * * * * * * */
 
int main(int argc, char *argv[])
{
   int ix;
   Composition c;
 
   printf("Function1(functionA, 3.0) = %f\n", Function1(WhichFunc(0), 3.0));
 
   for (ix=0; ix<4; ix++) {
       c = Compose(funcListA[ix], funcListB[ix]);
       printf("Compostion %d(0.9) = %f\n", ix, CallComposed(c, 0.9));
   }
 
   return 0;
}
```

===Non-portable function body duplication===
Following code generates true functions at run time.  Extremely unportable, and [http://en.wikipedia.org/wiki/Considered_harmful should be considered harmful] in general, but it's one (again, harmful) way for the truly desperate (or perhaps for people supporting only one platform -- and note that some other languages only work on one platform).

```C>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef double (*f_dbl)(double);
#define TAGF (f_dbl)0xdeadbeef
#define TAGG (f_dbl)0xbaddecaf

double dummy(double x)
{
	f_dbl f = TAGF;
	f_dbl g = TAGG;
	return f(g(x));
}

f_dbl composite(f_dbl f, f_dbl g)
{
	size_t len = (void*)composite - (void*)dummy;
	f_dbl ret = malloc(len);
	char *ptr;
	memcpy(ret, dummy, len);
	for (ptr = (char*)ret; ptr < (char*)ret + len - sizeof(f_dbl); ptr++) {
		if (*(f_dbl*)ptr == TAGF)      *(f_dbl*)ptr = f;
		else if (*(f_dbl*)ptr == TAGG) *(f_dbl*)ptr = g;
	}
	return ret;
}

double cube(double x)
{
	return x * x * x;
}

/* uncomment next line if your math.h doesn't have cbrt() */
/* double cbrt(double x) { return pow(x, 1/3.); } */

int main()
{
	int i;
	double x;

	f_dbl A[3] = { cube, exp, sin };
	f_dbl B[3] = { cbrt, log, asin}; /* not sure about availablity of cbrt() */
	f_dbl C[3];

	for (i = 0; i < 3; i++)
		C[i] = composite(A[i], B[i]);

	for (i = 0; i < 3; i++) {
		for (x = .2; x <= 1; x += .2)
			printf("C%d(%g) = %g\n", i, x, C[i](x));
		printf("\n");
	}
	return 0;
}
```
(Boring) output<lang>C0(0.2) = 0.2
C0(0.4) = 0.4
C0(0.6) = 0.6
C0(0.8) = 0.8
C0(1) = 1

C1(0.2) = 0.2
C1(0.4) = 0.4
C1(0.6) = 0.6
C1(0.8) = 0.8
C1(1) = 1

C2(0.2) = 0.2
C2(0.4) = 0.4
C2(0.6) = 0.6
C2(0.8) = 0.8
C2(1) = 1
```


=={{header|C sharp|C#}}==

```csharp
using System;

class Program
{
    static void Main(string[] args)
    {
        var cube = new Func<double, double>(x => Math.Pow(x, 3.0));
        var croot = new Func<double, double>(x => Math.Pow(x, 1 / 3.0));

        var functionTuples = new[]
        {
            (forward: Math.Sin, backward: Math.Asin),
            (forward: Math.Cos, backward: Math.Acos),
            (forward: cube,     backward: croot)
        };

        foreach (var ft in functionTuples)
        {
            Console.WriteLine(ft.backward(ft.forward(0.5)));
        }
    }
}

```


Output:

```txt
0.5
0.5
0.5
```



## C++

{{works with|C++11}}


```cpp

#include <functional>
#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
 
using std::cout;
using std::endl;
using std::vector;
using std::function;
using std::transform;
using std::back_inserter;

typedef function<double(double)> FunType;

vector<FunType> A = {sin, cos, tan, [](double x) { return x*x*x; } };
vector<FunType> B = {asin, acos, atan, [](double x) { return exp(log(x)/3); } };

template <typename A, typename B, typename C>
function<C(A)> compose(function<C(B)> f, function<B(A)> g) {
    return [f,g](A x) { return f(g(x)); };
}

int main() {
    vector<FunType> composedFuns;
    auto exNums = {0.0, 0.2, 0.4, 0.6, 0.8, 1.0};

    transform(B.begin(), B.end(),
                A.begin(),
                back_inserter(composedFuns),
                compose<double, double, double>);

    for (auto num: exNums)
        for (auto fun: composedFuns)
            cout << u8"f\u207B\u00B9.f(" << num << ") = " << fun(num) << endl;

    return 0;
}

```



## Ceylon

{{works with|Ceylon 1.2.1}}
First, you need to import the numeric module in you module.ceylon file

```ceylon
module rosetta "1.0.0" {
	import ceylon.numeric "1.2.1";
}
```

And then you can use the math functions in your run.ceylon file

```ceylon
import ceylon.numeric.float {

	sin, exp, asin, log
}

shared void run() {
	
	function cube(Float x) => x ^ 3;
	function cubeRoot(Float x) => x ^ (1.0 / 3.0);
	
	value functions = {sin, exp, cube};
	value inverses = {asin, log, cubeRoot};
	
	for([func, inv] in zipPairs(functions, inverses)) {
		print(compose(func, inv)(0.5));
	}
}
```



## Clojure


```clojure

(use 'clojure.contrib.math)
(let [fns [#(Math/sin %) #(Math/cos %) (fn [x] (* x x x))]
      inv [#(Math/asin %) #(Math/acos %) #(expt % 1/3)]]
  (map #(% 0.5) (map #(comp %1 %2) fns inv)))

```

Output: 

```txt
(0.5 0.4999999999999999 0.5000000000000001)
```



## CoffeeScript

{{trans|JavaScript}}

```coffeescript
# Functions as values of a variable
cube = (x) -> Math.pow x, 3
cuberoot = (x) -> Math.pow x, 1 / 3

# Higher order function
compose = (f, g) -> (x) -> f g(x)

# Storing functions in a array
fun = [Math.sin, Math.cos, cube]
inv = [Math.asin, Math.acos, cuberoot]

# Applying the composition to 0.5
console.log compose(inv[i], fun[i])(0.5) for i in [0..2]​​​​​​​
```

Output:

```txt
0.5
0.4999999999999999
0.5
```



## Common Lisp


```lisp
(defun compose (f g) (lambda (x) (funcall f (funcall g x))))
(defun cube (x) (expt x 3))
(defun cube-root (x) (expt x (/ 3)))

(loop with value = 0.5
      for func in (list #'sin  #'cos  #'cube     )
      for inverse  in (list #'asin #'acos #'cube-root)
      for composed = (compose inverse func)
      do (format t "~&(~A ∘ ~A)(~A) = ~A~%"
                 inverse
                 func
                 value 
                 (funcall composed value)))
```


Output:


```lisp
(#<FUNCTION ASIN> ∘ #<FUNCTION SIN>)(0.5) = 0.5
(#<FUNCTION ACOS> ∘ #<FUNCTION COS>)(0.5) = 0.5
(#<FUNCTION CUBE-ROOT> ∘ #<FUNCTION CUBE>)(0.5) = 0.5
```



## D


### Using Standard Compose


```d
void main() {
    import std.stdio, std.math, std.typetuple, std.functional;

    alias dir = TypeTuple!(sin,  cos,  x => x ^^ 3);
    alias inv = TypeTuple!(asin, acos, cbrt);
    // foreach (f, g; staticZip!(dir, inv))
    foreach (immutable i, f; dir)
        writefln("%6.3f", compose!(f, inv[i])(0.5));
}
```

{{out}}

```txt
 0.500
 0.500
 0.500
```



### Defining Compose

Here we need wrappers because the standard functions have different signatures (eg pure/nothrow). Same output.

```d
void main() {
    import std.stdio, std.math, std.range;

    static T delegate(S) compose(T, U, S)(in T function(in U) f,
                                          in U function(in S) g) {
        return s => f(g(s));
    }

    immutable sin  = (in real x) pure nothrow => x.sin,
              asin = (in real x) pure nothrow => x.asin,
              cos  = (in real x) pure nothrow => x.cos,
              acos = (in real x) pure nothrow => x.acos,
              cube = (in real x) pure nothrow => x ^^ 3,
              cbrt = (in real x) /*pure*/ nothrow => x.cbrt;

    foreach (f, g; [sin, cos, cube].zip([asin, acos, cbrt]))
        writefln("%6.3f", compose(f, g)(0.5));
}
```



## Dart


```dart
import 'dart:math' as Math;
cube(x) => x*x*x;
cuberoot(x)  => Math.pow(x, 1/3);
compose(f,g) => ((x)=>f(g(x)));
main(){
  var functions = [Math.sin, Math.exp, cube];
  var inverses = [Math.asin, Math.log, cuberoot];
  for (int i = 0; i < 3; i++){
    print(compose(functions[i], inverses[i])(0.5));
  }
}
```

{{out}}

```txt

0.49999999999999994
0.5
0.5000000000000001

```


=={{header|Déjà Vu}}==

```dejavu
negate:
	- 0

set :A [ @++ $ @negate @-- ]

set :B [ @-- $ @++ @negate ]

test n:
	for i range 0 -- len A:
		if /= n call compose @B! i @A! i n:
			return false
	true

test to-num !prompt "Enter a number: "
if:
	!print "f^-1(f(x)) = x"
else:
	!print "Something went wrong."

```


{{out}}

```txt
Enter a number: 23
f^-1(f(x)) = x
```



## Dyalect


===Create new functions from preexisting functions at run-time===

Using partial application:


```Dyalect
func apply(fun, x) { y => fun(x, y) }

func sum(x, y) { x + y }

func sum2 = apply(sum, 2)
```



### Store functions in collections



```Dyalect
func sum(x, y) { x + y }
func doubleMe(x) { x + x }

var arr = []
arr.add(sum)
arr.add(doubleMe)
arr.add(arr.toString)
```



### Use functions as arguments to other functions



```Dyalect
func Iterator.filter(pred) {
    for x in this when pred(x) {
        yield x
    }
}

[1,2,3,4,5].iter().filter(x => x % 2 == 0)
```



### Use functions as return values of other functions



```Dyalect
func flip(fun, x, y) {
    (y, x) => fun(x, y)
}
```



## E


First, a brief summary of the relevant semantics: In E, every value, including built-in and user-defined functions, "is an object" — it has methods which respond to messages. Methods are distinguished by the given name (''verb'') and the number of parameters (''arity''). By convention and syntactic sugar, a ''function'' is an object which has a method whose verb is "run".

The relevant mathematical operations are provided as methods on floats, so the first thing we must do is define them as functions.


```e
def sin(x)  { return x.sin() }
def cos(x)  { return x.cos() }
def asin(x) { return x.asin() }
def acos(x) { return x.acos() }
def cube(x) { return x ** 3     }
def curt(x) { return x ** (1/3) }

def forward := [sin,  cos,  cube]
def reverse := [asin, acos, curt]
```


There are no built-in functions in this list, since the original author couldn't easily think of any which had one parameter and were inverses of each other, but composition would work just the same with them.

Defining composition. <code>fn ''params'' { ''expr'' }</code> is shorthand for an anonymous function returning a value.

```e
def compose(f, g) {
    return fn x { f(g(x)) }
}
```



```e
? def x := 0.5  \
> for i => f in forward {
>     def g := reverse[i]
>     println(`x = $x, f = $f, g = $g, compose($f, $g)($x) = ${compose(f, g)(x)}`)
> }

x = 0.5, f = <sin>, g = <asin>, compose(<sin>, <asin>)(0.5) = 0.5
x = 0.5, f = <cos>, g = <acos>, compose(<cos>, <acos>)(0.5) = 0.4999999999999999
x = 0.5, f = <cube>, g = <curt>, compose(<cube>, <curt>)(0.5) = 0.5000000000000001
```


Note: <code>def g := reverse[i]</code> is needed here because E as yet has no defined protocol for iterating over collections in parallel. [http://wiki.erights.org/wiki/Parallel_iteration Page for this issue.]


## EchoLisp


```scheme

;; adapted from Racket
;; (compose f g h ... ) is a built-in defined as :
;; (define (compose f g) (λ (x) (f (g x))))

(define (cube x) (expt x 3))
(define (cube-root x) (expt x (// 1 3)))
(define funlist (list sin cos cube))
(define ifunlist (list asin acos cube-root))

(for ([f funlist] [i ifunlist])
  (writeln ((compose i f) 0.5)))
→ 
    0.5    
    0.4999999999999999    
    0.5   

```


## Ela

Translation of Haskell:


```ela
open number //sin,cos,asin,acos
open list //zipWith

cube x = x ** 3
croot x = x ** (1/3)

funclist = [sin, cos, cube]
funclisti = [asin, acos, croot]

zipWith (\f inversef -> (inversef << f) 0.5) funclist funclisti
```


Function (<<) is defined in standard prelude as:


```ela
(<<) f g x = f (g x)
```


Output (calculations are performed on 32-bit floats):


```ela
[0.5,0.5,0.499999989671302]
```




## Elena

ELENA 4.1 :

```elena
import system'routines;
import system'math;
import extensions'routines;
import extensions'math;
 
extension op
{
    compose(f,g)
        = f(g(self));
}
 
public program()
{
   var fs := new::( mssgconst sin<mathOp>[0], mssgconst cos<mathOp>[0], (x => power(x, 3.0r)) );
   var gs := new::( mssgconst arcsin<mathOp>[0], mssgconst arccos<mathOp>[0], (x => power(x, 1.0r / 3)) );
 
   fs.zipBy(gs, (f,g => 0.5r.compose(f,g)))
     .forEach:printingLn
}
```

{{out}}

```txt

0.5
0.5
0.5

```



## Elixir


```elixir
defmodule First_class_functions do
  def task(val) do
    as = [&:math.sin/1, &:math.cos/1, fn x -> x * x * x end]
    bs = [&:math.asin/1, &:math.acos/1, fn x -> :math.pow(x, 1/3) end]
    Enum.zip(as, bs)
    |> Enum.each(fn {a,b} -> IO.puts compose([a,b], val) end)
  end
  
  defp compose(funs, x) do
    Enum.reduce(funs, x, fn f,acc -> f.(acc) end)
  end
end

First_class_functions.task(0.5)
```


{{out}}

```txt

0.5
0.4999999999999999
0.5

```



## Erlang


```Erlang

-module( first_class_functions ).

-export( [task/0] ).

task() ->
	As = [fun math:sin/1, fun math:cos/1, fun cube/1],
	Bs = [fun math:asin/1, fun math:acos/1, fun square_inverse/1],
	[io:fwrite( "Value: 1.5 Result: ~p~n", [functional_composition([A, B], 1.5)]) || {A, B} <- lists:zip(As, Bs)].



functional_composition( Funs, X ) -> lists:foldl( fun(F, Acc) -> F(Acc) end, X, Funs ).

square( X ) -> math:pow( X, 2 ).

square_inverse( X ) -> math:sqrt( X ).

```

{{out}}

```txt

93> first_class_functions:task().
Value: 1.5 Result: 1.5000000000000002
Value: 1.5 Result: 1.5
Value: 1.5 Result: 1.5

```


=={{header|F_Sharp|F#}}==

```fsharp
open System

let cube x = x ** 3.0
let croot x = x ** (1.0/3.0) 

let funclist = [Math.Sin; Math.Cos; cube]
let funclisti = [Math.Asin; Math.Acos; croot]
let composed = List.map2 (<<) funclist funclisti

let main() = for f in composed do printfn "%f" (f 0.5)

main()
```


Output:

```txt
0.500000
0.500000
0.500000
```



## Factor

The constants A and B consist of arrays containing quotations (aka anonymous functions).

```factor
USING: assocs combinators kernel math.functions prettyprint sequences ;
IN: rosettacode.first-class-functions

CONSTANT: A { [ sin ] [ cos ] [ 3 ^ ] }
CONSTANT: B { [ asin ] [ acos ] [ 1/3 ^ ] } 

: compose-all ( seq1 seq2 -- seq ) [ compose ] 2map ;

: test-fcf ( -- )
    0.5 A B compose-all
    [ call( x -- y ) ] with map . ;
```

{{out}}

```txt
{ 0.5 0.4999999999999999 0.5 }
```



## Fantom


Methods defined for classes can be pulled out into functions, e.g. "Float#sin.func" pulls the sine method for floats out into a function accepting a single argument.  This function is then a first-class value.


```fantom

class FirstClassFns
{
  static |Obj -> Obj| compose (|Obj -> Obj| fn1, |Obj -> Obj| fn2)
  {
    return |Obj x -> Obj| { fn2 (fn1 (x)) }
  }

  public static Void main () 
  {
    cube := |Float a -> Float| { a * a * a }
    cbrt := |Float a -> Float| { a.pow(1/3f) }

    |Float->Float|[] fns := [Float#sin.func, Float#cos.func, cube]
    |Float->Float|[] inv := [Float#asin.func, Float#acos.func, cbrt]
    |Float->Float|[] composed := fns.map |fn, i| { compose(fn, inv[i]) }

    composed.each |fn| { echo (fn(0.5f)) }
  }
}

```


Output:

```txt

0.5
0.4999999999999999
0.5

```



## Forth


```forth
: compose ( xt1 xt2 -- xt3 )
  >r >r :noname
     r> compile,
     r> compile,
     postpone ;
;

: cube  fdup fdup f* f* ;
: cuberoot  1e 3e f/ f** ;

: table  create does> swap cells + @ ;

table fn      ' fsin ,  ' fcos ,  ' cube ,
table inverse ' fasin , ' facos , ' cuberoot ,

: main
  3 0 do
    i fn i inverse compose  ( xt )
    0.5e execute f.
  loop ;

main    \ 0.5 0.5 0.5
```


## FreeBASIC

Like C, FreeBASIC doesn't have first class functions so I've contented myself by translating their code:
{{trans|C}}

```freebasic
' FB 1.05.0 Win64

#Include "crt/math.bi"  '' include math functions in C runtime library

' Declare function pointer type
' This implicitly assumes default StdCall calling convention on Windows
Type Class2Func As Function(As Double) As Double  
 
' A couple of functions with the above prototype
Function functionA(v As Double) As Double
  Return v*v*v  '' cube of v
End Function

Function functionB(v As Double) As Double
  Return Exp(Log(v)/3)  '' same as cube root of v which would normally be v ^ (1.0/3.0) in FB
End Function
 
' A function taking a function as an argument 
Function function1(f2 As Class2Func, val_ As Double) As Double
  Return f2(val_)
End Function
 
' A function returning a function 
Function whichFunc(idx As Long) As Class2Func
   Return IIf(idx < 4, @functionA, @functionB)
End Function

' Additional function needed to treat CDecl function pointer as StdCall
' Get compiler warning otherwise
Function cl2(f As Function CDecl(As Double) As Double) As Class2Func
  Return CPtr(Class2Func, f)
End Function 

' A list of functions 
' Using C Runtime library versions of trig functions as it doesn't appear
' to be possible to apply address operator (@) to FB's built-in versions
Dim funcListA(0 To 3) As Class2Func = {@functionA, cl2(@sin_),  cl2(@cos_), cl2(@tan_)}
Dim funcListB(0 To 3) As Class2Func = {@functionB, cl2(@asin_), cl2(@acos_), cl2(@atan_)}

' Composing Functions
Function invokeComposed(f1 As Class2Func, f2 As Class2Func, val_ As double) As Double
   Return f1(f2(val_))
End Function

Type Composition 
  As Class2Func f1, f2
End Type

Function compose(f1 As Class2Func, f2 As Class2Func) As Composition Ptr
  Dim comp As Composition Ptr = Allocate(SizeOf(Composition))
  comp->f1 = f1
  comp->f2 = f2
  Return comp
End Function
 
Function callComposed(comp As Composition Ptr, val_ As Double ) As Double
  Return comp->f1(comp->f2(val_))
End Function

Dim ix As Integer
Dim c As Composition Ptr
 
Print "function1(functionA, 3.0) = "; CSng(function1(whichFunc(0), 3.0))
Print
For ix = 0 To 3
  c = compose(funcListA(ix), funcListB(ix))
  Print "Composition"; ix; "(0.9) = "; CSng(callComposed(c, 0.9))
Next
 
Deallocate(c)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

function1(functionA, 3.0) =  27

Composition 0(0.9) =  0.9
Composition 1(0.9) =  0.9
Composition 2(0.9) =  0.9
Composition 3(0.9) =  0.9

```



## GAP


```gap
# Function composition
Composition := function(f, g)
  local h;
  h := function(x)
    return f(g(x));
  end;
  return h;
end;

# Apply each function in list u, to argument x
ApplyList := function(u, x)
  local i, n, v;
  n := Size(u);
  v := [ ];
  for i in [1 .. n] do
    v[i] := u[i](x);
  od;
  return v;
end;

# Inverse and Sqrt are in the built-in library. Note that Sqrt yields values in cyclotomic fields.
# For example,
#    gap> Sqrt(7);
#    E(28)^3-E(28)^11-E(28)^15+E(28)^19-E(28)^23+E(28)^27
# where E(n) is a primitive n-th root of unity
a := [ i -> i + 1, Inverse, Sqrt ];
# [ function( i ) ... end, <Operation "InverseImmutable">, <Operation "Sqrt"> ]
b := [ i -> i - 1, Inverse, x -> x*x ];
# [ function( i ) ... end, <Operation "InverseImmutable">, function( x ) ... end ]

# Compose each couple
z := ListN(a, b, Composition);

# Now a test
ApplyList(z, 3);
[ 3, 3, 3 ]
```



## Go


```go
package main

import "math"
import "fmt"

// user-defined function, per task.  Other math functions used are built-in.
func cube(x float64) float64 { return math.Pow(x, 3) }

// ffType and compose function taken from Function composition task
type ffType func(float64) float64

func compose(f, g ffType) ffType {
    return func(x float64) float64 {
        return f(g(x))
    }
}

func main() {
    // collection A
    funclist := []ffType{math.Sin, math.Cos, cube}
    // collection B
    funclisti := []ffType{math.Asin, math.Acos, math.Cbrt}
    for i := 0; i < 3; i++ {
        // apply composition and show result
        fmt.Println(compose(funclisti[i], funclist[i])(.5))
    }
}
```

Output:

```txt

0.49999999999999994
0.5
0.5

```



## Groovy

Solution:

```groovy
def compose = { f, g -> { x -> f(g(x)) } }
```


Test program:

```groovy
def cube = { it * it * it }
def cubeRoot = { it ** (1/3) }

funcList = [ Math.&sin, Math.&cos, cube ]
inverseList = [ Math.&asin, Math.&acos, cubeRoot ]

println ([funcList, inverseList].transpose().collect { f, finv -> compose(f, finv) }.collect{ it(0.5) })
println ([inverseList, funcList].transpose().collect { finv, f -> compose(finv, f) }.collect{ it(0.5) })
```


Output:

```txt
[0.5, 0.4999999999999999, 0.5000000000346574]
[0.5, 0.4999999999999999, 0.5000000000346574]
```



## Haskell


```haskell>cube :: Floating a =
 a -> a
cube x = x ** 3.0

croot :: Floating a => a -> a
croot x = x ** (1/3)

-- compose already exists in Haskell as the `.` operator
-- compose :: (a -> b) -> (b -> c) -> a -> c
-- compose f g = \x -> g (f x)

funclist :: Floating a => [a -> a]
funclist = [sin,  cos,  cube ]

invlist :: Floating a => [a -> a]
invlist  = [asin, acos, croot]

main :: IO ()
main = print $ zipWith (\f i -> f . i $ 0.5) funclist invlist 
```

{{output}}

```txt
[0.5,0.4999999999999999,0.5000000000000001]
```


=={{header|Icon}} and {{header|Unicon}}==
The Unicon solution can be modified to work in Icon. See [[Function_composition#Icon_and_Unicon]].

```Unicon
link compose 
procedure main(arglist)

    fun := [sin,cos,cube]
    inv := [asin,acos,cuberoot]
    x := 0.5
    every i := 1 to *inv do 
       write("f(",x,") := ", compose(inv[i],fun[i])(x))
end

procedure cube(x)
return x*x*x
end

procedure cuberoot(x)
return x ^ (1./3)
end
```

Please refer to See [[Function_composition#Icon_and_Unicon]] for 'compose'.

Sample Output:

```txt
f(0.5) := 0.5
f(0.5) := 0.4999999999999999
f(0.5) := 0.5
```



## J



### Explicit version


J has some subtleties which are not addressed in this specification (J functions have grammatical character and their [[wp:Gerund|gerundial form]] may be placed in data structures where the spec sort of implies that there be no such distinction - for those uncomfortable with this terminology it is best to think of these as type distinctions - the type which appears in data structures and the type which may be applied are distinct though each may be directly derived from the other).

However, here are the basics which were requested:

```j
   sin=:  1&o.
   cos=:  2&o.
  cube=: ^&3
square=: *:
  unqo=: `:6
  unqcol=: `:0
  quot=: 1 :'{.u`'''''
  A=: sin`cos`cube`square
  B=: monad def'y unqo inv quot'"0 A
  BA=. A dyad def'x unqo@(y unqo) quot'"0 B
```


<lang>   A unqcol 0.5
0.479426 0.877583 0.125 0.25
   BA unqcol 0.5
0.5 0.5 0.5 0.5
```


===Tacit (unorthodox) version===
In J only adverbs and conjunctions (functionals) can produce verbs (functions)... Unless they are forced to cloak as verbs (functions). (Note that this takes advantage of a bug/feature of the interpreter ; see [http://rosettacode.org/wiki/Closures/Value_capture#Tacit_.28unorthodox.29_version unorthodox tacit] .)  The resulting functions (which correspond to functionals) can take and produce functions:


```j
   train  =. (<'`:')(0:`)(,^:)&6   NB. Producing the function  train    corresponding to the functional `:6
   inverse=. (<'^:')(0:`)(,^:)&_1  NB. Producing the function  inverse  corresponding to the functional ^:_1
   compose=. (<'@:')(0:`)(,^:)     NB. Producing the function  compose  corresponding to the functional @:
   an     =. <@:((,'0') ; ])       NB. Producing the atomic representation of a noun
   of     =. train@:([ ; an)       NB. Evaluating a function for an argument
   box    =. < @: train"0          NB. Producing a boxed list of the trains of the components
   
   ]A =. box (1&o.)`(2&o.)`(^&3)   NB. Producing a boxed list containing the Sin, Cos and Cubic functions 
┌────┬────┬───┐
│1&o.│2&o.│^&3│
└────┴────┴───┘
   ]B =. inverse &.> A             NB. Producing their inverses
┌────────┬────────┬───────┐
│1&o.^:_1│2&o.^:_1│^&3^:_1│
└────────┴────────┴───────┘
   ]BA=. B compose &.> A           NB. Producing the compositions of the functions and their inverses
┌────────────────┬────────────────┬──────────────┐
│1&o.^:_1@:(1&o.)│2&o.^:_1@:(2&o.)│^&3^:_1@:(^&3)│
└────────────────┴────────────────┴──────────────┘
   BA of &> 0.5                    NB. Evaluating the compositions at 0.5
0.5 0.5 0.5

```



## Java

Java doesn't technically have first-class functions. Java can simulate first-class functions to a certain extent, with anonymous classes and generic function interface. 
{{works with|Java|1.5+}}

```java5
import java.util.ArrayList;

public class FirstClass{
	
	public interface Function<A,B>{
		B apply(A x);
	}
	
	public static <A,B,C> Function<A, C> compose(
			final Function<B, C> f, final Function<A, B> g) {
		return new Function<A, C>() {
			@Override public C apply(A x) {
				return f.apply(g.apply(x));
			}
		};
	}
	 
	public static void main(String[] args){
		ArrayList<Function<Double, Double>> functions =
			new ArrayList<Function<Double,Double>>();
		
		functions.add(
				new Function<Double, Double>(){
					@Override public Double apply(Double x){
						return Math.cos(x);
					}
				});
		functions.add(
				new Function<Double, Double>(){
					@Override public Double apply(Double x){
						return Math.tan(x);
					}
				});
		functions.add(
				new Function<Double, Double>(){
					@Override public Double apply(Double x){
						return x * x;
					}
				});
		
		ArrayList<Function<Double, Double>> inverse = new ArrayList<Function<Double,Double>>();
		
		inverse.add(
				new Function<Double, Double>(){
					@Override public Double apply(Double x){
						return Math.acos(x);
					}
				});
		inverse.add(
				new Function<Double, Double>(){
					@Override public Double apply(Double x){
						return Math.atan(x);
					}
				});
		inverse.add(
				new Function<Double, Double>(){
					@Override public Double apply(Double x){
						return Math.sqrt(x);
					}
				});
		System.out.println("Compositions:");
		for(int i = 0; i < functions.size(); i++){
			System.out.println(compose(functions.get(i), inverse.get(i)).apply(0.5));
		}
		System.out.println("Hard-coded compositions:");
		System.out.println(Math.cos(Math.acos(0.5)));
		System.out.println(Math.tan(Math.atan(0.5)));
		System.out.println(Math.pow(Math.sqrt(0.5), 2));
	}
}
```

Output:

```txt
Compositions:
0.4999999999999999
0.49999999999999994
0.5000000000000001
Hard-coded compositions:
0.4999999999999999
0.49999999999999994
0.5000000000000001
```


{{works with|Java|8+}}

```java5
import java.util.ArrayList;
import java.util.function.Function;

public class FirstClass{
	
	public static void main(String... arguments){
		ArrayList<Function<Double, Double>> functions = new ArrayList<>();
		
		functions.add(Math::cos);
		functions.add(Math::tan);
		functions.add(x -> x * x);
		
		ArrayList<Function<Double, Double>> inverse = new ArrayList<>();
		
		inverse.add(Math::acos);
		inverse.add(Math::atan);
		inverse.add(Math::sqrt);
		System.out.println("Compositions:");
		for (int i = 0; i < functions.size(); i++){
			System.out.println(functions.get(i).compose(inverse.get(i)).apply(0.5));
		}
		System.out.println("Hard-coded compositions:");
		System.out.println(Math.cos(Math.acos(0.5)));
		System.out.println(Math.tan(Math.atan(0.5)));
		System.out.println(Math.pow(Math.sqrt(0.5), 2));
	}
}
```



## JavaScript


### ES5


```javascript
// Functions as values of a variable
var cube = function (x) {
  return Math.pow(x, 3);
};
var cuberoot = function (x) {
  return Math.pow(x, 1 / 3);
};

// Higher order function
var compose = function (f, g) {
  return function (x) {
    return f(g(x));
  };
};

// Storing functions in a array
var fun = [Math.sin, Math.cos, cube];
var inv = [Math.asin, Math.acos, cuberoot];

for (var i = 0; i < 3; i++) {
  // Applying the composition to 0.5
  console.log(compose(inv[i], fun[i])(0.5));
}
```



### ES6


```javascript
// Functions as values of a variable
var cube = x => Math.pow(x, 3);

var cuberoot = x => Math.pow(x, 1 / 3);


// Higher order function
var compose = (f, g) => (x => f(g(x)));

// Storing functions in a array
var fun = [ Math.sin, Math.cos, cube ];
var inv = [ Math.asin, Math.acos, cuberoot ];

for (var i = 0; i < 3; i++) {
  // Applying the composition to 0.5
  console.log(compose(inv[i], fun[i])(0.5));
}
```



Result is always: 

```txt
0.5
0.4999999999999999
0.5
```



## Julia


```Julia
#!/usr/bin/julia

function compose(f::Function, g::Function)
  return x -> f(g(x))
end

value = 0.5
for pair in [(sin, asin), (cos, acos), (x -> x^3, x -> x^(1/3))]
  func, inverse = pair
  println(compose(func, inverse)(value))
end
```


Output: 

```txt
0.5
0.4999999999999999
0.5000000000000001

```




## Kotlin


```scala
// version 1.0.6

fun compose(f: (Double) -> Double,  g: (Double) -> Double ): (Double) -> Double  = { f(g(it)) }

fun cube(d: Double) = d * d * d

fun main(args: Array<String>) {
    val listA = listOf(Math::sin, Math::cos, ::cube)
    val listB = listOf(Math::asin, Math::acos, Math::cbrt)
    val x = 0.5
    for (i in 0..2) println(compose(listA[i], listB[i])(x))
}
```


{{out}}

```txt

0.5
0.4999999999999999
0.5000000000000001

```



## Lambdatalk

Tested in [http://epsilonwiki.free.fr/ELS_YAW/?view=p227]

```Scheme

{def cube {lambda {:x} {pow :x 3}}}
{def cuberoot {lambda {:x} {pow :x {/ 1 3}}}}
{def compose {lambda {:f :g :x} {:f {:g :x}}}}
{def fun sin cos cube}
{def inv asin acos cuberoot}
{def display {lambda {:i} 
  {br}{compose {nth :i {fun}} 
               {nth :i {inv}} 0.5}}}
{map display {serie 0 2}}

Output:
0.5 
0.49999999999999994 
0.5000000000000001

```



## Lasso


```Lasso
#!/usr/bin/lasso9

define cube(x::decimal) => {
	return #x -> pow(3.0)
}

define cuberoot(x::decimal) => {
	return #x -> pow(1.0/3.0)
}

define compose(f, g, v) => {
	return {
		return #f -> detach -> invoke(#g -> detach -> invoke(#1))
	} -> detach -> invoke(#v)
}


local(functions = array({return #1 -> sin}, {return #1 -> cos}, {return cube(#1)}))
local(inverse = array({return #1 -> asin}, {return #1 -> acos}, {return cuberoot(#1)}))

loop(3)
	stdoutnl(
		compose(
			#functions -> get(loop_count),
			#inverse -> get(loop_count),
			0.5
		)
	)

/loop
```


Output:

```txt
0.500000
0.500000
0.500000

```



## Lingo

Lingo does not support functions as first-class objects. But with the limitations described under [https://www.rosettacode.org/wiki/Function_composition#Lingo Function composition] the task can be solved:

```lingo
-- sin, cos and sqrt are built-in, square, asin and acos are user-defined
A = [#sin, #cos, #square]
B = [#asin, #acos, #sqrt]

testValue = 0.5

repeat with i = 1 to 3
  -- for implementation details of compose() see https://www.rosettacode.org/wiki/Function_composition#Lingo
  f = compose(A[i], B[i])
  res = call(f, _movie, testValue)
  put res = testValue
end repeat
```


{{out}}

```txt

 -- 1
 -- 1
 -- 1

```


User-defined arithmetic functions used in code above:

```lingo
on square (x)
  return x*x
end

on asin (x)
  res = atan(sqrt(x*x/(1-x*x)))
  if x<0 then res = -res
  return res
end

on acos (x)
  return PI/2 - asin(x)
end
```



## Lua


```lua
function compose(f,g) return function(...) return f(g(...)) end end

fn = {math.sin, math.cos, function(x) return x^3 end}
inv = {math.asin, math.acos, function(x) return x^(1/3) end}

for i, v in ipairs(fn) do
  local f = compose(v, inv[i])
  print(f(0.5))
end
```


Output:
<lang>0.5
0.5
0.5
```



## M2000 Interpreter

Cos, Sin works with degrees, Number pop number from stack of values, so we didn't use a variable like this POW3INV =Lambda (x)->x**(1/3)

```M2000 Interpreter

Module CheckFirst {
      RAD = lambda -> number/180*pi
      ASIN = lambda RAD -> {
          Read x : x=Round(x,10)
            If x>=0 and X<1 Then {
                  =RAD(abs(2*Round(ATN(x/(1+SQRT(1-x**2))))))
            } Else.if x==1 Then {
                  =RAD(90)
            } Else error "asin exit limit"
      }
      ACOS=lambda ASIN (x) -> PI/2 - ASIN(x)
      POW3 = Lambda ->number**3
      POW3INV =Lambda ->number**(1/3)
      COSRAD =lambda ->Cos(number*180/pi)
      SINRAD=lambda ->Sin(number*180/pi)
      Composed=lambda (f1, f2) -> {
            =lambda f1, f2 (x)->{
                  =f1(f2(x))
            }
      }
      Dim Base 0, A(3), B(3), C(3)
      A(0)=ACOS, ASIN, POW3INV
      B(0)=COSRAD, SINRAD, POW3
      C(0)=Composed(ACOS, COSRAD), Composed(ASIN, SINRAD), Composed(POW3INV, POW3)
      Print $("0.00")
      For i=0 To 2 {
            Print A(i)(B(i)(.5)), C(i)(.5)
      }
}
CheckFirst

```



## Maple

The composition operator in Maple is denoted by "@".  We use "zip" to produce the list of compositions.  The cubing procedure and its inverse are each computed.

```Maple

> A := [ sin, cos, x -> x^3 ]:
> B := [ arcsin, arccos, rcurry( surd, 3 ) ]:
> zip( `@`, A, B )( 2/3 );
                            [2/3, 2/3, 2/3]

> zip( `@`, B, A )( 2/3 );
                            [2/3, 2/3, 2/3]

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
The built-in function Composition can do composition, a custom function that does the same would be compose[f_,g_]:=f[g[#]]&. However the latter only works with 2 arguments, Composition works with any number of arguments.

```Mathematica
funcs = {Sin, Cos, #^3 &};
funcsi = {ArcSin, ArcCos, #^(1/3) &};
compositefuncs = Composition @@@ Transpose[{funcs, funcsi}];
Table[i[0.666], {i, compositefuncs}]
```

gives back:

```Mathematica
{0.666, 0.666, 0.666}
```

Note that I implemented cube and cube-root as pure functions. This shows that Mathematica is fully able to handle functions as variables, functions can return functions, and functions can be given as an argument. Composition can be done in more than 1 way:

```Mathematica
Composition[f,g,h][x]
f@g@h@x
x//h//g//f
```

all give back:

```Mathematica
f[g[h[x]]]
```



## Maxima


```maxima
a: [sin, cos, lambda([x], x^3)]$
b: [asin, acos, lambda([x], x^(1/3))]$
compose(f, g) := buildq([f, g], lambda([x], f(g(x))))$
map(lambda([fun], fun(x)), map(compose, a, b));
[x, x, x]
```



## Mercury

This solution uses the <code>compose/3</code> function defined in <code>std_util</code> (part of the Mercury standard library) to demonstrate the use of first-class functions.  The following process is followed:
# A list of "forward" functions is provided (sin, cosine and a lambda that calls ln).
# A list of "reverse" functions is provided (asin, acosine and a lambda that calls exp).
# The lists are mapped in corresponding members through an anonymous function that composes the resulting pairs of functions and applies them to the value 0.5.
# The results are returned and printed when all function pairs have been processed.


### firstclass.m


```Mercury

:- module firstclass.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module exception, list, math, std_util.

main(!IO) :-
    Forward = [sin,  cos,  (func(X) = ln(X))],
    Reverse = [asin, acos, (func(X) = exp(X))],
    Results = map_corresponding(
        (func(F, R) = compose(R, F, 0.5)), 
        Forward, Reverse),
    write_list(Results, ", ", write_float, !IO),
    write_string("\n", !IO).

```



### Use and output


```txt
<nowiki> $ mmc -E firstclass.m && ./firstclass 
0.5, 0.4999999999999999, 0.5</nowiki>
```


(Limitations of the IEEE floating point representation make the cos/acos pairing lose a little bit of accuracy.)


## min

{{works with|min|0.19.3}}
Note <code>concat</code> is what performs the function composition, as functions are lists in min.

```min
('sin 'cos (3 pow)) =A
('asin 'acos (1 3 / pow)) =B

(A bool) (
  0.5 A first B first concat -> puts!
  A rest #A
  B rest #B
) while
```

{{out}}

```txt

0.5
0.4999999999999999
0.5

```



## Nemerle


{{trans|Python}}

```Nemerle
using System;
using System.Console;
using System.Math;
using Nemerle.Collections.NCollectionsExtensions;

module FirstClassFunc
{
    Main() : void
    {
        def cube = fun (x) {x * x * x};
        def croot = fun (x) {Pow(x, 1.0/3.0)};
        def compose = fun(f, g) {fun (x) {f(g(x))}};
        def funcs = [Sin, Cos, cube];
        def ifuncs = [Asin, Acos, croot];
        WriteLine($[compose(f, g)(0.5) | (f, g) in ZipLazy(funcs, ifuncs)]);
    }
}
```



### Use and Output

 C:\Rosetta>ncc -o:FirstClassFunc FirstClassFunc.n
 
 C:Rosetta>FirstClassFunc
 [0.5, 0.5, 0.5]


## newLISP


```newLISP>
 (define (compose f g) (expand (lambda (x) (f (g x))) 'f 'g))
(lambda (f g) (expand (lambda (x) (f (g x))) 'f 'g))
> (define (cube x) (pow x 3))
(lambda (x) (pow x 3))
> (define (cube-root x) (pow x (div 1 3)))
(lambda (x) (pow x (div 1 3)))
> (define functions '(sin  cos  cube))
(sin cos cube)
> (define inverses  '(asin acos cube-root))
(asin acos cube-root)
> (map (fn (f g) ((compose f g) 0.5)) functions inverses)
(0.5 0.5 0.5)

```



## Nim

{{trans|ES6}} 

```nim
from math import nil

proc cube(x: float64) : float64 {.procvar.} =
  math.pow(x, 3)

proc cuberoot(x: float64) : float64 {.procvar.} =
  math.pow(x, 1/3)

proc compose[A](f: proc(x: A): A, g: proc(x: A): A) : (proc(x: A): A) =
  proc c(x: A): A {.closure.} =
    f(g(x))
  return c

proc sin(x: float64) : float64 {.procvar.} =
  math.sin(x)
proc asin(x: float64) : float64 {.procvar.}=
  math.arcsin(x)
proc cos(x: float64) : float64 {.procvar.} =
  math.cos(x)
proc acos(x: float64) : float64 {.procvar.} =
  math.arccos(x)

var fun = @[sin, cos, cube]
var inv = @[asin, acos, cuberoot]

for i in 0..2:
  echo $compose(inv[i], fun[i])(0.5)
```

Output:

```txt
0.5
0.4999999999999999
0.5
```



## Objeck


```objeck
use Collection.Generic;

lambdas Func {
  Double : (FloatHolder) ~ FloatHolder
}

class FirstClass {
  function : Main(args : String[]) ~ Nil {
    vector := Vector->New()<Func2Holder <FloatHolder, FloatHolder> >;
    # store functions in collections
    vector->AddBack(Func2Holder->New(\Func->Double : (v) 
      =>  v * v)<FloatHolder, FloatHolder>);
    # new function from preexisting function at run-time
    vector->AddBack(Func2Holder->New(\Func->Double : (v) 
      => Float->SquareRoot(v->Get()))<FloatHolder, FloatHolder>);
    # process collection
    each(i : vector) {
      # return value of other functions and pass argument to other function
      Show(vector->Get(i)<Func2Holder>->Get()<FloatHolder, FloatHolder>);
    };
  }
  
  function : Show(func : (FloatHolder) ~ FloatHolder) ~ Nil {
    func(13.5)->Get()->PrintLine();
  }
}
```



## OCaml


```ocaml
# let cube x = x ** 3. ;;
val cube : float -> float = <fun>

# let croot x = x ** (1. /. 3.) ;;
val croot : float -> float = <fun>

# let compose f g = fun x -> f (g x) ;;  (* we could have written "let compose f g x = f (g x)" but we show this for clarity *)
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>

# let funclist = [sin; cos; cube] ;;
val funclist : (float -> float) list = [<fun>; <fun>; <fun>]

# let funclisti = [asin; acos; croot] ;;
val funclisti : (float -> float) list = [<fun>; <fun>; <fun>]

# List.map2 (fun f inversef -> (compose inversef f) 0.5) funclist funclisti ;;
- : float list = [0.5; 0.499999999999999889; 0.5]
```



## Octave


```octave
function r = cube(x)
  r = x.^3;
endfunction

function r = croot(x)
  r = x.^(1/3);
endfunction

compose = @(f,g) @(x) f(g(x));

f1 = {@sin, @cos, @cube};
f2 = {@asin, @acos, @croot};

for i = 1:3
  disp(compose(f1{i}, f2{i})(.5))
endfor
```


{{Out}}

```txt
 0.50000
 0.50000
 0.50000
```



## Oforth


```Oforth
: compose(f, g)   #[ g perform f perform ] ;

[ #cos, #sin, #[ 3 pow ] ] [ #acos, #asin, #[ 3 inv powf ] ] zipWith(#compose)
map(#[ 0.5 swap perform ]) conform(#[ 0.5 == ]) println

```

{{Out}}

```txt

1

```



## Ol


```scheme

; creation of new function from preexisting functions at run-time
(define (compose f g) (lambda (x) (f (g x))))

; storing functions in collection
(define (quad x) (* x x x x))
(define (quad-root x) (sqrt (sqrt x)))

(define collection (tuple quad quad-root))

; use functions as arguments to other functions
; and use functions as return values of other functions
(define identity (compose (ref collection 2) (ref collection 1)))
(print (identity 11211776))

```



## Oz

This is now also compatible with Oz v 2.0
(To be executed in the Oz OPI, by typing ctl+. ctl+b)


```oz
declare

  fun {Compose F G}
    fun {$ X}
      {F {G X}}
    end
  end

  fun {Cube X} {Number.pow X 3.0} end

  fun {CubeRoot X} {Number.pow X 1.0/3.0} end

in

  for
     F in [Float.sin  Float.cos  Cube]
     I in [Float.asin Float.acos CubeRoot]
  do
     {Show {{Compose I F} 0.5}}
  end

```

This will output the following in the Emulator output window

```oz

0.5
0.5
0.5

```



## PARI/GP

{{works with|PARI/GP|2.4.2 and above}}

```parigp
compose(f,g)={
  x -> f(g(x))
};

fcf()={
  my(A,B);
  A=[x->sin(x), x->cos(x), x->x^2];
  B=[x->asin(x), x->acos(x), x->sqrt(x)];
  for(i=1,#A,
    print(compose(A[i],B[i])(.5))
  )
};
```

Usage note: In Pari/GP 2.4.3 the vectors can be written as

```parigp
  A=[sin, cos, x->x^2];
  B=[asin, acos, x->sqrt(x)];
```


Output:

```txt
0.5000000000000000000000000000
0.5000000000000000000000000000
0.5000000000000000000000000000
```



## Perl


```perl
use Math::Complex ':trig';

sub compose {
    my ($f, $g) = @_;
    
    sub {
        $f -> ($g -> (@_));
    };
}

my $cube  = sub { $_[0] ** (3)   };
my $croot = sub { $_[0] ** (1/3) };

my @flist1 = ( \&Math::Complex::sin, \&Math::Complex::cos, $cube  );
my @flist2 = ( \&asin,               \&acos,               $croot );

print join "\n", map {
    compose($flist1[$_], $flist2[$_]) -> (0.5)   
} 0..2;
```

Output:

```txt
0.5
0.5
0.5
```



## Perl 6

Here we use the <tt>Z</tt> ("zipwith") metaoperator to zip the 𝐴 and 𝐵 lists with a user-defined compose function, expressed as an infix operator, <tt>∘</tt>.  The <tt>.()</tt> construct invokes the function contained in the <tt>$_</tt> (current topic) variable.

```perl6
sub infix:<∘> (&𝑔, &𝑓) { -> \x { 𝑔 𝑓 x } }

my \𝐴 = &sin,  &cos,  { $_ ** <3/1> }
my \𝐵 = &asin, &acos, { $_ ** <1/3> }

say .(.5) for 𝐴 Z∘ 𝐵
```

Output:

```txt
0.5
0.5
0.5
```


Operators, both buildin and user-defined, are first class too.


```perl6
my @a = 1,2,3;
my @op = &infix:<+>, &infix:<->, &infix:<*>;
for flat @a Z @op -> $v, &op { say 42.&op($v) }
```


{{output}}

```txt
43
40
126
```



## Phix

There is not really any direct support for this sort of thing in Phix, but it is all pretty trivial to manage explicitly.

In the following, as it stands, constant m cannot be used the same way as a routine_id, and a standard routine_id cannot be passed to call_composite, but tagging ctable entries so that you know exactly what to do with them does not sound difficult to me.

```Phix
sequence ctable = {}

function compose(integer f, integer g)
    ctable = append(ctable,{f,g})
    return length(ctable)   
end function

function call_composite(integer f, atom x)
integer g
    {f,g} = ctable[f]
    return call_func(f,{call_func(g,{x})})
end function

function plus1(atom x)
    return x+1
end function

function halve(atom x)
    return x/2
end function

constant m = compose(routine_id("halve"),routine_id("plus1"))

?call_composite(m,1)    -- displays 1
?call_composite(m,4)    -- displays 2.5
```



## PHP


{{trans|JavaScript}}
{{works with|PHP|5.3+}}

Non-anonymous functions can only be passed around by name, but the syntax for calling them is identical in both cases. Object or class methods require a different syntax involving array pseudo-types and <tt>call_user_func</tt>. So PHP could be said to have ''some'' first class functionality.


```php
$compose = function ($f, $g) {
    return function ($x) use ($f, $g) {
        return $f($g($x));
    };
};

$fn  = array('sin', 'cos', function ($x) { return pow($x, 3); });
$inv = array('asin', 'acos', function ($x) { return pow($x, 1/3); });

for ($i = 0; $i < 3; $i++) {
    $f = $compose($inv[$i], $fn[$i]);
    echo $f(0.5), PHP_EOL;
}
```


Output:


```txt
0.5
0.5
0.5
```



## PicoLisp


```PicoLisp
(load "@lib/math.l")

(de compose (F G)
   (curry (F G) (X)
      (F (G X)) ) )

(de cube (X)
   (pow X 3.0) )

(de cubeRoot (X)
   (pow X 0.3333333) )

(mapc
   '((Fun Inv)
      (prinl (format ((compose Inv Fun) 0.5) *Scl)) )
   '(sin  cos  cube)
   '(asin acos cubeRoot) )
```

Output:

```txt
0.500001
0.499999
0.500000
```



## PostScript



```ps

% PostScript has 'sin' and 'cos', but not these
/asin { dup dup 1. add exch 1. exch sub mul sqrt atan } def
/acos { dup dup 1. add exch 1. exch sub mul sqrt exch atan } def

/cube { 3 exp } def
/cuberoot { 1. 3. div exp } def

/compose { % f g -> { g f }
  [ 3 1 roll exch
  % procedures are not executed when encountered directly
  % insert an 'exec' after procedures, but not after operators
  1 index type /operatortype ne { /exec cvx exch } if
  dup type /operatortype ne { /exec cvx } if
  ] cvx
} def

/funcs [ /sin load /cos load /cube load ] def
/ifuncs [ /asin load /acos load /cuberoot load ] def

0 1 funcs length 1 sub { /i exch def
  ifuncs i get funcs i get compose
  .5 exch exec ==
} for

```



## Prolog

Works with SWI-Prolog and module lambda, written by <b>Ulrich Neumerkel</b> found here: http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl


```Prolog
:- use_module(library(lambda)).


compose(F,G, FG) :-
	FG =  \X^Z^(call(G,X,Y), call(F,Y,Z)).

cube(X, Y) :-
	Y is X ** 3.

cube_root(X, Y) :-
	Y is X ** (1/3).

first_class :-
	L = [sin, cos, cube],
	IL = [asin, acos, cube_root],

	% we create the composed functions
	maplist(compose, L, IL, Lst),

	% we call the functions
	maplist(call, Lst, [0.5,0.5,0.5], R),

	% we display the results
	maplist(writeln, R).

```

Output :

```txt
 ?- first_class.
0.5
0.4999999999999999
0.5000000000000001
true.

```



## Python



```python>>>
 # Some built in functions and their inverses
>>> from math import sin, cos, acos, asin
>>> # Add a user defined function and its inverse
>>> cube = lambda x: x * x * x
>>> croot = lambda x: x ** (1/3.0)
>>> # First class functions allow run-time creation of functions from functions
>>> # return function compose(f,g)(x) == f(g(x))
>>> compose = lambda f1, f2: ( lambda x: f1(f2(x)) )
>>> # first class functions should be able to be members of collection types
>>> funclist = [sin, cos, cube]
>>> funclisti = [asin, acos, croot]
>>> # Apply functions from lists as easily as integers
>>> [compose(inversef, f)(.5) for f, inversef in zip(funclist, funclisti)]
[0.5, 0.4999999999999999, 0.5]
>>>
```



Or, equivalently:
{{Works with|Python|3.7}}

```python
'''First-class functions'''

from math import (acos, cos, asin, sin)
from inspect import signature


# main :: IO ()
def main():
    '''Composition of several functions.'''

    pwr = flip(curry(pow))

    fs = [sin, cos, pwr(3.0)]
    ifs = [asin, acos, pwr(1 / 3.0)]

    print([
        f(0.5) for f in
        zipWith(compose)(fs)(ifs)
    ])


# GENERIC FUNCTIONS ------------------------------


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# curry :: ((a, b) -> c) -> a -> b -> c
def curry(f):
    '''A curried function derived
       from an uncurried function.'''
    return lambda a: lambda b: f(a, b)


# flip :: (a -> b -> c) -> b -> a -> c
def flip(f):
    '''The (curried or uncurried) function f with its
       two arguments reversed.'''
    if 1 < len(signature(f).parameters):
        return lambda a, b: f(b, a)
    else:
        return lambda a: lambda b: f(b)(a)


# zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
def zipWith(f):
    '''A list constructed by zipping with a
       custom function, rather than with the
       default tuple constructor.'''
    return lambda xs: lambda ys: [
        f(a)(b) for (a, b) in zip(xs, ys)
    ]


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
[0.49999999999999994, 0.5000000000000001, 0.5000000000000001]
```



## R


```R
cube <- function(x) x^3
croot <- function(x) x^(1/3)
compose <- function(f, g) function(x){f(g(x))}

f1 <- c(sin, cos, cube)
f2 <- c(asin, acos, croot)

for(i in 1:3) {
  print(compose(f1[[i]], f2[[i]])(.5))
}
```


{{Out}}

```txt
[1] 0.5
[1] 0.5
[1] 0.5
```


Alternatively:


```R
sapply(mapply(compose,f1,f2),do.call,list(.5))
```


{{Out}}

```txt
[1] 0.5 0.5 0.5
```



## Racket


```racket
#lang racket

(define (compose f g) (λ (x) (f (g x))))
(define (cube x) (expt x 3))
(define (cube-root x) (expt x (/ 1 3)))
(define funlist (list sin cos cube))
(define ifunlist (list asin acos cube-root))
 
(for ([f funlist] [i ifunlist])
  (displayln ((compose i f) 0.5)))
```


{{out}}

```txt

0.5
0.4999999999999999
0.5

```



## REBOL

{{incomplete|REBOL|Fails to demonstrate that the result of applying the composition of each function in A and its inverse in B to a value, is the original value}}

```REBOL
REBOL [
	Title: "First Class Functions"
	URL: http://rosettacode.org/wiki/First-class_functions
]

; Functions "foo" and "bar" are used to prove that composition
; actually took place by attaching their signatures to the result.

foo: func [x][reform ["foo:" x]]
bar: func [x][reform ["bar:" x]]

cube:  func [x][x * x * x]
croot: func [x][power  x  1 / 3]

; "compose" means something else in REBOL, so I "fashion" an alternative. 

fashion: func [f1 f2][
	do compose/deep [func [x][(:f1) (:f2) x]]]

A: [foo sine    cosine    cube]
B: [bar arcsine arccosine croot]

while [not tail? A][
	fn: fashion get A/1 get B/1
	source fn ; Prove that functions actually got composed.
	print [fn 0.5  crlf]

	A: next A  B: next B  ; Advance to next pair.
]
```



## REXX

The REXX language doesn't have any trigonometric functions built-in, nor the square root function, so several higher-math functions are included herein as RYO functions.

The only REXX functions that have an inverse are: 
::::*   d2x   ◄──►   x2d
::::*   d2c   ◄──►   c2d
::::*   c2x   ◄──►   x2c
These six functions (generally) only support non-negative integers, so a special test in the program below only

supplies appropriate integers when testing the first function listed in the   '''A'''   collection.

```rexx
/*REXX program demonstrates first─class functions (as a list of the names of functions).*/
A = 'd2x   square   sin    cos'                  /*a list of  functions  to demonstrate.*/
B = 'x2d   sqrt     Asin   Acos'                 /*the inverse functions of above list. */
w=digits()                                       /*W:  width of numbers to be displayed.*/
                                                 /* [↓]  collection of  A & B  functions*/
     do j=1  for words(A);      say;       say   /*step through the list;  2 blank lines*/
     say center("number",w)     center('function', 3*w+1)     center("inverse", 4*w)
     say copies("─"     ,w)     copies("─",        3*w+1)     copies("─",       4*w)
     if j<2   then call test j, 20  60   500     /*functions  X2D, D2X:  integers only. */
              else call test j, 0  0.5  1  2     /*all other functions:  floating point.*/
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Acos:    procedure; parse arg x; if x<-1|x>1  then call AcosErr;    return .5*pi()-Asin(x)
r2r:     return arg(1) // (pi()*2)               /*normalize radians ──► 1 unit circle*/
square:  return arg(1) ** 2
pi:      pi=3.14159265358979323846264338327950288419716939937510582097494459230; return pi
tellErr: say;   say '*** error! ***';  say;  say arg(1);  say;  exit 13
tanErr:  call tellErr 'tan(' || x") causes division by zero, X="             ||  x
AsinErr: call tellErr 'Asin(x),  X  must be in the range of  -1 ──► +1,  X=' ||  x
AcosErr: call tellErr 'Acos(x),  X  must be in the range of  -1 ──► +1,  X=' ||  x
/*──────────────────────────────────────────────────────────────────────────────────────*/
Asin:    procedure; parse arg x;  if x<-1 | x>1  then call AsinErr;   s=x*x
             if abs(x)>=.7  then return sign(x)*Acos(sqrt(1-s));      z=x;  o=x;  p=z
                      do j=2 by 2; o=o*s*(j-1)/j; z=z+o/(j+1); if z=p then leave; p=z; end
             return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
cos:     procedure; parse arg x;   x=r2r(x);          a=abs(x);         Hpi=pi*.5
             numeric fuzz min(6,digits()-3);          if a=pi()    then return -1
             if a=Hpi | a=Hpi*3  then return  0 ;     if a=pi()/3  then return .5
             if a=pi()*2/3       then return -.5;                  return .sinCos(1,1,-1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
sin:     procedure; parse arg x;   x=r2r(x);          numeric fuzz min(5, digits()-3)
             if abs(x)=pi()  then return 0;                        return .sinCos(x,x,1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
.sinCos: parse arg z 1 p,_,i;  x=x*x
             do k=2  by 2; _=-_*x/(k*(k+i)); z=z+_; if z=p  then leave; p=z; end; return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
invoke:  parse arg fn,v;  q='"';  if datatype(v,"N")  then q=
             _=fn || '('q || v || q")";          interpret 'func='_;           return func
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt:    procedure; parse arg x;  if x=0  then return 0;  d=digits();  m.=9;  numeric form
         numeric digits; parse value format(x,2,1,,0) 'E0'  with  g 'E' _ .;  g=g*.5'e'_%2
         h=d+6;     do j=0  while h>9;      m.j=h;               h=h%2+1;        end /*j*/
                    do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;   end /*k*/
         numeric digits d;                  return g/1
/*──────────────────────────────────────────────────────────────────────────────────────*/
test:    procedure expose A B w;  parse arg fu,xList;     d=digits() /*xList:  numbers. */
                 do k=1  for words(xList);          x=word(xList, k)
                 numeric digits d+5                                  /*higher precision.*/
                 fun=word(A, fu);  funV=invoke(fun, x)   ;  fun@=_
                 inv=word(B, fu);  invV=invoke(inv, funV);  inv@=_
                 numeric digits d                                    /*restore precision*/
                 if datatype(funV, 'N')  then funV=funV/1            /*round to digits()*/
                 if datatype(invV, 'N')  then invV=invV/1            /*round to digits()*/
                 say center(x, w)   right(fun@, 2*w)'='left(left('', funV>=0)funV, w),
                                    right(inv@, 3*w)'='left(left('', invV>=0)invV, w)
                 end   /*k*/
         return
```

'''output''' 

```txt

 number             function                         inverse
───────── ──────────────────────────── ────────────────────────────────────
   20                d2x(20)= 14                           x2d(14)= 20
   60                d2x(60)= 3C                         x2d("3C")= 60
   500              d2x(500)= 1F4                       x2d("1F4")= 500


 number             function                         inverse
───────── ──────────────────────────── ────────────────────────────────────
    0              square(0)= 0                            sqrt(0)= 0
   0.5           square(0.5)= 0.25                      sqrt(0.25)= 0.5
    1              square(1)= 1                            sqrt(1)= 1
    2              square(2)= 4                            sqrt(4)= 2


 number             function                         inverse
───────── ──────────────────────────── ────────────────────────────────────
    0                 sin(0)= 0                            Asin(0)= 0
   0.5              sin(0.5)= 0.479425      Asin(0.47942553860419)= 0.5
    1                 sin(1)= 0.841470      Asin(0.84147098480862)= 1
    2                 sin(2)= 0.909297      Asin(0.90929742682567)= 1.141592


 number             function                         inverse
───────── ──────────────────────────── ────────────────────────────────────
    0                 cos(0)= 1                            Acos(1)= 0
   0.5              cos(0.5)= 0.877582      Acos(0.87758256188987)= 0.5
    1                 cos(1)= 0.540302      Acos(0.54030230586810)= 1
    2                 cos(2)=-0.416146     Acos(-0.41614683650659)= 2

```

The reason why   '''Asin[sin(n)]'''   may not equal   '''n''':

Each of the trigonometric functions is periodic in the real part of its argument, running through all its values twice in each interval of   <big>2<big><math>\pi</math></big></big>.

'''Sine''' and '''cosecant'''   begin their period at   <big>2<big><math>\pi</math></big>k − <big><math>\pi</math></big>/2</big>   (where   <big>k</big>   is an integer),   finish it at   <big>2<big><math>\pi</math></big>k + <big><math>\pi</math></big>/2</big>,   and then reverse themselves over   <big>2<big><math>\pi</math></big>k + <big><math>\pi</math></big>/2</big>   ───►   <big>2<big><math>\pi</math></big>k + 3<big><math>\pi</math></big>/2</big>. 

'''Cosine'''   and   '''secant'''   begin their period at   <big>2<big><math>\pi</math></big>k</big>,   finish it at   <big>2<big><math>\pi</math></big>k + <big><math>\pi</math></big></big>,   and then reverse themselves over   <big>2<big><math>\pi</math></big>k + <big><math>\pi</math></big></big>   ───►   <big>2<big><math>\pi</math></big>k + 2<big><math>\pi</math></big></big>. 

'''Tangent'''   begins its period at   <big>2<big><math>\pi</math></big>k − <big><math>\pi</math></big>/2</big>,     finishes it at   <big>2<big><math>\pi</math></big>k + <big><math>\pi</math></big>/2</big>,   and then repeats it (forward) over <big>2<big><math>\pi</math></big>k + <big><math>\pi</math></big>/2</big>   ───►   <big>2<big><math>\pi</math></big>k + 3<big><math>\pi</math></big>/2</big>. 

'''Cotangent'''   begins its period at   <big>2<big><math>\pi</math></big>k</big>,   finishes it at   <big>2<big><math>\pi</math></big>k + <big><math>\pi</math></big></big>,   and then repeats it (forward) over <big>2<big><math>\pi</math></big>k + <big><math>\pi</math></big></big>   ───►   <big>2<big><math>\pi</math></big>k + 2<big><math>\pi</math></big></big>.


The above text is from the Wikipedia webpage:   http://en.wikipedia.org/wiki/Inverse_trigonometric_functions





## Ruby


```ruby
cube = proc{|x| x ** 3}
croot = proc{|x| x ** (1.quo 3)}
compose = proc {|f,g| proc {|x| f[g[x]]}}
funclist = [Math.method(:sin), Math.method(:cos), cube]
invlist = [Math.method(:asin), Math.method(:acos), croot]

puts funclist.zip(invlist).map {|f, invf| compose[invf, f][0.5]}
```


{{out}}

```txt

0.5
0.4999999999999999
0.5

```



## Rust

This solution uses a feature of Nightly Rust that allows us to return a closure from a function without using the extra indirection of a pointer. Stable Rust can also accomplish this challenge --  the only difference being that compose would return a <code>Box<Fn(T) -> V></code> which would result in an extra heap allocation. 


```rust
#![feature(conservative_impl_trait)]
fn main() {
    let cube      = |x: f64| x.powi(3);
    let cube_root = |x: f64| x.powf(1.0 / 3.0);

    let flist  : [&Fn(f64) -> f64; 3] = [&cube     , &f64::sin , &f64::cos ];
    let invlist: [&Fn(f64) -> f64; 3] = [&cube_root, &f64::asin, &f64::acos];

    let result = flist.iter()
                      .zip(&invlist)
                      .map(|(f,i)| compose(f,i)(0.5))
                      .collect::<Vec<_>>();

    println!("{:?}", result);

}

fn compose<'a, F, G, T, U, V>(f: F, g: G) -> impl 'a + Fn(T) -> V
    where F: 'a + Fn(T) -> U,
          G: 'a + Fn(U) -> V,
{
    move |x| g(f(x))

}
```



## Scala


```scala
import math._

// functions as values
val cube = (x: Double) => x * x * x
val cuberoot = (x: Double) => pow(x, 1 / 3d)

// higher order function, as a method
def compose[A,B,C](f: B => C, g: A => B) = (x: A) => f(g(x))

// partially applied functions in Lists
val fun = List(sin _, cos _, cube)
val inv = List(asin _, acos _, cuberoot)

// composing functions from the above Lists
val comp = (fun, inv).zipped map (_ compose _)

// output results of applying the functions
comp foreach {f => print(f(0.5) + "   ")}
```

Output:

```txt
0.5   0.4999999999999999   0.5000000000000001
```

<!--
Here's how you could add a composition operator to make that syntax prettier:


```scala
class SweetFunction[B,C](f: B => C) {
  def o[A](g: A => B) = (x: A) => f(g(x))
}
implicit def sugarOnTop[A,B](f: A => B) = new SweetFunction(f)

// now functions can be composed thus
println((cube o cube o cuberoot)(0.5)) 
```
-->


## Scheme


```scheme
(define (compose f g) (lambda (x) (f (g x))))
(define (cube x) (expt x 3))
(define (cube-root x) (expt x (/ 1 3)))

(define function (list sin cos cube))
(define inverse (list asin acos cube-root))

(define x 0.5)
(define (go f g)
  (if (not (or (null? f)
               (null? g)))
      (begin (display ((compose (car f) (car g)) x))
             (newline)
             (go (cdr f) (cdr g)))))

(go function inverse)
```

Output:
 0.5
 0.5
 0.5


## Sidef

{{trans|Perl}}

```ruby
func compose(f,g) {
    func (*args) {
        f(g(args...))
    }
}

var cube  = func(a) { a.pow(3) }
var croot = func(a) { a.root(3) }

var flist1 = [Num.method(:sin),  Num.method(:cos),  cube]
var flist2 = [Num.method(:asin), Num.method(:acos), croot]

for a,b (flist1 ~Z flist2) {
    say compose(a, b)(0.5)
}
```

{{out}}

```txt

0.5
0.5
0.5

```



## Slate

{{incomplete|Slate|Fails to demonstrate that the result of applying the composition of each function in A and its inverse in B to a value, is the original value}}
Compose is already defined in slate as (note the examples in the comment):


```slate
m@(Method traits) ** n@(Method traits)
"Answers a new Method whose effect is that of calling the first method
on the results of the second method applied to whatever arguments are passed.
This composition is associative, i.e. (a ** b) ** c = a ** (b ** c).
When the second method, n, does not take a *rest option or the first takes
more than one input, then the output is chunked into groups for its
consumption. E.g.:
#; `er ** #; `er applyTo: {'a'. 'b'. 'c'. 'd'} => 'abcd'
#; `er ** #name `er applyTo: {#a. #/}. => 'a/'"
[
  n acceptsAdditionalArguments \/ [m arity = 1]
    ifTrue:
      [[| *args | m applyTo: {n applyTo: args}]]
    ifFalse:
      [[| *args |
        m applyTo:
          ([| :stream |
             args do: [| *each | stream nextPut: (n applyTo: each)]
                  inGroupsOf: n arity] writingAs: {})]]
].

#**`er asMethod: #compose: on: {Method traits. Method traits}.
```

used as:

```slate
n@(Number traits) cubed [n raisedTo: 3].
n@(Number traits) cubeRoot [n raisedTo: 1 / 3].
define: #forward -> {#cos `er. #sin `er. #cube `er}.
define: #reverse -> {#arcCos `er. #arcSin `er. #cubeRoot `er}.

define: #composedMethods -> (forward with: reverse collect: #compose: `er).
composedMethods do: [| :m | inform: (m applyWith: 0.5)].
```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
|forward reverse composer compounds|
"commodities"
Number extend [
   cube [ ^self raisedTo: 3 ]
].
Number extend [
   cubeRoot [ ^self raisedTo: (1 / 3) ]
].

forward := #( #cos #sin #cube ).
reverse := #( #arcCos #arcSin #cubeRoot ).

composer := [ :f :g | [ :x | f value: (g value: x) ] ].

"let us create composed funcs"
compounds := OrderedCollection new.

1 to: 3 do: [ :i |
  compounds add: ([ :j | composer value: [ :x | x perform: (forward at: j) ]
                                  value: [ :x | x perform: (reverse at: j) ] ] value: i)
].

compounds do: [ :r | (r value: 0.5) displayNl ].
```


Output:


```txt
0.4999999999999999
0.5
0.5000000000000001
```



## Standard ML


```sml
- fun cube x = Math.pow(x, 3.0);
val cube = fn : real -> real
- fun croot x = Math.pow(x, 1.0 / 3.0);
val croot = fn : real -> real
- fun compose (f, g) = fn x => f (g x); (* this is already implemented in Standard ML as the "o" operator
=                                          we could have written "fun compose (f, g) x = f (g x)" but we show this for clarity *)
val compose = fn : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
- val funclist = [Math.sin, Math.cos, cube];
val funclist = [fn,fn,fn] : (real -> real) list
- val funclisti = [Math.asin, Math.acos, croot];
val funclisti = [fn,fn,fn] : (real -> real) list
- ListPair.map (fn (f, inversef) => (compose (inversef, f)) 0.5) (funclist, funclisti);
val it = [0.5,0.5,0.500000000001] : real list
```



## Stata


In Mata it's not possible to get the address of a builtin function, so here we define user functions.


```stata
function _sin(x) {
	return(sin(x))
}

function _asin(x) {
	return(asin(x))
}

function _cos(x) {
	return(cos(x))
}

function _acos(x) {
	return(acos(x))
}

function cube(x) {
	return(x*x*x)
}

function cuberoot(x) {
	return(sign(x)*abs(x)^(1/3))
}

function compose(f,g,x) {
	return((*f)((*g)(x)))
}

a=&_sin(),&_cos(),&cube()
b=&_asin(),&_acos(),&cuberoot()

for(i=1;i<=length(a);i++) {
	printf("%10.5f\n",compose(a[i],b[i],0.5))
}
```



## SuperCollider


```SuperCollider

a = [sin(_), cos(_), { |x| x ** 3 }];
b = [asin(_), acos(_), { |x| x ** (1/3) }];
c = a.collect { |x, i| x <> b[i] };
c.every { |x| x.(0.5) - 0.5 < 0.00001 }

```



## Swift

{{works with|Swift|1.2+}}

```swift
import Darwin
func compose<A,B,C>(f: (B) -> C, g: (A) -> B) -> (A) -> C {
  return { f(g($0)) }
}
let funclist = [ { (x: Double) in sin(x) }, { (x: Double) in cos(x) }, { (x: Double) in pow(x, 3) } ]
let funclisti = [ { (x: Double) in asin(x) }, { (x: Double) in acos(x) }, { (x: Double) in cbrt(x) } ]
println(map(zip(funclist, funclisti)) { f, inversef in compose(f, inversef)(0.5) })
```

{{out}}

```txt

[0.5, 0.5, 0.5]

```



## Tcl

The following is a transcript of an interactive session:

{{works with|tclsh|8.5}}

```Tcl
% namespace path tcl::mathfunc ;# to import functions like abs() etc.
% proc cube x {expr {$x**3}}
% proc croot x {expr {$x**(1/3.)}}
% proc compose {f g} {list apply {{f g x} {{*}$f [{*}$g $x]}} $f $g}

% compose abs cube          ;# returns a partial command, without argument
apply {{f g x} {{*}$f [{*}$g $x]}} abs cube

% {*}[compose abs cube] -3  ;# applies the partial command to argument -3
27

% set forward [compose [compose sin cos] cube] ;# omitting to print result
% set backward [compose croot [compose acos asin]]
% {*}$forward 0.5
0.8372297964617733
% {*}$backward [{*}$forward 0.5]
0.5000000000000017
```

Obviously, the ([[C]]) library implementation of some of the trigonometric functions (on which Tcl depends for its implementation) on the platform used for testing is losing a little bit of accuracy somewhere.

=={{header|TI-89 BASIC}}==

See the comments at [[Function as an Argument#TI-89 BASIC]] for more information on first-class functions or the lack thereof in TI-89 BASIC. In particular, it is not possible to do proper function composition, because functions cannot be passed as values nor be closures.

Therefore, this example does everything but the composition.

(Note: The names of the inverse functions may not display as intended unless you have the “TI Uni” font.)


```ti89b
Prgm
  Local funs,invs,composed,x,i

  Define rc_cube(x) = x^3     © Cannot be local variables
  Define rc_curt(x) = x^(1/3)

  Define funs = {"sin","cos","rc_cube"}
  Define invs = {"sin","cos","rc_curt"}

  Define x = 0.5
  Disp "x = " & string(x)
  For i,1,3
    Disp "f=" & invs[i] & " g=" & funs[i] & " f(g(x))=" & string(#(invs[i])(#(funs[i])(x)))
  EndFor

  DelVar rc_cube,rc_curt  © Clean up our globals
EndPrgm
```



## TXR


{{trans|Racket}}

Translation notes: we use <code>op</code> to create cube and inverse cube anonymously and succinctly.
<code>chain</code> composes a variable number of functions, but unlike <code>compose</code>, from left to right, not right to left.


```txrlisp
(defvar funlist [list sin
                      cos
                      (op expt @1 3)])

(defvar invlist [list asin
                      acos
                      (op expt @1 (/ 1 3))])

(each ((f funlist) (i invlist))
  (prinl [(chain f i) 0.5]))
```


{{out}}


```txt
0.5
0.5
0.5
0.5
```



## Ursala

The algorithm is to zip two lists of functions into a list of pairs of functions, make
that a list of functions by composing each pair, "<code>gang</code>" the list of
functions into a single function returning a list, and apply it to the
argument 0.5.

```Ursala
#import std
#import flo

functions = <sin,cos,times^/~& sqr>
inverses  = <asin,acos,math..cbrt>

#cast %eL

main = (gang (+)*p\functions inverses) 0.5
```

In more detail,
* <code>(+)*p\functions inverses</code> evaluates to <code>(+)*p(inverses,functions)</code> by definition of the reverse binary to unary combinator (<code>\</code>)
* This expression evaluates to <code>(+)*p(<asin,acos,math..cbrt>,<sin,cos,times^/~& sqr>)</code> by substitution.
* The zipping is indicated by the <code>p</code> suffix on the map operator, (<code>*</code>) so that <code>(+)*p</code> evaluates to <code>(+)* <(asin,sin),(acos,cos),(cbrt,times^/~& sqr)></code>.
* The composition (<code>(+)</code>) operator is then mapped over the resulting list of pairs of functions, to obtain the list of functions <code><asin+sin,acos+cos,cbrt+ times^/~& sqr></code>.
* <code>gang<aisn+sin,acos+cos,cbrt+ times^/~& sqr></code> expresses a function returning a list in terms of a list of functions.

output:

```txt
<5.000000e-01,5.000000e-01,5.000000e-01>
```



## zkl

In zkl, methods bind their instance so something like x.sin is the sine method bound to x (whatever real number x is). eg var a=(30.0).toRad().sin; is a method and a() will always return 0.5 (ie basically a const in this case). Which means you can't just use the word "sin", it has to be used in conjunction with an instance.

```zkl
var a=T(fcn(x){ x.toRad().sin() },  fcn(x){ x.toRad().cos() },  fcn(x){ x*x*x} );
var b=T(fcn(x){ x.asin().toDeg() }, fcn(x){ x.acos().toDeg() }, fcn(x){ x.pow(1.0/3) });

var H=Utils.Helpers;
var ab=b.zipWith(H.fcomp,a);  //-->list of deferred calculations
ab.run(True,5.0); //-->L(5.0,5.0,5.0)

a.run(True,5.0) //-->L(0.0871557,0.996195,125)
```

fcomp is the function composition function, fcomp(b,a) returns the function (x)-->b(a(x)). List.run(True,x) is inverse of List.apply/map, it returns a list of list[i](x). The True is to return the result, False is just do it for the side effects.

{{Omit From|AWK}}
{{omit from|gnuplot}}
{{omit from|LaTeX}}
{{omit from|Make}}
{{omit from|PlainTeX}}
{{omit from|PureBasic}}
{{omit from|TI-83 BASIC|Cannot define functions.}}

[[Category:Functions and subroutines]]
