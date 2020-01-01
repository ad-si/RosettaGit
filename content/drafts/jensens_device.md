+++
title = "Jensen's Device"
description = ""
date = 2019-04-15T19:46:30Z
aliases = []
[extra]
id = 3139
[taxonomies]
categories = []
tags = []
+++

{{wikipedia}}
{{task|Classic CS problems and programs}}
This task is an exercise in [[wp:Call-by-name#Call_by_name|call by name]].

'''Jensen's Device''' is a computer programming technique devised by Danish computer scientist [[wp:Jørn_Jensen|Jørn Jensen]] after studying the [[ALGOL 60]] Report.

The following program was proposed to illustrate the technique. It computes the 100th [[wp:Harmonic_number|harmonic number]]:

 '''begin'''
    '''integer''' i;
    '''real procedure''' sum (i, lo, hi, term);
       '''value''' lo, hi;
       '''integer''' i, lo, hi;
       '''real''' term;
       '''comment''' term is passed by-name, and so is i;
    '''begin'''
       '''real''' temp;
       temp := 0;
       '''for''' i := lo '''step''' 1 '''until''' hi '''do'''
          temp := temp + term;
       sum := temp
    '''end''';
    '''comment''' note the correspondence between the mathematical notation and the call to sum;
    print (sum (i, 1, 100, 1/i))
 '''end'''

The above exploits [[wp:Call-by-name#Call_by_name|call by name]] to produce the correct answer (5.187...). It depends on the assumption that an expression passed as an actual parameter to a procedure would be re-evaluated in the caller's context every time the corresponding formal parameter's value was required. If the last parameter to ''sum'' had been passed by value, and assuming the initial value of ''i'' were 1, the result would have been 100 &times; 1/1 = 100.

Moreover, the ''first'' parameter to ''sum'', representing the "bound" variable of the summation, must also be passed by name (or at least by reference), otherwise changes to it (made within ''sum'') would not be visible in the caller's context when computing each of the values to be added.
(On the other hand, the global variable does not have to use the same identifier, in this case ''i'', as the formal parameter.)

[[wp:Donald_Knuth|Donald Knuth]] later proposed the [[Man or boy test|Man or Boy Test]] as a more rigorous exercise.




## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Jensen_Device is
   function Sum
            (  I : not null access Float;
               Lo, Hi : Float;
               F : access function return Float
            )  return Float is
      Temp : Float := 0.0;
   begin
      I.all := Lo;
      while I.all <= Hi loop
         Temp := Temp + F.all;
         I.all := I.all + 1.0;
      end loop;
      return Temp;
   end Sum;

   I : aliased Float;
   function Inv_I return Float is
   begin
      return 1.0 / I;
   end Inv_I;
begin
   Put_Line (Float'Image (Sum (I'Access, 1.0, 100.0, Inv_I'Access)));
end Jensen_Device;
```


```txt

 5.18738E+00

```


## ALGOL 60

Honor given where honor is due. In Algol 60, 'call by name' is the default argument evaluation.
 '''begin'''
    '''integer''' i;
    '''real procedure''' sum (i, lo, hi, term);
       '''value''' lo, hi;
       '''integer''' i, lo, hi;
       '''real''' term;
       '''comment''' term is passed by-name, and so is i;
    '''begin'''
       '''real''' temp;
       temp := 0;
       '''for''' i := lo '''step''' 1 '''until''' hi '''do'''
          temp := temp + term;
       sum := temp
    '''end''';
    '''comment''' note the correspondence between the mathematical notation and the call to sum;
    print (sum (i, 1, 100, 1/i))
 '''end'''


## ALGOL 68

{{trans|ALGOL 60}}

```algol68
BEGIN
   INT i;
   PROC sum  = (REF INT i, INT lo, hi, PROC REAL term)REAL:
      COMMENT term is passed by-name, and so is i COMMENT
   BEGIN
      REAL temp := 0;
      i := lo;
      WHILE i <= hi DO           # ALGOL 68 has a "for" loop but it creates a distinct #
         temp +:= term;          # variable which would not be shared with the passed "i" #
         i +:= 1                 # Here the actual passed "i" is incremented. #
      OD;
      temp
   END;
   COMMENT note the correspondence between the mathematical notation and the call to sum COMMENT
   print (sum (i, 1, 100, REAL: 1/i))
END
```

Output: +5.18737751763962e  +0


## AppleScript


```AppleScript
set i to 0

on jsum(i, lo, hi, term)
	set {temp, i's contents} to {0, lo}
	repeat while i's contents ≤ hi
		set {temp, i's contents} to {temp + (term's f(i)), (i's contents) + 1}
	end repeat
	return temp
end jsum

script term_func
	on f(i)
		return 1 / i
	end f
end script

return jsum(a reference to i, 1, 100, term_func)
```

Output: 5.18737751764


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program jensen.s   */
/* compil as with option  -mcpu=<processor> -mfpu=vfpv4 -mfloat-abi=hard  */
/* link with gcc          */

/* Constantes    */
.equ EXIT,   1                           @ Linux syscall
/* Initialized data */
.data

szFormat: .asciz "Result = %.8f \n"
.align 4

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:
    mov r0,#1                                   @ first indice
    mov r1,#100                                 @ last indice
    adr r2,funcdiv                              @ address function
    bl funcSum
    vcvt.f64.f32  d1, s0                        @ conversion double float for print by C
    ldr r0,iAdrszFormat                         @ display format
    vmov r2,r3,d1                               @ parameter function printf for float double
    bl printf                                   @ display float double

100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc 0                                       @ perform system call

iAdrszFormat:             .int szFormat
/******************************************************************/
/*     function sum                                               */
/******************************************************************/
/* r0 contains begin  */
/* r1 contains end */
/* r2 contains address function */

/* r0 return result                      */
funcSum:
    push {r0,r3,lr}                       @ save  registers
    mov r3,r0
    mov r0,#0                             @ init r0
    vmov s3,r0                            @ and s3
    vcvt.f32.s32 s3, s3                   @ convert in float single précision (32bits)
1:                                        @ begin loop
    mov r0,r3                             @ loop indice -> parameter function
    blx r2                                @ call function address in r2
    vadd.f32 s3,s0                        @ addition float
    add r3,#1                             @ increment indice
    cmp r3,r1                             @ end ?
    ble 1b                                @ no loop
    vmov s0,s3                            @ return float result in s0

100:
    pop {r0,r3,lr}                        @ restaur registers
    bx lr                                 @ return
/******************************************************************/
/*     compute 1/r0                                               */
/******************************************************************/
/* r0 contains the value                 */
/* r0 return result                      */
funcdiv:
    push {r1,lr}                       @ save  registers
    vpush {s1}                         @ save float registers
    cmp r0,#0                          @ division by zero -> end
    beq 100f
    ldr r1,fUn                         @ load float constant 1.0
    vmov s0,r1                         @ in float register s3
    vmov s1,r0                         @
    vcvt.f32.s32 s1, s1                @conversion in float single précision (32 bits)
    vdiv.f32 s0,s0,s1                  @ division 1/r0
                                       @ and return result in s0
100:
    vpop {s1}                          @ restaur float registers
    pop {r1,lr}                        @ restaur registers
    bx lr                              @ return
fUn:                .float 1


```




## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      PRINT FNsum(j, 1, 100, FNreciprocal)
      END

      DEF FNsum(RETURN i, lo, hi, RETURN func)
      LOCAL temp
      FOR i = lo TO hi
        temp += FN(^func)
      NEXT
      = temp

      DEF FNreciprocal = 1/i
```

Output:

```txt

5.18737752

```



## Bracmat


```bracmat
( ( sum
  =   I lo hi Term temp
    .   !arg:((=?I),?lo,?hi,(=?Term))
      & 0:?temp
      & !lo:?!I
      &   whl
        ' ( !!I:~>!hi
          & !temp+!Term:?temp
          & 1+!!I:?!I
          )
      & !temp
  )
& sum$((=i),1,100,(=!i^-1))
);
```

Output:

```txt
14466636279520351160221518043104131447711/2788815009188499086581352357412492142272
```



## C


```c
#include <stdio.h>

int i;
double sum(int *i, int lo, int hi, double (*term)()) {
    double temp = 0;
    for (*i = lo; *i <= hi; (*i)++)
        temp += term();
    return temp;
}

double term_func() { return 1.0 / i; }

int main () {
    printf("%f\n", sum(&i, 1, 100, term_func));
    return 0;
}
```

Output: 5.18738

{{works with|gcc}}
Alternatively, C's macros provide a closer imitation of ALGOL's call-by-name semantics:

```c
#include <stdio.h>

int i;

#define sum(i, lo_byname, hi_byname, term)      \
  ({                                            \
  int lo = lo_byname;                           \
  int hi = hi_byname;                           \
                                                \
  double temp = 0;                              \
  for (i = lo; i <= hi; ++i)                    \
    temp += term;                               \
  temp;                                         \
  })

int main () {
    printf("%f\n", sum(i, 1, 100, 1.0 / i));
    return 0;
}
```

Output: 5.187378


## C++


```cpp
#include <iostream>

int i;
double sum(int &i, int lo, int hi, double (*term)()) {
    double temp = 0;
    for (i = lo; i <= hi; i++)
        temp += term();
    return temp;
}

double term_func() { return 1.0 / i; }

int main () {
    std::cout << sum(i, 1, 100, term_func) << std::endl;
    return 0;
}
```

Output: 5.18738


## C#

Can be simulated via lambda expressions:

```c#
using System;

class JensensDevice
{
    public static double Sum(ref int i, int lo, int hi, Func<double> term)
    {
        double temp = 0.0;
        for (i = lo; i <= hi; i++)
        {
            temp += term();
        }
        return temp;
    }

    static void Main()
    {
        int i = 0;
        Console.WriteLine(Sum(ref i, 1, 100, () => 1.0 / i));
    }
}
```



## Clipper

With hindsight Algol60 provided this feature in a way that is terrible for program maintenance, because the calling code looks innocuous.

```Clipper
// Jensen's device in Clipper (or Harbour)
//    A fairly direct translation of the Algol 60
// John M Skelton 11-Feb-2012

function main()
local i
? transform(sum(@i, 1, 100, {|| 1 / i}), "##.###############")
   // @ is the quite rarely used pass by ref, {|| ...} is a
   // code block (an anonymous function, here without arguments)
   // The @i makes it clear that something unusual is occurring;
   // a called function which modifies a parameter is commonly
   // poor design!
return 0

function sum(i, lo, hi, bFunc)
local temp := 0
for i = lo to hi
   temp += eval(bFunc)
next i
return temp

```



## Common Lisp


Common Lisp does not have call-by-name for functions; however, it can be directly simulated by a macro wrapping selected parameters in lambdas.


```lisp
(declaim (inline %sum))

(defun %sum (lo hi func)
  (loop for i from lo to hi sum (funcall func i)))

(defmacro sum (i lo hi term)
  `(%sum ,lo ,hi (lambda (,i) ,term)))
```



```lisp
CL-USER> (sum i 1 100 (/ 1 i))
14466636279520351160221518043104131447711/2788815009188499086581352357412492142272
CL-USER> (float (sum i 1 100 (/ 1 i)))
5.1873775
```



## D

There are better ways to do this in D, but this is closer to the original Algol version:

```d
double sum(ref int i, in int lo, in int hi, lazy double term)
pure @safe /*nothrow @nogc*/ {
    double result = 0.0;
    for (i = lo; i <= hi; i++)
        result += term();
    return result;
}

void main() {
    import std.stdio;

    int i;
    sum(i, 1, 100, 1.0/i).writeln;
}
```

{{out}}

```txt

5.18738
```



## DWScript

Must use a "while" loop, as "for" loop variables are restricted to local variable for code clarity, and this indeed a case where any kind of extra clarity helps.

```delphi
function sum(var i : Integer; lo, hi : Integer; lazy term : Float) : Float;
begin
   i:=lo;
   while i<=hi do begin
      Result += term;
      Inc(i);
   end;
end;

var i : Integer;

PrintLn(sum(i, 1, 100, 1.0/i));
```

Output: 5.187...


## E


In E, the distinct mutable locations behind assignable variables can be reified as [http://wiki.erights.org/wiki/Slot Slot] objects. The E language allows a variable name (''noun'') to be bound to a particular slot, and the slot of an already-bound noun to be extracted, using the <tt>&</tt> operator.

(The definition of the outer <var>i</var> has been moved down to emphasize that it is unrelated to the <var>i</var> inside of <var>sum</var>.)


```e
pragma.enable("one-method-object") # "def _.get" is experimental shorthand
def sum(&i, lo, hi, &term) {   # bind i and term to passed slots
    var temp := 0
    i := lo
    while (i <= hi) {          # E has numeric-range iteration but it creates a distinct
        temp += term           # variable which would not be shared with the passed i
        i += 1
    }
    return temp
}
{
    var i := null
    sum(&i, 1, 100, def _.get() { return 1/i })
}
```


<tt>1/i</tt> is not a noun, so there is no slot associated with it; so we use <tt>def _.get() { return 1/i }</tt> to define a slot object which does the computation when it is read as a slot.

The value returned by the above program (expression) is 5.187377517639621.

This emulation of the original call-by-name is of course unidiomatic; a natural version of the same computation would be:


```e
def sum(lo, hi, f) {
    var temp := 0
    for i in lo..hi { temp += f(i) }
    return temp
}
sum(1, 100, fn i { 1/i })
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule JensenDevice do
  def task, do: sum( 1, 100, fn i -> 1 / i end )

  defp sum( i, high, _term ) when i > high, do: 0
  defp sum( i, high, term ) do
    temp = term.( i )
    temp + sum( i + 1, high, term )
  end
end

IO.puts JensenDevice.task
```


{{out}}

```txt

5.1873775176396215

```



## Erlang

No call by name, no macros, so I use a fun(ction). Actually, the the macro part is a lie. Somebody else, that knows how, could do a parse transform.


```Erlang

-module( jensens_device ).

-export( [task/0] ).

task() ->
    sum( 1, 100, fun (I) -> 1 / I end ).

sum( I, High, _Term ) when I > High -> 0;
sum( I, High, Term ) ->
    Temp = Term( I ),
    Temp + sum( I + 1, High, Term ).

```


{{out}}

```txt

4> jensens_device:task().
5.1873775176396215

```



## Factor

Similar to the Java and Kotlin examples:

```factor
: sum ( lo hi term -- x ) [ [a,b] ] dip map-sum ; inline

1 100 [ recip ] sum .
```


This version is a bit closer to the original, as it increments <code>i</code> in the caller's namespace.

```factor
SYMBOL: i

: sum ( i lo hi term -- x )
    [ [a,b] ] dip pick [ inc ] curry compose map-sum nip ;
    inline

i 1 100 [ recip ] sum .
```

{{out}}

```txt

5+522561233577855727314756256041670736351/2788815009188499086581352357412492142272

```



## Forth

This version passes i on the stack:


```forth>: sum 0 s
f 1+ swap ?do i over execute f+ loop drop ;
:noname s>f 1 s>f fswap f/ ; 1 100 sum f.
```

Output: 5.18737751763962

The following version passes i and 1/i as execution tokens and is thus closer to the original, but less idiomatic:


```forth
fvariable ii \ i is a Forth word that we need
: sum ( xt1 lo hi xt2 -- r )
  0e swap 1+ rot ?do ( addr xt r1 )
    i s>f over execute f! dup execute f+
  loop 2drop ;
' ii 1 100 :noname 1e ii f@ f/ ; sum f.
```



## Fortran

Fortran does not offer call-by-name in the manner of the Algol language. It passes parameters by reference (i.e. by passing the storage address) and alternatively uses copy-in, copy-out to give the same effect, approximately, as by reference. If a parameter is an arithmetic expression, it will be evaluated and its value stored in a temporary storage area, whose address will be passed to the routine. This evaluation is done once only for each call, thus vitiating the repeated re-evaluation required by Jensen's device every time within the routine that the parameter is accessed. So, this will ''not'' work
```Fortran
      FUNCTION SUM(I,LO,HI,TERM)
        SUM = 0
        DO I = LO,HI
          SUM = SUM + TERM
        END DO
      END FUNCTION SUM
      WRITE (6,*) SUM(I,1,100,1.0/I)
      END
```

Here, type declarations have been omitted to save space because they won't help - until there appears a "BY NAME" or some such phrasing. Although variable <code>I</code> in the calling routine will have its value adjusted as the DO-loop in SUM proceeds (the parameter being passed by reference), this won't affect the evaluation of 1.0/I, which will be performed once using whatever value is in the caller's variable (it is uninitialised, indeed, undeclared also and so by default an integer) then the function is invoked with the address of the location containing that result. The function will make many references to that result, obtaining the same value each time. The fact that the caller's <code>I</code> will be changed each time doesn't matter.

Fortran does offer a facility to pass a function as a parameter using the EXTERNAL declaration, as follows - SUM is a F90 library function, so a name change to SUMJ:
```Fortran
      FUNCTION SUMJ(I,LO,HI,TERM)	!Attempt to follow Jensen's Device...
       INTEGER I	!Being by reference is workable.
       INTEGER LO,HI	!Just as any other parameters.
       EXTERNAL TERM	!Thus, not a variable, but a function.
        SUMJ = 0
        DO I = LO,HI	!The specified span.
          SUMJ = SUMJ + TERM(I)	!Number and type of parameters now apparent.
        END DO		!TERM will be evaluated afresh, each time.
      END FUNCTION SUMJ	!So, almost there.

      FUNCTION THIS(I)	!A function of an integer.
       INTEGER I
        THIS = 1.0/I	!Convert to floating-point.
      END		!Since 1/i will mostly give zero.

      PROGRAM JENSEN	!Aspiration.
      EXTERNAL THIS	!Thus, not a variable, but a function.
      INTEGER I		!But this is a variable, not a function.

      WRITE (6,*) SUMJ(I,1,100,THIS)	!No statement as to the parameters of THIS.
      END
```

The result of this is 5.187378, however it does not follow the formalism of Jensen's Device. The invocation statement SUMJ(I,1,100,THIS) does not contain the form of the function but only its name, and the function itself is defined separately. This means that the convenience of different functions via the likes of SUM(I,1,100,1.0/I**2) is unavailable, a separately-defined function with its own name must be defined for each such function. Further, the SUM routine must invoke TERM(I) itself, explicitly supplying the appropriate parameter. And the fact that variable <code>I</code> is a parameter to SUM is an irrelevance, and might as well be omitted from SUMJ.

Incidentally, a subroutine such as TEST(A,B) invoked as TEST(X,X) enables the discovery of copy-in, copy-out parameter passing. Within the routine, modify the value of A and look to see if B suddenly has a new value also.


## Go


```go
package main

import "fmt"

var i int

func sum(i *int, lo, hi int, term func() float64) float64 {
    temp := 0.0
    for *i = lo; *i <= hi; (*i)++ {
        temp += term()
    }
    return temp
}

func main() {
    fmt.Printf("%f\n", sum(&i, 1, 100, func() float64 { return 1.0 / float64(i) }))
}
```


{{out}}

```txt

5.187378

```



## Groovy

{{trans|JavaScript}}
Solution:

```groovy
def sum = { i, lo, hi, term ->
    (lo..hi).sum { i.value = it; term() }
}
def obj = [:]
println (sum(obj, 1, 100, { 1 / obj.value }))
```


Output:

```txt
5.1873775176
```



## Haskell


```haskell
import Control.Monad
import Control.Monad.ST
import Data.STRef

sum' ref_i lo hi term =
  return sum `ap`
         mapM (\i -> writeSTRef ref_i i >> term) [lo..hi]

foo = runST $ do
        i <- newSTRef undefined -- initial value doesn't matter
        sum' i 1 100 $ return recip `ap` readSTRef i

main = print foo
```

Output: 5.187377517639621


## Huginn


```huginn
harmonic_sum( i, lo, hi, term ) {
        temp = 0.0;
        i *= 0.0;
        i += lo;
        while ( i <= hi ) {
                temp += term();
                i += 1.0;
        }
        return ( temp );
}

main() {
        i = 0.0;
        print( "{}\n".format( harmonic_sum( i, 1.0, 100.0, @[i](){ 1.0 / i; } ) ) );
}
```

{{Output}}
```txt
5.18737751764
```


=={{header|Icon}} and {{header|Unicon}}==
Traditional call by name and reference are not features of Icon/Unicon. Procedures parameters are passed by value (immutable types) and reference (mutable types).  However, a similar effect may be accomplished by means of co-expressions.  The example below was selected for cleanliness of calling.


```Icon
record mutable(value)   # record wrapper to provide mutable access to immutable types

procedure main()
    A := mutable()
    write( sum(A, 1, 100, create 1.0/A.value) )
end

procedure sum(A, lo, hi, term)
    temp := 0
    every A.value := lo to hi do
        temp +:= @^term
    return temp
end
```


Refreshing the co-expression above is more expensive to process but to avoid it requires unary alternation in the call.

```Icon
    write( sum(A, 1, 100, create |1.0/A.value) )
...
        temp +:= @term
```


Alternately, we can use a programmer defined control operator (PDCO) approach that passes every argument as a co-expression.  Again the refresh co-expression/unary iteration trade-off can be made.  The call is cleaner looking but the procedure code is less clear.  Additionally all the parameters are passed as individual co-expressions.

```Icon
    write( sum{A.value, 1, 100, 1.0/A.value} )
...
procedure sum(X)
...
    every @X[1] := @X[2] to @X[3] do
        temp +:= @^X[4]
```



## J

'''Solution:'''

```j
jensen=: monad define
  'name lo hi expression'=. y
  temp=. 0
  for_n. lo+i.1+hi-lo do.
    (name)=. n
    temp=. temp + ".expression
  end.
)
```

'''Example:'''

```j
   jensen 'i';1;100;'1%i'
 5.18738
```


Note, however, that in J it is reasonably likely that the expression (or an obvious variation on the expression) can deal with the looping itself.  And in typical use this often simplifies to entering the expression and data directly on the command line.

And another obvious variation here would be turning the expression into a named entity (if it has some lasting usefulness).


## Java

This is Java 8.


```java
import java.util.function.*;
import java.util.stream.*;

public class Jensen {
    static double sum(int lo, int hi, IntToDoubleFunction f) {
        return IntStream.rangeClosed(lo, hi).mapToDouble(f).sum();
    }

    public static void main(String args[]) {
        System.out.println(sum(1, 100, (i -> 1.0/i)));
    }
}

```

The program prints '5.187377517639621'.

Java 7 is more verbose, but under the hood does essentially the same thing:


```java
public class Jensen2 {

    interface IntToDoubleFunction {
        double apply(int n);
    }

    static double sum(int lo, int hi, IntToDoubleFunction f) {
        double res = 0;
        for (int i = lo; i <= hi; i++)
            res += f.apply(i);
        return res;

    }
    public static void main(String args[]) {
        System.out.println(
            sum(1, 100,
                new IntToDoubleFunction() {
                    public double apply(int i) { return 1.0/i;}
                }));
    }
}

```



## JavaScript

{{trans|C}}

Uses an object ''o'' instead of integer pointer ''i'', as the C example does.


```javascript
var obj;

function sum(o, lo, hi, term) {
  var tmp = 0;
  for (o.val = lo; o.val <= hi; o.val++)
    tmp += term();
  return tmp;
}

obj = {val: 0};
alert(sum(obj, 1, 100, function() {return 1 / obj.val}));
```

The alert shows us '5.187377517639621'.


## Joy


```Joy
100 [0] [[1.0 swap /] dip +] primrec.
```

Joy does not have named parameters.
Neither i nor 1/i are visible in the program.

## jq

The technique used in the Javascript example can also be used in jq, but in jq it is more idiomatic to use "." to refer to the current term. For example, using sum/3 defined below, we can write: sum(1; 100; 1/.) to perform the task.

```jq
def sum(lo; hi; term):
  reduce range(lo; hi+1) as $i (0; . + ($i|term));

# The task:
sum(1;100;1/.)
```

{{Out}}
 $ jq -n -f jensen.jq
 5.187377517639621


## Julia

{{works with|Julia|0.6}}
{{trans|C}}


```julia
macro sum(i, loname, hiname, term)
    return quote
        lo = $loname
        hi = $hiname
        tmp = 0.0
        for i in lo:hi
            tmp += $term
        end
        return tmp
    end
end

i = 0
@sum(i, 1, 100, 1.0 / i)
```



## Kotlin


```scala
fun sum(lo: Int, hi: Int, f: (Int) -> Double) = (lo..hi).sumByDouble(f)

fun main(args: Array<String>) = println(sum(1, 100, { 1.0 / it }))
```



## Lua



```Lua

function sum(var, a, b, str)
  local ret = 0
  for i = a, b do
    ret = ret + setfenv(loadstring("return "..str), {[var] = i})()
  end
  return ret
end
print(sum("i", 1, 100, "1/i"))

```



## M2000 Interpreter

The definition of the lazy function has two statements. First statement is a Module with one argument, the actual name of Jensen`s_Device, which make the function to get the same scope as module Jensen`s_Device, and the second statement is =1/i which return the expression.


```M2000 Interpreter

Module Jensen`s_Device {
      Def double i
      Report Lazy$(1/i)  ' display the definition of the lazy function
      Function Sum (&i, lo, hi, &f()) {
            def double temp
            For i= lo to hi {
                  temp+=f()
            }
            =temp
      }
      Print Sum(&i, 1, 100, Lazy$(1/i))==5.1873775176392  ' true
      Print i=101 ' true
}
Jensen`s_Device

```


Using Decimal for better accuracy. change &i to &any to show that: when any change, change i, so f() use this i.

```M2000 Interpreter

Module Jensen`s_Device {
      Def decimal i
      Function Sum (&any, lo, hi, &f()) {
            def decimal temp
            For any= lo to hi {
                  temp+=f()
            }
            =temp
      }
      Print Sum(&i, 1, 100, Lazy$(1/i))=5.1873775176396202608051176755@  ' true
      Print i=101 ' true
}
Jensen`s_Device

```


Many other examples use single float. So this is one for single.

```M2000 Interpreter

Module Jensen`s_Device {
      Def single i
      Function Sum (&any, lo, hi, &f()) {
            def single temp
            For any= lo to hi {
                  temp+=f()
            }
            =temp
      }
      Print Sum(&i, 1, 100, Lazy$(1/i))=5.187378~  ' true
      Print i=101 ' true
}
Jensen`s_Device

```



## M4


```M4
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')
define(`sum',
   `pushdef(`temp',0)`'for(`$1',$2,$3,
      `define(`temp',eval(temp+$4))')`'temp`'popdef(`temp')')
sum(`i',1,100,`1000/i')
```


Output:

```txt

5142

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==


```Mathematica
sum[term_, i_, lo_, hi_] := Block[{temp = 0},
   				Do[temp = temp + term, {i, lo, hi}];
   				temp];
SetAttributes[sum, HoldFirst];
```


Output:

```txt
In[2]:= sum[1/i, i, 1, 100]
Out[2]= 14466636279520351160221518043104131447711/2788815009188499086581352357412492142272

In[3]:=N[sum[1/i, i, 1, 100]]
Out[3]:=5.18738

```



## Maxima


```maxima
mysum(e, v, lo, hi) := block([s: 0], for i from lo thru hi do s: s + subst(v=i, e), s)$

mysum(1/n, n, 1, 10);
7381/2520

/* compare with builtin sum */
sum(1/n, n, 1, 10);
7381/2520

/* what if n is assigned a value ? */
n: 200$

/* still works */
mysum(1/n, n, 1, 10);
7381/2520
```



## NetRexx


```netrexx

import COM.ibm.netrexx.process.

class JensensDevice

  properties static
  interpreter=NetRexxA
  exp=Rexx ""
  termMethod=Method

  method main(x=String[]) static
    say sum('i',1,100,'1/i')

  method sum(i,lo,hi,term) static SIGNALS IOException,NoSuchMethodException,IllegalAccessException,InvocationTargetException
    sum=0
    loop iv=lo to hi
      sum=sum+termeval(i,iv,term)
    end
    return sum

  method termeval(i,iv,e) static returns Rexx SIGNALS IOException,NoSuchMethodException,IllegalAccessException,InvocationTargetException
    if e\=exp then interpreter=null
    exp=e

    if interpreter=null then do
      termpgm='method term('i'=Rexx) static returns rexx;return' e
      fw=FileWriter("termpgm.nrx")
      fw.write(termpgm,0,termpgm.length)
      fw.close
      interpreter=NetRexxA()
      interpreter.parse([String 'termpgm.nrx'],[String 'nocrossref'])
      termClass=interpreter.getClassObject(null,'termpgm')
      classes=[interpreter.getClassObject('netrexx.lang', 'Rexx', 0)]
      termMethod=termClass.getMethod('term', classes)
    end

    return Rexx termMethod.invoke(null,[iv])


```



## Nim


```nim
var i: int

proc harmonicSum(i: var int, lo, hi, term): float =
  i = lo
  while i <= hi:
    result += term()
    inc i

echo harmonicSum(i, 1, 100, proc: float = 1.0 / float(i))
```

Output:

```txt
5.1873775176396206e+00
```



## Objeck


```objeck

bundle Default {
  class Jensens {
    i : static : Int;

    function : Sum(lo : Int, hi : Int, term : () ~ Float) ~ Float {
      temp := 0.0;

      for(i := lo; i <= hi; i += 1;) {
        temp += term();
      };

      return temp;
    }

    function : term() ~ Float {
      return 1.0 / i;
    }

    function : Main(args : String[]) ~ Nil {
      Sum(1, 100, term() ~ Float)->PrintLine();
    }
  }
}

```


Output: 5.18738


## OCaml


```ocaml
let i = ref 42 (* initial value doesn't matter *)

let sum' i lo hi term =
  let result = ref 0. in
    i := lo;
    while !i <= hi do
      result := !result +. term ();
      incr i
    done;
    !result

let () =
  Printf.printf "%f\n" (sum' i 1 100 (fun () -> 1. /. float !i))
```

Output: 5.187378


## Oforth



```Oforth
: mysum(lo, hi, term)  | i | 0 lo hi for: i [ i term perform + ] ;
```


{{out}}

```txt

mysum(1, 100, #inv) println
5.18737751763962

mysum(1, 100, #[ sq inv ]) println
1.63498390018489

```



## Oz

Translation using mutable references and an anonymous function:

```oz
declare
  fun {Sum I Lo Hi Term}
     Temp = {NewCell 0.0}
  in
     I := Lo
     for while:@I =< Hi do
        Temp := @Temp + {Term}
        I := @I + 1
     end
     @Temp
  end
  I = {NewCell unit}
in
  {Show {Sum I 1 100 fun {$} 1.0 / {Int.toFloat @I} end}}
```


Idiomatic code:

```oz
declare
  fun {Sum Lo Hi F}
     {FoldL {Map {List.number Lo Hi 1} F} Number.'+' 0.0}
  end
in
  {Show {Sum 1 100 fun {$ I} 1.0/{Int.toFloat I} end}}
```



## PARI/GP

GP does not have pass-by-reference semantics for user-generated functions, though some predefined functions do.  PARI programming allows this, though such a solution would essentially be identical to the [[#C|C]] solution above.

## Pascal


```pascal
{$MODE objFPC}
type
  tTerm = function(i: integer):real;

function term(i:integer):real;
Begin
  term := 1/i;
end;

function sum(var i: LongInt;
              lo,hi: integer;
              term:tTerm):real;

Begin
  result := 0;
  i := lo;
  while i<=hi do begin
    result := result+term(i);
    inc(i);
    end;
end;

var
  i : LongInt;
Begin
  writeln(sum(i,1,100,@term));
end.

```

Out

```txt
 5.1873775176396206E+000
```


## Perl


```perl
my $i;
sub sum {
    my ($i, $lo, $hi, $term) = @_;
    my $temp = 0;
    for ($$i = $lo; $$i <= $hi; $$i++) {
        $temp += $term->();
    }
    return $temp;
}

print sum(\$i, 1, 100, sub { 1 / $i }), "\n";
```

Output: 5.18737751763962

Or you can take advantage of the fact that elements of the @_ are aliases of the original:

```perl
my $i;
sub sum {
    my (undef, $lo, $hi, $term) = @_;
    my $temp = 0;
    for ($_[0] = $lo; $_[0] <= $hi; $_[0]++) {
        $temp += $term->();
    }
    return $temp;
}

print sum($i, 1, 100, sub { 1 / $i }), "\n";
```

Output: 5.18737751763962


## Perl 6

Rather than playing tricks like Perl 5 does, the declarations of the formal parameters are quite straightforward in Perl 6:

```perl6
sub sum($i is rw, $lo, $hi, &term) {
    my $temp = 0;
    loop ($i = $lo; $i <= $hi; $i++) {
        $temp += term;
    }
    return $temp;
}

my $i;
say sum $i, 1, 100, { 1 / $i };
```

Note that the C-style "for" loop is pronounced "loop" in Perl 6, and is the only loop statement that actually requires parens.


## Phix

Not really as asked for (implicit assumption replaced with explicit parameter) but this gives the required result.

I could also have done what C and PHP are doing, though in Phix I'd have to explicitly assign the static var within the loop.

I wholeheartedly agree with the comment on the Clipper example.

```Phix
function sumr(integer lo, hi, rid)
    atom res = 0
    for i=lo to hi do
        res += call_func(rid,{i})
    end for
    return res
end function

function reciprocal(atom i) return 1/i end function

?sumr(1, 100, routine_id("reciprocal"))
```

{{out}}

```txt

5.187377518

```



## PHP


```php
$i;
function sum (&$i, $lo, $hi, $term) {
    $temp = 0;
    for ($i = $lo; $i <= $hi; $i++) {
        $temp += $term();
    }
    return $temp;
}

echo sum($i, 1, 100, create_function('', 'global $i; return 1 / $i;')), "\n";
//Output: 5.18737751764 (5.1873775176396)

function sum ($lo,$hi)
{
 $temp = 0;
 for ($i = $lo; $i <= $hi; $i++)
 {
  $temp += (1 / $i);
 }
 return $temp;
}
echo sum(1,100);

//Output: 5.1873775176396

```



## PicoLisp


```PicoLisp
(scl 6)

(de jensen (I Lo Hi Term)
   (let Temp 0
      (set I Lo)
      (while (>= Hi (val I))
         (inc 'Temp (Term))
         (inc I) )
      Temp ) )

(let I (box)  # Create indirect reference
   (format
      (jensen I 1 100 '(() (*/ 1.0 (val I))))
      *Scl ) )
```

Output:

```txt
-> "5.187383"
```


## PureBasic

{{trans|C}}

```PureBasic
Prototype.d func()

Global i

Procedure.d Sum(*i.Integer, lo, hi, *term.func)
  Protected Temp.d
  For i=lo To hi
    temp + *term()
  Next
  ProcedureReturn Temp
EndProcedure

Procedure.d term_func()
  ProcedureReturn 1/i
EndProcedure

Answer.d = Sum(@i, 1, 100, @term_func())
```



## Python


```python
class Ref(object):
    def __init__(self, value=None):
        self.value = value

def harmonic_sum(i, lo, hi, term):
    # term is passed by-name, and so is i
    temp = 0
    i.value = lo
    while i.value <= hi:  # Python "for" loop creates a distinct which
        temp += term() # would not be shared with the passed "i"
        i.value += 1   # Here the actual passed "i" is incremented.
    return temp

i = Ref()

# note the correspondence between the mathematical notation and the
# call to sum it's almost as good as sum(1/i for i in range(1,101))
print harmonic_sum(i, 1, 100, lambda: 1.0/i.value)
```

Output: 5.18737751764


## R

R uses a [[wp:Evaluation_strategy#Call_by_need|call by need]] evaluation strategy where function inputs
are evaluated on demand and then cached; functions can bypass the normal argument evaluation by using functions <tt>substitute</tt> and <tt>match.call</tt> to access the parse tree of the as-yet-unevaluated arguments, and using <tt>parent.frame</tt> to access the scope of the caller. There are some proposed
[http://developer.r-project.org/nonstandard-eval.pdf conventions] to do this in a way that is less confusing to the user
of a function; however, ignoring conventions we can come disturbingly close to the ALGOL call-by-name semantics.


```R
sum <- function(var, lo, hi, term)
  eval(substitute({
    .temp <- 0;
    for (var in lo:hi) {
      .temp <- .temp + term
    }
    .temp
  }, as.list(match.call()[-1])),
  enclos=parent.frame())

sum(i, 1, 100, 1/i) #prints 5.187378

##and because of enclos=parent.frame(), the term can involve variables in the caller's scope:
x <- -1
sum(i, 1, 100, i^x) #5.187378
```




## Racket

Racket happens to have an Algol 60-language, so Jensen's Device can
be written just as Jørn Jensen did at Regnecentralen.


```racket

#lang algol60
begin
   integer i;
   real procedure sum (i, lo, hi, term);
      value lo, hi;
      integer i, lo, hi;
      real term;
      comment term is passed by-name, and so is i;
   begin
      real temp;
      temp := 0;
      for i := lo step 1 until hi do
         temp := temp + term;
      sum := temp
   end;
   comment note the correspondence between the mathematical notation and the call to sum;
   printnln (sum (i, 1, 100, 1/i))
end

```


But of course you can also use the more boring popular alternative of first class functions:


```racket

#lang racket/base
(define (sum lo hi f)
  (for/sum ([i (in-range lo (add1 hi))]) (f i)))
(sum 1 100 (λ(i) (/ 1.0 i)))

```



## Rascal


```rascal
public num Jenssen(int lo, int hi, num (int i) term){
	temp = 0;
	while (lo <= hi){
		temp += term(lo);
		lo += 1;}
	return temp;
}
```


With as output:


```rascal>rascal
Jenssen(1, 100, num(int i){return 1.0/i;})
num: 5.18737751763962026080511767565825315790897212670845165317653395662
```



## REXX

Note:   the 2<sup>nd</sup> and 3<sup>rd</sup> arguments for the   '''sum'''   function needn't be enclosed in quotes   (as they're numeric);

they were enclosed just to be consistent with the other arguments.

```rexx
/*REXX program demonstrates   Jensen's device   (via call subroutine, and args by name).*/
parse arg d .;   if d=='' | d==","  then d=100   /*Not specified?  Then use the default.*/
numeric digits d                                 /*use  D  decimal digits (9 is default)*/
say 'using '    d    " decimal digits:"          /*display what's being used for digits.*/
say
say sum( 'i',   "1",   '100',   "1/i" )          /*invoke  SUM  (100th harmonic number).*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sum: procedure;   parse arg j,start,finish,exp;              $=0

     interpret   'do'    j    "="   start   'to'   finish";   $=$+"    exp    ';   end'
            /*   ────    ═    ───   ═════   ────   ══════──────────    ═══    ───────── */
            /*    lit   var   lit    var     lit     var   literal     var     literal  */

     return $
```

{{out|output|text=  when using the default input:}}

```txt

using  100  decimal digits:

5.187377517639620260805117675658253157908972126708451653176533956587219557532550496605687768923120415

```

{{out|output|text=  when using the input:   <tt> 1000 </tt>}}

(Shown at three-quarter size    and   with 200 characters per line.)
<pre style="font-size:75%">
using  1000  decimal digits:

5.187377517639620260805117675658253157908972126708451653176533956587219557532550496605687768923120413552951372900080959485764334902003859251284547479399606488677719356437701034351417501628003612133813
93634033610397170258150385609229760925775852490242015786454123413833660918987060275907253504512582948807527866739590394714709377905509971663909084580816222756304901297019081913723833776150679344482592
19985786828216280140988475651174867766685160764730429716983310052063466701008405663630740646670436720827975050329078640945579952223172461998152578702106818073281191723171032278163615245743308956980821
10786794204451169328900410057940565163334352244388766863157323818250401277131246550164879348955299573048040410736739783727083287179928615106959660501145265658411572959372901925824344377263363761945330
17905075097606740175205276891748232922334187250177881689092871712673549165589217457070884105311065936887252732260150280756519586504475363590572034459636088593436136141078274322996362525543164325745468
2

```



## Ring


```ring

# Project : Jensen's Device

decimals(14)
i = 100
see sum(i,1,100,"1/n") + nl

func sum(i,lo,hi,term)
        temp = 0
        for n = lo to hi step 1
             eval("num = " + term)
             temp = temp + num
        next
        return temp

```

Output:

```txt

5.18737751763962

```



## Ruby

Here, setting the variable and evaluating the term are truly executed in the "outer" context:

```ruby
def sum(var, lo, hi, term, context)
  sum = 0.0
  lo.upto(hi) do |n|
    sum += eval "#{var} = #{n}; #{term}", context
  end
  sum
end
p sum "i", 1, 100, "1.0 / i", binding   # => 5.18737751763962
```


But here is the Ruby way to do it:

```ruby
def sum2(lo, hi)
  lo.upto(hi).inject(0.0) {|sum, n| sum += yield n}
end
p sum2(1, 100) {|i| 1.0/i}  # => 5.18737751763962
```


Even more concise: (requires ruby >= 2.4)

```ruby

def sum lo, hi, &term
  (lo..hi).sum(&term)
end
p sum(1,100){|i| 1.0/i}   # => 5.187377517639621
# or using Rational:
p sum(1,100){|i| Rational(1,i)}  # => 14466636279520351160221518043104131447711 / 2788815009188499086581352357412492142272

```



## Scala

Actually, the <code>i</code> parameter needs to be passed by reference, as done in so many
examples here, so that changes made to it reflect on the parameter that was passed. Scala
supports passing parameters by name, but not by reference, which means it can't change the
value of any parameter passed. The code below gets around that by creating a mutable integer
class, which is effectively the same as passing by reference.


```scala
class MyInt { var i: Int = _ }
val i = new MyInt
def sum(i: MyInt, lo: Int, hi: Int, term: => Double) = {
  var temp = 0.0
  i.i = lo
  while(i.i <= hi) {
    temp = temp + term
    i.i += 1
  }
  temp
}
sum(i, 1, 100, 1.0 / i.i)
```


Result:


```txt

res2: Double = 5.187377517639621

```



## Scheme

Scheme procedures do not support call-by-name. Scheme macros, however, do:


```scheme

(define-syntax sum
  (syntax-rules ()
    ((sum var low high . body)
     (let loop ((var low)
                (result 0))
       (if (> var high)
           result
           (loop (+ var 1)
                 (+ result . body)))))))

```



```txt

(exact->inexact (sum i 1 100 (/ 1 i)))
5.18737751763962

```



## Seed7

Seed7 supports call-by-name with function parameters:


```seed7

$ include "seed7_05.s7i";
  include "float.s7i";

var integer: i is 0;

const func float: sum (inout integer: i, in integer: lo, in integer: hi,
    ref func float: term) is func
  result
    var float: sum is 0.0
  begin
    for i range lo to hi do
      sum +:= term;
    end for;
  end func;

const proc: main is func
  begin
   writeln(sum(i, 1, 100, 1.0/flt(i)) digits 6);
  end func;

```


Output:

```txt

5.187378

```



## Sidef


```ruby
var i;
func sum (i, lo, hi, term) {
    var temp = 0;
    for (*i = lo; *i <= hi; (*i)++) {
        temp += term.run;
    };
    return temp;
};
say sum(\i, 1, 100, { 1 / i });
```

{{out}}

```txt
5.18737751763962026080511767565825315790899
```



## Simula

{{trans|algol60}}
{{works with|SIMULA-67}}
Compare with Algol 60, in Simula 67 'call by name' is specified with '''name'''. It is a true 'call by name' evaluation not a 'procedure parameter' emulation.
```simula
comment Jensen's Device;
begin
   integer i;
   real procedure sum (i, lo, hi, term);
      name i, term;
      value lo, hi;
      integer i, lo, hi;
      real term;
      comment term is passed by-name, and so is i;
   begin
      integer j;
      real temp;
      temp := 0;
      for j := lo step 1 until hi do
      begin
         i := j;
         temp := temp + term
      end;
      sum := temp
   end;
   comment note the correspondence between the mathematical notation and the call to sum;
   outreal (sum (i, 1, 100, 1/i), 7, 14)
end
```

{{out}}

```txt

 5.187378&+000  

```



## Standard ML


```sml
val i = ref 42 (* initial value doesn't matter *)

fun sum' (i, lo, hi, term) = let
  val result = ref 0.0
in
  i := lo;
  while !i <= hi do (
    result := !result + term ();
    i := !i + 1
  );
  !result
end

val () =
  print (Real.toString (sum' (i, 1, 100, fn () => 1.0 / real (!i))) ^ "\n")
```

Output: 5.18737751764


## Swift


```swift
var i = 42 // initial value doesn't matter

func sum(inout i: Int, lo: Int, hi: Int, @autoclosure term: () -> Double) -> Double {
  var result = 0.0
  for i = lo; i <= hi; i++ {
    result += term()
  }
  return result
}

println(sum(&i, 1, 100, 1 / Double(i)))
```

(Prior to Swift 1.2, replace <code>@autoclosure term: () -> Double</code> with <code>term: @autoclosure () -> Double</code>.)
{{out}}

```txt
5.187378
```



## VBA


```vb

Private Function sum(i As String, ByVal lo As Integer, ByVal hi As Integer, term As String) As Double
    Dim temp As Double
    For k = lo To hi
        temp = temp + Evaluate(Replace(term, i, k))
    Next k
    sum = temp
End Function
Sub Jensen_Device()
    Debug.Print sum("i", 1, 100, "1/i")
    Debug.Print sum("i", 1, 100, "i*i")
    Debug.Print sum("j", 1, 100, "sin(j)")
End Sub

```

{{out}}

```txt

 5,18737751763962
 338350
-0,12717101366042

```



## Tcl

Here, we set the value of the passed variable in the caller's frame.  We then evaluate the passed term there too.

```tcl
proc sum {var lo hi term} {
    upvar 1 $var x
    set sum 0.0
    for {set x $lo} {$x < $hi} {incr x} {
        set sum [expr {$sum + [uplevel 1 [list expr $term]]}]
    }
    return $sum
}
puts [sum i 1 100 {1.0/$i}] ;# 5.177377517639621
```

However, the solution is expressed more simply like this

```tcl
proc sum2 {lo hi lambda} {
    set sum 0.0
    for {set n $lo} {$n < $hi} {incr n} {
        set sum [expr {$sum + [apply $lambda $n]}]
    }
    return $sum
}
puts [sum2 1 100 {i {expr {1.0/$i}}}] ;# 5.177377517639621
```



## zkl

zkl doesn't support call by name/address but does have reference objects. Using an explicit call to term:

```zkl
fcn sum(ri, lo,hi, term){
   temp:=0.0; ri.set(lo);
   do{ temp+=term(ri); } while(ri.inc()<hi); // inc return previous value
   return(temp);
}
sum(Ref(0), 1,100, fcn(ri){ 1.0/ri.value }).println();
```

Using function application/deferred(lazy) objects, we can make the function call implicit (addition forces evaluation of the LHS):

```zkl
fcn sum2(ri, lo,hi, term){
   temp:=0.0; ri.set(lo);
   do{ temp=term + temp; } while(ri.inc()<hi); // inc return previous value
   return(temp);
}
ri:=Ref(0);
sum2(ri, 1,100, 'wrap(){ 1.0/ri.value }).println();
```

In this case, we can call sum or sum2 and it does the same thing (the ri parameter will be ignored).

Of course, as others have pointed out, this can be expressed very simply:

```zkl
fcn sum3(lo,hi, term){ [lo..hi].reduce('wrap(sum,i){ sum + term(i) },0.0) }
sum3(1,100, fcn(i){ 1.0/i }).println();
```

{{out}}

```txt

5.187378
5.187378
5.187378

```



## ZX Spectrum Basic


```zxbasic
10 DEF FN r(x)=1/x
20 LET f$="FN r(i)"
30 LET lo=1: LET hi=100
40 GO SUB 1000
50 PRINT temp
60 STOP
1000 REM Evaluation
1010 LET temp=0
1020 FOR i=lo TO hi
1030 LET temp=temp+VAL f$
1040 NEXT i
1050 RETURN

```

{{out}}

```txt

5.1873775

```


{{omit from|GUISS}}
{{omit from|gnuplot}}
