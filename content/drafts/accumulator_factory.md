+++
title = "Accumulator factory"
description = ""
date = 2019-09-11T03:46:02Z
aliases = []
[extra]
id = 5255
[taxonomies]
categories = []
tags = []
+++

{{task}}
{{requires|Mutable State}}
{{Omit from|MUMPS|Creating a function implies that there is routine somewhere that has the function stored, and that function could be modified}}

A problem posed by [[wp:Paul Graham|Paul Graham]] is that of creating a function that takes a single (numeric) argument and which returns another function that is an accumulator. The returned accumulator function in turn also takes a single numeric argument, and returns the sum of all the numeric values passed in so far to that accumulator (including the initial value passed when the accumulator was created).


;Rules:
The detailed rules are at http://paulgraham.com/accgensub.html and are reproduced here for simplicity (with additions in <small>''small italic text''</small>).
:Before you submit an example, make sure the function

:# Takes a number n and returns a function (lets call it g), that takes a number i, and returns n incremented by the accumulation of i from every call of function g(i).
<small>Although these exact function and parameter names need not be used</small>
:# Works for any numeric type-- i.e. can take both ints and floats and returns functions that can take both ints and floats. (It is not enough simply to convert all input to floats. An accumulator that has only seen integers must return integers.) <small>''(i.e., if the language doesn't allow for numeric polymorphism, you have to use overloading or something like that)''</small>
:# Generates functions that return the sum of every number ever passed to them, not just the most recent. <small>''(This requires a piece of state to hold the accumulated value, which in turn means that pure functional languages can't be used for this task.)''</small>
:# Returns a real function, meaning something that you can use wherever you could use a function you had defined in the ordinary way in the text of your program. <small>''(Follow your language's conventions here.)''</small>
:# Doesn't store the accumulated value or the returned functions in a way that could cause them to be inadvertently modified by other code. <small>''(No global variables or other such things.)''</small>
: E.g. if after the example, you added the following code (in a made-up language) <small>''where the factory function is called foo''</small>:
:: 
```pseudocode
x = foo(1); 
x(5); 
foo(3);
print x(2.3);
```

: It should print <tt>8.3</tt>. <small>''(There is no need to print the form of the accumulator function returned by <tt>foo(3)</tt>; it's not part of the task at all.)''</small>


;Task:
Create a function that implements the described rules. 


It need not handle any special error cases not described above. The simplest way to implement the task as described is typically to use a [[Closures|closure]], providing the language supports them.

Where it is not possible to hold exactly to the constraints above, describe the deviations.





## 8th


```Forth

\ RossetaCode 'accumulator factory'

\ The 'accumulate' word stores the accumulated value in an array, because arrays
\ are mutable:
: accumulate \ n [m] -- n+m \ [m] -> [n+m]
  a:pop rot n:+
  tuck a:push swap ;

\ To comply with the rules, this takes a number and wraps it in an array, and
\ then curries it.  Since 'curry:' is "immediate", we need to postpone its
\ action using 'p:.

: make-accumulator
  1 a:close 
  ' accumulate 
  p: curry: ;

\ We 'curry' the initial value along with 'accumulate' to create
\ a new word, '+10', which will give us the accumulated values
10 make-accumulator +10

\ This loop will add 1, then 2, then 3, to the accumulator, which prints the
\ results 11,13,16:
( +10 . cr ) 1 3 loop 
bye
```


{{out}}

```txt

11
13
16

```


## ABAP

ABAP, unfortunately, has no first order functions, nor does its OO paradigm implement method overloading. One potential solution to this problem is to use classes to maintain the state, with the import/export parameters being defined as type 'any', so that the resultant type is calculated dynamically.

Another possible solution would be to use the languages in-built JavaScript processing capabilities to dynamically construct a JS source at run-time, which implements the JS Accumulator factory.

###  Object Oriented Solution 


```ABAP
report z_accumulator
class acc definition.
  public section.
    methods:
      call importing iv_i type any default 0 exporting ev_r type any,
      constructor importing iv_d type f.
  private section.
    data a_sum type f.
endclass.

class acc implementation.
  method call.
      add iv_i to a_sum.
      ev_r = a_sum.
  endmethod.

start-of-selection.

data: cl_acc type ref to acc,
      lv_ret2 type f,
      lv_ret1 type i.

create object cl_acc exporting iv_d = 1.
cl_acc->call( exporting iv_i = 5 ).
cl_acc->call( exporting iv_i = '2.3' importing ev_r = lv_ret2 ).
cl_acc->call( exporting iv_i = 2 importing ev_r = lv_ret1 ).
write : / lv_ret2 decimals 2 exponent 0 left-justified, / lv_ret1 left-justified.
```

{{out}}

```txt

 8.30
10

```


###  JavaScript Solution 


```ABAP
data: lv_source type string,
      cl_processor type ref to cl_java_script,
      lv_ret type string.

cl_processor = cl_java_script=>create( ).
concatenate
'function acc(sum) { '
'  return function(n) { '
'   return sum += n;'
'  }; '
' }; '
' var x = acc(1); '
' x(5);'
' var ret = acc(3).toString();'
' ret = ret + x(2.3);'
 into lv_source.
lv_ret = cl_processor->evaluate( lv_source ).

if cl_processor->last_condition_code <> cl_java_script=>cc_ok.
  write cl_processor->last_error_message.
else.
  write lv_ret.
  write / 'Done'.
endif.
```



```txt
#function (n) {#    return sum += n;#}#8.3
```



## ActionScript

Closures work the same in ActionScript  as in JavaScript. ActionScript will transparently convert integers to reals if the function is given a real argument, but the typeof operator must be used to ensure the function isn't sent invalid arguments, such as strings (which would silently convert the accumulated number to a string without throwing an error).
{{trans|Javascript}}

```ActionScript
//Throw an error if a non-number argument is used. (typeof evaluates to
// "number" for both integers and reals)
function checkType(obj:Object):void {
    if(typeof obj != "number")
	throw new ArgumentError("Expected integer or float argument. Recieved " + typeof obj);
}
function accumulator(sum:Object):Function {
    checkType(sum);
    return function(n:Object):Object {checkType(n); return sum += n};
}
var acc:Function=accumulator(2);
trace(acc(10));
trace(acc(4));
trace(acc("123")); //This causes an ArgumentError to be thrown.
```



## Ada


```Ada
with Accumulator;
with Ada.Text_IO; use Ada.Text_IO;

procedure Example is
   package A is new Accumulator;
   package B is new Accumulator;
begin
   Put_Line (Integer'Image (A.The_Function (5)));
   Put_Line (Integer'Image (B.The_Function (3)));
   Put_Line (Float'Image (A.The_Function (2.3)));
end;
```



```Ada
generic package Accumulator is

--  This Ada generic package represents an accumulator factory.
--  The required function is provided as The_Function.
--  The first call to The_Function sets the initial value.
--  (Marius Amado-Alves)

   function The_Function (X : Integer) return Integer;
   function The_Function (X : Integer) return Float;
   function The_Function (X : Float) return Float;
end;
```



```Ada
package body Accumulator is

--  The accumulator lives through three states. It is in Virgin_State
--  before any use of The_Function. It changes to Integer_State or
--  Float_State, according to the input type used. The accumulation is
--  memorized in variable I or F, according to the state. Float_State,
--  once reached, is never left. A Float output on an Integer_State is
--  simply a conversion, sans effect on state. (Marius Amado-Alves)

   type State_T is (Virgin_State, Integer_State, Float_State);
   State : State_T := Virgin_State;
   I : Integer;
   F : Float;

   function The_Function (X : Float) return Float is
   begin
      case State is
         when Virgin_State =>
            State := Float_State;
            F := X;
            return F;
         when Integer_State =>
            State := Float_State;
            F := Float (I) + X;
            return F;
         when Float_State =>
            F := F + X;
            return F;
      end case;
   end;

   function The_Function (X : Integer) return Float is
   begin
      case State is
         when Virgin_State =>
            State := Integer_State;
            I := X;
            return Float (I);
         when Integer_State =>
            I := I + X;
            return Float (I);
         when Float_State =>
            F := F + Float (X);
            return F;
      end case;
   end;

   function The_Function (X : Integer) return Integer is
   begin
      case State is
         when Virgin_State =>
            State := Integer_State;
            I := X;
            return I;
         when Integer_State =>
            I := I + X;
            return I;
         when Float_State =>
            F := F + Float (X);
            return Integer (F);
      end case;
   end;

end;
```



## Aikido

{{trans|Javascript}}

```aikido
function accumulator (sum:real) {
    return function(n:real) { return sum += n }
}

var x = accumulator(1)
x(5)
println (accumulator)
println (x(2.3))
```

{{out}}
 accumulator
 8.3


## Aime


```aime
af(list l, object o)
{
    l[0] = l[0] + o;
}

main(void)
{
    object (*f)(object);

    f = af.apply(list(1));

    f(5);
    af.apply(list(3));
    o_(f(2.3), "\n");

    0;
}
```

{{Out}}

```txt
8.3
```

The type is properly preserved over summing:

```aime
    f = af.apply(list(5));

    f(-6);
    f(7);
    o_form("~: ~\n", f(0).__type, f(0));

    f = af.apply(list(8));

    f(-6.6);
    f(4.2);
    o_form("~: /d1/\n", f(0).__type, f(0));
```

{{Out}}

```txt
integer: 6
real: 5.6
```



## ALGOL 68

{{trans|aikido}}{{wont work with|ALGOL 68|Revision 1 - scoping rules forbid exporting a procedure out of it's scope}}
{{wont work with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny] - scoping rules forbid exporting a procedure out of it's scope - detected at compile time and again at runtime}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}
Note: Standard ALGOL 68's scoping rules forbids exporting a '''procedure''' (or '''format''') out of it's scope (closure).  Hence this specimen will run on [[ELLA ALGOL 68]], but is non-standard.  For a discussion of first-class functions in ALGOL 68 consult [http://www.cs.ru.nl/~kees/home/papers/psi96.pdf "The Making of Algol 68"] - [[wp:Cornelis_H.A._Koster|C.H.A. Koster]] (1993). <!-- Retrieved April 28, 2007 -->

```algol68
MODE NUMBER =  UNION(INT,REAL,COMPL);

PROC plus = (NUMBER in a, in b)NUMBER: (
    CASE in a IN
        (INT a):   CASE in b IN (INT b): a+b, (REAL b): a+b, (COMPL b): a+b ESAC,
        (REAL a):  CASE in b IN (INT b): a+b, (REAL b): a+b, (COMPL b): a+b ESAC,
        (COMPL a): CASE in b IN (INT b): a+b, (REAL b): a+b, (COMPL b): a+b ESAC
    ESAC
);

main: (

# now override the + and +:= OPerators #
  OP + = (NUMBER a, b)NUMBER: plus(a,b);

  OP +:= = (REF NUMBER lhs, NUMBER rhs)NUMBER:
      lhs := lhs + rhs;

  PROC accumulator  = (REF NUMBER sum)PROC(NUMBER)NUMBER:
      (NUMBER n)NUMBER:  
          sum +:= n;
   
  PROC (NUMBER)NUMBER x = accumulator(LOC NUMBER := 1);
  x(5);
  print(("x:",x(2.3), new line));

  PROC (NUMBER)NUMBER y = accumulator(LOC NUMBER := 100);
  y(500);
  print(("y:",y(230), new line));

  print(("x:",x(0), new line))

)
```

{{out}}

```txt

x: +.830000000000000e +1
y:        +830
x: +.830000000000000e +1

```



## AppleScript

This has one deviation. AppleScript needs a script object for the closure on the sum <code>n</code>. So this factory returns a script object, not a handler by itself. One must call the handler through its script object, as in <code>x's call(1)</code>.


```applescript
on accumulator(n)
	-- Returns a new script object
	-- containing a handler.
	script
		on call(i)
			set n to n + i -- Returns n.
		end call
	end script
end accumulator

set x to accumulator(10)
log x's call(1)
set y to accumulator(5)
log y's call(2)
log x's call(3.5)
-- Event Log: (*11*) (*7*) (*14.5*)
```



Or, to match the task spec and output a little more closely:


```AppleScript
on run
    
    set x to foo(1)
    
    x's |λ|(5)
    
    foo(3)
    
    x's |λ|(2.3)
    
end run

-- foo :: Int -> Script
on foo(sum)
    script
        on |λ|(n)
            set sum to sum + n
        end |λ|
    end script
end foo
```

{{Out}}

```txt
8.3
```



## Argile

{{works with|Argile|1.1.1}}

```Argile
use std, array

let A = accumulator 42
print(A 0)
print(A 1)
print(A 10)
print(A 100)

let B = accumulator 4.2
print(B 0)
print(B 1)
print(B 10.0)
print(B 100.4)

~A ; ~B
(: use dbg; check mem leak :)

(: accumulator call :)
=: <accumulator a> <num x> := -> (a.t)
   call ((a.func) as function(any)(a.t)->(a.t)) with (a.data) ((Cgen x) as a.t)

(: accumulator constructors :)
.: accumulator <int x> :. -> int accumulator
   (val (int accumulator) A).init(x)
   (A as Accumulator).func = ( .:<int& accu, int x>:. ->int {accu += x; accu} )
   A

.: accumulator <real x> :. -> real accumulator
   (val (real accumulator) A).init(x)
   (A as Accumulator).func = ( .:<real&accu,real x>:. ->real{accu += x; accu} )
   A

=: <accumulator& a>.init <num x> :=
   a = new (Accumulator)
   a.data = (new array of 1 a.t)
   *(a.data as (a.t*)) = Cgen x

(: accumulator destructor :)
.: del Accumulator <Accumulator a>:.
   free a.data
   free a
=: ~ <accumulator a> := {del Accumulator a}

(: accumulator type :)
class Accumulator
  function	func
  any		data

=: [<type t=(int)>] accumulator := -> type
   Accumulator.prefix
   Accumulator.suffix

autocast accumulator<->Accumulator
```



## Astro


```python
fun accumulator(var sum): :: Real -> _
    n => sum += n

let f = accumulator!(5)
print f(5)   # 10
print f(10)  # 20
print f(2.4) # 22.4
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
This code works by copying the function FNdummy() onto the heap and returning a pointer to it.

```bbcbasic
      x = FNaccumulator(1)
      dummy = FN(x)(5)
      dummy = FNaccumulator(3)
      PRINT FN(x)(2.3)
      END
      
      DEF FNaccumulator(sum)
      LOCAL I%, P%, Q%
      DIM P% 53 : Q% = !^FNdummy()
      FOR I% = 0 TO 49 : P%?I% = Q%?I% : NEXT
      P%!I% = P% : sum = FN(P%+I%)(sum)
      = P%+I%
      
      DEF FNdummy(n)
      PRIVATE sum
      sum += n
      = sum
```



## Bracmat

Notice that Bracmat has no floating point numbers, only rational numbers. 

```bracmat
( ( accumulator
  =
    .
      ' ( add sum object
        .   (object=add=$arg+!arg)
          & !(object.add):?sum
          & '($($sum)+!arg):(=?(object.add))
          & !sum
        )
  )
& accumulator$1:(=?x)
& x$5
& accumulator$3
& out$(x$23/10)
)
```

Output:

```txt
83/10
```


## Brat


```brat
accumulator = { sum |
  { n | sum = sum + n }
}

x = accumulator 1
x 5
accumulator 3 #Does not affect x
p x 2.3 #Prints 8.3 (1 + 5 + 2.3)
```



## C

Deviation: Not in standard C, but several compilers include the typeof operator as an extension which can be used like a typedef. Functions must be defined outside of the main program body and they retain the same type throughout their life. C11 is supposed to give us some Type-generic macro expressions.


```C>#include <stdio.h

//~ Take a number n and return a function that takes a number i
#define ACCUMULATOR(name,n) __typeof__(n) name (__typeof__(n) i) { \
    static __typeof__(n) _n=n; LOGIC; }
//~ have it return n incremented by the accumulation of i
#define LOGIC return _n+=i
ACCUMULATOR(x,1.0)
ACCUMULATOR(y,3)
ACCUMULATOR(z,'a')
#undef LOGIC
int main (void) {
    printf ("%f\n", x(5));   /* 6.000000 */
    printf ("%f\n", x(2.3)); /* 8.300000 */
    printf ("%i\n", y(5.0)); /* 8 */
    printf ("%i\n", y(3.3)); /* 11 */
    printf ("%c\n", z(5));   /* f */
    return 0;
}
```



## C++

First solution has a deviation: The return type is wrong when the accumulator is called with an integer argument after is has been called with a float argument.  Later it is explained how to correct this.


```cpp>#include <iostream


class Acc
{
public:
    Acc(int init)
        : _type(intType)
        , _intVal(init)
    {}

    Acc(float init)
        : _type(floatType)
        , _floatVal(init)
    {}

    int operator()(int x)
    {
        if( _type == intType )
        {
            _intVal += x;
            return _intVal;
        }
        else
        {
            _floatVal += x;
            return static_cast<int>(_floatVal);
        }
    }

    float operator()(float x)
    {
        if( _type == intType )
        {
            _floatVal = _intVal + x;
            _type = floatType;
            return _floatVal;
        }
        else
        {
            _floatVal += x;
            return _floatVal;
        }
    }
private:
    enum {floatType, intType} _type;
    float _floatVal;
    int _intVal;
};

int main()
{
    Acc a(1);
    a(5);
    Acc(3);
    std::cout << a(2.3f);
    return 0;
}
```

{{works with|C++11}}
The following is similar to the above, using lambda functions from C++11. Note that we declared the lambda <code>mutable</code>, which allows us to modify variables that were captured by value. This feature allows us to maintain mutable state, which is essential for an accumulator.

It suffers from the same deviation as the former, where the return type is wrong when the accumulator is called with a float argument after is has been called with an integer argument.

```cpp>#include <iostream

#include <functional>

template <typename T>
std::function<T(T)> makeAccumulator(T sum) {
	return [=](T increment) mutable {
		return sum += increment;
	};
}

int main() {
	auto acc = makeAccumulator<float>(1);
	acc(5);
	makeAccumulator(3);
	std::cout << acc(2.3) << std::endl;
	return 0;
}
```


The deviation stems from two sources.  First, a C++ object (such as the accumulator) has an immutable type.  To correct this, we must separate the accumulator from the cumulant value it holds.  For example:

```cpp
struct CumulantBase_
{
   virtual ~CumulantBase_();
   virtual std::ostream& Write(std::ostream& dst) const = 0;
};

template<class T_> struct Cumulant_ : CumulantBase_
{
   T_ val_;
   Cumulant_(const T_& val) : val_(val) {}
   std::ostream& Write(std::ostream& dst) const override 
   {
      return dst << val_;
   }
};

struct Accumulator_
{
   std::unique_ptr<CumulantBase_> val_;
   template<class T_> Accumulator_(const T_& val) { Set(val); }
   template<class T_> void Set(const T_& val) { val_.reset(new Cumulant_<T_>(val)); }

```

(This is Coplien's "State" pattern.)  

The second issue is that the built-in operator + is a multimethod, implementing a compile-time dispatch and promotion which we must manually reproduce.  

```cpp
// still inside struct Accumulator_
	// various operator() implementations provide a de facto multimethod
	Accumulator_& operator()(int more)
	{
		if (auto i = CoerceInt(*val_))
			Set(+i + more);
		else if (auto d = CoerceDouble(*val_))
			Set(+d + more);
		else
			THROW("Accumulate(int) failed");
		return *this;
	}
	Accumulator_& operator()(double more)
	{
		if (auto d = CoerceDouble(*val_))
			Set(+d + more);
		else
			THROW("Accumulate(double) failed");
		return *this;
	}
	Accumulator_& operator()(const String_& more)
	{
		if (auto s = CoerceString(*val_))
			Set(+s + more);
		else
			THROW("Accumulate(string) failed");
		return *this;
	}
};

```


These rely on coercion functions which switch on the so-far-accumulated type:

```cpp
// recognize cumulants by type
boost::optional<int> CoerceInt(const CumulantBase_& c)
{
	if (auto p = dynamic_cast<const Cumulant_<int>*>(&c))
		return p->val_;
	return boost::optional<int>();
}
boost::optional<double> CoerceDouble(const CumulantBase_& c)
{
	if (auto p = dynamic_cast<const Cumulant_<double>*>(&c))
		return p->val_;
	if (auto i = CoerceInt(c))
		return boost::optional<double>(i);
	return boost::optional<double>();
}
boost::optional<String_> CoerceString(const CumulantBase_& c)
{
	if (auto p = dynamic_cast<const Cumulant_<String_>*>(&c))
		return p->val_;
	return boost::optional<String_>();
}

```


All that remains is to write to the stream:

```cpp
std::ostream& operator<<(std::ostream& dst, const Accumulator_& acc)
{
	return acc.val_->Write(dst);
}

```


=={{header|C sharp|C#}}==
{{works with|C sharp|4.0}}

```csharp
using System;

class Program
{
    static Func<dynamic, dynamic> Foo(dynamic n)
    {
        return i => n += i;
    }

    static void Main(string[] args)
    {
        var x = Foo(1);
        x(5);
        Foo(3);
        Console.WriteLine(x(2.3));
    }
}
```



## Ceylon


```Ceylon
shared void run() {
    Integer|Float accumulator
            (variable Integer|Float n)
            (Integer|Float i)
        =>  switch (i)
            case (is Integer)
                (n = n.plusInteger(i))
            case (is Float)
                (n = i + (switch(prev = n)
                          case (is Float) prev
                          case (is Integer) prev.float));

    value x = accumulator(1);
    print(x(5));
    print(accumulator(3));
    print(x(2.3));
}
```


{{out}}

```txt

6
<Integer|Float>(Integer|Float)
8.3

```



## Clay

To my knowledge Clay does not admit of an elegant solution to this problem, although it should be stated that I am still exploring the language. But a clean solution mirroring that for other static languages is quite simple (one in which the operative numeric type is constrained by the original call to acc):

```Clay
acc(n) {
    return (m) => {
        n = n + m;
        return n;
    };
}

main() {
    var x = acc(1.0);
    x(5);
    acc(3);
    println(x(2.3)); // Prints “8.300000000000001”.
}
```

Although statically typed, due to Clay’s everywhere-genericity this has the advantage of working out of the box for any type that defines addition:

```Clay
    var y = acc(Vector[Char]("Hello"));
    println(y(" World!")); // Prints "Hello World!”.
```

But you could constrain the function to numeric types were you so inclined:

```Clay
[N | Numeric?(N)] acc(n: N) {
    return (m) => {
        n = n + m;
        return n;
    };
}
```

One could go crazy with tagged unions and runtime dispatching to rig something up that adhered more closely to the problem’s specification. But I know of no easier way to “change types” in the fashion necessary.


## Clojure

The ''atom'' function creates an atomically updatable identity holding a value. The ''swap!'' function atomically updates the atom's value, returning the new value. The function returned from an ''accum'' call satisfies all the requirements.

```clojure
(defn accum [n]
  (let [acc (atom n)]
    (fn [m] (swap! acc + m))))
```

Similarly, a ''ref'' could be used.

```clojure
(defn accum [n]
  (let [acc (ref n)]
    #(dosync (alter acc + %))))
```



## CoffeeScript


```coffeescript
accumulator = (sum) ->
  (n) -> sum += n
  
f = accumulator(1)
console.log f(5)
console.log f(2.3)
```



## Common Lisp

{{trans|TXR}}

```lisp
(defun accumulator (sum)
  (lambda (n)
    (incf sum n)))
```

Example usage:

```lisp
(defvar x (accumulator 1))
(funcall x 5)
(accumulator 3)
(funcall x 2.3)
```

{{out}}

```txt

X
6
#<CLOSURE :LAMBDA (N) (SETF SUM (+ SUM N))>
8.3

```


## Crystal


```crystal

# Make types a bit easier with an alias
alias Num = Int32 | Int64 | Float32 | Float64

def accumulator(sum : Num)
  # This proc is very similar to a Ruby lambda
  ->(n : Num){ sum += n }
end

x = accumulator(5)
puts x.call(5)   #=> 10
puts x.call(10)  #=> 20
puts x.call(2.4) #=> 22.4

```


## D



```d
import std.stdio;

void main() {
    auto x = acc(1);
    x(5);
    acc(3);
    writeln(x(2.3));
}

auto acc(U = real, T)(T initvalue) { // U is type of the accumulator
    auto accum = cast(U)initvalue ;
    return (U n) { return accum += n ; } ;
}
```



## Dart


The => operator is Dart's special syntax for single line closures. When you use it the value of the expression is automatically returned without the return statement.

note: Function is the return type of the accumulator function, not the keyword used to define functions. There is no function keyword in Dart. The return type is optional, just like all types in Dart. The declaration could just be: accumulator(var n) => ...


```dart
Function accumulator(var n) => (var i) => n += i;

void main() {
  var a = accumulator(42);
  print("${a(0)}, ${a(1)}, ${a(10)}, ${a(100)}");

  var b = accumulator(4.2);
  print("${b(0)}, ${b(1)}, ${b(10.0)}, ${b(100.4)}");
}
```


{{out}}

```txt
42, 43, 53, 153
4.2, 5.2, 15.2, 115.60000000000001
```


=={{header|Déjà Vu}}==

```dejavu
accum n:
	labda i:
		set :n + n i
		n

local :x accum 1
drop x 5
drop accum 3
!print x 2.3
```



## E


```e
def foo(var x) {
  return fn y { x += y }
}
```



## EchoLisp


```lisp

(define-syntax-rule (inc x v) (set! x (+ x v)))
(define (accumulator (sum 0)) (lambda(x) (inc sum x) sum))

(define x (accumulator 1)) → x
(x 5) → 6

;; another closure
(accumulator 3) → (🔒 λ (_x) (📝 #set! sum (#+ sum _x)) sum)

(x 2.3) → 8.3

```



## Elena

ELENA 4.x :

```elena
function(acc)
    = (n => acc.append:n);

accumulator(n)
    = function(new Variable(n));

public program()
{
    var x := accumulator(1);
    
    x(5);
    
    var y := accumulator(3);
    
    console.write(x(2.3r))
}
```

{{out}}

```txt

8.3

```



## Elixir

Elixir provides Agents to simplify creating a process to maintain state where mutable variables aren't allowed.

```elixir
defmodule AccumulatorFactory do
  def new(initial) do
    {:ok, pid} = Agent.start_link(fn() -> initial end)
    fn (a) -> 
      Agent.get_and_update(pid, fn(old) -> {a + old, a + old} end) 
    end
  end
end
```

The passing test to exercise the Accumulator and show usage:

```elixir
ExUnit.start

defmodule AccumulatorFactoryTest do
  use ExUnit.Case

  test "Accumulator basic function" do
    foo = AccumulatorFactory.new(1)
    foo.(5)
    bar = AccumulatorFactory.new(3)
    assert bar.(4) == 7
    assert foo.(2.3) == 8.3
  end
end
```


{{out}}

```txt

.

Finished in 0.06 seconds (0.06s on load, 0.00s on tests)
1 test, 0 failures

Randomized with seed 587000

```



## Erlang

Erlang doesn't allow for mutable variables, but does have variable capture in closures. By spawning a process which loops endlessly, incrementing the sum and returning it to the caller, this mutable state can be imitated.

```erlang

-module(acc_factory).
-export([loop/1,new/1]).

loop(N)->  
    receive
        {P,I}->
            S =N+I, P!S, loop(S)
    end.

new(N)->
    P=spawn(acc_factory,loop,[N]),
    fun(I)->
            P!{self(),I},
            receive  
                V-> V
            end
    end.

```



## ERRE


```ERRE
PROGRAM ACCUMULATOR

PROCEDURE ACCUMULATOR(SUM,N,A->SUM)
    IF NOT A THEN SUM=N ELSE SUM=SUM+N
END PROCEDURE

BEGIN
   PRINT(CHR$(12);) ! CLS
   ACCUMULATOR(X,1,FALSE->X)  ! INIT FIRST ACCUMULATOR
   ACCUMULATOR(X,-15,TRUE->X)
   ACCUMULATOR(X,2.3,TRUE->X)

   ACCUMULATOR(Z,3,FALSE->Z)  ! INIT SECOND ACCUMULATOR
   ACCUMULATOR(Z,5,TRUE->Z)
   ACCUMULATOR(Z,2.3,TRUE->Z)
   PRINT(X,Z)
END PROGRAM
```

{{out}}

```txt

-11.7 10.3

```



## Factor


```factor
USE: locals
:: accumulator ( n! -- quot ) [ n + dup n! ] ;

1 accumulator
[ 5 swap call drop ]
[ drop 3 accumulator drop ]
[ 2.3 swap call ] tri .
```



## Fantom

The accumulator function is a little unwieldy using multiple ifs to maintain the type of 'sum' until forced to change.  Again, a result of the three concrete Num types, Int, Float and Decimal, all being separated in the API.

```fantom
class AccumulatorFactory
{
  static |Num -> Num| accumulator (Num sum)
  {
    return |Num a -> Num| 
    { // switch on type of sum
      if (sum is Int)
      { // and then type of a
        if (a is Int)
          return sum = sum->plus(a)
        else if (a is Float)
          return sum = sum->plusFloat(a)
        else
          return sum = sum->plusDecimal(a)
      }
      else if (sum is Float)
      {
        if (a is Int)
          return sum = sum->plusInt(a)
        else if (a is Float)
          return sum = sum->plus(a)
        else
          return sum = sum->plusDecimal(a)
      }
      else // if (sum is Decimal)
      {
        if (a is Int)
          return sum = sum->plusInt(a)
        else if (a is Float)
          return sum = sum->plusFloat(a)
        else
          return sum = sum->plus(a)
      }
    }
  } 

  public static Void main () 
  {
    x := accumulator (3.1)
    y := accumulator (3f)
    echo (x(5))              // the Decimal sum combines with an Int
    echo (x(2))
    echo (y(5.1))            // the Float sum combines with a Decimal

    x = accumulator (1)
    x (5)
    accumulator (3)
    echo (x(2.3))          // the Int sum is now a Decimal
  }
}
```



## Forth

Forth is untyped; this works on integers.

```forth
: accumulator
  create ( n -- ) ,
  does> ( n -- acc+n ) tuck +! @ ;

0 accumulator foo

1 foo .  \ 1
2 foo .  \ 3
3 foo .  \ 6
```


The idiomatic way to deal with floats is to have a float version of this code; for a mixture of integers and floats, you decide at the start to use a float accumulator, and convert integers to floats explicitly:


```forth

: faccumulator ( r "name" -- )
  create falign f,
does> ( r1 -- r2 )
  faligned dup f@ f+ fdup f! ;

1 s>f faccumulator x
5 s>f x fdrop
3 s>f faccumulator y \ unused
2.3e x f.

```


=={{header|F Sharp|F#}}==
A statically typed version is not possible, but it is quite easy to write dynamically typed functions in F#:

```fsharp
// dynamically typed add
let add (x: obj) (y: obj) =
  match x, y with
  | (:? int as m), (:? int as n) -> box(m+n)
  | (:? int as n), (:? float as x)
  | (:? float as x), (:? int as n) -> box(x + float n)
  | (:? float as x), (:? float as y) -> box(x + y)
  | _ -> failwith "Run-time type error"

let acc init =
  let state = ref (box init)
  fun y ->
    state := add !state (box y)
    !state

do 
  let x : obj -> obj = acc 1
  printfn "%A" (x 5) // prints "6"
  acc 3 |> ignore
  printfn "%A" (x 2.3) // prints "8.3"
```


Actually, it is possible to create a statically typed version by using an inline accumulator creation function.

```fsharp
let inline makeAccumulator init =
    let acc = ref init
    fun i -> 
        acc := !acc + i
        !acc

do
    let acc = makeAccumulator 1.0 // create a float accumulator

    acc 5.0 |> ignore
    let _ = makeAccumulator 3 // create an unused integer accumulator
    printfn "%A" (acc 2.3)
```

{{out}}

```txt
8.3
```




## Fortran

Fortran does not have functions as first class objects, and can not create functions
at runtime.


###  Fortran77 

Fortran77 does not support objects and overloading and thus the user must declare the
type of the function to generate. The following are noted:

The code uses CPP which is at least available on the GNU compiler with the -cpp directive.

The code uses the semicolon as command separators. This was not standard in Fortran77
but was accepted by many compilers (some used colon instead).

The "data" command implies that the variables are static. This was not standard
in Fortran77 but was accepted by virtually all compilers.


```Fortran
#define foo(type,g,nn) \
typex function g(i);\
typex i,s,n;\
data s,n/0,nn/;\
s=s+i;\
g=s+n;\
end

      foo(real,x,1)
      foo(integer,y,3)

      program acc
      real x, temp
      integer y, itemp
      temp = x(5.0)
      print *, x(2.3)
      itemp = y(5)
      print *, y(2)
      stop
      end
```

{{out}}

```txt

   8.30000019
          10

```



###  Fortran2003 

Fortran2003 and later supports objects and overloading. The overloaded functions are encapsulated in an object.


```Fortran

module modAcc
implicit none
private
integer, public, parameter :: KRL = selected_real_kind(14)

type, public :: AccType
    real(KRL), private :: dn, dsum
    complex(KRL), private :: fn, fsum
    integer, private :: jn, jsum, icod
    contains
    procedure, private :: initd, initf, initi
    generic, public :: init => initd, initf, initi
    procedure, private :: dfun, ffun, jfun
    generic, public :: fun => dfun, jfun, ffun
end type AccType


contains

subroutine initd(self, dd)
    class(AccType), intent(inout) :: self
    real(KRL), intent(in) :: dd
    self%dn = dd
    self%icod = 1
end subroutine initd

subroutine initf(self, ff)
    class(AccType), intent(inout) :: self
    complex(KRL), intent(in) :: ff
    self%fn = ff
    self%icod = 2
end subroutine initf

subroutine initi(self, jj)
    class(AccType), intent(inout) :: self
    integer, intent(in) :: jj
    self%jn = jj
    self%icod = 3
end subroutine initi

real(KRL) function dfun(self, di)
    class(AccType), intent(inout) :: self
    real(KRL), intent(in) :: di
    self%dsum = self%dsum + di
    dfun = self%dn + self%dsum
end function dfun


complex(KRL) function ffun(self, fi)
    class(AccType), intent(inout) :: self
    complex(KRL), intent(in) :: fi
    self%fsum = self%fsum + fi
    ffun = self%fn + self%fsum
end function ffun


integer function jfun(self, ji)
    class(AccType), intent(inout) :: self
    integer, intent(in) :: ji
    self%jsum = self%jsum + ji
    jfun = self%jn + self%jsum
end function jfun

end module modAcc


program test
    use modAcc
    implicit none
    type(AccType) :: x, y
    integer :: itemp
    real(KRL) :: temp
    call x%init(1.0_KRL)
    temp = x%fun(5.0_KRL)
    call y%init(3)
    print *, x%fun(2.3_KRL)
    itemp = y%fun(5)
    print *, y%fun(2)
end program test
```

{{out}}

```txt

   8.3000000000000007     
          10

```



## FreeBASIC

It doesn't appear to be possible to program this task in FreeBASIC in the precise way it is posed.

The problem is that FB doesn't support closures and, whilst we can manufacture an equivalent object, we'd then have the further problem that you can't pass pointers to object methods, only to static procedures. 

To get around this restriction we'd normally wrap the object method in a static procedure and pass an object pointer to that followed by any other arguments required by the method. However, this won't work here because the task specifies that the method can take only a single number argument and the object pointer would be internal to 'foo' in any case. 

Probably the best we can do is for 'foo' to return the object and then to call the method 'g' directly on that:

```freebasic
' FB 1.05.0 Win64

' uses overloaded methods to deal with the integer/float aspect (long and single are both 4 bytes)
Type Bar
  Public:
    Declare Constructor(As Long)
    Declare Constructor(As Single)   
    Declare Function g(As Long) As Long
    Declare Function g(As Single) As Single
  Private:
    As Single sum_   '' can't be altered by external code
End Type

Constructor Bar(i As Long)
  sum_ = i
End Constructor

Constructor Bar(s As Single)
  sum_ = s
End Constructor

Function Bar.g(i As Long) As Long
  sum_ += i
  Return sum_  '' would round down to a Long if non-integral Singles had been added previously
End Function

Function Bar.g(s As Single) As Single
  sum_ += s
  Return sum_  
End Function

Function foo Overload(i As Long) As Bar  '' returns a Bar object rather than a pointer to Bar.g
  Dim b As Bar = Bar(i)
  Return b
End Function 

Function foo Overload(s As Single) As Bar  '' overload of foo to deal with Single argument
  Dim b As Bar = Bar(s)
  Return b
End Function 

Dim x As Bar = foo(1)  '' assigns Bar object to x
x.g(5)  '' calls the Long overload of g on the Bar object
foo(3)  '' creates a separate Bar object which is unused
print x.g(2.3) '' calls the Single overload of g on the Bar object and should print 1 + 5 + 2.3 = 8.3

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

 8.3

```



## Go

Small deviation on condition 2. The task specifies to handle all numeric types, and only int and float64 are shown here.  The technique would extend to all types just as easily, but Go has lots of numeric types and the program would be big.

```go
package main

import "fmt"

func accumulator(sum interface{}) func(interface{}) interface{} {
    return func(nv interface{}) interface{} {
        switch s := sum.(type) {
        case int:
            switch n := nv.(type) {
            case int:
                sum = s + n
            case float64:
                sum = float64(s) + n
            }
        case float64:
            switch n := nv.(type) {
            case int:
                sum = s + float64(n)
            case float64:
                sum = s + n
            }
        default:
            sum = nv
        }
        return sum
    }
}

func main() {
    x := accumulator(1)
    x(5)
    accumulator(3)
    fmt.Println(x(2.3))
}
```

{{out}}

```txt
8.3
```



## Golo


```golo
#!/usr/bin/env golosh
----
An accumulator factory example for Rosetta Code.
This one uses the box function to create an AtomicReference.
----
module rosetta.AccumulatorFactory

function accumulator = |n| {
  let number = box(n)
  return |i| -> number: accumulateAndGet(i, |a, b| -> a + b)
}

function main = |args| {
  let acc = accumulator(3)
  println(acc(1))
  println(acc(1.1))
  println(acc(10))
  println(acc(100.101))
}
```



## Groovy

Solution:

```groovy
def accumulator = { Number n ->
    def value = n;
    { it = 0 -> value += it}
}
```

Test:

```groovy
def x = accumulator(1)

println x()
assert x() instanceof Integer

println x(5)
assert x() instanceof Integer

def y = accumulator(3)
println y()
assert y() instanceof Integer

println x(2.3)
assert x() instanceof BigDecimal

println y(10)
assert y() instanceof Integer

println y(200L)
assert y() instanceof Long

println y(2.25D)
assert y() instanceof Double
```

{{out}}

```txt
1
6
3
8.3
13
213
215.25
```



## Haskell

{{trans|Ruby}}

```haskell
import Control.Monad.ST
import Data.STRef

accumulator :: (Num a) => a -> ST s (a -> ST s a)
accumulator sum0 = do
  sum <- newSTRef sum0
  return $ \n -> do
    modifySTRef sum (+ n)
    readSTRef sum

main :: IO ()
main = print foo
    where foo = runST $ do
                  x <- accumulator 1
                  x 5
                  accumulator 3
                  x 2.3
```

{{out}}

```txt
8.3
```


'''Note''' The <code>accumulator</code> function could be written in applicative style:

```haskell>accumulator = newSTRef >=
 return . factory
  where factory s n = modifySTRef s (+ n) >> readSTRef s
```


=={{header|Icon}} and {{header|Unicon}}==
At first glance you might expect the example below to run under Icon; however, as the co-expression calling sequence is Unicon specific.

Strictly speaking, <tt>genAcc(n)</tt> returns a <i>co-expression</i>, not a function.  However, the invocation syntax here is indistinguishable from calling a function.

```Unicon
procedure main()
    a := genAcc(3)
    b := genAcc(5)
    
    write("        " ,center("a",5),  " ", center("b", 5))
    write("genAcc: ", right(a(4),5),  " ", right(b(4), 5))
    write("genAcc: ", right(a(2),5),  " ", right(b(3),5))
    write("genAcc: ", right(a(4.5),5)," ", right(b(1.3),5))
end

procedure genAcc(n)   # The generator factory
    return makeProc { while i := (n@&source)[1] do n +:= i }
end

procedure makeProc(A) # A Programmer-Defined Control Operation
    return (@A[1],A[1])
end
```

This example produces the output:

```txt

          a     b  
genAcc:     7     9
genAcc:     9    12
genAcc:  13.5  13.3

```

To adapt the above for use in Icon, the function-syntax for activating co-expressions (e.g. <tt>a(4)</tt>) available in Unicon would have to be replaced with the <i>activation</i> operator (e.g. <tt>[4]@a</tt>).  The use of a list as the value passed through activation is to retain compatibility with the Unicon approach.


## Io


```Io
accumulator := method(sum,
    block(x, sum = sum + x) setIsActivatable(true)
)
x := accumulator(1)
x(5)
accumulator(3)
x(2.3) println  // --> 8.3000000000000007
```



## J

See http://www.jsoftware.com/jwiki/Guides/Lexical_Closure, including the [[j:Guides/Lexical%20Closure#dissent|dissent]] section.

```J
oleg=:1 :0
  a=. cocreate''
  n__a=: m
  a&(4 : 'n__x=: n__x + y')
)
```

Example use: 

```j
   F=: 10 oleg
   F 11
21
   F 12
33
   F 11
44
```



## Java

Java has no first-class functions, so an accumulator can't use the <code>x(5)</code> syntax.  The standard syntactic workaround is to use a standard method name, like <code>x.call(5)</code> or <code>x.apply(5)</code>.  This is a deviation from task.

Our accumulator sums with long integers as far as possible before switching to floats.  This requires the use of the <code>Number</code> class.  The code needs Java 5 to autobox primitive values <code>1</code> or <code>2.3</code> into instances of Number.  The <code>apply</code> method is ready to implement interface UnaryOperator in Java 8.

{{works with|Java|5 and up}}

```java
public class Accumulator
    //implements java.util.function.UnaryOperator<Number> // Java 8
{
    private Number sum;

    public Accumulator(Number sum0) {
	sum = sum0;
    }

    public Number apply(Number n) {
	// Acts like sum += n, but chooses long or double.
	// Converts weird types (like BigInteger) to double.
	return (longable(sum) && longable(n)) ?
	    (sum = sum.longValue() + n.longValue()) :
	    (sum = sum.doubleValue() + n.doubleValue());
    }

    private static boolean longable(Number n) {
	return n instanceof Byte || n instanceof Short ||
	    n instanceof Integer || n instanceof Long;
    }

    public static void main(String[] args) {
	Accumulator x = new Accumulator(1);
	x.apply(5);
	new Accumulator(3);
	System.out.println(x.apply(2.3));
    }
}

```

{{out}}

```txt
8.3
```


A printed Accumulator would look like <code>Accumulator@42e816</code>

Java 8 added the lambda syntax.  A lambda is an anonymous inner class that implements a one-method interface.  We can make the accumulator as a lambda, but it must store the sum in another object.  We use a one-element array.

{{works with|Java|8 and up}}

```java
import java.util.function.UnaryOperator;

public class AccumulatorFactory {

    public static UnaryOperator<Number> accumulator(Number sum0) {
	// Allows sum[0] = ... inside lambda.
	Number[] sum = { sum0 };

	// Acts like n -> sum[0] += n, but chooses long or double.
	// Converts weird types (like BigInteger) to double.
	return n -> (longable(sum[0]) && longable(n)) ?
	    (sum[0] = sum[0].longValue() + n.longValue()) :
	    (sum[0] = sum[0].doubleValue() + n.doubleValue());
    }

    private static boolean longable(Number n) {
	return n instanceof Byte || n instanceof Short ||
	    n instanceof Integer || n instanceof Long;
    }

    public static void main(String[] args) {
	UnaryOperator<Number> x = accumulator(1);
	x.apply(5);
	accumulator(3);
	System.out.println(x.apply(2.3));
    }
}
```



## JavaScript


### ES5


```javascript
function accumulator(sum) {
  return function(n) {
    return sum += n;
  }
}
var x = accumulator(1);
x(5);
console.log(accumulator(3).toString() + '
');
console.log(x(2.3));
```

{{out}}

```txt
function (n) { return sum += n; }
8.3
```



### ES6


```javascript>let accumulator = sum =
 (n => sum += n);
let x = accumulator(1);
console.log(x(5));
accumulator(3);
console.log(x(2.3));
```

{{out}}

```txt
6
8.3
```


===JavaScript 1.8 (SpiderMonkey Only)===

```javascript
function accumulator(sum) function(n) sum += n;
var x = accumulator(1);
x(5);
console.log(accumulator(3).toSource());
console.log(x(2.3));
```

{{out}}

```txt
(function (n) sum += n)
8.3
```



## Jsish

From Javascript ES5 entry.

```javascript
/* Accumulator factory, in Jsish */
function accumulator(sum) {
    return function(n) {
        return sum += n;
    };
}

provide('accumulatorFactory', '0.6');

if (Interp.conf('unitTest')) {
var x,y;
;x = accumulator(1);
;accumulator;
;x;
;x(5);
;accumulator(3);
;x(2.3);

;y = accumulator(0);
;y;
;x(1);
;y(2);
;x(3);
;y(4);
;x(5);
}

/*
=!EXPECTSTART!=
x = accumulator(1) ==> "function(n) {\n        return sum += n;\n    }"
accumulator ==> "function accumulator(sum) {\n    return function(n) {\n        return sum += n;\n    };\n}"
x ==> "function(n) {\n        return sum += n;\n    }"
x(5) ==> 6
accumulator(3) ==> "function(n) {\n        return sum += n;\n    }"
x(2.3) ==> 8.3
y = accumulator(0) ==> "function(n) {\n        return sum += n;\n    }"
y ==> "function(n) {\n        return sum += n;\n    }"
x(1) ==> 9.3
y(2) ==> 2
x(3) ==> 12.3
y(4) ==> 6
x(5) ==> 17.3
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u accumulatorFactory.jsi
[PASS] accumulatorFactory.jsi
```



## Julia

{{works with|Julia|0.6}}


```julia
function accumulator(i)
	f(n) = i += n
	return f
end

x = accumulator(1)
@show x(5)

accumulator(3)
@show x(2.3)
```


{{out}}

```txt
x(5) = 6
x(2.3) = 8.3
```



## Kotlin

Overloads would be needed for all six primitive numeric types but, in the interests of brevity, only two overloads of 'foo' have been coded: 

```scala
// version 1.1

fun foo(n: Double): (d: Double) -> Double {
    var nn = n
    return { nn += it; nn }
}

fun foo(n: Int): (i: Int) -> Int {
    var nn = n
    return { nn += it; nn }
}

fun main(args: Array<String>) {
    val x = foo(1.0) // calls 'Double' overload
    x(5.0)
    foo(3.0)
    println(x(2.3))
    val y = foo(1)   // calls 'Int' overload
    y(5)
    foo(5)
    println(y(2))
}
```


{{out}}

```txt

8.3
8

```



## LFE


LFE doesn't support mutable data (nor global variables); as such, this task requires a work-around. There are two ways to accomplish it: via closure on anonymous function, or closure on a process.


###  Traditional closure 



```lisp

(defun accum (m)
    (lambda (n)
      (let ((sum (+ m n)))
        `(#(func ,(accum sum))
          #(sum ,sum)))))

```


Since we want to use both the returned function as well as the data for the call, we return a tuple containing both. Using standard LFE pattern matching, we can extract these.

Usage (in the REPL):


```txt

> (set x (accum 1))
#Fun<lfe_eval.12.122728658>
> (set `(#(func ,x) ,_) (funcall x 5))
(#(func #Fun<lfe_eval.12.122728658>) #(sum 6))
> (funcall x 3)
(#(func #Fun<lfe_eval.12.122728658>) #(sum 9))
> (set `(#(func ,x) ,_) (funcall x 2.3))
(#(func #Fun<lfe_eval.12.122728658>) #(sum 8.3))

```


Note that we want to re-set the variable <code>x</code> with each call in order to use its updated state (since LFE is a functional programming language which doesn't support mutable global variables.


###  Process closure 


We can creating a looping process which provides the same functionality as the self-calling function in the "traditional closure" approach:


```lisp

(defun loop (m)
  (receive
    (`#(,caller ,n)
     (let ((sum (+ m n)))
       (! caller sum)
       (loop sum)))))

(defun accum (m)
  (let ((loop-pid (spawn (lambda () (loop m)))))
    (lambda (n)
      (! loop-pid `#(,(self) ,n))
      (receive
        (sum sum)))))

```


Usage (in the REPL):


```txt

> (accum 1)
#Fun<lfe_eval.12.122728658>
> (set x (accum 1))
#Fun<lfe_eval.12.122728658>
> (funcall x 5)
6
> (accum 3)
#Fun<lfe_eval.12.122728658>
> (funcall x 2.3)
8.3

```


Since we're using a looping process to track state, there's no need to re-set the <code>x</code> variable with each call.


## Lua

A simple implementation:

```Lua
function acc(init)
  init = init or 0
  return function(delta)
    init = init + (delta or 0)
    return init
  end
end
```

An expanded example of similar but more complex functionality:
{{works with|Lua|5.1}}

```lua
do
    local accSum = 0;               -- accumulator factory 'upvalue'
    function acc(v)                 -- the accumulator factory
        accSum = accSum + (v or 0)  -- increment factory sum
        
        local closuredSum = accSum;               -- new 'upvalue' at each factory call
        return function (w)                       -- the produced accumulator function
            closuredSum = closuredSum + (w or 0)  -- increment product 'upvalue'
            return closuredSum                    -- return 'upvalue'
        end, accSum                               -- end of product closure
        
    end--acc
end--end of factory closure
```

Usage example:

```lua
x = acc(1)                 -- x stores the product with initial value = 1
x(5)                       -- add 5 to x's sum
acc(3)                     -- add 3 to factory's sum
print (x(2.3))  --> 8.3    -- add 2.3 to x's sum then print the result
y = acc()                  -- create new function with factory's sum as initial value
print (y())     --> 4      -- print the accumulated value inside the product y
```



## M2000 Interpreter



<lang >\\ M2000 Interpreter
\\ accumulator factory
foo=lambda acc=0 (n as double=0) -> {
      \\ interpreter place this: read n as double=0 as first line of lambda function
      if n=0 then =acc : exit 
      acc+=n
      \\ acc passed as a closuer to lambda (a copy of acc in the result lambda function)
      =lambda acc -> {
            ' if stack of values is empty then return a copy of acc
            if empty then =acc : exit
            read x
            \\ x has no type here, can be any numeric type (also can be an object too)       
            \\ accumulator is double, and is a closure (a copy of acc in foo)
            acc+=x
            \\ any variable in M2000 hold  first type
            \\ if x is an object then we get error, except if object use this operator
            x=acc
            \\ so we return x type
            =x
      }
}
x=foo(1&)   ' 1& is long type (32bit)
call void x(5) ' 5 is double type (the default type for M2000)
call void foo(3#)   ' void tell to interpreter to throw result, 3# is Currency type
print x(2.3@) ' print 8.3,   2.3@ is Decimal type
print foo()=4 ' print true
def ExpType$(z)=type$(z)
print ExpType$(foo())="Double"
print ExpType$(x(0&))="Long"
print ExpType$(x(0@))="Decimal"
print ExpType$(x())="Double"
print ExpType$(foo(20))="lambda"

```



## Maple

This creates a procedure closed over the local variable total in the factory procedure.  The initial value, if not passed to the factory procedure, is taken to be 0 and, if the generated accumulator is given no value, it increments the total by 1.

```Maple
AccumulatorFactory := proc( initial := 0 )
        local   total := initial;
        proc( val := 1 ) total := total + val end
end proc:
```

Running this, we get:

```Maple>
 acc := AccumulatorFactory( 1 ):
> acc( 5 );
                                     6

> AccumulatorFactory( 3 ):
> acc( 2.3 );
                                     8.3

> acc(); # use the default increment of 1
                                     9.3

> acc( 3 - 4 * I ); # also handles complex numbers
                                     12.3 - 4. I

> acc( I ); # add the imaginary unit
                                     12.3 - 3. I
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
accFactory[initial_] := 
  Module[{total = initial},
    Function[x, total += x]
  ]
x=accFactory[1];
x[5.0];
accFactory[3];
x[2.3]
```

{{out}}

```txt
8.3
```



## Mercury


===Strict-adherence-to-the-task solution===

Deviations:

1. this doesn't work with "any numerical type" out of the box, but requires that users add numerical types to a typeclass.

2. this likely violates some hidden taste requirements of the task, as used by Paul Graham to dismiss Forth solutions. Certainly, this is not really an example of Mercury that anyone would want to use in a Mercury project.


```Mercury
:- module accum.
:- interface.

:- typeclass addable(T) where [
    func T + T = T
].

:- impure func gen(T) = (impure (func(T)) = T) <= addable(T).

:- implementation.
:- import_module bt_array, univ, int.

:- mutable(states, bt_array(univ), make_empty_array(0), ground, [untrailed]).

gen(N) = F :-
    some [!S] (
        semipure get_states(!:S),
        size(!.S, Size),
        resize(!.S, 0, Size + 1, univ(N), !:S),
        impure set_states(!.S)
    ),
    F = (impure (func(Add)) = M :-
        some [!SF] (
            semipure get_states(!:SF),
            !.SF ^ elem(Size) = U,
            det_univ_to_type(U, M0),
            M = M0 + Add,
            !SF ^ elem(Size) := univ(M),
            impure set_states(!.SF)
        )).
```


As used:


```Mercury
:- module accumuser.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module accum, list, string, int, float.

:- instance addable(int) where [
    A + B = int.(A + B)
].

:- instance addable(float) where [
    A + B = float.(A + B)
].

:- pragma promise_pure main/2.
main(!IO) :-
    impure F = accum.gen(1),
    impure N1 = impure_apply(F, 1),
    impure N2 = impure_apply(F, 1),
    impure G = accum.gen(500.0),
    impure R1 = impure_apply(G, -10.0),
    impure R2 = impure_apply(G, -50.0),
    io.format("%d, %d\n", [i(N1), i(N2)], !IO),
    io.format("%.0f, %.0f\n", [f(R1), f(R2)], !IO).
```


{{out}}

```txt

2, 3
490, 440

```



### Realistic solution


Deviations:

1. This still requires addition of numeric types to a typeclass, for a generic +

2. This doesn't return a closure with mutable state, but the state itself, which the caller can thread through rules that apply to them.


```Mercury
:- module accum2.
:- interface.

:- typeclass addable(T) where [
    func T + T = T
].

:- type accum(T).

    % init(N) = Acc
    % Return an accumulator with initial value of N
    %
:- func init(T) = accum(T)
    <= addable(T).

    % bump(By, N, !Acc)
    % Add By to accumulator !Acc, yielding the next number as N
    %
:- pred bump(T::in, T::out, accum(T)::in, accum(T)::out) is det
    <= addable(T).

:- implementation.

:- type accum(T) == T.

init(N) = N.

bump(X, N, N0, N) :-
    N = X + N0.
```


As used, with the same output:


```Mercury
:- module accumuser2.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module accum2, list, string, int, float.

:- instance addable(int) where [
    A + B = int.(A + B)
].

:- instance addable(float) where [
    A + B = float.(A + B)
].

main(!IO) :-
    some [!A1] (
        !:A1 = accum2.init(1),
        accum2.bump(1, N1, !A1),
        accum2.bump(1, N2, !.A1, _)
    ),
    some [!A2] (
        !:A2 = accum2.init(500.0),
        accum2.bump(-10.0, R1, !A2),
        accum2.bump(-50.0, R2, !.A2, _)
    ),
    io.format("%d, %d\n", [i(N1), i(N2)], !IO),
    io.format("%.0f, %.0f\n", [f(R1), f(R2)], !IO).
```



## Nemerle

Nemerle doesn't have a <tt>dynamic</tt> type, but we can use matching to bind types to <tt>object</tt>s.

```Nemerle
def Foo(n) {
    mutable value : object = n;
    fun (i : object) { 
        match(i) {
            |x is int    => match(value) {
                                |y is int => value = x + y;
                                |y is double => value = x + y;
                            }
            |x is double => match(value) {
                                |y is int => value = x + (y :> double);
                                |y is double => value = x + y;
                            }
        }
        value 
    }
}

def x = Foo(1);
def y = Foo(2.2);
x(5);
System.Console.WriteLine(x(2.3));
System.Console.WriteLine(y(3));
```

Output:

```txt
8.3
5.2
```



## NewLisp


```NewLisp
(define (sum (x 0)) (inc 0 x))

```


{{out}}

```txt

> (define (sum (x 0)) (inc 0 x))
(lambda ((x 0)) (inc 0 x))
> (sum 1)
1
> (sum 1)
2
> (sum 1)
3
> (sum 1.4)
4.4
> (sum 1.4)
5.8
> (sum 1.8)
7.6
>

```



## NGS


```NGS
{
	F Acc(start:Int) {
		sum = start
		F acc(i:Int) {
			sum = sum + i
			sum
		}
	}

	acc = Acc(10)
	echo(acc(5))
	echo(acc(2))
}
```

{{out}}

```txt
15
17
```



## Nim


```nim
proc accumulator(sum: float): auto =
  var sum = sum
  return proc (n: float): float =
    sum += n
    return sum

var x = accumulator(1)
echo x(5) # 6
echo x(2.3) # 8.3

var y = accumulator(1)
echo y(5) # 6
echo y(2.3) # 8.3

var z = accumulator(3)
echo z(5) # 8
echo z(2.3) # 10.3
echo x(0) # 8.3
echo z(0) # 10.3
```

Output:

```txt
6.0000000000000000e+00
8.3000000000000007e+00
6.0000000000000000e+00
8.3000000000000007e+00
8.0000000000000000e+00
1.0300000000000001e+01
8.3000000000000007e+00
1.0300000000000001e+01
```



## Nit


Source: [https://github.com/nitlang/nit/blob/master/examples/rosettacode/accumulator_factory.nit the official Nit repository]


```nit
# The `accumulator factory` task.
#
# Nit has no first-class function.
# A class is used to store the state.
module accumulator_factory

class Accumulator
	# The accumulated sum
	# Numeric is used, so Int and Float are accepted
	private var sum: Numeric
	fun call(n: Numeric): Numeric
	do
		# `add` is the safe `+` method on Numeric
		sum = sum.add(n)
		return sum
	end
end

var x = new Accumulator(1)
x.call(5)
var y = new Accumulator(3)
print x.call(2.3)
```


Output:

```txt
8.3
```



## Objeck

Uses objects instead of first class functions.

```objeck
bundle Default {
  class Accumulator {
    @sum : Float;
      
    New(sum : Float) {
      @sum := sum;
    }
      
    method : public : Call(n : Float) ~ Float {
      @sum += n;
      return @sum;
    }
      
    function : Main(args : String[]) ~ Nil {
      x := Accumulator->New(1.0);
      x->Call(5.0  );
      x->Call(2.3)->PrintLine();
    }
  }
}
```


=={{header|Objective-C}}==
{{works with|Mac OS X|10.6+}}

```objc>#import <Foundation/Foundation.h


typedef double (^Accumulator)(double);

Accumulator accumulator_factory(double initial) {
    __block double sum = initial;
    Accumulator acc = ^(double n){
        return sum += n;
    };
    return acc;
}

int main (int argc, const char * argv[]) {
    @autoreleasepool {

        Accumulator x = accumulator_factory(1);
        x(5);
        accumulator_factory(3);
        NSLog(@"%f", x(2.3));
	
    }
    return 0;
}
```

{{out}}

```txt
8.300000
```



## OCaml

{{trans|Ruby}}
Deviations: An accumulator instance can take ''either'' integers ''or'' floats, but not both mixed (due to lack of runtime polymorphism).

```ocaml
let accumulator sum0 =
  let sum = ref sum0 in
  fun n ->
    sum := !sum +. n;
    !sum;;

let _ =
  let x = accumulator 1.0 in
  ignore (x 5.0);
  let _ = accumulator 3.0 in
  Printf.printf "%g\n" (x 2.3)
;;
```

{{out}}

```txt
8.3
```



## Octave



```octave
# not a function file:
1;
function fun = foo(init)
  currentSum = init;
  fun = @(add) currentSum = currentSum + add; currentSum;
endfunction

x = foo(1);
x(5); 
foo(3);
disp(x(2.3));
```



## Oforth


Oforth can only returns blocks, not functions, but a block can be used wherever a function is used.

The block returned by foo (a closure), when performed, retrieves the current value from the closure parameter, adds the top of stack, and stores the result back to the closure's parameter. The result is dup, so it is also returned.


```Oforth
: foo( n -- bl )
   #[ n swap + dup ->n ] ;
```


Usage :

```Oforth
: testfoo 
| x y z |
   1 foo ->x
   5 x perform .
   3 foo ->y
   2.3 x perform dup . ", x accumulator value is a" . class .cr
   10  y perform dup . ", y accumulator value is a" . class .cr
   "aaa" foo ->z
   "bbb" z perform dup . ", z accumulator value is a" . class .cr 
;
```


{{out}}

```txt

>testfoo
6 8.3 , x accumulator value is a #Float
13 , y accumulator value is a #Integer
aaabbb , z accumulator value is a #String
ok
```



## ooRexx

ooRexx does not have functions that can maintain state between calls.  The standard work around is to use an object instance and a defined method name. 

```ooRexx

x = .accumulator~new(1)    -- new accumulator with initial value of "1"
x~call(5)
x~call(2.3)
say "Accumulator value is now" x    -- displays current value

-- an accumulator class instance can be instantiated and
-- used to sum up a series of numbers
::class accumulator
::method init    -- instance initializer...sets the accumulator initial value
  expose sum
  use strict arg sum = 0 -- sets default sum value if not specified

-- perform the accumulator function
::method call
  expose sum
  use strict arg n
  sum += n       -- bump the accumulator
  return sum     -- return the new value

-- extra credit...display the current accumulator value
::method string
  expose sum
  return sum


```



## OxygenBasic


```txt


Class AccumFactory
'
### ===========


  double v

  method constructor()
  end method

  method destructor()
  end method

  method Accum(double n) as AccumFactory
  new AccumFactory af
  af.v=v+n
  return af
  end method

  method FloatValue() as double
  return v
  end method

  method IntValue() as sys
  return v
  end method

  method StringValue(sys dp=16) as string
  return str v,dp
  end method
 

end class

'
### =================

'TESTS (all results: PI)
'
### =================


new AccumFactory af

'GENERATE ACCUMULATORS

let a=af.Accum(1)   'integer
let b=a.Accum(pi)   'float
let c=b.Accum("-1") 'string

'STRING OUTPUT

print c.StringValue(4) ' show 4 decimal places

'FLOAT OUTPUT

print c.FloatValue

'USE FUNCTIONS IN EXPRESSION

print 10 * c.FloatValue() / ( 10 * a.IntValue() )

'FINISH

del af : del a : del b : del c

```




## Oz

A bit unwieldy because the '+' operator does not allow mixed type operands. The implementation is thread-safe (atomic Exchange operation).

```oz
declare
  fun {Acc Init}
     State = {NewCell Init}
  in
     fun {$ X}
        OldState
     in
        {Exchange State OldState} = {Sum OldState X}
     end
  end

  fun {Sum A B}
     if {All [A B] Int.is} then A+B
     else {ToFloat A}+{ToFloat B}
     end
  end

  fun {ToFloat X}
     if {Float.is X} then X
     elseif {Int.is X} then {Int.toFloat X}
     end
  end

  X = {Acc 1}
in
  {X 5 _}
  {Acc 3 _}
  {Show {X 2.3}}
```



## PARI/GP



```parigp
stack = List([1]);
factory(b,c=0) = my(a=stack[1]++);listput(stack,c);(b)->stack[a]+=b;

foo(f) = factory(0, f);     \\ initialize the factory
```


Run the factory:
```txt
gp > x = foo(1);
gp > x(5);
gp > y = foo(3);
gp > print(x(2.3));
8.300000000000
gp > print(y(1));
4
gp > print(x(1));
9.300000000000
gp > print(y(1/3));
13/3
```



## Perl

There's a little deviation: the syntax <code>$x->(5)</code> differs from the usual <code>x(5)</code>.

{{trans|Ruby}}

```perl
sub accumulator {
  my $sum = shift;
  sub { $sum += shift }
}

my $x = accumulator(1);
$x->(5);
accumulator(3);
print $x->(2.3), "\n";
```

{{out}}

```txt
8.3
```



## Perl 6

{{works with|Rakudo|2018.03}}

```perl6
sub accum ($n is copy) { sub { $n += $^x } }

#Example use:
my $a = accum 5;
$a(4.5);
say $a(.5);   # Prints "10".

# You can also use the "&" sigil to create a function that behaves syntactically
# like any other function (i.e. no sigil nor parentheses needed to call it):

my &b = accum 5;
say b 3;   # Prints "8".
```



## Phix

Emulated. There is nothing clever about this - both the answer and the task requirements!

Numeric polymorphism is inherently supported in phix. While technically this does not return 
a function, the following demonstrates how the "standard_function" can be invoked in exactly
the same manner as a result from the factory, without the caller knowing which is which, and
I would guess that is one of the more important motivations for the original task. But it is
worth stating there are much easier ways to do this, hence generally speaking this approach 
is not particularly recommended or advocated.

A variation on [[Closures/Value_capture#Phix]], only in this case the inner function is kept
in the returned variable and for simplicity there are no partial args - but it would be easy 
enough to add that sort of flexibility here if needed.

Rule#5 is deliberately ignored: if rogue code can corrupt the accumulators variable, it can 
just as easily corrupt the "closure" it would otherwise be held in, however well-hidden some 
other programming language would like to pretend that is, and of course the latter sort of
corruption would be <i>significantly</i> harder to debug. Obviously, for safety, you would
normally make the accumulators variable private(/non-global) in a separate source file,
along with accumulate/accumulate_factory/call_function, and if you <i>really</i> don't like
accumulators being visible (??) I suppose you could always just allocate a bit of memory in 
accumulator_factory() and return a pointer to that instead of an id/length.

```Phix
sequence accumulators = {}

function accumulate(integer id, atom v)
    accumulators[id] += v
    return accumulators[id]
end function
constant r_accumulate = routine_id("accumulate")

function accumulator_factory(atom initv=0)
    accumulators = append(accumulators,initv)
    return {r_accumulate,length(accumulators)}
end function

function call_function(object rid, object args)
    if sequence(rid) then
        {rid, integer id} = rid
        args = id&args
    end if
    return call_func(rid,args)
end function

function standard_function()
    return "standard function"
end function
constant r_standard_function = routine_id("standard_function")

constant x = accumulator_factory(1),
         y = accumulator_factory(3)
{} = call_function(x,5)
{} = call_function(y,3)
?call_function(x,2.3)
?call_function(y,4)
?call_function(r_standard_function,{})
```

{{out}}

```txt

8.3
10
"standard function"

```



## PHP


```PHP
<?php
function accumulator($start){
 return create_function('$x','static $v='.$start.';return $v+=$x;');
}
$acc = accumulator(5);
echo $acc(5), "\n"; //prints 10
echo $acc(10), "\n"; //prints 20
?>
```

{{works with|PHP|5.3+}}

```php
<?php
function accumulator($sum){
 return function ($x) use (&$sum) { return $sum += $x; };
}
$acc = accumulator(5);
echo $acc(5), "\n"; //prints 10
echo $acc(10), "\n"; //prints 20
?>
```



## PicoLisp


```PicoLisp
(de accumulator (Sum)
   (curry (Sum) (N)
      (inc 'Sum N) ) )

(def 'a (accumulator 7))
(a 1)  # Output: -> 8
(a 2)  # Output: -> 10
(a -5)  # Output: -> 5
```



## Pony



```Pony

use "assert"
class Accumulator
    var value:(I64|F64)
    new create(v:(I64|F64))=>
        value=v
    fun ref apply(v:(I64|F64)=I64(0)):(I64|F64)=>
        value=match value
        | let x:I64=>match v
            | let y:I64=>x+y
            | let y:F64=>x.f64()+y
            end
        | let x:F64=>match v
            | let y:I64=>x+y.f64()
            | let y:F64=>x+y
            end
        end
        value

actor Main
    new create(env:Env)=>
        var r:Accumulator=Accumulator(I64(0))
        r(I64(5))
        r(I64(2))
        try
        Fact(match r()
            |let x:I64=>x==7
            |let y:F64=>y==7.0
            end)?
        env.out.print("The value I have so far is " + r().string())
        else
            env.out.print("An error of some sort happened!")
        end
        r(F64(5.5))
        env.out.print("This is okay..." + r().string())

```



## PostScript


```PostScript
/mk-acc  {             % accumulator generator
  {0 add 0 0 2 index put}
  7 array copy
  dup 0 4 -1 roll put
  dup dup 2 exch put
  cvx
}  def

% Examples (= is a printing command in PostScript):
/a 1 mk-acc def        % create accumulator #1, name it a
5 a =                  % add 5 to 1, print it
10 mk-acc              % create accumulator #2, leave it anonymous on the stack
2.71 a =               % add 2.71 to 6, print it
dup 3.14 exch exec =   % add 3.14 to 10, print it
dup 100 exch exec =    % add 100 to 13.14, print it
12 a =                 % add 12 to 8.71, print it
                       % accumulator #2 is still available on the stack
```



## PowerShell

Wikipedia says, “In programming languages, a closure is a function or reference to a function together with a referencing
environment. A closure—unlike a plain function pointer—allows a function to access those non-local variables even when invoked
outside its immediate lexical scope.”

The GetNewClosure method returns a ScriptBlock with captured variables.

```PowerShell

function Get-Accumulator ([double]$Start)
{
    {param([double]$Plus) return $script:Start += $Plus}.GetNewClosure()
}

```


```PowerShell

$total = Get-Accumulator -Start 1
& $total -Plus 5.0 | Out-Null
& $total -Plus 2.3

```

{{Out}}

```txt

8.3

```



## Prolog

{{works with|SWI Prolog}}
Uses the module '''lambda''' written by  '''Ulrich Neumerkel'''.

```Prolog
:- use_module(library(lambda)).

define_g(N, G) :-
	put_attr(V, user, N),
	G = V +\X^Y^(get_attr(V, user, N1),
		  Y is X + N1,
		  put_attr(V, user, Y)).

accumulator :-
	define_g(1, G),
	format('Code of g : ~w~n', [G]),
	call(G, 5, S),
	writeln(S),
	call(G, 2.3, R1),
	writeln(R1).
```

{{out}}

```txt
8 ?- accumulator.
Code of g : _G275+\_G285^_G288^ (get_attr(_G275,user,_G296),_G288 is _G285+_G296,put_attr(_G275,user,_G288))
6
8.3
true.
```



## Python

{{works with|Python|2.x/3.x}}

```python>>>
 def accumulator(sum):
  def f(n):
    f.sum += n
    return f.sum
  f.sum = sum
  return f

>>> x = accumulator(1)
>>> x(5)
6
>>> x(2.3)
8.3000000000000007
>>> x = accumulator(1)
>>> x(5)
6
>>> x(2.3)
8.3000000000000007
>>> x2 = accumulator(3)
>>> x2(5)
8
>>> x2(3.3)
11.300000000000001
>>> x(0)
8.3000000000000007
>>> x2(0)
11.300000000000001
```


{{trans|Ruby}}
{{works with|Python|3.x}}

```python
def accumulator(sum):
  def f(n):
    nonlocal sum
    sum += n
    return sum
  return f

x = accumulator(1)
x(5)
print(accumulator(3))
print(x(2.3))
```

{{out}}

```txt
<function f at 0xb7c2d0ac>
8.3
```


{{works with|Python|2.5+}}

```python
def accumulator(sum):
  while True:
    sum += yield sum

x = accumulator(1)
x.send(None)
x.send(5)
print(accumulator(3))
print(x.send(2.3))
```

{{out}}

```txt
<generator object accumulator at 0x106555e60>
8.3
```



## R


```R
accumulatorFactory <- function(init) {
  currentSum <- init
  function(add) {
    currentSum <<- currentSum + add
    currentSum
  }
}
```

{{out}}

```txt

> f <- accumulatorFactory(1)
> f(5)
[1] 6
> f(2.3)
[1] 8.3

```



## Racket


```racket
#lang racket
(define ((accumulator n) i)
  (set! n (+ n i))
  n)

```



## REBOL


```rebol
make-acc-gen: func [start-val] [
    use [state] [
        state: start-val
        func [value] [
            state: state + value
        ]
    ]
]
```

{{out}}

```txt
>> x: make-acc-gen 1
>> x 5
== 6
>> make-acc-gen 3
>> print x 2.3
8.3
```



## Retro

Retro only supports integers.

```Retro
  : acc here swap , [ &+! &@ bi ] curry ;

  ( create an accumulator function )
  1 acc

  ( and give it a name )
  constant x

  ( add values to it and display the results )
  5 x do putn
  2 x do putn
```



RETRO 12, 2018.4


```Retro
:acc (ns-)
  d:create , [ [ fetch ] [ v:inc ] bi ] does ;
```

{{out}}

```txt
  #10 'foo acc
  foo
  foo
  foo

dump-stack

10 11 12
Ok
```



## REXX

This REXX program is partially modeled after the ooRexx example.


This example will handle any kind of number: integer, floating point.

```rexx
/*REXX program shows one method an   accumulator factory   could be implemented.        */
x=.accumulator(1)                                /*initialize accumulator with a 1 value*/
x=call(5)
x=call(2.3)
say '          X value is now'   x               /*displays the current value of   X.   */
say 'Accumulator value is now'  sum              /*displays the current value of  accum.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
.accumulator:  procedure expose sum;   if symbol('SUM')=="LIT"  then sum=0   /*1st time?*/
               sum=sum + arg(1)                                              /*add──►sum*/
               return sum
/*──────────────────────────────────────────────────────────────────────────────────────*/
call:  procedure expose sum;  sum=sum+arg(1);    return sum          /*add arg1 ──► sum.*/
```

'''output''' 

```txt

          X value is now 8.3
Accumulator value is now 8.3

```



## Ring


```ring
oGenerator = new Generator

Func main
   oGenerator {
       accumulator = generator(1)
       see call accumulator(5)
       see nl
       generator(3)
       see call accumulator(2.3)
   }

Class Generator
    aN = []  

    func generator i
    aN + i
        return eval(substr("return func d {                          
           oGenerator {
               aN[#id#] += d
               return aN[#id#]
           }
       }","#id#",string(len(aN))))
```


{{out}}

```txt

6
8.30

```



## Ruby

Ruby deviates from the task because methods and Proc objects have different syntax.  So, <tt>x = accumulator(1)</tt> is valid, but <tt>x(5)</tt> is an error: the syntax must be <tt>x.call(5)</tt> or <tt>x[5]</tt> (with square brackets). Ruby 1.9 also allows <tt>x.(5)</tt> (with an extra dot).


```ruby
def accumulator(sum)
  lambda {|n| sum += n}
end

# mixing Integer and Float
x = accumulator(1)
x.call(5)
accumulator(3)
puts x.call(2.3)  # prints 8.3
```


The output of <tt>p accumulator(3)</tt> looks like


```txt

#<Proc:0x0000000207ba7f30@/tmp/accumulator.rb:2>         # Ruby 1.8.6
#<Proc:0x000002060d1788@/tmp/accumulator.rb:2 (lambda)>  # Ruby 1.9.2

```


This accumulator also works with other types that have a <tt>+</tt> method.


```ruby
require 'rational'
require 'complex'
y = accumulator(Rational(2, 3))
puts y[Rational(1, 2)]  # 7/6
puts y[4]               # 31/6
puts y[Complex(0, 1)]   # 31/6+1i

t = accumulator(Time.utc(1999, 8, 7, 6, 5))
                       # (Ruby 1.8.6)                  (Ruby 1.9.2)
puts t[4]              # Sat Aug 07 06:05:04 UTC 1999  1999-08-07 06:05:04 UTC
puts t[-12 * 60 * 60]  # Fri Aug 06 18:05:04 UTC 1999  1999-08-06 18:05:04 UTC

require 'matrix'
m = accumulator(Matrix[[1, 2], [3, 4]])
puts m[Matrix[[5, 6], [7, 8]]]  # Matrix[[6, 8], [10, 12]]
```


If we define x as a method of self, then the syntax <code>x(5)</code> works, but we deviate more from the task, because x might get "inadvertently modified" by other methods of self.


```ruby
def accumulator(sum)
  lambda {|n| sum += n}
end
class << self
  define_method :x, &accumulator(1)
end
x(5)
accumulator(3)
puts x(2.3)  # prints 8.3
```



## Rust

This solution is explicitly <b>rejected</b> by the task description.  It must be possible to create the accumulator with one type (e.g. int), then accumulate another type (e.g. float) correctly.

Changing "x = foo(1.)" to "x = foo(1)" in the code below should not change the output (it does).


```rust
// rustc -V
// rustc 1.2.0-nightly (0cc99f9cc 2015-05-17) (built 2015-05-18)

use std::ops::Add;

fn foo<Num>(n: Num) -> Box<FnMut(Num) -> Num>
        where Num: Add<Output=Num> + Copy + 'static {
    let mut acc = n;
    Box::new(move |i: Num| {
        acc = acc + i;
        acc
    })
}

fn main() {
    let mut x = foo(1.);
    x(5.);
    foo(3.);
    println!("{}", x(2.3));
}
```

{{out}}

```txt

8.3

```



## Scala

The type of a function can't change in Scala, and there is no "numeric" type that is a supertype of all such types. So, if the accumulator is declared as integer, it can only receive and return integers, and so on.

```scala
def AccumulatorFactory[N](n: N)(implicit num: Numeric[N]) = {
  import num._
  var acc = n
  (inc: N) => {
    acc = acc + inc
    acc
  }
}
```

{{out|Sample}}

```txt

scala> val x = AccumulatorFactory(1.0)
x: (Double) => Double = <function1>

scala> x(5.0)
res7: Double = 6.0

scala> AccumulatorFactory(3.0)
res8: (Double) => Double = <function1>

scala> println(x(2.3))
8.3

```



## Scheme

{{trans|Ruby}}

```scheme
(define (accumulator sum)
  (lambda (n)
    (set! sum (+ sum n))
    sum))

;; or:

(define ((accumulator sum) n)
  (set! sum (+ sum n))
  sum)

(define x (accumulator 1))
(x 5)
(display (accumulator 3)) (newline)
(display (x 2.3)) (newline)
```

{{out}}

```txt
#<procedure>
8.3
```



## Sidef


```ruby
class Accumulator(sum) {
    method add(num) {
        sum += num;
    }
}

var x = Accumulator(1);
x.add(5);
Accumulator(3);
say x.add(2.3);               # prints: 8.3
```


The same thing can be achieved by returning a closure from the '''Accumulator''' function.


```ruby
func Accumulator(sum) {
    func(num) { sum += num };
}

var x = Accumulator(1);
x(5);
Accumulator(3);
say x(2.3);                  # prints: 8.3
```


## Simula


```simula
BEGIN

    ! ABSTRACTION FOR SIMULA'S TWO NUMERIC TYPES ;
    CLASS NUMBER;
    VIRTUAL:
        PROCEDURE OUT IS PROCEDURE OUT;;
    BEGIN
    END NUMBER;

    NUMBER CLASS INTEGERNUMBER(INTVAL); INTEGER INTVAL;
    BEGIN
        PROCEDURE OUT; OUTINT(INTVAL, 10);
    END INTEGERNUMBER;

    NUMBER CLASS REALNUMBER(REALVAL); REAL REALVAL;
    BEGIN
        PROCEDURE OUT; OUTFIX(REALVAL, 4, 10);
    END REALNUMBER;


    ! SIMULA CANNOT RETURN FUNCTIONS - SIMULATE FUNCTIONS WITH CLASSES ;
    CLASS ACCUMULATOR(ACC); REF(NUMBER) ACC;
    BEGIN

        PROCEDURE SWITCHTOREAL(Y); REAL Y;
        BEGIN
            REF(REALNUMBER) NEWACC;
            NEWACC :- NEW REALNUMBER(ACC QUA INTEGERNUMBER.INTVAL);
            NEWACC.REALVAL:= NEWACC.REALVAL + Y;
            ACC :- NEWACC;
        END SWITCHTOREAL;

        REF(NUMBER) PROCEDURE ACCUMULATE(OTHERNUM); REF(NUMBER) OTHERNUM;
        BEGIN
            INSPECT ACC
            WHEN INTEGERNUMBER DO
                BEGIN
                    INSPECT OTHERNUM
                    WHEN INTEGERNUMBER DO
                        ACC QUA INTEGERNUMBER.INTVAL
                            := ACC QUA INTEGERNUMBER.INTVAL + INTVAL
                    WHEN REALNUMBER DO
                        SWITCHTOREAL(REALVAL)
                END
            WHEN REALNUMBER DO
                BEGIN
                    INSPECT OTHERNUM
                    WHEN INTEGERNUMBER DO
                        ACC QUA REALNUMBER.REALVAL
                            := ACC QUA REALNUMBER.REALVAL + INTVAL
                    WHEN REALNUMBER DO
                        ACC QUA REALNUMBER.REALVAL
                            := ACC QUA REALNUMBER.REALVAL + REALVAL
                END;
            ACCUMULATE :- ACC;
        END ACCUMULATE;

        PROCEDURE OUT; ACC.OUT;

    END FOO;


    REF(ACCUMULATOR) FOO;
    FOO :- NEW ACCUMULATOR(NEW INTEGERNUMBER(1)); FOO.OUT; OUTIMAGE;
    FOO.ACCUMULATE(NEW INTEGERNUMBER(5));         FOO.OUT; OUTIMAGE;
    NEW ACCUMULATOR(NEW INTEGERNUMBER(3));
    FOO.ACCUMULATE(NEW REALNUMBER(2.3));          FOO.OUT; OUTIMAGE;

END.

```

{{out}}

```txt

         1
         6
    8.3000

```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
Object subclass: AccumulatorFactory [
  AccumulatorFactory class >> new: aNumber [
    |r sum|
    sum := aNumber.
    r := [ :a |
           sum := sum +  a.
           sum
         ].
    ^r
  ]
]

|x y|
x := AccumulatorFactory new: 1.
x value: 5.
y := AccumulatorFactory new: 3.
(x value: 2.3) displayNl.
"x inspect."
"de-comment the previous line to show that x is a block closure"
```


the above can also be done without a class to hold the block, simply by putting it into another block:
{{works with|Smalltalk/X}}

```smalltalk
|factory a|

factory := [:initial |
    [
        |sum|

        sum := initial.
        [:addend | sum := sum + addend].
    ] value.
].

a := factory value:1.
a value:5.
factory value:3.
(a value:2.3) printCR  "-> 8.3 "
```



## Standard ML

{{trans|OCaml}}
Deviations: An accumulator instance can take ''either'' integers ''or'' reals, but not both mixed (due to lack of runtime polymorphism).

```sml
fun accumulator (sum0:real) : real -> real = let
  val sum = ref sum0
  in
    fn n => (
      sum := !sum + n;
      !sum)
  end;

let
  val x = accumulator 1.0
  val _ = x 5.0
  val _ = accumulator 3.0
in
  print (Real.toString (x 2.3) ^ "\n")
end;
```

{{out}}

```txt
8.3
```



## Swift


```swift
func makeAccumulator(var sum: Double) -> Double -> Double {
  return {
    sum += $0
    return sum
  }
}

let x = makeAccumulator(1)
x(5)
let _ = makeAccumulator(3)
println(x(2.3))
```

{{out}}

```txt
8.3
```



## Tcl

{{works with|Tcl|8.6}}
This uses nested [[wp:coroutine|coroutine]]s to manage the state, which for the outer coroutine is a counter used to generate unique instances of the inner coroutine, and for the inner coroutine it is the actual accumulator variable. Note that Tcl commands (including coroutines) are ''never'' nameless, but it is trivial to synthesize a name for them. It's possible to guarantee uniqueness of names, but just using a simple sequence generator gets 90% of the effect for 10% of the effort.

```tcl
package require Tcl 8.6

# make the creation of coroutines without procedures simpler
proc coro {name arguments body args} {
    coroutine $name apply [list $arguments $body] {*}$args
}
# Wrap the feeding of values in and out of a generator
proc coloop {var body} {
    set val [info coroutine]
    upvar 1 $var v
    while 1 {
	set v [yield $val]
        if {$v eq "stop"} break
	set val [uplevel 1 $body]
    }
}

# The outer coroutine is the accumulator factory
# The inner coroutine is the particular accumulator
coro accumulator {} {
    coloop n {
	coro accumulator.[incr counter] n {
	    coloop i {
		set n [expr {$n + $i}]
	    }
	} $n
    }
}
```

Sample usage (extra characters over Paul's example to show more clearly what is going on):

```tcl
% set x [accumulator 1]
::accumulator.1
% $x 5
6
% accumulator 3
::accumulator.2
% puts ">>[$x 2.3]<<"
>>8.3<<
```



## Unicon

Strictly speaking, <tt>genAcc(n)</tt> returns a <i>co-expression</i>, not a function.  However, the invocation syntax here is indistinguishable from calling a function.

```Unicon
procedure main()
    a := genAcc(3)
    b := genAcc(5)
    
    write("        " ,center("a",5),  " ", center("b", 5))
    write("genAcc: ", right(a(4),5),  " ", right(b(4), 5))
    write("genAcc: ", right(a(2),5),  " ", right(b(3),5))
    write("genAcc: ", right(a(4.5),5)," ", right(b(1.3),5))
end

procedure genAcc(n)   # The generator factory
    return makeProc { while i := (n@&source)[1] do n +:= i }
end

procedure makeProc(A) # A Programmer-Defined Control Operation
    return (@A[1],A[1])
end
```

Note: The co-expression calling sequence used is Unicon specific.
{{out}}

```txt

          a     b  
genAcc:     7     9
genAcc:     9    12
genAcc:  13.5  13.3

```



## TXR



### Verbose



```txrlisp
(defun accumulate (sum)
  (lambda (n)
    (inc sum n)))

;; test
(for ((f (accumulate 0)) num)
     ((set num (iread : : nil)))
     ((format t "~s -> ~s\n" num [f num])))
(exit 0)
```


{{out|Run}}


```txt
$ txr accumulator-factory.tl
1
1 -> 1
2
2 -> 3
3
3 -> 6
400000000000000000000000000000000000000000000000000000000000000000000000
400000000000000000000000000000000000000000000000000000000000000000000000 -> 400000000000000000000000000000000000000000000000000000000000000000000006
5.3
5.3 -> 4e71
1e71
1e71 -> 5e71
[Ctrl-D][Enter]
$
```



### Sugared



```txrlisp
(let ((f (let ((sum 0)) (do inc sum @1))))
  (mapdo (do put-line `@1 -> @[f @1]`) (gun (iread : : nil))))
```

{{out}}

```txt
$ echo "1 2 3 4.5" | txr accumulator-factory2.tl 
1 -> 1
2 -> 3
3 -> 6
4.5 -> 10.5
```


===Yield-based===

Using the <code>obtain</code>/<code>yield</code> interface to delimited continuations, we can turn an imperative for loop into an accumulation function:


```txrlisp
(defun accum ()
  (for ((sum (yield-from accum)))
       ()
       ((inc sum (yield-from accum sum)))))

(let ((f (obtain (accum))))
  (mapdo (do put-line `@1 -> @[f @1]`) (gun (iread : : nil))))
```

{{out}}

```txt
$ echo "1 2 3 4.5" | txr accumulator-factory2.tl 
1 -> 1
2 -> 3
3 -> 6
4.5 -> 10.5
```


===OOP-based===

OOP languages can use objects to simulate closures. In particular, function-objects which can be called as if they were functions, without any visible method being referenced. TXR Lisp supports functors as an expression of irony in language design. A structure object for which a method named <code>lambda</code> is defined can be used as function. Arguments applied to the objects are applied to lambda, preceded by the object itself as the leftmost argument:


```txrlisp
(defstruct (accum count) nil
  (count 0))

(defmeth accum lambda (self delta)
  (inc self.count delta))

;; Identical test code to Yield-Based and Sugared, except for
;; the construction of the function object bound to variable f.
(let ((f (new (accum 0))))
  (mapdo (do put-line `@1 -> @[f @1]`) (gun (iread : : nil))))
```



## UNIX Shell

Deviation from task: The accumulator factory returns a ''global function'', which stores the sum in a ''global variable''. Other code can modify the function or the variable, perhaps by accident.

The shell is a bad choice for this task. This example plays tricks with <tt>eval</tt>. The difficulty with <tt>eval</tt> is to put the quotation marks " and dollar signs <tt>$</tt> in the correct place, and escape them with the correct number of backslashes \. One missing (or one extra) backslash can ruin the entire program.
{{works with|pdksh}}

```bash
#!/bin/sh
accumulator() {
	# Define a global function named $1
	# with a global variable named ${1}_sum.
	eval "${1}_sum=\$2"
	eval "$1() {
		${1}_sum=\$(echo \"(\$${1}_sum) + (\$2)\" | bc)
		eval \"\$1=\\\$${1}_sum\"  # Provide the current sum.
	}"
}

accumulator x 1
x r 5
accumulator y 3
x r 2.3
echo $r
y r -3000
echo $r
```

{{out}}

```txt
$ sh accumulator.sh
8.3
-2997
```


=
## es
=
A better shell for this task is ''es'', because it has lexical variables and closures. <code>@ i {code}</code> is a lambda with parameter ''i'', and <code>fn accumulator n {code}</code> is sugar for <code>fn-accumulator = @ n {code}</code>.

```es
fn accumulator n {
	result @ i {
		n = `{echo $n + $i | bc}
		result $n
	}
}

fn-x = <={accumulator 1}
x 5
fn-y = <={accumulator 3}
echo <={x 2.3}
echo <={y -3000}
```



## VBScript

I'm not entirely convinced that this is actually doing what is asked. A VBScript guru I'm not. The answer's right, though.
;Implementation

```vb
class accumulator
	dim A
	public default function acc(x)
		A = A + x
		acc = A
	end function
	public property get accum
		accum = A
	end property
end class
```

;Invocation

```vb
dim a
set a = new accumulator
x = a( 1 )
a 5
dim b
set b = new accumulator
b 3
wscript.echo a(2.3)
```

{{out}}

```txt

8.3

```



## XLISP

There are probably other ways of doing it, but this is one way.

```lisp
(defun accumulator (x)
	(lambda (n)
		(setq x (+ n x))
		x ) )
```

Test it in a REPL:

```txt
[1] (define f (accumulator 1))

F
[2] (define g (accumulator 3))

G
[3] (f 5)

6
[4] (g 1.7)

4.7
[5] (f 9)

15
```



## Wart


```python
def (accumulator n)
  (fn() ++n)
```


Example usage:

```txt
a <- (accumulator 3)
(a)
=> 4
(a)
=> 5
b <- (accumulator 23)
(b)
=> 24
(a)
=> 6
```



## Yabasic


```Yabasic
sub foo$(n)
	local f$
	
	f$ = "f" + str$(int(ran(1000000)))
	compile("sub " + f$ + "(n): static acum : acum = acum + n : return acum : end sub")
	execute(f$, n)
	return f$
end sub

x$ = foo$(1)
execute(x$, 5)
foo$(3)
print execute(x$, 2.3)

```



## Yorick

Yorick cannot dynamically create new functions. Instead, the accum function can be called in two ways: directly, in which case its first argument is numerical; or through a closure, where its first argument is implicitly an object and the second is the user-provided argument. This example uses closures and group objects, which require Yorick 2.2 or later.

```yorick
func accum(data, n) {
   if(!is_obj(data))
      return closure(accum, save(total=data));
   save, data, total=data.total + n;
   return data.total;
}
```

Example of use (interactive session):

```txt
> x = accum(1)
> x(5)
6
> y = accum(3)
> x(2.3)
8.3
> y(2.3)
5.3
```



## zkl


```zkl
fcn foo(n){ fcn(n,acc){ acc.set(n+acc.value).value }.fp1(Ref(n)) }
```

A strong reference (Ref) is used as the accumulator, a Ref acts like a one element list. The Ref is bound to the new functions second parameter with the .fp1 method. 

```txt

x:=foo(1)   //--> partially applied function
x(5)       //-->6 (int)
y:=foo(3) //-->new PFA
x(2.3).println()
8.3

x(2)  //-->10 (int)
y(2) //-->5 (int)

```

The output switches between int and float based on the most recent input: With addition, the first operand casts the second: int + int|float --> int and float + int|float --> float. If the desire is to make the behavior "once float, always float", a 0 or 0.0 can be used to start the sum and stashed in a another bit of state.

{{omit from|Scratch|cannot generate functions nor pass them as arguments or values}}
{{omit from|C}} <!-- C's type system imcompatible with task spec -->
{{omit from|ML/I}}
{{omit from|Commodore BASIC}}
