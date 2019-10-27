+++
title = "Higher-order functions"
description = ""
date = 2019-10-18T10:44:56Z
aliases = []
[extra]
id = 1758
[taxonomies]
categories = []
tags = []
+++

{{task|Programming language concepts}}

;Task:
Pass a function ''as an argument'' to another function.


;Related task:
*   [[First-class functions]]





## 8th


```forth

: pass-me 
  "I was passed\n" . ;
: passer
  w:exec ;
\ pass 'pass-me' to 'passer'
' pass-me passer

```

{{out}}
I was passed

## ActionScript


```actionscript
package {
    public class MyClass {

        public function first(func:Function):String {
            return func.call();
        }
     
        public function second():String {
            return "second";
        }
        
        public static function main():void {
            var result:String = first(second);
            trace(result);
            result = first(function() { return "third"; });
            trace(result);
        }
    }
}
```



## Ada


### Simple Example


```ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Subprogram_As_Argument is
   type Proc_Access is access procedure;
   
   procedure Second is
   begin
      Put_Line("Second Procedure");
   end Second;
  
   procedure First(Proc : Proc_Access) is
   begin
      Proc.all;
   end First;
begin
   First(Second'Access);
end Subprogram_As_Argument;
```


### Complex Example


```ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Subprogram_As_Argument_2 is
   
   -- Definition of an access to long_float

   type Lf_Access is access Long_Float;
   
   -- Definition of a function returning Lf_Access taking an
   -- integer as a parameter

   function Func_To_Be_Passed(Item : Integer) return Lf_Access is
      Result : Lf_Access := new Long_Float;
   begin
      Result.All := 3.14159 * Long_Float(Item);
      return Result;
   end Func_To_Be_Passed;
   
   -- Definition of an access to function type matching the function
   -- signature above

   type Func_Access is access function(Item : Integer) return Lf_Access;
   
   -- Definition of an integer access type

   type Int_Access is access Integer;
   
   -- Define a function taking an instance of Func_Access as its
   -- parameter and returning an integer access type

   function Complex_Func(Item : Func_Access; Parm2 : Integer) return Int_Access is
      Result : Int_Access := new Integer;
   begin
      Result.All := Integer(Item(Parm2).all / 3.14149);
      return Result;
   end Complex_Func;
   
   -- Declare an access variable to hold the access to the function

   F_Ptr : Func_Access := Func_To_Be_Passed'access;
   
   -- Declare an access to integer variable to hold the result

   Int_Ptr : Int_Access;

begin

   -- Call the function using the access variable

   Int_Ptr := Complex_Func(F_Ptr, 3);
   Put_Line(Integer'Image(Int_Ptr.All));
end Subprogram_As_Argument_2;
```



## Aime


```aime
integer
average(integer p, integer q)
{
    return (p + q) / 2;
}


void
out(integer p, integer q, integer (*f) (integer, integer))
{
    o_integer(f(p, q));
    o_byte('\n');
}


integer
main(void)
{
    # display the minimum, the maximum and the average of 117 and 319
    out(117, 319, min);
    out(117, 319, max);
    out(117, 319, average);

    return 0;
}
```



## AntLang


```AntLang
twice:{x[x[y]]}
echo twice "Hello!"
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
PROC first = (PROC(LONG REAL)LONG REAL f) LONG REAL:
(
  f(1) + 2
);

PROC second = (LONG REAL x)LONG REAL:
(
  x/2
);

main: (
  printf(($xg(5,2)l$,first(second)))
)
```

Output:

```txt

+2.50

```



## AmigaE

The <tt>{}</tt> takes the pointer to an object (code/functions, variables and so on); Amiga E does not distinguish nor check anything, so it is up to the programmer to use the pointer properly. For this reason, a warning is always raised when a ''variable'' (<tt>func</tt>, holding a pointer to a real function in our case) is used like a function.

```amigae
PROC compute(func, val)
  DEF s[10] : STRING
  WriteF('\s\n', RealF(s,func(val),4))
ENDPROC

PROC sin_wrap(val) IS Fsin(val)
PROC cos_wrap(val) IS Fcos(val)

PROC main()
  compute({sin_wrap}, 0.0)
  compute({cos_wrap}, 3.1415)
ENDPROC
```


## AppleScript


```applescript
-- This handler takes a script object (singer)
-- with another handler (call).
on sing about topic by singer
    call of singer for "Of " & topic & " I sing"
end sing
 
-- Define a handler in a script object,
-- then pass the script object.
script cellos
    on call for what
        say what using "Cellos"
    end call
end script
sing about "functional programming" by cellos
 
-- Pass a different handler. This one is a closure
-- that uses a variable (voice) from its context.
on hire for voice
    script
        on call for what
            say what using voice
        end call
    end script
end hire
sing about "closures" by (hire for "Pipe Organ")
```


As we can see above, AppleScript functions (referred to as 'handlers' in Apple's documentation) are not, in themselves, first class objects. They can only be applied within other functions, when passed as arguments, if wrapped in Script objects. If we abstract out this lifting of functions into objects by writing an '''mReturn''' or '''mInject''' function, we can then use it to write some higher-order functions which directly accept unadorned AppleScript handlers as arguments.

We could, for example, write '''map''', '''fold/reduce''' and '''filter''' functions for AppleScript as follows:


```applescript
on run
    -- PASSING FUNCTIONS AS ARGUMENTS TO
    -- MAP, FOLD/REDUCE, AND FILTER, ACROSS A LIST
    
    set lstRange to {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    
    map(squared, lstRange)
    --> {0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100}
    
    foldl(summed, 0, map(squared, lstRange))
    --> 385
    
    filter(isEven, lstRange)
    --> {0, 2, 4, 6, 8, 10}
    
    
    -- OR MAPPING OVER A LIST OF FUNCTIONS
    
    map(testFunction, {doubled, squared, isEven})
    
    --> {{0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20}, 
    --    {0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100}, 
    --    {true, false, true, false, true, false, true, false, true, false, true}}
end run

-- testFunction :: (a -> b) -> [b]
on testFunction(f)
    map(f, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
end testFunction


-- MAP, REDUCE, FILTER

-- Returns a new list consisting of the results of applying the 
-- provided function to each element of the first list
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

-- Applies a function against an accumulator and
-- each list element (from left-to-right) to reduce it 
-- to a single return value

-- In some languages, like JavaScript, this is called reduce()

-- Arguments: function, initial value of accumulator, list
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


-- Sublist of those elements for which the predicate
-- function returns true
-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

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

-- HANDLER FUNCTIONS TO BE PASSED AS ARGUMENTS

-- squared :: Number -> Number
on squared(x)
    x * x
end squared

-- doubled :: Number -> Number
on doubled(x)
    x * 2
end doubled

-- summed :: Number -> Number -> Number
on summed(a, b)
    a + b
end summed

-- isEven :: Int -> Bool
on isEven(x)
    x mod 2 = 0
end isEven
```

{{Out}}

```applescript
{{0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20}, 
{0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100}, 
{true, false, true, false, true, false, true, false, true, false, true}}
```



## Arturo


```arturo
doSthWith [x,y,func]{
	$(func x y)
}

print "add: " + $(doSthWith 2 3 { &0 + &1 })
print "multiply: " + $(doSthWith 2 3 { &0 * &1 })
```

{{out}}

```txt
add: 5
multiply: 6
```



## ATS


```ATS

#include
"share/atspre_staload.hats"

fun app_to_0 (f: (int) -> int): int = f (0)

implement
main0 () =
{
//
val () = assertloc (app_to_0(lam(x) => x+1) = 1)
val () = assertloc (app_to_0(lam(x) => 10*(x+1)) = 10)
//
} (* end of [main0] *)

```



## AutoHotkey


```AutoHotkey

f(x) {
    return "This " . x
}

g(x) {
    return "That " . x
}

show(fun) {
    msgbox % %fun%("works")
}

show(Func("f")) ; either create a Func object
show("g")       ; or just name the function
return

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      REM Test passing a function to a function:
      PRINT FNtwo(FNone(), 10, 11)
      END
      
      REM Function to be passed:
      DEF FNone(x, y) = (x + y) ^ 2
      
      REM Function taking a function as an argument:
      DEF FNtwo(RETURN f%, x, y) = FN(^f%)(x, y)
```

'''Output:'''

```txt

       441

```



## Bracmat


```bracmat
( (plus=a b.!arg:(?a.?b)&!a+!b)
& ( print
  =   text a b func
    .   !arg:(?a.?b.(=?func).?text)
      & out$(str$(!text "(" !a "," !b ")=" func$(!a.!b)))
  )
& print$(3.7.'$plus.add)
&   print
  $ ( 3
    . 7
    . (=a b.!arg:(?a.?b)&!a*!b)
    . multiply
    )
);
```

Output:

```txt
add(3,7)=10
multiply(3,7)=21
```



## Brat


```brat
add = { a, b | a + b }

doit = { f, a, b | f a, b }

p doit ->add 1 2 #prints 3
```



## Burlesque


Burlesque doesn't have functions in the usual sense. One can think of blocks in Burlesque as anonymous functions.
The function "m[" (map) takes a block (a 'function') as it's argument. Add 5 to every element in a list (like map (+5) [1,2,3,4] in haskell):


```burlesque

blsq ) {1 2 3 4}{5.+}m[
{6 7 8 9}

```




## C

'''Simple example'''

The pointer to the function to be passed as an argument is the only involved pointer.

Definition of a function whose only parameter is a pointer to a function with no parameters and no return value:


```c
void myFuncSimple( void (*funcParameter)(void) )
{
    /* ... */
   
    (*funcParameter)();  /* Call the passed function. */
    funcParameter();     /* Same as above with slight different syntax. */

    /* ... */
}
```


Note that you ''can't'' call the passed function by " *funcParameter() ", since that would mean "call funcParameter and then apply the * operator on the returned value".

Call:


```c
void funcToBePassed(void);

/* ... */

myFuncSimple(&funcToBePassed);
```


'''Complex example'''

Definition of a function whose return value is a pointer to int and whose only parameter is a pointer to a function, whose (in turn) return value is a pointer to double and whose only parameter is a pointer to long. 


```c
int* myFuncComplex( double* (*funcParameter)(long* parameter) )
{
     long inLong;
     double* outDouble;
     long *inLong2 = &inLong;

     /* ... */

     outDouble = (*funcParameter)(&inLong);  /* Call the passed function and store returned pointer. */
     outDouble = funcParameter(inLong2);     /* Same as above with slight different syntax. */

     /* ... */
}
```

Call:


```c
double* funcToBePassed(long* parameter);

/* ... */

int* outInt;  

outInt = myFuncComplex(&funcToBePassed);
```


'''Pointer'''

Finally, declaration of a pointer variable of the proper type to hold such a function as myFunc:


```c
int* (*funcPointer)( double* (*funcParameter)(long* parameter) );

/* ... */

funcPointer = &myFuncComplex;
```


Of course, in a real project you shouldn't write such a convoluted code, but use some typedef instead, in order to break complexity into steps.


## C++



### Function Pointer


{{works with|g++|3.4.2 (mingw-special)}}

C++ can pass function pointers in the same manner as C.


### Function class template


Using the std::tr1::function class template allows more powerful usage. function<> can be used to pass around arbitrary function objects. This permits them to be used as closures. 

For C++11 this is now std::function.

{{works with|gcc|4.4}}

```cpp

// Use <functional> for C++11
#include <tr1/functional>
#include <iostream>

using namespace std;
using namespace std::tr1;

void first(function<void()> f)
{
  f();
}

void second()
{
  cout << "second\n";
}

int main()
{
  first(second);
}

```



### Template and Inheritance


{{works with|Visual C++|2005}}


```cpp>#include <iostream

#include <functional>

template<class Func>
typename Func::result_type first(Func func, typename Func::argument_type arg)
{
  return func(arg);
}

class second : public std::unary_function<int, int>
{
public:
  result_type operator()(argument_type arg) const
  {
    return arg * arg;
  }
};

int main()
{
  std::cout << first(second(), 2) << std::endl;
  return 0;
}
```



## C sharp


Each example below performs the same task and utilizes .NET delegates, which are objects that refer to a static method or to an instance method of a particular object instance.

{{out|note=for each example}}

```txt

f=Add, f(6, 2) = 8
f=Mul, f(6, 2) = 12
f=Div, f(6, 2) = 3

```


==={{anchor|C#: Named methods}}Named methods===
This implementation works in all standard versions of C#.


```csharp
using System;

// A delegate declaration. Because delegates are types, they can exist directly in namespaces.
delegate int Func2(int a, int b);

class Program
{
    static int Add(int a, int b)
    {
        return a + b;
    }
    
    static int Mul(int a, int b)
    {
        return a * b;
    }
    
    static int Div(int a, int b)
    {
        return a / b;
    }
    
    static int Call(Func2 f, int a, int b)
    {
        // Invoking a delegate like a method is syntax sugar; this compiles down to f.Invoke(a, b);
        return f(a, b);
    }

    static void Main()
    {
        int a = 6;
        int b = 2;

        // Delegates must be created using the "constructor" syntax in C# 1.0; in C# 2.0 and above, only the name of the method is required (when a target type exists, such as in an assignment to a variable with a delegate type or usage in a function call with a parameter of a delegate type; initializers of implicitly typed variables must use the constructor syntax as a raw method has no delegate type). Overload resolution is performed using the parameter types of the target delegate type.
        Func2 add = new Func2(Add);
        Func2 mul = new Func2(Mul);
        Func2 div = new Func2(Div);
        
        Console.WriteLine("f=Add, f({0}, {1}) = {2}", a, b, Call(add, a, b));
        Console.WriteLine("f=Mul, f({0}, {1}) = {2}", a, b, Call(mul, a, b));
        Console.WriteLine("f=Div, f({0}, {1}) = {2}", a, b, Call(div, a, b));
    }
}
```



### C# 2.0: Anonymous methods

Anonymous methods were added in C# 2.0. Parameter types must be specified. Anonymous methods must be "coerced" to a delegate type known at compile-time; they cannot be used with a target type of Object or to initialize implicitly typed variables.


```csharp
using System;

delegate int Func2(int a, int b);

class Program
{
    static int Call(Func2 f, int a, int b)
    {
        return f(a, b);
    }

    static void Main()
    {
        int a = 6;
        int b = 2;

        Console.WriteLine("f=Add, f({0}, {1}) = {2}", a, b, Call(delegate(int x, int y) { return x + y; }, a, b));
        Console.WriteLine("f=Mul, f({0}, {1}) = {2}", a, b, Call(delegate(int x, int y) { return x * y; }, a, b));
        Console.WriteLine("f=Div, f({0}, {1}) = {2}", a, b, Call(delegate(int x, int y) { return x / y; }, a, b));
    }
}
```


==={{anchor|C#: Lambda expressions}}C# 3.0: Lambda expressions===
Lambda expressions were added in C# 3.0 as a more concise replacement to anonymous methods. The target delegate type must also be known at compile-time.

With .NET Framework 3.5, the System namespace also gained the Func and Action "families" of generic delegate types. Action delegates are void-returning, while Func delegates return a value of a specified type. In both families, a separate type exists for every function arity from zero to sixteen, as .NET does not have variadic generics.

For instance, the <code>Action</code> delegate has no parameters, <code>Action<T></code>, has one parameter of type T, <code>Action<T1, T2></code> has two parameters of types T1 and T2, and so on. Similarly, <code>Func<TResult></code> has no parameters and a return type of TResult, <code>Func<T1, TResult></code> additionally has one parameter of type T, and so on.

{{works with|C sharp|C#|3+}}


```csharp
using System;

class Program
{
    static int Call(Func<int, int, int> f, int a, int b)
    {
        return f(a, b);
    }

    static void Main()
    {
        int a = 6;
        int b = 2;
        
                                                                 // No lengthy delegate keyword.
        Console.WriteLine("f=Add, f({0}, {1}) = {2}", a, b, Call((int x, int y) => { return x + y; }, a, b));

                                                                 // Parameter types can be inferred.
        Console.WriteLine("f=Mul, f({0}, {1}) = {2}", a, b, Call((x, y) => { return x * y; }, a, b));

                                                                 // Expression lambdas are even shorter (and are most idiomatic).
        Console.WriteLine("f=Div, f({0}, {1}) = {2}", a, b, Call((x, y) => x / y, a, b));
    }
}
```



## Clean

Take a function as an argument and apply it to all elements in a list:

```clean
map f [x:xs] = [f x:map f xs]
map f []     = []
```

Pass a function as an argument:

```clean
incr x = x + 1

Start = map incr [1..10]
```

Do the same using a anonymous function:

```clean
Start = map (\x -> x + 1) [1..10]
```

Do the same using currying:

```clean
Start = map ((+) 1) [1..10]
```



## Clojure



```lisp

(defn append-hello [s]
  (str "Hello " s))

(defn modify-string [f s]
  (f s))

(println (modify-string append-hello "World!"))

```



## CoffeeScript


Passing an anonymous function to built-in map/reduce functions:


```coffeescript
double = [1,2,3].map (x) -> x*2
```


Using a function stored in a variable:


```coffeescript
fn = -> return 8
sum = (a, b) -> a() + b()
sum(fn, fn) # => 16

```


List comprehension with a function argument:


```coffeescript
bowl = ["Cheese", "Tomato"]

smash = (ingredient) ->
    return "Smashed #{ingredient}"

contents = smash ingredient for ingredient in bowl
# => ["Smashed Cheese", "Smashed Tomato"]

```


Nested function passing:


```coffeescript
double = (x) -> x*2
triple = (x) -> x*3
addOne = (x) -> x+1

addOne triple double 2 # same as addOne(triple(double(2)))
```


A function that returns a function that returns a function that returns a function that returns 2, immediately executed:


```coffeescript
(-> -> -> -> 2 )()()()() # => 2
```


A function that takes a function that takes a function argument:


```coffeescript
((x)->
    2 + x(-> 5)
)((y) -> y()+3)
# result: 10
```



## Common Lisp

In Common Lisp, functions are [[wp:First-class_object|first class objects]], so you can pass function objects as arguments to other functions:


```lisp
CL-USER> (defun add (a b) (+ a b))
ADD
CL-USER> (add 1 2)
3
CL-USER> (defun call-it (fn x y)
            (funcall fn x y))
CALL-IT
CL-USER> (call-it #'add 1 2)
3
```

The Common Lisp library makes extensive use of higher-order functions:

```txt
CL-USER> (funcall #'+ 1 2 3)
6
CL-USER> (apply #'+ (list 1 2 3))
6
CL-USER> (sort (string-downcase "Common Lisp will bend your mind!") #'string<)
"     !bcddeiiilllmmmnnnoooprsuwy"
CL-USER> (reduce #'/ '(1 2 3 4 5))
1/120
CL-USER> (mapcar #'(lambda (n) (expt 2 n)) '(0 1 2 3 4 5))
(1 2 4 8 16 32)
CL-USER> 
```



## D


```d
int hof(int a, int b, int delegate(int, int) f) {
    return f(a, b);
}

void main() {
    import std.stdio;
    writeln("Add: ", hof(2, 3, (a, b) => a + b));
    writeln("Multiply: ", hof(2, 3, (a, b) => a * b));
}
```

{{out}}

```txt
Add: 5
Multiply: 6
```

This longer and more systematic example shows D functions/delegates by passing each type of function/delegate to _test_ as argument.

```d
import std.stdio;

// Test the function argument.
string test(U)(string scopes, U func) {
    string typeStr = typeid(typeof(func)).toString();

    string isFunc = (typeStr[$ - 1] == '*') ? "function" : "delegate";
    writefln("Hi, %-13s : scope: %-8s (%s) : %s",
             func(), scopes, isFunc, typeStr );
    return scopes;
}

// Normal module level function.
string aFunction() { return "Function"; }

// Implicit-Function-Template-Instantiation (IFTI) Function.
T tmpFunc(T)() { return "IFTI.function"; }

// Member in a template.
template tmpGroup(T) {
    T t0(){ return "Tmp.member.0"; }
    T t1(){ return "Tmp.member.1"; }
    T t2(){ return "Tmp.member.2"; }
}

// Used for implementing member function at class & struct.
template Impl() {
    static string aStatic() { return "Static Method";  }
    string aMethod() { return "Method"; }
}

class C { mixin Impl!(); }
struct S { mixin Impl!(); }

void main() {
    // Nested function.
    string aNested() {
        return "Nested";
    }

  // Bind to a variable.
  auto variableF = function string() { return "variable.F"; };
  auto variableD = delegate string() { return "variable.D"; };

  C c = new C;
  S s;

      "Global".test(&aFunction);
      "Nested".test(&aNested);
       "Class".test(&C.aStatic)
              .test(&c.aMethod);
      "Struct".test(&S.aStatic)
              .test(&s.aMethod);
    "Template".test(&tmpFunc!(string))
              .test(&tmpGroup!(string).t2);
     "Binding".test(variableF)
              .test(variableD);
    // Literal function/delegate.
     "Literal".test(function string() { return "literal.F"; })
              .test(delegate string() { return "literal.D"; });
}
```

{{out}}}

```txt
Hi, Function      : scope: Global   (function) : immutable(char)[]()*
Hi, Nested        : scope: Nested   (delegate) : immutable(char)[] delegate()
Hi, Static Method : scope: Class    (function) : immutable(char)[]()*
Hi, Method        : scope: Class    (delegate) : immutable(char)[] delegate()
Hi, Static Method : scope: Struct   (function) : immutable(char)[]()*
Hi, Method        : scope: Struct   (delegate) : immutable(char)[] delegate()
Hi, IFTI.function : scope: Template (function) : immutable(char)[]()*
Hi, Tmp.member.2  : scope: Template (function) : immutable(char)[]()*
Hi, variable.F    : scope: Binding  (function) : immutable(char)[]()*
Hi, variable.D    : scope: Binding  (delegate) : immutable(char)[] delegate()
Hi, literal.F     : scope: Literal  (function) : immutable(char)[]()*
Hi, literal.D     : scope: Literal  (delegate) : immutable(char)[] delegate()
```



## Delphi

:''See [[#Pascal|Pascal]]''
=={{header|Déjà Vu}}==

```dejavu
map f lst:
    ]
    for item in lst:
        f item
    [

twice:
    * 2

!. map @twice [ 1 2 5 ]
```

{{out}}

```txt
[ 2 4 10 ]
```



## DWScript


```delphi
type TFnType = function(x : Float) : Float;
 
function First(f : TFnType) : Float;
begin
   Result := f(1) + 2;
end;
 
function Second(f : Float) : Float;
begin
   Result := f/2;
end;
 
PrintLn(First(Second));
```



## Dyalect


{{trans|C#}}


```Dyalect
func call(f, a, b) {
    f(a, b)
}
 
const a = 6
const b = 2

print("f=add, f(\(a), \(b)) = \(call((x, y) => x + y, a, b))")
print("f=mul, f(\(a), \(b)) = \(call((x, y) => x * y, a, b))")
print("f=div, f(\(a), \(b)) = \(call((x, y) => x / y, a, b))")
```


{{out}}


```txt
f=add, f(6, 2) = 8
f=mul, f(6, 2) = 12
f=div, f(6, 2) = 3
```



## E


```e
def map(f, list) {
  var out := []
  for x in list {
    out with= f(x)
  }
  return out
}

? map(fn x { x + x }, [1, "two"])
# value: [2, "twotwo"]

? map(1.add, [5, 10, 20])
# value: [6, 11, 21]

? def foo(x) { return -(x.size()) }
> map(foo, ["", "a", "bc"])
# value: [0, -1, -2]
```




## ECL

<lang>//a Function prototype:
INTEGER actionPrototype(INTEGER v1, INTEGER v2) := 0;

INTEGER aveValues(INTEGER v1, INTEGER v2) := (v1 + v2) DIV 2;
INTEGER addValues(INTEGER v1, INTEGER v2) := v1 + v2;
INTEGER multiValues(INTEGER v1, INTEGER v2) := v1 * v2;

//a Function prototype using a function prototype:
INTEGER applyPrototype(INTEGER v1, actionPrototype actionFunc) := 0;

//using the Function prototype and a default value:
INTEGER applyValue2(INTEGER v1,
                    actionPrototype actionFunc = aveValues) :=
                    actionFunc(v1, v1+1)*2;
                       
//Defining the Function parameter inline, witha default value:
INTEGER applyValue4(INTEGER v1,
                    INTEGER actionFunc(INTEGER v1,INTEGER v2) = aveValues)
               := actionFunc(v1, v1+1)*4; 
INTEGER doApplyValue(INTEGER v1,
                     INTEGER actionFunc(INTEGER v1, INTEGER v2))
        := applyValue2(v1+1, actionFunc);
       
//producing simple results:
OUTPUT(applyValue2(1));                           // 2
OUTPUT(applyValue2(2));                           // 4
OUTPUT(applyValue2(1, addValues));                // 6
OUTPUT(applyValue2(2, addValues));                // 10
OUTPUT(applyValue2(1, multiValues));              // 4
OUTPUT(applyValue2(2, multiValues));              // 12
OUTPUT(doApplyValue(1, multiValues));             // 12
OUTPUT(doApplyValue(2, multiValues));             // 24


          
//A definition taking function parameters which themselves
//have parameters that are functions...

STRING doMany(INTEGER v1,
              INTEGER firstAction(INTEGER v1,
                                  INTEGER actionFunc(INTEGER v1,INTEGER v2)),
              INTEGER secondAction(INTEGER v1,
                                   INTEGER actionFunc(INTEGER v1,INTEGER v2)),
              INTEGER actionFunc(INTEGER v1,INTEGER v2)) 
       := (STRING)firstAction(v1, actionFunc) + ':' + (STRING)secondaction(v1, actionFunc);

OUTPUT(doMany(1, applyValue2, applyValue4, addValues));
     // produces "6:12"
     
OUTPUT(doMany(2, applyValue4, applyValue2,multiValues));
     // produces "24:12"
```



## Efene



```efene
first = fn (F) {
  F()
}

second = fn () { 
  io.format("hello~n")
}

@public
run = fn () {
    # passing the function specifying the name and arity
    # arity: the number of arguments it accepts
    first(fn second:0)

    first(fn () { io.format("hello~n") })

    # holding a reference to the function in a variable
    F1 = fn second:0
    F2 = fn () { io.format("hello~n") }

    first(F1)
    first(F2)
}

```


## Elena

{{trans|Smalltalk}}
ELENA 4.1 :

```elena
import extensions;
 
public program()
{
    var first := (f => f());
    var second := {"second"};
    console.printLine(first(second))
}
```

{{out}}

```txt
second
```



## Elixir


```elixir
iex(1)> defmodule RC do
...(1)>   def first(f), do: f.()
...(1)>   def second, do: :hello
...(1)> end
{:module, RC,
 <<70, 79, 82, 49, 0, 0, 4, 224, 66, 69, 65, 77, 69, 120, 68, 99, 0, 0, 0, 142,
131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114, 95, 100, 111, 99, 115, 95
, 118, 49, 108, 0, 0, 0, 2, 104, 2, ...>>,
 {:second, 0}}
iex(2)> RC.first(fn -> RC.second end)
:hello
iex(3)> RC.first(&RC.second/0)			# Another expression
:hello
iex(4)> f = fn -> :world end                    # Anonymous function
#Function<20.54118792/0 in :erl_eval.expr/5>
iex(5)> RC.first(f)
:world
```



## Erlang


Erlang functions are atoms, and they're considered different functions if their arity (the number of arguments they take) is different. As such, an Erlang function must be passed as <code>fun Function/Arity</code>, but can be used as any other variable:

```erlang
-module(test).
-export([first/1, second/0]).

first(F) -> F().
second() -> hello.
```

Testing it:

```erlang>1
 c(tests).
{ok, tests}
2> tests:first(fun tests:second/0).
hello
3> tests:first(fun() -> anonymous_function end).
anonymous_function
```



## ERRE

ERRE function are limited to one-line FUNCTION, but you can write:

```ERRE

PROGRAM FUNC_PASS

  FUNCTION ONE(X,Y)
     ONE=(X+Y)^2
  END FUNCTION

  FUNCTION TWO(X,Y)
     TWO=ONE(X,Y)+1
  END FUNCTION

BEGIN
  PRINT(TWO(10,11))
END PROGRAM

```

Answer is 442


## Euler Math Toolbox



```Euler Math Toolbox

>function f(x,a) := x^a-a^x
>function dof (f$:string,x) := f$(x,args());
>dof("f",1:5;2)
 [ -1  0  1  0  -7 ]
>plot2d("f",1,5;2):

```



## Euphoria


```euphoria
procedure use(integer fi, integer a, integer b)
    print(1,call_func(fi,{a,b}))
end procedure

function add(integer a, integer b)
    return a + b
end function

use(routine_id("add"),23,45)
```


=={{header|F Sharp|F#}}==
We define a function that takes another function f as an argument and applies that function twice to the argument x:

```fsharp>
 let twice f x = f (f x);;

val twice : ('a -> 'a) -> 'a -> 'a

> twice System.Math.Sqrt 81.0;;
val it : float = 3.0
```


Another example, using an operator as a function:

```fsharp>
 List.map2 (+) [1;2;3] [3;2;1];;
val it : int list = [4; 4; 4]
```



## Factor

Using words (factor's functions) :

```factor
USING: io ;
IN: rosetacode
: argument-function1 ( -- ) "Hello World!" print ;
: argument-function2 ( -- ) "Goodbye World!" print ;

! normal words have to know the stack effect of the input parameters they execute
: calling-function1 ( another-function -- ) execute( -- ) ;

! unlike normal words, inline words do not have to know the stack effect.
: calling-function2 ( another-function -- ) execute ; inline

! Stack effect has to be written for runtime computed values :
: calling-function3 ( bool -- ) \ argument-function1 \ argument-function2 ? execute( -- ) ;

```


    ( scratchpad )
    \ argument-function1 calling-function1
    \ argument-function1 calling-function2
    t calling-function3
    f calling-function3

    Hello World!
    Hello World!
    Hello World!
    Goodbye World!


## FALSE

Anonymous code blocks are the basis of FALSE control flow and function definition. These blocks may be passed on the stack as with any other parameter.

```false
[f:[$0>][@@\f;!\1-]#%]r:   { reduce n stack items using the given basis and binary function }

1 2 3 4  0 4[+]r;!." " { 10 }
1 2 3 4  1 4[*]r;!." " { 24 }
1 2 3 4  0 4[$*+]r;!.  { 30 }
```



## Fantom



```fantom

class Main
{
  // apply given function to two arguments
  static Int performOp (Int arg1, Int arg2, |Int, Int -> Int| fn)
  {
    fn (arg1, arg2)
  }

  public static Void main ()
  {
    echo (performOp (2, 5, |Int a, Int b -> Int| { a + b }))
    echo (performOp (2, 5, |Int a, Int b -> Int| { a * b }))
  }
}

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Higher-order_functions this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

Forth words can be referenced on the stack via their ''execution token'' or XT. An XT is obtained from a word via the quote operator, and invoked via '''EXECUTE'''. Anonymous functions may be defined via ''':NONAME''' (returning an XT) instead of a standard colon definition.


```forth
: square  dup * ;
: cube  dup dup * * ;
: map. ( xt addr len -- )
  0 do  2dup i cells + @ swap execute .  loop 2drop ;

create array 1 , 2 , 3 , 4 , 5 ,
' square array 5 map. cr   \ 1 4 9 16 25
' cube   array 5 map. cr   \ 1 8 27 64 125
:noname 2* 1+ ; array 5 map. cr   \ 3 5 7 9 11
```



## Fortran

{{works with|Fortran|90 and later}}
use the EXTERNAL attribute to show the dummy argument is another function rather than a data object. i.e.

```fortran
FUNCTION FUNC3(FUNC1, FUNC2, x, y)
  REAL, EXTERNAL :: FUNC1, FUNC2 
  REAL :: FUNC3
  REAL :: x, y

  FUNC3 = FUNC1(x) * FUNC2(y)
END FUNCTION FUNC3
```


Another way is to put the functions you want to pass in a module:


```fortran
module FuncContainer
  implicit none
contains

  function func1(x)
    real :: func1
    real, intent(in) :: x

    func1 = x**2.0
  end function func1

  function func2(x)
    real :: func2
    real, intent(in) :: x

    func2 = x**2.05
  end function func2

end module FuncContainer

program FuncArg
  use FuncContainer
  implicit none

  print *, "Func1"
  call asubroutine(func1)

  print *, "Func2"
  call asubroutine(func2)

contains

  subroutine asubroutine(f)
    ! the following interface is redundant: can be omitted
    interface
       function f(x)
         real, intent(in) :: x
         real :: f
       end function f
    end interface
    real :: px

    px = 0.0
    do while( px < 10.0 )
       print *, px, f(px)
       px = px + 1.0
    end do
  end subroutine asubroutine

end program FuncArg
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function square(n As Integer) As Integer  
  Return n * n
End Function

Function cube(n As Integer) As Integer
  Return n * n * n
End Function

Sub doCalcs(from As Integer, upTo As Integer, title As String, func As Function(As Integer) As Integer)
  Print title; " -> ";
  For i As Integer = from To upTo
    Print Using "#####"; func(i);
  Next
  Print  
End Sub

doCalcs 1, 10, "Squares", @square
doCalcs 1, 10, "Cubes  ", @cube
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Squares ->     1    4    9   16   25   36   49   64   81  100
Cubes   ->     1    8   27   64  125  216  343  512  729 1000

```



## Frink

The following defines an anonymous function and passes it to another function.  In this case, the anonymous function is a comparison function that sorts by string length.


```frink

cmpFunc = {|a,b| length[a] <=> length[b]}

a = ["tree", "apple", "bee", "monkey", "z"]
sort[a, cmpFunc]

```


You can also look up functions by name and number of arguments.  The following is equivalent to the previous example.


```frink

lengthCompare[a,b] := length[a] <=> length[b]

func = getFunction["lengthCompare", 2]
a = ["tree", "apple", "bee", "monkey", "z"]
sort[a, func]

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as pointer functionOneAddress

def fn FunctionOne( x as long, y as long ) as long = (x + y) ^ 2
functionOneAddress = @fn FunctionOne
 
def fn FunctionTwo( x as long, y as long ) using functionOneAddress

print fn FunctionTwo( 12, 12 )

```


Output:

```txt

576

```



## GAP


```gap
Eval := function(f, x)
  return f(x);
end;

Eval(x -> x^3, 7);
# 343
```



## Go


```go
package main
import "fmt"

func func1(f func(string) string) string { return f("a string") }
func func2(s string) string { return "func2 called with " + s }
func main() { fmt.Println(func1(func2)) }
```



## Groovy

As [[closures]]:

```groovy
first = { func -> func() }
second = { println "second" }

first(second)
```


As functions:

```groovy
def first(func) { func() }
def second() { println "second" }

first(this.&second)
```



## Haskell

{{works with|GHC|GHCi|6.6}}

A function is just a value that wants arguments:

```haskell
func1 f = f "a string"
func2 s = "func2 called with " ++ s

main = putStrLn $ func1 func2
```

Or, with an anonymous function:

```haskell
func f = f 1 2 

main = print $ func (\x y -> x+y)
-- output: 3
```

Note that <tt>func (\x y -> x+y)</tt> is equivalent to <tt>func (+)</tt>. (Operators are functions too.)

=={{header|Icon}} and {{header|Unicon}}==

```icon
 procedure main()
   local lst
   lst := [10, 20, 30, 40]
   myfun(callback, lst)
end

procedure myfun(fun, lst)
   every fun(!lst)
end

procedure callback(arg)
   write("->", arg)
end
```



## Inform 6

As in C, functions in Inform 6 are not first-class, but pointers to functions can be used.

```Inform6
[ func;
  print "Hello^";
];

[ call_func x;
  x();
];

[ Main;
  call_func(func);
];
```



## Inform 7

Phrases usually aren't defined with names, only with invocation syntax. A phrase must be given a name (here, "addition" and "multiplication") in order to be passed as a phrase value.

```inform7
Higher Order Functions is a room.

To decide which number is (N - number) added to (M - number) (this is addition):
	decide on N + M.

To decide which number is multiply (N - number) by (M - number) (this is multiplication):
	decide on N * M.

To demonstrate (P - phrase (number, number) -> number) as (title - text):
	say "[title]: [P applied to 12 and 34]."

When play begins:
	demonstrate addition as "Add";
	demonstrate multiplication as "Mul";
	end the story.
```



## J


''Adverbs'' take a single verb or noun argument and ''conjunctions'' take two. For example,<tt> / </tt>(insert)<tt> \ </tt>(prefix) and<tt> \. </tt>(suffix) are adverbs and <tt> @ </tt>(atop), <tt> & </tt>(bond or compose) and <tt> ^: </tt>(power) are conjunctions. The following expressions illustrate their workings.


```j
   + / 3 1 4 1 5 9   NB. sum
23
   >./ 3 1 4 1 5 9   NB. max
9
   *./ 3 1 4 1 5 9   NB. lcm
180

   +/\ 3 1 4 1 5 9   NB. sum prefix (partial sums)
3 4 8 9 14 23

   +/\. 3 1 4 1 5 9  NB. sum suffix
23 20 19 15 14 9

   2&% 1 2 3         NB. divide 2 by
2 1 0.666667

   %&2 (1 2 3)       NB. divide by 2 (need parenthesis to break up list formation)
0.5 1 1.5
   -: 1 2 3          NB. but divide by 2 happens a lot so it's a primitive
0.5 1 1.5

   f=: -:@(+ 2&%)    NB. one Newton iteration
   f 1
1.5
   f f 1
1.41667

   f^:(i.5) 1        NB. first 5 Newton iterations
1 1.5 1.41667 1.41422 1.41421
   f^:(i.5) 1x       NB. rational approximations to sqrt 2
1 3r2 17r12 577r408 665857r470832
```


Adverbs and conjunctions may also be user defined


```J
   + conjunction def 'u' -
+
   + conjunction def 'v' -
-
   * adverb def '10 u y' 11
110
   ^ conjunction def '10 v 2 u y' * 11
20480
```



## Java


There is no real callback in Java like in C or C++, but we can do the same as swing does for executing an event. We need to create an interface that has the method we want to call or create one that will call the method we want to call. The following example uses the second way.


```java
public class NewClass {
   
   public NewClass() {
       first(new AnEventOrCallback() {
           public void call() {
               second();
           }
       });
   }
   
   public void first(AnEventOrCallback obj) {
       obj.call();
   }
   
   public void second() {
       System.out.println("Second");
   }
   
   public static void main(String[] args) {
       new NewClass();
   }
}

interface AnEventOrCallback {
   public void call();
}
```


From Java 8, lambda expressions may be used. Example (from Oracle):

```java
public class ListenerTest {
   public static void main(String[] args) {   
     JButton testButton = new JButton("Test Button");
     testButton.addActionListener(new ActionListener(){
     @Override public void actionPerformed(ActionEvent ae){
         System.out.println("Click Detected by Anon Class");
       }
     });
     
     testButton.addActionListener(e -> System.out.println("Click Detected by Lambda Listner"));
     
     // Swing stuff
     JFrame frame = new JFrame("Listener Test");
     frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
     frame.add(testButton, BorderLayout.CENTER);
     frame.pack();
     frame.setVisible(true);     
   }
}
```



## JavaScript



```javascript
function first (func) {
  return func();
}

function second () {
  return "second";
}

var result = first(second);
result = first(function () { return "third"; });
```


An example with anonymous functions and uses in the core library

{{works with|Firefox|1.5}} for methods <code>filter</code> and <code>map</code>.


```javascript>>>
 var array = [2, 4, 5, 13, 18, 24, 34, 97];
>>> array
[2, 4, 5, 13, 18, 24, 34, 97]

// return all elements less than 10
>>> array.filter(function (x) { return x < 10 });
[2, 4, 5]

// return all elements less than 30
>>> array.filter(function (x) { return x < 30 });
[2, 4, 5, 13, 18, 24]

// return all elements less than 100
>>> array.filter(function (x) { return x < 100 });
[2, 4, 5, 13, 18, 24, 34, 97]

// multiply each element by 2 and return the new array
>>> array.map(function (x) { return x * 2 });
[4, 8, 10, 26, 36, 48, 68, 194]

// sort the array from smallest to largest
>>> array.sort(function (a, b) { return a > b });
[2, 4, 5, 13, 18, 24, 34, 97]

// sort the array from largest to smallest
>>> array.sort(function (a, b) { return a < b });
[97, 34, 24, 18, 13, 5, 4, 2]
```



## Joy

This example is taken from V.
Define first as multiplying two numbers on the stack.

```joy
DEFINE first == *.
```

There will be a warning about overwriting builtin first.
Define second as interpreting the passed quotation on the stack.

```joy>DEFINE second == i.</lang

Pass first enclosed in quotes to second which applies it on the stack.

```joy
2 3 [first] second.
```

The program prints 6.


## jq

The examples given in this section closely follow the exposition in the Julia section of this page.

To understand these examples, it is helpful to remember that:

* jq functions are filters that can participate in a left-to-right pipeline, just as in most modern command shells;

* "." on the right of a pipe ("|") refers to the output from the filter on the left.

====Example 1: "hello blue world"====

```jq
def foo( filter ):
  ("world" | filter) as $str
  | "hello \($str)" ;

# blue is defined here as a filter that adds blue to its input:
def blue: "blue \(.)";

foo( blue ) # prints "hello blue world"

```

====Example 2: g(add; 2; 3)====

```jq

def g(f; x; y): [x,y] | f;

g(add; 2; 3) # => 5
```

====Example: Built-in higher-order functions====
In the following sequence of interactions, we pass the function *is_even/0* to some built-in higher order functions. *is_even/0* is defined as follows:

```jq
def is_even:
  if floor == . then (. % 2) == 0 
  else error("is_even expects its input to be an integer")
  end;
```

```jq

# Are all integers between 1 and 5 even?
# For this example, we will use all/2 even
# though it requires a release of jq after jq 1.4;
# we do so to highlight the fact that all/2
# terminates the generator once the condition is satisfied:
all( range(1;6); is_even )       
false
 
# Display the even integers in the given range:
range(1;6) | select(is_even)
2
4

# Evaluate is_even for each integer in an array
[range(1;6)] | map(is_even)
[false, true, false, true, false]

# Note that in jq, there is actually no need to call
# a higher-order function in cases like this.
# For example one can simply write:
range(1;6) | is_even
false
true
false
true
false
```


## Julia



```Julia

function foo(x)
  str = x("world")
  println("hello $(str)!")
end
foo(y -> "blue $y") # prints "hello blue world"

```


The above code snippet defines a named function, <tt>foo</tt>, which takes a single argument, which is a <tt>Function</tt>.
<tt>foo</tt> calls this function on the string literal <tt>"world"</tt>, and then interpolates the result into the "hello ___!" string literal, and prints it.
In the final line, <tt>foo</tt> is called with an anonymous function that takes a string, and returns a that string with <tt>"blue "</tt> preppended to it.


```Julia

function g(x,y,z)
  x(y,z)
end
println(g(+,2,3)) # prints 5

```


This code snippet defines a named function <tt>g</tt> that takes three arguments: <tt>x</tt> is a function to call, and <tt>y</tt> and <tt>z</tt> are the values to call <tt>x</tt> on.
We then call <tt>g</tt> on the <tt>+</tt> function. Operators in Julia are just special names for functions.

In the following interactive session, we pass the function iseven to a few higher order functions. The function iseven returns true if its argument is an even integer, false if it is an odd integer, and throws an error otherwise. The second argument to the functions is a range of integers, specifically the five integers between 1 and 5 included.

```julia>julia
 all(iseven, 1:5)              # not all integers between 1 and 5 are even.
false

julia> findfirst(iseven, 1:5)        # the first even integer is at index 2 in the range.
2

julia> count(iseven, 1:5)            # there are two even integers between 1 and 5.
2

julia> filter(iseven, 1:5)           # here are the even integers in the given range.
2-element Array{Int64,1}:
 2
 4

julia> map(iseven, 1:5)              # we apply our function to all integers in range.
5-element Array{Bool,1}:
 false
  true
 false
  true
 false

```



## Kotlin

Kotlin is a functional language. Example showing how the builtin map function is used to get the average value of a transformed list of numbers:

```scala
fun main(args: Array<String>) {
    val list = listOf(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    val a = list.map({ x -> x + 2 }).average()
    val h = list.map({ x -> x * x }).average()
    val g = list.map({ x -> x * x * x }).average()
    println("A = %f  G = %f  H = %f".format(a, g, h))
}
```


Another example showing the syntactic sugar available to Kotlin developers which allows them to put the lambda expression out of the parenthesis whenever the function is the last argument of the higher order function. Notice the usage of the inline modifier, which inlines the bytecode of the argument function on the callsite, reducing the object creation overhead (an optimization for pre Java 8 JVM environments, like Android) (translation from Scala example):

```scala
inline fun higherOrderFunction(x: Int, y: Int, function: (Int, Int) -> Int) = function(x, y)

fun main(args: Array<String>) {
    val result = higherOrderFunction(3, 5) { x, y -> x + y }
    println(result)
}
```


{{out}}

```txt
8
```



## Lingo

Lingo doesn't support first-class functions. But functions can be passed as "symbols", and then be called via Lingo's 'call' command. Global functions - i.e. either built-in functions or user-defined functions in movie scripts - are always methods of the core '_movie' object, for other object functions (methods) also the object has to be specified. Here as example an implementation of a generic "map" function:


```lingo
-- in some movie script
----------------------------------------
-- Runs provided function (of some object) on all elements of the provided list, returns results as new list
-- @param {list} aList
-- @param {symbol} cbFunc
-- @param {object} [cbObj=_movie]
-- @return {list}
----------------------------------------
on map (aList, cbFunc, cbObj)
  if voidP(cbObj) then cbObj = _movie
  res = []
  cnt = aList.count
  repeat with i = 1 to cnt
    res[i] = call(cbFunc, cbObj, aList[i])
  end repeat
  return res
end
```



```lingo
l = [1, 2, 3]

-- passes the built-in function 'sin' (which is a method of the _movie object) as argument to map
res = map(l, #sin)

put res
-- [0.8415, 0.9093, 0.1411]
```



## Logo

You can pass the quoted symbol for the function and invoke it with RUN.

```logo
to printstuff
  print "stuff
end
to runstuff :proc
  run :proc
end
runstuff "printstuff    ; stuff
runstuff [print [also stuff]]  ; also stuff
```



## Lily


```lily
define square(x: Integer): Integer
{
    return x * x
}

var l = [1, 2, 3] # Inferred type: List[Integer].

# Transform using a user-defined function.
print(l.map(square)) # [1, 4, 9]

# Using a built-in method this time.
print(l.map(Integer.to_s)) # ["1", "2", "3"]

# Using a lambda (with the type of 'x' properly inferred).
print(l.map{|x| (x + 1).to_s()}) # ["2", "3", "4"]

# In reverse order using F#-styled pipes.
Boolean.to_i |> [true, false].map |> print

define apply[A, B](value: A, f: Function(A => B)): B
{
    return f(value)
}

# Calling user-defined transformation.
print(apply("123", String.parse_i)) # Some(123)
```



## Lua

Lua functions are first-class:

```lua
a = function() return 1 end
b = function(r) print( r() ) end
b(a)
```



## Luck

Higher-order functions can be used to implement conditional expressions:

```luck
function lambda_true(x: 'a)(y: 'a): 'a = x;;
function lambda_false(x: 'a)(y: 'a): 'a = y;;
function lambda_if(c:'a -> 'a -> 'a )(t: 'a)(f: 'a): 'a = c(t)(f);;

print( lambda_if(lambda_true)("condition was true")("condition was false") );;
```



## M2000 Interpreter

We can pass by reference a standard function, or we can pass by value a lambda function (also we can pass by reference as reference to lambda function)

```M2000 Interpreter

Function Foo (x) {
      =x**2
}
Function Bar(&f(), k) {
      =f(k)
}
Print Bar(&foo(), 20)=400
Group K {
      Z=10
      Function MulZ(x) {
            =.Z*x
            .Z++
      }
}
Print Bar(&K.MulZ(), 20)=200
Print K.Z=11

```


Example using lambda function


```M2000 Interpreter

Foo = Lambda k=1 (x)-> {
      k+=2
      =x**2+K
}
\\ by ref1
Function Bar1(&f(), k) {
      =f(k)
}
Print Bar1(&Foo(), 20)=403
\\ by ref2
Function Bar2(&f, k) {
      =f(k)
}
Print Bar2(&Foo, 20)=405
\\ by value
Function Bar(f, k) {
      =f(k)
}
\\ we sent a copy of lambda, and any value type closure copied too
Print Bar(Foo, 20)=407
Print Bar1(&Foo(), 20)=407
\\ we can get a copy of Foo to NewFoo (also we get a copy of closure too)
NewFoo=Foo
Print Bar1(&Foo(), 20)=409
Print Bar2(&Foo, 20)=411
Print Bar2(&NewFoo, 20)=409


```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Passing 3 arguments and a value (could be a number, variable, graphic or a function as well, actually it could be ''anything''), and composing them in an unusual way:

```Mathematica
PassFunc[f_, g_, h_, x_] := f[g[x]*h[x]]
PassFunc[Tan, Cos, Sin, x]
% /. x -> 0.12
PassFunc[Tan, Cos, Sin, 0.12]
```

gives back:

```Mathematica
Tan[Cos[x] Sin[x]]
0.119414
0.119414
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
   F1=@sin;	% F1 refers to function sin()
   F2=@cos;	% F2 refers to function cos()

   % varios ways to call the referred function 	
   F1(pi/4)
   F2(pi/4)
   feval(@sin,pi/4)
   feval(@cos,pi/4)
   feval(F1,pi/4)
   feval(F2,pi/4)

   % named functions, stored as strings	
   feval('sin',pi/4)
   feval('cos',pi/4)
   F3 = 'sin';
   F4 = 'cos';
   feval(F3,pi/4)
   feval(F4,pi/4)
```



## Maxima


```maxima
callee(n) := (print(sconcat("called with ", n)), n + 1)$
caller(f, n) := sum(f(i), i, 1, n)$
caller(callee, 3);
"called with 1"
"called with 2"
"called with 3"
```



## MAXScript


```maxscript
fn second =
(
    print "Second"
)

fn first func =
(
    func()
)

first second
```



## Metafont


We can simulate this by using <code>scantokens</code>, which ''digests'' a string as if it would be a source input.


```metafont
def calcit(expr v, s) = scantokens(s & decimal v) enddef;

t := calcit(100.4, "sind");
show t;
end
```


=={{header|МК-61/52}}==
<lang>6	ПП	04
П7	КПП7	В/О
1	В/О
```


''Note'': as the receiver of argument used register ''Р7''; the result is "1" on the indicator.

=={{header|Modula-3}}==

```modula3
MODULE Proc EXPORTS Main;

IMPORT IO;

TYPE Proc = PROCEDURE();

PROCEDURE Second() =
  BEGIN
    IO.Put("Second procedure.\n");
  END Second;

PROCEDURE First(proc: Proc) =
  BEGIN
    proc();
  END First;

BEGIN
  First(Second);
END Proc.
```



## Morfa

{{trans|D}}

```morfa

func g(a: int, b: int, f: func(int,int): int): int
{
    return f(a, b);
}

import morfa.base;

func main(): void
{
    println("Add: ", g(2, 3, func(a: int, b: int) { return a + b; }));
    println("Multiply: ", g(2, 3, func(a: int, b: int) { return a * b; }));
}

```



## Nemerle

Functions must declare the types of their parameters in Nemerle.  Function types in Nemerle are written ''params type'' -> ''return type'', as seen in the simple example below.

```Nemerle
Twice[T] (f : T -> T, x : T) : T { f(f(x)) }
```



## NewLISP


```NewLISP>
 (define (my-multiply a b) (* a b))
(lambda (a b) (* a b))
> (define (call-it f x y) (f x y))
(lambda (f x y) (f x y))
> (call-it my-multiply 2 3)
6

```



## Nim


```nim
proc first(fn: proc): auto =
  return fn()

proc second(): string =
  return "second"

echo first(second)
```


=={{header|Oberon-2}}==
Works with oo2c version 2

```oberon2

MODULE HOFuns;
IMPORT
  NPCT:Tools,
  Out;
TYPE
  Formatter = PROCEDURE (s: STRING; len: LONGINT): STRING;
VAR
  words: ARRAY 8 OF STRING;
  
  PROCEDURE PrintWords(w: ARRAY OF STRING; format: Formatter);
  VAR
    i: INTEGER;
  BEGIN
    i := 0;
    WHILE (i < LEN(words)) DO
      Out.Object(format(words[i],16));
      INC(i)
    END;
    Out.Ln
  END PrintWords;
BEGIN
  words[0] := "Al-Andalus";
  words[1] := "contributed";
  words[2] := "significantly";
  words[3] := "to";
  words[4] := "the";
  words[5] := "field";
  words[6] := "of";
  words[7] := "medicine";
  
  PrintWords(words,Tools.AdjustLeft);
  PrintWords(words,Tools.AdjustCenter);
  PrintWords(words,Tools.AdjustRight)
END HOFuns.

```



## Objeck


```objeck

bundle Default {
  class HighOrder {
    function : Main(args : String[]) ~ Nil {
      f := GetSize(String) ~ Int;
      Print(f);
    }

    function : GetSize(s : String) ~ Int {
      return s->Size();
    }

    function : Print(func : (String)~Int) ~ Nil {
      func("Hello World!")->PrintLine();
    }
  }
}

```



## OCaml


A function is just a value that wants arguments:

```ocaml
# let func1 f = f "a string";;
val func1 : (string -> 'a) -> 'a = <fun>
# let func2 s = "func2 called with " ^ s;;
val func2 : string -> string = <fun>

# print_endline (func1 func2);;
func2 called with a string
- : unit = ()
```


Or, with an anonymous function:

```ocaml
# let func f = f 1 2;;
val func : (int -> int -> 'a) -> 'a = <fun>

# Printf.printf "%d\n" (func (fun x y -> x + y));;
3
- : unit = ()
```

Note that <tt>func (fun x y -> x + y)</tt> is equivalent to <tt>func (+)</tt>. (Operators are functions too.)


## Octave


We can pass a function handle (<code>@function_name</code>)


```octave
function r = computeit(f, g, v)
  r = f(g(v));
endfunction

computeit(@exp, @sin, pi/3)
computeit(@log, @cos, pi/6)
```


Or pass the string name of the function and use the <code>feval</code> primitive.


```octave
function r = computeit2(f, g, v)
  r = f(feval(g, v));
endfunction

computeit2(@exp, "sin", pi/3)
```



## Oforth


If you add # before a function or method name you push the function object on the stack (instead of performing the function). This allows to pass functions to other functions, as for any other object.
Here we pass #1+ to map :

```Oforth
[1, 2, 3, 4, 5 ] map(#1+)
```



## Ol


```scheme

; typical use:
(for-each display '(1 2 "ss" '(3 4) 8))
; ==> 12ss(quote (3 4))8'()

; manual implementation in details:
(define (do f x)
   (f x))
(do print 12345)
; ==> 12345

```



## ooRexx

routines are first class ooRexx objects that can be passed to other routines or methods and invoked. 

```ooRexx
say callit(.routines~fib, 10)
say callit(.routines~fact, 6)
say callit(.routines~square, 13)
say callit(.routines~cube, 3)
say callit(.routines~reverse, 721)
say callit(.routines~sumit, 1, 2)
say callit(.routines~sumit, 2, 4, 6, 8)

-- call the provided routine object with the provided variable number of arguments
::routine callit
  use arg function
  args = arg(2, 'a')   -- get all arguments after the first to pass along
  return function~callWith(args)  -- and pass along the call

::routine cube
  use arg n
  return n**3

::routine square
  use arg n
  return n**2

::routine reverse
  use arg n
  return reverse(n)

::routine fact
   use arg n
   accum = 1
   loop j = 2 to n
     accum = accum * j
   end
   return accum

::routine sumit
  use arg n
  accum = 0
  do i over arg(1, 'a')  -- iterate over the array of args
     accum += i
  end
  return accum

::routine fib
  use arg n
  if n == 0 then
     return n
  if n == 1 then
     return n
  last = 0
  next = 1
  loop j = 2 to n;
    current = last + next
    last = next
    next = current
  end
  return current
```

{{out}}

```txt
55
720
169
27
127
3
20
```



## Order


Functions in Order can accept any other named function, local variable, or anonymous function as arguments:


```C

#include <order/interpreter.h>

#define ORDER_PP_DEF_8func1 ORDER_PP_FN ( \
8fn(8F, \
    8ap(8F, 8("a string")) ))

#define ORDER_PP_DEF_8func2 ORDER_PP_FN ( \
8fn(8S, \
    8adjoin(8("func2 called with "), 8S ) ))

ORDER_PP(
  8func1(8func2)
)
// -> "func2 called with ""a string"

#define ORDER_PP_DEF_8func3 ORDER_PP_FN ( \
8fn(8F, \
    8ap(8F, 1, 2) ))

ORDER_PP(
  8func3(8plus)
)
// -> 3

ORDER_PP(
  8ap( 8fn(8X, 8Y, 8mul(8add(8X, 8Y), 8sub(8X, 8Y))), 5, 3)
)
// -> 16

```


The only difference between toplevel function definitions, and variables or literals, is that variables and anonymous functions must be called using the <code>8ap</code> syntactic form rather than direct argument application syntax. This is a limitation of the C preprocessor.


## OxygenBasic


```oxygenbasic

'FUNCTION TO BE PASSED
'
### ===============


function f(double d,e) as double
  return (d+e)*2
end function


'FUNCTION TAKING A FUNCTION AS AN ARGUMENT
'
### ===================================


function g(sys p) as string

  declare function x(double d,e) as double at p

  return x(10,11)

end function


'TEST: PASSING ADDRESS OF FUNCTION f
'
### =============================


'the name 'f' is combined with the prototype signature '#double#double'
'@' signifies the address of the function is being passed

print g(@f#double#double) 'result '42'


```



## Oz

Functions are just regular values in Oz.

```oz
declare
  fun {Twice Function X}
     {Function {Function X}}
  end
in
  {Show {Twice Sqrt 81.0}}  %% prints 3.0
```



## PARI/GP

{{works with|PARI/GP|2.4.2 and above}} <!-- requires closures -->

```parigp
secant_root(ff,a,b)={
	e = eps() * 2;
	aval=ff(a);
	bval=ff(b);
	while (abs(bval) > e,
		oldb = b;
		b = b - (b - a)/(bval - aval) * bval;
		aval = bval;
		bval = ff(b);
		a = oldb
	);
	b
};
addhelp(secant_root, "secant_root(ff,a,b): Finds a root of ff between a and b using the secant method.");

eps()={
	precision(2. >> (32 * ceil(default(realprecision) * 38539962 / 371253907)), 9)
};
addhelp(eps,"Returns machine epsilon for the current precision.");
```



## Pascal

Standard Pascal (will not work with Turbo Pascal):

```pascal
program example(output);

function first(function f(x: real): real): real;
 begin
  first := f(1.0) + 2.0;
 end;

function second(x: real): real;
 begin
  second := x/2.0;
 end;

begin
 writeln(first(second));
end.
```


[[Turbo Pascal]] (will not work with Standard Pascal):


```pascal
program example;

type
   FnType = function(x: real): real;

function first(f: FnType): real;
begin
   first := f(1.0) + 2.0;
end;

{$F+}
function second(x: real): real;
begin
   second := x/2.0;
end;
{$F-}

begin
   writeln(first(second));
end.
```



## Perl


```perl
sub another {
    # take a function and a value
    my $func = shift;
    my $val  = shift;

    # call the function with the value as argument
    return $func->($val);
};

sub reverser {
    return scalar reverse shift;
};

# pass named coderef
print another \&reverser, 'data';
# pass anonymous coderef
print another sub {return scalar reverse shift}, 'data';

# if all you have is a string and you want to act on that,
# set up a dispatch table
my %dispatch = (
    square => sub {return shift() ** 2},
    cube   => sub {return shift() ** 3},
    rev    => \&reverser,
);

print another $dispatch{$_}, 123 for qw(square cube rev);
```



```perl
sub apply (&@) {            # use & as the first item in a prototype to take bare blocks like map and grep
    my ($sub, @ret) = @_;   # this function applies a function that is expected to modify $_ to a list
    $sub->() for @ret;      # it allows for simple inline application of the s/// and tr/// constructs
    @ret
}

print join ", " => apply {tr/aeiou/AEIOU/} qw/one two three four/;
# OnE, twO, thrEE, fOUr
```



```perl
sub first {shift->()}

sub second {'second'}

print first \&second;

print first sub{'sub'};
```



## Perl 6

The best type to use for the parameter of a higher-order function is <code>Callable</code> (implied by the <code>&</code> sigil), a role common to all function-like objects. For an example of defining and calling a second-order function, see [[Functional Composition#Perl_6|Functional Composition]].

Convenient syntax is provided for anonymous functions,
either a bare block, or a parameterized block introduced with <tt>-></tt>, which serves as a "lambda":


```perl6
sub twice(&todo) {
   todo(); todo(); # declaring &todo also defines bare function
}
twice { say "Boing!" }
# output:
# Boing!
# Boing!

sub twice-with-param(&todo) {
    todo(0); todo(1);
}
twice-with-param -> $time {
   say "{$time+1}: Hello!"
}
# output:
# 1: Hello!
# 2: Hello!
```



## Phix

Copy of [[Higher-order_functions#Euphoria|Euphoria]]

```Phix
procedure use(integer fi, integer a, integer b)
    print(1,call_func(fi,{a,b}))
end procedure
 
function add(integer a, integer b)
    return a + b
end function
 
use(routine_id("add"),23,45)
```

{{out}}

```txt

68

```



## PHP


```php
function first($func) {
  return $func();
}

function second() {
  return 'second';
}

$result = first('second');
```

Or, with an anonymous function in PHP 5.3+:

```php
function first($func) {
  return $func();
}

$result = first(function() { return 'second'; });
```



## PicoLisp


```PicoLisp
: (de first (Fun)
   (Fun) )
-> first

: (de second ()
   "second" )
-> second

: (first second)
-> "second"

: (de add (A B)
   (+ A B) )
-> add

: (add 1 2)
-> 3

: (de call-it (Fun X Y)
   (Fun X Y) )
-> call-it

: (call-it add 1 2)
-> 3

: (mapcar inc (1 2 3 4 5))
-> (2 3 4 5 6)

: (mapcar + (1 2 3) (4 5 6))
-> (5 7 9)

:  (mapcar add (1 2 3) (4 5 6))
-> (5 7 9)
```




## PL/I


```PL/I

f: procedure (g) returns (float);
   declare g entry (float);

   get (x);
   put (g(x));
end f;

   x = f(p); /* where "p" is the name of a function. */

```



## Pop11


```pop11
;;; Define a function
define x_times_three_minus_1(x);
  return(3*x-1);
enddefine;

;;; Pass it as argument to built-in function map and print the result
mapdata({0 1 2 3 4}, x_times_three_minus_1) =>
```



## PostScript

Postscript functions are either built-in operators or executable arrays (procedures). Both can take either as arguments.
<lang>
% operator example
% 'ifelse' is passed a boolean and two procedures
/a 5 def
a 0 gt { (Hello!) } { (World?) } ifelse ==

% procedure example
% 'bar' is loaded onto the stack and passed to 'foo'
/foo { exec } def
/bar { (Hello, world!) } def
/bar load foo ==

```



## PowerShell

{{works with|PowerShell|4.0}}

```PowerShell

function f ($y)  {
    $y*$y
}
function g (${function:f}, $y) {
    (f $y)
}

```


You can implement a function inside a function.


```PowerShell

function g2($y) {
    function f2($y)  {
        $y*$y
    }
    (f2 $y)
}

```

<b>Calling:</b>

```PowerShell

g f 5
g2 9

```

<b>Output:</b>

```txt

25
81

```



## Prolog


```prolog

first(Predicate) :- call(Predicate).
second(Argument) :- write(Argument).

:-first(second('Hello World!')).

```



## PureBasic


```PureBasic
Prototype.d func(*text$)

Procedure NumberTwo(arg$)
  Debug arg$
EndProcedure

Procedure NumberOne(*p, text$)
  Define MyFunc.func=*p
  MyFunc(@text$)
EndProcedure

NumberOne(@NumberTwo(),"Hello Worldy!")
```



## Python

{{works with|Python|2.5}}


```python
def first(function):
    return function()

def second():
    return "second"

result = first(second)
```


or


```python
  result = first(lambda: "second")
```


Functions are first class objects in Python.  They can be bound to names ("assigned" to "variables"), associated with keys in dictionaries, and passed around like any other object.


## Q


Its helpful to remember that in Q, when parameters aren't named in the function declaration, <tt>x</tt> is assumed to be the first parameter.


```Q

q)sayHi:{-1"Hello ",x;}
q)callFuncWithParam:{x["Peter"]}
q)callFuncWithParam sayHi
Hello Peter
q)callFuncWithParam[sayHi]
Hello Peter
```



## R


```R
f <- function(f0) f0(pi) # calc. the function in pi
tf <- function(x) x^pi   # a func. just to test

print(f(sin))
print(f(cos))
print(f(tf))
```



## Racket


```Racket

  #lang racket/base
  (define (add f g x)
    (+ (f x) (g x)))
  (add sin cos 10)

```



## Raven

This is not strictly passing a function, but the string representing the function name.

```Raven
define doit use $v1
   "doit called with " print $v1 print "\n" print

define callit use $v2
   "callit called with " print $v2 print "\n" print
   $v2 call

23.54 "doit" callit
```

{{out}}

```txt
callit called with doit
doit called with 23.54

```


## REBOL


```REBOL
REBOL [
	Title: "Function Argument"
	URL: http://rosettacode.org/wiki/Function_as_an_Argument
]

map: func [
	"Apply function to contents of list, return new list."
	f [function!] "Function to apply to list."
	data [block! list!] "List to transform."
	/local result i
][
	result: copy []  repeat i data [append result f i]  result]

square: func [
	"Calculate x^2."
	x [number!]
][x * x]

cube: func [
	"Calculate x^3."
	x [number!]
][x * x * x]

; Testing:

x: [1 2 3 4 5]
print ["Data:   "  mold x]
print ["Squared:"  mold map :square x]
print ["Cubed:  "  mold map :cube x]
print ["Unnamed:"  mold map func [i][i * 2 + 1] x]
```


Output: 


```txt
Data:    [1 2 3 4 5]
Squared: [1 4 9 16 25]
Cubed:   [1 8 27 64 125]
Unnamed: [3 5 7 9 11]
```



## Retro


```Retro
:disp (nq-)  call n:put ;

#31 [ (n-n) #100 * ] disp

```



## REXX


```rexx
/*REXX program demonstrates the  passing of a  name of a function  to another function. */
call function  'fact'   ,   6;           say right(    'fact{'$"} = ", 30)    result
call function  'square' ,  13;           say right(  'square{'$"} = ", 30)    result
call function  'cube'   ,   3;           say right(    'cube{'$"} = ", 30)    result
call function  'reverse', 721;           say right( 'reverse{'$"} = ", 30)    result
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cube:     return $**3
fact:     procedure expose $;  !=1;      do j=2  to $;    !=!*j;     end;         return !
function: arg ?.;   parse arg ,$;        signal value (?.)
reverse:  return 'REVERSE'($)
square:   return $**2
```

{{out|output|text=  when using the default input:}}

```txt

                    fact{6} =  720
                 square{13} =  169
                    cube{3} =  27
               reverse{721} =  127

```



## Ring


```ring

# Project : Higher-order functions

docalcs(1,10,"squares",:square)
docalcs(1,10,"cubes",:cube)

func square(n)  
        return n * n
 
func cube(n)
        return n * n * n
 
func docalcs(from2,upto,title,func2)
       see title + " -> " + nl
       for i = from2 to upto
            x = call func2(i)
            see x + nl
       next
       see nl 

```

Output:

```txt

squares -> 
1
4
9
16
25
36
49
64
81
100

cubes -> 
1
8
27
64
125
216
343
512
729
1000

```



## Ruby

With a proc (procedure):

```ruby
succ = proc{|x| x+1}
def to2(&f)
  f[2]
end

to2(&succ) #=> 3
to2{|x| x+1} #=> 3
```

  
With a method:

```ruby
def succ(n)
  n+1
end
def to2(m)
  m[2]
end

meth = method(:succ)
to2(meth) #=> 3
```




## Rust

Functions are first class values and identified in the type system by implementing the FnOnce, FnMut or the Fn trait which happens implicitly for functions and closures.

```rust
fn execute_with_10<F: Fn(u64) -> u64> (f: F) -> u64 {
	f(10)
}

fn square(n: u64) -> u64 {
	n*n
}

fn main() {
	println!("{}", execute_with_10(|n| n*n )); // closure
	println!("{}", execute_with_10(square));   // function
}
```

{{out}}

```txt
100
100
```


## Scala


```scala
def functionWithAFunctionArgument(x : int, y : int, f : (int, int) => int) = f(x,y)
```

Call:

```scala
functionWithAFunctionArgument(3, 5, {(x, y) => x + y}) // returns 8
```



## Scheme


A function is just a value that wants arguments:

```scheme>
 (define (func1 f) (f "a string"))
> (define (func2 s) (string-append "func2 called with " s))
> (begin (display (func1 func2)) (newline))
func2 called with a string
```


Or, with an anonymous function:

```scheme>
 (define (func f) (f 1 2))
> (begin (display (func (lambda (x y) (+ x y)))) (newline))
3
```

Note that <tt>(func (lambda (x y) (+ x y)))</tt> is equivalent to <tt>(func +)</tt>. (Operators are functions too.)


## Sidef


```ruby
func first(f) {
  return f();
}

func second {
  return "second";
}

say first(second);              # => "second"
say first(func { "third" });    # => "third"
```



## Slate

Methods and blocks can both be passed as arguments to functions (other methods and blocks):

```slate
define: #function -> [| :x | x * 3 - 1].
#(1 1 2 3 5 8) collect: function.
```



## Smalltalk



```Smalltalk
first := [ :f | f value ].
second := [ 'second' ].
Transcript show: (first value: second).
```


```Smalltalk
function := [:x | x * 3 - 1].
#(1 1 2 3 5 8) collect: function.
```



## Sparkling


```sparkling
function call_me(func, arg) {
    return func(arg);
}

let answer = call_me(function(x) { return 6 * x; }, 7);
print(answer);
```



## Standard ML



```sml
- fun func1 f = f "a string";
val func1 = fn : (string -> 'a) -> 'a
- fun func2 s = "func2 called with " ^ s;
val func2 = fn : string -> string

- print (func1 func2 ^ "\n");
func2 called with a string
val it = () : unit
```


Or, with an anonymous function:

```sml
- fun func f = f (1, 2);
val func = fn : (int * int -> 'a) -> 'a

- print (Int.toString (func (fn (x, y) => x + y)) ^ "\n");
3
val it = () : unit
```

Note that <tt>func (fn (x, y) => x + y)</tt> is equivalent to <tt>func op+</tt>. (Operators are functions too.)


## SuperCollider


```SuperCollider
f = { |x, y| x.(y) }; // a function that takes a function and calls it with an argument
f.({ |x| x + 1 }, 5); // returns 5
```



## Swift


```swift
func func1(f: String->String) -> String { return f("a string") }
func func2(s: String) -> String { return "func2 called with " + s }
println(func1(func2)) // prints "func2 called with a string"
```


Or, with an anonymous function:

```swift>func func3<T
(f: (Int,Int)->T) -> T { return f(1, 2) }
println(func3 {(x, y) in x + y}) // prints "3"
```

Note that <tt>{(x, y) in x + y}</tt> can also be written as <tt>{$0 + $1}</tt> or just <tt>+</tt>.


## Tcl


```tcl
# this procedure executes its argument:
proc demo {function} { 
    $function 
}
# for example:
demo bell
```

It is more common to pass not just a function, but a command fragment or entire script. When used with the built-in <tt>list</tt> command (which introduces a very useful degree of quoting) this makes for a very common set of techniques when doing advanced Tcl programming.

```tcl
# This procedure executes its argument with an extra argument of "2"
proc demoFrag {fragment} {
    {*}$fragment 2
}
# This procedure executes its argument in the context of its caller, which is
# useful for scripts so they get the right variable resolution context
proc demoScript {script} {
    uplevel 1 $script
}

# Examples...
set chan stderr
demoFrag [list puts $chan]
demoFrag {
    apply {x {puts [string repeat ? $x]}}
}
demoScript {
    parray tcl_platform
}
```


=={{header|TI-89 BASIC}}==

TI-89 BASIC does not have first-class functions; while function ''definitions'' as stored in ''variables'' are fully dynamic, it is not possible to extract a function value from a variable rather than calling it. In this case, we use the indirection operator <code>#</code>, which takes a string and returns the value of the named variable, to use the name of the function as something to be passed.

The function name passed cannot be that of a local function, because the local function <code>map</code> does not see the local variables of the enclosing function.


```ti89b
Local map
Define map(f,l)=Func
  Return seq(#f(l[i]),i,1,dim(l))
EndFunc
Disp map("sin", {0, π/6, π/4, π/3, π/2})
```



## Toka

Toka allows obtaining a function pointer via the '''`''' (''backtick'') word. The pointers are passed on the stack, just like all other data.


```toka
[ ." First\n" ] is first
[ invoke ] is second
` first second
```



## Trith

Due to the homoiconic program representation and the [[concatenative]] nature of the language, higher-order functions are as simple as:

```trith
: twice 2 times ;
: hello "Hello, world!" print ;
[hello] twice
```



## TXR


<code>lambda</code> passed to <code>mapcar</code> with environment capture:


```txr
@(bind a @(let ((counter 0))
            (mapcar (lambda (x y) (list (inc counter) x y))
                    '(a b c) '(t r s))))
@(output)
@  (repeat)
@    (rep)@a:@(last)@a@(end)
@  (end)
@(end)
```



```txt
1:a:t
2:b:r
3:c:s
```



## Ursa

{{trans|Python}}
Functions are first-class objects in Ursa.

```ursa
def first (function f)
	return (f)
end

def second ()
	return "second"
end

out (first second) endl console
# "second" is output to the console
```



## Ursala


Autocomposition is a user defined function that
takes a function as an argument, and returns a function
equivalent to the given functon composed with itself.


```Ursala
(autocomposition "f") "x" = "f" "f" "x"
```

test program:

```Ursala
#import flo
#cast %e

example = autocomposition(sqrt) 16.0
```

output:

```txt
2.000000e+00
```




## V

Define first as multiplying two numbers on stack

```v
[first *].
```

Define second as applying the passed quote on stack

```v
[second i].
```

Pass the first enclosed in quote to second which applies it on stack.

```v
2 3 [first] second
```

 =6


## VBA

Based on the Pascal solution

```pascal
Sub HigherOrder()
    Dim result As Single
    result = first("second")
    MsgBox result
End Sub
Function first(f As String) As Single
    first = Application.Run(f, 1) + 2
End Function
Function second(x As Single) As Single
    second = x / 2
End Function
```



## Visual Basic .NET

Each example below performs the same task and utilizes .NET delegates, which are objects that refer to a static method or to an instance method of a particular object instance.

c.f. [[#C#|C#]]

{{out|note=for each example}}

```txt
f=Add, f(6, 2) = 8
f=Mul, f(6, 2) = 12
f=Div, f(6, 2) = 3
```



### Named methods

{{trans|C#: Named methods}}

```vbnet
' Delegate declaration is similar to C#.
Delegate Function Func2(a As Integer, b As Integer) As Integer

Module Program
    Function Add(a As Integer, b As Integer) As Integer
        Return a + b
    End Function

    Function Mul(a As Integer, b As Integer) As Integer
        Return a * b
    End Function

    Function Div(a As Integer, b As Integer) As Integer
        Return a \ b
    End Function

    ' Call is a keyword and must be escaped using brackets.
    Function [Call](f As Func2, a As Integer, b As Integer) As Integer
        Return f(a, b)
    End Function

    Sub Main()
        Dim a = 6
        Dim b = 2

        ' Delegates in VB.NET could be created without a New expression from the start. Both syntaxes are shown here.
        Dim add As Func2 = New Func2(AddressOf Program.Add)

        ' The "As New" idiom applies to delegate creation.
        Dim div As New Func2(AddressOf Program.Div)

        ' Directly coercing the AddressOf expression:
        Dim mul As Func2 = AddressOf Program.Mul

        Console.WriteLine("f=Add, f({0}, {1}) = {2}", a, b, [Call](add, a, b))
        Console.WriteLine("f=Mul, f({0}, {1}) = {2}", a, b, [Call](mul, a, b))
        Console.WriteLine("f=Div, f({0}, {1}) = {2}", a, b, [Call](div, a, b))
    End Sub
End Module
```



### Lambda expressions

{{trans|C#: Lambda expressions}}
Lambda expressions in VB.NET are similar to those in C#, except they can also explicitly specify a return type and exist as standalone "anonymous delegates". An anonymous delegate is created when a lambda expression is assigned to an implicitly typed variable (in which case the variable receives the type of the anonymous delegate) or when the target type given by context (at compile-time) is MulticastDelegate, Delegate, or Object. Anonymous delegates are derived from MulticastDelegate and are implicitly convertible to all compatible delegate types. A formal definition of delegate compatibility can be found in the language specification.

```vbnet
Module Program
    ' Uses the System generic delegate; see C# entry for details.
    Function [Call](f As Func(Of Integer, Integer, Integer), a As Integer, b As Integer) As Integer
        Return f(a, b)
    End Function

    Sub Main()
        Dim a = 6
        Dim b = 2

        Console.WriteLine("f=Add, f({0}, {1}) = {2}", a, b, [Call](Function(x As Integer, y As Integer) x + y, a, b))

                                                                   ' With inferred parameter types:
        Console.WriteLine("f=Mul, f({0}, {1}) = {2}", a, b, [Call](Function(x, y) x * y, a, b))

        ' The block syntax must be used in order to specify a return type. As there is no target type in this case, the parameter types must be explicitly specified. anon has an anonymous, compiler-generated type.
        Dim anon = Function(x As Integer, y As Integer) As Integer
                       Return x \ y
                   End Function

        ' Parameters are contravariant and the return type is covariant. Note that this conversion is not valid CLR variance (which disallows boxing conversions) and so is compiled as an additional anonymous delegate that explicitly boxes the return value.
        Dim example As Func(Of Integer, Integer, Object) = anon

        ' Dropped-return-type conversion.
        Dim example2 As Action(Of Integer, Integer) = anon

        Console.WriteLine("f=Div, f({0}, {1}) = {2}", a, b, [Call](anon, a, b))
    End Sub
End Module
```



## Visual Prolog



```Prolog

domains
  intFunction = (integer In) -> integer Out procedure (i).
  
class predicates
  addone : intFunction.
  doTwice : (intFunction, integer) -> integer procedure (i, i).

clauses
  doTwice(Pred,X) = Y :- Y = Pred(Pred(X)).

  addone(X) = Y := Y = X + 1.

  run():-
    init(),
    write(dotwice(addone,2)),
    succeed().

```



## zkl

Everything is a first class object so

```zkl
fcn f(g){ g() } fcn g{ "Hello World!".println() }
```

{{out}}

```txt

f(g)
"Hello World!"

```

or

```zkl
fcn f(g){ g() }
fcn(g){ g() }(fcn{ "Hello World!".println() } )
```



## ZX Spectrum Basic

{{trans|BBC_BASIC}}
Input "FN " token first, then enclose it in double quotation marks.

```zxbasic
10 DEF FN f(f$,x,y)=VAL ("FN "+f$+"("+STR$ (x)+","+STR$ (y)+")")
20 DEF FN n(x,y)=(x+y)^2
30 PRINT FN f("n",10,11)
```


{{omit from|GUISS}}

[[Category:Functions and subroutines]]
