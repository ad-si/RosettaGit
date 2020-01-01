+++
title = "Partial function application"
description = ""
date = 2019-10-18T11:48:19Z
aliases = []
[extra]
id = 9391
[taxonomies]
categories = []
tags = []
+++

{{task|Programming language concepts}}

[[wp:Partial application|Partial function application]]   is the ability to take a function of many
parameters and apply arguments to some of the parameters to create a new
function that needs only the application of the remaining arguments to
produce the equivalent of applying all arguments to the original function.

E.g:
: Given values <code>v1, v2</code>
: Given <code>f(param1, param2)</code>
: Then  <code>partial(f, param1=v1)</code> returns <code>f'(param2)</code>
: And   <code>f(param1=v1, param2=v2) == f'(param2=v2)</code> (for any value v2)


Note that in the partial application of a parameter, (in the above case param1), other parameters are not explicitly mentioned. This is a recurring feature of partial function application.


;Task
* Create a function fs( f, s ) that takes a function, f( n ), of one value and a sequence of values s.
 Function fs should return an ordered sequence of the result of applying function f to every value of s in turn.

* Create function f1 that takes a value and returns it multiplied by 2.
* Create function f2 that takes a value and returns it squared.

* Partially apply f1 to fs to form function fsf1( s )
* Partially apply f2 to fs to form function fsf2( s )

* Test fsf1 and fsf2 by evaluating them with s being the sequence of integers from 0 to 3 inclusive and then the sequence of even integers from 2 to 8 inclusive.


;Notes
* In partially applying the functions f1 or f2 to fs, there should be no ''explicit'' mention of any other parameters to fs, although introspection of fs within the partial applicator to find its parameters ''is'' allowed.
* This task is more about ''how'' results are generated rather than just getting results.





## Ada


Ada allows to define generic functions with generic parameters, which are partially applicable.


```Ada
with Ada.Text_IO;

procedure Partial_Function_Application is

   type Sequence is array(Positive range <>) of Integer;

   -- declare a function FS with a generic parameter F and a normal parameter S
   generic
      with function F(I: Integer) return Integer; -- generic parameter
   function FS (S: Sequence) return Sequence;

   -- define FS
   function FS (S: Sequence) return Sequence is
      Result: Sequence(S'First .. S'Last);
   begin
      for Idx in S'Range loop
         Result(Idx) := F(S(Idx));
      end loop;
      return Result;
   end FS;

   -- define functions F1 and F2
   function F1(I: Integer) return Integer is
   begin
      return 2*I;
   end F1;

   function F2(I: Integer) return Integer is
   begin
      return I**2;
   end F2;

   -- instantiate the function FS by F1 and F2 (partially apply F1 and F2 to FS)
   function FSF1 is new FS(F1);
   function FSF2 is new FS(F2);

   procedure Print(S: Sequence) is
   begin
      for Idx in S'Range loop
         Ada.Text_IO.Put(Integer'Image(S(Idx)));
      end loop;
      Ada.Text_IO.New_Line;
   end Print;

begin
   Print(FSF1((0,1,2,3)));
   Print(FSF2((0,1,2,3)));
   Print(FSF1((2,4,6,8)));
   Print(FSF2((2,4,6,8)));
end Partial_Function_Application;
```


Output:


```txt
 0 2 4 6
 0 1 4 9
 4 8 12 16
 4 16 36 64
```



## ALGOL 68

{{trans|Python}}
{{works with|ALGOL 68|Revision 1 - Requires [[Currying]] extensions to language.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
MODE SET = FLEX[0]INT;

MODE F = PROC(INT)INT,
     FS = PROC(SET)SET;

PROC fs = (F f, SET set)SET: (
  [LWB set:UPB set]INT out;
  FOR i FROM LWB set TO UPB set DO out[i]:=f(set[i]) OD;
  out
);

PROC f1 = (INT value)INT: value * 2,
     f2 = (INT value)INT: value ** 2;

FS fsf1 = fs(f1,),
   fsf2 = fs(f2,);

[4]INT set;
FORMAT set fmt = $"("n(UPB set-LWB set)(g(0)", ")g(0)")"l$;

set := (0, 1, 2, 3);
  printf((set fmt, fsf1((0, 1, 2, 3)))); # prints (0, 2, 4, 6) #
  printf((set fmt, fsf2((0, 1, 2, 3)))); # prints (0, 1, 4, 9) #

set := (2, 4, 6, 8);
  printf((set fmt, fsf1((2, 4, 6, 8)))); # prints (4, 8, 12, 16) #
  printf((set fmt, fsf2((2, 4, 6, 8))))  # prints (4, 16, 36, 64) #

```

Output:

```txt

(0, 2, 4, 6)
(0, 1, 4, 9)
(4, 8, 12, 16)
(4, 16, 36, 64)

```



## AppleScript

To derive first class functions in AppleScript, we have to lift ordinary handlers into script objects with lambda handlers.


```AppleScript
-- PARTIAL APPLICATION --------------------------------------------

on f1(x)
    x * 2
end f1

on f2(x)
    x * x
end f2

on run

    tell curry(map)
        set fsf1 to |λ|(f1)
        set fsf2 to |λ|(f2)
    end tell

    {fsf1's |λ|({0, 1, 2, 3}), ¬
        fsf2's |λ|({0, 1, 2, 3}), ¬
        fsf1's |λ|({2, 4, 6, 8}), ¬
        fsf2's |λ|({2, 4, 6, 8})}
end run


-- GENERIC FUNCTIONS --------------------------------------------

-- curry :: (Script|Handler) -> Script
on curry(f)
    script
        on |λ|(a)
            script
                on |λ|(b)
                    |λ|(a, b) of mReturn(f)
                end |λ|
            end script
        end |λ|
    end script
end curry

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
```

{{Out}}

```txt
{{0, 2, 4, 6}, {0, 1, 4, 9}, {4, 8, 12, 16}, {4, 16, 36, 64}}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      fsf1 = FNpartial(PROCfs(), FNf1())
      fsf2 = FNpartial(PROCfs(), FNf2())

      DIM seq(3)
      PRINT "Calling function fsf1 with sequence 1:"
      seq() = 0, 1, 2, 3 : PROC(fsf1)(seq())
      FOR i% = 0 TO 3 : PRINT seq(i%); : NEXT : PRINT
      PRINT "Calling function fsf1 with sequence 2:"
      seq() = 2, 4, 6, 8 : PROC(fsf1)(seq())
      FOR i% = 0 TO 3 : PRINT seq(i%); : NEXT : PRINT
      PRINT "Calling function fsf2 with sequence 1:"
      seq() = 0, 1, 2, 3 : PROC(fsf2)(seq())
      FOR i% = 0 TO 3 : PRINT seq(i%); : NEXT : PRINT
      PRINT "Calling function fsf2 with sequence 2:"
      seq() = 2, 4, 6, 8 : PROC(fsf2)(seq())
      FOR i% = 0 TO 3 : PRINT seq(i%); : NEXT : PRINT
      END

      REM Create a partial function:
      DEF FNpartial(RETURN f1%, RETURN f2%)
      LOCAL f$, p%
      DIM p% 7 : p%!0 = f1% : p%!4 = f2%
      f$ = "(s())" + CHR$&F2 + "(&" + STR$~p% + ")(" + \
      \              CHR$&A4 + "(&" + STR$~(p%+4) + ")(),s()):" + CHR$&E1
      DIM p% LEN(f$) + 4
      $(p%+4) = f$ : !p% = p%+4
      = p%

      REM Replaces the input sequence with the output sequence:
      DEF PROCfs(RETURN f%, seq())
      LOCAL i%
      FOR i% = 0 TO DIM(seq(),1)
        seq(i%) = FN(^f%)(seq(i%))
      NEXT
      ENDPROC

      DEF FNf1(n) = n * 2

      DEF FNf2(n) = n ^ 2
```

'''Output:'''

```txt

Calling function fsf1 with sequence 1:
         0         2         4         6
Calling function fsf1 with sequence 2:
         4         8        12        16
Calling function fsf2 with sequence 1:
         0         1         4         9
Calling function fsf2 with sequence 2:
         4        16        36        64

```



## Bracmat

This task is hard to solve if we use imperative/procedural style Bracmat functions. Instead, we use lambda expressions throughout the solution given below.
The the function <code>fs</code> consists of a lambda abstraction inside a lambda abstraction. In that way <code>fs</code> can take two arguments. Similarly, the function <code>partial</code>, which also needs to take two arguments, is defined using lambda abstractions.
Currying takes place by applying a two-argument function to its first argument. This happens in <code>($x)$($y)</code>.

```bracmat
( (fs=/('(x./('(y.map'($x.$y))))))
& (f1=/('(x.$x*2)))
& (f2=/('(x.$x^2)))
& (partial=/('(x./('(y.($x)$($y))))))
& (!partial$!fs)$!f1:?fsf1
& (!partial$!fs)$!f2:?fsf2
& out$(!fsf1$(0 1 2 3))
& out$(!fsf2$(0 1 2 3))
& out$(!fsf1$(2 4 6 8))
& out$(!fsf2$(2 4 6 8))
);
```

Output:

```txt
0 2 4 6
0 1 4 9
4 8 12 16
4 16 36 64
```



## Clojure


```Clojure
(defn fs [f s] (map f s))
(defn f1 [x] (* 2 x))
(defn f2 [x] (* x x))
(def fsf1 (partial fs f1))
(def fsf2 (partial fs f2))

(doseq [s [(range 4) (range 2 9 2)]]
  (println "seq: " s)
  (println "  fsf1: " (fsf1 s))
  (println "  fsf2: " (fsf2 s)))
```

Output:

```txt
seq:  (0 1 2 3)
  fsf1:  (0 2 4 6)
  fsf2:  (0 1 4 9)
seq:  (2 4 6 8)
  fsf1:  (4 8 12 16)
  fsf2:  (4 16 36 64)
```



## Common Lisp


```lisp
(defun fs (f s)
  (mapcar f s))
(defun f1 (i)
  (* i 2))
(defun f2 (i)
  (expt i 2))

(defun partial (func &rest args1)
  (lambda (&rest args2)
    (apply func (append args1 args2))))

(setf (symbol-function 'fsf1) (partial #'fs #'f1))
(setf (symbol-function 'fsf2) (partial #'fs #'f2))

(dolist (seq '((0 1 2 3) (2 4 6 8)))
  (format t
          "~%seq: ~A~%  fsf1 seq: ~A~%  fsf2 seq: ~A"
	  seq
          (fsf1 seq)
          (fsf2 seq)))

```


Output:
```txt
seq: (0 1 2 3)
  fsf1 seq: (0 2 4 6)
  fsf2 seq: (0 1 4 9)
seq: (2 4 6 8)
  fsf1 seq: (4 8 12 16)
  fsf2 seq: (4 16 36 64)
```



## C

Nasty hack, but the partial does return a true C function pointer, which is otherwise hard to achieve. (In case you are wondering, no, this is not a good or serious solution.)  Compiled with <code>gcc -Wall -ldl</code>.

```c
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <sys/wait.h>
#include <err.h>

typedef int (*intfunc)(int);
typedef void (*pfunc)(int*, int);

pfunc partial(intfunc fin)
{
	pfunc f;
	static int idx = 0;
	char cc[256], lib[256];
	FILE *fp;
	sprintf(lib, "/tmp/stuff%d.so", ++idx);
	sprintf(cc, "cc -pipe -x c -shared -o %s -", lib);

	fp = popen(cc, "w");
	fprintf(fp, "#define t typedef\xat int _i,*i;t _i(*__)(_i);__ p =(__)%p;"
		"void _(i _1, _i l){while(--l>-1)l[_1]=p(l[_1]);}", fin);
	fclose(fp);

	*(void **)(&f) = dlsym(dlopen(lib, RTLD_LAZY), "_");
	unlink(lib);
	return f;
}

int square(int a)
{
	return a * a;
}

int dbl(int a)
{
	return a + a;
}

int main()
{
	int x[] = { 1, 2, 3, 4 };
	int y[] = { 1, 2, 3, 4 };
	int i;

	pfunc f = partial(square);
	pfunc g = partial(dbl);

	printf("partial square:\n");
	f(x, 4);
	for (i = 0; i < 4; i++) printf("%d\n", x[i]);

	printf("partial double:\n");
	g(y, 4);
	for (i = 0; i < 4; i++) printf("%d\n", y[i]);

	return 0;
}
```
output:

```txt
partial square:
1
4
9
16
partial double:
2
4
6
8
```


## C++

```cpp
#include <utility>
// For declval.
#include <algorithm>
#include <array>
#include <iterator>
#include <iostream>

/* Partial application helper. */
template< class F, class Arg >
struct PApply
{
    F f;
    Arg arg;

    template< class F_, class Arg_ >
    PApply( F_&& f, Arg_&& arg )
        : f(std::forward<F_>(f)), arg(std::forward<Arg_>(arg))
    {
    }

    /*
     * The return type of F only gets deduced based on the number of arguments
     * supplied. PApply otherwise has no idea whether f takes 1 or 10 args.
     */
    template< class ... Args >
    auto operator() ( Args&& ...args )
        -> decltype( f(arg,std::declval<Args>()...) )
    {
        return f( arg, std::forward<Args>(args)... );
    }
};

template< class F, class Arg >
PApply<F,Arg> papply( F&& f, Arg&& arg )
{
    return PApply<F,Arg>( std::forward<F>(f), std::forward<Arg>(arg) );
}

/* Apply f to cont. */
template< class F >
std::array<int,4> fs( F&& f, std::array<int,4> cont )
{
    std::transform( std::begin(cont), std::end(cont), std::begin(cont),
                    std::forward<F>(f) );
    return cont;
}

std::ostream& operator << ( std::ostream& out, const std::array<int,4>& c )
{
    std::copy( std::begin(c), std::end(c),
               std::ostream_iterator<int>(out, ", ") );
    return out;
}

int f1( int x ) { return x * 2; }
int f2( int x ) { return x * x; }

int main()
{
    std::array<int,4> xs = {{ 0, 1, 2, 3 }};
    std::array<int,4> ys = {{ 2, 4, 6, 8 }};

    auto fsf1 = papply( fs<decltype(f1)>, f1 );
    auto fsf2 = papply( fs<decltype(f2)>, f2 );

    std::cout << "xs:\n"
              << "\tfsf1: " << fsf1(xs) << '\n'
              << "\tfsf2: " << fsf2(xs) << "\n\n"
              << "ys:\n"
              << "\tfsf1: " << fsf1(ys) << '\n'
              << "\tfsf2: " << fsf2(ys) << '\n';
}
```



## C sharp



### First approach


A partial application function for binary functions.


```csharp
using System;
using System.Collections.Generic;
using System.Linq;

class PartialFunctionApplication
{
    static Func<T1, TResult> PartiallyApply<T1, T2, TResult>(Func<T1, T2, TResult> function, T2 argument2)
    {
        return argument1 => function(argument1, argument2);
    }

    static void Main()
    {
        var fs = (Func<IEnumerable<int>, Func<int, int>, IEnumerable<int>>)Enumerable.Select;
        var f1 = (Func<int, int>)(n => n * 2);
        var f2 = (Func<int, int>)(n => n * n);
        var fsf1 = PartiallyApply(fs, f1);
        var fsf2 = PartiallyApply(fs, f2);

        var s = new[] { 0, 1, 2, 3 };
        Console.WriteLine(string.Join(", ", fsf1(s)));
        Console.WriteLine(string.Join(", ", fsf2(s)));

        s = new[] { 2, 4, 6, 8 };
        Console.WriteLine(string.Join(", ", fsf1(s)));
        Console.WriteLine(string.Join(", ", fsf2(s)));
    }
}
```


{{out}}

```txt
0, 2, 4, 6
0, 1, 4, 9
4, 8, 12, 16
4, 16, 36, 64
```



### Second approach


{{trans|Visual Basic .NET|second approach}}

A partial application function that accepts arbitrary function and applied function arity. f1 and f2 also use late binding in this example to work with any argument that can be multiplied.


```csharp
using System;
using System.Collections.Generic;
using System.Linq;

static class PartialApplicationDynamic
{
    // Create a matching delegate type to simplify delegate creation.
    delegate IEnumerable<TResult> fsDelegate<TSource, TResult>(Func<TSource, TResult> f, IEnumerable<TSource> s);

    static IEnumerable<TResult> fs<TSource, TResult>(Func<TSource, TResult> f, IEnumerable<TSource> s) => s.Select(f);

    static dynamic f1(dynamic x) => x * 2;

    static dynamic f2(dynamic x) => x * x;

    static T[] ArrayConcat<T>(T[] arr1, T[] arr2)
    {
        var result = new T[arr1.Length + arr2.Length];
        Array.Copy(arr1, result, arr1.Length);
        Array.Copy(arr2, 0, result, 1, arr2.Length);
        return result;
    }

    // Use a specialized params delegate to simplify calling at the risk of inadvertent params expansion.
    delegate TResult partialDelegate<TParams, TResult>(params TParams[] args);
    static partialDelegate<dynamic, TResult> PartialApplyDynamic<TDelegate, TResult>(TDelegate f, params dynamic[] args) where TDelegate : Delegate
    {
        return rest => (TResult)f.DynamicInvoke(ArrayConcat(args, rest).Cast<dynamic>().ToArray());
    }

    static void Main()
    {
        // Cast to object to avoid params expansion of the arrays.
        object args1 = new object[] { 0, 1, 2, 3 };
        object args2 = new object[] { 2, 4, 6, 8 };

        var fsf1 = PartialApplyDynamic<fsDelegate<dynamic, dynamic>, IEnumerable<dynamic>>(fs, new Func<dynamic, dynamic>(f1));
        var fsf2 = PartialApplyDynamic<fsDelegate<dynamic, dynamic>, IEnumerable<dynamic>>(fs, new Func<dynamic, dynamic>(f2));

        Console.WriteLine("fsf1, 0-3: " + string.Join(", ", fsf1(args1)));
        Console.WriteLine("fsf1, evens: " + string.Join(", ", fsf1(args2)));
        Console.WriteLine("fsf2, 0-3: " + string.Join(", ", fsf2(args1)));
        Console.WriteLine("fsf2, evens: " + string.Join(", ", fsf2(args2)));
    }
}
```


{{out}}

```txt
fsf1, 0-3: 0, 2, 4, 6
fsf1, evens: 4, 8, 12, 16
fsf2, 0-3: 0, 1, 4, 9
fsf2, evens: 4, 16, 36, 64
```



## Ceylon


```ceylon
shared void run() {

	function fs(Integer f(Integer n), {Integer*} s) => s.map(f);

	function f1(Integer n) => n * 2;
	function f2(Integer n) => n ^ 2;

	value fsCurried = curry(fs);
	value fsf1 = fsCurried(f1);
	value fsf2 = fsCurried(f2);

	value ints = 0..3;
	print("fsf1(``ints``) is ``fsf1(ints)`` and fsf2(``ints``) is ``fsf2(ints)``");

	value evens = (2..8).by(2);
	print("fsf1(``evens``) is ``fsf1(evens)`` and fsf2(``evens``) is ``fsf2(evens)``");
}
```



## CoffeeScript


```coffeescript

partial = (f, g) ->
  (s) -> f(g, s)

fs = (f, s) -> (f(a) for a in s)
f1 = (a) -> a * 2
f2 = (a) -> a * a
fsf1 = partial(fs, f1)
fsf2 = partial(fs, f2)

do ->
  for seq in [[0..3], [2,4,6,8]]
    console.log fsf1 seq
    console.log fsf2 seq

```

output
<lang>
> coffee partials.coffee
[ 0, 2, 4, 6 ]
[ 0, 1, 4, 9 ]
[ 4, 8, 12, 16 ]
[ 4, 16, 36, 64 ]

```




## D

fs has a static template argument f and the runtime argument s. The template constraints of fs statically require f to be a callable with just one argument, as requested by the task.

```d
import std.stdio, std.algorithm, std.traits;

auto fs(alias f)(in int[] s) pure nothrow
if (isCallable!f && ParameterTypeTuple!f.length == 1) {
    return s.map!f;
}

int f1(in int x) pure nothrow { return x * 2; }
int f2(in int x) pure nothrow { return x ^^ 2; }

alias fsf1 = fs!f1;
alias fsf2 = fs!f2;

void main() {
    foreach (const d; [[0, 1, 2, 3], [2, 4, 6, 8]]) {
        d.fsf1.writeln;
        d.fsf2.writeln;
    }
}
```

{{out}}

```txt
[0, 2, 4, 6]
[0, 1, 4, 9]
[4, 8, 12, 16]
[4, 16, 36, 64]
```



## E



```e
def pa(f, args1) {
  return def partial {
    match [`run`, args2] {
      E.call(f, "run", args1 + args2)
    }
  }
}

def fs(f, s) {
  var r := []
  for n in s {
    r with= f(n)
  }
  return r
}

def f1(n) { return n * 2 }
def f2(n) { return n ** 2 }

def fsf1 := pa(fs, [f1])
def fsf2 := pa(fs, [f2])
for s in [0..3, [2, 4, 6, 8]] {
  for f in [fsf1, fsf2] {
    println(f(s))
  }
}
```



## Egison



```egison

(define $fs (map $1 $2))

(define $f1 (* $ 2))
(define $f2 (power $ 2))

(define $fsf1 (fs f1 $))
(define $fsf2 (fs f2 $))

(test (fsf1 {0 1 2 3}))
(test (fsf2 {0 1 2 3}))
(test (fsf1 {2 4 6 8}))
(test (fsf2 {2 4 6 8}))

```

'''Output:'''

```egison

{0 2 4 6}
{0 1 4 9}
{4 8 12 16}
{4 16 36 64}

```


## Elena

{{trans|Smalltalk}}
ELENA 4.1 :

```elena
import system'collections;
import system'routines;
import extensions;

public program()
{
    var partial := (afs,af => (s => afs(af, s)));

    var fs := (f,s => s.selectBy:(x => f(x)).summarize(new ArrayList()).toArray());
    var f1 := (x => x * 2);
    var f2 := (x => x * x);

    var fsf1 := partial(fs, f1);
    var fsf2 := partial(fs, f2);

    console.printLine(fsf1(new int[]::(2,4,6,8)).toString());
    console.printLine(fsf2(new int[]::(2,4,6,8)).toString())
}
```

{{out}}

```txt

4,8,12,16
4,16,36,64

```


=={{Header|F_Sharp|F#}}==
Translation of Racket


```fsharp

let fs f s = List.map f s
let f1 n = n * 2
let f2 n = n * n

let fsf1 = fs f1
let fsf2 = fs f2

printfn "%A" (fsf1 [0; 1; 2; 3])
printfn "%A" (fsf1 [2; 4; 6; 8])
printfn "%A" (fsf2 [0; 1; 2; 3])
printfn "%A" (fsf2 [2; 4; 6; 8])

```

Output:

```txt

[0; 2; 4; 6]
[4; 8; 12; 16]
[0; 1; 4; 9]
[4; 16; 36; 64]

```



## Factor

<lang>USING: kernel math prettyprint sequences ;
IN: rosetta-code.partial-function-application

ALIAS: fs map
: f1   ( n -- m  ) 2 * ;
: f2   ( n -- m  ) dup * ;
: fsf1 ( s -- s' ) [ f1 ] fs ;
: fsf2 ( s -- s' ) [ f2 ] fs ;

{ 0 1 2 3 } [ fsf1 . ] [ fsf2 . ] bi
{ 2 4 6 8 } [ fsf1 . ] [ fsf2 . ] bi
```

{{out}}

```txt

{ 0 2 4 6 }
{ 0 1 4 9 }
{ 4 8 12 16 }
{ 4 16 36 64 }

```



## FunL


```funl
fs = map
f1 = (* 2)
f2 = (^ 2)

fsf1 = fs.curry( f1 )
fsf2 = fs.curry( f2 )

println( fsf1(0..3) )
println( fsf2(0..3) )
println( fsf1(2..8 by 2) )
println( fsf2(2..8 by 2) )
```


{{out}}


```txt

[0, 2, 4, 6]
[0, 1, 4, 9]
[4, 8, 12, 16]
[4, 16, 36, 64]

```



## Go

{{works with|Go|1.1}} (The first way shown uses [http://golang.org/ref/spec#Method_values Method values] which were added in Go 1.1. The second uses a function returning a function which was always possible.)
[http://play.golang.org/p/fbmK4qfFZr Run this in the Go playground].

```go
package main

import "fmt"

// Using a method bound to a function type:

// fn is a simple function taking an integer and returning another.
type fn func(int) int

// fs applies fn to each argument returning all results.
func (f fn) fs(s ...int) (r []int) {
	for _, i := range s {
		r = append(r, f(i))
	}
	return r
}

// Two simple functions for demonstration.
func f1(i int) int { return i * 2 }
func f2(i int) int { return i * i }

// Another way:

// addn returns a function that adds n to a sequence of numbers
func addn(n int) func(...int) []int {
	return func(s ...int) []int {
		var r []int
		for _, i := range s {
			r = append(r, n+i)
		}
		return r
	}
}

func main() {
	// Turning a method into a function bound to it's reciever:
	fsf1 := fn(f1).fs
	fsf2 := fn(f2).fs
	// Or using a function that returns a function:
	fsf3 := addn(100)

	s := []int{0, 1, 2, 3}
	fmt.Println("For s =", s)
	fmt.Println("  fsf1:", fsf1(s...))       // Called with a slice
	fmt.Println("  fsf2:", fsf2(0, 1, 2, 3)) // ... or with individual arguments
	fmt.Println("  fsf3:", fsf3(0, 1, 2, 3))
	fmt.Println("  fsf2(fsf1):", fsf2(fsf1(s...)...))

	s = []int{2, 4, 6, 8}
	fmt.Println("For s =", s)
	fmt.Println("  fsf1:", fsf1(2, 4, 6, 8))
	fmt.Println("  fsf2:", fsf2(s...))
	fmt.Println("  fsf3:", fsf3(s...))
	fmt.Println("  fsf3(fsf1):", fsf3(fsf1(s...)...))
}
```

{{out}}

```txt
For s = [0 1 2 3]
  fsf1: [0 2 4 6]
  fsf2: [0 1 4 9]
  fsf3: [100 101 102 103]
  fsf2(fsf1): [0 4 16 36]
For s = [2 4 6 8]
  fsf1: [4 8 12 16]
  fsf2: [4 16 36 64]
  fsf3: [102 104 106 108]
  fsf3(fsf1): [104 108 112 116]
```



## Groovy


```groovy
def fs = { fn, values -> values.collect { fn(it) } }
def f1 = { v -> v * 2 }
def f2 = { v -> v ** 2 }
def fsf1 = fs.curry(f1)
def fsf2 = fs.curry(f2)
```

Testing:

```groovy
[(0..3), (2..8).step(2)].each { seq ->
    println "fsf1$seq = ${fsf1(seq)}"
    println "fsf2$seq = ${fsf2(seq)}"
}
```

Output:

```txt
fsf1[0, 1, 2, 3] = [0, 2, 4, 6]
fsf2[0, 1, 2, 3] = [0, 1, 4, 9]
fsf1[2, 4, 6, 8] = [4, 8, 12, 16]
fsf2[2, 4, 6, 8] = [4, 16, 36, 64]
```



## Haskell

Haskell functions are curried. i.e. All functions actually take exactly one argument. Functions of multiple arguments are simply functions that take the first argument, which returns another function to take the remaining arguments, etc. Therefore, partial function application is trivial. Not giving a multi-argument function all of its arguments will simply return a function that takes the remaining arguments.

```haskell
fs = map
f1 = (* 2)
f2 = (^ 2)

fsf1 = fs f1
fsf2 = fs f2

main :: IO ()
main = do
  print $ fsf1 [0, 1, 2, 3] -- prints [0, 2, 4, 6]
  print $ fsf2 [0, 1, 2, 3] -- prints [0, 1, 4, 9]
  print $ fsf1 [2, 4, 6, 8] -- prints [4, 8, 12, 16]
  print $ fsf2 [2, 4, 6, 8] -- prints [4, 16, 36, 64]
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link printf

procedure main()
   fsf1 := partial(fs,f1)
   fsf2 := partial(fs,f2)
   every s :=  [ 0, 1, 2, 3 ] |
               [ 2, 4, 6, 8 ] do {
         printf("\ns       := %s\n",list2string(s))
         printf("fsf1(s) := %s\n",list2string(fsf1(s)))
         printf("fsf2(s) := %s\n",list2string(fsf2(s)))
      }
end

procedure partial(f,g)  #: partial application of f & g
   @( p := create repeat {
                  s := (r@&source)[1]  # return r / get argument s
                  r := f(g,s)          # apply f(g,...)
                  }
      )                                # create and activate procedure p
   return p
end

procedure fs(f,s)       #: return list where f is applied to each element of s
   every put(r := [], f(!s))
   return r
end

procedure f1(n)         # double
   return n * 2
end

procedure f2(n)         #: square
   return n ^ 2
end

procedure list2string(L)         #: format list as a string
   every (s := "[ ") ||:= !L || " "
   return s || "]"
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]

Output:
```txt
s       := [ 0 1 2 3 ]
fsf1(s) := [ 0 2 4 6 ]
fsf2(s) := [ 0 1 4 9 ]

s       := [ 2 4 6 8 ]
fsf1(s) := [ 4 8 12 16 ]
fsf2(s) := [ 4 16 36 64 ]
```



## J


Given:


```j
fs=:1 :'u"0 y'
f1=:*&2
f2=:^&2
fsf1=:f1 fs
fsf2=:f2 fs
```


The required examples might look like this:


```j
   fsf1 i.4
0 2 4 6
   fsf2 i.4
0 1 4 9
   fsf1 fsf1 1+i.4
4 8 12 16
   fsf2 fsf1 1+i.4
4 16 36 64
```


That said, note that much of this is unnecessary, since f1 and f2 already work the same way on list arguments.


```j
   f1 i.4
0 2 4 6
   f2 i.4
0 1 4 9
   f1 1+i.4
2 4 6 8
   f2 f1 1+i.4
4 16 36 64
```


That said, note that if we complicated the definitions of f1 and f2, so that they would not work on lists, the fs approach would still work:

In other words, given:


```j
crippled=:1 :0
  assert.1=#y
  u y
)

F1=: f1 crippled
F2=: f2 crippled
fsF1=: F1 fs
fsF2=: F2 fs
```


the system behaves like this:


```j
   F1 i.4
|assertion failure: F1
|   1=#y
   fsF1 i.4
0 2 4 6
NB. and so on...
```



## Java

To solve this task, I wrote <tt>fs()</tt> as a curried method. I changed the syntax from <tt>fs(arg1, arg2)</tt> to <tt>fs(arg1).call(arg2)</tt>. Now I can use <tt>fs(arg1)</tt> as partial application.


```java
import java.util.Arrays;

public class PartialApplication {
	interface IntegerFunction {
		int call(int arg);
	}

	// Original method fs(f, s).
	static int[] fs(IntegerFunction f, int[] s) {
		int[] r = new int[s.length];
		for (int i = 0; i < s.length; i++)
			r[i] = f.call(s[i]);
		return r;
	}

	interface SequenceFunction {
		int[] call(int[] arg);
	}

	// Curried method fs(f).call(s),
	// necessary for partial application.
	static SequenceFunction fs(final IntegerFunction f) {
		return new SequenceFunction() {
			public int[] call(int[] s) {
				// Call original method.
				return fs(f, s);
			}
		};
	}

	static IntegerFunction f1 = new IntegerFunction() {
		public int call(int i) {
			return i * 2;
		}
	};

	static IntegerFunction f2 = new IntegerFunction() {
		public int call(int i) {
			return i * i;
		}
	};

	static SequenceFunction fsf1 = fs(f1); // Partial application.

	static SequenceFunction fsf2 = fs(f2);

	public static void main(String[] args) {
		int[][] sequences = {
			{ 0, 1, 2, 3 },
			{ 2, 4, 6, 8 },
		};

		for (int[] array : sequences) {
			System.out.printf(
			    "array: %s\n" +
			    "  fsf1(array): %s\n" +
			    "  fsf2(array): %s\n",
			    Arrays.toString(array),
			    Arrays.toString(fsf1.call(array)),
			    Arrays.toString(fsf2.call(array)));
		}
	}
}
```


The aforementioned code, lambda-ized in Java 8.


```java5
import java.util.Arrays;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.IntUnaryOperator;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

@FunctionalInterface
public interface PartialApplication<INPUT1, INPUT2, OUTPUT> extends BiFunction<INPUT1, INPUT2, OUTPUT> {
  // Original method fs(f, s).
  public static int[] fs(IntUnaryOperator f, int[] s) {
    return Arrays.stream(s)
      .parallel()
      .map(f::applyAsInt)
      .toArray()
    ;
  }

  // Currying method f.apply(a).apply(b),
  // in lieu of f.apply(a, b),
  // necessary for partial application.
  public default Function<INPUT2, OUTPUT> apply(INPUT1 input1) {
    return input2 -> apply(input1, input2);
  }

  // Original method fs turned into a partially-applicable function.
  public static final PartialApplication<IntUnaryOperator, int[], int[]> fs = PartialApplication::fs;

  public static final IntUnaryOperator f1 = i -> i + i;

  public static final IntUnaryOperator f2 = i -> i * i;

  public static final UnaryOperator<int[]> fsf1 = fs.apply(f1)::apply; // Partial application.

  public static final UnaryOperator<int[]> fsf2 = fs.apply(f2)::apply;

  public static void main(String... args) {
    int[][] sequences = {
      {0, 1, 2, 3},
      {2, 4, 6, 8},
    };

    Arrays.stream(sequences)
      .parallel()
      .map(array ->
        Stream.of(
          array,
          fsf1.apply(array),
          fsf2.apply(array)
        )
          .parallel()
          .map(Arrays::toString)
          .toArray()
      )
      .map(array ->
        String.format(
          String.join("\n",
            "array: %s",
            "  fsf1(array): %s",
            "  fsf2(array): %s"
          ),
          array
        )
      )
      .forEachOrdered(System.out::println)
    ;
  }
}
```


Compilation and output for both versions:
```txt
$ javac PartialApplication.java
$ java PartialApplication
array: [0, 1, 2, 3]
  fsf1(array): [0, 2, 4, 6]
  fsf2(array): [0, 1, 4, 9]
array: [2, 4, 6, 8]
  fsf1(array): [4, 8, 12, 16]
  fsf2(array): [4, 16, 36, 64]
```




## JavaScript


### ES5

Higher order functions are part of the core architecture of JavaScript.

(No special libraries are required for the creation or application of partial functions)


```JavaScript
var f1 = function (x) { return x * 2; },
    f2 = function (x) { return x * x; },

    fs = function (f, s) {
        return function (s) {
            return s.map(f);
        }
    },

    fsf1 = fs(f1),
    fsf2 = fs(f2);

// Test
    [
        fsf1([0, 1, 2, 3]),
        fsf2([0, 1, 2, 3]),

        fsf1([2, 4, 6, 8]),
        fsf2([2, 4, 6, 8])
    ]
```


Output:


```txt
[[0, 2, 4, 6], [0, 1, 4, 9], [4, 8, 12, 16], [4, 16, 36, 64]]
```


For additional flexibility ( allowing for an arbitrary number of arguments in applications of a partially applied function, and dropping the square brackets from the function calls in the tests above ) we can make use of the array-like ''arguments'' object, which is a property of any JavaScript function.


```JavaScript
var f1 = function (x) { return x * 2; },
    f2 = function (x) { return x * x; },

    fs = function (f) {
        return function () {
            return Array.prototype.slice.call(
                arguments
            ).map(f);
        }
    },

    fsf1 = fs(f1),
    fsf2 = fs(f2);

// Test alternative approach, with arbitrary numbers of arguments
    [
        fsf1(0, 1, 2, 3, 4),
        fsf2(0, 1, 2),
        fsf1(2, 4, 6, 8, 10, 12),
        fsf2(2, 4, 6, 8)
    ]
```


Output:


```txt
[[0, 2, 4, 6, 8], [0, 1, 4], [4, 8, 12, 16, 20, 24], [4, 16, 36, 64]]
```



### ES6


### =Simple curry=


```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS ------------------------------------------------------

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // map :: (a -> b) -> [a] -> [b]
    const map = curry((f, xs) => xs.map(f));


    // PARTIAL APPLICATION ----------------------------------------------------

    const
        f1 = x => x * 2,
        f2 = x => x * x,

        fs = map,

        fsf1 = fs(f1),
        fsf2 = fs(f2);

    // TEST -------------------------------------------------------------------
    return [
        fsf1([0, 1, 2, 3]),
        fsf2([0, 1, 2, 3]),

        fsf1([2, 4, 6, 8]),
        fsf2([2, 4, 6, 8])
    ];
})();
```

{{Out}}

```txt
[[0, 2, 4, 6], [0, 1, 4, 9], [4, 8, 12, 16], [4, 16, 36, 64]]
```


### =Generic curry=

The simple version of the higher-order '''curry''' function above works only on functions with two arguments. For more flexibility, we can generalise it to a form which curries functions with an arbitrary number of arguments:


```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS ------------------------------------------------------

    // 2 or more arguments
    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat(Array.from(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = curry((f, xs) => xs.map(f));

    // PARTIAL APPLICATION ----------------------------------------------------
    const
        f1 = x => x * 2,
        f2 = x => x * x,

        fs = map,

        fsf1 = fs(f1),
        fsf2 = fs(f2);

    // TEST -------------------------------------------------------------------
    return [
        fsf1([0, 1, 2, 3]),
        fsf2([0, 1, 2, 3]),

        fsf1([2, 4, 6, 8]),
        fsf2([2, 4, 6, 8])
    ];
})();
```

{{Out}}

```txt
[[0, 2, 4, 6], [0, 1, 4, 9], [4, 8, 12, 16], [4, 16, 36, 64]]
```



## Julia


```julia

fs(f, s) = map(f, s)
f1(x) = 2x
f2(x) = x^2
fsf1(s) = fs(f1, s)
fsf2(s) = fs(f2, s)

s1 = [0, 1, 2 ,3]
s2 = [2, 4, 6, 8]
println("fsf1 of s1 is $(fsf1(s1))")
println("fsf2 of s1 is $(fsf2(s1))")
println("fsf1 of s2 is $(fsf1(s2))")
println("fsf2 of s2 is $(fsf2(s2))")

```

{{output}}
```txt

fsf1 of s1 is [0, 2, 4, 6]
fsf2 of s1 is [0, 1, 4, 9]
fsf1 of s2 is [4, 8, 12, 16]
fsf2 of s2 is [4, 16, 36, 64]

```



## Kotlin


```scala
// version 1.1.2

typealias Func  = (Int) -> Int
typealias FuncS = (Func, List<Int>) -> List<Int>

fun fs(f: Func, seq: List<Int>) = seq.map { f(it) }

fun partial(fs: FuncS, f: Func) = { seq: List<Int> -> fs(f, seq) }

fun f1(n: Int) = 2 * n

fun f2(n: Int) = n * n

fun main(args: Array<String>) {
    val fsf1 = partial(::fs, ::f1)
    val fsf2 = partial(::fs, ::f2)
    val seqs = listOf(
        listOf(0, 1, 2, 3),
        listOf(2, 4, 6, 8)
    )
    for (seq in seqs) {
        println(fs(::f1, seq))      // normal
        println(fsf1(seq))          // partial
        println(fs(::f2, seq))      // normal
        println(fsf2(seq))          // partial
        println()
    }
}
```


{{out}}

```txt

[0, 2, 4, 6]
[0, 2, 4, 6]
[0, 1, 4, 9]
[0, 1, 4, 9]

[4, 8, 12, 16]
[4, 8, 12, 16]
[4, 16, 36, 64]
[4, 16, 36, 64]

```



## Lambdatalk


{lambda talk} doesn't know closures but accepts de facto partial application. Not giving a multi-argument function all of its arguments will simply return a function that takes the remaining arguments.

```Scheme

1) just define function as usual:
  {def add  {lambda {:a :b :c} {+ :a :b :c}}} -> add

2) and use it:
  {add 1 2 3}     -> 6
  {{add 1} 2 3}   -> 6
  {{add 1 2} 3}   -> 6
  {{{add 1} 2} 3} -> 6

3) application:
{def fs {lambda {:f} map :f}}
{def f1 {lambda {:x} {* :x 2}}}
{def f2 {lambda {:x} {pow :x 2}}}
{def fsf1 {fs f1}}
{def fsf2 {fs f2}}

{{fsf1} 0 1 2 3}
{{fsf2} 0 1 2 3}
{{fsf1} 2 4 6 8}
{{fsf2} 2 4 6 8}

Output:
0 2 4 6
0 1 4 9
4 8 12 16
4 16 36 64

```



## LFE


There is no partial in Erlang, so in LFE we use a closure.

Here is the code, made more general to account for different arrities (note that to copy and paste into the LFE REPL, you'll need to leave out the docstring):

```lisp

(defun partial
  "The partial function is arity 2 where the first parameter must be a
  function and the second parameter may either be a single item or a list of
  items.

  When funcall is called against the result of the partial call, a second
  parameter is applied to the partial function. This parameter too may be
  either a single item or a list of items."
  ((func args-1) (when (is_list args-1))
    (match-lambda
      ((args-2) (when (is_list args-2))
        (apply func (++ args-1 args-2)))
      ((arg-2)
        (apply func (++ args-1 `(,arg-2))))))
  ((func arg-1)
    (match-lambda
      ((args-2) (when (is_list args-2))
        (apply func (++ `(,arg-1) args-2)))
      ((arg-2)
        (funcall func arg-1 arg-2)))))

```


Here is the problem set:

```lisp

(defun fs (f s) (lists:map f s))
(defun f1 (i) (* i 2))
(defun f2 (i) (math:pow i 2))

(set fsf1 (partial #'fs/2 #'f1/1))
(set fsf2 (partial #'fs/2 #'f2/1))
(set seq1 '((0 1 2 3)))
(set seq2 '((2 4 6 8)))

> (funcall fsf1 seq1)
(0 2 4 6)
> (funcall fsf2 seq1)
(0.0 1.0 4.0 9.0)
> (funcall fsf1 seq2)
(4 8 12 16)
> (funcall fsf2 seq2)
(4.0 16.0 36.0 64.0)


```



## Logtalk

Using Logtalk's built-in and library meta-predicates:

```logtalk

:- object(partial_functions).

    :- public(show/0).

    show :-
        % create the partial functions
        create_partial_function(f1, PF1),
        create_partial_function(f2, PF2),
        % apply the partial functions
        Sequence1 = [0,1,2,3],
        call(PF1, Sequence1, PF1Sequence1), output_results(PF1, Sequence1, PF1Sequence1),
        call(PF2, Sequence1, PF2Sequence1), output_results(PF2, Sequence1, PF2Sequence1),
        Sequence2 = [2,4,6,8],
        call(PF1, Sequence2, PF1Sequence2), output_results(PF1, Sequence2, PF1Sequence2),
        call(PF2, Sequence2, PF2Sequence2), output_results(PF2, Sequence2, PF2Sequence2).

    create_partial_function(Closure, fs(Closure)).

    output_results(Function, Input, Output) :-
        write(Input), write(' -> '), write(Function), write(' -> '), write(Output), nl.

    fs(Closure, Arg1, Arg2) :-
        meta::map(Closure, Arg1, Arg2).

    f1(Value, Double) :-
        Double is 2*Value.

    f2(Value, Square) :-
        Square is Value*Value.

:- end_object.

```

Output:

```text

| ?- partial_functions::show.
[0,1,2,3] -> fs(f1) -> [0,2,4,6]
[0,1,2,3] -> fs(f2) -> [0,1,4,9]
[2,4,6,8] -> fs(f1) -> [4,8,12,16]
[2,4,6,8] -> fs(f2) -> [4,16,36,64]
yes

```



## Lua



```lua
function map(f, ...)
    local t = {}
    for k, v in ipairs(...) do
        t[#t+1] = f(v)
    end
    return t
end

function timestwo(n)
    return n * 2
end

function squared(n)
    return n ^ 2
end

function partial(f, arg)
    return function(...)
        return f(arg, ...)
    end
end

timestwo_s = partial(map, timestwo)
squared_s = partial(map, squared)

print(table.concat(timestwo_s{0, 1, 2, 3}, ', '))
print(table.concat(squared_s{0, 1, 2, 3}, ', '))
print(table.concat(timestwo_s{2, 4, 6, 8}, ', '))
print(table.concat(squared_s{2, 4, 6, 8}, ', '))
```


'''Output:'''

    0, 2, 4, 6
    0, 1, 4, 9
    4, 8, 12, 16
    4, 16, 36, 64


## Mathematica


```Mathematica
fs[f_, s_] := Map[f, s]
f1 [n_] := n*2
f2 [n_] := n^2
fsf1[s_] := fs[f1, s]
fsf2[s_] := fs[f2, s]
```

Example usage:

```txt
fsf1[{0, 1, 2, 3}]
->{0, 2, 4, 6}
fsf2[{0, 1, 2, 3}]
->{0, 1, 4, 9}
fsf1[{2, 4, 6, 8}]
->{4, 8, 12, 16}
fsf2[{2, 4, 6, 8}]
->{4, 16, 36, 64}
```



## Mercury


```mercury
:- module partial_function_application.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

main(!IO) :-
    io.write((fsf1)([0, 1, 2, 3]), !IO), io.nl(!IO),
    io.write((fsf2)([0, 1, 2, 3]), !IO), io.nl(!IO),
    io.write((fsf1)([2, 4, 6, 8]), !IO), io.nl(!IO),
    io.write((fsf2)([2, 4, 6, 8]), !IO), io.nl(!IO).

:- func fs(func(V) = V, list(V)) = list(V).

fs(_, []) = [].
fs(F, [V | Vs]) = [F(V) | fs(F, Vs)].

:- func f1(int) = int.

f1(V) = V * 2.

:- func f2(int) = int.

f2(V) = V * V.

:- func fsf1 = (func(list(int)) = list(int)).

fsf1 = fs(f1).

:- func fsf2 = (func(list(int)) = list(int)).

fsf2 = fs(f2).
```



## min

{{works with|min|0.19.3}}

```min
'map :fs
(dup +) :f1
(dup *) :f2
('f1 fs) :fsf1
('f2 fs) :fsf2

(0 1 2 3) fsf1 puts!
(0 1 2 3) fsf2 puts!
(2 4 6 8) fsf1 puts!
(2 4 6 8) fsf2 puts!
```

{{out}}

```txt

(0 2 4 6)
(0 1 4 9)
(4 8 12 16)
(4 16 36 64)

```



## Nemerle


```Nemerle
using System;
using System.Console;

module Partial
{
    fs[T] (f : T -> T, s : list[T]) : list[T]
    {
        $[f(x)| x in s]
    }

    f1 (x : int) : int
    {
        x * 2
    }

    f2 (x : int) : int
    {
        x * x
    }

    curry[T, U, V] (f : T * U -> V, x : T) : U -> V
    {
        f(x, _)
    }

    // curryr() isn't actually used in this task, I just include it for symmetry
    curryr[T, U, V] (f : T * U -> V, x : U) : T -> V
    {
        f(_, x)
    }

    Main() : void
    {
        def fsf1 = curry(fs, f1);
        def fsf2 = curry(fs, f2);
        def test1 = $[0 .. 3];
        def test2 = $[x | x in [2 .. 8], x % 2 == 0];

        WriteLine (fsf1(test1));
        WriteLine (fsf1(test2));
        WriteLine (fsf2(test1));
        WriteLine (fsf2(test2));

    }
}
```



## OCaml

OCaml functions are curried. i.e. All functions actually take exactly one argument. Functions of multiple arguments are simply functions that take the first argument, which returns another function to take the remaining arguments, etc. Therefore, partial function application is trivial. Not giving a multi-argument function all of its arguments will simply return a function that takes the remaining arguments.

```ocaml
#
let fs f s = List.map f s
let f1 value = value * 2
let f2 value = value * value

let fsf1 = fs f1
let fsf2 = fs f2
;;
val fs : ('a -> 'b) -> 'a list -> 'b list = <fun>
val f1 : int -> int = <fun>
val f2 : int -> int = <fun>
val fsf1 : int list -> int list = <fun>
val fsf2 : int list -> int list = <fun>

# fsf1 [0; 1; 2; 3];;
- : int list = [0; 2; 4; 6]
# fsf2 [0; 1; 2; 3];;
- : int list = [0; 1; 4; 9]
# fsf1 [2; 4; 6; 8];;
- : int list = [4; 8; 12; 16]
# fsf2 [2; 4; 6; 8];;
- : int list = [4; 16; 36; 64]
```



## Oforth



```Oforth
: fs(s, f)     f s map ;
: f1           2 * ;
: f2           sq  ;

#f1 #fs curry => fsf1
#f2 #fs curry => fsf2
```


{{out}}

```txt

>[ 0, 1, 2, 3 ] fsf1 .
[0, 2, 4, 6] ok
>[ 0, 1, 2, 3 ] fsf2 .
[0, 1, 4, 9] ok
>[ 2, 4, 6, 8 ] fsf1 .
[4, 8, 12, 16] ok
>[ 2, 4, 6, 8 ] fsf2 .
[4, 16, 36, 64] ok

```



## Order

Much like Haskell and ML, not giving a multi-argument function all of its arguments returns a function that will accept the rest.

```c
#include <order/interpreter.h>
```


#define ORDER_PP_DEF_8fs ORDER_PP_FN( 8fn(8F, 8S, 8seq_map(8F, 8S)) )

#define ORDER_PP_DEF_8f1 ORDER_PP_FN( 8fn(8V, 8times(8V, 2)) )

#define ORDER_PP_DEF_8f2 ORDER_PP_FN( 8fn(8V, 8times(8V, 8V)) )

ORDER_PP(
  8let((8F, 8fs(8f1))
       (8G, 8fs(8f2)),
       8do(
         8print(8ap(8F, 8seq(0, 1, 2, 3)) 8comma 8space),
         8print(8ap(8G, 8seq(0, 1, 2, 3)) 8comma 8space),
         8print(8ap(8F, 8seq(2, 4, 6, 8)) 8comma 8space),
         8print(8ap(8G, 8seq(2, 4, 6, 8))))) )
```

{{out}}

```txt
(0)(2)(4)(6), (0)(1)(4)(9), (4)(8)(12)(16), (4)(16)(36)(64)
```

This example highlights two related syntactic limitations: only a statically-defined function (using <code>#define ORDER_PP_DEF_</code> etc.) can have a multi-character name, so variables - i.e. the result of expressions - are limited to <code>8A</code>-<code>8Z</code>; and similarly only statically-defined functions can be applied using the C-like <code>8name(args)</code> syntax: variables or expression results must be applied using the <code>8ap</code> operator (which is semantically identical, but not quite as pretty).


## PARI/GP

This pure-GP solution cheats slightly, since GP lacks variadic arguments and reflection.

```parigp
fs=apply;
f1(x)=2*x;
f2(x)=x^2;
fsf1=any->=fs(f1,any);
fsf2=any->=fs(f2,any);
fsf1([0..3])
fsf1(2([1..4])
fsf2([0..3])
fsf2(2([1..4])
```


PARI can do true partial function application, along the lines of [[#C|C]]; see also the <code>E*</code> parser code.


## Perl

Note: this is written according to my understanding of the task spec and the discussion page; it doesn't seem a consensus was reached regarding what counts as a "partial" yet.

```Perl
sub fs(&) {
        my $func = shift;
        sub { map $func->($_), @_ }
}

sub double($) { shift() * 2 }
sub square($) { shift() ** 2 }

my $fs_double = fs(\&double);
my $fs_square = fs(\&square);

my @s = 0 .. 3;
print "fs_double(@s): @{[ $fs_double->(@s) ]}\n";
print "fs_square(@s): @{[ $fs_square->(@s) ]}\n";

@s = (2, 4, 6, 8);
print "fs_double(@s): @{[ $fs_double->(@s) ]}\n";
print "fs_square(@s): @{[ $fs_square->(@s) ]}\n";
```

Output:
```txt
fs_double(0 1 2 3): 0 2 4 6
fs_square(0 1 2 3): 0 1 4 9
fs_double(2 4 6 8): 4 8 12 16
fs_square(2 4 6 8): 4 16 36 64
```



## Perl 6

{{works with|rakudo|2015-09-25}}
All Code objects have the .assuming method, which partially applies its arguments.  For both type safety reasons and parsing sanity reasons we do not believe in implicit partial application by leaving out arguments.  Also, people can understand "assuming" without being steeped in FP culture.

```perl6
sub fs ( Code $f, @s ) { @s.map: { .$f } }

sub f1 ( $n ) { $n *  2 }
sub f2 ( $n ) { $n ** 2 }

my &fsf1 := &fs.assuming(&f1);
my &fsf2 := &fs.assuming(&f2);

for [1..3], [2, 4 ... 8] X &fsf1, &fsf2 -> ($s, $f) {
    say $f.($s);
}
```


Output:
```txt
(2 4 6)
(1 4 9)
(4 8 12 16)
(4 16 36 64)
```

The <tt>*+2</tt> is also a form of partial application in Perl 6.  In this case we partially apply the <tt>infix:<+></tt> function with a second argument of 2.  That is, the star (known as the "whatever" star) indicates which argument <em>not</em> to apply.  In contrast to languages that keep some arguments unbound by leaving holes, the explicit star in Perl 6 allows us to avoid syntactic ambiguity in whether to expect a term or an infix operator; such self-clocking code contributes to better error messages when things go wrong.


## Phix

Phix does not explicitly support this, but you can easily emulate it with routine_id


```Phix
function fs(integer rid, sequence s)
    for i=1 to length(s) do
        s[i] = call_func(rid,{s[i]})
    end for
    return s
end function

function p_apply(sequence f, sequence args)
    return call_func(f[1],{f[2],args})
end function

function f1(integer i)
    return i+i
end function

function f2(integer i)
    return i*i
end function

constant fsf1 = {routine_id("fs"),routine_id("f1")},
         fsf2 = {routine_id("fs"),routine_id("f2")}

?p_apply(fsf1,{0,1,2,3})
?p_apply(fsf2,{2,4,6,8})
```

{{out}}

```txt

{0,2,4,6}
{4,16,36,64}

```

Should you want the first few arguments set as part of fsf1/2 [ie as a 3rd sequence element], then obviously p_apply might be more like

```Phix
function p_apply(sequence ffsa, sequence extra_args)
    object {fa,fx,set_args} = ffsa
    return call_func(fa,{fx,set_args&extra_args})
end function
```



## PicoLisp


```PicoLisp
(def 'fs mapcar)
(de f1 (N) (* 2 N))
(de f2 (N) (* N N))

(de partial (F1 F2)
   (curry (F1 F2) @
      (pass F1 F2) ) )

(def 'fsf1 (partial fs f1))
(def 'fsf2 (partial fs f2))

(for S '((0 1 2 3) (2 4 6 8))
   (println (fsf1 S))
   (println (fsf2 S)) )
```

Output:

```txt
(0 2 4 6)
(0 1 4 9)
(4 8 12 16)
(4 16 36 64)
```



## Prolog

Works with SWI-Prolog.

```Prolog
fs(P, S, S1) :-
	maplist(P, S, S1).

f1(X, Y) :-
	Y is 2 * X.

f2(X, Y) :-
	Y is X * X.

create_partial(P, fs(P)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fs :-
	% partial functions
	create_partial(f1, FSF1),
	create_partial(f2, FSF2),

	S1 = [0,1,2,3],
	call(FSF1,S1, S11), format('~w : ~w ==> ~w~n',[FSF1, S1, S11]),
	call(FSF1,S1, S12), format('~w : ~w ==> ~w~n',[FSF2, S1, S12]),

	S2 = [2,4,6,8],
	call(FSF1,S2, S21), format('~w : ~w ==> ~w~n',[FSF2, S2, S21]),
	call(FSF2,S2, S22), format('~w : ~w ==> ~w~n',[FSF1, S2, S22]).

```

Output :

```txt
?- fs.
fs(f1) : [0,1,2,3] ==> [0,2,4,6]
fs(f2) : [0,1,2,3] ==> [0,1,4,9]
fs(f1) : [2,4,6,8] ==> [4,8,12,16]
fs(f2) : [2,4,6,8] ==> [4,16,36,64]
true.
```



## Python


```python
from functools import partial

def fs(f, s): return [f(value) for value in s]

def f1(value): return value * 2

def f2(value): return value ** 2

fsf1 = partial(fs, f1)
fsf2 = partial(fs, f2)

s = [0, 1, 2, 3]
assert fs(f1, s) == fsf1(s) # ==  [0, 2, 4, 6]
assert fs(f2, s) == fsf2(s) # ==  [0, 1, 4, 9]

s = [2, 4, 6, 8]
assert fs(f1, s) == fsf1(s) # ==  [4, 8, 12, 16]
assert fs(f2, s) == fsf2(s) # ==  [4, 16, 36, 64]
```


The program runs without triggering the assertions.

Explicitly spelling out the partial function without hiding behind a library:
```Python
def partial(f, g):
	def fg(*x): return f(g, *x)
	return fg

def fs(f, *x): return [ f(a) for a in x]
def f1(a): return a * 2
def f2(a): return a * a

fsf1 = partial(fs, f1)
fsf2 = partial(fs, f2)

print fsf1(1, 2, 3, 4)
print fsf2(1, 2, 3, 4)
```



## R



```R
partially.apply <- function(f, ...) {
  capture <- list(...)
  function(...) {
    do.call(f, c(capture, list(...)))
  }
}

fs <- function(f, ...) sapply(list(...), f)
f1 <- function(x) 2*x
f2 <- function(x) x^2

fsf1 <- partially.apply(fs, f1)
fsf2 <- partially.apply(fs, f2)

fsf1(0:3)
fsf2(0:3)
fsf1(seq(2,8,2))
fsf2(seq(2,8,2))
```



## Racket



```racket

#lang racket

(define (fs f s) (map f s))
(define (f1 n) (* n 2))
(define (f2 n) (* n n))

(define fsf1 (curry fs f1))
(define fsf2 (curry fs f2))

(fsf1 '(0 1 2 3))
(fsf1 '(2 4 6 8))
(fsf2 '(0 1 2 3))
(fsf2 '(2 4 6 8))

```



## REXX


```rexx
/*REXX program demonstrates a method of a  partial function application.      */
s=;      do a=0  to 3                  /*build 1st series of some low integers*/
         s=strip(s a)                  /*append to the integer to the  S  list*/
         end   /*a*/

call fs 'f1',s;         say 'for f1:  series=' s",   result="  result
call fs 'f2',s;         say 'for f2:  series=' s",   result="  result

s=;      do b=2  to  8  by 2           /*build 2nd series, low even integers. */
         s=strip(s b)                  /*append to the integer to the  S  list*/
         end   /*b*/

call fs 'f1',s;         say 'for f1:  series=' s",   result="  result
call fs 'f2',s;         say 'for f2:  series=' s",   result="  result
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
f1:  return arg(1)* 2
f2:  return arg(1)**2
/*────────────────────────────────────────────────────────────────────────────*/
fs:  procedure;   arg f,s;   $=;       do j=1  for words(s);   z=word(s,j)
                                       interpret '$=$'     f"("z')'
                                       end  /*j*/
     return strip($)
```

'''output'''

```txt

for f1, series= 0 1 2 3,   result= 0 2 4 6
for f2, series= 0 1 2 3,   result= 0 1 4 9
for f1, series= 2 4 6 8,   result= 4 8 12 16
for f2, series= 2 4 6 8,   result= 4 16 36 64

```



## Ruby

<tt>Proc#curry</tt> is a new method from Ruby 1.9. A [[currying|curried]] proc applies its arguments to the first parameters of the original proc. In this example, <code>fs.curry[f1][e]</code> is a call to <code>fs[f1, e]</code>, so <code>fs.curry[f1]</code> is a partial application.

{{works with|Ruby|1.9}}

```ruby
fs = proc { |f, s| s.map &f }
f1 = proc { |n| n * 2 }
f2 = proc { |n| n ** 2 }
fsf1 = fs.curry[f1]
fsf2 = fs.curry[f2]

[0..3, (2..8).step(2)].each do |e|
  p fsf1[e]
  p fsf2[e]
end
```


Output

```txt
[0, 2, 4, 6]
[0, 1, 4, 9]
[4, 8, 12, 16]
[4, 16, 36, 64]
```



## Scala


```Scala
def fs[X](f:X=>X)(s:Seq[X]) = s map f
def f1(x:Int) = x * 2
def f2(x:Int) = x * x

def fsf[X](f:X=>X) = fs(f) _
val fsf1 = fsf(f1) // or without the fsf intermediary: val fsf1 = fs(f1) _
val fsf2 = fsf(f2) // or without the fsf intermediary: val fsf2 = fs(f2) _

assert(fsf1(List(0,1,2,3)) == List(0,2,4,6))
assert(fsf2(List(0,1,2,3)) == List(0,1,4,9))
```



## Sidef

{{trans|Perl}}

```ruby
func fs(f) {
    func(*args) {
        args.map {f(_)}
    }
}

func double(n) { n  * 2 };
func square(n) { n ** 2 };

var fs_double = fs(double);
var fs_square = fs(square);

var s = (0 .. 3);
say "fs_double(#{s}): #{fs_double(s...)}";
say "fs_square(#{s}): #{fs_square(s...)}";

s = [2, 4, 6, 8];
say "fs_double(#{s}): #{fs_double(s...)}";
say "fs_square(#{s}): #{fs_square(s...)}";
```

{{out}}

```txt

fs_double(0 1 2 3): 0 2 4 6
fs_square(0 1 2 3): 0 1 4 9
fs_double(2 4 6 8): 4 8 12 16
fs_square(2 4 6 8): 4 16 36 64

```



## Smalltalk

{{works with|Pharo|1.3-13315}}

```smalltalk

| f1 f2 fs fsf1 fsf2 partial |

partial := [ :afs :af | [ :s | afs value: af value: s ] ].

fs := [ :f :s | s collect: [ :x | f value: x ]].
f1 := [ :x | x * 2 ].
f2:= [ :x | x * x ].
fsf1 := partial value: fs value: f1.
fsf2 := partial value: fs value: f2.

fsf1 value: (0 to: 3).
" #(0 2 4 6)"
fsf2 value: (0 to: 3).
" #(0 1 4 9)"

fsf1 value: #(2 4 6 8).
" #(4 8 12 16)"
fsf2 value: #(2 4 6 8).
" #(4 16 36 64)"

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6
proc partial {f1 f2} {
    variable ctr
    coroutine __curry[incr ctr] apply {{f1 f2} {
	for {set x [info coroutine]} 1 {} {
	    set x [{*}$f1 $f2 [yield $x]]
	}
    }} $f1 $f2
}
```

Demonstration:

```tcl
proc fs {f s} {
    set r {}
    foreach n $s {
	lappend r [{*}$f $n]
    }
    return $r
}
proc f1 x {expr {$x * 2}}
proc f2 x {expr {$x ** 2}}
set fsf1 [partial fs f1]
set fsf2 [partial fs f2]
foreach s {{0 1 2 3} {2 4 6 8}} {
    puts "$s ==f1==> [$fsf1 $s]"
    puts "$s ==f2==> [$fsf2 $s]"
}
```

Output:

```txt

0 1 2 3 ==f1==> 0 2 4 6
0 1 2 3 ==f2==> 0 1 4 9
2 4 6 8 ==f1==> 4 8 12 16
2 4 6 8 ==f2==> 4 16 36 64

```



## TXR


Partial application is built in via the <code>op</code> operator, so there is no need to create all these named functions, which defeats the purpose and beauty of partial application: which is to partially apply arguments to functions in an anonymous, implicit way, possibly in multiple places in a single expression.

Indeed, functional language purists would probably say that even the explicit <code>op</code> operator spoils it, somewhat.


```sh
$ txr -p "(mapcar (op mapcar (op * 2)) (list (range 0 3) (range 2 8 2)))"
((0 2 4 6) (4 8 12 16))

$ txr -p "(mapcar (op mapcar (op * @1 @1)) (list (range 0 3) (range 2 8 2)))"
((0 1 4 9) (4 16 36 64))
```


Note how in the above, '''no''' function arguments are explicitly mentioned at all except the necessary reference <code>@1</code> to an argument whose existence is implicit.

Now, without further ado, we surrender the concept of partial application to meet the task requirements:


```sh
$ txr -e "(progn
  (defun fs (fun seq) (mapcar fun seq))
  (defun f1 (num) (* 2 num))
  (defun f2 (num) (* num num))
  (defvar fsf1 (op fs f1))  ;; pointless: can just be (defun fsf1 (seq) (fs f1 seq)) !!!
  (defvar fsf2 (op fs f2))

  (print [fs fsf1 '((0 1 2 3) (2 4 6 8))]) (put-line \"\")
  (print [fs fsf2 '((0 1 2 3) (2 4 6 8))]) (put-line \"\"))"
((0 2 4 6) (4 8 12 16))
((0 1 4 9) (4 16 36 64))
```



## Visual Basic .NET


Functions are not curried in VB, and so this entry details the creation of functions that take a function and one or more arguments and returns a function that is the result of the partial application of the given function to those arguments.

This is done with two approaches: one that takes generic functions of fixed arity and returns a lambda that then calls the function, and a generalized one that allows arbitrary arity of function and arguments.


### First approach


The "type-safe" approach, which has the disadvantage that a new overload of PartialApply must be created for every combination of function arity and applied argument arity.


```vbnet
Module PartialApplication
    Function fs(Of TSource, TResult)(f As Func(Of TSource, TResult), s As IEnumerable(Of TSource)) As IEnumerable(Of TResult)
        ' This is exactly what Enumerable.Select does.
        Return s.Select(f)
    End Function

    Function f1(x As Integer) As Integer
        Return x * 2
    End Function

    Function f2(x As Integer) As Integer
        Return x * x
    End Function

    ' The overload that takes a binary function and partially applies to its first parameter.
    Function PartialApply(Of T1, T2, TResult)(f As Func(Of T1, T2, TResult), arg As T1) As Func(Of T2, TResult)
        Return Function(arg2) f(arg, arg2)
    End Function

    Sub Main()
        Dim args1 As Integer() = {0, 1, 2, 3}
        Dim args2 As Integer() = {2, 4, 6, 8}

        Dim fsf1 = PartialApply(Of Func(Of Integer, Integer), IEnumerable(Of Integer), IEnumerable(Of Integer))(AddressOf fs, AddressOf f1)
        Dim fsf2 = PartialApply(Of Func(Of Integer, Integer), IEnumerable(Of Integer), IEnumerable(Of Integer))(AddressOf fs, AddressOf f2)

        Console.WriteLine("fsf1, 0-3: " & String.Join(", ", fsf1(args1)))
        Console.WriteLine("fsf1, evens: " & String.Join(", ", fsf1(args2)))
        Console.WriteLine("fsf2, 0-3: " & String.Join(", ", fsf2(args1)))
        Console.WriteLine("fsf2, evens: " & String.Join(", ", fsf2(args2)))
    End Sub
End Module
```



### Second approach


f1 and f2 in the second approach will also be defined to use late binding in order to work with any argument that can be multiplied. In the interest of idiomatic VB.NET, a minimal amount of code is to have Option Strict off:


```vbnet
Option Strict Off

Partial Module PartialApplicationDynamic
    Function f1(x As Object) As Object
        Return x * 2
    End Function

    Function f2(x As Object) As Object
        Return x * x
    End Function
End Module
```


and in a separate file,


```vbnet
Option Strict On

Partial Module PartialApplicationDynamic
    ' Create a matching delegate type to simplify delegate creation.
    Delegate Function fsDelegate(Of TSource, TResult)(f As Func(Of TSource, TResult), s As IEnumerable(Of TSource)) As IEnumerable(Of TResult)
    Function fs(Of TSource, TResult)(f As Func(Of TSource, TResult), s As IEnumerable(Of TSource)) As IEnumerable(Of TResult)
        ' This is exactly what Enumerable.Select does.
        Return s.Select(f)
    End Function

    Function ArrayConcat(Of T)(arr1 As T(), arr2 As T()) As T()
        Dim result(arr1.Length + arr2.Length - 1) As T
        Array.Copy(arr1, result, arr1.Length)
        Array.Copy(arr2, 0, result, 1, arr2.Length)
        Return result
    End Function

    ' C# can define ParamArray delegates and VB can consume them, but VB cannot define them on its own.
    ' The argument list of calls to the resulting function thus must be wrapped in a coerced array literal.
    ' VB also doesn't allow Delegate as a type constraint. :(
    ' The function is generic solely to ease use for callers. In this case generics aren't providing any type-safety.
    Function PartialApplyDynamic(Of TDelegate, TResult)(f As TDelegate, ParamArray args As Object()) As Func(Of Object(), TResult)
        Dim del = CType(CObj(f), [Delegate])
        Return Function(rest) CType(del.DynamicInvoke(ArrayConcat(args, rest).Cast(Of Object).ToArray()), TResult)
    End Function

    Sub Main()
        Dim args1 As Object = New Object() {0, 1, 2, 3}
        Dim args2 As Object = New Object() {2, 4, 6, 8}

        Dim fsf1 = PartialApplyDynamic(Of fsDelegate(Of Object, Object), IEnumerable(Of Object))(AddressOf fs, New Func(Of Object, Object)(AddressOf f1))
        Dim fsf2 = PartialApplyDynamic(Of fsDelegate(Of Object, Object), IEnumerable(Of Object))(AddressOf fs, New Func(Of Object, Object)(AddressOf f2))

        ' The braces are array literals.
        Console.WriteLine("fsf1, 0-3: " & String.Join(", ", fsf1({args1})))
        Console.WriteLine("fsf1, evens: " & String.Join(", ", fsf1({args2})))
        Console.WriteLine("fsf2, 0-3: " & String.Join(", ", fsf2({args1})))
        Console.WriteLine("fsf2, evens: " & String.Join(", ", fsf2({args2})))
    End Sub
End Module
```


{{out|note=for both versions}}

```txt
fsf1, 0-3: 0, 2, 4, 6
fsf1, evens: 4, 8, 12, 16
fsf2, 0-3: 0, 1, 4, 9
fsf2, evens: 4, 16, 36, 64
```



## zkl


```zkl
fcn fs(f,s){s.apply(f)} fcn f1(n){n*2} fcn f2(n){n*n}
var fsf1=fs.fp(f1), fsf2=fs.fp(f2);
fsf1([0..3]);   //-->L(0,2,4,6)
fsf2([2..8,2]); //-->L(4,16,36,64)
```



{{omit from|Euphoria}}

[[Category:Functions and subroutines]]
