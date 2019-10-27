+++
title = "Closures/Value capture"
description = ""
date = 2019-10-17T14:58:01Z
aliases = []
[extra]
id = 10125
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Create a list of ten functions, in the simplest manner possible   (anonymous functions are encouraged),   such that the function at index  <big> ''<b> i </b>'' </big>   (you may choose to start   <big> ''<b> i </b>'' </big>   from either   <big> '''0''' </big>   or   <big> '''1'''), </big>    when run, should return the square of the index,   that is,   <big> ''<b> i </b>'' <sup>2</sup>.</big> 

Display the result of running any but the last function, to demonstrate that the function indeed remembers its value.


;Goal:
Demonstrate how to create a series of independent closures based on the same template but maintain separate copies of the variable closed over. 

In imperative languages, one would generally use a loop with a mutable counter variable. 

For each function to maintain the correct number, it has to capture the ''value'' of the variable at the time it was created, rather than just a reference to the variable, which would have a different value by the time the function was run.

See also: [[Multiple distinct objects]]


## Ada


One way to realize closures in Ada is the usage of protected objects. 


```Ada
with Ada.Text_IO;

procedure Value_Capture is
   
   protected type Fun is -- declaration of the type of a protected object
      entry Init(Index: Natural);
      function Result return Natural;
   private
      N: Natural := 0;
   end Fun;
   
   protected body Fun is -- the implementation of a protected object
      entry Init(Index: Natural) when N=0 is
      begin -- after N has been set to a nonzero value, it cannot be changed any more
         N := Index;
      end Init;
      function Result return Natural is (N*N);
   end Fun;
   
   A: array (1 .. 10) of Fun; -- an array holding 10 protected objects
   
begin
   for I in A'Range loop -- initialize the protected objects
      A(I).Init(I);
   end loop;
   
   for I in A'First .. A'Last-1 loop -- evaluate the functions, except for the last
      Ada.Text_IO.Put(Integer'Image(A(I).Result));
   end loop;
end Value_Capture;
```


{{out}}

```txt
 1 4 9 16 25 36 49 64 81
```



## ALGOL 68

{{works with|ALGOL 68G|2.8}} 


```algol68

[1:10]PROC(BOOL)INT squares;

FOR i FROM 1 TO 10 DO
        HEAP INT captured i := i;
        squares[i] := ((REF INT by ref i, INT by val i,BOOL b)INT:(INT i = by ref i; (b|by ref i := 0); by val i*i))
                (captured i, captured i,)
OD;

FOR i FROM 1 TO 8 DO print(squares[i](i MOD 2 = 0)) OD;
print(new line);
FOR i FROM 1 TO 10 DO print(squares[i](FALSE)) OD


```

{{out}}

```txt

         +1         +4         +9        +16        +25        +36        +49        +64
         +1         +0         +9         +0        +25         +0        +49         +0        +81       +100

```


Using partial parametrization as proposed in Algol Bulletin by Charles Lindsey. Algol68G does not support binding ''all'' actual parameters "partially" without deproceduring, so a PROC(BOOL)INT mode is used instead of a PROC INT. The variable ''captured i'' is passed twice, once by reference and once by value, to demonstrate that it is possible to capture both ways, and a little extra code is added to show that the closure can modify the captured variable.


## AntLang


```AntLang
fns: {n: x; {n expt 2}} map range[10]
(8 elem fns)[]
```



## AppleScript

{{trans|JavaScript}}

```AppleScript
on run
    set fns to {}
    
    repeat with i from 1 to 10
        set end of fns to closure(i)
    end repeat
    
    |λ|() of item 3 of fns
end run

on closure(x)
    script
        on |λ|()
            x * x
        end |λ|
    end script
end closure
```

{{Out}}

```txt
9
```


Or, in a more functional pattern of composition:


```AppleScript
-- CLOSURE --------------------------------------------------------------------

script closure
    on |λ|(x)
        script
            on |λ|()
                x * x
            end |λ|
        end script
    end |λ|
end script

|λ|() of (item 3 of (map(closure, enumFromTo(1, 10))))


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
9
```



## Axiom

Using the Spad compiler:

```Axiom
)abbrev package TESTP TestPackage
TestPackage() : with
     test: () -> List((()->Integer))
   == add
     test() == [(() +-> i^2) for i in 1..10]
```


This can be called from the interpreter using:

```Axiom
[x() for x in test()]
```


{{out}}

```Axiom
[1,4,9,16,25,36,49,64,81,100]
                                     Type: List(Integer)
```



## Babel



```babel
((main { 
    { iter 
        1 take bons 1 take
        dup cp 
        {*} cp 
        3 take 
        append }
    10 times
    collect !
    {eval %d nl <<} each }))
```


{{out}}

```babel
100
81
64
49
36
25
16
9
4
1
```


Essentially, a function has been constructed for each value to be squared (10 down to 1). The cp operator ensures that we generate a fresh copy of the number to be squared, as well as the code for multiplying, {*}. 
In the final each loop, we eval each of the constructed functions and output the result.


## Bracmat


```bracmat
( -1:?i
& :?funcs
&   whl
  ' ( 1+!i:<10:?i
    & !funcs ()'(.$i^2):?funcs
    )
& whl'(!funcs:%?func %?funcs&out$(!func$))
);

```

{{out}}

```txt
0
1
4
9
16
25
36
49
64
```



## C



### Function image copying approach


Non-portable. Copying a function body depends on implementation-specific semantics of volatile, if the replacement target still exists after optimization, if the dest memory is suitably aligned, if the memory is executable, if it makes any function calls to a relative offset, if it refers to any memory location with an absolute address, etc.  It only very occasionally works.


```c>#include <stdio.h

#include <string.h>
#include <stdlib.h>
#include <sys/mman.h>

typedef int (*f_int)();
 
#define TAG 0xdeadbeef
int _tmpl() { 
	volatile int x = TAG;
	return x * x;
}

#define PROT (PROT_EXEC | PROT_WRITE)
#define FLAGS (MAP_PRIVATE | MAP_ANONYMOUS) 
f_int dupf(int v)
{
	size_t len = (void*)dupf - (void*)_tmpl;
	f_int ret = mmap(NULL, len, PROT, FLAGS, 0, 0);
	char *p;
	if(ret == MAP_FAILED) {
		perror("mmap");
		exit(-1);
	}
	memcpy(ret, _tmpl, len);
	for (p = (char*)ret; p < (char*)ret + len - sizeof(int); p++)
		if (*(int *)p == TAG) *(int *)p = v;
	return ret;
}
 
int main()
{
	f_int funcs[10];
	int i;
	for (i = 0; i < 10; i++) funcs[i] = dupf(i);
 
	for (i = 0; i < 9; i++)
		printf("func[%d]: %d\n", i, funcs[i]());
 
	return 0;
}
```

{{out}}
<lang>func[0]: 0
func[1]: 1
func[2]: 4
func[3]: 9
func[4]: 16
func[5]: 25
func[6]: 36
func[7]: 49
func[8]: 64
```



### Greenspunned mini Lisp dialect


See [[Closures/Variable_capture/C]] for complete code. The relevant excerpt is:


```c
void init(void)
{
  t = intern(lit("t"));
  x = intern(lit("x"));
}

val square(val env)
{
  val xbind = assoc(env, x); /* look up binding of variable x in env */
  val xval = cdr(xbind);     /* value is the cdr of the binding cell */
  return num(cnum(xval) * cnum(xval));
}

int main(void)
{
  int i;
  val funlist = nil, iter;

  init();

  for (i = 0; i < 10; i++) {
    val closure_env = cons(cons(x, num(i)), nil);
    funlist = cons(func_f0(closure_env, square), funlist);
  }

  for (iter = funlist; iter != nil; iter = cdr(iter)) {
    val fun = car(iter);
    val square = funcall(fun, nao);

    printf("%d\n", cnum(square));
  }
  return 0;
}
```


Here, we create an environment explicitly as an association list 
which we can search with the <code>assoc</code> function. 
The environment contains a binding for the symbol <code>x</code>. 
The <code>square</code> function retrieves the value and returns its square.

{{out}}

```txt
$ ./a.out
81
64
49
36
25
16
9
4
1
0

```



## C++

{{works with|C++11}}

```cpp>#include <iostream

#include <functional>
#include <vector>

int main() {
  std::vector<std::function<int()> > funcs;
  for (int i = 0; i < 10; i++)
    funcs.push_back([=]() { return i * i; });
  for ( std::function<int( )> f : funcs ) 
    std::cout << f( ) << std::endl ; 
  return 0;
}
```

{{out}}

```txt
0
1
4
9
16
25
36
49
64
81

```


=={{header|C sharp|C#}}==

### Using Linq


```csharp
using System;
using System.Linq;

class Program
{
    static void Main()
    {
        var captor = (Func<int, Func<int>>)(number => () => number * number);
        var functions = Enumerable.Range(0, 10).Select(captor);
        foreach (var function in functions.Take(9))
        {
            Console.WriteLine(function());
        }
    }
}
```

{{out}}
<lang>0
1
4
9
16
25
36
49
64
```



### Using delegates only



```csharp

using System;
using System.Collections.Generic;

class Program
{
    static void Main( string[] args )
    {
        List<Func<int>> l = new List<Func<int>>();
        for ( int i = 0; i < 10; ++i )
        {
            // This is key to avoiding the closure trap, because
            // the anonymous delegate captures a reference to 
            // outer variables, not their value.  So we create 10
            // variables, and each created anonymous delegate 
            // has references to that variable, not the loop variable
            var captured_val = i;
            l.Add( delegate() { return captured_val * captured_val; } );
        }

        l.ForEach( delegate( Func<int> f ) { Console.WriteLine( f() ); } );
    }
}

```

{{out}}
<lang>0
1
4
9
16
25
36
49
64
```



## Ceylon


```ceylon
shared void run() {
	
	//create a list of closures with a list comprehension
	value closures = [for(i in 0:10) () => i ^ 2];
	
	for(i->closure in closures.indexed) {
		print("closure number ``i`` returns: ``closure()``");
	}
}
```



## Clojure


```clojure
(def funcs (map #(fn [] (* % %)) (range 11)))
(printf "%d\n%d\n" ((nth funcs 3)) ((nth funcs 4)))
```

{{Out}}

```txt
9
16
```



## CoffeeScript



```coffeescript

# Generate an array of functions.
funcs = ( for i in [ 0...10 ] then do ( i ) -> -> i * i )

# Call each function to demonstrate value capture.
console.log func() for func in funcs

```



## Common Lisp


```lisp
CL-USER> (defparameter alist
	   (loop for i from 1 to 10
	      collect (cons i (let ((i i))
				(lambda () (* i i))))))
ALIST
CL-USER> (funcall (cdr (assoc 2 alist)))
4
CL-USER> (funcall (cdr (assoc 8 alist)))
64
```


The ''loop'' mutates its binding ''i''. The purpose of <code>(let ((i i)) ...)</code> is to create a different binding ''i'' for each ''lambda'' to capture. Otherwise, all 10 lambdas would capture the same binding and return 100.


## D


### Less Functional Version


```d
import std.stdio;

void main() {
    int delegate()[] funcs;

    foreach (i; 0 .. 10)
        funcs ~= (i => () => i ^^ 2)(i);

    writeln(funcs[3]());
}
```

{{out}}

```txt
9
```


### More Functional Version


```d
void main() {
    import std.stdio, std.range, std.algorithm;

    10.iota.map!(i => () => i ^^ 2).map!q{ a() }.writeln;
}
```

{{out}}

```txt
[0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
```



## Delphi

{{works with|Delphi 2009}}

```Delphi
program Project1;

type
  TFuncIntResult = reference to function: Integer;

// use function that returns anonymous method to avoid capturing the loop variable
function CreateFunc(i: Integer): TFuncIntResult;
begin
  Result :=
    function: Integer
    begin
      Result := i * i;
    end;
end;

var
  Funcs: array[0..9] of TFuncIntResult;
  i: integer;
begin
  // create 10 anonymous functions
  for i := Low(Funcs) to High(Funcs) do
    Funcs[i] := CreateFunc(i);

  // call all 10 functions
  for i := Low(Funcs) to High(Funcs) do
    Writeln(Funcs[i]());
end.
```

{{out}}

```txt
0
1
4
9
16
25
36
49
64
81

```



## EchoLisp


```scheme

(define (fgen i) (lambda () (* i i)))
(define fs (for/vector ((i 10)) (fgen i))) ;; vector of 10 anonymous functions
((vector-ref fs 5)) ;; calls fs[5]
    → 25

```



## Dyalect

Dyalect captures variables by reference, therefore a way to achieve this is to capture a variable through a closure which in its turn returns a anonymous function like so:


```dyalect
var xs = []
const num = 10

for n in 0..(num-1) {
    xs.add((n => () => n * n)(n))
}

for x in xs {
    print(x())
}
```


{{out}}


```txt
0
1
4
9
16
25
36
49
64
81
```


This is similar to a JavaScript (ES6) solution.


## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;
 
public program()
{
    var functions := Array.allocate(10).populate:(int i => {^ i * i} );
 
    functions.forEach:(func) { console.printLine(func()) }
}
```

{{out}}

```txt
0
1
4
9
16
25
36
49
64
81
```



## Elixir


```elixir
funs = for i <- 0..9, do: (fn -> i*i end)
Enum.each(funs, &IO.puts &1.())
```


{{out}}

```txt

0
1
4
9
16
25
36
49
64
81

```



## Emacs Lisp

Emacs Lisp now has lexical-let, which allows for the capture of variables.

```lisp

(require 'cl)
(mapcar 'funcall
	(mapcar (lambda (x)
		  (lexical-let ((x x))
		    (lambda () (* x x)))) [1 2 3 4 5 6 7 8 9 10]))
;; => (1 4 9 16 25 36 49 64 81 100)

```



## Erlang

Erlang uses lexical scoping and has anonymous functions.

```erlang

-module(capture_demo).
-export([demo/0]).

demo() ->
    Funs = lists:map(fun (X) ->
                             fun () ->
                                     X * X
                             end
                     end,
                     lists:seq(1,10)),
    lists:foreach(fun (F) ->
                    io:fwrite("~B~n",[F()])
            end, Funs).

```


```txt

1> capture_demo:demo().
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
ok

```



## FreeBASIC


FreeBASIC doesn't support closures or anonymous methods, as such. However, what we can do is to create an array of objects to capture their index and then call a method on those objects which squares the index. This approach is similar to how some other object oriented languages implement closures 'under the hood'.


```freebasic
' FB 1.05.0 Win64

Type Closure
  Private:
    index As Integer
  Public:
    Declare Constructor(index As Integer = 0)
    Declare Function Square As Integer 
End Type

Constructor Closure(index As Integer = 0)
   This.index = index
End Constructor

Function Closure.Square As Integer
   Return index * index
End Function

Dim a(1 To 10) As Closure

' create Closure objects which capture their index
For i As Integer = 1 To 10
  a(i) = Closure(i)
Next

' call the Square method on all but the last object
For i As Integer = 1 to 9
  Print a(i).Square
Next

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

 1
 4
 9
 16
 25
 36
 49
 64
 81

```



## F#

Nearly identical to OCaml

```fsharp>[<EntryPoint
]
let main argv = 
    let fs = List.init 10 (fun i -> fun () -> i*i)
    do List.iter (fun f -> printfn "%d" <| f()) fs
    0
```


With List.map

```fsharp>[<EntryPoint
]
let main argv = 
    let fs = List.map (fun i -> fun () -> i*i) [0..9]
    do List.iter (fun f -> printfn "%d" <| f()) fs
    0
```


With List.mapi

```fsharp>[<EntryPoint
]
let main argv = 
    let fs = List.mapi (fun i x -> fun () -> i*i) (List.replicate 10 None) 
    do List.iter (fun f -> printfn "%d" <| f()) fs
    0
```


With an infinite sequence

```fsharp>[<EntryPoint
]
let main argv = 
    let fs = Seq.initInfinite (fun i -> fun () -> i*i)
    do Seq.iter (fun f -> printfn "%d" <| f()) (Seq.take 10 fs)
    0
```


{{out}}

```txt

0
1
4
9
16
25
36
49
64
81

```



## Factor


### Using lexical variables


```factor
USING: io kernel locals math prettyprint sequences ;

[let
    ! Create a sequence of 10 quotations
    10 iota [
        :> i            ! Bind lexical variable i
        [ i i * ]       ! Push a quotation to calculate i squared
    ] map :> seq

    { 3 8 } [
        dup pprint " squared is " write
        seq nth call .
    ] each
]
```



```txt
$ ./factor script.factor
3 squared is 9
8 squared is 64
```


The code <code>:> i</code> always binds a new variable. This happens inside a loop, so this program creates 10 different bindings. Each closure <code>[ i i * ]</code> captures a different binding, and remembers a different value.

The wrong way would use <code>f :> i! 10 iota [ i! [ i i * ] ] map :> seq</code> to mutate a single binding. Then the program would print, "3 squared is 81", "8 squared is 81".


### Using fried quotations

Forget the variable! Each ''fried quotation'' captures some values by pulling them from the stack.


```factor
USING: fry io kernel math prettyprint sequences ;

! Push a sequence of 10 quotations
10 iota [
    '[ _ dup * ]        ! Push a quotation ( i -- i*i )
] map

{ 3 8 } [
    dup pprint " squared is " write
    over nth call .
] each
drop
```



## Fantom



```fantom

class Closures
{
  Void main ()
  {
    // define a list of functions, which take no arguments and return an Int
    |->Int|[] functions := [,]

    // create and store a function which returns i*i for i in 0 to 10
    (0..10).each |Int i|
    {
      functions.add (|->Int| { i*i })
    }

    // show result of calling function at index position 7
    echo ("Function at index: " + 7 + " outputs " + functions[7].call)
  }
}

```


{{out}}

```txt

Function at index: 7 outputs 49

```



## Forth


```forth
: xt-array here { a }
    10 cells allot 10 0 do
	:noname i ]] literal dup * ; [[ a i cells + !
    loop a ;

xt-array 5 cells + @ execute .
```


{{out}}


```forth>25</lang



## Go


```go
package main

import "fmt"

func main() {
    fs := make([]func() int, 10)
    for i := range fs {
        i := i
        fs[i] = func() int {
            return i * i
        }
    }
    fmt.Println("func #0:", fs[0]())
    fmt.Println("func #3:", fs[3]())
}
```

{{out}}

```txt

func #0: 0
func #3: 9

```



## Groovy

Solution:

```groovy
def closures = (0..9).collect{ i -> { -> i*i } }
```


Test:

```groovy
assert closures instanceof List
assert closures.size() == 10
closures.each { assert it instanceof Closure }
println closures[7]()
```


{{out}}

```txt
49
```



## Haskell


Using <code>map</code>:


```haskell
fs = map (\i _ -> i * i) [1 .. 10]
```


Using list comprehensions:


```haskell
fs = [const $ i * i | i <- [1 .. 10]]
```


Using infinite lists:


```haskell
fs = take 10 coFs where coFs = [const $ i * i | i <- [1 ..]]
```


Testing:


```haskell>
 :t fs
fs :: [b -> Integer]
> map ($ ()) fs
[1,4,9,16,25,36,49,64,81,100]
> fs !! 9 $ ()
100
> fs !! 8 $ undefined
81
```


=={{header|Icon}} and {{header|Unicon}}==
This uses Unicon specific calling sequences for co-expressions.  It can be made to run under Icon by modifying the calling syntax.


```Unicon
procedure main(args)                                      # Closure/Variable Capture
    every put(L := [], vcapture(1 to 10))                 # build list of index closures
    write("Randomly selecting L[",i := ?*L,"] = ",L[i]()) # L[i]() calls the closure
end
    
# The anonymous 'function', as a co-expression.  Most of the code is standard 
# boilerplate needed to use a co-expression as an anonymous function.

procedure vcapture(x)             # vcapture closes over its argument 
   return makeProc { repeat { (x[1]^2) @ &source } }  
end

procedure makeProc(A)             # the makeProc PDCO from the UniLib Utils package
    return (@A[1], A[1])
end
```


{{libheader|Unicon Code Library}}

[http://tapestry.tucson.az.us/unilib/pack_Utils.html package Utils provides makeProc]
[https://tapestry.tucson.az.us/twiki/bin/view/Main/AnonymousFunctions Summary of Anonymous Functions in Unicon]

{{out}}

```txt
Randomly selecting L[8] = 64
```



## Io

<lang>blist := list(0,1,2,3,4,5,6,7,8,9) map(i,block(i,block(i*i)) call(i))
writeln(blist at(3) call)  // prints 9
```



## J



### Explicit version


The natural way of implementing this in J is to define a function which produces a gerund of a constant function.


```j
constF=:3 :0
  {.''`(y "_)
)
```


Thus, a list of 10 functions each producing a value in 0..9, and another with their squares:


```j
flist=: constF"0 i.10
slist=: constF"0 *:i.10
```


Referencing a function by its index (its position in that list):


```j
   flist @.3
3"_
   slist @.3
9"_
```


Using a function, given its index:


```j
   flist @.4''
4
   slist @.4''
16
```


Running a randomly picked function which is not the last one:


```j
   flist@.(?9) ''
7
   slist@.(?9) ''
25
```


===Tacit (unorthodox) version===
In J only adverbs and conjunctions (functionals) can produce verbs (functions)...  Unless they are forced to cloak  as verbs; in this instance, the rank conjunction (“) cloaks as a dyadic verb. (Note that this takes advantage of a bug/feature where the interpreter does not produce a result with [http://www.jsoftware.com/help/dictionary/dictb.htm the correct shape]):


```j
   ( VL=. (<@:((<'"')(0:`)(,^:)&_))"0@:(^&2)@:i. 10 ) NB. Producing a list of boxed anonymous verbs (functions)
┌───┬───┬───┬───┬────┬────┬────┬────┬────┬────┐
│0"_│1"_│4"_│9"_│16"_│25"_│36"_│49"_│64"_│81"_│
└───┴───┴───┴───┴────┴────┴────┴────┴────┴────┘
   
   {::&VL 5                                           NB. Evoking the 6th verb (function)
25"_
   {::&VL 5 ''                                        NB. Invoking the 6th verb with a dummy argument ('')
25
```



## Java

{{works with|Java|8+}}

```java
import java.util.function.Supplier;
import java.util.ArrayList;

public class ValueCapture {
    public static void main(String[] args) {
	ArrayList<Supplier<Integer>> funcs = new ArrayList<>();
	for (int i = 0; i < 10; i++) {
	    int j = i;
	    funcs.add(() -> j * j);
	}

	Supplier<Integer> foo = funcs.get(3);
	System.out.println(foo.get()); // prints "9"
    }
}
```


Alternative implementation that also {{works with|Java|8+}}

```java
import java.util.List;
import java.util.function.IntSupplier;
import java.util.stream.IntStream;

import static java.util.stream.Collectors.toList;

public interface ValueCapture {
  public static void main(String... arguments) {
    List<IntSupplier> closures = IntStream.rangeClosed(0, 10)
      .<IntSupplier>mapToObj(i -> () -> i * i)
      .collect(toList())
    ;

    IntSupplier closure = closures.get(3);
    System.out.println(closure.getAsInt()); // prints "9"
  }
}
```



## JavaScript



### Imperative



```javascript
var funcs = [];
for (var i = 0; i < 10; i++) {
    funcs.push( (function(i) {
                     return function() { return i * i; }
                })(i) );
}
window.alert(funcs[3]()); // alerts "9"
```


{{works with|JavaScript|1.7+}} (Firefox 2+)

```javascript
<script type="application/javascript;version=1.7">
var funcs = [];
for (var i = 0; i < 10; i++) {
    let (i = i) {
        funcs.push( function() { return i * i; } );
    }
}
window.alert(funcs[3]()); // alerts "9"
</script>
```


{{works with|JavaScript|ES6}}

```javascript
"use strict";
let funcs = [];
for (let i = 0; i < 10; ++i) {
    funcs.push((i => () => i*i)(i));
}
console.log(funcs[3]());
```



### Functional 


{{works with|JavaScript|ES5}}


```javascript
(function () {
    'use strict';

    // Int -> Int -> [Int]
    function range(m, n) {
        return Array.apply(null, Array(n - m + 1))
            .map(function (x, i) {
                return m + i;
            });
    }

    var lstFns = range(0, 10)
        .map(function (i) {
            return function () {
                return i * i;
            };
        })
        
    return lstFns[3]();

})();
```


{{out}}


```txt
9
```



{{works with|JavaScript|ES6}}

```javascript
let funcs = [...Array(10).keys()].map(i => () => i*i);
```

{{out}}

```txt

console.log(funcs[3]());
9

```



## Julia


```julia
funcs = [ () -> i^2 for i = 1:10 ]
```

{{out}}

```txt

julia> funcs[7]()
49

```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    // create an array of 10 anonymous functions which return the square of their index
    val funcs = Array(10){ fun(): Int = it * it }
    // call all but the last
    (0 .. 8).forEach { println(funcs[it]()) } 
}
```


{{out}}

```txt

0
1
4
9
16
25
36
49
64

```



## LFE


Input at the REPL:

```lisp

> (set funcs (list-comp ((<- m (lists:seq 1 10)))
                      (lambda () (math:pow m 2))))

```


Output:

```lisp

(#Fun<lfe_eval.23.101079464> #Fun<lfe_eval.23.101079464>
 #Fun<lfe_eval.23.101079464> #Fun<lfe_eval.23.101079464>
 #Fun<lfe_eval.23.101079464> #Fun<lfe_eval.23.101079464>
 #Fun<lfe_eval.23.101079464> #Fun<lfe_eval.23.101079464>
 #Fun<lfe_eval.23.101079464> #Fun<lfe_eval.23.101079464>)

```


Calling the functions:

```lisp

> (funcall (car funcs))
1.0
> (funcall (cadr funcs))
4.0
> (funcall (cadddr funcs))
16.0
> (funcall (lists:nth 8 funcs))
64.0


```



## Lingo


Lingo doesn't really support closures. But with the limitations described at [https://www.rosettacode.org/wiki/Function_composition#Lingo Function composition] and based on the fact that Lingo allows to create arbitrary code at runtime, the task can be solved like this:


```lingo
-- parent script "CallFunction"

property _code

-- if the function is supposed to return something, the code must contain a line that starts with "res="
on new (me, code)
  me._code = code
  return me
end

on call (me)
  ----------------------------------------  
  -- If custom arguments were passed, evaluate them in the current context.
  -- Note: in the code passed to the constructor they have to be referenced
  -- as arg[1], arg[2], ...
  arg = []
  repeat with i = 3 to the paramCount
    arg[i-2] = param(i)
  end repeat
  ----------------------------------------
  res = VOID
  do(me._code)
  return res
end
```



```lingo
funcs = []
repeat with i = 1 to 10
  code = "res="&i&"*"&i
  funcs[i] = script("CallFunction").new(code)
end repeat

put call(funcs[3], _movie)
-- 9
```


Since the original task is a little trivial in terms of not depending on runtime arguments, here also a solution for an extended task: let each function[i] return the square of i plus the sum of all arguments passed to it at runtime:


```lingo
funcs = []
repeat with i = 1 to 10
  code = ""
  put "res = "&i&"*"&i &RETURN after code
  put "repeat with i = 1 to arg.count" &RETURN after code
  put "  res = res + arg[i]" &RETURN after code
  put "end repeat" after code
  funcs[i] = script("CallFunction").new(code)
end repeat

put call(funcs[3], _movie, 23)
-- 32

put call(funcs[7], _movie, 4, 5, 6)
-- 64
```



## Logtalk

The example that follow uses Logtalk's native support for lambda expressions.

```logtalk

:- object(value_capture).

    :- public(show/0).
    show :-
        integer::sequence(1, 10, List),
        meta::map(create_closure, List, Closures),
        meta::map(call_closure, List, Closures).

    create_closure(Index, [Double]>>(Double is Index*Index)).

    call_closure(Index, Closure) :-
        call(Closure, Result),
        write('Closure '), write(Index), write(' : '), write(Result), nl.

:- end_object.

```

{{out}}

```text

| ?- value_capture::show.
Closure 1 : 1
Closure 2 : 4
Closure 3 : 9
Closure 4 : 16
Closure 5 : 25
Closure 6 : 36
Closure 7 : 49
Closure 8 : 64
Closure 9 : 81
Closure 10 : 100
yes

```



## Lua


```Lua

funcs={}
for i=1,10 do
    table.insert(funcs, function() return i*i end)
end
funcs[2]()
funcs[3]()

```

{{out}}

```txt
4
9

```



## M2000 Interpreter


```M2000 Interpreter

Dim Base 0, A(10)
For i=0 to 9 {
      a(i)=lambda i -> i**2
}
For i=0 to 9 {
      Print a(i)()
}

```

Print 
                   0
                   1
                   4
                   9
                  16
                  25
                  36
                  49
                  64
                  81

Export list to clipboard

```M2000 Interpreter

document a$
For i=0 to 9 {
      a$=format$("{0:0:-20}",a(i)())+{
      }
}
Clipboard a$

```


Using Inventory, and a stack object (reading from position, and another way, we pop functions, using Read)



```M2000 Interpreter

Inventory Alfa
For i=0 to 9 {
     Append Alfa, i:=lambda i -> i**2
}
For i=0 to 9 {
      Print Alfa(i)()
}

Beta=Stack
Stack Beta {
      For i=0 to 9 {
           Data lambda i -> i**2
      }
}
Def Fun(X)=X()
\\ reading functions from position 1 to 10
For i=0 to 9 {
     Print fun(stackitem(Beta,i+1))
}
\\ pop functions form stack Beta
Stack Beta {
      While not empty {
            Read M
            Print M()
      }
}


```



## Maple


```Maple>
 L := map( i -> (() -> i^2), [seq](1..10) ):
> seq( L[i](),i=1..10);                      
                  1, 4, 9, 16, 25, 36, 49, 64, 81, 100
> L[4]();
                                   16

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Function[i, i^2 &] /@ Range@10
->{1^2 &, 2^2 &, 3^2 &, 4^2 &, 5^2 &, 6^2 &, 7^2 &, 8^2 &, 9^2 &, 10^2 &}

%[[2]][]
->4
```



## Nemerle


```Nemerle
using System.Console;

module Closures
{
    Main() : void
    { 
        def f(x) { fun() { x ** 2 } }
        def funcs = $[f(x) | x in $[0 .. 10]].ToArray(); // using array for easy indexing
        
        WriteLine($"$(funcs[4]())");
        WriteLine($"$(funcs[2]())");
    }
}
```

{{out}}

```txt
16
4
```



## Nim


```nim
var funcs: seq[proc(): int] = @[]

for i in 0..9:
  (proc =
    let x = i
    funcs.add(proc (): int = x * x))()

for i in 0..8:
  echo "func[", i, "]: ", funcs[i]()
```



## Objeck


```objeck
use Collection.Generic;

class Capture {
  function : Main(args : String[]) ~ Nil {
     funcs := Vector->New()<FuncHolder<IntHolder> >;
     
     for(i := 0; i < 10; i += 1;) {
       funcs->AddBack(FuncHolder->New(\() ~ IntHolder : () => i * i)<IntHolder>);
     };

     each(i : funcs) {
       func := funcs->Get(i)->Get()<IntHolder>;
       func()->Get()->PrintLine();
     };
  }
}

```


{{output}}

```txt

0
1
4
9
16
25
36
49
64
81

```


=={{header|Objective-C}}==
{{works with|Cocoa|Mac OS X 10.6+}} with ARC

```objc
NSMutableArray *funcs = [[NSMutableArray alloc] init];
for (int i = 0; i < 10; i++) {
  [funcs addObject:[^ { return i * i; } copy]];
}

int (^foo)(void) = funcs[3];
NSLog(@"%d", foo()); // logs "9"

```



## OCaml


All functions in OCaml are closures.


```ocaml
let () =
  let cls = Array.init 10 (fun i -> (function () -> i * i)) in
  Random.self_init ();
  for i = 1 to 6 do
    let x = Random.int 9 in
    Printf.printf " fun.(%d) = %d\n" x (cls.(x) ());
  done
```


{{out}}

```txt

 fun.(4) = 16
 fun.(1) = 1
 fun.(4) = 16
 fun.(7) = 49
 fun.(3) = 9
 fun.(6) = 36

```



## Oforth


```Oforth
: newClosure(i)  #[ i sq ] ;
10 seq map(#newClosure) at(7) perform .
```


{{out}}

```txt

49

```



## PARI/GP

{{works with|PARI/GP|2.4.2 and above}}

```parigp
vector(10,i,()->i^2)[5]()
```


{{out}}

```txt
%1 = 25
```



## Perl


```perl
my @f = map(sub { $_ * $_ }, 0 .. 9);   # @f is an array of subs
print $f[$_](), "\n" for (0 .. 8); # call and print all but last
```

{{out}}

```txt

0
1
4
9
16
25
36
49
64

```



## Perl 6

{{Works with|Rakudo|2015.12}}
All blocks are anonymous closures in Perl 6, and parameters are lexicals, so it's easy to generate a list of them.  We'll use a <tt>gather</tt>/<tt>take</tt> generator loop, and call the closures in random order, just to keep things interesting.

```perl6
my @c = gather for ^10 -> $i {
    take { $i * $i }
}

.().say for @c.pick(*);  # call them in random order
```

{{out}}

```txt
36
64
25
1
16
0
4
9
81
49
```

Or equivalently, using a more functional notation:

```perl6
say .() for pick *, map -> $i { -> {$i * $i} }, ^10
```



## Phix

Phix does not support closures, but they seem easy enough to emulate

```Phix
-- First some generic handling stuff, handles partial_args
-- of any mixture of any length and element types.
sequence closures = {}
function add_closure(integer rid, sequence partial_args)
    closures = append(closures,{rid,partial_args})
    return length(closures) -- (return an integer id)
end function

function call_closure(integer id, sequence args)
    {integer rid, sequence partial_args} = closures[id]
    return call_func(rid,partial_args&args)
end function

-- The test routine to be made into a closure, or ten
-- Note that all external references/captured variables must
-- be passed as arguments, and grouped together on the lhs
function square(integer i)
    return i*i
end function

-- Create the ten closures as asked for.
-- Here, cids is just {1,2,3,4,5,6,7,8,9,10}, however ids would be more
-- useful for a mixed bag of closures, possibly stored all over the shop.
-- Likewise add_closure could have been a procedure for this demo, but
-- you would probably want the function in a real-world application.
sequence cids = {}
for i=1 to 10 do
--for i=11 to 20 do -- alternative test
    cids &= add_closure(routine_id("square"),{i})
end for
-- And finally call em (this loop is blissfully unaware what function 
-- it is actually calling, and what partial_arguments it is passing)
for i=1 to 10 do
    printf(1," %d",call_closure(cids[i],{}))
end for
```

{{out}}

```txt

 1 4 9 16 25 36 49 64 81 100

```

output if that 11 to 20 add_closure loop is used instead:

```txt

 121 144 169 196 225 256 289 324 361 400

```

Note however that any captured values are effectively immutable,
unless you also pass the id to the closure, and that in turn
does rude things to closures[id][2].

A dictionary based approach may prove somewhat easier:

```Phix
function square(integer tid)
    integer i = getd("i",tid)   -- (setd valid here too)
    return i*i
end function

sequence tids = {}
for i=1 to 10 do
--for i=11 to 20 do
    tids &= new_dict({{"i",i}})
end for
for i=1 to 10 do
    printf(1," %d",square(tids[i]))
end for
```

same output, for both tests


## PHP

{{works with|PHP|5.3+}}

```php
<?php
$funcs = array();
for ($i = 0; $i < 10; $i++) {
    $funcs[] = function () use ($i) { return $i * $i; };
}
echo $funcs[3](), "\n"; // prints 9
?>
```


{{works with|PHP|pre-5.3}}
This method can capture value types like numbers, strings, arrays, etc., but not objects.

```php
<?php
$funcs = array();
for ($i = 0; $i < 10; $i++) {
    $funcs[] = create_function('', '$i = ' . var_export($i, true) . '; return $i * $i;');
}
echo $funcs[3](), "\n"; // prints 9
?>
```



## PicoLisp


```PicoLisp
(setq FunList
   (make
      (for @N 10
         (link (curry (@N) () (* @N @N))) ) ) )
```

Test:

```txt
: ((get FunList 2))
-> 4

: ((get FunList 8))
-> 64
```



## Pike


```Pike
array funcs = ({});
foreach(enumerate(10);; int i)
{ 
  funcs+= ({ 
              lambda(int j)
              {
                  return lambda()
                         { 
                             return j*j; 
                         }; 
              }(i) 
          }); 
}
```



## PowerShell

I'm not sure that I understood the question/task.  This task seems to be the same as the 'Accumulator Factory' task.

```PowerShell

function Get-Closure ([double]$Number)
{
    {param([double]$Sum) return $script:Number *= $Sum}.GetNewClosure()
}

```


```PowerShell

for ($i = 1; $i -lt 11; $i++)
{ 
    $total = Get-Closure -Number $i

    [PSCustomObject]@{
        Function = $i
        Sum      = & $total -Sum $i
    }
}

```

{{Out}}

```txt

Function Sum
-------- ---
       1   1
       2   4
       3   9
       4  16
       5  25
       6  36
       7  49
       8  64
       9  81
      10 100

```


```PowerShell

$numbers = 1..20 | Get-Random -Count 10

foreach ($number in $numbers)
{
    $total = Get-Closure -Number $number

    [PSCustomObject]@{
        Function = $number
        Sum      = & $total -Sum $number
    }
}

```

{{Out}}

```txt

Function Sum
-------- ---
       4  16
      16 256
       3   9
      17 289
       9  81
      15 225
       7  49
       6  36
       1   1
      20 400

```



## Prolog

Works with SWI-Prolog and module '''lambda.pl''' from '''Ulrich Neumerkel'''. 

'''lambda.pl''' can be found there : http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl


```Prolog
:-use_module(library(lambda)).


closure :-
	numlist(1,10, Lnum),
	maplist(make_func, Lnum, Lfunc),
	maplist(call_func, Lnum, Lfunc).


make_func(I, \X^(X is I*I)).

call_func(N, F) :-
	call(F, R),
	format('Func ~w : ~w~n', [N, R]).

```

{{out}}

```txt
 ?- closure.
Func 1 : 1
Func 2 : 4
Func 3 : 9
Func 4 : 16
Func 5 : 25
Func 6 : 36
Func 7 : 49
Func 8 : 64
Func 9 : 81
Func 10 : 100
true.

```



## Python

The naive way does not work:

```python
funcs = []
for i in range(10):
    funcs.append(lambda: i * i)
print funcs[3]() # prints 81
```


The simplest solution is to add optional parameters with default arguments at the end of the parameter list, to create a local copy of the variable, and evaluate the variable at the time the function is created. (The optional parameter is not expected to ever be passed.) Often, the optional parameter will be named the same as the variable to be closed over (leading to odd-looking code of the form <code>foo=foo</code> in the arguments), so that the code inside the function need not be changed, but this might lead to confusion. This technique does not work for functions with a variable number of arguments.

```python
funcs = []
for i in range(10):
    funcs.append(lambda i=i: i * i)
print funcs[3]() # prints 9
```

or equivalently the list comprehension:

```python
funcs = [lambda i=i: i * i for i in range(10)]
print funcs[3]() # prints 9
```


Another solution is to wrap an immediately-executed function around our function. The wrapping function creates a new scope, and its execution forces the evaluation of the variable to be closed over.

```python
funcs = []
for i in range(10):
    funcs.append((lambda i: lambda: i * i)(i))
print funcs[3]() # prints 9
```

or equivalently the list comprehension:

```python
funcs = [(lambda i: lambda: i)(i * i) for i in range(10)]
print funcs[3]() # prints 9
```


In this case it is also possible to use <code>map()</code> since the function passed to it creates a new scope

```python
funcs = map(lambda i: lambda: i * i, range(10))
print funcs[3]() # prints 9
```


It is also possible to use <code>eval</code>.

```python
funcs=[eval("lambda:%s"%i**2)for i in range(10)]
print funcs[3]() # prints 9
```



## R


R is a natural language for this task, but you need to understand the nuances of delayed evaluation.  
Arguments in R are referred to as ''promises'' because they aren't evaluated until first use.  
If you're not careful, you can bind to a promise that hasn't yet been evaluated, and you won't get 
what you expect.   


```R

# assign 's' a list of ten functions 
s <- sapply (1:10,  # integers 1..10 become argument 'x' below 
    function (x) {
        x  # force evaluation of promise x
	function (i=x) i*i   # this *function* is the return value
    })

s[[5]]()  # call the fifth function in the list of returned functions 
[1] 25    # returns vector of length 1 with the value 25

```


Note that I bound the captured variable as the default argument on a unary function.  
If you supply your own argument, as below, it squares the supplied argument and 
ignores the default argument. 


```R

s[[5]](10) 
[1] 100 

```


As a further technicality, note that you need some extra voodoo to '''modify''' the bound argument 
with persistence across calls.  This example increments the bound variable after each call.  


```R

s <- sapply (1:10,  
    function (x) {
        x  # force evaluation of promise x
	function () {   
            R <- x*x 
            # evaluate the language expression "x <- x + 1" in the persistent parent environment 
            evalq (x <- x + 1, parent.env(environment()))
            R  # return squared value 
    }})

s[[5]]() 
[1] 25     # 5^2
s[[5]]() 
[1] 36     # now 6^2
s[[1]]()
[1] 1      # 1^2
s[[1]]()
[1] 4      # now 2^2

```


As shown, each instance increments separately.


<b>--- Edit ---</b>

I think that modifying the bound variable can be done in a simpler way.
Instead of:

```R
    evalq (x <- x + 1, parent.env(environment()))
```

substitute:

```R
    x <<- x + 1
```

Testing:

```txt

> s[[5]]()
[1] 25
> s[[5]]()
[1] 36
> s[[5]]()
[1] 49
> s[[2]]()
[1] 4
> s[[2]]()
[1] 9
> s[[2]]()
[1] 16

```



## Racket


```racket

#lang racket
(define functions (for/list ([i 10]) (λ() (* i i))))
(map (λ(f) (f)) functions)

```

{{out}}

```racket

'(0 1 4 9 16 25 36 49 64 81)

```



## Red


```Red

funs: collect [repeat i 10 [keep func [] reduce [i ** 2]]]

>> funs/7
== 49

```



## REXX

This REXX version supports both a one─ and zero─based list   (it can be specified at the command line.)

The default is to use a zero─based list.

The list can also be specified at the command line.   The default is an ordered list based on an obscure sequence   (but puzzle enthusiasts can figure it out).

No error checking is performed on the user input(s).

```rexx
/*REXX program has a list of ten functions, each returns its invocation (index) squared.*/
parse arg seed base $                            /*obtain optional arguments from the CL*/
if datatype(seed, 'W')  then call random ,,seed  /*Not given?  Use random  start seed.  */
if base=='' | base=","  then base=0              /* "    "     Use a zero─based list.   */
if $=''  then $= 8 5 4 9 1 3 2 7 6 0             /* "    "     Use ordered function list*/
                                                 /*the $ list must contain 10 functions.*/
say 'the' word("zero one", base+1)'─based list is: '   $       /*show list of functions.*/
                                                 /*BASED  must be either   1   or   0.  */
?='.'random(0, 9)                                /*get a random name of a function.     */
interpret  'CALL'  ?                             /*invoke a randomly selected function. */
say 'function '    ?     " returned "    result  /*display the value of random function.*/
exit                                             /*stick a fork in it,  we're all done. */
/*────────────────────────[Below are the closest things to anonymous functions in REXX].*/
.0: return  .(0)                                 /*function  .0   ─── bump its counter. */
.1: return  .(1)                                 /*    '     .1    "    "   "     "     */
.2: return  .(2)                                 /*    '     .2    "    "   "     "     */
.3: return  .(3)                                 /*    '     .3    "    "   "     "     */
.4: return  .(4)                                 /*    '     .4    "    "   "     "     */
.5: return  .(5)                                 /*    '     .5    "    "   "     "     */
.6: return  .(6)                                 /*    '     .6    "    "   "     "     */
.7: return  .(7)                                 /*    '     .7    "    "   "     "     */
.8: return  .(8)                                 /*    '     .8    "    "   "     "     */
.9: return  .(9)                                 /*    '     .9    "    "   "     "     */
/*──────────────────────────────────────────────────────────────────────────────────────*/
.:  arg #;  _=wordpos(#,$); if _==0  then return 'not in the list.'; return (_-(\base))**2
```

{{out|output|text=  when using the default input   which assume a zero─based list):}}

```txt

the zero─based list is:  8 5 4 9 1 3 2 7 6 0
function  .0  returned  81

```

{{out|output|text=  when using the input of:     <tt> ,   1 </tt>}}

```txt

the one─based list is:  8 5 4 9 1 3 2 7 6 0
function  .0  returned  100

```



## Ring


```ring

x = funcs(7)
see x + nl

func funcs n
     fn = list(n)
     for i = 1 to n    
         fn[i] =i*i
     next 
     return fn      

```

Output:

```txt

1
4
9
16
25
36
49

```



## Ruby


```ruby
procs = Array.new(10){|i| ->{i*i} } # -> creates a lambda
p procs[7].call # => 49
```


In Ruby, lambdas (and procs) are closures.


## Rust

One note here about referencing values and capturing values: 

Rust employs strong ownership rules that do not allow mutating a value that is referenced (pointed to without allowing mutation) from elsewhere. It also doesn't allow referencing a value that may be dropped before the reference is released. The proof that we really did capture the value is therefore unnecessary. Either we did or it wouldn't have compiled.


```rust
fn main() {
    let fs: Vec<_> = (0..10).map(|i| {move || i*i} ).collect();
    println!("7th val: {}", fs[7]());
}
```


{{out}}

```txt
7th val: 49
```



## Scheme



```scheme
;;; Collecting lambdas in a tail-recursive function.
(define (build-list-of-functions n i list)
  (if (< i n)
      (build-list-of-functions n (+ i 1) (cons (lambda () (* (- n i) (- n i))) list))
      list))

(define list-of-functions (build-list-of-functions 10 1 '()))

(map (lambda (f) (f)) list-of-functions)

((list-ref list-of-functions 8))
```


{{out}}

```scheme
'(1 4 9 16 25 36 49 64 81)
81
```


----

Using Scheme [http://srfi.schemers.org/srfi-1/srfi-1.html SRFI 1] ''iota'' procedure can be simplified to:

```scheme

(define list-of-functions (map (lambda (x) (lambda () (* x x))) (iota 0 1 10)))

; print the result
(display
  (map (lambda (n) (n)) list-of-functions)
(newline)

```



## Scala


```scala
val closures=for(i <- 0 to 9) yield (()=>i*i)
0 to 8 foreach (i=> println(closures(i)()))
println("---\n"+closures(7)())
```

{{out}}

```txt
0
1
4
9
16
25
36
49
64
---
49
```



## Sidef


```ruby
var f = (
    10.of {|i| func(j){i * j} }
)

9.times { |j|
    say f[j](j)
}
```

{{out}}

```txt

0
1
4
9
16
25
36
49
64

```


Starting from i=1:

```ruby
var f = (1..10).map { |i|
    func(j){i * j}
}

for j (1..9) {
    say f[j-1](j)
}
```

{{out}}

```txt

1
4
9
16
25
36
49
64
81

```



## Smalltalk


```smalltalk
funcs := (1 to: 10) collect: [ :i | [ i * i ] ] .
(funcs at: 3) value displayNl .
```

{{out}}

```txt
9
```



## Sparkling

In Sparkling, upvalues (variables in the closure) are captured by value.


```sparkling
var fnlist = {};
for var i = 0; i < 10; i++ {
	fnlist[i] = function() {
		return i * i;
	};
}

print(fnlist[3]()); // prints 9
print(fnlist[5]()); // prints 25
```


Alternately:


```sparkling
var fnlist = map(range(10), function(k, v) {
	return function() {
		return v * v;
	};
});

print(fnlist[3]()); // prints 9
print(fnlist[5]()); // prints 25
```



## Swift

By default, Swift captures variables by reference. A naive implementation like the following C-style for loop does not work:

```swift
var funcs: [() -> Int] = []
for var i = 0; i < 10; i++ {
  funcs.append({ i * i })
}
println(funcs[3]()) // prints 100
```


However, using a for-in loop over a range does work, since you get a new constant at every iteration:

```swift
var funcs: [() -> Int] = []
for i in 0..<10 {
  funcs.append({ i * i })
}
println(funcs[3]()) // prints 9
```


The C-style for loop can also work if we explicitly capture the loop counter:

```swift
var funcs: [() -> Int] = []
for var i = 0; i < 10; i++ {
  funcs.append({ [i] in i * i })
}
println(funcs[3]()) // prints 9
```


Alternately, we can also use <code>map()</code> to map over a range, and create the squaring closure inside the mapping closure which has the integer as a parameter:

```swift
let funcs = [] + map(0..<10) {i in { i * i }}
println(funcs[3]()) // prints 9
```



## Tcl

Tcl does not support closures (either value-capturing or variable-capturing) by default, but value-capturing closures are easy to emulate.

```tcl
package require Tcl 8.6; # Just for tailcall command
# Builds a value-capturing closure; does NOT couple variables
proc closure {script} {
    set valuemap {}
    foreach v [uplevel 1 {info vars}] {
	lappend valuemap [list $v [uplevel 1 [list set $v]]]
    }
    set body [list $valuemap $script [uplevel 1 {namespace current}]]
    # Wrap, to stop untoward argument passing
    return [list apply [list {} [list tailcall apply $body]]]
    # A version of the previous line compatible with Tcl 8.5 would be this
    # code, but the closure generated is more fragile:
    ### return [list apply $body]
}

# Simple helper, to avoid capturing unwanted variable
proc collectFor {var from to body} {
    upvar 1 $var v
    set result {}
    for {set v $from} {$v < $to} {incr v} {lappend result [uplevel 1 $body]}
    return $result
}
# Build a list of closures
proc buildList {} {
    collectFor i 0 10 {
	closure {
	    # This is the body of the closure
	    return [expr $i*$i]
	}
    }
}
set theClosures [buildList]
foreach i {a b c d e} {# Do 5 times; demonstrates no variable leakage
    set idx [expr {int(rand()*9)}]; # pick random int from [0..9)
    puts $idx=>[{*}[lindex $theClosures $idx]]
}
```

{{out}}

```txt

5=>25
0=>0
8=>64
1=>1
8=>64

```



## TXR



### =Sugared=



```txrlisp
(let ((funs (mapcar (ret (op * @@1 @@1)) (range 1 10))))
  [mapcar call [funs 0..-1]])
```


{{out}}


```txrlisp
(1 4 9 16 25 36 49 64 81)
```



### =Desugared=


{{trans|Emacs Lisp}}

The explicit <code>lambda</code> structure here is much like the implicit ones in the "Sugared" example:


```txrlisp
;; Dropping distracting "skip last" requirement
;; (not implemented in original Elisp either).
(mapcar 'call
	(mapcar (lambda ()
		  (lambda () (* x x))) '(1 2 3 4 5 6 7 8 9 10)))
```



### =Delimited Continuations=


In this interactive example, we capture delimited continuations inside a simple <code>for</code> loop. Because the variable binding environment is not necessarily in the stack which is captured, we rebind the loop variable.


```txt
This is the TXR Lisp interactive listener of TXR 124.
Use the :quit command or type Ctrl-D on empty line to exit.
1> (let ((conts))
      (for ((i 0)) ((< i 10) (nreverse conts)) ((inc i))
        (let ((cap i))
           (push (block sqr
                    (suspend sqr f (op f nil))
                    (* cap cap))
                 conts))))
(#<interpreted fun: lambda #:rest-0112> #<interpreted fun: lambda #:rest-0112>
 #<interpreted fun: lambda #:rest-0112> #<interpreted fun: lambda #:rest-0112>
 #<interpreted fun: lambda #:rest-0112> #<interpreted fun: lambda #:rest-0112>
 #<interpreted fun: lambda #:rest-0112> #<interpreted fun: lambda #:rest-0112>
 #<interpreted fun: lambda #:rest-0112> #<interpreted fun: lambda #:rest-0112>)
2> (call (first *1))
0
3> (call (second *1))
1
4> (call (fifth *1))
16
5> (call [*1 4])
16
6> (call [*1 7])
49
```


The <code>suspend</code> operator suspends the execution of the <code>sqr</code> block, causing it to return the function <code>(op f nil)</code>. The variable <code>f</code> represents the captured continuation as a function. Continuation functions take one mandatory argument. We don't need that here, hence the <code>(op f nil)</code> expression is returned: it curries the one arg continuation function <code>f</code> to a function with no arguments.

The loop pushes these suspended continuations into a list, and then <code>nreverse</code>-s it.

We then interactively call the continuations in the list.

Whenever we call a continuation, the <code>(block sqr ...)</code> environment is restored. and the suspended computation inside the block resumes by returning out of the <code>(suspend ...)</code> form normally. The block then executes to completion, returning the <code>(* cap cap)</code> form's value.  At that point, our call to the continuation terminates, yielding that value.


## Yabasic


```Yabasic

dim funcs$(10)

sub power2(i)
    return i * i
end sub

for i = 1 to 10
    funcs$(i) = "power2"
next

for i = 1 to 10
    print execute(funcs$(i), i)
next

```



## zkl

Create a closure of the index over a square function

```zkl
(0).pump(10,List,fcn(i){i*i}.fp)[8]() //-->64
list:=(0).pump(10,List,fcn(i){i*i}.fp);
foreach n in (list.len()-1) { list[n]().println() }
list.run(True).println()
```

{{out}}

```txt

0
1
4
9
16
25
36
49
64
L(0,1,4,9,16,25,36,49,64,81)

```


{{omit from|BASIC}}
{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Locomotive Basic}}
{{omit from|PureBasic}}
{{omit from|ZX Spectrum Basic}}

[[Category:Functions and subroutines]]
