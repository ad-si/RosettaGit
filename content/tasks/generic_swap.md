+++
title = "Generic swap"
description = ""
date = 2019-10-18T10:31:38Z
aliases = []
[extra]
id = 2119
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "360_assembly",
  "8th",
  "acl2",
  "ada",
  "aime",
  "algol_68",
  "amigae",
  "applescript",
  "arc",
  "autohotkey",
  "awk",
  "axe",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "burlesque",
  "c",
  "chapel",
  "clojure",
  "cmake",
  "coldfusion",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "dc",
  "dcl",
  "delphi",
  "e",
  "echolisp",
  "elena",
  "elixir",
  "emacs_lisp",
  "erlang",
  "factor",
  "falcon",
  "fish",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "frink",
  "futurebasic",
  "gambas",
  "gecho",
  "go",
  "gri",
  "groovy",
  "haskell",
  "idl",
  "j",
  "java",
  "javascript",
  "joy",
  "jq",
  "julia",
  "kotlin",
  "lang5",
  "lasso",
  "lhogho",
  "lingo",
  "lisaac",
  "livecode",
  "logo",
  "logtalk",
  "lolcode",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "maxima",
  "maxscript",
  "metafont",
  "min",
  "miniscript",
  "nemerle",
  "netrexx",
  "newlisp",
  "nial",
  "nim",
  "oasys_assembler",
  "ocaml",
  "oforth",
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
  "rebol",
  "retro",
  "rexx",
  "ring",
  "rlab",
  "ruby",
  "run_basic",
  "rust",
  "sather",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "slate",
  "smalltalk",
  "snobol4",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "thinbasic",
  "trith",
  "txr",
  "ubasic_4th",
  "unix_shell",
  "ursala",
  "v",
  "vbscript",
  "verbexx",
  "visual_basic",
  "visual_basic_dotnet",
  "visual_foxpro",
  "wart",
  "xpl0",
  "yorick",
  "zkl",
]
+++

## Task

Write a generic swap function or operator which exchanges the values of two variables (or, more generally, any two storage places that can be assigned), regardless of their types.

If your solution language is statically typed please describe the way your language provides genericity.

If variables are typed in the given language, it is permissible that the two variables be constrained to having a mutually compatible type, such that each is permitted to hold the value previously stored in the other without a type violation.
That is to say, solutions do not have to be capable of exchanging, say, a string and integer value, if the underlying storage locations are not attributed with types that permit such an exchange.

Generic swap is a task which brings together a few separate issues in programming language semantics.

Dynamically typed languages deal with values in a generic way quite readily, but do not necessarily make it easy to write a function to destructively swap two variables, because this requires indirection upon storage places or upon the syntax designating storage places.

Functional languages, whether static or dynamic, do not necessarily allow a destructive operation such as swapping two variables regardless of their generic capabilities.

Some static languages have difficulties with generic programming due to a lack of support for ([[Parametric Polymorphism]]).

Do your best!





## 360 Assembly

Three consecutive exclusive OR's swap variable contents

```360 Assembly

SWAP     CSECT ,                   control section start
         BAKR  14,0                stack caller's registers
         LR    12,15               entry point address to reg.12
         USING SWAP,12             use as base
         MVC   A,=C'5678____'      init field A
         MVC   B,=C'____1234'      init field B
         LA    2,L                 address of length field in reg.2
         WTO   TEXT=(2)            Write To Operator, results in:
*                                  +5678________1234
         XC    A,B                 XOR A,B
         XC    B,A                 XOR B,A
         XC    A,B                 XOR A,B. A holds B, B holds A
         WTO   TEXT=(2)            Write To Operator, results in:
*                                  +____12345678____
         PR    ,                   return to caller
         LTORG ,                   literals displacement
L        DC    H'16'               halfword containg decimal 16
A        DS    CL8                 field A, 8 bytes
B        DS    CL8                 field B, 8 bytes
         END   SWAP                program end

```



## 8th


```forth

swap

```

Or to swap between the stack and a var:

```forth

dup @ -rot !

```



## ACL2


```Lisp
(defun swap (pair)
   (cons (cdr pair)
         (car pair)))

(let ((p (cons 1 2)))
  (cw "Before: ~x0~%After: ~x1~%" p (swap p)))
```



## Ada

The generic parameters for an Ada generic procedure are defined in a procedure specification, while the algorithm is defined in a procedure body. The first code snippet is the procedure specification. The second code snippet is the procedure body.


```ada
generic
   type Swap_Type is private; -- Generic parameter
procedure Generic_Swap (Left, Right : in out Swap_Type);

procedure Generic_Swap (Left, Right : in out Swap_Type) is
   Temp : constant Swap_Type := Left;
begin
   Left := Right;
   Right := Temp;
end Generic_Swap;
```



### usage

To use the generic swap procedure, you need to instantiate the procedure for each type that you intend to use.

```ada

with Generic_Swap;
...
type T is ...
procedure T_Swap is new Generic_Swap (Swap_Type => T);
A, B : T;
...
T_Swap (A, B);

```



## Aime

Aime is statically typed.  A generic swap utility may nonetheless be defined in terms of parameters of unspecified type and pass by reference.

```aime
void
__swap(&, &,,)
{
    set(0, $3);
    set(1, $2);
}

void
swap(&, &)
{
    xcall(xcall, __swap);
}
```



## ALGOL 68

A generic swap operator =:= was proposed in [[wp:ALGOL Bulletin|ALGOL Bulletin]] for standard ALGOL 68 so that the compiler could optimise the operation.  However such an operator was not adopted and needs to be manually defined for each '''mode''' required.

```algol68
MODE GENMODE = STRING;

GENMODE v1:="Francis Gary Powers", v2:="Vilyam Fisher";

PRIO =:= = 1;

OP =:= = (REF GENMODE v1, v2)VOID: (
  GENMODE tmp:=v1; v1:=v2; v2:=tmp
);

v1 =:= v2;

print(("v1: ",v1, ", v2: ", v2, new line))
```

```txt

v1: Vilyam Fisher, v2: Francis Gary Powers

```


### Special option

The B6700 Algol compiler offered access to a special machine operation via a function called ReadLock(a,b) that could be invoked on a variety of operands. By using the ability to #define this = that; one could define Swap(a,b) to be a:=ReadLock(a,b) to attain the appearance of a Swap operation. This all relied on the working of magnetic core memory, specifically that to read a word, the word is made zero and in the process the memory hardware notes which bits were thereby flipped. Thus it passes on the value in the word and meanwhile, rewrites that content back to the word so as to preserve its value on reading. Similarly, to write a value to a word, the word is first zeroed.

ReadLock(a,b) functioned by reading ''a'' and writing its value to ''b'', but also, recovering the value that was in ''b'' which it returns as the result of the function - which is written to ''a'' by the assignment, completing the swap. The ReadLock part is "atomic" or not interruptable, so it is used in semaphores and the like, but was available for other use. It swapped a single word, so could swap types such as integers or floating-point numbers (single precision) thus being somewhat generic.


## AmigaE

The simpler way to write a swap is to use the Amiga E ability to return multiple values. All basic data type in Amiga E can be held by its LONG type, and complex data type (like lists) are indeed pointers (which fits into a LONG too); so, because of the fact that Amiga E is not strongly typed, this solution works for any type.

```amigae
PROC swap(a,b) IS b,a

PROC main()
  DEF v1, v2, x
  v1 := 10
  v2 := 20
  v1, v2 := swap(v1,v2)
  WriteF('\d \d\n', v1,v2)           -> 20 10
  v1 := [ 10, 20, 30, 40 ]
  v2 := [ 50, 60, 70, 80 ]
  v1, v2 := swap(v1,v2)
  ForAll({x}, v1, `WriteF('\d ',x))  -> 50 60 70 80
  WriteF('\n')
  ForAll({x}, v2, `WriteF('\d ',x))  -> 10 20 30 40
  WriteF('\n')
ENDPROC
```



## AppleScript

AppleScript has built-in support for swapping. This is generic and works for all combinations of data types.

```AppleScript
set {x,y} to {y,x}
```



## Arc


```Arc
(mac myswap (a b)
     (w/uniq gx
             `(let ,gx a
                   (= a b)
                   (= b ,gx))))

(with (a 1
       b 2)
      (myswap a b)
      (prn "a:" a #\Newline "b:" b))

```



## AutoHotkey


```autohotkey
Swap(ByRef Left, ByRef Right)
{
    temp := Left
    Left := Right
    Right := temp
}
```



## AWK


```AWK

# syntax: GAWK -f GENERIC_SWAP.AWK
BEGIN {
    printf("%s version %s\n",ARGV[0],PROCINFO["version"])
    foo = 1
    bar = "a"
    printf("\n%s %s\n",foo,bar)
    rc = swap("foo","bar") # ok
    printf("%s %s %s\n",foo,bar,rc?"ok":"ng")
    printf("\n%s %s\n",foo,bar)
    rc = swap("FOO","BAR") # ng
    printf("%s %s %s\n",foo,bar,rc?"ok":"ng")
    exit(0)
}
function swap(a1,a2,  tmp) { # strings or numbers only; no arrays
    if (a1 in SYMTAB && a2 in SYMTAB) {
      if (isarray(SYMTAB[a1]) || isarray(SYMTAB[a2])) {
        return(0)
      }
      tmp = SYMTAB[a1]
      SYMTAB[a1] = SYMTAB[a2]
      SYMTAB[a2] = tmp
      return(1)
    }
    return(0)
}

```

```txt

gawk version 4.1.0

1 a
a 1 ok

a 1
a 1 ng

```



## Axe

The Exch() command can swap data of any size at any two addresses. This example swaps two 2-byte variables.

```axe
Exch(°A,°B,2)
```



## Batch File


Swap using pass-by-name


```dos
@echo off
setlocal enabledelayedexpansion
set a=1
set b=woof
echo %a%
echo %b%
call :swap a b
echo %a%
echo %b%
goto :eof

:swap
set temp1=!%1!
set temp2=!%2!
set %1=%temp2%
set %2=%temp1%
goto :eof
```



## BBC BASIC

===Built-in function===

```bbcbasic
      a = 1.23 : b = 4.56
      SWAP a,b
      PRINT a,b

      a$ = "Hello " : b$ = "world!"
      SWAP a$,b$
      PRINT a$,b$
```


### Custom function


```bbcbasic
      a = 1.23 : b = 4.56
      PROCswap(^a,^b, 5)
      PRINT a,b

      a$ = "Hello " : b$ = "world!"
      PROCswap(^a$,^b$, 6)
      PRINT a$,b$
      END

      DEF PROCswap(a%, b%, s%)
      LOCAL i%
      FOR i% = 0 TO s%-1
        SWAP a%?i%,b%?i%
      NEXT
      ENDPROC
```

```txt

      4.56      1.23
world!    Hello

```



## Bracmat


```bracmat
(!a.!b):(?b.?a)
```



## Burlesque



```burlesque

\/

```


Stack-based swap.


## C

This has a restriction that a and b must be the same size.


```c
void swap(void *va, void *vb, size_t s)
{
  char t, *a = (char*)va, *b = (char*)vb;
  while(s--)
    t = a[s], a[s] = b[s], b[s] = t;
}
```


If you have gcc, you can write a preprocessor macro with <tt>__typeof__</tt>.

* ''Caution:'' <tt>__typeof__</tt> is a gcc extension, not part of standard C. <tt>__typeof__</tt> does not conflict with C89 because the standard allows compilers to add keywords with underscores like <tt>__typeof__</tt>.


```c
#define Swap(X,Y)  do{ __typeof__ (X) _T = X; X = Y; Y = _T; }while(0)
```


Usage examples are:


```c
#include <stdio.h>

#define Swap(X,Y)  do{ __typeof__ (X) _T = X; X = Y; Y = _T; }while(0)

struct test
{
  int a, b, c;
};


int main()
{
  struct test t = { 1, 2, 3 };
  struct test h = { 4, 5, 6 };
  double alfa = 0.45, omega = 9.98;

  struct test *pt = &t;
  struct test *th = &h;

  printf("%d %d %d\n", t.a, t.b, t.c );
  Swap(t, h);
  printf("%d %d %d\n", t.a, t.b, t.c );
  printf("%d %d %d\n", h.a, h.b, h.c );

  printf("%lf\n", alfa);
  Swap(alfa, omega);
  printf("%lf\n", alfa);

  printf("%d\n", pt->a);
  Swap(pt, th);
  printf("%d\n", pt->a);
}
```


This is tested with GCC with <tt>-std=c89</tt> option.


## C++

Generic programming in C++ is provided through templates. Templates in C++ are quite powerful: They form a Turing-complete compile-time sub-language. However, that power isn't needed for swap. Note that the C++ standard library already provides a swap function which contains optimized implementations for standard library types; thus it's advisable to use that instead of a self-written variant like the one below.

While the standard allows to separate declaration and definition of templates into different files using the export keyword, most compilers (including the most used ones) don't implement that. Therefore in practice, templates declared in header files also have to be defined there.

The implementation of the swap function template is straightforward:


```cpp
template<typename T>

void swap(T& left, T& right)
{
  T tmp(left);
  left = right;
  right = tmp;
}
```

Note that this function requires that the type T has an accessible copy constructor and assignment operator.


The standard utility 'swap' can be used to swap two values:


```cpp
std::swap(x,y);
```


It will work with any types.


### C++11

C++11 adds move constructors which can be more efficient than copy constructors.

```cpp
template<class T>

void swap(T &lhs, T &rhs){
  T tmp = std::move(lhs);
  lhs = std::move(rhs);
  rhs = std::move(tmp);
}
```


## C#

### C#: Using a generic method

C# 2.0 introduced the concept of generics to the language. Generics are outwardly
similar to C++ templates, but are implemented quite differently: generics are
maintained generically at runtime rather than being substitued with definite types
by the compiler. Generics are intended to promote reusable, efficient, type-safe
code, and are used widely throughout the .NET framework and 3rd party libraries,
especially in collections. C# generics are less flexible than C++ templates, but
are more strongly typed and arguably easier to work with.


```csharp
static void Swap<T>
(ref T a, ref T b)
{
    T temp = a;
    a = b;
    b = temp;
}
```


Usage:


```c#
int a = 1;
int b = 2;
Swap(ref a, ref b); // Type parameter is inferred.
```



### C#: Using tuple syntax

C# 7.0 introduced language support for tuples, which are implemented using the <code>ValueTuple</code> family of structs. The example below creates a tuple with the values of <code>b</code> and <code>a</code> and uses deconstructing assignment to assign the members of the tuple back to the variables.

```c#
int a = 1;
int b = 2;
(a, b) = (b, a);
```



## Chapel

Chapel includes a swap operator:

```chapel
a <=> b
```

and supports swapping directly via tuples and destructuring:

```chapel
(a, b) = (b, a)
```

Both variables must be of the same type.
The [[Fibonacci sequence#Chapel|Fibonnacci implementation]] contains an example.


## Clojure


```lisp

(defn swap [pair] (reverse pair))    ; returns a list
(defn swap [[a b]] '(b a))           ; returns a list
(defn swap [[a b]] [b a])            ; returns a vector

```


The latter two implementations use destructured binding to define local names for the two elements.


## CMake

CMake has only one data type: the string.


```cmake
function(swap var1 var2)
  set(_SWAP_TEMPORARY "${${var1}}")
  set(${var1} "${${var2}}" PARENT_SCOPE)
  set(${var2} "${_SWAP_TEMPORARY}" PARENT_SCOPE)
endfunction(swap)
```



```cmake
set(x 42)
set(y "string")
swap(x y)
message(STATUS ${x})  # -- string
message(STATUS ${y})  # -- 42
```


Because of limitations in CMake, there are a few specific situations where swap() will fail to swap the variables.

# When ''_SWAP_TEMPORARY'' is the name of the second variable:
```cmake
set(x 42)
set(_SWAP_TEMPORARY "string")
swap(x _SWAP_TEMPORARY)
message(STATUS ${x})                # -- 42
message(STATUS ${_SWAP_TEMPORARY})  # -- 42
```
 Inside swap(), its local variable ''_SWAP_TEMPORARY'' shadows the original ''_SWAP_TEMPORARY'' from the parent scope, preventing access to the original value.
# When value of either variable is "CACHE" or "PARENT_SCOPE":
```cmake
string(TOUPPER CACHE x)
set(y "string")
swap(x y)  # CMake Error... set given invalid arguments for CACHE mode.
```
 swap() can never set a variable to "CACHE" or "PARENT_SCOPE", because these are keywords of set() command.


## Common Lisp


```lisp
(rotatef a b)

(psetq a b b a)
```



## ColdFusion

This is another standard swap.


```cfm
<cfset temp = a />

<cfset a = b />
<cfset b = temp />
```



## Crystal

Crystal directly supports swapping:


```ruby
a, b = b, a
```



## D


```d
import std.algorithm: swap; // from Phobos standard library

// The D solution uses templates and it's similar to the C++ one:
void mySwap(T)(ref T left, ref T right) {
    auto temp = left;
    left = right;
    right = temp;
}

void main() {
    import std.stdio;

    int[] a = [10, 20];
    writeln(a);

    // The std.algorithm standard library module
    // contains a generic swap:
    swap(a[0], a[1]);
    writeln(a);

    // Using mySwap:
    mySwap(a[0], a[1]);
    writeln(a);
}
```

```txt
[10, 20]
[20, 10]
[10, 20]
```



## dc

We use two registers to swap in POSIX dc.

```dc
1 2 SaSbLaLb f
=2 1
```

Reverse (r) is a built-in stack command available as a GNU extension for dc.

```dc
1 2 r f
=2 1
```


## DCL

symbols do not have to be declared, they can be integers or strings, they can change type on the fly

```DCL
$ a1 = 123
$ a2 = "hello"
$ show symbol a*
$ gosub swap
$ show symbol a*
$ exit
$
$ swap:
$  t = a1
$  a1 = a2
$  a2 = t
$ return
```

```txt
$ @generic_swap
  A1 = 123   Hex = 0000007B  Octal = 00000000173
  A2 = "hello"
  A1 = "hello"
  A2 = 123   Hex = 0000007B  Octal = 00000000173
```


=={{header|Déjà Vu}}==
To swap the two top-most items on the stack:

```dejavu
swap>
```

To swap two variables without needing a third name, using the stack for temporary storage:

```dejavu
set :a set :b @a @b>
```



## Delphi

Delphi does not have generics as such. The following code must be copied for each type that a swap is required. T should be changed to the required type.

```Delphi

procedure Swap_T(var a, b: T);
var
  temp: T;
begin
  temp := a;
  a := b;
  b := temp;
end;

```


Generics were introduced with Delphi 2009

```Delphi

program GenericSwap;

type
  TSwap = class
    class procedure Swap<T>(var left, right: T);
  end;

class procedure TSwap.Swap<T>(var left, right: T);
var
  temp : T;
begin
  temp := left;
  left := right;
  right := temp;
end;

var
  a, b : integer;

begin
  a := 5;
  b := 3;
  writeln('Before swap: a=', a, ' b=', b);
  TSwap.Swap<integer>(a, b);
  writeln('After swap: a=', a, ' b=', b);
end.

```



## E

(slots)


```e
def swap(&left, &right) {
  def t := left
  left := right
  right := t
}
```


(functional)


```e
def swap([left, right]) {
  return [right, left]
}
```



## EchoLisp


```lisp

;; 1)
;; a macro will do it, as shown in Racket (same syntax)
(define-syntax-rule (swap a b)
    (let ([tmp a])
    (set! a b)
    (set! b tmp)))

(define A 666)
(define B "simon")
(swap A B)
A → "simon"
B → 666

;; 2)
;; The list-swap! function allows to swap two items inside a list, regardless of their types
;; This physically alters the list

(define L ' ( 1 2 3 4 🎩 ))
(list-swap! L 1 ' 🎩 )
    → (🎩 2 3 4 1)

```


## Elena

ELENA 4.1 :

```elena
import extensions;

swap(ref object v1, ref object v2)
{
    var tmp := v1;

    v1 := v2;
    v2 := tmp
}

public program()
{
    var n := 2;
    var s := "abc";

    console.printLine(n," ",s);

    swap(ref n, ref s);

    console.printLine(n," ",s)
}
```


```txt

2 abc
abc 2

```



## Elixir

Elixir provides a robust mechanism of pattern matching; the <code>=</code> operator is actually the match operator. Using the match operator, values can be assigned and variables can be bound or unbound, but only on the left (<code>=:</code>).

```Elixir

x = 4
y = 5

{y,x} = {x,y}
y # => 4
x # => 5

[x,y] = [y,x]
x # => 4
y # => 5

```

Data structures can be used both for matching and for generally destructuring complex data. We can use anonymous functions to create a generic swap in iex. Note: using multiple value requires a data construct on which to match (as opposed to, say, Ruby's <code>a,b = 1,2</code>), but we can use a list:


```Elixir

swap = fn x,y -> [y|x] end
[x|y] = swap.(1,2)
x # => 2
y # => 1

```


Variables can be bound and rebound regardless of type

```Elixir

swap_tuple = fn {x,y} -> {y,x} end
{a,b} = swap_tuple.({1,:ok})
a # => :ok
b # => 1

swap_list  = fn [x,y] -> [y,x] end
[a,b] = swap_list.([1,"2"])
a # => "2"
b # => 1

```



## Emacs Lisp


```Lisp
(defun swap (a-sym b-sym)
  "Swap values of the variables given by A-SYM and B-SYM."
  (let ((a-val (symbol-value a-sym)))
    (set a-sym (symbol-value b-sym))
    (set b-sym a-val)))
(swap 'a 'b)
```


A macro can take variable names unquoted.  Here <code>prog1</code> eliminates the temporary variable above so as to avoid any chance of a name clash between the two variables and the temporary.


```Lisp
(defmacro swap (a b)
  `(setq ,b (prog1 ,a (setq ,a ,b))))
```


A macro could use the <code>cl.el</code> <code>setf</code> which can store to various kinds of expressions as locations, for example list elements.  <code>psetf</code> evaluates all its values before storing (like the [[#Common Lisp|Common Lisp example]]).


```Lisp
(require 'cl)
(defmacro swap (a b)
  `(psetf ,a ,b
          ,b ,a))

(setq lst (list 123 456))
(swap (car lst) (cadr lst))
;; now lst is '(456 123)
```



## Erlang


Erlang variables are single assignment and Erlang is dynamically typed, so this task doesn't really apply.

The closest thing would be to swap the items in a list (shown in the shell).


```Erlang

1> L = [a, 2].
[a,2]
2> lists:reverse(L).
[2,a]

```


Or swap the items in a tuple (also shown in the shell).


```Erlang

1> T = {2,a}.
{2,a}
2> list_to_tuple(lists:reverse(tuple_to_list(T))).
{a,2}

```


=={{header|F_Sharp|F#}}==

```fsharp
let swap (a,b) = (b,a)
```



## Factor

Depending on how you look at it: this task doesn't apply, or it's trivial:

```factor
swap>
```



## Falcon


```falcon

a = 1
b = 2
a,b = arr = b,a

```

Reading right to left: Assign b & a into an array variable called arr, then assign into a & b


## Fish

Swap the top two values on the stack:

```fish
$
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Generic_swap this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran

```fortran
MODULE Genericswap
  IMPLICIT NONE

  INTERFACE Swap
    MODULE PROCEDURE Swapint, Swapreal, Swapstring
  END INTERFACE

CONTAINS

  SUBROUTINE Swapint(a, b)
    INTEGER, INTENT(IN OUT) :: a, b
    INTEGER :: temp
    temp = a ; a = b ; b = temp
  END SUBROUTINE Swapint

  SUBROUTINE Swapreal(a, b)
    REAL, INTENT(IN OUT) :: a, b
    REAL :: temp
    temp = a ; a = b ; b = temp
  END SUBROUTINE Swapreal

  SUBROUTINE Swapstring(a, b)
    CHARACTER(*), INTENT(IN OUT) :: a, b
    CHARACTER(len(a)) :: temp
    temp = a ; a = b ; b = temp
  END SUBROUTINE Swapstring
END MODULE Genericswap

PROGRAM EXAMPLE
  USE Genericswap
  IMPLICIT NONE
  INTEGER :: i1 = 1, i2 = 2
  REAL :: r1 = 1.0, r2 = 2.0
  CHARACTER(3) :: s1="abc", s2="xyz"

  CALL Swap(i1, i2)
  CALL Swap(r1, r2)
  CALL Swap(s1, s2)

  WRITE(*,*) i1, i2   ! Prints 2 and 1
  WRITE(*,*) r1, r2   ! Prints 2.0 and 1.0
  WRITE(*,*) s1, s2   ! Prints xyz and abc
END PROGRAM EXAMPLE
```



## Forth

Since the Forth stack can contain pointers to any data type all we need is...
<!-- if this is deemed unworthy, then the Factor, Postscript, and Retro examples should also be removed -->

```forth
swap>
```



## FreeBASIC

FreeBASIC already has a built-in generic Swap procedure but a macro can be used to build another one:

```freebasic
' FB 1.05.0
#Macro Declare_Swap(T)
Sub Swap_##T(ByRef t1 As T, ByRef t2 As T)
  Dim temp As T = t2
  t2 = t1
  t1 = temp
End Sub
#EndMacro

Dim As Integer i, j
i = 1 : j = 2

Declare_Swap(Integer) ' expands the macro
Swap_Integer(i, j)
Print i, j

Dim As String s, t
s = "Hello" : t = "World"

Declare_Swap(String)
Swap_String(s, t)
Print s, t

Print
Print "Press any key to exit"
Sleep
```


```txt

 2             1
World         Hello

```



## Free Pascal


```pascal
{$ifdef fpc}{$mode delphi}{$H+}{$endif}
{ note this is compiled with delphi mode but will only compile in Free Pascal }
{ Delphi doesn't support this syntax                                          }
procedure swap<T>(var left,right:T);
var
  temp:T;
begin
  temp:=left;
  left:=right;
  right:=temp;
end;
var
  a:string = 'Test';
  b:string = 'me';
begin
  writeln(a:6,b:6);
  swap<string>(a,b);
  writeln(a:6,b:6);
end.
```


```txt

Output:
    Test    me
    me  Test
```



## Frink

The following example will work on all Frink data types:


```frink

[b,a] = [a,b]

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as long   i, j
dim as double x, y
dim as Str15  a, b

i = 1059 : j = 62
print i, j
swap i, j
print i, j
print

x = 1.23 : y = 4.56
print x, y
swap x, y
print x, y
print

a = "Hello" : b = "World!"
print a, b
swap a, b
print a, b

```


Output:

```txt

 1059            62
 62              1059

 1.23            4.56
 4.56            1.23

Hello           World!
World!          Hello

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=b746e857f6f280fb92c204795f6053be Click this link to run this code]'''

```gambas
Public Sub Main()
Dim vA As Variant = " World"
Dim vB As Variant = 1

Swap vA, vB

Print vA; vB

End
```

Output:

```txt

1 World

```



## Gecho


```gecho

1 !0 2 !1

```

Now tape[0] and tape[1] are set to 1 and 2, respectively.

```gecho

&0 &1 !0 pop !1

```

This pushes the value of tape[0] to the stack, tape[1] to the stack, sets tape[0] to the top element, and then pops it, then tape[1] to the top element.


## Go


### Built in

Not a valid solution, since the task requires writing a function or operator, but it is worth mentioning that Go's built in assignment operator does generic swap.  The following swaps the values of a and b as long as they are of identical type.

```go
a, b = b, a
```


### Pass interfaces

A generic swap function can easily be written however, if you require the caller to use variables of the empty interface type.  The empty interface can hold a value of any type.

```go
package main

import "fmt"

func swap(a, b *interface{}) {
    *a, *b = *b, *a
}

func main() {
    var a, b interface{} = 3, "four"
    fmt.Println(a, b)
    swap(&a, &b)
    fmt.Println(a, b)
}
```

```txt

3 four
four 3

```



### Pass pointers

Somewhat less restrictive, this version allows pointers of any type to be passed, as long as they are the same type.

```go
package main

import (
    "fmt"
    "reflect"
)

func swap(a, b interface{}) error {
    ta := reflect.TypeOf(a)
    tb := reflect.TypeOf(b)
    if ta != tb {
        return fmt.Errorf("swap args are different types: %v and %v", ta, tb)
    }
    if ta.Kind() != reflect.Ptr {
        return fmt.Errorf("swap args must be pointers")
    }
    ea := reflect.ValueOf(a).Elem()
    eb := reflect.ValueOf(b).Elem()
    temp := reflect.New(ea.Type()).Elem()
    temp.Set(ea)
    ea.Set(eb)
    eb.Set(temp)
    return nil
}

func main() {
    a, b := 3, "cats"
    fmt.Println("a b:", a, b)
    err := swap(a, b)
    fmt.Println(err, "\n")

    c, d := 3, 4
    fmt.Println("c d:", c, d)
    err = swap(c, d)
    fmt.Println(err, "\n")

    e, f := 3, 4
    fmt.Println("e f:", e, f)
    swap(&e, &f)
    fmt.Println("e f:", e, f, "\n")

    type mult struct {
        int
        string
    }

    g, h := mult{3, "cats"}, mult{4, "dogs"}
    fmt.Println("g h:", g, h)
    swap(&g, &h)
    fmt.Println("g h:", g, h)
}
```

```txt

a b: 3 cats
swap args are different types: int and string

c d: 3 4
swap args must be pointers

e f: 3 4
e f: 4 3

g h: {3 cats} {4 dogs}
g h: {4 dogs} {3 cats}

```



## Gri

Putting <code>&</code> in a call makes the parameter "call by reference", giving a command the opportunity to modify a variable in the caller.  So to swap variables,


```Gri
`Swap Vars &.a. &.b.'
{
    new .temp.
    .temp. = \.word2.
    \.word2. = \.word3.
    \.word3. = .temp.
    delete .temp.
}

.foo. = 123
.bar. = 456
Swap Vars &.foo. &.bar.

show .foo. " " .bar.
# prints "456 123"
```


Or similar to swap synonyms (strings),


```Gri
`Swap Syns &\a &\b'
{
    new \temp
    \temp = "\.word2."
    \.word2. = "\.word3."
    \.word3. = "\temp"
    delete \temp
}

\quux = "one"
\xyzzy = "two"
Swap Syns &\quux &\xyzzy

show "\quux \xyzzy"
# prints "two one"
```



## Groovy


Groovy has support for swapping built in:


```groovy
(a, b) = [b, a]
```


But the task calls for a "generic swap method" to be written, so here it is:


```groovy
def swap(a, b) {
    [b, a]
}
```

This function doesn't mutate anything, but simply returns a new list with the order of the elements switched. It can be used like shown below:

```groovy
def (x, y) = swap(1, 3)
assert x == 3
assert y == 1
```


Some examples here show an in-place swap of indexed elements in an array or collection, so for completeness here is an in-place swap of arbitrary indexed elements in a list:

```groovy
def listSwap = { a, i, j ->
    assert (0..<(a.size())).containsAll([i,j]);
    a[[j,i]] = a[[i,j]]
}

def list = [2,4,6,8]
listSwap(list, 1, 3)
assert list == [2,8,6,4]
```



## Haskell


###  Pure swap

Usually Haskellers prefer to work with immutable data. The following function doesn't mutate anything, but simply returns a new pair with the order of the elements switched.

The type signature, the first line, is optional; it may be inferred.


```haskell
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
```

This <code>swap</code> function is available in the <code>Data.Tuple</code> standard library module in GHC 7.0+


###  Swap mutable variables

The following function swaps the contents of two mutable references. Again the type signature is optional.

```haskell
import Control.Monad.Ref
swap :: MonadRef r m => r a -> r a -> m ()
swap xRef yRef = do
   x<-readRef xRef
   y<-readRef yRef
   writeRef xRef y
   writeRef yRef x
```


=={{header|Icon}} and {{header|Unicon}}==
Icon provides a :=: operator for this.  Additionally, there is a reversible exchange operator <-> that reverses the exchange if resumed.

```icon
procedure main()
   x := 1
   y := 2
   x :=: y
   write(x," ",y)
   # swap that will reverse if surrounding expression fails
   if x <-> y & x < y then write(x, " ", y)
end

```



## IDL

IDL is dynamically typed and array-centric, so swapping is quite easy for any data type.  The TEMPORARY function sets its argument to "undefined", and allows us to swap without any large copying.


```IDL
pro swap, a, b
  c = temporary(a)
  a = temporary(b)
  b = temporary(c)
end
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 DEF SWAP(REF A,REF B)
110   LET T=A:LET A=B:LET B=T
120 END DEF
130 LET A=1:LET B=2
140 PRINT A,B
150 CALL SWAP(A,B)
160 PRINT A,B
```



## J

J is dynamically typed and J's cycle primitive (<code>C.</code>) will swap elements of an arbitrary list.  See also J's reference documentation on [http://www.jsoftware.com/help/dictionary/dccapdot.htm <code>C.</code>]

Shown here are a list of prime numbers and the result of J's parser on some random text (inverting the parsing process on the swapped result):


```J
   (<2 4) C. 2 3 5 7 11 13 17 19
2 3 11 7 5 13 17 19
   (<0 3)&C.&.;:'Roses are red. Violets are blue.'
Violets are red. Roses are blue.
```


Also, if the argument list can be guaranteed to be a pair, J's reverse primitive will swap the pair.


```J
   |.2 3
3 2
   |.&.;:'one two'
two one
```


A generic destructive swap of named values would instead require reference to the locations being destroyed.  Here's an implementation of that:


```J
destructiveSwap=:4 :0
  t=. do y
  (y)=: do x
  (x)=: t
  i.0 0  NB. result is meaningless
)
```


Example use:


```J
   V1=: 'cat'
   V2=: 7
   'V1' destructiveSwap 'V2'
   V1
7
   V2
cat
```



## Java

Java uses references, so it can't swap the values of two variables that don't belong to a class.


```java
class Pair<T>
 {
    T first;
    T second;
}
public static <T> void swap(Pair<T> p) {
   T temp = p.first;
   p.first = p.second;
   p.second = temp;
}
```



## JavaScript

JavaScript uses references, but if a function reassigns a parametric reference, the new object only has a local reference. However, if we wrap the variables to be switched in some other structure, like an object or an array, we can easily swap the values.

There's no actual "generics", since all variables are just that, variables of some kind.

The below function expects an array of length 2 (or longer), and switches the first two values in place, in the same array. This is closely related to how the Java solution works.


```javascript
function swap(arr) {
  var tmp = arr[0];
  arr[0] = arr[1];
  arr[1] = tmp;
}
```


Also there is metaprogramming solution. It uses code generation and <tt>eval</tt>. To avoid naming conflicts(user can pass <tt>'tmp'</tt>, which causes <tt>var tmp = tmp</tt>) it uses buildin, per activation context (thats why it is enclosed into self executing lambda), var <tt>arguments</tt> for temp storage.

```javascript
function swap(aName, bName) {
  eval('(function(){ arguments[0] = aName; aName = bName; bName = arguments[0] })()'
    .replace(/aName/g, aName)
    .replace(/bName/g, bName)
  )
}
var x = 1
var y = 2
swap('x', 'y')

```


Solution without <tt>eval()</tt>, assuming that the code is running in the browser (<tt>window</tt> is the global object)

```javascript
function swap(a, b) {
  var tmp = window[a];
  window[a] = window[b];
  window[b] = tmp;
}
var x = 1;
var y = 2;
swap('x', 'y');

```



## Joy

Provided that the stack contains at least two elements and/or aggregates:

```joy
swap
```

changes the order of those elements and/or aggregates.


## jq

jq is a functional language, so one is more likely to want to swap the two elements of an array than to swap the values of two variables, but jq does have variables and their values can be swapped, for example, using an intermediate variable, say $tmp, as illustrated here:
```jq
jq -n '1 as $a | 2 as $b | $a as $tmp | $b as $a | $tmp as $b | [$a,$b]'
```


Here is a filter that will swap the elements of a two-element array:

```jq
reverse
```


And here is a filter that, if presented with an array, will in effect copy it and then swap the i-th and j-th items, it being understood that if a is an array and k < 0 or k >= (a|length), then a[k] will evaluate to null:

```jq
def swap(i;j): .[i] as $t | .[i] = .[j] | .[j] = $t;
```



## Julia

Similar to Python, Julia has built-in support for swapping:


```julia
a, b = b, a
```



## Kotlin

As Kotlin does not support passing parameters by reference and tuples cannot be destructured automatically to pre-existing variables, it's just as easy to swap variable values 'inline' rather than using a function. However, here's one of way of doing it generically using the latter:

```scala
// version 1.1

fun <T> swap(t1: T, t2: T) = Pair(t2, t1)

fun main(args: Array<String>) {
    var a = 3
    var b = 4
    val c = swap(a, b) // infers that swap<Int> be used
    a = c.first
    b = c.second
    println("a = $a")
    println("b = $b")
    var d = false
    var e = true
    val f = swap(d, e) // infers that swap<Boolean> be used
    d = f.first
    e = f.second
    println("d = $d")
    println("e = $e")
}
```


```txt

a = 4
b = 3
d = true
e = false

```



## Lang5


```Lang5
swap        # stack
reverse     # array
```




## Lasso


```lasso
define swap(a, b) => (: #b, #a)

local(a) = 'foo'
local(b) = 42

local(a,b) = swap(#a, #b)
stdoutnl(#a)
stdoutnl(#b)
```


```txt
42
foo
```



### Using Decompositional Assignment



```lasso
local(a)   = 'hair'
local(b)   = 'moose'
local(a,b) = (: #b, #a)
stdoutnl(#a)
stdoutnl(#b)
```


```txt
moose
hair
```



## Lisaac


```Lisaac
(a, b) := (b, a);
```



## LiveCode


```LiveCode
put "first" into a1
put "last" into b2
swap a1,b2
put a1 && b2

command swap @p1, @p2
    put p2 into p3
    put p1 into p2
    put p3 into p1
end swap
```



## Logo


```logo

to swap :s1 :s2
  localmake "t thing :s1
  make :s1 thing :s2
  make :s2 :t
end

make "a 4
make "b "dog
swap "a "b        ; pass the names of the variables to swap
show list :a :b  ; [dog 4]

```



## Lhogho

Lhogho is very similar except that it does not have a localmake opcode.


```logo

to swap :s1 :s2
  local "t
  make "t thing :s1
  make :s1 thing :s2
  make :s2 :t
end

make "a 4
make "b "dog
swap "a "b        ; pass the names of the variables to swap
show list :a :b  ; [dog 4]

```



## Lingo

A generic swap function is not possible in Lingo, since scalar values are passed by value. But the following solution shows how such generic swapping still can be achieved by executing a single line of code:

```lingo
on swap (x, y)
  return "tmp="&x&RETURN&x&"="&y&RETURN&y&"=tmp"
end
```

Usage:

```lingo
x = 1
y = 2
do(swap("x","y"))
put x, y
-- 2 1
```



## Logtalk


```logtalk
:- object(paws).

    :- public(swap/4).
    swap(First, Second, Second, First).

:- end_object.
```

Usage examples:

```logtalk
| ?- paws::swap(apples, oranges, X, Y).
X = oranges
Y = apples
yes

| ?- paws::swap(3.14, ext(lgt), X, Y).
X = ext(lgt)
Y = 3.14
```

yes


## LOLCODE

LOLCODE's dynamic typing makes generic swapping trivial. In addition, the special <tt>IT</tt> variable‒which contains the most recently evaluated expression‒permits doing so without explicitly creating a temporary variable.


```LOLCODE
HAI 1.3

I HAS A foo ITZ "kittehz"
I HAS A bar ITZ 42

foo, foo R bar, bar R IT

VISIBLE foo BTW, 42
VISIBLE bar BTW, kittehz

KTHXBYE
```



## Lua

Lua evaluates the values on the right-hand side before assigning them to the variables on the left-hand side. This behaviour allows the following notation to be used to swap two values:

```lua

x, y = y, x                -- swap the values inside x and y
t[1], t[2] = t[2], t[1]    -- swap the first and second values inside table t

```


Usage example:

```lua

x, y = 3, 4
print(x, y)                --> 3 4
x, y = y, x                -- swap
print(x, y)                --> 4 3

```


## M2000 Interpreter

Swap is a statement in M2000 which get two identifiers, variables or array items. Variables and Array items are all internal type of Variant. Normally a numeric variable hold the first type we assign to it. Numeric types are: Double, Single, Decimal, Currency, Decimal, Long, Integer. Boolean is also a type but true and false are not boolean, they are double -1 and 0). When we use Swap internal only variant swap happen, without check of type of variant.

Here we make a local Swap and pass by reference, numbers and strings. References created without testing what type of variant we use. So calling swap we make a swap moving bytes, and for strings this means moving pointers to BSTR type of strings.


```M2000 Interpreter

\\ pgramming again Swap (for local use)
Module Swap (&a, &b) {
      \\ this call internal command - by default is by reference without using character &
      Swap a, b
}
X=20
Y=100
Swap &x, &y
Print X, Y, Type$(X)="Double",Type$(Y)="Double"
A$="A$"
B$="B$"
Swap &A$, &B$
Print A$="B$", B$="A$"

```


Using Swap (internal command), for variables, groups (only for variables inside groups), pointers to groups, pointers to containers, etc.

```M2000 Interpreter

a=1000
b=50
Swap a,b
Print a, b
A$="Hello"
B$="There"
Swap A$, B$
Print A$, B$
Dim A(4)
A(0):=1,2,3,4
Swap  A(3), A(2)
Print A(3), A(2)

\\ Groups are Values
Group alfa {
      x=10, y=20
}
Group Beta {
      x=40, y=50
}
\\ with List we show the public variables
\\ so among other variables there are:
\\ alfa[Group], alfa.x=10, alfa.y=20, beta[group], beta.x=40, beta.y=50
\\ So Alfa.x and Beta.x are simple variables, we can use swap
Swap Alfa.x, Beta.x
Print Alfa.x, Beta.x
Swap Alfa.x, Beta.x
List
\\ We have to use a third variable to hold value
For This {
      \\ Local always make a new variable, and shadow any same local variable
      Local M=alfa
      alfa=beta
      beta=m
}
\\ Now M erased (defined in For This block)
Print Alfa.x=40, Alfa.y=50
Print Beta.x=10, Beta.y=20

\\ Using -> we make pointers to Alfa, and Beta
\\ These pointers are valid until Alfa and Beta erased, or get Empty Group (->0)
pA->Alfa
pB->Beta
Print pA=>x=40, pA=>y=50
Print pB=>x=10, pB=>y=20
Swap pA,pB
Print pA=>x=10, pA=>y=20   ' pA point to beta
Print pB=>x=40, pB=>y=50   'pB point to alfa
Print type$(pA)="Group",Valid(pA=>X)=True
pA->0
pB->0
Print type$(pA)="Group",Valid(pA=>X)=False
\\ These pointers are valid until get Empty Group (->0), they point to a copy of Alfa  and Beta
\\ both are in heap as "closed groups"
pA->(Alfa)
pB->(Beta)
Print pA=>x, pA=>y
\\ swap need variables or arrays
\\ pA=>x are closed to object so we have to open the object, and use the open one, where all public variables of group can be used
For pA, pB {
      Swap .x, .y
}
Print pA=>x, pA=>y
For pA, pB {
      Swap .x, .y
}
Print pA=>x, pA=>y
Print pB=>x, pB=>y
Swap pA,pB
Print pA=>x, pA=>y
Print pB=>x, pB=>y

L1=lambda x=1->{=x : x++}
L2=lambda x=100->{=x : x--}
Print L1()=1, L2()=100
Swap L1, L2
Print L1()=99, L2()=2
Swap L1, L2
Print L1()=3, L2()=98
\\ swap change pointers to containers (here pointers to arrays)
A=(1,2,3,4,5)
B=(6,7,8,9,10)
Swap A, B
Print A
Print B
\\ Arrays with () in names are values
Dim A(10)=1, B(10)=2
For This {
      Dim C()
      C()=A()
      A()=B()
      B()=C()
}
Print A()
Print B()

```



## M4


```m4
define(`def2', `define(`$1',`$2')define(`$3',`$4')')dnl
define(`swap', `def2(`$1',defn(`$2'),`$2',defn(`$1'))')dnl
dnl
define(`a',`x')dnl
define(`b',`y')dnl
a b
swap(`a',`b')
a b
```


```txt

x y

y x

```



## Maple

The assignment operator in Maple can swap values, since the right hand side is evaluated before the assignment occurs.

```Maple

> a, b := 2, "foo":
> a;
                                   2

> b;
                                 "foo"

> a, b := b, a: # SWAP
> a;
                                 "foo"

> b;
                                   2

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

Mathematica functions are generic by default; however, it has to be told not to evaluate the arguments before executing the function.

```mathematica
swap[a_, b_] := {a, b} = {b, a}
SetAttributes[swap, HoldAll]
```


=={{header|MATLAB}} / {{header|Octave}}==
Numercial swaps are trivial operations. In fact, they are so natural to the language that multiple swaps can be performed simultaneously.

Example:

```MATLAB
>>
 a = [30 40 50 60 70]

a =

    30    40    50    60    70

>> a([1 3]) = a([3 1]) %Single swap

a =

    50    40    30    60    70

>> a([1 2 4 3]) = a([2 3 1 4]) %Multiple swap, a.k.a permutation.

a =

    40    30    60    50    70
```



A generic swap (compatible with any variable type) can be performed with the ''deal'' command:


```MATLAB

>> a = 12
a =  12

>> b = 'foo'
b = foo

>> [b, a] = deal (a, b)
b =  12
a = foo

```



## Maxima


```maxima
a: 10$
b: foo$

/* A simple way to swap values */
[a, b]: [b, a]$

a; /* foo */
b; /* 10 */

/* A macro to hide this */
swap(x, y) ::= buildq([x, y], ([x, y]: [y, x], 'done))$

swap(a, b)$

a; /* 10 */
b; /* foo */
```



## MAXScript


```maxscript
swap a b>
```



## Metafont

In Metafont, only <tt>numeric</tt> declarations can be omitted; any other type, must be explicitly given. So our swap, in order to declare and use a proper temporary variable(<tt>?</tt> in this code), must check the type of the variable passed (we check only for a; if b is of another kind, an error will occur)


```metafont
vardef swap(suffix a, b) =
  save ?; string s_;
  if boolean a: boolean ?
    elseif numeric a: numeric ? % this one could be omitted
    elseif pair a: pair ?
    elseif path a: path ?
    elseif pen a: pen ?
    elseif picture a: picture ?
    elseif string a: string ?
    elseif transform a: transform ? fi;
  ? := a; a := b; b := ?
enddef;
```


''Examples'':


```metafont
j := 10;
i := 5;
show j, i;
swap(j,i);
show j, i;

boolean truth[];
truth1 := true;
truth2 := false;
show truth1, truth2;
swap(truth1,truth2);
show truth1, truth2;
```



## min

Like many other stack languages, this is trivial.

```min
swap>
```



## MiniScript

Like many other languages, MiniScript passes references by value, so a straightforward swap is impossible.  However, there is a trick: given the map the variales are in (e.g. <code>locals</code>) and the ''names'' of the variables, we can swap them.

```MiniScript
swap = function(map, a, b)
    temp = map[a]
    map[a] = map[b]
    map[b] = temp
end function

x = 1
y = 2
print "BEFORE: x=" + x + ", y=" + y
swap(locals, "x", "y")
print "AFTER:  x=" + x + ", y=" + y
```

{{out}

```txt
BEFORE: x=1, y=2
AFTER:  x=2, y=1
```


=={{header|Modula-3}}==

```modula3
GENERIC INTERFACE GenericSwap(Elem);

PROCEDURE Swap(VAR left: Elem.T; VAR right: Elem.T);

END GenericSwap.
```


```modula3
GENERIC MODULE GenericSwap(Elem);

PROCEDURE Swap(VAR left: Elem.T; VAR right: Elem.T) =
  VAR temp: Elem.T := left;
  BEGIN
    left := right;
    right := temp;
  END Swap;

BEGIN
END GenericSwap.
```


Here is an example usage for integers:

```modula3
INTERFACE IntSwap = GenericSwap(Integer) END IntSwap.
```


```modula3
MODULE IntSwap = GenericSwap(Integer) END IntSwap.
```


```modula3
MODULE Main;

IMPORT IntSwap, IO, Fmt;

VAR left := 10;
    right := 20;

BEGIN
  IO.Put("Left = " & Fmt.Int(left) & "\n");
  IntSwap.Swap(left, right);
  IO.Put("Left = " & Fmt.Int(left) & "\n");
END Main.
```


```txt

Left = 10
Left = 20

```



## Nemerle

For pairs, namespace Nemerle.Utility.Pair contains Swap():

```Nemerle
def coords    = (1, -1);
def invcoords = Swap(coords);
```

Or to swap two mutable variables of the same type:
```Nemerle
a <-> b;
```

But, enough about built in functionality, let's demonstrate using generics:

```Nemerle
Swap[T, U] (a : T, b : U) : U * T
{
    (b, a)
}
```



## NetRexx

Values stored in the '''default''' <tt>Rexx</tt> data type are treated as typeless data; context is based on the contents.
Swapping the contents of variables stored in <tt>Rexx</tt> object can be achieved via the <tt>PARSE</tt> instruction.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

  -- Simple values with no spaces can be swapped without the use of a parse template
  lval = 27
  rval = 5
  say 'Before - <lval>'lval'</lval> <rval>'rval'</rval>'
  parse (lval rval) rval lval
  say 'After  - <lval>'lval'</lval> <rval>'rval'</rval>'
  say

  -- More complex data needs to use some form of parsing template
  lval = 'This value started on the left'
  rval = 'This value started on the right'
  dlm  = 12x80facebead01 -- some delimiting value that is unlikely to occur in the LVAL to be swapped
  say 'Before - <lval>'lval'</lval> <rval>'rval'</rval>'
  parse (lval || dlm || rval) rval (dlm) lval
  say 'After  - <lval>'lval'</lval> <rval>'rval'</rval>'
  say

  return

```

```txt

Before - <lval>27</lval> <rval>5</rval>
After  - <lval>5</lval> <rval>27</rval>

Before - <lval>This value started on the left</lval> <rval>This value started on the right</rval>
After  - <lval>This value started on the right</lval> <rval>This value started on the left </rval>

```



## Nial

Like J

```nial
|reverse 1 2
=2 1
```



## NewLISP


```NewLISP
(swap a b)
```



## Nim

Builtin procedure <code>swap</code>. Example usage:

```nim
swap(a, b)
```



## OASYS Assembler

You can swap variable <tt>%A#</tt> with <tt>%B#</tt> by writing:
<lang oasys_oaa>%A#%B#<%B#%A#<>>
```

A method which can be called to implement it can be written like:
<lang oasys_oaa>[&SW,A^,B^],A^<,B^<<,B^<,A^<<>>
```

To call such method:
<lang oasys_oaa>+%A#%B#&SW
```



## OCaml

Tuples are immutable in OCaml. This function doesn't mutate anything, but simply returns a new pair with the order of the elements switched.

```ocaml
let swap (x, y) = (y, x)
```

If the arguments are constrained to be reference values, a swap function is simple:

```ocaml
let swapref x y =
  let temp = !x in
    x := !y;
    y := temp
```



## Oforth



```oforth
swap>
```



## Oz

Oz variables are dataflow variables and cannot be changed once a value has been assigned. So a swap operation on dataflow variables does not make sense.

We can write a swap procedure for cells, though. Cells are mutable references.

```oz
  proc {SwapCells A B}
     Tmp = @A
  in
     A := @B
     B := Tmp
  end
```


Or shorter, if we exploit the fact that the assignment operator <code>:=</code> returns the old value of the cells:

```oz
  proc {SwapCells A B}
     B := A := @B
  end
```


A functional swap, operating on pairs:

```oz
  fun {SwapPair A#B}
     B#A
  end
```



## PARI/GP

Pari is near-typeless&mdash;everything is a GEN.

```parigp
my(tmp=a);
a=b;
b=tmp;
```


```parigp
[a,b]=[b,a]
```



## Pascal

Standard Pascal does not have generics, but FreePascal has a start:

```pascal
program generictest;

{$mode objfpc}

type
  generic TSwap<T> = procedure (var a, b: T);

procedure Proc1(var a, b: integer);
  var
    temp: integer;
  begin
    temp := a;
    a := b;
    b := temp;
  end;

var
  S, T: integer;
  SwapInt: specialize TSwap<integer>;

begin
  S := 4;
  T := 3;
  SwapInt := @Proc1;
  writeln(S, T:2);
  SwapInt(S, T);
  writeln(S, T:2);
end.
```

```txt

4 3
3 4

```

'''since FreePascal version 3.2.0:'''

```pascal
program generic_test;
{$mode objfpc}{H+}
uses
  SysUtils;

generic procedure GSwap<T>(var L, R: T);
var
  Tmp: T;
begin
  Tmp := L;
  L := R;
  R := Tmp;
end;

var
  I, J: Integer;
begin
  I := 100;
  J := 11;
  WriteLn('I = ',  I, ', J = ', J);
  specialize GSwap<Integer>(I, J);
  WriteLn('I = ',  I, ', J = ', J);
end.
```

```txt

I = 100, J = 11
I = 11, J = 100

```



## Perl

Perl has support for swapping built-in


```perl
($y, $x) = ($x, $y);
```


Here's a generic swap routine:


```perl
sub swap {@_[0, 1] = @_[1, 0]}
```



## Perl 6

As Perl 5. Perl 6 supports type constraints for variables and subroutines, unlike Perl 5, but the default is still to permit all values.

Alternatively, you can write it like this:


```perl6
($x, $y) .= reverse;
```



## Phix

The following applies to any types. Subscripting and nesting may also be used freely on either side.

```Phix
{a,b} = {b,a}
```



## PHP



```php
function swap(&$a, &$b) {
    list($a, $b) = array($b, $a);
}
```



## PicoLisp

[http://software-lab.de/doc/refX.html#xchg xchg] works with any data type

```PicoLisp
(let (A 1  B 2)
   (xchg 'A 'B)
   (println A B) )

(let (Lst1 '(a b c)  Lst2 '(d e f))
   (xchg (cdr Lst1) (cdr Lst2))
   (println Lst1 Lst2) )
```

```txt
2 1
(a e c) (d b f)
```



## PL/I



### = Using the preprocessor =


```pli

%swap: procedure (a, b);
   declare (a, b) character;
   return ( 't=' || a || ';' || a || '=' || b || ';' || b '=t;' );
%end swap;
%activate swap;
```


The statement:-
   swap (p, q);

is replaced, at compile time, by the three statements as in-line code:
   t = p; p = q; q = t;


### = Using generic procedures =


```pli
declare swap generic (
   swapf when (float, float),
   swapc when (char, char));

swapf: proc (a, b);
   declare (a, b, t) float;
   t = a; a = b; b = t;
end swapf;
swapc: proc (a, b);
   declare (a, b) character(*);
   declare t character (length(b));
   t = a; a = b; b = t;
end swapc;

declare (r, s) character (5);
call swap (r, s);
```


Both of the above are not completely generic, but depend on either the presence of
* a temporary variable with the same attributes of the variables to be swapped, OR
* data-attribute specific procedures for the swap

The following code is completely generic, but, in line with the usual safety offered by PL/I, swaps only the contents up to the storage occupied by the smallest of the two variables: [[User:Prino|Prino]] 01:24, 11 February 2011 (UTC)

==== Completely generic code using the pre-processor ====

```pli
%swap: proc(x,y);
dcl (x, y) char;

x = trim(x); /* Just for neatness sake */
y = trim(y);

ans('begin;                                                ') skip;
ans('  dcl c  char       (1);                              ') skip;
ans('  dcl sx char       (1) based(px);                    ') skip;
ans('  dcl sy char       (1) based(py);                    ') skip;
ans('  dcl i  fixed bin (31);                              ') skip;
ans('  dcl px ptr            init (addr(' || x || '));     ') skip;
ans('  dcl py ptr            init (addr(' || y || '));     ') skip;
ans('  do i = 1 to min(stg(' || x || '), stg(' || y || '));') skip;
ans('    c  = sx;                                          ') skip;
ans('    sx = sy;                                          ') skip;
ans('    sy = c;                                           ') skip;
ans('    px = px + 1;                                      ') skip;
ans('    py = py + 1;                                      ') skip;
ans('  end;                                                ') skip;
ans('end;                                                  ') skip;
%end swap;
%act swap;

dcl c1 char (10) init ('1234567890');
dcl c2 char (10) init ('ABCDEFGHIJ');
dcl f1 fixed bin (31) init (12345);
dcl f2 fixed bin (31) init (98765);

put data(c1, c2, f1, f2);
swap(c1, c2);
swap(f1, f2);
put data(c1, c2, f1, f2);
f1 = -656877352; /* '5a5a5a5a'x, aka 'QQQQ' */
swapper(c1, f1);
put data(c1,f1);
```


The code generated by 'swap(c1, c2);' looks like


```pli

begin;
  dcl c  char       (1);
  dcl sx char       (1) based(px);
  dcl sy char       (1) based(py);
  dcl i  fixed bin (31);
  dcl px ptr            init (addr(C1));
  dcl py ptr            init (addr(C2));
  do i = 1 to min(stg(C1), stg(C2));
    c  = sx;
    sx = sy;
    sy = c;
    px = px + 1;
    py = py + 1;
  end;
end;
```


and, because declarations in PL/I begin blocks are local to that block, generating several blocks with the same variables will not cause any problems.

The result of compiling, linking and executing the above code:

```txt
C1='1234567890'
C2='ABCDEFGHIJ'
F1=         12345
F2=         98765;
C1='ABCDEFGHIJ'
C2='1234567890'
F1=         98765
F2=         12345;
C1='QQQQEFGHIJ'
F1=   -1044200508;
```


===Or, using "Like"===
The key problem is that a temporary storage area is needed (there alas being no compiler-recognised "swap" statement), and the waystation variable must have the correct type, nor can there be reliance on a suitable "t" variable being available for use. Devising a different Swap for each type of parameter would be tedious and tiresome to use, however one could employ the "generic" facility as above demonstrated, and use the pre-processor to generate a collection of Swap routines by it employing a template and stepping through a list of accommodated types.

Instead, the first example can be generalised via two steps. Firstly, it is possible to declare a variable to be of a type "like" some named variable (otherwise a third parameter naming the type could be supplied), and secondly, placing the in-line code between Begin ... End; means that any declaration is local to within that block only. Further, this bracketing allows a Swap to be invoked via an if-statement, as in <code>If ... then Swap(x,y);</code> - otherwise there would be a mess. Thus:

```pli

%Swap:Procedure(a,b);
   declare (a,b) character; /*These are proper strings of arbitrary length, pre-processor only.*/
   return ('Begin; declare t like '|| a ||'; t='|| a ||';'|| a ||'='|| b ||';'|| b ||'=t; End;');
%End Swap;
```

Whereupon a Swap(this,that); would generate a rather longer text of in-line source code. This and other text expansions caused odd difficulties, because the 1980s compiler replaced the invocation by the expansion and then reformatted the result into lines of 71 characters (not 72) as necessary, and then, since any additional lines were given the same source sequence number as the original line, added 100000 as needed to generate strictly increasing sequence numbers. If many lines overflowed, eventually the sequence field (eight digits) overflowed, and all following source lines thereby acquired the same source sequence number. For this and other reasons, one approach was two-stage compilation: the output from the pre-processor stage could be saved and further compilation cancelled. That file could then be resequenced and fed to the pl/i compiler afresh.

Such a file would have the expansion of Swap(this,that) as follows (but with added layout here):

```pli

Begin;
 declare t like this;
  t = this;
  this = that;
  that = t;
End;
```


There would however be trouble if the type of ''this'' differed from the type of ''that'', and a pl/i compiler may not generate a warning because it handles many type conversions in an assignment without complaint. There are no pre-processor enquiry functions to inspect the types at pre-processor time - if there were, a more accomplished Swap procedure could produce suitable error reports, which can be classed as "warning" or "severe", etc. The "storage" function produces its result at run time, but, each invocation of Swap being compiled would have its actual parameters known as the compiler dealt with the code produced by that invocation of Swap, and so for each invocation, the results of "storage" would be constants - except for items that were allocated at run time.

The bracketing could be via <code>DO; ... END;</code> instead of <code>BEGIN; ... END;</code> but in that case the declared temporary variable would be visible outside its fragment and there could be conflicts, either of differing type for the same name or of multiple declaration. This could be solved by adjusting Swap to generate a different name each time. One could try a prefix (or suffix) to the name of the first parameter (thus generating say <code>SwapTemp_this</code> or similar), but there would still be difficulty if there were multiple swaps involving the same variable. Instead, Swap could count its invocations and generate a name involving that. Temporary variables would then litter the storage area, and they could consume a lot of space. On the other hand, the <code>BEGIN; ... END;</code> arrangement, though typically involving temporary space on the data stack, could have its own constraints. In the 1980s, the IBM mainframe pl/i compiler had a limit of no more than 240 (or so) <code>BEGIN; ... END;</code> blocks, plus procedure blocks, plus a few other items, in any one compilation otherwise there would be a failure "in phase PI". Separate compilation and the linking of pieces introduced its own oddities, as when pieces had been compiled with different compiler options.


## Pop11


Swap is easily done via multiple assignment:


```pop11
(a, b) -> (b, a);
```


Pop11 is dynamically typed, so the code above is "generic".


## PostScript

Works with anything you can put on the operand stack:
```PostScript
exch>
```



## PowerShell

PowerShell allows swapping directly, through tuple assignment:

```powershell
$b, $a = $a, $b
```

But one can also define a function which swaps the values of two references:

```powershell
function swap ([ref] $a, [ref] $b) {
    $a.Value, $b.Value = $b.Value, $a.Value
}
```

When using this function the arguments have to be explicitly given as references:

```powershell
swap ([ref] $a) ([ref] $b)
```




## Prolog



```prolog

swap(A,B,B,A).

?- swap(1,2,X,Y).
X = 2,
Y = 1.

```



## PureBasic

Built in function:

```PureBasic
Swap a, b
```



## Python


Python has support for swapping built in:


```python
a, b = b, a
```


But the task calls for a "generic swap method" to be written, so here it is:


```python
def swap(a, b):
    return b, a
```

Note that tuples are immutable in Python. This function doesn't mutate anything, but simply returns a new pair with the order of the elements switched.


## R


R function arguments are passed by value, not by reference.  You can work around this, however, by using their names and environment:


```R
swap <- function(name1, name2, envir = parent.env(environment()))
{
    temp <- get(name1, pos = envir)
    assign(name1, get(name2, pos = envir), pos = envir)
    assign(name2, temp, pos = envir)
}
```


Usage:

```txt
> x <- 1
> y <- 2
> swap('x', 'y')
> cat(x, y)
2 1
```



## Racket


A swap operation can be easily written as a macro in Racket. The macro will even work as expected in Typed Racket.


```racket

#lang racket/load

(module swap racket
  (provide swap)

  ;; a simple macro to swap two variables
  (define-syntax-rule (swap a b)
    (let ([tmp a])
      (set! a b)
      (set! b tmp))))

;; works fine in a statically typed setting
(module typed typed/racket
  (require 'swap)

  (: x Integer)
  (define x 3)

  (: y Integer)
  (define y 4)

  (swap x y)
  (printf "x is ~a~n" x)
  (printf "y is ~a~n" y))

```



## REBOL


```REBOL
REBOL [
	Title: "Generic Swap"
	URL: http://rosettacode.org/wiki/Generic_swap
	Reference: [http://reboltutorial.com/blog/rebol-words/]
]

swap: func [
	"Swap contents of variables."
	a [word!] b [word!] /local x
][
	x: get a
	set a get b
	set b x
]

answer: 42  ship: "Heart of Gold"
swap 'answer 'ship ; Note quoted variables.
print rejoin ["The answer is " answer ", the ship is " ship "."]
```


```txt
The answer is Heart of Gold, the ship is 42.
```



## Retro



```Retro
swap>
```



## REXX

REXX has no primitive for swapping, but it can easily be performed using a temporary variable.

(This is the slowest of the three versions.)

### using temp


```rexx
a = 'I see you.'
b = -6

_temp_ = a                           /*swap ···     */
     a = b                           /*     A ···   */
     b = _temp_                      /*  and  B     */
```



### using VALUE

This version will work with any values.

```rexx
a = "bull feathers"
b = 10

a=value('b', a)                      /*swap A and B */
```



### using PARSE

If it's known that there are
::* no blanks
::* no null values
::* (maybe) no whitespace (such as tabs)
in the values, the following method can be used:

(This is the fastest of the three versions.)

```rexx
a = -199e-12
b = 12.

parse value  a  b    with    b  a    /*swap A and B */
```

Note that some REXX interpreters handle whitespace differently, some honor whitespace other than blanks,

others don't   (particularly the older versions).


## Ring


```ring

a = 1
b = 2
temp = a
a = b
b = temp
see "a = " + a + nl
see "b = " + b + nl

```



## RLaB

RLaB does not have a built-in function for swapping the content of two variables. However, there is a workaround which comes from the fact that the global variable space $$ contains all the variables ''var1'', ''var2'' and so forth as $$.var1, ...

Let we want to swap the content of two variables, which names are ''a'' and ''b'',
then the following function would do the trick

```RLaB

swap = function(x,y)
{
  if (!exist($$.[x]))
  { return 0; }
  if (!exist($$.[y]))
  { return 0; }
  local (t);
  t = $$.[x];
  $$.[x] = $$.[y];
  $$.[y] = t;
  return 1;
};

>>  a=1
1
>>  b = "fish"
fish
>> swap( "a" , "b" );
>>  a
fish
>>  b
1

```



## Ruby

Ruby has support for swapping built in:


```ruby
a, b = b, a
```


But the task calls for a "generic swap method", so here it is:


```ruby
def swap(a, b)
    return b, a
end
```


This method does not swap the original variables, because Ruby passes parameters by value.
Instead, this method returns simply a new array with the order of the elements switched. The caller may assign the original variables with the return value:


```ruby
x = 42
y = "string"
x, y = swap x, y
puts x  # prints string
puts y  # prints 42
```



## Run BASIC


Run BASIC does not have support for swapping built in:

```runbasic
a = 1
b = 2
'----- swap ----
tmp = a
a   = b
b  = tmp
end
```



## Rust


```rust
fn main() {
    let mut a="Anna".to_owned();
    let mut b="Bob".to_owned();
    std::mem::swap(&mut a, &mut b);
    println!("a={},b={}",a,b);
}
```



## Sather

A possible way that needs the type of the objects to be specified:


```sather
class SWAP{T} is
  swap(inout a, inout b:T) is
    t ::= a;
    a := b;
    b := t;
  end;
end;
```



```sather
class MAIN is
  main is
    x ::= 10;
    y ::= 20;
    SWAP{INT}::swap(inout x, inout y);
    #OUT + x + ", " + y + "\n";
  end;
end;
```



## Scala

Scala has type parameters and abstract types (not to be confused with abstract data types).
The swap example is about as simple as such things can be, with no variance or high-order
type parameters.

The return type need not be declared in the example below, but it is shown for clarity. However,
as Scala does not pass parameters by reference, it cannot swap values in-place.
To make up for that, it receives two values, and returns a tuple with the values inverted.


```scala
def swap[A,B](a: A, b: B): (B, A) = (b, a)
```



## Scheme


```scheme
; swap elements of a vector
; vector-swap! is not part of r5rs, so we define it
(define (vector-swap! v i j)
(let ((a (vector-ref v i)) (b (vector-ref v j)))
(vector-set! v i b)
(vector-set! v j a)))

(let ((vec (vector 1 2 3 4 5)))
  (vector-swap! vec 0 4)
  vec)
; #(5 2 3 4 1)


; we can swap also in lists
(define (list-swap! v i j)
(let* ((x (list-tail v i))
       (y (list-tail v j))
       (a (car x))
       (b (car y)))
(set-car! x b)
(set-car! y a)))

(let ((lis (list 1 2 3 4 5)))
   (list-swap! lis 0 4)
   lis)
; (5 2 3 4 1)


; using macros (will work on variables, not on vectors or lists)
(define-syntax swap!
(syntax-rules ()
((_ a b)
   (let ((tmp a))
   (set! a b)
   (set! b tmp)))))

; try it
(let ((a 1) (b 2)) (swap! a b) (list a b))
; (2 1)
```



## Seed7


A generic template to generate swap functions is defined with:

```seed7
const proc: generate_swap (in type: aType) is func
  begin

    const proc: swap (inout aType: left, inout aType: right) is func
      local
        var aType: temp is aType.value;
      begin
        temp := left;
        left := right;
        right := temp;
      end func;

  end func;
```

An instance of a swap function can be generated with:

```seed7
generate_swap(integer);
generate_swap(string);
```

A swap function can be called with:

```seed7
swap(a, b);
```



## Sidef


```ruby
func swap(Ref a, Ref b) {
    var tmp = *a;
    *a = *b;
    *b = tmp;
}
```


or:

```ruby
func swap(Ref a, Ref b) {
    (*a, *b) = (*b, *a);
}
```


or:

```ruby
func swap(Ref a, Ref b) {
    [*a, *b] » (b, a);
}
```


The swap functions must be called with variable references.


```ruby
var (x, y) = (1, 2);
swap(\x, \y);
```



## Slate

This must be done with a macro method in Slate, but is in the standard library:

```slate
x@(Syntax LoadVariable traits) swapWith: y@(Syntax LoadVariable traits) &environment: env
"A macro that expands into simple code swapping the values of two variables
in the current scope."
[
  env ifNil: [error: 'Cannot swap variables outside of a method'].
  tmpVar ::= env addVariable.
  {tmpVar store: x variable load.
   x variable store: y variable load.
   y variable store: tmpVar load} parenthesize
].
```


Usage:

```slate
a `swapWith: b
```



## Smalltalk

An OrderedCollection can collect any kind of objects; so this swap implementend extending the OrderedCollection class is really generic.

```smalltalk
OrderedCollection extend [
    swap: a and: b [
	|t|
	t := self at: a.
	self at: a put: (self at: b).
	self at: b put: t
    ]
]
```



## SNOBOL4


The "canonical" version from M. Emmers tutorial:


```snobol4
* SWAP(.V1, .V2) - Exchange the contents of two variables.
*  The variables must be prefixed with the name operator
*  when the function is called.

        DEFINE('SWAP(X,Y)TEMP')              :(SWAP_END)
SWAP    TEMP = $X
        $X = $Y
        $Y = TEMP                            :(RETURN)
SWAP_END
```



## Standard ML

Tuples are immutable in Standard ML. This function doesn't mutate anything, but simply returns a new pair with the order of the elements switched.

```sml
fun swap (x, y) = (y, x)
```

If the arguments are constrained to be reference values, a swap function is simple:

```sml
fun swapref (x, y) =
    let temp = !x in x := !y; y := temp end
```



## Stata

The Mata '''[http://www.stata.com/help.cgi?mf_swap swap]''' function is built-in.


```stata
mata
a=1,2,3
b="ars longa vita brevis"
swap(a, b)
end
```


Notice that swap only works with variables, not with indexed arrays. For instance, swap(a[i],a[j]) does not work. One would instead write a[(i,j)]=a[(j,i)].


## Swift



```swift
func swap<T>
(inout a: T, inout b: T) {
  (a, b) = (b, a)
}
```


'''Note:''' The Swift standard library has already a swap function.


## Tcl


```tcl
proc swap {aName bName} {
    upvar 1 $aName a $bName b
    lassign [list $a $b] b a
}
```


```tcl
proc swap {aName bName} {
    upvar 1 $aName a $bName b
    foreach {b a} [list $a $b] break
}
```


alternatively:


```tcl
proc swap {aName bName} {
    upvar 1 $aName a $bName b
    set a $b[set b $a; list]
}
```



```tcl
set a 1
set b 2
puts "before\ta=$a\tb=$b"
swap a b
puts "after\ta=$a\tb=$b"
```


```txt
before	a=1	b=2
after	a=2	b=1
```


An idiomatic method:


```tcl
set a 1
set b 2
puts "before\ta=$a\tb=$b"
set a $b[set b $a;lindex {}]
puts "after\ta=$a\tb=$b"
```


```txt
before	a=1	b=2
after	a=2	b=1
```



## ThinBASIC

Generic function, swap the content of two variables.

```ThinBASIC
Swap Var1, Var2
```


=={{header|TI-89 BASIC}}==

TI-89 BASIC is dynamically typed, so the genericity is implicit. It has no pass by reference, so we must pass the variable names as strings. It is dynamically scoped, so we must choose hopefully distinct names for the variables.


```ti89b
Define swap(swapvar1, swapvar2) = Prgm
  Local swaptmp
  #swapvar1 → swaptmp
  #swapvar2 → #swapvar1
  swaptmp → #swapvar2
EndPrgm

1 → x
2 → y
swap("x", "y")
x
    2
y
    1
```



## Trith

As with other stack-based languages (e.g. [[Factor]] and [[Joy]]), the solution to this task is a trivial matter of swapping the top two operands on the stack:

```trith
swap>
```



## TXR


TXR Lisp has a <code>swap</code> macro operator. However, an operator just like it can be user-defined (let us call it <code>swp</code>). Moreover, the user-defined version can be just as robust, ensuring once-only evaluation for both expressions.

Swapping can be achieved with <code>pset</code> and <code>rotate</code> also. We won't use these in the following examples.


### =Naive macro=


This allows multiple evaluation of the argument expressions.


```txrlisp
(defmacro swp (left right)
  (with-gensyms (tmp)
    ^(let ((,tmp ,left))
       (set ,left ,right
            ,right ,tmp))))
```



### =Using <code>placelet</code>=


TXR Lisp's <code>placelet</code> macro allows the programmer to bind a lexically scoped alias for a syntactic place. The place can be accessed and stored through this alias. Yet, the place is evaluated only once. With <code>placelet</code> it is easy to write many kinds of place-manipulating macros very simply. We can write a robust swap which evaluates the left and right expressions just once:


```txrlisp
(defmacro swp (left right)
  (with-gensyms (tmp lpl rpl)
    ^(placelet ((,lpl ,left)
                (,rpl ,right))
       (let ((,tmp ,lpl))
         (set ,lpl ,rpl
              ,rpl ,tmp)))))
```



### =Using place expanders=


Finally, the following is closely based on how <code>swap</code> is actually implemented in TXR Lisp's library. This explicitly uses the general mechanism for handling places, on which <code>placelet</code> is based also:


```txrlisp
(defmacro swp (left right :env env)
  (with-gensyms (tmp)
    (with-update-expander (l-getter l-setter) left env
      (with-update-expander (r-getter r-setter) right env
        ^(let ((,tmp (,l-getter)))
           (,l-setter (,r-getter))
           (,r-setter ,tmp))))))
```


<code>with-update-expander</code> is a macro which writes code for accessing and updating a place, and makes that code available as local macros. The result is wrapped around the body of code passed to the macro; the body can access these functions, using a backquote to insert the symbols which refer to them. For instance the macro call <code>(,l-getter)</code> expands to code which accesses the prior value of the <code>left</code> place, and <code>(,r-setter ,tmp)</code> stores the value of the temporary variable into the <code>right</code> place.


## uBasic/4tH

Since uBasic/4tH has a stack (just like [[Forth]]) and it is an integer BASIC only, this is quite trivial. However, making a function or procedure with the same functionality is impossible, because there is no way to pass variables by reference.
<lang>a = 5 : b = 7
Print a,b
Push a,b : a = Pop() : b = Pop()
Print a,b
```



## UNIX Shell

```bash
$ swap() { typeset -n var1=$1 var2=$2; set -- "$var1" "$var2"; var1=$2; var2=$1; }
$ a=1 b=2
$ echo $a $b
1 2
$ swap a b
$ echo $a $b
2 1
$ swap a b
$ echo $a $b
1 2
```


```bash
$ swap() { local var1=$1 var2=$2; set -- "${!var1}" "${!var2}"; declare -g "$var1"="$2" "$var2"="$1"; }
$ a=1 b=2
$ echo $a $b
1 2
$ swap a b
$ echo $a $b
2 1
$ swap a b
$ echo $a $b
1 2
```



## Ursala

Most functions are polymorphic without any special provision to that effect.
Swapping a pair is a very inexpensive operation because no actual copying or overwriting
is performed.

```Ursala
pmgs("x","y") = ("y","x")    # the pattern matching way

ugs = ~&rlX                  # the idiosyncratic Ursala way

#cast %sWL

test = <pmgs ('a','b'),ugs ('x','y')>
```


```txt
<('b','a'),('y','x')>
```



## V

Using the view to shuffle the stack.


```v
[swap [a b : b a] view].

1 2 swap
= 2 1
'hello' 'hi' swap
```

 ='hi' 'hello'


## VBScript

This works for everything: strings, dates, booleans ... The fact is, with everything being a Variant, it's always generic.


```vb
sub swap( byref x, byref y )
	dim temp
	temp = x
	x = y
	y = temp
end sub
```


Usage:

```vb
dim a
a = "woof"
dim b
b = now()
swap a,b
wscript.echo a
wscript.echo b
```


```vb
5/02/2010 2:35:36 PM
woof
```



## Verbexx



```verbexx
// user-defined swap verb -- parms are passed by alias, not value, so they can be updated:

'<==> [_a] @FN [_b] { _a _b = _b _a } by_alias: ;


// test out swap verb

@VAR a = 12345;
@VAR b = "*****";

@SAY "a=" a "   b=" b;

\b <==> \a;                    // "\" verb prevents evaluation of a and b here,
                               // so they can be passed by alias to <==>
@SAY "a=" a "   b=" b;

a b = b a;                     // swap them back, just using the usual  =  verb

@SAY "a=" a "   b=" b;
```



## Visual Basic


Visual Basic can use the [[#VBScript|VBScript]] example above, with the caveat that it won't work if any <code>DEFtype</code> (except <code>DefVar</code>) has been used. (The default data type is <code>Variant</code>, which can be used as a stand-in for any variable type.)

Also, the sub will fail if one arg is a string containing non-numeric data and the other arg is numeric.


## Visual Basic .NET

Semantically identical to [[#C#|C#]]

```vbnet
Sub Swap(Of T)(ByRef a As T, ByRef b As T)
    Dim temp = a
    a = b
    b = temp
End Sub
```


Usage:

```vbnet
Dim a = 1, b = 2
Swap(a, b)
```


−


## Visual FoxPro

 Since Visual FoxPro is not strongly typed, this will work with any data types.

```vfp

 *!* Swap two variables
 LOCAL a, b
 a = 1
 b = "Hallo"
 ? a, b
 *!* Pass a and b by reference
 Swap(@a, @b)
 ? a, b

PROCEDURE Swap(v1, v2)
LOCAL dum
dum = v1
v1 = v2
v2 = dum
ENDPROC

```

 {{out}}

```txt

 1 Hallo
 Hallo 1

```



## Wart

There's a primitive for modifying bindings.

```wart
(swap! x y)
```


New bindings can be created in parallel.

```wart
let (x y) (list y x)
  ...
```



## XPL0

The name Swap is normally used to call an intrinsic routine that swaps
bytes in an integer. Thus Exch is used here instead. A and B must both be
the same size.


```XPL0
include c:\cxpl\codes;

proc Exch(A, B, S);
char A, B, S;
int  I, T;
for I:= 0 to S-1 do
        [T:= A(I);  A(I):= B(I);  B(I):= T];


real X, Y;
[X:= 3.0;  Y:= 4.0;
Exch(addr X, addr Y, 8);
RlOut(0, X);  RlOut(0, Y);  CrLf(0);
]
```


```txt

    4.00000    3.00000

```



## Yorick

Yorick has a built-in function ''swap'' for exchanging the contents of two variables without requiring a temporary copy.
Example of use:

```txt
> a = 1
> b = "foo"
> swap, a, b
> a
"foo"
> b
1
```


Swapping elements in an array can be accomplished using index lists.
Arbitrary permutations of swaps are also straightforward.
Example:

```txt
> foo = [10,20,30,40,50]
> foo([1,2]) = foo([2,1])
> foo
[20,10,30,40,50]
> foo([3,4,5]) = foo([4,5,3])
> foo
[20,10,40,50,30]
```



## zkl

As a general case, no. Here a fake around for two known classes
(which are the same for ease of example):

```zkl
class C{var v; fcn init(n){v=n}}
var c1=C(1), c2=C(2);
println(c1.v," : ",c2.v);
fcn swap(ca,cb,name){
   tmp:=ca.resove(name);
   ca.setVar(name,cb.resolve(name)); cb.setVar(name,tmp)
}
swap(c1,c2,"v");
println(c1.v," : ",c2.v);
```

```txt

1 : 2
2 : 1

```



