+++
title = "Variadic function"
description = ""
date = 2019-10-18T20:47:42Z
aliases = []
[extra]
id = 2868
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}[[Category:Functions and subroutines]]

;Task:
Create a function which takes in a variable number of arguments and prints each one on its own line.

Also show, if possible in your language, how to call the function on a list of arguments constructed at runtime.


Functions of this type are also known as [[wp:Variadic_function|Variadic Functions]].


;Related task:
*   [[Call a function]]





## Ada


Ada doesn't have variadic functions. But you can mimic the behavior by defining a function with an unconstrained array as its parameter, i.e., an array whose length is determined at run time.


```Ada
with Ada.Strings.Unbounded, Ada.Text_IO;

procedure Variadic is

   subtype U_String is Ada.Strings.Unbounded.Unbounded_String;
   use type U_String;

   function "+"(S: String) return U_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-"(U: U_String) return String
     renames Ada.Strings.Unbounded.To_String;

   type Variadic_Array is array(Positive range <>) of U_String;

   procedure Print_Line(Params: Variadic_Array) is
   begin
      for I in Params'Range loop
         Ada.Text_IO.Put(-Params(I));
         if I < Params'Last then
            Ada.Text_IO.Put(" ");
         end if;
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Line;

begin
   Print_Line((+"Mary", +"had", +"a", +"little", +"lamb.")); -- print five strings
   Print_Line((1 => +"Rosetta Code is cooool!")); -- print one string
end;
```


Output:
```txt
Mary had a little lamb.
Rosetta Code is cooool!
```



## ACL2


```Lisp
(defun print-all-fn (xs)
   (if (endp xs)
       nil
       (prog2$ (cw "~x0~%" (first xs))
               (print-all-fn (rest xs)))))

(defmacro print-all (&rest args)
   `(print-all-fn (quote ,args)))
```



## ActionScript


```actionscript
public function printArgs(... args):void
{
    for (var i:int = 0; i < args.length; i++)
        trace(args[i]);
}
```



## Aime

Printing strings:

```aime
void
f(...)
{
    integer i;

    i = 0;
    while (i < count()) {
	o_text($i);
	o_byte('\n');
	i += 1;
    }
}

integer
main(void)
{
    f("Mary", "had", "a", "little", "lamb");

    return 0;
}
```

Printing data of assorted types:

```aime
void
output_date(date d)
{
    o_form("~%//f2/%//f2/", d.year, d.y_month, d.m_day);
}

void
g(...)
{
    integer i;
    record r;

    r["integer"] = o_integer;
    r["real"] = o_;
    r["text"] = o_text;
    r["date"] = output_date;

    i = 0;
    while (i < count()) {
        r[__type($i)]($i);
        o_byte('\n');
        i += 1;
    }
}

integer
main(void)
{
    g("X.1", 707, .5, date().now);

    return 0;
}
```



## ALGOL 68

Variable arguments of arbitrarily typed values are not permitted in '''ALGOL 68'''.
''However'' a flexible array of tagged types (union) <u>is</u> permitted.  This effectively
allows the passing of strongly typed variable arguments to procedures.

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
main:(
  MODE STRINT = UNION(STRING, INT, PROC(REF FILE)VOID, VOID);

  PROC print strint = (FLEX[]STRINT argv)VOID: (
    FOR i TO UPB argv DO
      CASE argv[i] IN
        (INT i):print(whole(i,-1)),
        (STRING s):print(s),
        (PROC(REF FILE)VOID f):f(stand out),
        (VOID):print(error char)
      ESAC;
      IF i NE UPB argv THEN print((" ")) FI
    OD
  );

 print strint(("Mary","had",1,"little",EMPTY,new line))
)
```

Output:

```txt

Mary had 1 little *

```

Also note that '''empty''' (of type '''void''') can be used to indicate missing or optional arguments.

For another example see [[Average/Simple_moving_average#ALGOL_68|Average/Simple moving average]]. This example is closer  to the keyword arguments found in python.



## AppleScript

{{works with|OS X Yosemite onwards|(10.10+) for the use of NSDictionary with record arguments}}

AppleScript handlers have no internal access to an argument vector, but we can use AppleScript's Patterned Parameters, in the form of lists of arbitrary length for variadic positional parameters, or records for variadic named parameters.


```AppleScript
use framework "Foundation"

-- positionalArgs :: [a] -> String
on positionalArgs(xs)

    -- follow each argument with a line feed
    map(my putStrLn, xs) as string
end positionalArgs

-- namedArgs :: Record -> String
on namedArgs(rec)
    script showKVpair
        on |λ|(k)
            my putStrLn(k & " -> " & keyValue(rec, k))
        end |λ|
    end script

    -- follow each argument name and value with line feed
    map(showKVpair, allKeys(rec)) as string
end namedArgs

-- TEST
on run
    intercalate(linefeed, ¬
        {positionalArgs(["alpha", "beta", "gamma", "delta"]), ¬
            namedArgs({epsilon:27, zeta:48, eta:81, theta:8, iota:1})})

    --> "alpha
    --   beta
    --   gamma
    --   delta
    --
    --   epsilon -> 27
    --   eta -> 81
    --   iota -> 1
    --   zeta -> 48
    --   theta -> 8
    --  "
end run


-- GENERIC FUNCTIONS

-- putStrLn :: a -> String
on putStrLn(a)
    (a as string) & linefeed
end putStrLn

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- allKeys :: Record -> [String]
on allKeys(rec)
    (current application's NSDictionary's dictionaryWithDictionary:rec)'s allKeys() as list
end allKeys

-- keyValue :: Record -> String -> Maybe String
on keyValue(rec, strKey)
    set ca to current application
    set v to (ca's NSDictionary's dictionaryWithDictionary:rec)'s objectForKey:strKey
    if v is not missing value then
        item 1 of ((ca's NSArray's arrayWithObject:v) as list)
    else
        missing value
    end if
end keyValue

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
"alpha
beta
gamma
delta

epsilon -> 27
eta -> 81
iota -> 1
zeta -> 48
theta -> 8
"
```



## Applesoft BASIC

An array of parameters with a count as parameter zero can be used in a subroutine to simulate a variadic function.  The values in the array should probably be cleared when the subroutine returns because the array is a global variable.

```ApplesoftBasic
10 P$(0) = STR$(5)
20 P$(1) = "MARY"
30 P$(2) = "HAD"
40 P$(3) = "A"
50 P$(4) = "LITTLE"
60 P$(5) = "LAMB"
70 GOSUB 90"VARIADIC FUNCTION
80 END
90 FOR I = 1 TO VAL(P$(0)) : ? P$(I) : P$(I) = "" : NEXT I : P$(0) = "" : RETURN
```



## Arturo



```arturo
printAll [args]{
	loop args {
		print &
	}
}

!printAll #("one" "two" "three")
```


{{out}}


```txt
one
two
three
```



## AutoHotkey

{{works with|AutoHotkey_L}}
Writing an asterisk after the final parameter marks the function as variadic, allowing it to receive a variable number of parameters:

```AutoHotkey
printAll(args*) {
  for k,v in args
    t .= v "`n"
  MsgBox, %t%
}
```

This function can be called with any number of arguments:
```AutoHotkey
printAll(4, 3, 5, 6, 4, 3)
printAll(4, 3, 5)
printAll("Rosetta", "Code", "Is", "Awesome!")
```

An array of parameters can be passed to any function by applying the same syntax to a function-call:
```AutoHotkey
args := ["Rosetta", "Code", "Is", "Awesome!"]
printAll(args*)
```



'''AutoHotkey Basic (deprecated):'''

Function arguments can be given default values. Comparison with "" can indicate that an argument was present (and not of value ""). As of version 1.0.48, you can pass more parameters than defined by a function, in which case the parameters are evaluated but discarded. Versions earlier than that produce warnings.

```autohotkey
string = Mary had a little lamb
StringSplit, arg, string, %A_Space%

Function(arg1,arg2,arg3,arg4,arg5)  ;Calls the function with 5 arguments.
Function()  ;Calls the function with no arguments.
return

Function(arg1="",arg2="",arg3="",arg4="",arg5="") {
  Loop,5
    If arg%A_Index% !=
      out .= arg%A_Index% "`n"
  MsgBox,% out ? out:"No non-blank arguments were passed."
}
```



## AWK

AWK allows to call functions with fewer than the defined arguments; the missing one(s) default to "". Comparison with "" can check if the argument was present (and not of value ""). To call a function with more than the defined arguments, this produces a warning.

This f() can accept 0 to 3 arguments.


```awk
function f(a, b, c){
	if (a != "") print a
	if (b != "") print b
	if (c != "") print c
}

BEGIN {
	print "[1 arg]"; f(1)
	print "[2 args]"; f(1, 2)
	print "[3 args]"; f(1, 2, 3)
}
```



```txt
[1 arg]
1
[2 args]
1
2
[3 args]
1
2
3
```


This f() can also accept array elements. This works because any missing array elements default to "", so f() ignores them.


```awk
function f(a, b, c) {
	if (a != "") print a
	if (b != "") print b
	if (c != "") print c
}

BEGIN {
	# Set ary[1] and ary[2] at runtime.
	split("Line 1:Line 2", ary, ":")

	# Pass to f().
	f(ary[1], ary[2], ary[3])
}
```



```txt
Line 1
Line 2
```


Functions like f() can take only a few arguments. To accept more arguments, or to accept "" as an argument, the function must take an array, and the caller must bundle its arguments into an array. This g() accepts 0 or more arguments in an array.


```awk
function g(len, ary,    i) {
	for (i = 1; i <= len; i++) print ary[i];
}

BEGIN {
	c = split("Line 1:Line 2:Next line is empty::Last line", a, ":")
	g(c, a)		# Pass a[1] = "Line 1", a[4] = "", ...

}
```



```txt
Line 1
Line 2
Next line is empty

Last line
```



## BaCon

Variable argument lists are defined with the keyword '''VAR''', and are passed as an indexed array of strings.  The number of elements is specified by a SIZE parameter.  ''Arguments to functions could also simply be indexed or associative arrays or multiple element delimited strings.''


```freebasic
' Variadic functions
OPTION BASE 1
SUB demo (VAR arg$ SIZE argc)
    LOCAL x
    PRINT "Amount of incoming arguments: ", argc
    FOR x = 1 TO argc
        PRINT arg$[x]
    NEXT
END SUB

' No argument
demo(0)
' One argument
demo("abc")
' Three arguments
demo("123", "456", "789")
```


{{out}}

```txt
prompt$ bacon variadic.bac
Converting 'variadic.bac'... done, 16 lines were processed in 0.003 seconds.
Compiling 'variadic.bac'... cc  -c variadic.bac.c
cc -o variadic variadic.bac.o -lbacon -lm
Done, program 'variadic' ready.

prompt$ ./variadic
Amount of incoming arguments: 0
Amount of incoming arguments: 1
abc
Amount of incoming arguments: 3
123
456
789
```



## BASIC

Using variable arguments has not been standardised in BASIC. Therefore there are several different implementations, and many BASIC versions do not have this feature at all.

{{works with|FreeBASIC}}

Variadic functions on FreeBASIC are somewhat similar to those in C.
The parameter list does not pass information about parameter type. If necessary, the type information has to be passed for example in the first parameter.
C calling convention has to be used (with keyword cdecl).

```freebasic
SUB printAll cdecl (count As Integer, ... )
    DIM arg AS Any Ptr
    DIM i   AS Integer

    arg = va_first()
    FOR i = 1 To count
        PRINT va_arg(arg, Double)
        arg = va_next(arg, Double)
    NEXT i
END SUB

printAll 3, 3.1415, 1.4142, 2.71828
```

For some reason, I was not able to get a Strings version of the above to work.
=
## FreeBASIC
=
String version

```freebasic
' version 15-09-2015
' compile with: fbc -s console

Sub printAll_string Cdecl (count As Integer, ... )
    Dim arg As Any Ptr
    Dim i   As Integer

    arg = va_first()
    For i = 1 To count
        Print *Va_Arg(arg, ZString Ptr)
        arg = va_next(arg, ZString Ptr)
    Next i
End Sub

' ------=< MAIN >=------
' direct
printAll_string (5, "Foxtrot", "Romeo", "Echo", "Echo", "BASIC")

' strings
Print : Print
Dim As String a = "one", b = "two", c = "three"
printAll_string (3, a, b, c)

' count is smaller then the number of arguments, no problem
Print : Print
printAll_string (1, a, b, c)

' count is greater then the number of arguments
' after the last valid argument garbage is displayed
' should be avoided, could lead to disaster
Print : Print
printAll_string (4, a, b, c)
Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
only the last example shown
one
two
three
…À�”ÀƒÄ��¶À÷ØÃ¡<"A
```

{{works with|Beta BASIC|3.0}}
{{works with|SAM BASIC}}

Beta BASIC uses keyword DATA to specify variable parameter list.
The parameters are read with READ command just like when reading conventional DATA statements.
The existence of more parameters as well as the type of each parameter can be checked with function ITEM().


```zxbasic
100 DEF PROC printAll DATA
110   DO UNTIL ITEM()=0
120     IF ITEM()=1 THEN
          READ a$
          PRINT a$
130     ELSE
          READ num
          PRINT num
140   LOOP
150 END PROC

200 printAll 3.1415, 1.4142, 2.71828
210 printAll "Mary", "had", "a", "little", "lamb",
```


The code above is for Beta BASIC. There is a small difference between Beta BASIC and SAM BASIC.
On Beta BASIC, the function ITEM has empty parenthesis, on SAM BASIC the parenthesis are not used.

See also: [[Varargs#RapidQ|RapidQ]]


## Batch File


```dos

@echo off

:_main
call:_variadicfunc arg1 "arg 2" arg-3
pause>nul

:_variadicfunc
setlocal
for %%i in (%*) do echo %%~i
exit /b

:: Note: if _variadicfunc was called from cmd.exe with arguments parsed to it, it would only need to contain:
::  @for %%i in (%*) do echo %%i

```

{{out}}

```txt

arg1
arg 2
arg-3

```



## bc

To simulate a variadic function one would define a function which takes an array as parameter and
:a) a second parameter for the actual number of arguments,
:b) uses a special value which marks the end or
:c) the first element in the array specifies the number of arguments.


```bc
/* Version a */
define f(a[], l) {
    auto i
    for (i = 0; i < l; i++) a[i]
}

/* Version b */
define g(a[]) {
    auto i
    for (i = 0; a[i] != -1; i++) a[i]
}

/* Version c */
define h(a[]) {
    auto i

    for (i = 1; i <= a[0]; i++) a[i]
}
```



## C

The ANSI C standard header <tt>stdarg.h</tt> defines macros for low-level access to the parameter stack. It does not know the number or types of these parameters; this is specified by the required initial parameter(s). For example, it could be a simple count, a terminating <tt>NULL</tt>, or a more complicated parameter specification like a printf() format string.

```c
#include <stdio.h>
#include <stdarg.h>

void varstrings(int count, ...)   /* the ellipsis indicates variable arguments */
{
    va_list args;
    va_start(args, count);
    while (count--)
        puts(va_arg(args, const char *));
    va_end(args);
}

varstrings(5, "Mary", "had", "a", "little", "lamb");
```


In C, there is no way to call a variadic function on a list of arguments constructed at runtime.

However, all standard library functions which are variadic have a corresponding version, usually named by prepending the letter "v", that is non-variadic and takes a <tt>va_list</tt> as argument in place of the variadic arguments. For example, <tt>printf</tt> has a corresponding <tt>vprintf</tt> which takes a format string and a <tt>va_list</tt> value as arguments.

Nevertheless, the only way of obtaining a <tt>va_list</tt> is from a variadic function itself. So the "v" functions are only useful for writing a variadic function "wrapper" that performs some processing and then calls on one of the "v" functions with its <tt>va_list</tt>. C still provides no standard way to construct a <tt>va_list</tt> manually at runtime.

The actual implementation of <tt>va_list</tt> is implementation-dependent. If you are developing on a specific platform, you may use platform-specific knowledge to create a <tt>va_list</tt> by hand in a non-portable way. For example, on many platforms, a <tt>va_list</tt> is simply a pointer to a buffer where the arguments are arranged contiguously in memory.


## C++

The C++ varargs are basically the same as in C (therefore you can just take the code from C), but there are some limitations:
* Only PODs (basically, every type you could also write in C) can be passed to varargs
* An important difference is that enums are distinct types with possibly different representation than int in C++, but enumeration values are still converted to <code>int</code> when passed to varargs. Therefore they have to be accessed as <code>int</code> in <code>va_arg</code>.

[[C++11]] in addition allows typesafe variadic arguments through variadic templates. Some compilers, such as gcc, already provide this functionality. The following implements the task with variadic templates:

{{works with|g++|4.3.0}} using option -std=c++0x


```cpp
#include <iostream>

template<typename T>
 void print(T const& t)
{
  std::cout << t;
}

template<typename First, typename ... Rest>
 void print(First const& first, Rest const& ... rest)
{
  std::cout << first;
  print(rest ...);
}

int main()
{
  int i = 10;
  std::string s = "Hello world";
  print("i = ", i, " and s = \"", s, "\"\n");
}
```

As the example shows, variadic templates allow any type to be passed.

=={{header|C sharp|C#}}==


```csharp
using System;

class Program {
    static void Main(string[] args) {
        PrintAll("test", "rosetta code", 123, 5.6);
    }

    static void PrintAll(params object[] varargs) {
        foreach (var i in varargs) {
            Console.WriteLine(i);
        }
    }
}
```


Output:


```txt
test
rosetta code
123
5.6
```



## Clojure



```lisp
(defn foo [& args]
  (doseq [a args]
    (println a)))

(foo :bar :baz :quux)
(apply foo [:bar :baz :quux])
```



## COBOL

{{works with|Micro Focus COBOL V3.2}}
<lang>
       program-id. dsp-str is external.
       data division.
       linkage section.
       1 cnt comp-5 pic 9(4).
       1 str pic x.
       procedure division using by value cnt
           by reference str delimited repeated 1 to 5.
       end program dsp-str.

       program-id. variadic.
       procedure division.
           call "dsp-str" using 4 "The" "quick" "brown" "fox"
           stop run
           .
       end program variadic.

       program-id. dsp-str.
       data division.
       working-storage section.
       1 i comp-5 pic 9(4).
       1 len comp-5 pic 9(4).
       1 wk-string pic x(20).
       linkage section.
       1 cnt comp-5 pic 9(4).
       1 str1 pic x(20).
       1 str2 pic x(20).
       1 str3 pic x(20).
       1 str4 pic x(20).
       1 str5 pic x(20).
       procedure division using cnt str1 str2 str3 str4 str5.
           if cnt < 1 or > 5
               display "Invalid number of parameters"
               stop run
           end-if
           perform varying i from 1 by 1
           until i > cnt
               evaluate i
               when 1
                   unstring str1 delimited low-value
                   into wk-string count in len
               when 2
                   unstring str2 delimited low-value
                   into wk-string count in len
               when 3
                   unstring str3 delimited low-value
                   into wk-string count in len
               when 4
                   unstring str4 delimited low-value
                   into wk-string count in len
               when 5
                   unstring str5 delimited low-value
                   into wk-string count in len
               end-evaluate
               display wk-string (1:len)
           end-perform
           exit program
           .
       end program dsp-str.

```


{{out}}

```txt

The
quick
brown
fox

```



## Common Lisp


The [http://www.lispworks.com/documentation/HyperSpec/Body/03_dac.htm <tt>&rest</tt>] [http://www.lispworks.com/documentation/HyperSpec/Body/03_da.htm lambda list keyword] causes all remaining arguments to be bound to the following variable.


```lisp
(defun example (&rest args)
  (dolist (arg args)
    (print arg)))

(example "Mary" "had" "a" "little" "lamb")

(let ((args '("Mary" "had" "a" "little" "lamb")))
  (apply #'example args))
```



## Coq

To define a variadic function, we build a variadic type:


```coq

Fixpoint Arity (A B: Set) (n: nat): Set := match n with
|O => B
|S n' => A -> (Arity A B n')
end.

```


This function can be used as a type, Arity A B n means <math>\underbrace{A \rightarrow \cdots \rightarrow A}_{\text{n times}} \rightarrow B</math> .
Hence each functions that takes an arbitrary number n of parameter of type A and returns B will have the type Arity A B n (Note that we can parameter n to be a specific value) <br \>
Those functions will be called with their first parameters yielding the number of arguments and the rest being the arguments themselves.


Since Arity is a type, we can compound it with itself as the destination to mean, for instance, "n naturals and 2 * n booleans" like so:

```coq

Definition nat_twobools (n: nat) := Arity nat (Arity bool nat (2*n)) n.

```


There is no equivalent to printf in Coq, because this function has border effects. We will then instead of printing each arguments build a list from it. <br \>
Our function has type Arity A (list A) n and we obviously want to use induction on n.
To build the heritance, we will have the hypothesis of Arity A (list A) n and will have to build a term of Arity A (list A) (S n).
Forall A and B, Arity A B (S n)  is but <math>A \rightarrow \text{Arity A B n}</math>, aka a function that takes A and returns an Arity A B n <br \>
Hence to introduce a new value, we simply create a function that takes one parameter and uses it. <br \>
Finally, for the function to work, we need an accumulator of some sort


```coq

Require Import List.
Fixpoint build_list_aux {A: Set} (acc: list A) (n : nat): Arity A (list A) n := match n with
|O => acc
|S n' => fun (val: A) => build_list_aux (acc ++ (val :: nil)) n'
end.

```


Our function is then just an application of this one:

```coq

Definition build_list {A: Set} := build_list_aux (@nil A).

```


To call it we give it the number of argument and then the parameters we want in the list

```coq

Check build_list 5 1 2 5 90 42.

```

Which gives the result [1; 2; 5; 90; 42]

If instead of a list we wanted a vector (a list which size is now in its own type), then it gets trickier. <br \>
One of the problem is that we will have to prove equality of types such as one of the types t A n and t A (n + 0).
We '''should not''' use lemmas or automatic tactics in this case.
The reason for that is that the proof will be then a part of the type and computation of our function, so when we will try to compute it, Coq will be unable to unfold the opaque proof. Instead we should define our own lemmas and set their opacity to be transparent. Here are the two lemmas we will need:


```coq

Lemma transparent_plus_zero: forall n, n + O = n.
intros n; induction n.
- reflexivity.
- simpl; rewrite IHn; trivial.
Defined.

Lemma transparent_plus_S: forall n m, n + S m = S n + m .
intros n; induction n; intros m.
- reflexivity.
- simpl; f_equal; rewrite IHn; reflexivity.
Defined.

```


Now on to the function. <br \>
Here the accumulator has to be of a fixed size, so we give this size a value, and for each step, we decrement the number of argument and increment this size.
The size of the result is the sum of the size of the accumulator and of the current number of argument. This sum is constant.
Instead of defining a function directly, we will construct it as a proof that will be easier for us to write:


```coq

Require Import Vector.

Definition build_vector_aux {A: Set} (n: nat): forall (size_acc : nat) (acc: t A size_acc), Arity A (t A (size_acc + n)) n.
induction n; intros size_acc acc.
- rewrite transparent_plus_zero; apply acc. (*Just one argument, return the accumulator*)
- intros val. rewrite transparent_plus_S. apply IHn. (*Here we use the induction hypothesis. We just have to build the new accumulator*)
  apply shiftin; [apply val | apply acc]. (*Shiftin adds a term at the end of a vector*)

```


As before, we can now build the full function with a null accumulator:

```coq

Definition build_vector {A: Set} (n: nat) := build_vector_aux n O (@nil A).

```


When we call it:

```coq

Require Import String.
Eval compute in build_vector 4 "Hello" "how" "are" "you".

```

Which gives the vector of members "Hello", "how", "are" and "you" of size 4


## D


```d
import std.stdio, std.algorithm;

void printAll(TyArgs...)(TyArgs args) {
    foreach (el; args)
        el.writeln;
}

// Typesafe variadic function for dynamic array
void showSum1(int[] items...) {
    items.sum.writeln;
}

// Typesafe variadic function for fixed size array
void showSum2(int[4] items...) {
    items[].sum.writeln;
}

void main() {
    printAll(4, 5.6, "Rosetta", "Code", "is", "awesome");
    writeln;
    showSum1(1, 3, 50);
    showSum2(1, 3, 50, 10);
}
```

{{out}}

```txt
4
5.6
Rosetta
Code
is
awesome

54
64
```

Being a system language, in D there are also:
* C-style variadic functions
* D-style variadic functions with type info
* Typesafe variadic function for class objects

See for more info: http://dlang.org/function.html

=={{header|Déjà Vu}}==

Variadic functions in the Déjà Vu standard library generally end with <code>(</code>, <code>[</code> or <code>{</code>. For this purpose, <code>)</code>, <code>]</code> and <code>}</code> are autonyms (that is, they have a global bindings to themselves, so that <code>)</code> is the same as <code>:)</code>).


```dejavu
show-all(:
    while /= ) dup:
        !.
    drop

show-all( :foo "Hello" 42 [ true ] )
```

{{out}}

```txt
:foo
"Hello"
42
[ true ]
```



## Dyalect



```Dyalect
func printAll(args...) {
    for i in args {
        print(i)
    }
}

printAll("test", "rosetta code", 123, 5.6)
```


{{out}}


```txt
test
rosetta code
123
5.6
```



## E


Varargs is mildly unidiomatic in E, as the argument count is dispatched on, and often considered part of the method name.

However, accepting any number of arguments can easily be done, as it is just a particular case of the basic mechanism for dynamic message handling:


```e
def example {
    match [`run`, args] {
        for x in args {
            println(x)
        }
    }
}

example("Mary", "had", "a", "little", "lamb")

E.call(example, "run", ["Mary", "had", "a", "little", "lamb"])
```



For comparison, a plain method doing the same thing for exactly two arguments would be like this:


```e
def non_example {
    to run(x, y) {
        println(x)
        println(y)
    }
}
```


or, written using the function syntax,


```e
def non_example(x, y) {
    println(x)
    println(y)
}
```



## Egel

Egel performs almost all of its work with pattern-matching anonymous functions which may match against any number of arguments. The following combinator discriminates between 2, 1, or 0 arguments; more elaborate examples are straightforward.


```Egel

[ X Y -> "two" | X -> "one" | -> "zero" ]

```



## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

extension variadicOp
{
    printAll(params object[] list)
    {
        for(int i := 0, i < list.Length, i+=1)
        {
            self.printLine(list[i])
        }
    }
}

public program()
{
    console.printAll("test", "rosetta code", 123, 5.6r)
}
```

{{out}}

```txt

test
rosetta code
123
5.6

```



## Elixir

Elixir doesn't have the feature of the variable number of arguments.
However, it is possible to process as the list if putting in an argument in [].

```elixir
defmodule RC do
  def print_each( arguments ) do
    Enum.each(arguments, fn x -> IO.inspect x end)
  end
end

RC.print_each([1,2,3])
RC.print_each(["Mary", "had", "a", "little", "lamb"])
```


{{out}}

```txt

1
2
3
"Mary"
"had"
"a"
"little"
"lamb"

```



## Emacs Lisp


An <code>&rest</code> in the formal parameters gives all further arguments in a list, which the code can then act on in usual list ways.  Fixed arguments can precede the <code>&rest</code> if desired.


```Lisp
(defun my-print-args (&rest arg-list)
  (message "there are %d argument(s)" (length arg-list))
  (dolist (arg arg-list)
    (message "arg is %S" arg)))

(my-print-args 1 2 3)
```


A function can be called with a list of arguments (and optionally fixed arguments too) with <code>apply</code>, similar to most Lisp variants.


```Lisp
(let ((arg-list '("some thing %d %d %d" 1 2 3)))
  (apply 'message arg-list))
```



## Erlang

Variable amount of anything (like arguments): use a list.

```Erlang

print_each( Arguments ) -> [io:fwrite( "~p~n", [X]) || X <- Arguments].

```



## Euphoria


```euphoria
procedure print_args(sequence args)
    for i = 1 to length(args) do
        puts(1,args[i])
        puts(1,' ')
    end for
end procedure

print_args({"Mary", "had", "a", "little", "lamb"})
```



## Euler Math Toolbox



```Euler Math Toolbox

>function allargs () ...
$  loop 1 to argn();
$    args(#),
$  end
$endfunction
>allargs(1,3,"Test",1:2)
 1
 3
 Test
 [ 1  2 ]
>function args test (x) := {x,x^2,x^3}
>allargs(test(4))
 4
 16
 64

```



## Factor

Variadic functions can be created by making a word which accepts a number specifying how many data stack items to operate on.

```factor
MACRO: variadic-print ( n -- quot ) [ print ] n*quot ;
```

An interactive demonstration in the listener:

```factor
IN: scratchpad "apple" "banana" "cucumber"

--- Data stack:
"apple"
"banana"
"cucumber"

IN: scratchpad 2 variadic-print
cucumber
banana

--- Data stack:
"apple"
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Variadic_function this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

Words taking variable numbers of arguments may be written by specifying the number of parameters to operate upon as the top parameter. There are two standard words which operate this way: PICK and ROLL.


```forth
: sum ( x_1 ... x_n n -- sum ) 1 ?do + loop ;
4 3 2 1  4 sum .   \ 10
```


Alternatively, you can operate upon the entire parameter stack for debugging by using the word DEPTH, which returns the number of items currently on the stack.


```forth
: .stack ( -- ) depth 0 ?do i pick . loop ;
```



## Fortran

{{works with|Fortran|95 and later}}
Fortran has no ''varargs'' for subroutines and functions, but has <code>optional</code> arguments and ''varargs'' functions can be programmed passing an array as argument. Moreover you can program ''elemental'' functions or subroutines, i.e. function acting on a single element but which can be used automatically over a vector (but there are limits to respect in order to make it possible, e.g. it is not possible to use <code>print</code>)

The following code shows how an optional vector argument can be used to pass a variable number of argument to a subroutine.


```fortran
program varargs

  integer, dimension(:), allocatable :: va
  integer :: i

  ! using an array (vector) static
  call v_func()
  call v_func( (/ 100 /) )
  call v_func( (/ 90, 20, 30 /) )

  ! dynamically creating an array of 5 elements
  allocate(va(5))
  va = (/ (i,i=1,5) /)
  call v_func(va)
  deallocate(va)

contains

  subroutine v_func(arglist)
    integer, dimension(:), intent(in), optional :: arglist

    integer :: i

    if ( present(arglist) ) then
       do i = lbound(arglist, 1), ubound(arglist, 1)
          print *, arglist(i)
       end do
    else
       print *, "no argument at all"
    end if
  end subroutine v_func

end program varargs
```



## Free Pascal

Note, strictly speaking the routine <tt>writeLines</tt> has exactly ''one'' parameter.

```pascal
program variadicRoutinesDemo(input, output, stdErr);
{$mode objFPC}

// array of const is only supported in $mode objFPC or $mode Delphi
procedure writeLines(const arguments: array of const);
var
	argument: TVarRec;
begin
	// inside the body `array of const` is equivalent to `array of TVarRec`
	for argument in arguments do
	begin
		with argument do
		begin
			case vType of
				vtInteger:
				begin
					writeLn(vInteger);
				end;
				vtBoolean:
				begin
					writeLn(vBoolean);
				end;
				vtChar:
				begin
					writeLn(vChar);
				end;
				vtAnsiString:
				begin
					writeLn(ansiString(vAnsiString));
				end;
				// and so on
			end;
		end;
	end;
end;

begin
	writeLines([42, 'is', true, #33]);
end.
```

{{out}}

```text
42
is
TRUE
!
```



## Go

A variadic function in Go has a <code>...</code> prefix on the type of the final parameter.  [https://golang.org/ref/spec#Function_types (spec, Function types)]


```go
func printAll(things ... string) {
  // it's as if you declared "things" as a []string, containing all the arguments
  for _, x := range things {
    fmt.Println(x)
  }
}
```


If you wish to supply an argument list to a variadic function at runtime, you can do this by adding a <code>...</code> ''after'' a slice argument:

```go
args := []string{"foo", "bar"}
printAll(args...)
```



## Golo


```golo
#!/usr/bin/env golosh
----
This module demonstrates variadic functions.
----
module Variadic

import gololang.Functions

----
Varargs have the three dots after them just like Java.
----
function varargsFunc = |args...| {
  foreach arg in args {
    println(arg)
  }
}

function main = |args| {

  varargsFunc(1, 2, 3, 4, 5, "against", "one")

  # to call a variadic function with an array we use the unary function
  unary(^varargsFunc)(args)
}
```



## Groovy


```groovy
def printAll( Object[] args) { args.each{ arg -> println arg } }

printAll(1, 2, "three", ["3", "4"])
```


Sample output:

```txt
1
2
three
[3, 4]
```



## Haskell

You can use some fancy recursive type-class instancing to make a function that takes an unlimited number of arguments. This is how, for example, printf works in Haskell.

```haskell
class PrintAllType t where
    process :: [String] -> t

instance PrintAllType (IO a) where
    process args = do mapM_ putStrLn args
                      return undefined

instance (Show a, PrintAllType r) => PrintAllType (a -> r) where
    process args = \a -> process (args ++ [show a])

printAll :: (PrintAllType t) => t
printAll = process []

main :: IO ()
main = do printAll 5 "Mary" "had" "a" "little" "lamb"
          printAll 4 3 5
          printAll "Rosetta" "Code" "Is" "Awesome!"
```

So here we created a type class specially for the use of this variable-argument function. The type class specifies a function, which takes as an argument some kind of accumulated state of the arguments so far, and returns the type of the type class. Here I chose to accumulate a list of the string representations of each of the arguments; this is not the only way to do it; for example, you could choose to print them directly and just accumulate the IO monad.

We need two kinds of instances of this type class. There is the "base case" instance, which has the type that can be thought of as the "return type" of the vararg function. It describes what to do when we are "done" with our arguments. Here we just take the accumulated list of strings and print them, one per line.
We actually wanted to use "IO ()" instead of "IO a"; but since you can't instance just a specialization like "IO ()", we used "IO a" but return "undefined" to make sure nobody uses it. Or we can use GADTs pragma and constraint in instance like this :

```haskell
{-# LANGUAGE GADTs #-}
...

instance a ~ () => PrintAllType (IO a) where
    process args = do mapM_ putStrLn args

...
```


You can have multiple base case instances; for example, you might want an instances that returns the result as a string instead of printing it. This is how "printf" in Haskell can either print to stdout or print to string (like sprintf in other languages), depending on the type of its context.

The other kind of instance is the "recursive case". It describes what happens when you come across an argument. Here we simply append its string representation to the end of our previous "accumulated state", and then pass that state onto the next iteration. Make sure to specify the requirements of the types of the arguments; here I just required that each argument be an instance of Show (so you can use "show" to get the string representation), but it might be different for you.

=={{header|Icon}} and {{header|Unicon}}==
varargs.icn


```icon
procedure main ()
  varargs("some", "extra", "args")
  write()
  varargs ! ["a","b","c","d"]
end

procedure varargs(args[])
  every write(!args)
end
```


Using it

```txt
|icon varargs.icn
some
extra
args

a
b
c
d
```



## Io


```io
printAll := method(call message arguments foreach(println))
```



## J


J's data is arbitrary length lists.  So all functions implicitly support variable length argument lists unless their definitions specifically reject them.

For example:


```J
   A=:2
   B=:3
   C=:5
   sum=:+/
   sum 1,A,B,4,C
15
```


That said, J expects that members of lists all use the same kind of machine representation.  If you want both character literals and numbers for arguments, or if you want arrays with different dimensions, each argument must be put into a box, and the function is responsible for dealing with the packing material.


```J
   commaAnd=: [: ; (<' and ') _2} ::] 1 }.&, (<', ') ,. ":each
   commaAnd 'dog';A;B;'cat';C
dog, 2, 3, cat and 5
```



## Java

{{works with|Java|1.5+}}
Using <tt>...</tt> after the type of argument will take in any number of arguments and put them all in one array of the given type with the given name.

```java5
public static void printAll(Object... things){
   // "things" is an Object[]
   for(Object i:things){
      System.out.println(i);
   }
}
```

This function can be called with any number of arguments:

```java5
printAll(4, 3, 5, 6, 4, 3);
printAll(4, 3, 5);
printAll("Rosetta", "Code", "Is", "Awesome!");
```


Or with an array directly (the array must have the appropriate array type; i.e. if it is <tt>String...</tt>, then you need to pass a <tt>String[]</tt>):

```java5
Object[] args = {"Rosetta", "Code", "Is", "Awesome!"};
printAll(args);
```


But not with both (in this case the array is considered as just one of two arguments, and not expanded):

```java5
Object[] args = {"Rosetta", "Code", "Is", "Awesome,"};
printAll(args, "Dude!");//does not print "Rosetta Code Is Awesome, Dude!"
//instead prints the type and hashcode for args followed by "Dude!"
```


In some rare cases, you may want to pass an array as just a single argument, but doing it directly would expand it to be the entire argument. In this case, you need to cast the array to <tt>Object</tt> (all arrays are objects) so the compiler doesn't know it's an array anymore.

```java5
printAll((Object)args);
```



## JavaScript


### ES5

The [https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Functions/arguments <code>arguments</code>] special variable, when used inside a function, contains an array of all the arguments passed to that function.

```javascript
function printAll() {
  for (var i=0; i<arguments.length; i++)
    print(arguments[i])
}
printAll(4, 3, 5, 6, 4, 3);
printAll(4, 3, 5);
printAll("Rosetta", "Code", "Is", "Awesome!");
```

The <code><var>function</var>.arguments</code> property is equivalent to the <code>arguments</code> variable above, but is deprecated.

You can use the <tt>apply</tt> method of a function to apply it to a list of arguments:

```javascript
args = ["Rosetta", "Code", "Is", "Awesome!"]
printAll.apply(null, args)
```


===ECMAScript 2015 (ES6) variants===
The newest version of ECMAScript added fat arrow function expression syntax, rest arguments and the spread operator. These make writing something like this easy. Of course, a better version might use Array.prototype.map, but here we have a variant that works on variadic arguments:

```javascript
let
  fix = // Variant of the applicative order Y combinator
    f => (f => f(f))(g => f((...a) => g(g)(...a))),
  forAll =
    f =>
      fix(
        z => (a,...b) => (
          (a === void 0)
          ||(f(a), z(...b)))),
  printAll = forAll(print);

printAll(0,1,2,3,4,5);
printAll(6,7,8);
(f => a => f(...a))(printAll)([9,10,11,12,13,14]);
//  0
//  1
//  2
//  3
//  4
//  5
//  6
//  7
//  8
//  9
//  10
//  11
//  12
//  13
//  14

```


Or, less ambitiously:


```javascript
(() => {
    'use strict';

    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);

    // printAll [any] -> String
    const printAll = (...a) => a.map(show)
        .join('\n');

    return printAll(1, 2, 3, 2 + 2, "five", 6);
})();
```


{{Out}}

```txt
1
2
3
4
"five"
6
```



## jq


jq does not support variadic functions, but all versions of jq allow JSON arrays and objects to be used as arguments and as
inputs of functions, and thus variadic functions can easily be simulated.  In addition, as described in the next subsection, recent releases of jq support variadic function names.

The first task requirement can in effect be accomplished using a 0-arity function defined as follows:

```jq
def demo: .[];
```


The parameters would be presented to <code>demo</code> in the form of an array. For example, given an array, args, constructed at runtime, the second task requirement can be accomplished by calling:

```jq
args | demo
```

For example:

```jq
["cheese"] + [3.14] + [[range(0;3)]] | demo
```

produces:

```sh
"cheese"
3.14
[0,1,2]
```


'''Variadic Function Names''':

In this subsection, the notation f/n will be used to refer to a function named f with arity n. For example, recurse/1 is a builtin function that requires one argument.

In recent releases of jq (after version 1.4), function names are variadic in the sense that, if f is a function name, then f/n can be defined for multiple values of n. However, jq does not support the programmatic construction of function calls, and if a function is called with an undefined name/arity combination, then an error will be raised.

```jq

# arity-0:
def f: "I have no arguments";

# arity-1:
def f(a1): a1;

# arity-1:
def f(a1;a2): a1,a2;

def f(a1;a2;a3): a1,a2,a3;

# Example:
f, f(1), f(2;3), f(4;5;6)
```

produces:

```sh
1
2
3
4
5
6
```





## Julia

Putting <code>...</code> after the last argument in a function definition makes it variadic (any number of arguments are passed as a tuple):

```julia

julia> print_each(X...) = for x in X; println(x); end

julia> print_each(1, "hello", 23.4)
1
hello
23.4

```

Conversely, when <code>...</code> is appended to an array (or other iterable object) passed to the function, the array is converted to a sequence of arguments:

```julia

julia> args = [ "first", (1,2,17), "last" ]
3-element Array{Any,1}:
 "first"
 (1,2,17)
 "last

julia> print_each(args...)
first
(1,2,17)
last

```



## Kotlin


```scala
// version 1.1

fun variadic(vararg va: String) {
    for (v in va) println(v)
}

fun main(args: Array<String>) {
    variadic("First", "Second", "Third")
    println("\nEnter four strings for the function to print:")
    val va = Array(4) { "" }
    for (i in 1..4) {
        print("String $i = ")
        va[i - 1] = readLine()!!
    }
    println()
    variadic(*va)
}
```

Sample input/output:
{{out}}

```txt

First
Second
Third

Enter four strings for the function to print:
String 1 = Animal
String 2 = Vegetable
String 3 = Mineral
String 4 = Whatever

Animal
Vegetable
Mineral
Whatever

```



## Lasso

A Lasso method parameter name can prefixed by "..." to specify a variable number of parameters, which are made available as a staticarray. If no name is specified, the staticarray will be named "rest".

```lasso
define printArgs(...items) => stdoutnl(#items)
define printEachArg(...) => with i in #rest do stdoutnl(#i)

printArgs('a', 2, (:3))
printEachArg('a', 2, (:3))
```

To expand an existing list, pass it to the method using invocation syntax.

```lasso
local(args = (:"Rosetta", "Code", "Is", "Awesome!"))
printEachArg(:#args)
```

Output:

```lasso
staticarray(a, 2, staticarray(3))
a
2
staticarray(3)
Rosetta
Code
Is
Awesome!
```



## Logo

{{works with|UCB Logo}}
UCB Logo allows four classes of arguments (in order):
# 0 or more required inputs (colon prefixed words)
# 0 or more optional inputs (two member lists: colon prefixed word with default value)
# an optional "rest" input (a list containing a colon prefixed word, set to the list of remaining arguments)
# ...with an optional default arity (a number)

```logo
to varargs [:args]
 foreach :args [print ?]
end

(varargs "Mary "had "a "little "lamb)
apply "varargs [Mary had a little lamb]
```



## Lua


The generic syntax for defining a variadic function is appending an ellipsis to the list of arguments:


```lua
function varar(...)
  for i, v in ipairs{...} do print(v) end
end
```


It is then used like so:


```lua
varar(1, "bla", 5, "end");
```

{{out}}

```txt
1
bla
5
end
```


When used with runtime arrays, the unpack function must be called on the array, otherwise the array itself will be used as the only argument:


```lua
local runtime_array = {1, "bla", 5, "end"};

varar(unpack(runtime_array));
```



## M2000 Interpreter

Each function has own stack of values. We can Read arguments from there, using a Read statement or we can check stack or do anything to it. This stack erased when function exit.
We can use parenthesis in the name or not. When we use parentesis we can put some arguments, but interpreter make a Read statement with these arguments.

Function Variadic() {...}  is the same as Function Variadic {...} and Function Variadic () {...}.

Function Abc(x, y) {...} is the same as Function Abc {Read x, y : ... }

We can use Read at some point after some statement executed in Abc function. We can check the stack before the Read to find the type of values in function's stack. This stack isn't the return or process stack.

Module's have stack too, but calling a module from module pass the same stack. This hold if we call function using Call statement.


```M2000 Interpreter

Module CheckIt {
      \\ Works for numbers and strings (letters in M2000)
      Function Variadic {
            \\ print a letter for each type in function stack
            Print Envelope$()
            \\Check types using Match
            Print Match("NNSNNS")
            =stack.size
            While not Empty {
                  if islet then {print letter$} else print number
            }
      }
      M=Variadic(1,2,"Hello",3,4,"Bye")
      Print M
      \\ K is a poiner to Array
      K=(1,2,"Hello 2",3,4,"Bye 2")
      \\ !K pass all items to function's stack
      M=Variadic(!K)
}
Checkit


Module CheckIt2 {
      Function Variadic {
            \\ [] return a pointer to stack, and leave a new stack as function's stack
            a=[]
            \\ a is a pointer to stack
            \\ objects just leave a space, and cursor move to next column (spread on lines)
            Print a
      }
      M=Variadic(1,2,"Hello",3,4,"Bye")
      Print M
      \\ K is a poiner to Array
      K=(1,2,"Hello 2",3,4,"Bye 2")
      \\ !K pass all items to function stack
      M=Variadic(!K)
}
Checkit2

```



## M4


```M4
define(`showN',
   `ifelse($1,0,`',`$2
$0(decr($1),shift(shift($@)))')')dnl
define(`showargs',`showN($#,$@)')dnl
dnl
showargs(a,b,c)
dnl
define(`x',`1,2')
define(`y',`,3,4,5')
showargs(x`'y)
```


Output (with tracing):

```txt

m4trace: -1- showargs(a, b, c)
a
b
c



m4trace: -1- showargs(1, 2, 3, 4, 5)
1
2
3
4
5

```



## Mathematica

Function that takes 0 to infinite arguments and prints the arguments:

```Mathematica
ShowMultiArg[x___] := Do[Print[i], {i, {x}}]
```

Example:

```Mathematica
ShowMultiArg[]
ShowMultiArg[a, b, c]
ShowMultiArg[5, 3, 1]
```

gives back:

```Mathematica
[nothing]

a
b
c

5
3
1
```

In general Mathematica supports patterns in functions, mostly represented by the blanks and sequences: _, __ and ___ . With those you can create functions with variable type and number of arguments.


## MATLAB

In MATLAB, the keyword "varargin" in the argument list of a function denotes that function as a variadic function. This keyword must come last in the list of arguments. "varargin" is actually a cell-array that assigns a comma separated list of input arguments as elements in the list. You can access each of these elements like you would any normal cell array.


```MATLAB
function variadicFunction(varargin)

    for i = (1:numel(varargin))
        disp(varargin{i});
    end

end
```


Sample Usage:

```MATLAB>>
 variadicFunction(1,2,3,4,'cat')
     1

     2

     3

     4

cat
```



## Maxima


```maxima
show([L]) := block([n], n: length(L), for i from 1 thru n do disp(L[i]))$

show(1, 2, 3, 4);

apply(show, [1, 2, 3, 4]);

/* Actually, the built-in function "disp" is already what we want */
disp(1, 2, 3, 4);

apply(disp, [1, 2, 3, 4]);
```



## Metafont


Variable number of arguments to a macro can be done using the <tt>text</tt> keyword identifying the kind of argument to the macro. In this way, each argument can be of any kind (here, as example, I show all the primitive types that Metafont knows)


```metafont
ddef print_arg(text t) =
for x = t:
  if unknown x: message "unknown value"
  elseif numeric x: message decimal x
  elseif string x: message x
  elseif path x: message "a path"
  elseif pair x: message decimal (xpart(x)) & ", " & decimal (ypart(x))
  elseif boolean x: if x: message "true!" else: message "false!" fi
  elseif pen x: message "a pen"
  elseif picture x: message "a picture"
  elseif transform x: message "a transform" fi; endfor enddef;

print_arg("hello", x, 12, fullcircle, currentpicture, down, identity, false, pencircle);
end
```


=={{header|Modula-3}}==
Modula-3 provides the built ins <tt>FIRST</tt> and <tt>LAST</tt>, which can be used with <tt>FOR</tt> loops to cycle over all elements of an array.  This, combined with open arrays allows Modula-3 to simulate variadic functions.

```modula3
MODULE Varargs EXPORTS Main;

IMPORT IO;

VAR strings := ARRAY [1..5] OF TEXT {"foo", "bar", "baz", "quux", "zeepf"};

PROCEDURE Variable(VAR arr: ARRAY OF TEXT) =
  BEGIN
    FOR i := FIRST(arr) TO LAST(arr) DO
      IO.Put(arr[i] & "\n");
    END;
  END Variable;

BEGIN
  Variable(strings);
END Varargs.
```

Output:

```txt

foo
bar
baz
quux
zeepf

```

Things get more complicated if you want to mix types:

```modula3
MODULE Varargs EXPORTS Main;

IMPORT IO, Fmt;

VAR
  strings := NEW(REF TEXT);
  ints := NEW(REF INTEGER);
  reals := NEW(REF REAL);
  refarr := ARRAY [1..3] OF REFANY {strings, ints, reals};

PROCEDURE Variable(VAR arr: ARRAY OF REFANY) =
  BEGIN
    FOR i := FIRST(arr) TO LAST(arr) DO
      TYPECASE arr[i] OF
      | REF TEXT(n) => IO.Put(n^ & "\n");
      | REF INTEGER(n) => IO.Put(Fmt.Int(n^) & "\n");
      | REF REAL(n) => IO.Put(Fmt.Real(n^) & "\n");
      ELSE (* skip *)
      END;
    END;
  END Variable;

BEGIN
  strings^ := "Rosetta"; ints^ := 1; reals^ := 3.1415;
  Variable(refarr);
END Varargs.
```

Output:

```txt

Rosetta
1
3.1415

```



## Nemerle

{{trans|C#}}
Like C#, Nemerle uses the <tt>params</tt> keyword to specify that arguments are collected into an array.

```Nemerle
using System;
using System.Console;

module Variadic
{
    PrintAll (params args : array[object]) : void
    {
        foreach (arg in args) WriteLine(arg);
    }

    Main() : void
    {
        PrintAll("test", "rosetta code", 123, 5.6, DateTime.Now);
    }
}
```



## Nim


```nim
proc print(xs: varargs[string, `$`]) =
  for x in xs:
    echo x
```

The function can be called with any number of arguments and the argument list can be constructed at runtime:

```nim
print(12, "Rosetta", "Code", 15.54321)

print 12, "Rosetta", "Code", 15.54321, "is", "awesome!"

let args = @["12", "Rosetta", "Code", "15.54321"]
print(args)
```


=={{header|Objective-C}}==
Objective-C uses the same varargs functionality as C. Like C, it has no way of knowing the number or types of the arguments. When the arguments are all objects, the convention is that, if the number of arguments is undetermined, then the list must be "terminated" with <code>nil</code>. Functions that follow this convention include the constructors of data structures that take an undetermined number of elements, like <code>[NSArray arrayWithObjects:...]</code>.


```objc
#include <stdarg.h>

void logObjects(id firstObject, ...) // <-- there is always at least one arg, "nil", so this is valid, even for "empty" list
{
  va_list args;
  va_start(args, firstObject);
  id obj;
  for (obj = firstObject; obj != nil; obj = va_arg(args, id))
    NSLog(@"%@", obj);
  va_end(args);
}

// This function can be called with any number or type of objects, as long as you terminate it with "nil":
logObjects(@"Rosetta", @"Code", @"Is", @"Awesome!", nil);
logObjects(@4, @3, @"foo", nil);
```



## Oforth


As Oforth uses a data stack, the only way to have a function using a variable number of parameters is to define one of its parameters as the number of parameters to use on the stack.

For instance :

```Oforth
: sumNum(n)  | i | 0 n loop: i [ + ] ;
```


{{out}}

```txt

sumNum(3, 1, 2, 3) println
6
sumNum(2, 1, 4) println
5
sumNum(5, 3, 4, 5, 6, 7) println
25

```



## Oz

This is only possible for methods, not for functions/procedures.

```oz
declare
  class Demo from BaseObject
     meth test(...)=Msg
        {Record.forAll Msg Show}
     end
  end

  D = {New Demo noop}
  Constructed = {List.toTuple test {List.number 1 10 1}}
in
  {D test(1 2 3 4)}
  {D Constructed}
```



## PARI/GP

A variadic function can be coded directly in PARI using the parser code <code>s*</code>.

{{Works with|PARI/GP|2.8+}}

```parigp
f(a[..])=for(i=1,#a,print(a[i]))
```


{{Works with|PARI/GP|2.8.1+}}

```parigp
call(f, v)
```



## Pascal

Standard Pascal does not allow variadic functions.

''See [[#Free Pascal|Free Pascal]] instead.''


## Perl

Functions in Perl 5 don't have argument lists. All arguments are stored in the array <tt>@_</tt> anyway, so there is variable arguments by default.


```perl
sub print_all {
  foreach (@_) {
    print "$_\n";
  }
}
```


This function can be called with any number of arguments:

```perl
print_all(4, 3, 5, 6, 4, 3);
print_all(4, 3, 5);
print_all("Rosetta", "Code", "Is", "Awesome!");
```


Since lists are flattened when placed in a list context, you can just pass an array in as an argument and all its elements will become separate arguments:

```perl
@args = ("Rosetta", "Code", "Is", "Awesome!");
print_all(@args);
```


Introduced '''experimentally''' in 5.20.0, subroutines can have signatures when the feature is turned on:

```perl
use 5.020;
use experimental 'signatures';
```

Perl policy states that all bets are off with experimental features—their behavior is subject to change at any time, and they may even be removed completely (''this feature will most likely stay in, but expect changes in the future that will break any scripts written using it as it stands in 5.20.1'').

Functions can be declared with fixed arity:

```perl
sub print ($x, $y) {
    say $x, "\n", $y;
}
```


But this can easily be converted to a variadic function with a slurpy parameter:

```perl
sub print_many ($first, $second, @rest) {
    say "First: $first\n"
       ."Second: $second\n"
       ."And the rest: "
       . join("\n", @rest);
}
```

It is valid for the @rest array to be empty, so this is also an optional parameter (see [[Optional parameters]]).


## Perl 6

{{works with|Rakudo|#25 "Minneapolis"}}

If a subroutine has no formal parameters but mentions the variables <code>@_</code> or <code>%_</code> in its body, it will accept arbitrary positional or keyword arguments, respectively. You can even use both in the same function.


```perl6
sub foo {
   .say for @_;
   say .key, ': ', .value for %_;
}

foo 1, 2, command => 'buckle my shoe',
    3, 4, order => 'knock at the door';
```


This prints:


```txt
1
2
3
4
command: buckle my shoe
order: knock at the door
```


Perl 6 also supports slurpy arrays and hashes, which are formal parameters that consume extra positional and keyword arguments like <code>@_</code> and <code>%_</code>. You can make a parameter slurpy with the <code>*</code> twigil. This implementation of <code>&foo</code> works just like the last:


```perl6
sub foo (*@positional, *%named) {
   .say for @positional;
   say .key, ': ', .value for %named;
}
```


Unlike in Perl 5, arrays and hashes aren't flattened automatically. Use the <code>|</code> operator to flatten:


```perl6
foo |@ary, |%hsh;
```



## Phix

Copy of [[Variadic_function#Euphoria|Euphoria]]. The argument to print_args could be anything constructed at runtime. You can also specify optional parameters, simply by specifying a default value. Any non-optional arguments must be grouped together at the start.

```Phix
procedure print_args(sequence args)
    for i=1 to length(args) do
        ?args[i]
    end for
end procedure
print_args({"Mary", "had", "a", "little", "lamb"})
```



## PHP

PHP 4 and above supports varargs. You can deal with the argument list using the <tt>func_num_args()</tt>, <tt>func_get_arg()</tt>, and <tt>func_get_args()</tt> functions.

```php
<?php
function printAll() {
  foreach (func_get_args() as $x) // first way
    echo "$x\n";

  $numargs = func_num_args(); // second way
  for ($i = 0; $i < $numargs; $i++)
    echo func_get_arg($i), "\n";
}
printAll(4, 3, 5, 6, 4, 3);
printAll(4, 3, 5);
printAll("Rosetta", "Code", "Is", "Awesome!");
?>
```


You can use the <tt>call_user_func_array</tt> function to apply it to a list of arguments:

```php
<?php
$args = array("Rosetta", "Code", "Is", "Awesome!");
call_user_func_array('printAll', $args);
?>
```


{{works with|PHP|5.6+}}
You can receive variable arguments in a list by having a parameter preceded by <tt>...</tt>:

```php
<?php
function printAll(...$things) {
  foreach ($things as $x)
    echo "$x\n";
}
printAll(4, 3, 5, 6, 4, 3);
printAll(4, 3, 5);
printAll("Rosetta", "Code", "Is", "Awesome!");
?>
```


You can use the same <tt>...</tt> syntax to supply a list of arguments to a function:

```php
<?php
$args = ["Rosetta", "Code", "Is", "Awesome!"];
printAll(...$args);
?>
```



## PL/I


```pli
/* PL/I permits optional arguments, but not an infinitely varying */
/* argument list: */
s: procedure (a, b, c, d);
   declare (a, b, c, d) float optional;
   if ^omitted(a) then put skip list (a);
   if ^omitted(b) then put skip list (b);
   if ^omitted(c) then put skip list (c);
   if ^omitted(d) then put skip list (d);
end s;
```



## PicoLisp

The '@' operator causes a function to accept a variable number of arguments.
These can be accesed with the
'[http://software-lab.de/doc/refA.html#args args]',
'[http://software-lab.de/doc/refN.html#next next]',
'[http://software-lab.de/doc/refA.html#arg arg]' and
'[http://software-lab.de/doc/refR.html#rest rest]' functions.

```PicoLisp
(de varargs @
   (while (args)
      (println (next)) ) )
```

The '@' operator may be used in combination with normal parameters:

```PicoLisp
(de varargs (Arg1 Arg2 . @)
   (println Arg1)
   (println Arg2)
   (while (args)
      (println (next)) ) )
```

It is called like any other function

```PicoLisp
(varargs 'a 123 '(d e f) "hello")
```

also by possibly applying it to a ready-made list

```PicoLisp
(apply varargs '(a 123 (d e f) "hello"))
```

Output in all cases:

```txt
a
123
(d e f)
"hello"
```



## PowerShell


```powershell
function print_all {
    foreach ($x in $args) {
        Write-Host $x
    }
}
```

Normal usage of the function just uses all arguments one after another:

```powershell
print_all 1 2 'foo'
```

In PowerShell v1 there was no elegant way of using an array of objects as arguments to a function which leads to the following idiom:

```powershell
$array = 1,2,'foo'
Invoke-Expression "& print_all $array"
```

PowerShell v2 introduced the splat operator which makes this easier:

{{works with|PowerShell|2}}

```powershell
print_all @array
```



## Prolog

The Prolog standard does not require support for variadic functions,
but there is no need for them in Prolog, because Prolog has first-class support for terms, including lists and terms such as (1,2,3), which are also
known as comma-lists.

For example, the standard predicate ''write/1'' has just one formal
argument, but it will accept any term.  Thus, except for the
additional parentheses, ''write/1'' is like a variadic function that
requires at least one argument:


```txt

?- write( (1) ), nl.
1

?- write( (1,2,3) ), nl.
1,2,3

```


In practice, since the minimum length of a comma-list is 2, Prolog
lists are often used instead of comma-lists to handle situations where
vararg-behavior is wanted.  For example:

```prolog
printAll( List ) :- forall( member(X,List), (write(X), nl)).

```

To handle more esoteric situations, we could define a higher-order predicate to handle terms of arbitrary arity, e.g.

```prolog

execute( Term ) :-
  Term =.. [F | Args],
  forall( member(X,Args), (G =.. [F,X], G, nl) ).

```


```txt

?- execute( write(1,2,3) ).
1
2
3

```


## Python

Putting <tt>*</tt> before an argument will take in any number of arguments and put them all in a tuple with the given name.


```python
def print_all(*things):
    for x in things:
        print x
```


This function can be called with any number of arguments:

```python
print_all(4, 3, 5, 6, 4, 3)
print_all(4, 3, 5)
print_all("Rosetta", "Code", "Is", "Awesome!")
```


You can use the same "*" syntax to apply the function to an existing list of arguments:

```python
args = ["Rosetta", "Code", "Is", "Awesome!"]
print_all(*args)
```



### Keyword arguments

Python also has keyword arguments were you can add arbitrary ''func('''''keyword1=value1, keyword2=value2 ...''''')'' keyword-value pairs when calling a function.
This example shows both keyword arguments and positional arguments. The two calls to the function are equivalent. '''*alist''' spreads the members of the list to create positional arguments, and '''**adict''' does similar for the keyword/value pairs from the dictionary.

```python>>>
 def printargs(*positionalargs, **keywordargs):
	print "POSITIONAL ARGS:\n  " + "\n  ".join(repr(x) for x in positionalargs)
	print "KEYWORD ARGS:\n  " + '\n  '.join(
		"%r = %r" % (k,v) for k,v in keywordargs.iteritems())


>>> printargs(1,'a',1+0j, fee='fi', fo='fum')
POSITIONAL ARGS:
  1
  'a'
  (1+0j)
KEYWORD ARGS:
  'fee' = 'fi'
  'fo' = 'fum'
>>> alist = [1,'a',1+0j]
>>> adict = {'fee':'fi', 'fo':'fum'}
>>> printargs(*alist, **adict)
POSITIONAL ARGS:
  1
  'a'
  (1+0j)
KEYWORD ARGS:
  'fee' = 'fi'
  'fo' = 'fum'
>>>
```


See the Python entry in [[Named_Arguments#Python|Named Arguments]] for a more comprehensive description of Python function parameters and call arguments.


## Qi


Qi doesn't have support for variable argument functions, but we can fake it by using a macro that
puts all arguments into a list.


```qi

(define varargs-func
  A -> (print A))

(define varargs
  [varargs | Args] -> [varargs-func [list | Args]]
  A -> A)

(sugar in varargs 1)

```



## R

This first function, almost completes the task, but the formatting isn't quite as specified.

```rsplus
 printallargs1 <- function(...) list(...)
 printallargs1(1:5, "abc", TRUE)
# [[1]]
# [1] 1 2 3 4 5
#
# [[2]]
# [1] "abc"
#
# [[3]]
# [1] TRUE
```

This function is corrrect, though a little longer.

```rsplus
 printallargs2 <- function(...)
 {
   args <- list(...)
   lapply(args, print)
   invisible()
 }
 printallargs2(1:5, "abc", TRUE)
# [1] 1 2 3 4 5
# [1] "abc"
# [1] TRUE
```

Use do.call to call a function with a list of arguments.

```rsplus
arglist <- list(x=runif(10), trim=0.1, na.rm=TRUE)
do.call(mean, arglist)
```



## Racket


The following defines and uses an any-number-of-arguments variadic
function called "vfun".


```Racket

-> (define (vfun . xs) (for-each displayln xs))
-> (vfun)
-> (vfun 1)
1
-> (vfun 1 2 3 4)
1
2
3
4
-> (apply vfun (range 10 15))
10
11
12
13
14

```



## REALbasic


This subroutine prints it arguments. ParamArrays must be the last argument but may be preceded by any number of normal arguments.


```vb

Sub PrintArgs(ParamArray Args() As String)
  For i As Integer = 0 To Ubound(Args)
    Print(Args(i))
  Next
End Sub

```


Calling the subroutine.

```vb

PrintArgs("Hello", "World!", "Googbye", "World!")

```



## REBOL


REBOL does not have variadic functions, nevertheless, it is easy to define a function taking just one argument, an ARGS block. The ARGS block contents can then be processed one by one:

```REBOL
REBOL [
	Title: "Variadic Arguments"
]

print-all: func [
    args [block!] {the arguments to print}
] [
    foreach arg args [print arg]
]

print-all [rebol works this way]
```



## RapidQ

RapidQ uses special keywords SUBI and FUNCTIONI for procedures and functions with variable number of parameters.
Numeric parameters are accessed from array ParamVal and string parameters from array ParamStr$.

```rapidq
SUBI printAll (...)
    FOR i = 1 TO ParamValCount
	PRINT ParamVal(i)
    NEXT i
    FOR i = 1 TO ParamStrCount
	PRINT ParamStr$(i)
    NEXT i
END SUBI

printAll 4, 3, 5, 6, 4, 3
printAll 4, 3, 5
printAll "Rosetta", "Code", "Is", "Awesome!"
```



## REXX


### simplistic


```rexx
print_all:  procedure              /*   [↓]     is the # of args passed.*/
                           do j=1  for arg()
                           say  arg(j)
                           end   /*j*/
return
```



### annotated


```rexx
print_all:  procedure              /*   [↓]     is the # of args passed.*/
                           do j=1  for arg()
                           say  '[argument'   j"]: "   arg(j)
                           end   /*j*/
return
```



### invocations

The function can be called with any number of arguments (including no arguments and/or omitted arguments),

although some REXX implementations impose a limit and the number of arguments.

```rexx
call print_all .1,5,2,4,-3, 4.7e1, 013.000 ,, 8**2 -3, sign(-66), abs(-71.00), 8 || 9, 'seven numbers are prime, 8th is null'

call print_all  "One ringy-dingy,",
                "two ringy-dingy,",
                "three ringy-dingy...",
                "Hello?  This is Ma Bell.",
                "Have you been misusing your instrument?",
                "(Lily Tomlin routine)"

                         /*  [↑]   example showing multi-line arguments.*/
```



## Ring


```ring

# Project : Variadic function

sum([1,2])
sum([1,2,3])
nums = [1,2,3,4]
sum(nums)

func sum(nums)
       total = 0
       for num = 1 to len(nums)
           total = total + num
       next
       showarray(nums)
       see " " + total + nl

func showarray(vect)
       see "["
       svect = ""
       for n = 1 to len(vect)
           svect = svect + vect[n] + " "
       next
       svect = left(svect, len(svect) - 1)
       see "" + svect + "]"

```

Output:

```txt

[1 2] 3
[1 2 3] 6
[1 2 3 4] 10

```



## Ruby

The * is sometimes referred to as the "splat" in Ruby.

```ruby
def print_all(*things)
  puts things
end
```


This function can be called with any number of arguments:

```ruby
print_all(4, 3, 5, 6, 4, 3)
print_all(4, 3, 5)
print_all("Rosetta", "Code", "Is", "Awesome!")
```


You can use the same "*" syntax to apply the function to an existing list of arguments:

```ruby
args = ["Rosetta", "Code", "Is", "Awesome!"]
print_all(*args)
```



## Scala


```scala
def printAll(args: Any*) = args foreach println
```


Example:


```txt

scala> printAll(1,2,3, "Rosetta", "is cool")
1
2
3
Rosetta
is cool

scala> val list = List(1,2,3, "Rosetta", "is cool")
list: List[Any] = List(1, 2, 3, Rosetta, is cool)

scala> printAll(list: _*)
1
2
3
Rosetta
is cool

```



## Scheme

Putting a dot before the last argument will take in any number of arguments and put them all in a list with the given name.


```scheme
(define (print-all . things)
    (for-each
        (lambda (x) (display x) (newline))
        things))
```


Note that if you define the function anonymously using <tt>lambda</tt>, and you want all the args to be collected in one list (i.e. you have no parameters before the parameter that collects everything), then you can just replace the parentheses altogether with that parameter, as if to say, let this be the argument list:


```scheme
(define print-all
  (lambda things
    (for-each
        (lambda (x) (display x) (newline))
        things)))
```


This function can be called with any number of arguments:

```scheme
(print-all 4 3 5 6 4 3)
(print-all 4 3 5)
(print-all "Rosetta" "Code" "Is" "Awesome!")
```


The <tt>apply</tt> function will apply the function to a list of arguments:

```scheme
(define args '("Rosetta" "Code" "Is" "Awesome!"))
(apply print-all args)
```



## Sidef

A parameter declared with "*", can take any number of arguments of any type.

```ruby
func print_all(*things) {
    things.each { |x| say x };
}
```

This function can be called with any number of arguments:

```ruby
print_all(4, 3, 5, 6, 4, 3);
print_all(4, 3, 5);
print_all("Rosetta", "Code", "Is", "Awesome!");
```

Also, there is "..." which transforms an array into a list of arguments.

```ruby
var args = ["Rosetta", "Code", "Is", "Awesome!"];
print_all(args...);
```



## Slate


Putting an asterisk before a method's input variable header name means it will contain all non-core input variables (those are prefixed with a colon) in an Array.


```slate
define: #printAll -> [| *rest | rest do: [| :arg | inform: arg printString]].

printAll applyTo: #(4 3 5 6 4 3).
printAll applyTo: #('Rosetta' 'Code' 'Is' 'Awesome!').
```


For method definitions and message sends, the same mechanism is employed, but the syntax for passing arguments after the message phrase is special (using commas to append arguments which fill <tt>*rest</tt>):

```slate
_@lobby printAll [| *rest | rest do: [| :arg | inform: arg printString]].
lobby printAll, 4, 3, 5, 6, 4, 3.
lobby printAll, 'Rosetta', 'Code', 'Is', 'Awesome!'.

```



## Swift

Using <tt>...</tt> after the type of argument will take in any number of arguments and put them all in one array of the given type with the given name.

```swift>func printAll<T
(things: T...) {
  // "things" is a [T]
  for i in things {
    print(i)
  }
}
```

This function can be called with any number of arguments:

```swift
printAll(4, 3, 5, 6, 4, 3)
printAll(4, 3, 5)
printAll("Rosetta", "Code", "Is", "Awesome!")
```



## Tcl

{{works with|Tcl|8.5}}
If the last argument is named "args", it collects all the remaining arguments

```tcl
proc print_all {args} {puts [join $args \n]}

print_all 4 3 5 6 4 3
print_all 4 3 5
print_all Rosetta Code Is Awesome!

set things {Rosetta Code Is Awesome!}

print_all $things ;# ==> incorrect: passes a single argument (a list) to print_all
print_all {*}$things ;# ==> correct: passes each element of the list to the procedure
```

The above code will work in all versions of Tcl except for the last line. A version-independent transcription of that (one of many possible) would be:

```Tcl
eval [list print_all] [lrange $things 0 end]
```



## TIScript


In TIScript last parameter of function may have '..' added to its name. On call that parameter will contain an array of rest of arguments passed to that function.


```javascript

function printAll(separator,argv..) {
  if(argv.length)
    stdout.print(argv[0]);
  for (var i=1; i < argv.length; i++)
    stdout.print(separator, argv[i]);
}
printAll(" ", 4, 3, 5, 6, 4, 3);
printAll(",", 4, 3, 5);
printAll("! ","Rosetta", "Code", "Is", "Awesome");
```



## uBasic/4tH

It's not easy to make a variadic function or procedure in uBasic/4tH, but it is possible with a little effort, provided the stack is used. However, sometimes it is required to reverse the order of the values by loading them into the array, from high memory to low memory. Strings may require even more effort, but the built-in hashing helps.
<lang>Push _Mary, _had, _a, _little, _lamb   ' Push the hashes
Proc _PrintStrings (5)                 ' Print the string

Push 1, 4, 5, 19, 12, 3                ' Push the numbers
Print "Maximum is: ";FUNC(_Max(6))     ' Call the function

End


_PrintStrings Param(1)                 ' Print a variadic number of strings
  Local(1)

  For b@ = a@-1 To 0 Step -1           ' Reverse the hashes, load in array
    @(b@) = Pop()
  Next

  For b@ = 0 To a@-1                   ' Now call the appropriate subroutines
    Proc @(b@)
  Until b@ = a@-1
    Print " ";                         ' Print a space
  Next                                 ' unless it is the last word

  Print                                ' Terminate the string
Return


_Max Param(1)                          ' Calculate the maximum value
  Local(3)

  d@ = -(2^31)                         ' Set maximum to a tiny value

  For b@ = 1 To a@                     ' Get all values from the stack
    c@ = Pop()
    If c@ > d@ THEN d@ = c@            ' Change maximum if required
  Next
Return (d@)                            ' Return the maximum

                                       ' Hashed labels
_Mary   Print "Mary"; : Return
_had    Print "had"; : Return
_a      Print "a"; : Return
_little Print "little"; : Return
_lamb   Print "lamb"; : Return
```

{{out}}

```txt
Mary had a little lamb
Maximum is: 19

0 OK, 0:236
```



## Ursala


```Ursala
f = %gP*=

#show+

main = f <'foo',12.5,('x','y'),100>
```

<code>f</code> is defined as a function that takes a list of any length of items of any type, and uses a built-in heuristic to decide how to print them. All functions in the language are polymorphic and variadic unless specifically restricted to the contrary.

output:

```txt
'foo'
1.250000e+01
('x','y')
100
```



## Unicon

See [[#Icon|Icon]].


## V

In V, all the arguments are passed in stack, and the stack is freely accessible so
var args is the default to any level of functions

Using a count as the indication of number of arguments to extract,


```v
[myfn
   [zero? not] [swap puts pred]
   while
].

100 200 300 400 500 3 myfn
```

results in:

```v
500
400
300
```



## Visual Basic

{{works with|Visual Basic|6}}

```vb
Option Explicit
'--------------------------------------------------
Sub varargs(ParamArray a())
Dim n As Long, m As Long
    Debug.Assert VarType(a) = (vbVariant Or vbArray)
    For n = LBound(a) To UBound(a)
        If IsArray(a(n)) Then
            For m = LBound(a(n)) To UBound(a(n))
                Debug.Print a(n)(m)
            Next m
        Else
            Debug.Print a(n)
        End If
    Next
End Sub
'--------------------------------------------------
Sub Main()
Dim v As Variant

    Debug.Print "call 1"
    varargs 1, 2, 3

    Debug.Print "call 2"
    varargs 4, 5, 6, 7, 8

    v = Array(9, 10, 11)
    Debug.Print "call 3"
    varargs v

    ReDim v(0 To 2)
    v(0) = 12
    v(1) = 13
    v(2) = 14
    Debug.Print "call 4"
    varargs 11, v

    Debug.Print "call 5"
    varargs v(2), v(1), v(0), 11

End Sub
```

{{out}}

```txt
call 1
 1
 2
 3
call 2
 4
 5
 6
 7
 8
call 3
 9
 10
 11
call 4
 11
 12
 13
 14
call 5
 14
 13
 12
 11
```



## Vorpal

Each method can have a variable-length parameter (VPL), indicated by empty brackets after the parameter name.  The VLP (if present) will be replaced with an array containing all the extra arguments passed to the method.  Effectively, extra arguments are absorbed into the array.  Calling the function with fewer parameters than needed is still a runtime error.  The VPL may be omitted, which will result in an empty array as the value of that parameter.

```vorpal
self.f = method(x, y[ ], z){
   x.print()
   for(i = 0, i < y.size(), i = i + 1){
      ('[' + y[i] + ']').print()
   }
   z.print()
}

self.f(1, 2, 3)
'---'.print()
self.f(1, 2, 3, 4)
'---'.print()
self.f(1, 2)
```



## XLISP


```lisp
(defun print-on-separate-lines (&rest xs)
    (for-each print xs))

; test the function:

(print-on-separate-lines 'i 'am 'doing 'a 'great 'work 'so 'that 'i 'cannot 'come 'down)

; to use it on a list of arguments assembled at run time, first create your list

(define test '(why should the work cease whilst i leave it and come down to you))

; and then call APPLY:

(apply print-on-separate-lines test)
```

{{out}}

```txt
I
AM
DOING
A
GREAT
WORK
SO
THAT
I
CANNOT
COME
DOWN
WHY
SHOULD
THE
WORK
CEASE
WHILST
I
LEAVE
IT
AND
COME
DOWN
TO
YOU
```



## XPL0

Passing an array is probably a better way to accomplish this task.

```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
def IntSize=4;                  \number of bytes in an integer

proc Var(N...);                 \Display N strings passed as arguments
int  N;
[N:= Reserve(N*IntSize);        \reserve space for N string pointers
repeat  Text(0,N(0)); CrLf(0);  \display strings pointed to by N(0)
        N:= N+IntSize;          \point to next string
until   N=GetHp;                \pointing beyond reserved space?
];

Var(4, "Mary", "had", "a", "little")
```


Output:

```txt

Mary
had
a
little

```



## zkl

In zkl, all functions and methods are variadic (they are just passed a list).

```zkl
fcn f{vm.arglist.apply2("println")}
f("Mary","had","a","little");
```

{{output}}

```txt

Mary
had
a
little

```


```zkl
a:="This is a test".split(); //-->L("This","is","a","test")
f(a.xplode());  // xplode takes a list and blows it apart into call args
```

{{output}}

```txt

This
is
a
test

```


```zkl
fcn g{f(vm.pasteArgs(2)}
g(a.xplode());
```

pasteArgs takes the passed in function args and stuffs them back into the arglist of the function call
{{output}}

```txt

a
test

```

Of course, parameter lists can be named, have defaults, etc. Using the arglist, as a list, isn't the usual case.

{{omit from|GUISS}}
{{omit from|TI-89 BASIC}} <!-- No varargs -->
{{Omit from|ZX Spectrum Basic}} <!-- No varargs -->
{{omit from|OCaml}}
{{omit from|Axe}}
