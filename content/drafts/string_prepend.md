+++
title = "String prepend"
description = ""
date = 2019-10-09T15:11:04Z
aliases = []
[extra]
id = 16420
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}} 
{{basic data operation}}
[[Category:String manipulation]] 
[[Category: String manipulation]] 
[[Category:Simple]] 
{{omit from|bc|No string operations in bc}}
{{omit from|dc|No string operations in dc}}

;Task:
Create a string variable equal to any text value. 

Prepend the string variable with another string literal.
 
If your language supports any idiomatic ways to do this without referring to the variable twice in one expression, include such solutions.


To illustrate the operation, show the content of the variable.





## Ada


In Ada, a variable of type String cannot change its length. So the variable S which we will change, need to be of the type Unbounded_String. Thus the need for conversions from String literal to Unbounded_String for initialization, and from Unbounded_String to String for printing. 


```Ada
with Ada.Text_IO; with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Prepend_String is   
   S: Unbounded_String := To_Unbounded_String("World!"); 
begin
   S := "Hello " & S;-- this is the operation to prepend "Hello " to S. 
   Ada.Text_IO.Put_Line(To_String(S));
end Prepend_String;
```


{{out}}

```txt
Hello World!
```



## ALGOL 68


{{works with|ALGOL 68|Revision 1.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.7 algol68g-2.7].}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards).}}
'''File: String_prepend.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

STRING str := "12345678";
"0" +=: str;
print(str)
```

{{out}}

```txt

012345678

```



## AutoHotkey


```autohotkey
s := "foo"
s := s "bar"
Msgbox % s
```

{{out}}

```txt

foobar

```



## AWK


```AWK

# syntax: GAWK -f STRING_PREPEND.AWK
BEGIN {
    s = "bar"
    s = "foo" s
    print(s)
    exit(0)
}

```

{{out}}

```txt

foobar

```



## BASIC


```BBC BASIC
S$ = " World!"
S$ = "Hello" + S$
PRINT S$

```

{{out}}

```txt
Hello World!
```


=
## Applesoft BASIC
=
{{works with|Applesoft BASIC}}

=
## BBC BASIC
=
{{works with|BBC BASIC}}

==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET S$=" World!"
110 LET S$="Hello"&S$
120 PRINT S$
```



## Bracmat


```bracmat
  World!:?string
& str$("Hello " !string):?string
& out$!string
```


```txt
Hello World!
```



## C


```c>#include<stdio.h

#include<string.h>
#include<stdlib.h>

int main()
{
    char str[100]="my String";
    char *cstr="Changed ";
    char *dup;
    sprintf(str,"%s%s",cstr,(dup=strdup(str)));
    free(dup);
    printf("%s\n",str);
    return 0;
}
```

{{out}}

```txt
Changed my String
```



## C++


```cpp>include <vector

#include <algorithm>
#include <string>
#include <iostream>

int main( ) {
   std::vector<std::string> myStrings { "prepended to" , "my string" } ;
   std::string prepended = std::accumulate( myStrings.begin( ) , 
	 myStrings.end( ) , std::string( "" ) , []( std::string a , 
	    std::string b ) { return a + b ; } ) ;
   std::cout << prepended << std::endl ;
   return 0 ;
}
```

{{out}}

```txt
prepended tomy string
```



## C sharp


```csharp
using System;

namespace PrependString
{
    class Program
    {
        static void Main(string[] args)
        {
            string str = "World";
            str = "Hello " + str;
            Console.WriteLine(str);
            Console.ReadKey();
        }
    }
}
```


```txt
Hello World
```



## Clojure


```clojure
(def s (ref "World"))
(dosync (alter s #(str "Hello " %)))

user=> @s
"Hello World"
```



## COBOL


```COBOL
       identification division.
       program-id. prepend.
       data division.
       working-storage section.
       1 str pic x(30) value "World!".
       1 binary.
        2 len pic 9(4) value 0.
        2 scratch pic 9(4) value 0.
       procedure division.
       begin.
           perform rev-sub-str
           move function reverse ("Hello ") to str (len + 1:)
           perform rev-sub-str
           display str
           stop run
           .

       rev-sub-str.
           move 0 to len scratch
           inspect function reverse (str)
           tallying scratch for leading spaces
               len for characters after space
           move function reverse (str (1:len)) to str
           .
       end program prepend.
```


```txt
Hello World!
```

{{works with|GNU Cobol|2.0}}

```cobol>       >
SOURCE FREE
PROGRAM-ID. prepend.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  str                                 PIC X(30) VALUE "world!".

PROCEDURE DIVISION.
    MOVE FUNCTION CONCATENATE("Hello, ", str) TO str
    DISPLAY str
    .
END PROGRAM prepend.
```



## ColdFusion


###  Classic tag based CFML 


```cfm

<cfoutput>
	<cfset who = "World!">
	#"Hello " & who#
</cfoutput>

```

{{Output}}

```txt

Hello World! 

```



###  Script Based CFML 


```cfm><cfscript

	who = "World!";
	greeting = "Hello " & who;
	writeOutput( greeting );
</cfscript>
```

{{Output}}

```txt

Hello World! 

```



## Common Lisp

A macro is necessary in order to prepend a string in-place:

```lisp
(defmacro prependf (s &rest strs)
  "Prepend the given string variable with additional strings. The string variable is modified in-place."
  `(setf ,s (concatenate 'string ,@strs ,s)))

(defvar *str* "foo")
(prependf *str* "bar")
(format T "~a~%" *str*)
```

{{out}}

```txt
barfoo
```



## D


```d
import std.stdio;

void main() {
    string s = "world!";
    s = "Hello " ~ s; 
    writeln(s);
}
```

{{out}}

```txt
Hello world!
```


=={{header|Déjà Vu}}==

```dejavu
local :s "world!"
set :s concat( "Hello " s)
!print s
```

{{out}}

```txt
Hello world!
```



## Dyalect



```Dyalect
var s = "world!"
s = "Hello " + s
print(s)
```



## EchoLisp


```scheme

define-syntax-rule 
    (set!-string-prepend a before) 
    (set! a (string-append before a)))
   → #syntax:set!-string-prepend

(define name "Presley")
    → name
(set!-string-prepend name "Elvis ")
name
    → "Elvis Presley"

```



## Elena

ELENA 4.x:

```elena
import extensions;
import extensions'text;
 
public program()
{
    var s := "World";
    s := "Hello " + s;
    console.writeLine:s;
 
    // Alternative way
    var s2 := StringWriter.load("World");
    s2.insert(0, "Hello ");
    console.writeLine:s2;
    console.readChar()
}
```



## Elixir


```Elixir

str1 = "World!"
str = "Hello, " <> str1

```


{{out}}
"Hello, World!"


## Emacs Lisp


### version 1


```Emacs Lisp

(defun glue (str1 str2)
  (concat str1 str2) )

```


### version 2


```Emacs Lisp

(defun glue (str1 str2)
  (format "%s%s" str1 str2) )

```

<b>Eval:</b>

```Emacs Lisp

(setq str "World!")
(setq str (glue "Hello, " str) )
(insert str)

```

<b>Output:</b>

```txt

Hello, World!

```



## Erlang

{{out}}

```txt

1> S = "world".
"world"
2> "Hello " ++ S.
"Hello world"

```



## ERRE


```ERRE

......
S$=" World!"
S$="Hello"+S$
PRINT(S$)
......

```

{{out}}

```txt
Hello World!
```


=={{header|F Sharp|F#}}==

```fsharp
let mutable s = "world!"
s <- "Hello, " + s
printfn "%s" s
```



## Factor


```factor

"world"
"Hello " prepend

```




## Falcon

'''VBA/Python programmer's approach not sure if it's the most falconic way'''

```falcon

/* created by Aykayayciti Earl Lamont Montgomery
April 9th, 2018 */


s = "fun "
s = s + "Falcon"
> s

```

{{out}}

```txt

fun Falcon
[Finished in 0.2s]

```



## Forth

Forth has no string prepend word, but adding it is not difficult. This demonstration starts from the low level operations that Forth gives us and quickly builds a simple set of "WORDS" (sub-routines) that let us move strings from address to address. Strings are just an address on the data stack so we can reference them as many times as we need to. Our prepend word makes use of the Return stack as a temporary storage for the address of the string we want to prepend. Standard Forth also provides a named general purpose buffer called PAD, so we make use of that too. With this PREPEND becomes part of the language.

<lang>\ the following functions are commonly native to a Forth system. Shown for completeness

: C+!     ( n addr -- ) dup c@ rot + swap c! ;             \ primitive: increment a byte at addr by n

: +PLACE  ( addr1 length addr2 -- )                        \ Append addr1 length to addr2
          2dup 2>r  count + swap move 2r> c+! ;

: PLACE   ( addr1 len addr2 -- )                           \ addr1 and length, placed at addr2 as counted string
          2dup 2>r  1+  swap  move  2r> c! ;

\ Example begins here
: PREPEND ( addr len addr2 -- addr2)
           >R                                              \ push addr2 to return stack
           PAD PLACE                                       \ place the 1st string in PAD
           R@  count PAD +PLACE                            \ append PAD with addr2 string
           PAD count R@   PLACE                            \ move the whole thing back into addr2
           R> ;                                            \ leave a copy of addr2 on the data stack

: writeln ( addr -- ) cr count type ;                      \ syntax sugar for testing
```


Test our language extensions interactively at the console

```txt
256 buffer: string1 ok                                     
s" needs no introduction!" string1 place  ok                
string1 writeln 
needs no introduction! ok

s" This string "  string1 prepend writeln
This string needs no introduction! ok
```



## Fortran


### Early inability

Early Fortran had almost no ability to manipulate text except via overwriting text literals in a FORMAT statement used in a READ, that would then appear when the same FORMAT statement was used in a WRITE (!) perhaps as a heading. 


### Initial difficulty

With Fortran IV came the ability to use arrays of integers and the A1 format code in READ and WRITE statements for them. With sixteen-bit integers, one might use A2 and so forth, but the numerical values of the integers would not be convenient especially if the sign bit was involved. This would be even more difficult with floating-point variables. Still, the desire for good headings and annotations and flexible layout flogged one onwards. Following the Pascal "Hello world!" example, one might proceed somewhat as follows:
```Fortran
      INTEGER*4 I,TEXT(66)
      DATA TEXT(1),TEXT(2),TEXT(3)/"Wo","rl","d!"/

      WRITE (6,1) (TEXT(I), I = 1,3)
    1 FORMAT ("Hello ",66A2)

      DO 2 I = 1,3
    2   TEXT(I + 3) = TEXT(I)
      TEXT(1) = "He"
      TEXT(2) = "ll"
      TEXT(3) = "o "

      WRITE (6,3) (TEXT(I), I = 1,6)
    3 FORMAT (66A2)
      END
```

This old-style source is acceptable to the F90 compiler as it stands. By chance, two characters per integer fits nicely but in many cases having one character per variable is easier for manipulation. So, as usual with Fortran, it's all done with arrays. The DATA statement demonstrates that a quoted string is acceptable as a value for an integer; it is just a matter of bit patterns, and this type miscegnation will work with floating-point variables also though resulting in even stranger numerical values. Looking more closely, note that an INTEGER*4 variable can hold four eight-bit characters but only two-character text literals have been specified. Unlike integer constants, which might be considered to have leading zero digits, text literals are deemed to have trailing spaces as needed: <code>"Wo"</code> is deemed to be <code>"Wo  "</code> to make up to the recipient's capacity for four characters, and when format code A2 is specified, the leftmost two characters in the variable are taken. The strange ideas of "little-endianism" have never flourished on mainframes! Thus, if the format code were to be A4, then "Wo  " would appear, not "  Wo".

The first output (to standard output: unit 6) thus prepends the text "Hello " via the workings of the nominated FORMAT statement without a further mention of variable TEXT, itself not being modified in this action. Thus, this is an example of a single-mention possibility.

Some versions of Fortran offered the ability to write to a variable such as an array rather than to a nominated output unit, via a statement like <code>WRITE (TEXT,1) (TEXT(I),I = 1,3)</code>, which array could then itself be written to the actual output via normal statements. This would involve a working variable within the routines for formatted I/O to hold the output, and thus provides one of the reasons that Fortran I/O implementations seldom enable re-entrancy - as with a WRITE statement whose output expression list includes a function evaluation, which function itself attempts to WRITE something, say to a log file, with both WRITE statements employing formatting statements. More modern compilers now require the recipient for this sort of WRITE statement to be of type CHARACTER, so the older style is blocked - and re-entrancy is still a problem.

Still another variant involved writing to unit number zero, which did not actually send anything to an output recipient. Instead, the scratchpad used by the formatted I/O system would retain whatever was produced, which could then be read back via unit number zero. Indeed, reading from unit zero would reveal whatever had been the last line of the previous I/O statement. This would be of interest if a format error had been reported on a READ during some mass data acquisition, so that the error message could show the problematic input that had been obtained rather than just complain. But this facility was not common, and did not become a part of the F90 standard. Perhaps a BACKSPACE and re-read to a text variable will work instead... 

Retreating from FORMAT usage to the case of manipulating a "string" variable so as to prepend a given text to the working variable, first the existing content must be moved right to make room (again, an even number of characters is involved) which is achieved via the DO-loop, using certain constants. If on the other hand, text were to be removed from the front, then a loop would be needed to shift the surviving content leftwards. In doing this, one must pay attention to any overlaps and the direction of the loop! By chance, this exercise starts the placement after the end of the existing text but if instead the shift were to be two units, then the first-placed unit would land atop the tail end of the existing text. Thus, for rightwards shifts, one should start with the end of the surviving text and work back to its start.

Having made space, the next statements merely assign some bit patterns to elements of TEXT, and then the result is revealed, again using known constants instead of the associated variables of the more general approach. The result from the two WRITE statements is of course 
```txt
Hello world!
Hello world!
```



### Character facility

With F77 came the CHARACTER type... 
```Fortran
      CHARACTER*66 TEXT
      TEXT = "World!"
      TEXT = "Hello "//TEXT
      WRITE (6,*) TEXT
      END 
```
 
This means that variable TEXT has space for 66 characters, addressed as TEXT(''first'':''last'') starting with one. There is no associated string length facility, so the first assignment places the six characters of the supplied literal, followed by spaces all the way to the end of TEXT. Alternatively, <code>TEXT(1:6) = "World!"</code> would place only six characters, leaving the rest of TEXT to hold whatever it may. This would probably be unsuitable for the next statement, which prepends "Hello " to the content of TEXT (including positions past six) and assigns the result to TEXT, overwriting its previous content - with the aid of a temporary working area. Although in principle there could be cunning schemes that update the recipient "in place" with a minimum of character copying to and fro, this doesn't happen. Only characters up to the capacity of the recipient will be transferred from the expression's result, and if the result is shorter than the capacity of the recipient, trailing spaces will be added. All of this is extra effort! And when TEXT is written out, all 66 characters will be sent forth. It is useful to have a function that locates the last non-blank character!


### Modern

With F90, and standardised in F2003, came extensions that enable a variable to be "cut-to-fit" on each usage. The first assignment would discard any storage associated with TEXT and re-assign space matching the size of the expression's result, so TEXT would have six characters. In the next statement, the expression would be evaluated and produce twelve characters (six from "Hello ", and the six of the current size of TEXT), then the current storage for text would be discarded and TEXT re-allocated to be of size twelve. At some cost in overhead. On the other hand, rather than copy the result of an expression from the scratchpad to the recipient, with re-allocation, the recipient variable could be repointed to the result area: no copying needed.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Var s = "prepend"
s = "String " + s
Print s 
Sleep
```


{{out}}

```txt

String prepend

```


## Gambas

'''[https://gambas-playground.proko.eu/?gist=cd5ab867c22e872d69ed81fd9da96707 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString1 As String = "world!"
Dim sString2 As String = "Hello "

sString1 = sString2 & sString1

Print sString1

End
```

Output:

```txt

Hello world!

```



## Go


```go
s := "world!"
s = "Hello, " + s
```



## Haskell


```haskell

Prelude> let f = (++" World!")
Prelude> f "Hello"

```


{{out}}

```txt
"Hello world!"
```


=={{header|Icon}} and {{header|Unicon}}==


```unicon
s := "world!"
s := "Hello, " || s
```



## J


```j
   s=: 'value'
   s
value
   s=: 'new ',s
   s
new value
```



## Java


```java
public class Prepend {

    public static void main(String[] args) {
        String s = "world!";
        System.out.println("Hello " + s);
    }
}
```


{{out}}

```txt
Hello world!
```



## Javascript


```javascript
// No built-in prepend
var s=", World"
s = "Hello" + s
print(s);
```



## jq


```jq
"world!" as $s
| "Hello " + $s
```



## Julia


```julia
s = "world!"
s = "Hello " * s
```



## K


```K

    s: "world!"
"world!"
    "Hello " , s
"Hello world!"

```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    var s = "Obama"
    s = "Barack " + s
    println(s)

    // It's also possible to use this standard library function
    // though this is not what it's really intended for
    var t = "Trump"
    t = t.prependIndent("Donald ")
    println(t)
}
```


{{out}}

```txt

Barack Obama
Donald Trump

```



## Lasso


```Lasso
local(x = ', World!')
#x->merge(1,'Hello')
#x // Hello, World!
```



## LFE


Using the concatenation operator:


```lisp

> (set s "world")
"world"
> (++ "hello " s)
"hello world"

```


Using the concatenation function:


```lisp

> (set s "world")
"world"
> (string:concat "hello " s)
"hello world"

```



## Lingo


```lingo
str = "world!"
put "Hello " before str
put str
-- "Hello world!"
```



## LiveCode

The idiomatic way is to use "before"
```LiveCode
put "world" into x
put "hello" before x
put x // hello world
```



## Lua


By concatenation:


```lua

s = "12345678"
s = "0" .. s
print(s)
```


By string formatting:


```lua

s = "12345678"
s = string.format("%s%s", "0", s)
print(s)
```


By list joining:


```lua

s = "12345678"
s = table.concat({"0", s})
print(s)
```


{{out}} of each solution:

```txt

    012345678

```


## M2000 Interpreter


```M2000 Interpreter

Module PrependString {
      A$="Hello"
      A$+=" World"
      Print A$
}
PrependString

```


{{out}}

```txt

Hello World

```



## Maple


```maple
l := " World";
m := cat("Hello", l);
n := "Hello"||l;
o := `||`("Hello", l);
```

{{out}}

```txt

                            " World"
                         "Hello World"
                         "Hello World"
                         "Hello World"

```



## Mathematica


```Mathematica
a = "any text value";
a =  "another string literal" <> a  (* using concatenation (no built-in prepend) *)
```

{{out}}

```txt
"another string literalany text value"
```



## Mercury


```mercury
:- module string_prepend.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module string.
main(!IO) :-
    S = "World!\n",
    io.write_string("Hello " ++ S, !IO).
```

{{out}}

```txt

Hello World!

```



## Neko

The plus operator, +, concatenates string data.  Neko treats strings as mutable fixed length buffers, so some care would need to be taken when prepending variables to variables as there may be buffer sizing to take into consideration.  For literals, this is not a concern, as the literals are placed in buffers of the proper size by the compiler.


```ActionScript
/**
 <doc><p>String prepend in Neko
```
</doc>
**/

var str = ", world"
str = "Hello" + str
$print(str, "\n")
```

{{out}}

```txt
prompt$ nekoc string-prepend.neko
prompt$ neko string-prepend.n
Hello, world
```



## NetRexx


```NetRexx
s_ = 'world!'
s_ = 'Hello, 's_
say s_
```

{{out}}

```txt

Hello, world!

```



## NewLISP


```NewLISP
(setq str "bar")
(push "foo" str)
(println str)
```



## Nim


```nim
var str = "12345678"
str = "0" & str
echo str
```



## Objeck


```objeck
class Prepend  {
  function : Main(args : String[]) ~ Nil {
    s := "world!";
    "Hello {$s}"->PrintLine();
  }
}
```



```txt

Hello World!

```



## OCaml


```ocaml
let () =
  let s = ", world" in
  let s = "Hello" ^ s in
  print_endline s
```



## Oforth



```Oforth
" World" "Hello" swap + println
```


{{out}}

```txt

Hello World

```



## PARI/GP

Not supported in GP.

```parigp
s = "world!";
s = Str("Hello, ", s)
```

{{out}}

```txt
%1 = "Hello, world!"
```



## Pascal


{{works with|Free Pascal|2.6.2}}


```Pascal
program StringPrepend;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this };

var
  s: String = ' World !';
begin
  s := 'Hello' + s;
  WriteLn(S);
  ReadLn;
end.
```

{{out}}

```txt
Hello  World !
```



## Perl


```Perl
use strict;
use warnings;
use feature ':all';

# explicit concatentation
$_ = 'bar';
$_ = 'Foo' . $_;
say;


# lvalue substr
$_ = 'bar';
substr $_, 0, 0, 'Foo';
say;


# interpolation as concatenation
# (NOT safe if concatenate sigils)
$_ = 'bar';
$_ = "Foo$_";
say;
```


{{out}}

```txt

Foobar
Foobar
Foobar

```



## Perl 6


```perl6
# explicit concatentation
$_ = 'byte';
$_ = 'kilo' ~ $_;
.say;

# interpolation as concatenation
$_ = 'buck';
$_ = "mega$_";
.say;

# lvalue substr
$_ = 'bit';
substr-rw($_,0,0) = 'nano';
.say;

# regex substitution
$_ = 'fortnight';
s[^] = 'micro';
.say;

# reversed append assignment
$_ = 'cooper'; 
$_ [R~]= 'mini';
.say;
```

{{out}}

```txt
kilobyte
megabuck
nanobit
microfortnight
minicooper
```



## Phix


```Phix
string s = "World"
s = "Hello "&s
```

<b>NB:</b> s = prepend(s,"Hello ") gives typecheck: s is {"Hello ",'W','o','r','l','d'}, rather than the "Hello World" you probably wanted.


## PicoLisp


```picolisp
(setq Str1 "12345678!")
(setq Str1 (pack "0" Str1))
(println Str1)
```

{{out}}

```txt

"012345678!"

```



## PL/I


```PL/I

Pre_Cat: procedure options (main); /* 2 November 2013 */
   declare s character (100) varying;
   s = ' bowl';
   s = 'dust' || s;
   put (s);
end Pre_Cat;

```


```txt

dust bowl

```



## PlainTeX


```tex
\def\prepend#1#2{% #1=string  #2=macro containing a string
	\def\tempstring{#1}%
	\expandafter\expandafter\expandafter
	\def\expandafter\expandafter\expandafter
	#2\expandafter\expandafter\expandafter
	{\expandafter\tempstring#2}%
}
\def\mystring{world!}
\prepend{Hello }\mystring
Result : \mystring
\bye
```


Here is an equivalent code with eTeX capabilities:

```tex
\def\prepend#1#2{% #1=string  #2=macro containing a string
	\edef#2{\unexpanded{#1}\unexpanded\expandafter{#2}}%
}
\def\mystring{world!}
\prepend{Hello }\mystring
Result : \mystring
\bye
```



## PowerShell


```PowerShell

$str = "World!"
$str = "Hello, " + $str
$str

```


```txt
Hello, World!
```



## Prolog


{{works with|SWI-Prolog}}

In its admirable wisdom, Prolog is generally unfriendly 
to state mutations and destructive assignment. However, it
is also very flexible. Using the traditional representation
of strings as lists of character codes, and the non-logical
predicate `setarg/3`, we can destructively set the head and 
tail of the list to achieve a mutation of the variable holding
the string. I define an operator for the purpose: 


```prolog

:- op(200, xfx, user:(=+)).

%% +Prepend =+ +Chars
%
%    Will destructively update Chars
%    So that Chars = Prepend prefixed to Chars.
%    eazar001 in ##prolog helped refine this approach.

[X|Xs] =+ Chars :-
  append(Xs, Chars, Rest),
  nb_setarg(2, Chars, Rest),
  nb_setarg(1, Chars, X).

```


Example of this abomination in action:


```prolog

?- Str = `World!`, `Hello, ` =+ Str.
Str = "Hello, World!".
```


Note: I can't imagine when I would want to do this in Prolog.


## PureBasic


```purebasic
S$ = " World!"
S$ = "Hello" + S$
If OpenConsole()
  PrintN(S$)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
Hello World!
```



## Python

'''File: string_prepend.py'''


```python
#!/usr/bin/env python
# -*- coding: utf-8 -*-

s = "12345678"
s = "0" + s  # by concatenation
print(s)
```


{{out}}

```txt

012345678

```



## Racket


```racket
;there is no built-in way to set! prepend in racket
(define str "foo")
(set! str (string-append "bar " str))
(displayln str)

;but you can create a quick macro to solve that problem
(define-syntax-rule (set-prepend! str value)
  (set! str (string-append value str)))

(define macrostr " bar")
(set-prepend! macrostr "foo")
(displayln macrostr)

```

{{out}}

```txt
bar foo
foo bar
```


## Red


```Red
Red []
s: "world"
insert s "hello "
print s

```

{{out}}

```txt
hello world
```


## REXX


```rexx
zz= 'llo world!'          /*─────────────── using literal abuttal.────────────*/
zz= 'he'zz                /*This won't work if the variable name is  X  or  B */
say zz


gg = "llo world!"         /*─────────────── using literal concatenation.──────*/
gg = 'he' || gg
say gg


aString= 'llo world!'     /*─────────────── using variable concatenation.─────*/
bString= "he"
aString= bString || aString
say aString
```

'''output'''

```txt

hello world!
hello world!
hello world!

```



## Ring


```ring

aString = "World!"
bString = "Hello, " + aString
see bString + nl

```



## Ruby

There is a method for prepending a string, aptly named "prepend".

```ruby
str = "llo world"
str.prepend("He")
p str #=> "Hello world"
```



## Rust


```rust

let mut s = "World".to_string();
s.insert_str(0, "Hello ");
println!("{}", s);

```



## Scala

Evaluation in Scala worksheet

```scala
  val s = "World" // Immutables are recommended   //> s  : String = World
  val f2 = () => ", " //Function assigned to variable
                                                  //> f2  : () => String = <function0>
  val s1 = "Hello" + f2() + s                     //> s1  : String = Hello, World
  println(s1);                                    //> Hello, World
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var string: s is "world!";
  begin
    s := "Hello " & s; 
    writeln(s);
  end func;
```


{{out}}

```txt

Hello world!

```



## Sidef


```ruby
var str = 'llo!';
str.sub!(/^/, 'He');
say str;
```


or

```ruby
var str = 'llo!';
str.prepend!('He');
say str;
```


{{out}}

```txt
Hello!
```



## SNOBOL4


```SNOBOL4
    s = ', World!'
    OUTPUT = s = 'Hello' s
END
```

{{out}}

```txt

Hello, World!

```


## Stata


```stata
sca s="Vita Brevis"
sca s="Ars Longa "+s
di s

Ars Longa Vita Brevis
```



## Swift


```Swift
var str = ", World"
str = "Hello \(str)"
println(str)
```

{{out}}

```txt

Hello, World!

```



## Tcl

Concatenation is a fundamental feature of Tcl's basic language syntax.

```tcl
set s "llo world"
set s "he$s"
puts $s
```

{{out}}

```txt
hello world
```



## Ursa


```ursa
decl string s

# set s to "world"
set s "world"

# prepend "hello "
set s (+ "hello " s)

# outputs "hello world"
out s endl console
```



## VBA


```VB
Function StringPrepend()
Dim s As String
s = "bar"
s = "foo" & s
Debug.Print s
End Function
```



## VBScript


```vb
s = "bar"
s = "foo" & s
WScript.Echo s
```

{{Out}}

```txt
foobar
```



## Wart


```python
s <- "12345678"
s <- ("0" + s)
```



## Xojo


```vb
Dim s As String = "bar"
s = "foo" + s
MsgBox(s)
```

{{Out}}

```txt
foobar
```



## zkl


```zkl
s:="bar"; s="foo" + s;           s.println();
s:="bar"; s=String("foo",s);     s.println();
s:="bar"; s="%s%s".fmt("foo",s); s.println();
   // a Data is a byte buffer/editor:
s:=Data(Void,"bar").insert(0,"foo").text; s.println();
```

{{out}}

```txt

foobar
foobar
foobar
foobar

```

