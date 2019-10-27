+++
title = "String concatenation"
description = ""
date = 2019-10-09T08:35:44Z
aliases = []
[extra]
id = 3263
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}[[Category:String manipulation]][[Category: String manipulation]]{{basic data operation}}[[Category:Simple]]
Create a string variable equal to any text value. 
Create another string variable whose value is the original variable concatenated with another string literal.

To illustrate the operation, show the content of the variables.

;Tasks featuring Strings:
{{Template:Strings}}


## ABAP


```ABAP
DATA: s1 TYPE string,
      s2 TYPE string.

s1 = 'Hello'.
CONCATENATE s1 ' literal' INTO s2 RESPECTING BLANKS.
WRITE: / s1.
WRITE: / s2.

```

{{out}}

```txt

Hello
Hello literal

```


### Another way


```ABAP
REPORT string_concatenation.

DATA(var1) = 'Hello'.
DATA(var2) = 'Literal'.

cl_demo_output=>new(
          )->begin_section( 'String concatenation using |{ }|'
          )->write( 'Statement: |{ var1 } { var2 }|'
          )->write( |{ var1 } { var2 }|
          )->begin_section( 'String concatenation with new string'
          )->write( 'Statement: |{ var1 } world!|'
          )->write( |{ var1 } world!|
          )->display( ).

```

{{out}}

```txt

Hello literal
Hello world!

```



## ActionScript


```actionscript
package
{
    public class Str
    {
        public static function main():void
        {
            var s:String = "hello";
            trace(s + " literal");
            var s2:String = s + " literal";
            trace(s2);
        }
    }
}
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure String_Concatenation is
   S1 : constant String := "Hello";
   S2 : constant String := S1 & " literal";
begin
   Put_Line (S1);
   Put_Line (S2);
end String_Concatenation;
```

{{out|Sample output}}

```txt

Hello
Hello literal

```



## Aime


```aime
text s, v;

s = "Hello";
o_(s, "\n");
v = s + ", World!";
o_(v, "\n");
```

{{out}}

```txt
Hello
Hello, World!
```



## Apex



```apex

String s1 = 'Hello ';
String s2 = 'Salesforce Developer!';

String s3 = s1+s2;

// Print output
System.debug(s3);
```

{{out}}

```txt
Hello Salesforce Developer!
```



## AppleScript


```applescript
try
    set endMsg to "world!"
    set totMsg to "Hello, " & endMsg
    display dialog totMsg
end try
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program strConcat.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall
/* Initialized data */
.data
szMessFinal:   .asciz "The final string is \n"

szString:            .asciz "Hello "
szString1:           .asciz " the world. \n"

/* UnInitialized data */
.bss 
szFinalString:   .skip 255

/*  code section */
.text
.global main 
main:
                                         @ load string 
    ldr r1,iAdrszString
	ldr r2,iAdrszFinalString
    mov r4,#0
1:
    ldrb r0,[r1,r4]                      @ load byte of string
    strb r0,[r2,r4]
    cmp r0,#0                            @ compar with zero ?
    addne r4,#1
    bne 1b
    ldr r1,iAdrszString1
    mov r3,#0
2:
    ldrb r0,[r1,r3]                      @ load byte of string 1
    strb r0,[r2,r4]
    cmp r0,#0                            @ compar with zero ?
    addne r4,#1
    addne r3,#1
    bne 2b
    mov r0,r2                            @ display final string
    bl affichageMess
100:                                     @ standard end of the program */
    mov r0, #0                           @ return code
    mov r7, #EXIT                        @ request to exit program
    svc 0                                @ perform the system call
iAdrszString:             .int szString
iAdrszString1:            .int szString1
iAdrszFinalString:       .int szFinalString
iAdrszMessFinal:          .int szMessFinal

/******************************************************************/
/*     display text with size calculation                         */ 
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers 
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index 
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop 
                                                @ so here r2 contains the length of the message 
    mov r1,r0                                   @ address message in r1 
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write" 
    svc #0                                      @ call systeme
    pop {r0,r1,r2,r7,lr}                        @ restaur des  2 registres
    bx lr                                       @ return


```



## AutoHotkey


```AutoHotkey
s := "hello"
Msgbox, %s%
s1 := s . "  literal" ;the . is optional
Msgbox, %s1%
```



## AWK

The AWK concatenation operator is nothing.

```awk
BEGIN {
   s = "hello"
   print s " literal"
   s1 = s " literal"
   print s1
}
```



## Axe


```axe
Lbl CONCAT
Copy(r₁,L₁,length(r₁))
Copy(r₂,L₁+length(r₁),length(r₂)+1)
L₁
Return
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
STRING s := "hello";
print ((s + " literal", new line));
STRING s1 := s + " literal";
print ((s1, new line))
```

{{out}}

```txt

hello literal
hello literal

```



## Arturo



```arturo
str1 "Hello "
str2 "World"

print str1 + str2 + "!"
```


{{out}}


```txt
Hello World!
```



## BASIC

{{works with|QuickBasic|4.5}}
{{works with|Liberty BASIC}}

```qbasic
s$ = "hello"
print s$;" literal" 'or s$ + " literal"
s2$ = s$ + " literal"
print s2$
```

{{out}}

```txt
hello literal
hello literal
```


=
## Applesoft BASIC
=

```ApplesoftBasic
S$ = "HELLO"
PRINT S$;" LITERAL" :REM OR S$ + " LITERAL"
S2$ = S$ + " LITERAL"
PRINT S2$
```


=
## BBC BASIC
=

```bbcbasic
      stringvar1$ = "Hello,"
      stringvar2$ = stringvar1$ + " world!"
      PRINT "Variable 1 is """ stringvar1$ """"
      PRINT "Variable 2 is """ stringvar2$ """"
```

{{out}}

```txt
Variable 1 is "Hello,"
Variable 2 is "Hello, world!"
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET S$="Hello"
110 LET S$=S$&" world!"
120 PRINT S$
```


=
## ZX Spectrum Basic
=

```zxbasic
10 LET s$="Hello"
20 LET s$=s$+" World!"
30 PRINT s$
```



## Batch File


```dos
set string=Hello
echo %string% World
set string2=%string% World
echo %string2%
```



## Bracmat


```bracmat
"Hello ":?var1 
& "World":?var2 
& str$(!var1 !var2):?var12 
& put$("var1=" !var1 ", var2=" !var2 ", var12=" !var12 "\n")
```

{{out}}

```txt
var1= Hello  , var2= World , var12= Hello World
```



## Burlesque


```burlesque
blsq ) "Hello, ""world!"?+
"Hello, world!"
```



## C


```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>

char *sconcat(const char *s1, const char *s2)
{
  char *s0 = malloc(strlen(s1)+strlen(s2)+1);
  strcpy(s0, s1);
  strcat(s0, s2);
  return s0;
}

int main()
{
   const char *s = "hello";
   char *s2;
   
   printf("%s literal\n", s);
   /* or */
   printf("%s%s\n", s, " literal");
   
   s2 = sconcat(s, " literal");
   puts(s2);
   free(s2);
}
```



## ChucK

<lang>
"Hello" => string A;
A + " World!" => string B;
<<< B >>>;

```

{{out}}

```txt
"Hello World!"
```


## C++


```cpp>#include <string

#include <iostream>

int main() {
   std::string s = "hello";
   std::cout << s << " literal" << std::endl;
   std::string s2 = s + " literal";
   std::cout << s2 << std::endl;
   return 0;
}
```

{{out}}

```txt
hello literal
hello literal
```


=={{header|C sharp|C#}}==

```csharp
using System;

class Program {
    static void Main(string[] args) {
        var s = "hello";
        Console.Write(s);
        Console.WriteLine(" literal");
        var s2 = s + " literal";
        Console.WriteLine(s2);
    }
}
```



## Clojure


```lisp
(def a-str "abcd")
(println (str a-str "efgh"))

(def a-new-str (str a-str "efgh"))
(println a-new-str)
```



## COBOL

With the <code>STRING</code> verb:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Concat.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Str  PIC X(7) VALUE "Hello, ".
       01  Str2 PIC X(15).

       PROCEDURE DIVISION.
           DISPLAY "Str  : " Str
           STRING Str " World!" DELIMITED BY SIZE INTO Str2
           DISPLAY "Str2 : " Str2

           GOBACK
           .
```

Alternate method using the <code>CONCATENATE</code> intrinsic function:

```cobol
       ...
       PROCEDURE DIVISION.
           DISPLAY "Str  : " Str
           MOVE FUNCTION CONCATENATE(Str, " World!") TO Str2
           DISPLAY "Str2 : " Str2

           GOBACK
           .
```


String literals can also be concatenated in the follwing ways:

```cobol
*      *> Using a '&'.
       01  Long-Str-Val     PIC X(200) VALUE "Lorem ipsum dolor sit "
           & "amet, consectetuer adipiscing elit, sed diam nonummy "
           & "nibh euismod tincidunt ut laoreet dolore magna aliquam "
           & "erat volutpat.".

*      *> Using a '-' in column 7. Note the first two literals have no
*      *> closing quotes.
       01  Another-Long-Str PIC X(200) VALUE " Ut wisi enim ad minim 
      -    "veniam, quis nostrud exerci tation ullamcorper suscipit
      -    "lobortis nisl ut aliquip ex ea commodo consequat".
```



## Common Lisp


```lisp
(let ((s "hello"))
    (format t "~a there!~%" s)
    (let* ((s2 " there!")
           (s (concatenate 'string s s2)))
        (format t "~a~%" s)))
```


```lisp
(defparameter *s* "hello")
(print (concatenate 'string *s* " literal"))
(defparameter *s1* (concatenate 'string *s* " literal"))
(print *s1*)
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE StringConcatenation;
IMPORT StdLog;

PROCEDURE Do*;
VAR
	str1,str2: ARRAY 128 OF CHAR;
BEGIN
	str1 := "Hello";
	str2 := str1 + " world";
	StdLog.String(":> " + str2);StdLog.Ln
END Do;

END StringConcatenation.

```

Execute: ^Q StringConcatenation.Do<br/>
{{out}}

```txt

:> Hello world

```


## D


```d
import std.stdio;
 
void main() {
    string s = "hello";
    writeln(s ~ " world");
    auto s2 = s ~ " world";
    writeln(s2);
}
```


## DCL


```DCL
$ string1 = "hello"
$ string2 = string1 + " world"
$ show symbol string*
```

{{out}}

```txt
  STRING1 = "hello"
  STRING2 = "hello world"
```



## Delphi


```delphi
program Concat;

{$APPTYPE CONSOLE}

var
  s1, s2: string;
begin
  s1 := 'Hello';
  s2 := s1 + ' literal';
  WriteLn(s1);
  WriteLn(s2);
end.
```

=={{header|Déjà Vu}}==

```dejavu
local :s1 "hello"
local :s2 concat( s1 ", world" )
!print s2
```

{{out}}

```txt
hello, world
```



## DWScript


```delphi
var s1 := 'Hello';
var s2 := s1 + ' World';

PrintLn(s1);
PrintLn(s2);
```



## Dyalect


{{trans|Swift}}


```Dyalect
var s = "hello"
print(s + " literal")
var s1 = s + " literal"
print(s1)
```



## Dylan.NET


```Dylan.NET

//to be compiled using dylan.NET v. 11.5.1.2 or later.
#refstdasm mscorlib.dll

import System

assembly concatex exe
ver 1.3.0.0

class public Program

   method public static void main()
        var s as string = "hello"
        Console::Write(s)
        Console::WriteLine(" literal")
        var s2 as string = s + " literal"
        Console::WriteLine(s2)
  end method

end class
```



## EasyLang

<lang>a$ = "hello"
b$ = a$ & " world"
print b$
```



## Ela

Strings in Ela support a polymorphic concatenation operator (++):

```ela
hello = "Hello"
hello'world = hello ++ ", " ++ "world"
(hello, hello'world)
```

{{out}}

```txt
("Hello", "Hello, world!")
```

However, as long as strings in Ela are indexed arrays, this operator is not very effective for
a large number of concatenations. Therefore one can use an alternate technique (a pure StringBuilder
type defined in standard prelude). The resulting code would look like so:

```ela
toString $ "Hello" +> ", " +> "world"
```

The (+>) token is a type constructor. Therefore the result of its application is an instance of type
StringBuilder. In order to produce a string one should call a polymorphic toString function at the end
as shown above.


## Elena

ELENA 4.x:

```elena
public program()
{
    var s := "Hello";
    var s2 := s + " literal";
 
    console.writeLine:s;
    console.writeLine:s2;
    console.readChar()
}
```

{{out}}

```txt

Hello
Hello literal

```



## Elixir


```elixir

s = "hello"
t = s <> " literal"

IO.puts s
IO.puts t

```

{{out}}

```txt

hello
hello literal

```



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

(setq str1 "Hello, ")
(setq str2 "World!")
(insert (glue str1 str2) )

```

<b>Output:</b>

```txt

Hello, World!

```



## Erlang


```Erlang
S = "hello",
S1 = S ++ " literal",
io:format ("~s literal~n",[S]),
io:format ("~s~n",[S1])
```

{{out|Sample output}}

```txt

hello literal
hello literal

```



## ERRE


```ERRE

  ..........
  S$="HELLO"
  PRINT(S$;" LITERAL") ! or S$+" LITERAL"
  S2$=S$+" LITERAL"
  PRINT(S2$)
  ..........

```



## Euphoria


```Euphoria
sequence s, s1
s = "hello"
puts(1, s & " literal")
puts(1,'\n')
s1 = s & " literal"
print (1, s1))
puts(1,'\n')
```

{{out}}
 hello literal
 hello literal



## Excel

Take three cells, say A1,B1 and C1. In C1, type in :


```excel


=CONCATENATE(A1;" ";B1)


```


As the text in A1 and/or B1 is changed, C1 will be updated.

<lang>
Hello	World	Hello World

```



=={{header|F_Sharp|F#}}==
{{trans|C#}}

```fsharp
open System

[<EntryPoint>]
let main args =
    let s = "hello"
    Console.Write(s)
    Console.WriteLine(" literal")
    let s2 = s + " literal"
    Console.WriteLine(s2)
    0
```



## Factor


```factor
"wake up" [ " sheeple" append print ] [ ", you sheep" append ] bi print
```




## Falcon

'''VBA/Python programmer's approach.  I'm just a junior Falconeer but this code seems falconic''

```falcon

/* created by Aykayayciti Earl Lamont Montgomery
April 9th, 2018 */

s = "critical"
> s + " literal"
s2 = s + " literal"
> s2

```

{{out}}

```txt

critical literal
critical literal
[Finished in 0.2s]

```



## Fantom

Illustrating in <tt>fansh</tt>:

```fantom>fansh
 a := "abc"
abc
fansh> b := a + "def"
abcdef
fansh> a
abc
fansh> b
abcdef
```



## Forth

{{works with|GNU Forth}}

```forth
s" hello" pad place
pad count type
s"  there!" pad +place    \ +place is called "append" on some Forths
pad count type
```



## Fortran


```fortran
program StringConcatenation

integer, parameter          :: maxstringlength = 64
character (maxstringlength) :: s1, s = "hello"

print *,s // " literal"
s1 = trim(s) // " literal"
print *,s1

end program
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Var s1 = "String"
Var s2 = s1 + " concatenation"
Print s1
Print s2
Sleep
```


{{out}}

```txt

String
String concatenation

```



## Frink


```frink

a = "Frink"
b = a + " rules!"
println[b]

```



## Gambas

In gambas, the ampersand symbol is used as a concatenation operator:

'''[https://gambas-playground.proko.eu/?gist=098450adbbe0e284f0b9cdac67d74dda Click this link to run this code]'''

```gambas
Public sub main()
DIM bestclub AS String
DIM myconcat AS String

bestclub = "Liverpool"
myconcat = bestclub & " Football Club"

Print myconcat

End
```



## GlovePIE


```glovepie
var.text1="Hello, "
debug=var.text1+"world!"
```



## Go


```go
package main

import "fmt"

func main() {
    // text assigned to a string variable
    s := "hello"

    // output string variable
    fmt.Println(s)

    // this output requested by original task descrption, although
    // not really required by current wording of task description.
    fmt.Println(s + " literal")

    // concatenate variable and literal, assign result to another string variable
    s2 := s + " literal"

    // output second string variable
    fmt.Println(s2)
}
```

{{out}}

```txt

hello
hello literal
hello literal

```



## Golfscript


```golfscript
"Greetings ":s;
s"Earthlings"+puts
s"Earthlings"+:s1;
s1 puts
```



## Groovy


```groovy
def s = "Greetings "
println s + "Earthlings"

def s1 = s + "Earthlings"
println s1
```

{{out}}

```txt
Greetings Earthlings
Greetings Earthlings
```



## Halon

The dot (concatenation) operator may cast datatypes to strings.

```halon
echo "Hello" . "World " . 123;
```



## Haskell


```haskell
import System.IO
s = "hello"
s1 = s ++ " literal"
main = do putStrLn (s ++ " literal")
          putStrLn s1
```



## HicEst


```HicEst
CHARACTER s = "hello", sl*100

WRITE() s // " literal"
sl = s // " literal"
WRITE() sl
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
s1 := "hello"
write(s2 := s1 || " there.")  # capture the reuslt for 
write(s2)                     # ... the 2nd write
end
```



## IDL


```idl
s1='Hello'
print, s1 + ' literal'
s2=s1 + ' literal'
print, s2
```



## J


```J
   s1 =. 'Some '
   ]s1, 'text '
Some text 
   ]s2 =. s1 , 'more text!'
Some more text!
```

For more info see:
* http://www.jsoftware.com/help/dictionary/d320.htm on <code>,</code>
* http://www.jsoftware.com/help/dictionary/d500.htm on <code>]</code>


## Java


```java5
public class Str{
   public static void main(String[] args){
      String s = "hello";
      System.out.println(s + " literal");
      String s2 = s + " literal";
      System.out.println(s2);
   }
}
```

{{out}}

```txt
hello literal
hello literal
```



## JavaScript


```javascript
var s = "hello"
print(s + " there!")
```



## jq


```jq
"hello" as $s | $s + " there!"
```

{{Out}}
 # Use the -r command-line option if you wish 
 # to suppress the string quotation marks
 hello there!

## Julia


```julia
s = "hello"
println(s * " there!")
```



## K

Translation of <b>J</b> code:

```K

s1: "Some "
s1, "text "
s2: s1 , "more text!"

```

{{out}}

```txt

"Some "
"Some text"
"Some more text!"

```



## Kotlin


```scala
fun main(args: Array<String>) {
    val s1 = "James"
    val s2 = "Bond"
    println(s1)
    println(s2)
    val s3 = s1 + " " + s2
    println(s3)
}
```

{{Out}}

```txt
James
Bond
James Bond
```



## LabVIEW

The two input on the left are String Controls, the output on the right is a String Indicator. All of them can be placed on the Front Panel. 
The Concatenate Strings function can be placed on the Block Diagram. 
You can switch between the Front Panel and the Block Diagram by pressing Ctrl+E.

[[File:LV_strcat.png]]


## Lang5


```lang5
: concat  2 compress "" join ;
'hello " literal" concat
```




## Lasso


```Lasso
local(x = 'Hello')
local(y = #x + ', World!')
#x // Hello
#y // Hello, World!
```



## Liberty BASIC

See [[#BASIC|BASIC]].


## Lingo


```lingo
a = "Hello"
b = a & " world!"
put b
-- "Hello world!"
```



## Lisaac


```Lisaac
Section Header

+ name := STRING_CONCATENATION;

Section Public

- main <- (
  + sc : STRING_CONSTANT;
  + sv : STRING;

  sc := "Hello";
  (sc + " literal").println;

  sv := sc + " literal";
  sv.println;

);
```



## LiveCode


```LiveCode
local str="live"
put str & "code" into str2
put str && str2
```

Output
```txt
live livecode
```



## Logo


```logo
make "s "hello
print word :s "| there!|
```



## Lua


```lua
a = "hello "
print(a .. "world")
c = a .. "world"
print(c)
```



## M2000 Interpreter

M2000 Environment is an application written in Visual Basic 6, so can use strings UTF16LE (max 2GByte), and Ansi strings. So here is an example which add two Ansi strings. To print them to console we have to convert to UTF16LE. We can use ansi strings for files and for buffers (memory blocks). Conversion use Locale (so Locale 1032 can be used for Greek Ansi codepage)

A memory word is two bytes.


```M2000 Interpreter

Module CheckString {
      s$ = "hello"
      PRINT s$;" literal" 'or s$ + " literal"
      s2$ = s$ + " literal"
      PRINT s2$
      Print Len(s2$)=13
      \\ get an ansi string
      k$=Str$("Hello")
      Print Len(k$)=2.5  ' 2.5 words or 5 bytes
      Print Chr$(k$)
      k2$=k$+Str$(" literal")
      Print Len(k2$)=6.5  ' 13 bytes
      Print Chr$(k2$)
      Print Len(Chr$(k2$))=13 ' words
}
CheckString

```



## Maple


```Maple
str := "Hello":
newstr := cat(str,", world!"):
str;
newstr;
```

{{out}}

```txt

                            "Hello"
                        "Hello, world!"

```



## Mathematica


```Mathematica
str= "Hello ";
str<>"Literal"
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB>>
 string1 = '1 Fish'

string1 =

1 Fish

>> string2 = [string1 ', 2 Fish, Red Fish, Blue Fish'] 

string2 =

1 Fish, 2 Fish, Red Fish, Blue Fish
```



## Maxima


```maxima
s: "the quick brown fox";
t: "jumps over the lazy dog";
sconcat(s, " ", t);
/* "the quick brown fox jumps over the lazy dog" */
```



## Mercury


```mercury
:- module string_concat.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

main(!IO) :-
    S = "hello",
    S1 = S ++ " world",
    io.write_string(S, !IO), io.nl(!IO),
    io.write_string(S1, !IO), io.nl(!IO).
```



## min

{{works with|min|0.19.3}}

```min
"butter" :a
(a "scotch")=> "" join :b
a puts!
b puts!
```

{{out}}

```txt

butter
butterscotch

```



## MiniScript


```MiniScript
s = "hello"
print s + " world!"
```


{{output}}

```txt
hello world!
```



## MUMPS


```MUMPS
STRCAT
 SET S="STRING"
 WRITE !,S
 SET T=S_" LITERAL"
 WRITE !,T
 QUIT
```

{{out}}

```txt

CACHE>D STRCAT^ROSETTA
 
STRING
STRING LITERAL

```



## M4

M4 has macros rather than variables, but a macro expanded can work like a variable.

```m4
define(`concat',`$1$2')dnl
define(`A',`any text value')dnl
concat(`A',` concatenated with string literal')
define(`B',`concat(`A',` and string literal')')dnl
B
```



## MAXScript


```maxscript
s = "hello"
print (s + " literal")
s1 = s + " literal"
print s1
```



## Metafont


```metafont
string a, b;
a := "String";
message a & " literal";
b := a & " literal";
message b;
```


=={{header|Modula-3}}==
Strings in Modula-3 are called <code>TEXT</code>s.  Concatenation can use <code>&</code>, just like [[Ada]].

```modula3
MODULE Concat EXPORTS Main;

IMPORT IO;

VAR string: TEXT := "String";
    string1: TEXT;

BEGIN
  IO.Put(string & " literal.\n");
  string1 := string & " literal.\n";
  IO.Put(string1);
END Concat.
```

Modula-3 also provides modules for dealing with <code>TEXT</code>s, such as <code>Text</code>.

```modula3
string1 := Text.Concat(string, " literal.\n");
```



## Neko


```ActionScript
/**
 String concatenation, in Neko
 Tectonics:
    nekoc string-concatenation.neko
    neko string-concatenation [addon]
*/

var arg = $loader.args[0]
var addon
if arg != null addon = $string(arg)

var s = "abc"
$print("s: ", s, "\n")

var c = s + "def"
$print("c: ", c, "\n")

if arg != null {
    c += addon
    $print("addon: ", addon, "\n")
    $print("c: ", c, "\n")
}
```


{{out}}

```txt
prompt$ nekoc string-concatenation.neko
prompt$ neko string-concatenation.n xyz
s: abc
c: abcdef
addon: xyz
c: abcdefxyz
```



## Nemerle

Can be done with Concat() method or + operator:

```Nemerle
using System;
using System.Console;
using Nemerle.Utility.NString; // contains method Concat()

module Stringcat
{
    Main() : void
    {
        def text1 = "This string has";
        def cat1  = Concat( " ", [text, "been concatenated"]);
        def cat2  = text1 + " also been concatenated";
        Write($"$cat1\n$cat2\n");
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols

s1 = 'any text value'
s2 = 'another string literal'
s3 = s1    s2 -- concatenate variables with blank space (note that only one blank space is added)
s4 = s1 || s2 -- concatenate variables with abuttal (here, no blank spaces are added)
s5 = s1    'another string literal' -- concatenate a variable and a literal with blank space
s6 = s1'another string literal'     -- concatenate a variable and a literal using abuttal
s7 = s1 || 'another string literal' -- ditto

say 's1:' s1 -- concatenation with blank space is employed here too
say 's2:' s2
say 's3:' s3
say 's4:' s4
say 's5:' s5
say 's6:' s6
say 's7:' s7

```

{{out}}

```txt

s1: any text value
s2: another string literal
s3: any text value another string literal
s4: any text valueanother string literal
s5: any text value another string literal
s6: any text valueanother string literal
s7: any text valueanother string literal
```



## NewLISP


```NewLISP
(let (str1 "foo")
     (println str1)
     (let (str2 (string str1 "bar"))
	  (println str2)))
```



## Nim

Strings can be concatenated with <code>&</code>.

```nim
var str, str1 = "String"
echo(str & " literal.")
str1 = str1 & " literal."
echo(str1)

# -> String literal.
```


Strings can be concatenated as arrays and joined with a separating characters:

```nim
var str1 = "String"
echo(join([str1, " literal.", "HelloWorld!"], "~~"))   

# -> String~~ literal.~~HelloWorld!
```


Strings can be combined using string formatting:

```nim
var str1 = "String"
echo "$# $# $#" % [str1, "literal.", "HelloWorld!"]

# -> String literal. HelloWorld!
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 STRING$="HELLO"
20 STRING$=STRING$+" WORLD!"
30 PRINT STRING$
```



## Objeck


```objeck
bundle Default {
  class Repeat {
    function : Main(args : String[]) ~ Nil {
      s := "hello";
      s->PrintLine();
      " literal"->PrintLine();
      s->Append(" literal");
      s->PrintLine();
    }
  }
}
```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h


int main()
{
  @autoreleasepool {

    NSString *s = @"hello";
    printf("%s%s\n", [s UTF8String], " literal");
  
    NSString *s2 = [s stringByAppendingString:@" literal"];
    // or, NSString *s2 = [NSString stringWithFormat:@"%@%@", s, @" literal"];
    puts([s2 UTF8String]);
    /* or */
    NSMutableString *s3 = [NSMutableString stringWithString: s];
    [s3 appendString: @" literal"];
    puts([s3 UTF8String]);
  
  }
  return 0;
}
```



## OCaml


```ocaml
let s = "hello"
let s1 = s ^ " literal"
let () =
  print_endline (s ^ " literal");
  print_endline s1
```



## Oforth


.s show the stack : 

```Oforth
"Hello" dup " World!" + .s 
```


{{out}}

```txt

[1] (String) Hello World!
[2] (String) Hello

```



## Openscad


```openscad
a="straw";
b="berry";
c=str(a,b);    /* Concatenate a and b */
echo (c);
```



## Oz

Strings are lists and are concatenated with the "Append" function. However, often "virtual strings" are used instead. [http://www.mozart-oz.org/home/doc/base/virtualstring.html "Virtual string are designed as a convenient way to combine strings, byte strings, atoms, integers and floats to compound strings without explicit concatenation and conversion"].

```oz
declare
S = "hello"
{System.showInfo S#" literal"} %% virtual strings are constructed with "#"
S1 = {Append S " literal"}
{System.showInfo S1}
```



## PARI/GP


```parigp
s = "Hello ";
s = Str(s, "world");
\\ Alternately, this could have been:
\\ s = concat(s, "world");
print(s);
```



## Pascal


```pascal
Program StringConcat;
  Var
     s, s1   : String;
  
Begin
    s := 'hello';
    writeln(s + ' literal');
    s1 := concat(s, ' literal');
    { s1 := s + ' literal'; works too, with FreePascal }
    writeln(s1);
End.
```



## Perl


```perl
my $s = 'hello';
print $s . ' literal', "\n";
my $s1 = $s . ' literal';
print $s1, "\n";
```

An example of destructive concatenation:

```perl
$s .= ' literal';
print $s, "\n";
```



## Perl 6

{{works with|Rakudo|#22 "Thousand Oaks"}}

```perl6
my $s = 'hello';
say $s ~ ' literal';
my $s1 = $s ~ ' literal';
say $s1;

# or, using mutating concatenation:

$s ~= ' literal';
say $s;
```

Note also that most concatenation in Perl 6 is done implicitly via interpolation.


## Phix


```Phix
string s1 = "at"                                ?s1
string s2 = "c"&s1                              ?s2
string s3 = "s"&s1                              ?s3
string s4 = "m"&s1                              ?s4
string s5 = "The "&s2&" "&s3&" on the "&s4&"."  ?s5
```

{{out}}

```txt

"at"
"cat"
"sat"
"mat"
"The cat sat on the mat."

```



## PHL


<lang>module stringcat;

extern printf;

@Integer main [
    var a = "hello";
    var b = a + " literal";
    printf("%s\n", b);

    return 0;
]
```



## PHP


```php
<?php
$s = "hello";
echo $s . " literal" . "\n";
$s1 = $s . " literal";
echo $s1 . "\n";
?>
```



## PicoLisp


```PicoLisp
(let Str1 "First text"
   (prinl Str1 " literal")
   (let Str2 (pack Str1 " literal")
      (prinl Str2) ) )
```



## PL/I


```PL/I
declare (s, t) character (30) varying;

s = 'hello from me';
display (s || ' to you.' );
t = s || ' to you all';
display (t);
```



## PowerShell


```powershell
$s = "Hello"
Write-Host $s World.

# alternative, using variable expansion in strings
Write-Host "$s World."

$s2 = $s + " World."
Write-Host $s2
```



## PureBasic


```PureBasic
If OpenConsole()

  s$ = "hello"
  PrintN( s$ + " literal")
  s2$ = s$ + " literal"
  PrintN(s2$)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```



## Python


```python
s1 = "hello"
print s1 + " world"

s2 = s1 + " world"
print s2
```

{{out}}

```txt
hello world
hello world
```

When concatenating many strings, it is more efficient to use the join method of a string object, which takes a list of strings to be joined. The string on which join is called is used as a separator.

```python
s1 = "hello"
print ", ".join([s1, "world", "mom"])

s2 = ", ".join([s1, "world", "mom"])
print s2
```

{{out}}

```txt
hello, world, mom
hello, world, mom
```



## R


```R
hello <- "hello"
paste(hello, "literal") # "hello literal"
hl <- paste(hello, "literal") #saves concatenates string to a new variable
paste("no", "spaces", "between", "words", sep="") # "nospacesbetweenwords"
```



## Racket


```Racket
#lang racket
(define hello "hello")
(displayln hello)

(define world (string-append hello " " "world" "!"))
(displayln world)

;outputs:
;  hello
;  hello world!
```



## Raven


```Raven
# Cat strings
"First string and " "second string" cat print

# Join
[ "First string" "second string" "third string" ] " and " join print

# print
[ "First string" "second string" "third string" ] each print

# Formatted print
"\n" "Third string" "Second string" "First string" "%s %s %s %s" print

# Heredoc
" - NOT!!" as $x
"This is the only way to do it%($x)s" print
```

{{out}}

```txt
First string and second string
First string and second string and third string
First stringsecond stringthird string
First string Second string Third string 

This is the only way to do it - NOT!!
```



## REBOL


```REBOL
s: "hello"
print s1: rejoin [s " literal"]
print s1
```



## Red


```Red>>
str1: "Hello"
>>str2: append str1 " World"
>> print str2
Hello World
>> print str1 
Hello World
```



## REXX


```rexx
s = "hello"
say s "literal" 
t = s "literal"    /* Whitespace between the two strings causes a space in the output */
say t

                        /* The above method works without spaces too */
genus="straw"
say genus"berry"        /* This outputs strawberry */
say genus || "berry"    /* Concatenation using a doublepipe does not cause spaces */
```



## Retro


```Retro
with strings'
"hello" "literal" append puts
```



## Ring


```ring

aString = "Welcome to the "
bString = "Ring Programming Language"

see astring + bString + nl

```



## Ruby


```ruby
s = "hello"
p s + " literal"        #=> "hello literal"
s1 = s + " literal"
p s1                    #=> "hello literal"
s1 << " another" # append to s1
p s1                    #=> "hello literal another"

s = "hello"
p s.concat(" literal")  #=> "hello literal"
p s                     #=> "hello literal"
```



## Rust


```rust
fn main() {
    let s = "hello".to_owned();
    println!("{}", s);
    
    let s1 = s + " world";
    println!("{}", s1);
}
```



## SAS


```sas
data _null_;
   a="Hello,";
   b="World!";
   c=a !! " " !! b;
   put c;
   *Alternative using the catx function;
   c=catx (" ", a, b);
   put c;
run;
```



## Sather


```sather
class MAIN is
  main is
    s ::= "hello";
    #OUT + s + " literal\n";
    s2 ::= s + " literal";
    #OUT + s2 + "\n";
  end;
end;
```



## Scala

Evaluation in a Scala worksheet, to val f2 is an anonymous function assigned.

```scala
  val s = "hello"                                 //> s  : String = hello
  val s2 = s + " world"                           //> s2  : String = hello world
  val f2 = () =>  " !"                            //> f2  : () => String = <function0>

  println(s2 + f2())                              //> hello world !
```



## Scheme


```scheme
(define s "hello")
(display (string-append s " literal"))
(newline)
(define s1 (string-append s " literal"))
(display s1)
(newline)
```



## Scilab

<lang>s1="Hello"
s1+" world!"
s2=s1+" world"
s2

```

{{out}}
<pre style="height:20ex> --> s1="Hello"
 s1  =
 Hello   
 -->s1+" world!"
 ans  =
 Hello world!   
 -->s2=s1+" world!"
 s2  =
 Hello world!   
 -->s2
 Hello world!   
```




## Seed7


```seed7
$ include "seed7_05.s7i";
 
const proc: main is func
  local
    var string: s is "hello";
    var string: s2 is "";
  begin
    writeln(s <& " world");
    s2 := s & " world";
    writeln(s2);
  end func;
```

{{out}}

```txt

hello world
hello world

```



## Sidef


```ruby
var s = 'hello';
say s+' literal';
var s1 = s+' literal';
say s1;
```

An example of destructive concatenation:

```ruby
s += ' literal';
say s;
```


## Simula


```Simula
TEXT PROCEDURE concatenate(head, tail);
    TEXT head, tail;
BEGIN TEXT c;
    c :- blanks(head.length + tail.length);
    c.sub(c.start, head.length) := head; ! putText(), anyone?;
    c.sub(c.start + head.length, tail.length) := tail;
    concatenate:- c;
END;

TEXT stringVariable, another;
stringVariable :- "head ";
another :- concatenate(stringVariable, "and tail");
OutText("stringVariable: """); OutText(stringVariable);
OutText(""", another: "); OutText(another); Outimage;
```



## Slate


```slate
define: #s -> 'hello'.
inform: s ; ' literal'.
define: #s1 -> (s ; ' literal').
inform: s1.
```



## Smalltalk


```smalltalk
|s s1| s := 'hello'.
(s,' literal') printNl.
s1 := s,' literal'.
s1 printNl.
```



## SNOBOL4


```snobol
	greet1 = "Hello, "
	output = greet1
	greet2 = greet1 "World!"
	output = greet2
end
```



## Sparkling


```sparkling
let s1 = "Hello";
let s2 = " world!";
print(s1 .. s2); // prints "Hello world!"
```



## Standard ML


```sml
val s = "hello"
val s1 = s ^ " literal\n"
val () =
  print (s ^ " literal\n");
  print s1
```



## Stata


###  Stata string scalars 


```stata
sca a = "foo"
sca b = "bar"
sca c = a+b
di c
  foobar
```



###  Mata 


```stata
a = "foo"
b = "bar"
c = a+b
c
  foobar
```



## Swift


```swift
let s = "hello"
println(s + " literal")
let s1 = s + " literal"
println(s1)
```



## Tcl


```tcl
set s hello
puts "$s there!"
append s " there!"
puts $s
```

You can also just group the strings to concatenate together at the point where they are used, using Tcl's built-in syntactic concatenation:

```tcl
set s "Hello "
set t "World"
set u "!"
puts $s$t$u  ;# There is nothing special here about using puts; just an example
```


=={{header|TI-83 BASIC}}==

```ti83b
"HELLO"→Str0
Str0+" WORLD!"→Str0
```

{{out}}

```txt
HELLO WORLD!
```


=={{header|TI-89 BASIC}}==

```ti89b
"aard" → sv
Disp sv & "vark"
sv & "wolf" → sv2
```



## TorqueScript


```Torque
%string = "Hello";
echo(%string);
%other = " world!";
echo(%other);
echo(%string @ %other);
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
s = "Hello "
print s, "literal"

s1 = CONCAT (s,"literal")
print s1
```

{{out}}

```txt

Hello literal
Hello literal 

```



## UNIX Shell

{{works with|Bourne Shell}} {{works with|bash}}

```sh
s="hello"
echo "$s literal"
s1="$s literal"    # This method only works with a space between the strings
echo $s1

# To concatenate without the space we need squiggly brackets:
genus='straw'
fruit=${genus}berry  # This outputs the word strawberry
echo $fruit
```



## UnixPipes


```bash
echo "hello" 
 | xargs -n1 -i echo {} literal
```



## Ursa


```ursa
decl string s1 s2
# make s1 contain "hello "
set s1 "hello "

# set s2 to contain s1 and "world"
set s2 (+ s1 "world")

# outputs "hello world"
out s2 endl console
```



## Visual Basic

{{works with|Visual Basic|VB6 Standard}}
works the same as in VBA, see [[String_concatenation#VBA]]


## Visual Basic .NET

'''Platform:''' [[.NET]]
{{works with|Visual Basic .NET|9.0+}}

```vbnet
s = "Hello"
Console.WriteLine(s & " literal")
s1 = s + " literal"
Console.WriteLine(s1)
```



## VBA



```vb

Option Explicit

Sub String_Concatenation()
Dim str1 As String, str2 As String

    str1 = "Rosetta"
    Debug.Print str1
    Debug.Print str1 & " code!"
    str2 = str1 & " code..."
    Debug.Print str2 & " based on concatenation of : " & str1 & " and code..."
End Sub

```

{{out}}

```txt

Rosetta
Rosetta code!
Rosetta code... based on concatenation of : Rosetta and code...
```



## Wee Basic


```Wee Basic
let string1$="Hello "
let string2$="world!"
print 1 string1$+string2$
end
```



## XPL0


```XPL0
func Concat(S1, S2, S3);        \Concatenate strings: S3:= S1 + S2
char S1, S2, S3;
int  C, I, J;
[I:= 0;
repeat  C:= S1(I);
        S3(I):= C & $7F;        \remove MSb terminator from first string
        I:= I+1;
until   C >= $80;
J:= 0;
repeat  C:= S2(J);
        S3(I+J):= C;
        J:= J+1;
until   C >= $80;
return S3;
];

code Text=12;
char A, B, C(80);
[A:= "Hello";
 B:= " World!";
Concat(A, B, C);
Text(0, C);
]
```



## Yorick


```yorick
var1 = "Hello";
var2 = var1 + ", world!";
write, var1;
write, var2;
```



## zkl


```zkl
var s="Hello";
s2:=s+", world!"; s2.println(); //-->Hello, world!
s3:=String(s," ",s2); s3.println(); //-->Hello Hello, world!
```



{{omit from|bc|No string operations in bc}}
{{omit from|dc|No string operations in dc}}
[[Wikipedia::https://en.wikipedia.org/wiki/Comparison_of_programming_languages_%28string_functions%29#Concatenation]]
