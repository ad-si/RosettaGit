+++
title = "String append"
description = ""
date = 2019-10-09T14:32:39Z
aliases = []
[extra]
id = 16419
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:String manipulation]]
[[Category: String manipulation]]
{{basic data operation}} 
[[Category:Simple]]

Most languages provide a way to concatenate two string values, but some languages also provide a convenient way to append in-place to an existing string variable without referring to the variable twice.


;Task:
Create a string variable equal to any text value. 

Append the string variable with another string literal in the most idiomatic way, without double reference if your language supports it. 

Show the contents of the variable after the append operation.





## Ada


```ada

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_Io; use Ada.Text_IO.Unbounded_IO;

procedure String_Append is
   Str : Unbounded_String := To_Unbounded_String("Hello");
begin
   Append(Str, ", world!");
   Put_Line(Str);
end String_Append;

```

{{out}}

```txt

Hello, world!

```



## ALGOL 68

{{works with|ALGOL 68|Revision 1.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.7 algol68g-2.7].}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards).}}
'''File: String_append.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

STRING str := "12345678";
str +:= "9!";
print(str)
```

{{out}}

```txt

123456789!

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program appendstr.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall

.equ BUFFERSIZE,          100

/* Initialized data */
.data
szMessString:            .asciz "String :\n"
szString1:              .asciz "Alphabet : "
sComplement:            .fill BUFFERSIZE,1,0
szString2:              .asciz "abcdefghijklmnopqrstuvwxyz"

szCarriageReturn:       .asciz "\n"

/* UnInitialized data */
.bss 

/*  code section */
.text
.global main 
main: 

    ldr r0,iAdrszMessString                     @ display message
    bl affichageMess
    ldr r0,iAdrszString1                        @ display begin string
    bl affichageMess
    ldr r0,iAdrszCarriageReturn                 @ display line return
    bl affichageMess
    ldr r0,iAdrszString1
    ldr r1,iAdrszString2
    bl append                                   @ append sting2 to string1
    ldr r0,iAdrszMessString
    bl affichageMess
    ldr r0,iAdrszString1                        @ display string
    bl affichageMess 
    ldr r0,iAdrszCarriageReturn
    bl affichageMess

100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc 0                                       @ perform system call
iAdrszMessString:         .int szMessString
iAdrszString1:            .int szString1
iAdrszString2:            .int szString2
iAdrszCarriageReturn:     .int szCarriageReturn
/******************************************************************/
/*     append two strings                         */ 
/******************************************************************/
/* r0 contains the address of the string1 */
/* r1 contains the address of the string2 */
append:
    push {r0,r1,r2,r7,lr}                       @ save  registers 
    mov r2,#0                                   @ counter byte string 1
1:
    ldrb r3,[r0,r2]                             @ load byte string 1
    cmp r3,#0                                   @ zero final ?
    addne r2,#1
    bne 1b                                      @ no -> loop
    mov r4,#0                                   @ counter byte string 2
2:
    ldrb r3,[r1,r4]                             @ load byte string 2
    strb r3,[r0,r2]                             @ store byte string 1
    cmp r3,#0                                   @ zero final ?
    addne r2,#1                                 @ no -> increment counter 1
    addne r4,#1                                 @ no -> increment counter 2
    bne 2b                                      @ no -> loop
100:
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return

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
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return



```



## AutoHotkey


```autohotkey
s := "Hello, "
s .= "world."
MsgBox % s
```

{{out}}
```txt
Hello, world.
```



## AWK


```AWK

# syntax: GAWK -f STRING_APPEND.AWK
BEGIN {
    s = "foo"
    s = s "bar"
    print(s)
    exit(0)
}

```

{{out}}

```txt

foobar

```



## Axe


```axe
Lbl STRCAT
Copy(r₂,r₁+length(r₁),length(r₂)+1)
r₁
Return
```



## BASIC

=
## Applesoft BASIC
=

```BASIC
S$ = "Hello"
S$ = S$ + " World!"
PRINT S$
```

=
## BBC BASIC
=

```BBC BASIC
      S$="Hello"
      S$+=" World!"
      PRINT S$
      END
```

{{out}}

```txt
Hello World!
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET S$="Hello"
110 LET S$=S$&" World!"
120 PRINT S$
```



## Bracmat


```Bracmat
str="Hello";
str$(!str " World!"):?str;
out$!str;
```

{{out}}

```txt
Hello World!
```



## C


```c>#include<stdio.h

#include<string.h>

int main()
{
    char str[24]="Good Morning";
    char *cstr=" to all";
    char *cstr2=" !!!";
    int x=0;
    //failure when space allocated to str is insufficient.

    if(sizeof(str)>strlen(str)+strlen(cstr)+strlen(cstr2))
            {
                /* 1st method*/
                strcat(str,cstr);

                /*2nd method*/
                x=strlen(str);
                sprintf(&str[x],"%s",cstr2);

                printf("%s\n",str);

            }
    return 0;
}
```

{{out}}

```txt
Good Morning to all !!!
```



## C++


```cpp>#include <iostream

#include <string>

int main( ) {
   std::string greeting( "Hello" ) ;
   greeting.append( " , world!" ) ;
   std::cout << greeting << std::endl ;
   return 0 ;
}
```

{{out}}

```txt
Hello , world!
```


=={{header|C sharp|C#}}==

```csharp
class Program
{
    static void Main(string[] args)
    {
        string x = "foo";
        x += "bar";
        System.Console.WriteLine(x);
    }
}
```



## Clojure

Using global vars.

```clojure>user=
 (def s "app")
#'user/s
user=> s
"app"
user=> (def s (str s "end"))
#'user/s
user=> s
"append"
```


Using local bindings.

```clojure

user=> (let [s "ap", s (str s "pend")] s)
"append"
```



## COBOL

COBOL is not really a variable length field programming language.  Most data items are fixed in size at compile time.

This example uses OCCURS DEPENDING ON, and ''reference modification'' to simulate a string append, all within an already maximally bounded character field.  This type of programming task, while possible, is not overly common in COBOL applications.

{{works with|GnuCOBOL}}

```COBOL
      identification division.                                         
       program-id. string-append.                                       

       data division.
       working-storage section.
       01 some-string.
          05 elements pic x occurs 0 to 80 times depending on limiter.
       01 limiter     usage index value 7.
       01 current     usage index.

       procedure division.
       append-main.

       move "Hello, " to some-string

      *> extend the limit and move using reference modification
       set current to length of some-string
       set limiter up by 5
       move "world" to some-string(current + 1:)
       display some-string

       goback.
       end program string-append.

```

{{out}}

```txt
$ cobc -xj string-append.cob
Hello, world
```


=={{Header|CoffeeScript}}==
{{works with|Node.js}}

```coffeescript
a = "Hello, "
b = "World!"
c = a + b

console.log c
```

Or with concat:

```coffeescript
console.log "Hello, ".concat "World!"
```

{{out}}

```txt
Hello, World!
```



## Common Lisp

Similar to the [[String append#Racket| Racket]] solution, a macro is necessary to append in-place:

```lisp
(defmacro concatenatef (s &rest strs) 
  "Append additional strings to the first string in-place."
  `(setf ,s (concatenate 'string ,s ,@strs)))
(defvar *str* "foo")
(concatenatef *str* "bar")
(format T "~a~%" *str*)
(concatenatef *str* "baz" "abc" "def")
(format T "~a~%" *str*)
```


Output:

```txt
foobar
foobarbazabcdef
```



## D


```d
import std.stdio;

void main() {
    string s = "Hello";
    s ~= " world!"; 
    writeln(s);
}
```

{{out}}

```txt
Hello world!
```



## Dyalect



```Dyalect
var s = "foo"
s += "bar"
print(s)
```



## EasyLang

<lang>a$ = "hello"
a$ &= " world"
print a$
```



## EchoLisp


```lisp

;; Solution from Common Lisp and Racket
(define-syntax-rule (set-append! str tail) 
   (set! str (string-append str tail)))

(define name "Albert") → name

(set-append! name " de Jeumont-Schneidre")
name
   → "Albert de Jeumont-Schneidre"

```



## Elena

ELENA 4.x :

```elena
import extensions;
import extensions'text;

public program()
{
    var s := StringWriter.load("Hello");
    s.append:" World";
     
    console.printLine:s.readChar()
}
```



## Elixir


```elixir
iex(60)> s = "Hello"
"Hello"
iex(61)> s <> " World!"
"Hello World!"
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

(setq str "Hello, ")
(setq str (glue str "World!") )
(insert str)

```

<b>Output:</b>

```txt

Hello, World!

```




## Erlang

{{out}}

```txt

1> S = "Hello".
"Hello"
2> S ++ " world".
"Hello world"

```




## Euphoria


```euphoria

sequence string = "String"

printf(1,"%s\n",{string})

string &= " is now longer\n"

printf(1,"%s",{string})

```

{{out}}

```txt

String
String is now longer

```


=={{header|F_Sharp|F#}}==
Strings are immutable in .NET. To append (to the same variable) the variable has to be declared mutable.

```fsharp
let mutable x = "foo"
x <- x + "bar"
printfn "%s" x
```



## Factor


```factor
"Hello, " "world!" append
```

{{out}}

```txt

"Hello, world!"

```




## Falcon


```falcon

/* Added by Aykayayciti Earl Lamont Montgomery
April 10th, 2018 */

s1, s2 = "Hello", "Foo"
> s1 + " World"
printl(s2 + " bar")

```

{{out}}

```txt

Hello World
Foo bar
[Finished in 0.2s]

```



## Forth


```Forth
\ Strings in Forth are simply named memory locations

create astring  256 allot   \ create a "string"

s" Hello " astring PLACE     \ initialize the string

s" World!" astring +PLACE   \ append with "+place"
```


Test at the console

<lang>  ok
s" Hello " astring place  ok
s" World!" astring +place  ok
astring count type Hello World! ok

```



## Fortran


Using deferred length character strings:


```Fortran

program main

 character(len=:),allocatable :: str

 str = 'hello'
 str = str//' world'

 write(*,*) str

end program main

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Var s = "String"
s += " append"
Print s 
Sleep
```


{{out}}

```txt

String append

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=0b17e205d56985c8cd1ff108c6fc9ca4 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String = "Hello "

sString &= "World!"
Print sString

End
```

Output:

```txt

Hello World!

```



## Genie


```genie
[indent=4]
/* String append, in Genie */
init
    str:string = "Hello"
    str += ", world"

    print str
```

{{out}}

```txt
prompt$ valac stringAppend.gs
prompt$ ./stringAppend
Hello, world
```



## GlovePIE


```glovepie
var.string="This is "
var.string+="Sparta!"
debug=var.string
```



## Go


```go
s := "foo"
s += "bar"
```



###  String Builder 

The first solution redefines the string variable every time. It might be short in code but it uses much CPU cycles. A better way is to use `string.Builder` but it is not a string. It is more like a buffer which can produce a string. And it really appends the string to the existing variable.

```go

package main

import (
  "fmt"
  "strings"
)

func main() {
  var s strings.Builder
  s.WriteString("foo")
  s.WriteString("bar")
  fmt.Print(s.String())
}

```

{{out}}
foobar


## Gosu


```gosu
// Example 1
var s = "a"
s += "b"
s += "c"
print(s)

// Example 2
print("a" + "b" + "c")

// Example 3
var a = "a"
var b = "b"
var c = "c"
print("${a}${b}${c}")
```

{{out}}

```txt

abc
abc
abc

```




## Groovy


```Groovy

class Append{
static void main(String[] args){
def c="Hello ";
def d="world";
def e=c+d;
println(e);
}
}

```

{{out}}

```txt

Hello world

```



## Haskell



```haskell

main = putStrLn ("Hello" ++ "World")

```


=={{header|Icon}} and {{header|Unicon}}==
In both languages you can:


```unicon

procedure main()
    s := "foo"
    s ||:= "bar"
    write(s)
end

```


Outputs:


```txt

->ss
foobar
->

```



## J


```j
   s=: 'new'
   s
new
   s=: s,' value'   NB. append is in-place
   s
new value
```



## Java


```Java
String sa = "Hello";
sa += ", World!";
System.out.println(sa);

StringBuilder ba = new StringBuilder();
ba.append("Hello");
ba.append(", World!");
System.out.println(ba.toString());
```

{{out}}

```txt

Hello, World!
Hello, World!

```



## JavaScript

{{works with|Rhino}}
{{works with|SpiderMonkey}}

```JavaScript
var s1 = "Hello";
s1 += ", World!";
print(s1);

var s2 = "Goodbye";
// concat() returns the strings together, but doesn't edit existing string
// concat can also have multiple parameters
print(s2.concat(", World!"));
```

{{out}}

```txt

"Hello, World!"
"Goodbye, World!"

```


## jq

jq's <code>+</code> operator can be used to append two strings, and under certain circumstances the <code>+=</code> operator can be used as an abbreviation for appending a string to an existing string.  For example, all three of the following produce the same output:
```jq
"Hello" | . += ", world!"

["Hello"] | .[0] += ", world!" | .[0]

{ "greeting": "Hello"} | .greeting += ", world!" | .greeting
```

However the <code>+=</code> operator cannot be used with jq variables in the conventional manner. One could nevertheless use the technique illustrated by the following:
```jq
"Hello" as $a | $a | . += ", world!" as $a | $a
```



## Jsish

From Javascript entry.

```javascript
/* String append, in Jsish */
var str = 'Hello';
;str += ', world';

var s2 = 'Goodbye';
;s2.concat(', World!');

/*
=!EXPECTSTART!=
str += ', world' ==> Hello, world
s2.concat(', World!') ==> Goodbye, World!
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U stringAppend.jsi
str += ', world' ==> Hello, world
s2.concat(', World!') ==> Goodbye, World!
```



## Julia


```julia
s = "Hello"
s *= ", world!"
```

{{out}}

```txt
"Hello, world!"
```



## Kotlin


```kotlin
fun main(args: Array<String>) {
    var s = "a"
    s += "b"
    s += "c"
    println(s)
    println("a" + "b" + "c")
    val a = "a"
    val b = "b"
    val c = "c"
    println("$a$b$c")
}
```

{{out}}

```txt
abc
abc
abc
```



## Lasso


```Lasso
local(x = 'Hello')
#x->append(', World!')
#x
```

{{out}}

```txt
Hello, World!
```



## Lingo


```lingo
str = "Hello" 
put " world!" after str
put str
-- "Hello world!"
```



## LiveCode

Livecode has an "after" keyword for this

```LiveCode
local str="live"
put "code" after str
```

Output is "livecode"


## Lua

Not possible as strings are immutable.  We can demonstrate their immutability using 'self':

```Lua
function string:show ()
    print(self)
end

function string:append (s)
    self = self .. s
end

x = "Hi "
x:show()
x:append("there!")
x:show()
```

{{out}}

```txt
Hi 
Hi 
```

You can of course concatentate them and store the result in the original variable name but that requires a double reference:

```Lua
x = "Hi "
x = x .. "there!"
print(x)
```

{{out}}

```txt
Hi there!
```



## M2000 Interpreter

Documents in M2000 are objects with paragraphs.


```M2000 Interpreter

a$="ok"
a$+="(one)"
Print a$

Document b$
b$="ok"
b$="(one)"
Print b$ 

```

{{out}}

```txt
ok(one)
ok(one)

```



## Maple


```maple
a := "Hello";
b := cat(a, " World");
c := `||`(a, " World");
```

{{out}}

```txt

                            "Hello"
                         "Hello World"
                         "Hello World"

```



## Mathematica


```Mathematica

(* mutable strings are not supported *)
s1 = "testing";
s1 = s1 <> " 123";
s1
```

{{out}}

```txt
"testing 123"
```



## MontiLang


```MontiLang
|Hello | |world!| swap + print
```


```MontiLang
|Hello | var hello .
|world!| var world .
world hello + print
```



## Neko

The plus operator +, concats strings.
 

```ActionScript
/**
 <doc><p>String append in Neko
```
</doc>
**/

var str = "Hello"
str += ", world"
$print(str, "\n")
```

{{out}}

```txt
prompt$ nekoc string-append.neko
prompt$ neko ./string-append.n
Hello, world
```



## NetRexx


```NetRexx
s_ = 'Hello'
s_ = s_', world!'
say s_
```

{{out}}

```txt

Hello, world!

```



## NewLISP


```NewLISP
(setq str "foo")

(push "bar" str -1)
; or as an alternative introduced in v.10.1
(extend str "bar")

(println str)

```



## Nim


```nim
var str = "123456"
str.add("78") # two ways
str &= "9!" # to append
```



## Objeck


```objeck

class Append {
  function : Main(args : String[]) ~ Nil {
    x := "foo";
    x->Append("bar");
    x->PrintLine();
  }
}

```



## OCaml


```ocaml
let () =
  let s = Buffer.create 17 in
  Buffer.add_string s "Bonjour";
  Buffer.add_string s " tout le monde!";
  print_endline (Buffer.contents s)
```

{{out}}

```txt
Bonjour tout le monde!
```




## Oforth



```Oforth
StringBuffer new "Hello, " << "World!" << println
```



## PARI/GP

Not supported in GP.

```parigp
s = "Hello";
s = Str(s, ", world!")
```

{{out}}

```txt
%1 = "Hello, world!"
```



## Pascal


{{works with|Free Pascal|2.6.2}}


```Pascal
program StringAppend;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this };

var
    s: String = 'Hello';
begin
  s += ' World !';
  WriteLn(S);
  ReadLn;
end.
```

Output:

```txt
Hello  World !
```



## Perl


```perl
my $str = 'Foo';
$str .= 'bar';
print $str;
```

{{out}}

```txt
Foobar
```



## Perl 6


```perl6
my $str = "foo";
$str ~= "bar";
say $str;
```

{{out}}

```txt
foobar
```



## Phix


```Phix
string s = "this string"        ?s
s &= " is now longer"           ?s
```

{{out}}

```txt

"this string"
"this string is now longer"

```



## PicoLisp


```picolisp
(setq Str1 "12345678")
(setq Str1 (pack Str1 "9!"))
(println Str1)
```

{{out}}

```txt
"123456789!"
```



## PL/I


```PL/I
Cat: procedure options (main);
   declare s character (100) varying;
   s = 'dust ';
   s ||= 'bowl';
   put (s);
end Cat;
```


```txt
dust bowl
```




## plainTeX

Works with any TeX engine

```tex
\def\addtomacro#1#2{\expandafter\def\expandafter#1\expandafter{#1#2}}
\def\foo{Hello}
Initial: \foo

\addtomacro\foo{ world!}
Appended: \foo
\bye
```


pdf or dvi output:

```txt
Initial: Hello
Appended: Hello world!

```



## PowerShell


```PowerShell

$str = "Hello, "
$str += "World!"
$str

```


```txt
Hello, World!
```



## PureBasic


```purebasic
S$ = "Hello"
S$ = S$ + " Wo" ;by referencing the string twice
S$ + "rld!"     ;by referencing the string once
If OpenConsole()
  PrintN(S$)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
Hello World!
```



## Python

'''File: String_append.py'''
```python
#!/usr/bin/env python
# -*- coding: utf-8 -*- #

str = "12345678";
str += "9!";
print(str)
```

{{out}}

```txt

123456789!

```



## Racket


```racket
;there is no built-in way to set! append in racket
(define mystr "foo")
(set! mystr (string-append mystr " bar"))
(displayln mystr)

;but you can create a quick macro to solve that problem
(define-syntax-rule (set-append! str value)
  (set! str (string-append str value)))

(define mymacrostr "foo")
(set-append! mymacrostr " bar")
(displayln mystr)
```


{{out}}

```txt

foo bar
foo bar

```



## REXX


### using abutment


```rexx
s='he'
s=s'llo world!'
Say s
```

'''output'''

```txt

hello world!

```



### using concatenation


```rexx
s="He"
s=s || 'llo, World!'       /*same as:   s=s||'llo, World!'    */
say s
```
 
'''output'''

```txt

Hello, World!

```



## Ring


```ring

aString1 = "Welcome to the "
aString2 = "Ring Programming Language"
aString3 = aString1 + aString2
see aString3

```



## Robotic


```robotic

set "$str1" to "Hello "
inc "$str1" by "world!"
* "&$str1&"
end

```



## Ruby


```ruby
s = "Hello wo"
s += "rld" # new string object
s << "!"   # mutates in place, same object
puts s
```

{{out}}
```txt
Hello world!
```




## Rust


```rust

use std::ops::Add;

fn main(){
    let hello = String::from("Hello world");
    println!("{}", hello.add("!!!!"));
}
```

{{out}}
Hello world!!!!


###  Real append 

The first solution doesn't append to the string variable. This solution really appends to the existing variable.

```rust

fn main(){
    let mut hello = String::from("Hello world");
    hello.push_str("!!!!");
    println!("{}", hello);
}

```

{{out}}
Hello world!!!!


## Scala

An evaluation in Scala worksheet.

```scala
  var d = "Hello" // Mutables are discouraged     //> d  : String = Hello
  d += ", World!" // var contains a totally new re-instantiationed String

  val s = "Hello" // Immutables are recommended   //> s  : String = Hello
  val s1 = s + s                                  //> s1  : String = HelloHello
  val f2 = () => " !" //Function assigned to variable
                                                  //> f2  : () => String = <function0>
  println(s1 + f2());                             //> HelloHello !
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var string: str is "12345678";
  begin
    str &:= "9!";
    writeln(str);
  end func;
```


{{out}}

```txt

123456789!

```



## Sidef


```ruby
var str = 'Foo';
str += 'bar';
say str;
```

{{out}}

```txt
Foobar
```



## SNOBOL4


```SNOBOL4
     s = "Hello"
     s = s ", World!"
     OUTPUT = s
END
```

{{out}}

```txt
Hello, World!
```



## Stata


```stata
sca s="Ars Longa"
sca s=s+" Vita Brevis"
di s

Ars Longa Vita Brevis
```



## Swift


```swift
var s = "foo"              // "foo"
s += "bar"                 // "foobar"
print(s)                   // "foobar"
s.appendContentsOf("baz")  // "foobarbaz"
print(s)                   // "foobarbaz"
```



## Tcl

String concatenation is a fundamental feature of the Tcl language, and there is also an <code>append</code> that makes concatenation even simpler:

```tcl
set s "he"
set s "${s}llo wo";   # The braces distinguish varname from text to concatenate
append s "rld"
puts $s
```

{{out}}
```txt
hello world
```



## Ursa


```ursa
decl string str
set str "hello "

# append "world" to str
set str (+ str "world")

# outputs "hello world"
out str endl console
```



## VBA


```VB
Function StringAppend()
Dim s As String
s = "foo"
s = s & "bar"
Debug.Print s
End Function
```



## VBScript


```vb
s = "Rosetta"
s = s & " Code"
WScript.StdOut.Write s
```

{{out}}

```txt
Rosetta Code
```



## Wart


```python
s <- "12345678"
s <- (s + "9!")
```



## zkl

zkl strings are immutable, but byte blobs are mutable.

```zkl
var s="foo";
s.append("bar"); //-->new string "foobar", var s unchanged
s+="bar";        //-->new string "foobar", var s modifed to new value

s=Data(Void,"foo");	// byte blob/character blob/text editor buffer
s.append("bar");  // or s+="bar"
s.text; //-->"foobar"
```


{{omit from|bc|No string operations in bc}}
{{omit from|dc|No string operations in dc}}
