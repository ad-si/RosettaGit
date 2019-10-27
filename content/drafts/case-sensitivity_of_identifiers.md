+++
title = "Case-sensitivity of identifiers"
description = ""
date = 2019-10-13T14:11:54Z
aliases = []
[extra]
id = 9220
[taxonomies]
categories = []
tags = []
+++

{{task|Case Sensitivity}}

Three dogs (Are there three dogs or one dog?) is a code snippet used to illustrate the lettercase sensitivity of the programming language. For a case-sensitive language, the identifiers dog, Dog and DOG are all different and we should get the output:

```txt

The three dogs are named Benjamin, Samba and Bernie.

```

For a language that is lettercase insensitive, we get the following output:

```txt

There is just one dog named Bernie.

```



;Related task:
* [[Unicode variable names]]





## 11l

11l identifiers are case sensitive.

```11l
V dog = ‘Benjamin’
V Dog = ‘Samba’
V DOG = ‘Bernie’
print(‘The three dogs are named ’dog‘, ’Dog‘ and ’DOG‘.’)
```



## Ada

case insensitive

```Ada
with Ada.Text_IO;
procedure Dogs is
   Dog : String := "Bernie";
begin
   Ada.Text_IO.Put_Line ("There is just one dog named " & DOG);
end Dogs;
```


Output:

```txt
There is just one dog named Bernie
```



## Agena

Translation of Algol W. Agena is case sensitive, as this example demonstrates. Tested with Agena 2.9.5 Win32

```agena
scope
    local dog := "Benjamin";
    scope
        local Dog := "Samba";
        scope
            local DOG := "Bernie";
            if DOG <> Dog or DOG <> dog
            then print( "The three dogs are named: " & dog & ", " & Dog & " and " & DOG )
            else print( "There is just one dog named: " & DOG )
            fi
        epocs
    epocs
epocs
```

{{out}}

```txt

The three dogs are named: Benjamin, Samba and Bernie

```



## Aime


```aime
text dog, Dog, DOG;

dog = "Benjamin";
Dog = "Samba";
DOG = "Bernie";

o_form("The three dogs are named ~, ~ and ~.\n", dog, Dog, DOG);
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.6 algol68g-2.6].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
A joke code entry... :-) ¢ but the code does actually work!
'''File: Case-sensitivity_of_identifiers.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

STRING dog = "Benjamin";
OP D = (INT og)STRING: "Samba"; 
OP DOG = (INT gy)STRING: "Bernie";
INT og=~, gy=~;
 
main:(
  printf(($"The three dogs are named "g", "g" and "g"."l$, dog, Dog, DOGgy));
  0
)
```
'''Output:'''

```txt

The three dogs are named Benjamin, Samba and Bernie.

```


Alternative version.


{{works with|Rutgers_ALGOL_68|Any - Tested with the DOS version}}
{{Trans|Algol W}}
Most recent implementations of Algol 68 use "upper stropping", the "keywords" are in upper case and the identifiers are an lower case. This precludes use of e.g. Dog or DOG as the name of a variable or constant.


However, depending on the "stropping" convention used and the implementation, Algol 68 can be case-sensitive. Rutgers ALGOL 68 uses quote stropping and allows both upper and lower case in identifiers and bold words. The standard bold words must be in lower-case.

```algol68
'begin'
    'string' dog = "Benjamin";
    'begin'
        'string' Dog = "Samba";
        'begin'
            'string' DOG = "Bernie";
            'if' DOG /= Dog 'or' DOG /= dog
            'then' print( ( "The three dogs are named: ", dog, ", ", Dog, " and ", DOG ) )
            'else' print( ( "There is just one dog named: ", DOG ) )
            'fi'
        'end'
    'end'
'end'
```


{{out}}

```txt

The three dogs are named: Benjamin, Samba and Bernie

```



## ALGOL W

Algol W identifiers are not case-sensitive but variable names in inner blocks can be the same as those in outer blocks...

```algolw
begin
    string(8) dog;
    dog := "Benjamin";
    begin
        string(8) Dog;
        Dog := "Samba";
        begin
            string(8) DOG;
            DOG := "Bernie";
            if DOG not = Dog
            or DOG not = dog
            then write( "The three dogs are named: ", dog, ", ", Dog, " and ", DOG )
            else write( "There is just one dog named: ", DOG )
        end
    end
end.
```

{{out}}

```txt

There is just one dog named: Bernie  

```



## APL


```apl
      DOG←'Benjamin'
      Dog←'Samba'
      dog←'Bernie'
      'The three dogs are named ',DOG,', ',Dog,', and ',dog
The three dogs are named Benjamin, Samba, and Bernie
```



## AutoHotkey


```AutoHotkey
dog := "Benjamin"
Dog := "Samba"
DOG := "Bernie"
MsgBox There is just one dog named %dOG%
```



## AWK


```awk
BEGIN {
	dog = "Benjamin"
	Dog = "Samba"
	DOG = "Bernie"
	printf "The three dogs are named %s, %s and %s.\n", dog, Dog, DOG
}
```


The three dogs are named Benjamin, Samba and Bernie.


## Batch File


```dos

@echo off

set dog=Benjamin
set Dog=Samba
set DOG=Bernie

echo There is just one dog named %dog%.
pause>nul

```

{{out}}

```txt

There is just one dog named Bernie.

```



## BBC BASIC


```bbcbasic
      dog$ = "Benjamin"
      Dog$ = "Samba"
      DOG$ = "Bernie"
      PRINT "The three dogs are " dog$ ", " Dog$ " and " DOG$ "."
```

Output:

```txt
The three dogs are Benjamin, Samba and Bernie.
```



## bc

The only variables are 'a' through 'z'. They can only hold numbers, not strings. Some implementations allow longer names like 'dog', but only with lowercase letters. A name like 'Dog' or 'DOG' is a syntax error.


```bc
obase = 16
ibase = 16

/*
 * Store the hexadecimal number 'BE27A312'
 * in the variable 'd'.
 */
d = BE27A312
"There is just one dog named "; d
quit
```


There is just one dog named BE27A312


## Bracmat


```Bracmat
( Benjamin:?dog
& Samba:?Dog
& Bernie:?DOG
& out$("There are three dogs:" !dog !Dog and !DOG)
);
```

Output:

```txt
There are three dogs: Benjamin Samba and Bernie
```



## Brlcad


The three dogs are drawn as spheres in this simple example:


```mged

opendb dogs.g y            # Create a database to hold our dogs
units ft                   # The dogs are measured in feet
in dog.s sph 0 0 0 1       # Benjie is a little Scottie dog
in Dog.s sph 4 0 0 3       # Samba is a Labrador
in DOG.s sph 13 0 0 5      # Bernie is massive. He is a New Foundland
echo The three dogs are named Benjamin, Samba and Bernie
```



## C

C is case sensitive; if it would be case insensitive, an error about redefinition of a variable would be raised.


```c>#include <stdio.h


static const char *dog = "Benjamin";
static const char *Dog = "Samba";
static const char *DOG = "Bernie";

int main()
{
    printf("The three dogs are named %s, %s and %s.\n", dog, Dog, DOG);
    return 0;
}
```



## C++

C++ is case-sensitive.

```cpp>#include <iostream

#include <string>
using namespace std;

int main() {
    string dog = "Benjamin", Dog = "Samba", DOG = "Bernie";
    
    cout << "The three dogs are named " << dog << ", " << Dog << ", and " << DOG << endl;
}
```

{{out}}

```txt
The three dogs are named Benjamin, Samba, and Bernie
```



## C sharp

C# is case sensitive

```C sharp

using System;

class Program
{
    static void Main(string[] args)
    {
        string dog = "Benjamin";
        string Dog = "Samba";
        string DOG = "Bernie";
        Console.WriteLine(string.Format("The three dogs are named {0}, {1}, and {2}.", dog, Dog, DOG));
    }
}
```



## Clojure


```txt
user=> (let [dog "Benjamin" Dog "Samba" DOG "Bernie"] (format "The three dogs are named %s, %s and %s." dog Dog DOG))
"The three dogs are named Benjamin, Samba and Bernie."
```



## COBOL

<lang cobol       *>* Case sensitivity of identifiers
       *>* Commented-out lines in the working storage
       *>* are considered as invalid redefinitions
       *>* of ''dog'' that can only be ambiguously
       *>* referenced in the procedure body.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. case-sensitivity.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *>* 01  dog PICTURE X(8) VALUE IS "Benjamin".
       *>* 01  Dog PICTURE X(5) VALUE IS "Samba".
       01  DOG PICTURE X(6) VALUE IS "Bernie".
       PROCEDURE DIVISION.
         DISPLAY
       *>*     "The three dogs are named "
       *>*     dog ", " Dog " and " DOG "."
           "There is just one dog named " DOG "."
         END-DISPLAY
         STOP RUN.
       END PROGRAM case-sensitivity.
```



## CoffeeScript


```coffeescript

dog="Benjamin"
Dog = "Samba"
DOG = "Bernie"
console.log "The three dogs are names #{dog}, #{Dog}, and #{DOG}."

```


output
<lang>
> coffee foo.coffee 
The three dogs are names Benjamin, Samba, and Bernie.

```



## Common Lisp


```lisp
CL-USER> (let* ((dog "Benjamin") (Dog "Samba") (DOG "Bernie"))
	   (format nil "There is just one dog named ~a." dog))
; in: LAMBDA NIL
;     (LET* ((DOG "Benjamin") (DOG "Samba") (DOG "Bernie"))
;       (FORMAT NIL "There is just one dog named ~a." DOG))
; 
; caught STYLE-WARNING:
;   The variable DOG is defined but never used.
; 
; caught STYLE-WARNING:
;   The variable DOG is defined but never used.
; 
; compilation unit finished
;   caught 2 STYLE-WARNING conditions
"There is just one dog named Bernie."
```


These are the style warnings from [[SBCL]]. Other implementations of Common Lisp might give different warnings.


## D


```d
import std.stdio;

void main() {
    string dog = "Benjamin";
    // identifiers that start with capital letters are type names
    string Dog = "Samba";
    string DOG = "Bernie";
    writefln("There are three dogs named ",
             dog, ", ", Dog, ", and ", DOG, "'");
}
```

Output:

```txt
There are three dogs named Benjamin, Samba, and Bernie'
```



## dc

A register name has only one character, so this example uses 'd' and 'D'.


```dc
[Benjamin]sd
[Samba]sD
[The two dogs are named ]P ldP [ and ]P lDP [.
]P
```


{{Out}}

```txt
The two dogs are named Benjamin and Samba.
```



## Delphi

Delphi is case insensitive.


```Delphi
program CaseSensitiveIdentifiers;

{$APPTYPE CONSOLE}

var
  dog: string;
begin
  dog := 'Benjamin';
  Dog := 'Samba';
  DOG := 'Bernie';
  Writeln('There is just one dog named ' + dog);
end.
```


Output:

```txt
There is just one dog named Bernie
```


=={{header|Déjà Vu}}==

```dejavu
local :dog "Benjamin"
local :Dog "Samba"
local :DOG "Bernie"
 
!print( "There are three dogs named " dog ", " Dog " and " DOG "." )
```

{{out}}

```txt
There are three dogs named Benjamin, Samba and Bernie.
```



## DWScript


```Delphi

var dog : String;

dog := 'Benjamin';
Dog := 'Samba';
DOG := 'Bernie';

PrintLn('There is just one dog named ' + dog);
```


Output:

```txt
There is just one dog named Bernie
```



## EchoLisp


```scheme

(define dog "Benjamin")
(define Dog "Samba")
(define DOG "Bernie")

(printf "The three dogs are named %a, %a and %a. " dog Dog DOG)
    The three dogs are named Benjamin, Samba and Bernie. 

```


## Elena

In ELENA identifiers are case sensitive. 
ELENA 4.x:

```elena
import extensions;

public program()
{
    var dog := "Benjamin";
    var Dog := "Samba";
    var DOG := "Bernie";
    console.printLineFormatted("The three dogs are named {0}, {1} and {2}", dog, Dog, DOG)
}
```

{{out}}

```txt

The three dogs are named Benjamin, Samba and Bernie

```



## Elixir

While Elixir's identifiers are case-sensitive, they generally must start with a lowercase letter. Capitalized identifiers are reserved for modules.

```elixir
dog = "Benjamin"
doG = "Samba"
dOG = "Bernie"
IO.puts "The three dogs are named #{dog}, #{doG} and #{dOG}."
```


{{out}}

```txt

The three dogs are named Benjamin, Samba and Bernie.

```



## Erlang

Erlang variables are case sensitive but must start with an uppercase letter.

```Erlang

-module( case_sensitivity_of_identifiers ).

-export( [task/0] ).

task() ->
	catch dog = "Benjamin", % Function will crash without catch
	Dog = "Samba",
	DOG = "Bernie",
	io:fwrite( "The three dogs are named ~s, ~s and ~s~n", [dog, Dog, DOG] ).

```


{{out}}

```txt

4> case_sensitivity_of_identifiers:task().
The three dogs are named dog, Samba and Bernie

```



## Euphoria

{{works with|Euphoria|4.0.0}}

```Euphoria
-- These variables are all different
sequence dog = "Benjamin"
sequence Dog = "Samba"
sequence DOG = "Bernie"
printf( 1, "The three dogs are named %s, %s and %s\n", {dog, Dog, DOG} )
```



## Factor

Factor identifiers are case-sensitive.

```factor
USING: formatting locals ;
IN: scratchpad
[let
    "Benjamin" :> dog
    "Samba"    :> Dog
    "Bernie"   :> DOG
    { dog Dog DOG } "There are three dogs named %s, %s, and %s." vprintf
]
```

{{out}}

```txt

There are three dogs named Benjamin, Samba, and Bernie.

```



## Forth


```forth
: DOG ." Benjamin" ;
: Dog ." Samba" ;
: dog ." Bernie" ;
: HOWMANYDOGS ." There is just one dog named " DOG ;
HOWMANYDOGS
```

{{out}}

```txt
There is just one dog named Bernie
```



## Fortran

{{works with|Fortran|90 and later}}
Fortran is case insensitive, and so the three "dog" variants name the same variable - which therefore is multiply declared and will likely evoke a compiler complaint.

```fortran
program Example
  implicit none

  character(8) :: dog, Dog, DOG

  dog = "Benjamin"
  Dog = "Samba"
  DOG = "Bernie"

  if (dog == DOG) then
    write(*,*) "There is just one dog named ", dog
  else
    write(*,*) "The three dogs are named ", dog, Dog, " and ", DOG
  end if

end program Example
```

Output:

```txt
 There is just one dog named Bernie
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' FreeBASIC is case-insensitive
Dim dog As String 
dog = "Benjamin"
Dog = "Samba"
DOG = "Bernie"
Print "There is just one dog, named "; dog
Sleep
```


{{out}}

```txt

There is just one dog, named Bernie

```



## Frink

Frink is case-sensitive.

```frink
dog = "Benjamin"
Dog = "Samba"
DOG = "Bernie"
println["There are three dogs named $dog, $Dog and $DOG"]
```


=={{header|F Sharp|F#}}==
F# is case-sensitive.

```fsharp
let dog = "Benjamin"
let Dog = "Samba"
let DOG = "Bernie"
printfn "There are three dogs named %s, %s and %s" dog Dog DOG
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=fed58074944b894f5d4cfb8e16c6819a Click this link to run this code]'''

Gambas in case insensitive

```gambas
Public Sub Main()
Dim dog As String 

Dog = "Benjamin"
DOG = "Samba"
dog = "Bernie"
Print "There is just one dog, named "; dog

End
```

Output:

```txt

There is just one dog, named Bernie

```



## GAP


```gap
# GAP is case sensitive
ThreeDogs := function()
	local dog, Dog, DOG;
	dog := "Benjamin";
	Dog := "Samba";
	DOG := "Bernie";
	if dog = DOG then
		Print("There is just one dog named ", dog, "\n");
	else
		Print("The three dogs are named ", dog, ", ", Dog, " and ", DOG, "\n");
	fi;
end;

ThreeDogs();
# The three dogs are named Benjamin, Samba and Bernie
```



## Go

Go is case sensitive.  Further, visibility depends on case. See the Go entry under the [[Scope_modifiers#Go|Scope modifiers]] task.

```go
package dogs

import "fmt"

// Three variables, three different names.
// (It wouldn't compile if the compiler saw the variable names as the same.)
var dog = "Salt"
var Dog = "Pepper"
var DOG = "Mustard"

func PackageSees() map[*string]int {
    // Print dogs visible from here.
    fmt.Println("Package sees:", dog, Dog, DOG)
    // Return addresses of the variables visible from here.
    // The point of putting them in a map is that maps store only
    // unique keys, so it will end up with three items only if
    // the variables really represent different places in memory.
    return map[*string]int{&dog: 1, &Dog: 1, &DOG: 1}
}
```


```go
package main

import (
    . "dogs"
    "fmt"
)

func main() {
    // with the dogs package imported, there are three dogs.
    d := PackageSees()
    fmt.Println("There are", len(d), "dogs.\n")

    // Declaration of new variable dog.  It lives in this package, main.
    dog := "Benjamin"
    d = PackageSees()
    fmt.Println("Main sees:   ", dog, Dog, DOG)
    // Four dogs now.  two of the three visible from here are the
    // the same as ones in the dogs package.
    d[&dog] = 1
    d[&Dog] = 1
    d[&DOG] = 1
    fmt.Println("There are", len(d), "dogs.\n")

    // Not a declaration, just an assigment.  This assigns a new value to
    // the variable Dog declared in the package.  Dog is visible because
    // it begins with an upper case letter.
    Dog = "Samba"
    // same four dogs, same three visible, one just has a new name.
    d = PackageSees()
    fmt.Println("Main sees:   ", dog, Dog, DOG)
    d[&dog] = 1
    d[&Dog] = 1
    d[&DOG] = 1
    fmt.Println("There are", len(d), "dogs.\n")

    // Of course you can still declare a variable if you want to.  This
    // declares a new variable, shadowing DOG in the package and rendering
    // it inaccessable even though it begins with an upper case letter.
    var DOG = "Bernie"
    // five dogs now.  three visible from here.
    d = PackageSees()
    fmt.Println("Main sees:   ", dog, Dog, DOG)
    d[&dog] = 1
    d[&Dog] = 1
    d[&DOG] = 1
    fmt.Println("There are", len(d), "dogs.")
}
```

{{out}}

```txt

Package sees: Salt Pepper Mustard
There are 3 dogs.

Package sees: Salt Pepper Mustard
Main sees:    Benjamin Pepper Mustard
There are 4 dogs.

Package sees: Salt Samba Mustard
Main sees:    Benjamin Samba Mustard
There are 4 dogs.

Package sees: Salt Samba Mustard
Main sees:    Benjamin Samba Bernie
There are 5 dogs.

```



## Groovy

Solution:

```groovy
def dog = "Benjamin", Dog = "Samba", DOG = "Bernie"
println (dog == DOG ? "There is one dog named ${dog}" : "There are three dogs named ${dog}, ${Dog} and ${DOG}.")
```


Output:

```txt
There are three dogs named Benjamin, Samba and Bernie.
```



## Haskell

Identifiers are case sensitive in Haskell, but must start with a lower case letter.


```Haskell
import Text.Printf

main = printf "The three dogs are named %s, %s and %s.\n" dog dOG dOg
    where dog = "Benjamin"
          dOG = "Samba"
          dOg = "Bernie"
```


=={{header|Icon}} and {{header|Unicon}}==
The program below demonstrates the three dog task.  All variants of Icon/Unicon have case sensitive variable names. But if one wasn't this would find it.

```Icon
procedure main()

   dog := "Benjamin"
   Dog := "Samba"
   DOG := "Bernie"
	
   if dog == DOG then 
      write("There is just one dog named ", dog,".") 
   else 
      write("The three dogs are named ", dog, ", ", Dog, " and ", DOG, ".")

end
```



## J


```j
   NB. These variables are all different
   dog=: 'Benjamin'
   Dog=: 'Samba'
   DOG=: 'Bernie'
   'The three dogs are named ',dog,', ',Dog,', and ',DOG
The three dogs are named Benjamin, Samba, and Bernie 
```



## Java


```java
String dog = "Benjamin";
String Dog = "Samba"; //in general, identifiers that start with capital letters are class names
String DOG = "Bernie"; //in general, identifiers in all caps are constants
//the conventions listed in comments here are not enforced by the language
System.out.println("There are three dogs named " + dog + ", " + Dog + ", and " + DOG + "'");
```



## JavaScript

Javascript is case sensitive.

```javascript
var dog = "Benjamin";
var Dog = "Samba";
var DOG = "Bernie";
document.write("The three dogs are named " + dog + ", " + Dog + ", and " + DOG + ".");
```



## jq

jq identifiers are case-sensitive.

'''Function parameters''':

```jq
def task(dog; Dog; DOG):
 "The three dogs are named \(dog), \(Dog), and \(DOG)." ;

task("Benjamin"; "Samba"; "Bernie")
```


{{Out}}
 $ jq -n -f Case-sensitivity.jq
 "The three dogs are named Benjamin, Samba, and Bernie."

'''Variable names''':

```jq
"Benjamin" as $dog | "Samba" as $Dog | "Bernie" as $DOG
 | "The three dogs are named \($dog), \($Dog), and \($DOG)."
```

{{out}}
As above.


## Julia

{{works with|Julia|0.6}}
Variable names are case sensitive.

```julia
dog, Dog, DOG = "Benjamin", "Samba", "Bernie"

if dog === Dog
    println("There is only one dog, ", DOG)
else
    println("The three dogs are:  ", dog, ", ", Dog, " and ", DOG)
end
```


{{out}}

```txt
The three dogs are:  Benjamin, Samba and Bernie
```


Conventionally, variable names should be all lower case.  Type and Macro names should be capitalized.


## K


```k

  dog: "Benjamin"
  Dog: "Samba"
  DOG: "Bernie"
  "There are three dogs named ",dog,", ",Dog," and ",DOG
"There are three dogs named Benjamin, Samba and Bernie"

```



## Kotlin

Kotlin is case-sensitive though (as in Java) the convention is for local variable names to begin with a lower case letter. The second and third identifiers would therefore be unlikely to be used in practice.

```scala
fun main(args: Array<String>) {
    val dog = "Benjamin"
    val Dog = "Samba"
    val DOG = "Bernie"
    println("The three dogs are named $dog, $Dog and $DOG")
}
```


{{out}}

```txt

The three dogs are named Benjamin, Samba and Bernie

```



## Lasso

Lasso is not case sensitive for names

```Lasso

local(dog = 'Benjamin')
local(Dog = 'Samba')
local(DOG = 'Bernie')

stdoutnl('There is just one dog named ' + #dog)

```

Output:

```txt
There is just one dog named Bernie
```


Same with string comparisons. (Lasso maps can only contain unique keys)

```Lasso

local(dogs = map(
	'dog' = 'Benjamin',
	'Dog' = 'Samba',
	'DOG' = 'Bernie'
))
stdoutnl(#dogs -> size)
```

Output:

```txt
1
```


To get case sensitivity we need to use bytes

```Lasso

local(dogs = map(
	bytes('dog') = 'Benjamin',
	bytes('Dog') = 'Samba',
	bytes('DOG') = 'Bernie'
))

stdoutnl(#dogs -> size)

stdoutnl(#dogs -> find(bytes('Dog')))
```

Output:

```txt
3
Samba 
```



## Liberty BASIC

NB the IDE warns you that there are similar variables named dog$, Dog$ & DOG$

```lb

dog$ = "Benjamin"
Dog$ = "Samba"
DOG$ = "Bernie"
print "The three dogs are "; dog$; ", "; Dog$; " and "; DOG$; "."

end

```

The three dogs are Benjamin, Samba and Bernie.


## Lua


```lua
dog = "Benjamin"
Dog = "Samba"
DOG = "Bernie"

print( "There are three dogs named "..dog..", "..Dog.." and "..DOG.."." )
```


```txt
There are three dogs named Benjamin, Samba and Bernie.
```


## M2000 Interpreter

Labels are case sensitive, but identifiers are not case sensitive.
Keys in Inventory are case sensitive
Types in Enumeration are case sensitive, identifiers are not case sensitive.


```M2000 Interpreter

MoDuLe CheckIT {
      \\ keys as case sensitive if they are strings
      Inventory A= "Dog":=1, "dog":=2,"DOG":="Hello", 100:="Dog"
      Print A("Dog"), A("dog"), A$("DOG"), A$(100)
      
      \\ Enumeration get type as defined (same case)
      Enum Dogs {Benjamin, Samba, Bernie}
      Print Type$(Bernie)="Dogs"
      Print Type$(DOGS)="Dogs"
      m=BenJamiN
      m++
      Print Eval$(m)="Samba"  ' same case as defined
      
      DoG$="Benjamin"
      DOG$="Samba"
      doG$="Bernie"
      PrinT "There is just one dog named "+Dog$+"."
      goto Dog
dog:
      Print "dog"
      Exit
Dog:   
      Print "Dog"   
      GoTo dog
}
Checkit

```



## Maple

<lang>> dog, Dog, DOG := "Benjamin", "Samba", "Bernie":
> if nops( { dog, Dog, DOG } ) = 3 then
>   printf( "There are three dogs named %s, %s and %s.\n", dog, Dog, DOG )
> elif nops( { dog, Dog, DOG } ) = 2 then
>   printf( "WTF? There are two dogs named %s and %s.\n", op( { dog, Dog, DOG } ) )
> else
>   printf( "There is one dog named %s.\n", dog )
> end if:
There are three dogs named Benjamin, Samba and Bernie.
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
dog = "Benjamin"; Dog = "Samba"; DOG = "Bernie";
"The three dogs are named "<> dog <>", "<> Dog <>" and "<> DOG 

-> "The three dogs are named Benjamin, Samba and Bernie"
```


=={{header|MATLAB}} / {{header|Octave}}== 


```Matlab
  dog = 'Benjamin';
  Dog = 'Samba';
  DOG = 'Bernie';

  printf('There are three dogs %s, %s, %s.\n',dog, Dog, DOG); 
```


Output


```txt
  There are three dogs Benjamin, Samba, Bernie.  
```



## Maxima


```maxima
/* Maxima is case sensitive */
a: 1$
A: 2$

is(a = A);
false
```



## min

{{works with|min|0.19.3}}
min's symbols are case sensitive.

```min
"Benjamin" :dog
"Samba" :Dog
"Bernie" :DOG

"There are three dogs named $1, $2, and $3." (dog Dog DOG)=> % print
```

{{out}}

```txt

There are three dogs named Benjamin, Samba, and Bernie.

```



## MiniScript


```MiniScript
dog = "Benjamin"
Dog = "Samba"
DOG = "Bernie"

print "There are three dogs named " + dog + ", " + Dog + " and " + DOG
```


=={{header|Modula-2}}==

```modula2
MODULE  dog;

IMPORT  InOut;

TYPE    String          = ARRAY [0..31] OF CHAR;

VAR     dog, Dog, DOG   : String;

(* No compiler error, so the rest is simple  *)

BEGIN
  InOut.WriteString ("Three happy dogs.");
  InOut.WriteLn
END dog.
```



## Nemerle


```Nemerle
def dog = "Benjamin";
def Dog = "Samba";
def DOG = "Bernie";
WriteLine($"The three dogs are named $dog, $Dog, and $DOG");
```



## NESL

NESL is completely case-insensitive.

```nesl
dog = "Benjamin";
Dog = "Samba";
DOG = "Bernie";
"There is just one dog, named " ++ dog;
```

{{out}}

```txt
it = "There is just one dog, named Bernie" : [char]
```



## NetRexx

NetRexx is not case sensitive:

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

dog = "Benjamin";
Dog = "Samba";
DOG = "Bernie";

if dog == Dog & Dog == DOG & dog == DOG then do
  say 'There is just one dog named' dog'.'
  end
else do
  say 'The three dogs are named' dog',' Dog 'and' DOG'.'
  end

return

```

'''Output:'''

```txt

There is just one dog named Bernie.

```



## Nim


```Nim
var dog = "Benjamin"
Dog = "Samba"
DOG = "Bernie"
echo("There is just one dog named " & doG)
```


=={{header|Oberon-2}}==
{{Works with| oo2c Version 2}}

```oberon2

MODULE CaseSensitivity;
IMPORT
  Out;
VAR
  dog, Dog, DOG: STRING;
BEGIN  
  dog := "Benjamin";
  Dog := "Samba";
  DOG := "Bernie";
  Out.Object("The three dogs are named " + dog + ", " + Dog + " and " + DOG);
  Out.Ln
END CaseSensitivity.

```

{{Out}}

```txt

The three dogs are named Benjamin, Samba and Bernie

```


## Objeck

Objeck is case sensitive


```objeck
class Program {
  function : Main(args : String[]) ~ Nil {
    dog := "Benjamin";
    Dog := "Samba";
    DOG := "Bernie";
    "The three dogs are named {$dog}, {$Dog}, and {$DOG}."->PrintLine();
  }
}
```



## OCaml


Identifiers in OCaml are lettercase sensitive, but the first letter has to be lowercase.


```ocaml
let () =
  let dog = "Benjamin" in
  let dOG = "Samba" in
  let dOg = "Bernie" in
  Printf.printf "The three dogs are named %s, %s and %s.\n" dog dOG dOg
```




## Oforth


Oforth is case-sensitive.


```Oforth
: threeDogs
| dog Dog DOG |

   "Benjamin" ->dog
   "Samba"  ->Dog
   "Bernie" ->DOG

   System.Out "The three dogs are named " << dog << ", " << Dog << " and " << DOG << "." << cr ;
```



## PARI/GP


```parigp
dog="Benjamin";
Dog="Samba";
DOG="Bernie";
printf("The three dogs are named %s, %s, and %s.", dog, Dog, DOG)
```



## Pascal

See [[#Delphi|Delphi]]


## Perl


```perl
# These variables are all different
$dog='Benjamin';
$Dog='Samba';
$DOG='Bernie';
print "The three dogs are named $dog, $Dog, and $DOG \n"
```



## Perl 6


```perl6
my $dog = 'Benjamin';
my $Dog = 'Samba';
my $DOG = 'Bernie';
say "The three dogs are named $dog, $Dog, and $DOG."
```

The only place that Perl�pays any attention to the case of identifiers is that, for certain error messages, it will guess that an identifier starting lowercase is probably a function name, while one starting uppercase is probably a type or constant name.  But this case distinction is merely a convention in Perl, not mandatory:

```perl6
constant dog = 'Benjamin';
sub Dog() { 'Samba' }
my &DOG = { 'Bernie' }
say "The three dogs are named {dog}, {Dog}, and {DOG}."
```



## Phix

Phix is case sensitive

```Phix

sequence dog = "Benjamin"
sequence Dog = "Samba"
sequence DOG = "Bernie"
printf( 1, "The three dogs are named %s, %s and %s\n", {dog, Dog, DOG} )

```

{{out}}

```txt

The three dogs are named Benjamin, Samba and Bernie

```




## PicoLisp


```PicoLisp
(let (dog "Benjamin"  Dog "Samba"  DOG "Bernie")
   (prinl "The three dogs are named " dog ", " Dog " and " DOG) )
```

Output:

```txt
The three dogs are named Benjamin, Samba and Bernie
```



## PL/I


```pli
*process or(!) source xref attributes macro options;
 /*********************************************************************
 * Program to show that PL/I is case-insensitive
 * 28.05.2013 Walter Pachl
 *********************************************************************/
 case: proc options(main);
 Dcl dog Char(20) Var;
 dog = "Benjamin";
 Dog = "Samba";
 DOG = "Bernie";
 Put Edit(dog,Dog,DOG)(Skip,3(a,x(1)));
 End;
```

'''Output'''

```txt
Bernie Bernie Berni
```



## PowerShell

PowerShell is not case sensitive.

```PowerShell

$dog = "Benjamin"
$Dog = "Samba"
$DOG = "Bernie"

"There is just one dog named {0}." -f $dOg

```

{{Out}}

```txt

There is just one dog named Bernie.

```



## Prolog

In Prolog, the initial of a variable must be a uppercase letter. So the task can't be completed but we can write this code :

```Prolog
three_dogs :-
	DoG = 'Benjamin',
	Dog = 'Samba',
	DOG = 'Bernie',
	format('The three dogs are named ~w, ~w and ~w.~n', [DoG, Dog, DOG]).

```

The output is :

```txt
?- three_dogs.
The three dogs are named Benjamin, Samba and Bernie.
true.


```


## PureBasic


```PureBasic
dog$="Benjamin"
Dog$="Samba"
DOG$="Bernie"
Debug "There is just one dog named "+dog$
```



## Python

Python names are case sensitive:

```python>>>
 dog = 'Benjamin'; Dog = 'Samba'; DOG = 'Bernie'
>>> print ('The three dogs are named ',dog,', ',Dog,', and ',DOG)
The three dogs are named  Benjamin ,  Samba , and  Bernie
>>> 
```



## R


```R
dog <- 'Benjamin'
Dog <- 'Samba'
DOG <- 'Bernie'

# Having fun with cats and dogs 
cat('The three dogs are named ')
cat(dog)
cat(', ')
cat(Dog)
cat(' and ')
cat(DOG)
cat('.\n')
# In one line it would be:
# cat('The three dogs are named ', dog, ', ', Dog, ' and ', DOG, '.\n', sep = '')
```


Output:

```txt

The three dogs are named Benjamin, Samba and Bernie.

```



## Racket

The default setting for the Racket reader is to be case sensitive:

```Racket

#lang racket
(define dog "Benjamin")
(define Dog "Samba")
(define DOG "Bernie")
 
(if (equal? dog DOG)
    (displayln (~a "There is one dog named " DOG "."))
    (displayln (~a "The three dogs are named " dog ", " Dog ", and, " DOG ".")))

```

Output:

```txt

The three dogs are named Benjamin, Samba, and, Bernie.

```


If you need case insensitive identifiers, then use #ci to turn on case insensitivity:

```Racket

#lang racket
#ci(module dogs racket
     (define dog "Benjamin")
     (set! Dog "Samba")
     (set! DOG "Bernie")     
     (if (equal? dog DOG)
         (displayln (~a "There is one dog named " DOG "."))
         (displayln (~a "The three dogs are named " dog ", " Dog ", and, " DOG "."))))
(require 'dogs)

```

Output:

```txt

There is one dog named Bernie.

```



## Retro

Retro is case sensitive.


```Retro
: dog  ( -$ )  "Benjamin" ;
: Dog  ( -$ )  "Samba" ;
: DOG  ( -$ )  "Bernie" ;

DOG Dog dog "The three dogs are named %s, %s, and %s.\n" puts
```



## REXX


### simple variables

The REXX language is case insensitive   (with respect to simple variables).

```rexx
/*REXX program demonstrate  case insensitivity  for  simple  REXX  variable names.      */

  /*  ┌──◄── all 3 left─hand side REXX variables are identical (as far as assignments). */
  /*  │                                                                                 */
  /*  ↓                                                                                 */
     dog= 'Benjamin'                             /*assign a   lowercase   variable (dog)*/
     Dog= 'Samba'                                /*   "   "  capitalized     "      Dog */
     DOG= 'Bernie'                               /*   "   an  uppercase      "      DOG */

                              say center('using simple variables', 35, "─")     /*title.*/
                              say

if dog\==Dog | DOG\==dog  then say 'The three dogs are named:'     dog"," Dog 'and' DOG"."
                          else say 'There is just one dog named:'  dog"."

                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

──────using simple variables───────

There is just one dog named: Bernie.

```



### compound variables

However, the REXX language is case sensitive   (with respect to compound variables, or indices).

```rexx
/*REXX program demonstrate case sensitive REXX  index  names  (for compound variables). */

 /*  ┌──◄── all 3 indices (for an array variable)  are unique  (as far as array index). */
 /*  │                                                                                  */
 /*  ↓                                                                                  */
x= 'dog';  dogname.x= "Gunner"                   /*assign an array index,  lowercase dog*/
x= 'Dog';  dogname.x= "Thor"                     /*   "    "   "     "   capitalized Dog*/
x= 'DOG';  dogname.x= "Jax"                      /*   "    "   "     "     uppercase DOG*/
x= 'doG';  dogname.x= "Rex"                      /*   "    "   "     "       mixed   doG*/

                              say center('using compound variables', 35, "═")   /*title.*/
                              say

_= 'dog';  say "dogname.dog="  dogname._         /*display an array index, lowercase dog*/
_= 'Dog';  say "dogname.Dog="  dogname._         /*   "     "   "     "  capitalized Dog*/
_= 'DOG';  say "dogname.DOG="  dogname._         /*   "     "   "     "    uppercase DOG*/
_= 'doG';  say "dogname.doG="  dogname._         /*   "     "   "     "      mixed   doG*/

                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

═════using compound variables══════

dogname.dog= Gunner
dogname.Dog= Thor
dogname.DOG= Jax
dogname.doG= Rex

```



## Ring


```ring

dog = "Benjamin"
doG = "Smokey"
Dog = "Samba"
DOG = "Bernie"
see "The 4 dogs are : " + dog +  ", " + doG + ", " + Dog + " and " + DOG + "."

```



## Ruby

Ruby gives a special meaning to the first letter of a name. A lowercase letter starts a local variable. An uppercase letter starts a constant. So <tt>dog</tt> is a local variable, but <tt>Dog</tt> and <tt>DOG</tt> are constants. To adapt this task to Ruby, I added <tt>dOg</tt> and <tt>doG</tt> so that I have more than one local variable.


```ruby
module FiveDogs
  dog = "Benjamin"
  dOg = "Dogley"
  doG = "Fido"
  Dog = "Samba"   # this constant is FiveDogs::Dog
  DOG = "Bernie"  # this constant is FiveDogs::DOG

  names = [dog, dOg, doG, Dog, DOG]
  names.uniq!
  puts "There are %d dogs named %s." % [names.length, names.join(", ")]
  puts
  puts "The local variables are %s." % local_variables.join(", ")
  puts "The constants are %s." % constants.join(", ")
end
```


Output: 
```txt
There are 5 dogs named Benjamin, Dogley, Fido, Samba, Bernie.

The local variables are dog, dOg, doG, names.
The constants are Dog, DOG.
```



## Rust

Rust style dictates that identifiers should be written in snake case, e.g. <tt>big_dog</tt>, <tt>small_dog</tt>; whereas types (structs and enums) should be written in camel case, e.g. <tt>BigDog</tt>, <tt>SmallDog</tt>. Failing to comply with this standard does not cause a compiler error, but it will trigger a compiler warning, and the culture is very strongly towards compliance with this standard.


```rust
fn main() {
    let dog = "Benjamin";
    let Dog = "Samba";
    let DOG = "Bernie";
    println!("The three dogs are named {}, {} and {}.", dog, Dog, DOG);
}
```


This triggers two warnings at compilation:

<lang><anon>:3:9: 3:12 warning: variable `Dog` should have a snake case name such as `dog`, #[warn(non_snake_case)] on by default
<anon>:3     let Dog = "Samba";
                 ^~~
<anon>:4:9: 4:12 warning: variable `DOG` should have a snake case name such as `dog`, #[warn(non_snake_case)] on by default
<anon>:4     let DOG = "Bernie";
                 ^~~
```


The resulting program will compile and run just fine, producing the output:

<lang>The three dogs are named Benjamin, Samba and Bernie.
```



## Run BASIC


```runbasic

dog$ = "Benjamin"
doG$ = "Smokey"
Dog$ = "Samba"
DOG$ = "Bernie"
print "The 4 dogs are "; dog$; ", "; doG$; ", "; Dog$; " and "; DOG$; "."

```



## Sather

Though by convention Sather uses all uppercase letters for class names, a variable can be
all uppercase.


```sather
class MAIN is
  main is
    dog ::= "Benjamin";
    Dog ::= "Samba";
    DOG ::= "Bernie";
    #OUT + #FMT("The three dogs are %s, %s and %s\n", 
                 dog, Dog, DOG);
  end;
end;
```


Outputs:


```txt
The three dogs are Benjamin, Samba and Bernie
```



## Scala


```scala
val dog = "Benjamin"
val Dog = "Samba"
val DOG = "Bernie"
println("There are three dogs named " + dog + ", " + Dog + ", and " + DOG + ".")
```

Output:

```txt
There are three dogs named Benjamin, Samba, and Bernie.
```


## Scheme

Output may differ depending on implementation.

```scheme
(define dog "Benjamin")
(define Dog "Samba")
(define DOG "Bernie")

(if (eq? dog DOG)
        (begin (display "There is one dog named ")
                (display DOG)
                (display ".")
                (newline))
        (begin (display "The three dogs are named ")
                (display dog) (display ", ")
                (display Dog) (display " and ")
                (display DOG)
                (display ".")
                (newline)))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const string: dog is "Benjamin";
const string: Dog is "Samba";
const string: DOG is "Bernie";

const proc: main is func
  begin
    writeln("The three dogs are named " <& dog <& ", " <& Dog <& " and " <& DOG <& ".");
  end func;
```



## SETL


```pascal
dog := 'Benjamin';
Dog := 'Samba';
DOG := 'Bernie';
print( 'There is just one dog named', dOg );
```

{{out}}

```txt
There is just one dog named Bernie
```



## Sidef


```ruby
var dog = 'Benjamin';
var Dog = 'Samba';
var DOG = 'Bernie';
say "The three dogs are named #{dog}, #{Dog}, and #{DOG}.";
```

{{out}}

```txt
The three dogs are named Benjamin, Samba, and Bernie.
```



## Simula

Simula identifiers are case-insensitive, and the compiler will indignantly reject a program that tries to declare multiple variables with names differing only in case. (Same with ''key words'': Case of a character in Simula ''code'' generally only matters in [http://simula67.at.ifi.uio.no/Standard-86/chap_1.htm| a simple string or a character constant].)

```simula
begin
    text dog;
    dog :- blanks( 8 );
    dog := "Benjamin";
    Dog := "Samba";
    DOG := "Bernie";
    outtext( "There is just one dog, named " );
    outtext( dog );
    outimage
end
```

{{out}}

```txt
There is just one dog, named Bernie
```



## Smalltalk

{{works with|GNU Smalltalk}}

Smalltalk's symbols are case sensitive.


```smalltalk
|dog Dog DOG|
dog := 'Benjamin'.
Dog := 'Samba'.
DOG := 'Bernie'.
( 'The three dogs are named %1, %2 and %3' %
  { dog . Dog . DOG } ) displayNl.
```


Outputs:


```txt
The three dogs are named Benjamin, Samba and Bernie
```



## SNOBOL4


```snobol4
    DOG = 'Benjamin'
    Dog = 'Samba'
    dog = 'Bernie'
    OUTPUT = 'The three dogs are named ' DOG ', ' Dog ', and ' dog
END
```

{{out}}

```txt
The three dogs are named Benjamin, Samba, and Bernie
```



## Standard ML

Standard ML is case sensitive.

```sml
let
  val dog = "Benjamin"
  val Dog = "Samba"
  val DOG = "Bernie"
in
  print("The three dogs are named " ^ dog ^ ", " ^ Dog ^ ", and " ^ DOG ^ ".\n")
end;
```

{{out}}

```txt
The three dogs are named Benjamin, Samba, and Bernie.
```



## Stata

Stata is case-sensitive.


```stata
. local dog Benjamin
. local Dog Samba
. local DOG Bernie
. display "The three dogs are named $_dog, $_Dog, and $_DOG."
The three dogs are named Benjamin, Samba, and Bernie.
```



## Swift


```swift
let dog = "Benjamin"
let Dog = "Samba"
let DOG = "Bernie"
println("The three dogs are named \(dog), \(Dog), and \(DOG).")
```



## Tcl

Tcl variable names are case sensitive:

```tcl
set dog "Benjamin"
set Dog "Samba"
set DOG "Bernie"
puts "The three dogs are named $dog, $Dog and $DOG"
```

Which prints...

```txt
The three dogs are named Benjamin, Samba and Bernie
```



## UNIX Shell


```sh
dog="Benjamin"
Dog="Samba"
DOG="Bernie"
echo "The three dogs are named $dog, $Dog and $DOG."
```


The three dogs are named Benjamin, Samba and Bernie.


## Ursa

Ursa names are case sensitive:

```ursa>
 decl string dog Dog DOG
> set dog "Benjamin"
> set Dog "Samba"
> set DOG "Bernie"
> out "The three dogs are named " dog ", "  Dog ", and " DOG endl console
The three dogs are named Benjamin, Samba, and Bernie
>
```



## VBA

VBA is case sensitive case insensitive. The variable names 'dog', 'Dog' and 'DOG' can not co-exist. 

```vb
Public Sub case_sensitivity()
    'VBA does not allow variables that only differ in case
    'The VBA IDE vbe will rename variable 'dog' to 'DOG'
    'when trying to define a second variable 'DOG'
    Dim DOG As String
    DOG = "Benjamin"
    DOG = "Samba"
    DOG = "Bernie"
    Debug.Print "There is just one dog named " & DOG
End Sub
```
{{out}}

```txt
There is just one dog named Bernie
```


## XLISP

XLISP is entirely case-insensitive. The user can decide whether to have the system print symbols, etc., in capitals or in lower case, by assigning to the variable <tt>*PRINT-CASE*</tt>.

```xlisp
(SETQ DOG 'BENJAMIN)
(SETQ Dog 'SAMBA)
(SETQ dog 'BERNIE)
(DISPLAY `(THERE IS JUST ONE DOG NAMED ,DOG))
```

There is, in any event, only one dog.

```txt
(THERE IS JUST ONE DOG NAMED BERNIE)
```



## XPL0

XPL0 is normally case-insensitive, so there is really just one dog named
Bernie. However, it has a command-line switch (/c) that turns case
sensitivity on. All names must start with a capital letter (or an underline, so they can't
clash with command words such as 'for'). Thus "dog" cannot be used as a
name, but Dog, DOG and DoG (and For) can. The intent of the command-line
switch (/c) is to detect inconsistent capitalizing of names such as
Ascii and ASCII or CpuReg and CPUReg.


## ZX Spectrum Basic


```basic
10 LET D$="Benjamin"
20 PRINT "There is just one dog named ";d$
```



## zkl


```zkl
var dog = "Benjamin", Dog = "Samba", DOG = "Bernie";
```

{{out}}

```txt

vars
L(L("DOG","Bernie"),L("Dog","Samba"),L("dog","Benjamin"))

```


{{omit from|sed|No variables.}}
