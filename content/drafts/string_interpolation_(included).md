+++
title = "String interpolation (included)"
description = ""
date = 2019-05-23T09:45:53Z
aliases = []
[extra]
id = 5394
[taxonomies]
categories = []
tags = []
+++

{{Task|Basic language learning}}[[Category:String manipulation]][[Category:Simple]]
{{basic data operation}}
{{omit from|NSIS}}{{omit from|BBC BASIC}}
Given a string and defined variables or values, [[wp:String literal#Variable_interpolation|string interpolation]] is the replacement of defined character sequences in the string by values or variable values. 
: For example, given an original string of <code>"Mary had a X lamb."</code>, a value of "big", and if the language replaces X in its interpolation routine, then the result of its interpolation would be the string <code>"Mary had a big lamb"</code>.

:(Languages usually include an infrequently used character or sequence of characters to indicate what is to be replaced such as "%", or "#" rather than "X").

;The task is to: 
# Use your languages inbuilt string interpolation abilities to interpolate a string missing the text <code>"little"</code> which is held in a variable, to produce the output string <code>"Mary had a little lamb"</code>.
# If possible, give links to further documentation on your languages string interpolation features.

<small>Note: The task is not to create a string interpolation routine, but to show a language's built-in capability.</small>


## Ada


```Ada
with Ada.Strings.Fixed, Ada.Text_IO;
use  Ada.Strings, Ada.Text_IO;
procedure String_Replace is
   Original : constant String := "Mary had a @__@ lamb.";
   Tbr : constant String := "@__@";
   New_Str : constant String := "little";
   Index : Natural := Fixed.Index (Original, Tbr);
begin
   Put_Line (Fixed.Replace_Slice (
     Original, Index, Index + Tbr'Length - 1, New_Str));
end String_Replace;
```


Alternatively


```Ada
Put_Line ("Mary had a " & New_Str & " lamb.");
```



## Aikido


```aikido
const little = "little"
printf ("Mary had a %s lamb\n", little)

// alternatively
println ("Mary had a " + little + " lamb")
```



## ALGOL 68

{{trans|C}}

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - requires formatted transput}}
'''string'''s are simply '''flex''' arrays of '''char'''.  '''format'''s on the other hand take on some of the properties of '''proc'''edures including the scoping rules.

```algol68
main:(
# as a STRING #
  STRING extra = "little";
  printf(($"Mary had a "g" lamb."l$, extra));

# as a FORMAT #
  FORMAT extraf = $"little"$;
  printf($"Mary had a "f(extraf)" lamb."l$);

# or: use simply use STRING concatenation #
  print(("Mary had a "+extra+" lamb.", new line))
)
```

{{out}}

```txt

Mary had a little lamb.
Mary had a little lamb.
Mary had a little lamb.

```


## AutoHotkey


```AutoHotkey
; Using the = operator
LIT = little
string = Mary had a %LIT% lamb.

; Using the := operator
LIT := "little"
string := "Mary had a" LIT " lamb."

MsgBox %string%
```


Documentation: [http://www.autohotkey.com/docs/Variables.htm#Variables Variables] (see '''Storing values in variables''' and '''Retrieving the contents of variables''')

{{omit from|ARM Assembly}}


## AWK

String interpolation is usually done with functions sub() and gsub(). gawk has also gensub().

```AWK
#!/usr/bin/awk -f
BEGIN {
	str="Mary had a # lamb."
	gsub(/#/, "little", str)
	print str
}
```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion
call :interpolate %1 %2 res
echo %res%
goto :eof

:interpolate
set pat=%~1
set str=%~2
set %3=!pat:X=%str%! 
goto :eof
```


''Demo''

```dos>
interpolate.cmd "Mary had a X lamb" little
Mary had a little lamb
```



## Bracmat

Use pattern matching to find the part of the string up to and the part of the string following the magic X. Concatenate these parts with the string "little" in the middle.


```bracmat
@("Mary had a X lamb":?a X ?z) & str$(!a little !z)
```



## C

Include the <code><stdio.h></code> header to use the functions of the [[wp:Printf|printf]] family:

```c>#include <stdio.h


int main() {
  const char *extra = "little";
  printf("Mary had a %s lamb.\n", extra);
  return 0;
}
```



## C++


```cpp>#include <string

#include <iostream>

int main( ) {
   std::string original( "Mary had a X lamb." ) , toBeReplaced( "X" ) ,
      replacement ( "little" ) ;
   std::string newString = original.replace( original.find( "X" ) ,
	 toBeReplaced.length( ) , replacement ) ;
   std::cout << "String after replacement: " << newString << " \n" ;
   return 0 ;
}
```



## C++

{{works with|C++11}}

```cpp
// Variable argument template

#include <string>
#include <vector>

using std::string;
using std::vector;

template<typename S, typename... Args>
string interpolate( const S& orig , const Args&... args)
{
   string out(orig);

   // populate vector from argument list
   auto va = {args...};
   vector<string> v{va};
   
   size_t i = 1;

   for( string s: v)
     {
       string is = std::to_string(i);
       string t = "{" +  is + "}";  // "{1}", "{2}", ...
       try
	 {
	   auto pos = out.find(t);

	   if ( pos != out.npos)  // found token
	     {
	       out.erase(pos, t.length()); //erase token
	       out.insert( pos, s);       // insert arg
	     }

	   i++;                           // next 
	 }
	 catch( std::exception& e)
	   {
	     std::cerr << e.what() << std::endl;
	   }

     } // for

   return out;
}
```


=={{header|C sharp|C#}}==
This is called [http://msdn.microsoft.com/en-us/library/txafckwd.aspx "composite formatting"] in MSDN.


```csharp
class Program
{
    static void Main()
    {
        string extra = "little";
        string formatted = $"Mary had a {extra} lamb.";
        System.Console.WriteLine(formatted);
    }
}
```



## Clojure


```lisp
(let [little "little"]
  (println (format "Mary had a %s lamb." little)))
```



## COBOL

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. interpolation-included.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  extra PIC X(6) VALUE "little".

       PROCEDURE DIVISION.
           DISPLAY FUNCTION SUBSTITUTE("Mary had a X lamb.", "X", extra)

           GOBACK
           .
```



## Coco


As CoffeeScript, but [https://github.com/satyr/coco/wiki/additions#wiki-variable-interpolation-x the braces are optional if the expression to be interpolated is just a variable]:


```coco
size = 'little'
console.log "Mary had a #size lamb."
```



## CoffeeScript


```coffeescript

size = 'little'
console.log "Mary had a #{size} lamb." # Mary had a little lamb.
console.log "Escaping: \#{}" # Escaping: #{}
console.log 'No #{ interpolation} with single quotes' # No #{ interpolation} with single quotes

# Multi-line strings and arbtrary expressions work: 20
console.log """
  Multi-line strings and arbtrary expressions work: #{ 5 * 4 }
  """

```



## Common Lisp


```lisp
(let ((extra "little"))
  (format t "Mary had a ~A lamb.~%" extra))
```


More documentation on the [http://www.cs.cmu.edu/Groups/AI/html/hyperspec/HyperSpec/Body/fun_format.html FORMAT] function.


## D


```d
void main() {
    import std.stdio, std.string;

    "Mary had a %s lamb.".format("little").writeln;
    "Mary had a %2$s %1$s lamb.".format("little", "white").writeln;
}
```

{{out}}

```txt
Mary had a little lamb.
Mary had a white little lamb.
```

More documentation on the [http://www.digitalmars.com/d/2.0/phobos/std_string.html#format format()] function.


## Delphi


```Delphi
program Project1;

uses
  System.SysUtils;

var
  Template : string;
  Marker : string;
  Description : string;
  Value : integer;
  Output : string;

begin
  // StringReplace can be used if you are definitely using strings
  // http://docwiki.embarcadero.com/Libraries/XE7/en/System.SysUtils.StringReplace
  Template := 'Mary had a X lamb.';
  Marker := 'X';
  Description := 'little';
  Output := StringReplace(Template, Marker, Description, [rfReplaceAll, rfIgnoreCase]);
  writeln(Output);

  // You could also use format to do the same thing.
  // http://docwiki.embarcadero.com/Libraries/XE7/en/System.SysUtils.Format
  Template := 'Mary had a %s lamb.';
  Description := 'little';
  Output := format(Template,[Description]);
  writeln(Output);

  // Unlike StringReplace, format is not restricted to strings.
  Template := 'Mary had a %s lamb. It was worth $%d.';
  Description := 'little';
  Value := 20;
  Output := format(Template,[Description, Value]);
  writeln(Output);

end.
```

{{out}}

```txt
Mary had a little lamb.
Mary had a little lamb.
Mary had a little lamb. It was worth $20.

```



## DWScript


```delphi
PrintLn(Format('Mary had a %s lamb.', ['little']))
```

{{out}}

```txt
Mary had a little lamb.
```



## Dyalect


Dyalect has a built-in [https://github.com/vorov2/dyalect/wiki/String#interpolation string interpolation] feature.


```Dyalect
const lamb_size = "little"
print("Mary had a \(lamb_size) lamb.")
```



## E



```e
def adjective := "little"
`Mary had a $adjective lamb`
```


The <code>`...`</code> syntax in general may be used as a sort of syntax extension; string interpolation is just the default case. [http://www.erights.org/elang/grammar/quasi-overview.html More information on E quasi-literals.] (Note that this documentation may be somewhat out of date.)

The above code is equivalent to (expands into):


```e
def adjective := "little"
simple__quasiParser.valueMaker("Mary had a ${0} lamb").substitute([adjective])
```


If an identifier precedes the opening <code>`</code>, then it replaces <code>simple</code>; the quasiParser may be an arbitrary user-defined object. In this way, E provides lightweight syntax for embedding other languages: XML, JSON, GUI layouts, regular expressions, etc.


## EchoLisp

'''format''' and '''printf''' use replacement directives to perform interpolation. See [http://www.echolalie.org/echolisp/help.html#format format specification] in EchoLisp documentatiuon.

```scheme

;; format uses %a or ~a as replacement directive
(format "Mary had a ~a lamb" "little")
   → "Mary had a little lamb"
(format "Mary had a %a lamb" "little")
   → "Mary had a little lamb"

```



## ECL


```ECL

IMPORT STD;
STD.Str.FindReplace('Mary had a X Lamb', 'X','little');

```


## Elena

ELENA 4.x :

```elena
import extensions;
 
public program()
{
    var s := "little";
    console.printLineFormatted("Mary had a {0} lamb.",s).readChar()
}
```



## Elixir

Elixir borrows Ruby's #{...} interpolation syntax.

```elixir

x = "little"
IO.puts "Mary had a #{x} lamb"

```



## Erlang

{{out}}

```txt

7> S1 = "Mary had a ~s lamb".
8> S2 = lists:flatten( io_lib:format(S1, ["big"]) ).
9> S2.
"Mary had a big lamb"

```



## Euphoria


```euphoria
constant lambType = "little"
sequence s
s = sprintf("Mary had a %s lamb.",{lambType})
puts(1,s)
```

See
[http://openeuphoria.org/docs/std_text.html#_3233_sprintf sprintf],
[http://openeuphoria.org/docs/std_io.html#_1488_printf printf]

=={{header|F_Sharp|F#}}==
[http://msdn.microsoft.com/en-us/library/ee370560(VS.100).aspx Documentation]

```fsharp
let lambType = "little"
printfn "Mary had a %s lamb." lambType
```



## Factor


```factor
USE: formatting 

SYMBOL: little

"little" little set

little get "Mary had a %s lamb" sprintf
```


I tried to be as specific as possible here. The challenge says to use a ''variable'' so that is what I used. It could have been done more cleanly using a CONSTANT.


```factor
USE: formatting 

CONSTANT: little "little"

little "Mary had a %s lamb" sprintf
```





## Falcon

'''VBA/Python programmer's approach.  I'm just a junior Falconeer but this code seems falconic''

```falcon

/* created by Aykayayciti Earl Lamont Montgomery
April 9th, 2018 */

size = "little"
> @ "Mary had a $size lamb"

// line 1: use of the = operator
// line 2: use of the @ and $ operator

```

{{out}}

```txt

Mary had a little lamb
[Finished in 0.2s]

```



## Fantom


Interpolating a variable value into a string is done by using a $ prefix on the variable name within a string.  For example:


```fantom

fansh> x := "little"
little
fansh> echo ("Mary had a $x lamb")
Mary had a little lamb

```


Documentation at: [http://fantom.org/doc/docLang/Literals.html#interpolation Fantom website]


## Fortran


```Fortran
program interpolate

  write (*,*) trim(inter("Mary had a X lamb.","X","little"))

contains

  elemental function inter(string,place,ins) result(new)
    character(len=*), intent(in)                          :: string,place,ins
    character(len=len(string)+max(0,len(ins)-len(place))) :: new
    integer                                               :: idx
    idx = index(string,place)
    if ( idx == 0 ) then
      new = string
    else
      new = string(1:idx-1)//ins//string(idx+len(place):len(string))
    end if
  end function inter
  
end program interpolate
```



## FreeBASIC

FreeBASIC has a complex Print statement which, amongst other things, enables variables to be embedded in the string to be printed.

It is also possible to use C library functions such as printf or sprintf, which allow more conventional string interpolation,
as easily as if they were part of FB itself:

```freebasic
' FB 1.05.0 Win64

#Include "crt/stdio.bi"  '' header needed for printf

Dim x As String = "big"
Print "Mary had a "; x; " lamb"   '' FB's native Print statement
x = "little"
printf("Mary also had a %s lamb", x) 
Sleep
```


{{out}}

```txt

Mary had a big lamb
Mary also had a little lamb

```



## Frink


```frink
x = "little"
println["Mary had a $x lamb."]
```



## FunL


```funl
X = 'little'
println( "Mary had a $X lamb." )
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=29bc3d7010d216a49382a47cdeeec15f Click this link to run this code]'''

```gambas
Public Sub Main()

Print Subst("Mary had a &1 lamb", "little")

End
```

Output:

```txt

Mary had a little lamb

```



## Gastona

This kind of string interpolation is indeed a strong feature in Gastona. We add one more
indirection in the sample just to ilustrate it. 

```gastona
#listix#

   <how>   //little
   <what>  //has a @<how> lamb
   
   <main>  //Mary @<what>

```

{{out|Output}}

```txt

Mary has a little lamb

```



## Go

Doc: [http://golang.org/pkg/fmt/ http://golang.org/pkg/fmt/]

```go

package main

import (
    "fmt"
)

func main() {
    str := "Mary had a %s lamb"
    txt := "little"
    out := fmt.Sprintf(str, txt)
    fmt.Println(out)
}

```



## Groovy


```groovy
def adj = 'little'
assert 'Mary had a little lamb.' == "Mary had a ${adj} lamb."
```



## Haskell


No such facilities are defined in Haskell 98, but the <code>base</code> package distributed with GHC provides a <code>[http://hackage.haskell.org/packages/archive/base/latest/doc/html/Text-Printf.html#v:printf printf]</code> function.


```haskell
import Text.Printf

main = printf "Mary had a %s lamb\n" "little"
```



## HicEst

[http://www.HicEst.com/EDIT Further documentation on HicEst string interpolation function EDIT()]

```hicest
CHARACTER original="Mary had a X lamb", little = "little", output_string*100

output_string = original
EDIT(Text=output_string, Right='X', RePLaceby=little)
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon are descended from a line of languages with a wealth of string manipulation capabilities. [http://www.cs.arizona.edu/icon/ftp/doc/lb1up.pdf See The Icon Programming Language, 3rd Edition; Griswold and Griswold; Chapter 3 String Scanning]

```Icon
  s2 := "humongous"
  s3 := "little"
  s1 :=  "Mary had a humongous lamb."
  s1 ?:= tab(find(s2)) || (=s2,s3) || tab(0)          # replaces the first instance of s2 with s3
  while s1 ?:= tab(find(s2)) || (=s2,s3) || tab(0)    # replaces all instances of s2 with s3, equivalent to replace
```

{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn Note the strings library includes convenient procedures for string replacement] such as replace(s1,s2,s3) which replaces all occurrences of s2 in s1 with s3 and replacem(s1,s2,s3,...) which replaces multiple pairs.


## J

The <code>strings</code> and <code>printf</code> scripts are part of the base library.

```j
   require 'printf'
   'Mary had a %s lamb.' sprintf <'little'
Mary had a little lamb.

   require 'strings'
   ('%s';'little') stringreplace 'Mary had a %s lamb.'
Mary had a little lamb.
   'Mary had a %s lamb.' rplc '%s';'little'
Mary had a little lamb.
```


Documentation:

The comments in these library files give brief descriptions of their contents, and you can browse them using open:


```J
   open'strings printf'
```


Alternatively, both [http://www.jsoftware.com/docs/help602/user/script_strings.htm strings] and [http://www.jsoftware.com/help/jforc/input_and_output.htm#_Toc191734427 printf] have various web pages describing them, and printf has a lab demonstrating its use (from J's IDE's menu, go Studio -> Labs... and then look in the System category).

That said, note that in recent versions of J, ''strings'' is no longer a separate script but part of the core library.


## Java


```java
String original = "Mary had a X lamb";
String little = "little";
String replaced = original.replace("X", little); //does not change the original String
System.out.println(replaced);
//Alternative:
System.out.printf("Mary had a %s lamb.", little);
//Alternative:
String formatted = String.format("Mary had a %s lamb.", little);
System.out.println(formatted);
```



## JavaScript


```javascript
var original = "Mary had a X lamb";
var little = "little";
var replaced = original.replace("X", little); //does not change the original string
```


Or,


```javascript
// ECMAScript 6
var X = "little";
var replaced = `Mary had a ${X} lamb`;
```



## jq


```jq
"little" as $x
  | "Mary had a \($x) lamb"
```


Any valid jq expression (including a pipeline) can appear between the interpolating parentheses, e.g.:
```jq
$ jq -M -n -r '"Jürgen" as $x | "The string \"\($x)\" has \($x|length) codepoints."'
The string "Jürgen" has 6 codepoints.
```

'''Documentation''': [http://stedolan.github.io/jq/manual/#Stringinterpolationfoo String interpolation]


## Julia


```julia
X = "little"
"Mary had a $X lamb"
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
   val s = "little"
    // String interpolation using a simple variable
    println("Mary had a $s lamb")

    // String interpolation using an expression (need to wrap it in braces)
    println("Mary had a ${s.toUpperCase()} lamb")

    // However if a simple variable is immediately followed by a letter, digit or underscore
    // it must be treated as an expression
    println("Mary had a ${s}r lamb") // not $sr    
}
```


{{out}}

```txt

Mary had a little lamb
Mary had a LITTLE lamb
Mary had a littler lamb

```



## Lasso

Lasso doesn't really have built-in string interpolation, but you can use the built-in email mail-merge capability:

```lasso
email_merge("Mary had a #adjective# lamb", map("token"="little", "adjective"=""), null, 'plain')
```

{{out}}

```txt
Mary had a little lamb
```




## LiveCode

Livecode has a [http://docs.runrev.com/Function/merge merge] function for interpolation

```LiveCode
local str="little"
put merge("Mary had a [[str]] lamb.")

-- Mary had a little lamb.
```



## Lua



###  Variable names 


There is no default support for automatic interpolation of variables names being used as placeholders within a string. However, interpolation is easily emulated by using the [string.gsub] function:


```Lua
str = string.gsub( "Mary had a X lamb.", "X", "little" )
print( str )
```



###  Literal characters 


Interpolation of literal characters escape sequences does occur within a string:


```lua
print "Mary had a \n lamb"    -- The \n is interpreted as an escape sequence for a newline
```



## M2000 Interpreter


```M2000 Interpreter

module checkit {
      size$="little"
      m$=format$("Mary had a {0} lamb.", size$)
      Print m$
      Const RightJustify=1
      \\ format$(string_expression)  process escape codes
      Report RightJustify, format$(format$("Mary had a {0} {1} lamb.\r\n We use {0} for size, and {1} for color\r\n", size$, "wh"+"ite"))
      \\ we can use { } for multi line string
      Report RightJustify, format$({Mary had a {0} {1} lamb.
             We use {0} for size, and {1} for color
             }, size$, "wh"+"ite")
}
checkit

```





## Mathematica


```Mathematica
Extra = "little";
StringReplace["Mary had a X lamb.", {"X" -> Extra}]
->"Mary had a little lamb."
```




## Maxima


```maxima
printf(true, "Mary had a ~a lamb", "little");
```



## Neko


```actionscript
/**
 <doc><h2>String interpolation, in Neko</h2>
   <p><a href="https://nekovm.org/doc/view/string/">NekoVM String Library</a></p>
 </doc>
**/

var sprintf = $loader.loadprim("std@sprintf", 2)

$print(sprintf("Mary had a %s lamb\n", "little"))
```


{{out}}

```txt
prompt$ nekoc string-interpolation.neko
prompt$ neko string-interpolation.n
Mary had a little lamb
```



## Nemerle

Nemerle has a few ways to accomplish this.  It provides an implementation of '''printf()''', $ interpolation within the '''print()''' method, and the most general use is $ interpolation within $ quoted strings.

```Nemerle
using System;
using System.Console;
using Nemerle.IO;  // contains printf() and print()

module Stringy
{
    Main() : void
    {
        def extra = "little";
        printf("Mary had a %s lamb.\n", extra);
        print("Mary had a $extra lamb.\n");
        WriteLine($"Mary had a $extra lamb.");
    }
}
```



## NetRexx

The Built In Functions (BIFs) of [[NetRexx]] can be employed to manipulate strings quite successfully but for more flexible string interpolation a function package like Java's MessageFormat should be used.

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols

import java.text.MessageFormat
import java.text.FieldPosition

useBif()
useMessageFormat()

return

method useBif public static

  st = "Mary had a %1$ lamb."
  si = 'little'

  say st.changestr('%1$', si)

  return

method useMessageFormat public static

  result = StringBuffer('')

  args = Object [                       -
    Object Integer(7),                  -
    Object Date(),                      -
    Object 'a disturbance in the Force' -
  ]
  msgfmt = MessageFormat('At {1, time} on {1, date}, there was {2} on planet {0, number, integer}.')
  result = msgfmt.format(args, result, FieldPosition(0))
  say result

  return

```

{{out}}
<pre style="height: 5ex; overflow:scroll;">
Mary had a little lamb.
At 5:43:05 PM on Aug 22, 2011, there was a disturbance in the Force on planet 7.

```



## Nim


```nim
import strutils

var str = "little"
echo "Mary had a $# lamb" % [str]   
# doesn't need an array for one substitution, but use an array for multiple substitutions
```



```nim
import strformat

var str: string = "little"
echo fmt"Mary had a {str} lamb"
echo &"Mary had a {str} lamb"
```



## OCaml

The OCaml standard library provides the module [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Printf.html Printf]:


```ocaml
let extra = "little" in
Printf.printf "Mary had a %s lamb." extra
```



## OOC

In a String all expressions between #{...} will be evaluated.

```ooc

main: func {
  X := "little"
  "Mary had a #{X} lamb" println()
}

```



## Oz

String interpolation is unidiomatic in Oz. Instead, "virtual strings" are used. [http://www.mozart-oz.org/documentation/op/node4.html Virtual strings] are tuples of printable values and are supported by many library functions.


```oz
declare
  X = "little"
in
  {System.showInfo "Mary had a "#X#" lamb"}
```



## PARI/GP

The Pari library has string interpolation, which extends C's:

```C
GEN
string_interpolate(GEN n)
{
  pari_printf("The value was: %Ps.\n", n);
  GEN s = pari_sprintf("Storing %Ps in a string", n);
}
```


{{works with|PARI/GP|version 2.4.4 and above}}
GP can also interpolate strings:

```parigp
s=Strprintf("The value was: %Ps", 1<<20);
printf("The value was: %Ps", 1<<20);
```



## Perl


```perl
$extra = "little";
print "Mary had a $extra lamb.\n";
printf "Mary had a %s lamb.\n", $extra;
```



## Perl 6


```perl6
my $extra = "little";
say "Mary had a $extra lamb";           # variable interpolation
say "Mary had a { $extra } lamb";       # expression interpolation
printf "Mary had a %s lamb.\n", $extra; # standard printf
say $extra.fmt("Mary had a %s lamb");   # inside-out printf
my @lambs = <Jimmy Bobby Tommy>;
say Q :array { $$$ The lambs are called @lambs[]\\\.} # only @-sigiled containers are interpolated
```



## Phix


```Phix
string size = "little"
string s = sprintf("Mary had a %s lamb.",{size})
?s
```

{{out}}

```txt

"Mary had a little lamb."

```



## PHP


```php
<?php
$extra = 'little';
echo "Mary had a $extra lamb.\n";
printf("Mary had a %s lamb.\n", $extra);
?>
```



## PicoLisp


```PicoLisp
(let Extra "little"
   (prinl (text "Mary had a @1 lamb." Extra)) )
```



## PL/I


```PLI
*process or(!) source xref attributes;
 sit: Proc Options(main);
 /*********************************************************************
 * Test string replacement
 * 02.08.2013 Walter Pachl
 *********************************************************************/
 Dcl s Char(50) Var Init('Mary had a &X lamb. It is &X');
 Put Edit(repl(s,'little','&X'))(Skip,A);

 repl: Proc(str,new,old) Returns(Char(50) Var);
 /*********************************************************************
 * ooREXX has CHANGESTR(old,str,new[,count])
 * repl follows, however, the translate "philosophy"
 * translate(str,new,old) when old and new are just  a character each
 * and replaces all occurrences of old in str by new
 *********************************************************************/
 Dcl str Char(*) Var;
 Dcl (new,old) Char(*);
 Dcl (res,tmp) Char(50) Var init('');
 Dcl p Bin Fixed(31);
 tmp=str;                             /* copy the input string       */
 Do Until(p=0);
   p=index(tmp,old);                  /* position of old in tmp      */
   If p>0 Then Do;                    /* found                       */
     res=res!!left(tmp,p-1)!!new;     /* append new to current result*/
     tmp=substr(tmp,p+length(old));   /* prepare rest of input       */
     End;
   End;
 res=res!!tmp;                        /* final append                */
 Return(res);
 End;
 End;
```

{{out}}

```txt

Mary had a little lamb. It is little

```



## PowerShell

Using the format (-f) operator:

```powershell
$extra = "little"
"Mary had a {0} lamb." -f $extra
```


Using format string with the WriteLine static method

```powershell
$extra = "little"
[console]::writeline("Mary had a {0} lamb.", $extra)
```


Using the format method of the string type

```powershell
$extra = "little"
[string]::Format("Mary had a {0} lamb.", $extra)
```


Note: numeric and date/time formats can be specified with {index:formatString} (i.e. {0:###,###})


## Prolog


```Prolog
Extra = little,
format('Mary had a ~w lamb.', [Extra]),  % display result
format(atom(Atom), 'Mary had a ~w lamb.', [Extra]).  % ... or store it a variable
```


Using [http://www.swi-prolog.org/pack/list?p=func library(func)] for SWI-Prolog:


```Prolog
Extra = little,
Atom = 'Mary had a ~w lamb' $ Extra.
```


Using [http://www.swi-prolog.org/pack/list?p=interpolate library(interpolate)] for SWI-Prolog:


```Prolog
Extra = little,
Atom = 'Mary had a $Extra lamb'.
```



## PureBasic

The function [http://www.purebasic.com/documentation/string/replacestring.html ReplaceString()] is built-in and can have both constants and variables as parameters. 

```PureBasic
ReplaceString("Mary had a X lamb.","X","little")
```

''' Implemented in a program context

```PureBasic
; String variable can be defined by appending .s to its name during definition or by appending and using $ as a part of its name.
Define txt$, txtvar.s="little"

;Load datasegment into variable txt$
Restore Mary 
Read.s  txt$

; Replace X with "little" and store result in txt$
txt$=ReplaceString(txt$,"X",txtvar)

OpenConsole(): Print(txt$)

DataSection:
  Mary:
  Data.s  "Mary had a X lamb."
EndDataSection
```



## Python

Python has more than one inbuilt way of accomplishing the task. The methods have different capabilities that are not stretched by this small task

Using the % [http://docs.python.org/library/stdtypes.html#string-formatting-operations string interpolation operator]:

```python>>>
 original = 'Mary had a %s lamb.'
>>> extra = 'little'
>>> original % extra
'Mary had a little lamb.'
```


Using the [http://docs.python.org/library/string.html#string-formatting .format method of strings]:

```python>>>
 original = 'Mary had a {extra} lamb.'
>>> extra = 'little'
>>> original.format(**locals())
'Mary had a little lamb.'
```

Using the format method, but replace by an expressions position as an argument to the format method call instead of by name:

```python>>>
 original = 'Mary had a {0} lamb.'
>>> extra = 'little'
>>> original.format(extra)
'Mary had a little lamb.'
```


Using the [http://docs.python.org/library/string.html#template-strings Template] class of the string module:

```python>>>
 from string import Template
>>> original = Template('Mary had a $extra lamb.')
>>> extra = 'little'
>>> original.substitute(**locals())
'Mary had a little lamb.'
```


Using the new [https://docs.python.org/3/whatsnew/3.6.html#whatsnew36-pep498 f-strings] string literal available from Python 3.6:

```python>>>
 extra = 'little'
>>> f'Mary had a {extra} lamb.'
'Mary had a little lamb.'
>>> 
```



## Racket


See the documentation on [http://docs.racket-lang.org/reference/Writing.html?q=printf#%28def._%28%28quote._~23~25kernel%29._fprintf%29%29 fprintf] for more information on string interpolation in Racket.


```racket

#lang racket

(format "Mary had a ~a lamb" "little")

```



## REBOL


```rebol
str: "Mary had a <%size%> lamb"
size: "little"
build-markup str

;REBOL3 also has the REWORD function
str: "Mary had a $size lamb"
reword str [size "little"]
```




## REXX


Interpolation does not occur in literal strings, neither within   ''singlequote''   or   ''doublequote''   enclosures. 

However, interpolation can be emulated using the   '''changestr'''   function:


```rexx
/*REXX program to demonstrate string interpolation (string replacement).*/
                                       /*the string to be replaced is   */
replace   = "little"                   /*usually a unique character(s)  */
                                       /*string and is  case  sensative.*/
original1 = "Mary had a X lamb."
new1      = changestr('X', original1, replace)
say 'original1 =' original1
say 'replaced  =' new1
say

original2 = "Mary had a % lamb."
new2      = changestr('%', original2, replace)
say 'original2 =' original2
say 'replaced  =' new2
say

original3 = "Mary had a $$$ lamb."
new3      = changestr('$$$',original3,replace)
say 'original3 =' original3
say 'replaced3 =' new3
say

original4 = "Mary had a someKindOf lamb."
new3      = changestr('someKindOf', original4, "little")
say 'original4 =' original4
say 'replaced4 =' new3
                                       /*stick a fork in it, we're done.*/
```

Some older REXXes don't have a    '''changestr'''   BIF,   so one is included here   ──►   [[CHANGESTR.REX]].


'''output'''

```txt

original1 = Mary had a X lamb.
replaced  = Mary had a little lamb.

original2 = Mary had a % lamb.
replaced  = Mary had a little lamb.

original3 = Mary had a $$$ lamb.
replaced3 = Mary had a little lamb.

original4 = Mary had a someKindOf lamb.
replaced4 = Mary had a little lamb.

```



## Ring


```ring

aString =substr("Mary had a X lamb.", "X", "little")
see aString + nl

```



## Ruby


```ruby
irb(main):001:0> extra = 'little'
=> "little"
irb(main):002:0> "Mary had a #{extra} lamb."
=> "Mary had a little lamb."
irb(main):003:0> "Mary had a %s lamb." % extra
=> "Mary had a little lamb."
```


Documentation:

* [https://github.com/rubyspec/rubyspec/blob/master/language/string_spec.rb string_spec.rb] describes interpolation using <tt>#{....}</tt> in double-quoted strings.
* [http://www.ruby-doc.org/core/ Core API] describes printf-style interpolation by String#% and Kernel#sprintf.


## Run BASIC


```runbasic
a$ = Mary had a X lamb."
a$ = word$(a$,1,"X")+"little"+word$(a$,2,"X")

```



## Rust

Rust has very powerful string interpolation. [https://doc.rust-lang.org/beta/std/fmt/ Documentation here.]

```rust
fn main() {
    println!("Mary had a {} lamb", "little");
    // You can specify order
    println!("{1} had a {0} lamb", "little", "Mary");
    // Or named arguments if you prefer
    println!("{name} had a {adj} lamb", adj="little", name="Mary");
}
```



## Scala

{{libheader|Scala}}


```Scala
object StringInterpolation extends App {

  import util.matching.Regex._
  val size = "little"

  { // Method I (preferred)
    // Scala 2.10.0 supports direct string interpolation
    // by putting "s" at the beginning of the string.
    println("V2.10+  : " + s"Mary had a $size lamb,")
  }

  { // Method II
    // Pre Scala 2.10 indirect use of Java Class Formatter
    val originalFormatter = "Mary had a %s lamb,"
    println("V2.10- 1: " + originalFormatter format size)
    // Above mentioned is Scala's postfix notation and equivalent for: 
    println("V2.10- 2: " + originalFormatter.format(size))
    // Also possible
    printf(("V2.10- 3: " + originalFormatter + '\n').format(size))
    // All will be expanded to
    print(("V2.10- 3: " + originalFormatter + '\n').format(size))
    print((new java.util.Formatter).format("V2.10- 4: " + originalFormatter + '\n', size))
  }

  { // Method III
    // Regular expressions, only for demonstration
    val extractor = """\$\{([^}]+)\}""".r
    println((extractor.replaceAllIn("Regex  1: Mary had a ${x} lamb,", "snow white")))

    // RegEx freaking
    def interpolate(text: String, vars: (String, String)*) =
      extractor.replaceAllIn(text,
        _ match { case Groups(v) => vars.toMap.getOrElse(v, "" /*in case nothing provided*/ ) })

    println(interpolate("Regex  2: ${who} had a ${size} ${pet}, ${unknown}",
      ("pet", "lamb"), ("size", "fat"), ("size", "humongous"), ("who", "Mary")))
  }

  { // Method IV, not recommended.
    // Standard API method, search argument (1st ones) supposed to be a regular expression
    println("Replace1: " + "Mary had a ${x} lamb".replaceAll("""\$\{x\}""", size))
    // Standard API method, literally, on regular expression
    println("Replace2: " + "Mary had a ${x} lamb".replaceAllLiterally("${x}", size))
  }

  { // Method IV, not recommended.
    println("Split   : " + "Mary had a ${x} lamb.".split("""\$\{([^}]+)\}""").mkString(size))
  }
}
```

Documentation:
* Scala 2.10.0: [http://docs.scala-lang.org/overviews/core/string-interpolation.html string interpolation]


## Sed


```bash
#!/bin/bash
# Usage example: . interpolate "Mary has a X lamb" "quite annoying"
echo "$1" | sed "s/ X / $2 /g"
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const string: original is "Mary had a X lamb";
    const string: little is "little";
    var string: replaced is "";
  begin
    replaced := replace(original, "X", little);
    writeln(replaced);
  end func;
```


{{out}}

```txt

Mary had a little lamb

```



## Sidef


```ruby
var extra = 'little';
say "Mary had a #{extra} lamb";
```


or:

```ruby
say ("Mary had a %s lamb" % 'little');
```


See: [https://github.com/trizen/sidef/wiki#strings documentation]


## SNOBOL4

Every statement in SNOBOL can is a subset of pattern replacement having a subject (s1 in this case), object (s2), and replacement (s3).

```snobol
        s1 = "Mary had a humongous lamb."
	s2 = "humongous"
        s3 = "little"           
	s1 s2 = s3 
end
```

See [ftp://ftp.cs.arizona.edu/snobol/gb.pdf The SNOBOL4 Programming Language; Griswold, Poage, Polonsky; Chapter 2 Pattern Matching]


## Stata

See '''[https://www.stata.com/help.cgi?mf_printf printf]''' in Stata help.


```stata
: printf("Mary had a %s lamb.\n", "little")
Mary had a little lamb.
```



## Swift


```swift
let extra = "little"
println("Mary had a \(extra) lamb.")
```



## Tcl

String interpolation is a fundamental operation of the Tcl language itself, and is carried out in a <tt>"</tt>double-quoted<tt>"</tt> program strings as well as bare-words. Thus, interpolation of the string from a variable is carried out with the <tt>$</tt> syntax and the string result of a command is interpolated with the <tt>[…]</tt> syntax.

```tcl
set size "little"
puts "Mary had a $size lamb."

proc RandomWord {args} {
   lindex $args [expr {int(rand()*[llength $args])}]
}
puts "Mary had a [RandomWord little big] lamb."
```

When more sophisticated control is required the <code>format</code> command can be used, which is very similar to the standard [[C]] library's <code>sprintf</code> function:

```tcl
puts [format "Mary had a %s %s." [RandomWord little big] [RandomWord lamb piglet calf]]
```


A third approach is to use <code>string map</code>.

```tcl
set s "Mary had a @SIZE@ lamb."
puts [string map {@SIZE@ "miniscule"} $s]
```


Tcl also supports variable variable names. Even more powerful is nested interpolation with the <tt>subst</tt> command.

```tcl
set grandpa {$daddy}; set grandma \$mommy
set daddy myself; set mommy {lil' bro}
set fun1 \[string\ to
set fun2 lower
set lower middle
set middle upper
set fun3 {aNd]}
puts [subst "$grandpa $fun1$[subst $$fun2]  $fun3 $grandma"]
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT

sentence_old="Mary had a X lamb."

values=*
DATA little
DATA big

sentence_new=SUBSTITUTE (sentence_old,":X:",0,0,values)
PRINT sentence_old
PRINT sentence_new

```

{{out}}

```txt

Mary had a X lamb.
Mary had a little lamb. 

```



## UNIX Shell

{{works with|Bourne Shell}}
Within the Unix shell, interpolation only occurs within doublequotes. Strings enclosed in singlequotes will not be subject to interpolation. Note that within the shell, a string may be used bare. If this is done each word within the string is treated separately, and any variable references or escape sequences will be substituted for their values:


```sh
extra='little'
echo Mary had a $extra lamb.
echo "Mary had a $extra lamb."
printf "Mary had a %s lamb.\n" $extra
```


A ''parameter substitution'', like <code>$extra</code> or <code>${extra}</code>, interpolates its value into some string. This happens outside quotes or inside "double quotes". The other form of interpolation is [http://www.openbsd.org/cgi-bin/man.cgi?query=printf&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html printf(1)] with <code>%s</code>.

The shell has more forms of parameter substitution, like <code>${tea:?no tea}</code>. Your shell's manual explains those. For the original Bourne Shell, [http://heirloom.sourceforge.net/sh/sh.1.html sh(1) manual] explains those.

=
## C Shell
=

```csh
set extra='little'
echo Mary had a $extra lamb.
echo "Mary had a $extra lamb."
printf "Mary had a %s lamb.\n" $extra
```


C Shell has <code>$extra</code> and <code>${extra}</code>. There are also modifiers, like <code>$file:t</code>; [http://www.openbsd.org/cgi-bin/man.cgi?query=csh&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html csh(1) manual] explains those.


## Ursala

Expressions like this

```Ursala
-[foo-[ x ]-bar]-
```

evaluate to a list of character strings beginning with <code>foo</code> and ending
with <code>bar</code>, where <code>foo</code> and <code>bar</code> are literal text (possibly multiple lines)
and <code>x</code> is any expression evaluating to a list of character
strings. Using a dot like this

```Ursala
-[foo-[. f ]-bar]-
```

makes it a function returning a list of character strings consisting
of the output from the function <code>f</code> bracketed by the literal text <code>foo</code>
and <code>bar</code>. In this task, the identity function, <code>~&</code>, is used for <code>f</code>.

```Ursala
x = <'little'>

#show+

main = -[Mary had a -[. ~& ]- lamb.]- x
```

These operators are parsed like parentheses. 
{{out}}

```txt

Mary had a little lamb.

```



## VBA

Here are 2 examples:

'''With Replace'''

```txt

a="little"
debug.print replace("Mary had a X lamb","X",a)  'prints Mary had a little lamb

```

[https://msdn.microsoft.com/en-us/library/bt3szac5(v=vs.90).aspx Replace]

'''With Interpolation function'''

```txt

Sub Main()
    a="little"
    debug.print Format("Mary had a {0} lamb",a)
End Sub

Public Function Format(ParamArray arr() As Variant) As String
    Dim i As Long, temp As String
    temp = CStr(arr(0))
    For i = 1 To UBound(arr)
        temp = Replace(temp, "{" & i - 1 & "}", CStr(arr(i)))
    Next
    Format = temp
End Function

```


[https://msdn.microsoft.com/en-us/library/s2dy91zy(v=vs.90).aspx CStr]
[https://msdn.microsoft.com/en-us/library/95b8f22f(v=vs.90).aspx UBound]
[https://msdn.microsoft.com/en-us/library/ct363x9h.aspx ParamArray]


## Verbexx


```verbexx
////////////////////////////////////////////////////////////////////////////////////////
//
//  The @INTERPOLATE verb processes a string with imbedded blocks of code.  The code
//  blocks are parsed and evaluated.  Any results are converted to a string, which 
//  is then inserted into the output string, replacing the code and braces.
//  
// example: @INTERPOLATE "text{ @IF (x > y) then:{x} else:{y} }more text "
//
////////////////////////////////////////////////////////////////////////////////////////

@VAR v = "little"; 

@SAY (@INTERPOLATE "Mary had a { v } lamb");

//   output:    Mary had a litle lamb
```



## Visual Basic .NET



```vbnet

Dim name as String = "J. Doe"
Dim balance as Double = 123.45
Dim prompt as String = String.Format("Hello {0}, your balance is {1}.", name, balance)
Console.WriteLine(prompt)

```



## zkl


```zkl
"Mary had a X lamb.".replace("X","big")
```

Generates a new string. For more info, refer to manual in the downloads section of [http://zenkinetic.com/ zenkinetic.com zkl page]

{{omit from|8086 Assembly}}
{{omit from|80386 Assembly}}
{{omit from|bc|No built-in string interpolation in bc}}
{{omit from|dc|No built-in string interpolation in dc}}
{{omit from|GUISS}}
{{omit from|Unlambda|Does not have built-in string interpolatiopn (or built-in strings, for that matter).}}
{{omit from|Z80 Assembly}}
{{omit from|Axe}}
