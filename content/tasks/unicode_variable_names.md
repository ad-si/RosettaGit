+++
title = "Unicode variable names"
description = ""
date = 2019-09-09T19:11:23Z
aliases = []
[extra]
id = 10008
[taxonomies]
categories = ["task", "Unicode"]
languages = [
  "ada",
  "algol_68",
  "auto_hotkey",
  "ba_con",
  "bracmat",
  "c",
  "c_sharp",
  "clojure",
  "common_lisp",
  "d",
  "delphi",
  "dwscript",
  "echolisp",
  "elena",
  "emacs_lisp",
  "factor",
  "forth",
  "freebasic",
  "frink",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lingo",
  "livecode",
  "lily",
  "lolcode",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "nemerle",
  "netr_exx",
  "nim",
  "objeck",
  "pari_gp",
  "peloton",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "prolog",
  "python",
  "r",
  "racket",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "stata",
  "swift",
  "tcl",
  "vala",
  "zkl",
]
tags = []
+++

## Task

# Describe, and give a pointer to documentation on your languages use of characters ''beyond'' those of the ASCII character set in the naming of variables.
# Show how to:
:* Set a variable with a name including the 'Δ', (delta ''character''), to 1
:* Increment it
:* Print its value.


## Related tasks

* [[Case-sensitivity of identifiers]]





## 8th

''Text in 8th is stored in the UTF-8 encoding, which means that the text is always represented correctly, even when due to other issues (e.g. font problems) it may appear incorrect.

''To make the programmer’s task easier, 8th not only lets you actually enter any UTF-8 text, it also lets you use special “escapes” in your text to make it easier to enter obscure characters. Thus, for example, this string: "qu\u00e9" results in this: qué.

''The words (e.g. “functions”) 8th provides to do string manipulation are also UTF-8 aware, which means you don’t have to worry about creating an invalid bit of UTF-8 encoded text (unless you deliberately do so).''

-- [http://8th-dev.com/local.html Writing localized applications with 8th]


```forth

1 var, Δ

Δ @ n:1+ Δ !

Δ @ . cr

\ unicode silliness

: 念 ' G:@ w:exec ;
: 店 ' G:! w:exec ;
: ਵਾਧਾ ' n:1+ w:exec ;
: الوداع ' G:bye w:exec ;
: キャリッジリターン ' G:cr w:exec ;
: प्रिंट ' G:. w:exec ;

Δ 念 ਵਾਧਾ Δ 店

Δ 念 प्रिंट キャリッジリターン
الوداع


```



## ACL2

Variables in ACL2 cannot be modified in place.

```Lisp
(let ((Δ 1))
     (1+ Δ))
```



## Ada

As of Ada 2005, all source code can be made of up to 32bit characters.
Unless you have made it a default, GNAT would require the -gnatW8 flag to understand you are using UTF8 for the code below, other encodings are possible.

```Ada
with Ada.Text_IO;
procedure main is
   Δ : Integer;
begin
   Δ := 41;
   Δ := Δ + 1;
   Ada.Text_IO.Put_Line (Δ'Img);
end main;
```

```txt
 42
```


## ALGOL 68

The definition of Algol 68 is character set independent.  Not only variables may be represented in Unicode, all other language elements may be so represented.  All that is required is that the representation language must be capable of being translated unambigously back and forth into the reference language.

The following excerpt from the Revised Report states this:

----

d) A '''program''' in the strict language must be represented in some "representation language"
{9.3.a} chosen by the implementer. In most cases this will be the official "reference
language".

(i) A '''program''' in a representation language is obtained by replacing the '''symbols''' of a
'''program''' in the strict language by certain typographical marks {9 .3}.

(ii) Even the reference language allows considerable discretion to the implementer
{9.4.a,b,c}. A restricted form of the reference language in which such freedom has
not been exercised may be termed the "canonical form" of the language, and it is expected
that this form will be used in algorithms intended for publication.

(iii) The meaning of a '''program''' in a representation language is the same as that of the
'''program''' {in the strict language} from which it was obtained

----

The great majority of implementations have used EBCDIC, ASCII and an encoding of Cyrillic (precise encoding not known to me).
[[wp:ALGOL_68]] gives as an example:

''Russian/Soviet example: In English Algol68's reverent case statement reads '''case''' ~ '''in''' ~ '''out''' ~ '''esac''', in Cyrillic this reads '''выб''' ~ '''в''' ~ '''либо''' ~ '''быв'''.''


## AutoHotkey

The earlier version of AutoHotkey (AutoHotkey Basic) will produce an error since it doesn't support Unicode. It is perfectly working in AutoHotkey_L Unicode (Lexikos Custom Build).
Documentation: http://www.autohotkey.net/~Lexikos/AutoHotkey_L/docs/Variables.htm
```ahk
Δ = 1
Δ++
MsgBox, % Δ
```



## BaCon

This is a port from the C example. As mentioned there, C has limited support for Unicode variable names which is specified in the C standard, and BaCon, being a Basic-to-C converter, therefore has the same restrictions. The below example works with the CLang compiler.

```qbasic
PRAGMA COMPILER clang

DECLARE Δ TYPE int

Δ = 1

INCR Δ

PRINT Δ
```

```txt

user@host $ bacon prog
Converting 'prog.bac'... done, 10 lines were processed in 0.008 seconds.
Compiling 'prog.bac'... clang  -c prog.bac.c
clang -o prog prog.bac.o -lbacon -lm
Done, program 'prog' ready.
user@host $ ./prog
2

```



## Bracmat

Bracmat allows any sequence of non-zero bytes as symbol and therefore, as variable name. Even the empty string is a variable, though a special one. If a symbol/variable name contains characters that have special meaning (operators, prefixes, parentheses, braces and the semicolon) it may be necessary to enclose it in quotes. Other special characters must be escaped C-style. See bracmat.html in the git-repo. The example below requires a terminal that supports UTF-8 encoded characters.

```bracmat
( (Δ=1)
& 1+!Δ:?Δ
& out$("Δ:" !Δ)
);
```

Output:

```txt
Δ: 2
```



## C

C has limited support for Unicode in variable names, see Annex D of the [http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf C standard].

----

'''IMPORTANT''' : This implementation works, but it has been tested with clang 3.8.0, in order for the code to compile and run properly you must ensure that the file is saved in Unicode and you are using the correct compiler. Not doing either one of them will result in errors.

```C

#include<stdio.h>

int main() {
    int Δ = 1;

    Δ++;

    printf("%d",Δ);

    return 0;
}

```

Output:

```txt

2

```


## C#
Section 2.4.2 of the [http://go.microsoft.com/fwlink/?LinkId=199552 C# Language Specification] gives rules for identifiers. They correspond exactly to those recommended by the [http://unicode.org/reports/tr31/ Unicode Standard Annex 31], except that underscore is allowed as an initial character (as is traditional in the C programming language), Unicode escape sequences are permitted in identifiers, and the "@" character is allowed as a prefix to enable keywords to be used as identifiers.

```c#
class Program
{
    static void Main()
    {
        var Δ = 1;
        Δ++;
        System.Console.WriteLine(Δ);
    }
}
```

```txt
2
```



## Clojure

According to the current [http://clojure.org/reader documentation], one should stick to naming with alphanumeric characters and *, +, !, -, _, and ? to avoid possible problems if future versions of Clojure decide to apply special meaning to a character.

That being said, it is not currently enforced, so while you probably shouldn't, you technically can.


```clojure
(let [Δ 1]
  (inc Δ))
```

```txt
2
```



## Common Lisp


```lisp
(let ((Δ 1))
  (incf Δ))
```

```txt
2
```



## D

D source files support four character encodings: ASCII, UTF-8, UTF-16 and UTF-32.

```d
import std.stdio;

void main() {
    auto Δ = 1;
    Δ++;
    writeln(Δ);
}
```

```txt
2
```

You can use any of the following:
    Letters,
    digits,
    underscore (_),
    code points >= \u00A0 and < \uD800,
    code points > \uDFFF.
However, the following cannot be used:
    \u0024 ($),
    \u0040 (@) and
    \u0060 (`).
See:
http://www.prowiki.org/wiki4d/wiki.cgi?DanielKeep/TextInD


## Delphi

For more information about naming identifiers (including variables) visit: [http://docwiki.embarcadero.com/RADStudio/en/Identifiers Identifiers in Delphi]

```Delphi
(* Compiled with Delphi XE *)
program UnicodeVariableName;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  Δ: Integer;

begin
  Δ:= 1;
  Inc(Δ);
  Writeln(Δ);
  Readln;
end.
```


=={{header|Déjà Vu}}==

```dejavu
set :Δ 1
set :Δ ++ Δ
!. Δ
```



## DWScript


```Delphi
var Δ : Integer;

Δ := 1;
Inc(Δ);
PrintLn(Δ);
```



## EchoLisp

Symbol names can be any string including unicode characters. See the EchoLisp [http://www.echolalie.org/echolisp/help.html#language reference] documentation.

```lisp

(define ∆-🍒 1)  → ∆-🍒
(set! ∆-🍒 (1+ ∆-🍒)) → 2
(printf "🔦 Look at ∆-🍒 : %d" ∆-🍒)
🔦 Look at ∆-🍒 : 2

```


## Elena

ELENA 4.x:

```elena
public program()
{
    var Δ := 1;
    Δ := Δ + 1;

    console.writeLine:Δ
}
```



## Emacs Lisp


```Lisp
(setq Δ 1)
(setq Δ (1+ Δ))
(message "Δ is %d" Δ)
```


Variables are symbols and symbol names can be any string.  Source code <code>.el</code> files can have all usual Emacs coding system specifications to give variables in non-ASCII.

The byte compiler writes <code>utf-8</code> (or past versions wrote <code>emacs-mule</code>) into <code>.elc</code> so that any mixture of non-ASCII is preserved.

=={{header|F_Sharp|F#}}==
As with C# the [http://research.microsoft.com/en-us/um/cambridge/projects/fsharp/manual/spec.html#_Toc207705761 F# Language Specification] refers to [http://www.unicode.org/reports/tr31/#Default_Identifier_Syntax Unicode Standard Annex #31] for identifier syntax, allowing Unicode letter characters.

```fsharp
let mutable Δ = 1
Δ <- Δ + 1
printfn "%d" Δ
```



## Factor

Variable names can contain any character, inlcuding unicode characters, as long as they don't parse as a string or a number.

```factor
USE: locals
[let
    1 :> Δ!
    Δ 1 + Δ!
    Δ .
]
```



## Forth

Historically, Forth has worked only in ASCII (going so far as to reserve the eighth bit for symbol smudging), but modern implementations (e.g., Gforth) allow UTF-8 in word names, strings and comments.

```forth
variable ∆
1 ∆ !
1 ∆ +!
∆ @ .
```



## FreeBASIC

FreeBASIC does not allow non-ASCII characters in variable names or identifiers generally.

The only ASCII characters allowed are numerals (0-9), letters (a-z, A-Z) and the underscore(_).

However, identifiers cannot begin with a numeral.

If one wanted to use a Greek character such as Δ for a variable name, it would therefore have to be spelled out :


```freebasic
'FB 1.05.0 Win64

Var delta = 1
delta += 1
Print delta  '' 2
Sleep
```



## Frink

Frink can use Unicode variable names that meet certain constraints.  Variable names that don't meet these constraints can still be parsed and displayed by specifying them as Unicode escapes:  [https://frinklang.org/#UnicodeInFrink Unicode Variable Names]

```frink

Δ = 1
Δ = Δ + 1
println[Δ]

```



## Go

Go source encoding is [http://golang.org/doc/go_spec.html#Source_code_representation specified] to be UTF-8.  Allowable variable names are specified in the sections [http://golang.org/doc/go_spec.html#Identifiers identifiers] and [http://golang.org/doc/go_spec.html#Exported_identifiers Exported identifiers].

```go
package main

import "fmt"

func main() {
    Δ := 1
    Δ++
    fmt.Println(Δ)
}
```

```txt

2

```



## Groovy

The Groovy solution for [[Arithmetic/Complex#Groovy|Arithmetic/Complex]] demonstrates a number of Unicode variable names


## Haskell

Haskell variables must start with a lower case character, however Δ is an upper case delta. As such, lower case delta (δ) was used as the first character instead, followed by an upper case delta as the second character in the variable name.

Also, Haskell does not allow mutable variables, so incrementing delta isn't possible. Instead lower case psi was used to store the incremented value of delta since tridents are cool.

```Haskell
main = print ψ
    where δΔ = 1
          ψ = δΔ + 1
```



## J

Variable names must be comprised of ASCII characters.

From the Dictionary page [http://www.jsoftware.com/help/dictionary/dict1.htm Alphabet and Words]:

:"The alphabet is standard ASCII, comprising digits, letters (of the English alphabet), the underline (used in names and numbers), ..."

:"Names ... begin with a letter and may continue with letters, underlines, and digits."


## Java


```java
int Δ = 1;
double π = 3.141592;
String 你好 = "hello";
Δ++;
System.out.println(Δ);
```

```txt

2

```



## JavaScript


```javascript
var ᾩ = "something";
var ĦĔĽĻŎ = "hello";
var 〱〱〱〱 = "too less";
var जावास्क्रिप्ट = "javascript"; // ok that's JavaScript in hindi
var KingGeorgeⅦ = "Roman numerals.";

console.log([ᾩ, ĦĔĽĻŎ, 〱〱〱〱, जावास्क्रिप्ट, KingGeorgeⅦ])
```

```txt

["something", "hello", "too less", "javascript", "Roman numerals."]

```



## jq

Apart from the initial "$", the characters allowed in so-called "$ variables" in jq are restricted to alphanumeric characters and the underscore: [A-Za-z0-9_].

However, in practice, the keys of JSON objects can also be used as variable names.  For example, in the following expression, "Δ" is in effect set to 1 and then its value is retrieved in the environment in which "Δ" has been set:

```jq
{ "Δ": 1 } | .["Δ"]
```


In jq 1.5 and later,
```jq
.["Δ"]
```
 can be abbreviated to
```jq
."Δ"
```


Strictly speaking, variables in jq cannot be incremented (in fact, strictly speaking, jq does not have variables at all), but the equivalent operation is illustrated here:


```jq
{ "Δ": 1 }    # initialization
| .["Δ"] += 1 # increment by 1
| .["Δ"]      # emit the incremented value
```



## Julia

The Julia documentation on
[http://docs.julialang.org/en/latest/manual/variables/#allowed-variable-names allowed variable names] explicitly describes the wide variety of Unicode codepoints that are allowed:

```Julia>julia
 Δ = 1 ; Δ += 1 ; Δ
2
```

The allowed identifiers also include sub/superscripts and combining characters (e.g. accent marks):

```julia>julia
 Δ̂₂ = Δ^2
4
```

and the Julia interactive shells (and many editors) allow typing these symbols via tab-completion of their LaTeX abbreviations.


## Kotlin


```scala
fun main(args: Array<String>) {
    var Δ = 1
    Δ++
    print(Δ)
}
```


```txt

2

```



## Lingo

Since version 11, in Lingo/Director both native strings and scripts use UTF-8 encoding. Variable names support Unicode characters:

```lingo
Δ = 1
Δ = Δ + 1
put Δ
-- 2
```



## LiveCode

In LiveCode 7+ all characters are stored as unicode. This includes variable (container) names, although it does not seem to state this in the LC dictionary.

```LiveCode
put 1 into Δ
add 1 to Δ
put Δ
-- result is 2
```



## Lily


```Lily
var Δ = 1
Δ += 1
print(Δ.to_s())
```



## LOLCODE

The [http://lolcode.com/specs/1.2#variables spec] mandates that identifiers be alphanumeric. However, the fact that [http://lolcode.com/specs/1.2#strings YARNs] are Unicode-aware permits the use of the [http://lolcode.com/proposals/1.3/bukkit2#srs-serious-cast SRS operator] introduced in 1.3 to utilize variables of arbitrary name.

```LOLCODE
I HAS A SRS "Δ" ITZ 1
SRS "Δ" R SUM OF SRS ":(394)" AN 1
VISIBLE SRS ":[GREEK CAPITAL LETTER DELTA]"
```

```txt
2
```



## Lua

Lua 5.3 supports UTF-8 encoding as documented here: https://www.lua.org/manual/5.3/manual.html#6.5 .
However, this support is not strictly necessary for this task so long as the Lua script is edited using a UTF-8 enabled text editor.

```Lua
∆ = 1
∆ = ∆ + 1
print(∆)
```

```txt
2
```

This output was produced using LuaJIT, which implements Lua 5.1.  This works because although Lua doesn't 'understand' the delta character, it still resolves to a consistent set of bytes.  This string is "Γêå" in ASCII but the programmer does not need to be aware of that; the unicode variable name works just like any other.


## M2000 Interpreter


```M2000 Interpreter

Δ=1
Δ++
Print Δ

```



## Mathematica


```Mathematica
Δ = 1;
Δ++;
Print[Δ]
```



## Nemerle

From the Nemerle [http://nemerle.org/wiki/index.php?title=Lexical_structure_%28ref%29 Reference Manual]: "Programs are written using the Unicode character set, using the UTF-8 encoding."

```Nemerle
using System.Console;

module UnicodeVar
{
    Main() : void
    {
        mutable Δ = 1;
        Δ++;
        WriteLine($"Δ = $Δ");
    }
}
```



## NetRexx

The ''NetRexx Language Definition'' section of the NetRexx documentation ([http://netrexx.org/files/nrl3.pdf netrexx.org/files/nrl3.pdf]) describes the character set support within the language.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

upperΔ = 1
Δupper = upperΔ
lowerδ = 2
δlower = lowerδ

say upperΔ '+' Δupper '= \-'
upperΔ = upperΔ + Δupper
say upperΔ

say lowerδ '+' δlower '= \-'
lowerδ = lowerδ + δlower
say lowerδ
say

-- Unicode works with the NetRexx built-in functions
Υππερ = '\u0391'.sequence('\u03a1') || '\u03a3'.sequence('\u03a9') -- ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ
Λοωερ = '\u03b1'.sequence('\u03c1') || '\u03c3'.sequence('\u03c9') -- αβγδεζηθικλμνξοπρστυφχψω
say Υππερ'.Lower =' Υππερ.lower()
say Λοωερ'.Upper =' Λοωερ.upper()
say

-- Note: Even with unicode characters NetRexx variables are case-insensitive
numeric digits 12
δ = 20.0
π = Math.PI
θ = Π * Δ
σ = Θ ** 2 / (Π * 4) -- == Π * (Δ / 2) ** 2
say 'Π =' π', diameter =' δ', circumference =' Θ', area =' Σ

return

```

'''Output:'''

```txt

1 + 1 = 2
2 + 2 = 4

ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ.Lower = αβγδεζηθικλμνξοπρστυφχψω
αβγδεζηθικλμνξοπρστυφχψω.Upper = ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ

Π = 3.141592653589793, diameter = 20.0, circumference = 62.8318530718, area = 314.159265359

```



## Nim

From the spec: http://nim-lang.org/docs/manual.html#identifiers-keywords


```Nimrod
var Δ = 1
Δ.inc()
echo(Δ)
```



## Objeck

As of 3.2, Objeck supports UTF-8 encoded I/O and stores characters in the runtime's native Unicode format.

```objeck

class Test {
  function : Main(args : String[]) ~ Nil {
    Δ := 1;
    π := 3.141592;
    你好 := "hello";
    Δ += 1;
    Δ->PrintLine();
  }
}

```



## PARI/GP

GP accepts only ASCII in strings and variable names.

PARI supports Unicode variable names only insofar as [[#C|C]] does.


## Peloton

1. (working on it)

2.

```sgml><@ LETVARLIT
Δ|1</@>
<@ ACTICRVAR>Δ</@>
<@ SAYVAR>Δ</@>
```

Using what Google Translate says is the Traditional Chinese for 'delta'

```sgml><@ LETVARLIT
三角洲|1</@>
<@ ACTICRVAR>三角洲</@>
<@ SAYVAR>三角洲</@>
```



## Perl

Requires Perl 5.8.1 at the minimum. See http://perldoc.perl.org/utf8.html


```perl
use utf8;

my $Δ = 1;
$Δ++;
print $Δ, "\n";
```


<code>$</code> sigil can be omitted by using [http://perldoc.perl.org/perlsub.html#Lvalue-subroutines lvalue] subroutine:


```perl
use utf8;

BEGIN {
    my $val;
    sub Δ () : lvalue {
        $val;
    }
}

Δ = 1;
Δ++;
print Δ, "\n";
```


or with Perl 5.10 and [http://perldoc.perl.org/functions/state.html state] modifier:


```perl
use utf8;
use v5.10;

sub Δ () : lvalue {
    state $val;
}

Δ = 1;
Δ++;
say Δ;
```


One can have Unicode in identifier or subroutine names and also in package or class names. Use of Unicode for the last two purposes is, due to file and directory names, dependent on the filesystem.


## Perl 6

Perl 6 is written in Unicode so, with narrow restrictions, nearly any Unicode letter can be used in identifiers.

See Perl 6 Synopsis 02. - http://perlcabal.org/syn/S02.html#Names

```perl6
my $Δ = 1;
$Δ++;
say $Δ;
```

Function and subroutine names can also use Unicode characters: (as can methods, classes, packages, whatever...)

```perl6
my @ᐁ = (0, 45, 60, 90);

sub π { pi };

sub postfix:<°>($degrees) { $degrees * π / 180 };

for @ᐁ -> $ಠ_ಠ { say sin $ಠ_ಠ° };

sub c͓͈̃͂̋̈̆̽h̥̪͕ͣ͛̊aͨͣ̍͞ơ̱͔̖͖̑̽ș̻̥ͬ̃̈ͩ { 'HE COMES' }
```




'''See Also:'''

[[Egyptian_division#More_.22Egyptian.22_version|Egyptian division]]


## Phix

Phix does not officially support unicode variable names, however it took me less than 5 minutes (changes, which are now permanent, labelled with "for rosettacode/unicode" in ptok.e and pttree.e, setting charset and identset respectively) to get the following to work, as long as the source file is stored using utf8 with a proper BOM, as supported by Notepad and the included Edita. I will happily add further character ranges as required/requested: I simply don't know what those ranges are, but I believe that no code points in utf8 should overlap existing ascii chars such as +-* etc.

```Phix
integer Δ = 1
    Δ += 1
    ?Δ
```

```txt

2

```



## PHP

PHP is not made to support Unicode. UTF-16 (UCS-2) will not work because it adds null bytes before or after ASCII characters (depending on endianness of UTF-16). As every code has to start with <code>&lt;?php</code> (ASCII) exactly, the parser doesn't find the match and just prints <code>&lt;?php</code> mark.

UTF-8 uses ASCII values for bytes which can be represented as ASCII and as result it's possible to insert <code>&lt;?php</code> mark at beginning. PHP sees your document as some 8-bit encoding (like ISO-8859-1), but it doesn't matter because UTF-8 doesn't use ASCII ranges for its values and calls to the variable are consistent.

Documentation: [http://php.net/manual/en/mbstring.php4.req.php mbstring.php4.req], [http://php.net/manual/en/language.variables.basics.php language.variables.basics]

```php
<?php
$Δ = 1;
++$Δ;
echo $Δ;
```



## PicoLisp

Variables are usually [http://software-lab.de/doc/ref.html#internal-io Internal Symbols], and their names may contain any UTF-8 character except null-bytes. White space, and 11 special characters (see the reference) must be escaped with a backslash. [http://software-lab.de/doc/ref.html#transient-io Transient Symbols] are often used as variables too, they follow the syntax of strings in other languages.

```PicoLisp
: (setq Δ 1)
-> 1
: Δ
-> 1
: (inc 'Δ)
-> 2
: Δ
-> 2
```



## PowerShell


```PowerShell

$Δ = 2
$π = 3.14
$π*$Δ

```

<b>Output:</b>

```txt

6.28

```



## Prolog


```prolog
% Unicode in predicate names:
是.            % be: means, approximately, "True".
不是 :- \+ 是.  % not be: means, approximately, "False".  Defined as not 是.

% Unicode in variable names:
test(Garçon, Δ) :-
  Garçon = boy,
  Δ = delta.

% Call test2(1, Result) to have 2 assigned to Result.
test2(Δ, R) :- R is Δ + 1.
```


Putting this into use:

```prolog
?- 是.
true.

?- 不是.
false.

?- test(X,Y).
X = boy,
Y = delta.

?- test2(1,Result).
Result = 2.
```



## Python

Within the ASCII range (U+0001..U+007F), the valid characters for identifiers are the same as in Python 2.x: the uppercase and lowercase letters A through Z, the underscore _ and, except for the first character, the digits 0 through 9.

Python 3.0 introduces additional characters from outside the ASCII range (see [http://www.python.org/dev/peps/pep-3131 PEP 3131]). For these characters, the classification uses the version of the Unicode Character Database as included in the unicodedata module.

Identifiers are unlimited in length. Case is significant.

```python>>>
 Δx = 1
>>> Δx += 1
>>> print(Δx)
2
>>>
```




## R

See <code>?assign</code> for details.


```Rsplus
f <- function(`∆`=1) `∆`+1

f(1)
```

```txt
[1] 2
```



## Racket


Racket has virtually no restrictions on valid characters for identifiers. In particular, Unicode identifiers are supported.


```Racket

#lang racket

;; Racket can use Unicode in identifier names
(define √ sqrt)
(√ 256) ; -> 16
;; and in fact the standard language makes use of some of these
(λ(x) x) ; -> an identity function

;; The required binding:
(define Δ 1)
(set! Δ (add1 Δ))
(printf "Δ = ~s\n" Δ) ; prints "Δ = 2"


```



## Retro

This has been tested on Retro 11.0 running under OS X.

```Retro
variable Δ
1 !Δ
@Δ putn
1 +Δ
@Δ putn
```

Function and variable names are stored as strings, and UTF-8 is usable, as long as the host system allows it.


## REXX

Note:   this REXX program   ''only''   works with the   '''R4'''   or   '''ROO'''   REXX interpreter under DOS or DOS under Windows.

This REXX program works because the   '''R4'''   and   '''ROO'''   REXX interpreters supports an extended character set.

```rexx
/*REXX program (using the R4 REXX interpreter) which uses a Greek delta char).*/
'chcp'  1253  "> NUL"                  /*ensure we're using correct code page.*/
Δ=1                                    /*define delta (variable name  Δ)  to 1*/
Δ=Δ+1                                  /*bump the delta REXX variable by unity*/
say 'Δ=' Δ                             /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

Δ= 2

```



## Ring


```ring

# Project : Unicode variable names

Δ = "Ring Programming Language"
see Δ + nl

```

Output:

```txt

Ring Programming Language

```



## Ruby

Ruby supports about 100 encodings, the default being UTF-8.

```ruby
Δ = 1
Δ += 1
puts Δ  # => 2
```



## Rust

Rust source encoding is [https://doc.rust-lang.org/reference.html#input-format specified] to be UTF-8. [https://doc.rust-lang.org/reference.html#identifiers Identifiers] must begin with a character that has Unicode XID_start property and remaining characters must have the XID_Continue property. (Which means that [https://github.com/mozilla/rust/issues/7048#issuecomment-19254166 ╯°□°╯︵┻━┻] is not permitted under current specification)

<b>Non-ASCII identifiers are [https://github.com/mozilla/rust/pull/10605 feature gated] since version 0.9</b>


```rust
#![feature(non_ascii_idents)]
#![allow(non_snake_case)]

fn main() {
    let mut Δ: i32 = 1;
    Δ += 1;
    println!("{}", Δ);
}
```


=={{header|S-lang}}==
S-Lang documentation is decent, but there are imperfections, and this
is a case in point.  The "Identifiers" chapter in the primary doc says
that an identifier must start with [A-Za-z$_] followed by zero or more
of [A-Za-z0-9$_].  (The chapter is probably a holdover from S-Lang 1.)

But in reality, any non-ascii Unicode character is legal in identifiers,
including the first character, as long as it's encoded in UTF-8.  [As an
aside, S-Lang includes iconv.sl to interface to the Gnu iconv library.  The
0.99.18 ms-win version of the S-Lang-powered [http://www.paneura.com/~dino/wjed.html#HISTORY Jed]
programmer's editor includes this to handle the UTF-16 files popular
with windows.]

The same document does give a hint in the "S-Lang 2 Interpreter NEWS"
appendix, which says "Native support for Unicode via UTF-8 throughout
the library" was added as of S-Lang 2.0.

As the doc isn't completely clear, I've used the requested non-ascii char
not just in variable but also function and reference names, and tested under
S-Lang versions 2.0.6 and pre2.3.1-23.
<lang S-lang>define ∆increment(∆ref) {
  @∆ref++;
}
variable foo∆bar = 1;
foo∆bar++;
variable ∆bar = 1;
∆bar++;
∆increment(&∆bar);
% foo∆bar should be 2 and ∆bar should be 3.
print(foo∆bar);
print(∆bar);

```

2
3


## Scala


```scala
var Δ = 1
val π = 3.141592
val 你好 = "hello"
Δ += 1
println(Δ)
```



## Sidef


```ruby
var Δ = 1;
Δ += 1;
say Δ;
```

```txt
2
```



## Stata


Here is how to create a macro, a scalar and a Mata variable named Δ:


```stata
sca Δ=10
sca Δ=Δ+1
di Δ

local Δ=20
local ++Δ
di `Δ'

mata
Δ=30
Δ++
Δ
end
```



## Swift


```swift
var Δ = 1
let π = 3.141592
let 你好 = "hello"
Δ++
println(Δ)
```

```txt

2

```



## Tcl

Tcl variable names can include any character <!-- but the <tt>::</tt> sequence is special — it is the namespace separator — and there are restrictions when parentheses are involved, as they are used for associative arrays; these are not matters that are in the spirit of this task though, so this is a comment! --> (the <code>$var</code> syntax can't, but that's just a shorthand for the operationally-equivalent <code>[set var]</code>). Thus, this script is entirely legal:

```tcl
set Δx 1
incr Δx
puts [set Δx]
```

However, this script only works smoothly if the “<tt>Δ</tt>” character is in the system's default encoding (thankfully more common than it used to be, as more and more systems use UTF-8 or UTF-16 as their default encodings) so normal Tcl practice is to stick to ASCII for identifier names.

It is also possible to encode characters using a <tt>\u''XXXX''</tt> substitution (each <tt>''X''</tt> is a hexadecimal digit), thus the <code>Δx</code> could be replaced throughout above by <code>\u0394x</code>; the result is a variable with exactly the same name as before. Doing this allows a script to be written with just ASCII characters, which tends to maximize portability across platforms.


## Vala

Vala has limited support for Unicode in variable names. This limitation comes from its source-to-source compilation to C.


## zkl

The short answer is zkl identifiers are a small subset of ASCII. This is enforced by the compiler. That said, the VM doesn't particularly care about names (although UTF-8 will cause sorting/etc issues). So ...

```zkl
delta:="\U0394;";  // UTF-8 delta
klass:=	// embryo(names, numFcns, numClasses, numParents, ...)
     self.embryo(L("","",delta),0,0,0).cook();
klass.setVar(0,Ref(1));  // indirect set since delta not valid var name
klass.vars.println();

dv:=klass.setVar(0);  // which actually gets the var, go figure
dv.inc();	      // ie (*ptr)++
dv.value.println();
```

```txt

L(L("Δ",Ref))
2

```

