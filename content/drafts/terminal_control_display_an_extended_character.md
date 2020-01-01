+++
title = "Terminal control/Display an extended character"
description = ""
date = 2019-07-30T12:24:46Z
aliases = []
[extra]
id = 8596
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}}

;Task:
Display an extended (non ASCII) character onto the terminal.

Specifically, display a   <big> £ </big>   (GBP currency sign).





## ACL2


```Lisp
(cw "£")
```



## Ada



```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

procedure Pound is
begin
   Put(Ada.Characters.Latin_1.Pound_Sign);
end Pound;
```


Ada allows Unicode characters in the source, and provides output functions on "wide characters".


```ada
with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;

procedure Unicode is
begin
   Put("札幌");
end Unicode;
```



## AWK


You can print a literal "£".


```awk
BEGIN { print "£" }
```


You can print a "£" using the escape sequences that match the encoding of your terminal.

{| class="wikitable"
! cp437
| <tt>"\234"</tt>
|-
! iso-8859-1
| <tt>"\243"</tt>
|-
! euc-jp
| <tt>"\241\362"</tt>
|-
! utf-8
| <tt>"\302\243"</tt>
|-
! gb18030
| <tt>"\201\60\204\65"</tt>
|}


```awk
BEGIN { print "\302\243" } # if your terminal is utf-8
```



## BASIC

=== {{header|Applesoft BASIC}} ===
Poke the glyph onto the hi-res screen.

```basic
10  DATA 56,68,4,14,4,4,122,0
20  HGR
30  FOR I = 8192 TO 16383 STEP 1024
40  READ B: POKE I,B: NEXT
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>PRINT "£"
```


or

<lang IS-BASIC>PRINT CHR$(35)
```


=== {{header|ZX Spectrum Basic}} ===
The ZX Spectrum uses a modified ascii character set that has a uk pound sign at character number 96:


```basic
10 PRINT CHR$(96);
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
You can print a literal £ if it is available in the default ANSI code page:

```bbcbasic
      PRINT "£"
```

But to be on the safe side you can do this:

```bbcbasic
      VDU 23,22,640;512;8,16,16,128+8 : REM Enable UTF-8 mode
      PRINT CHR$(&C2) CHR$(&A3)       : REM UTF-8 encoding for £
```



## bc

You can print a literal "£".


```bc
"£
"
quit
```



## beeswax


```beeswax
_4~9P.P.M}
```



## Befunge

There's no portable way to print an extended character in Befunge, since character output will typically use the default code page of the operating system or environment. On Windows this will often be [[wp:Windows-1252|Windows-1252]] or [[wp:ISO/IEC 8859-1|ISO-8859-1]] for GUI applications and [[wp:Code page 437|Code page 437]] for console applications (but that also likely depends on the OS localisation).

Example output of a pound character in Code page 437:

```befunge
"| "+,@
```


Example output of a pound character in ISO-8859-1:

```befunge
"%~"+,@
```



## Bracmat


```bracmat
put$£
```



## C

{{trans|AWK}}

```c
#include <stdio.h>

int
main()
{
	puts("£");
	puts("\302\243"); /* if your terminal is utf-8 */
	return 0;
}
```



## C sharp


```csharp
class Program
{
    static void Main()
    {
        System.Console.WriteLine("£");
    }
}
```

Output:

```txt
£
```



## C++



```cpp
#include <iostream>

int main()
{
    std::cout << static_cast<char>(163); // pound sign
    return 0;
}
```



## Clojure


```clojure
(println "£")
```



## COBOL

{{works with|OpenCOBOL}}


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Display-Pound.

       PROCEDURE DIVISION.
       DISPLAY "£"

       GOBACK
       .
```



## Common Lisp


```lisp

(format t "札幌~%")
(format t "~C~%" (code-char #x00A3))

```



## D

Assuming unicode support on the terminal

```d
import std.stdio;

void main() {
    writeln('\u00A3');
}
```


```txt
£
```



## Dc

Assuming unicode support on the terminal

```dc>49827 P</lang



## EchoLisp


```scheme

;; simplest
(display "£")
;; unicode character
(display "\u00a3")
;; HTML special character
(display "&pound;")
;; CSS enhancement
(display "£" "color:blue;font-size:2em")

```

{{out}}
<span style="color:blue;font-size:2em">
£
</span>


## Erlang

In Erlang a string is a list of integers. So the list of 196 is £.
{{out}}

```txt

8> Pound = [163].
9> io:fwrite( "~s~n", [Pound] ).
£

```



## Forth

{{works with|GNU Forth|0.7.0}}
The emerging ANS Forth 20xx standard includes an XCHAR wordset which allows manipulation of non-ASCII character sets such as Unicode.


```forth
163 xemit    \ , or
s" £" type
```



## Go


```go
package main

import "fmt"

func main() {
    fmt.Println("£")
}
```



## Haskell


```Haskell

module Main where
main = do
        putStrLn "£"
        putStrLn "札幌"

```


=={{header|Icon}} and {{header|Unicon}}==

Write a given character number, say '163', using <code>char</code> to convert the integer into a string.


```Icon

procedure main ()
  write ("£ " || char (163)) # £
end

```



## J


```J
   '£'
£
   '札幌'
札幌
```



## Java


```Java
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

public class Main
{
    public static void main(String[] args) throws UnsupportedEncodingException
    {
        PrintStream writer = new PrintStream(System.out, true, "UTF-8");
        writer.println("£");
        writer.println("札幌");
    }
}
```



## Julia

{{trans|C}}

```julia
println("£")
println("\302\243"); # works if your terminal is utf-8

```



## Kotlin


```scala
// version 1.1.2

fun main(args:Array<String>) = println("£")
```



## Lasso


```Lasso
stdout(' £ ')
```

Result:

```txt
 £
```



## Locomotive Basic



```locobasic
10 PRINT CHR$(163)
```



## Lua

Lua requires an extension module for UTF-8 support.  However, the '£' symbol specified for this task is part of extended ASCII (codes 128 - 255) which can be accessed in the same way as normal ASCII.

```Lua
print(string.char(156))
```



## Mathematica


```Mathematica
FromCharacterCode[{163}]
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  GBP = '\u00a3' -- unicode code point
  say GBP
  GBP = '£' -- if the editor's up to it
  say GBP
  GBP = 16x00a3 -- yet another way
  say (Rexx GBP).d2c
  return

```

{{out}}

```txt

£
£
£

```



## Nim


```nim
echo "£"
echo "札幌"

import unicode
echo Rune(0xa3)
```



## Objeck


```objeck
class Program {
  function : Main(args : String[]) ~ Nil {
    "£"->PrintLine();
  }
}
```



## Pascal


```Pascal
program pound;
uses crt;
begin
  write(chr( 163 ));
end.

```



## Perl


```perl
use feature 'say';

# OK as is
say '￡';

# these need 'binmode needed to surpress warning about 'wide' char
binmode STDOUT, ":utf8";
say "\N{FULLWIDTH POUND SIGN}";
say "\x{FFE1}";
say chr 0xffe1;
```



## Perl 6

To demonstrate we're not limited to Latin-1, we'll print the fullwidth variant.

```perl6
say '￡';
say "\x[FFE1]";
say "\c[FULLWIDTH POUND SIGN]";
0xffe1.chr.say;
```



## Phix

On Windows (Linux should be fine), you may need to set the terminal to a truetype font (eg Lucida Console) and the code page to CP_UTF8 (chcp 65001).

See demo\HelloUTF8.exw for a (not very pretty) way to do that programmaticaly.

The following assumes you have done that manually, and saved the source code file in UTF-8 format.

```Phix
puts(1,"£")
```

Output:

```txt
£
```



## PicoLisp


```PicoLisp
(prinl (char 26413) (char 24140))  # Sapporo
```

Output:

```txt
札幌
```



## PL/I


```PL/I
   declare pound character (1) static initial ('9c'x);
   put skip list (pound);
```



## PureBasic


```PureBasic
Print(Chr(163))
```


```txt
£
```



## Python


```Python
print u'\u00a3'
```


```txt
£
```



## Racket


```racket

#lang racket
(display "£")

```



## REXX


```rexx
/*REXX program demonstrates displaying an extended character (glyph) to the terminal.   */
                            /* [↓]  this will display the £ glphy (if term supports it).*/
say '£'                     /*assuming the pound sign glyph is displayable on the term. */
                            /* [↑]  this can execute on an  EBCDIC  or  ASCII  machine. */
                                                 /*stick a fork in it,  we're all done. */
```

{{out|output|text=:}}

```txt

£

```



## Ring


```ring

# Project : Terminal control/Display an extended character

see "£"

```

Output:

```txt

£

```



## Ruby


```ruby
#encoding: UTF-8  #superfluous in Ruby > 1.9.3
puts "£"
```


## Scala

{{libheader|Scala}}
```Scala
object ExtendedCharacter extends App {
  println("£")
  println("札幌")
}
```



## Seed7

A [http://seed7.sourceforge.net/libraries/console.htm#write%28ref_console_file,in_string%29 write]
to a [http://seed7.sourceforge.net/libraries/console.htm console] accepts Unicode characters.
```seed7
$ include "seed7_05.s7i";
  include "console.s7i";

const proc: main is func
  local
    var text: console is STD_NULL;
  begin
    console := open(CONSOLE);
    write(console, "£");
    # Terminal windows often restore the previous
    # content, when a program is terminated. Therefore
    # the program waits until Return/Enter is pressed.
    readln;
  end func;
```



## Sidef


```ruby
say '￡';
say "\x{FFE1}";
say "\N{FULLWIDTH POUND SIGN}";
say 0xffe1.chr;
```



## Tcl

Provided the system encoding has a “£” symbol in it, this works:

```tcl>puts \u00a3</lang

Tcl can output all unicode characters in the BMP, but only if the consumer of the output (terminal, etc.) is able to understand those characters in its current encoding will the output actually make sense. Strictly, this is not a limitation of Tcl but of the environment in which it is placed.


## Xidel

http://videlibri.sourceforge.net/xidel.html

```bash
xidel -s -e 'parse-html("&#163; or &#xa3")'
£ or £
```


```bash
echo '"\u00a3"' | xidel -s - -e 'json($raw)'
£

xidel -s -e 'json("""\\u00a3""")' --xquery 'json("&quot;\\u00a3&quot;")'
£
£
```



## XPL0


```XPL0
code ChOut=8;
ChOut(0, $9C)             \code for IBM PC's extended (OEM) character set

```



## zkl

If you output device support UTF-8 then:

```zkl
"\u00a3 \Ua3;".println() //-->£ £
```



[[Category:Terminal control]]

{{omit from|Axe}}
