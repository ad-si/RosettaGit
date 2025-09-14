+++
title = "Generate lower case ASCII alphabet"
description = ""
date = 2019-10-18T10:29:54Z
aliases = []
[extra]
id = 15789
[taxonomies]
categories = ["task", "String_manipulation"]
tags = []
languages = [
  "0815",
  "360_assembly",
  "6502_assembly",
  "8th",
  "abap",
  "ada",
  "algol_68",
  "algol_w",
  "apl",
  "applescript",
  "applesoft_basic",
  "arturo",
  "ats",
  "autohotkey",
  "autoit",
  "awk",
  "basic",
  "basic256",
  "batch_file",
  "bbc_basic",
  "befunge",
  "bracmat",
  "burlesque",
  "c",
  "clojure",
  "cobol",
  "coffeescript",
  "commodore_basic",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dc",
  "delphi",
  "dup",
  "dyalect",
  "echolisp",
  "elena",
  "elixir",
  "erlang",
  "factor",
  "false",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "huginn",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "k",
  "kotlin",
  "lc3_assembly",
  "lingo",
  "logo",
  "lua",
  "m2000_interpreter",
  "maple",
  "maxima",
  "mercury",
  "miniscript",
  "mumps",
  "neko",
  "nesl",
  "nim",
  "ocaml",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "smalltalk",
  "snobol",
  "spl",
  "standard_ml",
  "stata",
  "supercollider",
  "swift",
  "tcl",
  "ubasic_4th",
  "unix_shell",
  "ursa",
  "vba",
  "vbscript",
  "vim_script",
  "visual_basic",
  "visual_basic_dotnet",
  "xeec",
  "xlisp",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Generate an array, list, lazy sequence, or even an indexable string of all the lower case ASCII characters, from <big> a </big> to <big> z.</big> If the standard library contains such a sequence, show how to access it, but don't fail to show how to generate a similar sequence.

For this basic task use a reliable style of coding, a style fit for a very large program, and use strong typing if available. It's bug prone to enumerate all the lowercase characters manually in the code. During code review it's not immediate obvious to spot the bug in a Tcl line like this contained in a page of code:

```tcl
set alpha {a b c d e f g h i j k m n o p q r s t u v w x y z}
```






## 0815

This creates the list in the queue

```0815
<:61:~}:000:>>&{~<:7a:-#:001:<:1:+^:000:
```



## 360 Assembly

In EBCDIC coding there are more than 24 characters between a and z.
So we have to get rid of characters between i and j and also between r and s.

```360asm
*        Generate lower case alphabet - 15/10/2015
LOWER    CSECT
         USING  LOWER,R15          set base register
         LA     R7,PG              pgi=@pg
         SR     R6,R6              clear
         IC     R6,=C'a'           char='a'
         BCTR   R6,0               char=char-1
LOOP     LA     R6,1(R6)           char=char+1
         STC    R6,CHAR
         CLI    CHAR,C'i'          if char>'i'
         BNH    OK
         CLI    CHAR,C'j'          and char<'j'
         BL     SKIP               then skip
         CLI    CHAR,C'r'          if char>'r'
         BNH    OK
         CLI    CHAR,C's'          and char<'s'
         BL     SKIP               then skip
OK       MVC    0(1,R7),CHAR       output char
         LA     R7,1(R7)           pgi=pgi+1
SKIP     CLI    CHAR,C'z'          if char='z'
         BNE    LOOP               loop
         XPRNT  PG,26              print buffer
         XR     R15,R15            set return code
         BR     R14                return to caller
CHAR     DS     C                  character
PG       DS     CL26               buffer
         YREGS
         END    LOWER
```

```txt

abcdefghijklmnopqrstuvwxyz

```



## 6502 Assembly

Stores the lower-case ASCII alphabet as a null-terminated string beginning at address 2000 hex. Register contents are preserved.

```asm6502
ASCLOW: PHA             ; push contents of registers that we
        TXA             ; shall be using onto the stack
        PHA
        LDA   #$61      ; ASCII "a"
        LDX   #$00
ALLOOP: STA   $2000,X
        INX
        CLC
        ADC   #$01
        CMP   #$7B      ; have we got beyond ASCII "z"?
        BNE   ALLOOP
        LDA   #$00      ; terminate the string with ASCII NUL
        STA   $2000,X
        PLA             ; retrieve register contents from
        TAX             ; the stack
        PLA
        RTS             ; return
```



## 8th

We take an empty string, and use the "loop" word to create a new character using "'a n:+".  The loop passes the current index to the code being iterated, so it starts with 0 and up to 25, adding to the "'a" - which is the numeric value of lowercase "a", and the resultant number is then appended to the string.  That converts the number to the appropriate character and appends it:

```forth

"" ( 'a n:+ s:+ ) 0 25 loop
. cr

```

```txt

abcdefghijklmnopqrstuvwxyz

```



## ABAP


###  Example with simple write statement


```ABAP
REPORT lower_case_ascii.

WRITE: / to_lower( sy-abcde ).
```


=== Example with / without space using CL_DEMO_OUTPUT class ===

```ABAP
REPORT lower_case_ascii.

cl_demo_output=>new(
          )->begin_section( |Generate lower case ASCII alphabet|
          )->write( REDUCE string( INIT out TYPE string
                                    FOR char = 1 UNTIL char > strlen( sy-abcde )
                                   NEXT out = COND #( WHEN out IS INITIAL THEN sy-abcde(1)
                                                      ELSE |{ out } { COND string( WHEN char <> strlen( sy-abcde ) THEN sy-abcde+char(1) ) }| ) )
          )->write( |Or use the system field: { sy-abcde }|
          )->display( ).
```



## Ada


We start with a strong type definition: A character range that can only hold lower-case letters:


```Ada
   type Lower_Case is new Character range 'a' .. 'z';
```


Now we define an array type and initialize the Array A of that type with the 26 letters:

```Ada
   type Arr_Type is array (Integer range <>) of Lower_Case;
   A : Arr_Type (1 .. 26) := "abcdefghijklmnopqrstuvwxyz";
```


Strong typing would catch two errors: (1) any upper-case letters or other symbols in the string assigned to A, and (2) too many or too few letters assigned to A. However, a letter might still appear twice (or more) in A, at the cost of one or more other letters. Array B is safe even against such errors:


```Ada
   B : Arr_Type (1 .. 26);
begin
   B(B'First) := 'a';
   for I in B'First .. B'Last-1 loop
      B(I+1) := Lower_Case'Succ(B(I));
   end loop; -- now all the B(I) are different
```



## ALGOL 68

```algol68
    # in ALGOL 68, a STRING is an array of characters with flexible bounds #
    # so we can declare an array of 26 characters and assign a string      #
    # containing the lower-case letters to it                              #

    [ 26 ]CHAR lc := "abcdefghijklmnopqrstuvwxyz"

```

Alternative version

```algol68
    # fills lc with the 26 lower-case letters, assuming that               #
    # they are consecutive in the character set, as they are in ASCII      #

    [ 26 ]CHAR lc;

    FOR i FROM LWB lc TO UPB lc
    DO
        lc[ i ] := REPR ( ABS "a" + ( i - 1 ) )
    OD
```



## ALGOL W


```algolw
    % set lc to the lower case alphabet          %
    string(26) lc;
    for c := 0 until 25 do lc( c // 1 ) := code( decode( "a" ) + c );
```



## APL

```apl
      ⎕UCS 96+⍳26
```



## AppleScript



```AppleScript
on run

    {enumFromTo("a", "z"), ¬
        enumFromTo("🐐", "🐟")}

end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

-- enumFromTo :: Enum a => a -> a -> [a]
on enumFromTo(m, n)
    if class of m is integer then
        enumFromToInt(m, n)
    else
        enumFromToChar(m, n)
    end if
end enumFromTo

-- enumFromToChar :: Char -> Char -> [Char]
on enumFromToChar(m, n)
    set {intM, intN} to {id of m, id of n}
    set xs to {}
    repeat with i from intM to intN by signum(intN - intM)
        set end of xs to character id i
    end repeat
    return xs
end enumFromToChar

-- signum :: Num -> Num
on signum(x)
    if x < 0 then
        -1
    else if x = 0 then
        0
    else
        1
    end if
end signum
```

```AppleScript
{{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
"n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"},
{"🐐", "🐑", "🐒", "🐓", "🐔", "🐕", "🐖", "🐗",
"🐘", "🐙", "🐚", "🐛", "🐜", "🐝", "🐞", "🐟"}}
```



## Applesoft BASIC


```ApplesoftBasic
L$="abcdefghijklmnopqrstuvwxyz"
```

On the older model Apple II and Apple II plus, it is difficult to enter lower case characters.  The following code generates the same string:

```ApplesoftBasic
L$="":FORI=1TO26:L$=L$+CHR$(96+I):NEXT
```



## Arturo



```arturo
lcase $(map $(range 97 122) { char & })

print lcase
```


```txt
#("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
```



## ATS


```ATS

(* ****** ****** *)
//
// How to compile:
//
// patscc -DATS_MEMALLOC_LIBC -o lowercase lowercase.dats
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

implement
main0 () =
{
//
val N = 26
//
val A =
arrayref_tabulate_cloref<char>
(
  i2sz(N), lam(i) => int2char0(char2int0('a') + sz2i(i))
) (* end of [val] *)
//
} (* end of [main0] *)

```



## AutoHotkey

```AutoHotkey
a :={}
Loop, 26
	a.Insert(Chr(A_Index + 96))
```



## AutoIt


```AutoIt

Func _a2z()
	Local $a2z = ""
	For $i = 97 To 122
		$a2z &= Chr($i)
	Next
	Return $a2z
EndFunc

```



## AWK

Generate all character codes, and test each one if it matches
the POSIX character class for "lowercase".

Note this is dependent on the locale-setting,
and options, e.g.  --traditional  and  --posix

```AWK

# syntax: GAWK -f GENERATE_LOWER_CASE_ASCII_ALPHABET.AWK
BEGIN {
    for (i=0; i<=255; i++) {
      c = sprintf("%c",i)
      if (c ~ /[[:lower:]]/) {
        lower_chars = lower_chars c
      }
    }
    printf("%s %d: %s\n",ARGV[0],length(lower_chars),lower_chars)
    exit(0)
}

```

```txt

gawk_3_1_8 26: abcdefghijklmnopqrstuvwxyz
gawk_4_1_0 65: abcdefghijklmnopqrstuvwxyzƒsozªµºßàáâaäåæçèéêëìíîïdñòóôoöoùúûüy_ÿ

```



## BASIC

=
## BBC BASIC
=

```bbcbasic
      DIM lower&(25)
      FOR i%=0TO25
        lower&(i%)=ASC"a"+i%
      NEXT
      END
```


=
## BASIC256
=

```basic256

# generate lowercase ascii alphabet
# basic256 1.1.4.0

dim a$(27)                      # populating array for possible future use

for i = 1 to 26
    a$[i] = chr(i + 96)
    print a$[i] + " ";
next i

```

```txt

a b c d e f g h i j k l m n o p q r s t u v w x y z

```


=
## Commodore BASIC
=

```gwbasic
10 FOR I=ASC("A") TO ASC("Z")
20 A$ = A$+CHR$(I)
30 NEXT
40 PRINT CHR$(14) : REM 'SWITCH CHARACTER SET TO LOWER/UPPER CASES
50 PRINT A$
```


=
## FreeBASIC
=

```freebasic
' FB 1.05.0 Win64

' Create a string buffer to store the alphabet plus a final null byte
Dim alphabet As Zstring * 27

' ASCII codes for letters a to z are 97 to 122 respectively
For i As Integer = 0 To 25
  alphabet[i] = i + 97
Next

Print alphabet
Print
Print "Press any key to quit"
Sleep

```


```txt

abcdefghijklmnopqrstuvwxyz

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 STRING ALPHA$*26
110 LET ALPHA$=""
120 FOR I=ORD("a") TO ORD("z")
130   LET ALPHA$=ALPHA$&CHR$(I)
140 NEXT
150 PRINT ALPHA$
```


=
## PureBasic
=

```purebasic
Dim lower_case('z' - 'a') ;indexing goes from 0 -> 25
For i = 0 To ArraySize(lower_case())
  lower_case(i) = i + 'a'
Next
```


=
## Run BASIC
=

```Runbasic
for i = asc("a") to asc("z")
 print chr$(i);
next i
```
Output:

```txt
abcdefghijklmnopqrstuvwxyz
```


=
## uBasic/4tH
=
<lang>For x= ORD("a") To ORD("z") : @(x - ORD("a")) = x : Next
```


=
## ZX Spectrum Basic
=
```zxbasic
10 DIM l$(26): LET init= CODE "a"-1
20 FOR i=1 TO 26
30 LET l$(i)=CHR$ (init+i)
40 NEXT i
50 PRINT l$
```



## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

:: This code appends the ASCII characters from 97-122 to %alphabet%, removing any room for error.

for /l %%i in (97,1,122) do (
  cmd /c exit %%i
  set "alphabet=!alphabet! !=exitcodeAscii!"
)
echo %alphabet%
pause>nul

```

```txt

 a b c d e f g h i j k l m n o p q r s t u v w x y z

```



## Befunge

The left hand side pushes the sequence 'a' to 'z' onto the stack in reverse order with a null terminator (a fairly typical Befunge pattern). The right hand side is just printing it out again to test.

```Befunge
0"z":>"a"`#v_  >:#,_$@
     ^:- 1:<
```



## Bracmat


```bracmat
  a:?seq:?c
&   whl
  ' ( chr$(asc$!c+1):~>z:?c
    & !seq !c:?seq
    )
& !seq
```


=={{header|Brainfuck}}==


```bf
Make room for 26 characters
>>>>>>>>>>>>>
>>>>>>>>>>>>>
Set counter to 26
>>
+++++++++++++
+++++++++++++
Generate the numbers 1 to 26
[-<<    Decrement counter
  [+<]  Add one to each nonzero cell moving right to left
  +     Add one to first zero cell encountered
  [>]>  Return head to counter
]
<<
Add 96 to each cell
[
++++++++++++++++
++++++++++++++++
++++++++++++++++
++++++++++++++++
++++++++++++++++
++++++++++++++++
<]
Print each cell
>[.>]
++++++++++. \n
```


Uncommented:

```bf>>>>>>>>>>>>>>>>>>>>>>>>>>>>
++++++++++++++++++++++++++[-<<[+<]
+[>]>]<<[+++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++++++++++++++++++++++++<]>[.>]++++++++++.
```


```txt
abcdefghijklmnopqrstuvwxyz
```



## Burlesque


```burlesque
blsq ) @azr\sh
abcdefghijklmnopqrstuvwxyz
```



## C


```cpp
#include <iostream>

#define N 26

int main() {
    unsigned char lower[N];

    for (size_t i = 0; i < N; i++) {
        lower[i] = i + 'a';
    }

    return EXIT_SUCCESS;
}

```



## C++

C++ can do the task in the identical way as C, or else, it can use a STL function.
```cpp
#include <string>
#include <numeric>

int main() {
    std::string lower(26,' ');

    std::iota(lower.begin(), lower.end(), 'a');
}
```



## C#

Simple Linq 1 liner solution

```c#
using System;
using System.Linq;

internal class Program
{
    private static void Main()
    {
        Console.WriteLine(String.Concat(Enumerable.Range('a', 26).Select(c => (char)c)));
    }
}
```
```txt
abcdefghijklmnopqrstuvwxyz
```


Old style Property and enumerable based solution

```c#
namespace RosettaCode.GenerateLowerCaseASCIIAlphabet
{
    using System;
    using System.Collections.Generic;

    internal class Program
    {
        private static IEnumerable<char> Alphabet
        {
            get
            {
                for (var character = 'a'; character <= 'z'; character++)
                {
                    yield return character;
                }
            }
        }

        private static void Main()
        {
            Console.WriteLine(string.Join(string.Empty, Alphabet));
        }
    }
}
```
```txt
abcdefghijklmnopqrstuvwxyz
```



## Clojure


```clojure
(map char (range (int \a) (inc (int \z))))
```

```txt

(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z)

```



## COBOL

Strings in COBOL are mutable and can be subscripted: each time we go round the loop, we assign to a one-character-long section of the string we are building.

```cobol
identification division.
program-id. lower-case-alphabet-program.
data division.
working-storage section.
01  ascii-lower-case.
    05 lower-case-alphabet pic a(26).
    05 character-code      pic 999.
    05 loop-counter        pic 99.
procedure division.
control-paragraph.
    perform add-next-letter-paragraph varying loop-counter from 1 by 1
    until loop-counter is greater than 26.
    display lower-case-alphabet upon console.
    stop run.
add-next-letter-paragraph.
    add 97 to loop-counter giving character-code.
    move function char(character-code) to lower-case-alphabet(loop-counter:1).
```

```txt
abcdefghijklmnopqrstuvwxyz
```



## CoffeeScript


```coffeescript

(String.fromCharCode(x) for x in [97..122])

```



## Common Lisp

<nowiki>;; as a list</nowiki>

```lisp
(defvar *lower*
  (loop with a = (char-code #\a)
        for i below 26
        collect (code-char (+ a i))))
```


<nowiki>;; as a string</nowiki>

```lisp
(defvar *lowercase-alphabet-string*
  (map 'string #'code-char (loop
			      for c from (char-code #\a) to (char-code #\z)
			      collect c))
  "The 26 lower case letters in alphabetical order.")
```


<nowiki>;; verify</nowiki>

```lisp
(assert (= 26 (length *lowercase-alphabet-string*) (length *lower*)))
(assert (every #'char< *lowercase-alphabet-string* (subseq *lowercase-alphabet-string* 1)))
(assert (apply #'char< *lower*))
(assert (string= *lowercase-alphabet-string* (coerce *lower* 'string)))
```



## D

The lower case ASCII letters of the Phobos standard library:

```d
import std.ascii: lowercase;

void main() {}
```


The generation of the ASCII alphabet array:

```d
void main() {
    char['z' - 'a' + 1] arr;

    foreach (immutable i, ref c; arr)
        c = 'a' + i;
}
```


An alternative version:

```d
void main() {
    import std.range, std.algorithm, std.array;

    char[26] arr = 26.iota.map!(i => cast(char)('a' + i)).array;
}
```

Another version:

```d
void main() {
    char[] arr;

    foreach (immutable char c; 'a' .. 'z' + 1)
        arr ~= c;

    assert(arr == "abcdefghijklmnopqrstuvwxyz");
}
```



## Dc

Construct the numerical representation of the desired output and print it.

```dc
122 [ d 1 - d 97<L 256 * + ] d sL x P
```

Output:

```txt

abcdefghijklmnopqrstuvwxyz

```



## Delphi


```delphi
program atoz;

var
  ch : char;

begin
  for ch in ['a'..'z'] do
  begin
    write(ch);
  end;
end.
```
```txt
abcdefghijklmnopqrstuvwxyz
```



## DUP


In DUP, strings between double quotes are stored in a numerically addressed array. The integer before the first <code>"</code> which gets pushed on the data stack, defines the cell address in which the ASCII value of first character of the string will be stored. All following characters will be stored like an array as values in the following cells. At the end, DUP pushes the length of the string on the data stack.


```DUP
0"abcdefghijklmnopqrstuvwxyz"        {store character values of string in cells 0..length of string-1}
26[$][^^-;,1-]#                      {Loop from 26-26 to 26-0, print the respective cell contents to STDOUT}
```


Output:

<code>abcdefghijklmnopqrstuvwxyz</code>


## Dyalect


Generates a lazy sequence and prints it to a standard output:


```dyalect
print('a'..'z')
```



## EchoLisp


```scheme

;; 1)
(define \a (first (string->unicode "a")))
(for/list ((i 25)) (unicode->string (+ i \a)))
    → (a b c d e f g h i j k l m n o p q r s t u v w x y)

;;2) using a sequence
(lib 'sequences)

(take ["a" .. "z"] 26)
    → (a b c d e f g h i j k l m n o p q r s t u v w x y z)

; or
(for/string ((letter ["a" .. "z"])) letter)
    → abcdefghijklmnopqrstuvwxyz

```



## Elena

ELENA 4.1 :

```elena
import extensions;
import system'collections;

singleton Alphabet : Enumerable
{
    Enumerator enumerator() = new Enumerator::
    {
        char current;

        get() = current;

        bool next()
        {
            if (nil==current)
            {
                current := $97
            }
            else if (current != $122)
            {
                current := (current.toInt() + 1).toChar()
            }
            else
            {
                ^ false
            };

            ^ true
        }

        reset()
        {
            current := nil
        }

        enumerable() = self;
    };
}

public program()
{
    console.printLine(Alphabet)
}
```

```txt

a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z

```



## Elixir


```elixir
iex(1)> Enum.to_list(?a .. ?z)
'abcdefghijklmnopqrstuvwxyz'
iex(2)> Enum.to_list(?a .. ?z) |> List.to_string
"abcdefghijklmnopqrstuvwxyz"
```



## Erlang


```erlang
lists:seq($a,$z).
```


```txt
"abcdefghijklmnopqrstuvwxyz"
```


=={{header|F_Sharp|F#}}==

```fsharp
let lower = ['a'..'z']

printfn "%A" lower
```



## Factor

Strings are represented as fixed-size mutable sequences of Unicode code points.


```factor
USING: spelling ; ! ALPHABET

ALPHABET print
0x61 0x7A [a,b] >string print
: russian-alphabet-without-io ( -- str ) 0x0430 0x0450 [a,b) >string ;
: russian-alphabet ( -- str ) 0x0451 6 russian-alphabet-without-io insert-nth ;
russian-alphabet print
```

```txt
abcdefghijklmnopqrstuvwxyz
abcdefghijklmnopqrstuvwxyz
абвгдеёжзийклмнопрстуфхцчшщъыьэюя
```



## Free Pascal

One can use ''set constructors'' like in [[#Delphi|Delphi]].
<tt>alphabet</tt>’s type will be <tt>set of char</tt>.

```pascal
program lowerCaseAscii(input, output, stdErr);
const
	alphabet = ['a'..'z'];
begin
end.
```

Note, Pascal does not define that the letters A through Z are contiguous, the set constructor above assumes that, though.
However, the FPC – the FreePascal compiler – virtually only works on systems, that use at least ASCII as common denominator.


## FALSE


```FALSE
'a[$'z>~][$,1+]#%
```


```txt
abcdefghijklmnopqrstuvwxyz
```



## Forth

Generate a string filled with the lowercase ASCII alphabet

```Forth
: printit    26 0 do   [char] a I + emit   loop ;
```

Or coded another way

```Forth
: printit2  [char] z 1+  [char] a  do  I emit  loop ;
```


We could do something more complicated and allocate space for a string and fill it.
Two methods are demonstrated below

<lang>create lalpha    27 chars allot    \ create a string in memory for 26 letters and count byte

: ]lalpha ( index -- addr )              \ index the string like an array (return an address)
          lalpha char+ + ;

\ method 1: fill memory with ascii values using a loop
: fillit ( -- )
         26 0
         do
           [char] a I +            \ calc. the ASCII value, leave on the stack
           I ]lalpha c!            \ store the value on stack in the string at index I
         loop
         26 lalpha c! ;            \ store the count byte at the head of the string


\ method 2: load with a string literal
: Loadit    s" abcdefghijklmnopqrstuvwxyz" lalpha PLACE ;


```


{{Output}}Test at the console
<lang>printit  abcdefghijklmnopqrstuvwxyz ok

fillit ok
lalpha count type abcdefghijklmnopqrstuvwxyz ok
lalpha count erase ok
lalpha count type ok
loadit ok
lalpha count type abcdefghijklmnopqrstuvwxyz ok

```



## Fortran

```fortran
  character(26) :: alpha
  integer :: i

  do i = 1, 26
    alpha(i:i) = achar(iachar('a') + i - 1)
  end do
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=debd3987d4db75099032a86927978046 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount As Short

For siCount = Asc("a") To Asc("z")
  Print Chr(siCount);
Next

End
```

Output:

```txt

abcdefghijklmnopqrstuvwxyz

```



## Go


```go
func loweralpha() string {
	p := make([]byte, 26)
	for i := range p {
		p[i] = 'a' + byte(i)
	}
	return string(p)
}
```



## Groovy


```groovy
def lower = ('a'..'z')
```

Test

```groovy
assert 'abcdefghijklmnopqrstuvwxyz' == lower.join('')
```



## Haskell


```haskell
lower = ['a' .. 'z']

main = print lower
```


Or, equivalently:

```haskell
alpha :: String
alpha = enumFromTo 'a' 'z'

main :: IO ()
main = print alpha
```


```txt
"abcdefghijklmnopqrstuvwxyz"
```



## Huginn


```huginn
import Algorithms as algo;
import Text as text;

main() {
  print(
    "{}\n".format(
      text.character_class( text.CHARACTER_CLASS.LOWER_CASE_LETTER )
    )
  );
  print(
    "{}\n".format(
      algo.materialize(
        algo.map(
          algo.range( integer( 'a' ), integer( 'z' ) + 1 ),
          character
        ),
        string
      )
    )
  );
}
```

```txt
abcdefghijklmnopqrstuvwxyz
abcdefghijklmnopqrstuvwxyz
```


=={{header|Icon}} and {{header|Unicon}}==
You can just use the keyword:

```txt
&lcase
```

(although this technically produces a character set instead of a string, it can be used
as a string, so string subscripting, generation, etc., all work).

E.g.

```unicon
every a := put([], !&lcase) # array of 1 character per element
c := create !&lcase         # lazy generation of letters in sequence
```



```icon

procedure lower_case_letters()               # entry point for function lower_case_letters
	return &lcase                        # returning lower caser letters represented by the set &lcase
end

procedure main(param)                        # main procedure as entry point
	write(lower_case_letters())          # output of result of function lower_case_letters()
end

```



## J

'''Solution''':
```j
   thru=: <. + i.@(+*)@-~
   thru&.(a.&i.)/'az'
abcdefghijklmnopqrstuvwxyz
```

or
```J
   u:97+i.26
abcdefghijklmnopqrstuvwxyz
```

and, obviously, other variations are possible.


## Java


```java
public class LowerAscii {

    public static void main(String[] args) {
        StringBuilder sb = new StringBuilder(26);
        for (char ch = 'a'; ch <= 'z'; ch++)
            sb.append(ch);
        System.out.printf("lower ascii: %s, length: %s", sb, sb.length());
    }
}
```


Output:


```txt
lower ascii: abcdefghijklmnopqrstuvwxyz, length: 26
```


## JavaScript



### ES5


In ES5, we can use '''String.fromCharCode()''', which suffices for Unicode characters which can be represented with one 16 bit number.

For Unicode characters beyond this range, in ES5 we have to enter a pair of Unicode number escapes.


```JavaScript
(function (cFrom, cTo) {

  function cRange(cFrom, cTo) {
    var iStart = cFrom.charCodeAt(0);

    return Array.apply(
      null, Array(cTo.charCodeAt(0) - iStart + 1)
    ).map(function (_, i) {

      return String.fromCharCode(iStart + i);

    });
  }

  return cRange(cFrom, cTo);

})('a', 'z');
```


Returns:

```JavaScript
["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
```



### ES6


In ES6, the new '''String.fromCodePoint()''' method can can return 4-byte characters (such as Emoji, for example) as well as the usual 2-byte characters.


```JavaScript
(function (lstRanges) {

  function cRange(cFrom, cTo) {
    var iStart = cFrom.codePointAt(0);

    return Array.apply(
      null, Array(cTo.codePointAt(0) - iStart + 1)
    ).map(function (_, i) {

      return String.fromCodePoint(iStart + i);

    });
  }

  return lstRanges.map(function (lst) {
    return cRange(lst[0], lst[1]);
  });

})([
  ['a', 'z'],
  ['🐐', '🐟']
]);
```


Output:


```JavaScript
[["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"],
 ["🐐", "🐑", "🐒", "🐓", "🐔", "🐕", "🐖", "🐗", "🐘", "🐙", "🐚", "🐛", "🐜", "🐝", "🐞", "🐟"]]
```


```JavaScript
var letters = []
for (var i = 97; i <= 122; i++) {
    letters.push(String.fromCodePoint(i))
}
```


Or, if we want to write a more general ES6 function:


```JavaScript
(() => {
    // enumFromTo :: Enum a => a -> a -> [a]
    const enumFromTo = (m, n) => {
        const [intM, intN] = [m, n].map(fromEnum),
            f = typeof m === 'string' ? (
                (_, i) => chr(intM + i)
            ) : (_, i) => intM + i;
        return Array.from({
            length: Math.floor(intN - intM) + 1
        }, f);
    };


    // GENERIC FUNCTIONS ------------------------------------------------------

    // compose :: (b -> c) -> (a -> b) -> (a -> c)
    const compose = (f, g) => x => f(g(x));

    // chr :: Int -> Char
    const chr = x => String.fromCodePoint(x);

    // ord :: Char -> Int
    const ord = c => c.codePointAt(0);

    // fromEnum :: Enum a => a -> Int
    const fromEnum = x => {
        const type = typeof x;
        return type === 'boolean' ? (
            x ? 1 : 0
        ) : type === 'string' ? ord(x) : x;
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // show :: a -> String
    const show = x => JSON.stringify(x);

    // uncurry :: Function -> Function
    const uncurry = f => args => f.apply(null, args);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // TEST -------------------------------------------------------------------
    return unlines(map(compose(unwords, uncurry(enumFromTo)), [
        ['a', 'z'],
        ['α', 'ω'],
        ['א', 'ת'],
        ['🐐', '🐟']
    ]));
})();
```

```txt
a b c d e f g h i j k l m n o p q r s t u v w x y z
α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ ς σ τ υ φ χ ψ ω
א ב ג ד ה ו ז ח ט י ך כ ל ם מ ן נ ס ע ף פ ץ צ ק ר ש ת
🐐 🐑 🐒 🐓 🐔 🐕 🐖 🐗 🐘 🐙 🐚 🐛 🐜 🐝 🐞 🐟
```



## jq


```jq
"az" | explode | [range( .[0]; 1+.[1] )] | implode'
```

produces:

<tt>"abcdefghijklmnopqrstuvwxyz"</tt>


## Jsish


```javascript
/* Generate the lower case alphabet with Jsish, assume ASCII */
var letterA = "a".charCodeAt(0);
var lowers = Array(26);
for (var i = letterA; i < letterA + 26; i++) {
    lowers[i - letterA] = Util.fromCharCode(i);
}
puts(lowers);
puts(lowers.join(''));
puts(lowers.length);

/*
=!EXPECTSTART!=
[ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" ]
abcdefghijklmnopqrstuvwxyz
26
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish generate-lowers.jsi
[ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" ]
abcdefghijklmnopqrstuvwxyz
26
prompt$ jsish -u generate-lowers.jsi
[PASS] generate-lowers.jsi
```



## Julia

```julia
@show collect('a':'z')
@show join('a':'z')
```


```txt
collect('a':'z') = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
join('a':'z') = "abcdefghijklmnopqrstuvwxyz"
```



## K

<tt>`c$</tt> casts a list of integers to a string of characters; <tt>!26</tt> produces a list of the integers from 0 to 25. So the lower-case ASCII alphabet can be generated using:

```k
`c$97+!26
```

```txt
"abcdefghijklmnopqrstuvwxyz"
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val alphabet = CharArray(26) { (it + 97).toChar() }.joinToString("")
    println(alphabet)
}
```


```txt

abcdefghijklmnopqrstuvwxyz

```



## LC3 Assembly


```lc3asm
        .ORIG      0x3000

        LD         R0,ASCIIa
        LD         R1,ASCIIz
        NOT        R1,R1

LOOP    OUT
        ADD        R0,R0,1
        ADD        R2,R0,R1
        BRN        LOOP

        HALT

ASCIIa  .FILL      0x61
ASCIIz  .FILL      0x7A
```

Output:

```txt
abcdefghijklmnopqrstuvwxyz
```



## Lingo


```lingo
alphabet = []
repeat with i = 97 to 122
  alphabet.add(numtochar(i))
end repeat
put alphabet
-- ["a", "b", "c", ... , "x", "y", "z"]
```



## Logo

Straightforward, assuming ASCII:

```logo
show map "char iseq 97 122
```

Slightly less straightforward, but without the magic numbers:

```logo
show map "char apply "iseq map "ascii [a z]
```

Same output either way:
```txt
[a b c d e f g h i j k l m n o p q r s t u v w x y z]
```



## Lua


```Lua
function getAlphabet ()
    local letters = {}
    for ascii = 97, 122 do table.insert(letters, string.char(ascii)) end
    return letters
end

local alpha = getAlphabet()
print(alpha[25] .. alpha[1] .. alpha[25])
```

```txt
yay
```



## M2000 Interpreter


```M2000 Interpreter

\\ old style Basic, including a Binary.Or() function
Module OldStyle {
      10 LET A$=""
      20 FOR I=ASC("A") TO ASC("Z")
      30 LET A$=A$+CHR$(BINARY.OR(I, 32))
      40 NEXT I
      50 PRINT A$
}
CALL OldStyle

```



## Maple


```Maple
seq(StringTools:-Char(c), c = 97 .. 122);
```

```txt
"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
```




## Maxima


```Maxima

delete([], makelist(if(alphacharp(ascii(i))) then parse_string(ascii(i)) else [], i, 96, 122));
```

```txt
 [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
start = 97;
lowerCaseLetters = Table[FromCharacterCode[start + i], {i, 0, 25}]
```

```txt
{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"}
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
  'a':'z'
```

or alternatively

```MATLAB
  char(96+[1:26])
```

```txt
  abcdefghijklmnopqrstuvwxyz
```



## Mercury


```mercury
:- module gen_lowercase_ascii.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char, int, list.

main(!IO) :-
    list.map(char.det_from_int, 0'a .. 0'z, Alphabet),
    io.print_line(Alphabet, !IO).

:- end_module gen_lowercase_ascii.
```

```txt

['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']

```



## MiniScript


```MiniScript
letters = []
for i in range(code("a"), code("z"))
    letters.push char(i)
end for

print letters
```


```txt
["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
```



## MUMPS

```MUMPS

LOWASCMIN
    set lowstr = ""
    for i = 97:1:122 set delim = $select(i=97:"",1:",") set lowstr = lowstr_delim_$char(i)
    write lowstr
    quit

```


```txt

SAMPLES>DO ^LOWASCMIN
a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z

```



## Neko


```ActionScript
/**
 <doc>Generate lower case ASCII, in Neko</doc>
**/

var slot = 25
var generated = $smake(slot + 1)
var lower_a = $sget("a", 0)

/* 'a'+25 down to 'a'+0 */
while slot >= 0 {
    $sset(generated, slot, slot + lower_a)
    slot -= 1
}

$print(generated, "\n")
```


```txt
prompt$ nekoc generate-lower.neko
prompt$ neko generate-lower.n
abcdefghijklmnopqrstuvwxyz
```



## NESL


```nesl
lower_case_ascii = {code_char(c) : c in [97:123]};
```



## Nim


```nim
# A slice just contains the first and last value
let alpha: Slice[char] = 'a'..'z'
echo alpha # (a: a, b: z)

# but can be used to check if a character is in it:
echo 'f' in alpha # true
echo 'G' in alpha # false

# A set contains all elements as a bitvector:
let alphaSet: set[char] = {'a'..'z'}
echo alphaSet # {a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z}
echo 'f' in alphaSet # true
var someChars = {'a','f','g'}
echo someChars <= alphaSet # true

import sequtils
# A sequence:
let alphaSeq = toSeq 'a'..'z'
echo alphaSeq # @[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]
echo alphaSeq[10] # k
```



## OCaml


```ocaml
# Array.make 26 'a' |> Array.mapi (fun i c -> int_of_char c + i |> char_of_int);;
- : char array =
[|'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o';
  'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'|]
```



## Oforth

Oforth characters are integers. This list is a list of 26 integers

```Oforth
'a' 'z' seqFrom
```


If necessary, these integers can be added to a string to have a indexed string of chars

```Oforth
StringBuffer new 'a' 'z' seqFrom apply(#<<c)
```



## PARI/GP



```parigp
Strchr(Vecsmall([97..122]))
```


Output:
```txt
"abcdefghijklmnopqrstuvwxyz"
```



## Pascal


```pascal
program lowerCaseAscii(input, output, stdErr);
var
	alphabet: set of char;
begin
	// as per ISO 7185, 'a'..'z' do not necessarily have to be contiguous
	alphabet := [
			'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
			'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
		];
end.
```



## Perl


```Perl
print 'a'..'z'
```



## Perl 6


```perl6
say my @letters = 'a'..'z';
```


* <code>'a'..'z'</code> is a range literal, it constructs an immutable <code>Range</code> object.
* Assigning to an <code>@</code> variable flattens it into an <code>Array</code>.


## Phix


```Phix
string az = ""
    for ch='a' to 'z' do
        az &= ch
    end for
?az
```

Alternative version

```Phix
puts(1,sq_add(tagset(26),'a'-1)&"\n")
```

```txt

"abcdefghijklmnopqrstuvwxyz"
abcdefghijklmnopqrstuvwxyz

```



## PHP


```php
<?php
$lower = range('a', 'z');
var_dump($lower);
?>
```



## PicoLisp

<lang>(mapcar char (range (char "a") (char "z")))
```



## PL/I


```PL/I
gen: procedure options (main);  /* 7 April 2014. */
   declare 1 ascii union,
             2 letters (26) character (1),
             2 iletters(26) unsigned fixed binary (8),
           letter character(1);
   declare i fixed binary;

   letters(1), letter = lowercase('A');

   do i = 2 to 26;
      iletters(i) = iletters(i-1) + 1;
   end;
   put edit (letters) (a);

end gen;
```

Output:

```txt

abcdefghijklmnopqrstuvwxyz

```

Alternative, using library:
<lang>   /* Accessing library lower-case ASCII (PC only). */

   letter = lowercase('A');
   i = index(collate(), letter);
   put skip list (substr(collate, i, 26));
```

Output:

```txt

abcdefghijklmnopqrstuvwxyz

```




## PowerShell


```PowerShell

$asString = 97..122 | ForEach-Object -Begin {$asArray = @()} -Process {$asArray += [char]$_} -End {$asArray -join('')}
$asString

```

```txt

abcdefghijklmnopqrstuvwxyz

```


```PowerShell

$asArray

```

```txt

a
b
c
d
e
f
g
h
i
j
k
l
m
n
o
p
q
r
s
t
u
v
w
x
y
z

```


'''Alternative:'''

```PowerShell

-join [Char[]] (97..122)

```

```txt

abcdefghijklmnopqrstuvwxyz

```


'''Alternative as of PowerShell-v6.0.0rc:'''

```PowerShell

-join ('a'..'z')

```

```txt

abcdefghijklmnopqrstuvwxyz

```



## Prolog

Works with SWI-Prolog 6.5.3

```Prolog
a_to_z(From, To, L) :-
	maplist(atom_codes, [From, To], [[C_From], [C_To]]),
	bagof([C], between(C_From, C_To, C), L1),
	maplist(atom_codes,L, L1).

```

Output :

```txt
 ?- a_to_z(a, z, L).
L = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z].

```



## Python


```Python
# From the standard library:
from string import ascii_lowercase

# Generation:
lower = [chr(i) for i in range(ord('a'), ord('z') + 1)]
```


Or, as a particular instance of a more general enumeration pattern:
```python
'''Enumeration a-z'''

from inspect import signature
import enum


# TEST ----------------------------------------------------
def main():
    '''Testing particular instances of a general pattern:
    '''
    print(
        fTable(__doc__ + ':\n')(repr)(showList)(
            uncurry(enumFromTo)
        )([
            ('a', 'z'),
            ('α', 'ω'),
            ('א', 'ת'),
            (1, 10),
            (round((5**(1 / 2) - 1) / 2, 5), 5),
            ('🌱', '🍂')
        ])
    )


# GENERIC -------------------------------------------------

# enumFromTo :: Enum a => a -> a -> [a]
def enumFromTo(m):
    '''Enumeration of values [m..n]'''
    def go(x, y):
        t = type(m)
        i = fromEnum(x)
        d = 0 if t != float else (x - i)
        return list(map(
            lambda x: toEnum(t)(d + x),
            range(i, 1 + fromEnum(y))
        ) if int != t else range(x, 1 + y))
    return lambda n: go(m, n)


# fromEnum :: Enum a => a -> Int
def fromEnum(x):
    '''Index integer for enumerable value.'''
    Enum = enum.Enum
    return ord(x) if isinstance(x, str) else (
        x.value if isinstance(x, Enum) else int(x)
    )


# toEnum :: Type -> Int -> a
def toEnum(t):
    '''Enumerable value from index integer'''
    dct = {
        int: int,
        float: float,
        str: chr,
        bool: bool
    }
    return lambda x: dct[t](x) if t in dct else t(x)


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    '''A function over a tuple, derived from
       a vanilla or curried function.
    '''
    if 1 < len(signature(f).parameters):
        return lambda xy: f(*xy)
    else:
        return lambda xy: f(xy[0])(xy[1])


# FORMATTING -------------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# showList :: [a] -> String
def showList(xs):
    '''Stringification of a list.'''
    return '[' + ','.join(str(x) for x in xs) + ']'


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Enumeration a-z:

  ('a', 'z') -> [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  ('α', 'ω') -> [α,β,γ,δ,ε,ζ,η,θ,ι,κ,λ,μ,ν,ξ,ο,π,ρ,ς,σ,τ,υ,φ,χ,ψ,ω]
  ('א', 'ת') -> [א,ב,ג,ד,ה,ו,ז,ח,ט,י,ך,כ,ל,ם,מ,ן,נ,ס,ע,ף,פ,ץ,צ,ק,ר,ש,ת]
     (1, 10) -> [1,2,3,4,5,6,7,8,9,10]
(0.61803, 5) -> [0.61803,1.61803,2.61803,3.61803,4.61803,5.61803]
  ('🌱', '🍂') -> [🌱,🌲,🌳,🌴,🌵,🌶,🌷,🌸,🌹,🌺,🌻,🌼,🌽,🌾,🌿,🍀,🍁,🍂]
```



## R


```R
# From constants built into R:
letters

# Or generate the same with:
sapply(97:122, intToUtf8)
```



## Racket


```racket
(define lowercase-letters (build-list 26 (lambda (x) (integer->char (+ x (char->integer #\a))))))
```



## REXX


### ASCII version

This version only works under ASCII machines   (where the values of the lowercase '''a''' through the lowercase '''z''' characters are contiguous (and consecutive).

```rexx
/* REXX ---------------------------------------------------------------
* 08.02.2014 Walter Pachl
*--------------------------------------------------------------------*/
say xrange('a','z')
```

'''Output:'''

```txt
abcdefghijklmnopqrstuvwxyz
```



### idiomatic version

This REXX version shows how to generate an indexable string of a similar sequence   as per the

lowercase ASCII alphabet   (or rather, the Latin [English] alphabet),   using a reliable style of coding

(for both   '''ASCII'''   and   '''EBCDIC'''   systems).

This version also works on non-ASCII systems   (such as EBCDIC)   and isn't dependent on the

consecutiveness nature of any particular ASCII character subsequence.

Note that on an '''EBCDIC''' system,   there are   '''41'''   characters between (lowercase)   <big><big> a </big></big>   ──►   <big><big> z </big></big>

(inclusive),   some of which don't have viewable/displayable glyphs.

```rexx
/*REXX program creates an indexable string of lowercase ASCII or EBCDIC characters: a─►z*/
$=                                               /*set lowercase letters list to  null. */
      do j=0  for 2**8;                _=d2c(j)  /*convert decimal  J  to a character.  */
      if datatype(_, 'L')  then $=$ || _         /*Is lowercase?  Then add it to $ list.*/
      end   /*j*/                                /* [↑]  add lowercase letters ──► $    */
say $                                            /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

abcdefghijklmnopqrstuvwxyz

```



## Ring


```ring

for i = ascii("a") to ascii("z")
    see char(i);
next i

```



## Ruby


```ruby
p ('a' .. 'z').to_a
p [*'a' .. 'z']
```



## Rust


```rust
fn main() {
    // An iterator over the lowercase alpha's
    let ascii_iter = (0..26)
        .map(|x| (x + b'a') as char);

    println!("{:?}", ascii_iter.collect::<Vec<char>>());
}
```

```txt

['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']

```


=={{header|S-lang}}==
Char_Type is just an integer-type so a "range array" can be easily created:
<lang S-lang>variable alpha_ch = ['a':'z'], a;
```

If you need single-char strings, convert thusly:
<lang S-lang>variable alpha_st = array_map(String_Type, &char, alpha_ch);
```

Let's take a peek:
<lang S-lang>print(alpha_st[23]);
foreach a (alpha_ch)
  () = printf("%c ", a);

```

```txt
"x"
a b c d e f g h i j k l m n o p q r s t u v w x y z
```



## Scala

```scala
object Abc extends App {
  val lowAlpha = 'a' to 'z' //That's all
  // Now several tests
  assert(lowAlpha.toSeq == Seq('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
    'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'),
    "No complete lowercase alphabet.")
  assert(lowAlpha.size == 26, "No 26 characters in alphabet")
  assert(lowAlpha.start == 'a', "Character 'a' not first char! ???")
  assert(lowAlpha.head == 'a', "Character 'a' not heading! ???")
  assert(lowAlpha.head == lowAlpha(0), "Heading char is not first char.")
  assert(lowAlpha contains 'n', "Character n not present.")
  assert(lowAlpha.indexOf('n') == 13, "Character n not on the 14th position.")
  assert(lowAlpha.last == lowAlpha(25), "Expected character (z)on the last and 26th pos.")

  println(s"Successfully completed without errors. [within ${
    scala.compat.Platform.currentTime - executionStart
  } ms]")
}
```
 Successfully completed without errors. [within 675 ms]

 Process finished with exit code 0


## Scheme

```Scheme
(map integer->char (iota 26 (char->integer #\a)))
```

```txt

(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
 #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var string: lower is "";
    var char: ch is ' ';
  begin
    for ch range 'a' to 'z' do
      lower &:= ch;
    end for;
    writeln(lower);
  end func;
```


```txt

abcdefghijklmnopqrstuvwxyz

```



## Sidef


```ruby
var arr = 'a'..'z';
say arr.join(' ');
```



## Smalltalk


```smalltalk
| asciiLower |
asciiLower := String new.
97 to: 122 do: [:asciiCode |
    asciiLower := asciiLower , asciiCode asCharacter
].
^asciiLower
```



## Snobol


```sml
  &ALPHABET ('a' LEN(25)) . OUTPUT ;* Works in ASCII but not EBCDIC.
```



## SPL


```spl>
 i, 1..26
  d = [i+96,0]
  a[i] = #.str(d)
<
'now A is an array of letters a..z

> i, 1..#.size(a,1)
  #.output(a[i],#.rs)
<
```

```txt

abcdefghijklmnopqrstuvwxyz

```



## Standard ML


```sml
val lowercase_letters = List.tabulate (26, fn x => chr (x + ord #"a"));
```



## Stata


```stata
// built-in: lowercase and uppercase letters
display c(alpha)
display c(ALPHA)

// generate a variable with the letters
clear
set obs 26
gen a=char(96+_n)

// or in Mata
mata
char(97..122)
end
```



## SuperCollider

Previously, it was claimed that the method that maps ascii number to character is polymorphic on collections. However, that doesn't seem to be the case – at least not anymore in the newer version (3.10.2). A fix was added below the original code.

```SuperCollider

(97..122).asAscii; // This example unfortunately throws an error
                   // for me when running it on version 3.10.2

// Apparently, the message 'asAscii' cannot be understood by
// an Array, so I used the message 'collect' to apply the function
// enclosed in {} to each individual element of the Array,
// passing them the message 'asAscii':

(97..122).collect({|asciiCode| asciiCode.asAscii});

// Instead of writing the ascii codes directly as numbers,
// one could also pass the chars a and z the message 'ascii' to convert
// them to ascii codes – perhaps making the code a bit clearer:

($a.ascii..$z.ascii).collect({|asciiCode| asciiCode.asAscii});

// both examples output [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z ]



```

Backwards:

```SuperCollider

"abcdefghijklmnopqrstuvwxyz".ascii
// answers [ 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122 ]

```



## Swift


```Swift
var letters = [Character]()

for i in 97...122 {
    let char = Character(UnicodeScalar(i))
    letters.append(char)
}
```



## Tcl

The most common way of doing this in Tcl would be to use a simple literal; it's only 51 characters after all:

```tcl
set alpha {a b c d e f g h i j k l m n o p q r s t u v w x y z}
```

Though it could be done like this as well:

```tcl
set alpha [apply {{} {
    scan "az" "%c%c" from to
    for {set i $from} {$i <= $to} {incr i} {
        lappend l [format "%c" $i]
    }
    return $l
}}]
```



## UNIX Shell

In bash or ksh93 with <tt>braceexpand</tt> set:

```sh
lower=({a..z})
```


In zsh with <tt>braceccl</tt> set:

```sh
lower=({a-z})
```


Either way, you can display the result like this:


```sh
echo "${lower[@]}"
```


```txt
a b c d e f g h i j k l m n o p q r s t u v w x y z
```



## Ursa

Creates a string named low containing the lower case ASCII alphabet.

```ursa
decl int i
decl string low
for (set i (ord "a")) (< i (+ (ord "z") 1)) (inc i)
        set low (+ low (chr i))
end for
out low endl console
```



## VBA

```vb

Option Explicit

Sub Main_Lower_Case_Ascii_Alphabet()
Dim Alpha() As String

    Alpha = Alphabet(97, 122)
    Debug.Print Join(Alpha, ", ")
End Sub

Function Alphabet(FirstAscii As Byte, LastAscii As Byte) As String()
Dim strarrTemp() As String, i&

    ReDim strarrTemp(0 To LastAscii - FirstAscii)
    For i = FirstAscii To LastAscii
        strarrTemp(i - FirstAscii) = Chr(i)
    Next
    Alphabet = strarrTemp
    Erase strarrTemp
End Function

```

```txt
a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
```



## VBScript


```vb
Function ASCII_Sequence(range)
	arr = Split(range,"..")
	For i = Asc(arr(0)) To Asc(arr(1))
		ASCII_Sequence = ASCII_Sequence & Chr(i) & " "
	Next
End Function

WScript.StdOut.Write ASCII_Sequence(WScript.Arguments(0))
WScript.StdOut.WriteLine
```

```txt
C:\>cscript /nologo ascii_sequence.vbs a..z
a b c d e f g h i j k l m n o p q r s t u v w x y z

C:\>cscript /nologo ascii_sequence.vbs A..F
A B C D E F
```



## Vim Script


```vim
let lower = []
for c in range(0, 25)
   let lower += [nr2char(c + char2nr("a"))]
endfor
```


or:

```vim
echo map(range(char2nr('a'), char2nr('z')), 'nr2char(v:val)')
```



## Visual Basic

The [[#VBA]] example works in VB6 as well, without any change.


## Visual Basic .NET

Used '''Asc(Char)''' [returns Integer value of Char passed] and '''Chr(Integer)''' [returns Char value of Integer passed] functions. <br/>
String.Join() is used to print the list, converted to array, without looping through it.


```vbnet
Module LowerASCII

    Sub Main()
        Dim alphabets As New List(Of Char)
        For i As Integer = Asc("a") To Asc("z")
            alphabets.Add(Chr(i))
        Next
        Console.WriteLine(String.Join("", alphabets.ToArray))
    End Sub

End Module

```


```txt

abcdefghijklmnopqrstuvwxyz

```



## xEec


```xEec
h$` h$` >0_0 t h$y ms p h? jn00_0 p r h#1 ma t jn0_0 >00_0 p p r p
```



## XLISP


```lisp
(defun ascii-lower ()
    (defun add-chars (x y s)
        (if (<= x y)
            (add-chars (+ x 1) y (string-append s (string (integer->char x))))
            s))
    (add-chars 97 122 ""))
```



## zkl


```zkl
["a".."z"]  // lasy list
["a".."z"].walk() //-->L("a","b","c","d","e",...
"a".toAsc().pump(26,List,"toChar")  // another way to create the list
"a".toAsc().pump(26,String,"toChar")  // create a string
   //-->"abcdefghijklmnopqrstuvwxyz"
Utils.Helpers.lowerLetters  // string const
```

