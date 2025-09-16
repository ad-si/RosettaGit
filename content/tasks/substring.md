+++
title = "Substring"
description = ""
date = 2019-09-07T23:03:48Z
aliases = []
[extra]
id = 4645
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "ada",
  "aikido",
  "aime",
  "algol_68",
  "apex",
  "applescript",
  "arm_assembly",
  "autohotkey",
  "awk",
  "axe",
  "basic",
  "bbc_basic",
  "bracmat",
  "burlesque",
  "c",
  "clojure",
  "cobol",
  "coldfusion",
  "commodore_basic",
  "common_lisp",
  "cpp",
  "d",
  "delphi",
  "e",
  "easylang",
  "ecl",
  "eero",
  "elena",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "falcon",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "gambas",
  "gap",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "labview",
  "lang5",
  "lasso",
  "lfe",
  "liberty_basic",
  "lingo",
  "livecode",
  "logo",
  "logtalk",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "maxima",
  "mumps",
  "nemerle",
  "netrexx",
  "newlisp",
  "nim",
  "niue",
  "objeck",
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
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "raven",
  "rebol",
  "rexx",
  "ring",
  "rpg",
  "ruby",
  "run_basic",
  "rust",
  "sas",
  "sather",
  "scala",
  "scheme",
  "sed",
  "seed7",
  "sidef",
  "slate",
  "smalltalk",
  "snobol4",
  "sql_pl",
  "stata",
  "swift",
  "tcl",
  "tuscript",
  "unix_shell",
  "vala",
  "vba",
  "vbscript",
  "wart",
  "yorick",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

[[Category:String manipulation]] [[Category:Simple]]
In this task display a substring:

* starting from <tt>n</tt> characters in and of <tt>m</tt> length;
* starting from <tt>n</tt> characters in, up to the end of the string;
* whole string minus last character;
* starting from a known character within the string and of <tt>m</tt> length;
* starting from a known substring within the string and of <tt>m</tt> length.



If the program uses UTF-8 or UTF-16, it must work on any valid Unicode code point,
whether in the Basic Multilingual Plane or above it.

The program must reference logical characters (code points), not 8-bit code units for UTF-8 or 16-bit code units for UTF-16.

Programs for other encodings (such as 8-bit ASCII, or EUC-JP) are not required to handle all Unicode characters.





## Ada

String in [[Ada]] is an array of Character elements indexed by Positive:

```Ada
type String is array (Positive range <>) of Character;
```

Substring is a first-class object in [[Ada]], an anonymous subtype of String. The language uses the term '''slice''' for it. Slices can be retrieved, assigned and passed as a parameter to subprograms in mutable or immutable mode. A slice is specified as:

```Ada
A (<first-index>..<last-index>)
```


A string array in [[Ada]] can start with any positive index. This is why the implementation below uses Str'First in all slices, which in this concrete case is 1, but intentionally left in the code because the task refers to N as an ''offset'' to the string beginning rather than an ''index'' in the string. In [[Ada]] it is unusual to deal with slices in such way. One uses plain string index instead.

```Ada
with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;

procedure Test_Slices is
   Str : constant String := "abcdefgh";
   N : constant := 2;
   M : constant := 3;
begin
   Put_Line (Str (Str'First + N - 1..Str'First + N + M - 2));
   Put_Line (Str (Str'First + N - 1..Str'Last));
   Put_Line (Str (Str'First..Str'Last - 1));
   Put_Line (Head (Tail (Str, Str'Last - Index (Str, "d", 1)), M));
   Put_Line (Head (Tail (Str, Str'Last - Index (Str, "de", 1) - 1), M));
end Test_Slices;
```

```txt

bcd
bcdefgh
abcdefg
efg
fgh

```



## Aikido

Aikido uses square brackets for slices.  The syntax is <code>[start:end]</code>.
If you want to use length you have to add to the start.
Shifting strings left or right removes characters from the ends.


```aikido

const str = "abcdefg"
var n = 2
var m = 3

println (str[n:n+m-1])    // pos 2 length 3
println (str[n:])           // pos 2 to end
println (str >> 1)      // remove last character
var p = find (str, 'c')
println (str[p:p+m-1])    // from pos of p length 3

var s = find (str, "bc")
println (str[s, s+m-1])    // pos of bc length 3

```



## Aime


```aime
text s;
data b, d;

s = "The quick brown fox jumps over the lazy dog.";

o_text(cut(s, 4, 15));
o_newline();
o_text(cut(s, 4, length(s)));
o_newline();
o_text(delete(s, -1));
o_newline();
o_text(cut(s, index(s, 'q'), 5));
o_newline();

b_cast(b, s);
b_cast(d, "brown");
o_text(cut(s, b_find(b, d), 15));
o_newline();
```

```txt
quick brown fox
quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog
quick
brown fox jumps
```



## ALGOL 68

<!-- {{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8.8d.fc9.i386]}} -->

```Algol68
main: (
  STRING s = "abcdefgh";
  INT n = 2, m = 3;
  CHAR char = "d";
  STRING chars = "cd";

  printf(($gl$, s[n:n+m-1]));
  printf(($gl$, s[n:]));
  printf(($gl$, s[:UPB s-1]));

  INT pos;
  char in string("d", pos, s);
  printf(($gl$, s[pos:pos+m-1]));
  string in string("de", pos, s);
  printf(($gl$, s[pos:pos+m-1]))
)
```

```txt

bcd
bcdefgh
abcdefg
def
def

```




## Apex

In Apex, the substring method returns a new String that begins with the character at the specified zero-based startIndex and extends to the end of the String.

```apex
String x = 'testing123';
//Test1: testing123
System.debug('Test1: ' + x.substring(0,x.length()));
//Test2: esting123
System.debug('Test2: ' + x.substring(1,x.length()));
//Test3: testing123
System.debug('Test3: ' + x.substring(0));
//Test4: 3
System.debug('Test4: ' + x.substring(x.length()-1));
//Test5:
System.debug('Test5: ' + x.substring(1,1));
//Test 6: testing123
System.debug('Test6: ' + x.substring(x.indexOf('testing')));
//Test7: e
System.debug('Test7: ' + x.substring(1,2));

```



## AppleScript


Expressed in terms of some familiar functional primitives, so that we can focus more on the task, without too much distraction by the parochial quirks of a particular scripting language.

(Functional primitives version)
```AppleScript
-- SUBSTRINGS -----------------------------------------------------------------

--  take :: Int -> Text -> Text
on take(n, s)
    text 1 thru n of s
end take

--  drop :: Int -> Text -> Text
on drop(n, s)
    text (n + 1) thru -1 of s
end drop

-- breakOn :: Text -> Text -> (Text, Text)
on breakOn(strPattern, s)
    set {dlm, my text item delimiters} to {my text item delimiters, strPattern}
    set lstParts to text items of s
    set my text item delimiters to dlm
    {item 1 of lstParts, strPattern & (item 2 of lstParts)}
end breakOn

--  init :: Text -> Text
on init(s)
    if length of s > 0 then
        text 1 thru -2 of s
    else
        missing value
    end if
end init


-- TEST -----------------------------------------------------------------------
on run
    set str to "一二三四五六七八九十"

    set legends to {¬
        "from n in, of n length", ¬
        "from n in, up to end", ¬
        "all but last", ¬
        "from matching char, of m length", ¬
        "from matching string, of m length"}

    set parts to {¬
        take(3, drop(4, str)), ¬
        drop(3, str), ¬
        init(str), ¬
        take(3, item 2 of breakOn("五", str)), ¬
        take(4, item 2 of breakOn("六七", str))}

    script tabulate
        property strPad : "                                        "

        on |λ|(l, r)
            l & drop(length of l, strPad) & r
        end |λ|
    end script

    linefeed & intercalate(linefeed, ¬
        zipWith(tabulate, ¬
            legends, parts)) & linefeed
end run

-- GENERIC FUNCTIONS FOR TEST -------------------------------------------------

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

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

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith
```

```txt
from n in, of n length                  五六七
from n in, up to end                    四五六七八九十
all but last                            一二三四五六七八九
from matching char, of m length         五六七
from matching string, of m length       六七八九
```



## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program substring.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall

.equ BUFFERSIZE,          100

/* Initialized data */
.data
szMessString:            .asciz "Result : "
szString1:               .asciz "abcdefghijklmnopqrstuvwxyz"
szStringStart:           .asciz "abcdefg"
szCarriageReturn:        .asciz "\n"

/* UnInitialized data */
.bss
szSubString:             .skip 500             @ buffer result


/*  code section */
.text
.global main
main:

    ldr r0,iAdrszString1                        @ address input string
    ldr r1,iAdrszSubString                      @ address output string
    mov r2,#22                                  @ location
    mov r3,#4                                   @ length
    bl subStringNbChar                          @ starting from n characters in and of m length
    ldr r0,iAdrszMessString                     @ display message
    bl affichageMess
    ldr r0,iAdrszSubString                      @ display substring result
    bl affichageMess
    ldr r0,iAdrszCarriageReturn                 @ display line return
    bl affichageMess
    @
    ldr r0,iAdrszString1
    ldr r1,iAdrszSubString
    mov r2,#15                                  @ location
    bl subStringEnd                             @starting from n characters in, up to the end of the string
    ldr r0,iAdrszMessString                     @ display message
    bl affichageMess
    ldr r0,iAdrszSubString
    bl affichageMess
    ldr r0,iAdrszCarriageReturn                 @ display line return
    bl affichageMess
    @
    ldr r0,iAdrszString1
    ldr r1,iAdrszSubString
    bl subStringMinus                           @ whole string minus last character
    ldr r0,iAdrszMessString                     @ display message
    bl affichageMess
    ldr r0,iAdrszSubString
    bl affichageMess
    ldr r0,iAdrszCarriageReturn                 @ display line return
    bl affichageMess
    @
    ldr r0,iAdrszString1
    ldr r1,iAdrszSubString
    mov r2,#'c'                                 @ start character
    mov r3,#5                                   @ length
    bl subStringStChar                          @starting from a known character within the string and of m length
    cmp r0,#-1                                  @ error ?
    beq 2f
    ldr r0,iAdrszMessString                     @ display message
    bl affichageMess
    ldr r0,iAdrszSubString
    bl affichageMess
    ldr r0,iAdrszCarriageReturn                 @ display line return
    bl affichageMess
    @
2:
    ldr r0,iAdrszString1
    ldr r1,iAdrszSubString
    ldr r2,iAdrszStringStart                    @ sub string to start
    mov r3,#10                                  @ length
    bl subStringStString                        @ starting from a known substring within the string and of m length
    cmp r0,#-1                                  @ error ?
    beq 3f
    ldr r0,iAdrszMessString                     @ display message
    bl affichageMess
    ldr r0,iAdrszSubString
    bl affichageMess
    ldr r0,iAdrszCarriageReturn                 @ display line return
    bl affichageMess
3:
100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc 0                                       @ perform system call
iAdrszMessString:         .int szMessString
iAdrszString1:            .int szString1
iAdrszSubString:            .int szSubString
iAdrszStringStart:            .int szStringStart
iAdrszCarriageReturn:     .int szCarriageReturn
/******************************************************************/
/*     sub strings  index start  number of characters             */
/******************************************************************/
/* r0 contains the address of the input string */
/* r1 contains the address of the output string */
/* r2 contains the start index                  */
/* r3 contains numbers of characters to extract */
/* r0 returns number of characters or -1 if error */
subStringNbChar:
    push {r1-r5,lr}                             @ save  registers
    mov r4,#0                                   @ counter byte output string
1:
    ldrb r5,[r0,r2]                             @ load byte string input
    cmp r5,#0                                   @ zero final ?
    beq 2f
    strb r5,[r1,r4]                             @ store byte output string
    add r2,#1                                   @ increment counter
    add r4,#1
    cmp r4,r3                                   @ end ?
    blt 1b                                      @ no -> loop
2:
    mov r5,#0
    strb r5,[r1,r4]                             @ load byte string 2
    mov r0,r4
100:
    pop {r1-r5,lr}                              @ restaur registers
    bx lr                                       @ return
/******************************************************************/
/*     sub strings  index start at end of string             */
/******************************************************************/
/* r0 contains the address of the input string */
/* r1 contains the address of the output string */
/* r2 contains the start index                  */
/* r0 returns number of characters or -1 if error */
subStringEnd:
    push {r1-r5,lr}                             @ save registers
    mov r4,#0                                   @ counter byte output string
1:
    ldrb r5,[r0,r2]                             @ load byte string 1
    cmp r5,#0                                   @ zero final ?
    beq 2f
    strb r5,[r1,r4]
    add r2,#1
    add r4,#1
    b 1b                                        @ loop
2:
    mov r5,#0
    strb r5,[r1,r4]                             @ load byte string 2
    mov r0,r4
100:
    pop {r1-r5,lr}                              @ restaur registers
    bx lr
/******************************************************************/
/*      whole string minus last character                        */
/******************************************************************/
/* r0 contains the address of the input string */
/* r1 contains the address of the output string */
/* r0 returns number of characters or -1 if error */
subStringMinus:
    push {r1-r5,lr}                             @ save  registers
    mov r2,#0                                   @ counter byte input string
    mov r4,#0                                   @ counter byte output string
1:
    ldrb r5,[r0,r2]                             @ load byte string
    cmp r5,#0                                   @ zero final ?
    beq 2f
    strb r5,[r1,r4]
    add r2,#1
    add r4,#1
    b 1b                                        @  loop
2:
    sub r4,#1
    mov r5,#0
    strb r5,[r1,r4]                             @ load byte string 2
    mov r0,r4
100:
    pop {r1-r5,lr}                              @ restaur registers
    bx lr
/******************************************************************/
/*   starting from a known character within the string and of m length  */
/******************************************************************/
/* r0 contains the address of the input string */
/* r1 contains the address of the output string */
/* r2 contains the character    */
/* r3 contains the length
/* r0 returns number of characters or -1 if error */
subStringStChar:
    push {r1-r5,lr}                             @ save  registers
    mov r6,#0                                   @ counter byte input string
    mov r4,#0                                   @ counter byte output string

1:
    ldrb r5,[r0,r6]                             @ load byte string
    cmp r5,#0                                   @ zero final ?
    streqb r5,[r1,r4]
    moveq r0,#-1
    beq 100f
    cmp r5,r2
    beq 2f
    add r6,#1
    b 1b                                        @  loop
2:
    strb r5,[r1,r4]
    add r6,#1
    add r4,#1
    cmp r4,r3
    bge 3f
    ldrb r5,[r0,r6]                             @ load byte string
    cmp r5,#0
    bne 2b
3:
    mov r5,#0
    strb r5,[r1,r4]                             @ load byte string 2
    mov r0,r4
100:
    pop {r1-r5,lr}                              @ restaur registers
    bx lr

/******************************************************************/
/*   starting from a known substring within the string and of m length  */
/******************************************************************/
/* r0 contains the address of the input string */
/* r1 contains the address of the output string */
/* r2 contains the address of string to start    */
/* r3 contains the length
/* r0 returns number of characters or -1 if error */
subStringStString:
    push {r1-r8,lr}                             @ save  registers
    mov r7,r0                                   @ save address
    mov r8,r1                                   @ counter byte string
    mov r1,r2
    bl searchSubString
    cmp r0,#-1
    beq 100f
    mov r6,r0                                   @ counter byte input string
    mov r4,#0
1:
    ldrb r5,[r7,r6]                             @ load byte string
    strb r5,[r8,r4]
    cmp r5,#0                                   @ zero final ?
    moveq r0,r4
    beq 100f
    add r4,#1
    cmp r4,r3
    addlt r6,#1
    blt 1b                                      @  loop
    mov r5,#0
    strb r5,[r8,r4]
    mov r0,r4
100:
    pop {r1-r8,lr}                              @ restaur registers
    bx lr

/******************************************************************/
/*   search a substring in the string                            */
/******************************************************************/
/* r0 contains the address of the input string */
/* r1 contains the address of substring */
/* r0 returns index of substring in string or -1 if not found */
searchSubString:
    push {r1-r6,lr}                       @ save registers
    mov r2,#0                             @ counter byte input string
    mov r3,#0                             @ counter byte string
    mov r6,#-1                            @ index found
    ldrb r4,[r1,r3]
1:
    ldrb r5,[r0,r2]                       @ load byte string
    cmp r5,#0                             @ zero final ?
    moveq r0,#-1                          @ yes returns error
    beq 100f
    cmp r5,r4                             @ compare character
    beq 2f
    mov r6,#-1                            @ no equals - > raz index
    mov r3,#0                             @ and raz counter byte
    add r2,#1                             @ and increment counter byte
    b 1b                                  @ and loop
2:                                        @ characters equals
    cmp r6,#-1                            @ first characters equals ?
    moveq r6,r2                           @ yes -> index begin in r6
    add r3,#1                             @ increment counter substring
    ldrb r4,[r1,r3]                       @ and load next byte
    cmp r4,#0                             @ zero final ?
    beq 3f                                @ yes -> end search
    add r2,#1                             @ else increment counter string
    b 1b                                  @ and loop
3:
    mov r0,r6
100:
    pop {r1-r6,lr}                        @ restaur registers
    bx lr

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

The code contains some alternatives.

```autohotkey
String := "abcdefghijklmnopqrstuvwxyz"
; also: String = abcdefghijklmnopqrstuvwxyz
n := 12
m := 5

; starting from n characters in and of m length;
subString := SubStr(String, n, m)
; alternative:  StringMid, subString, String, n, m
MsgBox % subString

; starting from n characters in, up to the end of the string;
subString := SubStr(String, n)
; alternative:  StringMid, subString, String, n
MsgBox % subString

; whole string minus last character;
StringTrimRight, subString, String, 1
; alternatives: subString := SubStr(String, 1, StrLen(String) - 1)
;               StringMid, subString, String, 1, StrLen(String) - 1
MsgBox % subString

; starting from a known character within the string and of m length;
findChar := "q"
subString := SubStr(String, InStr(String, findChar), m)
; alternatives: RegExMatch(String, findChar . ".{" . m - 1 . "}", subString)
;               StringMid, subString, String, InStr(String, findChar), m
MsgBox % subString

; starting from a known character within the string and of m length;
findString := "pq"
subString := SubStr(String, InStr(String, findString), m)
; alternatives: RegExMatch(String, findString . ".{" . m - StrLen(findString) . "}", subString)
;               StringMid, subString, String, InStr(String, findString), m
MsgBox % subString

```


```txt

 lmnop
 lmnopqrstuvwxyz
 abcdefghijklmnopqrstuvwxy
 qrstu
 pqrst

```



## AWK

```awk
BEGIN {
	str = "abcdefghijklmnopqrstuvwxyz"
	n = 12
	m = 5

	print substr(str, n, m)
	print substr(str, n)
	print substr(str, 1, length(str) - 1)
	print substr(str, index(str, "q"), m)
	print substr(str, index(str, "pq"), m)
}
```


```txt
$ awk -f substring.awk
lmnop
lmnopqrstuvwxyz
abcdefghijklmnopqrstuvwxy
qrstu
pqrst
```



## Axe

```axe
Lbl SUB1
0→{r₁+r₂+r₃}
r₁+r₂
Return

Lbl SUB2
r₁+r₂
Return

Lbl SUB3
0→{r₁+length(r₁)-1}
r₁
Return

Lbl SUB4
inData(r₂,r₁)-1→I
0→{r₁+I+r₃}
r₁+I
Return
```



## BASIC


```qbasic
DIM baseString AS STRING, subString AS STRING, findString AS STRING
DIM m AS INTEGER, n AS INTEGER

baseString = "abcdefghijklmnopqrstuvwxyz"
n = 12
m = 5

' starting from n characters in and of m length;
subString = MID$(baseString, n, m)
PRINT subString

' starting from n characters in, up to the end of the string;
subString = MID$(baseString, n)
PRINT subString

' whole string minus last character;
subString = LEFT$(baseString, LEN(baseString) - 1)
PRINT subString

' starting from a known character within the string and of m length;
' starting from a known substring within the string and of m length.
findString = "pq"
subString = MID$(baseString, INSTR(baseString, findString), m)
PRINT subString

```


```txt

 lmnop
 lmnopqrstuvwxyz
 abcdefghijklmnopqrstuvwxy
 pqrst

```


=
## Commodore BASIC
=

```basic
10 REM SUBSTRING ... ROSETTACODE.ORG
20 A$ = "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
30 X$ = "J" : S$ = "FOX"
40 N = 5: M = 11
50 PRINT "THE STRING:"
60 PRINT A$
70 PRINT
80 PRINT "SUBSTRING STARTING FROM" N "CHARACTERS IN AND OF" M "LENGTH:"
90 PRINT MID$(A$,N,M)
100 PRINT
110 PRINT "STARTING FROM" N "CHARACTERS IN, UP TO THE END OF THE STRING:"
120 PRINT RIGHT$(A$,LEN(A$)+1-N)
130 PRINT
140 PRINT "WHOLE STRING MINUS LAST CHARACTER:"
150 PRINT LEFT$(A$,LEN(A$)-1)
160 PRINT
170 PRINT "STARTING FROM '";X$;"' AND OF" M "LENGTH:"
180 I = 1
190 IF MID$(A$,I,1)=X$ THEN 220
200 I = I+1
210 GOTO 190
220 PRINT RIGHT$(A$,LEN(A$)+1-I)
230 PRINT
240 PRINT "STARTING FROM '";S$;"' AND OF" M "LENGTH:"
250 I = 1
260 IF MID$(A$,I,LEN(S$))=S$ THEN 290
270 I = I+1
280 GOTO 260
290 PRINT RIGHT$(A$,LEN(A$)+1-I)
300 END
```


```txt

THE STRING:
THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG

SUBSTRING STARTING FROM 5 CHARACTERS IN AND OF 11 LENGTH:
QUICK BROWN

STARTING FROM 5 CHARACTERS IN, UP TO THE END OF THE STRING:
QUICK BROWN FOX JUMPS OVER THE LAZY DOG

WHOLE STRING MINUS LAST CHARACTER:
THE QUICK BROWN FOX JUMPS OVER THE LAZY DO

STARTING FROM 'J' AND OF 11 LENGTH:
JUMPS OVER THE LAZY DOG

STARTING FROM 'FOX' AND OF 11 LENGTH:
FOX JUMPS OVER THE LAZY DOG

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET A$="abcdefghijklmnopqrstuvwxyz"
110 LET N=10:LET M=7
120 PRINT A$(N:N+M-1)
130 PRINT A$(N:)
140 PRINT A$(:LEN(A$)-1)
150 LET I=POS(A$,"g")
160 PRINT A$(I:I+M-1)
170 LET I=POS(A$,"ijk")
180 PRINT A$(I:I+M-1)
```


=
## ZX Spectrum Basic
=
ZX Spectrum Basic has unfortunately no direct way to find a substring within a string, however a similar effect can be done searching with a for loop:

```zxbasic
10 LET A$="abcdefghijklmnopqrstuvwxyz"
15 LET n=10: LET m=7
20 PRINT A$(n TO n+m-1)
30 PRINT A$(n TO )
40 PRINT A$( TO LEN (A$)-1)
50 FOR i=1 TO LEN (A$)
60 IF A$(i)="g" THEN PRINT A$(i TO i+m-1): LET i=LEN (A$): GO TO 70
70 NEXT i
80 LET B$="ijk"
90 FOR i=1 TO LEN (A$)-LEN (B$)+1
100 IF A$(i TO i+LEN (B$)-1)=B$ THEN PRINT A$(i TO i+m-1): LET i=LEN (A$)-LEN (B$)+1:  GO TO 110
110 NEXT i
120 STOP
```


Without superfluous code:

```zxbasic
10 LET A$="abcdefghijklmnopqrstuvwxyz": LET la=LEN A$
20 LET n=10: LET m=7
30 PRINT A$(n TO n+m-1)
40 PRINT A$(n TO )
50 PRINT A$( TO la-1)
60 FOR i=1 TO la
70 IF A$(i)="g" THEN PRINT A$(i TO i+m-1): LET i=la
80 NEXT i
90 LET B$="ijk": LET lb=LEN b$
100 FOR i=1 TO la-lb+1
110 IF A$(i TO i+lb-1)=B$ THEN PRINT A$(i TO i+m-1): LET i=la-lb+1
120 NEXT i
```

```txt
jklmnop
jklmnopqrstuvwxyz
abcdefghijklmnopqrstuvwxy
ghijklm
ijklmno
```


=
## BBC BASIC
=

```bbcbasic
      basestring$ = "The five boxing wizards jump quickly"
      n% = 10
      m% = 5

      REM starting from n characters in and of m length:
      substring$ = MID$(basestring$, n%, m%)
      PRINT substring$

      REM starting from n characters in, up to the end of the string:
      substring$ = MID$(basestring$, n%)
      PRINT substring$

      REM whole string minus last character:
      substring$ = LEFT$(basestring$)
      PRINT substring$

      REM starting from a known character within the string and of m length:
      char$ = "w"
      substring$ = MID$(basestring$, INSTR(basestring$, char$), m%)
      PRINT substring$

      REM starting from a known substring within the string and of m length:
      find$ = "iz"
      substring$ = MID$(basestring$, INSTR(basestring$, find$), m%)
      PRINT substring$
```

```txt
boxin
boxing wizards jump quickly
The five boxing wizards jump quickl
wizar
izard
```



## Bracmat

```bracmat
( (basestring = "The five boxing wizards jump quickly")
& (n = 10)
& (m = 5)

  { starting from n characters in and of m length: }
& @(!basestring:? [(!n+-1) ?substring [(!n+!m+-1) ?)
& out$!substring

  { starting from n characters in, up to the end of the string: }
& @(!basestring:? [(!n+-1) ?substring)
& out$!substring

  { whole string minus last character: }
& @(!basestring:?substring [-2 ?)
& out$!substring

  { starting from a known character within the string and of m length: }
& (char = "w")
& @(!basestring:? ([?p !char ?: ?substring [(!p+!m) ?))
& out$!substring

  { starting from a known substring within the string and of m length: }
& (find = "iz")
& @(!basestring:? ([?p !find ?: ?substring [(!p+!m) ?))
& out$!substring
&
)
```

```txt
boxin
boxing wizards jump quickly
The five boxing wizards jump quickl
wizar
izard
```



## Burlesque


```blsq

blsq ) "RosettaCode"5.+
"Roset"
blsq ) "RosettaCode"5.+2.-
"set"
blsq ) "RosettaCode""set"ss
2
blsq ) "RosettaCode"J"set"ss.-
"settaCode"
blsq ) "RosettaCode"~]
"RosettaCod"
blsq ) "RosettaCode"[-
"osettaCode"

```


Selecting/Deleting individual characters


```blsq

blsq ) "RosettaCode"{0 1 3 5}si
"Roet"
blsq ) "RosettaCode"{0 1 3 5}di
"oetaCde"

```



## C


### C: ASCII version


```C
/*
 * RosettaCode: Substring, C89
 *
 * In this task display a substring: starting from n characters in and of m
 * length; starting from n characters in, up to the end of the string; whole
 * string minus last character; starting from a known character within the
 * string and of m length; starting from a known substring within the string
 * and of m length.
 *
 * This example program DOES NOT make substrings. The program simply displays
 * certain parts of the input string.
 *
 */
#define _CRT_SECURE_NO_WARNINGS /* MSVS compilers need this */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Put no more than m characters from string to standard output.
 *
 * It is worth noting that printf("%*s",width,string) does not limit the number
 * of characters to be printed.
 *
 * @param string null terminated string
 * @param m      number of characters to display
 */
void putm(char* string, size_t m)
{
    while(*string && m--)
        putchar(*string++);
}

int main(void)
{

    char string[] =
        "Programs for other encodings (such as 8-bit ASCII, or EUC-JP)."

    int n = 3;
    int m = 4;
    char knownCharacter = '(';
    char knownSubstring[] = "encodings";

    putm(string+n-1, m );                       putchar('\n');
    puts(string+n+1);                           putchar('\n');
    putm(string, strlen(string)-1);             putchar('\n');
    putm(strchr(string, knownCharacter), m );   putchar('\n');
    putm(strstr(string, knownSubstring), m );   putchar('\n');

    return EXIT_SUCCESS;
}
```



### C: Unicode version


```c
/*
 * RosettaCode: Substring, C89, Unicode
 *
 * In this task display a substring: starting from n characters in and of m
 * length; starting from n characters in, up to the end of the string; whole
 * string minus last character; starting from a known character within the
 * string and of m length; starting from a known substring within the string
 * and of m length.
 *
 * This example program DOES NOT make substrings. The program simply displays
 * certain parts of the input string.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Put all characters from string to standard output AND write newline.
 * BTW, _putws may not be avaliable.
 */
void put(wchar_t* string)
{
    while(*string)
        putwchar(*string++);
    putwchar(L'\n');
}

/*
 * Put no more than m characters from string to standard output AND newline.
 */
void putm(wchar_t* string, size_t m)
{
    while(*string && m--)
        putwchar(*string++);
    putwchar(L'\n');
}

int main(void)
{
    wchar_t string[] =
        L"Programs for other encodings (such as 8-bit ASCII).";

    int n = 3;
    int m = 4;
    wchar_t knownCharacter = L'(';
    wchar_t knownSubstring[] = L"encodings";

    putm(string+n-1,m);
    put (string+n+1);
    putm(string, wcslen(string)-1);
    putm(wcschr(string, knownCharacter), m );
    putm(wcsstr(string, knownSubstring), m );

    return EXIT_SUCCESS;
}
```



### C: another version


```c
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *substring(const char *s, size_t n, ptrdiff_t m)
{
  char *result;
  /* check for null s */
  if (NULL == s)
    return NULL;
  /* negative m to mean 'up to the mth char from right' */
  if (m < 0)
    m = strlen(s) + m - n + 1;

  /* n < 0 or m < 0 is invalid */
  if (n < 0 || m < 0)
    return NULL;

  /* make sure string does not end before n
   * and advance the "s" pointer to beginning of substring */
  for ( ; n > 0; s++, n--)
    if (*s == '\0')
      /* string ends before n: invalid */
      return NULL;

  result = malloc(m+1);
  if (NULL == result)
    /* memory allocation failed */
    return NULL;
  result[0]=0;
  strncat(result, s, m); /* strncat() will automatically add null terminator
                          * if string ends early or after reading m characters */
  return result;
}

char *str_wholeless1(const char *s)
{
  return substring(s, 0, strlen(s) - 1);
}

char *str_fromch(const char *s, int ch, ptrdiff_t m)
{
  return substring(s, strchr(s, ch) - s, m);
}

char *str_fromstr(const char *s, char *in, ptrdiff_t m)
{
  return substring(s, strstr(s, in) - s , m);
}


#define TEST(A) do {		\
    char *r = (A);		\
    if (NULL == r)		\
      puts("--error--");	\
    else {			\
      puts(r);			\
      free(r);			\
    }				\
  } while(0)

int main()
{
  const char *s = "hello world shortest program";

  TEST( substring(s, 12, 5) );		// get "short"
  TEST( substring(s, 6, -1) );		// get "world shortest program"
  TEST( str_wholeless1(s) );		// "... progra"
  TEST( str_fromch(s, 'w', 5) );	// "world"
  TEST( str_fromstr(s, "ro", 3) );	// "rog"

  return 0;
}
```



## C++


```cpp
#include <iostream>
#include <string>

int main()
{
  std::string s = "0123456789";

  int const n = 3;
  int const m = 4;
  char const c = '2';
  std::string const sub = "456";

  std::cout << s.substr(n, m)<< "\n";
  std::cout << s.substr(n) << "\n";
  std::cout << s.substr(0, s.size()-1) << "\n";
  std::cout << s.substr(s.find(c), m) << "\n";
  std::cout << s.substr(s.find(sub), m) << "\n";
}
```


=={{header|C_sharp|C#}}==

```c#
using System;
namespace SubString
{
    class Program
    {
        static void Main(string[] args)
        {
            string s = "0123456789";
            const int n = 3;
            const int m = 2;
            const char c = '3';
            const string z = "345";

            Console.WriteLine(s.Substring(n, m));
            Console.WriteLine(s.Substring(n, s.Length - n));
            Console.WriteLine(s.Substring(0, s.Length - 1));
            Console.WriteLine(s.Substring(s.IndexOf(c,0,s.Length), m));
            Console.WriteLine(s.Substring(s.IndexOf(z, 0, s.Length), m));
        }
    }
}

```



## Clojure


```lisp


(def string "alphabet")
(def n 2)
(def m 4)
(def len (count string))

;starting from n characters in and of m length;
(println
 (subs string n (+ n m)))              ;phab
;starting from n characters in, up to the end of the string;
(println
 (subs string n))                      ;phabet
;whole string minus last character;
(println
 (subs string 0 (dec len)))            ;alphabe
;starting from a known character within the string and of m length;
(let [pos (.indexOf string (int \l))]
  (println
   (subs string pos (+ pos m))))     ;lpha
;starting from a known substring within the string and of m length.
(let [pos (.indexOf string "ph")]
  (println
   (subs string pos (+ pos m))))      ;phab

```



## COBOL


```COBOL
       identification division.
       program-id. substring.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 original.
          05 value "this is a string".
       01 starting  pic 99 value 3.
       01 width     pic 99 value 8.
       01 pos       pic 99.
       01 ender     pic 99.
       01 looking   pic 99.
       01 indicator pic x.
          88 found  value high-value when set to false is low-value.
       01 look-for  pic x(8).

       procedure division.
       substring-main.

       display "Original |" original "|, n = " starting " m = " width
       display original(starting : width)
       display original(starting :)
       display original(1 : length(original) - 1)

       move "a" to look-for
       move 1 to looking
       perform find-position
       if found
           display original(pos : width)
       end-if

       move "is a st" to look-for
       move length(trim(look-for)) to looking
       perform find-position
       if found
           display original(pos : width)
       end-if
       goback.

       find-position.
       set found to false
       compute ender = length(original) - looking
       perform varying pos from 1 by 1 until pos > ender
           if original(pos : looking) equal look-for then
               set found to true
               exit perform
           end-if
       end-perform
       .

       end program substring.
```

```txt
prompt$ cobc -xj substring.cob
Original |this is a string|, n = 03 m = 08
is is a
is is a string
this is a strin
a string
is a str
```



## ColdFusion


###  Classic tag based CFML


```cfm

<cfoutput>
	<cfset str = "abcdefg">
	<cfset n = 2>
	<cfset m = 3>

	<!--- Note: In CF index starts at 1 rather than 0
	starting from n characters in and of m length --->
	#mid( str, n, m )#
	<!--- starting from n characters in, up to the end of the string --->
	<cfset countFromRight = Len( str ) - n + 1>
	#right( str, countFromRight )#
	<!--- whole string minus last character --->
	<cfset allButLast = Len( str ) - 1>
	#left( str, allButLast )#
	<!--- starting from a known character within the string and of m length --->
	<cfset startingIndex = find( "b", str )>
	#mid( str, startingIndex, m )#
	<!--- starting from a known substring within the string and of m length --->
	<cfset startingIndexSubString = find( "bc", str )>
	#mid( str, startingIndexSubString, m )#

</cfoutput>

```

```txt

bcd
bcdefg
abcdef
bcd
bcd

```



###  Script Based CFML


```cfm
<cfscript>

	str="abcdefg";
	n = 2;
	m = 3;

	// Note: In CF index starts at 1 rather than 0
	// starting from n characters in and of m length
	writeOutput( mid( str, n, m ) );
	// starting from n characters in, up to the end of the string
	countFromRight = Len( str ) - n + 1;
	writeOutput( right( str, countFromRight ) );
	// whole string minus last character
	allButLast = Len( str ) - 1;
	writeOutput( left( str, allButLast ) );
	// starting from a known character within the string and of m length
	startingIndex = find( "b", str );
	writeOutput( mid( str, startingIndex, m ) );
	// starting from a known substring within the string and of m length
	startingIndexSubString = find( "bc", str );
	writeOutput( mid( str, startingIndexSubString, m ) );
</cfscript>
```

```txt

bcd
bcdefg
abcdef
bcd
bcd

```



## Common Lisp



```lisp
(let ((string "0123456789")
      (n 2)
      (m 3)
      (start #\5)
      (substring "34"))
  (list (subseq string n (+ n m))
        (subseq string n)
        (subseq string 0 (1- (length string)))
        (let ((pos (position start string)))
          (subseq string pos (+ pos m)))
        (let ((pos (search substring string)))
          (subseq string pos (+ pos m)))))
```


=={{Header|Component Pascal}}==
BlackBox Component Builder

```oberon2

MODULE Substrings;
IMPORT StdLog,Strings;

PROCEDURE Do*;
CONST
	aStr = "abcdefghijklmnopqrstuvwxyz";
VAR
	str: ARRAY 128 OF CHAR;
	pos: INTEGER;
BEGIN
	Strings.Extract(aStr,3,10,str);
	StdLog.String("from 3, 10 characters:> ");StdLog.String(str);StdLog.Ln;
	Strings.Extract(aStr,3,LEN(aStr) - 3,str);
	StdLog.String("from 3, until the end:> ");StdLog.String(str);StdLog.Ln;
	Strings.Extract(aStr,0,LEN(aStr) - 1,str);
	StdLog.String("whole string but last:> ");StdLog.String(str);StdLog.Ln;
	Strings.Find(aStr,'d',0,pos);
	Strings.Extract(aStr,pos + 1,10,str);
	StdLog.String("from 'd', 10 characters:> ");StdLog.String(str);StdLog.Ln;
	Strings.Find(aStr,"de",0,pos);
	Strings.Extract(aStr,pos + LEN("de"),10,str);
	StdLog.String("from 'de', 10 characters:> ");StdLog.String(str);StdLog.Ln;
END Do;

END Substrings.

```

Execute: ^Q Substrings.Do <br/>
```txt

from 3, 10 characters:> defghijklm
from 3, until the end:> defghijklmnopqrstuvwxyz
whole string but last:> abcdefghijklmnopqrstuvwxy
from 'd', 10 characters:> efghijklmn
from 'de', 10 characters:> fghijklmno

```



## D

```d
import std.stdio, std.string;

void main() {
    const s = "the quick brown fox jumps over the lazy dog";
    enum n = 5, m = 3;

    writeln(s[n .. n + m]);

    writeln(s[n .. $]);

    writeln(s[0 .. $ - 1]);

    const i = s.indexOf("q");
    writeln(s[i .. i + m]);

    const j = s.indexOf("qu");
    writeln(s[j .. j + m]);
}
```

```txt

uic
uick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog
qui
qui

```



## Delphi


```Delphi
program ShowSubstring;

{$APPTYPE CONSOLE}

uses SysUtils;

const
  s = '0123456789';
  n = 3;
  m = 4;
  c = '2';
  sub = '456';
begin
  Writeln(Copy(s, n, m));             // starting from n characters in and of m length;
  Writeln(Copy(s, n, Length(s)));     // starting from n characters in, up to the end of the string;
  Writeln(Copy(s, 1, Length(s) - 1)); // whole string minus last character;
  Writeln(Copy(s, Pos(c, s), m));     // starting from a known character within the string and of m length;
  Writeln(Copy(s, Pos(sub, s), m));   // starting from a known substring within the string and of m length.
end.
```


 2345
 23456789
 012345678
 2345
 4567


## E


```e
def string := "aardvarks"
def n := 4
def m := 4
println(string(n, n + m))
println(string(n))
println(string(0, string.size() - 1))
println({string(def i := string.indexOf1('d'), i + m)})
println({string(def i := string.startOf("ard"), i + m)})
```

 vark
 varks
 aardvark
 dvar
 ardv


## EasyLang


<lang>a$ = "2019-05-22 22:54:22"
print substr a$ 11 5
print substr a$ 11 -1
```


```txt

22:54
22:54:22

```



## ECL


```ECL

/* In this task display a substring:

1.       starting from n characters in and of m length;
2.       starting from n characters in, up to the end of the string;
3.       whole string minus last character;
4.       starting from a known character within the string and of m length;
5.       starting from a known substring within the string and of m length.
*/

IMPORT STD; //imports a standard string library

TheString := 'abcdefghij';
CharIn    := 3; //n
StrLength := 4; //m
KnownChar := 'f';
KnownSub  := 'def';
FindKnownChar := STD.Str.Find(TheString, KnownChar,1);
FindKnownSub  := STD.Str.Find(TheString, KnownSub,1);

OUTPUT(TheString[Charin..CharIn+StrLength-1]); //task1
OUTPUT(TheString[Charin..]);                   //task2
OUTPUT(TheString[1..LENGTH(TheString)-1]);     //task3
OUTPUT(TheString[FindKnownChar..FindKnownChar+StrLength-1]);//task4
OUTPUT(TheString[FindKnownSub..FindKnownSub+StrLength-1]);  //task5

/* OUTPUTS:
   defg
   cdefghij
   abcdefghi
   fghi
   defg
*/

```



## Eero


```objc
#import <Foundation/Foundation.h>


int main()
  autoreleasepool
    str := 'abcdefgh'
    n := 2
    m := 3
    Log( '%@', str[0 .. str.length-1] )                     // abcdefgh
    Log( '%@', str[n .. m]            )                     // cd
    Log( '%@', str[n .. str.length-1] )                     // cdefgh
    Log( '%@', str.substringFromIndex: n                  ) // cdefgh
    Log( '%@', str[(str.rangeOfString:'b').location .. m] ) // bcd
  return 0
```


## Elena

ELENA 4.x :

```elena
import extensions;

public program()
{
    var s := "0123456789";
    var n := 3;
    var m := 2;
    var c := $51;
    var z := "345";

    console.writeLine(s.Substring(n, m));
    console.writeLine(s.Substring(n, s.Length - n));
    console.writeLine(s.Substring(0, s.Length - 1));
    console.writeLine(s.Substring(s.indexOf(0, c), m));
    console.writeLine(s.Substring(s.indexOf(0, z), m))
}
```

```txt

34
3456789
012345678
34
34

```



## Elixir


```elixir
s = "abcdefgh"
String.slice(s, 2, 3)           #=> "cde"
String.slice(s, 1..3)           #=> "bcd"
String.slice(s, -3, 2)          #=> "fg"
String.slice(s, 3..-1)          #=> "defgh"

# UTF-8
s = "αβγδεζηθ"
String.slice(s, 2, 3)           #=> "γδε"
String.slice(s, 1..3)           #=> "βγδ"
String.slice(s, -3, 2)          #=> "ζη"
String.slice(s, 3..-1)          #=> "δεζηθ"
```



## Erlang

Interactive session in Erlang shell showing built in functions doing the task.

```txt

1> N = 3.
2> M = 5.
3> string:sub_string( "abcdefghijklm", N ).
"cdefghijklm"
4> string:sub_string( "abcdefghijklm", N, N + M - 1 ).
"cdefg"
6> string:sub_string( "abcdefghijklm", 1, string:len("abcdefghijklm") - 1 ).
"abcdefghijkl"
7> Start_character = string:chr( "abcdefghijklm", $e ).
8> string:sub_string( "abcdefghijklm", Start_character, Start_character + M - 1 ).
"efghi"
9> Start_string = string:str( "abcdefghijklm", "efg" ).
10> string:sub_string( "abcdefghijklm", Start_string, Start_string + M - 1 ).
"efghi"

```



## Euphoria


```Euphoria
sequence baseString, subString, findString
integer findChar
integer m, n

baseString = "abcdefghijklmnopqrstuvwxyz"

-- starting from n characters in and of m length;
n = 12
m = 5
subString = baseString[n..n+m-1]
puts(1, subString )
puts(1,'\n')

-- starting from n characters in, up to the end of the string;
n = 12
subString = baseString[n..$]
puts(1, subString )
puts(1,'\n')

-- whole string minus last character;
subString = baseString[1..$-1]
puts(1, subString )
puts(1,'\n')

-- starting from a known character within the string and of m length;
findChar = 'o'
m = 5
n = find(findChar,baseString)
subString = baseString[n..n+m-1]
puts(1, subString )
puts(1,'\n')

-- starting from a known substring within the string and of m length.
findString = "pq"
m = 5
n = match(findString,baseString)
subString = baseString[n..n+m-1]
puts(1, subString )
puts(1,'\n')
```


 lmnop
 lmnopqrstuvwxyz
 abcdefghijklmnopqrstuvwxy
 opqrs
 pqrst


=={{header|F_Sharp|F#}}==

```fsharp
[<EntryPoint>
]
let main args =
    let s = "一二三四五六七八九十"
    let n, m  = 3, 2
    let c = '六'
    let z = "六七八"

    printfn "%s" (s.Substring(n, m))
    printfn "%s" (s.Substring(n))
    printfn "%s" (s.Substring(0, s.Length - 1))
    printfn "%s" (s.Substring(s.IndexOf(c), m))
    printfn "%s" (s.Substring(s.IndexOf(z), m))
    0
```

```txt
四五
四五六七八九十
一二三四五六七八九
六七
六七
```



## Factor


```factor
USING: math sequences kernel ;

! starting from n characters in and of m length
: subseq* ( from length seq -- newseq ) [ over + ] dip subseq ;

! starting from n characters in, up to the end of the string
: dummy ( seq n -- tailseq ) tail ;

! whole string minus last character
: dummy1 ( seq -- headseq ) but-last ;

USING: fry sequences kernel ;
! helper word
: subseq-from-* ( subseq len seq quot -- seq ) [ nip ] prepose 2keep subseq* ; inline

! starting from a known character within the string and of m length;
: subseq-from-char ( char len seq -- seq ) [ index ] subseq-from-* ;

! starting from a known substring within the string and of m length.
: subseq-from-seq ( subseq len seq -- seq ) [ start ] subseq-from-* ;
```




## Falcon

'''VBA/Python programmer's approach not sure if it's the most falconic way'''

```falcon

/* created by Aykayayciti Earl Lamont Montgomery
April 9th, 2018 */
s = "FalconPL is not just a multi-paradign language but also fun"
n = 12
m = 5

> "starting from n characters in and of m length: ", s[n:n+m]
> "starting from n characters in, up to the end of the string: ", s[n:]
> "whole string minus last character: ", s[0:len(s)-1]
new_n = s.find("j", 0)
> "starting from a known character within the string and of m length: ", s[new_n:new_n+m]
new_n = s.find("mu", 0)
> "starting from a known character within the string and of m length: ", s[new_n:new_n+m]

```

```txt

starting from n characters in and of m length: not j
starting from n characters in, up to the end of the string: not just a multi-paradign language but also fun
whole string minus last character: FalconPL is not just a multi-paradign language but also fu
starting from a known character within the string and of m length: just
starting from a known character within the string and of m length: multi
[Finished in 2.3s]

```



## Forth

/STRING and SEARCH are standard words.  [http://home.earthlink.net/~neilbawd/tool2002.txt SCAN] is widely implemented.  Substrings represented by address/length pairs require neither mutation nor allocation.


```forth
2 constant Pos
3 constant Len
: Str ( -- c-addr u )  s" abcdefgh" ;

Str Pos /string drop Len type    \ cde
Str Pos /string type             \ cdefgh
Str 1- type                      \ abcdefg
Str char d scan drop Len type    \ def
Str s" de" search 2drop Len type \ def
```



## Fortran

```fortran
program test_substring

  character (*), parameter :: string = 'The quick brown fox jumps over the lazy dog.'
  character (*), parameter :: substring = 'brown'
  character    , parameter :: c = 'q'
  integer      , parameter :: n = 5
  integer      , parameter :: m = 15
  integer                  :: i

! Display the substring starting from n characters in and of length m.
  write (*, '(a)') string (n : n + m - 1)
! Display the substring starting from n characters in, up to the end of the string.
  write (*, '(a)') string (n :)
! Display the whole string minus the last character.
  i = len (string) - 1
  write (*, '(a)') string (: i)
! Display the substring starting from a known character and of length m.
  i = index (string, c)
  write (*, '(a)') string (i : i + m - 1)
! Display the substring starting from a known substring and of length m.
  i = index (string, substring)
  write (*, '(a)') string (i : i + m - 1)

end program test_substring
```

 quick brown fox
 quick brown fox jumps over the lazy dog.
 The quick brown fox jumps over the lazy dog
 quick brown fox
 brown fox jumps
Note that in Fortran positions inside character strings are one-based, i. e. the first character is in position one.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim s As String = "123456789"
Dim As Integer n = 3, m = 4
Print Mid(s, n, m)
Print Mid(s, n)
Print Left(s, Len(s) - 1)
'start from "5" say
Print Mid(s, Instr(s, "5"), m)
' start from "12" say
Print Mid(s, Instr(s, "12"), m)
Sleep
```


```txt

3456
3456789
12345678
5678
1234

```



## Free Pascal


```pascal
s[n..n+m]
s[n..high(nativeUInt)]
s[1..length(s)-1]
s[pos(c, s)..pos(c, s)+m]
s[pos(p, s)..pos(p, s)+m]
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=d4baf4adccd2220f63a1019695e072b0 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String = "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"

Print Mid(sString, 11, 5)                     'Starting from n characters in and of m length
Print Mid(sString, 17)                        'Starting from n characters in, up to the end of the string
Print Left(sString, -1)                       'Whole string minus last character
Print Mid(sString, InStr(sString, "B"), 9)    'Starting from a known character within the string and of m length
Print Mid(sString, InStr(sString, "OVER"), 8) 'Starting from a known substring within the string and of m length

End
```

Output:

```txt

BROWN
FOX JUMPS OVER THE LAZY DOG
THE QUICK BROWN FOX JUMPS OVER THE LAZY DO
BROWN FOX
OVER THE

```



## GAP


```gap
LETTERS;
# "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
LETTERS{[5 .. 10]};
# "EFGHIJ"
```



## Go


### ASCII

The task originally had no mention of unicode.  This solution works with ASCII data.

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "ABCDEFGH"
    n, m := 2, 3
    // for reference
    fmt.Println("Index: ", "01234567")
    fmt.Println("String:", s)
    // starting from n characters in and of m length
    fmt.Printf("Start %d, length %d:    %s\n", n, m, s[n : n+m])
    // starting from n characters in, up to the end of the string
    fmt.Printf("Start %d, to end:      %s\n", n, s[n:])
    // whole string minus last character
    fmt.Printf("All but last:         %s\n", s[:len(s)-1])
    // starting from a known character within the string and of m length
    dx := strings.IndexByte(s, 'D')
    fmt.Printf("Start 'D', length %d:  %s\n", m, s[dx : dx+m])
    // starting from a known substring within the string and of m length
    sx := strings.Index(s, "DE")
    fmt.Printf(`Start "DE", length %d: %s`+"\n", m, s[sx : sx+m])
}
```

```txt

Index:  01234567
String: ABCDEFGH
Start 2, length 3:    CDE
Start 2, to end:      CDEFGH
All but last:         ABCDEFG
Start 'D', length 3:  DEF
Start "DE", length 3: DEF

```


===UTF-8===
Strings are generally handled as UTF-8 in Go.

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "αβγδεζηθ"
    r := []rune(s)
    n, m := 2, 3
    kc := 'δ'  // known character
    ks := "δε" // known string
    // for reference
    fmt.Println("Index: ", "01234567")
    fmt.Println("String:", s)
    // starting from n characters in and of m length
    fmt.Printf("Start %d, length %d:    %s\n", n, m, string(r[n:n+m]))
    // starting from n characters in, up to the end of the string
    fmt.Printf("Start %d, to end:      %s\n", n, string(r[n:]))
    // whole string minus last character
    fmt.Printf("All but last:         %s\n", string(r[:len(r)-1]))
    // starting from a known character within the string and of m length
    dx := strings.IndexRune(s, kc)
    fmt.Printf("Start %q, length %d:  %s\n", kc, m, string([]rune(s[dx:])[:m]))
    // starting from a known substring within the string and of m length
    sx := strings.Index(s, ks)
    fmt.Printf("Start %q, length %d: %s\n", ks, m, string([]rune(s[sx:])[:m]))
}
```

```txt

Index:  01234567
String: αβγδεζηθ
Start 2, length 3:    γδε
Start 2, to end:      γδεζηθ
All but last:         αβγδεζη
Start 'δ', length 3:  δεζ
Start "δε", length 3: δεζ

```



## Groovy

Strings in Groovy are 0-indexed.

```groovy
def str = 'abcdefgh'
def n = 2
def m = 3
// #1
println str[n..n+m-1]
/* or */
println str[n..<(n+m)]
// #2
println str[n..-1]
// #3
println str[0..-2]
// #4
def index1 = str.indexOf('d')
println str[index1..index1+m-1]
/* or */
println str[index1..<(index1+m)]
// #5
def index2 = str.indexOf('de')
println str[index2..index2+m-1]
/* or */
println str[index2..<(index2+m)]
```



## Haskell


### =Strings=

A string in Haskell is a list of chars: [Char]

*The first three tasks are simply:

```txt

*Main> take 3 $ drop 2 "1234567890"
"345"

*Main> drop 2 "1234567890"
"34567890"

*Main> init "1234567890"
"123456789"

```

*The last two can be formulated with the following function:

```Haskell
t45 n c s | null sub = []
          | otherwise = take n. head $ sub
  where sub = filter(isPrefixOf c) $ tails s
```



```txt
*Main> t45 3 "4" "1234567890"
"456"

*Main> t45 3 "45" "1234567890"
"456"

*Main> t45 3 "31" "1234567890"
""
```



### =Data.Text=

Testing with an extended set of characters, and using '''Data.Text''' functions, including ''breakOn'':
```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T (Text, take, drop, init, breakOn)
import qualified Data.Text.IO as O (putStrLn)

fromMforN :: Int -> Int -> T.Text -> T.Text
fromMforN n m s = T.take m (T.drop n s)

fromNtoEnd :: Int -> T.Text -> T.Text
fromNtoEnd = T.drop

allButLast :: T.Text -> T.Text
allButLast = T.init

fromCharForN, fromStringForN :: Int -> T.Text -> T.Text -> T.Text
fromCharForN m needle haystack = T.take m $ snd $ T.breakOn needle haystack

fromStringForN = fromCharForN

-- TEST ---------------------------------------------------
main :: IO ()
main =
  mapM_
    O.putStrLn
    ([ fromMforN 9 10
     , fromNtoEnd 20
     , allButLast
     , fromCharForN 6 "话"
     , fromStringForN 6 "大势"
     ] <*>
     ["天地不仁仁者人也🐒话说天下大势分久必合🍑合久必分🔥"])
```

```txt
话说天下大势分久必合
合久必分🔥
天地不仁仁者人也🐒话说天下大势分久必合🍑合久必分
话说天下大势
大势分久必合
```



## HicEst


```hicest
CHARACTER :: string = 'ABCDEFGHIJK', known = 'B',  substring = 'CDE'
REAL, PARAMETER :: n = 5,  m = 8

WRITE(Messagebox) string(n : n + m - 1), "| substring starting from n, length m"
WRITE(Messagebox) string(n :), "| substring starting from n, to  end of string"
WRITE(Messagebox) string(1: LEN(string)-1), "| whole string minus last character"

pos_known = INDEX(string, known)
WRITE(Messagebox) string(pos_known : pos_known+m-1), "| substring starting from pos_known, length m"

pos_substring = INDEX(string, substring)
WRITE(Messagebox) string(pos_substring : pos_substring+m-1), "| substring starting from pos_substring, length m"
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
write("Usage: substring  <string> <first position> <second position> <single character> <substring>")
s := \arglist[1] | "aardvarks"
n := \arglist[2] | 5
m := \arglist[3] | 4
c := \arglist[4] | "d"
ss := \arglist[5] | "ard"

write( s[n+:m] )
write( s[n:0] )
write( s[1:-1] )
write( s[find(c,s)+:m] )
write( s[find(ss,s)+:m] )
end
```



## J



```J
   5{.3}.'Marshmallow'
shmal
   3}.'Marshmallow'
shmallow
   }.'Marshmallow'
arshmallow
   }:'Marshmallow'
Marshmallo
   5{.(}.~ i.&'m')'Marshmallow'
mallo
   5{.(}.~ I.@E.~&'sh')'Marshmallow'
shmal
```


Note that there are other, sometimes better, ways of accomplishing this task.


```J
   'Marshmallow'{~(+i.)/3 5
shmal
```


The <code>taketo</code> / <code>takeafter</code> and <code>dropto</code> / <code>dropafter</code> utilities from the <code>strings</code> script further simplify these types of tasks.

```J
   require 'strings'
   'sh' dropto 'Marshmallow'
shmallow
   5{. 'sh' dropto 'Marshmallow'
shmal
   'sh' takeafter 'Marshmallow'
mallow
```


Note also that these operations work the same way on lists of numbers that they do on this example list of characters.


```J
   3}. 2 3 5 7 11 13 17 19
7 11 13 17 19
   7 11 dropafter 2 3 5 7 11 13 17 19
2 3 5 7 11
```



## Java

Strings in Java are 0-indexed.

```java
String x = "testing123";
System.out.println(x.substring(n, n + m));
System.out.println(x.substring(n));
System.out.println(x.substring(0, x.length() - 1));
int index1 = x.indexOf('i');
System.out.println(x.substring(index1, index1 + m));
int index2 = x.indexOf("ing");
System.out.println(x.substring(index2, index2 + m));
//indexOf methods also have an optional "from index" argument which will
//make indexOf ignore characters before that index
```



## JavaScript

The <code>String</code> object has two similar methods: <code>substr</code> and <code>substring</code>.
*<code>substr(start, [len])</code> returns a substring beginning at a specified location and having a specified length.
*<code>substring(start, [end])</code> returns a string containing the substring from <code>start</code> up to, ''but not including'', <code>end</code>.


```javascript
var str = "abcdefgh";

var n = 2;
var m = 3;

//  *  starting from n characters in and of m length;
str.substr(n, m);  // => "cde"

//  * starting from n characters in, up to the end of the string;
str.substr(n);  // => "cdefgh"
str.substring(n);  // => "cdefgh"

//  * whole string minus last character;
str.substring(0, str.length - 1);  // => "abcdefg"

//  * starting from a known character within the string and of m length;
str.substr(str.indexOf('b'), m);  // => "bcd"

//  * starting from a known substring within the string and of m length.
str.substr(str.indexOf('bc'), m);  // => "bcd"
```



Or, in terms of some familiar functional primitives, translating broadly from Haskell:


```AppleScript
(function () {
    'use strict';

    //  take :: Int -> Text -> Text
    function take(n, s) {
        return s.substr(0, n);
    }

    //  drop :: Int -> Text -> Text
    function drop(n, s) {
        return s.substr(n);
    }


    // init :: Text -> Text
    function init(s) {
        var n = s.length;
        return (n > 0 ? s.substr(0, n - 1) : undefined);
    }

    // breakOn :: Text -> Text -> (Text, Text)
    function breakOn(strPattern, s) {
        var i = s.indexOf(strPattern);
        return i === -1 ? [strPattern, ''] : [s.substr(0, i), s.substr(i)];
    }


    var str = '一二三四五六七八九十';


    return JSON.stringify({

        'from n in, of m length': (function (n, m) {
            return take(m, drop(n, str));
        })(4, 3),


        'from n in, up to end' :(function (n) {
            return drop(n, str);
        })(3),


        'all but last' : init(str),


        'from matching char, of m length' : (function (pattern, s, n) {
            return take(n, breakOn(pattern, s)[1]);
        })('五', str, 3),


        'from matching string, of m length':(function (pattern, s, n) {
            return take(n, breakOn(pattern, s)[1]);
        })('六七', str, 4)

    }, null, 2);

})();
```


```JavaScript
{
  "from n in, of m length": "五六七",
  "from n in, up to end": "四五六七八九十",
  "all but last": "一二三四五六七八九",
  "from matching char, of m length": "五六七",
  "from matching string, of m length": "六七八九"
}
```



## jq

For this exercise we use the Chinese characters for 1 to 10, the character for "10" being "十":

```jq
def s: "一二三四五六七八九十";
```


jq strings are UTF-8 strings, and array-based string indexing and
most string functions, such as length/0, are based on Unicode code
points. However, the function index/1 currently uses character counts when its input is a string, and therefore in the following we use ix/1 defined as follows:

```jq
def ix(s): explode | index(s|explode);
```


(Users who have access to the regex function match/1 can use it, as illustrated in the comments below.)

Since jq arrays and strings have an index origin of 0, "n characters in" is interpreted to require an index of (n+1).

```jq
# starting from n characters in and of m length:  .[n+1: n+m+1]
"s[1:2] => \( s[1:2] )",

# starting from n characters in, up to the end of the string:  .[n+1:]
"s[9:] => \( s[9:] )",

# whole string minus last character: .[0:length-1]
"s|.[0:length-1] => \(s | .[0:length-1] )",

# starting from a known character within the string and of m length:
  # jq 1.4: ix(c) as $i | .[ $i: $i + m]
  # jq>1.4: match(c).offset as $i | .[ $i: $i + m]
"s | ix(\"五\") as $i | .[$i: $i + 1] => \(s | ix("五") as $i | .[$i: $i + 1] )",


# starting from a known substring within the string and of m length:
  # jq 1.4: ix(sub) as $i | .[ $i: $i + m]
  # jq>1.4: match(sub).offset as $i | .[ $i: $i + m]
"s | ix(\"五六\") as $i | .[$i: $i + 2] => " +
 "\( s | ix("五六") as $i | .[$i: $i + 2] )"
```

# {{Out}}

```sh
$ jq -M -n -r -f Substring.jq
s[1:2] => 二
s[9:] => 十
s|.[0:length-1] => 一二三四五六七八九
s | ix("五") as $i | .[$i: $i + 1] => 五
s | ix("五六") as $i | .[$i: $i + 2] => 五六
```



## Jsish

```javascript
#!/usr/local/bin/jsish -u %s

var str = "abcdefgh";

var n = 2;
var m = 3;

// In jsish, semi-colon first character lines are echoed with result
;str;
;n;
;m;

//  *  starting from n characters in and of m length;
;str.substr(n, m);

//  * starting from n characters in, up to the end of the string;
;str.substr(n);
;str.substring(n);

//  * whole string minus last character;
;str.substring(0, str.length - 1);

//  * starting from a known character within the string and of m length;
;str.substr(str.indexOf('b'), m);

//  * starting from a known substring within the string and of m length.
;str.substr(str.indexOf('bc'), m);


/* Functional */
var res = (function () {
    'use strict';

    //  take :: Int -> Text -> Text
    function take(n, s) {
        return s.substr(0, n);
    }

    //  drop :: Int -> Text -> Text
    function drop(n, s) {
        return s.substr(n);
    }


    // init :: Text -> Text
    function init(s) {
        var n = s.length;
        return (n > 0 ? s.substr(0, n - 1) : undefined);
    }

    // breakOn :: Text -> Text -> (Text, Text)
    function breakOn(strPattern, s) {
        var i = s.indexOf(strPattern);
        return i === -1 ? [strPattern, ''] : [s.substr(0, i), s.substr(i)];
    }


    var str = 'abcdefgh';


    return JSON.stringify({

        'from 4 in, of 3 length': (function (n, m) {
            return take(m, drop(n, str));
        })(4, 3),


        'from 3 in, up to end' : (function (n) {
            return drop(n, str);
        })(3),


        'all but last' : init(str),


        'from matching b, of length 3' : (function (pattern, s, n) {
            return take(n, breakOn(pattern, s)[1]);
        })('b', str, 3),


        'from matching bc, of length 4':(function (pattern, s, n) {
            return take(n, breakOn(pattern, s)[1]);
        })('bc', str, 4)

    }, true);

})();
;res;
```


```txt
prompt$ jsish --U substringing.jsi
str ==> abcdefgh
n ==> 2
m ==> 3
str.substr(n, m) ==> cde
str.substr(n) ==> cdefgh
str.substring(n) ==> cdefgh
str.substring(0, str.length - 1) ==> abcdefgh
str.substr(str.indexOf('b'), m) ==> bcd
str.substr(str.indexOf('bc'), m) ==> bcd
res ==> { "all but last":"abcdefg", "from 3 in, up to end":"defgh", "from 4 in, of 3 length":"efg", "from matching b, of length 3":"bcd", "from matching bc, of length 4":"bcde" }

prompt$ jsish -u -update true substringing.jsi
Created substringing.jsi

prompt$ jsish -u substringing.jsi
[PASS] substringing.jsi
```


The initial --U is a run with echo mode.  The '''-u -update true''' puts jsish in unit test mode, and will add a comparison block.  After the test pass, the code file is changed to


```javascript
#!/usr/local/bin/jsish -u %s

var str = "abcdefgh";

var n = 2;
var m = 3;

// In jsish, semi-colon first character lines are echoed with result
;str;
;n;
;m;

//  *  starting from n characters in and of m length;
;str.substr(n, m);

//  * starting from n characters in, up to the end of the string;
;str.substr(n);
;str.substring(n);

//  * whole string minus last character;
;str.substring(0, str.length - 1);

//  * starting from a known character within the string and of m length;
;str.substr(str.indexOf('b'), m);

//  * starting from a known substring within the string and of m length.
;str.substr(str.indexOf('bc'), m);


/* Functional */
var res = (function () {
    'use strict';

    //  take :: Int -> Text -> Text
    function take(n, s) {
        return s.substr(0, n);
    }

    //  drop :: Int -> Text -> Text
    function drop(n, s) {
        return s.substr(n);
    }


    // init :: Text -> Text
    function init(s) {
        var n = s.length;
        return (n > 0 ? s.substr(0, n - 1) : undefined);
    }

    // breakOn :: Text -> Text -> (Text, Text)
    function breakOn(strPattern, s) {
        var i = s.indexOf(strPattern);
        return i === -1 ? [strPattern, ''] : [s.substr(0, i), s.substr(i)];
    }


    var str = 'abcdefgh';


    return JSON.stringify({

        'from 4 in, of length 3': (function (n, m) {
            return take(m, drop(n, str));
        })(4, 3),


        'from 3 in, up to end' : (function (n) {
            return drop(n, str);
        })(3),


        'all but last' : init(str),


        'from matching b, of length 3' : (function (pattern, s, n) {
            return take(n, breakOn(pattern, s)[1]);
        })('b', str, 3),


        'from matching bc, of length 4':(function (pattern, s, n) {
            return take(n, breakOn(pattern, s)[1]);
        })('bc', str, 4)

    }, true);

})();
;res;

/*
=!EXPECTSTART!=
str ==> abcdefgh
n ==> 2
m ==> 3
str.substr(n, m) ==> cde
str.substr(n) ==> cdefgh
str.substring(n) ==> cdefgh
str.substring(0, str.length - 1) ==> abcdefgh
str.substr(str.indexOf('b'), m) ==> bcd
str.substr(str.indexOf('bc'), m) ==> bcd
res ==> { "all but last":"abcdefg", "from 3 in, up to end":"defgh", "from 4 in, of length 3":"efg", "from matching b, of length 3":"bcd", "from matching bc, of length 4":"bcde" }
=!EXPECTEND!=
*/
```



## Julia

By default, the type of the string is infered from its elements. In the example below, the string s is an ASCII string. In order to interpret the string as an UTF8 string with logical access to its argument, one should use
<code>CharString("/'''\ʕ•ᴥ•ʔ/'''\"...)</code>. Without the CharString declaration, the string is interpreted as an UTF8 string with access through its byte representation.


```julia
julia>
 s = "abcdefg"
"abcdefg"

julia> n = 3
3

julia> s[n:end]
"cdefg"

julia> m=2
2

julia> s[n:n+m]
"cde"

julia> s[1:end-1]
"abcdef"

julia> s[search(s,'c')]
'c'

julia> s[search(s,'c'):search(s,'c')+m]
"cde"
```



## Kotlin

Strings in Kotlin are 0-indexed:

```scala
// version 1.0.6

fun main(args: Array<String>) {
    val s = "0123456789"
    val n = 3
    val m = 4
    val c = '5'
    val z = "12"
    var i: Int
    println(s.substring(n, n + m))
    println(s.substring(n))
    println(s.dropLast(1))
    i = s.indexOf(c)
    println(s.substring(i, i + m))
    i = s.indexOf(z)
    println(s.substring(i, i + m))
}
```


```txt

3456
3456789
012345678
5678
1234

```



## LabVIEW

To enhance readability, this task was split into two separate GUI's. In the second, note that "Known Substring" can be a single character.<br/>1:
[[file:LabVIEW_Substring1.png]]<br/>2:
[[file:LabVIEW_Substring2.png]]


## Lang5


```lang5
: cr "\n". ; [] '__A set : dip swap __A swap 1 compress append '__A set execute __A
    -1 extract nip ; : nip swap drop ; : tuck swap over ; : -rot rot rot ; : 0= 0 == ; : 1+ 1 + ;
: 2dip swap 'dip dip ; : 2drop drop drop ; : |a,b> over - iota + ; : bi* 'dip dip execute ; : bi@ dup bi* ;
: comb "" split ; : concat "" join ; : empty? length 0= ; : tail over lensize |a,b> subscript ;
: lensize length nip ; : while do 'dup dip 'execute 2dip rot if dup 2dip else break then loop 2drop ;

: <substr>  comb -rot over + |a,b> subscript concat ;
: str-tail  tail concat ;
: str-index
    : 2streq  2dup over lensize iota subscript eq '* reduce ;
    swap 'comb bi@ length -rot 0 -rot
    "2dup 'lensize bi@ <="
    "2streq if 0 reshape else '1+ 2dip 0 extract drop then"
    while empty? if 2drop tuck == if drop -1 then else 4 ndrop -1 then ;

'abcdefgh 'str set 2 'n set 3 'm set
n m str <substr>
str comb n str-tail
str "d" str-index m str <substr>
str "de" str-index m str <substr>
```





## Lasso


```Lasso
local(str = 'The quick grey rhino jumped over the lazy green fox.')

//starting from n characters in and of m length;
#str->substring(16,5) //rhino

//starting from n characters in, up to the end of the string
#str->substring(16) //rhino jumped over the lazy green fox.

//whole string minus last character
#str->substring(1,#str->size - 1) //The quick grey rhino jumped over the lazy green fox

//starting from a known character within the string and of m length;
#str->substring(#str->find('g'),10) //grey rhino

//starting from a known substring within the string and of m length
#str->substring(#str->find('rhino'),12) //rhino jumped
```



## LFE

From the LFE REPL:


```lisp

> (set n 3)
3
> (set m 5)
5
> (string:sub_string "abcdefghijklm" n)
"cdefghijklm"
> (string:sub_string "abcdefghijklm" n (+ n m -1))
"cdefg"
> (string:sub_string "abcdefghijklm" 1 (- (length "abcdefghijklm") 1))
"abcdefghijkl"
> (set char-index (string:chr "abcdefghijklm" #\e))
5
> (string:sub_string "abcdefghijklm" char-index (+ char-index m -1))
"efghi"
> (set start-str (string:str "abcdefghijklm" "efg"))
5
> (string:sub_string "abcdefghijklm" start-str (+ start-str m -1))
"efghi"

```



## Liberty BASIC


```lb
'These tasks can be completed with various combinations of Liberty Basic's
'built in Mid$()/ Instr()/ Left$()/ Right$()/ and Len() functions, but these
'examples only use the Mid$()/ Instr()/ and Len() functions.

baseString$ = "Thequickbrownfoxjumpsoverthelazydog."
n = 12
m = 5

'starting from n characters in and of m length
Print Mid$(baseString$, n, m)

'starting from n characters in, up to the end of the string
Print Mid$(baseString$, n)

'whole string minus last character
Print Mid$(baseString$, 1, (Len(baseString$) - 1))

'starting from a known character within the string and of m length
Print Mid$(baseString$, Instr(baseString$, "f", 1), m)

'starting from a known substring within the string and of m length
Print Mid$(baseString$, Instr(baseString$, "jump", 1), m)
```



## Lingo


```lingo
str = "The quick brown fox jumps over the lazy dog"

-- starting from n characters in and of m length
n = 5
m = 11
put str.char[n..n+m-1]
-- "quick brown"

-- starting from n characters in, up to the end of the string
n = 11
put str.char[n..str.length]
-- "brown fox jumps over the lazy dog"

-- whole string minus last character
put str.char[1..str.length-1]
-- "The quick brown fox jumps over the lazy do"

-- starting from a known character within the string and of m length
c = "x"
m = 7
pos = offset(c, str)
put str.char[pos..pos+m-1]
-- "x jumps"

-- starting from a known substring within the string and of m length
sub = "fox"
m = 9
pos = offset(sub, str)
put str.char[pos..pos+m-1]
-- "fox jumps"
```



## LiveCode


```LiveCode
put "pple" into x
answer char 2 to char 5 of x  // n = 2, m=5
answer char 2 to len(x) of x  // n = 2, m = len(x), can also use -1
answer char 1 to -2 of x  // n = 1, m = 1 less than length of string
answer char offset("p",x) to -1 of x // known char "p" to end of string
answer char offset("pl",x) to -1 of x // known "pl" to end of string
```

n.b. Offset also supports a third parameter "charsToSkip" allowing you to loop through subsequent matches of the substring.


## Logo

The following are defined to behave similarly to the built-in index operator ITEM. As with most Logo list operators, these are designed to work for both words (strings) and lists.

```logo
to items :n :thing
  if :n >= count :thing [output :thing]
  output items :n butlast :thing
end

to butitems :n :thing
  if or :n <= 0 empty? :thing [output :thing]
  output butitems :n-1 butfirst :thing
end

to middle :n :m :thing
  output items :m-(:n-1) butitems :n-1 :thing
end

to lastitems :n :thing
  if :n >= count :thing [output :thing]
 output lastitems :n butfirst :thing
end

to starts.with :sub :thing
  if empty? :sub [output "true]
  if empty? :thing [output "false]
  if not equal? first :sub first :thing [output "false]
  output starts.with butfirst :sub butfirst :thing
end

to members :sub :thing
  output cascade [starts.with :sub ?] [bf ?] :thing
end

; note: Logo indices start at one
make "s "abcdefgh
print items 3 butitems 2 :s ; cde
print middle 3 5  :s   ; cde
print butitems 2  :s   ; cdefgh
print butlast     :s   ; abcdefg
print items 3 member  "d  :s ; def
print items 3 members "de :s ; def
```



## Logtalk

Using atoms for representing strings and usng the same sample data as e.g. in the Java solution:

```logtalk

:- object(substring).

    :- public(test/5).

    test(String, N, M, Character, Substring) :-
        sub_atom(String, N, M, _, Substring1),
        write(Substring1), nl,
        sub_atom(String, N, _, 0, Substring2),
        write(Substring2), nl,
        sub_atom(String, 0, _, 1, Substring3),
        write(Substring3), nl,
        % there can be multiple occurences of the character
        once(sub_atom(String, Before4, 1, _, Character)),
        sub_atom(String, Before4, M, _, Substring4),
        write(Substring4), nl,
        % there can be multiple occurences of the substring
        once(sub_atom(String, Before5, _, _, Substring)),
        sub_atom(String, Before5, M, _, Substring5),
        write(Substring5), nl.

:- end_object.

```

```text

| ?- ?- substring::test('abcdefgh', 2, 3, 'b', 'bc').
cde
cdefgh
abcdefg
bcd
bcd
yes

```



## Lua


```lua
str = "abcdefghijklmnopqrstuvwxyz"
n, m = 5, 15

print( string.sub( str, n, m ) )    -- efghijklmno
print( string.sub( str, n, -1 ) )   -- efghijklmnopqrstuvwxyz
print( string.sub( str, 1, -2 ) )   -- abcdefghijklmnopqrstuvwxy

pos = string.find( str, "i" )
if pos ~= nil then print( string.sub( str, pos, pos+m ) ) end -- ijklmnopqrstuvwx

pos = string.find( str, "ijk" )
if pos ~= nil then print( string.sub( str, pos, pos+m ) ) end-- ijklmnopqrstuvwx

-- Alternative (more modern) notation

print ( str:sub(n,m) )         -- efghijklmno
print ( str:sub(n) )           -- efghijklmnopqrstuvwxyz
print ( str:sub(1,-2) )        -- abcdefghijklmnopqrstuvwxy

pos = str:find "i"
if pos then print (str:sub(pos,pos+m)) end -- ijklmnopqrstuvwx

pos = str:find "ijk"
if pos then print (str:sub(pos,pos+m)) end d-- ijklmnopqrstuvwx


```



## M2000 Interpreter

By default a sting can contain anything, and has a maximum length of 2GBytes. Literals are always UTF-16LE. Print/edit done as UTF-16LE. But we can use Str$(a_string) to convert UTF-16LE to Ansi, using Locale id. To display it we can use Chr$(a_String), to convert back to UTF-16LE. Mid$, Right$, Left$, Instr,RInstr works for Ansi using "as byte". For Utf16-le, we get next 16bit value, not exactly next char, but for many languages it is exactly next char.

Function for length always return length as Words (two bytes), so we can get half, if we have an odd number of ansi characters. For Utf16-le there is another Len function,Len.Disp  which returns the needed positions for displaying characters. So Print LEN.DISP("aããz")=4  :  Print Len("̃ãz")=4


```M2000 Interpreter

Module CheckAnsi {
      \\ ANSI STRING
      Locale 1033
      \\ convert UTF16-LE to ANSI 8bit
      s$ =Str$("ABCDEFG")
      Print Len(s$)=3.5  ' 3.5 words, means 7 bytes (3.5*2)
      AnsiLen=Len(s$)*2
      ' From 4th byte get 3 bytes
      n=4
      m=3
      substring$=Mid$(s$, n, m as byte)
      substring2End$=Mid$(s$, n , AnsiLen as byte)
      substringMinusOne$=Left$(s$, AnsiLen-1 as byte)
      substringFromKnownCharacter$=Mid$(s$, Instr(s$, str$("B") as byte) , m as byte)
      substringFromKnownSubstring$=Mid$(s$, Instr(s$, str$("BC") as byte) , m as byte)
      Print Len(substring$)*2=m

      \\ convert to UTF-16LE
      Print Chr$(substring$)="DEF"
      Print Chr$(substring2End$)="DEFG"
      Print Chr$(substringMinusOne$)="ABCDEF"
      Print Chr$(substringFromKnownCharacter$)="BCD"
      Print Chr$(substringFromKnownSubstring$)="BCD"
}
CheckAnsi
Module CheckUTF16LE {
      s$ ="ABCDEFG"
      Print Len(s$)=7
      Utf16Len=Len(s$)
      ' From 4th byte get 3 bytes
      n=4
      m=3
      substring$=Mid$(s$, n, m)
      substring2End$=Mid$(s$, n , Utf16Len)
      substringMinusOne$=Left$(s$, Utf16Len-1)
      substringFromKnownCharacter$=Mid$(s$, Instr(s$, "B") , m)
      substringFromKnownSubstring$=Mid$(s$, Instr(s$, "BC") , m)
      Print Len(substring$)=m

      \\ convert to UTF-16LE
      Print substring$="DEF"
      Print substring2End$="DEFG"
      Print substringMinusOne$="ABCDEF"
      Print substringFromKnownCharacter$="BCD"
      Print substringFromKnownSubstring$="BCD"
}
CheckUTF16LE


```




## Maple


```Maple

> n, m := 3, 5:
> s := "The Higher, The Fewer!":
> s[ n .. n + m - 1 ];
                     "e Hig"

```

There are a few ways to get everything from the n-th character on.

```Maple

> s[ n .. -1 ] = s[ n .. ];
 "e Higher, The Fewer!" = "e Higher, The Fewer!"

> StringTools:-Drop( s, n - 1 );
              "e Higher, The Fewer!"

```

There are a few ways to get all but the last character.

```Maple

> s[ 1 .. -2 ] = s[ .. -2 ];
"The Higher, The Fewer" = "The Higher, The Fewer"

> StringTools:-Chop( s );
             "The Higher, The Fewer"

```

The <code>searchtext</code> command returns the position of a matching substring.

```Maple

> pos := searchtext( ",", s ):
> s[ pos .. pos + m - 1 ];
                     ", The"

> pos := searchtext( "Higher", s ):
> s[ pos .. pos + m - 1 ];
                     "Highe"

```

But, note that <code>searchtext</code> returns 0 when there is no match, and 0 is not a valid index into a string.


## Mathematica

The <code>StringTake</code> and <code>StringDrop</code> are relevant for this exercise.


```Mathematica

n = 2
m = 3
StringTake["Mathematica", {n+1, n+m-1}]

StringDrop["Mathematica", n]

(* StringPosition returns a list of starting and ending character positions for a substring *)
pos = StringPosition["Mathematica", "e"][[1]][[1]]
StringTake["Mathematica", {pos, pos+m-1}]

(* Similar to above *)
pos = StringPosition["Mathematica", "the"][[1]]
StringTake["Mathematica", {pos, pos+m-1}]

```


=={{header|MATLAB}} / {{header|Octave}}==

Unicode, UTF-8, UTF-16 is only partially supported. In some cases, a conversion of unicode2native() or native2unicode() is necessary.

```Matlab

    % starting from n characters in and of m length;
        s(n+(1:m))
        s(n+1:n+m)
    % starting from n characters in, up to the end of the string;
        s(n+1:end)
    % whole string minus last character;
        s(1:end-1)
    % starting from a known character within the string and of m length;
        s(find(s==c,1)+[0:m-1])
    % starting from a known substring within the string and of m length.
        s(strfind(s,pattern)+[0:m-1])

```




## Maxima


```maxima
s: "the quick brown fox jumps over the lazy dog";
substring(s, 17);
/* "fox jumps over the lazy dog" */
substring(s, 17, 20);
/* "fox" */
```




## MUMPS

MUMPS has the first position in a string numbered as 1.

```MUMPS

SUBSTR(S,N,M,C,K)
 ;show substring operations
 ;S is the string
 ;N is a position within the string (that is, n<length(string))
 ;M is an integer of positions to show
 ;C is a character within the string S
 ;K is a substring within the string S
 ;$Find returns the position after the substring
 NEW X
 WRITE !,"The base string is:",!,?5,"'",S,"'"
 WRITE !,"From position ",N," for ",M," characters:"
 WRITE !,?5,$EXTRACT(S,N,N+M-1)
 WRITE !,"From position ",N," to the end of the string:"
 WRITE !,?5,$EXTRACT(S,N,$LENGTH(S))
 WRITE !,"Whole string minus last character:"
 WRITE !,?5,$EXTRACT(S,1,$LENGTH(S)-1)
 WRITE !,"Starting from character '",C,"' for ",M," characters:"
 SET X=$FIND(S,C)-$LENGTH(C)
 WRITE !,?5,$EXTRACT(S,X,X+M-1)
 WRITE !,"Starting from string '",K,"' for ",M," characters:"
 SET X=$FIND(S,K)-$LENGTH(K)
 W !,?5,$EXTRACT(S,X,X+M-1)
 QUIT

```

Usage:

```txt

USER>D SUBSTR^ROSETTA("ABCD1234efgh",3,4,"D","23")

The base string is:
     'ABCD1234efgh'
From position 3 for 4 characters:
     CD12
From position 3 to the end of the string:
     CD1234efgh
Whole string minus last character:
     ABCD1234efg
Starting from character 'D' for 4 characters:
     D123
Starting from string '23' for 4 characters:
     234e

```



## Nemerle


```Nemerle
using System;
using System.Console;

module Substrings
{
    Main() : void
    {
        string s = "0123456789";
        def n = 3;
        def m = 2;
        def c = '3';
        def z = "345";

        WriteLine(s.Substring(n, m));
        WriteLine(s.Substring(n, s.Length - n));
        WriteLine(s.Substring(0, s.Length - 1));
        WriteLine(s.Substring(s.IndexOf(c,0,s.Length), m));
        WriteLine(s.Substring(s.IndexOf(z, 0, s.Length), m));
    }
}
```



## NetRexx

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols

s = 'abcdefghijk'
n = 4
m = 3

say s
say s.substr(n, m)
say s.substr(n)
say s.substr(1, s.length - 1)
say s.substr(s.pos('def'), m)
say s.substr(s.pos('g'), m)

return

```

<pre style="height: 24ex; overflow:scroll;">
abcdefghijk
def
defghijk
abcdefghij
def
ghi

```



## newLISP


```newLISP>
 (set 'str "alphabet" 'n 2 'm 4)
4
> ; starting from n characters in and of m length
> (slice str n m)
"phab"
> ; starting from n characters in, up to the end of the string
> (slice str n)
"phabet"
> ; whole string minus last character
> (chop str)
"alphabe"
> ; starting from a known character within the string and of m length
> (slice str (find "l" str) m)
"lpha"
> ; starting from a known substring within the string and of m length
> (slice str (find "ph" str) m)
"phab"

```



## Nim


```nim
import strutils

let
  s = "abcdefgh"
  n = 2
  m = 3
  c = 'd'
  cs = "cd"
var i = 0

# starting from n=2 characters in and m=3 in length
echo s[n-1 .. n+m-2]

# starting from n characters in, up to the end of the string
echo s[n-1 .. s.high]

# whole string minus last character:
echo s[0 .. <s.high]

# starting from a known character c='d'within the string and of m length
i = s.find(c)
echo s[i .. <i+m]

# starting from a known substring cs="cd" within the string and of m length
i = s.find(cs)
echo s[i .. <i+m]
```



## Niue


```Niue
( based on the JavaScript code )
'abcdefgh 's ;
s str-len 'len ;
2 'n ;
3 'm ;

( starting from n characters in and of m length )
s n n m + substring . ( => cde ) newline

( starting from n characters in, up to the end of the string )
s n len substring . ( => cdefgh ) newline

( whole string minus last character )
s 0 len 1 - substring . ( => abcdefg ) newline

( starting from a known character within the string and of m length )
s s 'b str-find dup m + substring . ( => bcd ) newline

( starting from a known substring within the string and of m length )
s s 'bc str-find dup m + substring . ( => bcd ) newline

```



## Objeck


```objeck

bundle Default {
  class SubString {
    function : Main(args : String[]) ~ Nil {
      s := "0123456789";

      n := 3;
      m := 4;
      c := '2';
      sub := "456";

      s->SubString(n, m)->PrintLine();
      s->SubString(n)->PrintLine();
      s->SubString(0, s->Size())->PrintLine();
      s->SubString(s->Find(c), m)->PrintLine();
      s->SubString(s->Find(sub), m)->PrintLine();
    }
  }
}

```



## OCaml


```ocaml
# let s = "ABCDEFGH" ;;
val s : string = "ABCDEFGH"

# let n, m = 2, 3 ;;
val n : int = 2
val m : int = 3

# String.sub s n m ;;
- : string = "CDE"

# String.sub s n (String.length s - n) ;;
- : string = "CDEFGH"

# String.sub s 0 (String.length s - 1) ;;
- : string = "ABCDEFG"

# String.sub s (String.index s 'D') m ;;
- : string = "DEF"

# #load "str.cma";;
# let n = Str.search_forward (Str.regexp_string "DE") s 0 in
  String.sub s n m ;;
- : string = "DEF"
```



## Oforth



```Oforth
: substrings(s, n, m)
   s sub(n, m) println
   s right(s size n - 1 +) println
   s left(s size 1 - ) println
   s sub(s indexOf('d'), m) println
   s sub(s indexOfAll("de"), m) println ;
```


```txt

"abcdefgh" 2 3 substrings
bcd
bcdefgh
abcdefg
def
def

```



## Oz


```oz
declare
  fun {DropUntil Xs Prefix}
     case Xs of nil then nil
     [] _|Xr then
        if {List.isPrefix Prefix Xs} then Xs
        else {DropUntil Xr Prefix}
        end
     end
  end

  Digits = "1234567890"
in
  {ForAll
   [{List.take {List.drop Digits 2} 3}     = "345"
    {List.drop Digits 2}                   = "34567890"
    {List.take Digits {Length Digits}-1}   = "123456789"
    {List.take {DropUntil Digits "4"} 3}   = "456"
    {List.take {DropUntil Digits "56"} 3}  = "567"
    {List.take {DropUntil Digits "31"} 3}  = ""
   ]
   System.showInfo}
```



## PARI/GP


```parigp

\\ Returns the substring of string str specified by the start position s and length n.
\\ If n=0 then to the end of str.
\\ ssubstr() 3/5/16 aev
ssubstr(str,s=1,n=0)={
my(vt=Vecsmall(str),ve,vr,vtn=#str,n1);
if(vtn==0,return(""));
if(s<1||s>vtn,return(str));
n1=vtn-s+1; if(n==0,n=n1); if(n>n1,n=n1);
ve=vector(n,z,z-1+s); vr=vecextract(vt,ve); return(Strchr(vr));
}

{\\ TEST
my(s="ABCDEFG",ns=#s);
print(" *** Testing ssubstr():");
print("1.",ssubstr(s,2,3));
print("2.",ssubstr(s));
print("3.",ssubstr(s,,ns-1));
print("4.",ssubstr(s,2));
print("5.",ssubstr(s,,4));
print("6.",ssubstr(s,0,4));
print("7.",ssubstr(s,3,7));
print("8.|",ssubstr("",1,4),"|");
}

```


```txt

 *** Testing ssubstr():
1.BCD
2.ABCDEFG
3.ABCDEF
4.BCDEFG
5.ABCD
6.ABCDEFG
7.CDEFG
8.||

```



## Pascal

See [[#Delphi|Delphi]] and [[#Free Pascal|Free Pascal]]


## Perl


```perl
my $str = 'abcdefgh';
print substr($str, 2, 3), "\n"; # Returns 'cde'
print substr($str, 2), "\n"; # Returns 'cdefgh'
print substr($str, 0, -1), "\n"; #Returns 'abcdefg'
print substr($str, index($str, 'd'), 3), "\n"; # Returns 'def'
print substr($str, index($str, 'de'), 3), "\n"; # Returns 'def'
```



## Perl 6


```perl6
my $str = 'abcdefgh';
my $n = 2;
my $m = 3;
say $str.substr($n, $m);
say $str.substr($n);
say $str.substr(0, *-1);
say $str.substr($str.index('d'), $m);
say $str.substr($str.index('de'), $m);
```



## Phix


```Phix
--(1) starting from n characters in and of m length;
--(2) starting from n characters in, up to the end of the string;
--(3) whole string minus last character;
--(4) starting from a known character within the string and of m length;
--(5) starting from a known substring within the string and of m length.

constant sentence = "the last thing the man said was the",
         n = 10, m = 5
integer k, l
l = n+m-1
if l<=length(sentence) then
    ?sentence[n..l]           -- (1)
end if
if n<=length(sentence) then
    ?sentence[n..-1]          -- (2) or [n..$]
end if
if length(sentence)>0 then
    ?sentence[1..-2]          -- (3) or [1..$-1]
end if
k = find('m',sentence)
l = k+m-1
if l<=length(sentence) then
    ?sentence[k..l]           -- (4)
end if
k = match("aid",sentence)
l = k+m-1
if l<=length(sentence) then
    ?sentence[k..l]           -- (5)
end if
```

```txt

"thing"
"thing the man said was the"
"the last thing the man said was th"
"man s"
"aid w"

```

Alternative version with no error handling, for those in a hurry (same ouput):

```Phix
?sentence[n..n+m-1]
?sentence[n..-1]
?sentence[1..-2]
?(sentence[find('m',sentence)..$])[1..m]
?(sentence[match("aid",sentence)..$])[1..m]
```



## PHP


```php
<?php
$str = 'abcdefgh';
$n = 2;
$m = 3;
echo substr($str, $n, $m), "\n"; //cde
echo substr($str, $n), "\n"; //cdefgh
echo substr($str, 0, -1), "\n"; //abcdefg
echo substr($str, strpos($str, 'd'), $m), "\n"; //def
echo substr($str, strpos($str, 'de'), $m), "\n"; //def
?>
```



## PicoLisp


```PicoLisp
(let Str (chop "This is a string")
   (prinl (head 4 (nth Str 6)))        # From 6 of 4 length
   (prinl (nth Str 6))                 # From 6 up to the end
   (prinl (head -1 Str))               # Minus last character
   (prinl (head 8 (member "s" Str)))   # From character "s" of length 8
   (prinl                              # From "isa" of length 8
      (head 8
         (seek '((S) (pre? "is a" S)) Str) ) ) )
```

```txt
is a
is a string
This is a strin
s is a s
is a str
```



## PL/I


```PL/I

s='abcdefghijk';
n=4; m=3;
u=substr(s,n,m);
u=substr(s,n);
u=substr(s,1,length(s)-1);
u=left(s,length(s)-1);
u=substr(s,1,length(s)-1);
u=substr(s,index(s,'g'),m);

```



## PowerShell

Since .NET and PowerShell use zero-based indexing, all character indexes have to be reduced by one.

```powershell
# test string
$s = "abcdefgh"
# test parameters
$n, $m, $c, $s2 = 2, 3, [char]'d', $s2 = 'cd'

# starting from n characters in and of m length
# n = 2, m = 3
$s.Substring($n-1, $m)              # returns 'bcd'

# starting from n characters in, up to the end of the string
# n = 2
$s.Substring($n-1)                  # returns 'bcdefgh'

# whole string minus last character
$s.Substring(0, $s.Length - 1)      # returns 'abcdefg'

# starting from a known character within the string and of m length
# c = 'd', m =3
$s.Substring($s.IndexOf($c), $m)    # returns 'def'

# starting from a known substring within the string and of m length
# s2 = 'cd', m = 3
$s.Substring($s.IndexOf($s2), $m)   # returns 'cde'
```



## PureBasic


```PureBasic
If OpenConsole()

  Define baseString.s, m, n

  baseString = "Thequickbrownfoxjumpsoverthelazydog."
  n = 12
  m = 5

  ;Display the substring starting from n characters in and of m length.
  PrintN(Mid(baseString, n, m))

  ;Display the substring starting from n characters in, up to the end of the string.
  PrintN(Mid(baseString, n)) ;or PrintN(Right(baseString, Len(baseString) - n))

  ;Display the substring whole string minus last character
  PrintN(Left(baseString, Len(baseString) - 1))

  ;Display the substring starting from a known character within the string and of m length.
  PrintN(Mid(baseString, FindString(baseString, "b", 1), m))

  ;Display the substring starting from a known substring within the string and of m length.
  PrintN(Mid(baseString, FindString(baseString, "ju", 1), m))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

```txt
wnfox
wnfoxjumpsoverthelazydog.
Thequickbrownfoxjumpsoverthelazydog
brown
jumps
```



## Prolog


```prolog

substring_task(Str, N, M, Char, SubStr) :-
    sub_string(Str, N, M, _, Span),
    sub_string(Str, N, _, 0, ToEnd),
    sub_string(Str, 0, _, 1, MinusLast),
    string_from_substring_to_m(Str, Char, M, FromCharToMth),
    string_from_substring_to_m(Str, SubStr, M, FromSubToM),
    maplist( writeln,
            [ 'from n to m ':Span,
              'from n to end ': ToEnd,
              'string minus last char ': MinusLast,
              'form known char to m ': FromCharToMth,
              'from known substring to m ': FromSubToM ]).


string_from_substring_to_m(String, Sub, M, FromSubToM) :-
    sub_string(String, Before, _, _, Sub),
    sub_string(String, Before, M, _, FromSubToM).

```


Running it:


```prolog

?- substring_task("abcdefghijk", 2, 4, "d", "ef").
from n to m :cdef
from n to end :cdefghijk
string minus last char :abcdefghij
form known char to m :defg
from known substring to m :efgh
true

```



## Python

Python uses zero-based indexing, so the n'th character is at index n-1.


```python
>>>
 s = 'abcdefgh'
>>> n, m, char, chars = 2, 3, 'd', 'cd'
>>> # starting from n=2 characters in and m=3 in length;
>>> s[n-1:n+m-1]
'bcd'
>>> # starting from n characters in, up to the end of the string;
>>> s[n-1:]
'bcdefgh'
>>> # whole string minus last character;
>>> s[:-1]
'abcdefg'
>>> # starting from a known character char="d" within the string and of m length;
>>> indx = s.index(char)
>>> s[indx:indx+m]
'def'
>>> # starting from a known substring chars="cd" within the string and of m length.
>>> indx = s.index(chars)
>>> s[indx:indx+m]
'cde'
>>>
```



## R


```R
s <- "abcdefgh"
n <- 2; m <- 2; char <- 'd'; chars <- 'cd'
substring(s, n, n + m)
substring(s, n)
substring(s, 1, nchar(s)-1)
indx <- which(strsplit(s, '')[[1]] %in% strsplit(char, '')[[1]])
substring(s, indx, indx + m)
indx <- which(strsplit(s, '')[[1]] %in% strsplit(chars, '')[[1]])[1]
substring(s, indx, indx + m)
```



## Racket



```Racket

#lang racket

(define str "abcdefghijklmnopqrstuvwxyz")

(define n 10)
(define m 2)
(define start-char #\x)
(define start-str "xy")

;; starting from n characters in and of m length;
(substring str n (+ n m)) ; -> "kl"

;; starting from n characters in, up to the end of the string;
(substring str m) ; -> "klmnopqrstuvwxyz"

;; whole string minus last character;
(substring str 0 (sub1 (string-length str))) ; -> "abcdefghijklmnopqrstuvwxy"

;; starting from a known character within the string and of m length;
(substring str (caar (regexp-match-positions (regexp-quote (string start-char))
                                             str))) ; -> "xyz"

;; starting from a known substring within the string and of m length.
(substring str (caar (regexp-match-positions (regexp-quote start-str)
                                             str))) ; -> "xyz"

```



## Raven


```Raven
define println use $s
   $s print "\n" print

"0123456789" as $str

$str 3 2 extract println      # at 4th pos get 2 chars
$str 8 4 extract println      # at 9th pos get 4 chars (when only 1 char available)


$str 3  $str length  extract println      # at 4th pos get all chars to end of str
$str 3  0x7FFFFFFF  extract println      # at 4th pos get all chars to end of str

$str 3 -1 extract println      # at 4th pos get rest of chars but last one
$str 0 -1 extract println      # all chars but last one

"3" as $matchChr               # starting chr for extraction
4 as $subLen                   # Nr chars after found starting char
$str $matchChr split as $l
"" $l 0 set     $l $matchChr join
0 $subLen extract println

"345" as $matchChrs            # starting chrs for extraction
6 as $subLen                   # Nr chars after found starting chars
$str $matchChrs split as $l
"" $l 0 set     $l $matchChrs join
0 $subLen extract println
```

```txt
34
89
3456789
3456789
345678
012345678
3456
345678
```



## REBOL


```REBOL
REBOL [
	Title: "Retrieve Substring"
	URL: http://rosettacode.org/wiki/Substring#REBOL
]

s: "abcdefgh"  n: 2  m: 3  char: #"d"  chars: "cd"

; Note that REBOL uses base-1 indexing. Strings are series values,
; just like blocks or lists so I can use the same words to manipulate
; them. All these examples use the 'copy' function against the 's'
; string with a particular offset as needed.

; For the fragment "copy/part  skip s n - 1  m", read from right to
; left.  First you have 'm', which we ignore for now. Then evaluate
; 'n - 1' (makes 1), to adjust the offset. Then 'skip' jumps from the
; start of the string by that offset. 'copy' starts copying from the
; new start position and the '/part' refinement limits the copy by 'm'
; characters.

print ["Starting from n, length m:"
	copy/part  skip s n - 1  m]

; It may be helpful to see the expression with optional parenthesis:

print ["Starting from n, length m (parens):"
	(copy/part  (skip s (n - 1))  m)]

; This example is much simpler, so hopefully it's easier to see how
; the string start is position for the copy:

print ["Starting from n to end of string:"
	copy skip s n - 1]

print ["Whole string minus last character:"
	copy/part s (length? s) - 1]

print ["Starting from known character, length m:"
	copy/part  find s char  m]

print ["Starting from substring, length m:"
	copy/part  find s chars  m]
```


```txt
Script: "Retrieve Substring" (6-Dec-2009)
Starting from n, length m: bcd
Starting from n, length m (parens): bcd
Starting from n to end of string: bcdefgh
Whole string minus last character: abcdefg
Starting from known character, length m: def
Starting from substring, length m: cde
```



## REXX

Note:   in REXX,   the 1<sup>st</sup> character   ''index''   of a string is   '''1''',   not   '''0'''.

```rexx
/*REXX program demonstrates various ways to extract substrings from a  string  of characters.*/
$='abcdefghijk';  n=4;  m=3        /*define some constants: string, index, length of string. */
say 'original string='$            /* [↑]   M   can be zero  (which indicates a null string).*/
L=length($)                        /*the length of the  $  string   (in bytes or characters).*/
              say center(1,30,'═') /*show a centered title for the  1st  task requirement.   */
u=substr($, n, m)                  /*start from  N  characters in  and of  M  length.        */
say u
parse var $ =(n) a +(m)            /*an alternate method by using the  PARSE  instruction.   */
say a
              say center(2,30,'═') /*show a centered title for the  2nd  task requirement.   */
u=substr($,n)                      /*start from  N  characters in,  up to the end-of-string. */
say u
parse var $ =(n) a                 /*an alternate method by using the  PARSE  instruction.   */
say a
              say center(3,30,'═') /*show a centered title for the  3rd  task requirement.   */
u=substr($, 1, L-1)                /*OK:     the entire string  except  the last character.  */
say u
v=substr($, 1, max(0, L-1) )       /*better: this version handles the case of a null string. */
say v
lm=L-1
parse var $ a +(lm)                /*an alternate method by using the  PARSE  instruction.   */
say a
              say center(4,30,'═') /*show a centered title for the  4th  task requirement.   */
u=substr($,pos('g',$), m)          /*start from a known char within the string of length  M. */
say u
parse var $ 'g' a +(m)             /*an alternate method by using the  PARSE  instruction.   */
say a
              say center(5,30,'═') /*show a centered title for the  5th  task requirement.   */
u=substr($,pos('def',$),m)         /*start from a known substr within the string of length M.*/
say u
parse var $ 'def' a +(m)           /*an alternate method by using the  PARSE  instruction.   */
say a                              /*stick a fork in it, we're all done and Bob's your uncle.*/
```

'''output'''   when using the (internal) default strings:

```txt

original string=abcdefghijk
══════════════1═══════════════
def
def
══════════════2═══════════════
defghijk
defghijk
══════════════3═══════════════
abcdefghij
abcdefghij
abcdefghij
══════════════4═══════════════
ghi
ghi
══════════════5═══════════════
def
def

```

Programming note:   generally, the REXX   '''parse'''   statement is faster than using an assignment statement ''and'' using a BIF ('''b'''uilt-'''i'''n '''f'''unction), but the use of   '''parse'''   is more obtuse to novice programmers.




## RPG


```rpg
      *                                         1...5....1....5....2....5..
     D myString        S             30    inz('Liebe bewegt das Universum!')
     D output          S             30    inz('')
     D n               S              2  0 inz(1)
     D m               S              2  0 inz(5)
     D length          S              2  0 inz(0)
     D find            S              2  0 inz(0)

      /free
       *inlr = *on;
        dsply    %subst(myString:n:m);
        dsply    %subst(myString:7:20);

        length = %len(%trim(myString));
        dsply    %subst(myString:1:length-1);

        find   = %scan('U':myString);
        dsply    %subst(myString:find:9);

        find   = %scan('bewegt':myString);
        dsply    %subst(myString:find:%len('bewegt'));

        output = ' *** end *** ';
        dsply ' ' ' ' output;
      /end-free
```

```txt

DSPLY  Liebe
DSPLY  bewegt das Universum
DSPLY  Liebe bewegt das Universum
DSPLY  Universum
DSPLY  bewegt

```



## Ring


```ring
cStr = "a":"h"  # 'abcdefgh'
n = 3  m = 3
# starting from n characters in and of m length
See substr(cStr,n, m) + nl          #=> cde
# starting from n characters in, up to the end of the string
See substr(cStr,n) + nl             #=> cdefgh
# whole string minus last character
See substr(cstr,1,len(cStr)-1) + nl #=> abcdefg
# starting from a known character within the string and of m length
See substr(cStr,substr(cStr,"e"),m) +nl #=> efg
# starting from a known substring within the string and of m length
See substr(cStr,substr(cStr,"de"),m) +nl #=> def

```



## Ruby


```ruby
str = 'abcdefgh'
n = 2
m = 3
puts str[n, m]                  #=> cde
puts str[n..m]                  #=> cd
puts str[n..-1]                 #=> cdefgh
puts str[0..-2]                 #=> abcdefg
puts str[str.index('d'), m]     #=> def
puts str[str.index('de'), m]    #=> def
puts str[/a.*d/]                #=> abcd
```



## Rust


```rust

let s = "abc文字化けdef";
let n = 2;
let m = 3;

    // Print 3 characters starting at index 2 (c文字)
println!("{}", s.chars().skip(n).take(m).collect::<String>());

    // Print all characters starting at index 2 (c文字化けdef)
println!("{}", s.chars().skip(n).collect::<String>());

    // Print all characters except the last (abc文字化けde)
println!("{}", s.chars().rev().skip(1).collect::<String>());

    // Print 3 characters starting with 'b' (bc文)
let cpos = s.find('b').unwrap();
println!("{}", s[cpos..].chars().take(m).collect::<String>());

    // Print 3 characters starting with "けd" (けde)
let spos = s.find("けd").unwrap();
println!("{}", s[spos..].chars().take(m).collect::<String>());

```



## Run BASIC


```runbasic
n  = 2
m  = 3
s$ = "abcd"
a$ = mid$(a$,n,m)                  ' starting from n characters in and of m length
a$ = mid$(a$,n)                    ' starting from n characters in, up to the end of the string
a$ = Print mid$(a$,1,(len(a$)-1))  ' whole string minus last character
a$ = mid$(a$,instr(a$,s$,1),m)     ' starting from a known character within the string and of m length
a$ = mid$(a$,instr(a$,s$,1), m)    ' starting from a known substring within the string and of m length.
```



## SAS


```sas
data _null_;
   a="abracadabra";
   b=substr(a,2,3); /* first number is position, starting at 1,
                       second number is length */
   put _all_;
run;
```



## Sather


```sather
class MAIN is
  main is
    s ::= "hello world shortest program";
    #OUT + s.substring(12, 5) + "\n";
    #OUT + s.substring(6) + "\n";
    #OUT + s.head( s.size - 1) + "\n";
    #OUT + s.substring(s.search('w'), 5) + "\n";
    #OUT + s.substring(s.search("ro"), 3) + "\n";
  end;
end;
```



## Scala

```scala
object Substring {
  // Ruler             1         2         3         4         5         6
  //         012345678901234567890123456789012345678901234567890123456789012
  val str = "The good life is one inspired by love and guided by knowledge."
  val (n, m) = (21, 16) // An one-liner to set n = 21, m = 16

  // Starting from n characters in and of m length
  assert("inspired by love" == str.slice(n, n + m))

  // Starting from n characters in, up to the end of the string
  assert("inspired by love and guided by knowledge." == str.drop(n))

  // Whole string minus last character
  assert("The good life is one inspired by love and guided by knowledge" == str.init)

  // Starting from a known character within the string and of m length
  assert("life is one insp" == str.dropWhile(_ != 'l').take(m) )

  // Starting from a known substring within the string and of m length
  assert("good life is one" == { val i = str.indexOf("good"); str.slice(i, i + m) })
  // Alternatively
  assert("good life is one" == str.drop(str.indexOf("good")).take(m))
}
```



## Scheme

```scheme
(define s "Hello, world!")
(define n 5)
(define m (+ n 6))

(display (substring s n m))
(newline)

(display (substring s n))
(newline)

(display (substring s 0 (- (string-length s) 1)))
(newline)

(display (substring s (string-index s #\o) m))
(newline)

(display (substring s (string-contains s "lo") m))
(newline)
```



## Sed


```bash

# 2 chars starting from 3rd
$ echo string | sed -r 's/.{3}(.{2}).*/\1/'
in
# remove first 3 chars
echo string | sed -r 's/^.{3}//'
# delete last char
$ echo string | sed -r 's/.$//'
strin
# `r' with two following chars
$ echo string | sed -r 's/.*(r.{2}).*/\1/'
rin

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const string: stri is "abcdefgh";
    const integer: N is 2;
    const integer: M is 3;
  begin
    writeln(stri[N len M]);
    writeln(stri[N ..]);
    writeln(stri[.. pred(length(stri))]);
    writeln(stri[pos(stri, 'c') len M]);
    writeln(stri[pos(stri, "de") len M]);
  end func;
```


```txt

bcd
bcdefgh
abcdefg
cde
def

```



## Sidef


```ruby
var str = 'abcdefgh';
var n = 2;
var m = 3;
say str.substr(n, m);                   #=> cde
say str.substr(n);                      #=> cdefgh
say str.substr(0, -1);                  #=> abcdefg
say str.substr(str.index('d'), m);      #=> def
say str.substr(str.index('de'), m);     #=> def
```



## Slate


```slate

#s := 'hello world shortest program'.
#n := 13.
#m := 4.
inform: (s copyFrom: n to: n + m).
inform: (s copyFrom: n).
inform: s allButLast.
inform: (s copyFrom: (s indexOf: $w) to: (s indexOf: $w) + m).
inform: (s copyFrom: (s indexOfSubSeq: 'ro') to: (s indexOfSubSeq: 'ro') + m).

```



## Smalltalk

The distinction between searching a single character or a string into another string is rather blurred. In the following code, instead of using <tt>'w'</tt> (a string) we could use <tt>$w</tt> (a character), but it makes no difference.


```smalltalk
|s|
s := 'hello world shortest program'.

(s copyFrom: 13 to: (13+4)) displayNl.
"4 is the length (5) - 1, since we need the index of the
 last char we want, which is included"

(s copyFrom: 7) displayNl.
(s allButLast) displayNl.

(s copyFrom: ((s indexOfRegex: 'w') first)
   to: ( ((s indexOfRegex: 'w') first) + 4) ) displayNl.
(s copyFrom: ((s indexOfRegex: 'ro') first)
   to: ( ((s indexOfRegex: 'ro') first) + 2) ) displayNl.
```


These last two examples in particular seem rather complex, so
we can extend the string class.

```smalltalk
String extend [
  copyFrom: index length: nChar [
    ^ self copyFrom: index to: ( index + nChar - 1 )
  ]
  copyFromRegex: regEx length: nChar [
    |i|
    i := self indexOfRegex: regEx.
    ^ self copyFrom: (i first) length: nChar
  ]
].

"and show it simpler..."

(s copyFrom: 13 length: 5) displayNl.
(s copyFromRegex: 'w' length: 5) displayNl.
(s copyFromRegex: 'ro' length: 3) displayNl.
```



## SNOBOL4


```snobol
	string = "abcdefghijklmnopqrstuvwxyz"
	n = 12
	m = 5
	known_char = "q"
	known_str = "pq"
*  starting from n characters in and of m length;
	string len(n - 1) len(m) . output
* starting from n characters in, up to the end of the string;
	string len(n - 1) rem . output
* whole string minus last character;
	string rtab(1) . output
* starting from a known character within the string and of m length;
	string break(known_char) len(m) . output
* starting from a known substring <= m within the string and of m length.
	string (known_str len(m - size(known_str))) . output
end
```


  lmnop
  lmnopqrstuvwxyz
  abcdefghijklmnopqrstuvwxy
  qrstu
  pqrst


## SQL PL

In Db2, there are different ways to find the position of a character or substring. For this reason, several examples are shown. Please take a look at the documentation for more details.

```sql pl

select 'the quick brown fox jumps over the lazy dog' from sysibm.sysdummy1;
select substr('the quick brown fox jumps over the lazy dog', 5, 15) from sysibm.sysdummy1;
select substr('the quick brown fox jumps over the lazy dog', 32) from sysibm.sysdummy1;
select substr('the quick brown fox jumps over the lazy dog', 1, length ('the quick brown fox jumps over the lazy dog') - 1) from sysibm.sysdummy1;

select locate('j', 'the quick brown fox jumps over the lazy dog') from sysibm.sysdummy1;
select locate_in_string('the quick brown fox jumps over the lazy dog', 'j') from sysibm.sysdummy1;
select posstr('the quick brown fox jumps over the lazy dog', 'j') from sysibm.sysdummy1;
select position('j', 'the quick brown fox jumps over the lazy dog',  OCTETS) from sysibm.sysdummy1;

select substr('the quick brown fox jumps over the lazy dog', locate('j', 'the quick brown fox jumps over the lazy dog')) from sysibm.sysdummy1;

select locate('fox', 'the quick brown fox jumps over the lazy dog') from sysibm.sysdummy1;
select locate_in_string('the quick brown fox jumps over the lazy dog', 'fox') from sysibm.sysdummy1;
select posstr('the quick brown fox jumps over the lazy dog', 'fox') from sysibm.sysdummy1;
select position('fox', 'the quick brown fox jumps over the lazy dog',  OCTETS) from sysibm.sysdummy1;

select substr('the quick brown fox jumps over the lazy dog', locate('fox', 'the quick brown fox jumps over the lazy dog')) from sysibm.sysdummy1;

```

Output:

```txt

db2 => select 'the quick brown fox jumps over the lazy dog' from sysibm.sysdummy1;
1
-------------------------------------------
the quick brown fox jumps over the lazy dog

  1 record(s) selected.
db2 => select substr('the quick brown fox jumps over the lazy dog', 5, 15) from sysibm.sysdummy1;
1
---------------
quick brown fox

  1 record(s) selected.

db2 => select substr('the quick brown fox jumps over the lazy dog', 32) from sysibm.sysdummy1;
1
-------------------------------------------
the lazy dog

  1 record(s) selected.

db2 => select substr('the quick brown fox jumps over the lazy dog', 1, length ('the quick brown fox jumps over the lazy dog') - 1) from sysibm.sysdummy1;
1
-------------------------------------------
the quick brown fox jumps over the lazy do

  1 record(s) selected.


db2 => select substr('the quick brown fox jumps over the lazy dog', locate('j', 'the quick brown fox jumps over the lazy dog')) from sysibm.sysdummy1;

1
-------------------------------------------
jumps over the lazy dog

  1 record(s) selected.

db2 => select substr('the quick brown fox jumps over the lazy dog', locate('fox', 'the quick brown fox jumps over the lazy dog')) from sysibm.sysdummy1;

1
-------------------------------------------
fox jumps over the lazy dog

  1 record(s) selected.

```



## Stata



```stata
s = "Ἐν ἀρχῇ ἐποίησεν ὁ θεὸς τὸν οὐρανὸν καὶ τὴν γῆν"

usubstr(s, 25, 11)
  τὸν οὐρανὸν

usubstr(s, 25, .)
  τὸν οὐρανὸν καὶ τὴν γῆν

usubstr(s, 1, ustrlen(s)-1)
  Ἐν ἀρχῇ ἐποίησεν ὁ θεὸς τὸν οὐρανὸν καὶ τὴν γῆ

usubstr(s, -3, .)
  γῆν
```



## Swift


```swift

let string = "Hello, Swift language"
let (n, m) = (5, 4)

// Starting from `n` characters in and of `m` length.
do {
  let start = string.startIndex.advancedBy(n)
  let end = start.advancedBy(m)
  // Pure-Swift (standard library only):
  _ = string[start..<end]
  // With Apple's Foundation framework extensions:
  string.substringWithRange(start..<end)
}

// Starting from `n` characters in, up to the end of the string.
do {
  // Pure-Swift (standard library only):
  _ = String(
    string.characters.suffix(string.characters.count - n)
  )
  // With Apple's Foundation framework extensions:
  _ = string.substringFromIndex(string.startIndex.advancedBy(n))
}

// Whole string minus last character.
do {
  // Pure-Swift (standard library only):
  _ = String(
    string.characters.prefix(
      string.characters.count.predecessor()
    )
  )
  // With Apple's Foundation framework extensions:
  _ = string.substringToIndex(string.endIndex.predecessor())
}

// Starting from a known character within the string and of `m` length.
do {
  // Pure-Swift (standard library only):
  let character = Character("l")
  guard let characterIndex = string.characters.indexOf(character) else {
    fatalError("Index of '\(character)' character not found.")
  }
  let endIndex = characterIndex.advancedBy(m)
  _ = string[characterIndex..<endIndex]
}

// Starting from a known substring within the string and of `m` length.
do {
  // With Apple's Foundation framework extensions:
  let substring = "Swift"
  guard let range = string.rangeOfString(substring) else {
    fatalError("Range of substring \(substring) not found")
  }
  let start = range.startIndex
  let end = start.advancedBy(m)
  string[start..<end]
}

```



## Tcl


```tcl
set str "abcdefgh"
set n 2
set m 3

puts [string range $str $n [expr {$n+$m-1}]]
puts [string range $str $n end]
puts [string range $str 0 end-1]
# Because Tcl does substrings with a pair of indices, it is easier to express
# the last two parts of the task as a chained pair of [string range] operations.
# A maximally efficient solution would calculate the indices in full first.
puts [string range [string range $str [string first "d" $str] end] [expr {$m-1}]]
puts [string range [string range $str [string first "de" $str] end] [expr {$m-1}]]

# From Tcl 8.5 onwards, these can be contracted somewhat.
puts [string range [string range $str [string first "d" $str] end] $m-1]
puts [string range [string range $str [string first "de" $str] end] $m-1]
```

Of course, if you were doing 'position-plus-length' a lot, it would be easier to add another subcommand to <code>string</code>, like this:
```tcl
# Define the substring operation, efficiently
proc ::substring {string start length} {
    string range $string $start [expr {$start + $length - 1}]
}
# Plumb it into the language
set ops [namespace ensemble configure string -map]
dict set ops substr ::substring
namespace ensemble configure string -map $ops

# Now show off by repeating the challenge!
set str "abcdefgh"
set n 2
set m 3

puts [string substr $str $n $m]
puts [string range $str $n end]
puts [string range $str 0 end-1]
puts [string substr $str [string first "d" $str] $m]
puts [string substr $str [string first "de" $str] $m]
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
string="abcdefgh", n=4,m=n+2
substring=EXTRACT (string,#n,#m)
  PRINT substring
substring=Extract (string,#n,0)
  PRINT substring
substring=EXTRACT (string,0,-1)
  PRINT substring
n=SEARCH (string,":d:"),m=n+2
substring=EXTRACT (string,#n,#m)
  PRINT substring
substring=EXTRACT (string,":{substring}:"|,0)
  PRINT substring

```

```txt

de
defgh
abcdefg
de
fgh

```



## UNIX Shell


### POSIX shells

```bash
str="abc qrdef qrghi"
n=6
m=3

expr "x$str" : "x.\{$n\}\(.\{1,$m\}\)"
expr "x$str" : "x.\{$n\}\(.*\)"
printf '%s\n' "${str%?}"
expr "r${str#*r}" : "\(.\{1,$m\}\)"
expr "qr${str#*qr}" : "\(.\{1,$m\}\)"
```



```txt
def
def qrghi
abc qrdef qrgh
rde
qrd
```


This program uses [http://www.openbsd.org/cgi-bin/man.cgi?query=expr&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html ''expr(1)''] to capture a substring.


### Bourne Shell

```bash
str="abc qrdef qrghi"
n=6
m=3

expr "x$str" : "x.\{$n\}\(.\{1,$m\}\)"
expr "x$str" : "x.\{$n\}\(.*\)"
expr "x$str" : "x\(.*\)."

index() {
	i=0 s=$1
	until test "x$s" = x || expr "x$s" : "x$2" >/dev/null; do
		i=`expr $i + 1` s=`expr "x$s" : "x.\(.*\)"`
	done
	echo $i
}
expr "x$str" : "x.\{`index "$str" r`\}\(.\{1,$m\}\)"
expr "x$str" : "x.\{`index "$str" qr`\}\(.\{1,$m\}\)"
```



```txt
def
def qrghi
abc qrdef qrgh
rde
qrd
```



### zsh

Note that the last two constructs won't work with bash as only zsh supports nested string manipulation.

```bash

#!/bin/zsh
string='abcdefghijk'
echo ${string:2:3}              # Display 3 chars starting 2 chars in ie: 'cde'
echo ${string:2}                # Starting 2 chars in, display to end of string
echo ${string:0:${#string}-1}   # Whole string minus last character
echo ${string%?}                 # Shorter variant of the above
echo ${${string/*c/c}:0:3}      # Display 3 chars starting with 'c'
echo ${${string/*cde/cde}:0:3}  # Display 3 chars starting with 'cde'

```



### Pipe

This example shows how to [http://www.openbsd.org/cgi-bin/man.cgi?query=cut&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html cut(1)] a substring from a string.

```bash
#!/bin/sh
str=abcdefghijklmnopqrstuvwxyz
n=12
m=5

printf %s "$str" | cut -c $n-`expr $n + $m - 1`
printf %s "$str" | cut -c $n-
printf '%s\n' "${str%?}"
printf q%s "${str#*q}" | cut -c 1-$m
printf pq%s "${str#*pq}" | cut -c 1-$m
```


```txt
$ sh substring.sh
lmnop
lmnopqrstuvwxyz
abcdefghijklmnopqrstuvwxy
qrstu
pqrst
```


* <tt>cut -c</tt> counts characters from 1.
* cut(1) runs on each line of standard input, therefore the string must not contain a newline.
* One can use the old style <tt>`expr $n + $m - 1`</tt> or the new style <tt>$((n + m - 1))</tt> to calculate the index.
* cut(1) prints the substring to standard output. To put the substring in a variable, use one of
** <tt>var=`printf %s "$str" | cut -c $n-\`expr $n + $m - 1\``</tt>
** <tt>var=$( printf %s "$str" | cut -c $n-$((n + m - 1)) )</tt>


## Vala


```vala

string s = "Hello, world!";
int n = 1;
int m = 3;
// start at n and go m letters
string s_n_to_m = s[n:n+m];
// start at n and go to end
string s_n_to_end = s[n:s.length];
// start at beginning and show all but last
string s_notlast = s[0:s.length - 1];
// start from known letter and then go m letters
int index_of_l = s.index_of("l");
string s_froml_for_m = s[index_of_l:index_of_l + m];
// start from known substring then go m letters
int index_of_lo = s.index_of("lo");
string s_fromlo_for_m = s[index_of_lo:index_of_lo + m];

```



## VBA

```vb
Public Sub substring()
'(1) starting from n characters in and of m length;
'(2) starting from n characters in, up to the end of the string;
'(3) whole string minus last character;
'(4) starting from a known character within the string and of m length;
'(5) starting from a known substring within the string and of m length.

    sentence = "the last thing the man said was the"
    n = 10: m = 5

    '(1)
    Debug.Print Mid(sentence, n, 5)
    '(2)
    Debug.Print Right(sentence, Len(sentence) - n + 1)
    '(3)
    Debug.Print Left(sentence, Len(sentence) - 1)
    '(4)
    k = InStr(1, sentence, "m")
    Debug.Print Mid(sentence, k, 5)
    '(5)
    k = InStr(1, sentence, "aid")
    Debug.Print Mid(sentence, k, 5)
End Sub
```
```txt
thing
thing the man said was the
the last thing the man said was th
man s
aid w
```



## VBScript


```vb

s = "rosettacode.org"

'starting from n characters in and of m length
WScript.StdOut.WriteLine Mid(s,8,4)

'starting from n characters in, up to the end of the string
WScript.StdOut.WriteLine Mid(s,8,Len(s)-7)

'whole string minus last character
WScript.StdOut.WriteLine Mid(s,1,Len(s)-1)

'starting from a known character within the string and of m length
WScript.StdOut.WriteLine Mid(s,InStr(1,s,"c"),4)

'starting from a known substring within the string and of m length
WScript.StdOut.WriteLine Mid(s,InStr(1,s,"ose"),6)

```


```txt

code
code.org
rosettacode.or
code
osetta

```



## Wart


```python
s <- "abcdefgh"
s.0
=> "a"

# starting from n characters in and of m length;
def (substr s start len)
  (s start start+len)
(substr s 3 2)
=> "de"

# starting from n characters in, up to the end of the string
(s 3 nil)
=> "defgh"

# whole string minus last character;
(s 3 -1)
=> "defg"

# starting from a known character within the string and of <tt>m</tt> length;
# starting from a known substring within the string and of <tt>m</tt> length.
let start (pos s pat)
  (s start start+m)
```



## Yorick


```yorick
str = "abcdefgh";
n = 2;
m = 3;

// starting from n character in and of m length
write, strpart(str, n:n+m-1);
// starting from n character in, up to the end of the string
write, strpart(str, n:);
// whole string minus last character
write, strpart(str, :-1);
// starting from a known character within the string and of m length
match = strfind("d", str);
write, strpart(str, [match(1), match(1)+m]);
// starting from a known substring within the string and of m length
match = strfind("cd", str);
write, strpart(str, [match(1), match(1)+m]);
```



## zkl

8 bit ASCII

```zkl
var str = "abcdefgh", n = 2, m = 3;
str[n,m]  //-->"cde"
str[n,*]  //-->"cdefgh"
str[0,-1] //-->"abcdefg"
str[str.find("d"),m]  //-->"def"
str[str.find("de"),m] //-->"def"
```



