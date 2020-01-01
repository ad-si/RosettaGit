+++
title = "Reverse a string"
description = ""
date = 2019-10-14T10:52:06Z
aliases = []
[extra]
id = 2831
[taxonomies]
categories = []
tags = []
+++

{{task|String manipulation}}
;Task:
Take a string and reverse it.

For example, "asdf" becomes "fdsa".


;Extra credit:
Preserve Unicode combining characters.

For example, "as⃝df̅" becomes "f̅ds⃝a", not "̅fd⃝sa".


{{Template:Strings}}





## 0815

This program reverses each line of its input.

```0815
}:r:     Start reader loop.
  !~>&   Push a character to the "stack".
  <:a:=- Stop reading on newline.
^:r:
@>       Rotate the newline to the end and enqueue a sentinel 0.
{~       Dequeue and rotate the first character into place.
}:p:
  ${~    Print the current character until it's 0.
^:p:
#:r:     Read again.
```


{{out}}


```bash
echo -e "foo\nbar" | 0815 rev.0
oof
rab
```



## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set (S/360)
and an ASSIST macro (XPRNT) to keep the code as short as possible.

```360asm
*        Reverse a string          21/05/2016
REVERSE  CSECT
         USING  REVERSE,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         MVC    TMP(L'C),C         tmp=c
         LA     R8,C               @c[1]
         LA     R9,TMP+L'C-1       @tmp[n-1]
         LA     R6,1               i=1
         LA     R7,L'C             n=length(c)
LOOPI    CR     R6,R7              do i=1 to n
         BH     ELOOPI             leave i
         MVC    0(1,R8),0(R9)        substr(c,i,1)=substr(tmp,n-i+1,1)
         LA     R8,1(R8)             @c=@c+1
         BCTR   R9,0                 @tmp=@tmp-1
         LA     R6,1(R6)             i=i+1
         B      LOOPI              next i
ELOOPI   XPRNT  C,L'C              print c
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
C        DC     CL12'edoC attesoR'
TMP      DS     CL12
         YREGS
         END    REVERSE
```

{{out}}

```txt

Rosetta Code

```



## 8th

In 8th strings are UTF-8 and the language retains characters per-se:

```forth

"abc" s:rev

```

{{out}}
 <tt>"cba"</tt>


## ACL2


```Lisp
(reverse "hello")
```


ACL2 does not support unicode.


## ActionScript


```ActionScript
function reverseString(string:String):String
{
	var reversed:String = new String();
	for(var i:int = string.length -1; i >= 0; i--)
		reversed += string.charAt(i);
	return reversed;
}

function reverseStringCQAlternative(string:String):String
{
	return string.split('').reverse().join('');
}
```



## Ada


```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Reverse_String is
   function Reverse_It (Item : String) return String is
      Result : String (Item'Range);
   begin
      for I in Item'range loop
         Result (Result'Last - I + Item'First) := Item (I);
      end loop;
      return Result;
   end Reverse_It;
begin
   Put_Line (Reverse_It (Get_Line));
end Reverse_String;
```




## Agda

Using the Agda standard library, version 0.6.

```agda2
module reverse_string where

open import Data.String
open import Data.List

reverse_string : String → String
reverse_string s = fromList (reverse (toList s))
```



## Aime


```aime
o_(b_reverse("Hello, World!"), "\n");
```



## ALGOL 68

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
PROC reverse = (REF STRING s)VOID:
  FOR i TO UPB s OVER 2 DO
    CHAR c = s[i];
    s[i] := s[UPB s - i + 1];
    s[UPB s - i + 1] := c
  OD;

main:
(
  STRING text := "Was it a cat I saw";
  reverse(text);
  print((text, new line))
)
```

{{out}}

```txt

was I tac a ti saW

```



## Apex


```java

String str = 'Hello World!';
str = str.reverse();
system.debug(str);

```


== {{header|APL}} ==

```apl
      ⌽'asdf'
fdsa
```



## AppleScript

{{works with |AppleScript| 2.0 or newer.}}

```AppleScript
reverseString("Hello World!")

on reverseString(str)
    reverse of characters of str as string
end reverseString
```



Or, if we want a polymorphic '''reverse()''' for both strings and lists, we can define it either in terms of a generic fold/reduce, or using the built-in method for lists:


```AppleScript
-- Using either a generic foldr(f, a, xs)

-- reverse1 :: [a] -> [a]
on reverse1(xs)
    script rev
        on |λ|(a, x)
            a & x
        end |λ|
    end script

    if class of xs is text then
        foldr(rev, {}, xs) as text
    else
        foldr(rev, {}, xs)
    end if
end reverse1

-- or the built-in reverse method for lists

-- reverse2 :: [a] -> [a]
on reverse2(xs)
    if class of xs is text then
        (reverse of characters of xs) as text
    else
        reverse of xs
    end if
end reverse2


-- TESTING reverse1 and reverse2 with same string and list ---------------------------------------------------------------------------
on run
    script test
        on |λ|(f)
            map(f, ["Hello there !", {1, 2, 3, 4, 5}])
        end |λ|
    end script

    map(test, [reverse1, reverse2])
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------------------------

-- foldr :: (a -> b -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldr

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

```AppleScript
{{"! ereht olleH", {5, 4, 3, 2, 1}},
 {"! ereht olleH", {5, 4, 3, 2, 1}}}
```



## Applesoft BASIC


```ApplesoftBasic
10 A$ = "THE FIVE BOXING WIZARDS JUMP QUICKLY"
20 GOSUB 100REVERSE
30 PRINT R$
40 END

100 REMREVERSE A$
110 R$ = ""
120 FOR I = 1 TO LEN(A$)
130    R$ = MID$(A$, I, 1) + R$
140 NEXT I
150 RETURN
```



## Arturo



```arturo
str "Hello World"

print $(reverse str)
```


{{out}}


```txt
dlroW olleH
```



## ATS


```ATS

//
// How to compile:
// patscc -DATS_MEMALLOC_LIBC -o string_reverse string_reverse.dats
//

#include
"share/atspre_staload.hats"

fun
string_reverse
(
  x: string
) : Strptr1 = let
//
val [n:int] x = g1ofg0(x)
val y = string1_copy(x)
val n = string1_length(x)
val (pf, fpf | p) =
  $UNSAFE.ptr_vtake{array(char,n)}(ptrcast(y))
val () = array_subreverse(!p, i2sz(0), n)
prval () = fpf(pf)
//
in
  $UNSAFE.castvwtp0{Strptr1}(y)
end (* end of [string_reverse] *)

(* ****** ****** *)

implement
main0 () = let
//
val rev = string_reverse("asdf")
val ((*void*)) = println! ("reverse(\"asdf\") = \"", rev, "\"")
val ((*freed*)) = strptr_free (rev)
//
in
  // nothing
end // end of [main0]

```



## AutoHotkey

; <nowiki>"Normal" version:</nowiki>

```AutoHotkey
MsgBox % reverse("asdf")

reverse(string)
{
  Loop, Parse, string
    reversed := A_LoopField . reversed
  Return reversed
}
```

; <nowiki>A ''much'' slower version:</nowiki>

```AHK
Reverse(String){ ; credit to Rseding91
   If (A_IsUnicode){
      SLen := StrLen(String) * 2
      VarSetCapacity(RString,SLen)

      Loop,Parse,String
         NumPut(Asc(A_LoopField),RString,SLen-(A_Index * 2),"UShort")
   } Else {
      SLen := StrLen(String)
      VarSetCapacity(RString,SLen)

      Loop,Parse,String
         NumPut(Asc(A_LoopField),RString,SLen-A_Index,"UChar")
   }

   VarSetCapacity(RString,-1)

   Return RString
}
```



## AutoIt


```AutoIt
#AutoIt Version: 3.2.10.0
$mystring="asdf"
$reverse_string = ""
$string_length = StringLen($mystring)

For $i = 1 to $string_length
   $last_n_chrs = StringRight($mystring, $i)
   $nth_chr = StringTrimRight($last_n_chrs, $i-1)
   $reverse_string= $reverse_string & $nth_chr
Next

MsgBox(0, "Reversed string is:", $reverse_string)
```



## AWK


```awk
function reverse(s)
{
  p = ""
  for(i=length(s); i > 0; i--) { p = p substr(s, i, 1) }
  return p
}

BEGIN {
  print reverse("edoCattesoR")
}
```


;Recursive

```awk
function reverse(s   ,l)
{
  l = length(s)
  return l < 2 ? s:( substr(s,l,1) reverse(substr(s,1,l-1)) )
}

BEGIN {
  print reverse("edoCattesoR")
}
```


;using split, then joining in front:

```awk
# Usage: awk -f reverse.awk -v s=Rosetta

function rev(s,   i,len,a,r) {
   len = split(s, a, "")
  #for (i in a) r = a[i] r	# may not work - order is not guaranteed !
   for (i=1; i<=len; i++) r = a[i] r
   return r
}
BEGIN {
   if(!s) s = "Hello, world!"
   print s, "<-->", rev(s)
}

```

{{out}}
  Rosetta <--> attesoR


## Babel

This example will handle UTF-8 encoded Unicode but doesn't handle combining characters.

```babel
strrev: { str2ar ar2ls reverse ls2lf ar2str }
```

*str2ar - this operator converts a UTF-8 encoded string to an array of Unicode codepoints
*ar2ls - this operator converts the array to a linked-list
*reverse - this operator reverses a linked-list
*ls2lf - this operator undoes the effect of ar2ls
*ar2str - this operator undoes the effect of str2ar


## BaCon


```freebasic
OPTION UTF8 TRUE
s$ = "asdf"
PRINT REVERSE$(s$)
```


Unicode preservation works in BaCon 3.6 and higher.


## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
function reverse$(a$)
   b$ = ""
   for i = 1 to len(a$)
      b$ = mid$(a$, i, 1) + b$
   next i
   reverse$ = b$
end function
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 INPUT PROMPT "String: ":TX$
120 LET REV$=""
130 FOR I=LEN(TX$) TO 1 STEP-1
140   LET REV$=REV$&TX$(I)
150 NEXT
160 PRINT REV$
```


=
## Sinclair ZX81 BASIC
=

```basic
10 INPUT S$
20 LET T$=""
30 FOR I=LEN S$ TO 1 STEP -1
40 LET T$=T$+S$(I)
50 NEXT I
60 PRINT T$
```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion
call :reverse %1 res
echo %res%
goto :eof

:reverse
set str=%~1
set cnt=0
:loop
if "%str%" equ "" (
	goto :eof
	)
set chr=!str:~0,1!
set str=%str:~1%
set %2=%chr%!%2!
goto loop
```



## BBC BASIC


```bbcbasic
      PRINT FNreverse("The five boxing wizards jump quickly")
      END

      DEF FNreverse(A$)
      LOCAL B$, C%
      FOR C% = LEN(A$) TO 1 STEP -1
        B$ += MID$(A$,C%,1)
      NEXT
      = B$
```



## Befunge

Reads a line from stdin and write the reverse to stdout. Can be made to repeat indefinitely by removing the final <tt>@</tt> command.


```befunge
55+~>:48>*#8\#4`#:!#<#~_$>:#,_@
```



## Bracmat


```bracmat
  ( reverse
  = L x
    .     :?L
        & @( !arg
           :   ?
               ( %?x
               & utf$!x
               & !x !L:?L
               & ~`
               )
               ?
           )
      | str$!L
  )
& out$reverse$Ελληνικά
```

{{out}}

```txt
άκινηλλΕ
```


=={{header|Brainfuck}}==

```bf
[-]>,+[->,+]<[.<]
```

The former wont stop taking input bytes unless a special compiler was made to stop at ENTER.
The following checks for 10 ascii (line feed) and stops taking input at that point

```bf
,----- ----- [+++++ +++++ > , ----- -----]	If a newline is hit counter will be zero and input loop ends
<[.<]	run all chars backwards and print them

just because it looks good we print CRLF
+++++ +++++ +++ . --- .
```



## Brat


```brat
p "olleh".reverse  #Prints "hello"
```



## Burlesque



```burlesque

"Hello, world!"<-

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <wchar.h>

const char *sa = "abcdef";
const char *su = "as⃝df̅"; /* Should be in your native locale encoding. Mine is UTF-8 */

int is_comb(wchar_t c)
{
	if (c >= 0x300 && c <= 0x36f) return 1;
	if (c >= 0x1dc0 && c <= 0x1dff) return 1;
	if (c >= 0x20d0 && c <= 0x20ff) return 1;
	if (c >= 0xfe20 && c <= 0xfe2f) return 1;
	return 0;
}

wchar_t* mb_to_wchar(const char *s)
{
	wchar_t *u;
	size_t len = mbstowcs(0, s, 0) + 1;
	if (!len) return 0;

	u = malloc(sizeof(wchar_t) * len);
	mbstowcs(u, s, len);
	return u;
}

wchar_t* ws_reverse(const wchar_t* u)
{
	size_t len, i, j;
	wchar_t *out;
	for (len = 0; u[len]; len++);
	out = malloc(sizeof(wchar_t) * (len + 1));
	out[len] = 0;
	j = 0;
	while (len) {
		for (i = len - 1; i && is_comb(u[i]); i--);
		wcsncpy(out + j, u + i, len - i);
		j += len - i;
		len = i;
	}
	return out;
}

char *mb_reverse(const char *in)
{
	size_t len;
	char *out;
	wchar_t *u = mb_to_wchar(in);
	wchar_t *r = ws_reverse(u);
	len = wcstombs(0, r, 0) + 1;
	out = malloc(len);
	wcstombs(out, r, len);
	free(u);
	free(r);
	return out;
}

int main(void)
{
	setlocale(LC_CTYPE, "");

	printf("%s => %s\n", sa, mb_reverse(sa));
	printf("%s => %s\n", su, mb_reverse(su));
	return 0;
}
```

{{out}}

```txt
abcdef => fedcba
as⃝df̅ => f̅ds⃝a
```


{{libheader|GLib}}

```c
#include <glib.h>

gchar *srev (const gchar *s) {
    if (g_utf8_validate(s,-1,NULL)) {
        return g_utf8_strreverse (s,-1);
}   }
// main
int main (void) {
    const gchar *t="asdf";
    const gchar *u="as⃝df̅";
    printf ("%s\n",srev(t));
    printf ("%s\n",srev(u));
    return 0;
}
```



## C++


```cpp
#include <iostream>
#include <string>
#include <algorithm>

int main()
{
  std::string s;
  std::getline(std::cin, s);
  std::reverse(s.begin(), s.end()); // modifies s
  std::cout << s << std::endl;
  return 0;
}
```



## C sharp

C# does not have a built-in Reverse method for strings, and cannot reverse them in place because they are immutable. One way to implement this is to convert the string to an array of characters, reverse that, and return a new string from the reversed array:

```csharp
static string ReverseString(string input)
{
    char[] inputChars = input.ToCharArray();
    Array.Reverse(inputChars);
    return new string(inputChars);
}
```


As of .Net 3.5 the LINQ-to-objects allows the Reverse() extension method to be called on a string, since String implements the IEnumerable<char> interface. Because of this, the return type of Reverse is IEnumerable<char>. Fortunately, LINQ also provides the ToArray extension method, which can be used in conjunction with the constructor of string that accepts a char array:

```csharp
using System.Linq;

// ...

return new string(input.Reverse().ToArray());

// ...
```


'''Version supporting combining characters:'''

System.Globalization.StringInfo provides a means of separating a string into individual graphemes.

```csharp
public string ReverseElements(string s)
{
    // In .NET, a text element is series of code units that is displayed as one character, and so reversing the text
    // elements of the string correctly handles combining character sequences and surrogate pairs.
    var elements = System.Globalization.StringInfo.GetTextElementEnumerator(s);
    return string.Concat(AsEnumerable(elements).OfType<string>().Reverse());
}

// Wraps an IEnumerator, allowing it to be used as an IEnumerable.
public IEnumerable AsEnumerable(IEnumerator enumerator)
{
    while (enumerator.MoveNext())
        yield return enumerator.Current;
}
```


=={{header|Caché ObjectScript}}==


```txt
USER>Write $Reverse("Hello, World")

dlroW ,olleH
```




## Ceylon


```Ceylon

shared void run() {

   while(true) {
      process.write("> ");
      String? text = process.readLine();
      if (is String text) {
      print(text.reversed);
      }
      else {
      break;
      }
    }
}

```



## Clipper

Works with versions since 5, because ''LOCAL'' variables and the ''+='' operator was not implemented before.

```Clipper
FUNCTION Reverse(sIn)
   LOCAL sOut := "", i
   FOR i := Len(sIn) TO 1 STEP -1
      sOut += Substr(sIn, i, 1)
   NEXT
RETURN sOut
```



## Clojure


###  Basic reverse

For normal strings, the reverse function can be used to do the bulk of the work. However, it returns a character sequence, which has to be converted back to a string.

```lisp
(defn str-reverse [s] (apply str (reverse s)))
```


###  Reverse words in a string


```lisp
(apply str (interpose " " (reverse (.split "the quick brown fox" " "))))
```


###  Supporting combining characters

Handling combining characters present a trickier task. We need to protect the relative ordering of the combining character and the character to its left. Thus, before reversing, the characters need to be grouped.

```lisp
(defn combining? [c]
  (let [type (Character/getType c)]
    ;; currently hardcoded to the types taken from the sample string
    (or (= type 6) (= type 7))))

(defn group
  "Group normal characters with their combining characters"
  [chars]
  (cond (empty? chars) chars
	(empty? (next chars)) (list chars)
	:else
	(let [dres (group (next chars))]
	  (cond (combining? (second chars)) (cons (cons (first chars)
							(first dres))
						  (rest dres))
		:else (cons (list (first chars)) dres)))))

(defn str-reverse
  "Unicode-safe string reverse"
  [s]
  (apply str (apply concat (reverse (group s)))))
```

{{out}}

```txt

user=> s
"as⃝df̅"
user=> (str-reverse s)
"f̅ds⃝a"[
user=> (str-reverse (str-reverse s))
"as⃝df̅"
user=>

```



## COBOL


```cobol
FUNCTION REVERSE('QWERTY')
```



## CoffeeScript


```javascript
"qwerty".split("").reverse().join ""
```



## ColdFusion

You can reverse anything that can be written to the document in hashmarks (i.e. strings, numbers, now( ), etc.).

```cfm
<cfset myString  = "asdf" />
<cfset myString  = reverse( myString ) />
```



## Common Lisp


```lisp
(reverse my-string)
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE BbtReverseString;
IMPORT StdLog;

PROCEDURE ReverseStr(str: ARRAY OF CHAR): POINTER TO ARRAY OF CHAR;
VAR
	top,middle,i: INTEGER;
	c: CHAR;
	rStr: POINTER TO ARRAY OF CHAR;
BEGIN
	NEW(rStr,LEN(str$) + 1);
	top := LEN(str$) - 1; middle := (top - 1) DIV 2;
	FOR i := 0 TO middle DO
		rStr[i] := str[top - i];
		rStr[top - i] := str[i];
	END;
	IF ODD(LEN(str$)) THEN rStr[middle + 1] := str[middle + 1] END;
	RETURN rStr;
END ReverseStr;

PROCEDURE Do*;
VAR
	x: CHAR;
BEGIN
	StdLog.String("'asdf' reversed:> ");StdLog.String(ReverseStr("asdf"));StdLog.Ln
END Do;
END BbtReverseString.

```

Execute: ^Q BbtReverseString.Do<br/>
{{Out}}

```txt

'asdf' reversed:> fdsa

```



## Crystal


```ruby
# version 0.21.1

strings = ["asdf", "as⃝df̅"]
strings.each do |s|
  puts "#{s} -> #{s.reverse}"
end
```


{{out}}

```txt

asdf -> fdsa
as⃝df̅ -> f̅ds⃝a

```



## D


```d
void main() {
	import std.range, std.conv;

	string s1 = "hello"; // UTF-8
	assert(s1.retro.text == "olleh");

	wstring s2 = "hello"w; // UTF-16
	assert(s2.retro.wtext == "olleh"w);

	dstring s3 = "hello"d; // UTF-32
	assert(s3.retro.dtext == "olleh"d);

	// without using std.range:
	dstring s4 = "hello"d;
	assert(s4.dup.reverse == "olleh"d); // simple but inefficient (copies first, then reverses)
}
```



## Dc

Reversing "Hello world!" which is "22405534230753963835153736737" in Dc's numerical string representaion.

Due to using "~" this example needs GNU Dc or OpenBSD Dc.

```dc
22405534230753963835153736737 [ 256 ~ d SS 0<F LS SR 1+ ] d sF x 1 - [ 1 - d 0<F 256 * LR + ] d sF x P
```


```txt

!dlrow olleH

```



## Dart

Since Dart strings are sequences of [http://en.wikipedia.org/wiki/UTF-16 UTF-16] code units, it would not be sufficient to simply reverse the characters in strings, as this would not work with UTF-16 [http://en.wikipedia.org/wiki/UTF-16#Code_points_U.2B10000_to_U.2B10FFFF surrogate pairs] (pairs of UTF-16 code units that represent single characters [http://en.wikipedia.org/wiki/Plane_(Unicode)#Supplementary_Multilingual_Plane outside the Unicode BMP]). However, Dart provides a method to convert strings to sequences of unicode code points (called "runes" in Dart), and these sequences can easily be reversed and used to create new strings, so a string reversal function can be written with a single line of Dart code:


```dart
String reverse(String s) => new String.fromCharCodes(s.runes.toList().reversed);
```


A more complete example with unit tests would look like this:


```dart
import 'package:unittest/unittest.dart';

String reverse(String s) => new String.fromCharCodes(s.runes.toList().reversed);

main() {
  group("Reverse a string -", () {
    test("Strings with ASCII characters are reversed correctly.", () {
      expect(reverse("hello, world"), equals("dlrow ,olleh"));
    });
    test("Strings with non-ASCII BMP characters are reversed correctly.", () {
      expect(reverse("\u4F60\u4EEC\u597D"), equals("\u597D\u4EEC\u4F60"));
    });
    test("Strings with non-BMP characters are reversed correctly.", () {
      expect(reverse("hello, \u{1F310}"), equals("\u{1F310} ,olleh"));
    });
  });
}
```



## Delphi


```Delphi
function ReverseString(const InString: string): string;
var
  i: integer;
begin
  for i := Length(InString) downto 1 do
    Result := Result + InString[i];
end;
```

You could also use this RTL function Introduced in Delphi 6:

```Delphi>StrUtils.ReverseString</lang

=={{header|Déjà Vu}}==

```dejavu
!print concat chars "Hello"
```

{{out}}

```txt
olleH
```



## DWScript

See [[Reverse a string#Delphi|Delphi]].


## Dyalect



```dyalect
const str = "asdf"

func String.reverse() {
    var cs = []
    const len = this.len();
    for n in 1..len {
        cs.add(this[len - n])
    }
    String(values: cs)
}

str.reverse()
```



## E

[[Category:E examples needing attention]] <!-- Replacing accum, grapheme clusters -->

```e
pragma.enable("accumulator")
def reverse(string) {
  return accum "" for i in (0..!(string.size())).descending() { _ + string[i] }
}
```



## EchoLisp


```lisp

(define (string-reverse string)
    (list->string (reverse (string->list string))))

(string-reverse "ghij")
    → jihg
(string-reverse "un roc lamina l animal cornu")
    → unroc lamina l animal cor nu

```



## Emacs Lisp


```lisp
(reverse "Hello World")
```

{{out}}

```txt

"dlroW olleH"

```



## Eiffel


```eiffel
class
    APPLICATION
create
    make
feature
    make
            -- Demonstrate string reversal.
        do
            my_string := "Hello World!"
            my_string.mirror
            print (my_string)
        end
    my_string: STRING
            -- Used for reversal
end
```

{{out}}

```txt

!dlroW olleH

```



## EGL


```EGL
function reverse( str string ) returns( string )
	result string;
	for ( i int from StrLib.characterLen( str ) to 1 decrement by 1 )
		result ::= str[i:i];
	end
	return( result );
end
```


## Ela


```ela
reverse_string str = rev len str
  where len = length str
        rev 0 str = ""
        rev n str = toString (str : nn) +> rev nn str
          where nn = n - 1

reverse_string "Hello"
```


{{out}}
```txt
"olleH"
```


Another approach is to covert a string to a list, reverse a list and then convert it back to a string:


```ela
open string
fromList <| reverse <| toList "Hello" ::: String
```



## Elena

ELENA 4.x:

```elena
import system'routines;
import extensions;
import extensions'text;

extension extension
{
    reversedLiteral()
        = self.toArray().sequenceReverse().summarize(new StringWriter());
}

public program()
{
    console.printLine("Hello World".reversedLiteral())
}
```

{{out}}

```txt

dlroW olleH

```



## Elixir

Elixir handles Unicode graphemes correctly by default.

```elixir

IO.puts (String.reverse "asdf")
IO.puts (String.reverse "as⃝df̅")

```

{{Out}}

```txt

fdsa
f̅ds⃝a

```


## Elm


```elm
-- The import on the next line provides the reverse string
-- functionality satisfying the rosettacode.org task description.
import String exposing (reverse)

-- The rest is fairly boilerplate code demonstrating
-- interactively that the reverse function works.
import Html exposing (Html, Attribute, text, div, input)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (on, targetValue)
import Html.App exposing (beginnerProgram)

main = beginnerProgram { model = "", view = view, update = update }

update newStr oldStr = newStr

view : String -> Html String
view forward =
  div []
    ([ input
        [ placeholder "Enter a string to be reversed."
        , value forward
        , on "input" targetValue
        , myStyle
        ]
        []
     ] ++
     [ let backward = reverse forward
       in div [ myStyle] [text backward]
     ])

myStyle : Attribute msg
myStyle =
  style
    [ ("width", "100%")
    , ("height", "20px")
    , ("padding", "5px 0 0 5px")
    , ("font-size", "1em")
    , ("text-align", "left")
    ]
```


Link to live demo: http://dc25.github.io/reverseStringElm/


## Erlang


```erlang>1
 lists:reverse("reverse!").
"!esrever"
```

Erlang also supports binary strings, which uses its binary format. There is no standard function to reverse a binary sequence, but the following one does the job well enough. It works by changing the endianness (from little to big or the opposite) of the whole sequence, effectively reversing the string.

```erlang
reverse(Bin) ->
    Size = size(Bin)*8,
    <<T:Size/integer-little>> = Bin,
    <<T:Size/integer-big>>.
```

{{out}}

```txt

1> test:reverse(<<"hello">>).
<<"olleh">>

```



## ERRE


```ERRE

PROGRAM REVERSE_STRING

PROCEDURE REVERSE(A$->R$)
   LOCAL I%
   R$=""
   FOR I=1 TO LEN(A$) DO
     R$=MID$(A$,I,1)+R$
   END FOR
END PROCEDURE

BEGIN
   A$="THE FIVE BOXING WIZARDS JUMP QUICKLY"
   REVERSE(A$->R$)
   PRINT(R$)
END PROGRAM

```



## Euler Math Toolbox



```Euler Math Toolbox

>function strrev (s) := chartostr(fliplr(strtochar(s)))
>strrev("This is a test!")
 !tset a si sihT

```



## Euphoria



```euphoria

include std/sequence.e
printf(1, "%s\n", {reverse("abcdef") })

```



## Ezhil


```Ezhil


## இந்த நிரல் தரப்படும் சரம் ஒன்றைத் தலைகீழாகத் திருப்பி அச்சிடும்
## உதாரணமாக "abc" என்ற சரம் தரப்பட்டால் அதனைத் திருப்பி "cba" என அச்சிடும்
## "எழில்" மொழியின்மூலம் இரண்டு வகைகளில் இதனைச் செய்யலாம். இரண்டு உதாரணங்களும் இங்கே தரப்பட்டுள்ளன

நிரல்பாகம் திருப்புக (சரம்1)

    ## முதல் வகை

    சரம்2 = ""

    @( சரம்1  இல் இ) ஒவ்வொன்றாக
          சரம்2 =  இ + சரம்2
        முடி

    பின்கொடு சரம்2

முடி

நிரல்பாகம் மீண்டும்திருப்புக (சரம்1)

    ## இரண்டாம் வகை

    சரநீளம் = len(சரம்1)

    சரம்2 = ""

    @(எண் = 0, எண் < சரநீளம், எண் = எண் + 1) ஆக

      சரம்2 = எடு(சரம்1, எண்) + சரம்2

    முடி

    பின்கொடு சரம்2

முடி


அ = உள்ளீடு("ஓர் எழுத்துச் சரத்தைத் தாருங்கள் ")

பதிப்பி "நீங்கள் தந்த எழுத்துச் சரம்" அ

பதிப்பி "அதனை முதல் வகையில் திருப்பியுள்ளோம்: " திருப்புக(அ)

பதிப்பி "வேறொரு வகையில் திருப்பியுள்ளோம்: " மீண்டும்திருப்புக(அ)


```



## FBSL

A slow way

```qbasic
Function StrRev1(ByVal $p1)
	dim $b = ""
	REPEAT len(p1)
		b = b & RIGHT(p1,1)
		p1 = LEFT(p1,LEN(p1)-1)
	END REPEAT
	return b
End Function

```


A much faster (twice at least) way

```qbasic
Function StrRev2(ByVal $p1)
	dim $b = "", %i
	for i = len(p1) DOWNTO 1
		b = b & MID(p1,i,1)
	next
	return b
End Function
```


An even faster way using PEEK, POKE, double-calls and quantity-in-hand

```qbasic
Function StrRev3( $s )
	FOR DIM x = 1 TO LEN(s) \ 2
		PEEK(@s + LEN - x, $1)
		POKE(@s + LEN - x, s{x})(@s + x - 1, PEEK)
	NEXT
	RETURN s
end function

```


An even faster way using the DynC (Dynamic C) mode

```c
DynC StringRev($theString) As String
   void rev(char *str)
   {
		int len = strlen(str);
		char *HEAD = str;
		char *TAIL = str + len - 1;
		char temp;
		int i;
		for ( i = 0; i <= len / 2; i++, HEAD++, TAIL--) {
			temp = *HEAD;
			*HEAD = *TAIL;
			*TAIL = temp;
		}
   }
   char *main(char* theString)
   {
      rev(theString);
      return theString;
   }
End DynC
```


Using DynASM, the Dynamic Assembler mode.

```asm
DYNASM RevStr(BYVAL s AS STRING) AS STRING
   // get length of string
   // divide by two
   // setup pointers to head and tail
   // iterate from 1 to (length \ 2)
   //   swap head with tail
   //   increment head pointer
   //   decrement tail pointer

   ENTER 0, 0 // = PUSH EBP: MOV EBP, ESP
   PUSH EBX // by Windows convention EBX, EDI, ESI must be saved before modification

   MOV EAX, s // get string pointer
   MOV ECX, EAX // duplicate it

   .WHILE BYTE PTR [ECX] <> 0

	INC ECX // propagate to tail

   .WEND

   MOV EDX, ECX // duplicate tail pointer
   DEC EDX // set it to last byte before trailing zero

   SUB ECX, EAX // get length in ECX in 1 CPU cycle
   SHR ECX, 1 // get length \ 2 in 1 CPU cycle; that's the beauty of power-of-two division

   .WHILE ECX > 0

      MOV BL, [EDX] // no need to XOR; just overwrite BL and BH contents
      MOV BH, [EAX] // DynAsm deduces data size from destination register sizes

      MOV [EDX], BH // ditto, source register sizes
      MOV [EAX], BL

      INC EAX // propagate pointers
      DEC EDX

      DEC ECX // decrement counter

   .WEND

   // point to start of string again
   MOV EAX, s // MOV = 1 CPU cycle, PUSH + POP = 2 CPU cycles

   POP EBX // by Windows convention ESI, EDI, EBX must be restored if modified
   LEAVE // = POP EBP
   RET
END DYNASM

```


=={{header|F_Sharp|F#}}==

### The function


```fsharp

// Reverse a string. Nigel Galloway: August 14th., 2019
let strRev α=let N=System.Globalization.StringInfo.GetTextElementEnumerator(α)
             List.unfold(fun n->if n then Some(N.GetTextElement(),N.MoveNext()) else None)(N.MoveNext())|>List.rev|>String.concat ""

```


### The Task

I was a little concerned when entering this task because in the edit window the overline appears above the d, but when previewed it is correctly above the f, using Firefox anyway. Using XTERM the output is correct with the s inside a circle but appears as sO in Firefox.

```fsharp

printfn "%s" (strRev "as⃝df̅")
printfn "%s" (strRev "Nigel")

```

{{out}}

```txt

f̅ds⃝a
legiN

```



## Factor

A string is a sequence and there is a default reverse implementation for those.

```factor
"hello" reverse
```

<code>string-reverse</code> preserves graphemes:

```factor
"as⃝df̅" string-reverse "f̅ds⃝a" = .
```



## FALSE

This solution does not take into account combination characters:

```false
1_
[^$1_=~][]#%
[$1_=~][,]#
```

This solution does take into account combination characters (except for half-marks):

```false
1_
[^$1_=~][
  $$767>\879\>&
  1ø$7615>\7620\>&|
  1ø$8399>\8428\>&|
  [\]?
]#%
[$1_=~][,]#
```



## Fancy


```fancy
"hello world!" reverse
```



## Forth



###  Method 1


```forth
: exchange ( a1 a2 -- )
  2dup c@ swap c@ rot c! swap c! ;
: reverse ( c-addr u -- )
  1- bounds begin 2dup > while
    2dup exchange
    -1 /string
  repeat 2drop ;

s" testing" 2dup reverse type   \ gnitset
```



###  Method 2 Using the stack


```forth
\ reverse a counted string using the stack
\ Method: Read the input string character by character onto the parameter stack
\         Then write the character back into the same string from the stack

create mystring ," ABCDEFGHIJKLMNOPQRSTUVWXYZ987654321"     \ this is a counted string

: pushstr ( str -- char[1].. char[n])    \ read the contents of STR onto the stack
            count bounds  do  I c@  loop ;

: popstr   ( char[1].. char[n] str -- )  \ read chars off stack into str
            count bounds  do  I c!  loop ;

: reverse ( str -- )         \ create the reverse function with the factored words
          dup >r             \ put a copy of the string addr on return stack
          pushstr            \ push the characters onto the parameter stack
          r> popstr ;        \ get back our copy of the string addr and pop the characters into it

\ test in the Forth console

```


Forth Console Output

```txt

mystring count type ABCDEFGHIJKLMNOPQRSTUVWXYZ987654321 ok
mystring dup reverse count type 123456789ZYXWVUTSRQPONMLKJIHGFEDCBA ok


```


=== Using the Forth-2012 Xchars wordset to handle multi-byte characters ===

Characters accessed with C@ C! are usually bytes and can therefore only represent characters in 8-bit encodings (e.g., Latin-1).  Forth-2012 added the Xchars wordset for dealing with multi-byte encodings such as UTF-8; actually these words are not needed much, because the magic of UTF-8 means that most byte-oriented code works as intended, but the present task is one of the few examples where that is not good enough.

The xchars wordset offers several ways to skin this cat; this is just one way to do it, not necessarily the best one.  Because the xchars wordset currently does not support recognizing combining characters, this code does not get extra credit.


```forth
: xreverse {: c-addr u -- c-addr2 u :}
    u allocate throw u + c-addr swap over u + >r begin ( from to r:end)
	over r@ u< while
	    over r@ over - x-size dup >r - 2dup r@ cmove
	    swap r> + swap repeat
    r> drop nip u ;

\ example use
s" ώщыē" xreverse type \ outputs "ēыщώ"
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
PROGRAM Example

  CHARACTER(80) :: str = "This is a string"
  CHARACTER :: temp
  INTEGER :: i, length

  WRITE (*,*) str
  length = LEN_TRIM(str) ! Ignores trailing blanks. Use LEN(str) to reverse those as well
  DO i = 1, length/2
     temp = str(i:i)
     str(i:i) = str(length+1-i:length+1-i)
     str(length+1-i:length+1-i) = temp
  END DO
  WRITE(*,*) str

END PROGRAM Example
```

{{out}}
 This is a string
 gnirts a si sihT
Another implementation that uses a recursive not-in-place algorithm:

```fortran
program reverse_string

  implicit none
  character (*), parameter :: string = 'no devil lived on'

  write (*, '(a)') string
  write (*, '(a)') reverse (string)

contains

  recursive function reverse (string) result (res)

    implicit none
    character (*), intent (in) :: string
    character (len (string)) :: res

    if (len (string) == 0) then
      res = ''
    else
      res = string (len (string) :) // reverse (string (: len (string) - 1))
    end if

  end function reverse

end program reverse_string
```

{{out}}

```txt
no devil lived on
no devil lived on
```


Another shorter implementation (adapted version from stackoverflow question 10605574 how-to-reverse-a-chain-of-character-fortran-90):

```fortran
 program reverse_string
  implicit none
  character (80) :: cadena
  integer :: k, n
  !
  cadena = "abcdefgh"
  n = len_trim (cadena)
  !
  write (*,*) cadena
  forall (k=1:n) cadena (k:k) = cadena (n-k+1:n-k+1)
  write (*,*) cadena
  !
end program reverse_string
```

{{out}}

```txt

abcdefgh
hgfedcba
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function ReverseString(s As Const String) As String
  If s = "" Then Return s
  Dim length As Integer = Len(s)
  Dim r As String = Space(length)
  For i As Integer = 0 To length - 1
     r[i] = s[length - 1 - i]
  Next
  Return r
End Function

Dim s As String = "asdf"
Print "'"; s; "' reversed is '"; ReverseString(s); "'"
```


{{out}}

```txt

'asdf' reversed is 'fdsa'

```



## Frink

The built-in <CODE>reverse</CODE> function reverses a string or the elements of a list.

Frink's built-in <CODE>reverse[''string'']</CODE> is quite smart and uses a grapheme-based algorithm to handle Unicode correctly.  That is, it preserves "user-perceived characters" that may consist of characters, combining accents, high-plane Unicode characters (that is, above U+FFFF,) surrogate pairs, ''etc.'' correctly.

Many languages will not work correctly with upper-plane Unicode characters because they are represented as Unicode "surrogate pairs" which are represented as two characters in a UTF-16 stream.

For example, the string "g\u0308o" represents a g with combining diaeresis, followed by the letter o. Or, in other words, "g̈o". Note that while there are three Unicode codepoints, only two "graphemes" are displayed.  Using Frink's smart "reverse" function preserves these combined graphemes.  A naive reverse would move the diaeresis over the o instead of the g.

```frink
println[reverse["abcdef"]]
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as Str31 str
dim as long  i

str = "123456789abcdefghijk"

print str
print

for i = str[0] to 1 step -1
print mid$( str, i, 1 );
next i

```


Output:

```txt

123456789abcdefghijk

kjihgfedcba987654321

```



## Futhark


Futhark has no real strings beyond a little bit of syntactic sugar, so this is the same as reversing an array.


```Futhark

fun main(s: []i32) = s[::-1]

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=e32989a1ffdc4428075ca6d4cb15dfa6 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String = "asdf"
Dim sOutput As String
Dim siCount As Short

For siCount = Len(sString) DownTo 1
  sOutput &= Mid(sString, siCount, 1)
Next

Print sOutput

End
```

Output:

```txt

fdsa

```



## GAP


```gap
Reversed("abcdef");
# "fedcba"
```



## Gema

Reverse each line in the input stream. Using built in function:

```gema>\L<U
=@reverse{$1}
```

Not using built in function (recursively apply substring to same rule):

```gema>\L<U1><U
=@{$2}$1
```



## Genie

Pretty sure the output capture fails the extra credit, but that may be more local setup and font installs rather than the glib functions used.


```genie
[indent=4]
/*
   Reverse a string, in Genie
   valac reverse.gs
*/

init
    utf8:string = "asdf"
    combining:string = "asdf̅"

    print utf8
    print utf8.reverse()

    print combining
    print combining.reverse()
```


{{out}}

```txt

prompt$ valac reverse.gs
prompt$ ./reverse
asdf
fdsa
as?df?
?fd?sa
```



## GFA Basic


<lang>
PRINT @reverse$("asdf")
'
FUNCTION reverse$(string$)
  LOCAL result$,i%
  result$=""
  FOR i%=1 TO LEN(string$)
    result$=MID$(string$,i%,1)+result$
  NEXT i%
  RETURN result$
ENDFUNC

```



## Go

Functions below assume UTF-8 encoding.  (The task mentions Unicode but does not specify an encoding.)  Strings in Go are not restricted to be UTF-8, but Go has good support for it and works with UTF-8 most natually.  As shown below, certain string conversions work in UTF-8 and the range clause over a string works in UTF-8.  Go also has a Unicode package in the standard library that makes easy work of recognizing combining characters for this task.

```go
package main

import (
    "fmt"
    "unicode"
    "unicode/utf8"
)

// no encoding
func reverseBytes(s string) string {
    r := make([]byte, len(s))
    for i := 0; i < len(s); i++ {
        r[i] = s[len(s)-1-i]
    }
    return string(r)
}

// reverseCodePoints interprets its argument as UTF-8 and ignores bytes
// that do not form valid UTF-8.  return value is UTF-8.
func reverseCodePoints(s string) string {
    r := make([]rune, len(s))
    start := len(s)
    for _, c := range s {
        // quietly skip invalid UTF-8
        if c != utf8.RuneError {
            start--
            r[start] = c
        }
    }
    return string(r[start:])
}

// reversePreservingCombiningCharacters interprets its argument as UTF-8
// and ignores bytes that do not form valid UTF-8.  return value is UTF-8.
func reversePreservingCombiningCharacters(s string) string {
    if s == "" {
        return ""
    }
    p := []rune(s)
    r := make([]rune, len(p))
    start := len(r)
    for i := 0; i < len(p); {
        // quietly skip invalid UTF-8
        if p[i] == utf8.RuneError {
            i++
            continue
        }
        j := i + 1
        for j < len(p) && (unicode.Is(unicode.Mn, p[j]) ||
            unicode.Is(unicode.Me, p[j]) || unicode.Is(unicode.Mc, p[j])) {
            j++
        }
        for k := j - 1; k >= i; k-- {
            start--
            r[start] = p[k]
        }
        i = j
    }
    return (string(r[start:]))
}

func main() {
    test("asdf")
    test("as⃝df̅")
}

func test(s string) {
    fmt.Println("\noriginal:      ", []byte(s), s)
    r := reverseBytes(s)
    fmt.Println("reversed bytes:", []byte(r), r)
    fmt.Println("original code points:", []rune(s), s)
    r = reverseCodePoints(s)
    fmt.Println("reversed code points:", []rune(r), r)
    r = reversePreservingCombiningCharacters(s)
    fmt.Println("combining characters:", []rune(r), r)
}
```

{{out}}

```txt

original:       [97 115 100 102] asdf
reversed bytes: [102 100 115 97] fdsa
original code points: [97 115 100 102] asdf
reversed code points: [102 100 115 97] fdsa
combining characters: [102 100 115 97] fdsa

original:       [97 115 226 131 157 100 102 204 133] as⃝df̅
reversed bytes: [133 204 102 100 157 131 226 115 97] ��fd���sa
original code points: [97 115 8413 100 102 773] as⃝df̅
reversed code points: [773 102 100 8413 115 97] ̅fd⃝sa
combining characters: [102 773 100 115 8413 97] f̅ds⃝a

```



## Groovy


### ==Solution:==


```groovy
println "Able was I, 'ere I saw Elba.".reverse()
```

{{out}}

```txt
.ablE was I ere' ,I saw elbA
```



### ==Extra Credit:==


```groovy
def string = "as⃝df̅"

List combiningBlocks = [
    Character.UnicodeBlock.COMBINING_DIACRITICAL_MARKS,
    Character.UnicodeBlock.COMBINING_DIACRITICAL_MARKS_SUPPLEMENT,
    Character.UnicodeBlock.COMBINING_HALF_MARKS,
    Character.UnicodeBlock.COMBINING_MARKS_FOR_SYMBOLS
]
List chars = string as List
chars[1..-1].eachWithIndex { ch, i ->
    if (Character.UnicodeBlock.of((char)ch) in combiningBlocks) {
        chars[i..(i+1)] = chars[(i+1)..i]
    }
}
println chars.reverse().join()
```

{{out}}

```txt
f̅ds⃝a
```



## Harbour


```visualfoxpro
FUNCTION Reverse( sIn )

   LOCAL cOut := "", i

   FOR i := Len( sIn ) TO 1 STEP -1
      cOut += Substr( sIn, i, 1 )
   NEXT

   RETURN cOut
```



## Haskell


```haskell
reverse = foldl (flip (:)) []
```

This function as defined in the Haskell Prelude.

Though variants using a helper function with an additional accumulator argument are more efficient, and are now used by default in GHC.List unless the USE_REPORT_PRELUDE key is set.

Perhaps, for example:

```haskell
accumulatingReverse :: [a] -> [a]
accumulatingReverse lst =
  let rev xs a = foldl (flip (:)) a xs
  in rev lst []
```



### Supporting combining characters


```haskell
import Data.Char (isMark)
import Data.List (groupBy)
myReverse = concat . reverse . groupBy (const isMark)
```

<code>groupBy (const isMark)</code> is an unusual way of splitting a string into its combined characters


## HicEst


```hicest
CHARACTER string = "Hello World", tmp

L = LEN( string )
DO i = 1, L/2
  tmp = string(i)
  string(i) = string(L-i+1)
  string(L-i+1) = tmp
ENDDO

WRITE(Messagebox, Name) string
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
s := \arglist[1] | "asdf"
write(s," <-> ", reverse(s))    # reverse is built-in
end
```



## Io


```io
"asdf" reverse
```



## J

Reverse (<tt>|.</tt>) reverses a list of items (of any shape or type).

```j
   |.'asdf'
fdsa
```

Extra credit:
First, a function to determine whether a Unicode character is a combining character:

```j
   ranges=.16b02ff 16b036f, 16b1dbf 16b1dff, 16b20cf 16b20ff, 16bfe1f 16bfe2f
   iscombining=. 2 | ranges&I.
```

Then we need to box groups of letters and combining characters, reverse, and unbox. The boxing function can be carried out easily with dyad cut, which uses the indices of the ones on the right as the starting points for groups of characters. For clarity, its inverse will be defined as raze, which simply runs together the items inside boxes of its argument.

```j
   split=. (<;.1~ -.@iscombining) :. ;
```


After this, the solution is just to reverse under the split transformation. This also takes place under J code to convert from Unicode to integers.

```j
   |.&.split&.(3 u: 7&u:) 'as⃝df̅'
f̅ds⃝a
```



## Java


```java
public static String reverseString(String s) {
    return new StringBuffer(s).reverse().toString();
}
```

{{works with|Java|1.5+}}

```java5
public static String reverseString(String s) {
    return new StringBuilder(s).reverse().toString();
}
```



## JavaScript



### ES5



```javascript
//using chained methods
function reverseStr(s) {
  return s.split('').reverse().join('');
}

//fast method using for loop
function reverseStr(s) {
  for (var i = s.length - 1, o = ''; i >= 0; o += s[i--]) { }
  return o;
}

//fast method using while loop (faster with long strings in some browsers when compared with for loop)
function reverseStr(s) {
  var i = s.length, o = '';
  while (i--) o += s[i];
  return o;
}
```



### ES6



```JavaScript
(() => {

    // .reduceRight() can be useful when reversals
    // are composed with some other process

    let reverse1 = s => Array.from(s)
        .reduceRight((a, x) => a + (x !== ' ' ? x : ' <- '), ''),

        // but ( join . reverse . split ) is faster for
        // simple string reversals in isolation

        reverse2 = s => s.split('').reverse().join('');


    return [reverse1, reverse2]
        .map(f => f("Some string to be reversed"));

})();
```


{{Out}}

```JavaScript
["desrever <- eb <- ot <- gnirts <- emoS", "desrever eb ot gnirts emoS"]
```



## jq

jq's explode/implode filters are based on codepoints, and therefore "reverse_string" as defined here will reverse the sequence of codepoints.
The topic of Unicode combining characters is a large one that is not touched on here.

```jq
def reverse_string: explode | reverse | implode;
```

'''Examples''':
 "nöel" | reverse_string  # => "leön"

 "as⃝df̅" | reverse_string  # => "̅fd⃝sa"


## Jsish

ECMAScript has no builtin string reversal, so split the characters into an array, reverse the array and join it back together.

Jsi only supports UTF-8 literals so far (in release 2.8), character by character manipulation routines of multibyte UTF-8 data will not be correct. No extra credit, ''yet''.

```javascript
var str = "Never odd or even";
puts(str);
puts(str.split('').reverse().join(''));
```

{{out}}

```txt
Never odd or even
neve ro ddo reveN
```



## Julia


```julia>julia
 reverse("hey")
"yeh"
```

The <code>reverse</code> function reverses codepoints ([https://github.com/JuliaLang/julia/issues/6165 because this is the right behavior] for the main application of string reversal: reversed string processing by external C libraries).  However, [https://github.com/JuliaLang/julia/pull/9261 starting in Julia 0.4], you can also reverse the graphemes if you want (i.e. to reverse "visual order" including combining characters etc.) by:

```julia>julia
 join(reverse(collect(graphemes("as⃝df̅"))))
"f̅ds⃝a"
```



## K

Monadic reverse (| ) verb reverses a string or list of any shape

```K

     |"asdf"
"fdsa"

     | 23 4 5 1
1 5 4 23

```



## Kotlin


```kotlin
fun main(args: Array<String>) {
    println("asdf".reversed())
}
```



## L++


```lisp
(include "string" "algorithm")
(main
  (decl std::string s)
  (std::getline std::cin s)
  (std::reverse (s.begin) (s.end))
  (prn s))
```



## LabVIEW

{{VI solution|LabVIEW_Reverse_a_string.png}}


## Lang5


```lang5
: flip "" split reverse "" join ;
"qwer asdf" flip .
```



## Lasso


```Lasso
local(input) = 'asdf'
#input->reverse
```


===Using Query Expression & Array===
More verbose than the string->reverse method, but this example illustrates different techniques to achieve the same result:
using string->values to iterate over a string in order, inserting at position 1, and joining the resulting array as a string.

```Lasso
local(input = 'asdf', output = array)
with i in #input->values
do #output->insertFirst(#i)
#output->join
```



## LC3 Assembly

A string is stored as a zero-terminated array of character codes. To reverse it, we first scan forwards until we find the end; we then move backwards again, copying each code into a block of memory we have reserved for the purpose; and finally, when we have got back to the beginning, we append a terminal zero to the new string we have created. We can then call <tt>PUTS</tt> to print it.

```lc3asm
        .ORIG      0x3000

        LEA        R1,STRING
        LEA        R2,GNIRTS
        LD         R3,MINUS1
        NOT        R5,R1
        ADD        R5,R5,1

SCAN    LDR        R4,R1,0
        BRZ        COPY
        ADD        R1,R1,1
        BRNZP      SCAN

COPY    ADD        R1,R1,R3
        ADD        R4,R1,R5
        BRN        COPIED
        LDR        R4,R1,0
        STR        R4,R2,0
        ADD        R2,R2,1
        BRNZP      COPY

COPIED  AND        R4,R4,0
        STR        R4,R2,0

        LEA        R0,GNIRTS
        PUTS

        HALT

MINUS1  .FILL      0xFFFF

STRING  .STRINGZ   "If thou beest he -- but O how fall'n! how chang'd"
GNIRTS  .BLKW      128

        .END
```

{{out}}

```txt
d'gnahc woh !n'llaf woh O tub -- eh tseeb uoht fI
```



## LFE


Ordinary string:

```lisp

> (lists:reverse "asdf")
"fdsa"

```


Create a UTF-8 encoded string:

```lisp

> (set encoded (binary ("åäö ð" utf8)))
#B(195 165 195 164 195 182 32 195 176)

```


Display it, to be sure:

```lisp

> (io:format "~tp~n" (list encoded))
<<"åäö ð"/utf8>>

```


Reverse it:

```lisp

> (lists:reverse (unicode:characters_to_list encoded))
"ð öäå"

```




## Liberty BASIC


```lb
input$ ="abcdefgABCDEFG012345"
print input$
print ReversedStr$( input$)

end

function ReversedStr$(in$)
    for i =len(in$) to 1 step -1
    ReversedStr$ =ReversedStr$ +mid$( in$, i, 1)
    next i
end function
```



## Lingo

Lingo strings are always UTF-8 encoded and string operations are based on Unicode code points, so the "extra credit" is built-in:

```lingo
on reverse (str)
  res = ""
  repeat with i = str.length down to 1
    put str.char[i] after res
  end repeat
  return res
end
```

To reverse a string byte-wise, the ByteArray data type has to be used:

```lingo
on reverseBytes (str)
  ba = byteArray(str)
  res = byteArray()
  repeat with i = ba.length down to 1
    res[res.length+1] = ba[i]
  end repeat
  return res
end
```



## LiveCode


```livecode
function reverseString S
   repeat with i = length(S) down to 1
      put char i of S after R
   end repeat
   return R
end reverseString
```



## LLVM


```llvm
; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; Additional comments have been inserted, as well as changes made from the output produced by clang such as putting more meaningful labels for the jumps

$"main.printf" = comdat any

@main.str = private unnamed_addr constant [12 x i8] c"Hello world\00", align 1
@"main.printf" = linkonce_odr unnamed_addr constant [4 x i8] c"%s\0A\00", comdat, align 1

define void @reverse(i64, i8*) {
  %3 = alloca i8*, align 8          ; allocate str (local)
  %4 = alloca i64, align 8          ; allocate len (local)
  %5 = alloca i64, align 8          ; allocate i
  %6 = alloca i64, align 8          ; allocate j
  %7 = alloca i8, align 1           ; allocate t
  store i8* %1, i8** %3, align 8    ; set str (local) to the parameter str
  store i64 %0, i64* %4, align 8    ; set len (local) to the paremeter len
  store i64 0, i64* %5, align 8     ; i = 0
  %8 = load i64, i64* %4, align 8   ; load len
  %9 = sub i64 %8, 1                ; decrement len
  store i64 %9, i64* %6, align 8    ; j =
  br label %loop

loop:
  %10 = load i64, i64* %5, align 8  ; load i
  %11 = load i64, i64* %6, align 8  ; load j
  %12 = icmp ult i64 %10, %11       ; i < j
  br i1 %12, label %loop_body, label %exit

loop_body:
  %13 = load i8*, i8** %3, align 8                  ; load str
  %14 = load i64, i64* %5, align 8                  ; load i
  %15 = getelementptr inbounds i8, i8* %13, i64 %14 ; address of str[i]
  %16 = load i8, i8* %15, align 1                   ; load str[i]
  store i8 %16, i8* %7, align 1                     ; t = str[i]
  %17 = load i64, i64* %6, align 8                  ; load j
  %18 = getelementptr inbounds i8, i8* %13, i64 %17 ; address of str[j]
  %19 = load i8, i8* %18, align 1                   ; load str[j]
  %20 = getelementptr inbounds i8, i8* %13, i64 %14 ; address of str[i]
  store i8 %19, i8* %20, align 1                    ; str[i] = str[j]
  %21 = load i8, i8* %7, align 1                    ; load t
  %22 = getelementptr inbounds i8, i8* %13, i64 %17 ; address of str[j]
  store i8 %21, i8* %22, align 1                    ; str[j] = t

;-- loop increment
  %23 = load i64, i64* %5, align 8  ; load i
  %24 = add i64 %23, 1              ; increment i
  store i64 %24, i64* %5, align 8   ; store i
  %25 = load i64, i64* %6, align 8  ; load j
  %26 = add i64 %25, -1             ; decrement j
  store i64 %26, i64* %6, align 8   ; store j
  br label %loop

exit:
  ret void
}

define i32 @main() {
;-- char str[]
  %1 = alloca [12 x i8], align 1
;-- memcpy(str, "Hello world")
  %2 = bitcast [12 x i8]* %1 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %2, i8* getelementptr inbounds ([12 x i8], [12 x i8]* @main.str, i32 0, i32 0), i64 12, i32 1, i1 false)
;-- printf("%s\n", str)
  %3 = getelementptr inbounds [12 x i8], [12 x i8]* %1, i32 0, i32 0
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @"main.printf", i32 0, i32 0), i8* %3)
;-- %7 = strlen(str)
  %5 = getelementptr inbounds [12 x i8], [12 x i8]* %1, i32 0, i32 0
  %6 = call i64 @strlen(i8* %5)
;-- reverse(%6, str)
  call void @reverse(i64 %6, i8* %5)
;-- printf("%s\n", str)
  %7 = getelementptr inbounds [12 x i8], [12 x i8]* %1, i32 0, i32 0
  %8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @"main.printf", i32 0, i32 0), i8* %7)
;-- end of main
  ret i32 0
}

;--- The declaration for the external C printf function.
declare i32 @printf(i8*, ...)

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i32, i1)

declare i64 @strlen(i8*)
```

{{out}}

```txt
Hello world
dlrow olleH
```



## Logo

REVERSE works on both words and lists.

```logo
print reverse "cat   ; tac
```



## Lua

Built-in string.reverse(s) or s:reverse().

```lua
theString = theString:reverse()
```


## M2000 Interpreter


### Using Custom Function

Version 2, using insert to string (with no copies of strings)

```M2000 Interpreter

Module ReverseString {
      a$="as⃝df̅"
      Print Len(a$), len.disp(a$)
      Let i=1, j=Len(a$)
      z$=String$(" ",j)
      j++
      do {
            k$=mid$(a$, i, 1)
            if i<len(a$) then {
                  while len.disp(k$+mid$(a$, i+1,1)) =len.disp(k$)  {
                        k$+=mid$(a$, i+1,1)
                        i++
                        if i>len(a$) then exit
                        j--
                  }
                  j--
                  insert j, len(k$) Z$=K$
            } else j-- :Insert j,1 z$=k$
            if i>=len(a$) then exit
            i++
      } Always
      Print len(z$), len.disp(z$)
      Print z$="f̅ds⃝a"
      Print z$
}
ReverseString

```


===using StrRev$()===
this function (new to 9.5 version) use StrReverse from Vb6

```M2000 Interpreter

a$="as⃝df̅"
b$=strrev$(a$)
clipboard b$
Print b$="̅fd⃝sa"

```



## M4


```m4
define(`invert',`ifelse(len(`$1'),0,,`invert(substr(`$1',1))'`'substr(`$1',0,1))')
```



## Maclisp


```lisp
(readlist (reverse (explode "my-string")))
```

Output:

```txt
"gnirts-ym"
```



## Maple


```Maple>
 StringTools:-Reverse( "foo" );
                                 "oof"
```



## Mathematica


```mathematica
StringReverse["asdf"]
```



## MATLAB

A built-in function, "fliplr(string)" handles reversing a string of ASCII characters. Unicode is a whole other beast, if you need this functionality test to see if "fliplr()" properly handles the unicode characters you use. If it doesn't then you will need to code a function that is specific to your application.

Sample Usage:

```MATLAB>>
 fliplr(['She told me that she spoke English and I said great. '...
'Grabbed her hand out the club and I said let''s skate.'])

ans =

.etaks s'tel dias I dna bulc eht tuo dnah reh debbarG .taerg dias I dna hsilgnE ekops ehs taht em dlot ehS
```



## Maxima


```maxima
sreverse("abcdef");         /* "fedcba" */

sreverse("rats live on no evil star");   /* not a bug :o) */
```



## MAXScript


```maxscript
fn reverseString s =
(
    local reversed = ""
    for i in s.count to 1 by -1 do reversed += s[i]
    reversed
)
```



## min

{{works with|min|0.19.3}}

```min
("" split reverse "" join) :reverse-str
```



## MiniScript


```MiniScript
str = "This is a string"
print "Forward: " + str
newStr = ""
for i in range(str.len-1, 0)
    newStr = newStr + str[i]
end for
print "Reversed: " + newStr
```

{{out}}

```txt

Forward: This is a string
Reversed: gnirts a si sihT

```



## MIPS Assembly

This is heavily based off of the [http://rosettacode.org/wiki/Copy_a_string#MIPS_Assembly Copy String] solution. Only a few lines are changed. In the Copy String solution, the pointer at the source string starts at 0th then keeps adding until the loaded byte isn't 0. This instead when copying the string starts at the ''last'' index, then decrements the source pointer a number of times equal to the determined string length.


```mips

# First, it gets the length of the original string
# Then, it allocates memory from the copy
# Then it copies the pointer to the original string, and adds the strlen
#     subtract 1, then that new pointer is at the last char.
# while(strlen)
#     copy char
#     decrement strlen
#     decrement source pointer
#     increment target pointer

.data
	ex_msg_og: .asciiz "Original string:\n"
	ex_msg_cpy: .asciiz "\nCopied string:\n"
	string: .asciiz "Wow, what a string!"

.text
	main:
		la $v1,string #load addr of string into $v0
		la $t1,($v1)  #copy addr into $t0 for later access
		lb $a1,($v1)  #load byte from string addr
	strlen_loop:
		beqz $a1,alloc_mem
		addi $a0,$a0,1 #increment strlen_counter
		addi $v1,$v1,1 #increment ptr
		lb $a1,($v1)   #load the byte
		j strlen_loop

	alloc_mem:
		li $v0,9 #alloc memory, $a0 is arg for how many bytes to allocate
		         #result is stored in $v0
		syscall
		la $t0,($v0) #$v0 is static, $t0 is the moving ptr
		la $v1,($t1) #get a copy we can increment

		add $t1,$t1,$a0 #add strlen to our original, static addr to equal last char
		subi $t1,$t1,1  #previous operation is on NULL byte, i.e. off-by-one error.
		                #this corrects.
	copy_str:
		lb $a1,($t1) #copy first byte from source

	strcopy_loop:
		beq $a0,0,exit_procedure
		sb $a1,($t0)            #store the byte at the target pointer
		addi $t0,$t0,1          #increment target ptr
		subi $t1,$t1,1
		subi $a0,$a0,1
		lb $a1,($t1)            #load next byte from source ptr
		j strcopy_loop

	exit_procedure:
		la $a1,($v0) #store our string at $v0 so it doesn't get overwritten
		li $v0,4 #set syscall to PRINT

		la $a0,ex_msg_og  #PRINT("original string:")
		syscall

		la $a0,($v1)      #PRINT(original string)
		syscall

		la $a0,ex_msg_cpy #PRINT("copied string:")
		syscall

		la $a0,($a1)      #PRINT(strcopy)
		syscall

		li $v0,10         #EXIT(0)
		syscall

```



## Mirah


```mirah
def reverse(s:string)
    StringBuilder.new(s).reverse
end

puts reverse('reversed')
```


=={{header|Modula-2}}==

```modula2
MODULE ReverseStr;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT Write,WriteString,WriteLn,ReadChar;

PROCEDURE WriteInt(n : INTEGER);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%i", buf, n);
    WriteString(buf)
END WriteInt;

PROCEDURE ReverseStr(in : ARRAY OF CHAR; VAR out : ARRAY OF CHAR);
VAR ip,op : INTEGER;
BEGIN
    ip := 0;
    op := 0;
    WHILE in[ip] # 0C DO
        INC(ip)
    END;
    DEC(ip);
    WHILE ip>=0 DO
        out[op] := in[ip];
        INC(op);
        DEC(ip)
    END
END ReverseStr;

TYPE A = ARRAY[0..63] OF CHAR;
VAR is,os : A;
BEGIN
    is := "Hello World";
    ReverseStr(is, os);

    WriteString(is);
    WriteLn;
    WriteString(os);
    WriteLn;

    ReadChar
END ReverseStr.
```


=={{header|Modula-3}}==

```modula3
MODULE Reverse EXPORTS Main;

IMPORT IO, Text;

PROCEDURE String(item: TEXT): TEXT =
  VAR result: TEXT := "";
  BEGIN
    FOR i := Text.Length(item) - 1 TO 0 BY - 1 DO
      result := Text.Cat(result, Text.FromChar(Text.GetChar(item, i)));
    END;
    RETURN result;
  END String;

BEGIN
  IO.Put(String("Foobarbaz") & "\n");
END Reverse.
```

{{out}}

```txt

zabrabooF

```



## MUMPS


```MUMPS
REVERSE
 ;Take in a string and reverse it using the built in function $REVERSE
 NEW S
 READ:30 "Enter a string: ",S
 WRITE !,$REVERSE(S)
 QUIT
```

{{out}}

```txt

USER>D REVERSE^ROSETTA
Enter a string: Hello, World!
!dlroW ,olleH

```



## Neko

No extra credit for UTF in this example.

```actionscript
/* Reverse a string, in Neko */

var reverse = function(s) {
  var len = $ssize(s)
  if len < 2 return s

  var reverse = $smake(len)
  var pos = 0
  while len > 0 $sset(reverse, pos ++= 1, $sget(s, len -= 1))
  return reverse
}

var str = "never odd or even"
$print(str, "\n")
$print(reverse(str), "\n\n")

str = "abcdefghijklmnopqrstuvwxyz"
$print(str, "\n")
$print(reverse(str), "\n\n")

$print("single test\n")
str = "a"
$print(str, "\n")
$print(reverse(str), "\n\n")


$print("empty test\n")
str = ""
$print(str, "\n")
$print(reverse(str), "\n")
```

{{out}}

```txt
prompt$ nekoc reverse.neko
prompt$ neko reverse.n
never odd or even
neve ro ddo reven

abcdefghijklmnopqrstuvwxyz
zyxwvutsrqponmlkjihgfedcba

single test
a
a

empty test


```



## Nemerle


### Supporting Combining Characters

Compile with:
```txt
ncc -reference:System.Windows.Forms reverse.n
```


```Nemerle
using System;
using System.Globalization;
using System.Windows.Forms;
using System.Console;
using Nemerle.Utility.NString;

module StrReverse
{
    UReverse(text : string) : string
    {
        mutable output = [];
        def elements = StringInfo.GetTextElementEnumerator(text);
        while (elements.MoveNext())
            output ::= elements.GetTextElement().ToString();
        Concat("", output.Reverse());
    }

    Main() : void
    {
        def test = "as⃝df̅";
        MessageBox.Show($"$test --> $(UReverse(test))");  //for whatever reason my console didn't display Unicode properly, but a MessageBox worked
    }
}
```


### Basic Reverse

Doesn't require the '''System.Globalization''' namespace, probably a little less overhead.

```Nemerle
Reverse(text : string) : string
{
    mutable output = [];
    foreach (c in text.ToCharArray())
        output ::= c.ToString();
    Concat("", output)
}
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols nobinary

reverseThis = 'asdf'
sihTesrever = reverseThis.reverse

say reverseThis
say sihTesrever

return
```

{{out}}

```txt

asdf
fdsa

```



## NewLISP


```NewLISP
(reverse "!dlroW olleH")
```



## Nial


```nial
reverse 'asdf'
=fdsa
```



## Nim


```nim
import unicode

proc reverse(s: var string) =
  for i in 0 .. s.high div 2:
    swap(s[i], s[s.high - i])

proc reversed(s: string): string =
  result = newString(s.len)
  for i,c in s:
    result[s.high - i] = c

proc uniReversed(s: string): string =
  result = newStringOfCap(s.len)
  var tmp: seq[Rune] = @[]
  for r in runes(s):
    tmp.add(r)
  for i in countdown(tmp.high, 0):
    result.add(toUtf8(tmp[i]))

proc isComb(r: Rune): bool =
  (r >=% Rune(0x300) and r <=% Rune(0x36f)) or
    (r >=% Rune(0x1dc0) and r <=% Rune(0x1dff)) or
    (r >=% Rune(0x20d0) and r <=% Rune(0x20ff)) or
    (r >=% Rune(0xfe20) and r <=% Rune(0xfe2f))

proc uniReversedPreserving(s: string): string =
  result = newStringOfCap(s.len)
  var tmp: seq[Rune] = @[]
  for r in runes(s):
    if isComb(r): tmp.insert(r, tmp.high)
    else: tmp.add(r)
  for i in countdown(tmp.high, 0):
    result.add(toUtf8(tmp[i]))

for str in ["Reverse This!", "as⃝df̅"]:
  echo "Original string:       ", str
  echo "Reversed:              ", reversed(str)
  echo "UniReversed:           ", uniReversed(str)
  echo "UniReversedPreserving: ", uniReversedPreserving(str)
```

{{out}}

```txt
Original string:       Reverse This!
Reversed:              !sihT esreveR
UniReversed:           !sihT esreveR
UniReversedPreserving: !sihT esreveR
Original string:       as⃝df̅
Reversed:              Ìfdâsa
UniReversed:          ‾fd⃝sa
UniReversedPreserving: f̅ds⃝a
```


Since Nim 0.11.0, the ''unicode'' module provides a ''reversed'' proc...
Hence:


```nim
import unicode

doAssert "foobar".reversed == "raboof"
doAssert "先秦兩漢".reversed == "漢兩秦先"
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 STRING$="THIS TEXT IS REVERSED."
20 REVERSED$=""
30 FOR I=1 TO LEN(STRING$)
40 REVERSED$=MID$(STRING$,I,1)+REVERSED$
50 NEXT
60 PRINT REVERSED$
```



## Oberon

Tested with [https://miasap.se/obnc OBNC].

```Oberon
MODULE reverse;

   IMPORT Out, Strings;

   VAR s: ARRAY 12 + 1 OF CHAR;

   PROCEDURE Swap(VAR c, d: CHAR);
      VAR oldC: CHAR;
   BEGIN
      oldC := c; c := d; d := oldC
   END Swap;


   PROCEDURE Reverse(VAR s: ARRAY OF CHAR);
      VAR len, i: INTEGER;
   BEGIN
      len := Strings.Length(s);
      FOR i := 0 TO len DIV 2 DO
         Swap(s[i], s[len - 1 - i])
      END
   END Reverse;

BEGIN
   s := "hello, world";
   Reverse(s);
   Out.String(s);
   Out.Ln
END reverse.
```



## Objeck


```objeck

result := "asdf"->Reverse();

```


=={{header|Objective-C}}==
This extends the <code>NSString</code> object adding a <code>reverseString</code> class method.

```objc>#import <Foundation/Foundation.h


@interface NSString (Extended)
-(NSString *)reverseString;
@end

@implementation NSString (Extended)
-(NSString *) reverseString
{
    NSUInteger len = [self length];
    NSMutableString *rtr=[NSMutableString stringWithCapacity:len];
    //        unichar buf[1];

    while (len > (NSUInteger)0) {
        unichar uch = [self characterAtIndex:--len];
        [rtr appendString:[NSString stringWithCharacters:&uch length:1]];
    }
    return rtr;
}
@end
```

Usage example:

```objc
int main()
{
    @autoreleasepool {

        NSString *test = [@"!A string to be reverted!" reverseString];

        NSLog(@"%@", test);

    }
    return 0;
}
```


### Supporting combining characters

Extra credit

```objc>#import <Foundation/Foundation.h


@interface NSString (Extended)
-(NSString *)reverseString;
@end

@implementation NSString (Extended)
-(NSString *)reverseString
{
	NSInteger l = [self length] - 1;
	NSMutableString *ostr = [NSMutableString stringWithCapacity:[self length]];
	while (l >= 0)
	{
		NSRange range = [self rangeOfComposedCharacterSequenceAtIndex:l];
		[ostr appendString:[self substringWithRange:range]];
		l -= range.length;
	}
	return ostr;
}
@end
```

Usage example:

```objc
int main()
{
    @autoreleasepool {

        NSString *test = [@"as⃝df̅" reverseString];

        NSLog(@"%@", test);

    }
    return 0;
}
```



## OCaml


Since OCaml 4.02 we can use the handy [http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html#VALinit String.init] function.

Here a version that returns a new allocated string (preserving the original one):

{{works with|OCaml|4.02+}}

```ocaml
let string_rev s =
  let len = String.length s in
  String.init len (fun i -> s.[len - 1 - i])

let () =
  print_endline (string_rev "Hello world!")
```


for in place modification we can't use strings anymore because strings became immutable in ocaml 4.02, so the type bytes has to be used instead:


```ocaml
let rev_bytes bs =
  let last = Bytes.length bs - 1 in
  for i = 0 to last / 2 do
    let j = last - i in
    let c = Bytes.get bs i in
    Bytes.set bs i (Bytes.get bs j);
    Bytes.set bs j c;
  done

let () =
  let s = Bytes.of_string "Hello World" in
  rev_bytes s;
  print_bytes s;
  print_newline ();
;;
```


Here is a 100% functionnal string reversing function:

```ocaml
let rec revs_aux strin list index =
  if List.length list = String.length strin
  then String.concat "" list
  else revs_aux strin ((String.sub strin index 1)::list) (index+1)

let revs s = revs_aux s [] 0

let () =
  print_endline (revs "Hello  World!")
```


will return "!dlroW olleH"


## Octave


```octave
s = "a string";
rev = s(length(s):-1:1)
```



## Oforth


```Oforth>reverse</lang



## Ol


```scheme

(define (rev s)
   (runes->string (reverse (string->runes s))))

; testing:
(print (rev "as⃝df̅"))
; ==> ̅fd⃝sa

```



## OpenEdge/Progress

<lang Progress (OpenEdge ABL)>FUNCTION reverseString RETURNS CHARACTER (
   INPUT i_c AS CHARACTER
):

   DEFINE VARIABLE cresult AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE ii      AS INTEGER     NO-UNDO.

   DO ii = LENGTH( i_c ) TO 1 BY -1:
      cresult = cresult + SUBSTRING( i_c, ii, 1 ).
   END.
   RETURN cresult.

END FUNCTION.

MESSAGE reverseString( "asdf" ) VIEW-AS ALERT-BOX.
```



## OOC


```ooc

main: func {
  "asdf" reverse() println() // prints "fdsa"
}

```



## OxygenBasic


```oxygenbasic


'8 BIT CHARACTERS

string s="qwertyuiop"
sys a,b,i,j,le=len s
'
for i=1 to le
  j=le-i+1
  if j<=i then exit for
  a=asc s,i
  b=asc s,j
  mid s,j,chr a
  mid s,i,chr b
next
'

print s

'16 BIT CHARACTERS

wstring s="qwertyuiop"
sys a,b,i,j,le=len s
'
for i=1 to le
  j=le-i+1
  if j<=i then exit for
  a=unic s,i
  b=unic s,j
  mid s,j,wchr a
  mid s,i,wchr b
next
'
print s

```


==OxygenBasic x86 Assembler==
32 bit code, 8-bit characters:

```oxygenbasic


string s="qwertyuiop"
sys p=strptr s, le=len s
mov esi,p
mov edi,esi
add edi,le
dec edi
(
 cmp esi,edi
 jge exit
 mov al,[esi]
 mov ah,[edi]
 mov [esi],ah
 mov [edi],al
 inc esi
 dec edi
 repeat
)

print s

```



## Oz

Strings are lists. A function "Reverse" defined on lists is part of the implementation.

```oz
{System.showInfo {Reverse "!dlroW olleH"}}
```

An efficient (tail-recursive) implementation could look like this:

```oz
local
   fun {DoReverse Xs Ys}
      case Xs of nil then Ys
      [] X|Xr then {DoReverse Xr X|Ys}
      end
   end
in
   fun {Reverse Xs} {DoReverse Xs nil} end
end
```

Oz uses a single-byte encoding by default. If you decide to use a multi-byte encoding, Reverse will not work correctly.


## PARI/GP



### Version #1.


```parigp
reverse(s)=concat(Vecrev(s))
```



### Version #2.

{{Works with|PARI/GP|2.7.4 and above}}


```parigp

\\ Return reversed string str.
\\ 3/3/2016 aev
sreverse(str)={return(Strchr(Vecrev(Vecsmall(str))))}

{
\\ TEST1
print(" *** Testing sreverse from Version #2:");
print(sreverse("ABCDEF"));
my(s,sr,n=10000000);
s="ABCDEFGHIJKL";
for(i=1,n, sr=sreverse(s));
}

```


{{Output}}

```txt

 *** Testing sreverse from Version #2:
FEDCBA
(17:28) gp > ##
  ***   last result computed in 8,642 ms.

```



```parigp

\\ Version #1 upgraded to complete function. Practically the same.
reverse(str)={return(concat(Vecrev(str)))}

{
\\ TEST2
print(" *** Testing reverse from Version #1:");
print(reverse("ABCDEF"));
my(s,sr,n=10000000);
s="ABCDEFGHIJKL";
for(i=1,n, sr=reverse(s));
}

```


{{Output}}

```txt

 *** Testing reverse from Version #1:
FEDCBA
(17:31) gp > ##
  ***   last result computed in 11,814 ms.

```



## Pascal

The following examples handle correctly only single-byte encodings.

###  Standard Pascal

The following only works on implementations which implement Level 1 of standard Pascal (many popular compilers don't).

Standard Pascal doesn't have a separate string type, but uses arrays of char for strings. Note that Standard Pascal doesn't allow a return type of char array, therefore the destination array is passed through a var parameter (which is more efficient anyway).

```pascal
{ the result array must be at least as large as the original array }
procedure reverse(s: array[min .. max: integer] of char, var result: array[min1 .. max1: integer] of char);
 var
  i, len: integer;
 begin
  len := max-min+1;
  for i := 0 to len-1 do
   result[min1 + len-1 - i] := s[min + i]
 end;
```


```pascal
{Copy and paste it in your program}
function revstr(my_s:string):string;
    var out_s:string;
    ls,i:integer;
    begin
    ls:=length(my_s);
    for i:=1 to ls do
    out_s:=out_s+my_s[ls-i+1];
    revstr:=out_s;
    end;
```

=== Extended Pascal, Turbo Pascal, Delphi and compatible compilers ===

```pascal
function reverse(s:string):string;
var i:integer;
var tmp:char;
begin
    for i:=1 to length(s) div 2 do
      begin
       tmp:=s[i];
       s[i]:=s[length(s)+1-i];
       s[length(s)+1-i]:=tmp;
       reverse:=s;
      end;
end;
```

alternative as procedure which changes the original

```pascal
procedure revString(var s:string);
var
  i,j:integer;
  tmp:char;
begin
  i := 1;
  j := length(s);
  while i<j do
  begin
     tmp:=s[i];
     s[i]:=s[j];
     s[j]:=tmp;
     inc(i);
     dec(j)
  end;
end;
```



## Peloton

Padded out, variable length Chinese dialect

```sgml
<# 显示 指定 变量 反转顺序 字串>集装箱|猫坐在垫子</#>
```

This assigns the reverse of 'the cat sat on the mat' to the variable 'container' and displays the result which is
```txt
子垫在坐猫
```
 which Google Translate renders as
```txt
Sub-pad sitting cat
```
.

The same again but with everything in Korean.

```sgml
<# 보이십 할당하 변물건 열거꾸 문자그>컨테이너|고양이가 매트 위에 앉아</#>
```

Reversing the Korean makes an untranslatable-by-Google mess of the sentence, viz
```txt
아앉 에위 트매 가이양고
```
.

The short-opcode version in English dialect is

```sgml><@ SAYLETVARREVLIT
集装箱|猫坐在垫子</@>
```

Peloton works in Unicode.


## Perl


```perl
use utf8;
binmode STDOUT, ":utf8";

# to reverse characters (code points):
print reverse('visor'), "\n";

# to reverse graphemes:
print join("", reverse "José" =~ /\X/g), "\n";

$string = 'ℵΑΩ 駱駝道 🤔 🇸🇧 🇺🇸 🇬🇧‍ 👨‍👩‍👧‍👦🆗🗺';
print join("", reverse $string =~ /\X/g), "\n";
```

{{out}}

```txt
rosiv
ésoJ
🗺🆗👨‍👩‍👧‍👦 🇬🇧‍ 🇺🇸 🇸🇧 🤔 道駝駱 ΩΑℵ
```



## Perl 6

{{Works with|rakudo|2018.08}}
Perl 6 handles graphemes, multi-byte characters and emoji correctly by default.

```perl6
say "hello world".flip;
say "as⃝df̅".flip;
say 'ℵΑΩ 駱駝道 🤔 🇸🇧 🇺🇸 🇬🇧‍ 👨‍👩‍👧‍👦🆗🗺'.flip;
```

{{out}}

```txt
dlrow olleh
f̅ds⃝a
🗺🆗👨‍👩‍👧‍👦 🇬🇧‍ 🇺🇸 🇸🇧 🤔 道駝駱 ΩΑℵ
```



## Phix


```Phix
?reverse("asdf")
```

However that would go horribly wrong on utf8 strings, even without combining characters, so... this seems ok on "as\u203Ddf\u0305", as long as it is displayed in a message box rather than a Windows Console (even with chcp 65001 and Lucida Console, the characters do not combine).

```Phix
function unicode_reverse(string utf8)
sequence utf32 = utf8_to_utf32(utf8)
integer ch
    -- the assumption is made that <char><comb1><comb2>
    -- and <char><comb2><comb1> etc would work the same.
    for i=1 to length(utf32) do
        ch = utf32[i]
        if (ch>=0x300 and ch<=0x36f)
        or (ch>=0x1dc0 and ch<=0x1dff)
        or (ch>=0x20d0 and ch<=0x20ff)
        or (ch>=0xfe20 and ch<=0xfe2f) then
            utf32[i] = utf32[i-1]
            utf32[i-1] = ch
        end if
    end for
    utf32 = reverse(utf32)
    utf8 = utf32_to_utf8(utf32)
    return utf8
end function
```



## PHP


```php
strrev($string);
```

If you want Unicode support, you have to use some multibyte function. Sadly, PHP doesn't contain <code>mb_strrev()</code>. One of functions which support Unicode and is useful in this case is <code>preg_split()</code>.

```php
// Will split every Unicode character to array, reverse array and will convert it to string.
join('', array_reverse(preg_split('""u', $string, -1, PREG_SPLIT_NO_EMPTY)));
```



## PicoLisp


```PicoLisp
(pack (flip (chop "äöüÄÖÜß")))
```

{{out}}

```txt
-> "ßÜÖÄüöä"
```



## PL/I


```PL/I
s = reverse(s);
```



## Pop11


```pop11
define reverse_string(s);
    lvars i, l = length(s);
    for i from l by -1 to 1 do
        s(i);
    endfor;
    consstring(l);
enddefine;
```




## plainTeX

Works well if the string has no space (spaces are gobbled).


```tex
\def\gobtoA#1\revA{}\def\gobtoB#1\revB{}
\def\reverse#1{\reversei{}#1\revA\revB\revB\revB\revB\revB\revB\revB\revB\revA}
\def\reversei#1#2#3#4#5#6#7#8#9{\gobtoB#9\revend\revB\reversei{#9#8#7#6#5#4#3#2#1}}
\def\revend\revB\reversei#1#2\revA{\gobtoA#1}
\reverse{Rosetta}
\bye
```


Output:

```txt
attesoR
```




## PostScript

The following implementation works on arrays of numerics as well as characters ( string ).

```PostScript
/reverse{
/str exch def
/temp str 0 get def
/i 0 def
str length 2 idiv{
/temp str i get def
str i str str length i sub 1 sub get put
str str length i sub 1 sub temp put
/i i 1 add def
}repeat
str pstack
}def
```

{{Out}}

```PostScript
[1 2 3] reverse % input
[3 2 1]

(Hello World) reverse % input
(dlroW olleH)
```



## PowerBASIC


```powerbasic
#DIM ALL
#COMPILER PBCC 6

FUNCTION PBMAIN () AS LONG
CON.PRINT STRREVERSE$("PowerBASIC")
END FUNCTION
```



## PowerShell

Test string

```powershell
$s = "asdf"
```


### Array indexing

Creating a character array from the end to the string's start and join it together into a string again.
{{works with|PowerShell|1}}

```powershell
[string]::Join('', $s[$s.Length..0])
```

{{works with|PowerShell|2}}

```powershell
-join ($s[$s.Length..0])
```

{{works with|PowerShell|2}}

```powershell
[array]::Reverse($s)
```


### Regular expressions

Creating a regular expression substitution which captures every character of the string in a capture group and uses a reverse-ordered string of references to those to construct the reversed string.
{{works with|PowerShell|1}}

```powershell
$s -replace
      ('(.)' * $s.Length),
      [string]::Join('', ($s.Length..1 | ForEach-Object { "`$$_" }))
```

{{works with|PowerShell|2}}

```powershell
$s -replace
      ('(.)' * $s.Length),
      -join ($s.Length..1 | ForEach-Object { "`$$_" } )
```

{{works with|PowerShell|3}}

```PowerShell

[Regex]::Matches('abc','.','RightToLeft').Value -join ''

```

{{Out}}

```txt

cba

```



## Prolog

{{works with|SWI Prolog}}

```prolog
reverse("abcd", L), string_to_list(S,L).
```

{{Out}}

```txt

L = [100,99,98,97],
S = "dcba".
```


The main workings are hidden inside the reverse/2 predicate,
so lets write one to see how it works:

```prolog
accRev([H|T], A, R) :- accRev(T, [H|A], R).
accRev([], A, A).

rev(L,R) :- accRev(L,[],R).
```



## PureBasic


```PureBasic
Debug ReverseString("!dekrow tI")
```



## Python


### Optimized for user input


```python
input()[::-1]
```



### Already known string


```python
string[::-1]
```

or

```python
''.join(reversed(string))
```



### Python: Unicode reversal

(See [http://paddy3118.blogspot.com/2009/07/case-of-disappearing-over-bar.html this article] for more information from which this is improved)

'''Note:''' How this looks may be subject to how the tool you are using to view this page can render Unicode.

```python
import unicodedata

def ureverse(ustring):
    'Reverse a string including unicode combining characters'
    groupedchars = []
    uchar = list(ustring)
    while uchar:
        if unicodedata.combining(uchar[0]) != 0:
            groupedchars[-1] += uchar.pop(0)
        else:
            groupedchars.append(uchar.pop(0))
    # Grouped reversal
    groupedchars = groupedchars[::-1]

    return ''.join(groupedchars)

def say_string(s):
    return ' '.join([s, '=', ' | '.join(unicodedata.name(ch, '') for ch in s)])

def say_rev(s):
    print(f"Input:              {say_string(s)}")
    print(f"Character reversed: {say_string(s[::-1])}")
    print(f"Unicode reversed:   {say_string(ureverse(s))}")
    print(f"Unicode reverse²:   {say_string(ureverse(ureverse(s)))}")

if __name__ == '__main__':
    ucode = ''.join(chr(int(n[2:], 16)) for n in
                     'U+0041 U+030A U+0073 U+0074 U+0072 U+006F U+0308 U+006D'.split())
    say_rev(ucode)
```


{{out}}

```txt
Input:              Åström = LATIN CAPITAL LETTER A | COMBINING RING ABOVE | LATIN SMALL LETTER S | LATIN SMALL LETTER T | LATIN SMALL LETTER R | LATIN SMALL LETTER O | COMBINING DIAERESIS | LATIN SMALL LETTER M
Character reversed: m̈orts̊A = LATIN SMALL LETTER M | COMBINING DIAERESIS | LATIN SMALL LETTER O | LATIN SMALL LETTER R | LATIN SMALL LETTER T | LATIN SMALL LETTER S | COMBINING RING ABOVE | LATIN CAPITAL LETTER A
Unicode reversed:   mörtsÅ = LATIN SMALL LETTER M | LATIN SMALL LETTER O | COMBINING DIAERESIS | LATIN SMALL LETTER R | LATIN SMALL LETTER T | LATIN SMALL LETTER S | LATIN CAPITAL LETTER A | COMBINING RING ABOVE
Unicode reverse²:   Åström = LATIN CAPITAL LETTER A | COMBINING RING ABOVE | LATIN SMALL LETTER S | LATIN SMALL LETTER T | LATIN SMALL LETTER R | LATIN SMALL LETTER O | COMBINING DIAERESIS | LATIN SMALL LETTER M

```


If this code is then used:

```python
ucode = ''.join(chr(int(n[2:], 16)) for n in
                 'U+006B U+0301 U+0075 U+032D U+006F U+0304 U+0301 U+006E'.split())
say_rev(ucode)
```

It produces this output
{{out}}

```txt
Input:              ḱṷṓn = LATIN SMALL LETTER K | COMBINING ACUTE ACCENT | LATIN SMALL LETTER U | COMBINING CIRCUMFLEX ACCENT BELOW | LATIN SMALL LETTER O | COMBINING MACRON | COMBINING ACUTE ACCENT | LATIN SMALL LETTER N
Character reversed: ń̄o̭úk = LATIN SMALL LETTER N | COMBINING ACUTE ACCENT | COMBINING MACRON | LATIN SMALL LETTER O | COMBINING CIRCUMFLEX ACCENT BELOW | LATIN SMALL LETTER U | COMBINING ACUTE ACCENT | LATIN SMALL LETTER K
Unicode reversed:   nṓṷḱ = LATIN SMALL LETTER N | LATIN SMALL LETTER O | COMBINING MACRON | COMBINING ACUTE ACCENT | LATIN SMALL LETTER U | COMBINING CIRCUMFLEX ACCENT BELOW | LATIN SMALL LETTER K | COMBINING ACUTE ACCENT
Unicode reverse²:   ḱṷṓn = LATIN SMALL LETTER K | COMBINING ACUTE ACCENT | LATIN SMALL LETTER U | COMBINING CIRCUMFLEX ACCENT BELOW | LATIN SMALL LETTER O | COMBINING MACRON | COMBINING ACUTE ACCENT | LATIN SMALL LETTER N
```



'''This uses the unicode string mentioned in the task:'''

```python
ucode = ''.join(chr(int(n, 16))
                 for n in ['61', '73', '20dd', '64', '66', '305'])
say_rev(ucode)
```

It produces this output
{{out}}

```txt
Input:              as⃝df̅ = LATIN SMALL LETTER A | LATIN SMALL LETTER S | COMBINING ENCLOSING CIRCLE | LATIN SMALL LETTER D | LATIN SMALL LETTER F | COMBINING OVERLINE
Character reversed: ̅fd⃝sa = COMBINING OVERLINE | LATIN SMALL LETTER F | LATIN SMALL LETTER D | COMBINING ENCLOSING CIRCLE | LATIN SMALL LETTER S | LATIN SMALL LETTER A
Unicode reversed:   f̅d⃝sa = LATIN SMALL LETTER F | COMBINING OVERLINE | LATIN SMALL LETTER D | COMBINING ENCLOSING CIRCLE | LATIN SMALL LETTER S | LATIN SMALL LETTER A
Unicode reverse²:   as⃝df̅ = LATIN SMALL LETTER A | LATIN SMALL LETTER S | COMBINING ENCLOSING CIRCLE | LATIN SMALL LETTER D | LATIN SMALL LETTER F | COMBINING OVERLINE
```



## Qi

It's simplest just to use the common lisp REVERSE function.

```Qi
(REVERSE "ABCD")
```



## R

{{works with|R|2.8.1}}
The following code works with UTF-8 encoded strings too.

```R
revstring <- function(stringtorev) {
   return(
      paste(
           strsplit(stringtorev,"")[[1]][nchar(stringtorev):1]
           ,collapse="")
           )
}
```


Alternatively (using rev() function):


```R
 revstring <- function(s) paste(rev(strsplit(s,"")[[1]]),collapse="")
```



```R
revstring("asdf")
revstring("m\u00f8\u00f8se")
Encoding("m\u00f8\u00f8se")   # just to check if on your system it's something
                              # different!
```

{{out}}

```txt

[1] "fdsa"
[1] "esøøm"
[1] "UTF-8"

```

R can encode strings in Latin1 and UTF-8 (the default may depend on the locale); the <tt>Encoding(string)</tt> can be used to know if the string is encoded in Latin1 or UTF-8; the encoding can be forced (<tt>Encoding(x) <- "latin1"</tt>), or we can use <tt>iconv</tt> to properly translate between encodings whenever possible.


## Rascal


```rascal
import String;
reverse("string")
```



## Racket


As in Scheme:


```Racket
#lang racket

(define (string-reverse s)
  (list->string (reverse (string->list s))))

(string-reverse "aoeu")
```


{{out}}

```txt
Welcome to DrRacket, version 5.3.3.5--2013-02-20(5eddac74/d) [3m].
Language: racket; memory limit: 512 MB.
"ueoa"
>
```



## RapidQ


```vb

print reverse$("This is a test")

```



## Raven


```Raven
"asdf" reverse
```

{{out}}

```txt
fdsa
```


## REBOL


```REBOL
print reverse "asdf"
```

Note the string is reversed in place. If you were using it anywhere else, you would find it reversed:

```REBOL
x: "asdf"
print reverse x
print x ; Now reversed.
```

REBOL/View 2.7.6.3.1 14-Mar-2008 does not handle Unicode strings. This is planned for REBOL 3.


## Red


```Red>>
 reverse "asdf"
== "fdsa"
```



## Retro


```Retro
with strings'
"asdf" reverse puts
```



## REXX

All methods shown below also work with   '''NULL'''   values.

### using REVERSE BIF


```rexx
/*REXX program to reverse a string  (and show before and after strings).*/

string1 = 'A man, a plan, a canal, Panama!'
string2 = reverse(string1)

say ' original string: '  string1
say ' reversed string: '  string2
                                       /*stick a fork in it, we're done.*/
```

'''output'''

```txt

 original string:  A man, a plan, a canal, Panama!
 reversed string:  !amanaP ,lanac a ,nalp a ,nam A

```


===using SUBSTR BIF, left to right===

```rexx
/*REXX program to reverse a string  (and show before and after strings).*/

string1 = 'A man, a plan, a canal, Panama!'
string2 =
                                   do j=1  for length(string1)
                                   string2 = substr(string1,j,1)string2
                                   end   /*j*/
say ' original string: '  string1
say ' reversed string: '  string2
                                       /*stick a fork in it, we're done.*/
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.


(Regarding the previous example)   Another method of coding an abutment (an implied concatenation) is:

```rexx
                                   string2 = substr(string1,j,1) || string2
                                                /*───── or ─────*/
                                   string2=substr(string1,j,1)string2
```


===using SUBSTR BIF, right to left===

```rexx
/*REXX program to reverse a string  (and show before and after strings).*/

string1 = 'A man, a plan, a canal, Panama!'
string2 =
                                  do j=length(string1)  to 1  by -1
                                  string2 = string2 || substr(string1,j,1)
                                  end   /*j*/
say ' original string: '  string1
say ' reversed string: '  string2
                                       /*stick a fork in it, we're done.*/
```

'''output''' is identical to the 1<sup>st</sup> version.





## RLaB


```RLaB>>
 x = "rosettacode"
rosettacode

// script
rx = "";
for (i in strlen(x):1:-1)
{
  rx = rx + substr(x, i);
}

>> rx
edocattesor
```



## Ring


```ring

cStr = "asdf"  cStr2 = ""
for x = len(cStr) to 1 step -1 cStr2 += cStr[x] next
See cStr2  # fdsa

```



## Robotic


```robotic

. "local1 = Main string"
. "local2 = Temporary string storage"
. "local3 = String length"
set "$local1" to ""
set "$local2 " to ""
set "local3" to 0

input string "String to reverse:"
set "$local1" to "&INPUT&"
set "$local2" to "$local1"
set "local3" to "$local2.length"
loop start
set "$local1.(('local3' - 1) - 'loopcount')" to "$local2.('loopcount')"
loop for "('local3' - 1)"
* "Reversed string: &$local1& (Length: &$local1.length&)"
end

```



## Ruby


```ruby
str = "asdf"
reversed = str.reverse
```

or

```ruby
#encoding: utf-8
"résumé niño".reverse #=> "oñin émusér"
```



## Run BASIC


```runbasic
string$ = "123456789abcdefghijk"
for i = len(string$) to 1 step -1
 print mid$(string$,i,1);
next i
```



## Rust


```rust
extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

fn main() {
    let s = "一二三四五六七八九十";
    let s2 = "as⃝df̅";
    let reversed: String = s.chars().rev().collect();
    let reversed2: String = UnicodeSegmentation::graphemes(s2, true)
        .rev().collect();
    println!("{}", reversed);
    println!("{}", reversed2);
}
```


{{out}}
十九八七六五四三二一

f̅ds⃝a

=={{header|S-lang}}==
Here is an 8-bit version:
<lang S-lang>variable sa = "Hello, World", aa = Char_Type[strlen(sa)+1];
init_char_array(aa, sa);
array_reverse(aa);
% print(aa);

% Unfortunately, strjoin() only joins strings, so we map char()
% [sadly named: actually converts char into single-length string]
% onto the array:

print( strjoin(array_map(String_Type, &char, aa), "") );
```


Output: "dlroW ,olleH"

For a Unicode version, we'll create a variant of init_char_array().
Side note: If needed, strbytelen() would give total length of string.

<lang S-lang>define init_unicode_array(a, buf)
{
  variable len = strbytelen(buf), ch, p0 = 0, p1 = 0;
  while (p1 < len) {
    (p1, ch) = strskipchar(buf, p1, 1);
    if (ch < 0) print("oops.");
    a[p0] = ch;
    p0++;
  }
}

variable su = "Σὲ γνωρίζω ἀπὸ τὴν κόψη";
variable au = Int_Type[strlen(su)+1];
init_unicode_array(au, su);
array_reverse(au);
% print(au);
print(strjoin(array_map(String_Type, &char, au), "") );
```


Output: "ηψόκ νὴτ ὸπἀ ωζίρωνγ ὲΣ"

Note: The init...array() functions include the terminating '\0' chars, but
we don't have to filter them out as char(0) produces a zero-length string.


## SAS


```sas
data _null_;
length a b $11;
a="I am Legend";
b=reverse(a);
put a;
put b;
run;
```



## Sather


```sather
class MAIN is
  main is
    s ::= "asdf";
    reversed ::= s.reverse;
    -- current implementation does not handle multibyte encodings correctly
  end;
end;
```



## Scala

Easy way:

```scala
"asdf".reverse
```


Slightly less easy way:

```scala
"asdf".foldRight("")((a,b) => b+a)
```


Unicode-aware, method 1:


```scala
def reverse(s: String) = {
  import java.text.{Normalizer,BreakIterator}
  val norm = Normalizer.normalize(s, Normalizer.Form.NFKC) // waﬄe -> waffle (optional)
  val it = BreakIterator.getCharacterInstance
  it setText norm
  def break(it: BreakIterator, prev: Int, result: List[String] = Nil): List[String] = it.next match {
    case BreakIterator.DONE => result
    case cur => break(it, cur, norm.substring(prev, cur) :: result)
  }
  break(it, it.first).mkString
}
```

{{out}}

```txt
scala> reverse("as⃝df̅")
res0: String = f̅ds⃝a
```


Unicode-aware, method 2: I can't guarantee it get all the cases, but it does work with combining characters as well as supplementary characters. I did not bother to preserve the order of newline characters, and I didn't even consider directionality beyond just ruling it out.

```scala
def reverseString(s: String) = {
  import java.lang.Character._

  val combiningTypes = List(NON_SPACING_MARK, ENCLOSING_MARK, COMBINING_SPACING_MARK)
  def isCombiningCharacter(c: Char) = combiningTypes contains c.getType
  def isCombiningSurrogate(high: Char, low: Char) = combiningTypes contains getType(toCodePoint(high, low))
  def isCombining(l: List[Char]) = l match {
    case List(a, b) => isCombiningSurrogate(a, b)
    case List(a) => isCombiningCharacter(a)
    case Nil => true
    case _ => throw new IllegalArgumentException("isCombining expects a list of up to two characters")
  }

  def cleanSurrogate(l: List[Char]) = l match {
    case List(a, b) if a.isHighSurrogate && b.isLowSurrogate => l
    case List(a, b) if a.isLowSurrogate => Nil
    case List(a, b) => List(a)
    case _ => throw new IllegalArgumentException("cleanSurrogate expects lists of two characters, exactly")
  }

  def splitString(string: String) = (string+" ").iterator sliding 2 map (_.toList) map cleanSurrogate toList

  def recurse(fwd: List[List[Char]], rev: List[Char]): String = fwd match {
    case Nil => rev.mkString
    case c :: rest =>
      val (combining, remaining) = rest span isCombining
      recurse(remaining, c ::: combining.foldLeft(List[Char]())(_ ::: _) ::: rev)
  }
  recurse(splitString(s), Nil)
}
```

REPL on Windows doesn't handle Unicode, so I'll show the bytes instead:

```txt

scala> res71 map ("\\u%04x" format _.toInt)
res80: scala.collection.immutable.IndexedSeq[String] = IndexedSeq(\u0061, \u0073, \u20dd, \u0064, \u0066, \u0305)

scala> reverseString(res71) map ("\\u%04x" format _.toInt)
res81: scala.collection.immutable.IndexedSeq[String] = IndexedSeq(\u0066, \u0305, \u0064, \u0073, \u20dd, \u0061)

```



## Scheme


```scheme
(define (string-reverse s)
  (list->string (reverse (string->list s))))
```


 > (string-reverse "asdf")
 "fdsa"


## Scratch

[[File:ScratchReverseAString.png]]


## Sed


```sed
#!/bin/sed -f

/../! b

# Reverse a line.  Begin embedding the line between two newlines
s/^.*$/\
&\
/

# Move first character at the end.  The regexp matches until
# there are zero or one characters between the markers
tx
:x
s/\(\n.\)\(.*\)\(.\n\)/\3\2\1/
tx

# Remove the newline markers
s/\n//g
```



## Seed7

Seed7 strings are encoded with UTF-32 therefore no special Unicode solution is necessary

```seed7
$ include "seed7_05.s7i";

const func string: reverse (in string: stri) is func
  result
    var string: result is "";
  local
    var integer: index is 0;
  begin
    for index range length(stri) downto 1 do
      result &:= stri[index];
    end for;
  end func;

const proc: main is func
  begin
    writeln(reverse("Was it a cat I saw"));
  end func;
```

{{out}}

```txt

was I tac a ti saW

```



## SequenceL

'''Using Library Function:'''

There is a library function to reverse any Sequence. This works for strings since strings are Sequences of characters.

```sequencel>import <Utilities/Sequence.sl
;

main(args(2)) := Sequence::reverse(args[1]);
```


'''The Library Function:'''

The following is the library implementation of the reverse function.

```sequencel>reverse<T
 : T(1) -> T(1);
reverse(list(1))[i] :=
	let
		range := - ((1 ... size(list)) - (size(list) + 1));
	in
		list[i] foreach i within range;
```



## Sidef


```ruby
"asdf".reverse;             # fdsa
"résumé niño".reverse;      # oñin émusér
```


## Simula


```simula

BEGIN
   TEXT PROCEDURE REV(S); TEXT S;
   BEGIN
       TEXT T;
       INTEGER L,R;
       T :- COPY(S);
       L := 1; R := T.LENGTH;
       WHILE L < R DO
       BEGIN
           CHARACTER CL,CR;
           T.SETPOS(L); CL := T.GETCHAR;
           T.SETPOS(R); CR := T.GETCHAR;
           T.SETPOS(L); T.PUTCHAR(CR);
           T.SETPOS(R); T.PUTCHAR(CL);
           L := L+1;
           R := R-1;
        END;
        REV :- T;
   END REV;

   TEXT INP;
   INP :- "asdf";

   OUTTEXT(INP);      OUTIMAGE;
   OUTTEXT(REV(INP)); OUTIMAGE;
END

```

{{out}}

```txt

asdf
fdsa

```



## Self

In-place reversal:

```self
'asdf' copyMutable reverse
```



## Slate

In-place reversal:

```slate
'asdf' reverse
```

Non-destructive reversal:

```slate
'asdf' reversed
```



## Smalltalk


```smalltalk
'asdf' reverse
```

{{works with|Smalltalk/X}}
the above does inplace, destructive reverse. It is usually better to use

```smalltalk
'asdf' reversed
```

which returns a new string.


## SNOBOL4

ASCII-only

```snobol
	output = reverse(reverse("reverse"))
end
```

{{out}}

```txt
reverse
```



## Standard ML


```sml
val str_reverse = implode o rev o explode;
val string = "asdf";
val reversed = str_reverse string;
```



## Stata


Use '''[https://www.stata.com/help.cgi?f_strreverse strreverse]''' if there are only ASCII characters, and '''[https://www.stata.com/help.cgi?f_ustrreverse ustrreverse]''' if there are Unicode characters in the string.


```stata
. scalar s="ARS LONGA VITA BREVIS"
. di strreverse(s)
SIVERB ATIV AGNOL SRA
. scalar s="Ἐν ἀρχῇ ἐποίησεν ὁ θεὸς τὸν οὐρανὸν καὶ τὴν γῆν"
. di ustrreverse(s)
νῆγ νὴτ ὶακ νὸναρὐο νὸτ ςὸεθ ὁ νεσηίοπἐ ῇχρἀ νἘ
```




## Swift

Swift's strings are iterated by <code>Character</code>s, which represent "Unicode grapheme clusters", so reversing it reverses it with combining characters too:
{{works with|Swift|2.x+}}

```swift
func reverseString(s: String) -> String {
  return String(s.characters.reverse())
}
print(reverseString("asdf"))
print(reverseString("as⃝df̅"))
```

{{works with|Swift|1.x}}

```swift
func reverseString(s: String) -> String {
  return String(reverse(s))
}
println(reverseString("asdf"))
println(reverseString("as⃝df̅"))
```

{{out}}

```txt

fdsa
f̅ds⃝a

```



## Tailspin


```tailspin

templates reverse
  '$:[ $... ] -> $(-1..1:-1)...;' !
end reverse

'asdf' -> reverse -> !OUT::write

'
' -> !OUT::write

'as⃝df̅' -> reverse -> !OUT::write

```

{{out}}

```txt

fdsa
f̅ds⃝a

```



## Tcl


```tcl
package require Tcl 8.5
string reverse asdf
```


=={{header|TI-83 BASIC}}==
Note: length( and sub( can be found in the catalog.

```ti83b
:Str1
:For(I,1,length(Ans)-1
:sub(Ans,2I,1)+Ans
:End
:sub(Ans,1,I→Str1
```



## Turing


```Turing

% Reverse a string

var input : string (100)

put "Enter a string to reverse: " ..
get input

var count : int := length(input)
loop
    if count >= 1 then
        put input(count) ..
    else
        exit
    end if
    count := count - 1
end loop

```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
SET input="was it really a big fat cat i saw"
SET reversetext=TURN (input)
PRINT "before: ",input
PRINT "after:  ",reversetext
```

{{out}}

```txt

before: was it really a big fat cat i saw
after:  was i tac taf gib a yllaer ti saw

```



## UNIX Shell


```bash

#!/bin/bash
str=abcde

for((i=${#str}-1;i>=0;i--)); do rev="$rev${str:$i:1}"; done

echo $rev

```


'''or'''


```bash

str='i43go1342iu 23iu4o 23iu14i324y 2i13'
rev <<< "$str"
#rev is not built-in function, though is in /usr/bin/rev

```



## Unlambda

Reverse the whole input:

```Unlambda
``@c`d``s`|k`@c
```



## Ursala


```Ursala
#import std

#cast %s

example = ~&x 'asdf'

verbose_example = reverse 'asdf'
```

{{out}}

```txt

'fdsa'

```


=={{Header|Vala}}==

```vala
int main (string[] args) {
	if (args.length < 2) {
		stdout.printf ("Please, input a string.\n");
		return 0;
	}
	var str = new StringBuilder ();
	for (var i = 1; i < args.length; i++) {
		str.append (args[i] + " ");
	}
	stdout.printf ("%s\n", str.str.strip ().reverse ());
	return 0;
}
```


=={{Header|VBA}}==
===Non-recursive version===

```VBA
Public Function Reverse(aString as String) as String
' returns the reversed string
dim L as integer        'length of string
dim newString as string

newString = ""
L = len(aString)
for i = L to 1 step -1
 newString = newString & mid$(aString, i, 1)
next
Reverse = newString
End Function
```


### Recursive version


```VBA
Public Function RReverse(aString As String) As String
'returns the reversed string
'do it recursively: cut the string in two, reverse these fragments and put them back together in reverse order
Dim L As Integer    'length of string
Dim M As Integer    'cut point

L = Len(aString)
If L <= 1 Then   'no need to reverse
  RReverse = aString
Else
  M = Int(L / 2)
  RReverse = RReverse(Right$(aString, L - M)) & RReverse(Left$(aString, M))
End If
End Function
```


### Example dialogue


```txt

print Reverse("Public Function Reverse(aString As String) As String")
gnirtS sA )gnirtS sA gnirtSa(esreveR noitcnuF cilbuP

print RReverse("Sunday Monday Tuesday Wednesday Thursday Friday Saturday Love")
evoL yadrutaS yadirF yadsruhT yadsendeW yadseuT yadnoM yadnuS

print RReverse(Reverse("I know what you did last summer"))
I know what you did last summer

```



## VBScript

{{works with|Windows Script Host|*}}

```VBScript

WScript.Echo StrReverse("asdf")

```



## Vedit macro language

This routine reads the text from current line, reverses it and stores the reversed string in text register 10:

```vedit
Reg_Empty(10)
for (BOL; !At_EOL; Char) {
    Reg_Copy_Block(10, CP, CP+1, INSERT)
}
```

This routine reverses the current line in-place:

```vedit
BOL
while (!At_EOL) {
    Block_Copy(EOL_pos-1, EOL_pos, DELETE)
}
```



## Visual Basic

{{works with|Visual Basic|6}}

```vb
Debug.Print VBA.StrReverse("Visual Basic")
```



## Visual Basic .NET

'''Compiler:''' >= Visual Basic 2012

Includes both a simple version and a version that uses .NET's built-in ability to enumerate strings by grapheme to support combining characters.

Since the windows console may not support Unicode, the program can optionally redirect its output to a file.


```vbnet
#Const REDIRECTOUT = True

Module Program
    Const OUTPATH = "out.txt"

    ReadOnly TestCases As String() = {"asdf", "as⃝df̅", "Les Misérables"}

    ' SIMPLE VERSION
    Function Reverse(s As String) As String
        Dim t = s.ToCharArray()
        Array.Reverse(t)
        Return New String(t)
    End Function

    ' EXTRA CREDIT VERSION
    Function ReverseElements(s As String) As String
        ' In .NET, a text element is series of code units that is displayed as one character, and so reversing the text
        ' elements of the string correctly handles combining character sequences and surrogate pairs.
        Dim elements = Globalization.StringInfo.GetTextElementEnumerator(s)
        Return String.Concat(AsEnumerable(elements).OfType(Of String).Reverse())
    End Function

    ' Wraps an IEnumerator, allowing it to be used as an IEnumerable.
    Iterator Function AsEnumerable(enumerator As IEnumerator) As IEnumerable
        Do While enumerator.MoveNext()
            Yield enumerator.Current
        Loop
    End Function

    Sub Main()
        Const INDENT = "    "

#If REDIRECTOUT Then
        Const OUTPATH = "out.txt"
        Using s = IO.File.Open(OUTPATH, IO.FileMode.Create),
              sw As New IO.StreamWriter(s)
            Console.SetOut(sw)
#Else
        Try
            Console.OutputEncoding = Text.Encoding.ASCII
            Console.OutputEncoding = Text.Encoding.UTF8
            Console.OutputEncoding = Text.Encoding.Unicode
        Catch ex As Exception
            Console.WriteLine("Failed to set console encoding to Unicode." & vbLf)
        End Try
#End If
            For Each c In TestCases
                Console.WriteLine(c)
                Console.WriteLine(INDENT & "SIMPLE:   " & Reverse(c))
                Console.WriteLine(INDENT & "ELEMENTS: " & ReverseElements(c))
                Console.WriteLine()
            Next
#If REDIRECTOUT Then
        End Using
#End If
    End Sub
End Module
```


{{out|note=copied from Notepad}}
Output is presented using non-fixed-width typeface to properly display combining characters.
<pre style="font-family:TimesNewRoman,Times New Roman,Times,serif">asdf
    SIMPLE:   fdsa
    ELEMENTS: fdsa

as⃝df̅
    SIMPLE:   ̅fd⃝sa
    ELEMENTS: f̅ds⃝a

Les Misérables
    SIMPLE:   selbaŕesiM seL
    ELEMENTS: selbarésiM seL
```



## Wart


```wart
(rev "asdf")
```


Wart doesn't support Unicode yet.


## Wortel


```wortel
; the @rev operator reverses strings and arrays
@rev "abc" ; returns "cba"
; or the same thing using a pointer expression
!~r "abc"
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
string 0;               \use zero-terminated strings, instead of MSb terminated

func StrLen(Str);       \Return the number of characters in an ASCIIZ string
char Str;
int  I;
for I:= 0 to -1>>1-1 do
        if Str(I) = 0 then return I;

func RevStr(S);         \Reverse the order of the bytes in a string
char S;
int  L, I, T;
[L:= StrLen(S);
for I:= 0 to L/2-1 do
        [T:= S(I);  S(I):= S(L-I-1);  S(L-I-1):= T];
return S;
];

[Text(0, RevStr("a")); CrLf(0);
 Text(0, RevStr("ab")); CrLf(0);
 Text(0, RevStr("abc")); CrLf(0);
 Text(0, RevStr("Able was I ere I saw Elba.")); CrLf(0);
]
```


Output:

```txt

a
ba
cba
.ablE was I ere I saw elbA

```



## Yorick

This only handles ASCII characters. It works by converting a string to an array of char; dropping the last character (which is the null byte); reversing the order of the characters; then converting back to a string.

```yorick
strchar(strchar("asdf")(:-1)(::-1))
```



## zkl

These only handle ASCII characters, no extra credit.

```txt
"this is a test".reverse()
```

Old school ways to do it:

Build by prepending characters, creates n strings:

```txt
"this is a test".reduce(fcn(p,c){c+p})
```

Convert to list, reverse, convert back to string:

```txt
"this is a test".split("").reverse().concat()
```

Write to a byte buffer and convert to string:

```txt
"this is a test".pump(Void,Data().insert.fp(0)).text
```

The ".fp(0)" creates a closure so each character is fed to data.insert(0,c). pump is a method that sends each character to a function to a sink (in this case /dev/null). The output is the result of the last call, which is data.insert which is self/data.


{{omit from|bc|no string operations}}
{{omit from|dc|no string operations}}
{{omit from|Openscad}}
[[Wikipedia::https://en.wikipedia.org/wiki/Comparison_of_programming_languages_%28string_functions%29#reverse]]
