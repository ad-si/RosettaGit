+++
title = "String length"
description = ""
date = 2019-10-09T08:15:28Z
aliases = []
[extra]
id = 2472
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
+++

## Task

Find the <em>character</em> and <em>byte</em> length of a string.

This means encodings like [[UTF-8]] need to be handled properly, as there is not necessarily a one-to-one relationship between bytes and characters.

By ''character'', we mean an individual Unicode ''code point'', not a user-visible ''grapheme'' containing combining characters.

For example, the character length of "møøse" is 5 but the byte length is 7 in UTF-8 and 10 in UTF-16.

Non-BMP code points (those between 0x10000 and 0x10FFFF) must also be handled correctly: answers should produce actual character counts in code points, not in code unit counts.

Therefore a string like "𝔘𝔫𝔦𝔠𝔬𝔡𝔢" (consisting of the 7 Unicode characters U+1D518  U+1D52B U+1D526 U+1D520 U+1D52C U+1D521 U+1D522) is 7 characters long, '''not''' 14 UTF-16 code units; and it is 28 bytes long whether encoded in UTF-8 or in  UTF-16.

Please mark your examples with <nowiki>
### Character Length=== or ===Byte Length
</nowiki>.

If your language is capable of providing the string length in graphemes, mark those examples with <nowiki>
### Grapheme Length
</nowiki>.

For example, the string "J̲o̲s̲é̲" ("J\x{332}o\x{332}s\x{332}e\x{301}\x{332}") has 4 user-visible graphemes, 9 characters (code points), and 14 bytes when encoded in UTF-8.




## 360 Assembly

Assembler 360 use EBCDIC coding, so one character is one byte.
The L' atrribute can be seen as the length function for assembler 360.

```360asm
*        String length             06/07/2016
LEN      CSECT
         USING  LEN,15             base register
         LA     1,L'C              length of C
         XDECO  1,PG
         XPRNT  PG,12
         LA     1,L'H              length of H
         XDECO  1,PG
         XPRNT  PG,12
         LA     1,L'F              length of F
         XDECO  1,PG
         XPRNT  PG,12
         LA     1,L'D              length of D
         XDECO  1,PG
         XPRNT  PG,12
         LA     1,L'PG             length of PG
         XDECO  1,PG
         XPRNT  PG,12
         BR     14                 exit           length
C        DS     C                  character       1
H        DS     H                  half word       2
F        DS     F                  full word       4
D        DS     D                  double word     8
PG       DS     CL12               string         12
         END    LEN
```

```txt

           1
           2
           4
           8
          12

```



## 4D


### Byte Length


```4d
$length:=Length("Hello, world!")
```



## ActionScript


### Byte length

This uses UTF-8 encoding. For other encodings, the ByteArray's <code>writeMultiByte()</code> method can be used.

```ActionScript

package {

    import flash.display.Sprite;
    import flash.events.Event;
    import flash.utils.ByteArray;

    public class StringByteLength extends Sprite {

        public function StringByteLength() {
            if ( stage ) _init();
            else addEventListener(Event.ADDED_TO_STAGE, _init);
        }

        private function _init(e:Event = null):void {
            var s1:String = "The quick brown fox jumps over the lazy dog";
            var s2:String = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢";
            var s3:String = "José";

            var b:ByteArray = new ByteArray();
            b.writeUTFBytes(s1);
            trace(b.length);  // 43

            b.clear();
            b.writeUTFBytes(s2);
            trace(b.length);  // 28

            b.clear();
            b.writeUTFBytes(s3);
            trace(b.length);  // 5
        }

    }

}

```



### Character Length


```actionscript

var s1:String = "The quick brown fox jumps over the lazy dog";
var s2:String = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢";
var s3:String = "José";
trace(s1.length, s2.length, s3.length);  // 43, 14, 4

```



## Ada

### Byte Length


```ada
Str    : String := "Hello World";
Length : constant Natural := Str'Size / 8;
```

The 'Size attribute returns the size of an object in bits. Provided that under "byte" one understands an octet of bits, the length in "bytes" will be 'Size divided to 8. Note that this  is not necessarily the machine storage unit. In order to make the program portable, System.Storage_Unit should be used instead of "magic number" 8. System.Storage_Unit yields the number of bits in a storage unit on the current machine. Further, the length of a string object is not the length of what the string contains in whatever measurement units. String as an object may have a "dope" to keep the array bounds. In fact the object length can even be 0, if the compiler optimized the object away. So in most cases "byte length" makes no sense in Ada.


### Character Length


```ada
Latin_1_Str    : String           := "Hello World";
UCS_16_Str     : Wide_String      := "Hello World";
Unicode_Str    : Wide_Wide_String := "Hello World";
Latin_1_Length : constant Natural := Latin_1_Str'Length;
UCS_16_Length  : constant Natural := UCS_16_Str'Length;
Unicode_Length : constant Natural := Unicode_Str'Length;
```

The attribute 'Length yields the number of elements of an [[array]]. Since strings in Ada are arrays of characters, 'Length is the string length. Ada supports strings of [[Latin-1]], [[UCS-16]] and full [[Unicode]] characters. In the example above character length of all three strings is 11. The length of the objects in bits will differ.


## Aime


### Byte Length


```aime
length("Hello, World!")
```

or

```aime
~"Hello, World!"
```



## ALGOL 68


### Bits and Bytes Length


```algol68
BITS bits := bits pack((TRUE, TRUE, FALSE, FALSE)); # packed array of BOOL #
BYTES bytes := bytes pack("Hello, world"); # packed array of CHAR #
print((
  "BITS and BYTES are fixed width:", new line,
  "bits width:", bits width, ", max bits: ", max bits, ", bits:", bits, new line,
  "bytes width: ",bytes width, ", UPB:",UPB STRING(bytes), ", string:", STRING(bytes),"!", new line
))
```

Output:

```txt

BITS and BYTES are fixed width:
bits width:        +32, max bits: TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT, bits:TTFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
bytes width:         +32, UPB:        +32, string:Hello, world!

```


### Character Length


```algol68
STRING str := "hello, world";
INT length := UPB str;
printf(($"Length of """g""" is "g(3)l$,str,length));

printf(($l"STRINGS can start at -1, in which case LWB must be used:"l$));
STRING s := "abcd"[@-1];
print(("s:",s, ", LWB:", LWB s, ", UPB:",UPB s, ", LEN:",UPB s - LWB s + 1))
```

Output:

```txt

Length of "hello, world" is +12
STRINGS can start at -1, in which case LWB must be used:
s:abcd, LWB:         -1, UPB:         +2, LEN:         +4

```



## Apex


```Apex

String myString = 'abcd';
System.debug('Size of String', myString.length());

```



## AppleScript


### Byte Length


```applescript
count of "Hello World"
```

Mac OS X 10.5 (Leopard) includes AppleScript 2.0 which uses only Unicode (UTF-16) character strings.
This example has been tested on OSX 10.8.5. Added a combining char for testing.

```applescript

set inString to "Hello é̦世界"
set byteCount to 0

repeat with c in inString
	set t to id of c
	if ((count of t) > 0) then
		repeat with i in t
			set byteCount to byteCount + doit(i)
		end repeat
	else
		set byteCount to byteCount + doit(t)
	end if
end repeat

byteCount

on doit(cid)
	set n to (cid as integer)
	if n > 67108863 then -- 0x3FFFFFF
		return 6
	else if n > 2097151 then -- 0x1FFFFF
		return 5
	else if n > 65535 then -- 0xFFFF
		return 4
	else if n > 2047 then -- 0x07FF
		return 3
	else if n > 127 then -- 0x7F
		return 2
	else
		return 1
	end if
end doit
```



### Character Length


```applescript
count of "Hello World"
```

Or:

```applescript
count "Hello World"
```



## Applesoft BASIC


```ApplesoftBASIC
? LEN("HELLO, WORLD!")
```



## Arturo



### Character Length



```arturo
str "Hello World"

print "length = " + $(size str)
```


```txt
length = 11
```



## AutoHotkey


### Character Length


```AutoHotkey
Msgbox % StrLen("Hello World")
```

Or:

```AutoHotkey
String := "Hello World"
StringLen, Length, String
Msgbox % Length
```



## AWK


### Byte Length

From within any code block:

```awk
w=length("Hello, world!")      # static string example
x=length("Hello," s " world!") # dynamic string example
y=length($1)                   # input field example
z=length(s)                    # variable name example
```

Ad hoc program from command line:

```txt
 echo "Hello, wørld!" | awk '{print length($0)}'   # 14
```

From executable script: (prints for every line arriving on stdin)

```awk
#!/usr/bin/awk -f
{print"The length of this line is "length($0)}
```



## Axe

Axe supports two string encodings: a rough equivalent to ASCII, and a token-based format. These examples are for ASCII.


### Byte Length


```axe
"HELLO, WORLD"→Str1
Disp length(Str1)▶Dec,i
```



## Batch File


### Byte Length


```dos
@echo off
setlocal enabledelayedexpansion
call :length %1 res
echo length of %1 is %res%
goto :eof

:length
set str=%~1
set cnt=0
:loop
if "%str%" equ "" (
	set %2=%cnt%
	goto :eof
	)
set str=!str:~1!
set /a cnt = cnt + 1
goto loop
```



## BaCon

BaCon has full native support for UTF-8 encoding.

```qbasic
PRINT "Bytelen of 'hello': ", LEN("hello")
PRINT "Charlen of 'hello': ", ULEN("hello")

PRINT "Bytelen of 'møøse': ", LEN("møøse")
PRINT "Charlen of 'møøse': ", ULEN("møøse")

PRINT "Bytelen of '𝔘𝔫𝔦𝔠𝔬𝔡𝔢': ", LEN("𝔘𝔫𝔦𝔠𝔬𝔡𝔢")
PRINT "Charlen of '𝔘𝔫𝔦𝔠𝔬𝔡𝔢': ", ULEN("𝔘𝔫𝔦𝔠𝔬𝔡𝔢")
```

```txt

Bytelen of 'hello': 5
Charlen of 'hello': 5
Bytelen of 'møøse': 7
Charlen of 'møøse': 5
Bytelen of '𝔘𝔫𝔦𝔠𝔬𝔡𝔢': 28
Charlen of '𝔘𝔫𝔦𝔠𝔬𝔡𝔢': 7

```



## BASIC


### Character Length

BASIC only supports single-byte characters. The character "ø" is converted to "°" for printing to the console and length functions, but will still output to a file as "ø".

```qbasic
 INPUT a$
 PRINT LEN(a$)
```


=
## ZX Spectrum Basic
=
The ZX Spectrum needs line numbers:


```zxbasic
10 INPUT a$
20 PRINT LEN a$
```


However, it's not quite as trivial as this.


### =Byte length=

Strings can contain embedded colour codes; an inline INVERSE (CAPS SHIFT + 4) would be represented as CHR$ 20 + CHR$ 1. The LEN function will account for all these bytes. On the flipside, ZX Spectrum keywords are all tokenised, and there's nothing stopping you using them in a string; " RANDOMIZE ", if the keyword is used, will take a single byte (CHR$ 249) rather than the 11 characters it actually uses. The above version of the code will produce byte length.


### =Character length=

Stripping out all entries in the string with codes in the lower 32 will get rid of colour control codes. The character length of a token is not a simple thing to determine, so this version strips them out too by eliminating anything above CHR$ 164 (the last UDG). A 91-entry DATA list of token lengths might be the next step.


```zxbasic
10 INPUT a$
20 LET b$=""
30 FOR x=1 TO LEN a$
40 LET k=CODE a$(x)
50 IF k<32 OR k>164 THEN GOTO 70
60 LET b$=b$+a$(k)
70 NEXT x
80 PRINT LEN b$
```



### =Grapheme length=


Alternatively, the string might include control codes for backspacing and overwriting;


```zxbasic
10 LET a$=CHR$ 111+CHR$ 8+CHR$ 21+CHR$ 1+CHR$ 34
```

will produce an "o" character overprinted with a quotation mark, resulting in a "passable" impression of an umlaut. The above code will reduce this to two characters when the actual printed length is one (byte length is of course five). The other possible workaround is to print the string and calculate the character length based on the resultant change in screen position. (This will only work for a string with a character length that actually fits on the screen, so below about 670.)


```zxbasic
10 INPUT a$
20 CLS
30 PRINT a$;
40 LET x=PEEK 23688: LET y=PEEK 23689
50 PRINT CHR$ 13;33-x+32*(24-y)
```


=
## Commodore BASIC
=
Commodore BASIC needs line numbers too, and can't use mixed case. When in mixed case mode, everything must be in lower case letters. However, the default is UPPERCASE + graphic characters; thus everything appears as UPPER case character.


```basic
10 INPUT A$
20 PRINT LEN(A$)
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 INPUT PROMPT "String: ":TX$
110 PRINT LEN(TX$)
```



## BBC BASIC


### Character Length


```bbcbasic
      INPUT text$
      PRINT LEN(text$)
```


### Byte Length

```bbcbasic
      CP_ACP = 0
      CP_UTF8 = &FDE9

      textA$ = "møøse"
      textW$ = "                 "
      textU$ = "                 "

      SYS "MultiByteToWideChar", CP_ACP, 0, textA$, -1, !^textW$, LEN(textW$)/2 TO nW%
      SYS "WideCharToMultiByte", CP_UTF8, 0, textW$, -1, !^textU$, LEN(textU$), 0, 0
      PRINT "Length in bytes (ANSI encoding) = " ; LEN(textA$)
      PRINT "Length in bytes (UTF-16 encoding) = " ; 2*(nW%-1)
      PRINT "Length in bytes (UTF-8 encoding) = " ; LEN($$!^textU$)
```

Output:

```txt
Length in bytes (ANSI encoding) = 5
Length in bytes (UTF-16 encoding) = 10
Length in bytes (UTF-8 encoding) = 7
```



## Bracmat

The solutions work with UTF-8 encoded strings.

### Byte Length


```bracmat
(ByteLength=
  length
.   @(!arg:? [?length)
  & !length
);

out$ByteLength$𝔘𝔫𝔦𝔠𝔬𝔡𝔢
```

Answer:

```txt
28
```


### Character Length


```bracmat
(CharacterLength=
  length c
.     0:?length
    & @( !arg
       :   ?
           ( %?c
           & utf$!c:?k
           & 1+!length:?length
           & ~
           )
           ?
       )
  | !length
);

out$CharacterLength$𝔘𝔫𝔦𝔠𝔬𝔡𝔢
```

Answer:

```txt
7
```

An improved version scans the input string character wise, not byte wise. Thus many string positions that are deemed not to be possible starting positions of UTF-8 are not even tried. The patterns <code>[!p</code> and <code>[?p</code> implement a ratchet mechanism. <code>[!p</code> indicates the start of a character and <code>[?p</code> remembers the end of the character, which becomes the start position of the next byte.

```bracmat
(CharacterLength=
  length c p
.     0:?length:?p
    & @( !arg
       :   ?
           ( [!p %?c
           & utf$!c:?k
           & 1+!length:?length
           )
           ([?p&~)
           ?
       )
  | !length
);
```


Later versions of Bracmat have the built in function <code>vap</code> that "vaporises" a string into "atoms". If the string is UTF-8 encoded, then each "atom" is one UTF-8 character, so the length of the list of atoms is the character length of the input string. The first argument to the <code>vap</code> function is a function that will be applied to every UTF-8 encoded character in the input string. The outcomes of these function calls are the elements in the resulting list. In the solution below we choose an anonymous function <code>(=.!arg)</code> that just returns the characters themselves.

```bracmat
(CharacterLength=
  length
. vap$((=.!arg).!arg):? [?length&!length
);
```



## C


### Byte Length

```c
#include <string.h>

int main(void)
{
  const char *string = "Hello, world!";
  size_t length = strlen(string);

  return 0;
}
```

or by hand:


```c
int main(void)
{
  const char *string = "Hello, world!";
  size_t length = 0;

  const char *p = string;
  while (*p++ != '\0') length++;

  return 0;
}
```


or (for arrays of char only)


```cpp
#include <iostream>

int main(void)
{
  char s[] = "Hello, world!";
  size_t length = sizeof s - 1;

  return 0;
}
```



### Character Length

For wide character strings (usually Unicode uniform-width encodings such as UCS-2 or UCS-4):


```c
#include <stdio.h>
#include <wchar.h>

int main(void)
{
   wchar_t *s = L"\x304A\x306F\x3088\x3046"; /* Japanese hiragana ohayou */
   size_t length;

   length = wcslen(s);
   printf("Length in characters = %d\n", length);
   printf("Length in bytes      = %d\n", sizeof(s) * sizeof(wchar_t));

   return 0;
}
```



### Dealing with raw multibyte string

Following code is written in UTF-8, and environment locale is assumed to be UTF-8 too.  Note that "møøse" is here directly written in the source code for clarity, which is not a good idea in general.  <code>mbstowcs()</code>, when passed NULL as the first argument, effectively counts the number of chars in given string under current locale.

```c
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

int main()
{
	setlocale(LC_CTYPE, "");
	char moose[] = "møøse";
	printf("bytes: %d\n", sizeof(moose) - 1);
	printf("chars: %d\n", (int)mbstowcs(0, moose, 0));

	return 0;
}
```
output
```txt
bytes: 7
chars: 5
```



## C++


### Byte Length

```cpp
#include <string> // (not <string.h>!)
using std::string;

int main()
{
  string s = "Hello, world!";
  string::size_type length = s.length(); // option 1: In Characters/Bytes
  string::size_type size = s.size();     // option 2: In Characters/Bytes
  // In bytes same as above since sizeof(char) == 1
  string::size_type bytes = s.length() * sizeof(string::value_type);
}
```

For wide character strings:


```cpp
#include <string>
using std::wstring;

int main()
{
  wstring s = L"\u304A\u306F\u3088\u3046";
  wstring::size_type length = s.length() * sizeof(wstring::value_type); // in bytes
}
```



### Character Length


For wide character strings:


```cpp
#include <string>
using std::wstring;

int main()
{
  wstring s = L"\u304A\u306F\u3088\u3046";
  wstring::size_type length = s.length();
}
```


For narrow character strings:

```cpp
#include <iostream>
#include <codecvt>
int main()
{
    std::string utf8 = "\x7a\xc3\x9f\xe6\xb0\xb4\xf0\x9d\x84\x8b"; // U+007a, U+00df, U+6c34, U+1d10b
    std::cout << "Byte length: " << utf8.size() << '\n';
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv;
    std::cout << "Character length: " << conv.from_bytes(utf8).size() << '\n';
}
```


```cpp
#include <cwchar>
// for mbstate_t
#include <locale>

// give the character length for a given named locale
std::size_t char_length(std::string const& text, char const* locale_name)
{
  // locales work on pointers; get length and data from string and
  // then don't touch the original string any more, to avoid
  // invalidating the data pointer
  std::size_t len = text.length();
  char const* input = text.data();

  // get the named locale
  std::locale loc(locale_name);

  // get the conversion facet of the locale
  typedef std::codecvt<wchar_t, char, std::mbstate_t> cvt_type;
  cvt_type const& cvt = std::use_facet<cvt_type>(loc);

  // allocate buffer for conversion destination
  std::size_t bufsize = cvt.max_length()*len;
  wchar_t* destbuf = new wchar_t[bufsize];
  wchar_t* dest_end;

  // do the conversion
  mbstate_t state = mbstate_t();
  cvt.in(state, input, input+len, input, destbuf, destbuf+bufsize, dest_end);

  // determine the length of the converted sequence
  std::size_t length = dest_end - destbuf;

  // get rid of the buffer
  delete[] destbuf;

  // return the result
  return length;
}
```


Example usage (note that the locale names are OS specific):


```cpp
#include <iostream>

int main()
{
  // Tür (German for door) in UTF8
  std::cout << char_length("\x54\xc3\xbc\x72", "de_DE.utf8") << "\n"; // outputs 3

  // Tür in ISO-8859-1
  std::cout << char_length("\x54\xfc\x72", "de_DE") << "\n"; // outputs 3
}
```


Note that the strings are given as explicit hex sequences, so that the encoding used for the source code won't matter.

## C#
'''Platform:''' [[.NET]]

### Character Length


```c#
string s = "Hello, world!";
int characterLength = s.Length;
```



### Byte Length

Strings in .NET are stored in Unicode.

```c#
using System.Text;

string s = "Hello, world!";
int byteLength = Encoding.Unicode.GetByteCount(s);
```

To get the number of bytes that the string would require in a different encoding, e.g., UTF8:

```c#
int utf8ByteLength = Encoding.UTF8.GetByteCount(s);
```



## Clean


### Byte Length

Clean Strings are unboxed arrays of characters. Characters are always a single byte. The function size returns the number of elements in an array.


```clean
import StdEnv

strlen :: String -> Int
strlen string = size string

Start = strlen "Hello, world!"
```



## Clojure


### Byte Length


```clojure
(def utf-8-octet-length #(-> % (.getBytes "UTF-8") count))
(map utf-8-octet-length  ["møøse" "𝔘𝔫𝔦𝔠𝔬𝔡𝔢" "J\u0332o\u0332s\u0332e\u0301\u0332"]) ; (7 28 14)

(def utf-16-octet-length (comp (partial * 2) count))
(map utf-16-octet-length ["møøse" "𝔘𝔫𝔦𝔠𝔬𝔡𝔢" "J\u0332o\u0332s\u0332e\u0301\u0332"]) ; (10 28 18)

(def code-unit-length count)
(map code-unit-length    ["møøse" "𝔘𝔫𝔦𝔠𝔬𝔡𝔢" "J\u0332o\u0332s\u0332e\u0301\u0332"]) ; (5 14 9)
```



### Character length


```clojure
(def character-length #(.codePointCount % 0 (count %)))
(map character-length    ["møøse" "𝔘𝔫𝔦𝔠𝔬𝔡𝔢" "J\u0332o\u0332s\u0332e\u0301\u0332"]) ; (5 7 9)
```



### Grapheme Length


```clojure
(def grapheme-length
  #(->> (doto (java.text.BreakIterator/getCharacterInstance)
          (.setText %))
        (partial (memfn next))
        repeatedly
        (take-while (partial not= java.text.BreakIterator/DONE))
        count))
(map grapheme-length     ["møøse" "𝔘𝔫𝔦𝔠𝔬𝔡𝔢" "J\u0332o\u0332s\u0332e\u0301\u0332"]) ; (5 7 4)
```



## COBOL


### Byte Length


```cobol
FUNCTION BYTE-LENGTH(str)
```


Alternative, non-standard extensions:
```cobol>LENGTH OF str</lang


```cobol
FUNCTION LENGTH-AN(str)
```



### Character Length


```cobol
FUNCTION LENGTH(str)
```



## ColdFusion


### Byte Length


```cfm

<cfoutput>
<cfset str = "Hello World">
<cfset j = createObject("java","java.lang.String").init(str)>
<cfset t = j.getBytes()>
<p>#arrayLen(t)#</p>
</cfoutput>

```



### Character Length


```cfm
#len("Hello World")#
```



## Common Lisp


### Byte Length

In Common Lisp, there is no standard way to examine byte representations of characters, except perhaps to write a string to a file, then reopen the file as binary. However, specific implementations will have ways to do so. For example:

```lisp
(length (sb-ext:string-to-octets "Hello Wørld"))
```

returns 12.

### Character Length

Common Lisp represents strings as sequences of characters, not bytes, so there is no ambiguity about the encoding. The [http://www.lispworks.com/documentation/HyperSpec/Body/f_length.htm length] function always returns the number of characters in a string.

```lisp
(length "Hello World")
```

returns 11, and

```txt
(length "Hello Wørld")
```

returns 11 too.


## Component Pascal

Component Pascal encodes strings in UTF-16, which represents each character with 16-bit value.


### Character Length


```oberon2

MODULE TestLen;

	IMPORT Out;

	PROCEDURE DoCharLength*;
		VAR s: ARRAY 16 OF CHAR; len: INTEGER;
	BEGIN
		s := "møøse";
		len := LEN(s$);
		Out.String("s: "); Out.String(s); Out.Ln;
		Out.String("Length of characters: "); Out.Int(len, 0); Out.Ln
	END DoCharLength;

END TestLen.

```


A symbol ''$'' in ''LEN(s$)'' in Component Pascal allows to copy sequence of characters up to null-terminated character. So, ''LEN(s$)'' returns a real length of characters instead of allocated by variable.

Running command ''TestLen.DoCharLength'' gives following output:

```txt

s: møøse
Length of characters: 5

```



### Byte Length


```oberon2

MODULE TestLen;

	IMPORT Out;

	PROCEDURE DoByteLength*;
		VAR s: ARRAY 16 OF CHAR; len, v: INTEGER;
	BEGIN
		s := "møøse";
		len := LEN(s$);
		v := SIZE(CHAR) * len;
		Out.String("s: "); Out.String(s); Out.Ln;
		Out.String("Length of characters in bytes: "); Out.Int(v, 0); Out.Ln
	END DoByteLength;

END TestLen.

```


Running command ''TestLen.DoByteLength'' gives following output:

```txt

s: møøse
Length of characters in bytes: 10

```



## D


### Byte Length


```d
import std.stdio;

void showByteLen(T)(T[] str) {
    writefln("Byte length: %2d - %(%02x%)",
             str.length * T.sizeof, cast(ubyte[])str);
}

void main() {
    string s1a = "møøse"; // UTF-8
    showByteLen(s1a);
    wstring s1b = "møøse"; // UTF-16
    showByteLen(s1b);
    dstring s1c = "møøse"; // UTF-32
    showByteLen(s1c);
    writeln();

    string s2a = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢";
    showByteLen(s2a);
    wstring s2b = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢";
    showByteLen(s2b);
    dstring s2c = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢";
    showByteLen(s2c);
    writeln();

    string s3a = "J̲o̲s̲é̲";
    showByteLen(s3a);
    wstring s3b = "J̲o̲s̲é̲";
    showByteLen(s3b);
    dstring s3c = "J̲o̲s̲é̲";
    showByteLen(s3c);
}
```

```txt
Byte length:  7 - 6dc3b8c3b87365
Byte length: 10 - 6d00f800f80073006500
Byte length: 20 - 6d000000f8000000f80000007300000065000000

Byte length: 28 - f09d9498f09d94abf09d94a6f09d94a0f09d94acf09d94a1f09d94a2
Byte length: 28 - 35d818dd35d82bdd35d826dd35d820dd35d82cdd35d821dd35d822dd
Byte length: 28 - 18d501002bd5010026d5010020d501002cd5010021d5010022d50100

Byte length: 14 - 4accb26fccb273ccb265cc81ccb2
Byte length: 18 - 4a0032036f00320373003203650001033203
Byte length: 36 - 4a000000320300006f000000320300007300000032030000650000000103000032030000
```



### Character Length


```d
import std.stdio, std.range, std.conv;

void showCodePointsLen(T)(T[] str) {
    writefln("Character length: %2d - %(%x %)",
             str.walkLength(), cast(uint[])to!(dchar[])(str));
}

void main() {
    string s1a = "møøse"; // UTF-8
    showCodePointsLen(s1a);
    wstring s1b = "møøse"; // UTF-16
    showCodePointsLen(s1b);
    dstring s1c = "møøse"; // UTF-32
    showCodePointsLen(s1c);
    writeln();

    string s2a = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢";
    showCodePointsLen(s2a);
    wstring s2b = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢";
    showCodePointsLen(s2b);
    dstring s2c = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢";
    showCodePointsLen(s2c);
    writeln();

    string s3a = "J̲o̲s̲é̲";
    showCodePointsLen(s3a);
    wstring s3b = "J̲o̲s̲é̲";
    showCodePointsLen(s3b);
    dstring s3c = "J̲o̲s̲é̲";
    showCodePointsLen(s3c);
}
```

```txt
Character length:  5 - 6d f8 f8 73 65
Character length:  5 - 6d f8 f8 73 65
Character length:  5 - 6d f8 f8 73 65

Character length:  7 - 1d518 1d52b 1d526 1d520 1d52c 1d521 1d522
Character length:  7 - 1d518 1d52b 1d526 1d520 1d52c 1d521 1d522
Character length:  7 - 1d518 1d52b 1d526 1d520 1d52c 1d521 1d522

Character length:  9 - 4a 332 6f 332 73 332 65 301 332
Character length:  9 - 4a 332 6f 332 73 332 65 301 332
Character length:  9 - 4a 332 6f 332 73 332 65 301 332
```



## DataWeave


### Character Length


```DataWeave
sizeOf("foo")
```


```txt

3

```



## Dc


### Byte Length

Dc's "P" command prints numbers as strings. The number 22405534230753963835153736737 (hint: look at it in hex) represents "Hello world!". Counting the byte length of it is counting how often it iteratively can be divided by 256 with non zero result. The snippet defines the macro which calculates the length, prints the string 1st and then its length.

```Dc
[256 / d 0<L 1 + ] sL
22405534230753963835153736737 d P A P
lL x f
```


```txt

Hello world!
12

```



### Character Length

The following code output 5, which is the length of the string "abcde"

```Dc
[abcde]Zp
```


=={{header|Déjà Vu}}==

### Byte Length

Byte length depends on the encoding, which internally is UTF-8, but users of the language can only get at the raw bytes after encoding a string into a blob.

```dejavu
!. len !encode!utf-8 "møøse"
!. len !encode!utf-8 "𝔘𝔫𝔦𝔠𝔬𝔡𝔢"
```

```txt

7
28
```



### Character Length


```dejavu
!. len "møøse"
!. len "𝔘𝔫𝔦𝔠𝔬𝔡𝔢"
```

```txt
5
7
```



## E


### Character Length


```e
"Hello World".size()
```


## Elena


### Character Length

ELENA 4.x :

```elena
import extensions;

public program()
{
    var s := "Hello, world!";             // UTF-8 literal
    var ws := "Привет мир!"w;             // UTF-16 literal

    var s_length := s.Length;             // Number of UTF-8 characters
    var ws_length := ws.Length;           // Number of UTF-16 characters
    var u_length := ws.toArray().Length;    //Number of UTF-32 characters
}
```



### Byte Length

ELENA 4.x :

```elena
import extensions;

public program()
{
    var s := "Hello, world!";                     // UTF-8 literal
    var ws := "Привет мир!"w;                     // UTF-16 literal

    var s_byte_length := s.toByteArray().Length;  // Number of bytes
    var ws_byte_length := ws.toByteArray().Length;  // Number of bytes
}
```



## Elixir


### Byte Length


```elixir

name = "J\x{332}o\x{332}s\x{332}e\x{301}\x{332}"
byte_size(name)
# => 14

```


### Character Length


```elixir

name = "J\x{332}o\x{332}s\x{332}e\x{301}\x{332}"
Enum.count(String.codepoints(name))
# => 9

```


### Grapheme Length


```elixir

name = "J\x{332}o\x{332}s\x{332}e\x{301}\x{332}"
String.length(name)
# => 4

```



## Emacs Lisp


### Character Length


```lisp
(length "hello")
=> 5
```


### Byte Length


```lisp
(string-bytes "\u1D518\u1D52B\u1D526")
=> 12
```


<code>string-bytes</code> is the length of Emacs' internal representation.  In Emacs 23 up this is utf-8.  In earlier versions it was "emacs-mule".


### Display Length

<code>string-width</code> is the displayed width of a string in the current frame and window.  This is not the same as grapheme length since various Asian characters may display in 2 columns, depending on the type of tty or GUI.


```lisp
(let ((str (apply 'string
                  (mapcar (lambda (c) (decode-char 'ucs c))
                          '(#x1112 #x1161 #x11ab #x1100 #x1173 #x11af)))))
  (list (length str)
        (string-bytes str)
        (string-width str)))
=> (6 18 4)  ;; in emacs 23 up
```



## Erlang


### Character Length

Strings are lists of integers in Erlang. So "𝔘𝔫𝔦𝔠𝔬𝔡𝔢" is the list [120088,120107,120102,120096,120108,120097,120098].

```txt

9> U = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢".
[120088,120107,120102,120096,120108,120097,120098]
10> erlang:length(U).
7

```



## Euphoria


### Character Length


```Euphoria
print(1,length("Hello World"))
```


=={{header|F_Sharp|F#}}==
This is delegated to the standard .Net framework string and encoding functions.

### Byte Length


```fsharp
open System.Text
let byte_length str = Encoding.UTF8.GetByteCount(str)
```


### Character Length


```fsharp
"Hello, World".Length
```



## Factor


### Byte Length

Here are two words to compute the byte length of strings. The first one doesn't allocate new memory, the second one can easily be adapted to measure the byte length of encodings other than UTF8.

```factor
: string-byte-length ( string -- n ) [ code-point-length ] map-sum ;
: string-byte-length-2 ( string -- n ) utf8 encode length ;
```


### Character Length

<code>length</code> works on any sequece, of which strings are one. Strings are UTF8 encoded.

```factor>length</lang



## Fantom



### Byte length


A string can be converted into an instance of <code>Buf</code> to treat the string as a sequence of bytes according to a given charset: the default is UTF8, but 16-bit representations can also be used.


```fantom

fansh> c := "møøse"
møøse
fansh> c.toBuf.size   // find the byte length of the string in default (UTF8) encoding
7
fansh> c.toBuf.toHex  // display UTF8 representation
6dc3b8c3b87365
fansh> c.toBuf(Charset.utf16LE).size    // byte length in UTF16 little-endian
10
fansh> c.toBuf(Charset.utf16LE).toHex   // display as UTF16 little-endian
6d00f800f80073006500
fansh> c.toBuf(Charset.utf16BE).size    // byte length in UTF16 big-endian
10
fansh> c.toBuf(Charset.utf16BE).toHex   // display as UTF16 big-endian
006d00f800f800730065

```



### Character length



```fantom

fansh> c := "møøse"
møøse
fansh> c.size
5

```



## Forth

### Byte Length

Strings in Forth come in two forms, neither of which are the null-terminated form commonly used in the C standard library.

'''Counted string'''

A counted string is a single pointer to a short string in memory. The string's first byte is the count of the number of characters in the string. This is how symbols are stored in a Forth dictionary.


```forth
CREATE s ," Hello world" \ create string "s"
s C@ ( -- length=11 )
s COUNT  ( addr len )   \ convert to a stack string, described below
```


'''Stack string'''

A string on the stack is represented by a pair of cells: the address of the string data and the length of the string data (in characters). The word '''COUNT''' converts a counted string into a stack string. The STRING utility wordset of ANS Forth works on these addr-len pairs. This representation has the advantages of not requiring null-termination, easy representation of substrings, and not being limited to 255 characters.


```forth
S" string" ( addr len)
DUP .   \ 6
```



### Character Length

The 1994 ANS standard does not have any notion of a particular character encoding, although it distinguishes between character and machine-word addresses. (There is some ongoing work on standardizing an "XCHAR" wordset for dealing with strings in particular encodings such as UTF-8.)

The following code will count the number of UTF-8 characters in a null-terminated string. It relies on the fact that all bytes of a UTF-8 character except the first have the the binary bit pattern "10xxxxxx".


```forth
2 base !
: utf8+ ( str -- str )
  begin
    char+
    dup c@
    11000000 and
    10000000 <>
  until ;
decimal
```



```forth
: count-utf8 ( zstr -- n )
  0
  begin
    swap dup c@
  while
    utf8+
    swap 1+
  repeat drop ;
```



## Fortran

Fortran 77 introduced variables of type CHARACTER and associated syntax. These are fixed-size entities, declared at compile time as in <code>CHARACTER*66 TEXT</code>, however a subroutine (or function) receiving such a variable could declare it as <code>CHARACTER*(*) TEXT</code> so that any size may be supplied to the routine, and with F90 came the ability within subroutines (or functions) to declare items of a size determined at run time. There is no associated length variable, as with strings that have both a content ''and'' a length, nor is there a special character value (such as zero) deemed to mark the end-of-text in such a variable to give string-like facilities. However, with F90 came facilities, standardised in F2003 whereby a CHARACTER variable could be re-allocated exactly the right amount of storage whenever it was assigned to. So, <code>TEXT = "this"</code> would cause TEXT to become a CHARACTER variable of length four, adjusted so at run time. Again, the length information is not associated with the variable itself, for instance as the content of a character zero prefixing the content to enable strings of a length up to 255. The length information must be stored somewhere...

Previously, character data would be stored in arithmetic variables, using format codes such as <code>A1</code> to store one character per variable, which might be an integer or a floating-point variable of much larger size. Format <code>A2</code> would store two such characters, and so on. Code A1 would give ease of manipulation, while A8 (say for a REAL*8 variable) would save space. Numerical values would be strange, and word sizes may not be a multiple of eight bits nor character encodements require eight bits, especially on a decimal computer such as the IBM1620 where storage usage was counted in digits, and a character required two.

An intrinsic function LEN(text) reports the number of characters in the variable (with no consideration of any storage needed anywhere to hold the length), while SIZE(array) reports the number of elements in an array and SIZEOF(''x'') may be available to report the number of bytes of storage of ''x''. Since these days, everyone uses computers with eight-bit characters and this is deemed universal, the result from LEN will be equivalent to both a byte and a character count.

There is no facility for fancy Unicode schemes, other than by writing suitable routines. In that regard, plotting packages often supply a special function that returns the length of a text string, ''as it would appear on the plot, in plotting units'', especially useful when the plotter's rendition of text employs a proportionally-spaced typeface and interprets superscripts and subscripts and so forth, so that the programmer can prepare code to juggle with the layout, perhaps of mathematical expressions. This is of course not in any standard.


### Byte Length

LEN(text)

### Character Length

LEN(text)


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim s As String      = "moose"  '' variable length ascii string
Dim f As String  * 5 = "moose"  '' fixed length ascii string (in practice a zero byte is appended)
Dim z As ZString * 6 = "moose"  '' fixed length zero terminated ascii string
Dim w As WString * 6 = "møøse"  '' fixed length zero terminated unicode string

' Variable length strings have a descriptor consisting of 3 Integers (12 bytes on 32 bit, 24 bytes on 64 bit systems)
' In order, the descriptor contains the address of the data, the memory currently used and the memory allocated

' In Windows, WString uses UCS-2 encoding (i.e. 2 bytes per character, surrogates are not supported)
' In Linux,   WString uses UCS-4 encoding (i.e. 4 bytes per character)

' The Len function always returns the length of the string in characters
' The SizeOf function returns the bytes used (by the descriptor in the case of variable length strings)

Print "s : " ; s, "Character Length : "; Len(s), "Byte Length : "; Len(s); "  (data)"
Print "s : " ; s, "Character Length : "; Len(s), "Byte Length : "; SizeOf(s); " (descriptor)"
Print "f : " ; f, "Character Length : "; Len(s), "Byte Length : "; SizeOf(f)
Print "z : " ; z, "Character Length : "; Len(s), "Byte Length : "; SizeOf(z)
Print "w : " ; w, "Character Length : "; Len(s), "Byte Length : "; SizeOf(w)
Print
Sleep
```


```txt

s : moose     Character Length :  5       Byte Length :  5  (data)
s : moose     Character Length :  5       Byte Length :  24 (descriptor)
f : moose     Character Length :  5       Byte Length :  6
z : moose     Character Length :  5       Byte Length :  6
w : møøse     Character Length :  5       Byte Length :  12

```



## Frink


### Byte Length

A string can be converted to an array of bytes in any supported encoding.

```frink

b = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢"
length[stringToBytes[b, "UTF-8"]]

```



### Character Length

Frink's string operations correctly handle upper-plane Unicode characters as a single codepoint.

```frink

b = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢"
length[b]

```



### Grapheme Length


```frink

b = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢"
graphemeLength[b]

```



## GAP


```gap
Length("abc");
# or same result with
Size("abc");
```



## Gnuplot


### Byte Length


```gnuplot
print strlen("hello")
=> 5
```



## Go


### =Byte Length=


```go
package main

import "fmt"

func main() {
    m := "møøse"
    u := "𝔘𝔫𝔦𝔠𝔬𝔡𝔢"
    j := "J̲o̲s̲é̲"
    fmt.Printf("%d %s % x\n", len(m), m, m)
    fmt.Printf("%d %s %x\n", len(u), u, u)
    fmt.Printf("%d %s % x\n", len(j), j, j)
}
```

Output:

```txt

7 møøse 6d c3 b8 c3 b8 73 65
28 𝔘𝔫𝔦𝔠𝔬𝔡𝔢 f09d9498f09d94abf09d94a6f09d94a0f09d94acf09d94a1f09d94a2
14 J̲o̲s̲é̲ 4a cc b2 6f cc b2 73 cc b2 65 cc 81 cc b2

```


### =Character Length=


```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    m := "møøse"
    u := "𝔘𝔫𝔦𝔠𝔬𝔡𝔢"
    j := "J̲o̲s̲é̲"
    fmt.Printf("%d %s %x\n", utf8.RuneCountInString(m), m, []rune(m))
    fmt.Printf("%d %s %x\n", utf8.RuneCountInString(u), u, []rune(u))
    fmt.Printf("%d %s %x\n", utf8.RuneCountInString(j), j, []rune(j))
}
```

Output:

```txt

5 møøse [6d f8 f8 73 65]
7 𝔘𝔫𝔦𝔠𝔬𝔡𝔢 [1d518 1d52b 1d526 1d520 1d52c 1d521 1d522]
9 J̲o̲s̲é̲ [4a 332 6f 332 73 332 65 301 332]

```


### Grapheme Length

Go does not have language or library features to recognize graphemes directly.  For example, it does not provide functions implementing [http://www.unicode.org/reports/tr29/ Unicode Standard Annex #29, Unicode Text Segmentation].  It does however have convenient functions for recognizing Unicode character categories, and so an expected subset of grapheme possibilites is easy to recognize.  Here is a solution recognizing the category "Mn", which includes the combining characters used in the task example.

```go
package main

import (
    "fmt"
    "unicode"
    "unicode/utf8"
)

func main() {
    m := "møøse"
    u := "𝔘𝔫𝔦𝔠𝔬𝔡𝔢"
    j := "J̲o̲s̲é̲"
    fmt.Printf("%d %s %x\n", grLen(m), m, []rune(m))
    fmt.Printf("%d %s %x\n", grLen(u), u, []rune(u))
    fmt.Printf("%d %s %x\n", grLen(j), j, []rune(j))
}

func grLen(s string) int {
    if len(s) == 0 {
        return 0
    }
    gr := 1
    _, s1 := utf8.DecodeRuneInString(s)
    for _, r := range s[s1:] {
        if !unicode.Is(unicode.Mn, r) {
            gr++
        }
    }
    return gr
}
```

Output:

```txt

5 møøse [6d f8 f8 73 65]
7 𝔘𝔫𝔦𝔠𝔬𝔡𝔢 [1d518 1d52b 1d526 1d520 1d52c 1d521 1d522]
4 J̲o̲s̲é̲ [4a 332 6f 332 73 332 65 301 332]

```



## Groovy

Calculating "Byte-length" (by which one typically means "in-memory storage size in bytes") is not possible through the facilities of the Groovy language alone. Calculating "Character length" is built into the Groovy extensions to java.lang.String.

### Character Length


```groovy
println "Hello World!".size()
```


Output:

```txt
12
```


Note: The Java "String.length()" method also works in Groovy, but "size()" is consistent with usage in other sequential or composite types.

=={{header|GW-BASIC}}==
GW-BASIC only supports single-byte characters.


```qbasic
10 INPUT A$
20 PRINT LEN(A$)
```



## Haskell


### Byte Length

It is not possible to determine the "byte length" of an ordinary string, because in Haskell, a string is a boxed list of unicode characters. So each character in a string is represented as whatever the compiler considers as the most efficient representation of a cons-cell and a unicode character, and not as a byte.

For efficient storage of sequences of bytes, there's ''Data.ByteString'', which uses ''Word8'' as a base type. Byte strings have an additional ''Data.ByteString.Char8'' interface, which will truncate each Unicode ''Char'' to 8 bits as soon as it is converted to a byte string. However, this is not adequate for the task, because truncation simple will garble characters other than Latin-1, instead of encoding them into UTF-8, say.

There are several (non-standard, so far) Unicode encoding libraries available on [http://hackage.haskell.org/ Hackage]. As an example, we'll use [http://hackage.haskell.org/packages/archive/encoding/0.2/doc/html/Data-Encoding.html encoding-0.2], as ''Data.Encoding'':


```haskell
import Data.Encoding
import Data.ByteString as B

strUTF8  :: ByteString
strUTF8  = encode UTF8  "Hello World!"

strUTF32 :: ByteString
strUTF32 = encode UTF32 "Hello World!"

strlenUTF8  = B.length strUTF8
strlenUTF32 = B.length strUTF32
```


### Character Length

The base type ''Char'' defined by the standard is already intended for (plain) Unicode characters.


```haskell
strlen = length "Hello, world!"
```



## HicEst


```hicest
LEN("1 character == 1 byte") ! 21
```

=={{header|Icon}} and {{header|Unicon}}==

### = Character Length =


```Icon
   length := *s
```


Note: Neither Icon nor Unicon currently supports double-byte character sets.


## HolyC


### Byte Length


```holyc
U8 *string = "Hello, world!";
Print("%d\n", StrLen(string));

```



## IDL


### Byte Length

'''Compiler:''' any IDL compiler should do


```idl
length = strlen("Hello, world!")
```


### Character Length

```idl
length = strlen("Hello, world!")
```



## Io


### Byte Length


```io
"møøse" sizeInBytes
```



### Character Length


```io
"møøse" size
```



## J


### Byte Length


```j
   #     'møøse'
7
```

Here we use the default encoding for character literals (8 bit wide literals).

### Character Length


```j
   #7 u: 'møøse'
5
```

Here we have used 16 bit wide character literals.  See also the dictionary page for [http://www.jsoftware.com/help/dictionary/duco.htm u:].


## Java


### Byte Length

Java encodes strings in UTF-16, which represents each character with one or two 16-bit values.

Another way to know the byte length of a string -who cares- is to explicitly specify the charset we desire.


```java5
String s = "Hello, world!";
int byteCountUTF16 = s.getBytes("UTF-16").length; // Incorrect: it yields 28 (that is with the BOM)
int byteCountUTF16LE = s.getBytes("UTF-16LE").length; // Correct: it yields 26
int byteCountUTF8  = s.getBytes("UTF-8").length; // yields 13
```



### Character Length

Java encodes strings in UTF-16, which represents each character (''code point'') with one or two 16-bit ''code units''. This is a variable-length encoding scheme. The most commonly used characters are represented by one 16-bit code unit, while rarer ones like some mathematical symbols are represented by two.

The length method of String objects is not the length of that String in characters.  Instead, it only gives the number of 16-bit code units used to encode a string. This is not (always) the number of Unicode characters (code points) in the string.

```java5
String s = "Hello, world!";
int not_really_the_length = s.length(); // XXX: does not (always) count Unicode characters (code points)!
```


Since Java 1.5, the actual number of characters (code points) can be determined by calling the codePointCount method.

```java5
String str = "\uD834\uDD2A"; //U+1D12A
int not_really__the_length = str.length(); // value is 2, which is not the length in characters
int actual_length = str.codePointCount(0, str.length()); // value is 1, which is the length in characters
```


### Grapheme Length


```java
import java.text.BreakIterator;

public class Grapheme {
  public static void main(String[] args) {
    printLength("møøse");
    printLength("𝔘𝔫𝔦𝔠𝔬𝔡𝔢");
    printLength("J̲o̲s̲é̲");
  }

  public static void printLength(String s) {
    BreakIterator it = BreakIterator.getCharacterInstance();
    it.setText(s);
    int count = 0;
    while (it.next() != BreakIterator.DONE) {
      count++;
    }
    System.out.println("Grapheme length: " + count+ " " + s);
  }
}
```

Output:

```txt

Grapheme length: 5 møøse
Grapheme length: 7 𝔘𝔫𝔦𝔠𝔬𝔡𝔢
Grapheme length: 4 J̲o̲s̲é̲

```



## JavaScript


### Byte Length

JavaScript encodes strings in UTF-16, which represents each character with one or two 16-bit values. The length property of string objects gives the number of 16-bit values used to encode a string, so the number of bytes can be determined by doubling that number.


```javascript
var s = "Hello, world!";
var byteCount = s.length * 2; //26
```


### Character Length

JavaScript encodes strings in UTF-16, which represents each character with one or two 16-bit values. The most commonly used characters are represented by one 16-bit value, while rarer ones like some mathematical symbols are represented by two.

JavaScript has no built-in way to determine how many characters are in a string. However, if the string only contains commonly used characters, the number of characters will be equal to the number of 16-bit values used to represent the characters.

```javascript
var str1 = "Hello, world!";
var len1 = str1.length; //13

var str2 = "\uD834\uDD2A"; //U+1D12A represented by a UTF-16 surrogate pair
var len2 = str2.length; //2
```


### ES6 destructuring/iterators

ES6 provides several ways to get a string split into an array of code points instead of UTF-16 code units:

```javascript
let
  str='AöЖ€𝄞'
 ,countofcodeunits=str.length // 6
 ,cparr=[...str],
 ,countofcodepoints=cparr.length; // 5
{ let
    count=0
  for(let codepoint of str)
    count++
  countofcodepoints=count // 5
}
{ let
    count=0,
    it=str[Symbol.iterator]()
  while(!it.next().done)
    count++
  countofcodepoints=count // 5
}
{ cparr=Array.from(str)
  countofcodepoints=cparr.length // 5
}

```



## jq

jq strings are JSON strings and are therefore encoded as UTF-8. When given a JSON string, the <tt>length</tt> filter emits the number of Unicode codepoints that it contains:

```jq
$ cat String_length.jq
def describe:
   "length of \(.) is \(length)";

("J̲o̲s̲é̲", "𝔘𝔫𝔦𝔠𝔬𝔡𝔢") | describe
```

```sh

$ jq -n -f String_length.jq
"length of J̲o̲s̲é̲ is 8"
"length of 𝔘𝔫𝔦𝔠𝔬𝔡𝔢 is 7"
```



## Kotlin

As in Java, a string in Kotlin is essentially a sequence of UTF-16 encoded characters and the 'length' property simply returns the number of such characters in the string. Surrogates or graphemes are not treated specially for this purpose - they are just represented by the appropriate number of UTF-16 characters.

As each UTF-16 character occupies 2 bytes, it follows that the number of bytes occupied by the string will be twice the length:

```scala
// version 1.0.6
fun main(args: Array<String>) {
    val s = "José"
    println("The char length is ${s.length}")
    println("The byte length is ${Character.BYTES * s.length}")
}
```


```txt

The char length is 4
The byte length is 8

```



## Julia

Julia encodes strings as UTF-8, so the byte length (via <code>sizeof</code>) will be different from the string length (via <code>length</code>) only if the string contains non-ASCII characters.


### Byte Length


```julia
sizeof("Hello, world!") # gives 13
sizeof("Hellö, wørld!") # gives 15
```



### Character Length


```julia
length("Hello, world!") # gives 13
length("Hellö, wørld!") # gives 13
```



## JudoScript


### Byte Length

```judoscript
//Store length of hello world in length and print it
. length = "Hello World".length();
```


### Character Length

```judoscript
//Store length of hello world in length and print it
. length = "Hello World".length()
```



## K


### Character Length


```K

    #"Hello, world!"
13
    #"Hëllo, world!"
13

```



## LabVIEW


### Byte Length

LabVIEW is using a special variant of UTF-8, so byte length == character length.



### Character Length


[[File:LV strlen.png]]



## Lasso


### Character Length


```Lasso
'Hello, world!'->size // 13
'møøse'->size // 5
'𝔘𝔫𝔦𝔠𝔬𝔡𝔢'->size // 7
```



### Byte Length


```Lasso
'Hello, world!'->asBytes->size // 13
'møøse'->asBytes->size // 7
'𝔘𝔫𝔦𝔠𝔬𝔡𝔢'->asBytes->size // 28
```



## LFE



###  Character Length



```lisp

(length "ASCII text")
10
(length "𝔘𝔫𝔦𝔠𝔬𝔡𝔢 𝔗𝔢𝒙𝔱")
12
> (set encoded (binary ("𝔘𝔫𝔦𝔠𝔬𝔡𝔢 𝔗𝔢𝒙𝔱" utf8)))
#B(240 157 148 152 240 157 148 171 240 157 ...)
> (length (unicode:characters_to_list encoded 'utf8))
12

```



###  Byte Length



```lisp

> (set encoded (binary ("𝔘𝔫𝔦𝔠𝔬𝔡𝔢 𝔗𝔢𝒙𝔱" utf8)))
#B(240 157 148 152 240 157 148 171 240 157 ...)
> (byte_size encoded)
45
> (set bytes (binary ("𝔘𝔫𝔦𝔠𝔬𝔡𝔢 𝔗𝔢𝒙𝔱")))
#B(24 43 38 32 44 33 34 32 23 34 153 49)
> (byte_size bytes)
12
> (set encoded (binary ("ASCII text" utf8)))
#B(65 83 67 73 73 32 116 101 120 116)
> (byte_size encoded)
10

```



## Liberty BASIC

See BASIC


## Lingo


### Character Length


```lingo
utf8Str = "Hello world äöü"
put utf8Str.length
-- 15
```


### Byte Length


```lingo
utf8Str = "Hello world äöü"
put bytearray(utf8Str).length
-- 18
```



## Logo

Logo is so old that only ASCII encoding is supported. Modern versions of Logo may have enhanced character set support.

```logo
print count "|Hello World|  ; 11
print count "møøse            ; 5
print char 248   ; ø - implies ISO-Latin character set
```



## LSE64


### Byte Length

LSE stores strings as arrays of characters in 64-bit cells plus a count.

```lse64
" Hello world" @ 1 + 8 * ,   # 96 = (11+1)*(size of a cell) = 12*8
```


### Character Length

LSE uses counted strings: arrays of characters, where the first cell contains the number of characters in the string.

```lse64
" Hello world" @ ,   # 11
```



## Lua

In Lua, a character is always the size of one byte so there is no difference between byte length and character length.

### Byte Length


```lua
str = "Hello world"
length = #str
```


or


```lua
str = "Hello world"
length = string.len(str)
```



### Character Length


```lua
str = "Hello world"
length = #str
```


or


```lua
str = "Hello world"
length = string.len(str)
```



## M2000 Interpreter


```M2000 Interpreter

A$=format$("J\u0332o\u0332s\u0332e\u0301\u0332")
Print Len(A$) = 9  ' true Utf-16LE
Print Len.Disp(A$) = 4 \\ display length
Buffer Clear Mem as Byte*100
\\ Write at memory at offset 0 or address Mem(0)
Return Mem, 0:=A$
Print Eval$(Mem, 0, 18)
For i=0 to 17 step 2
      \\ print hex value and character
      Hex Eval(Mem, i as integer), ChrCode$(Eval(Mem, i as integer))
Next i
Document B$=A$
\\ encode to utf-8 with BOM (3 bytes 0xEF,0xBB,0xBF)
Save.Doc B$, "Checklen.doc", 2
Print Filelen("Checklen.doc")=17
\\ So length is 14 bytes + 3 the BOM

```



## Maple


###  Character length


```maple
length("Hello world");
```


###  Byte count


```maple
nops(convert("Hello world",bytes));
```



## Mathematica


###  Character length


```mathematica
StringLength["Hello world"]
```


###  Byte length


```mathematica
StringByteCount["Hello world"]
```



## MATLAB


### Character Length


```MATLAB>>
 length('møøse')

ans =

     5
```


### Byte Length

MATLAB apparently encodes strings using UTF-16.

```MATLAB>>
 numel(dec2hex('møøse'))

ans =

    10
```



## Maxima


```maxima
s: "the quick brown fox jumps over the lazy dog";
slength(s);
/* 43 */
```



## MAXScript


### Character Length


```maxscript
"Hello world".count
```




## Mercury

Mercury's C and Erlang backends use UTF-8 encoded strings; the Java and C# backends using the
underlying UTF-16 encoding of those languages.  The function <tt>string.length/1</tt> returns
the number of code units in a string in target language encoding.  The function
<tt>string.count_utf8_code_units/1</tt> returns the number of UTF-8 code units in a string
regardless of the target language.


### Byte Length


```mercury
:- module string_byte_length.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, string.

main(!IO) :-
    Words = ["møøse", "𝔘𝔫𝔦𝔠𝔬𝔡𝔢", "J\x332\o\x332\s\x332\e\x301\\x332\"],
    io.write_list(Words, "", write_length, !IO).

:- pred write_length(string::in, io::di, io::uo) is det.

write_length(String, !IO):-
    NumBytes = count_utf8_code_units(String),
    io.format("%s: %d bytes\n", [s(String), i(NumBytes)], !IO).
```


Output:

```txt

møøse: 7 bytes
𝔘𝔫𝔦𝔠𝔬𝔡𝔢: 28 bytes
J̲o̲s̲é̲: 14 bytes

```



### Character Length

The function <tt>string.count_codepoints/1</tt> returns the number of code points in a string.

```mercury
:- module string_character_length.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, string.

main(!IO) :-
    Words = ["møøse", "𝔘𝔫𝔦𝔠𝔬𝔡𝔢", "J\x332\o\x332\s\x332\e\x301\\x332\"],
    io.write_list(Words, "", write_length, !IO).

:- pred write_length(string::in, io::di, io::uo) is det.

write_length(String, !IO) :-
    NumChars = count_codepoints(String),
    io.format("%s: %d characters\n", [s(String), i(NumChars)], !IO).
```


Output:

```txt

møøse: 5 characters
𝔘𝔫𝔦𝔠𝔬𝔡𝔢: 7 characters
J̲o̲s̲é̲: 9 characters

```



## Metafont


Metafont has no way of handling properly encodings different from ASCII. So it is able to count only the number of bytes in a string.


```metafont
string s;
s := "Hello Moose";
show length(s);          % 11 (ok)
s := "Hello Møøse";
show length(s);          % 13 (number of bytes when the string is UTF-8 encoded,
                         % since ø takes two bytes)
```


'''Note''': in the lang tag, Møøse is Latin1-reencoded, showing up two bytes (as Latin1) instead of one


## MIPS Assembly

This only supports ASCII encoding, so it'll return both byte length and char length.

```mips

.data
	#.asciiz automatically adds the NULL terminator character, \0 for us.
	string: .asciiz "Nice string you got there!"

.text
main:
	la $a1,string           #load the beginning address of the string.

loop:
	lb $a2,($a1)            #load byte (i.e. the char) at $a1 into $a2
	addi $a1,$a1,1          #increment $a1
	beqz $a2,exit_procedure #see if we've hit the NULL char yet
	addi $a0,$a0,1          #increment counter
	j loop                  #back to start

exit_procedure:
	li $v0,1                #set syscall to print integer
	syscall

	li $v0,10               #set syscall to cleanly exit EXIT_SUCCESS
	syscall

```



## mIRC Scripting Language


### Byte Length

```mirc
alias stringlength { echo -a Your Name is: $len($$?="Whats your name") letters long! }
```


### Character Length

''$utfdecode()'' converts an UTF-8 string to the locale encoding, with unrepresentable characters as question marks. Since mIRC is not yet fully Unicode aware, entering Unicode text trough a dialog box will automatically convert it to ASCII.

```mirc
alias utf8len { return $len($utfdecode($1)) }
alias stringlength2 {
  var %name = Børje
  echo -a %name is: $utf8len(%name) characters long!
}
```


=={{header|Modula-3}}==

### Byte Length


```modula3
MODULE ByteLength EXPORTS Main;

IMPORT IO, Fmt, Text;

VAR s: TEXT := "Foo bar baz";

BEGIN
  IO.Put("Byte length of s: " & Fmt.Int((Text.Length(s) * BYTESIZE(s))) & "\n");
END ByteLength.
```


### Character Length


```modula3
MODULE StringLength EXPORTS Main;

IMPORT IO, Fmt, Text;

VAR s: TEXT := "Foo bar baz";

BEGIN
  IO.Put("String length of s: " & Fmt.Int(Text.Length(s)) & "\n");
END StringLength.
```



## NewLISP


### Character Length


```NewLISP
(set 'Str "møøse")
(println  Str  " is " (length Str) " characters long")
```



## Nemerle

Both examples rely on .Net faculties, so they're almost identical to C#

### Character Length


```Nemerle
def message = "How long am I anyways?";
def charlength = message.Length;
```



### Byte Length


```Nemerle
using System.Text;

def message = "How long am I anyways?";
def bytelength = Encoding.Unicode.GetByteCount(message);
```



## Nim


### Byte Length


```Nim
var s: string = "Hello, world! ☺"
echo '"',s, '"'," has byte length: ", len(s)

# ->  "Hello, world! ☺" has unicode char length: 17
```



### Character Length


```Nim
import unicode

var s: string = "Hello, world! ☺"
echo '"',s, '"'," has unicode char length: ", runeLen(s)

# ->  "Hello, world! ☺" has unicode char length: 15
```


=={{header|Oberon-2}}==


### Byte Length


```oberon2
MODULE Size;

   IMPORT Out;

   VAR s: LONGINT;
      string: ARRAY 5 OF CHAR;

BEGIN
   string := "Foo";
   s := LEN(string);
   Out.String("Size: ");
   Out.LongInt(s,0);
   Out.Ln;
END Size.
```


Output:

```txt

Size: 5

```



### Character Length


```oberon2
MODULE Length;

   IMPORT Out, Strings;

   VAR l: INTEGER;
      string: ARRAY 5 OF CHAR;

BEGIN
   string := "Foo";
   l := Strings.Length(string);
   Out.String("Length: ");
   Out.Int(l,0);
   Out.Ln;
END Length.
```


Output:

```txt

Length: 3

```


=={{header|Objective-C}}==
In order to be not ambiguous about the encoding used in the string, we explicitly provide
it in UTF-8 encoding. The string is "møøse" (ø UTF-8 encoded is in hexadecimal C3 B8).


### Character Length

Objective-C encodes strings in UTF-16, which represents each character (''code point'') with one or two 16-bit ''code units''. This is a variable-length encoding scheme. The most commonly used characters are represented by one 16-bit code unit, while "supplementary characters" are represented by two (called a "surrogate pair").

The length method of NSString objects is not the length of that string in characters.  Instead, it only gives the number of 16-bit code units used to encode a string. This is not (always) the number of Unicode characters (code points) in the string.

```objc
// Return the length in characters
// XXX: does not (always) count Unicode characters (code points)!
unsigned int numberOfCharacters = [@"møøse" length];  // 5
```


Since Mac OS X 10.6, CFString has methods for converting between supplementary characters and surrogate pair. However, the easiest way to get the number of characters is probably to encode it in UTF-32 (which is a fixed-length encoding) and divide by 4:

```objc
int realCharacterCount = [s lengthOfBytesUsingEncoding: NSUTF32StringEncoding] / 4;
```



### Byte Length

Objective-C encodes strings in UTF-16, which represents each character with one or two 16-bit values. The length method of NSString objects returns the number of 16-bit values used to encode a string, so the number of bytes can be determined by doubling that number.


```objc
int byteCount = [@"møøse" length] * 2; // 10
```


Another way to know the byte length of a string is to explicitly specify the charset we desire.


```objc
// Return the number of bytes depending on the encoding,
// here explicitly UTF-8
unsigned numberOfBytes =
   [@"møøse" lengthOfBytesUsingEncoding: NSUTF8StringEncoding]; // 7
```



## Objeck

All character string elements are 1-byte in size therefore a string's byte size and length are the same.


### Character Length


```objeck

"Foo"->Size()->PrintLine();

```



### Byte Length


```objeck

"Foo"->Size()->PrintLine();

```



## OCaml


In OCaml currently, characters inside the standard type string are bytes, and a single character taken alone has the same binary representation as the OCaml int (which is equivalent to a C long) which is a machine word.

For internationalization there is [https://github.com/yoriyuki/Camomile Camomile], a comprehensive Unicode library for OCaml. Camomile provides Unicode character type, UTF-8, UTF-16, and more...


### Byte Length


Standard OCaml strings are classic ASCII ISO 8859-1, so the function String.length returns the byte length which is the character length in this encoding:

```ocaml
String.length "Hello world" ;;
```



### Character Length


While using the '''UTF8''' module of ''Camomile'' the byte length of an utf8 encoded string will be get with <tt>String.length</tt> and the character length will be returned by <tt>UTF8.length</tt>:

```ocaml
open CamomileLibrary

let () =
  Printf.printf " %d\n" (String.length "møøse");
  Printf.printf " %d\n" (UTF8.length "møøse");
;;
```


Run this code with the command:

```txt

$ ocaml bigarray.cma -I $(ocamlfind query camomile)/library/ camomileLibrary.cma strlen.ml
 7
 5

```



## Octave


```octave
s = "string";
stringlen = length(s)
```


This gives the number of bytes, not of characters. e.g. length("è") is 2 when "è" is encoded e.g. as UTF-8.



## Oforth


Oforth strings are UTF8 encoded.

size method returns number of UTF8 characters into a string

basicSize method returns number of bytes into a string


## Ol


```scheme

; Character length
(print (string-length "Hello, wørld!"))
; ==> 13

; Byte (utf-8 encoded) length
(print (length (string->bytes "Hello, wørld!")))
; ==> 14

```



## OpenEdge/Progress

The codepage can be set independently for input / output and internal operations. The following examples are started from an iso8859-1 session and therefore need to use fix-codepage to adjust the string to utf-8.


### Character Length


```progress
DEF VAR lcc AS LONGCHAR.

FIX-CODEPAGE( lcc ) = "UTF-8".
lcc = "møøse".

MESSAGE LENGTH( lcc ) VIEW-AS ALERT-BOX.
```


### Byte Length


```progress
DEF VAR lcc AS LONGCHAR.

FIX-CODEPAGE( lcc ) = "UTF-8".
lcc = "møøse".

MESSAGE LENGTH( lcc, "RAW" ) VIEW-AS ALERT-BOX.
```



## Oz


### Byte Length


```oz
{Show {Length "Hello World"}}
```

Oz uses a single-byte encoding by default. So for normal strings, this will also show the correct character length.


## PARI/GP


### Character Length

Characters = bytes in Pari; the underlying strings are C strings interpreted as US-ASCII.

```parigp
len(s)=#s; \\ Alternately, len(s)=length(s); or even len=length;
```


### Byte Length

This works on objects of any sort, not just strings, and includes overhead.

```parigp
len(s)=sizebyte(s);
```



## Pascal


### Byte Length


```pascal

const
  s = 'abcdef';
begin
  writeln (length(s))
end.

```

Output:

```txt

6

```



## Perl


### Byte Length

Strings in Perl consist of characters. Measuring the byte length therefore requires conversion to some binary representation (called encoding, both noun and verb).


```perl
use utf8; # so we can use literal characters like ☺ in source
use Encode qw(encode);

print length encode 'UTF-8', "Hello, world! ☺";
# 17. The last character takes 3 bytes, the others 1 byte each.

print length encode 'UTF-16', "Hello, world! ☺";
# 32. 2 bytes for the BOM, then 15 byte pairs for each character.
```



### Character Length

```perl
my $length = length "Hello, world!";
```



### Grapheme Length

Since Perl 5.12, <code>/\X/</code> matches an ''extended grapheme cluster''. See [http://perldoc.perl.org/perl5120delta.html#Unicode-overhaul "Unicode overhaul" in perl5120delta] and also [http://www.unicode.org/reports/tr29/ UAX #29].

Perl understands that "\x{1112}\x{1161}\x{11ab}\x{1100}\x{1173}\x{11af}" (한글) contains 2 graphemes, just like "\x{d55c}\x{ae00}" (한글). The longer string uses Korean combining jamo characters.

```perl
use v5.12;
my $string = "\x{1112}\x{1161}\x{11ab}\x{1100}\x{1173}\x{11af}";  # 한글
my $len;
$len++ while ($string =~ /\X/g);
printf "Grapheme length: %d\n", $len;
```


```txt
Grapheme length: 2
```



## Perl 6


### Byte Length



```perl6
say 'møøse'.encode('UTF-8').bytes;
```



### Character Length



```perl6
say 'møøse'.codes;
```



### Grapheme Length



```perl6
say 'møøse'.chars;
```



## Phix

The standard length function returns the number of bytes, character length is achieved by converting to utf32

```Phix
constant s = "𝔘𝔫𝔦𝔠𝔬𝔡𝔢"
?length(s)
?length(utf8_to_utf32(s))
```

```txt

28
7

```



## PHP

Program in a UTF8 linux:

```PHP
<?php
foreach (array('møøse', '𝔘𝔫𝔦𝔠𝔬𝔡𝔢', 'J̲o̲s̲é̲') as $s1) {
   printf('String "%s" measured with strlen: %d mb_strlen: %s grapheme_strlen %s%s',
                  $s1, strlen($s1),mb_strlen($s1), grapheme_strlen($s1), PHP_EOL);
}

```

yields the result:

```txt

String "møøse" measured with strlen: 7 mb_strlen: 7 grapheme_strlen 5
String "𝔘𝔫𝔦𝔠𝔬𝔡𝔢" measured with strlen: 28 mb_strlen: 28 grapheme_strlen 7
String "J̲o̲s̲é̲" measured with strlen: 13 mb_strlen: 13 grapheme_strlen 4

```



## PicoLisp


```PicoLisp
(let Str "møøse"
   (prinl "Character Length of \"" Str "\" is " (length Str))
   (prinl "Byte Length of \"" Str "\" is " (size Str)) )
```

Output:

```txt
Character Length of "møøse" is 5
Byte Length of "møøse" is 7
-> 7
```



## PL/I


```pli
declare WS widechar (13) initial ('Hello world.');
put ('Character length=', length (WS));
put skip list ('Byte length=', size(WS));

declare SM graphic (13) initial ('Hello world');
put ('Character length=', length(SM));
put skip list ('Byte length=', size(trim(SM)));
```



## PL/SQL

LENGTH calculates length using characters as defined by the input character set.
LENGTHB uses bytes instead of characters.
LENGTHC uses Unicode complete characters.
LENGTH2 uses UCS2 code points.
LENGTH4 uses UCS4 code points.

### Byte Length


```plsql
DECLARE
  string VARCHAR2(50) := 'Hello, world!';
  stringlength NUMBER;
BEGIN
  stringlength := LENGTHB(string);
END;
```



### Character Length


```plsql
DECLARE
  string VARCHAR2(50) := 'Hello, world!';
  stringlength NUMBER;
  unicodelength NUMBER;
  ucs2length NUMBER;
  ucs4length NUMBER;
BEGIN
  stringlength := LENGTH(string);
  unicodelength := LENGTHC(string);
  ucs2length := LENGTH2(string);
  ucs4length := LENGTH4(string);
END;
```



## Pop11


### Byte Length

Currently Pop11 supports only strings consisting of 1-byte units. Strings can carry arbitrary binary data, so user can for example use UTF-8 (however builtin procedures will treat each byte as a single character). The length function for strings returns length in bytes:


```pop11
lvars str = 'Hello, world!';
lvars len = length(str);
```



## PostScript


### Character Length

<lang>
(Hello World) length =
11

```



## Potion


### Character Length


```potion
"møøse" length print
"𝔘𝔫𝔦𝔠𝔬𝔡𝔢" length print
"J̲o̲s̲é̲" length print
```



## PowerShell


### Character Length


```powershell
$s = "Hëlló Wørłð"
$s.Length
```


### Byte Length

For UTF-16, which is the default in .NET and therefore PowerShell:

```powershell
$s = "Hëlló Wørłð"
[System.Text.Encoding]::Unicode.GetByteCount($s)
```

For UTF-8:

```powershell
[System.Text.Encoding]::UTF8.GetByteCount($s)
```



## PureBasic


### Character Length


```PureBasic
 a = Len("Hello World") ;a will be 11
```



### Byte Length

Returns the number of bytes required to store the string in memory in the given format in bytes. 'Format' can be #PB_Ascii, #PB_UTF8 or #PB_Unicode.  PureBasic code can be compiled using either Unicode (2-byte) or Ascii (1-byte) encodings for strings.  If 'Format' is not specified, the mode of the executable (unicode or ascii) is used.

Note: The number of bytes returned does not include the terminating Null-Character of the string. The size of the Null-Character is 1 byte for Ascii and UTF8 mode and 2 bytes for Unicode mode.


```PureBasic
a = StringByteLength("ä", #PB_UTF8)    ;a will be 2
b = StringByteLength("ä", #PB_Ascii)   ;b will be 1
c = StringByteLength("ä", #PB_Unicode) ;c will be 2

```



## Python


### 2.x

In Python 2.x, there are two types of strings: regular (8-bit) strings, and Unicode strings. Unicode string literals are prefixed with "u".


### =Byte Length=

For 8-bit strings, the byte length is the same as the character length:

```python
print len('ascii')
# 5
```


For Unicode strings, length depends on the internal encoding. Since version 2.2 Python shipped with two build options: it either uses 2 or 4 bytes per character. The internal representation is not interesting for the user.


```python
# The letter Alef
print len(u'\u05d0'.encode('utf-8'))
# 2
print len(u'\u05d0'.encode('iso-8859-8'))
# 1
```


Example from the problem statement:

```python
#!/bin/env python
# -*- coding: UTF-8 -*-
s = u"møøse"
assert len(s) == 5
assert len(s.encode('UTF-8')) == 7
assert len(s.encode('UTF-16-BE')) == 10 # There are 3 different UTF-16 encodings: LE and BE are little endian and big endian respectively, the third one (without suffix) adds 2 extra leading bytes: the byte-order mark (BOM).
```


### =Character Length=

len() returns the number of code units (not code points!) in a Unicode string or plain ASCII string. On a wide build, this is the same as the number of  code points, but on a narrow one it is not. Most linux distributions install the wide build by default, you can check the build at runtime with:


```python
import sys
sys.maxunicode # 1114111 on a wide build, 65535 on a narrow build
```


To get the length of encoded string, you have to decode it first:

```python
print len('ascii')
# 5
print len(u'\u05d0') # the letter Alef as unicode literal
# 1
print len('\xd7\x90'.decode('utf-8')) # Same encoded as utf-8 string
# 1
print hex(sys.maxunicode), len(unichr(0x1F4A9))
# ('0x10ffff', 1)
```


On a narrow build, len() gives the wrong answer for non-BMP chars


```python
print hex(sys.maxunicode), len(unichr(0x1F4A9))
# ('0xffff', 2)
```



### 3.x

In Python 3.x, strings are Unicode strings and a bytes type if available for storing an immutable sequence of bytes (there's also available a bytearray type, which is mutable)


### =Byte Length=



You can use len() to get the length of a byte sequence.


```python
print(len(b'Hello, World!'))
# 13
```


To get a byte sequence from a string, you have to encode it with the desired encoding:


```python
# The letter Alef
print(len('\u05d0'.encode())) # the default encoding is utf-8 in Python3
# 2
print(len('\u05d0'.encode('iso-8859-8')))
# 1
```


Example from the problem statement:

```python
#!/bin/env python
# -*- coding: UTF-8 -*-
s = "møøse"
assert len(s) == 5
assert len(s.encode('UTF-8')) == 7
assert len(s.encode('UTF-16-BE')) == 10 # There are 3 different UTF-16 encodings: LE and BE are little endian and big endian respectively, the third one (without suffix) adds 2 extra leading bytes: the byte-order mark (BOM).
u="𝔘𝔫𝔦𝔠𝔬𝔡𝔢"
assert len(u.encode()) == 28
assert len(u.encode('UTF-16-BE')) == 28
```


### =Character Length=


Since Python3.3 the internal storage of unicode strings has been optimized: strings that don't contain characters outside the latin-1 set, are stored with 8 bits for each character, strings that don't contain codepoints outside the BMP (lone surrogates aren't allowed) are stored as UCS-2, while all the others use UCS-4.

Thus Python is able to avoid memory overhead when dealing with only ASCII strings, while handling correctly all codepoints in Unicode. len() returns the number of characters/codepoints:


```python
print(len("𝔘𝔫𝔦𝔠𝔬𝔡𝔢"))
# 7
```


Until Python 3.2 instead, length depended on the internal encoding, since it shipped with two build options: it either used 2 or 4 bytes per character.

len() returned the number of code units in a string, which could be different from the number of characters. In a narrow build, this is not a reliable way to get the number of characters. You can only easily count code points in a wide build. Most linux distributions install the wide build by default, you can check the build at runtime with:


```python
import sys
sys.maxunicode # 1114111 on a wide build, 65535 on a narrow build
```



```python
print(len('ascii'))
# 5
print(len('\u05d0')) # the letter Alef as unicode literal
# 1
```


To get the length of an encoded byte sequence, you have to decode it first:


```python
print(len(b'\xd7\x90'.decode('utf-8'))) # Alef encoded as utf-8 byte sequence
# 1
```



```python
print(hex(sys.maxunicode), len(unichr(0x1F4A9)))
# ('0x10ffff', 1)
```


On a narrow build, len() gives the wrong answer for non-BMP chars


```python
print(hex(sys.maxunicode), len(unichr(0x1F4A9)))
# ('0xffff', 2)
```



## R



### Byte length


```rsplus
a <- "m\u00f8\u00f8se"
print(nchar(a, type="bytes"))  # print 7
```



### Character length


```rsplus
print(nchar(a, type="chars"))  # print 5
```



## Racket


Using this definition:

```Racket
(define str "J\u0332o\u0332s\u0332e\u0301\u0332")
```

on the REPL, we get the following:


### Character length


```Racket
-> (printf "str has ~a characters" (string-length str))
str has 9 characters
```



### Byte length


```Racket
-> (printf "str has ~a bytes in utf-8" (bytes-length (string->bytes/utf-8 str)))
str has 14 bytes in utf-8
```



## REBOL


Rebol 2 does not natively support UCS (Unicode), so character and byte
length are the same. See [http://www.reboltech.com/library/html/utf-8.html utf-8.r] for an external UTF-8 library.

Rebol 3 natively supports UTF-8.


### Byte Length



```REBOL
;; r2
length? "møøse"

;; r3
length? to-binary "møøse"
```



### Character length



```REBOL
;; r3
length? "møøse"
```



## Retro


### Byte Length


```Retro
"møøse" getLength putn
```



### Character Length

Retro does not have built-in support for Unicode, but counting of characters can be done with a small amount of effort.


```Retro
chain: UTF8'
{{
  : utf+ ( $-$ )
    [ 1+ dup @ %11000000 and %10000000 = ] while ;

  : count ( $-$ )
    0 !here
    repeat dup @ 0; drop utf+ here ++ again ;
---reveal---
  : getLength ( $-n )
    count drop @here ;
}}
;chain

"møøse" ^UTF8'getLength putn
```



## REXX

Classic REXX don't support Unicodes, so ''character'' and ''byte'' length are the same.

All characters (in strings) are stored as 8-bit bytes.      Indeed, ''everything'' in REXX

is stored as character strings.

### Byte Length


```REXX
/*REXX program displays the lengths  (in bytes/characters)  for various strings.        */
    /*            1         */                         /*a handy-dandy over/under scale.*/
    /*   123456789012345    */
hello = 'Hello, world!'      ;        say  'the length of HELLO is '   length(hello)
happy = 'Hello, world! ☺'    ;        say  'the length of HAPPY is '   length(happy)
jose  = 'José'               ;        say  'the length of  JOSE is '   length(jose)
nill  = ''                   ;        say  'the length of  NILL is '   length(nill)
null  =                      ;        say  'the length of  NULL is '   length(null)
sum   = 5+1                  ;        say  'the length of   SUM is '   length(sum)
                                                       /*   [↑]  is, of course,  6.     */
                                                       /*stick a fork in it, we're done.*/
```

'''output'''

```txt

length of HELLO is  13
length of HAPPY is  15
length of  JOSE is  4
length of  NILL is  0
length of  NULL is  0
length of   SUM is  1

```



## Ring


### Character Length


```ring

aString = "Welcome to the Ring Programming Language"
aStringSize = len(aString)
see  "Character lenghts : " + aStringSize

```



## Robotic


### Character Length


```robotic

set "$local1" to "Hello world!"
* "String length: &$local1.length&"
end

```


Unfortunately, only character length can be retrieved in this language.


## Ruby


### Byte Length

Since Ruby 1.8.7, [http://www.ruby-doc.org/core-1.9.1/String.html#method-i-bytesize String#bytesize] is the byte length.

```ruby
# -*- coding: utf-8 -*-

puts "あいうえお".bytesize
# => 15
```



### Character Length

Since Ruby 1.9, [http://www.ruby-doc.org/core-1.9.1/String.html#method-i-length String#length] (alias String#size) is the character length. The magic comment, "coding: utf-8", sets the encoding of all string literals in this file.

```ruby
# -*- coding: utf-8 -*-

puts "あいうえお".length
# => 5

puts "あいうえお".size  # alias for length
# => 5
```



### Code Set Independence

The next examples show the '''byte length''' and '''character length''' of "møøse" in different encodings.

<blockquote style="background-color: #ffc;">
To run these programs, you must convert them to different encodings.

* If you use [[Emacs]]: Paste each program into Emacs. The magic comment, like <code>-*- coding: iso-8859-1 -*-</code>, will tell Emacs to save with that encoding.
* If your text editor saves UTF-8: Convert the file before running it. For example:<br /><code>$ ruby -pe '$_.encode!("iso-8859-1", "utf-8")' scratch.rb | ruby</code>
</blockquote>

{| class="wikitable"
! Program
! Output
|-
|
```ruby
# -*- coding: iso-8859-1 -*-
s = "møøse"
puts "Byte length: %d" % s.bytesize
puts "Character length: %d" % s.length
```

|
```txt
Byte length: 5
Character length: 5
```

|-
|
```ruby
# -*- coding: utf-8 -*-
s = "møøse"
puts "Byte length: %d" % s.bytesize
puts "Character length: %d" % s.length
```

|
```txt
Byte length: 7
Character length: 5
```

|-
|
```ruby
# -*- coding: gb18030 -*-
s = "møøse"
puts "Byte length: %d" % s.bytesize
puts "Character length: %d" % s.length
```

|
```txt
Byte length: 11
Character length: 5
```

|}


### Ruby 1.8

The next example works with both Ruby 1.8 and Ruby 1.9. In Ruby 1.8, the strings have no encodings, and String#length is the byte length. In Ruby 1.8, the regular expressions knows three Japanese encodings.

* <code>/./n</code> uses no multibyte encoding.
* <code>/./e</code> uses EUC-JP.
* <code>/./s</code> uses Shift-JIS or Windows-31J.
* <code>/./u</code> uses UTF-8.

Then either <code>string.scan(/./u).size</code> or <code>string.gsub(/./u, ' ').size</code> counts the UTF-8 characters in string.


```ruby
# -*- coding: utf-8 -*-

class String
  # Define String#bytesize for Ruby 1.8.6.
  unless method_defined?(:bytesize)
    alias bytesize length
  end
end

s = "文字化け"
puts "Byte length: %d" % s.bytesize
puts "Character length: %d" % s.gsub(/./u, ' ').size
```



## Run BASIC


```runbasic
input a$
print len(a$)
```



## SAS


```sas
data _null_;
   a="Hello, World!";
   b=length(c);
   put _all_;
run;
```



## Rust


### Byte Length

<lang>
fn main() {
    let s = "文字化け";  // UTF-8
    println!("Byte Length: {}", s.len());
}

```


### Character Length

<lang>
fn main() {
    let s = "文字化け";  // UTF-8
    println!("Character length: {}", s.chars().count());
}

```



## Scheme


### Byte Length

'''string-size''' function is only Gauche function.

```scheme
(string-size "Hello world")
```


```scheme
(bytes-length #"Hello world")
```



### Character Length

'''string-length''' function is in [[R5RS]], [[R6RS]].

```scheme
  (string-length "Hello world")
```



## sed


### Character Length

Sed breaks strings on newline characters, and doesn't include them in the count.
Text is read from standard input e.g. <code>echo "string" | sed -f script.sed</code> or <code>sed -f script.sed file.txt</code> (The solution given would be the contents of a text file <code>script.sed</code> in these cases).
For files with more than one line, sed will give a count for each line.
The 'convert to digits' section is based off of [http://unix.stackexchange.com/a/36959/11750 this StackExchange answer].

```sed
# Change all characters to '|'.
s/./\|/g;

# Convert to digits
:convert
s/||||||||||/</g
s/<\([0-9]*\)$/<0\1/g
s/|||||||||/9/g;
s/|||||||||/9/g; s/||||||||/8/g; s/|||||||/7/g; s/||||||/6/g;
s/|||||/5/g; s/||||/4/g; s/|||/3/g; s/||/2/g; s/|/1/g;
s/</|/g
t convert
s/^$/0/
```



## Seed7


### Character Length


```seed7
length("Hello, world!")
```



## SETL


### Character Length


```haskell
print(# "Hello, world!"); -- '#' is the cardinality operator. Works on strings, tuples, and sets.
```



## Sidef



```ruby
var str = "J\x{332}o\x{332}s\x{332}e\x{301}\x{332}";
```



### Byte Length

UTF-8 byte length (default):

```ruby>say str.bytes.len;       #=> 14</lang


UTF-16 byte length:

```ruby
say str.encode('UTF-16').bytes.len;      #=> 20
```



### Character Length


```ruby>say str.chars.len;    #=> 9</lang



### Grapheme Length


```ruby>say str.graphs.len;   #=> 4</lang


## Simula

Simula has no bultin support for character encodings (Unicode was not even invented in the year 1967). The encoding was regarded responsibility of the operating system and one byte must match one character.
So character constants encoded in UTF-8 are not possible.
But reading from a utf8-encoded input file is actually possible.

```txt
møøse
𝔘𝔫𝔦𝔠𝔬𝔡𝔢
J̲o̲s̲é̲
€

```


### Byte Length


```simula
BEGIN
    TEXT LINE;
    WHILE NOT LASTITEM DO
    BEGIN
        INTEGER L;
        LINE :- COPY(SYSIN.IMAGE).STRIP;
        OUTCHAR('"');
        OUTTEXT(LINE);
        OUTCHAR('"');
        OUTTEXT(" BYTE LENGTH = "); OUTINT(LINE.LENGTH, 0);
        OUTIMAGE;
        INIMAGE;
    END;
END.

```

```txt

"møøse" BYTE LENGTH = 7
"𝔘𝔫𝔦𝔠𝔬𝔡𝔢" BYTE LENGTH = 28
"J̲o̲s̲é̲" BYTE LENGTH = 13
"€" BYTE LENGTH = 3

```



### Character Length

To calculate the character length, one can do it manually:

```simula
BEGIN

    ! NUMBER OF UFT8 CHARACTERS IN STRING ;
    INTEGER PROCEDURE UTF8STRLEN(S); TEXT S;
    BEGIN
        INTEGER R, LEN, BYTES, ALLBYTES;
        CHARACTER BYTE;
        WHILE S.MORE DO
        BEGIN
            BYTE := S.GETCHAR;
            ALLBYTES := ALLBYTES + 1;
            R := RANK(BYTE);
            LEN := LEN + 1;
            BYTES :=
                IF R >=   0 AND R <= 127 THEN 1 ELSE ! 0....... ASCII ;
                IF R >= 128 AND R <= 191 THEN 0 ELSE ! 10...... CONTINUATION ;
                IF R >= 192 AND R <= 223 THEN 2 ELSE ! 110..... 10x ;
                IF R >= 224 AND R <= 239 THEN 3 ELSE ! 1110.... 10x 10x ;
                IF R >= 240 AND R <= 247 THEN 4 ELSE ! 11110... 10x 10x 10x ;
                  -1;
            IF BYTES = -1 THEN ERROR("ILLEGAL UTF8 STRING");
            WHILE BYTES > 1 DO
            BEGIN
                BYTE := S.GETCHAR;
                ALLBYTES := ALLBYTES + 1;
                BYTES := BYTES - 1;
            END;
        END;
        UTF8STRLEN := LEN;
    END UTF8STRLEN;

    TEXT LINE;
    WHILE NOT LASTITEM DO
    BEGIN
        INTEGER L;
        LINE :- COPY(SYSIN.IMAGE).STRIP;
        OUTCHAR('"');
        OUTTEXT(LINE);
        OUTCHAR('"');
        L := UTF8STRLEN(LINE);
        OUTTEXT(" CHARACTER LENGTH = "); OUTINT(UTF8STRLEN(LINE), 0);
        OUTIMAGE;
        INIMAGE;
    END;

END.
```

```txt
"møøse" CHARACTER LENGTH = 5
"𝔘𝔫𝔦𝔠𝔬𝔡𝔢" CHARACTER LENGTH = 7
"J̲o̲s̲é̲" CHARACTER LENGTH = 8
"€" CHARACTER LENGTH = 1

```



## Scala

```scala

object StringLength extends App {
  val s1 = "møøse"
  val s3 = List("\uD835\uDD18", "\uD835\uDD2B", "\uD835\uDD26",
    "\uD835\uDD20", "\uD835\uDD2C", "\uD835\uDD21", "\uD835\uDD22").mkString
  val s4 = "J\u0332o\u0332s\u0332e\u0301\u0332"

    List(s1, s3, s4).foreach(s => println(
        s"The string: $s, characterlength= ${s.length} UTF8bytes= ${
      s.getBytes("UTF-8").size
    } UTF16bytes= ${s.getBytes("UTF-16LE").size}"))
}

```

```txt
The string: møøse, characterlength= 5 UTF8bytes= 7 UTF16bytes= 10
The string: 𝔘𝔫𝔦𝔠𝔬𝔡𝔢, characterlength= 14 UTF8bytes= 28 UTF16bytes= 28
The string: J̲o̲s̲é̲, characterlength= 9 UTF8bytes= 14 UTF16bytes= 18
```



## Slate


```slate
'Hello, world!' length.
```



## Smalltalk


### Byte Length


```smalltalk
string := 'Hello, world!'.
string size.
```


### Character Length

In GNU Smalltalk:


```smalltalk
string := 'Hello, world!'.
string numberOfCharacters.
```


requires loading the Iconv package:


```smalltalk
PackageLoader fileInPackage: 'Iconv'
```



## SNOBOL4


### Byte Length


```snobol4

	output = "Byte length: " size(trim(input))
end

```



### Character Length

The example works AFAIK only with CSnobol4 by Phil Budne

```snobol4

-include "utf.sno"
	output = "Char length: " utfsize(trim(input))
end

```



## Sparkling


### Byte length


```Sparkling>spn:1
 sizeof "Hello, wørld!"
= 14
```



## SPL


### Byte Length

All strings in SPL are Unicode. See code below.

### Character Length


```spl
t = ["abc","J̲o̲s̲é̲","møøse","𝔘𝔫𝔦𝔠𝔬𝔡𝔢"]

> i, 1..#.size(t,1)
  ? i>1, #.output()
  #.output(#.quot,t[i],#.quot," contains")

  p = #.split(t[i])
  cn = #.size(p,1)
  s = #.str(cn,">3>")+" chars: "
  > j, 1..cn
    ? j>1, s += ", "
    s += p[j]
  <
  #.output(s)

  q = #.array(t[i])
  bn = #.size(q,1)
  s = #.str(bn,">3>")+" bytes: "
  > j, 1..bn
    ? j>1, s += ", "
    s += #.str(q[j],"X2")+"h"
  <
  #.output(s)
<
```

```txt

"abc" contains
  3 chars: a,b,c
  6 bytes: 61h, 00h, 62h, 00h, 63h, 00h

"J̲o̲s̲é̲" contains
  4 chars: J̲,o̲,s̲,é̲
 16 bytes: 4Ah, 00h, 32h, 03h, 6Fh, 00h, 32h, 03h, 73h, 00h, 32h, 03h, E9h, 00h, 32h, 03h

"møøse" contains
  5 chars: m,ø,ø,s,e
 10 bytes: 6Dh, 00h, F8h, 00h, F8h, 00h, 73h, 00h, 65h, 00h

"𝔘𝔫𝔦𝔠𝔬𝔡𝔢" contains
  7 chars: 𝔘,𝔫,𝔦,𝔠,𝔬,𝔡,𝔢
 28 bytes: 35h, D8h, 18h, DDh, 35h, D8h, 2Bh, DDh, 35h, D8h, 26h, DDh, 35h, D8h, 20h, DDh, 35h, D8h, 2Ch, DDh, 35h, D8h, 21h, DDh, 35h, D8h, 22h, DDh

```


### Grapheme Length

SPL treats grapheme as a single character when splitting text. See code above.


## SQL


### Byte length

SELECT LENGTH(CAST('møøse' AS BLOB));


### Character length

SELECT LENGTH('møøse');


## SQL PL


### Character Length

With SQL only:

```sql pl

VALUES LENGTH('møøse', CODEUNITS16);
VALUES LENGTH('møøse', CODEUNITS32);
VALUES CHARACTER_LENGTH('møøse', CODEUNITS32);
VALUES LENGTH2('møøse');
VALUES LENGTH4('møøse');
VALUES LENGTH('𝔘𝔫𝔦𝔠𝔬𝔡𝔢', CODEUNITS16);
VALUES LENGTH('𝔘𝔫𝔦𝔠𝔬𝔡𝔢', CODEUNITS32);
VALUES CHARACTER_LENGTH('𝔘𝔫𝔦𝔠𝔬𝔡𝔢', CODEUNITS32);
VALUES LENGTH2('𝔘𝔫𝔦𝔠𝔬𝔡𝔢');
VALUES LENGTH4('𝔘𝔫𝔦𝔠𝔬𝔡𝔢');
VALUES LENGTH('J̲o̲s̲é̲', CODEUNITS16);
VALUES LENGTH('J̲o̲s̲é̲', CODEUNITS32);
VALUES CHARACTER_LENGTH('J̲o̲s̲é̲', CODEUNITS32);
VALUES LENGTH2('J̲o̲s̲é̲');
VALUES LENGTH4('J̲o̲s̲é̲');

```

Output:

```txt

db2 -t
db2 => VALUES LENGTH('møøse', CODEUNITS16);
1
-----------
          5

  1 record(s) selected.

db2 => VALUES LENGTH('møøse', CODEUNITS32);
1
-----------
          5

  1 record(s) selected.

db2 => VALUES CHARACTER_LENGTH('møøse', CODEUNITS32);
1
-----------
          5

  1 record(s) selected.

db2 => VALUES LENGTH2('møøse');
1
-----------
          5

  1 record(s) selected.

db2 => VALUES LENGTH4('møøse');
1
-----------
          5

  1 record(s) selected.

db2 => VALUES LENGTH('𝔘𝔫𝔦𝔠𝔬𝔡𝔢', CODEUNITS16);

1
-----------
         14

  1 record(s) selected.

db2 => VALUES LENGTH('𝔘𝔫𝔦𝔠𝔬𝔡𝔢', CODEUNITS32);
1
-----------
          7

  1 record(s) selected.

db2 => VALUES CHARACTER_LENGTH('𝔘𝔫𝔦𝔠𝔬𝔡𝔢', CODEUNITS32);
1
-----------
          7

  1 record(s) selected.

db2 => VALUES LENGTH2('𝔘𝔫𝔦𝔠𝔬𝔡𝔢');
1
-----------
         14

  1 record(s) selected.

db2 => VALUES LENGTH4('𝔘𝔫𝔦𝔠𝔬𝔡𝔢');
1
-----------
          7

  1 record(s) selected.

db2 => VALUES LENGTH('J̲o̲s̲é̲', CODEUNITS16);

1
-----------
          8

  1 record(s) selected.

db2 => VALUES LENGTH('J̲o̲s̲é̲', CODEUNITS32);
1
-----------
          8

  1 record(s) selected.

db2 => VALUES CHARACTER_LENGTH('J̲o̲s̲é̲', CODEUNITS32);
1
-----------
          8

  1 record(s) selected.

db2 => VALUES LENGTH2('J̲o̲s̲é̲');
1
-----------
          8

  1 record(s) selected.

db2 => VALUES LENGTH4('J̲o̲s̲é̲');
1
-----------
          8

  1 record(s) selected.


```


### Byte Length

With SQL only:

```sql pl

VALUES LENGTH('møøse');
VALUES LENGTHB('møøse');
VALUES LENGTH('𝔘𝔫𝔦𝔠𝔬𝔡𝔢');
VALUES LENGTHB('𝔘𝔫𝔦𝔠𝔬𝔡𝔢');
VALUES LENGTH('J̲o̲s̲é̲');
VALUES LENGTHB('J̲o̲s̲é̲');

```

Output:

```txt

db2 -t
db2 => VALUES LENGTH('møøse');

1
-----------
          7

  1 record(s) selected.

db2 => VALUES LENGTHB('møøse');
1
-----------
          7

  1 record(s) selected.

db2 => VALUES LENGTH('𝔘𝔫𝔦𝔠𝔬𝔡𝔢');
1
-----------
         28

  1 record(s) selected.

db2 => VALUES LENGTHB('𝔘𝔫𝔦𝔠𝔬𝔡𝔢');
1
-----------
         28

  1 record(s) selected.

db2 => VALUES LENGTH('J̲o̲s̲é̲');
1
-----------
         13

  1 record(s) selected.

db2 => VALUES LENGTHB('J̲o̲s̲é̲');
1
-----------
         13

  1 record(s) selected.


```



## Standard ML


### Byte Length

```sml
val strlen = size "Hello, world!";
```


### Character Length

```sml
val strlen = UTF8.size "Hello, world!";
```



## Stata


Use '''[https://www.stata.com/help.cgi?f_strlen strlen]''' for byte length, and '''[https://www.stata.com/help.cgi?f_ustrlen ustrlen]''' for the number of Unicode characters in a string.


```stata
scalar s="Ἐν ἀρχῇ ἐποίησεν ὁ θεὸς τὸν οὐρανὸν καὶ τὴν γῆν"
di strlen(s)
di ustrlen(s)
```



## Swift



### Grapheme Length

Swift has a concept of "character" that goes beyond Unicode code points. A <code>Character</code> is a "Unicode grapheme cluster", which can consist of one or more Unicode code points.

To count "characters" (Unicode grapheme clusters):
```swift
let numberOfCharacters = "møøse".characters.count  // 5
```

```swift
let numberOfCharacters = count("møøse")            // 5
```

```swift
let numberOfCharacters = countElements("møøse")    // 5
```



### Character Length

To count Unicode code points:
```swift
let numberOfCodePoints = "møøse".unicodeScalars.count           // 5
```

```swift
let numberOfCodePoints = count("møøse".unicodeScalars)          // 5
```

```swift
let numberOfCodePoints = countElements("møøse".unicodeScalars)  // 5
```



### Byte Length

This depends on which encoding you want to use.

For length in UTF-8, count the number of UTF-8 code units:
```swift
let numberOfBytesUTF8 = "møøse".utf8.count           // 7
```

```swift
let numberOfBytesUTF8 = count("møøse".utf8)          // 7
```

```swift
let numberOfBytesUTF8 = countElements("møøse".utf8)  // 7
```


For length in UTF-16, count the number of UTF-16 code units, and multiply by 2:
```swift
let numberOfBytesUTF16 = "møøse".utf16.count * 2           // 10
```

```swift
let numberOfBytesUTF16 = count("møøse".utf16) * 2          // 10
```

```swift
let numberOfBytesUTF16 = countElements("møøse".utf16) * 2  // 10
```



## Tcl


### Byte Length

Formally, Tcl does not guarantee to use any particular representation for its strings internally (the underlying implementation objects can hold strings in at least three different formats, mutating between them as necessary) so the way to calculate the "byte length" of a string can only be done with respect to some user-selected encoding. This is done this way (for UTF-8):

```tcl
string length [encoding convertto utf-8 $theString]
```

<!-- Yes, there's <tt>string bytelength</tt>; don't use it. It's deeply wrong-headed and will probably go away in future releases. [[DKF]] -->
Thus, we have these examples:

```tcl
set s1 "hello, world"
set s2 "\u304A\u306F\u3088\u3046"
set enc utf-8
puts [format "length of \"%s\" in bytes is %d" \
     $s1 [string length [encoding convertto $enc $s1]]]
puts [format "length of \"%s\" in bytes is %d" \
     $s2 [string length [encoding convertto $enc $s2]]]
```



### Character Length

Basic version:


```tcl
string length "Hello, world!"
```


or more elaborately, needs '''Interpreter''' any 8.X. Tested on 8.4.12.


```tcl
fconfigure stdout -encoding utf-8; #So that Unicode string will print correctly
set s1 "hello, world"
set s2 "\u304A\u306F\u3088\u3046"
puts [format "length of \"%s\" in characters is %d"  $s1 [string length $s1]]
puts [format "length of \"%s\" in characters is %d"  $s2 [string length $s2]]
```


=={{header|TI-89 BASIC}}==

The TI-89 uses an fixed 8-bit encoding so there is no difference between character length and byte length.


```ti89b
■ dim("møøse")              5
```



## Toka


### Byte Length


```toka
" hello, world!" string.getLength
```



## Trith


### Character Length


```trith
"møøse" length
```


### Byte Length


```trith
"møøse" size
```


## TUSCRIPT


### Character Length


```tuscript

$$ MODE TUSCRIPT
string="hello, world"
l=LENGTH (string)
PRINT "character length of string '",string,"': ",l

```

Output:

```txt

Character length of string 'hello, world': 12

```



## UNIX Shell


### Byte Length


### =With external utility:=


```bash
string='Hello, world!'
length=`expr "x$string" : '.*' - 1`
echo $length # if you want it printed to the terminal
```


====With [[Unix|SUSv3]] parameter expansion modifier:====

```bash
string='Hello, world!'
length="${#string}"
echo $length # if you want it printed to the terminal
```



## Vala


### Character Length


```vala

string s = "Hello, world!";
int characterLength = s.length;

```



## VBA

Cf. VBScript (below).


## VBScript


### Byte Length


```vbscript
LenB(string|varname)
```


Returns the number of bytes required to store a string in memory. Returns null if string|varname is null.

### Character Length


```vbscript
Len(string|varname)
```


Returns the length of the string|varname . Returns null if string|varname is null.


## Visual Basic

same as [[#VBScript]].


## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic (language version >=15.5)

Strings in .NET are immutable wrappers around arrays of the <code>Char</code> type, which represents a UTF-16 code unit (with a size of two bytes). Classes for encoding and decoding strings to and from byte arrays in various encodings are located in the <code>System.Text</code> namespace, with <code>System.Text.Encoding</code> representing different string encodings (and providing means of encoding and decoding strings to raw byte arrays). The Length property of a string returns the number of Chars it contains, and is thus the number of UTF-16 code units in that string.


### =Byte Length=


One method of Encoding returns the number of bytes required to encode a .NET string in that encoding (encoding objects can be obtained through readonly static [Shared in VB.NET] properties of the Encoding class).


```vbnet
Module ByteLength
    Function GetByteLength(s As String, encoding As Text.Encoding) As Integer
        Return encoding.GetByteCount(s)
    End Function
End Module
```



### =Character Length=


There is no intended means of obtaining the number of code points in a string in .NET, though a straightforward implementation is to take one fourth of the string's byte length in UTF-32 (as UTF-32 is a fixed-length encoding where each code point is four bytes).

An alternative implementation is to count the number of UTF-16 surrogate pairs in a string and subtract that number from the number of UTF-16 code units in the string.


```vbnet
Module CharacterLength
    Function GetUTF16CodeUnitsLength(s As String) As Integer
        Return s.Length
    End Function

    Private Function GetUTF16SurrogatePairCount(s As String) As Integer
        GetUTF16SurrogatePairCount = 0
        For i = 1 To s.Length - 1
            If Char.IsSurrogatePair(s(i - 1), s(i)) Then GetUTF16SurrogatePairCount += 1
        Next
    End Function

    Function GetCharacterLength_FromUTF16(s As String) As Integer
        Return GetUTF16CodeUnitsLength(s) - GetUTF16SurrogatePairCount(s)
    End Function

    Function GetCharacterLength_FromUTF32(s As String) As Integer
        Return GetByteLength(s, Text.Encoding.UTF32) \ 4
    End Function
End Module
```



### =Grapheme Length=


<code>System.Globalization.StringInfo</code> provides a means of enumerating the text elements of a string, where each "text element" is a Unicode grapheme.


```vbnet
Module GraphemeLength
    ' Wraps an IEnumerator, allowing it to be used as an IEnumerable.
    Private Iterator Function AsEnumerable(enumerator As IEnumerator) As IEnumerable
        Do While enumerator.MoveNext()
            Yield enumerator.Current
        Loop
    End Function

    Function GraphemeCount(s As String) As Integer
        Dim elements = Globalization.StringInfo.GetTextElementEnumerator(s)
        Return AsEnumerable(elements).OfType(Of String).Count()
    End Function
End Module
```



### =Test Code=


The compiler constant <code>PRINT_TESTCASE</code> toggles whether to write the contents of each test case to the console; disable for inputs that may mess with the console.


```vbnet
#Const PRINT_TESTCASE = True

Module Program
    ReadOnly TestCases As String() =
    {
        "Hello, world!",
        "møøse",
        "𝔘𝔫𝔦𝔠𝔬𝔡𝔢", ' String normalization of the file makes the e and diacritic in é̲ one character, so use VB's char "escapes"
        $"J{ChrW(&H332)}o{ChrW(&H332)}s{ChrW(&H332)}e{ChrW(&H301)}{ChrW(&H332)}"
    }

    Sub Main()
        Const INDENT = "    "
        Console.OutputEncoding = Text.Encoding.Unicode

        Dim writeResult = Sub(s As String, result As Integer) Console.WriteLine("{0}{1,-20}{2}", INDENT, s, result)

        For i = 0 To TestCases.Length - 1
            Dim c = TestCases(i)

            Console.Write("Test case " & i)
#If PRINT_TESTCASE Then
            Console.WriteLine(": " & c)
#Else
            Console.WriteLine()
#End If
            writeResult("graphemes", GraphemeCount(c))
            writeResult("UTF-16 units", GetUTF16CodeUnitsLength(c))
            writeResult("Cd pts from UTF-16", GetCharacterLength_FromUTF16(c))
            writeResult("Cd pts from UTF-32", GetCharacterLength_FromUTF32(c))
            Console.WriteLine()
            writeResult("bytes (UTF-8)", GetByteLength(c, Text.Encoding.UTF8))
            writeResult("bytes (UTF-16)", GetByteLength(c, Text.Encoding.Unicode))
            writeResult("bytes (UTF-32)", GetByteLength(c, Text.Encoding.UTF32))
            Console.WriteLine()
        Next

    End Sub
End Module
```


<code>graphemes</code> corresponds to Grapheme Length in the task description, and either <code>Cd pts</code> value corresponds with Character Length. Byte lengths are given for three Unicode encodings.

Note that the byte length in UTF-16 is always twice the length of a string due to .NET strings using UTF-16.


```txt
Test case 0: Hello, world!
    graphemes           13
    UTF-16 units        13
    Cd pts from UTF-16  13
    Cd pts from UTF-32  13

    bytes (UTF-8)       13
    bytes (UTF-16)      26
    bytes (UTF-32)      52

Test case 1: møøse
    graphemes           5
    UTF-16 units        5
    Cd pts from UTF-16  5
    Cd pts from UTF-32  5

    bytes (UTF-8)       7
    bytes (UTF-16)      10
    bytes (UTF-32)      20

Test case 2: 𝔘𝔫𝔦𝔠𝔬𝔡𝔢
    graphemes           7
    UTF-16 units        14
    Cd pts from UTF-16  7
    Cd pts from UTF-32  7

    bytes (UTF-8)       28
    bytes (UTF-16)      28
    bytes (UTF-32)      28

Test case 3: J̲o̲s̲é̲
    graphemes           4
    UTF-16 units        9
    Cd pts from UTF-16  9
    Cd pts from UTF-32  9

    bytes (UTF-8)       14
    bytes (UTF-16)      18
    bytes (UTF-32)      36

```



## x86 Assembly


### Byte Length

The following code uses AT&T syntax and was tested using AS (the portable GNU assembler) under Linux.


```x86 Assembly

.data
string:         .asciz "Test"

.text
.globl  main

main:
        pushl   %ebp
        movl    %esp, %ebp

        pushl   %edi
        xorb    %al, %al
        movl    $-1, %ecx
        movl    $string, %edi
        cld
        repne   scasb
        not     %ecx
        dec     %ecx
        popl    %edi

        ;; string length is stored in %ecx register

        leave
        ret

```



## XPL0


```XPL0
include c:\cxpl\stdlib;
IntOut(0, StrLen("Character length = Byte length = String length = "))
```


Output:

```txt

49

```



## XSLT


### Character Length


```xml
<?xml version="1.0" encoding="UTF-8"?>
```

 ...

```xml
<xsl:value-of select="string-length('møøse')" />   <!-- 5 -->
```



## xTalk

### Byte Length

```xtalk
put the length of "Hello World"
```


or


```xtalk
put the number of characters in "Hello World"
```


### Character Length

```xtalk
put the length of "Hello World"
```


or


```xtalk
put the number of characters in "Hello World"
```



## Wren


### Byte Length


```wren
System.print("møøse".bytes.count)
System.print("𝔘𝔫𝔦𝔠𝔬𝔡𝔢".bytes.count)
System.print("J̲o̲s̲é̲".bytes.count)

```


### Character Length


```wren
System.print("møøse".count)
System.print("𝔘𝔫𝔦𝔠𝔬𝔡𝔢".count)
System.print("J̲o̲s̲é̲".count)

```



## Yorick


### Character Length


```yorick
strlen("Hello, world!")
```



## zkl

Although zkl handles 8 bit ASCII natively, it also knows UTF-8 up to 6 bytes. In a string, "\uabcd" (hex) and "\U???;" encode 2 and n byte UTF-8 Unicode characters. It does not handle UTF-16 or 4 byte Unicode. It also knows about the bad spots in UTF-8 (such as overlongs).

If your terminal/editor  deals with UTF-8 (mine doesn't), you don't need to use the escapes, just put the unicode characters in quotes (ie the editor inserts UTF bytes, which are non zero).

### Character Length


```zkl
"abc".len() //-->3
"\ufeff\u00A2 \u20ac".len() //-->9 "BOM¢ €"
```


### Byte Length


```zkl
"abc".len() //-->3
"\ufeff\u00A2 \u20ac".len() //-->9
Data(0,Int,"\ufeff\u00A2 \u20ac") //-->Data(9) (bytes)
"J\u0332o\u0332s\u0332e\u0301\u0332".len()  //-->14
"\U1D518;\U1D52B;\U1D526;\U1D520;\U1D52C;\U1D521;\U1D522;".len() //-->28
```


### Character Length

UTF-8 characters are counted, modifiers (such as underscore) are counted as separate characters.

```zkl
"abc".len(8) //-->3
"\ufeff\u00A2 \u20ac".len(8) //-->4 "BOM¢ €"
"\U1000;".len(8)  //-->Exception thrown: ValueError(Invalid UTF-8 string)
"\uD800" //-->SyntaxError : Line 2: Bad Unicode constant (\uD800-\uDFFF)
"J\u0332o\u0332s\u0332e\u0301\u0332".len(8) //-->9 "J̲o̲s̲é̲"
"\U1D518;\U1D52B;\U1D526;\U1D520;\U1D52C;\U1D521;\U1D522;".len(8) //-->7 "𝔘𝔫𝔦𝔠𝔬𝔡𝔢"
```

[[Wikipedia::https://en.wikipedia.org/wiki/Comparison_of_programming_languages_%28string_functions%29#length]]
