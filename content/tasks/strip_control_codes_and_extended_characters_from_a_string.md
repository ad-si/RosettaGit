+++
title = "Strip control codes and extended characters from a string"
description = ""
date = 2019-05-01T19:34:27Z
aliases = []
[extra]
id = 9875
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "awk",
  "basic",
  "bbc_basic",
  "bracmat",
  "c",
  "c_plus_plus",
  "c_sharp",
  "clojure",
  "common_lisp",
  "d",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "j",
  "javascript",
  "java",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "lua",
  "mathematica",
  "nim",
  "ocaml",
  "pascal",
  "peloton",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pike",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "scala",
  "seed7",
  "sidef",
  "tcl",
  "txr",
  "vbscript",
  "xpl0",
  "zkl",
]
+++

The task is to strip control codes and extended characters from a string. The solution should demonstrate how to achieve each of the following results:

* a string with control codes stripped (but extended characters not stripped)
* a string with control codes and extended characters stripped


In ASCII, the control codes have decimal codes 0 through to 31 and 127. On an ASCII based system, if the control codes are stripped, the resultant string would have all of its characters within the range of 32 to 126 decimal on the ASCII table.

On a non-ASCII based system, we consider characters that do not have a corresponding glyph on the ASCII table (within the ASCII range of 32 to 126 decimal) to be an extended character for the purpose of this task.





## Ada



```Ada
with Ada.Text_IO;

procedure Strip_ASCII is

   Full: String := 'a' & Character'Val(11) & 'b' & Character'Val(166) &
                   'c' & Character'Val(127) & Character'Val(203) &
                   Character'Val(202) & "de";
   -- 5 ordinary characters ('a' .. 'e')
   -- 2 control characters (11, 127); note that 11 is the "vertical tab"
   -- 3 extended characters (166, 203, 202)

   function Filter(S:     String;
                   From:  Character := ' ';
                   To:    Character := Character'Val(126);
                   Above: Character := Character'Val(127)) return String is
   begin
      if S'Length = 0 then
         return "";
      elsif (S(S'First) >= From and then S(S'First) <= To) or else S(S'First) > Above then
         return S(S'First) & Filter(S(S'First+1 .. S'Last), From, To, Above);
      else
         return Filter(S(S'First+1 .. S'Last), From, To, Above);
      end if;
   end Filter;

   procedure Put_Line(Text, S: String) is
   begin
      Ada.Text_IO.Put_Line(Text & " """ & S & """, Length:" & Integer'Image(S'Length));
   end Put_Line;

begin
   Put_Line("The full string :", Full);
   Put_Line("No Control Chars:", Filter(Full)); -- default values for From, To, and Above
   Put_Line("Neither_Extended:", Filter(Full, Above => Character'Last)); -- defaults for From and To
end Strip_ASCII;

```


Output:


```txt
The full string : "a
                    b�c��de", Length: 10
No Control Chars: "ab�c��de", Length: 8
Neither_Extended: "abcde", Length: 5
```



## ALGOL 68


```algol68
# remove control characters and optionally extended characters from the string text  #
# assums ASCII is the character set                                                  #
PROC strip characters = ( STRING text, BOOL strip extended )STRING:
     BEGIN
         # we build the result in a []CHAR and convert back to a string at the end #
         INT text start = LWB text;
         INT text max   = UPB text;
         [ text start : text max ]CHAR result;
         INT result pos := text start;
         FOR text pos FROM text start TO text max DO
             INT ch := ABS text[ text pos ];
             IF ( ch >= 0 AND ch <= 31 ) OR ch = 127 THEN
                 # control character #
                 SKIP
             ELIF strip extended AND ( ch > 126 OR ch < 0 ) THEN
                 # extened character and we don't want them #
                 SKIP
             ELSE
                 # include this character #
                 result[ result pos ] := REPR ch;
                 result pos +:= 1
             FI
         OD;
         result[ text start : result pos - 1 ]
     END # strip characters # ;

# test the control/extended character stripping procedure #
STRING t = REPR 2 + "abc" + REPR 10 + REPR 160 + "def~" + REPR 127 + REPR 10 + REPR 150 + REPR 152 + "!";
print( ( "<<" + t + ">> - without control characters:             <<" + strip characters( t, FALSE ) + ">>", newline ) );
print( ( "<<" + t + ">> - without control or extended characters: <<" + strip characters( t, TRUE  ) + ">>", newline ) )
```

```txt

<<�abc
ádef~
ûÿ!>> - without control characters:             <<abcádef~ûÿ!>>
<<�abc
ádef~
ûÿ!>> - without control or extended characters: <<abcdef~!>>

```



## AutoHotkey

```AHK
Stripped(x){
	Loop Parse, x
		if Asc(A_LoopField) > 31 and Asc(A_LoopField) < 128
			r .= A_LoopField
	return r
}
MsgBox % stripped("`ba" Chr(00) "b`n`rc`fd" Chr(0xc3))
```



## AWK


```AWK

# syntax: GAWK -f STRIP_CONTROL_CODES_AND_EXTENDED_CHARACTERS.AWK
BEGIN {
    s = "ab\xA2\x09z" # a b cent tab z
    printf("original string: %s (length %d)\n",s,length(s))
    gsub(/[\x00-\x1F\x7F]/,"",s); printf("control characters stripped: %s (length %d)\n",s,length(s))
    gsub(/[\x80-\xFF]/,"",s); printf("control and extended stripped: %s (length %d)\n",s,length(s))
    exit(0)
}

```

<p>output:</p>

```txt

original string: ab¢    z (length 5)
control characters stripped: ab¢z (length 4)
control and extended stripped: abz (length 3)

```


## BASIC

While DOS does support ''some'' extended characters, they aren't entirely standardized, and shouldn't be relied upon.


```qbasic
DECLARE FUNCTION strip$ (what AS STRING)
DECLARE FUNCTION strip2$ (what AS STRING)

DIM x AS STRING, y AS STRING, z AS STRING

'   tab                c+cedilla           eof
x = CHR$(9) + "Fran" + CHR$(135) + "ais" + CHR$(26)
y = strip(x)
z = strip2(x)

PRINT "x:"; x
PRINT "y:"; y
PRINT "z:"; z

FUNCTION strip$ (what AS STRING)
    DIM outP AS STRING, L0 AS INTEGER, tmp AS STRING
    FOR L0 = 1 TO LEN(what)
        tmp = MID$(what, L0, 1)
        SELECT CASE ASC(tmp)
            CASE 32 TO 126
                outP = outP + tmp
        END SELECT
    NEXT
    strip$ = outP
END FUNCTION

FUNCTION strip2$ (what AS STRING)
    DIM outP AS STRING, L1 AS INTEGER, tmp AS STRING
    FOR L1 = 1 TO LEN(what)
        tmp = MID$(what, L1, 1)
        SELECT CASE ASC(tmp)
                'normal     accented    various     greek, math, etc.
            CASE 32 TO 126, 128 TO 168, 171 TO 175, 224 TO 253
                outP = outP + tmp
        END SELECT
    NEXT
    strip2$ = outP
END FUNCTION
```


Output:
 x:      Français→
 y:Franais
 z:Français

See also: [[#Liberty BASIC|Liberty BASIC]], [[#PureBasic|PureBasic]], [[#Run BASIC|Run BASIC]]


## BBC BASIC


```bbcbasic
      test$ = CHR$(9) + "Fran" + CHR$(231) + "ais." + CHR$(127)
      PRINT "Original ISO-8859-1 string: " test$ " (length " ; LEN(test$) ")"
      test$ = FNstripcontrol(test$)
      PRINT "Control characters stripped: " test$ " (length " ; LEN(test$) ")"
      test$ = FNstripextended(test$)
      PRINT "Control & extended stripped: " test$ " (length " ; LEN(test$) ")"
      END

      DEF FNstripcontrol(A$) : REM CHR$(127) is a 'control' code
      LOCAL I%
      WHILE I%<LEN(A$)
        I% += 1
        IF ASCMID$(A$,I%)<32 OR ASCMID$(A$,I%)=127 THEN
          A$ = LEFT$(A$,I%-1) + MID$(A$,I%+1)
        ENDIF
      ENDWHILE
      = A$

      DEF FNstripextended(A$)
      LOCAL I%
      WHILE I%<LEN(A$)
        I% += 1
        IF ASCMID$(A$,I%)>127 THEN
          A$ = LEFT$(A$,I%-1) + MID$(A$,I%+1)
        ENDIF
      ENDWHILE
      = A$
```

Output:

```txt

Original ISO-8859-1 string:  Français (length 11)
Control characters stripped: Français. (length 9)
Control & extended stripped: Franais. (length 8)

```



## Bracmat


```bracmat
(  "string of ☺☻♥♦⌂, may include control
characters and other ilk.\L\D§►↔◄
Rødgrød med fløde"
  : ?string1
  : ?string2
& :?newString
&   whl
  ' ( @(!string1:?clean (%@:<" ") ?string1)
    & !newString !clean:?newString
    )
& !newString !string1:?newString
& out$(str$("Control characters stripped:
" str$!newString))
& :?newString
&   whl
  ' ( @(!string2:?clean (%@:(<" "|>"~")) ?string2)
    & !newString !clean:?newString
    )
& !newString !string2:?newString
&   out
  $ ( str
    $ ( "
Control characters and extended characters stripped:
"
        str$!newString
      )
    )
& );
```

Output:

```txt
Control characters stripped:
string of ⌂, may include controlcharacters and other ilk.§Rødgrød med fløde

Control characters and extended characters stripped:
string of , may include controlcharacters and other ilk.Rdgrd med flde
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

#define IS_CTRL  (1 << 0)
#define IS_EXT	 (1 << 1)
#define IS_ALPHA (1 << 2)
#define IS_DIGIT (1 << 3) /* not used, just give you an idea */

unsigned int char_tbl[256] = {0};

/* could use ctypes, but then they pretty much do the same thing */
void init_table()
{
	int i;

	for (i = 0; i < 32; i++) char_tbl[i] |= IS_CTRL;
	char_tbl[127] |= IS_CTRL;

	for (i = 'A'; i <= 'Z'; i++) {
		char_tbl[i] |= IS_ALPHA;
		char_tbl[i + 0x20] |= IS_ALPHA; /* lower case */
	}

	for (i = 128; i < 256; i++) char_tbl[i] |= IS_EXT;
}

/* depends on what "stripped" means; we do it in place.
 * "what" is a combination of the IS_* macros, meaning strip if
 * a char IS_ any of them
 */
void strip(char * str, int what)
{
	unsigned char *ptr, *s = (void*)str;
	ptr = s;
	while (*s != '\0') {
		if ((char_tbl[(int)*s] & what) == 0)
			*(ptr++) = *s;
		s++;
	}
	*ptr = '\0';
}

int main()
{
	char a[256];
	int i;

	init_table();

	/* populate string with one of each char */
	for (i = 1; i < 255; i++) a[i - 1] = i; a[255] = '\0';
	strip(a, IS_CTRL);
	printf("%s\n", a);

	for (i = 1; i < 255; i++) a[i - 1] = i; a[255] = '\0';
	strip(a, IS_CTRL | IS_EXT);
	printf("%s\n", a);

	for (i = 1; i < 255; i++) a[i - 1] = i; a[255] = '\0';
	strip(a, IS_CTRL | IS_EXT | IS_ALPHA);
	printf("%s\n", a);

	return 0;
}
```
output:<lang> !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ <odd stuff my xterm thinks are bad unicode hence can't be properly shown>
 !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
 !"#$%&'()*+,-./0123456789:;<=>?@[\]^_`{|}~
```



## C++


```cpp
#include <string>
#include <iostream>
#include <algorithm>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/casts.hpp>
#include <ctime>
#include <cstdlib>
using namespace boost::lambda ;

struct MyRandomizer {
   char operator( )( ) {
      return static_cast<char>( rand( ) % 256 ) ;
   }
} ;

std::string deleteControls ( std::string startstring ) {
   std::string noControls( "                                        " ) ;//creating space for
   //the standard algorithm remove_copy_if
   std::remove_copy_if( startstring.begin( ) , startstring.end( ) , noControls.begin( ) ,
	 ll_static_cast<int>( _1 ) < 32 && ll_static_cast<int>( _1 ) == 127 ) ;
   return noControls ;
}

std::string deleteExtended( std::string startstring ) {
   std::string noExtended ( "                                        " ) ;//same as above
   std::remove_copy_if( startstring.begin( ) , startstring.end( ) , noExtended.begin( ) ,
	 ll_static_cast<int>( _1 ) > 127 || ll_static_cast<int>( _1 ) < 32 ) ;
   return noExtended ;
}

int main( ) {
   std::string my_extended_string ;
   for ( int i = 0 ; i < 40 ; i++ ) //we want the extended string to be 40 characters long
      my_extended_string.append( " " ) ;
   srand( time( 0 ) ) ;
   std::generate_n( my_extended_string.begin( ) , 40 , MyRandomizer( ) ) ;
   std::string no_controls( deleteControls( my_extended_string ) ) ;
   std::string no_extended ( deleteExtended( my_extended_string ) ) ;
   std::cout << "string with all characters: " << my_extended_string << std::endl ;
   std::cout << "string without control characters: " << no_controls << std::endl ;
   std::cout << "string without extended characters: " << no_extended << std::endl ;
   return 0 ;
}
```

Output:
<PRE>string with all characters: K�O:~���7�5����
���W��@>��ȓ�q�Q@���W-
string without control characters: K�O:~���7�5����
���W��@>��ȓ�q�Q@���W-
string without extended characters: KO:~75W@>qQ@W-
</PRE>
## C#
Uses the test string from REXX.

```c#

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RosettaCode
{
    class Program
    {
        static void Main(string[] args)
        {
            string test = "string of ☺☻♥♦⌂, may include control characters and other ilk.♫☼§►↔◄";
            Console.WriteLine("Original: {0}", test);
            Console.WriteLine("Stripped of control codes: {0}", StripControlChars(test));
            Console.WriteLine("Stripped of extended: {0}", StripExtended(test));
        }

        static string StripControlChars(string arg)
        {
            char[] arrForm = arg.ToCharArray();
            StringBuilder buffer = new StringBuilder(arg.Length);//This many chars at most

            foreach(char ch in arrForm)
                if (!Char.IsControl(ch)) buffer.Append(ch);//Only add to buffer if not a control char

            return buffer.ToString();
        }

        static string StripExtended(string arg)
        {
            StringBuilder buffer = new StringBuilder(arg.Length); //Max length
            foreach(char ch in arg)
            {
                UInt16 num = Convert.ToUInt16(ch);//In .NET, chars are UTF-16
                //The basic characters have the same code points as ASCII, and the extended characters are bigger
                if((num >= 32u) && (num <= 126u)) buffer.Append(ch);
            }
            return buffer.ToString();
        }
    }
}

```

Output:

```txt

Original: string of ☺☻♥♦⌂, may include control characters and other ilk.♫☼§►↔◄
Stripped of control codes: string of ☺☻♥♦⌂, may include control characters and other ilk.♫☼§►↔◄
Stripped of extended: string of , may include control characters and other ilk.

```



## Clojure


```clojure
; generate our test string of characters with control and extended characters
(def range-of-chars (apply str (map char (range 256))))

; filter out the control characters:
(apply str (filter #(not (Character/isISOControl %)) range-of-chars))

; filter to return String of characters that are between 32 - 126:
(apply str (filter #(<= 32 (int %) 126) range-of-chars))
```



## Common Lisp


```txt
> (defparameter *extended-ascii* (coerce (loop for i from 0 to 255 collect (code-char i)) 'string))

*EXTENDED-ASCII*
> (defparameter *control-codes-stripped*
    (remove-if #'(lambda (c)
                   (let ((x (char-code c)))
                     (or (< x 32) (= x 127))))
               *extended-ascii*))

*CONTROL-CODES-STRIPPED*
> *control-codes-stripped*

" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklm¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"
> (defparameter *control-codes-and-extended-stripped*
    (remove-if-not #'(lambda (c) (and (standard-char-p c) (graphic-char-p c)))
                   *extended-ascii*))

*CONTROL-CODES-AND-EXTENDED-STRIPPED*
> *control-codes-and-extended-stripped*

" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
```



## D


```d
import std.traits;

S stripChars(S)(S s, bool function(dchar) pure nothrow mustStrip)
        pure nothrow if (isSomeString!S) {
    S result;
    foreach (c; s) {
        if (!mustStrip(c))
            result ~= c;
    }
    return result;
}

void main() {
    import std.stdio, std.uni;
    auto s = "\u0000\u000A abc\u00E9def\u007F";
    writeln(s.stripChars( &isControl ));
    writeln(s.stripChars( c => isControl(c) || c == '\u007F' ));
    writeln(s.stripChars( c => isControl(c) || c >= '\u007F' ));
}
```

```txt
 abcédef
 abcédef
 abcdef
```



## Erlang

Exported functions to be used by [[Update_a_configuration_file]]

```Erlang

-module( strip_control_codes ).

-export( [is_not_control_code/1, is_not_control_code_nor_extended_character/1, task/0] ).

is_not_control_code( C ) when C > 127 -> true;
is_not_control_code( C ) when C < 32; C =:= 127 -> false;
is_not_control_code( _C ) -> true.

is_not_control_code_nor_extended_character( C ) when C > 127 -> false;
is_not_control_code_nor_extended_character( C )	-> is_not_control_code( C ).

task() ->
    String = lists:seq( 0, 255 ),
    io:fwrite( "String (~p characters): ~s~n", [erlang:length(String), String] ),
    String_without_cc = lists:filter( fun is_not_control_code/1, String ),
    io:fwrite( "String without control codes (~p characters): ~s~n", [erlang:length(String_without_cc), String_without_cc] ),
    String_without_cc_nor_ec = lists:filter( fun is_not_control_code_nor_extended_character/1, String ),
    io:fwrite( "String without control codes nor extended characters (~p characters): ~s~n", [erlang:length(String_without_cc_nor_ec), String_without_cc_nor_ec] ).

```

```txt

41> strip_control_codes:task().
String (256 characters): ^@^A^B^C^D^E^F^G^H
^N^O^P^Q^R^S^T^U^V^W^X^Y^Z^[^\^]^^^_ !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~^ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ
String without control codes (223 characters):  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ
String without control codes nor extended characters (95 characters):  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~

```


=={{header|F Sharp|F#}}==
Uses test string from REXX.

```fsharp

open System

let stripControl (arg:string) =
    String(Array.filter (fun x -> not (Char.IsControl(x))) (arg.ToCharArray()))
//end stripControl

let stripExtended (arg:string) =
    let numArr = Array.map (fun (x:char) -> Convert.ToUInt16(x)) (arg.ToCharArray()) in
    String([|for num in numArr do if num >= 32us && num <= 126us then yield Convert.ToChar(num) |])
//end stripExtended

[<EntryPoint>]
let main args =
    let test = "string of ☺☻♥♦⌂, may include control characters and other ilk.♫☼§►↔◄"
    printfn "Original: %s" test
    printfn "Stripped of controls: %s" (stripControl test)
    printfn "Stripped of extended: %s" (stripExtended test)
    0//main must return integer, much like in C/C++

```

Output:

```txt

Original: string of ☺☻♥♦, may include control characters and other ilk.♫☼§►↔◄
Stripped of controls: string of ☺☻♥♦, may include control characters and other ilk.♫☼§►↔◄
Stripped of extended: string of , may include control characters and other ilk.

```



## Factor

<lang>USING: ascii kernel sequences ;

: strip-control-codes ( str -- str' ) [ control? not ] filter ;

: strip-control-codes-and-extended ( str -- str' )
    strip-control-codes [ ascii? ] filter ;
```



## Forth


```forth
: strip ( buf len -- buf len' )  \ repacks buffer, so len' <= len
  over + over swap over ( buf dst limit src )
  do
    i c@ 32 127 within if
      i c@ over c! char+
    then
  loop
  over - ;
```



## Fortran


```fortran
module stripcharacters
implicit none

contains

  pure logical function not_control(ch)
    character, intent(in) :: ch
    not_control = iachar(ch) >= 32 .and. iachar(ch) /= 127
  end function not_control

  pure logical function not_extended(ch)
    character, intent(in) :: ch
    not_extended = iachar(ch) >= 32 .and. iachar(ch) < 127
  end function not_extended

  pure function strip(string,accept) result(str)
    character(len=*), intent(in) :: string
    character(len=len(string))   :: str
    interface
      pure logical function accept(ch)
        character, intent(in) :: ch
      end function except
    end interface
    integer :: i,n
    str = repeat(' ',len(string))
    n = 0
    do i=1,len(string)
      if ( accept(string(i:i)) ) then
        n = n+1
        str(n:n) = string(i:i)
      end if
    end do
  end function strip

end module stripcharacters


program test
  use stripcharacters

  character(len=256) :: string, str
  integer            :: ascii(256), i
  forall (i=0:255) ascii(i) = i
  forall (i=1:len(string)) string(i:i) = achar(ascii(i))
  write (*,*) string

  write (*,*) 'Control characters deleted:'
  str = strip(string,not_control)
  write (*,*) str

  forall (i=1:len(string)) string(i:i) = achar(ascii(i))
  write (*,*) 'Extended characters deleted:'
  write (*,*) strip(string,not_extended)
end program test

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function stripControlChars(s As Const String) As String
  If s = "" Then Return ""
  Dim count As Integer = 0
  Dim strip(0 To Len(s) - 1) As Boolean
  For i As Integer = 0 To Len(s) - 1
    For j As Integer = 0 To 31
      If s[i] = j OrElse s[i] = 127 Then
        count += 1
        strip(i) = True
        Exit For
      End If
    Next j
  Next i

  Dim buffer As String = Space(Len(s) - count)
  count  = 0
  For i As Integer = 0 To Len(s) - 1
    If Not Strip(i) Then
      buffer[count] = s[i]
      count += 1
    End If
  Next
  Return buffer
End Function

Function stripExtendedChars(s As Const String) As String
  If s = "" Then Return ""
  Dim count As Integer = 0
  Dim strip(0 To Len(s) - 1) As Boolean
  For i As Integer = 0 To Len(s) - 1
    For j As Integer = 128 To 255
      If s[i] = j Then
        count += 1
        strip(i) = True
        Exit For
      End If
    Next j
  Next i

  Dim buffer As String = Space(Len(s) - count)
  count  = 0
  For i As Integer = 0 To Len(s) - 1
    If Not Strip(i) Then
      buffer[count] = s[i]
      count += 1
    End If
  Next
  Return buffer
End Function

Dim s  As String = !"\v\001The\t quick\255 \vbrown\127\f fox\156"
Dim s1 As String = stripControlChars(s)
Dim s2 As String = stripExtendedChars(s)
Dim s3 As String = stripExtendedChars(s1)

' Under Windows console, code page 850 :
' "vertical tab" displays as ♂
' "form feed" displays as ♀
' Chr(1) displays as ☺
' Chr(127) displays as ⌂
' the other control characters do what it says on the tin
' Chr(156) displays as £
' Chr(255) displays as space

Print "Before stripping   :" , s
Print "Ctl chars stripped :" , s1
Print "Ext chars stripped :" , s2
Print "Both sets stripped :" , s3
Print
Print "Before stripping"   ,  "Length => " ; Len(s)
Print "Ctl chars stripped" ,  "Length => " ; Len(s1)
Print "Ext chars stripped" ,  "Length => " ; Len(s2)
Print "Both sets stripped" ,  "Length => " ; Len(s3)
Print
Print "Press any key to quit"
Sleep
```


```txt

Before stripping   :        ♂☺The        quick  ♂brown⌂♀ fox£
Ctl chars stripped :        The quick  brown fox£
Ext chars stripped :        ♂☺The        quick ♂brown⌂♀ fox
Both sets stripped :        The quick brown fox

Before stripping            Length =>  27
Ctl chars stripped          Length =>  21
Ext chars stripped          Length =>  25
Both sets stripped          Length =>  19

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=19db658a6c44cfb1f6f887ff53e549bb Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String = "The\t \equick\n \fbrownfox \vcost £125.00 or €145.00 or $160.00 \bto \ncapture ©®"
Dim sStd, sExtend As String
Dim siCount As Short

For siCount = 32 To 126
  sStd &= Chr(siCount)
Next

For siCount = 128 To 255
  sExtend &= Chr(siCount)
Next

Print "Original string: -\t" & sString & gb.NewLine
Print "No extended characters: -\t" & Check(sString, sStd)
sStd &= sExtend
Print "With extended characters: -\t" & Check(sString, sStd)

End
'________________________________________________________________
Public Sub Check(sString As String, sCheck As String) As String
Dim siCount As Short
Dim sResult As String

For siCount = 1 To Len(sString)
  If InStr(sCheck, Mid(sString, siCount, 1)) Then sResult &= Mid(sString, siCount, 1)
Next

Return sResult

End
```

Output:

```txt

Original string: -      The      uick

brownfox ^Kcost £125.00 or €145.00 or $160.00to
capture ©®

No extended characters: -       The quick brownfox cost 125.00 or 145.00 or $160.00 to capture
With extended characters: -     The quick brownfox cost £125.00 or €145.00 or $160.00 to capture ©®

```



## Go

Go works for ASCII and non-ASCII systems.  The first pair of functions below interpret strings as byte strings, presumably useful for strings consisting of ASCII and 8-bit extended ASCII data.  The second pair of functions interpret strings as UTF-8.

```go
package main

import (
	"golang.org/x/text/transform"
	"golang.org/x/text/unicode/norm"
	"fmt"
	"strings"
)

// two byte-oriented functions identical except for operator comparing c to 127.
func stripCtlFromBytes(str string) string {
	b := make([]byte, len(str))
	var bl int
	for i := 0; i < len(str); i++ {
		c := str[i]
		if c >= 32 && c != 127 {
			b[bl] = c
			bl++
		}
	}
	return string(b[:bl])
}

func stripCtlAndExtFromBytes(str string) string {
	b := make([]byte, len(str))
	var bl int
	for i := 0; i < len(str); i++ {
		c := str[i]
		if c >= 32 && c < 127 {
			b[bl] = c
			bl++
		}
	}
	return string(b[:bl])
}

// two UTF-8 functions identical except for operator comparing c to 127
func stripCtlFromUTF8(str string) string {
	return strings.Map(func(r rune) rune {
		if r >= 32 && r != 127 {
			return r
		}
		return -1
	}, str)
}

func stripCtlAndExtFromUTF8(str string) string {
	return strings.Map(func(r rune) rune {
		if r >= 32 && r < 127 {
			return r
		}
		return -1
	}, str)
}

// Advanced Unicode normalization and filtering,
// see http://blog.golang.org/normalization and
// http://godoc.org/golang.org/x/text/unicode/norm for more
// details.
func stripCtlAndExtFromUnicode(str string) string {
	isOk := func(r rune) bool {
		return r < 32 || r >= 127
	}
	// The isOk filter is such that there is no need to chain to norm.NFC
	t := transform.Chain(norm.NFKD, transform.RemoveFunc(isOk))
	// This Transformer could also trivially be applied as an io.Reader
	// or io.Writer filter to automatically do such filtering when reading
	// or writing data anywhere.
	str, _, _ = transform.String(t, str)
	return str
}

const src = "déjà vu" + // precomposed unicode
	"\n\000\037 \041\176\177\200\377\n" + // various boundary cases
	"as⃝df̅" // unicode combining characters

func main() {
	fmt.Println("source text:")
	fmt.Println(src)
	fmt.Println("\nas bytes, stripped of control codes:")
	fmt.Println(stripCtlFromBytes(src))
	fmt.Println("\nas bytes, stripped of control codes and extended characters:")
	fmt.Println(stripCtlAndExtFromBytes(src))
	fmt.Println("\nas UTF-8, stripped of control codes:")
	fmt.Println(stripCtlFromUTF8(src))
	fmt.Println("\nas UTF-8, stripped of control codes and extended characters:")
	fmt.Println(stripCtlAndExtFromUTF8(src))
	fmt.Println("\nas decomposed and stripped Unicode:")
	fmt.Println(stripCtlAndExtFromUnicode(src))
}
```

Output: (varies with display configuration)

```txt

source text:
déjà vu
� !~��
as⃝df̅

as bytes, stripped of control codes:
déjà vu !~��as⃝df̅

as bytes, stripped of control codes and extended characters:
dj vu !~asdf

as UTF-8, stripped of control codes:
déjà vu !~��as⃝df̅

as UTF-8, stripped of control codes and extended characters:
dj vu !~asdf

as decomposed and stripped Unicode:
deja vu !~asdf

```



## Groovy


```Groovy
def stripControl = { it.replaceAll(/\p{Cntrl}/, '') }
def stripControlAndExtended = { it.replaceAll(/[^\p{Print}]/, '') }
```

Test:

```Groovy
def text = (0..255).collect { (char) it }.join('')
def textMinusControl = text.findAll { int v = (char)it; v > 31 && v != 127 }.join('')
def textMinusControlAndExtended = textMinusControl.findAll {((char)it) < 128 }.join('')

assert stripControl(text) == textMinusControl
assert stripControlAndExtended(text) == textMinusControlAndExtended
```



## Haskell


```Haskell
import Control.Applicative (liftA2)

strip, strip2 :: String -> String
strip = filter (liftA2 (&&) (> 31) (< 126) . fromEnum)

-- or
strip2 = filter (((&&) <$> (> 31) <*> (< 126)) . fromEnum)

main :: IO ()
main =
  (putStrLn . unlines) $
  [strip, strip2] <*> ["alphabetic 字母 with some less parochial parts"]
```

```txt
alphabetic  with some less parochial parts
alphabetic  with some less parochial parts
```


=={{header|Icon}} and {{header|Unicon}}==
We'll use ''deletec'' to remove unwanted characters (2nd argument) from a string (1st argument).  The procedure below coerces types back and forth between string and cset.  The character set of unwanted characters is the difference of all ASCII characters and the ASCII characters from 33 to 126.

```Icon
procedure main(A)
write(image(deletec(&ascii,&ascii--(&ascii)[33:127])))
end
link strings

```


[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings.icn provides deletec]

The IPL procedure ''deletec'' is equivalent to this:

```Icon
procedure deletec(s, c)			#: delete characters
   result := ""
   s ? {
      while  result ||:= tab(upto(c)) do tab(many(c))
      return result ||:= tab(0)
      }
end
```



Output:
```txt
" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"
```



## J

'''Solution:'''

```j
stripControlCodes=: -.&(DEL,32{.a.)
stripControlExtCodes=: ([ -. -.)&(32}.127{.a.)
```

'''Usage:'''

```j
   mystring=: a. {~ ?~256        NB. ascii chars 0-255 in random order
   #mystring                       NB. length of string
256
   #stripControlCodes mystring     NB. length of string without control codes
223
   #stripControlExtCodes mystring  NB. length of string without control codes or extended chars
95
   #myunicodestring=: u: ?~1000     NB. unicode characters 0-999 in random order
1000
   #stripControlCodes myunicodestring
967
   #stripControlExtCodes myunicodestring
95
   stripControlExtCodes myunicodestring
k}w:]U3xEh9"GZdr/#^B.Sn%\uFOo[(`t2-J6*IA=Vf&N;lQ8,${XLz5?D0~s)'Y7Kq|ip4<WRCaM!b@cgv_T +mH>1ejPy
```


## JavaScript



### ES 5



```JavaScript
(function (strTest) {

    // s -> s
    function strip(s) {
        return s.split('').filter(function (x) {
            var n = x.charCodeAt(0);

            return 31 < n && 127 > n;
        }).join('');
    }

    return strip(strTest);

})("\ba\x00b\n\rc\fd\xc3");
```


```JavaScript
"abcd"
```



## Java

```java
import java.util.function.IntPredicate;

public class StripControlCodes {

    public static void main(String[] args) {
        String s = "\u0000\n abc\u00E9def\u007F";
        System.out.println(stripChars(s, c -> c > '\u001F' && c != '\u007F'));
        System.out.println(stripChars(s, c -> c > '\u001F' && c < '\u007F'));
    }

    static String stripChars(String s, IntPredicate include) {
        return s.codePoints().filter(include::test).collect(StringBuilder::new,
                StringBuilder::appendCodePoint, StringBuilder::append).toString();
    }
}
```


```txt
 abcédef
 abcdef
```


## jq

```jq
def strip_control_codes:
 explode | map(select(. > 31 and . != 127)) | implode;

def strip_extended_characters:
  explode | map(select(31 < . and . < 127)) | implode;
```


'''Example''':

```jq
def string: "string of ☺☻♥♦⌂, may include control characters such as null(\u0000) and other ilk.\n§►↔◄\nRødgrød med fløde";

"string | strip_control_codes\n => \(string | strip_control_codes)",
"string | strip_extended_characters\n => \(string | strip_extended_characters)"
```

```sh
$ jq -n -r -f Strip_control_codes_and_extended_characters.jq
string | strip_control_codes
 => string of ☺☻♥♦⌂, may include control characters such as null() and other ilk.§►↔◄Rødgrød med fløde
string | strip_extended_characters
 => string of , may include control characters such as null() and other ilk.Rdgrd med flde
```



## Julia


```Julia

stripc0{T<:String}(a::T) = replace(a, r"[\x00-\x1f\x7f]", "")
stripc0x{T<:String}(a::T) = replace(a, r"[^\x20-\x7e]", "")

a = "a\n\tb\u2102d\u2147f"

println("Original String:\n    ", a)
println("\nWith C0 control characters removed:\n    ", stripc0(a))
println("\nWith C0 and extended characters removed:\n    ", stripc0x(a))

```


```txt

Original String:
    a
        bℂdⅇf

With C0 control characters removed:
    abℂdⅇf

With C0 and extended characters removed:
    abdf

```



## Kotlin


```scala
// version 1.1.2

fun String.strip(extendedChars: Boolean = false): String {
    val sb = StringBuilder()
    for (c in this) {
        val i = c.toInt()
        if (i in 32..126 || (!extendedChars && i >= 128)) sb.append(c)
    }
    return sb.toString()
}

fun main(args: Array<String>) {
    println("Originally:")
    val s = "123\tabc\u0007DEF\u007F+-*/€æŧðłþ"
    println("String = $s  Length = ${s.length}")
    println("\nAfter stripping control characters:")
    val t = s.strip()
    println("String = $t  Length = ${t.length}")
    println("\nAfter stripping control and extended characters:")
    val u = s.strip(true)
    println("String = $u  Length = ${u.length}")
}
```


```txt

Originally:
String = 123	abcDEF+-*/€æŧðłþ  Length = 22

After stripping control characters:
String = 123abcDEF+-*/€æŧðłþ  Length = 19

After stripping control and extended characters:
String = 123abcDEF+-*/  Length = 13

```



## Liberty BASIC


```lb

    all$ =""
    for i =0 to 255
        all$ =all$ +chr$( i)
    next i

    print "Original string of bytes.  ( chr$( 10) causes a CRLF.)"
    print all$
    print

    lessControl$ =controlStripped$( all$)
    print "With control codes stripped out."
    print lessControl$
    print

    lessExtendedAndControl$ =extendedStripped$( lessControl$)
    print "With extended codes stripped out too."
    print lessExtendedAndControl$

    end

    function controlStripped$( i$)
        r$ =""
        for j =1 to len( i$)
            ch$ =mid$( i$, j, 1)
            if asc( ch$) >=32 then r$ =r$ +ch$
        next j
        controlStripped$ =r$
    end function

    function extendedStripped$( i$)
        r$ =""
        for j =1 to len( i$)
            ch$ =mid$( i$, j, 1)
            if asc( ch$) <=128 then r$ =r$ +ch$
        next j
        extendedStripped$ =r$
    end function

```



## Lua


```lua
function Strip_Control_Codes( str )
    local s = ""
    for i in str:gmatch( "%C+" ) do
 	s = s .. i
    end
    return s
end

function Strip_Control_and_Extended_Codes( str )
    local s = ""
    for i = 1, str:len() do
	if str:byte(i) >= 32 and str:byte(i) <= 126 then
  	    s = s .. str:sub(i,i)
	end
    end
    return s
end

q = ""
for i = 0, 255 do
	q = q .. string.char(i)
end

print( Strip_Control_Codes(q) )
print( Strip_Control_and_Extended_Codes(q) )
```


```txt
 !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ
 !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
```



## Mathematica


```Mathematica
stripCtrl[x_]:=StringJoin[Select[Characters[x],
MemberQ[CharacterRange["!","~"]~Join~Characters[FromCharacterCode[Range[128,255]]],#]&]]

stripCtrlExt[x_]:=StringJoin[Select[Characters[x],
MemberQ[CharacterRange["!","~"],#]&]]
```


Test:

```txt
CompleteSet=FromCharacterCode[Range[0,255]]
->\.00\.02\.03\.04\.05\.06\.07\.08\.0b\.0e\.0f\.10\.11\.12\.13\.14
\.15\.16\.17\.18\.19\.1a\[RawEscape]\.1c\.1d\.1e\.1f !"#$%&'()*+,-./
0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]
^_`abcdefghijklmnopqrstuvwxyz{|}~ ¡¢£¤¥¦§¨©ª«\[Not]­®¯\[Degree]
\[PlusMinus]\.b2\.b3\.b4\[Micro]\[Paragraph]\[CenterDot]¸¹º»¼½¾¿
ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ*ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö/øùúûüýþÿ

stripCtrl[CompleteSet]
->!"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]
^_`abcdefghijklmnopqrstuvwxyz{|}~ ¡¢£¤¥¦§¨©ª«\[Not]­®¯\[Degree]
\[PlusMinus]\.b2\.b3\.b4\[Micro]\[Paragraph]\[CenterDot]
¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ*ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö
/øùúûüýþÿ

stripCtrlExt[CompleteSet]
->!"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]
^_`abcdefghijklmnopqrstuvwxyz{|}~
```



=={{header|MATLAB}} / {{header|Octave}}==


```MATLAB
 function str = stripped(str)
    str = str(31<str & str<127);
  end;
```



## Nim


```nim
proc stripped(str): string =
  result = ""
  for c in str:
    if ord(c) in 32..126:
      result.add c

echo stripped "\ba\x00b\n\rc\fd\xc3"
```

Output:

```txt
abcd
```



## OCaml



```ocaml
let is_control_code c =
  let d = int_of_char c in
  d < 32 || d = 127

let is_extended_char c =
  let d = int_of_char c in
  d > 127

let strip f str =
  let len = String.length str in
  let res = Bytes.create len in
  let rec aux i j =
    if i >= len
    then Bytes.to_string (Bytes.sub res 0 j)
    else if f str.[i]
    then aux (succ i) j
    else begin
      Bytes.set res j str.[i];
      aux (succ i) (succ j)
    end
  in
  aux 0 0

let () =
  Random.self_init ();
  let len = 32 in
  let s =
    String.init len (fun _ ->
      char_of_int (Random.int 256))
  in
  print_endline (strip is_control_code s);
  print_endline (strip (fun c -> (is_control_code c) || (is_extended_char c)) s);
;;
```



## Pascal

```pascal
program StripCharacters(output);

function Strip (s: string; control, extended: boolean): string;
  var
    index: integer;
  begin
    Strip := '';
    for index:= 1 to length(s) do
    if not ((control and (ord(s[index]) <= 32)) or (extended and (ord(s[index]) > 127))) then
      Strip := Strip + s[index];
  end;

var
  test: string;
  i: integer;

begin
  setlength(test, 40);
  randomize;
  for i := 1 to length(test) do
    test[i] := char(1 + random(255));
  writeln ('Original: ', test);
  writeln ('No CNTL:  ', Strip(test, true,  false));
  writeln ('No extnd: ', Strip(test, false, true));
  writeln ('ASCII:    ', Strip(test, true,  true));
end.
```

Output:

```txt
% ./StripCharacters
Original: )?z8i9?a?K??N?s?F˪w?a??s
                                  #?b?B}PT?ٜ
No CNTL:  )?z8i9?a?K??N?s?F˪w?a??s#?b?B}PT?ٜ
No extnd: )z8i9aKNsFwas
                       #bB}PT
ASCII:    )z8i9aKNsFwas#bB}PT

```



## Peloton

Peloton has a native instruction for removing control codes from a string, SAL, the Low ASCII Strip. From the manual:

```sgml>Create variable with control characters: <@ SAYLETVARLIT
i|This string has control characters
	-	-	-	-	-	-

in it</@>
Strip control characters <@ SAYSALVAR>i</@>
Assign infix <@ LETVARSALVAR>j|i</@> <@ SAYVAR>j</@>
Assign prepend <@ LETSALVARVAR>k|i</@> <@ SAYVAR>k</@>
Reflexive assign <@ ACTSALVAR>i</@> <@ SAYVAR>i</@>
```


Peloton also has SAH, High ASCII Strip. Again, from the manual:

```sgml>Create variable with high and low ANSI: <@ SAYLETVARLIT
i|This string has both low ansi and high ansi characters - il doit d'être prévenu</@>
Strip high  ANSI <@ SAYSAHVAR>i</@>
Assign infix <@ LETVARSAHVAR>j|i</@> <@ SAYVAR>j</@>
Assign prepend <@ LETSAHVARVAR>k|i</@> <@ SAYVAR>k</@>
Reflexive assign <@ ACTSAHVAR>i</@> <@ SAYVAR>i</@>
```



## Perl


```Perl
#!/usr/bin/perl -w
use strict ;

my @letters ;
my @nocontrols ;
my @noextended ;
for ( 1..40 ) {
   push @letters ,  int( rand( 256 ) ) ;
}
print "before sanitation : " ;
print join( '' , map { chr( $_ ) } @letters ) ;
print "\n" ;
@nocontrols = grep { $_ > 32 && $_ != 127 } @letters ;
print "Without controls: " ;
print join( '' , map { chr( $_ ) } @nocontrols ) ;
@noextended = grep { $_ < 127 } @nocontrols ;
print "\nWithout extended: " ;
print join( '' , map { chr( $_ ) } @noextended ) ;
print "\n" ;
```

Output:
<PRE>before sanitation : �L08&YH�O��n)�:���O�G$���.���"zO���Q�?��
Without controls: �L08&YH�O��n)�:�O�G$���.���"zO��Q�?��
Without extended: L08&YHOn):OG$."zOQ?
</PRE>


## Perl 6

```perl6
my $str = (0..400).roll(80)».chr.join;

say $str;
say $str.subst(/<:Cc>/,      '', :g); # unicode property: control character
say $str.subst(/<-[\ ..~]>/, '', :g);
```


```txt
kşaNĹĭŗ|Ęw��"ÄlĄWł8iCƁę��Ż�¬5ĎĶ'óü¸'ÍŸ;ŢƐ¦´ŷQċűÒŴ$ÃŅĐįð+=ĥƂ+Ōĭħ¼ŕc¤H~ìïēÕ
kşaNĹĭŗ|Ęw"ÄlĄWł8iCƁęŻ¬5ĎĶ'óü¸'ÍŸ;ŢƐ¦´ŷQċűÒŴ$ÃŅĐįð+=ĥƂ+Ōĭħ¼ŕc¤H~ìïēÕ
kaN|w"lW8iC5'';Q$+=+cH~
```



## Phix

While you can delete a character from a string using say s[i..i] = "", the fastest and easiest way is always just
to build a new one character-by-character.

I've credited Ada solely for the sensible fromch/toch/abovech idea.

```Phix
function filter(string s, integer fromch=' ', toch=#7E, abovech=#7F)
string res = ""
    for i=1 to length(s) do
        integer ch = s[i]
        if ch>=fromch and (ch<=toch or ch>abovech) then
            res &= ch
        end if
    end for
    return res
end function

procedure put_line(string text, s)
    printf(1,"%s \"%s\", Length:%d\n",{text,s,length(s)})
end procedure

string full = "\u0000 abc\u00E9def\u007F"

put_line("The full string:", full)
put_line("No Control Chars:", filter(full)) -- default values for fromch, toch, and abovech
put_line("\" and no Extended:", filter(full, abovech:=#FF)) -- defaults for fromch and toch
```

```txt

The full string: "  abc+®def", Length:11
No Control Chars: " abc+®def", Length:9
" and no Extended: " abcdef", Length:7

```



## PicoLisp

Control characters in strings are written with a hat (^) in PicoLisp. ^? is the DEL character.

```PicoLisp
(de stripCtrl (Str)
   (pack
      (filter
         '((C)
            (nor (= "^?" C) (> " " C "^A")) )
         (chop Str) ) ) )

(de stripCtrlExt (Str)
   (pack
      (filter
         '((C) (> "^?" C "^_"))
         (chop Str) ) ) )
```

Test:

```txt
: (char "^?")
-> 127

: (char "^_")
-> 31

: (stripCtrl "^I^M a b c^? d äöüß")
-> " a b c d äöüß"

: (stripCtrlExt "^I^M a b c^? d äöüß")
-> " a b c d "
```



## Pike


```Pike>
 string input = random_string(100);
> (string)((array)input-enumerate(32)-enumerate(255-126,1,127));
Result: "p_xx08M]cK<FHgR3\\I.x>)Tm<VgakYddy&P7"
```



## PL/I


```PL/I

stripper: proc options (main);
   declare s character (100) varying;
   declare i fixed binary;

   s = 'the quick brown fox jumped';
   /* A loop to replace blanks with control characters */
   do i = 1 to length(s);
      if substr(s, i, 1) = ' ' then
         substr(s, i, 1) = '01'x;
   end;
   put skip list (s);

   call stripcc (s);
   put skip list (s);

   s = 'now is the time for all good men';
   /* A loop to replace blanks with control characters */
   do i = 1 to length(s);
      if substr(s, i, 1) = ' ' then
         substr(s, i, 1) = 'A1'x;
   end;
   put skip list (s);

   call stripex (s);
   put skip list (s);

/* Strip control codes. */
stripcc: procedure (s);
   declare s character (*) varying;
   declare w character (length(s));
   declare c character (1);
   declare (i, j) fixed binary;

   j = 0;
   do i = 1 to length (s);
      c = substr(s, i, 1);
      if unspec(c) >= '00100000'b | unspec(c) = '01111111'b then
         do;
            j = j + 1;
            substr(w, j, 1) = c;
         end;
   end;
   s = substr(w, 1, j);
end stripcc;

/* Strips control codes and extended characters. */
stripex: procedure (s);
   declare s character (*) varying;
   declare w character (length(s));
   declare c character (1);
   declare (i, j) fixed binary;

   j = 0;
   do i = 1 to length (s);
      c = substr(s, i, 1);
      if unspec(c) >= '00100000'b & unspec(c) < '01111111'b then
         do;
            j = j + 1;
            substr(w, j, 1) = c;
         end;
   end;
   s = substr(w, 1, j);
end stripex;

end stripper;

```

Output:

```txt

the�quick�brown�fox�jumped
thequickbrownfoxjumped
now¡is¡the¡time¡for¡all¡good¡men
nowisthetimeforallgoodmen

```



## PowerShell


```PowerShell

function Remove-Character
{
    [CmdletBinding(DefaultParameterSetName="Control and Extended")]
    [OutputType([string])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [string]
        $String,

        [Parameter(ParameterSetName="Control")]
        [switch]
        $Control,

        [Parameter(ParameterSetName="Extended")]
        [switch]
        $Extended
    )

    Begin
    {
        filter Remove-ControlCharacter
        {
            $_.ToCharArray() | ForEach-Object -Begin {$out = ""} -Process {if (-not [Char]::IsControl($_)) {$out += $_ }} -End {$out}
        }

        filter Remove-ExtendedCharacter
        {
            $_.ToCharArray() | ForEach-Object -Begin {$out = ""} -Process {if ([int]$_ -lt 127) {$out += $_ }} -End {$out}
        }
    }
    Process
    {
        foreach ($s in $String)
        {
            switch ($PSCmdlet.ParameterSetName)
            {
                "Control"  {$s | Remove-ControlCharacter}
                "Extended" {$s | Remove-ExtendedCharacter}
                Default    {$s | Remove-ExtendedCharacter | Remove-ControlCharacter}
            }
        }
    }
}

```



```PowerShell

$test = "$([char]9)Français."

"Original string              : `"$test`""
"Control characters stripped  : `"$($test | Remove-Character -Control)`""
"Extended characters stripped : `"$($test | Remove-Character -Extended)`""
"Control & extended stripped  : `"$($test | Remove-Character)`""

```

```txt

Original string              : "	Français."
Control characters stripped  : "Français."
Extended characters stripped : "	Franais."
Control & extended stripped  : "Franais."

```


```PowerShell

"Français", "Čeština" | Remove-Character -Extended

```

```txt

Franais
etina

```



## PureBasic


```PureBasic
Procedure.s stripControlCodes(source.s)
  Protected i, *ptrChar.Character, length = Len(source), result.s
  *ptrChar = @source
  For i = 1 To length
    If *ptrChar\c > 31
      result + Chr(*ptrChar\c)
    EndIf
    *ptrChar + SizeOf(Character)
  Next
  ProcedureReturn result
EndProcedure

Procedure.s stripControlExtCodes(source.s)
  Protected i, *ptrChar.Character, length = Len(source), result.s
  *ptrChar = @source
  For i = 1 To length
    If *ptrChar\c > 31 And *ptrChar\c < 128
      result + Chr(*ptrChar\c)
    EndIf
    *ptrChar + SizeOf(Character)
  Next
  ProcedureReturn result
EndProcedure

If OpenConsole()
  ;create sample string
  Define i, s.s
  For i = 1 To 80
    s + Chr(Random(254) + 1) ;include character values from 1 to 255
  Next

  PrintN(stripControlCodes(s))    ;string without control codes
  PrintN("---------")
  PrintN(stripControlExtCodes(s)) ;string without control codes or extended chars

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
»╫=┐C─≡G(═ç╤â√╝÷╔¬ÿ▌x  è4∞|)ï└⌐ƒ9²òτ┌ºáj)▓<~-vPÿφQ╨ù¿╖îFh"[ü╗dÉ₧q#óé├p╫■
---------
=CG(x 4|)9j)<~-vPQFh"[dq#p
```



## Python


```Python
stripped = lambda s: "".join(i for i in s if 31 < ord(i) < 127)

print(stripped("\ba\x00b\n\rc\fd\xc3"))
```
Output:<lang>abcd
```



## Racket



```Racket

#lang racket
;; Works on both strings (Unicode) and byte strings (raw/ASCII)
(define (strip-controls str)
  (regexp-replace* #rx"[\0-\037\177]+" str ""))
(define (strip-controls-and-extended str)
  (regexp-replace* #rx"[^\040-\176]+" str ""))

```



## REXX

Note that   ''guillemets''   were used as fences in presenting/displaying the   «««before»»»   and   «««after»»»   text strings.

### idiomatic version

This REXX version processes each character in an idiomatic way   (if it's a wanted character, then keep it).

```rexx
/*REXX program strips all  "control codes"  from a character string  (ASCII or EBCDIC). */
z= 'string of ☺☻♥♦⌂, may include control characters and other    ♫☼§►↔◄░▒▓█┌┴┐±÷²¬└┬┘ilk.'
@=' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~'
$=
   do j=1  for length(z);   _=substr(z, j, 1)    /*get a char from   X   one at a time. */
   if verify(_, @)==0  then $=$ || _             /*Is char in the @ list?   Then use it.*/
   end   /*j*/                                   /*stick a fork in it,  we're all done. */

say 'old = »»»'z"«««"                            /*add ««fence»» before & after old text*/
say 'new = »»»'$"«««"                            /* "      "        "   "   "   new   " */
```

```txt

old = »»»string of ☺☻♥♦⌂, may include control characters and other    ♫☼§►↔◄░▒▓█┌┴┐±÷²¬└┬┘ilk.«««
new = »»»string of , may include control characters and other    ilk.«««

```



### faster version

This REXX version only deletes unwanted characters.

It also shows a different way of performing concatenations (without using abutments,   and a way to split a long literal (character) string.

Because there are   (or should be)   fewer unwanted characters than wanted characters, this version is faster.

```rexx
/*REXX program strips all  "control codes"  from a character string  (ASCII or EBCDIC). */
x= 'string of ☺☻♥♦⌂, may include control characters and other    ♫☼§►↔◄░▒▓█┌┴┐±÷²¬└┬┘ilk.'
@=' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghij' || ,
                                              'klmnopqrstuvwxyz{|}~'
$=x                                              /*set "new" string to same as the old. */
   do  until _=0;             _=verify($, @)     /*check if  any  character isn't in  @.*/
   if _\==0  then $=delstr($, _, 1)              /*Is this a bad char?   Then delete it.*/
   end   /*until*/                               /*stick a fork in it,  we're all done. */

say 'old = »»»' || x || "«««"                    /*add ««fence»» before & after old text*/
say 'new = »»»' || $ || "«««"                    /* "      "        "   "   "   new   " */
```

## Ring


```ring

s = char(31) + "abc" + char(13) + "def" + char(11) + "ghi" + char(10)
see strip(s) + nl

func strip str
strip = ""
for i = 1 to len(str)
    nr = substr(str,i,1)
    a = ascii(nr)
    if a > 31 and a < 123 and nr != "'" and nr != """"
       strip = strip + nr ok
next
return strip

```



## Ruby


```ruby
class String
  def strip_control_characters()
    chars.each_with_object("") do |char, str|
      str << char unless char.ascii_only? and (char.ord < 32 or char.ord == 127)
    end
  end

  def strip_control_and_extended_characters()
    chars.each_with_object("") do |char, str|
      str << char if char.ascii_only? and char.ord.between?(32,126)
    end
  end
end

p s = "\ba\x00b\n\rc\fd\xc3\x7ffoo"
p s.strip_control_characters
p s.strip_control_and_extended_characters
```


```txt
"\ba\u0000b\n\rc\fd\xC3\u007Ffoo"
"abcd\xC3foo"
"abcdfoo"
```



## Run BASIC


```runbasic
s$ = chr$(31) + "abc" + chr$(13) + "def" + chr$(11) + "ghi" + chr$(10)
print strip$(s$)

' -----------------------------------------
' strip junk
' -----------------------------------------
FUNCTION strip$(str$)
for i = 1 to len(str$)
  a$ = MID$(str$,i,1)
  a = ASC(a$)
  if a > 31 then
    if a <  123 then
      if a$ <> "'" then
        if a$ <> """" then
          strip$ = strip$ + a$
        end if
      end if
    end if
  end if
next i
END FUNCTION
```


```txt

input  : chr$(31)+"abc"+chr$(13)+"def"+chr$(11)+"ghi"+chr$(10)
output : abcdefghi
```




## Scala


### ASCII: Using StringOps Class


```Scala
val controlCode : (Char) => Boolean = (c:Char) => (c <= 32 || c == 127)
val extendedCode : (Char) => Boolean = (c:Char) => (c <= 32 || c > 127)


// ASCII test...
val teststring = scala.util.Random.shuffle( (1.toChar to 254.toChar).toList ).mkString

println( "ctrl filtered out: \n\n" +
  teststring.filterNot(controlCode) + "\n" )

println( "ctrl and extended filtered out: \n\n" +
  teststring.filterNot(controlCode).filterNot(extendedCode) + "\n" )
```

```txt
ctrl filtered out:

?d2??6ú╖)ⁿ┼gEhW3RS⌠!a?┬╘├╢-ß?·▄╔B,_?╟│┤'C║j«?ΩcqJ╣²▀÷±?0s∩░uτ8Φ½&Σ¬y▓H?*?AL?═eDX??≥╚╧
4σ+r=Ñ¼╙U▌"?.⌐?≡K?k╥áF\?╕QΘ?╪Z?▐√╠?`M?7▒°G^≈@xz?>t:╦╨íw¿─┐]Io(V╡?P¡┴?º┌ΓO┘φ└╓~|#⌡π?}µ
╗l???$ó{n/╫mi╤<9f≤?∙»Nª;?1??εT?■╩%╒╛[╜p∞α╬vñ╞bYδ╝5█


ctrl and extended filtered out:

?d26)gEhW3RS!a-B,_'CjcqJ0su8&yH*ALeDX4+r=U".KkF
\QZ`M7G^@xz>t:w]Io(VPO~|#}l${n/mi<9fN;1T%[pvbY5

```



### Unicode: Using Regular Expressions


```Scala
//
// A Unicode test string
//
val ulist = 0x8232.toChar :: 0xFFF9.toChar :: 0x200E.toChar :: (1.toChar to 2000.toChar).toList
val ustring = scala.util.Random.shuffle( ulist ).mkString

// Remove control codes including private codes
val sNoCtrlCode = ustring.replaceAll("[\\p{C}]","")

val htmlNoCtrlCode = for( i <- sNoCtrlCode.indices ) yield
  "&#" + sNoCtrlCode(i).toInt + ";" + (if( (i+1) % 10 == 0 ) "\n" else "")
println( "ctrl filtered out: <br/><br/>\n\n" + htmlNoCtrlCode.mkString  + "<br/><br/>\n" )


// Keep 0x00-0x7f and remove control codes
val sNoExtCode = ustring.replaceAll("[^\\p{InBasicLatin}]","").replaceAll("[\\p{C}]","")

val htmlNoExtCode = for( i <- sNoExtCode.indices ) yield
  "&#" + sNoExtCode(i).toInt + ";" + (if( (i+1) % 10 == 0 ) "\n" else "")
println( "ctrl and extended filtered out: <br/><br/>\n\n" + htmlNoExtCode.mkString  + "<br/><br/>\n" )
```

```txt
ctrl filtered out:

&#1178;&#1779;&#1209;&#1597;&#1888;&#268;&#471;&#32;&#580;&#1582;&#575;&#1147;&#570;&#183;&#726;&#1166;&#68;&#1520;&#1129;&#1336;&#1509;&#758;&#1636;&#478;&#317;&#1773;&#1297;&#995;&#689;&#385;&#939;&#1740;&#1400;&#976;&#1755;&#1557;&#415;&#881;&#1220;&#1510;
&#1886;&#196;&#1210;&#996;&#938;&#113;&#838;&#1910;&#562;&#1414;&#1451;&#1374;&#1121;&#1624;&#1658;&#1861;&#1894;&#992;&#1784;&#1822;&#1045;&#178;&#587;&#1673;&#1170;&#1218;&#527;&#984;&#666;&#935;&#766;&#1825;&#563;&#1415;&#1355;&#406;&#1491;&#519;&#1920;&#1719;
&#1251;&#1546;&#1369;&#808;&#1759;&#1728;&#294;&#172;&#962;&#1863;&#1563;&#319;&#927;&#1365;&#412;&#49;&#1333;&#1763;&#1900;&#878;&#452;&#810;&#1593;&#1684;&#1034;&#1715;&#339;&#1247;&#1146;&#1372;&#684;&#922;&#1856;&#855;&#1525;&#830;&#650;&#225;&#455;&#1130;
&#1657;&#557;&#1501;&#1966;&#581;&#1436;&#700;&#1893;&#1956;&#1678;&#1738;&#898;&#365;&#1025;&#761;&#299;&#694;&#241;&#1788;&#91;&#1480;&#1277;&#1607;&#58;&#51;&#348;&#83;&#1307;&#291;&#243;&#849;&#1565;&#1032;&#1497;&#773;&#1871;&#748;&#750;&#1110;&#1741;
&#80;&#1285;&#77;&#1527;&#1135;&#1334;&#596;&#1850;&#1432;&#1911;&#1342;&#1725;&#718;&#926;&#110;&#783;&#343;&#1767;&#722;&#1957;&#1015;&#453;&#320;&#755;&#1120;&#814;&#1877;&#361;&#1731;&#633;&#446;&#1930;&#1148;&#1818;&#1613;&#1698;&#381;&#1819;&#324;&#1387;
&#1984;&#67;&#382;&#702;&#884;&#1412;&#1261;&#958;&#1174;&#1186;&#472;&#90;&#60;&#636;&#208;&#1349;&#788;&#1895;&#1574;&#1643;&#662;&#1132;&#809;&#1996;&#511;&#583;&#1915;&#247;&#1106;&#932;&#448;&#1105;&#1363;&#74;&#1126;&#1019;&#933;&#168;&#806;&#1701;
&#717;&#854;&#1645;&#1079;&#1889;&#1127;&#1505;&#822;&#821;&#175;&#528;&#396;&#1965;&#1326;&#985;&#1190;&#43;&#1702;&#199;&#1421;&#637;&#421;&#1062;&#1769;&#486;&#1184;&#234;&#679;&#622;&#240;&#803;&#815;&#1944;&#429;&#416;&#890;&#1952;&#1530;&#1897;&#947;
&#1139;&#1418;&#1053;&#1756;&#218;&#1274;&#1005;&#771;&#1909;&#428;&#327;&#1038;&#1627;&#1322;&#1395;&#607;&#1065;&#1035;&#1457;&#988;&#1938;&#102;&#1939;&#680;&#1540;&#1838;&#625;&#1394;&#364;&#941;&#463;&#117;&#1417;&#1208;&#709;&#1585;&#638;&#664;&#1526;&#837;
&#1987;&#190;&#170;&#34;&#598;&#574;&#973;&#404;&#253;&#1352;&#1289;&#1379;&#552;&#1724;&#1774;&#845;&#245;&#344;&#93;&#224;&#1994;&#1841;&#1067;&#1204;&#1623;&#1310;&#1762;&#160;&#706;&#1729;&#414;&#1531;&#634;&#410;&#1758;&#1188;&#1723;&#440;&#501;&#495;
&#1796;&#672;&#434;&#357;&#239;&#1824;&#109;&#1058;&#971;&#613;&#904;&#974;&#886;&#1634;&#1558;&#1660;&#92;&#372;&#739;&#233;&#1428;&#1700;&#255;&#987;&#963;&#1986;&#923;&#248;&#670;&#1279;&#259;&#1396;&#1545;&#1002;&#1047;&#1402;&#403;&#1158;&#1792;&#1904;
&#1145;&#911;&#616;&#1171;&#1714;&#384;&#606;&#1303;&#50;&#54;&#276;&#373;&#179;&#1125;&#859;&#488;&#1348;&#467;&#315;&#1420;&#1674;&#669;&#1273;&#653;&#800;&#1008;&#888;&#644;&#1357;&#601;&#1797;&#1083;&#289;&#1935;&#460;&#756;&#643;&#1781;&#1018;&#454;
&#1375;&#1212;&#776;&#1785;&#1338;&#1998;&#1052;&#1923;&#1141;&#843;&#1598;&#1031;&#1104;&#124;&#1320;&#1577;&#1885;&#597;&#469;&#1716;&#457;&#553;&#254;&#1552;&#1542;&#1481;&#106;&#767;&#1602;&#1789;&#627;&#1054;&#1143;&#281;&#307;&#1647;&#194;&#283;&#1971;&#564;
&#723;&#1495;&#1707;&#1632;&#1925;&#1498;&#263;&#1569;&#585;&#1250;&#599;&#1095;&#1033;&#337;&#860;&#1024;&#1448;&#1746;&#1016;&#1853;&#1280;&#187;&#1101;&#1112;&#1735;&#1955;&#1444;&#1168;&#842;&#555;&#1927;&#594;&#1747;&#1783;&#833;&#1590;&#1238;&#921;&#1614;&#1041;
&#215;&#691;&#1191;&#1029;&#535;&#85;&#389;&#375;&#200;&#632;&#533;&#1798;&#309;&#1942;&#513;&#610;&#1276;&#1298;&#1122;&#1389;&#470;&#64;&#481;&#560;&#1368;&#1973;&#1118;&#1165;&#1043;&#1243;&#186;&#1790;&#1507;&#724;&#1098;&#867;&#655;&#171;&#1115;&#1679;
&#1581;&#479;&#918;&#1301;&#401;&#508;&#1385;&#1656;&#101;&#1446;&#663;&#1936;&#1677;&#409;&#1050;&#1339;&#831;&#953;&#252;&#807;&#180;&#1411;&#1009;&#676;&#1837;&#951;&#439;&#1128;&#1070;&#1325;&#1151;&#162;&#223;&#1882;&#1554;&#1642;&#1193;&#674;&#1722;&#96;
&#1760;&#956;&#432;&#589;&#1225;&#216;&#78;&#812;&#1928;&#408;&#520;&#1422;&#477;&#1625;&#1271;&#936;&#1080;&#165;&#1367;&#1077;&#959;&#1899;&#823;&#407;&#63;&#880;&#313;&#1437;&#1912;&#95;&#1543;&#192;&#780;&#1473;&#251;&#677;&#301;&#566;&#461;&#1300;
&#1199;&#417;&#789;&#1490;&#1528;&#727;&#977;&#1252;&#1532;&#1264;&#39;&#940;&#1846;&#1134;&#256;&#1588;&#1713;&#1275;&#507;&#1852;&#447;&#1801;&#874;&#1981;&#1136;&#787;&#1809;&#75;&#126;&#1200;&#1454;&#960;&#1196;&#695;&#747;&#1775;&#1663;&#112;&#1042;&#805;
&#1407;&#1555;&#1037;&#1810;&#754;&#1253;&#624;&#1959;&#1692;&#1323;&#846;&#1023;&#369;&#1224;&#998;&#420;&#1699;&#659;&#398;&#1137;&#1947;&#1103;&#226;&#1969;&#443;&#1028;&#195;&#824;&#1600;&#770;&#1429;&#1242;&#876;&#791;&#1443;&#1254;&#1860;&#1745;&#422;&#1814;
&#1988;&#631;&#1737;&#1704;&#529;&#1393;&#1030;&#534;&#431;&#366;&#181;&#1761;&#1091;&#295;&#1766;&#1616;&#817;&#1314;&#593;&#1060;&#1534;&#1633;&#826;&#1583;&#642;&#115;&#1401;&#46;&#1739;&#1214;&#279;&#232;&#749;&#1655;&#98;&#1006;&#1905;&#1876;&#489;&#648;
&#571;&#105;&#875;&#1007;&#1318;&#282;&#1595;&#355;&#1805;&#1517;&#1381;&#1471;&#967;&#1978;&#1858;&#1578;&#1793;&#1388;&#576;&#1075;&#1945;&#549;&#329;&#712;&#1351;&#1891;&#816;&#1226;&#617;&#626;&#546;&#1241;&#458;&#556;&#1433;&#1472;&#1916;&#335;&#1603;&#1069;
&#658;&#1521;&#828;&#1113;&#1092;&#515;&#72;&#1366;&#1567;&#1154;&#802;&#661;&#690;&#444;&#1968;&#1216;&#321;&#1668;&#1976;&#764;&#517;&#1305;&#217;&#55;&#1617;&#383;&#1752;&#1641;&#561;&#1439;&#405;&#853;&#646;&#346;&#892;&#1413;&#176;&#1544;&#62;&#1914;
...


ctrl and extended filtered out:

&#32;&#68;&#113;&#49;&#91;&#58;&#51;&#83;&#80;&#77;&#110;&#67;&#90;&#60;&#74;&#43;&#102;&#117;&#34;&#93;&#109;&#92;&#50;&#54;&#124;&#106;&#85;&#64;&#101;&#96;&#78;&#63;&#95;&#39;&#75;&#126;&#112;&#115;&#46;&#98;&#105;&#72;&#55;&#62;&#122;&#65;&#88;&#86;&#70;&#53;
&#61;&#79;&#103;&#66;&#104;&#89;&#71;&#99;&#45;&#52;&#41;&#69;&#47;&#42;&#97;&#44;&#37;&#119;&#84;&#76;&#111;&#82;&#38;&#87;&#123;&#107;&#100;&#125;&#56;&#108;&#94;&#59;&#48;&#35;&#40;&#33;&#116;&#114;&#118;&#73;&#120;&#36;&#81;&#121;&#57;
```



## Seed7

Seed7 [http://seed7.sourceforge.net/manual/types.htm#string strings] are UTF-32 encoded,
therefore no destinction between BYTE and Unicode strings is necessary.
The example below uses [http://seed7.sourceforge.net/libraries/utf8.htm#STD_UTF8_OUT STD_UTF8_OUT]
from the library [http://seed7.sourceforge.net/libraries/utf8.htm utf8.s7i], to write
Unicode characters with UTF-8 encoding to the console.


```seed7
$ include "seed7_05.s7i";
  include "utf8.s7i";

const func string: stripControl (in string: stri) is func
  result
    var string: stripped is "";
  local
    var integer: old_pos is 1;
    var integer: index is 0;
    var char: ch is ' ';
  begin
    for ch key index range stri do
      if ch < ' ' or ch = '\127;' then
        stripped &:= stri[old_pos .. pred(index)];
        old_pos := succ(index);
      end if;
    end for;
    stripped &:= stri[old_pos ..];
  end func;

const func string: stripControlAndExtended (in string: stri) is func
  result
    var string: stripped is "";
  local
    var integer: old_pos is 1;
    var integer: index is 0;
    var char: ch is ' ';
  begin
    for ch key index range stri do
      if ch < ' ' or ch >= '\127;' then
        stripped &:= stri[old_pos .. pred(index)];
        old_pos := succ(index);
      end if;
    end for;
    stripped &:= stri[old_pos ..];
  end func;

const string: src is "déjà vu\              # Unicode
    \\n\0;\31; \33;\126;\127;\128;\255;\n\  # Various boundary cases
    \as⃝df̅";                                 # Unicode combining characters

const proc: main is func
  begin
    OUT := STD_UTF8_OUT;
    writeln("source text:");
    writeln(src);
    writeln("Stripped of control codes:");
    writeln(stripControl(src));
    writeln("Stripped of control codes and extended characters:");
    writeln(stripControlAndExtended(src));
  end func;
```


Output:

```txt

source text:
déjà vu
� !~ÿ
as⃝df̅
Stripped of control codes:
déjà vu !~ÿas⃝df̅
Stripped of control codes and extended characters:
dj vu !~asdf

```



## Sidef


```ruby
var str = "\ba\x00b\n\rc\fd\xc3\x7ffoo"

var letters = str.chars.map{.ord}
say letters.map{.chr}.join.dump

var nocontrols = letters.grep{ (_ > 32) && (_ != 127) }
say nocontrols.map{.chr}.join.dump

var noextended = nocontrols.grep{ _ < 127 }
say noextended.map{.chr}.join.dump
```

```txt

"\ba\0b\n\rc\fd\xC3\x7Ffoo"
"abcd\xC3foo"
"abcdfoo"

```



## Tcl


```tcl
proc stripAsciiCC str {
    regsub -all {[\u0000-\u001f\u007f]+} $str ""
}
proc stripCC str {
    regsub -all {[^\u0020-\u007e]+} $str ""
}
```


=={{header|TI-83 BASIC}}==
TI-83 BASIC doesn't support ASCII or Unicode, so the following program just strips every character that doesn't have a corresponding glyph from 32 to 126 decimal in a real ASCII table.

The following "normal characters" do exist, but can't be typed on the calculator and a hex editor must be used to enter them:


```ti83b
#$&@;_`abcdefghijklmnopqrstuvwxyz|~
```


The double quote character (ASCII decimal 34) can be entered, but cannot be escaped and thus cannot be stored to strings without the use of hex editors. The following program will remove double quotes from the input string if they were hacked in simply because having one stored to the "check" string is syntactically invalid.

So, in sum, you have to hack the calculator to enter in this program, but once it's entered you can transfer it to unhacked calculators and it will work.


```ti83b
:" !#$%&'()*+,-./0123456789:;<=>?ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"→Str0
:Input ">",Str1
:":"+Str1+":"→Str1
:For(I,2,length(Str1)-2)
:If not(inString(Str0,sub(Str1,I,1)))
:sub(Str1,1,I-1)+sub(Str1,I+1,length(Str1)-(I+1))→Str1
:End
:sub(Str1,2,length(Str1)-1)→Str1
:Pause Str1
```



## TXR


```txrlisp
(defun strip-controls (str)
 (regsub #/[\x0-\x1F\x7F]+/ "" str))

(defun strip-controls-and-extended (str)
  (regsub #/[^\x20-\x7F]+/ "" str))
```



## VBScript

Derived from the BASIC version.

```vb

Function StripCtrlCodes(s)
	tmp = ""
	For i = 1 To Len(s)
		n = Asc(Mid(s,i,1))
		If (n >= 32 And n <= 126) Or n >=128 Then
			tmp = tmp & Mid(s,i,1)
		End If
	Next
	StripCtrlCodes = tmp
End Function

Function StripCtrlCodesExtChrs(s)
	tmp = ""
	For i = 1 To Len(s)
		n = Asc(Mid(s,i,1))
		If n >= 32 And n <= 126 Then
			tmp = tmp & Mid(s,i,1)
		End If
	Next
	StripCtrlCodesExtChrs = tmp
End Function

WScript.StdOut.Write "ab�cd�ef�gh€" & " = " & StripCtrlCodes("ab�cd�ef�gh€")
WScript.StdOut.WriteLine
WScript.StdOut.Write "ab�cd�ef�ghij†klð€" & " = " & StripCtrlCodesExtChrs("ab�cd�ef�ghij†klð€")
WScript.StdOut.WriteLine

```


```txt

ab�cd�ef�gh€ = abcdefgh€
ab�cd�ef�ghij†klð€ = abcdefghijkl

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
string 0;                       \use zero-terminated string convention

proc Strip(Str, Both);          \Strip out control and optionally extended chars
char Str; int Both;
int  I, J, C;
[I:= 0;
while Str(I) do
    [C:= Str(I);
    if Both then C:= extend(C); \if stripping extended chars too, extend sign
    if C<$20 or C=$7F then
        [J:= I;                 \eliminate char by shifting string down over it
        repeat  C:= Str(J+1);
                Str(J):= C;
                J:= J+1;
        until   C=0;
        ]
    else I:= I+1;
    ];
];

char String;
[String:= "Hello^M^J World àáâã";
Text(0, String);  CrLf(0);
Strip(String, false);
Text(0, String);  CrLf(0);
Strip(String, true);
Text(0, String);  CrLf(0);
]
```


Output:

```txt

Hello
 World àáâã
Hello World àáâã
Hello World

```



## zkl

ASCII

```zkl
var ctlCodes=([1..31].pump(String,"toChar") +(127).toChar());
var extdChars=[127..255].pump(String,"toChar");

var test = "string of ☺☻♥♦⌂, control characters(\t\b\e) and other ilk.♫☼§►↔◄";
test.println("<< test string");
(test-ctlCodes).println("<< no control chars");
(test-extdChars).println("<< no extended chars");
(test-extdChars-ctlCodes).println("<< text");

```

```txt

string of ☺☻♥♦⌂, control characters(   and other ilk.♫☼§►↔◄<< test string
string of ☺☻♥♦⌂, control characters() and other ilk.♫☼§►↔◄<< no control chars
string of , control characters(and other ilk.<< no extended chars
string of , control characters() and other ilk.<< text

```


