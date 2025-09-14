+++
title = "Morse code"
description = ""
date = 2019-08-26T20:49:22Z
aliases = []
[extra]
id = 8080
[taxonomies]
categories = ["task", "Sound"]
tags = []
languages = [
  "abap",
  "ada",
  "autohotkey",
  "awk",
  "basic",
  "bbc_basic",
  "befunge",
  "c",
  "clojure",
  "coffeescript",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "easylang",
  "echolisp",
  "elixir",
  "factor",
  "forth",
  "freebasic",
  "fsharp",
  "gambas",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "liberty_basic",
  "lua",
  "mathematica",
  "matlab",
  "ocaml",
  "ol",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "red",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sed",
  "tcl",
  "tuscript",
  "ursa",
  "vba",
  "visual_basic",
  "xpl0",
  "yabasic",
]
+++

{{task|Sound}} {{requires|Sound}} [[Category:Temporal media]]
[[wp:Morse_code|Morse code]] is one of the simplest and most versatile methods of telecommunication in existence.
It has been in use for more than 160 years — longer than any other electronic encoding system.


## Task

Send a string as audible Morse code to an audio device   (e.g., the PC speaker).


As the standard Morse code does not contain all possible characters,
you may either ignore unknown characters in the file,
or indicate them somehow   (e.g. with a different pitch).





## ABAP


```ABAP
REPORT morse_code.
TYPES: BEGIN OF y_morse_code,
         letter TYPE string,
         code   TYPE string,
       END OF y_morse_code,
       ty_morse_code TYPE STANDARD TABLE OF y_morse_code WITH EMPTY KEY.

cl_demo_output=>new(
          )->begin_section( |Morse Code|
          )->write( REDUCE stringtab( LET words = VALUE stringtab( ( |sos|                 )
                                                                   ( |   Hello     World!| )
                                                                   ( |Rosetta Code|        ) )
                                          morse_code = VALUE ty_morse_code( ( letter = 'A'   code = '.-     ' )
                                                                            ( letter = 'B'   code = '-...   ' )
                                                                            ( letter = 'C'   code = '-.-.   ' )
                                                                            ( letter = 'D'   code = '-..    ' )
                                                                            ( letter = 'E'   code = '.      ' )
                                                                            ( letter = 'F'   code = '..-.   ' )
                                                                            ( letter = 'G'   code = '--.    ' )
                                                                            ( letter = 'H'   code = '....   ' )
                                                                            ( letter = 'I'   code = '..     ' )
                                                                            ( letter = 'J'   code = '.---   ' )
                                                                            ( letter = 'K'   code = '-.-    ' )
                                                                            ( letter = 'L'   code = '.-..   ' )
                                                                            ( letter = 'M'   code = '--     ' )
                                                                            ( letter = 'N'   code = '-.     ' )
                                                                            ( letter = 'O'   code = '---    ' )
                                                                            ( letter = 'P'   code = '.--.   ' )
                                                                            ( letter = 'Q'   code = '--.-   ' )
                                                                            ( letter = 'R'   code = '.-.    ' )
                                                                            ( letter = 'S'   code = '...    ' )
                                                                            ( letter = 'T'   code = '-      ' )
                                                                            ( letter = 'U'   code = '..-    ' )
                                                                            ( letter = 'V'   code = '...-   ' )
                                                                            ( letter = 'W'   code = '.-   - ' )
                                                                            ( letter = 'X'   code = '-..-   ' )
                                                                            ( letter = 'Y'   code = '-.--   ' )
                                                                            ( letter = 'Z'   code = '--..   ' )
                                                                            ( letter = '0'   code = '-----  ' )
                                                                            ( letter = '1'   code = '.----  ' )
                                                                            ( letter = '2'   code = '..---  ' )
                                                                            ( letter = '3'   code = '...--  ' )
                                                                            ( letter = '4'   code = '....-  ' )
                                                                            ( letter = '5'   code = '.....  ' )
                                                                            ( letter = '6'   code = '-....  ' )
                                                                            ( letter = '7'   code = '--...  ' )
                                                                            ( letter = '8'   code = '---..  ' )
                                                                            ( letter = '9'   code = '----.  ' )
                                                                            ( letter = ''''  code = '.----. ' )
                                                                            ( letter = ':'   code = '---... ' )
                                                                            ( letter = ','   code = '--..-- ' )
                                                                            ( letter = '-'   code = '-....- ' )
                                                                            ( letter = '('   code = '-.--.- ' )
                                                                            ( letter = '.'   code = '.-.-.- ' )
                                                                            ( letter = '?'   code = '..--.. ' )
                                                                            ( letter = ';'   code = '-.-.-. ' )
                                                                            ( letter = '/'   code = '-..-.  ' )
                                                                            ( letter = '_'   code = '..--.- ' )
                                                                            ( letter = ')'   code = '---..  ' )
                                                                            ( letter = '='   code = '-...-  ' )
                                                                            ( letter = '@'   code = '.--.-. ' )
                                                                            ( letter = '\'   code = '.-..-. ' )
                                                                            ( letter = '+'   code = '.-.-.  ' )
                                                                            ( letter = ' '   code = '/'       ) )
                                      IN INIT word_coded_tab TYPE stringtab
                                          FOR word IN words
                                         NEXT word_coded_tab = VALUE #( BASE word_coded_tab ( REDUCE string( INIT word_coded TYPE string
                                                                                                              FOR index = 1 UNTIL index > strlen( word )
                                                                                                              LET _morse_code = VALUE #( morse_code[ letter = COND #( WHEN index = 1 THEN to_upper( word(index) )
                                                                                                                                                                      ELSE LET prev = index - 1 IN to_upper( word+prev(1) ) ) ]-code OPTIONAL )
                                                                                                               IN NEXT word_coded = |{ word_coded } { _morse_code }| ) ) ) )
          )->display( ).
```



## Ada

Conforms to Ada95.
Works for Windows 32 XP , not for Vista since Beep is no longer effective.

Specification of the package :

```Ada
package Morse is

   type Symbols is (Nul, '-', '.', ' ');
   -- Nul is the letter separator, space the word separator;
   Dash : constant Symbols := '-';
   Dot : constant Symbols := '.';
   type Morse_Str is array (Positive range <>) of Symbols;
   pragma Pack (Morse_Str);

   function Convert (Input : String) return Morse_Str;
   procedure Morsebeep (Input : Morse_Str);

private
   subtype Reschars is Character range ' ' .. 'Z';
   -- restricted set of characters from 16#20# to 16#60#
   subtype Length is Natural range 1 .. 5;
   subtype Codes is Morse_Str (Length);
   -- using the current ITU standard with 5 signs
   -- only alphanumeric characters  are taken into consideration

   type Codings is record
      L : Length;
      Code : Codes;
   end record;
   Table : constant array (Reschars) of Codings :=
     ('A' => (2, ".-   "), 'B' => (4, "-... "),  'C' => (4, "-.-. "),
      'D' => (3, "-..  "), 'E' => (1, ".    "),  'F' => (4, "..-. "),
      'G' => (3, "--.  "), 'H' => (4, ".... "),  'I' => (2, "..   "),
      'J' => (4, ".--- "), 'K' => (3, "-.-  "),  'L' => (4, ".-.. "),
      'M' => (2, "--   "), 'N' => (2, "-.   "),  'O' => (3, "---  "),
      'P' => (4, ".--. "), 'Q' => (4, "--.- "),  'R' => (3, ".-.  "),
      'S' => (3, "...  "), 'T' => (1, "-    "),  'U' => (3, "..-  "),
      'V' => (4, "...- "), 'W' => (3, ".--  "),  'X' => (4, "-..- "),
      'Y' => (4, "-.-- "), 'Z' => (4, "--.. "),  '1' => (5, ".----"),
      '2' => (5, "..---"), '3' => (5, "...--"),  '4' => (5, "....-"),
      '5' => (5, "....."), '6' => (5, "-...."),  '7' => (5, "--..."),
      '8' => (5, "---.."), '9' => (5, "----."),  '0' => (5, "-----"),
      others => (1, "     ")); -- Dummy => Other characters do not need code.

end Morse;
```


```Ada
with Ada.Strings.Maps, Ada.Characters.Handling, Interfaces.C;
use  Ada, Ada.Strings, Ada.Strings.Maps, Interfaces;

package body Morse is

   Dit, Dah, Lettergap, Wordgap : Duration; -- in seconds
   Dit_ms, Dah_ms : C.unsigned; -- durations expressed in ms
   Freq : constant C.unsigned := 1280; -- in Herz

   Morse_Sequence : constant Character_Sequence :=
      " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
   Morse_charset : constant Character_Set := To_Set (Morse_Sequence);

   function Convert (Input : String) return Morse_Str is
      Cap_String : constant String := Characters.Handling.To_Upper (Input);
      Result : Morse_Str (1 .. 7 * Input'Length); -- Upper Capacity
      First, Last : Natural := 0;
      Char_code : Codings;
   begin
      for I in Cap_String'Range loop
         if Is_In (Cap_String (I), Morse_charset) then
            First := Last + 1;
            if Cap_String (I) = ' ' then
               Result (First) := ' ';
               Last := Last + 1;
            else
               Char_code := Table (Reschars (Cap_String (I)));
               Last := First + Char_code.L - 1;
               Result (First .. Last) := Char_code.Code (1 .. Char_code.L);
               Last := Last + 1;
               Result (Last) := Nul;
            end if;
         end if;
      end loop;
      if Result (Last) /= ' ' then
         Last := Last + 1;
         Result (Last) := ' ';
      end if;
      return Result (1 .. Last);
   end Convert;

   procedure Morsebeep (Input : Morse_Str) is
      -- Beep is not portable : adapt to your OS/sound board
      -- Implementation for Windows XP / interface to fn in stdlib
      procedure win32xp_beep
        (dwFrequency : C.unsigned;
         dwDuration : C.unsigned);
      pragma Import (C, win32xp_beep, "_beep");
   begin
      for I in Input'Range loop
         case Input (I) is
            when Nul =>
               delay Lettergap;
            when Dot =>
               win32xp_beep (Freq, Dit_ms);
               delay Dit;
            when Dash =>
               win32xp_beep (Freq, Dah_ms);
               delay Dit;
            when ' ' =>
               delay Wordgap;
         end case;
      end loop;
   end Morsebeep;
begin
   Dit := 0.20;
   Lettergap := 2 * Dit;
   Dah := 3 * Dit;
   Wordgap := 4 * Dit;
   Dit_ms := C.unsigned (Integer (Dit * 1000));
   Dah_ms := C.unsigned (Integer (Dah * 1000));
end Morse;
```

Main program :

```Ada
with Morse;            use Morse;
procedure Morse_Tx is
begin
   Morsebeep (Convert ("Science sans Conscience"));
end Morse_Tx;
```



## AutoHotkey

This function converts acceptable characters into dot-dash notation then uses SoundBeep to play them.
The frequency and element length are stored near the bottom of the script.

```AutoHotkey

TestString := "Hello World! abcdefg @\;" ; Create a string to be sent with multiple caps and some punctuation
MorseBeep(teststring)                    ; Beeps our string after conversion
return                                   ; End Auto-Execute Section


MorseBeep(passedString)
{
	StringLower, passedString, passedString      ; Convert to lowercase for simpler checking
	loop, parse, passedString                    ; This loop stores each character in A_loopField one by one
	{
		If (A_LoopField = " ")
			morse .= "     "  ; Add a long delay between words (5*e)
		If (A_LoopField = "a")
			morse .=".- "     ; Morse is a local variable
		If (A_LoopField = "b")
			morse .="-... "   ; .= is the simple way of appending to a string
		If (A_LoopField = "c")
			morse .="-.-. "   ; we add a space after every character to pause for e
		If (A_LoopField = "d")
			morse .="-.. "
		If (A_LoopField = "e")
			morse .=". "
		If (A_LoopField = "f")
			morse .="..-. "
		If (A_LoopField = "g")
			morse .="--. "
		If (A_LoopField = "h")
			morse .=".... "
		If (A_LoopField = "i")
			morse .=".. "
		If (A_LoopField = "j")
			morse .=".--- "
		If (A_LoopField = "k")
			morse .="-.- "
		If (A_LoopField = "l")
			morse .=".-.. "
		If (A_LoopField = "m")
			morse .="-- "
		If (A_LoopField = "n")
			morse .="-. "
		If (A_LoopField = "o")
			morse .="--- "
		If (A_LoopField = "p")
			morse .=".--. "
		If (A_LoopField = "q")
			morse .="--.- "
		If (A_LoopField = "r")
			morse .=".-. "
		If (A_LoopField = "s")
			morse .="... "
		If (A_LoopField = "t")
			morse .="- "
		If (A_LoopField = "u")
			morse .="..- "
		If (A_LoopField = "v")
			morse .="...- "
		If (A_LoopField = "w")
			morse .=".-- "
		If (A_LoopField = "x")
			morse .="-..- "
		If (A_LoopField = "y")
			morse .="-.-- "
		If (A_LoopField = "z")
			morse .="--.. "
		If (A_LoopField = "!")
			morse .="---. "
		If (A_LoopField = "\")
			morse .=".-..-. "
		If (A_LoopField = "$")
			morse .="...-..- "
		If (A_LoopField = "'")
			morse .=".----. "
		If (A_LoopField = "(")
			morse .="-.--. "
		If (A_LoopField = ")")
			morse .="-.--.- "
		If (A_LoopField = "+")
			morse .=".-.-. "
		If (A_LoopField = ",")
			morse .="--..-- "
		If (A_LoopField = "-")
			morse .="-....- "
		If (A_LoopField = ".")
			morse .=".-.-.- "
		If (A_LoopField = "/")
			morse .="-..-. "
		If (A_LoopField = "0")
			morse .="----- "
		If (A_LoopField = "1")
			morse .=".---- "
		If (A_LoopField = "2")
			morse .="..--- "
		If (A_LoopField = "3")
			morse .="...-- "
		If (A_LoopField = "4")
			morse .="....- "
		If (A_LoopField = "5")
			morse .="..... "
		If (A_LoopField = "6")
			morse .="-.... "
		If (A_LoopField = "7")
			morse .="--... "
		If (A_LoopField = "8")
			morse .="---.. "
		If (A_LoopField = "9")
			morse .="----. "
		If (A_LoopField = ":")
			morse .="---... "
		If (A_LoopField = ";")
			morse .="-.-.-. "
		If (A_LoopField = "=")
			morse .="-...- "
		If (A_LoopField = "?")
			morse .="..--.. "
		If (A_LoopField = "@")
			morse .=".--.-. "
		If (A_LoopField = "[")
			morse .="-.--. "
		If (A_LoopField = "]")
			morse .="-.--.- "
		If (A_LoopField = "_")
			morse .="..--.- "
	}                                                  ; ---End conversion loop---

	Freq=1280    ; Frequency between 37 and 32767
	e=120        ; element time in milliseconds
		     ; . is one e, - is 3, and a space is a pause of one e
	loop, parse, morse
	{
		if (A_LoopField = ".")
			SoundBeep, Freq, e          ;Format: SoundBeep, frequency, duration

		If (A_LoopField = "-")
			SoundBeep, Freq, 3*e        ; duration can be an expression

		If (A_LoopField = " ")
			Sleep, e                    ; Above, each character is followed by a space, and literal
	}                                           ; spaces are extended. Sleep pauses the script.
} ;                                                         ---End Function Morse---

```




## AWK

AWK cannot play sounds by itself,
so here we just translate text to dits and dots:

```AWK
# usage: awk -f morse.awk [inputfile]
BEGIN { FS="";
 m="A.-B-...C-.-.D-..E.F..-.G--.H....I..J.---K-.-L.-..M--N-.";
 m=m "O---P.--.Q--.-R.-.S...T-U..-V...-W.--X-..-Y-.--Z--..  ";
}

{ for(i=1; i<=NF; i++)
  {
    c=toupper($i); n=1; b=".";

    while((c!=b)&&(b!=" ")) { b=substr(m,n,1); n++; }

    b=substr(m,n,1);

    while((b==".")||(b=="-")) { printf("%s",b); n++; b=substr(m,n,1); }

    printf("|");
  }
  printf("\n");
}

```


{{Out}} with input "sos sos titanic"

```txt

...|---|...||...|---|...||-|..|-|.-|-.|..|-.-.|

```




## BASIC

This replaces all non-supported characters with a hash ('''#''') and plays the lowest supported note in their place.

Note that this will ''only'' work as-is under [[DOS]] (and [[Windows]] 9x); under NT systems, the <code>player</code> routine must be changed to use the <code>Beep</code> API call. ([http://www.freebasic.net/forum/viewtopic.php?p=20441#20441 This forum post] details how to use the speaker under Linux, DOS, and Windows in FreeBASIC; the Linux & DOS code differs further from the below by require inline assembly.)


```qbasic
DECLARE SUB player (what AS STRING)

'this determines the length of the notes
'lower number = longer duration
CONST noteLen = 16

DIM tones(62) AS STRING

FOR n% = 0 TO 62
    READ tones(n%)
NEXT n%

'set up the playing system
PLAY "t255o4l" + LTRIM$(STR$(noteLen))

LINE INPUT "String to convert to Morse code: "; x$

FOR n% = 1 TO LEN(x$)
    c$ = UCASE$(MID$(x$, n%, 1))
    PLAY "p" + LTRIM$(STR$(noteLen / 2)) + "."
    SELECT CASE UCASE$(c$)
        CASE " "
            'since each char is effectively wrapped with 6 p's, we only need to add 1:
            PLAY "p" + LTRIM$(STR$(noteLen))
            PRINT "  ";
        CASE "!" TO "_"
            PRINT tones(ASC(c$) - 33); " ";
            player tones(ASC(c$) - 33)
        CASE ELSE
            PRINT "# ";
            player "#"
    END SELECT
    PLAY "p" + LTRIM$(STR$(noteLen / 2)) + "."
NEXT n%
PRINT

'all the Morse codes in ASCII order, from "!" to "_"
DATA "-.-.--", ".-..-.", "#", "...-..-", "#", ".-...", ".----.", "-.--."
DATA "-.--.-", "#", ".-.-.", "--..--", "-....-", ".-.-.-", "-..-.", "-----"
DATA ".----", "..---", "...--", "....-", ".....", "-....", "--...", "---.."
DATA "----.", "---...", "-.-.-.", "#", "-...-", "#", "..--..", ".--.-.", ".-"
DATA "-...", "-.-.", "-..", ".", "..-.", "--.", "....", "..", ".---", "-.-"
DATA ".-..", "--", "-.", "---", ".--.", "--.-", ".-.", "...", "-", "..-"
DATA "...-", ".--", "-..-", "-.--", "--..", "#", "#", "#", "#", "..--.-"

SUB player (what AS STRING)
    FOR i% = 1 TO LEN(what)
        z$ = MID$(what, i%, 1)
        SELECT CASE z$
            CASE "."
                o$ = "g"
            CASE "-"
                o$ = "g" + LTRIM$(STR$(noteLen / 2)) + "."
            CASE ELSE
                o$ = "<<<<c>>>>"
        END SELECT
        PLAY o$
        PLAY "p" + LTRIM$(STR$(noteLen))
    NEXT i%
END SUB
```


{{out}} (2 runs):

```txt

 String to convert to Morse code: Rosetta Code
 .-. --- ... . - - .-   -.-. --- -.. .
 String to convert to Morse code: ~!@#$%^&*()_+
 # -.-.-- .--.-. # ...-..- # # .-... # -.--. -.--.- ..--.- .-.-.

```


=
## BBC BASIC
=
```bbcbasic
      *TEMPO 8
      DIM morse$(63)
      FOR char% = 0 TO 63 : READ morse$(char%) : NEXT char%

      PROCmorse("The five boxing wizards jump quickly.")
      END

      DEF PROCmorse(text$)
      LOCAL element%, index%, char&, morse$
      FOR index% = 1 TO LEN(text$)
        char& = ASC(MID$(text$,index%)) AND &7F
        IF char& < 32 char& = 32
        IF char& > 95 char& -= 32
        morse$ = morse$(char&-32)
        FOR element% = 1 TO LEN(morse$)
          SOUND 1, -15, 148, VAL(MID$(morse$,element%,1))
          SOUND 1, -15, 0, 1
        NEXT element%
        SOUND 1, -15, 0, 2
      NEXT index%
      ENDPROC

      DATA 00,313133,131131,6,1113113,6,13111,133331,31331,313313,6,13131,331133,311113,131313,31131
      DATA 33333,13333,11333,11133,11113,11111,31111,33111,33311,33331,333111,313131,6,31113,6,113311
      DATA 133131,13,3111,3131,311,1,1131,331,1111,11,1333,313,1311,33,31,333
      DATA 1331,3313,131,111,3,113,1113,133,3113,3133,3311,6,6,6,6,113313
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Morse.bas"
110 STRING TONE$(48 TO 90)*5,ST$*254
120 SET CHARACTER 46,0,0,0,0,24,24,0,0,0:SET CHARACTER 47,0,0,0,0,126,126,0,0,0
130 FOR I=48 TO 90
140   READ TONE$(I)
150 NEXT
160 DO
170   PRINT :PRINT "String to convert to Morse code: ":INPUT PROMPT ">":ST$
180   LET ST$=LTRIM$(RTRIM$(UCASE$(ST$)))
190   FOR I=1 TO LEN(ST$)
200     LET C=ORD(ST$(I:I))
210     IF C>47 AND C<91 THEN
220       PRINT TONE$(C);" ";
230       FOR J=1 TO LEN(TONE$(C))
240         SOUND PITCH 48,DURATION(ORD(TONE$(C)(J))-45)^3+4
250         SOUND PITCH 126,DURATION 8
260       NEXT
270     ELSE
280       PRINT
290       SOUND PITCH 126,DURATION 16
300     END IF
310     SOUND PITCH 126,DURATION 16
320   NEXT
330   PRINT
340 LOOP UNTIL ST$=""
350 CLEAR FONT
360 DATA .////,..///,...//,..../,.....,/....,//...,///..,////.,/////,"","","","","","",""
370 DATA ./,/...,/./.,/..,.,../.,//.,....,..,.///,/./,./..,//,/.,///,.//.,//./,./.,...,/,../,.../,.//,/../,/.//,//..
```



## Befunge

Since Befunge doesn't have a way to output sound, this implementation just generates a text representation of the morse. The string to convert is read from stdin.


```befunge>
~>48*-:0\`#@_2*::"!"%\"!"/3+g75v
^v('_')v!:-*57g+3/"!"\%"!":+1\-*<
^$$,*84_\#!:#:2#-%#15#\9#/*#2+#,<
##X)P)##Z*##3(D)5);(##8(/)A)8)9(#
($(&(*(2(B(A(?(;(3([)M)##1(##V)L)
$%1'-')&$$.''&2'&%$'%&0'#%%%#&,''
'(&*&#$&&*'$&)'%'/'########6)##$%
1'-')&$$.''&2'&%$'%&0'#%%%#&,'''(
&*&#$&&*'$&)'%'/'################
```


```txt
Hello World!
.... . .-.. .-.. ---  .-- --- .-. .-.. -.. .-.-..
```



## C


One could substitute another program for  ubuntu beep  command.


```C

/*

  David Lambert, 2010-Dec-09

  filter producing morse beep commands.

  build:
    make morse

  use:
    $ echo tie a. | ./morse
    beep -n -f 440 -l 300 -D 100 -n -D 200 -n -f 440 -l 100 -D 100 -n -f 440 -l 100 -D 100 -n -D 200 -n -f 440 -l 100 -D 100 -n -D 200 -n -D 400 -n -f 440 -l 100 -D 100 -n -f 440 -l 300 -D 100 -n -D 200 -n -f 440 -l 100 -D 100 -n -f 440 -l 300 -D 100 -n -f 440 -l 100 -D 100 -n -f 440 -l 300 -D 100 -n -f 440 -l 100 -D 100 -n -f 440 -l 300 -D 100 -n -D 200 -n -D 400 -n -D 400

  bugs:
    What is the space between letter and punctuation?
    Demo truncates input lines at 71 characters or so.

 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BIND(A,L,H) ((L)<(A)?(A)<(H)?(A):(H):(L))
/*
  BIND(-1,0,9) is 0
  BIND( 7,0,9) is 7
  BIND(77,0,9) is 9
*/

char
  /* beep args for */
  /* dit  dah     extra space */
  dih[50],dah[50],medium[30],word[30],
  *dd[2] = {dih,dah};
const char
  *ascii = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.,?'!/()&:;=+-_\"$@",
  *itu[] = {
     "13","3111","3131","311","1","1131","331","1111","11","1333","313","1311","33","31","333","1331","3313","131","111","3","113","1113","133","3113","3133","3311","33333","13333","11333","11133","11113","11111","31111","33111","33311","33331","131313","331133","113311","133331","313133","31131","31331","313313","13111","333111","313131","31113","13131","311113","113313","131131","1113113","133131"
  };

void append(char*s,const char*morse) {
  for (; *morse; ++morse)
    strcat(s,dd['3'==*morse]);
  strcat(s,medium);
}

char*translate(const char*i,char*o) {
  const char*pc;
  sprintf(o,"beep");
  for (; *i; ++i)
    if (NULL == (pc = strchr(ascii,toupper(*i))))
      strcat(o,word);
    else
      append(o,itu[pc-ascii]);
  strcat(o,word);
  return o;
}

int main(int ac,char*av[]) {
  char
    sin[73],sout[100000];
  int
    dit = 100;
  if (1 < ac) {
    if (strlen(av[1]) != strspn(av[1],"0123456789"))
      return 0*fprintf(stderr,"use: %s [duration]   dit in ms, default %d\n",*av,dit);
    dit = BIND(atoi(av[1]),1,1000);
  }
  sprintf(dah," -n -f 440 -l %d -D %d",3*dit,dit);
  sprintf(dih," -n -f 440 -l %d -D %d",dit,dit);
  sprintf(medium," -n -D %d",(3-1)*dit);
  sprintf(word," -n -D %d",(7-(3-1)-1)*dit);
  while (NULL != fgets(sin,72,stdin))
    puts(translate(sin,sout));
  return 0;
}

```


## C++


```C++

/*
Michal Sikorski
06/07/2016
*/
#include <cstdlib>
#include <iostream>
#include <windows.h>
#include <string.h>
using namespace std;
int main(int argc, char *argv[])
{
    string inpt;
    char ascii[28] = " ABCDEFGHIJKLMNOPQRSTUVWXYZ", lwcAscii[28] = " abcdefghijklmnopqrstuvwxyz";
    string morse[27] = {"  ", ".- ", "-... ", "-.-. ", "-.. ", ". ", "..-. ", "--. ", ".... ", ".. ", ".--- ", "-.- ", ".-.. ", "-- ", "-. ", "--- ", ".--.", "--.- ", ".-. ", "... ", "- ", "..- ", "...- ", ".-- ", "-..- ", "-.-- ", "--.. "};
    string outpt;
    getline(cin,inpt);
    int xx=0;
    int size = inpt.length();
    cout<<"Length:"<<size<<endl;

    xx=0;
    while(xx<inpt.length())
    {
                         int x=0;
                         bool working = false;
                         while(!working)
                         {
                                  if(ascii[x] != inpt[xx]&&lwcAscii[x] != inpt[xx])
                                  {
                                        x++;
                                  }
                                  else
                                  {
                                        working = !working;
                                  }
                         }

                         cout<<morse[x];
                         outpt = outpt + morse[x];
                         xx++;
    }

    xx=0;
    while(xx<outpt.length()+1)
    {
                         if(outpt[xx] == '.')
                         {
                                   Beep(1000,250);
                         }
                         else
                         {
                             if(outpt[xx] == '-')
                             {
                                          Beep(1000,500);
                             }
                             else
                             {
                                 if(outpt[xx] == ' ')
                                 {
                                              Sleep(500);
                                 }
                             }
                         }
                         xx++;
    }
    system("PAUSE");
    return EXIT_SUCCESS;
}


```

## C#

```c#
using System;
using System.Collections.Generic;

namespace Morse
{
    class Morse
    {
        static void Main(string[] args)
        {
            string word = "sos";
            Dictionary<string, string> Codes = new Dictionary<string, string>
            {
                {"a", ".-   "}, {"b", "-... "}, {"c", "-.-. "}, {"d", "-..  "},
                {"e", ".    "}, {"f", "..-. "}, {"g", "--.  "}, {"h", ".... "},
                {"i", "..   "}, {"j", ".--- "}, {"k", "-.-  "}, {"l", ".-.. "},
                {"m", "--   "}, {"n", "-.   "}, {"o", "---  "}, {"p", ".--. "},
                {"q", "--.- "}, {"r", ".-.  "}, {"s", "...  "}, {"t", "-    "},
                {"u", "..-  "}, {"v", "...- "}, {"w", ".--  "}, {"x", "-..- "},
                {"y", "-.-- "}, {"z", "--.. "}, {"0", "-----"}, {"1", ".----"},
                {"2", "..---"}, {"3", "...--"}, {"4", "....-"}, {"5", "....."},
                {"6", "-...."}, {"7", "--..."}, {"8", "---.."}, {"9", "----."}
            };

            foreach (char c in word.ToCharArray())
            {
                string rslt = Codes[c.ToString()].Trim();
                foreach (char c2 in rslt.ToCharArray())
                {
                    if (c2 == '.')
                        Console.Beep(1000, 250);
                    else
                        Console.Beep(1000, 750);
                }
                System.Threading.Thread.Sleep(50);
            }
        }
    }
}

```



## Clojure


```Clojure
(import [javax.sound.sampled AudioFormat AudioSystem SourceDataLine])

(defn play [sample-rate bs]
  (let [af (AudioFormat. sample-rate 8 1 true true)]
    (doto (AudioSystem/getSourceDataLine af)
      (.open af sample-rate)
      .start
      (.write bs 0 (count bs))
      .drain
      .close)))

(defn note [hz sample-rate ms]
  (let [period (/ hz sample-rate)]
    (->> (range (* sample-rate ms 1/1000))
	 (map #(->> (* 2 Math/PI % period)
		    Math/sin
		    (* 127 ,)
		    byte) ,))))

(def morse-codes
     {\A ".-"   \J ".---" \S "..."   \1 ".----" \. ".-.-.-" \: "---..."
      \B "-..." \K "-.-"  \T "-"     \2 "..---" \, "--..--" \; "-.-.-."
      \C "-.-." \L ".-.." \U "..-"   \3 "...--" \? "..--.." \= "-...-"
      \D "-.."  \M "--"   \V "...-"  \4 "....-" \' ".----." \+ ".-.-."
      \E "."    \N "-."   \W ".--"   \5 "....." \! "-.-.--" \- "-....-"
      \F "..-." \O "---"  \X "-..-"  \6 "-...." \/ "-..-."  \_ "..--.-"
      \G "--."  \P ".--." \Y "-.--"  \7 "--..." \( "-.--."  \" ".-..-."  ;"
      \H "...." \Q "--.-" \Z "--.."  \8 "---.." \) "-.--.-" \$ "...-..-"
      \I ".."   \R ".-."  \0 "-----" \9 "----." \& ".-..."  \@ ".--.-."
      \space " "})

(def sample-rate 1024)

(let [hz 440
      ms 50]
  (def sounds
       {\. (note hz sample-rate (* 1 ms))
	\- (note hz sample-rate(* 3 ms))
	:element-gap (note 0 sample-rate (* 1 ms))
	:letter-gap (note 0 sample-rate (* 3 ms))
	\space (note 0 sample-rate (* 1 ms))})) ;includes letter-gap on either side

(defn convert-letter [letter]
  (->> (get morse-codes letter "")
       (map sounds ,)
       (interpose (:element-gap sounds) ,)
       (apply concat ,)))

(defn morse [s]
  (->> (.toUpperCase s)
       (map convert-letter ,)
       (interpose (:letter-gap sounds) ,)
       (apply concat ,)
       byte-array
       (play sample-rate ,)))
```



## CoffeeScript


```coffeescript
class Morse
  constructor : (@unit=0.05, @freq=700) ->

    @cont = new AudioContext()
    @time = @cont.currentTime
    @alfabet = "..etianmsurwdkgohvf.l.pjbxcyzq..54.3...2.......16.......7...8.90"

  getCode : (letter) ->
    i = @alfabet.indexOf letter
    result = ""
    while i > 1
      result = ".-"[i%2] + result
      i //= 2
    result

  makecode : (data) ->
    for letter in data
      code = @getCode letter
      if code != undefined then @maketime code else @time += @unit * 7

  maketime : (data) ->
    for timedata in data
      timedata = @unit * ' . _'.indexOf timedata
      if timedata > 0
        @maketone timedata
        @time += timedata
        @time += @unit * 1
    @time += @unit * 2

  maketone : (data) ->
    start = @time
    stop = @time + data
    @gain.gain.linearRampToValueAtTime 0, start
    @gain.gain.linearRampToValueAtTime 1, start + @unit / 8
    @gain.gain.linearRampToValueAtTime 1, stop - @unit / 16
    @gain.gain.linearRampToValueAtTime 0, stop

  send : (text) ->
    osci = @cont.createOscillator()
    osci.frequency.value = @freq
    @gain = @cont.createGain()
    @gain.gain.value = 0
    osci.connect @gain
    @gain.connect @cont.destination

    osci.start @time
    @makecode text
    @cont

morse = new Morse()
morse.send 'hello world 0123456789'
```



## D


```d

import std.conv;
import std.stdio;

immutable string[char] morsecode;

static this() {
    morsecode = [
        'a': ".-",
        'b': "-...",
        'c': "-.-.",
        'd': "-..",
        'e': ".",
        'f': "..-.",
        'g': "--.",
        'h': "....",
        'i': "..",
        'j': ".---",
        'k': "-.-",
        'l': ".-..",
        'm': "--",
        'n': "-.",
        'o': "---",
        'p': ".--.",
        'q': "--.-",
        'r': ".-.",
        's': "...",
        't': "-",
        'u': "..-",
        'v': "...-",
        'w': ".--",
        'x': "-..-",
        'y': "-.--",
        'z': "--..",
        '0': "-----",
        '1': ".----",
        '2': "..---",
        '3': "...--",
        '4': "....-",
        '5': ".....",
        '6': "-....",
        '7': "--...",
        '8': "---..",
        '9': "----."
    ];
}

void main(string[] args) {
    foreach (arg; args[1..$]) {
        writeln(arg);
        foreach (ch; arg) {
            if (ch in morsecode) {
                write(morsecode[ch]);
            }
            write(' ');
        }
        writeln();
    }
}

```



## Delphi


```Delphi

program Morse;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Generics.Collections,
  SysUtils,
  Windows;

const
  Codes: array[0..35, 0..1] of string =
   (('a', '.-   '), ('b', '-... '), ('c', '-.-. '), ('d', '-..  '),
    ('e', '.    '), ('f', '..-. '), ('g', '--.  '), ('h', '.... '),
    ('i', '..   '), ('j', '.--- '), ('k', '-.-  '), ('l', '.-.. '),
    ('m', '--   '), ('n', '-.   '), ('o', '---  '), ('p', '.--. '),
    ('q', '--.- '), ('r', '.-.  '), ('s', '...  '), ('t', '-    '),
    ('u', '..-  '), ('v', '...- '), ('w', '.--  '), ('x', '-..- '),
    ('y', '-.-- '), ('z', '--.. '), ('0', '-----'), ('1', '.----'),
    ('2', '..---'), ('3', '...--'), ('4', '....-'), ('5', '.....'),
    ('6', '-....'), ('7', '--...'), ('8', '---..'), ('9', '----.'));
var
  Dictionary: TDictionary<String, String>;

procedure InitCodes;
var
  i: Integer;
begin
  for i := 0 to High(Codes) do
    Dictionary.Add(Codes[i, 0], Codes[i, 1]);
end;

procedure SayMorse(const Word: String);
var
  s: String;
begin
  for s in Word do
    if s = '.' then
      Windows.Beep(1000, 250)
    else if s = '-' then
      Windows.Beep(1000, 750)
    else
      Windows.Beep(1000, 1000);
end;

procedure ParseMorse(const Word: String);
var
  s, Value: String;
begin
  for s in word do
    if Dictionary.TryGetValue(s, Value) then
    begin
      Write(Value + ' ');
      SayMorse(Value);
    end;
end;

begin
  Dictionary := TDictionary<String, String>.Create;
  try
    InitCodes;
    if ParamCount = 0 then
      ParseMorse('sos')
    else if ParamCount = 1 then
      ParseMorse(LowerCase(ParamStr(1)))
    else
      Writeln('Usage: Morse.exe anyword');

    Readln;
  finally
    Dictionary.Free;
  end;
end.
```



## EasyLang

<lang>abc$[] = str_split "abcdefghijklmnopqrstuvwxyz "
mc$[] = [ ".-" "-..." "-.-." "-.." "." "..-."
          "--." "...." ".---" ".---" "-.-"
          ".-.." "--" "-." "---" ".--." "--.-"
          ".-." "..." "-" "..-" "...-" ".--"
          "-..-" "-.--" "--.." " " ]
#
func letter c$ . .
  j = 0
  while j < len abc$[] and abc$[j] <> c$
    j += 1
  .
  if j < len abc$[]
    m$ = mc$[j]
  else
    m$ = "x"
  .
  print c$ & " " & m$
  m$[] = str_split m$
  for j range len m$[]
    if m$[j] = "."
      sound [ 440 0.2 ]
      sleep 0.4
    elif m$[j] = "-"
      sound [ 440 0.6 ]
      sleep 0.8
    elif m$[j] = " "
      sleep 0.8
    else
      # error
      sound [ 880 0.6 ]
      sleep 0.8
    .
  .
  sleep 0.4
.
#
txt$[] = str_split "sos sos"
for i range len txt$[]
  call letter txt$[i]
.
```



## EchoLisp


```scheme

(require 'json)
(require 'hash)
(require 'timer)

;; json table from RUBY
(define morse-alphabet
#'{"0":"-----","1":".----","2":"..---","3":"...--","4":"....-","5":".....","6":"-....","7":"--...","8":"---..","9":"----.","!":"---.","$":"...-..-","'":".----.","(":"-.--.",")":"-.--.-","+":".-.-.",",":"--..--","-":"-....-",".":".-.-.-","/":"-..-.",":":"---...",";":"-.-.-.","=":"-...-","?":"..--..","@":".--.-.","A":".-","B":"-...","C":"-.-.","D":"-..","E":".","F":"..-.","G":"--.","H":"....","I":"..","J":".---","K":"-.-","L":".-..","M":"--","N":"-.","O":"---","P":".--.","Q":"--.-","R":".-.","S":"...","T":"-","U":"..-","V":"...-","W":".--","X":"-..-","Y":"-.--","Z":"--..","[":"-.--.","]":"-.--.-","_":"..--.-"," ":"|"}'#)

(define MORSE (json->hash (string->json morse-alphabet)))

;; translates a string into morse string
;; use "|" as letters separator
(define (string->morse str morse)
(apply append (map string->list
	(for/list [(a (string-diacritics str))]
		(string-append
		(or (hash-ref morse (string-upcase a)) "?") "|")))))

(define (play-morse)
	(when EMIT ;; else return #f which stops (at-every)
		(case  (first EMIT)
		((".") (play-sound 'digit) (write "dot"))
		(("-") (play-sound 'tick) (write "dash"))
		(else (writeln) (blink)))
		(set! EMIT (rest EMIT))))

```

```txt

(define EMIT (string->morse "Longtemps je me suis couché de bônne heure" MORSE))

    → ("." "-" "." "." "|" "-"  "." "-" "-" "." " [ ... ] "-" "." "|" "-" "." "|" "." "|" "|" "|" "."
"." "." "." "|" "." "|" "." "." "-" "|" "." "-" "." "|" "." "|")

;; speakers ON
(at-every 300 'play-morse)

```



## Elixir


```elixir
defmodule Morse do
  @morse %{"!" => "---.",       "\"" => ".-..-.",    "$" => "...-..-",    "'" => ".----.",
           "(" => "-.--.",      ")" => "-.--.-",     "+" => ".-.-.",      "," => "--..--",
           "-" => "-....-",     "." => ".-.-.-",     "/" => "-..-.",
           "0" => "-----",      "1" => ".----",      "2" => "..---",      "3" => "...--",
           "4" => "....-",      "5" => ".....",      "6" => "-....",      "7" => "--...",
           "8" => "---..",      "9" => "----.",
           ":" => "---...",     ";" => "-.-.-.",     "=" => "-...-",      "?" => "..--..",
           "@" => ".--.-.",
           "A" => ".-",         "B" => "-...",       "C" => "-.-.",       "D" => "-..",
           "E" => ".",          "F" => "..-.",       "G" => "--.",        "H" => "....",
           "I" => "..",         "J" => ".---",       "K" => "-.-",        "L" => ".-..",
           "M" => "--",         "N" => "-.",         "O" => "---",        "P" => ".--.",
           "Q" => "--.-",       "R" => ".-.",        "S" => "...",        "T" => "-",
           "U" => "..-",        "V" => "...-",       "W" => ".--",        "X" => "-..-",
           "Y" => "-.--",       "Z" => "--..",
           "[" => "-.--.",      "]" => "-.--.-",     "_" => "..--.-" }
  def code(text) do
    String.upcase(text)
    |> String.codepoints
    |> Enum.map_join(" ", fn c -> Map.get(@morse, c, " ") end)
  end
end

IO.puts Morse.code("Hello, World!")
```


```txt

.... . .-.. .-.. --- --..--   .-- --- .-. .-.. -.. ---.

```



## Factor


```factor
USE: morse
"Hello world!" play-as-morse
```



## Forth

Starting with basic I/O operations, this demonstration creates a simple hardware driver for the PC speaker to generate tones.
The Morse code generator could have taken the form of a look-up array with bit patterns cross referencing dits and dahs, but we instead used the Forth dictionary, which is functionally like a big case statement. By making each letter an executable routine in Forth we get a very succinct way to code our Morse. Proper sounding morse code requires precise time delays between the dits, dahs, letters and words. The program uses the Forth word "ms" which delays for n milliseconds and gives us real time control. This morse code sounds right.
There is a lot accomplished in 75 lines of code for a "low level" language.
<lang>
HEX
\ PC speaker hardware control (requires GIVEIO or DOSBOX for windows operation)
 042 constant fctrl
 043 constant tctrl
 061 constant sctrl
 0FC constant smask

\ PC@ is Port char fetch (Intel IN instruction).  PC! is port char store (Intel OUT instruction)
: speak     ( -- ) sctrl pc@               03 or  sctrl pc! ;
: silence   ( -- ) sctrl pc@   smask and   01 or  sctrl pc! ;

: tone     ( freq -- )                 \ freq is actually just a divisor value
            ?dup                       \ check for non-zero input
            if   0B6 tctrl pc!         \ enable PC speaker
                 dup fctrl pc!         \ set freq
                 8 rshift fctrl pc!
                 speak
            else
                 silence
            then ;

\ morse demonstration begins here
DECIMAL
1000 value freq                        \ arbitrary value that sounded ok
  90 value adit                        \ 1 dit will be 90 ms

: dit_dur      adit ms ;
: dah_dur      adit 3 * ms ;
: wordgap      adit 5 * ms ;
: off_dur      adit 2/ ms ;
: lettergap    dah_dur ;

: sound ( -- ) freq tone ;

: MORSE-EMIT  ( char -- )
        dup  bl =                      \ check for space character
        if
             wordgap drop              \ and delay if detected
        else
             pad C!                    \ write char to buffer
             pad 1 evaluate            \ evaluate 1 character
             lettergap                 \ pause for correct sounding morse code
        then ;

: TRANSMIT ( ADDR LEN -- )
           cr                          \ newline,
           bounds                      \ convert loop indices to address ranges
           do
              I C@ dup emit            \ dup and send char to console
              morse-emit               \ send the morse code
           loop ;

VOCABULARY MORSE                       \ prevent name conflicts with letters and numbers

MORSE DEFINITIONS                      \ the following definitions go into MORSE namespace
: .   ( -- ) sound  dit_dur  silence off_dur ;
: -   ( -- ) sound  dah_dur  silence off_dur ;

\ define morse letters as Forth words. They transmit when executed

: A  . -  ;     : B  - . . . ;   : C  - . - . ;    : D  - . . ;
: E  . ;        : F  . . - . ;   : G  - - . ;      : H  . . . . ;
: I  . . ;      : J  . - - - ;   : K  - . - ;      : L  . - . . ;
: M  - - ;      : N  - . ;       : O  - - - ;      : P  . - - . ;
: Q  - - . - ;  : R  . - . ;     : S  . . . ;      : T  - ;
: U  . . - ;    : V  . . . - ;   : W  . - - ;      : X  - . . - ;
: Y  - . - - ;  : Z  - - . . ;

: 0  - - - - - ;     : 1  . - - - - ;
: 2  . . - - - ;     : 3  . . . - - ;
: 4  . . . . - ;     : 5  . . . . . ;
: 6  - . . . . ;     : 7  - - . . . ;
: 8  - - - . . ;     : 9  - - - - . ;

: '  - . . - . ;
: \  . - - - . ;
: !  . - . - . ;
: ?  . . - - . . ;
: ,  - - . . - - ;
: /   _ . . - . ;
: .  . - . - . - ;

 PREVIOUS DEFINITIONS                   \ go back to previous namespace
: TRANSMIT   MORSE  TRANSMIT  PREVIOUS ;
```


Test at the Forth console

```txt
S" CQ CQ CQ DE VE3CFW VE3CFW  K " TRANSMIT
CQ CQ CQ DE VE3CFW VE3CFW  K  ok
MORSE  A B C D E F G  PREVIOUS ok
```



## FreeBASIC


```freebasic

' FB 1.05.0 Win64

' Using Beep function in Win32 API
Dim As Any Ptr library = DyLibLoad("kernel32")
Dim Shared beep_ As Function (ByVal As ULong, ByVal As ULong) As Long
beep_ = DyLibSymbol(library, "Beep")

Sub playMorse(m As String)
  For i As Integer = 0 To Len(m) - 1
    If m[i] = 46 Then '' ascii code for dot
      beep_(1000, 250)
    Else '' must be ascii code for dash (45)
      beep_(1000, 750)
    End If
    Sleep 50
  Next
  Sleep 150
End Sub

Dim morse(0 To 35) As String => _
{           _
   ".-",    _  '' a
   "-...",  _  '' b
   "-.-.",  _  '' c
   "-..",   _  '' d
   ".",     _  '' e
   "..-.",  _  '' f
   "--.",   _  '' g
   "....",  _  '' h
   "..",    _  '' i
   ".---",  _  '' j
   "-.-",   _  '' k
   ".-..",  _  '' l
   "--",    _  '' m
   "-.",    _  '' n
   "---",   _  '' o
   ".--.",  _  '' p
   "--.-",  _  '' q
   ".-.",   _  '' r
   "...",   _  '' s
   "-",     _  '' t
   "..-",   _  '' u
   "...-",  _  '' v
   ".--",   _  '' w
   "-..-",  _  '' x
   "-.--",  _  '' y
   "--..",  _  '' z
   "-----", _  '' 0
   ".----", _  '' 1
   "..---", _  '' 2
   "...--", _  '' 3
   "....-", _  '' 4
   ".....", _  '' 5
   "-....", _  '' 6
   "--...", _  '' 7
   "---..", _  '' 8
   "----."  _  '' 9
}

Dim s As String = "The quick brown fox"
For i As Integer = 0 To Len(s) -1
  Select Case As Const s[i]
    Case 65 To 90  '' A - Z
      playMorse(morse(s[i] - 65))
    Case 97 To 122 '' a - z
      playMorse(morse(s[i] - 97))
    Case 48 To 57  '' 0 - 9
       playMorse(morse(s[i] - 22))
    Case Else
       '' ignore any other character
       Sleep 250
  End Select
Next

DyLibFree(library)
End

```



## F#


```fsharp

open System
open System.Threading

let morse = Map.ofList
                [('a', "._ "); ('b', "_... "); ('c', "_._. "); ('d', "_.. ");
                ('e', ". "); ('f', ".._. "); ('g', "__. "); ('h', ".... ");
                ('i', ".. "); ('j', ".___ "); ('k', "_._ "); ('l', "._.. ");
                ('m', "__ "); ('n', "_. "); ('o', "___ "); ('p', ".__. ");
                ('q', "__._ "); ('r', "._. "); ('s', "... "); ('t', "_ ");
                ('u', ".._ "); ('v', "..._ "); ('w', ".__ "); ('x', "_.._ ");
                ('y', "_.__ "); ('z', "__.. "); ('0', "_____ "); ('1', ".____ ");
                ('2', "..___ "); ('3', "...__ "); ('4', "...._ "); ('5', "..... ");
                ('6', "_.... "); ('7', "__... "); ('8', "___.. "); ('9', "____. ")]

let beep c =
    match c with
    | '.' ->
        printf "."
        Console.Beep(1200, 250)
    | '_' ->
        printf "_"
        Console.Beep(1200, 1000)
    | _ ->
        printf " "
        Thread.Sleep(125)

let trim (s: string) = s.Trim()
let toMorse c = Map.find c morse
let lower (s: string) = s.ToLower()
let sanitize = String.filter Char.IsLetterOrDigit

let send = sanitize >> lower >> String.collect toMorse >> trim >> String.iter beep

send "Rosetta Code"

```

```txt
._. ___ ... . _ _ ._ _._. ___ _.. .
```



## Gambas


```gambas
'Requires component 'gb.sdl2.audio'

Public Sub Main()
Dim sMessage As String = "Hello World"
Dim sFile As String = File.Load("../morse.txt") 'Contains A,.- B,-... etc
Dim sChar As New String[]
Dim sMorse As New String[]
Dim sOutPut As New String[]
Dim sTemp As String
Dim siCount, siLoop As Short
Dim bTrigger As Boolean
Dim fDelay As Float = 0.4

If Not Exist("/tmp/dot.ogg") Then Copy "../dot.ogg" To "/tmp/dot.ogg"     'Sounds downloaded from
If Not Exist("/tmp/dash.ogg") Then Copy "../dash.ogg" To "/tmp/dash.ogg"  'https://en.wikipedia.org/wiki/Morse_code#Letters.2C_numbers.2C_punctuation.2C_prosigns_for_Morse_code_and_non-English_variants

For Each sTemp In Split(sFile, gb.NewLine)
  sChar.add(Split(Trim(sTemp))[0])
  sMorse.add(Split(Trim(sTemp))[1])
Next

For siCount = 1 To Len(sMessage)
  For siLoop = 0 To sChar.Max
    If sChar[siLoop] = Mid(UCase(sMessage), siCount, 1) Then
      sOutPut.Add(sMorse[siLoop])
      bTrigger = True
      Break
    End If
  Next
  If bTrigger = False Then sOutPut.Add(" ")
  bTrigger = False
Next

Print sOutPut.Join(" ")

For siCount = 0 To Len(sMessage) - 1
  For siLoop = 0 To Len(sOutPut[siCount]) - 1
    If Mid(sOutPut[siCount], siLoop + 1, 1) = "." Then
      Music.Load("/tmp/dot.ogg")
      Music.Play
      Wait fDelay
    Else If Mid(sOutPut[siCount], siLoop + 1, 1) = "-" Then
      Music.Load("/tmp/dash.ogg")
      Music.Play
      Wait fDelay
    Else
      Wait fDelay
    Endif
  Next
Next

End
```

Output (plus sound):

```txt

.... . .-.. .-.. ---   .-- --- .-. .-.. -..

```



## Go



```Go
// Command morse translates an input string into morse code,
// showing the output on the console, and playing it as sound.
// Only works on ubuntu.
package main

import (
	"flag"
	"fmt"
	"log"
	"regexp"
	"strings"
	"syscall"
	"time"
	"unicode"
)

// A key represents an action on the morse key.
// It's either on or off, for the given duration.
type key struct {
	duration int
	on       bool
	sym      string // for debug output
}

var (
	runeToKeys   = map[rune][]key{}
	interCharGap = []key{{1, false, ""}}
	punctGap     = []key{{7, false, " / "}}
	charGap      = []key{{3, false, " "}}
	wordGap      = []key{{7, false, " / "}}
)

const rawMorse = `
A:.-    J:.---  S:...    1:.----  .:.-.-.-  ::---...
B:-...  K:-.-   T:-      2:..---  ,:--..--  ;:-.-.-.
C:-.-.  L:.-..  U:..-    3:...--  ?:..--..  =:-...-
D:-..   M:--    V:...-   4:....-  ':.----.  +:.-.-.
E:.     N:-.    W:.--    5:.....  !:-.-.--  -:-....-
F:..-.  O:---   X:-..-   6:-....  /:-..-.   _:..--.-
G:--.   P:.--.  Y:-.--   7:--...  (:-.--.   ":.-..-.
H:....  Q:--.-  Z:--..   8:---..  ):-.--.-  $:...-..-
I:..    R:.-.   0:-----  9:----.  &:.-...   @:.--.-.
`

func init() {
	// Convert the rawMorse table into a map of morse key actions.
	r := regexp.MustCompile("([^ ]):([.-]+)")
	for _, m := range r.FindAllStringSubmatch(rawMorse, -1) {
		c := m[1][0]
		keys := []key{}
		for i, dd := range m[2] {
			if i > 0 {
				keys = append(keys, interCharGap...)
			}
			if dd == '.' {
				keys = append(keys, key{1, true, "."})
			} else if dd == '-' {
				keys = append(keys, key{3, true, "-"})
			} else {
				log.Fatalf("found %c in morse for %c", dd, c)
			}
			runeToKeys[rune(c)] = keys
			runeToKeys[unicode.ToLower(rune(c))] = keys
		}
	}
}

// MorseKeys translates an input string into a series of keys.
func MorseKeys(in string) ([]key, error) {
	afterWord := false
	afterChar := false
	result := []key{}
	for _, c := range in {
		if unicode.IsSpace(c) {
			afterWord = true
			continue
		}
		morse, ok := runeToKeys[c]
		if !ok {
			return nil, fmt.Errorf("can't translate %c to morse", c)
		}
		if unicode.IsPunct(c) && afterChar {
			result = append(result, punctGap...)
		} else if afterWord {
			result = append(result, wordGap...)
		} else if afterChar {
			result = append(result, charGap...)
		}
		result = append(result, morse...)
		afterChar = true
		afterWord = false
	}
	return result, nil
}

func main() {
	var ditDuration time.Duration
	flag.DurationVar(&ditDuration, "d", 40*time.Millisecond, "length of dit")
	flag.Parse()
	in := "hello world."
	if len(flag.Args()) > 1 {
		in = strings.Join(flag.Args(), " ")
	}
	keys, err := MorseKeys(in)
	if err != nil {
		log.Fatalf("failed to translate: %s", err)
	}
	for _, k := range keys {
		if k.on {
			if err := note(true); err != nil {
				log.Fatalf("failed to play note: %s", err)
			}
		}
		fmt.Print(k.sym)
		time.Sleep(ditDuration * time.Duration(k.duration))
		if k.on {
			if err := note(false); err != nil {
				log.Fatalf("failed to stop note: %s", err)
			}
		}
	}
	fmt.Println()
}

// Implement sound on ubuntu. Needs permission to access /dev/console.

var consoleFD uintptr

func init() {
	fd, err := syscall.Open("/dev/console", syscall.O_WRONLY, 0)
	if err != nil {
		log.Fatalf("failed to get console device: %s", err)
	}
	consoleFD = uintptr(fd)
}

const KIOCSOUND = 0x4B2F
const clockTickRate = 1193180
const freqHz = 600

// note either starts or stops a note.
func note(on bool) error {
	arg := uintptr(0)
	if on {
		arg = clockTickRate / freqHz
	}
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL, consoleFD, KIOCSOUND, arg)
	if errno != 0 {
		return errno
	}
	return nil

}

```



## Haskell


This implementation requires that the "play" program of the SoX (Sound eXchange)
package be installed.  However, it is easy to replace the module that uses it with
a different one.

The main program.

```Haskell

import System.IO
import MorseCode
import MorsePlaySox

-- Read standard input, converting text to Morse code, then playing the result.
-- We turn off buffering on stdin so it will play as you type.
main = do
  hSetBuffering stdin NoBuffering
  text <- getContents
  play $ toMorse text

```


The module to convert text to Morse code symbols.

```Haskell

module MorseCode (Morse, MSym(..), toMorse) where

import Data.List
import Data.Maybe
import qualified Data.Map as M

type Morse = [MSym]
data MSym = Dot | Dash | SGap | CGap | WGap deriving (Show)

-- Based on the table of International Morse Code letters and numerals at
-- http://en.wikipedia.org/wiki/Morse_code.
dict = M.fromList
       [('a', m ".-"   ), ('b', m "-..." ), ('c', m "-.-." ), ('d', m "-.."  ),
        ('e', m "."    ), ('f', m "..-." ), ('g', m "--."  ), ('h', m "...." ),
        ('i', m ".."   ), ('j', m ".---" ), ('k', m "-.-"  ), ('l', m ".-.." ),
        ('m', m "--"   ), ('n', m "-."   ), ('o', m "---"  ), ('p', m ".--." ),
        ('q', m "--.-" ), ('r', m ".-."  ), ('s', m "..."  ), ('t', m "-"    ),
        ('u', m "..-"  ), ('v', m "...-" ), ('w', m ".--"  ), ('x', m "-..-" ),
        ('y', m "-.--" ), ('z', m "--.." ), ('1', m ".----"), ('2', m "..---"),
        ('3', m "...--"), ('4', m "....-"), ('5', m "....."), ('6', m "-...."),
        ('7', m "--..."), ('8', m "---.."), ('9', m "----."), ('0', m "-----")]
    where m = intersperse SGap . map toSym
          toSym '.' = Dot
          toSym '-' = Dash

-- Convert a string to a stream of Morse symbols.  We enhance the usual dots
-- and dashes with special "gap" symbols, which indicate the border between
-- symbols, characters and words.  This allows a player to easily adjust its
-- timing by simply looking at the current symbol, rather than trying to keep
-- track of state.
toMorse :: String -> Morse
toMorse = fromWords . words . weed
    where fromWords = intercalate [WGap] . map fromWord
          fromWord  = intercalate [CGap] . map fromChar
          fromChar  = fromJust . flip M.lookup dict
          weed      = filter (\c -> c == ' ' || M.member c dict)

```


The module to interpret Morse code symbols as sound.

```Haskell

module MorsePlaySox (play) where

import Sound.Sox.Play
import Sound.Sox.Option.Format
import Sound.Sox.Signal.List
import Data.Int
import System.Exit
import MorseCode

samps = 15           -- samples/cycle
freq  = 700          -- cycles/second (frequency)
rate  = samps * freq -- samples/second (sampling rate)

type Samples = [Int16]

-- One cycle of silence and a sine wave.
mute, sine :: Samples
mute = replicate samps 0
sine = let n = fromIntegral samps
           f k = 8000.0 * sin (2*pi*k/n)
       in map (round . f . fromIntegral) [0..samps-1]

-- Repeat samples until we have the specified duration in seconds.
rep :: Float -> Samples -> Samples
rep dur = take n . cycle
    where n = round (dur * fromIntegral rate)

-- Convert Morse symbols to samples.  Durations are in seconds, based on
-- http://en.wikipedia.org/wiki/Morse_code#Representation.2C_timing_and_speeds.
toSamples :: MSym -> Samples
toSamples Dot  = rep 0.1 sine
toSamples Dash = rep 0.3 sine
toSamples SGap = rep 0.1 mute
toSamples CGap = rep 0.3 mute
toSamples WGap = rep 0.7 mute

-- Interpret the stream of Morse symbols as sound.
play :: Morse -> IO ExitCode
play = simple put none rate . concatMap toSamples

```



## J



```j
require'strings media/wav'
morse=:[:; [:(' ',~' .-' {~ 3&#.inv)&.> (_96{.".0 :0-.LF) {~ a.&i.@toupper
 79 448 0 1121 0 0 484 214 644 0 151 692 608 455 205 242 161 134 125 122
 121 202 229 238 241 715 637 0 203 0 400 475 5 67 70 22 1 43 25 40 4 53
 23 49 8 7 26 52 77 16 13 2 14 41 17 68 71 76 214 0 644 0 401
)

onoffdur=: 0.01*100<.@*(1.2%[)*(4 4#:2 5 13){~' .-'i.]
playmorse=: 30&$: :((wavnote&, 63 __"1)@(onoffdur morse))
```


Example use:

<lang>   morse'this is an example'
- .... .. ...   .. ...   .- -.   . -..- .- -- .--. .-.. .
   playmorse'this is an example'
1
   12 playmorse'as is this'
1
```


morse converts from text to dot dash notation

playmorse converts from text to sound (and produces an inconsequential result which should always be 1).

Note that playmorse takes an optional left argument (wpm).  However, the current implementation is limited as it truncates the length of a morse element to the nearest 10 milliseconds in duration, to work around a limitation in wavnote.  This means generated morse code sounds jump directly from 30wpm to 40wpm.

(If wpm were a part of the task spec, it might be worthwhile doing away with wavnote and generating sound samples directly.  However, currently this task has no such requirement so merely documenting this issue should be sufficient.)

The long sequence of numbers in the definition of "morse" are meant to be interpreted as base 3 numbers, where a 1 digit represents a dot and a 2 digit represents a dash and a 0 digit represents a pause. The first of these numbers corresponds to the character <code>!</code> and the last of the numbers corresponds to the character <code>_</code> .  To convert morse text to base 3 use: <code>3&#.@i.~&' .-'&></code>.

<code>3&#.@i.~&' .-'&></code>, on the international morse for printable ascii characters, one character per line, with blank lines for the missing characters, was how that big long numeric list was originally generated.  To recover that list you can use <code>morse ::(<nowiki>''</nowiki>"_)"1,.a.</code>, but note that this will include blank lines for every missing ascii character.  If you instead use <code>morse ::('*'"_)"1,.a.</code>, you will get asterisks for the ascii characters after the final representable character.

The short sequence of numbers (2 5 13) in the definition of "onoffdur" are meant to be interpreted base 4 and correspond to the three characters <<space, dot and dash>> which can appear in a morse text sequence.  The first "base 4 digit" in each of these numbers represents the relative "on" duration for the sound of one of these three characters and the second represents the relative "off" duration for that character.

The sequence 63 __ in the implementation corresponds to the note D# in the fifth octave above middle C's octave, and silence.  In other words, the number 63 corresponds to midi note number 123, or almost 10kHz, and the number __ (negative infinity) corresponds to 0kHz or silence.  (Note: midi uses 60 for middle C, where J's wavnote uses 0, because midi note numbers can not be negative.)  In other words 63 is the "on note" and __ is the "off note" for generating morse sound.


## Java

```java
import java.util.*;

public class MorseCode {

    final static String[][] code = {
        {"A", ".-     "}, {"B", "-...   "}, {"C", "-.-.   "}, {"D", "-..    "},
        {"E", ".      "}, {"F", "..-.   "}, {"G", "--.    "}, {"H", "....   "},
        {"I", "..     "}, {"J", ".---   "}, {"K", "-.-    "}, {"L", ".-..   "},
        {"M", "--     "}, {"N", "-.     "}, {"O", "---    "}, {"P", ".--.   "},
        {"Q", "--.-   "}, {"R", ".-.    "}, {"S", "...    "}, {"T", "-      "},
        {"U", "..-    "}, {"V", "...-   "}, {"W", ".-   - "}, {"X", "-..-   "},
        {"Y", "-.--   "}, {"Z", "--..   "}, {"0", "-----  "}, {"1", ".----  "},
        {"2", "..---  "}, {"3", "...--  "}, {"4", "....-  "}, {"5", ".....  "},
        {"6", "-....  "}, {"7", "--...  "}, {"8", "---..  "}, {"9", "----.  "},
        {"'", ".----. "}, {":", "---... "}, {",", "--..-- "}, {"-", "-....- "},
        {"(", "-.--.- "}, {".", ".-.-.- "}, {"?", "..--.. "}, {";", "-.-.-. "},
        {"/", "-..-.  "}, {"-", "..--.- "}, {")", "---..  "}, {"=", "-...-  "},
        {"@", ".--.-. "}, {"\"", ".-..-."}, {"+", ".-.-.  "}, {" ", "/"}}; // cheat a little

    final static Map<Character, String> map = new HashMap<>();

    static {
        for (String[] pair : code)
            map.put(pair[0].charAt(0), pair[1].trim());
    }

    public static void main(String[] args) {
        printMorse("sos");
        printMorse("   Hello     World!");
        printMorse("Rosetta Code");
    }

    static void printMorse(String input) {
        System.out.printf("%s %n", input);

        input = input.trim().replaceAll("[ ]+", " ").toUpperCase();
        for (char c : input.toCharArray()) {
            String s = map.get(c);
            if (s != null)
                System.out.printf("%s ", s);
        }
        System.out.println("\n");
    }
}
```



```txt
sos
... --- ...

   Hello     World!
.... . .-.. .-.. --- / .-   - --- .-. .-.. -..

Rosetta Code
.-. --- ... . - - .- / -.-. --- -.. .
```



## JavaScript


This implementation utilises the fairly new Web Audio API in the browser for generating tones, as such it only uses one vendor implementation (WebKit). It is split into three modules; 1. translating the characters into morse code. 2. creating timings for the morse code. 3. creating tones with the timings.


```JavaScript

var globalAudioContext = new webkitAudioContext();

function morsecode(text, unit, freq) {
	'use strict';

	// defaults
	unit = unit ? unit : 0.05;
	freq = freq ? freq : 700;
	var cont = globalAudioContext;
	var time = cont.currentTime;

	// morsecode
	var code = {
		a: '._',    b: '_...',  c: '_._.',  d: '_..',   e: '.',     f: '.._.',
		g: '__.',   h: '....',  i: '..',    j: '.___',  k: '_._',   l: '._..',
		m: '__',    n: '_.',    o: '___',   p: '.__.',  q: '__._',  r: '._.',
		s: '...',   t: '_',     u: '.._',   v: '..._',  w: '.__',   x: '_.._',
		y: '_.__',  z: '__..',  0: '_____', 1: '.____', 2: '..___', 3: '...__',
		4: '...._', 5: '.....', 6: '_....', 7: '__...', 8: '___..', 9: '____.'
	};

	// generate code for text
	function makecode(data) {
		for (var i = 0; i <= data.length; i ++) {
			var codedata = data.substr(i, 1).toLowerCase();
			codedata = code[codedata];
			// recognised character
			if (codedata !== undefined) {
				maketime(codedata);
			}
			// unrecognised character
			else {
				time += unit * 7;
			}
		}
	}

	// generate time for code
	function maketime(data) {
		for (var i = 0; i <= data.length; i ++) {
			var timedata = data.substr(i, 1);
			timedata = (timedata === '.') ? 1 : (timedata === '_') ? 3 : 0;
			timedata *= unit;
			if (timedata > 0) {
				maketone(timedata);
				time += timedata;
				// tone gap
				time += unit * 1;
			}
		}
		// char gap
		time += unit * 2;
	}

	// generate tone for time
	function maketone(data) {
		var start = time;
		var stop = time + data;
		// filter: envelope the tone slightly
		gain.gain.linearRampToValueAtTime(0, start);
		gain.gain.linearRampToValueAtTime(1, start + (unit / 8));
		gain.gain.linearRampToValueAtTime(1, stop - (unit / 16));
		gain.gain.linearRampToValueAtTime(0, stop);
	}

	// create: oscillator, gain, destination
	var osci = cont.createOscillator();
	osci.frequency.value = freq;
	var gain = cont.createGainNode();
	gain.gain.value = 0;
	var dest = cont.destination;
	// connect: oscillator -> gain -> destination
	osci.connect(gain);
	gain.connect(dest);
	// start oscillator
	osci.start(time);

	// begin encoding: text -> code -> time -> tone
	makecode(text);

	// return web audio context for reuse / control
	return cont;
}

```


Usage:


```JavaScript

morsecode('Hello World');

```


[http://jsbin.com/orubaq/1/edit Live Version]



## Julia

Requires a sound card and the PortAudio libraries.

```Julia
using PortAudio

const pstream = PortAudioStream(0, 2)
sendmorsesound(t, f) = write(pstream, SinSource(eltype(stream), samplerate(stream)*0.8, [f]), (t/1000)s)

char2morse = Dict[
      "!" => "---.", "\"" => ".-..-.", "$" => "...-..-", "'" => ".----.",
      "(" => "-.--.", ")" => "-.--.-", "+" => ".-.-.", "," => "--..--",
      "-" => "-....-", "." => ".-.-.-", "/" => "-..-.", "0" => "-----",
      "1" => ".----", "2" => "..---", "3" => "...--", "4" => "....-", "5" => ".....",
      "6" => "-....", "7" => "--...", "8" => "---..", "9" => "----.", ":" => "---...",
      ";" => "-.-.-.", "=" => "-...-", "?" => "..--..", "@" => ".--.-.", "A" => ".-",
      "B" => "-...", "C" => "-.-.", "D" => "-..", "E" => ".", "F" => "..-.",
      "G" => "--.", "H" => "....", "I" => "..", "J" => ".---", "K" => "-.-",
      "L" => ".-..", "M" => "--", "N" => "-.", "O" => "---", "P" => ".--.",
      "Q" => "--.-", "R" => ".-.", "S" => "...", "T" => "-", "U" => "..-",
      "V" => "...-", "W" => ".--", "X" => "-..-", "Y" => "-.--", "Z" => "--..",
      "[" => "-.--.", "]" => "-.--.-", "_" => "..--.-"]

function sendmorsesound(freq, duration)
cpause() = sleep(0.080)
wpause = sleep(0.400)

dit() = sendmorsesound(0.070, 700)
dash() = sensmorsesound(0.210, 700)
sendmorsechar(c) = for d in char2morse(c) d == '.' ? dit(): dash() end end
sendmorseword(w) = for c in w sendmorsechar(c) cpause() end wpause() end
sendmorse(msg) = for word in uppercase(msg) sendmorseword(word) end

sendmorse("sos sos sos")
sendmorse("The case of letters in Morse coding is ignored."

```



## Kotlin


Java does not have easy access to the beep method, so we need to create one using the Audio API it provides.


```scala
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem

val morseCode = hashMapOf(
        'a' to ".-", 'b' to "-...", 'c' to "-.-.",
        'd' to "-..", 'e' to ".", 'f' to "..-.",
        'g' to "--.", 'h' to "....", 'i' to "..",
        'j' to ".---", 'k' to "-.-", 'l' to ".-..",
        'm' to "--", 'n' to "-.", 'o' to "---",
        'p' to ".--.", 'q' to "--.-", 'r' to ".-.",
        's' to "...", 't' to "-", 'u' to "..-",
        'v' to "...-", 'w' to ".--", 'x' to "-..-",
        'y' to "-.--", 'z' to "--..",

        '0' to ".....", '1' to "-....", '2' to "--...",
        '3' to "---..", '4' to "----.", '5' to "-----",
        '6' to ".----", '7' to "..---", '8' to "...--",
        '9' to "....-",

        ' ' to "/", ',' to "--..--", '!' to "-.-.--",
        '"' to ".-..-.", '.' to ".-.-.-", '?' to "..--..",
        '\'' to ".----.", '/' to "-..-.", '-' to "-....-",
        '(' to "-.--.-", ')' to "-.--.-"
)

val symbolDurationInMs = hashMapOf('.' to 200, '-' to 500, '/' to 1000)


fun toMorseCode(message: String) = message.filter { morseCode.containsKey(it) }
                                          .fold("") { acc, ch -> acc + morseCode[ch]!! }

fun playMorseCode(morseCode: String) = morseCode.forEach { symbol -> beep(symbolDurationInMs[symbol]!!) }

fun beep(durationInMs: Int) {
    val soundBuffer = ByteArray(durationInMs * 8)
    for ((i, _) in soundBuffer.withIndex()) {
        soundBuffer[i] = (Math.sin(i / 8.0 * 2.0 * Math.PI) * 80.0).toByte()
    }

    val audioFormat = AudioFormat(
            /*sampleRate*/ 8000F,
            /*sampleSizeInBits*/ 8,
            /*channels*/ 1,
            /*signed*/ true,
            /*bigEndian*/ false
    )
    with (AudioSystem.getSourceDataLine(audioFormat)!!) {
        open(audioFormat)

        start()
        write(soundBuffer, 0, soundBuffer.size)
        drain()

        close()
    }
}

fun main(args: Array<String>) {
    args.forEach {
        playMorseCode(toMorseCode(it.toLowerCase()))
    }
}
```



## Liberty BASIC


```lb
'The following code relies on the Windows API
Input "Input the text to translate to Morse Code... "; string$
Print PlayMorse$(string$)
End

Function PlayMorse$(string$)
    'LetterGap = (3 * BaseTime)
    'WordGap = (7 * BaseTime)
    BaseTime = 50
    freq = 1250
    PlayMorse$ = TranslateToMorse$(string$)
    morseCode$ = "./-"
    For i = 1 To Len(PlayMorse$)
        Scan
        dwDuration = (Instr(morseCode$, Mid$(PlayMorse$, i, 1)) * BaseTime)
        If (Mid$(PlayMorse$, i, 1) <> " ") Then
            CallDLL #kernel32, "Beep", freq As ulong, dwDuration As ulong, ret As long
            CallDLL #kernel32, "Sleep", BaseTime As long, ret As void
        End If
        If (Mid$(PlayMorse$, i, 1) <> " ") Then
            sleep = (3 * BaseTime)
        Else
            sleep = (7 * BaseTime)
        End If
        CallDLL #kernel32, "Sleep", sleep As long, ret As void
    Next i
End Function

Function TranslateToMorse$(string$)
    string$ = Upper$(string$)
    For i = 1 To Len(string$)
        While desc$ <> "End"
            Read desc$, value$
            If desc$ = "" Then desc$ = chr$(34)
            If desc$ = Mid$(string$, i, 1) Then
                If Mid$(string$, i, 1) <> " " Then value$ = " " + value$
                TranslateToMorse$ = TranslateToMorse$ + value$
                Exit While
            End If
        Wend
        If desc$ = "End" Then Notice Mid$(string$, i, 1) + " is not accounted for in the Morse Code Table."
        Restore
    Next i
    TranslateToMorse$ = Trim$(TranslateToMorse$)
    Data "A", ".-", "B", "-...", "C", "-.-.", "D", "-..", "E", ".", "F", "..-.", "G", "--."
    Data "H", "....", "I", "..", "J", ".---", "K", "-.-", "L", ".-..", "M", "--", "N", "-."
    Data "O", "---", "P", ".--.", "Q", "--.-", "R", ".-.", "S", "...", "T", "-", "U", "..-"
    Data "V", "...-", "W", ".--", "X", "-..-", "Y", "-.--", "Z", "--..", "Á", "--.-", "Ä", ".-.-"
    Data "É", "..-..", "Ñ", "--.--", "Ö", "---.", "Ü", "..--", "1", ".----", "2", "..---"
    Data "3", "...--", "4", "....-", "5", ".....", "6", "-....", "7", "--...", "8", "---.."
    Data "9", "----.", "0", "-----", ",", "--..--", ".", ".-.-.-", "?", "..--..", ";", "-.-.-"
    Data ":", "---...", "/", "-..-.", "-", "-....-", "'", ".----.", "+", ".-.-.", "", ".-..-."
    Data "@", ".--.-.", "(", "-.--.", ")", "-.--.-", "_", "..--.-", "$", "...-..-", "&", ".-..."
    Data "=", "-...-", "!", "..--.", " ", " ", "End", ""
End Function
```



## Lua

The following code is actual eLua code used to beep the speaker in Morse code n the Shenzhou III STM32F103ZET6 evaluation board.  eLua is a Lua 5.1.4 implementation paired with libraries for low-level hardware access in embedded systems.  The below code could easily be converted to any other Lua 5.n environment (including games), provided some kind of sound library has been installed.  Only the functions buzz and pause would have to be modified.


```lua
local M = {}

-- module-local variables
local BUZZER = pio.PB_10
local dit_length, dah_length, word_length

-- module-local functions
local buzz, dah, dit, init, inter_element_gap, medium_gap, pause, sequence, short_gap

buzz = function(duration)
  pio.pin.output(BUZZER)
  pio.pin.setlow(BUZZER)
  tmr.delay(tmr.SYS_TIMER, duration)
  pio.pin.sethigh(BUZZER)
  pio.pin.input(BUZZER)
end

dah = function()
  buzz(dah_length)
end

dit = function()
  buzz(dit_length)
end

init = function(baseline)
  dit_length = baseline
  dah_length = 2 * baseline
  word_length = 4 * baseline
end

inter_element_gap = function()
  pause(dit_length)
end

medium_gap = function()
  pause(word_length)
end

pause = function(duration)
  tmr.delay(tmr.SYS_TIMER, duration)
end

sequence = function(codes)
  if codes then
    for _,f in ipairs(codes) do
      f()
      inter_element_gap()
    end
    short_gap()
  end
end

short_gap = function()
  pause(dah_length)
end

local morse = {
  a = { dit, dah }, b = { dah, dit, dit, dit }, c = { dah, dit, dah, dit },
  d = { dah, dit, dit }, e = { dit }, f = { dit, dit, dah, dit },
  g = { dah, dah, dit }, h = { dit, dit, dit ,dit }, i = { dit, dit },
  j = { dit, dah, dah, dah }, k = { dah, dit, dah }, l = { dit, dah, dit, dit },
  m = { dah, dah }, n = { dah, dit }, o = { dah, dah, dah },
  p = { dit, dah, dah, dit }, q = { dah, dah, dit, dah }, r = { dit, dah, dit },
  s = { dit, dit, dit }, t = { dah }, u = { dit, dit, dah },
  v = { dit, dit, dit, dah }, w = { dit, dah, dah }, x = { dah, dit, dit, dah },
  y = { dah, dit, dah, dah }, z = { dah, dah, dit, dit },

  ["0"] = { dah, dah, dah, dah, dah }, ["1"] = { dit, dah, dah, dah, dah },
  ["2"] = { dit, dit, dah, dah, dah }, ["3"] = { dit, dit, dit, dah, dah },
  ["4"] = { dit, dit, dit, dit, dah }, ["5"] = { dit, dit, dit, dit, dit },
  ["6"] = { dah, dit, dit, dit, dit }, ["7"] = { dah, dah, dit, dit, dit },
  ["8"] = { dah, dah, dah, dit, dit }, ["9"] = { dah, dah, dah, dah, dit },

  [" "] = { medium_gap }
}

-- public interface
M.beep = function(message)
  message = message:lower()
  for _,ch in ipairs { message:byte(1, #message) } do
    sequence(morse[string.char(ch)])
  end
end

M.set_dit = function(duration)
  init(duration)
end

-- initialization code
init(50000)

return M
```


Using this module is as simple as:


```lua
morse = require 'morse'
morse.beep "I am the very model of a modern major-general."
```



## MATLAB

This function will remove any characters not defined in the morse code.


```MATLAB
function [morseText,morseSound] = text2morse(string,playSound)

%% Translate AlphaNumeric Text to Morse Text

    string = lower(string);

    %Defined such that the ascii code of the characters in the string map
    %to the indecies of the dictionary.
    morseDictionary = {{' ',' '},{'',''},{'',''},{'',''},...
                       {'',''},{'',''},{'',''},{'',''},{'',''},{'',''},...
                       {'',''},{'',''},{'',''},{'',''},{'',''},{'',''},...
                       {'0','-----'},{'1','.----'},{'2','..---'},{'3','...--'},...
                       {'4','....-'},{'5','.....'},{'6','-....'},{'7','--...'},...
                       {'8','---..'},{'9','----.'},...
                       {'',''},{'',''},{'',''},{'',''},{'',''},{'',''},...
                       {'',''},{'',''},{'',''},{'',''},{'',''},{'',''},...
                       {'',''},{'',''},{'',''},{'',''},{'',''},{'',''},...
                       {'',''},{'',''},{'',''},{'',''},{'',''},{'',''},...
                       {'',''},{'',''},{'',''},{'',''},{'',''},{'',''},...
                       {'',''},{'',''},{'',''},{'',''},{'',''},{'',''},...
                       {'',''},{'',''},{'',''},...
                       {'a','.-'},{'b','-...'},{'c','-.-.'},{'d','-..'},...
                       {'e','.'},{'f','..-.'},{'g','--.'},{'h','....'},...
                       {'i','..'},{'j','.---'},{'k','-.-'},{'l','.-..'},...
                       {'m','--'},{'n','-.'},{'o','---'},{'p','.--.'},...
                       {'q','--.-'},{'r','.-.'},{'s','...'},{'t','-'},...
                       {'u','..-'},{'v','...-'},{'w','.--'},{'x','-..-'},...
                       {'y','-.--'},{'z','--..'}};

    %Iterates through each letter in the string and converts it to morse
    %code
    morseText = arrayfun(@(x)[morseDictionary{x}{2} '|'],(string - 31),'UniformOutput',false);

    %The output of the previous operation is a cell array, we want it to be
    %a string. This line accomplishes that.
    morseText = cell2mat(morseText);

    morseText(end) = []; %delete extra pipe

%% Translate Morse Text to Morse Audio

    %Generate the tones for each element of the code
    SamplingFrequency = 8192; %Hz
    ditLength = .1; %s
    dit = (0:1/SamplingFrequency:ditLength);
    dah = (0:1/SamplingFrequency:3*ditLength);
    dit = sin(3520*dit);
    dah = sin(3520*dah);
    silent = zeros(1,length(dit));

    %A dictionary of the audio components of each symbol
    morseTiming = {{'.',[dit silent]},{'-',[dah silent]},{'|',[silent silent]},{' ',[silent silent]}};
    morseSound = [];

    for i = (1:length(morseText))

        %Iterate through each cell in the morseTiming cell array and
        %find which timing sequence corresponds to the current morse
        %text symbol.
        cellNum = find(cellfun(@(x)(x{1}==morseText(i)),morseTiming));

        morseSound = [morseSound morseTiming{cellNum}{2}];
    end

    morseSound(end-length(silent):end) = []; %Delete the extra silent tone at the end

    if(playSound)
        sound(morseSound,SamplingFrequency); %Play sound
    end

end %text2morse
```


{{out}} This will play the audio automatically, because the playSound argument is "true".

```MATLAB>>
 text2morse('Call me Ishmael.',true)

ans =

-.-.|.-|.-..|.-..| |--|.| |..|...|....|--|.-|.|.-..|
```




## Mathematica

A Morse "codec" based on replacement rule programming.
Replacement rules also translate the text Morse code into audible Morse code.
The dots and dashes become clarinet middle-C notes of different lengths.
Unknown characters encode into "?" in Morse text and clarinet F# in Morse audio.
The function, sonicMorse[s_String], plays the Morse code audio translation of a string.


```Mathematica
Dictionary = Join[CharacterRange["a", "z"], CharacterRange["0", "9"]];
mark = 0.1; gap = 0.125; (* gap should be equal to mark. But longer gap makes audio code easier to decode *)
shortgap = 3*gap; medgap = 7*gap;
longmark = 3*mark;
MorseDictionary = {
   ".-", "-...", "-.-.", "-..",
   ".", "..-.", "--.", "....", "..",
   ".---", "-.-", ".-..", "--", "-.",
   "---", ".--.", "--.-", ".-.",
   "...", "-", "..-", "...-", ".--",
   "-..-", "-.--", "--..",
   "-----", ".----", "..---", "...--", "....-", ".....",
   "-....", "--...", "---..", "----."
   };

MorseDictionary = # <> " " & /@ MorseDictionary; (* Force short gap silence after each letter/digit *)

Tones = {
   SoundNote[None, medgap],
   SoundNote[None, shortgap],
   {SoundNote["C", mark, "Clarinet"], SoundNote[None, gap]},
   {SoundNote["C", longmark, "Clarinet"], SoundNote[None, gap]},
   {SoundNote["fsharp", mark, "Clarinet"], SoundNote[None, gap]}  (* Use F# short mark to denote unrecognized character *)
   };

codeRules = MapThread[Rule, {Dictionary, MorseDictionary}];
decodeRules = MapThread[Rule, {MorseDictionary, Dictionary}];
soundRules = MapThread[Rule, {{"  ", " ", ".", "-", "?"}, Tones}];
(* The order of the rules here is important. Otherwise medium gaps and short gaps get confounded *)

morseCode[s_String] := StringReplace[ToLowerCase@s, codeRules~Join~{x_ /; FreeQ[Flatten@{Dictionary, " "}, x] -> "? "}]
morseDecode[s_String] := StringReplace[s, decodeRules]
sonicMorse[s_String] := EmitSound@Sound@Flatten[Characters@morseCode@s /. soundRules]
```


```txt
morseCode["SOS   soS"]
morseDecode[%]
morseCode["s@os|"]
```

```txt
"... --- ...    ... --- ... "

"sos   sos"

"... ? --- ... ? "
```


=={{header|Modula-2}}==

```modula2
MODULE MorseCode;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteMorseCode(str : ARRAY OF CHAR);
VAR i : CARDINAL;
BEGIN
    WriteString(str);
    WriteLn;
    FOR i:=0 TO HIGH(str) DO
        CASE CAP(str[i]) OF
              'A': WriteString(".-");
            | 'B': WriteString("-...");
            | 'C': WriteString("-.-");
            | 'D': WriteString("-..");
            | 'E': WriteString(".");
            | 'F': WriteString("..-.");
            | 'G': WriteString("--.");
            | 'H': WriteString("....");
            | 'I': WriteString("..");
            | 'J': WriteString(".---");
            | 'K': WriteString("-.-");
            | 'L': WriteString(".-..");
            | 'M': WriteString("--");
            | 'N': WriteString("-.");
            | 'O': WriteString("---");
            | 'P': WriteString(".--.");
            | 'Q': WriteString("--.-");
            | 'R': WriteString(".-.");
            | 'S': WriteString("...");
            | 'T': WriteString("-");
            | 'U': WriteString("..-");
            | 'V': WriteString("...-");
            | 'W': WriteString(".--");
            | 'X': WriteString("-..-");
            | 'Y': WriteString("-.--");
            | 'Z': WriteString("--..");
            | '0': WriteString("-----");
            | '1': WriteString(".----");
            | '2': WriteString("..---");
            | '3': WriteString("...--");
            | '4': WriteString("....-");
            | '5': WriteString(".....");
            | '6': WriteString("-....");
            | '7': WriteString("--...");
            | '8': WriteString("---..");
            | '9': WriteString("----.");
            | ' ': WriteString("  ");
        ELSE
            IF (str[i] # 0C) THEN
                WriteString("?");
            END
        END;
        WriteString(" ");
    END;
END WriteMorseCode;

BEGIN
    WriteMorseCode("hello world");
    WriteLn;

    ReadChar;
END MorseCode.
```



## OCaml

Using <code>/dev/dsp</code>:


```ocaml
let codes = [
  'a', ".-";     'b', "-...";   'c', "-.-.";
  'd', "-..";    'e', ".";      'f', "..-.";
  'g', "--.";    'h', "....";   'i', "..";
  'j', ".---";   'k', "-.-";    'l', ".-..";
  'm', "--";     'n', "-.";     'o', "---";
  'p', ".--.";   'q', "--.-";   'r', ".-.";
  's', "...";    't', "-";      'u', "..-";
  'v', "...-";   'w', ".--";    'x', "-..-";
  'y', "-.--";   'z', "--..";   '0', "-----";
  '1', ".----";  '2', "..---";  '3', "...--";
  '4', "....-";  '5', ".....";  '6', "-....";
  '7', "--...";  '8', "---..";  '9', "----.";
]

let oc = open_out "/dev/dsp"

let bip u =
  for i = 0 to pred u do
    let j = sin(0.6 *. (float i)) in
    let k = ((j +. 1.0) /. 2.0) *. 127.0 in
    output_byte oc (truncate k)
  done

let gap u =
  for i = 0 to pred u do
    output_byte oc 0
  done

let morse =
  let u = 1000 in  (* length of one unit *)
  let u2 = u * 2 in
  let u3 = u * 3 in
  let u6 = u * 6 in
  String.iter (function
  | ' ' -> gap u6
  | 'a'..'z' | 'A'..'Z' | '0'..'9' as c ->
      let s = List.assoc c codes in
      String.iter (function
        '.' -> bip u; gap u
      | '-' -> bip u3; gap u
      | _ -> assert false
      ) s; gap u2
  | _ -> prerr_endline "unknown char")

let () = morse "rosettacode morse"
```



## Ol

There are no portable way to play the sound cross different OS, so this code only translates the input string into dot-dash notation.

To simplify the example will be used only lower letter case.

```ol

(display "Please, enter the string in lower case bounded by \" sign: ")
(lfor
   (list->ff '(
     (#\a . ".-"   ) (#\b . "-..." ) (#\c . "-.-." )
     (#\d . "-.."  ) (#\e . "."    ) (#\f . "..-." )
     (#\g . "--."  ) (#\h . "...." ) (#\i . ".."   )
     (#\j . ".---" ) (#\k . "-.-"  ) (#\l . ".-.." )
     (#\m . "--"   ) (#\n . "-."   ) (#\o . "---"  )
     (#\p . ".--." ) (#\q . "--.-" ) (#\r . ".-."  )
     (#\s . "..."  ) (#\t . "-"    ) (#\u . "..-"  )
     (#\v . "...-" ) (#\w . ".--"  ) (#\x . "-..-" )
     (#\y . "-.--" ) (#\z . "--.." ) (#\1 . ".----")
     (#\2 . "..---") (#\3 . "...--") (#\4 . "....-")
     (#\5 . ".....") (#\6 . "-....") (#\7 . "--...")
     (#\8 . "---..") (#\9 . "----.") (#\0 . "-----")
     (#\space . " ") (#\. . "  ")))
  (str-iter (read))
  (lambda (codes char)
     (let ((out (getf codes char)))
        (if out (display out)))
     codes))

; ==> Please, enter the string in lower case bounded by " sign:
; <== "hello world"
; ==> ......-...-..--- .-----.-..-..-..

```



## PARI/GP


```parigp
sleep(ms)={
  while((ms-=gettime()) > 0,);
};
dot()=print1(Strchr([7]));sleep(250);
dash()=print1(Strchr([7]));sleep(10);print1(Strchr([7]));sleep(10);print1(Strchr([7]));sleep(250);
Morse(s)={
  s=Vec(s);
  for(i=1,#s,
    if(s[i] == ".", dot(),
      if(s[i] == "-", dash(), sleep(250))
    )
  )
};

Morse("...---...")
```



## Pascal


Free Pascal in Delphi mode.
This program uses OpenAL for cross-platform PCM audio.


```Pascal

{$mode delphi}
PROGRAM cw;
		// Output a string as Morse code and CW.
		// Cross-platform PCM audio uses OpenAL
USES 	OpenAL, HRTimer;

		// Intl. Morse codes in ASCII order
CONST  	Morse: ARRAY [32..95] OF STRING = (' ','-.-.--','.-..-.','#','...-..-','%','.-...','.----.','-.--.','-.--.-','*','.-.-.','--..--','-....-','.-.-.-','-..-.','-----','.----','..---','...--','....-','.....','-....','--...','---..','----.','---...','-.-.-.','>','-...-','<','..--..','.--.-.','.-','-...','-.-.','-..','.','..-.','--.','....','..','.---','-.-','.-..','--','-.','---','.--.','--.-','.-.','...','-','..-','...-','.--','-..-','-.--','--..','-.--.','\','-.--.-','~','..--.-');
		// lengthen dah by this fraction of dit:
		// best = 0.4; also lengthens pauses
		doh = 0.4;
		// an 0.05 sec dit is around 26 wpm
		dit = 0.05;
		dah = 3 * dit + doh * dit;

VAR 	// OpenAL variables
		buffer : TALuint;
		source : TALuint;
		sourcepos: ARRAY [0..2] OF TALfloat= ( 0.0, 0.0, 0.0 );
		sourcevel: ARRAY [0..2] OF TALfloat= ( 0.0, 0.0, 0.0 );

		argv: ARRAY OF PalByte;
		format: TALEnum;
		size: TALSizei;
		freq: TALSizei;
		loop: TALInt;
		data: TALVoid;

		// rewinding has an effect on the output:
		// <with> and <without> sound rather different
		rewind	: BOOLEAN = FALSE;
		// the high-res timer is from Wolfgang Ehrhardt
		// http://www.wolfgang-ehrhardt.de/misc_en.html
		t		: THRTimer;
		msg		: STRING = 'the quick brown fox jumps over the lazy dog.';


	PROCEDURE PlayS(s: Extended);
		BEGIN
			StartTimer(t);
			AlSourcePlay(source);
			WHILE readseconds(t) < s DO BEGIN END;
			IF rewind THEN AlSourceRewind(source);
			AlSourceStop(source);
		END;

	PROCEDURE Pause(s: Extended);
		BEGIN
			StartTimer(t);
			WHILE readseconds(t) < s DO BEGIN END;
		END;

	PROCEDURE doDit;
		BEGIN
			PlayS(dit);
			Pause(dit);
		END;

	PROCEDURE doDah;
		BEGIN
			PlayS(dah);
			Pause(dit);
		END;

	// ASCII char to Morse CW
	FUNCTION AtoM(ch: CHAR): STRING;
		VAR i: Integer;
			u: CHAR;
		BEGIN
			u := ch;
			IF ch IN ['a'..'z'] THEN u := chr(ord(ch) AND $5F);
			result := Morse[ord(u)];
			FOR i := 1 TO Length(result) DO
				CASE result[i] OF
					'.': BEGIN doDit; Write('. ') END;
					'-': BEGIN doDah; Write('_ ') END;
				END;
			Pause(dah);
			Write('  ');
			IF u = ' ' THEN Write('  ');
		END;

	// ASCII string to Morse CW
	PROCEDURE StoM(s: STRING);
		VAR i: Integer;
		BEGIN
			FOR i := 1 TO Length(s) DO AtoM(s[i]);
		END;

BEGIN
	// OpenAL preparation
	InitOpenAL;
	AlutInit(nil,argv);

	AlGenBuffers(1, @buffer);
	// load the 500 Hz 1 sec sine-wave file
	// get it from http://audiocheck.net
	AlutLoadWavFile('audiocheck.net_sin_500Hz_-3dBFS_1s.wav', format, data, size, freq, loop);
	AlBufferData(buffer, format, data, size, freq);
	AlutUnloadWav(format, data, size, freq);

	AlGenSources(1, @source);
	AlSourcei ( source, AL_BUFFER, buffer);
	AlSourcef ( source, AL_PITCH, 1.0 );
	AlSourcef ( source, AL_GAIN, 1.0 );
	AlSourcefv ( source, AL_POSITION, @sourcepos);
	AlSourcefv ( source, AL_VELOCITY, @sourcevel);
	AlSourcei ( source, AL_LOOPING, AL_TRUE);

	// Sound and print the Morse
	StoM(msg);
	Pause(1.0);

	AlSourceRewind(source);
	AlSourceStop(source);

	// Clean up
	AlDeleteBuffers(1, @buffer);
	AlDeleteSources(1, @source);
	AlutExit();
END.

```



## Perl


```Perl
use Acme::AGMorse qw(SetMorseVals SendMorseMsg);
SetMorseVals(20,30,400);
SendMorseMsg('Hello World! abcdefg @\;');  # note, caps are ingnored in Morse Code
exit;
```


The above code requires:

```txt
Acme::AGMorse
Audio::Beep
Switch
```


Some known problems on UNIX:

```txt
1) pcspkr may not be available by default. Run: sudo modprobe pcspkr
2) pcspkr may be available but muted.
    - Check your sound prefrences,usually a right click over the speaker icon
```



## Perl 6

Here we use the user as the audio device.
Just read the output, leaving extra pauses where indicated
by either whitespace or underscore.

```perl6
my %m = ' ', '_ _ ',
|<
    !	---.
    "	.-..-.
    $	...-..-
    '	.----.
    (	-.--.
    )	-.--.-
    +	.-.-.
    ,	--..--
    -	-....-
    .	.-.-.-
    /	-..-.
    :	---...
    ;	-.-.-.
    =	-...-
    ?	..--..
    @	.--.-.
    [	-.--.
    ]	-.--.-
    _	..--.-
    0	-----
    1	.----
    2	..---
    3	...--
    4	....-
    5	.....
    6	-....
    7	--...
    8	---..
    9	----.
    A	.-
    B	-...
    C	-.-.
    D	-..
    E	.
    F	..-.
    G	--.
    H	....
    I	..
    J	.---
    K	-.-
    L	.-..
    M	--
    N	-.
    O	---
    P	.--.
    Q	--.-
    R	.-.
    S	...
    T	-
    U	..-
    V	...-
    W	.--
    X	-..-
    Y	-.--
    Z	--..
>.map: -> $c, $m is copy {
    $m.=subst(rx/'-'/, 'BGAAACK!!! ', :g);
    $m.=subst(rx/'.'/, 'buck ', :g);
    $c => $m ~ '_';
}

say prompt("Gimme a string: ").uc.comb.map: { %m{$_} // "<scratch> " }
```

Sample run:
<p>Gimme a string: <b>Howdy, World!</b>

buck buck buck buck _ BGAAACK!!! BGAAACK!!! BGAAACK!!! _ buck BGAAACK!!! BGAAACK!!! _ BGAAACK!!! buck buck _ BGAAACK!!! buck BGAAACK!!! BGAAACK!!! _ BGAAACK!!! BGAAACK!!! buck buck BGAAACK!!! BGAAACK!!! _ _ _ buck BGAAACK!!! BGAAACK!!! _ BGAAACK!!! BGAAACK!!! BGAAACK!!! _ buck BGAAACK!!! buck _ buck BGAAACK!!! buck buck _ BGAAACK!!! buck buck _ BGAAACK!!! BGAAACK!!! BGAAACK!!! buck _


## Phix

Windows only version

```Phix
sequence morse = repeat(0,255)

procedure setMorse(sequence data)
-- data is a list of strings, first char of each is the letter to encode,
--  with the rest being the actual morse code for that letter, eg "S..."
    for i=1 to length(data) do
        morse[data[i][1]] = data[i][2..$]   -- eg morse['S'] = "..."
    end for
end procedure

setMorse({"0-----","1.----","2..---","3...--","4....-","5.....","6-....","7--...","8---..","9----.",
          "A.-","B-...","C-.-.","D-..","E.","F..-.","G--.","H....","I..","J.---","K-.-","L.-..","M--",
          "N-.","O---","P.--.","Q--.-","R.-.","S...","T-","U..-","V...-","W.--","X-..-","Y-.--","Z--..",
          "!-.-.--","\".-..-.","$...-..-",":---...",";-.-.-.","=-...-","?..--..","@.--.-.","_..--.-",
          "&.-...","'.----.","(-.--.",")-.--.-","+.-.-.",",--..--","--....-","..-.-.-","/-..-.",
          "     "})

    morse['a'..'z'] = morse['A'..'Z']
    morse['['] = morse['(']
    morse[']'] = morse[')']

constant EOM = ".-.-."

constant frequency = 1280,          -- (in Hz, 37..32767)
         dit = 200,                 -- (in milliseconds)
         dah = 3*dit,               -- ""
         lettergap = 2*dit/1000,    -- (in seconds)
         wordgap = 4*dit/1000       -- ""

atom xBeep = 0

procedure beep(integer duration)
    if platform()=WIN32 then
        if xBeep=0 then
            atom kernel32 = open_dll("kernel32.dll")
            xBeep = define_c_proc(kernel32, "Beep", {C_INT,C_INT})
        end if
        c_proc(xBeep,{frequency,duration})
    end if
end procedure

procedure playAndRebuild(string line)
-- line should only contain '.'/'-'/' ', like the example below
string rebuilt = ""
integer start = 1
integer ch
    if length(line)=0 then
        line = "... --- ...      - .. - .- -. .. -.-. "
        puts(1,line)
    end if
    for i=1 to length(line) do
        ch = line[i]
        if ch=' ' then
            ch = find(line[start..i-1],morse)
            if ch!=0 then
                rebuilt &= ch
                start = i+1
                if ch=' ' then
                    sleep(wordgap)
                else
                    sleep(lettergap)
                end if
            end if
        elsif ch='.' then
            beep(dit)
        elsif ch='-' then
            beep(dah)
        end if
    end for
    puts(1,rebuilt)
    puts(1,"\n")
end procedure

procedure main()
integer key
object code
string line = ""

    puts(1,"enter text, return to play/rebuild, escape to quit\n")
    while 1 do
        key = wait_key()
        if key = 27 then exit end if    -- escape
        if key = 13 then                -- return
            playAndRebuild(line)
            line = ""
        else
            code = morse[key]
            if string(code) then
                code &= ' '
                puts(1,code)
                line &= code
            end if
        end if
    end while
    puts(1,EOM)
end procedure
main()
```



## PicoLisp

The following simply uses the 'beep' pc-speaker beeper utility.

```PicoLisp
# *Morse *Dit *Dah

(balance '*Morse
   (mapcar
      '((L)
         (def (car L)
            (mapcar = (chop (cadr L)) '("." .)) ) )
      (quote
         ("!"  "---.")     ("\"" ".-..-.")   ("$" "...-..-")   ("'" ".----.")
         ("(" "-.--.")     (")" "-.--.-")    ("+" ".-.-.")     ("," "--..--")
         ("-" "-....-")    ("." ".-.-.-")    ("/" "-..-.")
         ("0" "-----")     ("1" ".----")     ("2" "..---")     ("3" "...--")
         ("4" "....-")     ("5" ".....")     ("6" "-....")     ("7" "--...")
         ("8" "---..")     ("9" "----.")
         (":" "---...")    (";" "-.-.-.")    ("=" "-...-")     ("?" "..--..")
         ("@" ".--.-.")
         ("A" ".-")        ("B" "-...")      ("C" "-.-.")      ("D" "-..")
         ("E" ".")         ("F" "..-.")      ("G" "--.")       ("H" "....")
         ("I" "..")        ("J" ".---")      ("K" "-.-")       ("L" ".-..")
         ("M" "--")        ("N" "-.")        ("O" "---")       ("P" ".--.")
         ("Q" "--.-")      ("R" ".-.")       ("S" "...")       ("T" "-")
         ("U" "..-")       ("V" "...-")      ("W" ".--")       ("X" "-..-")
         ("Y" "-.--")      ("Z" "--..")
         ("[" "-.--.")     ("]" "-.--.-")    ("_" "..--.-") ) ) )

# Words per minute
(de wpm (N)
   (setq *Dit (*/ 1200 N)  *Dah (* 3 *Dit)) )

(wpm 20)

# Morse a string
(de morse (Str)
   (for C (chop Str)
      (cond
         ((sp? C) (wait (+ *Dah *Dit)))         # White space: Pause
         ((idx '*Morse (uppc C))                # Known character
            (for Flg (val (car @))
               (call "/usr/bin/beep" "-D" *Dit "-l" (if Flg *Dit *Dah)) ) )
         (T (call "/usr/bin/beep" "-f" 370)) )  # Unkown character
      (wait (- *Dah *Dit)) ) )

(morse "Hello world!")
```



## PL/I


```PL/I

/* Sound Morse code via the PC buzzer.           June 2011 */
MORSE: procedure options (main);
   declare (i, j) fixed binary;
   declare buzz character (1) static initial ('07'x);
   declare text character (100) varying,
      c character (1);
   declare alphabet character (36) static initial (
      'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789');
   declare morse_character character(5) varying;
   declare morse_codes(36) character(5) varying static initial (
      /* Letters A-Z */
      '.-',    '-...',   '-.-.',   '-..',    '.',
      '..-.',  '--.',    '....',   '..',     '.---',
      '-.-',   '.-..',   '--',     '-.',     '---',
      '--.-',  '--.-',   '.-.',    '...',    '-',
      '..-',   '...-',   '.--',    '-..-',   '-.--',
      '--..',
      /* Digits 0-9 */
      '-----', '.----',  '..---', '...--',   '....-',
      '.....', '-....',  '--...', '---..',   '----.' );

   put skip list ('Please type the text to be transmitted:');
   get edit (text) (L);

   do i = 1 to length (text);
      c = substr(text, i, 1);
      j = index(alphabet, uppercase(c));
      if j > 0 then
         do;
            morse_character = morse_codes(j);
            put skip list (morse_character); /* Display the Morse. */
            call send_morse (morse_character);
         end;
   end;

send_morse: procedure (morse_character);
   declare morse_character character(*) varying;
   declare i fixed binary;

   do i = 1 to length(morse_character);
      if substr(morse_character, 1, 1) = '-' then
         put skip edit (buzz, ' ', buzz) (a(1), A, skip, a(1));
      else
         put skip edit (buzz, ' ') (a(1));
      delay (1000); /* Delay one period. */
   end;
   delay (1000);
      /* Making a delay of 2 periods after each English letter. */
end send_morse;

END MORSE;
```




## PowerShell

This function is case insensitive, ignores all non-Morse characters and optionally displays the Morse code.

```PowerShell

function Send-MorseCode
{
    [CmdletBinding()]
    [OutputType([string])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   Position=0)]
        [string]
        $Message,

        [switch]
        $ShowCode
    )

    Begin
    {
        $morseCode = @{
            a = ".-"   ; b = "-..." ; c = "-.-." ; d = "-.."
            e = "."    ; f = "..-." ; g = "--."  ; h = "...."
            i = ".."   ; j = ".---" ; k = "-.-"  ; l = ".-.."
            m = "--"   ; n = "-."   ; o = "---"  ; p = ".--."
            q = "--.-" ; r = ".-."  ; s = "..."  ; t = "-"
            u = "..-"  ; v = "...-" ; w = ".--"  ; x = "-..-"
            y = "-.--" ; z = "--.." ; 0 = "-----"; 1 = ".----"
            2 = "..---"; 3 = "...--"; 4 = "....-"; 5 = "....."
            6 = "-...."; 7 = "--..."; 8 = "---.."; 9 = "----."
        }
    }
    Process
    {
        foreach ($word in $Message)
        {
            $word.Split(" ",[StringSplitOptions]::RemoveEmptyEntries) | ForEach-Object {

                foreach ($char in $_.ToCharArray())
                {
                    if ($char -in $morseCode.Keys)
                    {
                        foreach ($code in ($morseCode."$char").ToCharArray())
                        {
                            if ($code -eq ".") {$duration = 250} else {$duration = 750}

                            [System.Console]::Beep(1000, $duration)
                            Start-Sleep -Milliseconds 50
                        }

                        if ($ShowCode) {Write-Host ("{0,-6}" -f ("{0,6}" -f $morseCode."$char")) -NoNewLine}
                    }
                }

                if ($ShowCode) {Write-Host}
            }

            if ($ShowCode) {Write-Host}
        }
    }
}

```


```PowerShell

Send-MorseCode -Message "S.O.S" -ShowCode

```

```txt

...   ---   ...

```


```PowerShell

"S.O.S", "Goodbye, cruel world!" | Send-MorseCode -ShowCode

```

```txt

   ...   ---   ...

   --.   ---   ---   -..  -...  -.--     .
  -.-.   .-.   ..-     .  .-..
   .--   ---   .-.  .-..   -..

```



## PureBasic


```PureBasic
#BaseTime =50
#Frequence=1250
#Short    =   #BaseTime
#Long     =3* #BaseTime
#Intergap =   #BaseTime
#LetterGap=3* #BaseTime
#WordGap  =7* #BaseTime

Declare.s TextToMorse(Text$)
Declare.i PlayMorse(Text$)

Text$ =InputRequester("Morse coder","Enter text to send","Hello RosettaWorld!")
Text$ =TextToMorse(Text$)
If Not (InitSound() And PlayMorse(Text$))
  Text$=ReplaceString(Text$, ",","")
  MessageRequester("Morse EnCoded",Text$)
EndIf

;-
Procedure PlayMorse(Code$)    ;- Beep() is normally only Ok on Windows_x86
  CompilerIf #PB_Compiler_Processor=#PB_Processor_x86 And #PB_Compiler_OS=#PB_OS_Windows
    Protected i, sign
    For i=1 To Len(Code$)
      sign=Asc(Mid(Code$,i,1))
      Select sign
        Case '.': Beep_(#Frequence,#Short): Delay(#Intergap)
        Case '-': Beep_(#Frequence,#Long) : Delay(#Intergap)
        Case ',': Delay(#LetterGap)
        Case ' ': Delay(#WordGap)
      EndSelect
    Next
    ProcedureReturn 1
  CompilerElse
    ProcedureReturn 0
  CompilerEndIf
EndProcedure

Procedure.s TextToMorse(InString$)
  Protected *p.Character=@InString$, CurrStr$, i=1
  Protected.s s1, s2, result
  Repeat
    If Not *p\c: Break: EndIf
    CurrStr$=UCase(PeekS(*p,1))
    *p+StringByteLength(">")
    Restore MorseCode
    Repeat
      Read.s s1
      If s1="Done"
        s2+s1+" " ; failed to find this coding
        Break
      ElseIf Not s1=CurrStr$
        Continue
      EndIf
      Read.s s2
      result+s2
      If s2<>" "
        result+","
      EndIf
    ForEver
  ForEver
  ProcedureReturn result
EndProcedure

DataSection
  MorseCode:
  Data.s "A",  ".-"
  Data.s "B",  "-..."
  Data.s "C",  "-.-."
  Data.s "D",  "-.."
  Data.s "E",  "."
  Data.s "F",  "..-."
  Data.s "G",  "--."
  Data.s "H",  "...."
  Data.s "I",  ".."
  Data.s "J",  ".---"
  Data.s "K",  "-.-"
  Data.s "L",  ".-.."
  Data.s "M",  "--"
  Data.s "N",  "-."
  Data.s "O",  "---"
  Data.s "P",  ".--."
  Data.s "Q",  "--.-"
  Data.s "R",  ".-."
  Data.s "S",  "..."
  Data.s "T",  "-"
  Data.s "U",  "..-"
  Data.s "V",  "...-"
  Data.s "W",  ".--"
  Data.s "X",  "-..-"
  Data.s "Y",  "-.--"
  Data.s "Z",  "--.."
  Data.s "Á",  "--.-"
  Data.s "Ä",  ".-.-"
  Data.s "É",  "..-.."
  Data.s "Ñ",  "--.--"
  Data.s "Ö",  "---."
  Data.s "Ü",  "..--"
  Data.s "1",  ".----"
  Data.s "2",  "..---"
  Data.s "3",  "...--"
  Data.s "4",  "....-"
  Data.s "5",  "....."
  Data.s "6",  "-...."
  Data.s "7",  "--..."
  Data.s "8",  "---.."
  Data.s "9",  "----."
  Data.s "0",  "-----"
  Data.s ",",  "--..--"
  Data.s ".",  ".-.-.-"
  Data.s "?",  "..--.."
  Data.s ";",  "-.-.-"
  Data.s ":",  "---..."
  Data.s "/",  "-..-."
  Data.s "-",  "-....-"
  Data.s "'",  ".----."
  Data.s "+",  ".-.-."
  Data.s "-",  "-....-"
  Data.s #DOUBLEQUOTE$, ".-..-."
  Data.s "@",  ".--.-."
  Data.s "(",  "-.--."
  Data.s ")",  "-.--.-"
  Data.s "_",  "..--.-"
  Data.s "$",  "...-..-"
  Data.s "&",  ".-..."
  Data.s "=",  "---..."
  Data.s " ",  " "
  Data.s  "Done",""
  EndOfMorseCode:
EndDataSection
```



## Python


```python
import time, winsound #, sys

char2morse = {
          "!": "---.",      "\"": ".-..-.",     "$": "...-..-",    "'": ".----.",
          "(": "-.--.",      ")": "-.--.-",     "+": ".-.-.",      ",": "--..--",
          "-": "-....-",     ".": ".-.-.-",     "/": "-..-.",
          "0": "-----",      "1": ".----",      "2": "..---",      "3": "...--",
          "4": "....-",      "5": ".....",      "6": "-....",      "7": "--...",
          "8": "---..",      "9": "----.",
          ":": "---...",     ";": "-.-.-.",     "=": "-...-",      "?": "..--..",
          "@": ".--.-.",
          "A": ".-",         "B": "-...",       "C": "-.-.",       "D": "-..",
          "E": ".",          "F": "..-.",       "G": "--.",        "H": "....",
          "I": "..",         "J": ".---",       "K": "-.-",        "L": ".-..",
          "M": "--",         "N": "-.",         "O": "---",        "P": ".--.",
          "Q": "--.-",       "R": ".-.",        "S": "...",        "T": "-",
          "U": "..-",        "V": "...-",       "W": ".--",        "X": "-..-",
          "Y": "-.--",       "Z": "--..",
          "[": "-.--.",      "]": "-.--.-",     "_": "..--.-",
 }

e = 50      # Element time in ms. one dit is on for e then off for e
f = 1280    # Tone freq. in hertz
chargap = 1 # Time between characters of a word, in units of e
wordgap = 7 # Time between words, in units of e

def gap(n=1):
    time.sleep(n * e / 1000)
off = gap

def on(n=1):
    winsound.Beep(f, n * e)

def dit():
    on(); off()

def dah():
    on(3); off()

def bloop(n=3):
    winsound.Beep(f//2, n * e)

def windowsmorse(text):
    for word in text.strip().upper().split():
        for char in word:
            for element in char2morse.get(char, '?'):
                if element == '-':
                    dah()
                elif element == '.':
                    dit()
                else:
                    bloop()
            gap(chargap)
        gap(wordgap)

# Outputs its own source file as Morse. An audible quine!
#with open(sys.argv[0], 'r') as thisfile:
#    windowsmorse(thisfile.read())

while True:
    windowsmorse(input('A string to change into morse: '))

```



## Racket

Using MIDI on Windows for the beeps.

```racket

#lang racket
(require ffi/unsafe ffi/unsafe/define)
(define-ffi-definer defmm (ffi-lib "Winmm"))
(defmm midiOutOpen (_fun [h : (_ptr o _int32)] [_int = -1] [_pointer = #f]
                         [_pointer = #f] [_int32 = 0] -> _void -> h))
(defmm midiOutShortMsg (_fun _int32 _int32 -> _void))
(define M (midiOutOpen))
(define (midi x y z) (midiOutShortMsg M (+ x (* 256 y) (* 65536 z))))

(define raw-codes
  '("a.-|b-...|c-.-.|d-..|e.|f..-.|g--.|h....|i..|j.---|k-.-|l.-..|m--|n-."
    "|o---|p--.-|q--.-|r.-.|s...|t-|u..-|v...-|w.--|x-..-|y-.--|z--..|1.----"
    "|2..---|3...--|4....-|5.....|6-....|7--...|8---..|9----.|0-----"))

(define codes
  (for/list ([x (regexp-split #rx"\\|" (string-append* raw-codes))])
    (cons (string-ref x 0) (substring x 1))))

(define (morse str [unit 0.1])
  (define (sound len)
    (midi #x90 72 127) (sleep (* len unit))
    (midi #x90 72 0)   (sleep unit))
  (define (play str)
    (midi #xC0 #x35 0) ; use a cute voice
    (for ([c str])
      (case c [(#\.) (sound 1)] [(#\-) (sound 3)] [(#\ ) (sleep (* 3 unit))])))
  (let* ([str (string-foldcase str)]
         [str (regexp-replace* #rx"[,:;]+" str " ")]
         [str (regexp-replace* #rx"[.!?]+" str ".")]
         [str (string-normalize-spaces str)])
    (for ([s (string-split str)])
      (define m
        (string-join
         (for/list ([c s])
           (cond [(assq c codes) => cdr]
                 [else (case c [(#\space) " "] [(#\.) "  "] [else ""])]))))
      (printf "~a: ~a\n" s m)
      (play (string-append m "  ")))))

(morse "Say something here")

```



## Red

The code must be compiled, because it contains red system code that is needed for the winapi calls
so use "red.exe -c morse.red " from command line to compile with red runtime.dll
or "red.exe -r morse.red " to compile to single .exe file

each character will be printed with its corresponding code before played

```Red
Red [
  file: %morse.red   ;; filename, could be ommited
]
; ";" is character for comment, i use double ones for better readability

DIT: 100 ;; constant : 100 ms for short Beep
FREQ: 700 ;; frequency for Beep

;; exported code for red/system win api calls to Beep / Sleep:
#include %api.reds

;; string with morse codes for alphabet:
;; ( caution, u must use "str: copy ..."  if code ist to be executed multiple times ! )
str: "A.-B-...C-.-.D-..E.F..-.G--.H....I..J.---K-.-L.-..M--N-."
append str "O---P.--.Q--.-R.-.S...T-U..-V...-W.--X-..-Y-.--Z--.."

 delim: charset [#"A" - #"Z"]

 ;; use of parse to generate "mc" morse code series / array containing codes for A - Z
 ;; use characters only as delimiter for each code
 mc:  parse str [ thru "A"  collect some [ keep copy result to [delim | end ] skip ] ]

  ;;--------------------------------------------
 send-code: func ["function to play morse code for character "
 ;;--------------------------------------------
           chr [char!]  ;; character A .. Z
      ][
      sleep 500           ;; short break so u can read the character first
      ind:   to-integer chr - 64 ;; calculate index for morse array
      foreach sym mc/:ind [     ;; foreach symbol of code for character ...
        prin sym                  ;; prin(t) "." or "-"
        either sym = #"." [     ;; short beep
          beep FREQ DIT
      ][
        beep FREQ 3 * DIT     ;; or long beep = 3 x short
      ]
      sleep DIT                   ;; short break after each character
    ]
]
 ;;----------------------------------------------
 morse-text: func ["extract valid characters from sentence"
 ;;----------------------------------------------
        msg [string!]
][
 foreach chr uppercase msg [
    prin  chr prin " "    ;; print character
  ;; valid character  A-Z ?
   either   (chr >= #"A") and (chr <= #"Z") [
      send-code chr
    ] [          ;; ... "else" word gap or unknown
      sleep 6 * DIT   ;; pause after word
    ]
    prin newline    ;; equal to :  print """ ,( prin prints without crlf )
  ]
  sleep 6 * DIT  ;; pause after sentence
 ]
 ;;----------------------------------

morse-text "rosetta code"
morse-text "hello world"

```


```Red
Red/System [
  file: %api.reds ;; filename, could be ommited
]

; --- lib import -----
;; for winapi functions "Beep" and "Sleep"
#system [ #import [  "kernel32.dll" stdcall [
      wbeep: "Beep" [
           frequ    [integer!]
           dur [integer!]
          return: [integer!]
       ]
         wsleep: "Sleep" [ dur [integer!]  ]
] ] ]

beep: routine [
        freq [integer!]
        duration [integer!]
        return: [integer!]
      ]  [ wbeep freq duration  ]

sleep: routine [
        duration [integer!]
      ]  [ wsleep duration  ]
;;----------------------------------------------

```



## REXX

The   '''$MORSE.REX'''   REXX program is included here   ──►   [[$MORSE.REX]].


This program supports the   ''International Morse code''   as well as the   ''USA Morse code''   (the later being primarily used by the North American Railroads).

Some translation is done for unsupported characters such as braces   '''{'''   '''}''', brackets   '''['''   ''']'''   and the like.

This REXX program normally shows Morse code words one word to a line before sounding.


This REXX programs only works for Regina and PC/REXX, but other REXXes (specifically R4) will only display the Morse code, but not sound it.



'''output'''   when using the input of:   <tt> ( CQD --- indicates the vessel sending is in distress and requires immediate assistance. </tt>

```txt

-∙-∙ --∙- -∙∙
-∙∙∙∙- -∙∙∙∙- -∙∙∙∙-
∙∙ -∙ -∙∙ ∙∙ -∙-∙ ∙- - ∙ ∙∙∙
- ∙∙∙∙ ∙
∙∙∙- ∙ ∙∙∙ ∙∙∙ ∙ ∙-∙∙
∙∙∙ ∙ -∙ -∙∙ ∙∙ -∙ --∙
∙∙ ∙∙∙
∙∙ -∙
-∙∙ ∙∙ ∙∙∙ - ∙-∙ ∙ ∙∙∙ ∙∙∙
∙- -∙ -∙∙
∙-∙ ∙ --∙- ∙∙- ∙∙ ∙-∙ ∙ ∙∙∙
∙∙ -- -- ∙ -∙∙ ∙∙ ∙- - ∙
∙- ∙∙∙ ∙∙∙ ∙∙ ∙∙∙ - ∙- -∙ -∙-∙ ∙ ∙-∙-∙-

```



## Ring


```ring



 morsecode = [["a", ".-"],
                      ["b", "-..."],
                      ["c", "-.-."],
                      ["d", "-.."],
                      ["e", "."],
                      ["f", "..-."],
                      ["g", "--."],
                      ["h", "...."],
                      ["i", ".."],
                      ["j", ".---"],
                      ["k", "-.-"],
                      ["l", ".-.."],
                      ["m", "--"],
                      ["n", "-."],
                      ["o", "---"],
                      ["p", ".--."],
                      ["q", "--.-"],
                      ["r", ".-."],
                      ["s", "..."],
                      ["t", "-"],
                      ["u", "..-"],
                      ["v", "...-"],
                      ["w", ".--"],
                      ["x", "-..-"],
                      ["y", "-.--"],
                      ["z", "--.."],
                      ["0", "-----"],
                      ["1", ".----"],
                      ["2", "..---"],
                      ["3", "...--"],
                      ["4", "....-"],
                      ["5", "....."],
                      ["6", "-...."],
                      ["7", "--..."],
                      ["8", "---.."],
                      ["9", "----."]]
strmorse = ""
str = "this is a test text"
for n = 1 to len(str)
     pos = 0
     for m = 1 to len(morsecode)
          if morsecode[m][1] = str[n]
             pos = m
          ok
     next
     if str[n] = " "
        strmorse = strmorse + " "
     else
        if pos > 0
           strmorse = strmorse + morsecode[pos][2] + "|"
        ok
      ok
next
strmorse = left(strmorse,len(strmorse)-1)
see strmorse + nl

```

Output:

```txt

-|....|..|...| ..|...| .-| -|.|...|-| -|.|-..-|-

```



## Ruby

{{works with|Ruby|1.8.7+}} (uses <code>each_char</code>)
```ruby
require 'win32/sound'

class MorseCode
  MORSE = {
      "!" => "---.", "\"" => ".-..-.", "$" => "...-..-", "'" => ".----.",
      "(" => "-.--.", ")" => "-.--.-", "+" => ".-.-.", "," => "--..--",
      "-" => "-....-", "." => ".-.-.-", "/" => "-..-.", "0" => "-----",
      "1" => ".----", "2" => "..---", "3" => "...--", "4" => "....-", "5" => ".....",
      "6" => "-....", "7" => "--...", "8" => "---..", "9" => "----.", ":" => "---...",
      ";" => "-.-.-.", "=" => "-...-", "?" => "..--..", "@" => ".--.-.", "A" => ".-",
      "B" => "-...", "C" => "-.-.", "D" => "-..", "E" => ".", "F" => "..-.",
      "G" => "--.", "H" => "....", "I" => "..", "J" => ".---", "K" => "-.-",
      "L" => ".-..", "M" => "--", "N" => "-.", "O" => "---", "P" => ".--.",
      "Q" => "--.-", "R" => ".-.", "S" => "...", "T" => "-", "U" => "..-",
      "V" => "...-", "W" => ".--", "X" => "-..-", "Y" => "-.--", "Z" => "--..",
      "[" => "-.--.", "]" => "-.--.-", "_" => "..--.-",
  }

  T_UNIT = 75 # ms
  FREQ = 700
  DIT = 1 * T_UNIT
  DAH = 3 * T_UNIT
  CHARGAP = 1 * T_UNIT
  WORDGAP = 7 * T_UNIT

  def initialize(string)
    @message = string
    puts "your message is #{string.inspect}"
  end

  def send
    @message.strip.upcase.split.each do |word|
      word.each_char do |char|
        send_char char
        pause CHARGAP
        print " "
      end
      pause WORDGAP
      puts ""
    end
  end

  private
  def send_char(char)
    MORSE[char].each_char do |code|
      case code
      when '.' then beep DIT
      when '-' then beep DAH
      end
      pause CHARGAP
      print code
    end
  end

  def beep(ms)
    ::Win32::Sound.beep(FREQ, ms)
  end

  def pause(ms)
    sleep(ms.to_f/1000.0)
  end
end

MorseCode.new('sos').send
MorseCode.new('this is a test.').send
```


```txt
your message is "sos"
... --- ...
your message is "this is a test."
- .... .. ...
.. ...
.-
- . ... - .-.-.-
```



## Rust


Original code can be found on [https://github.com/Dragonrun1/morse_code GitHub].

morse_code/src/main.rs file:


```rust

//!
//! morse_code/src/main.rs
//!
//! Michael G. Cummings
//! 2019-08-26
//!
//! Since Rust doesn't have build-in audio support text output is used.
//!

use std::process;
use structopt::StructOpt;
use morse_code::{Config, Opt, run};

/// Core of the command-line binary.
///
/// By default expects input from stdin and outputs resulting morse code to stdout, but can also
/// read and/or write to files.
/// Use `morse_code --help` for more information about options.
fn main() {
    let opts = Opt::from_args();
    let mut config = Config::new(opts).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        process::exit(1);
    });
    if let Err(err) = run(&mut config) {
        eprintln!("Application error: {}", err);
        process::exit(2);
    }
}

```


morse_code/src/lib.rs file:


```rust

//!
//! morse_code/src/lib.rs
//!
//! Michael G. Cummings
//! 2019-08-26
//!

#[macro_use]
extern crate structopt;

use std::{fs, io};
use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;

/// Main library function that does the actual work.
///
/// Each character has one space between them and there are two spaces between words.
/// Unknown characters in the input are replaced with a '#' in the output.
///
pub fn run(config: &mut Config) -> Result<(), Box<dyn Error>> {
    let mut contents = String::new();
    config.read.read_to_string(&mut contents)?;
    let morse_map = init_code_map();
    let mut result = String::new();
    for char in contents.trim().to_uppercase().chars() {
        match morse_map.get(&char) {
            Some(hash) => {
                result = result + *hash;
            }
            None => { result = result + "#" }
        }
        result = result + " ";
    }
    config.write.write(result.as_ref())?;
    Ok(())
}

/// Configuration structure for the input and output streams.
#[derive(Debug)]
pub struct Config {
    read: Box<dyn io::Read>,
    write: Box<dyn io::Write>,
}

impl Config {
    pub fn new(opts: Opt) -> Result<Config, &'static str> {
        let input: Box<dyn io::Read> = match opts.input {
            Some(p) => Box::new(fs::File::open(p).unwrap()),
            None => Box::new(io::stdin()),
        };
        let output: Box<dyn io::Write> = match opts.output {
            Some(p) => Box::new(fs::File::create(p).unwrap()),
            None => Box::new(io::stdout()),
        };
        Ok(Config { read: input, write: output })
    }
}

/// Structure used to hold command line opts(parameters) of binary.
///
/// Using StructOpt crate to parse command-line parameters/options.
///
#[derive(Debug, StructOpt)]
#[structopt(rename_all = "kebab-case", raw(setting = "structopt::clap::AppSettings::ColoredHelp"))]
pub struct Opt {
    /// Input file, stdin if not present
    #[structopt(short, long, parse(from_os_str))]
    input: Option<PathBuf>,
    /// Output file, stdout if not present
    #[structopt(short, long, parse(from_os_str))]
    output: Option<PathBuf>,
}

/// Initialize hash map of characters to morse code as string.
pub fn init_code_map() -> HashMap<char, &'static str> {
    let mut morse_map: HashMap<char, &str> = HashMap::with_capacity(37);
    morse_map.insert(' ', " ");
    morse_map.insert('A', "._");
    morse_map.insert('B', "_...");
    morse_map.insert('C', "_._.");
    morse_map.insert('D', "_..");
    morse_map.insert('E', ".");
    morse_map.insert('F', ".._.");
    morse_map.insert('G', "__.");
    morse_map.insert('H', "....");
    morse_map.insert('I', "..");
    morse_map.insert('J', ".___");
    morse_map.insert('K', "_._");
    morse_map.insert('L', "._..");
    morse_map.insert('M', "__");
    morse_map.insert('N', "_.");
    morse_map.insert('O', "___");
    morse_map.insert('P', ".__.");
    morse_map.insert('Q', "__._");
    morse_map.insert('R', "._.");
    morse_map.insert('S', "...");
    morse_map.insert('T', "_");
    morse_map.insert('U', ".._");
    morse_map.insert('V', "..._");
    morse_map.insert('W', ".__");
    morse_map.insert('X', "_.._");
    morse_map.insert('Y', "_.__");
    morse_map.insert('Z', "__..");
    morse_map.insert('1', ".____");
    morse_map.insert('2', "..___");
    morse_map.insert('3', "...__");
    morse_map.insert('4', "...._");
    morse_map.insert('5', ".....");
    morse_map.insert('6', "_....");
    morse_map.insert('7', "__...");
    morse_map.insert('8', "___..");
    morse_map.insert('9', "____.");
    morse_map.insert('0', "_____");
    morse_map
}

```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/9Dsd74J/1 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/68KAarvEQQafYevTkWaCWg Scastie (remote JVM)].

```Scala
object MorseCode extends App {

  private val code = Map(
    ('A', ".-     "), ('B', "-...   "), ('C', "-.-.   "), ('D', "-..    "),
    ('E', ".      "), ('F', "..-.   "), ('G', "--.    "), ('H', "....   "),
    ('I', "..     "), ('J', ".---   "), ('K', "-.-    "), ('L', ".-..   "),
    ('M', "--     "), ('N', "-.     "), ('O', "---    "), ('P', ".--.   "),
    ('Q', "--.-   "), ('R', ".-.    "), ('S', "...    "), ('T', "-      "),
    ('U', "..-    "), ('V', "...-   "), ('W', ".-   - "), ('X', "-..-   "),
    ('Y', "-.--   "), ('Z', "--..   "), ('0', "-----  "), ('1', ".----  "),
    ('2', "..---  "), ('3', "...--  "), ('4', "....-  "), ('5', ".....  "),
    ('6', "-....  "), ('7', "--...  "), ('8', "---..  "), ('9', "----.  "),
    ('\'', ".----."), (':', "---... "), (',', "--..-- "), ('-', "-....- "),
    ('(', "-.--.- "), ('.', ".-.-.- "), ('?', "..--.. "), (';', "-.-.-. "),
    ('/', "-..-.  "), ('-', "..--.- "), (')', "---..  "), ('=', "-...-  "),
    ('@', ".--.-. "), ('"', ".-..-. "), ('+', ".-.-.  "), (' ', "/")) // cheat a little

  private def printMorse(input: String): Unit = {
    println(input)
    println(input.trim.replaceAll("[ ]+", " ").toUpperCase
      .map(code.getOrElse(_, "").trim).mkString(" "))
  }

  printMorse("sos")
  printMorse("   Hello     World!")
  printMorse("Rosetta Code")

}
```


## sed

Translation of AWK:

```sed
#!/bin/sed -rf
# Convert to uppercase
s/.*/\U&/
# Add lookup table
s/$/\nA.-B-...C-.-.D-..E.F..-.G--.H....I..J.---K-.-L.-..M--N-.O---P.--.Q--.-R.-.S...T-U..-V...-W.--X-..-Y-.--Z--../
# Main loop
:a
s/([A-Z])([^\n]*\n.*\1([-.]+))/\3 \2/
ta
# Remove lookup table
s/\n.*//
```

Example:

```bash

$ echo hello world! | ./morse.sed
.... . .-.. .-.. ---  .-- --- .-. .-.. -.. !
```



## Tcl

```tcl
# This uses the GUI-free part of the Snack library
package require sound

# A simple pause while running the event loop, in terms of basic time units
proc pause n {
    global t
    after [expr {$t * $n}] set ok 1
    vwait ok
}
# Generate using a sine-wave filter
proc beep n {
    global frequency
    set f [snack::filter generator $frequency 30000 0.0 sine -1]
    set s [snack::sound -rate 22050]
    $s play -filter $f
    pause $n
    $s stop
    $s destroy
    $f destroy
    pause 1
}
# The dits and the dahs are just beeps of different lengths
interp alias {} dit {} beep 1
interp alias {} dah {} beep 3

set MORSE_CODE {
    "!" "---."	 "\"" ".-..-."	"$" "...-..-"	"'" ".----."
    "(" "-.--."	 ")" "-.--.-"	"+" ".-.-."	"," "--..--"
    "-" "-....-" "." ".-.-.-"	"/" "-..-."
    ":" "---..." ";" "-.-.-."	"=" "-...-"	"?" "..--.."
    "@" ".--.-." "[" "-.--."	"]" "-.--.-"	"_" "..--.-"
    "0" "-----"	 "1" ".----"	"2" "..---"	"3" "...--"
    "4" "....-"	 "5" "....."	"6" "-...."	"7" "--..."
    "8" "---.."	 "9" "----."
    "A" ".-"	 "B" "-..."	"C" "-.-."	"D" "-.."
    "E" "."	 "F" "..-."	"G" "--."	"H" "...."
    "I" ".."	 "J" ".---"	"K" "-.-"	"L" ".-.."
    "M" "--"	 "N" "-."	"O" "---"	"P" ".--."
    "Q" "--.-"	 "R" ".-."	"S" "..."	"T" "-"
    "U" "..-"	 "V" "...-"	"W" ".--"	"X" "-..-"
    "Y" "-.--"	 "Z" "--.."
}

# The code to translate text to morse code and play it
proc morse {str wpm} {
    global t MORSE_CODE
    set t [expr {1200 / $wpm}]
    # Backslash and space are special cases in various ways
    set map {"\\" {} " " {[pause 4]}}
    # Append each item in the code to the map, with an inter-letter pause after
    foreach {from to} $MORSE_CODE {lappend map $from "$to\[pause 3\]"}
    # Convert to dots and dashes
    set s [string map $map [string toupper $str]]
    # Play the dots and dashes by substituting commands for them
    subst [string map {"." [dit] "-" [dah]} $s]
    return
}

# We'll play at a fairly high pitch
set frequency 700

morse "Morse code with Tcl and Snack." 20
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
MODE DATA
$$ BUILD X_TABLE alfabet2moco =*
"!"---."\".-..-."$"...-..-"'".----."
"("-.--.")"-.--.-"+".-.-.","--..--"
"-"-....-".".-.-.-"/"-..-."
"0"-----"1".----"2"..---"3"...--"
"4"....-"5"....."6"-...."7"--..."
"8"---.."9"----."
":"---...";"-.-.-."="-...-"?"..--.."
"@".--.-."
"A".-"B"-..."C"-.-."D"-.."
"E"."F"..-."G"--."H"...."
"I".."J".---"K"-.-"L".-.."
"M"--"N"-."O"---"P".--."
"Q"--.-"R".-."S"..."T"-"
"U"..-"V"...-"W".--"X"-..-"
"Y"-.--"Z"--.."
"["-.--."]"-.--.-"_"..--.-"
$$ BUILD X_TABLE moco2sound =*
" "p2 "
"-"a4 "
"."a2 "
$$ BUILD X_TABLE space2split=": :':"
$$ MODE TUSCRIPT
ASK "Please enter your sentence": mc=""
PRINT "SEE your morsecode !"
mc=EXCHANGE (mc,alfabet2moco)
PRINT mc
PRINT "HEAR your morsecode !"
mc=EXCHANGE (mc,moco2sound)
mc=EXCHANGE (mc,space2split)
BEEP $mc

```

```txt

Please enter your sentence >Hello World
SEE your morsecode !
......-...-..--- .-----.-..-..-..
HEAR your morsecode !

```

```txt

Please enter your sentence >SOS
SEE your morsecode !
...---...
HEAR your morsecode !

```



## Ursa

```ursa
decl ursa.util.sound snd
decl string<> chars
decl string<> morse

append "!"    "\""     "$"       "'"      "("     ")"      "+"     chars
append "---." ".-..-." "...-..-" ".----." "-.--." "-.--.-" ".-.-." morse
append ","      "-"      "."      "/"     "0"     "1"     "2"      chars
append "--..--" "-....-" ".-.-.-" "-..-." "-----" ".----" "..---"  morse
append "3"     "4"     "5"     "6"     "7"     "8"     "9"         chars
append "...--" "....-" "....." "-...." "--..." "---.." "----."     morse
append ":"      ";"      "="     "?"      "@"      "A"  "B"        chars
append "---..." "-.-.-." "-...-" "..--.." ".--.-." ".-" "-..."     morse
append "C"    "D"  "E" "F"    "G"   "H"    "I"  "J"    "K"         chars
append "-.-." "-." "." "..-." "--." "...." ".." ".---" "-.-"       morse
append "L"    "M"  "N"  "O"   "P"    "Q"    "R"   "S"   "T" "U"    chars
append ".-.." "--" "-." "---" ".--." "--.-" ".-." "..." "-" "..-"  morse
append "V"    "W"   "X"    "Y"    "Z"    "["     "]"      "_"      chars
append "...-" ".--" "-..-" "-.--" "--.." "-.--." "-.--.-" "..--.-" morse

decl int e f chargap wordgap
# element time in ms. one dot is on for e then off for e
set e 50
# tone frequency in hertz
set f 1280
# time between characters of a word (in units of e)
set chargap 1
# time between words (in units of e)
set wordgap 7


def gap (int n)
	sleep (* n e)
end gap
decl function off
set off gap


def on (int n)
	 snd.beep f (/ (* n e) 1000)
end on


def dot ()
	on 1
	off 1
end dot


def dash ()
	on 3
	off 1
end dash


def bloop (int n)
	snd.beep (/ f 2) (/ (* n e) 1000)
end bloop


def encode_morse (string text)
	decl string<> words
	set words (split (upper (trim text)) " ")
	decl int i j k
	for () (< i (size words)) (inc i)
		for (set j 0) (< j (size words<i>)) (inc j)
			decl int loc
			set loc (locate words<i><j> chars)
			if (= loc -1)
				bloop 3
			else
				for (set k 0) (< k (size morse<loc>)) (inc k)
					if (= morse<loc><k> "-")
						dash
					elif (= morse<loc><k> ".")
						dot
					else
						bloop 3
					end if
				end for
			end if
			gap chargap
		end for
		gap wordgap
	end for
end encode_morse


# --- uncomment this block to output the source of this file as morse
# decl file src
# src.open args<0>
# encode_morse (src.readall)


while true
	out "A string to change into morse: " console
	encode_morse (in string console)
end while
```



## VBA



```vb
Option Explicit

Private Declare Function Beep Lib "kernel32" (ByVal dwFreq As Long, ByVal dwDuration As Long) As Long
Private Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)

Private Const MORSE_ALPHA As String = ".-,-...,-.-.,-..,.,..-.,--.,....,..,.---,-.-,.-..,--,-.,---,.--.,--.-,.-.,...,-,..-,...-,.--,-..-,-.--,--.."
Private Const MORSE_NUMERIC As String = "-----,.----,..---,...--,....-,.....,-....,--...,---..,----."

Private Const ONE_UNIT As Integer = 100

Private Const BEEP_DOT As Integer = ONE_UNIT
Private Const BEEP_DASH As Integer = 3 * ONE_UNIT
Private Const BEEP_OTHER As Integer = 7 * ONE_UNIT
Private Const DELAY As Integer = ONE_UNIT
Private Const LETTERS_DELAY As Integer = 3 * ONE_UNIT
Private Const SPACE_DELAY As Integer = 7 * ONE_UNIT

Private Const FREQUENCY_CHARS As Integer = 1200
Private Const FREQUENCY_OTHERCHARS As Integer = 400

Sub Main()
Dim p$, temp$
    p = ToMorse("Hel/lo 123 world")
    temp = Replace(p, "+", "")
    Debug.Print Replace(temp, "_", "")
    PlayMorse p
End Sub

Private Function ToMorse(s As String) As String
Dim i&, t$, j&
    s = UCase(s)
    For i = 1 To Len(s)
        j = Asc(Mid(s, i, 1))
        Select Case j
            Case 65 To 90 'alpha
                t = t & Split(MORSE_ALPHA, ",")(j - 65) & "+" ' "+" ==> separate each characters
            Case 48 To 57 'numerics
                t = t & Split(MORSE_NUMERIC, ",")(j - 48) & "+"
            Case 32 'space
                t = t & " " & "+"
            Case Else 'others
                t = t & "_" & "+"
        End Select
    Next i
    ToMorse = t
End Function

Private Sub PlayMorse(s As String)
Dim i&
    For i = 1 To Len(s)
        Select Case Mid(s, i, 1)
            Case ".": Beep FREQUENCY_CHARS, BEEP_DOT
            Case "-": Beep FREQUENCY_CHARS, BEEP_DASH
            Case "_": Beep FREQUENCY_OTHERCHARS, BEEP_OTHER
            Case "+": Sleep LETTERS_DELAY
            Case " ": Sleep SPACE_DELAY
        End Select
        Sleep DELAY
    Next i
End Sub
```

Play the sound's morse and display :

```txt
......-...-..--- .----..---...-- .-----.-..-..-..
```



## Visual Basic

The [[#VBA]] example works in VB6 as well, without any change.


## XPL0

This uses the Sound intrinsic, which has a minimum duration
of one system tick (about 1/18th of a second) for a dit (.).
The result is Morse code sent at about 20 words per minute.
The Sound intrinsic also provides the delay for sound gaps
when its volume parameter is set to zero.


```XPL0
code ChOut=8, CrLf=9, Sound=39;
string 0;               \use zero-terminated strings

proc Morse(Msg);        \Output Msg string as audible International Morse code
char Msg;
char C, D;
int  Code, Vol;
[Code:= ["    ",                                        \space
        ".-.-..",".-..-.", "    ", "...-..-","    ",    \!"#$%
        "----.", ".----.", "-.--.-", "---..",  "    ",  \&'()*
        ".-.-.", "--..--", "-....-", ".-.-.-", "-..-.", \+,-./
        "-----", ".----",  "..---",  "...--",  "....-", \01234
        ".....", "-....",  "--...",  "---..",  "----.", \56789
        "---...","-.-.-.", "    ",   "-...-",  "    ",  \:;<=>
        "..--..",".--.-.",                              \?@
        ".-",    "-...",   "-.-.",   "-..",    ".",     \ABCDE
        "..-.",  "--.",    "....",   "..",     ".---",  \FGHIJ
        "-.-",   ".-..",   "--",     "-.",     "---",   \KLMNO
        ".--.",  "--.-",   ".-.",    "...",    "-",     \PQRST
        "..-",   "...-",   ".--",    "-..-",   "-.--",  \UVWXY
        "--.."];                                        \Z
Sound(0, 1, 1);                                 \sync, for consistent durations
repeat  C:= Msg(0);  Msg:= Msg+1;               \get character from message
        ChOut(0, C);                            \show the character
        if C>=^a & C<=^z then C:= C-$20;        \convert letters to uppercase
        Vol:= 1;                                \assume volume is on (not space)
        if C>=$21 & C<=^Z then D:= Code(C-$20)  \convert characters to code
        else [D:= Code(0);  Vol:= 0];           \space (or unsupported char)
        repeat  Sound(Vol, if D(0)=^- then 3 else 1, 1190000/600);      \600 Hz
                Sound(0, 1, 1);                 \gap between . and -
                ChOut(0, D(0));                 \show dots and dashes
                D:= D+1;                        \next dot or dash for character
        until   D(0) = 0;                       \string terminator
        Sound(0, 2, 1);                         \gap between letters (2+1=3)
        ChOut(0, ^ );                           \show gap
until   Msg(0) = 0;                             \string terminator
];

[Morse("SOS SOS SOS ");                         \something easy to recognize
CrLf(0);
Morse("Hello, world!");
]
```

```txt

S... O--- S...       S... O--- S...       S... O--- S...
H.... e. l.-.. l.-.. o--- ,--..--       w.-- o--- r.-. l.-.. d-.. !.-.-..

```


## Yabasic

Mixin classic/modern style. In Yabasic, line number is not mandatory.

```Yabasic
10 REM Morse code
20 DIM c$(54)
30 FOR f = 1 TO 54
40 READ l$, m$
50 LET d$ = d$ + l$ : LET c$(f) = m$
60 NEXT f

100 LINE INPUT "Message? " t$
105 t$ = upper$(t$)
110 FOR f = 1 TO LEN(t$)
120 p = INSTR(d$, mid$(t$, f, 1))
130 IF p > 0 then
        PRINT c$(p);
    else
        print "?";
    end if
140 NEXT f
print
goto 100


1000 DATA "A","._","B","_...","C","_._.","D","_..","E",".","F",".._."
1010 DATA "G","__.","H","....","I","..","J",".___","K","_._","L","._.."
1020 DATA "M","__","N","_.","O","___","P",".__.","Q","__._","R","._."
1030 DATA "S","...","T","_","U",".._","V","..._","W",".__","X","_.._"
1040 DATA "Y","_.__","Z","__..","1",".____","2","..___","3","...__"
1050 DATA "4","...._","5",".....","6","_....","7","__...","8","___.."
1060 DATA "9","____.","0","_____",".","._._._",",","__..__","?","..__.."
1070 DATA "'",".____.","!","_._.__","/","_.._.","(","_.__.",")","_.__._"
1080 DATA "&","._...",":","___...",";","_._._.","=","_..._","+","._._.","-","_...._"
1090 DATA "_","..__._","\"","._.._.","$","..._.._","@",".__._."
```

