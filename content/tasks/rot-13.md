+++
title = "Rot-13"
description = ""
date = 2019-08-24T03:21:41Z
aliases = []
[extra]
id = 2253
[taxonomies]
categories = ["task", "Encryption"]
tags = []
+++

## Task

Implement a   '''rot-13'''   function   (or procedure, class, subroutine, or other "callable" object as appropriate to your programming environment).

Optionally wrap this function in a utility program   (like [[:Category:Tr|tr]],   which acts like a common [[UNIX]] utility, performing a line-by-line rot-13 encoding of every line of input contained in each file listed on its command line,   or (if no filenames are passed thereon) acting as a filter on its   "standard input."


(A number of UNIX scripting languages and utilities, such as   ''awk''   and   ''sed''   either default to processing files in this way or have command line switches or modules to easily implement these wrapper semantics, e.g.,   [[Perl]]   and   [[Python]]).

The   '''rot-13'''   encoding is commonly known from the early days of Usenet "Netnews" as a way of obfuscating text to prevent casual reading of   [[wp:Spoiler (media)|spoiler]]   or potentially offensive material.

Many news reader and mail user agent programs have built-in '''rot-13''' encoder/decoders or have the ability to feed a message through any external utility script for performing this (or other) actions.

The definition of the rot-13 function is to simply replace every letter of the ASCII alphabet with the letter which is "rotated" 13 characters "around" the 26 letter alphabet from its normal cardinal position   (wrapping around from   '''z'''   to   '''a'''   as necessary).

Thus the letters   '''abc'''   become   '''nop'''   and so on.

Technically '''rot-13''' is a   "mono-alphabetic substitution cipher"   with a trivial   "key".

A proper implementation should work on upper and lower case letters, preserve case, and pass all non-alphabetic characters
in the input stream through without alteration.


## Related tasks

*   [[Caesar cipher]]
*   [[Substitution Cipher]]
*   [[Vigenère Cipher/Cryptanalysis]]





## 360 Assembly

Sytem/360 uses EBCDIC encoding.
The power of the CISC architecture, only one hardware instruction (TR) does all the job!

```360asm
ROT13    CSECT
         USING  ROT13,R15          use calling register
         XPRNT  CC,L'CC
         TR     CC,TABLE           translate
         XPRNT  CC,L'CC
         TR     CC,TABLE           translate
         XPRNT  CC,L'CC
         BR     R14                return to caller
CC       DC     CL10'{NOWHERE!}'
TABLE    DC     CL64' '
*                    0123456789ABCDEF
         DC     CL16'           .<(+|'   X'4.'
         DC     CL16'          !$*);^'   X'5.'
         DC    CL16'&&          ,%_>?'   X'6.'
         DC     CL16'-/       `:#@''="'   X'7.'
         DC     CL16' nopqrstuv      '   X'8.'
         DC     CL16' wxyzabcde      '   X'9.'
         DC     CL16' ~fghijklm   [  '   X'A.'
         DC     CL16'             ]  '   X'B.'
         DC     CL16'{NOPQRSTUV      '   X'C.'
         DC     CL16'}WXYZABCDE      '   X'D.'
         DC     CL16'\ FGHIJKLM      '   X'E.'
         DC     CL16'0123456789      '   X'F.'
*                    0123456789ABCDEF
         YREGS
         END    ROT13
```

```txt
{NOWHERE!}
{ABJURER!}
{NOWHERE!}
```



## 6502 Assembly

Written for the BeebAsm assembler, which uses '&' to indicate a hexadecimal number. Call with the address of a zero terminated string in X and Y.
On exit X is hi-order address of the last page of the string, Y is the length of the string modulo 256 and A is zero unless the string is not terminated.


```6502asm
buffer = $fa               ; or anywhere in zero page that's good
maxpage = $95              ; if non-terminated that's bad

org $1900
.rot13
          stx buffer
          sty buffer+1
          ldy #0
          beq readit
.loop     cmp #$7b          ; high range
          bcs next          ; >= {
          cmp #$41          ; low range
          bcc next          ; < A
          cmp #$4e
          bcc add13         ; < N
          cmp #$5b
          bcc sub13set      ; < [
          cmp #$61
          bcc next          ; < a
          cmp #$6e
          bcs sub13         ; >= n
.add13    adc #13           ; we only get here via bcc; so clc not needed
          bne storeit
.sub13set sec               ; because we got here via bcc; so sec is needed
.sub13    sbc #13           ; we can get here via bcs; so sec not needed
.storeit  sta (buffer),y
.next     iny
          bne readit
          ldx buffer+1
          cpx maxpage
          beq done
          inx
          stx buffer+1
.readit   lda (buffer),y    ; quit on ascii 0
          bne loop
.done     rts
.end
save "rot-13", rot13, end
```



## ACL2


```lisp
(include-book "arithmetic-3/top" :dir :system)

(defun char-btn (c low high)
   (and (char>= c low)
        (char<= c high)))

(defun rot-13-cs (cs)
   (cond ((endp cs) nil)
         ((or (char-btn (first cs) #\a #\m)
              (char-btn (first cs) #\A #\M))
          (cons (code-char (+ (char-code (first cs)) 13))
                (rot-13-cs (rest cs))))
         ((or (char-btn (first cs) #\n #\z)
              (char-btn (first cs) #\N #\Z))
          (cons (code-char (- (char-code (first cs)) 13))
                (rot-13-cs (rest cs))))
         (t (cons (first cs) (rot-13-cs (rest cs))))))

(defun rot-13 (s)
   (coerce (rot-13-cs (coerce s 'list)) 'string))
```



## Ada


```ada
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Command_Line; use Ada.Command_Line;

procedure Rot_13 is

   From_Sequence : Character_Sequence := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
   Result_Sequence : Character_Sequence := "nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM";
   Rot_13_Mapping : Character_Mapping := To_Mapping(From_Sequence, Result_Sequence);

   In_Char : Character;
   Stdio : Stream_Access := Stream(Ada.Text_IO.Standard_Input);
   Stdout : Stream_Access := Stream(Ada.Text_Io.Standard_Output);
   Input : Ada.Text_Io.File_Type;

begin
   if Argument_Count > 0 then
      for I in 1..Argument_Count loop
         begin
            Ada.Text_Io.Open(File => Input, Mode => Ada.Text_Io.In_File, Name => Argument(I));
            Stdio := Stream(Input);
             while not Ada.Text_Io.End_Of_File(Input) loop
               In_Char :=Character'Input(Stdio);
               Character'Output(Stdout, Value(Rot_13_Mapping, In_Char));
            end loop;
            Ada.Text_IO.Close(Input);
         exception
            when Ada.Text_IO.Name_Error =>
               Ada.Text_Io.Put_Line(File => Ada.Text_Io.Standard_Error, Item => "File " & Argument(I) & " is not a file.");
            when Ada.Text_Io.Status_Error =>
               Ada.Text_Io.Put_Line(File => Ada.Text_Io.Standard_Error, Item => "File " & Argument(I) & " is already opened.");
         end;
      end loop;
   else
      while not Ada.Text_Io.End_Of_File loop
         In_Char :=Character'Input(Stdio);
         Character'Output(Stdout, Value(Rot_13_Mapping, In_Char));
      end loop;
   end if;
end Rot_13;
```



## ALGOL 68

<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - this release is missing on line end}} -->

```algol68
BEGIN
  CHAR c;
  on logical file end(stand in, (REF FILE f)BOOL: (stop; SKIP));
  on line end(stand in, (REF FILE f)BOOL: (print(new line); FALSE));
  DO
    read(c);
    IF c >= "A" AND c <= "M" OR c >= "a" AND c <= "m" THEN
      c := REPR(ABS c + 13)
    ELIF c >= "N" AND c <= "Z" OR c >= "n" AND c <= "z" THEN
      c := REPR(ABS c - 13)
    FI;
    print(c)
  OD
END # rot13 #
```

Sample run on linux:

```txt

$ echo Big fjords vex quick waltz nymph! | a68g Rot-13.a68
Ovt swbeqf irk dhvpx jnygm alzcu!

```


## AppleScript

Using '''do shell script'''

```Applescript
to rot13(textString)
do shell script "tr a-zA-Z n-za-mN-ZA-M <<<" & quoted form of textString
end rot13
```

Pure AppleScript solution

```Applescript
to rot13(textString)
  local outChars
  set outChars to {}
  repeat with ch in (characters of textString)
    if (ch >= "a" and ch <= "m") or (ch >= "A" and ch <= "M") then
      set ch to character id (id of ch + 13)
    else if (ch >= "n" and ch <= "z") or (ch >= "N" and ch <= "Z") then
      set ch to character id (id of ch - 13)
    end
    set end of outChars to ch
  end
  return outChars as text
end rot13

```

Demo code:

```AppleScript
rot13("nowhere ABJURER")
```

```txt
abjurer NOWHERE
```



Or using some generic primitives, and a slightly more functional style of composition:


```AppleScript
-- ROT 13 --------------------------------------------------------------------

-- rot13 :: String -> String
on rot13(str)
    script rt13
        on |λ|(x)
            if (x ≥ "a" and x ≤ "m") or (x ≥ "A" and x ≤ "M") then
                character id ((id of x) + 13)
            else if (x ≥ "n" and x ≤ "z") or (x ≥ "N" and x ≤ "Z") then
                character id ((id of x) - 13)
            else
                x
            end if
        end |λ|
    end script

    intercalate("", map(rt13, characters of str))
end rot13


-- TEST ----------------------------------------------------------------------
on run
    rot13("nowhere ABJURER")

    -->  "abjurer NOWHERE"
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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



## Applesoft BASIC


```ApplesoftBasic
100HOME:INPUT"ENTER A STRING:";S$:FORL=1TOLEN(S$):I$=MID$(S$,L,1):LC=(ASC(I$)>95)*32:C$=CHR$(ASC(I$)-LC):IFC$>="A"ANDC$<="Z"THENC$=CHR$(ASC(C$)+13):C$=CHR$(ASC(C$)-26*(C$>"Z")):I$=CHR$(ASC(C$)+LC)
110A$=A$+I$:NEXT:PRINTA$
```



## AutoHotkey

Simple alphabet remapping method by Raccoon. Similar to a translate() function in many languages.

```AutoHotkey
ROT13(string) ; by Raccoon July-2009
{
  Static a := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ "
  Static b := "nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM "
  s=
  Loop, Parse, string
  {
    c := substr(b,instr(a,A_LoopField,True),1)
    if (c != " ")
      s .= c
    else
      s .= A_LoopField
  }
  Return s
}
```


Simple ASCII math method by Raccoon. Add or subtract 13 depending on the character's decimal value.

```AutoHotkey
ROT13(string) ; by Raccoon July-2009
{
  s=
  Loop, Parse, string
  {
    c := asc(A_LoopField)
    if (c >= 97) && (c <= 109) || (c >= 65) && (c <= 77)
      c += 13
    else if (c >= 110) && (c <= 122) || (c >= 78) && (c <= 90)
      c -= 13
    s .= chr(c)
  }
  Return s
}
```


Code modified from [http://www.autohotkey.com/forum/viewtopic.php?t=8421 stringmod] by [http://www.autohotkey.com/forum/author-HugoV.html Hugo]: [http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=45 ahk discussion]

```AutoHotkey
Str0=Hello, This is a sample text with 1 2 3 or other digits!@#$^&*()-_=
Str1 := Rot13(Str0)
Str2 := Rot13(Str1)
MsgBox % Str0 "`n" Str1 "`n" Str2

Rot13(string)
{
   Loop Parse, string
   {
      char := Asc(A_LoopField)
      ; o is 'A' code if it is an uppercase letter, and 'a' code if it is a lowercase letter
      o := Asc("A") * (Asc("A") <= char && char <= Asc("Z")) + Asc("a") * (Asc("a") <= char && char <= Asc("z"))
      If (o > 0)
      {
         ; Set between 0 and 25, add rotation factor, modulus alphabet size
         char := Mod(char - o + 13, 26)
         ; Transform back to char, upper or lower
         char := Chr(char + o)
      }
      Else
      {
         ; Non alphabetic, unchanged
         char := A_LoopField
      }
      rStr .= char
   }
   Return rStr
}
```


from [http://ahkscript.org/boards/viewtopic.php?f=5&t=2630#p14059 LinearSpoon]'s Translation of [http://hea-www.harvard.edu/~fine/Tech/rot13.html The Worlds Shortest C Implementation of Rot13]


```AutoHotkey
Rot13(string) {
  Output := ""
  Loop, Parse, string
  {
    a := ~Asc(A_LoopField)
    Output .= Chr(~a-1//(~(a|32)//13*2-11)*13)
  }
  return Output
}
```



## AWK

<!--  http://ideone.com/TTZykC -->

```awk
# usage: awk -f rot13.awk
BEGIN {
  for(i=0; i < 256; i++) {
    amap[sprintf("%c", i)] = i
  }
  for(l=amap["a"]; l <= amap["z"]; l++) {
    rot13[l] = sprintf("%c", (((l-amap["a"])+13) % 26 ) + amap["a"])
  }
  FS = ""
}
{
  o = ""
  for(i=1; i <= NF; i++) {
    if ( amap[tolower($i)] in rot13 ) {
      c = rot13[amap[tolower($i)]]
      if ( tolower($i) != $i ) c = toupper(c)
      o = o c
    } else {
      o = o $i
    }
  }
  print o
}
```

 Hello, HAL !
 Uryyb, UNY !


## BaCon


```qbasic
INPUT "String: ", s$
PRINT "Output: ", REPLACE$(s$, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", "nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM", 2)
```

```txt

user@host $ bacon rot13
Converting 'rot13.bac'... done, 2 lines were processed in 0.001 seconds.
Compiling 'rot13.bac'... cc  -c rot13.bac.c
cc -o rot13 rot13.bac.o -lbacon -lm
Done, program 'rot13' ready.
user@host $ ./rot13
String: Oolite quick Thargoid jumps lazy Vipers = blown up + special fx
Output: Bbyvgr dhvpx Gunetbvq whzcf ynml Ivcref = oybja hc + fcrpvny sk

```



## BASIC

```qbasic
CLS
INPUT "Enter a string: ", s$
ans$ = ""
FOR a = 1 TO LEN(s$)
        letter$ = MID$(s$, a, 1)
        IF letter$ >= "A" AND letter$ <= "Z" THEN
                char$ = CHR$(ASC(letter$) + 13)
                IF char$ > "Z" THEN char$ = CHR$(ASC(char$) - 26)
        ELSEIF letter$ >= "a" AND letter$ <= "z" THEN
                char$ = CHR$(ASC(letter$) + 13)
                IF char$ > "z" THEN char$ = CHR$(ASC(char$) - 26)
        ELSE
                char$ = letter$
        END IF
        ans$ = ans$ + char$
NEXT a
PRINT ans$
```



### Alternate version

This version does the rotation in-place without the use of a second variable.


```qbasic
INPUT "Enter a string "; Text$
FOR c% = 1 TO LEN(Text$)
    SELECT CASE ASC(MID$(Text$, c%, 1))
        CASE 65 TO 90
            MID$(Text$, c%, 1) = CHR$(65 + ((ASC(MID$(Text$, c%, 1)) - 65 + 13) MOD 26))
        CASE 97 TO 122
            MID$(Text$, c%, 1) = CHR$(97 + ((ASC(MID$(Text$, c%, 1)) - 97 + 13) MOD 26))
    END SELECT
NEXT c%
PRINT "Converted......: "; Text$
```


 Enter a string ? Oolite quick Thargoid jumps lazy Vipers = blown up + special fx
 Converted......: Bbyvgr dhvpx Gunetbvq whzcf ynml Ivcref = oybja hc + fcrpvny sk

;See also: [[#BBC BASIC|BBC BASIC]], [[#FBSL|FBSL]], [[#GW-BASIC|GW-BASIC]], [[#Liberty BASIC|Liberty BASIC]], [[#Locomotive Basic|Locomotive Basic]], [[#PureBasic|PureBasic]], [[#Run BASIC|Run BASIC]], [[#TI-83 BASIC|TI-83 BASIC]], [[#Visual Basic .NET|Visual Basic .NET]], [[#ZX Spectrum Basic|ZX Spectrum Basic]]


## BASIC256


```basic256

# rot13.bas
# basic256 1.1.4.0

iOffset = 13

input "> ", str$

str$ = upper(str$)          # a bit of a cheat however old school encryption is always upper case
len = length(str$)

for i = 1 to len
    iTemp =  asc(mid(str$,i,1))

    if iTemp > 64 AND iTemp < 91 then
        iTemp = ((iTemp - 65) + iOffset) % 26
        print chr(iTemp + 65);
    else
        print chr(iTemp);
    end if

next i

```

```txt

ARTHUR:  If you will not show us the Grail, we shall take your castle
      by force!
  GUARD:  You don't frighten us, English pig-dogs!  Go and boil your
      bottoms, sons of a silly person.  I blow my nose at you, so-called
      Arthur-king, you and all your silly English kaniggets.  Thppppt!
  GALAHAD:  What a strange person.
  ARTHUR:  Now look here, my good man!
  GUARD:  I don't want to talk to you no more, you empty headed animal
      food trough whopper!  I fart in your general direction!  You mother
      was a hamster and your father smelt of eldeberries.
  GALAHAD:  Is there someone else up there we could talk to?

NEGUHE:  VS LBH JVYY ABG FUBJ HF GUR TENVY, JR FUNYY GNXR LBHE PNFGYR
      OL SBEPR!
  THNEQ:  LBH QBA'G SEVTUGRA HF, RATYVFU CVT-QBTF!  TB NAQ OBVY LBHE
      OBGGBZF, FBAF BS N FVYYL CREFBA.  V OYBJ ZL ABFR NG LBH, FB-PNYYRQ
      NEGUHE-XVAT, LBH NAQ NYY LBHE FVYYL RATYVFU XNAVTTRGF.  GUCCCCG!
  TNYNUNQ:  JUNG N FGENATR CREFBA.
  NEGUHE:  ABJ YBBX URER, ZL TBBQ ZNA!
  THNEQ:  V QBA'G JNAG GB GNYX GB LBH AB ZBER, LBH RZCGL URNQRQ NAVZNY
      SBBQ GEBHTU JUBCCRE!  V SNEG VA LBHE TRARENY QVERPGVBA!  LBH ZBGURE
      JNF N UNZFGRE NAQ LBHE SNGURE FZRYG BS RYQROREEVRF.
  TNYNUNQ:  VF GURER FBZRBAR RYFR HC GURER JR PBHYQ GNYX GB?

```


## BBC BASIC



```bbcbasic
      REPEAT
        INPUT A$
        PRINT FNrot13(A$)
      UNTIL FALSE
      END

      DEF FNrot13(A$)
      LOCAL A%,B$,C$
      IF A$="" THEN =""
      FOR A%=1 TO LEN A$
        C$=MID$(A$,A%,1)
        IF C$<"A" OR (C$>"Z" AND C$<"a") OR C$>"z" THEN
          B$=B$+C$
        ELSE
          IF (ASC(C$) AND &DF)<ASC("N") THEN
            B$=B$+CHR$(ASC(C$)+13)
          ELSE
            B$=B$+CHR$(ASC(C$)-13)
          ENDIF
        ENDIF
      NEXT A%
      =B$

```



## Batch File


```windowsnt

@echo off & setlocal enabledelayedexpansion

:: ROT13 obfuscator Michael Sanders - 2017
::
:: example: rot13.cmd Rire abgvpr cflpuvpf arire jva gur ybggrel?

:setup

   set str=%*
   set buf=%str%
   set len=0

:getlength

   if not defined buf goto :start
   set buf=%buf:~1%
   set /a len+=1
   goto :getlength

:start

   if %len% leq 0 (echo rot13: zero length string & exit /b 1)
   set abc=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
   set nop=NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm
   set r13=
   set num=0
   set /a len-=1

:rot13

   for /l %%x in (!num!,1,%len%) do (
      set log=0
      for /l %%y in (0,1,51) do (
         if "!str:~%%x,1!"=="!abc:~%%y,1!" (
            call set r13=!r13!!nop:~%%y,1!
            set /a num=%%x+1
            set /a log+=1
            if !num! lss %len% goto :rot13
         )
      )
      if !log!==0 call set r13=!r13!!str:~%%x,1!
   )

:done

   echo !r13!
   endlocal & exit /b 0

```



## Befunge


```befunge
~:"z"`#v_:"m"`#v_:"`"` |>
 :"Z"`#v_:"M"`#v_:"@"`|>
 : 0 `#v_@v-6-7<      >
,      <  <+6+7       <<v
```



## Burlesque



```burlesque

blsq ) "HELLO WORLD"{{'A'Zr\\/Fi}m[13?+26.%'A'Zr\\/si}ww
"URYYB JBEYQ"
blsq ) "URYYB JBEYQ"{{'A'Zr\\/Fi}m[13?+26.%'A'Zr\\/si}ww
"HELLO WORLD"

```



## C


The following code can handle all character sets, even if the letters are not in a contiguous range (in ASCII they are, in EBCDIC they aren't).


```c
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

static char rot13_table[UCHAR_MAX + 1];

static void init_rot13_table(void) {
	static const unsigned char upper[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	static const unsigned char lower[] = "abcdefghijklmnopqrstuvwxyz";

	for (int ch = '\0'; ch <= UCHAR_MAX; ch++) {
		rot13_table[ch] = ch;
	}
	for (const unsigned char *p = upper; p[13] != '\0'; p++) {
		rot13_table[p[0]] = p[13];
		rot13_table[p[13]] = p[0];
	}
	for (const unsigned char *p = lower; p[13] != '\0'; p++) {
		rot13_table[p[0]] = p[13];
		rot13_table[p[13]] = p[0];
	}
}

static void rot13_file(FILE *fp)
{
	int ch;
	while ((ch = fgetc(fp)) != EOF) {
		fputc(rot13_table[ch], stdout);
	}
}

int main(int argc, char *argv[])
{
	init_rot13_table();

	if (argc > 1) {
		for (int i = 1; i < argc; i++) {
			FILE *fp = fopen(argv[i], "r");
			if (fp == NULL) {
				perror(argv[i]);
				return EXIT_FAILURE;
			}
			rot13_file(fp);
			fclose(fp);
		}
	} else {
		rot13_file(stdin);
	}
	return EXIT_SUCCESS;
}
```


## C#

```c#
using System;
using System.IO;
using System.Linq;
using System.Text;

class Program
{
    static char Rot13(char c)
    {
        if ('a' <= c && c <= 'm' || 'A' <= c && c <= 'M')
        {
            return (char)(c + 13);
        }
        if ('n' <= c && c <= 'z' || 'N' <= c && c <= 'Z')
        {
            return (char)(c - 13);
        }
        return c;
    }

    static string Rot13(string s)
    {
        return new StringBuilder().Append(s.Select(Rot13).ToArray()).ToString();
    }


    static void Main(string[] args)
    {
        foreach (var file in args.Where(file => File.Exists(file)))
        {
            Console.WriteLine(Rot13(File.ReadAllText(file)));
        }
        if (!args.Any())
        {
            Console.WriteLine(Rot13(Console.In.ReadToEnd()));
        }
    }
}
```



## C++


```cpp
#include <iostream>
#include <istream>
#include <ostream>
#include <fstream>
#include <cstdlib>
#include <string>

// the rot13 function
std::string rot13(std::string s)
{
  static std::string const
    lcalph = "abcdefghijklmnopqrstuvwxyz",
    ucalph = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  std::string result;
  std::string::size_type pos;

  result.reserve(s.length());

  for (std::string::iterator it = s.begin(); it != s.end(); ++it)
  {
    if ( (pos = lcalph.find(*it)) != std::string::npos )
      result.push_back(lcalph[(pos+13) % 26]);
    else if ( (pos = ucalph.find(*it)) != std::string::npos )
      result.push_back(ucalph[(pos+13) % 26]);
    else
      result.push_back(*it);
  }

  return result;
}

// function to output the rot13 of a file on std::cout
// returns false if an error occurred processing the file, true otherwise
// on entry, the argument is must be open for reading
int rot13_stream(std::istream& is)
{
  std::string line;
  while (std::getline(is, line))
  {
    if (!(std::cout << rot13(line) << "\n"))
      return false;
  }
  return is.eof();
}

// the main program
int main(int argc, char* argv[])
{
  if (argc == 1) // no arguments given
    return rot13_stream(std::cin)? EXIT_SUCCESS : EXIT_FAILURE;

  std::ifstream file;
  for (int i = 1; i < argc; ++i)
  {
    file.open(argv[i], std::ios::in);
    if (!file)
    {
      std::cerr << argv[0] << ": could not open for reading: " << argv[i] << "\n";
      return EXIT_FAILURE;
    }
    if (!rot13_stream(file))
    {
      if (file.eof())
        // no error occurred for file, so the error must have been in output
        std::cerr << argv[0] << ": error writing to stdout\n";
      else
        std::cerr << argv[0] << ": error reading from " << argv[i] << "\n";
      return EXIT_FAILURE;
    }
    file.clear();
    file.close();
    if (!file)
      std::cerr << argv[0] << ": warning: closing failed for " << argv[i] << "\n";
  }
  return EXIT_SUCCESS;
}
```


Here is an other approach which can rotate by any number:
```cpp
#include <iostream>
#include <string>
#include <boost/iostreams/concepts.hpp>    // output_filter
#include <boost/iostreams/operations.hpp>  // put
#include <boost/iostreams/filtering_stream.hpp>
#include <fstream>
namespace io = boost::iostreams;

class rot_output_filter : public io::output_filter
{
public:
    explicit rot_output_filter(int r=13):rotby(r),negrot(alphlen-r){};

    template<typename Sink>
    bool put(Sink& dest, int c){
        char uc = toupper(c);

        if(('A' <= uc) && (uc <= ('Z'-rotby)))
            c = c + rotby;
        else if ((('Z'-rotby) <= uc) && (uc <= 'Z'))
            c = c - negrot;
        return boost::iostreams::put(dest, c);
    };
private:
        static const int alphlen = 26;
        const int rotby;
        const int negrot;
};

int main(int argc, char *argv[])
{
    io::filtering_ostream out;
    out.push(rot_output_filter(13));
    out.push(std::cout);

    if (argc == 1) out << std::cin.rdbuf();
    else for(int i = 1; i < argc; ++i){
        std::ifstream in(argv[i]);
        out << in.rdbuf();
    }
}


```



### C++11


```cpp
#include <string>
#include <iostream>
#include <fstream>

char rot13(const char c){
	if (c >= 'a' && c <= 'z')
		return (c - 'a' + 13) % 26 + 'a';
	else if (c >= 'A' && c <= 'Z')
		return (c - 'A' + 13) % 26 + 'A';
	return c;
}

std::string &rot13(std::string &s){
	for (auto &c : s) //range based for is the only used C++11 feature
		c = rot13(c);
	return s;
}

void rot13(std::istream &in, std::ostream &out){
	std::string s;
	while (std::getline(in, s))
		out << rot13(s) << '\n';
}

int main(int argc, char *argv[]){
	if (argc == 1)
		rot13(std::cin, std::cout);
	for (int arg = 1; arg < argc; ++arg){
		std::ifstream f(argv[arg]);
		if (!f)
			return EXIT_FAILURE;
		rot13(f, std::cout);
	}
}
```



## Clojure

All invocations produce "Gur Dhvpx Oebja Sbk Whzcrq Bire Gur Ynml Qbt!"

```clojure
(ns rosettacode.rot-13)

(let [a (int \a) m (int \m) A (int \A) M (int \M)
      n (int \n) z (int \z) N (int \N) Z (int \Z)]
  (defn rot-13 [^Character c]
    (char (let [i (int c)]
      (cond-> i
        (or (<= a i m) (<= A i M)) (+ 13)
        (or (<= n i z) (<= N i Z)) (- 13))))))

(apply str (map rot-13 "The Quick Brown Fox Jumped Over The Lazy Dog!"))

; An alternative implementation using a map:
(let [A (into #{} "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
      Am (->> (cycle A) (drop 26) (take 52) (zipmap A))]
  (defn rot13 [^String in]
    (apply str (map #(Am % %) in))))

(rot13 "The Quick Brown Fox Jumped Over The Lazy Dog!")
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rot-13.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       78  STR-LENGTH   VALUE 100.

       78  normal-lower VALUE "abcdefghijklmnopqrstuvwxyz".
       78  rot13-lower  VALUE "nopqrstuvwxyzabcdefghijklm".

       78  normal-upper VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       78  rot13-upper  VALUE "NOPQRSTUVWXYZABCDEFGHIJKLM".

       LINKAGE SECTION.
       01  in-str       PIC X(STR-LENGTH).
       01  out-str      PIC X(STR-LENGTH).

       PROCEDURE DIVISION USING VALUE in-str, REFERENCE out-str.
           MOVE in-str TO out-str

           INSPECT out-str CONVERTING normal-lower TO rot13-lower
           INSPECT out-str CONVERTING normal-upper TO rot13-upper

           GOBACK
           .
```



## Common Lisp

The standard gives implementations great leeway with respect to character encodings, so we can't rely on the convenient properties of ASCII.

```lisp
(defconstant +alphabet+
'(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
  #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(defun rot13 (s)
  (map 'string
    (lambda (c &aux (n (position (char-upcase c) +alphabet+)))
      (if n
        (funcall
          (if (lower-case-p c) #'char-downcase #'identity)
          (nth (mod (+ 13 n) 26) +alphabet+))
        c))
    s))
```



### =Assuming ASCII Character Set=

Though the standard intentionally doesn't specify encoding, every popular implementation today uses ASCII.

```lisp
(defun rot13 (string)
  (map 'string
       (lambda (char &aux (code (char-code char)))
         (if (alpha-char-p char)
             (if (> (- code (char-code (if (upper-case-p char)
                                           #\A #\a))) 12)
                 (code-char (- code 13))
                 (code-char (+ code 13)))
             char))
       string))

(rot13 "Moron") ; -> "Zbeba"
```



## Cubescript


```cubescript
alias rot13 [
	push alpha [
		"A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
		"a b c d e f g h i j k l m n o p q r s t u v w x y z"
	] [ push chars [] [
		loop i (strlen $arg1) [
			looplist n $alpha [
				if (! (listlen $chars)) [
					alias chars (? (> (listindex $n (substr $arg1 $i 1)) -1) $n [])
				]
			]
			alias arg1 (
				concatword (substr $arg1 0 $i) (
					? (listlen $chars) (
						at $chars (
							mod (+ (
								listindex $chars (substr $arg1 $i 1)
							) 13 ) (listlen $chars)
						)
					) (substr $arg1 $i 1)
				) (substr $arg1 (+ $i 1) (strlen $arg1))
			)
			alias chars []
		]
	] ]
	result $arg1
]
```


Usage:

```cubescript>>>
 rot13 "Hello World"
> Uryyb Jbeyq
>>> rot13 "Gur Dhvpx Oebja Sbk Whzcf Bire Gur Ynml Qbt!"
> The Quick Brown Fox Jumps Over The Lazy Dog!
```



## D


### Using Standard Functions


```d
import std.stdio;
import std.ascii: letters, U = uppercase, L = lowercase;
import std.string: makeTrans, translate;

immutable r13 = makeTrans(letters,
                          //U[13 .. $] ~ U[0 .. 13] ~
                          U[13 .. U.length] ~ U[0 .. 13] ~
                          L[13 .. L.length] ~ L[0 .. 13]);

void main() {
    writeln("This is the 1st test!".translate(r13, null));
}
```

```txt
The Quick Brown Fox Jumps Over The Lazy Dog!
```


### Imperative Implementation


```d
import std.stdio, std.string, std.traits;

pure S rot13(S)(in S s) if (isSomeString!S) {
    return rot(s, 13);
}

pure S rot(S)(in S s, in int key) if (isSomeString!S) {
    auto r = s.dup;

    foreach (i, ref c; r) {
        if ('a' <= c && c <= 'z')
            c = ((c - 'a' + key) % 26 + 'a');
        else if ('A' <= c && c <= 'Z')
            c = ((c - 'A' + key) % 26 + 'A');
    }
    return cast(S) r;
}

void main() {
    "Gur Dhvpx Oebja Sbk Whzcf Bire Gur Ynml Qbt!".rot13().writeln();
}
```


=={{header|Déjà Vu}}==


```dejavu
rot-13:
	)
	for ch in chars swap:
		ord ch
		if <= 65 dup:
			if >= 90 dup:
				+ 13 - swap 65
				+ 65 % swap 26
		if <= 97 dup:
			if >= 122 dup:
				+ 13 - swap 97
				+ 97 % swap 26
		chr
	concat(

!print rot-13 "Snape kills Frodo with Rosebud."
```

```txt
Fancr xvyyf Sebqb jvgu Ebfrohq.
```



## Dyalect



```dyalect
func Char.rot13() {
    if ('a' <= this && this <= 'm' || 'A' <= this && this <= 'M') {
        return Char(this.order() + 13)
    }
    if ('n' <= this && this <= 'z' || 'N' <= this && this <= 'Z') {
        return Char(this.order() - 13);
    }
    return this
}

func String.rot13() {
    var cs = []
    for c in this {
        cs.add(c.rot13())
    }
    String.concat(values: cs)
}

"ABJURER nowhere".rot13()
```


```txt
"NOWHERE abjurer"
```



## E


```e
pragma.enable("accumulator")

var rot13Map := [].asMap()
for a in ['a', 'A'] {
    for i in 0..!26 {
        rot13Map with= (a + i, E.toString(a + (i + 13) % 26))
    }
}

def rot13(s :String) {
  return accum "" for c in s { _ + rot13Map.fetch(c, fn{ c }) }
}
```



## Elena

ELENA 4.x :

```elena
import system'routines;
import extensions;
import extensions'text;

singleton rotConvertor
{
    char convert(char ch)
    {
        if (($97 <= ch && ch <= $109) || ($65 <= ch && ch <= $77))
        {
            ^ (ch.toInt() + 13).toChar()
        };
        if (($110 <= ch && ch <= $122) || ($78 <= ch && ch <= $90))
        {
            ^ (ch.toInt() - 13).toChar()
        };

        ^ ch
    }

    string convert(string s)
        = s.selectBy:(ch => rotConvertor.convert(ch)).summarize(new StringWriter());
}

public program()
{
    if (program_arguments.Length > 1)
    {
        console.printLine(rotConvertor.convert(program_arguments[1]))
    }
}
```

```txt

rot13 "Hello World"
Hryyb Wbeyq

```



## Elixir

```elixir
defmodule RC do
  def rot13(clist) do
    f = fn(c) when (?A <= c and c <= ?M) or (?a <= c and c <= ?m) -> c + 13
          (c) when (?N <= c and c <= ?Z) or (?n <= c and c <= ?z) -> c - 13
          (c) -> c
        end
    Enum.map(clist, f)
  end
end

IO.inspect encode = RC.rot13('Rosetta Code')
IO.inspect RC.rot13(encode)
```


```txt

'Ebfrggn Pbqr'
'Rosetta Code'

```



## Erlang



```erlang
rot13(Str) ->
    F = fun(C) when (C >= $A andalso C =< $M); (C >= $a andalso C =< $m) -> C + 13;
           (C) when (C >= $N andalso C =< $Z); (C >= $n andalso C =< $z) -> C - 13;
           (C) -> C
        end,
    lists:map(F, Str).
```




## ERRE


```ERRE
PROGRAM ROT13

BEGIN
INPUT("Enter a string ",TEXT$)
FOR C%=1 TO LEN(TEXT$) DO
    A%=ASC(MID$(TEXT$,C%,1))
    CASE A% OF
            65..90->
                   MID$(TEXT$,C%,1)=CHR$(65+(A%-65+13) MOD 26)
            END ->
            97..122->
                   MID$(TEXT$,C%,1)=CHR$(97+(A%-97+13) MOD 26)
            END ->
    END CASE
END FOR
PRINT("Converted: ";TEXT$)
END PROGRAM
```

```txt
Enter a string ? pippo
Converted: cvccb

```



## Euphoria

```Euphoria

include std/types.e
include std/text.e

atom FALSE = 0
atom TRUE = not FALSE

function Rot13( object oStuff )
	integer iOffset
	integer bIsUpper
	object oResult
	sequence sAlphabet = "abcdefghijklmnopqrstuvwxyz"
	if sequence(oStuff) then
		oResult = repeat( 0, length( oStuff ) )
		for i = 1 to length( oStuff ) do
			oResult[ i ] = Rot13( oStuff[ i ] )
		end for
	else
		bIsUpper = FALSE
		if t_upper( oStuff ) then
			bIsUpper = TRUE
			oStuff = lower( oStuff )
		end if
		iOffset = find( oStuff, sAlphabet )
		if iOffset != 0 then
			iOffset += 13
			iOffset = remainder( iOffset, 26 )
			if iOffset = 0 then iOffset = 1 end if
			oResult = sAlphabet[iOffset]
			if bIsUpper then
				oResult = upper(oResult)
			end if
		else
			oResult = oStuff --sprintf( "%s", oStuff )
		end if
	end if
	return oResult
end function

puts( 1, Rot13( "abjurer NOWHERE." ) & "\n" )

```


=={{header|F_Sharp|F#}}==
Illustrates turning a string into an array of chars then composition of type casting with a conversion function. We create a composite that converts its input to an integer, calls the convertion function and
then casts to a char type. The result is an array of modified chars that we can use to create a new string.

```fsharp
let rot13 (s : string) =
   let rot c =
       match c with
       | c when c > 64 && c < 91 -> ((c - 65 + 13) % 26) + 65
       | c when c > 96 && c < 123 -> ((c - 97 + 13) % 26) + 97
       | _ -> c
   s |> Array.of_seq
   |> Array.map(int >> rot >> char)
   |> (fun seq -> new string(seq))
```



## Factor


```factor
#! /usr/bin/env factor

USING: kernel io ascii math combinators sequences ;
IN: rot13

: rot-base ( ch ch -- ch ) [ - 13 + 26 mod ] keep + ;

: rot13-ch ( ch -- ch )
    {
        { [ dup letter? ] [ CHAR: a rot-base ] }
        { [ dup LETTER? ] [ CHAR: A rot-base ] }
        [ ]
    }
    cond ;

: rot13 ( str -- str ) [ rot13-ch ] map ;

: main ( -- )
    [ readln dup ]
    [ rot13 print flush ]
    while
    drop ;

MAIN: main
```



## FALSE


```false
[^$1+][$32|$$'z>'a@>|$[\%]?~[13\'m>[_]?+]?,]#%
```



## Fantom



```fantom

class Rot13
{
  static Str rot13 (Str input)
  {
    Str result := ""
    input.each |Int c|
    {
      if ((c.lower >= 'a') && (c.lower <= 'm'))
        result += (c+13).toChar
      else if ((c.lower >= 'n') && (c.lower <= 'z'))
        result += (c-13).toChar
      else
        result += c.toChar
    }
    return result
  }

  public static Void main (Str[] args)
  {
    if (args.size == 1)
    { // process each line of given file
      Str filename := args[0]
      File(filename.toUri).eachLine |Str line|
      {
        echo (rot13(line))
      }
    }
    else
    {
      echo ("Test:")
      Str text := "abcstuABCSTU123!+-"
      echo ("Text $text becomes ${rot13(text)}")
    }
  }
}

```



## FBSL

Implements a circular queue, finds the required character and then rotates the queue forward 13 places. Would do as a solution to Caesar Cipher with a different rotation number. Please note that FBSL is not case sensitive, thus the use of lstrcmp.

```qbasic
#APPTYPE CONSOLE

REM Create a CircularQueue object
REM CQ.Store item
REM CQ.Find items
REM CQ.Forward nItems
REM CQ.Recall

REM SO CQ init WITH "A"... "Z"
REM CQ.Find "B"
REM QC.Forward 13
REM QC.Recall

CLASS CircularQueue
	items[]
	head
	tail
	here

	SUB INITIALIZE(dArray)
		head = 0
		tail = 0
		here = 0
		FOR DIM i = LBOUND(dArray) TO UBOUND(dArray)
			items[tail] = dArray[i]
			tail = tail + 1
		NEXT
	END SUB

	SUB TERMINATE()
		REM
	END SUB

	METHOD Put(s AS STRING)
		items[tail] = s
		tail = tail + 1
	END METHOD

	METHOD Find(s AS STRING)
		FOR DIM i = head TO tail - 1
			IF items[i] = s THEN
				here = i
				RETURN TRUE
			END IF
		NEXT
		RETURN FALSE
	END METHOD

	METHOD Move(n AS INTEGER)
		DIM bound AS INTEGER = UBOUND(items) + 1
		here = (here + n) MOD bound
	END METHOD

	METHOD Recall()
		RETURN items[here]
	END METHOD

	PROPERTY Size()
		RETURN COUNT(items)
	END PROPERTY
END CLASS

DIM CQ AS NEW CircularQueue({"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"})

DIM c AS STRING
DIM isUppercase AS INTEGER
DIM s AS STRING = "nowhere ABJURER"

FOR DIM i = 1 TO LEN(s)
	c = MID(s, i, 1)
	isUppercase = lstrcmp(LCASE(c), c)
	IF CQ.Find(UCASE(c)) THEN
		CQ.Move(13)
		PRINT IIF(isUppercase, UCASE(CQ.Recall()), LCASE(CQ.Recall())) ;
	ELSE
		PRINT c;
	END IF
NEXT

PAUSE

```



## Forth


A simple version, using nested conditionals.

```forth
: r13 ( c -- o )
  dup 32 or                                    \ tolower
  dup [char] a [char] z 1+ within if
    [char] m > if -13 else 13 then +
  else drop then ;
```


A table driven version which should be more efficient. The mechanism is flexible enough to express any sort of transform.

```forth
: ,chars ( end start -- )
  do i c, loop ;

: xlate create does> ( c -- c' ) + c@ ;

xlate rot13
  char A         0    ,chars
  char Z 1+ char N    ,chars
  char N    char A    ,chars
  char a    char Z 1+ ,chars
  char z 1+ char n    ,chars
  char n    char a    ,chars
  256       char z 1+ ,chars

: rot13-string ( addr len -- )
  over + swap do i c@ rot13 i c! loop ;

: .rot13" ( string -- )
  [char] " parse 2dup rot13-string type ;

.rot13" abjurer NOWHERE"   \ nowhere ABJURER
```



## Fortran

```fortran
program test_rot_13

  implicit none
  integer, parameter :: len_max = 256
  integer, parameter :: unit = 10
  character (len_max) :: file
  character (len_max) :: fmt
  character (len_max) :: line
  integer :: arg
  integer :: arg_max
  integer :: iostat

  write (fmt, '(a, i0, a)') '(a', len_max, ')'
  arg_max = iargc ()
  if (arg_max > 0) then
! Encode all files listed on the command line.
    do arg = 1, arg_max
      call getarg (arg, file)
      open (unit, file = file, iostat = iostat)
      if (iostat /= 0) cycle
      do
        read (unit, fmt = fmt, iostat = iostat) line
        if (iostat /= 0) exit
        write (*, '(a)') trim (rot_13 (line))
      end do
      close (unit)
    end do
  else
! Encode standard input.
    do
      read (*, fmt = fmt, iostat = iostat) line
      if (iostat /= 0) exit
      write (*, '(a)') trim (rot_13 (line))
    end do
  end if

contains

  function rot_13 (input) result (output)

    implicit none
    character (len_max), intent (in) :: input
    character (len_max) :: output
    integer :: i

    output = input
    do i = 1, len_trim (output)
      select case (output (i : i))
      case ('A' : 'M', 'a' : 'm')
        output (i : i) = char (ichar (output (i : i)) + 13)
      case ('N' : 'Z', 'n' : 'z')
        output (i : i) = char (ichar (output (i : i)) - 13)
      end select
    end do

  end function rot_13

end program test_rot_13
```

Note: <code>iargc</code> and <code>getarg</code> are common extensions that are implemented by e.g. the Intel Fortran Compiler, G95 and gfortran.

Sample usage:
<lang>> cat foo.txt
foo
> cat bar.txt
bar
> ./rot_13 foo.txt bar.txt
sbb
one
> ./rot_13 < foo.txt
sbb
> cat foo.txt bar.txt | ./rot_13
sbb
one
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' uses in place encoding/decoding
Sub rot13(ByRef s As String)
  If s = "" Then Exit Sub
  Dim code As Integer
  For i As Integer = 0 To Len(s) - 1
    Select Case As Const s[i]
      Case 65 To 90  '' A to Z
        code = s[i] + 13
        If code > 90 Then code -= 26
        s[i] = code
      Case 97 To 122 '' a to z
        code = s[i] + 13
        If code > 122 Then code -= 26
        s[i] = code
     End Select
  Next
End Sub

Dim s As String = "nowhere ABJURER"
Print "Before encoding : "; s
rot13(s)
Print "After encoding  : "; s
rot13(s)
Print "After decoding  : "; s
Print
Print "Press any key to quit"
Sleep
```


```txt

Before encoding : nowhere ABJURER
After encoding  : abjurer NOWHERE
After decoding  : nowhere ABJURER

```



## FunL


```funl
import io.{lines, stdin}

def rot13( s ) =
  buf = StringBuilder()

  for c <- s
    if isalpha( c )
      n = ((ord(c) and 0x1F) - 1 + 13)%26 + 1

      buf.append( chr(n or (if isupper(c) then 64 else 96)) )
    else
      buf.append( c )

  buf.toString()

def rot13lines( ls ) =
  for l <- ls
    println( rot13(l) )

if _name_ == '-main-'
  if args.isEmpty()
    rot13lines( stdin() )
  else
    for f <- args
      rot13lines( lines(f) )
```



## GAP


```gap
rot13 := function(s)
  local upper, lower, c, n, t;
  upper := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  lower := "abcdefghijklmnopqrstuvwxyz";
  t := [ ];
  for c in s do
    n := Position(upper, c);
    if n <> fail then
      Add(t, upper[((n+12) mod 26) + 1]);
    else
      n := Position(lower, c);
      if n <> fail then
        Add(t, lower[((n+12) mod 26) + 1]);
      else
        Add(t, c);
      fi;
    fi;
  od;
  return t;
end;

a := "England expects that every man will do his duty";
# "England expects that every man will do his duty"
b := rot13(a);
# "Ratynaq rkcrpgf gung rirel zna jvyy qb uvf qhgl"
c := rot13(b);
# "England expects that every man will do his duty"
```



## Gema


```gema
/[a-mA-M]/=@int-char{@add{@char-int{$1};13}}
/[n-zN-Z]/=@int-char{@sub{@char-int{$1};13}}
```



## GML



```GML
#define rot13
var in, out, i, working;
in = argument0;
out = "";
for (i = 1; i <= string_length(in); i += 1)
    {
    working = ord(string_char_at(in, i));
    if ((working > 64) && (working < 91))
        {
        working += 13;
        if (working > 90)
            {
            working -= 26;
            }
        }
    else if ((working > 96) && (working < 123))
        {
        working += 13;
        if (working > 122) working -= 26;
        }
    out += chr(working);
    }
return out;
```


The above code is called like this:

```GML
show_message(rot13("My dog has fleas!"));
```


Output (in a message box):
 Zl qbt unf syrnf!


## Go


```go
package main

import (
    "fmt"
    "strings"
)

func rot13char(c rune) rune {
    if c >= 'a' && c <= 'm' || c >= 'A' && c <= 'M' {
        return c + 13
    } else if c >= 'n' && c <= 'z' || c >= 'N' && c <= 'Z' {
        return c - 13
    }
    return c
}

func rot13(s string) string {
    return strings.Map(rot13char, s)
}

func main() {
    fmt.Println(rot13("nowhere ABJURER"))
}
```

Output:

```txt

abjurer NOWHERE

```



## Golo


```golo
#!/usr/bin/env golosh
----
This module encrypts strings by rotating each character by 13.
----
module Rot13

augment java.lang.Character {

  function rot13 = |this| -> match {
    when this >= 'a' and this <= 'z' then charValue((this - 'a' + 13) % 26 + 'a')
    when this >= 'A' and this <= 'Z' then charValue((this - 'A' + 13) % 26 + 'A')
    otherwise this
  }
}

augment java.lang.String {

  function rot13 = |this| -> vector[this: charAt(i): rot13() foreach i in [0..this: length()]]: join("")
}

function main = |args| {

  require('A': rot13() == 'N', "A is not N")
  require("n": rot13() == "a", "n is not a")
  require("nowhere ABJURER": rot13() == "abjurer NOWHERE", "nowhere is not abjurer")

  foreach string in args {
    print(string: rot13())
    print(" ")
  }
  println("")
}
```



## Groovy

Solution:

```groovy
def rot13 = { String s ->
    (s as List).collect { ch ->
        switch (ch) {
            case ('a'..'m') + ('A'..'M'):
                return (((ch as char) + 13) as char)
            case ('n'..'z') + ('N'..'Z'):
                return (((ch as char) - 13) as char)
            default:
                return ch
        }
    }.inject ("") { string, ch -> string += ch}
}
```


Test program:

```groovy
println rot13("Noyr jnf V, 'rer V fnj Ryon.")
```


Output:

```txt
Able was I, 'ere I saw Elba.
```


=={{header|GW-BASIC}}==

```qbasic
10 INPUT "Enter a string: ",A$
20 GOSUB 50
30 PRINT B$
40 END
50 FOR I=1 TO LEN(A$)
60 N=ASC(MID$(A$,I,1))
70 E=255
80 IF N>64 AND N<91 THEN E=90   ' uppercase
90 IF N>96 AND N<123 THEN E=122 ' lowercase
100 IF E<255 THEN N=N+13
110 IF N>E THEN N=N-26
120 B$=B$+CHR$(N)
130 NEXT
140 RETURN
```



## Haskell

Straightforward implementation by checking multiple cases:


```haskell
import Data.Char (chr, isAlpha, ord, toLower)
import Data.Bool (bool)

rot13 :: Char -> Char
rot13 c
  | isAlpha c = chr $ bool (-) (+) ('m' >= toLower c) (ord c) 13
  | otherwise = c

-- Simple test
main :: IO ()
main = print $ rot13 <$> "Abjurer nowhere"
```


Or in point-free applicative terms:

```haskell
import Data.Char (chr, isAlpha, ord, toLower)
import Data.Bool (bool)

rot13 :: Char -> Char
rot13 =
  let rot = flip ((bool (-) (+) . ('m' >=) . toLower) <*> ord)
  in (bool <*> chr . rot 13) <*> isAlpha

-- Simple test
main :: IO ()
main = print $ rot13 <$> "Abjurer nowhere"
```

```txt
"Nowhere abjurer"
```


To wrap rot13 as a utility program, here's a quick implementation of a general framework:


```haskell
import System.Environment
import System.IO
import System.Directory
import Control.Monad

hInteract :: (String -> String) -> Handle -> Handle -> IO ()
hInteract f hIn hOut =
  hGetContents hIn >>= hPutStr hOut . f

processByTemp :: (Handle -> Handle -> IO ()) -> String -> IO ()
processByTemp f name = do
  hIn <- openFile name ReadMode
  let tmp = name ++ "$"
  hOut <- openFile tmp WriteMode
  f hIn hOut
  hClose hIn
  hClose hOut
  removeFile name
  renameFile tmp name

process :: (Handle -> Handle -> IO ()) -> [String] -> IO ()
process f [] = f stdin stdout
process f ns = mapM_ (processByTemp f) ns
```


Then the wrapped program is simply

```haskell
main = do
 names <- getArgs
 process (hInteract (map rot13)) names
```


Note that the framework will read the file lazily, which also provides buffering.


## HicEst


```hicest
CHARACTER c, txt='abc? XYZ!', cod*100

  DO i = 1, LEN_TRIM(txt)
    c = txt(i)
    n = ICHAR(txt(i))
    IF( (c >= 'a') * (c <= 'm') + (c >= 'A') * (c <= 'M') ) THEN
        c = CHAR( ICHAR(c) + 13 )
    ELSEIF( (c >= 'n') * (c <= 'z') + (c >= 'N') * (c <= 'Z') ) THEN
        c = CHAR( ICHAR(c) - 13 )
    ENDIF

    cod(i) = c
  ENDDO

  WRITE(ClipBoard, Name) txt, cod ! txt=abc? XYZ!; cod=nop? KLM!;
END
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
   file := open(arglist[1],"r") | &input
   every write(rot13(|read(file)))
end

procedure rot13(s)   #: returns rot13(string)
static a,n
initial {
   a := &lcase || &ucase
   (&lcase || &lcase) ? n := ( move(13), move(*&lcase) )
   (&ucase || &ucase) ? n ||:= ( move(13), move(*&ucase) )
   }
return map(s,a,n)
end
```

This example uses a number of Icon features.
* alternation ( x | y ) selects and opens a file if supplied or fall back to standard output
* repeated alternation ( |x ) is used to generate the contents of the input file
* the rot13 procedure does a one time setup (initially) of persistent (static) mapping strings so the procedure can return the rot13 mapping
* the setup exploits the ordered cset variables &lcase and &ucase coercing them into strings
* the rot13 mapping string is then aggregated with strings taken by offsetting into double length values to avoid unnecessary and messy rotation


## J


```j
rot13=: {&((65 97+/~i.2 13) |.@[} i.256)&.(a.&i.)
```


For example:

    rot13 'abc! ABC!'
 nop! NOP!

Compare with the solution to the [[Change_string_case#J|Change String Case]] task.


## Java


```java
import java.io.*;

public class Rot13 {

    public static void main(String[] args) throws IOException {
        if (args.length >= 1) {
            for (String file : args) {
                try (InputStream in = new BufferedInputStream(new FileInputStream(file))) {
                    rot13(in, System.out);
                }
            }
        } else {
            rot13(System.in, System.out);
        }
    }

    private static void rot13(InputStream in, OutputStream out) throws IOException {
        int ch;
        while ((ch = in.read()) != -1) {
            out.write(rot13((char) ch));
        }
    }

    private static char rot13(char ch) {
        if (ch >= 'A' && ch <= 'Z') {
            return (char) (((ch - 'A') + 13) % 26 + 'A');
        }
        if (ch >= 'a' && ch <= 'z') {
            return (char) (((ch - 'a') + 13) % 26 + 'a');
        }
        return ch;
    }
}
```



## JavaScript


```javascript
function rot13(c) {
    return c.replace(/([a-m])|([n-z])/ig, function($0,$1,$2) {
        return String.fromCharCode($1 ? $1.charCodeAt(0) + 13 : $2 ? $2.charCodeAt(0) - 13 : 0) || $0;
    });
}
rot13("ABJURER nowhere") // NOWHERE abjurer

```


TDD with Jasmine using Underscore.js


```javascript

function rot13(value){
  if (!value)
    return "";

  function singleChar(c) {
    if (c.toUpperCase() < "A" || c.toUpperCase() > "Z")
      return c;

    if (c.toUpperCase() <= "M")
      return String.fromCharCode(c.charCodeAt(0) + 13);

    return String.fromCharCode(c.charCodeAt(0) - 13);
  }

  return _.map(value.split(""), singleChar).join("");
}

describe("Rot-13", function() {
  it("Given nothing will return nothing", function() {
    expect(rot13()).toBe("");
  });

  it("Given empty string will return empty string", function() {
    expect(rot13("")).toBe("");
  });

  it("Given A will return N", function() {
    expect(rot13("A")).toBe("N");
  });

  it("Given B will return O", function() {
    expect(rot13("B")).toBe("O");
  });

  it("Given N will return A", function() {
    expect(rot13("N")).toBe("A");
  });

  it("Given Z will return M", function() {
    expect(rot13("Z")).toBe("M");
  });

  it("Given ZA will return MN", function() {
    expect(rot13("ZA")).toBe("MN");
  });

  it("Given HELLO will return URYYB", function() {
    expect(rot13("HELLO")).toBe("URYYB");
  });

  it("Given hello will return uryyb", function() {
    expect(rot13("hello")).toBe("uryyb");
  });


  it("Given hello1 will return uryyb1", function() {
    expect(rot13("hello1")).toBe("uryyb1");
  });
});

```



## jq


### =jq on the shebang line=


```sh
#!/usr/bin/env jq -M -R -r -f
# or perhaps:
#!/usr/local/bin/jq -M -R -r -f

# If your operating system does not allow more than one option
# to be specified on the command line,
# then consider using a version of jq that allows
# command-line options to be squished together (-MRrf),
# or see the following subsection.

def rot13:
  explode
 | map( if 65 <= . and . <= 90 then ((. - 52) % 26) + 65
        elif 97 <= . and . <= 122 then (. - 84) % 26 + 97
        else .
   end)
 | implode;

rot13
```


### =bash on the shebang line=


```sh
#!/bin/bash

jq -M -R -r '

def rot13:
  explode
 | map( if 65 <= . and . <= 90 then ((. - 52) % 26) + 65
        elif 97 <= . and . <= 122 then (. - 84) % 26 + 97
        else .
   end)
 | implode;

rot13'
```

'''Example''':
 $ echo abc123ABC | ./rot13
 nop123NOP


## Jsish

rot13 function borrowed from Javascript entry, and modified to take into account typed parameters to functions in Jsish.

Can be used as a require module or a command line utility, and includes unit testing.

```javascript
#!/usr/local/bin/jsish
/* ROT-13 in Jsish */
function rot13(msg:string) {
    return msg.replace(/([a-m])|([n-z])/ig, function(m,p1,p2,ofs,str) {
        return String.fromCharCode(
            p1 ? p1.charCodeAt(0) + 13 : p2 ? p2.charCodeAt(0) - 13 : 0) || m;
    });
}
provide('rot13', Util.verConvert("1.0"));

/* rot13 command line utility */
if (isMain()) {
    /* Unit testing */
    if (Interp.conf('unitTest') > 0) {
;       rot13('ABJURER nowhere 123!');
;       rot13(rot13('Same old same old'));
        return;
    }

    /* rot-13 of data lines from given filenames or stdin, to stdout */
    function processFile(fname:string) {
        var str;
        if (fname == "stdin") fname = "./stdin";
        if (fname == "-") fname = "stdin";
        var fin = new Channel(fname, 'r');
        while (str = fin.gets()) puts(rot13(str));
        fin.close();
    }

    if (console.args.length == 0) console.args.push('-');
    for (var fn of console.args) {
        try { processFile(fn); } catch(err) { puts(err, "processing", fn); }
    }
}

/*
=!EXPECTSTART!=
rot13('ABJURER nowhere 123!') ==> NOWHERE abjurer 123!
rot13(rot13('Same old same old')) ==> Same old same old
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -e 'require("rot13"); puts(rot13("abcxyz"));'
nopklm

prompt$ jsish -u rot13.jsi
[PASS] rot13.jsi

prompt$ jsish rot13.jsi
this is a stdin filter test
guvf vf n fgqva svygre grfg

prompt$ ./rot13.jsi rot13.jsi | head -4
#!/hfe/ybpny/ova/wfvfu

/* EBG-13 va Wfvfu */
shapgvba ebg13(zft:fgevat) {

prompt$ ./rot13.jsi rot13.jsi | head -4 | ./rot13.jsi
#!/usr/local/bin/jsish

/* ROT-13 in Jsish */
function rot13(msg:string) {
```



## Julia


```Julia

# Julia 1.0
function rot13(c::Char)
    shft = islowercase(c) ? 'a' : 'A'
    isletter(c) ? c = shft + (c - shft + 13) % 26 : c
end

rot13(str::AbstractString) = map(rot13, str)
```


```txt
julia> rot13("abcdefghijklmnopqrtuvwxyz 123 ABCDEFGHIJKLMNOPQRTUVWXYZ")
"nopqrstuvwxyzabcdeghijklm 123 NOPQRSTUVWXYZABCDEGHIJKLM"

```



### Alternative version


```lua
replace("nowhere ABJURER", r"[A-Za-z]" => s -> map(c -> c + (uppercase(c) < 'N' ? 13 : -13), s))
```

```txt
abjurer NOWHERE
```



## K


```k
  rot13: {a:+65 97+\:2 13#!26;_ci@[!256;a;:;|a]_ic x}

  rot13 "Testing! 1 2 3"
"Grfgvat! 1 2 3"
```



## Kotlin


```scala
import java.io.*

fun String.rot13() = map {
    when {
        it.isUpperCase() -> { val x = it + 13; if (x > 'Z') x - 26 else x }
        it.isLowerCase() -> { val x = it + 13; if (x > 'z') x - 26 else x }
        else -> it
    } }.toCharArray()

fun InputStreamReader.println() =
        try { BufferedReader(this).forEachLine { println(it.rot13()) } }
        catch (e: IOException) { e.printStackTrace() }

fun main(args: Array<String>) {
    if (args.any())
        args.forEach { FileReader(it).println() }
    else
        InputStreamReader(System.`in`).println()
}
```



## LabVIEW

## Lasso



```Lasso
// Extend the string type

define string->rot13 => {
    local(
        rot13 = bytes,
        i, a, b
    )

    with char in .eachCharacter
    let int = #char->integer
    do {
        // We only modify these ranges, set range if we should modify
        #int >= 65 and #int < 91  ? local(a=65,b=91)  |
        #int >= 97 and #int < 123 ? local(a=97,b=123) | local(a=0,b=0)

        if(#a && #b) => {
            #i = (#int+13) % #b         // loop back if past ceiling (#b)
            #i += #a * (1 - #i / #a)    // offset if below floor (#a)
            #rot13->import8bits(#i)     // import the new character
        else
            #rot13->append(#char)       // just append the character
        }
    }

    return #rot13->asstring
}
```


;Example:

```txt
'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'->rot13

'Where do you find a dog with no legs?
Evtug jurer lbh yrsg uvz.'->rot13
```


```txt
NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm

Jurer qb lbh svaq n qbt jvgu ab yrtf?
Right where you left him.
```


;Another implementation:


```Lasso
define rot13(p::string) => {
    local(
        rot13 = bytes,
        a = bytes('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'),
        b = bytes('NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm'),
        i
    )

    with char in #p->eachCharacter
    let c = bytes(#char) do {
        #i = #a->find(#b)
        #i ? #rot13->import8bits(#b->get(#i)) | #rot13->append(#c)
    }

    return #rot13->asString
}

rot13('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz')
```


```txt
NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm
```



## Liberty BASIC

Liberty BASIC string comparisons are not ascii-based.
Verbose version:

```lb
input "Type some text to be encoded, then ENTER. ";tx$

tex$ = Rot13$(tx$)
print tex$
'check
print Rot13$(tex$)

wait

Function Rot13$(t$)
    if t$="" then
        Rot13$=""
        exit function
    end if
    for i = 1 to len(t$)
        c$=mid$(t$,i,1)
        ch$=c$
        if (asc(c$)>=asc("A")) and (asc(c$)<=asc("Z")) then
            ch$=chr$(asc(c$)+13)
            if (asc(ch$)>asc("Z")) then ch$=chr$(asc(ch$)-26)
        end if
        if (asc(c$)>=asc("a")) and (asc(c$)<=asc("z")) then
            ch$=chr$(asc(c$)+13)
            if (asc(ch$)>asc("z")) then ch$=chr$(asc(ch$)-26)
        end if
        rot$=rot$+ch$
    next
    Rot13$=rot$
    end function

```


Concise:

```lb
Function Rot13$(t$)
    for i = 1 to len(t$)
        ch$=mid$(t$,i,1)
        if (asc(ch$)>=asc("A")) and (asc(ch$)<=asc("Z")) then
            ch$=chr$(asc("A")+ (asc(ch$)-asc("A")+13) mod 26)
        end if
        if (asc(ch$)>=asc("a")) and (asc(ch$)<=asc("z")) then
            ch$=chr$(asc("a")+ (asc(ch$)-asc("a")+13) mod 26)
        end if
        Rot13$=Rot13$+ch$
    next
    end function

```



## Limbo


A fairly straightforward version that uses a lookup table, based on Inferno's cat(1).


```Limbo
implement Rot13;

include "sys.m"; sys: Sys;
include "draw.m";

Rot13: module
{
	init:	fn(ctxt: ref Draw->Context, argv: list of string);
};

stdout: ref Sys->FD;
tab: array of int;

init(nil: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	stdout = sys->fildes(1);
	inittab();
	args = tl args;
	if(args == nil)
		args = "-" :: nil;
	for(; args != nil; args = tl args){
		file := hd args;
		if(file != "-"){
			fd := sys->open(file, Sys->OREAD);
			if(fd == nil){
				sys->fprint(sys->fildes(2), "rot13: cannot open %s: %r\n", file);
				raise "fail:bad open";
			}
			rot13cat(fd, file);
		}else
			rot13cat(sys->fildes(0), "<stdin>");
	}
}

inittab()
{
	tab = array[256] of int;
	for(i := 0; i < 256; i++)
		tab[i] = i;

	for(i = 'a'; i <= 'z'; i++)
		tab[i] = (((i - 'a') + 13) % 26) + 'a';
	for(i = 'A'; i <= 'Z'; i++)
		tab[i] = (((i - 'A') + 13) % 26) + 'A';
}


rot13(s: string): string
{
	for(i := 0; i < len s; i++) {
		if(s[i] < 256)
			s[i] = tab[s[i]];
	}
	return s;
}

rot13cat(fd: ref Sys->FD, file: string)
{
	buf := array[Sys->ATOMICIO] of byte;

	while((n := sys->read(fd, buf, len buf)) > 0) {
		obuf := array of byte (rot13(string buf));
		if(sys->write(stdout, obuf, n) < n) {
			sys->fprint(sys->fildes(2), "rot13: write error: %r\n");
			raise "fail:write error";
		}
	}
	if(n < 0) {
		sys->fprint(sys->fildes(2), "rot13: error reading %s: %r\n", file);
		raise "fail:read error";
	}
}

```



## LiveCode


```LiveCode
function rot13 S
   repeat with i = 1 to length(S)
      get chartonum(char i of S)
      if it < 65 or it > 122 or (it > 90 and it < 97) then next repeat
      put char it - 64 of "NOPQRSTUVWXYZABCDEFGHIJKLM      nopqrstuvwxyzabcdefghijklm" into char i of S
   end repeat
   return S
end rot13
```




## Locomotive Basic



```locobasic
10 INPUT "Enter a string: ",a$
20 GOSUB 50
30 PRINT b$
40 END
50 FOR i=1 TO LEN(a$)
60 n=ASC(MID$(a$,i,1))
70 e=255
80 IF n>64 AND n<91 THEN e=90   ' uppercase
90 IF n>96 AND n<123 THEN e=122 ' lowercase
100 IF e<255 THEN n=n+13
110 IF n>e THEN n=n-26
120 b$=b$+CHR$(n)
130 NEXT
140 RETURN
```



## Logo


```logo
to rot13 :c
  make "a difference ascii lowercase :c  ascii "a
  if or :a < 0 :a > 25 [output :c]
  make "delta ifelse :a < 13 [13] [-13]
  output char sum :delta ascii :c
end

print map "rot13 "|abjurer NOWHERE|
nowhere ABJURER
```


## Lua


```lua
function rot13(s)
	local a = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
	local b = "NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm"
	return (s:gsub("%a", function(c) return b:sub(a:find(c)) end))
end
```



### Alternative version


```lua
function rot13(s)
  return (s:gsub("%a", function(c) c=c:byte() return string.char(c+(c%32<14 and 13 or -13)) end))
end
```



## Maple

There is a built-in command for this in Maple.

```Maple>
 StringTools:-Encode( "The Quick Brown Fox Jumped Over The Lazy Dog!", encoding = rot13 );
            "Gur Dhvpx Oebja Sbk Whzcrq Bire Gur Ynml Qbt!"
```



## Mathematica


```Mathematica
ruleslower=Thread[#-> RotateLeft[#, 13]]&[CharacterRange["a", "z"]];
rulesupper=Thread[#-> RotateLeft[#, 13]]&[CharacterRange["A", "Z"]];
rules=Join[ruleslower,rulesupper];
text="Hello World! Are you there!?"
text=StringReplace[text,rules]
text=StringReplace[text,rules]
```

gives back:

```txt
Hello World! Are you there!?
Uryyb Jbeyq! Ner lbh gurer!?
Hello World! Are you there!?
```



## MATLAB


```matlab
function r=rot13(s)
    if ischar(s)
        r=s;  % preallocation and copy of non-letters
        for i=1:size(s,1)
            for j=1:size(s,2)
                if isletter(s(i,j))
                    if s(i,j)>=97   % lower case
                        base = 97;
                    else            % upper case
                        base = 65;
                    end
                    r(i,j)=char(mod(s(i,j)-base+13,26)+base);
                end
            end
        end
    else
        error('Argument must be a CHAR')
    end
end
```

Call it like this:

```matlab>>
 rot13('Hello World!')

ans =

Uryyb Jbeyq!
```


It is possible to vectorize this code, the example below is not fully vectorized in order to make the order of operations clear. It is possible to reduce this solution to two lines by integrating the "selectedLetters" calculations directly into the line following them.


```MATLAB
function text = rot13(text)
    if ischar(text)

        selectedLetters = ( (text >= 'A') & (text <= 'Z') ); %Select upper case letters
        text(selectedLetters) = char( mod( text(selectedLetters)-'A'+13,26 )+'A' );

        selectedLetters = ( (text >= 'a') & (text <= 'z') ); %Select lower case letters
        text(selectedLetters) = char( mod( text(selectedLetters)-'a'+13,26 )+'a' );

    else
        error('Argument must be a string.')
    end
end
```


Sample Output:

```MATLAB>>
 plainText = char((64:123))

plainText =

@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{

>> rot13(plainText)

ans =

@NOPQRSTUVWXYZABCDEFGHIJKLM[\]^_`nopqrstuvwxyzabcdefghijklm{
```



## Maxima


```maxima
rot13(a) := simplode(map(ascii, map(lambda([n],
      if (n >= 65 and n <= 77) or (n >= 97 and n <= 109) then n + 13
      elseif (n >= 78 and n <= 90) or (n >= 110 and n <= 122) then n - 13
      else n), map(cint, sexplode(a)))))$

lowercase: "abcdefghijklmnopqrstuvwxyz"$
uppercase: "ABCDEFGHIJKLMNOPQRSTUVWXYZ"$

rot13(lowercase);
"nopqrstuvwxyzabcdefghijklm"

rot13(uppercase);
"NOPQRSTUVWXYZABCDEFGHIJKLM"

rot13("The quick brown fox jumps over the lazy dog");
"Gur dhvpx oebja sbk whzcf bire gur ynml qbt"

rot13(%);
"The quick brown fox jumps over the lazy dog"
```



## MiniScript


```MiniScript
rot13 = function(s)
    chars = s.values
    for i in chars.indexes
        c = chars[i]
        if c >= "a" and c <= "z" then chars[i] = char(97 + (code(c)-97+13)%26)
        if c >= "A" and c <= "Z" then chars[i] = char(65 + (code(c)-65+13)%26)
    end for
    return chars.join("")
end function

print rot13("Hello world!")
print rot13("Uryyb jbeyq!")
```


```txt
Uryyb jbeyq!
Hello world!
```



## Mirah


```mirah
def rot13 (value:string)
    result = ""
    d = ' '.toCharArray[0]
    value.toCharArray.each do |c|
        testChar = Character.toLowerCase(c)
        if testChar <= 'm'.toCharArray[0] && testChar >= 'a'.toCharArray[0] then
            d = char(c + 13)
        end
        if testChar <= 'z'.toCharArray[0] && testChar >= 'n'.toCharArray[0] then
            d = char(c - 13)
        end
        result += d
    end
    result
end


puts rot13("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
```



## ML

=
## Standard ML
=

```sml
fun rot13char c =
  if c >= #"a" andalso c <= #"m" orelse c >= #"A" andalso c <= #"M" then
    chr (ord c + 13)
  else if c >= #"n" andalso c <= #"z" orelse c >= #"N" andalso c <= #"Z" then
    chr (ord c - 13)
  else
    c

val rot13 = String.map rot13char
```

=
## mLite
=
Reads text file from stdin to list of lines, encodes and prints lines of encoding to stdout.

```ocaml
fun readfile () = readfile []
           | x = let val ln = readln ()
                 in if eof ln then
                      rev x
                    else
                      readfile ` ln :: x
                 end

local
	val lower_z = ord #"z";
	val upper_z = ord #"Z";
	val lower_a = ord #"a";
	val upper_a = ord #"A";

	fun which
			(c_upper c) = (upper_a, upper_z)
		|	_	    = (lower_a, lower_z)
		;

	fun scale
			(c, az) where (c > #1 az) = scale( (#0 az + (c - #1 az - 1)), az)
		|	(c, az) 		  = c

in
	fun rot13
			([], t) 			    = implode ` rev t
		|	(x :: xs, t) where (c_alphabetic x) = rot13 (xs, (chr ` scale (ord x + 13, which x)) :: t)
		|	(x :: xs, t)                        = rot13 (xs, x :: t)
		|	s 				    = rot13 (explode s, [])
end;

map (println o rot13) ` readfile ();

```



## MMIX


```mmix
// main registers
p	IS	$255	% text pointer
c	GREG		% char
cc	GREG		% uppercase copy of c
u	GREG		% all purpose

	LOC	Data_Segment
	GREG	@
Test	BYTE	"dit is een bericht voor de keizer",#a,0

	LOC	#100
Main	LDA	p,Test
	TRAP	0,Fputs,StdOut	% show text to encrypt
	LDA	p,Test		% points to text to encrypt
	JMP	4F
// do in place text encryption
				% REPEAT
2H	ADD	cc,c,0		%  copy char
	SUB	cc,cc,' '	%  make uppercase
	CMP	u,cc,'A'
	BN	u,3F		%  IF c < 'A' OR c > 'Z' THEN next char
	CMP	u,cc,'Z'
	BP	u,3F
	CMP	u,cc,'N'	%  ELSE
	BN	u,1F		%    IF c < 'N' THEN encrypt 'up'
	SUB	c,c,26		%    ELSE char ready for encrypt 'down'
1H	INCL	c,13		%  encrypt char
	STBU	c,p		%  replace char with encrypted char
3H	INCL	p,1		%  move to next char
4H	LDBU	c,p		%  get next char
	PBNZ	c,2B		% UNTIL EOT
// print result
	LDA	p,Test
	TRAP	0,Fputs,StdOut	% show encrypted text
	TRAP	0,Halt,0
```

Example:

```txt
~/MIX/MMIX/Progs> mmix rot13simpl
dit is een bericht voor de keizer
qvg vf rra orevpug ibbe qr xrvmre
```


=={{header|Modula-3}}==
This implementation reads from '''stdin''' and writes to '''stdout'''.

```modula3
MODULE Rot13 EXPORTS Main;

IMPORT Stdio, Rd, Wr;

VAR c: CHAR;

<*FATAL ANY*>

BEGIN
  WHILE NOT Rd.EOF(Stdio.stdin) DO
    c := Rd.GetChar(Stdio.stdin);
    IF c >= 'A' AND c <= 'M' OR c >= 'a' AND c <= 'm' THEN
      c := VAL(ORD((ORD(c) + 13)), CHAR);
    ELSIF c >= 'N' AND c <= 'Z' OR c >= 'n' AND c <= 'z' THEN
      c := VAL(ORD((ORD(c) - 13)), CHAR);
    END;
    Wr.PutChar(Stdio.stdout, c);
  END;
END Rot13.
```


Output:

```txt

martin@thinkpad:~$ ./prog
Foo bar baz
Sbb one onm
martin@thinkpad:~$ echo "Bar baz foo" | ./prog
One onm sbb
martin@thinkpad:~$ echo "Foo bar baz" > foo.txt
martin@thinkpad:~$ echo "quux zeepf" >> foo.txt
martin@thinkpad:~$ cat foo.txt | ./prog
Sbb one onm
dhhk mrrcs

```



## MUMPS


```MUMPS
Rot13(in)	New low,rot,up
	Set up="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	Set low="abcdefghijklmnopqrstuvwxyz"
	Set rot=$Extract(up,14,26)_$Extract(up,1,13)
	Set rot=rot_$Extract(low,14,26)_$Extract(low,1,13)
	Quit $Translate(in,up_low,rot)

Write $$Rot13("Hello World!") ; Uryyb Jbeyq!
Write $$Rot13("ABCDEFGHIJKLMNOPQRSTUVWXYZ") ; NOPQRSTUVWXYZABCDEFGHIJKLM
```



## Neko

rotate13 function based on ALGOL-68 entry.

```ActionScript
/*
 ROT-13 in Neko
*/

/* Assume ASCII encoding */
var rotate13 = function(c) {
    if (c >= 65 && c <= 77) || (c >= 97 && c <= 109)
        c += 13
    else
        if (c >= 78 && c <= 90) || (c >= 110 && c <= 122)
            c -= 13
    return c
}

var rot13 = function(s) {
    var r = $scopy(s)
    var len = $ssize(r)
    var pos = 0
    while pos < len {
        $sset(r, pos, rotate13($sget(r, pos)))
        pos += 1
    }
    return r
}

var msg = $loader.args[0]
if msg == null {
    var testing = "[abcdefghijklmnopqrstuvwxyz 0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ]"
    $print(testing, "\n", replaced = rot13(testing), "\n")
    $print(rot13(replaced), "\n")
    $print("\n")
    msg = "The secret message said \"Open Sesame!\""
}
$print(msg, "\n", replaced = rot13(msg), "\n")
$print(rot13(replaced), "\n")
```


```txt
prompt$ nekoc rot-13.neko
prompt$ neko rot-13
[abcdefghijklmnopqrstuvwxyz 0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ]
[nopqrstuvwxyzabcdefghijklm 0123456789 NOPQRSTUVWXYZABCDEFGHIJKLM]
[abcdefghijklmnopqrstuvwxyz 0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ]

The secret message said "Open Sesame!"
Gur frperg zrffntr fnvq "Bcra Frfnzr!"
The secret message said "Open Sesame!"

prompt$ neko rot-13 'nowhere ABJURER'
nowhere ABJURER
abjurer NOWHERE
nowhere ABJURER
```



## NetRexx

This sample leverages the code demonstrated in the [[Caesar cipher#NetRexx|Caesar cipher &ndash; NetRexx]] task.

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

parse arg fileNames

rdr = BufferedReader

do
  if fileNames.length > 0 then do
    loop n_ = 1 for fileNames.words
      fileName = fileNames.word(n_)
      rdr = BufferedReader(FileReader(File(fileName)))
      encipher(rdr)
      end n_
    end
  else do
    rdr = BufferedReader(InputStreamReader(System.in))
    encipher(rdr)
    end
catch ex = IOException
  ex.printStackTrace
end

return

method encipher(rdr = BufferedReader) public static signals IOException

  loop label l_ forever
    line = rdr.readLine
    if line = null then leave l_
    say rot13(line)
    end l_
  return

method rot13(input) public static signals IllegalArgumentException

  return caesar(input, 13, isFalse)

method caesar(input = Rexx, idx = int, caps = boolean) public static signals IllegalArgumentException

  if idx < 1 | idx > 25 then signal IllegalArgumentException()

  --      12345678901234567890123456
  itab = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  shift = itab.length - idx
  parse itab tl +(shift) tr
  otab = tr || tl

  if caps then input = input.upper

  cipher = input.translate(itab || itab.lower, otab || otab.lower)

  return cipher

method caesar_encipher(input = Rexx, idx = int, caps = boolean) public static signals IllegalArgumentException

  return caesar(input, idx, caps)

method caesar_decipher(input = Rexx, idx = int, caps = boolean) public static signals IllegalArgumentException

  return caesar(input, int(26) - idx, isFalse)

method caesar_encipher(input = Rexx, idx = int) public static signals IllegalArgumentException

  return caesar(input, idx, isFalse)

method caesar_decipher(input = Rexx, idx = int) public static signals IllegalArgumentException

  return caesar(input, int(26) - idx, isFalse)

method caesar_encipher(input = Rexx, idx = int, opt = Rexx) public static signals IllegalArgumentException

  return caesar(input, idx, opt)

method caesar_decipher(input = Rexx, idx = int, opt = Rexx) public static signals IllegalArgumentException

  return caesar(input, int(26) - idx, opt)

method caesar(input = Rexx, idx = int, opt = Rexx) public static signals IllegalArgumentException

  if opt.upper.abbrev('U') >= 1 then caps = isTrue
  else                               caps = isFalse

  return caesar(input, idx, caps)

method caesar(input = Rexx, idx = int) public static signals IllegalArgumentException

  return caesar(input, idx, isFalse)

method isTrue public static returns boolean
  return (1 == 1)

method isFalse public static returns boolean
  return \isTrue

```


'''Output''' ''(using the source file as input)'':
<pre style="height: 60ex; overflow: scroll;">
/* ArgErkk */
bcgvbaf ercynpr sbezng pbzzragf wnin pebffers fnirybt flzobyf abovanel

cnefr net svyrAnzrf

eqe = OhssrerqErnqre

qb
  vs svyrAnzrf.yratgu > 0 gura qb
    ybbc a_ = 1 sbe svyrAnzrf.jbeqf
      svyrAnzr = svyrAnzrf.jbeq(a_)
      eqe = OhssrerqErnqre(SvyrErnqre(Svyr(svyrAnzr)))
      rapvcure(eqe)
      raq a_
    raq
  ryfr qb
    eqe = OhssrerqErnqre(VachgFgernzErnqre(Flfgrz.va))
    rapvcure(eqe)
    raq
pngpu rk = VBRkprcgvba
  rk.cevagFgnpxGenpr
raq

erghea

zrgubq rapvcure(eqe = OhssrerqErnqre) choyvp fgngvp fvtanyf VBRkprcgvba

  ybbc ynory y_ sberire
    yvar = eqe.ernqYvar
    vs yvar = ahyy gura yrnir y_
    fnl ebg13(yvar)
    raq y_
  erghea

zrgubq ebg13(vachg) choyvp fgngvp fvtanyf VyyrtnyNethzragRkprcgvba

  erghea pnrfne(vachg, 13, vfSnyfr)

zrgubq pnrfne(vachg = Erkk, vqk = vag, pncf = obbyrna) choyvp fgngvp fvtanyf VyyrtnyNethzragRkprcgvba

  vs vqk < 1 | vqk > 25 gura fvtany VyyrtnyNethzragRkprcgvba()

  --      12345678901234567890123456
  vgno = 'NOPQRSTUVWXYZABCDEFGHIJKLM'
  fuvsg = vgno.yratgu - vqk
  cnefr vgno gy +(fuvsg) ge
  bgno = ge || gy

  vs pncf gura vachg = vachg.hccre

  pvcure = vachg.genafyngr(vgno || vgno.ybjre, bgno || bgno.ybjre)

  erghea pvcure

zrgubq pnrfne_rapvcure(vachg = Erkk, vqk = vag, pncf = obbyrna) choyvp fgngvp fvtanyf VyyrtnyNethzragRkprcgvba

  erghea pnrfne(vachg, vqk, pncf)

zrgubq pnrfne_qrpvcure(vachg = Erkk, vqk = vag, pncf = obbyrna) choyvp fgngvp fvtanyf VyyrtnyNethzragRkprcgvba

  erghea pnrfne(vachg, vag(26) - vqk, vfSnyfr)

zrgubq pnrfne_rapvcure(vachg = Erkk, vqk = vag) choyvp fgngvp fvtanyf VyyrtnyNethzragRkprcgvba

  erghea pnrfne(vachg, vqk, vfSnyfr)

zrgubq pnrfne_qrpvcure(vachg = Erkk, vqk = vag) choyvp fgngvp fvtanyf VyyrtnyNethzragRkprcgvba

  erghea pnrfne(vachg, vag(26) - vqk, vfSnyfr)

zrgubq pnrfne_rapvcure(vachg = Erkk, vqk = vag, bcg = Erkk) choyvp fgngvp fvtanyf VyyrtnyNethzragRkprcgvba

  erghea pnrfne(vachg, vqk, bcg)

zrgubq pnrfne_qrpvcure(vachg = Erkk, vqk = vag, bcg = Erkk) choyvp fgngvp fvtanyf VyyrtnyNethzragRkprcgvba

  erghea pnrfne(vachg, vag(26) - vqk, bcg)

zrgubq pnrfne(vachg = Erkk, vqk = vag, bcg = Erkk) choyvp fgngvp fvtanyf VyyrtnyNethzragRkprcgvba

  vs bcg.hccre.nooeri('H') >= 1 gura pncf = vfGehr
  ryfr                               pncf = vfSnyfr

  erghea pnrfne(vachg, vqk, pncf)

zrgubq pnrfne(vachg = Erkk, vqk = vag) choyvp fgngvp fvtanyf VyyrtnyNethzragRkprcgvba

  erghea pnrfne(vachg, vqk, vfSnyfr)

zrgubq vfGehr choyvp fgngvp ergheaf obbyrna
  erghea (1 == 1)

zrgubq vfSnyfr choyvp fgngvp ergheaf obbyrna
  erghea \vfGehr

```



## NewLISP


```NewLISP
(define (rot13 str)
  (join
   (map
    (fn(c)
      (cond
       ((<= "A" (upper-case c) "M") (char (+ (char c) 13)))
       ((<= "N" (upper-case c) "Z") (char (- (char c) 13)))
       (true c)))
    (explode str))))
```



## Nim


```nim
import strutils

proc rot13(c: char): char =
  case toLowerAscii(c)
  of 'a'..'m': chr(ord(c) + 13)
  of 'n'..'z': chr(ord(c) - 13)
  else:        c

for line in stdin.lines:
  for c in line:
    stdout.write rot13(c)
  stdout.write "\n"
```



## Objeck


```objeck

bundle Default {
  class Rot13 {
    function : Main(args : String[]) ~ Nil {
      Rot13("nowhere ABJURER")->PrintLine();
    }

    function : native : Rot13(text : String) ~ String {
      rot := "";
      each(i : text) {
        c := text->Get(i);
        if(c >= 'a' & c <= 'm' | c >= 'A' & c <= 'M') {
          rot->Append(c + 13);
        }
        else if(c >= 'n' & c <= 'z' | c >= 'N' & c <= 'Z') {
          rot->Append(c - 13);
        }
        else {
          rot->Append(c);
        };
      };

      return rot;
    }
  }
}

```



## OCaml


Straightforward implementation for characters by using character range patterns:


```ocaml
let rot13 c = match c with
  | 'A'..'M' | 'a'..'m' -> char_of_int (int_of_char c + 13)
  | 'N'..'Z' | 'n'..'z' -> char_of_int (int_of_char c - 13)
  | _ -> c
```


We provide a function for converting whole strings:


```ocaml
let rot13_str s =
  let len = String.length s in
  let result = String.create len in
  for i = 0 to len - 1 do
    result.[i] <- rot13 s.[i]
  done;
  result

(* or in OCaml 4.00+:
   let rot13_str = String.map rot13
*)
```


And here is a utility program that converts the content read on <code>sdtin</code> and write it to <code>stdout</code>:


```ocaml
let () =
  try while true do
    String.iter (fun c -> print_char (rot13 c)) (read_line());
    print_newline()
  done with End_of_file -> ()
```



## Oforth



```Oforth
: encryptRot13(c)
   c dup isLetter ifFalse: [ return ]
   isUpper ifTrue: [ 'A' ] else: [ 'a' ] c 13 + over - 26 mod + ;

: rot13   map(#encryptRot13) charsAsString ;
```


```txt

>"Oolite quick Thargoid jumps lazy Vipers = blown up + special fx" rot13 println
Bbyvgr dhvpx Gunetbvq whzcf ynml Ivcref = oybja hc + fcrpvny sk

```



## Oz


```oz
declare
  fun {RotChar C}
     if     C >= &A andthen C =< &Z then &A + (C - &A + 13) mod 26
     elseif C >= &a andthen C =< &z then &a + (C - &a + 13) mod 26
     else C
     end
  end

  fun {Rot13 S}
     {Map S RotChar}
  end
in
  {System.showInfo {Rot13 "NOWHERE Abjurer 42"}}
  {System.showInfo {Rot13 {Rot13 "NOWHERE Abjurer 42"}}}
```



## PARI/GP


```parigp
rot13(s)={
  s=Vecsmall(s);
  for(i=1,#s,
    if(s[i]>109&s[i]<123,s[i]-=13,if(s[i]<110&s[i]>96,s[i]+=13,if(s[i]>77&s[i]<91,s[i]-=13,if(s[i]<78&s[i]>64,s[i]+=13))))
  );
  Strchr(s)
};
```



## Pascal


```Pascal
program rot13;

var
    line: string;

    function rot13(someText: string): string;

    var
        i: integer;
        ch: char;
        result: string;

    begin
        result := '';
        for i := 1 to Length(someText) do
            begin
                ch := someText[i];
                case ch of
                    'A' .. 'M', 'a' .. 'm':
                        ch := chr(ord(ch)+13);
                    'N' .. 'Z', 'n' .. 'z':
                        ch := chr(ord(ch)-13);
                end;
                result := result + ch;
            end;
        rot13 := result;
    end;

begin
    while not eof(input) do
        begin
            readln(line);
            writeln(rot13(line));
        end;
end.
```



## Perl


```perl
sub rot13 {
  shift =~ tr/A-Za-z/N-ZA-Mn-za-m/r;
}

print rot13($_) while (<>);
```


Input:
 NOWHERE Abjurer

Output:
 ABJURER Nowhere

One-liner version:


```perl
perl -pe 'tr/A-Za-z/N-ZA-Mn-za-m/'
```



## Perl 6


```perl6
print slurp.trans: ['A'..'Z','a'..'z'] => ['N'..'Z','A'..'M','n'..'z','a'..'m']
```


Input:

```txt
Rosetta Code
```


Output:

```txt
Ebfrggn Pbqr
```



## Phix


```Phix
function rot13(string s)
integer ch
    for i=1 to length(s) do
        ch = upper(s[i])
        if ch>='A' and ch<='Z' then
            s[i] += iff(ch<='M',+13,-13)
        end if
    end for
    return s
end function
?rot13("abjurer NOWHERE.")
```

```txt

"nowhere ABJURER."

```



## PHP

PHP has a built-in function for this:

```php
echo str_rot13('foo'), "\n";
```

will output
 sbb

Here is an implementation:

```php
<?php
function rot13($s) {
    return strtr($s, 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz',
                     'NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm');
}

echo rot13('foo'), "\n";
?>
```


Output:
 sbb


## PicoLisp


```PicoLisp
(de rot13-Ch (C)
   (if
      (or
         (member C '`(apply circ (chop "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
         (member C '`(apply circ (chop "abcdefghijklmnopqrstuvwxyz"))) )
      (get @ 14)
      C ) )
```

or:

```PicoLisp
(de rot13-Ch (C)
   (cond
      ((>= "M" (uppc C) "A")
         (char (+ (char C) 13)) )
      ((>= "Z" (uppc C) "N")
         (char (- (char C) 13)) )
      (T C) ) )
```

Then call it as:

```PicoLisp
(de rot13-stdIn ()
   (while (line)
      (prinl (mapcar rot13-Ch @)) ) )
```



## Pike


```pike

import Crypto;

int main(){
   string r = rot13("Hello, World");
   write(r + "\n");
}

```



## PL/I


```pli

rotate: procedure (in) options (main);    /* 2 March 2011 */
   declare in character (100) varying;
   declare line character (500) varying;
   declare input file;

   open file (input) title ('/' || in || ',type(text),recsize(500)' );

   on endfile (input) stop;

   do forever;
      get file (input) edit (line) (L);
      line = translate (
             line, 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz',
                   'NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm');
      put edit (line) (a); put skip;
   end;
end;

```

Data file:

```txt

"The time has come,"
the walrus said,
"to speak of many things;
of ships and shoes and sealing wax;
of cabbages and kings."

```

Output:

```txt

"Gur gvzr unf pbzr,"
gur jnyehf fnvq,
"gb fcrnx bs znal guvatf;
bs fuvcf naq fubrf naq frnyvat jnk;
bs pnoontrf naq xvatf."

```



## PL/SQL


```plsql
-- Works for VARCHAR2 (up to 32k chars)
CREATE OR REPLACE FUNCTION fn_rot13_native(p_text VARCHAR2) RETURN VARCHAR2 IS
  c_source CONSTANT VARCHAR2(52) := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  c_target CONSTANT VARCHAR2(52) := 'NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm';
BEGIN
  RETURN TRANSLATE(p_text, c_source, c_target);
END;
/

-- For CLOBs (translate only works with VARCHAR2, so do it in chunks)
CREATE OR REPLACE FUNCTION fn_rot13_clob(p_text CLOB) RETURN CLOB IS
  c_source CONSTANT VARCHAR2(52) := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  c_target CONSTANT VARCHAR2(52) := 'NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm';
  c_chunk_size CONSTANT PLS_INTEGER := 4000;
  v_result CLOB := NULL;
BEGIN
  FOR i IN 0..TRUNC(LENGTH(p_text) / c_chunk_size) LOOP
    v_result := v_result ||
      TRANSLATE(dbms_lob.substr(p_text, c_chunk_size, i * c_chunk_size + 1), c_source, c_target);
  END LOOP;
  RETURN v_result;
END;
/

-- The full algorithm (Slower. And MUCH slower if using CLOB!)
CREATE OR REPLACE FUNCTION fn_rot13_algorithm(p_text VARCHAR2) RETURN CLOB IS
  c_upper_a CONSTANT PLS_INTEGER := ASCII('A');
  c_lower_a CONSTANT PLS_INTEGER := ASCII('a');
  v_rot VARCHAR2(32000);
  v_char VARCHAR2(1);
BEGIN
  FOR i IN 1..LENGTH(p_text) LOOP
    v_char := SUBSTR(p_text, i, 1);
    IF v_char BETWEEN 'A' AND 'Z' THEN
      v_rot := v_rot || CHR(MOD(ASCII(v_char) - c_upper_a + 13, 26) + c_upper_a);
    ELSIF v_char BETWEEN 'a' AND 'z' THEN
      v_rot := v_rot || CHR(MOD(ASCII(v_char) - c_lower_a + 13, 26) + c_lower_a);
    ELSE
      v_rot := v_rot || v_char;
    END IF;
  END LOOP;
  RETURN v_rot;
END;
/

```

```txt

SELECT fn_rot13_native('Hello ROT-13') FROM DUAL;
SELECT fn_rot13_clob('Hello ROT-13') FROM DUAL;
SELECT fn_rot13_algorithm('Hello ROT-13') FROM DUAL;

-- All return:
-- Uryyb EBG-13

```



## PostScript

```postscript

/r13 {
4 dict begin
    /rotc {
        {
        {{{64 gt} {91 lt}} all?} {65 - 13 + 26 mod 65 +} is?
        {{{95 gt} {123 lt}} all?} {97 - 13 + 26 mod 97 +} is?
        } cond
    }.
    {rotc} map cvstr
end}.

```



## Pop11


In Pop11 characters are just integers, so we can use integer
comparisons and arithmetic (assuming ASCII based encoding).


```pop11
define rot13(s);
    lvars j, c;
    for j from 1 to length(s) do
        s(j) -> c;
        if `A` <= c and c <= `M` or `a` <= c and c <= `m` then
            c + 13 -> s(j);
        elseif `N` <= c and c <= `Z` or `n` <= c and c <= `z` then
            c - 13 -> s(j);
        endif;
    endfor;
    s;
enddefine;

rot13('NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm') =>
```



## PowerBASIC


```powerbasic

#COMPILE EXE
#COMPILER PBWIN 9.05
#DIM ALL

FUNCTION ROT13(BYVAL a AS STRING) AS STRING
  LOCAL p AS BYTE PTR
  LOCAL n AS BYTE, e AS BYTE
  LOCAL res AS STRING

  res = a
  p = STRPTR(res)
  n = @p
  DO WHILE n
    SELECT CASE n
      CASE 65 TO 90
        e = 90
        n += 13
      CASE 97 TO 122
        e = 122
        n += 13
      CASE ELSE
        e = 255
    END SELECT

    IF n > e THEN
      n -= 26
    END IF
    @p = n
    INCR p
    n = @p
  LOOP
FUNCTION = res
END FUNCTION

'testing:

FUNCTION PBMAIN () AS LONG
#DEBUG PRINT ROT13("abc")
#DEBUG PRINT ROT13("nop")
END FUNCTION

```



## PowerShell



```powershell

$e = "This is a test Guvf vf n grfg"

[char[]](0..64+78..90+65..77+91..96+110..122+97..109+123..255)[[char[]]$e] -join ""

```



## Prolog

Works with SWI-Prolog.

```Prolog
rot13(Str, SR) :-
	maplist(rot, Str, Str1),
	string_to_list(SR, Str1).

rot(C, C1) :-
	(   member(C, "abcdefghijklmABCDEFGHIJKLM") -> C1 is C+13;
	    (	member(C, "nopqrstuvwxyzNOPQRSTUVWXYZ") -> C1 is C-13; C1 = C)).

```

Output :

```Prolog
 ?- rot13("The Quick Brown Fox Jumped Over The Lazy Dog!", SR).
SR = "Gur Dhvpx Oebja Sbk Whzcrq Bire Gur Ynml Qbt!".
```



## PureBasic


```PureBasic
Declare.s Rot13(text_to_code.s)

If OpenConsole()
  Define txt$

  Print("Enter a string to encode: "): txt$=Input()

  PrintN("Coded  : "+Rot13(txt$))
  PrintN("Decoded: "+Rot13(Rot13(txt$)))

  Print("Press ENTER to quit."): Input()
  CloseConsole()
EndIf

Procedure.s Rot13(s.s)
  Protected.i i
  Protected.s t, u
  For i=1 To Len(s)
    t=Mid(s,i,1)
    Select Asc(t)
      Case Asc("a") To Asc("m"), Asc("A") To Asc("M")
        t=chr(Asc(t)+13)
      Case Asc("n") To Asc("z"), Asc("N") To Asc("Z")
        t=chr(Asc(t)-13)
    EndSelect
    u+t
  Next
  ProcedureReturn u
EndProcedure
```



## Python

Python 2.x (but not 3.x) has built-in rot13 encoding and decoding:
```python>>>
 u'foo'.encode('rot13')
'sbb'
>>> 'sbb'.decode('rot13')
u'foo'
```


In both Python 2.x and 3.x one can use the standard library module <code>codecs</code> for rot13 encoding and decoding:

```python>>>
 import codecs
>>> codecs.encode("The quick brown fox jumps over the lazy dog", "rot13")
'Gur dhvpx oebja sbk whzcf bire gur ynml qbt'
>>> codecs.decode(_, "rot13")
'The quick brown fox jumps over the lazy dog'
```


An alternative that doesn't rely on the built-in "rot13" codec:

```python
#!/usr/bin/env python
import string

TRANSLATION_TABLE = str.maketrans(
    string.ascii_uppercase + string.ascii_lowercase,
    string.ascii_uppercase[13:] + string.ascii_uppercase[:13] +
    string.ascii_lowercase[13:] + string.ascii_lowercase[:13]
)


def rot13(s):
    """Return the rot-13 encoding of s."""
    return s.translate(TRANSLATION_TABLE)


if __name__ == "__main__":
    """rot-13 encode the input files, or stdin if no files are provided."""
    import fileinput
    for line in fileinput.input():
        print(rot13(line), end="")
```


The ''str.translate()'' and ''str.maketrans()'' functions make the function's definition almost trivial. The ''fileinput'' module makes the wrapper functionality trivial to implement. This can be adapted for Python2.x by replacing ''str.maketrans'' with ''string.maketrans'' and using Python2 style print statement in place of the Python3 print function.

This one uses a dictionary comprehension to define the key for
lowercase then another to updates with with uppercase mappings.
It uses generator expression in a lambda as the encoding function
and the dictionary ''.get()'' (with default value) to preserve any non-letter
characters during encoding.  This lambda line can be used to generate
an encoding function for '''any''' substitution cipher defined in the name
"key."

```python
#!/usr/bin/env python
from __future__ import print_function
import string
lets = string.ascii_lowercase
key = {x:y for (x,y) in zip(lets[13:]+lets[:14], lets)}
key.update({x.upper():key[x].upper() for x in key.keys()})
encode = lambda x: ''.join((key.get(c,c) for c in x))
if __name__ == '__main__':
   """Peform line-by-line rot-13 encoding on any files listed on our
      command line or act as a standard UNIX filter (if no arguments
      specified).
   """
   import fileinput
   for line in fileinput.input():
      print(encode(line), end="")
```



## R


```R
rot13 <- function(x)
{
  old <- paste(letters, LETTERS, collapse="", sep="")
  new <- paste(substr(old, 27, 52), substr(old, 1, 26), sep="")
  chartr(old, new, x)
}
x <- "The Quick Brown Fox Jumps Over The Lazy Dog!.,:;'#~[]{}"
rot13(x)   # "Gur Dhvpx Oebja Sbk Whzcf Bire Gur Ynml Qbt!.,:;'#~[]{}"
x2 <- paste(letters, LETTERS, collapse="", sep="")
rot13(x2)  # "nNoOpPqQrRsStTuUvVwWxXyYzZaAbBcCdDeEfFgGhHiIjJkKlLmM"
```

For a slightly more general function, see the [http://stat.ethz.ch/R-manual/R-patched/library/base/html/chartr.html example on the chartr help page].


## Racket


```racket

#!/usr/bin/env racket
#lang racket/base

(define (run i o)
  (for ([ch (in-producer regexp-match #f #rx#"[a-zA-Z]" i 0 #f o)])
    (define b (bytes-ref (car ch) 0))
    (define a (if (< b 96) 65 97))
    (write-byte (+ (modulo (+ 13 (- b a)) 26) a))))

(require racket/cmdline)
(command-line
 #:help-labels "(\"-\" specifies standard input)"
 #:args files
 (for ([f (if (null? files) '("-") files)])
   (if (equal? f "-")
     (run (current-input-port) (current-output-port))
     (call-with-input-file f (λ(i) (run i (current-output-port)))))))

```



## RapidQ


```vb

function ROT13 (InputTxt as string) as string
    dim i as integer, ascVal as byte
    Result = ""

    for i = 1 to len(InputTxt)
        ascVal = asc(InputTxt[i])

        select case ascVal
        case 65 to 77, 97 to 109
            Result = Result + chr$(ascVal + 13)
        case 78 to 90, 110 to 122
            Result = Result + chr$(ascVal - 13)
        case else
            Result = Result + chr$(ascVal)
        end select
    next
end function

Input "Text to encode: "; a$
Print ROT13(a$)
Input "Press a key to end..."; a$


```



## Raven


```Raven
define rot13 use $str
    $str each chr
        dup m/[A-Ma-m]/ if
            ord 13 + chr
        else
            dup m/[N-Zn-z]/ if
               ord 13 - chr
    $str length list "" join

"12!ABJURER nowhere"
dup print "\nas rot13 is\n" print
rot13
print "\n" print
```

```txt
12!ABJURER nowhere
as rot13 is
12!NOWHERE abjurer
```



## REBOL


```REBOL
REBOL [
    Title: "Rot-13"
    URL: http://rosettacode.org/wiki/Rot-13
]

; Test data has upper and lower case characters as well as characters
; that should not be transformed, like numbers, spaces and symbols.

text: "This is a 28-character test!"

print "Using cipher table:"

; I build a set of correspondence lists here. 'x' is the letters from
; A-Z, in both upper and lowercase form. Note that REBOL can iterate
; directly over the alphabetic character sequence in the for loop. 'y'
; is the cipher form, 'x' rotated by 26 characters (remember, I have
; the lower and uppercase forms together). 'r' holds the final result,
; built as I iterate across the 'text' string. I search for the
; current character in the plaintext list ('x'), if I find it, I get
; the corresponding character from the ciphertext list
; ('y'). Otherwise, I pass the character through untransformed, then
; return the final string.

rot-13: func [
	"Encrypt or decrypt rot-13 with tables."
	text [string!] "Text to en/decrypt."
	/local x y r i c
] [
	x: copy ""  for i #"a" #"z" 1 [append x rejoin [i  uppercase i]]
	y: rejoin [copy skip x 26  copy/part x 26]
	r: copy ""

	repeat i text [append r  either c: find/case x i [y/(index? c)][i]]
	r
]

; Note that I am setting the 'text' variable to the result of rot-13
; so I can reuse it again on the next call. The rot-13 algorithm is
; reversible, so I can just run it again without modification to decrypt.

print ["    Encrypted:"  text: rot-13 text]
print ["    Decrypted:"  text: rot-13 text]


print "Using parse:"

clamp: func [
	"Contain a value within two enclosing values. Wraps if necessary."
	x v y
][
	x: to-integer x  v: to-integer v  y: to-integer y
	case [v < x [y - v] v > y [v - y + x - 1] true v]
]

; I'm using REBOL's 'parse' word here. I set up character sets for
; upper and lower-case letters, then let parse walk across the
; text. It looks for matches to upper-case letters, then lower-case,
; then skips to the next one if it can't find either. If a matching
; character is found, it's mathematically incremented by 13 and
; clamped to the appropriate character range. parse changes the
; character in place in the string, hence this is a destructive
; operation.

rot-13: func [
	"Encrypt or decrypt rot-13 with parse."
	text [string!] "Text to en/decrypt. Note: Destructive!"
] [
	u: charset [#"A" - #"Z"]
	l: charset [#"a" - #"z"]

	parse text [some [
			i:                                          ; Current position.
			u (i/1: to-char clamp #"A" i/1 + 13 #"Z") | ; Upper case.
			l (i/1: to-char clamp #"a" i/1 + 13 #"z") | ; Lower case.
			skip]]                                      ; Ignore others.
	text
]

; As you see, I don't need to re-assign 'text' anymore.

print ["    Encrypted:" rot-13 text]
print ["    Decrypted:" rot-13 text]
```


Output:


```txt
Using cipher table:
    Encrypted: Guvf vf n 28-punenpgre grfg!
    Decrypted: This is a 28-character test!
Using parse:
    Encrypted: Guvf vf n 28-punenpgre grfg!
    Decrypted: This is a 28-character test!
```



## Retro


```retro
{{
  : rotate  ( cb-c ) tuck - 13 + 26 mod + ;
  : rotate? (  c-c )
    dup 'a 'z within [ 'a rotate ] ifTrue
    dup 'A 'Z within [ 'A rotate ] ifTrue ;
---reveal---
  : rot13   (  s-s ) dup [ [ @ rotate? ] sip ! ] ^types'STRING each@ ;
}}

"abcdef123GHIJKL" rot13 dup puts cr rot13 puts
"abjurer NOWHERE" rot13 puts
```



## REXX

This REXX version supports upper and lower case letters, preserves their case (upper/lower),

and passes through (without alteration) all non-alphabetic input characters.

```rexx
/*REXX program  encodes  several example text strings  using  the  ROT-13  algorithm.   */
$='foo'                         ; say "simple text=" $;  say 'rot-13 text=' rot13($);  say
$='bar'                         ; say "simple text=" $;  say 'rot-13 text=' rot13($);  say
$="Noyr jnf V, 'rer V fnj Ryon."; say "simple text=" $;  say 'rot-13 text=' rot13($);  say
$='abc?  ABC!'                  ; say "simple text=" $;  say 'rot-13 text=' rot13($);  say
$='abjurer NOWHERE'             ; say "simple text=" $;  say 'rot-13 text=' rot13($);  say
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
rot13: return  translate( arg(1), 'abcdefghijklmABCDEFGHIJKLMnopqrstuvwxyzNOPQRSTUVWXYZ',,
                                  "nopqrstuvwxyzNOPQRSTUVWXYZabcdefghijklmABCDEFGHIJKLM")
```

'''output'''

```txt

simple text = foo
rot-13 text = sbb

simple text = bar
rot-13 text = one

simple text = Noyr jnf V, 'rer V fnj Ryon.
rot-13 text = Able was I, 'ere I saw Elba.

simple text = abc?  ABC!
rot-13 text = nop?  NOP!

simple text = abjurer NOWHERE
rot-13 text = nowhere ABJURER

```



## Ring


```ring

see "enter a string : " give s
ans = ""
for a = 1 to len(s)
    letter = substr(s, a, 1)
    if letter >= "a" and letter <= "z"
       char = char(ascii(letter) + 13)
       if char > "z" char = chr(asc(char) - 26) ok
    else
       if letter >= "a" and letter <= "z" char = char(ascii(letter) + 13) ok
       if char > "z" char = char(ascii(char) - 26) else  char = letter ok
    ok
    ans = ans + char
next
see ans + nl

```



## Ruby


```ruby
# Returns a copy of _s_ with rot13 encoding.
def rot13(s)
  s.tr('A-Za-z', 'N-ZA-Mn-za-m')
end

# Perform rot13 on files from command line, or standard input.
while line = ARGF.gets
  print rot13(line)
end
```


One can run <code>ruby rot13.rb file1 file2</code> to rot13 those files, or run <code>ruby rot13.rb</code> to rot13 the standard input.

Input:
 ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz

Output:
 NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm


## Run BASIC


```runbasic
INPUT "Enter a string: "; s$
ans$ = ""
FOR a = 1 TO LEN(s$)
        letter$ = MID$(s$, a, 1)
        IF letter$ >= "A" AND letter$ <= "Z" THEN
           char$ = CHR$(ASC(letter$) + 13)
           IF char$ > "Z" THEN char$ = CHR$(ASC(char$) - 26)
        else
           if letter$ >= "a" AND letter$ <= "z" THEN   char$ = CHR$(ASC(letter$) + 13)
           IF char$ > "z" THEN  char$ = CHR$(ASC(char$) - 26) ELSE  char$ = letter$
        END IF
        ans$ = ans$ + char$
NEXT a
PRINT ans$
```
Output:
```txt
Enter a string: ?abc
nop
Enter a string: ?ABC
NOP
```



## Rust

This program works for ascii, but is likely to break for utf8-strings containing non-ascii characters.

```rust
fn rot13 (string: String) -> String {
    let mut bytes: Vec<u8> = string.into();
    for byte in &mut bytes {
        match *byte {
            b'a'...b'm' | b'A'...b'M' => *byte += 13,
            b'n'...b'z' | b'N'...b'Z' => *byte -= 13,
            _ => (), // do nothing
        }
    }
    String::from_utf8(bytes).unwrap()
}

fn main () {
    let a =  rot13("abc".to_owned());
    assert_eq!(a, "nop");
}
```

A more rustcean way to implement Rot-13 which is UTF-8 safe.

```rust
fn rot13(string: String) -> String {
     let alphabet = [
         'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
         'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
     ];
     let upper_alphabet: Vec<_> = alphabet.iter().map(|c| c.to_ascii_uppercase()).collect();

     string.chars()
           .map(|c| *alphabet.iter()
                             .chain(alphabet.iter())
                             .chain(upper_alphabet.iter())
                             .chain(upper_alphabet.iter())
                             .skip_while(|&x| *x != c)
                             .nth(13)
                             .unwrap_or(&c))
           .collect()
}
```


=={{header|S-lang}}==
Seems to work even with UTF-8 text.
<lang S-lang>variable old = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
variable new = "NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm";

define rot13(s) {
  s = strtrans(s, old, new);
  return s;
}

define rot13_stream(s) {
  variable ln;
  while (-1 != fgets(&ln, s))
    fputs(rot13(ln), stdout);
}

if (__argc > 1) {
  variable arg, fp;
  foreach arg (__argv[[1:]]) {
    fp = fopen(arg, "r");
    rot13_stream(fp);
  }
}
else
  rot13_stream(stdin);
```



## Scala


```scala>scala
 def rot13(s: String) = s map {
     |   case c if 'a' <= c.toLower && c.toLower <= 'm' => c + 13 toChar
     |   case c if 'n' <= c.toLower && c.toLower <= 'z' => c - 13 toChar
     |   case c => c
     | }
rot13: (s: String)String

scala> rot13("7 Cities of Gold.")
res61: String = 7 Pvgvrf bs Tbyq.

scala> rot13(res61)
res62: String = 7 Cities of Gold.
```



## Scheme


```scheme
(define (rot13 str)
  (define (rot13-char c)
    (integer->char (+ (char->integer c)
                      (cond ((and (char>=? c #\a) (char<? c #\n))
                             13)
                            ((and (char>=? c #\A) (char<? c #\N))
                             13)
                            ((and (char>=? c #\n) (char<=? c #\z))
                             -13)
                            ((and (char>=? c #\N) (char<=? c #\Z))
                             -13)
                            (else
                             0)))))
  (list->string (map rot13-char (string->list str))))

```



## sed

The two translations (upper and lower case) are separate only for documentation and ease of understanding; they could be combined into one command.

```sed
y/abcdefghijklmnopqrstuvwxyz/nopqrstuvwxyzabcdefghijklm/
y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/NOPQRSTUVWXYZABCDEFGHIJKLM/
```



## Seed7


This rot13 program reads from standard input and writes to standard output:

```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var char: ch is ' ';
  begin
    ch := getc(IN);
    while not eof(IN) do
      if (ch >= 'a' and ch <= 'm') or (ch >= 'A' and ch <= 'M') then
        ch := chr(ord(ch) + 13);
      elsif (ch >= 'n' and ch <= 'z') or (ch >= 'N' and ch <= 'Z') then
        ch := chr(ord(ch) - 13);
      end if;
      write(ch);
      ch := getc(IN);
    end while;
  end func;
```



## Sidef


```ruby
# Returns a copy of 's' with rot13 encoding.
func rot13(s) {
    s.tr('A-Za-z', 'N-ZA-Mn-za-m');
}

# Perform rot13 on standard input.
STDIN.each { |line| print rot13(line) }
```



## Slate

A shell script:


```slate
#!/usr/local/bin/slate

ch@(String Character traits) rot13
[| value |
  upper ::= ch isUppercase.
  value := ch toLowercase as: Integer.
  (value >= 97) /\ [value < 110]
    ifTrue: [value += 13]
    ifFalse: [(value > 109) /\ [value <= 122]
                ifTrue: [value -= 13]].
  upper
    ifTrue: [(value as: String Character) toUppercase]
    ifFalse: [value as: String Character]
].

lobby define: #Rot13Encoder &parents: {Encoder}.

c@(Rot13Encoder traits) convert
[
  [c in isAtEnd] whileFalse: [c out nextPut: c in next rot13].
].

(Rot13Encoder newFrom: Console reader to: Console writer) convert.
```


Normal functions:


```slate
ch@(String Character traits) rot13
[| value |
  upper ::= ch isUppercase.
  value := ch toLowercase as: Integer.
  (value >= 97) /\ [value < 110]
    ifTrue: [value += 13]
    ifFalse: [(value > 109) /\ [value <= 122]
                ifTrue: [value -= 13]].
  upper
    ifTrue: [(value as: String Character) toUppercase]
    ifFalse: [value as: String Character]
].

s@(String traits) rot13
[
  result ::= s newSameSize.
  s doWithIndex: [| :each :index | result at: index put: each rot13].
  result
].

slate[37]> 'abc123' rot13.
'nop123'
```



## Smalltalk

Here we implemented three ways. The first one is the simplest. The second demonstrates extending the String class with a generic <code>rot</code> method, which in turn uses two ''new'' method for the class Character (+ and -). The third one is an imitation of the <tt>tr '[a-m][n-z]' '[n-z][a-m]'</tt> approach (see UNIX Shell example), done through a block closure and using also the new method <code>trFrom:to:</code> for Character.


```smalltalk
"1. simple approach"
rot13 := [ :string |
  string collect: [ :each | | index |
    index := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
      indexOf: each ifAbsent: [ 0 ]. "Smalltalk uses 1-based indexing"
        index isZero
          ifTrue: [ each ]
          ifFalse: [ 'nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM' at:
            index ] ] ].

(rot13 value: 'Test123') printNl "gives 'Grfg123'"

"2. extending built-in classes"
Character extend [
  + inc [
     (inc isKindOf: Character)
     ifTrue: [
        ^ ( Character value: ((self asInteger) + (inc asInteger)) )
     ] ifFalse: [
       ^ ( Character value: ((self asInteger) + inc) )
     ]
  ]
  - inc [
    ^ ( self + (inc asInteger negated) )
  ]
  trFrom: map1 to: map2 [
     (map1 includes: self) ifTrue: [
        ^ map2 at: (map1 indexOf: self)
     ] ifFalse: [ ^self ]
  ]
].

String extend [
  rot: num [ |s|
    s := String new.
    self do: [ :c |
         ((c asLowercase) between: $a and: $z)
	   ifTrue: [ |c1|
              c1 := ( $a + ((((c asLowercase) - $a + num) asInteger) rem:26)).
              (c isLowercase) ifFalse: [ c1 := c1 asUppercase ].
              s := s, (c1 asString)
           ]
           ifFalse: [
	      s := s, (c asString)
           ]
     ].
     ^s
  ]
].

('abcdefghijklmnopqrstuvwxyz123!' rot: 13) displayNl.
(('abcdefghijklmnopqrstuvwxyz123!' rot: 13) rot: 13) displayNl.



"2. using a 'translation'. Not very idiomatic Smalltalk code"
rotThirteen := [ :s | |m1 m2 r|
  r := String new.
  m1 := OrderedCollection new.
  0 to: 25 do: [ :i | m1 add: ($a + i) ].
  m2 := OrderedCollection new.
  0 to: 25 do: [ :i | m2 add: ($a + ((i+13) rem: 26)) ].
  s do: [ :c |
    (c between: $a and: $z) | (c between: $A and: $Z)
      ifTrue: [ | a |
        a := (c asLowercase) trFrom: m1 to: m2.
        (c isUppercase) ifTrue: [ a := a asUppercase ].
        r := r, (a asString)]
      ifFalse: [ r := r, (c asString) ]
  ].
  r
].

(rotThirteen value: 'abcdefghijklmnopqrstuvwxyz123!') displayNl.
```



## SNOBOL4


```SNOBOL4
*       # Function using replace( )
        define('rot13(s)u1,u2,l1,l2') :(rot13_end)
rot13   &ucase len(13) . u1 rem . u2
        &lcase len(13) . l1 rem . l2
        rot13 = replace(s,&ucase &lcase,u2 u1 l2 l1) :(return)
rot13_end

*       # Function using pattern
        define('rot13s(s)c')
        alfa = &ucase &ucase &lcase &lcase :(rot13s_end)
rot13s  s len(1) . c = :f(return)
        alfa break(c) len(13) len(1) . c
        rot13s = rot13s c :(rot13s)
rot13s_end

*       # Test and display both
        str = rot13("I abjure the $19.99 trinket!")
        output = str; output = rot13(str)
        str = rot13s("He's a real Nowhere Man.")
        output = str; output = rot13s(str)
end
```


Output:

```txt
V nowher gur $19.99 gevaxrg!
I abjure the $19.99 trinket!
Ur'f n erny Abjurer Zna.
He's a real Nowhere Man.
```



## SQL


```sql

select translate(
        'The quick brown fox jumps over the lazy dog.',
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz',
        'NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm'
        )
        from dual;

```

```tsql

with cte(num) as
(
 select 1
 union all
 select num+1
 from cte
)
select cast((
select char(ascii(chr) +
		case
			when    ascii(chr) between ascii('a') and ascii('m') or
				ascii(chr) between ascii('A') and ascii('M') then 13
			when    ascii(chr) between ascii('n') and ascii('z') or
				ascii(chr) between ascii('N') and ascii('Z') then -13
			else    0
		end)
from
(
select top(1000) num,
		 -- your string to be converted to ROT13
                 substring('The Quick Brown Fox Jumps Over The Lazy Dog',num,1) chr
from cte
) tmp
For XML PATH ('')) as xml).value('.', 'VARCHAR(max)') rot13
option (maxrecursion 0)

```



## Stata



```stata
function rot13(s) {
	u = ascii(s)
	i = selectindex(u:>64 :& u:<91)
	if (length(i)>0) u[i] = mod(u[i]:-52, 26):+65
	i = selectindex(u:>96 :& u:<123)
	if (length(i)>0) u[i] = mod(u[i]:-84, 26):+97
	return(char(u))
}

rot13("Shall Not Perish")
  Funyy Abg Crevfu
```



## Swift



```swift
func rot13char(c: UnicodeScalar) -> UnicodeScalar {
  switch c {
  case "A"..."M", "a"..."m":
    return UnicodeScalar(UInt32(c) + 13)
  case "N"..."Z", "n"..."z":
    return UnicodeScalar(UInt32(c) - 13)
  default:
    return c
  }
}

func rot13(str: String) -> String {
  return String(map(str.unicodeScalars){ c in Character(rot13char(c)) })
}

println(rot13("The quick brown fox jumps over the lazy dog"))
```

```txt

Gur dhvpx oebja sbk whzcf bire gur ynml qbt

```



## Tcl

===tcl-only===
using string map:

```tcl
proc rot13 line {
    string map {
        a n b o c p d q e r f s g t h u i v j w k x l y m z
        n a o b p c q d r e s f t g u h v i w j x k y l z m
        A N B O C P D Q E R F S G T H U I V J W K X L Y M Z
        N A O B P C Q D R E S F T G U H V I W J X K Y L Z M
    } $line
}
set tx "Hello, World !"
puts "$tx : [rot13 $tx]"
```

 Hello, World ! : Uryyb, Jbeyq !

==={{libheader|TclX}}===
using translit:

```tcl
package require Tclx
proc rot13 str {
    translit "A-Za-z" "N-ZA-Mn-za-m" $str
}
```



## TorqueScript

--[[User:Ipquarx|Ipquarx]] 8:45 PM

```Torque
function rot13(%string)
{
	%alph = "abcdefghijklmnopqrstuvwxyz";
	%len = strLen(%string);

	for(%a = 0; %a < %len; %a++)
	{
		%char = getSubStr(%string,%a,1);
		%pos = striPos(%alph, %char);

		if(%pos < 0)
			%out = %out @ %char;
		else
		{
			if(strPos(%alph, %char) < 0)
				%out = %out @ strUpr(getSubStr(%alph, (%pos + 13) % 26));
			else
				%out = %out @ getSubStr(%alph, (%pos + 13) % 26);
		}
	}
	return %out;
}
```


=={{header|TI-83 BASIC}}==
Calculator symbol translations:

"STO" arrow: &#8594;

Perfoms ROT-13 on the contents of Str1. Also uses the string variables Str0 and Str2 and the real variable N.


```ti83b
:"ABCDEFGHIJKLMNOPQRSTUVWXYZ→Str0
:".→Str2
:For(N,1,length(Str1
:If inString(Str0,sub(Str1,N,1
:Then
:inString(Str0,sub(Str1,N,1
:Ans+13-26(Ans>13
:Str2+sub(Str0,Ans,1→Str2
:Else
:Str2+sub(Str1,N,1→Str2
:End
:End
:sub(Str2,2,length(Str2)-1→Str1
```



## TXR


Via definition and subsequent use of a named filter.


```txr
@(deffilter rot13
   ("a" "n") ("b" "o") ("c" "p") ("d" "q") ("e" "r") ("f" "s") ("g" "t")
   ("h" "u") ("i" "v") ("j" "w") ("k" "x") ("l" "y") ("m" "z") ("n" "a")
   ("o" "b") ("p" "c") ("q" "d") ("r" "e") ("s" "f") ("t" "g") ("u" "h")
   ("v" "i") ("w" "j") ("x" "k") ("y" "l") ("z" "m")
   ("A" "N") ("B" "O") ("C" "P") ("D" "Q") ("E" "R") ("F" "S") ("G" "T")
   ("H" "U") ("I" "V") ("J" "W") ("K" "X") ("L" "Y") ("M" "Z") ("N" "A")
   ("O" "B") ("P" "C") ("Q" "D") ("R" "E") ("S" "F") ("T" "G") ("U" "H")
   ("V" "I") ("W" "J") ("X" "K") ("Y" "L") ("Z" "M"))
@(repeat)
@line
@  (output :filter rot13)
@line
@  (end)
@(end)
```


Via TXR Lisp:


```txrlisp
(defun rot13 (ch)
  (cond
    ((<= #\A ch #\Z) (wrap #\A #\Z (+ ch 13)))
    ((<= #\a ch #\z) (wrap #\a #\z (+ ch 13)))
    (t ch)))

   (whilet ((ch (get-char)))
     (put-char (rot13 ch)))
```



## UNIX Shell

===[[Bourne Shell]]===

UNIX shell assumes availability of the standard UNIX utility commands (in the "coreutils" package on Linux systems, for example); thus the ''tr'' (translate) command is trivially provided with the proper arguments to perform the rotations.

====[[POSIX]] compliant====

Built from six rotonyms.


```bash
tr \
'NOWHERE AZ clerk IRAQ faber gnat ABJURER NM pyrex VEND snore tang'\
'abjurer nm PYREX vend SNORE TANG nowhere az CLERK iraq FABER GNAT' \
'ABJURER NM pyrex VEND snore tang NOWHERE AZ clerk IRAQ faber gnat'\
'nowhere az CLERK iraq FABER GNAT abjurer nm PYREX vend SNORE TANG'

```


====Not [[POSIX]] compliant====


```bash
#!/bin/sh
rot13() {
   tr '[a-m][n-z][A-M][N-Z]' '[n-z][a-m][N-Z][A-M]'
}

cat ${1+"$@"} | rot13
```


A simple <code>tr a-zA-Z n-za-mN-ZA-M</code> would work with modern systems that follow [[POSIX]]. Our <code>tr '[a-m][n-z][A-M][N-Z]' '[n-z][a-m][N-Z][A-M]'</code> also works with those older System V systems. For newer systems, it translates '[' and ']' to themselves. (Refer to [http://www.openbsd.org/cgi-bin/man.cgi?query=tr&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html#STANDARDS OpenBSD tr(1) manual page, section STANDARDS].)

This example shows proper quoting around "$@" (magical argument list) such that this script work properly even if some of the files named on the command line contain embedded spaces or other such characters.  (The ${1+"$@"} check, unnecessary in modern systems, allows the script to work even on older systems where a bare "$@" expanded to a single empty string when no arguments were supplied).


## Unlambda


```Unlambda
``ci`d``@i`c``s`d```?aic.n``s`d```?bic.o``s`d```?cic.p``s`d```?dic.q``s`d```?eic
.r``s`d```?fic.s``s`d```?gic.t``s`d```?hic.u``s`d```?iic.v``s`d```?jic.w``s`d```
?kic.x``s`d```?lic.y``s`d```?mic.z``s`d```?nic.a``s`d```?oic.b``s`d```?pic.c``s`
d```?qic.d``s`d```?ric.e``s`d```?sic.f``s`d```?tic.g``s`d```?uic.h``s`d```?vic.i
``s`d```?wic.j``s`d```?xic.k``s`d```?yic.l``s`d```?zic.m``s`d```?Nic.A``s`d```?O
ic.B``s`d```?Pic.C``s`d```?Qic.D``s`d```?Ric.E``s`d```?Sic.F``s`d```?Tic.G``s`d`
``?Uic.H``s`d```?Vic.I``s`d```?Wic.J``s`d```?Xic.K``s`d```?Yic.L``s`d```?Zic.M``
s`d```?Aic.N``s`d```?Bic.O``s`d```?Cic.P``s`d```?Dic.Q``s`d```?Eic.R``s`d```?Fic
.S``s`d```?Gic.T``s`d```?Hic.U``s`d```?Iic.V``s`d```?Jic.W``s`d```?Kic.X``s`d```
?Lic.Y``s`d```?Mic.Z`d`|c
```



## Ursala

I/O in Ursala is meant to be handled automatically as much as possible by the run time system.
This source text describes only a function that operates on the contents of a list of files passed
to it as an argument, with the transformed files returned as a result. The #executable compiler
directive and its parameters mean that this source will be compiled to an executable file
with the required command line interface. The rot13 encryption algorithm itself is a simple
finite map implemented in a half line of code.

```Ursala
#import std

#executable (<'parameterized','default-to-stdin'>,<>)

rot = ~command.files; * contents:= ~contents; * * -:~& -- ^p(~&,rep13~&zyC)~~ ~=`A-~ letters
```



## Vedit macro language

Using ROT13.TBL from [http://cu2.home.comcast.net/~cu2/vedit/ here]

```vedit
Translate_Load("ROT13.TBL")
Translate_Block(0, File_Size)
```


You can execute the macro from DOS command prompt with the following command:
 vpw -q -x rot13.vdm inputfile -a outputfile

In addition to translating a block of text, the translate table allows viewing and editing ROT-13 text without translating the actual file into ASCII.
The displayed characters and keyboard input are translated on-the-fly.
This is the normal way to edit for example DOS/OEM and EBCDIC files.


## Visual Basic

```vb

Function ROT13(ByVal a As String) As String
  Dim i As Long
  Dim n As Integer, e As Integer

  ROT13 = a
  For i = 1 To Len(a)
    n = Asc(Mid$(a, i, 1))
    Select Case n
      Case 65 To 90
        e = 90
        n = n + 13
      Case 97 To 122
        e = 122
        n = n + 13
      Case Else
        e = 255
    End Select

    If n > e Then
      n = n - 26
    End If
    Mid$(ROT13, i, 1) = Chr$(n)
  Next i
End Function

```

Testing:

```vb

Sub Main()
  Debug.Assert ROT13("abc") = "nop"
  Debug.Assert ROT13("nop") = "abc"
End Sub

```



## Visual Basic .NET

'''Platform:''' [[.NET]]

```vbnet
Module Module1

   Private Function rot13(ByVal str As String) As String
       Dim newChars As Char(), i, j As Integer, original, replacement As String

       original = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
       replacement = "NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm"

       newChars = str.ToCharArray()

       For i = 0 To newChars.Length - 1
           For j = 0 To 51
               If newChars(i) = original(j) Then
                   newChars(i) = replacement(j)
                   Exit For
               End If
           Next
       Next

       Return New String(newChars)
   End Function

End Module
```

This solution just uses simple textual substitution, since the number of characters involved is small.  If the cipher involved more characters, it would be better to use character arithmetic; however, this is not encouraged in VB.Net.


## Wart


```python
def (rot13 s)
  (as string
      (map rot13
           (as list s)))

Alphabet <- "abcdefghijklmnopqrstuvwxyz"
def (rot13 c) :case (and string?.c len.c=1)
  if ("a" <= c <= "z")
       let idx (pos c Alphabet)
         Alphabet (idx+13 % 26)
     ("A" <= c <= "Z")
       (downcase.c -> rot13 -> upcase)
     :else
       c
```


Output:

```python
(rot13 "Moron")
=> "Zbeba"
```



## X86 Assembly

Using Linux/FASM.

```asm
format 	ELF 	executable 3
entry 	start

segment	readable writeable
buf	rb	1

segment	readable executable
start:	mov	eax, 3		; syscall "read"
	mov	ebx, 0		; stdin
	mov	ecx, buf	; buffer for read byte
	mov	edx, 1		; len (read one byte)
	int	80h

	cmp	eax, 0		; EOF?
	jz	exit

	xor 	eax, eax	; load read char to eax
	mov	al, [buf]
	cmp	eax, "A"	; see if it is in ascii a-z or A-Z
	jl	print
	cmp	eax, "z"
	jg	print
	cmp	eax, "Z"
	jle	rotup
	cmp	eax, "a"
	jge	rotlow
	jmp	print

rotup:	sub	eax, "A"-13	; do rot 13 for A-Z
	cdq
	mov	ebx, 26
	div	ebx
	add	edx, "A"
	jmp	rotend

rotlow:	sub	eax, "a"-13	; do rot 13 for a-z
	cdq
	mov	ebx, 26
	div	ebx
	add	edx, "a"

rotend:	mov	[buf], dl

print: 	mov	eax, 4		; syscall write
	mov	ebx, 1		; stdout
	mov	ecx, buf	; *char
	mov	edx, 1		; string length
	int	80h

	jmp	start

exit: 	mov     eax,1		; syscall exit
	xor     ebx,ebx		; exit code
	int     80h
```



## XPL0

Usage: rot13 <infile.txt >outfile.txt

```XPL0
code    ChIn=7, ChOut=8;
int     C, CC;
repeat  C:= ChIn(1);  CC:= C&~$20;  \CC handles lowercase too
        ChOut(0, C + (if CC>=^A & CC<=^M then +13
                 else if CC>=^N & CC<=^Z then -13
                 else 0));
until   C = $1A;  \EOF
```



## XSLT

Textual transforms are one of the domains XSLT was designed for.

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" />
  <xsl:variable name="alpha">ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz</xsl:variable>
  <xsl:variable name="rot13">NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm</xsl:variable>
  <xsl:template match="body">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="rot13">
    <xsl:value-of select="translate(.,$alpha,$rot13)"/>
  </xsl:template>
</xsl:stylesheet>
```

This will transform the input:
 <body>The <rot13>Abjurer</rot13> was to be found <rot13>Nowhere</rot13>.</body>
into:
 The Nowhere was to be found Abjurer.


## zig


```zig
fn rot13(in: []u8) void {
    for (in) |*c| {
        if (c.* < 109) {
            c.* = c.* + 13;
        } else if (c.* > 109) {
            c.* = c.* - 13;
        }
    }
}
```



## zkl

File rot13.zkl:

```zkl
#!/home/craigd/Bin/zkl
fcn rot13(text){
   text.translate("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
   		  "nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM");
}
text:=(vm.arglist or File.stdin);  // command line or pipe
text.pump(File.stdout,rot13);      // rotate each word and print it
if(text.isType(List)) File.stdout.writeln();	// command line gets ending newline
```


```txt

$ ./rot13.zkl "Hello World!"
Uryyb Jbeyq!

$ ./rot13.zkl Hello " " 'World!'
Uryyb Jbeyq!

$ echo "Uryyb Jbeyq!" | ./rot13.zkl
Hello World!

$ ./rot13.zkl < rot13.zkl
#!/ubzr/penvtq/Ova/mxy
spa ebg13(grkg){
   grkg.genafyngr("nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM",
   		  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
}
grkg:=(iz.netyvfg be Svyr.fgqva);  // pbzznaq yvar be cvcr
grkg.chzc(Svyr.fgqbhg,ebg13);      // ebgngr rnpu jbeq naq cevag vg
vs(grkg.vfGlcr(Yvfg)) Svyr.fgqbhg.jevgrya();	// pbzznaq yvar trgf raqvat arjyvar

```



## ZX Spectrum Basic

```zxbasic
10 CLS
20 INPUT "Enter a string: ", s$
30 LET a$ = "": REM a$ is the encoded string
40 FOR l = 1 TO LEN(s$)
50 LET i$ = s$(l): REM i$ is the letter being worked on
60 IF i$ < "A" OR i$ > "Z" THEN GO TO 100
70 LET c$ = CHR$(CODE(i$) + 13): REM c$ is the encoded letter
80 IF c$ > "Z" THEN LET c$ = CHR$(CODE(c$) - 26)
90 GO TO 300
100 IF i$ < "a" OR i$ > "z" THEN GO TO 200
110 LET c$ = CHR$(CODE(i$) + 13)
120 IF c$ > "z" THEN LET c$ = CHR$(CODE(c$) - 26)
130 GO TO 300
200 LET c$ = i$
300 LET a$ = a$ + c$
310 NEXT l
320 PRINT a$
```

