+++
title = "Temperature conversion"
description = ""
date = 2019-10-18T20:26:52Z
aliases = []
[extra]
id = 12919
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "aime",
  "algol_68",
  "apl",
  "applescript",
  "autohotkey",
  "autoit",
  "awk",
  "basic",
  "sinclair_zx81_basic",
  "basic256",
  "bbc_basic",
  "befunge",
  "bracmat",
  "c",
  "cpp",
  "csharp",
  "ceylon",
  "clojure",
  "cobol",
  "common_lisp",
  "d",
  "delphi",
  "easylang",
  "elena",
  "elixir",
  "erlang",
  "euphoria",
  "excel",
  "ezhil",
  "factor",
  "focal",
  "forth",
  "fortran",
  "freebasic",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lasso",
  "lil",
  "livecode",
  "lua",
  "maple",
  "mathematica",
  "miniscript",
  "minizinc",
  "ml",
  "netrexx",
  "never",
  "newlisp",
  "nim",
  "objeck",
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
  "pure_data",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "swift",
  "tcl",
  "unix_shell",
  "korn_shell",
  "ursa",
  "vba",
  "vbscript",
  "visual_foxpro",
  "xlisp",
  "xpl0",
  "zx_spectrum_basic",
]
+++

There are quite a number of temperature scales. For this task we will concentrate on four of the perhaps best-known ones:
[[wp:Kelvin|Kelvin]], [[wp:Degree Celsius|Celsius]], [[wp:Fahrenheit|Fahrenheit]], and  [[wp:Degree Rankine|Rankine]].

The Celsius and Kelvin scales have the same magnitude, but different null points.

: 0 degrees Celsius corresponds to 273.15 kelvin.
: 0 kelvin is absolute zero.

The Fahrenheit and Rankine scales also have the same magnitude, but different null points.

: 0 degrees Fahrenheit corresponds to 459.67 degrees Rankine.
: 0 degrees Rankine is absolute zero.

The Celsius/Kelvin and Fahrenheit/Rankine scales have a ratio of 5 : 9.


## Task

Write code that accepts a value of kelvin, converts it to values of the three other scales, and prints the result.


;Example:

```txt

K  21.00

C  -252.15

F  -421.87

R  37.80

```






## 360 Assembly

Use of packed decimal arithmetic
(ZAP,SP,MP,DP,UNPK,CVD,EDMK opcodes).
<lang>*        Temperature conversion    10/09/2015
TEMPERAT CSECT
         USING  TEMPERAT,R15
         LA     R4,1               i=1
         LA     R5,TT              @tt(1)
         LA     R6,IDE             @ide(1)
LOOPI    CH     R4,=AL2((T-TT)/8)  do i=1 to hbound(tt)
         BH     ELOOPI
         ZAP    T,0(8,R5)          t=tt(i)
         CVD    R4,DW              store to packed decimal
         UNPK   PG(1),DW+7(1)      unpack
         OI     PG,X'F0'           zap sign
         MVI    PG+1,C' '
         MVC    PG+2(12),0(R6)     ide(i)
         XPRNT  PG,14              output i
         MVC    PG(12),=C'Kelvin:     '
         MVC    ZN,EDMASKN         load mask
         EDMK   ZN,T+5             t (PL3)
         BCTR   R1,0               sign location
         MVC    0(1,R1),ZN+L'ZN-1  put sign
         MVC    PG+12(L'ZN-1),ZN   value
         MVC    PG+19(2),=C' K'    unit
         XPRNT  PG,21              output Kelvin
         MVC    PG(12),=C'Celsius:    '
         ZAP    DW,T               t
         SP     DW,=P'273.15'      t-273.15
         MVC    ZN,EDMASKN         load mask
         EDMK   ZN,DW+5            (PL3)
         BCTR   R1,0               sign location
         MVC    0(1,R1),ZN+L'ZN-1  put sign
         MVC    PG+12(L'ZN-1),ZN   value
         MVC    PG+19(2),=C' C'    unit
         XPRNT  PG,21              output Celsius
         MVC    PG(12),=C'Fahrenheit: '
         ZAP    DW,T               t
         MP     DW,=P'18'          *18
         DP     DW,=PL3'10'        /10
         ZAP    DW,DW(5)
         SP     DW,=P'459.67'      t*1.8-459.67
         MVC    ZN,EDMASKN         load mask
         EDMK   ZN,DW+5            (PL3)
         BCTR   R1,0               sign location
         MVC    0(1,R1),ZN+L'ZN-1  put sign
         MVC    PG+12(L'ZN-1),ZN   value
         MVC    PG+19(2),=C' F'    unit
         XPRNT  PG,21              output Fahrenheit
         MVC    PG(12),=C'Rankine:    '
         ZAP    DW,T               t
         MP     DW,=P'18'          *18
         DP     DW,=PL3'10'        /10
         ZAP    DW,DW(5)           t*1.8
         MVC    ZN,EDMASKN         load mask
         EDMK   ZN,DW+5            (PL3)
         BCTR   R1,0               sign location
         MVC    0(1,R1),ZN+L'ZN-1  put sign
         MVC    PG+12(L'ZN-1),ZN   value
         MVC    PG+19(2),=C' R'    unit
         XPRNT  PG,21              output Rankine
         LA     R4,1(R4)           i=i+1
         LA     R5,8(R5)           @tt(i)
         LA     R6,12(R6)          @ide(i)
         B      LOOPI
ELOOPI   XR     R15,R15
         BR     R14
IDE      DC     CL12'absolute',CL12'ice melts',CL12'water boils'
TT       DC     PL8'0.00',PL8'273.15',PL8'373.15'
T        DS     PL8
PG       DS     CL24
ZN       DS     ZL8                5num
DW       DS     D                  PL8 15num
EDMASKN  DC     X'402021204B202060'     CL8 5num
         YREGS
         END    TEMPERAT
```

```txt

1 absolute
Kelvin:        0.00 K
Celsius:    -273.15 C
Fahrenheit: -459.67 F
Rankine:       0.00 R
2 ice melts
Kelvin:      273.15 K
Celsius:       0.00 C
Fahrenheit:   32.00 F
Rankine:     491.67 R
3 water boils
Kelvin:      373.15 K
Celsius:     100.00 C
Fahrenheit:  212.00 F
Rankine:     671.67 R

```



## 8th


```forth
: KtoC \ n -- n
	273.15 n:-
;

: KtoF \ n -- n
	1.8 n:* 459.67 n:-
;

: KtoR \ n -- n
	1.8 n:*
;

: KtoCFR \ n --
	dup dup dup
	. " degrees Kelvin" . cr
	KtoC
	. " degrees Celcius" . cr
	KtoF
	. " degrees Fahrenheit" . cr
	KtoR
	. " degrees Rankine" . cr
;

: app:main \
	argc 0 n:=
	if
		"Syntax" . cr "    temp.8th number" . cr
	else
		0 args >n KtoCFR
	then
	bye
;

```

```txt
>8th temp.8th 21
21 degrees Kelvin
-252.15000 degrees Celcius
-421.87000 degrees Fahrenheit
37.80000 degrees Rankine

```



## Ada



```Ada
with Ada.Float_Text_IO, Ada.Text_IO;  use Ada.Float_Text_IO, Ada.Text_IO;

procedure Temperatur_Conversion is
   K: Float;
   function C return Float is (K - 273.15);
   function F return Float is (K * 1.8 - 459.67);
   function R return Float is (K * 1.8);
begin
   Get(K); New_Line;                                           -- Format
   Put("K: "); Put(K, Fore => 4, Aft => 2, Exp => 0); New_Line;-- K: dddd.dd
   Put("C: "); Put(C, Fore => 4, Aft => 2, Exp => 0); New_Line;-- C: dddd.dd
   Put("F: "); Put(F, Fore => 4, Aft => 2, Exp => 0); New_Line;-- F: dddd.dd
   Put("R: "); Put(R, Fore => 4, Aft => 2, Exp => 0); New_Line;-- R: dddd.dd
end;
```


```txt
21.0

K:   21.00
C: -252.15
F: -421.87
R:   37.80
```



## Aime


```aime
void
show(integer symbol, real temperature)
{
    o_form("%c /d2p2w8/\n", symbol, temperature);
}

integer
main(void)
{
    real k;

    k = atof(argv(1));

    show('K', k);
    show('C', k - 273.15);
    show('F', k * 1.8 - 459.67);
    show('R', k * 1.8);

    return 0;
}
```

```txt
aime$ aime -a tmp/tconvert 300
K   300
C    26.85
F    80.32
R   540
```



## ALGOL 68


```algol68

BEGIN
   REAL kelvin;
   read (kelvin);
   FORMAT f = $g(8,2), " K = ", g(8,2)xgl$;
   printf ((f, kelvin, kelvin - 273.15, "C"));
   printf ((f, kelvin, 9.0 * kelvin / 5.0, "R"));
   printf ((f, kelvin, 9.0 * kelvin / 5.0 - 459.67, "F"))
END
```

```txt
$ echo 21 | a68g Temperature_conversion.a68
  +21.00 K =  -252.15 C
  +21.00 K =   +37.80 R
  +21.00 K =  -421.87 F
$
```


=={{header|ALGOL-M}}==
If the temperature in Kelvin is a whole number, you should type a decimal point after it (e.g. <code>290.</code>): <code>290</code> with no decimal point will be interpreted as 0.29 rather than 290.0.

```algol
BEGIN
    DECIMAL K, C, F, R;
    WRITE( "Temperature in Kelvin:" );
    READ( K );
    C := K - 273.15;
    F := K * 1.8 - 459.67;
    R := K * 1.8;
    WRITE( K, " Kelvin is equivalent to" );
    WRITE( C, " degrees Celsius" );
    WRITE( F, " degrees Fahrenheit" );
    WRITE( R, " degrees Rankine" );
END
```



## APL

Given a temperature in Kelvin, prints the equivalent in Kelvin, Celsius, Fahrenheit, and Rankine (in that order).

```apl
      CONVERT←{⍵,(⍵-273.15),(R-459.67),(R←⍵×9÷5)}
```

```apl
      CONVERT 21
21 ¯252.15 ¯421.87 37.8
```

The "high minus" character <tt>¯</tt> is used in APL to mark negative numbers, preventing any possible confusion with <tt>-</tt> (the subtraction operator).



## AppleScript

{{Trans|JavaScript}} ( ES6 version )

```AppleScript
use framework "Foundation" -- Yosemite onwards, for the toLowerCase() function

-- KELVIN TO OTHER SCALE -----------------------------------------------------

-- kelvinAs :: ScaleName -> Num -> Num
on kelvinAs(strOtherScale, n)
    heatBabel(n, "Kelvin", strOtherScale)
end kelvinAs

-- MORE GENERAL CONVERSION ---------------------------------------------------

-- heatBabel :: n -> ScaleName -> ScaleName -> Num
on heatBabel(n, strFromScale, strToScale)
    set ratio to 9 / 5
    set cels to 273.15
    set fahr to 459.67

    script reading
        on |λ|(x, strFrom)
            if strFrom = "k" then
                x as real
            else if strFrom = "c" then
                x + cels
            else if strFrom = "f" then
                (fahr + x) * ratio
            else
                x / ratio
            end if
        end |λ|
    end script

    script writing
        on |λ|(x, strTo)
            if strTo = "k" then
                x
            else if strTo = "c" then
                x - cels
            else if strTo = "f" then
                (x * ratio) - fahr
            else
                x * ratio
            end if
        end |λ|
    end script

    writing's |λ|(reading's |λ|(n, ¬
        toLower(text 1 of strFromScale)), ¬
        toLower(text 1 of strToScale))
end heatBabel


-- TEST ----------------------------------------------------------------------
on kelvinTranslations(n)
    script translations
        on |λ|(x)
            {x, kelvinAs(x, n)}
        end |λ|
    end script

    map(translations, {"K", "C", "F", "R"})
end kelvinTranslations

on run
    script tabbed
        on |λ|(x)
            intercalate(tab, x)
        end |λ|
    end script

    intercalate(linefeed, map(tabbed, kelvinTranslations(21)))
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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

-- toLower :: String -> String
on toLower(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        lowercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toLower
```

```txt
K    21.0
C    -252.15
F    -421.87
R    37.8
```



## AutoHotkey


```AutoHotkey
MsgBox, % "Kelvin:`t`t 21.00 K`n"
        . "Celsius:`t`t" kelvinToCelsius(21) " C`n"
        . "Fahrenheit:`t" kelvinToFahrenheit(21) " F`n"
        . "Rankine:`t`t" kelvinToRankine(21) " R`n"

kelvinToCelsius(k)
{
    return, round(k - 273.15, 2)
}
kelvinToFahrenheit(k)
{
    return, round(k * 1.8 - 459.67, 2)
}
kelvinToRankine(k)
{
    return, round(k * 1.8, 2)
}
```

```txt
Kelvin:          21.00 K
Celsius:       -252.15 C
Fahrenheit:    -421.87 F
Rankine:         37.80 R
```



## AutoIt


```AutoIt
;	### USAGE - TESTING PURPOSES ONLY

Local Const $_KELVIN = 21
ConsoleWrite("Kelvin: " & $_KELVIN & @CRLF)
ConsoleWrite("Kelvin: " & Kelvin(21, "C") & @CRLF)
ConsoleWrite("Kelvin: " & Kelvin(21, "F") & @CRLF)
ConsoleWrite("Kelvin: " & Kelvin(21, "R") & @CRLF)

;	### KELVIN TEMPERATURE CONVERSIONS

Func Kelvin($degrees, $conversion)
	Select
		Case $conversion = "C"
			Return Round($degrees - 273.15, 2)
		Case $conversion = "F"
			Return Round(($degrees * 1.8) - 459.67, 2)
		Case $conversion = "R"
			Return Round($degrees * 1.8, 2)
	EndSelect
EndFunc ;==> Kelvin
```

```txt
Kelvin: 21°
Celsius: -252.15°
Fahrenheit: -421.87°
Rankine: 37.8°
```



## AWK

"Interactive" version, reading from stdin only:

```AWK
# syntax: AWK -f TEMPERATURE_CONVERSION.AWK
BEGIN {
    while (1) {
      printf("\nKelvin degrees? ")
      getline K
      if (K ~ /^$/) {
        break
      }
      if (K < 0) {
        print("K must be >= 0")
        continue
      }
      printf("K = %.2f\n",K)
      printf("C = %.2f\n",K - 273.15)
      printf("F = %.2f\n",K * 1.8 - 459.67)
      printf("R = %.2f\n",K * 1.8)
    }
    exit(0)
}
```


"Regular" version, reading from input-file(s).

With no such file, or "-" as filename, reading from stdin:

{{works with|gawk}} BEGINFILE is a gawk-extension

```AWK
# usage: gawk -f temperature_conversion.awk  input.txt -

BEGIN     { print("# Temperature conversion\n") }
BEGINFILE { print "# reading", FILENAME
            if( FILENAME=="-" ) print "# Please enter temperature values in K:\n"
          }

!NF       { exit }

          { print "Input:" $0 }
$1<0      { print("K must be >= 0\n"); next }
          { K = 0+$1
            printf("K = %8.2f Kelvin degrees\n",K)
            printf("C = %8.2f\n",  K - 273.15)
            printf("F = %8.2f\n",  K * 1.8 - 459.67)
            printf("R = %8.2f\n\n",K * 1.8)
          }

END       { print("# Bye.") }

```


{{out|Input}} the numeric value of the first word in each line is used as input for the conversion

```txt

-1
absolute
184
273.15 ice melts
310x
373.15 water boils
1941 Titanium melts

```

So, "absolute" has a value of 0, and 310x is just 310.

After that file is read and processed, values are read from stdin.

Here entering "333" by hand, and then stopping with an empty input.
```txt

# Temperature conversion

# reading input.txt
Input:-1
K must be >= 0

Input:absolute
K =     0.00 Kelvin degrees
C =  -273.15
F =  -459.67
R =     0.00

Input:184
K =   184.00 Kelvin degrees
C =   -89.15
F =  -128.47
R =   331.20

Input:273.15 ice melts
K =   273.15 Kelvin degrees
C =     0.00
F =    32.00
R =   491.67

Input:310x
K =   310.00 Kelvin degrees
C =    36.85
F =    98.33
R =   558.00

Input:373.15 water boils
K =   373.15 Kelvin degrees
C =   100.00
F =   212.00
R =   671.67

Input:1941 Titanium melts
K =  1941.00 Kelvin degrees
C =  1667.85
F =  3034.13
R =  3493.80

# reading -
# Please enter temperature values in K:

Input:333
K =   333.00 Kelvin degrees
C =    59.85
F =   139.73
R =   599.40

# Bye.

```



## BASIC



```basic

10 REM TRANSLATION OF AWK VERSION
20 INPUT "KELVIN DEGREES",K
30 IF K <= 0 THEN END: REM A VALUE OF ZERO OR LESS WILL END PROGRAM
40 LET C = K - 273.15
50 LET F = K * 1.8 - 459.67
60 LET R = K * 1.8
70 PRINT K; " KELVIN IS EQUIVALENT TO"
80 PRINT C; " DEGREES CELSIUS"
90 PRINT F; " DEGREES FAHRENHEIT"
100 PRINT R; " DEGREES RANKINE"
110 GOTO 20

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 INPUT PROMPT "Kelvin degrees: ":K
110 PRINT K;TAB(10);"Kelvin is equivalent to"
120 PRINT K-273.15;TAB(10);"Degrees Celsius"
130 PRINT K*1.8-459.67;TAB(10);"Degrees Fahrenheit"
140 PRINT K*1.8;TAB(10);"Degrees Rankine"
```


=
## Sinclair ZX81 BASIC
=

```basic
10 PRINT "ENTER A TEMPERATURE IN KELVINS"
20 INPUT K
30 PRINT K;" KELVINS ="
40 PRINT K-273.15;" DEGREES CELSIUS"
50 PRINT K*1.8-459.67;" DEGREES FAHRENHEIT"
60 PRINT K*1.8;" DEGREES RANKINE"
```



## BASIC256


```basic256

do
  print "Kelvin degrees (>=0): ";
  input K
  until K>=0

print "K = " + string(K)
print "C = " + string(K - 273.15)
print "F = " + string(K * 1.8 - 459.67)
print "R = " + string(K * 1.8)

```



## BBC BASIC


```bbcbasic

REPEAT
  INPUT "Kelvin degrees (>=0): " K
UNTIL K>=0
@%=&20208
PRINT '"K = " K
PRINT "C = " K - 273.15
PRINT "F = " K * 1.8 - 459.67
PRINT "R = " K * 1.8
END

```

```txt

Kelvin degrees (>=0): 21

K =    21.00
C =  -252.15
F =  -421.87
R =    37.80

```



## Befunge

The temperature to convert is read from stdin. Befunge has no support for real numbers, though, so reading and writing of decimal values is done with character I/O. For the same reason, the temperature calculations use integer arithmetic to emulate fixed point. The first two lines handle the input; the second line performs the conversion calculations; and the last three handle the output.


```befunge>0000
0p~>"."-:!#v_2-::0\`\9`+!#v_$1>/\:3`#v_\>\:3 \`#v_v
1#<<^0 /2++g001!<1 \+g00\+*+55\<   ^+55\-1<  ^*+55\+1<v_
"K"\-+**"!Y]"9:\"C"\--\**"^CIT"/5*9:\"F"\/5*9:\"R"\0\0<v
v/+55\+*86%+55: /+55\+*86%+55: \0/+55+5*-\1*2 p00:`\0:,<
>"."\>:55+% 68*v                       >:#,_$55+,\:!#@_^
   $_^#!:/+55\+<                       ^\" :"_<g00*95
```


```txt
21
K: 21.00
C: -252.15
F: -421.87
R: 37.80
```



## Bracmat


```bracmat
( ( rational2fixedpoint
  =   minus fixedpointnumber number decimals
    .   !arg:(#?number.~<0:~/#?decimals)
      & ( !number:0&"0.0"
        |     ( !number:>0&
              | -1*!number:?number&"-"
              )
            : ?minus
          & !number+1/2*10^(-1*!decimals):?number
          & !minus div$(!number.1) ".":?fixedpointnumber
          &   whl
            ' ( !decimals+-1:~<0:?decimals
              &     !fixedpointnumber
                    div$(mod$(!number.1)*10:?number.1)
                : ?fixedpointnumber
              )
          & str$!fixedpointnumber
        )
  )
& ( fixedpoint2rational
  =   integerpart fractionalpart decimals
    .   @( !arg
         :   #?integerpart
             ( "." ?fractionalpart
             | &0:?fractionalpart
             )
         )
      & @(!fractionalpart:? #?fractionalpart [?decimals)
      &   !integerpart
        +   (!integerpart:<0&-1|1)
          * 10^(-1*!decimals)
          * !fractionalpart
  )
&   whl
  ' ( put$"Enter Kelvin temperature:"
    & fixedpoint2rational$(get'(,STR)):?kelvin
    & !kelvin+-27315/100:?celcius
    & (degree=.str$(chu$(x2d$b0) !arg))
    & out$(rational2fixedpoint$(!kelvin.2) K)
    & out$(rational2fixedpoint$(!celcius.2) degree$C)
    & out$(rational2fixedpoint$(!celcius*9/5+32.2) degree$F)
    & out$(rational2fixedpoint$(!kelvin*9/5.2) degree$Ra)
    & out$(rational2fixedpoint$(!celcius*4/5.2) degree$Ré)
    )
& done!
)
```

```txt
Enter Kelvin temperature:21.00
21.00 K
-252.15 °C
-421.87 °F
37.80 °Ra
-201.72 °Ré
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

double kelvinToCelsius(double k){
    return k - 273.15;
}

double kelvinToFahrenheit(double k){
    return k * 1.8 - 459.67;
}

double kelvinToRankine(double k){
    return k * 1.8;
}
void convertKelvin(double kelvin) {
    printf("K %.2f\n", kelvin);
    printf("C %.2f\n", kelvinToCelsius(kelvin));
    printf("F %.2f\n", kelvinToFahrenheit(kelvin));
    printf("R %.2f", kelvinToRankine(kelvin));
}

int main(int argc, const char * argv[])
{
    if (argc > 1) {
        double kelvin = atof(argv[1]);
        convertKelvin(kelvin);
    }
    return 0;
}
```



## C++


```cpp

#include <iostream>
#include <iomanip>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class converter
{
public:
    converter() : KTC( 273.15f ), KTDel( 3.0f / 2.0f ), KTF( 9.0f / 5.0f ), KTNew( 33.0f / 100.0f ),
		  KTRank( 9.0f / 5.0f ), KTRe( 4.0f / 5.0f ), KTRom( 21.0f / 40.0f ) {}

    void convert( float kelvin )
    {
	float cel = kelvin - KTC,
	      del = ( 373.15f - kelvin ) * KTDel,
	      fah = kelvin * KTF - 459.67f,
	      net = cel * KTNew,
	      rnk = kelvin * KTRank,
	      rea = cel * KTRe,
	      rom = cel * KTRom + 7.5f;

	cout << endl << left
	     << "TEMPERATURES:" << endl
	     << "
### =========
" << endl << setw( 13 )
	     << "CELSIUS:" << cel << endl << setw( 13 )
	     << "DELISLE:" << del << endl << setw( 13 )
	     << "FAHRENHEIT:" << fah << endl << setw( 13 )
	     << "KELVIN:" << kelvin << endl  << setw( 13 )
	     << "NEWTON:" << net << endl << setw( 13 )
	     << "RANKINE:" << rnk << endl << setw( 13 )
	     << "REAUMUR:" << rea << endl << setw( 13 )
	     << "ROMER:" << rom << endl << endl << endl;
	}
private:
    const float KTRank, KTC, KTF, KTRe, KTDel, KTNew, KTRom;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    converter con;
    float k;
    while( true )
    {
	cout << "Enter the temperature in Kelvin to convert: ";
	cin >> k;
	con.convert( k );
	system( "pause" );
	system( "cls" );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------

```

```txt

Enter the temperature in Kelvin to convert: 373.15

TEMPERATURES:

### =========

CELSIUS:     100
DELISLE:     0
FAHRENHEIT:  212
KELVIN:      373.15
NEWTON:      33
RANKINE:     671.67
REAUMUR:     80
ROMER:       60


```


## C#

```c#
using System;

namespace TemperatureConversion
{
    class Program
    {
        static Func<double, double> ConvertKelvinToFahrenheit = x => (x * 1.8) - 459.67;
        static Func<double, double> ConvertKelvinToRankine = x => x * 1.8;
        static Func<double, double> ConvertKelvinToCelsius = x => x = 273.13;

        static void Main(string[] args)
        {
            Console.Write("Enter a Kelvin Temperature: ");
            string inputVal = Console.ReadLine();
            double kelvinTemp = 0f;

            if (double.TryParse(inputVal, out kelvinTemp))
            {
                Console.WriteLine(string.Format("Kelvin: {0}", kelvinTemp));
                Console.WriteLine(string.Format("Fahrenheit: {0}", ConvertKelvinToFahrenheit(kelvinTemp)));
                Console.WriteLine(string.Format("Rankine: {0}", ConvertKelvinToRankine(kelvinTemp)));
                Console.WriteLine(string.Format("Celsius: {0}", ConvertKelvinToCelsius(kelvinTemp)));
                Console.ReadKey();
            }
            else
            {
                Console.WriteLine("Invalid input value: " + inputVal);
            }
        }
    }
}
```



```txt

Enter a Kelvin Temperature: 21
Kelvin: 21
Fahrenheit: -421.87
Rankine: 37.8
Celsius: 273.13

```



## Ceylon


```ceylon
shared void run() {

	void printKelvinConversions(Float kelvin) {
		value celsius =	kelvin - 273.15;
		value rankine = kelvin * 9.0 / 5.0;
		value fahrenheit = rankine - 459.67;

		print("Kelvin:     ``formatFloat(kelvin, 2, 2)``
		       Celsius:    ``formatFloat(celsius, 2, 2)``
		       Fahrenheit: ``formatFloat(fahrenheit, 2, 2)``
		       Rankine:    ``formatFloat(rankine, 2, 2)``");
	}

	printKelvinConversions(21.0);

}
```



## Clojure

```clojure
(defn to-celsius [k]
  (- k 273.15))
(defn to-fahrenheit [k]
  (- (* k 1.8) 459.67))
(defn to-rankine [k]
  (* k 1.8))

(defn temperature-conversion [k]
  (if (number? k)
    (format "Celsius: %.2f Fahrenheit: %.2f Rankine: %.2f"
      (to-celsius k) (to-fahrenheit k) (to-rankine k))
    (format "Error: Non-numeric value entered.")))
```


```txt

user=> (temperature-conversion 21.0)
"Celsius: -252.15 Fahrenheit: -421.87 Rankine: 37.80"

```



## COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. temp-conversion.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  Kelvin-Rankine-Ratio    VALUE 0.5556. *> 5 / 9 to 4 d.p.
       78  Kelvin-Celsius-Diff     VALUE 273.15.
       78  Rankine-Fahrenheit-Diff VALUE 459.67.

       01  temp-kelvin             PIC S9(8)V99.
       01  temp-rankine            PIC S9(8)V99.

       01  kelvin                  PIC -(7)9.99.
       01  celsius                 PIC -(7)9.99.
       01  rankine                 PIC -(7)9.99.
       01  fahrenheit              PIC -(7)9.99.

       PROCEDURE DIVISION.
           DISPLAY "Enter a temperature in Kelvin to convert: " NO ADVANCING
           ACCEPT temp-kelvin

           MOVE temp-kelvin TO kelvin
           DISPLAY "K " kelvin

           SUBTRACT Kelvin-Celsius-Diff FROM temp-kelvin GIVING celsius
           DISPLAY "C " celsius

           DIVIDE temp-kelvin BY Kelvin-Rankine-Ratio
               GIVING temp-rankine, rankine
           SUBTRACT Rankine-Fahrenheit-Diff FROM temp-rankine GIVING fahrenheit

           DISPLAY "F " fahrenheit
           DISPLAY "R " rankine

           GOBACK
           .
```


```txt

Enter a temperature in Kelvin to convert: 21
K       21.00
C     -252.15
F     -421.88
R       37.79

```



## Common Lisp

Three functions define the necessary conversion formulas. A fancy format string is used to print these values.


```lisp

(defun to-celsius (k)
  (- k 273.15))
(defun to-fahrenheit (k)
  (- (* k 1.8) 459.67))
(defun to-rankine (k)
  (* k 1.8))

(defun temperature-conversion ()
  (let ((k (read)))
    (if (numberp k)
      (format t "Celsius: ~d~%Fahrenheit: ~d~%Rankine: ~d~%"
        (to-celsius k) (to-fahrenheit k) (to-rankine k))
      (format t "Error: Non-numeric value entered."))))

```


```txt

* (temperature-conversion)
21
Celsius: -252.15
Fahrenheit: -421.87003
Rankine: 37.8
NIL

```



## D


```d
double kelvinToCelsius(in double k) pure nothrow @safe {
    return k - 273.15;
}

double kelvinToFahrenheit(in double k) pure nothrow @safe {
    return k * 1.8 - 459.67;
}

double kelvinToRankine(in double k) pure nothrow @safe {
    return k * 1.8;
}

unittest {
    import std.math: approxEqual;
    assert(approxEqual(kelvinToCelsius(21.0), -252.15));
    assert(approxEqual(kelvinToFahrenheit(21.0), -421.87));
    assert(approxEqual(kelvinToRankine(21.0), 37.8));
}

void main(string[] args) {
    import std.stdio, std.conv, std.string;

    if (args.length == 2 && isNumeric(args[1])) {
        immutable kelvin = to!double(args[1]);
        if (kelvin >= 0) {
            writefln("K  %2.2f", kelvin);
            writefln("C  %2.2f", kelvinToCelsius(kelvin));
            writefln("F  %2.2f", kelvinToFahrenheit(kelvin));
            writefln("R  %2.2f", kelvinToRankine(kelvin));
        } else
            writefln("%2.2f K is below absolute zero", kelvin);
    }
}
```

```txt

K  21.00

C  -252.15

F  -421.87

R  37.80

```



## Delphi


```delphi

program Temperature;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  TTemp = class
  private
    fCelsius, fFahrenheit, fRankine: double;
  public
    constructor Create(aKelvin: double);
    property AsCelsius: double read fCelsius;
    property AsFahrenheit: double read fFahrenheit;
    property AsRankine: double read fRankine;
  end;

  { TTemp }

constructor TTemp.Create(aKelvin: double);
begin
  fCelsius := aKelvin - 273.15;
  fRankine := aKelvin * 9 / 5;
  fFahrenheit := fRankine - 459.67;
end;

var
  kelvin: double;
  temp: TTemp;

begin
  write('Kelvin: ');
  readln(kelvin);
  temp := TTemp.Create(kelvin);
  writeln(Format('Celsius: %.2f', [temp.AsCelsius]));
  writeln(Format('Fahrenheit: %.2f', [temp.AsFahrenheit]));
  writeln(Format('Rankine: %.2f', [temp.AsRankine]));
  temp.Free;
  readln;
end.

```

```txt

Kelvin: 21.00
Celsius: -252.15
F: -421.87
R: 37.80

```


## EasyLang


<lang>floatvars
k = numberf input
print k & "° K"
print k - 273.15 & "° C"
print k * 1.8 - 459.67 & "° F"
print k * 1.8 & "° R"
```



## Elena

ELENA 4.1 :

```elena
import extensions;

convertKelvinToFahrenheit(x)
    = x * 1.8r - 459.6r;

convertKelvinToRankine(x)
    = x * 1.8r;

convertKelvinToCelsius(x)
    = x - 273.15r;

public program()
{
    console.print("Enter a Kelvin Temperature: ");
    var inputVal := console.readLine();
    real kelvinTemp := 0.0r;
    try
    {
        kelvinTemp := realConvertor.convert(inputVal)
    }
    catch(Exception e)
    {
        console.printLine("Invalid input value: ", inputVal);

        AbortException.raise()
    };

    console.printLine("Kelvin: ", kelvinTemp);
    console.printLine("Fahrenheit: ", convertKelvinToFahrenheit(kelvinTemp));
    console.printLine("Rankine: ", convertKelvinToRankine(kelvinTemp));
    console.printLine("Celsius: ", convertKelvinToCelsius(kelvinTemp));
    console.readChar()
}
```

```txt

Enter a Kelvin Temperature: 21
Kelvin: 21.0
Fahrenheit: -421.87
Rankine: 37.8
Celsius: -252.15

```



## Elixir


```elixir
defmodule Temperature do
  def conversion(t) do
    IO.puts "K : #{f(t)}"
    IO.puts "\nC : #{f(t - 273.15)}"
    IO.puts "\nF : #{f(t * 1.8 - 459.67)}"
    IO.puts "\nR : #{f(t * 1.8)}"
  end

  defp f(a) do
    Float.round(a, 2)
  end

  def task, do: conversion(21.0)
end

Temperature.task
```


```txt

K : 21.0

C : -252.15

F : -421.87

R : 37.8

```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(temp_conv).
-export([main/0]).

main() ->
	conversion(21).

conversion(T) ->
	io:format("\nK : ~p\n\n",[f(T)]),
	io:format("C : ~p \n\n",[f(T - 273.15)]),
	io:format("F : ~p\n\n",[f(T * 1.8 - 459.67)]),
	io:format("R : ~p\n\n",[f(T * 1.8)]).

f(A) ->
	(round(A*100))/100 .

```

```txt

K : 21.0

C : -252.15

F : -421.87

R : 37.8

ok

```



## Euphoria


```OpenEuphoria

include std/console.e

atom K
while 1 do
	K = prompt_number("Enter temperature in Kelvin >=0: ",{0,4294967296})
	printf(1,"K = %5.2f\nC = %5.2f\nF = %5.2f\nR = %5.2f\n\n",{K,K-273.15,K*1.8-459.67,K*1.8})
end while

```

```txt

Enter temperature in Kelvin >=0: 21
K = 21.00
C = -252.15
F = -421.87
R = 37.80

Enter temperature in Kelvin >=0:

```



## Excel

<lang>A1 : Kelvin
B1 : Celsius
C1 : Fahrenheit
D1 : Rankine
Name A2 : K
B2 : =K-273.15
C2 : =K*1.8-459.67
D2 : =K*1.8
Input in A1
```

```txt

  A          B          C          D
1 Kelvin     Celsius    Fahrenheit Rankine
2         21    -252.15    -421.87       37.8

```



## Ezhil


```Ezhil

# convert from Kelvin
நிரல்பாகம் கெல்வின்_இருந்து_மாற்று( k )
 பதிப்பி "Kelvin: ",k,"Celsius: ",round(k-273.15)," Fahrenheit: ",(round(k*1.8 - 459.67))," Rankine: ",(round(k*1.8))
முடி

கெல்வின்_இருந்து_மாற்று( 0 ) #absolute zero
கெல்வின்_இருந்து_மாற்று( 273 ) #freezing pt of water
கெல்வின்_இருந்து_மாற்று( 30 + 273 ) #room temperature in Summer

```


=={{header|F_Sharp|F#}}==

```fsharp

// Define units of measure
[<Measure>] type k
[<Measure>] type f
[<Measure>] type c
[<Measure>] type r

// Define conversion functions
let kelvinToCelsius (t : float<k>) = ((float t) - 273.15) * 1.0<c>
let kelvinToFahrenheit (t : float<k>) = (((float t) * 1.8) - 459.67) * 1.0<f>
let kelvinToRankine (t : float<k>) = ((float t) * 1.8) * 1.0<r>

// Example code
let K = 21.0<k>
printfn "%A Kelvin is %A Celsius" K (kelvinToCelsius K)
printfn "%A Kelvin is %A Fahrenheit" K (kelvinToFahrenheit K)
printfn "%A Kelvin is %A Rankine" K (kelvinToRankine K)

```



## Factor

<lang>USING: combinators formatting kernel math ;
IN: rosetta-code.temperature

: k>c ( kelvin -- celsius )    273.15 - ;
: k>r ( kelvin -- rankine )    9/5 * ;
: k>f ( kelvin -- fahrenheit ) k>r 459.67 - ;

: convert ( kelvin -- )
    { [ ] [ k>c ] [ k>f ] [ k>r ] } cleave
    "K  %.2f\nC  %.2f\nF  %.2f\nR  %.2f\n" printf ;

21 convert
```

```txt

K  21.00
C  -252.15
F  -421.87
R  37.80

```



## FOCAL


```focal
01.10 ASK "TEMPERATURE IN KELVIN", K
01.20 TYPE "K ", %6.02, K, !
01.30 TYPE "C ", %6.02, K - 273.15, !
01.40 TYPE "F ", %6.02, K * 1.8 - 459.67, !
01.50 TYPE "R ", %6.02, K * 1.8, !
```

```txt
TEMPERATURE IN KELVIN:373.15
K =  373.15
C =  100.00
F =  212.00
R =  671.67
```



## Forth

{{works with|GNU Forth}} for the command line handling

```forth>: k
°C  ( F: kelvin  -- celsius )     273.15e0 f- ;
: k>°R  ( F: kelvin  -- rankine )     1.8e0 f* ;
: °R>°F ( F: rankine -- fahrenheit )  459.67e0 f- ;
: k>°F  ( F: kelvin  -- fahrenheit )  k>°R °R>°F ;
: main
   argc 1 > if  1 arg >float
      fdup      f. ." K"  cr
      fdup k>°C f. ." °C" cr
      fdup k>°F f. ." °F" cr
      fdup k>°R f. ." °R" cr
   then ;

main bye
```

```txt
&gt; gforthamd64 rosetta_temp_conv.fs 21
21. K
-252.15 °C
-421.87 °F
37.8 °R
```


## Fortran

```fortran
Program Temperature
  implicit none

  real :: kel, cel, fah, ran

  write(*,*) "Input Kelvin temperature to convert"
  read(*,*) kel

  call temp_convert(kel, cel, fah, ran)
  write(*, "((a10), f10.3)") "Kelvin", kel
  write(*, "((a10), f10.3)") "Celsius", cel
  write(*, "((a10), f10.3)") "Fahrenheit", fah
  write(*, "((a10), f10.3)") "Rankine", ran

contains

subroutine temp_convert(kelvin, celsius, fahrenheit, rankine)
  real, intent(in)  :: kelvin
  real, intent(out) :: celsius, fahrenheit, rankine

  celsius = kelvin - 273.15
  fahrenheit = kelvin * 1.8 - 459.67
  rankine = kelvin * 1.8

end subroutine
end program
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub convKelvin(temp As Double)
  Dim f As String = "####.##"
  Print Using f; temp;
  Print " degrees Kelvin"
  Print Using f; temp - 273.15;
  Print " degrees Celsius"
  Print Using f; (temp - 273.15) * 1.8 + 32.0;
  Print " degrees Fahreneit"
  Print Using f; (temp - 273.15) * 1.8 + 32.0 + 459.67;
  Print " degrees Rankine"
End Sub

convKelvin(0.0)
Print
convKelvin(21.0)
Print
Print "Press any key to quit"
Sleep
```


```txt

   0.00 degrees Kelvin
-273.15 degrees Celsius
-459.67 degrees Fahreneit
   0.00 degrees Rankine

  21.00 degrees Kelvin
-252.15 degrees Celsius
-421.87 degrees Fahreneit
  37.80 degrees Rankine

```



## Gambas


```gambas
Public Sub Form_Open()
Dim fKelvin As Float

fKelvin = InputBox("Enter a Kelvin value", "Kelvin converter")

Print "Kelvin =\t" & Format(Str(fKelvin), "#.00")
Print "Celsius =\t" & Format(Str(fKelvin - 273.15), "#.00")
Print "Fahrenheit =\t" & Format(Str(fKelvin * 1.8 - 459.67), "#.00")
Print "Rankine =\t" & Format(Str(fKelvin * 1.8), "#.00")

End
```

Output:

```txt

Kelvin =        21.00
Celsius =       -252.15
Fahrenheit =    -421.87
Rankine =       37.80

```



## Go


```go
package main

import (
    "fmt"
    "os"
    "strconv"
)

func main() {
    if len(os.Args) != 2 {
        fmt.Println("Usage: k <Kelvin>")
        return
    }
    k, err := strconv.ParseFloat(os.Args[1], 64)
    if err != nil {
        fmt.Println(err)
        return
    }
    if k < 0 {
        fmt.Println("Kelvin must be >= 0.")
        return
    }
    fmt.Printf("K  %.2f\n", k)
    fmt.Printf("C  %.2f\n", k-273.15)
    fmt.Printf("F  %.2f\n", k*9/5-459.67)
    fmt.Printf("R  %.2f\n", k*9/5)
}
```

```txt

> k 21
K  21.00
C  -252.15
F  -421.87
R  37.80

```



## Groovy


```Groovy

class Convert{
static void main(String[] args){
def c=21.0;
println("K "+c)
println("C "+k_to_c(c));
println("F "+k_to_f(k_to_c(c)));
println("R "+k_to_r(c));
}
static def k_to_c(def k=21.0){return k-273.15;}
static def k_to_f(def k=21.0){return ((k*9)/5)+32;}
static def k_to_r(def k=21.0){return k*1.8;}
}

```

```txt

K 21.0
C -252.15
F -421.87
R 37.80

```



## Haskell



```haskell
import System.Exit (die)
import Control.Monad (mapM_)

main = do
  putStrLn "Please enter temperature in kelvin: "
  input <- getLine
  let kelvin = read input
  if kelvin < 0.0
      then die "Temp cannot be negative"
      else mapM_ putStrLn $ convert kelvin

convert :: Double -> [String]
convert n = zipWith (++) labels nums
    where labels      = ["kelvin: ", "celcius: ", "farenheit: ", "rankine: "]
          conversions = [id, subtract 273, subtract 459.67 . (1.8 *), (*1.8)]
          nums        = (show . ($n)) <$> conversions
```


Or with properly managed exceptions:


```haskell
{-# LANGUAGE LambdaCase #-}

import System.Exit (die)
import Control.Monad (mapM_)
import Control.Error.Safe (tryAssert, tryRead)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except

main = putStrLn "Please enter temperature in kelvin: " >>
       runExceptT getTemp >>=
       \case Right x  -> mapM_ putStrLn $ convert x
             Left err -> die err

convert :: Double -> [String]
convert n = zipWith (++) labels nums
    where labels      = ["kelvin: ", "celcius: ", "farenheit: ", "rankine: "]
          conversions = [id, subtract 273, subtract 459.67 . (1.8 *), (1.8 *)]
          nums        = (show . ($ n)) <$> conversions

getTemp :: ExceptT String IO Double
getTemp = do
    t <- liftIO getLine >>= tryRead "Could not read temp"
    tryAssert "Temp cannot be negative" (t>=0)
    return t
```


=={{header|Icon}} and {{header|Unicon}}==

The following program works in both languages:

```unicon
procedure main(A)
    k := A[1] | 21.00
    write("K ",k)
    write("C ",k-273.15)
    write("R ",r := k*(9.0/5.0))
    write("F ",r - 459.67)
end
```


Sample runs:


```txt

->tc
K 21.0
C -252.15
R 37.8
F -421.87
->tc 273.15
K 273.15
C 0.0
R 491.67
F 32.0
->

```



## J

'''Solution''':
```j
   NB.  Temp conversions are all linear polynomials
   K2K    =:     0    1    NB. K = (1  *k) +   0
   K2C    =:  _273    1    NB. C = (1  *k) - 273
   K2F    =:  _459.67 1.8  NB. F = (1.8*k) - 459.67
   K2R    =:     0    1.8  NB. R = (1.8*k) +   0

   NB.  Do all conversions at once (eval
   NB.  polynomials in parallel). This is the
   NB.  numeric matrix J programs would manipulate
   NB.  directly.
   k2KCFR =:  (K2K , K2C , K2F ,: K2R) p./ ]
```

```j
   NB. Format matrix for printing & tag each
   NB. temp with scale, for human legibility
   fmt    =:  [: (;:inv"1) 0 _1 |: 'KCFR' ;"0 1"_1 '0.2' 8!:0 ]
   kcfr   =:  fmt@k2KCFR

   kcfr 21
K   21.00
C -252.00
F -421.87
R   37.80

   kcfr 0 NB. Absolute zero
K    0.00
C -273.00
F -459.67
R    0.00

   kcfr 21 100 300  NB. List of temps works fine
K   21.00  100.00  300.00
C -252.00 -173.00   27.00
F -421.87 -279.67   80.33
R   37.80  180.00  540.00
```

'''Notes''': The approach is founded on polynomials, one for each conversion (e.g. <tt>Fahrenheit  = 1.8*x - 459.67</tt> where <tt>x</tt> is measured in degrees Kelvin), and all polynomials are evaluated simultaneously using the built-in <code>p.</code>. Through some code decorations (specifically the <code>/</code> in <code>p./</code> the <code>"0 1"_1</code> and the <code>0 _1 |:</code>), we permit our function to convert arrays of temperatures of arbitrarily high dimension (a single temp, lists of temps, tables of temps, cubes of temps, etc).


## Java


```java
public class TemperatureConversion {
    public static void main(String args[]) {
        if (args.length == 1) {
            try {
                double kelvin = Double.parseDouble(args[0]);
                if (kelvin >= 0) {
                    System.out.printf("K  %2.2f\n", kelvin);
                    System.out.printf("C  %2.2f\n", kelvinToCelsius(kelvin));
                    System.out.printf("F  %2.2f\n", kelvinToFahrenheit(kelvin));
                    System.out.printf("R  %2.2f\n", kelvinToRankine(kelvin));
                } else {
                    System.out.printf("%2.2f K is below absolute zero", kelvin);
                }
            } catch (NumberFormatException e) {
                System.out.println(e);
            }
        }
    }

    public static double kelvinToCelsius(double k) {
        return k - 273.15;
    }

    public static double kelvinToFahrenheit(double k) {
        return k * 1.8 - 459.67;
    }

    public static double kelvinToRankine(double k) {
        return k * 1.8;
    }
}
```

```txt

K  21.00

C  -252.15

F  -421.87

R  37.80

```



## JavaScript



### ES5


```javascript>var k2c = k =
 k - 273.15
var k2r = k => k * 1.8
var k2f = k => k2r(k) - 459.67

Number.prototype.toMaxDecimal = function (d) {
	return +this.toFixed(d) + ''
}

function kCnv(k) {
	document.write( k,'K° = ', k2c(k).toMaxDecimal(2),'C° = ', k2r(k).toMaxDecimal(2),'R° = ', k2f(k).toMaxDecimal(2),'F°
' )
}

kCnv(21)
kCnv(295)
```

```txt

21K° = -252.15C° = 37.8R° = -421.87F°
295K° = 21.85C° = 531R° = 71.33F°

```



### ES6


Deriving '''kelvinTranslations()''' from a more general '''heatBabel()''' function.


```javascript
(() => {
    'use strict';

    let kelvinTranslations = k => ['K', 'C', 'F', 'R']
        .map(x => [x, heatBabel(k, 'K', x)]);

    // heatBabel :: Num -> ScaleName -> ScaleName -> Num
    let heatBabel = (n, strFromScale, strToScale) => {
        let ratio = 9 / 5,
            cels = 273.15,
            fahr = 459.67,
            id = x => x,
            readK = {
                k: id,
                c: x => cels + x,
                f: x => (fahr + x) * ratio,
                r: x => x / ratio
            },
            writeK = {
                k: id,
                c: x => x - cels,
                f: x => (x * ratio) - fahr,
                r: x => ratio * x
            };

        return writeK[strToScale.charAt(0).toLowerCase()](
            readK[strFromScale.charAt(0).toLowerCase()](n)
        ).toFixed(2);
    };


    // TEST
    return kelvinTranslations(21)
        .map(([s, n]) => s + ('          ' + n)
            .slice(-10))
        .join('\n');

})();

```


```txt
K     21.00
C   -252.15
F   -421.87
R     37.80
```



## jq

The hard part here is defining round/1 generically.
```jq

# round(keep) takes as input any jq (i.e. JSON) number and emits a string.
# "keep" is the desired maximum number of numerals after the decimal point,
# e.g. 9.999|round(2) => 10.00
def round(keep):
  tostring
  | (index("e") | if . then . else index("E") end) as $e
  | if $e then (.[0:$e] | round(keep)) + .[$e+1:]
    else index(".") as $ix
      | if $ix == null then .
        else .[0:$ix + 1] as $head
          | .[$ix+1:$ix+keep+2] as $tail
          | if ($tail|length) <= keep then $head + $tail
            else ($tail | .[length-1:] | tonumber) as $last
              | if $last < 5 then  $head + $tail[0:$tail|length - 1]
                else (($head + $tail) | length) as $length
                  | ($head[0:-1] + $tail)
                  | (tonumber +  (if $head[0:1]=="-" then -5 else 5 end))
                  | tostring
                  | .[0: ($ix+1+length-$length)] + "." + .[length-keep-1:-1]
                end
            end
        end
    end;

def k2c: . - 273.15;
def k2f: . * 1.8 - 459.67;
def k2r: . * 1.8;

# produce a stream
def cfr:
  if . >= 0
  then "Kelvin: \(.)", "Celsius: \(k2c|round(2))",
       "Fahrenheit: \(k2f|round(2))", "Rankine: \(k2r|round(2))"
  else error("cfr: \(.) is an invalid temperature in degrees Kelvin")
  end;

cfr
```

'''Example'''

```sh
 $ jq -M -r -f Temperature_conversion.jq
  21
  Kelvin: 21
  Celsius: -252.15
  Fahrenheit: -421.87
  Rankine: 37.80

  -1
  jq: error: cfr: -1 is an invalid temperature in degrees Kelvin
```



## Julia


```julia
cfr(k) = print("Kelvin: $k, ",
  "Celsius: $(round(k-273.15,2)), ",
  "Fahrenheit: $(round(k*1.8-459.67,2)), ",
  "Rankine: $(round(k*1.8,2))")
```


```txt
julia> cfr(21)
Kelvin: 21, Celsius: -252.15, Fahrenheit: -421.87, Rankine: 37.8
```



## Kotlin


```scala
// version 1.1.2

class Kelvin(val degrees: Double) {
    fun toCelsius() = degrees - 273.15

    fun toFahreneit() = (degrees - 273.15) * 1.8 + 32.0

    fun toRankine() = (degrees - 273.15) * 1.8 + 32.0 + 459.67
}

fun main(args: Array<String>) {
    print("Enter the temperature in degrees Kelvin : ")
    val degrees = readLine()!!.toDouble()
    val k = Kelvin(degrees)
    val f = "% 1.2f"
    println()
    println("K  ${f.format(k.degrees)}\n")
    println("C  ${f.format(k.toCelsius())}\n")
    println("F  ${f.format(k.toFahreneit())}\n")
    println("R  ${f.format(k.toRankine())}")
}
```


```txt

Enter the temperature in degrees Kelvin : 21

K   21.00

C  -252.15

F  -421.87

R   37.80

```



## Lasso


```Lasso
define tempconverter(temp, kind) => {

	local(
		_temp		= decimal(#temp),
		convertratio	= 1.8,
		k_c		= 273.15,
		r_f		= 459.67,
		k,c,r,f
	)

	match(#kind) => {
		case('k')
			#k = #_temp
			#c = -#k_c + #k
			#r = #k * #convertratio
			#f = -#r_f + #r
		case('c')
			#c = #_temp
			#k = #k_c + #c
			#r = #k * #convertratio
			#f = -#r_f + #r
		case('r')
			#r = #_temp
			#f = -#r_f + #r
			#k = #r / #convertratio
			#c = -#k_c + #k
		case('f')
			#f = #_temp
			#r = #r_f + #f
			#k = #r / #convertratio
			#c = -#k_c + #k
		case
			return 'Something wrong'
	}

	return ('K = ' + #k -> asstring(-precision = 2) +
		' C = ' + #c -> asstring(-precision = 2) +
		' R = ' + #r -> asstring(-precision = 2) +
		' F = ' + #f -> asstring(-precision = 2)
		)
}

tempconverter(21, 'k')
'<br />'
tempconverter(21, 'c')
'<br />'
tempconverter(-41, 'c')
'<br />'
tempconverter(37.80, 'r')
'<br />'
tempconverter(69.80, 'f')
```


```txt
K = 21.00 C = -252.15 R = 37.80 F = -421.87
K = 294.15 C = 21.00 R = 529.47 F = 69.80
K = 232.15 C = -41.00 R = 417.87 F = -41.80
K = 21.00 C = -252.15 R = 37.80 F = -421.87
K = 294.15 C = 21.00 R = 529.47 F = 69.80
```



## LIL


```tcl
# Temperature conversion, in LIL
func kToc k {expr $k - 273.15}
func kTor k {expr $k / 5.0 * 9.0}
func kTof k {expr [kTor $k] - 469.67}

write "Enter kelvin temperatures or just enter to quit: "
for {set k [readline]} {![streq $k {}]} {set k [readline]} {
    print "Kelvin:     $k"
    print "Celsius:    [kToc $k]"
    print "Fahrenheit: [kTof $k]"
    print "Rankine:    [kTor $k]"
}
```


```txt
prompt$ lil temperatureConversion.lil
Enter kelvin temperatures or just enter to quit: 21
Kelvin:     21
Celsius:    -252.150000
Fahrenheit: -431.870000
Rankine:    37.800000
```



## LiveCode


```LiveCode
function convertDegrees k
    put k/5 * 9 into r
    put k - 273.15 into c
    put r - 459.67 into f
    return k,r,c,f
end convertDegrees
```

Example
```LiveCode
put convertDegrees(21.00) into tTemp
put item 1 of tTemp into temperature["Kelvin"]
put item 2 of tTemp into temperature["Rankine"]
put item 3 of tTemp into temperature["Celsius"]
put item 4 of tTemp into temperature["Fahrenheit"]
combine temperature using comma and colon
put temperature

-- Celsius:-252.15,Fahrenheit:-421.87,Kelvin:21.00,Rankine:37.8
```



## Lua



```lua
function convert_temp(k)
    local c = k - 273.15
    local r = k * 1.8
    local f = r - 459.67
    return k, c, r, f
end

print(string.format([[
Kelvin: %.2f K
Celcius: %.2f °C
Rankine: %.2f °R
Fahrenheit: %.2f °F
]],convert_temp(21.0)))
```



## Maple


```maple
tempConvert := proc(k)
	seq(printf("%c: %.2f\n", StringTools[UpperCase](substring(i, 1)), convert(k, temperature, kelvin, i)), i in [kelvin, Celsius, Fahrenheit, Rankine]);
	return NULL;
end proc:

tempConvert(21);
```

```txt

K: 21.00
C: -252.15
F: -421.87
R: 37.80

```



## Mathematica


```Mathematica
tempConvert[t_] :=
Grid[Transpose@{{"K", "C", "F", "R"},
Round[{t, t - 273.15, 9 t/5 - 459.67, 9 t/5}, .01]}]

tempConvert[21]
```

```txt

K	21.
C	-252.15
F	-421.87
R	37.8

```



## min

```min
(
  ((float) (273.15 -) (9 5 / * 459.67 -) (9 5 / *)) cleave
  () 'cons 4 times "K  $1\nC  $2\nF  $3\nR  $4" swap % puts!
) :convert

21 convert
```

```txt

K  21.0
C  -252.15
F  -421.87
R  37.8

```



## MiniScript


```MiniScript
fromKelvin = function(temp)
	print temp + " degrees in Kelvin is :-"
	Celsius = temp - 273.15
	print Celsius + " degrees Celsius"
	Fahrenheit = round(Celsius  * 9/5 + 32,2)
	print Fahrenheit + " degrees Fahrenheit"
	Rankine = Fahrenheit + 459.67
	print Rankine + " degrees Rankine"
end function

temp = input("enter a temperature in Kelvin: ")
fromKelvin temp.val
```

```txt

enter a temperature in Kelvin: 273.15
273.15 degrees in Kelvin is :-
0 degrees Celsius
32 degrees Fahrenheit
491.67 degrees Rankine

enter a temperature in Kelvin: 300
300 degrees in Kelvin is :-
26.85 degrees Celsius
80.33 degrees Fahrenheit
540 degrees Rankine

```



## MiniZinc


```MiniZinc
float: kelvin;

var float: celsius;
var float: fahrenheit;
var float: rankine;

constraint celsius == kelvin - 273.15;
constraint fahrenheit == celsius * 1.8 + 32;
constraint rankine == fahrenheit + 459.67;
solve satisfy;

output ["K \(kelvin)\n", "C \(celsius)\n", "F \(fahrenheit)\n", "R \(rankine)\n"];
```

```txt

Compiling temperature.mzn, additional arguments kelvin=1000;
Running temperature.mzn
K 1000.0
C 726.850000000001
F 1340.33
R 1800.0
----------
Finished in 62msec

```


=={{header|MK-61/52}}==

```mk61
П7	0	,	8	*	П8	ИП7	9	*	5
/	3	2	+	П9	ИП7	2	7	3	,
1	5	+	П4	С/П	П8	1	,	8	/
БП	00	П9	3	2	-	5	*	9	/
БП	00	П4	2	7	3	,	1	5	-
БП	00
```


''Instruction:''

tºC = РX В/О С/П;

tºRa = РX БП 25 С/П;

tºF = РX БП 32 С/П;

tK = РX БП 42 С/П;

''Result:''

РX = Р4 = tK;

Р7 = tºC;

Р8 = tºRa;

Р9 = tºF.


## ML

=
## mLite
=
Temperature in Kelvin given on command line.

```ocaml
fun KtoC n = n - 273.15;
fun KtoF n = n * 1.8 - 459.67;
fun KtoR n = n * 1.8;
val K = argv 0;

if K = false then
	println "mlite -f temcon.m <temp>"
else
	let
		val K = ston K
	in
		print "Kelvin:     "; println K;
		print "Celcius:    "; println ` KtoC K;
		print "Fahrenheit: "; println ` KtoF K;
		print "Rankine:    "; println ` KtoR K
	end

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols

numeric digits 20

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/*
  +  Kelvin               Celsius        Fahrenheit      Rankine             Delisle        Newton            Réaumur        Rømer
  K  T                    T-273.15       T*9/5-459.67    T*9/5               (373.15-T)*3/2 (T-273.15)*33/100 (T-273.15)*4/5 (T-273.15)*21/40+7.5
  C  T+273.15             T              T*9/5+32        (T+273.15)*9/5      (100-T)*3/2    T*33/100          T*4/5          T*21/40+7.5
  F  (T+459.67)*5/9       (T-32)*5/9     T               T+459.67            (212-T)*5/6    (T-32)*11/60      (T-32)*4/9     (T-32)*7/24+7.5
  R  T*5/9                (T-491.67)*5/9 T-459.67        T                   (671.67-T)*5/6 (T-491.67)*11/60  (T-491.67)*4/9 (T-491.67)*7/24+7.5
  De 373.15-T*2/3         100-T*2/3      212-T*6/5       671.67-T*6/5        T              33-T*11/50        80-T*8/15      60-T*7/20
  N  T*100/33+273.15      T*100/33       T*60/11+32      T*60/11+491.67      (33-T)*50/11   T                 T*80/33        T*35/22+7.5
  Ré T*5/4+273.15         T*5/4          T*9/4+32        T*9/4+491.67        (80-T)*15/8    T*33/80           T              T*21/32+7.5
  Rø (T-7.5)*40/21+273.15 (T-7.5)*40/21  (T-7.5)*24/7+32 (T-7.5)*24/7+491.67 (60-T)*20/7    (T-7.5)*22/35     (T-7.5)*32/21  T
 */
method temperatureConversion(scaleFrom, scaleTo, T) public static

  parse 'KELVIN CELSIUS FAHRENHEIT RANKINE DELISLE NEWTON REAUMUR ROEMER' -
         KELVIN CELSIUS FAHRENHEIT RANKINE DELISLE NEWTON REAUMUR ROEMER .
  scaleFrom = scaleFrom.upper()
  scaleTo   = scaleTo.upper()
  select label sF case scaleFrom
    when KELVIN then do
      select case scaleTo
        when KELVIN     then val = T
        when CELSIUS    then val = T - 273.15
        when FAHRENHEIT then val = T * 9 / 5 - 459.67
        when RANKINE    then val = T * 9 / 5
        when DELISLE    then val = (373.15 - T) * 3 / 2
        when NEWTON     then val = (T - 273.15) * 33 / 100
        when REAUMUR    then val = (T - 273.15) * 4 / 5
        when ROEMER     then val = (T - 273.15) * 21 / 40 + 7.5
        otherwise       signal IllegalArgumentException(scaleFrom',' scaleTo',' T)
        end
      end
    when CELSIUS then do
      select case scaleTo
        when KELVIN     then val = T + 273.15
        when CELSIUS    then val = T
        when FAHRENHEIT then val = T * 9 / 5 + 32
        when RANKINE    then val = (T + 273.15) * 9 / 5
        when DELISLE    then val = (100 - T) * 3 / 2
        when NEWTON     then val = T * 33 / 100
        when REAUMUR    then val = T * 4 / 5
        when ROEMER     then val = T * 21 / 40 + 7.5
        otherwise       signal IllegalArgumentException(scaleFrom',' scaleTo',' T)
        end
      end
    when FAHRENHEIT then do
      select case scaleTo
        when KELVIN     then val = (T + 459.67) * 5 / 9
        when CELSIUS    then val = (T - 32) * 5 / 9
        when FAHRENHEIT then val = T
        when RANKINE    then val = T + 459.67
        when DELISLE    then val = (212 - T) * 5 / 6
        when NEWTON     then val = (T - 32) * 11 / 60
        when REAUMUR    then val = (T - 32) * 4 / 9
        when ROEMER     then val = (T - 32) * 7 / 24 + 7.5
        otherwise       signal IllegalArgumentException(scaleFrom',' scaleTo',' T)
        end
      end
    when RANKINE then do
      select case scaleTo
        when KELVIN     then val = T * 5 / 9
        when CELSIUS    then val = (T - 491.67) * 5 / 9
        when FAHRENHEIT then val = T - 459.67
        when RANKINE    then val = T
        when DELISLE    then val = (671.67 - T) * 5 / 6
        when NEWTON     then val = (T - 491.67) * 11 / 60
        when REAUMUR    then val = (T - 491.67) * 4 / 9
        when ROEMER     then val = (T - 491.67) * 7 / 24 + 7.5
        otherwise       signal IllegalArgumentException(scaleFrom',' scaleTo',' T)
        end
      end
    when DELISLE then do
      select case scaleTo
        when KELVIN     then val = 373.15 - T * 2 / 3
        when CELSIUS    then val = 100 - T * 2 / 3
        when FAHRENHEIT then val = 212 - T * 6 / 5
        when RANKINE    then val = 671.67 - T * 6 / 5
        when DELISLE    then val = T
        when NEWTON     then val = 33 - T * 11 / 50
        when REAUMUR    then val = 80 - T * 8 / 15
        when ROEMER     then val = 60 - T * 7 / 20
        otherwise       signal IllegalArgumentException(scaleFrom',' scaleTo',' T)
        end
      end
    when NEWTON then do
      select case scaleTo
        when KELVIN     then val = T * 100 / 33 + 273.15
        when CELSIUS    then val = T * 100 / 33
        when FAHRENHEIT then val = T * 60 / 11 + 32
        when RANKINE    then val = T * 60 / 11 + 491.67
        when DELISLE    then val = (33 - T) * 50 / 11
        when NEWTON     then val = T
        when REAUMUR    then val = T * 80 / 33
        when ROEMER     then val = T * 35 / 22 + 7.5
        otherwise       signal IllegalArgumentException(scaleFrom',' scaleTo',' T)
        end
      end
    when REAUMUR then do
      select case scaleTo
        when KELVIN     then val = T * 5 / 4 + 273.15
        when CELSIUS    then val = T * 5 / 4
        when FAHRENHEIT then val = T * 9 / 4 + 32
        when RANKINE    then val = T * 9 / 4 + 491.67
        when DELISLE    then val = (80 - T) * 15 / 8
        when NEWTON     then val = T * 33 / 80
        when REAUMUR    then val = T
        when ROEMER     then val = T * 21 / 32 + 7.5
        otherwise       signal IllegalArgumentException(scaleFrom',' scaleTo',' T)
        end
      end
    when ROEMER then do
      select case scaleTo
        when KELVIN     then val = (T - 7.5) * 40 / 21 + 273.15
        when CELSIUS    then val = (T - 7.5) * 40 / 21
        when FAHRENHEIT then val = (T - 7.5) * 24 / 7 + 32
        when RANKINE    then val = (T - 7.5) * 24 / 7 + 491.67
        when DELISLE    then val = (60 - T) * 20 / 7
        when NEWTON     then val = (T - 7.5) * 22 / 35
        when REAUMUR    then val = (T - 7.5) * 32 / 21
        when ROEMER     then val = T
        otherwise       signal IllegalArgumentException(scaleFrom',' scaleTo',' T)
        end
      end
    otherwise
      signal IllegalArgumentException(scaleFrom',' scaleTo',' T)
    end sF

  return val

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static

  tlist = [ -
  /* C....... F....... K....... R.......*/ -
    ' 5500.00  9932.00  5773.15 10391.67', -
    '  300.00   572.00   573.15  1031.67', -
    '  200.00   392.00   473.15   851.67', -
    '  100.00   212.00   373.15   671.67', -
    '   37.00    98.60   310.15   558.27', -
    '    0.00    32.00   273.15   491.67', -
    ' -100.00  -148.00   173.15   311.67', -
    ' -200.00  -328.00    73.15   131.67', -
    ' -252.15  -421.87    21.00    37.80', -
    ' -273.15  -459.67     0.00     0.00'  -
    ]

  parse 'CELSIUS FAHRENHEIT KELVIN RANKINE' CELSIUS FAHRENHEIT KELVIN RANKINE .
  loop temp over tlist
    parse temp ttC ttF ttK ttR .
    say '   C....... F....... K....... R.......'
    say 'C ' -
        temperatureConversion(CELSIUS,    CELSIUS,    ttC).format(5, 2) -
        temperatureConversion(CELSIUS,    FAHRENHEIT, ttC).format(5, 2) -
        temperatureConversion(CELSIUS,    KELVIN,     ttC).format(5, 2) -
        temperatureConversion(CELSIUS,    RANKINE,    ttC).format(5, 2)

    say 'F ' -
        temperatureConversion(FAHRENHEIT, CELSIUS,    ttF).format(5, 2) -
        temperatureConversion(FAHRENHEIT, FAHRENHEIT, ttF).format(5, 2) -
        temperatureConversion(FAHRENHEIT, KELVIN,     ttF).format(5, 2) -
        temperatureConversion(FAHRENHEIT, RANKINE,    ttF).format(5, 2)

    say 'K ' -
        temperatureConversion(KELVIN,     CELSIUS,    ttK).format(5, 2) -
        temperatureConversion(KELVIN,     FAHRENHEIT, ttK).format(5, 2) -
        temperatureConversion(KELVIN,     KELVIN,     ttK).format(5, 2) -
        temperatureConversion(KELVIN,     RANKINE,    ttK).format(5, 2)

    say 'R ' -
        temperatureConversion(RANKINE,    CELSIUS,    ttR).format(5, 2) -
        temperatureConversion(RANKINE,    FAHRENHEIT, ttR).format(5, 2) -
        temperatureConversion(RANKINE,    KELVIN,     ttR).format(5, 2) -
        temperatureConversion(RANKINE,    RANKINE,    ttR).format(5, 2)
    say
    end temp

  return

```

<pre pre style="height: 40ex; overflow: scroll">
   C....... F....... K....... R.......
C   5500.00  9932.00  5773.15 10391.67
F   5500.00  9932.00  5773.15 10391.67
K   5500.00  9932.00  5773.15 10391.67
R   5500.00  9932.00  5773.15 10391.67

   C....... F....... K....... R.......
C    300.00   572.00   573.15  1031.67
F    300.00   572.00   573.15  1031.67
K    300.00   572.00   573.15  1031.67
R    300.00   572.00   573.15  1031.67

   C....... F....... K....... R.......
C    200.00   392.00   473.15   851.67
F    200.00   392.00   473.15   851.67
K    200.00   392.00   473.15   851.67
R    200.00   392.00   473.15   851.67

   C....... F....... K....... R.......
C    100.00   212.00   373.15   671.67
F    100.00   212.00   373.15   671.67
K    100.00   212.00   373.15   671.67
R    100.00   212.00   373.15   671.67

   C....... F....... K....... R.......
C     37.00    98.60   310.15   558.27
F     37.00    98.60   310.15   558.27
K     37.00    98.60   310.15   558.27
R     37.00    98.60   310.15   558.27

   C....... F....... K....... R.......
C      0.00    32.00   273.15   491.67
F      0.00    32.00   273.15   491.67
K      0.00    32.00   273.15   491.67
R      0.00    32.00   273.15   491.67

   C....... F....... K....... R.......
C   -100.00  -148.00   173.15   311.67
F   -100.00  -148.00   173.15   311.67
K   -100.00  -148.00   173.15   311.67
R   -100.00  -148.00   173.15   311.67

   C....... F....... K....... R.......
C   -200.00  -328.00    73.15   131.67
F   -200.00  -328.00    73.15   131.67
K   -200.00  -328.00    73.15   131.67
R   -200.00  -328.00    73.15   131.67

   C....... F....... K....... R.......
C   -252.15  -421.87    21.00    37.80
F   -252.15  -421.87    21.00    37.80
K   -252.15  -421.87    21.00    37.80
R   -252.15  -421.87    21.00    37.80

   C....... F....... K....... R.......
C   -273.15  -459.67     0.00     0.00
F   -273.15  -459.67     0.00     0.00
K   -273.15  -459.67     0.00     0.00
R   -273.15  -459.67     0.00     0.00

```



## Never


```Never

func KtoC(k : float) -> float { k - 273.15 }
func KtoF(k : float) -> float { k * 1.8 - 459.67 }
func KtoR(k : float) -> float { k * 1.8 }

func convertK(k : float) -> int {
    prints("K " + k + "\n");
    prints("C " + KtoC(k) + "\n");
    prints("F " + KtoF(k) + "\n");
    prints("R " + KtoR(k) + "\n");
    0
}

func main(k : float) -> int {
    convertK(k);
    0
}

```

```txt

K 21.00
C -252.15
F -421.87
R 37.80

```



## NewLISP


```NewLISP

(define (to-celsius k)
    (- k 273.15)
)

(define (to-fahrenheit k)
    (- (* k 1.8) 459.67)
)

(define (to-rankine k)
    (* k 1.8)
)

(define (kelvinConversion k)
    (if (number? k)
        (println k " kelvin is equivalent to:\n"
            (to-celsius k) " celsius\n"
            (to-fahrenheit k) " fahrenheit\n"
            (to-rankine k) " rankine")
        (println "Please enter a number only, with no º or letter. ")
    )
)

```

```txt

21 kelvin is equivalent to:
-252 celsius
-438 fahrenheit
21 rankine

```



## Nim


```nim
import rdstdin, strutils, strfmt

while true:
  let k = parseFloat readLineFromStdin "K ? "
  echo "{:g} Kelvin = {:g} Celsius = {:g} Fahrenheit = {:g} Rankine degrees".fmt(
    k, k - 273.15, k * 1.8 - 459.67, k * 1.8)
```

Sample usage:

```txt
K ? 21.0
21 Kelvin = -252.15 Celsius = -421.87 Fahrenheit = 37.8 Rankine degrees
K ? 222.2
222.2 Kelvin = -50.95 Celsius = -59.71 Fahrenheit = 399.96 Rankine degrees
```



## Objeck


```objeck

class Temperature {
  function : Main(args : String[]) ~ Nil {
    k := System.IO.Console->ReadString()->ToFloat();
    c := KelvinToCelsius(k);
    f := KelvinToFahrenheit(k);
    r := KelvinToRankine(k);

    "K: {$k}"->PrintLine();
    "C: {$c}"->PrintLine();
    "F: {$f}"->PrintLine();
    "R: {$r}"->PrintLine();
  }

  function : KelvinToCelsius(k : Float) ~ Float {
    return k - 273.15;
  }

  function : KelvinToFahrenheit(k : Float) ~ Float {
    return k * 1.8 - 459.67;
  }

  function : KelvinToRankine(k : Float) ~ Float {
    return k * 1.8;
  }
}

```



```txt

K: 21.0
C: -252.150
F: -421.870
R: 37.800

```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h


int main(int argc, const char * argv[])
{
    @autoreleasepool {
        if(argc > 1)
        {
            NSString *arg1 = [NSString stringWithCString:argv[1] encoding:NSUTF8StringEncoding];
            // encoding shouldn't matter in this case
            double kelvin = [arg1 doubleValue];

            NSLog(@"K %.2f",kelvin);
            NSLog(@"C %.2f\n", kelvin - 273.15);
            NSLog(@"F %.2f\n", (kelvin * 1.8) - 459.67);
            NSLog(@"R %.2f", kelvin * 1.8);
        }
    }
    return 0;
}
```



## OCaml


```ocaml

let print_temp s t =
  print_string s;
  print_endline (string_of_float t);;

let kelvin_to_celsius k =
  k -. 273.15;;

let kelvin_to_fahrenheit k =
  (kelvin_to_celsius k)*. 9./.5. +. 32.00;;

let kelvin_to_rankine k =
  (kelvin_to_celsius k)*. 9./.5. +. 491.67;;


print_endline "Enter a temperature in Kelvin please:";
let k = read_float () in
print_temp "K " k;
print_temp "C " (kelvin_to_celsius k);
print_temp "F " (kelvin_to_fahrenheit k);
print_temp "R " (kelvin_to_rankine k);;

```


Sample session:

```txt

Enter a temperature in Kelvin please:
184
K 184.
C -89.15
F -128.47
R 331.2

```



## Oforth



```Oforth
: kelvinToCelsius  273.15 - ;
: kelvinToFahrenheit   1.8 * 459.67 - ;
: kelvinToRankine  1.8 * ;

: testTemp(n)
   n kelvinToCelsius println
   n kelvinToFahrenheit println
   n kelvinToRankine println ;
```


```txt

>21 testTemp
-252.15
-421.87
37.8

```



## PARI/GP


```parigp
f(x)=[x,x-273.15,1.8*x-459.67,1.8*x]
```



## Pascal


```Pascal
program TemperatureConvert;

type
    TemperatureType = (C, F, K, R);

var
    kelvin: real;

    function ConvertTemperature(temperature: real; fromType, toType: TemperatureType): real;

    var
        initial, result: real;

    begin
        (* We are going to first convert whatever we're given into Celsius.
           Then we'll convert that into whatever we're asked to convert into.
           Maybe not the most efficient way to do this, but easy to understand
           and should make it easier to add any additional temperature units. *)
        if fromType <> toType then
            begin
                case fromType of (* first convert the temperature into Celsius *)
                    C:
                        initial := temperature;
                    F:
                        initial := (temperature - 32) / 1.8;
                    K:
                        initial := temperature - 273.15;
                    R:
                        initial := (temperature - 491.67) / 1.8;
                end;
                case toType of (* now convert from Celsius into whatever degree type was asked for *)
                    C:
                        result := initial;
                    F:
                        result := (initial * 1.8) + 32;
                    K:
                        result := initial + 273.15;
                    R:
                        result := (initial * 1.8) + 491.67;
                end;
            end
        else (* no point doing all that math if we're asked to convert from and to the same type *)
            result := temperature;
        ConvertTemperature := result;
    end;

begin
    write('Temperature to convert (in kelvins): ');
    readln(kelvin);
    writeln(kelvin : 3 : 2, ' in kelvins is ');
    writeln('    ', ConvertTemperature(kelvin, K, C) : 3 : 2, ' in degrees Celsius.');
    writeln('    ', ConvertTemperature(kelvin, K, F) : 3 : 2, ' in degrees Fahrenheit.');
    writeln('    ', ConvertTemperature(kelvin, K, R) : 3 : 2, ' in degrees Rankine.');
end.
```


```txt

Temperature to convert (in kelvins): 373.15
373.15 in kelvins is
    100.00 in degrees Celsius.
    212.00 in degrees Fahrenheit.
    671.67 in degrees Rankine.

```



## Perl


```Perl
my %scale = (
    Celcius    => { factor => 1  , offset => -273.15 },
    Rankine    => { factor => 1.8, offset =>    0    },
    Fahrenheit => { factor => 1.8, offset => -459.67 },
);

print "Enter a temperature in Kelvin: ";
chomp(my $kelvin = <STDIN>);
die "No such temperature!\n" unless $kelvin > 0;

foreach (sort keys %scale) {
    printf "%12s:%8.2f\n", $_, $kelvin * $scale{$_}{factor} + $scale{$_}{offset};
}
```

```txt

Enter a temperature in Kelvin: 21
     Celcius: -252.15
  Fahrenheit: -421.87
     Rankine:   37.80

```



## Perl 6


```perl6
my %scale =
    Celcius    => { factor => 1  , offset => -273.15 },
    Rankine    => { factor => 1.8, offset =>    0    },
    Fahrenheit => { factor => 1.8, offset => -459.67 },
;

my $kelvin = +prompt "Enter a temperature in Kelvin: ";
die "No such temperature!" if $kelvin < 0;

for %scale.sort {
    printf "%12s: %7.2f\n", .key, $kelvin * .value<factor> + .value<offset>;
}
```


```txt

Enter a temperature in Kelvin: 21
     Celcius: -252.15
  Fahrenheit: -421.87
     Rankine:   37.80

```


Alternative version that accepts the input in any of the four scales:


```perl6
while my $answer = prompt 'Temperature: ' {
    my $k = do given $answer {
        when s/:i C $// { $_ + 273.15 }
        when s/:i F $// { ($_ + 459.67) / 1.8 }
        when s/:i R $// { $_ / 1.8 }
        when s/:i K $// { $_ }
        default         { $_ }
    }
    say "  { $k }K";
    say "  { $k - 273.15 }℃";
    say "  { $k * 1.8 - 459.67 }℉";
    say "  { $k * 1.8 }R";
}
```

```txt
Temperature: 0
  0K
  -273.15℃
  -459.67℉
  0R
Temperature: 0c
  273.15K
  0℃
  32℉
  491.67R
Temperature: 212f
  373.15K
  100℃
  212℉
  671.67R
Temperature: -40c
  233.15K
  -40℃
  -40℉
  419.67R
```



## Phix

Modified copy of [[Temperature_conversion#Euphoria|Euphoria]]

```Phix
atom K = prompt_number("Enter temperature in Kelvin >=0: ",{0,1e307})
printf(1,"    Kelvin: %5.2f\n   Celsius: %5.2f\nFahrenheit: %5.2f\n   Rankine: %5.2f\n\n",
                      {K,                 K-273.15,          K*1.8-459.67,      K*1.8})

```

```txt

Enter temperature in Kelvin >=0: 300
    Kelvin: 300.00
   Celsius: 26.85
Fahrenheit: 80.33
   Rankine: 540.00

```



## PHP


```php


while (true) {
    echo "\nEnter a value in kelvin (q to quit): ";
    if ($kelvin = trim(fgets(STDIN))) {
        if ($kelvin == 'q') {
            echo 'quitting';
            break;
        }
        if (is_numeric($kelvin)) {
            $kelvin = floatVal($kelvin);
            if ($kelvin >= 0) {
                printf(" K  %2.2f\n", $kelvin);
                printf(" C  %2.2f\n", $kelvin - 273.15);
                printf(" F  %2.2f\n", $kelvin * 1.8 - 459.67);
                printf(" R  %2.2f\n", $kelvin * 1.8);
            } else printf(" %2.2f K is below absolute zero\n", $kelvin);
        }
    }
}
```

```txt
Enter a value in kelvin (q to quit): 21
 K  21.00
 C  -252.15
 F  -421.87
 R  37.80

Enter a value in kelvin (q to quit): q
quitting
```



## PicoLisp


```PicoLisp
(scl 2)

(de convertKelvin (Kelvin)
   (for X
      (quote
         (K . prog)
         (C (K) (- K 273.15))
         (F (K) (- (*/ K 1.8 1.0) 459.67))
         (R (K) (*/ K 1.8 1.0)) )
      (tab (-3 8)
         (car X)
         (format ((cdr X) Kelvin) *Scl) ) ) )
```

Test:

```PicoLisp
(convertKelvin 21.0)
```

```txt
K     21.00
C   -252.15
F   -421.87
R     37.80
```



## PL/I


```pli
*process source attributes xref;
 /* PL/I **************************************************************
 * 15.08.2013 Walter Pachl translated from NetRexx
 * temperatures below 0K are considered invalid
 *********************************************************************/
 temperature: Proc Options(main);
 Dcl sysin record Input;
 On Endfile(sysin) Goto eoj;
 On Record(sysin);
 Dcl 1 dat,
      2 t Pic'SSSS9V.99',
      2 *    char( 1),
      2 from char(10),
      2 *    char( 1),
      2 to   char(10);
 Do Forever;
   Read File(sysin) Into(dat);
   If tc(t,from,'KELVIN')<0 Then
     Put Edit('Input (',t,from,') invalid. Below absolute zero')
             (Skip,a,f(8,2),x(1),a,a);
   Else
     Put edit(t,from,' -> ',tc(t,from,to),to)
             (skip,f(8,2),x(1),a(10),a,f(8,2),x(1),a(10));
   End;
 eoj: Return;

 tc: Procedure(T,scaleFrom,scaleTo) Returns(Dec Fixed(8,2));
 Dcl t Pic'SSSS9V.99';
 Dcl (val) Dec Fixed(8,2);
 Dcl (scaleFrom,scaleTo) Char(10);
  select(scaleFrom);
    when('KELVIN ') do;
      select(scaleTo);
        when('KELVIN    ') val = T;
        when('CELSIUS   ') val = T - 273.15;
        when('FAHRENHEIT') val = T * 9 / 5 - 459.67;
        when('RANKINE   ') val = T * 9 / 5;
        when('DELISLE   ') val = (373.15 - T) * 3 / 2;
        when('NEWTON    ') val = (T - 273.15) * 33 / 100;
        when('REAUMUR   ') val = (T - 273.15) * 4 / 5;
        when('ROEMER    ') val = (T - 273.15) * 21 / 40 + 7.5;
        otherwise Do;
          Put Edit('scaleTo=',scaleTo)(Skip,a,a);
          Call err(1);
          End;
        end;
      end;
    when('CELSIUS') do;
      select(scaleTo);
        when('KELVIN    ') val = T + 273.15;
        when('CELSIUS   ') val = T;
        when('FAHRENHEIT') val = T * 9 / 5 + 32;
        when('RANKINE   ') val = (T + 273.15) * 9 / 5;
        when('DELISLE   ') val = (100 - T) * 3 / 2;
        when('NEWTON    ') val = T * 33 / 100;
        when('REAUMUR   ') val = T * 4 / 5;
        when('ROEMER    ') val = T * 21 / 40 + 7.5;
        otherwise Call err(2);
        end;
      end;
    when('FAHRENHEIT') do;
      select(scaleTo);
        when('KELVIN    ') val = (T + 459.67) * 5 / 9;
        when('CELSIUS   ') val = (T - 32) * 5 / 9;
        when('FAHRENHEIT') val = T;
        when('RANKINE   ') val = T + 459.67;
        when('DELISLE   ') val = (212 - T) * 5 / 6;
        when('NEWTON    ') val = (T - 32) * 11 / 60;
        when('REAUMUR   ') val = (T - 32) * 4 / 9;
        when('ROEMER    ') val = (T - 32) * 7 / 24 + 7.5;
        otherwise Call err(3);
        end;
      end;
    when('RANKINE') do;
      select(scaleTo);
        when('KELVIN    ') val = T * 5 / 9;
        when('CELSIUS   ') val = (T - 491.67) * 5 / 9;
        when('FAHRENHEIT') val = T - 459.67;
        when('RANKINE   ') val = T;
        when('DELISLE   ') val = (671.67 - T) * 5 / 6;
        when('NEWTON    ') val = (T - 491.67) * 11 / 60;
        when('REAUMUR   ') val = (T - 491.67) * 4 / 9;
        when('ROEMER    ') val = (T - 491.67) * 7 / 24 + 7.5;
        otherwise Call err(4);
        end;
      end;
    when('DELISLE') do;
      select(scaleTo);
        when('KELVIN    ') val = 373.15 - T * 2 / 3;
        when('CELSIUS   ') val = 100 - T * 2 / 3;
        when('FAHRENHEIT') val = 212 - T * 6 / 5;
        when('RANKINE   ') val = 671.67 - T * 6 / 5;
        when('DELISLE   ') val = T;
        when('NEWTON    ') val = 33 - T * 11 / 50;
        when('REAUMUR   ') val = 80 - T * 8 / 15;
        when('ROEMER    ') val = 60 - T * 7 / 20;
        otherwise Call err(5);
        end;
      end;
    when('NEWTON') do;
      select(scaleTo);
        when('KELVIN    ') val = T * 100 / 33 + 273.15;
        when('CELSIUS   ') val = T * 100 / 33;
        when('FAHRENHEIT') val = T * 60 / 11 + 32;
        when('RANKINE   ') val = T * 60 / 11 + 491.67;
        when('DELISLE   ') val = (33 - T) * 50 / 11;
        when('NEWTON    ') val = T;
        when('REAUMUR   ') val = T * 80 / 33;
        when('ROEMER    ') val = T * 35 / 22 + 7.5;
        otherwise Call err(6);
        end;
      end;
    when('REAUMUR') do;
      select(scaleTo);
        when('KELVIN    ') val = T * 5 / 4 + 273.15;
        when('CELSIUS   ') val = T * 5 / 4;
        when('FAHRENHEIT') val = T * 9 / 4 + 32;
        when('RANKINE   ') val = T * 9 / 4 + 491.67;
        when('DELISLE   ') val = (80 - T) * 15 / 8;
        when('NEWTON    ') val = T * 33 / 80;
        when('REAUMUR   ') val = T;
        when('ROEMER    ') val = T * 21 / 32 + 7.5;
        otherwise Call err(7);
        end;
      end;
    when('ROEMER') do;
      select(scaleTo);
        when('KELVIN    ') val = (T - 7.5) * 40 / 21 + 273.15;
        when('CELSIUS   ') val = (T - 7.5) * 40 / 21;
        when('FAHRENHEIT') val = (T - 7.5) * 24 / 7 + 32;
        when('RANKINE   ') val = (T - 7.5) * 24 / 7 + 491.67;
        when('DELISLE   ') val = (60 - T) * 20 / 7;
        when('NEWTON    ') val = (T - 7.5) * 22 / 35;
        when('REAUMUR   ') val = (T - 7.5) * 32 / 21;
        when('ROEMER    ') val = T;
        otherwise Call err(8);
        end;
      end;
    otherwise Call err(9);
    end;
  return(val);
  err: Proc(e);
  Dcl e Dec fixed(1);
  Put Edit('error ',e,' invalid input')(Skip,a,f(1),a);
  val=0;
  End;
  End;
 End;
```

```txt

   21.00 KELVIN     ->  -252.15 CELSIUS
   21.00 KELVIN     ->  -421.87 FAHRENHEIT
   21.00 KELVIN     ->    37.80 RANKINE
Input (  600.00 DELISLE   ) invalid. Below absolute zero
Input (   -1.00 KELVIN    ) invalid. Below absolute zero
Input ( -300.00 CELSIUS   ) invalid. Below absolute zero
  212.00 FAHRENHEIT ->   100.00 CELSIUS
    0.00 FAHRENHEIT ->   -17.77 CELSIUS
    0.00 CELSIUS    ->    32.00 FAHRENHEIT
   37.00 CELSIUS    ->    98.60 FAHRENHEIT

```


## PowerShell

```powershell
function temp($k){
	try{
		$c = $k - 273.15
		$r = $k / 5 * 9
		$f = $r - 459.67
	} catch {
		Write-host "Input error."
		return
	}

	Write-host ""
	Write-host "	TEMP (Kelvin)    : " $k
	Write-host "	TEMP (Celsius)   : " $c
	Write-host "	TEMP (Fahrenheit): " $f
	Write-host "	TEMP (Rankine)   : " $r
	Write-host ""

}

$input=Read-host "Enter a temperature in Kelvin"
temp $input
```

```txt
PS> ./TEMPS
Enter a temperature in Kelvin: 100

        TEMP (Kelvin)    :  100
        TEMP (Celsius)   :  -173.15
        TEMP (Fahrenheit):  -279.67
        TEMP (Rankine)   :  180

PS>
```



### PowerShell Alternate Version

A more "PowerShelly" way to do it.

```PowerShell

function Convert-Kelvin
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [double]
        $InputObject
    )

    Process
    {
        foreach ($kelvin in $InputObject)
        {
            [PSCustomObject]@{
                Kelvin     = $kelvin
                Celsius    = $kelvin - 273.15
                Fahrenheit = $kelvin * 1.8 - 459.67
                Rankine    = $kelvin * 1.8
            }
        }
    }
}

```


```PowerShell

21, 100 | Convert-Kelvin

```

```txt

Kelvin Celsius Fahrenheit Rankine
------ ------- ---------- -------
    21 -252.15    -421.87    37.8
   100 -173.15    -279.67     180

```



## Pure Data


'''temperature.pd'''

```txt

#N canvas 200 200 640 600 10;
#X floatatom 130 54 8 0 0 2 Kelvin chgk -;
#X obj 130 453 rnd2;
#X floatatom 130 493 8 0 0 1 K - -;
#X floatatom 251 54 8 0 0 2 Celsius chgc -;
#X obj 251 453 rnd2;
#X floatatom 251 493 8 0 0 1 °C - -;
#X floatatom 374 54 8 0 0 2 Fahrenheit chgf -;
#X obj 374 453 rnd2;
#X floatatom 374 493 8 0 0 1 °F - -;
#X floatatom 498 54 8 0 0 2 Rankine chgr -;
#X obj 498 453 rnd2;
#X floatatom 498 493 8 0 0 1 °Ra - -;
#X obj 65 133 - 273.15;
#X obj 65 244 * 1.8;
#X obj 65 267 + 32;
#X obj 65 363 + 459.67;
#X obj 186 133 * 1.8;
#X obj 186 156 + 32;
#X obj 186 268 + 459.67;
#X obj 186 310 / 1.8;
#X obj 309 133 + 459.67;
#X obj 309 215 / 1.8;
#X obj 309 291 - 273.15;
#X obj 433 133 / 1.8;
#X obj 433 223 - 273.15;
#X obj 433 294 * 1.8;
#X obj 433 317 + 32;
#X text 20 53 Input:;
#X text 20 492 Output:;
#X connect 0 0 1 0;
#X connect 0 0 12 0;
#X connect 1 0 2 0;
#X connect 3 0 4 0;
#X connect 3 0 16 0;
#X connect 4 0 5 0;
#X connect 6 0 7 0;
#X connect 6 0 20 0;
#X connect 7 0 8 0;
#X connect 9 0 10 0;
#X connect 9 0 23 0;
#X connect 10 0 11 0;
#X connect 12 0 13 0;
#X connect 12 0 4 0;
#X connect 13 0 14 0;
#X connect 14 0 15 0;
#X connect 14 0 7 0;
#X connect 15 0 10 0;
#X connect 16 0 17 0;
#X connect 17 0 18 0;
#X connect 17 0 7 0;
#X connect 18 0 19 0;
#X connect 18 0 10 0;
#X connect 19 0 1 0;
#X connect 20 0 21 0;
#X connect 20 0 10 0;
#X connect 21 0 22 0;
#X connect 21 0 1 0;
#X connect 22 0 4 0;
#X connect 23 0 24 0;
#X connect 23 0 1 0;
#X connect 24 0 25 0;
#X connect 24 0 4 0;
#X connect 25 0 26 0;
#X connect 26 0 7 0;

```

Plugin to round the results to at most 2 digits:

'''rnd.pd'''

```txt

#N canvas 880 200 450 300 10;
#X obj 77 34 inlet;
#X obj 77 113 * 100;
#X obj 77 135 + 0.5;
#X obj 132 135 < 0;
#X obj 77 172 -;
#X obj 77 194 int;
#X obj 77 216 / 100;
#X obj 77 238 outlet;
#X connect 0 0 1 0;
#X connect 0 0 3 0;
#X connect 1 0 2 0;
#X connect 2 0 4 0;
#X connect 3 0 4 1;
#X connect 4 0 5 0;
#X connect 5 0 6 0;
#X connect 6 0 7 0;

```



## PureBasic


```purebasic
Procedure.d Kelvin2Celsius(tK.d)    : ProcedureReturn tK-273.15     : EndProcedure
Procedure.d Kelvin2Fahrenheit(tK.d) : ProcedureReturn tK*1.8-459.67 : EndProcedure
Procedure.d Kelvin2Rankine(tK.d)    : ProcedureReturn tK*1.8        : EndProcedure

OpenConsole()
Repeat
  Print("Temperatur Kelvin? ") : Kelvin.d = ValD(Input())
  PrintN("Conversion:")
  PrintN(#TAB$+"Celsius   "+#TAB$+RSet(StrD(Kelvin2Celsius(Kelvin),2),8,Chr(32)))
  PrintN(#TAB$+"Fahrenheit"+#TAB$+RSet(StrD(Kelvin2Fahrenheit(Kelvin),2),8,Chr(32)))
  PrintN(#TAB$+"Rankine   "+#TAB$+RSet(StrD(Kelvin2Rankine(Kelvin),2),8,Chr(32)))
  PrintN("ESC = End.")
  Repeat
    k$=Inkey() : Delay(50) : If RawKey()=#ESC : End : EndIf
  Until RawKey()
ForEver
```


```txt
Temperatur Kelvin? 21
Conversion:
        Celsius          -252.15
        Fahrenheit       -421.87
        Rankine            37.80
ESC = End.
```



## Python


```python>>>
 while True:
	k = float(input('K ? '))
	print("%g Kelvin = %g Celsius = %g Fahrenheit = %g Rankine degrees."
	      % (k, k - 273.15, k * 1.8 - 459.67, k * 1.8))


K ? 21.0
21 Kelvin = -252.15 Celsius = -421.87 Fahrenheit = 37.8 Rankine degrees.
K ? 222.2
222.2 Kelvin = -50.95 Celsius = -59.71 Fahrenheit = 399.96 Rankine degrees.
K ?
```



### Python: Universal conversion

This converts from any one of the units to all the others

```python>>>
 toK = {'C': (lambda c: c + 273.15),
           'F': (lambda f: (f + 459.67) / 1.8),
           'R': (lambda r: r / 1.8),
           'K': (lambda k: k) }
>>> while True:
	magnitude, unit = input('<value> <K/R/F/C> ? ').split()
	k = toK[unit](float(magnitude))
	print("%g Kelvin = %g Celsius = %g Fahrenheit = %g Rankine degrees."
	      % (k, k - 273.15, k * 1.8 - 459.67, k * 1.8))


<value> <K/R/F/C> ? 222.2 K
222.2 Kelvin = -50.95 Celsius = -59.71 Fahrenheit = 399.96 Rankine degrees.
<value> <K/R/F/C> ? -50.95 C
222.2 Kelvin = -50.95 Celsius = -59.71 Fahrenheit = 399.96 Rankine degrees.
<value> <K/R/F/C> ? -59.71 F
222.2 Kelvin = -50.95 Celsius = -59.71 Fahrenheit = 399.96 Rankine degrees.
<value> <K/R/F/C> ? 399.96 R
222.2 Kelvin = -50.95 Celsius = -59.71 Fahrenheit = 399.96 Rankine degrees.
<value> <K/R/F/C> ?
```



## Racket

Although not exactly the shortest code,
the converter function can turn any temperature into any other

```Racket
#lang racket
(define (converter temp init final)
  (define to-k
    (case init
      ('k temp)
      ('c (+ 273.15 temp))
      ('f (* (+ temp 459.67) 5/9))
      ('r (* temp 5/9))))
  (case final
    ('k to-k)
    ('c (- to-k 273.15))
    ('f (- (* to-k 9/5) 459.67))
    ('r (* to-k 1.8))))

(define (kelvin-to-all temp)
  (display (format "Kelvin: ~a \nCelsius: ~a \nFahrenheit: ~a \nRankine: ~a \n"
                   temp
                   (converter temp 'k 'c)
                   (converter temp 'k 'f)
                   (converter temp 'k 'r))))
(kelvin-to-all 21)
;Kelvin: 21
;Celsius: -252.14999999999998
;Fahrenheit: -421.87
;Rankine: 37.800000000000004

```



## REXX


### abridged

This REXX version supports:
::* (alternate spellings with optional   ''degree''   or   ''degrees''   preceding the scale name):
::* alternate temperature scale names
::* supports   ''any to all''   conversions
::* supports   ''any to any''    conversion   (with the   '''TO'''   option)
::* support of some common misspellings   (''it knows what you mean'')
::* support of some common temperature scales:
::::::* Celsius,   centigrade
::::::* Delisle
::::::* Fahrenheit
::::::* kelvin
::::::* Newton
::::::* Rankine
::::::* Reaumur,   Réaumur
::::::* Romer,   Rømer,   Roemer
::* multiple temperatures in a list
::* specification of which temperature scale to be used for conversion
::* conversion of a temperature to:
::::::* all other temperature scales
::::::* a specific temperature scale
::* supports proper pluralization of kelvin
::* comments (annotation notes) allowed within the list
::* aligned output (whole numbers and decimal fractions)

```rexx
/*REXX program  converts  temperatures  for a number (8)  of  temperature scales.       */
numeric digits 120                               /*be able to support some huge numbers.*/
parse arg tList                                  /*get the specified temperature list.  */

  do  until  tList=''                            /*process the list of temperatures.    */
  parse  var tList  x  ','  tList                /*temps are separated by commas.       */
  x=translate(x,'((',"[{")                       /*support other grouping symbols.      */
  x=space(x);  parse var x z '('                 /*handle any comments  (if any).       */
  parse upper  var  z  z   ' TO '  ! .           /*separate the  TO  option from number.*/
  if !==''  then !='ALL'; all=!=='ALL'           /*allow specification of "TO" opt*/
  if z==''     then call serr "no arguments were specified."                 /*oops-ay. */
  _=verify(z, '+-.0123456789')                   /*list of valid numeral/number thingys.*/
  n=z
  if _\==0  then do
                 if _==1  then call serr 'illegal temperature:'  z
                 n=left(z, _-1)                  /*pick off the number  (hopefully).    */
                 u=strip(substr(z, _))           /*pick off the  temperature  unit.     */
                 end
            else u='k'                           /*assume kelvin as per task requirement*/

  if \datatype(n, 'N')  then call serr 'illegal number:'    n
  if \all  then do                               /*is there is a     TO  ααα     scale? */
                call name !                      /*process the   TO   abbreviation.     */
                !=sn                             /*assign the full name to     !        */
                end                              /*!: now contains temperature full name*/
  call name u                                    /*allow alternate scale (miss)spellings*/

      select                                     /*convert ──► °Fahrenheit temperatures.*/
      when sn=='CELSIUS'          then F=n       *  9/5   +  32
      when sn=='DELISLE'          then F=212 -(n *  6/5)
      when sn=='FAHRENHEIT'       then F=n
      when sn=='KELVIN'           then F=n       *  9/5   - 459.67
      when sn=='NEWTON'           then F=n       * 60/11  +  32
      when sn=='RANKINE'          then F=n                - 459.67       /*a single  R  is taken as Rankine.*/
      when sn=='REAUMUR'          then F=n       *  9/4   +  32
      when sn=='ROMER'            then F=(n-7.5) * 27/4   +  32
      otherwise          call serr  'illegal temperature scale: '    u
      end   /*select*/

  K = (F + 459.67)  *  5/9                       /*compute temperature to  kelvins.     */
  say right(' ' x, 79, "─")                      /*show the original value, scale, sep. */
  if all | !=='CELSIUS'           then say $(   ( F   -  32     )   *  5/9           )    'Celsius'
  if all | !=='DELISLE'           then say $(   ( 212 -  F      )   *  5/6           )    'Delisle'
  if all | !=='FAHRENHEIT'        then say $(     F                                  )    'Fahrenheit'
  if all | !=='KELVIN'            then say $(        K                               )    'kelvin's(K)
  if all | !=='NEWTON'            then say $(   ( F   -  32     )   *  11/60         )    'Newton'
  if all | !=='RANKINE'           then say $(     F   + 459.67                       )    'Rankine'
  if all | !=='REAUMUR'           then say $(   ( F   -  32     )   *  4/9           )    'Reaumur'
  if all | !=='ROMER'             then say $(   ( F   -  32     )   *  4/27    + 7.5 )    'Romer'
  end   /*until*/

exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:    if arg(1)==1  then return arg(3);     return word(arg(2) 's',1)
serr: say;   say '***error!***';    say;    say arg(1);    say;   exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
$:    procedure;  showDig=8                      /*only show  eight  significant digits.*/
      _=format(arg(1), , showDig) / 1            /*format number 8 digs past dec, point.*/
      p=pos(., _);         L=length(_)           /*find position of the decimal point.  */
                                                 /* [↓]  align integers with FP numbers.*/
      if p==0 then _=_ || left('',5+showDig+1)   /*the number has no decimal point.     */
              else _=_ || left('',5+showDig-L+p) /* "     "    "   a    "      "        */
      return right(_, 50)                        /*return the re-formatted number (arg).*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
name: parse arg y                                       /*abbreviations  ──►  shortname.*/
      yU=translate(y, 'eE', "éÉ");   upper yU           /*uppercase the temperature unit*/
      if left(yU, 7)=='DEGREES'  then yU=substr(yU, 8)  /*redundant "degrees" after  #? */
      if left(yU, 6)=='DEGREE'   then yU=substr(yU, 7)  /*   "      "degree"    "    "  */
      yU=strip(yU)                                      /*elide blanks at front and back*/
      _=length(yU)                                      /*obtain the    yU    length.   */
      if right(yU, 1)=='S' & _>1 then yU=left(yU, _ -1) /*elide trailing plural, if any.*/
             select                                     /*abbreviations  ──►  shortname.*/
             when abbrev('CENTIGRADE' , yU)    |,
                  abbrev('CENTRIGRADE', yU)    |,                     /* 50% misspelled.*/
                  abbrev('CETIGRADE'  , yU)    |,                     /* 50% misspelled.*/
                  abbrev('CENTINGRADE', yU)    |,
                  abbrev('CENTESIMAL' , yU)    |,
                  abbrev('CELCIU'     , yU)    |,                     /* 82% misspelled.*/
                  abbrev('CELCIOU'    , yU)    |,                     /*  4% misspelled.*/
                  abbrev('CELCUI'     , yU)    |,                     /*  4% misspelled.*/
                  abbrev('CELSUI'     , yU)    |,                     /*  2% misspelled.*/
                  abbrev('CELCEU'     , yU)    |,                     /*  2% misspelled.*/
                  abbrev('CELCU'      , yU)    |,                     /*  2% misspelled.*/
                  abbrev('CELISU'     , yU)    |,                     /*  1% misspelled.*/
                  abbrev('CELSU'      , yU)    |,                     /*  1% misspelled.*/
                  abbrev('CELSIU'     , yU)       then sn='CELSIUS'
             when abbrev('DELISLE'    , yU,2)     then sn='DELISLE'
             when abbrev('FARENHEIT'  , yU)    |,                     /* 39% misspelled.*/
                  abbrev('FARENHEIGHT', yU)    |,                     /* 15% misspelled.*/
                  abbrev('FARENHITE'  , yU)    |,                     /*  6% misspelled.*/
                  abbrev('FARENHIET'  , yU)    |,                     /*  3% misspelled.*/
                  abbrev('FARHENHEIT' , yU)    |,                     /*  3% misspelled.*/
                  abbrev('FARINHEIGHT', yU)    |,                     /*  2% misspelled.*/
                  abbrev('FARENHIGHT' , yU)    |,                     /*  2% misspelled.*/
                  abbrev('FAHRENHIET' , yU)    |,                     /*  2% misspelled.*/
                  abbrev('FERENHEIGHT', yU)    |,                     /*  2% misspelled.*/
                  abbrev('FEHRENHEIT' , yU)    |,                     /*  2% misspelled.*/
                  abbrev('FERENHEIT'  , yU)    |,                     /*  2% misspelled.*/
                  abbrev('FERINHEIGHT', yU)    |,                     /*  1% misspelled.*/
                  abbrev('FARIENHEIT' , yU)    |,                     /*  1% misspelled.*/
                  abbrev('FARINHEIT'  , yU)    |,                     /*  1% misspelled.*/
                  abbrev('FARANHITE'  , yU)    |,                     /*  1% misspelled.*/
                  abbrev('FAHRENHEIT' , yU)       then sn='FAHRENHEIT'
             when abbrev('KALVIN'     , yU)    |,                     /* 27% misspelled.*/
                  abbrev('KERLIN'     , yU)    |,                     /* 18% misspelled.*/
                  abbrev('KEVEN'      , yU)    |,                     /*  9% misspelled.*/
                  abbrev('KELVIN'     , yU)       then sn='KELVIN'
             when abbrev('NEUTON'     , yU)    |,                     /*100% misspelled.*/
                  abbrev('NEWTON'     , yU)       then sn='NEWTON'
             when abbrev('RANKINE'    , yU, 1)    then sn='RANKINE'
             when abbrev('REAUMUR'    , yU, 2)    then sn='REAUMUR'
             when abbrev('ROEMER'     , yU, 2) |,
                  abbrev('ROMER'      , yU, 2)    then sn='ROMER'
             otherwise           call serr  'illegal temperature scale:'  y
             end   /*select*/
      return
```

'''output'''   when using the input of:     <tt> 98.6F to C,   -40C, 0 c (water freezes),   37C (body temp),   100 C (water boils),   21 degrees Kelvin,   0 K (outer space?) </tt>

```txt

───────────────────────────────────────────────────────────────────  98.6F to C
                                  37               Celsius
─────────────────────────────────────────────────────────────────────────  -40C
                                 -40               Celsius
                                 210               Delisle
                                 -40               Fahrenheit
                                 233.15            kelvins
                                 -13.2             Newton
                                 419.67            Rankine
                                 -32               Reaumur
                                  -3.16666667      Romer
──────────────────────────────────────────────────────────  0 c (water freezes)
                                   0               Celsius
                                 150               Delisle
                                  32               Fahrenheit
                                 273.15            kelvins
                                   0               Newton
                                 491.67            Rankine
                                   0               Reaumur
                                   7.5             Romer
──────────────────────────────────────────────────────────────  37C (body temp)
                                  37               Celsius
                                  94.5             Delisle
                                  98.6             Fahrenheit
                                 310.15            kelvins
                                  12.21            Newton
                                 558.27            Rankine
                                  29.6             Reaumur
                                  17.36666667      Romer
──────────────────────────────────────────────────────────  100 C (water boils)
                                 100               Celsius
                                   0               Delisle
                                 212               Fahrenheit
                                 373.15            kelvins
                                  33               Newton
                                 671.67            Rankine
                                  80               Reaumur
                                  34.16666667      Romer
────────────────────────────────────────────────────────────  21 degrees Kelvin
                                -252.15            Celsius
                                 528.225           Delisle
                                -421.87            Fahrenheit
                                  21               kelvins
                                 -83.2095          Newton
                                  37.8             Rankine
                                -201.72            Reaumur
                                 -59.74            Romer
───────────────────────────────────────────────────────────  0 K (outer space?)
                                -273.15            Celsius
                                 559.725           Delisle
                                -459.67            Fahrenheit
                                   0               kelvins
                                 -90.1395          Newton
                                   0               Rankine
                                -218.52            Reaumur
                                 -65.34            Romer

```

[Actually, water freezes at 0.000089º C,   and boils at 99.974º C.]


### unabridged

The REXX program can be seen at   ──►   [[Temperature conversion/REXX]]

This REXX version supports   '''58'''   temperature scales.

Scientific note:   at temperatures above   '''1 Planck''',   quantum gravitational effects become relevant, and current physical theory breaks down because there is a lack of a theory of quantum gravity.


See the Wikipedia article:     [http://en.wikipedia.org/wiki/Planck_temperature Planck temperature].




'''output'''   when using the input of:     <tt> 0 Fahrenheit </tt>

```txt

─────────────────────────────────────────────────────────────────  0 Fahrenheit
                                           255.37222222      Absolute
                                            47.67781999      Amonton
                                            -1               Barnsdorf
                                           -14.35292957      Beaumuir
                                           -21.81664121      Benart
                                           -23.8667          Bergen
                                           -15               Brisson
                                           -17.77777778      Celsius
                                             1.67777462      Cimento
                                           992.00031276      Cruquius
                                           -21.85185185      Dalence
                                           -21.57297438      Dalton
                                            -7.70075111      Daniell
                                             3               De la Hire
                                            -6.48011         De la Ville
                                           176.66666667      Delisle
                                           133.67787165      Delisle OLD
                                           -14               De Luc
                                           -17.5             De Lyon
                                           174.84536082      De Revillas
                                            72.4978          Derham
                                            -1.5             Derham OLD
                                           -23.7037          De Villeneuve
                                           -17.6666          De Suede
                                           -37.9202          Du Crest
                                             1.37508701      Edinburgh
                                             0.02200631      electron volts
                                             0               Fahrenheit
                                           -89.2727          Fahrenheit OLD
                                            -7.42857         Florentine large
                                           -73.9736          Florentine Magnum
                                             1.38571         Florentine small
                                           -83.97491258      Fowler
                                           -73.459919        Frick
                                           -10               gas mark
                                            16               Goubert
                                           -26.66666667      Hales
                                            10.000375        Hanow
                                          -122.44444444      Hauksbee
                                           210.7777          Jacobs-Holborn
                                           255.37222222      kelvins
                                           235.222           Leiden
                                            -5.86666667      Newton
                                            16               Oertel
                                             1.80241583E-30  Planck
                                           459.67            Rankine
                                           -14.22222222      Reaumur
                                             3.39999781      Richter
                                            -2.13333333      Rinaldini
                                             2.75925926      Romer
                                           866.84368889      Rosenthal
                                           122.82            Royal Society of London
                                            15.74512902      Segredo
                                           -44.20787188      Saint-Patrice
                                            -5.71111111      Stufe
                                           -29.00074174      Sulzer
                                            -0.59259259      Thermostat
                                           -11.53701838      Wedgwood

```

'''output'''   when using the input of:     <tt> 0 kelvin </tt>

```txt

─────────────────────────────────────────────────────────────────────  0 kelvin
                                             0               Absolute
                                            -7.22722761      Amonton
                                           -68.03513851      Barnsdorf
                                          -220.52827751      Beaumuir
                                          -342.38766729      Benart
                                          -452.89203333      Bergen
                                          -230.4703125       Brisson
                                          -273.15            Celsius
                                          -168.14455975      Cimento
                                          -131.1567538       Cruquius
                                          -192.1             Dalence
                                           -infinity         Dalton
                                           -70.91221875      Daniell
                                          -249.38503119      De la Hire
                                          -459.51615256      De la Ville
                                           559.725           Delisle
                                           423.52554742      Delisle OLD
                                          -215.105625        De Luc
                                          -268.88203125      De Lyon
                                           553.95463918      De Revillas
                                          -104.21950913      Derham
                                          -154.72333333      Derham OLD
                                          -364.20011547      De Villeneuve
                                          -272.01733333      De Suede
                                          -337.00724352      Du Crest
                                           -97.38098225      Edinburgh
                                             0               electron volts
                                          -459.67            Fahrenheit
                                          -925.03633636      Fahrenheit OLD
                                          -401.43149281      Florentine large
                                          -766.5078253       Florentine Magnum
                                          -172.63202157      Florentine small
                                          -801.84922875      Fowler
                                          -650.345769        Frick
                                           -28.3868          gas mark
                                          -213.835           Goubert
                                          -409.725           Hales
                                          -420.93486331      Hanow
                                          -760.875           Hauksbee
                                        -1,602.36507778      Jacobs-Holborn
                                             0               kelvins
                                           -20.15022222      Leiden
                                           -90.1395          Newton
                                          -213.835           Oertel
                                             0               Planck
                                             0               Rankine
                                          -218.52            Reaumur
                                          -206.32443969      Richter
                                           -32.778           Rinaldini
                                           -65.34            Romer
                                           -11.63675556      Rosenthal
                                           757.1646          Royal Society of London
                                        -1,194.54976303      Segredo
                                          -219.57210928      Saint-Patrice
                                           -15.926           Stufe
                                          -430.12644531      Sulzer
                                            -9.105           Thermostat
                                           -21.81059691      Wedgwood

```



## Ring


```ring

k = 21.0 c = 0 r = 0 f = 0
convertTemp(k)
see "Kelvin : " + k + nl +
"Celcius : " + c + nl +
"Rankine : " + r + nl +
"Fahrenheit : " + f + nl

func convertTemp k
     c = k - 273.15
     r = k * 1.8
     f = r - 459.67

```



## Ruby


```ruby
module TempConvert

  FROM_TEMP_SCALE_TO_K =
  {'kelvin'     => lambda{|t| t},
   'celsius'    => lambda{|t| t + 273.15},
   'fahrenheit' => lambda{|t| (t + 459.67) * 5/9.0},
   'rankine'    => lambda{|t| t * 5/9.0},
   'delisle'    => lambda{|t| 373.15 - t * 2/3.0},
   'newton'     => lambda{|t| t * 100/33.0 + 273.15},
   'reaumur'    => lambda{|t| t * 5/4.0 + 273.15},
   'roemer'     => lambda{|t| (t - 7.5) * 40/21.0 + 273.15}}

  TO_TEMP_SCALE_FROM_K =
  {'kelvin'     => lambda{|t| t},
   'celsius'    => lambda{|t| t - 273.15},
   'fahrenheit' => lambda{|t| t * 9/5.0 - 459.67},
   'rankine'    => lambda{|t| t * 9/5.0},
   'delisle'    => lambda{|t| (373.15 - t) * 3/2.0},
   'newton'     => lambda{|t| (t - 273.15) * 33/100.0},
   'reaumur'    => lambda{|t| (t - 273.15) * 4/5.0},
   'roemer'     => lambda{|t| (t - 273.15) * 21/40.0 + 7.5}}

  SUPPORTED_SCALES = FROM_TEMP_SCALE_TO_K.keys.join('|')

  def self.method_missing(meth, *args, &block)
    if valid_temperature_conversion?(meth) then
      convert_temperature(meth, *args)
    else
      super
    end
  end

  def self.respond_to_missing?(meth, include_private = false)
    valid_temperature_conversion?(meth) || super
  end

  def self.valid_temperature_conversion?(meth)
    !!(meth.to_s =~ /(#{SUPPORTED_SCALES})_to_(#{SUPPORTED_SCALES})/)
  end

  def self.convert_temperature(meth, temp)
    from_scale, to_scale = meth.to_s.split("_to_")
    return temp.to_f if from_scale == to_scale # no kelvin roundtrip
    TO_TEMP_SCALE_FROM_K[to_scale].call(FROM_TEMP_SCALE_TO_K[from_scale].call(temp)).round(2)
  end

end
```

Converts all eight scales to any other scale, by means of method_missing.

Usage:

```ruby
TempConvert.kelvin_to_celsius 100 #=> -173.15
TempConvert.kelvin_to_fahrenheit 100 #=> -279.67
TempConvert.kelvin_to_rankine 100 #=> 180.0
TempConvert.kelvin_to_delisle 100 #=> 409.73
TempConvert.kelvin_to_newton 100 #=> -57.14
TempConvert.kelvin_to_reaumur 100 #=> -138.52
TempConvert.kelvin_to_roemer 100 #=> -83.4

TempConvert.newton_to_celsius 100 #=> 303.03
TempConvert.newton_to_fahrenheit 100 #=> 577.45
# All 64 combinations possible
```



## Run BASIC


```runbasic
[loop]
input "Kelvin Degrees";kelvin
if kelvin <= 0 then end         ' zero or less ends the program
celcius		= kelvin - 273.15
fahrenheit	= kelvin * 1.8 - 459.67
rankine		= kelvin * 1.8
print kelvin;" kelvin is equal to ";celcius; " degrees celcius and ";fahrenheit;" degrees fahrenheit and ";rankine; " degrees rankine"
goto [loop]
```



## Scala

```Scala
object TemperatureConversion extends App {

  def kelvinToCelsius(k: Double) = k + 273.15

  def kelvinToFahrenheit(k: Double) = k * 1.8 - 459.67

  def kelvinToRankine(k: Double) = k * 1.8

  if (args.length == 1) {
    try {
      val kelvin = args(0).toDouble
      if (kelvin >= 0) {
        println(f"K  $kelvin%2.2f")
        println(f"C  ${kelvinToCelsius(kelvin)}%2.2f")
        println(f"F  ${kelvinToFahrenheit(kelvin)}%2.2f")
        println(f"R  ${kelvinToRankine(kelvin)}%2.2f")
      } else println("%2.2f K is below absolute zero", kelvin)

    } catch {
      case e: NumberFormatException => System.out.println(e)
      case e: Throwable => {
        println("Some other exception type:")
        e.printStackTrace()
      }
    }
  } else println("Temperature not given.")
}
```

```txt

K  21,00
C  294,15
F  -421,87
R  37,80

```



## Scheme



```scheme

(import (scheme base)
        (scheme read)
        (scheme write))

(define (kelvin->celsius k)
  (- k 273.15))

(define (kelvin->fahrenheit k)
  (- (* k 1.8) 459.67))

(define (kelvin->rankine k)
  (* k 1.8))

;; Run the program
(let ((k (begin (display "Kelvin    : ") (flush-output-port) (read))))
  (when (number? k)
    (display "Celsius   : ") (display (kelvin->celsius k)) (newline)
    (display "Fahrenheit: ") (display (kelvin->fahrenheit k)) (newline)
    (display "Rankine   : ") (display (kelvin->rankine k)) (newline)))

```


```txt

Kelvin    : 21
Celsius   : -252.14999999999998
Fahrenheit: -421.87
Rankine   : 37.800000000000004

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const func float: celsius (in float: kelvin) is
  return kelvin - 273.15;

const func float: fahrenheit (in float: kelvin) is
  return kelvin * 1.8 - 459.67;

const func float: rankine (in float: kelvin) is
  return kelvin * 1.8;

const proc: main is func
  local
    var float: kelvin is 0.0;
  begin
    write("Enter temperature in kelvin: ");
    readln(kelvin);
    writeln("K: " <& kelvin             digits 2 lpad 7);
    writeln("C: " <& celsius(kelvin)    digits 2 lpad 7);
    writeln("F: " <& fahrenheit(kelvin) digits 2 lpad 7);
    writeln("R: " <& rankine(kelvin)    digits 2 lpad 7);
  end func;
```


```txt

Enter temperature in kelvin: 21.0
K:   21.00
C: -252.15
F: -421.87
R:   37.80

```



## Sidef

```ruby
var scale = Hash(
    Celcius    => Hash.new(factor => 1  , offset => -273.15 ),
    Rankine    => Hash.new(factor => 1.8, offset =>    0    ),
    Fahrenheit => Hash.new(factor => 1.8, offset => -459.67 ),
);

var kelvin = Sys.readln("Enter a temperature in Kelvin: ").to_n;
kelvin >= 0 || die "No such temperature!";

scale.keys.sort.each { |key|
    printf("%12s:%8.2f\n", key, kelvin*scale{key}{:factor} + scale{key}{:offset});
}
```

```txt

Enter a temperature in Kelvin: 256
     Celcius:  -17.15
  Fahrenheit:    1.13
     Rankine:  460.80

```




## Swift



```swift

func KtoC(kelvin : Double)->Double{

    return kelvin-273.15
}

func KtoF(kelvin : Double)->Double{

    return ((kelvin-273.15)*1.8)+32
}

func KtoR(kelvin : Double)->Double{

    return ((kelvin-273.15)*1.8)+491.67
}

var k// input
print("\(k) Kelvin")
var c=KtoC(kelvin : k)
print("\(c) Celsius")
var f=KtoF(kelvin : k)
print("\(f) Fahrenheit")
var r=KtoR(kelvin : k)
print("\(r) Rankine")

```




## Tcl


```tcl
proc temps {k} {
    set c [expr {$k - 273.15}]
    set r [expr {$k / 5.0 * 9.0}]
    set f [expr {$r - 459.67}]
    list $k $c $f $r
}
```



Demonstrating:

```tcl
puts -nonewline "Enter a temperature in K: "
flush stdout
lassign [temps [gets stdin]] k c f r
puts [format "K: %.2f" $k]
puts [format "C: %.2f" $c]
puts [format "F: %.2f" $f]
puts [format "R: %.2f" $r]
```

```txt

Enter a temperature in K: 21
K: 21.00
C: -252.15
F: -421.87
R: 37.80

```



## UNIX Shell


=
## Korn Shell
=
```bash
#!/bin/ksh
# Temperature conversion
typeset tt[1]=0.00 tt[2]=273.15 tt[3]=373.15
for i in {1..3}
do
	((t=tt[i]))
	echo $i
	echo "Kelvin:     $t K"
	echo "Celsius:    $((t-273.15)) C"
	echo "Fahrenheit: $((t*18/10-459.67)) F"
	echo "Rankine:    $((t*18/10)) R"
done
```


=
## bash
=
```bash
#!/bin/bash
# Temperature conversion
tt[1]=0.00; tt[2]=273.15; tt[3]=373.15
for i in {1..3}
do
	t=${tt[$i]}
	echo $i
	echo "Kelvin:     $t K"
	echo "Celsius:    $(bc<<<"scale=2;$t-273.15") C"
	echo "Fahrenheit: $(bc<<<"scale=2;$t*18/10-459.67") F"
	echo "Rankine:    $(bc<<<"scale=2;$t*18/10") R"
done
```



## Ursa


```ursa
decl double k
while true
	out "Temp. in Kelvin? " console
	set k (in double console)
	out "K\t" k endl "C\t" (- k 273.15) endl console
	out "F\t" (- (* k 1.8) 459.67) endl "R\t" (* k 1.8) endl endl console
end while
```



## VBA



```vb

Option Explicit

Sub Main_Conv_Temp()
Dim K As Single, Result As Single
    K = 21
    Debug.Print "Input in Kelvin      : " & Format(K, "0.00")
    Debug.Print "Output in Celsius    : " & IIf(ConvTemp(Result, K, "C"), Format(Result, "0.00"), False)
    Debug.Print "Output in Fahrenheit : " & IIf(ConvTemp(Result, K, "F"), Format(Result, "0.00"), False)
    Debug.Print "Output in Rankine    : " & IIf(ConvTemp(Result, K, "R"), Format(Result, "0.00"), False)
    Debug.Print "Output error         : " & IIf(ConvTemp(Result, K, "T"), Format(Result, "0.00"), False)
End Sub

Function ConvTemp(sngReturn As Single, Kelv As Single, InWhat As String) As Boolean
Dim ratio As Single

    ConvTemp = True
    ratio = 9 / 5
    Select Case UCase(InWhat)
        Case "C": sngReturn = Kelv - 273.15
        Case "F": sngReturn = (Kelv * ratio) - 459.67
        Case "R": sngReturn = Kelv * ratio
        Case Else: ConvTemp = False
    End Select
End Function

```

```txt
Input in Kelvin      : 21,00
Output in Celsius    : -252,15
Output in Fahrenheit : -421,87
Output in Rankine    : 37,80
Output error         : False
```



## VBScript


```vb

WScript.StdOut.Write "Enter the temperature in Kelvin:"
tmp = WScript.StdIn.ReadLine

WScript.StdOut.WriteLine "Kelvin: " & tmp
WScript.StdOut.WriteLine "Fahrenheit: " & fahrenheit(CInt(tmp))
WScript.StdOut.WriteLine "Celsius: " & celsius(CInt(tmp))
WScript.StdOut.WriteLine "Rankine: " & rankine(CInt(tmp))

Function fahrenheit(k)
	fahrenheit = (k*1.8)-459.67
End Function

Function celsius(k)
	celsius = k-273.15
End Function

Function rankine(k)
	rankine = (k-273.15)*1.8+491.67
End Function

```


```txt

C:\>cscript /nologo tmp.vbs
Enter the temperature in Kelvin:21
Kelvin: 21
Fahrenheit: -421.87
Celsius: -252.15
Rankine: 37.8000000000001

```



## Visual FoxPro


```vfp
#DEFINE ABSZC 273.16
#DEFINE ABSZF 459.67
LOCAL k As Double, c As Double, f As Double, r As Double, n As Integer, ;
cf As String
n = SET("Decimals")
cf = SET("Fixed")
SET DECIMALS TO 2
SET FIXED ON
CLEAR
DO WHILE .T.
    k = VAL(INPUTBOX("Degrees Kelvin:", "Temperature"))
    IF k <= 0
	EXIT
    ENDIF
    ? "K:", k
    c = k - ABSZC
    ? "C:", c
    f = 1.8*c + 32
    ? "F:", f
    r = f + ABSZF
    ? "R:", r
    ?
ENDDO
SET FIXED &cf
SET DECIMALS TO n
```

```txt

K: 21.00
C: -252.15
F: -421.87
R: 37.80

```



## XLISP


```xlisp
(DEFUN CONVERT-TEMPERATURE ()
    (SETQ *FLONUM-FORMAT* "%.2f")
    (DISPLAY "Enter a temperature in Kelvin.")
    (NEWLINE)
    (DISPLAY "> ")
    (DEFINE K (READ))
    (DISPLAY `(K = ,K))
    (NEWLINE)
    (DISPLAY `(C = ,(- K 273.15)))
    (NEWLINE)
    (DISPLAY `(F = ,(- (* K 1.8) 459.67)))
    (NEWLINE)
    (DISPLAY `(R = ,(* K 1.8))))
```

```txt
(CONVERT-TEMPERATURE)
Enter a temperature in Kelvin.
> 291.5
(K = 291.50)
(C = 18.35)
(F = 65.03)
(R = 524.70)
```



## XPL0


```XPL0
include c:\cxpl\codes;
real K, C, F, R;
[ChOut(0, ^K);  K:= RlIn(0);
C:= K - 273.15;
ChOut(0, ^C);  RlOut(0, C);  CrLf(0);
F:= 1.8*C + 32.0;
ChOut(0, ^F);  RlOut(0, F);  CrLf(0);
R:= F + 459.67;
ChOut(0, ^R);  RlOut(0, R);  CrLf(0);
]
```

```txt

K 21
C -252.15000
F -421.87000
R   37.80000

```



## zkl


```zkl
K:=ask(0,"Kelvin: ").toFloat();
println("K %.2f".fmt(K));
println("F %.2f".fmt(K*1.8 - 459.67));
println("C %.2f".fmt(K - 273.15));
println("R %.2f".fmt(K*1.8));
```

```txt

Kelvin: 373.15
K 373.15
F 212.00
C 100.00
R 671.67

```



## ZX Spectrum Basic



```zxbasic
10 REM Translation of traditional basic version
20 INPUT "Kelvin Degrees? ";k
30 IF k <= 0 THEN STOP: REM A value of zero or less will end program
40 LET c = k - 273.15
50 LET f = k * 1.8 - 459.67
60 LET r = k * 1.8
70 PRINT k; " Kelvin is equivalent to"
80 PRINT c; " Degrees Celsius"
90 PRINT f; " Degrees Fahrenheit"
100 PRINT r; " Degrees Rankine"
110 GO TO 20
```

