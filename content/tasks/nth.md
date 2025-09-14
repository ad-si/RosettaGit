+++
title = "N'th"
description = ""
date = 2019-10-18T11:45:15Z
aliases = []
[extra]
id = 17382
[taxonomies]
categories = ["task", "Ordinal numbers"]
tags = []
languages = [
  "ada",
  "algol_68",
  "applescript",
  "applesoft_basic",
  "autohotkey",
  "awk",
  "babel",
  "bacon",
  "batch_file",
  "bbc_basic",
  "befunge",
  "c",
  "cobol",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "elena",
  "elixir",
  "erre",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "gambas",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "liberty_basic",
  "lua",
  "maple",
  "mathematica",
  "microsoft_small_basic",
  "miniscript",
  "ocaml",
  "oforth",
  "perl_6",
  "phix",
  "pl_i",
  "powershell",
  "prolog",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "set_lang",
  "sidef",
  "sinclair_zx81_basic",
  "sql",
  "stata",
  "swift",
  "tcl",
  "ubasic_4th",
  "vba",
  "xbasic",
  "xlisp",
  "zkl",
  "zx_spectrum_basic",
]
+++

{{task|Ordinal numbers}} [[Category:String manipulation]]
Write a function/method/subroutine/... that when given an integer greater than or equal to zero returns a string of the number followed by an apostrophe then the [[wp:Ordinal number (linguistics)|ordinal suffix]].

Example returns would include <code>1'st 2'nd 3'rd 11'th 111'th 1001'st 1012'th</code>

## Task

Use your routine to show here the output for ''at least'' the following (inclusive) ranges of integer inputs:
<code>0..25, 250..265, 1000..1025</code>

'''Note:''' apostrophes are now ''optional'' to allow correct apostrophe-less English.


## Ada



```Ada
with Ada.Text_IO;

procedure Nth is

   function Suffix(N: Natural) return String is
   begin
      if    N mod 10 = 1 and then N mod 100 /= 11 then return "st";
      elsif N mod 10 = 2 and then N mod 100 /= 12 then return "nd";
      elsif N mod 10 = 3 and then N mod 100 /= 13 then return "rd";
      else return "th";
      end if;
   end Suffix;

   procedure Print_Images(From, To: Natural) is
   begin
      for I in From .. To loop
	 Ada.Text_IO.Put(Natural'Image(I) & Suffix(I));
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Images;

begin
   Print_Images(   0,   25);
   Print_Images( 250,  265);
   Print_Images(1000, 1025);
end Nth;
```


```txt
 0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
 250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```



## ALGOL 68

```algol68
# PROC to suffix a number with st, nd, rd or th as appropriate #
PROC nth = ( INT number )STRING:
BEGIN

    INT number mod 100 = number MOD 100;

# RESULT #
    whole( number, 0 )
  + IF number mod 100 >= 10 AND number mod 100 <= 20
    THEN
        # numbers in the range 10 .. 20 always have "th" #
        "th"
    ELSE
        # not in the range 10 .. 20, suffix is st, nd, rd or th #
        # depending on the final digit                          #
        CASE number MOD 10
        IN  # 1 # "st"
        ,   # 2 # "nd"
        ,   # 3 # "rd"
        OUT       "th"
        ESAC
    FI
END; # nth #

# PROC to test nth, displays nth for all numbers in the range from .. to  #
PROC test nth = ( INT from, INT to )VOID:
BEGIN
    INT test count := 0;
    FOR test value FROM from TO to
    DO
        STRING test result = nth( test value );
        print( ( "        "[ 1 : 8 - UPB test result ], nth( test value ) ) );
        test count +:= 1;
        IF test count MOD 8 = 0
        THEN
            print( ( newline ) )
        FI
    OD;
    print( ( newline ) )
END; # test nth #


main: (

    test nth(    0,   25 );
    test nth(  250,  265 );
    test nth( 1000, 1025 )

)
```

```txt

     0th     1st     2nd     3rd     4th     5th     6th     7th
     8th     9th    10th    11th    12th    13th    14th    15th
    16th    17th    18th    19th    20th    21st    22nd    23rd
    24th    25th

   250th   251st   252nd   253rd   254th   255th   256th   257th
   258th   259th   260th   261st   262nd   263rd   264th   265th

  1000th  1001st  1002nd  1003rd  1004th  1005th  1006th  1007th
  1008th  1009th  1010th  1011th  1012th  1013th  1014th  1015th
  1016th  1017th  1018th  1019th  1020th  1021st  1022nd  1023rd
  1024th  1025th


```




## AppleScript


```AppleScript
-- ORDINAL STRINGS -----------------------------------------------------------

-- ordinalString :: Int -> String
on ordinalString(n)
    (n as string) & ordinalSuffix(n)
end ordinalString

-- ordinalSuffix :: Int -> String
on ordinalSuffix(n)
    set modHundred to n mod 100
    if (11 ≤ modHundred) and (13 ≥ modHundred) then
        "th"
    else
        item ((n mod 10) + 1) of ¬
            {"th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th"}
    end if
end ordinalSuffix

-- TEST ----------------------------------------------------------------------
on run

    -- showOrdinals :: [Int] -> [String]
    script showOrdinals
        on |λ|(lstInt)
            map(ordinalString, lstInt)
        end |λ|
    end script

    map(showOrdinals, ¬
        map(uncurry(enumFromTo), ¬
            [[0, 25], [250, 265], [1000, 1025]]))
end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

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

-- uncurry :: Handler (a -> b -> c) -> Script |λ| ((a, b) -> c)
on uncurry(f)
    script
        on |λ|(xy)
            set {x, y} to xy
            mReturn(f)'s |λ|(x, y)
        end |λ|
    end script
end uncurry
```

```txt
{{"0th", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th",
"10th", "11th", "12th", "13th", "14th", "15th", "16th", "17th", "18th",
"19th", "20th", "21st", "22nd", "23rd", "24th", "25th"},
{"250th", "251st", "252nd", "253rd", "254th", "255th", "256th", "257th",
"258th", "259th", "260th", "261st", "262nd", "263rd", "264th", "265th"},
{"1000th", "1001st", "1002nd", "1003rd", "1004th", "1005th", "1006th",
"1007th", "1008th", "1009th", "1010th", "1011th", "1012th", "1013th",
"1014th", "1015th", "1016th", "1017th", "1018th", "1019th", "1020th",
"1021st", "1022nd", "1023rd", "1024th", "1025th"}}
```



## Applesoft BASIC


```ApplesoftBasic
0 OP = 1
10 FOR N = 0 TO 25 : GOSUB 100 : NEXT
20 FOR N = 250 TO 265 : GOSUB 100 : NEXT
30 FOR N = 1000 TO 1025 : GOSUB 100 : NEXT
40 END

100 GOSUB 200"NTH
110 PRINT NTH$ " ";
120 RETURN

200 M1 = N - INT(N / 10) * 10
210 M2 = N - INT(N / 100) * 100
220 NTH$ = "TH"
230 IF M1 = 1 AND M2 <> 11 THEN NTH$ = "ST"
240 IF M1 = 2 AND M2 <> 12 THEN NTH$ = "ND"
250 IF M1 = 3 AND M2 <> 13 THEN NTH$ = "RD"
260 IF NOT OP THEN NTH$ = "'" + NTH$
270 NTH$ = STR$(N) + NTH$
280 RETURN
```

```txt
0'TH 1'ST 2'ND 3'RD 4'TH 5'TH 6'TH 7'TH 8'TH 9'TH 10'TH 11'TH 12'TH 13'TH 14'TH 15'TH 16'TH 17'TH 18'TH 19'TH 20'TH 21'ST 22'ND 23'RD 24'TH 25'TH 250'TH 251'ST 252'ND 253'RD 254'TH 255'TH 256'TH 257'TH 258'TH 259'TH 260'TH 261'ST 262'ND 263'RD 264'TH 265'TH 1000'TH 1001'ST 1002'ND 1003'RD 1004'TH 1005'TH 1006'TH 1007'TH 1008'TH 1009'TH 1010'TH 1011'TH 1012'TH 1013'TH 1014'TH 1015'TH 1016'TH 1017'TH 1018'TH 1019'TH 1020'TH 1021'ST 1022'ND 1023'RD 1024'TH 1025'TH
```



## AutoHotkey

```AutoHotkey
for k, v in [[0, 25], [250, 265], [1000, 1025]] {
	while v[1] <= v[2] {
		Out .= Ordinal(v[1]) " "
		v[1]++
	}
	Out .= "`n"
}
MsgBox, % Out

Ordinal(n) {
	s2 := Mod(n, 100)
	if (s2 > 10 && s2 < 14)
		return n "th"
	s1 := Mod(n, 10)
	return n (s1 = 1 ? "st" : s1 = 2 ? "nd" : s1 = 3 ? "rd" : "th")
}
```

```txt
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```



## AWK


```AWK

# syntax: GAWK -f NTH.AWK
BEGIN {
    prn(0,25)
    prn(250,265)
    prn(1000,1025)
    exit(0)
}
function prn(start,stop,  i) {
    printf("%d-%d: ",start,stop)
    for (i=start; i<=stop; i++) {
      printf("%d%s ",i,nth(i))
    }
    printf("\n")
}
function nth(yearday,  nthday) {
    if (yearday ~ /1[1-3]$/) {         # 11th,12th,13th
      nthday = "th"
    }
    else if (yearday ~ /1$/) {         # 1st,21st,31st,etc.
      nthday = "st"
    }
    else if (yearday ~ /2$/) {         # 2nd,22nd,32nd,etc.
      nthday = "nd"
    }
    else if (yearday ~ /3$/) {         # 3rd,23rd,33rd,etc.
      nthday = "rd"
    }
    else if (yearday ~ /[0456789]$/) { # 4th-10th,20th,24th-30th,etc.
      nthday = "th"
    }
    return(nthday)
}

```

```txt

0-25: 0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250-265: 250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000-1025: 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## Babel



```babel
((irregular ("st" "nd" "rd"))

(main
    {(0 250 1000)
    { test ! "\n" << }
    each})

(test {
    <-
    {iter 1 - -> dup <- + ordinalify ! <<
        {iter 10 %} {"  "} {"\n"} ifte << }
    26 times})

(ordinalify {
    <-
    {{ -> dup <- 100 % 10 cugt } !
     { -> dup <- 100 % 14 cult } !
     and not
     { -> dup <- 10  % 0  cugt } !
     { -> dup <- 10  % 4  cult } !
     and
     and}
        { -> dup
            <- %d "'"
            irregular -> 10 % 1 - ith
            . . }
        { -> %d "'th" . }
    ifte }))
```


```txt
25'th  24'th  23'rd  22'nd  21'st  20'th  19'th
18'th  17'th  16'th  15'th  14'th  13'th  12'th  11'th  10'th  9'th
8'th  7'th  6'th  5'th  4'th  3'rd  2'nd  1'st  0'th
275'th  274'th  273'rd  272'nd  271'st  270'th  269'th
268'th  267'th  266'th  265'th  264'th  263'rd  262'nd  261'st  260'th  259'th
258'th  257'th  256'th  255'th  254'th  253'rd  252'nd  251'st  250'th
1025'th  1024'th  1023'rd  1022'nd  1021'st  1020'th  1019'th
1018'th  1017'th  1016'th  1015'th  1014'th  1013'th  1012'th  1011'th  1010'th  1009'th
1008'th  1007'th  1006'th  1005'th  1004'th  1003'rd  1002'nd  1001'st  1000'th
```



## BaCon


```freebasic
' Nth (sans apostrophes)
FUNCTION nth$(NUMBER n) TYPE STRING
    LOCAL suffix
    IF n < 0 THEN RETURN STR$(n)
    IF MOD(n, 100) >= 11 AND MOD(n, 100) <= 13 THEN
        suffix = "th"
    ELSE
        suffix = MID$("thstndrdthththththth", MOD(n, 10) * 2 + 1, 2)
    ENDIF
    RETURN CONCAT$(STR$(n), suffix)
END FUNCTION

' Test a few ranges
FOR i = 1 TO 4
    READ first, last
    per = 1
    FOR n = first TO last
        PRINT nth$(n) FORMAT "%s "
        ' limit to 10 entries per line
        IF per = 10 OR n = last THEN
            per = 1
            PRINT
        ELSE
            INCR per
        ENDIF
    NEXT
NEXT
DATA 0, 25, 250, 265, 1000, 1025, -20, -11
```


```txt
prompt$ ./nth
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th
10th 11th 12th 13th 14th 15th 16th 17th 18th 19th
20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th
260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th
1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th
1020th 1021st 1022nd 1023rd 1024th 1025th
-20 -19 -18 -17 -16 -15 -14 -13 -12 -11
```



## Batch File


```dos
@echo off
::Main thing...
call :Nth 0 25
call :Nth 250 265
call :Nth 1000 1025
pause
exit /b

::The subroutine
:Nth <lbound> <ubound>
setlocal enabledelayedexpansion
for /l %%n in (%~1,1,%~2) do (
	set curr_num=%%n
	set "out=%%nth"
	if !curr_num:~-1!==1 (set "out=%%nst")
	if !curr_num:~-1!==2 (set "out=%%nnd")
	if !curr_num:~-1!==3 (set "out=%%nrd")
	set "range_output=!range_output! !out!"
)
echo."!range_output:~1!"
goto :EOF
```

```txt
"0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11st 12nd 13rd 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th"
"250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th"
"1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011st 1012nd 1013rd 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th"
Press any key to continue . . .
```



## BBC BASIC

```bbcbasic
      PROCNth(   0,  25)
      PROCNth( 250, 265)
      PROCNth(1000,1025)
      END

      DEF PROCNth(s%,e%)
      LOCAL i%,suff$
      FOR i%=s% TO e%
        suff$="th"
        IF i% MOD 10 = 1 AND i% MOD 100 <> 11 suff$="st"
        IF i% MOD 10 = 2 AND i% MOD 100 <> 12 suff$="nd"
        IF i% MOD 10 = 3 AND i% MOD 100 <> 13 suff$="rd"
        PRINT STR$i%+suff$+" ";
      NEXT
      PRINT
      ENDPROC
```


```txt
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## Befunge

The bottom section of code contains the "subroutine" that calculates the ordinal of a given number; the top section generates the list of values to test.


```befunge>0
55*:>1-\:0\`!v
#v$#$<^:\+*8"}"_
 >35*:>1-\:0\`!v
#v$#$<^:\+*2"}"_
5< v$_v#!::-<0*5
@v <,*>#81#4^# _

>>:0\>:55+%68*v:
tsnr |:/+ 55\+<,
htdd >$>:#,_$:vg
v"d"\*!`3:%+55<9
>%55+/1-!!*:8g,^
```


```txt
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th 250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```



## C


```c
#include <stdio.h>

char* addSuffix(int num, char* buf, size_t len)
{
    char *suffixes[4] = { "th", "st", "nd", "rd" };
    int i;

    switch (num % 10)
    {
        case 1 : i = (num % 100 == 11) ? 0 : 1;
	         break;
        case 2 : i = (num % 100 == 12) ? 0 : 2;
                 break;
        case 3 : i = (num % 100 == 13) ? 0 : 3;
                 break;
        default: i = 0;
    };

    snprintf(buf, len, "%d%s", num, suffixes[i]);
    return buf;
}

int main(void)
{
    int i;

    printf("Set [0,25]:\n");
    for (i = 0; i < 26; i++)
    {
        char s[5];
        printf("%s ", addSuffix(i, s, 5));
    }
    putchar('\n');

    printf("Set [250,265]:\n");
    for (i = 250; i < 266; i++)
    {
        char s[6];
        printf("%s ", addSuffix(i, s, 6));
    }
    putchar('\n');

    printf("Set [1000,1025]:\n");
    for (i = 1000; i < 1026; i++)
    {
        char s[7];
        printf("%s ", addSuffix(i, s, 7));
    }
    putchar('\n');

    return 0;
}
```

```txt
Set [0,25] :
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
Set [250,265] :
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
Set [1000,1025] :
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```



## C++


```cpp
#include <string>
#include <iostream>

using namespace std;

string Suffix(int num)
{
    switch (num % 10)
    {
        case 1 : if(num % 100 != 11) return "st";
           break;
        case 2 : if(num % 100 != 12) return "nd";
           break;
        case 3 : if(num % 100 != 13) return "rd";
    }

    return "th";
}

int main()
{
    cout << "Set [0,25]:" << endl;
    for (int i = 0; i < 26; i++)
        cout << i << Suffix(i) << " ";

    cout << endl;

    cout << "Set [250,265]:" << endl;
    for (int i = 250; i < 266; i++)
        cout << i << Suffix(i) << " ";

    cout << endl;

    cout << "Set [1000,1025]:" << endl;
    for (int i = 1000; i < 1026; i++)
        cout << i << Suffix(i) << " ";

    cout << endl;

    return 0;
}
```

```txt
Set [0,25] :
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
Set [250,265] :
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
Set [1000,1025] :
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```


## C#
```c#

class Program
{
    private static string Ordinalize(int i)
    {
        i = Math.Abs(i);

        if (new[] {11, 12, 13}.Contains(i%100))
            return i + "th";

        switch (i%10)
        {
            case 1:
                return i + "st";
            case 2:
                return i + "nd";
            case 3:
                return i + "rd";
            default:
                return i + "th";
        }
    }

    static void Main()
    {
        Console.WriteLine(string.Join(" ", Enumerable.Range(0, 26).Select(Ordinalize)));
        Console.WriteLine(string.Join(" ", Enumerable.Range(250, 16).Select(Ordinalize)));
        Console.WriteLine(string.Join(" ", Enumerable.Range(1000, 26).Select(Ordinalize)));
    }
}

```


```txt
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```


=={{Header|Clojure}}==

```clojure

(defn n-th [n]
  (str n
    (let [rem (mod n 100)]
      (if (and (>= rem 11) (<= rem 13))
        "th"
        (condp = (mod n 10)
          1 "st"
          2 "nd"
          3 "rd"
          "th")))))

(apply str (interpose " " (map n-th (range 0 26))))
(apply str (interpose " " (map n-th (range 250 266))))
(apply str (interpose " " (map n-th (range 1000 1026))))

```

```txt

"0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th"

"250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th"

"1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th"

```


Alternatively, if you want to print the full ordinal English, it becomes trivial with pprint:

```clojure

(apply str (interpose " " (map #(clojure.pprint/cl-format nil "~:R" %) (range 0 26))))

```

```txt

"zeroth first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth thirteenth fourteenth fifteenth sixteenth seventeenth eighteenth nineteenth twentieth twenty-first twenty-second twenty-third twenty-fourth twenty-fifth"

```



## COBOL

COBOL stores numbers in decimal form, so there is no need to use a modulo function: the last digit or the last two digits can be extracted directly.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. NTH-PROGRAM.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-NUMBER.
    05 N               PIC 9(8).
    05 LAST-TWO-DIGITS PIC 99.
    05 LAST-DIGIT      PIC 9.
    05 N-TO-OUTPUT     PIC Z(7)9.
    05 SUFFIX          PIC AA.
PROCEDURE DIVISION.
TEST-PARAGRAPH.
    PERFORM NTH-PARAGRAPH VARYING N FROM 0 BY 1 UNTIL N IS GREATER THAN 25.
    PERFORM NTH-PARAGRAPH VARYING N FROM 250 BY 1 UNTIL N IS GREATER THAN 265.
    PERFORM NTH-PARAGRAPH VARYING N FROM 1000 BY 1 UNTIL N IS GREATER THAN 1025.
    STOP RUN.
NTH-PARAGRAPH.
    MOVE 'TH' TO SUFFIX.
    MOVE N (7:2) TO LAST-TWO-DIGITS.
    IF LAST-TWO-DIGITS IS LESS THAN 4,
    OR LAST-TWO-DIGITS IS GREATER THAN 20,
    THEN PERFORM DECISION-PARAGRAPH.
    MOVE N TO N-TO-OUTPUT.
    DISPLAY N-TO-OUTPUT WITH NO ADVANCING.
    DISPLAY SUFFIX WITH NO ADVANCING.
    DISPLAY SPACE WITH NO ADVANCING.
DECISION-PARAGRAPH.
    MOVE N (8:1) TO LAST-DIGIT.
    IF LAST-DIGIT IS EQUAL TO 1 THEN MOVE 'ST' TO SUFFIX.
    IF LAST-DIGIT IS EQUAL TO 2 THEN MOVE 'ND' TO SUFFIX.
    IF LAST-DIGIT IS EQUAL TO 3 THEN MOVE 'RD' TO SUFFIX.
```

Output:

```txt
       0TH        1ST        2ND        3RD        4TH        5TH        6TH        7TH        8TH        9TH       10TH       11TH       12TH       13TH       14TH       15TH       16TH       17TH       18TH       19TH       20TH       21ST       22ND       23RD       24TH       25TH      250TH      251ST      252ND      253RD      254TH      255TH      256TH      257TH      258TH      259TH      260TH      261ST      262ND      263RD      264TH      265TH     1000TH     1001ST     1002ND     1003RD     1004TH     1005TH     1006TH     1007TH     1008TH     1009TH     1010TH     1011TH     1012TH     1013TH     1014TH     1015TH     1016TH     1017TH     1018TH     1019TH     1020TH     1021ST     1022ND     1023RD     1024TH     1025TH
```



## Common Lisp


```lisp
(defun add-suffix (number)
  (let* ((suffixes #10("th"  "st"  "nd"  "rd"  "th"))
         (last2 (mod number 100))
         (last-digit (mod number 10))
         (suffix (if (< 10 last2 20)
                   "th"
                   (svref suffixes last-digit))))
    (format nil "~a~a" number suffix)))

```



A more concise, albeit less readable version:

```lisp
(defun add-suffix (n)
  (format nil "~d'~:[~[th~;st~;nd~;rd~:;th~]~;th~]" n (< (mod (- n 10) 100) 10) (mod n 10)))

```



Display the results:

```lisp
(loop for (low high) in '((0 25) (250 265) (1000 1025))
      do (progn
           (format t "~a to ~a: " low high)
           (loop for n from low to high
                 do (format t "~a " (add-suffix n))
                 finally (terpri))))
```


```txt
0 to 25: 0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250 to 265: 250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000 to 1025: 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```



## Crystal

```ruby
struct Int
  def ordinalize
    num = self.abs
    ordinal = if (11..13).includes?(num % 100)
      "th"
    else
      case num % 10
        when 1; "st"
        when 2; "nd"
        when 3; "rd"
        else    "th"
      end
    end
    "#{self}#{ordinal}"
  end
end

[(0..25),(250..265),(1000..1025)].each{|r| puts r.map{ |n| n.ordinalize }.join(", "); puts}

```

```txt

0th, 1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th, 10th, 11th, 12th, 13th, 14th, 15th, 16th, 17th, 18th, 19th, 20th, 21st, 22nd, 23rd, 24th, 25th

250th, 251st, 252nd, 253rd, 254th, 255th, 256th, 257th, 258th, 259th, 260th, 261st, 262nd, 263rd, 264th, 265th

1000th, 1001st, 1002nd, 1003rd, 1004th, 1005th, 1006th, 1007th, 1008th, 1009th, 1010th, 1011th, 1012th, 1013th, 1014th, 1015th, 1016th, 1017th, 1018th, 1019th, 1020th, 1021st, 1022nd, 1023rd, 1024th, 1025th

```


=={{Header|D}}==
```d
import std.stdio, std.string, std.range, std.algorithm;

string nth(in uint n) pure {
    static immutable suffix = "th st nd rd th th th th th th".split;
    return "%d'%s".format(n, (n % 100 <= 10 || n % 100 > 20) ?
                             suffix[n % 10] : "th");
}

void main() {
    foreach (r; [iota(26), iota(250, 266), iota(1000, 1026)])
        writefln("%-(%s %)", r.map!nth);
}
```

```txt
0'th 1'st 2'nd 3'rd 4'th 5'th 6'th 7'th 8'th 9'th 10'th 11'th 12'th 13'th 14'th 15'th 16'th 17'th 18'th 19'th 20'th 21'st 22'nd 23'rd 24'th 25'th
250'th 251'st 252'nd 253'rd 254'th 255'th 256'th 257'th 258'th 259'th 260'th 261'st 262'nd 263'rd 264'th 265'th
1000'th 1001'st 1002'nd 1003'rd 1004'th 1005'th 1006'th 1007'th 1008'th 1009'th 1010'th 1011'th 1012'th 1013'th 1014'th 1015'th 1016'th 1017'th 1018'th 1019'th 1020'th 1021'st 1022'nd 1023'rd 1024'th 1025'th
```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'math;
import system'routines;

extension op
{
    ordinalize()
    {
        int i := self.Absolute;
        if (new int[]::(11,12,13).ifExists(i.mod:100))
        {
            ^ i.Printable + "th"
        };

        (i.mod:10) =>
            1 { ^ i.Printable + "st" }
            2 { ^ i.Printable + "nd" }
            3 { ^ i.Printable + "rd" };

        ^ i.Printable + "th"
    }
}

public program()
{
    console.printLine(new Range(0,26).selectBy(mssgconst ordinalize<op>[0]));
    console.printLine(new Range(250,26).selectBy(mssgconst ordinalize<op>[0]));
    console.printLine(new Range(1000,26).selectBy(mssgconst ordinalize<op>[0]))
}
```

```txt

0th,1st,2nd,3rd,4th,5th,6th,7th,8th,9th,10th,11th,12th,13th,14th,15th,16th,17th,18th,19th,20th,21st,22nd,23rd,24th,25th
250th,251st,252nd,253rd,254th,255th,256th,257th,258th,259th,260th,261st,262nd,263rd,264th,265th,266th,267th,268th,269th,270th,271st,272nd,273rd,274th,275th
1000th,1001st,1002nd,1003rd,1004th,1005th,1006th,1007th,1008th,1009th,1010th,1011th,1012th,1013th,1014th,1015th,1016th,1017th,1018th,1019th,1020th,1021st,1022nd,1023rd,1024th,1025th

```



## Elixir


```elixir
defmodule RC do
  def ordinalize(n) do
    num = abs(n)
    ordinal = if rem(num, 100) in 4..20 do
                "th"
              else
                case rem(num, 10) do
                  1 -> "st"
                  2 -> "nd"
                  3 -> "rd"
                  _ -> "th"
                end
              end
    "#{n}#{ordinal}"
  end
end

Enum.each([0..25, 250..265, 1000..1025], fn range ->
  Enum.map(range, fn n -> RC.ordinalize(n) end) |> Enum.join(" ") |> IO.puts
end)
```


```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## ERRE


```ERRE

PROGRAM NTH_SOLVE

!
! for rosettacode.org
!

PROCEDURE NTH(S%,E%)
      LOCAL I%,SUFF$
      FOR I%=S% TO E% DO
        SUFF$="th"
        IF I% MOD 10=1 AND I% MOD 100<>11 THEN SUFF$="st" END IF
        IF I% MOD 10=2 AND I% MOD 100<>12 THEN SUFF$="nd" END IF
        IF I% MOD 10=3 AND I% MOD 100<>13 THEN SUFF$="rd" END IF
        PRINT(STR$(I%)+SUFF$+" ";)
      END FOR
      PRINT
END PROCEDURE

BEGIN
   NTH(0,25)
   NTH(250,265)
   NTH(1000,1025)
END PROGRAM

```

```txt

 0th  1st  2nd  3rd  4th  5th  6th  7th  8th  9th  10th  11th  12th  13th  14th
 15th  16th  17th  18th  19th  20th  21st  22nd  23rd  24th  25th
 250th  251st  252nd  253rd  254th  255th  256th  257th  258th  259th  260th
 261st  262nd  263rd  264th  265th
 1000th  1001st  1002nd  1003rd  1004th  1005th  1006th  1007th  1008th  1009th
 1010th  1011th  1012th  1013th  1014th  1015th  1016th  1017th  1018th  1019th
 1020th  1021st  1022nd  1023rd  1024th  1025th

```

=={{header|F_Sharp|F#}}==

```fsharp
open System

let ordinalsuffix n =
    let suffixstrings = [|"th"; "st"; "nd"; "rd"|]
    let (d, r) = Math.DivRem(n, 10)
    n.ToString() + suffixstrings.[ if r < 4 && (d &&& 1) = 0 then r else 0 ]


[<EntryPoint>]
let main argv =
    let show = (Seq.iter (ordinalsuffix >> (printf " %s"))) >> (Console.WriteLine)
    [0..25] |> show
    [250..265] |> show
    [1000..1025] |> show
    0

```

```txt
 0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
 250th 251th 252th 253th 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```



## Factor


```factor
USING: io kernel math math.order math.parser math.ranges qw
sequences ;
IN: rosetta-code.nth

: n'th ( n -- str )
    dup 10 /mod swap 1 = [ drop 0 ] when
    [ number>string ]
    [ 4 min qw{ th st nd rd th } nth ] bi* append ;

: n'th-demo ( -- )
    0 25 250 265 1000 1025 [ [a,b] ] 2tri@
    [ [ n'th write bl ] each nl ] tri@ ;

MAIN: n'th-demo
```

```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## Forth


```forth
: 'nth ( -- c-addr )  s" th st nd rd th th th th th th " drop ;
: .nth ( n -- )
  dup 10 20 within if 0 .r ." th " exit then
  dup 0 .r 10 mod 3 * 'nth + 3 type ;

: test ( n n -- )  cr do i 5 mod 0= if cr then i .nth loop ;
: tests ( -- )
  26 0 test  266 250 test  1026 1000 test ;

tests
```


```txt
0th 1st 2nd 3rd 4th
5th 6th 7th 8th 9th
10th 11th 12th 13th 14th
15th 16th 17th 18th 19th
20th 21st 22nd 23rd 24th
25th

250th 251st 252nd 253rd 254th
255th 256th 257th 258th 259th
260th 261st 262nd 263rd 264th
265th

1000th 1001st 1002nd 1003rd 1004th
1005th 1006th 1007th 1008th 1009th
1010th 1011st 1012nd 1013rd 1014th
1015th 1016th 1017th 1018th 1019th
1020th 1021st 1022nd 1023rd 1024th
1024th  ok
```



## Fortran


THE INSTRUCTIONS!
Write a function/method/subroutine/... that when given an integer greater than or equal to zero returns a string of the number followed by an apostrophe then the ordinal suffix.
Example returns would include 1'st 2'nd 3'rd 11'th 111'th 1001'st 1012'th



Please find the compilation instructions and examples in comments at the start of the source.

```Fortran
!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Fri Jun  6 15:40:18
!
!a=./f && make -k $a && echo 0 25 | $a && echo 250 265 | $a && echo 1000 1025 | $a
!gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none -g f.f08 -o f
!                  0'th                  1'st                  2'nd
!                  3'rd                  4'th                  5'th
!                  6'th                  7'th                  8'th
!                  9'th                 10'th                 11'th
!                 12'th                 13'th                 14'th
!                 15'th                 16'th                 17'th
!                 18'th                 19'th                 20'th
!                 21'st                 22'nd                 23'rd
!                 24'th                 25'th
!                                      250'th                251'st
!                252'nd                253'rd                254'th
!                255'th                256'th                257'th
!                258'th                259'th                260'th
!                261'st                262'nd                263'rd
!                264'th                265'th
!                                     1000th                1001st
!               1002nd                1003rd                1004th
!               1005th                1006th                1007th
!               1008th                1009th                1010th
!               1011th                1012th                1013th
!               1014th                1015th                1016th
!               1017th                1018th                1019th
!               1020th                1021st                1022nd
!               1023rd                1024th                1025th
!
!Compilation finished at Fri Jun  6 15:40:18

program nth
  implicit none
  logical :: need
  integer :: here, there, n, i, iostat
  read(5,*,iostat=iostat) here, there
  if (iostat .ne. 0) then
     write(6,*)'such bad input never before seen.'
     write(6,*)'I AYE EYE QUIT!'
     call exit(1)
  end if
  need = .false.
  n = abs(there - here) + 1
  i = 0
  do while (0 /= mod(3+mod(here-i, 3), 3))
     write(6,'(a22)',advance='no') ''
     i = i+1
  end do
  do i = here, there, sign(1, there-here)
     write(6,'(a22)',advance='no') ordinate(i)
     if (2 /= mod(i,3)) then
        need = .true.
     else
        write(6,'(a)')''
        need = .false.
     end if
  end do
  if (need) write(6,'(a)')''

contains

  character(len=22) function ordinate(n)
    character(len=19) :: a
    character(len=20), parameter :: &
         &a09 =   "thstndrdthththththth",&
         &ateen = "thththththththththth"
    integer :: ones, tens, ones_index
    integer, intent(in) :: n
    write(a,'(i19)') n
    ones = mod(n,10)
    tens = mod(n,100)
    ones_index = ones*2+1
    if (n < 1000) then
       if ((10 .le. tens) .and. (tens .lt. 20)) then
          ordinate = a // "'" // ateen(ones_index:ones_index+1)
          !            ^^^^^^  remove these characters to remove the important '
       else
          ordinate = a // "'" // a09(ones_index:ones_index+1)
          !            ^^^^^^  remove these characters to remove the important '
       end if
    else
       if ((10 .le. tens) .and. (tens .lt. 20)) then
          ordinate = a // ateen(ones_index:ones_index+1)
       else
          ordinate = a // a09(ones_index:ones_index+1)
       end if
    end if
  end function ordinate

end program nth
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Apostrophes NOT used as incorrect English

Function ordinal(n As UInteger) As String
  Dim ns As String = Str(n)
  Select Case Right(ns, 1)
    Case "0", "4" To "9"
      Return ns + "th"
    Case "1"
      If Right(ns, 2) = "11" Then Return ns + "th"
      Return ns + "st"
    Case "2"
      If Right(ns, 2) = "12" Then Return ns + "th"
      Return ns + "nd"
    Case "3"
      If Right(ns, 2) = "13" Then Return ns + "th"
      Return ns + "rd"
  End Select
End Function

Dim i As Integer
For i = 0 To 25
  Print ordinal(i); " ";
Next
Print : Print

For i = 250 To 265
  Print ordinal(i); " ";
Next
Print : Print

For i = 1000 To 1025
  Print ordinal(i); " ";
Next
Print : Print

Print "Press any key to quit"
Sleep
```

```txt
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th

250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th

1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th
1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```


## Gambas

'''[https://gambas-playground.proko.eu/?gist=6d60749ae886a37f128e75cffc6c7118 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siNums As Short[] = [0, 25, 250, 265, 1000, 1025]
Dim siCount, siNumbers As Short
Dim sOrdinal As String

For siNumbers = 0 To 4 Step 2
  For siCount = siNums[siNumbers] To siNums[siNumbers + 1]
    sOrdinal = "th"
    If Right(Str(siCount), 1) = "1" And Right(Str(siCount), 2) <> "11" Then sOrdinal = "st"
    If Right(Str(siCount), 1) = "2" And Right(Str(siCount), 2) <> "12" Then sOrdinal = "nd"
    If Right(Str(siCount), 1) = "3" And Right(Str(siCount), 2) <> "13" Then sOrdinal = "rd"
    Print siCount & sOrdinal;;
  Next
  Print gb.NewLine
Next

End
```

Output:

```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th

250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th

1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## Go

```go
package main

import "fmt"

func ord(n int) string {
    s := "th"
    switch c := n % 10; c {
    case 1, 2, 3:
        if n%100/10 == 1 {
            break
        }
        switch c {
        case 1:
            s = "st"
        case 2:
            s = "nd"
        case 3:
            s = "rd"
        }
    }
    return fmt.Sprintf("%d%s", n, s)
}

func main() {
    for n := 0; n <= 25; n++ {
        fmt.Printf("%s ", ord(n))
    }
    fmt.Println()
    for n := 250; n <= 265; n++ {
        fmt.Printf("%s ", ord(n))
    }
    fmt.Println()
    for n := 1000; n <= 1025; n++ {
        fmt.Printf("%s ", ord(n))
    }
    fmt.Println()
}
```

```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```


=={{header|GW-BASIC}}==
```qbasic

10  ' N'th
20  LET LOLIM% = 0
30  LET HILIM% = 25
40  GOSUB 1000
50  LET LOLIM% = 250
60  LET HILIM% = 265
70  GOSUB 1000
80  LET LOLIM% = 1000
90  LET HILIM% = 1025
100 GOSUB 1000
110 END

995  ' Print images
1000 FOR I% = LOLIM% TO HILIM%
1010  LET NR% = I%
1020  GOSUB 1500
1030  LET SI$ = STR$(I%)
1040  PRINT RIGHT$(SI$, LEN(SI$) - 1); SUF$; " ";
1050 NEXT I%
1060 PRINT
1070 RETURN

1495 ' Get suffix
1500 IF (NR% MOD 10 = 1) AND (NR% MOD 100 <> 11) THEN LET SUF$ = "st": GOTO 2000
1600 IF (NR% MOD 10 = 2) AND (NR% MOD 100 <> 12) THEN LET SUF$ = "nd": GOTO 2000
1700 IF (NR% MOD 10 = 3) AND (NR% MOD 100 <> 13) THEN LET SUF$ = "rd": GOTO 2000
1800 LET SUF$ = "th"
2000 RETURN

```

```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## Haskell


```haskell
import Data.Array

ordSuffs :: Array Integer String
ordSuffs = listArray (0,9) ["th", "st", "nd", "rd", "th",
                            "th", "th", "th", "th", "th"]

ordSuff :: Integer -> String
ordSuff n = show n ++ suff n
  where suff m | (m `rem` 100) >= 11 && (m `rem` 100) <= 13 = "th"
               | otherwise          = ordSuffs ! (m `rem` 10)

printOrdSuffs :: [Integer] -> IO ()
printOrdSuffs = putStrLn . unwords . map ordSuff

main :: IO ()
main = do
  printOrdSuffs [   0..  25]
  printOrdSuffs [ 250.. 265]
  printOrdSuffs [1000..1025]
```

```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.

```unicon
procedure main(A)
    every writes(" ",nth(0 to 25) | "\n")
    every writes(" ",nth(250 to 265) | "\n")
    every writes(" ",nth(1000 to 1025) | "\n")
end

procedure nth(n)
    return n || ((n%10 = 1, n%100 ~= 11, "st") |
                 (n%10 = 2, n%100 ~= 12, "nd") |
                 (n%10 = 3, n%100 ~= 13, "rd") | "th")
end
```


```txt

->nth
 0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13h 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
 250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013h 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
->

```



## J


Implementation:


```J
suf=: (;:'th st nd rd th') {::~ 4 <. 10 10 (* 1&~:)~/@#: ]
nth=: [: ;:inv (": , suf)each
```


Task:


```J
   thru=: <./ + i.@(+ *)@-~
   nth 0 thru 25
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
   nth 250 thru 265
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
   nth 1000 thru 1025
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```



## Java


```java
public class Nth {
	public static String ordinalAbbrev(int n){
		String ans = "th"; //most of the time it should be "th"
		if(n % 100 / 10 == 1) return ans; //teens are all "th"
		switch(n % 10){
			case 1: ans = "st"; break;
			case 2: ans = "nd"; break;
			case 3: ans = "rd"; break;
		}
		return ans;
	}

	public static void main(String[] args){
		for(int i = 0; i <= 25;i++){
			System.out.print(i + ordinalAbbrev(i) + " ");
		}
		System.out.println();
		for(int i = 250; i <= 265;i++){
			System.out.print(i + ordinalAbbrev(i) + " ");
		}
		System.out.println();
		for(int i = 1000; i <= 1025;i++){
			System.out.print(i + ordinalAbbrev(i) + " ");
		}
	}
}
```

```java
package nth;

import java.util.stream.IntStream;
import java.util.stream.Stream;

public interface Nth {
  public static String suffix(int n){
    if(n % 100 / 10 == 1){
      return "th"; //teens are all "th"
    }
    switch(n % 10){
      case 1: return "st";
      case 2: return "nd";
      case 3: return "rd";
      default: return "th"; //most of the time it should be "th"
    }
  }

  public static void print(int start, int end) {
    IntStream.rangeClosed(start, end)
      .parallel()
      .mapToObj(i -> i + suffix(i) + " ")
      .reduce(String::concat)
      .ifPresent(System.out::println)
    ;
  }

  public static void print(int[] startAndEnd) {
    print(startAndEnd[0], startAndEnd[1]);
  }

  public static int[] startAndEnd(int start, int end) {
    return new int[] {
      start,
      end
    };
  }

  public static void main(String... arguments){
    Stream.of(
      startAndEnd(0, 25),
      startAndEnd(250, 265),
      startAndEnd(1000, 1025)
    )
      .forEach(Nth::print)
    ;
  }
}
```

```txt
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```




## JavaScript



### ES5



```JavaScript
console.log(function () {

  var lstSuffix = 'th st nd rd th th th th th th'.split(' '),

    fnOrdinalForm = function (n) {
      return n.toString() + (
        11 <= n % 100 && 13 >= n % 100 ?
        "th" : lstSuffix[n % 10]
      );
    },

    range = function (m, n) {
      return Array.apply(
        null, Array(n - m + 1)
      ).map(function (x, i) {
        return m + i;
      });
    };

  return [[0, 25], [250, 265], [1000, 1025]].map(function (tpl) {
    return range.apply(null, tpl).map(fnOrdinalForm).join(' ');
  }).join('\n\n');

}());
```



```JavaScript
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th

250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th

1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```




### ES6



```JavaScript
(function (lstTestRanges) {
    'use strict'

    let lstSuffix = 'th st nd rd th th th th th th'.split(' '),

        // ordinalString :: Int -> String
        ordinalString = n =>
            n.toString() + (
                11 <= n % 100 && 13 >= n % 100 ?
                "th" : lstSuffix[n % 10]
            ),

        // range :: Int -> Int -> [Int]
        range = (m, n) =>
            Array.from({
                length: (n - m) + 1
            }, (_, i) => m + i);


    return lstTestRanges
        .map(tpl => range
            .apply(null, tpl)
            .map(ordinalString)
        );

})([[0, 25], [250, 265], [1000, 1025]]);
```



```txt
[["0th", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th",
"9th", "10th", "11th", "12th", "13th", "14th", "15th", "16th",
"17th", "18th", "19th", "20th", "21st", "22nd", "23rd", "24th", "25th"],
["250th", "251st", "252nd", "253rd", "254th", "255th", "256th", "257th",
"258th", "259th", "260th", "261st", "262nd", "263rd", "264th", "265th"],
["1000th", "1001st", "1002nd", "1003rd", "1004th", "1005th", "1006th",
"1007th", "1008th", "1009th", "1010th", "1011th", "1012th", "1013th",
"1014th", "1015th", "1016th", "1017th", "1018th", "1019th", "1020th",
"1021st", "1022nd", "1023rd", "1024th", "1025th"]]
```



## jq


```jq

# ordinalize an integer input, positive or negative
def ordinalize:
 (if . < 0 then -(.) else . end) as $num
 | ($num % 100) as $small
 | (if 11 <= $small and $small <= 13 then "th"
    else
    ( $num % 10)
      | (if   . == 1 then "st"
         elif . == 2 then "nd"
         elif . == 3 then "rd"
         else             "th"
         end)
    end) as $ordinal
 | "\(.)\($ordinal)" ;

([range(-5; -1)], [range(0;26)], [range(250;266)], [range(1000;1026)])
 | map(ordinalize)

```

```txt

["-5th","-4th","-3rd","-2nd"]
["0th","1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th","11th","12th","13th","14th","15th","16th","17th","18th","19th","20th","21st","22nd","23rd","24th","25th"]
["250th","251st","252nd","253rd","254th","255th","256th","257th","258th","259th","260th","261st","262nd","263rd","264th","265th"]
["1000th","1001st","1002nd","1003rd","1004th","1005th","1006th","1007th","1008th","1009th","1010th","1011th","1012th","1013th","1014th","1015th","1016th","1017th","1018th","1019th","1020th","1021st","1022nd","1023rd","1024th","1025th"]

```



## Julia

'''Function''':

```julia
function ordinal(n::Integer)
    n < 0 && throw(DomainError())
    suffixes = ("st", "nd", "rd")
    u = n % 10
    t = n ÷ 10 % 10
    if u > 3 || u == 0 || t == 1
        suf = "th"
    else
        suf = suffixes[u]
    end
    return string(n, suf)
end
```


'''Main''':

```julia
println("Tests of ordinal formatting of integers.")
for (i, n) in enumerate(0:25)
    (i - 1) % 10 == 0 && println()
    @printf("%7s", ordinal(n))
end

println()
for (i, n) in enumerate(250:265)
    (i - 1) % 10 == 0 && println()
    @printf("%7s", ordinal(n))
end

println()
for (i, n) in enumerate(1000:1025)
    (i - 1) % 10 == 0 && println()
    @printf("%7s", ordinal(n))
end
```


```txt
Tests of ordinal formatting of integers.

    0th    1st    2nd    3rd    4th    5th    6th    7th    8th    9th
   10th   11th   12th   13th   14th   15th   16th   17th   18th   19th
   20th   21st   22nd   23rd   24th   25th

  250th  251st  252nd  253rd  254th  255th  256th  257th  258th  259th
  260th  261st  262nd  263rd  264th  265th

 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th
 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th
 1020th 1021st 1022nd 1023rd 1024th 1025th
```


=={{Header|Kotlin}}==

```scala
fun Int.ordinalAbbrev() =
        if (this % 100 / 10 == 1) "th"
        else when (this % 10) { 1 -> "st" 2 -> "nd" 3 -> "rd" else -> "th" }

fun IntRange.ordinalAbbrev() = map { "$it" + it.ordinalAbbrev() }.joinToString(" ")

fun main(args: Array<String>) {
    listOf((0..25), (250..265), (1000..1025)).forEach { println(it.ordinalAbbrev()) }
}
```



## Liberty BASIC

```lb

call printImages    0,   25
call printImages  250,  265
call printImages 1000, 1025
end

sub printImages loLim, hiLim
  loLim = int(loLim)
  hiLIm = int(hiLim)
  for i = loLim to hiLim
    print str$(i) + suffix$(i) + " ";
  next i
  print
end sub

function suffix$(n)
  n = int(n)
  nMod10 = n mod 10
  nMod100 = n mod 100
  if (nMod10 = 1) and (nMod100 <> 11) then
    suffix$ = "st"
  else
    if (nMod10 = 2) and (nMod100 <> 12) then
      suffix$ = "nd"
    else
      if (NMod10 = 3) and (NMod100 <> 13) then
        suffix$ = "rd"
      else
        suffix$ = "th"
      end if
    end if
  end if
end function

```

```txt

0th 1st 2nd 3th 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23th 24th 25th
250th 251st 252nd 253th 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263th 264th 265th
1000th 1001st 1002nd 1003th 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023th 1024th 1025th

```



## Lua

The apostrophe just looks weird if you ask me.  No one did, obviously.

```Lua
function getSuffix (n)
    local lastTwo, lastOne = n % 100, n % 10
    if lastTwo > 3 and lastTwo < 21 then return "th" end
    if lastOne == 1 then return "st" end
    if lastOne == 2 then return "nd" end
    if lastOne == 3 then return "rd" end
    return "th"
end

function Nth (n) return n .. "'" .. getSuffix(n) end

for i = 0, 25 do print(Nth(i), Nth(i + 250), Nth(i + 1000)) end
```


```txt

0'th    250'th  1000'th
1'st    251'st  1001'st
2'nd    252'nd  1002'nd
3'rd    253'rd  1003'rd
4'th    254'th  1004'th
5'th    255'th  1005'th
6'th    256'th  1006'th
7'th    257'th  1007'th
8'th    258'th  1008'th
9'th    259'th  1009'th
10'th   260'th  1010'th
11'th   261'st  1011'th
12'th   262'nd  1012'th
13'th   263'rd  1013'th
14'th   264'th  1014'th
15'th   265'th  1015'th
16'th   266'th  1016'th
17'th   267'th  1017'th
18'th   268'th  1018'th
19'th   269'th  1019'th
20'th   270'th  1020'th
21'st   271'st  1021'st
22'nd   272'nd  1022'nd
23'rd   273'rd  1023'rd
24'th   274'th  1024'th
25'th   275'th  1025'th

```



## Maple


```maple
toOrdinal := proc(n:: nonnegint)
	if 1 <= n and n <= 10 then
		if n >= 4 then
			printf("%ath", n);
		elif n = 3 then
			printf("%ard", n);
		elif n = 2 then
			printf("%and", n);
		else
			printf("%ast", n);
		end if:
	else
		printf(convert(n, 'ordinal'));
	end if:
	return NULL;
end proc:

a := [[0, 25], [250, 265], [1000, 1025]]:
for i in a do
	for j from i[1] to i[2] do
		toOrdinal(j);
		printf("   ");
	end do;
	printf("\n\n");
end do;
```

```txt

0th   1st   2nd   3rd   4th   5th   6th   7th   8th   9th   10th   11th   12th   13th   14th   15th   16th   17th   18th   19th   20th   21st   22nd   23rd   24th   25th

250th   251st   252nd   253rd   254th   255th   256th   257th   258th   259th   260th   261st   262nd   263rd   264th   265th

1000th   1001st   1002nd   1003rd   1004th   1005th   1006th   1007th   1008th   1009th   1010th   1011th   1012th   1013th   1014th   1015th   1016th   1017th   1018th   1019th   1020th   1021st   1022nd   1023rd   1024th   1025th

```



## Mathematica

I borrowed the logic from the Python code.

```Mathematica
suffixlist = {"th", "st", "nd", "rd", "th", "th", "th", "th", "th","th"};
addsuffix[n_] := Module[{suffix},
  suffix = Which[
    Mod[n, 100] <= 10, suffixlist[[Mod[n, 10] + 1]],
    Mod[n, 100] > 20, suffixlist[[Mod[n, 10] + 1]],
    True, "th"
    ];
  ToString[n] <> suffix
  ]
addsuffix[#] & /@ Range[0, 25] (* test 1 *)
addsuffix[#] & /@ Range[250, 265] (* test 2 *)
addsuffix[#] & /@ Range[1000, 1025] (* test 3 *)

```

```txt
{"0th", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th", "11th", "12th", "13th", "14th", "15th", "16th", "17th", "18th", "19th", "20th", "21st", "22nd", "23rd", "24th", "25th"}

{"250th", "251st", "252nd", "253rd", "254th", "255th", "256th", "257th", "258th", "259th", "260th", "261st", "262nd", "263rd", "264th", "265th"}

{"1000th", "1001st", "1002nd", "1003rd", "1004th", "1005th", "1006th", "1007th", "1008th", "1009th", "1010th", "1011th", "1012th", "1013th", "1014th", "1015th", "1016th", "1017th", "1018th", "1019th", "1020th", "1021st", "1022nd", "1023rd", "1024th", "1025th"}
```


=={{Header|MATLAB}}==

```MATLAB
function s = nth(n)
    tens = mod(n, 100);
    if tens > 9 && tens < 20
        suf = 'th';
    else
        switch mod(n, 10)
            case 1
                suf = 'st';
            case 2
                suf = 'nd';
            case 3
                suf = 'rd';
            otherwise
                suf = 'th';
        end
    end
    s = sprintf('%d%s', n, suf);
end
```

```txt
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## Microsoft Small Basic

```microsoftsmallbasic

loLim = 0
hiLim = 25
PrintImages()
loLim = 250
hiLim = 265
PrintImages()
loLim = 1000
hiLim = 1025
PrintImages()

Sub PrintImages
  For i = loLim To hiLim
    nr = i
    GetSuffix()
    TextWindow.Write(i + suffix + " ")
  EndFor
  TextWindow.WriteLine("")
EndSub

Sub GetSuffix
  rem10  = Math.Remainder(nr, 10)
  rem100 = Math.Remainder(nr, 100)
  If rem10 = 1 And rem100 <> 11 Then
    suffix = "st"
  ElseIf rem10 = 2 And rem100 <> 12 Then
    suffix = "nd"
  ElseIf rem10 = 3 And rem100 <> 13 Then
    suffix = "rd"
  Else
    suffix = "th"
  EndIf
EndSub

```



## MiniScript

To get the output all on one line, we append it to a list as we go, and then print the list all at once at the end.


```MiniScript
ordinal = function(n)
    if n > 3 and n < 20 then return n + "th"
    if n % 10 == 1 then return n + "st"
    if n % 10 == 2 then return n + "nd"
    if n % 10 == 3 then return n + "rd"
    return n + "th"
end function

out = []
test = function(from, to)
    for i in range(from, to)
        out.push ordinal(i)
    end for
end function

test 0, 25
test 250, 265
test 1000, 1025
print out.join
```


```txt
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th 250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011st 1012nd 1013rd 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```


=={{header|Modula-2}}==
```modula2

MODULE Nth;

FROM STextIO IMPORT
  WriteString, WriteLn;
FROM WholeStr IMPORT
  IntToStr;

PROCEDURE Suffix(N: CARDINAL; VAR OUT Destination: ARRAY OF CHAR);
VAR
  NMod10, NMod100: CARDINAL;
BEGIN
  NMod10 := N MOD 10;
  NMod100 := N MOD 100;
  IF (NMod10 = 1) AND (NMod100 <> 11) THEN
    Destination := "st";
  ELSIF (NMod10 = 2) AND (NMod100 <> 12) THEN
    Destination := "nd";
  ELSIF (NMod10 = 3) AND (NMod100 <> 13) THEN
    Destination := "rd";
  ELSE
    Destination := "th";
  END;
END Suffix;

PROCEDURE PrintImages(LoLim, HiLim: CARDINAL);
VAR
  I: CARDINAL;
  IString: ARRAY [0 .. 15] OF CHAR;
  ISuff: ARRAY [0 .. 1] OF CHAR;
BEGIN
  FOR I := LoLim TO HiLim DO
    IntToStr(I, IString);
    Suffix(I, ISuff);
    WriteString(IString);
    WriteString(ISuff);
    WriteString(" ");
  END;
  WriteLn;
END PrintImages;

BEGIN
  PrintImages(   0,   25);
  PrintImages( 250,  265);
  PrintImages(1000, 1025);
END Nth.

```


=={{Header|Nim}}==
```nim
const suffix = ["th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th"]

proc nth(n): string =
  $n & "'" & (if n mod 100 <= 10 or n mod 100 > 20: suffix[n mod 10] else: "th")

for j in countup(0, 1000, 250):
  for i in j..j+24:
    stdout.write nth(i), " "
  echo ""
```

```txt
0'th 1'st 2'nd 3'rd 4'th 5'th 6'th 7'th 8'th 9'th 10'th 11'th 12'th 13'th 14'th 15'th 16'th 17'th 18'th 19'th 20'th 21'st 22'nd 23'rd 24'th
250'th 251'st 252'nd 253'rd 254'th 255'th 256'th 257'th 258'th 259'th 260'th 261'st 262'nd 263'rd 264'th 265'th 266'th 267'th 268'th 269'th 270'th 271'st 272'nd 273'rd 274'th
500'th 501'st 502'nd 503'rd 504'th 505'th 506'th 507'th 508'th 509'th 510'th 511'th 512'th 513'th 514'th 515'th 516'th 517'th 518'th 519'th 520'th 521'st 522'nd 523'rd 524'th
750'th 751'st 752'nd 753'rd 754'th 755'th 756'th 757'th 758'th 759'th 760'th 761'st 762'nd 763'rd 764'th 765'th 766'th 767'th 768'th 769'th 770'th 771'st 772'nd 773'rd 774'th
1000'th 1001'st 1002'nd 1003'rd 1004'th 1005'th 1006'th 1007'th 1008'th 1009'th 1010'th 1011'th 1012'th 1013'th 1014'th 1015'th 1016'th 1017'th 1018'th 1019'th 1020'th 1021'st 1022'nd 1023'rd 1024'th
```



## OCaml



```Ocaml

let show_nth n =
  if (n mod 10 = 1) && (n mod 100 <> 11) then "st"
  else if (n mod 10 = 2) && (n mod 100 <> 12) then "nd"
  else if (n mod 10 = 3) && (n mod 100 <> 13) then "rd"
  else "th"


let () =
  let show_ordinals (min, max) =
    for i=min to max do
      Printf.printf "%d%s " i (show_nth i)
    done;
    print_newline() in

  List.iter show_ordinals [ (0,25); (250,265); (1000,1025) ]

```


```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## Oforth



```Oforth
: nth(n)
| r |
   n "th" over 10 mod ->r
   r 1 == ifTrue: [ n 100 mod 11 == ifFalse: [ drop "st" ] ]
   r 2 == ifTrue: [ n 100 mod 12 == ifFalse: [ drop "nd" ] ]
   r 3 == ifTrue: [ n 100 mod 13 == ifFalse: [ drop "rd" ] ]
   + ;
```


```txt

seqFrom(0, 25) map(#nth) println
[0th, 1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th, 10th, 11th, 12th, 13th, 14th, 15th, 16t
h, 17th, 18th, 19th, 20th, 21st, 22nd, 23rd, 24th, 25th]

seqFrom(250, 265) map(#nth) println
[250th, 251st, 252nd, 253rd, 254th, 255th, 256th, 257th, 258th, 259th, 260th, 261st, 262nd
, 263rd, 264th, 265th]

seqFrom(1000, 1025) map(#nth) println
[1000th, 1001st, 1002nd, 1003rd, 1004th, 1005th, 1006th, 1007th, 1008th, 1009th, 1010th, 1
011th, 1012th, 1013th, 1014th, 1015th, 1016th, 1017th, 1018th, 1019th, 1020th, 1021st, 102
2nd, 1023rd, 1024th, 1025th]

```


=={{Header|PARI/GP}}==
(Spurious apostrophes intentionally omitted, following Perl 6.)


```parigp
ordinal(n)=my(k=n%10,m=n%100); Str(n,if(m<21&&m>3,"th",k==1,"st",k==2,"nd",k==3,"rd","th"));
apply(ordinal, [0..25])
apply(ordinal, [250..265])
apply(ordinal, [1000..1025])
apply(ordinal, [111, 1012])
```

```txt
%1 = ["0th", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th", "11th", "12th", "13th", "14th", "15th", "16th", "17th", "18th", "19th", "20th", "21st", "22nd", "23rd", "24th", "25th"]
%2 = ["250th", "251st", "252nd", "253rd", "254th", "255th", "256th", "257th", "258th", "259th", "260th", "261st", "262nd", "263rd", "264th", "265th"]
%3 = ["1000th", "1001st", "1002nd", "1003rd", "1004th", "1005th", "1006th", "1007th", "1008th", "1009th", "1010th", "1011th", "1012th", "1013th", "1014th", "1015th", "1016th", "1017th", "1018th", "1019th", "1020th", "1021st", "1022nd", "1023rd", "1024th", "1025th"]
%4 = ["111th", "1012th"]
```

=={{Header|Pascal}}==
nearly copy of [[N'th#Ada|Ada]]

```pascal
Program n_th;

function Suffix(N: NativeInt):AnsiString;
var
  res: AnsiString;
begin
  res:= 'th';
  case N mod 10 of
  1:IF N mod 100 <> 11 then
      res:= 'st';
  2:IF N mod 100 <> 12 then
      res:= 'nd';
  3:IF N mod 100 <> 13 then
      res:= 'rd';
  else
  end;
  Suffix := res;
end;

procedure Print_Images(loLim, HiLim: NativeInt);
var
  i : NativeUint;
begin
  for I := LoLim to HiLim do
    write(i,Suffix(i),' ');
  writeln;
end;

begin
   Print_Images(   0,   25);
   Print_Images( 250,  265);
   Print_Images(1000, 1025);
end.
```

{{Out}} shortened

```txt

0th 1st 2nd 3rd 4th ... 11th 12th 13th ..20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th ..261st 262nd 263rd 264th 265th
..1001st 1002nd 1003rd 1004th..1011th..1013th..1021st 1022nd 1023rd 1024th

```

=={{Header|Perl}}==
Requires Perl 5.10 or newer for the Defined OR operator (//).

```perl
my %irregulars = ( 1 => 'st',
                   2 => 'nd',
                   3 => 'rd',
                  11 => 'th',
                  12 => 'th',
                  13 => 'th');
sub nth
{
    my $n = shift;
    $n . # q(') . # Uncomment this to add apostrophes to output
    ($irregulars{$n % 100} // $irregulars{$n % 10} // 'th');
}

sub range { join ' ', map { nth($_) } @{$_[0]} }
print range($_), "\n" for ([0..25], [250..265], [1000..1025]);

```

Same as Perl 6

```Perl
use Lingua::EN::Numbers::Ordinate 'ordinate';
foreach my $i (0..25, 250..265, 1000..1025) {
  print ordinate($i),"\n";
}
```



## Perl 6

(Spurious apostrophes intentionally omitted.)

```perl6
my %irregulars = <1 st 2 nd 3 rd>, (11..13 X=> 'th');

sub nth ($n) { $n ~ ( %irregulars{$n % 100} // %irregulars{$n % 10} // 'th' ) }

say .list».&nth for [^26], [250..265], [1000..1025];
```

```txt
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```

If you want to get Unicodally fancy, use this version instead:

```perl6
my %irregulars = <1 ˢᵗ 2 ⁿᵈ 3 ʳᵈ>, (11..13 X=> 'ᵗʰ');

sub nth ($n) { $n ~ ( %irregulars{$n % 100} // %irregulars{$n % 10} // 'ᵗʰ' ) }

say .list».&nth for [^26], [250..265], [1000..1025];
```

<blockquote>0ᵗʰ 1ˢᵗ 2ⁿᵈ 3ʳᵈ 4ᵗʰ 5ᵗʰ 6ᵗʰ 7ᵗʰ 8ᵗʰ 9ᵗʰ 10ᵗʰ 11ᵗʰ 12ᵗʰ 13ᵗʰ 14ᵗʰ 15ᵗʰ 16ᵗʰ 17ᵗʰ 18ᵗʰ 19ᵗʰ 20ᵗʰ 21ˢᵗ 22ⁿᵈ 23ʳᵈ 24ᵗʰ 25ᵗʰ

250ᵗʰ 251ˢᵗ 252ⁿᵈ 253ʳᵈ 254ᵗʰ 255ᵗʰ 256ᵗʰ 257ᵗʰ 258ᵗʰ 259ᵗʰ 260ᵗʰ 261ˢᵗ 262ⁿᵈ 263ʳᵈ 264ᵗʰ 265ᵗʰ

1000ᵗʰ 1001ˢᵗ 1002ⁿᵈ 1003ʳᵈ 1004ᵗʰ 1005ᵗʰ 1006ᵗʰ 1007ᵗʰ 1008ᵗʰ 1009ᵗʰ 1010ᵗʰ 1011ᵗʰ 1012ᵗʰ 1013ᵗʰ 1014ᵗʰ 1015ᵗʰ 1016ᵗʰ 1017ᵗʰ 1018ᵗʰ 1019ᵗʰ 1020ᵗʰ 1021ˢᵗ 1022ⁿᵈ 1023ʳᵈ 1024ᵗʰ 1025ᵗʰ</blockquote>


## Phix


```Phix
constant ordinals = {"th","st","nd","rd"}

function Nth(integer n, bool apostrophe=false)
    integer mod10 = mod(n,10)+1
    if mod10>4 or mod(n,100)=mod10+9 then mod10 = 1 end if
    return sprintf("%d%s",{n,repeat('\'',apostrophe)&ordinals[mod10]})
end function

constant ranges = {{0,25},{250,265},{1000,1025}}
for i=1 to length(ranges) do
    for j=ranges[i][1] to ranges[i][2] do
        if mod(j,10)=0 then puts(1,"\n") end if
        printf(1," %6s",{Nth(j,i=2)})
    end for
    puts(1,"\n")
end for
```

```txt

    0th    1st    2nd    3rd    4th    5th    6th    7th    8th    9th
   10th   11th   12th   13th   14th   15th   16th   17th   18th   19th
   20th   21st   22nd   23rd   24th   25th

 250'th 251'st 252'nd 253'rd 254'th 255'th 256'th 257'th 258'th 259'th
 260'th 261'st 262'nd 263'rd 264'th 265'th

 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th
 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th
 1020th 1021st 1022nd 1023rd 1024th 1025th

```


=={{Header|PHP}}==

```PHP
function nth($num) {
  $os = "th";
  if ($num % 100 <= 10 or $num % 100 > 20) {
    switch ($num % 10) {
      case 1:
        $os = "st";
        break;
      case 2:
        $os = "nd";
        break;
      case 3:
        $os = "rd";
        break;
    }
  }
  return $num . $os;
}

foreach ([[0,25], [250,265], [1000,1025]] as $i) {
  while ($i[0] <= $i[1]) {
    echo nth($i[0]) . " ";
    $i[0]++;
  }
  echo "\n";
}
```

```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```


=={{Header|PicoLisp}}==

```PicoLisp
(de rangeth (A B)
   (mapcar
      '((I)
         (pack I
            (if (member (% I 100) (11 12 13))
               'th
               (case (% I 10)
                  (1 'st)
                  (2 'nd)
                  (3 'rd)
                  (T 'th) ) ) ) )
         (range A B) ) )

(prinl (glue " " (rangeth 0 25)))
(prinl (glue " " (rangeth 250 265)))
(prinl (glue " " (rangeth 1000 1025)))

(bye)
```

```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## PL/I

<lang>Nth: procedure options (main);   /* 1 June 2014 */
   declare i fixed (10);

   do i = 0 to 25, 250 to 265, 1000 to 1025;
      if i = 250 | i = 1000 then put skip (2);
      put edit (enth(i)) (x(1), a);
   end;

enth: procedure (i) returns (character (25) varying);
   declare i fixed (10);
   declare suffix character (2);

   select (mod(i, 10));
      when (1)  suffix = 'st';
      when (2)  suffix = 'nd';
      when (3)  suffix = 'rd';
      otherwise suffix = 'th';
   end;
   select (mod(i, 100));
      when (11, 12, 13) suffix = 'th';
      otherwise ;
   end;
   return ( trim(i) || suffix );
end enth;

end Nth;
```

```txt
 0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th
 17th 18th 19th 20th 21st 22nd 23rd 24th 25th

 250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd
 263rd 264th 265th

 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th
 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st
 1022nd 1023rd 1024th 1025th
```



## PowerShell


```powershell
function nth($inp){
	$suffix = "th"

	switch($inp % 10){
		1{$suffix="st"}
		2{$suffix="nd"}
		3{$suffix="rd"}
	}
	return "$inp$suffix "
}

0..25 | %{Write-host -nonewline (nth "$_")};""
250..265 | %{Write-host -nonewline (nth "$_")};""
1000..1025 | %{Write-host -nonewline (nth "$_")};""
```

```txt
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11st 12nd 13rd 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011st 1012nd 1013rd 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```



### An Alternate Version

This is, I think, is a more "PowerShelly" way:

```PowerShell

function Get-Nth ([int]$Number)
{
    $suffix = "th"

    switch ($Number % 10)
    {
        1 {$suffix = "st"}
        2 {$suffix = "nd"}
        3 {$suffix = "rd"}
    }

    "$Number$suffix"
}

1..25      | ForEach-Object {Get-Nth $_} | Format-Wide {$_} -Column 5 -Force
251..265   | ForEach-Object {Get-Nth $_} | Format-Wide {$_} -Column 5 -Force
1001..1025 | ForEach-Object {Get-Nth $_} | Format-Wide {$_} -Column 5 -Force

```

```txt

1st                        2nd                        3rd                        4th                        5th
6th                        7th                        8th                        9th                        10th
11st                       12nd                       13rd                       14th                       15th
16th                       17th                       18th                       19th                       20th
21st                       22nd                       23rd                       24th                       25th


251st                      252nd                      253rd                      254th                      255th
256th                      257th                      258th                      259th                      260th
261st                      262nd                      263rd                      264th                      265th


1001st                     1002nd                     1003rd                     1004th                     1005th
1006th                     1007th                     1008th                     1009th                     1010th
1011st                     1012nd                     1013rd                     1014th                     1015th
1016th                     1017th                     1018th                     1019th                     1020th
1021st                     1022nd                     1023rd                     1024th                     1025th

```



## Prolog

Following Icon:


```prolog
nth(N, N_Th) :-
    ( tween(N)      -> Th = "th"
    ; 1 is N mod 10 -> Th = "st"
    ; 2 is N mod 10 -> Th = "nd"
    ; 3 is N mod 10 -> Th = "rd"
    ; Th = "th" ),
    string_concat(N, Th, N_Th).

tween(N) :- Tween is N mod 100, between(11, 13, Tween).

test :-
    forall( between(0,25, N),     (nth(N, N_Th), format('~w, ', N_Th)) ),
    nl, nl,
    forall( between(250,265,N),   (nth(N, N_Th), format('~w, ', N_Th)) ),
    nl, nl,
    forall( between(1000,1025,N), (nth(N, N_Th), format('~w, ', N_Th)) ).

```


{{out}} of `test/0`:

```txt

?- test.
0th, 1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th, 10th, 11th, 12th, 13th, 14th, 15th, 16th, 17th, 18th, 19th, 20th, 21st, 22nd, 23rd, 24th, 25th,

250th, 251st, 252nd, 253rd, 254th, 255th, 256th, 257th, 258th, 259th, 260th, 261st, 262nd, 263rd, 264th, 265th,

1000th, 1001st, 1002nd, 1003rd, 1004th, 1005th, 1006th, 1007th, 1008th, 1009th, 1010th, 1011th, 1012th, 1013th, 1014th, 1015th, 1016th, 1017th, 1018th, 1019th, 1020th, 1021st, 1022nd, 1023rd, 1024th, 1025th,
true.

```


=={{Header|Python}}==

```python
_suffix = ['th', 'st', 'nd', 'rd', 'th', 'th', 'th', 'th', 'th', 'th']

def nth(n):
    return "%i'%s" % (n, _suffix[n%10] if n % 100 <= 10 or n % 100 > 20 else 'th')

if __name__ == '__main__':
    for j in range(0,1001, 250):
        print(' '.join(nth(i) for i in list(range(j, j+25))))
```

```txt
0'th 1'st 2'nd 3'rd 4'th 5'th 6'th 7'th 8'th 9'th 10'th 11'th 12'th 13'th 14'th 15'th 16'th 17'th 18'th 19'th 20'th 21'st 22'nd 23'rd 24'th
250'th 251'st 252'nd 253'rd 254'th 255'th 256'th 257'th 258'th 259'th 260'th 261'st 262'nd 263'rd 264'th 265'th 266'th 267'th 268'th 269'th 270'th 271'st 272'nd 273'rd 274'th
500'th 501'st 502'nd 503'rd 504'th 505'th 506'th 507'th 508'th 509'th 510'th 511'th 512'th 513'th 514'th 515'th 516'th 517'th 518'th 519'th 520'th 521'st 522'nd 523'rd 524'th
750'th 751'st 752'nd 753'rd 754'th 755'th 756'th 757'th 758'th 759'th 760'th 761'st 762'nd 763'rd 764'th 765'th 766'th 767'th 768'th 769'th 770'th 771'st 772'nd 773'rd 774'th
1000'th 1001'st 1002'nd 1003'rd 1004'th 1005'th 1006'th 1007'th 1008'th 1009'th 1010'th 1011'th 1012'th 1013'th 1014'th 1015'th 1016'th 1017'th 1018'th 1019'th 1020'th 1021'st 1022'nd 1023'rd 1024'th
```

'''Alternate version'''

```python
#!/usr/bin/env python3

def ord(n):
    try:
        s = ['st', 'nd', 'rd'][(n-1)%10]
        if (n-10)%100//10:
            return str(n)+s
    except IndexError:
        pass
    return str(n)+'th'

if __name__ == '__main__':
    print(*(ord(n) for n in range(26)))
    print(*(ord(n) for n in range(250,266)))
    print(*(ord(n) for n in range(1000,1026)))
```

```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th
18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd
263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th
1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st
1022nd 1023rd 1024th 1025th

```


=={{Header|R}}==
Note that R vectors are 1-indexed.

```rsplus
nth <- function(n)
{
  if (length(n) > 1) return(sapply(n, nth))

  mod <- function(m, n) ifelse(!(m%%n), n, m%%n)
  suffices <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")

  if (n %% 100 <= 10 || n %% 100 > 20)
    suffix <- suffices[mod(n+1, 10)]
  else
    suffix <- 'th'

  paste(n, "'", suffix, sep="")
}

range <- list(0:25, 250:275, 500:525, 750:775, 1000:1025)

sapply(range, nth)
```

```txt
      [,1]    [,2]     [,3]     [,4]     [,5]
 [1,] "0'th"  "250'th" "500'th" "750'th" "1000'th"
 [2,] "1'st"  "251'st" "501'st" "751'st" "1001'st"
 [3,] "2'nd"  "252'nd" "502'nd" "752'nd" "1002'nd"
 [4,] "3'rd"  "253'rd" "503'rd" "753'rd" "1003'rd"
 [5,] "4'th"  "254'th" "504'th" "754'th" "1004'th"
 [6,] "5'th"  "255'th" "505'th" "755'th" "1005'th"
 [7,] "6'th"  "256'th" "506'th" "756'th" "1006'th"
 [8,] "7'th"  "257'th" "507'th" "757'th" "1007'th"
 [9,] "8'th"  "258'th" "508'th" "758'th" "1008'th"
[10,] "9'th"  "259'th" "509'th" "759'th" "1009'th"
[11,] "10'th" "260'th" "510'th" "760'th" "1010'th"
[12,] "11'th" "261'st" "511'th" "761'st" "1011'th"
[13,] "12'th" "262'nd" "512'th" "762'nd" "1012'th"
[14,] "13'th" "263'rd" "513'th" "763'rd" "1013'th"
[15,] "14'th" "264'th" "514'th" "764'th" "1014'th"
[16,] "15'th" "265'th" "515'th" "765'th" "1015'th"
[17,] "16'th" "266'th" "516'th" "766'th" "1016'th"
[18,] "17'th" "267'th" "517'th" "767'th" "1017'th"
[19,] "18'th" "268'th" "518'th" "768'th" "1018'th"
[20,] "19'th" "269'th" "519'th" "769'th" "1019'th"
[21,] "20'th" "270'th" "520'th" "770'th" "1020'th"
[22,] "21'st" "271'st" "521'st" "771'st" "1021'st"
[23,] "22'nd" "272'nd" "522'nd" "772'nd" "1022'nd"
[24,] "23'rd" "273'rd" "523'rd" "773'rd" "1023'rd"
[25,] "24'th" "274'th" "524'th" "774'th" "1024'th"
[26,] "25'th" "275'th" "525'th" "775'th" "1025'th"

```



## Racket



```racket
#lang racket
(define (teen? n) (<= 11 (modulo n 100) 19))
(define (Nth n)
  (format
   "~a'~a" n
   (match* ((modulo n 10) n)
     [((or 1 2 3) (? teen?)) 'th] [(1 _) 'st] [(2 _) 'nd] [(3 _) 'rd] [(_ _) 'th])))

(for ((range (list  (in-range 26) (in-range 250 266) (in-range 1000 1026))))
  (displayln (string-join (for/list ((nth (sequence-map Nth range))) nth) " ")))
```

```txt
0'th 1'st 2'nd 3'rd 4'th 5'th 6'th 7'th 8'th 9'th 10'th 11'th 12'th 13'th 14'th 15'th 16'th 17'th 18'th 19'th 20'th 21'st 22'nd 23'rd 24'th 25'th
250'th 251'st 252'nd 253'rd 254'th 255'th 256'th 257'th 258'th 259'th 260'th 261'st 262'nd 263'rd 264'th 265'th
1000'th 1001'st 1002'nd 1003'rd 1004'th 1005'th 1006'th 1007'th 1008'th 1009'th 1010'th 1011'th 1012'th 1013'th 1014'th 1015'th 1016'th 1017'th 1018'th 1019'th 1020'th 1021'st 1022'nd 1023'rd 1024'th 1025'th
```



## REXX

This version adds suffixes without apostrophes.

Negative numbers and fractions are also handled.

```rexx
/*REXX program shows ranges of numbers  with  ordinal  (st/nd/rd/th)  suffixes attached.*/
call tell     0,    25                           /*display the  1st  range of numbers.  */
call tell   250,   265                           /*   "     "   2nd    "    "    "      */
call tell  1000,  1025                           /*   "     "   3rd    "    "    "      */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell: procedure; parse arg L,H,,$                /*get the Low and High #s, nullify list*/
           do j=L  to  H;   $=$ th(j);   end     /*process the range, from low ───► high*/
      say 'numbers from  '    L    " to "    H    ' (inclusive):'  /*display the title. */
      say strip($);    say;     say                                /*display line; 2 sep*/
      return                                                       /*return to invoker. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
th: parse arg z; x=abs(z); return z||word('th st nd rd',1+x//10*(x//100%10\==1)*(x//10<4))
```

'''output'''   using the default inputs:

```txt

numbers from   0  to  25  (inclusive):
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th


numbers from   250  to  265  (inclusive):
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th


numbers from   1000  to  1025  (inclusive):
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## Ring


```ring

for nr = 0 to 25
    see Nth(nr) + Nth(nr + 250) + Nth(nr + 1000) + nl
next

func getSuffix n
     lastTwo = n % 100
     lastOne = n % 10
     if lastTwo > 3 and lastTwo < 21  "th" ok
     if lastOne = 1 return "st" ok
     if lastOne = 2 return "nd" ok
     if lastOne = 3 return "rd" ok
     return "th"

func Nth n
     return  "" + n + "'" +  getSuffix(n) + " "

```



## Ruby

Code (slightly adapted) and methodname taken from ActiveSupport (Ruby on Rails).

```ruby
class Integer
  def ordinalize
    num = self.abs
    ordinal = if (11..13).include?(num % 100)
      "th"
    else
      case num % 10
        when 1; "st"
        when 2; "nd"
        when 3; "rd"
        else    "th"
      end
    end
    "#{self}#{ordinal}"
  end
end

[(0..25),(250..265),(1000..1025)].each{|r| puts r.map(&:ordinalize).join(", "); puts}

```

```txt

0th, 1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th, 10th, 11th, 12th, 13th, 14th, 15th, 16th, 17th, 18th, 19th, 20th, 21st, 22nd, 23rd, 24th, 25th

250th, 251st, 252nd, 253rd, 254th, 255th, 256th, 257th, 258th, 259th, 260th, 261st, 262nd, 263rd, 264th, 265th

1000th, 1001st, 1002nd, 1003rd, 1004th, 1005th, 1006th, 1007th, 1008th, 1009th, 1010th, 1011th, 1012th, 1013th, 1014th, 1015th, 1016th, 1017th, 1018th, 1019th, 1020th, 1021st, 1022nd, 1023rd, 1024th, 1025th

```




## Rust


```rust
fn nth(num: isize) -> String {
    format!("{}{}", num, match (num % 10, num % 100) {
        (1, 11) | (2, 12) | (3, 13) => "th",
        (1, _) => "st",
        (2, _) => "nd",
        (3, _) => "rd",
        _ => "th",
    })
}

fn main() {
    let ranges = [(0, 26), (250, 266), (1000, 1026)];
    for &(s, e) in &ranges {
        println!("[{}, {}) :", s, e);
        for i in s..e {
            print!("{}, ", nth(i));
        }
        println!();
    }
}
```


```txt
[0, 26) :
0th, 1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th, 10th, 11th, 12th, 13th, 14th, 15th, 16th, 17th, 18th, 19th, 20th, 21st, 22nd, 23rd, 24th, 25th,
[250, 266) :
250th, 251st, 252nd, 253rd, 254th, 255th, 256th, 257th, 258th, 259th, 260th, 261st, 262nd, 263rd, 264th, 265th,
[1000, 1026) :
1000th, 1001st, 1002nd, 1003rd, 1004th, 1005th, 1006th, 1007th, 1008th, 1009th, 1010th, 1011th, 1012th, 1013th, 1014th, 1015th, 1016th, 1017th, 1018th, 1019th, 1020th, 1021st, 1022nd, 1023rd, 1024th, 1025th,
```



## Scala

```Scala
object Nth extends App {
  def abbrevNumber(i: Int) = print(s"$i${ordinalAbbrev(i)} ")

  def ordinalAbbrev(n: Int) = {
    val ans = "th" //most of the time it should be "th"
    if (n % 100 / 10 == 1) ans //teens are all "th"
    else (n % 10) match {
      case 1 => "st"
      case 2 => "nd"
      case 3 => "rd"
      case _ => ans
    }
  }

  (0 to 25).foreach(abbrevNumber)
  println()
  (250 to 265).foreach(abbrevNumber)
  println();
  (1000 to 1025).foreach(abbrevNumber)
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: suffix (in integer: num) is func
  result
    var string: suffix is "";
  begin
    if    num rem 10 = 1 and num rem 100 <> 11 then suffix := "st";
    elsif num rem 10 = 2 and num rem 100 <> 12 then suffix := "nd";
    elsif num rem 10 = 3 and num rem 100 <> 13 then suffix := "rd";
    else suffix := "th";
    end if;
   end func;

const proc: printImages (in integer: start, in integer: stop) is func
  local
    var integer: num is 0;
  begin
    for num range start to stop do
      write(num <& suffix(num) <& " ");
    end for;
    writeln;
  end func;

const proc: main is func
  begin
    printImages(   0,   25);
    printImages( 250,  265);
    printImages(1000, 1025);
  end func;
```


```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## Set lang

Due to the language's specification, the input can only contain one character. Therefore, the following code only works with 0-9.
<lang set_lang>set o 49
set t 50
set h 51
set n !
set ! n
set ! 39
[n=o] set ? 13
[n=t] set ? 16
[n=h] set ? 19
set ! T
set ! H
set ? 21
set ! S
set ! T
set ? 21
set ! N
set ! D
set ? 12
set ! R
set ! D
> EOF
```

Input: I, Output: O

```txt
I: 1, O: 1'ST
I: 2, O: 2'ND
I: 3, O: 3'RD
I: 4, O: 4'TH
I: 5, O: 5'TH
    ...
```



## Sidef

```ruby
func nth(n) {
    static irregulars = Hash(<1 ˢᵗ 2 ⁿᵈ 3 ʳᵈ 11 ᵗʰ 12 ᵗʰ 13 ᵗʰ>...)
    n.to_s + (irregulars{n % 100} \\ irregulars{n % 10} \\ 'ᵗʰ')
}

for r in [0..25, 250..265, 1000..1025] {
    say r.map {|n| nth(n) }.join(" ")
}
```

```txt

0ᵗʰ 1ˢᵗ 2ⁿᵈ 3ʳᵈ 4ᵗʰ 5ᵗʰ 6ᵗʰ 7ᵗʰ 8ᵗʰ 9ᵗʰ 10ᵗʰ 11ᵗʰ 12ᵗʰ 13ᵗʰ 14ᵗʰ 15ᵗʰ 16ᵗʰ 17ᵗʰ 18ᵗʰ 19ᵗʰ 20ᵗʰ 21ˢᵗ 22ⁿᵈ 23ʳᵈ 24ᵗʰ 25ᵗʰ
250ᵗʰ 251ˢᵗ 252ⁿᵈ 253ʳᵈ 254ᵗʰ 255ᵗʰ 256ᵗʰ 257ᵗʰ 258ᵗʰ 259ᵗʰ 260ᵗʰ 261ˢᵗ 262ⁿᵈ 263ʳᵈ 264ᵗʰ 265ᵗʰ
1000ᵗʰ 1001ˢᵗ 1002ⁿᵈ 1003ʳᵈ 1004ᵗʰ 1005ᵗʰ 1006ᵗʰ 1007ᵗʰ 1008ᵗʰ 1009ᵗʰ 1010ᵗʰ 1011ᵗʰ 1012ᵗʰ 1013ᵗʰ 1014ᵗʰ 1015ᵗʰ 1016ᵗʰ 1017ᵗʰ 1018ᵗʰ 1019ᵗʰ 1020ᵗʰ 1021ˢᵗ 1022ⁿᵈ 1023ʳᵈ 1024ᵗʰ 1025ᵗʰ

```



## Sinclair ZX81 BASIC

Works flawlessly with 2k or more of RAM. With 1k, the subroutine itself works but you can't quite print all the tests: the program crashes with an 'out of memory' error code after 1017th. (A slightly less useful and readable version gets as far as 1023rd; 1025th is probably attainable, but might involve obfuscating the program more than is appropriate for this site.)

```basic
 10 FOR N=0 TO 25
 20 GOSUB 160
 30 PRINT N$;" ";
 40 NEXT N
 50 PRINT
 60 FOR N=250 TO 265
 70 GOSUB 160
 80 PRINT N$;" ";
 90 NEXT N
100 PRINT
110 FOR N=1000 TO 1025
120 GOSUB 160
130 PRINT N$;" ";
140 NEXT N
150 STOP
160 LET N$=STR$ N
170 LET S$="TH"
180 IF LEN N$=1 THEN GOTO 200
190 IF N$(LEN N$-1)="1" THEN GOTO 230
200 IF N$(LEN N$)="1" THEN LET S$="ST"
210 IF N$(LEN N$)="2" THEN LET S$="ND"
220 IF N$(LEN N$)="3" THEN LET S$="RD"
230 LET N$=N$+S$
240 RETURN
```

```txt

0TH 1ST 2ND 3RD 4TH 5TH 6TH 7TH 8TH 9TH 10TH 11TH 12TH 13TH 14TH 15TH 16TH 17TH 18TH 19TH 20TH 21ST 22ND 23RD 24TH 25TH
250TH 251ST 252ND 253RD 254TH 255TH 256TH 257TH 258TH 259TH 260TH 261ST 262ND 263RD 264TH 265TH
1000TH 1001ST 1002ND 1003RD 1004TH 1005TH 1006TH 1007TH 1008TH 1009TH 1010TH 1011TH 1012TH 1013TH 1014TH 1015TH 1016TH 1017TH 1018TH 1019TH 1020TH 1021ST 1022ND 1023RD 1024TH 1025TH
```



## SQL

Oracle

```SQL

select level card,
        to_char(to_date(level,'j'),'fmjth') ord
from dual
connect by level <= 15;

select to_char(to_date(5373485,'j'),'fmjth')
from dual;

```


```txt

      CARD ORD
---------- ------------------------------
         1 1st
         2 2nd
         3 3rd
         4 4th
         5 5th
         6 6th
         7 7th
         8 8th
         9 9th
        10 10th
        11 11th
        12 12th
        13 13th
        14 14th
        15 15th

15 rows selected.

select to_char(to_date(5373485,'j'),'fmjth')
                       *
ERROR at line 1:
ORA-01854: julian date must be between 1 and 5373484

```



## Stata

We reuse here the '''maps''' function defined in the task [[Apply a callback to an array]].


```stata
mata
function maps(f,a) {
	nr = rows(a)
	nc = cols(a)
	b = J(nr,nc,"")
	for (i=1;i<=nr;i++) {
		for (j=1;j<=nc;j++) b[i,j] = (*f)(a[i,j])
	}
	return(b)
}

function nth(n) {
	k = max((min((mod(n-1,10)+1,4)),4*(mod(n-10,100)<10)))
	return(strofreal(n)+("st","nd","rd","th")[k])
}

maps(&nth(),((0::25),(250::275),(1000::1025)))
end
```


'''Output:'''


```txt
             1        2        3
     +----------------------------+
   1 |     0th    250th   1000th  |
   2 |     1st    251st   1001st  |
   3 |     2nd    252nd   1002nd  |
   4 |     3rd    253rd   1003rd  |
   5 |     4th    254th   1004th  |
   6 |     5th    255th   1005th  |
   7 |     6th    256th   1006th  |
   8 |     7th    257th   1007th  |
   9 |     8th    258th   1008th  |
  10 |     9th    259th   1009th  |
  11 |    10th    260th   1010th  |
  12 |    11th    261st   1011th  |
  13 |    12th    262nd   1012th  |
  14 |    13th    263rd   1013th  |
  15 |    14th    264th   1014th  |
  16 |    15th    265th   1015th  |
  17 |    16th    266th   1016th  |
  18 |    17th    267th   1017th  |
  19 |    18th    268th   1018th  |
  20 |    19th    269th   1019th  |
  21 |    20th    270th   1020th  |
  22 |    21st    271st   1021st  |
  23 |    22nd    272nd   1022nd  |
  24 |    23rd    273rd   1023rd  |
  25 |    24th    274th   1024th  |
  26 |    25th    275th   1025th  |
     +----------------------------+
```



## Swift


```Swift
func addSuffix(n:Int) -> String {
    if n % 100 / 10 == 1 {
        return "th"
    }

    switch n % 10 {
    case 1:
        return "st"
    case 2:
        return "nd"
    case 3:
        return "rd"
    default:
        return "th"
    }
}

for i in 0...25 {
    print("\(i)\(addSuffix(i)) ")
}
println()
for i in 250...265 {
    print("\(i)\(addSuffix(i)) ")
}
println()
for i in 1000...1025 {
    print("\(i)\(addSuffix(i)) ")
}
println()
```

```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## Tcl


```tcl
proc ordinal {n} {
    if {$n%100<10 || $n%100>20} {
	set suff [lindex {th st nd rd th th th th th th} [expr {$n % 10}]]
    } else {
	set suff th
    }
    return "$n'$suff"
}

foreach start {0 250 1000} {
    for {set n $start; set l {}} {$n<=$start+25} {incr n} {
	lappend l [ordinal $n]
    }
    puts $l
}
```

```txt

0'th 1'st 2'nd 3'rd 4'th 5'th 6'th 7'th 8'th 9'th 10'th 11'th 12'th 13'th 14'th 15'th 16'th 17'th 18'th 19'th 20'th 21'st 22'nd 23'rd 24'th 25'th
250'th 251'st 252'nd 253'rd 254'th 255'th 256'th 257'th 258'th 259'th 260'th 261'st 262'nd 263'rd 264'th 265'th 266'th 267'th 268'th 269'th 270'th 271'st 272'nd 273'rd 274'th 275'th
1000'th 1001'st 1002'nd 1003'rd 1004'th 1005'th 1006'th 1007'th 1008'th 1009'th 1010'th 1011'th 1012'th 1013'th 1014'th 1015'th 1016'th 1017'th 1018'th 1019'th 1020'th 1021'st 1022'nd 1023'rd 1024'th 1025'th

```



## uBasic/4tH

<lang>For x = 0 to 25                        ' Test range 0..25
  Push x : GoSub _PrintOrdinal
Next x  : Print

For x = 250 to 265                     ' Test range 250..265
  Push x : GoSub _PrintOrdinal
Next x : Print

For x = 1000 to 1025                   ' Test range 1000..1025
  Push x : GoSub _PrintOrdinal
Next x : Print

End                                    ' End test program
                                       ' ( n --)
_PrintOrdinal                          ' Ordinal subroutine
  If Tos() > -1 Then                   ' If within range then
    Print Using "____#";Tos();"'";     ' Print the number
                                       ' Take care of 11, 12 and 13
    If (Tos()%100 > 10) * (Tos()%100 < 14) Then
       Gosub (Pop() * 0) + 100         ' Clear stack and print "th"
       Return                          ' We're done here
    EndIf

    Push Pop() % 10                    ' Calculate n mod 10
    GoSub 100 + 10 * ((Tos()>0) + (Tos()>1) + (Tos()>2) - (3 * (Pop()>3)))
  Else                                 ' And decide which ordinal to use
    Print Pop();" is less than zero"   ' Otherwise, it is an error
  EndIf

Return
                                       ' Select and print proper ordinal
100 Print "th"; : Return
110 Print "st"; : Return
120 Print "nd"; : Return
130 Print "rd"; : Return
```

```txt

    0'th    1'st    2'nd    3'rd    4'th    5'th    6'th    7'th    8'th    9'th   10'th   11'th   12'th   13'th   14'th   15'th   16'th   17'th   18'th   19'th   20'th   21'st   22'nd   23'rd   24'th   25'th
  250'th  251'st  252'nd  253'rd  254'th  255'th  256'th  257'th  258'th  259'th  260'th  261'st  262'nd  263'rd  264'th  265'th
 1000'th 1001'st 1002'nd 1003'rd 1004'th 1005'th 1006'th 1007'th 1008'th 1009'th 1010'th 1011'th 1012'th 1013'th 1014'th 1015'th 1016'th 1017'th 1018'th 1019'th 1020'th 1021'st 1022'nd 1023'rd 1024'th 1025'th

```



## VBA

```vb
Private Function ordinals() As Variant
    ordinals = [{"th","st","nd","rd"}]
End Function

Private Function Nth(n As Variant, Optional apostrophe As Boolean = False) As String
    Dim mod10 As Integer: mod10 = n Mod 10 + 1
    If mod10 > 4 Or n Mod 100 = mod10 + 9 Then mod10 = 1
    Nth = CStr(n) & String$(Val(-apostrophe), "'") & ordinals()(mod10)
End Function

Public Sub main()
    Ranges = [{0,25;250,265;1000,1025}]
    For i = 1 To UBound(Ranges)
        For j = Ranges(i, 1) To Ranges(i, 2)
            If j Mod 10 = 0 Then Debug.Print
            Debug.Print Format(Nth(j, i = 2), "@@@@@@@");
        Next j
        Debug.Print
    Next i
End Sub
```
```txt
    0th    1st    2nd    3rd    4th    5th    6th    7th    8th    9th
   10th   11th   12th   13th   14th   15th   16th   17th   18th   19th
   20th   21st   22nd   23rd   24th   25th

 250'th 251'st 252'nd 253'rd 254'th 255'th 256'th 257'th 258'th 259'th
 260'th 261'st 262'nd 263'rd 264'th 265'th

 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th
 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th
 1020th 1021st 1022nd 1023rd 1024th 1025th
```



## XBasic

```xbasic

PROGRAM "nth"
VERSION "0.0002"

DECLARE FUNCTION  Entry()
INTERNAL FUNCTION Suffix$(n&&)
INTERNAL FUNCTION PrintImages (loLim&&, hiLim&&)

FUNCTION Entry()
  PrintImages(   0,   25)
  PrintImages( 250,  265)
  PrintImages(1000, 1025)
END FUNCTION

FUNCTION Suffix$(n&&)
  nMod10@@ = n&& MOD 10
  nMod100@@ = n&& MOD 100
  SELECT CASE TRUE
    CASE (nMod10@@ = 1) AND (nMod100@@ <> 11):
      RETURN ("st")
    CASE (nMod10@@ = 2) AND (nMod100@@ <> 12):
      RETURN ("nd")
    CASE (nMod10@@ = 3) AND (nMod100@@ <> 13):
      RETURN ("rd")
    CASE ELSE:
      RETURN ("th")
  END SELECT
END FUNCTION

FUNCTION PrintImages(loLim&&, hiLim&&)
  FOR i&& = loLim&& TO hiLim&&
    PRINT TRIM$(STRING$(i&&)); Suffix$(i&&); " ";
  NEXT
  PRINT
END FUNCTION
END PROGRAM

```

```txt

0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th
250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th
1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th

```



## XLISP


```xlisp
(DEFUN NTH (N)
    (COND
        ((AND (> (MOD N 100) 3) (< (MOD N 100) 21)) `(,N TH))
        ((= (MOD N 10) 1) `(,N ST))
        ((= (MOD N 10) 2) `(,N ND))
        ((= (MOD N 10) 3) `(,N RD))
        (T `(,N TH))))

(DEFUN RANGE (X Y)
    (IF (<= X Y)
        (CONS X (RANGE (+ X 1) Y))))

(DEFUN TEST-NTH ()
    (DISPLAY (MAPCAR NTH (RANGE 1 25)))
    (NEWLINE)
    (DISPLAY (MAPCAR NTH (RANGE 250 265)))
    (NEWLINE)
    (DISPLAY (MAPCAR NTH (RANGE 1000 1025))))

(TEST-NTH)
```

```txt
((1 ST) (2 ND) (3 RD) (4 TH) (5 TH) (6 TH) (7 TH) (8 TH) (9 TH) (10 TH) (11 TH) (12 TH) (13 TH) (14 TH) (15 TH) (16 TH) (17 TH) (18 TH) (19 TH) (20 TH) (21 ST) (22 ND) (23 RD) (24 TH) (25 TH))
((250 TH) (251 ST) (252 ND) (253 RD) (254 TH) (255 TH) (256 TH) (257 TH) (258 TH) (259 TH) (260 TH) (261 ST) (262 ND) (263 RD) (264 TH) (265 TH))
((1000 TH) (1001 ST) (1002 ND) (1003 RD) (1004 TH) (1005 TH) (1006 TH) (1007 TH) (1008 TH) (1009 TH) (1010 TH) (1011 TH) (1012 TH) (1013 TH) (1014 TH) (1015 TH) (1016 TH) (1017 TH) (1018 TH) (1019 TH) (1020 TH) (1021 ST) (1022 ND) (1023 RD) (1024 TH) (1025 TH))
```



## zkl

Two versions, your choice

```zkl
#if 0
fcn addSuffix(n){
   z:=n.abs()%100;
   if(11<=z<=13) return(String(n,"th"));
   z=z%10;
   String(n,(z==1 and "st") or (z==2 and "nd") or (z==3 and "rd") or "th");
}
#else
fcn addSuffix(n){
   var suffixes=T("th","st","nd","rd","th","th","th","th","th","th"); //0..10
   z:=n.abs()%100;
   String(n,(z<=10 or z>20) and suffixes[z%10] or "th");
}
#endif
```


```zkl
[0..25]     .apply(addSuffix).concat(",").println();
[250..265]  .apply(addSuffix).concat(",").println();
[1000..1025].apply(addSuffix).concat(",").println();
```

```txt

0th,1st,2nd,3rd,4th,5th,6th,7th,8th,9th,10th,11th,12th,13th,14th,15th,16th,17th,18th,19th,20th,21st,22nd,23rd,24th,25th
250th,251st,252nd,253rd,254th,255th,256th,257th,258th,259th,260th,261st,262nd,263rd,264th,265th
1000th,1001st,1002nd,1003rd,1004th,1005th,1006th,1007th,1008th,1009th,1010th,1011th,1012th,1013th,1014th,1015th,1016th,1017th,1018th,1019th,1020th,1021st,1022nd,1023rd,1024th,1025th

```



## ZX Spectrum Basic


```basic
 10 FOR n=0 TO 25
 20 GO SUB 140
 30 PRINT n$;" ";
 40 NEXT n
 50 FOR n=250 TO 265
 60 GO SUB 140
 70 PRINT n$;" ";
 80 NEXT n
 90 FOR n=1000 TO 1025
100 GO SUB 140
110 PRINT n$;" ";
120 NEXT n
130 STOP
140 LET s$="th"
150 LET n$=STR$ n
160 IF LEN n$=1 THEN GO TO 180
170 IF n$(LEN n$-1)="1" THEN GO TO 210
180 IF n$(LEN n$)="1" THEN LET s$="st"
190 IF n$(LEN n$)="2" THEN LET s$="nd"
200 IF n$(LEN n$)="3" THEN LET s$="rd"
210 LET n$=n$+s$
220 RETURN
```

```txt
0th 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th 21st 22nd 23rd 24th 25th 250th 251st 252nd 253rd 254th 255th 256th 257th 258th 259th 260th 261st 262nd 263rd 264th 265th 1000th 1001st 1002nd 1003rd 1004th 1005th 1006th 1007th 1008th 1009th 1010th 1011th 1012th 1013th 1014th 1015th 1016th 1017th 1018th 1019th 1020th 1021st 1022nd 1023rd 1024th 1025th
```

