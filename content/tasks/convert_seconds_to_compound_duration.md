+++
title = "Convert seconds to compound duration"
description = ""
date = 2019-10-07T22:57:54Z
aliases = []
[extra]
id = 19231
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Write a function or program which:
* takes a positive integer representing a duration in seconds as input (e.g., <code>100</code>), and
* returns a string which shows the same duration decomposed into weeks, days, hours, minutes, and seconds as detailed below (e.g., "<code>1 min, 40 sec</code>").



Demonstrate that it passes the following three test-cases:

<p style="font-size:115%; margin:1em 0 0.5em 0">'''''Test Cases'''''</p>

{| class="wikitable"
|-
! input number
! output string
|-
| 7259
| <code style="background:#eee">2 hr, 59 sec</code>
|-
| 86400
| <code style="background:#eee">1 d</code>
|-
| 6000000
| <code style="background:#eee">9 wk, 6 d, 10 hr, 40 min</code>
|}

<p style="font-size:115%; margin:1em 0 0.5em 0">'''''Details'''''</p>

<ul>
<li>The following five units should be used:
{| class="wikitable"
|-
! unit
! suffix used in output
! conversion
|-
| week
| <code style="background:#eee">wk</code>
| 1 week = 7 days
|-
| day
| <code style="background:#eee">d</code>
| 1 day = 24 hours
|-
| hour
| <code style="background:#eee">hr</code>
| 1 hour = 60 minutes
|-
| minute
| <code style="background:#eee">min</code>
| 1 minutes = 60 seconds
|-
| second
| <code style="background:#eee">sec</code>
|
|}
</li>

<li>However, '''only''' include quantities with non-zero values in the output (e.g., return "<code>1 d</code>" and not "<code>0 wk, 1 d, 0 hr, 0 min, 0 sec</code>").</li>

<li>Give larger units precedence over smaller ones as much as possible (e.g., return <code>2 min, 10 sec</code> and not <code>1 min, 70 sec</code> or <code>130 sec</code>)</li>

<li>Mimic the formatting shown in the test-cases (quantities sorted from largest unit to smallest and separated by comma+space; value and unit of each quantity separated by space).</li>
</ul>

<hr style="margin:1em 0;"/>





## 11l

```11l
F duration(=sec)
   [Int] t
   L(dm) [60, 60, 24, 7]
      Int m
      (sec, m) = (sec I/ dm, sec % dm)
      t.insert(0, m)
   t.insert(0, sec)
   R zip(t, [‘wk’, ‘d’, ‘hr’, ‘min’, ‘sec’]).filter(num_unit -> num_unit[0] > 0).map(num_unit -> num_unit[0]‘ ’num_unit[1]).join(‘, ’)

print(duration(7259))
print(duration(86400))
print(duration(6000000))
```



## Ada


```Ada
with Ada.Text_IO;

procedure Convert is

   type Time is range 0 .. 10_000*356*20*60*60; -- at most 10_000 years
   subtype Valid_Duration is Time range 1  .. 10_000*356*20*60*60;
   type Units is (WK, D, HR, MIN, SEC);

   package IO renames Ada.Text_IO;

   Divide_By: constant array(Units) of Time := (1_000*53, 7, 24, 60, 60);
   Split: array(Units) of Time;
   No_Comma: Units;
   X: Time;

   Test_Cases: array(Positive range <>) of Valid_Duration :=
     (6, 60, 3659, 7_259, 86_400, 6_000_000, 6_001_200, 6_001_230, 600_000_000);

begin
  for Test_Case of Test_Cases loop
     IO.Put(Time'Image(Test_Case) & " SECONDS =");
     X := Test_Case;

     -- split X up into weeks, days, ..., seconds
     No_Comma := Units'First;
     for Unit in reverse Units loop -- Unit = SEC, ..., WK (in that order)
	Split(Unit) := X mod Divide_By(Unit);
	X := X / Divide_By(Unit);
	if Unit > No_Comma and Split(Unit)>0 then
	   No_Comma := Unit;
	end if;
     end loop;

     -- ouput weeks, days, ..., seconds
     for Unit in Units loop -- Unit =  WK, .., SEC (in that order)
	if Split(Unit) > 0 then
	   IO.Put(Time'Image(Split(Unit)) & " " & Units'Image(Unit) &
		    (if No_Comma > Unit then "," else ""));
	end if;
     end loop;

  IO.New_Line;
  end loop;
end Convert;
```


```txt
 6 SECONDS = 6 SEC
 60 SECONDS = 1 MIN
 3659 SECONDS = 1 HR, 59 SEC
 7259 SECONDS = 2 HR, 59 SEC
 86400 SECONDS = 1 D
 6000000 SECONDS = 9 WK, 6 D, 10 HR, 40 MIN
 6001200 SECONDS = 9 WK, 6 D, 11 HR
 6001230 SECONDS = 9 WK, 6 D, 11 HR, 30 SEC
 600000000 SECONDS = 992 WK, 10 HR, 40 MIN
```




## ALGOL 68


```algol68
# MODE to hold the compound duration #
MODE DURATION = STRUCT( INT weeks, days, hours, minutes, seconds );

# returns seconds converted to a DURATION #
OP TODURATION = ( LONG INT seconds )DURATION:
   BEGIN
       LONG INT time     := seconds;
       DURATION result   := DURATION( 0, 0, 0, 0, 0 );
       seconds OF result := SHORTEN ( time MOD  60 );
       time OVERAB 60;
       minutes OF result := SHORTEN ( time MOD  60 );
       time OVERAB 60;
       hours   OF result := SHORTEN ( time MOD  24 );
       time OVERAB 24;
       days    OF result := SHORTEN ( time MOD   7 );
       time OVERAB 7;
       weeks   OF result := SHORTEN   time;
       result
   END # DURATION # ;# returns seconds converted to a DURATION #
OP TODURATION = ( INT seconds )DURATION: TODURATION LENG seconds;

# returns a readable form of the DURATION #
OP TOSTRING = ( DURATION t )STRING:
   BEGIN
       STRING result    := "";
       STRING separator := "";
       IF weeks   OF t /= 0 THEN result +:= separator + whole( weeks   OF t, 0 ) + " wk";  separator := ", " FI;
       IF days    OF t /= 0 THEN result +:= separator + whole( days    OF t, 0 ) + " d";   separator := ", " FI;
       IF hours   OF t /= 0 THEN result +:= separator + whole( hours   OF t, 0 ) + " hr";  separator := ", " FI;
       IF minutes OF t /= 0 THEN result +:= separator + whole( minutes OF t, 0 ) + " min"; separator := ", " FI;
       IF seconds OF t /= 0 THEN result +:= separator + whole( seconds OF t, 0 ) + " sec"; separator := ", " FI;
       IF result = ""
       THEN
           # duration is 0 #
           result := "0 sec"
       FI;
       result
   END # TOSTRING # ;

# test cases #
print( ( TOSTRING TODURATION    7259, newline ) );
print( ( TOSTRING TODURATION   86400, newline ) );
print( ( TOSTRING TODURATION 6000000, newline ) )
```

```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```



## ALGOL W

Based on Algol 68 but Algol W does not have dynamic string handling which makes this more complex.

```algolw
begin
    % record structure to hold a compound duration %
    record Duration ( integer weeks, days, hours, minutes, seconds );

    % returns seconds converted to a Duration %
    reference(Duration) procedure toDuration( integer value secs ) ;
    begin
        integer  time;
        reference(Duration) d;
        time := secs;
        d    := Duration( 0, 0, 0, 0, 0 );
        seconds(d) := time rem 60;
        time       := time div 60;
        minutes(d) := time rem 60;
        time       := time div 60;
        hours(d)   := time rem 24;
        time       := time div 24;
        days(d)    := time rem  7;
        time       := time div  7;
        weeks(d)   := time;
        d
    end toDuration ;

    % returns a readable form of the DURATION %
    string(80) procedure durationToString ( reference(Duration) value d ) ;
    begin
        % appends an element of the compound duration to text %
        procedure add ( integer   value componentValue
                      ; string(6) value componentName
                      ; integer   value nameLength
                      ) ;
        begin
            string(9) vStr;
            integer   v, vPos;
            if needSeparator then begin
                % must separate this component from the previous %
                text( textPos // 2 ) := ", ";
                textPos:= textPos + 2
             end if_needSepartator ;
             % add the value %
             % construct a string representaton of the value with the digits reversed %
             % as this routine isn't called if componentValue is 0 or -ve, we don't need to handle %
             % the componentVaue <= 0 case %
             v    := componentValue;
             vStr := "";
             vPos := 0;
             while v > 0 do begin
                 vStr( vPos // 1 ) := code( decode( "0" ) + ( v rem 10 ) );
                 vPos              := vPos + 1;
                 v                 := v div 10
             end while_v_gt_0 ;
             % add the digits in the correct order %
             while vPos > 0 do begin
                 vPos                 := vPos - 1;
                 text( textPos // 1 ) := vStr( vPos // 1 );
                 textPos              := textPos + 1
             end while_vPos_gt_0 ;
             % add the component name %
             text( textPos // 6 ) := componentName;
             textPos := textPos + nameLength;
             % if there is another component, we'll need a separator %
             needSeparator := true
        end add ;

        string(80) text;
        logical    needSeparator;
        integer    textPos;
        textPos       := 0;
        text          := "";
        needSeparator := false;
        if   weeks(d) not = 0 then add(   weeks(d), " wk",  3 );
        if    days(d) not = 0 then add(    days(d), " d",   2 );
        if   hours(d) not = 0 then add(   hours(d), " hr",  3 );
        if minutes(d) not = 0 then add( minutes(d), " min", 4 );
        if seconds(d) not = 0 then add( seconds(d), " sec", 4 );
        if text = "" then begin
            % duration is 0 %
            text := "0 sec"
        end if_text_is_blank ;
        text
   end % durationToString % ;

    % test cases %
    write( durationToString( toDuration(    7259 ) ) );
    write( durationToString( toDuration(   86400 ) ) );
    write( durationToString( toDuration( 6000000 ) ) )
end.
```

```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```



## AppleScript


```AppleScript
-- LOCAL NAMES FOR COMPOUND DURATIONS ----------------------------------------

script angloNames
    on |λ|(n)
        (n as string) & "     ->    " & ¬
            localCompoundDuration(["wk", "d", "hr", "min", "sec"], n)
    end |λ|
end script

unlines(map(angloNames, [7259, 86400, 6000000]))


-- DURATION STRINGS ----------------------------------------------------------

-- weekParts Int -> [Int]
on weekParts(intSeconds)
    unitParts(intSeconds, [missing value, 7, 24, 60, 60])
end weekParts

-- localCompoundDuration :: Int -> String
on localCompoundDuration(localNames, intSeconds)

    -- [String] -> (Int, String) -> [String]
    script formatted
        on |λ|(lstPair, a)
            set q to item 1 of lstPair
            if q > 0 then
                {(q as string) & space & item 2 of lstPair} & a
            else
                a
            end if
        end |λ|
    end script

    intercalate(", ", ¬
        foldr(formatted, [], ¬
            zip(weekParts(intSeconds), localNames)))
end localCompoundDuration

-- INTEGER DECOMPOSITION -----------------------------------------------------

-- unitParts :: Int -> [maybe Int] -> [Int]
on unitParts(intTotal, unitList)
    -- partList :: Record -> Int -> Record
    script partList
        on |λ|(x, a)
            set intRest to remaining of a

            if x is not missing value then
                set intMod to intRest mod x
                set d to x
            else
                set intMod to intRest
                set d to 1
            end if

            {remaining:(intRest - intMod) div d, parts:{intMod} & parts of a}
        end |λ|
    end script

    parts of foldr(partList, ¬
        {remaining:intTotal, parts:[]}, unitList)
end unitParts


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- foldr :: (b -> a -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(item i of xs, v, i, xs)
        end repeat
        return v
    end tell
end foldr

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

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines

-- zip :: [a] -> [b] -> [(a, b)]
on zip(xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    repeat with i from 1 to lng
        set end of lst to {item i of xs, item i of ys}
    end repeat
    return lst
end zip
```

```txt
7259     ->    2 hr, 59 sec
86400     ->    1 d
6000000     ->    9 wk, 6 d, 10 hr, 40 min
```



## Applesoft BASIC


```ApplesoftBasic
100 DATA604800,WK,86400,D,3600,HR,60,MIN,1,SEC
110 FOR I = 0 TO 4
120     READ M(I), U$(I)
130 NEXT
140 DATA7259,86400,6000000
150 ON ERR GOTO 270
160     READ S
170     GOSUB 200
180     PRINT S " = " S$
190 GOTO 160

200 N = S
210 S$ = ""
220 FOR I = 0 TO 4
230     IF INT(N / M(I)) THEN S$ = S$ + MID$(", ", 1, (LEN(S$) > 0) * 2) + STR$(INT(N / M(I))) + " " + U$(I)
240     N = N - INT(N / M(I)) * M(I)
250 NEXT I
260 RETURN

270 END

```



## AutoHotkey


```AutoHotkey
duration(n){
	sec:=1, min:=60*sec, hr:=60*min, day:=24*hr, wk:=7*day
	w	:=n//wk		, n:=Mod(n,wk)
	d	:=n//day	, n:=Mod(n,day)
	h	:=n//hr		, n:=Mod(n,hr)
	m	:=n//min	, n:=Mod(n,min)
	s	:=n
	return trim((w?w " wk, ":"") (d?d " d, ":"") (h?h " hr, ":"") (m?m " min, ":"") (s?s " sec":""),", ")
}
```

Examples:
```AutoHotkey
data=
(
7259
86400
6000000
)

loop, parse, data, `n, `r
	res .= A_LoopField "`t: " duration(A_LoopField) "`n"
MsgBox % res
return
```

Outputs:
```txt
7259	: 2 hr, 59 sec
86400	: 1 d
6000000	: 9 wk, 6 d, 10 hr, 40 min
```



## AWK


```AWK

# syntax: GAWK -f CONVERT_SECONDS_TO_COMPOUND_DURATION.AWK
BEGIN {
    n = split("7259 86400 6000000 0 1 60 3600 604799 604800 694861",arr," ")
    for (i=1; i<=n; i++) {
      printf("%9s %s\n",arr[i],howlong(arr[i]))
    }
    exit(0)
}
function howlong(seconds,  n_day,n_hour,n_min,n_sec,n_week,str,x) {
    if (seconds >= (x = 60*60*24*7)) {
      n_week = int(seconds / x)
      seconds = seconds % x
    }
    if (seconds >= (x = 60*60*24)) {
      n_day = int(seconds / x)
      seconds = seconds % x
    }
    if (seconds >= (x = 60*60)) {
      n_hour = int(seconds / x)
      seconds = seconds % x
    }
    if (seconds >= (x = 60)) {
      n_min = int(seconds / x)
      seconds = seconds % x
    }
    n_sec = int(seconds)
    str = (n_week > 0) ? (str n_week " wk, ") : str
    str = (n_day > 0) ? (str n_day " d, ") : str
    str = (n_hour > 0) ? (str n_hour " hr, ") : str
    str = (n_min > 0) ? (str n_min " min, ") : str
    str = (n_sec > 0) ? (str n_sec " sec") : str
    sub(/, $/,"",str)
    return(str)
}

```

<p>Output:</p>

```txt

     7259 2 hr, 59 sec
    86400 1 d
  6000000 9 wk, 6 d, 10 hr, 40 min
        0
        1 1 sec
       60 1 min
     3600 1 hr
   604799 6 d, 23 hr, 59 min, 59 sec
   604800 1 wk
   694861 1 wk, 1 d, 1 hr, 1 min, 1 sec

```



## Batch File


```dos
@echo off
::The Main Thing...
for %%d in (7259 86400 6000000) do call :duration %%d
exit/b 0
::/The Main Thing.

::The Function...
:duration
	set output=
	set /a "wk=%1/604800,rem=%1%%604800"
	if %wk% neq 0 set "output= %wk% wk,"

	set /a "d=%rem%/86400,rem=%rem%%%86400"
	if %d% neq 0 set "output=%output% %d% d,"

	set /a "hr=%rem%/3600,rem=%rem%%%3600"
	if %hr% neq 0 set "output=%output% %hr% hr,"

	set /a "min=%rem%/60,rem=%rem%%%60"
	if %min% neq 0 set "output=%output% %min% min,"

	if %rem% neq 0 set "output=%output% %rem% sec,"

	if %1 gtr 0 echo %1 sec = %output:~1,-1%
	goto :EOF
::/The Function.
```

```txt

7259 sec = 2 hr, 59 sec
86400 sec = 1 d
6000000 sec = 9 wk, 6 d, 10 hr, 40 min

```



## BASIC


=
## Commodore BASIC
=

```gwbasic
10 REM CONVERT SECONDS TO COMPOUND DURATION
20 REM ADAPTED FROM RUN BASIC VERSION
30 REM
### =========================================================

40 PRINT CHR$(14)
50 SEC = 7259
60 GOSUB 1000
70 SEC = 85400
80 GOSUB 1000
90 SEC = 6000000
100 GOSUB 1000
110 END
120 REM
### ========================================================

1000 WK  = INT(SEC/60/60/24/7)
1010 DY  = INT(SEC/60/60/24) - 7*WK
1020 HR  = INT(SEC/60/60) - 24*(DY+7*WK)
1030 MN  = INT(SEC/60) - 60*(HR+24*(DY+7*WK))
1040 SC  = SEC - 60*(MN+60*(HR+24*(DY+7*WK)))
1050 PRINT SEC;"SEC" : PRINT "  =";
1055 F = 0
1060 IF WK = 0 THEN 1080
1070 PRINT WK;"WK"; : F = 1
1080 IF DY = 0 THEN 1110
1090 IF F THEN PRINT ",";
1100 PRINT DY;"DY"; : F = 1
1110 IF HR = 0 THEN 1140
1120 IF F THEN PRINT ",";
1130 PRINT HR;"HR"; : F = 1
1140 IF MN = 0 THEN 1170
1150 IF F THEN PRINT ",";
1160 PRINT MN;"MIN"; : F = 1
1170 IF (SC > 0) AND F THEN PRINT ",";SC;"SEC" : GOTO 1200
1180 IF (SC = 0) AND F THEN 1200
1190 PRINT SC;"SEC"
1200 PRINT
1210 RETURN
```

 7259 sec
  = 2 hr, 59 sec

 85400 sec
  = 23 hr, 43 min, 20 sec

 6000000 sec
  = 9 wk, 6 dy, 10 hr, 40 min

=
## BBC BASIC
=

```bbcbasic>REM
compduration
PRINT FN_convert(7259)
PRINT FN_convert(86400)
PRINT FN_convert(6000000)
END
:
DEF FN_convert(seconds%)
LOCAL units%(), units$(), i%, unit%, compound$
DIM units%(4)
DIM units$(4)
units%() = 604800, 86400, 3600, 60, 1
units$() = "wk", "d", "hr", "min", "sec"
compound$ = ""
FOR i% = 0 TO 4
  IF seconds% >= units%(i%) THEN
    unit% = seconds% DIV units%(i%)
    seconds% = seconds% MOD units%(i%)
    compound$ += STR$(unit%) + " " + units$(i%)
    IF i% < 4 AND seconds% > 0 THEN compound$ += ", "
  ENDIF
NEXT
= compound$
```

```txt
2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Seconds.bas"
110 NUMERIC UN(1 TO 5),SEC,UNIT
120 STRING T$(1 TO 5)*3
130 LET UN(1)=604800:LET UN(2)=86400:LET UN(3)=3600:LET UN(4)=60:LET UN(5)=1
140 LET T$(1)="wk":LET T$(2)="d":LET T$(3)="hr":LET T$(4)="min":LET T$(5)="sec"
150 INPUT PROMPT "Duration in seconds: ":SEC
160 PRINT SEC;"sec =";
170 FOR I=1 TO 5
180   IF SEC>=UN(I) THEN
190     LET UNIT=INT(SEC/UN(I)):LET SEC=MOD(SEC,UN(I))
200     PRINT UNIT;T$(I);
210     IF I<4 AND SEC>0 THEN PRINT ",";
220   END IF
230 NEXT
240 PRINT
```



## beeswax



```beeswax>#
%f# #>%f#  #f%<##>%f#
pq":X~7~ :X@~++8~8@:X:X@~-~4~.+~8@T_
    ##    ##        ####          #`K0[`}`D2[`}BF3<         <
>{` wk, `>g?"p{` d, `>g?"p{` hr, `>g?"p{` min, `>g"b{` sec, `b
   >     d   >       d   >        d   >         d
```


```txt

julia> beeswax("seconds to compound duration.bswx")
i7259
2 hr, 59 sec
Program finished!

julia> beeswax("seconds to compound duration.bswx")
i86400
1 d
Program finished!

julia> beeswax("seconds to compound duration.bswx")
i6000000
9 wk, 6 d, 10 hr, 40 min
Program finished!

```



## Befunge

The value to convert is read from stdin, and the corresponding compound duration is written to stdout.


```befunge
&>:"<"%\"O{rq"**+\"<"/:"<"%\"r<|":*+*5-\v
v-7*"l~"/7\"d"\%7:/*83\+*:"xD"\%*83:/"<"<
> \:! #v_v#-#<",",#$48*#<,#<.#<>#_:"~"%,v
^_@#:$$< >       .02g92p       ^ ^!:/"~"<
```


```txt
7259
2 hr, 59 sec
86400
1 d
6000000
9 wk, 6 d, 10 hr, 40 min
```



## C



### C: Version written in C89. Average skill level.


```c
/*
 * Program seconds2string, C89 version.
 *
 * Read input from argv[1] or stdin, write output to stdout.
 */

#define _CRT_SECURE_NO_WARNINGS /* unlocks printf in Microsoft Visual Studio */

#include <stdio.h>
#include <stdlib.h>

/*
 * Converting the number of seconds in a human-readable string.
 * It is worth noting that direct output to stdout would be even simpler.
 */
char* seconds2string(unsigned long seconds)
{
    int i;

    const unsigned long s =  1;
    const unsigned long m = 60 * s;
    const unsigned long h = 60 * m;
    const unsigned long d = 24 * h;
    const unsigned long w =  7 * d;

    const unsigned long coeff[5] = { w, d, h, m, s };
    const char units[5][4] = { "wk", "d", "hr", "min", "sec" };

    static char buffer[256];
    char* ptr = buffer;

    for ( i = 0; i < 5; i++ )
    {
        unsigned long value;
        value   = seconds / coeff[i];
        seconds = seconds % coeff[i];
        if ( value )
        {
            if ( ptr != buffer )
                ptr += sprintf(ptr, ", ");
            ptr += sprintf(ptr,"%lu %s",value,units[i]);
        }
    }

    return buffer;
}

/*
 * Main function for seconds2string program.
 */
int main(int argc, char argv[])
{
    unsigned long seconds;

    if ( (argc <  2) && scanf( "%lu", &seconds )
    ||   (argc >= 2) && sscanf( argv[1], "%lu", & seconds ) )
    {
        printf( "%s\n", seconds2string(seconds) );
        return EXIT_SUCCESS;
    }

    return EXIT_FAILURE;
}
```



### C: Version written in C99. Low skill level.


```c

#include <inttypes.h> /* requires c99 */
#include <stdbool.h>  /* requires c99 */
#include <stdio.h>
#include <stdlib.h>

#define N_EL 5

uintmax_t sec_to_week(uintmax_t);
uintmax_t sec_to_day(uintmax_t);
uintmax_t sec_to_hour(uintmax_t);
uintmax_t sec_to_min(uintmax_t);

uintmax_t week_to_sec(uintmax_t);
uintmax_t day_to_sec(uintmax_t);
uintmax_t hour_to_sec(uintmax_t);
uintmax_t min_to_sec(uintmax_t);

char *format_sec(uintmax_t);
    /* the primary function */


int main(int argc, char *argv[])
{
    uintmax_t input;
    char *a;

    if(argc<2) {
        printf("usage: %s #seconds\n", argv[0]);
        return 1;
    }
    input = strtoumax(argv[1],(void *)0, 10 /*base 10*/);
    if(input<1) {
        printf("Bad input: %s\n", argv[1]);
        printf("usage: %s #seconds\n", argv[0]);
        return 1;
    }
    printf("Number entered: %" PRIuMAX "\n", input);
    a = format_sec(input);
    printf(a);
    free(a);

    return 0;
}

/* note: must free memory
 * after using this function */
char *format_sec(uintmax_t input)
{
    int i;
    bool first;
    uintmax_t weeks, days, hours, mins;
    /*seconds kept in input*/

    char *retval;
    FILE *stream;
    size_t size;
    uintmax_t *traverse[N_EL]={&weeks,&days,
            &hours,&mins,&input};
    char *labels[N_EL]={"wk","d","hr","min","sec"};

    weeks = sec_to_week(input);
    input = input - week_to_sec(weeks);

    days = sec_to_day(input);
    input = input - day_to_sec(days);

    hours = sec_to_hour(input);
    input = input - hour_to_sec(hours);

    mins = sec_to_min(input);
    input = input - min_to_sec(mins);
    /* input now has the remaining seconds */

    /* open stream */
    stream = open_memstream(&retval,&size);
    if(stream == 0) {
        fprintf(stderr,"Unable to allocate memory");
        return 0;
    }

    /* populate stream */
    first = true;
    for(i=0;i<N_EL;i++) {
        if ( *(traverse[i]) != 0 ) {
            if(!first) {
                fprintf(stream,", %" PRIuMAX " %s",
                        *(traverse[i]), labels[i]);
            } else {
                fprintf(stream,"%" PRIuMAX " %s",
                        *(traverse[i]), labels[i]);
            }
            fflush(stream);
            first=false;
        }
    }
    fprintf(stream,"\n");
    fclose(stream);
    return retval;

}

uintmax_t sec_to_week(uintmax_t seconds)
{
    return sec_to_day(seconds)/7;
}

uintmax_t sec_to_day(uintmax_t seconds)
{
    return sec_to_hour(seconds)/24;
}

uintmax_t sec_to_hour(uintmax_t seconds)
{
    return sec_to_min(seconds)/60;
}

uintmax_t sec_to_min(uintmax_t seconds)
{
    return seconds/60;
}

uintmax_t week_to_sec(uintmax_t weeks)
{
    return day_to_sec(weeks*7);
}

uintmax_t day_to_sec(uintmax_t days)
{
    return hour_to_sec(days*24);
}

uintmax_t hour_to_sec(uintmax_t hours)
{
    return min_to_sec(hours*60);
}

uintmax_t min_to_sec(uintmax_t minutes)
{
    return minutes*60;
}

```


```txt

Number entered: 7259
2 hr, 59 sec

Number entered: 86400
1 d

Number entered: 6000000
9 wk, 6 d, 10 hr, 40 min

```



## C#



### C#: Standard method


```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace ConvertSecondsToCompoundDuration
{
  class Program
  {
    static void Main( string[] args )
    {
      foreach ( string arg in args )
      {
        int duration ;
        bool isValid = int.TryParse( arg , out duration ) ;

        if ( !isValid     ) { Console.Error.WriteLine( "ERROR: Not an integer: {0}"           , arg ) ; }
        if ( duration < 0 ) { Console.Error.WriteLine( "ERROR: duration must be non-negative" , arg ) ; }

        Console.WriteLine();
        Console.WriteLine( "{0:#,##0} seconds ==> {1}" , duration , FormatAsDuration(duration) ) ;

      }
    }

    private static string FormatAsDuration( int duration )
    {
      if ( duration < 0 ) throw new ArgumentOutOfRangeException("duration") ;
      return string.Join( ", " , GetDurationParts(duration)  ) ;
    }

    private static IEnumerable<string> GetDurationParts( int duration )
    {
      var parts = new[]
      {
        new { Name="wk" , Length = 7*24*60*60*1 , } ,
        new { Name="d"  , Length =   24*60*60*1 , } ,
        new { Name="h"  , Length =      60*60*1 , } ,
        new { Name="m"  , Length =         60*1 , } ,
        new { Name="s"  , Length =            1 , } ,
      } ;

      foreach ( var part in parts )
      {
        int n = Math.DivRem( duration , part.Length , out duration ) ;
        if ( n > 0 ) yield return string.Format( "{0} {1}" , n , part.Name ) ;
      }

    }

  }

}
```


```txt

7,259 seconds ==> 2 h, 59 s

86,400 seconds ==> 1 d

6,000,000 seconds ==> 9 wk, 6 d, 10 h, 40 m

```



### C#: Using the TimeSpan struct and query syntax

```c#
private static string ConvertToCompoundDuration(int seconds)
{
    if (seconds < 0) throw new ArgumentOutOfRangeException(nameof(seconds));
    if (seconds == 0) return "0 sec";

    TimeSpan span = TimeSpan.FromSeconds(seconds);
    int[] parts = {span.Days / 7, span.Days % 7, span.Hours, span.Minutes, span.Seconds};
    string[] units = {" wk", " d", " hr", " min", " sec"};

    return string.Join(", ",
        from index in Enumerable.Range(0, units.Length)
        where parts[index] > 0
        select parts[index] + units[index]);
}
```



## C++

```cpp

#include <iostream>
#include <vector>

using entry = std::pair<int, const char*>;

void print(const std::vector<entry>& entries, std::ostream& out = std::cout)
{
    bool first = true;
    for(const auto& e: entries) {
        if(!first) out << ", ";
        first = false;
        out << e.first << " " << e.second;
    }
    out << '\n';
}

std::vector<entry> convert(int seconds)
{
    static const entry time_table[] = {
        {7*24*60*60, "wk"}, {24*60*60, "d"}, {60*60, "hr"}, {60, "min"}, {1, "sec"}
    };
    std::vector<entry> result;
    for(const auto& e: time_table) {
        int time = seconds / e.first;
        if(time != 0) result.emplace_back(time, e.second);
        seconds %= e.first;
    }
    return result;
}

int main()
{
    std::cout << "   7259 sec is "; print(convert(   7259));
    std::cout << "  86400 sec is "; print(convert(  86400));
    std::cout << "6000000 sec is "; print(convert(6000000));
}
```

```txt

   7259 sec is 2 hr, 59 sec
  86400 sec is 1 d
6000000 sec is 9 wk, 6 d, 10 hr, 40 min

```



## Clojure


```clojure
(require '[clojure.string :as string])

(def seconds-in-minute 60)
(def seconds-in-hour (* 60 seconds-in-minute))
(def seconds-in-day (* 24 seconds-in-hour))
(def seconds-in-week (* 7 seconds-in-day))

(defn seconds->duration [seconds]
  (let [weeks   ((juxt quot rem) seconds seconds-in-week)
        wk      (first weeks)
        days    ((juxt quot rem) (last weeks) seconds-in-day)
        d       (first days)
        hours   ((juxt quot rem) (last days) seconds-in-hour)
        hr      (first hours)
        min     (quot (last hours) seconds-in-minute)
        sec     (rem (last hours) seconds-in-minute)]
    (string/join ", "
                 (filter #(not (string/blank? %))
                         (conj []
                               (when (> wk 0) (str wk " wk"))
                               (when (> d 0) (str d " d"))
                               (when (> hr 0) (str hr " hr"))
                               (when (> min 0) (str min " min"))
                               (when (> sec 0) (str sec " sec")))))))

(seconds->duration 7259)
(seconds->duration 86400)
(seconds->duration 6000000)
```


```txt

"2 hr, 59 sec"
"1 d"
"9 wk, 6 d, 10 hr, 40 min"

```



## COBOL


```COBOL
       identification division.
       program-id. fmt-dura.
       data division.
       working-storage section.
       1 input-seconds pic 9(8).
       1 formatted-duration pic x(30) global.
       1 fractions.
        2 weeks pic z(3)9.
        2 days pic z(3)9.
        2 hours pic z(3)9.
        2 minutes pic z(3)9.
        2 seconds pic z(3)9.
       1 .
        2 weeks-str pic x(4) value "wk".
        2 days-str pic x(4) value "d".
        2 hours-str pic x(4) value "hr".
        2 minutes-str pic x(4) value "min".
        2 seconds-str pic x(4) value "sec".
       1 work binary global.
        2 str-pos pic 9(4).
        2 chars-transferred pic 9(4).
       procedure division.
       begin.
           display "Enter duration (seconds): " no advancing
           accept input-seconds
           divide input-seconds by 60 giving input-seconds
               remainder seconds
           divide input-seconds by 60 giving input-seconds
               remainder minutes
           divide input-seconds by 24 giving input-seconds
               remainder hours
           divide input-seconds by 7 giving weeks
               remainder days
           move 1 to str-pos
           call "fmt" using weeks weeks-str
           call "fmt" using days days-str
           call "fmt" using hours hours-str
           call "fmt" using minutes minutes-str
           call "fmt" using seconds seconds-str
           display formatted-duration
           stop run
           .

       identification division.
       program-id. fmt.
       data division.
       working-storage section.
       77 nothing pic x.
       linkage section.
       1 formatted-value pic x(4).
       1 duration-size pic x(4).
       procedure division using formatted-value duration-size.
       begin.
           if function numval (formatted-value) not = 0
               perform insert-comma-space
               unstring formatted-value delimited all space
                   into nothing formatted-duration (str-pos:)
                   count chars-transferred
               add chars-transferred to str-pos
               string space delimited size
                   duration-size delimited space
                   into formatted-duration pointer str-pos
           end-if
           exit program
           .

       insert-comma-space.
           if str-pos > 1
               move ", " to formatted-duration (str-pos:)
               add 2 to str-pos
           end-if
           .
       end program fmt.
       end program fmt-dura.
```


```txt
Enter duration (seconds): 7259
2 hr, 59 sec
Enter duration (seconds): 86400
1 d
Enter duration (seconds): 6000000
9 wk, 6 d, 10 hr, 40 min
```



## Common Lisp


```lisp
(defconstant +seconds-in-minute* 60)
(defconstant +seconds-in-hour* (* 60 +seconds-in-minute*))
(defconstant +seconds-in-day* (* 24 +seconds-in-hour*))
(defconstant +seconds-in-week* (* 7 +seconds-in-day*))

(defun seconds->duration (seconds)
  (multiple-value-bind (weeks wk-remainder) (floor seconds +seconds-in-week*)
    (multiple-value-bind (days d-remainder) (floor wk-remainder +seconds-in-day*)
      (multiple-value-bind (hours hr-remainder) (floor d-remainder +seconds-in-hour*)
        (multiple-value-bind (minutes secs) (floor hr-remainder +seconds-in-minute*)
          (let ((chunks nil))
            (unless (zerop secs)    (push (format nil "~D sec" secs) chunks))
            (unless (zerop minutes) (push (format nil "~D min" minutes) chunks))
            (unless (zerop hours)   (push (format nil "~D hr" hours) chunks))
            (unless (zerop days)    (push (format nil "~D d" days) chunks))
            (unless (zerop weeks)   (push (format nil "~D wk" weeks) chunks))
            (format t "~{~A~^, ~}~%" chunks)))))))

(seconds->duration 7259)
(seconds->duration 86400)
(seconds->duration 6000000)

```

```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```



## D


```d

import std.stdio, std.conv, std.algorithm;

immutable uint SECSPERWEEK = 604_800;
immutable uint SECSPERDAY = 86_400;
immutable uint SECSPERHOUR = 3_600;
immutable uint SECSPERMIN = 60;

string ConvertSeconds(in uint seconds)
{
    uint rem = seconds;

    uint weeks = rem / SECSPERWEEK;
    rem %= SECSPERWEEK;
    uint days = rem / SECSPERDAY;
    rem %= SECSPERDAY;
    uint hours = rem / SECSPERHOUR;
    rem %= SECSPERHOUR;
    uint mins = rem / SECSPERMIN;
    rem %= SECSPERMIN;

    string formatted = "";

    (weeks != 0) ? formatted ~= (weeks.to!string ~ " wk, ") : formatted;
    (days != 0) ? formatted ~= (days.to!string ~ " d, ") : formatted;
    (hours != 0) ? formatted ~= (hours.to!string ~ " hr, ") : formatted;
    (mins != 0) ? formatted ~= (mins.to!string ~ " min, ") : formatted;
    (rem != 0) ? formatted ~= (rem.to!string ~ " sec") : formatted;

    if (formatted.endsWith(", ")) return formatted[0..$-2];

    return formatted;
}

void main()
{
    7_259.ConvertSeconds.writeln;
    86_400.ConvertSeconds.writeln;
    6_000_000.ConvertSeconds.writeln;
}

```

```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```



## EasyLang

<lang>func split sec . s$ .
  div[] = [ 60 60 24 7 ]
  n$[] = [ "sec" "min" "hr" "d" "wk" ]
  len r[] 5
  for i range 4
    r[i] = sec mod div[i]
    sec = sec / div[i]
  .
  r[4] = sec
  s$ = ""
  for i = 4 downto 0
    if r[i] <> 0
      if s$ <> ""
        s$ &= ", "
      .
      s$ &= r[i] & " " & n$[i]
    .
  .
.
call split 7259 s$
print s$
call split 86400 s$
print s$
call split 6000000 s$
print s$
```


```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```



## Elixir


```elixir
defmodule Convert do
  @minute 60
  @hour   @minute*60
  @day    @hour*24
  @week   @day*7
  @divisor [@week, @day, @hour, @minute, 1]

  def sec_to_str(sec) do
    {_, [s, m, h, d, w]} =
        Enum.reduce(@divisor, {sec,[]}, fn divisor,{n,acc} ->
          {rem(n,divisor), [div(n,divisor) | acc]}
        end)
    ["#{w} wk", "#{d} d", "#{h} hr", "#{m} min", "#{s} sec"]
    |> Enum.reject(fn str -> String.starts_with?(str, "0") end)
    |> Enum.join(", ")
  end
end

Enum.each([7259, 86400, 6000000], fn sec ->
  :io.fwrite "~10w sec : ~s~n", [sec, Convert.sec_to_str(sec)]
end)
```


```txt

      7259 sec : 2 hr, 59 sec
     86400 sec : 1 d
   6000000 sec : 9 wk, 6 d, 10 hr, 40 min

```



## Erlang

Function ''mapaccumr/3'' is adapted from  [http://lpaste.net/edit/47875 here].

Function ''intercalate/2'' is copied from  [https://github.com/tim/erlang-oauth/blob/master/src/oauth.erl a Tim Fletcher's GitHub repository].


```erlang

-module(convert_seconds).

-export([test/0]).

test() ->
	lists:map(fun convert/1, [7259, 86400, 6000000]),
	ok.

convert(Seconds) ->
	io:format(
		"~7s seconds = ~s\n",
		[integer_to_list(Seconds), compoundDuration(Seconds)] ).

% Compound duration of t seconds.  The argument is assumed to be positive.
compoundDuration(Seconds) ->
	intercalate(
		", ",
		lists:map(
			fun({D,L}) -> io_lib:format("~p ~s",[D, L]) end,
			compdurs(Seconds) ) ).

% Time broken down into non-zero durations and their labels.
compdurs(T) ->
	Ds =
		reduceBy(
			T,
			lists:map(
				fun(Dl) -> element(1,Dl) end,
				tl(durLabs()) ) ),
	lists:filter(
			fun(Dl) -> element(1,Dl) /= 0 end,
			lists:zip(
				Ds,
				lists:map(
					fun(Dl) -> element(2,Dl) end,
					durLabs() ) ) ).

% Duration/label pairs.
durLabs() ->
	[
		{undefined, "wk"},
		{7, "d"},
		{24, "hr"},
		{60, "min"},
		{60, "sec"}
	].

reduceBy(N, Xs) ->
	{N_, Ys} = mapaccumr(fun quotRem/2, N, Xs),
	[N_ | Ys].

quotRem(X1, X2) ->
	{X1 div X2, X1 rem X2}.

% **************************************************
% Adapted from http://lpaste.net/edit/47875
% **************************************************

mapaccuml(_,I,[]) ->
	{I, []};
mapaccuml(F,I,[H|T]) ->
	{Accum, NH} = F(I,H),
	{FAccum, NT} = mapaccuml(F,Accum,T),
	{FAccum, [NH | NT]}.

mapaccumr(_,I,[]) ->
	{I, []};
mapaccumr(F,I,L) ->
	{Acc, Ys} = mapaccuml(F,I,lists:reverse(L)),
	{Acc, lists:reverse(Ys)}.

% **************************************************


% **************************************************
% Copied from https://github.com/tim/erlang-oauth/blob/master/src/oauth.erl
% **************************************************

intercalate(Sep, Xs) ->
  lists:concat(intersperse(Sep, Xs)).

intersperse(_, []) ->
  [];
intersperse(_, [X]) ->
  [X];
intersperse(Sep, [X | Xs]) ->
  [X, Sep | intersperse(Sep, Xs)].

% **************************************************

```


Output:

```txt

   7259 seconds = 2 hr, 59 sec
  86400 seconds = 1 d
6000000 seconds = 9 wk, 6 d, 10 hr, 40 min

```


=={{header|F_Sharp|F#}}==

```fsharp
open System

let convert seconds =
    let span = TimeSpan.FromSeconds(seconds |> float)
    let (wk, day) = Math.DivRem(span.Days, 7)
    let parts =
        [(wk, "wk"); (day, "day"); (span.Hours, "hr"); (span.Minutes, "min"); (span.Seconds, "sec")]
    let result =
        List.foldBack (fun (n, u) acc ->
                    (if n > 0 then n.ToString() + " " + u else "")
                    + (if n > 0 && acc.Length > 0 then ", " else "")
                    + acc
                  ) parts ""
    if result.Length > 0 then result else "0 sec"

[<EntryPoint>]
let main argv =
    argv
    |> Seq.map (fun str -> let sec = UInt32.Parse str in (sec, convert sec))
    |> Seq.iter (fun (s, v) -> printfn "%10i = %s" s v)
    0
```

```txt
>RosettaCode 7259 86400 6000000
      7259 = 2 hr, 59 sec
     86400 = 1 day
   6000000 = 9 wk, 6 day, 10 hr, 40 min
```



## Factor


```factor
USING: assocs io kernel math math.parser qw sequences
sequences.generalizations ;
IN: rosetta-code.compound-duration

CONSTANT: units qw{ wk d hr min sec }

: convert ( seconds -- {wk,d,hr,min,sec} )
    60 /mod swap 60 /mod swap 24 /mod swap 7 /mod swap
    5 narray reverse ;

: .compound ( n -- ) convert [ number>string ] map units zip
[ first "0" = not ] filter [ " " join ] map ", " join print ;

{ 7259 86400 6000000 } [ .compound ] each
```

```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```



## Forth

```Forth
CREATE C 0 ,
: .,   C @ IF ." , " THEN  1 C ! ;
: .TIME ( n --)
   [ 60 60 24 7 * * * ]L /MOD ?DUP-IF ., . ." wk" THEN
   [ 60 60 24   * *   ]L /MOD ?DUP-IF ., . ." d" THEN
   [ 60 60      *     ]L /MOD ?DUP-IF ., . ." hr" THEN
   [ 60               ]L /MOD ?DUP-IF ., . ." min" THEN
                              ?DUP-IF ., . ." sec" THEN  0 C ! ;
```

```txt
2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min
```



## Fortran

If the PARAMETER statements were replaced (say with suitable DATA statements) then this could be compiled with Fortran 77, if a find-last-non-blank function were to be supplied and a supplementary scratchpad used so that the numbers could be placed without leading spaces (thus "9" and "10", not " 9") because the I0 format specifier is a F90 facility. Earlier Fortran compilers lacking a CHARACTER variable would present further difficulty.

If the time to describe was not an integer but a floating-point value with fractional parts, then there is a complication. The number of seconds can be less than sixty, but, on output, 60 seconds can appear. If the number of seconds was to be written with one decimal digit (say) and the output format was F4.1 for that, then if the value was 59·95 or more, it will be rounded up for output, in this example to 60·0. Various systems make this mistake, as also with latitude and longitude, and it is a general problem. A fixup pass is necessary before generating the output: maintain an array with the integer values of the various units, then (for one decimal digit usage) check that the seconds part is less than 59·95. If not, set it to zero and augment the minutes count. If this is 60 or more, set it to zero and augment the hours count, and so on. Thus the array.

```Fortran

      SUBROUTINE PROUST(T)	!Remembrance of time passed.
       INTEGER T		!The time, in seconds. Positive only, please.
       INTEGER NTYPES		!How many types of time?
       PARAMETER (NTYPES = 5)	!This should do.
       INTEGER USIZE(NTYPES)	!Size of the time unit.
       CHARACTER*3 UNAME(NTYPES)!Name of the time unit.
       PARAMETER (USIZE = (/7*24*60*60, 24*60*60, 60*60,   60,    1/))	!The compiler does some arithmetic.
       PARAMETER (UNAME = (/      "wk",      "d",  "hr","min","sec"/))	!Approved names, with trailing spaces.
       CHARACTER*28 TEXT	!A scratchpad.
       INTEGER I,L,N,S		!Assistants.
        S = T			!A copy I can mess with.
        L = 0			!No text has been generated.
        DO I = 1,NTYPES		!Step through the types to do so.
          N = S/USIZE(I)	!Largest first.
          IF (N.GT.0) THEN	!Above the waterline?
            S = S - N*USIZE(I)		!Yes! Remove its contribution.
            IF (L.GT.0) THEN		!Is this the first text to be rolled?
              L = L + 2				!No.
              TEXT(L - 1:L) = ", "		!Cough forth some punctuation.
            END IF			!Now ready for this count.
            WRITE (TEXT(L + 1:),1) N,UNAME(I)	!Place, with the unit name.
    1       FORMAT (I0,1X,A)		!I0 means I only: variable-length, no leading spaces.
            L = LEN_TRIM(TEXT)		!Find the last non-blank resulting.
          END IF			!Since I'm not keeping track.
        END DO			!On to the next unit.
Cast forth the result.
        WRITE (6,*) T,">",TEXT(1:L),"<"	!With annotation.
       END			!Simple enough with integers.

       PROGRAM MARCEL		!Stir the cup.
       CALL PROUST(7259)
       CALL PROUST(7260)
       CALL PROUST(86400)
       CALL PROUST(6000000)
       CALL PROUST(0)
       CALL PROUST(-666)
       END

```

Output:

```txt

        7259 >2 hr, 59 sec<
        7260 >2 hr, 1 min<
       86400 >1 d<
     6000000 >9 wk, 6 d, 10 hr, 40 min<
           0 ><
        -666 ><

```



## FreeBASIC


```freebasic
'FreeBASIC version 1.05 32/64 bit

Sub Show(m As Long)
    Dim As Long c(1 To 5)={604800,86400,3600,60,1}
    Dim As String g(1 To 5)={" Wk"," d"," hr"," min"," sec"},comma
    Dim As Long b(1 To 5),flag,m2=m
    Redim As Long s(0)
    For n As Long=1 To 5
        If m>=c(n) Then
            Do
                Redim Preserve s(Ubound(s)+1)
                s(Ubound(s))=c(n)
                m=m-c(n)
            Loop Until m<c(n)
        End If
    Next n
    For n As Long=1 To Ubound(s)
        For m As Long=1 To 5
            If s(n)=c(m) Then b(m)+=1
        Next m
    Next n
    Print m2;" seconds = ";
    For n As Long=1 To 5
        If b(n) Then: comma=Iif(n<5 Andalso b(n+1),","," and"):flag+=1
        If flag=1 Then comma=""
        Print comma;b(n);g(n);
    End If
Next
Print
End Sub

#define seconds

Show 7259 seconds
Show 86400 seconds
Show 6000000 seconds
sleep
```

Output:

```txt

 7259 seconds =  2 hr and 59 sec
 86400 seconds =  1 d
 6000000 seconds =  9 Wk, 6 d, 10 hr and 40 min

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=d7f00b8a96a6f792f0164f622f0686df Click this link to run this code]'''

```gambas
Public Sub Main()
Dim iInput As Integer[] = [7259, 86400, 6000000]                                  'Input details
Dim iChecks As Integer[] = [604800, 86400, 3600, 60]                              'Weeks, days, hours, mins in seconds
Dim iTime As New Integer[5]                                                       'To store wk, d, hr, min & sec
Dim iOriginal, iSec, iLoop As Integer                                             'Various integers
Dim sOrd As String[] = [" wk", " d", " hr", " min", " sec"]                       'To add to the output string
Dim sOutput As String                                                             'Output string

For Each iSec In iInput                                                           'For each iInput
  iOriginal = iSec                                                                'Store orginal value in seconds
  iTime[4] = iSec                                                                 'Store seconds in iTime[4]

  For iLoop = 0 To 3                                                              'Loop through wk, d, hr, min & sec
    If iTime[4] >= iChecks[iLoop] Then                                            'Check if value is = to wk, d, hr, min
      iTime[iLoop] = Int(iTime[4] / iChecks[iLoop])                               'Put the correct value for wk, d, hr, min in iTime
      iTime[4] = iTime[4] - (iTime[iLoop] * iChecks[iLoop])                       'Remove the amount of seconds for wk, d, hr, min from iTime[4]
    Endif
  Next

  For iLoop = 0 To 4                                                              'Loop through wk, d, hr, min & secs
    If iTime[iLoop] > 0 Then sOutput &= ", " & Str(iTime[iLoop]) & sOrd[iLoop]    'Add comma and ordinal as needed
  Next

  If Left(sOutput, 2) = ", " Then sOutput = Mid(sOutput, 3)                       'Remove unnecessary ", "
  sOutput = Format(Str(iOriginal), "#######") & " Seconds = " & sOutput           'Add original seconds to the output string
  Print sOutput                                                                   'Print sOutput string
  sOutput = ""                                                                    'Clear the sOutput string
  iTime = New Integer[5]                                                          'Reset iTime[]
Next

End
```

Output:

```txt

   7259 Seconds = 2 hr, 59 sec
  86400 Seconds = 1 d
6000000 Seconds = 9 wk, 6 d, 10 hr, 40 min

```



## Go


```go
package main

import "fmt"

func main(){
	fmt.Println(TimeStr(7259))
	fmt.Println(TimeStr(86400))
	fmt.Println(TimeStr(6000000))
}

func TimeStr(sec int)(res string){
	wks, sec := sec / 604800,sec % 604800
	ds, sec := sec / 86400, sec % 86400
	hrs, sec := sec / 3600, sec % 3600
	mins, sec := sec / 60, sec % 60
	CommaRequired := false
	if wks != 0 {
		res += fmt.Sprintf("%d wk",wks)
		CommaRequired = true
	}
	if ds != 0 {
		if CommaRequired {
			res += ", "
		}
		res += fmt.Sprintf("%d d",ds)
		CommaRequired = true
	}
	if hrs != 0 {
		if CommaRequired {
			res += ", "
		}
		res += fmt.Sprintf("%d hr",hrs)
		CommaRequired = true
	}
	if mins != 0 {
		if CommaRequired {
			res += ", "
		}
		res += fmt.Sprintf("%d min",mins)
		CommaRequired = true
	}
	if sec != 0 {
		if CommaRequired {
			res += ", "
		}
		res += fmt.Sprintf("%d sec",sec)
	}
	return
}

```

```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```


## Haskell


```haskell
import Control.Monad (forM_)
import Data.List (intercalate, mapAccumR)
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

reduceBy :: Integral a => a -> [a] -> [a]
n `reduceBy` xs = n' : ys where (n', ys) = mapAccumR quotRem n xs

-- Duration/label pairs.
durLabs :: [(Integer, String)]
durLabs = [(undefined, "wk"), (7, "d"), (24, "hr"), (60, "min"), (60, "sec")]

-- Time broken down into non-zero durations and their labels.
compdurs :: Integer -> [(Integer, String)]
compdurs t = let ds = t `reduceBy` (map fst $ tail durLabs)
             in filter ((/=0) . fst) $ zip ds (map snd durLabs)

-- Compound duration of t seconds.  The argument is assumed to be positive.
compoundDuration :: Integer -> String
compoundDuration = intercalate ", " . map (uncurry $ printf "%d %s") . compdurs

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg -> case readMaybe arg of
    Just n  -> printf "%7d seconds = %s\n" n (compoundDuration n)
    Nothing -> putStrLn $ "Invalid number of seconds: " ++ arg
```


```txt

   7259 seconds = 2 hr, 59 sec
  86400 seconds = 1 d
6000000 seconds = 9 wk, 6 d, 10 hr, 40 min

```



Or, parameterising the local names for these durations:


```haskell
import Data.List (intercalate, mapAccumR)

translation :: Int -> String -> String
translation n local =
  intercalate "  ->  " $ [show, flip timePhrase (words local)] <*> [n]

timePhrase :: Int -> [String] -> String
timePhrase n xs = intercalate ", " (foldr timeTags [] (zip (weekParts n) xs))

timeTags :: (Int, String) -> [String] -> [String]
timeTags (n, s) xs
  | n > 0 = unwords [show n, s] : xs
  | otherwise = xs

weekParts :: Int -> [Int]
weekParts = snd . flip (mapAccumR byUnits) [0, 7, 24, 60, 60]

byUnits :: Int -> Int -> (Int, Int)
byUnits rest x =
  let (u, m)
        | x > 0 = (x, rem rest x)
        | otherwise = (1, rest)
  in (quot (rest - m) u, m)

-- TEST ----------------------------------------------------------------------
main :: IO ()
main =
  mapM_ (putStrLn . flip translation "wk d hr min sec") [7259, 86400, 6000000]
```

```txt
7259  ->  2 hr, 59 sec
86400  ->  1 d
6000000  ->  9 wk, 6 d, 10 hr, 40 min
```



## J


Implementation:


```J
fmtsecs=: verb define
  seq=. 0 7 24 60 60 #: y
  }: ;:inv ,(0 ~: seq) # (8!:0 seq) ,. <;.2'wk,d,hr,min,sec,'
)
```


Task examples:


```J
   fmtsecs 7259
2 hr, 59 sec
   fmtsecs 86400
1 d
   fmtsecs 6000000
9 wk, 6 d, 10 hr, 40 min
```



## Java


```java
public class CompoundDuration {

    public static void main(String[] args) {
        compound(7259);
        compound(86400);
        compound(6000_000);
    }

    private static void compound(long seconds) {
        StringBuilder sb = new StringBuilder();

        seconds = addUnit(sb, seconds, 604800, " wk, ");
        seconds = addUnit(sb, seconds, 86400, " d, ");
        seconds = addUnit(sb, seconds, 3600, " hr, ");
        seconds = addUnit(sb, seconds, 60, " min, ");
        addUnit(sb, seconds, 1, " sec, ");

        sb.setLength(sb.length() > 2 ? sb.length() - 2 : 0);

        System.out.println(sb);
    }

    private static long addUnit(StringBuilder sb, long sec, long unit, String s) {
        long n;
        if ((n = sec / unit) > 0) {
            sb.append(n).append(s);
            sec %= (n * unit);
        }
        return sec;
    }
}
```



```txt
2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min
```




## JavaScript



### Functional



### =ES5=



```JavaScript
(function () {
    'use strict';

    // angloDuration :: Int -> String
    function angloDuration(intSeconds) {
        return zip(
                weekParts(intSeconds),
                ['wk', 'd', 'hr', 'min','sec']
            )
            .reduce(function (a, x) {
                return a.concat(x[0] ? (
                    [(x[0].toString() + ' ' + x[1])]
                ) : []);
            }, [])
            .join(', ');
    }



    // weekParts :: Int -> [Int]
    function weekParts(intSeconds) {

        return [undefined, 7, 24, 60, 60]
            .reduceRight(function (a, x) {
                var intRest = a.remaining,
                    intMod = isNaN(x) ? intRest : intRest % x;

                return {
                    remaining:(intRest - intMod) / (x || 1),
                    parts: [intMod].concat(a.parts)
                };
            }, {
                remaining: intSeconds,
                parts: []
            })
            .parts
    }

    // GENERIC ZIP

    // zip :: [a] -> [b] -> [(a,b)]
    function zip(xs, ys) {
        return xs.length === ys.length ? (
            xs.map(function (x, i) {
                return [x, ys[i]];
            })
        ) : undefined;
    }

    // TEST

    return [7259, 86400, 6000000]
        .map(function (intSeconds) {
            return intSeconds.toString() +
                '    ->    ' + angloDuration(intSeconds);
        })
        .join('\n');

})();

```


```txt
7259     ->    2 hr, 59 sec
86400     ->    1 d
6000000     ->    9 wk, 6 d, 10 hr, 40 min
```




### =ES6=



```JavaScript
((localNames) => {

    // compoundDuration :: [String] -> Int -> String
    const compoundDuration = (labels, intSeconds) =>
        weekParts(intSeconds)
        .map((v, i) => [v, labels[i]])
        .reduce((a, x) =>
            a.concat(x[0] ? [`${x[0]} ${x[1] || '?'}`] : []), []
        )
        .join(', '),

        // weekParts :: Int -> [Int]
        weekParts = intSeconds => [0, 7, 24, 60, 60]
        .reduceRight((a, x) => {
            let r = a.rem,
                mod = x !== 0 ? r % x : r;

            return {
                rem: (r - mod) / (x || 1),
                parts: [mod].concat(a.parts)
            };
        }, {
            rem: intSeconds,
            parts: []
        })
        .parts;


    // TEST
    return [7259, 86400, 6E6]
        .map(intSeconds =>
            `${intSeconds} -> ${compoundDuration(localNames, intSeconds)}`
        )
        .join("\n");

})(["wk", "d", "hr", "min", "sec"]);
```


```txt
7259 -> 2 hr, 59 sec
86400 -> 1 d
6000000 -> 9 wk, 6 d, 10 hr, 40 min
```



## jq

```jq
def seconds_to_time_string:
  def nonzero(text): floor | if . > 0 then "\(.) \(text)" else empty end;
  if . == 0 then "0 sec"
  else
  [(./60/60/24/7    | nonzero("wk")),
   (./60/60/24 % 7  | nonzero("d")),
   (./60/60    % 24 | nonzero("hr")),
   (./60       % 60 | nonzero("min")),
   (.          % 60 | nonzero("sec"))]
  | join(", ")
  end;
```


''Examples''':

```jq
0, 7259, 86400, 6000000 | "\(.): \(seconds_to_time_string)"
```

```sh
$ jq -r -n -f Convert_seconds_to_compound_duration.jq
0: 0 sec
7259: 2 hr, 59 sec
86400: 1 d
6000000: 9 wk, 6 d, 10 hr, 40 min
```



## Julia


```julia
# 1.x
function duration(sec::Integer)::String
    t = Array{Int}([])
    for dm in (60, 60, 24, 7)
        sec, m = divrem(sec, dm)
        pushfirst!(t, m)
    end
    pushfirst!(t, sec)
    return join(["$num$unit" for (num, unit) in zip(t, ["w", "d", "h", "m", "s"]) if num > 0], ", ")
end

@show duration(7259)
@show duration(86400)
@show duration(6000000)

```


```txt

duration(7259) = "2h, 59s"
duration(86400) = "1d"
duration(6000000) = "9w, 6d, 10h, 40m"

```



## Kotlin


```scala
fun compoundDuration(n: Int): String {
    if (n < 0) return "" // task doesn't ask for negative integers to be converted
    if (n == 0) return "0 sec"
    val weeks  : Int
    val days   : Int
    val hours  : Int
    val minutes: Int
    val seconds: Int
    var divisor: Int = 7 * 24 * 60 * 60
    var rem    : Int
    var result = ""

    weeks = n / divisor
    rem   = n % divisor
    divisor /= 7
    days  = rem / divisor
    rem  %= divisor
    divisor /= 24
    hours = rem / divisor
    rem  %= divisor
    divisor /= 60
    minutes = rem / divisor
    seconds = rem % divisor

    if (weeks > 0)   result += "$weeks wk, "
    if (days > 0)    result += "$days d, "
    if (hours > 0)   result += "$hours hr, "
    if (minutes > 0) result += "$minutes min, "
    if (seconds > 0)
        result += "$seconds sec"
    else
        result = result.substring(0, result.length - 2)
    return result
}

fun main(args: Array<String>) {
    val durations = intArrayOf(0, 7, 84, 7259, 86400, 6000000)
    durations.forEach { println("$it\t-> ${compoundDuration(it)}") }
}
```


```txt

0       -> 0 sec
7       -> 7 sec
84      -> 1 min, 24 sec
7259    -> 2 hr, 59 sec
86400   -> 1 d
6000000 -> 9 wk, 6 d, 10 hr, 40 min

```



## Liberty BASIC

I got a bit carried away and added 'years'...

```lb

[start]
input "Enter SECONDS: "; seconds
seconds=int(abs(seconds))
if seconds=0 then print "Program complete.": end
UnitsFound=0: LastFound$=""
years=int(seconds/31449600): seconds=seconds mod 31449600
if years then LastFound$="years"
weeks=int(seconds/604800): seconds=seconds mod 604800
if weeks then LastFound$="weeks"
days=int(seconds/86400): seconds=seconds mod 86400
if days then LastFound$="days"
hours=int(seconds/3600): seconds=seconds mod 3600
if hours then LastFound$="hours"
minutes=int(seconds/60): seconds=seconds mod 60
if minutes then LastFound$="minutes"
if seconds then LastFound$="seconds"
select case years
    case 0
    case 1: print years; " year";
    case else: print years; " years";
end select
select case weeks
    case 0
    case 1
        if years then
            if LastFound$="weeks" then print " and "; else print ", ";
        end if
        print weeks; " week";
    case else
        if years then
            if LastFound$="weeks" then print " and "; else print ", ";
        end if
        print weeks; " weeks";
end select
select case days
    case 0
    case 1
        if years or weeks then
            if LastFound$="days" then print " and "; else print ", ";
        end if
        print days; " day";
    case else
        if years or weeks then
            if LastFound$="days" then print " and "; else print ", ";
        end if
        print days; " days";
end select
select case hours
    case 0
    case 1
        if years or weeks or days then
            if LastFound$="hours" then print " and "; else print ", ";
        end if
        print hours; " hour";
    case else
        if years or weeks or days then
            if LastFound$="hours" then print " and "; else print ", ";
        end if
        print hours; " hours";
end select
select case minutes
    case 0
    case 1
        if years or weeks or days or hours then
            if LastFound$="minutes" then print " and "; else print ", ";
        end if
        print minutes; " minute";
    case else
        if years or weeks or days or hours then
            if LastFound$="minutes" then print " and "; else print ", ";
        end if
        print minutes; " minutes";
end select
select case seconds
    case 0
    case 1
        if years or weeks or days or hours or minutes then
            if LastFound$="seconds" then print " and "; else print ", ";
        end if
        print seconds; " second";
    case else
        if years or weeks or days or hours or minutes then
            if LastFound$="seconds" then print " and "; else print ", ";
        end if
        print seconds; " seconds";
end select
print
goto [start]

```

```txt

Enter SECONDS: 7259
2 hours and 59 seconds
Enter SECONDS: 86400
1 day
Enter SECONDS: 6000000
9 weeks, 6 days, 10 hours and 40 minutes
Enter SECONDS: 987654321
31 years, 21 weeks, 4 hours, 25 minutes and 21 seconds
Enter SECONDS:
Program complete.

```



## Lua


```Lua
function duration (secs)
    local units, dur = {"wk", "d", "hr", "min"}, ""
    for i, v in ipairs({604800, 86400, 3600, 60}) do
        if secs >= v then
            dur = dur .. math.floor(secs / v) .. " " .. units[i] .. ", "
            secs = secs % v
        end
    end
    if secs == 0 then
        return dur:sub(1, -3)
    else
        return dur .. secs .. " sec"
    end
end

print(duration(7259))
print(duration(86400))
print(duration(6000000))
```



## Maple


```Maple

tim := proc (s) local weeks, days, hours, minutes, seconds;
 weeks := trunc((1/604800)*s);
 days := trunc((1/86400)*s)-7*weeks;
 hours := trunc((1/3600)*s)-24*days-168*weeks;
 minutes := trunc((1/60)*s)-60*hours-1440*days-10080*weeks;
 seconds := s-60*minutes-3600*hours-86400*days-604800*weeks;
printf("%s", cat(`if`(0 < weeks, cat(weeks, "wk, "), NULL), `if`(0 < days, cat(days, "d, "), NULL), `if`(0 < hours, cat(hours, "hr, "), NULL), `if`(0 < minutes, cat(minutes, "min, "), NULL), `if`(0 < seconds, cat(seconds, "sec"), NULL)))
end proc;

```



## Mathematica


```Mathematica

compoundDuration[x_Integer] :=
 StringJoin @@ (Riffle[
     ToString /@ ((({Floor[x/604800],
             Mod[x, 604800]} /. {a_, b_} -> {a, Floor[b/86400],
              Mod[b, 86400]}) /. {a__, b_} -> {a, Floor[b/3600],
            Mod[b, 3600]}) /. {a__, b_} -> {a, Floor[b/60],
          Mod[b, 60]}), {" wk, ", " d, ", " hr, ", " min, ",
      " sec"}] //. {a___, "0", b_, c___} -> {a, c})

Grid[Table[{n, "secs =",
   compoundDuration[n]}, {n, {7259, 86400, 6000000}}],
 Alignment -> {Left, Baseline}]


```


```txt

7259	secs =	2 hr, 59 sec
86400	secs =	1 d,
6000000	secs =	9 wk, 6 d, 10 hr, 40 min,

```



## OCaml

```ocaml
let divisors = [
  (max_int, "wk");  (* many wk = many wk *)
  (7, "d");         (* 7 d = 1 wk *)
  (24, "hr");       (* 24 hr = 1 d *)
  (60, "min");      (* 60 min = 1 hr *)
  (60, "sec")       (* 60 sec = 1 min *)
]


(* Convert a number of seconds into a list of values for weeks, days, hours,
 * minutes and seconds, by dividing the number of seconds 'secs' successively by
 * the values contained in the list 'divisors' (taking them in reverse order).
 * Ex:
 *     compute_duration 7259
 * returns
 *     [(0, "wk"); (0, "d"); (2, "hr") (0, "min"); (59, "sec")]
 *)
let compute_duration secs =
  let rec doloop remain res = function
    | [] -> res
    | (n, s) :: ds -> doloop (remain / n) ((remain mod n, s) :: res) ds
  in
  doloop secs [] (List.rev divisors)


(* Format nicely the list of values.
 * Ex:
 *     pretty_print [(0, "wk"); (0, "d"); (2, "hr") (0, "min"); (59, "sec")]
 * returns
 *     "2 hr, 59 sec"
 *
 * Intermediate steps:
 * 1. Keep only the pairs where duration is not 0
 *     [(2, "hr"); (59, "sec")]
 * 2. Format each pair as a string
 *     ["2 hr"; "59 sec"]
 * 3. Concatenate the strings separating them by a comma+space
 *     "2 hr, 59 sec"
 *)
let pretty_print dur =
  List.filter (fun (d, _) -> d <> 0) dur
  |> List.map (fun (d, l) -> Printf.sprintf "%d %s" d l)
  |> String.concat ", "


(* Transform a number of seconds into the corresponding compound duration
 * string.
 * Not sure what to do with 0... *)
let compound = function
  | n when n > 0 -> compute_duration n |> pretty_print
  | n when n = 0 -> string_of_int 0 ^ "..."
  | _ -> invalid_arg "Number of seconds must be positive"


(* Some testing... *)
let () =
  let test_cases = [
    (7259, "2 hr, 59 sec");
    (86400, "1 d");
    (6000000, "9 wk, 6 d, 10 hr, 40 min");
    (0, "0...");
    (3599, "59 min, 59 sec");
    (3600, "1 hr");
    (3601, "1 hr, 1 sec")
  ] in
  let testit (n, s) =
    let calc = compound n in
    Printf.printf "[%s] %d seconds -> %s; expected: %s\n"
                  (if calc = s then "PASS" else "FAIL")
                  n calc s
  in
  List.iter testit test_cases
```

 [PASS] 7259 seconds -> 2 hr, 59 sec; expected: 2 hr, 59 sec
 [PASS] 86400 seconds -> 1 d; expected: 1 d
 [PASS] 6000000 seconds -> 9 wk, 6 d, 10 hr, 40 min; expected: 9 wk, 6 d, 10 hr, 40 min
 [PASS] 0 seconds -> 0...; expected: 0...
 [PASS] 3599 seconds -> 59 min, 59 sec; expected: 59 min, 59 sec
 [PASS] 3600 seconds -> 1 hr; expected: 1 hr
 [PASS] 3601 seconds -> 1 hr, 1 sec; expected: 1 hr, 1 sec


## PARI/GP

Note: my own string function ssubstr() was used. You can find it here on RosettaCode Wiki.

```parigp

\\ Convert seconds to compound duration
\\ 4/11/16 aev
secs2compdur(secs)={
my(us=[604800,86400,3600,60,1],ut=[" wk, "," d, "," hr, "," min, "," sec"],
   cd=[0,0,0,0,0],u,cdt="");
for(i=1,5, u=secs\us[i]; if(u==0, next, cd[i]=u; secs-=us[i]*cd[i]));
for(i=1,5, if(cd[i]==0, next, cdt=Str(cdt,cd[i],ut[i])));
if(ssubstr(cdt,#cdt-1,1)==",", cdt=ssubstr(cdt,1,#cdt-2));
return(cdt);
}

{\\ Required tests:
print(secs2compdur(7259));
print(secs2compdur(86400));
print(secs2compdur(6000000));
}

```


```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```



## Perl



```perl
sub compound_duration {
    my $sec = shift;
    no warnings 'numeric';

    return join ', ', grep { $_ > 0 }
        int($sec/60/60/24/7)    . " wk",
        int($sec/60/60/24) % 7  . " d",
        int($sec/60/60)    % 24 . " hr",
        int($sec/60)       % 60 . " min",
        int($sec)          % 60 . " sec";
}
```


Demonstration:

```perl
for (7259, 86400, 6000000) {
    printf "%7d sec  =  %s\n", $_, compound_duration($_)
}
```


```txt

   7259 sec  =  2 hr, 59 sec
  86400 sec  =  1 d
6000000 sec  =  9 wk, 6 d, 10 hr, 40 min

```



## Perl 6


The built-in <code>polymod</code> method (which is a generalization of the <code>divmod</code> function known from other languages), is a perfect match for a task like this:


```perl6
sub compound-duration ($seconds) {
    ($seconds.polymod(60, 60, 24, 7) Z <sec min hr d wk>)
    .grep(*[0]).reverse.join(", ")
}

# Demonstration:

for 7259, 86400, 6000000 {
    say "{.fmt: '%7d'} sec  =  {compound-duration $_}";
}
```


```txt

   7259 sec  =  2 hr, 59 sec
  86400 sec  =  1 d
6000000 sec  =  9 wk, 6 d, 10 hr, 40 min

```



## Phix

The existing standard function for this gives the results shown

```Phix
?elapsed(7259)
?elapsed(86400)
?elapsed(6000000)
```

```txt

"2 hours and 59s"
"1 day, 0 hours"
"69 days, 10 hours, 40 minutes"

```

The routine is implemented in builtins\pelapsed.e which you can modify as desired, and is reproduced for reference below

```Phix
function elapzd(integer v, string d)
-- private helper function. formats v and pluralises d by adding an "s", or not.
    return sprintf("%d %s%s",{v,d,iff(v=1?"":"s")})
end function

global function elapsed(atom s)
-- convert s (in seconds) into an elapsed time string suitable for display.
-- limits: a type check error occurs if s exceeds approx 100 billion years.
atom m,h,d,y
string minus = "", secs, mins
    if s<0 then
        minus = "minus "
        s = 0-s
    end if
    m = floor(s/60)
    s = remainder(s,60)
    secs = sprintf(iff(integer(s)?"%ds":"%3.2fs"),s)
    if m=0 then
        return sprintf("%s%s",{minus,secs})
    end if
    s = round(s)
    secs = iff(s=0?"":sprintf(" and %02ds",s))
    h = floor(m/60)
    m = remainder(m,60)
    if h=0 then
        return sprintf("%s%s%s",{minus,elapzd(m,"minute"),secs})
    end if
    mins = iff(m=0?"":", "&elapzd(m,"minute"))
    if h<24 then
        return sprintf("%s%s%s%s",{minus,elapzd(h,"hour"),mins,secs})
    end if
    d = floor(h/24)
    h = remainder(h,24)
    if d<365 then
        return sprintf("%s%s, %s%s%s",{minus,elapzd(d,"day"),elapzd(h,"hour"),mins,secs})
    end if
    y = floor(d/365)
    d = remainder(d,365)
    return sprintf("%s%s, %s, %s%s%s",{minus,elapzd(y,"year"),elapzd(d,"day"),elapzd(h,"hour"),mins,secs})
end function
```



## PicoLisp


```PicoLisp
(for Sec (7259 86400 6000000)
   (tab (-10 -30)
      Sec
      (glue ", "
         (extract
            '((N Str)
               (when (gt0 (/ Sec N))
                  (setq Sec (% Sec N))
                  (pack @ " " Str) ) )
            (604800 86400 3600 60 1)
            '("wk" "d" "hr" "min" "sec") ) ) ) )
```

Output:

```txt
7259      2 hr, 59 sec
86400     1 d
6000000   9 wk, 6 d, 10 hr, 40 min
```



## PL/I


```PL/I

/* Convert seconds to Compound Duration (weeks, days, hours, minutes, seconds). */

cvt: procedure options (main);			/* 5 August 2015 */
   declare interval float (15);
   declare (unit, i, q controlled) fixed binary;
   declare done bit (1) static initial ('0'b);
   declare name (5) character (4) varying static initial (' wk', ' d', ' hr', ' min', ' sec' );

   get (interval);
   put edit (interval, ' seconds = ') (f(10), a);
   if interval = 0 then do; put skip list ('0 sec'); stop; end;

   do unit = 60, 60, 24, 7;
      allocate q;
      q = mod(interval, unit);
      interval = interval / unit;
   end;
   allocate q; q = interval;
   do i = 1 to 5;
      if q > 0 then
         do;
            if done then put edit (', ') (a);
            put edit (trim(q), name(i)) (a, a); done = '1'b;
         end;
      if i < 5 then free q;
   end;
end cvt;

```

Results:

```txt

        65 seconds = 1 min, 5 sec
      3750 seconds = 1 hr, 2 min, 30 sec
   1483506 seconds = 2 wk, 3 d, 4 hr, 5 min, 6 sec
        60 seconds = 1 min
      3604 seconds = 1 hr, 4 sec
 100000000 seconds = 165 wk, 2 d, 9 hr, 46 min, 40 sec
 987654321 seconds = 1633 wk, 4 hr, 25 min, 21 sec
     86400 seconds = 1 d
     86403 seconds = 1 d, 3 sec
3 more to come.

```



## PowerShell


```PowerShell

function Get-Time
{
  <#
    .SYNOPSIS
        Gets a time string in the form: # wk, # d, # hr, # min, # sec
    .DESCRIPTION
        Gets a time string in the form: # wk, # d, # hr, # min, # sec
        (Values of 0 are not displayed in the string.)

        Days, Hours, Minutes or Seconds in any combination may be used
        as well as Start and End dates.

        When used with the -AsObject switch an object containing properties
        similar to a System.TimeSpan object is returned.
    .INPUTS
        DateTime or Int32
    .OUTPUTS
        String or PSCustomObject
    .EXAMPLE
        Get-Time -Seconds 7259
    .EXAMPLE
        Get-Time -Days 31 -Hours 4 -Minutes 8 -Seconds 16
    .EXAMPLE
        Get-Time -Days 31 -Hours 4 -Minutes 8 -Seconds 16 -AsObject
    .EXAMPLE
        Get-Time -Start 3/10/2016 -End 1/20/2017
    .EXAMPLE
        Get-Time -Start (Get-Date) -End (Get-Date).AddSeconds(6000000)
  #>
    [CmdletBinding(DefaultParameterSetName='Date')]
    Param
    (
        # Start date
        [Parameter(Mandatory=$false, ParameterSetName='Date',
                   Position=0)]
        [datetime]
        $Start = (Get-Date),

        # End date
        [Parameter(Mandatory=$false, ParameterSetName='Date',
                   Position=1)]
        [datetime]
        $End = (Get-Date),

        # Days in the time span
        [Parameter(Mandatory=$false, ParameterSetName='Time')]
        [int]
        $Days = 0,

        # Hours in the time span
        [Parameter(Mandatory=$false, ParameterSetName='Time')]
        [int]
        $Hours = 0,

        # Minutes in the time span
        [Parameter(Mandatory=$false, ParameterSetName='Time')]
        [int]
        $Minutes = 0,

        # Seconds in the time span
        [Parameter(Mandatory=$false, ParameterSetName='Time')]
        [int]
        $Seconds = 0,

        [switch]
        $AsObject
    )

    Begin
    {
        [PSCustomObject]$timeObject = "PSCustomObject" |
            Select-Object -Property Weeks,RemainingDays,
                                    Days,Hours,Minutes,Seconds,Milliseconds,Ticks,
                                    TotalDays,TotalHours,TotalMinutes,TotalSeconds,TotalMilliseconds
        [int]$remainingDays  = 0
        [int]$weeks          = 0

        [string[]]$timeString = @()
    }
    Process
    {
        switch ($PSCmdlet.ParameterSetName)
        {
            'Date' { $timeSpan = New-TimeSpan -Start $Start -End $End }
            'Time' { $timeSpan = New-TimeSpan -Days $Days -Hours $Hours -Minutes $Minutes -Seconds $Seconds }
        }

        $weeks = [System.Math]::DivRem($timeSpan.Days, 7, [ref]$remainingDays)

        $timeObject.Weeks             = $weeks
        $timeObject.RemainingDays     = $remainingDays
        $timeObject.Days              = $timeSpan.Days
        $timeObject.Hours             = $timeSpan.Hours
        $timeObject.Minutes           = $timeSpan.Minutes
        $timeObject.Seconds           = $timeSpan.Seconds
        $timeObject.Milliseconds      = $timeSpan.Milliseconds
        $timeObject.Ticks             = $timeSpan.Ticks
        $timeObject.TotalDays         = $timeSpan.TotalDays
        $timeObject.TotalHours        = $timeSpan.TotalHours
        $timeObject.TotalMinutes      = $timeSpan.TotalMinutes
        $timeObject.TotalSeconds      = $timeSpan.TotalSeconds
        $timeObject.TotalMilliseconds = $timeSpan.TotalMilliseconds
    }
    End
    {
        if ($AsObject) { return $timeObject }

        if ($timeObject.Weeks)         { $timeString += "$($timeObject.Weeks) wk"        }
        if ($timeObject.RemainingDays) { $timeString += "$($timeObject.RemainingDays) d" }
        if ($timeObject.Hours)         { $timeString += "$($timeObject.Hours) hr"        }
        if ($timeObject.Minutes)       { $timeString += "$($timeObject.Minutes) min"     }
        if ($timeObject.Seconds)       { $timeString += "$($timeObject.Seconds) sec"     }

        return ($timeString -join ", ")
    }
}

```


```txt

PS C:\Scripts> 7259, 86400, 6000000 | ForEach-Object { Get-Time -Seconds $_ }
2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min


PS C:\Scripts> Get-Time -Start 3/10/2016 -End 1/20/2017
45 wk, 1 d


PS C:\Scripts> Get-Time -Seconds 6000000 -AsObject


Weeks             : 9
RemainingDays     : 6
Days              : 69
Hours             : 10
Minutes           : 40
Seconds           : 0
Milliseconds      : 0
Ticks             : 60000000000000
TotalDays         : 69.4444444444444
TotalHours        : 1666.66666666667
TotalMinutes      : 100000
TotalSeconds      : 6000000
TotalMilliseconds : 6000000000

```



## Prolog

Works with Swi-Prolog.
Use Contraints for this task so the that functionality can be reversed (eg: find the number of seconds for a split date).

See below for examples.


```prolog
:- use_module(library(clpfd)).

% helper to perform the operation with just a number.
compound_time(N) :-
    times(N, R),
    format('~p: ', N),
    write_times(R).

% write out the results in the 'special' format.
write_times([[Tt, Val]|T]) :-
    dif(T, []),
    format('~w ~w, ', [Val, Tt]),
    write_times(T).
write_times([[Tt, Val]|[]]) :-
    format('~w ~w~n', [Val, Tt]).


% this predicate is the main predicate, it takes either N
% or a list of split values to get N, or both.
times(N, R) :-
    findall(T, time_type(T,_), TTs),
    times(TTs, N, R).

% do the split, if there is a 1 or greater add to a list of results.
times([], _, []).
times([Tt|T], N, Rest) :-
    time_type(Tt, Div),
    Val #= N // Div,
    Val #< 1,
    times(T, N, Rest).
times([Tt|T], N, [[Tt,Val]|Rest]) :-
    time_type(Tt, Div),
    Val #= N // Div,
    Val #>= 1,
    Rem #= N mod Div,
    times(T, Rem, Rest).


% specifify the different time split types
time_type(wk, 60 * 60 * 24 * 7).
time_type(d, 60 * 60 * 24).
time_type(hr, 60 * 60).
time_type(min, 60).
time_type(sec, 1).
```


```txt

?- maplist(compound_time, [7259,86400,6000000]), !.
7259: 2 hr, 59 sec
86400: 1 d
6000000: 9 wk, 6 d, 10 hr, 40 min
true.

?- times(N, [[wk, 9],[d, 6],[hr,10],[min,40]]), !.
N = 6000000.

```



## PureBasic


```PureBasic

EnableExplicit

Procedure.s ConvertSeconds(NbSeconds)
  Protected weeks, days, hours, minutes, seconds
  Protected divisor, remainder
  Protected duration$ = ""
  divisor = 7 * 24 * 60 * 60 ; seconds in a week
  weeks = NbSeconds / divisor
  remainder = NbSeconds % divisor
  divisor / 7 ; seconds in a day
  days = remainder / divisor
  remainder % divisor
  divisor / 24 ; seconds in an hour
  hours = remainder / divisor
  remainder % divisor
  divisor / 60 ; seconds in a minute
  minutes = remainder / divisor
  seconds = remainder % divisor

  If weeks > 0
    duration$ + Str(weeks) + " wk, "
  EndIf

  If days > 0
    duration$ + Str(days) + " d, "
  EndIf

  If hours > 0
    duration$ + Str(hours) + " hr, "
  EndIf

  If minutes > 0
    duration$ + Str(minutes) + " min, "
  EndIf

  If seconds > 0
    duration$ + Str(seconds) + " sec"
  EndIf

  If Right(duration$, 2) = ", "
    duration$ = Mid(duration$, 0, Len(duration$) - 2)
  EndIf

  ProcedureReturn duration$
EndProcedure

If OpenConsole()
  PrintN(ConvertSeconds(7259))
  PrintN(ConvertSeconds(86400))
  PrintN(ConvertSeconds(6000000))
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```



## Python



### Python: Procedural


```python>>>
 def duration(seconds):
	t= []
	for dm in (60, 60, 24, 7):
		seconds, m = divmod(seconds, dm)
		t.append(m)
	t.append(seconds)
	return ', '.join('%d %s' % (num, unit)
			 for num, unit in zip(t[::-1], 'wk d hr min sec'.split())
			 if num)

>>> for seconds in [7259, 86400, 6000000]:
	print("%7d sec = %s" % (seconds, duration(seconds)))


   7259 sec = 2 hr, 59 sec
  86400 sec = 1 d
6000000 sec = 9 wk, 6 d, 10 hr, 40 min
>>>
```



### Python: Functional


```python>>>
 def duration(seconds, _maxweeks=99999999999):
    return ', '.join('%d %s' % (num, unit)
		     for num, unit in zip([(seconds // d) % m
					   for d, m in ((604800, _maxweeks),
                                                        (86400, 7), (3600, 24),
                                                        (60, 60), (1, 60))],
					  ['wk', 'd', 'hr', 'min', 'sec'])
		     if num)

>>> for seconds in [7259, 86400, 6000000]:
	print("%7d sec = %s" % (seconds, duration(seconds)))


   7259 sec = 2 hr, 59 sec
  86400 sec = 1 d
6000000 sec = 9 wk, 6 d, 10 hr, 40 min
>>>
```


Or, composing a solution from pure curried functions, including the '''mapAccumL''' abstraction (a combination of of '''map''' and '''reduce''', implemented in a variety of languages and functional libraries, in which a new list is derived by repeated application of the same function, as an accumulator (here, a remainder) passes from left to right):


```python
from functools import (reduce)
from itertools import (chain)


# main :: IO ()
def main():

    # angloDurations :: Int -> String
    angloDurations = compose(commaSpaced)(
        durationParts(['wk', 'd', 'hr', 'min', 'sec'])
    )

    for n in [7259, 86400, 6000000]:
        print (
            angloDurations(n)
        )


# durationParts :: [String] -> Int -> [(String, Int)]
def durationParts(ks):
    'Duration names -> Number of seconds -> (duration, name) pairs'

    # go :: (Int, (Int, String)) -> (Int, [(Int, String)])
    def go(a, nk):
        '''(Remainder, (Seconds, Name)) -> (New remainder, [(Units, Name)])'''
        q, r = divmod(a, nk[0]) if 0 < a else (0, 0)

        # A tuple of the remainder
        # and a possibly empty list,
        # containing any (value, name) pair for this unit.
        return (r, [(q, nk[1])] if 0 < q else [])

    return lambda n: (
        # concat eliminates any empty lists
        concat(
            # The second part of the tuple only
            # (we no longer need the remainder accumulator)
            mapAccumL(go)(n)(
                zip([604800, 86400, 3600, 60, 1], ks)
            )[1]
        )
    )


# commaSpaced :: [(Int, String)] -> String
def commaSpaced(xs):
    return ', '.join([str(n) + ' ' + k for n, k in xs])


# GENERIC ABSTRACTIONS ------------------------------------


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    return lambda f: lambda x: g(f(x))


# concat :: [[a]] -> [a]
def concat(xs):
    '''This turns out to be the fastest concat in the standard libraries,'''
    '''itertools-independent alternatives can also be written.'''
    return list(chain.from_iterable(xs))


# mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
def mapAccumL(f):
    def go(a, x):
        tpl = f(a[0], x)
        return (tpl[0], a[1] + [tpl[1]])
    return lambda acc: lambda xs: (
        reduce(go, xs, (acc, []))
    )


if __name__ == '__main__':
    main()
```

```txt
2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min
```



## Racket



```racket
#lang racket/base
(require racket/string
         racket/list)

(define (seconds->compound-durations s)
  (define-values (w d.h.m.s)
    (for/fold ((prev-q s) (rs (list))) ((Q (in-list (list 60 60 24 7))))
      (define-values (q r) (quotient/remainder prev-q Q))
      (values q (cons r rs))))
  (cons w d.h.m.s))

(define (maybe-suffix v n)
  (and (positive? v)
       (format "~a ~a" v n)))

(define (seconds->compound-duration-string s)
  (string-join (filter-map maybe-suffix
                           (seconds->compound-durations s)
                           '("wk" "d" "hr" "min" "sec"))
               ", "))

(module+ test
  (require rackunit)
  (check-equal? (seconds->compound-durations 7259)    (list 0 0  2  0 59))
  (check-equal? (seconds->compound-durations 86400)   (list 0 1  0  0  0))
  (check-equal? (seconds->compound-durations 6000000) (list 9 6 10 40  0))

  (check-equal? (seconds->compound-duration-string 7259)    "2 hr, 59 sec")
  (check-equal? (seconds->compound-duration-string 86400)   "1 d")
  (check-equal? (seconds->compound-duration-string 6000000) "9 wk, 6 d, 10 hr, 40 min"))

;; Tim Brown 2015-07-21
```


All tests pass... there is no output.


## REXX


### version 1


```rexx
/* REXX ---------------------------------------------------------------
* Format seconds into a time string
*--------------------------------------------------------------------*/
Call test 7259    ,'2 hr, 59 sec'
Call test 86400   ,'1 d'
Call test 6000000 ,'9 wk, 6 d, 10 hr, 40 min'
Call test 123.50  ,'2 min, 3.5 sec'
Call test 123.00  ,'2 min, 3 sec'
Call test 0.00    ,'0 sec'
Exit

test:
  Parse arg secs,xres
  res=sec2ct(secs)
  Say res
  If res<>xres Then Say '**ERROR**'
  Return

sec2ct:
Parse Arg s
/*
m=s%60; s=s//60
h=m%60; m=m//60
d=h%24; h=h//24
w=d%7;  d=d//7
*/
If s=0 Then Return '0 sec'
Parse Value split(s,60) with m s
Parse Value split(m,60) with h m
Parse Value split(h,24) with d h
Parse Value split(d, 7) with w d
ol=''
If w>0 Then ol=ol w 'wk,'
If d>0 Then ol=ol d 'd,'
If h>0 Then ol=ol h 'hr,'
If m>0 Then ol=ol m 'min,'
If s>0 Then ol=ol (s/1) 'sec'
ol=strip(ol)
ol=strip(ol,,',')
Return ol

split: Procedure
  Parse Arg what,how
  a=what%how
  b=what//how
  Return a b
```

```txt
2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min
2 min, 3.5 sec
2 min, 3 sec
0 sec
```



### version 2

This REXX version can also handle fractional (seconds) as well as values of zero (time units).

```rexx
/*rexx program  demonstrates  how to convert a number of seconds  to  bigger time units.*/
parse arg @;  if @=''  then @=7259 86400 6000000 /*Not specified?  Then use the default.*/

       do j=1  for words(@);       z= word(@, j) /* [↓]  process each number in the list*/
       say right(z, 25) 'seconds:  '  convSec(z) /*convert a number to bigger time units*/
       end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
convSec: parse arg x                             /*obtain a number from the argument.   */
         w= timeU( 60*60*24*7,  'wk'  )          /*obtain number of weeks     (if any). */
         d= timeU( 60*60*24  ,  'd'   )          /*   "      "    " days        "  "    */
         h= timeU( 60*60     ,  'hr'  )          /*   "      "    " hours       "  "    */
         m= timeU( 60        ,  'min' )          /*   "      "    " minutes     "  "    */
         s= timeU( 1         ,  'sec' )          /*   "      "    " seconds     "  "    */
         if x\==0  then s=word(s 0, 1) + x 'sec' /*handle fractional (residual) seconds.*/
         $=strip(space(w d h m s),,",");      if $==''  then z= 0 "sec"  /*handle 0 sec.*/
         return  $
/*──────────────────────────────────────────────────────────────────────────────────────*/
timeU:   parse arg u,$;   _= x%u;   if _==0  then return '';   x= x - _*u;   return _ $","
```

```txt

                     7259 seconds:   2 hr, 59 sec
                    86400 seconds:   1 d
                  6000000 seconds:   9 wk, 6 d, 10 hr, 40 min

```

```txt

                   1800.7 seconds:   30 min, 0.7 sec
                   123.50 seconds:   2 min, 3.50 sec
                   123.00 seconds:   2 min, 3 sec
                     0.00 seconds:   0 sec

```



## Ring


```ring

sec = 6000005
week = floor(sec/60/60/24/7)
if week > 0 see sec
   see " seconds is " + week + " weeks " ok
day = floor(sec/60/60/24) % 7
if day > 0 see day
   see " days " ok
hour = floor(sec/60/60) % 24
if hour > 0 see hour
   see " hours " ok
minute = floor(sec/60) % 60
if minute > 0 see minute
   see " minutes " ok
second = sec % 60
if second > 0 see second
   see " seconds" + nl ok

```



## Ruby


```ruby
MINUTE = 60
HOUR   = MINUTE*60
DAY    = HOUR*24
WEEK   = DAY*7

def sec_to_str(sec)
  w, rem = sec.divmod(WEEK)
  d, rem = rem.divmod(DAY)
  h, rem = rem.divmod(HOUR)
  m, s   = rem.divmod(MINUTE)
  units  = ["#{w} wk", "#{d} d", "#{h} h", "#{m} min", "#{s} sec"]
  units.reject{|str| str.start_with?("0")}.join(", ")
end

[7259, 86400, 6000000].each{|t| puts "#{t}\t: #{sec_to_str(t)}"}
```

Output:

```txt

7259	: 2 h, 59 sec
86400	: 1 d
6000000	: 9 wk, 6 d, 10 h, 40 min

```



## Run BASIC


```runbasic
sec = 6000005
week	= int(sec/60/60/24/7)
day	= int(sec/60/60/24) mod 7
hour	= int(sec/60/60) mod 24
minute	= int(sec/60) mod 60
second	= sec mod 60

print sec;" seconds is ";
if week > 0 then print week;" weeks ";
if day > 0 then print day;" days ";
if hour > 0 then print hour;" hours ";
if minute > 0 then print minute;" minutes ";
if second > 0 then print second;" seconds"
```



## Rust

This solution deviates from the prompt a bit in order to make it more general. The benefit of doing it this way is that any values can be filled in for days, hours, minutes and seconds and the `balance` method will do the balancing accordingly. Also, rather than converting the value into a String, it simply implements the `Display` trait.

```rust
use std::fmt;


struct CompoundTime {
    w: usize,
    d: usize,
    h: usize,
    m: usize,
    s: usize,
}

macro_rules! reduce {
    ($s: ident, $(($from: ident, $to: ident, $factor: expr)),+) => {{
        $(
            $s.$to += $s.$from / $factor;
            $s.$from %= $factor;
        )+
    }}
}

impl CompoundTime {
    #[inline]
    fn new(w: usize, d: usize, h: usize, m: usize, s: usize) -> Self{
        CompoundTime { w: w, d: d, h: h, m: m, s: s, }
    }

    #[inline]
    fn balance(&mut self) {
        reduce!(self, (s, m, 60), (m, h, 60),
                      (h, d, 24), (d, w, 7));
    }
}

impl fmt::Display for CompoundTime {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}w {}d {}h {}m {}s",
               self.w, self.d, self.h, self.m, self.s)
    }
}

fn main() {
    let mut ct = CompoundTime::new(0,3,182,345,2412);
    println!("Before: {}", ct);
    ct.balance();
    println!("After: {}", ct);
}
```



## Scala


```scala
//Converting Seconds to Compound Duration

object seconds{
	def main( args:Array[String] ){

		println("Enter the no.")
		val input = scala.io.StdIn.readInt()

		var week_r:Int = input % 604800
		var week:Int = (input - week_r)/604800

		var day_r:Int = week_r % 86400
		var day:Int = (week_r - day_r)/86400

		var hour_r:Int = day_r % 3600
		var hour:Int = (day_r - hour_r)/3600

		var minute_r:Int = hour_r % 60
		var minute:Int = (hour_r - minute_r)/60

		var sec:Int = minute_r % 60

		println("Week = " + week)
		println("Day = " + day)
		println("Hour = " + hour)
		println("Minute = " + minute)
		println("Second = " + sec)
	}
}
```



## Scheme


This version uses delete from SRFI 1 and string-join from SRFI 13:


```scheme

(import (scheme base)
        (scheme write)
        (srfi 1)
        (only (srfi 13) string-join))

(define *seconds-in-minute* 60)
(define *seconds-in-hour* (* 60 *seconds-in-minute*))
(define *seconds-in-day* (* 24 *seconds-in-hour*))
(define *seconds-in-week* (* 7 *seconds-in-day*))

(define (seconds->duration seconds)
  (define (format val unit)
    (if (zero? val) "" (string-append (number->string val) " " unit)))
  (let*-values (((weeks wk-remainder) (floor/ seconds *seconds-in-week*))
                ((days dy-remainder) (floor/ wk-remainder *seconds-in-day*))
                ((hours hr-remainder) (floor/ dy-remainder *seconds-in-hour*))
                ((minutes mn-remainder) (floor/ hr-remainder *seconds-in-minute*)))
               (string-join (delete ""
                                    (list (format weeks "wk")
                                          (format days "d")
                                          (format hours "hr")
                                          (format minutes "min")
                                          (format mn-remainder "sec"))
                                    string=?) ", ")))

(display (seconds->duration 7259)) (newline)
(display (seconds->duration 86400)) (newline)
(display (seconds->duration 6000000)) (newline)

```


```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```



## Sidef

```ruby
func polymod(n, *divs) {
    gather {
        divs.each { |i|
            var m = take(n % i)
            (n -= m) /= i
        }
        take(n)
    }
}

func compound_duration(seconds) {
    (polymod(seconds, 60, 60, 24, 7) ~Z <sec min hr d wk>).grep { |a|
        a[0] > 0
    }.reverse.map{.join(' ')}.join(', ')
}

[7259, 86400, 6000000].each { |s|
    say "#{'%7d' % s} sec  =  #{compound_duration(s)}"
}
```


```txt

   7259 sec  =  2 hr, 59 sec
  86400 sec  =  1 d
6000000 sec  =  9 wk, 6 d, 10 hr, 40 min

```



## Tcl


The data-driven procedure below can be customised to use different breakpoints, simply by editing the dictionary.


```Tcl
proc sec2str {i} {
    set factors {
        sec 60
        min 60
        hr  24
        d   7
        wk  Inf
    }
    set result ""
    foreach {label max} $factors {
        if {$i >= $max} {
            set r [expr {$i % $max}]
            set i [expr {$i / $max}]
            if {$r} {
                lappend result "$r $label"
            }
        } else {
            if {$i > 0} {
                lappend result "$i $label"
            }
            break
        }
    }
    join [lreverse $result] ", "
}

proc check {cmd res} {
    set r [uplevel 1 $cmd]
    if {$r eq $res} {
        puts "Ok! $cmd \t = $res"
    } else {
        puts "ERROR: $cmd = $r \t expected $res"
    }
}

check {sec2str 7259}    {2 hr, 59 sec}
check {sec2str 86400}   {1 d}
check {sec2str 6000000} {9 wk, 6 d, 10 hr, 40 min}
```


```txt
Ok! sec2str 7259         = 2 hr, 59 sec
Ok! sec2str 86400        = 1 d
Ok! sec2str 6000000      = 9 wk, 6 d, 10 hr, 40 min
```



## uBasic/4tH

Since uBasic/4tH is integer-only, it is hard to return a string. However, it is capable to transform an integer value as required.
<lang>Proc _CompoundDuration(7259)
Proc _CompoundDuration(86400)
Proc _CompoundDuration(6000000)

End


_CompoundDuration Param(1)             ' Print compound seconds
  a@ = FUNC(_Compound(a@, 604800, _wk))
  a@ = FUNC(_Compound(a@, 86400, _d))
  a@ = FUNC(_Compound(a@, 3600, _hr))
  a@ = FUNC(_Compound(a@, 60, _min))

  If a@ > 0 Then Print a@;" sec";      ' Still seconds left to print?
  Print
Return


_Compound Param(3)
  Local(1)

  d@ = a@/b@                           ' Get main component
  a@ = a@%b@                           ' Leave the rest

  If d@ > 0 Then                       ' Print the component
    Print d@;
    Proc c@

    If a@ > 0 Then
      Print ", ";                      ' If something follows, take
    EndIf                              ' care of the comma
  EndIf
Return (a@)


_wk  Print " wk";  : Return
_d   Print " d";   : Return
_hr  Print " hr";  : Return
_min Print " min"; : Return
```

```txt
2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

0 OK, 0:94
```



## VBA


```vb
Private Function compound_duration(ByVal seconds As Long) As String
    minutes = 60
    hours = 60 * minutes
    days_ = 24 * hours
    weeks = 7 * days_
    Dim out As String
    w = seconds \ weeks
    seconds = seconds - w * weeks
    d = seconds \ days_
    seconds = seconds - d * days_
    h = seconds \ hours
    seconds = seconds - h * hours
    m = seconds \ minutes
    s = seconds Mod minutes
    out = IIf(w > 0, w & " wk, ", "") & _
        IIf(d > 0, d & " d, ", "") & _
        IIf(h > 0, h & " hr, ", "") & _
        IIf(m > 0, m & " min, ", "") & _
        IIf(s > 0, s & " sec", "")
    If Right(out, 2) = ", " Then
        compound_duration = Left(out, Len(out) - 2)
    Else
        compound_duration = out
    End If
End Function

Public Sub cstcd()
    Debug.Print compound_duration(7259)
    Debug.Print compound_duration(86400)
    Debug.Print compound_duration(6000000)
End Sub
```
```txt
2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min
```


## VBScript


```vb

Function compound_duration(n)
	Do Until n = 0
		If n >= 604800 Then
			wk = Int(n/604800)
			n = n-(604800*wk)
			compound_duration = compound_duration & wk & " wk"
		End If
		If n >= 86400 Then
			d = Int(n/86400)
			n = n-(86400*d)
			If wk > 0 Then compound_duration = compound_duration & ", " End If
			compound_duration = compound_duration & d & " d"
		End If
		If n >= 3600 Then
			hr = Int(n/3600)
			n = n-(3600*hr)
			If d > 0 Then compound_duration = compound_duration & ", " End If
			compound_duration = compound_duration & hr & " hr"
		End If
		If n >= 60 Then
			min = Int(n/60)
			n = n-(60*min)
			If hr > 0 Then compound_duration = compound_duration & ", " End If
			compound_duration = compound_duration & min & " min"
		End If
		If n > 0 Then
			If min > 0 Then compound_duration = compound_duration & ", " End If
			compound_duration = compound_duration & ", " & n & " sec"
			n = 0
		End If
	Loop
End Function

'validating the function
WScript.StdOut.WriteLine compound_duration(7259)
WScript.StdOut.WriteLine compound_duration(86400)
WScript.StdOut.WriteLine compound_duration(6000000)

```


```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```



## zkl


```zkl
fcn toWDHMS(sec){  //-->(wk,d,h,m,s)
   r,b:=List(),0;
   foreach u in (T(60,60,24,7)){
      sec,b=sec.divr(u);   // aka divmod
      r.append(b);
   }
   r.append(sec).reverse()
}
```

Or, if you like to be concise:

```zkl
fcn toWDHMS(sec){  //-->(wk,d,h,m,s)
   T(60,60,24,7).reduce(fcn(n,u,r){ n,u=n.divr(u); r.append(u); n },
      sec,r:=List()):r.append(_).reverse();
}
```

were the ":" op takes the left result and stuffs it into the "_" position.

```zkl
units:=T(" wk"," d"," hr"," min"," sec");
foreach s in (T(7259,86400,6000000)){
   toWDHMS(s).zip(units).pump(List,fcn([(t,u)]){ t and String(t,u) or "" })
   .filter().concat(", ").println();
}
```

```txt

2 hr, 59 sec
1 d
9 wk, 6 d, 10 hr, 40 min

```



## ZX Spectrum Basic

```zxbasic
10 LET m=60: LET h=60*m: LET d=h*24: LET w=d*7
20 DATA 10,7259,86400,6000000,0,1,60,3600,604799,604800,694861
30 READ n
40 FOR i=1 TO n
50 READ s
60 LET nweek=0: LET nday=0: LET nhour=0: LET nmin=0: LET nsec=0: LET s$=""
70 PRINT s;" = ";
80 IF s>=w THEN LET nweek=INT (s/w): LET s=FN m(s,w)
90 IF s>=d THEN LET nday=INT (s/d): LET s=FN m(s,d)
100 IF s>=h THEN LET nhour=INT (s/h): LET s=FN m(s,h)
110 IF s>=m THEN LET nmin=INT (s/m): LET s=FN m(s,m)
120 LET nsec=INT (s)
130 IF nweek>0 THEN LET s$=s$+STR$ nweek+" wk, "
140 IF nday>0 THEN LET s$=s$+STR$ nday+" d, "
150 IF nhour>0 THEN LET s$=s$+STR$ nhour+" hr, "
160 IF nmin>0 THEN LET s$=s$+STR$ nmin+" min, "
170 IF nsec>0 THEN LET s$=s$+STR$ nsec+" sec"
180 IF s$<>"" THEN IF s$(LEN s$-1)="," THEN LET s$=s$( TO LEN s$-2)
190 PRINT s$
200 NEXT i
210 STOP
220 DEF FN m(a,b)=a-INT (a/b)*b
```

