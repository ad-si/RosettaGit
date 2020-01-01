+++
title = "Roman numerals/Encode"
description = ""
date = 2019-08-24T01:00:12Z
aliases = []
[extra]
id = 2789
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:String_manipulation]]
{{omit from|GUISS}}

;Task:
Create a function taking a positive integer as its parameter and returning a string containing the Roman numeral representation of that integer. Modern Roman numerals are written by expressing each digit separately, starting with the left most digit and skipping any digit with a value of zero.


In Roman numerals:
* 1990  is rendered: 1000=M, 900=CM, 90=XC; resulting in MCMXC
* 2008  is written as 2000=MM, 8=VIII; or MMVIII
* 1666  uses each Roman symbol in descending order: MDCLXVI





## ActionScript


```ActionScript
function arabic2roman(num:Number):String {
	var lookup:Object = {M:1000, CM:900, D:500, CD:400, C:100, XC:90, L:50, XL:40, X:10, IX:9, V:5, IV:4, I:1};
	var roman:String = "", i:String;
	for (i in lookup) {
		while (num >= lookup[i]) {
			roman += i;
			num -= lookup[i];
		}
	}
	return roman;
}
trace("1990 in roman is " + arabic2roman(1990));
trace("2008 in roman is " + arabic2roman(2008));
trace("1666 in roman is " + arabic2roman(1666));

```

{{out}}

```txt
1990 in roman is MCMXC
2008 in roman is MMVIII
1666 in roman is MDCLXVI

```

And the reverse:

```ActionScript
function roman2arabic(roman:String):Number {
	var romanArr:Array = roman.toUpperCase().split('');
	var lookup:Object = {I:1, V:5, X:10, L:50, C:100, D:500, M:1000};
	var num:Number = 0, val:Number = 0;
	while (romanArr.length) {
		val = lookup[romanArr.shift()];
		num += val * (val < lookup[romanArr[0]] ? -1 : 1);
	}
	return num;
}
trace("MCMXC in arabic is " + roman2arabic("MCMXC"));
trace("MMVIII in arabic is " + roman2arabic("MMVIII"));
trace("MDCLXVI in arabic is " + roman2arabic("MDCLXVI"));
```

{{out}}

```txt
MCMXC in arabic is 1990
MMVIII in arabic is 2008
MDCLXVI in arabic is 1666
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Roman_Numeral_Test is
   function To_Roman (Number : Positive) return String is
      subtype Digit is Integer range 0..9;
      function Roman (Figure : Digit; I, V, X : Character) return String is
      begin
         case Figure is
            when 0 => return "";
            when 1 => return "" & I;
            when 2 => return I & I;
            when 3 => return I & I & I;
            when 4 => return I & V;
            when 5 => return "" & V;
            when 6 => return V & I;
            when 7 => return V & I & I;
            when 8 => return V & I & I & I;
            when 9 => return I & X;
         end case;
      end Roman;
   begin
      pragma Assert (Number >= 1 and Number < 4000);
      return
         Roman (Number / 1000,       'M', ' ', ' ') &
         Roman (Number / 100 mod 10, 'C', 'D', 'M') &
         Roman (Number / 10 mod 10,  'X', 'L', 'C') &
         Roman (Number mod 10,       'I', 'V', 'X');
   end To_Roman;
begin
   Put_Line (To_Roman (1999));
   Put_Line (To_Roman (25));
   Put_Line (To_Roman (944));
end Roman_Numeral_Test;
```

{{out}}

```txt

 MCMXCIX
 XXV
 CMXLIV

```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
[]CHAR roman =        "MDCLXVmdclxvi"; # UPPERCASE for thousands #
[]CHAR adjust roman = "CCXXmmccxxii";
[]INT arabic =       (1000000, 500000, 100000, 50000, 10000, 5000, 1000, 500, 100, 50, 10, 5, 1);
[]INT adjust arabic = (100000, 100000,  10000, 10000,  1000, 1000,  100, 100,  10, 10,  1, 1, 0);

PROC arabic to roman = (INT dclxvi)STRING: (
  INT in := dclxvi; # 666 #
  STRING out := "";
  FOR scale TO UPB roman WHILE in /= 0 DO
    INT multiples = in OVER arabic[scale];
    in -:= arabic[scale] * multiples;
    out +:= roman[scale] * multiples;
    IF in >= -adjust arabic[scale] + arabic[scale] THEN
      in -:= -adjust arabic[scale] + arabic[scale];
      out +:=  adjust roman[scale] +  roman[scale]
    FI
  OD;
  out
);

main:(
  []INT test = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,40,50,60,69,70,
     80,90,99,100,200,300,400,500,600,666,700,800,900,1000,1009,1444,1666,1945,1997,1999,
     2000,2008,2500,3000,4000,4999,5000,6666,10000,50000,100000,500000,1000000,max int);
  FOR key TO UPB test DO
    INT val = test[key];
    print((val, " - ", arabic to roman(val), new line))
  OD
)
```

{{out}} (last example is manually wrapped):
<pre style="height:30ex;overflow:scroll">
         +1 - i
         +2 - ii
         +3 - iii
         +4 - iv
         +5 - v
         +6 - vi
         +7 - vii
         +8 - viii
         +9 - ix
        +10 - x
        +11 - xi
        +12 - xii
        +13 - xiii
        +14 - xiv
        +15 - xv
        +16 - xvi
        +17 - xvii
        +18 - xviii
        +19 - xix
        +20 - xx
        +25 - xxv
        +30 - xxx
        +40 - xl
        +50 - l
        +60 - lx
        +69 - lxix
        +70 - lxx
        +80 - lxxx
        +90 - xc
        +99 - xcix
       +100 - c
       +200 - cc
       +300 - ccc
       +400 - cd
       +500 - d
       +600 - dc
       +666 - dclxvi
       +700 - dcc
       +800 - dccc
       +900 - cm
      +1000 - m
      +1009 - mix
      +1444 - mcdxliv
      +1666 - mdclxvi
      +1945 - mcmxlv
      +1997 - mcmxcvii
      +1999 - mcmxcix
      +2000 - mm
      +2008 - mmviii
      +2500 - mmd
      +3000 - mmm
      +4000 - mV
      +4999 - mVcmxcix
      +5000 - V
      +6666 - Vmdclxvi
     +10000 - X
     +50000 - L
    +100000 - C
    +500000 - D
   +1000000 - M
+2147483647 - MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMCDLXXXmmmdcxlvii
```


## ALGOL W

<!-- {{works with|ALGOL W|Standard - no extensions to language used}} -->
{{works with|awtoc|any - tested with release [http://www.jampan.co.nz/~glyn/aw2c.tar.gz Mon Apr 27 14:25:27 NZST 2009]}}
<!-- This specimen was emailed to be by Glyn Webster > "Here's a Roman number procedure that would fit in:" -->

```algolw
BEGIN

PROCEDURE ROMAN (INTEGER VALUE NUMBER; STRING(15) RESULT CHARACTERS; INTEGER RESULT LENGTH);
    COMMENT
         Returns the Roman number of an integer between 1 and 3999.
         "MMMDCCCLXXXVIII" (15 characters long) is the longest Roman number under 4000;
    BEGIN
        INTEGER PLACE, POWER;

        PROCEDURE APPEND (STRING(1) VALUE C);
            BEGIN CHARACTERS(LENGTH|1) := C; LENGTH := LENGTH + 1 END;

        PROCEDURE I; APPEND(CASE PLACE OF ("I","X","C","M"));
        PROCEDURE V; APPEND(CASE PLACE OF ("V","L","D"));
        PROCEDURE X; APPEND(CASE PLACE OF ("X","C","M"));

        ASSERT (NUMBER >= 1) AND (NUMBER < 4000);

        CHARACTERS := "               ";
        LENGTH := 0;
        POWER := 1000;
        PLACE := 4;
        WHILE PLACE > 0 DO
            BEGIN
                CASE NUMBER DIV POWER + 1 OF BEGIN
                    BEGIN            END;
                    BEGIN I          END;
                    BEGIN I; I       END;
                    BEGIN I; I; I    END;
                    BEGIN I; V       END;
                    BEGIN V          END;
                    BEGIN V; I       END;
                    BEGIN V; I; I    END;
                    BEGIN V; I; I; I END;
                    BEGIN I; X       END
                END;
                NUMBER := NUMBER REM POWER;
                POWER := POWER DIV 10;
                PLACE := PLACE - 1
            END
    END ROMAN;

INTEGER I;
STRING(15) S;

ROMAN(1, S, I);    WRITE(S, I);
ROMAN(3999, S, I); WRITE(S, I);
ROMAN(3888, S, I); WRITE(S, I);
ROMAN(2009, S, I); WRITE(S, I);
ROMAN(405, S, I);  WRITE(S, I);
END.
```

{{out}}

```txt

I                           1
MMMCMXCIX                   9
MMMDCCCLXXXVIII            15
MMIX                        4
CDV                         3

```




## AppleScript

{{Trans|JavaScript}}
(ES6 version)
{{Trans|Haskell}}
(mapAccumL version)

```AppleScript
-- ROMAN INTEGER STRINGS ------------------------------------------------------
-- roman :: Int -> String
on roman(n)
    set kvs to {["M", 1000], ["CM", 900], ["D", 500], ¬
        ["CD", 400], ["C", 100], ["XC", 90], ["L", 50], ["XL", 40], ¬
        ["X", 10], ["IX", 9], ["V", 5], ["IV", 4], ["I", 1]}

    script stringAddedValueDeducted
        on |λ|(balance, kv)
            set {k, v} to kv
            set {q, r} to quotRem(balance, v)
            if q > 0 then
                {r, concat(replicate(q, k))}
            else
                {r, ""}
            end if
        end |λ|
    end script

    concat(snd(mapAccumL(stringAddedValueDeducted, n, kvs)))
end roman

-- TEST -----------------------------------------------------------------------
on run

    map(roman, [2016, 1990, 2008, 2000, 1666])

    --> {"MMXVI", "MCMXC", "MMVIII", "MM", "MDCLXVI"}
end run


-- GENERIC LIBRARY FUNCTIONS --------------------------------------------------

-- concat :: [[a]] -> [a] | [String] -> String
on concat(xs)
    script append
        on |λ|(a, b)
            a & b
        end |λ|
    end script

    if length of xs > 0 and class of (item 1 of xs) is string then
        set unit to ""
    else
        set unit to {}
    end if
    foldl(append, unit, xs)
end concat

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

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

-- 'The mapAccumL function behaves like a combination of map and foldl;
-- it applies a function to each element of a list, passing an
-- accumulating parameter from left to right, and returning a final
-- value of this accumulator together with the new list.' (see Hoogle)

-- mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
on mapAccumL(f, acc, xs)
    script
        on |λ|(a, x)
            tell mReturn(f) to set pair to |λ|(item 1 of a, x)
            [item 1 of pair, (item 2 of a) & {item 2 of pair}]
        end |λ|
    end script

    foldl(result, [acc, {}], xs)
end mapAccumL

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

--  quotRem :: Integral a => a -> a -> (a, a)
on quotRem(m, n)
    {m div n, m mod n}
end quotRem

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary
-- assembly of a target length

-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- snd :: (a, b) -> b
on snd(xs)
    if class of xs is list and length of xs = 2 then
        item 2 of xs
    else
        missing value
    end if
end snd
```

{{Out}}

```txt
{"MMXVI", "MCMXC", "MMVIII", "MM", "MDCLXVI"}
```



## AutoHotkey

{{trans|C++}}

```AutoHotkey
MsgBox % stor(444)

stor(value)
{
  romans = M,CM,D,CD,C,XC,L,XL,X,IX,V,IV,I
  M := 1000
  CM := 900
  D := 500
  CD := 400
  C := 100
  XC := 90
  L := 50
  XL := 40
  X := 10
  IX := 9
  V := 5
  IV := 4
  I := 1
  Loop, Parse, romans, `,
  {
    While, value >= %A_LoopField%
    {
      result .= A_LoopField
      value := value - (%A_LoopField%)
    }
  }
  Return result . "O"
}
```



## Autolisp


```Autolisp

(defun c:roman() (romanNumber (getint "\n Enter number > "))
(defun romanNumber (n / uni dec hun tho nstr strlist nlist rom)
  (if (and (> n 0) (<= n 3999))
    (progn
       (setq
         UNI (list "" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX")
         DEC (list "" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC")
         HUN (list "" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM")
         THO (list "" "M" "MM" "MMM")
         nstr (itoa n)
       )
       (while (> (strlen nstr) 0) (setq strlist (append strlist (list (substr nstr 1 1))) nstr (substr nstr 2 (strlen nstr))))
       (setq nlist (mapcar 'atoi strlist))
       (cond
          ((> n 999)(setq rom(strcat(nth (car nlist) THO)(nth (cadr nlist) HUN)(nth (caddr nlist) DEC) (nth (last nlist)UNI ))))
          ((and (> n 99)(<= n 999))(setq rom(strcat (nth (car nlist) HUN)(nth (cadr nlist) DEC) (nth (last nlist)UNI ))))
          ((and (> n 9)(<= n 99))(setq rom(strcat (nth (car nlist) DEC) (nth (last nlist)UNI ))))
          ((<= n 9)(setq rom(nth (last nlist)UNI)))
          )
        )
        (princ "\nNumber out of range!")
    )
rom
)

```

{{out}}

```txt

1577 "MDLXXVII"
3999 "MMMCMXCIX"
888 "DCCCLXXXVIII"
159 "CLIX"

```



## AWK


```AWK

# syntax: GAWK -f ROMAN_NUMERALS_ENCODE.AWK
BEGIN {
    leng = split("1990 2008 1666",arr," ")
    for (i=1; i<=leng; i++) {
      n = arr[i]
      printf("%s = %s\n",n,dec2roman(n))
    }
    exit(0)
}
function dec2roman(number,  v,w,x,y,roman1,roman10,roman100,roman1000) {
    number = int(number) # force to integer
    if (number < 1 || number > 3999) { # number is too small | big
      return
    }
    split("I II III IV V VI VII VIII IX",roman1," ")   # 1 2 ... 9
    split("X XX XXX XL L LX LXX LXXX XC",roman10," ")  # 10 20 ... 90
    split("C CC CCC CD D DC DCC DCCC CM",roman100," ") # 100 200 ... 900
    split("M MM MMM",roman1000," ")                    # 1000 2000 3000
    v = (number - (number % 1000)) / 1000
    number = number % 1000
    w = (number - (number % 100)) / 100
    number = number % 100
    x = (number - (number % 10)) / 10
    y = number % 10
    return(roman1000[v] roman100[w] roman10[x] roman1[y])
}

```

{{out}}

```txt

1990 = MCMXC
2008 = MMVIII
1666 = MDCLXVI

```



## BASIC

{{works with|FreeBASIC}}

```freebasic

DIM SHARED arabic(0 TO 12) AS Integer  => {1000, 900, 500, 400, 100, 90, 50,  40,  10,  9,  5,   4,  1 }
DIM SHARED  roman(0 TO 12) AS String*2 => {"M", "CM", "D","CD", "C","XC","L","XL","X","IX","V","IV","I"}

FUNCTION toRoman(value AS Integer) AS String
    DIM i 	AS Integer
    DIM result  AS String

    FOR i = 0 TO 12
        DO WHILE value >= arabic(i)
	    result = result + roman(i)
	    value  = value - arabic(i)
	LOOP
    NEXT i
    toRoman = result
END FUNCTION

'Testing
PRINT "2009 = "; toRoman(2009)
PRINT "1666 = "; toRoman(1666)
PRINT "3888 = "; toRoman(3888)

```


{{out}}

```txt

 2009 = MMIX
 1666 = MDCLXVI
 3888 = MMMDCCCLXXXVIII

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Roman.bas"
110 DO
120   PRINT :INPUT PROMPT "Enter an arabic number: ":N
130   IF N<1 THEN EXIT DO
140   PRINT TOROMAN$(N)
150 LOOP
160 DEF TOROMAN$(X)
170   IF X>3999 THEN
180     LET TOROMAN$="Too big."
190     EXIT DEF
200   END IF
210   RESTORE
220   LET SUM$=""
230   FOR I=1 TO 13
240     READ ARABIC,ROMAN$
250     DO WHILE X>=ARABIC
260       LET SUM$=SUM$&ROMAN$
270       LET X=X-ARABIC
280     LOOP
290   NEXT
300   LET TOROMAN$=SUM$
310 END DEF
320 DATA 1000,"M",900,"CM",500,"D",400,"CD",100,"C",90,"XC"
330 DATA 50,"L",40,"XL",10,"X",9,"IX",5,"V",4,"IV",1,"I"
```


=== {{header|ZX Spectrum Basic}} ===

```zxbasic
 10 DATA 1000,"M",900,"CM"
 20 DATA 500,"D",400,"CD"
 30 DATA 100,"C",90,"XC"
 40 DATA 50,"L",40,"XL"
 50 DATA 10,"X",9,"IX"
 60 DATA 5,"V",4,"IV",1,"I"
 70 INPUT "Enter an arabic number: ";V
 80 LET VALUE=V
 90 LET V$=""
100 FOR I=0 TO 12
110 READ A,R$
120 IF V<A THEN GO TO 160
130 LET V$=V$+R$
140 LET V=V-A
150 GO TO 120
160 NEXT I
170 PRINT VALUE;"=";V$
```



## Batch File

{{trans|BASIC}}

```dos
@echo off
setlocal enabledelayedexpansion

set cnt=0&for %%A in (1000,900,500,400,100,90,50,40,10,9,5,4,1) do (set arab!cnt!=%%A&set /a cnt+=1)
set cnt=0&for %%R in (M,CM,D,CD,C,XC,L,XL,X,IX,V,IV,I) do (set rom!cnt!=%%R&set /a cnt+=1)

::Testing
call :toRoman 2009
echo 2009 = !result!
call :toRoman 1666
echo 1666 = !result!
call :toRoman 3888
echo 3888 = !result!
pause>nul
exit/b 0

::The "function"...
:toRoman
set value=%1
set result=

for /l %%i in (0,1,12) do (
	set a=%%i
	call :add_val
)
goto :EOF

:add_val
if !value! lss !arab%a%! goto :EOF
set result=!result!!rom%a%!
set /a value-=!arab%a%!
goto add_val
```

{{Out}}

```txt
2009 = MMIX
1666 = MDCLXVI
3888 = MMMDCCCLXXXVIII
```



## BASIC256

{{works with|BASIC256 }}

```basic256

print 1666+" = "+convert$(1666)
print 2008+" = "+convert$(2008)
print 1001+" = "+convert$(1001)
print 1999+" = "+convert$(1999)

function convert$(value)
convert$=""
arabic = {1000, 900, 500, 400, 100, 90, 50,  40,  10,  9,  5,   4,  1 }
roman$ = {"M", "CM", "D","CD", "C","XC","L","XL","X","IX","V","IV","I"}
   for i = 0 to 12
           while value >= arabic[i]
	    convert$ += roman$[i]
	    value  = value - arabic[i]
	 end while
    next i
end function

```

{{out}}

```txt

1666 = MDCLXVI
2008 = MMVIII
1001 = MI
1999 = MCMXCIX

```



## BBC BASIC


```bbcbasic
      PRINT ;1999, FNroman(1999)
      PRINT ;2012, FNroman(2012)
      PRINT ;1666, FNroman(1666)
      PRINT ;3888, FNroman(3888)
      END

      DEF FNroman(n%)
      LOCAL i%, r$, arabic%(), roman$()
      DIM arabic%(12), roman$(12)
      arabic%() = 1,   4,   5,   9,  10,  40,  50,  90, 100, 400, 500, 900,1000
      roman$() = "I","IV", "V","IX", "X","XL", "L","XC", "C","CD", "D","CM", "M"
      FOR i% = 12 TO 0 STEP -1
        WHILE n% >= arabic%(i%)
          r$ += roman$(i%)
          n% -= arabic%(i%)
        ENDWHILE
      NEXT
      = r$
```

{{out}}

```txt

1999      MCMXCIX
2012      MMXII
1666      MDCLXVI
3888      MMMDCCCLXXXVIII

```



## Befunge

Reads the number to convert from standard input. No range validation is performed.


```befunge
&>0\0>00p:#v_$ >:#,_ $ @
4-v >5+#:/#<\55+%:5/\5%:
vv_$9+00g+5g\00g8+>5g\00
g>\20p>:10p00g \#v _20gv
> 2+ v^-1g01\g5+8<^ +9 _
        IVXLCDM
```


{{out}}

```txt
1666
MDCLXVI
```



## Bracmat


```bracmat
( ( encode
  =   indian roman cifr tenfoldroman letter tenfold
    .   !arg:#?indian
      & :?roman
      &   whl
        ' ( @(!indian:#%?cifr ?indian)
          & :?tenfoldroman
          &   whl
            ' ( !roman:%?letter ?roman
              &     !tenfoldroman
                    (       (I.X)
                            (V.L)
                            (X.C)
                            (L.D)
                            (C.M)
                        : ? (!letter.?tenfold) ?
                      & !tenfold
                    | "*"
                    )
                : ?tenfoldroman
              )
          & !tenfoldroman:?roman
          & ( !cifr:9&!roman I X:?roman
            |   !cifr:~<4
              &     !roman
                    (!cifr:4&I|)
                    V
                : ?roman
              & !cifr+-5:?cifr
              & ~
            |   whl
              ' ( !cifr+-1:~<0:?cifr
                & !roman I:?roman
                )
            )
          )
      & ( !roman:? "*" ?&~`
        | str$!roman
        )
  )
& 1990 2008 1666 3888 3999 4000:?NS
&   whl
  ' ( !NS:%?N ?NS
    &   out
      $ ( encode$!N:?K&!N !K
        | str$("Can't convert " !N " to Roman numeral")
        )
    )
);
```

{{out}}

```txt
1990 MCMXC
2008 MMVIII
1666 MDCLXVI
3888 MMMDCCCLXXXVIII
3999 MMMCMXCIX
Can't convert 4000 to Roman numeral
```



## C



```cpp
#include <iostream>
#include <stdio.h>

/*
 * Writes the Roman numeral representing n into the buffer s.
 * Handles up to n = 3999.
 * Since C doesn't have exceptions, n = 0 causes the whole program to exit
 * unsuccessfully.
 * s should be have room for at least 16 characters, including the trailing
 * null.
 */
void roman(char *s, unsigned int n)
{
 if (n == 0)
 {
  fputs("Roman numeral for zero requested.", stderr);
  exit(EXIT_FAILURE);
 }

  #define digit(loop, num, c) \
      loop (n >= num)         \
         {*(s++) = c;         \
          n -= num;}
  #define digits(loop, num, c1, c2) \
      loop (n >= num)               \
         {*(s++) = c1;              \
          *(s++) = c2;              \
          n -= num;}

  digit  ( while, 1000, 'M'      )
  digits ( if,     900, 'C', 'M' )
  digit  ( if,     500, 'D'      )
  digits ( if,     400, 'C', 'D' )
  digit  ( while,  100, 'C'      )
  digits ( if,      90, 'X', 'C' )
  digit  ( if,      50, 'L'      )
  digits ( if,      40, 'X', 'L' )
  digit  ( while,   10, 'X'      )
  digits ( if,       9, 'I', 'X' )
  digit  ( if,       5, 'V'      )
  digits ( if,       4, 'I', 'V' )
  digit  ( while,    1, 'I'      )

  #undef digit
  #undef digits

  *s = 0;}

int main(void)
{
 char buffer[16];
 unsigned int i;
 for (i = 1 ; i < 4000 ; ++i)
 {
  roman(buffer, i);
  printf("%4u: %s\n", i, buffer);
 }
 return EXIT_SUCCESS;
}
```


An alternative version which builds the string backwards.

```c
char *ToRoman(int num, char *buf, int buflen)
{
   static const char romanDgts[] = "ivxlcdmVXLCDM_";
   char *roman = buf + buflen;
   int  rdix, r, v;
   *--roman = '\0';        /* null terminate return string */
   if (num >= 4000000) {
      printf("Number Too Big.\n");
      return NULL;
      }
   for (rdix = 0; rdix < strlen(romanDgts); rdix += 2) {
      if (num == 0) break;
      v = (num % 10) / 5;
      r = num % 5;
      num = num / 10;
      if (r == 4) {
         if (roman < buf+2) {
            printf("Buffer too small.");
            return NULL;
            }
         *--roman = romanDgts[rdix+1+v];
         *--roman = romanDgts[rdix];
         }
      else {
         if (roman < buf+r+v) {
            printf("Buffer too small.");
            return NULL;
            }
         while(r-- > 0) {
            *--roman = romanDgts[rdix];
            }
         if (v==1) {
            *--roman = romanDgts[rdix+1];
            }
         }
      }
   return roman;
}
```


Most straightforward (nothing elegant about it,
but it's simple, and can calculate output length)

```c
#include <stdio.h>

int to_roman(char *out, int n)
{
        int len = 0;
        if (n <= 0) return 0; /* error indication */
#       define RPUT(c) if (out) out[len] = c; len++
        while(n>= 1000) { n -= 1000;RPUT('M'); };

        if (n >= 900)   { n -= 900; RPUT('C'); RPUT('M'); };
        if (n >= 500)   { n -= 500; RPUT('D'); };
        if (n >= 400)   { n -= 400; RPUT('C'); RPUT('D'); };
        while (n >= 100){ n -= 100; RPUT('C'); };

        if (n >= 90)    { n -= 90;  RPUT('X'); RPUT('C'); };
        if (n >= 50)    { n -= 50;  RPUT('L'); };
        if (n >= 40)    { n -= 40;  RPUT('X'); RPUT('L'); };
        while (n >= 10) { n -= 10;  RPUT('X'); };

        if (n >= 9)     { n -= 9;   RPUT('I'); RPUT('X'); };
        if (n >= 5)     { n -= 5;   RPUT('V'); };
        if (n >= 4)     { n -= 4;   RPUT('I'); RPUT('V'); };
        while (n)       { n--; RPUT('I'); };
        RPUT('\0');
#       undef RPUT

        return len;
}

int main()
{
        char buf[16];
        int d = to_roman(buf, 1666);
        printf("roman for 1666 is %d bytes: %s\n", d, buf);

        d = 68999123;
        printf("%d would have required %d bytes\n", d, to_roman(0, d));

        return 0;
}
```

{{out}}

```txt
roman for 1666 is 8 bytes: MDCLXVI
68999123 would have required 69006 bytes

```


## C#

```c#
using System;
class Program
{
    static uint[] nums = { 1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1 };
    static string[] rum = { "M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I" };

    static string ToRoman(uint number)
    {
        string value = "";
        for (int i = 0; i < nums.Length && number != 0; i++)
        {
            while (number >= nums[i])
            {
                number -= nums[i];
                value += rum[i];
            }
        }
        return value;
    }

    static void Main()
    {
        for (uint number = 1; number <= 1 << 10; number *= 2)
        {
            Console.WriteLine("{0} = {1}", number, ToRoman(number));
        }
    }
}
```


One-liner Mono REPL

```c#

Func<int, string> toRoman = (number) =>
  new Dictionary<int, string>
  {
    {1000, "M"},
    {900, "CM"},
    {500, "D"},
    {400, "CD"},
    {100, "C"},
    {90, "XC"},
    {50, "L"},
    {40, "XL"},
    {10, "X"},
    {9, "IX"},
    {5, "V"},
    {4, "IV"},
    {1, "I"}
  }.Aggregate(new string('I', number), (m, _) => m.Replace(new string('I', _.Key), _.Value));

```


{{out}}

```txt

1 = I
2 = II
4 = IV
8 = VIII
16 = XVI
32 = XXXII
64 = LXIV
128 = CXXVIII
256 = CCLVI
512 = DXII
1024 = MXXIV

```



## C++


### C++ 98


```cpp
#include <iostream>
#include <string>

std::string to_roman(int value)
{
  struct romandata_t { int value; char const* numeral; };
  static romandata_t const romandata[] =
     { 1000, "M",
        900, "CM",
        500, "D",
        400, "CD",
        100, "C",
         90, "XC",
         50, "L",
         40, "XL",
         10, "X",
          9, "IX",
          5, "V",
          4, "IV",
          1, "I",
          0, NULL }; // end marker

  std::string result;
  for (romandata_t const* current = romandata; current->value > 0; ++current)
  {
    while (value >= current->value)
    {
      result += current->numeral;
      value  -= current->value;
    }
  }
  return result;
}

int main()
{
  for (int i = 1; i <= 4000; ++i)
  {
    std::cout << to_roman(i) << std::endl;
  }
}
```



### C++ 11



```cpp
#include <iostream>
#include <string>

std::string to_roman(int x) {
    auto roman_digit = [&](char one, char five, char ten, int x) {
        if (x <= 3)
            return std::string().assign(x, one);
        if (x <= 5)
            return std::string().assign(5 - x, one) + five;
        if (x <= 8)
           return five + std::string().assign(x - 5, one);
        return std::string().assign(10 - x, one) + ten;
    };
    if (x <= 0)
        return "Negative or zero!";
    if (x >= 1000)
        return "M" + to_roman(x - 1000);
    if (x >= 100)
        return roman_digit('C', 'D', 'M', x / 100) + to_roman(x % 100);
    if (x >= 10)
        return roman_digit('X', 'L', 'C', x / 10) + to_roman(x % 10);
    return roman_digit('I', 'V', 'X', x);
}

int main(){
    for (int i = 1; i < 2018; i+= 1)
        std::cout << i << " --> " << to_roman(i) << std::endl;
}

```



## Ceylon


```ceylon
shared void run() {

	class Numeral(shared Character char, shared Integer int) {}

	value tiers = [
		[Numeral('I', 1),   Numeral('V', 5),   Numeral('X', 10)],
		[Numeral('X', 10),  Numeral('L', 50),  Numeral('C', 100)],
		[Numeral('C', 100), Numeral('D', 500), Numeral('M', 1k)]
	];

	String toRoman(Integer hindu, Integer tierIndex = 2) {

		assert (exists tier = tiers[tierIndex]);

		" Finds if it's a two character numeral like iv, ix, xl, xc, cd and cm."
		function findTwoCharacterNumeral() =>
			if (exists bigNum = tier.rest.find((numeral) => numeral.int - tier.first.int <= hindu < numeral.int))
			then [tier.first, bigNum]
			else null;

		if (hindu <= 0) {
			// if it's zero then we are done!
			return "";
		}
		else if (exists [smallNum, bigNum] = findTwoCharacterNumeral()) {
			value twoCharSymbol = "``smallNum.char````bigNum.char``";
			value twoCharValue = bigNum.int - smallNum.int;
			return "``twoCharSymbol````toRoman(hindu - twoCharValue, tierIndex)``";
		}
		else if (exists num = tier.reversed.find((Numeral elem) => hindu >= elem.int)) {
			return "``num.char````toRoman(hindu - num.int, tierIndex)``";
		}
		else {
			// nothing was found so move to the next smaller tier!
			return toRoman(hindu, tierIndex - 1);
		}
	}

	assert (toRoman(1) == "I");
	assert (toRoman(2) == "II");
	assert (toRoman(4) == "IV");
	assert (toRoman(1666) == "MDCLXVI");
	assert (toRoman(1990) == "MCMXC");
	assert (toRoman(2008) == "MMVIII");
}
```



## Clojure

The easiest way is to use the built-in cl-format function

```Clojure
(def arabic->roman
  (partial clojure.pprint/cl-format nil "~@R"))

(arabic->roman 147)
;"CXXIII"
(arabic->roman 99)
;"XCIX"
```
Alternatively:
```Clojure
(def roman-map
  (sorted-map
    1    "I", 4    "IV", 5   "V", 9   "IX",
    10   "X", 40   "XL", 50  "L", 90  "XC",
    100  "C", 400  "CD", 500 "D", 900 "CM"
    1000 "M"))

(defn int->roman [n]
  {:pre (integer? n)}
  (loop [res (StringBuilder.), n n]
    (if-let [v (roman-map n)]
      (str (.append res v))
      (let [[k v] (->> roman-map keys (filter #(> n %)) last (find roman-map))]
        (recur (.append res v) (- n k))))))

(int->roman 1999)
; "MCMXCIX"
```



An alternate implementation:


```Clojure

(defn a2r [a]
  (let [rv '(1000 500 100 50 10 5 1)
        rm (zipmap rv "MDCLXVI")
        dv (->> rv (take-nth 2) next #(interleave % %))]
    (loop [a a rv rv dv dv r nil]
      (if (<= a 0)
        r
        (let [v (first rv)
              d (or (first dv) 0)
              l (- v d)]
          (cond
            (= a v) (str r (rm v))
            (= a l) (str r (rm d) (rm v))
            (and (> a v) (> a l)) (recur (- a v) rv dv (str r (rm v)))
            (and (< a v) (< a l)) (recur a (rest rv) (rest dv) r)
            :else (recur (- a l) (rest rv) (rest dv) (str r (rm d) (rm v)))))))))

```


Usage:


```Clojure

(a2r 1666)
"MDCLXVI"

(map a2r [1000 1 389 45])
("M" "I" "CCCLXXXIX" "XLV")

```


An alternate implementation:


```Clojure

(def roman-map
  (sorted-map-by >
                 1    "I", 4    "IV", 5   "V", 9   "IX",
                 10   "X", 40   "XL", 50  "L", 90  "XC",
                 100  "C", 400  "CD", 500 "D", 900 "CM"
                 1000 "M"))

(defn a2r
  ([r]
   (reduce str (a2r r (keys roman-map))))
  ([r n]
   (when-not (empty? n)
     (let [e (first n)
           v (- r e)
           roman (roman-map e)]
       (cond
         (< v 0) (a2r r (rest n))
         (= v 0) (cons roman [])
         (>= v e) (cons roman (a2r v n))
         (< v e) (cons roman (a2r v (rest n))))))))

```


Usage:


```Clojure

(a2r 1666)
"MDCLXVI"

(map a2r [1000 1 389 45])
("M" "I" "CCCLXXXIX" "XLV")

```



## COBOL



```COBOL

IDENTIFICATION DIVISION.
PROGRAM-ID. TOROMAN.
DATA DIVISION.
working-storage section.
  01 ws-number pic 9(4) value 0.
  01 ws-save-number pic 9(4).
  01 ws-tbl-def.
    03 filler pic x(7) value '1000M  '.
    03 filler pic x(7) value '0900CM '.
    03 filler pic x(7) value '0500D  '.
    03 filler pic x(7) value '0400CD '.
    03 filler pic x(7) value '0100C  '.
    03 filler pic x(7) value '0090XC '.
    03 filler pic x(7) value '0050L  '.
    03 filler pic x(7) value '0040XL '.
    03 filler pic x(7) value '0010X  '.
    03 filler pic x(7) value '0009IX '.
    03 filler pic x(7) value '0005V  '.
    03 filler pic x(7) value '0004IV '.
    03 filler pic x(7) value '0001I  '.
  01  filler redefines ws-tbl-def.
    03 filler occurs 13 times indexed by rx.
      05 ws-tbl-divisor    pic 9(4).
      05 ws-tbl-roman-ch   pic x(1) occurs 3 times indexed by cx.
  01 ocx pic 99.
  01 ws-roman.
    03 ws-roman-ch         pic x(1) occurs 16 times.
PROCEDURE DIVISION.
  accept ws-number
  perform
  until ws-number = 0
    move ws-number to ws-save-number
    if ws-number > 0 and ws-number < 4000
      initialize ws-roman
      move 0 to ocx
      perform varying rx from 1 by +1
      until ws-number = 0
        perform until ws-number < ws-tbl-divisor (rx)
          perform varying cx from 1 by +1
  		  until ws-tbl-roman-ch (rx, cx) = spaces
            compute ocx = ocx + 1
            move ws-tbl-roman-ch (rx, cx) to ws-roman-ch (ocx)
          end-perform
          compute ws-number = ws-number - ws-tbl-divisor (rx)
        end-perform
      end-perform
      display 'inp=' ws-save-number ' roman=' ws-roman
    else
      display 'inp=' ws-save-number ' invalid'
    end-if
    accept ws-number
  end-perform
  .

```

{{out}} (input was supplied via STDIN)

```txt

inp=0111 roman=CXI
inp=2234 roman=MMCCXXXIV
inp=0501 roman=DI
inp=0010 roman=X
inp=0040 roman=XL
inp=0050 roman=L
inp=0066 roman=LXVI
inp=0666 roman=DCLXVI
inp=5666 invalid
inp=3333 roman=MMMCCCXXXIII
inp=3888 roman=MMMDCCCLXXXVIII
inp=3999 roman=MMMCMXCIX
inp=3345 roman=MMMCCCXLV

```



## CoffeeScript



```coffeescript

decimal_to_roman = (n) ->
  # This should work for any positive integer, although it
  # gets a bit preposterous for large numbers.
  if n >= 4000
    thousands = decimal_to_roman n / 1000
    ones = decimal_to_roman n % 1000
    return "M(#{thousands})#{ones}"

  s = ''
  translate_each = (min, roman) ->
    while n >= min
      n -= min
      s += roman
  translate_each 1000, "M"
  translate_each  900, "CM"
  translate_each  500, "D"
  translate_each  400, "CD"
  translate_each  100, "C"
  translate_each   90, "XC"
  translate_each   50, "L"
  translate_each   40, "XL"
  translate_each   10, "X"
  translate_each    9, "IX"
  translate_each    5, "V"
  translate_each    4, "IV"
  translate_each    1, "I"
  s

###################
tests =
  IV: 4
  XLII: 42
  MCMXC: 1990
  MMVIII: 2008
  MDCLXVI: 1666
  'M(IV)': 4000
  'M(VI)IX': 6009
  'M(M(CXXIII)CDLVI)DCCLXXXIX': 123456789
  'M(MMMV)I': 3005001

for expected, decimal of tests
  roman = decimal_to_roman(decimal)
  if roman == expected
    console.log "#{decimal} = #{roman}"
  else
    console.log "error for #{decimal}: #{roman} is wrong"

```



## Common Lisp



```lisp
(defun roman-numeral (n)
  (format nil "~@R" n))
```



## D


```d
string toRoman(int n) pure nothrow
in {
    assert(n < 5000);
} body {
    static immutable weights = [1000, 900, 500, 400, 100, 90,
                                50, 40, 10, 9, 5, 4, 1];
    static immutable symbols = ["M","CM","D","CD","C","XC","L",
                                "XL","X","IX","V","IV","I"];

    string roman;
    foreach (i, w; weights) {
        while (n >= w) {
            roman ~= symbols[i];
            n -= w;
        }
        if (n == 0)
            break;
    }
    return roman;
} unittest {
    assert(toRoman(455)  == "CDLV");
    assert(toRoman(3456) == "MMMCDLVI");
    assert(toRoman(2488) == "MMCDLXXXVIII");
}

void main() {}
```



## Delphi

{{trans|DWScript}}

```delphi
program RomanNumeralsEncode;

{$APPTYPE CONSOLE}

function IntegerToRoman(aValue: Integer): string;
var
  i: Integer;
const
  WEIGHTS: array[0..12] of Integer = (1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1);
  SYMBOLS: array[0..12] of string = ('M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX', 'V', 'IV', 'I');
begin
  for i := Low(WEIGHTS) to High(WEIGHTS) do
  begin
    while aValue >= WEIGHTS[i] do
    begin
      Result := Result + SYMBOLS[i];
      aValue := aValue - WEIGHTS[i];
    end;
    if aValue = 0 then
      Break;
  end;
end;

begin
  Writeln(IntegerToRoman(1990)); // MCMXC
  Writeln(IntegerToRoman(2008)); // MMVIII
  Writeln(IntegerToRoman(1666)); // MDCLXVI
end.
```



## DWScript

{{trans|D}}

```delphi
const weights = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
const symbols = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"];

function toRoman(n : Integer) : String;
var
   i, w : Integer;
begin
   for i := 0 to weights.High do begin
      w := weights[i];
      while n >= w do begin
         Result += symbols[i];
         n -= w;
      end;
      if n = 0 then Break;
   end;
end;

PrintLn(toRoman(455));
PrintLn(toRoman(3456));
PrintLn(toRoman(2488));
```



## ECL


```ECL
RomanEncode(UNSIGNED Int) := FUNCTION
  SetWeights := [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
  SetSymbols := ['M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX', 'V', 'IV', 'I'];
  ProcessRec := RECORD
    UNSIGNED val;
    STRING Roman;
  END;
  dsWeights  := DATASET(13,TRANSFORM(ProcessRec,SELF.val := Int, SELF := []));

  SymbolStr(i,n,STRING s) := CHOOSE(n+1,'',SetSymbols[i],SetSymbols[i]+SetSymbols[i],SetSymbols[i]+SetSymbols[i]+SetSymbols[i],s);

  RECORDOF(dsWeights) XF(dsWeights L, dsWeights R, INTEGER C) := TRANSFORM
    ThisVal := IF(C=1,R.Val,L.Val);
    IsDone := ThisVal = 0;
    SELF.Roman := IF(IsDone,L.Roman,L.Roman + SymbolStr(C,ThisVal DIV SetWeights[C],L.Roman));
    SELF.val := IF(IsDone,0,ThisVal - ((ThisVal DIV SetWeights[C])*SetWeights[C]));
  END;
  i := ITERATE(dsWeights,XF(LEFT,RIGHT,COUNTER));
  RETURN i[13].Roman;
END;

RomanEncode(1954);  //MCMLIV
RomanEncode(1990 ); //MCMXC
RomanEncode(2008 ); //MMVIII
RomanEncode(1666);  //MDCLXVI
```



## Eiffel


```Eiffel
class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
		local
			numbers: ARRAY [INTEGER]
		do
			numbers := <<1990, 2008, 1666, 3159, 1977, 2010>>
				   -- "MCMXC", "MMVIII", "MDCLXVI", "MMMCLIX", "MCMLXXVII", "MMX"
			across numbers as n loop
				print (n.item.out + " in decimal Arabic numerals is " +
				       decimal_to_roman (n.item) + " in Roman numerals.%N")
			end
		end

feature -- Roman numerals

	decimal_to_roman (a_int: INTEGER): STRING
		-- Representation of integer `a_int' as Roman numeral
		require
			a_int > 0
		local
			dnums: ARRAY[INTEGER]
			rnums: ARRAY[STRING]

			dnum: INTEGER
			rnum: STRING

			i: INTEGER
		do
			dnums := <<1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1>>
			rnums := <<"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I">>

			dnum := a_int
			rnum := ""

			from
				i := 1
			until
				i > dnums.count or dnum <= 0
			loop
				from
				until
					dnum < dnums[i]
				loop
					dnum := dnum - dnums[i]
					rnum := rnum + rnums[i]
				end
				i := i + 1
			end

			Result := rnum
		end
end
```



## Ela

{{trans|Haskell}}

```ela
open number string math

digit x y z k =
  [[x],[x,x],[x,x,x],[x,y],[y],[y,x],[y,x,x],[y,x,x,x],[x,z]] :
  (toInt k - 1)

toRoman 0 = ""
toRoman x | x < 0     = fail "Negative roman numeral"
          | x >= 1000 = 'M' :: toRoman (x - 1000)
          | x >= 100  = let (q,r) = x `divrem` 100 in
                        digit 'C' 'D' 'M' q ++ toRoman r
          | x >= 10   = let (q,r) = x `divrem` 10 in
                        digit 'X' 'L' 'C' q ++ toRoman r
          | else = digit 'I' 'V' 'X' x

map (join "" << toRoman) [1999,25,944]
```


{{out}}

```txt
["MCMXCIX","XXV","CMXLIV"]
```


## Elena

{{trans|C#}}
ELENA 4.x :

```elena
import system'collections;
import system'routines;
import extensions;
import extensions'text;

static RomanDictionary = new Dictionary()
                            .setAt(1000, "M")
                            .setAt(900, "CM")
                            .setAt(500, "D")
                            .setAt(400, "CD")
                            .setAt(100, "C")
                            .setAt(90, "XC")
                            .setAt(50, "L")
                            .setAt(40, "XL")
                            .setAt(10, "X")
                            .setAt(9, "IX")
                            .setAt(5, "V")
                            .setAt(4, "IV")
                            .setAt(1, "I");

extension op
{
    toRoman()
        = RomanDictionary.accumulate(new StringWriter("I", self), (m,kv => m.replace(new StringWriter("I",kv.Key), kv.Value)));
}

public program()
{
    console.printLine("1990 : ", 1990.toRoman());
    console.printLine("2008 : ", 2008.toRoman());
    console.printLine("1666 : ", 1666.toRoman())
}
```

{{out}}

```txt

1990 : MCMXC
2008 : MMVIII
1666 : MDCLXVI

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Roman_numeral do
  def encode(0), do: ''
  def encode(x) when x >= 1000, do: [?M | encode(x - 1000)]
  def encode(x) when x >= 100,  do: digit(div(x,100), ?C, ?D, ?M) ++ encode(rem(x,100))
  def encode(x) when x >= 10,   do: digit(div(x,10), ?X, ?L, ?C) ++ encode(rem(x,10))
  def encode(x) when x >= 1,    do: digit(x, ?I, ?V, ?X)

  defp digit(1, x, _, _), do: [x]
  defp digit(2, x, _, _), do: [x, x]
  defp digit(3, x, _, _), do: [x, x, x]
  defp digit(4, x, y, _), do: [x, y]
  defp digit(5, _, y, _), do: [y]
  defp digit(6, x, y, _), do: [y, x]
  defp digit(7, x, y, _), do: [y, x, x]
  defp digit(8, x, y, _), do: [y, x, x, x]
  defp digit(9, x, _, z), do: [x, z]
end
```


'''Another:'''
{{trans|Ruby}}

```elixir
defmodule Roman_numeral do
  @symbols [ {1000, 'M'}, {900, 'CM'}, {500, 'D'}, {400, 'CD'}, {100, 'C'}, {90, 'XC'},
             {50, 'L'}, {40, 'XL'}, {10, 'X'}, {9, 'IX'}, {5, 'V'}, {4, 'IV'}, {1, 'I'} ]
  def encode(num) do
    {roman,_} = Enum.reduce(@symbols, {[], num}, fn {divisor, letter}, {memo, n} ->
                  {memo ++ List.duplicate(letter, div(n, divisor)), rem(n, divisor)}
                end)
    Enum.join(roman)
  end
end
```


'''Test:'''

```elixir
Enum.each([1990, 2008, 1666], fn n ->
  IO.puts "#{n}: #{Roman_numeral.encode(n)}"
end)
```


{{out}}

```txt

1990: MCMXC
2008: MMVIII
1666: MDCLXVI

```



## Emacs Lisp


```lisp

(defun ar2ro (AN)
  "translate from arabic number AN to roman number,
   ar2ro(1666) returns (M D C L X V I)"
  (cond
   ((>= AN 1000) (cons 'M (ar2ro (- AN 1000))))
   ((>= AN 900) (cons 'C (cons 'M (ar2ro (- AN 900)))))
   ((>= AN 500) (cons 'D (ar2ro (- AN 500))))
   ((>= AN 400) (cons 'C (cons 'D (ar2ro (- AN 400)))))
   ((>= AN 100) (cons 'C (ar2ro (- AN 100))))
   ((>= AN 90) (cons 'X (cons 'C (ar2ro (- AN 90)))))
   ((>= AN 50) (cons 'L (ar2ro (- AN 50))))
   ((>= AN 40) (cons 'X (cons 'L (ar2ro (- AN 40)))))
   ((>= AN 10) (cons 'X (ar2ro (- AN 10))))
   ((>= AN 5) (cons 'V (ar2ro (- AN 5))))
   ((>= AN 4) (cons 'I (cons 'V (ar2ro (- AN 4)))))
   ((>= AN 1) (cons 'I (ar2ro (- AN 1))))
   ((= AN 0) nil)))

```



## Erlang

{{trans|OCaml}}

```erlang
-module(roman).
-export([to_roman/1]).

to_roman(0) -> [];
to_roman(X) when X >= 1000 -> [$M | to_roman(X - 1000)];
to_roman(X) when X >= 100 ->
    digit(X div 100, $C, $D, $M) ++ to_roman(X rem 100);
to_roman(X) when X >= 10 ->
    digit(X div 10, $X, $L, $C) ++ to_roman(X rem 10);
to_roman(X) when X >= 1 -> digit(X, $I, $V, $X).

digit(1, X, _, _) -> [X];
digit(2, X, _, _) -> [X, X];
digit(3, X, _, _) -> [X, X, X];
digit(4, X, Y, _) -> [X, Y];
digit(5, _, Y, _) -> [Y];
digit(6, X, Y, _) -> [Y, X];
digit(7, X, Y, _) -> [Y, X, X];
digit(8, X, Y, _) -> [Y, X, X, X];
digit(9, X, _, Z) -> [X, Z].
```


sample:

```txt

1> c(roman).
{ok,roman}
2> roman:to_roman(1999).
"MCMXCIX"
3> roman:to_roman(25).
"XXV"
4> roman:to_roman(944).
"CMXLIV"

```


Alternative:

```erlang

-module( roman_numerals ).

-export( [encode_from_integer/1]).

-record( encode_acc, {n, romans=""} ).

encode_from_integer( N ) when N > 0 ->
        #encode_acc{romans=Romans} = lists:foldl( fun encode_from_integer/2, #encode_acc{n=N}, map() ),
        Romans.


encode_from_integer( _Map, #encode_acc{n=0}=Acc ) -> Acc;
encode_from_integer( {_Roman, Value}, #encode_acc{n=N}=Acc ) when N < Value -> Acc;
encode_from_integer( {Roman, Value}, #encode_acc{n=N, romans=Romans} ) ->
        Times = N div Value,
        New_roman = lists:flatten( lists:duplicate(Times, Roman) ),
        #encode_acc{n=N - (Times * Value), romans=Romans ++ New_roman}.

map() -> [{"M",1000}, {"CM",900}, {"D",500}, {"CD",400}, {"C",100}, {"XC",90}, {"L",50}, {"XL",40}, {"X",10}, {"IX",9}, {"V",5}, {"IV",4}, {"I\
",1}].

```


{{out}}

```txt

36> roman_numerals:encode_from_integer( 1990 ).
"MCMXC"
37> roman_numerals:encode_from_integer( 2008 ).
"MMVIII"
38> roman_numerals:encode_from_integer( 1666 ).
"MDCLXVI"

```



## ERRE


```ERRE

PROGRAM ARAB2ROMAN

DIM ARABIC%[12],ROMAN$[12]

PROCEDURE TOROMAN(VALUE->ANS$)
LOCAL RESULT$
    FOR I%=0 TO 12 DO
        WHILE VALUE>=ARABIC%[I%] DO
            RESULT$+=ROMAN$[I%]
            VALUE-=ARABIC%[I%]
        END WHILE
    END FOR
    ANS$=RESULT$
END PROCEDURE

BEGIN
!
!Testing
!
   ARABIC%[]=(1000,900,500,400,100,90,50,40,10,9,5,4,1)
   ROMAN$[]=("M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I")
   TOROMAN(2009->ANS$) PRINT("2009 = ";ANS$)
   TOROMAN(1666->ANS$) PRINT("1666 = ";ANS$)
   TOROMAN(3888->ANS$) PRINT("3888 = ";ANS$)
END PROGRAM

```



## Euphoria

{{trans|BASIC}}

```Euphoria
constant arabic = {1000, 900, 500, 400, 100, 90, 50,  40,  10,  9,  5,   4,  1 }
constant roman  = {"M", "CM", "D","CD", "C","XC","L","XL","X","IX","V","IV","I"}

function toRoman(integer val)
    sequence result
    result = ""
    for i = 1 to 13 do
        while val >= arabic[i] do
            result &= roman[i]
            val -= arabic[i]
        end while
    end for
    return result
end function

printf(1,"%d = %s\n",{2009,toRoman(2009)})
printf(1,"%d = %s\n",{1666,toRoman(1666)})
printf(1,"%d = %s\n",{3888,toRoman(3888)})
```


{{out}}

```txt

 2009 = MMIX
 1666 = MDCLXVI
 3888 = MMMDCCCLXXXVIII

```



## Excel

Excel can encode numbers in Roman forms in 5 successively concise forms.
These can be indicated from 0 to 4. Type in a cell:

```Excel

=ROMAN(2013,0)

```


It becomes:
<lang>
MMXIII

```


=={{header|F_Sharp|F#}}==

```fsharp
let digit x y z = function
    1 -> x
  | 2 -> x + x
  | 3 -> x + x + x
  | 4 -> x + y
  | 5 -> y
  | 6 -> y + x
  | 7 -> y + x + x
  | 8 -> y + x + x + x
  | 9 -> x + z
  | _ -> failwith "invalid call to digit"

let rec to_roman acc = function
    | x when x >= 1000 -> to_roman (acc + "M") (x - 1000)
    | x when x >= 100 -> to_roman (acc + digit "C" "D" "M" (x / 100)) (x % 100)
    | x when x >= 10 -> to_roman (acc + digit "X" "L" "C" (x / 10)) (x % 10)
    | x when x > 0 -> acc + digit "I" "V" "X" x
    | 0 -> acc
    | _ -> failwith "invalid call to_roman (negative input)"

let roman n = to_roman "" n

[<EntryPoint>]
let main args =
    [1990; 2008; 1666]
    |> List.map (fun n -> roman n)
    |> List.iter (printfn "%s")
    0
```

{{out}}

```txt
MCMXC
MMVIII
MDCLXVI
```



## Factor

A roman numeral library ships with Factor.

```factor
USE: roman
( scratchpad ) 3333 >roman .
"mmmcccxxxiii"
```


Parts of the implementation:


```factor
CONSTANT: roman-digits
    { "m" "cm" "d" "cd" "c" "xc" "l" "xl" "x" "ix" "v" "iv" "i" }

CONSTANT: roman-values
    { 1000 900 500 400 100 90 50 40 10 9 5 4 1 }

ERROR: roman-range-error n ;

: roman-range-check ( n -- n )
    dup 1 10000 between? [ roman-range-error ] unless ;

: >roman ( n -- str )
    roman-range-check
    roman-values roman-digits [
        [ /mod swap ] dip <repetition> concat
    ] 2map "" concat-as nip ;
```



## FALSE


```false
^$." "
[$999>][1000- "M"]#
 $899> [ 900-"CM"]?
 $499> [ 500- "D"]?
 $399> [ 400-"CD"]?
[$ 99>][ 100- "C"]#
 $ 89> [  90-"XC"]?
 $ 49> [  50- "L"]?
 $ 39> [  40-"XL"]?
[$  9>][  10- "X"]#
 $  8> [   9-"IX"]?
 $  4> [   5- "V"]?
 $  3> [   4-"IV"]?
[$    ][   1- "I"]#%
```



## Fan


```Fan
**
** converts a number to its roman numeral representation
**
class RomanNumerals
{

  private Str digit(Str x, Str y, Str z, Int i)
  {
    switch (i)
    {
      case 1: return x
      case 2: return x+x
      case 3: return x+x+x
      case 4: return x+y
      case 5: return y
      case 6: return y+x
      case 7: return y+x+x
      case 8: return y+x+x+x
      case 9: return x+z
    }
    return ""
  }

  Str toRoman(Int i)
  {
    if (i>=1000) { return "M" + toRoman(i-1000) }
    if (i>=100) { return digit("C", "D", "M", i/100) + toRoman(i%100) }
    if (i>=10) { return digit("X", "L", "C", i/10) + toRoman(i%10) }
    if (i>=1) { return digit("I", "V", "X", i) }
    return ""
  }

  Void main()
  {
    2000.times |i| { echo("$i = ${toRoman(i)}") }
  }

}
```



## Forth


```forth
: vector create ( n -- ) 0 do , loop  does>  ( n -- ) swap cells + @ execute ;
\ these are ( numerals -- numerals )
: ,I  dup c@ C, ;  : ,V  dup 1 + c@ C, ;  : ,X  dup 2 + c@ C, ;

\ these are ( numerals -- )
:noname  ,I ,X     drop ;   :noname  ,V ,I ,I ,I  drop ;   :noname  ,V ,I ,I  drop ;
:noname  ,V ,I     drop ;   :noname  ,V           drop ;   :noname  ,I ,V     drop ;
:noname  ,I ,I ,I  drop ;   :noname  ,I ,I        drop ;   :noname  ,I        drop ;
' drop ( 0 : no output )  10 vector ,digit

: roman-rec ( numerals n -- )  10 /mod dup if >r over 2 + r> recurse else drop then ,digit ;
: roman ( n -- c-addr u )
  dup 0 4000 within 0= abort" EX LIMITO!"
  HERE SWAP  s" IVXLCDM" drop swap roman-rec  HERE OVER - ;

1999 roman type     \ MCMXCIX
  25 roman type     \ XXV
 944 roman type     \ CMXLIV
```

Alternative implementation

```forth
create romans 0 , 1 , 5 , 21 , 9 , 2 , 6 , 22 , 86 , 13 ,
  does> swap cells + @ ;

: roman-digit                          ( a1 n1 a2 n2 -- a3)
  drop >r romans
  begin dup while tuck 4 mod 1- chars r@ + c@ over c! char+ swap 4 / repeat
  r> drop drop
;

: (split) swap >r /mod r> swap ;

: >roman                               ( n1 a -- a n2)
  tuck 1000 (split) s" M  " roman-digit 100 (split) s" CDM" roman-digit
  10 (split) s" XLC" roman-digit 1 (split) s" IVX" roman-digit nip over -
;

create (roman) 16 chars allot

1999 (roman) >roman type cr
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program roman_numerals

  implicit none

  write (*, '(a)') roman (2009)
  write (*, '(a)') roman (1666)
  write (*, '(a)') roman (3888)

contains

function roman (n) result (r)

  implicit none
  integer, intent (in) :: n
  integer, parameter   :: d_max = 13
  integer              :: d
  integer              :: m
  integer              :: m_div
  character (32)       :: r
  integer,        dimension (d_max), parameter :: d_dec = &
    & (/1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1/)
  character (32), dimension (d_max), parameter :: d_rom = &
    & (/'M ', 'CM', 'D ', 'CD', 'C ', 'XC', 'L ', 'XL', 'X ', 'IX', 'V ', 'IV', 'I '/)

  r = ''
  m = n
  do d = 1, d_max
    m_div = m / d_dec (d)
    r = trim (r) // repeat (trim (d_rom (d)), m_div)
    m = m - d_dec (d) * m_div
  end do

end function roman

end program roman_numerals
```


{{out}}

```txt

  MMIX
  MDCLXVI
  MMMDCCCLXXXVIII

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function romanEncode(n As Integer) As String
  If n < 1 OrElse n > 3999 Then Return "" '' can only encode numbers in range 1 to 3999
  Dim roman1(0 To 2) As String = {"MMM", "MM", "M"}
  Dim roman2(0 To 8) As String = {"CM", "DCCC", "DCC", "DC", "D", "CD", "CCC", "CC", "C"}
  Dim roman3(0 To 8) As String = {"XC", "LXXX", "LXX", "LX", "L", "XL", "XXX", "XX", "X"}
  Dim roman4(0 To 8) As String = {"IX", "VIII", "VII", "VI", "V", "IV", "III", "II", "I"}
  Dim As Integer thousands, hundreds, tens, units
  thousands = n \ 1000
  n Mod= 1000
  hundreds = n \ 100
  n Mod= 100
  tens = n \ 10
  units = n Mod 10
  Dim roman As String = ""
  If thousands > 0  Then roman += roman1(3 - thousands)
  If hundreds > 0   Then roman += roman2(9 - hundreds)
  If tens > 0       Then roman += roman3(9 - tens)
  If units > 0      Then roman += roman4(9 - units)
  Return roman
End Function

Dim a(2) As Integer = {1990, 2008, 1666}
For i As Integer = 0 To 2
  Print a(i); " => "; romanEncode(a(i))
Next

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

 1990 => MCMXC
 2008 => MMVIII
 1666 => MDCLXVI

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

local fn DecimaltoRoman( decimal as short ) as Str15
dim as short arabic(12)
dim as Str15 roman(12)
dim as long  i
dim as Str15 result : result = ""

arabic(0) = 1000 : arabic(1) = 900 : arabic(2) = 500 : arabic(3) = 400
arabic(4) = 100  : arabic(5) = 90  : arabic(6) = 50  : arabic(7)  = 40
arabic(8) = 10   : arabic(9) = 9   : arabic(10) = 5  : arabic(11) = 4: arabic(12) = 1

roman(0) = "M" : roman(1) = "CM" : roman(2) = "D"  : roman(3)  = "CD"
roman(4) = "C" : roman(5) = "XC" : roman(6) = "L"  : roman(7)  = "XL"
roman(8) = "X" : roman(9) = "IX" : roman(10) = "V" : roman(11) = "IV" : roman(12) = "I"

for i = 0 to 12
   while ( decimal >= arabic(i) )
      result = result + roman(i)
      decimal = decimal - arabic(i)
   wend
next i
if result == "" then result = "Zepherium"
end fn = result

print "1990 = "; fn DecimaltoRoman( 1990 )
print "2008 = "; fn DecimaltoRoman( 2008 )
print "2016 = "; fn DecimaltoRoman( 2016 )
print "1666 = "; fn DecimaltoRoman( 1666 )
print "3888 = "; fn DecimaltoRoman( 3888 )
print "1914 = "; fn DecimaltoRoman( 1914 )
print "1000 = "; fn DecimaltoRoman( 1000 )
print " 513 = "; fn DecimaltoRoman(  513 )
print "  33 = "; fn DecimaltoRoman(   33 )

```


Output:

```txt

1990 = MCMXC
2008 = MMVIII
2016 = MMXVI
1666 = MDCLXVI
3888 = MMMDCCCLXXXVIII
1914 = MCMXIV
1000 = M
 513 = DXIII
  33 = XXXIII

```



## Go

For fluff, the unicode overbar is recognized as a factor of 1000, [http://en.wikipedia.org/wiki/Roman_numerals#Large_numbers as described in WP].

If you see boxes in the code below, those are supposed to be the Unicode combining overline (U+0305) and look like {{overline|IVXLCDM}}.  Or, if you see overstruck combinations of letters, that's a different font rendering problem.  (If you need roman numerals > 3999 reliably, it might best to stick to chiseling them in stone...)

```go
package main

import "fmt"

var (
    m0 = []string{"", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"}
    m1 = []string{"", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"}
    m2 = []string{"", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"}
    m3 = []string{"", "M", "MM", "MMM", "I̅V̅",
        "V̅", "V̅I̅", "V̅I̅I̅", "V̅I̅I̅I̅", "I̅X̅"}
    m4 = []string{"", "X̅", "X̅X̅", "X̅X̅X̅", "X̅L̅",
        "L̅", "L̅X̅", "L̅X̅X̅", "L̅X̅X̅X̅", "X̅C̅"}
    m5 = []string{"", "C̅", "C̅C̅", "C̅C̅C̅", "C̅D̅",
        "D̅", "D̅C̅", "D̅C̅C̅", "D̅C̅C̅C̅", "C̅M̅"}
    m6 = []string{"", "M̅", "M̅M̅", "M̅M̅M̅"}
)

func formatRoman(n int) (string, bool) {
    if n < 1 || n >= 4e6 {
        return "", false
    }
    // this is efficient in Go.  the seven operands are evaluated,
    // then a single allocation is made of the exact size needed for the result.
    return m6[n/1e6] + m5[n%1e6/1e5] + m4[n%1e5/1e4] + m3[n%1e4/1e3] +
        m2[n%1e3/1e2] + m1[n%100/10] + m0[n%10],
        true
}

func main() {
    // show three numbers mentioned in task descriptions
    for _, n := range []int{1990, 2008, 1666} {
        r, ok := formatRoman(n)
        if ok {
            fmt.Println(n, "==", r)
        } else {
            fmt.Println(n, "not representable")
        }
    }
}
```

{{out}}

```txt

1990 == MCMXC
2008 == MMVIII
1666 == MDCLXVI

```



## Golo


```golo
#!/usr/bin/env golosh
----
This module takes a decimal integer and converts it to a Roman numeral.
----
module Romannumeralsencode

augment java.lang.Integer {

  function digits = |this| {

    var remaining = this
    let digits = vector[]
    while remaining > 0 {
      digits: prepend(remaining % 10)
      remaining = remaining / 10
    }
    return digits
  }

  ----
  123: digitsWithPowers() will return [[1, 2], [2, 1], [3, 0]]
  ----
  function digitsWithPowers = |this| -> vector[
    [ this: digits(): get(i), (this: digits(): size() - 1) - i ] for (var i = 0, i < this: digits(): size(), i = i + 1)
  ]

  function encode = |this| {

    require(this > 0, "the integer must be positive!")

    let romanPattern = |digit, powerOf10| -> match {
      when digit == 1 then i
      when digit == 2 then i + i
      when digit == 3 then i + i + i
      when digit == 4 then i + v
      when digit == 5 then v
      when digit == 6 then v + i
      when digit == 7 then v + i + i
      when digit == 8 then v + i + i + i
      when digit == 9 then i + x
      otherwise ""
    } with {
      i, v, x = [
        [ "I", "V", "X" ],
        [ "X", "L", "C" ],
        [ "C", "D", "M" ],
        [ "M", "?", "?" ]
      ]: get(powerOf10)
    }

    return vector[ romanPattern(digit, power) foreach digit, power in this: digitsWithPowers() ]: join("")
  }
}

function main = |args| {
  println("1990 == MCMXC? " + (1990: encode() == "MCMXC"))
  println("2008 == MMVIII? " + (2008: encode() == "MMVIII"))
  println("1666 == MDCLXVI? " + (1666: encode() == "MDCLXVI"))
}
```



## Groovy


```groovy
symbols = [ 1:'I', 4:'IV', 5:'V', 9:'IX', 10:'X', 40:'XL', 50:'L', 90:'XC', 100:'C', 400:'CD', 500:'D', 900:'CM', 1000:'M' ]

def roman(arabic) {
    def result = ""
    symbols.keySet().sort().reverse().each {
        while (arabic >= it) {
            arabic-=it
            result+=symbols[it]
        }
    }
    return result
}
assert roman(1) == 'I'
assert roman(2) == 'II'
assert roman(4) == 'IV'
assert roman(8) == 'VIII'
assert roman(16) == 'XVI'
assert roman(32) == 'XXXII'
assert roman(25) == 'XXV'
assert roman(64) == 'LXIV'
assert roman(128) == 'CXXVIII'
assert roman(256) == 'CCLVI'
assert roman(512) == 'DXII'
assert roman(954) == 'CMLIV'
assert roman(1024) == 'MXXIV'
assert roman(1666) == 'MDCLXVI'
assert roman(1990) == 'MCMXC'
assert roman(2008) == 'MMVIII'
```



## Haskell


With an explicit decimal digit representation list:


```haskell
digit :: Char -> Char -> Char -> Integer -> String
digit x y z k =
  [[x], [x, x], [x, x, x], [x, y], [y], [y, x], [y, x, x], [y, x, x, x], [x, z]] !!
  (fromInteger k - 1)

toRoman :: Integer -> String
toRoman 0 = ""
toRoman x
  | x < 0 = error "Negative roman numeral"
toRoman x
  | x >= 1000 = 'M' : toRoman (x - 1000)
toRoman x
  | x >= 100 = digit 'C' 'D' 'M' q ++ toRoman r
  where
    (q, r) = x `divMod` 100
toRoman x
  | x >= 10 = digit 'X' 'L' 'C' q ++ toRoman r
  where
    (q, r) = x `divMod` 10
toRoman x = digit 'I' 'V' 'X' x

main :: IO ()
main = print $ toRoman <$> [1999, 25, 944]
```

{{out}}

```txt
["MCMXCIX","XXV","CMXLIV"]
```


or, defining '''roman''' in terms of mapAccumL


```haskell
import Data.List (mapAccumL)

roman :: [(Int, String)] -> Int -> String
roman vks n =
  concat . snd $
  mapAccumL
    (\a (m, s) ->
        let (q, r) = quotRem a m
        in (r, [1 .. q] >> s))
    n
    vks

romanFromInt :: Int -> String
romanFromInt =
  roman $
  zip
    [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
    ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

main :: IO ()
main = (putStrLn . unlines) (romanFromInt <$> [1666, 1990, 2008, 2016, 2018])
```

{{Out}}

```txt
MDCLXVI
MCMXC
MMVIII
MMXVI
MMXVIII
```


With the Roman patterns abstracted, and in a simple logic programming idiom:


```haskell

module Main where

------------------------
--  ENCODER FUNCTION  --
------------------------

romanDigits = "IVXLCDM"

--  Meaning and indices of the romanDigits sequence:
--
--    magnitude |  1 5  | index
--   -----------|-------|-------
--        0     |  I V  |  0 1
--        1     |  X L  |  2 3
--        2     |  C D  |  4 5
--        3     |  M    |  6
--
--  romanPatterns are index offsets into romanDigits,
--  from an index base of 2 * magnitude.

romanPattern 0 = []      -- empty string
romanPattern 1 = [0]     -- I or X or C or M
romanPattern 2 = [0,0]   -- II or XX...
romanPattern 3 = [0,0,0] -- III...
romanPattern 4 = [0,1]   -- IV...
romanPattern 5 = [1]     -- ...
romanPattern 6 = [1,0]
romanPattern 7 = [1,0,0]
romanPattern 8 = [1,0,0,0]
romanPattern 9 = [0,2]

encodeValue 0 _ = ""
encodeValue value magnitude = encodeValue rest (magnitude + 1) ++ digits
  where
    low = rem value 10 -- least significant digit (encoded now)
    rest = div value 10 -- the other digits (to be encoded next)
    indices = map addBase (romanPattern low)
    addBase i = i + (2 * magnitude)
    digits = map pickDigit indices
    pickDigit i = romanDigits!!i

encode value = encodeValue value 0

------------------
--  TEST SUITE  --
------------------

main = do
  test "MCMXC" 1990
  test "MMVIII" 2008
  test "MDCLXVI" 1666

test expected value = putStrLn ((show value) ++ " = " ++ roman ++ remark)
  where
    roman = encode value
    remark =
      " (" ++
      (if roman == expected then "PASS"
       else ("FAIL, expected " ++ (show expected))) ++ ")"

```

{{out}}

```txt

1990 = MCMXC (PASS)
2008 = MMVIII (PASS)
1666 = MDCLXVI (PASS)

```



## HicEst


```hicest
CHARACTER Roman*20

CALL RomanNumeral(1990, Roman) ! MCMXC
CALL RomanNumeral(2008, Roman) ! MMVIII
CALL RomanNumeral(1666, Roman) ! MDCLXVI

END

SUBROUTINE RomanNumeral( arabic, roman)
  CHARACTER roman
  DIMENSION ddec(13)
  DATA      ddec/1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1/

  roman = ' '
  todo = arabic
  DO d = 1, 13
    DO rep = 1, todo / ddec(d)
      roman = TRIM(roman) // TRIM(CHAR(d, 13, "M  CM D  CD C  XC L  XL X  OX V  IV I  "))
      todo = todo - ddec(d)
    ENDDO
  ENDDO
END
```

=={{header|Icon}} and {{header|Unicon}}==

```Icon
link numbers   # commas, roman

procedure main(arglist)
every x := !arglist do
   write(commas(x), " -> ",roman(x)|"*** can't convert to Roman numerals ***")
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/numbers.icn numbers.icn provides roman] as seen below and is based upon a James Gimple SNOBOL4 function.


```Icon
procedure roman(n)		#: convert integer to Roman numeral
   local arabic, result
   static equiv

   initial equiv := ["","I","II","III","IV","V","VI","VII","VIII","IX"]

   integer(n) > 0 | fail
   result := ""
   every arabic := !n do
      result := map(result,"IVXLCDM","XLCDM**") || equiv[arabic + 1]
   if find("*",result) then fail else return result
end
```


{{out}}

```txt
#roman.exe  3 4 8 49 2010 1666 3000 3999 4000

3 -> III
4 -> IV
8 -> VIII
49 -> XLIX
2,010 -> MMX
1,666 -> MDCLXVI
3,999 -> MMMCMXCIX
4,000 -> *** can't convert to Roman numerals ***
```



## Io


{{trans|C#}}

```Io
Roman := Object clone do (
    nums := list(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
    rum := list("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I")

    numeral := method(number,
        result := ""
        for(i, 0, nums size,
            if(number == 0, break)
            while(number >= nums at(i),
                number = number - nums at(i)
                result = result .. rum at(i)
            )
        )
        return result
    )
)

Roman numeral(1666) println
```



## J

<tt>rfd</tt> obtains Roman numerals from decimals.


```j
R1000=. ;L:1 ,{ <@(<;._1);._2]0 :0
  C CC CCC CD D DC DCC DCCC CM
  X XX XXX XL L LX LXX LXXX XC
  I II III IV V VI VII VIII IX
)

rfd=: ('M' $~ <.@%&1000) , R1000 {::~ 1000&|
```


Explanation: R1000's definition contains rows representing each of 10 different digits in the 100s, 10s and 1s column (the first entry in each row is blank -- each entry is preceded by a space).  R1000 itself represents the first 1000 roman numerals (the cartesian product of these three rows of roman numeral "digits" which is constructed so that they are in numeric order.  And the first entry -- zero -- is just blank).  To convert a number to its roman numeral representation, we will separate the number into the integer part after dividing by 1000 (that's the number of 'M's we need) and the remainder after dividing by 1000 (which will be an index into R1000).

For example:
```j
   rfd 1234
MCCXXXIV
   rfd 567
DLXVII
   rfd 89
LXXXIX
```


Derived from the [[j:Essays/Roman Numerals|J Wiki]]. Further examples of use will be found there.


## Java

{{trans|Ada}}

The conversion function throws an IllegalArgumentException for non-positive numbers, since Java does not have unsigned primitives.
{{works with|Java|1.5+}}

```java5
public class RN {

    enum Numeral {
        I(1), IV(4), V(5), IX(9), X(10), XL(40), L(50), XC(90), C(100), CD(400), D(500), CM(900), M(1000);
        int weight;

        Numeral(int weight) {
            this.weight = weight;
        }
    };

    public static String roman(long n) {

        if( n <= 0) {
            throw new IllegalArgumentException();
        }

        StringBuilder buf = new StringBuilder();

        final Numeral[] values = Numeral.values();
        for (int i = values.length - 1; i >= 0; i--) {
            while (n >= values[i].weight) {
                buf.append(values[i]);
                n -= values[i].weight;
            }
        }
        return buf.toString();
    }

    public static void test(long n) {
        System.out.println(n + " = " + roman(n));
    }

    public static void main(String[] args) {
        test(1999);
        test(25);
        test(944);
        test(0);
    }

}
```

{{out}}

```txt
1999 = MCMXCIX
25 = XXV
944 = CMXLIV
Exception in thread "main" java.lang.IllegalArgumentException
	at RN.roman(RN.java:15)
	at RN.test(RN.java:31)
	at RN.main(RN.java:38)
```

{{works with|Java|1.8+}}

```java5
import java.util.Set;
import java.util.EnumSet;
import java.util.Collections;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

public interface RomanNumerals {
  public enum Numeral {
    M(1000), CM(900), D(500), CD(400), C(100), XC(90), L(50), XL(40), X(10), IX(9), V(5), IV(4), I(1);

    public final long weight;

    private static final Set<Numeral> SET = Collections.unmodifiableSet(EnumSet.allOf(Numeral.class));

    private Numeral(long weight) {
      this.weight = weight;
    }

    public static Numeral getLargest(long weight) {
      return SET.stream()
        .filter(numeral -> weight >= numeral.weight)
        .findFirst()
        .orElse(I)
      ;
    }
  };

  public static String encode(long n) {
    return LongStream.iterate(n, l -> l - Numeral.getLargest(l).weight)
      .limit(Numeral.values().length)
      .filter(l -> l > 0)
      .mapToObj(Numeral::getLargest)
      .map(String::valueOf)
      .collect(Collectors.joining())
    ;
  }

  public static long decode(String roman) {
    long result =  new StringBuilder(roman.toUpperCase()).reverse().chars()
      .mapToObj(c -> Character.toString((char) c))
      .map(numeral -> Enum.valueOf(Numeral.class, numeral))
      .mapToLong(numeral -> numeral.weight)
      .reduce(0, (a, b) -> a + (a <= b ? b : -b))
    ;
    if (roman.charAt(0) == roman.charAt(1)) {
      result += 2 * Enum.valueOf(Numeral.class, roman.substring(0, 1)).weight;
    }
    return result;
  }

  public static void test(long n) {
    System.out.println(n + " = " + encode(n));
    System.out.println(encode(n) + " = " + decode(encode(n)));
  }

  public static void main(String[] args) {
    LongStream.of(1999, 25, 944).forEach(RomanNumerals::test);
  }
}
```

{{out}}

```txt
1999 = MCMXCIX
MCMXCIX = 1999
25 = XXV
XXV = 25
944 = CMXLIV
CMXLIV = 944
```



## JavaScript



### ES5


### =Iteration=


{{trans|Tcl}}

```javascript
var roman = {
    map: [
        1000, 'M', 900, 'CM', 500, 'D', 400, 'CD', 100, 'C', 90, 'XC',
        50, 'L', 40, 'XL', 10, 'X', 9, 'IX', 5, 'V', 4, 'IV', 1, 'I',
    ],
    int_to_roman: function(n) {
        var value = '';
        for (var idx = 0; n > 0 && idx < this.map.length; idx += 2) {
            while (n >= this.map[idx]) {
                value += this.map[idx + 1];
                n -= this.map[idx];
            }
        }
        return value;
    }
}

roman.int_to_roman(1999); // "MCMXCIX"
```



### =Functional composition=



```JavaScript
(function () {
    'use strict';


    // If the Roman is a string, pass any delimiters through

    // (Int | String) -> String
    function romanTranscription(a) {
        if (typeof a === 'string') {
            var ps = a.split(/\d+/),
                dlm = ps.length > 1 ? ps[1] : undefined;

            return (dlm ? a.split(dlm)
                    .map(function (x) {
                        return Number(x);
                    }) : [a])
                .map(roman)
                .join(dlm);
        } else return roman(a);
    }

    // roman :: Int -> String
    function roman(n) {
        return [[1000, "M"], [900, "CM"], [500, "D"], [400, "CD"], [100,
        "C"], [90, "XC"], [50, "L"], [40, "XL"], [10, "X"], [9,
        "IX"], [5, "V"], [4, "IV"], [1, "I"]]
            .reduce(function (a, lstPair) {
                var m = a.remainder,
                    v = lstPair[0];

                return (v > m ? a : {
                    remainder: m % v,
                    roman: a.roman + Array(
                            Math.floor(m / v) + 1
                        )
                        .join(lstPair[1])
                });
            }, {
                remainder: n,
                roman: ''
            }).roman;
    }

    // TEST

    return [2016, 1990, 2008, "14.09.2015", 2000, 1666].map(
        romanTranscription);

})();
```


{{Out}}

```JavaScript
["MMXVI", "MCMXC", "MMVIII", "XIV.IX.MMXV", "MM", "MDCLXVI"]
```



### ES6

{{Trans|Haskell}}
(mapAccumL version)

```JavaScript
(() => {
    // ROMAN INTEGER STRINGS ----------------------------------------------------

    // roman :: Int -> String
    const roman = n =>
        concat(snd(mapAccumL((balance, [k, v]) => {
            const [q, r] = quotRem(balance, v);
            return [r, q > 0 ? k.repeat(q) : ''];
        }, n, [
            ['M', 1000],
            ['CM', 900],
            ['D', 500],
            ['CD', 400],
            ['C', 100],
            ['XC', 90],
            ['L', 50],
            ['XL', 40],
            ['X', 10],
            ['IX', 9],
            ['V', 5],
            ['IV', 4],
            ['I', 1]
        ])));

    // GENERIC FUNCTIONS -------------------------------------------------------

    // concat :: [[a]] -> [a] | [String] -> String
    const concat = xs =>
        xs.length > 0 ? (() => {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        })() : [];

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // 'The mapAccumL function behaves like a combination of map and foldl;
    // it applies a function to each element of a list, passing an accumulating
    // parameter from left to right, and returning a final value of this
    // accumulator together with the new list.' (See Hoogle)

    // mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    const mapAccumL = (f, acc, xs) =>
        xs.reduce((a, x) => {
            const pair = f(a[0], x);
            return [pair[0], a[1].concat([pair[1]])];
        }, [acc, []]);

    // quotRem :: Integral a => a -> a -> (a, a)
    const quotRem = (m, n) => [Math.floor(m / n), m % n];

    // show :: a -> String
    const show = (...x) =>
        JSON.stringify.apply(
            null, x.length > 1 ? [x[0], null, x[1]] : x
        );

    // snd :: (a, b) -> b
    const snd = tpl => Array.isArray(tpl) ? tpl[1] : undefined;

    // TEST -------------------------------------------------------------------
    return show(
        map(roman, [2016, 1990, 2008, 2000, 1666])
    );
})();
```

{{Out}}

```JavasCript
["MMXVI","MCMXC","MMVIII","MM","MDCLXVI"]
```



### =Declarative=


```JavaScript

function toRoman(num) {
  return 'I'
    .repeat(num)
    .replace(/IIIII/g, 'V')
    .replace(/VV/g, 'X')
    .replace(/XXXXX/g, 'L')
    .replace(/LL/g, 'C')
    .replace(/CCCCC/g, 'D')
    .replace(/DD/g, 'M')
    .replace(/VIIII/g, 'IX')
    .replace(/LXXXX/g, 'XC')
    .replace(/XXXX/g, 'XL')
    .replace(/DCCCC/g, 'CM')
    .replace(/CCCC/g, 'CD')
    .replace(/IIII/g, 'IV');
}

console.log(toRoman(1666));

```


{{Out}}

```JavaScript>MDCLXVI</lang



## Jsish

This covers both Encode (toRoman) and Decode (fromRoman).


```javascript
/* Roman numerals, in Jsish */
var Roman = {
    ord: ['M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX', 'V', 'IV', 'I'],
    val: [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1],

    fromRoman: function(roman:string):number {
        var n = 0;
        var re = /IV|IX|I|V|XC|XL|X|L|CD|CM|C|D|M/g;
        var matches = roman.match(re);
        if (!matches) return NaN;
        for (var hit of matches) n += this.val[this.ord.indexOf(hit)];
        return n;
    },

    toRoman: function(n:number):string {
        var roman = '';
        var idx = 0;
        while (n > 0) {
            while (n >= this.val[idx]) {
                roman += this.ord[idx];
                n -= this.val[idx];
            }
            idx++;
        }
        return roman;
    }
};

provide('Roman', 1);

if (Interp.conf('unitTest')) {
;    Roman.fromRoman('VIII');
;    Roman.fromRoman('MMMDIV');
;    Roman.fromRoman('CDIV');
;    Roman.fromRoman('MDCLXVI');
;    Roman.fromRoman('not');

;    Roman.toRoman(8);
;    Roman.toRoman(3504);
;    Roman.toRoman(404);
;    Roman.toRoman(1666);
}

/*
=!EXPECTSTART!=
Roman.fromRoman('VIII') ==> 8
Roman.fromRoman('MMMDIV') ==> 3504
Roman.fromRoman('CDIV') ==> 404
Roman.fromRoman('MDCLXVI') ==> 1666
Roman.fromRoman('not') ==> NaN
Roman.toRoman(8) ==> VIII
Roman.toRoman(3504) ==> MMMDIV
Roman.toRoman(404) ==> CDIV
Roman.toRoman(1666) ==> MDCLXVI
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u Roman.jsi
[PASS] Roman.jsi
```



## Julia

{{works with|Julia|0.6}}


```julia
function romanencode(n::Integer)
    if n < 1 || n > 4999 throw(DomainError()) end

    DR = [["I", "X", "C", "M"] ["V", "L", "D", "MMM"]]
    rnum = ""
    for (omag, d) in enumerate(digits(n))
        if d == 0
            omr = ""
        elseif d <  4
            omr = DR[omag, 1] ^ d
        elseif d == 4
            omr = DR[omag, 1] * DR[omag, 2]
        elseif d == 5
            omr = DR[omag, 2]
        elseif d <  9
            omr = DR[omag, 2] * DR[omag, 1] ^ (d - 5)
        else
            omr = DR[omag, 1] * DR[omag + 1, 1]
        end
        rnum = omr * rnum
    end
    return rnum
end

testcases = [1990, 2008, 1668]
append!(testcases, rand(1:4999, 12))
testcases = unique(testcases)

println("Test romanencode, arabic => roman:")
for n in testcases
    @printf("%-4i => %s\n", n, romanencode(n))
end
```


{{out}}

```txt
Test romanencode, arabic => roman:
1990 => MCMXC
2008 => MMVIII
1668 => MDCLXVIII
2928 => MMCMXXVIII
129  => CXXIX
4217 => MMMMCCXVII
1503 => MDIII
2125 => MMCXXV
1489 => MCDLXXXIX
3677 => MMMDCLXXVII
1465 => MCDLXV
1421 => MCDXXI
1642 => MDCXLII
572  => DLXXII
3714 => MMMDCCXIV
```



## Kotlin


```kotlin
val romanNumerals = mapOf(
    1000 to "M",
    900 to "CM",
    500 to "D",
    400 to "CD",
    100 to "C",
    90 to "XC",
    50 to "L",
    40 to "XL",
    10 to "X",
    9 to "IX",
    5 to "V",
    4 to "IV",
    1 to "I"
)

fun encode(number: Int): String? {
    if (number > 5000 || number < 1) {
        return null
    }
    var num = number
    var result = ""
    for ((multiple, numeral) in romanNumerals.entries) {
        while (num >= multiple) {
            num -= multiple
            result += numeral
        }
    }
    return result
}

fun main(args: Array<String>) {
    println(encode(1990))
    println(encode(1666))
    println(encode(2008))
}
```


{{out}}
```txt
MCMXC
MDCLXVI
MMVIII
```
Alternatively:
```kotlin

fun Int.toRomanNumeral(): String {
    fun digit(k: Int, unit: String, five: String, ten: String): String {
        return when (k) {
            in 1..3 -> unit.repeat(k)
            4 -> unit + five
            in 5..8 -> five + unit.repeat(k - 5)
            9 -> unit + ten
            else -> throw IllegalArgumentException("$k not in range 1..9")
        }
    }
    return when (this) {
        0 -> ""
        in 1..9 -> digit(this, "I", "V", "X")
        in 10..99 -> digit(this / 10, "X", "L", "C") + (this % 10).toRomanNumeral()
        in 100..999 -> digit(this / 100, "C", "D", "M") + (this % 100).toRomanNumeral()
        in 1000..3999 -> "M" + (this - 1000).toRomanNumeral()
        else -> throw IllegalArgumentException("${this} not in range 0..3999")
    }
}
```


## Lasso


```Lasso>define br =
 '\r'
// encode roman
define encodeRoman(num::integer)::string => {
	local(ref = array('M'=1000, 'CM'=900, 'D'=500, 'CD'=400, 'C'=100, 'XC'=90, 'L'=50, 'XL'=40, 'X'=10, 'IX'=9, 'V'=5, 'IV'=4, 'I'=1))
	local(out = string)
	with i in #ref do => {
		while(#num >= #i->second) => {
			#out->append(#i->first)
			#num -= #i->second
		}
	}
	return #out
}

'1990 in roman is '+encodeRoman(1990)
br
'2008 in roman is '+encodeRoman(2008)
br
'1666 in roman is '+encodeRoman(1666)
```



## LaTeX

The macro <code>\Roman</code> is defined for uppercase roman numeral, accepting as ''argument'' a name of an existing counter.


```latex
\documentclass{article}
\begin{document}
\newcounter{currentyear}\setcounter{currentyear}{\year}
Anno Domini \Roman{currentyear}
\end{document}
```



## Liberty BASIC


```lb

    dim arabic( 12)
    for i =0 to 12
        read k
        arabic( i) =k
    next i
    data 1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1

    dim roman$( 12)
    for i =0 to 12
        read k$
        roman$( i) =k$
    next i
    data "M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"

    print 2009, toRoman$( 2009)
    print 1666, toRoman$( 1666)
    print 3888, toRoman$( 3888)

    end

function toRoman$( value)
    i       =0
    result$ =""
    for i = 0 to 12
        while value >=arabic( i)
            result$ = result$ + roman$( i)
            value   = value   - arabic( i)
        wend
    next i
    toRoman$ =result$
end function

```


```txt

2009          MMIX
1666          MDCLXVI
3888          MMMDCCCLXXXVIII

```




## LiveCode


```LiveCode
function toRoman intNum
    local roman,numArabic
    put "M,CM,D,CD,C,XC,L,XL,X,IX,V,IV,I" into romans
    put "1000,900,500,400,100,90,50,40,10,9,5,4,1" into arabics
    put intNum into numArabic
    repeat with n = 1 to the number of items of romans
        put numArabic div item n of arabics into nums
        if nums > 0 then
            put repeatChar(item n of romans,nums) after roman
            add -(nums * item n of arabics) to numArabic
        end if
    end repeat
return roman
end toRoman

function repeatChar c n
    local cc
    repeat n times
        put c after cc
    end repeat
    return cc
end repeatChar
```


Examples

```txt
toRoman(2009) -- MMIX
toRoman(1666) -- MDCLXVI
toRoman(1984) -- MCMLXXXIV
toRoman(3888) -- MMMDCCCLXXXVIII
```



## Logo


```logo
make "roman.rules [
  [1000 M] [900 CM] [500 D] [400 CD]
  [ 100 C] [ 90 XC] [ 50 L] [ 40 XL]
  [  10 X] [  9 IX] [  5 V] [  4 IV]
  [   1 I]
]

to roman :n [:rules :roman.rules] [:acc "||]
  if empty? :rules [output :acc]
  if :n < first first :rules [output (roman :n bf :rules :acc)]
  output (roman :n - first first :rules  :rules  word :acc last first :rules)
end
```


{{works with|UCB Logo}}

```logo
make "patterns [[?] [? ?] [? ? ?] [? ?2] [?2] [?2 ?] [?2 ? ?] [?2 ? ? ?] [? ?3]]

to digit :d :numerals
  if :d = 0 [output "||]
  output apply (sentence "\( "word (item :d :patterns) "\)) :numerals
end
to digits :n :numerals
  output word ifelse :n < 10 ["||] [digits int :n/10 bf bf :numerals] ~
              digit modulo :n 10 :numerals
end
to roman :n
  if or :n < 0 :n >= 4000 [output [EX MODVS!]]
  output digits :n [I V X L C D M]
end

print roman 1999  ; MCMXCIX
print roman 25    ; XXV
print roman 944   ; CMXLIV
```



## LotusScript


```lss

Function toRoman(value) As String
	Dim arabic(12) As Integer
	Dim roman(12) As String

	arabic(0) = 1000
	arabic(1) = 900
	arabic(2) = 500
	arabic(3) = 400
	arabic(4) = 100
	arabic(5) = 90
	arabic(6) = 50
	arabic(7) = 40
	arabic(8) = 10
	arabic(9) = 9
	arabic(10) = 5
	arabic(11) = 4
	arabic(12) = 1

	roman(0) = "M"
	roman(1) = "CM"
	roman(2) = "D"
	roman(3) = "CD"
	roman(4) = "C"
	roman(5) = "XC"
	roman(6) = "L"
	roman(7) = "XL"
	roman(8) = "X"
	roman(9) = "IX"
	roman(10) = "V"
	roman(11) = "IV"
	roman(12) = "I"

	Dim i As Integer, result As String

	For i = 0 To 12
		Do While value >= arabic(i)
			result = result + roman(i)
			value = value - arabic(i)
		Loop
	Next i

	toRoman = result
End Function


```


## Lua



```lua
romans = {
{1000, "M"},
{900, "CM"}, {500, "D"}, {400, "CD"}, {100, "C"},
{90, "XC"}, {50, "L"}, {40, "XL"}, {10, "X"},
{9, "IX"}, {5, "V"}, {4, "IV"}, {1, "I"} }

k = io.read() + 0
for _, v in ipairs(romans) do --note that this is -not- ipairs.
  val, let = unpack(v)
  while k >= val do
    k = k - val
	io.write(let)
  end
end
print()
```



## M4


```M4
define(`roman',`ifelse(eval($1>=1000),1,`M`'roman(eval($1-1000))',
`ifelse(eval($1>=900),1,`CM`'roman(eval($1-900))',
`ifelse(eval($1>=500),1,`D`'roman(eval($1-500))',
`ifelse(eval($1>=100),1,`C`'roman(eval($1-100))',
`ifelse(eval($1>=90),1,`XC`'roman(eval($1-90))',
`ifelse(eval($1>=50),1,`L`'roman(eval($1-50))',
`ifelse(eval($1>=40),1,`XL`'roman(eval($1-40))',
`ifelse(eval($1>=10),1,`X`'roman(eval($1-10))',
`ifelse(eval($1>=9),1,`IX`'roman(eval($1-9))',
`ifelse(eval($1>=5),1,`V`'roman(eval($1-5))',
`ifelse(eval($1>=4),1,`IV`'roman(eval($1-4))',
`ifelse(eval($1>=1),1,`I`'roman(eval($1-1))'
)')')')')')')')')')')')')dnl
dnl
roman(3675)
```


{{out}}

```txt

MMMDCLXXV

```



## Maple


```Maple>
 for n in [ 1666, 1990, 2008 ] do printf( "%d\t%s\n", n, convert( n, 'roman' ) ) end:
1666    MDCLXVI
1990    MCMXC
2008    MMVIII
```



## Mathematica

RomanNumeral is a built-in function in the Wolfram language

Examples:

```Mathematica
RomanNumeral[4]
RomanNumeral[99]
RomanNumeral[1337]
RomanNumeral[1666]
RomanNumeral[6889]
```

gives back:

```Mathematica
IV
XCIX
MCCCXXXVII
MDCLXVI
MMMMMMDCCCLXXXIX
```


== {{header|Mercury}} ==

The non-ceremonial work in this program starts at the function <code>to_roman/1</code>.  Unusually for Mercury the function is semi-deterministic.  This is because some of the helper functions it calls are also semi-deterministic and the determinism subsystem propagates the status upward.  (There are ways to stop it from doing this, but it would distract from the actual Roman numeral conversion process so the semi-determinism has been left in.)

<code>to_roman/1</code> is just a string of chained function calls.  The number is passed in as a string (and the <code>main/2</code> predicate ensures that it is *only* digits!) is converted into a list of characters.  This list is then reversed and the Roman numeral version is built from it.  This resulting character list is then converted back into a string and returned.

<code>build_roman/1</code> takes the lead character off the list (reversed numerals) and then recursively calls itself.  It uses the <code>promote/2</code> predicate to multiply the ensuing Roman numerals (if any) by an order of magnitude and converts the single remaining digit to the appropriate list of Roman numerals.  To clarify, if it's passed the number "123" (encoded by this point as ['3', '2', '1']) the following transpires:

* The '3' is removed and <code>build_roman/1</code> is now called with ['2', '1'].
** The '2' is removed and the function is recursively called with ['1'].
*** The '1' is removed and the function is recursively called with [] (the empty list)..
**** The function returns [].
*** The [] has its (non-existent) digits promoted and then gets ['I'] appended (1 converts to ['I'] via <code>digit_to_roman/1</code>).
** The ['I'] has its (single) digit promoted and is converted to ['X'] and then gets ['I','I'] appended from the 2's conversion.  The resulting list is now ['X','I','I'] (or 12).
* The ['X','I','I'] has all of its digits promoted, yielding ['C','X','X'] before getting ['I','I','I'] appended.  The resulting list is now ['C','X','X','I','I','I'] which is converted into the string "CXXIII" back up in <code>to_roman/1</code>.

It is possible for this to be implemented differently even keeping the same algorithm.  For example the <code>map</code> module from the standard library could be used for looking up conversions and promotions instead of having <code>digit_to_roman/1</code> and <code>promote</code>.  This would require, however, either passing around the conversion tables constantly (bulking up the parameter lists of all functions and predicates) or creating said conversion tables each time at point of use (slowing down the implementation greatly).

Now the semi-determinism of the functions involved is a little bit of a problem.  In the <code>main/2</code> predicate you can see one means of dealing with it.  <code>main/2</code> *must* be deterministic (or cc_multi, but this is equivalent for this discussion).  There can be *no* failure in a called function or predicate … unless that failure is explicitly handled somehow.  In this implementation the failure is handled in the <code>foldl/4</code>'s provided higher-order predicate lambda.  The call to <code>to_roman/1</code> is called within a conditional and both the success (true) and failure (false) branches are handled.  This makes the passed-in predicate lambda deterministic, even though the implementation functions and predicates are semi-deterministic.

But why are they semi-deterministic?  Well, this has to do with the type system.  It doesn't permit sub-typing, so when the type of a predicate is, say <code>pred(char, char)</code> (as is the case for <code>promote/2</code>), the underlying implementation *must* handle *all* values that a type <code>char</code> could possibly hold.  It is trivial to see that our code does not.  This means that, in theory, it is possible that <code>promote/2</code> (or <code>digit_to_roman/1</code>) could be passed a value which cannot be processed, thus triggering a false result, and thus being semi-deterministic.


###  roman.m



```Mercury

:- module roman.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char, int, list, string.

main(!IO) :-
    command_line_arguments(Args, !IO),
    filter(is_all_digits, Args, CleanArgs),
    foldl((pred(Arg::in, !.IO::di, !:IO::uo) is det :-
               ( Roman = to_roman(Arg) ->
                     format("%s => %s", [s(Arg), s(Roman)], !IO), nl(!IO)
               ;     format("%s cannot be converted.", [s(Arg)], !IO), nl(!IO) )
          ), CleanArgs, !IO).

:- func to_roman(string::in) = (string::out) is semidet.
to_roman(Number) = from_char_list(build_roman(reverse(to_char_list(Number)))).

:- func build_roman(list(char)) = list(char).
:- mode build_roman(in)         = out is semidet.
build_roman([]) = [].
build_roman([D|R]) = Roman :-
    map(promote, build_roman(R), Interim),
    Roman = Interim ++ digit_to_roman(D).

:- func digit_to_roman(char) = list(char).
:- mode digit_to_roman(in)   = out is semidet.
digit_to_roman('0') = [].
digit_to_roman('1') = ['I'].
digit_to_roman('2') = ['I','I'].
digit_to_roman('3') = ['I','I','I'].
digit_to_roman('4') = ['I','V'].
digit_to_roman('5') = ['V'].
digit_to_roman('6') = ['V','I'].
digit_to_roman('7') = ['V','I','I'].
digit_to_roman('8') = ['V','I','I','I'].
digit_to_roman('9') = ['I','X'].

:- pred promote(char::in, char::out) is semidet.
promote('I', 'X').
promote('V', 'L').
promote('X', 'C').
promote('L', 'D').
promote('C', 'M').

:- end_module roman.

```


{{out}}

```txt

 $ '''mmc roman && ./roman 1 8 27 64 125 216 343 512 729 1000 1331 1728 2197 2744 3375'''
 ''1 => I''
 ''8 => VIII''
 ''27 => XXVII''
 ''64 => LXIV''
 ''125 => CXXV''
 ''216 => CCXVI''
 ''343 => CCCXLIII''
 ''512 => DXII''
 ''729 => DCCXXIX''
 ''1000 => M''
 ''1331 => MCCCXXXI''
 ''1728 => MDCCXXVIII''
 ''2197 => MMCXCVII''
 ''2744 => MMDCCXLIV''
 ''3375 => MMMCCCLXXV''

```



###  roman2.m


Another implementation using an algorithm inspired by [[#Erlang|the Erlang implementation]] could look like this:


```Mercury

:- module roman2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char, int, list, string.

main(!IO) :-
    command_line_arguments(Args, !IO),
    filter_map(to_int, Args, CleanArgs),
    foldl((pred(Arg::in, !.IO::di, !:IO::uo) is det :-
               ( Roman = to_roman(Arg) ->
                     format("%i => %s",
                            [i(Arg), s(from_char_list(Roman))], !IO),
                     nl(!IO)
               ;     format("%i cannot be converted.", [i(Arg)], !IO), nl(!IO) )
          ), CleanArgs, !IO).

:- func to_roman(int) = list(char).
:- mode to_roman(in)  = out is semidet.
to_roman(N) = ( N >= 1000 ->
                    ['M'] ++ to_roman(N - 1000)
              ;( N >= 100 ->
                     digit(N / 100, 'C', 'D', 'M') ++ to_roman(N rem 100)
               ;( N >= 10 ->
                      digit(N / 10, 'X', 'L', 'C') ++ to_roman(N rem 10)
                ;( N >= 1 ->
                       digit(N, 'I', 'V', 'X')
                 ; [] ) ) ) ).

:- func digit(int, char, char, char) = list(char).
:- mode digit(in,  in,   in,   in)   = out is semidet.
digit(1, X, _, _) = [X].
digit(2, X, _, _) = [X, X].
digit(3, X, _, _) = [X, X, X].
digit(4, X, Y, _) = [X, Y].
digit(5, _, Y, _) = [Y].
digit(6, X, Y, _) = [Y, X].
digit(7, X, Y, _) = [Y, X, X].
digit(8, X, Y, _) = [Y, X, X, X].
digit(9, X, _, Z) = [X, Z].

:- end_module roman2.

```


This implementation calculates the value of the thousands, then the hundreds, then the tens, then the ones.  In each case it uses the <code>digit/4</code> function and some tricks with unification to build the appropriate list of characters for the digit and multiplier being targeted.

Its output is identical to that of the previous version.


## Microsoft Small Basic

{{trans|DWScript}}

```microsoftsmallbasic

arabicNumeral = 1990
ConvertToRoman()
TextWindow.WriteLine(romanNumeral) 'MCMXC
arabicNumeral = 2018
ConvertToRoman()
TextWindow.WriteLine(romanNumeral) 'MMXVIII
arabicNumeral = 3888
ConvertToRoman()
TextWindow.WriteLine(romanNumeral) 'MMMDCCCLXXXVIII

Sub ConvertToRoman
  weights[0] = 1000
  weights[1] =  900
  weights[2] =  500
  weights[3] =  400
  weights[4] =  100
  weights[5] =   90
  weights[6] =   50
  weights[7] =   40
  weights[8] =   10
  weights[9] =    9
  weights[10] =   5
  weights[11] =   4
  weights[12] =   1
  symbols[0] = "M"
  symbols[1] = "CM"
  symbols[2] = "D"
  symbols[3] = "CD"
  symbols[4] = "C"
  symbols[5] = "XC"
  symbols[6] = "L"
  symbols[7] = "XL"
  symbols[8] = "X"
  symbols[9] = "IX"
  symbols[10] = "V"
  symbols[11] = "IV"
  symbols[12] = "I"
  romanNumeral = ""
  i = 0
  While (i <= 12) And (arabicNumeral > 0)
    While arabicNumeral >= weights[i]
      romanNumeral = Text.Append(romanNumeral, symbols[i])
      arabicNumeral = arabicNumeral - weights[i]
    EndWhile
    i = i + 1
  EndWhile
EndSub

```

{{out}}

```txt

MCMXC
MMXVIII
MMMDCCCLXXXVIII

```


=={{header|Modula-2}}==
{{trans|DWScript}}
{{works with|ADW Modula-2|any (Compile with the linker option ''Console Application'').}}

```modula2

MODULE RomanNumeralsEncode;

FROM Strings IMPORT
  Append;
FROM STextIO IMPORT
  WriteString, WriteLn;

CONST
  MaxChars = 15;
  (* 3888 or MMMDCCCLXXXVIII (15 chars) is the longest string properly encoded
     with these symbols. *)

TYPE
  TRomanNumeral = ARRAY [0 .. MaxChars - 1] OF CHAR;

PROCEDURE ToRoman(AValue: CARDINAL; VAR OUT Destination: ARRAY OF CHAR);
TYPE
  TRomanSymbols = ARRAY [0 .. 1] OF CHAR;
  TWeights = ARRAY [0 .. 12] OF CARDINAL;
  TSymbols = ARRAY [0 .. 12] OF TRomanSymbols;
CONST
  Weights = TWeights {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};
  Symbols = TSymbols {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX",
    "V", "IV", "I"};
VAR
  I: CARDINAL;
BEGIN
  Destination := "";
  I := 0;
  WHILE (I <= HIGH(Weights)) AND (AValue > 0) DO
    WHILE AValue >= Weights[I] DO
      Append(Symbols[I], Destination);
      AValue := AValue - Weights[I]
    END;
    INC(I);
  END;
END ToRoman;

VAR
  Numeral: TRomanNumeral;

BEGIN
  ToRoman(1990, Numeral); WriteString(Numeral); WriteLn; (* MCMXC *)
  ToRoman(2018, Numeral); WriteString(Numeral); WriteLn; (* MMXVIII *)
  ToRoman(3888, Numeral); WriteString(Numeral); WriteLn; (* MMMDCCCLXXXVIII *)
END RomanNumeralsEncode.

```

{{out}}

```txt

MCMXC
MMXVIII
MMMDCCCLXXXVIII

```



## MUMPS


```MUMPS
TOROMAN(INPUT)
 ;Converts INPUT into a Roman numeral. INPUT must be an integer between 1 and 3999
 ;OUTPUT is the string to return
 ;I is a loop variable
 ;CURRVAL is the current value in the loop
 QUIT:($FIND(INPUT,".")>1)!(INPUT<=0)!(INPUT>3999) "Invalid input"
 NEW OUTPUT,I,CURRVAL
 SET OUTPUT="",CURRVAL=INPUT
 SET:$DATA(ROMANNUM)=0 ROMANNUM="I^IV^V^IX^X^XL^L^XC^C^CD^D^CM^M"
 SET:$DATA(ROMANVAL)=0 ROMANVAL="1^4^5^9^10^40^50^90^100^400^500^900^1000"
 FOR I=$LENGTH(ROMANVAL,"^"):-1:1 DO
 .FOR  Q:CURRVAL<$PIECE(ROMANVAL,"^",I)  SET OUTPUT=OUTPUT_$PIECE(ROMANNUM,"^",I),CURRVAL=CURRVAL-$PIECE(ROMANVAL,"^",I)
 KILL I,CURRVAL
 QUIT OUTPUT
```

{{out}}

```txt
USER>W $$ROMAN^ROSETTA(1666)
MDCLXVI
USER>W $$TOROMAN^ROSETTA(2010)
MMX
USER>W $$TOROMAN^ROSETTA(949)
CMXLIX
USER>W $$TOROMAN^ROSETTA(949.24)
Invalid input
USER>W $$TOROMAN^ROSETTA(-949)
Invalid input
```


Another variant

```MUMPS
TOROMAN(n)
    ;return empty string if input parameter 'n' is not in 1-3999
    Quit:(n'?1.4N)!(n'<4000)!'n ""
    New r  Set r=""
    New p  Set p=$Length(n)
    New j,x
    For j=1:1:p  Do
    . Set x=$Piece("~I~II~III~IV~V~VI~VII~VIII~IX","~",$Extract(n,j)+1)
    . Set x=$Translate(x,"IVX",$Piece("IVX~XLC~CDM~M","~",p-j+1))
    . Set r=r_x
    Quit r
```



## Nim

{{trans|Python}}

```nim
import strutils

const nums = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"),
              (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]

proc toRoman(x): string =
  var x = x
  result = ""
  for a,r in items(nums):
    result.add(repeatStr(x div a, r))
    x = x mod a

for i in [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,40,
          50,60,69,70,80,90,99,100,200,300,400,500,600,666,700,800,900,
          1000,1009,1444,1666,1945,1997,1999,2000,2008,2010,2011,2500,
          3000,3999]:
  echo toRoman(i)
```



## Objeck

{{trans|C sharp}}

```objeck

bundle Default {
  class Roman {
    nums: static : Int[];
    rum : static : String[];

    function : Init() ~ Nil {
      nums := [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
      rum := ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"];
    }

    function : native : ToRoman(number : Int) ~ String {
      result := "";

      for(i :=0; i < nums->Size(); i += 1;) {
        while(number >= nums[i]) {
          result->Append(rum[i]);
          number -= nums[i];
        };
      };

      return result;
    }

    function : Main(args : String[]) ~ Nil {
      Init();

      ToRoman(1999)->PrintLine();
      ToRoman(25)->PrintLine();
      ToRoman(944)->PrintLine();
    }
  }
}

```



## OCaml


With an explicit decimal digit representation list:


```ocaml
let digit x y z = function
    1 -> [x]
  | 2 -> [x;x]
  | 3 -> [x;x;x]
  | 4 -> [x;y]
  | 5 -> [y]
  | 6 -> [y;x]
  | 7 -> [y;x;x]
  | 8 -> [y;x;x;x]
  | 9 -> [x;z]

let rec to_roman x =
  if x = 0 then []
  else if x < 0 then
    invalid_arg "Negative roman numeral"
  else if x >= 1000 then
    'M' :: to_roman (x - 1000)
  else if x >= 100 then
    digit 'C' 'D' 'M' (x / 100) @ to_roman (x mod 100)
  else if x >= 10 then
    digit 'X' 'L' 'C' (x / 10) @ to_roman (x mod 10)
  else
    digit 'I' 'V' 'X' x
```


{{out}}

```txt

# to_roman 1999;;
- : char list = ['M'; 'C'; 'M'; 'X'; 'C'; 'I'; 'X']
# to_roman 25;;
- : char list = ['X'; 'X'; 'V']
# to_roman 944;;
- : char list = ['C'; 'M'; 'X'; 'L'; 'I'; 'V']

```



## Oforth



```Oforth
[ [1000,"M"], [900,"CM"], [500,"D"], [400,"CD"], [100,"C"], [90,"XC"], [50,"L"], [40,"XL"], [10,"X"], [9,"IX"], [5,"V"], [4,"IV"], [1,"I"] ] const: Romans

: roman(n)
| r |
   StringBuffer new
   Romans forEach: r [ while(r first n <=) [ r second << n r first - ->n ] ] ;
```



## OpenEdge/Progress


```progress
FUNCTION encodeRoman RETURNS CHAR (
   i_i AS INT
):

   DEF VAR cresult   AS CHAR.
   DEF VAR croman    AS CHAR EXTENT 7 INIT [  "M", "D", "C", "L", "X", "V", "I" ].
   DEF VAR idecimal  AS INT  EXTENT 7 INIT [ 1000, 500, 100,  50,  10,   5,   1 ].
   DEF VAR ipos      AS INT  INIT 1.

   DO WHILE i_i > 0:

      IF i_i - idecimal[ ipos ] >= 0 THEN
         ASSIGN
            cresult  =  cresult + croman[ ipos ]
            i_i      =  i_i - idecimal[ ipos ]
            .
      ELSE IF ipos < EXTENT( croman ) - 1 AND i_i - ( idecimal[ ipos ] - idecimal[ ipos + 2 ] ) >= 0 THEN
         ASSIGN
            cresult  =  cresult + croman[ ipos + 2 ] + croman[ ipos ]
            i_i      =  i_i - ( idecimal[ ipos ] - idecimal[ ipos + 2 ] )
            ipos     =  ipos + 1
            .
      ELSE
         ipos = ipos + 1.
   END.

   RETURN cresult.

END FUNCTION. /* encodeRoman */

MESSAGE
   1990 encodeRoman( 1990 ) SKIP
   2008 encodeRoman( 2008 ) SKIP
   2000 encodeRoman( 2000 ) SKIP
   1666 encodeRoman( 1666 ) SKIP
VIEW-AS ALERT-BOX.

```

{{out}}

```txt
---------------------------
Message (Press HELP to view stack trace)
---------------------------
1990 MCMXC
2008 MMVIII
2000 MM
1666 MDCLXVI
---------------------------
OK   Help
---------------------------
```



## Oz

{{trans|Haskell}}

```oz
declare
  fun {Digit X Y Z K}
     unit([X] [X X] [X X X] [X Y] [Y] [Y X] [Y X X] [Y X X X] [X Z])
     .K
  end

  fun {ToRoman X}
     if     X == 0    then ""
     elseif X < 0     then raise toRoman(negativeInput X) end
     elseif X >= 1000 then "M"#{ToRoman X-1000}
     elseif X >= 100  then {Digit &C &D &M  X div 100}#{ToRoman X mod 100}
     elseif X >= 10   then {Digit &X &L &C  X div 10}#{ToRoman X mod 10}
     else                  {Digit &I &V &X  X}
     end
  end
in
  {ForAll {Map [1999 25 944] ToRoman} System.showInfo}
```



## PARI/GP

Old-style Roman numerals

```parigp
oldRoman(n)={
  while(n>999999,
    n-=1000000;
    print1("((((I))))")
  );
  if(n>499999,
    n-=500000;
    print1("I))))")
  );
  while(n>99999,
    n-=100000;
    print1("(((I)))")
  );
  if(n>49999,
    n-=50000;
    print1("I)))")
  );
  while(n>9999,
    n-=10000;
    print1("((I))")
  );
  if(n>4999,
    n-=5000;
    print1("I))")
  );
  while(n>999,
    n-=1000;
    print1("(I)")
  );
  if(n>499,
    n-=500;
    print1("I)")
  );
  while(n>99,
    n-=100;
    print1("C")
  );
  if(n>49,
    n-=50;
    print1("L");
  );
  while(n>9,
    n-=10;
    print1("X")
  );
  if(n>4,
    n-=5;
    print1("V");
  );
  while(n,
    n--;
    print1("I")
  );
  print()
};
```


This simple version of medieval Roman numerals does not handle large numbers.

```parigp
medievalRoman(n)={
  while(n>999,
    n-=1000;
    print1("M")
  );
  if(n>899,
    n-=900;
    print1("CM")
  );
  if(n>499,
    n-=500;
    print1("D")
  );
  if(n>399,
    n-=400;
    print1("CD")
  );
  while(n>99,
    n-=100;
    print1("C")
  );
  if(n>89,
    n-=90;
    print1("XC")
  );
  if(n>49,
    n-=50;
    print1("L")
  );
  if(n>39,
    n-=40;
    print1("XL")
  );
  while(n>9,
    n-=10;
    print1("X")
  );
  if(n>8,
    n-=9;
    print1("IX")
  );
  if(n>4,
    n-=5;
    print1("V")
  );
  if(n>3,
    n-=4;
    print1("IV")
  );
  while(n,
    n--;
    print1("I")
  );
  print()
};
```



## Pascal

See [[Roman_numerals/Encode#Delphi | Delphi]]


## Peloton

Roman numbers are built in to Peloton as a particular form of national number. However, for the sake of the task the _RO opcode has been defined.

```sgml><@ DEFUDOLITLIT
_RO|__Transformer|<@ DEFKEYPAR>__NationalNumericID|2</@><@ LETRESCS%NNMPAR>...|1</@></@>

<@ ENU$$DLSTLITLIT>1990,2008,1,2,64,124,1666,10001|,|
<@ SAYELTLST>...</@> is <@ SAY_ROELTLSTLIT>...|RomanLowerUnicode</@> <@ SAY_ROELTLSTLIT>...|RomanUpperUnicode</@> <@ SAY_ROELTLSTLIT>...|RomanASCII</@>
</@>
```


Same code in padded-out, variable-length English dialect

```sgml><# DEFINE USERDEFINEDOPCODE LITERAL LITERAL
_RO|__Transformer|<# DEFINE KEYWORD PARAMETER>__NationalNumericID|2</#><# LET RESULT CAST NATIONALNUMBER PARAMETER>...|1</#></#>

<# ENUMERATION LAMBDASPECIFIEDDELMITER LIST LITERAL LITERAL>1990,2008,1,2,64,124,1666,10001|,|
<# SAY ELEMENT LIST>...</#> is <# SAY _RO ELEMENT LIST LITERAL>...|RomanLowerUnicode</#> <# SAY _RO ELEMENT LIST LITERAL>...|RomanUpperUnicode</#> <# SAY _RO ELEMENT LIST LITERAL>...|RomanASCII</#>
</#>
```


{{out}} Notice here the three different ways of representing the results.
For reasons for notational differences, see [[wp:Roman_numerals#Alternate_forms]]

```txt
1990 is ⅿⅽⅿⅹⅽ ⅯⅭⅯⅩⅭ MCMXC
2008 is ⅿⅿⅷ ⅯⅯⅧ MMVIII
1 is ⅰ Ⅰ I
2 is ⅱ Ⅱ II
64 is ⅼⅹⅳ ⅬⅩⅣ LXIV
124 is ⅽⅹⅹⅳ ⅭⅩⅩⅣ CXXIV
1666 is ⅿⅾⅽⅼⅹⅵ ⅯⅮⅭⅬⅩⅥ MDCLXVI
10001 is ⅿⅿⅿⅿⅿⅿⅿⅿⅿⅿⅰ ↂⅠ MMMMMMMMMMI
```



## Perl


### = Simple program =

Simple, fast, produces same output as the Math::Roman module and the Perl 6 example, less crazy than writing a Latin program, and doesn't require experimental modules like the Perl 6 translation.

```perl
my @symbols = ( [1000, 'M'], [900, 'CM'], [500, 'D'], [400, 'CD'], [100, 'C'], [90, 'XC'], [50, 'L'], [40, 'XL'], [10, 'X'], [9, 'IX'], [5, 'V'], [4, 'IV'], [1, 'I']  );

sub roman {
  my($n, $r) = (shift, '');
  ($r, $n) = ('-', -$n) if $n < 0;  # Optional handling of negative input
  foreach my $s (@symbols) {
    my($arabic, $roman) = @$s;
    ($r, $n) = ($r .= $roman x int($n/$arabic),  $n % $arabic)
       if $n >= $arabic;
  }
  $r;
}

say roman($_) for 1..2012;
```



### = Using a module =


```perl
use Math::Roman qw/roman/;
say roman($_) for 1..2012'
```



### = Ported version of Perl6 =


```perl
use List::MoreUtils qw( natatime );

my %symbols = (
    1 => "I", 5 => "V", 10 => "X", 50 => "L", 100 => "C",
    500 => "D", 1_000 => "M"
);

my @subtractors = (
        1_000, 100,  500, 100,  100, 10,  50, 10,  10, 1,  5, 1,  1, 0
);

sub roman {
    return '' if 0 == (my $n = shift);
    my $iter = natatime 2, @subtractors;
    while( my ($cut, $minus) = $iter->() ) {
        $n >= $cut
            and return $symbols{$cut} . roman($n - $cut);
        $n >= $cut - $minus
            and return $symbols{$minus} . roman($n + $minus);
    }
};

print roman($_) . "\n" for 1..2012;
```



## Perl 6



```perl6
my %symbols =
    1 => "I", 5 => "V", 10 => "X", 50 => "L", 100 => "C",
    500 => "D", 1_000 => "M";

my @subtractors =
    1_000, 100,  500, 100,  100, 10,  50, 10,  10, 1,  5, 1,  1, 0;

multi sub roman (0) { '' }
multi sub roman (Int $n) {
    for @subtractors -> $cut, $minus {
        $n >= $cut
            and return %symbols{$cut} ~ roman($n - $cut);
        $n >= $cut - $minus
            and return %symbols{$minus} ~ roman($n + $minus);
     }
}

# Sample usage

for 1 .. 2_010 -> $x {
    say roman($x);
}
```



## Phix

copied from Euphoria

```Phix
constant roman  = {"M", "CM", "D","CD", "C","XC","L","XL","X","IX","V","IV","I"}
constant decml  = {1000, 900, 500, 400, 100, 90, 50,  40,  10,  9,  5,   4,  1 }

function toRoman(integer val)
string res = ""
    for i=1 to length(roman) do
        while val>=decml[i] do
            res &= roman[i]
            val -= decml[i]
        end while
    end for
    return res
end function
```



## PHP

{{works with|PHP|4+ tested in 5.2.12}}

```php

/**
 * int2roman
 * Convert any positive value of a 32-bit signed integer to its modern roman
 * numeral representation. Numerals within parentheses are multiplied by
 * 1000. ie. M == 1 000, (M) == 1 000 000, ((M)) == 1 000 000 000
 *
 * @param number - an integer between 1 and 2147483647
 * @return roman numeral representation of number
 */
function int2roman($number)
{
	if (!is_int($number) || $number < 1) return false; // ignore negative numbers and zero

	$integers = array(900, 500,  400, 100,   90,  50,   40,  10,    9,   5,    4,   1);
	$numerals = array('CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX', 'V', 'IV', 'I');
	$major = intval($number / 1000) * 1000;
	$minor = $number - $major;
	$numeral = $leastSig = '';

	for ($i = 0; $i < sizeof($integers); $i++) {
		while ($minor >= $integers[$i]) {
			$leastSig .= $numerals[$i];
			$minor  -= $integers[$i];
		}
	}

	if ($number >= 1000 && $number < 40000) {
		if ($major >= 10000) {
			$numeral .= '(';
			while ($major >= 10000) {
				$numeral .= 'X';
				$major -= 10000;
			}
			$numeral .= ')';
		}
		if ($major == 9000) {
			$numeral .= 'M(X)';
			return $numeral . $leastSig;
		}
		if ($major == 4000) {
			$numeral .= 'M(V)';
			return $numeral . $leastSig;
		}
		if ($major >= 5000) {
			$numeral .= '(V)';
			$major -= 5000;
		}
		while ($major >= 1000) {
			$numeral .= 'M';
			$major -= 1000;
		}
	}

	if ($number >= 40000) {
		$major = $major/1000;
		$numeral .= '(' . int2roman($major) . ')';
	}

	return $numeral . $leastSig;
}

```



## PicoLisp


```PicoLisp
(de roman (N)
   (pack
      (make
         (mapc
            '((C D)
               (while (>= N D)
                  (dec 'N D)
                  (link C) ) )
            '(M CM D CD C XC L XL X IX V IV I)
            (1000 900 500 400 100 90 50 40 10 9 5 4 1) ) ) ) )
```

{{out}}

```txt
: (roman 1009)
-> "MIX"

: (roman 1666)
-> "MDCLXVI"
```



## Pike


```pike
import String;
int main(){
   write(int2roman(2009) + "\n");
   write(int2roman(1666) + "\n");
   write(int2roman(1337) + "\n");
}
```



## plainTeX

TeX has its own way to convert a number into roman numeral, but it produces lowercase letters; the following macro (and usage example), produce uppercase roman numeral.


```tex
\def\upperroman#1{\uppercase\expandafter{\romannumeral#1}}
Anno Domini \upperroman{\year}
\bye
```



## PL/I


```PL/I

/* From Wiki Fortran */
roman: procedure (n) returns(character (32) varying);
   declare n fixed binary nonassignable;
   declare (d, m) fixed binary;
   declare (r, m_div) character (32) varying;
   declare d_dec(13) fixed binary static initial
      (1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1);
   declare d_rom(13) character (2) varying static initial
      ('M', 'CM', 'D', 'CD', 'C', 'XC', 'L',
       'XL', 'X', 'IX', 'V', 'IV', 'I');
   r = '';
   m = n;
   do d = 1 to 13;
      m_div = m / d_dec (d);
      r = r || copy (d_rom (d), m_div);
      m = m - d_dec (d) * m_div;
   end;
   return (r);
end roman;

```

Results:

```txt

   11                   XI
   1990                 MCMXC
   2008                 MMVIII
   1666                 MDCLXVI
   1999                 MCMXCIX

```



## PL/SQL


```PL/SQL


/*****************************************************************
 * $Author: Atanas Kebedjiev $
 *****************************************************************
 * Encoding an Arabic numeral to a Roman in the range 1..3999 is much simpler as Oracle provides the conversion formats.
 * Please see also the SQL solution for the same task.
 */

CREATE OR REPLACE
FUNCTION rencode(an IN NUMBER)
  RETURN VARCHAR2
IS
BEGIN
  RETURN to_char(an, 'RN');
END rencode;

BEGIN

    DBMS_OUTPUT.PUT_LINE ('2012 = ' || rencode('2012'));     -- MMXII
    DBMS_OUTPUT.PUT_LINE ('1951 = ' || rencode('1951'));     -- MCMLI
    DBMS_OUTPUT.PUT_LINE ('1987 = ' || rencode('1987'));     -- MCMLXXXVII
    DBMS_OUTPUT.PUT_LINE ('1666 = ' || rencode('1666'));     -- MDCLXVI
    DBMS_OUTPUT.PUT_LINE ('1999 = ' || rencode('1999'));     -- MCMXCIX

END;

```



## PowerBASIC

{{trans|BASIC}}

{{works with|PB/Win|8+}}

{{works with|PB/CC|5}}


```powerbasic
FUNCTION toRoman(value AS INTEGER) AS STRING
    DIM arabic(0 TO 12) AS INTEGER
    DIM roman(0 TO 12) AS STRING
    ARRAY ASSIGN arabic() = 1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1
    ARRAY ASSIGN roman() = "M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"

    DIM i AS INTEGER
    DIM result AS STRING

    FOR i = 0 TO 12
        DO WHILE value >= arabic(i)
            result = result & roman(i)
            value = value - arabic(i)
        LOOP
    NEXT i
    toRoman = result
END FUNCTION

FUNCTION PBMAIN
    'Testing
    ? "2009 = " & toRoman(2009)
    ? "1666 = " & toRoman(1666)
    ? "3888 = " & toRoman(3888)
END FUNCTION
```



## PowerShell


```PowerShell

function ConvertTo-RomanNumeral
{
  <#
    .SYNOPSIS
        Converts a number to a Roman numeral.
    .DESCRIPTION
        Converts a number - in the range of 1 to 3,999 - to a Roman numeral.
    .PARAMETER Number
        An integer in the range 1 to 3,999.
    .INPUTS
        System.Int32
    .OUTPUTS
        System.String
    .EXAMPLE
        ConvertTo-RomanNumeral -Number (Get-Date).Year
    .EXAMPLE
        (Get-Date).Year | ConvertTo-RomanNumeral
  #>
    [CmdletBinding()]
    [OutputType([string])]
    Param
    (
        [Parameter(Mandatory=$true,
                   HelpMessage="Enter an integer in the range 1 to 3,999",
                   ValueFromPipeline=$true,
                   Position=0)]
        [ValidateRange(1,3999)]
        [int]
        $Number
    )

    Begin
    {
        $DecimalToRoman = @{
            Thousands = "","M","MM","MMM"
            Hundreds  = "","C","CC","CCC","CD","D","DC","DCC","DCCC","CM"
            Tens      = "","X","XX","XXX","XL","L","LX","LXX","LXXX","XC"
            Ones      = "","I","II","III","IV","V","VI","VII","VIII","IX"
        }

        $column = @{
            Thousands = 0
            Hundreds  = 1
            Tens      = 2
            Ones      = 3
        }
    }
    Process
    {
        [int[]]$digits = $Number.ToString().PadLeft(4,"0").ToCharArray() |
                            ForEach-Object { [Char]::GetNumericValue($_) }

        $RomanNumeral  = ""
        $RomanNumeral += $DecimalToRoman.Thousands[$digits[$column.Thousands]]
        $RomanNumeral += $DecimalToRoman.Hundreds[$digits[$column.Hundreds]]
        $RomanNumeral += $DecimalToRoman.Tens[$digits[$column.Tens]]
        $RomanNumeral += $DecimalToRoman.Ones[$digits[$column.Ones]]

        $RomanNumeral
    }
}

```

This may be (slightly) useful.

```PowerShell

1..50 | ForEach-Object {
    [PSCustomObject]@{
        SuperbowlNumber  = $_
        SuperbowlNumeral = ConvertTo-RomanNumeral -Number $_
    }
}

```

{{Out}}

```txt

SuperbowlNumber SuperbowlNumeral
--------------- ----------------
              1 I
              2 II
              3 III
              4 IV
              5 V
              6 VI
              7 VII
              8 VIII
              9 IX
             10 X
             11 XI
             12 XII
             13 XIII
             14 XIV
             15 XV
             16 XVI
             17 XVII
             18 XVIII
             19 XIX
             20 XX
             21 XXI
             22 XXII
             23 XXIII
             24 XXIV
             25 XXV
             26 XXVI
             27 XXVII
             28 XXVIII
             29 XXIX
             30 XXX
             31 XXXI
             32 XXXII
             33 XXXIII
             34 XXXIV
             35 XXXV
             36 XXXVI
             37 XXXVII
             38 XXXVIII
             39 XXXIX
             40 XL
             41 XLI
             42 XLII
             43 XLIII
             44 XLIV
             45 XLV
             46 XLVI
             47 XLVII
             48 XLVIII
             49 XLIX
             50 L

```



## Prolog

{{works with|SWI-Prolog}}
{{libheader|clpfd}}
Library clpfd assures that the program works in both managements : Roman towards Arabic and Arabic towards Roman.

```Prolog
:- use_module(library(clpfd)).

roman :-
	LA =  [    _       , 2010,    _, 1449,         _],
	LR =  ['MDCCLXXXIX',  _  , 'CX',    _, 'MDCLXVI'],
	maplist(roman,   LA, LR),
	maplist(my_print,LA, LR).


roman(A, R) :-
	A #> 0,
	roman(A, [u, t, h, th], LR, []),
	label([A]),
	parse_Roman(CR, LR, []),
	atom_chars(R, CR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using DCG

roman(0, []) --> [].

roman(N, [H | T]) -->
	{N1 #= N / 10,
	 N2 #= N mod 10},
	roman(N1, T),
	unity(N2, H).

unity(1, u) --> ['I'].
unity(1, t) --> ['X'].
unity(1, h) --> ['C'].
unity(1, th)--> ['M'].

unity(4, u) --> ['IV'].
unity(4, t) --> ['XL'].
unity(4, h) --> ['CD'].
unity(4, th)--> ['MMMM'].

unity(5, u) --> ['V'].
unity(5, t) --> ['L'].
unity(5, h) --> ['D'].
unity(5, th)--> ['MMMMM'].

unity(9, u) --> ['IX'].
unity(9, t) --> ['XC'].
unity(9, h) --> ['CM'].
unity(9, th)--> ['MMMMMMMMM'].

unity(0, _) --> [].


unity(V, U)-->
	{V #> 5,
	V1 #= V - 5},
	unity(5, U),
	unity(V1, U).

unity(V, U) -->
	{V #> 1, V #< 4,
	V1 #= V-1},
	unity(1, U),
	unity(V1, U).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extraction of roman "lexeme"
parse_Roman(['C','M'|T]) -->
	['CM'],
	parse_Roman(T).

parse_Roman(['C','D'|T]) -->
	['CD'],
	parse_Roman(T).

parse_Roman(['X','C'| T]) -->
	['XC'],
	parse_Roman(T).


parse_Roman(['X','L'| T]) -->
	['XL'],
	parse_Roman(T).


parse_Roman(['I','X'| T]) -->
	['IX'],
	parse_Roman(T).


parse_Roman(['I','V'| T]) -->
	['IV'],
	parse_Roman(T).

parse_Roman([H | T]) -->
	[H],
	parse_Roman(T).


parse_Roman([]) -->
	[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
my_print(A, R) :-
	format('~w in roman is ~w~n', [A, R]).

```

{{out}}

```txt
 ?- roman.
1789 in roman is MDCCLXXXIX
2010 in roman is MMX
110 in roman is CX
1449 in roman is MCDXLIX
1666 in roman is MDCLXVI
true .

```



## PureBasic


```PureBasic
#SymbolCount = 12 ;0 based count
DataSection
  denominations:
  Data.s "M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I" ;0-12

  denomValues:
  Data.i  1000,900,500,400,100,90,50,40,10,9,5,4,1 ;values in decending sequential order
EndDataSection

;-setup
Structure romanNumeral
  symbol.s
  value.i
EndStructure

Global Dim refRomanNum.romanNumeral(#SymbolCount)

Restore denominations
For i = 0 To #SymbolCount
  Read.s refRomanNum(i)\symbol
Next

Restore denomValues
For i = 0 To #SymbolCount
  Read refRomanNum(i)\value
Next

Procedure.s decRoman(n)
  ;converts a decimal number to a roman numeral
  Protected roman$, i

  For i = 0 To #SymbolCount
    Repeat
      If n >= refRomanNum(i)\value
        roman$ + refRomanNum(i)\symbol
        n - refRomanNum(i)\value
      Else
        Break
      EndIf
    ForEver
  Next

  ProcedureReturn roman$
EndProcedure

If OpenConsole()

  PrintN(decRoman(1999)) ;MCMXCIX
  PrintN(decRoman(1666)) ;MDCLXVI
  PrintN(decRoman(25))   ;XXV
  PrintN(decRoman(954))  ;CMLIV

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```



## Python


### Imperative

# Version for Python 2

```python
roman =        "MDCLXVmdclxvi"; # UPPERCASE for thousands #
adjust_roman = "CCXXmmccxxii";
arabic =       (1000000, 500000, 100000, 50000, 10000, 5000, 1000, 500, 100, 50, 10, 5, 1);
adjust_arabic = (100000, 100000,  10000, 10000,  1000, 1000,  100, 100,  10, 10,  1, 1, 0);

def arabic_to_roman(dclxvi):
  org = dclxvi; # 666 #
  out = "";
  for scale,arabic_scale  in enumerate(arabic):
    if org == 0: break
    multiples = org / arabic_scale;
    org -= arabic_scale * multiples;
    out += roman[scale] * multiples;
    if org >= -adjust_arabic[scale] + arabic_scale:
      org -= -adjust_arabic[scale] + arabic_scale;
      out +=  adjust_roman[scale] +  roman[scale]
  return out

if __name__ == "__main__":
  test = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,40,50,60,69,70,
     80,90,99,100,200,300,400,500,600,666,700,800,900,1000,1009,1444,1666,1945,1997,1999,
     2000,2008,2500,3000,4000,4999,5000,6666,10000,50000,100000,500000,1000000);
  for val in test:
    print '%d - %s'%(val, arabic_to_roman(val))
```

An alternative which uses the divmod() function
```python
romanDgts= 'ivxlcdmVXLCDM_'

def ToRoman(num):
   namoR = ''
   if num >=4000000:
      print 'Too Big -'
      return '-----'
   for rdix in range(0, len(romanDgts), 2):
      if num==0: break
      num,r = divmod(num,10)
      v,r = divmod(r, 5)
      if r==4:
         namoR += romanDgts[rdix+1+v] + romanDgts[rdix]
      else:
         namoR += r*romanDgts[rdix] + (romanDgts[rdix+1] if(v==1) else '')
   return namoR[-1::-1]
```


It is more Pythonic to use zip to iterate over two lists together:

```python
anums = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
rnums = "M CM D CD C XC L XL X IX V IV I".split()

def to_roman(x):
    ret = []
    for a,r in zip(anums, rnums):
        n,x = divmod(x,a)
        ret.append(r*n)
    return ''.join(ret)

if __name__ == "__main__":
    test = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,40,
            50,60,69,70,80,90,99,100,200,300,400,500,600,666,700,800,900,
            1000,1009,1444,1666,1945,1997,1999,2000,2008,2010,2011,2500,
            3000,3999)

    for val in test:
        print '%d - %s'%(val, to_roman(val))

```


# Version for Python 3

```python
def arabic_to_roman(dclxvi):
#
### =====================

  '''Convert an integer from the decimal notation to the Roman notation'''
  org = dclxvi; # 666 #
  out = "";
  for scale, arabic_scale  in enumerate(arabic):
    if org == 0: break
    multiples = org // arabic_scale;
    org -= arabic_scale * multiples;
    out += roman[scale] * multiples;
    if (org >= -adjust_arabic[scale] + arabic_scale):
      org -= -adjust_arabic[scale] + arabic_scale;
      out +=  adjust_roman[scale] +  roman[scale]
  return out

if __name__ == "__main__":
  test = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,40,50,60,69,70,
     80,90,99,100,200,300,400,500,600,666,700,800,900,1000,1009,1444,1666,1945,1997,1999,
     2000,2008,2500,3000,4000,4999,5000,6666,10000,50000,100000,500000,1000000);

  for val in test:
    print("%8d %s" %(val, arabic_to_roman(val)))
```



### Declarative

Less readable, but a 'one liner':

```python
rnl = [ { '4' : 'MMMM', '3' : 'MMM', '2' : 'MM', '1' : 'M', '0' : '' }, { '9' : 'CM', '8' : 'DCCC', '7' : 'DCC',
          '6' : 'DC', '5' : 'D', '4' : 'CD', '3' : 'CCC', '2' : 'CC', '1' : 'C', '0' : '' }, { '9' : 'XC',
          '8' : 'LXXX', '7' : 'LXX', '6' : 'LX', '5' : 'L', '4' : 'XL', '3' : 'XXX', '2' : 'XX', '1' : 'X',
          '0' : '' }, { '9' : 'IX', '8' : 'VIII', '7' : 'VII', '6' : 'VI', '5' : 'V', '4' : 'IV', '3' : 'III',
          '2' : 'II', '1' : 'I', '0' : '' }]
# Option 1
def number2romannumeral(n):
    return ''.join([rnl[x][y] for x, y in zip(range(4), str(n).zfill(4)) if n < 5000 and n > -1])
# Option 2
def number2romannumeral(n):
    return reduce(lambda x, y: x + y, map(lambda x, y: rnl[x][y], range(4), str(n).zfill(4))) if -1 < n < 5000 else None
```



Or, defining '''roman''' in terms of '''mapAccumL''':
{{works with|Python|3}}
{{Trans|Haskell}}

```python
'''Encoding Roman Numerals'''

from functools import reduce
from itertools import chain


# romanFromInt ::  Int -> String
def romanFromInt(n):
    '''A string of Roman numerals encoding an integer.'''
    def go(a, ms):
        m, s = ms
        q, r = divmod(a, m)
        return (r, s * q)
    return concat(snd(mapAccumL(go)(n)(
        zip([1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1],
            ['M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX',
             'V', 'IV', 'I'])
    )))


# MAIN -------------------------------------------------
# main :: IO ()
def main():
    '''Test'''
    print(
        list(map(romanFromInt, [1666, 1990, 2008, 2016, 2018]))
    )


# GENERIC FUNCTIONS ---------------------------------------

# concat :: [[a]] -> [a]
# concat :: [String] -> String
def concat(xxs):
    '''The concatenation of all the elements in a list.'''
    xs = list(chain.from_iterable(xxs))
    unit = '' if isinstance(xs, str) else []
    return unit if not xs else (
        ''.join(xs) if isinstance(xs[0], str) else xs
    )


# mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
def mapAccumL(f):
    '''A tuple of an accumulation and a list derived by a
       combined map and fold,
       with accumulation from left to right.'''
    def go(a, x):
        tpl = f(a[0], x)
        return (tpl[0], a[1] + [tpl[1]])
    return lambda acc: lambda xs: (
        reduce(go, xs, (acc, []))
    )


# snd :: (a, b) -> b
def snd(tpl):
    '''Second component of a tuple.'''
    return tpl[1]


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
['MDCLXVI', 'MCMXC', 'MMVIII', 'MMXVI', 'MMXVIII']
```



## R

R has a built-in function, <code>[https://svn.r-project.org/R/trunk/src/library/utils/R/roman.R as.roman]</code>, for conversion to Roman numerals.  The implementation details are found in <code>utils:::.numeric2roman</code> (see previous link), and <code>utils:::.roman2numeric</code>, for conversion back to Arabic decimals.

```R
as.roman(1666)   # MDCLXVI
```

Since the object <code>as.roman</code> creates is just an integer vector with a class, you can do arithmetic with Roman numerals:

```R
as.roman(1666) + 334   # MM
```



## Racket

Straight recursion:

```Racket
#lang racket
(define (encode/roman number)
  (cond ((>= number 1000) (string-append "M" (encode/roman (- number 1000))))
        ((>= number 900) (string-append "CM" (encode/roman (- number 900))))
        ((>= number 500) (string-append "D" (encode/roman (- number 500))))
        ((>= number 400) (string-append "CD" (encode/roman (- number 400))))
        ((>= number 100) (string-append "C" (encode/roman (- number 100))))
        ((>= number 90) (string-append "XC" (encode/roman (- number 90))))
        ((>= number 50) (string-append "L" (encode/roman (- number 50))))
        ((>= number 40) (string-append "XL" (encode/roman (- number 40))))
        ((>= number 10) (string-append "X" (encode/roman (- number 10))))
        ((>= number 9) (string-append "IX" (encode/roman (- number 9))))
        ((>= number 5) (string-append "V" (encode/roman (- number 5))))
        ((>= number 4) (string-append "IV" (encode/roman (- number 4))))
        ((>= number 1) (string-append "I" (encode/roman (- number 1))))
        (else "")))
```


Using for/fold and quotient/remainder to remove repetition:

```Racket
#lang racket
(define (number->list n)
  (for/fold ([result null])
    ([decimal '(1000 900 500 400 100 90 50 40 10 9  5 4  1)]
     [roman   '(M    CM  D   CD  C   XC L  XL X  IX V IV I)])
    #:break (= n 0)
    (let-values ([(q r) (quotient/remainder n decimal)])
      (set! n r)
      (append result (make-list q roman)))))

(define (encode/roman number)
  (string-join (map symbol->string (number->list number)) ""))

(for ([n '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 25 30 40
           50 60 69 70 80 90 99 100 200 300 400 500 600 666 700 800 900
           1000 1009 1444 1666 1945 1997 1999 2000 2008 2010 2011 2500
           3000 3999)])
  (printf "~a ~a\n" n (encode/roman n)))
```



## Red

Straight iterative solution:

```Red

table: [1000 M 900 CM 500 D 400 CD 100 C 90 XC 50 L 40 XL 10 X 5 V 4 IV 1 I]

to-Roman: function [n [integer!] return: [string!]][
    out: copy ""
    foreach [a r] table [while [n >= a][append out r n: n - a]]
    out
]

foreach number [40 33 1888 2016][print [number ":" to-Roman number]]

```

Straight recursive solution:

```Red

table: [1000 M 900 CM 500 D 400 CD 100 C 90 XC 50 L 40 XL 10 X 5 V 4 IV 1 I]

to-Roman: func [n [integer!] return: [string!]][
    case [
        tail? table [table: head table copy ""]
        table/1 > n [table: skip table 2 to-Roman n]
        'else       [append copy form table/2 to-Roman n - table/1]
    ]
]

foreach number [40 33 1888 2016][print [number ":" to-Roman number]]

```

This solution builds, using metaprogramming, a `case` table, that relies on recursion to convert every digit.


```Red

to-Roman: function [n [integer!]] reduce [
    'case collect [
        foreach [a r] [1000 M 900 CM 500 D 400 CD 100 C 90 XC 50 L 40 XL 10 X 9 IX 5 V 4 IV 1 I][
            keep compose/deep [n >= (a) [append copy (form r) any [to-Roman n - (a) copy ""]]]
        ]
    ]
]

foreach number [40 33 1888 2016][print [number ":" to-Roman number]]

```



## Retro

This is a port of the [[Forth]] code; but returns a string rather than displaying the roman numerals. It only handles numbers between 1 and 3999.


```Retro

: vector ( ...n"- )
  here [ &, times ] dip : .data ` swap ` + ` @ ` do ` ; ;
: .I  dup     @ ^buffer'add ;
: .V  dup 1 + @ ^buffer'add ;
: .X  dup 2 + @ ^buffer'add ;

[ .I .X       drop ]
[ .V .I .I .I drop ]
[ .V .I .I    drop ]
[ .V .I       drop ]
[ .V          drop ]
[ .I .V       drop ]
[ .I .I .I    drop ]
[ .I .I       drop ]
[ .I          drop ]
&drop
10 vector .digit

: record ( an- )
  10 /mod dup [ [ over 2 + ] dip record ] &drop if .digit ;
: toRoman   ( n-a )
  here ^buffer'set
  dup 1 3999 within 0 =
  [ "EX LIMITO!\n" ] [ "IVXLCDM" swap record here ] if ;

```



## REXX


### version 1


```rexx
roman: procedure
arg number

/* handle only 1 to 3999, else return ? */
if number >= 4000 | number <= 0 then return "?"

romans = "   M  CM   D  CD   C  XC  L  XL  X IX  V IV  I"
arabic = "1000 900 500 400 100  90 50  40 10  9  5  4  1"

result = ""
do i = 1 to words(romans)
  do while number >= word(arabic,i)
    result = result || word(romans,i)
    number = number - word(arabic,i)
  end
end
return result
```


### version 2

This version of a REXX program allows almost any non-negative decimal integer.

Most people think that the Romans had no word for "zero".   The Roman numeral system has no need for a

zero   ''placeholder'',   so there was no name for it   (just as we have no name for a   "¶"   in the middle of our

numbers ─── as we don't have that possibility).   However, the Romans did have a name for zero (or nothing).

In fact the Romans had several names for zero   (see the REXX code),   as does modern English.   In American

English, many words can be used for   '''0''':     zero, nothing, naught, bupkis, zilch, goose-egg, nebbish, squat, nil,

crapola, what-Patty-shot-at, nineteen (only in cribbage), love (in tennis), etc.

Also, this REXX version supports large numbers (with parentheses and deep parentheses).

(This REXX code was ripped out of my general routine that also supported versions for '''Attic''', '''ancient Roman''',

and '''modern Roman''' numerals.)

The general REXX code is bulkier than most at it deals with   ''any''   non-negative decimal number,   and more

boilerplate code is in the general REXX code to handle the above versions.

```rexx
/*REXX program converts (Arabic) non─negative decimal integers (≥0) ───► Roman numerals.*/
numeric digits 10000                             /*decimal digs can be higher if wanted.*/
parse arg #                                      /*obtain optional integers from the CL.*/
@er= "argument isn't a non-negative integer: "   /*literal used when issuing error msg. */
if #=''  then                                    /*Nothing specified?  Then generate #s.*/
    do
                                                  do j= 0  by  11  to  111; #=# j;     end
    #=# 49;                                       do k=88  by 100  to 1200; #=# k;     end
    #=# 1000 2000 3000 4000 5000 6000;            do m=88  by 200  to 1200; #=# m;     end
    #=# 1304 1405 1506 1607 1708 1809 1910 2011;  do p= 4          to   50; #=# 10**p; end
    end                                          /*finished with generation of numbers. */

  do i=1  for words(#);         x=word(#, i)     /*convert each of the numbers───►Roman.*/
  if \datatype(x, 'W') | x<0  then say "***error***"  @er  x     /*¬ whole #?  negative?*/
  say  right(x, 55)     dec2rom(x)
  end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
dec2rom: procedure;   parse arg n,#              /*obtain the number, assign # to a null*/
         n=space(translate(n/1, , ','),  0)      /*remove commas from normalized integer*/
         nulla= 'ZEPHIRUM NULLAE NULLA NIHIL'    /*Roman words for "nothing" or "none". */
         if n==0  then return word(nulla, 1)     /*return a Roman word for  "zero".     */
         maxnp=(length(n)-1)%3                   /*find max(+1) # of parenthesis to use.*/
         highPos=(maxnp+1)*3                     /*highest position of number.          */
         nn=reverse( right(n, highPos, 0) )      /*digits for Arabic──►Roman conversion.*/
                       do j=highPos  to 1  by -3
                       _=substr(nn, j,   1);  select     /*════════════════════hundreds.*/
                                              when _==9  then hx='CM'
                                              when _>=5  then hx='D'copies("C", _-5)
                                              when _==4  then hx='CD'
                                              otherwise       hx=   copies('C', _)
                                              end  /*select hundreds*/
                       _=substr(nn, j-1, 1);  select     /*════════════════════════tens.*/
                                              when _==9  then tx='XC'
                                              when _>=5  then tx='L'copies("X", _-5)
                                              when _==4  then tx='XL'
                                              otherwise       tx=   copies('X', _)
                                              end  /*select tens*/
                       _=substr(nn, j-2, 1);  select     /*═══════════════════════units.*/
                                              when _==9  then ux='IX'
                                              when _>=5  then ux='V'copies("I", _-5)
                                              when _==4  then ux='IV'
                                              otherwise       ux=   copies('I', _)
                                              end  /*select units*/
                       $=hx || tx || ux
                       if $\==''  then #=# || copies("(", (j-1)%3)$ ||copies(')', (j-1)%3)
                       end   /*j*/
         if pos('(I',#)\==0  then do i=1  for 4           /*special case: M,MM,MMM,MMMM.*/
                                  if i==4  then _ = '(IV)'
                                           else _ = '('copies("I", i)')'
                                  if pos(_, #)\==0  then #=changestr(_, #, copies('M', i))
                                  end   /*i*/
         return #
```

Some older REXXes don't have a   '''changestr'''   BIF,   so one is included here   ──►   [[CHANGESTR.REX]].


'''output'''   when using the default (internal) input):
<pre style="height:80ex">
                                                      0 ZEPHIRUM
                                                     11 XI
                                                     22 XXII
                                                     33 XXXIII
                                                     44 XLIV
                                                     55 LV
                                                     66 LXVI
                                                     77 LXXVII
                                                     88 LXXXVIII
                                                     99 XCIX
                                                    110 CX
                                                     49 XLIX
                                                     88 LXXXVIII
                                                    188 CLXXXVIII
                                                    288 CCLXXXVIII
                                                    388 CCCLXXXVIII
                                                    488 CDLXXXVIII
                                                    588 DLXXXVIII
                                                    688 DCLXXXVIII
                                                    788 DCCLXXXVIII
                                                    888 DCCCLXXXVIII
                                                    988 CMLXXXVIII
                                                   1088 MLXXXVIII
                                                   1188 MCLXXXVIII
                                                   1000 M
                                                   2000 MM
                                                   3000 MMM
                                                   4000 MMMM
                                                   5000 (V)
                                                   6000 (VI)
                                                     88 LXXXVIII
                                                    288 CCLXXXVIII
                                                    488 CDLXXXVIII
                                                    688 DCLXXXVIII
                                                    888 DCCCLXXXVIII
                                                   1088 MLXXXVIII
                                                   1304 MCCCIV
                                                   1405 MCDV
                                                   1506 MDVI
                                                   1607 MDCVII
                                                   1708 MDCCVIII
                                                   1809 MDCCCIX
                                                   1910 MCMX
                                                   2011 MMXI
                                                  10000 (X)
                                                 100000 (C)
                                                1000000 (M)
                                               10000000 ((X))
                                              100000000 ((C))
                                             1000000000 ((M))
                                            10000000000 (((X)))
                                           100000000000 (((C)))
                                          1000000000000 (((M)))
                                         10000000000000 ((((X))))
                                        100000000000000 ((((C))))
                                       1000000000000000 ((((M))))
                                      10000000000000000 (((((X)))))
                                     100000000000000000 (((((C)))))
                                    1000000000000000000 (((((M)))))
                                   10000000000000000000 ((((((X))))))
                                  100000000000000000000 ((((((C))))))
                                 1000000000000000000000 ((((((M))))))
                                10000000000000000000000 (((((((X)))))))
                               100000000000000000000000 (((((((C)))))))
                              1000000000000000000000000 (((((((M)))))))
                             10000000000000000000000000 ((((((((X))))))))
                            100000000000000000000000000 ((((((((C))))))))
                           1000000000000000000000000000 ((((((((M))))))))
                          10000000000000000000000000000 (((((((((X)))))))))
                         100000000000000000000000000000 (((((((((C)))))))))
                        1000000000000000000000000000000 (((((((((M)))))))))
                       10000000000000000000000000000000 ((((((((((X))))))))))
                      100000000000000000000000000000000 ((((((((((C))))))))))
                     1000000000000000000000000000000000 ((((((((((M))))))))))
                    10000000000000000000000000000000000 (((((((((((X)))))))))))
                   100000000000000000000000000000000000 (((((((((((C)))))))))))
                  1000000000000000000000000000000000000 (((((((((((M)))))))))))
                 10000000000000000000000000000000000000 ((((((((((((X))))))))))))
                100000000000000000000000000000000000000 ((((((((((((C))))))))))))
               1000000000000000000000000000000000000000 ((((((((((((M))))))))))))
              10000000000000000000000000000000000000000 (((((((((((((X)))))))))))))
             100000000000000000000000000000000000000000 (((((((((((((C)))))))))))))
            1000000000000000000000000000000000000000000 (((((((((((((M)))))))))))))
           10000000000000000000000000000000000000000000 ((((((((((((((X))))))))))))))
          100000000000000000000000000000000000000000000 ((((((((((((((C))))))))))))))
         1000000000000000000000000000000000000000000000 ((((((((((((((M))))))))))))))
        10000000000000000000000000000000000000000000000 (((((((((((((((X)))))))))))))))
       100000000000000000000000000000000000000000000000 (((((((((((((((C)))))))))))))))
      1000000000000000000000000000000000000000000000000 (((((((((((((((M)))))))))))))))
     10000000000000000000000000000000000000000000000000 ((((((((((((((((X))))))))))))))))
    100000000000000000000000000000000000000000000000000 ((((((((((((((((C))))))))))))))))

```



## Ring


```ring

arabic = [1000, 900, 500, 400, 100, 90, 50,  40,  10,  9,  5,   4,  1]
roman  = ["M", "CM", "D", "CD", "C" ,"XC", "L", "XL" ,"X", "IX", "V", "IV", "I"]

see "2009 = " + toRoman(2009) + nl
see "1666 = " + toRoman(1666) + nl
see "3888 = " + toRoman(3888) + nl

func toRoman val
     result = ""
     for i = 1 to 13
         while val >= arabic[i]
               result = result + roman[i]
               val = val - arabic[i]
         end
      next
      return result

```



## Ruby

Roman numeral generation was used as an example for demonstrating [http://www.xpsd.org/cgi-bin/wiki?TestDrivenDevelopmentTutorialRomanNumerals Test Driven Development] in Ruby. The solution came to be:

```ruby
Symbols = { 1=>'I', 5=>'V', 10=>'X', 50=>'L', 100=>'C', 500=>'D', 1000=>'M' }
Subtractors = [ [1000, 100], [500, 100], [100, 10], [50, 10], [10, 1], [5, 1], [1, 0] ]

def roman(num)
  return Symbols[num]  if Symbols.has_key?(num)
  Subtractors.each do |cutPoint, subtractor|
    return roman(cutPoint) + roman(num - cutPoint)      if num >  cutPoint
    return roman(subtractor) + roman(num + subtractor)  if num >= cutPoint - subtractor and num < cutPoint
  end
end

[1990, 2008, 1666].each do |i|
  puts "%4d => %s" % [i, roman(i)]
end
```


{{out}}

```txt

1990 => MCMXC
2008 => MMVIII
1666 => MDCLXVI

```


Another shorter version if we don't consider calculating the substractors:


```ruby

Symbols = [ [1000, 'M'], [900, 'CM'], [500, 'D'], [400, 'CD'], [100, 'C'], [90, 'XC'], [50, 'L'], [40, 'XL'], [10, 'X'], [9, 'IX'], [5, 'V'], [4, 'IV'], [1, 'I'] ]

def arabic_to_roman(arabic)
  return '' if arabic.zero?
  Symbols.each { |arabic_rep, roman_rep| return roman_rep + arabic_to_roman(arabic - arabic_rep) if arabic >= arabic_rep }
end

```


Yet another way to solve it in terms of reduce


```ruby

Symbols = [ [1000, 'M'], [900, 'CM'], [500, 'D'], [400, 'CD'], [100, 'C'], [90, 'XC'], [50, 'L'], [40, 'XL'], [10, 'X'], [9, 'IX'], [5, 'V'], [4, 'IV'], [1, 'I'] ]

def to_roman(num)
    Symbols.reduce "" do |memo, (divisor, letter)|
        div, num = num.divmod(divisor)
        memo + letter * div
    end
end

```



## Run BASIC


```runbasic
[loop]
input "Input value:";val$
print roman$(val$)
goto [loop]

' ------------------------------
' Roman numerals
' ------------------------------
FUNCTION roman$(val$)
a2r$ = "M:1000,CM:900,D:500,CD:400,C:100,XC:90,L:50,XL:40,X:10,IX:9,V:5,IV:4,I:1"
v = val(val$)
for i = 1 to 13
  r$  = word$(a2r$,i,",")
  a   = val(word$(r$,2,":"))
  while v >= a
    roman$ = roman$ + word$(r$,1,":")
    v      = v - a
  wend
next i
END FUNCTION
```



## Rust


```rust
struct RomanNumeral {
    symbol: &'static str,
    value: u32
}

const NUMERALS: [RomanNumeral; 13] = [
    RomanNumeral {symbol: "M",  value: 1000},
    RomanNumeral {symbol: "CM", value: 900},
    RomanNumeral {symbol: "D",  value: 500},
    RomanNumeral {symbol: "CD", value: 400},
    RomanNumeral {symbol: "C",  value: 100},
    RomanNumeral {symbol: "XC", value: 90},
    RomanNumeral {symbol: "L",  value: 50},
    RomanNumeral {symbol: "XL", value: 40},
    RomanNumeral {symbol: "X",  value: 10},
    RomanNumeral {symbol: "IX", value: 9},
    RomanNumeral {symbol: "V",  value: 5},
    RomanNumeral {symbol: "IV", value: 4},
    RomanNumeral {symbol: "I",  value: 1}
];

fn to_roman(mut number: u32) -> String {
    let mut min_numeral = String::new();
    for numeral in NUMERALS.iter() {
        while numeral.value <= number {
            min_numeral = min_numeral + numeral.symbol;
            number -= numeral.value;
        }
    }
    min_numeral
}

fn main() {
    let nums = [2014, 1999, 25, 1666, 3888];
    for &n in nums.iter() {
        // 4 is minimum printing width, for alignment
        println!("{:2$} = {}", n, to_roman(n), 4);
    }
}
```
{{out}}

```txt

2014 = MMXIV
1999 = MCMXCIX
  25 = XXV
1666 = MDCLXVI
3888 = MMMDCCCLXXXVIII

```


## Scala

{{works with|Scala|2.8}}

```scala
val romanDigits = Map(
  1 -> "I", 5 -> "V",
  10 -> "X", 50 -> "L",
  100 -> "C", 500 -> "D",
  1000 -> "M",
  4 -> "IV", 9 -> "IX",
  40 -> "XL", 90 -> "XC",
  400 -> "CD", 900 -> "CM")
val romanDigitsKeys = romanDigits.keysIterator.toList sortBy (x => -x)
def toRoman(n: Int): String = romanDigitsKeys find (_ >= n) match {
  case Some(key) => romanDigits(key) + toRoman(n - key)
  case None => ""
}
```

{{Out}}

```txt
scala> List(1990, 2008, 1666) map toRoman
res55: List[String] = List(MCMXC, MMVIII, MDCLXVI)
```


### Using foldLeft


```Scala
def toRoman( v:Int ) : String = {
  val romanNumerals = List(1000->"M",900->"CM",500->"D",400->"CD",100->"C",90->"XC",
                           50->"L",40->"XL",10->"X",9->"IX",5->"V",4->"IV",1->"I")

  var n = v
  romanNumerals.foldLeft(""){(s,t) => {val c = n/t._1; n = n-t._1*c;  s + (t._2 * c) } }
}

// A small test
def test( arabic:Int ) = println( arabic + " => " + toRoman( arabic ) )

test(1990)
test(2008)
test(1666)
```

===Different code-style===

```Scala
def toRoman(num: Int): String = {
  case class RomanUnit(value: Int, token: String)
  val romanNumerals = List(
    RomanUnit(1000, "M"),
    RomanUnit(900, "CM"),
    RomanUnit(500, "D"),
    RomanUnit(400, "CD"),
    RomanUnit(100, "C"),
    RomanUnit(90, "XC"),
    RomanUnit(50, "L"),
    RomanUnit(40, "XL"),
    RomanUnit(10, "X"),
    RomanUnit(9, "IX"),
    RomanUnit(5, "V"),
    RomanUnit(4, "IV"),
    RomanUnit(1, "I"))

  var remainingNumber = num
  romanNumerals.foldLeft("") { (outputStr, romanUnit) =>
    {
      val times = remainingNumber / romanUnit.value
      remainingNumber -= romanUnit.value * times
      outputStr + (romanUnit.token * times)
    }
  }
}
```

{{out}}

```txt
1990 => MCMXC
2008 => MMVIII
1666 => MDCLXVI
```


## Scheme

This uses format directives supported in Chez Scheme since v6.9b; YMMV.


```scheme
(define (to-roman n)
  (format "~@r" n))
```


This is a general example using Chicken Scheme.

```scheme
(define roman-decimal
  '(("M"  . 1000)
    ("CM" . 900)
    ("D"  . 500)
    ("CD" . 400)
    ("C"  . 100)
    ("XC" .  90)
    ("L"  .  50)
    ("XL" .  40)
    ("X"  .  10)
    ("IX" .   9)
    ("V"  .   5)
    ("IV" .   4)
    ("I"  .   1)))

(define (to-roman value)
  (apply string-append
         (let loop ((v value)
                    (decode roman-decimal))
           (let ((r (caar decode))
                 (d (cdar decode)))
             (cond
              ((= v 0) '())
              ((>= v d) (cons r (loop (- v d) decode)))
              (else (loop v (cdr decode))))))))


(let loop ((n '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 25 30 40
                  50 60 69 70 80 90 99 100 200 300 400 500 600 666 700 800 900
                  1000 1009 1444 1666 1945 1997 1999 2000 2008 2010 2011 2500
                  3000 3999)))
  (unless (null? n)
    (printf "~a ~a\n" (car n) (to-roman (car n)))
    (loop (cdr n))))

```



## Seed7

The following program writes the numbers between 1 and 3999 as roman numerals.
The [http://seed7.sourceforge.net/libraries/wrinum.htm wrinum.s7i] library contains the
function [http://seed7.sourceforge.net/libraries/wrinum.htm#str%28ROMAN,in_integer%29 str(ROMAN,)],
which writes a roman numeral to a string.


```seed7
$ include "seed7_05.s7i";
  include "stdio.s7i";
  include "wrinum.s7i";

const proc: main is func
  local
    var integer: number is 0;
  begin
    for number range 1 to 3999 do
      writeln(str(ROMAN, number));
    end for;
  end func;
```


Original source [http://seed7.sourceforge.net/algorith/puzzles.htm#roman_numerals].


## SETL


```ada
examples := [2008, 1666, 1990];

for example in examples loop
    print( roman_numeral(example) );
end loop;

proc roman_numeral( n );
    R := [[1000, 'M'], [900, 'CM'], [500, 'D'], [400, 'CD'], [100, 'C'], [90, 'XC'], [50, 'L'], [40, 'XL'], [10, 'X'], [9, 'IX'], [5, 'V'], [4, 'IV'], [1, 'I']];
    roman := '';
    for numeral in R loop
        while n >= numeral(1) loop
            n := n - numeral(1);
            roman := roman + numeral(2);
        end loop;
    end loop;
    return roman;
end;
```

{{out}}

```txt
MMVIII
MDCLXVI
MCMXC
```



## Sidef

{{trans|ActionScript}}

```ruby
func arabic2roman(num, roman='') {
    static lookup = [
        :M:1000, :CM:900, :D:500,
        :CD:400, :C:100,  :XC:90,
        :L:50,   :XL:40,  :X:10,
        :IX:9,   :V:5,    :IV:4,
        :I:1
    ];
    lookup.each { |pair|
        while (num >= pair.second) {
            roman += pair.first;
            num -= pair.second;
        }
    }
    return roman;
}
say("1990 in roman is " + arabic2roman(1990));
say("2008 in roman is " + arabic2roman(2008));
say("1666 in roman is " + arabic2roman(1666));
```

{{out}}

```txt
1990 in roman is MCMXC
2008 in roman is MMVIII
1666 in roman is MDCLXVI
```


## Simula


```simula
BEGIN

    TEXT PROCEDURE TOROMAN(N); INTEGER N;
    BEGIN
        PROCEDURE P(WEIGHT,LIT); INTEGER WEIGHT; TEXT LIT;
        BEGIN
            WHILE N >= WEIGHT DO
            BEGIN
                T :- T & LIT;
                N := N - WEIGHT;
            END WHILE;
        END P;
        TEXT T; T :- NOTEXT;
        P( 1000, "M"  );
        P(  900, "CM" );
        P(  500, "D"  );
        P(  400, "CD" );
        P(  100, "C"  );
        P(   90, "XC" );
        P(   50, "L"  );
        P(   40, "XL" );
        P(   10, "X"  );
        P(    9, "IX" );
        P(    5, "V"  );
        P(    4, "IV" );
        P(    1, "I"  );
        TOROMAN :- T;
    END TOROMAN;

    INTEGER Y;
    FOR Y := 1990, 2008, 1666 DO
    BEGIN
        OUTTEXT("YEAR ");
        OUTINT(Y, 4);
        OUTTEXT(" => ");
        OUTTEXT(TOROMAN(Y));
        OUTIMAGE;
    END FOR;

END PROGRAM;

```

{{out}}

```txt

YEAR 1990 => MCMXC
YEAR 2008 => MMVIII
YEAR 1666 => MDCLXVI

```



## Smalltalk


{{works with|Smalltalk/X}}
in ST/X, integers already know how to print themselves as roman number:

```smalltalk>2013 printRomanOn:Stdout naive:false</lang

{{out}}

```txt

MMXIII
```

the implementation is:

```smalltalk

printRomanOn:aStream naive:naive
    "print the receiver as roman number to the argument, aStream.
     The naive argument controls if the conversion is
     correct (i.e. subtracting prefix notation for 4,9,40,90, etc.),
     or naive (i.e. print 4 as IIII and 9 as VIIII); also called simple.
     The naive version is often used for page numbers in documents."

    |restValue spec|

    restValue := self.
    restValue > 0 ifFalse:[self error:'negative roman'].

    naive ifTrue:[
        spec := #(
                " value string repeat "
                   1000 'M'    true
                    500 'D'    false
                    100 'C'    true
                     50 'L'    false
                     10 'X'    true
                      5 'V'    false
                      1 'I'    true
                 ).
    ] ifFalse:[
        spec := #(
                " value string repeat "
                   1000 'M'    true
                    900 'CM'   false
                    500 'D'    false
                    400 'CD'   false
                    100 'C'    true
                     90 'XC'   false
                     50 'L'    false
                     40 'XL'   false
                     10 'X'    true
                      9 'IX'   false
                      5 'V'    false
                      4 'IV'   false
                      1 'I'    true
                 ).
    ].

    spec
        inGroupsOf:3
        do:[:rValue :rString :repeatFlag |

            [
                (restValue >= rValue) ifTrue:[
                    aStream nextPutAll:rString.
                    restValue := restValue - rValue.
                ].
            ] doWhile:[ repeatFlag and:[ restValue >= rValue] ].
        ].

```



## SNOBOL4

Adapted from [http://burks.bton.ac.uk/burks/language/snobol/catspaw/tutorial/ch6.htm Catspaw SNOBOL Tutorial, Chapter 6]


```snobol4

* ROMAN(N) - Convert integer N to Roman numeral form.
*
*  N must be positive and less than 4000.
*
*  An asterisk appears in the result if N >= 4000.
*
*  The function fails if N is not an integer.

	DEFINE('ROMAN(N)UNITS')              :(ROMAN_END)

*  Get rightmost digit to UNITS and remove it from N.
*  Return null result if argument is null.
ROMAN	N RPOS(1) LEN(1) . UNITS =           :F(RETURN)

*  Search for digit, replace with its Roman form.
*  Return failing if not a digit.
	'0,1I,2II,3III,4IV,5V,6VI,7VII,8VIII,9IX,'  UNITS
+	BREAK(',') . UNITS                 :F(FRETURN)

*  Convert rest of N and multiply by 10.  Propagate a
*  failure return from recursive call back to caller.
	ROMAN = REPLACE(ROMAN(N), 'IVXLCDM', 'XLCDM**')
+	UNITS            :S(RETURN) F(FRETURN)
ROMAN_END

*	Testing
	OUTPUT = "1999 = " ROMAN(1999)
	OUTPUT = "  24 = " ROMAN(24)
	OUTPUT = " 944 = " ROMAN(944)

END
```

{{out}}

```txt

1999 = MCMXCIX
  24 = XXIV
 944 = CMXLIV

```


Here's a non-recursive version, and a Roman-to-Arabic converter to boot.


```SNOBOL4
*       # Arabic to Roman
        define('roman(n)s,ch,val,str') :(roman_end)
roman   roman = ge(n,4000) n :s(return)
        s = 'M1000 CM900 D500 CD400 C100 XC90 L50 XL40 X10 IX9 V5 IV4 I1 '
rom1    s span(&ucase) . ch break(' ') . val span(' ') = :f(rom2)
        str = str dupl(ch,(n / val))
        n = remdr(n,val) :(rom1)
rom2    roman = str :(return)
roman_end

*       # Roman to Arabic
        define('arabic(n)s,ch,val,sum,x') :(arabic_end)
arabic  s = 'M1000 D500 C100 L50 X10 V5 I1 '
        n = reverse(n)
arab1   n len(1) . ch = :f(arab2)
        s ch break(' ') . val
        val = lt(val,x) (-1 * val)
        sum = sum + val; x = val :(arab1)
arab2   arabic = sum :(return)
arabic_end

*       # Test and display
        tstr = '2010 1999 1492 1066 476 '
tloop   tstr break(' ') . year span(' ') = :f(out)
        r = roman(year)
        rstr = rstr year '=' r ' '
        astr = astr r '=' arabic(r) ' ' :(tloop)
out     output = rstr; output = astr
end
```


{{out}}

```txt
2010=MMX 1999=MCMXCIX 1492=MCDXCII 1066=MLXVI 476=CDLXXVI
MMX=2010 MCMXCIX=1999 MCDXCII=1492 MLXVI=1066 CDLXXVI=476
```



## SPL


```spl
a2r(a)=
  r = ""
  n = [["M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I"],[1000,900,500,400,100,90,50,40,10,9,5,4,1]]
  > i, 1..13
    > a!<n[i,2]
      r += n[i,1]
      a -= n[i,2]
    <
  <
  <= r
.

t = [1990,2008,1666]
> i, 1..#.size(t,1)
  #.output(t[i]," = ",a2r(t[i]))
<
```

{{out}}

```txt

1990 = MCMXC
2008 = MMVIII
1666 = MDCLXVI

```



## SQL


```SQL

--
-- This only works under Oracle and has the limitation of 1 to 3999


SQL> select to_char(1666, 'RN') urcoman, to_char(1666, 'rn') lcroman from dual;

URCOMAN         LCROMAN
--------------- ---------------
        MDCLXVI         mdclxvi

```



## Swift


```swift
func ator(var n: Int) -> String {

    var result = ""

    for (value, letter) in
       [( 1000,    "M"),
        (  900,   "CM"),
        (  500,    "D"),
        (  400,   "CD"),
        (  100,    "C"),
        (   90,   "XC"),
        (   50,    "L"),
        (   40,   "XL"),
        (   10,    "X"),
        (    9,   "IX"),
        (    5,    "V"),
        (    4,   "IV"),
        (    1,    "I")]
    {
        while n >= value {
            result += letter
            n   -= value
        }
    }
    return result
}
```

Sample call:
{{works with|Swift|1.x}}

```swift
println(ator(1666)) // MDCLXVI
```

{{works with|Swift|2.0}}

```swift
print(ator(1666)) // MDCLXVI
```

{{output}}

```txt
MDCLXVI
```



## Tcl


```tcl
proc to_roman {i} {
    set map {1000 M 900 CM 500 D 400 CD 100 C 90 XC 50 L 40 XL 10 X 9 IX 5 V 4 IV 1 I}
    foreach {value roman} $map {
        while {$i >= $value} {
            append res $roman
            incr i -$value
        }
    }
    return $res
}
```


=={{header|TI-83 BASIC}}==

```ti83b
PROGRAM:DEC2ROM
:"="→Str1
:Lbl ST
:ClrHome
:Disp "NUMBER TO"
:Disp "CONVERT:"
:Input A
:If fPart(A) or A≠abs(A)
:Then
:Goto PI
:End
:A→B
:While B≥1000
:Str1+"M"→Str1
:B-1000→B
:End
:If B≥900
:Then
:Str1+"CM"→Str1
:B-900→B
:End
:If B≥500
:Then
:Str1+"D"→Str1
:B-500→B
:End
:If B≥400
:Then
:Str1+"CD"?Str1
:B-400→B
:End
:While B≥100
:Str1+"C"→Str1
:B-100→B
:End
:If B≥90
:Then
:Str1+"XC"→Str1
:B-90→B
:End
:If B≥50
:Then
:Str1+"L"→Str1
:B-50→B
:End
:If B≥40
:Then
:Str1+"XL"→Str1
:B-40→B
:End
:While B≥10
:Str1+"X"→Str1
:B-10→B
:End
:If B≥9
:Then
:Str1+"IX"→Str1
:B-9→B
:End
:If B≥5
:Then
:Str1+"V"→Str1
:B-5→B
:End
:If B≥4
:Then
:Str1+"IV"→Str1
:B-4→B
:End
:While B>0
:Str1+"I"→Str1
:B-1→B
:End
:ClrHome
:Disp A
:Disp Str1
:Stop
:Lbl PI
:ClrHome
:Disp "THE NUMBER MUST"
:Disp "BE A POSITIVE"
:Disp "INTEGER."
:Pause
:Goto ST

```




## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
LOOP arab_number="1990'2008'1666"
roman_number = ENCODE (arab_number,ROMAN)
PRINT "Arabic number ",arab_number, " equals ", roman_number
ENDLOOP

```

{{out}}

```txt

Arabic number 1990 equals MCMXC
Arabic number 2008 equals MMVIII
Arabic number 1666 equals MDCLXVI

```



## uBasic/4tH

{{trans|BBC Basic}}
<lang>Push 1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000
                                       ' Initialize array
For i = 12 To 0 Step -1
  @(i) = Pop()
Next
                                       ' Calculate and print numbers
Print 1999, : Proc _FNroman (1999)
Print 2014, : Proc _FNroman (2014)
Print 1666, : Proc _FNroman (1666)
Print 3888, : Proc _FNroman (3888)

End

_FNroman Param (1)                     ' ( n --)
  Local (1)                            ' Define b@
                                       ' Try all numbers in array
  For b@ = 12 To 0 Step -1
    Do While a@ > @(b@) - 1            ' Several occurences of same number?
      GoSub ((b@ + 1) * 10)            ' Print roman digit
      a@ = a@ - @(b@)                  ' Decrement number
    Loop
  Next

  Print                                ' Terminate line
Return
                                       ' Print roman digits
 10 Print "I";  : Return
 20 Print "IV"; : Return
 30 Print "V";  : Return
 40 Print "IX"; : Return
 50 Print "X";  : Return
 60 Print "XL"; : Return
 70 Print "L";  : Return
 80 Print "XC"; : Return
 90 Print "C";  : Return
100 Print "CD"; : Return
110 Print "D";  : Return
120 Print "CM"; : Return
130 Print "M";  : Return
```



## UNIX Shell

{{trans|Tcl}}
{{works with|bash}}

```bash
roman() {
    local values=( 1000 900 500 400 100 90 50 40 10 5 4 1 )
    local roman=(
        [1000]=M [900]=CM [500]=D [400]=CD
         [100]=C  [90]=XC  [50]=L  [40]=XL
          [10]=X   [9]=IX   [5]=V   [4]=IV
           [1]=I
    )
    local nvmber=""
    local num=$1
    for value in ${values[@]}; do
        while (( num >= value )); do
            nvmber+=${roman[value]}
            ((num -= value))
        done
    done
    echo $nvmber
}

for test in 1999 24 944 1666 2008; do
    printf "%d = %s\n" $test $(roman $test)
done
```

{{out}}

```txt

1999 = MCMXCVIV
24 = XXIV
944 = CMXLIV
1666 = MDCLXVI
2008 = MMVIII
```



## Ursala


The algorithm is to implement the
[http://www.en.wikipedia.org/wiki/Roman_Numerals#Subtractive_principle subtractive principle]
by string substitution only after constucting the numeral from successive
remainders. The order among the substitutions matters. For example,
occurrences of DCCCC must be replaced by CM before any occurrences of
CCCC are replaced by CD. The substitution operator (%=) is helpful
here.

```Ursala
#import nat

roman =

-+
   'IIII'%='IV'+ 'VIIII'%='IX'+ 'XXXX'%='XL'+ 'LXXXX'%='XC'+ 'CCCC'%='CD'+ 'DCCCC'%='CM',
   ~&plrDlSPSL/'MDCLXVI'+ iota*+ +^|(^|C/~&,\/division)@rlX=>~&iNC <1000,500,100,50,10,5>+-
```

This test program applies the function to each member of a list of numbers.

```Ursala
#show+

test = roman* <1990,2008,1,2,64,124,1666,10001>
```

{{out}}

```txt
MCMXC
MMVIII
I
II
LXIV
CXXIV
MDCLXVI
MMMMMMMMMMI
```



## Vedit macro language


```vedit
// Main program for testing the function
//
do {
    #1 = Get_Num("Number to convert: ", STATLINE)
    Call("NUM_TO_ROMAN")
    Num_Type(#1, NOCR) Message(" = ") Reg_Type(1) Type_Newline
} while (Reg_Size(1))
Return

// Convert numeric value into Roman number
//  #1 = number to convert; on return: T-reg(1) = Roman number
//
:NUM_TO_ROMAN:
    Reg_Empty(1)                        // @1 = Results (Roman number)
    if (#1 < 1) { Return }              // non-positive numbers return empty string

    Buf_Switch(Buf_Free)
    Ins_Text("M1000,CM900,D500,CD400,C100,XC90,L50,XL40,X10,IX9,V5,IV4,I1")

    BOF
    #2 = #1
    Repeat(ALL) {
        Search("|A|[|A]", ADVANCE+ERRBREAK)         // get next item from conversion list
        Reg_Copy_Block(20, CP-Chars_Matched, CP)    // @20 = Letter(s) to be inserted
        #11 = Num_Eval()                            // #11 = magnitude (1000...1)
        while (#2 >= #11) {
            Reg_Set(1, @20, APPEND)
            #2 -= #11
        }
    }
    Buf_Quit(OK)
Return
```


{{out}}

```txt
    4 = IV
   12 = XII
 1666 = MDCLXVI
 1990 = MCMXC
 2011 = MMXI
```



## Visual Basic

{{trans|BASIC}}


```vb
Function toRoman(value) As String
    Dim arabic As Variant
    Dim roman As Variant

    arabic = Array(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
    roman = Array("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I")

    Dim i As Integer, result As String

    For i = 0 To 12
        Do While value >= arabic(i)
            result = result + roman(i)
            value = value - arabic(i)
        Loop
    Next i

    toRoman = result
End Function

Sub Main()
    MsgBox toRoman(Val(InputBox("Number, please")))
End Sub
```



## XBasic

{{trans|DWScript}}
{{works with|Windows XBasic}}

```xbasic

PROGRAM "romanenc"
VERSION "0.0000"

DECLARE FUNCTION Entry()
INTERNAL FUNCTION ToRoman$(aValue%%)

' 3888 or MMMDCCCLXXXVIII (15 chars) is the longest string properly encoded with these symbols.

FUNCTION Entry()
  PRINT ToRoman$(1990) ' MCMXC
  PRINT ToRoman$(2018) ' MMXVIII
  PRINT ToRoman$(3888) ' MMMDCCCLXXXVIII
END FUNCTION

FUNCTION ToRoman$(aValue%%)
  DIM weights%%[12]
  DIM symbols$[12]

  weights%%[0] = 1000
  weights%%[1] = 900
  weights%%[2] = 500
  weights%%[3] = 400
  weights%%[4] = 100
  weights%%[5] = 90
  weights%%[6] = 50
  weights%%[7] = 40
  weights%%[8] = 10
  weights%%[9] = 9
  weights%%[10] = 5
  weights%%[11] = 4
  weights%%[12] = 1

  symbols$[0] = "M"
  symbols$[1] = "CM"
  symbols$[2] = "D"
  symbols$[3] = "CD"
  symbols$[4] = "C"
  symbols$[5] = "XC"
  symbols$[6] = "L"
  symbols$[7] = "XL"
  symbols$[8] = "X"
  symbols$[9] = "IX"
  symbols$[10] = "V"
  symbols$[11] = "IV"
  symbols$[12] = "I"

  destination$ = ""
  i@@ = 0
  DO WHILE (i@@ <= 12) AND (aValue%% > 0)
    DO WHILE aValue%% >= weights%%[i@@]
      destination$ = destination$ + symbols$[i@@]
      aValue%% = aValue%% - weights%%[i@@]
    LOOP
    i@@ = i@@ + 1
  LOOP
  RETURN destination$
END FUNCTION
END PROGRAM

```

{{out}}

```txt

MCMXC
MMXVIII
MMMDCCCLXXXVIII

```



## XLISP


```lisp
(defun roman (n)
    (define roman-numerals '((1000 "m") (900 "cm") (500 "d") (400 "cd") (100 "c") (90 "xc") (50 "l") (40 "xl") (10 "x") (9 "ix") (5 "v") (4 "iv") (1 "i")))
    (defun romanize (arabic-numeral numerals roman-numeral)
        (if (= arabic-numeral 0)
            roman-numeral
            (if (>= arabic-numeral (caar numerals))
                (romanize (- arabic-numeral (caar numerals)) numerals (string-append roman-numeral (cadar numerals)))
                (romanize arabic-numeral (cdr numerals) roman-numeral))))
    (romanize n roman-numerals ""))

; test the function:
(display (mapcar roman '(10 2016 800 2769 1666 476 1453)))
```

{{out}}

```txt
(x mmxvi dccc mmdcclxix mdclxvi cdlxxvi mcdliii)
```



## XSLT


```xslt

<xsl:stylesheet  version="1.0"    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/data/number">
        <xsl:call-template name="for">
               <xsl:with-param name="stop">13</xsl:with-param>
        	<xsl:with-param name="value"><xsl:value-of select="@value"></xsl:value-of></xsl:with-param>
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="for">
      <xsl:param name="start">1</xsl:param>
      <xsl:param name="stop">1</xsl:param>
      <xsl:param name="step">1</xsl:param>
      <xsl:param name="value">1</xsl:param>
      <xsl:text/>
      <xsl:choose>
      <xsl:when test="($value &gt; /data/roman
/numeral[@pos=$start]/@value or $value = /data/roman
/numeral[@pos=$start]/@value) ">
          <xsl:value-of select="/data/roman
/numeral[@pos=$start]/@letter"/>
          <xsl:call-template name="for">
          <xsl:with-param name="stop">
            <xsl:value-of select="$stop"/>
          </xsl:with-param>
          <xsl:with-param name="start">
            <xsl:value-of select="$start"/>
          </xsl:with-param>
          <xsl:with-param name="value">
          	<xsl:value-of select="$value - /data/roman/numeral[@pos=$start]/@value"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$start &lt; $stop">
        <xsl:call-template name="for">
          <xsl:with-param name="stop">
            <xsl:value-of select="$stop"/>
          </xsl:with-param>
          <xsl:with-param name="start">
            <xsl:value-of select="$start + $step"/>
          </xsl:with-param>
          <xsl:with-param name="value">
          	<xsl:value-of select="$value"/>
          </xsl:with-param>
        </xsl:call-template>
        </xsl:if>
      </xsl:otherwise>
      </xsl:choose>
    </xsl:template>
</xsl:stylesheet>

```



## Yabasic


```Yabasic
roman$ = "M, CM, D, CD, C, XC, L, XL, X, IX, V, IV, I"
decml$ = "1000, 900, 500, 400, 100, 90, 50,  40,  10,  9,  5,   4,  1"

sub toRoman$(value)
    local res$, i, roman$(1), decml$(1), long

    long = token(roman$, roman$(), ", ")
    long = token(decml$, decml$(), ", ")

    for i=1 to long
        while(value >= val(decml$(i)))
            res$ = res$ + roman$(i)
            value = value - val(decml$(i))
        wend
    next i
    return res$
end sub

print 400, " ", toRoman$(400)
print 1990, " ", toRoman$(1990)
print 2008, " ", toRoman$(2008)
print 2009, " ", toRoman$(2009)
print 1666, " ", toRoman$(1666)
print 3888, " ", toRoman$(3888)
//Output:
// 400 = CD
// 1990 = MCMXC
// 2008 = MMVIII
// 2009 = MMIX
// 1666 = MDCLXVI
// 3888 = MMMDCCCLXXXVIII
```



## VBA


```vb
Private Function roman(n As Integer) As String
    roman = WorksheetFunction.roman(n)
End Function
Public Sub main()
    s = [{10, 2016, 800, 2769, 1666, 476, 1453}]
    For Each x In s
        Debug.Print roman(CInt(x)); " ";
    Next x
End Sub
```
{{out}}

```txt
X MMXVI DCCC MMDCCLXIX MDCLXVI CDLXXVI MCDLIII
```


## zkl


```zkl
var [const] romans = L(
   L("M", 1000), L("CM", 900), L("D",  500), L("CD", 400), L("C",  100),
   L("XC",  90), L("L",   50), L("XL",  40), L("X",   10), L("IX",   9),
   L("V",    5), L("IV",   4), L("I",    1));
fcn toRoman(i){		// convert int to a roman number
   reg text = "";
   foreach R,N in (romans){ text += R*(i/N); i = i%N; }
   return(text);
}
```


```txt

toRoman(1990) //-->"MCMXC"
toRoman(2008) //-->"MMVIII"
toRoman(1666) //-->"MDCLXVI"

```



## Zsh

Based on the python solution.

```zsh
function printroman () {
  local -a conv
  local number=$1 div rom num out
  conv=(I 1 IV 4 V 5 IX 9 X 10 XL 40 L 50 XC 90 C 100 CD 400 D 500 CM 900 M 1000)
  for num rom in ${(Oa)conv}; do
    (( div = number / num, number = number % num ))
    while (( div-- > 0 )); do
      out+=$rom
    done
  done
  echo $out
}
```

